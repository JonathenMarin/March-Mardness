library(data.table)
library(dplyr)
library(xgboost)
library(ggplot2)

# ==============================================================================
# 1. CLEAN START & DATA LOADING
# ==============================================================================
data_dir <- "march-machine-learning-mania-2025"

M_regular <- fread(file.path(data_dir, "MRegularSeasonDetailedResults.csv"))
M_tourney <- fread(file.path(data_dir, "MNCAATourneyDetailedResults.csv"))
M_seeds   <- fread(file.path(data_dir, "MNCAATourneySeeds.csv"))
W_regular <- fread(file.path(data_dir, "WRegularSeasonDetailedResults.csv"))
W_tourney <- fread(file.path(data_dir, "WNCAATourneyDetailedResults.csv"))
W_seeds   <- fread(file.path(data_dir, "WNCAATourneySeeds.csv"))

regular_results <- rbind(M_regular, W_regular)[Season >= 2003]
tourney_results <- rbind(M_tourney, W_tourney)[Season >= 2003]
seeds           <- rbind(M_seeds, W_seeds)[Season >= 2003]

cat("Data Loaded. Regular Rows:", nrow(regular_results), "\n")

# ==============================================================================
# 2. FEATURE ENGINEERING (Strict Mode - No Leaks)
# ==============================================================================
# A. Get Regular Season Stats
get_season_stats <- function(df) {
  w_view <- df[, .(Season, TeamID=WTeamID, Score=WScore, OppScore=LScore, Win=1)]
  l_view <- df[, .(Season, TeamID=LTeamID, Score=LScore, OppScore=WScore, Win=0)]
  combined <- rbind(w_view, l_view)
  
  stats <- combined[, .(
    win_pct    = mean(Win),
    avg_score  = mean(Score),
    avg_opp    = mean(OppScore),
    avg_margin = mean(Score - OppScore)
  ), by = .(Season, TeamID)]
  return(stats)
}

season_stats <- get_season_stats(regular_results)

# B. Get Seeds
seeds[, seed_num := as.integer(gsub("[^0-9]", "", Seed))]
clean_seeds <- seeds[, .(Season, TeamID, seed_num)]

# C. Build Training Set
tourney_train <- tourney_results[, .(Season, T1_TeamID=WTeamID, T2_TeamID=LTeamID, Win=1, PointDiff=WScore-LScore)]
tourney_train_rev <- tourney_results[, .(Season, T1_TeamID=LTeamID, T2_TeamID=WTeamID, Win=0, PointDiff=LScore-WScore)]
tourney_master <- rbind(tourney_train, tourney_train_rev)

# Add men_women indicator
tourney_master[, men_women := as.integer(substr(as.character(T1_TeamID), 1, 1) == "1")]

# Merge Stats & Seeds
tourney_master <- merge(tourney_master, season_stats, by.x=c("Season","T1_TeamID"), by.y=c("Season","TeamID"))
setnames(tourney_master, c("win_pct","avg_score","avg_opp","avg_margin"), c("T1_win_pct","T1_avg_score","T1_avg_opp","T1_avg_margin"))

tourney_master <- merge(tourney_master, season_stats, by.x=c("Season","T2_TeamID"), by.y=c("Season","TeamID"))
setnames(tourney_master, c("win_pct","avg_score","avg_opp","avg_margin"), c("T2_win_pct","T2_avg_score","T2_avg_opp","T2_avg_margin"))

tourney_master <- merge(tourney_master, clean_seeds, by.x=c("Season","T1_TeamID"), by.y=c("Season","TeamID"), all.x=TRUE)
setnames(tourney_master, "seed_num", "T1_seed")

tourney_master <- merge(tourney_master, clean_seeds, by.x=c("Season","T2_TeamID"), by.y=c("Season","TeamID"), all.x=TRUE)
setnames(tourney_master, "seed_num", "T2_seed")

# Calculate Seed_diff (AFTER both seeds are merged)
tourney_master[, Seed_diff := T2_seed - T1_seed]

tourney_clean <- tourney_master[complete.cases(tourney_master)]

cat("Training Rows Ready:", nrow(tourney_clean), "\n")

# ==============================================================================
# 2.5. EXPLORATORY PLOTS
# ==============================================================================
cat("\n--- Generating Seed Plots ---\n")

# 1. Point Differential by T1 Seed
seed_summary <- tourney_clean[, .(
  games = .N,
  avg_point_diff = mean(PointDiff),
  sd_point_diff = sd(PointDiff),
  win_rate = mean(Win)
), by = .(T1_seed, men_women)]

seed_summary <- seed_summary[order(men_women, T1_seed)]

p1 <- ggplot(seed_summary, aes(x = T1_seed, y = avg_point_diff, color = factor(men_women))) +
  geom_line() +
  geom_ribbon(aes(ymin = avg_point_diff - sd_point_diff, 
                  ymax = avg_point_diff + sd_point_diff,
                  fill = factor(men_women)), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     labels = c("Women", "Men")) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("Women", "Men")) +
  labs(title = "Point Differential by Team 1 Seed",
       x = "Team 1 Seed",
       y = "Average Point Differential",
       color = "Gender",
       fill = "Gender") +
  theme_minimal()

print(p1)

# 2. Point Differential by Seed_diff
seed_diff_summary <- tourney_clean[, .(
  games = .N,
  avg_point_diff = mean(PointDiff),
  sd_point_diff = sd(PointDiff),
  win_rate = mean(Win)
), by = .(Seed_diff, men_women)]

seed_diff_summary <- seed_diff_summary[order(men_women, Seed_diff)]

p2 <- ggplot(seed_diff_summary[abs(Seed_diff) <= 15], 
             aes(x = Seed_diff, y = avg_point_diff, color = factor(men_women))) +
  geom_line() +
  geom_ribbon(aes(ymin = avg_point_diff - sd_point_diff,
                  ymax = avg_point_diff + sd_point_diff,
                  fill = factor(men_women)),
              alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("Women", "Men")) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("Women", "Men")) +
  labs(title = "Point Differential by Seed Difference",
       subtitle = "Positive Seed_diff means Team 1 is better seeded",
       x = "Seed Difference (T2_seed - T1_seed)",
       y = "Average Point Differential",
       color = "Gender",
       fill = "Gender") +
  theme_minimal()

print(p2)
cat("Plots Generated.\n")


# ==============================================================================
# 3. OPTIMIZATION (Cross-Validation)
# ==============================================================================
# Features explicitly listed to prevent leaks
features <- c("T1_win_pct", "T1_avg_score", "T1_avg_opp", "T1_avg_margin", "T1_seed",
              "T2_win_pct", "T2_avg_score", "T2_avg_opp", "T2_avg_margin", "T2_seed",
              "Seed_diff")

dtrain <- xgb.DMatrix(data = as.matrix(tourney_clean[, ..features]), label = tourney_clean$Win)

params <- list(
  objective = "binary:logistic", 
  eta = 0.02, 
  max_depth = 4, 
  tree_method = "hist"
)

# Eval function for Brier Score
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  probs <- 1 / (1 + exp(-preds)) 
  err <- mean((probs - labels)^2)
  return(list(metric = "brier_score", value = err))
}

cat("\n--- Running Cross-Validation ---\n")
cv_results <- xgb.cv(
  params = params, 
  data = dtrain, 
  nrounds = 1000, 
  nfold = 5,
  custom_metric = evalerror,
  maximize = FALSE, 
  print_every_n = 50, 
  verbose = TRUE
)

# Find the "Sweet Spot"
best_round <- ifelse(is.null(cv_results$best_iteration), 
                     which.min(cv_results$evaluation_log$test_brier_score_mean), 
                     cv_results$best_iteration)
cat("\nOptimal Rounds Found:", best_round, "\n")
cat("Expected Test Brier Score:", min(cv_results$evaluation_log$test_brier_score_mean), "\n")


# ==============================================================================
# 4. FINAL TRAINING
# ==============================================================================
cat("--- Training Final Model ---\n")
final_model <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = best_round, # Use the optimized number
  evals = list(train = dtrain), 
  custom_metric = evalerror,
  maximize = FALSE, 
  print_every_n = 50
)

# Feature Importance
importance <- xgb.importance(feature_names = features, model = final_model)
xgb.plot.importance(importance, main="Feature Importance (Clean Model)")


# ==============================================================================
# 5. PREDICT 2025
# ==============================================================================
file_path <- "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/2025_games_kaggle.csv"

if(file.exists(file_path)) {
  test_games <- fread(file_path)
  
  test_set <- test_games[, .(
    Season,
    T1_TeamID = ifelse(WTeamID < LTeamID, WTeamID, LTeamID),
    T2_TeamID = ifelse(WTeamID > LTeamID, WTeamID, LTeamID),
    Actual_Result = ifelse(WTeamID < LTeamID, 1, 0)
  )]
  
  # Merge Stats
  test_set <- merge(test_set, season_stats, by.x=c("Season","T1_TeamID"), by.y=c("Season","TeamID"), all.x=TRUE)
  setnames(test_set, c("win_pct","avg_score","avg_opp","avg_margin"), c("T1_win_pct","T1_avg_score","T1_avg_opp","T1_avg_margin"))
  
  test_set <- merge(test_set, season_stats, by.x=c("Season","T2_TeamID"), by.y=c("Season","TeamID"), all.x=TRUE)
  setnames(test_set, c("win_pct","avg_score","avg_opp","avg_margin"), c("T2_win_pct","T2_avg_score","T2_avg_opp","T2_avg_margin"))
  
  # Merge Seeds
  test_set <- merge(test_set, clean_seeds, by.x=c("Season","T1_TeamID"), by.y=c("Season","TeamID"), all.x=TRUE)
  setnames(test_set, "seed_num", "T1_seed")
  
  test_set <- merge(test_set, clean_seeds, by.x=c("Season","T2_TeamID"), by.y=c("Season","TeamID"), all.x=TRUE)
  setnames(test_set, "seed_num", "T2_seed")
  
  # Calculate Seed_diff for test set
  test_set[, Seed_diff := T2_seed - T1_seed]
  
  # Predict
  valid_test <- test_set[complete.cases(test_set[, ..features])]
  cat("\n2025 Games Ready:", nrow(valid_test), "\n")
  
  if(nrow(valid_test) > 0) {
    d2025 <- xgb.DMatrix(data = as.matrix(valid_test[, ..features]))
    probs <- predict(final_model, d2025)
    
    if(min(probs) < 0 || max(probs) > 1) { probs <- 1 / (1 + exp(-probs)) }
    
    acc <- mean((ifelse(probs > 0.5, 1, 0) == valid_test$Actual_Result))
    brier <- mean((probs - valid_test$Actual_Result)^2)
    
    cat("\n--- 2025 PERFORMANCE ---\n")
    cat("Accuracy:   ", round(acc*100, 2), "%\n")
    cat("Brier Score:", round(brier, 5), "\n\n")
    
    res <- valid_test[, .(T1_TeamID, T2_TeamID, Seed_diff, Actual_Result)]
    res$Prob_Win <- round(probs, 4)
    print(head(res, 10))
  } else {
    cat("ERROR: No valid test cases after merging features!\n")
    cat("Check that 2025 teams exist in regular season data.\n")
  }
  
} else {
  cat("\n!!! FILE NOT FOUND !!!\n")
  cat("Expected path:", file_path, "\n")
  cat("Please verify the file exists and path is correct.\n")
}

cat("\n=== SCRIPT COMPLETE ===\n")