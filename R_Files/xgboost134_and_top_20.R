library(data.table)
library(dplyr)
library(xgboost)
library(ggplot2)
data_dir <- "march-machine-learning-mania-2025"

M_regular <- fread(file.path(data_dir, "MRegularSeasonDetailedResults.csv"))
M_tourney <- fread(file.path(data_dir, "MNCAATourneyDetailedResults.csv"))
M_seeds <- fread(file.path(data_dir, "MNCAATourneySeeds.csv"))

cat("Men's data loaded:\n")
cat("  Regular season games:", nrow(M_regular), "\n")
cat("  Tournament games:", nrow(M_tourney), "\n")
cat("  Seeds:", nrow(M_seeds), "\n\n")

W_regular <- fread(file.path(data_dir, "WRegularSeasonDetailedResults.csv"))
W_tourney <- fread(file.path(data_dir, "WNCAATourneyDetailedResults.csv"))
W_seeds <- fread(file.path(data_dir, "WNCAATourneySeeds.csv"))

cat("Women's data loaded:\n")
cat("  Regular season games:", nrow(W_regular), "\n")
cat("  Tournament games:", nrow(W_tourney), "\n")
cat("  Seeds:", nrow(W_seeds), "\n\n")


regular_results <- rbind(M_regular, W_regular)
tourney_results <- rbind(M_tourney, W_tourney)
seeds <- rbind(M_seeds, W_seeds)

cat("\nCombined data:\n")
cat("  Total regular season games:", nrow(regular_results), "\n")
cat("  Total tournament games:", nrow(tourney_results), "\n")
cat("  Total seeds:", nrow(seeds), "\n\n")


season_cutoff <- 2003

regular_results <- regular_results[Season >= season_cutoff]
tourney_results <- tourney_results[Season >= season_cutoff]
seeds <- seeds[Season >= season_cutoff]

cat("\nFiltered data:\n")
cat("  Regular season games:", nrow(regular_results), "\n")
cat("  Tournament games:", nrow(tourney_results), "\n")
cat("  Seeds:", nrow(seeds), "\n")
cat("  Seasons covered:", min(regular_results$Season), "to", max(regular_results$Season), "\n\n")


cat("Columns:", paste(names(regular_results), collapse = ", "), "\n")
cat("Columns:", paste(names(tourney_results), collapse = ", "), "\n")
cat("Columns:", paste(names(seeds), collapse = ", "), "\n")

unique_regular_teams <- unique(c(regular_results$WTeamID, regular_results$LTeamID))
unique_tourney_teams <- unique(c(tourney_results$WTeamID, tourney_results$LTeamID))
unique_seed_teams <- unique(seeds$TeamID)


cat("Unique teams in regular season:", length(unique_regular_teams), "\n")
cat("Unique teams in tournament:", length(unique_tourney_teams), "\n")
cat("Unique teams with seeds:", length(unique_seed_teams), "\n\n")


cat("Seasons in regular_results:", paste(sort(unique(regular_results$Season)), collapse = ", "), "\n")
cat("Seasons in tourney_results:", paste(sort(unique(tourney_results$Season)), collapse = ", "), "\n")
cat("Seasons in seeds:", paste(sort(unique(seeds$Season)), collapse = ", "), "\n\n")

example_season <- 2024
example_teamid <- 3163  # Change this to any team you're interested in

# Get regular season games
team_regular <- regular_results[
  Season == example_season & 
    (WTeamID == example_teamid | LTeamID == example_teamid)
]

# Get tournament games
team_tourney <- tourney_results[
  Season == example_season & 
    (WTeamID == example_teamid | LTeamID == example_teamid)
]


team_regular$result <- ifelse(team_regular$WTeamID == example_teamid, "WIN", "LOSS")
team_tourney$result <- ifelse(team_tourney$WTeamID == example_teamid, "WIN", "LOSS")

cat("Team", example_teamid, "in season", example_season, ":\n")
cat("Regular season games:", nrow(team_regular), "\n")
cat("Tournament games:", nrow(team_tourney), "\n\n")

cat("Regular season record:\n")
print(team_regular[, .(DayNum, WScore, LScore, result)])
cat("\n")

cat("Tournament record:\n")
print(team_tourney[, .(DayNum, WScore, LScore, result)])
cat("\n")

# Get this team's seed
team_seed <- seeds[Season == example_season & TeamID == example_teamid]
cat("Team seed:\n")
print(team_seed)
cat("\n")


prepare_data <- function(df) {
  
  df <- as.data.table(df)
  
  # Select box score columns
  keep_cols <- c(
    "Season", "DayNum", "NumOT",
    "WTeamID", "WScore", 
    "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF",
    "LTeamID", "LScore",
    "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF"
  )
  
  df <- df[, ..keep_cols]
  
  # Adjust for overtime
  df[, adjot := (40 + 5 * NumOT) / 40]
  
  stat_cols <- c(
    "WScore", "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF",
    "LScore", "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF"
  )
  
  df[, (stat_cols) := lapply(.SD, function(x) x / adjot), .SDcols = stat_cols]
  
  # Create Version 1 - Winner is Team1
  df1 <- copy(df)
  
  old_names <- c(
    "WTeamID", "WScore",
    "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF",
    "LTeamID", "LScore",
    "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF"
  )
  
  new_names <- c(
    "T1_TeamID", "T1_Score",
    "T1_FGM", "T1_FGA", "T1_FGM3", "T1_FGA3", "T1_FTM", "T1_FTA",
    "T1_OR", "T1_DR", "T1_Ast", "T1_TO", "T1_Stl", "T1_Blk", "T1_PF",
    "T2_TeamID", "T2_Score",
    "T2_FGM", "T2_FGA", "T2_FGM3", "T2_FGA3", "T2_FTM", "T2_FTA",
    "T2_OR", "T2_DR", "T2_Ast", "T2_TO", "T2_Stl", "T2_Blk", "T2_PF"
  )
  
  setnames(df1, old = old_names, new = new_names)
  
  # Create Version 2 - Loser is Team1
  df2 <- copy(df)
  
  old_names_swap <- c(
    "LTeamID", "LScore",
    "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF",
    "WTeamID", "WScore",
    "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF"
  )
  
  setnames(df2, old = old_names_swap, new = new_names)
  
  # Combine both versions
  output <- rbind(df1, df2)
  
  # Add derived features
  output[, PointDiff := T1_Score - T2_Score]
  output[, win := as.integer(PointDiff > 0)]
  output[, men_women := as.integer(substr(as.character(T1_TeamID), 1, 1) == "1")]
  output[, adjot := NULL]
  
  return(output)
}


regular_data <- prepare_data(regular_results)
tourney_data <- prepare_data(tourney_results)

# Quick check
print(paste("Regular season rows:", nrow(regular_data)))
print(paste("Tournament rows:", nrow(tourney_data)))
head(tourney_data[, .(Season, T1_TeamID, T2_TeamID, T1_Score, T2_Score, PointDiff, win)])



# Easy Features -----------------------------------------------------------

seeds[, seed_num := as.integer(gsub("[^0-9]", "", Seed))]
# Create separate copies for T1 and T2
seeds_T1 <- seeds[, .(Season, TeamID, seed_num)]
seeds_T2 <- seeds[, .(Season, TeamID, seed_num)]
setnames(seeds_T1, c("TeamID", "seed_num"), c("T1_TeamID", "T1_seed"))
setnames(seeds_T2, c("TeamID", "seed_num"), c("T2_TeamID", "T2_seed"))

#Prepare tournament data with seeds
tourney_simple <- tourney_data[, .(Season, T1_TeamID, T2_TeamID, PointDiff, win, men_women)]

#merge seeds

tourney_simple <- merge(tourney_simple, seeds_T1, by = c("Season", "T1_TeamID"), all.x = TRUE)
tourney_simple <- merge(tourney_simple, seeds_T2, by = c("Season", "T2_TeamID"), all.x = TRUE)

# Create seed differential (T2 - T1, so positive means T1 is better seeded)
tourney_simple[, Seed_diff := T2_seed - T1_seed]

cat("Tournament data prepared:\n")
print(head(tourney_simple, 10))

# Remove any rows with missing seeds
tourney_simple <- tourney_simple[!is.na(T1_seed) & !is.na(T2_seed)]
cat("Rows with complete seed data:", nrow(tourney_simple), "\n\n")


# exploratory analysis on seeds -------------------------------------------

cat("1. Average Point Differential by T1 Seed:\n")
seed_summary <- tourney_simple[, .(
  games = .N,
  avg_point_diff = mean(PointDiff),
  sd_point_diff = sd(PointDiff),
  win_rate = mean(win)
), by = .(T1_seed, men_women)]

seed_summary <- seed_summary[order(men_women, T1_seed)]
seed_summary[is.na(sd_point_diff), sd_point_diff := 0]
print(seed_summary)
cat("\n")

# Plot: Point Diff by T1_seed (separated by men/women)
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
cat("2. Average Point Differential by Seed Differential:\n")
seed_diff_summary <- tourney_simple[, .(
  games = .N,
  avg_point_diff = mean(PointDiff),
  sd_point_diff = sd(PointDiff),
  win_rate = mean(win)
), by = .(Seed_diff, men_women)]

seed_diff_summary <- seed_diff_summary[order(men_women, Seed_diff)]
seed_diff_summary[is.na(sd_point_diff), sd_point_diff := 0]
print(seed_diff_summary[abs(Seed_diff) <= 10])  # Show only +/- 10 seed diff
cat("\n")

# Plot: Point Diff by Seed_diff (separated by men/women)
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



# easy season features ----------------------------------------------------

create_simple_stats <- function(regular_data) {
  
  stats <- regular_data[, .(
    games_played = .N,
    win_pct = mean(win),
    avg_score = mean(T1_Score),
    avg_opp_score = mean(T2_Score),
    avg_point_diff = mean(PointDiff)
  ), by = .(Season, T1_TeamID)]
  
  setnames(stats, "T1_TeamID", "TeamID")
  
  return(stats)
}
simple_stats <- create_simple_stats(regular_data)
print(summary(simple_stats))


tourney_with_stats <- copy(tourney_simple)

# Merge T1 stats
tourney_with_stats <- merge(tourney_with_stats, simple_stats,
                            by.x = c("Season", "T1_TeamID"),
                            by.y = c("Season", "TeamID"),
                            all.x = TRUE)
setnames(tourney_with_stats, 
         c("games_played", "win_pct", "avg_score", "avg_opp_score", "avg_point_diff"),
         c("T1_games", "T1_win_pct", "T1_avg_score", "T1_avg_opp_score", "T1_avg_point_diff"))

# Merge T2 stats
tourney_with_stats <- merge(tourney_with_stats, simple_stats,
                            by.x = c("Season", "T2_TeamID"),
                            by.y = c("Season", "TeamID"),
                            all.x = TRUE)
setnames(tourney_with_stats,
         c("games_played", "win_pct", "avg_score", "avg_opp_score", "avg_point_diff"),
         c("T2_games", "T2_win_pct", "T2_avg_score", "T2_avg_opp_score", "T2_avg_point_diff"))

cat("Tournament data with team stats:\n")
print(head(tourney_with_stats))
cat("\n")

cat("Columns:", paste(names(tourney_with_stats), collapse = ", "), "\n\n")

tourney_with_stats[, win_pct_diff := T1_win_pct - T2_win_pct]
tourney_with_stats[, avg_score_diff := T1_avg_score - T2_avg_score]
tourney_with_stats[, avg_opp_score_diff := T1_avg_opp_score - T2_avg_opp_score]
tourney_with_stats[, avg_point_diff_diff := T1_avg_point_diff - T2_avg_point_diff]
diff_cols <- c("Seed_diff", "win_pct_diff", "avg_score_diff", "avg_opp_score_diff", "avg_point_diff_diff")

feature_cols <- c(
  "Seed_diff",
  "T1_win_pct", "T2_win_pct",
  "T1_avg_score", "T2_avg_score",
  "T1_avg_opp_score", "T2_avg_opp_score",
  "T1_avg_point_diff", "T2_avg_point_diff",
  "men_women"
)

tourney_complete <- tourney_with_stats[complete.cases(tourney_with_stats[, ..feature_cols])]


cor_features <- c(feature_cols, "win")
cor_matrix <- cor(tourney_complete[, ..cor_features], use = "complete.obs")
print(round(cor_matrix[, "win"], 3))



# medium features ---------------------------------------------------------

base_metrics <- c("Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", 
                  "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF")

# 1. Raw Team Stats (Layer 1) with clean names
create_clean_stats <- function(reg_data, metrics) {
  cols_to_avg <- c(paste0("T1_", metrics), paste0("T2_", metrics), "PointDiff", "win")
  
  stats <- reg_data[, lapply(.SD, mean), 
                    by = .(Season, T1_TeamID), 
                    .SDcols = cols_to_avg]
  
  # RENAME HERE: T1 becomes 'Off' (Offense), T2 becomes 'Def' (Defense)
  old_names <- c(paste0("T1_", metrics), paste0("T2_", metrics))
  new_names <- c(paste0("Off_", metrics), paste0("Def_", metrics))
  setnames(stats, old_names, new_names)
  
  setnames(stats, "T1_TeamID", "TeamID")
  return(stats)
}

team_stats_clean <- create_clean_stats(regular_data, base_metrics)

# 2. SOS Calculation (Layer 2 & 3)
opp_lookup <- merge(
  regular_data[, .(Season, T1_TeamID, T2_TeamID)], 
  team_stats_clean, 
  by.x = c("Season", "T2_TeamID"), 
  by.y = c("Season", "TeamID")
)

# Average the 'Off' and 'Def' stats of everyone they played
sos_cols <- names(opp_lookup)[!names(opp_lookup) %in% c("Season", "T1_TeamID", "T2_TeamID")]
sos_stats <- opp_lookup[, lapply(.SD, mean), by = .(Season, T1_TeamID), .SDcols = sos_cols]

# Prefix SOS for clarity
setnames(sos_stats, sos_cols, paste0("SOS_", sos_cols))

# 3. Master Table
final_profiles <- merge(team_stats_clean, sos_stats, by.x = c("Season", "TeamID"), by.y = c("Season", "T1_TeamID"))

# 4. START WITH tourney_with_stats (which has easy features + seeds)
# This is the key change - we build on top of easy features instead of starting fresh
tourney_medium <- copy(tourney_with_stats)

# 5. Merge medium features for T1
t1_prof <- copy(final_profiles)
setnames(t1_prof, setdiff(names(t1_prof), c("Season", "TeamID")), 
         paste0("T1_", setdiff(names(t1_prof), c("Season", "TeamID"))))
tourney_medium <- merge(tourney_medium, t1_prof, 
                        by.x = c("Season", "T1_TeamID"), 
                        by.y = c("Season", "TeamID"), 
                        all.x = TRUE)

# 6. Merge medium features for T2
t2_prof <- copy(final_profiles)
setnames(t2_prof, setdiff(names(t2_prof), c("Season", "TeamID")), 
         paste0("T2_", setdiff(names(t2_prof), c("Season", "TeamID"))))
tourney_medium <- merge(tourney_medium, t2_prof, 
                        by.x = c("Season", "T2_TeamID"), 
                        by.y = c("Season", "TeamID"), 
                        all.x = TRUE)

cat("\n=== COMBINED FEATURES ===\n")
cat("Total columns:", ncol(tourney_medium), "\n")
cat("Column names:\n")
print(names(tourney_medium))
cat("\n")

# Check what we have
easy_feature_cols <- c("Seed_diff", "T1_win_pct", "T2_win_pct", "T1_avg_score", 
                       "T2_avg_score", "T1_avg_opp_score", "T2_avg_opp_score",
                       "T1_avg_point_diff", "T2_avg_point_diff", "men_women")
medium_feature_cols <- grep("^T[12]_(Off|Def|SOS)_", names(tourney_medium), value = TRUE)

cat("Easy features present:", sum(easy_feature_cols %in% names(tourney_medium)), "out of", length(easy_feature_cols), "\n")
cat("Medium features present:", length(medium_feature_cols), "\n\n")

# 7. Prepare for XGBoost
check_cols <- setdiff(names(tourney_medium), c("Season", "T1_TeamID", "T2_TeamID", "win", "PointDiff"))
tourney_complete <- tourney_medium[complete.cases(tourney_medium[, ..check_cols])]

cat("Rows before removing NAs:", nrow(tourney_medium), "\n")
cat("Rows after removing NAs:", nrow(tourney_complete), "\n")
cat("Rows dropped:", nrow(tourney_medium) - nrow(tourney_complete), "\n\n")

# XGBoost Training --------------------------------------------------------

# XGBoost outputs raw log-odds (margin scores). We must convert them to probabilities
# using the Sigmoid function (1 / (1 + exp(-x))) before calculating the error.
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  probs <- 1 / (1 + exp(-preds)) 
  err <- mean((probs - labels)^2)
  return(list(metric = "brier_score", value = err))
}

# Prepare Feature Matrix
exclude_cols <- c("Season", "T1_TeamID", "T2_TeamID", "win", "PointDiff", 
                  "T1_seed", "T2_seed", "T1_games", "T2_games")  # Exclude redundant columns
features <- setdiff(names(tourney_complete), exclude_cols)

cat("\n=== FEATURE BREAKDOWN ===\n")
cat("Total features for model:", length(features), "\n")
cat("Easy features:", sum(easy_feature_cols %in% features), "\n")
cat("Medium features:", sum(grepl("^T[12]_(Off|Def|SOS)_", features)), "\n\n")

dtrain <- xgb.DMatrix(
  data = as.matrix(tourney_complete[, ..features]), 
  label = tourney_complete$win
)

# Define Hyperparameters
params <- list(
  objective = "binary:logistic", 
  eta = 0.02,
  max_depth = 4,
  tree_method = "hist"
)

# Cross-Validation
cat("\n--- Running Cross-Validation ---\n")
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 2000,
  nfold = 5,
  custom_metric = evalerror,
  maximize = FALSE,
  early_stopping_rounds = 50,
  print_every_n = 20,
  verbose = TRUE
)

if (is.null(cv_results$best_iteration)) {
  best_nround <- which.min(cv_results$evaluation_log$test_brier_score_mean)
} else {
  best_nround <- cv_results$best_iteration
}

best_brier <- cv_results$evaluation_log$test_brier_score_mean[best_nround]

cat("\n--- OPTIMIZATION RESULTS ---\n")
cat("Optimal Rounds found:", best_nround, "\n")
cat("Best Test Brier Score:", round(best_brier, 5), "\n")

# Train Final Model
cat("\n--- Training Final Production Model ---\n")
final_model <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = best_nround,
  evals = list(train = dtrain),
  custom_metric = evalerror,
  maximize = FALSE,
  print_every_n = 20
)

# Feature Importance
importance_matrix <- xgb.importance(feature_names = features, model = final_model)

cat("\n--- Top 20 Most Important Features ---\n")
print(head(importance_matrix, 20))

# Visualize
xgb.plot.importance(importance_matrix[1:20], main = "Feature Importance (Easy + Medium)")

cat("\n=== EASY + MEDIUM FEATURES COMPLETE ===\n")

# ==============================================================================
# RETRAIN WITH TOP 20 FEATURES ONLY
# ==============================================================================

cat("\n\n=== RETRAINING WITH TOP 20 FEATURES ===\n")

# Extract top 20 feature names
top_20_features <- importance_matrix$Feature[1:20]

cat("Top 20 features selected:\n")
print(top_20_features)
cat("\n")

# Create new training matrix with only top 20 features
dtrain_top20 <- xgb.DMatrix(
  data = as.matrix(tourney_complete[, ..top_20_features]), 
  label = tourney_complete$win
)

# Cross-Validation with Top 20
cat("\n--- Running Cross-Validation (Top 20) ---\n")
cv_results_top20 <- xgb.cv(
  params = params,
  data = dtrain_top20,
  nrounds = 2000,
  nfold = 5,
  custom_metric = evalerror,
  maximize = FALSE,
  early_stopping_rounds = 50,
  print_every_n = 20,
  verbose = TRUE
)

if (is.null(cv_results_top20$best_iteration)) {
  best_nround_top20 <- which.min(cv_results_top20$evaluation_log$test_brier_score_mean)
} else {
  best_nround_top20 <- cv_results_top20$best_iteration
}

best_brier_top20 <- cv_results_top20$evaluation_log$test_brier_score_mean[best_nround_top20]

cat("\n--- TOP 20 OPTIMIZATION RESULTS ---\n")
cat("Optimal Rounds found:", best_nround_top20, "\n")
cat("Best Test Brier Score:", round(best_brier_top20, 5), "\n")

# Compare to full model
cat("\n--- COMPARISON ---\n")
cat("Full Model (", length(features), " features) - Brier:", round(best_brier, 5), "\n")
cat("Top 20 Model - Brier:", round(best_brier_top20, 5), "\n")
cat("Difference:", round(best_brier_top20 - best_brier, 5), "\n")

# Train Final Model with Top 20
cat("\n--- Training Final Model (Top 20) ---\n")
final_model_top20 <- xgb.train(
  params = params, 
  data = dtrain_top20, 
  nrounds = best_nround_top20,
  evals = list(train = dtrain_top20),
  custom_metric = evalerror,
  maximize = FALSE,
  print_every_n = 20
)

# Feature Importance for Top 20 model
importance_top20 <- xgb.importance(feature_names = top_20_features, model = final_model_top20)
cat("\n--- Feature Importance (Top 20 Model) ---\n")
print(importance_top20)

xgb.plot.importance(importance_top20, main = "Feature Importance (Top 20 Only)")

cat("\n=== TOP 20 MODEL TRAINING COMPLETE ===\n")


# ==============================================================================
# PREDICT 2025 WITH TOP 20 MODEL
# ==============================================================================

file_path <- "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/2025_games_kaggle.csv"

if(file.exists(file_path)) {
  cat("\n=== LOADING 2025 TEST DATA (TOP 20 MODEL) ===\n")
  test_games <- fread(file_path)
  
  # Prepare test matchups in standard format (lower ID = T1)
  test_set <- test_games[, .(
    Season,
    T1_TeamID = ifelse(WTeamID < LTeamID, WTeamID, LTeamID),
    T2_TeamID = ifelse(WTeamID > LTeamID, WTeamID, LTeamID),
    Actual_Result = ifelse(WTeamID < LTeamID, 1, 0)
  )]
  
  cat("Test games loaded:", nrow(test_set), "\n\n")
  
  # --- MERGE EASY FEATURES ---
  
  # Merge simple stats for T1
  test_set <- merge(test_set, simple_stats,
                    by.x = c("Season", "T1_TeamID"),
                    by.y = c("Season", "TeamID"),
                    all.x = TRUE)
  setnames(test_set, 
           c("games_played", "win_pct", "avg_score", "avg_opp_score", "avg_point_diff"),
           c("T1_games", "T1_win_pct", "T1_avg_score", "T1_avg_opp_score", "T1_avg_point_diff"))
  
  # Merge simple stats for T2
  test_set <- merge(test_set, simple_stats,
                    by.x = c("Season", "T2_TeamID"),
                    by.y = c("Season", "TeamID"),
                    all.x = TRUE)
  setnames(test_set,
           c("games_played", "win_pct", "avg_score", "avg_opp_score", "avg_point_diff"),
           c("T2_games", "T2_win_pct", "T2_avg_score", "T2_avg_opp_score", "T2_avg_point_diff"))
  
  # Merge seeds for T1
  test_set <- merge(test_set, seeds_T1, 
                    by = c("Season", "T1_TeamID"), 
                    all.x = TRUE)
  
  # Merge seeds for T2
  test_set <- merge(test_set, seeds_T2, 
                    by = c("Season", "T2_TeamID"), 
                    all.x = TRUE)
  
  # Create seed differential
  test_set[, Seed_diff := T2_seed - T1_seed]
  
  # Add men/women indicator
  test_set[, men_women := as.integer(substr(as.character(T1_TeamID), 1, 1) == "1")]
  
  # Create differential features
  test_set[, win_pct_diff := T1_win_pct - T2_win_pct]
  test_set[, avg_score_diff := T1_avg_score - T2_avg_score]
  test_set[, avg_opp_score_diff := T1_avg_opp_score - T2_avg_opp_score]
  test_set[, avg_point_diff_diff := T1_avg_point_diff - T2_avg_point_diff]
  
  # --- MERGE MEDIUM FEATURES ---
  
  # Merge T1 medium features
  t1_prof_test <- copy(final_profiles)
  setnames(t1_prof_test, setdiff(names(t1_prof_test), c("Season", "TeamID")), 
           paste0("T1_", setdiff(names(t1_prof_test), c("Season", "TeamID"))))
  test_set <- merge(test_set, t1_prof_test, 
                    by.x = c("Season", "T1_TeamID"), 
                    by.y = c("Season", "TeamID"), 
                    all.x = TRUE)
  
  # Merge T2 medium features
  t2_prof_test <- copy(final_profiles)
  setnames(t2_prof_test, setdiff(names(t2_prof_test), c("Season", "TeamID")), 
           paste0("T2_", setdiff(names(t2_prof_test), c("Season", "TeamID"))))
  test_set <- merge(test_set, t2_prof_test, 
                    by.x = c("Season", "T2_TeamID"), 
                    by.y = c("Season", "TeamID"), 
                    all.x = TRUE)
  
  cat("\n=== TEST SET FEATURE CHECK (TOP 20) ===\n")
  cat("Total columns:", ncol(test_set), "\n")
  cat("Rows before filtering:", nrow(test_set), "\n")
  
  # Filter to complete cases using ONLY top 20 features
  valid_test_top20 <- test_set[complete.cases(test_set[, ..top_20_features])]
  
  cat("Rows with complete features (top 20):", nrow(valid_test_top20), "\n\n")
  
  if(nrow(valid_test_top20) > 0) {
    # Create prediction matrix
    d2025_top20 <- xgb.DMatrix(data = as.matrix(valid_test_top20[, ..top_20_features]))
    
    # Get predictions from BOTH models
    probs_full <- predict(final_model, xgb.DMatrix(data = as.matrix(valid_test_top20[, ..features])))
    probs_top20 <- predict(final_model_top20, d2025_top20)
    
    # Check if sigmoid needed
    if(min(probs_full) < 0 || max(probs_full) > 1) { 
      probs_full <- 1 / (1 + exp(-probs_full))
    }
    if(min(probs_top20) < 0 || max(probs_top20) > 1) { 
      probs_top20 <- 1 / (1 + exp(-probs_top20))
    }
    
    # Calculate metrics for BOTH models
    predictions_full <- ifelse(probs_full > 0.5, 1, 0)
    predictions_top20 <- ifelse(probs_top20 > 0.5, 1, 0)
    
    acc_full <- mean(predictions_full == valid_test_top20$Actual_Result)
    brier_full <- mean((probs_full - valid_test_top20$Actual_Result)^2)
    
    acc_top20 <- mean(predictions_top20 == valid_test_top20$Actual_Result)
    brier_top20 <- mean((probs_top20 - valid_test_top20$Actual_Result)^2)
    
    cat("\n=== 2025 PERFORMANCE COMPARISON ===\n")
    cat("FULL MODEL (", length(features), " features):\n")
    cat("  Accuracy:    ", round(acc_full * 100, 2), "%\n")
    cat("  Brier Score: ", round(brier_full, 5), "\n\n")
    
    cat("TOP 20 MODEL:\n")
    cat("  Accuracy:    ", round(acc_top20 * 100, 2), "%\n")
    cat("  Brier Score: ", round(brier_top20, 5), "\n\n")
    
    cat("DIFFERENCE:\n")
    cat("  Accuracy:    ", round((acc_top20 - acc_full) * 100, 2), "% points\n")
    cat("  Brier Score: ", round(brier_top20 - brier_full, 5), "\n\n")
    
    # Create results table with BOTH predictions
    results_comparison <- valid_test_top20[, .(Season, T1_TeamID, T2_TeamID, T1_seed, T2_seed, 
                                               Seed_diff, Actual_Result)]
    results_comparison$Prob_Full <- round(probs_full, 4)
    results_comparison$Prob_Top20 <- round(probs_top20, 4)
    results_comparison$Pred_Full <- predictions_full
    results_comparison$Pred_Top20 <- predictions_top20
    results_comparison$Full_Correct <- results_comparison$Pred_Full == results_comparison$Actual_Result
    results_comparison$Top20_Correct <- results_comparison$Pred_Top20 == results_comparison$Actual_Result
    results_comparison$Agreement <- results_comparison$Pred_Full == results_comparison$Pred_Top20
    
    cat("=== SAMPLE PREDICTIONS (COMPARISON) ===\n")
    print(head(results_comparison[, .(T1_TeamID, T2_TeamID, Actual_Result, 
                                      Prob_Full, Prob_Top20, 
                                      Pred_Full, Pred_Top20, Agreement)], 15))
    
    cat("\n=== PREDICTION SUMMARY ===\n")
    cat("Full Model - Correct:", sum(results_comparison$Full_Correct), 
        "Incorrect:", sum(!results_comparison$Full_Correct), "\n")
    cat("Top 20 Model - Correct:", sum(results_comparison$Top20_Correct), 
        "Incorrect:", sum(!results_comparison$Top20_Correct), "\n")
    cat("Agreement rate:", round(mean(results_comparison$Agreement) * 100, 2), "%\n")
    
  } else {
    cat("ERROR: No valid test cases after merging features!\n")
    cat("Check that 2025 teams exist in regular season data.\n")
  }
  
} else {
  cat("\n!!! FILE NOT FOUND !!!\n")
  cat("Expected path:", file_path, "\n")
  cat("Please verify the file exists and path is correct.\n")
}

cat("\n=== TOP 20 MODEL COMPARISON COMPLETE ===\n")

# ==============================================================================
# PREDICT 2025
# ==============================================================================

file_path <- "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/2025_games_kaggle.csv"


if(file.exists(file_path)) {
  cat("\n=== LOADING 2025 TEST DATA ===\n")
  test_games <- fread(file_path)
  
  # Prepare test matchups in standard format (lower ID = T1)
  test_set <- test_games[, .(
    Season,
    T1_TeamID = ifelse(WTeamID < LTeamID, WTeamID, LTeamID),
    T2_TeamID = ifelse(WTeamID > LTeamID, WTeamID, LTeamID),
    Actual_Result = ifelse(WTeamID < LTeamID, 1, 0)
  )]
  
  cat("Test games loaded:", nrow(test_set), "\n\n")
  
  # --- MERGE EASY FEATURES ---
  
  # Merge simple stats for T1
  test_set <- merge(test_set, simple_stats,
                    by.x = c("Season", "T1_TeamID"),
                    by.y = c("Season", "TeamID"),
                    all.x = TRUE)
  setnames(test_set, 
           c("games_played", "win_pct", "avg_score", "avg_opp_score", "avg_point_diff"),
           c("T1_games", "T1_win_pct", "T1_avg_score", "T1_avg_opp_score", "T1_avg_point_diff"))
  
  # Merge simple stats for T2
  test_set <- merge(test_set, simple_stats,
                    by.x = c("Season", "T2_TeamID"),
                    by.y = c("Season", "TeamID"),
                    all.x = TRUE)
  setnames(test_set,
           c("games_played", "win_pct", "avg_score", "avg_opp_score", "avg_point_diff"),
           c("T2_games", "T2_win_pct", "T2_avg_score", "T2_avg_opp_score", "T2_avg_point_diff"))
  
  # Merge seeds for T1
  test_set <- merge(test_set, seeds_T1, 
                    by = c("Season", "T1_TeamID"), 
                    all.x = TRUE)
  
  # Merge seeds for T2
  test_set <- merge(test_set, seeds_T2, 
                    by = c("Season", "T2_TeamID"), 
                    all.x = TRUE)
  
  # Create seed differential
  test_set[, Seed_diff := T2_seed - T1_seed]
  
  # Add men/women indicator
  test_set[, men_women := as.integer(substr(as.character(T1_TeamID), 1, 1) == "1")]
  
  # Create differential features
  test_set[, win_pct_diff := T1_win_pct - T2_win_pct]
  test_set[, avg_score_diff := T1_avg_score - T2_avg_score]
  test_set[, avg_opp_score_diff := T1_avg_opp_score - T2_avg_opp_score]
  test_set[, avg_point_diff_diff := T1_avg_point_diff - T2_avg_point_diff]
  
  # --- MERGE MEDIUM FEATURES ---
  
  # Merge T1 medium features
  t1_prof_test <- copy(final_profiles)
  setnames(t1_prof_test, setdiff(names(t1_prof_test), c("Season", "TeamID")), 
           paste0("T1_", setdiff(names(t1_prof_test), c("Season", "TeamID"))))
  test_set <- merge(test_set, t1_prof_test, 
                    by.x = c("Season", "T1_TeamID"), 
                    by.y = c("Season", "TeamID"), 
                    all.x = TRUE)
  
  # Merge T2 medium features
  t2_prof_test <- copy(final_profiles)
  setnames(t2_prof_test, setdiff(names(t2_prof_test), c("Season", "TeamID")), 
           paste0("T2_", setdiff(names(t2_prof_test), c("Season", "TeamID"))))
  test_set <- merge(test_set, t2_prof_test, 
                    by.x = c("Season", "T2_TeamID"), 
                    by.y = c("Season", "TeamID"), 
                    all.x = TRUE)
  
  cat("\n=== TEST SET FEATURE CHECK ===\n")
  cat("Total columns:", ncol(test_set), "\n")
  cat("Rows before filtering:", nrow(test_set), "\n")
  
  # Filter to complete cases using same features as training
  valid_test <- test_set[complete.cases(test_set[, ..features])]
  
  cat("Rows with complete features:", nrow(valid_test), "\n\n")
  
  if(nrow(valid_test) > 0) {
    # Create prediction matrix
    d2025 <- xgb.DMatrix(data = as.matrix(valid_test[, ..features]))
    
    # Get predictions
    probs <- predict(final_model, d2025)
    
    # Note: binary:logistic should output probabilities directly
    # But check just in case
    if(min(probs) < 0 || max(probs) > 1) { 
      probs <- 1 / (1 + exp(-probs))
      cat("Applied sigmoid conversion to predictions\n")
    }
    
    # Calculate metrics
    predictions <- ifelse(probs > 0.5, 1, 0)
    acc <- mean(predictions == valid_test$Actual_Result)
    brier <- mean((probs - valid_test$Actual_Result)^2)
    
    cat("\n=== 2025 PERFORMANCE ===\n")
    cat("Accuracy:    ", round(acc * 100, 2), "%\n")
    cat("Brier Score: ", round(brier, 5), "\n\n")
    
    # Create results table
    results_2025 <- valid_test[, .(Season, T1_TeamID, T2_TeamID, T1_seed, T2_seed, 
                                   Seed_diff, Actual_Result)]
    results_2025$Predicted <- predictions
    results_2025$Prob_T1_Win <- round(probs, 4)
    results_2025$Correct <- results_2025$Predicted == results_2025$Actual_Result
    
    cat("=== SAMPLE PREDICTIONS ===\n")
    print(head(results_2025, 15))
    
    cat("\n=== PREDICTION SUMMARY ===\n")
    cat("Correct predictions:", sum(results_2025$Correct), "\n")
    cat("Incorrect predictions:", sum(!results_2025$Correct), "\n")
    cat("Average confidence (prob):", round(mean(abs(probs - 0.5) + 0.5), 3), "\n")
    
  } else {
    cat("ERROR: No valid test cases after merging features!\n")
    cat("Check that 2025 teams exist in regular season data.\n")
  }
  
} else {
  cat("\n!!! FILE NOT FOUND !!!\n")
  cat("Expected path:", file_path, "\n")
  cat("Please verify the file exists and path is correct.\n")
}

cat("\n=== 2025 PREDICTION SECTION COMPLETE ===\n")

