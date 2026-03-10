library(data.table)
library(dplyr)
library(xgboost)
library(ggplot2)
library(gridExtra)

# data load ---------------------------------------------------------------

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

# load team names
m_teams <- fread(file.path(data_dir, "MTeams.csv"))
w_teams <- fread(file.path(data_dir, "WTeams.csv"))
all_teams <- rbind(
  m_teams[, .(TeamID, TeamName)],
  w_teams[, .(TeamID, TeamName)]
)

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


# data prep ---------------------------------------------------------------

prepare_data <- function(df) {
  df <- as.data.table(df)
  
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
  
  df[, adjot := (40 + 5 * NumOT) / 40]
  
  stat_cols <- c(
    "WScore", "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF",
    "LScore", "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF"
  )
  df[, (stat_cols) := lapply(.SD, function(x) x / adjot), .SDcols = stat_cols]
  
  # version 1 - winner is T1
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
  
  # version 2 - loser is T1
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
  
  output <- rbind(df1, df2)
  output[, PointDiff := T1_Score - T2_Score]
  output[, win := as.integer(PointDiff > 0)]
  output[, men_women := as.integer(substr(as.character(T1_TeamID), 1, 1) == "1")]
  output[, adjot := NULL]
  
  return(output)
}

regular_data <- prepare_data(regular_results)
tourney_data <- prepare_data(tourney_results)

print(paste("Regular season rows:", nrow(regular_data)))
print(paste("Tournament rows:", nrow(tourney_data)))
head(tourney_data[, .(Season, T1_TeamID, T2_TeamID, T1_Score, T2_Score, PointDiff, win)])


# easy features -----------------------------------------------------------

seeds[, seed_num := as.integer(gsub("[^0-9]", "", Seed))]
seeds_T1 <- seeds[, .(Season, TeamID, seed_num)]
seeds_T2 <- seeds[, .(Season, TeamID, seed_num)]
setnames(seeds_T1, c("TeamID", "seed_num"), c("T1_TeamID", "T1_seed"))
setnames(seeds_T2, c("TeamID", "seed_num"), c("T2_TeamID", "T2_seed"))

tourney_simple <- tourney_data[, .(Season, T1_TeamID, T2_TeamID, PointDiff, win, men_women)]
tourney_simple <- merge(tourney_simple, seeds_T1, by = c("Season", "T1_TeamID"), all.x = TRUE)
tourney_simple <- merge(tourney_simple, seeds_T2, by = c("Season", "T2_TeamID"), all.x = TRUE)
tourney_simple[, Seed_diff := T2_seed - T1_seed]
tourney_simple <- tourney_simple[!is.na(T1_seed) & !is.na(T2_seed)]
cat("Rows with complete seed data:", nrow(tourney_simple), "\n\n")

# exploratory analysis on seeds -------------------------------------------

seed_summary <- tourney_simple[, .(
  games = .N,
  avg_point_diff = mean(PointDiff),
  sd_point_diff = sd(PointDiff),
  win_rate = mean(win)
), by = .(T1_seed, men_women)]

seed_summary <- seed_summary[order(men_women, T1_seed)]
seed_summary[is.na(sd_point_diff), sd_point_diff := 0]

# plot 1 - point diff by seed
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
  labs(title = "Average Point Differential by Team 1 Seed",
       x = "Team 1 Seed",
       y = "Average Point Differential",
       color = "Division",
       fill = "Division") +
  theme_minimal()

# seed differential summary
seed_diff_summary <- tourney_simple[, .(
  games = .N,
  avg_point_diff = mean(PointDiff),
  sd_point_diff = sd(PointDiff),
  win_rate = mean(win)
), by = .(Seed_diff, men_women)]

seed_diff_summary <- seed_diff_summary[order(men_women, Seed_diff)]
seed_diff_summary[is.na(sd_point_diff), sd_point_diff := 0]

# plot 2 - point diff by seed differential
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
  labs(title = "Average Point Differential by Seed Difference",
       subtitle = "Positive Seed_diff means Team 1 is better seeded",
       x = "Seed Difference (T2_seed - T1_seed)",
       y = "Average Point Differential",
       color = "Division",
       fill = "Division") +
  theme_minimal()

library(gridExtra)
print(p1)
print(p2)



# simple season stats -----------------------------------------------------

create_simple_stats <- function(regular_data) {
  stats <- regular_data[, .(
    games_played   = .N,
    win_pct        = mean(win),
    avg_score      = mean(T1_Score),
    avg_opp_score  = mean(T2_Score),
    avg_point_diff = mean(PointDiff)
  ), by = .(Season, T1_TeamID)]
  setnames(stats, "T1_TeamID", "TeamID")
  return(stats)
}

simple_stats <- create_simple_stats(regular_data)


# elo ratings -------------------------------------------------------------

calculate_elo <- function(regular_results, initial_rating = 1500, k = 64, width = 400, hca = 0, carry_over = 0.5) {
  
  games <- regular_results[order(Season, DayNum)]
  seasons <- sort(unique(games$Season))
  all_ratings <- list()
  
  # initialize empty ratings
  team_ratings <- c()
  
  for (s in seasons) {
    season_games <- games[Season == s]
    teams <- unique(c(season_games$WTeamID, season_games$LTeamID))
    
    # between season regression toward mean
    if (length(team_ratings) > 0) {
      # carry over existing teams
      for (tid in as.character(teams)) {
        if (tid %in% names(team_ratings)) {
          team_ratings[tid] <- carry_over * team_ratings[tid] + (1 - carry_over) * initial_rating
        } else {
          # new team starts at initial rating
          team_ratings[tid] <- initial_rating
        }
      }
    } else {
      # first season - everyone starts fresh
      team_ratings <- rep(initial_rating, length(teams))
      names(team_ratings) <- as.character(teams)
    }
    
    for (i in seq_len(nrow(season_games))) {
      wteam <- as.character(season_games$WTeamID[i])
      lteam <- as.character(season_games$LTeamID[i])
      wloc  <- season_games$WLoc[i]
      
      rW <- team_ratings[wteam]
      rL <- team_ratings[lteam]
      
      # home court adjustment
      if (wloc == "H") {
        rW_adj <- rW + hca
      } else if (wloc == "A") {
        rW_adj <- rW - hca
      } else {
        rW_adj <- rW
      }
      
      # expected win probabilities using adjusted rating
      eW <- 1 / (1 + 10^((rL - rW_adj) / width))
      eL <- 1 - eW
      
      # update ratings using unadjusted ratings
      team_ratings[wteam] <- rW + k * (1 - eW)
      team_ratings[lteam] <- rL + k * (0 - eL)
    }
    
    # store final ratings for this season
    season_elo <- data.table(
      Season    = s,
      TeamID    = as.integer(names(team_ratings)),
      Elo_final = as.numeric(team_ratings)
    )
    all_ratings[[as.character(s)]] <- season_elo
  }
  
  return(rbindlist(all_ratings))
}

elo_ratings <- calculate_elo(regular_results)

# top elo teams plot - mens and womens separate ---------------------------

latest_season <- max(elo_ratings$Season)

# mens top 20
top_elo_mens <- elo_ratings[Season == latest_season & TeamID < 3000] %>%
  merge(all_teams, by = "TeamID") %>%
  arrange(desc(Elo_final)) %>%
  head(20) %>%
  distinct(TeamName, .keep_all = TRUE)

top_elo_mens$TeamName <- factor(top_elo_mens$TeamName, 
                                levels = top_elo_mens$TeamName[order(top_elo_mens$Elo_final)])

plot_mens_elo <- ggplot(top_elo_mens, aes(x = TeamName, y = Elo_final, fill = Elo_final)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  coord_flip() +
  labs(
    title = paste("Top 20 Men's Teams 2025", latest_season),
    x = NULL,
    y = "Elo Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold"),
    legend.position = "none"
  )

# womens top 20
top_elo_womens <- elo_ratings[Season == latest_season & TeamID > 3000] %>%
  merge(all_teams, by = "TeamID") %>%
  arrange(desc(Elo_final)) %>%
  head(20) %>%
  distinct(TeamName, .keep_all = TRUE)

top_elo_womens$TeamName <- factor(top_elo_womens$TeamName, 
                                  levels = top_elo_womens$TeamName[order(top_elo_womens$Elo_final)])

plot_womens_elo <- ggplot(top_elo_womens, aes(x = TeamName, y = Elo_final, fill = Elo_final)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  coord_flip() +
  labs(
    title = paste("Top 20 Women's Teams 2025", latest_season),
    x = NULL,
    y = "Elo Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold"),
    legend.position = "none"
  )

grid.arrange(plot_mens_elo, plot_womens_elo, ncol = 2)

elo_T1 <- copy(elo_ratings)
setnames(elo_T1, c("TeamID", "Elo_final"), c("T1_TeamID", "T1_Elo"))

elo_T2 <- copy(elo_ratings)
setnames(elo_T2, c("TeamID", "Elo_final"), c("T2_TeamID", "T2_Elo"))


# build tourney_with_stats ------------------------------------------------

tourney_with_stats <- copy(tourney_simple)

tourney_with_stats <- merge(tourney_with_stats, simple_stats,
                            by.x = c("Season", "T1_TeamID"),
                            by.y = c("Season", "TeamID"),
                            all.x = TRUE)
setnames(tourney_with_stats,
         c("games_played", "win_pct", "avg_score", "avg_opp_score", "avg_point_diff"),
         c("T1_games", "T1_win_pct", "T1_avg_score", "T1_avg_opp_score", "T1_avg_point_diff"))

tourney_with_stats <- merge(tourney_with_stats, simple_stats,
                            by.x = c("Season", "T2_TeamID"),
                            by.y = c("Season", "TeamID"),
                            all.x = TRUE)
setnames(tourney_with_stats,
         c("games_played", "win_pct", "avg_score", "avg_opp_score", "avg_point_diff"),
         c("T2_games", "T2_win_pct", "T2_avg_score", "T2_avg_opp_score", "T2_avg_point_diff"))

tourney_with_stats[, win_pct_diff        := T1_win_pct        - T2_win_pct]
tourney_with_stats[, avg_score_diff      := T1_avg_score      - T2_avg_score]
tourney_with_stats[, avg_opp_score_diff  := T1_avg_opp_score  - T2_avg_opp_score]
tourney_with_stats[, avg_point_diff_diff := T1_avg_point_diff - T2_avg_point_diff]

# merge elo
tourney_with_stats <- merge(tourney_with_stats, elo_T1,
                            by = c("Season", "T1_TeamID"), all.x = TRUE)
tourney_with_stats <- merge(tourney_with_stats, elo_T2,
                            by = c("Season", "T2_TeamID"), all.x = TRUE)
tourney_with_stats[, Elo_diff := T1_Elo - T2_Elo]


# medium features ---------------------------------------------------------

base_metrics <- c("Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA",
                  "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF")

create_clean_stats <- function(reg_data, metrics) {
  cols_to_avg <- c(paste0("T1_", metrics), paste0("T2_", metrics), "PointDiff", "win")
  stats <- reg_data[, lapply(.SD, mean),
                    by = .(Season, T1_TeamID),
                    .SDcols = cols_to_avg]
  old_names <- c(paste0("T1_", metrics), paste0("T2_", metrics))
  new_names <- c(paste0("Off_", metrics), paste0("Def_", metrics))
  setnames(stats, old_names, new_names)
  setnames(stats, "T1_TeamID", "TeamID")
  return(stats)
}

team_stats_clean <- create_clean_stats(regular_data, base_metrics)

# strength of schedule
opp_lookup <- merge(
  regular_data[, .(Season, T1_TeamID, T2_TeamID)],
  team_stats_clean,
  by.x = c("Season", "T2_TeamID"),
  by.y = c("Season", "TeamID")
)
sos_cols  <- names(opp_lookup)[!names(opp_lookup) %in% c("Season", "T1_TeamID", "T2_TeamID")]
sos_stats <- opp_lookup[, lapply(.SD, mean), by = .(Season, T1_TeamID), .SDcols = sos_cols]
setnames(sos_stats, sos_cols, paste0("SOS_", sos_cols))

final_profiles <- merge(team_stats_clean, sos_stats,
                        by.x = c("Season", "TeamID"),
                        by.y = c("Season", "T1_TeamID"))

tourney_medium <- copy(tourney_with_stats)

t1_prof <- copy(final_profiles)
setnames(t1_prof, setdiff(names(t1_prof), c("Season", "TeamID")),
         paste0("T1_", setdiff(names(t1_prof), c("Season", "TeamID"))))
tourney_medium <- merge(tourney_medium, t1_prof,
                        by.x = c("Season", "T1_TeamID"),
                        by.y = c("Season", "TeamID"),
                        all.x = TRUE)

t2_prof <- copy(final_profiles)
setnames(t2_prof, setdiff(names(t2_prof), c("Season", "TeamID")),
         paste0("T2_", setdiff(names(t2_prof), c("Season", "TeamID"))))
tourney_medium <- merge(tourney_medium, t2_prof,
                        by.x = c("Season", "T2_TeamID"),
                        by.y = c("Season", "TeamID"),
                        all.x = TRUE)

cat("Total columns:", ncol(tourney_medium), "\n")

check_cols <- setdiff(names(tourney_medium), c("Season", "T1_TeamID", "T2_TeamID", "win", "PointDiff"))
tourney_complete <- tourney_medium[complete.cases(tourney_medium[, ..check_cols])]

cat("Rows before removing NAs:", nrow(tourney_medium), "\n")
cat("Rows after removing NAs:",  nrow(tourney_complete), "\n")


# xgboost training --------------------------------------------------------

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  probs  <- 1 / (1 + exp(-preds))
  err    <- mean((probs - labels)^2)
  return(list(metric = "brier_score", value = err))
}

exclude_cols <- c("Season", "T1_TeamID", "T2_TeamID", "win", "PointDiff",
                 "T1_games", "T2_games")
features <- setdiff(names(tourney_complete), exclude_cols)

dtrain <- xgb.DMatrix(
  data  = as.matrix(tourney_complete[, ..features]),
  label = tourney_complete$win
)

params <- list(
  objective = "binary:logistic",
  eta = 0.01,
  max_depth = 5,
  subsample = 0.8,
  colsample_bytree = 0.7,
  min_child_weight = 4,
  tree_method = "hist",
  seed = 12
)
set.seed(12)
#testing
cv_results <- xgb.cv(
  params                = params,
  data                  = dtrain,
  nrounds               = 3000,
  nfold                 = 5,
  custom_metric         = evalerror,
  maximize              = FALSE,
  early_stopping_rounds = 50,
  print_every_n         = 20,
  verbose               = TRUE
)

best_nround <- if (is.null(cv_results$best_iteration)) {
  which.min(cv_results$evaluation_log$test_brier_score_mean)
} else {
  cv_results$best_iteration
}

best_brier <- cv_results$evaluation_log$test_brier_score_mean[best_nround]
cat("Optimal Rounds found:", best_nround, "\n")
cat("Best Test Brier Score:", round(best_brier, 5), "\n")


# train final model -------------------------------------------------------

final_model <- xgb.train(
  params        = params,
  data          = dtrain,
  nrounds       = best_nround,
  evals         = list(train = dtrain),
  custom_metric = evalerror,
  maximize      = FALSE,
  print_every_n = 20
)

importance_matrix <- xgb.importance(feature_names = features, model = final_model)
print(head(importance_matrix, 20))
xgb.plot.importance(importance_matrix[1:20], main = "Feature Importance")


# predict 2025 ------------------------------------------------------------

file_path <- "Excel_Files/2025_games_kaggle.csv"

if (file.exists(file_path)) {
  cat("\nLoading 2025 test data...\n")
  test_games <- fread(file_path)
  
  test_set <- test_games[, .(
    Season,
    T1_TeamID     = ifelse(WTeamID < LTeamID, WTeamID, LTeamID),
    T2_TeamID     = ifelse(WTeamID > LTeamID, WTeamID, LTeamID),
    Actual_Result = ifelse(WTeamID < LTeamID, 1, 0)
  )]
  
  test_set <- merge(test_set, simple_stats,
                    by.x = c("Season", "T1_TeamID"),
                    by.y = c("Season", "TeamID"), all.x = TRUE)
  setnames(test_set,
           c("games_played", "win_pct", "avg_score", "avg_opp_score", "avg_point_diff"),
           c("T1_games", "T1_win_pct", "T1_avg_score", "T1_avg_opp_score", "T1_avg_point_diff"))
  
  test_set <- merge(test_set, simple_stats,
                    by.x = c("Season", "T2_TeamID"),
                    by.y = c("Season", "TeamID"), all.x = TRUE)
  setnames(test_set,
           c("games_played", "win_pct", "avg_score", "avg_opp_score", "avg_point_diff"),
           c("T2_games", "T2_win_pct", "T2_avg_score", "T2_avg_opp_score", "T2_avg_point_diff"))
  
  test_set <- merge(test_set, seeds_T1, by = c("Season", "T1_TeamID"), all.x = TRUE)
  test_set <- merge(test_set, seeds_T2, by = c("Season", "T2_TeamID"), all.x = TRUE)
  test_set[, Seed_diff := T2_seed - T1_seed]
  test_set[, men_women := as.integer(substr(as.character(T1_TeamID), 1, 1) == "1")]
  
  test_set[, win_pct_diff        := T1_win_pct        - T2_win_pct]
  test_set[, avg_score_diff      := T1_avg_score      - T2_avg_score]
  test_set[, avg_opp_score_diff  := T1_avg_opp_score  - T2_avg_opp_score]
  test_set[, avg_point_diff_diff := T1_avg_point_diff - T2_avg_point_diff]
  
  test_set <- merge(test_set, elo_T1, by = c("Season", "T1_TeamID"), all.x = TRUE)
  test_set <- merge(test_set, elo_T2, by = c("Season", "T2_TeamID"), all.x = TRUE)
  test_set[, Elo_diff := T1_Elo - T2_Elo]
  
  t1_prof_test <- copy(final_profiles)
  setnames(t1_prof_test, setdiff(names(t1_prof_test), c("Season", "TeamID")),
           paste0("T1_", setdiff(names(t1_prof_test), c("Season", "TeamID"))))
  test_set <- merge(test_set, t1_prof_test,
                    by.x = c("Season", "T1_TeamID"),
                    by.y = c("Season", "TeamID"), all.x = TRUE)
  
  t2_prof_test <- copy(final_profiles)
  setnames(t2_prof_test, setdiff(names(t2_prof_test), c("Season", "TeamID")),
           paste0("T2_", setdiff(names(t2_prof_test), c("Season", "TeamID"))))
  test_set <- merge(test_set, t2_prof_test,
                    by.x = c("Season", "T2_TeamID"),
                    by.y = c("Season", "TeamID"), all.x = TRUE)
  
  cat("Total columns:", ncol(test_set), "\n")
  cat("Rows before filtering:", nrow(test_set), "\n")
  
  valid_test <- test_set[complete.cases(test_set[, ..features])]
  cat("Rows with complete features:", nrow(valid_test), "\n\n")
  
  if (nrow(valid_test) > 0) {
    d2025 <- xgb.DMatrix(data = as.matrix(valid_test[, ..features]))
    probs_2025 <- predict(final_model, d2025)
    
    if (min(probs_2025) < 0 || max(probs_2025) > 1) {
      probs_2025 <- 1 / (1 + exp(-probs_2025))
    }
    
    predictions_2025 <- ifelse(probs_2025 > 0.5, 1, 0)
    acc_2025   <- mean(predictions_2025 == valid_test$Actual_Result)
    brier_2025 <- mean((probs_2025 - valid_test$Actual_Result)^2)
    
    cat("2025 Results:\n")
    cat("  Accuracy:    ", round(acc_2025 * 100, 2), "%\n")
    cat("  Brier Score: ", round(brier_2025, 5), "\n")
    cat("  Winning Score:", 0.10411, "\n")
  }
}