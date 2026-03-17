library(dplyr)
library(data.table)

# ── Functions ─────────────────────────────────────────────────────────────────

calculate_four_factors <- function(detailed_results_df) {
  
  game_factors <- detailed_results_df %>%
    mutate(
      W_Poss = (WFGA - WOR) + WTO + (0.44 * WFTA),
      L_Poss = (LFGA - LOR) + LTO + (0.44 * LFTA),
      W_eFG = (WFGM + 0.5 * WFGM3) / WFGA,
      W_TOV = WTO / W_Poss,
      W_ORB = WOR / (WOR + LDR),
      W_FTR = WFTM / WFGA,
      L_eFG = (LFGM + 0.5 * LFGM3) / LFGA,
      L_TOV = LTO / L_Poss,
      L_ORB = LOR / (LOR + WDR),
      L_FTR = LFTM / LFGA
    )
  
  winners_factors <- game_factors %>%
    select(Season, TeamID = WTeamID, eFG = W_eFG, TOV_Pct = W_TOV, ORB_Pct = W_ORB, FTR = W_FTR) %>%
    mutate(Win = 1)
  
  losers_factors <- game_factors %>%
    select(Season, TeamID = LTeamID, eFG = L_eFG, TOV_Pct = L_TOV, ORB_Pct = L_ORB, FTR = L_FTR) %>%
    mutate(Win = 0)
  
  four_factors_df <- bind_rows(winners_factors, losers_factors) %>%
    arrange(Season, TeamID) %>%
    group_by(Season, TeamID) %>%
    summarise(
      across(where(is.numeric), mean, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(four_factors_df)
}

prepare_model_data <- function(tourney_results_df, four_factors_df) {
  training_data <- tourney_results_df %>%
    mutate(
      Team1 = pmin(WTeamID, LTeamID),
      Team2 = pmax(WTeamID, LTeamID),
      Team1_win = if_else(Team1 == WTeamID, 1, 0)
    ) %>%
    select(Season, Team1, Team2, Team1_win)
  
  model_data <- training_data %>%
    left_join(four_factors_df, by = c("Season", "Team1" = "TeamID")) %>%
    rename_with(~paste0(., "_T1"), .cols = -c(Season, Team1, Team2, Team1_win)) %>%
    left_join(four_factors_df, by = c("Season", "Team2" = "TeamID")) %>%
    rename_with(~paste0(., "_T2"), .cols = -c(Season, Team1, Team2, Team1_win, ends_with("_T1"))) %>%
    mutate(
      eFG_diff     = eFG_T1 - eFG_T2,
      TOV_Pct_diff = TOV_Pct_T1 - TOV_Pct_T2,
      ORB_Pct_diff = ORB_Pct_T1 - ORB_Pct_T2,
      FTR_diff     = FTR_T1 - FTR_T2,
      Win_diff     = Win_T1 - Win_T2
    ) %>%
    na.omit()
  
  return(model_data)
}

prepare_prediction_data <- function(team_ids, four_factors_df, season = 2026, stats_season = 2025) {
  
  matchups <- expand.grid(Team1 = team_ids, Team2 = team_ids) %>%
    filter(Team1 < Team2) %>%
    mutate(Season = season, Team1_win = NA_real_)
  
  factors <- four_factors_df %>%
    filter(Season == stats_season) %>%
    select(-Season)
  
  message("Teams in seed file: ", length(team_ids))
  message("Teams with ", stats_season, " four factors: ", nrow(factors))
  message("Teams with no four factor data: ", sum(!team_ids %in% factors$TeamID))
  
  prediction_data <- matchups %>%
    left_join(factors, by = c("Team1" = "TeamID")) %>%
    rename_with(~paste0(., "_T1"), .cols = -c(Season, Team1, Team2, Team1_win)) %>%
    left_join(factors, by = c("Team2" = "TeamID")) %>%
    rename_with(~paste0(., "_T2"), .cols = -c(Season, Team1, Team2, Team1_win, ends_with("_T1"))) %>%
    mutate(
      eFG_diff     = eFG_T1 - eFG_T2,
      TOV_Pct_diff = TOV_Pct_T1 - TOV_Pct_T2,
      ORB_Pct_diff = ORB_Pct_T1 - ORB_Pct_T2,
      FTR_diff     = FTR_T1 - FTR_T2,
      Win_diff     = Win_T1 - Win_T2
    ) %>%
    mutate(across(where(is.numeric), ~ifelse(is.infinite(.) | is.nan(.), NA, .))) %>%
    # Only filter on predictor columns — Team1_win is intentionally NA for predictions
    filter(if_all(c(eFG_diff, TOV_Pct_diff, ORB_Pct_diff, FTR_diff, Win_diff), ~!is.na(.)))
  
  message("Matchups after filter: ", nrow(prediction_data))
  
  return(prediction_data)
}

# ── Load Data ─────────────────────────────────────────────────────────────────

mens_results           <- fread("march-machine-learning-mania-2026/MRegularSeasonDetailedResults.csv")
womens_results         <- fread("march-machine-learning-mania-2026/WRegularSeasonDetailedResults.csv")
tourney_results_mens   <- fread("march-machine-learning-mania-2026/MNCAATourneyCompactResults.csv")
tourney_results_womens <- fread("march-machine-learning-mania-2026/WNCAATourneyCompactResults.csv")
seeds_mens             <- fread("march-machine-learning-mania-2026/MNCAATourneySeeds.csv")
seeds_womens           <- fread("march-machine-learning-mania-2026/WNCAATourneySeeds.csv")

# ── Build Four Factors ────────────────────────────────────────────────────────

four_factors_mens   <- calculate_four_factors(mens_results)
four_factors_womens <- calculate_four_factors(womens_results)

message("Mens four factors seasons:   ", paste(sort(unique(four_factors_mens$Season)),   collapse = ", "))
message("Womens four factors seasons: ", paste(sort(unique(four_factors_womens$Season)), collapse = ", "))

# ── Prepare Training Data ─────────────────────────────────────────────────────

model_data_mens   <- prepare_model_data(tourney_results_mens   %>% filter(Season >= 2003), four_factors_mens)
model_data_womens <- prepare_model_data(tourney_results_womens %>% filter(Season >= 2010), four_factors_womens)

# ── Train Models (2024 + 2025 seasons) ───────────────────────────────────────

mens_model_2026 <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
                       data = model_data_mens %>% filter(Season %in% c(2024, 2025)),
                       family = binomial(link = "logit"))

womens_model_2026 <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
                         data = model_data_womens %>% filter(Season %in% c(2024, 2025)),
                         family = binomial(link = "logit"))

# ── Extract 2026 Tournament Teams from Seed Files ─────────────────────────────

mens_team_ids_2026   <- seeds_mens   %>% filter(Season == 2026) %>% pull(TeamID)
womens_team_ids_2026 <- seeds_womens %>% filter(Season == 2026) %>% pull(TeamID)

# ── Generate All Possible Matchups ────────────────────────────────────────────

predictions_2026_mens   <- prepare_prediction_data(mens_team_ids_2026,   four_factors_mens,   stats_season = 2025)
predictions_2026_womens <- prepare_prediction_data(womens_team_ids_2026, four_factors_womens, stats_season = 2025)

# ── Predict ───────────────────────────────────────────────────────────────────

predictions_2026_mens$Pred   <- predict(mens_model_2026,   newdata = predictions_2026_mens,   type = "response")
predictions_2026_womens$Pred <- predict(womens_model_2026, newdata = predictions_2026_womens, type = "response")

# ── Format & Write Output ─────────────────────────────────────────────────────

format_submission <- function(predictions_df, season = 2026) {
  predictions_df %>%
    mutate(ID = paste(season, Team1, Team2, sep = "_")) %>%
    select(ID, Pred)
}

submission_2026 <- bind_rows(
  format_submission(predictions_2026_mens),
  format_submission(predictions_2026_womens)
)

write.csv(submission_2026, "Excel_Files/DO_Model_submission_2026.csv", row.names = FALSE)

