library(dplyr)
library(data.table)
library(tidyr)

#functions- the mens_model_creation got ugly so i redid it
calculate_four_factors <- function(detailed_results_df) {
  
  game_factors <- detailed_results_df %>%
    mutate(
      W_Poss = WFGA + 0.44 * WFTA + WTO,
      L_Poss = LFGA + 0.44 * LFTA + LTO,
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
    )
  
  return(model_data)
}
#data loading
mens_results <- fread("march-machine-learning-mania-2025/MRegularSeasonDetailedResults.csv")
womens_results <- fread("march-machine-learning-mania-2025/WRegularSeasonDetailedResults.csv")
tourney_results_mens <- fread("march-machine-learning-mania-2025/MNCAATourneyCompactResults.csv")
tourney_results_womens <- fread("march-machine-learning-mania-2025/WNCAATourneyCompactResults.csv")

#calculations
four_factors_mens <- calculate_four_factors(mens_results)
four_factors_womens <- calculate_four_factors(womens_results)
#data loading
model_data_mens <- prepare_model_data(tourney_results_mens %>% filter(Season >= 2003), four_factors_mens)
model_data_womens <- prepare_model_data(tourney_results_womens %>% filter(Season >= 2010), four_factors_womens)
#2023 models
mens_model_2023 <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
                       data = model_data_mens %>% filter(Season %in% c(2021, 2022)),
                       family = binomial(link = "logit"))
womens_model_2023 <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
                         data = model_data_womens %>% filter(Season %in% c(2021, 2022)),
                         family = binomial(link = "logit"))
#mens 2023
prediction_data_2023_mens <- prepare_model_data(tourney_results_mens %>% filter(Season == 2023), four_factors_mens)
prediction_data_2023_mens$Pred <- predict(mens_model_2023, newdata = prediction_data_2023_mens, type = "response")
#womens 2023
prediction_data_2023_womens <- prepare_model_data(tourney_results_womens %>% filter(Season == 2023), four_factors_womens)
prediction_data_2023_womens$Pred <- predict(womens_model_2023, newdata = prediction_data_2023_womens, type = "response")

combined_report_2023 <- bind_rows(prediction_data_2023_mens, prediction_data_2023_womens) %>% 
  mutate(
    brier_score_2023 = round(mean((Pred - Team1_win)^2, na.rm = TRUE),5),
    winning_brier_score = 0.17371,
    brier_score_per_game = round((Pred - Team1_win)^2,5)
  )

#2024 model and data
mens_model_2024 <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
                       data = model_data_mens %>% filter(Season %in% c(2022, 2023)),
                       family = binomial(link = "logit"))

womens_model_2024 <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
                         data = model_data_womens %>% filter(Season %in% c(2022, 2023)),
                         family = binomial(link = "logit"))
#mens prediction per game 2024
prediction_data_2024_mens <- prepare_model_data(tourney_results_mens %>% filter(Season == 2024), four_factors_mens)
prediction_data_2024_mens$Pred <- predict(mens_model_2024, newdata = prediction_data_2024_mens, type = "response")
#womens prediction per game 2024
prediction_data_2024_womens <- prepare_model_data(tourney_results_womens %>% filter(Season == 2024), four_factors_womens)
prediction_data_2024_womens$Pred <- predict(womens_model_2024, newdata = prediction_data_2024_womens, type = "response")
#1st place has an interesting discussion on the ratings of two teams into probability of a team winning (elo system)
combined_report_2024 <- bind_rows(prediction_data_2024_mens, prediction_data_2024_womens) %>% 
  mutate(
    brier_score_2024 = round(mean((Pred - Team1_win)^2, na.rm = TRUE),5),
    winning_brier_score = 0.05313,
    brier_score_per_game = round((Pred - Team1_win)^2,5)
  ) 


write_csv(combined_report_2023, "C:/Math/Masters/March-Mardness/Excel_Files/DO_Model_Differences/combined_report_2023.csv")
write_csv(combined_report_2024, "C:/Math/Masters/March-Mardness/Excel_Files/DO_Model_Differences/combined_report_2024.csv")
