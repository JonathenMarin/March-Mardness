library(dplyr)
library(readr)
library(data.table)
#Season Data
mens_results <- fread("march-machine-learning-mania-2025/MRegularSeasonDetailedResults.csv")
#2025
game_factors <- mens_results %>% 
  mutate(
    W_Poss = WFGA + 0.44 * WFTA + WTO,
    L_Poss = LFGA + 0.44 * LFTA + LTO,
    W_eFG = (WFGM + 0.5 * WFGM3)/WFGA,
    W_TOV = WTO / W_Poss,
    W_ORB = WOR / (WOR + LDR),
    W_FTR = WFTM / WFGA,
    L_eFG = (LFGM + 0.5 * LFGM3) / LFGA,
    L_TOV = LTO / L_Poss,
    L_ORB = LOR / (LOR + WDR), 
    L_FTR = LFTM / LFGA  #generating stats from DO
  )

winners_factors <- game_factors %>%
  select(Season, 
         TeamID = WTeamID, 
         eFG = W_eFG, 
         TOV_Pct = W_TOV, 
         ORB_Pct = W_ORB, 
         FTR = W_FTR) %>%
  mutate(Win = 1) #1 if they won, winners stats

losers_factors <- game_factors %>%
  select(Season, 
         TeamID = LTeamID, 
         eFG = L_eFG, 
         TOV_Pct = L_TOV, 
         ORB_Pct = L_ORB, 
         FTR = L_FTR) %>%
  mutate(Win = 0) #0 if they lost, loser stats

four_factors_mens <- bind_rows(winners_factors, losers_factors) %>%
  arrange(Season, TeamID) %>%
  group_by(Season, TeamID) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    .groups = "drop" #combine the two factors into one column and take the mean by season and teamID
  )

View(four_factors_mens)

#Tournament Data
tourney_results_mens <- read.csv("march-machine-learning-mania-2025/MNCAATourneyCompactResults.csv")
training_data_mens <- tourney_results_mens %>% filter(Season >= 2003)

training_data_mens <- training_data_mens %>%
  mutate(
    # Identify the team with the lower ID and the higher ID
    Team1 = pmin(WTeamID, LTeamID),
    Team2 = pmax(WTeamID, LTeamID),
  ) %>%
  mutate(
    # Create the target variable: 1 if Team1 won, 0 otherwise
    Team1_win = if_else(Team1 == WTeamID, 1, 0)
  ) %>%
  # Select only the columns we need
  select(Season, Team1, Team2, Team1_win)
#Merging the two files


model_data_mens <- training_data_mens %>% 
  left_join(four_factors_mens, by = c("Season", "Team1" = "TeamID")) %>% 
  rename_with(~paste0(., "_T1"), .cols = -c(Season, Team1, Team2, Team1_win)) %>% #joining team1 data
  
  left_join(four_factors_mens, by = c("Season", "Team2" = "TeamID")) %>% 
  rename_with(~paste0(., "_T2"), .cols = -c(Season, Team1, Team2, Team1_win, ends_with("_T1")))
View(model_data_mens)  #join team2 data

#differentials in stats

model_data_mens <- model_data_mens %>% 
  mutate(
    eFG_diff    = eFG_T1 - eFG_T2,
    TOV_Pct_diff = TOV_Pct_T1 - TOV_Pct_T2,
    ORB_Pct_diff = ORB_Pct_T1 - ORB_Pct_T2,
    FTR_diff     = FTR_T1 - FTR_T2,
    Win_diff     = Win_T1 - Win_T2
  )

write.csv(model_data_mens, "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/mens_data_model_ready.csv")
#dataframe with team1 vs team 2, both teams season averages (that year) and the difference between the two, saying who won

mens_model <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
                  data = model_data_mens,
                  family = binomial(link = "logit")) #model with the differences ONLY
summary(mens_model)


matchups_2025 <- read.csv("Excel_Files/Mens_Sample_Submission_2025.csv") #bring in the file we created earlier

prediction_data <- matchups_2025 %>% 
  select(Season, Team1 = lower_id, Team2 = higher_id, ID = matchup_id, Actual_Result = outcome)

prediction_data <- prediction_data %>% 
  left_join(four_factors_mens, by = c("Season", "Team1" = "TeamID")) %>%
  rename(eFG_T1 = eFG, TOV_Pct_T1 = TOV_Pct, ORB_Pct_T1 = ORB_Pct, FTR_T1 = FTR, Win_T1 = Win) %>%
  
  left_join(four_factors_mens, by = c("Season", "Team2" = "TeamID")) %>%
  rename(eFG_T2 = eFG, TOV_Pct_T2 = TOV_Pct, ORB_Pct_T2 = ORB_Pct, FTR_T2 = FTR, Win_T2 = Win)

prediction_data <- prediction_data %>%
  mutate(
    eFG_diff     = eFG_T1 - eFG_T2,
    TOV_Pct_diff = TOV_Pct_T1 - TOV_Pct_T2,
    ORB_Pct_diff = ORB_Pct_T1 - ORB_Pct_T2,
    FTR_diff     = FTR_T1 - FTR_T2,
    Win_diff     = Win_T1 - Win_T2
  )
prediction_data$Pred <- predict(
  mens_model,
  newdata = prediction_data,
  type = "response"
)

brier_score_value <- mean((prediction_data$Pred - prediction_data$Actual_Result)^2)

submission_file <- prediction_data %>% 
  mutate(
    Per_Game_Error = (Pred- Actual_Result)^2) %>% 
  select(ID, Pred, Actual_Result, Per_Game_Error) %>% 
  mutate(OveralBrier = brier_score_value)
View(submission_file)

write.csv(submission_file, "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/DO_Model_Mens.csv")

first_place_preds <- read.csv("Excel_Files/first_place_pred_kaggle.csv")
first_place_preds <- first_place_preds %>% rename(Pred_Winner = Pred)

comparison_df <- submission_file %>% 
  left_join(first_place_preds, by = "ID")
write.csv(comparison_df, "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/final_report_2025")





#--------------------------------------------------- 2023 below
training_data_pre_2023 <- model_data_mens %>% filter(Season < 2023)

mens_model_2023 <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff+ ORB_Pct_diff + FTR_diff + Win_diff,
                       data = training_data_pre_2023,
                       family = binomial(link = "logit")
) 


mens_2023_games <- read.csv("march-machine-learning-mania-2025/MNCAATourneyCompactResults.csv")
mens_2023_games <- mens_2023_games %>% filter(Season == "2023") 

prediction_data_2023 <- mens_2023_games %>% 
  mutate(
    Team1 = pmin(WTeamID, LTeamID),
    Team2 = pmax(WTeamID, LTeamID),
    ID = paste(Season, Team1, Team2, sep = "_"),
    Actual_Result = if_else(Team1 == WTeamID, 1, 0)
  ) %>% 
select(Season, Team1, Team2, ID, Actual_Result)
    

prediction_data_2023 <- prediction_data_2023 %>% 
  left_join(four_factors_mens, by = c("Season", "Team1" = "TeamID")) %>% 
  rename(eFG_T1 = eFG, TOV_Pct_T1 = TOV_Pct, ORB_Pct_T1 = ORB_Pct, FTR_T1 = FTR, Win_T1 = Win) %>%
  left_join(four_factors_mens, by = c("Season", "Team2" = "TeamID")) %>%
  rename(eFG_T2 = eFG, TOV_Pct_T2 = TOV_Pct, ORB_Pct_T2 = ORB_Pct, FTR_T2 = FTR, Win_T2 = Win)

prediction_data_2023 <- prediction_data_2023 %>%
  mutate(
    # Calculate the difference for every stat. Format is always T1 - T2.
    eFG_diff     = eFG_T1 - eFG_T2,
    TOV_Pct_diff = TOV_Pct_T1 - TOV_Pct_T2,
    ORB_Pct_diff = ORB_Pct_T1 - ORB_Pct_T2,
    FTR_diff     = FTR_T1 - FTR_T2,
    Win_diff     = Win_T1 - Win_T2
  )

prediction_data_2023 %>% 
  select(ID, Actual_Result, ends_with("_diff")) %>% 
  head()

prediction_data_2023$Pred <- predict(
  mens_model_2023, # Using the model trained only up to 2022
  newdata = prediction_data_2023,
  type = "response"      # Get probabilities
)

brier_score_2023 <- mean((prediction_data_2023$Pred - prediction_data_2023$Actual_Result)^2)

final_report_2023 <- prediction_data_2023 %>%
  select(ID, Pred, Actual_Result) %>%
  mutate(OverallBrierScore = brier_score_2023,
         Per_Game_Brier = (Pred-Actual_Result)^2)
write.csv(final_report_2023, "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/DO_Model_Differences/final_report_2023_per_game.csv", 
          row.names = FALSE)

#------------------------------ 2024
training_data_pre_2024 <- model_data_mens %>%
  filter(Season < 2024)

mens_model_for_2024 <- glm(
  Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
  data = training_data_pre_2024,
  family = binomial(link = "logit")
)

mens_results_2024 <- read.csv("march-machine-learning-mania-2025/MNCAATourneyCompactResults.csv")

prediction_data_2024 <- mens_results_2024 %>% 
  filter(Season == 2024) %>% 
  mutate(
    Team1 = pmin(WTeamID, LTeamID),
    Team2 = pmax(WTeamID, LTeamID),
    ID = paste(Season, Team1, Team2, sep = "_"),
    Actual_Result = if_else(Team1 == WTeamID, 1, 0)
  ) %>%
  select(Season, Team1, Team2, ID, Actual_Result)

prediction_data_2024 <- prediction_data_2024 %>%
left_join(four_factors_mens, by = c("Season", "Team1" = "TeamID")) %>%
rename(eFG_T1 = eFG, TOV_Pct_T1 = TOV_Pct, ORB_Pct_T1 = ORB_Pct, FTR_T1 = FTR, Win_T1 = Win) %>%
left_join(four_factors_mens, by = c("Season", "Team2" = "TeamID")) %>%
rename(eFG_T2 = eFG, TOV_Pct_T2 = TOV_Pct, ORB_Pct_T2 = ORB_Pct, FTR_T2 = FTR, Win_T2 = Win)

prediction_data_2024 <- prediction_data_2024 %>%
  mutate(
    # Calculate the difference for every stat. Format is always T1 - T2.
    eFG_diff     = eFG_T1 - eFG_T2,
    TOV_Pct_diff = TOV_Pct_T1 - TOV_Pct_T2,
    ORB_Pct_diff = ORB_Pct_T1 - ORB_Pct_T2,
    FTR_diff     = FTR_T1 - FTR_T2,
    Win_diff     = Win_T1 - Win_T2
  )
prediction_data_2024 %>% 
  select(ID, Actual_Result, ends_with("_diff")) %>% 
  head()

prediction_data_2024$Pred <- predict(
  mens_model_for_2024, # Using the model trained only up to 2023
  newdata = prediction_data_2024,
  type = "response"      # Get probabilities
)
brier_score_2024 <- mean((prediction_data_2024$Pred - prediction_data_2024$Actual_Result)^2)

final_report_2024 <- prediction_data_2024 %>%
  select(ID, Pred, Actual_Result) %>%
  mutate(OverallBrierScore = brier_score_2024,
         Per_Game_Brier = (Pred-Actual_Result)^2)
write.csv(final_report_2024, "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/DO_Model_Differences/final_report_2024.csv", 
          row.names = FALSE)

