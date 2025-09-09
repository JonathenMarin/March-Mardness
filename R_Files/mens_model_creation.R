library(dplyr)
library(readr)
library(data.table)
#Season Data
mens_results <- fread("march-machine-learning-mania-2025/MRegularSeasonDetailedResults.csv")
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
training_data_mens <- tourney_results_mens %>% filter(Season < 2025 & Season >= 2003)

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


mens_model <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
                  data = model_data_mens,
                  family = binomial(link = "logit")) #model with the differences
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

first_place_preds <- read.csv("Excel_Files/first_place_pred.csv")
first_place_preds <- first_place_preds %>% rename(Pred_Winner = Pred)

comparison_df <- submission_file %>% 
  left_join(first_place_preds, by = "ID")
write.csv(comparison_df, "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/DO_Model_Mens_Comparison.csv")
