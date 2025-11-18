library(tidyr)
library(dplyr)
library(data.table)
library(fuzzyjoin)


womens_ranks <- fread("Excel_Files/womens_2025_reg_season_rankings.csv")
womens_spellings <- fread("march-machine-learning-mania-2025/WTeamSpellings.csv")

womens_ranks_norm <- womens_ranks %>%
  mutate(Team_normalized = tolower(Team))
womens_spellings_norm <- womens_spellings %>%
  mutate(TeamNameSpelling_normalized = tolower(TeamNameSpelling))

womens_ranks_matched <- womens_ranks_norm %>%
  left_join(
    womens_spellings_norm %>% select(TeamNameSpelling_normalized, TeamID) %>% distinct(),
    by = c("Team_normalized" = "TeamNameSpelling_normalized")
  )

womens_unmatched <- womens_ranks_matched %>%
  filter(is.na(TeamID))

if(nrow(womens_unmatched) > 0) {
  womens_fuzzy_matches <- stringdist_left_join(
    womens_unmatched %>% select(-TeamID),
    womens_spellings_norm,
    by = c("Team_normalized" = "TeamNameSpelling_normalized"),
    method = "jw",
    max_dist = 0.3,
    distance_col = "dist"
  ) %>%
    group_by(Team) %>%
    slice_min(dist, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # Combine exact and fuzzy matches
  womens_ranks_final <- womens_ranks_matched %>%
    filter(!is.na(TeamID)) %>%
    bind_rows(womens_fuzzy_matches %>% select(names(womens_ranks_matched)))
} else {
  womens_ranks_final <- womens_ranks_matched
}

womens_ranks_final <- womens_ranks_final %>%
  select(-Team_normalized)

cat("Matched women's teams:", sum(!is.na(womens_ranks_final$TeamID)), "out of", nrow(womens_ranks), "\n")

womens_still_unmatched <- womens_ranks_final %>%
  filter(is.na(TeamID))

if(nrow(womens_still_unmatched) > 0) {
  cat("\nStill unmatched women's teams:\n")
  print(womens_still_unmatched$Team)
}


# womens regular season results -------------------------------------------
womens_results_all <- fread("march-machine-learning-mania-2025/WRegularSeasonDetailedResults.csv")
womens_results <- womens_results_all %>% filter(Season == 2025)
cat("Total women's 2025 games:", nrow(womens_results), "\n")

womens_results_with_wteam <- womens_results %>%
  left_join(
    womens_ranks_final %>% select(TeamID, AdjOE, AdjDE, `Adj T.`), # <-- Check these names
    by = c("WTeamID" = "TeamID")
  ) %>%
  rename(
    WTeam_ORtg = AdjOE,
    WTeam_DRtg = AdjDE,
    WTeam_AdjT = `Adj T.`
  )

womens_results_with_both <- womens_results_with_wteam %>% 
  left_join(
    womens_ranks_final %>% select(TeamID, AdjOE, AdjDE, `Adj T.`),
    by = c("LTeamID" = "TeamID")
  ) %>% 
  rename(
    LTeam_ORtg = AdjOE,
    LTeam_DRtg = AdjDE,
    LTeam_AdjT = `Adj T.`
  )
womens_results_clean <- womens_results_with_both %>%
  filter(!is.na(WTeam_ORtg) & !is.na(LTeam_ORtg))

cat("Women's games remaining:", nrow(womens_results_clean), "out of", nrow(womens_results), "\n")
womens_league_avg_rating <- mean(c(womens_ranks_final$AdjOE, womens_ranks_final$AdjDE), na.rm = TRUE)

womens_train_wteam <- womens_results_clean %>%
  transmute(
    Points = WScore,
    OffRating = WTeam_ORtg,
    DefRating = LTeam_DRtg,
    AdjT_team = WTeam_AdjT,
    AdjT_opp = LTeam_AdjT,
    League_Avg = womens_league_avg_rating
  )
womens_train_lteam <- womens_results_clean %>%
  transmute(
    Points = LScore,
    OffRating = LTeam_ORtg,
    DefRating = WTeam_DRtg,
    AdjT_team = LTeam_AdjT,
    AdjT_opp = WTeam_AdjT,
    League_Avg = womens_league_avg_rating
  )
womens_train_data <- bind_rows(womens_train_wteam, womens_train_lteam)
cat("Total women's training samples:", nrow(womens_train_data), "\n")
summary(womens_train_data)

womens_model <- lm(Points ~ OffRating + DefRating + AdjT_team + AdjT_opp,
                   data = womens_train_data)
summary(womens_model)
cat("\nWomen's Model Coefficients:\n")
print(coef(womens_model))

womens_sigma <- summary(womens_model)$sigma
cat("Women's model standard deviation (sigma):", womens_sigma, "\n")

# simulation functions ----------------------------------------------------

predict_matchup <- function(team_a_id, team_b_id, kenpom_data, model, sigma) {
  team_a <- kenpom_data %>% filter(TeamID == team_a_id)
  team_b <- kenpom_data %>% filter(TeamID == team_b_id)
  
  if(nrow(team_a) == 0 | nrow(team_b) == 0) {
    warning(paste("Missing KenPom data for team", team_a_id, "or", team_b_id))
    return(list(team_a_win_prob = 0.5, team_b_win_prob = 0.5, avg_margin = 0))
  }
  
  # Predict Team A's score
  # *** This newdata data.frame MUST use the same predictor names as the model ***
  team_a_pred <- predict(model, newdata = data.frame(
    OffRating = team_a$AdjOE,
    DefRating = team_b$AdjDE,
    AdjT_team = team_a$`Adj T.`,
    AdjT_opp = team_b$`Adj T.`
  ))
  
  # Predict Team B's score
  team_b_pred <- predict(model, newdata = data.frame(
    OffRating = team_b$AdjOE,
    DefRating = team_a$AdjDE,
    AdjT_team = team_b$`Adj T.`,
    AdjT_opp = team_a$`Adj T.`
  ))
  
  # Simulate game
  result <- simulate_game(team_a_pred, team_b_pred, sigma, n_sims = 10000)
  
  return(result)
}

# womens tournament -------------------------------------------------------
womens_tournament_games <- fread("Excel_Files/2025_games_kaggle.csv")
womens_tournament_games_subset <- womens_tournament_games[68:135, ] 

# Filter out rows with NA team IDs
womens_tournament_games_subset <- womens_tournament_games_subset %>%
  filter(!is.na(LTeamID) & !is.na(WTeamID))

cat("Women's tournament games to predict:", nrow(womens_tournament_games_subset), "\n")

womens_predictions_list <- list()

for(i in 1:nrow(womens_tournament_games_subset)) {
  game <- womens_tournament_games_subset[i, ]
  
  lower_id <- min(game$LTeamID, game$WTeamID)
  higher_id <- max(game$LTeamID, game$WTeamID)
  
  game_id <- paste(game$Season, lower_id, higher_id, sep = "_")
  
  result <- predict_matchup(
    team_a_id = lower_id,
    team_b_id = higher_id,
    kenpom_data = womens_ranks_final, 
    model = womens_model,          
    sigma = womens_sigma            
  )
  
  # Store prediction
  pred_prob <- result$team_a_win_prob
  actual <- ifelse(game$WTeamID == lower_id, 1, 0)
  
  womens_predictions_list[[i]] <- data.frame(
    ID = game_id,
    Pred = pred_prob,
    Actual = actual,
    ActualWinner = game$WTeamID,
    ActualScore = paste(game$WScore, "-", game$LScore)
  )
  
  if(i %% 10 == 0) cat("Processed", i, "women's games...\n")
}

# Combine predictions
womens_tournament_predictions <- bind_rows(womens_predictions_list)

womens_log_loss <- -mean(womens_tournament_predictions$Actual * log(pmax(womens_tournament_predictions$Pred, 0.001)) + 
                           (1 - womens_tournament_predictions$Actual) * log(pmax(1 - womens_tournament_predictions$Pred, 0.001)))

womens_accuracy <- mean((womens_tournament_predictions$Pred > 0.5) == womens_tournament_predictions$Actual)  
cat("Log Loss:", round(womens_log_loss, 4), "\n")
cat("Bracket Accuracy:", round(womens_accuracy * 100, 1), "%\n\n")

# Calculate Brier Score
womens_tournament_predictions <- womens_tournament_predictions %>%
  mutate(Brier_Score = (Pred - Actual)^2)

womens_overall_brier <- mean(womens_tournament_predictions$Brier_Score)

cat("Overall Brier Score:", round(womens_overall_brier, 4), "\n")
cat("(0 = perfect, 0.25 = random guessing)\n\n")

# Show best and worst predictions
cat("Best predictions (lowest Brier):\n")
print(womens_tournament_predictions %>% 
        arrange(Brier_Score) %>% 
        select(ID, Pred, Actual, Brier_Score, ActualScore) %>% 
        head(10))

cat("\nWorst predictions (highest Brier):\n")
print(womens_tournament_predictions %>% 
        arrange(desc(Brier_Score)) %>% 
        select(ID, Pred, Actual, Brier_Score, ActualScore) %>% 
        head(10))

# Histogram
hist(womens_tournament_predictions$Brier_Score, 
     breaks = 20, 
     main = "Women's Tournament - Brier Score Distribution",
     xlab = "Brier Score", 
     col = "pink",
     border = "white")
abline(v = womens_overall_brier, col = "red", lwd = 2, lty = 2)
legend("topright", paste("Mean =", round(womens_overall_brier, 3)), 
       col = "red", lty = 2, lwd = 2)

# Save women's submission
write.csv(womens_tournament_predictions, "Excel_Files/KenPom_Batervirk_Model/womens_tournament_submission.csv", row.names = FALSE)


mens_predictions <- fread("Excel_Files/KenPom_Batervirk_Model/tournament_submission.csv")
womens_predictions <- fread("Excel_Files/KenPom_Batervirk_Model/womens_tournament_submission.csv")


combined_predictions <- bind_rows(mens_predictions, womens_predictions)

combined_brier <- mean(combined_predictions$Brier_Score)


combined_predictions$OverallBrier <- combined_brier

write.csv(combined_predictions, "Excel_Files/KenPom_Batervirk_Model/combined_men_women_2025.csv", row.names = FALSE)


