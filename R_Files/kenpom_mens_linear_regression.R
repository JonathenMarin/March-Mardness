library(tidyr)
library(dplyr)
library(data.table)
library(fuzzyjoin)
library(ggplot2)
library(performance)

# 1. DATA LOAD AND CLEANING -----------------------------------------------

Mteams <- fread("march-machine-learning-mania-2025/MTeams_2025.csv")
kenpom <- fread("Excel_Files/kenpom-ncaa-2025.csv")
spellings <- fread("march-machine-learning-mania-2025/MTeamSpellings.csv")

# Create normalized versions for matching
kenpom_norm <- kenpom %>%
  mutate(Team_normalized = tolower(Team))

spellings_norm <- spellings %>%
  mutate(TeamNameSpelling_normalized = tolower(TeamNameSpelling))

# First try exact match on normalized names
kenpom_matched <- kenpom_norm %>%
  left_join(
    spellings_norm %>% select(TeamNameSpelling_normalized, TeamID) %>% distinct(),
    by = c("Team_normalized" = "TeamNameSpelling_normalized")
  )

# For remaining unmatched, use fuzzy matching
unmatched <- kenpom_matched %>%
  filter(is.na(TeamID))

if(nrow(unmatched) > 0) {
  # Perform fuzzy matching
  fuzzy_matches <- stringdist_left_join(
    unmatched %>% select(-TeamID),
    spellings_norm,
    by = c("Team_normalized" = "TeamNameSpelling_normalized"),
    method = "jw",
    max_dist = 0.3,
    distance_col = "dist"
  ) %>%
    group_by(Team) %>%
    slice_min(dist, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # IMPORTANT: Ensure fuzzy_matches has the same columns as kenpom_matched
  # We select only the columns that exist in kenpom_matched to avoid errors
  cols_to_keep <- names(kenpom_matched)
  fuzzy_matches_clean <- fuzzy_matches %>% select(all_of(cols_to_keep))
  
  # Combine
  kenpom_final <- kenpom_matched %>%
    filter(!is.na(TeamID)) %>%
    bind_rows(fuzzy_matches_clean)
} else {
  kenpom_final <- kenpom_matched
}

# Clean up
kenpom_final <- kenpom_final %>% select(-Team_normalized)

cat("Matched teams:", sum(!is.na(kenpom_final$TeamID)), "out of", nrow(kenpom), "\n")

# 2. PREPARE TRAINING DATA ------------------------------------------------

# Read regular season results
results <- fread("march-machine-learning-mania-2025/MRegularSeasonDetailedResults.csv") 
results <- results %>% filter(results$Season == 2025)

# Join for WTeam - SELECTING NetRtg
results_with_wteam <- results %>%
  left_join(
    kenpom_final %>% select(TeamID, ORtg, DRtg, AdjT, NetRtg),
    by = c("WTeamID" = "TeamID")
  ) %>%
  dplyr::rename(
    WTeam_ORtg = ORtg,
    WTeam_DRtg = DRtg,
    WTeam_AdjT = AdjT,
    WTeam_NetRtg = NetRtg
  )

# Join for LTeam - SELECTING NetRtg
results_with_both <- results_with_wteam %>%
  left_join(
    kenpom_final %>% select(TeamID, ORtg, DRtg, AdjT, NetRtg),
    by = c("LTeamID" = "TeamID")
  ) %>%
  dplyr::rename(
    LTeam_ORtg = ORtg,
    LTeam_DRtg = DRtg,
    LTeam_AdjT = AdjT,
    LTeam_NetRtg = NetRtg
  )

# Drop games with missing KenPom data
results_clean <- results_with_both %>%
  filter(!is.na(WTeam_ORtg) & !is.na(LTeam_ORtg))

# Create training data - Row 1: WTeam scoring
train_wteam <- results_clean %>%
  transmute(
    Points = WScore,
    OffRating = WTeam_ORtg,
    DefRating = LTeam_DRtg,
    AdjT_team = WTeam_AdjT,
    AdjT_opp = LTeam_AdjT,
    Diff_NetRtg = WTeam_NetRtg - LTeam_NetRtg 
  )

# Create training data - Row 2: LTeam scoring
train_lteam <- results_clean %>%
  transmute(
    Points = LScore,
    OffRating = LTeam_ORtg,
    DefRating = WTeam_DRtg,
    AdjT_team = LTeam_AdjT,
    AdjT_opp = WTeam_AdjT,
    Diff_NetRtg = LTeam_NetRtg - WTeam_NetRtg
  )

# Combine into full training set
train_data <- bind_rows(train_wteam, train_lteam)

cat("Total training samples:", nrow(train_data), "\n")


# 3. MODEL CREATION -------------------------------------------------------


model <- lm(Points ~ OffRating + DefRating + AdjT_team + AdjT_opp + 
             Diff_NetRtg, 
            data = train_data)

summary(model)
sigma <- summary(model)$sigma
cat("Standard deviation (sigma):", sigma, "\n")

# Store predictions for diagnostics
train_data$Predicted <- predict(model, train_data)

#Visual Check of the Difference Feature 
ggplot(train_data, aes(x = Diff_NetRtg, y = Points)) +
  geom_point(alpha = 0.1, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Impact of NetRtg Difference on Points",
       x = "NetRtg Difference (My Net - Opp Net)",
       y = "Points Scored") +
  theme_minimal()


# 4. GAME SIMULATOR FUNCTIONS ---------------------------------------------

simulate_game <- function(team_a_mean, team_b_mean, sigma, n_sims = 500) {
  team_a_scores <- rnorm(n_sims, mean = team_a_mean, sd = sigma)
  team_b_scores <- rnorm(n_sims, mean = team_b_mean, sd = sigma)
  
  team_a_wins <- sum(team_a_scores > team_b_scores)
  
  return(list(
    team_a_win_prob = team_a_wins / n_sims,
    team_b_win_prob = 1 - (team_a_wins / n_sims),
    avg_margin = mean(team_a_scores - team_b_scores)
  ))
}

# Updated Predict Function to calculate the Difference
predict_matchup <- function(team_a_id, team_b_id, kenpom_data, model, sigma) {
  
  # Get team stats
  team_a <- kenpom_data %>% filter(TeamID == team_a_id)
  team_b <- kenpom_data %>% filter(TeamID == team_b_id)
  
  if(nrow(team_a) == 0 | nrow(team_b) == 0) {
    return(list(team_a_win_prob = 0.5))
  }
  
  # Calculate Difference for Prediction
  diff_a <- team_a$NetRtg - team_b$NetRtg
  diff_b <- team_b$NetRtg - team_a$NetRtg
  
  # Predict Team A
  team_a_pred <- predict(model, newdata = data.frame(
    OffRating = team_a$ORtg,
    DefRating = team_b$DRtg,
    AdjT_team = team_a$AdjT,
    AdjT_opp = team_b$AdjT,
    Diff_NetRtg = diff_a # Pass the difference
  ))
  
  # Predict Team B
  team_b_pred <- predict(model, newdata = data.frame(
    OffRating = team_b$ORtg,
    DefRating = team_a$DRtg,
    AdjT_team = team_b$AdjT,
    AdjT_opp = team_a$AdjT,
    Diff_NetRtg = diff_b # Pass the difference
  ))
  
  # Simulate
  result <- simulate_game(team_a_pred, team_b_pred, sigma, n_sims = 500)
  return(result)
}


# 5. PREDICT 2025 TOURNAMENT ----------------------------------------------

tournament_games <- fread("Excel_Files/2025_games_kaggle.csv")
tournament_games_subset <- tournament_games[1:67, ]

predictions_list <- list()

cat("Predicting tournament games...\n")
for(i in 1:nrow(tournament_games_subset)) {
  game <- tournament_games_subset[i, ]
  
  lower_id <- min(game$LTeamID, game$WTeamID)
  higher_id <- max(game$LTeamID, game$WTeamID)
  game_id <- paste(game$Season, lower_id, higher_id, sep = "_")
  
  result <- predict_matchup(
    team_a_id = lower_id,
    team_b_id = higher_id,
    kenpom_data = kenpom_final,
    model = model,
    sigma = sigma
  )
  
  pred_prob <- result$team_a_win_prob
  actual <- ifelse(game$WTeamID == lower_id, 1, 0)
  
  predictions_list[[i]] <- data.frame(
    ID = game_id,
    Pred = pred_prob,
    Actual = actual,
    Brier_Score = (pred_prob - actual)^2
  )
  
  if(i %% 10 == 0) cat(".")
}

 #RESULTS --------------------------------------------------------------

tournament_predictions <- bind_rows(predictions_list)
overall_brier <- mean(tournament_predictions$Brier_Score)
cat("Brier Score:  ", round(overall_brier, 5), "\n")

plot(model)
check_model(model, check="vif")



# tournament --------------------------------------------------------------

seeds <- fread("march-machine-learning-mania-2025/MNCAATourneySeeds.csv")

seeds_2025 <- seeds %>%
  filter(Season == 2025) %>%
  mutate(
    RegionCode = substr(Seed, 1, 1),                # "W","X","Y","Z"
    SeedNum    = as.integer(substr(Seed, 2, 3)),    # 1–16
    Region     = dplyr::recode(
      RegionCode,
      "W" = "East",
      "X" = "Midwest",
      "Y" = "South",
      "Z" = "West"
    )
  )


teams_2025 <- seeds_2025 %>%
  left_join(Mteams %>% select(TeamID, TeamName),
            by = "TeamID") %>%
  transmute(
    TeamID,
    TeamName,
    Region = Region,
    Seed   = SeedNum
  )

cat("Teams in 2025 bracket ", nrow(teams_2025), "\n")




# kenpom wrapper ----------------------------------------------------------




make_kenpom_prediction_model <- function(kenpom_data,
                                         model,
                                         sigma,
                                         mode = c("stochastic", "deterministic"),
                                         n_sims = 500) {
  mode <- match.arg(mode)
  
  function(team1_id, team2_id) {
    res <- predict_matchup(
      team_a_id   = team1_id,
      team_b_id   = team2_id,
      kenpom_data = kenpom_data,
      model       = model,
      sigma       = sigma
    )
    
    p1 <- res$team_a_win_prob
    p2 <- 1 - p1
    
    winner_id <- if (mode == "stochastic") {
      ifelse(runif(1) < p1, team1_id, team2_id)
    } else {
      ifelse(p1 >= 0.5, team1_id, team2_id)
    }
    
    list(
      winner_id = winner_id,
      p_team1   = p1,
      p_team2   = p2
    )
  }
}

prediction_model_kenpom <- make_kenpom_prediction_model(
  kenpom_data = kenpom_final, 
  model       = model,
  sigma       = sigma,
  mode        = "stochastic"   # or "deterministic"
)



source("R_Files/bracket_placement.R")

bracket_results_2025 <- run_full_tournament(
  teams_df         = teams_2025,
  prediction_model = prediction_model_kenpom
)

# Example: view full tournament nicely ordered
bracket_results_2025 %>%
  arrange(round, region, slot) %>%
  select(round, region, game_id,
         TeamName_High, seed_high,
         TeamName_Low,  seed_low,
         winner_id,
         p_team_high, p_team_low) %>%
  print(n = 100)

# Example: see just the championship game
champion_row <- bracket_results_2025 %>%
  filter(round == 6)

cat("\nPredicted championship matchup:\n")
print(champion_row)


View(bracket_results_2025)
