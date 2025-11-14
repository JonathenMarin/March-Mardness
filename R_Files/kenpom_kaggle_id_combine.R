library(tidyr)
library(dplyr)
library(data.table)
library(fuzzyjoin)

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
  
  # Combine exact and fuzzy matches
  kenpom_final <- kenpom_matched %>%
    filter(!is.na(TeamID)) %>%
    bind_rows(fuzzy_matches %>% select(names(kenpom_matched)))
} else {
  kenpom_final <- kenpom_matched
}

# Clean up and check
kenpom_final <- kenpom_final %>%
  select(-Team_normalized)

cat("Matched teams:", sum(!is.na(kenpom_final$TeamID)), "out of", nrow(kenpom), "\n")

still_unmatched <- kenpom_final %>%
  filter(is.na(TeamID))

if(nrow(still_unmatched) > 0) {
  cat("\nStill unmatched:\n")
  print(still_unmatched$Team)
}


write.csv(kenpom_final, "Excel_Files/kenpom_kaggle_combined_team.csv")

#part 2

# Read regular season results
results <- fread("march-machine-learning-mania-2025/MRegularSeasonDetailedResults.csv") 
results <- results %>% filter(results$Season == 2025)
cat(nrow(results))

# Join for WTeam (winning team) metrics
results_with_wteam <- results %>%
  left_join(
    kenpom_final %>% select(TeamID, ORtg, DRtg, AdjT),
    by = c("WTeamID" = "TeamID")
  ) %>%
  rename(
    WTeam_ORtg = ORtg,
    WTeam_DRtg = DRtg,
    WTeam_AdjT = AdjT
  )

# Join for LTeam (losing team) metrics
results_with_both <- results_with_wteam %>%
  left_join(
    kenpom_final %>% select(TeamID, ORtg, DRtg, AdjT),
    by = c("LTeamID" = "TeamID")
  ) %>%
  rename(
    LTeam_ORtg = ORtg,
    LTeam_DRtg = DRtg,
    LTeam_AdjT = AdjT
  )

# Drop games with missing KenPom data
results_clean <- results_with_both %>%
  filter(!is.na(WTeam_ORtg) & !is.na(LTeam_ORtg))

cat("Games remaining:", nrow(results_clean), "out of", nrow(results), "\n")

# Calculate league average rating
league_avg_rating <- mean(c(kenpom_final$ORtg, kenpom_final$DRtg), na.rm = TRUE)

# Create training data - Row 1: WTeam scoring
train_wteam <- results_clean %>%
  transmute(
    Points = WScore,
    OffRating = WTeam_ORtg,
    DefRating = LTeam_DRtg,
    AdjT_team = WTeam_AdjT,
    AdjT_opp = LTeam_AdjT,
    League_Avg = league_avg_rating
  )

# Create training data - Row 2: LTeam scoring
train_lteam <- results_clean %>%
  transmute(
    Points = LScore,
    OffRating = LTeam_ORtg,
    DefRating = WTeam_DRtg,
    AdjT_team = LTeam_AdjT,
    AdjT_opp = WTeam_AdjT,
    League_Avg = league_avg_rating
  )

# Combine into full training set
train_data <- bind_rows(train_wteam, train_lteam)

cat("Total training samples:", nrow(train_data), "\n")
cat("Summary:\n")
summary(train_data)

#model

model <- lm(Points ~ OffRating + DefRating + AdjT_team + AdjT_opp, 
            data = train_data)

summary(model)
cat("\nModel Coefficients:\n")
print(coef(model))

cat("\nR-squared:", summary(model)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model)$adj.r.squared, "\n")
cat("RMSE:", sqrt(mean(model$residuals^2)), "\n")

# CREATE PREDICTIONS (this was missing!)
train_data$Predicted <- predict(model, train_data)

# Mean Absolute Error
cat("\nMean Absolute Error:", mean(abs(train_data$Points - train_data$Predicted)), "\n")

# Visual check - actual vs predicted
plot(train_data$Points, train_data$Predicted,
     xlab = "Actual Points", ylab = "Predicted Points",
     main = "Actual vs Predicted Points",
     pch = 16, col = rgb(0, 0, 0, 0.1))
abline(0, 1, col = "red", lwd = 2)

# Check if model is reasonable
cat("\nPredicted points range:", 
    round(min(train_data$Predicted), 1), "to", 
    round(max(train_data$Predicted), 1), "\n")

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))



sigma <- summary(model)$sigma
cat("Standard deviation (sigma):", sigma, "\n")

train_data <- train_data %>%
  mutate(
    Predicted_Mean = Predicted,
    Predicted_SD = sigma
  )

# Function to simulate game outcome
simulate_game <- function(team_a_mean, team_b_mean, sigma, n_sims = 10000) {
  # Simulate scores for both teams
  team_a_scores <- rnorm(n_sims, mean = team_a_mean, sd = sigma)
  team_b_scores <- rnorm(n_sims, mean = team_b_mean, sd = sigma)
  
  # Count how many times Team A wins
  team_a_wins <- sum(team_a_scores > team_b_scores)
  
  # Calculate win probability
  win_prob_a <- team_a_wins / n_sims
  
  return(list(
    team_a_win_prob = win_prob_a,
    team_b_win_prob = 1 - win_prob_a,
    avg_margin = mean(team_a_scores - team_b_scores)
  ))
}

#ex
#result <- simulate_game(team_a_mean = 75, team_b_mean = 70, sigma = sigma, n_sims = 10000)

#cat("Team A win probability:", round(result$team_a_win_prob * 100, 1), "%\n")
#at("Team B win probability:", round(result$team_b_win_prob * 100, 1), "%\n")
#cat("Average margin (Team A):", round(result$avg_margin, 1), "points\n")

# Visualize the distributions
#set.seed(123)
#team_a_sims <- rnorm(10000, mean = 75, sd = sigma)
#team_b_sims <- rnorm(10000, mean = 70, sd = sigma)

#hist(team_a_sims, breaks = 50, col = rgb(1, 0, 0, 0.5), 
     #xlim = c(40, 110), main = "Score Distributions",
     #xlab = "Points", freq = FALSE)
#hist(team_b_sims, breaks = 50, col = rgb(0, 0, 1, 0.5), add = TRUE, freq = FALSE)
#legend("topright", c("Team A", "Team B"), fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

# Function to predict a matchup
predict_matchup <- function(team_a_id, team_b_id, kenpom_data, model, sigma) {
  # Get team stats
  team_a <- kenpom_data %>% filter(TeamID == team_a_id)
  team_b <- kenpom_data %>% filter(TeamID == team_b_id)
  
  # Predict Team A's score
  team_a_pred <- predict(model, newdata = data.frame(
    OffRating = team_a$ORtg,
    DefRating = team_b$DRtg,
    AdjT_team = team_a$AdjT,
    AdjT_opp = team_b$AdjT
  ))
  
  # Predict Team B's score
  team_b_pred <- predict(model, newdata = data.frame(
    OffRating = team_b$ORtg,
    DefRating = team_a$DRtg,
    AdjT_team = team_b$AdjT,
    AdjT_opp = team_a$AdjT
  ))
  
  # Simulate game
  result <- simulate_game(team_a_pred, team_b_pred, sigma, n_sims = 10000)
  
  return(result)
}

result <- predict_matchup(team_a_id = 1181, team_b_id = 1222, 
                          kenpom_data = kenpom_final, 
                          model = model, 
                          sigma = sigma)

duke_stats <- kenpom_final %>% filter(TeamID == 1181)
houston_stats <- kenpom_final %>% filter(TeamID == 1277)

duke_pred <- predict(model, newdata = data.frame(
  OffRating = duke_stats$ORtg,
  DefRating = houston_stats$DRtg,
  AdjT_team = duke_stats$AdjT,
  AdjT_opp = houston_stats$AdjT
))

houston_pred <- predict(model, newdata = data.frame(
  OffRating = houston_stats$ORtg,
  DefRating = duke_stats$DRtg,
  AdjT_team = houston_stats$AdjT,
  AdjT_opp = duke_stats$AdjT
))

cat("Duke predicted:", round(duke_pred, 1), "points\n")
cat("Houston predicted:", round(houston_pred, 1), "points\n\n")

set.seed(42)
duke_sims <- rnorm(10000, mean = duke_pred, sd = sigma)
houston_sims <- rnorm(10000, mean = houston_pred, sd = sigma)

# Plot overlapping histograms
hist(duke_sims, breaks = 50, col = rgb(0, 0, 1, 0.5),
     xlim = c(40, 110), main = "Duke vs Houston - First 15 Simulations Connected",
     xlab = "Points", freq = FALSE, ylim = c(0, 0.05))
hist(houston_sims, breaks = 50, col = rgb(1, 0, 0, 0.5), add = TRUE, freq = FALSE)


y_duke <- rep(0.002, 150)
y_houston <- rep(0.0005, 150)

points(duke_sims[1:150], y_duke, col = "lightblue", pch = 19, cex = 1.2)
points(houston_sims[1:150], y_houston, col = "pink", pch = 19, cex = 1.2)


legend("topright", 
       c("Duke distribution", "Houston distribution"), 
       fill = c(rgb(0,0,1,0.5), rgb(1,0,0,0.5), NA, NA),
       lty = c(NA, NA, 2, 2),
       lwd = c(NA, NA, 1.5, 1.5),
       col = c(NA, NA, "blue", "red"))


#march madness bracket
tournament_games <- fread("Excel_Files/2025_games_kaggle.csv")

# Filter to rows 2-68
tournament_games_subset <- tournament_games[2:68, ]

predictions_list <- list()

for(i in 1:nrow(tournament_games_subset)) {
  game <- tournament_games_subset[i, ]
  
  # Determine lower and higher team IDs
  lower_id <- min(game$LTeamID, game$WTeamID)
  higher_id <- max(game$LTeamID, game$WTeamID)
  
  # Create ID in Kaggle format
  game_id <- paste(game$Season, lower_id, higher_id, sep = "_")
  
  # Simulate the matchup
  result <- predict_matchup(
    team_a_id = lower_id,
    team_b_id = higher_id,
    kenpom_data = kenpom_final,
    model = model,
    sigma = sigma
  )
  
  # Store prediction
  pred_prob <- result$team_a_win_prob
  actual <- ifelse(game$WTeamID == lower_id, 1, 0)
  
  predictions_list[[i]] <- data.frame(
    ID = game_id,
    Pred = pred_prob,
    Actual = actual,
    ActualWinner = game$WTeamID,
    ActualScore = paste(game$WScore, "-", game$LScore)
  )
  
  if(i %% 10 == 0) cat("Processed", i, "games...\n")
}

# Combine all predictions
tournament_predictions <- bind_rows(predictions_list)



