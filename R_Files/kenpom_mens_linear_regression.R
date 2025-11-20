library(tidyr)
library(dplyr)
library(data.table)
library(fuzzyjoin)
library(ggplot2)
# data load and cleaning --------------------------------------------------


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


# model creation and accuracy check ---------------------------------------

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
plot(model, sub.caption = "")
par(mfrow = c(1, 1))



sigma <- summary(model)$sigma
cat("Standard deviation (sigma):", sigma, "\n")

train_data <- train_data %>%
  mutate(
    Predicted_Mean = Predicted,
    Predicted_SD = sigma
  )


# game simulator function -------------------------------------------------


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
  
  if(nrow(team_a) == 0 | nrow(team_b) == 0) {
    warning(paste("Missing KenPom data for team", team_a_id, "or", team_b_id))
    return(list(
      team_a_win_prob = 0.5,
      team_b_win_prob = 0.5,
      avg_margin = 0
    ))
  }
  
  
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


# houston vs duke example -------------------------------------------------


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
# 1. Prepare Data (Duke Only)
df_duke <- data.frame(Points = duke_sims, Team = "Duke")

# Subset for the first 15 points
df_duke_subset <- data.frame(
  Points = duke_sims[1:15],
  Team = "Duke",
  Label = 1:15,
  Y_pos = 0.0025 # Height of the dots
)

# 2. Plot Duke
ggplot() +
  # Histogram
  geom_histogram(data = df_duke, 
                 aes(x = Points, y = after_stat(density)), 
                 fill = "blue", alpha = 0.5, bins = 50) +
  
  # Small Points
  geom_point(data = df_duke_subset, 
             aes(x = Points, y = Y_pos), 
             color = "lightblue", size = 3) + # Reduced size to 3
  
  # Numbers (Placed slightly above the point)
  geom_text(data = df_duke_subset, 
            aes(x = Points, y = Y_pos, label = Label), 
            color = "black", size = 3, vjust = -1, fontface = "bold") +
  
  # Formatting
  coord_cartesian(xlim = c(40, 110), ylim = c(0, 0.05)) +
  labs(title = "Duke Simulations (First 15)", x = "Points", y = "Density") +
  theme_minimal()

# 1. Prepare Combined Data
df_sims <- data.frame(
  Points = c(duke_sims, houston_sims),
  Team = c(rep("Duke", length(duke_sims)), 
           rep("Houston", length(houston_sims)))
)

# Subset for dots (keeping your specific Y-heights)
df_subset <- data.frame(
  Points = c(duke_sims[1:15], houston_sims[1:15]),
  Team = c(rep("Duke", 15), rep("Houston", 15)),
  Label = c(1:15, 1:15),
  Y_pos = c(rep(0.0025, 15), rep(0.0005, 15)) 
)

# 2. Plot Combined
ggplot() +
  # Histograms (Identity position allows overlap)
  geom_histogram(data = df_sims, 
                 aes(x = Points, fill = Team, y = after_stat(density)), 
                 alpha = 0.5, position = "identity", bins = 50) +
  
  # Small Points
  geom_point(data = df_subset, 
             aes(x = Points, y = Y_pos, color = Team), 
             size = 3, show.legend = FALSE) +
  
  # Numbers (Placed above points)
  geom_text(data = df_subset, 
            aes(x = Points, y = Y_pos, label = Label), 
            color = "black", size = 2, vjust = -1, fontface = "bold") +
  
  # Colors and Limits
  scale_fill_manual(values = c("Duke" = "blue", "Houston" = "red")) +
  scale_color_manual(values = c("Duke" = "blue", "Houston" = "red")) +
  coord_cartesian(xlim = c(40, 110), ylim = c(0, 0.05)) +
  
  # Formatting
  labs(title = "Duke vs Houston Simulations", x = "Points", y = "Density") +
  theme_minimal() +
  theme(legend.position = "top")

# Print the matchups
cat("\nFirst 15 simulated games:\n")
for(i in 1:15) {
  winner <- ifelse(duke_sims[i] > houston_sims[i], "Duke", "Houston")
  cat("Sim", i, ": Duke", round(duke_sims[i]), "- Houston", round(houston_sims[i]), "-->", winner, "\n")
}

# load 2025 tourney -------------------------------------------------------
#march madness bracket
tournament_games <- fread("Excel_Files/2025_games_kaggle.csv")

# Filter to rows 1-67
tournament_games_subset <- tournament_games[1:67, ]


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

cat("\nDone! Predicted", nrow(tournament_predictions), "games\n\n")

# Evaluate
log_loss <- -mean(tournament_predictions$Actual * log(pmax(tournament_predictions$Pred, 0.001)) + 
                    (1 - tournament_predictions$Actual) * log(pmax(1 - tournament_predictions$Pred, 0.001)))
cat("Log Loss:", round(log_loss, 4), "\n")

accuracy <- mean((tournament_predictions$Pred > 0.5) == tournament_predictions$Actual)
cat("Bracket Accuracy:", round(accuracy * 100, 1), "%\n")

write.csv(tournament_predictions, "Excel_Files/KenPom_Batervirk_Model/tournament_submission.csv", row.names = FALSE)
cat("\nSubmission saved!\n")


tournament_predictions <- tournament_predictions %>%
  mutate(
    Brier_Score = (Pred - Actual)^2
  )

# Overall Brier Score
overall_brier <- mean(tournament_predictions$Brier_Score)
cat("Overall Brier Score:", round(overall_brier, 4), "\n")




results_table <- tournament_predictions %>%
  arrange(Brier_Score) %>%
  mutate(
    Lower_Team = sapply(strsplit(ID, "_"), function(x) x[2]),
    Higher_Team = sapply(strsplit(ID, "_"), function(x) x[3]),
    Winner = ifelse(Actual == 1, "Lower", "Higher"),
    Confidence = round(ifelse(Pred > 0.5, Pred, 1 - Pred) * 100, 1)
  ) %>%
  select(ID, Pred, Actual, Winner, Confidence, Brier_Score, ActualScore)

print(head(results_table, 20))

# Summary statistics
cat("\n=== SUMMARY ===\n")
cat("Best prediction (lowest Brier):", round(min(tournament_predictions$Brier_Score), 4), "\n")
cat("Worst prediction (highest Brier):", round(max(tournament_predictions$Brier_Score), 4), "\n")
cat("Median Brier Score:", round(median(tournament_predictions$Brier_Score), 4), "\n")

# Histogram of Brier scores
hist(tournament_predictions$Brier_Score, 
     breaks = 20, 
     main = "Distribution of Brier Scores",
     xlab = "Brier Score", 
     col = "lightblue",
     border = "white")
abline(v = overall_brier, col = "red", lwd = 2, lty = 2)
legend("topright", paste("Mean =", round(overall_brier, 3)), 
       col = "red", lty = 2, lwd = 2)

# Compare to Log Loss
cat("\nLog Loss:", round(log_loss, 4), "\n")
cat("Brier Score:", round(overall_brier, 4), "\n")


