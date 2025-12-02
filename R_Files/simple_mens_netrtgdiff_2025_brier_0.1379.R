library(tidyr)
library(dplyr)
library(data.table)
library(fuzzyjoin)
library(ggplot2)

Mteams   <- fread("march-machine-learning-mania-2025/MTeams_2025.csv")
kenpom   <- fread("Excel_Files/kenpom-ncaa-2025.csv")
spellings <- fread("march-machine-learning-mania-2025/MTeamSpellings.csv")

# Create normalized versions for matching
kenpom_norm <- kenpom %>%
  mutate(Team_normalized = tolower(Team))

spellings_norm <- spellings %>%
  mutate(TeamNameSpelling_normalized = tolower(TeamNameSpelling))

# First: exact matches on normalized names
kenpom_matched <- kenpom_norm %>%
  left_join(
    spellings_norm %>%
      select(TeamNameSpelling_normalized, TeamID) %>%
      distinct(),
    by = c("Team_normalized" = "TeamNameSpelling_normalized")
  )

# Remaining unmatched for fuzzy matching
unmatched <- kenpom_matched %>%
  filter(is.na(TeamID))

if (nrow(unmatched) > 0) {
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
  
  # Combine exact + fuzzy
  kenpom_final <- kenpom_matched %>%
    filter(!is.na(TeamID)) %>%
    bind_rows(fuzzy_matches %>% select(names(kenpom_matched)))
} else {
  kenpom_final <- kenpom_matched
}

# Clean up
kenpom_final <- kenpom_final %>%
  select(-Team_normalized)

cat("Matched teams:", sum(!is.na(kenpom_final$TeamID)), "out of", nrow(kenpom), "\n")

still_unmatched <- kenpom_final %>%
  filter(is.na(TeamID))

if (nrow(still_unmatched) > 0) {
  cat("\nStill unmatched teams in KenPom:\n")
  print(still_unmatched$Team)
}

# Save mapping (optional)
write.csv(kenpom_final, "Excel_Files/kenpom_kaggle_combined_team.csv", row.names = FALSE)



# Simple NetRtg-only probability model

# We will use:
#   z = 0.7 * (NetRtg_A - NetRtg_B) / sigma_m
#   p(A wins) = pnorm(z)
#
# If your net rating column is named differently (e.g. "AdjEM" or "SS_NetRtg"),
# change "NetRtg" in the code below to the correct column name.


k_factor <- 0.7   # scaling from rating diff to spread
sigma_m  <- 9     # given matchup SD

predict_matchup_netrtg <- function(team_a_id, team_b_id,
                                   kenpom_data,
                                   k = 0.7,
                                   sigma_m = 9) {
  team_a <- kenpom_data %>% filter(TeamID == team_a_id)
  team_b <- kenpom_data %>% filter(TeamID == team_b_id)
  
  if (nrow(team_a) == 0 | nrow(team_b) == 0) {
    warning(paste(
      "Missing NetRtg data for team",
      team_a_id, "or", team_b_id, "- returning 0.5."
    ))
    return(list(
      team_a_win_prob = 0.5,
      team_b_win_prob = 0.5,
      spread = NA_real_,
      z = 0
    ))
  }
  
  # Net rating difference
  diff_netrtg <- team_a$NetRtg - team_b$NetRtg
  
  # Convert to an implied point spread
  spread <- k * diff_netrtg
  
  # Standardized difference
  z <- spread / sigma_m
  
  # Win probability via Normal CDF
  p_a <- pnorm(z)
  
  list(
    team_a_win_prob = p_a,
    team_b_win_prob = 1 - p_a,
    spread = spread,
    z = z
  )
}

# Load 2025 tourney results (Kaggle-style file)

tournament_games <- fread("Excel_Files/2025_games_kaggle.csv")


tournament_games_subset <- tournament_games[1:67, ]

cat("Tournament games loaded:", nrow(tournament_games_subset), "\n")


predictions_list <- vector("list", nrow(tournament_games_subset))

for (i in seq_len(nrow(tournament_games_subset))) {
  game <- tournament_games_subset[i, ]
  
  # Enforce lower/higher ID ordering to match Kaggle ID format
  lower_id  <- min(game$LTeamID, game$WTeamID)
  higher_id <- max(game$LTeamID, game$WTeamID)
  
  game_id <- paste(game$Season, lower_id, higher_id, sep = "_")
  
  res <- predict_matchup_netrtg(
    team_a_id   = lower_id,
    team_b_id   = higher_id,
    kenpom_data = kenpom_final,
    k           = k_factor,
    sigma_m     = sigma_m
  )
  
  pred_prob <- res$team_a_win_prob
  actual    <- ifelse(game$WTeamID == lower_id, 1, 0)
  
  predictions_list[[i]] <- data.frame(
    ID           = game_id,
    Pred         = pred_prob,
    Actual       = actual,
    ActualWinner = game$WTeamID,
    ActualScore  = paste(game$WScore, "-", game$LScore),
    Spread       = res$spread,
    Z_value      = res$z
  )
  
  if (i %% 10 == 0) cat("Processed", i, "games...\n")
}

tournament_predictions <- bind_rows(predictions_list)

cat("\nDone! Predicted", nrow(tournament_predictions), "games\n\n")






# 0.5 cutoff accuracy (bracket-style)
accuracy <- mean((tournament_predictions$Pred > 0.5) == tournament_predictions$Actual)
cat("Bracket Accuracy:", round(accuracy * 100, 1), "%\n")


# Brier Score
tournament_predictions <- tournament_predictions %>%
  mutate(Brier_Score = (Pred - Actual)^2)

overall_brier <- mean(tournament_predictions$Brier_Score)
cat("Overall Brier Score:", round(overall_brier, 4), "\n")



results_table <- tournament_predictions %>%
  arrange(Brier_Score) %>%
  mutate(
    Lower_Team  = sapply(strsplit(ID, "_"), function(x) x[2]),
    Higher_Team = sapply(strsplit(ID, "_"), function(x) x[3]),
    Winner      = ifelse(Actual == 1, "Lower", "Higher"),
    Confidence  = round(ifelse(Pred > 0.5, Pred, 1 - Pred) * 100, 1)
  ) %>%
  select(ID, Pred, Actual, Winner, Confidence, Brier_Score, ActualScore, Spread, Z_value)

cat("\nTop 20 (lowest Brier Scores):\n")
print(head(results_table, 20))

cat("\n=== SUMMARY ===\n")
cat("Best prediction (lowest Brier):", round(min(tournament_predictions$Brier_Score), 4), "\n")
cat("Worst prediction (highest Brier):", round(max(tournament_predictions$Brier_Score), 4), "\n")
cat("Median Brier Score:", round(median(tournament_predictions$Brier_Score), 4), "\n")

# Histogram of Brier scores
hist(
  tournament_predictions$Brier_Score,
  breaks = seq(0, 1, length.out = 51),
  xlim   = c(0, 1),
  ylim   = c(0, 25),
  yaxt   = "n",
  main   = "Distribution of Men's Brier Scores (NetRtg model)",
  xlab   = "Brier Score",
  col    = "lightblue",
  border = "white"
)
axis(2, at = seq(0, 25, by = 5), las = 1)
abline(v = overall_brier, col = "red", lwd = 2, lty = 2)
legend(
  "topright",
  paste("Mean =", round(overall_brier, 3)),
  col = "red", lty = 2, lwd = 2
)

cat("\nLog Loss:", round(log_loss, 4), "\n")
cat("Overall Brier Score:", round(overall_brier, 4), "\n")

