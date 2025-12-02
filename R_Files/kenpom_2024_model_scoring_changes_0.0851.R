library(tidyr)
library(dplyr)
library(data.table)
library(fuzzyjoin)
library(ggplot2)
library(stringdist)
# data load and cleaning --------------------------------------------------



SEASON_VAR <- 2024  # Run on 2024 Data
kenpom <- fread("Excel_Files/kenpom-ncaa-2024.csv") 
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
  select(-Team_normalized) %>%
  filter(!is.na(TeamID)) %>% 
  distinct(TeamID, .keep_all = TRUE)

cat("Matched teams:", nrow(kenpom_final), "out of", nrow(kenpom), "\n")




# Read regular season results
results <- fread("march-machine-learning-mania-2025/MRegularSeasonDetailedResults.csv") 
results <- results %>% filter(Season == SEASON_VAR)
cat("Regular Season Games found:", nrow(results), "\n")

# Join for WTeam (winning team) metrics
results_with_wteam <- results %>%
  left_join(
    kenpom_final %>% select(TeamID, ORtg, DRtg, AdjT),
    by = c("WTeamID" = "TeamID")
  ) %>%
  dplyr::rename(
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
  dplyr::rename(
    LTeam_ORtg = ORtg,
    LTeam_DRtg = DRtg,
    LTeam_AdjT = AdjT
  )

# Drop games with missing KenPom data
results_clean <- results_with_both %>%
  filter(!is.na(WTeam_ORtg) & !is.na(LTeam_ORtg))

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


# model creation and accuracy check

model <- lm(Points ~ OffRating + DefRating + AdjT_team + AdjT_opp, 
            data = train_data)

summary(model)
sigma <- summary(model)$sigma
cat("Standard deviation (sigma):", sigma, "\n")


# game simulator function -------------------------------------------------

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
    team_b_win_prob = 1 - win_prob_a
  ))
}

# Function to predict a matchup (ORIGINAL LOGIC)
predict_matchup <- function(team_a_id, team_b_id, kenpom_data, model, sigma) {
  # Get team stats
  team_a <- kenpom_data %>% filter(TeamID == team_a_id)
  team_b <- kenpom_data %>% filter(TeamID == team_b_id)
  
  if(nrow(team_a) == 0 | nrow(team_b) == 0) {
    return(list(team_a_win_prob = 0.5))
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


# --- 2024 PORTFOLIO EVALUATION (CORRECT SCORING) -------------------------

# 1. SETUP: Standard Bracket Order 
seed_order <- c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)

# Get 2024 Teams
seeds <- fread("march-machine-learning-mania-2025/MNCAATourneySeeds.csv") %>%
  filter(Season == 2024) %>%
  mutate(
    Region = substr(Seed, 1, 1),
    SeedNum = as.integer(substr(Seed, 2, 3))
  ) %>%
  filter(nchar(Seed) == 3 | substr(Seed, 4, 4) == "a") %>%
  arrange(Region, match(SeedNum, seed_order))

team_ids <- seeds$TeamID

# 2. MATRIX TO STORE PROBABILITIES
portfolio_probs <- matrix(0, nrow = 64, ncol = 6)
rownames(portfolio_probs) <- team_ids

# 3. PRE-CALCULATE ALL MATCHUPS (Optimization)
# To speed up the bracket simulation, we calculate the win prob matrix first
cat("Pre-calculating matchup probabilities...\n")
prob_matrix <- matrix(0, nrow=length(team_ids), ncol=length(team_ids))
rownames(prob_matrix) <- team_ids
colnames(prob_matrix) <- team_ids

for(i in 1:length(team_ids)) {
  for(j in 1:length(team_ids)) {
    if(i != j) {
      # Use your model logic
      p <- predict_matchup(team_ids[i], team_ids[j], kenpom_final, model, sigma)
      prob_matrix[i, j] <- p$team_a_win_prob
    }
  }
}

# 4. SIMULATION FUNCTION (Uses pre-calc probabilities)
sim_round <- function(teams) {
  n <- length(teams)
  winners <- c()
  
  for(i in seq(1, n, by=2)) {
    id_a <- teams[i]
    id_b <- teams[i+1]
    
    # Lookup prob in our matrix
    prob_a <- prob_matrix[as.character(id_a), as.character(id_b)]
    
    if(runif(1) < prob_a) {
      winners <- c(winners, id_a)
    } else {
      winners <- c(winners, id_b)
    }
  }
  return(winners)
}

# 5. RUN MONTE CARLO (1000 Sims)
n_sims <- 1000
cat("Simulating", n_sims, "brackets for Portfolio Score...\n")

for(s in 1:n_sims) {
  r1 <- sim_round(team_ids)
  portfolio_probs[as.character(r1), 1] <- portfolio_probs[as.character(r1), 1] + 1
  
  r2 <- sim_round(r1)
  portfolio_probs[as.character(r2), 2] <- portfolio_probs[as.character(r2), 2] + 1
  
  r3 <- sim_round(r2)
  portfolio_probs[as.character(r3), 3] <- portfolio_probs[as.character(r3), 3] + 1
  
  r4 <- sim_round(r3)
  portfolio_probs[as.character(r4), 4] <- portfolio_probs[as.character(r4), 4] + 1
  
  r5 <- sim_round(r4)
  portfolio_probs[as.character(r5), 5] <- portfolio_probs[as.character(r5), 5] + 1
  
  r6 <- sim_round(r5)
  portfolio_probs[as.character(r6), 6] <- portfolio_probs[as.character(r6), 6] + 1
  
  if(s %% 100 == 0) cat(".")
}

portfolio_probs <- portfolio_probs / n_sims

# 6. SCORING (CORRECT 2024 DATES & FILTERS)
tourney_results <- fread("march-machine-learning-mania-2025/MNCAATourneyCompactResults.csv")
results_2024 <- tourney_results %>% filter(Season == 2024)

truth_matrix <- matrix(0, nrow = 64, ncol = 6)
rownames(truth_matrix) <- team_ids

# Correct 2024 Calendar
round_days <- list(
  c(136, 137), # R1
  c(138, 139), # R2
  c(143, 144), # R3
  c(145, 146), # R4
  c(152),      # R5
  c(154)       # R6
)

for(r in 1:6) {
  winners <- results_2024 %>% 
    filter(DayNum %in% round_days[[r]]) %>% 
    pull(WTeamID)
  
  valid_winners <- as.character(winners)
  valid_winners <- valid_winners[valid_winners %in% rownames(truth_matrix)]
  
  if(length(valid_winners) > 0) {
    truth_matrix[valid_winners, r] <- 1
  }
}

# 7. CALCULATE BRIER SCORE
brier_matrix <- (portfolio_probs - truth_matrix)^2
round_scores <- colMeans(brier_matrix)

cat("\n\n=== FINAL PORTFOLIO RESULTS (2024 Backtest) ===\n")
round_names <- c("R1 (64)", "R2 (32)", "R3 (16)", "R4 (8)", "R5 (4)", "R6 (2)")
print(setNames(round(round_scores, 5), round_names))

cat("\nFinal Portfolio Score:", mean(round_scores), "\n")