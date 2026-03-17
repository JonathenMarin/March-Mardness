library(tidyr)
library(dplyr)
library(data.table)
library(fuzzyjoin)
library(ggplot2)
library(performance)

# 1. DATA LOAD AND CLEANING -----------------------------------------------

Mteams    <- fread("march-machine-learning-mania-2026/MTeams.csv")
kenpom <- fread("Excel_Files/kenpom-ncaa-2026.csv", encoding = "Latin-1") %>%
  mutate(Team = trimws(gsub("[^a-zA-Z0-9 &'.-]", "", Team)))
spellings <- fread("march-machine-learning-mania-2026/MTeamSpellings.csv")
results   <- fread("march-machine-learning-mania-2026/MRegularSeasonDetailedResults.csv")

kenpom_norm <- kenpom %>%
  mutate(Team_normalized = tolower(Team))

spellings_norm <- spellings %>%
  mutate(TeamNameSpelling_normalized = tolower(TeamNameSpelling))

kenpom_matched <- kenpom_norm %>%
  left_join(
    spellings_norm %>% select(TeamNameSpelling_normalized, TeamID) %>% distinct(),
    by = c("Team_normalized" = "TeamNameSpelling_normalized")
  )

unmatched <- kenpom_matched %>% filter(is.na(TeamID))

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
  
  cols_to_keep        <- names(kenpom_matched)
  fuzzy_matches_clean <- fuzzy_matches %>% select(all_of(cols_to_keep))
  
  kenpom_final <- kenpom_matched %>%
    filter(!is.na(TeamID)) %>%
    bind_rows(fuzzy_matches_clean)
} else {
  kenpom_final <- kenpom_matched
}

kenpom_final <- kenpom_final %>% select(-Team_normalized)

cat("Matched teams:", sum(!is.na(kenpom_final$TeamID)), "out of", nrow(kenpom), "\n")


# 2. ELO RATINGS ----------------------------------------------------------

calculate_elo <- function(regular_results, initial_rating = 1500, k = 64,
                          width = 400, hca = 0, carry_over = 0.5) {
  games        <- regular_results[order(Season, DayNum)]
  seasons      <- sort(unique(games$Season))
  all_ratings  <- list()
  team_ratings <- c()
  
  for (s in seasons) {
    season_games <- games[Season == s]
    teams <- unique(c(season_games$WTeamID, season_games$LTeamID))
    
    if (length(team_ratings) > 0) {
      for (tid in as.character(teams)) {
        if (tid %in% names(team_ratings)) {
          team_ratings[tid] <- carry_over * team_ratings[tid] + (1 - carry_over) * initial_rating
        } else {
          team_ratings[tid] <- initial_rating
        }
      }
    } else {
      team_ratings <- rep(initial_rating, length(teams))
      names(team_ratings) <- as.character(teams)
    }
    
    for (i in seq_len(nrow(season_games))) {
      wteam <- as.character(season_games$WTeamID[i])
      lteam <- as.character(season_games$LTeamID[i])
      wloc  <- season_games$WLoc[i]
      
      rW <- team_ratings[wteam]
      rL <- team_ratings[lteam]
      
      if (wloc == "H") {
        rW_adj <- rW + hca
      } else if (wloc == "A") {
        rW_adj <- rW - hca
      } else {
        rW_adj <- rW
      }
      
      eW <- 1 / (1 + 10^((rL - rW_adj) / width))
      eL <- 1 - eW
      
      team_ratings[wteam] <- rW + k * (1 - eW)
      team_ratings[lteam] <- rL + k * (0 - eL)
    }
    
    all_ratings[[as.character(s)]] <- data.table(
      Season    = s,
      TeamID    = as.integer(names(team_ratings)),
      Elo_final = as.numeric(team_ratings)
    )
  }
  
  return(rbindlist(all_ratings))
}

elo_ratings <- calculate_elo(results)

# Pull 2025 end-of-season Elo for each team
elo_2025 <- elo_ratings[Season == 2025, .(TeamID, Elo_final)]

cat("Teams with 2025 Elo ratings:", nrow(elo_2025), "\n")


# 3. PREPARE TRAINING DATA ------------------------------------------------

results_2025 <- results %>% filter(Season == 2025)

results_with_wteam <- results_2025 %>%
  left_join(kenpom_final %>% select(TeamID, ORtg, DRtg, AdjT, NetRtg),
            by = c("WTeamID" = "TeamID")) %>%
  dplyr::rename(WTeam_ORtg = ORtg, WTeam_DRtg = DRtg,
                WTeam_AdjT = AdjT, WTeam_NetRtg = NetRtg) %>%
  left_join(elo_2025, by = c("WTeamID" = "TeamID")) %>%
  dplyr::rename(WTeam_Elo = Elo_final)

results_with_both <- results_with_wteam %>%
  left_join(kenpom_final %>% select(TeamID, ORtg, DRtg, AdjT, NetRtg),
            by = c("LTeamID" = "TeamID")) %>%
  dplyr::rename(LTeam_ORtg = ORtg, LTeam_DRtg = DRtg,
                LTeam_AdjT = AdjT, LTeam_NetRtg = NetRtg) %>%
  left_join(elo_2025, by = c("LTeamID" = "TeamID")) %>%
  dplyr::rename(LTeam_Elo = Elo_final)

results_clean <- results_with_both %>%
  filter(!is.na(WTeam_ORtg) & !is.na(LTeam_ORtg) &
           !is.na(WTeam_Elo)  & !is.na(LTeam_Elo))

train_wteam <- results_clean %>%
  transmute(
    Points      = WScore,
    OffRating   = WTeam_ORtg,
    DefRating   = LTeam_DRtg,
    AdjT_team   = WTeam_AdjT,
    AdjT_opp    = LTeam_AdjT,
    Diff_NetRtg = WTeam_NetRtg - LTeam_NetRtg,
    Diff_Elo    = WTeam_Elo    - LTeam_Elo
  )

train_lteam <- results_clean %>%
  transmute(
    Points      = LScore,
    OffRating   = LTeam_ORtg,
    DefRating   = WTeam_DRtg,
    AdjT_team   = LTeam_AdjT,
    AdjT_opp    = WTeam_AdjT,
    Diff_NetRtg = LTeam_NetRtg - WTeam_NetRtg,
    Diff_Elo    = LTeam_Elo    - WTeam_Elo
  )

train_data <- bind_rows(train_wteam, train_lteam)

cat("Total training samples:", nrow(train_data), "\n")


# 4. MODEL CREATION -------------------------------------------------------

model <- lm(Points ~ OffRating + DefRating + AdjT_team + AdjT_opp +
              Diff_NetRtg + Diff_Elo,
            data = train_data)

summary(model)
sigma <- summary(model)$sigma
cat("Standard deviation (sigma):", sigma, "\n")

train_data$Predicted <- predict(model, train_data)

ggplot(train_data, aes(x = Diff_NetRtg, y = Points)) +
  geom_point(alpha = 0.1, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Impact of NetRtg Difference on Points",
       x = "NetRtg Difference (My Net - Opp Net)",
       y = "Points Scored") +
  theme_minimal()

plot(model)
check_model(model, check = "vif")


# 5. GAME SIMULATOR FUNCTIONS ---------------------------------------------

simulate_game <- function(team_a_mean, team_b_mean, sigma, n_sims = 500) {
  team_a_scores <- rnorm(n_sims, mean = team_a_mean, sd = sigma)
  team_b_scores <- rnorm(n_sims, mean = team_b_mean, sd = sigma)
  team_a_wins   <- sum(team_a_scores > team_b_scores)
  
  return(list(
    team_a_win_prob = team_a_wins / n_sims,
    team_b_win_prob = 1 - (team_a_wins / n_sims),
    avg_margin      = mean(team_a_scores - team_b_scores)
  ))
}

predict_matchup <- function(team_a_id, team_b_id, kenpom_data, elo_data, model, sigma) {
  
  team_a   <- kenpom_data %>% filter(TeamID == team_a_id)
  team_b   <- kenpom_data %>% filter(TeamID == team_b_id)
  elo_a    <- elo_data    %>% filter(TeamID == team_a_id) %>% pull(Elo_final)
  elo_b    <- elo_data    %>% filter(TeamID == team_b_id) %>% pull(Elo_final)
  
  if (nrow(team_a) == 0 | nrow(team_b) == 0 |
      length(elo_a) == 0 | length(elo_b) == 0) {
    return(list(team_a_win_prob = 0.5))
  }
  
  team_a_pred <- predict(model, newdata = data.frame(
    OffRating   = team_a$ORtg,
    DefRating   = team_b$DRtg,
    AdjT_team   = team_a$AdjT,
    AdjT_opp    = team_b$AdjT,
    Diff_NetRtg = team_a$NetRtg - team_b$NetRtg,
    Diff_Elo    = elo_a - elo_b
  ))
  
  team_b_pred <- predict(model, newdata = data.frame(
    OffRating   = team_b$ORtg,
    DefRating   = team_a$DRtg,
    AdjT_team   = team_b$AdjT,
    AdjT_opp    = team_a$AdjT,
    Diff_NetRtg = team_b$NetRtg - team_a$NetRtg,
    Diff_Elo    = elo_b - elo_a
  ))
  
  result <- simulate_game(team_a_pred, team_b_pred, sigma, n_sims = 500)
  return(result)
}


# 6. GENERATE ALL-PAIRS 2026 PREDICTIONS ----------------------------------

seeds      <- fread("march-machine-learning-mania-2026/MNCAATourneySeeds.csv")
seeds_2026 <- seeds %>% filter(Season == 2026)

mens_team_ids_2026 <- seeds_2026$TeamID

matchups_2026 <- expand.grid(Team1 = mens_team_ids_2026, Team2 = mens_team_ids_2026) %>%
  filter(Team1 < Team2)

cat("Total matchups to predict:", nrow(matchups_2026), "\n")

predictions_list <- list()

cat("Predicting all matchups...\n")
for (i in 1:nrow(matchups_2026)) {
  team_a_id <- matchups_2026$Team1[i]
  team_b_id <- matchups_2026$Team2[i]
  
  result <- predict_matchup(
    team_a_id   = team_a_id,
    team_b_id   = team_b_id,
    kenpom_data = kenpom_final,
    elo_data    = elo_2025,
    model       = model,
    sigma       = sigma
  )
  
  predictions_list[[i]] <- data.frame(
    ID   = paste(2026, team_a_id, team_b_id, sep = "_"),
    Pred = result$team_a_win_prob
  )
  
  if (i %% 100 == 0) cat("Completed", i, "of", nrow(matchups_2026), "\n")
}

submission_2026_mens <- bind_rows(predictions_list)
write.csv(submission_2026_mens, "Excel_Files/submission_2026_model3_mens.csv", row.names = FALSE)

cat("Submission written:", nrow(submission_2026_mens), "rows\n")
cat("Pred range: [", round(min(submission_2026_mens$Pred), 4), ",",
    round(max(submission_2026_mens$Pred), 4), "]\n\n")


# 7. BRACKET SIMULATION (with live Elo updates) ---------------------------

seeds_2026_bracket <- seeds_2026 %>%
  mutate(
    RegionCode = substr(Seed, 1, 1),
    SeedNum    = as.integer(substr(Seed, 2, 3)),
    Region     = dplyr::recode(
      RegionCode,
      "W" = "East",
      "X" = "Midwest",
      "Y" = "South",
      "Z" = "West"
    )
  )

teams_2026 <- seeds_2026_bracket %>%
  left_join(Mteams %>% select(TeamID, TeamName), by = "TeamID") %>%
  transmute(TeamID, TeamName, Region, Seed = SeedNum)

cat("Teams in 2026 bracket:", nrow(teams_2026), "\n")

make_kenpom_prediction_model <- function(kenpom_data, elo_data, model, sigma,
                                         mode = c("stochastic", "deterministic"),
                                         k = 64, width = 400) {
  mode <- match.arg(mode)
  
  # Copy Elo into a mutable environment so updates persist across game calls
  # This does NOT affect elo_2025 used in the Kaggle submission above
  elo_env <- new.env(parent = emptyenv())
  elo_env$ratings <- setNames(elo_data$Elo_final, as.character(elo_data$TeamID))
  
  function(team1_id, team2_id) {
    
    # Get current live Elo from environment
    elo_a <- elo_env$ratings[as.character(team1_id)]
    elo_b <- elo_env$ratings[as.character(team2_id)]
    
    # Fall back to 1500 if a team isn't found
    if (is.na(elo_a)) elo_a <- 1500
    if (is.na(elo_b)) elo_b <- 1500
    
    team_a <- kenpom_data %>% filter(TeamID == team1_id)
    team_b <- kenpom_data %>% filter(TeamID == team2_id)
    
    if (nrow(team_a) == 0 | nrow(team_b) == 0) {
      return(list(winner_id = team1_id, p_team1 = 0.5, p_team2 = 0.5))
    }
    
    team_a_pred <- predict(model, newdata = data.frame(
      OffRating   = team_a$ORtg,
      DefRating   = team_b$DRtg,
      AdjT_team   = team_a$AdjT,
      AdjT_opp    = team_b$AdjT,
      Diff_NetRtg = team_a$NetRtg - team_b$NetRtg,
      Diff_Elo    = elo_a - elo_b
    ))
    
    team_b_pred <- predict(model, newdata = data.frame(
      OffRating   = team_b$ORtg,
      DefRating   = team_a$DRtg,
      AdjT_team   = team_b$AdjT,
      AdjT_opp    = team_a$AdjT,
      Diff_NetRtg = team_b$NetRtg - team_a$NetRtg,
      Diff_Elo    = elo_b - elo_a
    ))
    
    result    <- simulate_game(team_a_pred, team_b_pred, sigma, n_sims = 500)
    p1        <- result$team_a_win_prob
    p2        <- 1 - p1
    
    winner_id <- if (mode == "stochastic") {
      ifelse(runif(1) < p1, team1_id, team2_id)
    } else {
      ifelse(p1 >= 0.5, team1_id, team2_id)
    }
    
    # ── Update Elo in environment after game result ───────────────────────
    loser_id  <- ifelse(winner_id == team1_id, team2_id, team1_id)
    winner_elo <- elo_env$ratings[as.character(winner_id)]
    loser_elo  <- elo_env$ratings[as.character(loser_id)]
    
    eW <- 1 / (1 + 10^((loser_elo - winner_elo) / width))
    elo_env$ratings[as.character(winner_id)] <- winner_elo + k * (1 - eW)
    elo_env$ratings[as.character(loser_id)]  <- loser_elo  + k * (0 - (1 - eW))
    
    list(winner_id = winner_id, p_team1 = p1, p_team2 = p2)
  }
}

prediction_model_kenpom <- make_kenpom_prediction_model(
  kenpom_data = kenpom_final,
  elo_data    = elo_2025,       # starting point only — updates live during bracket
  model       = model,
  sigma       = sigma,
  mode        = "stochastic"
)

source("R_Files/bracket_placement.R")

bracket_results_2026 <- run_full_tournament(
  teams_df         = teams_2026,
  prediction_model = prediction_model_kenpom
)

bracket_results_2026 %>%
  arrange(round, region, slot) %>%
  select(round, region, game_id,
         TeamName_High, seed_high,
         TeamName_Low,  seed_low,
         winner_id,
         p_team_high, p_team_low) %>%
  print(n = 100)

champion_row <- bracket_results_2026 %>% filter(round == 6)
cat("\nPredicted championship matchup:\n")
print(champion_row)

View(bracket_results_2026)