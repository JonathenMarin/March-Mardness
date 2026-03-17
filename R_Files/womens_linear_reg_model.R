library(tidyr)
library(dplyr)
library(data.table)
library(fuzzyjoin)
library(ggplot2)
library(performance)

# 1. DATA LOAD AND CLEANING -----------------------------------------------

Wteams           <- fread("march-machine-learning-mania-2026/WTeams.csv")
womens_ranks     <- fread("Excel_Files/womens_2026_reg_season_rankings.csv") %>%
  mutate(Team = trimws(gsub("[^a-zA-Z0-9 &'.-]", "", Team)))
womens_spellings <- fread("march-machine-learning-mania-2026/WTeamSpellings.csv")

womens_ranks_norm <- womens_ranks %>%
  mutate(Team_normalized = tolower(Team))

womens_spellings_norm <- womens_spellings %>%
  mutate(TeamNameSpelling_normalized = tolower(TeamNameSpelling))

womens_ranks_matched <- womens_ranks_norm %>%
  left_join(
    womens_spellings_norm %>% select(TeamNameSpelling_normalized, TeamID) %>% distinct(),
    by = c("Team_normalized" = "TeamNameSpelling_normalized")
  )

womens_unmatched <- womens_ranks_matched %>% filter(is.na(TeamID))

if (nrow(womens_unmatched) > 0) {
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
  
  womens_ranks_final <- womens_ranks_matched %>%
    filter(!is.na(TeamID)) %>%
    bind_rows(womens_fuzzy_matches %>% select(names(womens_ranks_matched)))
} else {
  womens_ranks_final <- womens_ranks_matched
}

womens_ranks_final <- womens_ranks_final %>% select(-Team_normalized)

cat("Matched women's teams:", sum(!is.na(womens_ranks_final$TeamID)), "out of", nrow(womens_ranks), "\n")

womens_still_unmatched <- womens_ranks_final %>% filter(is.na(TeamID))
if (nrow(womens_still_unmatched) > 0) {
  cat("\nStill unmatched women's teams:\n")
  print(womens_still_unmatched$Team)
}


# 2. ELO RATINGS ----------------------------------------------------------

womens_results_all <- fread("march-machine-learning-mania-2026/WRegularSeasonDetailedResults.csv")

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

elo_ratings_womens <- calculate_elo(womens_results_all)
elo_2025_womens    <- elo_ratings_womens[Season == 2025, .(TeamID, Elo_final)]

cat("Women's teams with 2025 Elo ratings:", nrow(elo_2025_womens), "\n")


# 3. PREPARE TRAINING DATA ------------------------------------------------

womens_results <- womens_results_all %>% filter(Season == 2025)
cat("Total women's 2025 games:", nrow(womens_results), "\n")

womens_results_with_wteam <- womens_results %>%
  left_join(
    womens_ranks_final %>% select(TeamID, AdjOE, AdjDE, `Adj T.`),
    by = c("WTeamID" = "TeamID")
  ) %>%
  rename(WTeam_ORtg = AdjOE, WTeam_DRtg = AdjDE, WTeam_AdjT = `Adj T.`) %>%
  left_join(elo_2025_womens, by = c("WTeamID" = "TeamID")) %>%
  rename(WTeam_Elo = Elo_final)

womens_results_with_both <- womens_results_with_wteam %>%
  left_join(
    womens_ranks_final %>% select(TeamID, AdjOE, AdjDE, `Adj T.`),
    by = c("LTeamID" = "TeamID")
  ) %>%
  rename(LTeam_ORtg = AdjOE, LTeam_DRtg = AdjDE, LTeam_AdjT = `Adj T.`) %>%
  left_join(elo_2025_womens, by = c("LTeamID" = "TeamID")) %>%
  rename(LTeam_Elo = Elo_final)

womens_results_clean <- womens_results_with_both %>%
  filter(!is.na(WTeam_ORtg) & !is.na(LTeam_ORtg) &
           !is.na(WTeam_Elo)  & !is.na(LTeam_Elo))

cat("Women's games remaining:", nrow(womens_results_clean), "out of", nrow(womens_results), "\n")

womens_train_wteam <- womens_results_clean %>%
  transmute(
    Points    = WScore,
    OffRating = WTeam_ORtg,
    DefRating = LTeam_DRtg,
    AdjT_team = WTeam_AdjT,
    AdjT_opp  = LTeam_AdjT,
    Diff_Elo  = WTeam_Elo - LTeam_Elo
  )

womens_train_lteam <- womens_results_clean %>%
  transmute(
    Points    = LScore,
    OffRating = LTeam_ORtg,
    DefRating = WTeam_DRtg,
    AdjT_team = LTeam_AdjT,
    AdjT_opp  = WTeam_AdjT,
    Diff_Elo  = LTeam_Elo - WTeam_Elo
  )

womens_train_data <- bind_rows(womens_train_wteam, womens_train_lteam)
cat("Total women's training samples:", nrow(womens_train_data), "\n")
summary(womens_train_data)


# 4. MODEL CREATION -------------------------------------------------------

womens_model <- lm(Points ~ OffRating + DefRating + AdjT_team + AdjT_opp + Diff_Elo,
                   data = womens_train_data)

summary(womens_model)
cat("\nWomen's Model Coefficients:\n")
print(coef(womens_model))

womens_sigma <- summary(womens_model)$sigma
cat("Women's model standard deviation (sigma):", womens_sigma, "\n")

womens_train_data$Predicted <- predict(womens_model, womens_train_data)

plot(womens_model)
check_model(womens_model, check = "vif")


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
  team_a <- kenpom_data %>% filter(TeamID == team_a_id)
  team_b <- kenpom_data %>% filter(TeamID == team_b_id)
  elo_a  <- elo_data    %>% filter(TeamID == team_a_id) %>% pull(Elo_final)
  elo_b  <- elo_data    %>% filter(TeamID == team_b_id) %>% pull(Elo_final)
  
  if (nrow(team_a) == 0 | nrow(team_b) == 0 |
      length(elo_a) == 0 | length(elo_b) == 0) {
    warning(paste("Missing data for team", team_a_id, "or", team_b_id))
    return(list(team_a_win_prob = 0.5, team_b_win_prob = 0.5, avg_margin = 0))
  }
  
  team_a_pred <- predict(model, newdata = data.frame(
    OffRating = team_a$AdjOE,
    DefRating = team_b$AdjDE,
    AdjT_team = team_a$`Adj T.`,
    AdjT_opp  = team_b$`Adj T.`,
    Diff_Elo  = elo_a - elo_b
  ))
  
  team_b_pred <- predict(model, newdata = data.frame(
    OffRating = team_b$AdjOE,
    DefRating = team_a$AdjDE,
    AdjT_team = team_b$`Adj T.`,
    AdjT_opp  = team_a$`Adj T.`,
    Diff_Elo  = elo_b - elo_a
  ))
  
  result <- simulate_game(team_a_pred, team_b_pred, sigma, n_sims = 500)
  return(result)
}


# 6. GENERATE ALL-PAIRS 2026 PREDICTIONS ----------------------------------

seeds_w      <- fread("march-machine-learning-mania-2026/WNCAATourneySeeds.csv")
seeds_2026_w <- seeds_w %>% filter(Season == 2026)

womens_team_ids_2026 <- seeds_2026_w$TeamID

cat("Teams in 2026 seed file:", length(womens_team_ids_2026), "\n")
cat("Seed teams with ranking data:", sum(womens_team_ids_2026 %in% womens_ranks_final$TeamID), "\n")
cat("Seed teams with Elo data:", sum(womens_team_ids_2026 %in% elo_2025_womens$TeamID), "\n")

matchups_2026_w <- expand.grid(Team1 = womens_team_ids_2026, Team2 = womens_team_ids_2026) %>%
  filter(Team1 < Team2)

cat("Total women's matchups to predict:", nrow(matchups_2026_w), "\n")

womens_predictions_list <- list()

cat("Predicting all women's matchups...\n")
for (i in 1:nrow(matchups_2026_w)) {
  team_a_id <- matchups_2026_w$Team1[i]
  team_b_id <- matchups_2026_w$Team2[i]
  
  result <- predict_matchup(
    team_a_id   = team_a_id,
    team_b_id   = team_b_id,
    kenpom_data = womens_ranks_final,
    elo_data    = elo_2025_womens,
    model       = womens_model,
    sigma       = womens_sigma
  )
  
  womens_predictions_list[[i]] <- data.frame(
    ID   = paste(2026, team_a_id, team_b_id, sep = "_"),
    Pred = result$team_a_win_prob
  )
  
  if (i %% 100 == 0) cat("Completed", i, "of", nrow(matchups_2026_w), "\n")
}

submission_2026_womens <- bind_rows(womens_predictions_list)
write.csv(submission_2026_womens, "Excel_Files/submission_2026_model3_womens.csv", row.names = FALSE)

cat("Submission written:", nrow(submission_2026_womens), "rows\n")
cat("Pred range: [", round(min(submission_2026_womens$Pred), 4), ",",
    round(max(submission_2026_womens$Pred), 4), "]\n\n")


# 7. BRACKET SIMULATION (with live Elo updates) ---------------------------

seeds_2026_bracket_w <- seeds_2026_w %>%
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

teams_2026_w <- seeds_2026_bracket_w %>%
  left_join(Wteams %>% select(TeamID, TeamName), by = "TeamID") %>%
  transmute(TeamID, TeamName, Region, Seed = SeedNum)

cat("Teams in 2026 women's bracket:", nrow(teams_2026_w), "\n")

make_womens_prediction_model <- function(kenpom_data, elo_data, model, sigma,
                                         mode = c("stochastic", "deterministic"),
                                         k = 64, width = 400) {
  mode <- match.arg(mode)
  
  # Copy Elo into a mutable environment — live updates during bracket only
  # Does NOT affect elo_2025_womens used in the Kaggle submission above
  elo_env <- new.env(parent = emptyenv())
  elo_env$ratings <- setNames(elo_data$Elo_final, as.character(elo_data$TeamID))
  
  function(team1_id, team2_id) {
    
    elo_a <- elo_env$ratings[as.character(team1_id)]
    elo_b <- elo_env$ratings[as.character(team2_id)]
    
    if (is.na(elo_a)) elo_a <- 1500
    if (is.na(elo_b)) elo_b <- 1500
    
    team_a <- kenpom_data %>% filter(TeamID == team1_id)
    team_b <- kenpom_data %>% filter(TeamID == team2_id)
    
    if (nrow(team_a) == 0 | nrow(team_b) == 0) {
      return(list(winner_id = team1_id, p_team1 = 0.5, p_team2 = 0.5))
    }
    
    team_a_pred <- predict(model, newdata = data.frame(
      OffRating = team_a$AdjOE,
      DefRating = team_b$AdjDE,
      AdjT_team = team_a$`Adj T.`,
      AdjT_opp  = team_b$`Adj T.`,
      Diff_Elo  = elo_a - elo_b
    ))
    
    team_b_pred <- predict(model, newdata = data.frame(
      OffRating = team_b$AdjOE,
      DefRating = team_a$AdjDE,
      AdjT_team = team_b$`Adj T.`,
      AdjT_opp  = team_a$`Adj T.`,
      Diff_Elo  = elo_b - elo_a
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
    loser_id   <- ifelse(winner_id == team1_id, team2_id, team1_id)
    winner_elo <- elo_env$ratings[as.character(winner_id)]
    loser_elo  <- elo_env$ratings[as.character(loser_id)]
    
    eW <- 1 / (1 + 10^((loser_elo - winner_elo) / width))
    elo_env$ratings[as.character(winner_id)] <- winner_elo + k * (1 - eW)
    elo_env$ratings[as.character(loser_id)]  <- loser_elo  + k * (0 - (1 - eW))
    
    list(winner_id = winner_id, p_team1 = p1, p_team2 = p2)
  }
}

prediction_model_womens <- make_womens_prediction_model(
  kenpom_data = womens_ranks_final,
  elo_data    = elo_2025_womens,
  model       = womens_model,
  sigma       = womens_sigma,
  mode        = "stochastic"
)

source("R_Files/bracket_placement.R")

bracket_results_2026_w <- run_full_tournament(
  teams_df         = teams_2026_w,
  prediction_model = prediction_model_womens
)

bracket_results_2026_w %>%
  arrange(round, region, slot) %>%
  select(round, region, game_id,
         TeamName_High, seed_high,
         TeamName_Low,  seed_low,
         winner_id,
         p_team_high, p_team_low) %>%
  print(n = 100)

champion_row_w <- bracket_results_2026_w %>% filter(round == 6)
cat("\nPredicted women's championship matchup:\n")
print(champion_row_w)

View(bracket_results_2026_w)


# 8. COMBINE MENS & WOMENS FOR KAGGLE SUBMISSION --------------------------

submission_2026_mens     <- fread("Excel_Files/submission_2026_model3_mens.csv")
submission_2026_womens   <- fread("Excel_Files/submission_2026_model3_womens.csv")

submission_2026_combined <- bind_rows(submission_2026_mens, submission_2026_womens)

write.csv(submission_2026_combined, "Excel_Files/submission_2026_model3_combined.csv", row.names = FALSE)

cat("Combined submission written:", nrow(submission_2026_combined), "rows\n")
cat("  Mens matchups:  ", nrow(submission_2026_mens),   "\n")
cat("  Womens matchups:", nrow(submission_2026_womens), "\n")
cat("Pred range: [", round(min(submission_2026_combined$Pred), 4), ",",
    round(max(submission_2026_combined$Pred), 4), "]\n")