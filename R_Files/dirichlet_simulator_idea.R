library(dplyr)
library(lme4)
library(hoopR)
library(tidyr)
library(performance)
library(gtools)# For the Dirichlet distribution
library(progress)
# Load player-level box scores and calculate per-minute statistics
player_data <- hoopR::load_mbb_player_box(seasons = 2025) %>%
  mutate(
    game_date = as.Date(game_date),
    ast_per_min = assists / pmax(minutes, 1),
    fgm_per_min = field_goals_made / pmax(minutes, 1),
    fga_per_min = field_goals_attempted / pmax(minutes, 1),
    tpm_per_min = three_point_field_goals_made / pmax(minutes, 1),
    tpa_per_min = three_point_field_goals_attempted / pmax(minutes, 1),
    ftm_per_min = free_throws_made / pmax(minutes, 1),
    fta_per_min = free_throws_attempted / pmax(minutes, 1),
    oreb_per_min = offensive_rebounds / pmax(minutes, 1),
    dreb_per_min = defensive_rebounds / pmax(minutes, 1),
    stl_per_min = steals / pmax(minutes, 1),
    blk_per_min = blocks / pmax(minutes, 1),
    to_per_min = turnovers / pmax(minutes, 1),
    pf_per_min = fouls / pmax(minutes, 1)
  ) %>%
  filter(!is.na(minutes), minutes > 0) %>%
  filter(game_date < as.Date("2025-03-17"))

# Load team-level box scores and calculate possessions
team_games <- hoopR::load_mbb_team_box(seasons = 2025) %>%
  mutate(game_date = as.Date(game_date)) %>%
  filter(game_date < as.Date("2025-03-17")) %>%
  mutate(
    possessions = field_goals_attempted - offensive_rebounds + turnovers + (0.44 * free_throws_attempted)
  )


# Calculate season-long stats (ratings and pace)
team_eff_season <- team_games %>%
  group_by(team_id, team_display_name) %>% 
  summarize(
    num_games = n(),
    total_possessions = sum(possessions, na.rm = TRUE),
    season_pace = total_possessions / num_games,
    off_rating = (sum(team_score, na.rm = TRUE) / total_possessions) * 100,
    def_rating = (sum(opponent_team_score, na.rm = TRUE) / total_possessions) * 100,
    .groups = 'drop'
  )

# Calculate pace from the last 10 games
team_eff_recent <- team_games %>%
  group_by(team_id) %>%
  arrange(desc(game_date)) %>%
  slice(1:10) %>%
  summarize(
    recent_pace = sum(possessions, na.rm = TRUE) / n(),
    .groups = 'drop'
  )

# Join season and recent stats to create the final weighted pace
team_eff <- team_eff_season %>%
  left_join(team_eff_recent, by = "team_id") %>%
  mutate(recent_pace = coalesce(recent_pace, season_pace)) %>%
  mutate(
    weighted_pace = (0.8 * recent_pace) + (0.2 * season_pace)
  )


# Join team efficiency stats back to the player data
player_data <- player_data %>%
  left_join(
    team_eff %>% select(team_id, team_off_rating = off_rating, team_def_rating = def_rating),
    by = "team_id"
  ) %>%
  left_join(
    team_eff %>% select(opponent_team_id = team_id,
                        opp_off_rating = off_rating,
                        opp_def_rating = def_rating),
    by = "opponent_team_id"
  ) %>%
  # Scale predictors for modeling
  mutate(
    minutes_s = scale(minutes),
    team_off_rating_s = scale(team_off_rating),
    team_def_rating_s = scale(team_def_rating),
    opp_off_rating_s = scale(opp_off_rating),
    opp_def_rating_s = scale(opp_def_rating)
  ) %>%
  filter(!is.na(team_off_rating_s), !is.na(opp_off_rating_s))


# Define a control object to help models converge
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# Base formula for all per-minute models
base_formula <- " ~ minutes_s + team_off_rating_s + team_def_rating_s + opp_off_rating_s + opp_def_rating_s + (1 | athlete_id)"

# Build a model for each statistic
model_ast  <- lmer(as.formula(paste("ast_per_min", base_formula)), data = player_data, control = ctrl)
model_fgm  <- lmer(as.formula(paste("fgm_per_min", base_formula)), data = player_data, control = ctrl)
model_fga  <- lmer(as.formula(paste("fga_per_min", base_formula)), data = player_data, control = ctrl)
model_tpm  <- lmer(as.formula(paste("tpm_per_min", base_formula)), data = player_data, control = ctrl)
model_tpa  <- lmer(as.formula(paste("tpa_per_min", base_formula)), data = player_data, control = ctrl)
model_ftm  <- lmer(as.formula(paste("ftm_per_min", base_formula)), data = player_data, control = ctrl)
model_fta  <- lmer(as.formula(paste("fta_per_min", base_formula)), data = player_data, control = ctrl)
model_oreb <- lmer(as.formula(paste("oreb_per_min", base_formula)), data = player_data, control = ctrl)
model_dreb <- lmer(as.formula(paste("dreb_per_min", base_formula)), data = player_data, control = ctrl)
model_stl  <- lmer(as.formula(paste("stl_per_min", base_formula)), data = player_data, control = ctrl)
model_blk  <- lmer(as.formula(paste("blk_per_min", base_formula)), data = player_data, control = ctrl)
model_to   <- lmer(as.formula(paste("to_per_min", base_formula)), data = player_data, control = ctrl)
model_pf   <- lmer(as.formula(paste("pf_per_min", base_formula)), data = player_data, control = ctrl)

# Build the model for minutes (uses a slightly different formula)
model_min <- lmer(minutes ~ team_off_rating_s + team_def_rating_s + opp_off_rating_s + opp_def_rating_s + (1 | athlete_id), data = player_data, control = ctrl)



# Step 6: The Simulation Function

simulate_game <- function(n_sims = 100) {
  
  home_team_id_input <- as.numeric(readline(prompt = "Enter Home Team ID: "))
  away_team_id_input <- as.numeric(readline(prompt = "Enter Away Team ID: "))
  
  home_team_info <- team_eff %>% filter(team_id == home_team_id_input)
  if(nrow(home_team_info) == 0) stop("Home team_id not found")
  
  away_team_info <- team_eff %>% filter(team_id == away_team_id_input)
  if(nrow(away_team_info) == 0) stop("Away team_id not found")
  
  cat("\nSimulating", n_sims, "games for:", home_team_info$team_display_name, "vs.", away_team_info$team_display_name, "\n")
  
  # --- Helper function to calculate player "strength scores" for minutes ---
  get_minute_strength_scores <- function(team_info, opponent_info, roster) {
    team_off_s <- (team_info$off_rating - mean(player_data$team_off_rating, na.rm = TRUE)) / sd(player_data$team_off_rating, na.rm = TRUE)
    team_def_s <- (team_info$def_rating - mean(player_data$team_def_rating, na.rm = TRUE)) / sd(player_data$team_def_rating, na.rm = TRUE)
    opp_off_s  <- (opponent_info$off_rating - mean(player_data$opp_off_rating, na.rm = TRUE)) / sd(player_data$opp_off_rating, na.rm = TRUE)
    opp_def_s  <- (opponent_info$def_rating - mean(player_data$opp_def_rating, na.rm = TRUE)) / sd(player_data$opp_def_rating, na.rm = TRUE)
    
    roster_pred_data <- roster %>%
      bind_cols(data.frame(team_off_rating_s=team_off_s, team_def_rating_s=team_def_s, opp_off_rating_s=opp_off_s, opp_def_rating_s=opp_def_s))
    
    strength_score <- predict(model_min, newdata = roster_pred_data, allow.new.levels = TRUE)
    strength_score[strength_score < 0] <- 0
    
    strength_score <- strength_score + 5 # Baseline score
    
    avg_minutes <- player_data %>%
      filter(athlete_id %in% roster$athlete_id) %>%
      group_by(athlete_id) %>%
      summarise(avg_min = mean(minutes, na.rm = TRUE), .groups = "drop")
    
    roster_pred_data <- roster_pred_data %>%
      left_join(avg_minutes, by = "athlete_id") %>%
      mutate(avg_min = coalesce(avg_min, 0))
    
    starter_bonus <- ifelse(roster_pred_data$avg_min >= 28, 15, 0)
    strength_score <- strength_score + starter_bonus
    
    # Add a small value to avoid alphas of 0, which rdirichlet doesn't like
    strength_score <- strength_score + 0.01 
    
    return(strength_score)
  }
  
  # --- Get rosters and calculate static strength scores once ---
  get_roster <- function(team_id_num) {
    player_data %>%
      filter(team_id == team_id_num) %>%
      group_by(athlete_id, athlete_display_name) %>%
      summarise(last_game = max(game_date), .groups = 'drop') %>%
      select(athlete_id, athlete_display_name)
  }
  home_roster <- get_roster(home_team_id_input)
  away_roster <- get_roster(away_team_id_input)
  
  home_minute_alphas <- get_minute_strength_scores(home_team_info, away_team_info, home_roster)
  away_minute_alphas <- get_minute_strength_scores(away_team_info, home_team_info, away_roster)
  
  pb <- progress::progress_bar$new(
    format = " simulating [:bar] :percent in :elapsed",
    total = n_sims, clear = FALSE, width = 60
  )
  # --- Simulation Loop ---
  results <- purrr::map_dfr(1:n_sims, ~{
    
    pb$tick()
    
    # 1. Simulate minutes for this game using Dirichlet distribution
    home_minute_shares <- gtools::rdirichlet(1, home_minute_alphas)
    home_sim_minutes <- as.vector(home_minute_shares * 200)
    
    away_minute_shares <- gtools::rdirichlet(1, away_minute_alphas)
    away_sim_minutes <- as.vector(away_minute_shares * 200)
    
    # 2. Predict stats based on simulated minutes
    predict_stats_for_sim <- function(team_info, opponent_info, roster, sim_minutes) {
      team_off_s <- (team_info$off_rating - mean(player_data$team_off_rating, na.rm = TRUE)) / sd(player_data$team_off_rating, na.rm = TRUE)
      team_def_s <- (team_info$def_rating - mean(player_data$team_def_rating, na.rm = TRUE)) / sd(player_data$team_def_rating, na.rm = TRUE)
      opp_off_s  <- (opponent_info$off_rating - mean(player_data$opp_off_rating, na.rm = TRUE)) / sd(player_data$opp_off_rating, na.rm = TRUE)
      opp_def_s  <- (opponent_info$def_rating - mean(player_data$opp_def_rating, na.rm = TRUE)) / sd(player_data$opp_def_rating, na.rm = TRUE)
      
      pred_data <- roster %>%
        bind_cols(data.frame(team_off_rating_s=team_off_s, team_def_rating_s=team_def_s, opp_off_rating_s=opp_off_s, opp_def_rating_s=opp_def_s))
      
      pred_data$minutes <- sim_minutes
      pred_data$minutes_s <- (sim_minutes - mean(player_data$minutes, na.rm = TRUE)) / sd(player_data$minutes, na.rm = TRUE)
      
      pred_data %>%
        mutate(
          FGM = predict(model_fgm, newdata=., allow.new.levels=TRUE) * minutes, FGA = predict(model_fga, newdata=., allow.new.levels=TRUE) * minutes,
          TPM = predict(model_tpm, newdata=., allow.new.levels=TRUE) * minutes, TPA = predict(model_tpa, newdata=., allow.new.levels=TRUE) * minutes,
          FTM = predict(model_ftm, newdata=., allow.new.levels=TRUE) * minutes, FTA = predict(model_fta, newdata=., allow.new.levels=TRUE) * minutes,
          OREB = predict(model_oreb, newdata=., allow.new.levels=TRUE) * minutes, `TO` = predict(model_to, newdata=., allow.new.levels=TRUE) * minutes
        )
    }
    
    home_baseline_box <- predict_stats_for_sim(home_team_info, away_team_info, home_roster, home_sim_minutes)
    away_baseline_box <- predict_stats_for_sim(away_team_info, home_team_info, away_roster, away_sim_minutes)
    
    # 3. Apply pace adjustment
    calculate_implied_poss <- function(df) { sum(df$FGA) - sum(df$OREB) + sum(df$`TO`) + (0.44 * sum(df$FTA)) }
    implied_game_pace <- (calculate_implied_poss(home_baseline_box) + calculate_implied_poss(away_baseline_box)) / 2
    expected_game_pace <- (home_team_info$weighted_pace + away_team_info$weighted_pace) / 2
    final_adjustment_factor <- expected_game_pace / implied_game_pace
    
    # 4. Calculate final scores for this single simulation
    calculate_final_pts <- function(df, factor) {
      df <- df %>% mutate(across(c(FGM, TPM, FTM), ~ . * factor))
      sum((df$FGM - df$TPM) * 2 + df$TPM * 3 + df$FTM)
    }
    
    home_pts <- calculate_final_pts(home_baseline_box, final_adjustment_factor)
    away_pts <- calculate_final_pts(away_baseline_box, final_adjustment_factor)
    
    # 5. Return a single row of results for this sim
    tibble(sim_id = ., home_pts = home_pts, away_pts = away_pts)
  })
  
  # --- Summarize and Print Results ---
  summary <- results %>%
    summarise(
      home_avg_pts = mean(home_pts),
      away_avg_pts = mean(away_pts),
      home_wins = sum(home_pts > away_pts),
      away_wins = sum(away_pts > home_pts)
    ) %>%
    mutate(
      home_win_prob = home_wins / n_sims,
      away_win_prob = away_wins / n_sims
    )
  
  cat("\n--- SIMULATION SUMMARY ---\n")
  cat(sprintf("Avg. Score: %s %.1f - %s %.1f\n",
              home_team_info$team_display_name, summary$home_avg_pts,
              away_team_info$team_display_name, summary$away_avg_pts))
  cat(sprintf("Win Probability: %s %.1f%% - %s %.1f%%\n",
              home_team_info$team_display_name, summary$home_win_prob * 100,
              away_team_info$team_display_name, summary$away_win_prob * 100))
  cat("--------------------------\n")
  
  return(results)
}



# After running all the code above, you can run the simulation and save the results
game_sim_results <- simulate_game(n_sims = 100)
