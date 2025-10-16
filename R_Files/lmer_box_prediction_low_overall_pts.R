
library(dplyr)
library(lme4)
library(hoopR)
library(tidyr)
library(performance)

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
    weighted_pace = (0.7 * recent_pace) + (0.3 * season_pace)
  )

# -------------------------------------------------------------------------
# Step 4: Finalize Player Data for Modeling
# -------------------------------------------------------------------------
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
  filter(!is.na(team_off_rating_s), !is.na(opp_off_rating_s)) # Remove rows with missing data

# -------------------------------------------------------------------------
# Step 5: Build All Mixed-Effects Models
# -------------------------------------------------------------------------
cat("Building mixed-effects models (this may take a few minutes)...\n")
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
cat("All models built successfully.\n")

# -------------------------------------------------------------------------
# Step 6: The Final Prediction Function
# -------------------------------------------------------------------------
predict_team_matchup_full <- function() {
  
  home_team_id_input <- as.numeric(readline(prompt = "Enter Home Team ID: "))
  away_team_id_input <- as.numeric(readline(prompt = "Enter Away Team ID: "))
  
  home_team_info <- team_eff %>% filter(team_id == home_team_id_input)
  if(nrow(home_team_info) == 0) stop("Home team_id not found")
  
  away_team_info <- team_eff %>% filter(team_id == away_team_id_input)
  if(nrow(away_team_info) == 0) stop("Away team_id not found")
  
  get_roster <- function(team_id_num) {
    player_data %>%
      filter(team_id == team_id_num) %>%
      group_by(athlete_id, athlete_display_name) %>%
      summarise(last_game = max(game_date), .groups = 'drop') %>%
      select(athlete_id, athlete_display_name)
  }
  
  home_roster <- get_roster(home_team_id_input)
  away_roster <- get_roster(away_team_id_input)
  
  cat("\nMatchup:", home_team_info$team_display_name, "(Home) vs.", away_team_info$team_display_name, "(Away)\n")
  
  # --- PACE ADJUSTMENT LOGIC ---
  expected_game_pace <- (home_team_info$weighted_pace + away_team_info$weighted_pace) / 2
  average_league_pace <- mean(team_eff$weighted_pace, na.rm = TRUE)
  pace_adjustment_factor <- expected_game_pace / average_league_pace
  
  cat(sprintf("Pace Adjustment: Game is expected to be %.1f%% %s than league average.\n",
              abs(1 - pace_adjustment_factor) * 100,
              ifelse(pace_adjustment_factor > 1, "faster", "slower")))
  
  # --- HELPER FUNCTIONS ---
  predict_one_team <- function(team_info, opponent_info, roster) {
    # ---- STEP 1: Standardized predictors ----
    team_off_s <- (team_info$off_rating - mean(player_data$team_off_rating, na.rm = TRUE)) / sd(player_data$team_off_rating, na.rm = TRUE)
    team_def_s <- (team_info$def_rating - mean(player_data$team_def_rating, na.rm = TRUE)) / sd(player_data$team_def_rating, na.rm = TRUE)
    opp_off_s  <- (opponent_info$off_rating - mean(player_data$opp_off_rating, na.rm = TRUE)) / sd(player_data$opp_off_rating, na.rm = TRUE)
    opp_def_s  <- (opponent_info$def_rating - mean(player_data$opp_def_rating, na.rm = TRUE)) / sd(player_data$opp_def_rating, na.rm = TRUE)
    
    roster_pred_data <- roster %>%
      bind_cols(data.frame(
        team_off_rating_s = team_off_s,
        team_def_rating_s = team_def_s,
        opp_off_rating_s = opp_off_s,
        opp_def_rating_s = opp_def_s
      ))
    
    # ---- STEP 2: Predict minutes ----
    unconstrained_minutes <- predict(model_min, newdata = roster_pred_data, allow.new.levels = TRUE)
    unconstrained_minutes[unconstrained_minutes < 0] <- 0
    
    # Add a small baseline for all players
    unconstrained_minutes <- unconstrained_minutes + 5
    
    # ---- NEW SECTION: Starter Bonus ----
    # Calculate each player’s average historical minutes
    avg_minutes <- player_data %>%
      filter(athlete_id %in% roster$athlete_id) %>%
      group_by(athlete_id) %>%
      summarise(avg_min = mean(minutes, na.rm = TRUE), .groups = "drop")
    
    roster_pred_data <- roster_pred_data %>%
      left_join(avg_minutes, by = "athlete_id") %>%
      mutate(avg_min = coalesce(avg_min, 0))
    
    # Add +10 minutes to predicted minutes if avg_min >= 30 (starter)
    unconstrained_minutes <- unconstrained_minutes + ifelse(roster_pred_data$avg_min >= 30, 10, 0)
    
    # Normalize to 200 total team minutes
    minute_proportions <- unconstrained_minutes / sum(unconstrained_minutes)
    pred_minutes <- minute_proportions * 200
    
    roster_pred_data$minutes <- pred_minutes
    roster_pred_data$minutes_s <- (pred_minutes - mean(player_data$minutes, na.rm = TRUE)) / sd(player_data$minutes, na.rm = TRUE)
    
    # ---- STEP 3: Predict per-minute stats and convert to totals ----
    base_preds <- roster_pred_data %>%
      mutate(
        MIN = minutes,
        FGM = predict(model_fgm, newdata = ., allow.new.levels = TRUE) * minutes,
        FGA = predict(model_fga, newdata = ., allow.new.levels = TRUE) * minutes,
        TPM = predict(model_tpm, newdata = ., allow.new.levels = TRUE) * minutes,
        TPA = predict(model_tpa, newdata = ., allow.new.levels = TRUE) * minutes,
        FTM = predict(model_ftm, newdata = ., allow.new.levels = TRUE) * minutes,
        FTA = predict(model_fta, newdata = ., allow.new.levels = TRUE) * minutes,
        OREB = predict(model_oreb, newdata = ., allow.new.levels = TRUE) * minutes,
        DREB = predict(model_dreb, newdata = ., allow.new.levels = TRUE) * minutes,
        AST = predict(model_ast, newdata = ., allow.new.levels = TRUE) * minutes,
        STL = predict(model_stl, newdata = ., allow.new.levels = TRUE) * minutes,
        BLK = predict(model_blk, newdata = ., allow.new.levels = TRUE) * minutes,
        `TO` = predict(model_to, newdata = ., allow.new.levels = TRUE) * minutes,
        PF = predict(model_pf, newdata = ., allow.new.levels = TRUE) * minutes
      )
    
    # ---- STEP 4: Scale team output to match expected efficiency ----
    expected_team_points <- team_info$off_rating * (team_info$season_pace / 100)
    actual_pred_points <- sum(
      (base_preds$FGM - base_preds$TPM) * 2 + base_preds$TPM * 3 + base_preds$FTM
    )
    
    if (actual_pred_points <= 1) actual_pred_points <- 1
    scaling_factor <- as.numeric(expected_team_points / actual_pred_points)
    
    base_preds <- base_preds %>%
      mutate(across(
        c(FGM, FGA, TPM, TPA, FTM, FTA, OREB, DREB, AST, STL, BLK, `TO`, PF),
        ~ . * scaling_factor
      ))
    
    return(base_preds)
  }
  
  
  
  finalize_box_score <- function(baseline_df, adjustment_factor) {
    counting_stats <- c("FGM", "FGA", "TPM", "TPA", "FTM", "FTA", "OREB", "DREB", "AST", "STL", "BLK", "TO", "PF")
    baseline_df %>%
      mutate(across(all_of(counting_stats), ~ . * adjustment_factor)) %>%
      mutate(across(FGM:PF, ~pmax(0, .))) %>%
      mutate(FGM = pmin(FGM, FGA), TPM = pmin(TPM, TPA), FTM = pmin(FTM, FTA)) %>%
      mutate(
        Player = athlete_display_name,
        FG = paste0(round(FGM), "-", round(FGA)),
        `3PT` = paste0(round(TPM), "-", round(TPA)),
        FT = paste0(round(FTM), "-", round(FTA)),
        REB = round(OREB) + round(DREB),
        PTS = (round(FGM) - round(TPM)) * 2 + round(TPM) * 3 + round(FTM)
      ) %>%
      select(Player, MIN, FG, `3PT`, FT, OREB, DREB, REB, AST, STL, BLK, `TO`, PF, PTS) %>%
      mutate(across(where(is.numeric), ~round(., 1)))
  }
  
  print_box_score <- function(box_score_df, team_name) {
    cat("\n---", toupper(team_name), "---\n")
    
    # --- FIXED CODE BLOCK ---
    box_score_df <- box_score_df %>% 
      mutate(
        # First, handle all the numeric columns
        across(where(is.numeric), ~if_else(MIN < 0.1, 0, .)),
        # Second, handle the specific character columns for shots
        across(c(FG, `3PT`, FT), ~if_else(MIN < 0.1, "0-0", .))
      )
    # --- END OF FIX ---
    
    print(box_score_df)
    
    split_shots <- function(df, col) {
      suppressWarnings(
        df %>%
          tidyr::separate(!!col, into = c("made", "att"), sep = "-", convert = TRUE, remove = FALSE) %>%
          summarise(made_sum = sum(made, na.rm = TRUE), att_sum = sum(att, na.rm = TRUE)))
    }
    fg_totals <- split_shots(box_score_df, "FG"); tp_totals <- split_shots(box_score_df, "3PT"); ft_totals <- split_shots(box_score_df, "FT")
    
    team_totals <- box_score_df %>%
      summarise(across(c(MIN, OREB, DREB, REB, AST, STL, BLK, `TO`, PF, PTS), ~round(sum(.))), Player = "TOTAL") %>%
      mutate(FG = paste0(fg_totals$made_sum, "-", fg_totals$att_sum), `3PT` = paste0(tp_totals$made_sum, "-", tp_totals$att_sum),
             FT = paste0(ft_totals$made_sum, "-", ft_totals$att_sum), .before = OREB)
    cat("\n"); print(team_totals); cat("---------------------------\n")
  }
  
  # --- EXECUTION ---
  home_baseline_box <- predict_one_team(home_team_info, away_team_info, home_roster)
  away_baseline_box <- predict_one_team(away_team_info, home_team_info, away_roster)
  
  home_box_score <- finalize_box_score(home_baseline_box, pace_adjustment_factor)
  away_box_score <- finalize_box_score(away_baseline_box, pace_adjustment_factor)
  
  print_box_score(home_box_score, home_team_info$team_display_name)
  print_box_score(away_box_score, away_team_info$team_display_name)
}

# -------------------------------------------------------------------------
# Step 7: Run the Prediction
# -------------------------------------------------------------------------
predict_team_matchup_full()
