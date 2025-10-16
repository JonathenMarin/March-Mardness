library(dplyr)
library(lme4)
library(hoopR)


# Load and prepare player box score data
player_data <- hoopR::load_mbb_player_box(seasons = 2025) %>%
  mutate(
    game_date = as.Date(game_date),
    pts_per_min = points / pmax(minutes, 1),
    ast_per_min = assists / pmax(minutes, 1),
    reb_per_min = rebounds / pmax(minutes, 1)
  ) %>%
  filter(!is.na(pts_per_min)) %>%
  filter(game_date < as.Date("2025-03-17"))

# Load and prepare team box score data
team_games <- hoopR::load_mbb_team_box(seasons = 2025) %>%
  mutate(game_date = as.Date(game_date)) %>%
  filter(game_date < as.Date("2025-03-17"))

# Calculate possession-based ratings for each game
team_games <- team_games %>%
  mutate(
    possessions = field_goals_attempted - offensive_rebounds + turnovers + (0.44 * free_throws_attempted),
    def_rating = (opponent_team_score / possessions) * 100,
    off_rating = (team_score / possessions) * 100
  )

# Calculate season-long team efficiency ratings
team_eff <- team_games %>%
  group_by(team_id, team_name, team_display_name) %>% 
  summarize(
    total_points_scored = sum(team_score, na.rm = TRUE),
    total_points_allowed = sum(opponent_team_score, na.rm = TRUE),
    total_possessions = sum(possessions, na.rm = TRUE),
    off_rating = (total_points_scored / total_possessions) * 100,
    def_rating = (total_points_allowed / total_possessions) * 100,
    .groups = 'drop'
  )

# Join team and opponent efficiency ratings to player data
player_data <- player_data %>%
  left_join(
    team_eff %>% select(team_id, team_off_rating = off_rating, team_def_rating = def_rating),
    by = "team_id"
  )

player_data <- player_data %>%
  left_join(
    team_eff %>% select(opponent_team_id = team_id,
                        opp_off_rating = off_rating,
                        opp_def_rating = def_rating),
    by = "opponent_team_id"
  )


# Scale predictors for the model
player_data <- player_data %>%
  mutate(
    minutes_s = scale(minutes),
    team_off_rating_s = scale(team_off_rating),
    team_def_rating_s = scale(team_def_rating),
    opp_off_rating_s = scale(opp_off_rating), #scale function is (x - mean(x)) / sd(x)
    opp_def_rating_s = scale(opp_def_rating)
  )

# Build linear mixed-effects models
model_pts <- lmer(
  pts_per_min ~ minutes_s + team_off_rating_s + team_def_rating_s +
    opp_off_rating_s + opp_def_rating_s +
    (1 | athlete_id),
  data = player_data
)

model_ast <- lmer(
  ast_per_min ~ minutes_s + team_off_rating_s + team_def_rating_s +
    opp_off_rating_s + opp_def_rating_s +
    (1 | athlete_id),
  data = player_data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

model_reb <- lmer(
  reb_per_min ~ minutes_s + team_off_rating_s + team_def_rating_s +
    opp_off_rating_s + opp_def_rating_s +
    (1 | athlete_id),
  data = player_data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

model_min <- lmer(
  minutes ~ team_off_rating_s + team_def_rating_s + 
    opp_off_rating_s + opp_def_rating_s + 
    (1 | athlete_id), 
  data = player_data
)

# Define the prediction function
predict_player_stats_by_id <- function() {
  
  player_data_local <- player_data
  model_pts_local <- model_pts
  model_ast_local <- model_ast
  model_reb_local <- model_reb
  model_min_local <- model_min
  
  # Ask for athlete_id, opponent_team_id, and expected minutes
  athlete_id_input <- as.numeric(readline(prompt = "Enter athlete_id: "))
  opponent_id_input <- as.numeric(readline(prompt = "Enter opponent team_id: "))
  
  # Pull player's team info
  player_info <- player_data_local %>%
    filter(athlete_id == athlete_id_input) %>%
    select(
      athlete_display_name,
      team_display_name, 
      team_off_rating,
      team_def_rating
    ) %>%
    slice(1)
  
  if(nrow(player_info) == 0) stop("athlete_id not found in player_data")
  
  # Pull opponent's info from team_eff
  opponent_info <- team_eff %>%
    filter(team_id == opponent_id_input) %>%
    select(
      opponent_team_display_name = team_display_name,
      opp_off_rating = off_rating,
      opp_def_rating = def_rating
    )
  
  if(nrow(opponent_info) == 0) stop("opponent_team_id not found")
  
  # Scale numeric predictors
  team_off_s <- (player_info$team_off_rating - mean(player_data_local$team_off_rating, na.rm = TRUE)) / sd(player_data_local$team_off_rating, na.rm = TRUE)
  team_def_s <- (player_info$team_def_rating - mean(player_data_local$team_def_rating, na.rm = TRUE)) / sd(player_data_local$team_def_rating, na.rm = TRUE)
  opp_off_s <- (opponent_info$opp_off_rating - mean(player_data_local$opp_off_rating, na.rm = TRUE)) / sd(player_data_local$opp_off_rating, na.rm = TRUE)
  opp_def_s <- (opponent_info$opp_def_rating - mean(player_data_local$opp_def_rating, na.rm = TRUE)) / sd(player_data_local$opp_def_rating, na.rm = TRUE)
  
  # Create new data frame for prediction
  new_game_min <- data.frame(
    athlete_id = athlete_id_input,
    team_off_rating_s = team_off_s,
    team_def_rating_s = team_def_s,
    opp_off_rating_s = opp_off_s,
    opp_def_rating_s = opp_def_s
  )
  
  pred_minutes <- predict(model_min_local, newdata = new_game_min, allow.new.levels = TRUE)
  pred_minutes <- max(0, pred_minutes)
  minutes_s <- (pred_minutes - mean(player_data_local$minutes, na.rm = TRUE))/ sd(player_data_local$minutes, na.rm = TRUE)
  
  new_game <- data.frame(
    athlete_id = athlete_id_input,
    team_off_rating_s = team_off_s,
    team_def_rating_s = team_def_s,
    opp_off_rating_s = opp_off_s,
    opp_def_rating_s = opp_def_s,
    minutes = pred_minutes,
    minutes_s = minutes_s
  )
  
  
  # Predict per-minute stats
  pred_pts_per_min <- predict(model_pts_local, newdata = new_game, allow.new.levels = TRUE)
  pred_ast_per_min <- predict(model_ast_local, newdata = new_game, allow.new.levels = TRUE)
  pred_reb_per_min <- predict(model_reb_local, newdata = new_game, allow.new.levels = TRUE)
  
  # Convert to total statline
  pred_points <- pred_pts_per_min * pred_minutes
  pred_assists <- pred_ast_per_min * pred_minutes
  pred_rebounds <- pred_reb_per_min * pred_minutes
  
  # Print results
  cat("\nPredicted statline for", player_info$athlete_display_name, ":\n")
  cat("Team:", player_info$team_display_name, "\n") 
  cat("Opponent:", opponent_info$opponent_team_display_name, "\n") 
  cat("Predicted Minutes:", round(pred_minutes,1), "\n")
  cat("Points:", round(pred_points, 1), "\n")
  cat("Assists:", round(pred_assists, 1), "\n")
  cat("Rebounds:", round(pred_rebounds, 1), "\n")
}



# Now run the function
predict_player_stats_by_id()



predict_team_matchup <- function() {
  
  # --- Step 1: Get Team IDs from User ---
  home_team_id_input <- as.numeric(readline(prompt = "Enter Higher Seed Team ID: "))
  away_team_id_input <- as.numeric(readline(prompt = "Enter Away Team ID: "))
  
  # --- Step 2: Get Team Info and Rosters ---
  home_team_info <- team_eff %>% filter(team_id == home_team_id_input)
  if(nrow(home_team_info) == 0) stop("Home team_id not found")
  
  away_team_info <- team_eff %>% filter(team_id == away_team_id_input)
  if(nrow(away_team_info) == 0) stop("Away team_id not found")
  
  # Get the rosters for both teams (most recent game is a good proxy for the current roster)
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
  
  # --- Step 3: Function to Predict Stats for one Team ---
  predict_one_team <- function(team_info, opponent_info, roster) {
    
    # Scale team/opponent ratings
    team_off_s <- (team_info$off_rating - mean(player_data$team_off_rating, na.rm = TRUE)) / sd(player_data$team_off_rating, na.rm = TRUE)
    team_def_s <- (team_info$def_rating - mean(player_data$team_def_rating, na.rm = TRUE)) / sd(player_data$team_def_rating, na.rm = TRUE)
    opp_off_s  <- (opponent_info$off_rating - mean(player_data$opp_off_rating, na.rm = TRUE)) / sd(player_data$opp_off_rating, na.rm = TRUE)
    opp_def_s  <- (opponent_info$def_rating - mean(player_data$opp_def_rating, na.rm = TRUE)) / sd(player_data$opp_def_rating, na.rm = TRUE)
    
    # Dataframe for all players on the roster
    new_game_base <- data.frame(
      team_off_rating_s = team_off_s,
      team_def_rating_s = team_def_s,
      opp_off_rating_s = opp_off_s,
      opp_def_rating_s = opp_def_s
    )
    
    # Create prediction data for the entire roster
    roster_pred_data <- roster %>%
      bind_cols(new_game_base)
    
    # Predict UNCONSTRAINED minutes for each player
    unconstrained_minutes <- predict(model_min, newdata = roster_pred_data, allow.new.levels = TRUE)
    
    # Ensure minutes are not negative
    unconstrained_minutes[unconstrained_minutes < 0] <- 0
    
    # NORMALIZE minutes to sum to 200
    total_minutes_weight <- sum(unconstrained_minutes)
    if (total_minutes_weight == 0) { # Avoid division by zero if no one is predicted to play
      minute_proportions <- rep(0, length(unconstrained_minutes))
    } else {
      minute_proportions <- unconstrained_minutes / total_minutes_weight
    }
    
    pred_minutes <- minute_proportions * 200
    
    # Add predicted minutes to the prediction data
    roster_pred_data$minutes <- pred_minutes
    roster_pred_data$minutes_s <- (pred_minutes - mean(player_data$minutes, na.rm = TRUE)) / sd(player_data$minutes, na.rm = TRUE)
    
    # Predict per-minute stats with the new normalized minutes
    pred_pts_per_min <- predict(model_pts, newdata = roster_pred_data, allow.new.levels = TRUE)
    pred_ast_per_min <- predict(model_ast, newdata = roster_pred_data, allow.new.levels = TRUE)
    pred_reb_per_min <- predict(model_reb, newdata = roster_pred_data, allow.new.levels = TRUE)
    
    # Calculate final stat lines
    final_stats <- roster_pred_data %>%
      mutate(
        Points = pred_pts_per_min * minutes,
        Assists = pred_ast_per_min * minutes,
        Rebounds = pred_reb_per_min * minutes,
        Player = athlete_display_name,
        Minutes = minutes
      ) %>%
      select(Player, Minutes, Points, Rebounds, Assists)
    
    return(final_stats)
  }
  
  # --- Step 4: Generate Predictions for Both Teams ---
  home_box_score <- predict_one_team(home_team_info, away_team_info, home_roster)
  away_box_score <- predict_one_team(away_team_info, home_team_info, away_roster)
  
  # --- Step 5: Format and Display the Full Box Score ---
  
  # Function to format and print one team's box score
  print_box_score <- function(box_score_df, team_name) {
    cat("\n---", toupper(team_name), "---\n")
    print(
      box_score_df %>%
        mutate(across(where(is.numeric), ~round(.x, 1)))
    )
    
    # Calculate and print totals
    team_totals <- box_score_df %>%
      summarise(
        Player = "TOTAL",
        Minutes = sum(Minutes),
        Points = sum(Points),
        Rebounds = sum(Rebounds),
        Assists = sum(Assists)
      ) %>%
      mutate(across(where(is.numeric), ~round(.x, 1)))
    
    cat("\n")
    print(team_totals)
    cat("---------------------------\n")
  }
  
  print_box_score(home_box_score, home_team_info$team_display_name)
  print_box_score(away_box_score, away_team_info$team_display_name)
  
}

# Run the function to predict a game
predict_team_matchup()



