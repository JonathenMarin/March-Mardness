library(hoopR)
library(dplyr)
library(lme4)

pbp <- hoopR::load_mbb_player_box(2025)
pbp <- pbp %>% filter(game_date > as.Date("2024-11-04") & game_date < as.Date("2025-03-17"))
pbp <- pbp[,-c(3,5,10,32,33,34,35,37,38,39,40,41,42,43)]

team_game_stats <- pbp %>%
  group_by(game_id, team_id, team_name, opponent_team_name) %>%
  summarise(
    # Sum the stats needed for the possession formula
    fga = sum(field_goals_attempted, na.rm = TRUE),
    fta = sum(free_throws_attempted, na.rm = TRUE),
    oreb = sum(offensive_rebounds, na.rm = TRUE),
    turnovers = sum(turnovers, na.rm = TRUE),
    score = first(team_score),
    opponent_score = first(opponent_team_score),
    location = first(home_away),
    .groups = 'drop'
  ) %>% mutate(
    possessions = fga - oreb + turnovers + (0.44 * fta)
  )


game_level_data <- team_game_stats %>%
  # Ensure teams within a game are always in the same order (e.g., by team_id)
  arrange(game_id, team_id) %>%
  group_by(game_id) %>%
  summarise(
    # Team 1 info
    team1_name = first(team_name),
    team1_score = first(score),
    team1_poss = first(possessions),
    team1_loc = first(location),
    
    # Team 2 info
    team2_name = last(team_name),
    team2_score = last(score),
    team2_poss = last(possessions)
  ) %>%
  # Make sure we only have games with two teams (removes corrupted data)
  filter(!is.na(team2_name))

model_prep_data <- game_level_data %>%
  mutate(
    # 1. Calculate the average pace for the game
    pace = (team1_poss + team2_poss) / 2,
    
    # 2. Calculate point differential and efficiency for Team 1
    team1_pt_diff = team1_score - team2_score,
    team1_off_eff = (team1_score / pace) * 100,
    team1_def_eff = (team2_score / pace) * 100,
    
    # 3. Calculate point differential and efficiency for Team 2
    team2_pt_diff = team2_score - team1_score,
    team2_off_eff = (team2_score / pace) * 100,
    team2_def_eff = (team1_score / pace) * 100
  )

# Finally, stack the data to create the two-rows-per-game format
team1_perspective <- model_prep_data %>%
  select(
    team_name = team1_name,
    opponent_name = team2_name,
    location = team1_loc,
    point_diff = team1_pt_diff,
    off_eff = team1_off_eff,
    def_eff = team1_def_eff,
    pace
  )

team2_perspective <- model_prep_data %>%
  mutate(
    team2_loc = case_when(
      team1_loc == "home" ~ "away",
      team1_loc == "away" ~ "home",
      TRUE ~ team1_loc  # handles "neutral"
    )
  ) %>%
  select(
    team_name = team2_name,
    opponent_name = team1_name,
    location = team2_loc,
    point_diff = team2_pt_diff,
    off_eff = team2_off_eff,
    def_eff = team2_def_eff,
    pace
  )

final_data <- bind_rows(team1_perspective, team2_perspective) %>%
  # Convert location to a factor for modeling
  mutate(location = factor(location, levels = c("home", "away", "neutral")))


# Let's view the final structure
print(head(final_data))    


me_ptdiff_model <- lmer(point_diff ~ location + (1 | team_name) + (1 | opponent_name), data = final_data)
summary(me_ptdiff_model)

ratings <- ranef(me_ptdiff_model)$team_name

lme4_ratings <- data.frame(
  team_name = rownames(ratings),
  rating = ratings$`(Intercept)`
) %>%
  arrange(desc(rating))

ratings_with_state <- lme4_ratings %>%
  
