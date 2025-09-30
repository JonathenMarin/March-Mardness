library(hoopR)
library(wehoop)
library(dplyr)
library(lme4)


ncaa_player <- hoopR::load_mbb_player_box(seasons = 2025)
wnba_player <- wehoop::load_wnba_player_box(season = 2024)


ncaa_player <- ncaa_player %>% filter(game_date => ("2024-11-04") & game_date < ("2025-03-17"))
wnba_player <- wnba_player %>% filter(game_date > ("2024-11-04") & game_date < ("2025-03-17"))

remove_zero_minutes <- function(df) {
  df %>% mutate(minutes = ifelse(minutes == 0, 1, minutes))
}

ncaa_player <- remove_zero_minutes(ncaa_player)
wnba_player <- remove_zero_minutes(wnba_player)
# nba_player <- remove_zero_minutes(nba_player)


keep_two_teams <- function(df) {
  df %>%
    group_by(game_id) %>%
    filter(n_distinct(team_id) == 2) %>%
    ungroup()
}

ncaa_player <- keep_two_teams(ncaa_player)
wnba_player <- keep_two_teams(wnba_player)
# nba_player <- keep_two_teams(nba_player)


add_opponent <- function(df) {
  df %>%
    group_by(game_id) %>%
    mutate(opponent_id = ifelse(team_id == unique(team_id)[1],
                                unique(team_id)[2],
                                unique(team_id)[1])) %>%
    ungroup()
}

ncaa_player <- add_opponent(ncaa_player)
wnba_player <- add_opponent(wnba_player)
# nba_player <- add_opponent(nba_player)


fit_lmer_safe <- function(df, stat_name) {
  formula <- as.formula(paste0(stat_name, " / minutes ~ 1 + (1 | athlete_id) + (1 | team_id) + (1 | opponent_id || 1)"))
  lmer(formula, data = df, REML = FALSE)
}

# Fit models for each stat
fit_models <- function(df) {
  list(
    pts = fit_lmer_safe(df, "points"),
    ast = fit_lmer_safe(df, "assists"),
    reb = fit_lmer_safe(df, "rebounds"),
    tov = fit_lmer_safe(df, "turnovers"),
    fg  = fit_lmer_safe(df, "field_goals_made"),
    fga = fit_lmer_safe(df, "field_goals_attempted"),
    ft  = fit_lmer_safe(df, "free_throws_made")
  )
}

ncaa_models <- fit_models(ncaa_player)
wnba_models <- fit_models(wnba_player)
# nba_models <- fit_models(nba_player)


predict_expected_totals <- function(df, models) {
  df <- df %>%
    mutate(
      pred_pts = predict(models$pts, newdata = ., allow.new.levels = TRUE) * minutes,
      pred_ast = predict(models$ast, newdata = ., allow.new.levels = TRUE) * minutes,
      pred_reb = predict(models$reb, newdata = ., allow.new.levels = TRUE) * minutes,
      pred_tov = predict(models$tov, newdata = ., allow.new.levels = TRUE) * minutes,
      pred_fg  = predict(models$fg,  newdata = ., allow.new.levels = TRUE) * minutes,
      pred_fga = predict(models$fga, newdata = ., allow.new.levels = TRUE) * minutes,
      pred_ft  = predict(models$ft,  newdata = ., allow.new.levels = TRUE) * minutes
    )
  
  df %>%
    group_by(athlete_id, athlete_display_name, team_id, team_name) %>%
    summarise(
      avg_minutes = mean(minutes),
      games_played = n(),
      expected_pts = mean(pred_pts),
      expected_ast = mean(pred_ast),
      expected_reb = mean(pred_reb),
      expected_tov = mean(pred_tov),
      expected_fg = mean(pred_fg),
      expected_fga = mean(pred_fga),
      expected_ft = mean(pred_ft),
      .groups = "drop"
    ) %>%
    filter(avg_minutes > 5, games_played >= 5)
}

ncaa_expected <- predict_expected_totals(ncaa_player, ncaa_models)
wnba_expected <- predict_expected_totals(wnba_player, wnba_models)

