library(hoopR)
library(dplyr)
library(lme4)
library(wehoop)

ncaa_mbb <- hoopR::load_mbb_player_box(seasons = 2025)
ncaa_wbb <- wehoop::load_wbb_player_box(seasons = 2025)

filter_and_fix_minutes <- function(df) {
  df %>%
    mutate(game_date = as.Date(game_date),
           minutes = ifelse(minutes == 0, 1, minutes)) %>%
    filter(game_date > as.Date("2024-11-04") & game_date < as.Date("2025-03-17"))
}

ncaa_mbb <- filter_and_fix_minutes(ncaa_mbb)
ncaa_wbb <- filter_and_fix_minutes(ncaa_wbb)

calc_per_min <- function(df) {
  df %>%
    mutate(
      pts_per_min = points / minutes,
      ast_per_min = assists / minutes,
      reb_per_min = rebounds / minutes,
      tov_per_min = turnovers / minutes,
      fg_per_min = field_goals_made / minutes,
      fga_per_min = field_goals_attempted / minutes,
      ft_per_min = free_throws_made / minutes
    )
}

ncaa_mbb_pm <- calc_per_min(ncaa_mbb)
ncaa_wbb_pm <- calc_per_min(ncaa_wbb)

calc_expected <- function(df) {
  df %>%
    group_by(athlete_id, athlete_display_name, team_id, team_name) %>%
    summarise(
      games_played = n(),
      avg_minutes = mean(minutes, na.rm = TRUE),
      pts_per_min = mean(pts_per_min, na.rm = TRUE),
      ast_per_min = mean(ast_per_min, na.rm = TRUE),
      reb_per_min = mean(reb_per_min, na.rm = TRUE),
      tov_per_min = mean(tov_per_min, na.rm = TRUE),
      fg_per_min = mean(fg_per_min, na.rm = TRUE),
      fga_per_min = mean(fga_per_min, na.rm = TRUE),
      ft_per_min = mean(ft_per_min, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(avg_minutes > 5, games_played >= 5) %>%
    mutate(
      expected_pts = pts_per_min * avg_minutes,
      expected_ast = ast_per_min * avg_minutes,
      expected_reb = reb_per_min * avg_minutes,
      expected_tov = tov_per_min * avg_minutes,
      expected_fg = fg_per_min * avg_minutes,
      expected_fga = fga_per_min * avg_minutes,
      expected_ft = ft_per_min * avg_minutes
    )
}

ncaa_mbb_expected <- calc_expected(ncaa_mbb_pm)
ncaa_wbb_expected <- calc_expected(ncaa_wbb_pm)

ncaa_combined <- bind_rows(
  ncaa_mbb_expected %>% mutate(league = "MBB"),
  ncaa_wbb_expected %>% mutate(league = "WBB")
)

fit_lmer <- function(df, stat_name) {
  formula <- as.formula(paste0(stat_name, " / minutes ~ 1 + (1 | athlete_id) + (1 | team_id)"))
  lmer(formula, data = df, REML = FALSE)
}

model_pts <- fit_lmer(bind_rows(ncaa_mbb, ncaa_wbb), "points")
model_ast <- fit_lmer(bind_rows(ncaa_mbb, ncaa_wbb), "assists")
model_reb <- fit_lmer(bind_rows(ncaa_mbb, ncaa_wbb), "rebounds")
model_tov <- fit_lmer(bind_rows(ncaa_mbb, ncaa_wbb), "turnovers")
model_fg  <- fit_lmer(bind_rows(ncaa_mbb, ncaa_wbb), "field_goals_made")
model_fga <- fit_lmer(bind_rows(ncaa_mbb, ncaa_wbb), "field_goals_attempted")
model_ft  <- fit_lmer(bind_rows(ncaa_mbb, ncaa_wbb), "free_throws_made")

predict_expected <- function(df, models) {
  df %>%
    group_by(athlete_id, athlete_display_name, team_id, team_name) %>%
    summarise(
      avg_minutes = mean(minutes),
      games_played = n(),
      expected_pts = mean(predict(models$pts, newdata = ., allow.new.levels = TRUE) * minutes),
      expected_ast = mean(predict(models$ast, newdata = ., allow.new.levels = TRUE) * minutes),
      expected_reb = mean(predict(models$reb, newdata = ., allow.new.levels = TRUE) * minutes),
      expected_tov = mean(predict(models$tov, newdata = ., allow.new.levels = TRUE) * minutes),
      expected_fg  = mean(predict(models$fg, newdata = ., allow.new.levels = TRUE) * minutes),
      expected_fga = mean(predict(models$fga, newdata = ., allow.new.levels = TRUE) * minutes),
      expected_ft  = mean(predict(models$ft, newdata = ., allow.new.levels = TRUE) * minutes),
      .groups = "drop"
    ) %>%
    filter(avg_minutes > 5, games_played >= 5)
}

ncaa_combined_expected <- predict_expected(
  bind_rows(ncaa_mbb, ncaa_wbb),
  list(
    pts = model_pts,
    ast = model_ast,
    reb = model_reb,
    tov = model_tov,
    fg  = model_fg,
    fga = model_fga,
    ft  = model_ft
  )
)
