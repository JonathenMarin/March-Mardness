library(dplyr)
library(lme4)
library(hoopR)
library(tidyr)
library(zoo)
library(Metrics)
library(performance)
library(ggplot2)

#possible elo idea https://kenpom.com/index.php?y=2025
#can use elo for team level strength
#can add random slope to athlete_id with recent_minutes as well 
#b2b and fatigue features

#added random effet of game_id and it got down a little. 
player_data_raw <- hoopR::load_mbb_player_box(seasons = 2025) %>%
  mutate(game_date = as.Date(game_date)) %>%
  filter(!is.na(minutes), minutes > 0) %>%
  filter(game_date < as.Date("2025-03-17")) %>%
  filter(!is.na(starter))

player_data_model <- player_data_raw %>%
  arrange(athlete_id, game_date) %>%
  group_by(athlete_id) %>%
  mutate(
    lag_1_min = lag(minutes, 1),
    avg_min_last_3 = rollmean(lag(minutes, 1), k = 3, fill = NA, align = "right"),
    recent_minutes = coalesce(avg_min_last_3, lag_1_min)
  ) %>%
  ungroup() %>%
  mutate(
    recent_minutes_s = scale(recent_minutes)
  ) %>%
  filter(!is.na(recent_minutes_s))



ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))


model_starter <- lmer(minutes ~ recent_minutes_s + (1 | athlete_id) + (1 | game_id),
                      data = subset(player_data_model, starter == TRUE),
                      control = ctrl)


model_bench <- lmer(minutes ~ recent_minutes_s + (1 | athlete_id) + (1 | game_id),
                    data = subset(player_data_model, starter == FALSE),
                    control = ctrl)

pred_starter <- predict(model_starter,
                        newdata = subset(player_data_model, starter == TRUE),
                        allow.new.levels = TRUE)
pred_bench <- predict(model_bench,
                      newdata = subset(player_data_model, starter == FALSE),
                      allow.new.levels = TRUE)

player_data_model$predicted <- NA
player_data_model$predicted[player_data_model$starter == TRUE] <- pred_starter
player_data_model$predicted[player_data_model$starter == FALSE] <- pred_bench

#results
mae_all <- mae(player_data_model$minutes, player_data_model$predicted)
rmse_all <- rmse(player_data_model$minutes, player_data_model$predicted)

r2_starter <- r2(model_starter)$R2_conditional
r2_bench <- r2(model_bench)$R2_conditional


cat(sprintf("Overall MAE:  %.3f minutes\n", mae_all))
cat(sprintf("Overall RMSE: %.3f minutes\n", rmse_all))

cat(sprintf("Starter Model  R²: %.3f | n = %d\n",
            r2_starter, nrow(subset(player_data_model, starter == TRUE))))
cat(sprintf("Bench Model    R²: %.3f | n = %d\n",
            r2_bench, nrow(subset(player_data_model, starter == FALSE))))




# --- Predicted vs Actual (Starters) ---
ggplot(subset(player_data_model, starter == TRUE),
       aes(x = minutes, y = predicted)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Starters: Predicted vs Actual Minutes",
       x = "Actual Minutes", y = "Predicted Minutes") +
  theme_minimal()

# --- Predicted vs Actual (Bench) ---
ggplot(subset(player_data_model, starter == FALSE),
       aes(x = minutes, y = predicted)) +
  geom_point(alpha = 0.4, color = "lightblue") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Bench Players: Predicted vs Actual Minutes",
       x = "Actual Minutes", y = "Predicted Minutes") +
  theme_minimal()

#doublechecking the comparison
# Model without the game random effect
model_no_game <- lmer(
  minutes ~ recent_minutes_s + (1 | athlete_id),
  data = subset(player_data_model, starter == TRUE),
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Model with the game random effect
model_with_game <- lmer(
  minutes ~ recent_minutes_s + (1 | athlete_id) + (1 | game_id),
  data = subset(player_data_model, starter == TRUE),
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Compare them
anova(model_no_game, model_with_game)
