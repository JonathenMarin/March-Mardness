# Model Comparison: Gaussian LMER vs Separate LMER vs Gamma GLMM
#gamma because min_model_start_recentminutes minutes show right-skew. use gamma. 
library(dplyr)
library(lme4)
library(glmmTMB)
library(hoopR)
library(tidyr)
library(zoo)
library(Metrics)
library(performance)
library(ggplot2)

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

#Baseline Gaussian LMER
model_lmer <- lmer(minutes ~ starter + recent_minutes_s + (1 | athlete_id),
                   data = player_data_model, control = ctrl)
pred_lmer <- fitted(model_lmer)
mae_lmer <- mae(player_data_model$minutes, pred_lmer)
rmse_lmer <- rmse(player_data_model$minutes, pred_lmer)
aic_lmer <- AIC(model_lmer)
r2_lmer <- r2(model_lmer)$R2_conditional


#Separate Models (Starters vs Non-Starters)
model_starter <- lmer(minutes ~ recent_minutes_s + (1 | athlete_id),
                      data = subset(player_data_model, starter == TRUE),
                      control = ctrl)
model_bench <- lmer(minutes ~ recent_minutes_s + (1 | athlete_id),
                    data = subset(player_data_model, starter == FALSE),
                    control = ctrl)

pred_starter <- predict(model_starter, newdata = subset(player_data_model, starter == TRUE))
pred_bench <- predict(model_bench, newdata = subset(player_data_model, starter == FALSE))

pred_split <- numeric(nrow(player_data_model))
pred_split[player_data_model$starter == TRUE] <- pred_starter
pred_split[player_data_model$starter == FALSE] <- pred_bench

mae_split <- mae(player_data_model$minutes, pred_split)
rmse_split <- rmse(player_data_model$minutes, pred_split)
aic_split <- AIC(model_starter) + AIC(model_bench)


#gamma GLMM
model_gamma <- glmmTMB(
  minutes ~ starter + recent_minutes_s + (1 | athlete_id),
  data = player_data_model,
  family = Gamma(link = "log")
)
pred_gamma <- predict(model_gamma, type = "response")
mae_gamma <- mae(player_data_model$minutes, pred_gamma)
rmse_gamma <- rmse(player_data_model$minutes, pred_gamma)
aic_gamma <- AIC(model_gamma)
r2_gamma <- r2(model_gamma)$R2_conditional


results <- data.frame(
  Model = c("Gaussian LMER", "Separate LMER (by Starter)", "Gamma GLMM"),
  MAE = round(c(mae_lmer, mae_split, mae_gamma), 3),
  RMSE = round(c(rmse_lmer, rmse_split, rmse_gamma), 3),
  AIC = round(c(aic_lmer, aic_split, aic_gamma), 1),
  R2_Conditional = round(c(r2_lmer, NA, r2_gamma), 3)
)


print(results)

#seperate lmer by starter seems best. 

player_data_model$pred_gamma <- pred_gamma
ggplot(player_data_model, aes(x = minutes, y = pred_gamma)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Gamma GLMM: Predicted vs Actual Minutes",
    x = "Actual Minutes",
    y = "Predicted Minutes"
  ) +
  theme_minimal()
#gammaglm actual vs predicted has issue of 0 minutes but predicted too high, and low prediction but high minutes.


#--------------------
r2_starter <- r2(model_starter)$R2_conditional
r2_bench   <- r2(model_bench)$R2_conditional

# Sample sizes
n_starter <- nrow(subset(player_data_model, starter == TRUE))
n_bench   <- nrow(subset(player_data_model, starter == FALSE))

# Weighted R² (by sample size)
weighted_r2_split <- (r2_starter * n_starter + r2_bench * n_bench) / (n_starter + n_bench)

# Add to your comparison results
comparison_df <- data.frame(
  Model = c("Gaussian LMER",
            "Separate LMER (weighted)",
            "Gamma GLMM"),
  MAE = round(c(mae_lmer, mae_split, mae_gamma), 3),
  RMSE = round(c(rmse_lmer, rmse_split, rmse_gamma), 3),
  AIC = round(c(aic_lmer, aic_split, aic_gamma), 1),
  R2_Conditional = round(c(r2_lmer, weighted_r2_split, r2_gamma), 3)
)

print(comparison_df)

ggplot(comparison_df, aes(x = R2_Conditional, y = MAE, label = Model)) +
  geom_point(size = 4, color = "steelblue") +
  geom_text(vjust = -0.7, hjust = 0.5) +
  labs(title = "Trade-off: R² vs MAE",
       x = "Conditional R² (Explained Variance)",
       y = "MAE (Prediction Error, minutes)") +
  theme_minimal()
