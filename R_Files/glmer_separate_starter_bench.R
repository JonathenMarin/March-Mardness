library(dplyr)
library(lme4)
library(hoopR)
library(tidyr)
library(zoo)
library(Metrics)
library(performance)
library(ggplot2)


# Load all player box scores for the season
all_player_data <- hoopR::load_mbb_player_box(seasons = 2025) %>%
  mutate(game_date = as.Date(game_date)) %>%
  filter(!is.na(minutes)) %>%
  filter(!is.na(starter))


all_data_features <- all_player_data %>%
  arrange(athlete_id, game_date) %>%
  group_by(athlete_id) %>%
  mutate(
    lag_1_min = lag(minutes, 1),
    avg_min_last_3 = rollmean(lag(minutes, 1), k = 3, fill = NA, align = "right"),
    recent_minutes = coalesce(avg_min_last_3, lag_1_min)
  ) %>%
  ungroup()

# Calculate the scaling values (mean/sd) from the *training data only*
train_stats <- all_data_features %>%
  filter(game_date < as.Date("2025-03-17")) %>%
  summarise(
    mean_recent_min = mean(recent_minutes, na.rm = TRUE),
    sd_recent_min = sd(recent_minutes, na.rm = TRUE)
  )

# Apply the training stats to scale the whole dataset
all_data_scaled <- all_data_features %>%
  mutate(
    recent_minutes_s = (recent_minutes - train_stats$mean_recent_min) / train_stats$sd_recent_min
  )


train_data <- all_data_scaled %>%
  filter(game_date < as.Date("2025-03-17")) %>%
  filter(!is.na(recent_minutes_s))

test_data <- all_data_scaled %>%
  filter(game_date >= as.Date("2025-03-17")) %>%
  filter(!is.na(recent_minutes_s))

cat(sprintf("Training on %d player-games, Testing on %d player-games.\n\n",
            nrow(train_data), nrow(test_data)))


ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))


model_starter <- glmer.nb(
  minutes ~ recent_minutes_s + (1 | athlete_id),
  data = subset(train_data, starter == TRUE),
  control = ctrl
)

# Model for bench players, trained ONLY on train_data
model_bench <- glmer.nb(
  minutes ~ recent_minutes_s + (1 | athlete_id),
  data = subset(train_data, starter == FALSE),
  control = ctrl
)

test_starters <- subset(test_data, starter == TRUE)
test_bench <- subset(test_data, starter == FALSE)

# We MUST use allow.new.levels = TRUE
# This tells lmer to ignore the random effects for game_id and any new athlete_id
pred_starter <- predict(model_starter,
                        newdata = test_starters,
                        allow.new.levels = TRUE,
                        type = "response")

pred_bench <- predict(model_bench,
                      newdata = test_bench,
                      allow.new.levels = TRUE,
                      type = "response")

# Combine predictions back into the test_data dataframe
test_data$predicted <- NA
test_data$predicted[test_data$starter == TRUE] <- pred_starter
test_data$predicted[test_data$starter == FALSE] <- pred_bench

calculate_r2 <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  return(r2)
}

# Calculate the "true" error of the model on unseen data
mae_all <- Metrics::mae(test_data$minutes, test_data$predicted)
rmse_all <- Metrics::rmse(test_data$minutes, test_data$predicted)
r2_all <- calculate_r2(test_data$minutes, test_data$predicted)
bias_all <- mean(test_data$minutes - test_data$predicted)


r2_starters <- calculate_r2(test_starters$minutes, pred_starter)
r2_bench <- calculate_r2(test_bench$minutes, pred_bench)

condr2_starter <- r2_nakagawa(model_starter)
condr2_bench <- r2_nakagawa(model_bench)
AIC_starter <- AIC(model_starter)
BIC_starter <- BIC(model_starter)



cat(sprintf("MAE: %.3f  |  RMSE: %.3f  |  Bias: %.3f  |  R²: %.3f\n",
            mae_all, rmse_all, bias_all, r2_all))
cat(sprintf("Starter R²: %.3f  |  Bench R²: %.3f\n", condr2_starter, condr2_bench)) #second line is fixed effects only 





print(
  ggplot(subset(test_data, starter == TRUE),
         aes(x = minutes, y = predicted)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "TEST SET (Starters): Predicted vs Actual",
         x = "Actual Minutes", y = "Predicted Minutes") +
    theme_minimal()
)

print(
  ggplot(subset(test_data, starter == FALSE),
         aes(x = minutes, y = predicted)) +
    geom_point(alpha = 0.5, color = "lightblue") +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    labs(title = "TEST SET (Bench): Predicted vs Actual",
         x = "Actual Minutes", y = "Predicted Minutes") +
    theme_minimal()
)

player_predictions <- test_data %>%
  filter(athlete_display_name == "Cooper Flagg") %>%
  select(
    game_date, 
    athlete_display_name, 
    starter, 
    minutes,   # The actual minutes they played
    predicted  
  ) %>%
  arrange(game_date)


