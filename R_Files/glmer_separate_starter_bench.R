library(dplyr)
library(lme4)
library(hoopR)
library(tidyr)
library(zoo)
library(Metrics)
library(performance)
library(ggplot2)

# --- 1. Load and Prepare ALL Data ---
# Load all player box scores for the season
all_player_data <- hoopR::load_mbb_player_box(seasons = 2025) %>%
  mutate(game_date = as.Date(game_date)) %>%
  filter(!is.na(minutes), minutes > 0) %>%
  filter(!is.na(starter))

# Calculate features across the *entire* dataset to prevent data leakage
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
# This is critical to prevent the test data from influencing the training data
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

# --- 2. Split into Training and Testing Sets ---
train_data <- all_data_scaled %>%
  filter(game_date < as.Date("2025-03-17")) %>%
  filter(!is.na(recent_minutes_s))

test_data <- all_data_scaled %>%
  filter(game_date >= as.Date("2025-03-17")) %>%
  filter(!is.na(recent_minutes_s))

cat(sprintf("Training on %d player-games, Testing on %d player-games.\n\n",
            nrow(train_data), nrow(test_data)))

# --- 3. Train Models on TRAINING Data Only ---
cat("Training models...\n")
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# Model for starters, trained ONLY on train_data
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
cat("Models trained.\n\n")

# --- 4. Generate Predictions on TESTING Data ---
# Create prediction dataframes from the test set
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



# --- 5. Evaluate Out-of-Sample Performance ---
# Calculate the "true" error of the model on unseen data
mae_all <- Metrics::mae(test_data$minutes, test_data$predicted)
rmse_all <- Metrics::rmse(test_data$minutes, test_data$predicted)
r2_all <- calculate_r2(test_data$minutes, test_data$predicted)

r2_starters <- calculate_r2(test_starters$minutes, pred_starter)
r2_bench <- calculate_r2(test_bench$minutes, pred_bench)

cat("--- OUT-OF-SAMPLE TEST RESULTS ---\n")
cat(sprintf("Overall MAE:  %.3f minutes\n", mae_all))
cat(sprintf("Overall RMSE: %.3f minutes\n\n", rmse_all))
cat(sprintf("Overall RMSE: %.3f minutes\n", rmse_all))
cat(sprintf("Overall R²:   %.3f\n\n", r2_all))
cat(sprintf("Starter-Only R²: %.3f\n", r2_starters))
cat(sprintf("Bench-Only R²:   %.3f\n", r2_bench))

# --- 6. Visualize Test Results ---
# --- Predicted vs Actual (Starters) ---
print(
  ggplot(subset(test_data, starter == TRUE),
         aes(x = minutes, y = predicted)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "TEST SET (Starters): Predicted vs Actual",
         x = "Actual Minutes", y = "Predicted Minutes") +
    theme_minimal()
)

# --- Predicted vs Actual (Bench) ---
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
    predicted  # Your glmer.nb model's prediction
  ) %>%
  arrange(game_date)



