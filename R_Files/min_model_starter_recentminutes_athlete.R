library(dplyr)
library(lme4)
library(hoopR)
library(tidyr)
library(performance)
library(Metrics) 
library(ggplot2) 
library(zoo)     
#team stats are useless (see lmer_box)
#opponent team id as ME is singularity

# Load player-level box scores
player_data_raw <- hoopR::load_mbb_player_box(seasons = 2025) %>%
  mutate(
    game_date = as.Date(game_date)
  ) %>%
  filter(!is.na(minutes), minutes > 0) %>%
  filter(game_date < as.Date("2025-03-17")) %>%
  filter(!is.na(starter)) # Ensure 'starter' column has no NAs


player_data_model <- player_data_raw %>%
  arrange(athlete_id, game_date) %>%
  group_by(athlete_id) %>%
  mutate(
    # Get minutes from the single previous game
    lag_1_min = lag(minutes, 1),

    avg_min_last_3 = rollmean(lag(minutes, 1), k = 3, fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  
  # If avg_min_last_3 is NA (e.g., first 3 games), fall back to using the last 1 game's minutes
  mutate(
    recent_minutes = coalesce(avg_min_last_3, lag_1_min)
  ) %>%
  
  # Scale the new predictor
  mutate(recent_minutes_s = scale(recent_minutes)) %>%
  
  # Filter out rows where recent_minutes_s couldn't be calculated (e.g., a player's first game)
  filter(!is.na(recent_minutes_s))



# Define a control object to help models converge
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# Build the model for minutes

model_min <- lmer(
  minutes ~ starter + recent_minutes_s + (1 | athlete_id),
  data = player_data_model, 
  control = ctrl
)


# Print the model summary
print(summary(model_min))


# check_model(model_min)

# Plot residuals
res <- residuals(model_min)

# Q-Q plot for residuals
qqnorm(res)
qqline(res, col = "red")

# Q-Q plot for random effects
qqnorm(ranef(model_min)$athlete_id$`(Intercept)`)
qqline(ranef(model_min)$athlete_id$`(Intercept)`, col = "red")

# Histogram of residuals
hist(res, breaks = 50, main = "Histogram of Model Residuals", xlab = "Residuals")


# Get R-squared values
r2_metrics <- r2(model_min)

# Get actual and predicted values
actual_minutes <- getME(model_min, "y")
predicted_minutes <- fitted(model_min)

# Calculate error metrics
mae_min <- Metrics::mae(actual_minutes, predicted_minutes)
mape_min <- Metrics::mape(actual_minutes, predicted_minutes)
rmse_min <- Metrics::rmse(actual_minutes, predicted_minutes)

cat("\n--- model_min Performance Report (starter + recent_min) ---\n")
cat(sprintf("Conditional R2 (Fixed + Random): %.3f\n", r2_metrics$R2_conditional))
cat(sprintf("Marginal R2    (Fixed Effects Only): %.3f\n", r2_metrics$R2_marginal))
cat("----------------------------------------------------------\n")
cat(sprintf("MAE:              %.3f minutes\n", mae_min))
cat(sprintf("RMSE:             %.3f minutes\n", rmse_min))
cat(sprintf("MAPE:             %.3f%%\n", mape_min * 100))
cat("----------------------------------------------------------\n")

ggplot(player_data_model, aes(x = minutes)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", boundary = 0) +
  geom_density(color = "red", size = 1) +
  scale_x_continuous(limits = c(0, 48), breaks = seq(0, 48, 4)) +
  labs(
    title = "Distribution of Minutes Played (bin = 1 minute)",
    x = "Minutes",
    y = "Density"
  ) +
  theme_minimal()
#this plot shows that lmer gaussian assumption wont work 


ggplot(player_data_model, aes(x = minutes, fill = starter)) +
  geom_histogram(binwidth = 1, color = "black", position = "identity", alpha = 0.6) +
  scale_x_continuous(limits = c(0, 48), breaks = seq(0, 48, 4)) +
  labs(title = "Minutes Played by Starter Status", x = "Minutes", y = "Density") +
  theme_minimal()
#this plot shows the model trying to model starters and bench players, and how different they are
#use gamma distributions for right skewed problems
#make new model for two groups




