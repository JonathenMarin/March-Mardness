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
  filter(did_not_play == FALSE) %>% 
  filter(!is.na(starter))

train_data <- all_player_data %>%
  filter(game_date < as.Date("2025-03-17"))

test_data <- all_player_data %>%
  filter(game_date >= as.Date("2025-03-17"))

player_median_stats <- train_data %>%
  mutate(
    pts_per_min = ifelse(minutes > 0, points/minutes, 0 )
  ) %>% 
  group_by(athlete_id, athlete_display_name) %>%
  summarise(
    # Get the median minutes from the training set
    median_min_history = median(minutes, na.rm = TRUE),
    # Get the number of games played in the training set
    median_pts_per_min = median(pts_per_min), na.rm = TRUE,
    n_games_history = n(),
    .groups = 'drop'
  )


test_data_with_stats <- test_data %>%
  left_join(player_median_stats, by = c("athlete_id", "athlete_display_name")) %>% #<-- Minor fix
  mutate(
    n_games_history = ifelse(is.na(n_games_history), 0, n_games_history),
    median_min_history = ifelse(is.na(median_min_history), 0, median_min_history),
    median_pts_per_min = ifelse(is.na(median_pts_per_min), 0, median_pts_per_min),
    predicted_minutes = ifelse(n_games_history < 10, 0, median_min_history),
    predicted_points  = ifelse(n_games_history < 10, 0, median_min_history * median_pts_per_min)                            
  )


cat(sprintf("Test set: %d player-games (%d unique players)\n",
            nrow(test_data_with_stats),
            n_distinct(test_data_with_stats$athlete_id)))


calculate_r2 <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  return(r2)
}
#checking minutes
mae_all <- Metrics::mae(test_data_with_stats$minutes, test_data_with_stats$predicted_minutes)
rmse_all <- Metrics::rmse(test_data_with_stats$minutes, test_data_with_stats$predicted_minutes)
bias_all <- mean(test_data_with_stats$predicted_minutes - test_data_with_stats$minutes) 
r2_all <- calculate_r2(test_data_with_stats$minutes, test_data_with_stats$predicted_minutes)
test_starters <- subset(test_data_with_stats, starter == TRUE)
test_bench <- subset(test_data_with_stats, starter == FALSE)
r2_starters <- calculate_r2(test_starters$minutes, test_starters$predicted_minutes)
r2_bench <- calculate_r2(test_bench$minutes, test_bench$predicted_minutes)
cat("\n--- OUT-OF-SAMPLE TEST RESULTS (All Players, 0s for <10 games) ---\n")
cat(sprintf("MAE: %.3f  |  RMSE: %.3f  |  Bias: %.3f  |  R²: %.3f\n",
            mae_all, rmse_all, bias_all, r2_all))
cat(sprintf("Starter R²: %.3f  |  Bench R²: %.3f\n", r2_starters, r2_bench))
cat("---------------------------------------------------------------\n\n")

#checking pts
mae_pts  <- Metrics::mae(test_data_with_stats$points, test_data_with_stats$predicted_points)
rmse_pts <- Metrics::rmse(test_data_with_stats$points, test_data_with_stats$predicted_points)
r2_pts   <- 1 - sum((test_data_with_stats$points - test_data_with_stats$predicted_points)^2) /
                sum((test_data_with_stats$points - mean(test_data_with_stats$points))^2)

cat(sprintf("MAE: %.3f  |  RMSE: %.3f  |  R²: %.3f\n", mae_pts, rmse_pts, r2_pts))


#checking team

team_error <- test_data_with_stats %>%
  group_by(team_id, game_id, team_display_name) %>%
  summarise(
    # Minutes totals
    actual_total     = sum(minutes, na.rm = TRUE),
    pred_total       = sum(predicted_minutes, na.rm = TRUE),
    abs_error        = abs(pred_total - actual_total),
    
    # Points totals
    actual_total_pts = sum(points, na.rm = TRUE),
    pred_total_pts   = sum(predicted_points, na.rm = TRUE),
    abs_error_pts    = abs(pred_total_pts - actual_total_pts),
    
    .groups = "drop"
  )

cat(sprintf("Average team total error:\n"))
cat(sprintf("  Minutes: %.2f minutes\n", mean(team_error$abs_error, na.rm = TRUE)))
cat(sprintf("  Points:  %.2f points\n\n", mean(team_error$abs_error_pts, na.rm = TRUE)))



# --- Plots ---
print(
ggplot(test_starters, aes(x = minutes, y = predicted_minutes)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "(Starters): predicted vs Actual",
         subtitle = sprintf("Out-of-Sample R² = %.3f", r2_starters),
         x = "Actual Minutes", y = "Predicted Minutes") +
    theme_minimal()
)


ggplot(test_bench, aes(x = minutes, y = predicted_minutes)) +
    geom_point(alpha = 0.5, color = "lightblue") +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    labs(title = "(Bench): predicted vs Actual",
         subtitle = sprintf("Out-of-Sample R² = %.3f", r2_bench),
         x = "Actual Minutes", y = "Predicted Minutes") +
    theme_minimal()

ggplot(test_bench, aes(x = points, y = predicted_points)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(alpha = 1.0,slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "TEST SET (Starters): predicted vs Actual",
       subtitle = sprintf("Out-of-Sample R² = %.3f", r2_bench),
       x = "Actual Points", y = "Predicted Points") +
  theme_minimal()

ggplot(test_bench, aes(x = points, y = predicted_points)) +
  geom_point(alpha = 0.5, color = "lightblue") +
  geom_abline(alpha = 1.0,slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "TEST SET (Bench): predicted vs Actual",
       subtitle = sprintf("Out-of-Sample R² = %.3f", r2_bench),
       x = "Actual Points", y = "Predicted Points") +
  theme_minimal()




predict_points_matchup <- function(team1_query, team2_query, med_table) {
  
  # ---- 1) Build a clean team index (once per session) ----
  team_index <- all_player_data %>%
    distinct(team_id, team_display_name, team_short_display_name) %>%
    mutate(across(c(team_display_name, team_short_display_name), ~trimws(.)))
  
  # helper to resolve a team query to a single team_id
  resolve_team <- function(q) {
    q_clean <- trimws(q)
    hits <- team_index %>%
      filter(
        grepl(q_clean, team_display_name, ignore.case = TRUE) |
          grepl(q_clean, team_short_display_name, ignore.case = TRUE)
      )
    
    if (nrow(hits) == 0) {
      stop(paste0("No team matched '", q, "'. Try one of: ",
                  paste(unique(team_index$team_display_name)[1:10], collapse = "; "), " ..."))
    }
    if (nrow(hits) > 1) {
      message("Multiple matches for '", q, "'. Using first: ",
              hits$team_display_name[1], " (team_id=", hits$team_id[1], ")")
    }
    hits[1, c("team_id","team_display_name")]
  }
  
  t1 <- resolve_team(team1_query)
  t2 <- resolve_team(team2_query)
  
  # ---- 2) Build rosters by team_id from all_player_data ----
  get_roster_by_id <- function(team_id_val) {
    all_player_data %>%
      filter(team_id == team_id_val) %>%
      distinct(athlete_id, athlete_display_name, team_id, team_display_name)
  }
  
  roster1 <- get_roster_by_id(t1$team_id)
  roster2 <- get_roster_by_id(t2$team_id)
  
  # Diagnostics: if these are 0, you’d see an empty View
  message("Resolved: ", t1$team_display_name, " (n=", nrow(roster1), " players)")
  message("Resolved: ", t2$team_display_name, " (n=", nrow(roster2), " players)")
  
  if (nrow(roster1) == 0 || nrow(roster2) == 0) {
    stop("One roster is empty. Check team names and that all_player_data contains those teams.")
  }
  
  roster_df <- bind_rows(roster1, roster2)
  
  # ---- 3) Join medians, handle NAs, normalize to 200 per team ----
  matchup_df <- roster_df %>%
    left_join(med_table, by = "athlete_id") %>%
    mutate(
      median_min_history = ifelse(is.na(median_min_history), 0, median_min_history),
      n_games_history    = ifelse(is.na(n_games_history), 0, n_games_history)
    ) %>%
    group_by(team_id, team_display_name) %>%
    mutate(
      projected_pts = median_min_history * median_pts_per_min,
      team_total = sum(median_min_history, na.rm = TRUE),
      #min_share  = ifelse(team_total > 0, median_min_history / team_total, 0),
      #projected_min = 200 * min_share
    ) %>%
    ungroup() %>%
    arrange(team_display_name, desc(projected_pts)) %>% 
    select(-c(1,3,8))
  
  team_sums <- matchup_df %>%
    group_by(team_display_name) %>%
    summarise(total_projected = sum(projected_pts), .groups = "drop")
  
  list(matchup_table = matchup_df, team_sums = team_sums)
}




matchup <- predict_points_matchup("Florida", "Houston", player_median_stats)
matchup2 <- predict_points_matchup("Duke", "Houston", player_median_stats) 
matchup3 <- predict_points_matchup("Auburn", "Florida", player_median_stats)
matchup4 <- predict_points_matchup("Duke", "Alabama", player_median_stats)
print(matchup$matchup_table, n = 28)
print(matchup$team_sums)

print(matchup2$matchup_table, n = 28)
print(matchup2$team_sums)

print(matchup3$matchup_table, n = 28)
print(matchup3$team_sums)

print(matchup4$team_sums)
