library(dplyr)
library(performance)
library(see)
library(pROC)
library(data.table)
library(corrplot)
#changes
#functions
calculate_four_factors <- function(detailed_results_df) {
  
  game_factors <- detailed_results_df %>%
    mutate(
      W_Poss = (WFGA - WOR) + WTO + (0.44 * WFTA),
      L_Poss = (LFGA - LOR) + LTO + (0.44 * LFTA),
      W_eFG = (WFGM + 0.5 * WFGM3) / WFGA,
      W_TOV = WTO / W_Poss,
      W_ORB = WOR / (WOR + LDR),
      W_FTR = WFTM / WFGA,
      L_eFG = (LFGM + 0.5 * LFGM3) / LFGA,
      L_TOV = LTO / L_Poss,
      L_ORB = LOR / (LOR + WDR),
      L_FTR = LFTM / LFGA
    )
  
  winners_factors <- game_factors %>%
    select(Season, TeamID = WTeamID, eFG = W_eFG, TOV_Pct = W_TOV, ORB_Pct = W_ORB, FTR = W_FTR) %>%
    mutate(Win = 1)
  
  losers_factors <- game_factors %>%
    select(Season, TeamID = LTeamID, eFG = L_eFG, TOV_Pct = L_TOV, ORB_Pct = L_ORB, FTR = L_FTR) %>%
    mutate(Win = 0)
  
  four_factors_df <- bind_rows(winners_factors, losers_factors) %>%
    arrange(Season, TeamID) %>%
    group_by(Season, TeamID) %>%
    summarise(
      across(where(is.numeric), mean, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(four_factors_df)
}

prepare_model_data <- function(tourney_results_df, four_factors_df) {
  training_data <- tourney_results_df %>%
    mutate(
      Team1 = pmin(WTeamID, LTeamID),
      Team2 = pmax(WTeamID, LTeamID),
      Team1_win = if_else(Team1 == WTeamID, 1, 0)
    ) %>%
    select(Season, Team1, Team2, Team1_win)
  model_data <- training_data %>%
    left_join(four_factors_df, by = c("Season", "Team1" = "TeamID")) %>%
    rename_with(~paste0(., "_T1"), .cols = -c(Season, Team1, Team2, Team1_win)) %>%
    left_join(four_factors_df, by = c("Season", "Team2" = "TeamID")) %>%
    rename_with(~paste0(., "_T2"), .cols = -c(Season, Team1, Team2, Team1_win, ends_with("_T1"))) %>%
    mutate(
      eFG_diff     = eFG_T1 - eFG_T2,
      TOV_Pct_diff = TOV_Pct_T1 - TOV_Pct_T2,
      ORB_Pct_diff = ORB_Pct_T1 - ORB_Pct_T2,
      FTR_diff     = FTR_T1 - FTR_T2,
      Win_diff     = Win_T1 - Win_T2
    ) %>%
    na.omit()
  return(model_data)
}
#load data
mens_results <- fread("march-machine-learning-mania-2025/MRegularSeasonDetailedResults.csv")
womens_results <- fread("march-machine-learning-mania-2025/WRegularSeasonDetailedResults.csv")
tourney_results_mens <- fread("march-machine-learning-mania-2025/MNCAATourneyCompactResults.csv")
tourney_results_womens <- fread("march-machine-learning-mania-2025/WNCAATourneyCompactResults.csv")

four_factors_mens <- calculate_four_factors(mens_results)
four_factors_womens <- calculate_four_factors(womens_results)

model_data_mens <- prepare_model_data(tourney_results_mens %>% filter(Season >= 2003), four_factors_mens)
model_data_womens <- prepare_model_data(tourney_results_womens %>% filter(Season >= 2010), four_factors_womens)

mens_model_2025 <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
                       data = model_data_mens %>% filter(Season %in% c(2023, 2024)),
                       family = binomial(link = "logit"))

womens_model_2025 <- glm(Team1_win ~ eFG_diff + TOV_Pct_diff + ORB_Pct_diff + FTR_diff + Win_diff,
                         data = model_data_womens %>% filter(Season %in% c(2023, 2024)),
                         family = binomial(link = "logit"))

games_to_predict_2025 <- fread("Excel_Files/2025_games_kaggle.csv")

predictions_2025_mens <- prepare_model_data(
  games_to_predict_2025 %>% filter(WTeamID < 3000), 
  four_factors_mens
)
predictions_2025_womens <- prepare_model_data(
  games_to_predict_2025 %>% filter(WTeamID > 3000), 
  four_factors_womens
)

predictions_2025_mens$Pred <- predict(mens_model_2025, newdata = predictions_2025_mens, type = "response")
predictions_2025_womens$Pred <- predict(womens_model_2025, newdata = predictions_2025_womens, type = "response")

combined_report_2025 <- bind_rows(predictions_2025_mens, predictions_2025_womens) %>% 
  mutate(
    brier_score_2025 = round(mean((Pred - Team1_win)^2, na.rm = TRUE),5),
    winning_brier_score = 0.10411,
    brier_score_per_game = round((Pred - Team1_win)^2,5)
  )

write.csv(combined_report_2025, "Excel_Files/DO_Model_Differences/combined_report_2025.csv")

library(ggplot2)
library(dplyr)
library(gridExtra)


create_conf_matrix <- function(data_df, true_col, pred_prob_col, title_text) {
  
  # Create Binary Classification (Cutoff 0.5)
  plot_data <- data.frame(
    Actual = data_df[[true_col]],
    Pred_Prob = data_df[[pred_prob_col]]
  )
  plot_data$Predicted_Class <- ifelse(plot_data$Pred_Prob > 0.5, 1, 0)
  
  # Aggregate Counts
  cm_counts <- plot_data %>%
    group_by(Actual, Predicted = Predicted_Class) %>%
    summarise(Count = n(), .groups = 'drop')
  
  # Set Factor Levels:
  # Actual: Win (Left) -> Loss (Right)
  cm_counts$Actual <- factor(cm_counts$Actual, levels = c(1, 0), labels = c("Win", "Loss"))
  # Predicted: Loss (Bottom) -> Win (Top)
  cm_counts$Predicted <- factor(cm_counts$Predicted, levels = c(0, 1), labels = c("Loss", "Win"))
  
  # Calculate Accuracy
  acc <- mean(plot_data$Actual == plot_data$Predicted_Class, na.rm = TRUE)
  
  # Plot
  ggplot(cm_counts, aes(x = Actual, y = Predicted, fill = Count)) +
    geom_tile(color = "white", lwd = 1) +
    geom_text(aes(label = Count), color = "white", size = 8, fontface = "bold") +
    scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
    scale_x_discrete(position = "top") + 
    labs(
      title = title_text,
      subtitle = paste0("Accuracy: ", round(acc * 100, 1), "%"),
      x = "Actual Outcome",
      y = "Predicted Outcome",
      fill = "Games"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 12, face = "bold"),
      panel.grid = element_blank(),
      legend.position = "none" 
    )
}

# Men's 2025
plot_mens_25 <- create_conf_matrix(predictions_2025_mens, "Team1_win", "Pred", "Men's 2025")

# Women's 2025
plot_womens_25 <- create_conf_matrix(predictions_2025_womens, "Team1_win", "Pred", "Women's 2025")

# Combined 2025
plot_combined_25 <- create_conf_matrix(combined_report_2025, "Team1_win", "Pred", "Combined 2025")

grid.arrange(plot_mens_25, plot_womens_25, plot_combined_25, ncol = 3)

# VIF - Men's
p_vif_mens <- plot(check_model(mens_model_2025, check = "vif"))
p_vif_mens + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# VIF - Women's
p_vif_womens <- plot(check_model(womens_model_2025, check = "vif"))
p_vif_womens + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# AUC - Men's
mens_train_data  <- model_data_mens %>% filter(Season %in% c(2023, 2024))
mens_train_preds <- predict(mens_model_2025, newdata = mens_train_data, type = "response")
roc_mens <- roc(mens_train_data$Team1_win, mens_train_preds)
cat("Men's Training AUC:", round(auc(roc_mens), 4), "\n")
plot(roc_mens, main = "ROC Curve - Men's Logistic Regression", col = "#132B43", lwd = 2)

# AUC - Women's
womens_train_data  <- model_data_womens %>% filter(Season %in% c(2023, 2024))
womens_train_preds <- predict(womens_model_2025, newdata = womens_train_data, type = "response")
roc_womens <- roc(womens_train_data$Team1_win, womens_train_preds)
cat("Women's Training AUC:", round(auc(roc_womens), 4), "\n")
plot(roc_womens, main = "ROC Curve - Women's Logistic Regression", col = "#132B43", lwd = 2)
#corr plot
mens_features <- model_data_mens %>%
  filter(Season %in% c(2023, 2024)) %>%
  select(eFG_diff, TOV_Pct_diff, ORB_Pct_diff, FTR_diff, Win_diff)

corrplot(cor(mens_features, use = "complete.obs"),
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         tl.col      = "black",
         tl.srt      = 45,
         title       = "Men's Feature Correlation Matrix",
         mar         = c(0, 0, 1, 0))

# Correlation Matrix - Women's
womens_features <- model_data_womens %>%
  filter(Season %in% c(2023, 2024)) %>%
  select(eFG_diff, TOV_Pct_diff, ORB_Pct_diff, FTR_diff, Win_diff)

corrplot(cor(womens_features, use = "complete.obs"),
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         tl.col      = "black",
         tl.srt      = 45,
         title       = "Women's Feature Correlation Matrix",
         mar         = c(0, 0, 1, 0))
