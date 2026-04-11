library(dplyr)
library(data.table)
library(ggplot2)
library(gridExtra)
library(pROC)

# ── Load Files ────────────────────────────────────────────────────────────────

actual_outcomes  <- fread("Excel_Files/Result Files/2026 MM Solution File.csv")
submissions_over <- fread("Excel_Files/Result Files/monte_carlo_combined_with_overrides.csv")
submissions_base <- fread("Excel_Files/Result Files/monte_carlo_mens_womens_no_overrides.csv")

setnames(actual_outcomes,  "Pred", "Actual")
setnames(submissions_over, "Pred", "Pred_Override")
setnames(submissions_base, "Pred", "Pred_Base")

# ── Merge ─────────────────────────────────────────────────────────────────────

results_table <- actual_outcomes %>%
  select(ID, TeamName.1, TeamName.2, Actual) %>%
  inner_join(submissions_over, by = "ID") %>%
  inner_join(submissions_base, by = "ID") %>%
  mutate(
    Brier_Override   = round((Pred_Override - Actual)^2, 5),
    Brier_Base       = round((Pred_Base     - Actual)^2, 5),
    Override_Applied = as.integer(round(Pred_Override, 4) != round(Pred_Base, 4))
  ) %>%
  arrange(ID)

cat("Total games:", nrow(results_table), "\n")
cat("Mean Brier (Override):", round(mean(results_table$Brier_Override), 5), "\n")
cat("Mean Brier (Base):    ", round(mean(results_table$Brier_Base),     5), "\n\n")

# ── Split mens and womens using Actual column directly ────────────────────────

results_mens   <- results_table %>% filter(substr(ID, 6, 9) < "3000")
results_womens <- results_table %>% filter(substr(ID, 6, 9) >= "3000")

cat("Men's games:  ", nrow(results_mens),   "\n")
cat("Women's games:", nrow(results_womens), "\n\n")

# actual outcomes directly from Actual column
actual_mens   <- results_mens$Actual
actual_womens <- results_womens$Actual
actual_all    <- results_table$Actual

# predicted classes
pred_over_mens   <- ifelse(results_mens$Pred_Override   > 0.5, 1, 0)
pred_over_womens <- ifelse(results_womens$Pred_Override > 0.5, 1, 0)
pred_over_all    <- ifelse(results_table$Pred_Override  > 0.5, 1, 0)

pred_base_mens   <- ifelse(results_mens$Pred_Base       > 0.5, 1, 0)
pred_base_womens <- ifelse(results_womens$Pred_Base     > 0.5, 1, 0)
pred_base_all    <- ifelse(results_table$Pred_Base      > 0.5, 1, 0)

# ── Accuracy ──────────────────────────────────────────────────────────────────

cat("Accuracy (Override) - Men's:   ", round(mean(pred_over_mens   == actual_mens)   * 100, 2), "%\n")
cat("Accuracy (Override) - Women's: ", round(mean(pred_over_womens == actual_womens) * 100, 2), "%\n")
cat("Accuracy (Override) - Combined:", round(mean(pred_over_all    == actual_all)    * 100, 2), "%\n\n")

cat("Accuracy (Base) - Men's:       ", round(mean(pred_base_mens   == actual_mens)   * 100, 2), "%\n")
cat("Accuracy (Base) - Women's:     ", round(mean(pred_base_womens == actual_womens) * 100, 2), "%\n")
cat("Accuracy (Base) - Combined:    ", round(mean(pred_base_all    == actual_all)    * 100, 2), "%\n\n")

# ── AUC ───────────────────────────────────────────────────────────────────────

roc_over_mens   <- roc(actual_mens,   results_mens$Pred_Override)
roc_over_womens <- roc(actual_womens, results_womens$Pred_Override)
roc_over_all    <- roc(actual_all,    results_table$Pred_Override)

roc_base_mens   <- roc(actual_mens,   results_mens$Pred_Base)
roc_base_womens <- roc(actual_womens, results_womens$Pred_Base)
roc_base_all    <- roc(actual_all,    results_table$Pred_Base)

cat("AUC (Override) - Men's:   ", round(auc(roc_over_mens),   4), "\n")
cat("AUC (Override) - Women's: ", round(auc(roc_over_womens), 4), "\n")
cat("AUC (Override) - Combined:", round(auc(roc_over_all),    4), "\n\n")

cat("AUC (Base) - Men's:       ", round(auc(roc_base_mens),   4), "\n")
cat("AUC (Base) - Women's:     ", round(auc(roc_base_womens), 4), "\n")
cat("AUC (Base) - Combined:    ", round(auc(roc_base_all),    4), "\n\n")

# ── Confusion matrix function ─────────────────────────────────────────────────

make_conf_plot <- function(actual, predicted, title) {
  conf_df <- as.data.frame(table(Predicted = factor(predicted, levels = c(0,1)),
                                 Actual    = factor(actual,    levels = c(0,1))))
  conf_df$Predicted <- factor(conf_df$Predicted, levels = c(0, 1))
  conf_df$Actual    <- factor(conf_df$Actual,    levels = c(1, 0))
  
  ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 6) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    scale_x_discrete(labels = c("0" = "Loss", "1" = "Win")) +
    scale_y_discrete(labels = c("1" = "Win",  "0" = "Loss")) +
    labs(title = title, x = "Actual Result", y = "Predicted Result") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 11))
}

# ── Override confusion matrices ───────────────────────────────────────────────

grid.arrange(
  make_conf_plot(actual_mens,   pred_over_mens,   "Override - Men's"),
  make_conf_plot(actual_womens, pred_over_womens, "Override - Women's"),
  make_conf_plot(actual_all,    pred_over_all,    "Override - Combined"),
  nrow = 1, ncol = 3
)

# ── Base confusion matrices ───────────────────────────────────────────────────

grid.arrange(
  make_conf_plot(actual_mens,   pred_base_mens,   "Base - Men's"),
  make_conf_plot(actual_womens, pred_base_womens, "Base - Women's"),
  make_conf_plot(actual_all,    pred_base_all,    "Base - Combined"),
  nrow = 1, ncol = 3
)

# ── ROC curves ────────────────────────────────────────────────────────────────

par(mfrow = c(1, 2))

plot(roc_over_all,
     legacy.axes = TRUE,
     main = paste0("Override (AUC = ", round(auc(roc_over_all), 4), ")"),
     col  = "#132B43",
     lwd  = 2,
     xlab = "False Positive Rate (FPR)",
     ylab = "True Positive Rate (TPR)")

plot(roc_base_all,
     legacy.axes = TRUE,
     main = paste0("Base (AUC = ", round(auc(roc_base_all), 4), ")"),
     col  = "#132B43",
     lwd  = 2,
     xlab = "False Positive Rate (FPR)",
     ylab = "True Positive Rate (TPR)")

# ── Top 10 worst predictions ──────────────────────────────────────────────────

mens_worst <- results_table %>%
  filter(substr(ID, 6, 9) < "3000") %>%
  arrange(desc(Brier_Override)) %>%
  head(10) %>%
  mutate(Winner  = ifelse(Actual == 1, TeamName.1, TeamName.2),
         Matchup = paste0(TeamName.1, " vs ", TeamName.2, " (", Winner, ")"))

womens_worst <- results_table %>%
  filter(substr(ID, 6, 9) >= "3000") %>%
  arrange(desc(Brier_Override)) %>%
  head(10) %>%
  mutate(Winner  = ifelse(Actual == 1, TeamName.1, TeamName.2),
         Matchup = paste0(TeamName.1, " vs ", TeamName.2, " (", Winner, ")"))

ggplot(mens_worst, aes(x = Brier_Override,
                       y = reorder(Matchup, Brier_Override))) +
  geom_bar(stat = "identity", fill = "#132B43") +
  geom_point(aes(x = Brier_Base), color = "#56B1F7", size = 3) +
  labs(title    = "Top 10 Worst Predicted Men's Games (2026)",
       subtitle = "Bar = Override Brier Score, Blue Dot = Base Brier Score",
       x = "Brier Score", y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggplot(womens_worst, aes(x = Brier_Override,
                         y = reorder(Matchup, Brier_Override))) +
  geom_bar(stat = "identity", fill = "#132B43") +
  geom_point(aes(x = Brier_Base), color = "#56B1F7", size = 3) +
  labs(title    = "Top 10 Worst Predicted Women's Games (2026)",
       subtitle = "Bar = Override Brier Score, Blue Dot = Base Brier Score",
       x = "Brier Score", y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# ── Export results table ──────────────────────────────────────────────────────

fwrite(results_table, "Excel_Files/Result Files/2026_results_comparison.csv")
cat("Results exported.\n")
