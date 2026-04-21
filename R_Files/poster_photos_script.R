library(gridExtra)
library(grid)

brier_df <- data.frame(
  Predicted   = c("0.50", "1.00", "1.00"),
  Outcome     = c("Win or Loss", "Win", "Loss"),
  Scenario    = c("Baseline", "Perfectly confident, correct", "Perfectly confident, incorrect"),
  Brier_Score = c("0.2500", "0.0000", "1.0000")
)

colnames(brier_df) <- c("Predicted", "Outcome", "Scenario", "Brier Score")

png("brier_table.png", width = 700, height = 150, res = 120)
grid.table(brier_df, rows = NULL)
dev.off()


library(gridExtra)
library(grid)

add_stars <- function(p) {
  sapply(p, function(x) {
    if (x < 0.001) "***"
    else if (x < 0.01) "**"
    else if (x < 0.05) "*"
    else if (x < 0.1) "."
    else ""
  })
}

mens_coef_mc <- summary(model)$coefficients

clean_vars_mens <- c("Intercept", "Off Rating", "Def Rating", "AdjT Team", "AdjT Opp", "Diff NetRtg", "Diff Elo")

mens_df <- data.frame(
  Variable = clean_vars_mens,
  Estimate = round(mens_coef_mc[,1], 4),
  SE       = round(mens_coef_mc[,2], 4),
  p_value  = round(mens_coef_mc[,4], 4),
  Sig      = add_stars(mens_coef_mc[,4])
)

colnames(mens_df) <- c("Variable", "Estimate", "Std. Error", "p-value", "Sig.")

png("mens_model.png", width = 600, height = 280, res = 120)
grid.table(mens_df, rows = NULL)
dev.off()


womens_coef_mc <- summary(womens_model)$coefficients

clean_vars_womens <- c("Intercept", "Off Rating", "Def Rating", "AdjT Team", "AdjT Opp", "Diff Elo")

womens_df <- data.frame(
  Variable = clean_vars_womens,
  Estimate = round(womens_coef_mc[,1], 4),
  SE       = round(womens_coef_mc[,2], 4),
  p_value  = round(womens_coef_mc[,4], 4),
  Sig      = add_stars(womens_coef_mc[,4])
)

colnames(womens_df) <- c("Variable", "Estimate", "Std. Error", "p-value", "Sig.")

png("womens_model.png", width = 600, height = 250, res = 120)
grid.table(womens_df, rows = NULL)
dev.off()



results_df <- data.frame(
  Overrides     = c("0.12797"),
  Original      = c("0.13237"),
  Kaggle_Winner = c("0.10975")
)

colnames(results_df) <- c("Overrides", "Original", "Kaggle Winner")

png("brier_results.png", width = 500, height = 100, res = 120)
grid.table(results_df, rows = NULL)
dev.off()




acc_df <- data.frame(
  Overrides    = c("63.6%"),
  Original     = c("64.2%"),
  First_Place  = c("66.9%")
)

colnames(acc_df) <- c("Overrides", "Original", "First Place")

png("accuracy_results.png", width = 500, height = 100, res = 120)
grid.table(acc_df, rows = NULL)
dev.off()
