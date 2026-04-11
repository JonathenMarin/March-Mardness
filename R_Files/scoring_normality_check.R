library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)

# ── Load Data ─────────────────────────────────────────────────────────────────

mens_results   <- fread("march-machine-learning-mania-2025/MRegularSeasonDetailedResults.csv")
womens_results <- fread("march-machine-learning-mania-2025/WRegularSeasonDetailedResults.csv")

# ── Extract All Scores ────────────────────────────────────────────────────────

mens_scores <- mens_results %>%
  select(Season, WScore, LScore) %>%
  pivot_longer(cols = c(WScore, LScore), values_to = "Points") %>%
  select(Season, Points) %>%
  mutate(Division = "Men's")

womens_scores <- womens_results %>%
  select(Season, WScore, LScore) %>%
  pivot_longer(cols = c(WScore, LScore), values_to = "Points") %>%
  select(Season, Points) %>%
  mutate(Division = "Women's")

all_scores <- bind_rows(mens_scores, womens_scores)

cat("Men's score observations:  ", nrow(mens_scores), "\n")
cat("Women's score observations:", nrow(womens_scores), "\n")
cat("Men's seasons:   ", min(mens_scores$Season), "to", max(mens_scores$Season), "\n")
cat("Women's seasons: ", min(womens_scores$Season), "to", max(womens_scores$Season), "\n\n")

# ── Summary Statistics ────────────────────────────────────────────────────────

for (div in c("Men's", "Women's")) {
  d <- all_scores %>% filter(Division == div) %>% pull(Points)
  cat(div, "Summary:\n")
  cat("  Mean:   ", round(mean(d),   2), "\n")
  cat("  Median: ", round(median(d), 2), "\n")
  cat("  SD:     ", round(sd(d),     2), "\n")
  cat("  Min:    ", min(d), "\n")
  cat("  Max:    ", max(d), "\n\n")
}

# ── Histogram with Normal Curve Overlay ──────────────────────────────────────

plot_hist <- function(scores_df, division) {
  d <- scores_df %>% filter(Division == division) %>% pull(Points)
  ggplot(data.frame(Points = d), aes(x = Points)) +
    geom_histogram(aes(y = ..density..), bins = 50,
                   fill = "#56B1F7", color = "white") +
    stat_function(fun  = dnorm,
                  args = list(mean = mean(d), sd = sd(d)),
                  color = "#132B43", linewidth = 1.2) +
    labs(title = paste(division, "Regular Season Score Distribution"),
         subtitle = paste0("Mean = ", round(mean(d), 1),
                           ",  SD = ", round(sd(d), 1),
                           ",  n = ", format(length(d), big.mark = ",")),
         x = "Points Scored",
         y = "Density") +
    theme_minimal()
}

print(plot_hist(all_scores, "Men's"))
print(plot_hist(all_scores, "Women's"))

# ── Q-Q Plots ─────────────────────────────────────────────────────────────────

par(mfrow = c(1, 2))

mens_pts <- all_scores %>% filter(Division == "Men's") %>% pull(Points)
qqnorm(mens_pts, main = "Q-Q Plot: Men's Scores",
       col = "#56B1F7", pch = 16, cex = 0.3)
qqline(mens_pts, col = "#132B43", lwd = 2)

womens_pts <- all_scores %>% filter(Division == "Women's") %>% pull(Points)
qqnorm(womens_pts, main = "Q-Q Plot: Women's Scores",
       col = "#56B1F7", pch = 16, cex = 0.3)
qqline(womens_pts, col = "#132B43", lwd = 2)

par(mfrow = c(1, 1))

# ── Shapiro-Wilk Test (sample of 5000) ────────────────────────────────────────

set.seed(12)

mens_sample   <- sample(mens_pts,   5000)
womens_sample <- sample(womens_pts, 5000)

mens_shapiro   <- shapiro.test(mens_sample)
womens_shapiro <- shapiro.test(womens_sample)

cat("Shapiro-Wilk Test (n = 5000 sample):\n")
cat("  Men's   W =", round(mens_shapiro$statistic,   4),
    "  p =", round(mens_shapiro$p.value,   6), "\n")
cat("  Women's W =", round(womens_shapiro$statistic, 4),
    "  p =", round(womens_shapiro$p.value, 6), "\n\n")

# ── Empirical Rule Check ──────────────────────────────────────────────────────

empirical_check <- function(pts, division) {
  m  <- mean(pts)
  s  <- sd(pts)
  p1 <- mean(abs(pts - m) <= 1 * s)
  p2 <- mean(abs(pts - m) <= 2 * s)
  p3 <- mean(abs(pts - m) <= 3 * s)
  cat(division, "Empirical Rule Check:\n")
  cat("  Within 1 SD: ", round(p1 * 100, 2), "% (expected ~68%)\n")
  cat("  Within 2 SD: ", round(p2 * 100, 2), "% (expected ~95%)\n")
  cat("  Within 3 SD: ", round(p3 * 100, 2), "% (expected ~99.7%)\n\n")
}

empirical_check(mens_pts,   "Men's")
empirical_check(womens_pts, "Women's")