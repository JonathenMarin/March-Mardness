library(tidyverse)
library(data.table)
library(reshape)
library(stringr)
library(dplyr)
library(pROC)

seeds_m <- fread("march-machine-learning-mania-2025/MNCAATourneySeeds.csv")
seeds_w <- fread("march-machine-learning-mania-2025/WNCAATourneySeeds.csv")
stats <- fread("march-machine-learning-mania-2025/MNCAATourneyDetailedResults.csv")

stats <- stats[stats$Season > 2000,]


stats <- stats %>%
  mutate(
  lower_id   = pmin(WTeamID, LTeamID),
  higher_id  = pmax(WTeamID, LTeamID),
    
  # Outcome: 1 if lower ID team won, else 0
  outcome = ifelse(WTeamID == lower_id, 1, 0),
  lower_eFG = ifelse(WTeamID == lower_id,
                      (WFGM + 0.5 * WFGM3) / WFGA,
                      (LFGM + 0.5 * LFGM3) / LFGA),
  higher_eFG = ifelse(WTeamID == higher_id, 
                      (WFGM + 0.5 * WFGM3) / WFGA,
                      (LFGM + 0.5 * LFGM3) / LFGA),
eFG_diff = lower_eFG - higher_eFG,
    lower_TOV = ifelse(WTeamID == lower_id,
                      (100 * WTO) / (WFGA + 0.44 * WFTA + WTO),
                      (100 * LTO) / (LFGA + 0.44 * LFTA + LTO)),
    higher_TOV = ifelse(WTeamID == higher_id,
                      (100 * WTO) / (WFGA + 0.44 * WFTA + WTO),
                      (100 * LTO) / (LFGA + 0.44 * LFTA + LTO)),
TOV_diff = lower_TOV - higher_TOV,
    lower_RB = ifelse(WTeamID == lower_id,
                      WOR / (WOR + WDR),
                      LOR / (LOR + LDR)),
    higher_RB = ifelse(WTeamID == higher_id,
                       WOR / (WOR + WDR),
                       LOR / (LOR + LDR)),
RB_diff = lower_RB - higher_RB,
    lower_FTR = ifelse(WTeamID == lower_id,
                       WFTA / WFGA,
                       LFTA / LFGA),
    higher_FTR = ifelse(WTeamID == higher_id,
                        WFTA / WFGA,
                        LFTA / LFGA),
FTR_diff = lower_FTR - higher_FTR
)

set.seed(123)
train_id <- sample(seq_len(nrow(stats)), size = 0.8 * nrow(stats))
train <- stats[train_id, ]
test <- stats[-train_id,]


model1 <- glm(outcome ~ eFG_diff + TOV_diff + RB_diff + FTR_diff, family = binomial, data = train) #might need to scale better 
summary(model1)


pred_prob <- predict(model1, newdata = test, type = "response")
test$brier <- (pred_prob - test$outcome)^2
overall_brier <- mean(test$brier)

overall_brier
