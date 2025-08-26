library(tidyverse)
library(data.table)
library(reshape)
library(stringr)
library(dplyr)

seeds_m <- fread("march-machine-learning-mania-2025/MNCAATourneySeeds.csv")
seeds_w <- fread("march-machine-learning-mania-2025/WNCAATourneySeeds.csv")

seed_table <- bind_rows(seeds_m, seeds_w) %>%
  as_tibble() %>%
  filter(Season == 2025) %>% 
  mutate(
    SeedValue = as.integer(str_extract(Seed, "\\d+")),
    SeedValue = replace_na(SeedValue, 16L)
  ) %>%
  select(Season, TeamID, SeedValue)


submission <- fread("march-machine-learning-mania-2025/SampleSubmissionStage2.csv") %>%
  as_tibble() %>%
  separate(ID, into = c("Season", "TeamID1", "TeamID2"),
           sep = "_", convert = TRUE, remove = FALSE)

seed_team1 <- seed_table %>% mutate(SeedTeam1 = SeedValue) %>% select(Season, TeamID, SeedTeam1)
seed_team2 <- seed_table %>% mutate(SeedTeam2 = SeedValue) %>% select(Season, TeamID, SeedTeam2)

submission <- submission %>%
  left_join(seed_team1, by = c("Season", "TeamID1" = "TeamID")) %>%
  left_join(seed_team2, by = c("Season", "TeamID2" = "TeamID"))

submission <- submission %>%
  mutate(
    Pred = case_when(
      SeedTeam1 < SeedTeam2 ~ 1.0,   # Team1 better seed → wins
      SeedTeam1 > SeedTeam2 ~ 0.0,   # Team2 better seed → Team1 loses
      TRUE ~ 0.5                      # tie or missing seeds
    )
  )


submission_final <- submission %>%
  select(ID, Pred)
