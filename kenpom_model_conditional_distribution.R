library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)

df <- fread("march-machine-learning-mania-2025/MRegularSeasonDetailedResults.csv")

long_df <- df %>%
  select(Season, DayNum, WTeamID, WScore, LTeamID, LScore) %>%
  pivot_longer(cols = c(WScore, LScore),
               names_to = "WL",
               values_to = "Score") %>%
  mutate(TeamID = ifelse(WL == "WScore", WTeamID, LTeamID)) %>%
  select(Season, WL,DayNum, TeamID, Score)

dist_5_year <- long_df %>% 
  filter(Season >= 2020, Season <= 2025)

summary(dist_5_year$Score) 

ggplot(dist_5_year, aes(x = Score)) + 
  geom_histogram(aes(y = ..density..), fill = 'white', color = 'black', alpha = 1.0, bins = 43) + 
geom_density(color = 'black', size = 1.0, alpha = 1.0) + 
  ggtitle("NCAAB Scoring Distribution, 2015-2025") + xlab("Points Scored") + ylab("Density")

#normal distribution of points, can use kenmpoms ratings.
#two different ratings against each other approach, one multiplicative (don oliver) and additive (new kenpom)
