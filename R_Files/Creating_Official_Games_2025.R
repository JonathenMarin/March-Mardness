library(dplyr)
library(readr)


games_2025 <- read.csv("C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/2025games.csv")

games_2025 <- games_2025 %>% mutate(
  lower_id = pmin(LTeamID, WTeamID),
  higher_id = pmax(LTeamID, WTeamID),
  matchup_id = paste(Season, lower_id, higher_id, sep  = "_")
)

games_2025 <- games_2025 %>% mutate(
  outcome = ifelse(WTeamID == lower_id, 1, 0)
)

games_2025 <- write_csv(games_2025, "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/Sample_Submission_2025.csv")
