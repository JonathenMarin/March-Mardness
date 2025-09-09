library(dplyr)
library(readr)
library(data.table)

mens <- fread("march-machine-learning-mania-2025/MRegularSeasonDetailedResults.csv")
womens  <- fread("march-machine-learning-mania-2025/WRegularSeasonDetailedResults.csv")

df <- bind_rows(mens, womens)

winners <- df %>% select(Season, TeamID = WTeamID, Score = WScore, 
                         FGM = WFGM, FGA = WFGA, FGM3 = WFGM3, FGA3 = WFGA3,
                         FTM = WFTM, FTA = WFTA, OR = WOR, DR = WDR, 
                         Ast = WAst, TO = WTO, Stl = WStl, Blk = WBlk, PF = WPF)
losers <- df %>% select(Season, TeamID = LTeamID, Score = LScore, 
                        FGM = LFGM, FGA = LFGA, FGM3 = LFGM3, FGA3 = LFGA3,
                        FTM = LFTM, FTA = LFTA, OR = LOR, DR = LDR, 
                        Ast = LAst, TO = LTO, Stl = LStl, Blk = LBlk, PF = LPF)
games <- bind_rows(winners, losers)


season2024 <- games %>% filter(Season == 2024) %>% 
  group_by(TeamID) %>%  
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            .groups = "drop") #two means teams entered in 2025 (1479 and 1480)

season2025 <- games %>% filter(Season == 2025) %>% 
  group_by(TeamID) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            .groups = "drop")

<<<<<<< HEAD
write_csv(season2024, "C:/Users/jonathenmarin/Documents/March-Mardness/Excel_Files/2024avg.csv")
write_csv(season2025, "C:/Users/jonathenmarin/Documents/March-Mardness/2025avg.csv")
=======
write_csv(season2024, "C:/Users/jonathenmarin/Documents/March-Mardness/2024avg")
write_csv(season2025, "C:/Users/jonathenmarin/Documents/March-Mardness/2025avg")
>>>>>>> cb0ed524978ce0acc1d98e0a515a1bf97db8b1e9



