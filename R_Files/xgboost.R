library(data.table)
library(dplyr)

data_dir <- "march-machine-learning-mania-2025"

M_regular <- fread(file.path(data_dir, "MRegularSeasonDetailedResults.csv"))
M_tourney <- fread(file.path(data_dir, "MNCAATourneyDetailedResults.csv"))
M_seeds <- fread(file.path(data_dir, "MNCAATourneySeeds.csv"))

cat("Men's data loaded:\n")
cat("  Regular season games:", nrow(M_regular), "\n")
cat("  Tournament games:", nrow(M_tourney), "\n")
cat("  Seeds:", nrow(M_seeds), "\n\n")

W_regular <- fread(file.path(data_dir, "WRegularSeasonDetailedResults.csv"))
W_tourney <- fread(file.path(data_dir, "WNCAATourneyDetailedResults.csv"))
W_seeds <- fread(file.path(data_dir, "WNCAATourneySeeds.csv"))

cat("Women's data loaded:\n")
cat("  Regular season games:", nrow(W_regular), "\n")
cat("  Tournament games:", nrow(W_tourney), "\n")
cat("  Seeds:", nrow(W_seeds), "\n\n")


regular_results <- rbind(M_regular, W_regular)
tourney_results <- rbind(M_tourney, W_tourney)
seeds <- rbind(M_seeds, W_seeds)

cat("\nCombined data:\n")
cat("  Total regular season games:", nrow(regular_results), "\n")
cat("  Total tournament games:", nrow(tourney_results), "\n")
cat("  Total seeds:", nrow(seeds), "\n\n")


season_cutoff <- 2003

regular_results <- regular_results[Season >= season_cutoff]
tourney_results <- tourney_results[Season >= season_cutoff]
seeds <- seeds[Season >= season_cutoff]

cat("\nFiltered data:\n")
cat("  Regular season games:", nrow(regular_results), "\n")
cat("  Tournament games:", nrow(tourney_results), "\n")
cat("  Seeds:", nrow(seeds), "\n")
cat("  Seasons covered:", min(regular_results$Season), "to", max(regular_results$Season), "\n\n")


cat("Columns:", paste(names(regular_results), collapse = ", "), "\n")
cat("Columns:", paste(names(tourney_results), collapse = ", "), "\n")
cat("Columns:", paste(names(seeds), collapse = ", "), "\n")

unique_regular_teams <- unique(c(regular_results$WTeamID, regular_results$LTeamID))
unique_tourney_teams <- unique(c(tourney_results$WTeamID, tourney_results$LTeamID))
unique_seed_teams <- unique(seeds$TeamID)


cat("Unique teams in regular season:", length(unique_regular_teams), "\n")
cat("Unique teams in tournament:", length(unique_tourney_teams), "\n")
cat("Unique teams with seeds:", length(unique_seed_teams), "\n\n")


cat("Seasons in regular_results:", paste(sort(unique(regular_results$Season)), collapse = ", "), "\n")
cat("Seasons in tourney_results:", paste(sort(unique(tourney_results$Season)), collapse = ", "), "\n")
cat("Seasons in seeds:", paste(sort(unique(seeds$Season)), collapse = ", "), "\n\n")

example_season <- 2024
example_teamid <- 3163  # Change this to any team you're interested in

# Get regular season games
team_regular <- regular_results[
  Season == example_season & 
    (WTeamID == example_teamid | LTeamID == example_teamid)
]

# Get tournament games
team_tourney <- tourney_results[
  Season == example_season & 
    (WTeamID == example_teamid | LTeamID == example_teamid)
]


team_regular$result <- ifelse(team_regular$WTeamID == example_teamid, "WIN", "LOSS")
team_tourney$result <- ifelse(team_tourney$WTeamID == example_teamid, "WIN", "LOSS")

cat("Team", example_teamid, "in season", example_season, ":\n")
cat("Regular season games:", nrow(team_regular), "\n")
cat("Tournament games:", nrow(team_tourney), "\n\n")

cat("Regular season record:\n")
print(team_regular[, .(DayNum, WScore, LScore, result)])
cat("\n")

cat("Tournament record:\n")
print(team_tourney[, .(DayNum, WScore, LScore, result)])
cat("\n")

# Get this team's seed
team_seed <- seeds[Season == example_season & TeamID == example_teamid]
cat("Team seed:\n")
print(team_seed)
cat("\n")


prepare_data <- function(df) {
  
  df <- as.data.table(df)
  
  # Select box score columns
  keep_cols <- c(
    "Season", "DayNum", "NumOT",
    "WTeamID", "WScore", 
    "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF",
    "LTeamID", "LScore",
    "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF"
  )
  
  df <- df[, ..keep_cols]
  
  # Adjust for overtime
  df[, adjot := (40 + 5 * NumOT) / 40]
  
  stat_cols <- c(
    "WScore", "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF",
    "LScore", "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF"
  )
  
  df[, (stat_cols) := lapply(.SD, function(x) x / adjot), .SDcols = stat_cols]
  
  # Create Version 1 - Winner is Team1
  df1 <- copy(df)
  
  old_names <- c(
    "WTeamID", "WScore",
    "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF",
    "LTeamID", "LScore",
    "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF"
  )
  
  new_names <- c(
    "T1_TeamID", "T1_Score",
    "T1_FGM", "T1_FGA", "T1_FGM3", "T1_FGA3", "T1_FTM", "T1_FTA",
    "T1_OR", "T1_DR", "T1_Ast", "T1_TO", "T1_Stl", "T1_Blk", "T1_PF",
    "T2_TeamID", "T2_Score",
    "T2_FGM", "T2_FGA", "T2_FGM3", "T2_FGA3", "T2_FTM", "T2_FTA",
    "T2_OR", "T2_DR", "T2_Ast", "T2_TO", "T2_Stl", "T2_Blk", "T2_PF"
  )
  
  setnames(df1, old = old_names, new = new_names)
  
  # Create Version 2 - Loser is Team1
  df2 <- copy(df)
  
  old_names_swap <- c(
    "LTeamID", "LScore",
    "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF",
    "WTeamID", "WScore",
    "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF"
  )
  
  setnames(df2, old = old_names_swap, new = new_names)
  
  # Combine both versions
  output <- rbind(df1, df2)
  
  # Add derived features
  output[, PointDiff := T1_Score - T2_Score]
  output[, win := as.integer(PointDiff > 0)]
  output[, men_women := as.integer(substr(as.character(T1_TeamID), 1, 1) == "1")]
  output[, adjot := NULL]
  
  return(output)
}

# ============================================================================
# Apply the function
# ============================================================================

regular_data <- prepare_data(regular_results)
tourney_data <- prepare_data(tourney_results)

# Quick check
print(paste("Regular season rows:", nrow(regular_data)))
print(paste("Tournament rows:", nrow(tourney_data)))
head(tourney_data[, .(Season, T1_TeamID, T2_TeamID, T1_Score, T2_Score, PointDiff, win)])


