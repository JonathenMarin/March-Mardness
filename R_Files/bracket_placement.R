library(tidyverse)

setup_first_four <- function(teams_df) {
  # Identify play-in teams (teams with same Region and Seed)
  play_ins <- teams_df %>%
    group_by(Region, Seed) %>%
    filter(n() > 1) %>%
    arrange(Region, Seed, TeamID) %>%
    mutate(play_in_id = row_number()) %>%
    ungroup()
  
  if(nrow(play_ins) == 0) {
    return(tibble())
  }
  
  # Create First Four games
  first_four <- play_ins %>%
    group_by(Region, Seed) %>%
    summarise(
      region = first(Region),
      round = 0L,  # Round 0 for First Four
      game_id = paste0(first(Region), "_FirstFour_", first(Seed)),
      seed_high = first(Seed),
      seed_low = first(Seed),
      TeamID_High = first(TeamID),
      TeamID_Low = last(TeamID),
      slot = first(Seed),
      .groups = "drop"
    )
  
  return(first_four)
}

setup_tournament_bracket <- function() {
  
  regions <- c("East", "West", "South", "Midwest")
  
  seed_matchups <- list(
    round1 = list(
      c(1, 16), c(8, 9), c(5, 12), c(4, 13),
      c(6, 11), c(3, 14), c(7, 10), c(2, 15)
    )
  )
  
  bracket <- tibble()
  
  for (region in regions) {
    for (i in seq_along(seed_matchups$round1)) {
      matchup <- seed_matchups$round1[[i]]
      
      bracket <- bind_rows(
        bracket,
        tibble(
          region    = region,
          round     = 1L,
          game_id   = paste0(region, "_R1_G", i),
          seed_high = matchup[1],
          seed_low  = matchup[2],
          slot      = i
        )
      )
    }
  }
  
  bracket
}

create_bracket_with_teams <- function(teams_df) {
  # Remove duplicates for main bracket (keep first of each play-in pair)
  teams_main <- teams_df %>%
    group_by(Region, Seed) %>%
    slice(1) %>%
    ungroup()
  
  bracket <- setup_tournament_bracket()
  
  bracket_teams <- bracket %>%
    left_join(
      teams_main %>%
        transmute(
          Region,
          Seed,
          TeamID_High   = TeamID,
          TeamName_High = TeamName
        ),
      by = c("region" = "Region", "seed_high" = "Seed")
    ) %>%
    left_join(
      teams_main %>%
        transmute(
          Region,
          Seed,
          TeamID_Low   = TeamID,
          TeamName_Low = TeamName
        ),
      by = c("region" = "Region", "seed_low" = "Seed")
    )
  
  bracket_teams
}

run_full_tournament <- function(teams_df, prediction_model) {
  
  # Step 1: Run First Four if applicable
  first_four_bracket <- setup_first_four(teams_df)
  all_results <- list()
  result_index <- 1
  
  # Track which seeds have been decided by First Four
  first_four_winners <- tibble()
  
  if(nrow(first_four_bracket) > 0) {
    cat("Running First Four games...\n")
    
    first_four_results <- first_four_bracket %>%
      rowwise() %>%
      mutate(
        .pred       = list(prediction_model(TeamID_High, TeamID_Low)),
        winner_id   = .pred$winner_id,
        p_team_high = .pred$p_team1,
        p_team_low  = .pred$p_team2,
        winner_seed = seed_high  # Both teams have same seed
      ) %>%
      ungroup() %>%
      select(-.pred)
    
    all_results[[result_index]] <- first_four_results
    result_index <- result_index + 1
    
    # Track winners to replace in main bracket
    first_four_winners <- first_four_results %>%
      select(region, seed = winner_seed, winner_id)
  }
  
  # Step 2: Create main bracket with First Four winners incorporated
  teams_for_bracket <- teams_df %>%
    group_by(Region, Seed) %>%
    slice(1) %>%
    ungroup()
  
  # Replace teams that lost in First Four
  if(nrow(first_four_winners) > 0) {
    teams_for_bracket <- teams_for_bracket %>%
      left_join(first_four_winners, by = c("Region" = "region", "Seed" = "seed")) %>%
      mutate(TeamID = coalesce(winner_id, TeamID)) %>%
      select(-winner_id)
  }
  
  current_bracket <- create_bracket_with_teams(teams_for_bracket)
  
  # Step 3: Run rounds 1-6
  for (round_num in 1:6) {
    
    round_results <- current_bracket %>%
      rowwise() %>%
      mutate(
        .pred       = list(prediction_model(TeamID_High, TeamID_Low)),
        winner_id   = .pred$winner_id,
        p_team_high = .pred$p_team1,
        p_team_low  = .pred$p_team2,
        winner_seed = ifelse(winner_id == TeamID_High, seed_high, seed_low)
      ) %>%
      ungroup() %>%
      select(-.pred)
    
    all_results[[result_index]] <- round_results
    result_index <- result_index + 1
    
    if (round_num == 6) break
    
    if (round_num <= 3) {
      current_bracket <- round_results %>%
        group_by(region) %>%
        arrange(slot, .by_group = TRUE) %>%
        mutate(next_slot = ceiling(row_number() / 2)) %>%
        group_by(region, next_slot) %>%
        summarise(
          round       = round_num + 1L,
          game_id     = paste0(first(region), "_R", round_num + 1L, "_G", first(next_slot)),
          TeamID_High = first(winner_id),
          TeamID_Low  = last(winner_id),
          seed_high   = first(winner_seed),
          seed_low    = last(winner_seed),
          slot        = first(next_slot),
          .groups     = "drop"
        )
      
    } else if (round_num == 4) {
      regional_winners <- round_results %>%
        distinct(region, winner_id, winner_seed)
      
      ff_pairs <- tribble(
        ~ff_game, ~region_high, ~region_low,
        1L,       "South",   "West",
        2L,       "Midwest", "East"
      )
      
      ff_high <- ff_pairs %>%
        inner_join(regional_winners, by = c("region_high" = "region")) %>%
        rename(
          TeamID_High = winner_id,
          seed_high   = winner_seed
        )
      
      ff_low <- ff_pairs %>%
        inner_join(regional_winners, by = c("region_low" = "region")) %>%
        rename(
          TeamID_Low = winner_id,
          seed_low   = winner_seed
        )
      
      current_bracket <- ff_high %>%
        inner_join(ff_low, by = "ff_game") %>%
        transmute(
          region      = "Final Four",
          round       = 5L,
          game_id     = paste0("FF_G", ff_game),
          TeamID_High,
          TeamID_Low,
          seed_high,
          seed_low,
          slot        = ff_game
        )
      
    } else if (round_num == 5) {
      current_bracket <- round_results %>%
        summarise(
          region      = "Championship",
          round       = 6L,
          game_id     = "Championship",
          TeamID_High = first(winner_id),
          TeamID_Low  = last(winner_id),
          seed_high   = first(winner_seed),
          seed_low    = last(winner_seed),
          slot        = 1L
        )
    }
  }
  
  # Step 4: Combine all results and attach team names
  full_results <- bind_rows(all_results)
  
  # Remove any existing name columns to avoid duplicates
  full_results <- full_results %>%
    select(-any_of(c("TeamName_High", "TeamName_Low", "WinnerName")))
  
  # Attach high team names
  full_results <- full_results %>%
    left_join(
      teams_df %>% select(TeamID, TeamName),
      by = c("TeamID_High" = "TeamID")
    ) %>%
    rename(TeamName_High = TeamName)
  
  # Attach low team names
  full_results <- full_results %>%
    left_join(
      teams_df %>% select(TeamID, TeamName),
      by = c("TeamID_Low" = "TeamID")
    ) %>%
    rename(TeamName_Low = TeamName)
  
  # Attach winner's team name
  full_results <- full_results %>%
    left_join(
      teams_df %>% select(TeamID, TeamName),
      by = c("winner_id" = "TeamID")
    ) %>%
    rename(WinnerName = TeamName)
  
  return(full_results)
}

message("bracket_placement.R loaded with First Four support.")
message("Use run_full_tournament(teams_df, prediction_model).")