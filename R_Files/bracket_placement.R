

library(tidyverse)



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
          slot      = i  # game number within region
        )
      )
    }
  }
  
  bracket
}


create_bracket_with_teams <- function(teams_df) {
  bracket <- setup_tournament_bracket()
  
  bracket_teams <- bracket %>%
    # Attach "high seed" team
    left_join(
      teams_df %>%
        transmute(
          Region,
          Seed,
          TeamID_High   = TeamID,
          TeamName_High = TeamName
        ),
      by = c("region" = "Region", "seed_high" = "Seed")
    ) %>%
    # Attach "low seed" team
    left_join(
      teams_df %>%
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

#--------------------------------------------------------------

run_full_tournament <- function(teams_df, prediction_model) {
  
  current_bracket <- create_bracket_with_teams(teams_df)
  all_results     <- list()
  
  for (round_num in 1:6) {
    
    #---------------------------
    # Simulate current round
    #---------------------------
    round_results <- current_bracket %>%
      rowwise() %>%
      mutate(
        .pred       = list(prediction_model(TeamID_High, TeamID_Low)),
        winner_id   = .pred$winner_id,
        p_team_high = .pred$p_team1,   # prob TeamID_High wins
        p_team_low  = .pred$p_team2,   # prob TeamID_Low wins
        winner_seed = ifelse(winner_id == TeamID_High, seed_high, seed_low)
      ) %>%
      ungroup() %>%
      select(-.pred)
    
    all_results[[round_num]] <- round_results
    
    # Championship done
    if (round_num == 6) break
    
    if (round_num <= 3) {
      # R1→R2, R2→Sweet16, Sweet16→Elite8 (inside each region)
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
      # After regional finals → Final Four
      
      regional_winners <- round_results %>%
        distinct(region, winner_id, winner_seed)
      
      # FINAL FOUR PAIRING:
      #   FF_G1: South vs West
      #   FF_G2: Midwest vs East
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
      # Final Four → Championship
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
  
  #---------------------------
  # Combine all rounds
  # AND reattach team names for every round
  #---------------------------
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
  
  
message("bracket_placement.R loaded. Use run_full_tournament(teams_df, prediction_model).")
