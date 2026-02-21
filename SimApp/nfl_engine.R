# ============================================================================
# NFL SIMULATION ENGINE - Based on original standalone app
# ============================================================================

library(data.table)
library(readxl)
library(dplyr)

# ============================================================================
# HELPER: DST Detection
# ============================================================================

has_dst_scoring <- function(similar_games, team_names) {
  team1_for_cols <- gsub("_", " ", team_names[1])
  team2_for_cols <- gsub("_", " ", team_names[2])
  
  def_cols <- c(
    paste0(team1_for_cols, "_Def_Sacks"),
    paste0(team1_for_cols, "_Def_Ints"),
    paste0(team1_for_cols, "_Def_Fum"),
    paste0(team1_for_cols, "_Def_Pts_Allow"),
    paste0(team2_for_cols, "_Def_Sacks"),
    paste0(team2_for_cols, "_Def_Ints"),
    paste0(team2_for_cols, "_Def_Fum"),
    paste0(team2_for_cols, "_Def_Pts_Allow")
  )
  
  all_exist <- all(def_cols %in% names(similar_games))
  return(all_exist)
}

simulate_team_game <- function(sim_id,
                               team_name,
                               team_data,
                               sampled_game,
                               dk_salaries,
                               similar_games,
                               use_dst = FALSE,
                               opponent_ints = 0) {
  # Sheet names have underscores, Similar_Games columns have spaces
  team_name_for_cols <- gsub("_", " ", team_name)
  
  # Extract game stats
  rush_col <- paste0(team_name_for_cols, "_Rush")
  pass_col <- paste0(team_name_for_cols, "_Pass")
  rush_td_col <- paste0(team_name_for_cols, "_Rush_TDs")
  pass_td_col <- paste0(team_name_for_cols, "_Pass_TDs")
  fg_col <- paste0(team_name_for_cols, "_FGs")
  
  team_rush_yds <- as.numeric(sampled_game[[rush_col]])
  team_pass_yds <- as.numeric(sampled_game[[pass_col]])
  team_rush_tds <- as.numeric(sampled_game[[rush_td_col]])
  team_pass_tds <- as.numeric(sampled_game[[pass_td_col]])
  team_fgs <- as.numeric(sampled_game[[fg_col]])
  
  # Validate and set defaults
  if (is.na(team_rush_yds) ||
      length(team_rush_yds) == 0)
    team_rush_yds <- 0
  if (is.na(team_pass_yds) ||
      length(team_pass_yds) == 0)
    team_pass_yds <- 0
  if (is.na(team_rush_tds) ||
      length(team_rush_tds) == 0)
    team_rush_tds <- 0
  if (is.na(team_pass_tds) ||
      length(team_pass_tds) == 0)
    team_pass_tds <- 0
  if (is.na(team_fgs) || length(team_fgs) == 0)
    team_fgs <- 0
  
  # Pre-allocate results list
  all_player_results <- list()
  result_idx <- 1
  
  # Helper function: Calculate compression factor based on team total extremeness
  # Higher team totals = more compression (narrower individual ranges)
  calculate_compression <- function(team_total, similar_games_column) {
    if (length(similar_games_column) == 0 ||
        is.na(team_total))
      return(0)
    
    # Calculate percentile of this team total
    team_percentile <- ecdf(similar_games_column)(team_total)
    
    # Compression increases as we move away from median (0.50)
    # Distance from median: 0 (at median) to 0.5 (at extremes)
    distance_from_median <- abs(team_percentile - 0.50)
    
    # Convert to compression: 0 at median, up to 0.85 at extremes
    # This means at 99th percentile, we compress range by 85%
    compression <- distance_from_median * 1.7  # 0.5 * 1.7 = 0.85 max
    compression <- min(compression, 0.85)  # Cap at 85%
    
    return(compression)
  }
  
  # Helper function: sample from percentiles with optional compression
  # compression = 0 means full range, compression = 0.85 means very narrow range around median
  sample_from_percentiles <- function(floor,
                                      p25,
                                      p50,
                                      p75,
                                      ceiling,
                                      compression = 0) {
    percentile <- runif(1, 0, 1)
    
    # Apply compression by squeezing percentile toward 0.50 (median)
    if (compression > 0) {
      # Compress the percentile range toward median
      compressed_percentile <- 0.50 + (percentile - 0.50) * (1 - compression)
      percentile <- compressed_percentile
    }
    
    if (percentile <= 0.05) {
      return(0)  # Below floor = injury/benched
    } else if (percentile <= 0.25) {
      return(floor + (p25 - floor) * (percentile - 0.05) / 0.20)
    } else if (percentile <= 0.50) {
      return(p25 + (p50 - p25) * (percentile - 0.25) / 0.25)
    } else if (percentile <= 0.75) {
      return(p50 + (p75 - p50) * (percentile - 0.50) / 0.25)
    } else if (percentile <= 0.95) {
      return(p75 + (ceiling - p75) * (percentile - 0.75) / 0.20)
    } else {
      return(ceiling)  # Hard ceiling at P95
    }
  }
  
  # CALCULATE COMPRESSION FACTORS based on team total extremeness
  rush_compression <- calculate_compression(team_rush_yds, similar_games[[rush_col]])
  pass_compression <- calculate_compression(team_pass_yds, similar_games[[pass_col]])
  
  # RUSHING
  rushing_data <- team_data$rushing
  n_rushers <- nrow(rushing_data)
  
  if (n_rushers > 0) {
    # Smart allocation with cumulative adjustment
    rush_yds_allocation <- numeric(n_rushers)
    remaining_share <- 1.0
    allocated_share <- 0.0
    
    for (i in 1:n_rushers) {
      floor_pct <- rushing_data$Floor[i]
      p25_pct <- rushing_data$Pct_P25[i]
      p50_pct <- rushing_data$Pct_P50[i]
      p75_pct <- rushing_data$Pct_P75[i]
      ceiling_pct <- rushing_data$Ceiling[i]
      
      # Handle NAs
      if (is.na(floor_pct))
        floor_pct <- 0
      if (is.na(p25_pct))
        p25_pct <- floor_pct
      if (is.na(p50_pct))
        p50_pct <- p25_pct
      if (is.na(p75_pct))
        p75_pct <- p50_pct
      if (is.na(ceiling_pct))
        ceiling_pct <- p75_pct
      
      # SMART STOP: If we've allocated 98%+ and this player has Floor=0, skip
      if (allocated_share > 0.98 && floor_pct == 0) {
        rush_yds_allocation[i] <- 0
        next
      }
      
      # Sample player share WITH COMPRESSION
      player_share <- sample_from_percentiles(floor_pct,
                                              p25_pct,
                                              p50_pct,
                                              p75_pct,
                                              ceiling_pct,
                                              rush_compression)
      
      # CUMULATIVE ADJUSTMENT
      if (i > 1 && remaining_share < 1.0) {
        # Only count remaining players who are expected to play (P50 > 1%)
        remaining_players_expected <- rushing_data$Pct_P50[(i):n_rushers]
        remaining_players_expected[is.na(remaining_players_expected)] <- 0
        remaining_players_expected <- remaining_players_expected[remaining_players_expected > 0.01]
        
        expected_remaining <- sum(remaining_players_expected, na.rm = TRUE)
        
        if (expected_remaining > 0) {
          adjustment_factor <- remaining_share / expected_remaining
          adjustment_factor <- max(0.5, min(2.0, adjustment_factor))
          player_share <- player_share * adjustment_factor
        }
      }
      
      # Ensure player doesn't exceed remaining share
      player_share <- min(player_share, remaining_share)
      player_share <- max(0, player_share)
      
      # SMART ROUNDING: If remaining < 5%, give it all to this player or none
      if (remaining_share < 0.05) {
        if (player_share >= remaining_share * 0.5) {
          player_share <- remaining_share  # Give the scraps
        } else {
          player_share <- 0  # Not worth it
        }
      }
      
      rush_yds_allocation[i] <- round(team_rush_yds * player_share)
      remaining_share <- remaining_share - player_share
      allocated_share <- allocated_share + player_share
      
      # HARD STOP: If we've allocated everything, done
      if (remaining_share <= 0.01) {
        # Zero out remaining players
        if (i < n_rushers) {
          rush_yds_allocation[(i + 1):n_rushers] <- 0
        }
        break
      }
    }
    
    # IMPROVED SWEEP UP: Distribute remaining yards respecting ceiling constraints
    if (remaining_share > 0.01) {
      # Pass 1: Go through players from top, cap at 20% of remaining per player
      for (i in 1:n_rushers) {
        if (remaining_share <= 0.01)
          break
        
        ceiling_pct <- rushing_data$Ceiling[i]
        if (is.na(ceiling_pct))
          ceiling_pct <- rushing_data$Pct_P75[i]
        if (is.na(ceiling_pct))
          ceiling_pct <- 1.0
        
        current_share <- rush_yds_allocation[i] / team_rush_yds
        room_to_ceiling <- ceiling_pct - current_share
        
        if (room_to_ceiling > 0.01) {
          additional_share <- min(
            remaining_share,
            room_to_ceiling,
            remaining_share * 0.20  # Cap at 20% per iteration
          )
          
          additional_yards <- round(team_rush_yds * additional_share)
          rush_yds_allocation[i] <- rush_yds_allocation[i] + additional_yards
          remaining_share <- remaining_share - additional_share
        }
      }
      
      # Pass 2: If still remaining, go through again without 20% cap
      if (remaining_share > 0.01) {
        for (i in 1:n_rushers) {
          if (remaining_share <= 0.01)
            break
          
          ceiling_pct <- rushing_data$Ceiling[i]
          if (is.na(ceiling_pct))
            ceiling_pct <- 1.0
          
          current_share <- rush_yds_allocation[i] / team_rush_yds
          room_to_ceiling <- ceiling_pct - current_share
          
          if (room_to_ceiling > 0.01) {
            additional_share <- min(remaining_share, room_to_ceiling)
            additional_yards <- round(team_rush_yds * additional_share)
            
            rush_yds_allocation[i] <- rush_yds_allocation[i] + additional_yards
            remaining_share <- remaining_share - additional_share
          }
        }
      }
      
      # Pass 3: Final cleanup - distribute evenly RESPECTING CEILINGS
      if (remaining_share > 0.01) {
        final_remaining_yards <- round(team_rush_yds * remaining_share)
        
        # Only give to players who still have room to ceiling
        for (yard in 1:final_remaining_yards) {
          # Find players with room to ceiling
          players_with_room <- numeric(0)
          room_amounts <- numeric(0)
          
          for (i in 1:n_rushers) {
            if (rush_yds_allocation[i] > 0) {
              # Only active players
              ceiling_pct <- rushing_data$Ceiling[i]
              if (is.na(ceiling_pct))
                ceiling_pct <- 1.0
              
              current_share <- rush_yds_allocation[i] / team_rush_yds
              room <- ceiling_pct - current_share
              
              if (room > 0.001) {
                # Has room
                players_with_room <- c(players_with_room, i)
                room_amounts <- c(room_amounts, room)
              }
            }
          }
          
          # If no one has room, we're done (better to under-allocate than break ceilings)
          if (length(players_with_room) == 0)
            break
          
          # Give 1 yard to a random player with room (equal probability)
          selected <- sample(players_with_room, 1)
          rush_yds_allocation[selected] <- rush_yds_allocation[selected] + 1
        }
      }
    }
    
    # Second pass: Allocate rushing TDs with 70% input rate / 30% production + diminishing returns
    td_allocation <- rep(0, n_rushers)
    if (team_rush_tds > 0) {
      # Historical TD rates (70% weight)
      td_rates <- as.numeric(rushing_data$TD_Rate)
      td_rates[is.na(td_rates)] <- 0
      td_rates[td_rates < 0] <- 0
      
      # Production in this sim (30% weight)
      production_weight <- rush_yds_allocation / max(rush_yds_allocation, 1)
      
      # 70/30 split
      td_probs <- (td_rates * 0.7) + (production_weight * 0.3)
      
      # Only players with rush yards are eligible
      eligible <- which(rush_yds_allocation > 0)
      
      if (length(eligible) > 0 && sum(td_probs[eligible]) > 0) {
        td_probs_eligible <- td_probs[eligible]
        td_probs_eligible <- td_probs_eligible / sum(td_probs_eligible)
        
        # Allocate TDs with diminishing returns
        for (td in 1:team_rush_tds) {
          selected_idx <- sample(1:length(eligible), 1, prob = td_probs_eligible)
          selected <- eligible[selected_idx]
          
          td_allocation[selected] <- td_allocation[selected] + 1
          
          # AGGRESSIVE diminishing returns: Each TD drastically reduces probability
          # After 1 TD: 0.3x probability
          # After 2 TDs: 0.09x probability (basically impossible to get 3rd)
          reduction_factor <- 0.3^td_allocation[selected]
          td_probs_eligible[selected_idx] <- td_probs_eligible[selected_idx] * reduction_factor
          
          if (sum(td_probs_eligible) > 0) {
            td_probs_eligible <- td_probs_eligible / sum(td_probs_eligible)
          } else {
            td_probs_eligible <- rep(1 / length(eligible), length(eligible))
          }
        }
      }
    }
    
    # CONSTRAINT: Rush TD requires rush yards > 0
    for (i in 1:n_rushers) {
      if (td_allocation[i] > 0 && rush_yds_allocation[i] == 0) {
        rush_yds_allocation[i] <- 1
      }
    }
    
    # WORKLOAD CONSTRAINT: Track rushing outcomes for receiving cap
    player_rush_outcome <- list()
    
    for (i in 1:n_rushers) {
      if (rush_yds_allocation[i] > 0) {
        rush_share <- rush_yds_allocation[i] / max(team_rush_yds, 1)
        p75 <- rushing_data$Pct_P75[i]
        
        # If rushing outcome >= P75, mark as "hot"
        if (!is.na(p75) && p75 > 0 && rush_share >= p75) {
          player_rush_outcome[[rushing_data$Player[i]]] <- "hot"
        }
      }
    }
    
    # Create player results
    for (i in 1:n_rushers) {
      all_player_results[[result_idx]] <- list(
        SimID = sim_id,
        Team = team_name,
        Player = rushing_data$Player[i],
        PassYds = 0,
        PassTDs = 0L,
        INTs = 0L,
        RushYds = rush_yds_allocation[i],
        RushTDs = as.integer(td_allocation[i]),
        Recs = 0L,
        RecYds = 0,
        RecTDs = 0L,
        FGsMade = 0L,
        FG_Under30 = 0L,
        FG_30_39 = 0L,
        FG_40_49 = 0L,
        FG_50Plus = 0L,
        XPs = 0L,
        FumLost = 0L
      )
      result_idx <- result_idx + 1
    }
  }
  
  # RECEIVING
  receiving_data <- team_data$receiving
  n_receivers <- nrow(receiving_data)
  
  if (n_receivers > 0) {
    # Get team total receptions from similar game
    team_recs_col <- paste0(team_name_for_cols, "_Recs")
    team_total_recs <- as.numeric(sampled_game[[team_recs_col]])
    if (is.na(team_total_recs) ||
        length(team_total_recs) == 0)
      team_total_recs <- 0
    
    # First pass: Allocate receiving yards with smart cumulative adjustment
    rec_yds_allocation <- numeric(n_receivers)
    ypr_values <- numeric(n_receivers)
    remaining_share <- 1.0
    allocated_share <- 0.0
    
    for (i in 1:n_receivers) {
      floor_pct <- receiving_data$Floor[i]
      p25_pct <- receiving_data$Pct_P25[i]
      p50_pct <- receiving_data$Pct_P50[i]
      p75_pct <- receiving_data$Pct_P75[i]
      ceiling_pct <- receiving_data$Ceiling[i]
      
      # Handle NAs
      if (is.na(floor_pct))
        floor_pct <- 0
      if (is.na(p25_pct))
        p25_pct <- floor_pct
      if (is.na(p50_pct))
        p50_pct <- p25_pct
      if (is.na(p75_pct))
        p75_pct <- p50_pct
      if (is.na(ceiling_pct))
        ceiling_pct <- p75_pct
      
      # WORKLOAD CONSTRAINT: If player had hot rushing game, cap receiving at P50
      player_name <- receiving_data$Player[i]
      if (player_name %in% names(player_rush_outcome)) {
        if (player_rush_outcome[[player_name]] == "hot") {
          ceiling_pct <- min(ceiling_pct, p50_pct)
        }
      }
      
      # Sample player share WITH COMPRESSION
      target_share <- sample_from_percentiles(floor_pct,
                                              p25_pct,
                                              p50_pct,
                                              p75_pct,
                                              ceiling_pct,
                                              pass_compression)
      
      # CUMULATIVE ADJUSTMENT
      if (i > 1 && remaining_share < 1.0) {
        # Only count remaining players who are expected to play (P50 > 1%)
        remaining_players_expected <- receiving_data$Pct_P50[(i):n_receivers]
        remaining_players_expected[is.na(remaining_players_expected)] <- 0
        remaining_players_expected <- remaining_players_expected[remaining_players_expected > 0.01]
        
        expected_remaining <- sum(remaining_players_expected, na.rm = TRUE)
        
        if (expected_remaining > 0) {
          adjustment_factor <- remaining_share / expected_remaining
          adjustment_factor <- max(0.5, min(2.0, adjustment_factor))
          target_share <- target_share * adjustment_factor
        }
      }
      
      # Ensure player doesn't exceed remaining share
      target_share <- min(target_share, remaining_share)
      target_share <- max(0, target_share)
      
      # SMART ROUNDING: If remaining < 5%, give it all or none
      if (remaining_share < 0.05) {
        if (target_share >= remaining_share * 0.5) {
          target_share <- remaining_share
        } else {
          target_share <- 0
        }
      }
      
      rec_yds_allocation[i] <- round(team_pass_yds * target_share)
      remaining_share <- remaining_share - target_share
      allocated_share <- allocated_share + target_share
      
      ypr <- receiving_data$YPR[i]
      if (is.na(ypr) || ypr <= 0)
        ypr <- 10
      ypr_values[i] <- ypr
      
      # HARD STOP: If we've allocated everything, done
      if (remaining_share <= 0.01) {
        if (i < n_receivers) {
          rec_yds_allocation[(i + 1):n_receivers] <- 0
          ypr_values[(i + 1):n_receivers] <- 10
        }
        break
      }
    }
    
    # IMPROVED SWEEP UP: Distribute remaining yards respecting ceiling constraints
    if (remaining_share > 0.01) {
      # Pass 1: Go through players from top, cap at 20% of remaining per player
      for (i in 1:n_receivers) {
        if (remaining_share <= 0.01)
          break
        
        ceiling_pct <- receiving_data$Ceiling[i]
        if (is.na(ceiling_pct))
          ceiling_pct <- receiving_data$Pct_P75[i]
        if (is.na(ceiling_pct))
          ceiling_pct <- 1.0
        
        current_share <- rec_yds_allocation[i] / team_pass_yds
        room_to_ceiling <- ceiling_pct - current_share
        
        if (room_to_ceiling > 0.01) {
          additional_share <- min(
            remaining_share,
            room_to_ceiling,
            remaining_share * 0.20  # Cap at 20% per iteration
          )
          
          additional_yards <- round(team_pass_yds * additional_share)
          rec_yds_allocation[i] <- rec_yds_allocation[i] + additional_yards
          remaining_share <- remaining_share - additional_share
        }
      }
      
      # Pass 2: If still remaining, go through again without 20% cap
      if (remaining_share > 0.01) {
        for (i in 1:n_receivers) {
          if (remaining_share <= 0.01)
            break
          
          ceiling_pct <- receiving_data$Ceiling[i]
          if (is.na(ceiling_pct))
            ceiling_pct <- 1.0
          
          current_share <- rec_yds_allocation[i] / team_pass_yds
          room_to_ceiling <- ceiling_pct - current_share
          
          if (room_to_ceiling > 0.01) {
            additional_share <- min(remaining_share, room_to_ceiling)
            additional_yards <- round(team_pass_yds * additional_share)
            
            rec_yds_allocation[i] <- rec_yds_allocation[i] + additional_yards
            remaining_share <- remaining_share - additional_share
          }
        }
      }
      
      # Pass 3: Final cleanup - distribute evenly RESPECTING CEILINGS
      if (remaining_share > 0.01) {
        final_remaining_yards <- round(team_pass_yds * remaining_share)
        
        # Only give to players who still have room to ceiling
        for (yard in 1:final_remaining_yards) {
          # Find players with room to ceiling
          players_with_room <- numeric(0)
          room_amounts <- numeric(0)
          
          for (i in 1:n_receivers) {
            if (rec_yds_allocation[i] > 0) {
              # Only active players
              ceiling_pct <- receiving_data$Ceiling[i]
              if (is.na(ceiling_pct))
                ceiling_pct <- 1.0
              
              current_share <- rec_yds_allocation[i] / team_pass_yds
              room <- ceiling_pct - current_share
              
              if (room > 0.001) {
                # Has room
                players_with_room <- c(players_with_room, i)
                room_amounts <- c(room_amounts, room)
              }
            }
          }
          
          # If no one has room, we're done (better to under-allocate than break ceilings)
          if (length(players_with_room) == 0)
            break
          
          # Give 1 yard to a random player with room (equal probability)
          selected <- sample(players_with_room, 1)
          rec_yds_allocation[selected] <- rec_yds_allocation[selected] + 1
        }
      }
    }
    
    # DYNAMIC YPR ADJUSTMENT: Adjust YPR based on actual yards vs expected
    # High yardage games likely had explosive plays (higher YPR)
    adjusted_ypr_values <- ypr_values  # Start with historical
    
    for (i in 1:n_receivers) {
      if (rec_yds_allocation[i] > 0) {
        # Calculate expected yards for this player based on P50
        p50_pct <- receiving_data$Pct_P50[i]
        if (is.na(p50_pct) || p50_pct <= 0)
          p50_pct <- 0.01
        
        expected_yds <- team_pass_yds * p50_pct
        
        if (expected_yds > 0) {
          # Calculate how actual compares to expected
          yards_ratio <- rec_yds_allocation[i] / expected_yds
          
          # If significantly OVER expected (explosive game), increase YPR
          if (yards_ratio > 1.3) {
            # Scale factor: the more explosive, the higher YPR
            # 1.3x expected = 1.09x YPR
            # 2.0x expected = 1.21x YPR
            # 3.0x expected = 1.51x YPR
            ypr_multiplier <- 1 + (yards_ratio - 1) * 0.3
            
            # Cap at 2.0x historical YPR
            ypr_multiplier <- min(ypr_multiplier, 2.0)
            
            adjusted_ypr_values[i] <- ypr_values[i] * ypr_multiplier
            
          } else if (yards_ratio < 0.7) {
            # If UNDER expected (possession/short-catch role), decrease YPR slightly
            # 0.7x expected = 0.97x YPR
            # 0.5x expected = 0.95x YPR
            ypr_multiplier <- 0.9 + (yards_ratio * 0.1)
            ypr_multiplier <- max(ypr_multiplier, 0.7)  # Floor at 70%
            
            adjusted_ypr_values[i] <- ypr_values[i] * ypr_multiplier
          }
          # If yards_ratio between 0.7-1.3, no adjustment (normal game)
        }
      }
    }
    
    # Second pass: Allocate receptions using POISSON DISTRIBUTION
    # This prevents unrealistic combinations like "4 catches for 1 yard"
    rec_allocation <- rep(0, n_receivers)
    
    if (team_total_recs > 0) {
      # Step 1: Calculate expected catches for each player based on their yards and historical YPR
      expected_catches <- numeric(n_receivers)
      
      for (i in 1:n_receivers) {
        if (rec_yds_allocation[i] > 0) {
          # Expected catches = yards / YPR
          expected_catches[i] <- rec_yds_allocation[i] / adjusted_ypr_values[i]
          expected_catches[i] <- max(1, expected_catches[i])  # Min 1 if they have yards
          
          # Sample from Poisson distribution for realistic variance
          simulated_catches <- rpois(1, lambda = expected_catches[i])
          simulated_catches <- max(1, simulated_catches)  # Ensure at least 1
          
          # Cap at reasonable maximum (prevent outliers like 50 catches)
          max_realistic <- ceiling(expected_catches[i] * 1.5)
          rec_allocation[i] <- min(simulated_catches, max_realistic)
        }
      }
      
      # Step 2: Scale to match team total receptions
      actual_total <- sum(rec_allocation)
      
      # If we're over team total, remove catches intelligently
      while (sum(rec_allocation) > team_total_recs) {
        # Find player who can best afford to lose a catch
        # (high catch count, maintains reasonable YPR after reduction)
        cushion <- numeric(n_receivers)
        for (i in 1:n_receivers) {
          if (rec_allocation[i] > 1 && rec_yds_allocation[i] > 0) {
            # How far above minimum can we go?
            new_ypr_if_reduced <- rec_yds_allocation[i] / (rec_allocation[i] - 1)
            # Prefer reducing from players with low YPR impact
            cushion[i] <- rec_allocation[i] - 1
          } else {
            cushion[i] <- -999  # Don't reduce below 1
          }
        }
        
        if (max(cushion) <= 0)
          break  # Can't reduce any more
        
        reduce_idx <- which.max(cushion)
        rec_allocation[reduce_idx] <- rec_allocation[reduce_idx] - 1
      }
      
      # If we're under team total, add catches intelligently
      while (sum(rec_allocation) < team_total_recs) {
        # Find players who could plausibly have more catches
        room <- numeric(n_receivers)
        for (i in 1:n_receivers) {
          if (rec_yds_allocation[i] > 0) {
            # Max realistic catches = yards / (YPR * 0.7) - generous allowance for short catches
            max_plausible <- ceiling(rec_yds_allocation[i] / (adjusted_ypr_values[i] * 0.7))
            room[i] <- max(0, max_plausible - rec_allocation[i])
          } else {
            room[i] <- 0  # Don't add to players with 0 yards
          }
        }
        
        if (sum(room) == 0)
          break  # No room to add
        
        # Add to player weighted by room available
        add_probs <- room / sum(room)
        add_idx <- sample(1:n_receivers, 1, prob = add_probs)
        rec_allocation[add_idx] <- rec_allocation[add_idx] + 1
      }
      
      # Step 3: FINAL VALIDATION - ensure no impossible combinations
      for (i in 1:n_receivers) {
        # Yards > 0 ??? catches >= 1
        if (rec_yds_allocation[i] > 0 && rec_allocation[i] == 0) {
          rec_allocation[i] <- 1
        }
        
        # Prevent absurd YPR (catches way too high for yards)
        if (rec_allocation[i] > 0 && rec_yds_allocation[i] > 0) {
          actual_ypr <- rec_yds_allocation[i] / rec_allocation[i]
          # If YPR drops below 3.0, that's too many catches for the yards
          if (actual_ypr < 3.0) {
            # Reduce catches to maintain at least 3.0 YPR
            rec_allocation[i] <- max(1, floor(rec_yds_allocation[i] / 3.0))
          }
        }
      }
    }
    
    # Third pass: Allocate receiving TDs with 70% input rate / 30% production + diminishing returns
    td_allocation <- rep(0, n_receivers)
    if (team_pass_tds > 0) {
      # Historical TD rates (70% weight)
      td_rates <- as.numeric(receiving_data$TD_Rate)
      td_rates[is.na(td_rates)] <- 0
      td_rates[td_rates < 0] <- 0
      
      # Production in this sim (30% weight)
      # Combine yards and receptions
      production_weight <- (rec_yds_allocation / max(rec_yds_allocation, 1)) * 0.5 +
        (rec_allocation / max(rec_allocation, 1)) * 0.5
      
      # 70/30 split
      td_probs <- (td_rates * 0.7) + (production_weight * 0.3)
      
      # Only players who caught passes are eligible
      eligible <- which(rec_allocation > 0)
      
      if (length(eligible) > 0 && sum(td_probs[eligible]) > 0) {
        td_probs_eligible <- td_probs[eligible]
        td_probs_eligible <- td_probs_eligible / sum(td_probs_eligible)
        
        # Allocate TDs with diminishing returns
        for (td in 1:team_pass_tds) {
          selected_idx <- sample(1:length(eligible), 1, prob = td_probs_eligible)
          selected <- eligible[selected_idx]
          
          td_allocation[selected] <- td_allocation[selected] + 1
          
          # AGGRESSIVE diminishing returns: Each TD drastically reduces probability
          # After 1 TD: 0.3x probability
          # After 2 TDs: 0.09x probability (basically impossible to get 3rd)
          reduction_factor <- 0.3^td_allocation[selected]
          td_probs_eligible[selected_idx] <- td_probs_eligible[selected_idx] * reduction_factor
          
          if (sum(td_probs_eligible) > 0) {
            td_probs_eligible <- td_probs_eligible / sum(td_probs_eligible)
          } else {
            td_probs_eligible <- rep(1 / length(eligible), length(eligible))
          }
        }
      }
    }
    
    # CONSTRAINT: If player has rec TDs, ensure catches >= TDs
    for (i in 1:n_receivers) {
      if (td_allocation[i] > 0 && rec_allocation[i] < td_allocation[i]) {
        needed_catches <- td_allocation[i] - rec_allocation[i]
        rec_allocation[i] <- rec_allocation[i] + needed_catches
        
        # Take from others proportionally
        for (take in 1:needed_catches) {
          eligible_to_reduce <- which(rec_allocation > td_allocation &
                                        (1:n_receivers) != i)
          if (length(eligible_to_reduce) > 0) {
            excess <- rec_allocation[eligible_to_reduce] - td_allocation[eligible_to_reduce]
            max_idx <- eligible_to_reduce[which.max(excess)]
            rec_allocation[max_idx] <- rec_allocation[max_idx] - 1
          } else {
            break
          }
        }
      }
    }
    
    # FINAL VALIDATION
    for (i in 1:n_receivers) {
      # Yards > 0 ??? catches >= 1
      if (rec_yds_allocation[i] > 0 && rec_allocation[i] == 0) {
        rec_allocation[i] <- 1
      }
      # TDs > 0 ??? catches >= TDs
      if (td_allocation[i] > 0 &&
          rec_allocation[i] < td_allocation[i]) {
        rec_allocation[i] <- td_allocation[i]
      }
    }
    
    # Now create player results
    for (i in 1:n_receivers) {
      player_name <- receiving_data$Player[i]
      existing_idx <- which(sapply(all_player_results, function(x)
        x$Player == player_name))
      
      if (length(existing_idx) > 0) {
        all_player_results[[existing_idx[1]]]$Recs <- as.integer(rec_allocation[i])
        all_player_results[[existing_idx[1]]]$RecYds <- rec_yds_allocation[i]
        all_player_results[[existing_idx[1]]]$RecTDs <- as.integer(td_allocation[i])
      } else {
        all_player_results[[result_idx]] <- list(
          SimID = sim_id,
          Team = team_name,
          Player = player_name,
          PassYds = 0,
          PassTDs = 0L,
          INTs = 0L,
          RushYds = 0,
          RushTDs = 0L,
          Recs = as.integer(rec_allocation[i]),
          RecYds = rec_yds_allocation[i],
          RecTDs = as.integer(td_allocation[i]),
          FGsMade = 0L,
          FG_Under30 = 0L,
          FG_30_39 = 0L,
          FG_40_49 = 0L,
          FG_50Plus = 0L,
          XPs = 0L,
          FumLost = 0L
        )
        result_idx <- result_idx + 1
      }
    }
  }
  
  # PASSING
  passing_data <- team_data$passing
  
  if (nrow(passing_data) > 0) {
    for (i in 1:nrow(passing_data)) {
      pass_share <- passing_data$Pass_Share[i]
      if (is.na(pass_share))
        pass_share <- 0
      
      player_pass_yds <- round(team_pass_yds * pass_share)
      player_pass_tds <- round(team_pass_tds * pass_share)
      
      player_name <- passing_data$Player[i]
      existing_idx <- which(sapply(all_player_results, function(x)
        x$Player == player_name))
      
      if (length(existing_idx) > 0) {
        all_player_results[[existing_idx[1]]]$PassYds <- player_pass_yds
        all_player_results[[existing_idx[1]]]$PassTDs <- player_pass_tds
        all_player_results[[existing_idx[1]]]$INTs <- as.integer(opponent_ints)
      } else {
        all_player_results[[result_idx]] <- list(
          SimID = sim_id,
          Team = team_name,
          Player = player_name,
          PassYds = player_pass_yds,
          PassTDs = player_pass_tds,
          INTs = as.integer(opponent_ints),
          RushYds = 0,
          RushTDs = 0L,
          Recs = 0L,
          RecYds = 0,
          RecTDs = 0L,
          FGsMade = 0L,
          FG_Under30 = 0L,
          FG_30_39 = 0L,
          FG_40_49 = 0L,
          FG_50Plus = 0L,
          XPs = 0L,
          FumLost = 0L
        )
        result_idx <- result_idx + 1
      }
    }
  }
  
  # KICKING
  kicking_data <- team_data$kicking
  total_xps <- team_rush_tds + team_pass_tds
  
  # Get kicker name from kicking sheet
  kicker <- if (nrow(kicking_data) > 0 &&
                "Kicker" %in% names(kicking_data)) {
    kicking_data$Kicker[1]
  } else {
    paste0(team_name, " K")
  }
  
  fg_under30 <- 0L
  fg_30_39 <- 0L
  fg_40_49 <- 0L
  fg_50plus <- 0L
  
  if (team_fgs > 0) {
    distance_probs <- as.numeric(kicking_data$Percentage) / 100
    for (fg in 1:team_fgs) {
      distance_cat <- sample(1:4, 1, prob = distance_probs)
      if (distance_cat == 1)
        fg_under30 <- fg_under30 + 1L
      else if (distance_cat == 2)
        fg_30_39 <- fg_30_39 + 1L
      else if (distance_cat == 3)
        fg_40_49 <- fg_40_49 + 1L
      else
        fg_50plus <- fg_50plus + 1L
    }
  }
  
  all_player_results[[result_idx]] <- list(
    SimID = sim_id,
    Team = team_name,
    Player = kicker,
    PassYds = 0,
    PassTDs = 0L,
    INTs = 0L,
    RushYds = 0,
    RushTDs = 0L,
    Recs = 0L,
    RecYds = 0,
    RecTDs = 0L,
    FGsMade = as.integer(team_fgs),
    FG_Under30 = fg_under30,
    FG_30_39 = fg_30_39,
    FG_40_49 = fg_40_49,
    FG_50Plus = fg_50plus,
    XPs = as.integer(total_xps),
    FumLost = 0L
  )
  
  # Convert to data.table
  results <- rbindlist(all_player_results, use.names = TRUE, fill = TRUE)
  
  # Calculate fantasy points (same as CFB version)
  results[, `:=`(
    PassPts = (PassYds * 0.04) + (PassTDs * 4) - INTs + ifelse(PassYds >= 300, 3, 0),
    RushPts = (RushYds * 0.1) + (RushTDs * 6) + ifelse(RushYds >= 100, 3, 0),
    RecPts = Recs + (RecYds * 0.1) + (RecTDs * 6) + ifelse(RecYds >= 100, 3, 0),
    KickPts = XPs + (FG_Under30 * 3) + (FG_30_39 * 3) + (FG_40_49 * 4) + (FG_50Plus * 5),
    FumPts = FumLost * -1
  )]
  
  results[, TotalPts := PassPts + RushPts + RecPts + KickPts + FumPts]
  
  return(results)
}
run_simulations <- function(input_data, n_sims) {
  cat(sprintf(
    "\n=== RUNNING %s SIMULATIONS ===\n",
    format(n_sims, big.mark = ",")
  ))
  
  cat("\n=== DEBUG: Inside run_simulations ===\n")
  cat("input_data names:", paste(names(input_data), collapse=", "), "\n")
  team_names <- input_data$team_names
  cat("team_names:", paste(team_names, collapse=" vs "), "\n")
  similar_games <- input_data$similar_games
  dk_salaries <- input_data$dk_salaries
  cat("similar_games class:", class(similar_games), "\n")
  cat("similar_games rows:", nrow(similar_games), "\n")
  
  team1_data <- input_data[[team_names[1]]]
  cat("dk_salaries class:", class(dk_salaries), "\n")
  cat("dk_salaries rows:", nrow(dk_salaries), "\n")
  cat("dk_salaries columns:", paste(names(dk_salaries), collapse=", "), "\n")
  cat("First Team:", dk_salaries$Team[1], "\n")
  cat("=== DEBUG: End initial setup ===\n\n")
  team2_data <- input_data[[team_names[2]]]
  
  # Detect DST scoring (NFL vs CFB)
  use_dst <- has_dst_scoring(similar_games, team_names)
  
  cat(sprintf("Teams: %s vs %s\n", team_names[1], team_names[2]))
  cat(sprintf("Similar games: %d\n", nrow(similar_games)))
  
  if (use_dst) {
    cat("??? DST scoring detected (NFL mode)\n\n")
  } else {
    cat("??? No DST data (CFB mode)\n\n")
  }
  
  # Pre-allocate (2 teams + 2 DST if NFL)
  list_size <- if (use_dst)
    n_sims * 4
  else
    n_sims * 2
  all_results <- vector("list", list_size)
  result_idx <- 1
  
  # Pre-fetch DST names and column names if using DST (avoid repeated operations in loop)
  team1_dst_name <- NULL
  team2_dst_name <- NULL
  def1_sacks_col <- NULL
  def1_ints_col <- NULL
  def1_fum_col <- NULL
  def1_pts_col <- NULL
  def2_sacks_col <- NULL
  def2_ints_col <- NULL
  def2_fum_col <- NULL
  def2_pts_col <- NULL
  
  if (use_dst) {
    # Pre-fetch DST names
    team1_dst_name <- dk_salaries %>%
      filter(Team == team_names[1], Pos == "DST") %>%
      pull(Name)
    if (length(team1_dst_name) == 0)
      team1_dst_name <- paste0(team_names[1], " DST")
    else
      team1_dst_name <- team1_dst_name[1]
    
    team2_dst_name <- dk_salaries %>%
      filter(Team == team_names[2], Pos == "DST") %>%
      pull(Name)
    if (length(team2_dst_name) == 0)
      team2_dst_name <- paste0(team_names[2], " DST")
    else
      team2_dst_name <- team2_dst_name[1]
    
    # Pre-build column names
    def1_sacks_col <- paste0(team_names[1], "_Def_Sacks")
    def1_ints_col <- paste0(team_names[1], "_Def_Ints")
    def1_fum_col <- paste0(team_names[1], "_Def_Fum")
    def1_pts_col <- paste0(team_names[1], "_Def_Pts_Allow")
    def1_tds_col <- paste0(team_names[1], "_Def_TDs")
    def1_safeties_col <- paste0(team_names[1], "_Def_Safeties")
    
    def2_sacks_col <- paste0(team_names[2], "_Def_Sacks")
    def2_ints_col <- paste0(team_names[2], "_Def_Ints")
    def2_fum_col <- paste0(team_names[2], "_Def_Fum")
    def2_pts_col <- paste0(team_names[2], "_Def_Pts_Allow")
    def2_tds_col <- paste0(team_names[2], "_Def_TDs")
    def2_safeties_col <- paste0(team_names[2], "_Def_Safeties")
  }
  
  batch_size <- 500
  n_batches <- ceiling(n_sims / batch_size)
  start_time <- Sys.time()
  
  for (batch in 1:n_batches) {
    start_sim <- (batch - 1) * batch_size + 1
    end_sim <- min(batch * batch_size, n_sims)
    
    for (sim in start_sim:end_sim) {
      sampled_game <- similar_games[sample(nrow(similar_games), 1), ]
      
      # Extract opponent INTs first (if using DST)
      team1_opponent_ints <- 0
      team2_opponent_ints <- 0
      
      if (use_dst) {
        team1_dst_ints <- as.numeric(sampled_game[[def1_ints_col]])
        team2_dst_ints <- as.numeric(sampled_game[[def2_ints_col]])
        if (is.na(team1_dst_ints))
          team1_dst_ints <- 0
        if (is.na(team2_dst_ints))
          team2_dst_ints <- 0
        
        # Team1's defense INTs ??? Team2's QB threw them
        team2_opponent_ints <- team1_dst_ints
        # Team2's defense INTs ??? Team1's QB threw them
        team1_opponent_ints <- team2_dst_ints
      }
      
      team1_result <- simulate_team_game(
        sim,
        team_names[1],
        team1_data,
        sampled_game,
        dk_salaries,
        similar_games,
        use_dst,
        team1_opponent_ints
      )
      all_results[[result_idx]] <- team1_result
      result_idx <- result_idx + 1
      
      team2_result <- simulate_team_game(
        sim,
        team_names[2],
        team2_data,
        sampled_game,
        dk_salaries,
        similar_games,
        use_dst,
        team2_opponent_ints
      )
      all_results[[result_idx]] <- team2_result
      result_idx <- result_idx + 1
      
      # CREATE DST PLAYERS (NFL MODE)
      if (use_dst) {
        # Extract remaining defensive stats (INTs already extracted above for QB assignment)
        team1_dst_sacks <- as.numeric(sampled_game[[def1_sacks_col]])
        team1_dst_fum <- as.numeric(sampled_game[[def1_fum_col]])
        team1_dst_pts_allow <- as.numeric(sampled_game[[def1_pts_col]])
        team1_dst_def_tds <- as.numeric(sampled_game[[def1_tds_col]])
        team1_dst_safeties <- as.numeric(sampled_game[[def1_safeties_col]])
        
        team2_dst_sacks <- as.numeric(sampled_game[[def2_sacks_col]])
        team2_dst_fum <- as.numeric(sampled_game[[def2_fum_col]])
        team2_dst_pts_allow <- as.numeric(sampled_game[[def2_pts_col]])
        team2_dst_def_tds <- as.numeric(sampled_game[[def2_tds_col]])
        team2_dst_safeties <- as.numeric(sampled_game[[def2_safeties_col]])
        
        # Validate (INTs already validated above)
        if (is.na(team1_dst_sacks))
          team1_dst_sacks <- 0
        if (is.na(team1_dst_fum))
          team1_dst_fum <- 0
        if (is.na(team1_dst_pts_allow))
          team1_dst_pts_allow <- 20
        if (is.na(team1_dst_def_tds))
          team1_dst_def_tds <- 0
        if (is.na(team1_dst_safeties))
          team1_dst_safeties <- 0
        if (is.na(team2_dst_sacks))
          team2_dst_sacks <- 0
        if (is.na(team2_dst_fum))
          team2_dst_fum <- 0
        if (is.na(team2_dst_pts_allow))
          team2_dst_pts_allow <- 20
        if (is.na(team2_dst_def_tds))
          team2_dst_def_tds <- 0
        if (is.na(team2_dst_safeties))
          team2_dst_safeties <- 0
        
        # PRE-CALCULATE DST fantasy points (FAST - no vectorization needed)
        team1_pts_allow_score <- if (team1_dst_pts_allow == 0)
          10
        else
          if (team1_dst_pts_allow <= 6)
            7
        else
          if (team1_dst_pts_allow <= 13)
            4
        else
          if (team1_dst_pts_allow <= 20)
            1
        else
          if (team1_dst_pts_allow <= 27)
            0
        else
          if (team1_dst_pts_allow <= 34)
            - 1
        else-4
        
        team2_pts_allow_score <- if (team2_dst_pts_allow == 0)
          10
        else
          if (team2_dst_pts_allow <= 6)
            7
        else
          if (team2_dst_pts_allow <= 13)
            4
        else
          if (team2_dst_pts_allow <= 20)
            1
        else
          if (team2_dst_pts_allow <= 27)
            0
        else
          if (team2_dst_pts_allow <= 34)
            - 1
        else-4
        
        team1_dst_total <- (team1_dst_sacks * 1) + (team1_dst_ints * 2) + (team1_dst_fum * 2) +
          team1_pts_allow_score + (team1_dst_def_tds * 6) + (team1_dst_safeties * 2)
        team2_dst_total <- (team2_dst_sacks * 1) + (team2_dst_ints * 2) + (team2_dst_fum * 2) +
          team2_pts_allow_score + (team2_dst_def_tds * 6) + (team2_dst_safeties * 2)
        
        # Store DST with pre-calculated points
        team1_dst_result <- data.table(
          SimID = sim,
          Team = team_names[1],
          Player = team1_dst_name,
          PassYds = 0,
          PassTDs = 0L,
          INTs = 0L,
          RushYds = 0,
          RushTDs = 0L,
          Recs = 0L,
          RecYds = 0,
          RecTDs = 0L,
          FGsMade = 0L,
          FG_Under30 = 0L,
          FG_30_39 = 0L,
          FG_40_49 = 0L,
          FG_50Plus = 0L,
          XPs = 0L,
          FumLost = 0L,
          PassPts = 0,
          RushPts = 0,
          RecPts = 0,
          KickPts = 0,
          FumPts = 0,
          DSTPts = team1_dst_total,
          TotalPts = team1_dst_total
        )
        
        team2_dst_result <- data.table(
          SimID = sim,
          Team = team_names[2],
          Player = team2_dst_name,
          PassYds = 0,
          PassTDs = 0L,
          INTs = 0L,
          RushYds = 0,
          RushTDs = 0L,
          Recs = 0L,
          RecYds = 0,
          RecTDs = 0L,
          FGsMade = 0L,
          FG_Under30 = 0L,
          FG_30_39 = 0L,
          FG_40_49 = 0L,
          FG_50Plus = 0L,
          XPs = 0L,
          FumLost = 0L,
          PassPts = 0,
          RushPts = 0,
          RecPts = 0,
          KickPts = 0,
          FumPts = 0,
          DSTPts = team2_dst_total,
          TotalPts = team2_dst_total
        )
        
        all_results[[result_idx]] <- team1_dst_result
        result_idx <- result_idx + 1
        all_results[[result_idx]] <- team2_dst_result
        result_idx <- result_idx + 1
        
        # (INTs now assigned directly to QB in simulate_team_game function)
      }
    }
    
    if (batch %% 5 == 0 || batch == n_batches) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      sims_done <- end_sim
      rate <- sims_done / elapsed
      eta <- (n_sims - sims_done) / rate
      
      cat(
        sprintf(
          "Batch %d/%d: %d/%d sims (%.1f%%) | %.0f sims/sec | ETA: %.0fs\n",
          batch,
          n_batches,
          sims_done,
          n_sims,
          (sims_done / n_sims) * 100,
          rate,
          eta
        )
      )
    }
    
    if (batch %% 10 == 0)
      gc(verbose = FALSE, full = FALSE)
  }
  
  combined <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
  
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(
    sprintf(
      "\n??? Complete! %.1f seconds (%.0f sims/sec)\n\n",
      total_time,
      n_sims / total_time
    )
  )
  
  return(combined)
}

# ============================================================================
# WRAPPER FOR UNIVERSAL APP
# ============================================================================

run_nfl_simulation <- function(input_data, n_sims, config, progress_callback = NULL) {
  
  cat("\n=== DEBUG: Starting NFL simulation wrapper ===\n")
  
  # Check if we need to load team sheets (universal app only loads required_sheets)
  if (is.null(input_data$team_names) && !any(grepl("_Rushing$", names(input_data)))) {
    cat("DEBUG: Team sheets not loaded, need to load them from file\n")
    
    # Get file path - this is a hack but needed since universal app doesn't load all sheets
    # The input_data should have a file_path attribute, but if not, we're stuck
    stop("ERROR: Team sheets (NE_Rushing, SEA_Rushing, etc.) were not loaded. 
         The universal app needs to be updated to load ALL sheets for NFL, not just required_sheets.
         Add 'load_all_sheets = TRUE' handling to the app's file loading code.")
  }
  
  # Restructure if needed
  if (is.null(input_data$team_names)) {
    sheet_names <- names(input_data)
    cat("Sheet names loaded:", paste(sheet_names, collapse=", "), "\n")
    
    if (any(grepl("_Rushing$", sheet_names))) {
      rushing_sheets <- grep("_Rushing$", sheet_names, value = TRUE)
      team_names <- gsub("_Rushing$", "", rushing_sheets)
      
      cat("Detected teams:", paste(team_names, collapse=" vs "), "\n")
      
      restructured <- list()
      restructured$team_names <- team_names
      
      for (team in team_names) {
        cat("Loading data for team:", team, "\n")
        restructured[[team]] <- list(
          rushing = as.data.frame(input_data[[paste0(team, "_Rushing")]]),
          receiving = as.data.frame(input_data[[paste0(team, "_Receiving")]]),
          passing = as.data.frame(input_data[[paste0(team, "_Passing")]]),
          kicking = as.data.frame(input_data[[paste0(team, "_Kicking")]])
        )
      }
      
      cat("Loading Salaries sheet...\n")
      restructured$salaries <- as.data.frame(input_data$Salaries)
      cat("Salaries columns:", paste(names(restructured$salaries), collapse=", "), "\n")
      cat("Salaries rows:", nrow(restructured$salaries), "\n")
      
      cat("Loading Similar_Games sheet...\n")
      restructured$similar_games <- as.data.frame(input_data$Similar_Games)
      cat("Similar_Games rows:", nrow(restructured$similar_games), "\n")
      
      restructured$dk_salaries <- as.data.frame(input_data$Salaries)
      cat("dk_salaries set with", nrow(restructured$dk_salaries), "rows\n")
      
      input_data <- restructured
    }
  } else {
    cat("team_names already exists in input_data\n")
    input_data$dk_salaries <- as.data.frame(input_data$salaries)
  }
  
  cat("Calling run_simulations with n_sims =", n_sims, "\n")
  cat("=== DEBUG: End of wrapper setup ===\n\n")
  
  # Run the original simulation function
  all_results <- run_simulations(input_data, n_sims)
  
  # Rename TotalPts to DKScore and FDScore
  setDT(all_results)
  all_results[, DKScore := TotalPts]
  all_results[, FDScore := TotalPts]
  
  # Calculate player averages from simulations
  player_stats <- all_results[, .(
    Sim_DK_Mean = mean(DKScore),
    Sim_DK_Median = median(DKScore),
    Sim_DK_StdDev = sd(DKScore),
    Sim_DK_Min = min(DKScore),
    Sim_DK_Max = max(DKScore),
    Sim_FD_Mean = mean(FDScore),
    Sim_FD_Median = median(FDScore),
    Sim_FD_StdDev = sd(FDScore)
  ), by = .(Player, Team)]
  
  # Create metadata from salaries (just base player info for sim display)
  salaries_dt <- as.data.table(input_data$salaries)
  metadata <- unique(salaries_dt[, .(
    Player = Name,
    Team = Team,
    Pos = Pos,
    DKSalary = as.numeric(DKSal),
    DKID = as.character(DKID),
    DKOwn = as.numeric(DKFOwn),
    FDSalary = as.numeric(FDSal),
    FDID = as.character(FDID),
    FDOwn = as.numeric(FDFOwn),
    # Captain/MVP columns for lineup optimizer
    DKCID = as.character(DKCID),
    DKCSalary = as.numeric(DKCSal),
    DKCOwn = as.numeric(DKCOwn),
    FDMSalary = as.numeric(FDMSal),
    FDMOwn = as.numeric(FDMOwn),
    # ETR projections
    ETR_DK = as.numeric(ETR_DK),
    ETR_FD = as.numeric(ETR_FD)
  )])
  
  # Merge sim stats with metadata
  projections <- merge(metadata, player_stats, by = c("Player", "Team"), all.x = TRUE)
  
  # Calculate variance metrics
  projections[, DK_Variance := Sim_DK_Mean - ETR_DK]
  projections[, DK_Pct_Diff := round((DK_Variance / ETR_DK) * 100, 1)]
  projections[, FD_Variance := Sim_FD_Mean - ETR_FD]
  projections[, FD_Pct_Diff := round((FD_Variance / ETR_FD) * 100, 1)]
  
  # Calculate value (points per $1K)
  projections[, DK_Value_ETR := round(ETR_DK / (DKSalary / 1000), 2)]
  projections[, DK_Value_Sim := round(Sim_DK_Mean / (DKSalary / 1000), 2)]
  projections[, FD_Value_ETR := round(ETR_FD / (FDSalary / 1000), 2)]
  projections[, FD_Value_Sim := round(Sim_FD_Mean / (FDSalary / 1000), 2)]
  
  # Sort by DK variance (biggest differences first)
  setorder(projections, -DK_Variance)
  
  return(list(
    sim_results = all_results,
    metadata = metadata,
    projections = projections
  ))
}