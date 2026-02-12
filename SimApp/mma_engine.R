# ============================================================================
# MMA SIMULATION ENGINE
# Vectorized, optimized simulation for MMA/UFC
# ============================================================================

library(data.table)

# Win bonus lookup tables
DK_WIN_BONUSES <- list(
  "QuickWin_R1" = 115,
  "R1" = 90,
  "R2" = 70,
  "R3" = 45,
  "R4" = 40,
  "R5" = 40,
  "Decision_R3" = 30,
  "Decision_R5" = 30,
  "Decision" = 30
)

FD_WIN_BONUSES <- list(
  "QuickWin_R1" = 100,
  "R1" = 100,
  "R2" = 75,
  "R3" = 50,
  "R4" = 35,
  "R5" = 25,
  "Decision_R3" = 20,
  "Decision_R5" = 20,
  "Decision" = 20
)


# ============================================================================
# HELPER: Get win bonus
# ============================================================================
get_win_bonus <- function(outcome, platform = "DK") {
  bonuses <- if(platform == "DK") DK_WIN_BONUSES else FD_WIN_BONUSES
  
  if(outcome %in% names(bonuses)) {
    return(bonuses[[outcome]])
  }
  
  return(0)
}


# ============================================================================
# VECTORIZED PERCENTILE INTERPOLATION
# ============================================================================

#' Sample from percentile distribution using linear interpolation
#' @param p5 5th percentile value
#' @param p10 10th percentile value
#' @param p25 25th percentile value
#' @param p50 50th percentile (median)
#' @param p75 75th percentile value
#' @param p90 90th percentile value
#' @param p95 95th percentile value
#' @param n Number of samples to generate
#' @return Vector of sampled values
sample_from_percentiles <- function(p5, p10, p25, p50, p75, p90, p95, n = 1) {
  percentiles <- runif(n, 0, 1)
  
  results <- numeric(n)
  
  for (i in 1:n) {
    pct <- percentiles[i]
    
    if (pct <= 0.05) {
      results[i] <- p5[i]
    } else if (pct <= 0.1) {
      results[i] <- p5[i] + (p10[i] - p5[i]) * (pct - 0.05) / 0.05
    } else if (pct <= 0.25) {
      results[i] <- p10[i] + (p25[i] - p10[i]) * (pct - 0.1) / 0.15
    } else if (pct <= 0.5) {
      results[i] <- p25[i] + (p50[i] - p25[i]) * (pct - 0.25) / 0.25
    } else if (pct <= 0.75) {
      results[i] <- p50[i] + (p75[i] - p50[i]) * (pct - 0.5) / 0.25
    } else if (pct <= 0.9) {
      results[i] <- p75[i] + (p90[i] - p75[i]) * (pct - 0.75) / 0.15
    } else if (pct <= 0.95) {
      results[i] <- p90[i] + (p95[i] - p90[i]) * (pct - 0.9) / 0.05
    } else {
      results[i] <- p95[i]
    }
  }
  
  return(results)
}


# ============================================================================
# FIGHT PAIR CREATION
# ============================================================================

#' Create fight pairs from fights data
#' @param fights_data Data table of fighter information
#' @return Data table of fight pairs
create_fight_pairs <- function(fights_data) {
  setDT(fights_data)
  
  # Create fight pairs based on opponent matching
  pairs <- data.table()
  processed <- character(0)
  
  for (i in 1:nrow(fights_data)) {
    fighter <- fights_data$Name[i]
    
    # Skip if already processed
    if (fighter %in% processed) next
    
    opponent <- fights_data$Opponent[i]
    
    # Find opponent's row
    opponent_row <- fights_data[Name == opponent]
    
    if (nrow(opponent_row) > 0) {
      pair <- data.table(
        Fighter1 = fighter,
        Fighter2 = opponent,
        WeightClass = fights_data$WeightClass[i],
        Rounds = fights_data$Rounds[i]
      )
      
      pairs <- rbind(pairs, pair)
      processed <- c(processed, fighter, opponent)
    }
  }
  
  return(pairs)
}


# ============================================================================
# VECTORIZED OUTCOME SAMPLING
# ============================================================================

#' Sample outcomes for all fights across all simulations
#' @param fight_pairs Data table of fight pairs
#' @param scores_data Data table of outcome probabilities and scores
#' @param n_sims Number of simulations
#' @return Data table with fight outcomes for each simulation
sample_outcomes_vectorized <- function(fight_pairs, scores_data, n_sims) {
  
  n_fights <- nrow(fight_pairs)
  
  # Pre-allocate results
  all_outcomes <- vector("list", n_fights * n_sims)
  idx <- 1
  
  for (i in 1:n_fights) {
    fighter1 <- fight_pairs$Fighter1[i]
    fighter2 <- fight_pairs$Fighter2[i]
    
    # Get all possible outcomes for this fight
    fight_outcomes <- scores_data[
      (Winner == fighter1 & Loser == fighter2) | 
        (Winner == fighter2 & Loser == fighter1)
    ]
    
    if (nrow(fight_outcomes) == 0) next
    
    # Normalize probabilities
    fight_outcomes[, Winner_Prob := ifelse(is.na(Winner_Prob), 0, Winner_Prob)]
    total_prob <- sum(fight_outcomes$Winner_Prob)
    if (total_prob == 0) next
    fight_outcomes[, Winner_Prob := Winner_Prob / total_prob]
    
    # Sample outcomes for all sims at once
    outcome_indices <- sample(
      1:nrow(fight_outcomes), 
      size = n_sims, 
      replace = TRUE, 
      prob = fight_outcomes$Winner_Prob
    )
    
    # Store sampled outcomes
    for (sim in 1:n_sims) {
      all_outcomes[[idx]] <- data.table(
        SimID = sim,
        FightID = i,
        Fighter1 = fighter1,
        Fighter2 = fighter2,
        OutcomeRow = outcome_indices[sim]
      )
      idx <- idx + 1
    }
  }
  
  # Combine all outcomes
  outcome_table <- rbindlist(all_outcomes[1:(idx-1)])
  
  return(outcome_table)
}


# ============================================================================
# SCORE GENERATION
# ============================================================================

#' Generate scores for all sampled outcomes
#' @param outcome_table Table of sampled outcomes
#' @param scores_data Original scores data with percentile distributions
#' @param fight_pairs Fight pairs information
#' @return Data table with fighter scores by simulation
generate_scores_vectorized <- function(outcome_table, scores_data, fight_pairs) {
  
  # Join outcome table with actual outcome data
  setkey(outcome_table, FightID, OutcomeRow)
  
  # We need to add the actual outcome info to each row
  results <- vector("list", nrow(outcome_table))
  
  for (i in 1:nrow(outcome_table)) {
    sim_id <- outcome_table$SimID[i]
    fight_id <- outcome_table$FightID[i]
    fighter1 <- outcome_table$Fighter1[i]
    fighter2 <- outcome_table$Fighter2[i]
    outcome_idx <- outcome_table$OutcomeRow[i]
    
    # Get the fight outcomes for this fight
    fight_outcomes <- scores_data[
      (Winner == fighter1 & Loser == fighter2) | 
        (Winner == fighter2 & Loser == fighter1)
    ]
    
    if (nrow(fight_outcomes) == 0) next
    
    selected_outcome <- fight_outcomes[outcome_idx]
    
    winner <- selected_outcome$Winner
    loser <- selected_outcome$Loser
    outcome <- selected_outcome$Outcome
    
    # Sample scores from percentile distributions
    dk_winner_base <- sample_from_percentiles(
      selected_outcome$Winner_DK_Base_P5,
      selected_outcome$Winner_DK_Base_P10,
      selected_outcome$Winner_DK_Base_P25,
      selected_outcome$Winner_DK_Base_P50,
      selected_outcome$Winner_DK_Base_P75,
      selected_outcome$Winner_DK_Base_P90,
      selected_outcome$Winner_DK_Base_P95,
      n = 1
    )
    
    fd_winner_base <- sample_from_percentiles(
      selected_outcome$Winner_FD_Base_P5,
      selected_outcome$Winner_FD_Base_P10,
      selected_outcome$Winner_FD_Base_P25,
      selected_outcome$Winner_FD_Base_P50,
      selected_outcome$Winner_FD_Base_P75,
      selected_outcome$Winner_FD_Base_P90,
      selected_outcome$Winner_FD_Base_P95,
      n = 1
    )
    
    dk_loser_base <- sample_from_percentiles(
      selected_outcome$Loser_DK_Base_P5,
      selected_outcome$Loser_DK_Base_P10,
      selected_outcome$Loser_DK_Base_P25,
      selected_outcome$Loser_DK_Base_P50,
      selected_outcome$Loser_DK_Base_P75,
      selected_outcome$Loser_DK_Base_P90,
      selected_outcome$Loser_DK_Base_P95,
      n = 1
    )
    
    fd_loser_base <- sample_from_percentiles(
      selected_outcome$Loser_FD_Base_P5,
      selected_outcome$Loser_FD_Base_P10,
      selected_outcome$Loser_FD_Base_P25,
      selected_outcome$Loser_FD_Base_P50,
      selected_outcome$Loser_FD_Base_P75,
      selected_outcome$Loser_FD_Base_P90,
      selected_outcome$Loser_FD_Base_P95,
      n = 1
    )
    
    # Add win bonuses
    dk_win_bonus <- get_win_bonus(outcome, "DK")
    fd_win_bonus <- get_win_bonus(outcome, "FD")
    
    # Winner results
    results[[i*2 - 1]] <- data.table(
      SimID = sim_id,
      Player = winner,
      DKScore = dk_winner_base + dk_win_bonus,
      FDScore = fd_winner_base + fd_win_bonus
    )
    
    # Loser results
    results[[i*2]] <- data.table(
      SimID = sim_id,
      Player = loser,
      DKScore = dk_loser_base,
      FDScore = fd_loser_base
    )
  }
  
  # Combine all results
  all_results <- rbindlist(results)
  
  return(all_results)
}


# ============================================================================
# METADATA CREATION
# ============================================================================

#' Create metadata table from fights data
#' @param fights_data Original fights input data
#' @return Metadata table with all fighter information
#' Create metadata table from fights data
#' @param fights_data Original fights input data
#' @return Metadata table with all fighter information
create_mma_metadata <- function(fights_data) {
  setDT(fights_data)
  
  # Extract unique fighters with their information
  metadata <- unique(fights_data[, .(
    Player = Name,
    DKSalary = as.numeric(DKSalary),
    DKID = as.character(DKID),        # Keep as character
    DKOwn = as.numeric(DKOwn),
    FDSalary = as.numeric(FDSalary),
    FDID = as.character(FDID),        # Keep as character (has hyphens)
    FDOwn = as.numeric(FDOwn),
    CPTID = as.character(CPTID),      # Keep as character
    SDID = as.character(SDID),        # Keep as character
    SDSalary = as.numeric(SDSal)      # Numeric salary
  )])
  
  return(metadata)
}
# ============================================================================
# MAIN SIMULATION FUNCTION
# ============================================================================

#' Run MMA simulation
#' @param input_data List containing Fights and Scores sheets
#' @param n_sims Number of simulations to run
#' @param config Sport configuration (from sport_configs_universal.R)
#' @param progress_callback Optional callback function for progress updates
#' @return List with sim_results and metadata
run_mma_simulation <- function(input_data, n_sims, config, progress_callback = NULL) {
  
  if (!is.null(progress_callback)) {
    progress_callback("Preparing data...", 0.1)
  }
  
  # Extract input data
  fights_data <- as.data.table(input_data$Fights)
  scores_data <- as.data.table(input_data$Scores)
  
  # Convert character columns explicitly
  character_cols_fights <- c("Name", "Opponent", "DKID", "FDID", "CPTID", "SDID")
  
  for (col in character_cols_fights) {
    if (col %in% names(fights_data)) {
      fights_data[, (col) := as.character(get(col))]
    }
  }
  
  # Convert numeric columns
  numeric_cols_fights <- c(
    "DKSalary", "FDSalary", "DKOwn", "FDOwn", 
    "SDSal", "OriginalML", "DeViggedProb", 
    "R1", "QuickWin_R1", "R2", "R3", "R4", "R5", "Decision"
  )
  
  for (col in numeric_cols_fights) {
    if (col %in% names(fights_data)) {
      fights_data[, (col) := as.numeric(get(col))]
    }
  }
  
  numeric_cols_scores <- c(
    "Winner_Prob",
    "Winner_DK_Base_P5", "Winner_DK_Base_P10", "Winner_DK_Base_P25", "Winner_DK_Base_P50",
    "Winner_DK_Base_P75", "Winner_DK_Base_P90", "Winner_DK_Base_P95",
    "Winner_FD_Base_P5", "Winner_FD_Base_P10", "Winner_FD_Base_P25", "Winner_FD_Base_P50",
    "Winner_FD_Base_P75", "Winner_FD_Base_P90", "Winner_FD_Base_P95",
    "Loser_DK_Base_P5", "Loser_DK_Base_P10", "Loser_DK_Base_P25", "Loser_DK_Base_P50",
    "Loser_DK_Base_P75", "Loser_DK_Base_P90", "Loser_DK_Base_P95",
    "Loser_FD_Base_P5", "Loser_FD_Base_P10", "Loser_FD_Base_P25", "Loser_FD_Base_P50",
    "Loser_FD_Base_P75", "Loser_FD_Base_P90", "Loser_FD_Base_P95"
  )
  
  for (col in numeric_cols_scores) {
    if (col %in% names(scores_data)) {
      scores_data[, (col) := as.numeric(get(col))]
    }
  }
  
  # Create fight pairs
  fight_pairs <- create_fight_pairs(fights_data)
  
  if (!is.null(progress_callback)) {
    progress_callback("Sampling outcomes...", 0.3)
  }
  
  # Sample outcomes for all fights × all sims
  outcome_table <- sample_outcomes_vectorized(fight_pairs, scores_data, n_sims)
  
  if (!is.null(progress_callback)) {
    progress_callback("Generating scores...", 0.6)
  }
  
  # Generate scores
  sim_results <- generate_scores_vectorized(outcome_table, scores_data, fight_pairs)
  
  if (!is.null(progress_callback)) {
    progress_callback("Finalizing results...", 0.9)
  }
  
  # Create metadata
  metadata <- create_mma_metadata(fights_data)
  
  if (!is.null(progress_callback)) {
    progress_callback("Complete!", 1.0)
  }
  
  return(list(
    sim_results = sim_results,
    metadata = metadata
  ))
}