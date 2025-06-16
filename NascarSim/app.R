# app.R
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)
library(DT)
library(plotly)
library(lpSolve)
library(memoise)
library(shinycssloaders)
library(shinyjs)

# Set up custom CSS for black and orange theme
custom_css <- "
  /* Override dashboard header colors */
  .skin-blue .main-header {
    background-color: #000000;
    border-bottom: 2px solid #FFD700;
  }
  .skin-blue .main-header .logo {
    background-color: #000000;
    color: #FFD700;
    font-weight: bold;
  }
  .skin-blue .main-header .logo:hover {
    background-color: #333333;
    color: #FFD700;
  }
  .skin-blue .main-header .navbar {
    background-color: #000000;
  }
  
  /* Override dashboard sidebar colors */
  .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
    background-color: #1a1a1a;
  }
  .skin-blue .sidebar a {
    color: #FFD700;
  }
  .skin-blue .sidebar-menu > li.active > a, 
  .skin-blue .sidebar-menu > li:hover > a {
    color: #000000;
    background: #FFD700;
    border-left-color: #FFD700;
    font-weight: bold;
  }
  
  /* Customize box headers */
  .box.box-primary .box-header {
    background-color: #FFD700;
    color: #000000;
    font-weight: bold;
  }
  
  /* Style buttons */
  .btn-primary {
    background-color: #FFD700;
    border-color: #E6C200;
    color: #000000;
    font-weight: bold;
  }
  .btn-primary:hover, .btn-primary:focus {
    background-color: #E6C200;
    border-color: #CCAD00;
    color: #000000;
  }
  
  .btn-default {
    background-color: #333333;
    border-color: #FFD700;
    color: #FFD700;
  }
  .btn-default:hover {
    background-color: #FFD700;
    color: #000000;
  }
  /* Style download button groups */
.btn-group .btn {
  margin-right: 5px;
}

/* Last button in group shouldn't have margin */
.btn-group .btn:last-child {
  margin-right: 0;
}

"



read_input_file <- function(file_path) {
  tryCatch({
    # Read sheets needed by both platforms
    sheets <- list(
      Driver = read_excel(file_path, sheet = "Driver"),
      Race = read_excel(file_path, sheet = "Race")
    )
    
    # Try to read DraftKings specific sheet with the new name
    sheets$DKDom <- tryCatch(
      read_excel(file_path, sheet = "DKDom"),
      error = function(e) NULL
    )
    
    # Try to read optional FanDuel specific sheets
    sheets$FDDom <- tryCatch(
      read_excel(file_path, sheet = "FDDom"),
      error = function(e) NULL
    )
    
    sheets$FDLaps <- tryCatch(
      read_excel(file_path, sheet = "FDLaps"),
      error = function(e) NULL
    )
    
    # Identify available platforms based on sheets
    has_dk <- !is.null(sheets$DKDom) && "DKSalary" %in% colnames(sheets$Driver)
    has_fd <- (!is.null(sheets$FDDom) || !is.null(sheets$FDLaps)) && "FDSalary" %in% colnames(sheets$Driver)
    
    # Create platform info
    platform_info <- list(
      has_draftkings = has_dk,
      has_fanduel = has_fd
    )
    
    
    list(
      sheets = sheets,
      platform_info = platform_info
    )
  }, error = function(e) {
    stop(paste("Error reading Excel file:", e$message))
  })
}

# Process input data efficiently with data.table
process_input_data <- function(input_data) {
  # Extract data components
  driver_data <- input_data$sheets$Driver
  race_data <- input_data$sheets$Race
  dk_dom_data <- input_data$sheets$DKDom
  fd_dom_data <- input_data$sheets$FDDom
  fd_laps_data <- input_data$sheets$FDLaps
  
  # Process driver data
  processed_drivers <- as.data.table(driver_data)
  
  # Convert relevant numeric columns efficiently
  numeric_cols <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30", 
                    "DKSalary", "FDSalary", "DKOP", "FDOP", "Starting", 
                    "DKDomTier", "FDDomTier")
  
  for(col in numeric_cols) {
    if(col %in% names(processed_drivers)) {
      processed_drivers[, (col) := as.numeric(get(col))]
    }
  }
  
  # Process race data
  processed_race <- as.data.table(race_data)
  
  # Process DK dominator data if available
  processed_dk_dominator <- if(!is.null(dk_dom_data)) {
    dom_dt <- as.data.table(dk_dom_data)
    dom_dt <- dom_dt[!(is.na(PtLow) & is.na(PtHigh) & is.na(FinLow) & is.na(FinHigh) & 
                         is.na(OR) & is.na(TierMin) & is.na(TierMax))]
    
    dom_dt[, ProcessedRank := {
      sapply(Rank, function(x) {
        if(x == "Strategy") return(0)
        if(x == "DomDead") return(999)
        # Try to convert to numeric, return 999 if it fails
        num <- suppressWarnings(as.numeric(as.character(x)))
        if(is.na(num)) return(999) else return(num)
      })
    }]
    
    
    dom_dt[, OriginalRank := Rank]
    setorder(dom_dt, ProcessedRank)
    dom_dt
  } else data.table()
  
  
  # Process FD dominator data if available
  processed_fd_dominator <- if(!is.null(fd_dom_data)) {
    fd_dom_dt <- as.data.table(fd_dom_data)
    fd_dom_dt <- fd_dom_dt[!is.na(PtLow) | !is.na(PtHigh)]
    
    # Calculate ProcessedRank safely without warnings
    fd_dom_dt[, ProcessedRank := {
      sapply(Rank, function(x) {
        if(x == "Strategy") return(0)
        if(x == "DomDead") return(999)
        # Try to convert to numeric, return 999 if it fails
        num <- suppressWarnings(as.numeric(as.character(x)))
        if(is.na(num)) return(999) else return(num)
      })
    }]
    
    fd_dom_dt[, OriginalRank := Rank]
    
    # Ensure all required columns exist
    if(!"OR" %in% names(fd_dom_dt)) fd_dom_dt[, OR := "R"]
    if(!"TierMin" %in% names(fd_dom_dt)) fd_dom_dt[, TierMin := 1]
    if(!"TierMax" %in% names(fd_dom_dt)) fd_dom_dt[, TierMax := 3]
    if(!"FinLow" %in% names(fd_dom_dt)) fd_dom_dt[, FinLow := 1]
    if(!"FinHigh" %in% names(fd_dom_dt)) fd_dom_dt[, FinHigh := 35]
    
    setorder(fd_dom_dt, ProcessedRank)
    fd_dom_dt
  } else data.table()
  
  # Process FD laps data if available
  processed_fd_laps <- if(!is.null(fd_laps_data)) {
    fd_laps_dt <- as.data.table(fd_laps_data)
    
    # Only handle Pt column (no more high/low)
    fd_laps_dt <- fd_laps_dt[!is.na(Pt)]
    
    # Convert columns to numeric
    for(col in c("ps", "Pt")) {
      if(col %in% names(fd_laps_dt)) {
        fd_laps_dt[, (col) := as.numeric(get(col))]
      }
    }
    
    # Use position (ps) as finish position
    fd_laps_dt[, FinishLow := ps]
    fd_laps_dt[, FinishHigh := ps]
    
    setorder(fd_laps_dt, ps)
    fd_laps_dt
  } else data.table()
  
  # Extract dominator points values for both platforms
  dk_dom_points <- if("DKDom" %in% names(processed_race)) {
    processed_race$DKDom
  } else numeric(0)
  
  fd_dom_points <- if("FDDom" %in% names(processed_race)) {
    processed_race$FDDom
  } else numeric(0)
  
  # Return processed data
  list(
    drivers = processed_drivers,
    race = processed_race,
    dk_dominator = processed_dk_dominator,
    fd_dominator = processed_fd_dominator,
    fd_laps = processed_fd_laps,
    dk_dom_points = dk_dom_points,
    fd_dom_points = fd_dom_points
  )
}


# Optimized race simulation function
simulate_finishing_positions <- function(drivers_dt) {
  n_drivers <- nrow(drivers_dt)
  
  # Extract cumulative probabilities for each driver
  prob_cols <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
  
  # Generate performance scores for each driver
  performance_scores <- numeric(n_drivers)
  
  for (i in 1:n_drivers) {
    # Convert cumulative probabilities to marginal probabilities
    cum_probs <- c(
      drivers_dt$W[i],
      drivers_dt$T3[i],
      drivers_dt$T5[i],
      drivers_dt$T10[i],
      drivers_dt$T15[i],
      drivers_dt$T20[i],
      drivers_dt$T25[i],
      drivers_dt$T30[i],
      1.0  # Ensure probabilities sum to 1
    )
    
    # Handle NA values
    cum_probs[is.na(cum_probs)] <- 0
    
    # Convert to marginal probabilities
    pos_ranges <- list(
      1,        # W: Position 1
      2:3,      # T3: Positions 2-3
      4:5,      # T5: Positions 4-5
      6:10,     # T10: Positions 6-10
      11:15,    # T15: Positions 11-15
      16:20,    # T20: Positions 16-20
      21:25,    # T25: Positions 21-25
      26:30,    # T30: Positions 26-30
      31:n_drivers  # Beyond T30
    )
    
    # Calculate marginal probabilities
    marg_probs <- numeric(length(pos_ranges))
    prev_prob <- 0
    for (j in 1:length(cum_probs)) {
      marg_probs[j] <- max(0, cum_probs[j] - prev_prob)  # Ensure non-negative
      prev_prob <- cum_probs[j]
    }
    
    # Normalize to ensure they sum to 1
    if (sum(marg_probs) > 0) {
      marg_probs <- marg_probs / sum(marg_probs)
    } else {
      # If all zero (should be rare), use uniform distribution
      marg_probs <- rep(1/length(marg_probs), length(marg_probs))
    }
    
    # Sample a position range based on marginal probabilities
    pos_range_idx <- sample(1:length(pos_ranges), 1, prob = marg_probs)
    pos_range <- pos_ranges[[pos_range_idx]]
    
    # Sample a specific position within the range
    if (length(pos_range) > 1) {
      sampled_pos <- sample(pos_range, 1)
    } else {
      sampled_pos <- pos_range
    }
    
    # Add small random noise to break ties
    performance_scores[i] <- sampled_pos + runif(1, 0, 0.1)
  }
  
  # Rank the drivers (lower score is better)
  finish_positions <- rank(performance_scores)
  
  # Check for duplicates (extremely rare but possible)
  if (anyDuplicated(finish_positions)) {
    # Add a tiny bit more randomness to resolve ties
    finish_positions <- rank(performance_scores + runif(n_drivers, 0, 0.001))
  }
  
  return(finish_positions)
}

# Platform-specific dominator point assignment for DraftKings
assign_dk_dominator_points <- function(race_results, dominator_data, total_dom_points) {
  setDT(race_results)
  setDT(dominator_data)
  
  # Pre-allocate and pre-process
  race_results[, DKDominatorPoints := 0]
  remaining_points <- total_dom_points
  
  # Add safety check for empty dominator_data
  if(nrow(dominator_data) == 0) {
    # Fallback distribution if no rules but points exist
    if(remaining_points > 0 && nrow(race_results) > 0) {
      # Distribute points to top finishers, prioritizing higher tier drivers
      top_finishers <- race_results[order(DKDomTier, FinishPosition)][1:min(5, .N)]
      if(nrow(top_finishers) > 0) {
        points_per_driver <- round(remaining_points / nrow(top_finishers), 2)
        for(i in 1:nrow(top_finishers)) {
          race_results[Name == top_finishers$Name[i], 
                       DKDominatorPoints := DKDominatorPoints + points_per_driver]
        }
      }
    }
    return(race_results)
  }
  
  # Track which drivers have received points
  drivers_with_points <- character(0)
  
  # Apply rules in order of rank
  if(remaining_points > 0) {
    # First Pass: Rank-Based Point Assignment
    for(i in 1:nrow(dominator_data)) {
      if(remaining_points <= 0) break
      
      rule <- dominator_data[i]
      
      # Skip invalid rules
      if(is.null(rule) || length(rule) == 0) next
      
      # Extract rule parameters with defaults
      fin_low <- ifelse(is.na(rule$FinLow), 1, rule$FinLow)
      fin_high <- ifelse(is.na(rule$FinHigh), Inf, rule$FinHigh)
      tier_min <- ifelse(is.na(rule$TierMin), 1, rule$TierMin)
      tier_max <- ifelse(is.na(rule$TierMax), Inf, rule$TierMax)
      
      # Find eligible drivers
      eligible <- race_results[
        !(Name %in% drivers_with_points) &
          DKDominatorPoints == 0 & 
          FinishPosition >= fin_low & 
          FinishPosition <= fin_high & 
          DKDomTier >= tier_min & 
          DKDomTier <= tier_max
      ]
      
      # Try less strict criteria if no eligible drivers
      if(nrow(eligible) == 0) {
        fin_high_expanded <- ifelse(is.infinite(fin_high), Inf, fin_high + 2)
        
        eligible <- race_results[
          !(Name %in% drivers_with_points) &
            DKDominatorPoints == 0 & 
            FinishPosition >= fin_low & 
            FinishPosition <= fin_high_expanded & 
            DKDomTier >= tier_min & 
            DKDomTier <= tier_max
        ]
      }
      
      # Try even more relaxed criteria if still no eligible drivers
      if(nrow(eligible) == 0) {
        fin_high_wider <- ifelse(is.infinite(fin_high), Inf, fin_high + 4)
        
        eligible <- race_results[
          !(Name %in% drivers_with_points) &
            DKDominatorPoints == 0 & 
            FinishPosition >= fin_low & 
            FinishPosition <= fin_high_wider & 
            DKDomTier >= tier_min & 
            DKDomTier <= tier_max
        ]
      }
      
      # Tier-based fallback as last resort
      if(nrow(eligible) == 0) {
        for(tier in 1:3) {
          if(tier > tier_max) break
          
          tier_drivers <- race_results[
            !(Name %in% drivers_with_points) &
              DKDominatorPoints == 0 & 
              DKDomTier == tier
          ]
          
          if(nrow(tier_drivers) > 0) {
            eligible <- tier_drivers[order(FinishPosition)][1]
            break
          }
        }
      }
      
      # Skip if still no eligible drivers
      if(nrow(eligible) == 0) next
      
      # Determine point range from rule
      points_low <- max(0, ifelse(is.na(rule$PtLow), 0, rule$PtLow))
      points_high <- min(
        ifelse(is.na(rule$PtHigh), 5, rule$PtHigh),
        remaining_points
      )
      
      # Skip if no points available
      if(points_low > points_high) next
      
      # Selection logic (ordered vs random)
      has_or_value <- !is.null(rule$OR) && !is.na(rule$OR) && length(rule$OR) > 0
      
      selected_driver <- if(has_or_value) {
        if(rule$OR == "O") {
          eligible[order(FinishPosition)][1]
        } else if(rule$OR == "R") {
          eligible[sample.int(nrow(eligible), 1)]
        } else {
          eligible[order(FinishPosition)][1]
        }
      } else {
        eligible[order(FinishPosition)][1]
      }
      
      # Determine points to assign
      points_to_assign <- if(i == nrow(dominator_data)) {
        if(selected_driver$DKDomTier <= 2) {
          min(remaining_points, total_dom_points * 0.4)
        } else {
          min(remaining_points, 5.0)
        }
      } else {
        min(
          round(runif(1, points_low, points_high), 2),
          remaining_points
        )
      }
      
      # Ensure non-negative points
      points_to_assign <- max(0, points_to_assign)
      
      # Assign points
      if(points_to_assign > 0) {
        race_results[Name == selected_driver$Name, 
                     DKDominatorPoints := DKDominatorPoints + points_to_assign]
        
        # Track this driver and update remaining points
        drivers_with_points <- c(drivers_with_points, selected_driver$Name)
        remaining_points <- remaining_points - points_to_assign
      }
    }
  }
  
  # Handle remaining points using "dominated but wrecked" logic
  if(remaining_points > 0) {
    if(remaining_points >= (total_dom_points * 0.20)) {
      # Look for Tier 1 drivers who finished poorly
      potential_dominators <- race_results[DKDomTier == 1 & FinishPosition > 15 & DKDominatorPoints == 0]
      
      # If no Tier 1 drivers meet criteria, check Tier 2
      if(nrow(potential_dominators) == 0) {
        potential_dominators <- race_results[DKDomTier == 2 & FinishPosition > 15 & DKDominatorPoints == 0]
      }
      
      if(nrow(potential_dominators) > 0) {
        # Select one with worse finish position (likely crashed/had issues)
        potential_dominators <- potential_dominators[order(-FinishPosition)]
        selected_dominator <- potential_dominators[1]
        
        # Assign a large chunk of remaining points
        points_to_assign <- round(remaining_points * runif(1, 0.6, 0.9), 2)
        
        race_results[Name == selected_dominator$Name, 
                     DKDominatorPoints := DKDominatorPoints + points_to_assign]
        
        remaining_points <- remaining_points - points_to_assign
      }
    }
    
    # Distribute any remaining points to drivers with no points yet
    remaining_drivers <- race_results[DKDominatorPoints == 0]
    
    if(nrow(remaining_drivers) > 0) {
      # Process each tier in sequence
      for(tier in 1:5) {
        # Get drivers in this tier
        tier_drivers <- remaining_drivers[DKDomTier == tier]
        
        if(nrow(tier_drivers) > 0) {
          # Different allocation strategy for different tiers
          if(tier <= 3) {
            # For tiers 1-3, allocate with exponential decay
            tier_weights <- c(0.65, 0.25, 0.1, 0, 0)
            tier_points <- remaining_points * tier_weights[tier]
            
            if(tier_points <= 0) next
            
            # Sort by finish position
            tier_drivers <- tier_drivers[order(FinishPosition)]
            
            # Create exponential decay weights
            position_factor <- 0.7 + (0.05 * tier)
            weights <- exp(-position_factor * (1:nrow(tier_drivers) - 1))
            weights <- weights / sum(weights)
            
            # Calculate points per driver
            points_allocation <- round(tier_points * weights, 2)
            
            # Assign points
            points_assigned <- FALSE
            for(i in 1:nrow(tier_drivers)) {
              if(points_allocation[i] > 0) {
                race_results[Name == tier_drivers$Name[i], 
                             DKDominatorPoints := DKDominatorPoints + points_allocation[i]]
                
                remaining_points <- remaining_points - points_allocation[i]
                points_assigned <- TRUE
              }
            }
            
            # Stop after assigning points to a tier
            if(points_assigned) break
          } else {
            # For tiers 4-5, assign minimal points
            if(remaining_points > 0.1) {
              tier_drivers <- tier_drivers[order(FinishPosition)]
              max_per_driver <- min(0.5, remaining_points / nrow(tier_drivers))
              
              for(i in 1:min(3, nrow(tier_drivers))) {
                if(remaining_points < 0.1) break
                
                points_to_add <- min(max_per_driver, remaining_points)
                
                race_results[Name == tier_drivers$Name[i], 
                             DKDominatorPoints := DKDominatorPoints + points_to_add]
                
                remaining_points <- remaining_points - points_to_add
              }
            }
          }
        }
      }
    }
  }
  
  # Validation - cap Tier 4/5 drivers with excessive points
  unusual_allocations <- race_results[DKDomTier >= 4 & DKDominatorPoints > 5]
  if(nrow(unusual_allocations) > 0) {
    for(i in 1:nrow(unusual_allocations)) {
      driver_name <- unusual_allocations$Name[i]
      original_points <- unusual_allocations$DKDominatorPoints[i]
      excess_points <- original_points - 5
      
      race_results[Name == driver_name, DKDominatorPoints := 5]
      
      # Redistribute excess points to top Tier 1 finishers
      if(excess_points > 0) {
        top_t1_drivers <- race_results[DKDomTier == 1][order(FinishPosition)][1:3]
        if(nrow(top_t1_drivers) > 0) {
          points_per_driver <- round(excess_points / nrow(top_t1_drivers), 2)
          for(j in 1:nrow(top_t1_drivers)) {
            race_results[Name == top_t1_drivers$Name[j], 
                         DKDominatorPoints := DKDominatorPoints + points_per_driver]
          }
        }
      }
    }
  }
  
  # Final check - ensure total is not higher than the race total
  total_allocated <- sum(race_results$DKDominatorPoints, na.rm = TRUE)
  if(total_allocated > total_dom_points) {
    # Scale everyone's points down proportionally
    scale_factor <- total_dom_points / total_allocated
    race_results[, DKDominatorPoints := round(DKDominatorPoints * scale_factor, 2)]
  }
  
  return(race_results)
}

# Platform-specific dominator point assignment for FanDuel
assign_fd_dominator_points <- function(race_results, fd_dominator_data, total_dom_points) {
  setDT(race_results)
  setDT(fd_dominator_data)
  
  # Create copy to avoid modifying the original
  result <- copy(race_results)
  
  # Initialize with zeros
  result[, FDDominatorPoints := 0]
  remaining_points <- total_dom_points
  
  # Safety check for empty dominator data
  if(nrow(fd_dominator_data) == 0) {
    # Fallback distribution if no rules but points exist
    if(remaining_points > 0 && nrow(result) > 0) {
      # Distribute based on finish position and tier
      top_drivers <- result[order(FDDomTier, FinishPosition)][1:min(5, .N)]
      if(nrow(top_drivers) > 0) {
        points_per_driver <- round(remaining_points / nrow(top_drivers), 2)
        for(i in 1:nrow(top_drivers)) {
          result[Name == top_drivers$Name[i], 
                 FDDominatorPoints := FDDominatorPoints + points_per_driver]
        }
      }
    }
    return(result)
  }
  
  # Process rank field for sorting
  if("Rank" %in% names(fd_dominator_data)) {
    fd_dominator_data[, ProcessedRank := {
      ifelse(Rank == "Strategy", 0,
             ifelse(Rank == "DomDead", 999,
                    as.numeric(Rank)))
    }]
    fd_dominator_data[, OriginalRank := Rank]
    setorder(fd_dominator_data, ProcessedRank)
  }
  
  # Track drivers who have received points
  drivers_with_points <- character(0)
  
  # Apply each rule in order
  for(i in 1:nrow(fd_dominator_data)) {
    if(remaining_points <= 0) break
    
    rule <- fd_dominator_data[i]
    if(is.null(rule) || length(rule) == 0) next
    
    # Extract rule parameters with defaults
    fin_low <- ifelse(is.na(rule$FinLow), 1, rule$FinLow)
    fin_high <- ifelse(is.na(rule$FinHigh), Inf, rule$FinHigh)
    tier_min <- ifelse(is.na(rule$TierMin), 1, rule$TierMin)
    tier_max <- ifelse(is.na(rule$TierMax), Inf, rule$TierMax)
    pt_low <- max(0, ifelse(is.na(rule$PtLow), 0, rule$PtLow))
    pt_high <- min(ifelse(is.na(rule$PtHigh), 5, rule$PtHigh), remaining_points)
    
    # Skip if no points available
    if(pt_low > pt_high) next
    
    # Find eligible drivers
    eligible <- result[
      !(Name %in% drivers_with_points) &
        FinishPosition >= fin_low & 
        FinishPosition <= fin_high & 
        FDDomTier >= tier_min & 
        FDDomTier <= tier_max
    ]
    
    if(nrow(eligible) == 0) next
    
    # Selection method - Ordered vs Random
    has_or_value <- !is.null(rule$OR) && !is.na(rule$OR) && length(rule$OR) > 0
    
    selected_driver <- if(has_or_value && rule$OR == "R") {
      # Random selection
      eligible[sample.int(nrow(eligible), 1)]
    } else {
      # Default to ordered by position
      eligible[order(FinishPosition)][1]
    }
    
    # Generate random points within range
    points_to_assign <- round(runif(1, pt_low, pt_high), 2)
    points_to_assign <- min(points_to_assign, remaining_points)
    
    # Assign points
    if(points_to_assign > 0) {
      result[Name == selected_driver$Name, 
             FDDominatorPoints := FDDominatorPoints + points_to_assign]
      
      # Track and update remaining points
      drivers_with_points <- c(drivers_with_points, selected_driver$Name)
      remaining_points <- remaining_points - points_to_assign
    }
  }
  
  # Handle dominated-but-wrecked scenario
  if(remaining_points > (total_dom_points * 0.1)) {
    potential_dominators <- result[
      !(Name %in% drivers_with_points) &
        FDDomTier <= 2 &
        FinishPosition > 10
    ]
    
    if(nrow(potential_dominators) > 0) {
      # Select one with worse finish position
      selected <- potential_dominators[order(-FinishPosition)][1]
      
      # Assign significant portion of remaining points
      points_to_assign <- round(remaining_points * runif(1, 0.6, 0.9), 2)
      
      result[Name == selected$Name, 
             FDDominatorPoints := FDDominatorPoints + points_to_assign]
      
      remaining_points <- remaining_points - points_to_assign
    }
  }
  
  # Distribute remaining points to drivers who haven't received points
  if(remaining_points > 0.5) {
    potential_recipients <- result[
      !(Name %in% drivers_with_points) &
        FinishPosition <= 15 &
        FDDomTier <= 3
    ]
    
    if(nrow(potential_recipients) > 0) {
      # Sort by tier and finish position
      potential_recipients <- potential_recipients[order(FDDomTier, FinishPosition)]
      
      # Distribute to top finishers
      n_recipients <- min(3, nrow(potential_recipients))
      
      # Create exponentially decaying weights
      weights <- exp(-0.7 * (1:n_recipients - 1))
      weights <- weights / sum(weights)
      
      # Distribute points
      for(i in 1:n_recipients) {
        points_to_add <- round(remaining_points * weights[i], 2)
        
        if(points_to_add < 0.1) next
        
        result[Name == potential_recipients$Name[i], 
               FDDominatorPoints := FDDominatorPoints + points_to_add]
        
        remaining_points <- remaining_points - points_to_add
      }
    }
  }
  
  # Ensure total matches expected
  total_allocated <- sum(result$FDDominatorPoints, na.rm = TRUE)
  
  if(total_allocated > total_dom_points) {
    # Scale down proportionally
    scale_factor <- total_dom_points / total_allocated
    result[, FDDominatorPoints := round(FDDominatorPoints * scale_factor, 2)]
  } else if(total_allocated < total_dom_points && (total_dom_points - total_allocated) >= 0.5) {
    # Add remaining points to top dominator
    remainder <- total_dom_points - total_allocated
    top_dominator <- result[order(-FDDominatorPoints)][1]
    
    result[Name == top_dominator$Name, 
           FDDominatorPoints := FDDominatorPoints + remainder]
  }
  
  # Final rounding for consistency
  result[, FDDominatorPoints := round(FDDominatorPoints, 2)]
  
  return(result)
}

# FanDuel lap points assignment
assign_fd_lap_points <- function(race_results, fd_laps_data) {
  # Ensure both are data.tables
  setDT(race_results)
  setDT(fd_laps_data)
  
  # Create a copy to avoid modifying the original
  result <- copy(race_results)
  
  # Initialize lap points without affecting other columns
  result[, FDLapPoints := 0]
  
  # If no lap data, return early
  if(nrow(fd_laps_data) == 0) return(result)
  
  # Ensure necessary columns exist
  if(!"ps" %in% names(fd_laps_data)) {
    warning("No 'ps' column in lap data, returning zeros for lap points")
    return(result)
  }
  
  if(!"Pt" %in% names(fd_laps_data) && !all(c("PtLow", "PtHigh") %in% names(fd_laps_data))) {
    warning("No point columns found in lap data, returning zeros for lap points")
    return(result)
  }
  
  # Determine which column to use
  point_col <- if("Pt" %in% names(fd_laps_data)) "Pt" else "PtLow"
  
  # Ensure ps is numeric
  fd_laps_data[, ps := as.numeric(ps)]
  
  # Remove any NA rows
  fd_laps_data <- fd_laps_data[!is.na(ps) & !is.na(get(point_col))]
  
  # If no valid data after filtering, return early
  if(nrow(fd_laps_data) == 0) return(result)
  
  # Build a complete positions lookup table from 1 to max position
  max_pos <- max(result$FinishPosition, na.rm = TRUE)
  max_ps <- max(fd_laps_data$ps, na.rm = TRUE)
  
  # Create a lookup table for all possible positions
  all_positions <- data.table(
    position = 1:max(max_pos, max_ps)
  )
  
  # For each position in the lookup table, find the points value
  for(i in 1:nrow(all_positions)) {
    pos <- all_positions$position[i]
    
    # Try exact match first
    exact_match <- fd_laps_data[ps == pos]
    
    if(nrow(exact_match) > 0) {
      # Use exact match
      all_positions$points[i] <- exact_match[[point_col]][1]
    } else {
      # Find nearest lower value
      lower_match <- fd_laps_data[ps < pos][order(-ps)]
      if(nrow(lower_match) > 0) {
        # Use the nearest lower value
        all_positions$points[i] <- lower_match[[point_col]][1]
      } else {
        # No lower value, use the nearest higher
        higher_match <- fd_laps_data[ps > pos][order(ps)]
        if(nrow(higher_match) > 0) {
          all_positions$points[i] <- higher_match[[point_col]][1]
        } else {
          # No match at all, default to 0
          all_positions$points[i] <- 0
        }
      }
    }
  }
  
  # Enforce the non-increasing property
  for(i in 2:nrow(all_positions)) {
    if(all_positions$points[i] > all_positions$points[i-1]) {
      all_positions$points[i] <- all_positions$points[i-1]
    }
  }
  
  # Apply lap points to each driver based on finish position
  for(i in 1:nrow(result)) {
    if(!is.na(result$FinishPosition[i])) {
      pos <- result$FinishPosition[i]
      if(pos <= nrow(all_positions)) {
        result$FDLapPoints[i] <- all_positions$points[pos]
      } else {
        # Position out of range, use the last available value
        result$FDLapPoints[i] <- all_positions$points[nrow(all_positions)]
      }
    } else {
      # NA finish position gets 0 points
      result$FDLapPoints[i] <- 0
    }
  }
  
  # Ensure consistent precision
  result[, FDLapPoints := round(FDLapPoints, 1)]
  
  return(result)
}

# Main simulation function (optimized for memory)
run_integrated_simulations <- function(input_data, n_sims = 1000, batch_size = 100) {
  # Extract necessary data
  drivers_dt <- as.data.table(input_data$drivers)
  n_drivers <- nrow(drivers_dt)
  
  # Determine which platforms are active
  has_dk <- "DKSalary" %in% names(drivers_dt) && length(input_data$dk_dom_points) > 0
  has_fd <- "FDSalary" %in% names(drivers_dt) && length(input_data$fd_dom_points) > 0
  
  # Pre-calculate scoring vectors
  # DraftKings finish points
  dk_finish_points <- c(45, 42, 41, 40, 39, 38, 37, 36, 35, 34,
                        32, 31, 30, 29, 28, 27, 26, 25, 24, 23,
                        21, 20, 19, 18, 17, 16, 15, 14, 13, 12,
                        10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
  
  # FanDuel finish points
  fd_finish_points <- c(43, 40, 38, 37, 36, 35, 34, 33, 32, 31,
                        30, 29, 28, 27, 26, 25, 24, 23, 22, 21,
                        20, 19, 18, 17, 16, 15, 14, 13, 12, 11,
                        10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
  
  # Create a common base result structure
  results_columns <- c(
    "SimID", "Name", "Starting", "FinishPosition"
  )
  
  # Add platform-specific columns
  if(has_dk) {
    results_columns <- c(
      results_columns, 
      "DKSalary", "DKOP", "DKName", "DKDominatorPoints", "DKFantasyPoints"
    )
  }
  
  if(has_fd) {
    results_columns <- c(
      results_columns, 
      "FDSalary", "FDOP", "FDName", "FDDominatorPoints", "FDLapPoints", "FDFantasyPoints"
    )
  }
  
  # Pre-allocate results data.table with proper types and sizes
  results <- data.table(matrix(NA, nrow = n_drivers * n_sims, ncol = length(results_columns)))
  setnames(results, results_columns)
  
  # Set column types
  results[, SimID := integer(n_drivers * n_sims)]
  results[, Name := character(n_drivers * n_sims)]
  results[, Starting := numeric(n_drivers * n_sims)]
  results[, FinishPosition := integer(n_drivers * n_sims)]
  
  if(has_dk) {
    results[, DKSalary := numeric(n_drivers * n_sims)]
    results[, DKOP := numeric(n_drivers * n_sims)]
    results[, DKName := character(n_drivers * n_sims)]
    results[, DKDominatorPoints := numeric(n_drivers * n_sims)]
    results[, DKFantasyPoints := numeric(n_drivers * n_sims)]
  }
  
  if(has_fd) {
    results[, FDSalary := numeric(n_drivers * n_sims)]
    results[, FDOP := numeric(n_drivers * n_sims)]
    results[, FDName := character(n_drivers * n_sims)]
    results[, FDDominatorPoints := numeric(n_drivers * n_sims)]
    results[, FDLapPoints := numeric(n_drivers * n_sims)]
    results[, FDFantasyPoints := numeric(n_drivers * n_sims)]
  }
  
  # Pre-fill static data
  results[, SimID := rep(1:n_sims, each = n_drivers)]
  results[, Name := rep(drivers_dt$Name, n_sims)]
  results[, Starting := rep(drivers_dt$Starting, n_sims)]
  
  if(has_dk) {
    results[, DKSalary := rep(drivers_dt$DKSalary, n_sims)]
    results[, DKOP := rep(drivers_dt$DKOP, n_sims)]
    results[, DKName := rep(drivers_dt$DKName, n_sims)]
  }
  
  if(has_fd) {
    results[, FDSalary := rep(drivers_dt$FDSalary, n_sims)]
    results[, FDOP := rep(drivers_dt$FDOP, n_sims)]
    results[, FDName := rep(drivers_dt$FDName, n_sims)]
  }
  
  # Calculate batch size based on number of drivers
  batch_size <- min(batch_size, max(50, ceiling(5000 / n_drivers)))
  n_batches <- ceiling(n_sims / batch_size)
  
  cat("Starting simulation with", n_sims, "races...\n")
  
  for(batch in 1:n_batches) {
    batch_start <- Sys.time()
    
    start_sim <- (batch - 1) * batch_size + 1
    end_sim <- min(batch * batch_size, n_sims)
    current_batch_size <- end_sim - start_sim + 1
    
    # Calculate row indices for this batch
    start_idx <- (start_sim - 1) * n_drivers + 1
    end_idx <- start_idx + (current_batch_size * n_drivers) - 1
    
    cat("Processing batch", batch, "of", n_batches, "(simulations", start_sim, "to", end_sim, ")\n")
    
    # Process each simulation in the batch
    for(sim_offset in 1:current_batch_size) {
      sim <- start_sim + sim_offset - 1
      sim_start_idx <- start_idx + (sim_offset - 1) * n_drivers
      sim_end_idx <- sim_start_idx + n_drivers - 1
      
      # Simulate finishing positions for this race using the improved function
      finish_positions <- simulate_finishing_positions(drivers_dt)
      
      # Store finish positions
      results[sim_start_idx:sim_end_idx, FinishPosition := finish_positions]
      
      # Create single race result for assigning dominator points
      race_result <- data.table(
        Name = drivers_dt$Name,
        FinishPosition = finish_positions
      )
      
      # If DraftKings is active, calculate DK dominator points and fantasy points
      if(has_dk) {
        race_result$DKDomTier <- drivers_dt$DKDomTier
        
        # Assign dominator points
        dk_dom_result <- assign_dk_dominator_points(
          race_result, 
          input_data$dk_dominator,
          input_data$dk_dom_points[1]
        )
        
        # Store DK dominator points
        results[sim_start_idx:sim_end_idx, DKDominatorPoints := dk_dom_result$DKDominatorPoints]
        
        # Calculate DK fantasy points
        dk_fantasy_points <- dk_finish_points[pmin(finish_positions, length(dk_finish_points))] + 
          (drivers_dt$Starting - finish_positions) + 
          dk_dom_result$DKDominatorPoints
        
        # Store DK fantasy points
        results[sim_start_idx:sim_end_idx, DKFantasyPoints := dk_fantasy_points]
      }
      
      # If FanDuel is active, calculate FD dominator points, lap points, and fantasy points
      if(has_fd) {
        race_result$FDDomTier <- drivers_dt$FDDomTier
        
        # Assign FD dominator points
        fd_dom_result <- assign_fd_dominator_points(
          race_result, 
          input_data$fd_dominator,
          input_data$fd_dom_points[1]
        )
        
        # Store FD dominator points
        results[sim_start_idx:sim_end_idx, FDDominatorPoints := fd_dom_result$FDDominatorPoints]
        
        # Assign FD lap points
        fd_lap_result <- assign_fd_lap_points(fd_dom_result, input_data$fd_laps)
        
        # Store FD lap points
        results[sim_start_idx:sim_end_idx, FDLapPoints := fd_lap_result$FDLapPoints]
        
        # Calculate FD fantasy points (FanDuel place differential is * 0.5)
        fd_fantasy_points <- fd_finish_points[pmin(finish_positions, length(fd_finish_points))] + 
          ((drivers_dt$Starting - finish_positions) * 0.5) + 
          fd_dom_result$FDDominatorPoints +
          fd_lap_result$FDLapPoints
        
        # Store FD fantasy points
        results[sim_start_idx:sim_end_idx, FDFantasyPoints := fd_fantasy_points]
      }
    }
    
    # Report progress and clean up
    batch_end <- Sys.time()
    batch_time <- difftime(batch_end, batch_start, units = "secs")
    cat(sprintf("Batch %d/%d completed in %.1f seconds\n", 
                batch, n_batches, as.numeric(batch_time)))
    
    # Force garbage collection every batch to manage memory
    gc(verbose = FALSE, full = TRUE)
  }
  
  # Set key for better performance in subsequent operations
  setkey(results, SimID)
  
  # Return the results and platform availability info
  list(
    results = results,
    has_dk = has_dk,
    has_fd = has_fd
  )
}

# Analysis Functions (optimized with data.table)
analyze_finishing_positions <- function(sim_results) {
  setDT(sim_results)
  
  results <- sim_results[, .(
    Win_Rate = mean(FinishPosition == 1, na.rm = TRUE) * 100,
    T3_Rate = mean(FinishPosition <= 3, na.rm = TRUE) * 100,
    T5_Rate = mean(FinishPosition <= 5, na.rm = TRUE) * 100,
    T10_Rate = mean(FinishPosition <= 10, na.rm = TRUE) * 100,
    T15_Rate = mean(FinishPosition <= 15, na.rm = TRUE) * 100,
    T20_Rate = mean(FinishPosition <= 20, na.rm = TRUE) * 100,
    T25_Rate = mean(FinishPosition <= 25, na.rm = TRUE) * 100,
    T30_Rate = mean(FinishPosition <= 30, na.rm = TRUE) * 100,
    Avg_Finish = mean(FinishPosition, na.rm = TRUE),
    Median = median(FinishPosition, na.rm = TRUE)
  ), by = Name]
  
  results <- results[order(Avg_Finish)]
  
  for (col in setdiff(names(results), "Name")) {
    results[, (col) := round(get(col), 1)]
  }
  
  return(results)
}

analyze_dk_dominator_points <- function(sim_results) {
  setDT(sim_results)
  
  # Calculate dominator rank for each simulation
  sim_results[, DKDominatorRank := frank(-DKDominatorPoints, ties.method = "min"), by = SimID]
  
  results <- sim_results[, .(
    Starting = first(Starting),
    DKSalary = first(DKSalary),
    Avg_Dom = mean(DKDominatorPoints),
    Median_Dom = median(DKDominatorPoints),
    Max_Dom = max(DKDominatorPoints),
    Avg_DomRank = mean(DKDominatorRank),
    Median_DomRank = median(DKDominatorRank),
    Top_DomRate = mean(DKDominatorRank == 1) * 100,
    Top3_DomRate = mean(DKDominatorRank <= 3) * 100,
    Top5_DomRate = mean(DKDominatorRank <= 5) * 100,
    Top10_DomRate = mean(DKDominatorRank <= 10) * 100
  ), by = Name]
  
  # Ensure numeric columns before rounding
  numeric_cols <- c("Starting", "DKSalary", "Avg_Dom", "Median_Dom", "Max_Dom", 
                    "Avg_DomRank", "Median_DomRank", 
                    "Top_DomRate", "Top3_DomRate", "Top5_DomRate", "Top10_DomRate")
  
  # Convert to numeric and round to 1 decimal place
  for (col in numeric_cols) {
    results[, (col) := round(as.numeric(get(col)), 1)]
  }
  
  # Sort by Average Dominator Points in descending order
  setorder(results, -Avg_Dom)
  
  return(results)
}

analyze_fd_dominator_points <- function(sim_results) {
  setDT(sim_results)
  
  # Calculate dominator rank for each simulation
  sim_results[, FDDominatorRank := frank(-FDDominatorPoints, ties.method = "min"), by = SimID]
  
  results <- sim_results[, .(
    Starting = first(Starting),
    FDSalary = first(FDSalary),
    Avg_Dom = mean(FDDominatorPoints),
    Median_Dom = median(FDDominatorPoints),
    Max_Dom = max(FDDominatorPoints),
    Avg_DomRank = mean(FDDominatorRank),
    Median_DomRank = median(FDDominatorRank),
    Top_DomRate = mean(FDDominatorRank == 1) * 100,
    Top3_DomRate = mean(FDDominatorRank <= 3) * 100,
    Top5_DomRate = mean(FDDominatorRank <= 5) * 100
  ), by = Name]
  
  # Ensure numeric columns before rounding
  numeric_cols <- c("Starting", "FDSalary", "Avg_Dom", "Median_Dom", "Max_Dom", 
                    "Avg_DomRank", "Median_DomRank", 
                    "Top_DomRate", "Top3_DomRate", "Top5_DomRate")
  
  # Convert to numeric and round to 1 decimal place
  for (col in numeric_cols) {
    results[, (col) := round(as.numeric(get(col)), 1)]
  }
  
  # Sort by Average Dominator Points in descending order
  setorder(results, -Avg_Dom)
  
  return(results)
}

analyze_fd_lap_points <- function(sim_results) {
  setDT(sim_results)
  
  results <- sim_results[, .(
    Starting = first(Starting),
    FDSalary = first(FDSalary),
    Avg_Lap = mean(FDLapPoints),
    Median_Lap = median(FDLapPoints),
    Max_Lap = max(FDLapPoints),
    Min_Lap = min(FDLapPoints)
  ), by = Name]
  
  # Round numeric columns
  numeric_cols <- c("Starting", "FDSalary", "Avg_Lap", "Median_Lap", "Max_Lap", "Min_Lap")
  for (col in numeric_cols) {
    results[, (col) := round(as.numeric(get(col)), 1)]
  }
  
  # Sort by Average Lap Points in descending order
  setorder(results, -Avg_Lap)
  
  return(results)
}

# DraftKings fantasy point projections
analyze_dk_fantasy_points <- function(sim_results) {
  setDT(sim_results)
  
  results <- sim_results[, .(
    DKSalary = first(DKSalary),
    Starting = first(Starting),
    DKOP = first(DKOP),
    Median_Fantasy_Pts = round(median(DKFantasyPoints), 1),
    FP_90thPct = round(quantile(DKFantasyPoints, 0.9), 1)
  ), by = .(Name)]
  
  # Add PPD calculation
  results[, PPD := round(Median_Fantasy_Pts / (DKSalary/1000), 1)]
  
  # Multiply DKOP by 100 to convert to percentage if it's a proportion
  if(max(results$DKOP, na.rm = TRUE) <= 1) {
    results[, DKOP := round(as.numeric(DKOP) * 100, 1)]
  }
  
  return(results)
}

# FanDuel fantasy point projections
analyze_fd_fantasy_points <- function(sim_results) {
  setDT(sim_results)
  
  results <- sim_results[, .(
    FDSalary = first(FDSalary),
    Starting = first(Starting),
    FDOP = first(FDOP),
    Median_Fantasy_Pts = round(median(FDFantasyPoints), 1),
    FP_90thPct = round(quantile(FDFantasyPoints, 0.9), 1)
  ), by = .(Name)]
  
  # Add PPD calculation
  results[, PPD := round(Median_Fantasy_Pts / (FDSalary/1000), 1)]
  
  # Multiply FDOP by 100 to convert to percentage if it's a proportion
  if(max(results$FDOP, na.rm = TRUE) <= 1) {
    results[, FDOP := round(as.numeric(FDOP) * 100, 1)]
  }
  
  return(results)
}

# Accuracy analysis
analyze_simulation_accuracy <- function(sim_results, input_data) {
  # Make sure we're working with data.tables
  setDT(sim_results)
  setDT(input_data)
  
  # Get unique drivers
  drivers <- unique(input_data$Name)
  metrics <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
  
  results <- data.table()
  
  for(driver in drivers) {
    # Get simulation results for this driver
    driver_sims <- sim_results[Name == driver]
    driver_input <- input_data[Name == driver]
    
    total_sims <- nrow(driver_sims)
    if(total_sims == 0) next
    
    # Calculate simulated percentages
    sim_pcts <- c(
      W = sum(driver_sims$FinishPosition == 1) / total_sims,
      T3 = sum(driver_sims$FinishPosition <= 3) / total_sims,
      T5 = sum(driver_sims$FinishPosition <= 5) / total_sims,
      T10 = sum(driver_sims$FinishPosition <= 10) / total_sims,
      T15 = sum(driver_sims$FinishPosition <= 15) / total_sims,
      T20 = sum(driver_sims$FinishPosition <= 20) / total_sims,
      T25 = sum(driver_sims$FinishPosition <= 25) / total_sims,
      T30 = sum(driver_sims$FinishPosition <= 30) / total_sims
    )
    
    for(metric in metrics) {
      input_value <- driver_input[[metric]]
      sim_value <- sim_pcts[metric]
      
      if(!is.na(input_value)) {
        results <- rbind(results, data.table(
          Driver = driver,
          Metric = metric,
          Input = input_value * 100, # Convert to percentage
          Simulated = sim_value * 100, # Convert to percentage
          Difference = abs(input_value * 100 - sim_value * 100) # Convert to percentage
        ))
      }
    }
  }
  
  # Sort by driver and metric
  setorder(results, Driver, Metric)
  
  return(results)
}


# Define UI
# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; font-weight: bold;",
      "Nascar Simulator"
    ),
    titleWidth = 250
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    useShinyjs(),
    div(
      style = "text-align: center; padding: 10px;",
      tags$img(src = "logo.jpg", height = "200px", width = "auto")
    ),
    fileInput("excel_file", "Upload Import File", accept = c(".xlsx")),
    div(style = "display: flex; align-items: center; margin: 5px 0; padding: 0 5px;",
        div(style = "color: #FFD700; font-weight: bold; margin-right: 5px; width: 90px; text-align: right;",
            "Sim Count:"),
        div(style = "flex: 1;",
            numericInput("n_sims", label = NULL, value = 25000, min = 100, max = 100000,
                         width = "120px"))
    ),
    actionButton("run_sim", "Run Simulation", class = "btn-primary", style = "margin: 5px; width: 90%"),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Data Input", tabName = "upload", icon = icon("upload")),
      menuItem("Finish Analysis", tabName = "finish_analysis", icon = icon("chart-line")),
      menuItem("Dominator Analysis", tabName = "dominator", icon = icon("trophy")),
      menuItem("Fantasy Projections", tabName = "fantasy", icon = icon("calculator"))
    )
  ),
  
  # Dashboard body
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      # Upload Tab
      tabItem(tabName = "upload",
              uiOutput("upload_content")
      ),
      
      # Finish Analysis Tab
      tabItem(tabName = "finish_analysis",

              fluidRow(
                box(width = 12,
                    title = "Simulated Finishing Results",
                    DTOutput("driver_stats") %>% withSpinner(color = "black")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Finish Position Boxplot",
                    plotlyOutput("position_box", height = "1000px") %>% withSpinner(color = "black")
                )
              )
      ),
      
      # Dominator Analysis Tab
      tabItem(tabName = "dominator",
              uiOutput("dominator_ui")
      ),
      
      # Fantasy Projections Tab
      tabItem(tabName = "fantasy",
              uiOutput("fantasy_ui")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store data
  rv <- reactiveValues(
    input_data = NULL,
    processed_data = NULL,
    simulation_results = NULL,
    has_draftkings = FALSE,
    has_fanduel = FALSE,
    finishing_analysis = NULL,
    dk_dominator_analysis = NULL,
    fd_dominator_analysis = NULL,
    fd_lap_analysis = NULL,
    dk_fantasy_analysis = NULL,
    fd_fantasy_analysis = NULL,
    file_uploaded = FALSE,
    simulation_complete = FALSE
  )
  
  output$has_draftkings <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(rv$has_draftkings))
    return(result)
  })
  outputOptions(output, "has_draftkings", suspendWhenHidden = FALSE)
  
  output$has_fanduel <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(rv$has_fanduel))
    return(result)
  })
  outputOptions(output, "has_fanduel", suspendWhenHidden = FALSE)
  

  # File upload handler
  observeEvent(input$excel_file, {
    req(input$excel_file)
    
    # Show progress notification
    withProgress(message = 'Reading file...', value = 0, {
      tryCatch({
        # Read the input file
        rv$input_data <- read_input_file(input$excel_file$datapath)
        rv$file_uploaded <- TRUE
        rv$simulation_complete <- FALSE
        
        # Reset all other values
        rv$processed_data <- NULL
        rv$simulation_results <- NULL
        rv$finishing_analysis <- NULL
        rv$dk_dominator_analysis <- NULL
        rv$fd_dominator_analysis <- NULL
        rv$fd_lap_analysis <- NULL
        rv$dk_fantasy_analysis <- NULL
        rv$fd_fantasy_analysis <- NULL
        rv$dk_optimal_lineups <- NULL
        rv$fd_optimal_lineups <- NULL
        rv$dk_driver_exposure <- NULL
        rv$fd_driver_exposure <- NULL
        rv$dk_random_lineups <- NULL
        rv$fd_random_lineups <- NULL
        rv$simulation_complete <- FALSE
        
        # Store platform availability
        rv$has_draftkings <- rv$input_data$platform_info$has_draftkings
        rv$has_fanduel <- rv$input_data$platform_info$has_fanduel
        
        
        # Process the data
        incProgress(0.5, detail = "Processing data...")
        rv$processed_data <- process_input_data(rv$input_data)
        
        # Update the data preview
        output$data_preview <- renderDT({
          # Create a filtered version of the data for display
          display_data <- rv$input_data$sheets$Driver
          
          # Remove the specified columns if they exist
          columns_to_remove <- c("DKName", "FDName", "DKID", "FDName", "FDID")
          for(col in columns_to_remove) {
            if(col %in% colnames(display_data)) {
              display_data[[col]] <- NULL
            }
          }
          
          # Create the datatable with better column alignment
          dt <- datatable(
            display_data,
            options = list(
              scrollX = TRUE, 
              pageLength = -1,  # Show all rows
              autoWidth = FALSE,  # Don't use autoWidth for better control
              dom = "t",  # Only show table ('t'), no search/pagination
              ordering = TRUE,  # Allow sorting
              columnDefs = list(
                list(className = 'dt-center', targets = "_all")  # Center-align all columns
              ),
              scrollCollapse = TRUE,
              fixedColumns = TRUE
            ),
            class = 'cell-border stripe display compact',  # Added compact class for tighter spacing
            rownames = FALSE,
            width = "100%",  # Use full width
            height = "auto"
          )
          
          # Formatting for various columns
          if("DKOP" %in% colnames(display_data)) {
            dt <- dt %>% formatRound("DKOP", digits = 2)
          }
          
          if("FDOP" %in% colnames(display_data)) {
            dt <- dt %>% formatRound("FDOP", digits = 2)
          }
          
          # Format W, T3, T5, etc. columns to 2 decimal places
          numeric_cols <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
          for(col in numeric_cols) {
            if(col %in% colnames(display_data)) {
              dt <- dt %>% formatRound(col, digits = 2)
            }
          }
          
          # Format salary columns as currency
          if("DKSalary" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("DKSalary", currency = "$", digits = 0)
          }
          
          if("FDSalary" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("FDSalary", currency = "$", digits = 0)
          }
          
          return(dt)
        })
        
        # Switch to upload tab
        updateTabItems(session, "sidebar_menu", selected = "upload")
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Error reading file:", e$message),
          easyClose = TRUE
        ))
        rv$file_uploaded <- FALSE
      })
    })
  })
  
  # Run simulation button handler
  observeEvent(input$run_sim, {
    req(rv$processed_data)
    
    # Clear previous results and force garbage collection
    rv$simulation_results <- NULL
    rv$finishing_analysis <- NULL
    rv$dk_dominator_analysis <- NULL
    rv$fd_dominator_analysis <- NULL
    rv$fd_lap_analysis <- NULL
    rv$dk_fantasy_analysis <- NULL
    rv$fd_fantasy_analysis <- NULL
 
    # Force garbage collection before running new simulation
    gc(verbose = FALSE, full = TRUE)
    
    # Show progress dialog
    withProgress(message = 'Running simulations...', value = 0, {
      # Run the simulations
      setProgress(0.1, detail = "Initializing simulation...")
      
      simulation_results <- run_integrated_simulations(
        rv$processed_data, 
        n_sims = input$n_sims,
        batch_size = 100
      )
      
      # Store results
      rv$simulation_results <- simulation_results$results
      
      # Remove dominator rank columns to save memory
      if("DKDominatorRank" %in% names(rv$simulation_results)) {
        rv$simulation_results$DKDominatorRank <- NULL
      }
      if("FDDominatorRank" %in% names(rv$simulation_results)) {
        rv$simulation_results$FDDominatorRank <- NULL
      }
      
      # Update platform availability
      rv$has_draftkings <- "DKFantasyPoints" %in% names(rv$simulation_results)
      rv$has_fanduel <- "FDFantasyPoints" %in% names(rv$simulation_results)
      
      # Force garbage collection
      gc(verbose = FALSE, full = TRUE)
      
      # Process finish position analysis
      setProgress(0.7, detail = "Analyzing finishing positions...")
      rv$finishing_analysis <- analyze_finishing_positions(rv$simulation_results)
      
      # Process dominator points analysis for each platform
      setProgress(0.8, detail = "Analyzing dominator points...")
      if(rv$has_draftkings) {
        rv$dk_dominator_analysis <- analyze_dk_dominator_points(rv$simulation_results)
      }
      
      if(rv$has_fanduel) {
        rv$fd_dominator_analysis <- analyze_fd_dominator_points(rv$simulation_results)
        rv$fd_lap_analysis <- analyze_fd_lap_points(rv$simulation_results)
      }
      
      # Process fantasy points analysis for each platform
      setProgress(0.9, detail = "Analyzing fantasy points...")
      if(rv$has_draftkings) {
        rv$dk_fantasy_analysis <- analyze_dk_fantasy_points(rv$simulation_results)
      }
      
      if(rv$has_fanduel) {
        rv$fd_fantasy_analysis <- analyze_fd_fantasy_points(rv$simulation_results)
      }
      
      # Mark simulation as complete
      rv$simulation_complete <- TRUE
      
      # Switch to finish analysis tab
      updateTabItems(session, "sidebar_menu", selected = "upload")
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        "Simulation completed successfully! Review the accuracy analysis and projections and then download results file.",
        easyClose = TRUE
      ))
      
      # Final cleanup
      gc(verbose = FALSE, full = TRUE)
    })
  })
  
  
  
  output$upload_content <- renderUI({
    if(rv$simulation_complete) {
      # Show accuracy analysis after simulation is complete
      tagList(
        fluidRow(
          box(width = 12,
              title = "Simulation Accuracy Analysis",
              DTOutput("accuracy_analysis") %>% withSpinner(color = "black")
          )
        )
      )
    } else {
      # Show input data before simulation is run
      tagList(
        fluidRow(
          box(width = 12,
              title = "Input Data",
              DTOutput("data_preview") %>% withSpinner(color = "black")
          )
        ),
        uiOutput("available_platforms")
      )
    }
  })
  
  # Accuracy analysis output
  output$accuracy_analysis <- renderDT({
    req(rv$simulation_results, rv$processed_data$drivers)
    
    # Calculate accuracy metrics
    accuracy_data <- analyze_simulation_accuracy(rv$simulation_results, rv$processed_data$drivers)
    
    # Create an informative summary view
    datatable(
      accuracy_data,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        order = list(list(4, 'desc')), # Sort by difference in descending order
        columnDefs = list(
          list(targets = c(2, 3, 4), className = 'dt-right')
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatRound(c('Input', 'Simulated', 'Difference'), digits = 2) %>%
      formatStyle(
        'Difference',
        background = styleColorBar(c(0, max(accuracy_data$Difference)), 'rgba(255, 102, 0, 0.5)'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Driver stats output
  output$driver_stats <- renderDT({
    req(rv$finishing_analysis)
    
    analysis_data <- rv$finishing_analysis
    
    # Add Starting position and salary data from input data
    if (!is.null(rv$processed_data$drivers)) {
      drivers_data <- as.data.table(rv$processed_data$drivers)
      
      # Identify available columns to join
      join_cols <- c("Name", "Starting")
      
      # Add salary columns based on available platforms
      if(rv$has_draftkings && "DKSalary" %in% names(drivers_data)) {
        join_cols <- c(join_cols, "DKSalary")
      }
      
      if(rv$has_fanduel && "FDSalary" %in% names(drivers_data)) {
        join_cols <- c(join_cols, "FDSalary")
      }
      
      # Join data with available columns
      if(length(join_cols) > 1) {
        # Filter to only include columns that exist
        join_cols <- intersect(join_cols, names(drivers_data))
        
        # Join with available columns
        analysis_data <- merge(
          analysis_data,
          drivers_data[, ..join_cols],
          by = "Name",
          all.x = TRUE
        )
        
        # Create dynamic column order based on available columns
        col_order <- c("Name", "Starting")
        
        # Add salary columns if available
        if("DKSalary" %in% names(analysis_data)) {
          col_order <- c(col_order, "DKSalary")
        }
        if("FDSalary" %in% names(analysis_data)) {
          col_order <- c(col_order, "FDSalary")
        }
        
        # Add remaining columns
        col_order <- c(
          col_order,
          "Avg_Finish", "Median", 
          "Win_Rate", "T3_Rate", "T5_Rate", "T10_Rate", 
          "T15_Rate", "T20_Rate", "T25_Rate", "T30_Rate"
        )
        
        # Ensure all columns exist
        col_order <- intersect(col_order, names(analysis_data))
        
        # Use proper data.table syntax
        analysis_data <- analysis_data[, ..col_order]
      }
    }
    
    # Prepare the table with formatting
    dt <- datatable(
      analysis_data,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(4, 'asc')),  # Sort by Avg_Finish in ascending order
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-center'
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) 
    
    # Format Salary columns
    if ("DKSalary" %in% names(analysis_data)) {
      dt <- dt %>% formatCurrency('DKSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    if ("FDSalary" %in% names(analysis_data)) {
      dt <- dt %>% formatCurrency('FDSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    dt
  })
  
  # Generate dynamic dominator UI based on available platforms
  output$dominator_ui <- renderUI({
    req(rv$simulation_results)
    
    # Create appropriate UI based on available platforms
    if(rv$has_draftkings && rv$has_fanduel) {
      # Both platforms available - use tabs
      tabsetPanel(
        id = "dominator_tabs",
        tabPanel(
          "DraftKings",
          fluidRow(
            box(width = 12,
                title = "DraftKings Dominator Points Analysis",
                DTOutput("dk_dominator_stats") %>% withSpinner(color = "black")
            )
          ),
          
          fluidRow(
            box(width = 12,
                title = "DraftKings Dominator Points Distribution",
                plotlyOutput("dk_dominator_dist", height = "1000px") %>% withSpinner(color = "black")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "DraftKings Dominator Points by Position",
                plotlyOutput("dk_points_by_position", height = "700px") %>% withSpinner(color = "black")
            )
          )
        ),
        tabPanel(
          "FanDuel",
          fluidRow(
            box(width = 12,
                title = "FanDuel Dominator Points Analysis",
                DTOutput("fd_dominator_stats") %>% withSpinner(color = "black")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Dominator Points Distribution",
                plotlyOutput("fd_dominator_dist", height = "800px") %>% withSpinner(color = "black")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Dominator Points by Position",
                plotlyOutput("fd_points_by_position", height = "700px") %>% withSpinner(color = "black")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Lap Points by Position",
                plotlyOutput("fd_lap_points_by_position") %>% withSpinner(color = "black")
            )
          )
        )
      )
    } else if(rv$has_draftkings) {
      # Only DraftKings available
      tagList(
        fluidRow(
          box(width = 12,
              title = "DraftKings Dominator Points Analysis",
              DTOutput("dk_dominator_stats") %>% withSpinner(color = "black")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Dominator Points Distribution",
              plotlyOutput("dk_dominator_dist", height = "1000px") %>% withSpinner(color = "black")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Dominator Points by Position",
              plotlyOutput("dk_points_by_position", height = "700px") %>% withSpinner(color = "black")
          )
        )
      )
    } else {
      # No platforms available
      fluidRow(
        box(
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          title = "No Fantasy Platforms Available",
          "No fantasy platform data was detected in your input file. Please ensure your file contains the necessary DraftKings or FanDuel columns."
        )
      )
    }
  })
  
  output$fantasy_ui <- renderUI({
    req(rv$simulation_results)
    
    # Create appropriate UI based on available platforms
    if(rv$has_draftkings && rv$has_fanduel) {
      # Both platforms available - use tabs
      tabsetPanel(
        id = "fantasy_tabs",
        tabPanel(
          "DraftKings",
          fluidRow(
            box(width = 12,
                title = "DraftKings Fantasy Point Projections",
                # Download buttons at the top
                div(style = "text-align: right; margin-bottom: 15px;",
                    downloadButton('downloadResults', 'Download Full Simulation Results', class = "btn-default"),
                    downloadButton('download_dk_fantasy_projections', 'Download DK Projections', class = "btn-default"),
                    downloadButton("download_dk_data", "Download DK Sim Scores", class = "btn-primary")
                ),
                DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "DraftKings Fantasy Points vs Salary",
                plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "DraftKings Fantasy Points Distribution",
                plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          )
        ),
        tabPanel(
          "FanDuel",
          fluidRow(
            box(width = 12,
                title = "FanDuel Fantasy Point Projections",
                # Download buttons at the top
                div(style = "text-align: right; margin-bottom: 15px;",
                    downloadButton('downloadResults2', 'Download Full Simulation Results', class = "btn-default"),
                    downloadButton('download_fd_fantasy_projections', 'Download FD Projections', class = "btn-default"),
                    downloadButton("download_fd_data", "Download FD Sim Scores", class = "btn-primary")
                ),
                DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Fantasy Points vs Salary",
                plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Fantasy Points Distribution",
                plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          )
        )
      )
    } else if(rv$has_draftkings) {
      # Only DraftKings available
      tagList(
        fluidRow(
          box(width = 12,
              title = "DraftKings Fantasy Point Projections",
              # Download buttons at the top
              div(style = "text-align: right; margin-bottom: 15px;",
                  downloadButton('downloadResults', 'Download Full Simulation Results', class = "btn-default"),
                  downloadButton('download_dk_fantasy_projections', 'Download DK Projections', class = "btn-default"),
                  downloadButton("download_dk_data", "Download DK Sim Scores", class = "btn-primary")
              ),
              DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Fantasy Points vs Salary",
              plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Fantasy Points Distribution",
              plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
          )
        )
      )
    } else if(rv$has_fanduel) {
      # Only FanDuel available
      tagList(
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Point Projections",
              # Download buttons at the top
              div(style = "text-align: right; margin-bottom: 15px;",
                  downloadButton('downloadResults2', 'Download Full Simulation Results', class = "btn-default"),
                  downloadButton('download_fd_fantasy_projections', 'Download FD Projections', class = "btn-default"),
                  downloadButton("download_fd_data", "Download FD Sim Scores", class = "btn-primary")
              ),
              DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Points vs Salary",
              plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Points Distribution",
              plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
          )
        )
      )
    } else {
      # No platforms available
      fluidRow(
        box(
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          title = "No Fantasy Platforms Available",
          "No fantasy platform data was detected in your input file. Please ensure your file contains the necessary DraftKings or FanDuel columns."
        )
      )
    }
  })

  # Create position boxplot
  output$position_box <- renderPlotly({
    req(rv$simulation_results)
    
    # Get all unique drivers and starting positions
    drivers_info <- unique(rv$simulation_results[, c("Name", "Starting")])
    
    # Order by starting position
    drivers_info <- drivers_info[order(drivers_info$Starting), ]
    
    # Get ordered list of driver names
    ordered_drivers <- drivers_info$Name
    
    # Plot with all drivers, ordered by starting position
    p <- ggplot(rv$simulation_results, aes(x = factor(Name, levels = ordered_drivers), 
                                           y = FinishPosition,
                                           fill = Name)) +
      geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 2) +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
        legend.position = "none"  # Hide legend to reduce clutter
      ) +
      labs(
        x = "Driver", 
        y = "Finish Position",
        title = NULL
      )
    
    ggplotly(p, height = 1000) %>%  
      layout(
        showlegend = FALSE,
        margin = list(
          l = 150,
          r = 40,
          b = 60,
          t = 30,
          pad = 5
        ),
        font = list(
          family = "Arial",
          size = 12
        )
      )
  })
  
  # DraftKings Dominator Stats
  output$dk_dominator_stats <- renderDT({
    req(rv$dk_dominator_analysis)
    
    dt <- datatable(
      rv$dk_dominator_analysis,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(3, 'desc')),  # Sort by Avg_Dom
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-center'
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency('DKSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('Avg_Dom', 'Median_Dom', 'Max_Dom', 
                    'Avg_DomRank', 'Median_DomRank', 
                    'Top_DomRate', 'Top3_DomRate', 'Top5_DomRate',
                    'Top10_DomRate'), 
                  digits = 1)
    
    dt
  })
  
  # FanDuel Dominator Stats
  output$fd_dominator_stats <- renderDT({
    req(rv$fd_dominator_analysis)
    
    dt <- datatable(
      rv$fd_dominator_analysis,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(3, 'desc')),  # Sort by Avg_Dom
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-center'
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency('FDSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('Avg_Dom', 'Median_Dom', 'Max_Dom', 
                    'Avg_DomRank', 'Median_DomRank', 
                    'Top_DomRate', 'Top3_DomRate', 'Top5_DomRate'), 
                  digits = 1)
    
    dt
  })
  
  # DraftKings Dominator distribution plot
  output$dk_dominator_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Calculate median DKDominatorPoints for each driver
    driver_medians <- rv$simulation_results %>%
      group_by(Name) %>%
      summarize(median_points = median(DKDominatorPoints, na.rm = TRUE)) %>%
      filter(median_points > 0)
    
    # Filter original data to only include drivers with median > 0
    plot_data <- rv$simulation_results %>%
      filter(Name %in% driver_medians$Name)
    
    # Create box and whisker plot
    p <- ggplot(plot_data, aes(x = reorder(Name, DKDominatorPoints, median), y = DKDominatorPoints, fill = Name)) +
      geom_boxplot(outlier.shape = NA) +  # Hide outliers if too noisy
      coord_flip()+
      theme_minimal() +
      labs(x = "Driver", y = "Dominator Points") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    ggplotly(p, height = 1000)
  })
  
  # FanDuel Dominator distribution plot
  output$fd_dominator_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Calculate median DKDominatorPoints for each driver
    driver_means <- rv$simulation_results %>%
      group_by(Name) %>%
      summarize(mean_points = mean(FDDominatorPoints, na.rm = TRUE)) %>%
      filter(mean_points > 0)
    
    # Filter original data to only include drivers with median > 0
    plot_data <- rv$simulation_results %>%
      filter(Name %in% driver_means$Name)
    
    # Create box and whisker plot
    p <- ggplot(plot_data, aes(x = reorder(Name, FDDominatorPoints, mean), y = FDDominatorPoints, fill = Name)) +
      geom_boxplot(outlier.shape = NA) +  # Hide outliers if too noisy
      coord_flip()+
      theme_minimal() +
      labs(x = "Driver", y = "Dominator Points") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    ggplotly(p, height = 800)
  })
  
  # DraftKings Points by Position
  output$dk_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    p <- ggplot(rv$simulation_results, 
                aes(x = factor(FinishPosition), y = DKDominatorPoints)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      geom_smooth(method = "lm", color = "red", se = FALSE, aes(group = 1)) +
      theme_minimal() +
      labs(x = "Finish Position", y = "Dominator Points")
    
    ggplotly(p, height = 700)
  })
  
  # FanDuel Points by Position
  output$fd_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    p <- ggplot(rv$simulation_results, 
                aes(x = factor(FinishPosition), y = FDDominatorPoints)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      geom_smooth(method = "lm", color = "red", se = FALSE, aes(group = 1)) +
      theme_minimal() +
      labs(x = "Finish Position", y = "Dominator Points")
    
    ggplotly(p, height = 700)
  })
  
  # FanDuel Lap Points by Position
  output$fd_lap_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    p <- ggplot(rv$simulation_results, 
                aes(x = factor(FinishPosition), y = FDLapPoints)) +
      geom_boxplot(fill = "lightgreen", color = "darkgreen") +
      theme_minimal() +
      labs(x = "Finish Position", y = "Lap Points")
    
    ggplotly(p)
  })
  
  # DraftKings Fantasy Projections
  output$dk_fantasy_projections <- renderDT({
    req(rv$dk_fantasy_analysis)
    
    dt <- datatable(
      rv$dk_fantasy_analysis,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(4, 'desc')),  # Sort by median
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-center'
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency('DKSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('Median_Fantasy_Pts', 
                    'FP_90thPct', 'PPD'), 
                  digits = 1)
    
    dt
  })
  
  # FanDuel Fantasy Projections
  output$fd_fantasy_projections <- renderDT({
    req(rv$fd_fantasy_analysis)
    
    dt <- datatable(
      rv$fd_fantasy_analysis,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(4, 'desc')),  # Sort by median
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-center'
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency('FDSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('Median_Fantasy_Pts', 
                    'FP_90thPct', 'PPD'), 
                  digits = 1)
    
    dt
  })
  
  # DraftKings Fantasy Points vs Salary
  output$dk_fantasy_points_salary <- renderPlotly({
    req(rv$dk_fantasy_analysis)
    
    plot_data <- rv$dk_fantasy_analysis
    
    # Combine Name and Starting into a label
    plot_data$label <- paste0(plot_data$Name, " (Start: ", plot_data$Starting, ")")
    
    p <- ggplot(plot_data, aes(
      x = DKSalary,
      y = Median_Fantasy_Pts,
      size = DKOP,
      text = label
    )) +
      geom_point(alpha = 0.7, color = "#2c7fb8") +
      geom_smooth(aes(x = DKSalary, y = Median_Fantasy_Pts), method = "lm", se = FALSE, color = "darkblue") +
      theme_minimal() +
      labs(x = "DK Salary", y = "Median Fantasy Points", size = "DKOP")
    
    ggplotly(p, height = 800, tooltip = c("text", "x", "y", "size")) %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        hovermode = "closest"
      )
  })
  
  # FanDuel Fantasy Points vs Salary
  output$fd_fantasy_points_salary <- renderPlotly({
    req(rv$fd_fantasy_analysis)
    
    plot_data <- rv$fd_fantasy_analysis
    
    # Combine Name and Starting into a label
    plot_data$label <- paste0(plot_data$Name, " (Start: ", plot_data$Starting, ")")
    
    p <- ggplot(plot_data, aes(
      x = FDSalary,
      y = Median_Fantasy_Pts,
      size = FDOP,
      text = label
    )) +
      geom_point(alpha = 0.7, color = "#2c7fb8") +
      geom_smooth(aes(x = FDSalary, y = Median_Fantasy_Pts), method = "lm", se = FALSE, color = "darkblue") +
      theme_minimal() +
      labs(x = "FD Salary", y = "Median Fantasy Points", size = "FDOP")
    
    ggplotly(p, height = 800, tooltip = c("text", "x", "y", "size")) %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        hovermode = "closest"
      )
  })
  
  # DraftKings Fantasy Points Distribution
  output$dk_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Get unique driver salary info
    driver_salaries <- rv$simulation_results %>%
      distinct(Name, DKSalary)
    
    # Order driver names by ascending DKSalary
    ordered_names <- driver_salaries %>%
      arrange(DKSalary) %>%
      pull(Name)
    
    plot_data <- rv$simulation_results %>%
      filter(Name %in% ordered_names)
    
    p <- ggplot(plot_data, aes(x = factor(Name, levels = ordered_names),
                               y = DKFantasyPoints,
                               fill = Name)) +
      geom_boxplot(outlier.alpha = 0.25) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver", y = "Fantasy Points") +
      theme(legend.position = "none")  # Hide legend to avoid clutter
    
    ggplotly(p, height = 700, tooltip = c("x", "y"))
  })
  
  # FanDuel Fantasy Points Distribution
  output$fd_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Get unique driver salary info
    driver_salaries <- rv$simulation_results %>%
      distinct(Name, FDSalary)
    
    # Order driver names by ascending DKSalary
    ordered_names <- driver_salaries %>%
      arrange(FDSalary) %>%
      pull(Name)
    
    plot_data <- rv$simulation_results %>%
      filter(Name %in% ordered_names)
    
    p <- ggplot(plot_data, aes(x = factor(Name, levels = ordered_names),
                               y = FDFantasyPoints,
                               fill = Name)) +
      geom_boxplot(outlier.alpha = 0.25) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver", y = "Fantasy Points") +
      theme(legend.position = "none")  # Hide legend to avoid clutter
    
    ggplotly(p, height = 700, tooltip = c("x", "y")) 
  })
  
  


  # Download handlers
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("nascar_simulation_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rv$simulation_results, file, row.names = FALSE)
    }
  )
  
  output$downloadResults2 <- downloadHandler(
    filename = function() {
      paste("nascar_simulation_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rv$simulation_results, file, row.names = FALSE)
    }
  )
  

  
  output$download_dk_fantasy_projections <- downloadHandler(
    filename = function() {
      paste("dk_fantasy_projections_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rv$dk_fantasy_analysis, file, row.names = FALSE)
    }
  )
  
  output$download_fd_fantasy_projections <- downloadHandler(
    filename = function() {
      paste("fd_fantasy_projections_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rv$fd_fantasy_analysis, file, row.names = FALSE)
    }
  )
  

  output$download_dk_data <- downloadHandler(
    filename = function() {
      paste("nascar_dk_simscores_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds", sep="")
    },
    content = function(file) {
      # Get only DraftKings simulation results - need full fantasy points for optimization
      dk_sim_data <- rv$simulation_results[!is.na(rv$simulation_results$DKSalary) & 
                                             rv$simulation_results$DKSalary > 0, 
                                           c("SimID", "Name", "DKName", "DKSalary", "DKOP", 
                                             "DKFantasyPoints")]
      
      dk_export_data <- list(
        simulation_results = dk_sim_data,          # Full sim results for lineup optimization
        platform = "DraftKings",
        roster_size = 6,                          
        salary_cap = 50000,                       
        n_sims = input$n_sims,
        export_timestamp = Sys.time()
      )
      saveRDS(dk_export_data, file)
    }
  )
  
  output$download_fd_data <- downloadHandler(
    filename = function() {
      paste("nascar_fd_simscores_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds", sep="")
    },
    content = function(file) {
      # Get only FanDuel simulation results
      fd_sim_data <- rv$simulation_results[!is.na(rv$simulation_results$FDSalary) & 
                                             rv$simulation_results$FDSalary > 0,
                                           c("SimID", "Name", "FDName", "FDSalary", "FDOP", 
                                             "FDFantasyPoints")]
      
      fd_export_data <- list(
        simulation_results = fd_sim_data,          # Full sim results for lineup optimization
        platform = "FanDuel", 
        roster_size = 5,                          
        salary_cap = 50000,                       
        n_sims = input$n_sims,
        export_timestamp = Sys.time()
      )
      saveRDS(fd_export_data, file)
    }
  )
  
  # Memory cleanup functions
  observe({
    invalidateLater(180000) # 3 minutes
    gc(verbose = FALSE, full = TRUE)
  })
  

  # Clean up on session end
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
}


# Run the application
shinyApp(ui = ui, server = server)