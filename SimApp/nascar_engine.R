# ============================================================================
# NASCAR SIMULATION ENGINE
# Golden Ticket Sims - Universal Template
# ============================================================================

library(data.table)

# ============================================================================
# MAIN SIMULATION FUNCTION (Required by universal app)
# ============================================================================

#' Run NASCAR Simulation
#' @param input_data List of data.frames from Excel sheets (Driver, Race_Weights, Race_Profiles)
#' @param n_sims Number of simulations to run
#' @param config Sport configuration from sport_configs_universal.R
#' @param progress_callback Optional function to report progress (for Shiny apps)
#' @return List with sim_results and metadata
run_nascar_simulation <- function(input_data, n_sims, config, progress_callback = NULL) {
  
  # Extract data from input
  driver_data <- as.data.table(input_data$Driver)
  race_weights <- as.data.table(input_data$Race_Weights)
  race_profiles <- as.data.table(input_data$Race_Profiles)
  
  # Validate required columns
  # Core columns (always required)
  core_required <- c("Name", "W", "T3", "T5", "T10", "T15", "T20", "T25", "T30",
                     "Starting", "team", "car")
  
  # DraftKings columns (always required since you always have DK data)
  dk_required <- c("DKSalary", "DKID", "DKOP", "DKMax")
  
  # FanDuel columns (optional - not all races have FD contests)
  fd_optional <- c("FDSalary", "FDID", "FDName", "FDOP", "FDMax")
  
  # Check for missing core and DK columns
  required_cols <- c(core_required, dk_required)
  missing_cols <- setdiff(required_cols, names(driver_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in Driver sheet: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check if FanDuel data is present
  has_fd <- all(fd_optional %in% names(driver_data))
  
  if (!has_fd) {
    cat("Note: FanDuel columns not found - FD scoring will be skipped\n")
    # Add placeholder FD columns so the rest of the code doesn't break
    driver_data[, FDSalary := 0]
    driver_data[, FDID := ""]
    driver_data[, FDName := Name]
    driver_data[, FDOP := 0]
    driver_data[, FDMax := 0]
  }
  
  # Rename columns to standardize (DKOP -> DKOwn, team -> Team, car -> Car)
  # Use set() to avoid copying and preserve data.table reference
  if ("DKOP" %in% names(driver_data)) setnames(driver_data, "DKOP", "DKOwn")
  if ("FDOP" %in% names(driver_data)) setnames(driver_data, "FDOP", "FDOwn")
  if ("team" %in% names(driver_data)) setnames(driver_data, "team", "Team")
  if ("car" %in% names(driver_data)) setnames(driver_data, "car", "Car")
  
  # After setnames, make a fresh copy to reset data.table reference
  driver_data <- copy(driver_data)
  
  cat(sprintf("\n[NASCAR SIMULATION]\n"))
  cat(sprintf("Drivers: %d\n", nrow(driver_data)))
  cat(sprintf("Simulations: %s\n", format(n_sims, big.mark = ",")))
  cat(sprintf("Race Profiles: %d\n", nrow(race_profiles)))
  cat(sprintf("Platforms: DK%s\n\n", ifelse(has_fd, " + FD", " only")))
  
  # ========================================================================
  # STEP 1: Simulate Finish Positions
  # ========================================================================
  
  cat("[STEP 1/3] Generating finish positions...\n")
  if (!is.null(progress_callback)) {
    progress_callback(detail = "Generating finish positions...", value = 0.1)
  }
  start_time <- Sys.time()
  
  driver_distributions <- precompute_driver_distributions(driver_data)
  all_finish_positions <- simulate_finish_positions_vectorized(driver_distributions, n_sims)
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("  b Finish positions completed in %.1f seconds\n\n", elapsed))
  
  # ========================================================================
  # STEP 2: Calculate Fantasy Points (DK and FD)
  # ========================================================================
  
  cat("[STEP 2/3] Calculating fantasy points...\n")
  if (!is.null(progress_callback)) {
    progress_callback(detail = "Calculating fantasy points...", value = 0.3)
  }
  start_time <- Sys.time()
  
  scoring_systems <- create_scoring_system()
  
  # SPEED OPTIMIZATION: Pre-cache race profiles by RaceID for fast lookup
  race_profiles_by_race <- split(race_profiles, race_profiles$RaceID)
  
  all_results <- list()
  update_freq <- max(1, floor(n_sims / 20))
  
  for (sim_id in 1:n_sims) {
    # Progress updates
    if (sim_id %% update_freq == 0 || sim_id == n_sims) {
      pct <- round(100 * sim_id / n_sims)
      cat(sprintf("\r  Progress: %3d%% (%d / %d)", pct, sim_id, n_sims))
      flush.console()
      
      # Report to Shiny UI if callback provided
      if (!is.null(progress_callback)) {
        progress_val <- 0.3 + (0.6 * (sim_id / n_sims))  # 30% to 90%
        progress_callback(
          detail = sprintf("Simulation %d of %d (%d%%)", sim_id, n_sims, pct),
          value = progress_val
        )
      }
    }
    
    # Create race result for this simulation (DK data always included)
    race_result <- data.table(
      SimID = sim_id,
      Name = driver_data$Name,
      Starting = driver_data$Starting,
      FinishPosition = all_finish_positions[, sim_id],
      DKSalary = driver_data$DKSalary,
      DKID = driver_data$DKID,
      DKOwn = driver_data$DKOwn,
      DKMax = driver_data$DKMax,
      Team = driver_data$Team,
      Car = driver_data$Car
    )
    
    # Add FD data only if present
    if (has_fd) {
      race_result[, FDSalary := driver_data$FDSalary]
      race_result[, FDID := driver_data$FDID]
      race_result[, FDName := driver_data$FDName]
      race_result[, FDOwn := driver_data$FDOwn]
      race_result[, FDMax := driver_data$FDMax]
    }
    
    # Pre-allocate space for columns that will be added
    setalloccol(race_result)
    
    # Assign dominator points from race profiles (using cached profiles)
    race_result <- assign_dominator_points_from_profiles_cached(
      race_result, race_weights, race_profiles_by_race, "DK"
    )
    
    # Only calculate FD dominator points if FD data is present
    if (has_fd) {
      race_result <- assign_dominator_points_from_profiles_cached(
        race_result, race_weights, race_profiles_by_race, "FD"
      )
    }
    
    # Calculate total fantasy points
    race_result <- calculate_fantasy_points(race_result, scoring_systems, has_fd)
    
    all_results[[sim_id]] <- race_result
  }
  
  cat("\n")
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("  b Fantasy points completed in %.1f seconds\n\n", elapsed))
  
  # ========================================================================
  # STEP 3: Combine Results
  # ========================================================================
  
  cat("[STEP 3/3] Combining results...\n")
  if (!is.null(progress_callback)) {
    progress_callback(detail = "Combining results...", value = 0.95)
  }
  start_time <- Sys.time()
  
  combined_results <- rbindlist(all_results)
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("  b Results combined in %.1f seconds\n\n", elapsed))
  
  if (!is.null(progress_callback)) {
    progress_callback(detail = "Simulation complete!", value = 1.0)
  }
  
  # ========================================================================
  # PREPARE OUTPUT (Match Universal Template Contract)
  # ========================================================================
  
  # Simulation results (long format)
  sim_results <- combined_results[, .(
    SimID,
    Player = Name,  # Rename to standard "Player" column
    DKScore = DKFantasyPoints,
    FDScore = FDFantasyPoints
  )]
  
  # Player metadata (one row per player)
  metadata <- unique(driver_data[, .(
    Player = Name,  # Rename to standard "Player" column
    DKSalary,
    DKID,
    DKOwn,
    FDSalary,
    FDID,
    FDName,
    FDOwn,
    Starting,
    Team,
    Car
  )])
  
  cat("[SIMULATION COMPLETE]\n\n")
  
  return(list(
    sim_results = sim_results,      # Summary for optimization
    metadata = metadata,             # Player metadata  
    full_results = combined_results, # Full data for visualizations
    has_fd = has_fd                  # Flag indicating if FD data was present
  ))
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Precompute driver probability distributions
precompute_driver_distributions <- function(driver_data) {
  drivers <- driver_data$Name
  n_drivers <- length(drivers)
  
  # Position ranges
  position_ranges <- list(
    list(min = 1, max = 1, prob_col = "W"),
    list(min = 2, max = 3, prob_col = "T3"),
    list(min = 4, max = 5, prob_col = "T5"),
    list(min = 6, max = 10, prob_col = "T10"),
    list(min = 11, max = 15, prob_col = "T15"),
    list(min = 16, max = 20, prob_col = "T20"),
    list(min = 21, max = 25, prob_col = "T25"),
    list(min = 26, max = 30, prob_col = "T30"),
    list(min = 31, max = n_drivers, prob_col = "Below30")
  )
  
  # Calculate marginal probabilities
  for (i in seq_along(position_ranges)) {
    range_info <- position_ranges[[i]]
    
    if (range_info$prob_col == "Below30") {
      # Below 30 = 1 - sum of all other probabilities
      driver_data[, Below30 := pmax(0, 1 - (W + T3 + T5 + T10 + T15 + T20 + T25 + T30))]
    }
  }
  
  # Build cumulative distributions
  distributions <- list()
  
  for (i in 1:n_drivers) {
    driver <- drivers[i]
    cumulative_prob <- 0
    breakpoints <- numeric(0)
    
    for (range_info in position_ranges) {
      prob <- driver_data[[range_info$prob_col]][i]
      cumulative_prob <- cumulative_prob + prob
      breakpoints <- c(breakpoints, cumulative_prob)
    }
    
    distributions[[driver]] <- list(
      breakpoints = breakpoints,
      position_ranges = position_ranges
    )
  }
  
  return(distributions)
}


#' Simulate finish positions (vectorized)
simulate_finish_positions_vectorized <- function(driver_distributions, n_sims) {
  drivers <- names(driver_distributions)
  n_drivers <- length(drivers)
  
  # Pre-allocate matrix
  sampled_positions <- matrix(0, nrow = n_drivers, ncol = n_sims)
  rownames(sampled_positions) <- drivers
  
  # Sample for each driver
  for (i in 1:n_drivers) {
    driver <- drivers[i]
    dist <- driver_distributions[[driver]]
    
    # Generate random values
    random_values <- runif(n_sims)
    
    # Find which range each random value falls into
    for (sim_id in 1:n_sims) {
      rv <- random_values[sim_id]
      range_idx <- which(rv <= dist$breakpoints)[1]
      
      if (is.na(range_idx)) range_idx <- length(dist$position_ranges)
      
      range_info <- dist$position_ranges[[range_idx]]
      
      # Sample uniformly within range
      sampled_positions[i, sim_id] <- sample(
        range_info$min:range_info$max, 
        size = 1
      )
    }
  }
  
  # Rank to get final positions (1st, 2nd, 3rd, etc.)
  final_positions <- apply(sampled_positions, 2, function(col) {
    rank(col, ties.method = "random")
  })
  
  rownames(final_positions) <- drivers
  
  return(final_positions)
}


#' Assign dominator points from race profiles
assign_dominator_points_from_profiles <- function(race_result, race_weights, 
                                                  race_profiles, platform) {
  
  # Ensure data.table has proper allocation
  setalloccol(race_result)
  
  # Select random race based on weights
  race_id <- sample(
    race_weights$RaceID,
    size = 1,
    prob = race_weights$Weight
  )
  
  # Get profiles for selected race
  profiles <- race_profiles[race_profiles$RaceID == race_id, ]
  
  if (nrow(profiles) == 0) {
    # No profiles for this race - assign 0 points using set()
    col_name <- paste0(platform, "DominatorPoints")
    set(race_result, j = col_name, value = 0)
    return(race_result)
  }
  
  # Get dominator points column
  dom_col <- paste0(platform, "DomPoints")
  max_col <- paste0(platform, "Max")
  
  # Check if dominator column exists (FD might not be present)
  if (!dom_col %in% names(profiles)) {
    # No dominator points for this platform - assign 0 points
    col_name <- paste0(platform, "DominatorPoints")
    set(race_result, j = col_name, value = 0)
    return(race_result)
  }
  
  # Calculate distances (vectorized)
  # NOTE: Your Excel uses StartPos and FinPos, not Starting and FinishPosition
  driver_finishes <- race_result$FinishPosition
  driver_starts <- race_result$Starting
  
  profile_finishes <- profiles$FinPos  # Your column name
  profile_starts <- profiles$StartPos  # Your column name
  
  # Distance matrix: rows = drivers, cols = profiles
  finish_diff_matrix <- outer(driver_finishes, profile_finishes, function(x, y) abs(x - y))
  start_diff_matrix <- outer(driver_starts, profile_starts, function(x, y) abs(x - y))
  distance_matrix <- sqrt(finish_diff_matrix^2 + start_diff_matrix^2)
  
  # Find closest profile for each driver
  closest_profile_idx <- apply(distance_matrix, 1, which.min)
  
  # Assign dominator points from closest profile
  dom_points <- profiles[[dom_col]][closest_profile_idx]
  
  # Apply driver-specific ceiling (DKMax or FDMax)
  max_allowed <- race_result[[max_col]]
  dom_points <- pmin(dom_points, max_allowed)
  
  # Use set() instead of [[ to avoid allocation issues
  col_name <- paste0(platform, "DominatorPoints")
  set(race_result, j = col_name, value = dom_points)
  
  return(race_result)
}


#' Create scoring systems for DK and FD
create_scoring_system <- function() {
  list(
    DK = data.table(
      Position = 1:41,
      Points = c(45, 42, 41, 40, 39, 38, 37, 36, 35, 34,
                 32, 31, 30, 29, 28, 27, 26, 25, 24, 23,
                 21, 20, 19, 18, 17, 16, 15, 14, 13, 12,
                 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
    ),
    FD = data.table(
      Position = 1:41,
      Points = c(43, 40, 38, 37, 36, 35, 34, 33, 32, 31,
                 30, 29, 28, 27, 26, 25, 24, 23, 22, 21,
                 20, 19, 18, 17, 16, 15, 14, 13, 12, 11,
                 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
    )
  )
}


#' Calculate total fantasy points
calculate_fantasy_points <- function(race_result, scoring_systems, has_fd = TRUE) {
  
  # DraftKings (always calculated)
  dk_finish_points <- scoring_systems$DK$Points[race_result$FinishPosition]
  dk_position_diff <- race_result$Starting - race_result$FinishPosition
  
  # Use set() instead of := to avoid internal reference warnings
  set(race_result, j = "DKFantasyPoints", 
      value = dk_finish_points + race_result$DKDominatorPoints + dk_position_diff)
  
  # FanDuel (only if has_fd is TRUE)
  if (has_fd) {
    fd_finish_points <- scoring_systems$FD$Points[race_result$FinishPosition]
    fd_position_diff <- race_result$Starting - race_result$FinishPosition
    
    # Use set() instead of := to avoid internal reference warnings
    set(race_result, j = "FDFantasyPoints", 
        value = fd_finish_points + race_result$FDDominatorPoints + (fd_position_diff)*.5)
  } else {
    # Set FD fantasy points to 0 when not available
    set(race_result, j = "FDFantasyPoints", value = 0)
  }
  
  return(race_result)
}

# ============================================================================
# VISUALIZATION HELPER FUNCTIONS
# ============================================================================

#' Get Team Colors for NASCAR Flair
#' @return List of team color schemes
get_team_colors <- function() {
  list(
    "Hendrick Motorsports" = list(
      primary = "#002244",   # Navy blue
      secondary = "#FFE500", # Gold (matches GTS theme!)
      text = "#FFFFFF"
    ),
    "Joe Gibbs Racing" = list(
      primary = "#FF6600",   # Orange
      secondary = "#FFCC00", # Yellow
      text = "#000000"
    ),
    "23XI Racing" = list(
      primary = "#000000",   # Black
      secondary = "#FFD700", # Gold
      text = "#FFFFFF"
    ),
    "RFK Racing" = list(
      primary = "#003DA5",   # Blue
      secondary = "#E4002B", # Red
      text = "#FFFFFF"
    ),
    "Trackhouse Racing" = list(
      primary = "#0066CC",   # Blue
      secondary = "#FFFFFF", # White
      text = "#000000"
    ),
    "Spire Motorsports" = list(
      primary = "#6B2C91",   # Purple
      secondary = "#FFFFFF", # White
      text = "#FFFFFF"
    ),
    "default" = list(
      primary = "#404040",   # Gray
      secondary = "#FFE500", # GTS Gold
      text = "#FFFFFF"
    )
  )
}


#' Create Car Number Badge HTML
#' @param car_num Car number
#' @param team Team name
#' @return HTML string for styled badge
create_car_badge <- function(car_num, team) {
  team_colors <- get_team_colors()
  colors <- team_colors[[team]]
  if (is.null(colors)) colors <- team_colors[["default"]]
  
  sprintf(
    '<span style="display:inline-block; width:32px; height:32px; border-radius:50%%; 
     background-color:%s; color:%s; border:2px solid %s; 
     font-weight:700; font-size:13px; text-align:center; line-height:32px; margin-right:6px;">
     %s</span>',
    colors$primary, colors$text, colors$secondary, car_num
  )
}


# ============================================================================
# DATA AGGREGATION FUNCTIONS
# ============================================================================

#' Calculate Finish Rates from Simulation Results
#' @param sim_results data.table with simulation results
#' @param driver_data data.table with driver input data (includes expected rates)
#' @return data.table with finish rates and validation
calculate_finish_rates <- function(sim_results, driver_data) {
  
  # Ensure proper data.table allocation
  if (!is.data.table(sim_results)) sim_results <- as.data.table(sim_results)
  if (!is.data.table(driver_data)) driver_data <- as.data.table(driver_data)
  setDT(sim_results)
  setDT(driver_data)
  
  # Aggregate sim results
  finish_rates <- sim_results[, .(
    Sim_W = mean(FinishPosition == 1),
    Sim_T3 = mean(FinishPosition <= 3),
    Sim_T5 = mean(FinishPosition <= 5),
    Sim_T10 = mean(FinishPosition <= 10),
    Sim_T15 = mean(FinishPosition <= 15),
    Sim_T20 = mean(FinishPosition <= 20)
  ), by = Name]
  
  # Join with driver data to get input rates and metadata
  finish_rates <- merge(
    finish_rates,
    driver_data[, .(Name, Car, Team, Starting, 
                    Input_W = W, Input_T3 = T3, Input_T5 = T5, 
                    Input_T10 = T10, Input_T15 = T15, Input_T20 = T20)],
    by = "Name",
    all.x = TRUE
  )
  
  # Pre-allocate for new columns
  setalloccol(finish_rates)
  
  # Calculate differences using set() to avoid allocation issues
  set(finish_rates, j = "Diff_W", value = finish_rates$Sim_W - finish_rates$Input_W)
  set(finish_rates, j = "Diff_T3", value = finish_rates$Sim_T3 - finish_rates$Input_T3)
  set(finish_rates, j = "Diff_T5", value = finish_rates$Sim_T5 - finish_rates$Input_T5)
  set(finish_rates, j = "Diff_T10", value = finish_rates$Sim_T10 - finish_rates$Input_T10)
  set(finish_rates, j = "Diff_T15", value = finish_rates$Sim_T15 - finish_rates$Input_T15)
  set(finish_rates, j = "Diff_T20", value = finish_rates$Sim_T20 - finish_rates$Input_T20)
  
  # Reorder columns
  setcolorder(finish_rates, c("Car", "Name", "Team", "Starting",
                              "Sim_W", "Input_W", "Diff_W",
                              "Sim_T3", "Input_T3", "Diff_T3",
                              "Sim_T5", "Input_T5", "Diff_T5",
                              "Sim_T10", "Input_T10", "Diff_T10",
                              "Sim_T15", "Input_T15", "Diff_T15",
                              "Sim_T20", "Input_T20", "Diff_T20"))
  
  # Order by starting position
  setorder(finish_rates, Starting)
  
  return(finish_rates)
}


#' Calculate Dominator Statistics
#' @param sim_results data.table with simulation results
#' @param driver_data data.table with driver metadata
#' @param platform "DK" or "FD"
#' @return data.table with dominator stats
calculate_dominator_stats <- function(sim_results, driver_data, platform = "DK") {
  
  # Ensure proper data.table allocation
  if (!is.data.table(sim_results)) sim_results <- as.data.table(sim_results)
  if (!is.data.table(driver_data)) driver_data <- as.data.table(driver_data)
  setDT(sim_results)
  setDT(driver_data)
  
  dom_col <- if (platform == "DK") "DKDominatorPoints" else "FDDominatorPoints"
  
  # Calculate stats
  dom_stats <- sim_results[, .(
    Avg_DomPts = mean(get(dom_col)),
    Median_DomPts = median(get(dom_col)),
    Max_DomPts = max(get(dom_col)),
    Dom_Rate = mean(get(dom_col) > 0)
  ), by = Name]
  
  # Calculate who had the most dominator points in each sim
  max_dom_by_sim <- sim_results[, .(
    MaxDom = max(get(dom_col))
  ), by = .(SimID, Name)]
  
  # Get the max for each sim
  max_values <- max_dom_by_sim[, .(MaxOverall = max(MaxDom)), by = SimID]
  max_dom_by_sim <- merge(max_dom_by_sim, max_values, by = "SimID")
  
  # Count how many times each driver had the max
  top1_dom <- max_dom_by_sim[MaxDom == MaxOverall, .N, by = Name]
  total_sims <- length(unique(sim_results$SimID))
  
  # Use set() to avoid allocation issues
  setalloccol(top1_dom)
  set(top1_dom, j = "Top1_DomRate", value = top1_dom$N / total_sims)
  
  # Merge
  dom_stats <- merge(dom_stats, top1_dom[, .(Name, Top1_DomRate)], 
                     by = "Name", all.x = TRUE)
  dom_stats[is.na(Top1_DomRate), Top1_DomRate := 0]
  
  # Join with driver metadata
  dom_stats <- merge(
    dom_stats,
    driver_data[, .(Name, Car, Team, Starting)],
    by = "Name",
    all.x = TRUE
  )
  
  # Reorder columns
  setcolorder(dom_stats, c("Car", "Name", "Team", "Starting",
                           "Avg_DomPts", "Median_DomPts", "Max_DomPts",
                           "Dom_Rate", "Top1_DomRate"))
  
  # Order by median dominator points
  setorder(dom_stats, -Median_DomPts)
  
  return(dom_stats)
}


#' Calculate Projection Percentiles
#' @param sim_results data.table with simulation results
#' @param driver_data data.table with driver metadata and salaries
#' @return data.table with percentile projections
calculate_projections <- function(sim_results, driver_data) {
  
  # Ensure proper data.table allocation
  if (!is.data.table(sim_results)) sim_results <- as.data.table(sim_results)
  if (!is.data.table(driver_data)) driver_data <- as.data.table(driver_data)
  setDT(sim_results)
  setDT(driver_data)
  
  # Calculate DK projections
  dk_proj <- sim_results[, .(
    DK_Floor = quantile(DKFantasyPoints, 0.10),
    DK_P25 = quantile(DKFantasyPoints, 0.25),
    DK_Median = median(DKFantasyPoints),
    DK_P75 = quantile(DKFantasyPoints, 0.75),
    DK_Ceiling = quantile(DKFantasyPoints, 0.90),
    DK_Mean = mean(DKFantasyPoints),
    DK_StdDev = sd(DKFantasyPoints)
  ), by = Name]
  
  # Calculate FD projections
  fd_proj <- sim_results[, .(
    FD_Floor = quantile(FDFantasyPoints, 0.10),
    FD_P25 = quantile(FDFantasyPoints, 0.25),
    FD_Median = median(FDFantasyPoints),
    FD_P75 = quantile(FDFantasyPoints, 0.75),
    FD_Ceiling = quantile(FDFantasyPoints, 0.90),
    FD_Mean = mean(FDFantasyPoints),
    FD_StdDev = sd(FDFantasyPoints)
  ), by = Name]
  
  # Merge projections
  projections <- merge(dk_proj, fd_proj, by = "Name")
  
  # Join with driver metadata and salaries
  projections <- merge(
    projections,
    driver_data[, .(Name, Car, Team, Starting, DKSalary, FDSalary)],
    by = "Name",
    all.x = TRUE
  )
  
  # Reorder columns
  setcolorder(projections, c("Car", "Name", "Team", "Starting",
                             "DKSalary", "DK_Floor", "DK_P25", "DK_Median", 
                             "DK_P75", "DK_Ceiling", "DK_Mean", "DK_StdDev",
                             "FDSalary", "FD_Floor", "FD_P25", "FD_Median", 
                             "FD_P75", "FD_Ceiling", "FD_Mean", "FD_StdDev"))
  
  # Order by starting position
  setorder(projections, Starting)
  
  return(projections)
}


# ============================================================================
# VISUALIZATION UI FUNCTIONS (for Shiny)
# ============================================================================

#' Create Finish Rates Table UI
#' @param finish_rates data.table from calculate_finish_rates()
#' @return DT::datatable object
create_finish_rates_table <- function(finish_rates) {
  
  require(DT)
  
  # Ensure proper data.table
  if (!is.data.table(finish_rates)) finish_rates <- as.data.table(finish_rates)
  setDT(finish_rates)
  
  # Create car badges
  finish_rates_display <- copy(finish_rates)
  setalloccol(finish_rates_display)
  
  # Use set() to add columns
  set(finish_rates_display, j = "CarBadge", 
      value = mapply(create_car_badge, finish_rates_display$Car, finish_rates_display$Team))
  set(finish_rates_display, j = "Display", 
      value = paste0(finish_rates_display$CarBadge, " ", finish_rates_display$Name))
  
  # Select and rename columns for display
  display_data <- finish_rates_display[, .(
    Driver = Display,
    Start = Starting,
    `Sim W%` = Sim_W,
    `Input W%` = Input_W,
    `Diff W` = Diff_W,
    `Sim T5%` = Sim_T5,
    `Input T5%` = Input_T5,
    `Diff T5` = Diff_T5,
    `Sim T10%` = Sim_T10,
    `Input T10%` = Input_T10,
    `Diff T10` = Diff_T10,
    `Sim T20%` = Sim_T20,
    `Input T20%` = Input_T20,
    `Diff T20` = Diff_T20
  )]
  
  # Create datatable
  dt <- datatable(
    display_data,
    escape = FALSE,  # Allow HTML in Driver column
    rownames = FALSE,
    options = list(
      pageLength = 50,
      scrollX = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = 1:13)
      )
    )
  ) %>%
    formatPercentage(c(2:4, 5:7, 8:10, 11:13), 1) %>%
    formatStyle(
      c('Diff W', 'Diff T5', 'Diff T10', 'Diff T20'),
      backgroundColor = styleInterval(
        cuts = c(-0.05, -0.02, 0.02, 0.05),
        values = c('#8B0000', '#CD5C5C', '#90EE90', '#FFFFE0', '#FFD700')
      )
    )
  
  return(dt)
}


#' Create Finish Position Violin Plot
#' @param sim_results data.table with simulation results
#' @param selected_drivers Character vector of driver names to display (NULL = all)
#' @param start_range Numeric vector of length 2 for starting position filter
#' @return plotly object
create_finish_violin_plot <- function(sim_results, selected_drivers = NULL, 
                                      start_range = c(1, 40)) {
  
  require(plotly)
  
  # Filter by starting position range
  plot_data <- sim_results[Starting >= start_range[1] & Starting <= start_range[2]]
  
  # Filter by selected drivers if specified
  if (!is.null(selected_drivers) && length(selected_drivers) > 0) {
    plot_data <- plot_data[Name %in% selected_drivers]
  }
  
  if (nrow(plot_data) == 0) {
    return(plotly_empty() %>% layout(title = "No data to display"))
  }
  
  # Get unique drivers ordered by starting position
  driver_order <- plot_data[, .(Starting = unique(Starting)), by = Name]
  setorder(driver_order, Starting)
  ordered_drivers <- driver_order$Name
  
  # Convert to regular data.frame for plotly
  plot_data <- as.data.frame(plot_data)
  plot_data$Name <- factor(plot_data$Name, levels = rev(ordered_drivers))
  
  # Get team colors for color palette
  team_colors_list <- get_team_colors()
  unique_teams <- unique(plot_data$Team)
  color_palette <- sapply(unique_teams, function(t) {
    colors <- team_colors_list[[t]]
    if (is.null(colors)) colors <- team_colors_list[["default"]]
    colors$primary
  })
  names(color_palette) <- unique_teams
  
  # Create violin plot
  p <- plot_ly(
    data = plot_data,
    x = ~FinishPosition,
    y = ~Name,
    type = 'violin',
    orientation = 'h',
    color = ~Team,
    colors = color_palette,
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Position: %{x}<br>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = "Finish Position Distribution",
      xaxis = list(
        title = "Finish Position",
        gridcolor = '#404040',
        gridwidth = 1,
        dtick = 5,  # Major gridlines every 5 positions
        showgrid = TRUE,
        range = c(0, 41)
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = rev(ordered_drivers)
      ),
      paper_bgcolor = '#121212',
      plot_bgcolor = '#1e1e1e',
      font = list(color = '#FFFFFF', size = 12),
      showlegend = TRUE,
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 1
      ),
      height = 600,
      margin = list(l = 150, r = 100, t = 50, b = 50)
    )
  
  return(p)
}


#' Create Dominator Violin Plot by Driver
#' @param sim_results data.table with simulation results
#' @param platform "DK" or "FD"
#' @param selected_drivers Character vector of driver names (NULL = all)
#' @return plotly object
create_dominator_violin_by_driver <- function(sim_results, platform = "DK", 
                                              selected_drivers = NULL) {
  
  require(plotly)
  
  dom_col <- if (platform == "DK") "DKDominatorPoints" else "FDDominatorPoints"
  
  # Filter by selected drivers if specified
  plot_data <- if (!is.null(selected_drivers) && length(selected_drivers) > 0) {
    sim_results[Name %in% selected_drivers]
  } else {
    copy(sim_results)
  }
  
  if (nrow(plot_data) == 0) {
    return(plotly_empty() %>% layout(title = "No data to display"))
  }
  
  # Calculate median for ordering
  driver_medians <- plot_data[, .(Median = median(get(dom_col))), by = Name]
  setorder(driver_medians, -Median)
  
  # Convert to data.frame and set factor levels
  plot_data <- as.data.frame(plot_data)
  plot_data$Name <- factor(plot_data$Name, levels = rev(driver_medians$Name))
  plot_data$DomPoints <- plot_data[[dom_col]]
  
  # Get team colors
  team_colors_list <- get_team_colors()
  unique_teams <- unique(plot_data$Team)
  color_palette <- sapply(unique_teams, function(t) {
    colors <- team_colors_list[[t]]
    if (is.null(colors)) colors <- team_colors_list[["default"]]
    colors$primary
  })
  names(color_palette) <- unique_teams
  
  # Create violin plot
  p <- plot_ly(
    data = plot_data,
    x = ~DomPoints,
    y = ~Name,
    type = 'violin',
    orientation = 'h',
    color = ~Team,
    colors = color_palette,
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Dominator Points: %{x}<br>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = paste(platform, "Dominator Points Distribution by Driver"),
      xaxis = list(
        title = "Dominator Points",
        gridcolor = '#404040',
        gridwidth = 1,
        showgrid = TRUE
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = rev(driver_medians$Name)
      ),
      paper_bgcolor = '#121212',
      plot_bgcolor = '#1e1e1e',
      font = list(color = '#FFFFFF', size = 12),
      showlegend = TRUE,
      legend = list(orientation = "v", x = 1.02, y = 1),
      height = 500,
      margin = list(l = 150, r = 100, t = 50, b = 50)
    )
  
  return(p)
}


#' Create Dominator Violin Plot by Position Group
#' @param sim_results data.table with simulation results
#' @param platform "DK" or "FD"
#' @param group_by "start" or "finish" - group by starting or finish position
#' @return plotly object
create_dominator_violin_by_position <- function(sim_results, platform = "DK", 
                                                group_by = "start") {
  
  require(plotly)
  
  # Ensure proper data.table
  if (!is.data.table(sim_results)) sim_results <- as.data.table(sim_results)
  setDT(sim_results)
  
  dom_col <- if (platform == "DK") "DKDominatorPoints" else "FDDominatorPoints"
  
  # Create position groups
  plot_data <- copy(sim_results)
  setalloccol(plot_data)
  
  if (group_by == "start") {
    set(plot_data, j = "PosGroup", 
        value = cut(plot_data$Starting, 
                    breaks = c(0, 5, 10, 15, 20, 25, 100),
                    labels = c("P1-5", "P6-10", "P11-15", "P16-20", "P21-25", "P26+"),
                    include.lowest = TRUE))
    title_text <- paste(platform, "Dominator Points by Starting Position")
  } else {
    set(plot_data, j = "PosGroup",
        value = cut(plot_data$FinishPosition,
                    breaks = c(0, 1, 3, 5, 10, 100),
                    labels = c("Winner", "Top3", "Top5", "Top10", "11+"),
                    include.lowest = TRUE))
    title_text <- paste(platform, "Dominator Points by Finish Position")
  }
  
  # Convert to data.frame
  plot_data <- as.data.frame(plot_data)
  plot_data$DomPoints <- plot_data[[dom_col]]
  
  # Create violin plot
  p <- plot_ly(
    data = plot_data,
    x = ~DomPoints,
    y = ~PosGroup,
    type = 'violin',
    orientation = 'h',
    fillcolor = '#FFE500',
    line = list(color = '#FFE500'),
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Dominator Points: %{x}<br>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = title_text,
      xaxis = list(
        title = "Dominator Points",
        gridcolor = '#404040',
        gridwidth = 1,
        showgrid = TRUE
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = if (group_by == "start") {
          c("P26+", "P21-25", "P16-20", "P11-15", "P6-10", "P1-5")
        } else {
          c("11+", "Top10", "Top5", "Top3", "Winner")
        }
      ),
      paper_bgcolor = '#121212',
      plot_bgcolor = '#1e1e1e',
      font = list(color = '#FFFFFF', size = 12),
      showlegend = FALSE,
      height = 400,
      margin = list(l = 100, r = 50, t = 50, b = 50)
    )
  
  return(p)
}


#' Assign dominator points from profiles (CACHED VERSION - UNIQUE ASSIGNMENT)
#' Uses pre-cached race profiles split by RaceID for faster lookup
#' Each profile is assigned to exactly one driver (greedy best-match algorithm)
assign_dominator_points_from_profiles_cached <- function(race_result, race_weights, 
                                                         race_profiles_by_race, platform) {
  
  # Ensure data.table has proper allocation
  setalloccol(race_result)
  
  # Select random race based on weights
  race_id <- sample(
    race_weights$RaceID,
    size = 1,
    prob = race_weights$Weight
  )
  
  # Get profiles for selected race from cache (FAST!)
  profiles <- race_profiles_by_race[[as.character(race_id)]]
  
  if (is.null(profiles) || nrow(profiles) == 0) {
    # No profiles for this race - assign 0 points using set()
    col_name <- paste0(platform, "DominatorPoints")
    set(race_result, j = col_name, value = 0)
    return(race_result)
  }
  
  # Get dominator points column
  dom_col <- paste0(platform, "DomPoints")
  max_col <- paste0(platform, "Max")
  
  # Check if dominator column exists (FD might not be present)
  if (!dom_col %in% names(profiles)) {
    # No dominator points for this platform - assign 0 points
    col_name <- paste0(platform, "DominatorPoints")
    set(race_result, j = col_name, value = 0)
    return(race_result)
  }
  
  # Initialize dominator points to 0 for all drivers
  n_drivers <- nrow(race_result)
  dom_points <- rep(0, n_drivers)
  
  # Calculate distances (vectorized)
  driver_finishes <- race_result$FinishPosition
  driver_starts <- race_result$Starting
  
  profile_finishes <- profiles$FinPos
  profile_starts <- profiles$StartPos
  
  # Distance matrix: rows = drivers, cols = profiles
  finish_diff_matrix <- outer(driver_finishes, profile_finishes, function(x, y) abs(x - y))
  start_diff_matrix <- outer(driver_starts, profile_starts, function(x, y) abs(x - y))
  distance_matrix <- sqrt(finish_diff_matrix^2 + start_diff_matrix^2)
  
  # GREEDY ASSIGNMENT: Each profile assigned to exactly one driver
  # Track which drivers and profiles have been assigned
  available_drivers <- 1:n_drivers
  available_profiles <- 1:nrow(profiles)
  
  # Assign profiles one at a time to their best available match
  while (length(available_profiles) > 0 && length(available_drivers) > 0) {
    
    # Get submatrix of only available drivers/profiles
    sub_matrix <- distance_matrix[available_drivers, available_profiles, drop = FALSE]
    
    # Find the overall best match (minimum distance)
    min_idx <- which.min(sub_matrix)
    
    # Convert linear index to row/col
    row_idx <- ((min_idx - 1) %% nrow(sub_matrix)) + 1
    col_idx <- ((min_idx - 1) %/% nrow(sub_matrix)) + 1
    
    # Map back to original indices
    driver_idx <- available_drivers[row_idx]
    profile_idx <- available_profiles[col_idx]
    
    # Assign dominator points from this profile to this driver
    profile_dom_points <- profiles[[dom_col]][profile_idx]
    driver_max <- race_result[[max_col]][driver_idx]
    
    # Apply driver-specific ceiling
    dom_points[driver_idx] <- min(profile_dom_points, driver_max)
    
    # Remove assigned driver and profile from available pools
    available_drivers <- available_drivers[-row_idx]
    available_profiles <- available_profiles[-col_idx]
  }
  
  # Use set() instead of [[ to avoid allocation issues
  col_name <- paste0(platform, "DominatorPoints")
  set(race_result, j = col_name, value = dom_points)
  
  return(race_result)
}


#' Get Full NASCAR Simulation Data for Visualizations
#' 
#' This function returns the complete simulation results including all columns
#' needed for visualizations (FinishPosition, DominatorPoints, etc.)
#' It's essentially a wrapper that returns the raw combined_results instead
#' of just the summarized sim_results.
#' 
#' @param input_data List of data.frames from Excel sheets
#' @param n_sims Number of simulations to run
#' @param config Sport configuration
#' @return data.table with complete simulation results (all columns, all sims)
get_full_nascar_simulation_data <- function(input_data, n_sims, config) {
  
  # Extract data from input
  driver_data <- as.data.table(input_data$Driver)
  race_weights <- as.data.table(input_data$Race_Weights)
  race_profiles <- as.data.table(input_data$Race_Profiles)
  
  # Standardize column names
  if ("DKOP" %in% names(driver_data)) setnames(driver_data, "DKOP", "DKOwn")
  if ("FDOP" %in% names(driver_data)) setnames(driver_data, "FDOP", "FDOwn")
  if ("team" %in% names(driver_data)) setnames(driver_data, "team", "Team")
  if ("car" %in% names(driver_data)) setnames(driver_data, "car", "Car")
  driver_data <- copy(driver_data)
  
  # Pre-compute distributions
  driver_distributions <- precompute_driver_distributions(driver_data)
  all_finish_positions <- simulate_finish_positions_vectorized(driver_distributions, n_sims)
  
  # Scoring system
  scoring_systems <- create_scoring_system()
  
  # Pre-cache race profiles
  race_profiles_by_race <- split(race_profiles, race_profiles$RaceID)
  
  # Run simulations - same as main function but return ALL columns
  all_results <- list()
  
  for (sim_id in 1:n_sims) {
    # Create race result for this simulation
    race_result <- data.table(
      SimID = sim_id,
      Name = driver_data$Name,
      Starting = driver_data$Starting,
      FinishPosition = all_finish_positions[, sim_id],
      DKSalary = driver_data$DKSalary,
      DKID = driver_data$DKID,
      DKOwn = driver_data$DKOwn,
      DKMax = driver_data$DKMax,
      FDSalary = driver_data$FDSalary,
      FDID = driver_data$FDID,
      FDName = driver_data$FDName,
      FDOwn = driver_data$FDOwn,
      FDMax = driver_data$FDMax,
      Team = driver_data$Team,
      Car = driver_data$Car
    )
    
    setalloccol(race_result)
    
    # Assign dominator points
    race_result <- assign_dominator_points_from_profiles_cached(
      race_result, race_weights, race_profiles_by_race, "DK"
    )
    race_result <- assign_dominator_points_from_profiles_cached(
      race_result, race_weights, race_profiles_by_race, "FD"
    )
    
    # Calculate fantasy points
    race_result <- calculate_fantasy_points(race_result, scoring_systems)
    
    all_results[[sim_id]] <- race_result
  }
  
  # Return FULL combined results
  combined_results <- rbindlist(all_results)
  
  return(combined_results)
}