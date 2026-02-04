


# === LIBRARIES ===
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(readxl)
library(DT)
library(plotly)
library(memoise)
library(shinycssloaders)
library(shinyjs)

# === SOURCE CORE FUNCTIONS ===
source("OptimalLineups_Core.R")
source("LineupBuilder_Core.R")
source("nascar_config.R")

# === GLOBAL CONSTANTS ===
DK_ROSTER_SIZE <- 6
FD_ROSTER_SIZE <- 5
DK_SALARY_CAP <- 50000
FD_SALARY_CAP <- 50000





read_input_file <- function(file_path) {
  tryCatch({
    # Read sheets needed by both platforms
    sheets <- list(Driver = read_excel(file_path, sheet = "Driver"))
    
    sheets$Race_Weights <- tryCatch(
      read_excel(file_path, sheet = "Race_Weights"),
      error = function(e)
        NULL
    )
    
    sheets$Race_Profiles <- tryCatch(
      read_excel(file_path, sheet = "Race_Profiles"),
      error = function(e)
        NULL
    )
    
    
    # Keep FD Laps (unchanged)
    sheets$FDLaps <- tryCatch(
      read_excel(file_path, sheet = "FDLaps"),
      error = function(e)
        NULL
    )
    
    # Identify available platforms
    has_race_data <- !is.null(sheets$Race_Weights) &&
      !is.null(sheets$Race_Profiles)
    has_dk <- "DKSalary" %in% colnames(sheets$Driver)
    has_fd <- "FDSalary" %in% colnames(sheets$Driver)
    
    # Create platform info
    platform_info <- list(has_draftkings = has_dk, has_fanduel = has_fd)
    
    
    list(sheets = sheets, platform_info = platform_info)
  }, error = function(e) {
    stop(paste("Error reading Excel file:", e$message))
  })
}

# Process input data efficiently with data.table
process_input_data <- function(input_data) {
  # Extract data components
  driver_data <- input_data$sheets$Driver
  race_data <- input_data$sheets$Race
  fd_laps_data <- input_data$sheets$FDLaps
  race_weights_data <- input_data$sheets$Race_Weights
  race_profiles_data <- input_data$sheets$Race_Profiles
  
  # Process driver data
  processed_drivers <- as.data.table(driver_data)
  
  # Convert relevant numeric columns efficiently
  numeric_cols <- c(
    "W",
    "T3",
    "T5",
    "T10",
    "T15",
    "T20",
    "T25",
    "T30",
    "DKSalary",
    "FDSalary",
    "DKOP",
    "FDOP",
    "Starting",
    "DKMax",
    "FDMax"
  )
  
  for (col in numeric_cols) {
    if (col %in% names(processed_drivers)) {
      processed_drivers[, (col) := as.numeric(get(col))]
    }
  }
  
  # Process Race Weights data
  processed_race_weights <- if (!is.null(race_weights_data)) {
    weights_dt <- as.data.table(race_weights_data)
    # Ensure required columns exist and are numeric
    if ("RaceID" %in% names(weights_dt) &&
        "Weight" %in% names(weights_dt)) {
      weights_dt[, Weight := as.numeric(Weight)]
      weights_dt[, RaceID := as.character(RaceID)]
      weights_dt <- weights_dt[!is.na(Weight) & Weight > 0]
      weights_dt
    } else {
      data.table()
    }
  } else {
    data.table()
  }
  
  # Process Race Profiles data
  processed_race_profiles <- if (!is.null(race_profiles_data)) {
    profiles_dt <- as.data.table(race_profiles_data)
    # Ensure required columns exist and are numeric
    required_cols <- c("RaceID",
                       "StartPos",
                       "FinPos",
                       "DKDomPoints",
                       "FDDomPoints")
    if (all(required_cols %in% names(profiles_dt))) {
      profiles_dt[, RaceID := as.character(RaceID)]
      profiles_dt[, StartPos := as.numeric(StartPos)]
      profiles_dt[, FinPos := as.numeric(FinPos)]
      profiles_dt[, DKDomPoints := as.numeric(DKDomPoints)]
      profiles_dt[, FDDomPoints := as.numeric(FDDomPoints)]
      # Remove rows with missing required data
      profiles_dt <- profiles_dt[!is.na(StartPos) &
                                   !is.na(FinPos) &
                                   !is.na(DKDomPoints) & !is.na(FDDomPoints)]
      profiles_dt
    } else {
      data.table()
    }
  } else {
    data.table()
  }
  
  
  # Process FD laps data if available
  processed_fd_laps <- if (!is.null(fd_laps_data)) {
    fd_laps_dt <- as.data.table(fd_laps_data)
    
    # Only handle Pt column (no more high/low)
    fd_laps_dt <- fd_laps_dt[!is.na(Pt)]
    
    # Convert columns to numeric
    for (col in c("ps", "Pt")) {
      if (col %in% names(fd_laps_dt)) {
        fd_laps_dt[, (col) := as.numeric(get(col))]
      }
    }
    
    # Use position (ps) as finish position
    fd_laps_dt[, FinishLow := ps]
    fd_laps_dt[, FinishHigh := ps]
    
    setorder(fd_laps_dt, ps)
    fd_laps_dt
  } else
    data.table()
  
  
  # Return processed data
  list(
    drivers = processed_drivers,
    race_weights = processed_race_weights,
    race_profiles = processed_race_profiles,
    fd_laps = processed_fd_laps
  )
}


configure_memory_settings <- function() {
  # Configure data.table for better memory usage
  data.table::setDTthreads(0)  # Use all available cores
  options(datatable.optimize = 2)
  
  # More aggressive garbage collection settings
  gcinfo(FALSE)  # Disable verbose GC messages
  
}

# Enhanced cleanup with performance monitoring
cleanup_memory <- function(verbose = TRUE) {
  if (verbose) {
    start_mem <- sum(gc()[, 2])
    cat("Memory cleanup starting - Used:",
        round(start_mem, 1),
        "MB\n")
  }
  
  # Force garbage collection multiple times
  for (i in 1:3) {
    gc(verbose = FALSE, full = TRUE)
    Sys.sleep(0.05)  # Brief pause between collections
  }
  
  if (verbose) {
    end_mem <- sum(gc()[, 2])
    freed <- start_mem - end_mem
    cat("Memory cleanup complete - Used:", round(end_mem, 1), "MB")
    if (freed > 0) {
      cat(" (freed", round(freed, 1), "MB)")
    }
    cat("\n")
  }
}




# Efficient memory cleanup
cleanup_memory <- function(verbose = FALSE) {
  if (verbose)
    cat("Running memory cleanup...\n")
  
  # Force garbage collection
  for (i in 1:3) {
    gc(verbose = FALSE, full = TRUE)
    Sys.sleep(0.1)  # Brief pause between collections
  }
  
  if (verbose) {
    mem_info <- gc()
    cat("Memory after cleanup - Used:", sum(mem_info[, 2]), "MB\n")
  }
}

optimize_simulation_storage <- function(sim_results) {
  # Don't remove SimID immediately - let analysis functions handle it
  # Only remove truly unnecessary columns
  columns_to_remove <- c()  # Keep SimID for now
  
  for (col in columns_to_remove) {
    if (col %in% names(sim_results)) {
      sim_results[[col]] <- NULL
    }
  }
  
  return(sim_results)
}


# Pre-calculate expensive lookup data
create_scoring_lookups <- function() {
  list(
    dk_finish_points = c(
      45,
      42,
      41,
      40,
      39,
      38,
      37,
      36,
      35,
      34,
      32,
      31,
      30,
      29,
      28,
      27,
      26,
      25,
      24,
      23,
      21,
      20,
      19,
      18,
      17,
      16,
      15,
      14,
      13,
      12,
      10,
      9,
      8,
      7,
      6,
      5,
      4,
      3,
      2,
      1,
      0
    ),
    fd_finish_points = c(
      43,
      40,
      38,
      37,
      36,
      35,
      34,
      33,
      32,
      31,
      30,
      29,
      28,
      27,
      26,
      25,
      24,
      23,
      22,
      21,
      20,
      19,
      18,
      17,
      16,
      15,
      14,
      13,
      12,
      11,
      10,
      9,
      8,
      7,
      6,
      5,
      4,
      3,
      2,
      1,
      0
    )
  )
}

# Initialize memory settings when app starts
configure_memory_settings()

create_driver_lookups <- function(drivers_dt) {
  # Pre-compute salary and name lookups to avoid repeated searches
  dk_lookup <- NULL
  fd_lookup <- NULL
  
  if ("DKSalary" %in% names(drivers_dt)) {
    dk_lookup <- drivers_dt[!duplicated(DKName), .(DKName, Name, DKSalary, DKOP, Starting)]
    setkey(dk_lookup, DKName)
  }
  
  if ("FDSalary" %in% names(drivers_dt)) {
    fd_lookup <- drivers_dt[!duplicated(FDName), .(FDName, Name, FDSalary, FDOP, Starting)]
    setkey(fd_lookup, FDName)
  }
  
  list(dk = dk_lookup, fd = fd_lookup)
}

# ============================================================================
# SIMULATION ENGINE - CLEAN VERSION
# ============================================================================

precompute_driver_distributions <- function(drivers_dt) {
  n_drivers <- nrow(drivers_dt)
  
  prob_cols <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
  prob_matrix <- as.matrix(drivers_dt[, ..prob_cols])
  prob_matrix[is.na(prob_matrix)] <- 0
  
  marginal_probs <- array(0, dim = c(n_drivers, 9))
  
  for (i in 1:n_drivers) {
    cum_probs <- c(prob_matrix[i, ], 1.0)
    marg_probs <- diff(c(0, cum_probs))
    marg_probs[marg_probs < 0] <- 0
    
    sum_probs <- sum(marg_probs)
    if (sum_probs > 0) {
      marg_probs <- marg_probs / sum_probs
    } else {
      marg_probs <- rep(1 / 9, 9)
    }
    
    marginal_probs[i, ] <- marg_probs
  }
  
  position_ranges <- list(
    c(1),
    c(2, 3),
    c(4, 5),
    c(6, 7, 8, 9, 10),
    c(11, 12, 13, 14, 15),
    c(16, 17, 18, 19, 20),
    c(21, 22, 23, 24, 25),
    c(26, 27, 28, 29, 30),
    c(31:n_drivers)
  )
  
  list(
    marginal_probs = marginal_probs,
    position_ranges = position_ranges,
    n_drivers = n_drivers
  )
}

simulate_finish_positions_vectorized <- function(driver_distributions, n_sims) {
  n_drivers <- driver_distributions$n_drivers
  marginal_probs <- driver_distributions$marginal_probs
  position_ranges <- driver_distributions$position_ranges
  
  all_positions <- matrix(0, nrow = n_drivers, ncol = n_sims)
  
  random_matrix <- matrix(runif(n_drivers * n_sims), nrow = n_drivers, ncol = n_sims)
  noise_matrix <- matrix(runif(n_drivers * n_sims, 0, 0.1),
                         nrow = n_drivers,
                         ncol = n_sims)
  
  for (sim in 1:n_sims) {
    performance_scores <- numeric(n_drivers)
    
    for (i in 1:n_drivers) {
      random_val <- random_matrix[i, sim]
      cumsum_probs <- cumsum(marginal_probs[i, ])
      pos_range_idx <- which(random_val <= cumsum_probs)[1]
      
      if (is.na(pos_range_idx))
        pos_range_idx <- 9
      
      pos_range <- position_ranges[[pos_range_idx]]
      
      if (length(pos_range) > 1) {
        range_random <- (random_val * 1000) %% 1
        sampled_pos <- pos_range[ceiling(range_random * length(pos_range))]
      } else {
        sampled_pos <- pos_range[1]
      }
      
      performance_scores[i] <- sampled_pos + noise_matrix[i, sim]
    }
    
    all_positions[, sim] <- rank(performance_scores, ties.method = "random")
  }
  
  return(all_positions)
}

select_weighted_race <- function(race_weights) {
  if (nrow(race_weights) == 0)
    return(NULL)
  
  total_weight <- sum(race_weights$Weight, na.rm = TRUE)
  if (total_weight == 0)
    return(NULL)
  
  random_val <- runif(1) * total_weight
  cumsum_weights <- cumsum(race_weights$Weight)
  selected_idx <- which(random_val <= cumsum_weights)[1]
  
  if (is.na(selected_idx))
    selected_idx <- 1
  
  return(race_weights$RaceID[selected_idx])
}

# Quick Win #2: Fully vectorized dominator assignment
assign_dominator_points_both_platforms_fast <- function(race_results,
                                                        race_weights,
                                                        profiles_by_race) {
  setDT(race_results)
  
  race_results[, DKDominatorPoints := 0]
  race_results[, FDDominatorPoints := 0]
  
  if (length(profiles_by_race) == 0)
    return(race_results)
  
  selected_race <- select_weighted_race(race_weights)
  if (is.null(selected_race))
    return(race_results)
  
  race_profiles_subset <- profiles_by_race[[as.character(selected_race)]]
  if (is.null(race_profiles_subset) ||
      nrow(race_profiles_subset) == 0)
    return(race_results)
  
  if (!"DKMax" %in% names(race_results))
    race_results[, DKMax := 0]
  if (!"FDMax" %in% names(race_results))
    race_results[, FDMax := 0]
  
  n_drivers <- nrow(race_results)
  n_profiles <- nrow(race_profiles_subset)
  
  driver_starts <- race_results$Starting
  driver_finishes <- race_results$FinishPosition
  profile_starts <- race_profiles_subset$StartPos
  profile_finishes <- race_profiles_subset$FinPos
  
  finish_diff_matrix <- outer(driver_finishes, profile_finishes, function(x, y)
    abs(x - y))
  start_diff_matrix <- outer(driver_starts, profile_starts, function(x, y)
    abs(x - y))
  distance_matrix <- finish_diff_matrix^2 + start_diff_matrix^2
  
  # DK - Vectorized approach
  dk_points <- race_profiles_subset$DKDomPoints
  dk_order <- order(-dk_points, na.last = TRUE)
  
  assigned_dk <- rep(FALSE, n_drivers)
  driver_max_dk <- race_results$DKMax
  
  for (i in dk_order) {
    points <- dk_points[i]
    if (is.na(points) || points <= 0)
      next
    
    eligible <- !assigned_dk &
      !is.na(driver_max_dk) & driver_max_dk >= points
    if (!any(eligible))
      next
    
    eligible_distances <- distance_matrix[, i]
    eligible_distances[!eligible] <- Inf
    
    best_idx <- which.min(eligible_distances)
    if (is.finite(eligible_distances[best_idx])) {
      race_results[best_idx, DKDominatorPoints := points]
      assigned_dk[best_idx] <- TRUE
    }
  }
  
  # FD - Vectorized approach
  fd_points <- race_profiles_subset$FDDomPoints
  fd_order <- order(-fd_points, na.last = TRUE)
  
  assigned_fd <- rep(FALSE, n_drivers)
  driver_max_fd <- race_results$FDMax
  
  for (i in fd_order) {
    points <- fd_points[i]
    if (is.na(points) || points <= 0)
      next
    
    eligible <- !assigned_fd &
      !is.na(driver_max_fd) & driver_max_fd >= points
    if (!any(eligible))
      next
    
    eligible_distances <- distance_matrix[, i]
    eligible_distances[!eligible] <- Inf
    
    best_idx <- which.min(eligible_distances)
    if (is.finite(eligible_distances[best_idx])) {
      race_results[best_idx, FDDominatorPoints := points]
      assigned_fd[best_idx] <- TRUE
    }
  }
  
  return(race_results)
}


# Quick Win #3: Optimized FD lap lookup with binary search
assign_fd_lap_points_fast <- function(race_results, fd_laps_data) {
  setDT(race_results)
  setDT(fd_laps_data)
  
  result <- copy(race_results)
  result[, FDLapPoints := 0]
  
  if (!"ps" %in% names(fd_laps_data))
    return(result)
  
  point_col <- if ("Pt" %in% names(fd_laps_data))
    "Pt"
  else
    NULL
  if (is.null(point_col))
    return(result)
  
  fd_laps_data[, ps := as.numeric(ps)]
  fd_laps_data[, (point_col) := as.numeric(get(point_col))]
  fd_laps_data <- fd_laps_data[!is.na(ps) &
                                 !is.na(get(point_col)) & ps > 0]
  
  if (nrow(fd_laps_data) == 0)
    return(result)
  
  setorder(fd_laps_data, ps)
  setkey(fd_laps_data, ps)
  
  positions <- fd_laps_data$ps
  points <- fd_laps_data[[point_col]]
  
  for (i in 1:nrow(result)) {
    finish_pos <- result$FinishPosition[i]
    
    if (is.na(finish_pos) || finish_pos <= 0) {
      result$FDLapPoints[i] <- 0
      next
    }
    
    # Binary search for exact match or closest lower
    idx <- findInterval(finish_pos, positions, rightmost.closed = TRUE)
    
    if (idx > 0) {
      result$FDLapPoints[i] <- points[idx]
    } else {
      result$FDLapPoints[i] <- 0
    }
  }
  
  result[, FDLapPoints := round(FDLapPoints, 1)]
  
  return(result)
}

create_scoring_system <- function() {
  list(
    dk = list(
      finish = c(45, 42, 39, 37, 35, 33, 32, 31, 30, 29, rep(28:1, each = 1)[1:30]),
      laps_led = 0.25,
      fastest_laps = 0.5
    ),
    fd = list(finish = c(
      43, 39, 36, 34, 33, 31, 30, 29, 28, 27, rep(26:1, each = 1)[1:30]
    ))
  )
}

calculate_fantasy_points <- function(race_result, scoring_systems) {
  setDT(race_result)
  
  n_drivers <- nrow(race_result)
  
  dk_finish_points <- numeric(n_drivers)
  for (i in 1:n_drivers) {
    pos <- race_result$FinishPosition[i]
    if (!is.na(pos) &&
        pos > 0 && pos <= length(scoring_systems$dk$finish)) {
      dk_finish_points[i] <- scoring_systems$dk$finish[pos]
    }
  }
  
  race_result[, DKFantasyPoints := dk_finish_points + DKDominatorPoints]
  
  fd_finish_points <- numeric(n_drivers)
  for (i in 1:n_drivers) {
    pos <- race_result$FinishPosition[i]
    if (!is.na(pos) &&
        pos > 0 && pos <= length(scoring_systems$fd$finish)) {
      fd_finish_points[i] <- scoring_systems$fd$finish[pos]
    }
  }
  
  if ("FDLapPoints" %in% names(race_result)) {
    race_result[, FDFantasyPoints := fd_finish_points + FDLapPoints + FDDominatorPoints]
  } else {
    race_result[, FDFantasyPoints := fd_finish_points + FDDominatorPoints]
  }
  
  return(race_result)
}

run_efficient_simulation <- function(input_data,
                                     n_sims = 10000,
                                     race_weights = NULL,
                                     race_profiles = NULL,
                                     fd_laps_data = NULL,
                                     use_parallel = TRUE) {
  if (n_sims < 1)
    stop("n_sims must be at least 1")
  
  cat(sprintf(
    "[STEP 1/3] Generating %s finish positions...\n",
    format(n_sims, big.mark = ",")
  ))
  start_time <- Sys.time()
  
  driver_distributions <- precompute_driver_distributions(input_data)
  all_finish_positions <- simulate_finish_positions_vectorized(driver_distributions, n_sims)
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("  ✓ %.1fs\n", elapsed))
  
  cat("[STEP 2/3] Calculating fantasy points...\n")
  step2_start <- Sys.time()
  
  scoring_systems <- create_scoring_system()
  
  # Pre-cache race profiles by RaceID
  profiles_by_race <- NULL
  if (!is.null(race_profiles) && !is.null(race_weights)) {
    setDT(race_profiles)
    profiles_by_race <- split(race_profiles, race_profiles$RaceID)
  }
  
  if (use_parallel && n_sims >= 1000) {
    library(parallel)
    
    total_cores <- detectCores()
    n_cores <- max(1, min(total_cores - 1, 7))
    
    batch_size <- ceiling(n_sims / n_cores)
    
    cat(sprintf(
      "  Using %d cores | %s sims per core | Processing...\n",
      n_cores,
      format(batch_size, big.mark = ",")
    ))
    flush.console()
    
    cl <- makeCluster(n_cores, type = "PSOCK")
    on.exit(stopCluster(cl), add = TRUE)
    
    clusterEvalQ(cl, {
      library(data.table)
    })
    
    clusterExport(
      cl,
      c(
        "assign_dominator_points_both_platforms_fast",
        "select_weighted_race",
        "assign_fd_lap_points_fast",
        "calculate_fantasy_points",
        "input_data",
        "race_weights",
        "profiles_by_race",
        "fd_laps_data",
        "scoring_systems",
        "all_finish_positions"
      ),
      envir = environment()
    )
    
    sim_batches <- split(1:n_sims, ceiling(seq_along(1:n_sims) / batch_size))
    
    all_results <- parLapply(cl, sim_batches, function(sim_indices) {
      batch_results <- list()
      
      for (sim_id in sim_indices) {
        race_result <- data.table(
          SimID = sim_id,
          Name = input_data$Name,
          Starting = input_data$Starting,
          FinishPosition = all_finish_positions[, sim_id],
          DKSalary = input_data$DKSalary,
          DKOP = input_data$DKOP,
          DKName = input_data$DKName,
          DKMax = input_data$DKMax,
          FDSalary = input_data$FDSalary,
          FDOP = input_data$FDOP,
          FDName = input_data$FDName,
          FDMax = input_data$FDMax
        )
        
        if (!is.null(race_weights) && !is.null(profiles_by_race)) {
          race_result <- assign_dominator_points_both_platforms_fast(race_result, race_weights, profiles_by_race)
        } else {
          race_result[, DKDominatorPoints := 0]
          race_result[, FDDominatorPoints := 0]
        }
        
        if (!is.null(fd_laps_data) && nrow(fd_laps_data) > 0) {
          race_result <- assign_fd_lap_points_fast(race_result, fd_laps_data)
        } else {
          race_result[, FDLapPoints := 0]
        }
        
        race_result <- calculate_fantasy_points(race_result, scoring_systems)
        
        batch_results[[length(batch_results) + 1]] <- race_result
      }
      
      rbindlist(batch_results)
    })
    
    all_results <- rbindlist(all_results)
    
  } else {
    all_results <- list()
    update_interval <- max(1, floor(n_sims / 10))
    
    for (sim_id in 1:n_sims) {
      race_result <- data.table(
        SimID = sim_id,
        Name = input_data$Name,
        Starting = input_data$Starting,
        FinishPosition = all_finish_positions[, sim_id],
        DKSalary = input_data$DKSalary,
        DKOP = input_data$DKOP,
        DKName = input_data$DKName,
        DKMax = input_data$DKMax,
        FDSalary = input_data$FDSalary,
        FDOP = input_data$FDOP,
        FDName = input_data$FDName,
        FDMax = input_data$FDMax
      )
      
      if (!is.null(race_weights) && !is.null(profiles_by_race)) {
        race_result <- assign_dominator_points_both_platforms_fast(race_result, race_weights, profiles_by_race)
      } else {
        race_result[, DKDominatorPoints := 0]
        race_result[, FDDominatorPoints := 0]
      }
      
      if (!is.null(fd_laps_data) && nrow(fd_laps_data) > 0) {
        race_result <- assign_fd_lap_points_fast(race_result, fd_laps_data)
      } else {
        race_result[, FDLapPoints := 0]
      }
      
      race_result <- calculate_fantasy_points(race_result, scoring_systems)
      
      all_results[[sim_id]] <- race_result
      
      if (sim_id %% update_interval == 0 || sim_id == n_sims) {
        elapsed <- as.numeric(difftime(Sys.time(), step2_start, units = "secs"))
        pct <- (sim_id / n_sims) * 100
        rate <- sim_id / elapsed
        eta <- (n_sims - sim_id) / rate
        
        cat(sprintf("\r  Step 2: %.0f%% | %.1f sims/s | ETA: %.0fs", pct, rate, eta))
        flush.console()
      }
    }
    cat("\n")
    
    all_results <- rbindlist(all_results)
  }
  
  elapsed <- as.numeric(difftime(Sys.time(), step2_start, units = "secs"))
  cat(sprintf("  ✓ %.1fs\n", elapsed))
  
  cat("[STEP 3/3] Combining results...\n")
  start_time <- Sys.time()
  
  setDT(all_results)
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("  ✓ %.1fs\n\n", elapsed))
  
  list(results = all_results, n_sims = n_sims)
}



# Fixed DK dominator analysis with proper data type handling
analyze_dk_dominator_points <- function(sim_results, max_sample_size = 1000000) {
  setDT(sim_results)
  
  n_total_results <- nrow(sim_results)
  
  
  # Sample for performance if dataset is very large
  analysis_data <- sim_results
  if (n_total_results > max_sample_size) {
    sample_size <- max_sample_size
    sample_indices <- sample(n_total_results, sample_size)
    analysis_data <- sim_results[sample_indices]
    cat(
      "Using sample of",
      sample_size,
      "results for analysis (",
      round(sample_size / n_total_results * 100, 1),
      "%)\n"
    )
  }
  
  # Check if SimID exists, if not create it
  if (!"SimID" %in% names(analysis_data)) {
    n_drivers <- length(unique(analysis_data$Name))
    analysis_data[, SimID := rep(1:(nrow(analysis_data) %/% n_drivers + 1), each = n_drivers)[1:nrow(analysis_data)]]
    created_simid <- TRUE
  } else {
    created_simid <- FALSE
  }
  
  # Ensure numeric columns are properly typed
  analysis_data[, Starting := as.numeric(Starting)]
  analysis_data[, DKSalary := as.numeric(DKSalary)]
  analysis_data[, DKDominatorPoints := as.numeric(DKDominatorPoints)]
  
  # Calculate dominator rank for each simulation
  analysis_data[, DKDominatorRank := frank(-DKDominatorPoints, ties.method = "min"), by = SimID]
  
  # Use explicit type conversion in aggregation
  results <- analysis_data[, .(
    Starting = as.numeric(first(Starting)),
    DKSalary = as.numeric(first(DKSalary)),
    Avg_Dom = as.numeric(mean(DKDominatorPoints, na.rm = TRUE)),
    Median_Dom = as.numeric(median(DKDominatorPoints, na.rm = TRUE)),
    Max_Dom = as.numeric(max(DKDominatorPoints, na.rm = TRUE)),
    Avg_DomRank = as.numeric(mean(DKDominatorRank, na.rm = TRUE)),
    Median_DomRank = as.numeric(median(DKDominatorRank, na.rm = TRUE)),
    Top_DomRate = as.numeric(mean(DKDominatorRank == 1, na.rm = TRUE) * 100),
    Top3_DomRate = as.numeric(mean(DKDominatorRank <= 3, na.rm = TRUE) * 100),
    Top5_DomRate = as.numeric(mean(DKDominatorRank <= 5, na.rm = TRUE) * 100),
    Top10_DomRate = as.numeric(mean(DKDominatorRank <= 10, na.rm = TRUE) * 100)
  ), by = Name]
  
  # Clean up temporary columns
  analysis_data[, DKDominatorRank := NULL]
  if (created_simid) {
    analysis_data[, SimID := NULL]
  }
  
  # Round numeric columns
  numeric_cols <- c(
    "Starting",
    "DKSalary",
    "Avg_Dom",
    "Median_Dom",
    "Max_Dom",
    "Avg_DomRank",
    "Median_DomRank",
    "Top_DomRate",
    "Top3_DomRate",
    "Top5_DomRate",
    "Top10_DomRate"
  )
  
  for (col in numeric_cols) {
    if (col %in% names(results)) {
      results[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  # Sort by Average Dominator Points in descending order
  setorder(results, -Avg_Dom)
  
  return(results)
}

# DraftKings contest driver analysis
calculate_dk_contest_driver_analysis <- function(contest_results, fantasy_analysis) {
  if (is.null(contest_results) || nrow(contest_results) == 0) {
    return(data.frame(Message = "No contest results available."))
  }
  
  setDT(contest_results)
  if (!is.null(fantasy_analysis))
    setDT(fantasy_analysis)
  
  driver_cols <- paste0("Driver", 1:6)
  all_drivers <- unique(unlist(contest_results[, ..driver_cols]))
  
  metrics_data <- data.table(
    DKName = all_drivers,
    Name = NA_character_,
    DKSalary = NA_real_,
    DKOP = NA_real_,
    ContestWinRate = 0,
    AvgLineupWinRate = 0,
    TopLineupWinRate = 0,
    Starting = NA_real_,
    Proj = NA_real_
  )
  
  # Calculate contest performance metrics for each driver
  for (driver in all_drivers) {
    # Find all lineups containing this driver
    driver_appears <- logical(nrow(contest_results))
    for (col in driver_cols) {
      driver_appears <- driver_appears |
        (contest_results[[col]] == driver)
    }
    
    if (any(driver_appears)) {
      driver_lineups <- contest_results[driver_appears]
      
      # Calculate metrics
      avg_win_rate <- mean(driver_lineups$WinRate, na.rm = TRUE)
      top_win_rate <- max(driver_lineups$WinRate, na.rm = TRUE)
      contest_win_rate <- (sum(driver_lineups$WinRate >= 50) / nrow(driver_lineups)) * 100
      
      metrics_data[DKName == driver, `:=`(
        ContestWinRate = contest_win_rate,
        AvgLineupWinRate = avg_win_rate,
        TopLineupWinRate = top_win_rate
      )]
    }
  }
  
  # Add driver information from fantasy analysis
  if (!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    for (i in 1:nrow(metrics_data)) {
      dk_name <- metrics_data$DKName[i]
      match_idx <- which(fantasy_analysis$DKName == dk_name)
      if (length(match_idx) > 0) {
        idx <- match_idx[1]
        metrics_data$Name[i] <- fantasy_analysis$Name[idx]
        metrics_data$DKSalary[i] <- fantasy_analysis$DKSalary[idx]
        metrics_data$DKOP[i] <- fantasy_analysis$DKOP[idx]
        metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
        if ("Median_Fantasy_Pts" %in% names(fantasy_analysis)) {
          metrics_data$Proj[i] <- fantasy_analysis$Median_Fantasy_Pts[idx]
        }
      }
    }
  }
  
  # Sort by average lineup win rate
  setorder(metrics_data, -AvgLineupWinRate)
  
  return(as.data.frame(metrics_data))
}

# FanDuel contest driver analysis
calculate_fd_contest_driver_analysis <- function(contest_results, fantasy_analysis) {
  if (is.null(contest_results) || nrow(contest_results) == 0) {
    return(data.frame(Message = "No contest results available."))
  }
  
  setDT(contest_results)
  if (!is.null(fantasy_analysis))
    setDT(fantasy_analysis)
  
  driver_cols <- paste0("Driver", 1:5)
  all_drivers <- unique(unlist(contest_results[, ..driver_cols]))
  
  metrics_data <- data.table(
    FDName = all_drivers,
    Name = NA_character_,
    FDSalary = NA_real_,
    FDOP = NA_real_,
    ContestWinRate = 0,
    AvgLineupWinRate = 0,
    TopLineupWinRate = 0,
    Starting = NA_real_,
    Proj = NA_real_
  )
  
  # Calculate contest performance metrics for each driver
  for (driver in all_drivers) {
    # Find all lineups containing this driver
    driver_appears <- logical(nrow(contest_results))
    for (col in driver_cols) {
      driver_appears <- driver_appears |
        (contest_results[[col]] == driver)
    }
    
    if (any(driver_appears)) {
      driver_lineups <- contest_results[driver_appears]
      
      # Calculate metrics
      avg_win_rate <- mean(driver_lineups$WinRate, na.rm = TRUE)
      top_win_rate <- max(driver_lineups$WinRate, na.rm = TRUE)
      contest_win_rate <- (sum(driver_lineups$WinRate >= 50) / nrow(driver_lineups)) * 100
      
      metrics_data[FDName == driver, `:=`(
        ContestWinRate = contest_win_rate,
        AvgLineupWinRate = avg_win_rate,
        TopLineupWinRate = top_win_rate
      )]
    }
  }
  
  # Add driver information from fantasy analysis
  if (!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    for (i in 1:nrow(metrics_data)) {
      fd_name <- metrics_data$FDName[i]
      match_idx <- which(fantasy_analysis$FDName == fd_name)
      if (length(match_idx) > 0) {
        idx <- match_idx[1]
        metrics_data$Name[i] <- fantasy_analysis$Name[idx]
        metrics_data$FDSalary[i] <- fantasy_analysis$FDSalary[idx]
        metrics_data$FDOP[i] <- fantasy_analysis$FDOP[idx]
        metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
        if ("Median_Fantasy_Pts" %in% names(fantasy_analysis)) {
          metrics_data$Proj[i] <- fantasy_analysis$Median_Fantasy_Pts[idx]
        }
      }
    }
  }
  
  # Check if FDOP is already in percentage format
  if (!is.null(metrics_data$FDOP) &&
      !all(is.na(metrics_data$FDOP))) {
    if (max(metrics_data$FDOP, na.rm = TRUE) <= 1) {
      metrics_data[, FDOP := FDOP * 100]
    }
  }
  
  # Sort by average lineup win rate
  setorder(metrics_data, -AvgLineupWinRate)
  
  return(as.data.frame(metrics_data))
}

# Fixed FD dominator analysis with proper data type handling
analyze_fd_dominator_points <- function(sim_results, max_sample_size = 1000000) {
  setDT(sim_results)
  
  n_total_results <- nrow(sim_results)
  
  
  # Sample for performance if dataset is very large
  analysis_data <- sim_results
  if (n_total_results > max_sample_size) {
    sample_size <- max_sample_size
    sample_indices <- sample(n_total_results, sample_size)
    analysis_data <- sim_results[sample_indices]
    cat(
      "Using sample of",
      sample_size,
      "results for analysis (",
      round(sample_size / n_total_results * 100, 1),
      "%)\n"
    )
  }
  
  # Check if SimID exists, if not create it
  if (!"SimID" %in% names(analysis_data)) {
    n_drivers <- length(unique(analysis_data$Name))
    analysis_data[, SimID := rep(1:(nrow(analysis_data) %/% n_drivers + 1), each = n_drivers)[1:nrow(analysis_data)]]
    created_simid <- TRUE
  } else {
    created_simid <- FALSE
  }
  
  # Ensure numeric columns are properly typed
  analysis_data[, Starting := as.numeric(Starting)]
  analysis_data[, FDSalary := as.numeric(FDSalary)]
  analysis_data[, FDDominatorPoints := as.numeric(FDDominatorPoints)]
  
  # Calculate dominator rank for each simulation
  analysis_data[, FDDominatorRank := frank(-FDDominatorPoints, ties.method = "min"), by = SimID]
  
  # Use explicit type conversion in aggregation
  results <- analysis_data[, .(
    Starting = as.numeric(first(Starting)),
    FDSalary = as.numeric(first(FDSalary)),
    Avg_Dom = as.numeric(mean(FDDominatorPoints, na.rm = TRUE)),
    Median_Dom = as.numeric(median(FDDominatorPoints, na.rm = TRUE)),
    Max_Dom = as.numeric(max(FDDominatorPoints, na.rm = TRUE)),
    Avg_DomRank = as.numeric(mean(FDDominatorRank, na.rm = TRUE)),
    Median_DomRank = as.numeric(median(FDDominatorRank, na.rm = TRUE)),
    Top_DomRate = as.numeric(mean(FDDominatorRank == 1, na.rm = TRUE) * 100),
    Top3_DomRate = as.numeric(mean(FDDominatorRank <= 3, na.rm = TRUE) * 100),
    Top5_DomRate = as.numeric(mean(FDDominatorRank <= 5, na.rm = TRUE) * 100)
  ), by = Name]
  
  # Clean up temporary columns
  analysis_data[, FDDominatorRank := NULL]
  if (created_simid) {
    analysis_data[, SimID := NULL]
  }
  
  # Round numeric columns
  numeric_cols <- c(
    "Starting",
    "FDSalary",
    "Avg_Dom",
    "Median_Dom",
    "Max_Dom",
    "Avg_DomRank",
    "Median_DomRank",
    "Top_DomRate",
    "Top3_DomRate",
    "Top5_DomRate"
  )
  
  for (col in numeric_cols) {
    if (col %in% names(results)) {
      results[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  # Sort by Average Dominator Points in descending order
  setorder(results, -Avg_Dom)
  
  return(results)
}

# Fixed finishing positions analysis with proper data type handling
analyze_finishing_positions <- function(sim_results, max_display_rows = 50) {
  setDT(sim_results)
  
  # For very large datasets, sample for display but keep full analysis
  n_total_results <- nrow(sim_results)
  n_drivers <- length(unique(sim_results$Name))
  n_sims <- n_total_results / n_drivers
  
  cat("Analyzing finishing positions:",
      n_drivers,
      "drivers,",
      n_sims,
      "simulations\n")
  
  # Ensure FinishPosition is numeric
  sim_results[, FinishPosition := as.numeric(FinishPosition)]
  
  # Always do full analysis for accuracy with explicit type conversion
  results <- sim_results[, .(
    Win_Rate = as.numeric(mean(FinishPosition == 1, na.rm = TRUE) * 100),
    T3_Rate = as.numeric(mean(FinishPosition <= 3, na.rm = TRUE) * 100),
    T5_Rate = as.numeric(mean(FinishPosition <= 5, na.rm = TRUE) * 100),
    T10_Rate = as.numeric(mean(FinishPosition <= 10, na.rm = TRUE) * 100),
    T15_Rate = as.numeric(mean(FinishPosition <= 15, na.rm = TRUE) * 100),
    T20_Rate = as.numeric(mean(FinishPosition <= 20, na.rm = TRUE) * 100),
    T25_Rate = as.numeric(mean(FinishPosition <= 25, na.rm = TRUE) * 100),
    T30_Rate = as.numeric(mean(FinishPosition <= 30, na.rm = TRUE) * 100),
    Avg_Finish = as.numeric(mean(FinishPosition, na.rm = TRUE)),
    Median = as.numeric(median(FinishPosition, na.rm = TRUE))
  ), by = Name]
  
  results <- results[order(Avg_Finish)]
  
  # Round all numeric columns
  for (col in setdiff(names(results), "Name")) {
    results[, (col) := round(as.numeric(get(col)), 1)]
  }
  
  if (nrow(results) > max_display_rows) {
    cat("Note: Showing all",
        nrow(results),
        "drivers in finishing analysis\n")
  }
  
  return(results)
}

# Fixed fantasy points analysis with proper data type handling
analyze_dk_fantasy_points <- function(sim_results, max_sample_size = 1000000) {
  setDT(sim_results)
  
  n_total_results <- nrow(sim_results)
  
  
  # Sample for performance if dataset is very large
  analysis_data <- sim_results
  if (n_total_results > max_sample_size) {
    sample_size <- max_sample_size
    sample_indices <- sample(n_total_results, sample_size)
    analysis_data <- sim_results[sample_indices]
    cat(
      "Using sample of",
      sample_size,
      "results for analysis (",
      round(sample_size / n_total_results * 100, 1),
      "%)\n"
    )
  }
  
  # Ensure numeric columns are properly typed
  analysis_data[, DKSalary := as.numeric(DKSalary)]
  analysis_data[, Starting := as.numeric(Starting)]
  analysis_data[, DKOP := as.numeric(DKOP)]
  analysis_data[, DKFantasyPoints := as.numeric(DKFantasyPoints)]
  
  # Use explicit type conversion in aggregation
  results <- analysis_data[, .(
    DKSalary = as.numeric(first(DKSalary)),
    Starting = as.numeric(first(Starting)),
    DKOP = as.numeric(first(DKOP)),
    Median_Fantasy_Pts = as.numeric(median(DKFantasyPoints, na.rm = TRUE)),
    FP_90thPct = as.numeric(quantile(DKFantasyPoints, 0.9, na.rm = TRUE))
  ), by = .(Name)]
  
  # Add PPD calculation with proper type handling
  results[, PPD := as.numeric(Median_Fantasy_Pts / (DKSalary / 1000))]
  
  # Convert DKOP to percentage if needed
  if (max(results$DKOP, na.rm = TRUE) <= 1) {
    results[, DKOP := as.numeric(DKOP * 100)]
  }
  
  # Round all numeric columns
  numeric_cols <- c("DKSalary",
                    "Starting",
                    "DKOP",
                    "Median_Fantasy_Pts",
                    "FP_90thPct",
                    "PPD")
  for (col in numeric_cols) {
    if (col %in% names(results)) {
      results[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  return(results)
}

# Fixed FD fantasy points analysis
analyze_fd_fantasy_points <- function(sim_results, max_sample_size = 1000000) {
  setDT(sim_results)
  
  n_total_results <- nrow(sim_results)
  
  
  # Sample for performance if dataset is very large
  analysis_data <- sim_results
  if (n_total_results > max_sample_size) {
    sample_size <- max_sample_size
    sample_indices <- sample(n_total_results, sample_size)
    analysis_data <- sim_results[sample_indices]
    cat(
      "Using sample of",
      sample_size,
      "results for analysis (",
      round(sample_size / n_total_results * 100, 1),
      "%)\n"
    )
  }
  
  # Ensure numeric columns are properly typed
  analysis_data[, FDSalary := as.numeric(FDSalary)]
  analysis_data[, Starting := as.numeric(Starting)]
  analysis_data[, FDOP := as.numeric(FDOP)]
  analysis_data[, FDFantasyPoints := as.numeric(FDFantasyPoints)]
  
  # Use explicit type conversion in aggregation
  results <- analysis_data[, .(
    FDSalary = as.numeric(first(FDSalary)),
    Starting = as.numeric(first(Starting)),
    FDOP = as.numeric(first(FDOP)),
    Median_Fantasy_Pts = as.numeric(median(FDFantasyPoints, na.rm = TRUE)),
    FP_90thPct = as.numeric(quantile(FDFantasyPoints, 0.9, na.rm = TRUE))
  ), by = .(Name)]
  
  # Add PPD calculation with proper type handling
  results[, PPD := as.numeric(Median_Fantasy_Pts / (FDSalary / 1000))]
  
  # Convert FDOP to percentage if needed
  if (max(results$FDOP, na.rm = TRUE) <= 1) {
    results[, FDOP := as.numeric(FDOP * 100)]
  }
  
  # Round all numeric columns
  numeric_cols <- c("FDSalary",
                    "Starting",
                    "FDOP",
                    "Median_Fantasy_Pts",
                    "FP_90thPct",
                    "PPD")
  for (col in numeric_cols) {
    if (col %in% names(results)) {
      results[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  return(results)
}

# Fixed FD lap analysis
analyze_fd_lap_points <- function(sim_results) {
  setDT(sim_results)
  
  # Ensure numeric columns are properly typed
  sim_results[, FDSalary := as.numeric(FDSalary)]
  sim_results[, Starting := as.numeric(Starting)]
  sim_results[, FDLapPoints := as.numeric(FDLapPoints)]
  
  # Use explicit type conversion in aggregation
  results <- sim_results[, .(
    Starting = as.numeric(first(Starting)),
    FDSalary = as.numeric(first(FDSalary)),
    Avg_Lap = as.numeric(mean(FDLapPoints, na.rm = TRUE)),
    Median_Lap = as.numeric(median(FDLapPoints, na.rm = TRUE)),
    Max_Lap = as.numeric(max(FDLapPoints, na.rm = TRUE)),
    Min_Lap = as.numeric(min(FDLapPoints, na.rm = TRUE))
  ), by = Name]
  
  # Round numeric columns
  numeric_cols <- c("Starting",
                    "FDSalary",
                    "Avg_Lap",
                    "Median_Lap",
                    "Max_Lap",
                    "Min_Lap")
  for (col in numeric_cols) {
    if (col %in% names(results)) {
      results[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  # Sort by Average Lap Points in descending order
  setorder(results, -Avg_Lap)
  
  return(results)
}




analyze_simulation_accuracy <- function(sim_results, input_data) {
  # Make sure we're working with data.tables
  setDT(sim_results)
  setDT(input_data)
  
  # Check if SimID exists, if not create it temporarily
  if (!"SimID" %in% names(sim_results)) {
    n_drivers <- length(unique(sim_results$Name))
    sim_results[, SimID := rep(1:(nrow(sim_results) %/% n_drivers + 1), each = n_drivers)[1:nrow(sim_results)]]
    created_simid <- TRUE
  } else {
    created_simid <- FALSE
  }
  
  # Get unique drivers
  drivers <- unique(input_data$Name)
  metrics <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
  
  results <- data.table()
  
  for (driver in drivers) {
    # Get simulation results for this driver
    driver_sims <- sim_results[Name == driver]
    driver_input <- input_data[Name == driver]
    
    total_sims <- nrow(driver_sims)
    if (total_sims == 0)
      next
    
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
    
    for (metric in metrics) {
      input_value <- driver_input[[metric]]
      sim_value <- sim_pcts[metric]
      
      if (!is.na(input_value)) {
        results <- rbind(
          results,
          data.table(
            Driver = driver,
            Metric = metric,
            Input = input_value * 100,
            Simulated = sim_value * 100,
            Difference = abs(input_value * 100 - sim_value * 100)
          )
        )
      }
    }
  }
  
  # Clean up temporary SimID if we created it
  if (created_simid) {
    sim_results[, SimID := NULL]
  }
  
  # Sort by driver and metric
  setorder(results, Driver, Metric)
  
  return(results)
}



# Define UI
# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(
    title = tags$div(style = "display: flex; align-items: center; font-weight: bold;", "NASCAR Simulations"),
    titleWidth = 300
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    useShinyjs(),
    div(
      style = "text-align: center; padding: 5px; margin-bottom: 5px;",
      tags$img(
        src = "logo.jpg",
        height = "175px",
        width = "auto",
        style = "border: 2px solid #FFD700; border-radius: 10px;"
      )
    ),
    br(),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Check", tabName = "upload", icon = icon("check")),
      menuItem(
        "Finish Analysis",
        tabName = "finish_analysis",
        icon = icon("chart-line")
      ),
      menuItem(
        "Dominator Analysis",
        tabName = "dominator",
        icon = icon("trophy")
      ),
      menuItem(
        "Projections",
        tabName = "fantasy",
        icon = icon("bars-progress")
      ),
      menuItem(
        "All Lineups",
        tabName = "optimal_lineups",
        icon = icon("calculator")
      ),
      menuItem(
        "Lineup Builder",
        tabName = "lineup_builder",
        icon = icon("flask")
      )
    ),
    br(),
    fileInput("excel_file", "Upload Excel File", accept = c(".xlsx")),
    numericInput(
      "n_sims",
      "Number of Simulations:",
      value = 10000,
      min = 100,
      max = 100000
    ),
    actionButton(
      "run_sim",
      "Run Simulation",
      class = "btn-primary",
      style = "margin: 15px; width: 90%"
    ),
    div(id = "sim_status", class = "text-center", style = "margin-top: 10px;")
  ),
  
  # Dashboard body
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "gts_theme.css")
    ),
    
    tabItems(
      # Upload Tab
      tabItem(tabName = "upload", uiOutput("upload_content")),
      
      # Finish Analysis Tab
      tabItem(tabName = "finish_analysis", fluidRow(
        div(
          style = "text-align: right; margin: 10px 15px;",
          downloadButton(
            'download_full_sims',
            'Download Full Simulation Results',
            style = "margin-top: 5px;"
          )
        )
      ), fluidRow(
        box(
          width = 12,
          title = "Simulated Finishing Results",
          DTOutput("driver_stats") %>% withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "Finish Position Boxplot",
          plotlyOutput("position_box", height = "1000px") %>% withSpinner(color = "#FFD700")
        )
      )),
      
      # Dominator Analysis Tab
      tabItem(tabName = "dominator", uiOutput("dominator_ui")),
      
      # Fantasy Projections Tab
      tabItem(tabName = "fantasy", uiOutput("fantasy_ui")),
      
      tabItem(
        tabName = "optimal_lineups",
        
        fluidRow(
          box(
            width = 12,
            title = "Score Lineups",
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            div(
              style = "display: flex; gap: 15px; margin: 10px 0;",
              conditionalPanel(
                condition = "output.has_draftkings === 'true'",
                actionButton(
                  "run_dk_optimization",
                  "Score DK",
                  class = "btn-warning",
                  icon = icon("calculator"),
                  style = "flex: 1; background-color: #FFD700; color: #000; border: none; font-weight: bold;"
                )
              ),
              conditionalPanel(
                condition = "output.has_fanduel === 'true'",
                actionButton(
                  "run_fd_optimization",
                  "Score FD",
                  class = "btn-warning",
                  icon = icon("calculator"),
                  style = "flex: 1; background-color: #FFD700; color: #000; border: none; font-weight: bold;"
                )
              )
            )
          )
        ),
        
        uiOutput("optimal_lineups_tabs_ui")
      ),
      tabItem(tabName = "lineup_builder", fluidRow(
        box(
          title = "Lineup Builder - Build Your Portfolio",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          uiOutput("lineup_builder_tabs_ui")
        )
      ))
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
    dk_optimal_lineups = NULL,
    fd_optimal_lineups = NULL,
    dk_driver_exposure = NULL,
    fd_driver_exposure = NULL,
    dk_random_lineups = NULL,
    fd_random_lineups = NULL,
    file_uploaded = FALSE,
    simulation_complete = FALSE,
    field_lineups = NULL,
    dk_contest_results = NULL,
    fd_contest_results = NULL,
    dk_optimal_lineups_display = NULL,
    dk_optimal_lineups_full = NULL,
    fd_optimal_lineups_display = NULL,
    fd_optimal_lineups_full = NULL,
    dk_contest_results_full = NULL,
    fd_contest_results_full = NULL,
    dk_contest_driver_analysis = NULL,
    fd_contest_driver_analysis = NULL,
    updating_sliders = FALSE
  )
  
  # Configure memory settings when server starts
  configure_memory_settings()
  
  
  
  dk_filters <- reactive({
    list(
      min_top1_count = input$dk_min_top1_slider,
      # FIXED: was dk_min_top1_count
      min_top3_count = input$dk_min_top3_slider,
      # FIXED: was dk_min_top3_count
      min_top5_count = input$dk_min_top5_slider,
      # FIXED: was dk_min_top5_count
      min_cumulative_ownership = input$dk_ownership_range[1],
      max_cumulative_ownership = input$dk_ownership_range[2],
      min_geometric_mean = input$dk_geometric_range[1],
      max_geometric_mean = input$dk_geometric_range[2],
      min_cumulative_starting = input$dk_starting_range[1],
      max_cumulative_starting = input$dk_starting_range[2],
      min_geometric_starting = input$dk_starting_geo_range[1],
      max_geometric_starting = input$dk_starting_geo_range[2],
      excluded_drivers = input$dk_excluded_drivers
    )
  })
  
  
  fd_filters <- reactive({
    list(
      min_top1_count = input$fd_min_top1_slider,
      # FIXED: was fd_min_top1_count
      min_top3_count = input$fd_min_top3_slider,
      # FIXED: was fd_min_top3_count
      min_top5_count = input$fd_min_top5_slider,
      # FIXED: was fd_min_top5_count
      min_cumulative_ownership = input$fd_ownership_range[1],
      max_cumulative_ownership = input$fd_ownership_range[2],
      min_geometric_mean = input$fd_geometric_range[1],
      max_geometric_mean = input$fd_geometric_range[2],
      min_cumulative_starting = input$fd_starting_range[1],
      max_cumulative_starting = input$fd_starting_range[2],
      min_geometric_starting = input$fd_starting_geo_range[1],
      max_geometric_starting = input$fd_starting_geo_range[2],
      excluded_drivers = input$fd_excluded_drivers
    )
  })
  
  
  # Periodic memory cleanup (every 5 minutes)
  observe({
    invalidateLater(300000, session)  # 5 minutes
    
    # Only clean up if we have significant data
    if (!is.null(rv$simulation_results)) {
      cleanup_memory(verbose = FALSE)
    }
  })
  
  # Fixed DK filter observer
  observeEvent({
    list(
      input$dk_min_top1_slider,
      input$dk_min_top3_slider,
      input$dk_min_top5_slider,
      input$dk_ownership_range,
      input$dk_geometric_range,
      input$dk_starting_range,
      input$dk_starting_geo_range,
      input$dk_salary_range,
      input$dk_excluded_drivers
    )
  }, {
    # Only process if we have data
    if (!is.null(rv$dk_optimal_lineups) &&
        !is.null(rv$dk_driver_exposure)) {
      # Get existing mapping
      existing_mapping <- rv$dk_driver_exposure[, c("DKName", "Name", "DKSalary", "DKOP", "Starting", "Proj")]
      
      # Build current filters
      current_filters <- list(
        min_top1_count = if (is.null(input$dk_min_top1_slider))
          0
        else
          input$dk_min_top1_slider[1],
        max_top1_count = if (is.null(input$dk_min_top1_slider))
          100
        else
          input$dk_min_top1_slider[2],
        min_top3_count = if (is.null(input$dk_min_top3_slider))
          0
        else
          input$dk_min_top3_slider[1],
        max_top3_count = if (is.null(input$dk_min_top3_slider))
          100
        else
          input$dk_min_top3_slider[2],
        min_top5_count = if (is.null(input$dk_min_top5_slider))
          0
        else
          input$dk_min_top5_slider[1],
        max_top5_count = if (is.null(input$dk_min_top5_slider))
          100
        else
          input$dk_min_top5_slider[2],
        min_cumulative_ownership = if (is.null(input$dk_ownership_range))
          0
        else
          input$dk_ownership_range[1],
        max_cumulative_ownership = if (is.null(input$dk_ownership_range))
          600
        else
          input$dk_ownership_range[2],
        min_geometric_mean = if (is.null(input$dk_geometric_range))
          0
        else
          input$dk_geometric_range[1],
        max_geometric_mean = if (is.null(input$dk_geometric_range))
          100
        else
          input$dk_geometric_range[2],
        min_cumulative_starting = if (is.null(input$dk_starting_range))
          6
        else
          input$dk_starting_range[1],
        max_cumulative_starting = if (is.null(input$dk_starting_range))
          240
        else
          input$dk_starting_range[2],
        min_geometric_starting = if (is.null(input$dk_starting_geo_range))
          1
        else
          input$dk_starting_geo_range[1],
        max_geometric_starting = if (is.null(input$dk_starting_geo_range))
          40
        else
          input$dk_starting_geo_range[2],
        min_total_salary = if (is.null(input$dk_salary_range))
          42500
        else
          input$dk_salary_range[1] * 1000,
        max_total_salary = if (is.null(input$dk_salary_range))
          50000
        else
          input$dk_salary_range[2] * 1000,
        excluded_drivers = input$dk_excluded_drivers
      )
      
      # Update driver exposure
      rv$dk_driver_exposure <- calculate_dk_driver_exposure(
        rv$dk_optimal_lineups,
        existing_mapping,
        rv$dk_random_lineups,
        current_filters
      )
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Fixed FD filter observer
  observeEvent({
    list(
      input$fd_min_top1_slider,
      input$fd_min_top3_slider,
      input$fd_min_top5_slider,
      input$fd_ownership_range,
      input$fd_geometric_range,
      input$fd_starting_range,
      input$fd_starting_geo_range,
      input$fd_salary_range,
      input$fd_excluded_drivers
    )
  }, {
    # Only process if we have data
    if (!is.null(rv$fd_optimal_lineups) &&
        !is.null(rv$fd_driver_exposure)) {
      # Get existing mapping
      existing_mapping <- rv$fd_driver_exposure[, c("FDName", "Name", "FDSalary", "FDOP", "Starting", "Proj")]
      
      # Build current filters
      current_filters <- list(
        min_top1_count = if (is.null(input$fd_min_top1_slider))
          0
        else
          input$fd_min_top1_slider[1],
        max_top1_count = if (is.null(input$fd_min_top1_slider))
          100
        else
          input$fd_min_top1_slider[2],
        min_top3_count = if (is.null(input$fd_min_top3_slider))
          0
        else
          input$fd_min_top3_slider[1],
        max_top3_count = if (is.null(input$fd_min_top3_slider))
          100
        else
          input$fd_min_top3_slider[2],
        min_top5_count = if (is.null(input$fd_min_top5_slider))
          0
        else
          input$fd_min_top5_slider[1],
        max_top5_count = if (is.null(input$fd_min_top5_slider))
          100
        else
          input$fd_min_top5_slider[2],
        min_cumulative_ownership = if (is.null(input$fd_ownership_range))
          0
        else
          input$fd_ownership_range[1],
        max_cumulative_ownership = if (is.null(input$fd_ownership_range))
          500
        else
          input$fd_ownership_range[2],
        min_geometric_mean = if (is.null(input$fd_geometric_range))
          0
        else
          input$fd_geometric_range[1],
        max_geometric_mean = if (is.null(input$fd_geometric_range))
          100
        else
          input$fd_geometric_range[2],
        min_cumulative_starting = if (is.null(input$fd_starting_range))
          5
        else
          input$fd_starting_range[1],
        max_cumulative_starting = if (is.null(input$fd_starting_range))
          200
        else
          input$fd_starting_range[2],
        min_geometric_starting = if (is.null(input$fd_starting_geo_range))
          1
        else
          input$fd_starting_geo_range[1],
        max_geometric_starting = if (is.null(input$fd_starting_geo_range))
          40
        else
          input$fd_starting_geo_range[2],
        min_total_salary = if (is.null(input$fd_salary_range))
          42500
        else
          input$fd_salary_range[1] * 1000,
        max_total_salary = if (is.null(input$fd_salary_range))
          50000
        else
          input$fd_salary_range[2] * 1000,
        excluded_drivers = input$fd_excluded_drivers
      )
      
      # Update driver exposure
      rv$fd_driver_exposure <- calculate_fd_driver_exposure(
        rv$fd_optimal_lineups,
        existing_mapping,
        rv$fd_random_lineups,
        current_filters
      )
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  # Cleanup when simulation data changes
  observeEvent(rv$simulation_results,
               {
                 # Clean up previous analysis results when new simulation starts
                 if (!is.null(rv$simulation_results)) {
                   # Remove any large intermediate objects that might be hanging around
                   cleanup_memory(verbose = FALSE)
                 }
               },
               ignoreNULL = TRUE,
               ignoreInit = TRUE)
  
  observeEvent(rv$sliders_initialized$fd, {
    if (isTRUE(rv$sliders_initialized$fd)) {
      # Force a recalculation of the filtered pool size after a short delay
      observe({
        invalidateLater(500, session)  # Wait 500ms for inputs to stabilize
        if (exists("input") &&
            !is.null(input$fd_ownership_range)) {
          # Trigger the output to recalculate by invalidating it
          outputOptions(output,
                        "fd_filtered_pool_size",
                        suspendWhenHidden = FALSE)
        }
      })
    }
  }, once = TRUE)
  
  
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
  
  output$has_fd_lineups <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(
      !is.null(rv$fd_optimal_lineups) && nrow(rv$fd_optimal_lineups) > 0
    ))
    return(result)
  })
  outputOptions(output, "has_fd_lineups", suspendWhenHidden = FALSE)
  
  output$has_dk_lineups <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(
      !is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0
    ))
    return(result)
  })
  outputOptions(output, "has_dk_lineups", suspendWhenHidden = FALSE)
  
  output$has_fd_random_lineups <- reactive({
    result <- tolower(as.character(
      !is.null(rv$fd_random_lineups) && nrow(rv$fd_random_lineups) > 0
    ))
    return(result)
  })
  outputOptions(output, "has_fd_random_lineups", suspendWhenHidden = FALSE)
  
  # Add these around line 1420
  output$has_dk_contest_results <- reactive({
    result <- tolower(as.character(!is.null(rv$dk_contest_results_full)))
    return(result)
  })
  outputOptions(output, "has_dk_contest_results", suspendWhenHidden = FALSE)
  
  output$has_fd_contest_results <- reactive({
    result <- tolower(as.character(!is.null(rv$fd_contest_results_full)))
    return(result)
  })
  outputOptions(output, "has_fd_contest_results", suspendWhenHidden = FALSE)
  
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
        rv$field_lineups <- NULL
        
        
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
          for (col in columns_to_remove) {
            if (col %in% colnames(display_data)) {
              display_data[[col]] <- NULL
            }
          }
          
          # Create the datatable with better column alignment
          dt <- datatable(
            display_data,
            options = list(
              scrollX = TRUE,
              pageLength = -1,
              # Show all rows
              autoWidth = FALSE,
              # Don't use autoWidth for better control
              dom = "t",
              # Only show table ('t'), no search/pagination
              ordering = TRUE,
              # Allow sorting
              columnDefs = list(
                list(className = 'dt-center', targets = "_all")  # Center-align all columns
              ),
              scrollCollapse = TRUE,
              fixedColumns = TRUE
            ),
            class = 'cell-border stripe display compact',
            # Added compact class for tighter spacing
            rownames = FALSE,
            width = "100%",
            # Use full width
            height = "auto"
          )
          
          # Formatting for various columns
          if ("DKOP" %in% colnames(display_data)) {
            dt <- dt %>% formatRound("DKOP", digits = 2)
          }
          
          if ("FDOP" %in% colnames(display_data)) {
            dt <- dt %>% formatRound("FDOP", digits = 2)
          }
          
          # Format W, T3, T5, etc. columns to 2 decimal places
          numeric_cols <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
          for (col in numeric_cols) {
            if (col %in% colnames(display_data)) {
              dt <- dt %>% formatRound(col, digits = 2)
            }
          }
          
          # Format salary columns as currency
          if ("DKSalary" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("DKSalary",
                                        currency = "$",
                                        digits = 0)
          }
          
          if ("FDSalary" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("FDSalary",
                                        currency = "$",
                                        digits = 0)
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
  
  observeEvent(input$run_sim, {
    req(rv$processed_data)
    
    cat("=== STARTING NEW SIMULATION ===\n")
    cat("Simulation parameters:", input$n_sims, "simulations\n")
    
    cat("Clearing previous results...\n")
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
    
    cat("Running garbage collection...\n")
    for (i in 1:3) {
      gc(verbose = FALSE, full = TRUE)
      Sys.sleep(0.1)
    }
    
    withProgress(message = 'Running simulations...', value = 0, {
      setProgress(0.05, detail = "Initializing simulation...")
      
      simulation_results <- run_efficient_simulation(
        rv$processed_data$drivers,
        n_sims = input$n_sims,
        race_weights = rv$processed_data$race_weights,
        race_profiles = rv$processed_data$race_profiles,
        fd_laps_data = rv$processed_data$fd_laps
      )
      
      setProgress(0.6, detail = "Storing simulation results...")
      rv$simulation_results <- simulation_results$results
      
      rv$has_draftkings <- "DKFantasyPoints" %in% names(rv$simulation_results)
      rv$has_fanduel <- "FDFantasyPoints" %in% names(rv$simulation_results)
      
      cat(
        "Platform detection: DraftKings =",
        rv$has_draftkings,
        ", FanDuel =",
        rv$has_fanduel,
        "\n"
      )
      
      setProgress(0.65, detail = "Memory cleanup...")
      gc(verbose = FALSE, full = TRUE)
      
      setProgress(0.7, detail = "Analyzing finishing positions...")
      rv$finishing_analysis <- analyze_finishing_positions(rv$simulation_results)
      
      if (rv$has_draftkings) {
        setProgress(0.75, detail = "Analyzing DraftKings dominator points...")
        rv$dk_dominator_analysis <- analyze_dk_dominator_points(rv$simulation_results)
        
        setProgress(0.80, detail = "Analyzing DraftKings fantasy points...")
        rv$dk_fantasy_analysis <- analyze_dk_fantasy_points(rv$simulation_results)
      }
      
      if (rv$has_fanduel) {
        setProgress(0.85, detail = "Analyzing FanDuel dominator points...")
        rv$fd_dominator_analysis <- analyze_fd_dominator_points(rv$simulation_results)
        
        setProgress(0.88, detail = "Analyzing FanDuel lap points...")
        rv$fd_lap_analysis <- analyze_fd_lap_points(rv$simulation_results)
        
        setProgress(0.92, detail = "Analyzing FanDuel fantasy points...")
        rv$fd_fantasy_analysis <- analyze_fd_fantasy_points(rv$simulation_results)
      }
      
      setProgress(0.95, detail = "Calculating simulation accuracy...")
      rv$simulation_accuracy <- analyze_simulation_accuracy(rv$simulation_results, rv$processed_data$drivers)
      
      setProgress(1.0, detail = "Complete!")
      rv$simulation_complete <- TRUE
      
      showModal(modalDialog(title = "Simulation Complete", HTML(
        sprintf(
          "<h4>Successfully completed %s simulations!</h4>
        <p>Platforms available:</p>
        <ul>
          <li>DraftKings: %s</li>
          <li>FanDuel: %s</li>
        </ul>",
          format(input$n_sims, big.mark = ","),
          if (rv$has_draftkings)
            "✓"
          else
            "✗",
          if (rv$has_fanduel)
            "✓"
          else
            "✗"
        )
      ), easyClose = TRUE))
    })
  })
  
  observeEvent(input$go_to_finish_analysis, {
    removeModal()
    updateTabItems(session, "sidebar_menu", selected = "finish_analysis")
  })
  
  
  output$upload_content <- renderUI({
    if (rv$simulation_complete) {
      # Show accuracy analysis after simulation is complete
      tagList(fluidRow(
        box(
          width = 12,
          title = "Simulation Accuracy Analysis",
          DTOutput("accuracy_analysis") %>% withSpinner(color = "#FFD700"),
          downloadButton('downloadAccuracy', 'Download Accuracy Analysis')
        )
      ))
    } else {
      # Show input data before simulation is run
      tagList(fluidRow(
        box(
          width = 12,
          title = "Input Data",
          DTOutput("data_preview") %>% withSpinner(color = "#FFD700")
        )
      ), uiOutput("available_platforms"))
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
        order = list(list(4, 'desc')),
        # Sort by difference in descending order
        columnDefs = list(list(
          targets = c(2, 3, 4), className = 'dt-right'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatRound(c('Input', 'Simulated', 'Difference'), digits = 2) %>%
      formatStyle(
        'Difference',
        background = styleColorBar(c(0, max(
          accuracy_data$Difference
        )), 'rgba(255, 102, 0, 0.5)'),
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
      if (rv$has_draftkings &&
          "DKSalary" %in% names(drivers_data)) {
        join_cols <- c(join_cols, "DKSalary")
      }
      
      if (rv$has_fanduel && "FDSalary" %in% names(drivers_data)) {
        join_cols <- c(join_cols, "FDSalary")
      }
      
      # Join data with available columns
      if (length(join_cols) > 1) {
        # Filter to only include columns that exist
        join_cols <- intersect(join_cols, names(drivers_data))
        
        # Join with available columns
        analysis_data <- merge(analysis_data,
                               drivers_data[, ..join_cols],
                               by = "Name",
                               all.x = TRUE)
        
        # Create dynamic column order based on available columns
        col_order <- c("Name", "Starting")
        
        # Add salary columns if available
        if ("DKSalary" %in% names(analysis_data)) {
          col_order <- c(col_order, "DKSalary")
        }
        if ("FDSalary" %in% names(analysis_data)) {
          col_order <- c(col_order, "FDSalary")
        }
        
        # Add remaining columns
        col_order <- c(
          col_order,
          "Avg_Finish",
          "Median",
          "Win_Rate",
          "T3_Rate",
          "T5_Rate",
          "T10_Rate",
          "T15_Rate",
          "T20_Rate",
          "T25_Rate",
          "T30_Rate"
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
        pageLength = -1,
        # Show all rows
        dom = "t",
        # Only show table ('t'), no search/pagination
        order = list(list(4, 'asc')),
        # Sort by Avg_Finish in ascending order
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
    
    # Format Salary columns
    if ("DKSalary" %in% names(analysis_data)) {
      dt <- dt %>% formatCurrency(
        'DKSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      )
    }
    
    if ("FDSalary" %in% names(analysis_data)) {
      dt <- dt %>% formatCurrency(
        'FDSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      )
    }
    
    dt
  })
  
  # Generate dynamic dominator UI based on available platforms
  output$dominator_ui <- renderUI({
    req(rv$simulation_results)
    
    # Create appropriate UI based on available platforms
    if (rv$has_draftkings && rv$has_fanduel) {
      # Both platforms available - use tabs
      tabsetPanel(
        id = "dominator_tabs",
        tabPanel(
          "DraftKings",
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Dominator Points Analysis",
              DTOutput("dk_dominator_stats") %>% withSpinner(color = "#FFD700")
            )
          ),
          
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Dominator Points Distribution",
              plotlyOutput("dk_dominator_dist", height = "1000px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Dominator Points by Position",
              plotlyOutput("dk_points_by_position", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          )
        ),
        tabPanel(
          "FanDuel",
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Dominator Points Analysis",
              DTOutput("fd_dominator_stats") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Dominator Points Distribution",
              plotlyOutput("fd_dominator_dist", height = "800px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Dominator Points by Position",
              plotlyOutput("fd_points_by_position", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Lap Points by Position",
              plotlyOutput("fd_lap_points_by_position") %>% withSpinner(color = "#FFD700")
            )
          )
        )
      )
    } else if (rv$has_draftkings) {
      # Only DraftKings available
      tagList(fluidRow(
        box(
          width = 12,
          title = "DraftKings Dominator Points Analysis",
          DTOutput("dk_dominator_stats") %>% withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "DraftKings Dominator Points Distribution",
          plotlyOutput("dk_dominator_dist", height = "1000px") %>% withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "DraftKings Dominator Points by Position",
          plotlyOutput("dk_points_by_position", height = "700px") %>% withSpinner(color = "#FFD700")
        )
      ))
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
  
  # Update the fantasy_ui output function to remove the optimization section
  output$fantasy_ui <- renderUI({
    req(rv$simulation_results)
    
    # Create appropriate UI based on available platforms
    if (rv$has_draftkings && rv$has_fanduel) {
      # Both platforms available - use tabs
      tabsetPanel(
        id = "fantasy_tabs",
        tabPanel(
          "DraftKings",
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Fantasy Point Projections",
              DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#FFD700"),
              downloadButton(
                'download_dk_fantasy_projections',
                'Download Projections'
              )
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Fantasy Points vs Salary",
              plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Fantasy Points Distribution",
              plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          )
        ),
        tabPanel(
          "FanDuel",
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Fantasy Point Projections",
              DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#FFD700"),
              downloadButton(
                'download_fd_fantasy_projections',
                'Download Projections'
              )
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Fantasy Points vs Salary",
              plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Fantasy Points Distribution",
              plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          )
        )
      )
    } else if (rv$has_draftkings) {
      # Only DraftKings available
      tagList(fluidRow(
        box(
          width = 12,
          title = "DraftKings Fantasy Point Projections",
          DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#FFD700"),
          downloadButton(
            'download_dk_fantasy_projections',
            'Download Projections'
          )
        )
      ), fluidRow(
        box(
          width = 12,
          title = "DraftKings Fantasy Points vs Salary",
          plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "DraftKings Fantasy Points Distribution",
          plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
        )
      ))
    } else if (rv$has_fanduel) {
      # Only FanDuel available
      tagList(fluidRow(
        box(
          width = 12,
          title = "FanDuel Fantasy Point Projections",
          DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#FFD700"),
          downloadButton(
            'download_fd_fantasy_projections',
            'Download Projections'
          )
        )
      ), fluidRow(
        box(
          width = 12,
          title = "FanDuel Fantasy Points vs Salary",
          plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "FanDuel Fantasy Points Distribution",
          plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
        )
      ))
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
  
  output$lineup_builder_ui <- renderUI({
    if (is.null(rv$dk_optimal_lineups) &&
        is.null(rv$fd_optimal_lineups)) {
      return(
        box(
          width = 12,
          status = "warning",
          title = "No Optimal Lineups Available",
          "Please calculate optimal lineups in the Optimal Lineups tab first."
        )
      )
    }
    
    # Create tab panels based on what's available
    tab_panels <- list()
    
    if (!is.null(rv$dk_optimal_lineups)) {
      tab_panels <- append(tab_panels, list(
        tabPanel(
          "DraftKings",
          value = "dk_tab",
          
          # DraftKings content
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Lineup Filters & Generation",
              
              # Two-column layout
              fluidRow(
                # Left Column - Filters
                column(
                  8,
                  # Performance Filters
                  div(
                    style = "margin-bottom: 15px;",
                    h4("Performance Filters", style = "margin-bottom: 10px; color: #333;"),
                    fluidRow(
                      column(
                        4,
                        sliderInput(
                          "dk_min_top1_slider",
                          "Top 1 Count Range:",
                          min = if ("Top1Pct" %in% names(rv$dk_optimal_lineups))
                            min(rv$dk_optimal_lineups$Top1Pct, na.rm = TRUE)
                          else
                            0,
                          max = if ("Top1Pct" %in% names(rv$dk_optimal_lineups))
                            max(rv$dk_optimal_lineups$Top1Pct, na.rm = TRUE)
                          else
                            100,
                          value = if ("Top1Pct" %in% names(rv$dk_optimal_lineups))
                            c(
                              min(rv$dk_optimal_lineups$Top1Pct, na.rm = TRUE),
                              max(rv$dk_optimal_lineups$Top1Pct, na.rm = TRUE)
                            )
                          else
                            c(0, 100),
                          step = 1,
                          width = "100%"
                        )
                      ),
                      column(
                        4,
                        sliderInput(
                          "dk_min_top3_slider",
                          "Top 3 Count Range:",
                          min = if ("Top5Pct" %in% names(rv$dk_optimal_lineups))
                            min(rv$dk_optimal_lineups$Top5Pct, na.rm = TRUE)
                          else
                            0,
                          max = if ("Top5Pct" %in% names(rv$dk_optimal_lineups))
                            max(rv$dk_optimal_lineups$Top5Pct, na.rm = TRUE)
                          else
                            100,
                          value = if ("Top5Pct" %in% names(rv$dk_optimal_lineups))
                            c(
                              min(rv$dk_optimal_lineups$Top5Pct, na.rm = TRUE),
                              max(rv$dk_optimal_lineups$Top5Pct, na.rm = TRUE)
                            )
                          else
                            c(0, 100),
                          step = 1,
                          width = "100%"
                        )
                      ),
                      column(
                        4,
                        sliderInput(
                          "dk_min_top5_slider",
                          "Top 5 Count Range:",
                          min = if ("Top10Pct" %in% names(rv$dk_optimal_lineups))
                            min(rv$dk_optimal_lineups$Top10Pct, na.rm = TRUE)
                          else
                            0,
                          max = if ("Top10Pct" %in% names(rv$dk_optimal_lineups))
                            max(rv$dk_optimal_lineups$Top10Pct, na.rm = TRUE)
                          else
                            100,
                          value = if ("Top10Pct" %in% names(rv$dk_optimal_lineups))
                            c(
                              min(rv$dk_optimal_lineups$Top10Pct, na.rm = TRUE),
                              max(rv$dk_optimal_lineups$Top10Pct, na.rm = TRUE)
                            )
                          else
                            c(0, 100),
                          step = 1,
                          width = "100%"
                        )
                      )
                    )
                  ),
                  
                  # Ownership Filters
                  div(
                    style = "margin-bottom: 15px;",
                    h4("Ownership Filters", style = "margin-bottom: 10px; color: #333;"),
                    fluidRow(column(
                      6,
                      sliderInput(
                        "dk_ownership_range",
                        "Cumulative Ownership Range:",
                        min = if ("CumulativeOwnership" %in% names(rv$dk_optimal_lineups))
                          floor(
                            min(rv$dk_optimal_lineups$CumulativeOwnership, na.rm = TRUE)
                          )
                        else
                          0,
                        max = if ("CumulativeOwnership" %in% names(rv$dk_optimal_lineups))
                          ceiling(
                            max(rv$dk_optimal_lineups$CumulativeOwnership, na.rm = TRUE)
                          )
                        else
                          600,
                        value = if ("CumulativeOwnership" %in% names(rv$dk_optimal_lineups))
                          c(floor(
                            min(rv$dk_optimal_lineups$CumulativeOwnership, na.rm = TRUE)
                          ), ceiling(
                            max(rv$dk_optimal_lineups$CumulativeOwnership, na.rm = TRUE)
                          ))
                        else
                          c(0, 600),
                        step = 5,
                        width = "100%"
                      )
                    ), column(
                      6,
                      sliderInput(
                        "dk_geometric_range",
                        "Geometric Mean Ownership Range:",
                        min = if ("GeometricMean" %in% names(rv$dk_optimal_lineups))
                          floor(
                            min(rv$dk_optimal_lineups$GeometricMean, na.rm = TRUE) * 10
                          ) / 10
                        else
                          0,
                        max = if ("GeometricMean" %in% names(rv$dk_optimal_lineups))
                          ceiling(
                            max(rv$dk_optimal_lineups$GeometricMean, na.rm = TRUE) * 10
                          ) / 10
                        else
                          100,
                        value = if ("GeometricMean" %in% names(rv$dk_optimal_lineups))
                          c(floor(
                            min(rv$dk_optimal_lineups$GeometricMean, na.rm = TRUE) * 10
                          ) / 10, ceiling(
                            max(rv$dk_optimal_lineups$GeometricMean, na.rm = TRUE) * 10
                          ) / 10)
                        else
                          c(0, 100),
                        step = 0.5,
                        width = "100%"
                      )
                    ))
                  ),
                  
                  # Starting Position Filters
                  div(
                    style = "margin-bottom: 15px;",
                    h4("Starting Position Filters", style = "margin-bottom: 10px; color: #333;"),
                    fluidRow(column(
                      6,
                      sliderInput(
                        "dk_starting_range",
                        "Cumulative Starting Position Range:",
                        min = if ("CumulativeStarting" %in% names(rv$dk_optimal_lineups))
                          floor(
                            min(rv$dk_optimal_lineups$CumulativeStarting, na.rm = TRUE)
                          )
                        else
                          6,
                        max = if ("CumulativeStarting" %in% names(rv$dk_optimal_lineups))
                          ceiling(
                            max(rv$dk_optimal_lineups$CumulativeStarting, na.rm = TRUE)
                          )
                        else
                          240,
                        value = if ("CumulativeStarting" %in% names(rv$dk_optimal_lineups))
                          c(floor(
                            min(rv$dk_optimal_lineups$CumulativeStarting, na.rm = TRUE)
                          ), ceiling(
                            max(rv$dk_optimal_lineups$CumulativeStarting, na.rm = TRUE)
                          ))
                        else
                          c(6, 240),
                        step = 1,
                        width = "100%"
                      )
                    ), column(
                      6,
                      sliderInput(
                        "dk_starting_geo_range",
                        "Geometric Mean Starting Position Range:",
                        min = if ("GeometricMeanStarting" %in% names(rv$dk_optimal_lineups))
                          floor(
                            min(rv$dk_optimal_lineups$GeometricMeanStarting, na.rm = TRUE) * 10
                          ) / 10
                        else
                          1,
                        max = if ("GeometricMeanStarting" %in% names(rv$dk_optimal_lineups))
                          ceiling(
                            max(rv$dk_optimal_lineups$GeometricMeanStarting, na.rm = TRUE) * 10
                          ) / 10
                        else
                          40,
                        value = if ("GeometricMeanStarting" %in% names(rv$dk_optimal_lineups))
                          c(floor(
                            min(rv$dk_optimal_lineups$GeometricMeanStarting, na.rm = TRUE) * 10
                          ) / 10, ceiling(
                            max(rv$dk_optimal_lineups$GeometricMeanStarting, na.rm = TRUE) * 10
                          ) / 10)
                        else
                          c(1, 40),
                        step = 0.1,
                        width = "100%"
                      )
                    ))
                  )
                ),
                
                # Right Column - Salary + Controls
                column(
                  4,
                  # Salary Filter
                  div(
                    style = "margin-bottom: 20px;",
                    h4("Salary Filter", style = "margin-bottom: 10px; color: #333;"),
                    sliderInput(
                      "dk_salary_range",
                      "Total Salary Range (k):",
                      min = if ("TotalSalary" %in% names(rv$dk_optimal_lineups))
                        floor(
                          min(rv$dk_optimal_lineups$TotalSalary, na.rm = TRUE) / 1000 * 10
                        ) / 10
                      else
                        42.5,
                      max = if ("TotalSalary" %in% names(rv$dk_optimal_lineups))
                        ceiling(
                          max(rv$dk_optimal_lineups$TotalSalary, na.rm = TRUE) / 1000 * 10
                        ) / 10
                      else
                        50,
                      value = if ("TotalSalary" %in% names(rv$dk_optimal_lineups))
                        c(floor(
                          min(rv$dk_optimal_lineups$TotalSalary, na.rm = TRUE) / 1000 * 10
                        ) / 10, ceiling(
                          max(rv$dk_optimal_lineups$TotalSalary, na.rm = TRUE) / 1000 * 10
                        ) / 10)
                      else
                        c(42.5, 50),
                      step = 0.1,
                      width = "100%"
                    )
                  ),
                  
                  # Generation Controls
                  selectizeInput(
                    "dk_excluded_drivers",
                    "Exclude Drivers:",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(
                      plugins = list('remove_button'),
                      placeholder = 'Select drivers to exclude'
                    ),
                    width = "100%"
                  ),
                  
                  numericInput(
                    "dk_num_random_lineups",
                    "Number of Lineups:",
                    value = 20,
                    min = 1,
                    max = 150,
                    width = "100%"
                  ),
                  
                  div(
                    class = "well well-sm",
                    style = "padding: 15px; margin-bottom: 15px;",
                    h6("Filtered Pool:", style = "margin-bottom: 5px; font-weight: bold;"),
                    textOutput("dk_filtered_pool_size")
                  ),
                  
                  div(
                    style = "display: flex; flex-direction: column; gap: 10px;",
                    actionButton(
                      "generate_dk_lineups",
                      "Randomly Select Lineups",
                      class = "btn-primary",
                      style = "width: 100%; height: 40px; font-size: 16px;"
                    ),
                    downloadButton(
                      "download_dk_random_lineups",
                      "Download Lineups",
                      style = "width: 100%; height: 35px; font-size: 14px;"
                    )
                  )
                )
              )
            )
          ),
          
          # Results tables
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Driver Exposure Analysis",
              DTOutput("dk_driver_exposure_table") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "Generated DraftKings Lineups",
              DTOutput("dk_random_lineups_table") %>% withSpinner(color = "#FFD700")
            )
          )
        )
      ))
    }
    
    if (!is.null(rv$fd_optimal_lineups)) {
      tab_panels <- append(tab_panels, list(
        tabPanel(
          "FanDuel",
          value = "fd_tab",
          
          # FanDuel content
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Lineup Filters & Generation",
              
              fluidRow(
                column(
                  8,
                  # Performance Filters
                  div(
                    style = "margin-bottom: 15px;",
                    h4("Performance Filters", style = "margin-bottom: 10px; color: #333;"),
                    fluidRow(
                      column(
                        4,
                        sliderInput(
                          "fd_min_top1_slider",
                          "Top 1 Count Range:",
                          min = if ("Top1Pct" %in% names(rv$fd_optimal_lineups))
                            min(rv$fd_optimal_lineups$Top1Pct, na.rm = TRUE)
                          else
                            0,
                          max = if ("Top1Pct" %in% names(rv$fd_optimal_lineups))
                            max(rv$fd_optimal_lineups$Top1Pct, na.rm = TRUE)
                          else
                            100,
                          value = if ("Top1Pct" %in% names(rv$fd_optimal_lineups))
                            c(
                              min(rv$fd_optimal_lineups$Top1Pct, na.rm = TRUE),
                              max(rv$fd_optimal_lineups$Top1Pct, na.rm = TRUE)
                            )
                          else
                            c(0, 100),
                          step = 1,
                          width = "100%"
                        )
                      ),
                      column(
                        4,
                        sliderInput(
                          "fd_min_top3_slider",
                          "Top 3 Count Range:",
                          min = if ("Top5Pct" %in% names(rv$fd_optimal_lineups))
                            min(rv$fd_optimal_lineups$Top5Pct, na.rm = TRUE)
                          else
                            0,
                          max = if ("Top5Pct" %in% names(rv$fd_optimal_lineups))
                            max(rv$fd_optimal_lineups$Top5Pct, na.rm = TRUE)
                          else
                            100,
                          value = if ("Top5Pct" %in% names(rv$fd_optimal_lineups))
                            c(
                              min(rv$fd_optimal_lineups$Top5Pct, na.rm = TRUE),
                              max(rv$fd_optimal_lineups$Top5Pct, na.rm = TRUE)
                            )
                          else
                            c(0, 100),
                          step = 1,
                          width = "100%"
                        )
                      ),
                      column(
                        4,
                        sliderInput(
                          "fd_min_top5_slider",
                          "Top 5 Count Range:",
                          min = if ("Top10Pct" %in% names(rv$fd_optimal_lineups))
                            min(rv$fd_optimal_lineups$Top10Pct, na.rm = TRUE)
                          else
                            0,
                          max = if ("Top10Pct" %in% names(rv$fd_optimal_lineups))
                            max(rv$fd_optimal_lineups$Top10Pct, na.rm = TRUE)
                          else
                            100,
                          value = if ("Top10Pct" %in% names(rv$fd_optimal_lineups))
                            c(
                              min(rv$fd_optimal_lineups$Top10Pct, na.rm = TRUE),
                              max(rv$fd_optimal_lineups$Top10Pct, na.rm = TRUE)
                            )
                          else
                            c(0, 100),
                          step = 1,
                          width = "100%"
                        )
                      )
                    )
                  ),
                  
                  # Ownership Filters
                  div(
                    style = "margin-bottom: 15px;",
                    h4("Ownership Filters", style = "margin-bottom: 10px; color: #333;"),
                    fluidRow(column(
                      6,
                      sliderInput(
                        "fd_ownership_range",
                        "Cumulative Ownership Range:",
                        min = if ("CumulativeOwnership" %in% names(rv$fd_optimal_lineups))
                          floor(
                            min(rv$fd_optimal_lineups$CumulativeOwnership, na.rm = TRUE)
                          )
                        else
                          0,
                        max = if ("CumulativeOwnership" %in% names(rv$fd_optimal_lineups))
                          ceiling(
                            max(rv$fd_optimal_lineups$CumulativeOwnership, na.rm = TRUE)
                          )
                        else
                          500,
                        value = if ("CumulativeOwnership" %in% names(rv$fd_optimal_lineups))
                          c(floor(
                            min(rv$fd_optimal_lineups$CumulativeOwnership, na.rm = TRUE)
                          ), ceiling(
                            max(rv$fd_optimal_lineups$CumulativeOwnership, na.rm = TRUE)
                          ))
                        else
                          c(0, 500),
                        step = 5,
                        width = "100%"
                      )
                    ), column(
                      6,
                      sliderInput(
                        "fd_geometric_range",
                        "Geometric Mean Ownership Range:",
                        min = if ("GeometricMean" %in% names(rv$fd_optimal_lineups))
                          floor(
                            min(rv$fd_optimal_lineups$GeometricMean, na.rm = TRUE) * 10
                          ) / 10
                        else
                          0,
                        max = if ("GeometricMean" %in% names(rv$fd_optimal_lineups))
                          ceiling(
                            max(rv$fd_optimal_lineups$GeometricMean, na.rm = TRUE) * 10
                          ) / 10
                        else
                          100,
                        value = if ("GeometricMean" %in% names(rv$fd_optimal_lineups))
                          c(floor(
                            min(rv$fd_optimal_lineups$GeometricMean, na.rm = TRUE) * 10
                          ) / 10, ceiling(
                            max(rv$fd_optimal_lineups$GeometricMean, na.rm = TRUE) * 10
                          ) / 10)
                        else
                          c(0, 100),
                        step = 0.5,
                        width = "100%"
                      )
                    ))
                  ),
                  
                  # Starting Position Filters
                  div(
                    style = "margin-bottom: 15px;",
                    h4("Starting Position Filters", style = "margin-bottom: 10px; color: #333;"),
                    fluidRow(column(
                      6,
                      sliderInput(
                        "fd_starting_range",
                        "Cumulative Starting Position Range:",
                        min = if ("CumulativeStarting" %in% names(rv$fd_optimal_lineups))
                          floor(
                            min(rv$fd_optimal_lineups$CumulativeStarting, na.rm = TRUE)
                          )
                        else
                          5,
                        max = if ("CumulativeStarting" %in% names(rv$fd_optimal_lineups))
                          ceiling(
                            max(rv$fd_optimal_lineups$CumulativeStarting, na.rm = TRUE)
                          )
                        else
                          200,
                        value = if ("CumulativeStarting" %in% names(rv$fd_optimal_lineups))
                          c(floor(
                            min(rv$fd_optimal_lineups$CumulativeStarting, na.rm = TRUE)
                          ), ceiling(
                            max(rv$fd_optimal_lineups$CumulativeStarting, na.rm = TRUE)
                          ))
                        else
                          c(5, 200),
                        step = 1,
                        width = "100%"
                      )
                    ), column(
                      6,
                      sliderInput(
                        "fd_starting_geo_range",
                        "Geometric Mean Starting Position Range:",
                        min = if ("GeometricMeanStarting" %in% names(rv$fd_optimal_lineups))
                          floor(
                            min(rv$fd_optimal_lineups$GeometricMeanStarting, na.rm = TRUE) * 10
                          ) / 10
                        else
                          1,
                        max = if ("GeometricMeanStarting" %in% names(rv$fd_optimal_lineups))
                          ceiling(
                            max(rv$fd_optimal_lineups$GeometricMeanStarting, na.rm = TRUE) * 10
                          ) / 10
                        else
                          40,
                        value = if ("GeometricMeanStarting" %in% names(rv$fd_optimal_lineups))
                          c(floor(
                            min(rv$fd_optimal_lineups$GeometricMeanStarting, na.rm = TRUE) * 10
                          ) / 10, ceiling(
                            max(rv$fd_optimal_lineups$GeometricMeanStarting, na.rm = TRUE) * 10
                          ) / 10)
                        else
                          c(1, 40),
                        step = 0.1,
                        width = "100%"
                      )
                    ))
                  )
                ),
                
                column(
                  4,
                  # Salary Filter
                  div(
                    style = "margin-bottom: 20px;",
                    h4("Salary Filter", style = "margin-bottom: 10px; color: #333;"),
                    sliderInput(
                      "fd_salary_range",
                      "Total Salary Range (k):",
                      min = if ("TotalSalary" %in% names(rv$fd_optimal_lineups))
                        floor(
                          min(rv$fd_optimal_lineups$TotalSalary, na.rm = TRUE) / 1000 * 10
                        ) / 10
                      else
                        42.5,
                      max = if ("TotalSalary" %in% names(rv$fd_optimal_lineups))
                        ceiling(
                          max(rv$fd_optimal_lineups$TotalSalary, na.rm = TRUE) / 1000 * 10
                        ) / 10
                      else
                        50,
                      value = if ("TotalSalary" %in% names(rv$fd_optimal_lineups))
                        c(floor(
                          min(rv$fd_optimal_lineups$TotalSalary, na.rm = TRUE) / 1000 * 10
                        ) / 10, ceiling(
                          max(rv$fd_optimal_lineups$TotalSalary, na.rm = TRUE) / 1000 * 10
                        ) / 10)
                      else
                        c(42.5, 50),
                      step = 0.1,
                      width = "100%"
                    )
                  ),
                  
                  # Generation Controls
                  selectizeInput(
                    "fd_excluded_drivers",
                    "Exclude Drivers:",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(
                      plugins = list('remove_button'),
                      placeholder = 'Select drivers to exclude'
                    ),
                    width = "100%"
                  ),
                  
                  numericInput(
                    "fd_num_random_lineups",
                    "Number of Lineups:",
                    value = 20,
                    min = 1,
                    max = 150,
                    width = "100%"
                  ),
                  
                  div(
                    class = "well well-sm",
                    style = "padding: 15px; margin-bottom: 15px;",
                    h6("Filtered Pool:", style = "margin-bottom: 5px; font-weight: bold;"),
                    textOutput("fd_filtered_pool_size")
                  ),
                  
                  div(
                    style = "display: flex; flex-direction: column; gap: 10px;",
                    actionButton(
                      "generate_fd_lineups",
                      "Randomly Select Lineups",
                      class = "btn-primary",
                      style = "width: 100%; height: 40px; font-size: 16px;"
                    ),
                    downloadButton(
                      "download_fd_random_lineups",
                      "Download Lineups",
                      style = "width: 100%; height: 35px; font-size: 14px;"
                    )
                  )
                )
              )
            )
          ),
          
          # Results tables
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Driver Exposure Analysis",
              DTOutput("fd_driver_exposure_table") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "Generated FanDuel Lineups",
              DTOutput("fd_random_lineups_table") %>% withSpinner(color = "#FFD700")
            )
          )
        )
      ))
    }
    
    # Return tabsetPanel with all available tabs
    do.call(tabsetPanel, c(list(id = "lineup_platform_tabs"), tab_panels))
  })
  
  
  
  # Create position boxplot
  output$position_box <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    n_drivers <- length(unique(rv$simulation_results$Name))
    n_sims <- n_total_results / n_drivers
    
    cat("Creating position boxplot:",
        n_drivers,
        "drivers,",
        n_sims,
        "simulations\n")
    
    # Sample data if too large for responsive plotting
    plot_data <- rv$simulation_results
    max_plot_points <- 50000  # Maximum points for responsive plotting
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat(
        "Sampling",
        sample_size,
        "results for plotting (",
        round(sample_size / n_total_results * 100, 1),
        "%)\n"
      )
    }
    
    # Get all unique drivers and starting positions
    drivers_info <- unique(plot_data[, c("Name", "Starting")])
    
    # Order by starting position
    drivers_info <- drivers_info[order(drivers_info$Starting), ]
    
    # Get ordered list of driver names
    ordered_drivers <- drivers_info$Name
    
    # Plot with all drivers, ordered by starting position
    p <- ggplot(plot_data, aes(
      x = factor(Name, levels = ordered_drivers),
      y = FinishPosition,
      fill = Name
    )) +
      geom_boxplot(
        alpha = 0.7,
        outlier.color = "red",
        outlier.size = 1,
        outlier.alpha = 0.5
      ) +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        plot.margin = margin(
          t = 10,
          r = 20,
          b = 10,
          l = 20
        ),
        legend.position = "none"  # Hide legend to reduce clutter
      ) +
      labs(x = "Driver",
           y = "Finish Position",
           title = if (n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           })
    
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
        font = list(family = "Arial", size = 12)
      )
  })
  
  # DraftKings Dominator Stats
  output$dk_dominator_stats <- renderDT({
    req(rv$dk_dominator_analysis)
    
    dt <- datatable(
      rv$dk_dominator_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        # Show all rows
        dom = "t",
        # Only show table ('t'), no search/pagination
        order = list(list(3, 'desc')),
        # Sort by Avg_Dom
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency(
        'DKSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      ) %>%
      formatRound(
        c(
          'Avg_Dom',
          'Median_Dom',
          'Max_Dom',
          'Avg_DomRank',
          'Median_DomRank',
          'Top_DomRate',
          'Top3_DomRate',
          'Top5_DomRate',
          'Top10_DomRate'
        ),
        digits = 1
      )
    
    dt
  })
  
  # FanDuel Dominator Stats
  output$fd_dominator_stats <- renderDT({
    req(rv$fd_dominator_analysis)
    
    dt <- datatable(
      rv$fd_dominator_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        # Show all rows
        dom = "t",
        # Only show table ('t'), no search/pagination
        order = list(list(3, 'desc')),
        # Sort by Avg_Dom
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency(
        'FDSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      ) %>%
      formatRound(
        c(
          'Avg_Dom',
          'Median_Dom',
          'Max_Dom',
          'Avg_DomRank',
          'Median_DomRank',
          'Top_DomRate',
          'Top3_DomRate',
          'Top5_DomRate'
        ),
        digits = 1
      )
    
    dt
  })
  
  output$dk_dominator_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Check if we have DK dominator points
    if (!"DKDominatorPoints" %in% names(rv$simulation_results)) {
      return(plotly_empty() %>% layout(title = "No DK dominator data available"))
    }
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating DK dominator distribution plot:",
        n_total_results,
        "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 50000
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for DK dominator plot\n")
    }
    
    # Calculate median DKDominatorPoints for each driver
    driver_medians <- plot_data %>%
      group_by(Name) %>%
      summarize(median_points = median(DKDominatorPoints, na.rm = TRUE),
                .groups = 'drop') %>%
      filter(median_points > 0)
    
    if (nrow(driver_medians) == 0) {
      return(plotly_empty() %>% layout(title = "No drivers with dominator points"))
    }
    
    # Filter original data to only include drivers with median > 0
    plot_data_filtered <- plot_data %>%
      filter(Name %in% driver_medians$Name)
    
    # Create box and whisker plot
    p <- ggplot(plot_data_filtered, aes(
      x = reorder(Name, DKDominatorPoints, median),
      y = DKDominatorPoints
    )) +
      geom_boxplot(
        outlier.shape = NA,
        fill = "lightblue",
        alpha = 0.7
      ) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver",
           y = "Dominator Points",
           title = if (n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           }) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    ggplotly(p, height = 1000)
  })
  
  # Optimized FD dominator distribution plot
  output$fd_dominator_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Check if we have FD dominator points
    if (!"FDDominatorPoints" %in% names(rv$simulation_results)) {
      return(plotly_empty() %>% layout(title = "No FD dominator data available"))
    }
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating FD dominator distribution plot:",
        n_total_results,
        "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 50000
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for FD dominator plot\n")
    }
    
    # Calculate median FDDominatorPoints for each driver
    driver_medians <- plot_data %>%
      group_by(Name) %>%
      summarize(median_points = median(FDDominatorPoints, na.rm = TRUE),
                .groups = 'drop') %>%
      filter(median_points > 0)
    
    if (nrow(driver_medians) == 0) {
      return(plotly_empty() %>% layout(title = "No drivers with dominator points"))
    }
    
    # Filter original data to only include drivers with median > 0
    plot_data_filtered <- plot_data %>%
      filter(Name %in% driver_medians$Name)
    
    # Create box and whisker plot
    p <- ggplot(plot_data_filtered, aes(
      x = reorder(Name, FDDominatorPoints, median),
      y = FDDominatorPoints
    )) +
      geom_boxplot(
        outlier.shape = NA,
        fill = "lightgreen",
        alpha = 0.7
      ) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver",
           y = "Dominator Points",
           title = if (n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           }) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    ggplotly(p, height = 800)
  })
  
  output$dk_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating DK points by position plot:",
        n_total_results,
        "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 25000  # Smaller sample for position plots
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling",
          sample_size,
          "results for DK points by position plot\n")
    }
    
    p <- ggplot(plot_data, aes(x = factor(FinishPosition), y = DKDominatorPoints)) +
      geom_boxplot(
        fill = "lightblue",
        color = "darkblue",
        alpha = 0.7,
        outlier.alpha = 0.3
      ) +
      geom_smooth(
        method = "lm",
        color = "red",
        se = FALSE,
        aes(group = 1)
      ) +
      theme_minimal() +
      labs(x = "Finish Position",
           y = "Dominator Points",
           title = if (n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           })
    
    ggplotly(p, height = 700)
  })
  
  output$fd_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating FD points by position plot:",
        n_total_results,
        "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 25000  # Smaller sample for position plots
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling",
          sample_size,
          "results for FD points by position plot\n")
    }
    
    p <- ggplot(plot_data, aes(x = factor(FinishPosition), y = FDDominatorPoints)) +
      geom_boxplot(
        fill = "lightblue",
        color = "darkblue",
        alpha = 0.7,
        outlier.alpha = 0.3
      ) +
      geom_smooth(
        method = "lm",
        color = "red",
        se = FALSE,
        aes(group = 1)
      ) +
      theme_minimal() +
      labs(x = "Finish Position",
           y = "Dominator Points",
           title = if (n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           })
    
    ggplotly(p, height = 700)
  })
  
  output$fd_lap_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating FD lap points by position plot:",
        n_total_results,
        "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 25000
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for FD lap points plot\n")
    }
    
    p <- ggplot(plot_data, aes(x = factor(FinishPosition), y = FDLapPoints)) +
      geom_boxplot(
        fill = "lightgreen",
        color = "darkgreen",
        alpha = 0.7,
        outlier.alpha = 0.3
      ) +
      theme_minimal() +
      labs(x = "Finish Position",
           y = "Lap Points",
           title = if (n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           })
    
    ggplotly(p)
  })
  
  
  # DraftKings Fantasy Projections
  output$dk_fantasy_projections <- renderDT({
    req(rv$dk_fantasy_analysis)
    
    dt <- datatable(
      rv$dk_fantasy_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        # Show all rows
        dom = "t",
        # Only show table ('t'), no search/pagination
        order = list(list(4, 'desc')),
        # Sort by median
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency(
        'DKSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      ) %>%
      formatRound(c('Median_Fantasy_Pts', 'FP_90thPct', 'PPD'), digits = 1)
    
    dt
  })
  
  # FanDuel Fantasy Projections
  output$fd_fantasy_projections <- renderDT({
    req(rv$fd_fantasy_analysis)
    
    dt <- datatable(
      rv$fd_fantasy_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        # Show all rows
        dom = "t",
        # Only show table ('t'), no search/pagination
        order = list(list(4, 'desc')),
        # Sort by median
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency(
        'FDSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      ) %>%
      formatRound(c('Median_Fantasy_Pts', 'FP_90thPct', 'PPD'), digits = 1)
    
    dt
  })
  
  # DraftKings Fantasy Points vs Salary
  output$dk_fantasy_points_salary <- renderPlotly({
    req(rv$dk_fantasy_analysis)
    
    plot_data <- rv$dk_fantasy_analysis
    
    # Combine Name and Starting into a label
    plot_data$label <- paste0(plot_data$Name, " (Start: ", plot_data$Starting, ")")
    
    p <- ggplot(plot_data,
                aes(
                  x = DKSalary,
                  y = Median_Fantasy_Pts,
                  size = DKOP,
                  text = label
                )) +
      geom_point(alpha = 0.7, color = "#2c7fb8") +
      geom_smooth(
        aes(x = DKSalary, y = Median_Fantasy_Pts),
        method = "lm",
        se = FALSE,
        color = "darkblue"
      ) +
      theme_minimal() +
      labs(x = "DK Salary", y = "Median Fantasy Points", size = "DKOP")
    
    ggplotly(p,
             height = 800,
             tooltip = c("text", "x", "y", "size")) %>%
      layout(hoverlabel = list(bgcolor = "white"),
             hovermode = "closest")
  })
  
  # FanDuel Fantasy Points vs Salary
  output$fd_fantasy_points_salary <- renderPlotly({
    req(rv$fd_fantasy_analysis)
    
    plot_data <- rv$fd_fantasy_analysis
    
    # Combine Name and Starting into a label
    plot_data$label <- paste0(plot_data$Name, " (Start: ", plot_data$Starting, ")")
    
    p <- ggplot(plot_data,
                aes(
                  x = FDSalary,
                  y = Median_Fantasy_Pts,
                  size = FDOP,
                  text = label
                )) +
      geom_point(alpha = 0.7, color = "#2c7fb8") +
      geom_smooth(
        aes(x = FDSalary, y = Median_Fantasy_Pts),
        method = "lm",
        se = FALSE,
        color = "darkblue"
      ) +
      theme_minimal() +
      labs(x = "FD Salary", y = "Median Fantasy Points", size = "FDOP")
    
    ggplotly(p,
             height = 800,
             tooltip = c("text", "x", "y", "size")) %>%
      layout(hoverlabel = list(bgcolor = "white"),
             hovermode = "closest")
  })
  
  output$dk_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating DK fantasy points distribution:",
        n_total_results,
        "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 50000
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling",
          sample_size,
          "results for DK fantasy points plot\n")
    }
    
    # Get unique driver salary info
    driver_salaries <- plot_data %>%
      distinct(Name, DKSalary)
    
    # Order driver names by ascending DKSalary
    ordered_names <- driver_salaries %>%
      arrange(DKSalary) %>%
      pull(Name)
    
    plot_data_filtered <- plot_data %>%
      filter(Name %in% ordered_names)
    
    p <- ggplot(plot_data_filtered,
                aes(
                  x = factor(Name, levels = ordered_names),
                  y = DKFantasyPoints,
                  fill = Name
                )) +
      geom_boxplot(outlier.alpha = 0.25, alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver",
           y = "Fantasy Points",
           title = if (n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           }) +
      theme(legend.position = "none")  # Hide legend to avoid clutter
    
    ggplotly(p, height = 700, tooltip = c("x", "y"))
  })
  
  # Optimized FD fantasy points distribution
  output$fd_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating FD fantasy points distribution:",
        n_total_results,
        "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 50000
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling",
          sample_size,
          "results for FD fantasy points plot\n")
    }
    
    # Get unique driver salary info
    driver_salaries <- plot_data %>%
      distinct(Name, FDSalary)
    
    # Order driver names by ascending FDSalary
    ordered_names <- driver_salaries %>%
      arrange(FDSalary) %>%
      pull(Name)
    
    plot_data_filtered <- plot_data %>%
      filter(Name %in% ordered_names)
    
    p <- ggplot(plot_data_filtered,
                aes(
                  x = factor(Name, levels = ordered_names),
                  y = FDFantasyPoints,
                  fill = Name
                )) +
      geom_boxplot(outlier.alpha = 0.25, alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver",
           y = "Fantasy Points",
           title = if (n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           }) +
      theme(legend.position = "none")  # Hide legend to avoid clutter
    
    ggplotly(p, height = 700, tooltip = c("x", "y"))
  })
  
  
  
  # ==========================================================================
  # OPTIMAL LINEUPS - DRAFTKINGS (CORE-BASED)
  # ==========================================================================
  
  observeEvent(input$run_dk_optimization, {
    req(rv$simulation_results, rv$has_draftkings)
    
    rv$dk_optimal_lineups <- NULL
    rv$dk_random_lineups <- NULL
    gc(verbose = FALSE, full = TRUE)
    
    withProgress(message = 'Calculating DraftKings optimal lineups...', value = 0, {
      showModal(
        modalDialog(
          title = "Processing DraftKings Optimal Lineups",
          "Finding optimal lineups using all simulations. This may take a few minutes.",
          footer = NULL,
          easyClose = FALSE
        )
      )
      
      # Prepare data for Core
      dk_sim_data <- rv$simulation_results %>%
        rename(
          Player = Name,
          FantasyPoints = DKFantasyPoints,
          Salary = DKSalary,
          Own = DKOP
        )
      
      setProgress(0.2, detail = "Phase 1: Finding unique lineups...")
      
      rv$dk_optimal_lineups <- tryCatch({
        # PHASE 1
        phase1_result <- find_optimal_lineups(
          sim_results = dk_sim_data,
          config = nascar_dk_optimal_config,
          k = 3,
          verbose = TRUE
        )
        
        setProgress(0.4, detail = "Phase 2: Scoring all lineups...")
        
        # PHASE 2
        score_matrix <- score_all_lineups(
          lineup_data = phase1_result,
          sim_results = dk_sim_data,
          verbose = TRUE
        )
        
        setProgress(0.6, detail = "Phase 3: Calculating metrics...")
        
        # PHASE 3
        final_result <- calculate_distribution_metrics(
          score_matrix = score_matrix,
          lineup_data = phase1_result,
          config = nascar_dk_optimal_config,
          ownership_data = dk_sim_data %>%
            filter(SimID == 1) %>%
            select(Player, Own, Starting) %>%
            distinct(Player, .keep_all = TRUE),
          verbose = TRUE
        )
        
        final_result
        
      }, error = function(e) {
        message("Error finding optimal lineups: ", e$message)
        removeModal()
        showModal(modalDialog(
          title = "Error Finding Optimal Lineups",
          paste("There was an error:", e$message),
          easyClose = TRUE
        ))
        NULL
      })
      
      gc(verbose = FALSE, full = TRUE)
      removeModal()
      
      if (!is.null(rv$dk_optimal_lineups)) {
        showModal(modalDialog(
          title = "Success",
          HTML(
            sprintf(
              "Successfully generated <b>%d</b> optimal lineups for DraftKings!<br><br>
            Go to the <b>Lineup Builder</b> tab to filter and build your portfolio.",
              nrow(rv$dk_optimal_lineups)
            )
          ),
          easyClose = TRUE
        ))
      }
    })
  })
  
  # ==========================================================================
  # OPTIMAL LINEUPS - FANDUEL (CORE-BASED)
  # ==========================================================================
  
  observeEvent(input$run_fd_optimization, {
    req(rv$simulation_results, rv$has_fanduel)
    
    rv$fd_optimal_lineups <- NULL
    rv$fd_random_lineups <- NULL
    gc(verbose = FALSE, full = TRUE)
    
    withProgress(message = 'Calculating FanDuel optimal lineups...', value = 0, {
      showModal(
        modalDialog(
          title = "Processing FanDuel Optimal Lineups",
          "Finding optimal lineups using all simulations. This may take a few minutes.",
          footer = NULL,
          easyClose = FALSE
        )
      )
      
      # Prepare data for Core
      fd_sim_data <- rv$simulation_results %>%
        rename(
          Player = Name,
          FantasyPoints = FDFantasyPoints,
          Salary = FDSalary,
          Own = FDOP
        )
      
      setProgress(0.2, detail = "Phase 1: Finding unique lineups...")
      
      rv$fd_optimal_lineups <- tryCatch({
        # PHASE 1
        phase1_result <- find_optimal_lineups(
          sim_results = fd_sim_data,
          config = nascar_fd_optimal_config,
          k = 3,
          verbose = TRUE
        )
        
        setProgress(0.4, detail = "Phase 2: Scoring all lineups...")
        
        # PHASE 2
        score_matrix <- score_all_lineups(
          lineup_data = phase1_result,
          sim_results = fd_sim_data,
          verbose = TRUE
        )
        
        setProgress(0.6, detail = "Phase 3: Calculating metrics...")
        
        # PHASE 3
        final_result <- calculate_distribution_metrics(
          score_matrix = score_matrix,
          lineup_data = phase1_result,
          config = nascar_fd_optimal_config,
          ownership_data = fd_sim_data %>%
            filter(SimID == 1) %>%
            select(Player, Own, Starting) %>%
            distinct(Player, .keep_all = TRUE),
          verbose = TRUE
        )
        
        final_result
        
      }, error = function(e) {
        message("Error finding optimal lineups: ", e$message)
        removeModal()
        showModal(modalDialog(
          title = "Error Finding Optimal Lineups",
          paste("There was an error:", e$message),
          easyClose = TRUE
        ))
        NULL
      })
      
      gc(verbose = FALSE, full = TRUE)
      removeModal()
      
      if (!is.null(rv$fd_optimal_lineups)) {
        showModal(modalDialog(
          title = "Success",
          HTML(
            sprintf(
              "Successfully generated <b>%d</b> optimal lineups for FanDuel!<br><br>
            Go to the <b>Lineup Builder</b> tab to filter and build your portfolio.",
              nrow(rv$fd_optimal_lineups)
            )
          ),
          easyClose = TRUE
        ))
      }
    })
  })
  
  
  
  
  observeEvent(input$run_both_optimization, {
    req(rv$simulation_results, rv$has_draftkings, rv$has_fanduel)
    
    showModal(
      modalDialog(
        title = "Scoring Both Platforms",
        "Running DraftKings and FanDuel scoring...",
        footer = NULL,
        easyClose = FALSE
      )
    )
    
    dk_sim_data <- rv$simulation_results %>%
      rename(
        Player = Name,
        FantasyPoints = DKFantasyPoints,
        Salary = DKSalary,
        Own = DKOP
      )
    
    fd_sim_data <- rv$simulation_results %>%
      rename(
        Player = Name,
        FantasyPoints = FDFantasyPoints,
        Salary = FDSalary,
        Own = FDOP
      )
    
    rv$dk_optimal_lineups <- tryCatch({
      phase1_result <- find_optimal_lineups(dk_sim_data,
                                            nascar_dk_optimal_config,
                                            k = 2,
                                            verbose = FALSE)
      score_matrix <- score_all_lineups(phase1_result, dk_sim_data, verbose = FALSE)
      calculate_distribution_metrics(
        score_matrix,
        phase1_result,
        nascar_dk_optimal_config,
        ownership_data = dk_sim_data %>% select(Player, Own, Starting) %>% distinct(),
        verbose = FALSE
      )
    }, error = function(e)
      NULL)
    
    rv$fd_optimal_lineups <- tryCatch({
      phase1_result <- find_optimal_lineups(fd_sim_data,
                                            nascar_fd_optimal_config,
                                            k = 2,
                                            verbose = FALSE)
      score_matrix <- score_all_lineups(phase1_result, fd_sim_data, verbose = FALSE)
      calculate_distribution_metrics(
        score_matrix,
        phase1_result,
        nascar_fd_optimal_config,
        ownership_data = fd_sim_data %>% select(Player, Own, Starting) %>% distinct(),
        verbose = FALSE
      )
    }, error = function(e)
      NULL)
    
    gc(verbose = FALSE, full = TRUE)
    removeModal()
    
    dk_count <- if (!is.null(rv$dk_optimal_lineups))
      nrow(rv$dk_optimal_lineups)
    else
      0
    fd_count <- if (!is.null(rv$fd_optimal_lineups))
      nrow(rv$fd_optimal_lineups)
    else
      0
    
    showModal(modalDialog(title = "Success", HTML(
      sprintf(
        "DraftKings: <b>%s</b> lineups<br>FanDuel: <b>%s</b> lineups",
        format(dk_count, big.mark = ","),
        format(fd_count, big.mark = ",")
      )
    ), easyClose = TRUE))
  })
  
  
  output$optimal_lineups_tabs_ui <- renderUI({
    has_dk <- !is.null(rv$dk_optimal_lineups) &&
      nrow(rv$dk_optimal_lineups) > 0
    has_fd <- !is.null(rv$fd_optimal_lineups) &&
      nrow(rv$fd_optimal_lineups) > 0
    
    if (!has_dk && !has_fd) {
      return(div(
        style = "text-align: center; padding: 50px;",
        h4("Run scoring to generate lineups", style = "color: #FFD700;")
      ))
    }
    
    tabs <- list()
    
    if (has_dk) {
      tabs[[length(tabs) + 1]] <- tabPanel("DraftKings", br(), fluidRow(column(
        width = 12,
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px; padding: 0 15px;",
          h3(paste0(
            "DraftKings (", format(nrow(rv$dk_optimal_lineups), big.mark = ","), ")"
          ), style = "margin: 0; color: #000; font-weight: bold;"),
          downloadButton('download_dk_optimal', 'Download CSV', style = "background-color: #FFD700; color: #000; border: none; font-weight: bold;")
        ),
        div(style = "padding: 0 15px;", withSpinner(
          DTOutput("dk_optimal_lineups_table"), color = "#FFD700"
        ))
      )))
    }
    
    if (has_fd) {
      tabs[[length(tabs) + 1]] <- tabPanel("FanDuel", br(), fluidRow(column(
        width = 12,
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px; padding: 0 15px;",
          h3(paste0(
            "FanDuel (", format(nrow(rv$fd_optimal_lineups), big.mark = ","), ")"
          ), style = "margin: 0; color: #000; font-weight: bold;"),
          downloadButton('download_fd_optimal', 'Download CSV', style = "background-color: #FFD700; color: #000; border: none; font-weight: bold;")
        ),
        div(style = "padding: 0 15px;", withSpinner(
          DTOutput("fd_optimal_lineups_table"), color = "#FFD700"
        ))
      )))
    }
    
    do.call(tabsetPanel, c(list(id = "optimal_results_tabs"), tabs))
  })
  
  
  output$download_full_sims <- downloadHandler(
    filename = function() {
      paste0("nascar_simulation_results_",
             format(Sys.time(), "%Y%m%d_%H%M%S"),
             ".csv")
    },
    content = function(file) {
      req(rv$simulation_results)
      write.csv(rv$simulation_results, file, row.names = FALSE)
    }
  )
  
  output$dk_optimal_lineups_table <- renderDT({
    req(rv$dk_optimal_lineups)
    
    df <- copy(rv$dk_optimal_lineups)
    setDT(df)
    
    # Multiply ownership by 100 for display if they're fractions
    if ("CumulativeOwnership" %in% names(df)) {
      if (mean(df$CumulativeOwnership, na.rm = TRUE) < 5) {
        df$CumulativeOwnership <- df$CumulativeOwnership * 100
      }
    }
    if ("GeometricMeanOwnership" %in% names(df)) {
      if (mean(df$GeometricMeanOwnership, na.rm = TRUE) < 5) {
        df$GeometricMeanOwnership <- df$GeometricMeanOwnership * 100
      }
    }
    
    # Select columns for display (no Pct10, Pct25, etc.)
    display_cols <- c(
      paste0("Player", 1:6),
      "WinRate",
      "Top1Pct",
      "Top5Pct",
      "Top10Pct",
      "Top20Pct",
      "TotalSalary",
      "CumulativeOwnership",
      "GeometricMeanOwnership",
      "CumulativeStarting",
      "GeometricMeanStarting"
    )
    
    df_display <- df[, intersect(display_cols, names(df)), with = FALSE]
    
    # Rename columns BEFORE creating datatable
    setnames(
      df_display,
      old = c(
        "CumulativeOwnership",
        "GeometricMeanOwnership",
        "CumulativeStarting",
        "GeometricMeanStarting"
      ),
      new = c("Total Own%", "Avg Own%", "Total Start", "Avg Start"),
      skip_absent = TRUE
    )
    
    datatable(
      df_display,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"),
        digits = 2
      ) %>%
      formatRound(columns = c("Total Own%", "Avg Own%", "Avg Start"),
                  digits = 1) %>%
      formatRound(columns = c("Total Start"), digits = 0)
  }, server = TRUE)  # <-- CHANGED TO TRUE FOR SERVER-SIDE PROCESSING
  
  
  output$fd_optimal_lineups_table <- renderDT({
    req(rv$fd_optimal_lineups)
    
    df <- copy(rv$fd_optimal_lineups)
    setDT(df)
    
    # Multiply ownership by 100 for display if they're fractions
    if ("CumulativeOwnership" %in% names(df)) {
      if (mean(df$CumulativeOwnership, na.rm = TRUE) < 5) {
        df$CumulativeOwnership <- df$CumulativeOwnership * 100
      }
    }
    if ("GeometricMeanOwnership" %in% names(df)) {
      if (mean(df$GeometricMeanOwnership, na.rm = TRUE) < 5) {
        df$GeometricMeanOwnership <- df$GeometricMeanOwnership * 100
      }
    }
    
    # Select columns for display (no Pct10, Pct25, etc.)
    display_cols <- c(
      paste0("Player", 1:5),
      "WinRate",
      "Top1Pct",
      "Top5Pct",
      "Top10Pct",
      "Top20Pct",
      "TotalSalary",
      "CumulativeOwnership",
      "GeometricMeanOwnership",
      "CumulativeStarting",
      "GeometricMeanStarting"
    )
    
    df_display <- df[, intersect(display_cols, names(df)), with = FALSE]
    
    # Rename columns BEFORE creating datatable
    setnames(
      df_display,
      old = c(
        "CumulativeOwnership",
        "GeometricMeanOwnership",
        "CumulativeStarting",
        "GeometricMeanStarting"
      ),
      new = c("Total Own%", "Avg Own%", "Total Start", "Avg Start"),
      skip_absent = TRUE
    )
    
    datatable(
      df_display,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"),
        digits = 2
      ) %>%
      formatRound(columns = c("Total Own%", "Avg Own%", "Avg Start"),
                  digits = 1) %>%
      formatRound(columns = c("Total Start"), digits = 0)
  }, server = TRUE)
  
  
  output$has_dk_lineups <- reactive({
    !is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0
  })
  outputOptions(output, "has_dk_lineups", suspendWhenHidden = FALSE)
  
  output$has_fd_lineups <- reactive({
    !is.null(rv$fd_optimal_lineups) && nrow(rv$fd_optimal_lineups) > 0
  })
  outputOptions(output, "has_fd_lineups", suspendWhenHidden = FALSE)
  
  # ==========================================================================
  # LINEUP BUILDER - DRAFTKINGS (CORE-BASED)
  # ==========================================================================
  
  observeEvent(input$generate_dk_lineups, {
    req(rv$dk_optimal_lineups)
    
    withProgress(message = 'Building lineups...', value = 0, {
      lineup_result <- tryCatch({
        build_lineups(
          optimal_lineups = rv$dk_optimal_lineups,
          num_lineups = input$dk_num_random_lineups,
          config = nascar_dk_builder_config,
          max_exposure = input$dk_max_exposure,
          excluded_players = input$dk_excluded_drivers,
          filters = list(
            Top1Pct = c(
              if (is.null(input$dk_min_top1_slider))
                0
              else
                input$dk_min_top1_slider[1],
              if (is.null(input$dk_min_top1_slider))
                100
              else
                input$dk_min_top1_slider[2]
            ),
            Top10Pct = c(
              if (is.null(input$dk_min_top5_slider))
                0
              else
                input$dk_min_top5_slider[1],
              if (is.null(input$dk_min_top5_slider))
                100
              else
                input$dk_min_top5_slider[2]
            ),
            CumulativeOwnership = c(
              if (is.null(input$dk_ownership_range))
                0
              else
                input$dk_ownership_range[1],
              if (is.null(input$dk_ownership_range))
                600
              else
                input$dk_ownership_range[2]
            ),
            GeometricMeanOwnership = c(
              if (is.null(input$dk_geometric_range))
                0
              else
                input$dk_geometric_range[1],
              if (is.null(input$dk_geometric_range))
                100
              else
                input$dk_geometric_range[2]
            ),
            CumulativeStarting = c(
              if (is.null(input$dk_starting_range))
                6
              else
                input$dk_starting_range[1],
              if (is.null(input$dk_starting_range))
                240
              else
                input$dk_starting_range[2]
            ),
            GeometricMeanStarting = c(
              if (is.null(input$dk_starting_geo_range))
                1
              else
                input$dk_starting_geo_range[1],
              if (is.null(input$dk_starting_geo_range))
                40
              else
                input$dk_starting_geo_range[2]
            ),
            TotalSalary = c(
              if (is.null(input$dk_salary_range))
                42500
              else
                input$dk_salary_range[1] * 1000,
              if (is.null(input$dk_salary_range))
                50000
              else
                input$dk_salary_range[2] * 1000
            )
          )
        )
      }, error = function(e) {
        message("Error building lineups: ", e$message)
        NULL
      })
      
      if (!is.null(lineup_result)) {
        rv$dk_random_lineups <- lineup_result$lineups
        rv$dk_driver_exposure <- lineup_result$exposure
        
        showModal(modalDialog(
          title = "Success",
          sprintf(
            "Generated %d DraftKings lineups successfully!",
            nrow(rv$dk_random_lineups)
          ),
          easyClose = TRUE
        ))
      } else {
        showModal(
          modalDialog(
            title = "Error",
            "No lineups match the selected filters. Try adjusting your criteria.",
            easyClose = TRUE
          )
        )
      }
    })
  })
  
  # ==========================================================================
  # LINEUP BUILDER - FANDUEL (CORE-BASED)
  # ==========================================================================
  
  observeEvent(input$generate_fd_lineups, {
    req(rv$fd_optimal_lineups)
    
    withProgress(message = 'Building lineups...', value = 0, {
      lineup_result <- tryCatch({
        build_lineups(
          optimal_lineups = rv$fd_optimal_lineups,
          num_lineups = input$fd_num_random_lineups,
          config = nascar_fd_builder_config,
          max_exposure = input$fd_max_exposure,
          excluded_players = input$fd_excluded_drivers,
          filters = list(
            Top1Pct = c(
              if (is.null(input$fd_min_top1_slider))
                0
              else
                input$fd_min_top1_slider[1],
              if (is.null(input$fd_min_top1_slider))
                100
              else
                input$fd_min_top1_slider[2]
            ),
            Top10Pct = c(
              if (is.null(input$fd_min_top5_slider))
                0
              else
                input$fd_min_top5_slider[1],
              if (is.null(input$fd_min_top5_slider))
                100
              else
                input$fd_min_top5_slider[2]
            ),
            CumulativeOwnership = c(
              if (is.null(input$fd_ownership_range))
                0
              else
                input$fd_ownership_range[1],
              if (is.null(input$fd_ownership_range))
                500
              else
                input$fd_ownership_range[2]
            ),
            GeometricMeanOwnership = c(
              if (is.null(input$fd_geometric_range))
                0
              else
                input$fd_geometric_range[1],
              if (is.null(input$fd_geometric_range))
                100
              else
                input$fd_geometric_range[2]
            ),
            CumulativeStarting = c(
              if (is.null(input$fd_starting_range))
                5
              else
                input$fd_starting_range[1],
              if (is.null(input$fd_starting_range))
                200
              else
                input$fd_starting_range[2]
            ),
            GeometricMeanStarting = c(
              if (is.null(input$fd_starting_geo_range))
                1
              else
                input$fd_starting_geo_range[1],
              if (is.null(input$fd_starting_geo_range))
                40
              else
                input$fd_starting_geo_range[2]
            ),
            TotalSalary = c(
              if (is.null(input$fd_salary_range))
                42500
              else
                input$fd_salary_range[1] * 1000,
              if (is.null(input$fd_salary_range))
                50000
              else
                input$fd_salary_range[2] * 1000
            )
          )
        )
      }, error = function(e) {
        message("Error building lineups: ", e$message)
        NULL
      })
      
      if (!is.null(lineup_result)) {
        rv$fd_random_lineups <- lineup_result$lineups
        rv$fd_driver_exposure <- lineup_result$exposure
        
        showModal(modalDialog(
          title = "Success",
          sprintf(
            "Generated %d FanDuel lineups successfully!",
            nrow(rv$fd_random_lineups)
          ),
          easyClose = TRUE
        ))
      } else {
        showModal(
          modalDialog(
            title = "Error",
            "No lineups match the selected filters. Try adjusting your criteria.",
            easyClose = TRUE
          )
        )
      }
    })
  })
  
  
  # ==========================================================================
  # LINEUP BUILDER - DRIVER DETAILS
  # ==========================================================================
  
  driver_details_dk <- reactive({
    req(rv$processed_data)
    
    driver_data <- rv$processed_data$drivers
    
    driver_details <- driver_data %>%
      select(
        Player = Name,
        Salary = DKSalary,
        Own = DKOP,
        Starting = Starting
      ) %>%
      mutate(
        Salary = as.numeric(Salary),
        Own = as.numeric(Own),
        Starting = as.numeric(Starting)
      )
    
    if ("Car" %in% names(driver_data)) {
      driver_details$Car <- driver_data$Car
    }
    if ("Team" %in% names(driver_data)) {
      driver_details$Team <- driver_data$Team
    }
    
    return(driver_details)
  })
  
  driver_details_fd <- reactive({
    req(rv$processed_data)
    
    driver_data <- rv$processed_data$drivers
    
    driver_details <- driver_data %>%
      select(
        Player = Name,
        Salary = FDSalary,
        Own = FDOP,
        Starting = Starting
      ) %>%
      mutate(
        Salary = as.numeric(Salary),
        Own = as.numeric(Own),
        Starting = as.numeric(Starting)
      )
    
    if ("Car" %in% names(driver_data)) {
      driver_details$Car <- driver_data$Car
    }
    if ("Team" %in% names(driver_data)) {
      driver_details$Team <- driver_data$Team
    }
    
    return(driver_details)
  })
  
  # ==========================================================================
  # LINEUP BUILDER - PREPARE OPTIMAL LINEUPS
  # ==========================================================================
  
  dk_optimal_for_builder <- reactive({
    req(rv$dk_optimal_lineups)
    
    df <- copy(rv$dk_optimal_lineups)
    setDT(df)
    
    # Strip DK IDs from player columns
    player_cols <- paste0("Player", 1:6)
    for (col in player_cols) {
      if (col %in% names(df)) {
        df[, (col) := gsub(" \\([0-9]+\\)$", "", get(col))]
      }
    }
    
    # Ensure ownership columns are percentages
    if ("TotalOwn%" %in% names(df)) {
      if (mean(df$`TotalOwn%`, na.rm = TRUE) < 5) {
        df$`TotalOwn%` <- df$`TotalOwn%` * 100
      }
    }
    
    if ("AvgOwn%" %in% names(df)) {
      if (mean(df$`AvgOwn%`, na.rm = TRUE) < 5) {
        df$`AvgOwn%` <- df$`AvgOwn%` * 100
      }
    }
    
    return(df)
  })
  
  fd_optimal_for_builder <- reactive({
    req(rv$fd_optimal_lineups)
    
    df <- copy(rv$fd_optimal_lineups)
    setDT(df)
    
    # Strip DK IDs from player columns (if present)
    player_cols <- paste0("Player", 1:5)
    for (col in player_cols) {
      if (col %in% names(df)) {
        df[, (col) := gsub(" \\([0-9]+\\)$", "", get(col))]
      }
    }
    
    # Ensure ownership columns are percentages
    if ("TotalOwn%" %in% names(df)) {
      if (mean(df$`TotalOwn%`, na.rm = TRUE) < 5) {
        df$`TotalOwn%` <- df$`TotalOwn%` * 100
      }
    }
    
    if ("AvgOwn%" %in% names(df)) {
      if (mean(df$`AvgOwn%`, na.rm = TRUE) < 5) {
        df$`AvgOwn%` <- df$`AvgOwn%` * 100
      }
    }
    
    return(df)
  })
  
  # ==========================================================================
  # LINEUP BUILDER - UI RENDERING
  # ==========================================================================
  
  output$lineup_builder_tabs_ui <- renderUI({
    
    has_dk <- !is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0
    has_fd <- !is.null(rv$fd_optimal_lineups) && nrow(rv$fd_optimal_lineups) > 0
    
    if (!has_dk && !has_fd) {
      return(div(
        style = "text-align: center; padding: 50px;",
        h4("Run optimal lineup generation first to use the lineup builder", 
           style = "color: #FFD700; font-weight: bold;"),
        p("Go to the 'All Lineups' tab and click 'Score DK' or 'Score FD' to generate lineups.")
      ))
    }
    
    tabs <- list()
    
    if (has_dk) {
      tabs[[length(tabs) + 1]] <- tabPanel(
        "DraftKings",
        lineupBuilderUI("dk_builder")
      )
    }
    
    if (has_fd) {
      tabs[[length(tabs) + 1]] <- tabPanel(
        "FanDuel",
        lineupBuilderUI("fd_builder")
      )
    }
    
    do.call(tabsetPanel, c(list(id = "lineup_builder_platform_tabs"), tabs))
  })
  
  # ==========================================================================
  # LINEUP BUILDER - SERVER MODULES (PASSING SIMULATION_RESULTS)
  # ==========================================================================
  
  # DraftKings Lineup Builder Module
  lineupBuilderServer(
    id = "dk_builder",
    optimal_lineups = dk_optimal_for_builder,
    config = nascar_dk_builder_config,
    driver_details = driver_details_dk,
    simulation_results = reactive({ rv$simulation_results })  # PASS THIS FOR DKNAME MAPPING
  )
  
  # FanDuel Lineup Builder Module
  lineupBuilderServer(
    id = "fd_builder",
    optimal_lineups = fd_optimal_for_builder,
    config = nascar_fd_builder_config,
    driver_details = driver_details_fd,
    simulation_results = reactive({ rv$simulation_results })  # PASS THIS FOR DKNAME MAPPING
  )
  
  # ==========================================================================
  # BUILT LINEUPS TABLES
  # ==========================================================================
  
  output$dk_random_lineups_table <- renderDT({
    req(rv$dk_random_lineups)
    
    display_data <- as.data.frame(rv$dk_random_lineups)
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tp"
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    ) %>%
      formatCurrency('TotalSalary', "$", digits = 0)
  })
  
  output$fd_random_lineups_table <- renderDT({
    req(rv$fd_random_lineups)
    
    display_data <- as.data.frame(rv$fd_random_lineups)
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tp"
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    ) %>%
      formatCurrency('TotalSalary', "$", digits = 0)
  })
  
  # ==========================================================================
  # DOWNLOAD HANDLERS
  # ==========================================================================
  
  output$download_dk_optimal <- downloadHandler(
    filename = function() {
      paste0("nascar_dk_scored_",
             format(Sys.time(), "%Y%m%d_%H%M%S"),
             ".csv")
    },
    content = function(file) {
      req(rv$dk_optimal_lineups)
      req(rv$simulation_results)
      
      df <- copy(rv$dk_optimal_lineups)
      setDT(df)
      
      sim_data <- copy(rv$simulation_results)
      setDT(sim_data)
      
      # Map Name -> DKName (which has ID in parentheses)
      name_to_dkname <- setNames(sim_data$DKName, sim_data$Name)
      
      # Replace player names with DKName format (Name (ID))
      for (i in 1:6) {
        player_col <- paste0("Player", i)
        if (player_col %in% names(df)) {
          df[[player_col]] <- name_to_dkname[df[[player_col]]]
        }
      }
      
      # Multiply ownership by 100 (Phase 3 stores as fractions)
      df$CumulativeOwnership <- df$CumulativeOwnership * 100
      df$GeometricMeanOwnership <- df$GeometricMeanOwnership * 100
      
      # Round to proper decimal places
      df$CumulativeOwnership <- round(df$CumulativeOwnership, 1)
      df$GeometricMeanOwnership <- round(df$GeometricMeanOwnership, 1)
      df$CumulativeStarting <- round(df$CumulativeStarting, 0)
      df$GeometricMeanStarting <- round(df$GeometricMeanStarting, 1)
      
      # Select export columns (no score percentiles)
      export_cols <- c(
        paste0("Player", 1:6),
        "WinRate",
        "Top1Pct",
        "Top5Pct",
        "Top10Pct",
        "Top20Pct",
        "TotalSalary",
        "CumulativeOwnership",
        "GeometricMeanOwnership",
        "CumulativeStarting",
        "GeometricMeanStarting"
      )
      
      df_export <- df[, intersect(export_cols, names(df)), with = FALSE]
      
      # Rename columns
      setnames(
        df_export,
        old = c(
          "CumulativeOwnership",
          "GeometricMeanOwnership",
          "CumulativeStarting",
          "GeometricMeanStarting"
        ),
        new = c("TotalOwn%", "AvgOwn%", "TotalStart", "AvgStart"),
        skip_absent = TRUE
      )
      
      write.csv(df_export, file, row.names = FALSE)
    }
  )
  
  output$download_fd_optimal <- downloadHandler(
    filename = function() {
      paste0("nascar_fd_scored_",
             format(Sys.time(), "%Y%m%d_%H%M%S"),
             ".csv")
    },
    content = function(file) {
      req(rv$fd_optimal_lineups)
      req(rv$simulation_results)
      
      df <- copy(rv$fd_optimal_lineups)
      setDT(df)
      
      sim_data <- copy(rv$simulation_results)
      setDT(sim_data)
      
      # Map Name -> FDName (which has ID:Name format)
      name_to_fdname <- setNames(sim_data$FDName, sim_data$Name)
      
      # Replace player names with FDName format (ID:Name)
      for (i in 1:5) {
        player_col <- paste0("Player", i)
        if (player_col %in% names(df)) {
          df[[player_col]] <- name_to_fdname[df[[player_col]]]
        }
      }
      
      # Multiply ownership by 100 (Phase 3 stores as fractions)
      df$CumulativeOwnership <- df$CumulativeOwnership * 100
      df$GeometricMeanOwnership <- df$GeometricMeanOwnership * 100
      
      # Round to proper decimal places
      df$CumulativeOwnership <- round(df$CumulativeOwnership, 1)
      df$GeometricMeanOwnership <- round(df$GeometricMeanOwnership, 1)
      df$CumulativeStarting <- round(df$CumulativeStarting, 0)
      df$GeometricMeanStarting <- round(df$GeometricMeanStarting, 1)
      
      # Select export columns (no score percentiles)
      export_cols <- c(
        paste0("Player", 1:5),
        "WinRate",
        "Top1Pct",
        "Top5Pct",
        "Top10Pct",
        "Top20Pct",
        "TotalSalary",
        "CumulativeOwnership",
        "GeometricMeanOwnership",
        "CumulativeStarting",
        "GeometricMeanStarting"
      )
      
      df_export <- df[, intersect(export_cols, names(df)), with = FALSE]
      
      # Rename columns
      setnames(
        df_export,
        old = c(
          "CumulativeOwnership",
          "GeometricMeanOwnership",
          "CumulativeStarting",
          "GeometricMeanStarting"
        ),
        new = c("TotalOwn%", "AvgOwn%", "TotalStart", "AvgStart"),
        skip_absent = TRUE
      )
      
      write.csv(df_export, file, row.names = FALSE)
    }
  )
  
  output$download_dk_random_lineups <- downloadHandler(
    filename = function() {
      paste("dk_lineups_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(rv$dk_random_lineups, file, row.names = FALSE)
    }
  )
  
  output$download_fd_random_lineups <- downloadHandler(
    filename = function() {
      paste("fd_lineups_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(rv$fd_random_lineups, file, row.names = FALSE)
    }
  )
  
  # ==========================================================================
  # MEMORY CLEANUP
  # ==========================================================================
  
  observe({
    invalidateLater(180000) # Every 3 minutes
    gc(verbose = FALSE, full = TRUE)
  })
  
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
  
} # End server

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)