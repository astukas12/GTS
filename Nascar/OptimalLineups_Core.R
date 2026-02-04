library(data.table)
library(dplyr)
library(lpSolve)
library(parallel)

# =============================================================================
# HELPER: FIND TOP K LINEUPS FOR ONE SIMULATION
# =============================================================================

find_top_k_lineups <- function(sim_data, config, k) {
  
  roster_size <- config$roster_size
  salary_cap <- config$salary_cap
  
  n_players <- nrow(sim_data)
  
  if (n_players < roster_size) return(NULL)
  
  # Objective: maximize fantasy points
  objective <- sim_data$FantasyPoints
  
  # Check for invalid values
  if (any(is.na(objective)) || any(is.infinite(objective))) {
    return(NULL)
  }
  
  # Constraint matrix
  constraints <- rbind(
    rep(1, n_players),              # Roster size constraint
    sim_data$Salary                 # Salary constraint
  )
  
  # Check for invalid salaries
  if (any(is.na(constraints[2,])) || any(is.infinite(constraints[2,]))) {
    return(NULL)
  }
  
  constraint_dir <- c("==", "<=")
  constraint_rhs <- c(roster_size, salary_cap)
  
  lineups <- list()
  excluded_indices <- c()
  
  for (lineup_num in 1:k) {
    # Solve with exclusions
    current_obj <- objective
    if (length(excluded_indices) > 0) {
      current_obj[excluded_indices] <- -Inf
    }
    
    result <- tryCatch({
      lp(
        direction = "max",
        objective.in = current_obj,
        const.mat = constraints,
        const.dir = constraint_dir,
        const.rhs = constraint_rhs,
        all.bin = TRUE
      )
    }, error = function(e) {
      list(status = 1)  # Return failure status
    })
    
    if (result$status == 0) {
      selected <- which(result$solution == 1)
      
      if (length(selected) == roster_size) {
        lineup <- sim_data[selected]
        
        # Create player columns
        player_data <- data.table(
          SimID = sim_data$SimID[1],
          TotalScore = sum(lineup$FantasyPoints),
          TotalSalary = sum(lineup$Salary)
        )
        
        # Add player names
        for (i in 1:roster_size) {
          player_data[[paste0("Player", i)]] <- lineup$Player[i]
        }
        
        lineups[[lineup_num]] <- player_data
        
        # Exclude this exact combination
        excluded_indices <- c(excluded_indices, selected)
      }
    }
  }
  
  if (length(lineups) > 0) {
    return(rbindlist(lineups))
  } else {
    return(NULL)
  }
}

# =============================================================================
# PHASE 1: PARALLEL LINEUP FINDING
# =============================================================================

find_optimal_lineups <- function(sim_results, config, k = 3, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: Finding optimal lineups...\n")
  
  setDT(sim_results)
  
  roster_size <- config$roster_size
  salary_cap <- config$salary_cap
  progress_freq <- if (!is.null(config$progress_frequency)) config$progress_frequency else 500
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else Inf
  use_parallel <- if (!is.null(config$use_parallel)) config$use_parallel else TRUE
  
  sim_ids <- unique(sim_results$SimID)
  n_sims <- length(sim_ids)
  
  if (verbose) {
    cat(sprintf("  %s sims | top %d per sim | cap: %s\n",
                format(n_sims, big.mark = ","), k,
                if (is.finite(max_lineups)) format(max_lineups, big.mark = ",") else "none"))
  }
  
  start_time <- Sys.time()
  
  if (use_parallel && n_sims > 100) {
    library(parallel)
    
    n_cores <- min(detectCores() - 1, 7)
    if (verbose) cat(sprintf("  Using %d cores\n", n_cores))
    
    cl <- makeCluster(n_cores, type = "PSOCK")
    
    clusterEvalQ(cl, {
      library(data.table)
      library(lpSolve)
    })
    
    clusterExport(cl, "find_top_k_lineups", envir = environment())
    clusterExport(cl, c("roster_size", "salary_cap", "k"), envir = environment())
    
    worker_function <- function(sim_id, sim_data_all) {
      worker_config <- list(roster_size = roster_size, salary_cap = salary_cap)
      sim_data <- sim_data_all[SimID == sim_id]
      tryCatch({
        find_top_k_lineups(sim_data, worker_config, k)
      }, error = function(e) NULL)
    }
    
    clusterExport(cl, "worker_function", envir = environment())
    clusterExport(cl, "sim_results", envir = environment())
    
    all_lineups <- tryCatch({
      parLapply(cl, sim_ids, function(sid) {
        worker_function(sid, sim_results)
      })
    }, error = function(e) {
      if (verbose) cat("  Parallel failed, using sequential\n")
      stopCluster(cl)
      return(NULL)
    })
    
    stopCluster(cl)
    
    if (is.null(all_lineups)) {
      use_parallel <- FALSE
    } else {
      all_lineups <- all_lineups[!sapply(all_lineups, is.null)]
    }
  }
  
  if (!use_parallel || n_sims <= 100) {
    all_lineups <- list()
    
    for (i in seq_along(sim_ids)) {
      sim_id <- sim_ids[i]
      sim_data <- sim_results[SimID == sim_id]
      
      sim_lineups <- tryCatch({
        find_top_k_lineups(sim_data, config, k)
      }, error = function(e) NULL)
      
      if (!is.null(sim_lineups) && nrow(sim_lineups) > 0) {
        all_lineups[[length(all_lineups) + 1]] <- sim_lineups
      }
      
      if (verbose && (i %% progress_freq == 0 || i == n_sims)) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        pct_complete <- (i / n_sims) * 100
        eta <- (elapsed / i) * (n_sims - i)
        
        cat(sprintf("\r  Phase 1: %.0f%% | %.1fs | ETA: %.0fs", pct_complete, elapsed, eta))
        flush.console()
      }
    }
    if (verbose) cat("\n")
  }
  
  if (length(all_lineups) == 0) {
    stop("No valid lineups found")
  }
  
  all_lineups_dt <- rbindlist(all_lineups)
  
  player_cols <- grep("^Player", names(all_lineups_dt), value = TRUE)
  all_lineups_dt[, lineup_sig := do.call(paste, c(.SD, sep = "_")), .SDcols = player_cols]
  
  unique_lineups <- all_lineups_dt[!duplicated(lineup_sig)]
  unique_lineups[, lineup_sig := NULL]
  
  if (nrow(unique_lineups) > max_lineups) {
    if (verbose) cat(sprintf("  Capping at %s lineups\n", format(max_lineups, big.mark = ",")))
    unique_lineups <- unique_lineups[1:max_lineups]
  }
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (verbose) {
    cat(sprintf("  ✓ Phase 1: %s lineups | %.1fs\n",
                format(nrow(unique_lineups), big.mark = ","), elapsed_time))
  }
  
  return(list(
    unique_lineups = unique_lineups,
    n_sims = n_sims,
    config = config
  ))
}
# =============================================================================
# PHASE 2: ULTRA-FAST PARALLEL SCORING WITH VECTORIZATION
# =============================================================================
score_all_lineups<- function(lineup_data, sim_results, verbose = TRUE, sims_per_batch = 5000) {
  
  if (verbose) cat("\nPhase 2: Scoring lineups (matrix method)...\n")
  
  setDT(sim_results)
  
  unique_lineups <- lineup_data$unique_lineups
  n_lineups <- nrow(unique_lineups)
  n_sims <- lineup_data$n_sims
  
  if (verbose) {
    cat(sprintf("  %s lineups × %s sims\n",
                format(n_lineups, big.mark = ","),
                format(n_sims, big.mark = ",")))
  }
  
  start_time <- Sys.time()
  
  player_cols <- grep("^Player", names(unique_lineups), value = TRUE)
  roster_size <- length(player_cols)
  
  if (verbose) {
    cat("  Phase 2: 10%% | Building lineup matrix...\n")
    flush.console()
  }
  
  platform_col <- if ("DKFantasyPoints" %in% names(sim_results)) {
    "DKFantasyPoints"
  } else {
    "FDFantasyPoints"
  }
  
  all_players <- unique(unlist(unique_lineups[, ..player_cols]))
  player_to_id <- setNames(seq_along(all_players), all_players)
  
  lineup_matrix <- matrix(0, nrow = n_lineups, ncol = length(all_players))
  
  for (i in 1:n_lineups) {
    players <- as.character(unique_lineups[i, ..player_cols])
    player_ids <- player_to_id[players]
    lineup_matrix[i, player_ids] <- 1
  }
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  Phase 2: 20%% | %.1fs | Organizing sim data...\n", elapsed))
    flush.console()
  }
  
  sim_score_list <- vector("list", length(all_players))
  names(sim_score_list) <- all_players
  
  for (player_name in all_players) {
    player_data <- sim_results[Player == player_name, .(SimID, Score = get(platform_col))]
    setkey(player_data, SimID)
    sim_score_list[[player_name]] <- player_data
  }
  
  n_batches <- ceiling(n_sims / sims_per_batch)
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  Phase 2: 30%% | %.1fs | Processing %d batches...\n", 
                elapsed, n_batches))
    flush.console()
  }
  
  score_matrix <- matrix(0, nrow = n_lineups, ncol = n_sims)
  
  for (batch_idx in 1:n_batches) {
    sim_start <- (batch_idx - 1) * sims_per_batch + 1
    sim_end <- min(batch_idx * sims_per_batch, n_sims)
    batch_sim_ids <- sim_start:sim_end
    n_batch_sims <- length(batch_sim_ids)
    
    batch_score_matrix <- matrix(0, nrow = n_batch_sims, ncol = length(all_players))
    
    for (player_idx in seq_along(all_players)) {
      player_name <- all_players[player_idx]
      player_data <- sim_score_list[[player_name]]
      
      matching_sims <- player_data[J(batch_sim_ids), nomatch = 0]
      if (nrow(matching_sims) > 0) {
        row_indices <- matching_sims$SimID - sim_start + 1
        batch_score_matrix[row_indices, player_idx] <- matching_sims$Score
      }
    }
    
    batch_lineup_scores <- lineup_matrix %*% t(batch_score_matrix)
    
    score_matrix[, batch_sim_ids] <- batch_lineup_scores
    
    if (verbose) {
      pct <- 30 + ((batch_idx / n_batches) * 60)
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      if (batch_idx < n_batches) {
        rate <- batch_idx / elapsed
        eta <- (n_batches - batch_idx) / rate
        cat(sprintf("\r  Phase 2: %.0f%% | %.1fs | ETA: %.0fs", pct, elapsed, eta))
      } else {
        cat(sprintf("\r  Phase 2: 90%% | %.1fs | Finalizing...", elapsed))
      }
      
      flush.console()
    }
    
    rm(batch_score_matrix, batch_lineup_scores)
    gc(verbose = FALSE)
  }
  
  if (verbose) {
    cat("\n")
    elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  ✓ Phase 2: %.1fs\n", elapsed_time))
  }
  
  return(score_matrix)
}


# =============================================================================
# =============================================================================
# PHASE 3: CALCULATE DISTRIBUTION METRICS
# =============================================================================
calculate_distribution_metrics <- function(score_matrix, lineup_data, config, 
                                           ownership_data = NULL, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 3: Calculating metrics...\n")
  
  unique_lineups <- lineup_data$unique_lineups
  n_sims <- lineup_data$n_sims
  n_lineups <- nrow(unique_lineups)
  
  start_time <- Sys.time()
  
  percentiles <- config$percentiles
  score_percentiles <- config$score_percentiles
  
  if (verbose) {
    cat("  Phase 3: 20%% | Calculating rankings...\n")
    flush.console()
  }
  
  # Step 1: Calculate Top % (vectorized, chunked for progress)
  top_pcts <- matrix(0, nrow = n_lineups, ncol = length(percentiles))
  
  chunk_size <- max(1, floor(n_sims / 5))
  
  for (chunk_start in seq(1, n_sims, by = chunk_size)) {
    chunk_end <- min(chunk_start + chunk_size - 1, n_sims)
    
    for (i in chunk_start:chunk_end) {
      sim_scores <- score_matrix[, i]
      sim_rank <- rank(-sim_scores, ties.method = "min")
      
      for (p_idx in seq_along(percentiles)) {
        threshold_rank <- ceiling(n_lineups * percentiles[p_idx])
        top_pcts[, p_idx] <- top_pcts[, p_idx] + (sim_rank <= threshold_rank)
      }
    }
    
    if (verbose && chunk_end < n_sims) {
      pct_done <- (chunk_end / n_sims) * 40 + 20
      cat(sprintf("\r  Phase 3: %.0f%% | Ranking sims...", pct_done))
      flush.console()
    }
  }
  
  top_pcts <- (top_pcts / n_sims) * 100
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("\r  Phase 3: 60%% | %.1fs | Calculating win rates...\n", elapsed))
    flush.console()
  }
  
  # Step 2: Win rate (vectorized)
  max_scores <- apply(score_matrix, 2, max)
  win_counts <- rowSums(sweep(score_matrix, 2, max_scores, "=="))
  win_rate <- (win_counts / n_sims) * 100
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  Phase 3: 70%% | %.1fs | Calculating score percentiles...\n", elapsed))
    flush.console()
  }
  
  # Step 3: Score percentiles (vectorized)
  score_pct_matrix <- matrix(0, nrow = n_lineups, ncol = length(score_percentiles))
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  Phase 3: 80%% | %.1fs | Calculating ownership/starting...\n", elapsed))
    flush.console()
  }
  
  # Step 4: Ownership and starting positions
  total_salary <- unique_lineups$TotalSalary
  player_cols <- grep("^Player", names(unique_lineups), value = TRUE)
  
  cumulative_own <- rep(0, n_lineups)
  geometric_own <- rep(0, n_lineups)
  cumulative_start <- rep(0, n_lineups)
  geometric_start <- rep(0, n_lineups)
  
  if (!is.null(ownership_data) && nrow(ownership_data) > 0) {
    setDT(ownership_data)
    
    # Make sure we have Player column
    if (!"Player" %in% names(ownership_data) && "Name" %in% names(ownership_data)) {
      setnames(ownership_data, "Name", "Player")
    }
    
    # Check what columns we have
    has_own <- any(c("Own", "DKOP", "FDOP") %in% names(ownership_data))
    has_starting <- "Starting" %in% names(ownership_data)
    
    if (has_own || has_starting) {
      # Determine which ownership column to use
      own_col <- NULL
      if ("Own" %in% names(ownership_data)) {
        own_col <- "Own"
      } else if ("DKOP" %in% names(ownership_data)) {
        own_col <- "DKOP"
      } else if ("FDOP" %in% names(ownership_data)) {
        own_col <- "FDOP"
      }
      
      for (i in 1:n_lineups) {
        players <- as.character(unique_lineups[i, ..player_cols])
        
        # Get ownership values
        if (!is.null(own_col)) {
          own_values <- ownership_data[Player %in% players, get(own_col)]
          
          if (length(own_values) > 0 && any(!is.na(own_values))) {
            cumulative_own[i] <- sum(own_values, na.rm = TRUE)
            own_values_safe <- own_values[!is.na(own_values) & own_values > 0]
            if (length(own_values_safe) > 0) {
              geometric_own[i] <- exp(mean(log(own_values_safe)))
            }
          }
        }
        
        # Get starting position values
        if (has_starting) {
          start_values <- ownership_data[Player %in% players, Starting]
          
          if (length(start_values) > 0 && any(!is.na(start_values))) {
            cumulative_start[i] <- sum(start_values, na.rm = TRUE)
            start_values_safe <- start_values[!is.na(start_values) & start_values > 0]
            if (length(start_values_safe) > 0) {
              geometric_start[i] <- exp(mean(log(start_values_safe)))
            }
          }
        }
      }
    }
  }
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  Phase 3: 90%% | %.1fs | Assembling results...\n", elapsed))
    flush.console()
  }
  
  # Step 5: Combine results
  results <- data.table(
    unique_lineups[, ..player_cols],
    WinRate = win_rate,
    Top1Pct = top_pcts[, 1],
    Top5Pct = top_pcts[, 2],
    Top10Pct = top_pcts[, 3],
    Top20Pct = top_pcts[, 4],
    Pct10 = score_pct_matrix[, 1],
    Pct25 = score_pct_matrix[, 2],
    Pct50 = score_pct_matrix[, 3],
    Pct75 = score_pct_matrix[, 4],
    Pct90 = score_pct_matrix[, 5],
    TotalSalary = total_salary,
    CumulativeOwnership = cumulative_own,
    GeometricMeanOwnership = geometric_own,
    CumulativeStarting = cumulative_start,
    GeometricMeanStarting = geometric_start
  )
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (verbose) {
    cat(sprintf("  ✓ Phase 3: %.1fs\n", elapsed_time))
  }
  
  return(results)
}