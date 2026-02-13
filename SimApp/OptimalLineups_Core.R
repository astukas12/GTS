# ============================================================================
# OPTIMAL LINEUPS CORE - CLEAN ARCHITECTURE WITH MODE ROUTING
# ============================================================================

library(data.table)
library(dplyr)
library(lpSolve)
library(parallel)

# =============================================================================
# MAIN ENTRY POINT - MODE ROUTER
# =============================================================================

find_optimal_lineups <- function(sim_results, config, mode = "standard", k = 3, verbose = TRUE) {
  
  if (mode == "standard") {
    return(find_optimal_lineups_standard(sim_results, config, k, verbose))
  } else if (mode == "mvp") {
    return(find_optimal_lineups_mvp(sim_results, config, k, verbose))
  } else if (mode == "captain") {
    return(find_optimal_lineups_captain(sim_results, config, k, verbose))
  } else {
    stop(paste("Unknown mode:", mode, "- must be 'standard', 'mvp', or 'captain'"))
  }
}


# =============================================================================
# MODE 1: STANDARD LINEUPS (DK, NASCAR FD, etc.)
# =============================================================================

find_optimal_lineups_standard <- function(sim_results, config, k = 3, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: Finding optimal lineups (STANDARD mode)...\n")
  
  setDT(sim_results)
  
  roster_size <- config$roster_size
  salary_cap <- config$salary_cap
  progress_freq <- if (!is.null(config$progress_frequency)) config$progress_frequency else 500
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else Inf
  use_parallel <- if (!is.null(config$use_parallel)) config$use_parallel else TRUE
  
  # OPTIMIZATION: Pre-filter to viable player pool per sim
  # Default: top 15 scorers + top 15 value (pts per dollar)
  player_pool_size <- if (!is.null(config$player_pool_size)) config$player_pool_size else 15
  
  sim_ids <- unique(sim_results$SimID)
  n_sims <- length(sim_ids)
  
  if (verbose) {
    cat(sprintf("  %s sims | top %d per sim | cap: %s\n",
                format(n_sims, big.mark = ","), k,
                if (is.finite(max_lineups)) format(max_lineups, big.mark = ",") else "none"))
    cat(sprintf("  Player pool: top %d by score + top %d by value\n", 
                player_pool_size, player_pool_size))
  }
  
  start_time <- Sys.time()
  
  # Helper function for one sim
  find_top_k_for_sim <- function(sim_data, roster_size, salary_cap, k, pool_size) {
    n_players <- nrow(sim_data)
    if (n_players < roster_size) return(NULL)
    
    # OPTIMIZATION: Filter to viable player pool
    # Keep top scorers + top value players (union, not intersection)
    if (n_players > pool_size * 2) {
      # Calculate value efficiently: points per $1000
      value <- sim_data$FantasyPoints / sim_data$Salary * 1000
      
      # Get top players by score
      # Use order() instead of partial sort (works in parallel)
      top_score_indices <- head(order(sim_data$FantasyPoints, decreasing = TRUE), pool_size)
      top_by_score <- sim_data$Player[top_score_indices]
      
      # Get top players by value
      top_value_indices <- head(order(value, decreasing = TRUE), pool_size)
      top_by_value <- sim_data$Player[top_value_indices]
      
      # Union of both
      viable_players <- unique(c(top_by_score, top_by_value))
      
      # Filter sim_data to viable pool
      sim_data <- sim_data[Player %in% viable_players]
      
      n_players <- nrow(sim_data)
    }
    
    if (n_players < roster_size) return(NULL)
    
    objective <- sim_data$FantasyPoints
    if (any(is.na(objective)) || any(is.infinite(objective))) return(NULL)
    
    constraints <- rbind(
      rep(1, n_players),
      sim_data$Salary
    )
    
    if (any(is.na(constraints[2,])) || any(is.infinite(constraints[2,]))) return(NULL)
    
    constraint_dir <- c("==", "<=")
    constraint_rhs <- c(roster_size, salary_cap)
    
    lineups <- list()
    excluded_indices <- c()
    
    for (lineup_num in 1:k) {
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
      }, error = function(e) list(status = 1))
      
      if (result$status == 0) {
        selected <- which(result$solution == 1)
        
        if (length(selected) == roster_size) {
          lineup <- sim_data[selected]
          
          player_data <- data.table(
            SimID = sim_data$SimID[1],
            TotalScore = sum(lineup$FantasyPoints),
            TotalSalary = sum(lineup$Salary)
          )
          
          for (i in 1:roster_size) {
            player_data[[paste0("Player", i)]] <- lineup$Player[i]
          }
          
          lineups[[lineup_num]] <- player_data
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
  
  # Process all sims
  if (use_parallel && n_sims > 100) {
    n_cores <- min(detectCores() - 1, 7)
    if (verbose) cat(sprintf("  Using %d cores\n", n_cores))
    
    cl <- makeCluster(n_cores, type = "PSOCK")
    clusterEvalQ(cl, {
      library(data.table)
      library(lpSolve)
    })
    clusterExport(cl, c("find_top_k_for_sim", "roster_size", "salary_cap", "k", "player_pool_size"), 
                  envir = environment())
    clusterExport(cl, "sim_results", envir = environment())
    
    all_lineups <- parLapply(cl, sim_ids, function(sid) {
      sim_data <- sim_results[SimID == sid]
      find_top_k_for_sim(sim_data, roster_size, salary_cap, k, player_pool_size)
    })
    
    stopCluster(cl)
    all_lineups <- all_lineups[!sapply(all_lineups, is.null)]
    
  } else {
    all_lineups <- list()
    
    for (i in seq_along(sim_ids)) {
      sim_id <- sim_ids[i]
      sim_data <- sim_results[SimID == sim_id]
      
      sim_lineups <- find_top_k_for_sim(sim_data, roster_size, salary_cap, k, player_pool_size)
      
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
  
  if (length(all_lineups) == 0) stop("No valid lineups found")
  
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
    config = config,
    mode = "standard"
  ))
}


# =============================================================================
# MODE 2: MVP LINEUPS (FD MMA)
# =============================================================================

find_optimal_lineups_mvp <- function(sim_results, config, k = 3, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: Finding optimal lineups (MVP mode)...\n")
  
  setDT(sim_results)
  
  roster_size <- config$roster_size
  salary_cap <- config$salary_cap
  mvp_multiplier <- if (!is.null(config$mvp_multiplier)) config$mvp_multiplier else 1.5
  progress_freq <- if (!is.null(config$progress_frequency)) config$progress_frequency else 500
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else Inf
  use_parallel <- if (!is.null(config$use_parallel)) config$use_parallel else TRUE
  
  sim_ids <- unique(sim_results$SimID)
  n_sims <- length(sim_ids)
  
  if (verbose) {
    cat(sprintf("  %s sims | top %d per sim | MVP multiplier: %.1fx\n",
                format(n_sims, big.mark = ","), k, mvp_multiplier))
  }
  
  start_time <- Sys.time()
  
  # Helper function for one sim
  find_top_k_mvp_for_sim <- function(sim_data, roster_size, salary_cap, mvp_multiplier, k) {
    n_players <- nrow(sim_data)
    if (n_players < roster_size) return(NULL)
    
    # Create expanded pool: MVP and FLEX versions
    mvp_data <- copy(sim_data)
    mvp_data[, PlayerType := "MVP"]
    mvp_data[, FantasyPoints := FantasyPoints * mvp_multiplier]
    # Salary stays same for MVP
    
    flex_data <- copy(sim_data)
    flex_data[, PlayerType := "FLEX"]
    
    expanded_data <- rbind(mvp_data, flex_data)
    n_expanded <- nrow(expanded_data)
    
    objective <- expanded_data$FantasyPoints
    if (any(is.na(objective)) || any(is.infinite(objective))) return(NULL)
    
    # Constraints
    roster_constraint <- rep(1, n_expanded)
    salary_constraint <- expanded_data$Salary
    mvp_constraint <- ifelse(expanded_data$PlayerType == "MVP", 1, 0)
    
    # Each player at most once
    player_constraints <- matrix(0, nrow = n_players, ncol = n_expanded)
    for (i in 1:n_players) {
      player_name <- sim_data$Player[i]
      player_constraints[i, ] <- ifelse(expanded_data$Player == player_name, 1, 0)
    }
    
    constraints <- rbind(
      roster_constraint,
      salary_constraint,
      mvp_constraint,
      player_constraints
    )
    
    constraint_dir <- c("==", "<=", "==", rep("<=", n_players))
    constraint_rhs <- c(roster_size, salary_cap, 1, rep(1, n_players))
    
    lineups <- list()
    excluded_combos <- list()
    
    for (lineup_num in 1:k) {
      current_obj <- objective
      
      if (length(excluded_combos) > 0) {
        for (combo in excluded_combos) {
          if (all(combo %in% 1:n_expanded)) {
            current_obj[combo] <- -Inf
          }
        }
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
      }, error = function(e) list(status = 1))
      
      if (result$status == 0) {
        selected <- which(result$solution == 1)
        
        if (length(selected) == roster_size) {
          lineup <- expanded_data[selected]
          
          mvp <- lineup[PlayerType == "MVP"]
          flex <- lineup[PlayerType == "FLEX"]
          
          player_data <- data.table(
            SimID = sim_data$SimID[1],
            MVP = mvp$Player,
            TotalScore = sum(lineup$FantasyPoints),
            TotalSalary = sum(lineup$Salary)
          )
          
          for (i in 1:nrow(flex)) {
            player_data[[paste0("Player", i)]] <- flex$Player[i]
          }
          
          lineups[[lineup_num]] <- player_data
          excluded_combos[[lineup_num]] <- selected
        }
      }
    }
    
    if (length(lineups) > 0) {
      return(rbindlist(lineups))
    } else {
      return(NULL)
    }
  }
  
  # Process all sims
  if (use_parallel && n_sims > 100) {
    n_cores <- min(detectCores() - 1, 7)
    if (verbose) cat(sprintf("  Using %d cores\n", n_cores))
    
    cl <- makeCluster(n_cores, type = "PSOCK")
    clusterEvalQ(cl, {
      library(data.table)
      library(lpSolve)
    })
    clusterExport(cl, c("find_top_k_mvp_for_sim", "roster_size", "salary_cap", "mvp_multiplier", "k"), 
                  envir = environment())
    clusterExport(cl, "sim_results", envir = environment())
    
    all_lineups <- parLapply(cl, sim_ids, function(sid) {
      sim_data <- sim_results[SimID == sid]
      find_top_k_mvp_for_sim(sim_data, roster_size, salary_cap, mvp_multiplier, k)
    })
    
    stopCluster(cl)
    all_lineups <- all_lineups[!sapply(all_lineups, is.null)]
    
  } else {
    all_lineups <- list()
    
    for (i in seq_along(sim_ids)) {
      sim_id <- sim_ids[i]
      sim_data <- sim_results[SimID == sim_id]
      
      sim_lineups <- find_top_k_mvp_for_sim(sim_data, roster_size, salary_cap, mvp_multiplier, k)
      
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
  
  if (length(all_lineups) == 0) stop("No valid lineups found")
  
  all_lineups_dt <- rbindlist(all_lineups)
  player_cols <- c("MVP", grep("^Player", names(all_lineups_dt), value = TRUE))
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
    config = config,
    mode = "mvp"
  ))
}


# =============================================================================
# MODE 3: CAPTAIN LINEUPS (DK Showdown)
# =============================================================================

find_optimal_lineups_captain <- function(sim_results, config, k = 3, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: Finding optimal lineups (CAPTAIN mode)...\n")
  
  setDT(sim_results)
  
  roster_size <- config$roster_size
  salary_cap <- config$salary_cap
  cpt_multiplier <- if (!is.null(config$cpt_multiplier)) config$cpt_multiplier else 1.5
  progress_freq <- if (!is.null(config$progress_frequency)) config$progress_frequency else 500
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else Inf
  use_parallel <- if (!is.null(config$use_parallel)) config$use_parallel else TRUE
  
  sim_ids <- unique(sim_results$SimID)
  n_sims <- length(sim_ids)
  
  if (verbose) {
    cat(sprintf("  %s sims | top %d per sim | CPT multiplier: %.1fx\n",
                format(n_sims, big.mark = ","), k, cpt_multiplier))
  }
  
  start_time <- Sys.time()
  
  # Helper function for one sim
  find_top_k_captain_for_sim <- function(sim_data, roster_size, salary_cap, cpt_multiplier, k) {
    n_players <- nrow(sim_data)
    if (n_players < roster_size) return(NULL)
    
    # Create expanded pool: CPT and UTIL versions
    cpt_data <- copy(sim_data)
    cpt_data[, PlayerType := "CPT"]
    cpt_data[, FantasyPoints := FantasyPoints * cpt_multiplier]
    cpt_data[, Salary := Salary * cpt_multiplier]  # Captain salary also multiplied
    
    util_data <- copy(sim_data)
    util_data[, PlayerType := "UTIL"]
    
    expanded_data <- rbind(cpt_data, util_data)
    n_expanded <- nrow(expanded_data)
    
    objective <- expanded_data$FantasyPoints
    if (any(is.na(objective)) || any(is.infinite(objective))) return(NULL)
    
    # Constraints
    roster_constraint <- rep(1, n_expanded)
    salary_constraint <- expanded_data$Salary
    cpt_constraint <- ifelse(expanded_data$PlayerType == "CPT", 1, 0)
    
    # Each player at most once
    player_constraints <- matrix(0, nrow = n_players, ncol = n_expanded)
    for (i in 1:n_players) {
      player_name <- sim_data$Player[i]
      player_constraints[i, ] <- ifelse(expanded_data$Player == player_name, 1, 0)
    }
    
    constraints <- rbind(
      roster_constraint,
      salary_constraint,
      cpt_constraint,
      player_constraints
    )
    
    constraint_dir <- c("==", "<=", "==", rep("<=", n_players))
    constraint_rhs <- c(roster_size, salary_cap, 1, rep(1, n_players))
    
    lineups <- list()
    excluded_combos <- list()
    
    for (lineup_num in 1:k) {
      current_obj <- objective
      
      if (length(excluded_combos) > 0) {
        for (combo in excluded_combos) {
          if (all(combo %in% 1:n_expanded)) {
            current_obj[combo] <- -Inf
          }
        }
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
      }, error = function(e) list(status = 1))
      
      if (result$status == 0) {
        selected <- which(result$solution == 1)
        
        if (length(selected) == roster_size) {
          lineup <- expanded_data[selected]
          
          captain <- lineup[PlayerType == "CPT"]
          utilities <- lineup[PlayerType == "UTIL"]
          
          player_data <- data.table(
            SimID = sim_data$SimID[1],
            Captain = captain$Player,
            TotalScore = sum(lineup$FantasyPoints),
            TotalSalary = sum(lineup$Salary)
          )
          
          for (i in 1:nrow(utilities)) {
            player_data[[paste0("Util", i)]] <- utilities$Player[i]
          }
          
          lineups[[lineup_num]] <- player_data
          excluded_combos[[lineup_num]] <- selected
        }
      }
    }
    
    if (length(lineups) > 0) {
      return(rbindlist(lineups))
    } else {
      return(NULL)
    }
  }
  
  # Process all sims
  if (use_parallel && n_sims > 100) {
    n_cores <- min(detectCores() - 1, 7)
    if (verbose) cat(sprintf("  Using %d cores\n", n_cores))
    
    cl <- makeCluster(n_cores, type = "PSOCK")
    clusterEvalQ(cl, {
      library(data.table)
      library(lpSolve)
    })
    clusterExport(cl, c("find_top_k_captain_for_sim", "roster_size", "salary_cap", "cpt_multiplier", "k"), 
                  envir = environment())
    clusterExport(cl, "sim_results", envir = environment())
    
    all_lineups <- parLapply(cl, sim_ids, function(sid) {
      sim_data <- sim_results[SimID == sid]
      find_top_k_captain_for_sim(sim_data, roster_size, salary_cap, cpt_multiplier, k)
    })
    
    stopCluster(cl)
    all_lineups <- all_lineups[!sapply(all_lineups, is.null)]
    
  } else {
    all_lineups <- list()
    
    for (i in seq_along(sim_ids)) {
      sim_id <- sim_ids[i]
      sim_data <- sim_results[SimID == sim_id]
      
      sim_lineups <- find_top_k_captain_for_sim(sim_data, roster_size, salary_cap, cpt_multiplier, k)
      
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
  
  if (length(all_lineups) == 0) stop("No valid lineups found")
  
  all_lineups_dt <- rbindlist(all_lineups)
  player_cols <- c("Captain", grep("^Util", names(all_lineups_dt), value = TRUE))
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
    config = config,
    mode = "captain"
  ))
}


# =============================================================================
# PHASE 2: SCORE ALL LINEUPS (MODE-AGNOSTIC)
# =============================================================================

score_all_lineups <- function(lineup_data, sim_results, verbose = TRUE, sims_per_batch = 5000) {
  
  if (verbose) cat("\nPhase 2: Scoring lineups (matrix method)...\n")
  
  setDT(sim_results)
  
  unique_lineups <- lineup_data$unique_lineups
  n_lineups <- nrow(unique_lineups)
  n_sims <- lineup_data$n_sims
  config <- lineup_data$config
  mode <- lineup_data$mode
  
  # MEMORY CHECK: Calculate if we can fit full matrix in memory (assume 4GB available)
  # Use as.numeric() to avoid integer overflow for large matrices
  matrix_size_gb <- (as.numeric(n_lineups) * as.numeric(n_sims) * 8) / (1024^3)
  use_efficient_mode <- matrix_size_gb > 4
  
  if (verbose) {
    cat(sprintf("  %s lineups × %s sims | Mode: %s\n",
                format(n_lineups, big.mark = ","),
                format(n_sims, big.mark = ","),
                mode))
    
    if (use_efficient_mode) {
      cat(sprintf("  Memory-efficient: %.1f GB needed, using rank accumulation\n", matrix_size_gb))
    }
  }
  
  start_time <- Sys.time()
  
  # Detect player columns based on what exists in data
  if ("Captain" %in% names(unique_lineups)) {
    player_cols <- c("Captain", grep("^Util", names(unique_lineups), value = TRUE))
    multipliers <- c(config$cpt_multiplier, rep(1, length(player_cols) - 1))
  } else if ("MVP" %in% names(unique_lineups)) {
    player_cols <- c("MVP", grep("^Player", names(unique_lineups), value = TRUE))
    multipliers <- c(config$mvp_multiplier, rep(1, length(player_cols) - 1))
  } else {
    player_cols <- grep("^Player", names(unique_lineups), value = TRUE)
    multipliers <- rep(1, length(player_cols))
  }
  
  # Get score column from config
  platform_col <- config$platform_col
  
  # Create player-to-index mapping
  all_players <- unique(unlist(unique_lineups[, ..player_cols]))
  player_to_id <- setNames(1:length(all_players), all_players)
  
  # Build lineup matrix
  lineup_matrix <- matrix(0, nrow = n_lineups, ncol = length(all_players))
  
  for (i in 1:n_lineups) {
    players <- as.character(unique_lineups[i, ..player_cols])
    
    for (j in seq_along(players)) {
      player_id <- player_to_id[players[j]]
      lineup_matrix[i, player_id] <- multipliers[j]
    }
  }
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  Phase 2: 20%% | %.1fs | Organizing sim data...\n", elapsed))
    flush.console()
  }
  
  # Organize sim scores by player
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
  
  # ============================================================================
  # MEMORY-EFFICIENT MODE: Accumulate ranks instead of storing full matrix
  # ============================================================================
  
  if (use_efficient_mode) {
    # Initialize counters
    percentiles_config <- c(0.01, 0.05, 0.10, 0.20)
    win_counts <- rep(0, n_lineups)
    top_counts <- matrix(0, nrow = n_lineups, ncol = length(percentiles_config))
    
    sims_processed <- 0
    
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
      
      # Calculate lineup scores for this batch: lineups × sims
      batch_lineup_scores <- lineup_matrix %*% t(batch_score_matrix)
      
      # ======================================================================
      # OPTIMIZATION: Vectorized batch ranking with partial sorting
      # Instead of ranking each sim individually, process in mini-batches
      # ======================================================================
      
      # Process sims in mini-batches for vectorization
      rank_batch_size <- 100  # Process 100 sims at once
      n_rank_batches <- ceiling(n_batch_sims / rank_batch_size)
      
      for (rank_batch_idx in 1:n_rank_batches) {
        rb_start <- (rank_batch_idx - 1) * rank_batch_size + 1
        rb_end <- min(rank_batch_idx * rank_batch_size, n_batch_sims)
        rb_size <- rb_end - rb_start + 1
        
        # Get scores for this mini-batch: lineups × mini_batch_sims
        mini_batch_scores <- batch_lineup_scores[, rb_start:rb_end, drop = FALSE]
        
        # VECTORIZED: Find max scores across all sims in mini-batch
        max_scores <- apply(mini_batch_scores, 2, max)
        
        # VECTORIZED: Accumulate win counts (lineups that equal max in each sim)
        win_matrix <- sweep(mini_batch_scores, 2, max_scores, "==")
        win_counts <- win_counts + rowSums(win_matrix)
        
        # PARTIAL SORTING: For each percentile, find threshold and count
        # This is faster than full ranking
        for (p_idx in seq_along(percentiles_config)) {
          threshold_rank <- ceiling(n_lineups * percentiles_config[p_idx])
          
          # For each sim in mini-batch, use partial sort to find top threshold_rank
          for (sim_offset in 1:rb_size) {
            sim_scores <- mini_batch_scores[, sim_offset]
            
            if (threshold_rank < n_lineups) {
              # Sort and get threshold value
              sorted_scores <- sort(sim_scores, decreasing = TRUE)
              threshold_score <- sorted_scores[threshold_rank]
              # Count lineups >= threshold (handles ties correctly)
              top_counts[, p_idx] <- top_counts[, p_idx] + (sim_scores >= threshold_score)
            } else {
              # If threshold_rank >= n_lineups, all lineups qualify
              top_counts[, p_idx] <- top_counts[, p_idx] + 1
            }
          }
        }
        
        sims_processed <- sims_processed + rb_size
      }
      
      if (verbose) {
        pct <- 30 + ((sims_processed / n_sims) * 60)
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        
        if (sims_processed < n_sims) {
          rate <- sims_processed / elapsed
          eta <- (n_sims - sims_processed) / rate
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
      cat(sprintf("  ✓ Phase 2: %.1fs (memory-efficient)\n", elapsed_time))
    }
    
    # Return accumulated counts
    return(list(
      win_counts = win_counts,
      top_counts = top_counts,
      percentiles_config = percentiles_config,
      mode = "efficient",
      n_lineups = n_lineups,
      n_sims = n_sims
    ))
  }
  
  # ============================================================================
  # STANDARD MODE: Store full score matrix
  # ============================================================================
  
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
# PHASE 3: CALCULATE DISTRIBUTION METRICS (MODE-AGNOSTIC)
# =============================================================================

calculate_distribution_metrics <- function(score_matrix, lineup_data, config, 
                                           ownership_data = NULL, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 3: Calculating metrics...\n")
  
  unique_lineups <- lineup_data$unique_lineups
  n_sims <- lineup_data$n_sims
  n_lineups <- nrow(unique_lineups)
  mode <- lineup_data$mode
  
  start_time <- Sys.time()
  
  # ============================================================================
  # CHECK FORMAT: Efficient (pre-calculated counts) or Standard (full matrix)
  # ============================================================================
  
  is_efficient <- is.list(score_matrix) && !is.null(score_matrix$mode) && 
    score_matrix$mode == "efficient"
  
  if (is_efficient) {
    # EFFICIENT PATH: Use pre-calculated win/top counts
    if (verbose) {
      cat("  Using pre-calculated ranks (memory-efficient mode)\n")
    }
    
    win_counts <- score_matrix$win_counts
    top_counts <- score_matrix$top_counts
    percentiles_config <- score_matrix$percentiles_config
    
    win_rate <- (win_counts / n_sims) * 100
    top_pcts <- (top_counts / n_sims) * 100
    
  } else {
    # STANDARD PATH: Calculate from full matrix
    percentiles <- config$percentiles
    
    if (verbose) {
      cat("  Phase 3: 20%% | Calculating rankings...\n")
      flush.console()
    }
    
    # Calculate Top % - OPTIMIZED with partial sorting
    top_pcts <- matrix(0, nrow = n_lineups, ncol = length(percentiles))
    
    # Process in larger chunks with vectorization
    chunk_size <- 1000  # Process 1000 sims at once
    n_chunks <- ceiling(n_sims / chunk_size)
    
    if (verbose) {
      cat(sprintf("  Phase 3: 20%% | Calculating rankings (vectorized)...\n"))
      flush.console()
    }
    
    for (chunk_idx in 1:n_chunks) {
      chunk_start <- (chunk_idx - 1) * chunk_size + 1
      chunk_end <- min(chunk_idx * chunk_size, n_sims)
      chunk_sims <- chunk_start:chunk_end
      n_chunk_sims <- length(chunk_sims)
      
      # Get scores for this chunk: lineups × chunk_sims
      chunk_scores <- score_matrix[, chunk_sims, drop = FALSE]
      
      # VECTORIZED win rate accumulation
      max_scores <- apply(chunk_scores, 2, max)
      win_matrix <- sweep(chunk_scores, 2, max_scores, "==")
      if (chunk_idx == 1) {
        win_counts_accum <- rowSums(win_matrix)
      } else {
        win_counts_accum <- win_counts_accum + rowSums(win_matrix)
      }
      
      # Top% - use sort without partial parameter
      for (p_idx in seq_along(percentiles)) {
        threshold_rank <- ceiling(n_lineups * percentiles[p_idx])
        
        # Process each sim in chunk
        for (sim_offset in 1:n_chunk_sims) {
          sim_scores <- chunk_scores[, sim_offset]
          
          if (threshold_rank < n_lineups) {
            # Sort and get the threshold value
            sorted_scores <- sort(sim_scores, decreasing = TRUE)
            threshold_score <- sorted_scores[threshold_rank]
            top_pcts[, p_idx] <- top_pcts[, p_idx] + (sim_scores >= threshold_score)
          } else {
            top_pcts[, p_idx] <- top_pcts[, p_idx] + 1
          }
        }
      }
      
      if (verbose && chunk_idx < n_chunks) {
        pct_done <- (chunk_end / n_sims) * 40 + 20
        cat(sprintf("\r  Phase 3: %.0f%% | Ranking sims...", pct_done))
        flush.console()
      }
    }
    
    top_pcts <- (top_pcts / n_sims) * 100
    win_rate <- (win_counts_accum / n_sims) * 100
    
    if (verbose) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      cat(sprintf("\r  Phase 3: 60%% | %.1fs | Rankings complete\n", elapsed))
      flush.console()
    }
  }
  
  # ============================================================================
  # OWNERSHIP CALCULATION - VECTORIZED for massive speedup
  # ============================================================================
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  Phase 3: 70%% | %.1fs | Calculating ownership (vectorized)...\n", elapsed))
    flush.console()
  }
  
  # Ownership
  total_salary <- unique_lineups$TotalSalary
  
  # Detect player columns
  if ("Captain" %in% names(unique_lineups)) {
    player_cols <- c("Captain", grep("^Util", names(unique_lineups), value = TRUE))
    multipliers <- c(config$cpt_multiplier, rep(1, length(player_cols) - 1))
  } else if ("MVP" %in% names(unique_lineups)) {
    player_cols <- c("MVP", grep("^Player", names(unique_lineups), value = TRUE))
    multipliers <- c(config$mvp_multiplier, rep(1, length(player_cols) - 1))
  } else {
    player_cols <- grep("^Player", names(unique_lineups), value = TRUE)
    multipliers <- rep(1, length(player_cols))
  }
  
  cumulative_own <- rep(0, n_lineups)
  geometric_own <- rep(0, n_lineups)
  
  if (!is.null(ownership_data) && nrow(ownership_data) > 0) {
    setDT(ownership_data)
    
    if (!"Player" %in% names(ownership_data) && "Name" %in% names(ownership_data)) {
      setnames(ownership_data, "Name", "Player")
    }
    
    has_own <- any(c("Own", "DKOwn", "FDOwn") %in% names(ownership_data))
    
    if (has_own) {
      own_col <- NULL
      if ("Own" %in% names(ownership_data)) {
        own_col <- "Own"
      } else if ("DKOwn" %in% names(ownership_data)) {
        own_col <- "DKOwn"
      } else if ("FDOwn" %in% names(ownership_data)) {
        own_col <- "FDOwn"
      }
      
      # ====================================================================
      # VECTORIZED OWNERSHIP CALCULATION
      # Instead of looping through lineups, create ownership lookup matrix
      # ====================================================================
      
      # Create fast lookup: player name -> ownership
      setkey(ownership_data, Player)
      
      # Extract all players from all lineups into matrix form
      # This creates a lineups × positions matrix of player names
      player_matrix <- as.matrix(unique_lineups[, ..player_cols])
      
      # Vectorized lookup: replace player names with ownership values
      # Using match() which is very fast
      all_players_flat <- as.vector(player_matrix)
      ownership_lookup <- ownership_data[[own_col]]
      names(ownership_lookup) <- ownership_data$Player
      
      # Get ownership for all players in all lineups (vectorized)
      ownership_flat <- ownership_lookup[all_players_flat]
      ownership_flat[is.na(ownership_flat)] <- 0  # Handle missing players
      
      # Reshape back to matrix: lineups × positions
      ownership_matrix <- matrix(ownership_flat, nrow = n_lineups, ncol = length(player_cols))
      
      # Apply multipliers (for Captain/MVP modes)
      multiplier_matrix <- matrix(rep(multipliers, each = n_lineups), nrow = n_lineups)
      weighted_ownership <- ownership_matrix * multiplier_matrix
      
      # CUMULATIVE OWNERSHIP: Just sum across positions (vectorized!)
      cumulative_own <- rowSums(weighted_ownership)
      
      # GEOMETRIC MEAN OWNERSHIP: 
      # Geometric mean = exp(mean(log(x))) for x > 0
      # Handle zeros and NAs properly
      
      # Replace zeros with NA for geometric mean calculation
      ownership_for_geomean <- ownership_matrix
      ownership_for_geomean[ownership_for_geomean <= 0] <- NA
      
      # For positions with multipliers > 1, replicate the ownership values
      if (any(multipliers > 1)) {
        # Create expanded matrix for geometric mean (accounts for multipliers)
        max_mult <- max(multipliers)
        expanded_cols <- sum(multipliers)
        expanded_ownership <- matrix(NA, nrow = n_lineups, ncol = expanded_cols)
        
        col_idx <- 1
        for (pos_idx in seq_along(player_cols)) {
          mult <- multipliers[pos_idx]
          own_vals <- ownership_matrix[, pos_idx]
          for (m in 1:mult) {
            expanded_ownership[, col_idx] <- own_vals
            col_idx <- col_idx + 1
          }
        }
        ownership_for_geomean <- expanded_ownership
      }
      
      # Calculate geometric mean: exp(mean(log(x))) for each lineup
      # Use rowMeans with na.rm=TRUE to handle NAs
      log_ownership <- log(ownership_for_geomean)
      mean_log_ownership <- rowMeans(log_ownership, na.rm = TRUE)
      geometric_own <- exp(mean_log_ownership)
      
      # Handle cases where all values were NA/zero
      geometric_own[is.na(geometric_own) | is.infinite(geometric_own)] <- 0
    }
  }
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  Phase 3: 90%% | %.1fs | Assembling results...\n", elapsed))
    flush.console()
  }
  
  # Combine results
  results <- data.table(
    unique_lineups[, ..player_cols],
    WinRate = win_rate,
    Top1Pct = top_pcts[, 1],
    Top5Pct = top_pcts[, 2],
    Top10Pct = top_pcts[, 3],
    Top20Pct = top_pcts[, 4],
    TotalSalary = total_salary,
    CumulativeOwnership = cumulative_own * 100,
    GeometricMeanOwnership = geometric_own * 100
  )
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (verbose) {
    cat(sprintf("  ✓ Phase 3: %.1fs\n", elapsed_time))
  }
  
  return(results)
}