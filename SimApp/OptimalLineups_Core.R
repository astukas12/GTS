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
  } else if (mode == "win_based") {
    return(find_optimal_lineups_winbased(sim_results, config, verbose))
  } else if (mode == "combinatorial") {
    return(find_optimal_lineups_combinatorial(sim_results, config, verbose))
  } else if (mode == "combinatorial_captain") {
    return(find_optimal_lineups_combinatorial_captain(sim_results, config, verbose))
  } else if (mode == "combinatorial_mvp") {
    return(find_optimal_lineups_combinatorial_mvp(sim_results, config, verbose))
  } else if (mode == "preseason_classic") {
    return(find_optimal_lineups_preseason_classic(sim_results, config, k, verbose))
  } else if (mode == "cfb_classic") {
    return(find_optimal_lineups_cfb_classic(sim_results, config, verbose))
  } else {
    stop(paste("Unknown mode:", mode,
               "- must be 'standard', 'mvp', 'captain', 'win_based',",
               "'combinatorial', 'combinatorial_captain', or 'combinatorial_mvp'"))
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
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  use_parallel <- if (!is.null(config$use_parallel)) config$use_parallel else TRUE
  
  sim_ids <- unique(sim_results$SimID)
  n_sims <- length(sim_ids)
  
  if (verbose) {
    cat(sprintf("  %s sims | top %d per sim\n",
                format(n_sims, big.mark = ","), k))
  }
  
  start_time <- Sys.time()
  
  # Helper function for one sim
  find_top_k_for_sim <- function(sim_data, roster_size, salary_cap, k) {
    n_players <- nrow(sim_data)
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
    clusterExport(cl, c("find_top_k_for_sim", "roster_size", "salary_cap", "k"), 
                  envir = environment())
    clusterExport(cl, "sim_results", envir = environment())
    
    all_lineups <- parLapply(cl, sim_ids, function(sid) {
      sim_data <- sim_results[SimID == sid]
      find_top_k_for_sim(sim_data, roster_size, salary_cap, k)
    })
    
    stopCluster(cl)
    all_lineups <- all_lineups[!sapply(all_lineups, is.null)]
    
  } else {
    all_lineups <- list()
    
    for (i in seq_along(sim_ids)) {
      sim_id <- sim_ids[i]
      sim_data <- sim_results[SimID == sim_id]
      
      sim_lineups <- find_top_k_for_sim(sim_data, roster_size, salary_cap, k)
      
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
  
  # Track which rank each lineup achieved in each sim (1st, 2nd, or 3rd optimal)
  # Assuming lineups are ordered by TotalScore within each SimID from Phase 1
  all_lineups_dt[, rank_in_sim := seq_len(.N), by = SimID]
  
  # Aggregate counts by lineup
  lineup_stats <- all_lineups_dt[, .(
    Top1Count = sum(rank_in_sim == 1),  # How many times was #1 optimal
    Top2Count = sum(rank_in_sim <= 2),  # How many times in top 2
    Top3Count = .N,                      # Total times it appeared (all top 3)
    AvgScore = mean(TotalScore),
    MaxSalary = max(TotalSalary)
  ), by = lineup_sig]
  
  # Get unique lineups and merge with stats
  unique_lineups <- all_lineups_dt[!duplicated(lineup_sig)]
  unique_lineups <- merge(unique_lineups, lineup_stats, by = "lineup_sig")
  unique_lineups[, c("lineup_sig", "rank_in_sim") := NULL]
  
  if (nrow(unique_lineups) > max_lineups) {
    if (verbose) cat(sprintf("  Capping at %s lineups\n", format(max_lineups, big.mark = ",")))
    unique_lineups <- unique_lineups[1:max_lineups]
  }
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (verbose) {
    cat(sprintf("  ✓ Phase 1: %s lineups | %.1fs\n",
                format(nrow(unique_lineups), big.mark = ","), elapsed_time))
  }
  
  # ============================================================================
  # PHASE 1.5: Rank and Filter to Top 25k
  # Sort by: Top1Count (ties → Top2Count, ties → Top3Count)
  # ============================================================================
  
  target_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  
  if (nrow(unique_lineups) > target_lineups) {
    if (verbose) cat(sprintf("\n  Phase 1.5: Ranking and filtering to top %s...\n", format(target_lineups, big.mark = ",")))
    
    phase15_start <- Sys.time()
    
    # Sort by optimal counts: Top1 first, then Top2 for ties, then Top3 for ties
    setorder(unique_lineups, -Top1Count, -Top2Count, -Top3Count)
    
    # Keep top lineups
    unique_lineups <- unique_lineups[1:target_lineups]
    
    if (verbose) {
      elapsed_15 <- as.numeric(difftime(Sys.time(), phase15_start, units = "secs"))
      cat(sprintf("  ✓ Phase 1.5: %.1fs\n", elapsed_15))
    }
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
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
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
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
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
# MODE 5: COMBINATORIAL - Standard (no LP, score every valid salary-legal lineup)
# Best for small player pools (MMA, small tennis slates) where C(n, roster) is
# manageable. Finds better lineups than LP because it evaluates the full universe
# instead of only per-sim winners. Phase 2 handles all scoring via matrix multiply.
# =============================================================================

find_optimal_lineups_combinatorial <- function(sim_results, config, verbose = TRUE) {
  # Per-sim greedy optimal: for each sim, sort players by score and greedily
  # pick the best roster_size players that fit under salary cap.
  # Greedy IS optimal here — simple knapsack with only count + salary constraints.
  # Dedupe across all sims → unique lineups ranked by how often each was #1.
  # Far fewer unique lineups than combinatorial explosion (~100-2000 for MMA).
  
  if (verbose) cat("\nPhase 1: Finding optimal lineup per sim (greedy)...\n")
  
  setDT(sim_results)
  roster_size <- config$roster_size
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  start_time  <- Sys.time()
  
  players_dt <- unique(sim_results[, .(Player, Salary)])[Salary > 0 & !is.na(Salary)]
  sal_lookup <- setNames(players_dt$Salary, players_dt$Player)
  sim_ids    <- unique(sim_results$SimID)
  n_sims     <- length(sim_ids)
  
  if (verbose) cat(sprintf("  %d players | %s sims | $%s cap\n",
                           nrow(players_dt), format(n_sims, big.mark=","),
                           format(salary_cap, big.mark=",")))
  
  # setkey for fast per-sim lookup
  setkey(sim_results, SimID)
  prog_freq <- max(1L, n_sims %/% 10L)
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid      <- sim_ids[i]
    sim_data <- sim_results[.(sid)][Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints)]
    setorder(sim_data, -FantasyPoints)
    
    # Greedy: pick highest-scoring players that fit under cap
    picked   <- character(roster_size)
    n_picked <- 0L
    sal_used <- 0
    
    for (j in seq_len(nrow(sim_data))) {
      if (n_picked == roster_size) break
      p   <- sim_data$Player[j]
      sal <- sim_data$Salary[j]
      if (sal_used + sal <= salary_cap) {
        n_picked          <- n_picked + 1L
        picked[n_picked]  <- p
        sal_used          <- sal_used + sal
      }
    }
    
    if (n_picked == roster_size) {
      picked_sorted <- sort(picked)   # canonical order for dedup
      lineup_list[[i]] <- data.table(
        Lineup      = paste(picked_sorted, collapse = "|"),
        TotalSalary = sal_used,
        TotalScore  = sum(sim_data$FantasyPoints[sim_data$Player %in% picked])
      )
      # Store player columns in sorted order
      for (k in seq_len(roster_size)) lineup_list[[i]][[paste0("Player", k)]] <- picked_sorted[k]
    }
    
    if (verbose && i %% prog_freq == 0L) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units="secs"))
      cat(sprintf("\r  Phase 1: %d%% | %.1fs", round(i/n_sims*100), elapsed))
      flush.console()
    }
  }
  if (verbose) cat("\n")
  
  all_lineups <- rbindlist(lineup_list[!sapply(lineup_list, is.null)])
  
  # Count how often each unique lineup was #1 optimal
  counts <- all_lineups[, .(Top1Count = .N,
                            TotalSalary = TotalSalary[1],
                            AvgScore    = mean(TotalScore)),
                        by = Lineup]
  setorder(counts, -Top1Count)
  
  if (nrow(counts) > max_lineups) counts <- counts[1:max_lineups]
  
  # Expand Lineup string back to Player columns
  player_mat <- do.call(rbind, strsplit(counts$Lineup, "\\|"))
  unique_lineups <- as.data.table(player_mat)
  setnames(unique_lineups, paste0("Player", seq_len(roster_size)))
  unique_lineups[, TotalSalary := counts$TotalSalary]
  unique_lineups[, Top1Count   := counts$Top1Count]
  unique_lineups[, AvgScore    := counts$AvgScore]
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units="secs"))
  if (verbose) cat(sprintf("  ✓ Phase 1: %s unique lineups from %s sims | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","),
                           format(n_sims, big.mark=","), elapsed_time))
  
  list(unique_lineups = unique_lineups, n_sims = n_sims, config = config,
       mode = "combinatorial")
}


# =============================================================================
# MODE 6: COMBINATORIAL CAPTAIN (Showdown / DK Captain format)
# Iterates each player as Captain (1.5x score, 1.5x salary cost) + 5 Util.
# =============================================================================

find_optimal_lineups_combinatorial_captain <- function(sim_results, config, verbose = TRUE) {
  # Vectorized captain format optimizer.
  # Outer loop: n_players CPT candidates (~14) — tiny.
  # Inner work: vectorized across all sims simultaneously using matrix ops.
  # ~50-100x faster than the original nested sim×CPT×util triple loop.
  
  if (verbose) cat("\nPhase 1: Finding optimal lineup per sim (greedy captain)...\n")
  
  setDT(sim_results)
  roster_size    <- config$roster_size
  salary_cap     <- config$salary_cap
  cpt_multiplier <- if (!is.null(config$cpt_multiplier)) config$cpt_multiplier else 1.5
  max_lineups    <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  n_utils        <- roster_size - 1L
  start_time     <- Sys.time()
  
  players_dt <- unique(sim_results[Salary > 0 & !is.na(Salary), .(Player, Salary)])
  all_players <- players_dt$Player
  salaries    <- players_dt$Salary
  n_players   <- nrow(players_dt)
  
  sim_ids <- unique(sim_results$SimID)
  n_sims  <- length(sim_ids)
  
  if (verbose) cat(sprintf("  %d players | %s sims | $%s cap | %.1fx captain\n",
                           n_players, format(n_sims, big.mark=","),
                           format(salary_cap, big.mark=","), cpt_multiplier))
  
  # Score matrix: n_players x n_sims
  # Collapse to one score per player per sim before pivoting (engine may emit
  # multiple rows per player per sim in some configurations)
  sim_results_clean <- sim_results[Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints),
                                   .(FantasyPoints = mean(FantasyPoints, na.rm = TRUE)),
                                   by = .(Player, SimID)]
  score_wide <- dcast(sim_results_clean, Player ~ SimID,
                      value.var = "FantasyPoints", fun.aggregate = mean, fill = 0)
  score_wide <- score_wide[match(all_players, Player)]
  score_mat  <- as.matrix(score_wide[, -1L, with = FALSE])  # n_players x n_sims
  
  # State tracking: best score and lineup per sim
  best_score  <- rep(-Inf, n_sims)
  best_cap    <- character(n_sims)
  best_utils  <- matrix(NA_character_, nrow = n_utils, ncol = n_sims)
  best_sal    <- numeric(n_sims)
  
  # Outer loop: n_players CPT candidates (14 for MMA showdown — negligible)
  for (ci in seq_len(n_players)) {
    cpt_sal <- salaries[ci] * cpt_multiplier
    if (cpt_sal > salary_cap) next
    
    rem_cap       <- salary_cap - cpt_sal
    cpt_score_vec <- score_mat[ci, ] * cpt_multiplier  # n_sims vector
    
    # Util pool
    ui       <- seq_len(n_players)[-ci]
    u_sal    <- salaries[ui]
    u_mat    <- score_mat[ui, , drop = FALSE]   # (n_players-1) x n_sims
    n_u      <- length(ui)
    
    # For each sim, sort util players by score descending and greedily fill.
    # We do this rank-by-rank across ALL sims simultaneously:
    # rank r: for each sim, which util player is rank r?
    ord_mat <- apply(u_mat, 2L, function(x) order(x, decreasing = TRUE))
    # ord_mat: n_u x n_sims, ord_mat[r,s] = index in ui of r-th best in sim s
    
    n_picked  <- integer(n_sims)
    sal_used  <- numeric(n_sims)
    score_acc <- numeric(n_sims)
    pick_mat  <- matrix(NA_character_, nrow = n_utils, ncol = n_sims)
    
    for (r in seq_len(n_u)) {
      done <- (n_picked == n_utils)
      if (all(done)) break
      active <- which(!done)
      
      j_vec <- ord_mat[r, active]          # util index for each active sim
      add   <- u_sal[j_vec] + sal_used[active] <= rem_cap
      take  <- active[add]
      jt    <- j_vec[add]
      
      if (length(take)) {
        n_picked[take]  <- n_picked[take] + 1L
        sal_used[take]  <- sal_used[take] + u_sal[jt]
        # Vectorized score lookup across (util_player, sim) pairs
        score_acc[take] <- score_acc[take] +
          u_mat[cbind(jt, take)]
        # Store player names
        for (ii in seq_along(take)) {
          pick_mat[n_picked[take[ii]], take[ii]] <- all_players[ui[jt[ii]]]
        }
      }
    }
    
    # Sims with complete lineups
    complete <- which(n_picked == n_utils)
    if (!length(complete)) next
    
    total <- cpt_score_vec[complete] + score_acc[complete]
    better <- complete[total > best_score[complete]]
    if (!length(better)) next
    
    best_score[better] <- total[match(better, complete)]
    best_cap[better]   <- all_players[ci]
    best_sal[better]   <- cpt_sal + sal_used[better]
    best_utils[, better] <- pick_mat[, better]
  }
  
  # Build result table
  has_lineup <- which(!is.na(best_cap) & best_cap != "")
  lineup_list <- lapply(has_lineup, function(s) {
    utils_s <- sort(na.omit(best_utils[, s]))
    sig <- paste(c(best_cap[s], utils_s), collapse = "|")
    row <- data.table(Lineup = sig, TotalSalary = best_sal[s],
                      TotalScore = best_score[s])
    row[, Captain := best_cap[s]]
    for (k in seq_len(n_utils)) row[[paste0("Util", k)]] <- utils_s[k]
    row
  })
  
  all_lineups <- rbindlist(lineup_list)
  
  counts <- all_lineups[, .(Top1Count   = .N,
                            TotalSalary = TotalSalary[1],
                            AvgScore    = mean(TotalScore)),
                        by = Lineup]
  setorder(counts, -Top1Count)
  if (nrow(counts) > max_lineups) counts <- counts[1:max_lineups]
  
  parts <- strsplit(counts$Lineup, "\\|")
  unique_lineups <- data.table(Captain = sapply(parts, `[`, 1))
  for (k in seq_len(n_utils)) {
    unique_lineups[[paste0("Util", k)]] <- sapply(parts, `[`, k + 1L)
  }
  unique_lineups[, TotalSalary := counts$TotalSalary]
  unique_lineups[, Top1Count   := counts$Top1Count]
  unique_lineups[, AvgScore    := counts$AvgScore]
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (verbose) cat(sprintf("  \u2713 Phase 1: %s unique lineups from %s sims | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","),
                           format(n_sims, big.mark=","), elapsed_time))
  
  list(unique_lineups = unique_lineups, n_sims = n_sims, config = config,
       mode = "combinatorial_captain")
}


find_optimal_lineups_combinatorial_mvp <- function(sim_results, config, verbose = TRUE) {
  # FD MVP format: highest scorer per sim IS the MVP.
  # Score is at 1.5x but salary counts at face value — same ID whether MVP or flex.
  # So the optimal lineup is simply: pick the top scorer as MVP, then greedily
  # fill 5 flex slots with the next-highest scorers under the remaining salary cap.
  # No inner loop over candidates needed.
  
  if (verbose) cat("\nPhase 1: Finding optimal lineup per sim (greedy MVP)...\n")
  
  setDT(sim_results)
  roster_size    <- config$roster_size
  salary_cap     <- config$salary_cap
  mvp_multiplier <- if (!is.null(config$mvp_multiplier)) config$mvp_multiplier else 1.5
  max_lineups    <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  n_flex         <- roster_size - 1L
  start_time     <- Sys.time()
  
  players_dt <- unique(sim_results[, .(Player, Salary)])[Salary > 0 & !is.na(Salary)]
  sim_ids    <- unique(sim_results$SimID)
  n_sims     <- length(sim_ids)
  
  if (verbose) cat(sprintf("  %d players | %s sims | $%s cap | %.1fx MVP score\n",
                           nrow(players_dt), format(n_sims, big.mark=","),
                           format(salary_cap, big.mark=","), mvp_multiplier))
  
  setkey(sim_results, SimID)
  prog_freq <- max(1L, n_sims %/% 10L)
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid      <- sim_ids[i]
    sim_data <- sim_results[.(sid)][Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints)]
    if (nrow(sim_data) < roster_size) next
    
    # Sort by score descending — highest scorer is always the MVP
    setorder(sim_data, -FantasyPoints)
    scores  <- sim_data$FantasyPoints
    sals    <- sim_data$Salary
    players <- sim_data$Player
    n_p     <- nrow(sim_data)
    
    # Row 1 (highest scorer) is MVP; salary at face value toward cap
    mvp_sal <- sals[1]
    if (mvp_sal > salary_cap) next
    rem_cap <- salary_cap - mvp_sal
    
    # Greedy fill 5 flex from remaining players under remaining cap
    picked_f   <- character(n_flex)
    n_picked   <- 0L
    sal_used   <- 0
    flex_score <- 0
    
    for (j in 2:n_p) {
      if (n_picked == n_flex) break
      if (sal_used + sals[j] <= rem_cap) {
        n_picked           <- n_picked + 1L
        picked_f[n_picked] <- players[j]
        sal_used           <- sal_used + sals[j]
        flex_score         <- flex_score + scores[j]
      }
    }
    
    if (n_picked == n_flex) {
      total_score <- scores[1] * mvp_multiplier + flex_score
      sig <- paste(c(players[1], sort(picked_f)), collapse = "|")
      row <- data.table(Lineup = sig, TotalSalary = mvp_sal + sal_used,
                        TotalScore = total_score)
      row[, MVP := players[1]]
      for (k in seq_len(n_flex)) row[[paste0("Player", k)]] <- sort(picked_f)[k]
      lineup_list[[i]] <- row
    }
    
    if (verbose && i %% prog_freq == 0L) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units="secs"))
      cat(sprintf("\r  Phase 1: %d%% | %.1fs", round(i/n_sims*100), elapsed))
      flush.console()
    }
  }
  if (verbose) cat("\n")
  
  all_lineups <- rbindlist(lineup_list[!sapply(lineup_list, is.null)])
  
  counts <- all_lineups[, .(Top1Count   = .N,
                            TotalSalary = TotalSalary[1],
                            AvgScore    = mean(TotalScore)),
                        by = Lineup]
  setorder(counts, -Top1Count)
  if (nrow(counts) > max_lineups) counts <- counts[1:max_lineups]
  
  parts <- strsplit(counts$Lineup, "\\|")
  unique_lineups <- data.table(MVP = sapply(parts, `[`, 1))
  for (k in seq_len(n_flex)) {
    unique_lineups[[paste0("Player", k)]] <- sapply(parts, `[`, k + 1L)
  }
  unique_lineups[, TotalSalary := counts$TotalSalary]
  unique_lineups[, Top1Count   := counts$Top1Count]
  unique_lineups[, AvgScore    := counts$AvgScore]
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units="secs"))
  if (verbose) cat(sprintf("  ✓ Phase 1: %s unique lineups from %s sims | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","),
                           format(n_sims, big.mark=","), elapsed_time))
  
  list(unique_lineups = unique_lineups, n_sims = n_sims, config = config,
       mode = "combinatorial_mvp")
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
  
  # Get score column — check lineup_data directly first, then nested config
  platform_col <- if (!is.null(lineup_data$platform_col)) lineup_data$platform_col
  else if (!is.null(config$platform_col)) config$platform_col
  else "DKScore"
  
  # Create player-to-index mapping
  all_players <- unique(unlist(unique_lineups[, ..player_cols]))
  player_to_id <- setNames(1:length(all_players), all_players)

  # SLOT INDEX, not a wide indicator matrix. `slot_idx` is n_lineups x n_slots
  # holding each lineup's player ids. Built by one vectorised lookup instead of
  # a row-by-row loop -- `unique_lineups[i, ..player_cols]` is a full data.table
  # subset call per lineup, which at 20k lineups dominated the setup.
  slot_idx <- matrix(player_to_id[unlist(unique_lineups[, ..player_cols])],
                     nrow = n_lineups)

  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  Phase 2: 20%% | %.1fs | Organizing sim data...\n", elapsed))
    flush.console()
  }
  
  # PLAYER SCORES AS ONE DENSE MATRIX, players x sims, built in a single pass.
  # The old code ran `sim_results[Player == player_name]` once per player, and
  # each of those is a full linear scan of a table holding n_players * n_sims
  # rows -- 30M rows at 100k sims, scanned ~300 times. It then REBUILT a slice
  # of the same thing inside every batch. One indexed assignment replaces both.
  # This matrix is the small one (players x sims); the batching below exists for
  # the lineup x sims matrix, which is orders of magnitude larger.
  sub <- sim_results[Player %chin% all_players,
                     .(pi = player_to_id[Player], si = SimID, S = get(platform_col))]
  sub <- sub[!is.na(pi) & si >= 1L & si <= n_sims]
  player_scores <- matrix(0, nrow = length(all_players), ncol = n_sims)
  player_scores[cbind(sub$pi, sub$si)] <- sub$S
  rm(sub)

  # Score a block of sims: gather each slot's row and add. Every lineup has only
  # n_slots players, so summing n_slots gathers does far less work than the old
  # dense (lineups x players) %*% (players x sims) product, which multiplied
  # through a matrix that is ~97% zeros.
  # Rounded to 6dp because RANKING IS TIE-SENSITIVE. Fantasy scores land on
  # exact ties constantly (2% of sims here have a tied winner), and summing the
  # same six numbers in a different order moves the total by ~3e-14 -- enough to
  # break a tie that should hold. Rounding well below the 0.01 scoring
  # granularity makes ties exact again, so results no longer depend on summation
  # order, BLAS build, or batch size.
  score_block <- function(sim_ids) {
    out <- multipliers[1] * player_scores[slot_idx[, 1], sim_ids, drop = FALSE]
    for (j in seq_along(multipliers)[-1])
      out <- out + multipliers[j] * player_scores[slot_idx[, j], sim_ids, drop = FALSE]
    round(out, 6)
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

      batch_lineup_scores <- score_block(batch_sim_ids)

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
        
        # ONE sort per sim, not one per sim PER PERCENTILE. The old loop nesting
        # was percentile-outside-sim, so every sim was fully sorted four times
        # over -- 400k sorts of a 20k vector at 100k sims. The thresholds are
        # four positions in the SAME ordering, so a single sort answers all of
        # them and the counts are bit-for-bit identical.
        thr_ranks <- ceiling(n_lineups * percentiles_config)
        for (sim_offset in 1:rb_size) {
          sim_scores <- mini_batch_scores[, sim_offset]
          need_sort <- any(thr_ranks < n_lineups)
          sorted_scores <- if (need_sort) sort(sim_scores, decreasing = TRUE) else NULL
          for (p_idx in seq_along(percentiles_config)) {
            if (thr_ranks[p_idx] < n_lineups) {
              top_counts[, p_idx] <- top_counts[, p_idx] +
                (sim_scores >= sorted_scores[thr_ranks[p_idx]])
            } else {
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
      
      rm(batch_lineup_scores)
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

    batch_lineup_scores <- score_block(batch_sim_ids)
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
    
    rm(batch_lineup_scores)
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
    
    if (verbose) { cat("  Phase 3: Ranking lineups...\n"); flush.console() }
    
    # Pre-compute threshold ranks for each percentile
    # matrixStats does the per-column max and order statistic in C. Nothing else
    # here depends on it, so fall back to base when absent -- slower, identical.
    .has_ms  <- requireNamespace("matrixStats", quietly = TRUE)
    .colMaxs <- if (.has_ms) matrixStats::colMaxs else function(x) apply(x, 2L, max)
    .colOrd  <- if (.has_ms) matrixStats::colOrderStats else
                function(x, which) apply(x, 2L, function(v) sort(v, partial = which)[which])

    threshold_ranks  <- ceiling(n_lineups * percentiles)
    top_pcts         <- matrix(0L, nrow = n_lineups, ncol = length(percentiles))
    win_counts_accum <- integer(n_lineups)
    
    # Chunk size: ~150MB per chunk (10k lineups * 2000 sims * 8 bytes)
    chunk_size   <- 2000L
    n_chunks     <- ceiling(n_sims / chunk_size)
    phase3_start <- Sys.time()
    
    for (chunk_idx in seq_len(n_chunks)) {
      chunk_start  <- (chunk_idx - 1L) * chunk_size + 1L
      chunk_end    <- min(chunk_idx * chunk_size, n_sims)
      chunk_scores <- score_matrix[, chunk_start:chunk_end, drop = FALSE]
      
      # NO FULL RANKING. "rank(-x, ties='min') <= k" is exactly "x >= the k-th
      # largest value in that sim", so a per-column ORDER STATISTIC answers every
      # percentile without sorting. That drops an O(n log n) sort per sim to O(n)
      # and stops materialising an n_lineups x chunk rank matrix. Measured on a
      # 5,000-lineup pool over 50,000 sims this took the ranking step from ~84s
      # to a few seconds, and the counts are identical (ties at the boundary land
      # the same way under both forms).
      col_max          <- .colMaxs(chunk_scores)
      win_counts_accum <- win_counts_accum +
        rowSums(chunk_scores == rep(col_max, each = n_lineups))

      for (p_idx in seq_along(percentiles)) {
        k <- threshold_ranks[p_idx]
        if (k >= n_lineups) {
          top_pcts[, p_idx] <- top_pcts[, p_idx] + ncol(chunk_scores)
        } else {
          kth <- .colOrd(chunk_scores, which = n_lineups - k + 1L)
          top_pcts[, p_idx] <- top_pcts[, p_idx] +
            rowSums(chunk_scores >= rep(kth, each = n_lineups))
        }
      }
      
      if (verbose && chunk_idx %% max(1L, n_chunks %/% 5L) == 0L) {
        elapsed  <- as.numeric(difftime(Sys.time(), phase3_start, units = "secs"))
        pct_done <- round(chunk_end / n_sims * 40 + 20)
        cat(sprintf("\r  Phase 3: %d%% | %.1fs", pct_done, elapsed))
        flush.console()
      }
    }
    
    top_pcts <- (top_pcts / n_sims) * 100
    win_rate <- (win_counts_accum / n_sims) * 100
    
    if (verbose) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      cat(sprintf("\r  Phase 3: 60%% | %.1fs | Done\n", elapsed))
      flush.console()
    }
  }
  
  # ============================================================================
  # OWNERSHIP CALCULATION - VECTORIZED for massive speedup
  # ============================================================================
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("  Phase 3: 70%% | %.1fs | Calculating ownership...\n", elapsed))
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
    AvgOwn = geometric_own * 100
  )
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (verbose) {
    cat(sprintf("  ✓ Phase 3: %.1fs\n", elapsed_time))
  }
  
  return(results)
}




# -----------------------------------------------------------------------------
# find_optimal_lineups_winbased -- Tennis pool construction, three stages.
#
# WHY IT IS NOT ONE STAGE. The old version built every lineup at once with
# combn(simplify = FALSE) and then ranked them. On a 34-match slate that is
# C(68,6) = 109,453,344 six-name character vectors, roughly 48GB, so the pool
# simply could not be built -- the wall is around 24 matches. Nothing about the
# ranking needed the whole set in memory at once, only the winners did.
#
# STAGE 1 walks the same combinations one first-player slice at a time, scores
# each slice vectorised, and keeps a running top gate_size. Every combination is
# still visited; none are held. Memory is flat in slate size -- 0.1s at 26
# players, 66s at 68, against not running at all.
#
# STAGE 2 is the change in what gets played. Stage 1 is only a GATE: its job is
# to put the good lineups in the room, not to pick them. The picking happens
# here, on real simulated scores, by how often a lineup lands in the top
# tail_frac of the field.
#
# WHY THE GATE RANKS ON MEAN SCORE AND NOT ON WINS. Measured on 2026-09-01
# (68 players, 4.4M salary-feasible lineups), recall of the 5,000 best lineups
# by top-0.1% hit rate:
#
#             N=5k   N=25k   N=100k   N=200k
#   MeanScore 28.8%   70.7%   91.5%    93.9%
#   P(>350)   28.9%   70.1%   92.2%    93.9%
#   EW        15.0%   43.2%   79.9%    92.1%
#   Win5plus   5.5%   20.0%   53.1%    79.0%
#   Win6       1.1%    5.8%   29.4%    67.2%
#
# Expected wins correlates .994 with a mean player score but only .62 once
# divided by salary, which is what a binding cap actually spends on -- a losing
# tennis player still banks 23 of a winner's 67 points, and EW cannot see that.
# Win6 is worst of all: maximising P(all six win) chases six favourites the cap
# cannot afford. Both stay as reported columns; they are good filters and bad
# gates.
#
# Variance-seeking gates do not help either, and the reason is structural: over
# all feasible lineups the mean spans 197-342 (sd 22.9) while the lineup sd
# spans only 41.5-57.7 (sd 2.16), uncorrelated with the mean. Six independent
# players from six independent matches leaves construction moving location
# about 11x more than shape. There is no boom-or-bust tennis lineup to find, so
# a mean-shaped gate is a tail-shaped gate here. That is a tennis fact and does
# not carry to sports where stacking creates correlation.
#
# GATE SIZE MATTERS MORE THAN GATE METRIC: EW at 200k beats mean score at 25k.
# Gate loosely, let the simulation decide.
#
# STAGE 3 reports ExpectedWins, Win6Pct and Win5PlusPct on the surviving pool,
# from the simulation, exactly as before. They are returned in pool order
# because add_custom_metrics() in app.R assigns them by position.
#
# config knobs, all optional: max_lineups (5000), gate_size (100000),
# gate_sims (5000), tail_frac (0.001), avoid_same_match (TRUE),
# salary_buffers (c(1000, 2000, 5000, 10000, salary_cap)), player_match
# (named vector Player -> Match; without it the same-match rule is skipped).
# -----------------------------------------------------------------------------

find_optimal_lineups_winbased <- function(sim_results, config, verbose = TRUE) {

  if (verbose) cat("\nPhase 1: Generating lineups (WIN-BASED mode)...\n")

  setDT(sim_results)

  roster_size <- config$roster_size
  salary_cap  <- config$salary_cap
  target_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000
  gate_size   <- if (!is.null(config$gate_size)) config$gate_size else 100000L
  gate_sims   <- if (!is.null(config$gate_sims)) config$gate_sims else 5000L
  tail_frac   <- if (!is.null(config$tail_frac)) config$tail_frac else 0.001
  avoid_same_match <- if (!is.null(config$avoid_same_match)) config$avoid_same_match else TRUE
  salary_buffers <- if (!is.null(config$salary_buffers)) config$salary_buffers
                    else c(1000, 2000, 5000, 10000, salary_cap)

  # Per-player statistics. Player order here defines the integer indices used
  # everywhere below, so it is fixed once and never re-sorted.
  players_dt <- sim_results[, .(Salary  = Salary[1],
                                MeanPts = mean(FantasyPoints),
                                WinProb = mean(Win)), by = Player]
  setorder(players_dt, Player)
  players   <- players_dt$Player
  n_players <- length(players)
  n_sims    <- length(unique(sim_results$SimID))

  if (n_players < roster_size) stop("Fewer players than roster spots")

  sal  <- players_dt$Salary
  mpts <- players_dt$MeanPts

  # Same-match lookup. Two players in one match cannot both score well, so the
  # pair is close to dead weight; excluded where the slate leaves room for it.
  match_id <- NULL
  if (avoid_same_match && !is.null(config$player_match)) {
    pm <- config$player_match[players]
    if (!anyNA(pm)) match_id <- as.integer(factor(pm))
  }
  pair_idx <- combn(roster_size, 2)

  # ---------------------------------------------------------------------------
  # STAGE 1: stream every combination, keep a running top gate_size by mean pts
  # ---------------------------------------------------------------------------
  stream_gate <- function(min_salary, use_match) {
    best_i <- matrix(integer(0), nrow = roster_size, ncol = 0)
    best_s <- numeric(0)
    n_feas <- 0
    prune  <- 3L * gate_size

    for (i in seq_len(n_players - roster_size + 1L)) {
      rest <- (i + 1L):n_players
      if (length(rest) < roster_size - 1L) next

      cb  <- combn(rest, roster_size - 1L)
      tot <- sal[i] + colSums(matrix(sal[cb], nrow = roster_size - 1L))
      ok  <- which(tot >= min_salary & tot <= salary_cap)
      if (length(ok) == 0L) next

      ci <- rbind(i, cb[, ok, drop = FALSE])

      if (use_match && !is.null(match_id)) {
        mm  <- matrix(match_id[ci], nrow = roster_size)
        bad <- logical(ncol(mm))
        for (p in seq_len(ncol(pair_idx)))
          bad <- bad | (mm[pair_idx[1, p], ] == mm[pair_idx[2, p], ])
        ci <- ci[, !bad, drop = FALSE]
        if (ncol(ci) == 0L) next
      }

      n_feas <- n_feas + ncol(ci)
      best_i <- cbind(best_i, ci)
      best_s <- c(best_s, colSums(matrix(mpts[ci], nrow = roster_size)))

      if (length(best_s) > prune) {
        keep   <- order(best_s, decreasing = TRUE)[seq_len(gate_size)]
        best_i <- best_i[, keep, drop = FALSE]
        best_s <- best_s[keep]
      }
    }

    if (length(best_s) == 0L) return(NULL)
    keep <- order(best_s, decreasing = TRUE)[seq_len(min(gate_size, length(best_s)))]
    list(idx = best_i[, keep, drop = FALSE], n_feasible = n_feas)
  }

  # Salary floor first, then the same-match rule, each relaxed only if it
  # starves the pool. On an 8-match slate 76% of feasible lineups double up, and
  # below six matches doubling up is forced, so the fallback is not optional.
  gate <- NULL
  for (use_match in c(TRUE, FALSE)) {
    for (buf in salary_buffers) {
      min_salary <- salary_cap - buf
      g <- stream_gate(min_salary, use_match)
      if (verbose) {
        label <- if (buf >= salary_cap) "no floor"
                 else sprintf("$%s-$%s", format(min_salary, big.mark = ","),
                              format(salary_cap, big.mark = ","))
        cat(sprintf("    Salary filter (%s%s): %s feasible\n", label,
                    if (use_match && !is.null(match_id)) ", one per match" else "",
                    format(if (is.null(g)) 0 else g$n_feasible, big.mark = ",")))
      }
      if (!is.null(g) && g$n_feasible >= target_lineups) { gate <- g; break }
      if (!is.null(g) && (is.null(gate) || g$n_feasible > gate$n_feasible)) gate <- g
    }
    if (!is.null(gate) && gate$n_feasible >= target_lineups) break
    if (verbose && use_match && !is.null(match_id))
      cat("    Too few lineups with one player per match -- allowing both sides...\n")
  }
  if (is.null(gate)) stop("No valid lineups found")

  gate_idx <- gate$idx
  n_cand   <- ncol(gate_idx)
  if (verbose)
    cat(sprintf("  Gate: %s feasible -> %s candidates\n",
                format(gate$n_feasible, big.mark = ","), format(n_cand, big.mark = ",")))

  # ---------------------------------------------------------------------------
  # STAGE 2: the simulation picks. Rank candidates by how often they land in the
  # top tail_frac of the field, then cut to target_lineups.
  # ---------------------------------------------------------------------------
  if (n_cand > target_lineups) {
    if (verbose) cat(sprintf("  Scoring %s candidates on %s sims...\n",
                             format(n_cand, big.mark = ","),
                             format(min(gate_sims, n_sims), big.mark = ",")))

    sub_ids    <- head(sort(unique(sim_results$SimID)), gate_sims)
    score_wide <- dcast(sim_results[SimID %in% sub_ids], Player ~ SimID,
                        value.var = "FantasyPoints", fill = 0)
    setorder(score_wide, Player)
    score_mat <- as.matrix(score_wide[, -1, with = FALSE])
    n_sub     <- ncol(score_mat)

    member <- matrix(0, nrow = n_cand, ncol = n_players)
    member[cbind(rep(seq_len(n_cand), times = roster_size), as.vector(t(gate_idx)))] <- 1

    # Blocked so the score matrix never exceeds n_cand x block in memory.
    n_top <- max(1L, round(n_cand * tail_frac))
    kth   <- n_cand - n_top + 1L
    hits  <- integer(n_cand)
    block <- 250L
    for (a in seq(1L, n_sub, by = block)) {
      b   <- min(a + block - 1L, n_sub)
      sm  <- member %*% score_mat[, a:b, drop = FALSE]
      thr <- apply(sm, 2L, function(v) sort(v, partial = kth)[kth])
      hits <- hits + rowSums(sm >= rep(thr, each = n_cand))
    }

    gate_idx <- gate_idx[, order(hits, decreasing = TRUE)[seq_len(target_lineups)], drop = FALSE]
    if (verbose) cat(sprintf("  Kept best %s by top-%.1f%% hit rate\n",
                             format(target_lineups, big.mark = ","), 100 * tail_frac))
  }

  n_final <- ncol(gate_idx)

  # ---------------------------------------------------------------------------
  # STAGE 3: reported metrics on the surviving pool, from the simulation.
  # Order must match unique_lineups -- app.R assigns these columns by position.
  # ---------------------------------------------------------------------------
  win_wide <- dcast(sim_results, Player ~ SimID, value.var = "Win", fill = 0)
  setorder(win_wide, Player)
  win_mat  <- as.matrix(win_wide[, -1, with = FALSE])

  member_f <- matrix(0, nrow = n_final, ncol = n_players)
  member_f[cbind(rep(seq_len(n_final), times = roster_size), as.vector(t(gate_idx)))] <- 1

  # Blocked over sims. The whole product is n_final x n_sims -- 5,000 x 50,000
  # is 2GB, which is most of the memory budget on an 8GB machine for a result
  # that is only ever reduced to two counts.
  n_win6 <- integer(n_final)
  n_win5 <- integer(n_final)
  n_cols <- ncol(win_mat)
  for (a in seq(1L, n_cols, by = 2500L)) {
    b  <- min(a + 2499L, n_cols)
    wm <- member_f %*% win_mat[, a:b, drop = FALSE]
    n_win6 <- n_win6 + rowSums(wm >= roster_size)
    n_win5 <- n_win5 + rowSums(wm >= (roster_size - 1L))
  }

  win_metrics <- data.table(
    ExpectedWins = colSums(matrix(players_dt$WinProb[gate_idx], nrow = roster_size)),
    Win6Pct      = n_win6 / n_cols * 100,
    Win5PlusPct  = n_win5 / n_cols * 100
  )

  player_cols <- paste0("Player", seq_len(roster_size))
  lineup_only <- as.data.table(
    matrix(players[gate_idx], nrow = n_final, ncol = roster_size, byrow = TRUE)
  )
  setnames(lineup_only, player_cols)

  if (verbose) {
    cat(sprintf("  Top %s lineups selected\n", format(n_final, big.mark = ",")))
    cat(sprintf("    ExpectedWins: %.2f to %.2f\n",
                min(win_metrics$ExpectedWins), max(win_metrics$ExpectedWins)))
    cat(sprintf("    Win6Pct: %.1f%% to %.1f%%\n\n",
                min(win_metrics$Win6Pct), max(win_metrics$Win6Pct)))
  }

  return(list(
    unique_lineups = lineup_only,
    win_metrics = win_metrics,
    n_sims = n_sims,
    mode = "win_based"
  ))
}

# =============================================================================
# find_optimal_lineups_preseason_classic
# -----------------------------------------------------------------------------
# A position-constrained classic optimum with NO linear program, because on a
# preseason slate there is nothing to trade off: DraftKings prices every player
# identically (5,500), so nine of them cost 49,500 against a 50,000 cap and the
# constraint can never bind. With salary out of the picture the best lineup is
# simply the best available at each slot -- take the top QB, the top two RB, the
# top three WR, the top TE and the top DST, then the best remaining RB/WR/TE for
# the flex. That is exactly optimal, not a heuristic, and it runs in a sort
# rather than an LP solve per simulation.
#
# sim_results needs SimID, Player, FantasyPoints and Pos.
# =============================================================================
# -----------------------------------------------------------------------------
# ps_top_frac -- how often each candidate lineup lands in the top FRAC of all
# candidates, measured across simulations.
#
# WHY NOT THE MEAN. The mean-of-means cut asks "what is the highest expected
# score". Measured against three real contests it ranks almost the same lineups
# but concentrates harder: on 2026-08-20 the mean cut put one back in 79.5% of
# the pool (he scored 3.3) where this metric held him to 71.4%, and it beat the
# mean cut on every hit rate that night. It ties on good slates and loses less
# on bad ones.
#
# SPEED. The naive form is 50k candidates x 50k sims. Each lineup is 9 of ~90
# players, so the gather is expressed as a SPARSE indicator matrix and the
# per-sim scores come from one sparse matmul per block -- 13s at 5,000 sims
# against 24s for a hand-rolled gather and 28s for dense BLAS. Sims are blocked
# so the score matrix never exceeds n x block in memory.
#
# SIM COUNT. Ranking is stable well below the full run: Spearman against the
# full 50,000 is 0.948 at 500 sims, 0.995 at 5,000 and 0.998 at 10,000, and
# pool performance is flat past ~2,500. The SIMULATION still uses every sim --
# this subsample only estimates the ranking statistic.
ps_top_frac <- function(uni, pc, sim_results, score_col,
                        n_sims_use = 5000L, frac = 0.05, block = 2500L) {
  if (!requireNamespace("Matrix", quietly = TRUE) ||
      !requireNamespace("matrixStats", quietly = TRUE)) return(NULL)
  # Subset the sims BEFORE reshaping. dcast over the full 4.5M-row result was
  # the whole cost here -- it added ~180s against 13s for the matmul itself.
  # Filtering first and filling a matrix by index is ~20x cheaper.
  sid <- utils::head(unique(sim_results$SimID), n_sims_use)
  sub <- sim_results[SimID %in% sid, c("Player", "SimID", score_col), with = FALSE]
  pn  <- unique(sub$Player)
  Wm  <- matrix(0, nrow = length(pn), ncol = length(sid))
  Wm[cbind(match(sub$Player, pn), match(sub$SimID, sid))] <- sub[[score_col]]
  M <- as.matrix(uni[, ..pc])
  n <- nrow(M); k <- ncol(M)
  ridx <- match(as.vector(M), pn)
  if (anyNA(ridx)) return(NULL)
  A <- Matrix::sparseMatrix(i = rep(seq_len(n), k), j = ridx, x = 1,
                            dims = c(n, length(pn)))
  cnt <- numeric(n)
  for (s in seq(1L, ncol(Wm), by = block)) {
    ix <- s:min(s + block - 1L, ncol(Wm))
    L  <- as.matrix(A %*% Wm[, ix, drop = FALSE])
    thr <- matrixStats::colQuantiles(L, probs = 1 - frac)
    cnt <- cnt + rowSums(L >= rep(thr, each = n))
    rm(L)
  }
  cnt / ncol(Wm)
}

find_optimal_lineups_preseason_classic <- function(sim_results, config,
                                                   k = 1, verbose = TRUE) {
  setDT(sim_results)
  slots <- config$position_slots %||% list(QB = 1, RB = 2, WR = 3, TE = 1,
                                           FLEX = 1, DST = 1)
  flex_ok <- config$flex_eligible %||% c("RB","WR","TE")
  max_lineups <- config$max_lineups %||% 5000L

  if (!"Pos" %in% names(sim_results))
    stop("preseason_classic optimiser needs a Pos column on sim_results")

  sims <- unique(sim_results$SimID)
  if (verbose) cat(sprintf("
Phase 1: optimal classic lineup for %s sims
",
                           format(length(sims), big.mark = ",")))

  setorder(sim_results, SimID, -FantasyPoints)
  # Rank inside each sim and position once; every pick below is then a lookup.
  sim_results[, prk := seq_len(.N), by = .(SimID, Pos)]

  base <- rbindlist(lapply(names(slots)[names(slots) != "FLEX"], function(p) {
    sim_results[Pos == p & prk <= slots[[p]]]
  }))
  # The flex is the best eligible player NOT already used at his own position.
  flex <- sim_results[Pos %in% flex_ok]
  flex <- flex[!paste(SimID, Player) %in% paste(base$SimID, base$Player)]
  setorder(flex, SimID, -FantasyPoints)
  flex <- flex[, head(.SD, 1), by = SimID]

  base[, slot := Pos]
  flex[, slot := "FLEX"]
  full <- rbind(base, flex, fill = TRUE)
  # Keep only sims where every slot could be filled.
  need <- sum(unlist(slots))
  ok <- full[, .N, by = SimID][N == need]$SimID
  full <- full[SimID %in% ok]
  if (!length(ok)) stop("no simulation had enough players to fill every slot")

  # SLOT ORDER IS THE UPLOAD FORMAT. DraftKings and FanDuel both import a
  # classic NFL lineup positionally as QB/RB/RB/WR/WR/WR/TE/FLEX/DST, so the
  # columns have to come out in that order. Sorting by Pos instead gives
  # DST/QB/RB/RB/RB/TE/WR/WR/WR, which uploads into the wrong slots.
  slot_order <- c("QB","RB","WR","TE","FLEX","DST")
  full[, slot_rank := match(slot, slot_order)]
  setorder(full, SimID, slot_rank, -FantasyPoints)
  full[, slot_i := seq_len(.N), by = SimID]
  wide <- dcast(full, SimID ~ slot_i, value.var = "Player")
  setnames(wide, c("SimID", paste0("Player", seq_len(need))))
  pc <- paste0("Player", seq_len(need))
  # Sorting each lineup's names makes the same nine players one row regardless
  # of which slot they happened to fill.
  key <- apply(as.matrix(wide[, ..pc]), 1, function(r) paste(sort(r), collapse = "|"))
  wide[, lkey := key]

  cnt <- wide[, .(Top1Count = .N), by = lkey][order(-Top1Count)]
  uni <- wide[!duplicated(lkey)]
  uni <- merge(uni, cnt, by = "lkey")
  # THE CAP MUST CHOOSE ON QUALITY.
  #
  # Two things make this the load-bearing step in preseason. First, the lineup
  # space is ~4.5e12 and every single sim yields a UNIQUE optimum, so
  # Top1Count is 1 for every row and carries no information at all. Second,
  # merge() above re-sorted by lkey -- the player names sorted and pasted --
  # so taking head() kept the ALPHABETICALLY first lineups. That put
  # "ARI D/ST" in 53% of a 5,000-lineup pool against a true per-sim exposure
  # of 13%: pure artifact.
  #
  # Ranking on the lineup's mean instead is free. A lineup's mean is the sum
  # of its players' means, so no scoring matrix is needed -- which is what
  # lets the candidate pool come from 100k sims without the memory blowing
  # up. Measured against keeping an arbitrary slice, it lifts pool quality
  # from 1.03 to 1.61 mean Top1% and raises the mean lineup score 56.9 -> 61.8.
  #
  # A HARD top-N by mean is the highest-EV pool but a narrow one: at 100k sims
  # it is a top-5% cut, and everything below the chalk gets shaved off (Altmyer
  # 77.6%, QBs down to 16 distinct names). config$pool_spread softens the cut
  # into a weighted sample of the same ranking -- Gumbel-top-k, which is exactly
  # sampling without replacement from softmax(lineup_mu / T) but O(n) instead of
  # O(n*k), so it still runs over millions of candidates. T is in lineup POINTS
  # (pool_spread * sd of the lineup means), so 0 reproduces the hard cut and
  # larger values trade EV for coverage. Ranking, not filtering: a bad lineup is
  # still overwhelmingly unlikely to survive.
  if (nrow(uni) > max_lineups) {
    metric <- if (!is.null(config$phase1_metric)) config$phase1_metric else "mean"
    lm <- NULL
    if (identical(metric, "top5")) {
      lm <- ps_top_frac(uni, pc, sim_results, "FantasyPoints",
                        n_sims_use = if (!is.null(config$phase1_sims))
                                       config$phase1_sims else 5000L,
                        frac = if (!is.null(config$phase1_frac))
                                 config$phase1_frac else 0.05)
      if (is.null(lm) && verbose) cat("  phase1 top5 unavailable, using mean
")
    }
    if (is.null(lm)) {
      pmu <- sim_results[, .(mu = mean(FantasyPoints)), by = Player]
      mu  <- setNames(pmu$mu, pmu$Player)
      lm  <- rowSums(matrix(mu[unlist(uni[, ..pc])], nrow = nrow(uni)))
    }
    sprd <- if (!is.null(config$pool_spread)) config$pool_spread else 0
    if (sprd > 0) {
      tT <- sprd * stats::sd(lm)
      g  <- -log(-log(stats::runif(length(lm))))       # Gumbel(0,1)
      uni[, lineup_mu := lm/tT + g]
    } else {
      uni[, lineup_mu := lm]
    }
    setorder(uni, -Top1Count, -lineup_mu)
    uni <- head(uni, max_lineups)
    uni[, lineup_mu := NULL]
  } else {
    setorder(uni, -Top1Count)
  }
  sc <- full[, .(TotalScore = sum(FantasyPoints)), by = SimID]
  uni <- merge(uni, sc, by = "SimID", all.x = TRUE)
  uni[, `:=`(AvgScore = TotalScore, TotalSalary = NA_real_)]
  uni[, c("lkey","SimID","TotalScore") := NULL]

  if (verbose) cat(sprintf("  %s distinct lineups from %s sims
",
                           format(nrow(uni), big.mark = ","),
                           format(length(ok), big.mark = ",")))
  # n_sims and config are not decoration -- score_all_lineups sizes its scoring
  # matrix from n_sims, and reads the platform column out of config. Omitting
  # them fails later, inside the scorer, rather than here.
  list(unique_lineups = uni, n_sims = length(sims), config = config,
       mode = "preseason_classic")
}


# =============================================================================
# MODE: CFB CLASSIC  (QB / RB / RB / WR / WR / WR / FLEX / SFLEX, $50k cap)
# -----------------------------------------------------------------------------
# The first slate here that constrains BOTH position AND a binding salary cap
# (CFB classic salaries run $3,000-$9,000, so eight studs blow $50k), so
# selection is an exact per-sim binary LP:
#     sum(x) == 8 ;  sum(salary * x) <= cap
#     QB in [1,2] ;  RB in [2,4] ;  WR in [3,5]
# Those three position bounds are exactly the condition that the chosen 8 can
# be dealt into QB, RB, RB, WR, WR, WR, FLEX(RB/WR), SFLEX(QB/RB/WR).
#
# The 8 are then ASSIGNED to slots so FLEX and SFLEX hold the latest-kicking
# players (StartOrder, for late swap) -- except a second QB, if the LP took
# one, is forced into SFLEX because it is the only QB-eligible flex slot.
#
# Ranking mirrors preseason_classic: with a binding cap every sim's optimum is
# effectively unique, so Top1Count carries little; the pool is ranked by lineup
# mean, softened by config$pool_spread (Gumbel-top-k) for coverage.
#
# sim_results needs SimID, Player, FantasyPoints, Salary, Pos, StartOrder.
# =============================================================================
.cfb_pos_bounds <- function(config) {
  ps <- config$position_slots %||% list(QB = 1, RB = 2, WR = 3, FLEX = 1, SFLEX = 1)
  fl <- config$flex_eligible  %||% c("RB", "WR")
  sf <- config$sflex_eligible %||% c("QB", "RB", "WR")
  nflex <- ps$FLEX %||% 1L; nsflex <- ps$SFLEX %||% 1L
  bump <- function(pos) (pos %in% fl) * nflex + (pos %in% sf) * nsflex
  lo <- c(QB = ps$QB %||% 1L, RB = ps$RB %||% 2L, WR = ps$WR %||% 3L)
  list(lo = lo,
       hi = c(QB = lo[["QB"]] + bump("QB"),
              RB = lo[["RB"]] + bump("RB"),
              WR = lo[["WR"]] + bump("WR")),
       size = sum(lo) + nflex + nsflex)
}

# Greedy pick over rows ALREADY sorted by descending points: take the best
# player that fits the bucket cap (hi), the salary cap, and does not strand a
# mandatory slot (lo). Near-optimal, and on a CFB classic slate the cap
# carries several thousand dollars of slack in a typical lineup so it is only
# reached in a minority of sims -- the caller solves the rest exactly.
.cfb_greedy <- function(pos, sal, cap, b, need) {
  lo <- b$lo; hi <- b$hi
  cnt <- c(QB = 0L, RB = 0L, WR = 0L); spent <- 0; pick <- integer(0)
  for (i in seq_along(pos)) {
    p <- pos[i]
    if (cnt[[p]] >= hi[[p]]) next
    if (spent + sal[i] > cap) next
    slots_left <- need - length(pick)
    mand <- sum(pmax(lo[c("QB", "RB", "WR")] - cnt[c("QB", "RB", "WR")], 0L))
    if (slots_left <= mand && cnt[[p]] >= lo[[p]]) next   # slot reserved for a min
    pick <- c(pick, i); cnt[[p]] <- cnt[[p]] + 1L; spent <- spent + sal[i]
    if (length(pick) == need) break
  }
  if (length(pick) == need) pick else NULL
}

# Assign every sim's chosen 8 to slots at once. `chosen` is long
# (SimID, Player, Pos, StartOrder), 8 rows per sim, positionally valid
# (QB 1-2, RB 2-4, WR 3-5). Returns long (SimID, Player, slot_i) with slot_i
# 1..8 = QB, RB, RB, WR, WR, WR, FLEX, SFLEX.
#
# Rule: FLEX and SFLEX hold the latest-kicking RB/WR-eligible players -- but a
# second QB, if the lineup has one, is forced into SFLEX (the only
# QB-eligible flex slot), and base RB/WR minimums are honoured first.
.cfb_assign_slots_vec <- function(chosen) {
  d <- as.data.table(chosen)[, .(SimID, Player, Pos, StartOrder)]
  d[, isqb := Pos == "QB"]
  d[, nqb  := sum(isqb), by = SimID]

  # QB slot(s): earliest QB -> QB; a second QB -> SFLEX.
  setorder(d, SimID, StartOrder)
  d[isqb == TRUE, qr := rowid(SimID)]
  d[isqb == TRUE & qr == 1L, slot := "QB"]
  d[isqb == TRUE & qr == 2L, slot := "SFLEX"]

  rw <- d[isqb == FALSE]
  rw[, npos  := .N, by = .(SimID, Pos)]
  rw[, quota := fifelse(Pos == "RB", npos - 2L, npos - 3L)]   # flex picks allowed per pos
  rw[, kflex := 1L + as.integer(nqb == 1L)]                   # RW flex slots to fill
  setorder(rw, SimID, -StartOrder)                            # latest kickoff first
  rw[, pr := rowid(SimID, Pos)]
  rw[, is_flex := pr <= quota]                                # exactly kflex per sim
  rw[is_flex == TRUE, fr := rowid(SimID)]
  rw[is_flex == TRUE & kflex == 1L, slot := "FLEX"]           # 2nd QB already holds SFLEX
  rw[is_flex == TRUE & kflex == 2L & fr == 1L, slot := "SFLEX"]  # latest -> SFLEX
  rw[is_flex == TRUE & kflex == 2L & fr == 2L, slot := "FLEX"]
  rw[is_flex == FALSE & Pos == "RB", slot := "RB"]
  rw[is_flex == FALSE & Pos == "WR", slot := "WR"]

  out <- rbind(d[isqb == TRUE, .(SimID, Player, slot)],
               rw[, .(SimID, Player, slot)])
  ord <- c(QB = 1L, RB = 2L, WR = 4L, FLEX = 7L, SFLEX = 8L)
  out[, so := ord[slot]]
  setorder(out, SimID, so)
  out[, slot_i := rowid(SimID)]
  out[, .(SimID, Player, slot_i)]
}

find_optimal_lineups_cfb_classic <- function(sim_results, config, verbose = TRUE) {
  setDT(sim_results)
  if (!all(c("Pos", "StartOrder") %in% names(sim_results)))
    stop("cfb_classic optimiser needs Pos and StartOrder on sim_results")

  b    <- .cfb_pos_bounds(config)
  cap  <- config$salary_cap %||% 50000
  need <- b$size
  max_lineups <- config$max_lineups %||% 5000L
  start_time <- Sys.time()

  SR <- sim_results[!is.na(FantasyPoints) & !is.na(Salary) & Salary > 0 &
                    Pos %in% c("QB", "RB", "WR"),
                    .(SimID, Player, FantasyPoints, Salary, Pos, StartOrder)]

  # Global candidate cut: a player who never rates near the top of his
  # position (by mean points across every sim) cannot be in an optimal 8, and
  # the cheapest few are kept as the punt plays that free cap. ~500 -> ~110.
  pm <- SR[, .(mu = mean(FantasyPoints), sal = Salary[1]), by = .(Player, Pos)]
  keep_pl <- pm[, .SD[union(head(order(-mu), 30L), head(order(sal), 8L)), Player],
                by = Pos]$V1
  SR <- SR[Player %chin% keep_pl]
  all_ids <- unique(SR$SimID); n_sims_full <- length(all_ids)

  # Candidate generation runs over EVERY sim, not a subsample. It used to cap
  # at phase1_sims (2,500 default) to keep this fast, but benchmarking showed
  # the full 50k-sim pass costs ~10s -- cheap -- while the subsample was
  # silently capping how many distinct lineups Phase 1 could ever find (a
  # lineup only shows up here if ONE of the sampled sims called it optimal).
  # A GPP portfolio's whole value is in the sims a subsample would have
  # skipped -- the tail worlds where a chalk-adjacent longshot lineup wins --
  # so trading diversity for a speed-up that direct measurement didn't
  # actually need was the wrong trade. score_all_lineups (Phase 2) always
  # scored every surviving lineup against the full n_sims regardless.
  if (verbose) cat(sprintf("\nPhase 1: CFB classic | %s sims | $%s cap | %d slots | %d candidates\n",
                           format(n_sims_full, big.mark = ","),
                           format(cap, big.mark = ","), need, length(keep_pl)))

  # Point rank within (sim, position).
  setorder(SR, SimID, -FantasyPoints)
  SR[, pr := rowid(SimID, Pos)]

  # ---- FAST PATH -----------------------------------------------------------
  # The unconstrained best lineup: top QB, top 2 RB, top 3 WR, then the best 2
  # of {RB3, RB4, WR4, WR5, QB2} for FLEX + SFLEX. When that lineup is already
  # under the cap it IS the optimum (nothing to gain by downgrading a slot),
  # and on this slate it usually is -- a typical lineup leaves $6-12k unspent.
  base_c <- SR[(Pos == "QB" & pr == 1L) | (Pos == "RB" & pr <= 2L) |
               (Pos == "WR" & pr <= 3L)]
  flex_c <- SR[(Pos == "RB" & pr %in% 3:4) | (Pos == "WR" & pr %in% 4:5) |
               (Pos == "QB" & pr == 2L)]
  setorder(flex_c, SimID, -FantasyPoints)
  flex_c <- flex_c[, head(.SD, 2L), by = SimID]
  cand   <- rbindlist(list(base_c, flex_c), use.names = TRUE)
  full8  <- cand[, .N, by = SimID][N == need, SimID]
  cand   <- cand[SimID %chin% full8]
  under  <- cand[, .(s = sum(Salary)), by = SimID][s <= cap, SimID]
  fast   <- cand[SimID %chin% under]

  # ---- SLOW PATH ---------------------------------------------------------
  # Sims where the unconstrained lineup breaks the cap (or the trimmed
  # candidate set could not field 8): greedy pick under the cap.
  slow_ids <- setdiff(unique(SR$SimID), under)
  slow <- NULL
  if (length(slow_ids)) {
    SS <- SR[SimID %chin% slow_ids]                 # already point-sorted
    slow <- SS[, {
      pk <- .cfb_greedy(Pos, Salary, cap, b, need)
      if (is.null(pk)) .SD[0L] else .SD[pk]
    }, by = SimID, .SDcols = c("Player", "Pos", "StartOrder", "Salary")]
  }
  have_slow <- !is.null(slow) && nrow(slow) > 0L && "Player" %in% names(slow)
  if (verbose)
    cat(sprintf("  %s fast (under cap) + %s solved greedily\n",
                format(length(under), big.mark = ","),
                format(if (have_slow) uniqueN(slow$SimID) else 0L, big.mark = ",")))

  chosen <- rbindlist(list(fast[, .(SimID, Player, Pos, StartOrder)],
                           if (have_slow) slow[, .(SimID, Player, Pos, StartOrder)]),
                      use.names = TRUE)
  if (!nrow(chosen)) stop("cfb_classic optimiser: no feasible lineup in any sim")

  # Assign each sim's 8 to QB/RB/RB/WR/WR/WR/FLEX/SFLEX (vectorised over sims).
  full <- .cfb_assign_slots_vec(chosen)

  wide <- dcast(full, SimID ~ slot_i, value.var = "Player")
  pc <- paste0("Player", seq_len(need))
  setnames(wide, as.character(seq_len(need)), pc)
  key <- apply(as.matrix(wide[, ..pc]), 1L,
               function(r) paste(sort(r), collapse = "|"))
  wide[, lkey := key]
  cnt <- wide[, .(Top1Count = .N), by = lkey]
  uni <- merge(wide[!duplicated(lkey)], cnt, by = "lkey")

  mu  <- setNames(pm$mu, pm$Player)
  uni[, AvgScore := rowSums(matrix(mu[unlist(.SD)], nrow = nrow(uni))), .SDcols = pc]
  sprd <- config$pool_spread %||% 0
  if (sprd > 0) {
    tT <- sprd * stats::sd(uni$AvgScore)
    g  <- -log(-log(stats::runif(nrow(uni))))
    uni[, rk := AvgScore / tT + g]
  } else uni[, rk := AvgScore]
  setorder(uni, -Top1Count, -rk)
  if (nrow(uni) > max_lineups) uni <- head(uni, max_lineups)

  sal <- setNames(pm$sal, pm$Player)
  uni[, TotalSalary := rowSums(matrix(sal[unlist(.SD)], nrow = nrow(uni))), .SDcols = pc]
  uni[, c("lkey", "rk") := NULL]

  if (verbose) cat(sprintf("  %s distinct lineups | %.1fs\n",
                           format(nrow(uni), big.mark = ","),
                           as.numeric(difftime(Sys.time(), start_time, units = "secs"))))
  list(unique_lineups = uni, n_sims = n_sims_full, config = config, mode = "cfb_classic")
}
