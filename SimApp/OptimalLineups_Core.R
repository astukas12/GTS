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
  
  sim_ids <- unique(sim_results$SimID)
  n_sims <- length(sim_ids)
  
  if (verbose) {
    cat(sprintf("  %s sims | top %d per sim | cap: %s\n",
                format(n_sims, big.mark = ","), k,
                if (is.finite(max_lineups)) format(max_lineups, big.mark = ",") else "none"))
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
  
  if (verbose) {
    cat(sprintf("  %s lineups × %s sims | Mode: %s\n",
                format(n_lineups, big.mark = ","),
                format(n_sims, big.mark = ","),
                mode))
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
  
  percentiles <- config$percentiles
  
  if (verbose) {
    cat("  Phase 3: 20%% | Calculating rankings...\n")
    flush.console()
  }
  
  # Calculate Top %
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
  
  # Win rate
  max_scores <- apply(score_matrix, 2, max)
  win_counts <- rowSums(sweep(score_matrix, 2, max_scores, "=="))
  win_rate <- (win_counts / n_sims) * 100
  
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
      
      for (i in 1:n_lineups) {
        players <- as.character(unique_lineups[i, ..player_cols])
        
        for (j in seq_along(players)) {
          player_own <- ownership_data[Player == players[j], get(own_col)]
          
          if (length(player_own) > 0 && !is.na(player_own)) {
            cumulative_own[i] <- cumulative_own[i] + (player_own * multipliers[j])
          }
        }
        
        # Geometric mean
        all_owns <- sapply(seq_along(players), function(j) {
          own <- ownership_data[Player == players[j], get(own_col)]
          if (length(own) > 0 && !is.na(own)) rep(own, multipliers[j]) else NULL
        })
        all_owns <- unlist(all_owns)
        all_owns_safe <- all_owns[!is.na(all_owns) & all_owns > 0]
        if (length(all_owns_safe) > 0) {
          geometric_own[i] <- exp(mean(log(all_owns_safe)))
        }
      }
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