# ============================================================================
# TENNIS SIMULATION ENGINE
# For Universal Golden Ticket Sims App
# ============================================================================

library(data.table)
library(readxl)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Convert American odds to probability
odds_to_probability <- function(odds) {
  if (is.na(odds)) return(0.5)
  
  if (odds > 0) {
    # Positive odds: +150 means bet $100 to win $150
    prob <- 100 / (odds + 100)
  } else {
    # Negative odds: -150 means bet $150 to win $100
    prob <- abs(odds) / (abs(odds) + 100)
  }
  
  return(prob)
}

# ============================================================================
# MAIN ENGINE FUNCTION (Called by app)
# ============================================================================

run_tennis_engine <- function(input_data, n_sims, config, progress_callback = NULL) {
  
  cat("\n=== TENNIS ENGINE STARTED ===\n")
  cat("Simulations:", format(n_sims, big.mark = ","), "\n")
  cat("Platform: DK\n\n")
  
  overall_start <- Sys.time()
  
  # Load historical database
  hist_file <- "tennis_clean_database.xlsx"
  
  if (!file.exists(hist_file)) {
    stop("Historical database not found: ", hist_file, 
         "\nPlease ensure tennis_clean_database.xlsx is in the working directory")
  }
  
  cat("Loading historical database:", hist_file, "\n")
  historical_data <- read_excel(hist_file, sheet = "Clean_Data")
  setDT(historical_data)
  cat("Loaded", nrow(historical_data), "historical matches\n\n")
  
  # Index historical data for speed
  setkey(historical_data, tour, surface, best_of, w_straight_sets)
  
  # Extract player data (tennis has single sheet)
  # Check if input_data is a list of data frames or a single data frame
  if (is.data.frame(input_data)) {
    player_data <- input_data
  } else if (is.list(input_data)) {
    # If it's a list, try to get first element or first non-NULL element
    if (length(input_data) > 0) {
      player_data <- input_data[[1]]
    } else {
      stop("input_data is empty list")
    }
  } else {
    stop("input_data must be a data.frame or list of data.frames")
  }
  
  setDT(player_data)
  
  # Add opponent column (extract from Game Info)
  player_data[, Opponent := {
    match_players <- player_data[`Game Info` == `Game Info`]
    sapply(Name, function(n) {
      opponents <- match_players[Name != n, Name]
      if (length(opponents) > 0) opponents[1] else NA_character_
    })
  }, by = `Game Info`]
  
  # Add Match column (cleaner display)
  player_data[, Match := `Game Info`]
  
  # Get unique matches
  matches <- unique(player_data$`Game Info`)
  total_matches <- length(matches)
  
  cat("Processing", total_matches, "matches:\n")
  for(i in seq_along(matches)) {
    match_players <- player_data[`Game Info` == matches[i]]
    if(nrow(match_players) == 2) {
      cat(sprintf("  %d. %s vs %s\n", i, match_players$Name[1], match_players$Name[2]))
    }
  }
  cat("\n")
  
  # ========================================================================
  # PRE-COMPUTATION PHASE
  # ========================================================================
  
  cat("=== PRE-COMPUTING MATCH PROBABILITIES ===\n")
  precomp_start <- Sys.time()
  
  match_cache <- list()
  
  for (match_idx in seq_along(matches)) {
    match_name <- matches[match_idx]
    
    cat(sprintf("Match %d/%d: %s", match_idx, total_matches, match_name))
    
    match_players <- player_data[`Game Info` == match_name]
    
    if (nrow(match_players) != 2) {
      cat(" - SKIPPED (invalid player count)\n")
      next
    }
    
    p1 <- match_players[1]
    p2 <- match_players[2]
    
    cat(sprintf(" (%s vs %s)", p1$Name, p2$Name))
    
    # Check for walkover
    is_walkover <- any(c(p1$Tour, p2$Tour) %in% c("WD", "WO"))
    
    if (is_walkover) {
      cat(" - WALKOVER\n")
      
      # Determine winner/loser
      if (p1$Tour %in% c("WD")) {
        winner_name <- p2$Name
        loser_name <- p1$Name
      } else {
        winner_name <- p1$Name
        loser_name <- p2$Name
      }
      
      match_cache[[match_name]] <- list(
        type = "walkover",
        p1_name = p1$Name,
        p2_name = p2$Name,
        winner = winner_name,
        loser = loser_name,
        winner_score = 30,
        loser_score = 0
      )
      
    } else {
      # NORMAL MATCH
      
      # Calculate ML probabilities (devigged)
      p1_ml_raw <- odds_to_probability(as.numeric(p1$ML))
      p2_ml_raw <- odds_to_probability(as.numeric(p2$ML))
      total_ml <- p1_ml_raw + p2_ml_raw
      p1_ml_prob <- (p1_ml_raw / total_ml) * 100  # Convert to percentage
      p2_ml_prob <- (p2_ml_raw / total_ml) * 100
      
      # Calculate SS probabilities
      p1_ss_raw <- odds_to_probability(as.numeric(p1$SS))
      p2_ss_raw <- odds_to_probability(as.numeric(p2$SS))
      
      # Ensure SS doesn't exceed ML
      p1_ss_raw <- min(p1_ss_raw, p1_ml_raw)
      p2_ss_raw <- min(p2_ss_raw, p2_ml_raw)
      
      # Devig SS probabilities
      p1_ss_prob <- (p1_ss_raw / total_ml) * 100
      p2_ss_prob <- (p2_ss_raw / total_ml) * 100
      
      # Calculate NSS probabilities
      p1_nss_prob <- p1_ml_prob - p1_ss_prob
      p2_nss_prob <- p2_ml_prob - p2_ss_prob
      
      # Cumulative probabilities for sampling
      cum_probs <- cumsum(c(p1_ss_prob, p1_nss_prob, p2_ss_prob, p2_nss_prob)) / 100
      
      # ====================================================================
      # QUERY HISTORICAL DATA - 2 QUERIES PER MATCH (NOT 4!)
      # ====================================================================
      
      # SS POOL (for both players)
      ss_pool <- historical_data[
        tour == p1$Tour & 
          surface == p1$Surface & 
          best_of == p1$BO & 
          w_straight_sets == TRUE
      ]
      
      if (nrow(ss_pool) > 0) {
        # Calculate odds difference
        ss_pool[, odds_diff := abs(winner_prob_pct - p1_ml_prob) + 
                  abs(loser_prob_pct - p2_ml_prob)]
        
        # Sort and take top 100
        setorder(ss_pool, odds_diff)
        ss_pool <- ss_pool[1:min(100, .N)]
      } else {
        ss_pool <- NULL
      }
      
      # NSS POOL (for both players)
      nss_pool <- historical_data[
        tour == p1$Tour & 
          surface == p1$Surface & 
          best_of == p1$BO & 
          w_straight_sets == FALSE
      ]
      
      if (nrow(nss_pool) > 0) {
        # Calculate odds difference
        nss_pool[, odds_diff := abs(winner_prob_pct - p1_ml_prob) + 
                   abs(loser_prob_pct - p2_ml_prob)]
        
        # Sort and take top 100
        setorder(nss_pool, odds_diff)
        nss_pool <- nss_pool[1:min(100, .N)]
      } else {
        nss_pool <- NULL
      }
      
      match_cache[[match_name]] <- list(
        type = "normal",
        p1_name = p1$Name,
        p2_name = p2$Name,
        cum_probs = cum_probs,
        ss_pool = ss_pool,
        nss_pool = nss_pool
      )
    }
    
    cat(" - COMPLETED\n")
  }
  
  precomp_elapsed <- difftime(Sys.time(), precomp_start, units = "secs")
  cat(sprintf("Pre-computation completed in %.2f seconds\n\n", as.numeric(precomp_elapsed)))
  
  # ========================================================================
  # SIMULATION PHASE
  # ========================================================================
  
  cat("=== RUNNING SIMULATIONS ===\n")
  sim_start <- Sys.time()
  
  # Pre-allocate results
  n_players <- nrow(player_data)
  estimated_rows <- n_sims * n_players
  
  sim_results <- data.table(
    SimID = integer(estimated_rows),
    Player = character(estimated_rows),
    DKScore = numeric(estimated_rows),
    Result = character(estimated_rows),  # Winner/Loser
    Outcome = character(estimated_rows), # SS/NSS/WO
    Win = integer(estimated_rows)        # 1 if Winner, 0 if Loser
  )
  
  row_idx <- 1
  progress_interval <- max(1000, n_sims %/% 20)
  
  for (iter in 1:n_sims) {
    for (match_name in names(match_cache)) {
      match_info <- match_cache[[match_name]]
      
      if (match_info$type == "walkover") {
        # Deterministic outcome
        sim_results[row_idx, `:=`(
          SimID = iter,
          Player = match_info$winner,
          DKScore = match_info$winner_score,
          Result = "Winner",
          Outcome = "WO",
          Win = 1L
        )]
        row_idx <- row_idx + 1
        
        sim_results[row_idx, `:=`(
          SimID = iter,
          Player = match_info$loser,
          DKScore = match_info$loser_score,
          Result = "Loser",
          Outcome = "WO",
          Win = 0L
        )]
        row_idx <- row_idx + 1
        
      } else {
        # Sample outcome
        random_val <- runif(1)
        
        # Determine outcome: 1=P1_SS, 2=P1_NSS, 3=P2_SS, 4=P2_NSS
        outcome_idx <- findInterval(random_val, match_info$cum_probs) + 1
        
        # Get appropriate pool and determine winner
        if (outcome_idx == 1) {
          # P1 wins SS
          pool <- match_info$ss_pool
          winner <- match_info$p1_name
          loser <- match_info$p2_name
          outcome_type <- "SS"
        } else if (outcome_idx == 2) {
          # P1 wins NSS
          pool <- match_info$nss_pool
          winner <- match_info$p1_name
          loser <- match_info$p2_name
          outcome_type <- "NSS"
        } else if (outcome_idx == 3) {
          # P2 wins SS
          pool <- match_info$ss_pool
          winner <- match_info$p2_name
          loser <- match_info$p1_name
          outcome_type <- "SS"
        } else {
          # P2 wins NSS
          pool <- match_info$nss_pool
          winner <- match_info$p2_name
          loser <- match_info$p1_name
          outcome_type <- "NSS"
        }
        
        # Sample score from pool
        if (!is.null(pool) && nrow(pool) > 0) {
          idx <- sample(nrow(pool), 1)
          winner_score <- pool$w_dk_score[idx]
          loser_score <- pool$l_dk_score[idx]
        } else {
          # Fallback if no historical data
          winner_score <- runif(1, 50, 70)
          loser_score <- runif(1, 20, 40)
        }
        
        # Record results
        sim_results[row_idx, `:=`(
          SimID = iter,
          Player = winner,
          DKScore = winner_score,
          Result = "Winner",
          Outcome = outcome_type,
          Win = 1L
        )]
        row_idx <- row_idx + 1
        
        sim_results[row_idx, `:=`(
          SimID = iter,
          Player = loser,
          DKScore = loser_score,
          Result = "Loser",
          Outcome = outcome_type,
          Win = 0L
        )]
        row_idx <- row_idx + 1
      }
    }
    
    # Progress reporting
    if (iter %% progress_interval == 0) {
      elapsed <- difftime(Sys.time(), sim_start, units = "secs")
      pct_complete <- (iter / n_sims) * 100
      est_total <- elapsed * (n_sims / iter)
      est_remaining <- est_total - elapsed
      
      msg <- sprintf("Progress: %d/%s (%.1f%%) - %.1fs elapsed, ~%.1fs remaining",
                     iter, format(n_sims, big.mark = ","),
                     pct_complete, as.numeric(elapsed), as.numeric(est_remaining))
      cat(msg, "\n")
      
      if (!is.null(progress_callback)) {
        progress_callback(msg, pct_complete / 100)
      }
    }
  }
  
  # Trim to actual size
  sim_results <- sim_results[1:(row_idx - 1)]
  
  sim_elapsed <- difftime(Sys.time(), sim_start, units = "secs")
  total_elapsed <- difftime(Sys.time(), overall_start, units = "mins")
  
  cat("\n=== SIMULATION COMPLETED ===\n")
  cat(sprintf("Pre-computation: %.2f seconds\n", as.numeric(precomp_elapsed)))
  cat(sprintf("Simulation: %.2f seconds\n", as.numeric(sim_elapsed)))
  cat(sprintf("Total time: %.2f minutes\n", as.numeric(total_elapsed)))
  cat(sprintf("Generated %s results\n\n", format(nrow(sim_results), big.mark = ",")))
  
  # ========================================================================
  # PREPARE OUTPUTS
  # ========================================================================
  
  # Calculate projections
  cat("Calculating player projections...\n")
  projections <- sim_results[, .(
    Mean = mean(DKScore),
    Median = median(DKScore),
    StdDev = sd(DKScore),
    Min = min(DKScore),
    Max = max(DKScore),
    P10 = quantile(DKScore, 0.10),
    P25 = quantile(DKScore, 0.25),
    P75 = quantile(DKScore, 0.75),
    P90 = quantile(DKScore, 0.90)
  ), by = Player]
  
  # Merge with player metadata
  projections <- merge(
    projections,
    player_data[, .(Player = Name, DKSalary = Salary, DKOwn = Own, Match, Opponent, Surface, Tour)],
    by = "Player"
  )
  
  # Calculate value metrics
  projections[, `:=`(
    PointsPerK = Mean / (DKSalary / 1000),
    Ceiling = P90,
    Floor = P10
  )]
  
  setorder(projections, -Mean)
  
  # Prepare metadata (unique players)
  metadata <- unique(player_data[, .(
    Player = Name,
    DKSalary = Salary,
    DKID = ID,
    DKOwn = Own,
    Match = Match,
    Opponent = Opponent,
    Surface = Surface,
    Tour = Tour
  )])
  
  # ========================================================================
  # PREPARE SPORT-SPECIFIC VISUALIZATIONS
  # ========================================================================
  
  cat("Preparing tennis-specific visualizations...\n")
  
  # 1. Match Analysis - Sim vs Implied Probabilities
  match_analysis_data <- list()
  
  for (match_name in unique(player_data$Match)) {
    match_players <- player_data[Match == match_name]
    
    if (nrow(match_players) != 2) next
    
    p1 <- match_players[1]
    p2 <- match_players[2]
    
    # Calculate implied probabilities
    p1_ml_raw <- odds_to_probability(as.numeric(p1$ML))
    p2_ml_raw <- odds_to_probability(as.numeric(p2$ML))
    total_ml <- p1_ml_raw + p2_ml_raw
    p1_implied_win <- p1_ml_raw / total_ml
    p2_implied_win <- p2_ml_raw / total_ml
    
    p1_ss_raw <- odds_to_probability(as.numeric(p1$SS))
    p2_ss_raw <- odds_to_probability(as.numeric(p2$SS))
    p1_implied_ss <- p1_ss_raw / total_ml
    p2_implied_ss <- p2_ss_raw / total_ml
    
    # Get simulation results for this match
    p1_sims <- sim_results[Player == p1$Name]
    p2_sims <- sim_results[Player == p2$Name]
    
    # Calculate sim win rates (head-to-head)
    h2h <- merge(
      p1_sims[, .(SimID, P1_Score = DKScore, P1_Result = Result, P1_Outcome = Outcome)],
      p2_sims[, .(SimID, P2_Score = DKScore, P2_Result = Result, P2_Outcome = Outcome)],
      by = "SimID"
    )
    
    p1_sim_win <- mean(h2h$P1_Result == "Winner")
    p2_sim_win <- mean(h2h$P2_Result == "Winner")
    
    # Calculate SS rates
    p1_sim_ss <- mean(h2h$P1_Result == "Winner" & h2h$P1_Outcome == "SS")
    p2_sim_ss <- mean(h2h$P2_Result == "Winner" & h2h$P2_Outcome == "SS")
    
    # Win-only average scores
    p1_win_avg <- mean(p1_sims[Result == "Winner", DKScore])
    p2_win_avg <- mean(p2_sims[Result == "Winner", DKScore])
    
    match_analysis_data[[match_name]] <- data.table(
      Match = match_name,
      Player = c(p1$Name, p2$Name),
      Salary = c(p1$Salary, p2$Salary),
      ImpliedWin = c(p1_implied_win, p2_implied_win),
      SimWin = c(p1_sim_win, p2_sim_win),
      WinDiff = c((p1_sim_win - p1_implied_win) * 100, (p2_sim_win - p2_implied_win) * 100),
      ImpliedSS = c(p1_implied_ss, p2_implied_ss),
      SimSS = c(p1_sim_ss, p2_sim_ss),
      AvgWinPts = c(p1_win_avg, p2_win_avg)  # Win-only average
    )
  }
  
  match_analysis_table <- rbindlist(match_analysis_data)
  
  # 2. Score Distribution Data (WINS ONLY with SS/NSS breakdown)
  wins_only <- sim_results[Result == "Winner"]
  
  score_dist_data <- list(
    all_wins = wins_only[, .(Player, SimID, Score = DKScore, Outcome)],
    ss_wins = wins_only[Outcome == "SS", .(Player, SimID, Score = DKScore)],
    nss_wins = wins_only[Outcome == "NSS", .(Player, SimID, Score = DKScore)]
  )
  
  # Return results in expected format
  cat("Returning results to app...\n\n")
  
  return(list(
    sim_results = sim_results,
    metadata = metadata,
    projections = projections,
    full_results = sim_results,
    
    # Sport-specific visualizations (NEW)
    sport_visuals = list(
      match_analysis = match_analysis_table,
      score_distributions = score_dist_data,
      player_data = player_data  # For salary vs points plot
    )
  ))
}

# ============================================================================
# TENNIS-SPECIFIC LINEUP METRICS
# ============================================================================

#' Calculate Tennis Lineup Metrics (TotalEW, Win6Pct, Win5PlusPct)
#' Called by app after standard distribution metrics are calculated
#' @param scored_lineups Data.table with lineups and standard metrics
#' @param sim_results Simulation results with Win column
#' @param metadata Player metadata with Match information
#' @return scored_lineups with added tennis-specific columns
calculate_tennis_lineup_metrics <- function(scored_lineups, sim_results, metadata) {
  
  cat("Calculating tennis-specific lineup metrics...\n")
  
  setDT(scored_lineups)
  setDT(metadata)
  setDT(sim_results)
  
  # Get player columns
  player_cols <- grep("^Player[0-9]", names(scored_lineups), value = TRUE)
  n_lineups <- nrow(scored_lineups)
  
  # ============================================================================
  # VECTORIZED APPROACH - NO LOOPS!
  # ============================================================================
  
  cat("  Calculating TotalEW (vectorized)...\n")
  
  # Pre-calculate individual EW from Win column
  individual_ew <- sim_results[, .(IndividualEW = mean(Win)), by = Player]
  setkey(individual_ew, Player)
  
  # Match mapping for constraint-aware EW
  player_match_map <- setNames(metadata$Match, metadata$Player)
  
  # Calculate TotalEW using vectorized operations
  scored_lineups[, TotalEW := {
    lineup_players <- unlist(.SD)
    player_matches <- player_match_map[lineup_players]
    match_counts <- table(player_matches)
    
    # For matches with 1 player: sum their individual EWs
    # For matches with 2+ players: add 1.0
    sum(
      ifelse(match_counts == 1, 
             individual_ew[lineup_players[!duplicated(player_matches)], IndividualEW],
             1.0)
    )
  }, by = 1:n_lineups, .SDcols = player_cols]
  
  cat("  Calculating Win6+ and Win5+ (vectorized)...\n")
  
  # ============================================================================
  # SUPER FAST: Convert to wide format, then vectorized operations
  # ============================================================================
  
  # Create a matrix: Player × SimID with Win values (0 or 1)
  # This is MUCH faster than filtering per lineup
  win_matrix <- dcast(sim_results, Player ~ SimID, value.var = "Win", fill = 0)
  setkey(win_matrix, Player)
  
  sim_cols <- setdiff(names(win_matrix), "Player")
  
  # For each lineup, sum wins across all sims
  # This is now just matrix operations!
  scored_lineups[, c("Win6Pct", "Win5PlusPct") := {
    lineup_players <- unlist(.SD)
    
    # Get win matrix for these players (vectorized lookup!)
    lineup_win_matrix <- as.matrix(win_matrix[lineup_players, ..sim_cols])
    
    # Sum wins per sim (column sums)
    wins_per_sim <- colSums(lineup_win_matrix)
    
    # Calculate percentages
    list(
      Win6Pct = mean(wins_per_sim >= 6) * 100,
      Win5PlusPct = mean(wins_per_sim >= 5) * 100
    )
  }, by = 1:n_lineups, .SDcols = player_cols]
  
  cat("Tennis metrics calculated\n\n")
  
  return(scored_lineups)
}