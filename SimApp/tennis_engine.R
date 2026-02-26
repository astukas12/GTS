# ============================================================================
# TENNIS SIMULATION ENGINE
# For Universal Golden Ticket Sims App
# ============================================================================

library(data.table)
library(readxl)

# ============================================================================
# HELPER: American odds -> devigged probability
# ============================================================================

odds_to_probability <- function(odds) {
  if (is.na(odds)) return(0.5)
  if (odds > 0) 100 / (odds + 100)
  else          abs(odds) / (abs(odds) + 100)
}

# ============================================================================
# MAIN ENGINE
# ============================================================================

run_tennis_engine <- function(input_data, n_sims, config, progress_callback = NULL) {
  
  cat("\n=== TENNIS ENGINE STARTED ===\n")
  cat("Simulations:", format(n_sims, big.mark = ","), "\n\n")
  
  overall_start <- Sys.time()
  cb <- function(val, msg) {
    if (!is.null(progress_callback)) progress_callback(msg, val)
  }
  
  # --------------------------------------------------------------------------
  # LOAD HISTORICAL DATABASE
  # --------------------------------------------------------------------------
  hist_file <- "tennis_clean_database.xlsx"
  if (!file.exists(hist_file))
    stop("Historical database not found: ", hist_file)
  
  cat("Loading historical database...\n")
  historical_data <- as.data.table(read_excel(hist_file, sheet = "Clean_Data"))
  cat("Loaded", nrow(historical_data), "historical matches\n\n")
  
  # Derive w_favorite if not present (winner was favourite when winner_prob_pct >= 50)
  if (!"w_favorite" %in% names(historical_data)) {
    historical_data[, w_favorite := winner_prob_pct >= 50]
  }
  
  setkey(historical_data, tour, surface, best_of, w_favorite, w_straight_sets)
  
  # --------------------------------------------------------------------------
  # PARSE INPUT
  # --------------------------------------------------------------------------
  player_data <- if (is.data.frame(input_data)) input_data
  else if (is.list(input_data) && length(input_data) > 0) input_data[[1]]
  else stop("input_data must be a data.frame or non-empty list")
  setDT(player_data)
  
  # Derive Opponent and Match columns
  player_data[, Opponent := {
    sapply(seq_len(.N), function(i) {
      others <- .SD[`Game Info` == `Game Info`[i] & Name != Name[i], Name]
      if (length(others) > 0) others[1] else NA_character_
    })
  }]
  player_data[, Match := `Game Info`]
  
  matches       <- unique(player_data[["Game Info"]])
  total_matches <- length(matches)
  
  cat("Processing", total_matches, "matches:\n")
  for (i in seq_along(matches)) {
    mp <- player_data[`Game Info` == matches[i]]
    if (nrow(mp) == 2) cat(sprintf("  %d. %s vs %s\n", i, mp$Name[1], mp$Name[2]))
  }
  cat("\n")
  
  # --------------------------------------------------------------------------
  # PRE-COMPUTATION: 4 separate score pools per match
  # --------------------------------------------------------------------------
  cat("=== PRE-COMPUTING MATCH POOLS ===\n")
  precomp_start <- Sys.time()
  cb(0.05, "Pre-computing match pools...")
  
  ODDS_THRESHOLD <- 5    # +/- 5 pct points on winner prob (combined diff <= 10)
  POOL_FLOOR     <- 10   # minimum pool size; expand beyond threshold if needed
  
  match_cache <- list()
  
  for (match_idx in seq_along(matches)) {
    match_name    <- matches[match_idx]
    match_players <- player_data[`Game Info` == match_name]
    
    cat(sprintf("Match %d/%d: %s", match_idx, total_matches, match_name))
    
    if (nrow(match_players) != 2) {
      cat(" - SKIPPED (not 2 players)\n"); next
    }
    
    p1 <- match_players[1]
    p2 <- match_players[2]
    cat(sprintf(" (%s vs %s)", p1$Name, p2$Name))
    
    # ---- WALKOVER: missing ML or explicit WD/WO tour flag ----
    is_walkover <- any(c(p1$Tour, p2$Tour) %in% c("WD", "WO")) ||
      is.na(suppressWarnings(as.numeric(p1$ML))) ||
      is.na(suppressWarnings(as.numeric(p2$ML)))
    
    if (is_walkover) {
      cat(" - WALKOVER\n")
      winner_name <- if (p1$Tour %in% c("WD", "WO") ||
                         is.na(suppressWarnings(as.numeric(p1$ML)))) p2$Name else p1$Name
      loser_name  <- if (winner_name == p1$Name) p2$Name else p1$Name
      match_cache[[match_name]] <- list(
        type         = "walkover",
        p1_name      = p1$Name, p2_name = p2$Name,
        winner       = winner_name, loser = loser_name,
        winner_score = 30, loser_score = 0
      )
      next
    }
    
    # ---- NORMAL MATCH ----
    
    # Devig ML
    p1_ml_raw  <- odds_to_probability(as.numeric(p1$ML))
    p2_ml_raw  <- odds_to_probability(as.numeric(p2$ML))
    total_ml   <- p1_ml_raw + p2_ml_raw
    p1_ml_prob <- (p1_ml_raw / total_ml) * 100
    p2_ml_prob <- (p2_ml_raw / total_ml) * 100
    
    # Devig SS (capped at ML prob)
    p1_ss_raw  <- min(odds_to_probability(as.numeric(p1$SS)), p1_ml_raw)
    p2_ss_raw  <- min(odds_to_probability(as.numeric(p2$SS)), p2_ml_raw)
    p1_ss_prob <- (p1_ss_raw / total_ml) * 100
    p2_ss_prob <- (p2_ss_raw / total_ml) * 100
    
    # NSS = ML - SS
    p1_nss_prob <- p1_ml_prob - p1_ss_prob
    p2_nss_prob <- p2_ml_prob - p2_ss_prob
    
    # Cumulative cutpoints: 1=P1_SS, 2=P1_NSS, 3=P2_SS, 4=P2_NSS
    cum_probs <- cumsum(c(p1_ss_prob, p1_nss_prob, p2_ss_prob, p2_nss_prob)) / 100
    
    # Who is the favourite?
    p1_is_fav <- p1_ml_prob >= 50
    fav_prob  <- if (p1_is_fav) p1_ml_prob else p2_ml_prob
    dog_prob  <- if (p1_is_fav) p2_ml_prob else p1_ml_prob
    
    # Build one pool for a given outcome bucket
    # Build one pool for a given outcome bucket
    # winner_is_fav: TRUE = historical winner was the favourite
    # When TRUE:  winner_prob_pct ~ fav_prob, loser_prob_pct ~ dog_prob
    # When FALSE: winner_prob_pct ~ dog_prob, loser_prob_pct ~ fav_prob
    get_pool <- function(winner_is_fav, straight_sets) {
      pool <- historical_data[
        tour            == p1$Tour    &
          surface         == p1$Surface &
          best_of         == p1$BO      &
          w_favorite      == winner_is_fav &
          w_straight_sets == straight_sets
      ]
      if (nrow(pool) == 0) return(NULL)
      
      pool <- copy(pool)
      
      if (winner_is_fav) {
        # Historical winner = favourite: compare winner_prob to fav_prob
        pool[, odds_diff := abs(winner_prob_pct - fav_prob) +
               abs(loser_prob_pct  - dog_prob)]
      } else {
        # Historical winner = underdog: compare winner_prob to dog_prob
        pool[, odds_diff := abs(winner_prob_pct - dog_prob) +
               abs(loser_prob_pct  - fav_prob)]
      }
      setorder(pool, odds_diff)
      
      # Use all matches within threshold; fall back to closest POOL_FLOOR if sparse
      in_range <- pool[odds_diff <= (ODDS_THRESHOLD * 2)]
      pool     <- if (nrow(in_range) >= POOL_FLOOR) in_range else pool[1:min(POOL_FLOOR, .N)]
      
      # Inverse-rank weights: closest match sampled most often
      pool[, sample_weight := 1 / seq_len(.N)]
      pool
    }
    
    # Four pools — winner perspective:
    #   Bucket 1 (P1_SS):  P1 wins SS  -> winner_is_fav = p1_is_fav
    #   Bucket 2 (P1_NSS): P1 wins NSS -> winner_is_fav = p1_is_fav
    #   Bucket 3 (P2_SS):  P2 wins SS  -> winner_is_fav = !p1_is_fav
    #   Bucket 4 (P2_NSS): P2 wins NSS -> winner_is_fav = !p1_is_fav
    pools <- list(
      p1_ss  = get_pool(winner_is_fav = p1_is_fav,  straight_sets = TRUE),
      p1_nss = get_pool(winner_is_fav = p1_is_fav,  straight_sets = FALSE),
      p2_ss  = get_pool(winner_is_fav = !p1_is_fav, straight_sets = TRUE),
      p2_nss = get_pool(winner_is_fav = !p1_is_fav, straight_sets = FALSE)
    )
    
    match_cache[[match_name]] <- list(
      type      = "normal",
      p1_name   = p1$Name,  p2_name   = p2$Name,
      p1_is_fav = p1_is_fav,
      cum_probs = cum_probs,
      pools     = pools
    )
    
    pool_sizes <- sapply(pools, function(p) if (is.null(p)) 0L else nrow(p))
    cat(sprintf(" - OK [pools: P1_SS=%d P1_NSS=%d P2_SS=%d P2_NSS=%d]\n",
                pool_sizes[1], pool_sizes[2], pool_sizes[3], pool_sizes[4]))
  }
  
  precomp_elapsed <- as.numeric(difftime(Sys.time(), precomp_start, units = "secs"))
  cat(sprintf("Pre-computation: %.2fs\n\n", precomp_elapsed))
  
  # --------------------------------------------------------------------------
  # SIMULATION — vectorized across all sims per match (no per-sim loop)
  # --------------------------------------------------------------------------
  cat("=== RUNNING SIMULATIONS ===\n")
  sim_start <- Sys.time()
  cb(0.15, "Running simulations...")
  
  pool_keys  <- c("p1_ss", "p1_nss", "p2_ss", "p2_nss")
  all_results <- vector("list", length(match_cache))
  result_idx  <- 0L
  
  for (match_name in names(match_cache)) {
    result_idx <- result_idx + 1L
    info       <- match_cache[[match_name]]
    
    # ---- Walkover: replicate deterministic result across all sims ----
    if (info$type == "walkover") {
      all_results[[result_idx]] <- data.table(
        SimID   = rep(seq_len(n_sims), 2),
        Player  = c(rep(info$winner, n_sims), rep(info$loser,   n_sims)),
        DKScore = c(rep(info$winner_score, n_sims), rep(info$loser_score, n_sims)),
        Result  = c(rep("Winner", n_sims), rep("Loser",  n_sims)),
        Outcome = rep("WO", n_sims * 2),
        Win     = c(rep(1L, n_sims), rep(0L, n_sims))
      )
      next
    }
    
    # ---- Normal match: sample all n_sims outcomes at once ----
    rand_vals   <- runif(n_sims)
    outcome_idx <- pmax(1L, pmin(4L, findInterval(rand_vals, info$cum_probs) + 1L))
    
    winner_vec    <- ifelse(outcome_idx <= 2, info$p1_name, info$p2_name)
    loser_vec     <- ifelse(outcome_idx <= 2, info$p2_name, info$p1_name)
    out_type      <- ifelse(outcome_idx %in% c(1L, 3L), "SS", "NSS")
    
    winner_scores <- numeric(n_sims)
    loser_scores  <- numeric(n_sims)
    
    for (bucket in 1:4) {
      idx  <- which(outcome_idx == bucket)
      if (length(idx) == 0) next
      pool <- info$pools[[pool_keys[bucket]]]
      
      if (!is.null(pool) && nrow(pool) > 0) {
        drawn <- sample(nrow(pool), size = length(idx), replace = TRUE,
                        prob = pool$sample_weight)
        
        # Historical data stores w_ = favourite's score, l_ = underdog's score.
        # Bucket 1 & 2: P1 wins. Bucket 3 & 4: P2 wins.
        # If the actual winner matches the historical "favourite" role, use w_ directly.
        # w_dk_score is always the match winner's score in the database,
        # l_dk_score is always the loser's — no swap needed regardless of who was favourite
        winner_scores[idx] <- pool$w_dk_score[drawn]
        loser_scores[idx]  <- pool$l_dk_score[drawn]
      } else {
        warning(sprintf(
          "No historical pool: match='%s' bucket='%s' — using fallback scores",
          match_name, pool_keys[bucket]
        ))
        winner_scores[idx] <- runif(length(idx), 50, 70)
        loser_scores[idx]  <- runif(length(idx), 20, 40)
      }
    }
    
    all_results[[result_idx]] <- data.table(
      SimID   = c(seq_len(n_sims),   seq_len(n_sims)),
      Player  = c(winner_vec,        loser_vec),
      DKScore = c(winner_scores,     loser_scores),
      Result  = c(rep("Winner", n_sims), rep("Loser", n_sims)),
      Outcome = c(out_type,          out_type),
      Win     = c(rep(1L, n_sims),   rep(0L, n_sims))
    )
  }
  
  sim_results   <- rbindlist(all_results)
  sim_elapsed   <- as.numeric(difftime(Sys.time(), sim_start,   units = "secs"))
  total_elapsed <- as.numeric(difftime(Sys.time(), overall_start, units = "mins"))
  
  cat(sprintf("\nPre-computation : %.2fs\n", precomp_elapsed))
  cat(sprintf("Simulation      : %.2fs\n",  sim_elapsed))
  cat(sprintf("Total           : %.2f mins\n", total_elapsed))
  cat(sprintf("Rows            : %s\n\n", format(nrow(sim_results), big.mark = ",")))
  cb(0.80, "Simulation complete, preparing outputs...")
  
  # --------------------------------------------------------------------------
  # PROJECTIONS
  # --------------------------------------------------------------------------
  projections <- sim_results[, .(
    Mean   = mean(DKScore),
    Median = median(DKScore),
    StdDev = sd(DKScore),
    Min    = min(DKScore),
    Max    = max(DKScore),
    P10    = quantile(DKScore, 0.10),
    P25    = quantile(DKScore, 0.25),
    P75    = quantile(DKScore, 0.75),
    P90    = quantile(DKScore, 0.90)
  ), by = Player]
  
  projections <- merge(
    projections,
    player_data[, .(Player = Name, DKSalary = Salary, DKOwn = Own,
                    Match, Opponent, Surface, Tour)],
    by = "Player"
  )
  projections[, `:=`(
    PointsPerK = Mean / (DKSalary / 1000),
    Ceiling    = P90,
    Floor      = P10
  )]
  setorder(projections, -Mean)
  
  # --------------------------------------------------------------------------
  # METADATA — standardised column names (DKSalary, DKID, DKOwn)
  # --------------------------------------------------------------------------
  metadata <- unique(player_data[, .(
    Player   = Name,
    DKSalary = Salary,
    DKID     = ID,
    DKOwn    = Own,
    Match    = Match,
    Opponent = Opponent,
    Surface  = Surface,
    Tour     = Tour
  )])
  
  # --------------------------------------------------------------------------
  # MATCH ANALYSIS VISUALS
  # --------------------------------------------------------------------------
  cb(0.90, "Building match analysis...")
  match_analysis_data <- list()
  
  for (match_name in unique(player_data$Match)) {
    mp <- player_data[Match == match_name]
    if (nrow(mp) != 2) next
    p1 <- mp[1]; p2 <- mp[2]
    
    p1_ml_raw <- odds_to_probability(as.numeric(p1$ML))
    p2_ml_raw <- odds_to_probability(as.numeric(p2$ML))
    total_ml  <- p1_ml_raw + p2_ml_raw
    
    p1_sims <- sim_results[Player == p1$Name]
    p2_sims <- sim_results[Player == p2$Name]
    
    h2h <- merge(
      p1_sims[, .(SimID, P1_Result = Result, P1_Outcome = Outcome)],
      p2_sims[, .(SimID, P2_Result = Result, P2_Outcome = Outcome)],
      by = "SimID"
    )
    
    match_analysis_data[[match_name]] <- data.table(
      Match      = match_name,
      Player     = c(p1$Name, p2$Name),
      Salary     = c(p1$Salary, p2$Salary),
      ImpliedWin = c(p1_ml_raw / total_ml, p2_ml_raw / total_ml),
      SimWin     = c(mean(h2h$P1_Result == "Winner"), mean(h2h$P2_Result == "Winner")),
      WinDiff    = c((mean(h2h$P1_Result == "Winner") - p1_ml_raw / total_ml) * 100,
                     (mean(h2h$P2_Result == "Winner") - p2_ml_raw / total_ml) * 100),
      ImpliedSS  = c(odds_to_probability(as.numeric(p1$SS)) / total_ml,
                     odds_to_probability(as.numeric(p2$SS)) / total_ml),
      SimSS      = c(mean(h2h$P1_Result == "Winner" & h2h$P1_Outcome == "SS"),
                     mean(h2h$P2_Result == "Winner" & h2h$P2_Outcome == "SS")),
      AvgWinPts  = c(mean(p1_sims[Result == "Winner", DKScore]),
                     mean(p2_sims[Result == "Winner", DKScore]))
    )
  }
  
  cb(1.0, "Tennis simulation complete!")
  
  list(
    sim_results  = sim_results,
    metadata     = metadata,
    projections  = projections,
    full_results = sim_results,
    sport_visuals = list(
      match_analysis = rbindlist(match_analysis_data),
      score_distributions = list(
        all_wins = sim_results[Result == "Winner",
                               .(Player, SimID, Score = DKScore, Outcome)],
        ss_wins  = sim_results[Result == "Winner" & Outcome == "SS",
                               .(Player, SimID, Score = DKScore)],
        nss_wins = sim_results[Result == "Winner" & Outcome == "NSS",
                               .(Player, SimID, Score = DKScore)]
      ),
      player_data = player_data
    )
  )
}

# ============================================================================
# TENNIS LINEUP METRICS
# Called by add_custom_metrics() for any non-win_based optimization path.
# (win_based path pre-calculates these in OptimalLineups_Core directly.)
# ============================================================================

calculate_tennis_lineup_metrics <- function(scored_lineups, sim_results, metadata) {
  
  cat("Calculating tennis lineup metrics...\n")
  setDT(scored_lineups); setDT(sim_results); setDT(metadata)
  
  if (!"Win" %in% names(sim_results)) {
    warning("Win column not found — skipping tennis lineup metrics")
    return(scored_lineups)
  }
  
  player_cols <- grep("^Player[0-9]", names(scored_lineups), value = TRUE)
  roster_size <- length(player_cols)
  n_lineups   <- nrow(scored_lineups)
  
  # Match map: player -> match label
  player_match_map <- setNames(metadata$Match, metadata$Player)
  
  # Individual EW lookup table
  ind_ew <- sim_results[, .(IndividualEW = mean(Win)), by = Player]
  setkey(ind_ew, Player)
  
  # Wide win matrix: players x sims
  win_wide <- dcast(sim_results, Player ~ SimID, value.var = "Win", fill = 0L)
  setkey(win_wide, Player)
  sim_cols <- setdiff(names(win_wide), "Player")
  win_mat  <- as.matrix(win_wide[, ..sim_cols])
  rownames(win_mat) <- win_wide$Player
  
  total_ew     <- numeric(n_lineups)
  win6_pct     <- numeric(n_lineups)
  win5plus_pct <- numeric(n_lineups)
  
  for (i in seq_len(n_lineups)) {
    players <- as.character(unlist(scored_lineups[i, ..player_cols]))
    matches <- player_match_map[players]
    
    # TotalEW: per match, sum individual EW if 1 player, add 1.0 if 2 players
    ew <- 0
    for (m in unique(matches[!is.na(matches)])) {
      m_players <- players[!is.na(matches) & matches == m]
      if (length(m_players) == 1) {
        val <- ind_ew[.(m_players), IndividualEW]
        ew  <- ew + ifelse(!is.na(val), val, 0)
      } else {
        ew <- ew + 1.0
      }
    }
    total_ew[i] <- round(ew, 2)
    
    # Win6/Win5+
    valid <- players[players %in% rownames(win_mat)]
    if (length(valid) > 0) {
      wins_per_sim    <- colSums(win_mat[valid, , drop = FALSE])
      win6_pct[i]    <- round(mean(wins_per_sim >= roster_size) * 100, 1)
      win5plus_pct[i] <- round(mean(wins_per_sim >= (roster_size - 1L)) * 100, 1)
    }
  }
  
  scored_lineups[, TotalEW     := total_ew]
  scored_lineups[, Win6Pct     := win6_pct]
  scored_lineups[, Win5PlusPct := win5plus_pct]
  
  cat("Tennis metrics done\n\n")
  scored_lineups
}