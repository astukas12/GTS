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
# HELPER: serve / return profiles
# The database's Player_Profiles sheet (built by GTS/Tennis/tennis_db_lib.R,
# build_player_profiles) holds each player's last-12-month ace, double-fault
# and break indices (1 = tour average). Sheet names are matched to profile names
# with the same normalisation the database pipeline uses (name_key), then by
# unique token containment ("Felix Auger Aliassime" vs "Felix Auger-Aliassime").
# ============================================================================

tennis_name_key <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  x <- tolower(ifelse(is.na(x), "", x))
  x <- gsub("[^a-z ]", " ", x)
  x <- gsub("\\b(jr|sr|ii|iii)\\b", " ", x)
  trimws(gsub(" +", " ", x))
}

match_player_profiles <- function(names, tours, profiles) {
  keys <- tennis_name_key(names)
  out <- data.table(Name = names, Tour = tours, name_key = keys, prof_row = NA_integer_)
  for (i in seq_along(names)) {
    cand <- which(profiles$Tour == tours[i])
    hit <- cand[profiles$name_key[cand] == keys[i]]
    if (length(hit) != 1) {
      tk <- strsplit(keys[i], " ")[[1]]
      hit <- cand[vapply(strsplit(profiles$name_key[cand], " "), function(pk)
        all(tk %in% pk) || all(pk %in% tk), logical(1))]
    }
    if (length(hit) == 1) out$prof_row[i] <- hit
  }
  out
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

  # Serve / return profiles (absent from databases built before 15 Sep 2026 --
  # then no player gets a shift and the engine runs as before)
  db_sheets <- excel_sheets(hist_file)
  player_profiles <- if ("Player_Profiles" %in% db_sheets)
    as.data.table(read_excel(hist_file, sheet = "Player_Profiles")) else NULL

  # Serve rescore (15 Sep 2026; replaces the flat per-player shift). Each sim
  # keeps its sampled match's scoreline and breaks; the player's own aces and
  # DFs in that match are rebuilt from their profile, then rescored with DK's
  # ace, DF, 10+/15+ ace and no-DF rules. Index above 1: add extra, in
  # proportion to games played in that match (games x tour rate x (idx - 1));
  # below 1: keep each of the match's own with probability idx. Powers soften
  # the index, per tour, picked on 2025 slates. Backtest (GTS/Tennis/backtest/
  # serve_tails.R): means and lineup ranks match the flat shift, but bonus
  # rates are now player-specific -- ATP top-ace-tercile 10+ ace rate 21% ->
  # 34% of sims (41% real), heavy WTA double-faulters' no-DF 7% -> 2.6% (2.9%).
  # No profile (or < 5 matches): the sampled match's own counts, unchanged.
  # WTA DF power 1.0 -> 0.8 (15 Sep): clean servers' no-DF rate 15% -> 13% of
  # sims (11% real); backtest/wta_df_power_check.R.
  SERVE_RESCORE_POWER <- data.table(Tour = c("ATP", "WTA"), b_ace = c(0.8, 1.0), b_df = c(0.8, 0.8))
  serve_rates <- historical_data[!is.na(w_ace) & !is.na(l_ace) & !is.na(w_df) & !is.na(l_df) &
                                   (w_games_won + l_games_won) > 0,
    .(rate_ace = sum(w_ace + l_ace) / (2 * sum(w_games_won + l_games_won)),
      rate_df  = sum(w_df + l_df)   / (2 * sum(w_games_won + l_games_won))),
    by = .(tour, surface, best_of)]

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
  # Matches left out of the sim: no ML on the sheet, or no historical pool.
  # Their players get no sim rows and are removed from metadata below.
  dropped_matches <- character(0)

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
    
    # ---- WALKOVER: explicit WD/WO tour flag only ----
    is_walkover <- any(c(p1$Tour, p2$Tour) %in% c("WD", "WO"))

    # ---- MISSING ML: leave the match out ----
    # A blank ML used to be read as a walkover (flat 30 / 0 in every sim). The
    # W3 backtest found 144 played matches simmed that way, real winners off by
    # 47 DK points each. No price, no sim, until the sheet has one.
    if (!is_walkover && (is.na(suppressWarnings(as.numeric(p1$ML))) ||
                         is.na(suppressWarnings(as.numeric(p2$ML))))) {
      cat(" - DROPPED (missing ML)\n")
      warning(sprintf("Match '%s' left out of the sim: missing ML on the sheet", match_name))
      dropped_matches <- c(dropped_matches, match_name)
      next
    }

    if (is_walkover) {
      cat(" - WALKOVER\n")
      winner_name <- if (p1$Tour %in% c("WD", "WO")) p2$Name else p1$Name
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
      
      # Uniform weights inside the window. The old 1/rank weights left ~55
      # effective historical matches per player and, since prices tie at 0.1,
      # favoured older rows by file order. Uniform was better on every held-out
      # measure in the W3 backtest (GTS/Tennis/backtest/FINDINGS.md).
      pool[, sample_weight := 1]
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

    # ---- NO HISTORY: leave the match out rather than invent scores ----
    # (was runif(50,70) / runif(20,40) with only a warning). An empty pool means
    # the sheet's Tour/Surface/BO combination isn't in the database.
    if (any(vapply(pools, is.null, logical(1)))) {
      cat(" - DROPPED (no historical pool: check Tour/Surface/BO)\n")
      warning(sprintf("Match '%s' left out of the sim: no historical matches for %s / %s / BO%s",
                      match_name, p1$Tour, p1$Surface, p1$BO))
      dropped_matches <- c(dropped_matches, match_name)
      next
    }

    match_cache[[match_name]] <- list(
      type      = "normal",
      tour      = p1$Tour, surface = p1$Surface, bo = as.numeric(p1$BO),
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

  # Serve rescore setup (see SERVE_RESCORE_POWER above)
  serve_prof <- data.table(Name = character(0), ace_m = numeric(0), df_m = numeric(0))
  if (!is.null(player_profiles)) {
    mp <- match_player_profiles(player_data$Name, player_data$Tour, player_profiles)
    mp <- mp[!is.na(prof_row)]
    mp <- cbind(mp, player_profiles[mp$prof_row, .(n_matches, ace_idx, df_idx)])
    mp <- merge(mp[n_matches >= 5], SERVE_RESCORE_POWER, by = "Tour")
    serve_prof <- mp[, .(Name, ace_m = ace_idx^b_ace, df_m = df_idx^b_df)]
  }
  serve_redraw <- function(cnt, G, rate, m) {
    ok <- !is.na(cnt) & !is.na(G)
    out <- cnt
    if (m > 1)      out[ok] <- cnt[ok] + rpois(sum(ok), G[ok] * rate * (m - 1))
    else if (m < 1) out[ok] <- rbinom(sum(ok), cnt[ok], m)
    out
  }
  # DK points change for one player's sims from rebuilding their aces / DFs
  serve_delta <- function(name, ace, df, G, rt, bo5) {
    p <- serve_prof[Name == name]
    if (!nrow(p) || nrow(rt) != 1 || !length(ace)) return(0)
    a2 <- serve_redraw(ace, G, rt$rate_ace, p$ace_m[1])
    d2 <- serve_redraw(df,  G, rt$rate_df,  p$df_m[1])
    v_ace <- if (bo5) 0.25 else 0.4; thr <- if (bo5) 15 else 10; nodf <- if (bo5) 5 else 2.5
    out <- v_ace * (a2 - ace) - (d2 - df) + 2 * ((a2 >= thr) - (ace >= thr)) + nodf * ((d2 == 0) - (df == 0))
    out[is.na(out)] <- 0
    out
  }
  serve_acc <- list()
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
    w_ace_s <- w_df_s <- l_ace_s <- l_df_s <- g_s <- rep(NA_real_, n_sims)
    
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
        w_ace_s[idx] <- pool$w_ace[drawn]; w_df_s[idx] <- pool$w_df[drawn]
        l_ace_s[idx] <- pool$l_ace[drawn]; l_df_s[idx] <- pool$l_df[drawn]
        g_s[idx]     <- pool$w_games_won[drawn] + pool$l_games_won[drawn]
      }
    }

    # ---- Serve rescore: rebuild each profiled player's aces / DFs per sim ----
    if (nrow(serve_prof)) {
      rt  <- serve_rates[tour == info$tour & surface == info$surface & best_of == info$bo]
      bo5 <- isTRUE(info$bo == 5)
      for (pn in intersect(c(info$p1_name, info$p2_name), serve_prof$Name)) {
        iw <- which(winner_vec == pn); il <- which(loser_vec == pn)
        dw <- serve_delta(pn, w_ace_s[iw], w_df_s[iw], g_s[iw], rt, bo5)
        dl <- serve_delta(pn, l_ace_s[il], l_df_s[il], g_s[il], rt, bo5)
        winner_scores[iw] <- winner_scores[iw] + dw
        loser_scores[il]  <- loser_scores[il]  + dl
        serve_acc[[pn]] <- (sum(dw) + sum(dl)) / n_sims
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

  # Serve rescore summary: mean DK change per rescored player (applied per sim above)
  serve_shift <- data.table(Player = names(serve_acc), ServeShift = unlist(serve_acc, use.names = FALSE))
  cat(sprintf("Serve profiles: %d of %d players rescored (mean change %+.1f to %+.1f pts)\n",
              nrow(serve_shift), uniqueN(sim_results[Outcome != "WO", Player]),
              if (nrow(serve_shift)) min(serve_shift$ServeShift) else 0,
              if (nrow(serve_shift)) max(serve_shift$ServeShift) else 0))
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

  # Tennis showdown (TENNIS_SHOWDOWN) sheets carry two extra draftableIds per
  # player -- DK prices the CPT and A-CPT slots separately from the base P
  # slot rather than as a clean multiple of it. Plain passthrough, no
  # simulation-affecting change: absent on a Classic sheet, so this is a no-op
  # there.
  if (all(c("CPTID", "CPTSalary", "ACPTID", "ACPTSalary") %in% names(player_data))) {
    showdown_cols <- unique(player_data[, .(
      Player = Name, CPTID, CPTSalary, ACPTID, ACPTSalary
    )])
    metadata <- merge(metadata, showdown_cols, by = "Player")
  }

  # Players in dropped matches have no sims -- keep them out of the optimizer
  if (length(dropped_matches)) {
    metadata <- metadata[!Match %in% dropped_matches]
    cat(sprintf("Left out of the sim (%d): %s\n", length(dropped_matches),
                paste(dropped_matches, collapse = "; ")))
  }

  # --------------------------------------------------------------------------
  # MATCH ANALYSIS VISUALS
  # --------------------------------------------------------------------------
  cb(0.90, "Building match analysis...")
  match_analysis_data <- list()

  for (match_name in unique(player_data$Match)) {
    if (match_name %in% dropped_matches) next
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
    dropped_matches = dropped_matches,
    serve_shift  = serve_shift,
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