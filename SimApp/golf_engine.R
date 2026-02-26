# ============================================================================
# GOLF ENGINE - Universal Sim App
# Golden Ticket Sims
# ============================================================================
# Outputs standardized columns expected by OptimalLineups_Core:
#   sim_results:  SimID, Player, DKScore, FDScore, Pool, FinishPosition
#   sim_metadata: Player, Pool, DKSalary, FDSalary, DKOwn, FDOwn,
#                 CutProb, TeeTimeGroup
# ============================================================================

library(data.table)
library(readxl)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================================
# INPUT READING
# ============================================================================

read_golf_input <- function(file_path) {
  sheet_names <- readxl::excel_sheets(file_path)
  if (!"Player" %in% sheet_names) stop("Golf input requires a 'Player' sheet.")
  if (!"DKPts"  %in% sheet_names) stop("Golf input requires a 'DKPts' sheet.")
  
  player_raw <- as.data.table(read_excel(file_path, sheet = "Player"))
  dk_pts_raw <- as.data.table(read_excel(file_path, sheet = "DKPts"))
  fd_pts_raw <- if ("FDPts" %in% sheet_names) as.data.table(read_excel(file_path, sheet = "FDPts")) else NULL
  
  list(
    player = player_raw,
    dk_pts = dk_pts_raw,
    fd_pts = fd_pts_raw
  )
}

# ============================================================================
# DATA PROCESSING
# ============================================================================

process_golf_players <- function(player_dt) {
  dt <- copy(player_dt)
  
  # Probability columns
  for (col in c("W","T5","T10","T20","T30","T40","Cut")) {
    if (col %in% names(dt)) dt[, (col) := as.numeric(get(col))]
  }
  
  # Salary columns
  for (col in c("DKSalary","FDSalary")) {
    if (col %in% names(dt)) dt[, (col) := as.numeric(get(col))]
  }
  
  # Ownership - strip % if present
  for (col in c("DKOP","FDOP")) {
    if (col %in% names(dt)) {
      dt[, (col) := as.numeric(gsub("%", "", as.character(get(col))))]
    }
  }
  
  # Pool column - normalise to "Pool"
  if ("POOL" %in% names(dt) && !"Pool" %in% names(dt)) setnames(dt, "POOL", "Pool")
  if (!"Pool" %in% names(dt)) dt[, Pool := "Y"]
  dt[, Pool := as.character(Pool)]
  
  # Tee time group
  dt <- process_tee_times(dt)
  
  dt
}

process_tee_times <- function(dt) {
  r1_col <- intersect(c("Round 1 Tee Time", "R1TeeTime"), names(dt))[1]
  r2_col <- intersect(c("Round 2 Tee Time", "R2TeeTime"), names(dt))[1]
  
  if (is.na(r1_col) || is.na(r2_col)) {
    dt[, TeeTimeGroup := "Unknown"]
    return(dt)
  }
  
  parse_mins <- function(x) {
    x <- trimws(as.character(x))
    parts <- strsplit(x, ":")
    sapply(parts, function(p) {
      if (length(p) < 2 || any(is.na(suppressWarnings(as.numeric(p))))) return(NA_real_)
      as.numeric(p[1]) * 60 + as.numeric(p[2])
    })
  }
  
  r1 <- parse_mins(dt[[r1_col]])
  r2 <- parse_mins(dt[[r2_col]])
  
  dt[, TeeTimeGroup := fcase(
    r1 <  r2, "EarlyLate",
    r1 >  r2, "LateEarly",
    default = "Unknown"
  )]
  dt
}

process_golf_pts_table <- function(pts_dt) {
  if (is.null(pts_dt) || nrow(pts_dt) == 0) return(NULL)
  dt <- copy(pts_dt)
  dt[, Rank := as.numeric(Rank)]
  score_cols <- setdiff(names(dt), "Rank")
  for (col in score_cols) dt[, (col) := as.numeric(get(col))]
  dt <- dt[!is.na(Rank)]
  setkey(dt, Rank)
  attr(dt, "score_columns") <- score_cols
  dt
}

# ============================================================================
# DISTRIBUTION PRE-COMPUTATION
# ============================================================================

precompute_golf_distributions <- function(players_dt, cut_line = 65, no_cut = FALSE) {
  n_players <- nrow(players_dt)
  wanted    <- if (no_cut) c("W","T5","T10","T20","T30","T40") else c("W","T5","T10","T20","T30","T40","Cut")
  avail     <- intersect(wanted, names(players_dt))
  prob_mat  <- as.matrix(players_dt[, ..avail])
  prob_mat[is.na(prob_mat)] <- 0
  
  if (no_cut) {
    n_cats <- 7L
    pos_ranges <- list(1L, 2:5, 6:10, 11:20, 21:30, 31:40, 41:n_players)
  } else {
    n_cats <- 8L
    pos_ranges <- list(1L, 2:5, 6:10, 11:20, 21:30, 31:40, 41:cut_line, (cut_line+1):n_players)
  }
  
  marg <- matrix(0, nrow = n_players, ncol = n_cats)
  
  for (i in seq_len(n_players)) {
    if (no_cut) {
      mp <- diff(c(0, prob_mat[i,], 1))
    } else {
      cut_p     <- if ("Cut" %in% avail) prob_mat[i, "Cut"] else 0.8
      fin_p     <- prob_mat[i, setdiff(avail, "Cut")]
      mp        <- c(diff(c(0, fin_p * cut_p, cut_p)), 1 - cut_p)
    }
    mp[mp < 0] <- 0
    s <- sum(mp)
    marg[i,] <- if (s > 0) mp / s else rep(1/n_cats, n_cats)
  }
  
  list(marginal_probs = marg, position_ranges = pos_ranges,
       n_players = n_players, cut_line = cut_line, no_cut = no_cut)
}

# ============================================================================
# POSITION SIMULATION
# ============================================================================

simulate_golf_positions <- function(dist, n_sims) {
  n_p    <- dist$n_players
  marg   <- dist$marginal_probs
  ranges <- dist$position_ranges
  cl     <- dist$cut_line
  no_cut <- dist$no_cut
  n_cats <- ncol(marg)
  missed_cat <- n_cats  # last category = missed cut (cut events only)
  
  pos_mat  <- matrix(0L, nrow = n_p, ncol = n_sims)
  rand_mat <- matrix(runif(n_p * n_sims), nrow = n_p)
  noise_mat <- matrix(runif(n_p * n_sims, 0, 0.02), nrow = n_p)
  cum_prob  <- t(apply(marg, 1, cumsum))
  
  batch_size <- 500L
  n_batches  <- ceiling(n_sims / batch_size)
  
  for (batch in seq_len(n_batches)) {
    s_sim <- (batch - 1L) * batch_size + 1L
    e_sim <- min(batch * batch_size, n_sims)
    
    for (sim in s_sim:e_sim) {
      rv   <- rand_mat[, sim]
      cat_idx <- rowSums(cum_prob < matrix(rv, nrow = n_p, ncol = n_cats)) + 1L
      cat_idx  <- pmin(cat_idx, n_cats)
      
      if (no_cut) {
        sc <- numeric(n_p)
        for (k in seq_along(ranges)) {
          idx_k <- which(cat_idx == k)
          if (length(idx_k) == 0) next
          rng <- ranges[[k]]
          sc[idx_k] <- rng[ceiling(runif(length(idx_k)) * length(rng))] + noise_mat[idx_k, sim]
        }
        pos_mat[, sim] <- rank(sc, ties.method = "random")
        
      } else {
        cm_score <- numeric(n_p)
        mc_score <- numeric(n_p)
        cut_makers <- cat_idx < missed_cat
        
        for (k in seq_along(ranges)) {
          idx_k <- which(cat_idx == k)
          if (length(idx_k) == 0) next
          rng <- ranges[[k]]
          mid <- mean(rng)
          if (k < missed_cat) {
            cm_score[idx_k] <- mid + noise_mat[idx_k, sim]
          } else {
            mc_score[idx_k] <- marg[idx_k, k] + noise_mat[idx_k, sim]
          }
        }
        
        # Enforce 65-80 cut makers
        n_cut <- sum(cut_makers)
        if (n_cut < 65) {
          mi      <- which(!cut_makers)
          promote <- mi[order(mc_score[mi], decreasing = TRUE)[seq_len(min(65 - n_cut, length(mi)))]]
          cut_makers[promote] <- TRUE
          cm_score[promote]   <- cl - 5 + runif(length(promote), 0, 10)
          n_cut <- sum(cut_makers)
        } else if (n_cut > 80) {
          ci     <- which(cut_makers)
          demote <- ci[order(cm_score[ci], decreasing = TRUE)[seq_len(n_cut - 80)]]
          cut_makers[demote] <- FALSE
          mc_score[demote]   <- marg[demote, n_cats] + runif(length(demote), 0, 0.1)
          n_cut <- sum(cut_makers)
        }
        
        fp <- integer(n_p)
        if (n_cut > 0) fp[cut_makers]  <- as.integer(rank(cm_score[cut_makers],  ties.method = "random"))
        mc_idx <- which(!cut_makers)
        if (length(mc_idx) > 0) fp[mc_idx] <- as.integer(rank(-mc_score[mc_idx], ties.method = "random")) + n_cut
        pos_mat[, sim] <- fp
      }
    }
    
    if (n_sims > 2000 && batch %% max(1L, n_batches %/% 10L) == 0L)
      cat(sprintf("  Positions: %.0f%%\n", batch / n_batches * 100))
  }
  
  pos_mat
}

# ============================================================================
# POINTS CACHE (random score column per batch for payout variance)
# ============================================================================

build_points_cache <- function(pts_dt) {
  score_cols <- attr(pts_dt, "score_columns") %||% setdiff(names(pts_dt), "Rank")
  sel_col    <- sample(score_cols, 1)
  max_rank   <- max(pts_dt$Rank, na.rm = TRUE)
  lookup     <- numeric(max_rank)
  ra         <- pts_dt$Rank
  sa         <- pts_dt[[sel_col]]
  for (r in seq_len(max_rank)) {
    cands <- which(ra <= r)
    if (length(cands) > 0) lookup[r] <- sa[max(cands)]
  }
  lookup
}

lookup_points <- function(positions, cache) {
  if (is.null(cache)) return(rep(0, length(positions)))
  cache[pmax(1L, pmin(as.integer(positions), length(cache)))]
}

# ============================================================================
# MAIN SIMULATION FUNCTION
# ============================================================================

run_golf_simulation <- function(input_data, n_sims = 10000,
                                cut_line = 65, no_cut = FALSE,
                                progress_callback = NULL) {
  t0 <- Sys.time()
  
  # Process
  players_dt <- process_golf_players(input_data$player)
  dk_pts     <- process_golf_pts_table(input_data$dk_pts)
  fd_pts     <- process_golf_pts_table(input_data$fd_pts)
  
  has_dk <- !is.null(dk_pts) && "DKSalary" %in% names(players_dt)
  has_fd <- !is.null(fd_pts) && "FDSalary" %in% names(players_dt)
  n_p    <- nrow(players_dt)
  
  cat(sprintf("Golf sim | %d players | %d sims | cut_line=%d | no_cut=%s\n",
              n_p, n_sims, cut_line, no_cut))
  
  progress_callback %||% (function(...) NULL)
  cb <- progress_callback %||% function(v, m) invisible()
  
  cb(0.05, "Pre-computing distributions...")
  dist <- precompute_golf_distributions(players_dt, cut_line, no_cut)
  
  cb(0.10, "Simulating finish positions...")
  pos_mat <- simulate_golf_positions(dist, n_sims)
  
  cb(0.55, "Calculating fantasy points...")
  
  # Score matrices - process in batches so score column varies per batch
  batch_size <- 500L
  n_batches  <- ceiling(n_sims / batch_size)
  dk_mat <- if (has_dk) matrix(0, nrow = n_p, ncol = n_sims) else NULL
  fd_mat <- if (has_fd) matrix(0, nrow = n_p, ncol = n_sims) else NULL
  
  for (batch in seq_len(n_batches)) {
    s <- (batch - 1L) * batch_size + 1L
    e <- min(batch * batch_size, n_sims)
    batch_pos <- pos_mat[, s:e, drop = FALSE]
    
    if (has_dk) {
      cache <- build_points_cache(dk_pts)
      dk_mat[, s:e] <- matrix(lookup_points(as.integer(batch_pos), cache), nrow = n_p)
    }
    if (has_fd) {
      cache <- build_points_cache(fd_pts)
      fd_mat[, s:e] <- matrix(lookup_points(as.integer(batch_pos), cache), nrow = n_p)
    }
  }
  
  cb(0.82, "Building output tables...")
  
  # Long-format sim_results
  sim_ids    <- rep(seq_len(n_sims), each = n_p)
  player_rep <- rep(players_dt$Name, times = n_sims)
  
  sim_results <- data.table(
    SimID          = sim_ids,
    Player         = player_rep,
    Pool           = rep(players_dt$Pool, times = n_sims),
    FinishPosition = as.integer(as.vector(pos_mat)),
    DKScore        = if (has_dk) as.vector(dk_mat) else 0,
    FDScore        = if (has_fd) as.vector(fd_mat) else 0
  )
  
  # Metadata (one row per player)
  sim_metadata <- data.table(
    Player       = players_dt$Name,
    Pool         = players_dt$Pool,
    TeeTimeGroup = players_dt$TeeTimeGroup %||% "Unknown",
    CutProb      = if ("Cut" %in% names(players_dt)) players_dt$Cut else 0.8
  )
  if (has_dk) {
    sim_metadata[, DKSalary := players_dt$DKSalary]
    sim_metadata[, DKOwn    := if ("DKOP" %in% names(players_dt)) players_dt$DKOP else 0]
  }
  if (has_fd) {
    sim_metadata[, FDSalary := players_dt$FDSalary]
    sim_metadata[, FDOwn    := if ("FDOP" %in% names(players_dt)) players_dt$FDOP else 0]
  }
  
  cat(sprintf("Golf sim done | %.1fs | %s rows\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs")),
              format(nrow(sim_results), big.mark = ",")))
  cb(1.0, "Simulation complete!")
  
  list(
    sim_results  = sim_results,
    sim_metadata = sim_metadata,
    has_dk       = has_dk,
    has_fd       = has_fd,
    no_cut       = no_cut,
    cut_line     = cut_line,
    n_sims       = n_sims
  )
}

# ============================================================================
# GOLF PHASE 1: CANDIDATE POOL (cut tournaments)
# ============================================================================
# Returns a lineup_data list compatible with score_all_lineups() in
# OptimalLineups_Core. For no_cut events the caller passes NULL and
# uses the standard LP-per-sim path instead.

generate_golf_candidate_pool <- function(sim_results, sim_metadata, config,
                                         no_cut      = FALSE,
                                         n_sample    = 25000L,   # unused, kept for compat
                                         target_pool = 10000L,
                                         verbose     = TRUE) {
  if (no_cut) return(NULL)
  
  platform    <- config$platform
  salary_col  <- paste0(platform, "Salary")
  salary_cap  <- config$salary_cap
  salary_min  <- 48500L
  roster_size <- config$roster_size
  
  # ------------------------------------------------------------------
  # PRE-FILTER: determine eligible players
  # If POOL column exists with Y values -> use only those players
  # Otherwise -> top 70 by CutProb
  # ------------------------------------------------------------------
  has_pool <- "Pool" %in% names(sim_metadata) &&
    any(sim_metadata$Pool == "Y", na.rm = TRUE)
  
  # Start with salary-feasible players (must be affordable in a 6-player lineup)
  # Max individual salary = cap - (5 * min_salary_of_others), but simpler:
  # just exclude anyone whose salary alone exceeds cap - (roster_size-1)*min_sal
  all_valid <- sim_metadata[!is.na(get(salary_col)) & get(salary_col) > 0]
  min_sal   <- min(all_valid[[salary_col]])
  max_afford <- salary_cap - (roster_size - 1L) * min_sal
  all_valid <- all_valid[get(salary_col) <= max_afford]
  
  if (has_pool) {
    eligible <- all_valid[Pool == "Y"]
    if (verbose) cat(sprintf("\nGolf Phase 1 [%s]: POOL filter -> %d players (after salary feasibility)\n",
                             platform, nrow(eligible)))
  } else {
    eligible <- all_valid
    if (verbose) cat(sprintf("\nGolf Phase 1 [%s]: No POOL — %d salary-feasible players\n",
                             platform, nrow(eligible)))
  }
  
  # Trim to top 70 by CutProb if still too large to enumerate
  if (nrow(eligible) > 70L) {
    setorder(eligible, -CutProb)
    eligible <- eligible[1:70L]
    if (verbose) cat(sprintf("  Trimmed to top 70 by CutProb: %.0f%% - %.0f%%\n",
                             min(eligible$CutProb)*100, max(eligible$CutProb)*100))
  }
  
  if (nrow(eligible) < roster_size)
    stop("Not enough eligible players with ", salary_col, " to build lineups.")
  
  # ------------------------------------------------------------------
  # SAMPLE valid salary combos — run until target_pool found or max_iter hit
  # Much faster than enumerating 100M+ combos when salary window is tight
  # ------------------------------------------------------------------
  n_pool     <- nrow(eligible)
  salaries   <- eligible[[salary_col]]
  cut_probs  <- eligible$CutProb
  players    <- eligible$Player
  
  if (verbose) cat(sprintf("  Sampling from %d players ($%.0fk-$%.0fk window)...\n",
                           n_pool, salary_min/1000, salary_cap/1000))
  
  target_sample <- 25000L
  max_iter      <- target_sample * 200L
  found_idx     <- vector("list", target_sample)
  found_sal     <- numeric(target_sample)
  found_ec      <- numeric(target_sample)
  n_found       <- 0L
  
  for (iter in seq_len(max_iter)) {
    idx <- sample.int(n_pool, roster_size, replace = FALSE)
    ts  <- sum(salaries[idx])
    if (ts >= salary_min && ts <= salary_cap) {
      n_found <- n_found + 1L
      found_idx[[n_found]] <- sort(idx)
      found_sal[n_found]   <- ts
      found_ec[n_found]    <- sum(cut_probs[idx])
      if (n_found >= target_sample) break
    }
  }
  
  if (n_found == 0) stop("No salary-valid lineups found. Check salary data and cap.")
  
  found_idx <- found_idx[seq_len(n_found)]
  found_sal <- found_sal[seq_len(n_found)]
  found_ec  <- found_ec[seq_len(n_found)]
  
  if (verbose) cat(sprintf("  Found %s valid combos\n", format(n_found, big.mark = ",")))
  
  # Build data.table, deduplicate, keep top by ExpectedCuts
  player_mat <- do.call(rbind, lapply(found_idx, function(i) players[i]))
  lineup_dt  <- as.data.table(player_mat)
  setnames(lineup_dt, paste0("Player", seq_len(roster_size)))
  lineup_dt[, TotalSalary  := found_sal]
  lineup_dt[, ExpectedCuts := found_ec]
  
  key_cols  <- paste0("Player", seq_len(roster_size))
  lineup_dt <- unique(lineup_dt, by = key_cols)
  setorder(lineup_dt, -ExpectedCuts)
  if (nrow(lineup_dt) > 5000L) lineup_dt <- lineup_dt[seq_len(5000L)]
  
  if (verbose)
    cat(sprintf("  Candidate pool: %s lineups | ExpCuts %.2f - %.2f\n",
                format(nrow(lineup_dt), big.mark = ","),
                min(lineup_dt$ExpectedCuts), max(lineup_dt$ExpectedCuts)))
  
  # Add analytical cut probability metrics
  lineup_dt <- add_golf_cut_metrics(lineup_dt, sim_metadata, roster_size)
  
  # Build opt_config in the format score_all_lineups() expects from lineup_data$config
  opt_config <- list(
    platform_col  = paste0(platform, "Score"),
    roster_size   = roster_size,
    salary_cap    = salary_cap
  )
  
  # Return in format expected by score_all_lineups()
  list(
    unique_lineups = lineup_dt,
    n_sims         = length(unique(sim_results$SimID)),
    config         = opt_config,
    mode           = "golf_cut",
    roster_size    = roster_size,
    player_cols    = paste0("Player", seq_len(roster_size))
  )
}

# ============================================================================
# CUT METRICS (DP - vectorized over lineups)
# ============================================================================

calculate_cut_distribution_dp <- function(cut_probs) {
  n  <- length(cut_probs)
  dp <- numeric(n + 1L)
  dp[1L] <- 1
  for (i in seq_len(n)) {
    p      <- cut_probs[i]
    new_dp <- numeric(n + 1L)
    for (j in 0:i) {
      new_dp[j + 1L] <- (if (j > 0L) dp[j] * p else 0) + dp[j + 1L] * (1 - p)
    }
    dp <- new_dp
  }
  # atleast[k+1] = P(at least k make cut), index 1 = atleast 0 = 1.0
  atleast <- rev(cumsum(rev(dp)))
  list(exact = dp, atleast = atleast)
}

add_golf_cut_metrics <- function(lineup_dt, sim_metadata, roster_size) {
  cut_lookup  <- setNames(sim_metadata$CutProb, sim_metadata$Player)
  player_cols <- paste0("Player", seq_len(roster_size))
  n           <- nrow(lineup_dt)
  
  at6 <- numeric(n)
  at5 <- numeric(n)
  ec  <- numeric(n)
  
  for (i in seq_len(n)) {
    ps         <- unlist(lineup_dt[i, ..player_cols])
    probs      <- cut_lookup[ps]
    probs[is.na(probs)] <- 0.8
    ec[i]  <- sum(probs)
    d      <- calculate_cut_distribution_dp(probs)
    at6[i] <- round(d$atleast[7L] * 100, 1)   # P(>=6) index 7 = atleast[6+1]
    at5[i] <- round(d$atleast[6L] * 100, 1)   # P(>=5) index 6
  }
  
  lineup_dt[, ExpectedCuts := round(ec, 2)]
  lineup_dt[, AtLeast6     := at6]
  lineup_dt[, AtLeast5     := at5]
  lineup_dt
}

# ============================================================================
# GOLF CUSTOM METRICS (called by app.R add_custom_metrics)
# ============================================================================

calculate_golf_lineup_metrics <- function(scored_lineups, sim_results,
                                          sim_metadata, no_cut = FALSE) {
  if (is.null(scored_lineups) || nrow(scored_lineups) == 0) return(scored_lineups)
  
  player_cols <- grep("^Player\\d+$", names(scored_lineups), value = TRUE)
  roster_size <- length(player_cols)
  if (roster_size == 0) return(scored_lineups)
  
  setDT(scored_lineups)
  
  # Cut metrics (skip for no_cut; Phase 1 may have already added them)
  if (!no_cut && "CutProb" %in% names(sim_metadata)) {
    if (!"AtLeast6" %in% names(scored_lineups)) {
      scored_lineups <- add_golf_cut_metrics(scored_lineups, sim_metadata, roster_size)
    }
  }
  
  # Tee time EarlyLate count
  if ("TeeTimeGroup" %in% names(sim_metadata)) {
    tee_map <- setNames(sim_metadata$TeeTimeGroup, sim_metadata$Player)
    el <- integer(nrow(scored_lineups))
    for (i in seq_len(nrow(scored_lineups))) {
      ps    <- unlist(scored_lineups[i, ..player_cols])
      grps  <- tee_map[ps]
      el[i] <- sum(grps == "EarlyLate", na.rm = TRUE)
    }
    scored_lineups[, EarlyLateCount := el]
  }
  
  scored_lineups
}