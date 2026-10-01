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
  event_raw  <- if ("Event" %in% sheet_names) as.data.table(read_excel(file_path, sheet = "Event")) else NULL
  
  list(
    player = player_raw,
    dk_pts = dk_pts_raw,
    fd_pts = fd_pts_raw,
    event  = event_raw     # optional (engine v2): Par, Level, CutN, CutAfter, FieldSize
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
          promote <- mi[order(mc_score[mi], decreasing = FALSE)[seq_len(min(65 - n_cut, length(mi)))]]
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
        if (length(mc_idx) > 0) fp[mc_idx] <- as.integer(rank(mc_score[mc_idx], ties.method = "random")) + n_cut
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

# v1, kept for the P4 bench only. The app runs v2 (golf_engine_v2.R, sourced at
# the end of this file), which redefines run_golf_simulation().
run_golf_simulation_v1 <- function(input_data, n_sims = 10000,
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
  
  # Explicit is.null, not %||%: engines share one environment and a later
  # engine's %||% (cfb_engine.R) calls is.na(a[1]), which errors on a function.
  cb <- if (is.null(progress_callback)) function(v, m) invisible() else progress_callback
  
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
    sim_metadata[, DKID     := if ("DKID" %in% names(players_dt)) players_dt$DKID else NA_character_]
  }
  if (has_fd) {
    sim_metadata[, FDSalary := players_dt$FDSalary]
    sim_metadata[, FDOwn    := if ("FDOP" %in% names(players_dt)) players_dt$FDOP else 0]
    sim_metadata[, FDID     := if ("FDID" %in% names(players_dt)) players_dt$FDID else NA_character_]
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
# GOLF PHASE 1: CANDIDATE POOL (exact, 30 Sep 2026 -- PLAN.md P2)
# ============================================================================
# Every sim's exact optimal 6 under the cap, pooled. Golf is NASCAR's problem
# (6 players, one cap, no positions), so this is NASCAR's knapsack DP
# (find_optimal_lineups_combinatorial, OptimalLineups_Core.R) -- not a new
# solver. Cut and no-cut events take the same path; the cut metrics are added
# AFTER, as columns, and no longer choose the pool.
#
# Replaced a sampler: the 70 likeliest cut-makers, 25,000 random salary-valid
# lineups, keep the 5,000 with the most expected cuts. On Bank of Utah (1k sims)
# its best lineup hit the per-sim optimum in 0% of sims, a median 73.5 DK pts
# (9.9%) short; Biltmore 10k: 10% short, and 57% of optima used a golfer it had
# excluded. The no-cut path was per-sim lpSolve, which is not exact either
# (lpsolve-not-exact).
#
# Every sim's optimum is a different lineup (1,000 sims -> 1,000 distinct), as
# on NFL classic, so Top1Count is 1 everywhere and cannot rank the pool.
# `max_lineups` caps it; at or above n_sims the pool holds every optimum.
#
# opt_data is prepare_optimization_data() output (SimID, Player, Salary,
# FantasyPoints, ...). Returns lineup_data for score_all_lineups().

generate_golf_candidate_pool <- function(opt_data, sim_metadata, config,
                                         no_cut      = FALSE,
                                         max_lineups = 25000L,
                                         verbose     = TRUE,
                                         progress_callback = NULL) {
  roster_size <- config$roster_size
  opt_config  <- list(platform_col = paste0(config$platform, "Score"),
                      roster_size  = roster_size,
                      salary_cap   = config$salary_cap,
                      max_lineups  = max_lineups)
  # The sheet's POOL column: when any golfer is Y, lineups are built from the Y
  # golfers only (N takes a golfer out of the optimal-lineup solve; he is still
  # simulated, so the field and everyone's finishes are unchanged).
  if ("Pool" %in% names(sim_metadata) && any(sim_metadata$Pool == "Y", na.rm = TRUE)) {
    in_pool  <- sim_metadata[Pool == "Y", Player]
    opt_data <- opt_data[Player %in% in_pool]
    if (verbose) cat(sprintf("  POOL filter: %d golfers marked Y\n", length(in_pool)))
  }
  ld <- find_optimal_lineups_combinatorial(opt_data, modifyList(opt_config, list(max_lineups = Inf)),
                                           verbose = verbose, progress_callback = progress_callback)
  lineup_dt <- ld$unique_lineups
  # The cap ranks by each optimum's top-5% rate among ALL the distinct optima
  # (ps_top_frac, as NHL / NFL / CFB classic), not Top1Count, which is 1 for all.
  if (nrow(lineup_dt) > max_lineups) {
    pc <- paste0("Player", seq_len(roster_size))
    t5 <- ps_top_frac(lineup_dt, pc, opt_data, "FantasyPoints", n_sims_use = 5000L, frac = 0.05)
    if (!is.null(t5)) { lineup_dt[, top5 := t5]; setorder(lineup_dt, -top5, -AvgScore); lineup_dt[, top5 := NULL] }
    else { setorder(lineup_dt, -AvgScore)
      if (verbose) cat("  top5 ranking unavailable (Matrix / matrixStats): capping by mean\n") }
    lineup_dt <- head(lineup_dt, max_lineups)
    if (verbose) cat(sprintf("  Capped to %s lineups by top-5%% rate\n", format(max_lineups, big.mark = ",")))
  }
  if (!no_cut) lineup_dt <- add_golf_cut_metrics(lineup_dt, sim_metadata, roster_size)

  list(
    unique_lineups = lineup_dt,
    n_sims         = ld$n_sims,
    config         = opt_config,
    mode           = "golf_exact",
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

# Same Poisson-binomial DP as calculate_cut_distribution_dp, run on every lineup
# at once (one column per make-count) -- the exact pool is ~n_sims lineups, and
# the row-by-row loop it replaces took minutes at that size.
add_golf_cut_metrics <- function(lineup_dt, sim_metadata, roster_size) {
  cut_lookup  <- setNames(sim_metadata$CutProb, sim_metadata$Player)
  player_cols <- paste0("Player", seq_len(roster_size))
  P <- matrix(cut_lookup[as.matrix(lineup_dt[, ..player_cols])], ncol = roster_size)
  P[is.na(P)] <- 0.8

  dp <- matrix(0, nrow(P), roster_size + 1L); dp[, 1L] <- 1   # dp[, j+1] = P(exactly j)
  for (i in seq_len(roster_size)) {
    p  <- P[, i]
    dp <- dp * (1 - p) + cbind(0, dp[, -(roster_size + 1L), drop = FALSE]) * p
  }

  lineup_dt[, ExpectedCuts := round(rowSums(P), 2)]
  lineup_dt[, AtLeast6     := round(dp[, roster_size + 1L] * 100, 1)]              # P(>= 6)
  lineup_dt[, AtLeast5     := round((dp[, roster_size] + dp[, roster_size + 1L]) * 100, 1)]  # P(>= 5)
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
    grps <- matrix(tee_map[as.matrix(scored_lineups[, ..player_cols])], ncol = roster_size)
    scored_lineups[, EarlyLateCount := as.integer(rowSums(grps == "EarlyLate", na.rm = TRUE))]
  }
  
  scored_lineups
}

# ---- engine v2 (round-score sim) replaces v1's run_golf_simulation --------
# Found next to this file, so the app (wd SimApp/) and headless scripts that
# source SimApp/golf_engine.R by full path both get it.
local({
  d <- "."
  for (i in rev(seq_len(sys.nframe()))) { f <- sys.frame(i)$ofile; if (!is.null(f)) { d <- dirname(f); break } }
  GOLF_ENGINE_DIR <<- normalizePath(d, winslash = "/")
})
source(file.path(GOLF_ENGINE_DIR, "golf_engine_v2.R"))
