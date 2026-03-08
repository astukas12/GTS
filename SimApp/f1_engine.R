# ============================================================================
# F1 DFS SIMULATION ENGINE
# Golden Ticket Sims — Universal App
#
# CONTRACT:
#   run_f1_simulation(input_data, n_sims, config, progress_callback)
#   -> list(sim_results, metadata, has_fd=FALSE, sport_visuals)
#
# sim_results — one row per DRIVER per sim + one row per CONSTRUCTOR per sim:
#   Drivers:     SimID | Player | PlayerType | DKScore | CptScore |
#                FinishPts | GridDiffPts | FL_Pts | LL_Pts | BeatTM_Pts | Classified_Pts |
#                Finish | Grid | LapsLed | FastestLap | Classified | BeatTeammate | Team
#   Constructors: SimID | Player | PlayerType | DKScore (component cols = NA)
#
# metadata — one row per entity, NO DKOwn:
#   Drivers:     Player | PlayerType | DKSalary | CptSalary | DKID | CptDFSID | Team | Grid
#   Constructors: Player | PlayerType | DKSalary | DKID
#
# DK Roster: 1 Captain (driver, 1.5x) + 4 Flex Drivers + 1 Constructor | $50k cap
# No ownership data for F1.
# ============================================================================

library(data.table)
library(readxl)
library(ggplot2)
library(plotly)

# ============================================================================
# TEAM COLORS
# ============================================================================

f1_team_colors <- c(
  "Red Bull Racing"      = "#0600EF",
  "Ferrari"              = "#DC0000",
  "Mercedes"             = "#00D2BE",
  "McLaren"              = "#FF8700",
  "Aston Martin F1 Team" = "#006F62",
  "Alpine F1 Team"       = "#0090FF",
  "Williams"             = "#005AFF",
  "Racing Bulls F1 Team" = "#2B4562",
  "Audi F1 Team"         = "#BF0000",
  "Haas F1 Team"         = "#808080",
  "Cadillac"             = "#D4AF37"
)

get_f1_color <- function(team) {
  unname(ifelse(team %in% names(f1_team_colors), f1_team_colors[team], "#555555"))
}

# DK points per finish position P1-P22
F1_POS_PTS <- c(40, 37, 35, 32, 30, 27, 25, 23, 22, 20,
                17, 15, 13, 12, 10,  7,  5,  4,  3,  2, 1, 0)


# ============================================================================
# READ INPUT
# ============================================================================

read_f1_input <- function(file_path) {
  cat("Reading F1 input:", file_path, "\n")
  
  sheets   <- excel_sheets(file_path)
  required <- c("Drivers", "LL", "FL", "Classification", "Constructors")
  missing  <- setdiff(required, sheets)
  if (length(missing) > 0)
    stop("Missing F1 input sheets: ", paste(missing, collapse = ", "))
  
  drivers        <- as.data.table(read_excel(file_path, sheet = "Drivers"))
  ll_data        <- as.data.table(read_excel(file_path, sheet = "LL"))
  fl_probs       <- as.data.table(read_excel(file_path, sheet = "FL"))
  classification <- as.data.table(read_excel(file_path, sheet = "Classification"))
  constructors   <- as.data.table(read_excel(file_path, sheet = "Constructors"))
  
  drivers <- drivers[!is.na(Name) & Name != ""]
  
  pos_cols  <- as.character(1:22)
  missing_p <- setdiff(pos_cols, names(drivers))
  if (length(missing_p) > 0)
    stop("Missing probability columns in Drivers sheet: ", paste(missing_p, collapse = ", "))
  
  # Normalise each driver's finish prob row to sum to 1
  prob_mat <- as.matrix(drivers[, pos_cols, with = FALSE])
  prob_mat[is.na(prob_mat)] <- 0
  rs <- rowSums(prob_mat); rs[rs == 0] <- 1
  drivers[, (pos_cols) := as.data.table(prob_mat / rs)]
  
  cat(sprintf("  Drivers: %d | Constructors: %d | LL rows: %d\n",
              nrow(drivers), nrow(constructors), nrow(ll_data)))
  
  list(
    Drivers        = drivers,
    LL             = ll_data,
    FL             = fl_probs,
    Classification = classification,
    Constructors   = constructors
  )
}


# ============================================================================
# PRE-COMPUTATION  (called once before the sim loop)
# ============================================================================

precompute_f1_data <- function(drivers, ll_data, fl_probs, classification) {
  pos_cols <- as.character(1:22)
  
  # Finish probability matrix: n_drivers x 22
  prob_mat <- as.matrix(drivers[, pos_cols, with = FALSE])
  prob_mat[is.na(prob_mat)] <- 0
  
  # FL position weights: named vector pos -> prob (0 for missing positions)
  fl_pos_w <- setNames(rep(0, 22), as.character(1:22))
  if (!is.null(fl_probs) && nrow(fl_probs) > 0) {
    fl_pos_w[as.character(fl_probs$Finish)] <- fl_probs$Prob
  } else {
    fl_pos_w[as.character(1:10)] <- 1 / 10
  }
  
  # Classification distribution
  cls_n    <- as.integer(classification$NumClassified)
  cls_prob <- classification$Probability / sum(classification$Probability)
  
  # LL data: split by season into lightweight lists for fast access
  seasons      <- unique(ll_data$Season)
  ll_by_season <- lapply(seasons, function(s) {
    d <- ll_data[Season == s]
    list(grid = as.integer(d$Grid), finish = as.integer(d$Finish), ll = as.integer(d$LL))
  })
  
  # Teammate index pairs: list of 2-element integer vectors (pre-computed once)
  team_pairs <- lapply(unique(drivers$Team), function(t) which(drivers$Team == t))
  team_pairs <- team_pairs[sapply(team_pairs, length) == 2]
  
  list(
    prob_mat     = prob_mat,
    fl_pos_w     = fl_pos_w,
    cls_n        = cls_n,
    cls_prob     = cls_prob,
    ll_by_season = ll_by_season,
    n_seasons    = length(ll_by_season),
    team_pairs   = team_pairs,
    grid_pos     = as.integer(drivers$Grid),
    fl_pct       = drivers$FL,
    ll_max       = as.integer(drivers$LLMax),
    cls_pct      = drivers$ClassPct,
    n            = nrow(drivers)
  )
}


# ============================================================================
# FAST SIMULATION PRIMITIVES
# ============================================================================

# Finish positions: each driver independently samples from their distribution,
# conflicts resolved by ranking (random tie-break). ~10x faster than
# sequential Plackett-Luce for 22 drivers.
sim_finish_positions <- function(prob_mat, n) {
  raw <- integer(n)
  for (i in seq_len(n)) {
    raw[i] <- sample.int(22L, 1L, prob = prob_mat[i, ])
  }
  # rank() with tiny random jitter avoids ties without another loop
  as.integer(rank(raw + runif(n) * 0.001, ties.method = "first"))
}

# DNFs: drivers with ClassPct=0 always DNF; remainder filled by weighted sample
apply_dnfs_fast <- function(finish_pos, cls_pct, n_dnf, n) {
  cls <- rep(TRUE, n)
  
  # Hard DNFs: ClassPct = 0 means always unclassified regardless of n_dnf
  always_dnf <- which(cls_pct == 0 | is.na(cls_pct))
  cls[always_dnf] <- FALSE
  
  # How many additional random DNFs needed beyond the hard ones?
  n_remaining <- max(0L, n_dnf - length(always_dnf))
  
  if (n_remaining > 0) {
    eligible <- which(cls)
    if (length(eligible) > 0) {
      dnf_w  <- pmax(1 - cls_pct[eligible], 0)
      if (sum(dnf_w) == 0) dnf_w[] <- 1
      n_pick <- min(n_remaining, length(eligible))
      extra  <- eligible[sample.int(length(eligible), n_pick,
                                    prob = dnf_w / sum(dnf_w), replace = FALSE)]
      cls[extra] <- FALSE
    }
  }
  
  # Re-rank: classified (ordered by finish) then DNF (ordered by finish)
  cls_idx          <- which(cls)[order(finish_pos[cls])]
  dnf_idx          <- which(!cls)[order(finish_pos[!cls])]
  new_pos          <- integer(n)
  new_pos[cls_idx] <- seq_along(cls_idx)
  new_pos[dnf_idx] <- length(cls_idx) + seq_along(dnf_idx)
  list(pos = new_pos, classified = cls)
}

# Fastest lap: vectorized weight build, single sample
assign_fl_fast <- function(finish_pos, fl_pct, fl_pos_w, n) {
  pos_w    <- fl_pos_w[as.character(finish_pos)]
  combined <- (fl_pct + pos_w) / 2
  combined[fl_pct == 0 | is.na(combined)] <- 0
  if (sum(combined) == 0) combined <- as.numeric(finish_pos <= 10)
  fl <- rep(FALSE, n)
  fl[sample.int(n, 1L, prob = combined / sum(combined))] <- TRUE
  fl
}

# Laps led: vectorized distance per LL row, no inner R loops
assign_laps_led_fast <- function(finish_pos, grid_pos, ll_max, ll_by_season, n_seasons) {
  ll_out <- integer(length(finish_pos))
  if (n_seasons == 0L) return(ll_out)
  
  race   <- ll_by_season[[sample.int(n_seasons, 1L)]]
  n_race <- length(race$ll)
  if (n_race == 0L) return(ll_out)
  
  assigned <- rep(FALSE, length(finish_pos))
  for (r in seq_len(n_race)) {
    amt  <- race$ll[r]; if (amt <= 0L) next
    elig <- which(!assigned & ll_max >= amt)
    if (length(elig) == 0L) next
    # Vectorized distance across eligible drivers
    dist <- abs(grid_pos[elig] - race$grid[r]) + abs(finish_pos[elig] - race$finish[r])
    best <- elig[which.min(dist + runif(length(elig)) * 0.001)]
    ll_out[best]   <- min(ll_out[best] + amt, ll_max[best])
    assigned[best] <- TRUE
  }
  ll_out
}

# Teammate defeat: pre-computed index pairs, no split()
teammate_defeat_fast <- function(finish_pos, team_pairs) {
  beaten <- rep(FALSE, length(finish_pos))
  for (p in team_pairs) {
    beaten[p[which.min(finish_pos[p])]] <- TRUE
  }
  beaten
}

# Constructor score: vectorized
calc_constructor_score <- function(finish_pos, classified, ll, fl) {
  pos_pts <- sum(F1_POS_PTS[pmin(finish_pos, 22L)])
  bonus   <- as.integer(all(classified))        * 2L +
    as.integer(all(finish_pos <= 10L)) * 5L +
    as.integer(all(finish_pos <= 3L))  * 3L +
    as.integer(any(fl))                * 3L
  pos_pts + bonus + sum(ll) * 0.25
}


# ============================================================================
# CHUNK SIMULATOR
# Pre-allocates plain vectors, fills them per sim, builds one data.table at end.
# Avoids per-row set() overhead and repeated data.table allocation.
# ============================================================================

simulate_f1_chunk <- function(pc, drivers, constructors, chunk_sims, start_id) {
  n_drv   <- pc$n
  n_cnstr <- nrow(constructors)
  total_d <- chunk_sims * n_drv
  total_c <- chunk_sims * n_cnstr
  
  # Driver output vectors
  v_simid    <- integer(total_d);   v_player   <- character(total_d)
  v_dkscore  <- numeric(total_d);   v_cptscore <- numeric(total_d)
  v_fpts     <- numeric(total_d);   v_gpts     <- numeric(total_d)
  v_flpts    <- numeric(total_d);   v_llpts    <- numeric(total_d)
  v_btpts    <- numeric(total_d);   v_clspts   <- numeric(total_d)
  v_finish   <- integer(total_d);   v_grid     <- integer(total_d)
  v_ll       <- integer(total_d);   v_fl       <- logical(total_d)
  v_cls      <- logical(total_d);   v_beat     <- logical(total_d)
  v_team     <- character(total_d)
  
  # Constructor output vectors
  c_simid  <- integer(total_c)
  c_player <- character(total_c)
  c_score  <- numeric(total_c)
  
  # Constructor -> driver index lookup (computed once per chunk call)
  cnstr_idx <- lapply(constructors$Name, function(cn) which(drivers$Team == cn))
  
  grid_pos  <- pc$grid_pos
  drv_names <- drivers$Name
  drv_teams <- drivers$Team
  
  for (s in seq_len(chunk_sims)) {
    sim_id <- start_id + s - 1L
    ds     <- (s - 1L) * n_drv + 1L
    de     <- s * n_drv
    cs     <- (s - 1L) * n_cnstr + 1L
    
    # 1. Finish positions
    raw_pos <- sim_finish_positions(pc$prob_mat, n_drv)
    
    # 2. DNFs
    n_dnf   <- n_drv - sample(pc$cls_n, 1L, prob = pc$cls_prob)
    dnf_res <- apply_dnfs_fast(raw_pos, pc$cls_pct, n_dnf, n_drv)
    fin_pos <- dnf_res$pos
    is_cls  <- dnf_res$classified
    
    # 3. Bonus events
    ll_out  <- assign_laps_led_fast(fin_pos, grid_pos, pc$ll_max, pc$ll_by_season, pc$n_seasons)
    fl_out  <- assign_fl_fast(fin_pos, pc$fl_pct, pc$fl_pos_w, n_drv)
    beat_tm <- teammate_defeat_fast(fin_pos, pc$team_pairs)
    
    # 4. Scoring — fully vectorized
    finish_pts  <- F1_POS_PTS[pmin(fin_pos, 22L)]
    grid_pts    <- grid_pos - fin_pos
    fl_pts      <- as.numeric(fl_out) * 3
    ll_pts      <- ll_out * 0.25
    beat_tm_pts <- as.numeric(beat_tm) * 5
    cls_pts     <- as.numeric(is_cls)
    dk_score    <- finish_pts + grid_pts + fl_pts + ll_pts + beat_tm_pts + cls_pts
    
    # 5. Fill driver vectors (direct vector slice assignment)
    v_simid[ds:de]    <- sim_id
    v_player[ds:de]   <- drv_names
    v_dkscore[ds:de]  <- dk_score
    v_cptscore[ds:de] <- dk_score * 1.5
    v_fpts[ds:de]     <- finish_pts
    v_gpts[ds:de]     <- grid_pts
    v_flpts[ds:de]    <- fl_pts
    v_llpts[ds:de]    <- ll_pts
    v_btpts[ds:de]    <- beat_tm_pts
    v_clspts[ds:de]   <- cls_pts
    v_finish[ds:de]   <- fin_pos
    v_grid[ds:de]     <- grid_pos
    v_ll[ds:de]       <- ll_out
    v_fl[ds:de]       <- fl_out
    v_cls[ds:de]      <- is_cls
    v_beat[ds:de]     <- beat_tm
    v_team[ds:de]     <- drv_teams
    
    # 6. Constructor scores
    for (ci in seq_len(n_cnstr)) {
      di <- cnstr_idx[[ci]]
      c_simid[cs + ci - 1L]  <- sim_id
      c_player[cs + ci - 1L] <- constructors$Name[ci]
      c_score[cs + ci - 1L]  <- if (length(di) > 0L)
        calc_constructor_score(fin_pos[di], is_cls[di], ll_out[di], fl_out[di]) else 0
    }
  }
  
  # Build data.tables in one shot — no per-row allocation overhead
  drv_dt <- data.table(
    SimID = v_simid, Player = v_player, PlayerType = "Driver",
    DKScore = v_dkscore, CptScore = v_cptscore,
    FinishPts = v_fpts, GridDiffPts = v_gpts,
    FL_Pts = v_flpts, LL_Pts = v_llpts,
    BeatTM_Pts = v_btpts, Classified_Pts = v_clspts,
    Finish = v_finish, Grid = v_grid, LapsLed = v_ll,
    FastestLap = v_fl, Classified = v_cls, BeatTeammate = v_beat,
    Team = v_team
  )
  
  cnstr_dt <- data.table(
    SimID = c_simid, Player = c_player, PlayerType = "Constructor",
    DKScore = c_score, CptScore = NA_real_,
    FinishPts = NA_real_, GridDiffPts = NA_real_,
    FL_Pts = NA_real_, LL_Pts = NA_real_,
    BeatTM_Pts = NA_real_, Classified_Pts = NA_real_,
    Finish = NA_integer_, Grid = NA_integer_, LapsLed = NA_integer_,
    FastestLap = NA, Classified = NA, BeatTeammate = NA,
    Team = c_player
  )
  
  list(drivers = drv_dt, constructors = cnstr_dt)
}


# ============================================================================
# MAIN ENTRY POINT
# ============================================================================

run_f1_simulation <- function(input_data, n_sims, config,
                              progress_callback = NULL) {
  pb <- function(v, m) if (!is.null(progress_callback)) progress_callback(m, v)
  
  pb(0.02, "Loading F1 input data...")
  
  drivers        <- as.data.table(input_data$Drivers)
  ll_data        <- as.data.table(input_data$LL)
  fl_probs       <- as.data.table(input_data$FL)
  classification <- as.data.table(input_data$Classification)
  constructors   <- as.data.table(input_data$Constructors)
  
  cat(sprintf("\n[F1 SIMULATION]\nDrivers: %d | Constructors: %d | Sims: %s\n",
              nrow(drivers), nrow(constructors), format(n_sims, big.mark = ",")))
  
  pb(0.04, "Pre-computing simulation data...")
  pc <- precompute_f1_data(drivers, ll_data, fl_probs, classification)
  
  # Validate constructor -> driver mapping and warn on mismatches
  cat("Constructor -> Driver mapping:\n")
  for (cn in constructors$Name) {
    matched <- drivers$Name[drivers$Team == cn]
    if (length(matched) == 0) {
      cat(sprintf("  WARNING: '%s' matched NO drivers (check Team column)\n", cn))
      cat(sprintf("    Team values in Drivers: %s\n",
                  paste(sort(unique(drivers$Team)), collapse = ", ")))
    } else {
      cat(sprintf("  '%s' -> %s\n", cn, paste(matched, collapse = ", ")))
    }
  }
  
  chunk_size <- min(1000L, max(250L, ceiling(20000L / pc$n)))
  n_chunks   <- ceiling(n_sims / chunk_size)
  cat(sprintf("Chunks: %d x %d\n\n", n_chunks, chunk_size))
  
  drv_chunks   <- vector("list", n_chunks)
  cnstr_chunks <- vector("list", n_chunks)
  
  t0 <- Sys.time()
  for (ch in seq_len(n_chunks)) {
    s0    <- (ch - 1L) * chunk_size + 1L
    s1    <- min(ch * chunk_size, n_sims)
    c_sim <- s1 - s0 + 1L
    
    pb(0.06 + 0.78 * (ch / n_chunks),
       sprintf("Simulating... %d%%", round(100 * ch / n_chunks)))
    
    res <- simulate_f1_chunk(pc, drivers, constructors, c_sim, s0)
    drv_chunks[[ch]]   <- res$drivers
    cnstr_chunks[[ch]] <- res$constructors
    
    if (ch %% 10 == 0) gc(verbose = FALSE)
  }
  
  pb(0.85, "Combining results...")
  drv_results   <- rbindlist(drv_chunks)
  cnstr_results <- rbindlist(cnstr_chunks)
  rm(drv_chunks, cnstr_chunks, pc); gc(verbose = FALSE)
  
  cat(sprintf("[SIMULATION COMPLETE] %.1fs | %s driver-rows | %s constructor-rows\n\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs")),
              format(nrow(drv_results),   big.mark = ","),
              format(nrow(cnstr_results), big.mark = ",")))
  
  pb(0.88, "Building output tables...")
  
  sim_results <- rbindlist(list(drv_results, cnstr_results), use.names = TRUE)
  
  drv_meta <- unique(drivers[, .(
    Player     = Name,
    PlayerType = "Driver",
    DKSalary   = Salary_Driver,
    CptSalary  = Salary_Captain,
    DKID       = DKID_Driver,
    CptDFSID   = DKID_Captain,
    Team,
    Starting   = Grid
  )])
  
  cnstr_meta <- unique(constructors[, .(
    Player     = Name,
    PlayerType = "Constructor",
    DKSalary   = Salary,
    CptSalary  = NA_real_,
    DKID,
    CptDFSID   = NA_character_,
    Team       = Name,
    Starting   = NA_integer_
  )])
  
  metadata <- rbindlist(list(drv_meta, cnstr_meta), fill = TRUE)
  metadata[, DKOwn := 0]
  
  pb(0.93, "Preparing visualizations...")
  
  drv_analysis   <- f1_driver_analysis(drv_results)
  cnstr_analysis <- f1_constructor_analysis(cnstr_results, constructors)
  
  sport_visuals <- list(
    driver_results       = drv_results,
    constructor_results  = cnstr_results,
    driver_analysis      = drv_analysis,
    constructor_analysis = cnstr_analysis,
    driver_meta          = drv_meta,
    constructor_meta     = cnstr_meta
  )
  
  pb(0.99, "Done!")
  
  list(
    sim_results   = sim_results,
    metadata      = metadata,
    has_fd        = FALSE,
    sport_visuals = sport_visuals
  )
}


# ============================================================================
# ANALYSIS
# ============================================================================

f1_driver_analysis <- function(drv_results) {
  drv_results[, .(
    Grid            = first(Grid),
    Team            = first(Team),
    Median_Finish   = round(median(Finish),         1),
    Win_Rate        = round(mean(Finish == 1) * 100, 1),
    Podium_Rate     = round(mean(Finish <= 3) * 100, 1),
    Points_Rate     = round(mean(Finish <= 10)* 100, 1),
    Classified_Rate = round(mean(Classified)  * 100, 1),
    Beat_TM_Rate    = round(mean(BeatTeammate)* 100, 1),
    FL_Rate         = round(mean(FastestLap)  * 100, 1),
    Avg_LL          = round(mean(LapsLed),          1),
    Avg_FinishPts   = round(mean(FinishPts),        1),
    Avg_GridPts     = round(mean(GridDiffPts),      1),
    Avg_FL_Pts      = round(mean(FL_Pts),           2),
    Avg_LL_Pts      = round(mean(LL_Pts),           2),
    Avg_BeatTM_Pts  = round(mean(BeatTM_Pts),       2),
    Avg_Cls_Pts     = round(mean(Classified_Pts),   2),
    Avg_DKScore     = round(mean(DKScore),          1),
    Median_DKScore  = round(median(DKScore),        1),
    Avg_CptScore    = round(mean(CptScore),         1),
    Median_CptScore = round(median(CptScore),       1)
  ), by = Player]
}

f1_constructor_analysis <- function(cnstr_results, constructors) {
  a <- cnstr_results[, .(
    Avg_Score    = round(mean(DKScore),           1),
    Median_Score = round(median(DKScore),         1),
    P75_Score    = round(quantile(DKScore, 0.75), 1),
    P90_Score    = round(quantile(DKScore, 0.90), 1)
  ), by = Player]
  merge(a, constructors[, .(Player = Name, DKSalary = Salary, DKID)],
        by = "Player", all.x = TRUE)
}


# ============================================================================
# PHASE 1: F1 OPTIMIZER
#
# Constraints:
#   - 1 Captain (driver only, CptSalary counts toward cap, score × 1.5)
#   - 4 Flex drivers (DKSalary)
#   - 1 Constructor (DKSalary) — stored as Util5
#   - Total salary <= $50,000
#   - A driver cannot appear as both Captain and a Util
#   - Max 2 of 3 from any team: cannot have Constructor + both its drivers
#     (in any role — captain counts as one of the two drivers)
#
# Output unique_lineups columns: Captain | Util1 | Util2 | Util3 | Util4 | Util5
#   Util5 is always the constructor. This matches the ^Util pattern so
#   score_all_lineups picks it up with multiplier = 1.
#
# Pass to score_all_lineups with cpt_multiplier = 1.5 and platform_col = "DKScore".
# The Captain's DKScore (flex score) gets multiplied by 1.5 there automatically.
# ============================================================================

find_optimal_f1_lineups <- function(sim_results, metadata, config, verbose = TRUE) {
  # Per-sim greedy optimal — matches generic combinatorial_captain pattern exactly.
  # Extra F1 constraints vs generic:
  #   1. Constructor slot (Util5) separate from 4 flex drivers
  #   2. Captain uses CptSalary not 1.5x flex salary
  #   3. Stacking: max 1 driver from constructor's team across captain+flex
  #      (0 if captain is from that team, 1 otherwise)
  # Speed: pts/$ pre-filter per sim drops bottom N% of drivers before loops.
  
  if (verbose) cat("\nPhase 1: Finding optimal lineup per sim (greedy F1 captain)...\n")
  setDT(sim_results); setDT(metadata)
  
  salary_cap    <- config$salary_cap  %||% 50000
  max_lineups   <- config$max_lineups %||% 5000L
  ppd_keep_pct  <- config$ppd_keep_pct %||% 0.75  # keep top 75% by pts/$ per sim
  start_time    <- Sys.time()
  
  # Static salary/team lookups — attached once, reused every sim
  drv_meta   <- metadata[PlayerType == "Driver"      & !is.na(DKSalary) & DKSalary > 0]
  cnstr_meta <- metadata[PlayerType == "Constructor" & !is.na(DKSalary) & DKSalary > 0]
  
  cpt_sal_lkp  <- setNames(drv_meta$CptSalary, drv_meta$Player)
  flex_sal_lkp <- setNames(drv_meta$DKSalary,  drv_meta$Player)
  con_sal_lkp  <- setNames(cnstr_meta$DKSalary, cnstr_meta$Player)
  
  # Constructor -> its two driver names (for stacking check)
  cnstr_team <- lapply(cnstr_meta$Player, function(cn)
    drv_meta$Player[drv_meta$Team == cn])
  names(cnstr_team) <- cnstr_meta$Player
  
  # Attach static salaries to sim_results once so per-sim subset already has them
  drv_sims <- merge(
    sim_results[PlayerType == "Driver" & !is.na(DKScore), .(SimID, Player, DKScore)],
    drv_meta[, .(Player, FlexSal = DKSalary, CptSal = CptSalary, Team)],
    by = "Player", all.x = FALSE
  )
  cnstr_sims <- merge(
    sim_results[PlayerType == "Constructor" & !is.na(DKScore), .(SimID, Player, DKScore)],
    cnstr_meta[, .(Player, ConSal = DKSalary)],
    by = "Player", all.x = FALSE
  )
  setkey(drv_sims,   SimID)
  setkey(cnstr_sims, SimID)
  
  sim_ids <- unique(drv_sims$SimID)
  n_sims  <- length(sim_ids)
  prog_freq <- max(1L, n_sims %/% 20L)
  
  if (verbose) cat(sprintf("  %d drivers | %d constructors | %s sims | $%s cap | 1.5x captain\n",
                           nrow(drv_meta), nrow(cnstr_meta),
                           format(n_sims, big.mark = ","),
                           format(salary_cap, big.mark = ",")))
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid <- sim_ids[i]
    
    d  <- drv_sims[.(sid)]
    cn <- cnstr_sims[.(sid)]
    if (nrow(d) < 5 || nrow(cn) < 1) next
    
    # pts/$ pre-filter: drop bottom (1 - ppd_keep_pct) of drivers by pts per dollar
    # Keeps pool small without ever discarding the best scorers
    d[, PPD := DKScore / FlexSal]
    ppd_thresh <- quantile(d$PPD, probs = 1 - ppd_keep_pct, na.rm = TRUE)
    d <- d[PPD >= ppd_thresh]
    if (nrow(d) < 5) next
    
    setorder(d,  -DKScore)
    setorder(cn, -DKScore)
    
    # Plain vectors for inner loop speed
    d_name   <- d$Player;  d_score  <- d$DKScore
    d_fsal   <- d$FlexSal; d_csal   <- d$CptSal; d_team <- d$Team
    cn_name  <- cn$Player; cn_score <- cn$DKScore; cn_sal <- cn$ConSal
    nd <- nrow(d); nc <- nrow(cn)
    
    best_score  <- -Inf
    best_lineup <- NULL
    
    for (ci in seq_len(nd)) {
      cs <- d_csal[ci]
      if (is.na(cs) || cs > salary_cap) next
      cpt_score <- d_score[ci] * 1.5
      cpt       <- d_name[ci]
      rem1      <- salary_cap - cs
      
      for (ki in seq_len(nc)) {
        ks <- cn_sal[ki]
        if (is.na(ks) || ks > rem1) next
        rem2 <- rem1 - ks
        con  <- cn_name[ki]
        
        # Stacking: 0 flex from con's team if captain is on it, else 1
        team_drvs <- cnstr_team[[con]]
        flex_limit <- if (cpt %in% team_drvs) 0L else 1L
        
        picked     <- character(4L)
        n_picked   <- 0L
        sal_used   <- 0
        flex_score <- 0
        team_cnt   <- 0L
        
        for (fi in seq_len(nd)) {
          if (n_picked == 4L) break
          if (fi == ci) next
          ps <- d_fsal[fi]
          if (sal_used + ps > rem2) next
          if (d_name[fi] %in% team_drvs) {
            if (team_cnt >= flex_limit) next
            team_cnt <- team_cnt + 1L
          }
          n_picked         <- n_picked + 1L
          picked[n_picked] <- d_name[fi]
          sal_used         <- sal_used + ps
          flex_score       <- flex_score + d_score[fi]
        }
        
        if (n_picked < 4L) next
        
        total <- cpt_score + cn_score[ki] + flex_score
        if (total > best_score) {
          best_score  <- total
          best_lineup <- list(
            Captain     = cpt,
            Flex        = sort(picked[1:4]),
            Constructor = con,
            TotalSalary = cs + ks + sal_used,
            TotalScore  = total
          )
        }
      }
    }
    
    if (!is.null(best_lineup)) {
      lineup_list[[i]] <- data.table(
        Lineup      = paste(c(best_lineup$Captain, best_lineup$Flex,
                              best_lineup$Constructor), collapse = "|"),
        TotalSalary = best_lineup$TotalSalary,
        TotalScore  = best_lineup$TotalScore,
        Captain     = best_lineup$Captain,
        Util1       = best_lineup$Flex[1],
        Util2       = best_lineup$Flex[2],
        Util3       = best_lineup$Flex[3],
        Util4       = best_lineup$Flex[4],
        Util5       = best_lineup$Constructor
      )
    }
    
    if (verbose && i %% prog_freq == 0L) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      cat(sprintf("\r  Phase 1: %d%% | %.1fs", round(i / n_sims * 100), elapsed))
      flush.console()
    }
  }
  
  if (verbose) cat(sprintf("\r  Phase 1: 100%% | %.1fs\n",
                           as.numeric(difftime(Sys.time(), start_time, units = "secs"))))
  
  all_lineups <- rbindlist(lineup_list[!sapply(lineup_list, is.null)])
  if (nrow(all_lineups) == 0) stop("No valid F1 lineups found.")
  
  counts <- all_lineups[, .(Top1Count   = .N,
                            TotalSalary = TotalSalary[1],
                            AvgScore    = mean(TotalScore)),
                        by = Lineup]
  setorder(counts, -Top1Count)
  if (nrow(counts) > max_lineups) counts <- counts[1:max_lineups]
  
  parts <- strsplit(counts$Lineup, "\\|")
  unique_lineups <- data.table(
    Captain     = sapply(parts, `[`, 1),
    Util1       = sapply(parts, `[`, 2),
    Util2       = sapply(parts, `[`, 3),
    Util3       = sapply(parts, `[`, 4),
    Util4       = sapply(parts, `[`, 5),
    Util5       = sapply(parts, `[`, 6),
    TotalSalary = counts$TotalSalary,
    Top1Count   = counts$Top1Count,
    AvgScore    = counts$AvgScore
  )
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (verbose) cat(sprintf("  ✓ %s unique lineups from %s sims | %.1fs\n",
                           format(nrow(unique_lineups), big.mark = ","),
                           format(n_sims, big.mark = ","), elapsed))
  
  list(unique_lineups = unique_lineups, n_sims = n_sims,
       config = config, mode = "f1_captain", platform_col = "DKScore")
}



# ============================================================================
# LINEUP METRICS HOOK
# ============================================================================

calculate_f1_lineup_metrics <- function(scored_lineups, sim_results, metadata) {
  scored_lineups
}



f1_plot_finish_dist <- function(drv_results, drv_meta) {
  setDT(drv_results); setDT(drv_meta)
  grid_order <- drv_meta[order(Starting), Player]
  pd         <- copy(drv_results)
  pd[, PlayerF := factor(Player, levels = rev(grid_order))]
  team_map   <- unique(pd[, .(Player, Team)])
  clr_vec    <- setNames(sapply(team_map$Team, get_f1_color), team_map$Player)
  
  p <- ggplot(as.data.frame(pd), aes(x = Finish, y = PlayerF, fill = Player)) +
    geom_violin(alpha = 0.75, trim = TRUE, width = 0.85, scale = "width") +
    geom_boxplot(width = 0.12, alpha = 0.6, outlier.shape = NA, color = "white") +
    scale_fill_manual(values = clr_vec) +
    scale_x_reverse(breaks = c(1, 5, 10, 15, 20, 22)) +
    labs(x = "Finish Position", y = NULL,
         title = "Finish Position Distribution (Grid Order)") +
    .f1_theme()
  ggplotly(p) %>% layout(paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e")
}

f1_plot_fp_dist <- function(drv_results, drv_meta) {
  setDT(drv_results); setDT(drv_meta)
  sal_order <- drv_meta[order(-DKSalary), Player]
  pd        <- copy(drv_results)
  pd[, PlayerF := factor(Player, levels = rev(sal_order))]
  team_map  <- unique(pd[, .(Player, Team)])
  clr_vec   <- setNames(sapply(team_map$Team, get_f1_color), team_map$Player)
  
  p <- ggplot(as.data.frame(pd), aes(x = DKScore, y = PlayerF, fill = Player)) +
    geom_violin(alpha = 0.75, trim = TRUE, width = 0.85, scale = "width") +
    geom_boxplot(width = 0.12, alpha = 0.6, outlier.shape = NA, color = "white") +
    scale_fill_manual(values = clr_vec) +
    labs(x = "DK Fantasy Points (Flex Score)", y = NULL,
         title = "Fantasy Points Distribution (Salary Order)") +
    .f1_theme()
  ggplotly(p) %>% layout(paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e")
}

f1_plot_dominators <- function(drv_results) {
  setDT(drv_results)
  ll_sum <- drv_results[, .(Avg_LL = mean(LapsLed), Team = first(Team)), by = Player]
  ll_sum <- ll_sum[Avg_LL > 0.01]
  
  if (nrow(ll_sum) == 0)
    return(plotly_empty() %>%
             layout(title = list(text = "No laps led data", font = list(color = "#FFE500")),
                    paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e"))
  
  setorder(ll_sum, -Avg_LL)
  ll_sum[, PlayerF := factor(Player, levels = Player)]
  clr_vec <- setNames(sapply(ll_sum$Team, get_f1_color), ll_sum$Player)
  
  p <- ggplot(as.data.frame(ll_sum),
              aes(x = PlayerF, y = Avg_LL, fill = Player,
                  text = paste0(Player, ": ", round(Avg_LL, 1), " avg laps led"))) +
    geom_col(alpha = 0.85) +
    scale_fill_manual(values = clr_vec) +
    labs(x = NULL, y = "Avg Laps Led", title = "Dominator - Average Laps Led") +
    .f1_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotly(p, tooltip = "text") %>%
    layout(paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e")
}

f1_plot_constructor_dist <- function(cnstr_results) {
  setDT(cnstr_results)
  med_order <- cnstr_results[, .(med = median(DKScore)), by = Player][order(-med), Player]
  pd        <- copy(cnstr_results)
  pd[, PlayerF := factor(Player, levels = rev(med_order))]
  clr_vec   <- setNames(sapply(med_order, get_f1_color), med_order)
  
  p <- ggplot(as.data.frame(pd), aes(x = DKScore, y = PlayerF, fill = Player)) +
    geom_violin(alpha = 0.75, trim = TRUE, width = 0.85, scale = "width") +
    geom_boxplot(width = 0.2, alpha = 0.6, outlier.shape = NA, color = "white") +
    scale_fill_manual(values = clr_vec) +
    labs(x = "DK Fantasy Points", y = NULL,
         title = "Constructor Points Distribution") +
    .f1_theme()
  ggplotly(p) %>% layout(paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e")
}

.f1_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      axis.text        = element_text(color = "#FFE500"),
      axis.title       = element_text(color = "#FFE500"),
      plot.title       = element_text(color = "#FFE500", face = "bold"),
      plot.background  = element_rect(fill = "#1e1e1e", color = NA),
      panel.background = element_rect(fill = "#1e1e1e", color = NA)
    )
}