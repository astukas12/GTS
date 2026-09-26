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
# metadata — one row per entity:
#   Drivers:     Player | PlayerType | DKSalary | CptSalary | DKID | CptDFSID | Team | Grid |
#                DKOwn (flex Own %) | CaptainOwn (CptOwn %)
#   Constructors: Player | PlayerType | DKSalary | DKID | DKOwn
#
# Ownership: optional columns Own / CptOwn in Drivers sheet; DKOwn in Constructors sheet.
#            Defaults to 0 if absent. DKOwn in metadata = flex Own for drivers.
#
# DK Roster: 1 Captain (driver, 1.5x) + 4 Flex Drivers + 1 Constructor | $50k cap
# ============================================================================

library(data.table)
library(readxl)

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
  
  # Ownership columns — optional; default to 0 if not present
  # Ownership columns — optional; default to 0 if not present
  # Drivers sheet:      Own (flex ownership), CptOwn (captain ownership)
  # Constructors sheet: DKOwn (constructor ownership)
  if (!"Own"    %in% names(drivers))      drivers[, Own    := 0]
  if (!"CptOwn" %in% names(drivers))      drivers[, CptOwn := 0]
  if (!"DKOwn"  %in% names(constructors)) constructors[, DKOwn := 0]
  drivers[is.na(Own),    Own    := 0]
  drivers[is.na(CptOwn), CptOwn := 0]
  constructors[is.na(DKOwn), DKOwn := 0]
  
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

precompute_f1_data <- function(drivers, ll_data, fl_probs, classification,
                               constructors = NULL) {
  pos_cols <- as.character(1:22)
  
  # Finish probability matrix: n_drivers x 22
  prob_mat <- as.matrix(drivers[, pos_cols, with = FALSE])
  prob_mat[is.na(prob_mat)] <- 0
  # Cumulative rows for the copula draw; renormalised because the sheet rounds.
  cdf <- t(apply(prob_mat, 1, function(v) { s <- sum(v); if (s == 0) v else cumsum(v) / s }))
  cdf[, 22] <- 1

  # TeamCorr: the sheet's own teammate correlation. Absent on older sheets, in
  # which case team_corr stays 0 and the sampler behaves exactly as before.
  team_corr <- 0
  if (!is.null(constructors) && "TeamCorr" %in% names(constructors)) {
    tc <- suppressWarnings(as.numeric(constructors$TeamCorr[1]))
    if (length(tc) == 1L && is.finite(tc)) team_corr <- max(0, min(0.99, tc))
  }
  
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
    cdf          = cdf,
    team_corr    = team_corr,
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
sim_finish_positions <- function(prob_mat, n, cdf = NULL, team_pairs = NULL,
                                 team_corr = 0) {
  if (is.null(cdf) || team_corr <= 0 || length(team_pairs) == 0L) {
    raw <- integer(n)
    for (i in seq_len(n)) raw[i] <- sample.int(22L, 1L, prob = prob_mat[i, ])
    # rank() with tiny random jitter avoids ties without another loop
    return(as.integer(rank(raw + runif(n) * 0.001, ties.method = "first")))
  }

  # Correlated teammates, via a Gaussian copula.
  #
  # The sheet is a MARGINAL: sampling each row on its own throws away
  # everything joint the sheet's own simulation had, above all the shared car.
  # The two cars of one constructor move together -- across 2014-2026 the
  # correlation of their finishes around each car's own expected result is
  # about 0.41, and they retire together 3.3x more often than two independent
  # draws would -- and a constructor scores off both of them, so drawing them
  # independently prices its upside as two coin flips.
  #
  # One standard normal per driver with a shared per-car component, mapped to a
  # uniform, and the position read off that driver's own cumulative row. The
  # marginals survive exactly, by construction of the inverse CDF; only the
  # dependence between them changes, so not a single cell of the sheet moves.
  # TeamCorr is the correlation on the latent normal, which is not the rank
  # correlation that comes out the far side -- the sheet solves it against that.
  #
  # The uniform doubles as the tie-break, so the shared shock survives the
  # collision ranking instead of being half undone by fresh noise.
  # The inverse is taken CONTINUOUSLY -- interpolating inside the position the
  # uniform lands in, rather than rounding to it. Rounding first and ranking
  # after throws away where in the bin the draw fell, and that costs real
  # accuracy: measured on this sheet, mean total-variation distance from the
  # rows is 0.063 rounded against 0.034 interpolated, with the teammate
  # correlation identical either way. The interpolated form also comes out a
  # strict permutation with no ties to break.
  z  <- rnorm(n) * sqrt(1 - team_corr)
  sc <- sqrt(team_corr)
  for (p in team_pairs) z[p] <- z[p] + rnorm(1L) * sc
  u <- pnorm(z)
  q <- numeric(n)
  for (i in seq_len(n)) {
    j     <- min(findInterval(u[i], cdf[i, ]) + 1L, 22L)
    lo    <- if (j > 1L) cdf[i, j - 1L] else 0
    q[i]  <- (j - 1L) + (u[i] - lo) / max(prob_mat[i, j], 1e-12)
  }
  as.integer(rank(q, ties.method = "first"))
}

# DNFs: the sheet's matrix is the whole result, so the BOTTOM n_dnf of the
# sampled order are the retirements.
#
# This used to draw the DNFs separately, weighted by 1 - ClassPct and
# independent of where the car had been sampled to finish, then re-rank. Against
# any sheet whose rows already carry attrition in their tail that counts it
# twice: a front-runner sampled to P20 by his own DNF mass could come back
# classified, while a car sampled to P3 was marked retired and dropped to the
# back. Finish points and grid-differential points were both scrambled, and
# exactly for the drivers the sheet had said the most about.
#
# ClassPct is now a QA output of the sheet rather than an input to the draw. The
# one thing it still does is the hard case: ClassPct = 0 means the car does not
# see the flag whatever the field does, so those are pushed to the back first.
apply_dnfs_fast <- function(finish_pos, cls_pct, n_dnf, n) {
  hard <- which(cls_pct == 0 | is.na(cls_pct))
  key  <- finish_pos
  if (length(hard)) key[hard] <- key[hard] + n      # behind every running car
  pos  <- as.integer(rank(key, ties.method = "first"))

  n_dnf <- min(max(as.integer(n_dnf), length(hard)), n)
  list(pos = pos, classified = pos <= (n - n_dnf))
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
    # Who led a race is mostly about where they FINISHED, and only then about
    # where they started, so the match is lexicographic: finishing position
    # first, grid as the tie-break. Weighting the two equally let a profile's
    # small mid-race stint be handed to the winner before the winner's own row
    # was reached, and the winner then led zero laps -- on a third of sims.
    dist <- 100 * abs(finish_pos[elig] - race$finish[r]) +
                  abs(grid_pos[elig]   - race$grid[r])
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
    raw_pos <- sim_finish_positions(pc$prob_mat, n_drv, pc$cdf,
                                    pc$team_pairs, pc$team_corr)
    
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
  pc <- precompute_f1_data(drivers, ll_data, fl_probs, classification, constructors)
  
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
    Starting   = Grid,
    DKOwn      = Own,
    CaptainOwn = CptOwn
  )])
  
  cnstr_meta <- unique(constructors[, .(
    Player     = Name,
    PlayerType = "Constructor",
    DKSalary   = Salary,
    CptSalary  = NA_real_,
    DKID,
    CptDFSID   = NA_character_,
    Team       = Name,
    Starting   = NA_integer_,
    DKOwn      = DKOwn,
    CaptainOwn = NA_real_
  )])
  
  metadata <- rbindlist(list(drv_meta, cnstr_meta), fill = TRUE)
  
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

# Flex-combination tables, built once when the engine is sourced rather than per
# sim: the INDICES are the same every sim (only the scores behind them change),
# so there is no reason to pay combn() fifty thousand times.
#   F1_FLEX_COMB[[n]]  4 x C(n,4) matrix of indices into the score-ordered pool
#   F1_FLEX_HAS[[n]]   n x C(n,4) logical, TRUE where that driver is in that combo
FLEX_POOL <- 16L
F1_FLEX_COMB <- vector("list", FLEX_POOL)
F1_FLEX_HAS  <- vector("list", FLEX_POOL)
for (n in 4L:FLEX_POOL) {
  cb <- utils::combn(n, 4L)
  F1_FLEX_COMB[[n]] <- cb
  h <- matrix(FALSE, n, ncol(cb))
  h[cbind(as.vector(cb), rep(seq_len(ncol(cb)), each = 4L))] <- TRUE
  F1_FLEX_HAS[[n]] <- h
}
rm(n, cb, h)

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

    # ---- the four flex slots are chosen EXACTLY, not greedily ---------------
    # This was a single pass down the score order taking whatever fitted, with no
    # backtracking, so it would lock in an expensive scorer and then have to fill
    # the rest with cheap filler. Measured on 60 sims of the Baku 50k run against
    # a full exact solve over every C(22,5) block: the greedy was short in 72% of
    # sims, by 11.1 points on average and 49 at worst -- 3.5% of the optimum,
    # given away in the pool the customer actually gets.
    #
    # The fix is to enumerate all four-driver combinations from the FLEX_POOL
    # best scorers in this sim and take the best legal one. At 16 that is 1,820
    # combinations, masked and maximised as vectors rather than looped. An exact
    # fill from the top 14 already matched the full optimum in 60 of 60 sims, so
    # 16 is the same answer with margin.
    #
    # (The 75% pts/$ pre-filter above is NOT what was wrong -- it costs under
    # 0.2 points per sim, because it is applied to each sim's REALISED scores and
    # a driver who scores well in a sim has good points per dollar in that sim.
    # It is left alone, and the score-ranked pool below is a second safety net.)
    np <- min(FLEX_POOL, nd)
    cmb <- F1_FLEX_COMB[[np]]                  # 4 x C(np,4), built once at load
    p_sal <- matrix(d_fsal[cmb], nrow = 4L)
    cmb_sal <- colSums(p_sal)
    cmb_sc  <- colSums(matrix(d_score[cmb], nrow = 4L))

    for (ki in seq_len(nc)) {
      ks <- cn_sal[ki]
      if (is.na(ks) || ks > salary_cap) next
      con <- cn_name[ki]
      team_drvs <- cnstr_team[[con]]
      # how many of this constructor's drivers each combo uses
      cmb_tm <- colSums(matrix(d_name[cmb] %in% team_drvs, nrow = 4L))

      for (ci in seq_len(nd)) {
        cs <- d_csal[ci]
        if (is.na(cs) || cs + ks > salary_cap) next
        rem2 <- salary_cap - cs - ks
        flex_limit <- if (d_name[ci] %in% team_drvs) 0L else 1L
        ok <- cmb_sal <= rem2 & cmb_tm <= flex_limit
        if (ci <= np) ok <- ok & !F1_FLEX_HAS[[np]][ci, ]   # captain cannot be a flex
        if (!any(ok)) next
        j <- which(ok)[which.max(cmb_sc[ok])]
        total <- d_score[ci] * 1.5 + cn_score[ki] + cmb_sc[j]
        if (total > best_score) {
          best_score  <- total
          best_lineup <- list(
            Captain     = d_name[ci],
            Flex        = sort(d_name[cmb[, j]]),
            Constructor = con,
            TotalSalary = cs + ks + cmb_sal[j],
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
  # Recompute AvgOwn correctly for F1's mixed captain/flex/constructor lineup:
  #   Captain slot  -> CaptainOwn (CptOwn from input)
  #   Util1-4 slots -> DKOwn      (flex Own from input)
  #   Util5 slot    -> DKOwn      (constructor DKOwn from input)
  # calculate_distribution_metrics set AvgOwn using DKOwn for ALL slots (including
  # the captain), so we override it here with the proper weighted average.
  
  if (!("CaptainOwn" %in% names(metadata)) || !("Captain" %in% names(scored_lineups)))
    return(scored_lineups)
  
  setDT(scored_lineups)
  setDT(metadata)
  
  # Build fast lookup vectors
  cpt_own_lkp  <- setNames(metadata$CaptainOwn, metadata$Player)
  flex_own_lkp <- setNames(metadata$DKOwn,      metadata$Player)
  
  # Identify columns: captain + 4 flex drivers (Util1-4) + constructor (Util5)
  flex_cols <- paste0("Util", 1:4)   # drivers in flex slots
  con_col   <- "Util5"               # constructor
  
  have_cpt  <- "Captain" %in% names(scored_lineups)
  have_flex <- all(flex_cols %in% names(scored_lineups))
  have_con  <- con_col %in% names(scored_lineups)
  
  if (!have_cpt || !have_flex || !have_con) return(scored_lineups)
  
  scored_lineups[, AvgOwn := {
    cpt_o  <- cpt_own_lkp[Captain]
    flex_o <- rowMeans(cbind(
      flex_own_lkp[Util1], flex_own_lkp[Util2],
      flex_own_lkp[Util3], flex_own_lkp[Util4]
    ), na.rm = TRUE)
    con_o  <- flex_own_lkp[Util5]
    # 6 slots total: 1 cpt + 4 flex + 1 constructor
    round((cpt_o + flex_o * 4 + con_o) / 6, 4)
  }]
  
  scored_lineups
}



# (Plot functions removed — all F1 visuals are rendered natively in app.R via plot_ly)