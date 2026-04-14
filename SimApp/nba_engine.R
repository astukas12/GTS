# ============================================================================
# NBA SIMULATION ENGINE
# Golden Ticket Sims — NBA DFS
# ============================================================================
# Platforms:  DraftKings Classic | FanDuel Classic | DK Showdown
#
# DK scoring:  PTS(1) + 3PM(0.5) + REB(1.25) + AST(1.5) + STL(2) + BLK(2)
#              - TO(0.5) + DD bonus(1.5) + TD bonus(3, replaces DD)
# FD scoring:  PTS(1) + 3PM(0.5) + REB(1.20) + AST(1.5) + STL(2) + BLK(2)
#              - TO(1.0)  [no bonus]
# SD scoring:  same as DK (DKScore × 1.5 for CPT slot)
#
# Input file sheet structure:
#   IDs tab     -> DKID, FDID, DKSalary, FDSalary, DKPos, FDPos
#   Games tab   -> GameKey, HomeTeam, AwayTeam, GameTime, GameRank,
#                  OverUnder, HomeSpread, ShowdownFile
#   Team tabs   -> one per team: Name, DKOwn, FDOwn, RGProj, RGFDProj,
#                  Mins, fg3_rate, pot_ast_share, ast_conv,
#                  fgm_p10..p90, ftm_p10..p90, reb_p10..p90,
#                  ast_p10..p90, stl_p10..p90, blk_p10..p90, to_p10..p90
#   Sim_ sheets -> similarity, {HOME}_fgm, {HOME}_tpm, {HOME}_ftm,
#                  {HOME}_reb, {HOME}_ast, {HOME}_stl, {HOME}_blk, {HOME}_to,
#                  {AWAY}_* same
#   SD#_IDs     -> showdown CPT/FLEX IDs and salaries
#
# Stat allocation order and logic:
#   1. to    — consumes possessions; allocated via percentile shares
#   2. fgm   — root offensive stat; percentile shares
#   3. tpm   — DERIVED: fgm_i × fg3_rate_i, scaled to sim-row team_tpm
#   4. ftm   — foul-drawing tendency; percentile shares
#   5. pts   — DERIVED: 2*fgm + tpm + ftm  (exact arithmetic)
#   6. ast   — potential-assist-weighted redistribution; scorer ≠ assister
#   7. reb   — percentile shares
#   8. stl   — percentile shares
#   9. blk   — percentile shares
#
# Assist model (NBA-specific rates from playoff DB):
#   ASSIST_RATE_3PM = 0.80  (80% of made 3s were assisted)
#   ASSIST_RATE_2PM = 0.49  (49% of made 2s were assisted)
#   Redistribution weighted by pot_ast_share × ast_conv per player
#   working_ast = min(sim_row_ast, sum(assistable_i))
# ============================================================================

library(data.table)
library(readxl)
library(lpSolve)


# ============================================================================
# INPUT READER
# ============================================================================

read_nba_input <- function(file_path) {
  
  sheets <- excel_sheets(file_path)
  
  # ── IDs tab ────────────────────────────────────────────────────────────────
  ids <- as.data.table(read_excel(file_path, sheet = "IDs"))
  setnames(ids, trimws(names(ids)))
  setnames(ids, "Name", "Player")
  
  # ── Games tab ──────────────────────────────────────────────────────────────
  games <- as.data.table(read_excel(file_path, sheet = "Games"))
  setnames(games, trimws(names(games)))
  games[, SimKey := paste0(HomeTeam, "_vs_", AwayTeam)]
  
  team_game_lu <- rbind(
    games[, .(Team = HomeTeam, SimKey, GameKey, GameTime, GameRank,
              OverUnder, HomeSpread, ShowdownFile)],
    games[, .(Team = AwayTeam, SimKey, GameKey, GameTime, GameRank,
              OverUnder, HomeSpread, ShowdownFile)]
  )
  
  # ── Sim_ sheets ────────────────────────────────────────────────────────────
  sim_names <- grep("^Sim_", sheets, value = TRUE)
  if (length(sim_names) == 0) stop("No Sim_ sheets found in input file.")
  sim_games <- setNames(
    lapply(sim_names, function(s) as.data.table(read_excel(file_path, sheet = s))),
    sub("^Sim_", "", sim_names)
  )
  
  # ── SD ID sheets ───────────────────────────────────────────────────────────
  sd_names <- grep("^SD\\d+_IDs$", sheets, value = TRUE)
  sd_ids <- if (length(sd_names) > 0) {
    setNames(
      lapply(sd_names, function(s) {
        dt <- as.data.table(read_excel(file_path, sheet = s))
        setnames(dt, trimws(names(dt)))
        dt
      }),
      sub("_IDs$", "", sd_names)
    )
  } else list()
  
  # ── Team percentile tabs ───────────────────────────────────────────────────
  fixed <- c("IDs", "Games", sd_names, sim_names)
  team_sheet_names <- setdiff(sheets, fixed)
  if (length(team_sheet_names) == 0) stop("No team tabs found.")
  
  team_data <- setNames(
    lapply(team_sheet_names, function(s) {
      dt <- as.data.table(read_excel(file_path, sheet = s))
      setnames(dt, trimws(names(dt)))
      # Drop columns that come from IDs tab — avoids .x/.y collision on merge
      # DKProj/FDProj/Mins live in the IDs tab; team tab is percentile shares only
      drop <- intersect(c("DKSal","FDSal","DKSalary","FDSalary",
                          "DKProj","FDProj","RGProj","RGFDProj","Mins",
                          "DKOwn","FDOwn"), names(dt))
      if (length(drop)) dt[, (drop) := NULL]
      dt
    }),
    team_sheet_names
  )
  
  # ── Build slate ────────────────────────────────────────────────────────────
  slate <- merge(ids, team_game_lu, by = "Team", all.x = TRUE)
  
  # DK position grouping: G / F / C — NBA uses specific positions
  # G slot: PG, SG, G  |  F slot: SF, PF, F  |  C: center only
  # UTIL: anything
  dk_pos_group <- function(pos) {
    fcase(
      grepl("^PG|^SG|^G$|G/",  pos), "G",
      grepl("^SF|^PF|^F$|F/",  pos), "F",
      grepl("^C$|C/|/C",       pos), "C",
      default = "UTIL"
    )
  }
  fd_pos_group <- function(pos) {
    fcase(
      grepl("^PG|^SG|G$|G/|/G", pos), "G",
      grepl("^SF|^PF|F$|F/|/F", pos), "F",
      grepl("^C$|C/|/C",        pos), "C",
      default = "UTIL"
    )
  }
  
  if ("DKPos" %in% names(slate)) {
    slate[, PosGroup := dk_pos_group(DKPos)]
    # Dual eligibility flags — a player can be both G and F eligible (e.g. SG/SF)
    slate[, dk_g_elig := grepl("PG|SG|^G$|G/|/G", DKPos)]
    slate[, dk_f_elig := grepl("SF|PF|^F$|F/|/F", DKPos)]
    slate[, dk_c_elig := grepl("^C$|C/|/C",        DKPos)]
  }
  if ("FDPos" %in% names(slate)) {
    slate[, FDPosGroup := fd_pos_group(FDPos)]
    slate[, fd_g_elig  := grepl("PG|SG|G$|G/|/G",  FDPos)]
    slate[, fd_f_elig  := grepl("SF|PF|F$|F/|/F",  FDPos)]
    slate[, fd_c_elig  := grepl("^C$|C/|/C",        FDPos)]
  }
  slate <- unique(slate, by = "Player")
  
  cat(sprintf("NBA Input: %d players | %d games | %d team tabs | %d sim sheets\n",
              nrow(slate), nrow(games), length(team_data), length(sim_games)))
  
  list(slate = slate, sim_games = sim_games, team_data = team_data,
       games = games, sd_ids = sd_ids)
}


# ============================================================================
# VECTORIZED PERCENTILE INTERPOLATION (identical to CBB — it's correct)
# ============================================================================

interp_shares <- function(draws, p10, p25, p50, p75, p90) {
  p10[is.na(p10)] <- 0
  p25[is.na(p25)] <- p10[is.na(p25)]
  p50[is.na(p50)] <- p25[is.na(p50)]
  p75[is.na(p75)] <- p50[is.na(p75)]
  p90[is.na(p90)] <- p75[is.na(p90)]
  
  n_p <- nrow(draws); n_s <- ncol(draws)
  P10 <- matrix(p10, n_p, n_s); P25 <- matrix(p25, n_p, n_s)
  P50 <- matrix(p50, n_p, n_s); P75 <- matrix(p75, n_p, n_s)
  P90 <- matrix(p90, n_p, n_s)
  
  pmax(
    ifelse(draws <= 0.10, P10,
           ifelse(draws <= 0.25, P10 + (draws-0.10)/0.15 * (P25-P10),
                  ifelse(draws <= 0.50, P25 + (draws-0.25)/0.25 * (P50-P25),
                         ifelse(draws <= 0.75, P50 + (draws-0.50)/0.25 * (P75-P50),
                                ifelse(draws <= 0.90, P75 + (draws-0.75)/0.15 * (P90-P75),
                                       P90))))),
    0
  )
}


# ============================================================================
# SCORING
# ============================================================================

dk_score_nba <- function(pts, tpm, reb, ast, stl, blk, to) {
  base <- pts + tpm*0.5 + reb*1.25 + ast*1.5 + stl*2.0 + blk*2.0 - to*0.5
  cats <- (pts >= 10) + (reb >= 10) + (ast >= 10) + (blk >= 10) + (stl >= 10)
  base + ifelse(cats >= 3, 3.0, ifelse(cats >= 2, 1.5, 0.0))
}

fd_score_nba <- function(pts, tpm, reb, ast, stl, blk, to) {
  # FD NBA scoring: STL/BLK = +3 (not +2), no DD/TD bonus, TO = -1
  pts + tpm*0.5 + reb*1.20 + ast*1.5 + stl*3.0 + blk*3.0 - to*1.0
}


# ============================================================================
# MAIN SIMULATION
# ============================================================================

run_nba_simulation <- function(input_data, n_sims = 10000, config = NULL,
                               progress_callback = NULL) {
  
  slate     <- input_data$slate
  sim_games <- input_data$sim_games
  team_data <- input_data$team_data
  
  cb <- function(detail, value) {
    if (!is.null(progress_callback)) progress_callback(detail, value)
    cat(sprintf("  [%.0f%%] %s\n", value * 100, detail))
    flush.console()
  }
  
  start_time   <- proc.time()
  team_abbrevs <- unique(slate$Team)
  
  # NBA-specific assist rates (computed from playoff DB: 674 team-game rows)
  ASSIST_RATE_3PM <- 0.80   # 80% of made 3s were assisted
  ASSIST_RATE_2PM <- 0.49   # 49% of made 2s were assisted
  
  # Stats allocated via percentile shares (tpm excluded — derived from fgm)
  share_stats <- c("fgm", "ftm", "reb", "ast", "stl", "blk", "to")
  
  # Sim sheet column names for each stat
  sim_col <- c(fgm="fgm", ftm="ftm", reb="reb", ast="ast",
               stl="stl", blk="blk", to="to")
  # tpm used for assist model + fg3 scaling; pts available for validation
  sim_tpm_col <- "tpm"
  sim_pts_col <- "pts"
  
  for (ta in team_abbrevs)
    if (!ta %in% names(team_data))
      stop(sprintf("No team tab found for: %s", ta))
  
  pct_cols <- list(
    fgm = c("fgm_p10","fgm_p25","fgm_p50","fgm_p75","fgm_p90"),
    ftm = c("ftm_p10","ftm_p25","ftm_p50","ftm_p75","ftm_p90"),
    reb = c("reb_p10","reb_p25","reb_p50","reb_p75","reb_p90"),
    ast = c("ast_p10","ast_p25","ast_p50","ast_p75","ast_p90"),
    stl = c("stl_p10","stl_p25","stl_p50","stl_p75","stl_p90"),
    blk = c("blk_p10","blk_p25","blk_p50","blk_p75","blk_p90"),
    to  = c("to_p10", "to_p25", "to_p50", "to_p75", "to_p90")
  )
  
  # ── Build player list ──────────────────────────────────────────────────────
  cb("Building player roster...", 0.03)
  
  slate_cols <- c("Player","DKID","FDID","DKSalary","FDSalary","DKPos","FDPos",
                  "PosGroup","FDPosGroup","GameKey","SimKey","GameTime","GameRank",
                  "OverUnder","HomeSpread","DKProj","FDProj","Mins","Team")
  
  player_list <- rbindlist(lapply(team_abbrevs, function(team) {
    tab <- team_data[[team]]
    sl  <- slate[Team == team, intersect(slate_cols, names(slate)), with = FALSE]
    matched <- tab[Name %in% sl$Player]
    if (nrow(matched) == 0) return(NULL)
    merged <- merge(matched, sl, by.x = "Name", by.y = "Player", all.x = TRUE)
    for (col in c("DKProj","FDProj","Mins","fg3_rate","pot_ast_share","ast_conv"))
      if (!col %in% names(merged)) merged[, (col) := NA_real_]
    merged[, Team := team]
    merged
  }), fill = TRUE)
  
  if (nrow(player_list) == 0) stop("No players matched between IDs and team tabs.")
  
  n_players    <- nrow(player_list)
  player_names <- player_list$Name
  player_teams <- player_list$Team
  
  cat(sprintf("  Active players: %d\n", n_players))
  
  # ── Resolve sim sheets ─────────────────────────────────────────────────────
  game_keys <- unique(player_list$SimKey)
  game_sim_dt <- setNames(lapply(game_keys, function(gk) {
    if (gk %in% names(sim_games)) return(sim_games[[gk]])
    parts   <- strsplit(gk, "_vs_")[[1]]
    rev_key <- paste0(parts[2], "_vs_", parts[1])
    if (rev_key %in% names(sim_games)) return(sim_games[[rev_key]])
    stop(sprintf("No Sim_ sheet found for game: %s", gk))
  }), game_keys)
  
  # ── Sample game rows — same-row constraint per game ────────────────────────
  # Both teams in a game share the same row index, preserving the real-world
  # correlation between opponent scoring environments.
  cb("Sampling game rows...", 0.06)
  
  # Equal-weight sampling — all similar games treated as equally valid
  game_row_idx <- setNames(lapply(game_keys, function(gk) {
    dt <- game_sim_dt[[gk]]
    sample.int(nrow(dt), n_sims, replace = TRUE)
  }), game_keys)
  
  # ── Per-team prep ──────────────────────────────────────────────────────────
  cb("Prepping team data...", 0.08)
  
  team_data_prepped <- setNames(lapply(team_abbrevs, function(team) {
    pidx   <- which(player_teams == team)
    n_team <- length(pidx)
    gk     <- player_list[pidx[1], SimKey]
    dt     <- game_sim_dt[[gk]]
    ri     <- game_row_idx[[gk]]
    
    # Team stat totals from sampled game rows
    totals <- setNames(lapply(share_stats, function(s) {
      col <- paste0(team, "_", sim_col[s])
      if (!col %in% names(dt)) stop(sprintf("Sim sheet missing column: %s", col))
      as.numeric(dt[[col]])[ri]
    }), share_stats)
    
    # tpm total from sim row (for assist model + tpm derivation scaling)
    tpm_col <- paste0(team, "_", sim_tpm_col)
    totals[["tpm_sim"]] <- if (tpm_col %in% names(dt)) as.numeric(dt[[tpm_col]])[ri] else rep(0, n_sims)
    
    # Percentile share matrices (n_team × 5)
    pcts <- setNames(lapply(share_stats, function(s) {
      cols <- pct_cols[[s]]
      m    <- matrix(0.0, n_team, 5)
      for (j in seq_along(cols)) {
        col <- cols[j]
        if (col %in% names(player_list)) {
          v <- as.numeric(player_list[[col]][pidx])
          m[, j] <- ifelse(is.na(v), 0, v)
        }
      }
      m
    }), share_stats)
    
    # Player-level fg3_rate, pot_ast_share, ast_conv
    fg3_rate     <- as.numeric(player_list$fg3_rate[pidx])
    fg3_rate[is.na(fg3_rate)] <- 0.30  # league average fallback
    
    pot_ast_share <- as.numeric(player_list$pot_ast_share[pidx])
    pot_ast_share[is.na(pot_ast_share)] <- 1 / n_team  # equal share fallback
    
    ast_conv <- as.numeric(player_list$ast_conv[pidx])
    ast_conv[is.na(ast_conv)] <- 0.35  # league average fallback
    
    # Combined assist weight: pot_ast_share × ast_conv, normalized
    ast_weight_raw <- pot_ast_share * ast_conv
    ast_weight_sum <- sum(ast_weight_raw, na.rm = TRUE)
    ast_weight <- if (ast_weight_sum > 0) ast_weight_raw / ast_weight_sum
    else rep(1/n_team, n_team)
    
    list(pidx = pidx, n_team = n_team, totals = totals, pcts = pcts,
         fg3_rate = fg3_rate, ast_weight = ast_weight)
  }), team_abbrevs)
  
  # ── Pre-draw uniform samples ───────────────────────────────────────────────
  cb("Drawing percentiles...", 0.10)
  
  team_draws <- setNames(lapply(team_abbrevs, function(team) {
    n_team <- team_data_prepped[[team]]$n_team
    setNames(
      lapply(share_stats, function(s) matrix(runif(n_team * n_sims), n_team, n_sims)),
      share_stats
    )
  }), team_abbrevs)
  
  # ── Allocate share-based stats ─────────────────────────────────────────────
  cb("Allocating stats...", 0.15)
  
  stat_mats <- setNames(
    lapply(share_stats, function(s) matrix(0L, n_players, n_sims)),
    share_stats
  )
  # tpm gets its own matrix (derived)
  tpm_mat <- matrix(0L, n_players, n_sims)
  
  for (team in team_abbrevs) {
    td   <- team_data_prepped[[team]]
    pidx <- td$pidx
    
    for (s in share_stats) {
      pm  <- td$pcts[[s]]
      shr <- interp_shares(team_draws[[team]][[s]],
                           pm[,1], pm[,2], pm[,3], pm[,4], pm[,5])
      cs  <- colSums(shr); cs[cs == 0] <- 1
      raw <- sweep(sweep(shr, 2, cs, `/`), 2, td$totals[[s]], `*`)
      stat_mats[[s]][pidx, ] <- matrix(as.integer(round(raw)), length(pidx), n_sims)
    }
  }
  
  # ── Derive tpm from fgm × fg3_rate, scaled to sim-row tpm total ───────────
  # This preserves player shot-type tendency while anchoring to game environment.
  # For each sim: tpm_i_natural = fgm_i × fg3_rate_i
  # Scale so sum(tpm_i) = team_tpm_simrow
  # Hard constraint: tpm_i <= fgm_i
  cb("Deriving 3-pointers...", 0.55)
  
  for (team in team_abbrevs) {
    td   <- team_data_prepped[[team]]
    pidx <- td$pidx
    
    fgm_t      <- stat_mats[["fgm"]][pidx, , drop = FALSE]   # n_team × n_sims
    fg3_rate_v <- td$fg3_rate                                  # n_team vector
    sim_tpm    <- td$totals[["tpm_sim"]]                       # n_sims vector
    
    # Natural tpm per player: fgm × fg3_rate
    tpm_natural <- sweep(fgm_t, 1, fg3_rate_v, `*`)            # n_team × n_sims
    
    # Scale to match sim-row tpm total
    natural_sum <- colSums(tpm_natural)
    natural_sum[natural_sum == 0] <- 1
    
    scale_factor <- sim_tpm / natural_sum                       # n_sims
    tpm_scaled   <- sweep(tpm_natural, 2, scale_factor, `*`)
    
    # Round and enforce tpm <= fgm — use matrix() to preserve dimensions
    n_team_tpm <- nrow(fgm_t)
    tpm_rounded  <- pmin(matrix(as.integer(round(tpm_scaled)), n_team_tpm, n_sims), fgm_t)
    
    # Reconcile integer rounding: sum may be off by 1-2 per sim
    # Adjust by adding/removing from players with largest fractional parts
    tpm_frac     <- tpm_scaled - floor(tpm_scaled)
    target_sum   <- as.integer(round(sim_tpm))
    
    for (s in seq_len(n_sims)) {
      current <- sum(tpm_rounded[, s])
      diff    <- target_sum[s] - current
      if (diff == 0L || all(fgm_t[, s] == 0L)) next
      if (diff > 0L) {
        # Need to add: pick players with room (tpm < fgm) and largest frac
        eligible <- which(tpm_rounded[, s] < fgm_t[, s])
        if (length(eligible) == 0) next
        ord <- eligible[order(tpm_frac[eligible, s], decreasing = TRUE)]
        add <- seq_len(min(diff, length(ord)))
        tpm_rounded[ord[add], s] <- tpm_rounded[ord[add], s] + 1L
      } else if (diff < 0L) {
        # Need to subtract: pick players with tpm > 0 and smallest frac
        eligible <- which(tpm_rounded[, s] > 0L)
        if (length(eligible) == 0) next
        ord <- eligible[order(tpm_frac[eligible, s], decreasing = FALSE)]
        sub_n <- seq_len(min(abs(diff), length(ord)))
        tpm_rounded[ord[sub_n], s] <- tpm_rounded[ord[sub_n], s] - 1L
      }
    }
    
    tpm_mat[pidx, ] <- tpm_rounded
  }
  
  # ── Derive pts ─────────────────────────────────────────────────────────────
  # pts = 2*fgm + tpm + ftm  (exact arithmetic, never independently allocated)
  pts_mat <- 2L * stat_mats[["fgm"]] + tpm_mat + stat_mats[["ftm"]]
  
  # ── Assist reallocation — scorer ≠ assister ────────────────────────────────
  # Uses potential-assist-weighted redistribution.
  # Each player's assistable baskets generate assist credit for teammates only,
  # weighted by teammate pot_ast_share × ast_conv (combined in ast_weight).
  cb("Assigning assists...", 0.75)
  
  for (team in team_abbrevs) {
    td    <- team_data_prepped[[team]]
    pidx  <- td$pidx
    n_t   <- td$n_team
    
    fgm_t   <- stat_mats[["fgm"]][pidx, , drop = FALSE]
    tpm_t   <- tpm_mat[pidx, , drop = FALSE]
    twom_t  <- pmax(fgm_t - tpm_t, 0L)
    ast_t   <- stat_mats[["ast"]][pidx, , drop = FALSE]  # initial share allocation
    
    # Assistable baskets per player per sim
    assistable <- round(tpm_t * ASSIST_RATE_3PM + twom_t * ASSIST_RATE_2PM)
    
    # Working assist total: bounded by both sim-row total and assistable ceiling
    team_assistable <- colSums(assistable)
    team_sim_ast    <- colSums(ast_t)
    working_ast     <- pmin(team_sim_ast, team_assistable)
    working_ast[is.na(working_ast)] <- 0L
    
    # Scale initial ast allocation to working total
    scale <- ifelse(!is.na(team_sim_ast) & team_sim_ast > 0,
                    working_ast / team_sim_ast, 1)
    scale[is.na(scale)] <- 1
    ast_scaled <- sweep(ast_t, 2, scale, `*`)
    
    # Integer reconciliation via fractional parts
    ast_floor <- matrix(as.integer(floor(ast_scaled)), n_t, n_sims)
    residual  <- working_ast - colSums(ast_floor)
    residual[is.na(residual)] <- 0L
    frac      <- ast_scaled - ast_floor
    
    for (s in seq_len(n_sims)) {
      r <- as.integer(residual[s])
      if (!is.na(r) && r > 0L) {
        top_idx <- order(frac[, s], decreasing = TRUE)[seq_len(r)]
        ast_floor[top_idx, s] <- ast_floor[top_idx, s] + 1L
      }
    }
    
    # Redistribute assists using potential-assist weights (scorer ≠ assister)
    # td$ast_weight: n_team vector of combined pot_ast_share × ast_conv, normalized
    ast_weight_base <- td$ast_weight   # n_team, normalized
    
    new_ast <- matrix(0L, n_t, n_sims)
    
    for (i in seq_len(n_t)) {
      baskets_i <- assistable[i, ]
      if (all(baskets_i == 0L)) next
      
      # Build per-sim weight matrix: exclude player i from their own assist pool
      w         <- matrix(ast_weight_base, n_t, n_sims)
      w[i, ]    <- 0
      col_sums_w <- colSums(w)
      
      # Handle sims where no teammates have weight
      zero_cols <- col_sums_w == 0
      if (any(zero_cols)) {
        uniform <- 1 / (n_t - 1L)
        w[, zero_cols]       <- uniform
        w[i, zero_cols]      <- 0
        col_sums_w[zero_cols] <- 1
      }
      
      w <- sweep(w, 2, col_sums_w, `/`)
      
      # Distribute player i's assistable baskets to teammates
      contrib_real  <- sweep(w, 2, baskets_i, `*`)
      contrib_floor <- matrix(as.integer(floor(contrib_real)), n_t, n_sims)
      contrib_frac  <- contrib_real - contrib_floor
      contrib_resid <- baskets_i - colSums(contrib_floor)
      contrib_resid[is.na(contrib_resid)] <- 0L
      
      for (s in seq_len(n_sims)) {
        r <- as.integer(contrib_resid[s])
        if (!is.na(r) && r > 0L) {
          elig    <- setdiff(order(contrib_frac[, s], decreasing = TRUE), i)
          top_idx <- elig[seq_len(min(r, length(elig)))]
          contrib_floor[top_idx, s] <- contrib_floor[top_idx, s] + 1L
        }
      }
      
      new_ast <- new_ast + contrib_floor
    }
    
    stat_mats[["ast"]][pidx, ] <- new_ast
  }
  
  # ── Score ──────────────────────────────────────────────────────────────────
  cb("Scoring...", 0.90)
  
  dk_mat <- dk_score_nba(pts_mat, tpm_mat,
                         stat_mats[["reb"]], stat_mats[["ast"]],
                         stat_mats[["stl"]], stat_mats[["blk"]],
                         stat_mats[["to"]])
  fd_mat <- fd_score_nba(pts_mat, tpm_mat,
                         stat_mats[["reb"]], stat_mats[["ast"]],
                         stat_mats[["stl"]], stat_mats[["blk"]],
                         stat_mats[["to"]])
  
  # ── Assemble sim results ───────────────────────────────────────────────────
  cb("Assembling results...", 0.93)
  
  sim_results <- data.table(
    SimID   = rep(seq_len(n_sims), each = n_players),
    Player  = rep(player_names,    times = n_sims),
    DKScore = as.vector(dk_mat),
    FDScore = as.vector(fd_mat)
  )
  for (s in share_stats)
    sim_results[[s]] <- as.integer(as.vector(stat_mats[[s]]))
  sim_results[["tpm"]] <- as.integer(as.vector(tpm_mat))
  sim_results[["pts"]] <- as.integer(as.vector(pts_mat))
  sim_results[["twom"]] <- as.integer(as.vector(
    pmax(stat_mats[["fgm"]] - tpm_mat, 0L)
  ))
  
  # ── Build metadata ─────────────────────────────────────────────────────────
  cb("Building metadata...", 0.96)
  
  keep_cols <- intersect(
    c("Name","DKID","FDID","DKSalary","FDSalary","DKPos","FDPos",
      "DKOwn","FDOwn","PosGroup","FDPosGroup","Team","GameKey","SimKey",
      "GameTime","GameRank","OverUnder","HomeSpread","DKProj","FDProj","Mins"),
    names(player_list)
  )
  metadata <- unique(player_list[, ..keep_cols], by = "Name")
  setnames(metadata, "Name", "Player")
  
  metadata[, GameTimeSort := as.numeric(as.POSIXct(
    paste(Sys.Date(), GameTime), format = "%Y-%m-%d %I:%M %p", tz = "America/New_York"
  ))]
  
  # Attach SD IDs
  if (length(input_data$sd_ids) > 0) {
    game_sd_lu <- unique(rbind(
      input_data$games[, .(Team = HomeTeam, ShowdownFile)],
      input_data$games[, .(Team = AwayTeam,  ShowdownFile)]
    ))
    metadata <- merge(metadata, game_sd_lu, by = "Team", all.x = TRUE)
    
    sd_all <- rbindlist(lapply(names(input_data$sd_ids), function(n) {
      dt <- copy(input_data$sd_ids[[n]]); dt[, SDFile := n]; dt
    }), fill = TRUE)
    setnames(sd_all, "Name", "Player")
    # Normalise player names same as DK CSV (strips BOM/accents)
    sd_all[, Player := trimws(iconv(Player, to = "ASCII//TRANSLIT"))]
    
    # Rename SD columns cleanly before merge — avoids .SD scoping issues
    sd_sub <- sd_all[, .(
      Player    = Player,
      Team      = Team,
      SDFile    = SDFile,
      CPTID     = CPT_ID,
      CPTSalary = as.numeric(CPT_Salary),
      SDID      = as.character(UTIL_ID),
      SDSalary  = as.numeric(UTIL_Salary)
    )]
    # Ensure ShowdownFile is populated on metadata before the three-key merge.
    # game_sd_lu maps Team -> ShowdownFile; players whose team isn't in Games
    # will have NA ShowdownFile and miss the join.
    if (!"ShowdownFile" %in% names(metadata))
      metadata[, ShowdownFile := NA_character_]
    
    # Fill any NA ShowdownFile from game_sd_lu
    missing_sf <- is.na(metadata$ShowdownFile)
    if (any(missing_sf)) {
      lu_vec <- setNames(game_sd_lu$ShowdownFile, game_sd_lu$Team)
      metadata[missing_sf, ShowdownFile := lu_vec[Team]]
    }
    
    metadata <- merge(
      metadata,
      sd_sub,
      by.x = c("Player","Team","ShowdownFile"),
      by.y = c("Player","Team","SDFile"),
      all.x = TRUE
    )
  }
  
  sim_results <- sim_results[Player %in% metadata$Player]
  
  has_fd <- "FDSalary" %in% names(metadata) &&
    any(!is.na(metadata$FDSalary) & metadata$FDSalary > 0)
  has_sd <- "CPTSalary" %in% names(metadata) &&
    any(!is.na(metadata$CPTSalary) & metadata$CPTSalary > 0)
  
  if (length(input_data$sd_ids) > 0 && !has_sd) {
    cat("  Warning: SD IDs loaded but no CPTSalary in metadata after join.\n")
    cat(sprintf("  metadata cols: %s\n", paste(names(metadata), collapse=", ")))
    cat(sprintf("  ShowdownFile values: %s\n",
                paste(unique(metadata$ShowdownFile), collapse=", ")))
  }
  
  elapsed <- round((proc.time() - start_time)["elapsed"], 1)
  cat(sprintf("  NBA sim complete: %d sims | %d players | %.1fs\n",
              n_sims, nrow(metadata), elapsed))
  
  # ── Pre-aggregate visuals ──────────────────────────────────────────────────
  cb("Building visuals...", 0.98)
  
  teams       <- sort(unique(metadata$Team))
  twom_mat_v  <- pmax(stat_mats[["fgm"]] - tpm_mat, 0L)
  
  player_means <- data.table(
    Player = player_names,
    Team   = player_teams,
    AvgFP  = round(rowMeans(dk_mat),              1),
    pts    = round(rowMeans(pts_mat),              1),
    tpm    = round(rowMeans(tpm_mat),              1),
    twom   = round(rowMeans(twom_mat_v),           1),
    ftm    = round(rowMeans(stat_mats[["ftm"]]),   1),
    reb    = round(rowMeans(stat_mats[["reb"]]),   1),
    ast    = round(rowMeans(stat_mats[["ast"]]),   1),
    stl    = round(rowMeans(stat_mats[["stl"]]),   1),
    blk    = round(rowMeans(stat_mats[["blk"]]),   1),
    to     = round(rowMeans(stat_mats[["to"]]),    1)
  )
  setorder(player_means, Team, -AvgFP)
  
  team_means <- rbindlist(lapply(teams, function(tm) {
    pidx <- which(player_teams == tm)
    data.table(
      Team  = tm,
      AvgFP = round(mean(colSums(dk_mat[pidx,,drop=FALSE])),           1),
      pts   = round(mean(colSums(pts_mat[pidx,,drop=FALSE])),          1),
      tpm   = round(mean(colSums(tpm_mat[pidx,,drop=FALSE])),          1),
      twom  = round(mean(colSums(twom_mat_v[pidx,,drop=FALSE])),       1),
      ftm   = round(mean(colSums(stat_mats[["ftm"]][pidx,,drop=FALSE])),1),
      reb   = round(mean(colSums(stat_mats[["reb"]][pidx,,drop=FALSE])),1),
      ast   = round(mean(colSums(stat_mats[["ast"]][pidx,,drop=FALSE])),1),
      stl   = round(mean(colSums(stat_mats[["stl"]][pidx,,drop=FALSE])),1),
      blk   = round(mean(colSums(stat_mats[["blk"]][pidx,,drop=FALSE])),1),
      to    = round(mean(colSums(stat_mats[["to"]][pidx,,drop=FALSE])), 1)
    )
  }))
  setorder(team_means, -AvgFP)
  
  sport_visuals <- list(
    teams        = teams,
    player_means = player_means,
    team_means   = team_means
  )
  
  list(sim_results  = sim_results,
       metadata     = metadata,
       has_fd       = has_fd,
       has_sd       = has_sd,
       sport_visuals = sport_visuals)
}


# ============================================================================
# NBA SLOT ASSIGNMENT
# DK:  PG/SG/SF/PF/C/G/F/UTIL  (8 players)
# FD:  PG/SG/SF/PF/C/PG/SG/SF  (actually FD uses G/F/C/UTIL system)
#
# Simplified grouping used for optimizer:
#   G-eligible: PosGroup == "G"  (PG, SG, G, PG/SG etc.)
#   F-eligible: PosGroup == "F"  (SF, PF, F, SF/PF etc.)
#   C-eligible: PosGroup == "C"
#   Any: UTIL
#
# DK slots: 1PG, 1SG, 1SF, 1PF, 1C, 1G, 1F, 1UTIL
# Simplified to LP constraints: >=2 G-elig, >=2 F-elig, >=1 C-elig, 8 total
# Post-LP slot assignment maps players to named slots deterministically.
# ============================================================================

assign_nba_slots_dk <- function(cm) {
  # cm: data.table with Player, DKPos (or PosGroup), game_rank
  # Handles dual-eligible positions: SG/SF fills G slots OR F slots
  # Slot preference order: specific named slot -> flex slot -> UTIL
  
  setorder(cm, game_rank, Player)
  
  slots <- list(PG=NA_character_, SG=NA_character_,
                SF=NA_character_, PF=NA_character_,
                C =NA_character_, G =NA_character_,
                F =NA_character_, UTIL=NA_character_)
  
  # Derive eligibility per player from DKPos if available, else PosGroup
  pos_vec <- if ("DKPos" %in% names(cm)) cm$DKPos else cm$PosGroup
  
  fill_slot <- function(player, pos) {
    # Build candidate slots based on SPECIFIC positions listed in pos string
    # A PG fills: PG, G, UTIL — NOT SG/SF/PF/F
    # A SG/SF fills: SG, SF, G, F, UTIL
    # A SF/PF fills: SF, PF, F, UTIL
    # A C fills: C, UTIL
    candidates <- character(0)
    if (grepl("PG", pos)) candidates <- c(candidates, "PG")
    if (grepl("SG", pos)) candidates <- c(candidates, "SG")
    if (grepl("SF", pos)) candidates <- c(candidates, "SF")
    if (grepl("PF", pos)) candidates <- c(candidates, "PF")
    if (grepl("^C$|C/|/C", pos)) candidates <- c(candidates, "C")
    # Generic flex slots: G if any guard, F if any forward
    if (grepl("PG|SG|^G$|G/|/G", pos)) candidates <- c(candidates, "G")
    if (grepl("SF|PF|^F$|F/|/F", pos)) candidates <- c(candidates, "F")
    candidates <- c(candidates, "UTIL")
    candidates <- unique(candidates)
    
    for (sl in candidates) {
      if (sl %in% names(slots) && is.na(slots[[sl]])) {
        slots[[sl]] <<- player; return(TRUE)
      }
    }
    FALSE
  }
  
  for (idx in seq_len(nrow(cm))) {
    if (!fill_slot(cm$Player[idx], pos_vec[idx])) return(NULL)
  }
  
  if (any(sapply(slots, is.na))) return(NULL)
  slots
}

assign_nba_slots_fd <- function(cm) {
  # FD NBA: PG / PG / SG / SG / SF / SF / PF / PF / C  (9 players, $60K)
  # Handles dual-eligible positions: SG/SF fills G or F slots
  setorder(cm, game_rank, Player)
  
  slots <- list(PG1=NA_character_, PG2=NA_character_,
                SG1=NA_character_, SG2=NA_character_,
                SF1=NA_character_, SF2=NA_character_,
                PF1=NA_character_, PF2=NA_character_,
                C  =NA_character_)
  
  pos_vec <- if ("FDPos" %in% names(cm)) cm$FDPos else cm$FDPosGroup
  
  fill_slot <- function(player, pos) {
    # Position-specific: PG fills PG slots only, SG fills SG slots only etc.
    # SG/SF fills SG or SF slots.
    candidates <- character(0)
    if (grepl("PG", pos)) candidates <- c(candidates, "PG1","PG2")
    if (grepl("SG", pos)) candidates <- c(candidates, "SG1","SG2")
    if (grepl("SF", pos)) candidates <- c(candidates, "SF1","SF2")
    if (grepl("PF", pos)) candidates <- c(candidates, "PF1","PF2")
    if (grepl("^C$|C/|/C", pos)) candidates <- c(candidates, "C")
    candidates <- unique(candidates)
    for (sl in candidates) {
      if (sl %in% names(slots) && is.na(slots[[sl]])) {
        slots[[sl]] <<- player; return(TRUE)
      }
    }
    FALSE
  }
  
  for (idx in seq_len(nrow(cm))) {
    if (!fill_slot(cm$Player[idx], pos_vec[idx])) return(NULL)
  }
  
  if (any(sapply(slots, is.na))) return(NULL)
  slots
}


# ============================================================================
# NBA DK CLASSIC OPTIMIZER
# Roster: PG / SG / SF / PF / C / G / F / UTIL  (8 players, $50K)
# LP constraints: 8 total, <=50K, >=2 G-elig, >=2 F-elig, >=1 C-elig
# ============================================================================

find_optimal_lineups_nba <- function(sim_results, metadata, config,
                                     verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: NBA DK lineups (per-sim LP)...\n")
  setDT(sim_results); setDT(metadata)
  
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  
  meta <- unique(metadata[, .(Player, DKSalary, PosGroup,
                              DKPos = if("DKPos" %in% names(metadata)) DKPos else NA_character_,
                              GameKey)], by = "Player")
  # Use DKPos directly for dual eligibility — SG/SF is both G and F eligible
  if ("DKPos" %in% names(meta) && !all(is.na(meta$DKPos))) {
    meta[, g_elig := grepl("PG|SG|^G$|G/|/G", DKPos)]
    meta[, f_elig := grepl("SF|PF|^F$|F/|/F", DKPos)]
    meta[, c_elig := grepl("^C$|C/|/C",        DKPos)]
  } else {
    meta[, g_elig := PosGroup == "G"]
    meta[, f_elig := PosGroup == "F"]
    meta[, c_elig := PosGroup == "C"]
  }
  
  if ("GameRank" %in% names(metadata)) {
    meta <- merge(meta, unique(metadata[, .(Player, GameRank)]), by = "Player", all.x = TRUE)
    meta[, game_rank := GameRank]; meta[is.na(game_rank), game_rank := 1L]
    meta[, GameRank := NULL]
  } else meta[, game_rank := 1L]
  
  opt_data <- merge(
    sim_results[, .(SimID, Player, FantasyPoints = DKScore)],
    meta[, .(Player, Salary = DKSalary, g_elig, f_elig, c_elig, game_rank)],
    by = "Player"
  )
  opt_data <- opt_data[Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints)]
  setkey(opt_data, SimID)
  
  sim_ids   <- unique(opt_data$SimID)
  n_sims    <- length(sim_ids)
  start_t   <- Sys.time()
  prog_freq <- max(1L, n_sims %/% 20L)
  
  if (verbose) cat(sprintf("  %d players | %s sims | $%s cap\n",
                           nrow(meta), format(n_sims, big.mark=","),
                           format(salary_cap, big.mark=",")))
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid  <- sim_ids[i]
    pool <- opt_data[.(sid)]
    n_p  <- nrow(pool)
    if (n_p < 8L) next
    
    res <- tryCatch(
      lp("max", pool$FantasyPoints,
         rbind(rep(1,n_p), pool$Salary,
               as.integer(pool$g_elig), as.integer(pool$f_elig),
               as.integer(pool$c_elig)),
         c("==","<=",">=",">=",">="),
         c(8L, salary_cap, 2L, 2L, 1L), all.bin = TRUE),
      error = function(e) list(status = 1L)
    )
    if (res$status != 0L) next
    selected <- which(res$solution == 1L)
    if (length(selected) != 8L) next
    
    chosen <- pool[selected]
    sig    <- paste(sort(chosen$Player), collapse = "|")
    lineup_list[[i]] <- data.table(
      Lineup      = sig,
      TotalSalary = sum(chosen$Salary),
      TotalScore  = sum(chosen$FantasyPoints)
    )
    
    if (verbose && i %% prog_freq == 0L) {
      cat(sprintf("\r  Phase 1: %d%% | %.1fs",
                  round(i/n_sims*100),
                  as.numeric(difftime(Sys.time(), start_t, units="secs"))))
      flush.console()
    }
  }
  if (verbose) cat("\n")
  
  valid <- lineup_list[!sapply(lineup_list, is.null)]
  if (length(valid) == 0L) stop("No valid NBA DK lineups found")
  
  all_dt <- rbindlist(valid)
  counts <- all_dt[, .(Top1Count=.N, TotalSalary=TotalSalary[1],
                       AvgScore=mean(TotalScore)), by = Lineup]
  counts[, rand := runif(.N)]
  setorder(counts, -Top1Count, rand)
  counts[, rand := NULL]
  
  slot_list <- vector("list", nrow(counts))
  for (li in seq_len(nrow(counts))) {
    players <- strsplit(counts$Lineup[li], "\\|")[[1]]
    cm_cols <- intersect(c("Player","PosGroup","DKPos","game_rank"), names(meta))
    cm      <- meta[Player %in% players, ..cm_cols]
    slots   <- assign_nba_slots_dk(cm)
    if (!is.null(slots)) {
      slot_list[[li]] <- as.data.table(c(list(Lineup = counts$Lineup[li]), slots))
    }
  }
  
  slot_dt <- rbindlist(slot_list[!sapply(slot_list, is.null)])
  counts  <- merge(counts, slot_dt, by = "Lineup", all.x = TRUE)
  
  unique_lineups <- counts[!is.na(PG), .(
    TotalSalary, Top1Count, AvgScore,
    Player1=PG, Player2=SG, Player3=SF, Player4=PF,
    Player5=C, Player6=G, Player7=F, Player8=UTIL
  )]
  
  if (nrow(unique_lineups) > max_lineups) unique_lineups <- unique_lineups[1:max_lineups]
  
  elapsed <- as.numeric(difftime(Sys.time(), start_t, units="secs"))
  if (verbose) cat(sprintf("  \u2713 %s DK lineups | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","), elapsed))
  
  list(unique_lineups = unique_lineups, n_sims = n_sims, config = config, mode = "nba")
}


# ============================================================================
# NBA FD CLASSIC OPTIMIZER
# Roster: PG / PG / SG / SG / SF / PF / C / UTIL  (8 players, $60K)
# LP constraints: 8 total, <=$60K, >=2 G-elig, >=2 F-elig, >=1 C-elig
# ============================================================================

find_optimal_lineups_nba_fd <- function(sim_results, metadata, config,
                                        verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: NBA FD lineups (per-sim LP)...\n")
  setDT(sim_results); setDT(metadata)
  
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  
  pos_col <- if ("FDPosGroup" %in% names(metadata)) "FDPosGroup" else "PosGroup"
  meta <- unique(metadata[, .(Player, FDSalary,
                              FDPosGroup = get(pos_col),
                              FDPos = if("FDPos" %in% names(metadata)) FDPos else NA_character_,
                              GameKey)], by = "Player")
  # Use FDPos directly for dual eligibility
  if ("FDPos" %in% names(meta) && !all(is.na(meta$FDPos))) {
    meta[, g_elig := grepl("PG|SG|G$|G/|/G",  FDPos)]
    meta[, f_elig := grepl("SF|PF|F$|F/|/F",  FDPos)]
    meta[, c_elig := grepl("^C$|C/|/C",        FDPos)]
  } else {
    meta[, g_elig := FDPosGroup == "G"]
    meta[, f_elig := FDPosGroup == "F"]
    meta[, c_elig := FDPosGroup == "C"]
  }
  meta <- meta[FDSalary > 0 & !is.na(FDSalary)]
  
  if ("GameRank" %in% names(metadata)) {
    meta <- merge(meta, unique(metadata[, .(Player, GameRank)]), by = "Player", all.x = TRUE)
    meta[, game_rank := GameRank]; meta[is.na(game_rank), game_rank := 1L]
    meta[, GameRank := NULL]
  } else meta[, game_rank := 1L]
  
  opt_data <- merge(
    sim_results[, .(SimID, Player, FantasyPoints = FDScore)],
    meta[, .(Player, Salary = FDSalary, g_elig, f_elig, c_elig, game_rank)],
    by = "Player"
  )
  opt_data <- opt_data[Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints)]
  setkey(opt_data, SimID)
  
  sim_ids   <- unique(opt_data$SimID)
  n_sims    <- length(sim_ids)
  start_t   <- Sys.time()
  prog_freq <- max(1L, n_sims %/% 20L)
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid  <- sim_ids[i]
    pool <- opt_data[.(sid)]
    n_p  <- nrow(pool)
    if (n_p < 9L) next
    
    res <- tryCatch(
      lp("max", pool$FantasyPoints,
         rbind(rep(1,n_p), pool$Salary,
               as.integer(pool$g_elig), as.integer(pool$f_elig),
               as.integer(pool$c_elig)),
         c("==","<=",">=",">=",">="),
         c(9L, salary_cap, 4L, 4L, 1L), all.bin = TRUE),
      error = function(e) list(status = 1L)
    )
    if (res$status != 0L) next
    selected <- which(res$solution == 1L)
    if (length(selected) != 9L) next
    
    chosen <- pool[selected]
    sig    <- paste(sort(chosen$Player), collapse = "|")
    lineup_list[[i]] <- data.table(
      Lineup      = sig,
      TotalSalary = sum(chosen$Salary),
      TotalScore  = sum(chosen$FantasyPoints)
    )
    
    if (verbose && i %% prog_freq == 0L) {
      cat(sprintf("\r  Phase 1: %d%% | %.1fs",
                  round(i/n_sims*100),
                  as.numeric(difftime(Sys.time(), start_t, units="secs"))))
      flush.console()
    }
  }
  if (verbose) cat("\n")
  
  valid <- lineup_list[!sapply(lineup_list, is.null)]
  if (length(valid) == 0L) stop("No valid NBA FD lineups found")
  
  all_dt <- rbindlist(valid)
  counts <- all_dt[, .(Top1Count=.N, TotalSalary=TotalSalary[1],
                       AvgScore=mean(TotalScore)), by = Lineup]
  counts[, rand := runif(.N)]
  setorder(counts, -Top1Count, rand)
  counts[, rand := NULL]
  
  slot_list <- vector("list", nrow(counts))
  for (li in seq_len(nrow(counts))) {
    players <- strsplit(counts$Lineup[li], "\\|")[[1]]
    cm_cols <- intersect(c("Player","FDPosGroup","FDPos","game_rank"), names(meta))
    cm      <- meta[Player %in% players, ..cm_cols]
    slots   <- assign_nba_slots_fd(cm)
    if (!is.null(slots)) {
      slot_list[[li]] <- as.data.table(c(list(Lineup = counts$Lineup[li]), slots))
    }
  }
  
  slot_dt <- rbindlist(slot_list[!sapply(slot_list, is.null)])
  counts  <- merge(counts, slot_dt, by = "Lineup", all.x = TRUE)
  
  unique_lineups <- counts[!is.na(PG1), .(
    TotalSalary, Top1Count, AvgScore,
    Player1=PG1, Player2=PG2, Player3=SG1, Player4=SG2,
    Player5=SF1, Player6=SF2, Player7=PF1, Player8=PF2, Player9=C
  )]
  
  if (nrow(unique_lineups) > max_lineups) unique_lineups <- unique_lineups[1:max_lineups]
  
  elapsed <- as.numeric(difftime(Sys.time(), start_t, units="secs"))
  if (verbose) cat(sprintf("  \u2713 %s FD lineups | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","), elapsed))
  
  list(unique_lineups = unique_lineups, n_sims = n_sims, config = config, mode = "nba_fd")
}


# ============================================================================
# NBA SHOWDOWN OPTIMIZER  (identical logic to CBB SD)
# CPT × 1.5 + 5 FLEX | $50K | both teams required
# ============================================================================

find_optimal_lineups_nba_sd <- function(sim_results, metadata, config,
                                        verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: NBA Showdown lineups (per-sim greedy)...\n")
  setDT(sim_results); setDT(metadata)
  
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  cpt_mult    <- 1.5
  
  meta <- unique(metadata[
    !is.na(CPTSalary) & CPTSalary > 0 & !is.na(SDSalary) & SDSalary > 0,
    .(Player, Team, CPTSalary, SDSalary, GameKey)
  ], by = "Player")
  
  if (nrow(meta) == 0) stop("No SD-eligible players. Check CPTSalary/SDSalary.")
  game_teams <- unique(meta$Team)
  if (length(game_teams) < 2)
    warning("NBA SD: fewer than 2 teams — both-teams constraint won't apply.")
  
  opt_data <- merge(
    sim_results[, .(SimID, Player, DKScore)],
    meta[, .(Player, Team, CPTSalary, SDSalary)],
    by = "Player"
  )
  opt_data <- opt_data[!is.na(DKScore)]
  setkey(opt_data, SimID)
  
  sim_ids   <- unique(opt_data$SimID)
  n_sims    <- length(sim_ids)
  start_t   <- Sys.time()
  prog_freq <- max(1L, n_sims %/% 20L)
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid <- sim_ids[i]
    sd  <- opt_data[.(sid)]
    setorder(sd, -DKScore)
    
    best_score  <- -Inf
    best_lineup <- NULL
    
    for (ci in seq_len(nrow(sd))) {
      cpt_player <- sd$Player[ci]
      cpt_sal    <- sd$CPTSalary[ci]
      cpt_score  <- sd$DKScore[ci] * cpt_mult
      if (cpt_sal > salary_cap) next
      
      rem_cap    <- salary_cap - cpt_sal
      flex       <- sd[Player != cpt_player]
      setorder(flex, -DKScore)
      
      picked_f   <- character(5L)
      n_picked   <- 0L; sal_used <- 0; flex_score <- 0
      
      for (j in seq_len(nrow(flex))) {
        if (n_picked == 5L) break
        if (sal_used + flex$SDSalary[j] <= rem_cap) {
          n_picked           <- n_picked + 1L
          picked_f[n_picked] <- flex$Player[j]
          sal_used           <- sal_used + flex$SDSalary[j]
          flex_score         <- flex_score + flex$DKScore[j]
        }
      }
      
      if (n_picked == 5L) {
        all_players  <- c(cpt_player, picked_f[seq_len(5L)])
        lineup_teams <- sd$Team[match(all_players, sd$Player)]
        if (length(unique(lineup_teams)) < 2L) next
        
        total <- cpt_score + flex_score
        if (total > best_score) {
          best_score  <- total
          best_lineup <- list(
            Captain     = cpt_player,
            Flex        = sort(picked_f),
            TotalSalary = cpt_sal + sal_used,
            TotalScore  = total
          )
        }
      }
    }
    
    if (!is.null(best_lineup)) {
      lineup_list[[i]] <- data.table(
        Lineup      = paste(c(best_lineup$Captain, best_lineup$Flex), collapse="|"),
        TotalSalary = best_lineup$TotalSalary,
        TotalScore  = best_lineup$TotalScore,
        Captain     = best_lineup$Captain,
        Util1       = best_lineup$Flex[1],
        Util2       = best_lineup$Flex[2],
        Util3       = best_lineup$Flex[3],
        Util4       = best_lineup$Flex[4],
        Util5       = best_lineup$Flex[5]
      )
    }
    
    if (verbose && i %% prog_freq == 0L) {
      cat(sprintf("\r  Phase 1: %d%% | %.1fs",
                  round(i/n_sims*100),
                  as.numeric(difftime(Sys.time(), start_t, units="secs"))))
      flush.console()
    }
  }
  if (verbose) cat("\n")
  
  valid <- lineup_list[!sapply(lineup_list, is.null)]
  if (length(valid) == 0L) stop("No valid NBA SD lineups found")
  
  all_dt <- rbindlist(valid)
  counts <- all_dt[, .(
    Top1Count=.N, TotalSalary=TotalSalary[1], AvgScore=mean(TotalScore),
    Captain=Captain[1],
    Util1=Util1[1], Util2=Util2[1], Util3=Util3[1], Util4=Util4[1], Util5=Util5[1]
  ), by = Lineup]
  counts[, rand := runif(.N)]
  setorder(counts, -Top1Count, rand)
  counts[, rand := NULL]
  if (nrow(counts) > max_lineups) counts <- counts[1:max_lineups]
  
  unique_lineups <- counts[, .(
    TotalSalary, Top1Count, AvgScore,
    Captain, Util1, Util2, Util3, Util4, Util5
  )]
  
  elapsed <- as.numeric(difftime(Sys.time(), start_t, units="secs"))
  if (verbose) cat(sprintf("  \u2713 %s SD lineups | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","), elapsed))
  
  list(unique_lineups = unique_lineups, n_sims = n_sims, config = config, mode = "captain")
}


# ============================================================================
# LINEUP METRICS PLACEHOLDER
# ============================================================================

calculate_nba_lineup_metrics <- function(scored_lineups, sim_results, metadata) {
  scored_lineups
}