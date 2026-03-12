# ============================================================================
# CBB SIMULATION ENGINE
# Golden Ticket Sims — College Basketball DFS (DraftKings Classic)
# ============================================================================
# Roster: 3G / 3F / 2UTIL(G or F) | $50K cap | 8 players | 2+ games required
# Scoring: PTS(1) + 3PM(0.5) + REB(1.25) + AST(1.5) + STL(2) + BLK(2) + TO(-0.5)
#          + DD bonus(1.5) + TD bonus(3, replaces DD)
#
# Architecture: vectorized across sims, looped over teams/stats.
#   Per team per stat:
#     - Draw n_players x n_sims uniform values -> interpolate to % shares
#     - Build n_sims exact team totals from pre-sampled game rows
#     - Allocate: sweep(shares/100, 2, totals, "*") -> round
# ============================================================================

library(data.table)
library(readxl)


# ============================================================================
# DATA LOADING
# ============================================================================

read_cbb_input <- function(file_path) {
  sheets <- excel_sheets(file_path)
  
  slate <- as.data.table(read_excel(file_path, sheet = "Slate"))
  setnames(slate, names(slate), trimws(names(slate)))
  
  rename_map <- c(
    "Name"            = "Player",
    "Roster Position" = "RosterPosition",
    "Roster.Position" = "RosterPosition",
    "Game Info"       = "GameInfo",
    "Game.Info"       = "GameInfo",
    "TeamAbbrev"      = "Team",
    "AvgPointsPerGame"= "AvgPPG",
    "ID"              = "DKID"
  )
  for (old in names(rename_map))
    if (old %in% names(slate)) setnames(slate, old, rename_map[[old]])
  
  slate[, PosGroup := fcase(
    grepl("G", RosterPosition) & grepl("F", RosterPosition), "G/F",
    grepl("G", RosterPosition), "G",
    default = "F"
  )]
  slate[, GameKey  := {
    raw   <- sub(" .*", "", GameInfo)
    parts <- strsplit(raw, "@")[[1]]
    paste0(parts[1], "_vs_", parts[2])
  }, by = seq_len(nrow(slate))]
  
  if (!"DKSalary" %in% names(slate) && "Salary" %in% names(slate))
    setnames(slate, "Salary", "DKSalary")
  if (!"DKOwn"  %in% names(slate)) slate[, DKOwn  := 0]
  if (!"RGProj" %in% names(slate)) slate[, RGProj := NA_real_]
  if (!"RGMin"  %in% names(slate)) slate[, RGMin  := NA_real_]
  # One row per player — DK lists each player once per eligible slot (G/UTIL, F/UTIL etc)
  # PosGroup already captures G/F/both; dedup keeps the row with the most specific position
  slate <- unique(slate, by = "Player")
  
  sim_sheets <- grep("^Sim_", sheets, value = TRUE)
  if (length(sim_sheets) == 0) stop("No Sim_ sheets found.")
  
  sim_games <- setNames(
    lapply(sim_sheets, function(s) as.data.table(read_excel(file_path, sheet = s))),
    sub("^Sim_", "", sim_sheets)
  )
  
  team_sheets <- sheets[!grepl("^Slate$|^Sim_", sheets)]
  team_data   <- setNames(
    lapply(team_sheets, function(s) as.data.table(read_excel(file_path, sheet = s))),
    team_sheets
  )
  
  cat(sprintf("CBB Input: %d slate players | %d games | %d team tabs\n",
              nrow(slate), length(sim_games), length(team_data)))
  
  list(slate = slate, sim_games = sim_games, team_data = team_data)
}


# ============================================================================
# VECTORIZED PERCENTILE INTERPOLATION
# draws: n_players x n_sims matrix
# p10..p90: n_players vectors (one value per player)
# Returns: n_players x n_sims matrix of % shares
# ============================================================================

interp_shares <- function(draws, p10, p25, p50, p75, p90) {
  p10[is.na(p10)] <- 0
  p25[is.na(p25)] <- p10[is.na(p25)]
  p50[is.na(p50)] <- p25[is.na(p50)]
  p75[is.na(p75)] <- p50[is.na(p75)]
  p90[is.na(p90)] <- p75[is.na(p90)]
  
  # Broadcast each player's breakpoints across all n_sims columns
  # draws is n_players x n_sims; p-vectors are length n_players
  # matrix(p10, nrow=n_players, ncol=n_sims) replicates column-wise
  n_p <- nrow(draws)
  n_s <- ncol(draws)
  P10 <- matrix(p10, n_p, n_s)
  P25 <- matrix(p25, n_p, n_s)
  P50 <- matrix(p50, n_p, n_s)
  P75 <- matrix(p75, n_p, n_s)
  P90 <- matrix(p90, n_p, n_s)
  
  out <- matrix(0.0, n_p, n_s)
  out <- ifelse(draws <= 0.10, P10,
                ifelse(draws <= 0.25, P10 + (draws-0.10)/0.15*(P25-P10),
                       ifelse(draws <= 0.50, P25 + (draws-0.25)/0.25*(P50-P25),
                              ifelse(draws <= 0.75, P50 + (draws-0.50)/0.25*(P75-P50),
                                     ifelse(draws <= 0.90, P75 + (draws-0.75)/0.15*(P90-P75),
                                            P90)))))
  pmax(out, 0)
}


# ============================================================================
# DK SCORING — works on vectors or matrices element-wise
# ============================================================================

dk_score_cbb <- function(pts, tpm, reb, ast, stl, blk, to) {
  base  <- pts + tpm*0.5 + reb*1.25 + ast*1.5 + stl*2.0 + blk*2.0 - to*0.5
  cats  <- (pts >= 10) + (reb >= 10) + (ast >= 10) + (blk >= 10) + (stl >= 10)
  bonus <- ifelse(cats >= 3, 3.0, ifelse(cats >= 2, 1.5, 0.0))
  base + bonus
}


# ============================================================================
# MAIN SIMULATION FUNCTION
# ============================================================================

run_cbb_simulation <- function(input_data, n_sims = 10000, config = NULL,
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
  stat_names   <- c("pts","tpm","reb","ast","stl","blk","to")
  
  for (ta in team_abbrevs)
    if (!ta %in% names(team_data))
      stop(sprintf("No team percentile tab found for: %s", ta))
  
  pct_cols <- list(
    pts = c("points_p10","points_p25","points_p50","points_p75","points_p90"),
    tpm = c("tpm_p10","tpm_p25","tpm_p50","tpm_p75","tpm_p90"),
    reb = c("reb_p10","reb_p25","reb_p50","reb_p75","reb_p90"),
    ast = c("ast_p10","ast_p25","ast_p50","ast_p75","ast_p90"),
    stl = c("stl_p10","stl_p25","stl_p50","stl_p75","stl_p90"),
    blk = c("blk_p10","blk_p25","blk_p50","blk_p75","blk_p90"),
    to  = c("to_p10","to_p25","to_p50","to_p75","to_p90")
  )
  sim_col       <- c(pts="score", tpm="3pm", reb="rebounds",
                     ast="assists", stl="steals", blk="blocks", to="turnovers")
  stat_defaults <- c(pts=72, tpm=8, reb=35, ast=15, stl=6, blk=4, to=13)
  
  # ============================================================================
  # BUILD ACTIVE PLAYER LIST
  # ============================================================================
  cb("Building player roster...", 0.03)
  
  player_list <- rbindlist(lapply(team_abbrevs, function(team) {
    tab     <- team_data[[team]]
    sl      <- slate[Team == team]
    matched <- tab[tab$Name %in% sl$Player]
    if (nrow(matched) == 0) return(NULL)
    merged  <- merge(matched,
                     sl[, .(Player, DKID, DKSalary, DKOwn, PosGroup, GameKey,
                            SlateRGProj = RGProj, SlateRGMin = RGMin)],
                     by.x = "Name", by.y = "Player", all.x = TRUE)
    # Team-tab RGProj/RGMin preferred; slate values fill any gaps
    if ("RGProj" %in% names(merged)) {
      merged[is.na(RGProj) & !is.na(SlateRGProj), RGProj := SlateRGProj]
    } else if ("SlateRGProj" %in% names(merged)) {
      setnames(merged, "SlateRGProj", "RGProj")
    }
    if ("RGMin" %in% names(merged)) {
      merged[is.na(RGMin) & !is.na(SlateRGMin), RGMin := SlateRGMin]
    } else if ("SlateRGMin" %in% names(merged)) {
      setnames(merged, "SlateRGMin", "RGMin")
    }
    if ("SlateRGProj" %in% names(merged)) merged[, SlateRGProj := NULL]
    if ("SlateRGMin"  %in% names(merged)) merged[, SlateRGMin  := NULL]
    merged[, Team := team]
    merged
  }), fill = TRUE)
  
  if (nrow(player_list) == 0) stop("No players matched between slate and percentile tabs.")
  
  n_players    <- nrow(player_list)
  player_names <- player_list$Name
  player_teams <- player_list$Team
  
  cat(sprintf("  Active players: %d (of %d on slate)\n", n_players, nrow(slate)))
  
  # ============================================================================
  # RESOLVE SIM SHEETS
  # ============================================================================
  
  game_keys   <- unique(slate$GameKey)
  game_sim_dt <- setNames(lapply(game_keys, function(gk) {
    if (gk %in% names(sim_games)) return(sim_games[[gk]])
    parts   <- strsplit(gk, "_vs_")[[1]]
    rev_key <- paste0(parts[2], "_vs_", parts[1])
    if (rev_key %in% names(sim_games)) return(sim_games[[rev_key]])
    stop(sprintf("No Sim_ sheet for game: %s", gk))
  }), game_keys)
  
  # ============================================================================
  # PER-TEAM SETUP: game key, player indices, percentile vectors, game totals
  # ============================================================================
  cb("Building team data...", 0.06)
  
  team_data_prepped <- setNames(lapply(team_abbrevs, function(team) {
    
    # Player indices in player_list for this team
    pidx   <- which(player_teams == team)
    n_team <- length(pidx)
    
    # Game sheet for this team
    gk  <- player_list[pidx[1], GameKey]
    dt  <- game_sim_dt[[gk]]
    w   <- 1 / (as.numeric(dt$similarity) + 0.001)
    w   <- w / sum(w)
    
    # Pre-sample all n_sims game row indices for this team's game
    row_idx <- sample.int(nrow(dt), n_sims, replace = TRUE, prob = w)
    
    # Pre-extract exact team totals for all stats, all sims: named list of n_sims vectors
    totals <- setNames(lapply(stat_names, function(s) {
      col <- paste0(team, "_", sim_col[s])
      if (col %in% names(dt)) {
        as.numeric(dt[[col]])[row_idx]   # exact value from sampled game row
      } else {
        rep(stat_defaults[s], n_sims)
      }
    }), stat_names)
    
    # Pre-extract percentile breakpoints for each player, each stat
    # pcts[[stat]] = n_team x 5 matrix (p10,p25,p50,p75,p90)
    pcts <- setNames(lapply(stat_names, function(s) {
      cols <- pct_cols[[s]]
      m    <- matrix(0.0, nrow = n_team, ncol = 5)
      for (j in seq_along(cols)) {
        col <- cols[j]
        if (col %in% names(player_list)) {
          v      <- as.numeric(player_list[[col]][pidx])
          m[, j] <- ifelse(is.na(v), 0, v)
        }
      }
      m
    }), stat_names)
    
    list(pidx = pidx, n_team = n_team, totals = totals, pcts = pcts)
  }), team_abbrevs)
  
  # ============================================================================
  # DRAW ALL RANDOM PERCENTILES UPFRONT
  # One n_team x n_sims matrix per team per stat — stored per team
  # pts and ast anti-correlated within each team
  # ============================================================================
  cb("Pre-drawing percentiles...", 0.10)
  
  team_draws <- setNames(lapply(team_abbrevs, function(team) {
    n_team <- team_data_prepped[[team]]$n_team
    draws  <- setNames(lapply(stat_names, function(s) {
      matrix(runif(n_team * n_sims), nrow = n_team, ncol = n_sims)
    }), stat_names)
    
    # Pts <-> ast anti-correlation
    d_pts <- draws[["pts"]]; d_ast <- draws[["ast"]]
    d_ast <- ifelse(d_pts >= 0.90, pmin(d_ast, 0.25),
                    ifelse(d_pts >= 0.75, pmin(d_ast, 0.50), d_ast))
    d_pts <- ifelse(d_ast >= 0.90, pmin(d_pts, 0.25),
                    ifelse(d_ast >= 0.75, pmin(d_pts, 0.50), d_pts))
    draws[["pts"]] <- d_pts; draws[["ast"]] <- d_ast
    draws
  }), team_abbrevs)
  
  # ============================================================================
  # ALLOCATE STATS — vectorized across all n_sims per team per stat
  #
  # For each team and stat:
  #   draws[[stat]]: n_team x n_sims uniform draws
  #   pcts[[stat]]:  n_team x 5 breakpoints (% share, 0-100 scale)
  #   totals[[stat]]: n_sims exact team totals from sampled game rows
  #
  #   shares = interp_shares(draws, p-breakpoints): n_team x n_sims % shares
  #   raw    = sweep(shares, 2, totals/100, "*"):   n_team x n_sims raw counts
  #   Scale down cols where colSums(raw) > totals (shares sampled > 100%)
  #   round() -> integer allocation
  # ============================================================================
  cb("Allocating stats...", 0.15)
  
  # Final stat matrices: n_players x n_sims
  stat_mats <- setNames(
    lapply(stat_names, function(s) matrix(0L, nrow = n_players, ncol = n_sims)),
    stat_names
  )
  
  for (team in team_abbrevs) {
    td     <- team_data_prepped[[team]]
    pidx   <- td$pidx
    
    for (s in stat_names) {
      pm      <- td$pcts[[s]]          # n_team x 5 breakpoints
      draws_s <- team_draws[[team]][[s]]  # n_team x n_sims uniform draws
      totals  <- td$totals[[s]]           # n_sims exact team totals
      
      # Interpolate draws to % shares: n_team x n_sims
      shares <- interp_shares(draws_s, pm[,1], pm[,2], pm[,3], pm[,4], pm[,5])
      
      # Normalize shares so rostered players always account for 100% of stats.
      cs          <- colSums(shares)
      cs[cs == 0] <- 1
      norm        <- sweep(shares, 2, cs, `/`)
      raw         <- sweep(norm, 2, totals, `*`)
      stat_mats[[s]][pidx, ] <- as.integer(round(raw))
    }
  }
  
  # 3PM cap
  stat_mats[["tpm"]] <- pmin(stat_mats[["tpm"]], stat_mats[["pts"]] %/% 3L)
  
  # ============================================================================
  # SCORE
  # ============================================================================
  cb("Scoring...", 0.88)
  
  score_mat <- dk_score_cbb(
    pts = stat_mats[["pts"]], tpm = stat_mats[["tpm"]],
    reb = stat_mats[["reb"]], ast = stat_mats[["ast"]],
    stl = stat_mats[["stl"]], blk = stat_mats[["blk"]],
    to  = stat_mats[["to"]]
  )
  
  # ============================================================================
  # ASSEMBLE RESULTS
  # stat_mats / score_mat: n_players x n_sims
  # Long format: for player i all n_sims results are rows (i-1)*n_sims+1 .. i*n_sims
  # as.vector() on n_players x n_sims reads col-major = player-major ✓
  # SimID and Player constructed to match
  # ============================================================================
  cb("Assembling results...", 0.92)
  
  # stat_mats / score_mat: n_players x n_sims
  # as.vector() reads col-major: sim1_all_players, sim2_all_players...
  # SimID = rep(1:n_sims, each=n_players) matches exactly
  # Player = rep(names, times=n_sims): all players repeated per sim
  sim_results <- data.table(
    SimID   = rep(seq_len(n_sims), each  = n_players),
    Player  = rep(player_names,    times = n_sims),
    DKScore = as.vector(score_mat)
  )
  for (s in stat_names)
    sim_results[[s]] <- as.integer(as.vector(stat_mats[[s]]))
  
  elapsed <- round((proc.time() - start_time)["elapsed"], 1)
  cat(sprintf("  Simulation core: %.1fs\n", elapsed))
  
  # ============================================================================
  # METADATA
  # ============================================================================
  cb("Building metadata...", 0.96)
  
  keep_cols <- intersect(
    c("Name","DKID","DKSalary","DKOwn","Team","PosGroup","RosterPosition","GameKey","RGProj","RGMin"),
    names(player_list)
  )
  metadata <- unique(player_list[, ..keep_cols], by = "Name")
  setnames(metadata, "Name", "Player")
  sim_results <- sim_results[Player %in% metadata$Player]
  
  cat(sprintf("CBB sim complete: %d sims | %d players | %d rows\n",
              n_sims, nrow(metadata), nrow(sim_results)))
  
  list(sim_results = sim_results, metadata = metadata, has_fd = FALSE, sport_visuals = NULL)
}


# ============================================================================
# LINEUP METRICS PLACEHOLDER
# ============================================================================

calculate_cbb_lineup_metrics <- function(scored_lineups, sim_results, metadata) {
  scored_lineups
}


# ============================================================================
# CBB LINEUP OPTIMIZER
# Per-sim: filter top 25 G + top 25 F by PPD, run LP with position constraints,
# assign UTIL slots to latest game-time players post-LP.
# Output matches standard mode format for scoring/metrics pipeline.
# ============================================================================

find_optimal_lineups_cbb <- function(sim_results, metadata, config, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: Finding optimal CBB lineups (per-sim LP)...\n")
  
  setDT(sim_results); setDT(metadata)
  
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  top_n       <- 25L   # per position group per sim
  
  # ── Player eligibility & game time ────────────────────────────────────────
  # PosGroup: "G", "F", "G/F"
  meta <- unique(metadata[, .(Player, DKSalary, PosGroup, GameKey)], by = "Player")
  meta[, g_elig := PosGroup %in% c("G", "G/F")]
  meta[, f_elig := PosGroup %in% c("F", "G/F")]
  
  # Game time rank: parse from GameKey or use order of appearance
  # GameKey format "TEX_vs_ARK" — use slate GameInfo if available, else fallback
  # Use slate game order as proxy (later index = later game)
  game_order <- setNames(seq_along(unique(meta$GameKey)), unique(meta$GameKey))
  meta[, game_rank := game_order[GameKey]]
  
  # ── Merge salary into sim_results ─────────────────────────────────────────
  opt_data <- merge(
    sim_results[, .(SimID, Player, FantasyPoints = DKScore)],
    meta[, .(Player, Salary = DKSalary, g_elig, f_elig, game_rank)],
    by = "Player"
  )
  opt_data <- opt_data[Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints)]
  opt_data[, ppd := FantasyPoints / Salary * 1000]
  
  setkey(opt_data, SimID)
  sim_ids  <- unique(opt_data$SimID)
  n_sims   <- length(sim_ids)
  start_t  <- Sys.time()
  prog_freq <- max(1L, n_sims %/% 20L)
  
  if (verbose) cat(sprintf("  %s players | %s sims | $%s cap | top %d per pos\n",
                           format(nrow(meta), big.mark=","), format(n_sims, big.mark=","),
                           format(salary_cap, big.mark=","), top_n))
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid <- sim_ids[i]
    sd  <- opt_data[.(sid)]
    
    # ── Per-sim PPD filter ─────────────────────────────────────────────────
    g_pool <- sd[g_elig == TRUE][order(-ppd)][seq_len(min(top_n, .N))]
    f_pool <- sd[f_elig == TRUE][order(-ppd)][seq_len(min(top_n, .N))]
    pool   <- unique(rbind(g_pool, f_pool), by = "Player")
    
    n_p <- nrow(pool)
    if (n_p < 8L) next
    
    # ── LP constraint matrix ───────────────────────────────────────────────
    # Variables: one binary per player in pool
    # Constraints:
    #   1. sum == 8           (roster size)
    #   2. sum(salary) <= cap
    #   3. sum(G-elig) >= 3   (at least 3 G-eligible; UTIL absorbs extras)
    #   4. sum(F-elig) >= 3   (at least 3 F-eligible; UTIL absorbs extras)
    # G/F players contribute to BOTH G and F counts — using >= avoids
    # infeasibility from double-counting. With 8 total and >=3G + >=3F,
    # the remaining slots are naturally UTIL.
    
    const_mat <- rbind(
      rep(1,    n_p),                          # total players
      pool$Salary,                             # salary
      as.integer(pool$g_elig),                 # G-eligible count
      as.integer(pool$f_elig)                  # F-eligible count
    )
    const_dir <- c("==", "<=", ">=", ">=")
    const_rhs <- c(8L, salary_cap, 3L, 3L)
    
    res <- tryCatch(
      lp("max", pool$FantasyPoints, const_mat, const_dir, const_rhs, all.bin = TRUE),
      error = function(e) list(status = 1L)
    )
    
    if (res$status != 0L) next
    selected <- which(res$solution == 1L)
    if (length(selected) != 8L) next
    
    chosen <- pool[selected]
    
    # ── Assign positions post-LP ───────────────────────────────────────────
    # Strategy: fill G and F slots first, then assign latest-game players to UTIL
    # This avoids UTIL "stealing" the only player covering a position side.
    #
    # 1. Pure-G players fill G slots first
    # 2. Pure-F players fill F slots first
    # 3. G/F players fill whichever side still needs players
    # 4. Remaining 2 players → UTIL (prefer latest game time)
    
    chosen_meta <- meta[Player %in% chosen$Player, .(Player, PosGroup, game_rank)]
    pure_g  <- chosen_meta[PosGroup == "G"]$Player
    pure_f  <- chosen_meta[PosGroup == "F"]$Player
    dual_gf <- chosen_meta[PosGroup == "G/F"]$Player
    
    # Fill G: pure G first, then G/F
    g_players <- c(pure_g, dual_gf)[seq_len(3L)]
    
    # Fill F: pure F first, then unused G/F
    used_gf <- intersect(g_players, dual_gf)
    f_players <- c(pure_f, setdiff(dual_gf, used_gf))[seq_len(3L)]
    
    # UTIL: whoever is left, sorted by game_rank desc (latest game first)
    used <- c(g_players, f_players)
    util_bench <- chosen_meta[!Player %in% used][order(-game_rank)]
    util_players <- util_bench$Player[seq_len(2L)]
    
    # Final check — skip lineup if position assignment still invalid
    all_assigned <- c(g_players, f_players, util_players)
    if (length(all_assigned) != 8L || anyNA(all_assigned) ||
        length(unique(all_assigned)) != 8L) next
    
    # ── Canonical lineup signature (sorted player set for dedup) ──────────
    sig <- paste(sort(chosen$Player), collapse = "|")
    
    lineup_list[[i]] <- data.table(
      Lineup      = sig,
      TotalSalary = sum(chosen$Salary),
      TotalScore  = sum(chosen$FantasyPoints),
      G1 = g_players[1],    G2 = g_players[2],    G3 = g_players[3],
      F1 = f_players[1],    F2 = f_players[2],    F3 = f_players[3],
      UTIL1 = util_players[1], UTIL2 = util_players[2]
    )
    
    if (verbose && i %% prog_freq == 0L) {
      elapsed <- as.numeric(difftime(Sys.time(), start_t, units = "secs"))
      cat(sprintf("\r  Phase 1: %d%% | %.1fs", round(i / n_sims * 100), elapsed))
      flush.console()
    }
  }
  if (verbose) cat("\n")
  
  valid <- lineup_list[!sapply(lineup_list, is.null)]
  if (length(valid) == 0L) stop("No valid CBB lineups found")
  
  all_dt <- rbindlist(valid)
  
  # ── Dedup by player set, rank by frequency ────────────────────────────────
  counts <- all_dt[, .(
    Top1Count   = .N,
    TotalSalary = TotalSalary[1],
    AvgScore    = mean(TotalScore),
    G1 = G1[1], G2 = G2[1], G3 = G3[1],
    F1 = F1[1], F2 = F2[1], F3 = F3[1],
    UTIL1 = UTIL1[1], UTIL2 = UTIL2[1]
  ), by = Lineup]
  
  setorder(counts, -Top1Count)
  if (nrow(counts) > max_lineups) counts <- counts[1:max_lineups]
  
  # Build unique_lineups in standard Player1..Player8 format
  # Order: G1 G2 G3 F1 F2 F3 UTIL1 UTIL2
  unique_lineups <- counts[, .(
    TotalSalary, Top1Count, AvgScore,
    Player1 = G1, Player2 = G2, Player3 = G3,
    Player4 = F1, Player5 = F2, Player6 = F3,
    Player7 = UTIL1, Player8 = UTIL2
  )]
  
  elapsed <- as.numeric(difftime(Sys.time(), start_t, units = "secs"))
  if (verbose) cat(sprintf("  \u2713 Phase 1: %s unique lineups from %s sims | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","),
                           format(n_sims, big.mark=","), elapsed))
  
  list(
    unique_lineups = unique_lineups,
    n_sims         = n_sims,
    config         = config,
    mode           = "cbb"
  )
}