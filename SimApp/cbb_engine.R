# ============================================================================
# CBB SIMULATION ENGINE
# Golden Ticket Sims — College Basketball DFS
# ============================================================================
# Platforms:  DraftKings Classic | FanDuel | DK Showdown (captain = DK score)
#
# DK scoring:  PTS(1) + 3PM(0.5) + REB(1.25) + AST(1.5) + STL(2) + BLK(2)
#              - TO(0.5) + DD bonus(1.5) + TD bonus(3, replaces DD)
# FD scoring:  PTS(1) + 3PM(0.5) + REB(1.20) + AST(1.5) + STL(2) + BLK(2)
#              - TO(1.0)  [no bonus]
# SD scoring:  same as DK (DKScore reused)
#
# Column ownership — avoids merge collisions:
#   IDs tab   -> DKID, FDID, DKSalary, FDSalary, DKPos, FDPos  (identity/salary)
#   Team tab  -> DKOwn, FDOwn, RGProj, RGFDProj, Mins, p10-p90 (projections/pcts)
#   Games tab -> SimKey, GameKey, GameTime, GameRank             (game context)
# ============================================================================

library(data.table)
library(readxl)


# ============================================================================
# DATA LOADING
# ============================================================================

read_cbb_input <- function(file_path) {
  
  sheets <- excel_sheets(file_path)
  
  # IDs tab: identity + salary (authoritative source for DKID/FDID/salaries)
  ids <- as.data.table(read_excel(file_path, sheet = "IDs"))
  setnames(ids, trimws(names(ids)))
  setnames(ids, "Name", "Player")
  
  # Games tab: game context + team-to-game mapping
  games <- as.data.table(read_excel(file_path, sheet = "Games"))
  setnames(games, trimws(names(games)))
  games[, SimKey := gsub("@", "_vs_", GameKey)]   # "UMBC@HOW" -> "UMBC_vs_HOW"
  
  team_game_lu <- rbind(
    games[, .(Team = FavTeam, SimKey, GameKey, GameTime, GameRank, ShowdownFile)],
    games[, .(Team = DogTeam, SimKey, GameKey, GameTime, GameRank, ShowdownFile)]
  )
  
  # Sim_ sheets
  sim_sheet_names <- grep("^Sim_", sheets, value = TRUE)
  if (length(sim_sheet_names) == 0) stop("No Sim_ sheets found in input file.")
  sim_games <- setNames(
    lapply(sim_sheet_names, function(s) as.data.table(read_excel(file_path, sheet = s))),
    sub("^Sim_", "", sim_sheet_names)
  )
  
  # SD ID sheets (SD1_IDs, SD2_IDs, ...)
  sd_sheet_names <- grep("^SD\\d+_IDs$", sheets, value = TRUE)
  sd_ids <- if (length(sd_sheet_names) > 0) {
    setNames(
      lapply(sd_sheet_names, function(s) {
        dt <- as.data.table(read_excel(file_path, sheet = s))
        setnames(dt, trimws(names(dt)))
        dt
      }),
      sub("_IDs$", "", sd_sheet_names)
    )
  } else list()
  
  # Team percentile tabs: everything that isn't IDs, Games, SD*_IDs, or Sim_*
  fixed_sheets <- c("IDs", "Games", sd_sheet_names, sim_sheet_names)
  team_sheet_names <- setdiff(sheets, fixed_sheets)
  if (length(team_sheet_names) == 0) stop("No team percentile tabs found.")
  
  team_data <- setNames(
    lapply(team_sheet_names, function(s) {
      dt <- as.data.table(read_excel(file_path, sheet = s))
      setnames(dt, trimws(names(dt)))
      # Rename projection cols to engine names
      rename <- c(DKProj = "RGProj", FDProj = "RGFDProj")
      for (col in names(rename))
        if (col %in% names(dt)) setnames(dt, col, rename[[col]])
      # Drop salary cols — IDs tab is authoritative; keeping both causes .x/.y collision
      drop <- intersect(c("DKSal", "FDSal", "DKSalary", "FDSalary"), names(dt))
      if (length(drop)) dt[, (drop) := NULL]
      dt
    }),
    team_sheet_names
  )
  
  # Build master slate: IDs + game context per player
  slate <- merge(ids, team_game_lu, by = "Team", all.x = TRUE)
  slate[, PosGroup   := fcase(DKPos == "G/F", "G/F", DKPos == "G", "G", default = "F")]
  slate[, FDPosGroup := fcase(FDPos == "G/F", "G/F", FDPos == "G", "G", default = "F")]
  slate <- unique(slate, by = "Player")
  
  cat(sprintf("CBB Input: %d players | %d games | %d team tabs | %d sim sheets\n",
              nrow(slate), nrow(games), length(team_data), length(sim_games)))
  
  list(slate = slate, sim_games = sim_games, team_data = team_data,
       games = games, sd_ids = sd_ids)
}


# ============================================================================
# VECTORIZED PERCENTILE INTERPOLATION
# draws : n_players x n_sims uniform matrix
# p10..p90 : n_players vectors (% share breakpoints, 0-100 scale)
# Returns  : n_players x n_sims matrix of % shares
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
           ifelse(draws <= 0.25, P10 + (draws - 0.10) / 0.15 * (P25 - P10),
                  ifelse(draws <= 0.50, P25 + (draws - 0.25) / 0.25 * (P50 - P25),
                         ifelse(draws <= 0.75, P50 + (draws - 0.50) / 0.25 * (P75 - P50),
                                ifelse(draws <= 0.90, P75 + (draws - 0.75) / 0.15 * (P90 - P75),
                                       P90))))),
    0
  )
}


# ============================================================================
# SCORING — element-wise on vectors or matrices
# ============================================================================

dk_score_cbb <- function(pts, tpm, reb, ast, stl, blk, to) {
  base  <- pts + tpm * 0.5 + reb * 1.25 + ast * 1.5 + stl * 2.0 + blk * 2.0 - to * 0.5
  cats  <- (pts >= 10) + (reb >= 10) + (ast >= 10) + (blk >= 10) + (stl >= 10)
  base + ifelse(cats >= 3, 3.0, ifelse(cats >= 2, 1.5, 0.0))
}

fd_score_cbb <- function(pts, tpm, reb, ast, stl, blk, to) {
  pts + tpm * 0.5 + reb * 1.20 + ast * 1.5 + stl * 2.0 + blk * 2.0 - to * 1.0
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
  stat_names   <- c("pts", "tpm", "reb", "ast", "stl", "blk", "to")
  
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
  sim_col <- c(pts="score", tpm="3pm", reb="rebounds",
               ast="assists", stl="steals", blk="blocks", to="turnovers")
  
  # ── Build active player list ───────────────────────────────────────────────
  cb("Building player roster...", 0.03)
  
  # Pull only IDs-tab columns from slate — team tab supplies DKOwn/FDOwn/pcts
  slate_cols <- c("Player", "DKID", "FDID", "DKSalary", "FDSalary",
                  "PosGroup", "FDPosGroup", "GameKey", "SimKey",
                  "GameTime", "GameRank", "Team")
  
  player_list <- rbindlist(lapply(team_abbrevs, function(team) {
    tab <- team_data[[team]]
    sl  <- slate[Team == team, ..slate_cols]
    matched <- tab[Name %in% sl$Player]
    if (nrow(matched) == 0) return(NULL)
    merged <- merge(matched, sl, by.x = "Name", by.y = "Player", all.x = TRUE)
    if (!"RGProj"   %in% names(merged)) merged[, RGProj   := NA_real_]
    if (!"RGFDProj" %in% names(merged)) merged[, RGFDProj := NA_real_]
    if (!"Mins"     %in% names(merged)) merged[, Mins     := NA_real_]
    merged[, Team := team]
    merged
  }), fill = TRUE)
  
  if (nrow(player_list) == 0) stop("No players matched between IDs tab and team percentile tabs.")
  
  n_players    <- nrow(player_list)
  player_names <- player_list$Name
  player_teams <- player_list$Team
  
  cat(sprintf("  Active players: %d (of %d in IDs tab)\n", n_players, nrow(slate)))
  
  # ── Resolve sim sheets ─────────────────────────────────────────────────────
  game_keys <- unique(player_list$SimKey)
  game_sim_dt <- setNames(lapply(game_keys, function(gk) {
    if (gk %in% names(sim_games)) return(sim_games[[gk]])
    parts   <- strsplit(gk, "_vs_")[[1]]
    rev_key <- paste0(parts[2], "_vs_", parts[1])
    if (rev_key %in% names(sim_games)) return(sim_games[[rev_key]])
    stop(sprintf("No Sim_ sheet found for game: %s", gk))
  }), game_keys)
  
  # ── Sample game rows once per game (same-row constraint) ──────────────────
  # Both teams in a game share identical row indices, preserving the ~-0.85
  # opponent score correlation and the shared pace/environment of each game.
  # Sampling independently per team destroys that correlation entirely.
  cb("Sampling game rows...", 0.06)
  
  game_row_idx <- setNames(lapply(game_keys, function(gk) {
    dt <- game_sim_dt[[gk]]
    w  <- 1 / (as.numeric(dt$similarity) + 0.001); w <- w / sum(w)
    sample.int(nrow(dt), n_sims, replace = TRUE, prob = w)
  }), game_keys)
  
  # ── Per-team setup ─────────────────────────────────────────────────────────
  cb("Building team data...", 0.08)
  
  team_data_prepped <- setNames(lapply(team_abbrevs, function(team) {
    pidx    <- which(player_teams == team)
    n_team  <- length(pidx)
    gk      <- player_list[pidx[1], SimKey]
    dt      <- game_sim_dt[[gk]]
    row_idx <- game_row_idx[[gk]]   # shared with opposing team
    
    totals <- setNames(lapply(stat_names, function(s) {
      as.numeric(dt[[paste0(team, "_", sim_col[s])]])[row_idx]
    }), stat_names)
    
    pcts <- setNames(lapply(stat_names, function(s) {
      cols <- pct_cols[[s]]
      m    <- matrix(0.0, nrow = n_team, ncol = 5)
      for (j in seq_along(cols)) {
        col <- cols[j]
        if (col %in% names(player_list)) {
          v <- as.numeric(player_list[[col]][pidx])
          m[, j] <- ifelse(is.na(v), 0, v)
        }
      }
      m
    }), stat_names)
    
    list(pidx = pidx, n_team = n_team, totals = totals, pcts = pcts)
  }), team_abbrevs)
  
  # ── Pre-draw all percentiles ────────────────────────────────────────────────
  # Independent uniform draws per player per stat — no cross-stat capping.
  # Game-level correlation is handled by same-row sampling above.
  cb("Pre-drawing percentiles...", 0.10)
  
  team_draws <- setNames(lapply(team_abbrevs, function(team) {
    n_team <- team_data_prepped[[team]]$n_team
    setNames(
      lapply(stat_names, function(s) matrix(runif(n_team * n_sims), n_team, n_sims)),
      stat_names
    )
  }), team_abbrevs)
  
  # ── Allocate stats ─────────────────────────────────────────────────────────
  cb("Allocating stats...", 0.15)
  
  stat_mats <- setNames(
    lapply(stat_names, function(s) matrix(0L, n_players, n_sims)),
    stat_names
  )
  
  for (team in team_abbrevs) {
    td   <- team_data_prepped[[team]]
    pidx <- td$pidx
    for (s in stat_names) {
      pm  <- td$pcts[[s]]
      shr <- interp_shares(team_draws[[team]][[s]],
                           pm[,1], pm[,2], pm[,3], pm[,4], pm[,5])
      cs  <- colSums(shr); cs[cs == 0] <- 1
      raw <- sweep(sweep(shr, 2, cs, `/`), 2, td$totals[[s]], `*`)
      stat_mats[[s]][pidx, ] <- as.integer(round(raw))
    }
  }
  stat_mats[["tpm"]] <- pmin(stat_mats[["tpm"]], stat_mats[["pts"]] %/% 3L)
  
  # ── Score DK and FD from the same stat matrices ────────────────────────────
  cb("Scoring DK and FD...", 0.88)
  
  dk_mat <- dk_score_cbb(stat_mats[["pts"]], stat_mats[["tpm"]], stat_mats[["reb"]],
                         stat_mats[["ast"]], stat_mats[["stl"]], stat_mats[["blk"]],
                         stat_mats[["to"]])
  fd_mat <- fd_score_cbb(stat_mats[["pts"]], stat_mats[["tpm"]], stat_mats[["reb"]],
                         stat_mats[["ast"]], stat_mats[["stl"]], stat_mats[["blk"]],
                         stat_mats[["to"]])
  
  # ── Assemble long-format results ───────────────────────────────────────────
  cb("Assembling results...", 0.92)
  
  sim_results <- data.table(
    SimID   = rep(seq_len(n_sims), each  = n_players),
    Player  = rep(player_names,    times = n_sims),
    DKScore = as.vector(dk_mat),
    FDScore = as.vector(fd_mat)
  )
  for (s in stat_names)
    sim_results[[s]] <- as.integer(as.vector(stat_mats[[s]]))
  
  elapsed <- round((proc.time() - start_time)["elapsed"], 1)
  cat(sprintf("  Simulation core: %.1fs\n", elapsed))
  
  # ── Build metadata ─────────────────────────────────────────────────────────
  cb("Building metadata...", 0.96)
  
  keep_cols <- intersect(
    c("Name", "DKID", "FDID", "DKSalary", "FDSalary", "DKOwn", "FDOwn",
      "PosGroup", "FDPosGroup", "Team", "GameKey", "SimKey",
      "GameTime", "GameRank", "RGProj", "RGFDProj", "Mins"),
    names(player_list)
  )
  metadata <- unique(player_list[, ..keep_cols], by = "Name")
  setnames(metadata, "Name", "Player")
  
  # GameTimeSort: epoch seconds for UTIL slot ordering in optimizer
  metadata[, GameTimeSort := as.numeric(as.POSIXct(
    paste(Sys.Date(), GameTime), format = "%Y-%m-%d %I:%M %p", tz = "America/New_York"
  ))]
  
  # Attach SD captain IDs
  if (length(input_data$sd_ids) > 0) {
    game_sd_lu <- unique(rbind(
      input_data$games[, .(Team = FavTeam, ShowdownFile)],
      input_data$games[, .(Team = DogTeam, ShowdownFile)]
    ))
    metadata <- merge(metadata, game_sd_lu, by = "Team", all.x = TRUE)
    sd_all <- rbindlist(lapply(names(input_data$sd_ids), function(n) {
      dt <- copy(input_data$sd_ids[[n]]); dt[, SDFile := n]; dt
    }), fill = TRUE)
    setnames(sd_all, "Name", "Player")
    # UTIL_ID / UTIL_Salary are the flex (non-captain) SD salary and ID
    # CPT_ID / CPT_Salary are the captain-slot salary and ID
    sd_join_cols <- intersect(
      c("Player", "Team", "SDFile", "CPT_ID", "CPT_Salary", "UTIL_ID", "UTIL_Salary"),
      names(sd_all)
    )
    metadata <- merge(
      metadata,
      sd_all[, ..sd_join_cols][, .(
        Player, Team, SDFile,
        CPTID     = CPT_ID,
        CPTSalary = CPT_Salary,
        SDID      = if ("UTIL_ID"     %in% names(.SD)) UTIL_ID     else NA_integer_,
        SDSalary  = if ("UTIL_Salary" %in% names(.SD)) UTIL_Salary else NA_real_
      )],
      by.x = c("Player", "Team", "ShowdownFile"),
      by.y = c("Player", "Team", "SDFile"),
      all.x = TRUE
    )
  }
  
  sim_results <- sim_results[Player %in% metadata$Player]
  
  cat(sprintf("CBB sim complete: %d sims | %d players | %d rows\n",
              n_sims, nrow(metadata), nrow(sim_results)))
  
  list(sim_results = sim_results, metadata = metadata, has_fd = TRUE, sport_visuals = NULL)
}


# ============================================================================
# LINEUP METRICS PLACEHOLDER
# ============================================================================

calculate_cbb_lineup_metrics <- function(scored_lineups, sim_results, metadata) {
  scored_lineups
}


# ============================================================================
# ============================================================================
# SHARED SLOT ASSIGNMENT HELPER
# Assign players to positional slots in game_rank order (rank 1 = earliest game).
# Each player gets their natural position slot (G or F) if still open,
# otherwise goes to UTIL. Latest-game players naturally end up in UTIL.
#
# Args:
#   cm    : data.table with Player, PosGroup, game_rank (chosen players only)
#   n_g   : G slots required
#   n_f   : F slots required
#   n_util: UTIL slots
# Returns: list(g, f, util) or NULL if assignment impossible
# ============================================================================

assign_cbb_slots <- function(cm, n_g, n_f, n_util) {
  setorder(cm, game_rank)  # earliest game first; latest will overflow to UTIL
  g_players <- character(0)
  f_players <- character(0)
  u_players <- character(0)
  
  for (idx in seq_len(nrow(cm))) {
    p   <- cm$Player[idx]
    pos <- cm$PosGroup[idx]
    if ((pos == "G" || pos == "G/F") && length(g_players) < n_g) {
      g_players <- c(g_players, p)
    } else if ((pos == "F" || pos == "G/F") && length(f_players) < n_f) {
      f_players <- c(f_players, p)
    } else if (length(u_players) < n_util) {
      u_players <- c(u_players, p)
    } else {
      return(NULL)  # couldn't place this player
    }
  }
  
  if (length(g_players) != n_g || length(f_players) != n_f ||
      length(u_players) != n_util) return(NULL)
  
  all_assigned <- c(g_players, f_players, u_players)
  if (anyNA(all_assigned) || length(unique(all_assigned)) != nrow(cm)) return(NULL)
  
  list(g = g_players, f = f_players, util = u_players)
}

# CBB LINEUP OPTIMIZER
# Per-sim: filter top 25 G + top 25 F by PPD, LP with position constraints,
# assign UTIL slots to latest game-time players post-LP.
# ============================================================================

find_optimal_lineups_cbb <- function(sim_results, metadata, config, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: Finding optimal CBB lineups (per-sim LP)...\n")
  
  setDT(sim_results); setDT(metadata)
  
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  top_n       <- 25L
  
  meta <- unique(metadata[, .(Player, DKSalary, PosGroup, GameKey)], by = "Player")
  meta[, g_elig := PosGroup %in% c("G", "G/F")]
  meta[, f_elig := PosGroup %in% c("F", "G/F")]
  
  if ("GameTimeSort" %in% names(metadata)) {
    gt <- unique(metadata[, .(GameKey, GameTimeSort)])
    gt[is.na(GameTimeSort), GameTimeSort := 0]
    setorder(gt, -GameTimeSort)
    game_order <- setNames(seq_len(nrow(gt)), gt$GameKey)
  } else {
    game_order <- setNames(seq_along(unique(meta$GameKey)), unique(meta$GameKey))
  }
  meta[, game_rank := game_order[GameKey]]
  meta[is.na(game_rank), game_rank := 0L]
  
  opt_data <- merge(
    sim_results[, .(SimID, Player, FantasyPoints = DKScore)],
    meta[, .(Player, Salary = DKSalary, g_elig, f_elig, game_rank)],
    by = "Player"
  )
  opt_data <- opt_data[Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints)]
  opt_data[, ppd := FantasyPoints / Salary * 1000]
  setkey(opt_data, SimID)
  
  sim_ids   <- unique(opt_data$SimID)
  n_sims    <- length(sim_ids)
  start_t   <- Sys.time()
  prog_freq <- max(1L, n_sims %/% 20L)
  
  if (verbose) cat(sprintf("  %s players | %s sims | $%s cap | top %d per pos\n",
                           format(nrow(meta), big.mark=","),
                           format(n_sims, big.mark=","),
                           format(salary_cap, big.mark=","), top_n))
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid  <- sim_ids[i]
    sd   <- opt_data[.(sid)]
    pool <- unique(rbind(
      sd[g_elig == TRUE][order(-ppd)][seq_len(min(top_n, .N))],
      sd[f_elig == TRUE][order(-ppd)][seq_len(min(top_n, .N))]
    ), by = "Player")
    
    n_p <- nrow(pool)
    if (n_p < 8L) next
    
    res <- tryCatch(
      lp("max", pool$FantasyPoints,
         rbind(rep(1,n_p), pool$Salary, as.integer(pool$g_elig), as.integer(pool$f_elig)),
         c("==","<=",">=",">="), c(8L, salary_cap, 3L, 3L), all.bin = TRUE),
      error = function(e) list(status = 1L)
    )
    if (res$status != 0L) next
    selected <- which(res$solution == 1L)
    if (length(selected) != 8L) next
    
    chosen <- pool[selected]
    cm     <- meta[Player %in% chosen$Player, .(Player, PosGroup, game_rank)]
    slots  <- assign_cbb_slots(cm, n_g=3L, n_f=3L, n_util=2L)
    if (is.null(slots)) next
    
    lineup_list[[i]] <- data.table(
      Lineup = paste(sort(chosen$Player), collapse="|"),
      TotalSalary = sum(chosen$Salary), TotalScore = sum(chosen$FantasyPoints),
      G1=slots$g[1], G2=slots$g[2], G3=slots$g[3],
      F1=slots$f[1], F2=slots$f[2], F3=slots$f[3],
      UTIL1=slots$util[1], UTIL2=slots$util[2]
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
  if (length(valid) == 0L) stop("No valid CBB lineups found")
  
  all_dt <- rbindlist(valid)
  counts <- all_dt[, .(
    Top1Count=.N, TotalSalary=TotalSalary[1], AvgScore=mean(TotalScore),
    G1=G1[1], G2=G2[1], G3=G3[1], F1=F1[1], F2=F2[1], F3=F3[1],
    UTIL1=UTIL1[1], UTIL2=UTIL2[1]
  ), by=Lineup]
  setorder(counts, -Top1Count)
  if (nrow(counts) > max_lineups) counts <- counts[1:max_lineups]
  
  unique_lineups <- counts[, .(
    TotalSalary, Top1Count, AvgScore,
    Player1=G1, Player2=G2, Player3=G3,
    Player4=F1, Player5=F2, Player6=F3,
    Player7=UTIL1, Player8=UTIL2
  )]
  
  elapsed <- as.numeric(difftime(Sys.time(), start_t, units="secs"))
  if (verbose) cat(sprintf("  \u2713 Phase 1: %s unique lineups from %s sims | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","),
                           format(n_sims, big.mark=","), elapsed))
  
  list(unique_lineups=unique_lineups, n_sims=n_sims, config=config, mode="cbb")
}


# ============================================================================
# CBB FD LINEUP OPTIMIZER
# FD roster: 4G / 3F / 1UTIL = 8 players | $50K cap
# Constraints: >=4 G-elig, >=3 F-elig, 8 total
# Uses FDScore and FDSalary; UTIL assigned to latest-game player post-LP.
# ============================================================================

find_optimal_lineups_cbb_fd <- function(sim_results, metadata, config, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: Finding optimal CBB FD lineups (per-sim LP)...\n")
  
  setDT(sim_results); setDT(metadata)
  
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  top_n       <- 25L
  
  # Use FDPosGroup if available, fall back to PosGroup
  pos_col <- if ("FDPosGroup" %in% names(metadata)) "FDPosGroup" else "PosGroup"
  meta <- unique(metadata[, .(Player, FDSalary, PosGroup = get(pos_col), GameKey)], by = "Player")
  meta[, g_elig := PosGroup %in% c("G", "G/F")]
  meta[, f_elig := PosGroup %in% c("F", "G/F")]
  meta <- meta[FDSalary > 0 & !is.na(FDSalary)]
  
  if ("GameTimeSort" %in% names(metadata)) {
    gt <- unique(metadata[, .(GameKey, GameTimeSort)])
    gt[is.na(GameTimeSort), GameTimeSort := 0]
    setorder(gt, -GameTimeSort)
    game_order <- setNames(seq_len(nrow(gt)), gt$GameKey)
  } else {
    game_order <- setNames(seq_along(unique(meta$GameKey)), unique(meta$GameKey))
  }
  meta[, game_rank := game_order[GameKey]]
  meta[is.na(game_rank), game_rank := 0L]
  
  opt_data <- merge(
    sim_results[, .(SimID, Player, FantasyPoints = FDScore)],
    meta[, .(Player, Salary = FDSalary, g_elig, f_elig, game_rank)],
    by = "Player"
  )
  opt_data <- opt_data[Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints)]
  opt_data[, ppd := FantasyPoints / Salary * 1000]
  setkey(opt_data, SimID)
  
  sim_ids   <- unique(opt_data$SimID)
  n_sims    <- length(sim_ids)
  start_t   <- Sys.time()
  prog_freq <- max(1L, n_sims %/% 20L)
  
  if (verbose) cat(sprintf("  %s players | %s sims | $%s cap | top %d per pos\n",
                           format(nrow(meta), big.mark=","),
                           format(n_sims, big.mark=","),
                           format(salary_cap, big.mark=","), top_n))
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid  <- sim_ids[i]
    sd   <- opt_data[.(sid)]
    pool <- unique(rbind(
      sd[g_elig == TRUE][order(-ppd)][seq_len(min(top_n, .N))],
      sd[f_elig == TRUE][order(-ppd)][seq_len(min(top_n, .N))]
    ), by = "Player")
    
    n_p <- nrow(pool)
    if (n_p < 8L) next
    
    # FD constraints: 8 total, <=50K, >=4 G-elig, >=3 F-elig, 1 UTIL
    res <- tryCatch(
      lp("max", pool$FantasyPoints,
         rbind(rep(1,n_p), pool$Salary, as.integer(pool$g_elig), as.integer(pool$f_elig)),
         c("==","<=",">=",">="), c(8L, salary_cap, 4L, 3L), all.bin = TRUE),
      error = function(e) list(status = 1L)
    )
    if (res$status != 0L) next
    selected <- which(res$solution == 1L)
    if (length(selected) != 8L) next
    
    chosen <- pool[selected]
    cm     <- meta[Player %in% chosen$Player, .(Player, PosGroup, game_rank)]
    slots  <- assign_cbb_slots(cm, n_g=4L, n_f=3L, n_util=1L)
    if (is.null(slots)) next
    
    lineup_list[[i]] <- data.table(
      Lineup      = paste(sort(chosen$Player), collapse="|"),
      TotalSalary = sum(chosen$Salary),
      TotalScore  = sum(chosen$FantasyPoints),
      G1=slots$g[1], G2=slots$g[2], G3=slots$g[3], G4=slots$g[4],
      F1=slots$f[1], F2=slots$f[2], F3=slots$f[3],
      UTIL1=slots$util[1]
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
  if (length(valid) == 0L) stop("No valid CBB FD lineups found")
  
  all_dt <- rbindlist(valid)
  counts <- all_dt[, .(
    Top1Count=.N, TotalSalary=TotalSalary[1], AvgScore=mean(TotalScore),
    G1=G1[1], G2=G2[1], G3=G3[1], G4=G4[1],
    F1=F1[1], F2=F2[1], F3=F3[1],
    UTIL1=UTIL1[1]
  ), by=Lineup]
  setorder(counts, -Top1Count)
  if (nrow(counts) > max_lineups) counts <- counts[1:max_lineups]
  
  unique_lineups <- counts[, .(
    TotalSalary, Top1Count, AvgScore,
    Player1=G1, Player2=G2, Player3=G3, Player4=G4,
    Player5=F1, Player6=F2, Player7=F3,
    Player8=UTIL1
  )]
  
  elapsed <- as.numeric(difftime(Sys.time(), start_t, units="secs"))
  if (verbose) cat(sprintf("  \u2713 Phase 1: %s unique FD lineups from %s sims | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","),
                           format(n_sims, big.mark=","), elapsed))
  
  list(unique_lineups=unique_lineups, n_sims=n_sims, config=config, mode="cbb_fd")
}