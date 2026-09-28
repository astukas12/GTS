# =============================================================================
# nhl_engine.R -- NHL (DK classic + showdown) for SimApp
# -----------------------------------------------------------------------------
# The input is the workbook GTS/NHL/R/live/build_slate.R writes: one per night,
# every game on it. The frame is computed there -- roles, TOI shares, rates, the
# market and each game's solved regulation grid -- so this file only simulates:
#
#   read_nhl_input(path)          Games / Players / Goalies / IDs_<dg> / Meta
#   run_nhl_simulation(input)     sim_team_box() -> sim_players(), DK points
#
# The model is GTS/NHL's own code, copied verbatim into nhl/ (team_model.R,
# player_model.R, fit/grid_core.R, goalie_core.R, player_core.R, dk_scoring.R;
# add_dk_shutout() from update_db.R). It is sourced into NHL_ENV rather than the
# global environment: it defines short helper names (lin, rcat, cmp, H, STATES,
# FLIP ...) that must not collide with any other engine's. nhl/nhl_params.rds
# is team_params$all + p3_params("all"), trimmed to what the sim reads.
#
# Goalies: the named starter only, scored off the team box's starter line
# (s_dk: W/OTL, saves, GA, DK shutout, 35+ saves). Backups are not offered.
#
# Metadata per player: DKID / DKUID (classic position and UTIL draftables --
# DK gives each skater a separate ID for UTIL), DKSalary, Pos (C / W / D / G),
# and for his game's showdown SDID / SDCID (FLEX / CPT) with SDSalary /
# CPTSalary. The classic columns are the MAIN classic (the biggest one); every
# classic on the sheet is kept in input_data$classic_dgs for the picker.
# =============================================================================

NHL_ENV <- new.env(parent = globalenv())
local({
  for (f in c("dk_scoring.R", "dk_shutout.R", "grid_core.R", "goalie_core.R",
              "player_core.R", "team_model.R", "player_model.R"))
    sys.source(file.path("nhl", f), envir = NHL_ENV)
  prm <- readRDS(file.path("nhl", "nhl_params.rds"))
  assign("NHL_PF", prm$team, envir = NHL_ENV)
  assign("NHL_P3", prm$p3,   envir = NHL_ENV)
})

read_nhl_input <- function(file_path) {
  sheets <- readxl::excel_sheets(file_path)
  rd <- function(s) data.table::as.data.table(suppressMessages(readxl::read_excel(file_path, sheet = s)))
  for (s in c("Games", "Players", "Goalies"))
    if (!s %in% sheets) stop("NHL workbook is missing the '", s, "' sheet")
  games <- rd("Games"); players <- rd("Players"); goalies <- rd("Goalies")
  id_sheets <- grep("^IDs_[0-9]+$", sheets, value = TRUE)
  ids <- setNames(lapply(id_sheets, rd), sub("^IDs_", "", id_sheets))

  # Excel hands every number back as double. The model's fcoalesce(slot, 4L)
  # needs the role columns integer, exactly as build_slate.R had them.
  for (j in intersect(c("gameId", "teamId", "playerId", "slot", "pp", "pk"), names(players)))
    set(players, j = j, value = as.integer(players[[j]]))
  for (j in intersect(c("gameId", "teamId", "playerId"), names(goalies)))
    set(goalies, j = j, value = as.integer(goalies[[j]]))
  for (j in c("gameId", "away_id", "home_id", "season"))
    if (j %in% names(games)) set(games, j = j, value = as.integer(games[[j]]))
  players[, is_home := as.logical(is_home)]; goalies[, `:=`(is_home = as.logical(is_home), starter = as.logical(starter))]

  gtype <- vapply(ids, function(d) if ("gameType" %in% names(d)) d$gameType[1] else NA_character_, "")
  cls <- names(ids)[gtype %in% "Classic"]
  cls <- cls[order(-vapply(ids[cls], nrow, 1L))]          # the main classic first
  games[, GameKey := paste(away, "@", home)]
  games[, ShowdownFile := fifelse(is.na(showdown_dg), NA_character_, as.character(showdown_dg))]

  list(games = games, players = players, goalies = goalies, ids = ids,
       classic_dgs = cls, sd_dgs = names(ids)[gtype %in% "Showdown"],
       meta = if ("Meta" %in% sheets) rd("Meta") else NULL,
       checks = if ("Checks" %in% sheets) rd("Checks") else NULL)
}

# One row per offered player: skaters + starting goalies, keyed by a unique
# Player name (two Elias Petterssons dress for VAN).
nhl_player_table <- function(input_data, classic_dg = input_data$classic_dgs[1]) {
  P <- copy(input_data$players); GL <- copy(input_data$goalies[starter == TRUE])
  G <- input_data$games
  pos_of <- function(p) fcase(p %chin% c("LW", "RW"), "W", default = p)
  sk <- P[, .(gameId, team, opp, is_home, playerId, name, grp, DKPos = dk_pos, slot, pp)]
  gl <- GL[, .(gameId, team, opp = NA_character_, is_home, playerId, name, grp = "G", DKPos = "G",
               slot = NA_integer_, pp = NA_integer_)]
  gl[G, on = "gameId", opp := fifelse(is_home, i.away, i.home)]
  T <- rbind(sk, gl)
  T[, Pos := pos_of(DKPos)]
  T[, Player := name]
  dup <- T[, .N, by = Player][N > 1, Player]
  T[Player %chin% dup, Player := sprintf("%s (%s %s)", name, team, Pos)]
  T[G, on = "gameId", `:=`(GameKey = i.GameKey, ShowdownFile = i.ShowdownFile, StartUTC = i.start_utc)]

  # IDs: skaters from Players, goalies from Goalies (same id_/uid_/cid_ columns)
  idcols <- grep("^(id|uid|cid)_[0-9]+$", union(names(P), names(GL)), value = TRUE)
  src <- rbind(P[, c("playerId", intersect(idcols, names(P))), with = FALSE],
               GL[, c("playerId", intersect(idcols, names(GL))), with = FALSE], fill = TRUE)
  getid <- function(col) if (col %in% names(src)) as.character(src[[col]][match(T$playerId, src$playerId)]) else rep(NA_character_, nrow(T))
  sal_of <- function(dg, id) { d <- input_data$ids[[dg]]; if (is.null(d)) return(rep(NA_real_, length(id)))
    as.numeric(d$Salary[match(id, as.character(d$ID))]) }

  if (length(classic_dg) && !is.na(classic_dg)) {
    T[, DKID := getid(paste0("id_", classic_dg))]
    T[, DKUID := getid(paste0("uid_", classic_dg))]
    T[, DKSalary := sal_of(classic_dg, DKID)]
  } else T[, `:=`(DKID = NA_character_, DKUID = NA_character_, DKSalary = NA_real_)]
  # a player's showdown is his own game's
  T[, `:=`(SDID = NA_character_, SDCID = NA_character_, SDSalary = NA_real_, CPTSalary = NA_real_)]
  for (dg in unique(na.omit(T$ShowdownFile))) {
    r <- which(T$ShowdownFile %chin% dg)
    fl <- getid(paste0("id_", dg))[r]; cp <- getid(paste0("cid_", dg))[r]
    T[r, `:=`(SDID = fl, SDCID = cp, SDSalary = sal_of(dg, fl), CPTSalary = sal_of(dg, cp))]
  }
  T[, `:=`(PosGroup = Pos, DKOwn = NA_real_)]
  T[]
}

# Point the metadata's classic columns at another classic on the sheet (the
# late classic). Players not in it lose their DKID and drop out of the DK pool.
nhl_metadata_for_classic <- function(meta, input_data, classic_dg) {
  m <- copy(meta)
  t <- nhl_player_table(input_data, classic_dg)
  m[t, on = "Player", `:=`(DKID = i.DKID, DKUID = i.DKUID, DKSalary = i.DKSalary)]
  m[]
}

run_nhl_simulation <- function(input_data, n_sims = 10000, config = NULL, progress_callback = NULL,
                               seed = NULL) {
  pcb <- function(d, v) if (is.function(progress_callback)) progress_callback(d, v)
  G <- copy(input_data$games); RF <- copy(input_data$players)
  G[, game := .I]
  GR <- G[, .(lam_h = grid_lam_h, lam_a = grid_lam_a, lm0 = grid_lm0, q = grid_q, live = grid_live, maxres = grid_maxres)]
  if (anyNA(GR[, .(lam_h, lam_a, lm0, q)])) stop("NHL: a game is missing its solved grid (grid_* columns in Games)")
  RF[, game := match(gameId, G$gameId)]
  if (anyNA(RF$game)) stop("NHL: Players rows whose gameId is not on the Games sheet")
  if (!is.null(seed)) set.seed(seed)

  pcb(sprintf("Team box scores: %s sims x %d games", format(n_sims, big.mark = ","), nrow(G)), 0.1)
  bx <- NHL_ENV$sim_team_box(G, NHL_ENV$NHL_PF, n_sims, grid = GR)
  bx[, `:=`(S = as.integer(S), blocks = as.integer(blocks))]
  pcb("Skater lines", 0.45)
  sk <- NHL_ENV$sim_players(bx, RF, NHL_ENV$NHL_P3)

  pcb("DK points", 0.85)
  T <- nhl_player_table(input_data)
  sk[, gameId := G$gameId[game]]
  sk_out <- sk[, .(SimID = sim, playerId, gameId, DKScore = dk)]
  gl <- T[Pos == "G", .(playerId, gameId, is_home)]
  gb <- bx[, .(SimID = sim, gameId = G$gameId[game], is_home, DKScore = s_dk)]
  gl_out <- merge(gl, gb, by = c("gameId", "is_home"), allow.cartesian = TRUE)[, .(SimID, playerId, gameId, DKScore)]
  sims <- rbind(sk_out, gl_out)
  sims[T, on = .(playerId, gameId), Player := i.Player]
  sims <- sims[!is.na(Player), .(SimID, Player, DKScore)]
  setorder(sims, SimID, Player)

  meta <- T[, .(Player, Team = team, Opp = opp, Pos, PosGroup, DKPos, Line = slot, PP = pp,
                DKID, DKUID, DKSalary, DKOwn, SDID, SDCID, SDSalary, CPTSalary,
                GameKey, ShowdownFile, playerId)]
  pr <- sims[, .(DKProj = mean(DKScore)), by = Player]
  meta[pr, on = "Player", DKProj := round(i.DKProj, 2)]
  meta[T, on = "Player", StartOrder := frank(i.StartUTC, ties.method = "dense")]   # late swap: the UTIL goes to the latest start
  pcb("Done", 1)
  list(sim_results = sims, metadata = meta)
}


# =============================================================================
# DK CLASSIC: C C W W W D D G UTIL, $50k
# -----------------------------------------------------------------------------
# find_optimal_lineups_nfl_classic's method with NHL's slot bounds: per sim the
# unconstrained best (top 2 C, 3 W, 2 D, 1 G, then EACH of C3 / W4 / D3 as its
# own UTIL variant); every sim whose best breaks the cap is solved exactly by
# .classic_exact_chunk (OptimalLineups_Core.R) with C 2-3 / W 3-4 / D 2-3 / G 1,
# need 9 -- exactly the condition that the 9 can be dealt into C C W W W D D G
# UTIL. The extra skater beyond the base 2/3/2 is the UTIL, the latest-starting
# one of his position for late swap. LW and RW are both W on DK.
#
# sim_results needs SimID, Player, FantasyPoints, Salary, Pos, StartOrder.
# =============================================================================
.NHL_CLASSIC_POS  <- c("C", "W", "D", "G")
.NHL_CLASSIC_LO   <- c(C = 2L, W = 3L, D = 2L, G = 1L)
.NHL_CLASSIC_HI   <- c(C = 3L, W = 4L, D = 3L, G = 1L)
.NHL_CLASSIC_BASE <- c(C = 2L, W = 3L, D = 2L)
NHL_CLASSIC_SLOTS <- c("C", "C", "W", "W", "W", "D", "D", "G", "UTIL")

.nhl_assign_slots_vec <- function(chosen) {
  d <- as.data.table(chosen)[, .(SimID, Player, Pos, StartOrder)]
  fixed <- d[Pos == "G"][, slot := "G"]
  sk <- d[Pos %chin% c("C", "W", "D")]
  sk[, npos := .N, by = .(SimID, Pos)]
  sk[, quota := pmax(npos - .NHL_CLASSIC_BASE[Pos], 0L)]   # 1 for the UTIL's position, else 0
  setorder(sk, SimID, -StartOrder)                          # latest start first
  sk[, pr := rowid(SimID, Pos)]
  sk[, slot := fifelse(pr <= quota, "UTIL", Pos)]
  out <- rbind(fixed[, .(SimID, Player, slot)], sk[, .(SimID, Player, slot)])
  out[, so := c(C = 1L, W = 2L, D = 3L, G = 4L, UTIL = 5L)[slot]]
  setorder(out, SimID, so)
  out[, slot_i := rowid(SimID)]
  out[, .(SimID, Player, slot_i)]
}

find_optimal_lineups_nhl_classic <- function(sim_results, config, verbose = TRUE) {
  setDT(sim_results)
  if (!"Pos" %in% names(sim_results)) stop("nhl_classic optimiser needs a Pos column on sim_results")
  if (!"StartOrder" %in% names(sim_results)) sim_results[, StartOrder := 1L]
  need <- 9L
  cap  <- config$salary_cap %||% 50000
  max_lineups <- config$max_lineups %||% 5000L
  start_time <- Sys.time()

  SR <- sim_results[!is.na(FantasyPoints) & !is.na(Salary) & Salary > 0 & Pos %chin% .NHL_CLASSIC_POS,
                    .(SimID, Player, FantasyPoints, Salary, Pos, StartOrder)]
  if (!nrow(SR)) stop("nhl_classic optimiser: no priced players on sim_results")
  top_n   <- config$candidate_top_n   %||% 40L
  cheap_n <- config$candidate_cheap_n %||% 10L
  pm <- SR[, .(mu = mean(FantasyPoints), sal = Salary[1]), by = .(Player, Pos)]
  keep_pl <- pm[, .SD[union(head(order(-mu), top_n), head(order(sal), cheap_n)), Player], by = Pos]$V1
  SR <- SR[Player %chin% keep_pl]
  n_sims_full <- uniqueN(SR$SimID)
  if (verbose) cat(sprintf("\nPhase 1: NHL classic | %s sims | $%s cap | %d slots | %d candidates\n",
                           format(n_sims_full, big.mark = ","), format(cap, big.mark = ","), need, length(keep_pl)))

  setorder(SR, SimID, -FantasyPoints)
  SR[, pr := rowid(SimID, Pos)]

  # FAST PATH: the unconstrained best, one variant per UTIL position
  base_c <- SR[(Pos == "C" & pr <= 2L) | (Pos == "W" & pr <= 3L) | (Pos == "D" & pr <= 2L) | (Pos == "G" & pr == 1L)]
  flex_c <- SR[(Pos == "C" & pr == 3L) | (Pos == "W" & pr == 4L) | (Pos == "D" & pr == 3L)]
  setorder(flex_c, SimID, -FantasyPoints)
  flex_c[, variant := rowid(SimID)]
  cand <- rbindlist(lapply(sort(unique(flex_c$variant)), function(v)
    rbindlist(list(base_c[, .(SimID = paste0(SimID, "_v", v), Player, Pos, StartOrder, Salary)],
                   flex_c[variant == v, .(SimID = paste0(SimID, "_v", v), Player, Pos, StartOrder, Salary)]),
              use.names = TRUE)), use.names = TRUE)
  full9 <- cand[, .N, by = SimID][N == need, SimID]
  cand  <- cand[SimID %chin% full9]
  under <- cand[, .(s = sum(Salary)), by = SimID][s <= cap, SimID]
  fast  <- cand[SimID %chin% under]

  # SLOW PATH: every sim whose unconstrained best breaks the cap, solved exactly
  v1_ok    <- unique(sub("_v[0-9]+$", "", grep("_v1$", under, value = TRUE)))
  slow_ids <- setdiff(unique(SR$SimID), v1_ok)
  slow <- NULL
  if (length(slow_ids)) {
    SS <- SR[SimID %chin% slow_ids, .(SimID, Player, Pos, StartOrder, Salary, FantasyPoints)]
    lo <- .NHL_CLASSIC_LO; hi <- .NHL_CLASSIC_HI
    n_cores <- .opt_workers()
    use_par <- isTRUE(config$use_parallel %||% TRUE) && length(slow_ids) > 500L && n_cores > 1L
    if (use_par) {
      grp <- split(slow_ids, cut(seq_along(slow_ids), n_cores, labels = FALSE))
      cl <- parallel::makeCluster(n_cores, type = "PSOCK")
      on.exit(parallel::stopCluster(cl), add = TRUE)
      slow <- rbindlist(parallel::parLapply(cl, lapply(grp, function(g) SS[SimID %in% g]),
                                            .classic_exact_chunk, cap = cap, lo = lo, hi = hi, need = need))
    } else {
      slow <- .classic_exact_chunk(SS, cap, lo, hi, need)
    }
    if (nrow(slow)) slow[, SimID := paste0(SimID, "_x")]
  }
  have_slow <- !is.null(slow) && nrow(slow) > 0L && "Player" %in% names(slow)
  if (verbose) cat(sprintf("  %s fast (under cap) + %s of %s cap-binding solved exactly\n",
                           format(length(under), big.mark = ","),
                           format(if (have_slow) uniqueN(slow$SimID) else 0L, big.mark = ","),
                           format(length(slow_ids), big.mark = ",")))
  chosen <- rbindlist(list(fast[, .(SimID, Player, Pos, StartOrder)],
                           if (have_slow) slow[, .(SimID, Player, Pos, StartOrder)]), use.names = TRUE)
  if (!nrow(chosen)) stop("nhl_classic optimiser: no feasible lineup in any sim")

  full <- .nhl_assign_slots_vec(chosen)
  wide <- dcast(full, SimID ~ slot_i, value.var = "Player", fun.aggregate = function(x) x[1], fill = NA_character_)
  pc <- paste0("Player", seq_len(need))
  setnames(wide, as.character(seq_len(need)), pc)
  wide[, lkey := apply(as.matrix(.SD), 1L, function(r) paste(sort(r), collapse = "|")), .SDcols = pc]
  wide <- wide[!duplicated(data.table(sub("_(v[0-9]+|x)$", "", SimID), lkey))]
  cnt <- wide[, .(Top1Count = .N), by = lkey]
  uni <- merge(wide[!duplicated(lkey)], cnt, by = "lkey")
  mu  <- setNames(pm$mu, pm$Player)
  uni[, AvgScore := rowSums(matrix(mu[unlist(.SD)], nrow = nrow(uni))), .SDcols = pc]
  setorder(uni, -Top1Count, -AvgScore)
  if (nrow(uni) > max_lineups) uni <- head(uni, max_lineups)
  sal <- setNames(pm$sal, pm$Player)
  uni[, TotalSalary := rowSums(matrix(sal[unlist(.SD)], nrow = nrow(uni))), .SDcols = pc]
  uni[, lkey := NULL]
  if (verbose) cat(sprintf("  %s distinct lineups | %.1fs\n", format(nrow(uni), big.mark = ","),
                           as.numeric(difftime(Sys.time(), start_time, units = "secs"))))
  list(unique_lineups = uni, n_sims = n_sims_full, config = config, mode = "nhl_classic")
}

# DK NHL classic: players from at least 3 teams, and at least 2 games.
nhl_drop_invalid_classic <- function(lineup_data, metadata) {
  ul <- lineup_data$unique_lineups; pc <- grep("^Player[0-9]+$", names(ul), value = TRUE)
  if (!nrow(ul)) return(lineup_data)
  M <- as.matrix(ul[, ..pc])
  tm <- matrix(metadata$Team[match(M, metadata$Player)], nrow(M))
  gm <- matrix(metadata$GameKey[match(M, metadata$Player)], nrow(M))
  ok <- apply(tm, 1L, function(r) length(unique(r)) >= 3L) & apply(gm, 1L, function(r) length(unique(r)) >= 2L)
  if (any(!ok)) cat(sprintf("  dropped %d lineup(s) with < 3 teams or < 2 games\n", sum(!ok)))
  lineup_data$unique_lineups <- ul[ok]
  lineup_data
}

# Classic upload: "Name (ID)" under C C W W W D D G UTIL. The UTIL slot takes
# the player's UTIL draftable (DKUID) -- DK gives each skater a second ID for it.
nhl_classic_download <- function(dl, metadata) {
  dl <- copy(as.data.table(dl)); setDT(metadata)
  pc <- grep("^Player[0-9]+$", names(dl), value = TRUE)
  if (length(pc) != 9L) return(dl)
  for (i in seq_along(pc)) {
    idc <- if (NHL_CLASSIC_SLOTS[i] == "UTIL") "DKUID" else "DKID"
    ids <- metadata[[idc]][match(dl[[pc[i]]], metadata$Player)]
    set(dl, j = pc[i], value = paste0(dl[[pc[i]]], " (", ids, ")"))
  }
  idx <- match(pc, names(dl))
  setcolorder(dl, c(idx, setdiff(seq_along(dl), idx)))
  setnames(dl, seq_along(NHL_CLASSIC_SLOTS), NHL_CLASSIC_SLOTS)
  dl
}
