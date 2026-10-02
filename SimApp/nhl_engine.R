# =============================================================================
# nhl_engine.R -- NHL (DK classic + showdown) for SimApp
# -----------------------------------------------------------------------------
# The input is the workbook GTS/NHL/R/live/build_slate.R writes: one per night,
# every game on it. The frame is computed there -- roles, TOI shares, rates, the
# market and each game's solved regulation grid -- so this file only simulates:
#
#   read_nhl_input(path)          Games / Players / Goalies / IDs_<dg> / Meta
#   run_nhl_simulation(input)     sim_team_box() -> sim_players(), DK points
#   nhl_sim_visuals(...)          the Sim Results tab's validation summaries
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
  for (s in c("Games", "Players", "Goalies", "Model_Games", "Model_Players"))
    if (!s %in% sheets) stop("NHL workbook is missing the '", s, "' sheet")
  # The readable tabs carry what a person reads; Model_* carry the builder's frame.
  # Join them back into the one frame the model takes, in the readable tabs' order.
  join_model <- function(x, m, by, what) {
    out <- merge(x[, .ord := .I], m, by = by, all.x = TRUE, sort = FALSE)
    if (nrow(out) != nrow(x)) stop("NHL: duplicate ", what, " rows on the Model tab")
    setorder(out, .ord)[, .ord := NULL]
  }
  mp <- rd("Model_Players")
  games   <- join_model(rd("Games"),   rd("Model_Games"), "gameId", "game")
  players <- join_model(rd("Players"), mp, c("gameId", "playerId"), "player")
  goalies <- join_model(rd("Goalies"), mp, c("gameId", "playerId"), "goalie")
  gnm <- setdiff(names(mp), c("gameId", "playerId"))                      # skater-only columns on goalie rows
  goalies[, names(which(vapply(goalies[, ..gnm], function(v) all(is.na(v)), TRUE))) := NULL]
  if (anyNA(games$grid_lam_h) || anyNA(players$mu_es))
    stop("NHL: a Games / Players row has no Model_Games / Model_Players row; rebuild the sheet")
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
  # Projected main-classic ownership, % (build_slate.R `own` on Players /
  # Goalies, RotoWire, from 30 Sep 2026). An older sheet has none: NA, and the
  # app shows no AvgOwn / OwnProj / Leverage.
  own <- rbind(P[, if ("own" %in% names(P)) .(playerId, own = as.numeric(own)) else .(playerId, own = NA_real_)],
               GL[, if ("own" %in% names(GL)) .(playerId, own = as.numeric(own)) else .(playerId, own = NA_real_)])
  T[, DKOwn := own$own[match(playerId, own$playerId)]]
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

  meta <- T[, .(Player, Team = team, Opp = opp, Pos, DKPos, Line = slot, PP = pp,
                DKID, DKUID, DKSalary, DKOwn, SDID, SDCID, SDSalary, CPTSalary,
                GameKey, ShowdownFile, playerId)]
  pr <- sims[, .(DKProj = mean(DKScore)), by = Player]
  meta[pr, on = "Player", DKProj := round(i.DKProj, 2)]
  meta[T, on = "Player", StartOrder := frank(i.StartUTC, ties.method = "dense")]   # late swap: the UTIL goes to the latest start
  pcb("Validation summaries", 0.95)
  # Never let a summary cost the sim: a failure here leaves the tab empty.
  vis <- tryCatch(nhl_sim_visuals(bx, sk, sims, G, T, input_data),
                  error = function(e) { warning("NHL visuals: ", conditionMessage(e)); NULL })
  # Engine review (stat_quantiles.R): skater / goalie / team stat quantiles. Off unless the review's
  # re-sim worker sets options(gts.stat_quantiles = TRUE); reads sk and bx, draws no random numbers.
  if (exists("gts_stat_q_on") && gts_stat_q_on())
    vis <- gts_stat_attach(vis, function() {
      who <- T[Pos != "G", .(playerId, gameId, Player, Team = team)]
      skq <- gts_stat_summ(sk, c(playerId = "playerId", game = "game"),
               list(g = "g", a = "a", pts = quote(g + a), sog = "sog", blk = "blk", toi_min = quote(toi / 60),
                    dk_fpts = "dk"), "player", list(sog = c(3, 5), blk = 3, pts = c(1, 3), g = c(1, 3)))
      for (k in c("q", "ge")) if (nrow(skq[[k]])) {
        skq[[k]][G, on = "game", gameId := i.gameId]
        skq[[k]] <- merge(who, skq[[k]], by = c("playerId", "gameId"))[, game := NULL]
      }
      b <- bx[, .(game, sim, is_home, goals = goals + (end == "SO" & result == "W"), sog, s_sv, s_ga, s_dk)]
      b[G, on = "game", Team := fifelse(is_home, i.home, i.away)]
      tot <- b[, .(total_goals = sum(goals)), by = .(game, sim)]
      tot[G, on = "game", Team := paste(i.away, "@", i.home)]
      list(skq,
           gts_stat_summ(b, c(Team = "Team"), list(saves = "s_sv", ga = "s_ga", dk_fpts = "s_dk"), "goalie", list(saves = 35)),
           gts_stat_summ(b, c(Team = "Team"), list(goals = "goals", sog = "sog"), "team"),
           gts_stat_summ(tot, c(Team = "Team"), list(total_goals = "total_goals"), "game"))
    })
  pcb("Done", 1)
  list(sim_results = sims, metadata = meta, sport_visuals = vis)
}

# =============================================================================
# SIM RESULTS: validation summaries
# -----------------------------------------------------------------------------
# Small tables only -- the per-sim box scores are summarised here and dropped.
# Everything is laid against something the sim should agree with:
#   games       win / total / regulation prices, market vs sim (goals count the
#               shootout winner and a push voids, as the de-vigged price does)
#   goal_dist   each team's goals, 0..7+
#   goalies     the starter's saves / win / shutout; team win vs the market
#   skaters     SOG, points, blocks, DK bonus rates; the Pinnacle SOG line and
#               its de-vigged P(over) when the sheet carries them (sog_line /
#               sog_p_over, written by build_slate.R); SheetDK is the
#               builder's own smoke-sim mean, a parity check on this app
#   line_share  each team's DK points by line / pair / PP unit
#   corr_*      DK-point correlation, by relation (linemates, PP unit, opponent,
#               goalie) and per game as a matrix, off the first 20k sims
#   score_dist  DK point percentiles per player
# =============================================================================
nhl_sim_visuals <- function(bx, sk, sims, G, T, input_data, n_corr = 20000L) {
  n <- uniqueN(bx$sim)
  G <- copy(G)
  b <- bx[, .(game, sim, is_home, goals, reg_goals, result, end, sog, s_dec, s_so, s_sv, s_ga, s_dk, relieved)]
  b[, fg := goals + (end == "SO" & result == "W")]
  b[G, on = "game", Team := fifelse(is_home, i.home, i.away)]

  # ---- games ------------------------------------------------------------------
  gm <- b[, .(tot = sum(fg), reg = sum(reg_goals), home_w = any(is_home & result == "W"),
              reg_draw = end[1] != "REG"), by = .(game, sim)]
  gm <- merge(gm, G[, .(game, total, reg_total)], by = "game")
  gs <- gm[, .(sim_p_home = mean(home_w), sim_total = mean(tot),
               sim_p_over = sum(tot > total) / max(1, sum(tot != total)),
               sim_p_reg_home = mean(home_w & !reg_draw), sim_p_reg_draw = mean(reg_draw),
               sim_reg_total = mean(reg),
               sim_p_reg_over = sum(reg > reg_total) / max(1, sum(reg != reg_total))), by = game]
  side <- dcast(b[, .(goals = mean(fg), sog = mean(sog)), by = .(game, is_home)],
                game ~ is_home, value.var = c("goals", "sog"), fun.aggregate = mean)
  mk <- function(col) if (col %in% names(G)) as.numeric(G[[col]]) else NA_real_
  games <- data.table(game = G$game, Game = paste(G$away, "@", G$home), away = G$away, home = G$home,
                      src = if ("mkt_src" %in% names(G)) G$mkt_src else NA_character_,
                      p_home = mk("p_home"), total = mk("total"), p_over = mk("p_over"),
                      p_reg_home = mk("p_reg_home"), p_reg_draw = mk("p_reg_draw"),
                      reg_total = mk("reg_total"), p_reg_over = mk("p_reg_over"))
  games <- merge(games, gs, by = "game")
  games <- merge(games, side, by = "game")
  setnames(games, c("goals_TRUE", "goals_FALSE", "sog_TRUE", "sog_FALSE"),
           c("home_goals", "away_goals", "home_sog", "away_sog"), skip_absent = TRUE)
  setorder(games, game)

  goal_dist <- b[, .N, by = .(game, Team, is_home, g = pmin(fg, 7L))][, pct := N / n][, N := NULL]
  setorder(goal_dist, game, -is_home, g)
  goal_dist[G, on = "game", Game := paste(i.away, "@", i.home)]

  # ---- goalies (the starter's line off the team box) --------------------------
  gl <- b[, .(team_win = mean(result == "W"), g_win = mean(s_dec %chin% "W"),
              shutout = mean(s_so > 0), saves = mean(s_sv),
              sv_p10 = as.numeric(quantile(s_sv, .1)), sv_p90 = as.numeric(quantile(s_sv, .9)),
              p_35sv = mean(s_sv >= 35), ga = mean(s_ga), relieved = mean(relieved %in% TRUE),
              dk = mean(s_dk)), by = .(game, is_home, Team)]
  gl[G, on = "game", mkt_win := fifelse(is_home, i.p_home, 1 - i.p_home)]
  gl[G, on = "game", Game := paste(i.away, "@", i.home)]
  G2 <- G[, .(game, gameId)]
  gl <- merge(gl, G2, by = "game")
  st <- T[Pos == "G", .(gameId, is_home, Player, playerId)]
  gl <- merge(gl, st, by = c("gameId", "is_home"), all.x = TRUE)
  gsd <- input_data$goalies[starter == TRUE, .(playerId, sheet_dk = as.numeric(sim_dk))]
  if ("sim_dk" %in% names(input_data$goalies)) gl[gsd, on = "playerId", SheetDK := i.sheet_dk]
  setorder(gl, game, -is_home)

  # ---- skaters ----------------------------------------------------------------
  sk2 <- sk[, .(playerId, game, g, a, sog, blk, toi, dk)]
  sk2[G2, on = "game", gameId := i.gameId]
  sks <- sk2[, .(TOI = mean(toi) / 60, SOG = mean(sog), SOG3 = mean(sog >= 3), SOG5 = mean(sog >= 5),
                 G = mean(g), A = mean(a), Pts = mean(g + a), PGoal = mean(g > 0), PPoint = mean(g + a > 0),
                 PTS3 = mean(g + a >= 3), BLK = mean(blk), BLK3 = mean(blk >= 3), DK = mean(dk)),
             by = .(playerId, gameId)]
  who <- T[Pos != "G", .(playerId, gameId, Player, Team = team, Pos, grp, Line = slot, PP = pp, GameKey)]
  sks <- merge(who, sks, by = c("playerId", "gameId"))
  P <- input_data$players
  if ("sim_dk" %in% names(P)) sks[P, on = "playerId", SheetDK := as.numeric(i.sim_dk)]
  if (all(c("sog_line", "sog_p_over") %in% names(P))) {
    ln <- P[!is.na(sog_line) & !is.na(sog_p_over), .(playerId, sog_line = as.numeric(sog_line), sog_p_over = as.numeric(sog_p_over))]
    if (nrow(ln)) {
      so <- sk2[ln, on = "playerId", nomatch = NULL][, .(SimOver = mean(sog > sog_line)), by = playerId]
      sks[ln, on = "playerId", `:=`(SOGLine = i.sog_line, MktOver = i.sog_p_over)]
      sks[so, on = "playerId", SimOver := i.SimOver]
    }
  }
  if (!"SOGLine" %in% names(sks)) sks[, `:=`(SOGLine = NA_real_, MktOver = NA_real_, SimOver = NA_real_)]
  setorder(sks, -DK)

  # ---- DK points by line / pair / PP unit, per team ----------------------------
  unit <- function(grp, line) fifelse(grp == "D", paste0("D", line), paste0("L", line))
  ls <- sks[, .(DK = sum(DK), SOG = sum(SOG)), by = .(Team, Unit = unit(grp, Line))]
  ls <- rbind(ls, gl[, .(Team, Unit = "G", DK = dk, SOG = 0)])
  ls[, share := DK / sum(DK), by = Team]
  pp <- sks[, .(DK = sum(DK)), by = .(Team, Unit = fifelse(PP %in% 1L, "PP1", fifelse(PP %in% 2L, "PP2", "no PP")))]
  pp[, share := DK / sum(DK), by = Team]

  # ---- DK score percentiles -----------------------------------------------------
  sd_ <- sims[, .(Mean = round(mean(DKScore), 1), P10 = round(quantile(DKScore, .10), 1),
                  P25 = round(quantile(DKScore, .25), 1), Median = round(quantile(DKScore, .5), 1),
                  P75 = round(quantile(DKScore, .75), 1), P90 = round(quantile(DKScore, .90), 1),
                  P99 = round(quantile(DKScore, .99), 1)), by = Player]
  sd_ <- merge(sd_, T[, .(Player, Team = team, Pos, GameKey)], by = "Player")

  # ---- correlation ------------------------------------------------------------
  keep_sim <- sort(unique(sims$SimID))[seq_len(min(n_corr, n))]
  cs <- sims[SimID %in% keep_sim]
  info <- T[, .(Player, Team = team, Pos, grp, Line = slot, PP = pp, gameId, GameKey)]
  info[, ord := fcase(grp == "F", 10L + fcoalesce(Line, 9L), grp == "D", 20L + fcoalesce(Line, 9L), default = 30L)]
  corr_game <- list(); pairs <- list()
  for (gid in G$gameId) {
    pl <- info[gameId == gid][order(Team, ord, Player)]
    w <- dcast(cs[Player %chin% pl$Player], SimID ~ Player, value.var = "DKScore", fun.aggregate = mean)
    m <- suppressWarnings(cor(as.matrix(w[, -1])))
    pl <- pl[Player %chin% colnames(m)]
    m <- round(m[pl$Player, pl$Player, drop = FALSE], 3)
    corr_game[[pl$GameKey[1]]] <- list(players = pl[, .(Player, Team, Pos, Line, PP)], cor = m)
    ij <- which(upper.tri(m), arr.ind = TRUE)
    a <- pl[ij[, 1]]; z <- pl[ij[, 2]]
    same <- a$Team == z$Team; ga <- a$Pos == "G"; gz <- z$Pos == "G"
    sl <- fcoalesce(a$Line, -1L) == fcoalesce(z$Line, -2L)   # element-wise; NA never matches
    rel <- fcase(same & ga & gz, NA_character_,
                 same & (ga | gz), "Skater + own goalie",
                 same & a$grp == "F" & z$grp == "F" & sl, "Forward linemates",
                 same & a$grp == "D" & z$grp == "D" & sl, "D partners",
                 same & a$PP %in% 1L & z$PP %in% 1L, "PP1 unit, not linemates",
                 same, "Teammates, other lines",
                 ga & gz, "Goalie vs goalie",
                 ga | gz, "Skater vs opposing goalie",
                 default = "Opponents")
    pairs[[length(pairs) + 1L]] <- data.table(rel = rel, r = m[ij])
  }
  pairs <- rbindlist(pairs)[!is.na(rel) & is.finite(r)]
  corr_sum <- pairs[, .(r = round(mean(r), 3), pairs = .N), by = rel]
  lvl <- c("Forward linemates", "D partners", "PP1 unit, not linemates", "Skater + own goalie",
           "Teammates, other lines", "Opponents", "Skater vs opposing goalie", "Goalie vs goalie")
  corr_sum <- corr_sum[order(match(rel, lvl))]

  checks <- input_data$checks
  list(n_sims = n, n_corr = length(keep_sim), games = games, goal_dist = goal_dist, goalies = gl,
       skaters = sks, line_share = ls, pp_share = pp, score_dist = sd_,
       corr_sum = corr_sum, corr_game = corr_game,
       checks = if (!is.null(checks) && "level" %in% names(checks)) checks[level != "OK"] else NULL,
       meta = input_data$meta)
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

# `metadata` (Player / Team / GameKey), when given, applies DK's 3-team / 2-game
# rule BEFORE the max_lineups cut, so the pool fills to the cap with legal
# lineups instead of being cut first and thinned after.
find_optimal_lineups_nhl_classic <- function(sim_results, config, verbose = TRUE, metadata = NULL) {
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
  if (!is.null(metadata)) {
    ok <- .nhl_classic_valid(as.matrix(uni[, ..pc]), metadata)
    if (verbose && any(!ok)) cat(sprintf("  dropped %s lineup(s) with skaters from < 3 teams or < 2 games (before the cap)
",
                                         format(sum(!ok), big.mark = ",")))
    uni <- uni[ok]
  }
  # The cap (30 Sep 2026). Top1Count is 1 for nearly every lineup on a big
  # classic, so the old order fell through to summed means and kept the 5,000
  # most mean-heavy optima. Rank instead by each lineup's top-5% rate among ALL
  # the distinct optima (ps_top_frac, OptimalLineups_Core.R; NFL/CFB classic's
  # phase1_metric = "top5"). On the 30 Sep slate the capped pool's top-150 by
  # Top1 then matched the uncapped pool's on 83 lineups (was 60); on the 29 Sep
  # flagship its top-150 by Top1 cashed 19.3% (was 9.3%). "mean" restores the old cut.
  if (nrow(uni) > max_lineups && !identical(config$phase1_metric, "mean")) {
    t5 <- ps_top_frac(uni, pc, sim_results, "FantasyPoints", n_sims_use = 5000L, frac = 0.05)
    if (!is.null(t5)) { uni[, top5 := t5]; setorder(uni, -top5, -AvgScore); uni[, top5 := NULL] }
    else if (verbose) cat("  top5 ranking unavailable (Matrix / matrixStats): capping by mean\n")
  }
  if (nrow(uni) > max_lineups) uni <- head(uni, max_lineups)
  sal <- setNames(pm$sal, pm$Player)
  uni[, TotalSalary := rowSums(matrix(sal[unlist(.SD)], nrow = nrow(uni))), .SDcols = pc]
  uni[, lkey := NULL]
  if (verbose) cat(sprintf("  %s distinct lineups | %.1fs\n", format(nrow(uni), big.mark = ","),
                           as.numeric(difftime(Sys.time(), start_time, units = "secs"))))
  list(unique_lineups = uni, n_sims = n_sims_full, config = config, mode = "nhl_classic")
}

# DK NHL classic: SKATERS from at least 3 teams, and players from at least 2
# games. The goalie does not count toward the 3 teams (DK rejected a BOS / EDM
# skater stack with an MTL goalie, 29 Sep 2026), so his team is blanked first.
.nhl_classic_valid <- function(M, metadata) {
  n_distinct <- function(X) {   # distinct values per row, vectorised: sort each row, count changes
    X <- t(apply(X, 1L, sort, na.last = TRUE))
    1L + rowSums(X[, -1L, drop = FALSE] != X[, -ncol(X), drop = FALSE], na.rm = TRUE)
  }
  tm <- matrix(metadata$Team[match(M, metadata$Player)], nrow(M))
  if ("Pos" %in% names(metadata)) tm[matrix(metadata$Pos[match(M, metadata$Player)] %in% "G", nrow(M))] <- NA
  gm <- matrix(metadata$GameKey[match(M, metadata$Player)], nrow(M))
  n_distinct(tm) >= 3L & n_distinct(gm) >= 2L
}

nhl_drop_invalid_classic <- function(lineup_data, metadata) {
  ul <- lineup_data$unique_lineups; pc <- grep("^Player[0-9]+$", names(ul), value = TRUE)
  if (!nrow(ul)) return(lineup_data)
  ok <- .nhl_classic_valid(as.matrix(ul[, ..pc]), metadata)
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
