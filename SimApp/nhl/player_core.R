# =============================================================================
# player_core.R -- P3 shared functions (P3_DESIGN.md)
# -----------------------------------------------------------------------------
# Shared by R/fit/08_roles_toi.R .. 11_shots_blocks.R and eventually
# player_model.R. Definitions only; sourcing it runs nothing.
#
#   state_seconds(season)     per player-game and team-game seconds in the P2 box
#                             states, rebuilt from the shift charts
#   prev_roles(pg, ss)        the pre-game roster frame: each dressed skater's
#                             role from the team's previous game (P3 step 0)
#   toi_alloc(mu, secs, cap)  shares -> seconds, rescaled to the team total with
#                             no skater above the whole state
#
# States match sim_team_box()'s columns, so a share fitted here multiplies a
# simulated second directly:
#   es   = 5v5 + regulation 4v4/3v3   (box: sec_5v5 + sec_eo - sec_ot)
#   pp, pk                            (box: sec_pp, sec_pk; regulation only)
#   ot   = every OT second with both goalies in, any skater count (box: sec_ot)
#   pul  = own goalie pulled          (box: sec_pulled)
#   opul = opponent's goalie pulled   (box: sec_opp_pulled)
# =============================================================================

STATES <- c("es", "pp", "pk", "ot", "pul", "opul")
N_ON   <- c(es = 5, pp = 5, pk = 4, ot = 3, pul = 6, opul = 5)   # nominal skaters on

# ---- state seconds from shifts ---------------------------------------------------------
# Second-by-second, as nhl_parse.R::build_units does, in chunks of games so a
# season (~56M player-seconds) never sits in memory at once.
state_seconds <- function(season, goalie_ids, chunk = 120L) {
  sh <- readRDS(sprintf("NHL_Database/shifts_%d.rds", season))
  sh <- sh[en > st]
  gids <- sort(unique(sh$gameId))
  out <- lapply(split(gids, ceiling(seq_along(gids) / chunk)), function(g) {
    x <- sh[gameId %in% g]
    n <- x$en - x$st
    secs <- x[rep(seq_len(.N), n), .(gameId, period, playerId, teamId)]
    secs[, s := rep(x$st, n) + sequence(n)]
    secs <- unique(secs, by = c("gameId", "period", "s", "playerId"))
    secs[, isG := playerId %in% goalie_ids]
    ts <- secs[, .(nsk = sum(!isG), ng = sum(isG)), by = .(gameId, period, s, teamId)]
    ts <- merge(ts, ts[, .(gameId, period, s, oteam = teamId, onsk = nsk, ong = ng)],
                by = c("gameId", "period", "s"), allow.cartesian = TRUE)[teamId != oteam]
    ts[, state := fcase(ng == 0L & ong == 1L, "pul",
                        ng == 1L & ong == 0L, "opul",
                        ng == 1L & ong == 1L & period >= 4L, "ot",
                        ng == 1L & ong == 1L & nsk == onsk, "es",
                        ng == 1L & ong == 1L & nsk > onsk, "pp",
                        ng == 1L & ong == 1L & nsk < onsk, "pk",
                        default = "other")]
    team <- ts[, .N, by = .(gameId, teamId, state)]
    ps <- merge(secs[isG == FALSE, .(gameId, period, s, playerId, teamId)],
                ts[, .(gameId, period, s, teamId, state)], by = c("gameId", "period", "s", "teamId"))
    list(player = ps[, .N, by = .(gameId, playerId, teamId, state)], team = team)
  })
  wide <- function(d, id) {
    w <- dcast(d, as.formula(paste(paste(id, collapse = "+"), "~ state")), value.var = "N", fill = 0L)
    for (s in c(STATES, "other")) if (!s %in% names(w)) set(w, j = s, value = 0L)
    setnames(w, c(STATES, "other"), paste0("sec_", c(STATES, "other")))
    w
  }
  list(player = wide(rbindlist(lapply(out, `[[`, "player")), c("gameId", "playerId", "teamId")),
       team   = wide(rbindlist(lapply(out, `[[`, "team")), c("gameId", "teamId")))
}

# ---- the pre-game roster frame -----------------------------------------------------------
# pg: one row per skater-game with gameId, teamId, playerId, date, season, grp
# (F / D), line, pair, pp_unit, toi, sec_<state>. Team-game order is by date.
#
# Roles, from the team's previous game:
#   es slot  F1..F4 / D1..D3. The previous game's modal line / pair where it
#            is 1-4 / 1-3; a fragment (NA or 5+) fills the line with the fewest
#            members (to 3, or to 2 for a pair) in order of 5v5 seconds, else the
#            bottom slot.
#   pp       PP1 / PP2 / none, from the team's most recent game with >= 60 s of PP
#            (a game with no penalties says nothing about the units).
#   pk       PK1 (top two of the group by PK seconds) / PK2 (next two) / none,
#            from the most recent game with >= 60 s of PK.
# A dressed skater who did not play that game takes the vacated role of a
# player who is out: newcomers ranked by recent TOI take the vacated slots best
# first, each inheriting that player's PP and PK roles too. More newcomers
# than vacancies (11F/7D swaps) go to the bottom slot with no PP / PK role.
label_slots <- function(d, grp_, cap, size) {    # d: one team-game, one group
  lab <- if (grp_ == "F") d$line else d$pair
  lab[is.na(lab) | lab > cap] <- NA_integer_
  o <- order(-d$sec_es)
  for (i in o[is.na(lab[o])]) {
    cnt <- tabulate(lab[!is.na(lab)], cap)
    j <- which(cnt < size)
    lab[i] <- if (length(j)) j[which.min(cnt[j])] else cap
  }
  lab
}
pk_tier <- function(sec, grp) {                 # within group: 1 = top two, 2 = next two
  r <- frank(-sec, ties.method = "first")
  fifelse(sec <= 0, 0L, fifelse(r <= 2L, 1L, fifelse(r <= 4L, 2L, 0L)))
}
prev_roles <- function(pg, tt, min_sec = 60) {
  pg <- copy(pg); setorder(pg, teamId, date, gameId)
  # this game's own (ex-post) roles, used as the next game's pre-game roles
  pg[, es_lab := 0L]
  pg[grp == "F", es_lab := label_slots(.SD, "F", 4L, 3L), by = .(gameId, teamId)]
  pg[grp == "D", es_lab := label_slots(.SD, "D", 3L, 2L), by = .(gameId, teamId)]
  pg[, pk_lab := pk_tier(sec_pk, grp[1]), by = .(gameId, teamId, grp)]
  pg[, pp_lab := fcoalesce(pp_unit, 0L)]

  # team-game sequence; the PP / PK source game is the latest with enough time
  tg <- unique(pg[, .(teamId, gameId, date, season)])
  tg <- merge(tg, tt[, .(gameId, teamId, t_pp = sec_pp, t_pk = sec_pk)], by = c("gameId", "teamId"))
  setorder(tg, teamId, date, gameId)
  tg[, prev_g := shift(gameId), by = teamId]
  last_ok <- function(g, ok) { v <- fifelse(ok, g, NA_integer_); v <- nafill(v, "locf"); shift(v) }
  tg[, `:=`(prev_pp_g = last_ok(gameId, t_pp >= min_sec), prev_pk_g = last_ok(gameId, t_pk >= min_sec)), by = teamId]

  # recent TOI (any team), for ranking newcomers: EW, 10 games, strictly earlier
  setorder(pg, playerId, date, gameId)
  pg[, rec_toi := { y <- stats::filter(toi, 0.5^(1 / 10), method = "recursive")
                    w <- stats::filter(rep(1, .N), 0.5^(1 / 10), method = "recursive")
                    c(NA, head(as.numeric(y / w), -1)) }, by = playerId]

  cur <- merge(pg[, .(gameId, teamId, playerId, grp, rec_toi)], tg[, .(gameId, teamId, prev_g, prev_pp_g, prev_pk_g)],
               by = c("gameId", "teamId"))
  P  <- pg[, .(prev_g = gameId, teamId, playerId, p_grp = grp, p_es = es_lab, p_toi = toi)]
  PP <- pg[, .(prev_pp_g = gameId, teamId, playerId, p_pp = pp_lab)]
  PK <- pg[, .(prev_pk_g = gameId, teamId, playerId, p_pk = pk_lab)]
  cur <- merge(cur, P, by = c("prev_g", "teamId", "playerId"), all.x = TRUE)
  cur <- merge(cur, PP, by = c("prev_pp_g", "teamId", "playerId"), all.x = TRUE)
  cur <- merge(cur, PK, by = c("prev_pk_g", "teamId", "playerId"), all.x = TRUE)
  cur[!is.na(p_es) & p_grp != grp, p_es := NA_integer_]     # changed position: treat as new to the slot
  cur[, carried := !is.na(p_es)]
  cur[, `:=`(slot = p_es, pp = fcoalesce(p_pp, 0L), pk = fcoalesce(p_pk, 0L))]

  # vacancies: previous-game players of the group not dressed now
  vac <- merge(P, tg[, .(gameId, teamId, prev_g, prev_pp_g, prev_pk_g)], by = c("prev_g", "teamId"), allow.cartesian = TRUE)
  vac <- vac[!cur, on = .(gameId, teamId, playerId)]
  vac <- merge(vac, PP, by = c("prev_pp_g", "teamId", "playerId"), all.x = TRUE)
  vac <- merge(vac, PK, by = c("prev_pk_g", "teamId", "playerId"), all.x = TRUE)
  vac <- vac[order(gameId, teamId, p_grp, p_es, -p_toi)][, k := seq_len(.N), by = .(gameId, teamId, p_grp)]
  nw <- cur[carried == FALSE & !is.na(prev_g)][order(gameId, teamId, grp, -fcoalesce(rec_toi, 0))]
  nw[, k := seq_len(.N), by = .(gameId, teamId, grp)]
  nw <- merge(nw[, .(gameId, teamId, playerId, grp, k)],
              vac[, .(gameId, teamId, grp = p_grp, k, v_es = p_es, v_pp = fcoalesce(p_pp, 0L), v_pk = fcoalesce(p_pk, 0L))],
              by = c("gameId", "teamId", "grp", "k"), all.x = TRUE)
  cur[nw, on = .(gameId, teamId, playerId), `:=`(slot = fcoalesce(i.v_es, fifelse(grp == "F", 4L, 3L)),
                                                   pp = fcoalesce(i.v_pp, 0L), pk = fcoalesce(i.v_pk, 0L))]
  cur[, newcomer := !carried & !is.na(prev_g)]
  cur[is.na(prev_g), `:=`(slot = NA_integer_, pp = NA_integer_, pk = NA_integer_)]   # the database's first game: no role
  out <- merge(cur[, .(gameId, teamId, playerId, slot, pp, pk, newcomer, has_prev = !is.na(prev_g))],
               pg[, .(gameId, teamId, playerId, es_lab, pp_lab, pk_lab)], by = c("gameId", "teamId", "playerId"))
  out[]
}

# ---- shares -> seconds -------------------------------------------------------------------
# mu: expected shares of the state (0..1) for the dressed skaters of one team,
# n_on: skaters on at once. Rescales to sum n_on with no share above 1 (the
# excess of a capped skater is spread over the rest, in proportion).
cap_shares <- function(mu, n_on) {
  mu <- pmax(mu, 0); if (sum(mu) <= 0) return(rep(n_on / length(mu), length(mu)))
  s <- mu * n_on / sum(mu)
  for (it in 1:20) {
    over <- s > 1; if (!any(over)) break
    free <- !over & s < 1
    s[over] <- 1
    s[free] <- s[free] * (n_on - sum(s[!free])) / sum(s[free])
  }
  s
}

# Vectorised cap_shares over groups (g = group id): the same rescale, run on a
# whole frame at once.
cap_shares_by <- function(mu, g, n_on) {
  gi <- match(g, unique(g)); gsum <- function(x) as.vector(rowsum(x, gi, reorder = TRUE))[gi]
  s <- pmax(mu, 1e-9)
  for (it in 1:12) {
    capd <- s >= 1 - 1e-12
    s[capd] <- 1
    f <- (n_on - gsum(as.numeric(capd))) / gsum(fifelse(capd, 0, s))
    s <- fifelse(capd, 1, s * f)
    if (!any(s > 1 + 1e-9, na.rm = TRUE)) break
  }
  pmin(s, 1)
}

# Gaussian share scorer: y ~ N(mu, sig2 * m (1 - mu) * T0 / T + tau2), T the
# team's seconds in the state (a share over 40 s of 6v5 is noisier than one
# over 50 min of ES; the ratio is capped at 5). A proper score for comparing mean specifications; the
# noise (sig2, tau2) is fitted by ML on the fit window and held fixed when a
# spec is scored out of sample.
share_ll <- function(y, mu, par, tr = 1) {
  # m = max(mu, 0.02): without the floor a skater predicted at ~0 who does play
  # a shift scores -1e40 and one row decides the comparison
  v <- exp(par[1]) * pmax(mu, 0.02) * pmax(1 - mu, 0.02) * pmin(tr, 5) + exp(par[2])
  dnorm(y, mu, sqrt(v), log = TRUE)
}
fit_share_noise <- function(y, mu, tr = 1) {
  f <- function(p) -sum(share_ll(y, mu, p, tr))
  optim(c(log(0.05), log(1e-3)), f, method = "BFGS")$par
}

# ---- per-player-game counts by state, from the PBP ---------------------------------------
# The state is read from the acting team's side, with the same six states as
# state_seconds(): an attempt's state is the shooter's; a block is credited in
# the BLOCKER's state (a PP shot blocked is a PK block); a goal's assists share
# the scorer's state. Teammate blocks are not blocks (box and DK agree).
# Shootout attempts are excluded (is_attempt is FALSE for them).
FLIP <- c(es = "es", pp = "pk", pk = "pp", ot = "ot", pul = "opul", opul = "pul")
ev_state <- function(e) fcase(e$own_goalie == 0L, "pul", e$opp_goalie == 0L, "opul", e$period >= 4L, "ot",
                              e$own_sk == e$opp_sk, "es", e$own_sk > e$opp_sk, "pp", e$own_sk < e$opp_sk, "pk",
                              default = "es")
count_events <- function(season) {
  e <- readRDS(sprintf("NHL_Database/events_%d.rds", season))[is_attempt == TRUE]
  e[, state := ev_state(e)]
  sog <- e[type %chin% c("shot-on-goal", "goal"), .(playerId = shooter, state, sog = 1L, g = as.integer(type == "goal"),
                                                   g_open = as.integer(type == "goal" & opp_goalie == 1L),
                                                   sog_open = as.integer(opp_goalie == 1L), gameId)]
  blk <- e[type == "blocked-shot" & !is.na(blocker) & teammate_block == FALSE, .(gameId, playerId = blocker, state = unname(FLIP[state]), blk = 1L)]
  ast <- rbind(e[type == "goal" & !is.na(a1), .(gameId, playerId = a1, state, a1 = 1L, a2 = 0L)],
               e[type == "goal" & !is.na(a2), .(gameId, playerId = a2, state, a1 = 0L, a2 = 1L)])
  out <- rbindlist(list(sog, blk, ast), fill = TRUE)
  for (j in c("sog", "g", "g_open", "sog_open", "blk", "a1", "a2")) set(out, which(is.na(out[[j]])), j, 0L)
  out[, lapply(.SD, sum), by = .(gameId, playerId, state), .SDcols = c("sog", "g", "g_open", "sog_open", "blk", "a1", "a2")]
}

# ---- spec profiling (shared by 08 on; VIEW and HOLD are the calling script's) ------------
# score tables are data.tables of (gameId, teamId, season, ll); cmp() is the paired
# difference on one season, by team-game.
cmp <- function(a, b, s) {
  m <- merge(a[season == s], b[season == s], by = c("gameId", "teamId"))
  dv <- m$ll.x - m$ll.y; c(d = sum(dv), se = sd(dv) * sqrt(length(dv)))
}
profile <- function(grid, run, base) {
  sc <- lapply(seq_len(nrow(grid)), function(i) run(grid[i]))
  ib <- base(grid)
  out <- cbind(grid, rbindlist(lapply(seq_along(sc), function(i) {
    a <- cmp(sc[[i]], sc[[ib]], VIEW); b <- cmp(sc[[i]], sc[[ib]], HOLD)
    data.table(view_d = a[["d"]], view_se = a[["se"]], hold_d = b[["d"]], hold_se = b[["se"]])
  })))
  best <- which.max(out$view_d)
  vb <- rbindlist(lapply(seq_along(sc), function(i) {
    a <- cmp(sc[[i]], sc[[best]], VIEW); b <- cmp(sc[[i]], sc[[best]], HOLD)
    data.table(v_vs_best = a[["d"]], v_se_best = a[["se"]], h_vs_best = b[["d"]], h_se_best = b[["se"]])
  }))
  cbind(out, vb)
}

# Buhlmann-Straub k from unit sums (mi = sum m, si = sum num, qi = sum num^2 / m,
# ni = rows with m > 0): the same estimator as rates_core::bs_k, but a bootstrap
# resamples unit rows instead of rebuilding game rows.
bs_k_units <- function(u) {
  u <- u[ni > 1 & mi > 0]
  xi <- u$si / u$mi
  epv <- sum(u$qi - u$si^2 / u$mi) / sum(u$ni - 1)
  mt <- sum(u$mi); xb <- sum(u$si) / mt
  vhm <- (sum(u$mi * (xi - xb)^2) - (nrow(u) - 1) * epv) / (mt - sum(u$mi^2) / mt)
  c(k = if (is.finite(vhm) && vhm > 0) epv / vhm else Inf, epv = epv, vhm = vhm, mean = xb)
}

# Pregame relative factor R for a rate, per row of d (playerId, season, date,
# gameId, num, E = the prior's expected count that game). Buhlmann on the
# player's recency-weighted counts against what his priors expected:
#   carry:  R = (S_num + kE) / (S_E + kE), weights run across seasons
#   reset:  within the season, shrunk to R0 = 1 + keep (last season's credible R - 1)
ratio_R <- function(d, kE, h, carry = TRUE, keep = 2 / 3) {
  dec <- if (is.finite(h)) 0.5^(1 / h) else 1
  o <- order(d$playerId, d$date, d$gameId)
  x <- d[o, .(playerId, season, num, E)]
  if (carry) {
    x[, `:=`(S_num = ew_lag(num, dec), S_E = ew_lag(E, dec)), by = playerId]
    x[, R := if (is.finite(kE)) (S_num + kE) / (S_E + kE) else 1]
  } else {
    ls <- x[, .(N = sum(num), EE = sum(E)), by = .(playerId, season)]
    ls[, `:=`(season = season + 10001L, R0 = 1 + keep * ((N + kE) / (EE + kE) - 1))]
    x[ls, on = .(playerId, season), R0 := i.R0]; x[is.na(R0), R0 := 1]
    x[, `:=`(S_num = ew_lag(num, dec), S_E = ew_lag(E, dec)), by = .(playerId, season)]
    x[, R := if (is.finite(kE)) (S_num + kE * R0) / (S_E + kE) else 1]
  }
  R <- numeric(nrow(d)); R[o] <- x$R; R
}
