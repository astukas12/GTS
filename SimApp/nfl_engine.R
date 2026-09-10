# =============================================================================
# nfl_engine.R -- NFL, DraftKings + FanDuel, classic + showdown
# -----------------------------------------------------------------------------
# SIMULATE A REAL SUNDAY, THEN DEAL IT OUT. Every simulated game here is an
# actual historical NFL game -- BOTH TEAMS, as played -- drawn from a
# 1,359-game pool (2021-2025 REG) matched on both teams' pre-game profiles and
# calibrated to the market. Its completions, designed runs, sacks and field
# goals are dealt to this slate's players one event at a time.
#
# Because events are DEALT rather than shared out, player totals sum to the team
# line by construction -- there is no reconciliation step in this file. Blowout
# substitution needs no model: the sampled game carries its own score, so a
# flattened backfield arrives already correlated with the margin that caused it.
#
# NO ERA LOGIC HERE. The event pool (slim_<year>_era.rds) is already detrended
# to 2026-equivalent units at build time (GTS/NFL/R/build_templates.R, BUILD
# QUEUE part 1). The engine reads it raw and carries no era knob. The pool it
# draws from is correct by construction.
#
# PLATFORM-NEUTRAL SIM, SCORING AT THE END. The deal produces raw stat lines
# (pass / rush / rec yards, TDs, receptions, carries, sack yards, INT, the DST
# components, kicking). Scoring is a separate layer: nfl_score_lines(A, "DK")
# is full PPR (1.0 / rec) with the yardage bonuses and a DST slot; "FD" is half
# PPR (0.5 / rec), no bonuses, a K slot. TD and yardage values are shared.
#
# CLASSIC vs SHOWDOWN: the SAME sim. Showdown is one game; classic loops the
# two-team core over N games in a shared SimID space. The CPT (DK) / MVP (FD)
# 1.5x multiplier is the optimiser's job -- this engine emits the UTIL score.
# Kicker + DST are eligible in showdown on both sites.
#
# WHAT THE SHEET SUPPLIES, AND NOTHING ELSE (see GTS/NFL/R/slate_sheet.R):
#   pass_share    who throws (and takes the drawn game's interceptions)
#   0-2 / 3-7 / 8-15 / 16-30 / 31+
#                 P(he caught it | the catch went that far). One column per
#                 completion-yardage band, each summing to 1 down the team.
#                 THIS is the deal -- no likelihood, no tilt, no league mix.
#   rz_tgt_share  P(he is the target | the completion was inside the 20). A
#                 SECOND player vector, dealt against only for rz events. Blank
#                 = the positional multiplier (TE 1.25 / RB 1.01 / WR 0.91).
#   carry_usage   P(handed any given NORMAL designed carry). Sums to 1.
#   sy_share      P(handed a short-yardage carry: dn>=3 & dist<=2). Blank = carry_usage.
#   gl_share      P(handed a goal-line carry: ytg<=3). Blank = carry_usage.
#   kicker / punt_returner / kick_returner / dst   one name / identity each
#   pys_target    the pass-yard share of scrimmage to ASK THE POOL for
#
# WHAT COMES FROM THE DRAWN GAME AND TAKES NO INPUT: passing yards, attempts,
# interceptions (read off the opponent's drawn defensive box), field goal
# distances and results, every DST counting stat, points allowed, team fumbles
# lost. Each was tested for player signal and found to have little or none
# (QB INT split-half 0.125, kicker FG% -0.04, README "Turnovers and kicking").
#
# SACKS have no offensive-side effect: a sacked QB loses nothing here (rushing
# and passing lines are untouched), and the only sack scoring is +1 per sack to
# the opponent DST, credited from that team's drawn defensive box.
#
# TOUCHDOWNS ARE NEVER ALLOCATED. A TD is a property of the catch or the carry
# that gets dealt. Whoever receives the end-zone event scores. Return TDs come
# from the drawn game's defensive box and credit BOTH the DST row and the
# designated returner (the same event, two rosters -- never a second draw).
#
# DATA: NFL_DATA_DIR / NFL_DB_DIR below. ~1.7MB of rds, built by GTS/NFL/R out
# of play-by-play that never ships. See nfl_data_dir() for the search order.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# ---- where the pool lives ---------------------------------------------------
# Search order, first hit wins:
#   1. options(nfl.data_dir = ...) / options(nfl.db_dir = ...)   -- the test sets this
#   2. SimApp/nfl_data/  (relative)                              -- future packaging (part 12)
#   3. the GTS/NFL working tree                                  -- dev fallback
nfl_data_dir <- function() {
  o <- getOption("nfl.data_dir")
  if (!is.null(o) && dir.exists(o)) return(o)
  if (dir.exists("nfl_data") && file.exists(file.path("nfl_data", "slim_2025_era.rds")))
    return("nfl_data")
  "C:/Users/astuk/OneDrive/Documents/GTS/NFL/data"
}
nfl_db_dir <- function() {
  o <- getOption("nfl.db_dir")
  if (!is.null(o) && dir.exists(o)) return(o)
  if (dir.exists("nfl_data") && file.exists(file.path("nfl_data", "nfl_dst_box.rds")))
    return("nfl_data")
  "C:/Users/astuk/OneDrive/Documents/GTS/NFL/db"
}

NFL_SEASONS <- 2021:2025

# =============================================================================
# CONSTANTS -- every one measured on 2021-2025 REG (GTS/NFL, parts 1-9)
# =============================================================================

# Event kind codes, as build_templates.R writes them (identical to CFB).
NFL_EVT_SACK <- 1L; NFL_EVT_FG <- 2L; NFL_EVT_RUN <- 3L; NFL_EVT_CMP <- 4L

# Six pool dimensions, same SET as CFB. LEVEL = total, absp (the market).
# COMPOSITION = each side's pass rate and pass share of scrimmage yards. NFL
# teams are ~3x more alike on composition than CFB teams, so market leans wider:
# 1.5 / 0.7 vs CFB's 1.4 / 1.0. Re-fit on the LOO backtest is BUILD QUEUE part 4.
NFL_POOL_DIMS      <- c("total", "absp", "fO_pr", "fO_pys", "dO_pr", "dO_pys")
NFL_POOL_W         <- c(1.5, 1.5, 0.7, 0.7, 0.7, 0.7)
NFL_POOL_BW        <- 0.9
NFL_ESS_FLOOR      <- 150
NFL_ESS_HARDFLOOR  <- 60

# Completion yardage bands + the league's own mix, era-adjusted pool.
NFL_BAND_EDGES     <- c(-Inf, 2, 7, 15, 30, Inf)
NFL_BAND_COLS      <- c("0-2", "3-7", "8-15", "16-30", "31+")
NFL_BAND_MID       <- c(0, 5, 11, 22, 45)
NFL_LEAGUE_BAND_MIX<- c(0.1118, 0.3299, 0.3435, 0.1688, 0.0460)

# Per-position 5-band base mix -- fallback for an untyped catcher / an empty band.
NFL_BASE_MIX <- list(
  WR = c(.079, .300, .360, .187, .074),
  TE = c(.090, .330, .350, .180, .050),
  RB = c(.210, .360, .290, .105, .035))

# Backfield latent draw: Dirichlet(a0 * carry_usage) once per simulated game,
# NORMAL carries only. NFL carries run 4.08x multinomial (CFB 2.78x) on fewer
# carries a game -- both push a0 well below CFB's 30. Tuned to the 4.08x
# headline on 8,000 pool draws (PART5_SHARE_CURVES_REPORT.md).
NFL_CARRY_A0 <- 6.5

# Catch dispersion: a mild per-(sim,band) Dirichlet jitter, NEW for NFL (CFB
# dealt flat). NFL receptions run ~1.15-1.33x multinomial; a0 = 60 lands
# realised total-reception dispersion ~1.17x. a0 = Inf collapses to flat.
NFL_CATCH_A0 <- 60

# Blank-fallback red-zone positional multiplier on open-field target share.
NFL_RZ_POS_FACTOR <- c(WR = 0.91, TE = 1.25, RB = 1.01)

# Per-touch fumble-lost weights, CARRIED FROM CFB (9,932 team-games) pending an
# NFL re-measure (BUILD QUEUE part 14 / open item #8). The QB's is ~4x a back's
# because his come from sacks and snaps; his weight rides on dropbacks. The
# drawn game supplies the COUNT (opponent defensive box); this only spreads it.
NFL_FUM_RATE <- c(QB = .0244, WR = .0078, TE = .0078, RB = .0056, K = 0, DST = 0)

# =============================================================================
# DK / FD SCORING TABLES
# -----------------------------------------------------------------------------
# TD and yardage values are shared. What differs: reception weight (DK 1.0 /
# FD 0.5) and the fumble-lost penalty (DK -1 / FD -2). The 300-pass / 100-rush /
# 100-rec game bonuses are +3 on BOTH sites.
#
# FANDUEL TABLE -- reconciled against FanDuel's live "Rules & Scoring" panel
# for the Week-1 NE @ SEA slate (Sept 2026). Confirmed there: the +3 yardage
# bonuses (AnyFLEX tier; 4.5 at MVP = flat 1.5x), fumble lost = -2, FG bands
# 0-39 / 40-49 / 50+ -> 3 / 4 / 5, reception 0.5, XP made = 1. FanDuel's panel
# shows no missed-XP penalty; xp_miss = -1 is kept but stays dormant (v1
# approximates XP made = offensive TD count, so no misses are generated).
# =============================================================================
NFL_SCORE <- list(
  DK = list(
    pass_yd = 0.04, pass_td = 4, interception = -1, pass_300 = 3,
    rush_yd = 0.10, rush_td = 6, rush_100 = 3,
    rec = 1.0, rec_yd = 0.10, rec_td = 6, rec_100 = 3,
    fumble_lost = -1, return_td = 6,
    xp = 1, xp_miss = 0),
  FD = list(
    pass_yd = 0.04, pass_td = 4, interception = -1, pass_300 = 3,
    rush_yd = 0.10, rush_td = 6, rush_100 = 3,
    rec = 0.5, rec_yd = 0.10, rec_td = 6, rec_100 = 3,
    fumble_lost = -2, return_td = 6,
    xp = 1, xp_miss = -1))

# DST points-allowed tier -- identical on DK and FD (README "Scoring").
nfl_dst_pa_tier <- function(pa) {
  data.table::fifelse(pa <= 0, 10,
    data.table::fifelse(pa <= 6, 7,
    data.table::fifelse(pa <= 13, 4,
    data.table::fifelse(pa <= 20, 1,
    data.table::fifelse(pa <= 27, 0,
    data.table::fifelse(pa <= 34, -1, -4))))))
}

# DST fantasy points. Sack +1, INT +2, fumble recovery +2, TD (any) +6,
# safety +2, blocked kick +2, plus the PA tier. Same on both sites.
nfl_dst_score <- function(x) {
  g <- function(nm) { v <- x[[nm]]; if (is.null(v)) 0 else data.table::fifelse(is.na(v), 0, v) }
  g("def_sacks") * 1 + g("def_int") * 2 + g("def_fum_rec") * 2 +
    g("def_td") * 6 + g("def_safety") * 2 + g("def_block") * 2 +
    nfl_dst_pa_tier(g("pa"))
}

# =============================================================================
# SMALL HELPERS
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
nfl_num <- function(x) { x <- suppressWarnings(as.numeric(x)); data.table::fifelse(is.na(x), 0, x) }
nfl_ess <- function(w) { w <- w / sum(w); 1 / sum(w^2) }

# band index 1..5 for a completion yardage (matches build_templates.R::band_of).
nfl_band_of <- function(y) as.integer(cut(y, NFL_BAND_EDGES, labels = FALSE, right = TRUE))

# DK / FD field-goal points by attempt. Distance = yards_to_goal + 17 (10 for
# the end zone, 7 for the snap). Bands 0-39 / 40-49 / 50+ -> 3 / 4 / 5, same on
# both sites. Take the makes as they happened -- do not re-roll.
nfl_fg_points <- function(ytg) { d <- ytg + 17
  data.table::fifelse(d < 40, 3, data.table::fifelse(d < 50, 4, 5)) }

# Per-game Dirichlet winners, vectorised (inverse-CDF over a cumulative
# n_sims x nP weight matrix). Within a simulated game every event dealt through
# one call shares a single Dirichlet(a0 * base_p) draw -- this is what creates
# the game-to-game "featured or not" swing. a0 = Inf -> plain multinomial.
nfl_dirichlet_winners <- function(sim_idx, base_p, a0, n_sims) {
  nP <- length(base_p)
  if (nP == 1L) return(rep(1L, length(sim_idx)))
  if (!length(sim_idx)) return(integer(0))
  if (!is.finite(a0))
    return(sample.int(nP, length(sim_idx), replace = TRUE, prob = base_p))
  g <- matrix(stats::rgamma(n_sims * nP,
                            shape = rep(pmax(a0 * base_p, 1e-6), each = n_sims)),
              nrow = n_sims, ncol = nP)
  g <- g / rowSums(g)
  for (j in 2:nP) g[, j] <- g[, j - 1L] + g[, j]
  u <- stats::runif(length(sim_idx))
  wv <- rep(1L, length(sim_idx))
  for (j in seq_len(nP - 1L)) wv <- wv + (g[cbind(sim_idx, j)] < u)
  wv
}

# =============================================================================
# POOL MATCHING  (inlined from GTS/NFL/R/build_pool.R -- CFB's mechanism)
# =============================================================================

# One row per GAME: favourite side (f*) and underdog side (d*), priors + outcomes.
nfl_pool_frame <- function(seasons = NFL_SEASONS) {
  P <- readRDS(file.path(nfl_data_dir(), "nfl_profiles_with_priors.rds")); setDT(P)
  P <- P[season %in% seasons & !is.na(spread) & !is.na(total)]
  P[, is_fav := (is_home & spread > 0) | (!is_home & spread < 0)]   # + spread = home favoured
  P <- P[, if (.N == 2 && sum(is_fav) == 1) .SD, by = game_id]

  side <- function(d, pre) {
    s <- d[, .(game_id, team, season, total, absp = abs(spread), neutral,
               O_pr = pri_pass_rate, O_pys = pri_pyd_share,
               D_pr = pri_d_pr,      D_pys = pri_d_pys,
               points, yds,
               cmp, pass_yds, pass_td, carries, rush_yds, rush_td, sacks, sack_yds)]
    nm <- setdiff(names(s), c("game_id", "season", "total", "absp", "neutral"))
    setnames(s, nm, paste0(pre, nm)); s
  }
  Fs <- side(P[is_fav == TRUE],  "f")
  Ds <- side(P[is_fav == FALSE], "d")[, .SD, .SDcols = !c("season", "total", "absp", "neutral")]
  G  <- merge(Fs, Ds, by = "game_id")
  G[, `:=`(pts_sum = fpoints + dpoints, margin = fpoints - dpoints)]
  setnames(G, c("fpoints", "dpoints"), c("ptsF", "ptsD"))
  G[complete.cases(G[, ..NFL_POOL_DIMS])]
}

nfl_wmean <- function(v, w) { ok <- is.finite(v)
  if (!any(ok)) return(NA_real_); sum(v[ok] * w[ok]) / sum(w[ok]) }

# Raw kernel weights over the pool. Soft weights only -- no hard filters.
nfl_pool_weights <- function(G, target, bw = NFL_POOL_BW, weights = NFL_POOL_W) {
  tgt <- unlist(target)[NFL_POOL_DIMS]
  if (anyNA(tgt)) stop("pool target missing: ",
                       paste(NFL_POOL_DIMS[is.na(tgt)], collapse = ", "))
  M  <- as.matrix(G[, ..NFL_POOL_DIMS])
  mu <- colMeans(M); sg <- apply(M, 2, sd)
  Mz <- sweep(sweep(M, 2, mu, "-"), 2, sg, "/")
  tz <- (tgt - mu) / sg
  d2 <- rowSums(sweep((sweep(Mz, 2, tz, "-"))^2, 2, weights, "*"))
  w  <- exp(-d2 / (2 * bw^2)); w[!is.finite(w)] <- 0; w <- w / sum(w)
  list(w = w, ess = nfl_ess(w), n = nrow(G), bw = bw, weights = weights)
}

# Damped fixed point on the two MARKET dims only. Style targets are your read
# on the teams, not something to solve for. Ported verbatim from CFB.
nfl_calibrate_target <- function(G, target, market, bw = NFL_POOL_BW,
                                 weights = NFL_POOL_W, iters = 12, damp = 0.8,
                                 tol = 0.05) {
  tg <- target
  tlo <- min(G$total); thi <- max(G$total); shi <- max(G$absp)
  for (k in seq_len(iters)) {
    w  <- nfl_pool_weights(G, tg, bw = bw, weights = weights)$w
    et <- nfl_wmean(G$pts_sum, w); em <- nfl_wmean(G$margin, w)
    if (!is.finite(et) || !is.finite(em)) break
    dt <- market$total - et; dm <- market$margin - em
    if (max(abs(c(dt, dm))) < tol) break
    tg$total <- min(max(tg$total + damp * dt, tlo), thi)
    tg$absp  <- min(max(tg$absp  + damp * dm, 0),   shi)
  }
  r <- nfl_pool_weights(G, tg, bw = bw, weights = weights)
  list(target = tg, ess = r$ess, iters = k,
       total = nfl_wmean(G$pts_sum, r$w), margin = nfl_wmean(G$margin, r$w))
}

# THE ESS GUARD -- relax or refuse, never warn-and-proceed. Ladder: composition
# widens first, bandwidth next, market last; re-calibrate at every rung; stop at
# the first rung >= floor; refuse (stop()) if the ladder is spent below the hard
# floor. Ported from GTS/NFL/R/build_pool.R.
nfl_pool_weights_guarded <- function(G, target, market,
                                     floor_ess = NFL_ESS_FLOOR,
                                     hard_ess  = NFL_ESS_HARDFLOOR,
                                     bw = NFL_POOL_BW, verbose = TRUE) {
  w0 <- NFL_POOL_W
  rungs <- list(
    list(lab = "as asked",               w = w0,                          bw = bw),
    list(lab = "dO weight x0.55",         w = w0 * c(1,1,1,1,.55,.55),      bw = bw),
    list(lab = "fO+dO weight x0.55",      w = w0 * c(1,1,.55,.55,.55,.55),  bw = bw),
    list(lab = "fO+dO weight x0.30",      w = w0 * c(1,1,.30,.30,.30,.30),  bw = bw),
    list(lab = "style x0.30, bw x1.3",    w = w0 * c(1,1,.30,.30,.30,.30),  bw = bw * 1.3),
    list(lab = "style x0.30, bw x1.7",    w = w0 * c(1,1,.30,.30,.30,.30),  bw = bw * 1.7),
    list(lab = "style x0.30, bw x1.7, mkt x0.75",
         w = w0 * c(.75,.75,.30,.30,.30,.30), bw = bw * 1.7),
    list(lab = "style x0.30, bw x1.7, mkt x0.55",
         w = w0 * c(.55,.55,.30,.30,.30,.30), bw = bw * 1.7))
  tried <- list()
  for (i in seq_along(rungs)) {
    rg  <- rungs[[i]]
    cal <- nfl_calibrate_target(G, target, market, bw = rg$bw, weights = rg$w)
    r   <- nfl_pool_weights(G, cal$target, bw = rg$bw, weights = rg$w)
    tried[[i]] <- data.table(rung = i - 1L, move = rg$lab, ess = r$ess,
                             total = cal$total, margin = cal$margin)
    if (r$ess >= floor_ess) {
      if (verbose && i > 1) {
        cat(sprintf("[nfl] ESS guard: relaxed to rung %d (%s) -- ESS %.0f\n",
                    i - 1L, rg$lab, r$ess)); print(rbindlist(tried), digits = 4)
      }
      return(list(w = r$w, ess = r$ess, n = r$n, bw = rg$bw, target = cal$target,
                  total = cal$total, margin = cal$margin,
                  relaxed = i > 1L, rung = i - 1L, ladder = rbindlist(tried)))
    }
  }
  best <- rbindlist(tried)
  if (verbose) { cat("[nfl] ESS guard: ladder exhausted --\n"); print(best, digits = 4) }
  if (max(best$ess) < hard_ess)
    stop(sprintf("REFUSED: NFL pool ESS tops out at %.0f (hard floor %d) for this ask -- ",
                 max(best$ess), hard_ess),
         "no comparable game in the pool. Widen the output by hand or add the ",
         "2019-2020 low-weight tail (BUILD QUEUE part 4).")
  i <- which.max(best$ess); rg <- rungs[[i]]
  cal <- nfl_calibrate_target(G, target, market, bw = rg$bw, weights = rg$w)
  r   <- nfl_pool_weights(G, cal$target, bw = rg$bw, weights = rg$w)
  list(w = r$w, ess = r$ess, n = r$n, bw = rg$bw, target = cal$target,
       total = cal$total, margin = cal$margin, relaxed = TRUE, rung = i - 1L, ladder = best)
}

# =============================================================================
# THE ALLOCATORS  (inlined from GTS/NFL/R/share_curves.R + rz_role.R + dst.R,
# runtime paths only -- the DB builders and split-half harnesses stay in the
# GTS/NFL tree. Numbers here MUST match those files.)
# =============================================================================

# (nP x 5) P(player | catch in band b) from a roster carrying the 5 band cols.
# A band nobody is typed into falls back to `usage` (or a flat split).
nfl_catch_pb <- function(R, band_cols = NFL_BAND_COLS) {
  nR <- nrow(R); if (!nR) return(matrix(0, 0, 5L))
  M <- as.matrix(R[, ..band_cols]); M[!is.finite(M)] <- 0
  fallback <- if ("usage" %in% names(R) && sum(R$usage, na.rm = TRUE) > 0) R$usage else rep(1 / nR, nR)
  cs <- colSums(M)
  for (b in seq_len(5L)) if (cs[b] <= 0) M[, b] <- fallback
  sweep(M, 2, colSums(M), "/")
}

# list(normal, sy, gl) carry-probability vectors. Blank / 0 sy_share|gl_share
# falls back to carry_usage.
nfl_carry_pb <- function(S) {
  cu <- as.numeric(S$carry_usage); cu[!is.finite(cu)] <- 0
  pick <- function(col) {
    v <- if (col %in% names(S)) as.numeric(S[[col]]) else rep(NA_real_, nrow(S))
    v[!is.finite(v) | v == 0] <- NA_real_
    ifelse(is.na(v), cu, v)
  }
  norm1 <- function(p) { s <- sum(p); if (s <= 0) rep(1 / length(p), length(p)) else p / s }
  list(normal = norm1(cu), sy = norm1(pick("sy_share")), gl = norm1(pick("gl_share")))
}

# P(player | the completion was flagged inside-20). Typed rz_tgt_share wins;
# blank = positional multiplier on open-field target share, renormalised.
nfl_rz_pb <- function(R, k_rz = 12) {
  nR <- nrow(R); if (!nR) return(numeric(0))
  pos <- as.character(R$pos); pos[is.na(pos) | !pos %in% names(NFL_RZ_POS_FACTOR)] <- "WR"
  if (all(NFL_BAND_COLS %in% names(R))) {
    M <- as.matrix(R[, ..NFL_BAND_COLS]); M[!is.finite(M)] <- 0; s <- rowSums(M)
  } else if ("usage" %in% names(R)) s <- as.numeric(R$usage) else s <- rep(1, nR)
  s[!is.finite(s) | s < 0] <- 0
  s <- if (sum(s) <= 0) rep(1 / nR, nR) else s / sum(s)
  seed <- s * NFL_RZ_POS_FACTOR[pos]; seed <- seed / sum(seed)
  raw <- seed
  if ("rz_tgt_share" %in% names(R)) {
    tv <- as.numeric(R$rz_tgt_share); ok <- is.finite(tv) & tv > 0
    raw[ok] <- tv[ok]
  }
  raw[!is.finite(raw) | raw < 0] <- 0
  if (sum(raw) <= 0) return(rep(1 / nR, nR))
  raw / sum(raw)
}

# deal a drawn game's completion events to pass-catchers, CATCH BY CATCH, with
# the inside-20 branch spliced in. rz == 0 -> band deal; rz == 1 -> one shared
# per-sim Dirichlet(a0 * pb_rz). Returns per (sim, player): rec, ryds, rtd,
# rec_b5 (31+ readback), rec_rz (inside-20 readback).
nfl_deal_receiving <- function(events, pb, pb_rz, players, a0 = NFL_CATCH_A0, n_sims) {
  nP <- length(players)
  E <- data.table::as.data.table(events)
  empty <- data.table(sim = integer(0), player = character(0), rec = integer(0),
                       ryds = numeric(0), rtd = numeric(0),
                       rec_b5 = integer(0), rec_rz = integer(0))
  if (!nrow(E) || !nP) return(empty)
  E <- E[is.finite(sim)]
  rzc  <- if ("rz" %in% names(E)) E$rz else rep(0L, nrow(E))
  E_of <- E[is.na(rzc) | rzc != 1L]
  E_rz <- E[!is.na(rzc) & rzc == 1L]

  # open field: band by band, one Dirichlet draw per (sim, band)
  of <- empty[0]
  if (nrow(E_of)) {
    E_of <- data.table::copy(E_of)
    E_of[, b := nfl_band_of(yds)][, w := NA_integer_]
    for (bb in seq_len(5L)) {
      ii <- which(E_of$b == bb)
      if (length(ii))
        data.table::set(E_of, ii, "w", nfl_dirichlet_winners(E_of$sim[ii], pb[, bb], a0, n_sims))
    }
    of <- E_of[, .(rec = .N, ryds = sum(yds), rtd = sum(td == 1L, na.rm = TRUE),
                   rec_b5 = sum(b == 5L)), by = .(sim, w)]
    of[, player := players[w]][, w := NULL][, rec_rz := 0L]
  }
  # inside 20: one shared per-sim player vector
  rz <- empty[0]
  if (nrow(E_rz)) {
    E_rz <- data.table::copy(E_rz)
    E_rz[, w := nfl_dirichlet_winners(sim, pb_rz, a0, n_sims)]
    rz <- E_rz[, .(rec = .N, ryds = sum(yds), rtd = sum(td == 1L, na.rm = TRUE),
                   rec_b5 = sum(nfl_band_of(yds) == 5L)), by = .(sim, w)]
    rz[, player := players[w]][, w := NULL][, rec_rz := rec]
  }
  both <- data.table::rbindlist(list(of, rz), use.names = TRUE)
  both[, .(rec = sum(rec), ryds = sum(ryds), rtd = sum(rtd),
           rec_b5 = sum(rec_b5), rec_rz = sum(rec_rz)), by = .(sim, player)]
}

# deal a drawn game's designed runs to runners (QB included), CARRY BY CARRY.
# Three situations read off the EVENT: goal line (gl==1) first, then short
# yardage (sy==1), then normal. Normal carries get the per-game Dirichlet.
nfl_deal_rushing <- function(events, shares, players, a0 = NFL_CARRY_A0, n_sims) {
  nP <- length(players)
  E <- data.table::as.data.table(events)
  if (!nrow(E) || !nP)
    return(data.table(sim = integer(0), player = character(0), car = integer(0),
                      cyds = numeric(0), ctd = numeric(0)))
  E <- data.table::copy(E[is.finite(sim)])
  gl <- if ("gl" %in% names(E)) E$gl else 0L
  sy <- if ("sy" %in% names(E)) E$sy else 0L
  E[, sit := data.table::fifelse(!is.na(gl) & gl == 1L, 3L,
             data.table::fifelse(!is.na(sy) & sy == 1L, 2L, 1L))]
  P <- nfl_carry_pb(shares)
  E[, w := NA_integer_]
  for (q in 2:3) {
    ii <- which(E$sit == q)
    if (length(ii))
      data.table::set(E, ii, "w", sample.int(nP, length(ii), replace = TRUE,
                                             prob = if (q == 2L) P$sy else P$gl))
  }
  ii <- which(E$sit == 1L)
  if (length(ii))
    data.table::set(E, ii, "w", nfl_dirichlet_winners(E$sim[ii], P$normal, a0, n_sims))
  out <- E[, .(car = .N, cyds = sum(yds), ctd = sum(td == 1L, na.rm = TRUE)), by = .(sim, w)]
  out[, player := players[w]][, w := NULL][]
}

# =============================================================================
# READING THE SHEET  -- one tab per team + a `game` tab, readxl (SimApp parity).
# Mirrors GTS/NFL/R/slate_sheet.R::read_slate_sheet in shape: the player table
# runs from column A up to the kicker / returners / DST identity field/value
# block at column S, `pys_target` melted onto the team table from the game tab.
# =============================================================================
NFL_PLAYER_COLS <- c("player", "route_base", "pass_share",
                     "0-2", "3-7", "8-15", "16-30", "31+", "rz_tgt_share",
                     "carry_usage", "sy_share", "gl_share", "availability")
NFL_TEAM_FIELDS <- c("kicker", "punt_returner", "kick_returner", "dst", "notes")
NFL_NUM_COLS    <- c("pass_share", NFL_BAND_COLS, "rz_tgt_share",
                     "carry_usage", "sy_share", "gl_share")

read_nfl_input <- function(file_path, slate = NULL, game = NULL) {
  sh <- readxl::excel_sheets(file_path)
  gtab <- sh[tolower(sh) == "game"]
  if (!length(gtab)) stop("NFL workbook needs a `game` tab")
  g <- as.data.table(readxl::read_excel(file_path, sheet = gtab[1]))
  for (nm in c("date", "slate_id", "away", "home", "market_source", "notes", "slate_type"))
    if (nm %in% names(g)) g[[nm]] <- as.character(g[[nm]])
  if ("start_order" %in% names(g)) g[, start_order := suppressWarnings(as.integer(start_order))]
  if (!"slate_type" %in% names(g) || is.na(g$slate_type[1]) || !nzchar(g$slate_type[1]))
    g[, slate_type := if (nrow(g) == 1L) "showdown" else "classic"]
  g[, slate_type := tolower(as.character(slate_type))]

  aux <- sh[tolower(sh) %in% c("projections", "etr")]
  tms <- setdiff(sh, c(gtab, aux))

  read_tab <- function(tm) {
    x <- as.data.table(readxl::read_excel(file_path, sheet = tm,
                                          .name_repair = "unique_quiet"))
    fi <- which(names(x) == "field")
    list(x = x, fi = if (length(fi)) fi[1] else NA_integer_)
  }
  tabs <- setNames(lapply(tms, read_tab), tms)

  # player block: everything left of the `field`/`value` team block
  pl <- rbindlist(lapply(tms, function(tm) {
    z <- tabs[[tm]]; x <- z$x
    if (!is.na(z$fi)) x <- x[, seq_len(z$fi - 1L), with = FALSE]
    x <- x[, !startsWith(names(x), "..."), with = FALSE]
    for (nm in setdiff(NFL_PLAYER_COLS, names(x)))
      x[, (nm) := if (nm %in% NFL_NUM_COLS) NA_real_ else NA_character_]
    x <- x[!is.na(player) & nzchar(as.character(player))]
    x[, team := tm][]
  }), fill = TRUE)

  # team block: the field/value pair
  tt <- rbindlist(lapply(tms, function(tm) {
    z <- tabs[[tm]]
    b <- if (is.na(z$fi)) data.table(field = character(), value = character())
         else z$x[, z$fi + 0:1, with = FALSE]
    setnames(b, c("field", "value")); b <- b[!is.na(field)]
    o <- as.list(setNames(as.character(b$value), b$field))
    data.table(team = tm,
               kicker        = o$kicker        %||% NA_character_,
               punt_returner = o$punt_returner %||% NA_character_,
               kick_returner = o$kick_returner %||% NA_character_,
               dst           = o$dst           %||% NA_character_,
               notes         = o$notes         %||% NA_character_)
  }), fill = TRUE)
  tt[is.na(dst) | !nzchar(trimws(dst)), dst := team]

  # melt pys_target + derive the DST opponent from the game tab
  tt[, `:=`(pys_target = NA_real_, dst_opp = NA_character_)]
  for (i in seq_len(nrow(tt))) {
    tm <- tt$team[i]; row <- g[away == tm | home == tm][1]
    if (nrow(row)) {
      tt$pys_target[i] <- if (identical(row$away, tm)) suppressWarnings(as.numeric(row$pys_target_away))
                          else                          suppressWarnings(as.numeric(row$pys_target_home))
      tt$dst_opp[i]    <- if (identical(row$away, tm)) row$home else row$away
    }
  }

  # numeric coercion + blank-share defaults (blank sy/gl -> carry_usage)
  setDT(pl)
  for (cl in NFL_NUM_COLS) if (cl %in% names(pl)) set(pl, j = cl, value = suppressWarnings(as.numeric(pl[[cl]])))
  for (cl in NFL_BAND_COLS) pl[is.na(get(cl)), (cl) := 0]
  pl[is.na(pass_share),  pass_share  := 0]
  pl[is.na(carry_usage), carry_usage := 0]
  pl[is.na(route_base) | route_base == "", route_base := "WR"]
  pl[, availability := tolower(trimws(ifelse(is.na(availability), "", availability)))]
  pl[is.na(sy_share) | sy_share == 0, sy_share := carry_usage]
  pl[is.na(gl_share) | gl_share == 0, gl_share := carry_usage]
  # a man flagged `out` holds no share (the sheet validator guarantees this;
  # belt-and-braces here so a hand-edited sheet can't smuggle one in).
  if (any(pl$availability == "out")) {
    for (cl in c("pass_share", "carry_usage", "sy_share", "gl_share", NFL_BAND_COLS,
                 "rz_tgt_share"))
      pl[availability == "out", (cl) := 0]
  }

  prj <- NULL
  hit <- sh[tolower(sh) == "projections"]
  if (length(hit)) {
    prj <- as.data.table(readxl::read_excel(file_path, sheet = hit[1]))
    setnames(prj, tolower(names(prj)))
    if (!"player" %in% names(prj) && "name" %in% names(prj)) setnames(prj, "name", "player")
  }

  list(game = g, team = tt, players = pl, projections = prj)
}

# a lightweight menu for a slate picker. NFL v1 has no multi-slate workbook, so
# this is always NULL (the caller shows no picker). Mirrors cfb_slate_menu.
nfl_slate_menu <- function(file_path) NULL

# =============================================================================
# SCORING  -- platform-neutral stat lines in, DK / FD fantasy points out.
# `A` carries, per (SimID, player): rec ryds rtd  car cyds ctd  pyds ptd pint
# fgp xp rettd fum  and, for the DST row, def_sacks def_int def_fum_rec def_td
# def_block def_safety pa (is_dst == TRUE). Returns a numeric vector.
# =============================================================================
nfl_score_lines <- function(A, platform = c("DK", "FD")) {
  platform <- match.arg(platform)
  s  <- NFL_SCORE[[platform]]
  is_dst <- if ("is_dst" %in% names(A)) A$is_dst %in% TRUE else rep(FALSE, nrow(A))
  off <- with(A,
    rec * s$rec + ryds * s$rec_yd + rtd * s$rec_td +
    cyds * s$rush_yd + ctd * s$rush_td +
    pyds * s$pass_yd + ptd * s$pass_td + pint * s$interception +
    data.table::fifelse(ryds >= 100, s$rec_100, 0) +
    data.table::fifelse(cyds >= 100, s$rush_100, 0) +
    data.table::fifelse(pyds >= 300, s$pass_300, 0) +
    fgp + xp * s$xp + rettd * s$return_td + fum * s$fumble_lost)
  dst <- nfl_dst_score(A)
  data.table::fifelse(is_dst, dst, off)
}

# =============================================================================
# THE TWO-TEAM CORE  -- used directly for a showdown slate, and looped by
# run_nfl_classic_simulation for a full slate.
# =============================================================================
run_nfl_simulation <- function(input_data, n_sims = 10000, config = NULL,
                               progress_callback = NULL, keep_components = FALSE,
                               seed = NULL, .slate_type = NULL) {
  say <- function(msg, frac = NULL) {
    if (is.function(progress_callback)) try(progress_callback(msg, frac), silent = TRUE)
    message("[nfl] ", msg)
  }
  if (is.null(n_sims) || is.na(n_sims)) n_sims <- 10000
  n_sims <- as.integer(n_sims)

  G  <- as.data.table(input_data$game)
  TT <- as.data.table(input_data$team)
  PL <- copy(as.data.table(input_data$players))
  slate_type <- .slate_type %||% (if ("slate_type" %in% names(G)) tolower(G$slate_type[1]) else "showdown")

  setDT(PL)
  for (cl in NFL_NUM_COLS) if (cl %in% names(PL)) set(PL, j = cl, value = nfl_num(PL[[cl]]))
  if (!"route_base" %in% names(PL)) PL[, route_base := "WR"]
  PL[is.na(route_base) | route_base == "", route_base := "WR"]
  PL[, pos := fifelse(route_base %in% c("WR", "TE", "RB", "QB", "K", "DST"), route_base, "WR")]
  PL[sy_share == 0, sy_share := carry_usage]
  PL[gl_share == 0, gl_share := carry_usage]
  # `usage` -- DERIVED from the bands (the row's band shares weighted by the
  # league mix). Downstream keys off it; it cancels inside the deal.
  M_ <- as.matrix(PL[, ..NFL_BAND_COLS]); M_[!is.finite(M_)] <- 0
  PL[, usage := as.vector(M_ %*% NFL_LEAGUE_BAND_MIX)]

  # ---- which slate team is the favourite -----------------------------------
  # spread is HOME-RELATIVE, signed, + = home favoured (nflfastR / the NFL
  # sheet convention -- the OPPOSITE of CFB).
  away <- as.character(G$away[1]); home <- as.character(G$home[1])
  spread <- suppressWarnings(as.numeric(G$spread[1]))
  fav <- if (is.finite(spread) && spread < 0) away else home
  dog <- setdiff(c(away, home), fav)
  pys <- setNames(suppressWarnings(as.numeric(TT$pys_target)), TT$team)

  say("loading pool", 0.03)
  Gp <- nfl_pool_frame()
  med_pr <- stats::median(c(Gp$fO_pr, Gp$dO_pr), na.rm = TRUE)
  pys_f <- if (is.finite(pys[[fav]] %||% NA)) pys[[fav]] else stats::median(Gp$fO_pys)
  pys_d <- if (is.finite(pys[[dog]] %||% NA)) pys[[dog]] else stats::median(Gp$dO_pys)
  total  <- suppressWarnings(as.numeric(G$total[1]))
  target <- list(total = total, absp = abs(spread),
                 fO_pr = med_pr, fO_pys = pys_f,
                 dO_pr = med_pr, dO_pys = pys_d)
  r <- nfl_pool_weights_guarded(Gp, target, market = list(total = total, margin = abs(spread)),
                                verbose = FALSE)
  say(sprintf("pool calibrated: ESS %.0f%s, total %.1f, margin %.1f, pys f %.2f d %.2f",
              r$ess, if (r$relaxed) sprintf(" [relaxed to rung %d]", r$rung) else "",
              r$total, r$margin, pys_f, pys_d), 0.08)

  set.seed(if (is.null(seed) || is.na(seed))
             as.integer(Sys.time()) %% .Machine$integer.max else as.integer(seed))
  idx  <- sample.int(nrow(Gp), n_sims, TRUE, prob = r$w)
  draw <- Gp[idx]

  say("loading events", 0.12)
  EV <- rbindlist(lapply(NFL_SEASONS, function(y) {
    x <- readRDS(file.path(nfl_data_dir(), sprintf("slim_%d_era.rds", y))); setDT(x)
    x[, .(game_id, posteam, kind, yds, made, ytg, td, rz, gl, sy)]
  }))
  setkey(EV, game_id, posteam)
  BLK <- EV[, .(s = .I[1], e = .I[.N]), by = .(game_id, posteam)]; setkey(BLK, game_id, posteam)

  DSTB <- readRDS(file.path(nfl_db_dir(), "nfl_dst_box.rds")); setDT(DSTB)
  setkey(DSTB, game_id, def_team)
  dst_cols <- c("def_sacks", "def_int", "def_fum_rec", "def_td", "def_block", "def_safety", "pa", "pf")

  # per-sim DST line for a drawn-game identity (the defence paired with that
  # offence in the real game). NO EXTRA DRAW -- same game_id the offence used.
  dst_for <- function(team_ids) {
    S <- data.table(game_id = draw$game_id, def_team = team_ids)
    D <- DSTB[S, on = .(game_id, def_team)]
    for (cc in setdiff(dst_cols, "pa")) D[is.na(get(cc)), (cc) := 0]
    D[, ret_td := def_td][]
  }
  dl_fav <- dst_for(draw$fteam)   # our fav team's defence in the drawn game
  dl_dog <- dst_for(draw$dteam)

  # ---- per-team setup ------------------------------------------------------
  setup <- lapply(c(fav, dog), function(tm) {
    side_ev  <- if (tm == fav) "f" else "d"
    pool_tm  <- if (tm == fav) draw$fteam else draw$dteam
    tr <- TT[team == tm]
    P  <- PL[team == tm]
    # catchers = any band mass; runners = carry_usage > 0; passers = pass_share > 0
    R <- P[pos %in% c("WR", "TE", "RB")]
    if (nrow(R)) R <- R[rowSums(as.matrix(R[, ..NFL_BAND_COLS])) > 0]
    S <- P[carry_usage > 0]
    Q <- P[pass_share > 0]
    list(tm = tm, side = side_ev, pool_tm = pool_tm,
         rec = R, rsh = S, qbs = Q,
         qb = if (nrow(Q)) Q$player[1] else NA_character_,
         pb    = nfl_catch_pb(R),
         pb_rz = nfl_rz_pb(R),
         k = tr$kicker, pr = tr$punt_returner, kr = tr$kick_returner,
         dst_id = if (!is.na(tr$dst) && nzchar(trimws(tr$dst))) tr$dst else tm,
         dl = if (tm == fav) dl_fav else dl_dog,
         pint_src = if (tm == fav) dl_dog else dl_fav,   # our QB's INTs = opp defence's picks
         who = { w <- unique(c(R$player, S$player, Q$player,
                               tr$kicker, tr$punt_returner, tr$kick_returner))
                 w[!is.na(w) & w != ""] })
  })
  names(setup) <- c(fav, dog)

  say(sprintf("simulating %s games", format(n_sims, big.mark = ",")), 0.2)
  out <- vector("list", length(setup))

  for (si in seq_along(setup)) {
    cf <- setup[[si]]; tm <- cf$tm
    R <- cf$rec; S <- cf$rsh; QB <- cf$qbs
    nR <- nrow(R); nS <- nrow(S); nQ <- nrow(QB)

    pos <- setNames(rep("WR", length(cf$who)), cf$who)
    if (nR) pos[R$player] <- R$pos
    if (nS) pos[S$player] <- fifelse(S$pos == "QB", "QB", "RB")
    if (nQ) pos[QB$player] <- "QB"
    if (!is.na(cf$k)) pos[cf$k] <- "K"
    miss <- setdiff(cf$who, names(pos)); if (length(miss)) pos[miss] <- "WR"

    # ---- gather every drawn game's events for this side, one shot ----------
    sel <- BLK[data.table(game_id = draw$game_id, posteam = cf$pool_tm)][, sim := .I]
    sel[is.na(s), `:=`(s = 1L, e = 0L)]
    lens <- pmax(sel$e - sel$s + 1L, 0L)
    E2 <- EV[rep(sel$s, lens) + sequence(lens) - 1L]
    E2[, sim := rep(sel$sim, lens)]

    cmpE  <- E2[kind == NFL_EVT_CMP,  .(sim, yds, td, rz)]
    runE  <- E2[kind == NFL_EVT_RUN,  .(sim, yds, td, gl, sy)]
    fgv   <- rep(0, n_sims)
    fgg   <- E2[kind == NFL_EVT_FG & !is.na(made) & made == 1L,
                .(fg = sum(nfl_fg_points(ytg))), by = sim]
    if (nrow(fgg)) fgv[fgg$sim] <- fgg$fg

    rec <- if (nR) nfl_deal_receiving(cmpE, cf$pb, cf$pb_rz, R$player, n_sims = n_sims) else NULL
    rsh <- if (nS) nfl_deal_rushing(runE, S[, .(carry_usage, sy_share, gl_share)], S$player, n_sims = n_sims) else NULL

    # ---- assemble the (sim x player) grid --------------------------------
    D <- CJ(sim = seq_len(n_sims), player = cf$who, sorted = FALSE)
    if (!is.null(rec)) D <- merge(D, rec, by = c("sim", "player"), all.x = TRUE)
    if (!is.null(rsh)) D <- merge(D, rsh, by = c("sim", "player"), all.x = TRUE)
    for (cl in c("rec", "ryds", "rtd", "rec_b5", "rec_rz", "car", "cyds", "ctd"))
      if (!cl %in% names(D)) D[, (cl) := 0] else D[is.na(get(cl)), (cl) := 0]
    D[, `:=`(pyds = 0, ptd = 0, pint = 0, fgp = 0, xp = 0, rettd = 0L, fum = 0L,
             is_dst = FALSE,
             def_sacks = 0, def_int = 0, def_fum_rec = 0, def_td = 0,
             def_block = 0, def_safety = 0, pa = NA_real_)]

    # passing line -> the first QB. yards / TDs come from the drawn game (they
    # equal the summed dealt receiving line by construction); INTs come from the
    # opponent's drawn defensive box.
    if (!is.na(cf$qb)) {
      pv  <- draw[[paste0(cf$side, "pass_yds")]]
      ptv <- draw[[paste0(cf$side, "pass_td")]]
      inv <- cf$pint_src$def_int
      D[player == cf$qb, `:=`(pyds = pv[sim], ptd = ptv[sim], pint = inv[sim])]
    }

    # kicker: FG points as they were kicked + one XP per offensive TD (v1
    # approximation -- slim carries no XP event; missed XP / 2pt not modelled).
    if (!is.na(cf$k)) {
      tdv <- D[, .(t = sum(rtd) + sum(ctd)), by = sim]
      xpv <- rep(0, n_sims); xpv[tdv$sim] <- tdv$t
      D[player == cf$k, `:=`(fgp = fgv[sim], xp = xpv[sim])]
    }

    # return TDs: the drawn game's non-offensive return-TD count, credited to
    # ONE returner slot (KR first, else PR) at +6 each -- the same events the
    # DST row also scores. v1 does not split defensive vs ST return TDs.
    ret_slot <- if (!is.na(cf$kr) && nzchar(cf$kr)) cf$kr
                else if (!is.na(cf$pr) && nzchar(cf$pr)) cf$pr else NA_character_
    if (!is.na(ret_slot)) {
      rtv <- cf$dl$ret_td
      D[player == ret_slot, rettd := rettd + rtv[sim]]
    }

    # fumbles lost: the drawn game's count (our fumbles lost = the opponent
    # defence's fumble recoveries), spread across assigned touches by the
    # per-touch weights. Vectorised by cumulative weight within each sim.
    flv <- cf$pint_src$def_fum_rec
    D[, tch := rec + car]
    if (!is.na(cf$qb)) D[player == cf$qb, tch := tch + 25]
    D[, fwt := tch * unname(NFL_FUM_RATE[pos[player]])]
    D[is.na(fwt), fwt := 0]
    setorder(D, sim)
    D[, cw := cumsum(fwt), by = sim]
    tw <- D[, .(tw = max(cw)), by = sim]$tw
    hit <- rep(seq_len(n_sims), pmax(round(flv), 0L))
    hit <- hit[tw[hit] > 0]
    if (length(hit)) {
      nW <- length(cf$who)
      u  <- runif(length(hit)) * tw[hit]
      cwv <- D$cw
      offs <- (hit - 1L) * nW
      w <- offs + vapply(seq_along(hit), function(j)
             which.max(cwv[(offs[j] + 1L):(offs[j] + nW)] >= u[j]), 1L)
      D[, fum := tabulate(w, nbins = nrow(D))]
    }
    D[, c("tch", "fwt", "cw") := NULL]

    # ---- the DST row -----------------------------------------------------
    dl <- cf$dl
    Ddst <- data.table(sim = seq_len(n_sims), player = cf$dst_id,
                       rec = 0, ryds = 0, rtd = 0, rec_b5 = 0, rec_rz = 0,
                       car = 0, cyds = 0, ctd = 0,
                       pyds = 0, ptd = 0, pint = 0, fgp = 0, xp = 0, rettd = 0L, fum = 0L,
                       is_dst = TRUE,
                       def_sacks = dl$def_sacks, def_int = dl$def_int,
                       def_fum_rec = dl$def_fum_rec, def_td = dl$def_td,
                       def_block = dl$def_block, def_safety = dl$def_safety,
                       pa = dl$pa)
    D[, team := tm]; Ddst[, team := tm]
    out[[si]] <- rbindlist(list(D, Ddst), use.names = TRUE, fill = TRUE)
    say(sprintf("%s dealt", tm), 0.2 + 0.6 * si / length(setup))
  }

  say("scoring", 0.85)
  A <- rbindlist(out, use.names = TRUE, fill = TRUE)
  setnames(A, "sim", "SimID")
  A[, DKScore := round(nfl_score_lines(A, "DK"), 3)]
  A[, FDScore := round(nfl_score_lines(A, "FD"), 3)]

  # ---- position + metadata --------------------------------------------------
  posmap <- unique(PL[, .(player, Pos = pos)])
  posmap <- rbind(posmap, TT[, .(player = dst, Pos = "DST")], fill = TRUE)
  posmap <- unique(posmap, by = "player")
  meta <- unique(A[, .(Player = player, Team = team)])
  meta <- merge(meta, posmap, by.x = "Player", by.y = "player", all.x = TRUE)
  meta[is.na(Pos), Pos := "WR"]
  meta[, `:=`(DKID = NA_integer_, DKCID = NA_integer_,
              DKSalary = NA_integer_, DKCSalary = NA_integer_,
              FDID = NA_integer_, FDSalary = NA_integer_,
              DKProj = NA_real_, DKOwn = 0, CPTOwn = 0,
              FDProj = NA_real_, FDOwn = 0, MVPOwn = 0)]
  prj <- input_data$projections
  if (!is.null(prj) && nrow(as.data.table(prj))) {
    prj <- as.data.table(prj)
    if ("dkproj" %in% names(prj)) meta[prj, DKProj := as.numeric(i.dkproj), on = .(Player = player)]
    if ("dkown"  %in% names(prj)) meta[prj, DKOwn  := as.numeric(i.dkown),  on = .(Player = player)]
    if ("fdproj" %in% names(prj)) meta[prj, FDProj := as.numeric(i.fdproj), on = .(Player = player)]
    if ("fdown"  %in% names(prj)) meta[prj, FDOwn  := as.numeric(i.fdown),  on = .(Player = player)]
    # Showdown captain-slot ownership: `cptown` (DK Captain) / `mvpown` (FD MVP),
    # read straight off the projections tab like the flat own columns above. The
    # Portfolio Builder splits ownership/leverage by premium vs flex slot when
    # these are present (see app.R make_filtered_exposure).
    if ("cptown" %in% names(prj)) meta[prj, CPTOwn := as.numeric(i.cptown), on = .(Player = player)]
    if ("mvpown" %in% names(prj)) meta[prj, MVPOwn := as.numeric(i.mvpown), on = .(Player = player)]
    # DK ids / salaries. FLEX slot: dkid | dk_id | dk_id_util. CAPTAIN slot is a
    # DIFFERENT DK number (~1.5x salary): dkcid | dk_id_cpt. Showdown upload and
    # the captain optimiser both need DKCID -- without it the portfolio export
    # prints "Name (NA)" for every captain. Column names mirror cfb_engine.
    for (idc in intersect(c("dkid", "dk_id", "dk_id_util"), names(prj)))
      meta[prj, DKID := suppressWarnings(as.integer(get(paste0("i.", idc)))), on = .(Player = player)]
    for (idc in intersect(c("dkcid", "dk_id_cpt"), names(prj)))
      meta[prj, DKCID := suppressWarnings(as.integer(get(paste0("i.", idc)))), on = .(Player = player)]
    for (sc in intersect(c("salary", "dksalary", "dk_salary", "salary_util"), names(prj)))
      meta[prj, DKSalary := suppressWarnings(as.integer(get(paste0("i.", sc)))), on = .(Player = player)]
    for (sc in intersect(c("dkcsalary", "salary_cpt"), names(prj)))
      meta[prj, DKCSalary := suppressWarnings(as.integer(get(paste0("i.", sc)))), on = .(Player = player)]
    # FanDuel ids / salaries -- FD classic optimiser + upload, and FD MVP.
    for (idc in intersect(c("fdid", "fd_id", "fd_id_util"), names(prj)))
      meta[prj, FDID := suppressWarnings(as.integer(get(paste0("i.", idc)))), on = .(Player = player)]
    for (sc in intersect(c("fdsalary", "fd_salary", "fd_salary_util"), names(prj)))
      meta[prj, FDSalary := suppressWarnings(as.integer(get(paste0("i.", sc)))), on = .(Player = player)]
  }
  meta[is.na(DKOwn), DKOwn := 0][is.na(CPTOwn), CPTOwn := 0]
  meta[is.na(FDOwn), FDOwn := 0][is.na(MVPOwn), MVPOwn := 0]

  sim_results <- A[, .(SimID, Player = player, Team = team,
                       DKScore, FDScore)]
  sim_results <- merge(sim_results, meta[, .(Player, DKSalary, DKID, FDSalary)],
                       by = "Player", all.x = TRUE, sort = FALSE)
  sim_results[, DKOwn := 0]

  projections <- A[, .(DKProj = round(mean(DKScore), 2), FDProj = round(mean(FDScore), 2)),
                   by = .(Player = player)]

  say("summaries", 0.93)
  sv <- nfl_sport_visuals(A, meta, draw, fav, dog, total, spread, r, nrow(Gp), n_sims, slate_type)
  # the drawn game per SimID, for acceptance checks / debugging (small: n_sims rows)
  sv$draw <- data.table(SimID = seq_len(n_sims), game_id = draw$game_id,
                        fav = fav, dog = dog, fteam = draw$fteam, dteam = draw$dteam,
                        fpass_yds = draw$fpass_yds, dpass_yds = draw$dpass_yds,
                        fcarries = draw$fcarries, dcarries = draw$dcarries,
                        fcmp = draw$fcmp, dcmp = draw$dcmp,
                        frush_yds = draw$frush_yds, drush_yds = draw$drush_yds,
                        ptsF = draw$ptsF, ptsD = draw$ptsD)

  say("done", 1)
  res <- list(sim_results = sim_results, metadata = meta, projections = projections,
              sport_visuals = sv)
  if (isTRUE(keep_components)) {
    ccols <- intersect(c("SimID", "player", "team", "is_dst", "rec", "ryds", "rtd",
                         "rec_b5", "rec_rz", "car", "cyds", "ctd", "pyds", "ptd", "pint",
                         "fgp", "xp", "rettd", "fum", "def_sacks", "def_int", "def_fum_rec",
                         "def_td", "def_block", "pa", "DKScore", "FDScore"), names(A))
    res$sim_components <- A[, ..ccols]
  }
  res
}

# =============================================================================
# sport_visuals -- the read-back tables. A subset of CFB's set, plus FD.
# =============================================================================
nfl_sport_visuals <- function(A, meta, draw, fav, dog, total, spread, r, pool_n, n_sims, slate_type) {
  P <- A[is_dst == FALSE]
  stat_line <- P[, .(
      Rec = round(mean(rec), 2), RecYds = round(mean(ryds), 1), RecTD = round(mean(rtd), 3),
      Car = round(mean(car), 2), RushYds = round(mean(cyds), 1), RushTD = round(mean(ctd), 3),
      PassYds = round(mean(pyds), 1), PassTD = round(mean(ptd), 3), INT = round(mean(pint), 3),
      RetTD = round(mean(rettd), 4), Fum = round(mean(fum), 3),
      DK = round(mean(DKScore), 2), FD = round(mean(FDScore), 2),
      Floor = round(as.numeric(quantile(DKScore, .25)), 1),
      Ceil  = round(as.numeric(quantile(DKScore, .90)), 1),
      Bust = round(100 * mean(DKScore < 3), 1), Boom = round(100 * mean(DKScore >= 20), 1)),
    by = .(Player = player, Team = team)]
  dst_line <- A[is_dst == TRUE, .(
      Sacks = round(mean(def_sacks), 2), INT = round(mean(def_int), 2),
      FumRec = round(mean(def_fum_rec), 2), DefTD = round(mean(def_td), 3),
      PA = round(mean(pa, na.rm = TRUE), 1),
      DK = round(mean(DKScore), 2), FD = round(mean(FDScore), 2)),
    by = .(Player = player, Team = team)]
  stat_line <- merge(stat_line, meta[, .(Player, Pos)], by = "Player", all.x = TRUE)
  setorder(stat_line, -DK)

  team_line <- P[, .(Rec = sum(rec), RecYds = sum(ryds), RecTD = sum(rtd),
                     Car = sum(car), RushYds = sum(cyds), RushTD = sum(ctd),
                     PassYds = sum(pyds), PassTD = sum(ptd), INT = sum(pint), Fum = sum(fum),
                     KickPts = sum(fgp + xp)),
                 by = .(SimID, team)][
                 , .(Rec = round(mean(Rec), 1), RecYds = round(mean(RecYds)),
                     Car = round(mean(Car), 1), RushYds = round(mean(RushYds)),
                     PassYds = round(mean(PassYds)),
                     PassTD = round(mean(PassTD), 2), RushTD = round(mean(RushTD), 2),
                     INT = round(mean(INT), 2), Fum = round(mean(Fum), 2),
                     KickPts = round(mean(KickPts), 1)),
                 by = .(Team = team)]
  team_line[, `:=`(Implied = fifelse(Team == fav,
                     round(total / 2 - spread / 2, 1),   # spread is home-relative; fav implied
                     round(total / 2 + spread / 2, 1)),
                   ScrimYds = RecYds + RushYds)]
  # the favourite's implied points regardless of home/away:
  fav_imp <- total / 2 + abs(spread) / 2
  team_line[, Implied := fifelse(Team == fav, round(fav_imp, 1), round(total - fav_imp, 1))]
  team_line[, PassShare := round(PassYds / pmax(PassYds + RushYds, 1), 3)]
  setcolorder(team_line, c("Team", "Implied", "ScrimYds", "PassShare"))

  components <- P[, .(
      Receptions = round(mean(rec) * 1.0, 2),
      RecYards   = round(mean(ryds) * 0.1, 2),
      RecTDs     = round(mean(rtd) * 6, 2),
      RushYards  = round(mean(cyds) * 0.1, 2),
      RushTDs    = round(mean(ctd) * 6, 2),
      PassYards  = round(mean(pyds) * 0.04, 2),
      PassTDs    = round(mean(ptd) * 4, 2),
      Kicking    = round(mean(fgp + xp), 2),
      ReturnTDs  = round(mean(rettd) * 6, 2),
      Turnovers  = round(mean(pint * -1 + fum * -1), 2),
      DK_Total   = round(mean(DKScore), 2)),
    by = .(Player = player, Team = team)][order(-DK_Total)]

  rates <- P[, .(
      AnyTD    = round(100 * mean((rtd + ctd + rettd) >= 1), 1),
      MultiTD  = round(100 * mean((rtd + ctd + rettd) >= 2), 1),
      Rec100   = round(100 * mean(ryds >= 100), 1),
      Rush100  = round(100 * mean(cyds >= 100), 1),
      Pass300  = round(100 * mean(pyds >= 300), 1),
      Blank    = round(100 * mean(rec == 0 & car == 0 & pyds == 0 & fgp == 0), 1)),
    by = .(Player = player, Team = team)]
  rates <- merge(rates, stat_line[, .(Player, Pos, DK)], by = "Player", all.x = TRUE)
  setorder(rates, -DK)

  ptsv <- data.table(SimID = seq_len(n_sims), f = draw$ptsF, d = draw$ptsD)
  tg <- P[, .(PassYds = sum(pyds), RushYds = sum(cyds), Rec = sum(rec),
              TotalTD = sum(rtd) + sum(ctd)), by = .(SimID, team)]
  tg[ptsv, Points := fifelse(team == fav, i.f, i.d), on = "SimID"]
  tg[, ScrimYds := PassYds + RushYds]
  TEAM_METRICS <- c("Points", "ScrimYds", "PassYds", "RushYds", "Rec", "TotalTD")
  for (cc in TEAM_METRICS) tg[[cc]] <- as.numeric(tg[[cc]])
  team_spread <- melt(tg, id.vars = c("SimID", "team"), measure.vars = TEAM_METRICS,
                      variable.name = "Metric", value.name = "V")[
    , .(Mean = round(mean(V), 1), P10 = round(quantile(V, .1), 1),
        P25 = round(quantile(V, .25), 1), Median = round(median(V), 1),
        P75 = round(quantile(V, .75), 1), P90 = round(quantile(V, .9), 1)),
    by = .(Team = team, Metric)]

  score_dist <- rbind(
    stat_line[, .(Player, Team, Pos, Mean = DK, Floor, Ceil, Bust, Boom)],
    dst_line[, .(Player, Team, Pos = "DST", Mean = DK, Floor = NA_real_, Ceil = NA_real_,
                 Bust = NA_real_, Boom = NA_real_)], fill = TRUE)[order(-Mean)]

  list(stat_line = stat_line, dst_line = dst_line, team_line = team_line,
       components = components, rates = rates, team_spread = team_spread,
       score_dist = score_dist,
       pool_size = pool_n, n_sims = n_sims, ess = round(r$ess),
       slate_type = slate_type,
       market = sprintf("%s slate | fav %s | total %.1f, spread %+.1f (home-rel)",
                        slate_type, fav, total, spread),
       pool_total = round(r$total, 1), pool_margin = round(r$margin, 1),
       asked_total = round(r$target$total, 2))
}

# =============================================================================
# CLASSIC (multi-game full slate) -- N two-team sims stacked in a shared SimID
# space (the games are independent, so SimID k is one Monte-Carlo world across
# the card). Adds GameKey / StartOrder for the classic optimiser. Mirrors
# run_cfb_classic_simulation.
# =============================================================================
run_nfl_classic_simulation <- function(input_data, n_sims = 10000, config = NULL,
                                       progress_callback = NULL, keep_components = FALSE,
                                       seed = NULL) {
  if (is.null(n_sims) || is.na(n_sims)) n_sims <- 10000
  G   <- copy(as.data.table(input_data$game))
  TT  <- as.data.table(input_data$team)
  PL  <- copy(as.data.table(input_data$players))
  PRJ <- input_data$projections
  if (!"start_order" %in% names(G) || anyNA(G$start_order)) G[, start_order := seq_len(.N)]
  setorder(G, start_order)
  ng <- nrow(G)

  say <- function(msg, frac = NULL) {
    if (is.function(progress_callback)) try(progress_callback(msg, frac), silent = TRUE)
    message("[nfl-classic] ", msg)
  }

  sr <- vector("list", ng); md <- vector("list", ng)
  pj <- vector("list", ng); vis <- vector("list", ng); cp <- vector("list", ng)

  for (i in seq_len(ng)) {
    gi  <- G[i]; tms <- c(gi$away, gi$home); gkey <- paste(gi$away, gi$home)
    say(sprintf("game %d/%d  %s", i, ng, gkey), (i - 1) / ng)
    sub <- list(game = gi, team = TT[team %in% tms], players = PL[team %in% tms],
                projections = PRJ)
    gp <- if (is.function(progress_callback))
            function(m, f) try(progress_callback(sprintf("game %d/%d: %s", i, ng, m),
                                                 (i - 1 + (f %||% 0)) / ng), silent = TRUE) else NULL
    res <- run_nfl_simulation(sub, n_sims = n_sims, config = config, progress_callback = gp,
                              keep_components = keep_components, .slate_type = "classic",
                              seed = if (is.null(seed)) NULL else as.integer(seed) + i)
    so <- as.integer(gi$start_order)
    sr[[i]] <- as.data.table(res$sim_results)[, `:=`(GameKey = gkey, StartOrder = so)]
    md[[i]] <- as.data.table(res$metadata)[,   `:=`(GameKey = gkey, StartOrder = so)]
    pj[[i]] <- as.data.table(res$projections)
    vis[[i]] <- res$sport_visuals
    if (isTRUE(keep_components) && !is.null(res$sim_components)) {
      cc <- res$sim_components; cc[, GameKey := gkey]; cp[[i]] <- cc
    }
  }

  say("combining", 0.95)
  sim_results <- rbindlist(sr, fill = TRUE)
  metadata    <- rbindlist(md, fill = TRUE)
  projections <- rbindlist(pj, fill = TRUE)
  sim_results <- sim_results[!is.na(Player) & Player != ""]
  metadata    <- metadata[!is.na(Player) & Player != ""]
  if (nrow(projections)) projections <- projections[!is.na(Player) & Player != ""]

  vk <- c("stat_line", "dst_line", "team_line", "components", "rates", "team_spread", "score_dist")
  sv <- list()
  for (k in vk)
    sv[[k]] <- rbindlist(lapply(seq_along(vis), function(j) {
      d <- vis[[j]][[k]]
      if (is.null(d) || !nrow(d)) return(NULL)
      as.data.table(copy(d))[, Game := vis[[j]]$market][]
    }), fill = TRUE)
  ess_all <- vapply(vis, function(v) as.numeric(v$ess %||% NA_real_), 0)
  sv$pool_size <- vis[[1]]$pool_size
  sv$n_sims    <- n_sims
  sv$ess       <- suppressWarnings(min(ess_all, na.rm = TRUE))
  sv$slate_type <- "classic"
  sv$market    <- sprintf("%d-game classic slate | worst-matched game ESS %s",
                          ng, format(round(sv$ess), big.mark = ","))

  say("done", 1)
  out <- list(sim_results = sim_results, metadata = metadata,
              projections = projections, sport_visuals = sv)
  if (isTRUE(keep_components))
    out$sim_components <- rbindlist(cp[!vapply(cp, is.null, logical(1))], fill = TRUE)
  out
}
