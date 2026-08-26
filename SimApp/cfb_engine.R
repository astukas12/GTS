# =============================================================================
# cfb_engine.R -- College Football showdown
# -----------------------------------------------------------------------------
# SIMULATE A REAL AFTERNOON, THEN DEAL IT OUT. Every simulated game here is an
# actual historical FBS game, drawn from a 4,966-game pool matched on both
# teams' pre-game profiles and calibrated to the market. Its individual
# completions, designed runs, sacks and field goals are then dealt to this
# slate's players one event at a time.
#
# Because events are dealt rather than shared out, player totals always sum to
# the team line by construction -- there is no reconciliation step anywhere in
# this file. Blowout substitution needs no model either: the sampled game
# carries its own score, so a flattened backfield arrives already correlated
# with the margin that caused it.
#
# WHAT THE SHEET SUPPLIES, AND NOTHING ELSE:
#   pass_share   who throws, and therefore who is debited the sacks
#   usage        P(target of any given completion). Sums to 1 per team.
#   ypc          expected yards per catch -- one exponential tilt on the
#                position's bucket mix
#   carry_usage  P(handed any given designed carry). Sums to 1.
#   sy_tilt      short-yardage tilt (split-half .376)
#   gl_tilt      goal-line tilt (.204), defaults to sy_tilt
#   kicker / punt_returner / kick_returner   one name each
#   pys_target   the pass-yard share to ASK THE POOL FOR
#
# WHAT COMES FROM THE DRAWN GAME AND TAKES NO INPUT: passing yards, attempts,
# interceptions, sack yardage, field goal distances and results, extra points,
# team fumbles lost. Each was tested for player signal and found to have none --
# QB interception rate 0.026 split-half, kicker FG% 0.009, fumble rate 0.084.
#
# TOUCHDOWNS ARE NEVER ALLOCATED. A touchdown is a property of the catch or the
# carry that gets dealt: the drawn game contained a 12-yard reception from the
# 14 that ended in the end zone, and whoever receives that event scores. There
# is no TD share vector in this engine.
#
# DATA: cfb_data/ holds three files totalling 1.65MB, built by
# CFB/R/build_templates.R out of 58.5MB of play-by-play that never ships.
# =============================================================================

CFB_DATA_DIR <- "cfb_data"

# ---- constants, all measured on 2019-25 FBS ---------------------------------

# Event kind codes, as written by build_templates.R
CFB_EVT_SACK <- 1L; CFB_EVT_FG <- 2L; CFB_EVT_RUN <- 3L; CFB_EVT_CMP <- 4L

# The six pool dimensions. Settled by held-out backtest on 2,930 games: these
# beat a fourteen-dimension set on every target while returning twelve times
# the effective sample. Explosiveness, success rate, drives, plays/drive and all
# six defensive dimensions each made things WORSE.
CFB_POOL_DIMS <- c("total", "absp", "fO_pr", "fO_pys", "dO_pr", "dO_pys")
CFB_POOL_W    <- c(1.4, 1.4, 1.0, 1.0, 1.0, 1.0)
CFB_BW        <- 0.9

# Reception yardage buckets and the league's own mix across them. THE LEAGUE MIX
# IS THE DENOMINATOR of the likelihood ratio -- not the player's own position.
# Using the position as denominator makes every default player's ratio exactly
# 1.0, which silently removes position from the deal entirely and lets a tight
# end compete for a 60-yard bomb on a wideout's terms.
CFB_BUCKETS   <- c("<=2", "3-7", "8-15", "16-30", "31+")
CFB_BUCKET_MID <- c(-0.32, 5.11, 10.91, 21.09, 44.17)  # ACTUAL bucket means
CFB_LEAGUE_MIX <- c(0.122, 0.312, 0.322, 0.173, 0.072)
CFB_BASE_MIX <- list(
  WR = c(.087, .287, .347, .193, .086),
  TE = c(.094, .328, .337, .192, .050),
  RB = c(.229, .360, .273, .103, .035))

# Per-touch fumble rates. The QB's is ~4x a back's because his come from SACKS
# AND SNAPS rather than from carrying, so his weight rides on dropbacks.
CFB_FUM_RATE <- c(QB = .0244, WR = .0078, TE = .0078, RB = .0056, K = 0)
CFB_FUM_DIST <- c(.579, .307, .091, .018, .005)   # team fumbles lost, 0..4

# A returner scores +6 in 4.72% of team-games. Small on average, but it is a
# spike on players who are usually minimum-priced, which is exactly what a
# mean-based projection cannot express.
CFB_PUNT_TD_RATE <- 0.0276
CFB_KICK_TD_RATE <- 0.0222

# DraftKings CFB scoring, taken from the published rules.
CFB_SCORE <- list(
  pass_yd = 0.04, pass_td = 4, interception = -1, pass_300 = 3,
  rush_yd = 0.10, rush_td = 6, rush_100 = 3,
  rec = 1, rec_yd = 0.10, rec_td = 6, rec_100 = 3,
  fumble_lost = -1, return_td = 6, xp = 1)

# ---- small helpers -----------------------------------------------------------

cfb_num <- function(x) { x <- suppressWarnings(as.numeric(x))
                         fifelse(is.na(x), 0, x) }
cfb_ess <- function(w) { w <- w / sum(w); 1 / sum(w^2) }

cfb_bucket <- function(y) findInterval(y, c(-Inf, 2.5, 7.5, 15.5, 30.5))

# DK's field goal bands: 3 / 3 / 4 / 5 by distance. Distance is yards_to_goal
# plus 17 (10 for the end zone, 7 for the snap).
cfb_fg_points <- function(ytg) { d <- ytg + 17
  fifelse(d < 40, 3, fifelse(d < 50, 4, 5)) }

# Solve the one exponential parameter that slides a position's bucket mix to a
# stated mean, then express the result as a likelihood ratio against the league.
#
# The ratio CENTRES ITSELF: sum over buckets of league_mix * (tilted/league) is
# exactly 1, so a stated .215 usage receives .215 of catches in a game whose
# event mix matches the league, and drifts above only when the drawn game
# genuinely favoured him.
cfb_lr <- function(pos, target_ypc) {
  p0 <- CFB_BASE_MIX[[pos]]
  if (is.null(p0)) p0 <- CFB_BASE_MIX$WR
  lo <- sum(p0 * CFB_BUCKET_MID)
  th <- if (is.na(target_ypc) || abs(target_ypc - lo) < 1e-6) 0 else
    tryCatch(uniroot(function(t) {
      q <- p0 * exp(t * CFB_BUCKET_MID); sum(q / sum(q) * CFB_BUCKET_MID) - target_ypc
    }, c(-0.4, 0.4))$root, error = function(e) 0)
  q <- p0 * exp(th * CFB_BUCKET_MID)
  (q / sum(q)) / CFB_LEAGUE_MIX
}

# ---- pool matching -----------------------------------------------------------

# Kernel weights over the pool. Everything is a soft weight -- no hard filters,
# so a slightly-off game contributes a little rather than nothing.
cfb_pool_weights <- function(P, target, bw = CFB_BW) {
  M  <- as.matrix(P[, ..CFB_POOL_DIMS])
  mu <- colMeans(M); sg <- apply(M, 2, stats::sd)
  tz <- (unlist(target)[CFB_POOL_DIMS] - mu) / sg
  Mz <- sweep(sweep(M, 2, mu, "-"), 2, sg, "/")
  d2 <- rowSums(sweep((sweep(Mz, 2, tz, "-"))^2, 2, CFB_POOL_W, "*"))
  w  <- exp(-d2 / (2 * bw^2)); w[!is.finite(w)] <- 0
  w / sum(w)
}

# KERNEL SMOOTHING SHRINKS EVERY ESTIMATE TOWARD THE POOL MEAN (total 53.9).
# Ask for a 47.5-point game and the pool answers 50.6 -- correct behaviour,
# wrong number to simulate from. The fix is to correct the TARGET, never the
# output: if the pool returns 50.6 when asked for 47.5, ask it for less until it
# returns 47.5. That changes WHICH GAMES GET DRAWN, so every drawn game stays a
# real, internally consistent afternoon.
#
# Rescaling a sampled line to hit the market would break the ratios inside it --
# yards against attempts, points against touchdowns -- and those ratios are the
# entire reason for resampling real games.
cfb_calibrate <- function(P, target, market, iters = 12, damp = 0.8, tol = 0.05) {
  tg <- target
  for (k in seq_len(iters)) {
    w  <- cfb_pool_weights(P, tg)
    et <- sum(P$pts_sum * w); em <- sum(P$margin * w)
    dt <- market$total - et;  dm <- market$margin - em
    if (max(abs(c(dt, dm))) < tol) break
    tg$total <- tg$total + damp * dt
    tg$absp  <- max(0, tg$absp + damp * dm)
  }
  w <- cfb_pool_weights(P, tg)
  list(target = tg, w = w, ess = cfb_ess(w),
       total = sum(P$pts_sum * w), margin = sum(P$margin * w))
}

# ---- reading the sheet -------------------------------------------------------

# One tab per team plus a `game` tab; the TAB NAME IS THE TEAM. Players occupy
# columns A..Q and the team-level block sits beside them from column S.
read_cfb_input <- function(file_path) {
  sh <- readxl::excel_sheets(file_path)
  gtab <- sh[tolower(sh) == "game"]
  if (!length(gtab)) stop("CFB workbook needs a `game` tab")
  g <- as.data.table(readxl::read_excel(file_path, sheet = gtab[1]))
  tms <- setdiff(sh, gtab)

  pl <- rbindlist(lapply(tms, function(tm) {
    x <- as.data.table(readxl::read_excel(file_path, sheet = tm, range = readxl::cell_cols("A:Q")))
    x <- x[!is.na(player)]
    x[, team := tm][]
  }), fill = TRUE)

  tt <- rbindlist(lapply(tms, function(tm) {
    b <- as.data.table(readxl::read_excel(file_path, sheet = tm,
                                          range = readxl::cell_cols("S:T")))
    setnames(b, c("field", "value"))
    b <- b[!is.na(field)]
    o <- as.list(setNames(b$value, b$field))
    data.table(team = tm,
               kicker = o$kicker %||% NA_character_,
               punt_returner = o$punt_returner %||% NA_character_,
               kick_returner = o$kick_returner %||% NA_character_,
               pys_target = as.numeric(o$pys_target %||% NA))
  }), fill = TRUE)

  list(game = g, team = tt, players = pl)
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

# =============================================================================
# THE SIMULATION
# =============================================================================
run_cfb_simulation <- function(input_data, n_sims = 10000,
                               config = NULL, progress_callback = NULL) {
  say <- function(msg, frac = NULL) {
    if (is.function(progress_callback)) try(progress_callback(msg, frac), silent = TRUE)
    message("[cfb] ", msg)
  }
  if (is.null(n_sims) || is.na(n_sims)) n_sims <- 10000

  G  <- as.data.table(input_data$game)
  TT <- as.data.table(input_data$team)
  PL <- as.data.table(input_data$players)
  for (cl in c("pass_share","usage","ypc","carry_usage","sy_tilt","gl_tilt",
               "salary_util","salary_cpt","dk_id_util","dk_id_cpt"))
    if (cl %in% names(PL)) PL[[cl]] <- cfb_num(PL[[cl]])
  PL[is.na(route_base) | route_base == "", route_base := dk_pos]
  PL[gl_tilt == 0, gl_tilt := fifelse(sy_tilt > 0, sy_tilt, 1)]
  PL[sy_tilt == 0, sy_tilt := 1]

  say("loading pool", 0.02)
  P   <- readRDS(file.path(CFB_DATA_DIR, "cfb_pool.rds"));   setDT(P)
  EV  <- readRDS(file.path(CFB_DATA_DIR, "cfb_events.rds")); setDT(EV)
  FUM <- readRDS(file.path(CFB_DATA_DIR, "cfb_fumbles.rds")); setDT(FUM)
  setkey(EV, game_id, pos_team); setkey(FUM, game_id, team); setkey(P, game_id)

  fav <- G$fav[1]
  dog <- setdiff(c(G$away[1], G$home[1]), fav)
  pys <- setNames(TT$pys_target, TT$team)

  # The market pins the LEVEL and nothing prices it better -- closing totals are
  # unbiased across all 4,966 pool games. The style dimensions exist because the
  # market is SILENT on composition: it predicts the favourite's pass/rush split
  # at r = 0.111 and the underdog's at 0.010.
  target <- list(total = G$total[1], absp = G$spread[1],
                 fO_pr = 0.52, fO_pys = pys[[fav]],
                 dO_pr = 0.52, dO_pys = pys[[dog]])
  cal <- cfb_calibrate(P, target, list(total = G$total[1], margin = G$spread[1]))
  say(sprintf("pool calibrated: ESS %.0f, total %.1f, margin %.1f",
              cal$ess, cal$total, cal$margin), 0.08)
  if (cal$ess < 150)
    warning("CFB pool is thin: ESS ", round(cal$ess),
            ". Widen the bandwidth or accept a wider output.")

  set.seed(as.integer(Sys.time()) %% .Machine$integer.max)
  idx <- sample.int(nrow(P), n_sims, TRUE, prob = cal$w)
  draw <- P[idx]

  # ---- per-team setup --------------------------------------------------------
  setup <- lapply(c(fav, dog), function(tm) {
    R <- PL[team == tm & usage > 0]
    S <- PL[team == tm & carry_usage > 0]
    Q <- PL[team == tm & pass_share > 0]
    tr <- TT[team == tm]
    list(tm = tm, side = if (tm == fav) "f" else "d",
         rec = R, rsh = S, qb = if (nrow(Q)) Q$player[1] else NA_character_,
         lr  = do.call(rbind, lapply(seq_len(nrow(R)),
                function(k) cfb_lr(R$route_base[k], R$ypc[k]))),
         k = tr$kicker, pr = tr$punt_returner, kr = tr$kick_returner,
         who = unique(c(R$player, S$player, Q$player,
                        tr$kicker, tr$punt_returner, tr$kick_returner)))
  })
  names(setup) <- c(fav, dog)

  say(sprintf("simulating %s games", format(n_sims, big.mark = ",")), 0.12)
  out <- vector("list", length(setup))

  for (si in seq_along(setup)) {
    cf <- setup[[si]]; tm <- cf$tm
    R <- cf$rec; S <- cf$rsh; nR <- nrow(R); nS <- nrow(S)
    pos <- setNames(R$route_base, R$player)
    pos[S$player] <- ifelse(S$dk_pos == "QB", "QB", "RB")
    pos[cf$qb] <- "QB"; pos[cf$k] <- "K"
    # A returner who neither catches nor carries (UNC's McGill) appears in
    # `who` but in none of the tables above, so give him a position explicitly.
    miss <- setdiff(cf$who, names(pos)); if (length(miss)) pos[miss] <- "WR"
    who <- cf$who; nW <- length(who)
    iR <- match(R$player, who); iS <- match(S$player, who)
    iQ <- match(cf$qb, who); iK <- match(cf$k, who)
    iPR <- match(cf$pr, who); iKR <- match(cf$kr, who)

    # Team-side columns for whichever side of the drawn game this team plays.
    # NAMED v_* DELIBERATELY. `ptd` would collide with the column of the same
    # name created below, and inside `:=` the column wins -- so `ptd = ptd[SimID]`
    # reads the zeroes it just wrote and every passing touchdown scores nothing.
    # Silent, and worth about 6 DK points a game on the quarterback.
    v_pyds <- draw[[paste0(cf$side, "pyds")]]
    v_ptd  <- draw[[paste0(cf$side, "ptd")]]
    v_pint <- draw[[paste0(cf$side, "pint")]]
    dteam_col <- draw[[if (cf$side == "f") "fteam" else "dteam"]]

    acc <- matrix(0, nrow = n_sims * nW, ncol = 9)   # rec ryds rtd car cyds ctd fg xp rtd6
    simv <- rep(seq_len(n_sims), each = nW)
    plyv <- rep(who, times = n_sims)

    for (i in seq_len(n_sims)) {
      gid <- draw$game_id[i]; pt <- dteam_col[i]
      ev  <- EV[.(gid, pt), nomatch = 0L]
      base <- (i - 1L) * nW
      if (nrow(ev)) {
        cmp <- ev[kind == CFB_EVT_CMP]
        if (nrow(cmp) && nR) for (j in seq_len(nrow(cmp))) {
          b <- cfb_bucket(cmp$yds[j])
          k <- sample.int(nR, 1L, prob = R$usage * cf$lr[, b])
          r <- base + iR[k]
          acc[r,1] <- acc[r,1]+1; acc[r,2] <- acc[r,2]+cmp$yds[j]
          if (isTRUE(cmp$td[j] == 1)) acc[r,3] <- acc[r,3]+1
        }
        run <- ev[kind == CFB_EVT_RUN]
        if (nrow(run) && nS) for (j in seq_len(nrow(run))) {
          gl <- !is.na(run$ytg[j]) && run$ytg[j] <= 3
          sy <- !gl && !is.na(run$dn[j]) && !is.na(run$dist[j]) &&
                run$dn[j] >= 3 && run$dist[j] <= 2
          wgt <- if (gl) S$carry_usage * S$gl_tilt else
                 if (sy) S$carry_usage * S$sy_tilt else S$carry_usage
          k <- sample.int(nS, 1L, prob = wgt)
          r <- base + iS[k]
          acc[r,4] <- acc[r,4]+1; acc[r,5] <- acc[r,5]+run$yds[j]
          if (isTRUE(run$td[j] == 1)) acc[r,6] <- acc[r,6]+1
        }
        # SACKS GO WHOLE TO WHOEVER WAS IN, never matched by name -- name
        # matching returns ZERO sacks for a 388-attempt season. The box folds
        # them into QB rushing and DK scores off the box, so they are added to
        # his rushing yards rather than deducted separately.
        sk <- ev[kind == CFB_EVT_SACK]
        if (nrow(sk) && !is.na(iQ)) acc[base+iQ,5] <- acc[base+iQ,5] + sum(sk$yds)
        # KICKS AS THEY HAPPENED, NEVER RE-ROLLED. The drawn game's point total
        # already includes these makes; re-simulating them would break the
        # internal consistency that resampling exists to preserve.
        fg <- ev[kind == CFB_EVT_FG]
        if (nrow(fg) && !is.na(iK)) {
          made <- fg[!is.na(made) & made == 1]
          if (nrow(made)) acc[base+iK,7] <- sum(cfb_fg_points(made$ytg))
        }
      }
      if (!is.na(iK)) acc[base+iK,8] <- sum(acc[(base+1):(base+nW),3]) +
                                        sum(acc[(base+1):(base+nW),6])
      if (!is.na(iPR) && runif(1) < CFB_PUNT_TD_RATE) acc[base+iPR,9] <- acc[base+iPR,9]+1
      if (!is.na(iKR) && runif(1) < CFB_KICK_TD_RATE) acc[base+iKR,9] <- acc[base+iKR,9]+1
      if (i %% 1000 == 0)
        say(sprintf("%s %d/%d", tm, i, n_sims), 0.12 + 0.7 * ((si-1)/2 + i/n_sims/2))
    }

    D <- data.table(SimID = simv, player = plyv, team = tm,
                    rec = acc[,1], ryds = acc[,2], rtd = acc[,3],
                    car = acc[,4], cyds = acc[,5], ctd = acc[,6],
                    fg = acc[,7], xp = acc[,8], rettd = acc[,9])
    D[, `:=`(pyds = 0, ptd = 0, pint = 0)]
    if (!is.na(cf$qb)) {
      D[player == cf$qb, `:=`(pyds = v_pyds[SimID], ptd = v_ptd[SimID],
                              pint = v_pint[SimID])]
    }
    # FUMBLES: the DRAWN GAME's team count, allocated across touches already
    # assigned. Not dealt as events -- the PBP cannot identify which play was a
    # fumble -- and no per-player input, because fumble rate per carry has
    # split-half reliability of 0.084.
    D[, tch := rec + car]
    if (!is.na(cf$qb)) D[player == cf$qb, tch := tch + 25]
    D[, fwt := tch * unname(CFB_FUM_RATE[pos[player]])]
    D[is.na(fwt), fwt := 0]
    fl <- FUM[.(draw$game_id, dteam_col), nomatch = NA]$fl
    fl[is.na(fl)] <- sample(0:4, sum(is.na(fl)), TRUE, prob = CFB_FUM_DIST)
    D[, fum := 0]
    for (i in which(fl > 0)) {
      rows <- which(D$SimID == i)
      w <- D$fwt[rows]
      if (sum(w) > 0) for (h in sample(rows, fl[i], TRUE, prob = w)) D$fum[h] <- D$fum[h] + 1
    }
    out[[si]] <- D
  }

  say("scoring", 0.88)
  A <- rbindlist(out)
  sc <- CFB_SCORE
  A[, dk := rec * sc$rec + ryds * sc$rec_yd + rtd * sc$rec_td +
            cyds * sc$rush_yd + ctd * sc$rush_td +
            fifelse(ryds >= 100, sc$rec_100, 0) +
            fifelse(cyds >= 100, sc$rush_100, 0) +
            pyds * sc$pass_yd + ptd * sc$pass_td + pint * sc$interception +
            fifelse(pyds >= 300, sc$pass_300, 0) +
            fg + xp * sc$xp + rettd * sc$return_td + fum * sc$fumble_lost]

  # ---- app contract ----------------------------------------------------------
  meta <- unique(PL[, .(Player = player, Team = team, Pos = dk_pos,
                        DKID = as.integer(dk_id_util), DKCID = as.integer(dk_id_cpt),
                        DKSalary = as.integer(salary_util),
                        DKCSalary = as.integer(salary_cpt))])
  meta <- meta[Player %in% unique(A$player)]

  sim_results <- A[, .(SimID, Player = player, Team = team,
                       DKScore = round(dk, 3))]
  sim_results <- merge(sim_results, meta[, .(Player, DKSalary, DKID)],
                       by = "Player", all.x = TRUE)
  sim_results[, `:=`(DKOwn = 0, FDScore = round(DKScore, 3))]

  projections <- A[, .(DKProj = round(mean(dk), 2)), by = .(Player = player)]

  score_dist <- A[, .(
      Mean = round(mean(dk), 2),
      P10 = round(as.numeric(quantile(dk, .10)), 1),
      P25 = round(as.numeric(quantile(dk, .25)), 1),
      Median = round(as.numeric(median(dk)), 1),
      P75 = round(as.numeric(quantile(dk, .75)), 1),
      P90 = round(as.numeric(quantile(dk, .90)), 1),
      P99 = round(as.numeric(quantile(dk, .99)), 1),
      Max = round(max(dk), 1),
      Bust = round(100 * mean(dk < 3), 1),
      Boom = round(100 * mean(dk >= 20), 1)),
    by = .(Player = player, Team = team)][order(Team, -Mean)]

  # THE STAT LINE WITH THE INPUTS THAT PRODUCED IT ALONGSIDE. If a receiver's
  # catches look wrong, the usage and ypc that caused them are on the same row.
  stat_line <- A[, .(
      Rec = round(mean(rec), 2), RecYds = round(mean(ryds), 1),
      Car = round(mean(car), 2), RushYds = round(mean(cyds), 1),
      PassYds = round(mean(pyds), 1),
      TD = round(mean(rtd + ctd + rettd), 3),
      Fum = round(mean(fum), 3), DK = round(mean(dk), 2)),
    by = .(Player = player, Team = team)]
  stat_line <- merge(stat_line, PL[, .(Player = player, Usage = round(usage, 3),
      YPC = round(ypc, 1), CarryUse = round(carry_usage, 3),
      SY = sy_tilt, GL = gl_tilt, Salary = as.integer(salary_util))],
      by = "Player", all.x = TRUE)
  stat_line[, Val := round(DK / pmax(Salary, 1) * 1000, 2)]
  setorder(stat_line, Team, -DK)

  # ---- validation: the mix, against real football ---------------------------
  # Measured on games the simulator never sees. These are the checks that say
  # whether the sheet produced a believable afternoon, and they belong in the
  # results rather than in a notebook.
  vs <- A[, .(catchers = sum(rec > 0), rushers = sum(car > 0),
              fum = sum(fum)), by = .(SimID, team)]
  ypcs <- A[rec >= 3, .(y = sum(ryds) / sum(rec)), by = .(SimID, team, player)]
  t3 <- A[, .(t = sum(rtd)), by = .(SimID, team)][t == 3]
  one3 <- A[t3, on = .(SimID, team)][, .(mx = max(rtd)), by = .(SimID, team)]
  vsum <- vs[, .(Catchers = round(mean(catchers), 2),
                 Rushers  = round(mean(rushers), 2),
                 FumLost  = round(mean(fum), 3)), by = .(team)]
  ysum <- ypcs[, .(YPCp10 = round(quantile(y, .1), 1),
                   YPCp90 = round(quantile(y, .9), 1)), by = .(team)]
  tsum <- one3[, .(All3TD = round(100 * mean(mx == 3), 1), N = .N), by = .(team)]

  # LONG, one row per check, so the real-football column is a single column
  # rather than one repeated per metric. `Real` is measured on FBS games the
  # simulator never sees; `Note` says what a miss would mean.
  vrow <- function(metric, get, real, note) {
    x <- as.list(setNames(get, c(fav, dog)))
    data.table(Check = metric, A = x[[fav]], B = x[[dog]], Real = real, Note = note)
  }
  g <- function(D, col) D[match(c(fav, dog), D$team)][[col]]
  validation <- rbindlist(list(
    vrow("Distinct catchers / game", g(vsum,"Catchers"), "7.53",
         "too many means the usage vector is padded at the tail"),
    vrow("Distinct rushers / game",  g(vsum,"Rushers"),  "5.51",
         "low if the sheet names fewer backs than a real rotation"),
    vrow("Team fumbles lost / game", g(vsum,"FumLost"),  "0.564",
         "taken from the drawn game's box, not modelled"),
    vrow("Player YPC, 10th pct",     g(ysum,"YPCp10"),   "5.3",
         "the tilt's low end"),
    vrow("Player YPC, 90th pct",     g(ysum,"YPCp90"),   "20.3",
         "the tilt's high end"),
    vrow("One man takes all 3 rec TD (%)", g(tsum,"All3TD"), "4.2",
         "KNOWN GAP: independent dealing gives 3.2; the affinity effect that lifts real football above it is not built"),
    vrow("3-TD games observed",      g(tsum,"N"),        "-",
         "sample size for the row above -- under ~300 that check is noise")))
  setnames(validation, c("A","B"), c(fav, dog))

  team_line <- A[, .(Rec = sum(rec), RecYds = sum(ryds), Car = sum(car),
                     RushYds = sum(cyds)), by = .(SimID, team)][
                  , .(Rec = round(mean(Rec), 1), RecYds = round(mean(RecYds)),
                      Car = round(mean(Car), 1), RushYds = round(mean(RushYds))),
                  by = .(Team = team)]

  sport_visuals <- list(
    score_dist = score_dist, stat_line = stat_line,
    validation = validation, team_line = team_line,
    pool_size = nrow(P), n_sims = n_sims,
    ess = round(cal$ess),
    market = sprintf("%s -%.1f, total %.1f", fav, G$spread[1], G$total[1]),
    pool_total = round(cal$total, 1), pool_margin = round(cal$margin, 1),
    asked_total = round(cal$target$total, 2))

  say("done", 1)
  list(sim_results = sim_results, metadata = meta, projections = projections,
       sport_visuals = sport_visuals)
}
