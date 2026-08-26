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

  # OPTIONAL `projections` tab: Player + ETR + Own. When ETR ships a file for a
  # slate, drop it in as a tab and the sim's average lands beside their number
  # on the main table, the same way the preseason engine does it. Absent, the
  # columns simply stay empty -- nothing else changes.
  prj <- NULL
  ptab <- sh[tolower(sh) %in% c("projections", "etr")]
  if (length(ptab)) {
    prj <- as.data.table(readxl::read_excel(file_path, sheet = ptab[1]))
    setnames(prj, tolower(names(prj)))
    if (!"player" %in% names(prj) && "name" %in% names(prj))
      setnames(prj, "name", "player")
  }
  list(game = g, team = tt, players = pl, projections = prj)
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

  # EVERY (game_id, pos_team) IS A CONTIGUOUS BLOCK because build_templates.R
  # ships the file keyed that way. So the drawn games' events are gathered by
  # integer slicing rather than 6,000 keyed joins -- the join alone was 2.4s per
  # 3,000 sims and was the single largest cost in the engine.
  BLK <- EV[, .(s = .I[1], e = .I[.N]), by = .(game_id, pos_team)]
  setkey(BLK, game_id, pos_team)

  say(sprintf("simulating %s games", format(n_sims, big.mark = ",")), 0.12)
  out <- vector("list", length(setup))

  for (si in seq_along(setup)) {
    cf <- setup[[si]]; tm <- cf$tm
    R <- cf$rec; S <- cf$rsh; nR <- nrow(R); nS <- nrow(S)
    pos <- setNames(R$route_base, R$player)
    if (nS) pos[S$player] <- fifelse(S$dk_pos == "QB", "QB", "RB")
    if (!is.na(cf$qb)) pos[cf$qb] <- "QB"
    if (!is.na(cf$k))  pos[cf$k]  <- "K"
    miss <- setdiff(cf$who, names(pos)); if (length(miss)) pos[miss] <- "WR"
    who <- cf$who; nW <- length(who)

    v_pyds <- draw[[paste0(cf$side, "pyds")]]
    v_ptd  <- draw[[paste0(cf$side, "ptd")]]
    v_pint <- draw[[paste0(cf$side, "pint")]]
    tcol   <- draw[[if (cf$side == "f") "fteam" else "dteam"]]

    # ---- gather every drawn game's events in one shot ------------------------
    sel <- BLK[data.table(sim = seq_len(n_sims), game_id = draw$game_id,
                          pos_team = tcol), on = .(game_id, pos_team)]
    sel[is.na(s), `:=`(s = 1L, e = 0L)]
    lens <- pmax(sel$e - sel$s + 1L, 0L)
    E2 <- EV[rep(sel$s, lens) + sequence(lens) - 1L]
    E2[, sim := rep(sel$sim, lens)]

    # ---- deal the catches ----------------------------------------------------
    # ONLY FIVE PROBABILITY VECTORS EXIST -- the bucket a catch falls in fully
    # determines the odds, so every completion in every simulated game is drawn
    # with five calls rather than one per event.
    rec <- NULL
    C2 <- E2[kind == CFB_EVT_CMP]
    if (nrow(C2) && nR) {
      C2[, b := cfb_bucket(yds)]
      PRB <- lapply(seq_len(5L), function(b) { p <- R$usage * cf$lr[, b]; p / sum(p) })
      C2[, w := NA_integer_]
      for (b in seq_len(5L)) {
        ii <- which(C2$b == b)
        if (length(ii)) set(C2, ii, "w", sample.int(nR, length(ii), TRUE, prob = PRB[[b]]))
      }
      rec <- C2[, .(rec = .N, ryds = sum(yds),
                    rtd = sum(td == 1L, na.rm = TRUE)), by = .(sim, w)]
      rec[, player := R$player[w]][, w := NULL]
    }

    # ---- deal the carries ----------------------------------------------------
    # Three situations, so three vectors. Goal-line is read off the event
    # (yards_to_goal <= 3) and short-yardage off down and distance -- the sheet
    # only says how LIKELY a man is once the situation arrives, never that he
    # owns it. Top man's realised goal-line share stays near the league's .455.
    rsh <- NULL
    R2 <- E2[kind == CFB_EVT_RUN]
    if (nrow(R2) && nS) {
      R2[, sit := fifelse(!is.na(ytg) & ytg <= 3L, 3L,
                  fifelse(!is.na(dn) & !is.na(dist) & dn >= 3L & dist <= 2L, 2L, 1L))]
      SPB <- list(S$carry_usage,
                  S$carry_usage * S$sy_tilt,
                  S$carry_usage * S$gl_tilt)
      SPB <- lapply(SPB, function(p) p / sum(p))
      R2[, w := NA_integer_]
      for (q in seq_len(3L)) {
        ii <- which(R2$sit == q)
        if (length(ii)) set(R2, ii, "w", sample.int(nS, length(ii), TRUE, prob = SPB[[q]]))
      }
      rsh <- R2[, .(car = .N, cyds = sum(yds),
                    ctd = sum(td == 1L, na.rm = TRUE)), by = .(sim, w)]
      rsh[, player := S$player[w]][, w := NULL]
    }

    # Sacks whole to whoever was in; kicks exactly as they happened.
    sk <- E2[kind == CFB_EVT_SACK, .(sk = sum(yds)), by = sim]
    fg <- E2[kind == CFB_EVT_FG & !is.na(made) & made == 1L,
             .(fg = sum(cfb_fg_points(ytg))), by = sim]

    # ---- assemble the full (sim x player) grid -------------------------------
    # Every player in every sim, because a zero counts: bust rate and the whole
    # left tail of the distribution live in the games a man did nothing.
    D <- CJ(sim = seq_len(n_sims), player = who, sorted = FALSE)
    if (!is.null(rec)) D <- merge(D, rec, by = c("sim","player"), all.x = TRUE)
    if (!is.null(rsh)) D <- merge(D, rsh, by = c("sim","player"), all.x = TRUE)
    for (cl in c("rec","ryds","rtd","car","cyds","ctd"))
      if (!cl %in% names(D)) D[, (cl) := 0] else D[is.na(get(cl)), (cl) := 0]

    D[, `:=`(pyds = 0, ptd = 0, pint = 0, fgp = 0, xp = 0, rettd = 0L)]
    if (!is.na(cf$qb)) {
      skv <- rep(0, n_sims); skv[sk$sim] <- sk$sk
      D[player == cf$qb, `:=`(cyds = cyds + skv[sim], pyds = v_pyds[sim],
                              ptd = v_ptd[sim], pint = v_pint[sim])]
    }
    if (!is.na(cf$k)) {
      fgv <- rep(0, n_sims); fgv[fg$sim] <- fg$fg
      tdv <- D[, .(t = sum(rtd) + sum(ctd)), by = sim]
      xpv <- rep(0, n_sims); xpv[tdv$sim] <- tdv$t
      D[player == cf$k, `:=`(fgp = fgv[sim], xp = xpv[sim])]
    }
    if (!is.na(cf$pr)) D[player == cf$pr & runif(.N) < CFB_PUNT_TD_RATE, rettd := rettd + 1L]
    if (!is.na(cf$kr)) D[player == cf$kr & runif(.N) < CFB_KICK_TD_RATE, rettd := rettd + 1L]

    # ---- fumbles: the drawn game's count, allocated across assigned touches ---
    # Vectorised by cumulative weight within each sim rather than a loop: draw
    # one uniform per fumble and find which player's slice it lands in.
    D[, tch := rec + car]
    if (!is.na(cf$qb)) D[player == cf$qb, tch := tch + 25]
    D[, fwt := tch * unname(CFB_FUM_RATE[pos[player]])]
    D[is.na(fwt), fwt := 0]
    fl <- FUM[data.table(game_id = draw$game_id, team = tcol),
              on = .(game_id, team)]$fl
    naf <- is.na(fl)
    if (any(naf)) fl[naf] <- sample(0:4, sum(naf), TRUE, prob = CFB_FUM_DIST)
    D[, fum := 0L]
    setorder(D, sim)
    D[, cw := cumsum(fwt), by = sim]
    tot <- D[, .(tw = max(cw)), by = sim]
    hit <- rep(seq_len(n_sims), fl)
    hit <- hit[tot$tw[hit] > 0]
    if (length(hit)) {
      u <- runif(length(hit)) * tot$tw[hit]
      key <- D[, .(sim, cw)]
      # rows are sim-major and contiguous, so the winner is the first cw >= u
      offs <- (hit - 1L) * nW
      w <- offs + vapply(seq_along(hit), function(j)
             which.max(key$cw[(offs[j]+1L):(offs[j]+nW)] >= u[j]), 1L)
      cnt <- tabulate(w, nbins = nrow(D))
      D[, fum := cnt]
    }
    D[, c("tch","fwt","cw") := NULL]
    D[, team := tm]
    out[[si]] <- D
    say(sprintf("%s dealt", tm), 0.12 + 0.7 * si / length(setup))
  }

  say("scoring", 0.88)
  A <- rbindlist(out)
  setnames(A, "sim", "SimID")   # the dealer works in `sim`; the app contract is SimID
  sc <- CFB_SCORE
  A[, dk := rec * sc$rec + ryds * sc$rec_yd + rtd * sc$rec_td +
            cyds * sc$rush_yd + ctd * sc$rush_td +
            fifelse(ryds >= 100, sc$rec_100, 0) +
            fifelse(cyds >= 100, sc$rush_100, 0) +
            pyds * sc$pass_yd + ptd * sc$pass_td + pint * sc$interception +
            fifelse(pyds >= 300, sc$pass_300, 0) +
            fgp + xp * sc$xp + rettd * sc$return_td + fum * sc$fumble_lost]

  # ---- app contract ----------------------------------------------------------
  meta <- unique(PL[, .(Player = player, Team = team, Pos = dk_pos,
                        DKID = as.integer(dk_id_util), DKCID = as.integer(dk_id_cpt),
                        DKSalary = as.integer(salary_util),
                        DKCSalary = as.integer(salary_cpt))])
  meta <- meta[Player %in% unique(A$player)]
  meta[, `:=`(DKProj = NA_real_, DKOwn = 0)]
  prj <- input_data$projections
  if (!is.null(prj) && nrow(prj)) {
    setDT(prj)
    if ("etr" %in% names(prj))
      meta[prj, DKProj := as.numeric(i.etr), on = .(Player = player)]
    if ("own" %in% names(prj))
      meta[prj, DKOwn := as.numeric(i.own), on = .(Player = player)]
  }

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

  # PLAYER TABLE -- SIM OUTPUTS ONLY. The sheet inputs that produced these live
  # in the workbook; repeating them here just makes the table wider than the
  # screen and invites reading a projection as if it were an assumption.
  stat_line <- A[, .(
      Rec = round(mean(rec), 2), RecYds = round(mean(ryds), 1),
      RecTD = round(mean(rtd), 3),
      Car = round(mean(car), 2), RushYds = round(mean(cyds), 1),
      RushTD = round(mean(ctd), 3),
      PassYds = round(mean(pyds), 1), PassTD = round(mean(ptd), 3),
      INT = round(mean(pint), 3),
      RetTD = round(mean(rettd), 4), Fum = round(mean(fum), 3),
      DK = round(mean(dk), 2),
      Floor = round(as.numeric(quantile(dk, .25)), 1),
      Ceil = round(as.numeric(quantile(dk, .90)), 1),
      Bust = round(100 * mean(dk < 3), 1),
      Boom = round(100 * mean(dk >= 20), 1)),
    by = .(Player = player, Team = team)]
  stat_line <- merge(stat_line, meta[, .(Player, Pos, Salary = DKSalary)],
                     by = "Player", all.x = TRUE)
  stat_line[, Val := round(DK / pmax(Salary, 1) * 1000, 2)]
  setcolorder(stat_line, c("Player","Team","Pos","Salary","DK","Floor","Ceil",
                           "Bust","Boom","Val"))
  setorder(stat_line, -DK)

  # TEAM LINE -- the whole box score, so a sheet that produces a nonsense
  # afternoon is visible before anyone reads a player row.
  team_line <- A[, .(Rec = sum(rec), RecYds = sum(ryds), RecTD = sum(rtd),
                     Car = sum(car), RushYds = sum(cyds), RushTD = sum(ctd),
                     PassYds = sum(pyds), PassTD = sum(ptd), INT = sum(pint),
                     Fum = sum(fum), FG = sum(fgp), XP = sum(xp)),
                 by = .(SimID, team)][
                , .(Rec = round(mean(Rec), 1), RecYds = round(mean(RecYds)),
                    Car = round(mean(Car), 1), RushYds = round(mean(RushYds)),
                    PassYds = round(mean(PassYds)),
                    PassTD = round(mean(PassTD), 2), RushTD = round(mean(RushTD), 2),
                    INT = round(mean(INT), 2), Fum = round(mean(Fum), 2),
                    KickPts = round(mean(FG + XP), 1),
                    DK = round(mean(Rec + RecYds * .1 + RecTD * 6 + RushYds * .1 +
                                    RushTD * 6 + PassYds * .04 + PassTD * 4), 1)),
                by = .(Team = team)]
  team_line[, `:=`(Implied = fifelse(Team == fav,
                     round(G$total[1]/2 + G$spread[1]/2, 1),
                     round(G$total[1]/2 - G$spread[1]/2, 1)),
                   ScrimYds = RecYds + RushYds)]
  team_line[, PassShare := round(PassYds / (PassYds + RushYds), 3)]
  setcolorder(team_line, c("Team","Implied","ScrimYds","PassShare"))

  # VIOLINS need the shape, not 20,000 rows a player. Downsample to a fixed
  # budget so the browser gets a few thousand points rather than half a million.
  nkeep <- min(1500L, n_sims)
  dist_sample <- A[, .(dk = if (.N > nkeep) dk[sample.int(.N, nkeep)] else dk),
                   by = .(Player = player, Team = team)]
  dist_sample <- merge(dist_sample, meta[, .(Player, Pos)], by = "Player", all.x = TRUE)

  # ---- validation: the mix, against real football --------------------------
  # NOT DISPLAYED IN THE APP. This answers "is this sheet believable", which is
  # a question for building the sheet rather than for reading the results, so
  # the consumer never sees it. Kept because when a slate looks wrong it is the
  # first thing to print: a usage vector can look plausible row by row and still
  # produce an afternoon nobody would recognise.
  vs   <- A[, .(catchers = sum(rec > 0), rushers = sum(car > 0),
                fum = sum(fum)), by = .(SimID, team)]
  ypcs <- A[rec >= 3, .(y = sum(ryds) / sum(rec)), by = .(SimID, team, player)]
  t3   <- A[, .(t = sum(rtd)), by = .(SimID, team)][t == 3]
  one3 <- A[t3, on = .(SimID, team)][, .(mx = max(rtd)), by = .(SimID, team)]
  vsum <- vs[,   .(Catchers = round(mean(catchers), 2),
                   Rushers  = round(mean(rushers), 2),
                   FumLost  = round(mean(fum), 3)), by = .(team)]
  ysum <- ypcs[, .(YPCp10 = round(quantile(y, .1), 1),
                   YPCp90 = round(quantile(y, .9), 1)), by = .(team)]
  tsum <- one3[, .(All3TD = round(100 * mean(mx == 3), 1), N = .N), by = .(team)]
  g <- function(D, col) D[match(c(fav, dog), D$team)][[col]]
  vrow <- function(metric, got, real, note)
    data.table(Check = metric, A = got[1], B = got[2], Real = real, Note = note)
  validation <- rbindlist(list(
    vrow("Distinct catchers / game", g(vsum,"Catchers"), "7.53",
         "too many means the usage vector is padded at the tail"),
    vrow("Distinct rushers / game",  g(vsum,"Rushers"),  "5.51",
         "low if the sheet names fewer backs than a real rotation"),
    vrow("Team fumbles lost / game", g(vsum,"FumLost"),  "0.564",
         "taken from the drawn game's box, not modelled"),
    vrow("Player YPC, 10th pct",     g(ysum,"YPCp10"),   "5.3",  "the tilt's low end"),
    vrow("Player YPC, 90th pct",     g(ysum,"YPCp90"),   "20.3", "the tilt's high end"),
    vrow("One man takes all 3 rec TD (%)", g(tsum,"All3TD"), "4.2",
         "KNOWN GAP: independent dealing alone gives ~3.2 and the affinity effect that lifts real football above it is not built"),
    vrow("3-TD games observed",      g(tsum,"N"),        "-",
         "sample for the row above -- under ~300 that check is noise")))
  setnames(validation, c("A","B"), c(fav, dog))

  sport_visuals <- list(
    score_dist = score_dist, stat_line = stat_line,
    dist_sample = dist_sample,
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
