# =============================================================================
# NFL PRESEASON ENGINE — Golden Ticket Sims
# =============================================================================
# Entry point: run_nfl_preseason_simulation(input_data, n_sims, config)
#
# SELF-CONTAINED. No source(), no readRDS, no database. Everything the engine
# needs is either a constant below or a sheet in the uploaded workbook.
#
# HOW IT DIFFERS FROM THE REGULAR-SEASON NFL ENGINE
# Nothing here is generated from a distribution. Every simulated game IS a real
# preseason game — both teams, all drives — drawn from the Sim_<A>_vs_<B> tab,
# and every yard and touchdown in the output actually happened in that game. The
# engine's only job is deciding WHICH PLAYER each real carry and catch belongs
# to, based on the drive windows the user set.
#
# WORKBOOK CONTRACT
#   <TEAM_A>, <TEAM_B>  one row per player. A row's PRESENCE means he plays.
#                       Player, Pos, DriveStart, DriveEnd, CatchWeight,
#                       Mobility, CatchDepth
#   Sim_<A>_vs_<B>      the pool. One row per historical game with its sampling
#                       Weight, plus KickA/KickB/DstA/DstB. Delete rows to drop
#                       games; weights renormalise.
#   Plays_Rush          every carry: game_id, team, team_drive, n_team_drives,
#                       grp (QB/RB/OTH), y, td
#   Plays_Rec           every completion: game_id, team, team_drive,
#                       n_team_drives, y, td, bucket
#   IDs                 Name, Team, Pos, DKID, DKCID, FDID, DKSalary, FDSalary
#
# SALARY IS A CONSTANT, OWNERSHIP IS ZERO. A preseason slate prices every player
# identically ($7,600 DK / $8,000 FD), so the cap never binds -- six players cost
# 45,600 against a 50,000 cap and a 1.5x captain reaches only 49,400 -- and there
# is no ownership to project. The columns are still emitted because the
# optimiser, the downloads and the lineup tables all read them by name; they
# simply carry no information. Lineup selection therefore rests entirely on
# projection and the simulation's variance.
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# =============================================================================
# CONSTANTS — all measured over 11 preseasons (2014-2019, 2021-2025)
# =============================================================================

# Drive windows are expressed on an 11-drive scale and applied as FRACTIONS of
# each sampled game's real drive count. That matters: 42% of team-games have
# more than 11 drives (range 1-17, median 11), so an absolute mapping would drop
# the late ones. Quarters land almost exactly on quarters of the drive count:
# Q1 ends at 2.8, Q2 at 5.5, Q3 at 8.2, Q4 at 11.
PS_DRIVES <- 11

# Once a drive's primary back is chosen, each carry stays with him at this rate.
# Calibrated: dealing every carry independently produced a 37.7% shared-drive
# rate against the 21.8% actually observed. It changes CLUSTERING, not volume —
# the top back's share moves 0.290 to 0.292 across the whole 0-1 range.
PS_STICKINESS <- 0.80

# QB rush rate by mobility tier (carries / (carries + attempts)), from the
# quartile means. Used ONLY to blend a target for the pool; the engine never
# adds carries to a quarterback. A mobile QB's runs come OUT of his team's pass
# attempts, not on top of them — across quartiles total offence is flat
# (319/318/329/322) while +30 QB rush yards costs -47 team pass yards.
PS_MOB_TIER <- c(pocket = 0.033, mid = 0.085, mobile = 0.174)

# Catch-length buckets and P(position | length) / P(position). This is what
# stops a running back taking a 40-yard reception: backs are 1.68x likely on
# sub-5-yard catches and 0.44x beyond 20, receivers the reverse.
PS_CATCH_BREAKS <- c(-99, 4, 9, 19, 999)
PS_AFFINITY <- matrix(
  c(1.6849, 1.0225, 0.7208, 0.4425,     # RB
    0.9697, 1.0956, 0.9485, 0.8955,     # TE
    0.7278, 0.9551, 1.1348, 1.2697),    # WR
  nrow = 3, byrow = TRUE,
  dimnames = list(c("RB","TE","WR"), c("<5","5-9","10-19","20+")))

# Within-position catch depth. CatchDepth (yards per reception) is converted to
# a z against these position means, then tilts the catch MIX — a deep player is
# favoured on long balls and disfavoured on short ones, so his catch COUNT stays
# where the user put it. DEPTH_LAMBDA is calibrated: 0.35 opened a 4.6-yard gap
# between the +/-1.2 slots against an observed 2.1 and moved counts 20.8%; 0.14
# matches the yardage and moves counts under 5%.
PS_DEPTH_MU     <- c(WR = 12.6384, TE = 10.5238, RB = 7.6287)
PS_DEPTH_SD     <- c(WR =  1.1533, TE =  0.8165, RB = 0.5862)
PS_DEPTH_LAMBDA <- 0.14
PS_MEAN_CATCH   <- 11.3
PS_SD_CATCH     <- 11.0

# DraftKings / FanDuel scoring, including DK's per-game yardage bonuses.
# The slate's actual prices. Every player carries the same one, so the cap is
# never a constraint -- these exist so the optimiser has a number to add up.
PS_DK_SALARY <- 7600
PS_FD_SALARY <- 8000

ps_dk_score <- function(d) {
  0.04*d$pass_yds + 4*d$pass_td + 0.1*d$rush_yds + 6*d$rush_td +
  1*d$rec + 0.1*d$rec_yds + 6*d$rec_td +
  3*(d$pass_yds >= 300) + 3*(d$rush_yds >= 100) + 3*(d$rec_yds >= 100)
}
ps_fd_score <- function(d) {
  0.04*d$pass_yds + 4*d$pass_td + 0.1*d$rush_yds + 6*d$rush_td +
  0.5*d$rec + 0.1*d$rec_yds + 6*d$rec_td
}

# =============================================================================
# HELPERS
# =============================================================================

# Sheet names arrive with inconsistent casing depending on the reader, so every
# lookup is case-insensitive rather than trusting one spelling.
ps_sheet <- function(input_data, nm) {
  i <- match(tolower(nm), tolower(names(input_data)))
  if (is.na(i)) NULL else as.data.table(input_data[[i]])
}

ps_num <- function(x) suppressWarnings(as.numeric(x))

# Yards per reception -> within-position z.
ps_depth_z <- function(ypr, pos) {
  p <- ifelse(pos %in% names(PS_DEPTH_MU), pos, "WR")
  z <- (ps_num(ypr) - PS_DEPTH_MU[p]) / PS_DEPTH_SD[p]
  z[is.na(z)] <- 0
  as.numeric(z)
}

# Weighted pick, one row at a time, from a matrix of weights.
ps_wpick <- function(Wm, u) {
  cw <- Wm / rowSums(Wm)
  cw <- t(apply(cw, 1, cumsum))
  max.col(cw >= u, ties.method = "first")
}

# =============================================================================
# RUSHING
# -----------------------------------------------------------------------------
# The drive window is the ONLY rushing input. Backs eligible for a drive are
# treated equally, because nothing reliably separates them: depth-chart rank
# does not predict preseason carry volume (the volume leader averages drive 5.36
# and is in the first three drives only 28.4% of the time).
#
# Each drive draws one primary back, then PS_STICKINESS keeps carries with him.
# If windows do not overlap the whole mechanism is inert — one eligible back
# makes the pick deterministic and stickiness has nobody to switch to.
#
# Quarterback carries are not weighted or shared: the one QB whose window covers
# the drive takes every QB-tagged carry on it. Receiver jet sweeps ("OTH", 1.9%
# of carries) go to the backfield rather than being dropped — losing them would
# take their yards out of the team total entirely.
# =============================================================================

# Redistribute a team's QB carries across its quarterbacks by mobility tier,
# holding the TEAM TOTAL fixed. The pool already sets how many carries the team's
# quarterbacks get (that is what qb_rate_A/B matches on), so this adds nothing —
# it only decides WHICH of them gets the run. Without it the Mobility label is
# inert inside a team: windows do not overlap, so the QB on the field takes every
# QB carry on the drive, and since late drives carry more QB runs the backup
# out-rushed the starter regardless of tier (Richardson 1.62 to Leonard's 2.31).
#
# The model is a PLAY BUDGET, not an addition. Each quarterback's dropbacks plus
# carries are held fixed at what the sampled game gave him, and his tier decides
# how that budget splits: carries = r_i * plays_i, attempts = (1 - r_i) * plays_i.
# So a mobile QB's runs come OUT of his own pass attempts — the documented
# tradeoff (+30 QB rush yards costs -47 team pass yards, total offence flat) —
# and a team with a runner at QB now shows a run-leaning team stat line rather
# than the same line with the carries shuffled between its own quarterbacks.
#
# This finishes a job the pool can only start. Pool matching is attenuated by
# calibration (an IND request of 0.134 achieves 0.112), and it matches a
# TEAM-GAME average, which carries no information about which of three
# quarterbacks did the running. Setting the per-QB rate is not double-counting:
# the play-weighted blend of r_i reproduces the same team rate the pool asked
# for, it just lands the split on the right man.
#
# PS_COMP_RATE converts completions to attempts; only the ratio matters, so a
# preseason-typical 0.60 is enough.
PS_COMP_RATE <- 0.60
PS_MOB_KC <- c(lo = 0.50, hi = 2.50)   # carry multiplier clamp
PS_MOB_KA <- c(lo = 0.75, hi = 1.15)   # attempt multiplier clamp -- passing is
                                       # the big number, so it moves gently

# Solve both multipliers from the untilted baseline means.
ps_qb_play_budget <- function(carries, cmp, mob) {
  r <- mob; r[is.na(r)] <- PS_MOB_TIER[["mid"]]
  att   <- pmax(1e-6, cmp / PS_COMP_RATE)
  plays <- pmax(1e-6, carries + att)
  kc <- pmin(PS_MOB_KC[["hi"]], pmax(PS_MOB_KC[["lo"]], (r * plays)/pmax(1e-6, carries)))
  ka <- pmin(PS_MOB_KA[["hi"]], pmax(PS_MOB_KA[["lo"]], ((1 - r) * plays)/att))
  list(kc = kc, ka = ka)
}

# Thin or thicken rows to hit a per-player multiplier. Rows are the unit of
# scoring, so a duplicated carry is a fresh bootstrap draw from that same player's
# carries in the SAME sampled game, never a copy of one yardage repeated.
ps_rescale_rows <- function(dt, key_col, k) {
  if (!nrow(dt) || !length(k)) return(dt)
  kk <- k[dt[[key_col]]]; kk[is.na(kk)] <- 1
  if (all(abs(kk - 1) < 1e-9)) return(dt)
  cp <- floor(kk) + (runif(length(kk)) < (kk - floor(kk)))
  out <- dt[cp >= 1]
  ex  <- which(cp > 1)
  if (length(ex)) {
    reps <- rep(ex, cp[ex] - 1L)
    gk  <- paste(dt$game_id, dt[[key_col]])
    tmp <- data.table(gk = gk, i = seq_along(gk))
    setorder(tmp, gk)
    ix  <- tmp[, .(st = .I[1], cnt = .N), by = gk]
    # index ix by COLUMN, not by ix[j] -- a keyed/ordered data.table treats a
    # bare vector as a key lookup, which silently returned all-NA rows and
    # dropped every added carry (the tilt then only ever thinned).
    j   <- match(gk[reps], ix$gk)
    src <- tmp$i[ix$st[j] + floor(runif(length(j)) * ix$cnt[j])]
    dup <- dt[src]
    set(dup, j = key_col, value = dt[[key_col]][reps])
    out <- rbind(out, dup)
  }
  out
}

ps_allocate_rushing <- function(draw, carries, sheet, team_col, qb_kc = NULL) {
  d  <- as.data.table(draw)[, .(sim_id, game_id, team = get(team_col))]
  cr <- merge(carries, d, by = c("game_id","team"), allow.cartesian = TRUE)
  if (!nrow(cr)) return(NULL)
  cr[, frac := (team_drive - 0.5) / n_team_drives]

  sh <- as.data.table(sheet)
  sh[, `:=`(f0 = (drive_start - 1)/PS_DRIVES, f1 = drive_end/PS_DRIVES)]
  qb <- sh[pos == "QB"]; rb <- sh[pos == "RB"]

  qcr <- cr[grp == "QB"]
  if (nrow(qcr) && nrow(qb)) {
    qcr[, player := NA_character_]
    for (i in seq_len(nrow(qb)))
      qcr[is.na(player) & frac >= qb$f0[i] & frac < qb$f1[i], player := qb$player[i]]
    qcr <- qcr[!is.na(player)]
    if (!is.null(qb_kc)) qcr <- ps_rescale_rows(qcr, "player", qb_kc)
  } else { qcr <- qcr[0]; qcr[, player := character(0)] }

  rcr <- cr[grp %in% c("RB","OTH")]
  if (nrow(rcr) && nrow(rb)) {
    # Rows must be grouped by drive before the primary-back logic: it carries a
    # value forward with cumsum(), which mis-assigns if a drive's carries are
    # not contiguous after the merge.
    setorder(rcr, sim_id, team_drive)
    W <- matrix(0, nrow = nrow(rcr), ncol = nrow(rb))
    for (i in seq_len(nrow(rb)))
      W[, i] <- as.numeric(rcr$frac >= rb$f0[i] & rcr$frac < rb$f1[i]) * rb$weight[i]
    ok <- rowSums(W) > 0
    rcr[, player := NA_character_]

    key <- rcr[, paste(sim_id, team_drive)]
    first_of <- !duplicated(key)
    fo <- which(first_of)
    prim_drive <- integer(length(fo)); okd <- ok[fo]
    if (any(okd))
      prim_drive[okd] <- ps_wpick(W[fo[okd], , drop = FALSE], runif(sum(okd)))
    prim <- prim_drive[cumsum(first_of)]

    stay <- runif(nrow(rcr)) < PS_STICKINESS
    sel  <- prim
    alt  <- which(ok & !(first_of | stay))
    if (length(alt)) {
      Wa <- W[alt, , drop = FALSE]
      Wa[cbind(seq_along(alt), prim[alt])] <- 0        # exclude the primary
      keep <- rowSums(Wa) > 0
      if (any(keep))
        sel[alt[keep]] <- ps_wpick(Wa[keep, , drop = FALSE], runif(sum(keep)))
    }
    # A drive whose first carry had nobody eligible leaves sel == 0; those pick
    # fresh rather than indexing position zero.
    gap <- which(ok & sel == 0)
    if (length(gap)) sel[gap] <- ps_wpick(W[gap, , drop = FALSE], runif(length(gap)))
    kr <- ok & sel > 0
    rcr[kr, player := rb$player[sel[kr]]]
    rcr <- rcr[!is.na(player)]
  } else { rcr <- rcr[0]; rcr[, player := character(0)] }

  out <- rbind(qcr[, .(sim_id, player, y, td)], rcr[, .(sim_id, player, y, td)])
  out[, .(carries = .N, rush_yds = sum(y), rush_td = sum(td)),
      by = .(sim_id, player)]
}

# =============================================================================
# RECEIVING
# -----------------------------------------------------------------------------
# Every completion picks independently — receivers genuinely share a series, so
# unlike rushing there is no stickiness. For each catch:
#
#   weight = CatchWeight x Affinity[pos, length] x DepthTilt
#
# CatchWeight is a RELATIVE pull among whoever is on the field at that moment
# (1.0 typical, 2.0 twice as likely), not a team-wide share. A share summing to
# 1 across the roster cannot be read locally: 0.023 for a receiver who plays the
# last four drives says nothing about his odds when he is one of five men out
# there. The position catch rate (WR .581 / TE .670 / RB .754) is already folded
# into the number when the workbook is built.
#
# The quarterback on the field takes the passing line from his own drives
# outright — two QBs on one drive is vanishingly rare.
# =============================================================================

ps_allocate_receiving <- function(draw, recs, sheet, qb_sheet, team_col,
                                  qb_ka = NULL) {
  d  <- as.data.table(draw)[, .(sim_id, game_id, team = get(team_col))]
  cr <- merge(recs, d, by = c("game_id","team"), allow.cartesian = TRUE)
  if (!nrow(cr)) return(NULL)
  cr[, frac := (team_drive - 0.5) / n_team_drives]

  # The pass/rush split is decided per quarterback BEFORE receivers are drawn:
  # a play a mobile QB ran instead of threw never happened, so it has to leave
  # the receiver's line as well as the passer's.
  if (!is.null(qb_ka)) {
    qs <- as.data.table(qb_sheet)
    qs[, `:=`(f0 = (drive_start - 1)/PS_DRIVES, f1 = drive_end/PS_DRIVES)]
    cr[, .qbo := NA_character_]
    for (i in seq_len(nrow(qs)))
      cr[is.na(.qbo) & frac >= qs$f0[i] & frac < qs$f1[i], .qbo := qs$player[i]]
    cr <- ps_rescale_rows(cr[!is.na(.qbo)], ".qbo", qb_ka)
    cr[, .qbo := NULL]
    if (!nrow(cr)) return(NULL)
  }

  sh <- as.data.table(sheet)
  sh[, `:=`(f0 = (drive_start - 1)/PS_DRIVES, f1 = drive_end/PS_DRIVES)]

  bk <- if ("bucket" %in% names(cr)) as.integer(cr$bucket) else
        as.integer(cut(cr$y, PS_CATCH_BREAKS))
  bk[is.na(bk)] <- 2L
  ys <- pmax(-1.5, pmin(3, (cr$y - PS_MEAN_CATCH)/PS_SD_CATCH))
  ys[is.na(ys)] <- 0

  W <- matrix(0, nrow = nrow(cr), ncol = nrow(sh))
  for (i in seq_len(nrow(sh))) {
    p   <- if (sh$pos[i] %in% rownames(PS_AFFINITY)) sh$pos[i] else "WR"
    aff <- PS_AFFINITY[p, bk]
    W[, i] <- as.numeric(cr$frac >= sh$f0[i] & cr$frac < sh$f1[i]) *
              sh$catch_w[i] * aff * exp(PS_DEPTH_LAMBDA * sh$depth_z[i] * ys)
  }
  tot <- rowSums(W); ok <- tot > 0
  cr[, player := NA_character_]
  if (any(ok)) {
    cw   <- t(apply(W[ok, , drop = FALSE] / tot[ok], 1, cumsum))
    pick <- max.col(cw >= runif(sum(ok)), ties.method = "first")
    cr[ok, player := sh$player[pick]]
  }
  rec_out <- cr[!is.na(player), .(rec = .N, rec_yds = sum(y), rec_td = sum(td)),
                by = .(sim_id, player)]

  # The new column must NOT be called `qb`: data.table would resolve `qb` to it
  # rather than to the sheet, and `column$f0` fails with "$ operator is invalid
  # for atomic vectors".
  qbs <- as.data.table(qb_sheet)
  qbs[, `:=`(f0 = (drive_start - 1)/PS_DRIVES, f1 = drive_end/PS_DRIVES)]
  cr[, qb_on := NA_character_]
  for (i in seq_len(nrow(qbs)))
    cr[is.na(qb_on) & frac >= qbs$f0[i] & frac < qbs$f1[i], qb_on := qbs$player[i]]
  qb_out <- cr[!is.na(qb_on), .(pass_cmp = .N, pass_yds = sum(y), pass_td = sum(td)),
               by = .(sim_id, player = qb_on)]

  list(rec = rec_out, qb = qb_out)
}

# =============================================================================
# ENTRY POINT
# =============================================================================

run_nfl_preseason_simulation <- function(input_data, n_sims = 20000,
                                         config = NULL, progress_callback = NULL) {
  say <- function(msg, frac = NULL) {
    if (is.function(progress_callback)) try(progress_callback(msg, frac), silent = TRUE)
    message("[preseason] ", msg)
  }
  if (is.null(n_sims) || is.na(n_sims)) n_sims <- 20000

  # ---- the pool -----------------------------------------------------------
  # Two workbook shapes are accepted, and the tabs say which is which:
  #
  #   SHOWDOWN  a single Sim_<A>_vs_<B> tab that IS the pool for one game.
  #   CLASSIC   a Pool tab holding every game's pool stacked, keyed by GameKey,
  #             plus a Games tab listing the matchups.
  #
  # Neither carries a settings sheet: the spread and roster slot were inputs to
  # BUILDING a pool and are never consulted again, so deleting rows from the
  # pool really does drop those games from the projection.
  pooltab <- ps_sheet(input_data, "Pool")
  is_classic <- !is.null(pooltab) && "GameKey" %in% names(pooltab)

  if (is_classic) {
    pool <- pooltab[!is.na(ps_num(Weight)) & ps_num(Weight) > 0]
    if (!nrow(pool)) stop("Pool sheet has no rows with a positive Weight")
    gm <- unique(pool[, .(GameKey, HomeTeam, AwayTeam)])
    say(sprintf("classic slate: %d games, %s pool rows",
                nrow(gm), format(nrow(pool), big.mark = ",")), 0.05)
  } else {
    simnm <- grep("^Sim_", names(input_data), value = TRUE, ignore.case = TRUE)[1]
    if (is.na(simnm)) stop("Preseason workbook has no Pool or Sim_<A>_vs_<B> sheet")
    pool <- ps_sheet(input_data, simnm)
    pool <- pool[!is.na(ps_num(Weight)) & ps_num(Weight) > 0]
    if (!nrow(pool)) stop("Sim sheet has no games with a positive Weight")
    tt <- strsplit(sub("^[Ss]im_", "", simnm), "_vs_", fixed = TRUE)[[1]]
    pool[, `:=`(GameKey = simnm, HomeTeam = tt[1],
                AwayTeam = if (length(tt) > 1) tt[2] else NA_character_)]
    gm <- unique(pool[, .(GameKey, HomeTeam, AwayTeam)])
    w <- ps_num(pool$Weight); w <- w/sum(w)
    say(sprintf("%s vs %s | %d games in pool | ESS %.0f | mean margin %+.2f",
                gm$HomeTeam[1], gm$AwayTeam[1], nrow(pool), 1/sum(w^2),
                sum(w * ps_num(pool$Margin_A))), 0.05)
  }
  teams_all <- unique(c(gm$HomeTeam, gm$AwayTeam))
  teams_all <- teams_all[!is.na(teams_all)]

  # Diagnostic per game: how much of the history each pool really draws on, and
  # whether the weighting landed on the line it was given.
  pool_diag <- pool[, {
    ww <- ps_num(Weight); ww <- ww/sum(ww)
    .(Games = .N, ESS = round(1/sum(ww^2)),
      Spread = round(ps_num(Spread_A)[1], 1),
      MeanMargin = round(sum(ww * ps_num(Margin_A)), 2),
      MeanTotal  = round(sum(ww * ps_num(Total)), 1))
  }, by = GameKey]
  pool_diag[, Miss := round(MeanMargin - Spread, 2)]

  # ---- players ------------------------------------------------------------
  players <- rbindlist(lapply(teams_all, function(tm) {
    d <- ps_sheet(input_data, tm)
    if (is.null(d)) stop("Workbook has no sheet for team ", tm)
    d[, Team := tm]
  }), fill = TRUE)
  # The classic sheet keeps non-playing rows for reference, flagged in Status.
  if ("Status" %in% names(players))
    players <- players[!Status %in% c("OUT","SIT","NOT ON DEPTH CHART")]
  # A row's PRESENCE is the flag — there is no Plays column.
  if ("Plays" %in% names(players))
    players <- players[toupper(as.character(Plays)) %in% c("TRUE","1","YES")]
  for (v in intersect(c("DriveStart","DriveEnd","CatchWeight","CatchDepth"),
                      names(players)))
    players[, (v) := ps_num(get(v))]
  if (!"CatchWeight" %in% names(players)) players[, CatchWeight := 1]
  players[is.na(DriveStart), DriveStart := 1]
  players[is.na(DriveEnd),   DriveEnd   := PS_DRIVES]

  # ---- play data ----------------------------------------------------------
  carries <- ps_sheet(input_data, "Plays_Rush")
  recs    <- ps_sheet(input_data, "Plays_Rec")
  if (is.null(carries) || is.null(recs))
    stop("Preseason workbook is missing Plays_Rush / Plays_Rec")
  for (cl in c("team_drive","n_team_drives","y","td")) {
    if (cl %in% names(carries)) carries[, (cl) := ps_num(get(cl))]
    if (cl %in% names(recs))    recs[,    (cl) := ps_num(get(cl))]
  }

  # ---- draw and allocate, one game at a time -------------------------------
  # Every game is drawn independently under a SHARED SimID. That is what lets a
  # classic lineup combine players from different games while each game keeps
  # its own betting line, roster slot and quarterback mobility. It also makes
  # cross-game correlation exactly zero, which is what the history shows --
  # opposing team totals inside a game correlate -0.253, but separate games
  # have no mechanism linking them at all.
  set.seed(20260806)
  out <- list()
  for (gi in seq_len(nrow(gm))) {
    gk <- gm$GameKey[gi]; ha <- gm$HomeTeam[gi]; aw <- gm$AwayTeam[gi]
    gp <- pool[GameKey == gk]
    if (!nrow(gp)) next
    w <- ps_num(gp$Weight); w <- w/sum(w)
    idx <- sample.int(nrow(gp), n_sims, replace = TRUE, prob = w)
    draw <- data.table(sim_id = seq_len(n_sims),
                       game_id = gp$GameID[idx],
                       team_A = gp$TeamA[idx], team_B = gp$TeamB[idx],
                       kA = ps_num(gp$KickA)[idx], kB = ps_num(gp$KickB)[idx],
                       dA = ps_num(gp$DstA)[idx], dB = ps_num(gp$DstB)[idx])
    for (c_ in c("kA","kB","dA","dB")) draw[[c_]][is.na(draw[[c_]])] <- 0
    say(sprintf("allocating %s", gk), 0.1 + 0.75 * gi/nrow(gm))

    for (side in c("A","B")) {
      tm  <- if (side == "A") ha else aw
      col <- paste0("team_", side)
      sh  <- players[Team == tm]
      if (!nrow(sh)) next

      rb <- sh[Pos %in% c("QB","RB"),
               .(player = Player, pos = Pos, drive_start = DriveStart,
                 drive_end = DriveEnd, weight = 1,
                 mob = if ("Mobility" %in% names(sh))
                         PS_MOB_TIER[tolower(trimws(Mobility))] else NA_real_)]
      wr <- sh[Pos %in% c("WR","TE","RB"),
               .(player = Player, pos = Pos, drive_start = DriveStart,
                 drive_end = DriveEnd, catch_w = CatchWeight,
                 depth_z = if ("CatchDepth" %in% names(sh))
                             ps_depth_z(CatchDepth, Pos) else 0)]
      qb <- sh[Pos == "QB", .(player = Player, drive_start = DriveStart,
                              drive_end = DriveEnd)]

      # Calibrate the mobility play-budget on a subsample: the multipliers only
      # need the BASELINE means, and 3,000 draws pin those down well inside the
      # noise of the tiers themselves.
      kc <- ka <- NULL
      qmob <- rb[pos == "QB"]
      if (nrow(qmob) && "mob" %in% names(qmob) && !all(is.na(qmob$mob)) &&
          nrow(wr) && nrow(qb)) {
        cal <- draw[seq_len(min(nrow(draw), 3000L))]
        b_ru <- ps_allocate_rushing(cal, carries, rb, col)
        b_rc <- ps_allocate_receiving(cal, recs, wr, qb, col)
        if (!is.null(b_ru) && !is.null(b_rc)) {
          nc  <- uniqueN(cal$sim_id)
          bc  <- b_ru[player %in% qmob$player, .(v = sum(carries)/nc),  by = player]
          bp  <- b_rc$qb[,                      .(v = sum(pass_cmp)/nc), by = player]
          cv  <- setNames(bc$v, bc$player)[qmob$player]; cv[is.na(cv)] <- 0
          pv  <- setNames(bp$v, bp$player)[qmob$player]; pv[is.na(pv)] <- 0
          ok  <- cv > 0.02 & pv > 0.1
          if (any(ok)) {
            kb <- ps_qb_play_budget(cv[ok], pv[ok], qmob$mob[ok])
            kc <- setNames(kb$kc, qmob$player[ok])
            ka <- setNames(kb$ka, qmob$player[ok])
          }
        }
      }

      ru <- if (nrow(rb)) ps_allocate_rushing(draw, carries, rb, col, kc) else NULL
      rc <- if (nrow(wr) && nrow(qb))
              ps_allocate_receiving(draw, recs, wr, qb, col, ka) else NULL

      # D/ST is SYNTHESIZED below from the sampled game's real defensive score.
      # The team sheet also carries a D/ST row, and leaving it here produced a
      # second row per sim carrying 0 (a defence has no rushing or receiving
      # stats to score). The two rows then averaged, halving every D/ST: a real
      # 6.7-point position came out at 3.4 with a median of zero.
      all_p <- data.table(player = sh$Player, Pos = sh$Pos, Team = tm)[Pos != "DST"]
      grid  <- all_p[rep(seq_len(.N), each = n_sims)]
      grid[, sim_id := rep(seq_len(n_sims), times = nrow(all_p))]
      if (!is.null(ru)) grid <- merge(grid, ru, by = c("sim_id","player"), all.x = TRUE)
      if (!is.null(rc)) {
        grid <- merge(grid, rc$rec, by = c("sim_id","player"), all.x = TRUE)
        grid <- merge(grid, rc$qb,  by = c("sim_id","player"), all.x = TRUE)
      }
      for (c_ in c("carries","rush_yds","rush_td","rec","rec_yds","rec_td",
                   "pass_cmp","pass_yds","pass_td"))
        if (!c_ %in% names(grid)) grid[, (c_) := 0] else grid[is.na(get(c_)), (c_) := 0]

      kcol <- if (side == "A") "kA" else "kB"
      dcol <- if (side == "A") "dA" else "dB"
      kv <- draw[[kcol]]
      grid[Pos == "K", `:=`(rush_yds = 0, rec_yds = 0, pass_yds = 0)]
      grid[, dk := ps_dk_score(.SD)]
      grid[, fd := ps_fd_score(.SD)]
      # KICKERS SPLIT THE TEAM'S KICKING OUTPUT, they do not each receive it.
      # kv is the whole team's kicker score from the sampled game; assigning it
      # flat to every K meant a two-kicker team scored its field goals twice --
      # live on this slate for Indianapolis, Pittsburgh and Las Vegas, and
      # Indianapolis is in a showdown where kickers are rosterable. Split it by
      # the share of drives each man is on the field for.
      kk <- sh[Pos == "K"]
      if (nrow(kk) <= 1) {
        grid[Pos == "K", `:=`(dk = kv[sim_id], fd = kv[sim_id])]
      } else {
        span <- pmax(1, ps_num(kk$DriveEnd) - ps_num(kk$DriveStart) + 1)
        frac <- span / sum(span)
        for (i in seq_len(nrow(kk)))
          grid[Pos == "K" & player == kk$Player[i],
               `:=`(dk = kv[sim_id] * frac[i], fd = kv[sim_id] * frac[i])]
      }
      grid[, GameKey := gk]

      dst <- data.table(sim_id = seq_len(n_sims), player = paste(tm, "D/ST"),
                        Pos = "DST", Team = tm, GameKey = gk,
                        dk = draw[[dcol]], fd = draw[[dcol]])
      for (c_ in c("carries","rush_yds","rush_td","rec","rec_yds","rec_td",
                   "pass_cmp","pass_yds","pass_td")) dst[, (c_) := 0]
      out[[paste(gk, side)]] <- rbind(grid, dst, fill = TRUE)
    }
  }
  res <- rbindlist(out, fill = TRUE)
  say("shaping output", 0.9)

  # ---- app contract -------------------------------------------------------
  sim_results <- res[, .(SimID = sim_id, Player = player, Team = Team,
                         DKScore = dk, FDScore = fd)]

  ids <- ps_sheet(input_data, "IDs")
  # Accept either naming, so a workbook built before the rename still loads.
  pick <- function(d, ...) {
    for (cl in c(...)) if (!is.null(d) && cl %in% names(d)) return(as.character(d[[cl]]))
    NA_character_
  }
  npick <- function(d, ..., dflt) {
    for (cl in c(...)) if (!is.null(d) && cl %in% names(d)) {
      v <- ps_num(d[[cl]]); if (any(!is.na(v))) return(v)
    }
    rep(dflt, if (is.null(d)) 0L else nrow(d))
  }
  # A classic workbook names these Player/DK_ID/FD_ID; a showdown one names them
  # Name/DKID/DKCID. Reading only one spelling drops every id silently -- the
  # sim still runs and the downloads come out blank -- so accept both.
  metadata <- if (!is.null(ids)) unique(data.table(
      Player = pick(ids, "Player", "Name"), Team = ids$Team, Pos = pick(ids, "Pos"),
      DKID  = pick(ids, "DKID", "DK_FLEX", "DK_ID"),
      DKCID = pick(ids, "DKCID", "DK_CPT"),
      FDID  = pick(ids, "FDID", "FD_ID"),
      # Constants, not information -- but the optimiser reads them by name.
      DKSalary  = npick(ids, "DKSalary",  dflt = PS_DK_SALARY),
      FDSalary  = npick(ids, "FDSalary",  dflt = PS_FD_SALARY),
      DKCSalary = npick(ids, "DKCSalary", dflt = PS_DK_SALARY * 1.5),
      FDMSalary = npick(ids, "FDMSalary", dflt = PS_FD_SALARY * 1.5),
      # Ownership if the workbook carries it, zero if not. Total and CAPTAIN
      # ownership are different quantities on a showdown slate -- a player at
      # 83% overall may be captained by only 28% -- and the gap between them is
      # the leverage. Both are passed through untouched.
      DKOwn  = npick(ids, "DKOwn",  dflt = 0),
      DKCOwn = npick(ids, "DKCOwn", dflt = 0),
      FDOwn  = npick(ids, "FDOwn",  "DKOwn",  dflt = 0),
      FDMOwn = npick(ids, "FDMOwn", "DKCOwn", dflt = 0),
      # ETR's own projection, carried untouched. The app renames DKProj to
      # "ETR" in the projections table, which is what puts their number beside
      # the sim's average. Nothing in the engine reads it.
      DKProj = npick(ids, "ETR_Proj", "DKProj", dflt = NA_real_),
      FDProj = npick(ids, "ETR_Proj", "FDProj", dflt = NA_real_))) else
    unique(res[, .(Player = player, Team, Pos)])
  # Anyone the sim scores but the IDs tab does not name still needs a row, or
  # the optimiser silently drops him.
  miss <- setdiff(unique(sim_results$Player), metadata$Player)
  if (length(miss)) {
    add <- unique(res[player %in% miss, .(Player = player, Team, Pos)])
    add[, `:=`(DKSalary = PS_DK_SALARY, FDSalary = PS_FD_SALARY,
               DKCSalary = PS_DK_SALARY * 1.5, FDMSalary = PS_FD_SALARY * 1.5,
               DKOwn = 0, FDOwn = 0, DKCOwn = 0, FDMOwn = 0)]
    metadata <- rbindlist(list(metadata, add), fill = TRUE)
  }
  for (cl in c("DKSalary","FDSalary","DKCSalary","FDMSalary"))
    metadata[is.na(get(cl)) | get(cl) == 0,
             (cl) := if (cl %in% c("DKSalary")) PS_DK_SALARY
                     else if (cl == "FDSalary") PS_FD_SALARY
                     else if (cl == "DKCSalary") PS_DK_SALARY * 1.5
                     else PS_FD_SALARY * 1.5]
  for (cl in c("DKOwn","FDOwn","DKCOwn","FDMOwn"))
    metadata[is.na(get(cl)), (cl) := 0]
  # The shared SD optimiser indexes salary as SDSalary/CPTSalary regardless of
  # sport. Preseason prices everyone the same, so these are constants -- but
  # they have to exist or the merge drops every row.
  metadata[, `:=`(SDSalary = DKSalary, CPTSalary = DKCSalary)]

  # SHOWDOWN IDENTITY. On a classic slate a showdown contest is one of the
  # games, so a player belongs to at most one -- his team decides it. The app's
  # SD picker filters metadata by ShowdownFile, and the download needs that
  # game's own captain/flex ids, which are NOT the classic ones.
  gsd <- ps_sheet(input_data, "Games")
  if (!is.null(gsd) && "ShowdownFile" %in% names(gsd)) {
    gsd <- gsd[!is.na(ShowdownFile) & nzchar(as.character(ShowdownFile))]
    if (nrow(gsd)) {
      tm2sd <- rbind(gsd[, .(Team = HomeTeam, ShowdownFile, GameKey)],
                     gsd[, .(Team = AwayTeam, ShowdownFile, GameKey)])
      metadata[, ShowdownFile := tm2sd$ShowdownFile[match(Team, tm2sd$Team)]]
      metadata[, GameKey      := tm2sd$GameKey[match(Team, tm2sd$Team)]]
      # SD1_CPT / SD1_FLEX ... one column pair per showdown contest.
      if (!is.null(ids)) for (sf in unique(tm2sd$ShowdownFile)) {
        cc <- paste0(sf, "_CPT"); fc <- paste0(sf, "_FLEX")
        if (!all(c(cc, fc) %in% names(ids))) next
        nm <- pick(ids, "Player", "Name")
        j  <- match(metadata$Player, nm)
        ok <- which(!is.na(j) & !is.na(metadata$ShowdownFile) &
                    metadata$ShowdownFile == sf)
        metadata[ok, `:=`(SDCID = as.character(ids[[cc]])[j[ok]],
                          SDID  = as.character(ids[[fc]])[j[ok]])]
      }
    }
  }

  projections <- res[, .(Sim_DK_Mean   = mean(dk),
                         Sim_DK_Median = as.numeric(median(dk)),
                         Sim_DK_StdDev = sd(dk),
                         Sim_DK_p90    = as.numeric(quantile(dk, 0.90)),
                         Sim_FD_Mean   = mean(fd),
                         Sim_FD_StdDev = sd(fd)),
                     by = .(Player = player, Team, Pos)][order(-Sim_DK_Mean)]

  # ---- validation visuals -------------------------------------------------
  # Team totals are the first sanity check: they should look like a preseason
  # game, not a regular-season one. The per-position split and the sim-vs-ETR
  # comparison are the two places a bad drive window shows itself.
  team_means <- res[, .(
      DK      = round(sum(dk)/n_sims, 1),
      PassYds = round(sum(pass_yds)/n_sims, 1),
      PassTD  = round(sum(pass_td)/n_sims, 2),
      Carries = round(sum(carries)/n_sims, 1),
      RushYds = round(sum(rush_yds)/n_sims, 1),
      Rec     = round(sum(rec)/n_sims, 1),
      RecYds  = round(sum(rec_yds)/n_sims, 1),
      TD      = round(sum(rush_td + rec_td)/n_sims, 2)),
    by = .(Team)][order(-DK)]

  pos_means <- res[Pos %in% c("QB","RB","WR","TE","K","DST"), .(
      Players = uniqueN(player),
      DK      = round(sum(dk)/n_sims, 1),
      Carries = round(sum(carries)/n_sims, 1),
      RushYds = round(sum(rush_yds)/n_sims, 1),
      Rec     = round(sum(rec)/n_sims, 1),
      RecYds  = round(sum(rec_yds)/n_sims, 1)),
    by = .(Team, Pos)][order(Team, -DK)]

  player_means <- merge(
    projections[, .(Player, Team, Pos, Sim = round(Sim_DK_Mean, 2),
                    SD = round(Sim_DK_StdDev, 2), P90 = round(Sim_DK_p90, 1))],
    metadata[, .(Player, ETR = ps_num(DKProj))], by = "Player", all.x = TRUE)
  player_means[, Diff := round(Sim - ETR, 2)]
  setorder(player_means, -Sim)

  # A drive nobody's window covers silently discards its plays -- it surfaces as
  # a quietly low projection rather than an error, so it is reported here.
  cover <- rbindlist(lapply(teams_all, function(tm) {
    sh <- players[Team == tm]
    if (!nrow(sh)) return(NULL)
    rbindlist(lapply(seq_len(PS_DRIVES), function(dv) {
      fr <- (dv - 0.5)/PS_DRIVES
      on <- sh[fr >= (DriveStart - 1)/PS_DRIVES & fr < DriveEnd/PS_DRIVES]
      data.table(Team = tm, Drive = dv,
                 QB = paste(on[Pos == "QB"]$Player, collapse = "/"),
                 RBs = nrow(on[Pos == "RB"]),
                 Receivers = nrow(on[Pos %in% c("WR","TE")]))
    }))
  }))
  cover[, Gap := RBs == 0 | Receivers == 0 | QB == ""]

  # SCORE RANGE per player. The mean alone hides the thing that matters on a
  # showdown slate -- two players with the same average can have very different
  # ceilings, and the ceiling is what wins. Quantiles are precomputed here
  # rather than shipping 800k rows to the browser.
  score_dist <- res[, .(
      Mean   = round(mean(dk), 2),
      P10    = round(as.numeric(quantile(dk, 0.10)), 1),
      P25    = round(as.numeric(quantile(dk, 0.25)), 1),
      Median = round(as.numeric(median(dk)), 1),
      P75    = round(as.numeric(quantile(dk, 0.75)), 1),
      P90    = round(as.numeric(quantile(dk, 0.90)), 1),
      Max    = round(max(dk), 1)),
    by = .(Player = player, Team, Pos)][order(Team, -Mean)]

  # PLAYER STAT LINE with the INPUTS that produced it sitting alongside. That
  # pairing is the point: if a receiver's catches look wrong, the drive window
  # and catch weight that caused it are on the same row, so the fix is visible
  # without cross-referencing the workbook.
  stat_line <- res[, .(
      Carries = round(sum(carries)/n_sims, 2),
      RushYds = round(sum(rush_yds)/n_sims, 1),
      Rec     = round(sum(rec)/n_sims, 2),
      RecYds  = round(sum(rec_yds)/n_sims, 1),
      PassCmp = round(sum(pass_cmp)/n_sims, 2),
      PassYds = round(sum(pass_yds)/n_sims, 1),
      TD      = round(sum(rush_td + rec_td + pass_td)/n_sims, 3),
      DK      = round(mean(dk), 2)),
    by = .(Player = player, Team, Pos)]
  inputs <- players[, .(Player, Drives = paste0(DriveStart, "-", DriveEnd),
                        CatchWeight = round(ps_num(CatchWeight), 2),
                        Mobility = if ("Mobility" %in% names(players)) Mobility else NA_character_)]
  stat_line <- merge(stat_line, inputs, by = "Player", all.x = TRUE)
  stat_line <- merge(stat_line, metadata[, .(Player, ETR = ps_num(DKProj))],
                     by = "Player", all.x = TRUE)
  stat_line[, Diff := round(DK - ETR, 2)]
  setcolorder(stat_line, c("Player","Team","Pos","Drives","CatchWeight","Mobility",
                           "Carries","RushYds","Rec","RecYds","PassCmp","PassYds",
                           "TD","DK","ETR","Diff"))
  setorder(stat_line, Team, -DK)

  sport_visuals <- list(
    team_means = team_means, pos_means = pos_means,
    player_means = player_means, coverage = cover,
    score_dist = score_dist, stat_line = stat_line,
    pool_size = nrow(pool), n_sims = n_sims,
    # Per game, because on a classic slate each game carries its own line and
    # its own pool -- one slate-wide ESS would average away the game that is
    # actually short of diversity.
    pool_diag = pool_diag,
    ess = round(mean(pool_diag$ESS)),
    mean_margin = round(mean(pool_diag$MeanMargin), 2))

  say("done", 1)
  list(sim_results = sim_results, metadata = metadata, projections = projections,
       sport_visuals = sport_visuals)
}
