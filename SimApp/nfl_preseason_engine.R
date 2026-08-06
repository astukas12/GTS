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

ps_allocate_rushing <- function(draw, carries, sheet, team_col) {
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

ps_allocate_receiving <- function(draw, recs, sheet, qb_sheet, team_col) {
  d  <- as.data.table(draw)[, .(sim_id, game_id, team = get(team_col))]
  cr <- merge(recs, d, by = c("game_id","team"), allow.cartesian = TRUE)
  if (!nrow(cr)) return(NULL)
  cr[, frac := (team_drive - 0.5) / n_team_drives]

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
  # The Sim tab names the matchup and IS the pool: there is no settings sheet,
  # because the spread and roster slot were inputs to BUILDING it and are never
  # consulted again. Deleting rows really does drop those games.
  simnm <- grep("^Sim_", names(input_data), value = TRUE, ignore.case = TRUE)[1]
  if (is.na(simnm)) stop("Preseason workbook has no Sim_<A>_vs_<B> sheet")
  pool <- ps_sheet(input_data, simnm)
  pool <- pool[!is.na(ps_num(Weight)) & ps_num(Weight) > 0]
  if (!nrow(pool)) stop("Sim sheet has no games with a positive Weight")

  tt <- strsplit(sub("^[Ss]im_", "", simnm), "_vs_", fixed = TRUE)[[1]]
  team_a <- tt[1]; team_b <- if (length(tt) > 1) tt[2] else NA_character_

  w <- ps_num(pool$Weight); w <- w/sum(w)
  say(sprintf("%s vs %s | %d games in pool | ESS %.0f | mean margin %+.2f",
              team_a, team_b, nrow(pool), 1/sum(w^2),
              sum(w * ps_num(pool$Margin_A))), 0.05)

  # ---- players ------------------------------------------------------------
  players <- rbindlist(lapply(c(team_a, team_b), function(tm) {
    d <- ps_sheet(input_data, tm)
    if (is.null(d)) stop("Workbook has no sheet for team ", tm)
    d[, Team := tm]
  }), fill = TRUE)
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

  # ---- draw ---------------------------------------------------------------
  set.seed(20260806)
  idx  <- sample.int(nrow(pool), n_sims, replace = TRUE, prob = w)
  draw <- data.table(sim_id = seq_len(n_sims),
                     game_id = pool$GameID[idx],
                     team_A  = pool$TeamA[idx],  team_B = pool$TeamB[idx],
                     kA = ps_num(pool$KickA)[idx], kB = ps_num(pool$KickB)[idx],
                     dA = ps_num(pool$DstA)[idx],  dB = ps_num(pool$DstB)[idx])
  for (c_ in c("kA","kB","dA","dB")) draw[[c_]][is.na(draw[[c_]])] <- 0

  # ---- allocate -----------------------------------------------------------
  out <- list()
  for (side in c("A","B")) {
    tm  <- if (side == "A") team_a else team_b
    col <- paste0("team_", side)
    sh  <- players[Team == tm]
    if (!nrow(sh)) next
    say(sprintf("allocating %s", tm), if (side == "A") 0.3 else 0.6)

    rb <- sh[Pos %in% c("QB","RB"),
             .(player = Player, pos = Pos, drive_start = DriveStart,
               drive_end = DriveEnd, weight = 1)]
    wr <- sh[Pos %in% c("WR","TE","RB"),
             .(player = Player, pos = Pos, drive_start = DriveStart,
               drive_end = DriveEnd, catch_w = CatchWeight,
               depth_z = if ("CatchDepth" %in% names(sh))
                           ps_depth_z(CatchDepth, Pos) else 0)]
    qb <- sh[Pos == "QB", .(player = Player, drive_start = DriveStart,
                            drive_end = DriveEnd)]

    ru <- if (nrow(rb)) ps_allocate_rushing(draw, carries, rb, col) else NULL
    rc <- if (nrow(wr) && nrow(qb)) ps_allocate_receiving(draw, recs, wr, qb, col) else NULL

    all_p <- data.table(player = sh$Player, Pos = sh$Pos, Team = tm)
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

    # Kicker and team defence are team-level totals that ride along with the
    # sampled game — no allocation, and no input required from the user.
    kcol <- if (side == "A") "kA" else "kB"
    dcol <- if (side == "A") "dA" else "dB"
    kv <- draw[[kcol]]
    grid[Pos == "K", `:=`(rush_yds = 0, rec_yds = 0, pass_yds = 0)]
    grid[, dk := ps_dk_score(.SD)]
    grid[, fd := ps_fd_score(.SD)]
    grid[Pos == "K", `:=`(dk = kv[sim_id], fd = kv[sim_id])]

    dst <- data.table(sim_id = seq_len(n_sims), player = paste(tm, "D/ST"),
                      Pos = "DST", Team = tm,
                      dk = draw[[dcol]], fd = draw[[dcol]])
    for (c_ in c("carries","rush_yds","rush_td","rec","rec_yds","rec_td",
                 "pass_cmp","pass_yds","pass_td")) dst[, (c_) := 0]
    out[[side]] <- rbind(grid, dst, fill = TRUE)
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
  metadata <- if (!is.null(ids)) unique(data.table(
      Player = ids$Name, Team = ids$Team, Pos = pick(ids, "Pos"),
      DKID  = pick(ids, "DKID", "DK_FLEX"),
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
  cover <- rbindlist(lapply(c(team_a, team_b), function(tm) {
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

  sport_visuals <- list(
    team_means = team_means, pos_means = pos_means,
    player_means = player_means, coverage = cover,
    pool_size = nrow(pool), n_sims = n_sims,
    ess = round(1/sum(w^2)),
    mean_margin = round(sum(w * ps_num(pool$Margin_A)), 2))

  say("done", 1)
  list(sim_results = sim_results, metadata = metadata, projections = projections,
       sport_visuals = sport_visuals)
}
