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
#   0-2 / 3-7 / 8-15 / 16-30 / 31+
#                P(he caught it | the catch went that far). One column per
#                reception-yardage band, each summing to 1 per team. This IS
#                the deal -- there is no likelihood, no tilt and no league mix
#                between what is typed and who gets the ball. `usage` is
#                DERIVED from these (the band shares weighted by how often each
#                band occurs) and `ypc` is a readback, not an input.
#   carry_usage  P(handed any given NORMAL designed carry). Sums to 1.
#   sy_share     P(handed a short-yardage carry: dn >= 3 & dist <= 2). Sums
#                to 1. Blank means "same as carry_usage".
#   gl_share     P(handed a goal-line carry: ytg <= 3). Sums to 1. Blank means
#                "same as carry_usage".
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
# WHY SHARES AND NOT TILTS (1 September 2026). sy_tilt and gl_tilt multiplied
# carry_usage and were never centred, so they could not say "this man is the
# goal-line back" without also saying "this man plays". NCST priced Will Wilson
# as a goal-line package quarterback; carry_usage .12 with gl_tilt 1.60
# delivered him 4.00 carries, 19.0 rushing yards and a 20-yard run in 37.4% of
# games, for a man who runs about once. Under shares he is carry_usage .01,
# gl_share .35 and reads as what he is.
#
# Measured over 286 team-seasons of play-by-play, goal-line share tracks normal
# share at cor .801 and slope .947 -- the role is very nearly pure volume, and
# true specialists (>=15% of goal-line carries on <5% of normal) are 40 rows in
# 4,345, about one team in seven, half of them quarterbacks. So the schema has
# to express the exception WITHOUT taxing the other 99%: a blank share falls
# back to carry_usage and the deal is unchanged.
#
# Deleted at the same time: big_run and stuff_rate, which no version of this
# engine ever read. Within-team rushing efficiency is not forecastable -- the
# RB1-RB2 YPC gap is 8.3% real, a true 0.35 yds/carry or about 0.6 DK points,
# and the sampling noise that makes up the rest is something the dealer already
# reproduces. carry_usage alone delivers 96% of the true spread.
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
# The sheet's catch columns, one per bucket, in bucket order. NAMED FOR THE
# YARDAGE THEY HOLD, because a column called `c_int` needs a legend and a
# column called `16-30` does not. Each holds that band's SHARE and each sums to
# 1 down the team, exactly like carry_usage. See the schema note below.
CFB_BAND_COLS <- c("0-2", "3-7", "8-15", "16-30", "31+")
CFB_BUCKET_MID <- c(-0.32, 5.11, 10.91, 21.09, 44.17)  # ACTUAL bucket means
CFB_LEAGUE_MIX <- c(0.122, 0.312, 0.322, 0.173, 0.072)
CFB_BASE_MIX <- list(
  WR = c(.087, .287, .347, .193, .086),
  TE = c(.094, .328, .337, .192, .050),
  RB = c(.229, .360, .273, .103, .035))

# CONCENTRATION FOR THE PER-GAME BACKFIELD DIRICHLET. A team's split is not the
# same every Saturday, and multinomial dealing alone is too tidy: the observed
# game-to-game variance of the top back's carry share is 2.13x what dealing
# from a fixed vector produces. Solving 1 + n/(a0+1) = 2.13 at n ~ 35 designed
# runs a game gives a0 ~ 30. Applied to the NORMAL vector only -- short yardage
# and the goal line are 2.5 and 1.25 carries a game, where multinomial noise
# already dominates and a Dirichlet on top would be noise on noise.
#
# OPEN: 30 LOOKS TOO HIGH, and the reason is that the 2.13 was measured against
# pure multinomial dealing at a fixed carry count, which is not what this engine
# was doing beforehand. The drawn game already varies both the carry total and
# the situation mix, so the engine carried a dispersion ratio of 1.41 before any
# Dirichlet at all. Measured on the same scale (observed share variance over
# p(1-p)/n at the mean carry count):
#
#   real football, 3,298 player-seasons   2.87 weighted, 3.19 for top backs
#   engine, a0 = 30                       2.00 weighted, 2.21
#   engine, a0 = 14                       2.69 weighted, 2.90
#   engine, a0 = 10                       3.12 weighted, 3.24
#
# so the fitted value is nearer a0 = 12. Left at 30 because 30 is what was
# agreed; changing it is a decision, not an implementation detail. Measured on
# the UNC/TCU showdown sheet only -- confirm on a classic slate before moving it.
CFB_CARRY_A0 <- 30

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
#
# MULTI-SLATE WORKBOOK. A sheet whose `game` tab carries `in_classics` holds
# every contest DK posted for the card at once -- the four-game main, a
# two-game main, and a showdown on each game -- as ONE full-card game tab plus
# a long `projections` tab keyed by slate. Pick the contest to simulate:
#   read_cfb_input(path)                          -> classic_main (the superset)
#   read_cfb_input(path, "classic_2")             -> the two-game main
#   read_cfb_input(path, "SD", game = "COLGT")    -> that game's showdown
# A legacy single-slate sheet has no `in_classics` column and `slate`/`game`
# are ignored -- it loads exactly as before.
read_cfb_input <- function(file_path, slate = NULL, game = NULL) {
  sh <- readxl::excel_sheets(file_path)
  gtab <- sh[tolower(sh) == "game"]
  if (!length(gtab)) stop("CFB workbook needs a `game` tab")
  g <- as.data.table(readxl::read_excel(file_path, sheet = gtab[1]))
  # THE TAB NAME IS THE TEAM, so every non-`game` tab is a team -- except the
  # optional projections tab, which must be excluded here or it is read as a
  # team and blows up on the missing S:T block.
  aux <- sh[tolower(sh) %in% c("projections", "etr")]
  tms <- setdiff(sh, c(gtab, aux))

  # LAYOUT-AGNOSTIC. The player block used to be pinned to A:Q and the team
  # block to S:T, which meant adding a column moved both and silently broke
  # every archived sheet. Read the whole tab instead and split it by NAME: the
  # team block is the `field`/`value` pair, the player block is everything
  # before it. Sheets written to either layout now load unchanged.
  read_tab <- function(tm) {
    x <- as.data.table(readxl::read_excel(file_path, sheet = tm,
                                          .name_repair = "unique_quiet"))
    fi <- which(names(x) == "field")
    list(x = x, fi = if (length(fi)) fi[1] else NA_integer_)
  }
  tabs <- setNames(lapply(tms, read_tab), tms)

  pl <- rbindlist(lapply(tms, function(tm) {
    z <- tabs[[tm]]; x <- z$x
    if (!is.na(z$fi)) x <- x[, seq_len(z$fi - 1L), with = FALSE]
    x <- x[, !startsWith(names(x), "..."), with = FALSE]
    x <- x[!is.na(player)]
    x[, team := tm][]
  }), fill = TRUE)

  tt <- rbindlist(lapply(tms, function(tm) {
    z <- tabs[[tm]]
    b <- if (is.na(z$fi)) data.table(field = character(), value = character())
         else z$x[, z$fi + 0:1, with = FALSE]
    setnames(b, c("field", "value"))
    b <- b[!is.na(field)]
    o <- as.list(setNames(b$value, b$field))
    data.table(team = tm,
               kicker = o$kicker %||% NA_character_,
               punt_returner = o$punt_returner %||% NA_character_,
               kick_returner = o$kick_returner %||% NA_character_,
               pys_target = as.numeric(o$pys_target %||% NA))
  }), fill = TRUE)

  # OPTIONAL `projections` / `etr` tabs, read raw. `projections` carries the
  # contest metadata (salary, DK ids, DK position, ownership); `etr` carries the
  # slate-independent ETR points projection, one row per player. Either absent,
  # the columns downstream simply stay empty.
  rd_aux <- function(nm) {
    hit <- sh[tolower(sh) == nm]
    if (!length(hit)) return(NULL)
    x <- as.data.table(readxl::read_excel(file_path, sheet = hit[1]))
    setnames(x, tolower(names(x)))
    if (!"player" %in% names(x) && "name" %in% names(x)) setnames(x, "name", "player")
    x
  }
  prj_raw <- rd_aux("projections")
  etr_raw <- rd_aux("etr")

  # ---- MULTI-SLATE SELECTION -----------------------------------------------
  multi <- ("in_classics" %in% names(g)) ||
           (!is.null(prj_raw) && "slate" %in% names(prj_raw))
  prj <- prj_raw
  if (multi) {
    if (is.null(slate)) {
      slate <- "classic_main"
      message("[cfb] multi-slate workbook, no slate given -- defaulting to classic_main")
    }
    if (identical(tolower(slate), "sd")) {
      if (is.null(game) || !nzchar(game))
        stop("slate = \"SD\" needs game = <showdown_slice label>, e.g. \"COLGT\"")
      sel <- g[!is.na(showdown_slice) &
               toupper(trimws(as.character(showdown_slice))) == toupper(trimws(game))]
      if (!nrow(sel)) stop("no game has showdown_slice == \"", game, "\" on this sheet")
      g <- sel
      prj_slate <- "SD"
    } else {
      tok <- strsplit(ifelse(is.na(g$in_classics), "", as.character(g$in_classics)),
                      "\\s*;\\s*")
      inrow <- vapply(tok, function(v) slate %in% v, logical(1))
      if (!any(inrow))
        stop("no game is in classic slate \"", slate, "\" -- check the game tab's in_classics")
      g <- g[inrow]
      prj_slate <- slate
    }
    # kickoff order re-ranks WITHIN the chosen subset (the column on the sheet
    # is the full-card rank); FLEX/SFLEX late-swap eligibility keys off it.
    if ("start_order" %in% names(g))
      g[, start_order := frank(as.numeric(start_order), ties.method = "first")]
    sel_teams <- unique(c(as.character(g$away), as.character(g$home)))
    pl <- pl[team %in% sel_teams]
    tt <- tt[team %in% sel_teams]
    if (!is.null(prj) && "slate" %in% names(prj))
      prj <- prj[as.character(slate) == prj_slate & player %in% pl$player]
  }

  # SALARY, DK IDS AND THE DK POSITION LIVE ON THE PROJECTIONS TAB. They are
  # contest metadata, not modelling inputs -- they arrive with the ETR file and
  # change when DK reprices (and differ across the slates on one card), while a
  # team tab is a football opinion. A sheet that still carries them on the team
  # tab keeps working: only columns MISSING from the player block get filled in.
  if (!is.null(prj) && nrow(prj)) {
    meta_cols <- intersect(c("dk_pos", "salary_util", "salary_cpt",
                             "dk_id_util", "dk_id_cpt"), names(prj))
    meta_cols <- setdiff(meta_cols, names(pl))
    if (length(meta_cols)) {
      mp <- unique(as.data.table(prj)[, c("player", meta_cols), with = FALSE],
                   by = "player")
      pl <- merge(pl, mp, by = "player", all.x = TRUE, sort = FALSE)
    }
  }

  # The object handed downstream as `projections` is always the per-player
  # scoreboard table the app contract expects: player + etr + own. On a multi-
  # slate sheet that is assembled here -- ETR points from the `etr` tab, and
  # ownership from the chosen slate's projections rows (populated for
  # classic_main only; NA elsewhere, which the sim reads as zero).
  if (multi) {
    po <- data.table(player = unique(pl$player))
    if (!is.null(etr_raw) && "etr_pts" %in% names(etr_raw))
      po <- merge(po, unique(etr_raw[, .(player, etr = suppressWarnings(as.numeric(etr_pts)))],
                             by = "player"), by = "player", all.x = TRUE)
    else po[, etr := NA_real_]
    if (!is.null(prj) && "own" %in% names(prj))
      po <- merge(po, unique(prj[, .(player, own = suppressWarnings(as.numeric(own)))],
                             by = "player"), by = "player", all.x = TRUE)
    else po[, own := NA_real_]
    prj <- po
  }

  if (!"dk_pos" %in% names(pl)) pl[, dk_pos := NA_character_]
  if (!"route_base" %in% names(pl)) pl[, route_base := NA_character_]
  # each is a football opinion about the other's blank
  pl[is.na(route_base) | route_base == "", route_base := dk_pos]
  pl[is.na(dk_pos) | dk_pos == "", dk_pos := route_base]

  list(game = g, team = tt, players = pl, projections = prj)
}

# One row per contest on a multi-slate workbook, for a slate-picker UI. A
# legacy single-slate sheet (no `in_classics` on the game tab) has nothing to
# pick -- returns NULL, and the caller shows no picker.
#   key       stable id for the UI (a select input's value)
#   label     what the picker shows
#   sport     CFB_CLASSIC (classic contests) or CFB (showdowns)
#   slate_arg / game_arg -- pass straight through to read_cfb_input()
cfb_slate_menu <- function(file_path) {
  sh <- readxl::excel_sheets(file_path)
  gtab <- sh[tolower(sh) == "game"]
  if (!length(gtab)) return(NULL)
  g <- as.data.table(readxl::read_excel(file_path, sheet = gtab[1]))
  if (!"in_classics" %in% names(g)) return(NULL)

  tok <- strsplit(ifelse(is.na(g$in_classics), "", as.character(g$in_classics)), "\\s*;\\s*")
  classics <- setdiff(unique(unlist(tok)), "")
  # classic_main first, then the rest in the order they first appear.
  classics <- c(intersect("classic_main", classics), setdiff(classics, "classic_main"))

  classic_rows <- rbindlist(lapply(classics, function(cl) {
    inrow <- vapply(tok, function(v) cl %in% v, logical(1))
    gg <- g[inrow]
    n  <- nrow(gg)
    label <- if (identical(cl, "classic_main")) {
      sprintf("Main - %d game", n)
    } else {
      matchups <- paste(sprintf("%s@%s", gg$away, gg$home), collapse = " + ")
      sprintf("%d-game - %s", n, matchups)
    }
    # NOTE: `key` collides with data.table()'s own reserved `key=` argument --
    # passing it by that name sets the resulting table's key column instead
    # of creating a column called "key". Build under a safe name and rename.
    data.table(menu_key = cl, label = label, sport = "CFB_CLASSIC",
               slate_arg = cl, game_arg = NA_character_)
  }))

  sd_rows <- if ("showdown_slice" %in% names(g)) {
    slices <- unique(g[!is.na(showdown_slice) & showdown_slice != "", showdown_slice])
    rbindlist(lapply(slices, function(sl) {
      gg <- g[showdown_slice == sl][1]
      data.table(menu_key = paste0("SD_", sl),
                 label = sprintf("Showdown - %s @ %s", gg$away, gg$home),
                 sport = "CFB", slate_arg = "SD", game_arg = sl)
    }))
  } else NULL

  out <- rbind(classic_rows, sd_rows, fill = TRUE)
  setnames(out, "menu_key", "key")
  out
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

# =============================================================================
# THE SIMULATION
# The Player/Team/Pos/DKID/DKCID/DKSalary/DKCSalary/DKProj/DKOwn/CPTOwn table
# the app contract expects, built from a player pool (`PL`, already carrying
# salary_util/salary_cpt/dk_id_util/dk_id_cpt from read_cfb_input) plus that
# contest's `projections` rows, restricted to `valid_players` (the players a
# simulation actually has draws for). Split out of run_cfb_simulation so
# cfb_reslice_for_lineups() can rebuild pricing for a different contest
# WITHOUT re-simulating -- the metadata build is the only slate-specific part,
# everything upstream of it (dealing the game) does not change per contest.
cfb_build_meta <- function(PL, prj, valid_players) {
  meta <- unique(PL[, .(Player = player, Team = team, Pos = dk_pos,
                        DKID = as.integer(dk_id_util), DKCID = as.integer(dk_id_cpt),
                        DKSalary = as.integer(salary_util),
                        DKCSalary = as.integer(salary_cpt))])
  meta <- meta[Player %in% valid_players]
  # CAPTAIN AND FLEX OWNERSHIP ARE DIFFERENT NUMBERS on a showdown slate, often
  # by a factor of two or more, so they are carried separately. DKOwn is the
  # FLEX figure and CPTOwn the captain one; the app pairs each against the
  # matching exposure to produce CptLev and UtlLev. Collapsing them would
  # misprice every leverage read on the board.
  meta[, `:=`(DKProj = NA_real_, DKOwn = 0, CPTOwn = 0)]
  if (!is.null(prj) && nrow(prj)) {
    prj <- as.data.table(prj)
    if ("etr" %in% names(prj))
      meta[prj, DKProj := as.numeric(i.etr), on = .(Player = player)]
    # flex ownership: `flex_own` when supplied, else the generic `own`
    if ("flex_own" %in% names(prj))
      meta[prj, DKOwn := as.numeric(i.flex_own), on = .(Player = player)]
    else if ("own" %in% names(prj))
      meta[prj, DKOwn := as.numeric(i.own), on = .(Player = player)]
    if ("cpt_own" %in% names(prj))
      meta[prj, CPTOwn := as.numeric(i.cpt_own), on = .(Player = player)]
    meta[is.na(DKOwn),  DKOwn  := 0]
    meta[is.na(CPTOwn), CPTOwn := 0]
  }
  meta
}

# Re-slice an already-simulated multi-slate CARD down to one contest, WITHOUT
# re-running the dealer. `sim_results`/`sim_metadata` are the classic_main
# outputs -- every game on the card, drawn once (see run_cfb_classic_simulation).
# `menu_row` is one row from cfb_slate_menu(). The workbook re-read here is
# cheap (no simulation): it exists only to get that contest's own player set
# and pricing off the `projections` tab. StartOrder/GameKey/Pos need no
# adjustment -- the classic optimiser sorts them WITHIN each sim (relative
# order), so filtering to a subset of games leaves them correct as-is.
cfb_reslice_for_lineups <- function(sim_results, sim_metadata, file_path, menu_row) {
  sl <- read_cfb_input(file_path, slate = menu_row$slate_arg,
                       game = if (is.na(menu_row$game_arg)) NULL else menu_row$game_arg)
  teams <- unique(sl$players$team)
  sr <- as.data.table(sim_results)[Team %in% teams]
  md <- as.data.table(sim_metadata)[Team %in% teams]

  fresh    <- cfb_build_meta(sl$players, sl$projections, unique(sr$Player))
  pcols    <- intersect(c("Pos","DKID","DKCID","DKSalary","DKCSalary","DKProj","DKOwn","CPTOwn"),
                        names(md))
  md[, (pcols) := NULL]
  md <- merge(md, fresh, by = c("Player","Team"), all.x = TRUE)

  list(sim_results = sr, metadata = md)
}

# =============================================================================
# keep_components: return the per-sim component draws (`A`) alongside the DK
# scores. OFF by default and the app never asks for it -- at 20k sims A is a
# ~500k-row table for a showdown and several million for a classic slate, and
# the app would hold it in `rv` for the whole session for no benefit.
#
# The daily review DOES need it: a mean DK score cannot tell you whether the
# team rushing line was right, and "was the team wrong or were the shares
# wrong" is not answerable from fantasy points alone. Everything downstream of
# the scoring step already collapses A to means and quantiles, so without this
# the component draws are computed and then thrown away.
# seed: pin the draw so two runs of the same sheet are comparable. The engine
# seeded from Sys.time(), which makes a before/after check impossible -- a
# change of a few tenths cannot be told apart from a different afternoon. The
# app passes nothing and still gets a fresh game every run.
run_cfb_simulation <- function(input_data, n_sims = 10000,
                               config = NULL, progress_callback = NULL,
                               keep_components = FALSE, seed = NULL) {
  say <- function(msg, frac = NULL) {
    if (is.function(progress_callback)) try(progress_callback(msg, frac), silent = TRUE)
    message("[cfb] ", msg)
  }
  if (is.null(n_sims) || is.na(n_sims)) n_sims <- 10000

  G  <- as.data.table(input_data$game)
  TT <- as.data.table(input_data$team)
  PL <- as.data.table(input_data$players)
  # A sheet written before the situation shares existed carries neither column;
  # a zero in either means "no opinion", which falls back to carry_usage. Both
  # paths therefore reproduce the old NORMAL deal exactly. What they do NOT
  # reproduce is the old tilts -- sy_tilt and gl_tilt are deleted, so an old
  # sheet's goal-line opinion is dropped rather than silently reinterpreted.
  # That is deliberate: the tilts were worth about 1.1 DK points across their
  # whole range and were never centred.
  for (cl in c("gl_share", "sy_share"))
    if (!cl %in% names(PL)) PL[[cl]] <- 0
  for (cl in c("pass_share","usage","ypc","carry_usage","gl_share","sy_share",
               "salary_util","salary_cpt","dk_id_util","dk_id_cpt"))
    if (cl %in% names(PL)) PL[[cl]] <- cfb_num(PL[[cl]])
  PL[is.na(route_base) | route_base == "", route_base := dk_pos]
  PL[sy_share == 0, sy_share := carry_usage]
  PL[gl_share == 0, gl_share := carry_usage]

  # ---- the catch schema: five band columns, or the old usage + ypc ----------
  # A SHEET WITH BAND COLUMNS SAYS WHO CATCHES WHAT SIZE OF BALL, DIRECTLY.
  # Each column is expected catches in that band; the engine normalises a
  # column to shares and deals on it. There is no likelihood, no tilt and no
  # league mix, because the operator has already answered the question those
  # were computing an answer to.
  #
  # Why the columns and not usage + ypc. A single mean is 44% of shape at best,
  # and typing a receiver's true measured yards-per-catch scored WORSE than
  # leaving the column blank -- one exponential tilt levers exp(t * 44.17) in
  # the top band against exp(t * 10.91) in the middle, four times the leverage,
  # applied at full confidence to a number that is about half luck. Measured
  # out of sample on 45,893 held-out catches, nats per catch: usage + ypc as
  # typed -0.0008, usage + a shrunk ypc .0158, the full band table .1355.
  #
  # usage survives as a DERIVED quantity -- his row total over the team's --
  # because everything downstream (the receiver set, fumble weights, the app
  # contract) keys off it.
  bands_typed <- all(CFB_BAND_COLS %in% names(PL))
  if (bands_typed) {
    for (cl in CFB_BAND_COLS) PL[[cl]] <- cfb_num(PL[[cl]])
    # `usage` is DERIVED, and it is not the row total -- these are shares of
    # five different pots, and the pots are different sizes. His share of the
    # team's catches is his band shares weighted by how often each band occurs,
    # which is the one job CFB_LEAGUE_MIX still does. It cancels inside the
    # deal (any common per-band divisor does), so this is its only remaining
    # use in the engine.
    # plain assignment, not `:=`, to match how the columns above are coerced
    M_ <- as.matrix(PL[, ..CFB_BAND_COLS])
    PL[["usage"]] <- as.vector(M_ %*% CFB_LEAGUE_MIX)
  } else if (!"usage" %in% names(PL)) {
    stop("CFB sheet has neither `usage` nor the five band columns (",
         paste(CFB_BAND_COLS, collapse = ", "), ")")
  }

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

  set.seed(if (is.null(seed) || is.na(seed))
             as.integer(Sys.time()) %% .Machine$integer.max
           else as.integer(seed))
  idx <- sample.int(nrow(P), n_sims, TRUE, prob = cal$w)
  draw <- P[idx]

  # ---- per-team setup --------------------------------------------------------
  setup <- lapply(c(fav, dog), function(tm) {
    R <- PL[team == tm & usage > 0]
    S <- PL[team == tm & carry_usage > 0]
    Q <- PL[team == tm & pass_share > 0]
    tr <- TT[team == tm]
    list(tm = tm, side = if (tm == fav) "f" else "d",
         rec = R, rsh = S, qbs = Q,
         qb = if (nrow(Q)) Q$player[1] else NA_character_,
         # pb[i, b] = P(player i caught it | the catch was in band b). Under the
         # band schema this IS the sheet, one column normalised. Under the old
         # schema it is the Bayes step the engine has always done: the prior
         # `usage` updated by how distinctive that size of catch is for him.
         pb = if (!nrow(R)) matrix(0, 0, 5L) else if (bands_typed) {
                M <- as.matrix(R[, ..CFB_BAND_COLS])
                cs <- colSums(M)
                # a band nobody is typed into falls back to overall usage,
                # otherwise the drawn game's catches there have nowhere to go
                for (b in seq_len(5L))
                  if (cs[b] <= 0) M[, b] <- R$usage
                sweep(M, 2, colSums(M), "/")
              } else {
                lr <- do.call(rbind, lapply(seq_len(nrow(R)),
                        function(k) cfb_lr(R$route_base[k], R$ypc[k])))
                apply(lr, 2, function(cl) { p <- R$usage * cl; p / sum(p) })
              },
         k = tr$kicker, pr = tr$punt_returner, kr = tr$kick_returner,
         # kicker/punt_returner/kick_returner are blank on plenty of real team
         # tabs (no punt/kick return man named). An NA_character_ column value
         # slipping into `who` becomes an NA "player" that rides all the way to
         # sim_results$Player and fails validate_simulation_output downstream.
         who = { w <- unique(c(R$player, S$player, Q$player,
                              tr$kicker, tr$punt_returner, tr$kick_returner))
                 w[!is.na(w) & w != ""] })
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
    QB <- cf$qbs; nQ <- nrow(QB)
    pos <- setNames(R$route_base, R$player)
    if (nS) pos[S$player] <- fifelse(S$dk_pos == "QB", "QB", "RB")
    # EVERY man with a pass_share is a quarterback for fumble purposes, not
    # just the first one -- his fumbles come from sacks and snaps.
    if (nQ) pos[QB$player] <- "QB"
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
      PRB <- lapply(seq_len(5L), function(b) cf$pb[, b])
      C2[, w := NA_integer_]
      for (b in seq_len(5L)) {
        ii <- which(C2$b == b)
        if (length(ii)) set(C2, ii, "w", sample.int(nR, length(ii), TRUE, prob = PRB[[b]]))
      }
      # rec31 is carried alongside for the READBACK, not for scoring. A band
      # cell is edited blind unless the operator can see what it bought, and
      # the top band is the one an edit moves hardest -- exp(t * 44.17) against
      # exp(t * 10.91) in the middle. Costs one sum over a table already built.
      rec <- C2[, .(rec = .N, ryds = sum(yds),
                    rtd = sum(td == 1L, na.rm = TRUE),
                    rec31 = sum(b == 5L)), by = .(sim, w)]
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
      SPB <- lapply(list(S$carry_usage, S$sy_share, S$gl_share),
                    function(p) p / sum(p))
      R2[, w := NA_integer_]

      # Short yardage and goal line: one vector each, dealt in one call.
      for (q in 2:3) {
        ii <- which(R2$sit == q)
        if (length(ii)) set(R2, ii, "w", sample.int(nS, length(ii), TRUE, prob = SPB[[q]]))
      }

      # NORMAL CARRIES GET A PER-GAME DIRICHLET (see CFB_CARRY_A0). Drawing
      # Dirichlet(a0 * carry_usage) once per simulated game and dealing from
      # THAT is mean-preserving, so the asked share still arrives exactly on
      # average while the week-to-week split moves the way real backfields do.
      #
      # Vectorised by inverse CDF rather than a per-sim loop: one cumulative
      # weight matrix (n_sims x nS), one uniform per carry, and nS-1 passes of
      # integer arithmetic over the whole event vector. A loop over 20,000 sims
      # would dominate the runtime of the entire engine.
      ii <- which(R2$sit == 1L)
      if (length(ii)) {
        if (nS == 1L) set(R2, ii, "w", 1L) else {
          gsh <- matrix(rgamma(n_sims * nS,
                               shape = rep(CFB_CARRY_A0 * SPB[[1]], each = n_sims)),
                        nrow = n_sims, ncol = nS)
          gsh <- gsh / rowSums(gsh)
          for (j in 2:nS) gsh[, j] <- gsh[, j - 1L] + gsh[, j]   # cumulative
          si_ <- R2$sim[ii]; u_ <- runif(length(ii))
          wv <- rep(1L, length(ii))
          for (j in seq_len(nS - 1L))
            wv <- wv + (gsh[cbind(si_, j)] < u_)
          set(R2, ii, "w", wv)
        }
      }
      rsh <- R2[, .(car = .N, cyds = sum(yds),
                    ctd = sum(td == 1L, na.rm = TRUE)), by = .(sim, w)]
      rsh[, player := S$player[w]][, w := NULL]
    }

    # SACKS ARE DEALT PER EVENT off pass_share. They used to be charged whole
    # to Q$player[1], which made pass_share a filter rather than a probability:
    # on a two-quarterback team the man listed first ate every sack in the
    # game, including the ones the other quarterback took. Identical on a
    # one-QB team. The same trick would split passing yards and touchdowns --
    # available, deliberately not done here.
    sk <- NULL
    K2 <- E2[kind == CFB_EVT_SACK]
    if (nrow(K2) && nQ) {
      K2[, qw := if (nQ == 1L) 1L
                 else sample.int(nQ, .N, TRUE, prob = QB$pass_share / sum(QB$pass_share))]
      sk <- K2[, .(sk = sum(yds)), by = .(sim, qw)]
      sk[, player := QB$player[qw]][, qw := NULL]
    }
    fg <- E2[kind == CFB_EVT_FG & !is.na(made) & made == 1L,
             .(fg = sum(cfb_fg_points(ytg))), by = sim]

    # ---- assemble the full (sim x player) grid -------------------------------
    # Every player in every sim, because a zero counts: bust rate and the whole
    # left tail of the distribution live in the games a man did nothing.
    D <- CJ(sim = seq_len(n_sims), player = who, sorted = FALSE)
    if (!is.null(rec)) D <- merge(D, rec, by = c("sim","player"), all.x = TRUE)
    if (!is.null(rsh)) D <- merge(D, rsh, by = c("sim","player"), all.x = TRUE)
    for (cl in c("rec","ryds","rtd","rec31","car","cyds","ctd"))
      if (!cl %in% names(D)) D[, (cl) := 0] else D[is.na(get(cl)), (cl) := 0]

    D[, `:=`(pyds = 0, ptd = 0, pint = 0, fgp = 0, xp = 0, rettd = 0L)]
    # Passing yards, touchdowns and interceptions still ride on the first
    # quarterback -- splitting those is scoped but not built. Sack YARDAGE now
    # lands on whoever took the sack.
    if (!is.na(cf$qb))
      D[player == cf$qb, `:=`(pyds = v_pyds[sim], ptd = v_ptd[sim],
                              pint = v_pint[sim])]
    if (!is.null(sk)) D[sk, on = .(sim, player), cyds := cyds + i.sk]
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
  meta <- cfb_build_meta(PL, input_data$projections, unique(A$player))

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

  # ---- WHERE THE PROJECTION COMES FROM -------------------------------------
  # A mean DK score is an answer without a derivation. These three tables are
  # the derivation, and they are what a sheet is actually checked against:
  # a receiver whose points are 80% touchdowns is a different object from one
  # whose points are 80% yardage, even at the same total.

  # 1. The score, decomposed into the DK line items that produced it.
  sc <- CFB_SCORE
  components <- A[, .(
      Receptions = round(mean(rec) * sc$rec, 2),
      RecYards   = round(mean(ryds) * sc$rec_yd, 2),
      RecTDs     = round(mean(rtd) * sc$rec_td, 2),
      RushYards  = round(mean(cyds) * sc$rush_yd, 2),
      RushTDs    = round(mean(ctd) * sc$rush_td, 2),
      PassYards  = round(mean(pyds) * sc$pass_yd, 2),
      PassTDs    = round(mean(ptd) * sc$pass_td, 2),
      Kicking    = round(mean(fgp + xp * sc$xp), 2),
      ReturnTDs  = round(mean(rettd) * sc$return_td, 2),
      Bonuses    = round(mean(fifelse(ryds >= 100, sc$rec_100, 0) +
                              fifelse(cyds >= 100, sc$rush_100, 0) +
                              fifelse(pyds >= 300, sc$pass_300, 0)), 2),
      Turnovers  = round(mean(pint * sc$interception + fum * sc$fumble_lost), 2),
      Total      = round(mean(dk), 2)),
    by = .(Player = player, Team = team)][order(-Total)]

  # 2. RATES, not averages. "0.37 touchdowns a game" is not a thing that can
  # happen; "scores in 31% of games, twice in 5%" is. Multi-TD rate is the one
  # the tournament price is really made of.
  rates <- A[, .(
      AnyTD     = round(100 * mean((rtd + ctd + rettd) >= 1), 1),
      MultiTD   = round(100 * mean((rtd + ctd + rettd) >= 2), 1),
      RecTDRate = round(100 * mean(rtd >= 1), 1),
      RushTDRate= round(100 * mean(ctd >= 1), 1),
      Rec100    = round(100 * mean(ryds >= 100), 1),
      Rush100   = round(100 * mean(cyds >= 100), 1),
      Pass300   = round(100 * mean(pyds >= 300), 1),
      AnyBonus  = round(100 * mean(ryds >= 100 | cyds >= 100 | pyds >= 300), 1),
      Blank     = round(100 * mean(rec == 0 & car == 0 & pyds == 0 & fgp == 0), 1)),
    by = .(Player = player, Team = team)]
  rates <- merge(rates, stat_line[, .(Player, Pos, DK)], by = "Player", all.x = TRUE)
  setcolorder(rates, c("Player","Team","Pos","DK"))
  setorder(rates, -DK)

  # 3. TEAM OUTCOME SPREAD. The player numbers all sit inside these, so if the
  # team passing range is wrong nothing downstream can be right.
  #
  # POINTS COME FROM THE DRAWN GAME, not from adding up the touchdowns we dealt.
  # A real final score includes defensive and special-teams scores this engine
  # never allocates, so reconstructing it from dealt TDs both understates it and
  # breaks the one number the pool was calibrated against.
  #
  # RECEIVING YARDS ARE NOT A SEPARATE METRIC. They reconcile to passing yards
  # by construction -- that is the whole point of dealing rather than sharing --
  # so listing both invites reading one as a check on the other. RECEPTIONS are
  # a different quantity and do belong: full PPR means the catch count is a
  # scoring line in its own right, and it is what the usage vector is dealing.
  ptsv <- data.table(SimID = seq_len(n_sims), f = draw$ptsF, d = draw$ptsD)
  tg <- A[, .(PassYds = sum(pyds), RushYds = sum(cyds), Rec = sum(rec),
              PassTD = sum(ptd), RushTD = sum(ctd), RecTD = sum(rtd),
              TotalTD = sum(rtd) + sum(ctd)),
          by = .(SimID, team)]
  tg[ptsv, Points := fifelse(team == fav, i.f, i.d), on = "SimID"]
  tg[, ScrimYds := PassYds + RushYds]

  TEAM_METRICS <- c("Points", "ScrimYds", "PassYds", "RushYds", "Rec", "TotalTD")
  tkeep <- min(2000L, n_sims)
  team_dist <- melt(tg[SimID %in% sample(unique(tg$SimID), tkeep)],
                    id.vars = c("SimID", "team"), measure.vars = TEAM_METRICS,
                    variable.name = "Metric", value.name = "Value")
  team_spread <- melt(tg, id.vars = c("SimID", "team"),
                      measure.vars = TEAM_METRICS,
                      variable.name = "Metric", value.name = "V")[
    , .(Mean = round(mean(V), 1), P10 = round(quantile(V, .1), 1),
        P25 = round(quantile(V, .25), 1), Median = round(median(V), 1),
        P75 = round(quantile(V, .75), 1), P90 = round(quantile(V, .9), 1)),
    by = .(Team = team, Metric)]

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
  # RUSHING YPC. Receiving has had this check since the engine was written and
  # rushing has not, which matters more now: the situation-shares design drops
  # the within-team efficiency dial on the evidence that carry_usage alone
  # delivers 96% of the true RB1-RB2 spread. The other 4% would show up here.
  # QBs are excluded because cyds carries their sack yardage by box convention,
  # which would drag the low tail for a reason that has nothing to do with
  # running the ball.
  rbn  <- PL[dk_pos == "RB", player]
  rypc <- A[player %in% rbn & car >= 5, .(y = sum(cyds) / sum(car)),
            by = .(SimID, team, player)]
  rsum <- if (nrow(rypc))
    rypc[, .(RYPCp10 = round(quantile(y, .1), 2),
             RYPCp90 = round(quantile(y, .9), 2)), by = .(team)]
  else data.table(team = c(fav, dog), RYPCp10 = NA_real_, RYPCp90 = NA_real_)
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
    vrow("Rush YPC, 10th pct",       g(rsum,"RYPCp10"),  "2.38",
         "RB games with 5+ carries, measured 2019-25 (the two rows above are RECEIVING)"),
    vrow("Rush YPC, 90th pct",       g(rsum,"RYPCp90"),  "8.43",
         "wide here means the dealer is making backs it was never asked for"),
    vrow("One man takes all 3 rec TD (%)", g(tsum,"All3TD"), "4.2",
         "KNOWN GAP: independent dealing alone gives ~3.2 and the affinity effect that lifts real football above it is not built"),
    vrow("3-TD games observed",      g(tsum,"N"),        "-",
         "sample for the row above -- under ~300 that check is noise")))
  setnames(validation, c("A","B"), c(fav, dog))

  sport_visuals <- list(
    score_dist = score_dist, stat_line = stat_line,
    dist_sample = dist_sample, components = components, rates = rates,
    team_dist = team_dist, team_spread = team_spread,
    validation = validation, team_line = team_line,
    pool_size = nrow(P), n_sims = n_sims,
    ess = round(cal$ess),
    market = sprintf("%s -%.1f, total %.1f", fav, G$spread[1], G$total[1]),
    pool_total = round(cal$total, 1), pool_margin = round(cal$margin, 1),
    asked_total = round(cal$target$total, 2))

  say("done", 1)
  out <- list(sim_results = sim_results, metadata = meta, projections = projections,
              sport_visuals = sport_visuals)
  if (isTRUE(keep_components)) {
    ccols <- intersect(c("SimID","player","team","rec","ryds","rtd","rec31","car","cyds",
                         "ctd","pyds","ptd","pint","fgp","xp","rettd","fum","dk"),
                       names(A))
    out$sim_components <- A[, ..ccols]
  }
  out
}

# =============================================================================
# CLASSIC (multi-game full slate)
# -----------------------------------------------------------------------------
# The engine above is two teams by construction. A classic slate is just N of
# those: draw each game from the pool on its own, deal it, then stack the
# results in a SHARED SimID space so SimID k is one Monte-Carlo world across
# every game -- correct, because the games are independent. Nothing about the
# dealing changes here; this only loops it and glues the outputs.
#
# Two columns are added that the classic optimiser needs and showdown does not:
#   GameKey     "AWAY HOME"
#   StartOrder  the game tab's kickoff rank (1 = earliest). FLEX / SFLEX must
#               hold the latest-starting players, so the optimiser reads this.
# =============================================================================
run_cfb_classic_simulation <- function(input_data, n_sims = 10000,
                                       config = NULL, progress_callback = NULL,
                                       keep_components = FALSE, seed = NULL) {
  if (is.null(n_sims) || is.na(n_sims)) n_sims <- 10000
  G   <- as.data.table(input_data$game)
  TT  <- as.data.table(input_data$team)
  PL  <- as.data.table(input_data$players)
  PRJ <- input_data$projections

  if (!"start_order" %in% names(G)) G[, start_order := seq_len(.N)]
  setorder(G, start_order)
  ng <- nrow(G)

  say <- function(msg, frac = NULL) {
    if (is.function(progress_callback)) try(progress_callback(msg, frac), silent = TRUE)
    message("[cfb-classic] ", msg)
  }

  sr <- vector("list", ng); md <- vector("list", ng)
  pj <- vector("list", ng); vis <- vector("list", ng)
  cp <- vector("list", ng)

  for (i in seq_len(ng)) {
    gi   <- G[i]
    tms  <- c(gi$away, gi$home)
    gkey <- paste(gi$away, gi$home)
    say(sprintf("game %d/%d  %s", i, ng, gkey), (i - 1) / ng)

    sub <- list(game        = gi,
                team        = TT[team %in% tms],
                players     = PL[team %in% tms],
                projections = PRJ)
    gp <- if (is.function(progress_callback))
            function(m, f) try(progress_callback(
                sprintf("game %d/%d: %s", i, ng, m),
                (i - 1 + (f %||% 0)) / ng), silent = TRUE)
          else NULL
    # Each game gets its own offset seed: one seed for the whole slate would
    # make every game reproducible but ALSO make them share a draw stream, so
    # re-ordering the slate would change every game's result.
    res <- run_cfb_simulation(sub, n_sims = n_sims, config = config,
                              progress_callback = gp,
                              keep_components = keep_components,
                              seed = if (is.null(seed)) NULL else as.integer(seed) + i)

    so <- as.integer(gi$start_order)
    r <- as.data.table(res$sim_results); r[, `:=`(GameKey = gkey, StartOrder = so)]
    m <- as.data.table(res$metadata);    m[, `:=`(GameKey = gkey, StartOrder = so)]
    sr[[i]]  <- r
    md[[i]]  <- m
    pj[[i]]  <- as.data.table(res$projections)
    vis[[i]] <- res$sport_visuals
    if (isTRUE(keep_components) && !is.null(res$sim_components)) {
      cc <- res$sim_components; cc[, GameKey := gkey]
      cp[[i]] <- cc
    }
  }

  say("combining", 0.95)
  sim_results <- rbindlist(sr, fill = TRUE)
  metadata    <- rbindlist(md, fill = TRUE)
  projections <- rbindlist(pj, fill = TRUE)

  # The two-team engine seeds its player list with the kicker and both
  # returners; a classic slate rosters none of those, so those names come
  # through as NA. Drop them here rather than teach the core engine about
  # classic.
  sim_results <- sim_results[!is.na(Player) & Player != ""]
  metadata    <- metadata[!is.na(Player) & Player != ""]
  if (nrow(projections)) projections <- projections[!is.na(Player) & Player != ""]

  # sport_visuals: the per-player / per-team tables just stack (tagged with the
  # game they came from); the per-game scalars cannot, so they collapse to
  # slate-level summaries.
  vk <- c("score_dist", "stat_line", "dist_sample", "components", "rates",
          "team_dist", "team_spread", "team_line", "validation")
  sv <- list()
  for (k in vk)
    sv[[k]] <- rbindlist(lapply(seq_along(vis), function(j) {
      d <- vis[[j]][[k]]
      if (is.null(d) || !nrow(d)) return(NULL)
      d <- as.data.table(copy(d)); d[, Game := vis[[j]]$market][]
    }), fill = TRUE)

  ess_all <- vapply(vis, function(v) as.numeric(v$ess %||% NA_real_), 0)
  sv$pool_size   <- vis[[1]]$pool_size
  sv$n_sims      <- n_sims
  sv$ess         <- suppressWarnings(min(ess_all, na.rm = TRUE))
  sv$market      <- sprintf("%d-game classic slate  |  worst-matched game ESS %s",
                            ng, format(round(sv$ess), big.mark = ","))
  sv$pool_total  <- NA_real_
  sv$pool_margin <- NA_real_
  sv$asked_total <- NA_real_

  say("done", 1)
  out <- list(sim_results = sim_results, metadata = metadata,
              projections = projections, sport_visuals = sv)
  if (isTRUE(keep_components))
    out$sim_components <- rbindlist(cp[!vapply(cp, is.null, logical(1))], fill = TRUE)
  out
}
