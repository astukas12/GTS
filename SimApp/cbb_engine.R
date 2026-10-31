# ============================================================================
# CBB SIMULATION ENGINE  --  v1 (the official CBB engine from 29 Oct 2026)
# Golden Ticket Sims -- College Basketball DFS
# ============================================================================
# Platforms:  DraftKings Classic | FanDuel | DK Showdown (captain = DK score)
#
# DK scoring:  PTS(1) + 3PM(0.5) + REB(1.25) + AST(1.5) + STL(2) + BLK(2)
#              - TO(0.5) + DD bonus(1.5) + TD bonus(3, replaces DD)
# FD scoring:  PTS(1) + 3PM(0.5) + REB(1.20) + AST(1.5) + STL(2) + BLK(2)
#              - TO(1.0)  [no bonus]
# SD scoring:  same as DK (DKScore reused)
#
# ONE ENGINE.  A game is a REAL earlier game drawn from a pool matched on the CLOSING line, venue and PRE-GAME style; its total and margin are the line plus the
# neighbour's actual-minus-line residual, and its box (both teams) comes with it, scoring counts rescaled to the drawn score. Players take that box by
# MINUTES x RATE:
#   minutes  = the sheet's forecast (pred_min, a team adds to 200) + rotation-slot noise, a left-tail mixture (unforced sit, early exit), a light foul module
#              (player foul rate x the league's pull behaviour at 2 / 3 / 4 / 5 fouls) and a blowout shift tied to the DRAWN margin; rescaled to 200
#   rate     = one of the player's own last-20 same-team games (per minute) x exp(sigma z - sigma^2/2), a hot / cold factor shared across the player's stats
#   deal     = minutes x rate normalised to the drawn team count per stat; total-preserving rounding; a player's threes capped at his field goals (excess to
#              teammates, team threes kept); pts DERIVED = tpm + 2 fgm + ftm; assists re-allocated scorer != assister (3PM assisted 86.2%, 2PM 38.5%)
# A team whose tab has no usable minutes forecast uses the percentile-SHARES method (the previous engine) for that team only: it is the internal fallback, and
# the way an old-format sheet is read. An input that supports neither (percentile columns missing) STOPS the build -- it never simulates zeros.
#
# Workbooks: v1 (tabs Params, Games, IDs, Sim_*, one tab per team, Profiles, FoulBank, SDn_IDs) or the old format (Slate/IDs, Games, Sim_*, team percentile tabs).
# Every stat draw (fgm, tpm, ftm, reb, ast, stl, blk, to, pts, twom, min) is returned in sim_results, so the review record keeps it (record_schema 2).
# Validated against the previous engine on 450 past games (CBB/dev/README.md): player DK 80% band 78.9% vs 64.8%, game total band 78.4% vs 22.7%.
# ============================================================================

library(data.table)
library(readxl)

CBB_ENGINE_VERSION <- "cbb-engine-v1.1 (31 Oct 2026)"
CBB_ENGINE_FILE0 <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/"), error = function(e) NA_character_)
# the file this engine was loaded from (found when asked, so a wrapper or a changed working directory does not matter); its md5 goes into every sheet / record
cbb_engine_info <- function() {
  cands <- c(CBB_ENGINE_FILE0, "cbb_engine.R", file.path("SimApp", "cbb_engine.R"), Sys.getenv("GTS_CBB_ENGINE_FILE"))
  f <- NA_character_
  for (cf in cands[!is.na(cands) & nzchar(cands)]) if (file.exists(cf) && any(grepl("^CBB_ENGINE_VERSION <- ", readLines(cf, n = 40, warn = FALSE)))) { f <- normalizePath(cf, winslash = "/"); break }
  list(version = CBB_ENGINE_VERSION, file = f, md5 = if (!is.na(f)) unname(tools::md5sum(f)) else NA_character_)
}

.CBB_S8   <- c("fgm", "tpm", "ftm", "reb", "ast", "stl", "blk", "to")
.CBB_POOL <- c(fgm = "fgm", tpm = "3pm", ftm = "ftm", reb = "rebounds", ast = "assists", stl = "steals", blk = "blocks", to = "turnovers")
.CBB_PCT  <- unlist(lapply(.CBB_S8, function(s) paste0(s, "_p", c(10, 25, 50, 75, 90))))


# ============================================================================
# SCORING -- element-wise on vectors or matrices
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
# PERCENTILE SHARES (fallback path) -- vectorized percentile interpolation
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
# DEAL HELPERS
# ============================================================================

# total-preserving (systematic) rounding down the columns: every value goes to its floor or ceiling, the column sum equals the (integer) team count
cbb_sys_round <- function(real, perm) {
  n <- nrow(real); N <- ncol(real)
  rr <- real[perm, , drop = FALSE]; cs <- rr
  if (n > 1) for (i in 2:n) cs[i, ] <- cs[i - 1, ] + rr[i, ]
  fl <- floor(sweep(cs, 2, runif(N), "+"))
  y <- fl; if (n > 1) y[2:n, ] <- fl[2:n, ] - fl[1:(n - 1), ]
  out <- y; out[perm, ] <- y
  out
}

# cap each player's threes at his field goals and hand the excess to teammates with room, so the team's threes are unchanged
cbb_cap_redistribute <- function(real, cap, total, iters = 5) {
  y <- pmin(real, cap)
  for (it in seq_len(iters)) {
    deficit <- total - colSums(y); if (all(deficit < 1e-6)) break
    slack <- cap - y; ss <- colSums(slack)
    w <- sweep(slack, 2, pmax(ss, 1e-9), "/")
    y <- y + sweep(w, 2, pmin(deficit, ss), "*")
  }
  y
}


# ============================================================================
# MINUTES -- noise, left-tail mixture, light foul module
# ============================================================================

cbb_default_params <- function() list(
  sigma_rate = 0.70, rho = 0.4, c_total = 1, c_margin = 1,
  smult = c(fgm = .45, tpm = 1, ftm = .45, reb = .3, ast = .4, stl = .25, blk = .9, to = .15),     # rate-noise multiplier per stat (between-game part only: the count noise comes from the discrete deals)
  slot_sd = c(starters = 6.04, `6th-8th` = 6.64, `9th+` = 5.72),
  p_unforced = 0.0256, p_exit = 0.0026, unforced_ratio_mean = 0.49, unforced_ratio_sd = 0.15,
  foul_module = 1, mixture = 1, blowout_shift = 1, blowout_margin = 20,
  blow_factor = c(starters = 0.944, `6th-8th` = 1.12, `9th+` = 1.0), min_regular = 20, rate_cap = Inf,
  flow = 1,                  # minutes follow the drawn game: slot multipliers on |margin|, team minutes 200 + 25 x OT periods of the drawn neighbour (replaces the 20-point blowout shift)
  min_cap = 40,              # regulation rule: a player never exceeds 40 + 5 x OT periods; the excess goes to teammates
  min_up = 0.5, min_up_from = 26,   # positive minutes deviations of heavy-minute players are compressed (the forecast mean is kept)
  z_soft = 2.5,              # soft clip of the rate-noise driver: z -> z_soft * tanh(z / z_soft)
  sig_gamma = 2,             # rate noise falls with the player's level (DK per minute against the median): real variability grows far more slowly than level
  mn_stl = 1, mn_blk = 1, mn_to = 1, mn_ast = 1, mn_reb = 1, mn_fgm = 1, mn_ftm = 1,   # these stats are dealt as discrete multinomial draws of the team count
  mn_use_r0 = 1,             # ... from the player's shrunk baseline rate (per-stat reliability shrinkage is applied by the builder)
  ast_event = 1,             # assists as discrete events: assisted baskets by scorer, assister a teammate (never the scorer)
  share_cap = 1)             # no per-player share cap

# light foul module: only trouble-CAUSED pulls change minutes. M0 = planned floor minutes (length n); returns minutes after fouls
cbb_foul_events <- function(M0, lam, e_k, banks, n) {
  Pc <- M0; Tt <- apply(matrix(rexp(n * 5, rate = lam), n, 5), 1, cumsum)
  for (k in 2:5) {
    tk <- Tt[k, ]
    if (k < 5) {
      trig <- tk < Pc & (k >= 4 | tk <= 0.5 * M0); if (!any(trig)) next
      caused <- trig & runif(n) < e_k[k]; if (!any(caused)) next
      bank <- banks[[as.character(k)]]; r <- bank[sample.int(length(bank), sum(caused), replace = TRUE)]
      Pc[caused] <- tk[caused] + r * (Pc[caused] - tk[caused])
    } else {
      trig <- tk < Pc; if (!any(trig)) next
      bank <- banks[["5"]]; r <- bank[sample.int(length(bank), sum(trig), replace = TRUE)]
      Pc[trig] <- tk[trig] + r * (Pc[trig] - tk[trig])
    }
  }
  Pc
}

cbb_mixture_events <- function(M0, par, regular) {
  n <- length(M0); lose_u <- regular & runif(n) < par$p_unforced; lose_e <- regular & runif(n) < par$p_exit; M <- M0
  if (any(lose_u)) {
    m <- par$unforced_ratio_mean; s <- par$unforced_ratio_sd
    a <- m * (m * (1 - m) / s^2 - 1); b <- (1 - m) * (m * (1 - m) / s^2 - 1)
    M[lose_u] <- M[lose_u] * rbeta(sum(lose_u), a, b)
  }
  if (any(lose_e)) M[lose_e] <- M[lose_e] * runif(sum(lose_e), 0.05, 0.45)
  M
}

`%|||%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a[1])) b else a
# no player above `cap` minutes (matrix, one cap per sim): the excess goes to teammates in proportion to their room; the team total is kept
cbb_min_cap_redistribute <- function(M, cap, iters = 8) {
  for (it in seq_len(iters)) { ex <- pmax(M - cap, 0); if (all(ex < 1e-6)) break; M <- pmin(M, cap); room <- cap - M; rs <- colSums(room); M <- M + sweep(room, 2, colSums(ex) / pmax(rs, 1e-9), "*") }
  pmin(M, cap + 1e-9)
}
# game flow (2025-26 box scores, regulation games): a team's minutes by slot against the game's |margin|, and what overtime adds
.CBB_FLOW_AM <- c(1.5, 5, 8.5, 12.5, 16.5, 20.5, 26.5, 36)
.CBB_FLOW_F <- list(starters = c(1.032, 1.034, 1.025, 1.014, .992, .974, .933, .881), `6th-8th` = c(.964, .960, .979, .991, 1.017, 1.025, 1.071, 1.103), `9th+` = c(.998, 1.006, .996, .987, .943, .892, .983, 1.174))
.CBB_FLOW_OT <- list(starters = c(1, 1.176, 1.323, 1.428), `6th-8th` = c(1, 1.051, 1.117, 1.308), `9th+` = c(1, 1.037, 1.064, 1.073))
cbb_flow_mult <- function(am, slot, otk) {   # n x N multipliers: the slot's game-flow factor (centred to mean 1 over this game's draws) times the overtime factor
  n <- length(slot); N <- length(am); out <- matrix(1, n, N)
  for (s in unique(slot)) { fa <- approx(.CBB_FLOW_AM, .CBB_FLOW_F[[s]], xout = am, rule = 2)$y; fa <- fa / mean(fa); fo <- .CBB_FLOW_OT[[s]][otk + 1L]; out[slot == s, ] <- matrix(fa * fo, sum(slot == s), N, byrow = TRUE) }
  out
}
# n x N shares (columns sum to 1) -> integer counts: a multinomial draw of `size` events per sim (sequential binomials)
cbb_multinom_deal <- function(shr, size) {
  n <- nrow(shr); N <- ncol(shr); out <- matrix(0L, n, N); rem <- as.integer(size); prem <- rep(1, N)
  for (i in seq_len(n - 1L)) { p <- pmin(pmax(shr[i, ] / pmax(prem, 1e-12), 0), 1); xi <- rbinom(N, rem, p); out[i, ] <- xi; rem <- rem - xi; prem <- prem - shr[i, ] }
  out[n, ] <- rem; out
}
# assists as discrete events: assisted baskets are assigned to scorers by their made-FG shares, each basket's assister is a teammate drawn from the assister weights; weights are tilted so a player's expected assists equal his drawn share
cbb_ast_event_deal <- function(shr, fgm, A_T) {
  n <- nrow(shr); N <- ncol(shr); s <- sweep(fgm, 2, pmax(colSums(fgm), 1), "/"); w <- shr
  for (it in 1:6) { den <- pmax(1 - w, 1e-6); E <- w * (matrix(colSums(s / den), n, N, byrow = TRUE) - s / den); E <- sweep(E, 2, pmax(colSums(E), 1e-9), "/"); w <- w * pmin(pmax(shr / pmax(E, 1e-9), .25), 4); w <- sweep(w, 2, pmax(colSums(w), 1e-9), "/") }
  nb <- cbb_multinom_deal(s, pmin(A_T, as.integer(colSums(fgm)))); out <- matrix(0L, n, N)
  for (i in seq_len(n)) { pw <- w; pw[i, ] <- 0; pw <- sweep(pw, 2, pmax(colSums(pw), 1e-12), "/"); out <- out + cbb_multinom_deal(pw, nb[i, ]) }
  out
}

# per player: Gaussian scale so the TOTAL minutes sd by slot is kept, and a mean shift so the simulated mean minutes equal the forecast
# (the shift also undoes the upward bias of flooring a low-minute player's noise at 0.5)
cbb_noise_calibration <- function(mp, sdm, pf40, par, fb, ndraw = 4000) {
  n <- length(mp); shift <- numeric(n); scale <- rep(1, n)
  for (i in seq_len(n)) {
    use_mix  <- par$mixture && mp[i] >= par$min_regular
    use_foul <- par$foul_module && mp[i] >= 12 && !is.null(fb)
    if (use_mix || use_foul) {
      M0 <- rep(mp[i], ndraw); M1 <- M0
      if (use_mix)  M1 <- cbb_mixture_events(M1, par, rep(TRUE, ndraw))
      if (use_foul) M1 <- cbb_foul_events(M1, pf40[i] / 40, fb$e_k, fb$banks, ndraw)
      scale[i] <- sqrt(max(1 - var(M1 - M0) / sdm[i]^2, 0.25))
    }
    z <- rnorm(ndraw); sh <- 0
    for (it in 1:6) {
      x <- pmax(mp[i] + sh + sdm[i] * scale[i] * z, 0.5)
      if (use_mix)  x <- cbb_mixture_events(x, par, rep(TRUE, ndraw))
      if (use_foul) x <- cbb_foul_events(x, pf40[i] / 40, fb$e_k, fb$banks, ndraw)
      if (par$min_up < 1 && mp[i] >= par$min_up_from) { d_ <- x - mp[i]; x <- mp[i] + ifelse(d_ > 0, d_ * par$min_up, d_) }
      if (is.finite(par$min_cap)) x <- pmin(x, par$min_cap)
      sh <- sh + 0.9 * (mp[i] - mean(x))
    }
    shift[i] <- sh
  }
  list(shift = shift, scale = scale)
}


# ============================================================================
# INPUT CHECKS -- an input the engine cannot simulate STOPS the build (never zeros)
# ============================================================================

cbb_validate <- function(v1) {
  stop_if <- function(cond, ...) if (cond) stop("CBB input check: ", sprintf(...), call. = FALSE)
  stop_if(is.null(v1$games) || !nrow(v1$games), "no games")
  stop_if(is.null(v1$players) || !nrow(v1$players), "no players")
  for (cl in c("FavTeam", "DogTeam", "Spread", "Total")) stop_if(!cl %in% names(v1$games), "Games is missing column %s", cl)
  for (i in seq_len(nrow(v1$games))) {
    g <- v1$games[i]; key <- paste0(g$FavTeam, "_vs_", g$DogTeam); pl <- v1$pool[[key]]
    stop_if(is.null(pl), "no Sim_ pool sheet for %s", key)
    need <- c("weight", "res_total", "res_margin", paste0(rep(c(g$FavTeam, g$DogTeam), each = 9), "_", rep(c(unname(.CBB_POOL), "score"), 2)))
    miss <- setdiff(need, names(pl)); stop_if(length(miss), "pool %s is missing column(s) %s", key, paste(head(miss, 6), collapse = ", "))
    stop_if(anyNA(pl$weight) || sum(pl$weight) <= 0, "pool %s has no usable weights", key)
    stop_if(nrow(pl) < 10, "pool %s has only %d neighbours", key, nrow(pl))
    for (tm in c(g$FavTeam, g$DogTeam)) {
      p <- v1$players[Team == tm]; stop_if(!nrow(p), "team %s has no players", tm)
      has_min <- "pred_min" %in% names(p) && !anyNA(p$pred_min) && sum(p$pred_min) > 100
      has_pct <- all(.CBB_PCT %in% names(p)) && !anyNA(p[[.CBB_PCT[1]]])
      stop_if(!has_min && !has_pct, "team %s has neither a minutes forecast (pred_min) nor the 40 percentile columns (missing e.g. %s)", tm, paste(head(setdiff(.CBB_PCT, names(p)), 3), collapse = ", "))
    }
  }
  invisible(TRUE)
}


# ============================================================================
# CORE SIMULATION (matrices) -- v1 input in, stat matrices out
# ============================================================================

cbb_core <- function(v1, n_sims, par, cb = function(detail, value) NULL) {
  N <- n_sims; fb <- v1$foul
  if (is.null(fb)) par$foul_module <- 0
  if (!is.null(fb) && is.null(fb$e_k)) fb$e_k <- c(NA, sapply(2:5, function(k) max((fb$HK[[as.character(k)]] - fb$base1) / (1 - fb$base1), 0)))
  players <- copy(v1$players); players[, .pid := .I]
  LVL_REF <- median(with(v1$players, if (all(paste0("r0_", c("fgm", "tpm", "ftm", "reb", "ast", "stl", "blk", "to")) %in% names(v1$players))) 2 * r0_fgm + 1.5 * r0_tpm + r0_ftm + 1.25 * r0_reb + 1.5 * r0_ast + 2 * r0_stl + 2 * r0_blk - 0.5 * r0_to else NA_real_), na.rm = TRUE)
  nP <- nrow(players); S8 <- .CBB_S8
  out <- setNames(lapply(c(S8, "pts", "twom"), function(x) matrix(0L, nP, N)), c(S8, "pts", "twom"))
  MIN <- matrix(NA_real_, nP, N); drawn <- list(); fallback_teams <- character(); ess <- numeric()
  ng <- nrow(v1$games)
  for (gi in seq_len(ng)) {
    g <- v1$games[gi]; key <- paste0(g$FavTeam, "_vs_", g$DogTeam); pool <- v1$pool[[key]]
    cb(sprintf("Game %d of %d: %s", gi, ng, key), 0.10 + 0.70 * (gi - 1) / ng)
    w <- pool$weight / sum(pool$weight); ess[key] <- 1 / sum(w^2)
    k <- sample.int(nrow(pool), N, replace = TRUE, prob = w)
    Td <- g$Total + par$c_total * pool$res_total[k]; Md <- abs(g$Spread) + par$c_margin * pool$res_margin[k]
    fav_pts <- (Td + Md) / 2; dog_pts <- (Td - Md) / 2
    drawn[[key]] <- data.table(sim = seq_len(N), total = Td, margin_fav = Md, fav_pts = fav_pts, dog_pts = dog_pts)
    blow <- abs(Md) >= par$blowout_margin
    otk <- if ("ot" %in% names(pool)) pmin(as.integer(pool$ot[k]), 3L) else rep(0L, N)
    for (side in c("fav", "dog")) {
      tm <- if (side == "fav") g$FavTeam else g$DogTeam; pts_draw <- if (side == "fav") fav_pts else dog_pts
      nb_score <- pool[[paste0(tm, "_score")]][k]
      box <- lapply(S8, function(s) {
        v <- pool[[paste0(tm, "_", .CBB_POOL[[s]])]][k]
        if (s %in% c("fgm", "tpm", "ftm", "ast")) v <- v * pts_draw / nb_score
        v
      })
      names(box) <- S8
      rows <- which(players$Team == tm); P <- players[rows]; n <- nrow(P)
      has_min <- "pred_min" %in% names(P) && !anyNA(P$pred_min) && sum(P$pred_min) > 100
      st <- list()
      if (!has_min) {
        # ---- fallback: percentile SHARES (internal path for a team without a minutes forecast / old-format sheets)
        fallback_teams <- c(fallback_teams, tm)
        for (s in S8) {
          pc <- sapply(c(10, 25, 50, 75, 90), function(q) P[[paste0(s, "_p", q)]])
          sh <- interp_shares(matrix(runif(n * N), n, N), pc[, 1], pc[, 2], pc[, 3], pc[, 4], pc[, 5])
          cs <- colSums(sh); cs[cs == 0] <- 1
          st[[s]] <- round(sweep(sh, 2, cs, "/") * rep(box[[s]], each = n))
        }
        st$tpm <- pmin(st$tpm, st$fgm); Mm <- matrix(NA_real_, n, N)
      } else {
        # ---- minutes x rate
        mp <- P$pred_min * 200 / sum(P$pred_min); rk <- rank(-mp, ties.method = "first")
        slot <- ifelse(rk <= 5, "starters", ifelse(rk <= 8, "6th-8th", "9th+")); sdm <- par$slot_sd[slot]
        pf40 <- if ("pf40" %in% names(P)) ifelse(is.na(P$pf40), 3.5, P$pf40) else rep(3.5, n)
        cal <- cbb_noise_calibration(mp, sdm, pf40, par, fb)
        M <- pmax(mp + cal$shift + sdm * cal$scale * matrix(rnorm(n * N), n, N), 0.5); regular <- mp >= par$min_regular
        for (i in seq_len(n)) {
          x <- M[i, ]
          if (par$mixture && regular[i]) x <- cbb_mixture_events(x, par, rep(TRUE, N))
          if (par$foul_module && mp[i] >= 12 && !is.null(fb)) x <- cbb_foul_events(x, pf40[i] / 40, fb$e_k, fb$banks, N)
          M[i, ] <- x
        }
        if (par$min_up < 1) for (i in which(mp >= par$min_up_from)) { d <- M[i, ] - mp[i]; M[i, ] <- mp[i] + ifelse(d > 0, d * par$min_up, d) }
        if (par$flow) M <- M * cbb_flow_mult(abs(Md), slot, otk) else if (par$blowout_shift) M[, blow] <- M[, blow, drop = FALSE] * par$blow_factor[slot]
        Tm <- if (par$flow) 200 + 25 * otk else rep(200, N); M <- sweep(M, 2, Tm / colSums(M), "*")
        if (is.finite(par$min_cap) && par$min_cap * n >= max(Tm)) M <- cbb_min_cap_redistribute(M, matrix(par$min_cap + 5 * otk, n, N, byrow = TRUE))
        Mm <- M
        z0 <- matrix(rnorm(n * N), n, N); ast_shr <- NULL
        lvl_f <- if (par$sig_gamma != 0 && is.finite(LVL_REF)) { dkr <- with(P, 2 * r0_fgm + 1.5 * r0_tpm + r0_ftm + 1.25 * r0_reb + 1.5 * r0_ast + 2 * r0_stl + 2 * r0_blk - 0.5 * r0_to); pmin(pmax(dkr / LVL_REF, .4), 2.5)^(-par$sig_gamma) } else rep(1, n)
        for (s in S8) {
          Rm <- matrix(0, n, N); sg0 <- par$sigma_rate * par$smult[[s]]; mnS <- isTRUE((par[[paste0("mn_", s)]] %|||% 0) > 0)
          for (r in seq_len(n)) {
            H <- v1$profiles[[paste0(tm, "|", P$Name[r])]]; has_prof <- !is.null(H) && nrow(H$R) >= 2; sg <- sg0 * lvl_f[r] * (if (has_prof) 1 else 2)
            base_r <- if (has_prof) { if (mnS) rep(if (par$mn_use_r0 > 0) P[[paste0("r0_", s)]][r] else sum(H$R[, s] * H$w) / sum(H$w), N) else H$R[sample.int(nrow(H$R), N, replace = TRUE, prob = H$w), s] } else rep(P[[paste0("r0_", s)]][r], N)
            zz <- sqrt(par$rho) * z0[r, ] + sqrt(1 - par$rho) * rnorm(N)
            mult <- if (par$z_soft > 0) { m_ <- exp(sg * par$z_soft * tanh(zz / par$z_soft)); m_ / mean(m_) } else exp(sg * zz - sg^2 / 2)
            Rm[r, ] <- base_r * pmin(mult, par$rate_cap)
          }
          x <- M * Rm; cs <- colSums(x); cs[cs == 0] <- 1; shr <- sweep(x, 2, cs, "/"); if (s == "ast") ast_shr <- shr
          st[[s]] <- if (mnS) cbb_multinom_deal(shr, round(box[[s]])) else shr * rep(box[[s]], each = n)      # discrete deal of the team count, or the real-valued deal
        }
        perm <- sample.int(n)
        st$fgm <- cbb_sys_round(st$fgm, perm)
        st$tpm <- cbb_sys_round(cbb_cap_redistribute(st$tpm, st$fgm, box$tpm), perm)   # a made three is a made field goal; the team's threes are kept
        for (s in c("ftm", "reb", "ast", "stl", "blk", "to")) st[[s]] <- cbb_sys_round(st[[s]], perm)
        if (par$ast_event > 0 && !is.null(ast_shr)) st$ast <- cbb_ast_event_deal(ast_shr, st$fgm, round(box$ast))
      }
      for (s in S8) out[[s]][rows, ] <- as.integer(st[[s]])
      MIN[rows, ] <- Mm
    }
  }
  # ---- assists: scorer != assister (assistable baskets by shot type go to TEAMMATES in proportion to their allocated assist weights)
  cb("Assigning assists...", 0.82)
  A3R <- 0.862; A2R <- 0.385
  for (tm in unique(players$Team)) {
    rows <- which(players$Team == tm); n <- length(rows); if (n < 2 || (par$ast_event > 0 && !(tm %in% fallback_teams))) next
    tpm_t <- out$tpm[rows, , drop = FALSE]; fgm_t <- out$fgm[rows, , drop = FALSE]
    B <- round(tpm_t * A3R + pmax(fgm_t - tpm_t, 0L) * A2R); A0 <- out$ast[rows, , drop = FALSE]
    W <- sweep(A0, 2, pmax(colSums(A0), 1L), "/"); W[, colSums(W) == 0] <- 1 / n
    if (tm %in% fallback_teams) {
      # shares-fallback assist draws already come from each player's own net assist history (scorer != assister is built in), so the exclusion below
      # would count twice: tilt the weights so the expected allocation matches the drawn shares (a few mean-level fixed-point steps)
      tw <- rowMeans(W); Bb <- rowMeans(B); f <- rep(1, n)
      for (it in 1:6) { wm <- tw * f; wm <- wm / sum(wm); E <- wm * (sum(Bb / pmax(1 - wm, 1e-6)) - Bb / pmax(1 - wm, 1e-6)); E <- E / max(sum(E), 1e-9); f <- f * pmin(pmax(tw / pmax(E, 1e-9), .25), 4) }
      W <- W * f; W <- sweep(W, 2, pmax(colSums(W), 1e-9), "/")
    }
    den <- pmax(1 - W, 1e-6)
    new <- W * (matrix(colSums(B / den), n, N, byrow = TRUE) - B / den)       # teammates only: sum over i != j of B_i * w_j / (1 - w_i)
    fl <- floor(new); res <- round(colSums(B)) - colSums(fl); res[is.na(res)] <- 0; frac <- new - fl
    for (s in which(res > 0)) { o <- order(frac[, s], decreasing = TRUE)[seq_len(min(res[s], n))]; fl[o, s] <- fl[o, s] + 1 }
    out$ast[rows, ] <- as.integer(fl)
  }
  out$pts  <- out$tpm + 2L * out$fgm + out$ftm
  out$twom <- pmax(out$fgm - out$tpm, 0L)
  list(players = players, stats = out, minutes = MIN, drawn = drawn, fallback_teams = unique(fallback_teams), ess = ess, params = par)
}


# ============================================================================
# INPUT READING -- v1 workbook or old-format workbook; both reach the same engine
# ============================================================================

cbb_read_common <- function(file_path) {
  sheets <- excel_sheets(file_path)
  ids <- as.data.table(read_excel(file_path, sheet = "IDs"))
  setnames(ids, trimws(names(ids)))
  setnames(ids, "Name", "Player")
  games <- as.data.table(read_excel(file_path, sheet = "Games"))
  setnames(games, trimws(names(games)))
  if (!"ShowdownFile" %in% names(games)) games[, ShowdownFile := NA_character_]
  if (!"GameKey" %in% names(games)) games[, GameKey := paste0(FavTeam, "@", DogTeam)]
  games[, SimKey := gsub("@", "_vs_", GameKey)]
  sd_sheet_names <- grep("^SD\\d+_IDs$", sheets, value = TRUE)
  sd_ids <- if (length(sd_sheet_names) > 0) {
    setNames(lapply(sd_sheet_names, function(s) { dt <- as.data.table(read_excel(file_path, sheet = s)); setnames(dt, trimws(names(dt))); dt }), sub("_IDs$", "", sd_sheet_names))
  } else list()
  team_game_lu <- rbind(
    games[, .(Team = FavTeam, SimKey, GameKey, GameTime, GameRank, ShowdownFile)],
    games[, .(Team = DogTeam, SimKey, GameKey, GameTime, GameRank, ShowdownFile)]
  )
  slate <- merge(ids, team_game_lu, by = "Team", all.x = TRUE)
  slate[, PosGroup   := fcase(DKPos == "G/F", "G/F", DKPos == "G", "G", default = "F")]
  slate[, FDPosGroup := fcase(FDPos == "G/F", "G/F", FDPos == "G", "G", default = "F")]
  slate <- unique(slate, by = "Player")
  list(sheets = sheets, slate = slate, games = games, sd_ids = sd_ids, sd_sheet_names = sd_sheet_names)
}

# old-format workbook -> v1 input: the Sim_ sheet becomes the pool (weight = 1/(similarity + 0.001); residuals taken against THIS game's line, so a draw is the
# neighbour's realised total and margin, as before); the team percentile tabs become the shares fallback
cbb_old_to_v1 <- function(games, sim_games, team_data) {
  pool <- list()
  for (i in seq_len(nrow(games))) {
    g <- games[i]; key <- paste0(g$FavTeam, "_vs_", g$DogTeam)
    sim <- sim_games[[g$SimKey]]
    if (is.null(sim)) { parts <- strsplit(g$SimKey, "_vs_")[[1]]; sim <- sim_games[[paste0(parts[2], "_vs_", parts[1])]] }
    if (is.null(sim)) stop(sprintf("CBB input check: no Sim_ sheet found for game: %s", g$SimKey), call. = FALSE)
    P <- copy(sim); P[, weight := 1 / (as.numeric(similarity) + 0.001)]; P[, weight := weight / sum(weight)]
    fs <- P[[paste0(g$FavTeam, "_score")]]; ds <- P[[paste0(g$DogTeam, "_score")]]
    P[, res_total := (fs + ds) - g$Total]; P[, res_margin := (fs - ds) - abs(g$Spread)]
    pool[[key]] <- P
  }
  players <- rbindlist(lapply(names(team_data), function(tm) { d <- copy(team_data[[tm]]); d[, Team := tm]; d }), fill = TRUE)
  setnames(players, "Name", "Name", skip_absent = TRUE)
  list(params = list(), games = games, pool = pool, players = players, profiles = list(), foul = NULL)
}

read_cbb_input <- function(file_path) {
  cm <- cbb_read_common(file_path); sheets <- cm$sheets
  fixed <- c("IDs", "Games", "Params", "Profiles", "FoulBank", cm$sd_sheet_names, grep("^Sim_", sheets, value = TRUE))
  team_sheets <- setdiff(sheets, fixed)
  is_v1 <- "Profiles" %in% sheets && "Params" %in% sheets
  rd <- function(s) { dt <- as.data.table(read_excel(file_path, sheet = s)); setnames(dt, trimws(names(dt))); dt }
  if (is_v1) {
    prm <- rd("Params"); params <- as.list(setNames(as.numeric(prm$value), prm$key)); params <- params[!is.na(params)]
    for (nm in c("smult", "slot_sd", "blow_factor")) {
      sub <- params[grepl(paste0("^", nm, "\\."), names(params))]
      if (length(sub)) { params[names(sub)] <- NULL; params[[nm]] <- setNames(unlist(sub), sub("^[^.]*\\.", "", names(sub))) }
    }
    sh <- sheets; games <- copy(cm$games)
    pool <- setNames(lapply(seq_len(nrow(games)), function(i) {
      g <- games[i]; cand <- c(paste0("Sim_", g$SimKey), paste0("Sim_", i), paste0("Sim_", g$FavTeam, "_vs_", g$DogTeam))
      s <- cand[cand %in% sh][1]; if (is.na(s)) stop(sprintf("CBB input check: no Sim_ sheet found for game: %s", g$SimKey), call. = FALSE); rd(s)
    }), paste0(games$FavTeam, "_vs_", games$DogTeam))
    players <- rbindlist(lapply(team_sheets, function(s) {
      d <- rd(s); if (!"Team" %in% names(d)) d[, Team := s]
      rn <- c(DKProj = "RGProj", FDProj = "RGFDProj"); for (nm in names(rn)) if (nm %in% names(d)) setnames(d, nm, rn[[nm]])
      d
    }), fill = TRUE)
    prof <- rd("Profiles"); S8 <- .CBB_S8
    profiles <- lapply(split(prof, prof$Key), function(d) { m <- as.matrix(d[, paste0("r_", S8), with = FALSE]); colnames(m) <- S8; list(R = m, w = d$w) })
    fb <- NULL
    if ("FoulBank" %in% sheets) {
      f <- rd("FoulBank")
      fb <- list(base1 = f[item == "base1", value], HK = setNames(as.list(f[item == "HK", value]), f[item == "HK", k]),
                 banks = lapply(split(f[item == "bank"], f[item == "bank", k]), function(d) d$value))
      fb$e_k <- c(NA, sapply(2:5, function(k) max((fb$HK[[as.character(k)]] - fb$base1) / (1 - fb$base1), 0)))
    }
    v1 <- list(params = params, games = games, pool = pool, players = players, profiles = profiles, foul = fb)
    fmt <- "v1"
  } else {
    sim_names <- grep("^Sim_", sheets, value = TRUE)
    if (!length(sim_names)) stop("No Sim_ sheets found in input file.")
    sim_games <- setNames(lapply(sim_names, rd), sub("^Sim_", "", sim_names))
    team_data <- setNames(lapply(team_sheets, function(s) {
      dt <- rd(s); rename <- c(DKProj = "RGProj", FDProj = "RGFDProj")
      for (col in names(rename)) if (col %in% names(dt)) setnames(dt, col, rename[[col]])
      drop <- intersect(c("DKSal", "FDSal", "DKSalary", "FDSalary"), names(dt)); if (length(drop)) dt[, (drop) := NULL]
      dt
    }), team_sheets)
    if (!length(team_data)) stop("No team percentile tabs found.")
    v1 <- cbb_old_to_v1(cm$games, sim_games, team_data); fmt <- "old-format (shares)"
  }
  cat(sprintf("CBB Input (%s): %d players | %d games | %d team tabs | %d pool sheets\n", fmt, nrow(cm$slate), nrow(cm$games), length(team_sheets), length(v1$pool)))
  list(slate = cm$slate, games = cm$games, sd_ids = cm$sd_ids, v1 = v1, format = fmt)
}


# ============================================================================
# MAIN SIMULATION FUNCTION
# ============================================================================

run_cbb_simulation <- function(input_data, n_sims = 10000, config = NULL, progress_callback = NULL) {
  cb <- function(detail, value) {
    if (!is.null(progress_callback)) progress_callback(detail, value)
    cat(sprintf("  [%.0f%%] %s\n", value * 100, detail)); flush.console()
  }
  start_time <- proc.time(); slate <- input_data$slate; v1 <- input_data$v1
  par <- cbb_default_params(); if (length(v1$params)) for (nm in names(v1$params)) par[[nm]] <- v1$params[[nm]]
  team_abbrevs <- unique(slate$Team)
  for (ta in team_abbrevs) if (!ta %in% v1$players$Team) stop(sprintf("No team input found for: %s", ta), call. = FALSE)

  # ── players: team tab rows that are in the IDs tab (the IDs tab is authoritative for ids / salaries) ──────────────────
  cb("Building player roster...", 0.03)
  slate_cols <- c("Player", "DKID", "FDID", "DKSalary", "FDSalary", "PosGroup", "FDPosGroup", "GameKey", "SimKey", "GameTime", "GameRank", "Team")
  drop_tab <- c("DKSal", "FDSal", "DKSalary", "FDSalary", "DKID", "FDID", "PosGroup", "FDPosGroup", "GameKey", "SimKey", "GameTime", "GameRank")
  player_list <- rbindlist(lapply(team_abbrevs, function(team) {
    tab <- v1$players[Team == team]; sl <- slate[Team == team, ..slate_cols]
    matched <- tab[Name %in% sl$Player]
    if (nrow(matched) == 0) return(NULL)
    matched <- matched[, setdiff(names(matched), c(drop_tab, "Team")), with = FALSE]
    merged <- merge(matched, sl, by.x = "Name", by.y = "Player", all.x = TRUE)
    if (!"RGProj"   %in% names(merged)) merged[, RGProj   := NA_real_]
    if (!"RGFDProj" %in% names(merged)) merged[, RGFDProj := NA_real_]
    if (!"Mins"     %in% names(merged)) merged[, Mins     := NA_real_]
    merged[, Team := team]; merged
  }), fill = TRUE)
  if (nrow(player_list) == 0) stop("No players matched between IDs tab and team tabs.")
  cat(sprintf("  Active players: %d (of %d in IDs tab)\n", nrow(player_list), nrow(slate)))
  v1$players <- player_list
  cbb_validate(v1)                                   # an input the engine cannot simulate STOPS here, with the reason; it never simulates zeros
  if (!is.null(config) && !is.null(config$seed)) set.seed(config$seed)

  core <- cbb_core(v1, n_sims, par, cb)
  players <- core$players; n_players <- nrow(players); N <- n_sims; st <- core$stats
  player_names <- players$Name; player_teams <- players$Team

  # ── score DK and FD ───────────────────────────────────────────────────────────────────────────────────────────────────
  cb("Scoring DK and FD...", 0.88)
  dk_mat <- dk_score_cbb(st$pts, st$tpm, st$reb, st$ast, st$stl, st$blk, st$to)
  fd_mat <- fd_score_cbb(st$pts, st$tpm, st$reb, st$ast, st$stl, st$blk, st$to)

  # ── assemble long-format results ──────────────────────────────────────────────────────────────────────────────────────
  cb("Assembling results...", 0.92)
  sim_results <- data.table(SimID = rep(seq_len(N), each = n_players), Player = rep(player_names, times = N),
                            DKScore = as.vector(dk_mat), FDScore = as.vector(fd_mat))
  for (s in .CBB_S8) sim_results[[s]] <- as.integer(as.vector(st[[s]]))
  sim_results[["pts"]]  <- as.integer(as.vector(st$pts))
  sim_results[["twom"]] <- as.integer(as.vector(st$twom))
  if (!all(is.na(core$minutes))) sim_results[["min"]] <- as.vector(core$minutes)       # drawn minutes (NA for a team on the shares fallback)
  elapsed <- round((proc.time() - start_time)["elapsed"], 1)
  cat(sprintf("  Simulation core: %.1fs\n", elapsed))

  # ── metadata ──────────────────────────────────────────────────────────────────────────────────────────────────────────
  cb("Building metadata...", 0.96)
  keep_cols <- intersect(c("Name", "DKID", "FDID", "DKSalary", "FDSalary", "DKOwn", "FDOwn", "PosGroup", "FDPosGroup", "Team", "GameKey", "SimKey",
                           "GameTime", "GameRank", "RGProj", "RGFDProj", "Mins", "pred_min", "pf40"), names(player_list))
  metadata <- unique(player_list[, ..keep_cols], by = "Name"); setnames(metadata, "Name", "Player")
  metadata[, GameTimeSort := as.numeric(as.POSIXct(paste(Sys.Date(), GameTime), format = "%Y-%m-%d %I:%M %p", tz = "America/New_York"))]
  if (length(input_data$sd_ids) > 0) {
    game_sd_lu <- unique(rbind(input_data$games[, .(Team = FavTeam, ShowdownFile)], input_data$games[, .(Team = DogTeam, ShowdownFile)]))
    metadata <- merge(metadata, game_sd_lu, by = "Team", all.x = TRUE)
    sd_all <- rbindlist(lapply(names(input_data$sd_ids), function(n) { dt <- copy(input_data$sd_ids[[n]]); dt[, SDFile := n]; dt }), fill = TRUE)
    setnames(sd_all, "Name", "Player")
    sd_join_cols <- intersect(c("Player", "Team", "SDFile", "CPT_ID", "CPT_Salary", "UTIL_ID", "UTIL_Salary"), names(sd_all))
    metadata <- merge(metadata, sd_all[, ..sd_join_cols][, .(Player, Team, SDFile, CPTID = CPT_ID, CPTSalary = CPT_Salary,
                        SDID = if ("UTIL_ID" %in% names(.SD)) UTIL_ID else NA_integer_, SDSalary = if ("UTIL_Salary" %in% names(.SD)) UTIL_Salary else NA_real_)],
                      by.x = c("Player", "Team", "ShowdownFile"), by.y = c("Player", "Team", "SDFile"), all.x = TRUE)
  }
  sim_results <- sim_results[Player %in% metadata$Player]
  cat(sprintf("CBB sim complete: %d sims | %d players | %d rows | %s\n", N, nrow(metadata), nrow(sim_results), CBB_ENGINE_VERSION))
  has_fd <- "FDSalary" %in% names(metadata) && any(!is.na(metadata$FDSalary) & metadata$FDSalary > 0)
  has_sd <- "CPTSalary" %in% names(metadata) && any(!is.na(metadata$CPTSalary) & metadata$CPTSalary > 0)

  # ── visuals ───────────────────────────────────────────────────────────────────────────────────────────────────────────
  cb("Building visuals data...", 0.98)
  teams <- sort(unique(metadata$Team)); twom_mat <- st$twom
  player_means <- data.table(Player = player_names, Team = player_teams, AvgFP = round(rowMeans(dk_mat), 1), pts = round(rowMeans(st$pts), 1), tpm = round(rowMeans(st$tpm), 1),
                             twom = round(rowMeans(twom_mat), 1), ftm = round(rowMeans(st$ftm), 1), reb = round(rowMeans(st$reb), 1), ast = round(rowMeans(st$ast), 1),
                             stl = round(rowMeans(st$stl), 1), blk = round(rowMeans(st$blk), 1), to = round(rowMeans(st$to), 1))
  setorder(player_means, Team, -AvgFP)
  team_means <- rbindlist(lapply(teams, function(tm) {
    pidx <- which(player_teams == tm); cs <- function(m) round(mean(colSums(m[pidx, , drop = FALSE])), 1)
    data.table(Team = tm, AvgFP = cs(dk_mat), pts = cs(st$pts), tpm = cs(st$tpm), twom = cs(twom_mat), ftm = cs(st$ftm), reb = cs(st$reb), ast = cs(st$ast), stl = cs(st$stl), blk = cs(st$blk), to = cs(st$to))
  }))
  setorder(team_means, -AvgFP)
  sport_visuals <- list(teams = teams, player_means = player_means, team_means = team_means)

  # ── record: what the review needs besides the stat draws (which ride in sim_results) ────────────────────────────────────
  record <- list(engine = cbb_engine_info(), n_sims = N, params = core$params[c("sigma_rate", "rho", "c_total", "c_margin", "p_unforced", "p_exit", "foul_module", "mixture", "blowout_shift")],
                 input_format = input_data$format, fallback_teams = core$fallback_teams, pool_ess = core$ess,
                 drawn = lapply(core$drawn, function(d) list(total_mean = mean(d$total), total_sd = sd(d$total), margin_mean = mean(d$margin_fav), margin_sd = sd(d$margin_fav))),
                 minutes_mean = if (all(is.na(core$minutes))) NULL else setNames(rowMeans(core$minutes), paste(player_teams, player_names, sep = "|")))
  list(sim_results = sim_results, metadata = metadata, has_fd = has_fd, has_sd = has_sd, sport_visuals = sport_visuals, record = record, engine = cbb_engine_info())
}



# ============================================================================
# LINEUP METRICS PLACEHOLDER
# ============================================================================

calculate_cbb_lineup_metrics <- function(scored_lineups, sim_results, metadata) {
  scored_lineups
}


# ============================================================================
# SHARED SLOT ASSIGNMENT HELPER
#
# Rules (deterministic — same 8 players always produce identical output):
#   1. Sort by game_rank ASC then Player name ASC (alpha within same game)
#   2. Iterate in that order; each player fills their eligible positional slot
#      (G preferred for G-eligible, F for F-eligible) if still open
#   3. Once all positional slots are filled, remaining players go to UTIL
#      -> latest-game players end up in UTIL naturally
#   4. Alphabetical within game ensures identical lineups score identically
#      regardless of which sim produced them
#
# Args:
#   cm    : data.table with Player, PosGroup, game_rank (chosen players only)
#   n_g   : G slots required
#   n_f   : F slots required
#   n_util: UTIL slots
# Returns: list(g, f, util) or NULL if assignment impossible
# ============================================================================

assign_cbb_slots <- function(cm, n_g, n_f, n_util) {
  # Primary: game_rank ASC (earliest game first)
  # Secondary: Player ASC (alphabetical within same game — deterministic)
  setorder(cm, game_rank, Player)
  
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
      return(NULL)
    }
  }
  
  if (length(g_players) != n_g || length(f_players) != n_f ||
      length(u_players) != n_util) return(NULL)
  
  all_assigned <- c(g_players, f_players, u_players)
  if (anyNA(all_assigned) || length(unique(all_assigned)) != nrow(cm)) return(NULL)
  
  list(g = g_players, f = f_players, util = u_players)
}


# ============================================================================
# CBB DK LINEUP OPTIMIZER
# DK roster: 3G / 3F / 2UTIL = 8 players | $50K cap
# Per-sim: filter top 25 G + top 25 F by PPD, LP with position constraints,
# assign UTIL slots to latest game-time players post-LP.
#
# FIX: slot assignment attrition — cap to max_lineups AFTER slot assignment
# so that failed assignments don't reduce the final pool below the target.
# ============================================================================

find_optimal_lineups_cbb <- function(sim_results, metadata, config, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: Finding optimal CBB lineups (per-sim LP)...\n")
  
  setDT(sim_results); setDT(metadata)
  
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  
  meta <- unique(metadata[, .(Player, DKSalary, PosGroup, GameKey)], by = "Player")
  meta[, g_elig := PosGroup %in% c("G", "G/F")]
  meta[, f_elig := PosGroup %in% c("F", "G/F")]
  
  # GameRank from input sheet (1 = first game, 2 = second game, etc.)
  if ("GameRank" %in% names(metadata)) {
    meta <- merge(meta, unique(metadata[, .(Player, GameRank)]), by = "Player", all.x = TRUE)
    meta[, game_rank := GameRank]
    meta[is.na(game_rank), game_rank := 1L]
    meta[, GameRank := NULL]
  } else {
    meta[, game_rank := 1L]
  }
  
  opt_data <- merge(
    sim_results[, .(SimID, Player, FantasyPoints = DKScore)],
    meta[, .(Player, Salary = DKSalary, g_elig, f_elig, game_rank)],
    by = "Player"
  )
  opt_data <- opt_data[Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints)]
  setkey(opt_data, SimID)
  
  sim_ids   <- unique(opt_data$SimID)
  n_sims    <- length(sim_ids)
  start_t   <- Sys.time()
  prog_freq <- max(1L, n_sims %/% 20L)
  
  if (verbose) cat(sprintf("  %s players | %s sims | $%s cap\n",
                           format(nrow(meta), big.mark=","),
                           format(n_sims, big.mark=","),
                           format(salary_cap, big.mark=",")))
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid  <- sim_ids[i]
    pool <- opt_data[.(sid)]
    
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
    sig    <- paste(sort(chosen$Player), collapse="|")
    lineup_list[[i]] <- data.table(
      Lineup      = sig,
      TotalSalary = sum(chosen$Salary),
      TotalScore  = sum(chosen$FantasyPoints)
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
  
  # Aggregate all unique lineups — do NOT cap before slot assignment.
  # Slot assignment drops a small fraction of lineups; capping early causes the
  # final pool to land below max_lineups. Cap AFTER slot assignment instead.
  all_dt <- rbindlist(valid)
  counts <- all_dt[, .(
    Top1Count=.N, TotalSalary=TotalSalary[1], AvgScore=mean(TotalScore)
  ), by=Lineup]
  counts[, rand := runif(.N)]
  setorder(counts, -Top1Count, rand)
  counts[, rand := NULL]
  
  # Slot assignment: done once per unique lineup on the canonical player set.
  # Deterministic: same 8 players always get the same slots regardless of sim order.
  slot_list <- vector("list", nrow(counts))
  for (li in seq_len(nrow(counts))) {
    players <- strsplit(counts$Lineup[li], "\\|")[[1]]
    cm      <- meta[Player %in% players, .(Player, PosGroup, game_rank)]
    slots   <- assign_cbb_slots(cm, n_g=3L, n_f=3L, n_util=2L)
    if (!is.null(slots)) {
      slot_list[[li]] <- data.table(
        Lineup=counts$Lineup[li],
        G1=slots$g[1], G2=slots$g[2], G3=slots$g[3],
        F1=slots$f[1], F2=slots$f[2], F3=slots$f[3],
        UTIL1=slots$util[1], UTIL2=slots$util[2]
      )
    }
  }
  slot_dt <- rbindlist(slot_list[!sapply(slot_list, is.null)])
  counts  <- merge(counts, slot_dt, by="Lineup", all.x=TRUE)
  
  unique_lineups <- counts[!is.na(G1), .(
    TotalSalary, Top1Count, AvgScore,
    Player1=G1, Player2=G2, Player3=G3,
    Player4=F1, Player5=F2, Player6=F3,
    Player7=UTIL1, Player8=UTIL2
  )]
  
  # Cap AFTER slot assignment — guarantees a full pool of max_lineups
  if (nrow(unique_lineups) > max_lineups) unique_lineups <- unique_lineups[1:max_lineups]
  
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
#
# FIX: same slot-assignment attrition fix as DK — cap AFTER slot assignment.
# ============================================================================

find_optimal_lineups_cbb_fd <- function(sim_results, metadata, config, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: Finding optimal CBB FD lineups (per-sim LP)...\n")
  
  setDT(sim_results); setDT(metadata)
  
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  
  # Use FDPosGroup if available, fall back to PosGroup
  pos_col <- if ("FDPosGroup" %in% names(metadata)) "FDPosGroup" else "PosGroup"
  meta <- unique(metadata[, .(Player, FDSalary, PosGroup = get(pos_col), GameKey)], by = "Player")
  meta[, g_elig := PosGroup %in% c("G", "G/F")]
  meta[, f_elig := PosGroup %in% c("F", "G/F")]
  meta <- meta[FDSalary > 0 & !is.na(FDSalary)]
  
  # GameRank from input sheet (1 = first game, 2 = second game, etc.)
  if ("GameRank" %in% names(metadata)) {
    meta <- merge(meta, unique(metadata[, .(Player, GameRank)]), by = "Player", all.x = TRUE)
    meta[, game_rank := GameRank]
    meta[is.na(game_rank), game_rank := 1L]
    meta[, GameRank := NULL]
  } else {
    meta[, game_rank := 1L]
  }
  
  opt_data <- merge(
    sim_results[, .(SimID, Player, FantasyPoints = FDScore)],
    meta[, .(Player, Salary = FDSalary, g_elig, f_elig, game_rank)],
    by = "Player"
  )
  opt_data <- opt_data[Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints)]
  setkey(opt_data, SimID)
  
  sim_ids   <- unique(opt_data$SimID)
  n_sims    <- length(sim_ids)
  start_t   <- Sys.time()
  prog_freq <- max(1L, n_sims %/% 20L)
  
  if (verbose) cat(sprintf("  %s players | %s sims | $%s cap\n",
                           format(nrow(meta), big.mark=","),
                           format(n_sims, big.mark=","),
                           format(salary_cap, big.mark=",")))
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid  <- sim_ids[i]
    pool <- opt_data[.(sid)]
    
    n_p <- nrow(pool)
    if (n_p < 8L) next
    
    # FD constraints: 8 total, <=50K, >=4 G-elig, >=3 F-elig
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
    sig    <- paste(sort(chosen$Player), collapse="|")
    lineup_list[[i]] <- data.table(
      Lineup      = sig,
      TotalSalary = sum(chosen$Salary),
      TotalScore  = sum(chosen$FantasyPoints)
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
  
  # Aggregate all unique lineups — do NOT cap before slot assignment.
  # Slot assignment drops a small fraction of lineups; capping early causes the
  # final pool to land below max_lineups. Cap AFTER slot assignment instead.
  all_dt <- rbindlist(valid)
  counts <- all_dt[, .(
    Top1Count=.N, TotalSalary=TotalSalary[1], AvgScore=mean(TotalScore)
  ), by=Lineup]
  counts[, rand := runif(.N)]
  setorder(counts, -Top1Count, rand)
  counts[, rand := NULL]
  
  slot_list <- vector("list", nrow(counts))
  for (li in seq_len(nrow(counts))) {
    players <- strsplit(counts$Lineup[li], "\\|")[[1]]
    cm      <- meta[Player %in% players, .(Player, PosGroup, game_rank)]
    slots   <- assign_cbb_slots(cm, n_g=4L, n_f=3L, n_util=1L)
    if (!is.null(slots)) {
      slot_list[[li]] <- data.table(
        Lineup=counts$Lineup[li],
        G1=slots$g[1], G2=slots$g[2], G3=slots$g[3], G4=slots$g[4],
        F1=slots$f[1], F2=slots$f[2], F3=slots$f[3],
        UTIL1=slots$util[1]
      )
    }
  }
  slot_dt <- rbindlist(slot_list[!sapply(slot_list, is.null)])
  counts  <- merge(counts, slot_dt, by="Lineup", all.x=TRUE)
  
  unique_lineups <- counts[!is.na(G1), .(
    TotalSalary, Top1Count, AvgScore,
    Player1=G1, Player2=G2, Player3=G3, Player4=G4,
    Player5=F1, Player6=F2, Player7=F3,
    Player8=UTIL1
  )]
  
  # Cap AFTER slot assignment — guarantees a full pool of max_lineups
  if (nrow(unique_lineups) > max_lineups) unique_lineups <- unique_lineups[1:max_lineups]
  
  elapsed <- as.numeric(difftime(Sys.time(), start_t, units="secs"))
  if (verbose) cat(sprintf("  \u2713 Phase 1: %s unique FD lineups from %s sims | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","),
                           format(n_sims, big.mark=","), elapsed))
  
  list(unique_lineups=unique_lineups, n_sims=n_sims, config=config, mode="cbb_fd")
}


# ============================================================================
# CBB SD (SHOWDOWN) LINEUP OPTIMIZER
# DK Showdown: 1 CPT + 5 FLEX = 6 players | $50K cap
# CPT:  uses CPTSalary, scores at 1.5x DKScore
# FLEX: uses SDSalary (UTIL_Salary from SD sheet), scores at 1.0x DKScore
# Both CPT and FLEX IDs are independent from DK Classic IDs.
# Per-sim greedy: try each player as CPT, greedy-fill 5 FLEX by PPD.
#
# NOTE: SD has no slot assignment step, so no attrition issue here.
# The max_lineups cap at the end is already in the correct position.
# ============================================================================

find_optimal_lineups_cbb_sd <- function(sim_results, metadata, config, verbose = TRUE) {
  
  if (verbose) cat("\nPhase 1: Finding optimal CBB Showdown lineups (per-sim greedy)...\n")
  
  setDT(sim_results); setDT(metadata)
  
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  cpt_mult    <- 1.5
  
  # Players must have both CPTSalary and SDSalary to be eligible
  meta <- unique(metadata[
    !is.na(CPTSalary) & CPTSalary > 0 & !is.na(SDSalary) & SDSalary > 0,
    .(Player, Team, CPTSalary, SDSalary, GameKey)
  ], by = "Player")
  
  if (nrow(meta) == 0) stop("No SD-eligible players found. Check CPTSalary/SDSalary in metadata.")
  
  # Confirm there are two distinct teams — required for the both-teams constraint
  game_teams <- unique(meta$Team)
  if (length(game_teams) < 2)
    warning("find_optimal_lineups_cbb_sd: fewer than 2 teams found in SD pool — both-teams constraint will have no effect.")
  
  # Merge scores into opt_data — one row per SimID x Player (Team included for constraint)
  opt_data <- merge(
    sim_results[, .(SimID, Player, DKScore)],
    meta[, .(Player, Team, CPTSalary, SDSalary)],
    by = "Player"
  )
  opt_data <- opt_data[!is.na(DKScore)]
  setkey(opt_data, SimID)
  
  sim_ids   <- unique(opt_data$SimID)
  n_sims    <- length(sim_ids)
  start_t   <- Sys.time()
  prog_freq <- max(1L, n_sims %/% 20L)
  
  if (verbose) cat(sprintf("  %d SD-eligible players | %s sims | $%s cap | %.1fx CPT\n",
                           nrow(meta), format(n_sims, big.mark=","),
                           format(salary_cap, big.mark=","), cpt_mult))
  
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid  <- sim_ids[i]
    sd   <- opt_data[.(sid)]
    setorder(sd, -DKScore)
    
    best_score  <- -Inf
    best_lineup <- NULL
    
    for (ci in seq_len(nrow(sd))) {
      cpt_player <- sd$Player[ci]
      cpt_sal    <- sd$CPTSalary[ci]
      cpt_score  <- sd$DKScore[ci] * cpt_mult
      if (cpt_sal > salary_cap) next
      
      rem_cap <- salary_cap - cpt_sal
      flex    <- sd[Player != cpt_player]
      setorder(flex, -DKScore)
      
      # Greedy fill 5 FLEX by DKScore under remaining cap using SDSalary
      picked_f   <- character(5L)
      n_picked   <- 0L
      sal_used   <- 0
      flex_score <- 0
      
      for (j in seq_len(nrow(flex))) {
        if (n_picked == 5L) break
        if (sal_used + flex$SDSalary[j] <= rem_cap) {
          n_picked           <- n_picked + 1L
          picked_f[n_picked] <- flex$Player[j]
          sal_used           <- sal_used + flex$SDSalary[j]
          flex_score         <- flex_score + flex$DKScore[j]
        }
      }
      
      if (n_picked == 5L) {
        # Both-teams constraint: lineup must include at least one player from each team
        all_players  <- c(cpt_player, picked_f[seq_len(n_picked)])
        lineup_teams <- sd$Team[match(all_players, sd$Player)]
        if (length(unique(lineup_teams)) < 2L) next
        
        total <- cpt_score + flex_score
        if (total > best_score) {
          best_score  <- total
          best_lineup <- list(
            Captain     = cpt_player,
            Flex        = sort(picked_f),
            TotalSalary = cpt_sal + sal_used,
            TotalScore  = total
          )
        }
      }
    }
    
    if (!is.null(best_lineup)) {
      lineup_list[[i]] <- data.table(
        Lineup      = paste(c(best_lineup$Captain, best_lineup$Flex), collapse = "|"),
        TotalSalary = best_lineup$TotalSalary,
        TotalScore  = best_lineup$TotalScore,
        Captain     = best_lineup$Captain,
        Util1       = best_lineup$Flex[1],
        Util2       = best_lineup$Flex[2],
        Util3       = best_lineup$Flex[3],
        Util4       = best_lineup$Flex[4],
        Util5       = best_lineup$Flex[5]
      )
    }
    
    if (verbose && i %% prog_freq == 0L) {
      cat(sprintf("\r  Phase 1: %d%% | %.1fs",
                  round(i/n_sims*100),
                  as.numeric(difftime(Sys.time(), start_t, units="secs"))))
      flush.console()
    }
  }
  if (verbose) cat("\n")
  
  valid <- lineup_list[!sapply(lineup_list, is.null)]
  if (length(valid) == 0L) stop("No valid CBB Showdown lineups found")
  
  all_dt <- rbindlist(valid)
  counts <- all_dt[, .(
    Top1Count   = .N,
    TotalSalary = TotalSalary[1],
    AvgScore    = mean(TotalScore),
    Captain     = Captain[1],
    Util1=Util1[1], Util2=Util2[1], Util3=Util3[1], Util4=Util4[1], Util5=Util5[1]
  ), by = Lineup]
  counts[, rand := runif(.N)]
  setorder(counts, -Top1Count, rand)
  counts[, rand := NULL]
  if (nrow(counts) > max_lineups) counts <- counts[1:max_lineups]
  
  unique_lineups <- counts[, .(
    TotalSalary, Top1Count, AvgScore,
    Captain, Util1, Util2, Util3, Util4, Util5
  )]
  
  elapsed <- as.numeric(difftime(Sys.time(), start_t, units="secs"))
  if (verbose) cat(sprintf("  \u2713 Phase 1: %s unique SD lineups from %s sims | %.1fs\n",
                           format(nrow(unique_lineups), big.mark=","),
                           format(n_sims, big.mark=","), elapsed))
  
  list(unique_lineups=unique_lineups, n_sims=n_sims, config=config, mode="captain")
}
