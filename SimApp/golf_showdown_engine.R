# ============================================================================
# GOLF SHOWDOWN ENGINE -- Round 1 (DK single-round showdown), built on golf v2
# ============================================================================
# Sourced by golf_engine_v2.R. Same sheet as classic (Player + Event tabs) plus
# showdown columns on the Player tab: SDSalary, SDID (and CPTID / CPTSalary for
# captain-mode contests). Golfers with no SDSalary are not in the contest pool
# but are still simulated (the skill fit, waves and conditions need the whole
# field). Round 1 only: R2-R4 need the played rounds (see design note).
#
# Scoring (DK, rounds 1-3): hole dbl eagle+ 16, eagle 11, birdie 5.75, par 1.5,
# bogey -1.8, dbl bogey+ -3.9; +5 streak of 3 birdies+, +5 bogey-free round,
# +5 hole-in-one. No finish points, no sub-70 bonus. Points are drawn from REAL
# rounds at the same score to par (golf/round_pool_sd.csv, built by
# GTS/Golf/showdown_work/make_sd_pool.R), same idea as classic's pool.
# ============================================================================

GOLF_SD <- local({
  f <- file.path(GOLF_V2_DIR, "round_pool_sd.csv")
  P <- if (file.exists(f)) data.table::fread(f) else NULL
  list(pool = P, idx = if (!is.null(P)) split(seq_len(nrow(P)), P$stp) else NULL)
})

golf_sd_round_points <- function(stp) {
  P <- GOLF_SD$pool
  if (is.null(P)) stop("golf/round_pool_sd.csv is missing: run GTS/Golf/showdown_work/make_sd_pool.R")
  lo <- min(P$stp); hi <- max(P$stp)
  s <- pmin(pmax(stp, lo), hi); out <- numeric(length(stp))
  for (v in names(grp <- split(seq_along(s), s))) {
    i <- grp[[v]]; idx <- GOLF_SD$idx[[v]]
    out[i] <- P$sd[idx[sample.int(length(idx), length(i), replace = TRUE)]]
  }
  # below the pool's best round: ~4.2 pts per extra stroke (hole scale slope)
  out + 4.2 * (s - stp)
}

# ---------------------------------------------------------------------------
# PLAYED ROUNDS (Round 2+ showdown). Additive: none of the v2 functions it
# mirrors is touched, so classic golf and the Round 1 path are unchanged.
#
# `played` is an n x k matrix of REAL strokes to par for rounds 1..k (NA = not
# played / WD). They are fixed in every sim and the sim covers rounds k+1..4.
# Two pieces of the model learn from them (normal-normal updates, design note
# engine_golf_showdown_design.md 2.2):
#   event level  Level + Lev_s: the field's realised mean of each played round,
#                net of skill and wave, is Lev + C_r + noise. Lev's posterior
#                is the share v_L / (v_L + v_C/k + v_e) of the deviation from
#                the sheet's Level, with a smaller sd. C_r (round conditions)
#                is NOT carried to later rounds: it is drawn fresh.
#   week effect  u_i: the golfer's mean residual over his played rounds
#                (net of the field's round mean, skill, expected wave) shrinks
#                to s_u^2 / (s_u^2 + sigma_i^2 / k_i) -- ~4% per round at
#                s_u 0.55, sigma 2.7, so one hot round barely moves him.
# Skill mu is fitted to the sheet's odds ladder WITH the played rounds fixed
# (golf_v2_fit_played): post-round odds already contain those rounds.
# ---------------------------------------------------------------------------
golf_v2_posterior <- function(mu, early, ev, played, pars = GOLF_V2$pars) {
  n <- length(mu); k <- ncol(played)
  sig <- pmax(pars$sig0 + pars$sig_slope * mu, 1.5)
  wexp <- function(r) {                       # expected wave effect on a played round
    if (r > 2 || is.na(early[1, r]) && all(is.na(early[, r]))) return(rep(0, n))
    ifelse(is.na(early[, r]), 0, ifelse(early[, r], 0.5, -0.5)) * pars$wave_mu
  }
  shared <- rep(NA_real_, k); ve <- 0
  for (r in seq_len(k)) {
    ok <- !is.na(played[, r])
    if (!any(ok)) next
    shared[r] <- mean(played[ok, r] - mu[ok] - wexp(r)[ok])
    ve <- ve + mean(sig[ok]^2) / sum(ok)
  }
  used <- !is.na(shared); ku <- sum(used)
  v_L <- (ev$lev_sd %||% pars$lev_sd)^2
  if (ku == 0) return(list(lev_m = 0, lev_v = v_L, u_m = rep(0, n), u_v = rep(pars$s_u^2, n), shared = shared))
  dev <- mean(shared[used] - ev$level)
  v_C <- pars$C_sd^2 / ku; v_e <- ve / ku^2
  tot <- v_L + v_C + v_e
  rs <- numeric(n); ki <- integer(n)
  for (r in which(used)) {
    ok <- !is.na(played[, r])
    rs[ok] <- rs[ok] + (played[ok, r] - shared[r] - mu[ok] - wexp(r)[ok]); ki[ok] <- ki[ok] + 1L
  }
  rbar <- ifelse(ki > 0, rs / pmax(ki, 1L), 0); s2 <- pars$s_u^2
  list(lev_m = v_L / tot * dev, lev_v = v_L * (v_C + v_e) / tot,
       u_m = ifelse(ki > 0, rbar * s2 / (s2 + sig^2 / pmax(ki, 1L)), 0),
       u_v = ifelse(ki > 0, s2 * sig^2 / (ki * s2 + sig^2), s2), shared = shared)
}

# golf_v2_sim with rounds 1..ncol(played) fixed. Same return shape.
golf_v2_sim_played <- function(mu, early, dr, ev, played, pars = GOLF_V2$pars) {
  S <- nrow(dr$u); n <- length(mu); k <- ncol(played)
  post <- golf_v2_posterior(mu, early, ev, played, pars)
  sig  <- matrix(pmax(pars$sig0 + pars$sig_slope * mu, 1.5), S, n, byrow = TRUE)
  muM  <- matrix(mu, S, n, byrow = TRUE)
  uM   <- matrix(post$u_m, S, n, byrow = TRUE) + matrix(sqrt(post$u_v), S, n, byrow = TRUE) * dr$u
  lev  <- ev$level + post$lev_m + sqrt(post$lev_v) * dr$lev
  strokes <- array(0L, c(S, n, 4)); cum <- matrix(0, S, n); alive <- matrix(TRUE, S, n)
  cut_tot <- NULL
  for (r in 1:4) {
    if (r <= k) {
      ok <- !is.na(played[, r])
      xi <- matrix(ifelse(ok, played[, r], 0L), S, n, byrow = TRUE)
      alive <- alive & matrix(ok, S, n, byrow = TRUE)            # no score = out (WD / missed the cut)
    } else {
      x <- lev + muM + uM + sig * dr$eps[, , r] + pars$C_sd * dr$C[, r]
      if (r <= 2) {
        e <- early[, r]
        if (any(!is.na(e))) {
          d <- pars$wave_mu + pars$wave_sd * dr$wave[, r]
          x <- x + outer(d, ifelse(is.na(e), 0, ifelse(e, 0.5, -0.5)))
        }
      }
      xi <- floor(x) + (dr$frac[, , r] < (x - floor(x)))
    }
    strokes[, , r] <- xi
    cum <- cum + xi * alive
    if (ev$cut_after > 0 && r == ev$cut_after) {
      cut_tot <- cum
      alive <- alive & row_rank(cum, "min") <= ev$cut_n
    }
  }
  tot <- cum; tot[!alive] <- NA
  pos <- row_rank(tot, "min"); pos_hi <- row_rank(tot, "max")
  mc_rank <- NULL
  if (!is.null(cut_tot)) { ct <- cut_tot; ct[alive] <- NA; mc_rank <- row_rank(ct, "random") }
  list(strokes = strokes, made = alive, pos = pos, pos_hi = pos_hi, mc_rank = mc_rank, post = post)
}

# golf_v2_fit with the played rounds fixed (the posterior is recomputed from the current mu each step)
golf_v2_fit_played <- function(mk, early, ev, played, S = 2000L, iters = 25L, step = 0.8, cb = NULL) {
  n <- nrow(mk); rungs <- colnames(mk)
  slope <- ifelse(rungs == "Cut", 0.5, 0.7)
  if (ev$cut_after > 0 && ev$cut_after <= ncol(played)) mk[, "Cut"] <- NA     # the cut is a fact now
  use <- !is.na(mk) & mk > 0 & mk < 1
  priced <- rowSums(use) > 0
  dr <- golf_v2_draws(n, S)
  mu <- rep(0, n)
  bottom <- function(mu) quantile(mu[priced], .95) + 0.3
  for (it in seq_len(iters)) {
    ps <- golf_v2_rung_probs(golf_v2_sim_played(mu, early, dr, ev, played))[, rungs, drop = FALSE]
    ps <- (ps * S + 0.5) / (S + 1)
    g  <- (qnorm(ps) - qnorm(pmin(pmax(mk, 1e-6), 1 - 1e-6))) / matrix(slope, n, length(rungs), byrow = TRUE)
    g[!use] <- 0
    mu <- mu + step * rowSums(g * use) / pmax(rowSums(use), 1)
    mu[!priced] <- bottom(mu)
    mu <- mu - mean(mu)
    if (!is.null(cb)) cb(0.05 + 0.35 * it / iters, sprintf("Fitting skill to the odds ladder, rounds played fixed (%d/%d)...", it, iters))
  }
  mu
}

# Real strokes of rounds 1..k from the Player tab columns R1..Rk (strokes to par; a column of
# raw strokes, median > 30, has par subtracted). NULL when a column is missing or nearly empty.
golf_sd_played_matrix <- function(players_dt, k, par) {
  cols <- paste0("R", seq_len(k))
  if (k < 1 || !all(cols %in% names(players_dt))) return(NULL)
  m <- sapply(cols, function(cn) suppressWarnings(as.numeric(players_dt[[cn]])))
  m <- matrix(m, nrow(players_dt), k)
  for (j in seq_len(k)) if (any(!is.na(m[, j])) && median(abs(m[, j]), na.rm = TRUE) > 30) m[, j] <- m[, j] - par
  if (sum(!is.na(m[, 1])) < 10) return(NULL)
  m
}

# mode "flex": six equal slots; "captain": CPT 1.5x points and 1.5x salary (sheet's CPTSalary wins)
# round: the round the contest covers (1-4); NULL = the Event tab's `Round` cell, else 1. For round > 1 the
# Player tab's R1..R(round-1) columns (real strokes to par) are fixed in every sim (see above); without them
# the odds are fitted as-is and the round is simulated unconditioned (said in the log line).
run_golf_showdown_simulation <- function(input_data, n_sims = 10000, progress_callback = NULL,
                                         mode = c("flex", "captain"), fit_sims = 2000L, batch = 2500L,
                                         round = NULL) {
  mode <- match.arg(mode); t0 <- Sys.time()
  players_dt <- process_golf_players(input_data$player)
  n_p <- nrow(players_dt)
  if (!"SDSalary" %in% names(players_dt) || !any(!is.na(players_dt$SDSalary)))
    stop("Showdown sheet needs SDSalary (and SDID) on the Player tab")
  in_pool <- !is.na(players_dt$SDSalary)
  ev <- golf_v2_event(input_data$event, n_p); n <- ev$field
  cb <- if (is.null(progress_callback)) function(v, m) invisible() else progress_callback
  if (is.null(round)) {
    rc <- if (!is.null(input_data$event) && "Round" %in% names(input_data$event))
            suppressWarnings(as.integer(input_data$event$Round[1])) else NA_integer_
    round <- if (is.na(rc)) 1L else rc
  }
  round <- as.integer(round)
  if (!round %in% 1:4) stop("round must be 1, 2, 3 or 4")
  played <- if (round > 1L) golf_sd_played_matrix(players_dt, round - 1L, ev$par) else NULL
  if (round == 1L) {
    cat(sprintf("Golf showdown R1 (%s) | %d golfers simulated | %d in pool | %d sims\n", mode, n_p, sum(in_pool), n_sims))
  } else {
    cat(sprintf("Golf showdown R%d (%s) | %d golfers simulated | %d in pool | %d sims | %s\n", round, mode, n_p,
                sum(in_pool), n_sims,
                if (is.null(played)) sprintf("R1-R%d scores NOT on the sheet: odds fitted as-is, round NOT conditioned", round - 1L)
                else sprintf("rounds 1-%d fixed from the sheet (%d golfers with R1)", round - 1L, sum(!is.na(played[, 1])))))
  }

  rn <- c(names(GOLF_V2$rungs), "Cut")
  mk <- matrix(NA_real_, n, length(rn), dimnames = list(NULL, rn))
  for (r in intersect(rn, names(players_dt))) mk[seq_len(n_p), r] <- as.numeric(players_dt[[r]])
  if (ev$cut_after == 0) mk[, "Cut"] <- NA
  early <- golf_v2_waves(players_dt)
  mu <- if (is.null(played)) golf_v2_fit(mk, early, ev, S = fit_sims, cb = cb)
        else golf_v2_fit_played(mk, early, ev, played, S = fit_sims, cb = cb)
  post <- if (is.null(played)) NULL else golf_v2_posterior(mu, early, ev, played)

  keep <- which(in_pool); k <- length(keep)
  n_b <- ceiling(n_sims / batch); sc <- vector("list", n_b); rk <- sc
  for (b in seq_len(n_b)) {
    S  <- min(batch, n_sims - (b - 1L) * batch)
    dr <- golf_v2_draws(n, S)
    sm <- if (is.null(played)) golf_v2_sim(mu, early, dr, ev) else golf_v2_sim_played(mu, early, dr, ev, played)
    sr <- sm$strokes[, , round]                         # the contest round's strokes to par
    if (round > 1L && ev$cut_after > 0 && round > ev$cut_after) sr[!sm$made] <- NA
    st <- sr[, keep, drop = FALSE]
    pts <- matrix(golf_sd_round_points(as.integer(st)), S, k)
    pts[is.na(st)] <- 0                                 # missed the cut: no round played
    sc[[b]] <- t(pts)
    rk[[b]] <- t(row_rank(sr, "min")[, keep, drop = FALSE])   # that round's rank among the whole field
    cb(0.40 + 0.5 * b / n_b, sprintf("Simulating round %d (%d/%d)...", round, b, n_b))
  }
  score <- do.call(cbind, sc); rank1 <- do.call(cbind, rk)

  nm <- players_dt$Name[keep]
  sim_results <- data.table(SimID = rep(seq_len(n_sims), each = k), Player = rep(nm, times = n_sims),
                            Pool = rep(players_dt$Pool[keep], times = n_sims),
                            FinishPosition = as.integer(as.vector(rank1)),
                            DKScore = as.vector(score), FDScore = 0)
  g <- function(col, d = NA) if (col %in% names(players_dt)) players_dt[[col]][keep] else rep(d, k)
  sim_metadata <- data.table(Player = nm, Pool = players_dt$Pool[keep],
                             TeeTimeGroup = players_dt$TeeTimeGroup[keep] %||% "Unknown",
                             DKSalary = g("SDSalary"), DKOwn = g("DKOP", 0), DKID = g("SDID", NA_character_),
                             SDSalary = g("SDSalary"), SDID = g("SDID", NA_character_))
  if (mode == "captain") {
    sim_metadata[, CPTID := g("CPTID", NA_character_)]
    sim_metadata[, CPTSalary := fifelse(is.na(g("CPTSalary")), 1.5 * SDSalary, g("CPTSalary"))]
    sim_metadata[, CPTOwn := g("CPTOP", NA_real_)]
  }
  cat(sprintf("Golf showdown done | %.1fs | mean pts %.1f\n", as.numeric(difftime(Sys.time(), t0, units = "secs")), mean(score)))
  cb(1.0, "Simulation complete!")
  list(sim_results = sim_results, sim_metadata = sim_metadata, has_dk = TRUE, has_fd = FALSE,
       no_cut = TRUE, cut_line = ev$cut_n, cut_after = ev$cut_after, cut_note = NULL, n_sims = n_sims,
       mode = mode, round = round, conditioned = !is.null(played), post = post,
       skill = data.table(Player = players_dt$Name, mu = mu[seq_len(n_p)]))
}
