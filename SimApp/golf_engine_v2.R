# ============================================================================
# GOLF ENGINE v2 -- round-score simulation (Golden Ticket Sims)
# ============================================================================
# Design record: GTS/Golf/ENGINE.md (agreed 30 Sep 2026). Sourced by
# golf_engine.R at startup; replaces v1's finish-band draw as
# run_golf_simulation(). v1's reader and helpers are reused.
#
# For golfer i, round r, sim s, strokes to par:
#   Level + mu_i + u_si + C_sr + Lev_s + wave_sr(i) + sigma_i * eps_sir
#     mu    skill vs the field, fitted so the sim reproduces the market ladder
#     u     this golfer this week, N(0, 0.55), same all four rounds
#     C     round conditions shared by the field, N(0, 0.75)
#     Lev   event scoring shock, N(0, LevelSD): the Event tab's LevelSD (the
#           sheet's Level miss for its source), else 0.87
#     wave  R1-R2: early minus late ~ N(-0.24, 0.59), half to each side
#     eps   empirical right-skewed shape (golf/noise_q.rds), sd 2.64 + 0.055 mu
# Integer strokes by unbiased stochastic rounding. Cut = top CutN and ties
# after CutAfter rounds. The field is the sheet's golfers (Event FieldSize is
# ignored: no unnamed fillers). Finish = 72-hole rank with real ties, except a tie
# for 1st is a playoff: one winner, the rest share 2nd.
#
# Points: each simulated round takes a REAL round's DK and FD points drawn
# from the db at the same score to par (golf/round_pool.rds), so golfers tied
# at one finish still differ, as real ones do. Classic = rounds played +
# finish points + DK's all-four-under-70 bonus.
# ============================================================================

GOLF_V2_DIR <- file.path(GOLF_ENGINE_DIR, "golf")   # set by golf_engine.R
GOLF_V2 <- list(
  noise_q = readRDS(file.path(GOLF_V2_DIR, "noise_q.rds")),
  pool    = as.data.table(readRDS(file.path(GOLF_V2_DIR, "round_pool.rds"))),
  pars    = list(s_u = 0.55, sig0 = 2.64, sig_slope = 0.055, C_sd = 0.75, lev_sd = 0.87,
                 wave_mu = -0.24, wave_sd = 0.59),
  rungs   = c(W = 1, T5 = 5, T10 = 10, T20 = 20, T30 = 30, T40 = 40)
)
GOLF_V2$pool_idx <- split(seq_len(nrow(GOLF_V2$pool)), GOLF_V2$pool$stp)

golf_v2_noise <- function(n) {
  q <- GOLF_V2$noise_q
  q[pmin(length(q), as.integer(runif(n) * (length(q) - 1)) + 1L)] + rnorm(n, 0, 0.004)
}

# Event settings come from the sheet's Event tab only (5 Oct 2026: the app's
# cut boxes are gone). The field is the sheet's golfers (5 Oct 2026):
# FieldSize no longer adds unnamed fillers. LevelSD (optional) replaces the Lev
# shock sd for the event.
# Cut: CutN = top N and ties, CutAfter = 2 (36-hole), 3 (54-hole), 0 (no cut).
# Fallback when the Event tab or a cut cell is missing (older sheets): CutN 65
# with more than 100 golfers, else 50; CutAfter 2. `cut_note` says so, and the
# app shows it in the sim status.
golf_v2_cut_fallback <- function(n_pool) list(cut_n = if (n_pool > 100) 65L else 50L, cut_after = 2L)

golf_v2_event <- function(event_dt, n_pool) {
  ev <- if (!is.null(event_dt) && nrow(event_dt)) as.list(event_dt[1]) else list()
  num <- function(x, d) { v <- suppressWarnings(as.numeric(x)); if (length(v) && !is.na(v[1])) v[1] else d }
  fb  <- golf_v2_cut_fallback(n_pool)
  ca  <- num(ev$CutAfter, NA); cn <- num(ev$CutN, NA)
  miss <- c(if (is.na(ca)) "CutAfter", if (is.na(cn) && !identical(ca, 0)) "CutN")
  cut_after <- as.integer(if (is.na(ca)) fb$cut_after else ca)
  cut_n     <- as.integer(if (is.na(cn)) fb$cut_n else cn)
  note <- if (length(miss)) sprintf("Cut fallback: the sheet has no %s%s, so the sim used top %d & ties after R%d (%d golfers)",
                                    paste(miss, collapse = "/"), if (length(ev)) "" else " (no Event tab)",
                                    cut_n, cut_after, n_pool) else NULL
  list(par = num(ev$Par, 72), level = num(ev$Level, -0.5),
       lev_sd = num(ev$LevelSD, GOLF_V2$pars$lev_sd),
       cut_n = cut_n, cut_after = cut_after, cut_note = note,
       field = n_pool, from_sheet = length(ev) > 0)
}

# One-line description of the cut rule, for the app and logs.
golf_v2_cut_label <- function(ev)
  if (ev$cut_after == 0) "No cut" else sprintf("Cut: top %d & ties after round %d", ev$cut_n, ev$cut_after)

# R1/R2 wave from tee times: TRUE = early. Split at the biggest gap in the
# day's tee sheet (morning vs afternoon); NA when unknown -> no wave term.
golf_v2_waves <- function(dt) {
  mins <- function(x) {
    x <- trimws(as.character(x))
    pm <- grepl("pm", x, ignore.case = TRUE); am <- grepl("am", x, ignore.case = TRUE)
    p <- strsplit(gsub("[^0-9:]", "", x), ":")
    m <- vapply(p, function(v) if (length(v) >= 2) as.numeric(v[1]) * 60 + as.numeric(v[2]) else NA_real_, 0)
    h <- m %/% 60
    m[pm & h < 12 & !is.na(m)] <- m[pm & h < 12 & !is.na(m)] + 720
    m[am & h == 12 & !is.na(m)] <- m[am & h == 12 & !is.na(m)] - 720
    m
  }
  side <- function(t) {
    if (sum(!is.na(t)) < 10) return(rep(NA, length(t)))
    u <- sort(unique(t[!is.na(t)])); g <- diff(u); j <- which.max(g)
    cp <- if (length(g) && g[j] >= 60 && mean(t <= u[j], na.rm = TRUE) > .25 &&
              mean(t <= u[j], na.rm = TRUE) < .75) u[j] else median(t, na.rm = TRUE)
    t <= cp
  }
  r1 <- intersect(c("R1TeeTime", "Round 1 Tee Time"), names(dt))[1]
  r2 <- intersect(c("R2TeeTime", "Round 2 Tee Time"), names(dt))[1]
  if (is.na(r1) || is.na(r2)) return(matrix(NA, nrow(dt), 2))
  cbind(side(mins(dt[[r1]])), side(mins(dt[[r2]])))
}

# Draws that do not depend on mu (common random numbers across fit iterations)
golf_v2_draws <- function(n, S) {
  list(u = matrix(rnorm(S * n), S, n), eps = array(golf_v2_noise(S * n * 4), c(S, n, 4)),
       C = matrix(rnorm(S * 4), S, 4), lev = rnorm(S), wave = matrix(rnorm(S * 2), S, 2),
       frac = array(runif(S * n * 4), c(S, n, 4)))
}

row_rank <- function(m, ties) {
  out <- t(apply(m, 1, rank, ties.method = ties, na.last = "keep"))
  if (ncol(m) == 1) out <- t(out)
  out
}

# One batch of events. Returns strokes (S x n x 4), made (S x n), pos (72-hole
# finish, min rank; NA for missers), pos_hi (max rank, for dead-heat rungs),
# mc_rank (missers' rank on their cut total).
golf_v2_sim <- function(mu, early, dr, ev, pars = GOLF_V2$pars) {
  S <- nrow(dr$u); n <- length(mu)
  sig  <- matrix(pmax(pars$sig0 + pars$sig_slope * mu, 1.5), S, n, byrow = TRUE)
  muM  <- matrix(mu, S, n, byrow = TRUE)
  strokes <- array(0L, c(S, n, 4)); cum <- matrix(0, S, n); alive <- matrix(TRUE, S, n)
  cut_tot <- NULL
  for (r in 1:4) {
    x <- ev$level + muM + pars$s_u * dr$u + sig * dr$eps[, , r] + pars$C_sd * dr$C[, r] +
         (ev$lev_sd %||% pars$lev_sd) * dr$lev
    if (r <= 2) {
      e <- early[, r]
      if (any(!is.na(e))) {
        d <- pars$wave_mu + pars$wave_sd * dr$wave[, r]            # early minus late, per sim
        sgn <- ifelse(is.na(e), 0, ifelse(e, 0.5, -0.5))
        x <- x + outer(d, sgn)
      }
    }
    xi <- floor(x) + (dr$frac[, , r] < (x - floor(x)))
    strokes[, , r] <- xi
    cum <- cum + xi * alive
    if (ev$cut_after > 0 && r == ev$cut_after) {
      cut_tot <- cum
      alive <- row_rank(cum, "min") <= ev$cut_n
    }
  }
  tot <- cum; tot[!alive] <- NA
  pos <- row_rank(tot, "min"); pos_hi <- row_rank(tot, "max")
  mc_rank <- NULL
  if (!is.null(cut_tot)) { ct <- cut_tot; ct[alive] <- NA; mc_rank <- row_rank(ct, "random") }
  list(strokes = strokes, made = alive, pos = pos, pos_hi = pos_hi, mc_rank = mc_rank)
}

# Dead-heat share of each rung (how books settle top-N) + cut made
golf_v2_rung_probs <- function(sim, K = GOLF_V2$rungs) {
  t <- sim$pos_hi - sim$pos + 1
  out <- sapply(K, function(k) colMeans(ifelse(is.na(sim$pos), 0, pmin(pmax(k - sim$pos + 1, 0), t) / t)))
  if (is.null(dim(out))) out <- matrix(out, nrow = 1, dimnames = list(NULL, names(K)))
  cbind(out, Cut = colMeans(sim$made))
}

# Fit mu so the sim reproduces the market ladder mk (n x rungs; NA / 0 / 1 =
# not used). Probit-gap steps, damped, centred on the field every iteration.
# Golfers with no usable rung (never priced) sit at the field bottom.
golf_v2_fit <- function(mk, early, ev, S = 2000L, iters = 25L, step = 0.8, cb = NULL) {
  n <- nrow(mk); rungs <- colnames(mk)
  slope <- ifelse(rungs == "Cut", 0.5, 0.7)
  use <- !is.na(mk) & mk > 0 & mk < 1
  priced <- rowSums(use) > 0
  dr <- golf_v2_draws(n, S)
  mu <- rep(0, n)
  bottom <- function(mu) quantile(mu[priced], .95) + 0.3
  for (it in seq_len(iters)) {
    ps <- golf_v2_rung_probs(golf_v2_sim(mu, early, dr, ev))[, rungs, drop = FALSE]
    ps <- (ps * S + 0.5) / (S + 1)
    g  <- (qnorm(ps) - qnorm(pmin(pmax(mk, 1e-6), 1 - 1e-6))) / matrix(slope, n, length(rungs), byrow = TRUE)
    g[!use] <- 0
    mu <- mu + step * rowSums(g * use) / pmax(rowSums(use), 1)
    mu[!priced] <- bottom(mu)
    mu <- mu - mean(mu)
    if (!is.null(cb)) cb(0.05 + 0.35 * it / iters, sprintf("Fitting skill to the odds ladder (%d/%d)...", it, iters))
  }
  mu
}

# Real (DK, FD) round points at each simulated score to par
golf_v2_round_points <- function(stp) {
  P <- GOLF_V2$pool; lo <- min(P$stp); hi <- max(P$stp)
  s <- pmin(pmax(stp, lo), hi); dk <- numeric(length(stp)); fd <- dk
  grp <- split(seq_along(s), s)
  for (v in names(grp)) {
    i <- grp[[v]]; idx <- GOLF_V2$pool_idx[[v]]
    j <- idx[sample.int(length(idx), length(i), replace = TRUE)]
    dk[i] <- P$dk2[j] / 2; fd[i] <- P$fd10[j] / 10
  }
  ext <- s - stp                               # > 0 below the pool's best round
  list(dk = dk + 2.5 * ext, fd = fd + 3.1 * ext)
}

golf_v2_dk_finish <- function(p) {
  tab <- c(30, 20, 18, 16, 14, 12, 10, 9, 8, 7, rep(6, 5), rep(5, 5), rep(4, 5), rep(3, 5), rep(2, 10), rep(1, 10))
  out <- numeric(length(p)); ok <- !is.na(p) & p <= length(tab); out[ok] <- tab[p[ok]]; out
}
golf_v2_fd_finish <- function(p) {
  tab <- c(30, 20, 18, 16, 14, 12, 10, 8, 7, 6, rep(5, 5), rep(4, 5), rep(3, 5), rep(2, 5), rep(1, 10))
  out <- numeric(length(p)); ok <- !is.na(p) & p <= length(tab); out[ok] <- tab[p[ok]]; out
}

run_golf_simulation <- function(input_data, n_sims = 10000,
                                progress_callback = NULL, keep_rounds = FALSE,
                                fit_sims = 2000L, batch = 2500L) {
  t0 <- Sys.time()
  players_dt <- process_golf_players(input_data$player)
  has_dk <- "DKSalary" %in% names(players_dt) && any(!is.na(players_dt$DKSalary))
  has_fd <- "FDSalary" %in% names(players_dt) && any(!is.na(players_dt$FDSalary))
  n_p <- nrow(players_dt)
  ev  <- golf_v2_event(input_data$event, n_p)
  n   <- ev$field                                  # the sheet's golfers
  cb  <- if (is.null(progress_callback)) function(v, m) invisible() else progress_callback

  cat(sprintf("Golf sim v2 | %d golfers | %d sims | level %+.2f (sd %.2f) par %g | cut %s\n",
              n_p, n_sims, ev$level, ev$lev_sd, ev$par,
              if (ev$cut_after == 0) "none" else sprintf("top %d & ties after R%d", ev$cut_n, ev$cut_after)))
  if (!ev$from_sheet) cat("  No Event tab: level/par defaults\n")
  if (!is.null(ev$cut_note)) cat("  ", ev$cut_note, "\n", sep = "")

  rn <- c(names(GOLF_V2$rungs), "Cut")
  mk <- matrix(NA_real_, n, length(rn), dimnames = list(NULL, rn))
  for (r in intersect(rn, names(players_dt))) mk[seq_len(n_p), r] <- as.numeric(players_dt[[r]])
  if (ev$cut_after == 0) mk[, "Cut"] <- NA
  early <- golf_v2_waves(players_dt)

  mu <- golf_v2_fit(mk, early, ev, S = fit_sims, cb = cb)

  n_b  <- ceiling(n_sims / batch)
  dk_l <- vector("list", n_b); fd_l <- dk_l; pos_l <- dk_l; rr_l <- dk_l; mc_l <- dk_l
  made_sum <- numeric(n_p)
  for (b in seq_len(n_b)) {
    S  <- min(batch, n_sims - (b - 1L) * batch)
    sm <- golf_v2_sim(mu, early, golf_v2_draws(n, S), ev)
    keep <- seq_len(n_p)
    st <- sm$strokes[, keep, , drop = FALSE]; made <- sm$made[, keep, drop = FALSE]
    made_sum <- made_sum + colSums(made)

    # Finish: a tie for 1st is a playoff -- one random winner, the rest 2nd.
    pos <- sm$pos
    for (s in which(rowSums(pos == 1, na.rm = TRUE) > 1)) {
      w <- which(pos[s, ] == 1); pos[s, w] <- 2L; pos[s, w[sample.int(length(w), 1)]] <- 1L
    }
    pos <- pos[, keep, drop = FALSE]
    # FinishPosition (v1 convention): missers = cut makers + their cut-total rank
    fp <- pos
    if (!is.null(sm$mc_rank)) {
      n_made <- rowSums(sm$made)
      mc <- sm$mc_rank[, keep, drop = FALSE] + n_made
      fp[!made] <- mc[!made]
    }

    dk <- matrix(0, S, n_p); fd <- dk; sub70 <- made; rounds <- list()
    for (r in 1:4) {
      played <- if (ev$cut_after > 0 && r > ev$cut_after) made else matrix(TRUE, S, n_p)
      rp <- golf_v2_round_points(as.integer(st[, , r]))
      dkr <- matrix(rp$dk, S, n_p); fdr <- matrix(rp$fd, S, n_p)
      dk <- dk + dkr * played; fd <- fd + fdr * played
      sub70 <- sub70 & (st[, , r] + ev$par < 70)
      if (keep_rounds) rounds[[r]] <- data.table(
        SimID = rep((b - 1L) * batch + seq_len(S), n_p), Player = rep(players_dt$Name, each = S),
        Round = r, Strokes = as.integer(st[, , r] + ev$par), DKRound = as.vector(dkr),
        FDRound = as.vector(fdr))[as.vector(played)]
    }
    if (ev$cut_after == 0) sub70 <- sub70 & TRUE
    dk <- dk + matrix(golf_v2_dk_finish(as.integer(pos)), S, n_p) + 5 * sub70
    fd <- fd + matrix(golf_v2_fd_finish(as.integer(pos)), S, n_p)

    dk_l[[b]] <- t(dk); fd_l[[b]] <- t(fd); pos_l[[b]] <- t(fp); mc_l[[b]] <- t(made)
    if (keep_rounds) rr_l[[b]] <- rbindlist(rounds)
    cb(0.40 + 0.45 * b / n_b, sprintf("Simulating rounds (%d/%d)...", b, n_b))
  }
  dk_mat <- do.call(cbind, dk_l); fd_mat <- do.call(cbind, fd_l); pos_mat <- do.call(cbind, pos_l)
  made_mat <- do.call(cbind, mc_l)
  rm(dk_l, fd_l, pos_l, mc_l)

  cb(0.88, "Building output tables...")
  sim_results <- data.table(
    SimID          = rep(seq_len(n_sims), each = n_p),
    Player         = rep(players_dt$Name, times = n_sims),
    Pool           = rep(players_dt$Pool, times = n_sims),
    FinishPosition = as.integer(as.vector(pos_mat)),
    MadeCut        = as.integer(as.vector(made_mat)),   # 1 = played the weekend (all 1 with no cut)
    DKScore        = if (has_dk) as.vector(dk_mat) else 0,
    FDScore        = if (has_fd) as.vector(fd_mat) else 0
  )
  sim_metadata <- data.table(
    Player       = players_dt$Name,
    Pool         = players_dt$Pool,
    TeeTimeGroup = players_dt$TeeTimeGroup %||% "Unknown",
    CutProb      = if (ev$cut_after == 0) 1 else made_sum / n_sims   # the sim's own world
  )
  if (has_dk) {
    sim_metadata[, DKSalary := players_dt$DKSalary]
    sim_metadata[, DKOwn    := if ("DKOP" %in% names(players_dt)) players_dt$DKOP else 0]
    sim_metadata[, DKID     := if ("DKID" %in% names(players_dt)) players_dt$DKID else NA_character_]
  }
  if (has_fd) {
    sim_metadata[, FDSalary := players_dt$FDSalary]
    sim_metadata[, FDOwn    := if ("FDOP" %in% names(players_dt)) players_dt$FDOP else 0]
    sim_metadata[, FDID     := if ("FDID" %in% names(players_dt)) players_dt$FDID else NA_character_]
  }

  cat(sprintf("Golf sim v2 done | %.1fs | %s rows\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs")),
              format(nrow(sim_results), big.mark = ",")))
  cb(1.0, "Simulation complete!")
  list(sim_results = sim_results, sim_metadata = sim_metadata,
       has_dk = has_dk, has_fd = has_fd,
       no_cut = ev$cut_after == 0, cut_line = ev$cut_n, cut_after = ev$cut_after,
       cut_note = ev$cut_note, n_sims = n_sims,
       skill = data.table(Player = players_dt$Name, mu = mu[seq_len(n_p)]),
       round_results = if (keep_rounds) rbindlist(rr_l) else NULL)
}
