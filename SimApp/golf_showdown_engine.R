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

# mode "flex": six equal slots; "captain": CPT 1.5x points and 1.5x salary (sheet's CPTSalary wins)
run_golf_showdown_simulation <- function(input_data, n_sims = 10000, progress_callback = NULL,
                                         mode = c("flex", "captain"), fit_sims = 2000L, batch = 2500L) {
  mode <- match.arg(mode); t0 <- Sys.time()
  players_dt <- process_golf_players(input_data$player)
  n_p <- nrow(players_dt)
  if (!"SDSalary" %in% names(players_dt) || !any(!is.na(players_dt$SDSalary)))
    stop("Showdown sheet needs SDSalary (and SDID) on the Player tab")
  in_pool <- !is.na(players_dt$SDSalary)
  ev <- golf_v2_event(input_data$event, n_p); n <- ev$field
  cb <- if (is.null(progress_callback)) function(v, m) invisible() else progress_callback
  cat(sprintf("Golf showdown R1 (%s) | %d golfers simulated | %d in pool | %d sims\n", mode, n_p, sum(in_pool), n_sims))

  rn <- c(names(GOLF_V2$rungs), "Cut")
  mk <- matrix(NA_real_, n, length(rn), dimnames = list(NULL, rn))
  for (r in intersect(rn, names(players_dt))) mk[seq_len(n_p), r] <- as.numeric(players_dt[[r]])
  if (ev$cut_after == 0) mk[, "Cut"] <- NA
  early <- golf_v2_waves(players_dt)
  mu <- golf_v2_fit(mk, early, ev, S = fit_sims, cb = cb)

  keep <- which(in_pool); k <- length(keep)
  n_b <- ceiling(n_sims / batch); sc <- vector("list", n_b); rk <- sc
  for (b in seq_len(n_b)) {
    S  <- min(batch, n_sims - (b - 1L) * batch)
    sm <- golf_v2_sim(mu, early, golf_v2_draws(n, S), ev)
    st <- sm$strokes[, keep, 1]                         # round 1 strokes to par
    pts <- matrix(golf_sd_round_points(as.integer(st)), S, k)
    sc[[b]] <- t(pts)
    rk[[b]] <- t(row_rank(sm$strokes[, , 1], "min")[, keep, drop = FALSE])   # R1 rank among the whole field
    cb(0.40 + 0.5 * b / n_b, sprintf("Simulating round 1 (%d/%d)...", b, n_b))
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
       mode = mode, skill = data.table(Player = players_dt$Name, mu = mu[seq_len(n_p)]))
}
