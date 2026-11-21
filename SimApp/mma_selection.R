# =============================================================================
# mma_selection.R -- MMA classic pool: start from every lineup that was optimal in some sim, keep the top N by ceiling
# -----------------------------------------------------------------------------
# 20 Nov 2026 lineup-process study (8 classic fall cards, every legal lineup scored from 50k sims against the real flagship, high-stakes and
# ticket standings, duplicated lineups splitting the prize).
#   * The old pool kept the 3,000 MOST-OFTEN-OPTIMAL lineups (Top1Count). That is 25% of the real top-1% lineups and 7% of the low-duplicated
#     ones, and the most-often-optimal lineups are the chalkiest (the first 150 averaged 33 duplicates each). The 3,000 cap itself came from the
#     Batch Review backtest, whose contest scoring used mid-event standings points.
#   * A lineup's own P99 score across the sims (its ceiling) finds the top-1% and winning lineups far better than Win% / Top1% (7.9x vs random
#     in the top 3,000, against 4.5x for the sim mean).
#   * Pool size: with customer-style filters (Win/Top1-20% inside the pool, ownership) the sweet spot is 8,000-12,000 lineups.
# So: Phase 1 now keeps every optimal-in-some-sim lineup (dk_candidate_lineups), this function keeps the top dk_pool_size by P99, and Phase 2/3
# score that pool exactly as before -- the customer sees the same columns, metrics and filters, from a better starting pool.
# Gated by the sport config keys dk_candidate_lineups / dk_pool_size (MMA classic only); every other sport and MMA showdown are untouched.
# =============================================================================

#' Keep the n_keep lineups with the highest P99 (99th-percentile score across the sims) among lineup_data$unique_lineups.
#' lineup_data comes from find_optimal_lineups(mode = "combinatorial"); opt_data is prepare_optimization_data() output (SimID, Player, FantasyPoints).
mma_trim_pool_by_ceiling <- function(lineup_data, opt_data, n_keep, chunk = 250L, verbose = TRUE, use_matrixStats = TRUE) {
  ul <- lineup_data$unique_lineups
  if (!is.null(ul) && !data.table::is.data.table(ul)) ul <- data.table::as.data.table(ul)
  if (is.null(ul) || !nrow(ul) || is.null(n_keep) || is.na(n_keep) || nrow(ul) <= n_keep) return(lineup_data)
  pc <- grep("^Player[0-9]+$", names(ul), value = TRUE)
  # fallback if anything below cannot run: keep the n_keep most-often-optimal lineups, so the pool is still capped
  cap_by_count <- function() { if ("Top1Count" %in% names(ul)) lineup_data$unique_lineups <- ul[order(-Top1Count)][seq_len(n_keep)]; lineup_data }
  if (!length(pc)) return(cap_by_count())
  has_ms <- use_matrixStats && requireNamespace("matrixStats", quietly = TRUE)
  od <- data.table::as.data.table(opt_data)
  sc <- if ("FantasyPoints" %in% names(od)) "FantasyPoints" else "DKScore"
  W <- data.table::dcast(od[, .(SimID, Player, S = get(sc))], Player ~ SimID, value.var = "S", fun.aggregate = function(x) x[1])
  pl <- W$Player; S <- as.matrix(W[, -1, with = FALSE]); S[is.na(S)] <- 0
  idx <- matrix(match(as.matrix(ul[, ..pc]), pl), ncol = length(pc))
  if (anyNA(idx)) return(cap_by_count())
  nL <- nrow(ul); p99 <- numeric(nL)
  for (a in seq(1L, nL, by = chunk)) {
    r <- a:min(a + chunk - 1L, nL); Cs <- S[idx[r, 1], , drop = FALSE]
    for (j in 2:length(pc)) Cs <- Cs + S[idx[r, j], , drop = FALSE]
    p99[r] <- if (has_ms) as.numeric(matrixStats::rowQuantiles(Cs, probs = 0.99)) else apply(Cs, 1L, stats::quantile, probs = 0.99, names = FALSE)
    rm(Cs)
  }
  keep <- order(-p99)[seq_len(n_keep)]
  if (verbose) cat(sprintf("  MMA pool: kept the top %s of %s optimal-in-some-sim lineups by P99 ceiling\n", format(n_keep, big.mark = ","), format(nL, big.mark = ",")))
  lineup_data$unique_lineups <- ul[sort(keep)]
  lineup_data
}
