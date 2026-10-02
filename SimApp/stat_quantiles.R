# =============================================================================
# stat_quantiles.R -- per-player / per-team stat quantiles for the Engine review
# -----------------------------------------------------------------------------
# OFF unless options(gts.stat_quantiles = TRUE). The review's re-sim worker
# (GTS/Common/contest_review_resim_worker.R) sets it; the app never does, so a
# customer's run is untouched.
#
# When it is on, an engine calls gts_stat_attach() after its sims are drawn and
# scored. That reads the per-sim stat draws the engine already holds, draws NO
# random numbers (so every later draw and every score is unchanged), and adds two
# tables to the engine's sport_visuals:
#   stat_quantiles   level, <id cols>, stat, n, mean, sd, p10, p25, p50, p75, p90, p_zero
#   stat_thresholds  level, <id cols>, stat, threshold, p_ge     (P(stat >= threshold))
# `level` is "player", "team" or "goalie". Nothing else in the result changes, and a
# failure here is a warning, never a failed sim. Full draws are never kept.
# =============================================================================

gts_stat_q_on <- function() isTRUE(getOption("gts.stat_quantiles"))

GTS_STAT_PROBS <- c(.1, .25, .5, .75, .9)

gts_q1 <- function(x) {
  x <- as.numeric(x); x <- x[!is.na(x)]
  if (!length(x)) return(NULL)
  q <- stats::quantile(x, GTS_STAT_PROBS, names = FALSE)
  list(n = length(x), mean = mean(x), sd = if (length(x) > 1) stats::sd(x) else NA_real_,
       p10 = q[1], p25 = q[2], p50 = q[3], p75 = q[4], p90 = q[5], p_zero = mean(x == 0))
}
gts_ge1 <- function(x, thr) {
  x <- as.numeric(x); x <- x[!is.na(x)]
  if (!length(x)) return(NULL)
  list(threshold = thr, p_ge = vapply(thr, function(t) mean(x >= t), 0))
}

#' Summaries of one table of per-sim draws.
#' @param D          per-sim draws (a data.table; read, never modified)
#' @param by         named character: output id column = column of D (c(Player = "player", Team = "team"))
#' @param stats      named list: stat name = a column name or an expression over D's columns
#' @param level      "player", "team", "goalie"
#' @param thresholds named list: stat name = numeric vector of thresholds
gts_stat_summ <- function(D, by, stats, level, thresholds = list()) {
  D <- data.table::as.data.table(D)
  ex <- lapply(stats, function(s) if (is.character(s)) as.name(s) else s)
  ok <- vapply(ex, function(e) all(all.vars(e) %in% names(D)), NA)
  ex <- ex[ok]
  q <- data.table::rbindlist(lapply(names(ex), function(s) {
    r <- eval(substitute(D[, gts_q1(EXPR), by = BY], list(EXPR = ex[[s]], BY = unname(by))))
    if (nrow(r)) r[, stat := s]
    r
  }), use.names = TRUE, fill = TRUE)
  ge <- data.table::rbindlist(lapply(intersect(names(thresholds), names(ex)), function(s) {
    thr <- as.numeric(thresholds[[s]])
    r <- eval(substitute(D[, gts_ge1(EXPR, THR), by = BY], list(EXPR = ex[[s]], THR = thr, BY = unname(by))))
    if (nrow(r)) r[, stat := s]
    r
  }), use.names = TRUE, fill = TRUE)
  lv_ <- level
  tidy <- function(d) {
    if (!nrow(d)) return(d)
    data.table::setnames(d, unname(by), names(by))
    d[, level := lv_]
    data.table::setcolorder(d, c("level", names(by), "stat"))
    d
  }
  list(q = tidy(q), ge = tidy(ge))
}

#' Add stat_quantiles / stat_thresholds to an engine's sport_visuals.
#' @param sv    the sport_visuals list (NULL is fine)
#' @param build a function returning a list of gts_stat_summ() results; run inside a tryCatch
gts_stat_attach <- function(sv, build) {
  parts <- tryCatch(build(), error = function(e) { warning("stat quantiles skipped: ", conditionMessage(e)); NULL })
  if (is.null(parts)) return(sv)
  if (is.null(sv)) sv <- list()
  sv$stat_quantiles  <- data.table::rbindlist(lapply(parts, `[[`, "q"),  use.names = TRUE, fill = TRUE)
  sv$stat_thresholds <- data.table::rbindlist(lapply(parts, `[[`, "ge"), use.names = TRUE, fill = TRUE)
  sv
}
