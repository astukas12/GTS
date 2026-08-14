# =============================================================================
# backtest.R — accuracy harness for the input-sheet auto-fill model
# -----------------------------------------------------------------------------
# Walk-forward backtest: for each held-out race, refit gts_auto_inputs() using
# ONLY races that happened before it, then score the predicted W..T30 lines
# against what actually happened.
#
#   Rscript backtest.R                    # Cup, 2024-2025, all tracks
#   Rscript backtest.R 2 2025             # series, from-season
#   Rscript backtest.R 1 2024 road_course # ... and one track type
#
# Metrics, per threshold (W, T3, ... T30):
#   brier  — mean (p - outcome)^2. Lower is better. The headline number.
#   logloss— penalizes confident misses harder. Lower is better.
#   skill  — 1 - brier/brier_base, where the baseline predicts the field's
#            base rate (k/F) for everyone. >0 means the model beats "no idea".
#            This is the number to watch: it is scale-free across thresholds.
#   calib  — mean(p) vs mean(outcome). Equal means well calibrated overall.
#
# DKMax is scored separately: exceed = share of drivers whose actual DKSP came
# in ABOVE the cap we handed the sim (the cap was too low), and headroom = mean
# (cap - actual) among the rest (how much slack we left on the table).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readxl); library(tidyr)
})

source("gts_model.R")

# Start-position-only reference. Buckets prior races by start slot and reads
# off the empirical P(finish <= k), shrunk toward the field base rate so thin
# buckets do not spike. Deliberately dumb — it is the bar to clear.
ref_start_model <- function(prior, starts, thresh, F) {
  brk <- c(0, 1, 2, 3, 5, 8, 12, 16, 20, 25, 30, Inf)
  pr  <- prior[!is.na(prior$start_ps) & !is.na(prior$ps), , drop = FALSE]
  pb  <- cut(pr$start_ps, brk)
  sb  <- cut(ifelse(is.na(starts) | starts <= 0, F, starts), brk)
  out <- matrix(NA_real_, length(starts), length(thresh))
  for (j in seq_along(thresh)) {
    k <- thresh[j]
    base <- k / F
    tab <- tapply(pr$ps <= k, pb, function(z) c(mean(z), length(z)))
    est <- vapply(levels(pb), function(l) {
      v <- tab[[l]]
      if (is.null(v) || is.na(v[1])) return(base)
      w <- v[2] / (v[2] + 50)                      # credibility shrink
      w * v[1] + (1 - w) * base
    }, numeric(1))
    out[, j] <- est[as.character(sb)]
  }
  out[is.na(out)] <- 0.5
  out
}

args      <- commandArgs(trailingOnly = TRUE)
SERIES    <- if (length(args) >= 1) as.numeric(args[1]) else 1
FROM_SEAS <- if (length(args) >= 2) as.numeric(args[2]) else 2024
TRK_TYPE  <- if (length(args) >= 3) args[3] else NA_character_
DOM_CUT   <- 10   # DKSP at/above this counts as a real dominator run

cat("Loading NascarData.xlsx ...\n")
results <- read_xlsx("NascarData.xlsx", sheet = "Results")
races   <- read_xlsx("NascarData.xlsx", sheet = "Races")

# Points races only — the Clash/Duels/All-Star have odd fields and formats and
# are not what the sim is ever pointed at.
races <- races %>%
  filter(series_id == SERIES, race_season >= FROM_SEAS) %>%
  filter(is.na(race_type_id) | race_type_id == 1)
if (!is.na(TRK_TYPE)) races <- races %>% filter(track_type == TRK_TYPE)
races <- races %>% arrange(race_season, race_id)

hist_all <- results %>% filter(series_id == SERIES)

cat(sprintf("Backtesting %d races (series %s, %s+%s)\n\n", nrow(races), SERIES,
            FROM_SEAS, if (is.na(TRK_TYPE)) "" else paste0(", ", TRK_TYPE)))

rows <- list(); dkrows <- list()

for (i in seq_len(nrow(races))) {
  tgt <- races[i, ]

  # Actual result of the held-out race, used both as the "entry list" we would
  # have had (name/car/team/start are known pre-race) and as the truth.
  act <- hist_all %>% filter(race_id == tgt$race_id)
  if (nrow(act) < 15) next

  # Only races strictly before this one. Ordering by (season, race_id) matches
  # the season calendar, so this is a true walk-forward split.
  prior <- hist_all %>%
    filter(race_season < tgt$race_season |
             (race_season == tgt$race_season & race_id < tgt$race_id))
  if (nrow(prior) < 500) next

  # No stored salary history, so the salary prior is left empty here. That
  # makes this a slightly pessimistic read for thin-history drivers.
  ent <- act %>%
    transmute(Name = Full_Name, Car = car_number, Team = team_name,
              DK_Salary = NA_real_, Start = start_ps)

  pred <- try(gts_auto_inputs(prior, ent, tgt), silent = TRUE)
  if (inherits(pred, "try-error") || is.null(pred)) next

  # Reference model: P(finish <= k | start bucket), read straight off prior
  # races. No driver identity at all. If the full model cannot clear this, the
  # hierarchy/credibility machinery is not paying for itself.
  ref <- ref_start_model(prior, act$start_ps, .gts_THRESH, nrow(act))

  F <- nrow(act)
  truth <- act$ps
  for (k_i in seq_along(.gts_THRESH)) {
    k <- .gts_THRESH[k_i]; lab <- .gts_LABELS[k_i]
    if (k >= F) next
    y <- as.numeric(truth <= k)
    p <- pmin(pmax(pred[[lab]], 1e-6), 1 - 1e-6)
    pr <- pmin(pmax(ref[, k_i], 1e-6), 1 - 1e-6)
    rows[[length(rows) + 1]] <- data.frame(
      race_id = tgt$race_id, season = tgt$race_season,
      track = tgt$track_name, track_type = tgt$track_type,
      lab = lab, k = k, F = F,
      brier = mean((p - y)^2),
      logloss = -mean(y * log(p) + (1 - y) * log(1 - p)),
      brier_base = mean((k / F - y)^2),
      brier_ref  = mean((pr - y)^2),
      mean_p = mean(p), mean_y = mean(y)
    )
  }

  # DKMax is an eligibility gate in the sim, not a prediction: a driver can
  # only be handed a dominator profile worth P if DKMax >= P. So it is scored
  # against the drivers who actually put up dominator points — averaging over
  # the whole field just measures how many backmarkers scored zero.
  if (!is.null(pred$DKMax) && any(!is.na(act$DKSP))) {
    ok  <- !is.na(act$DKSP) & !is.na(pred$DKMax)
    cap <- pred$DKMax[ok]; dksp <- act$DKSP[ok]
    dom <- dksp >= DOM_CUT              # a real dominator run
    top_profile <- max(dksp, na.rm = TRUE)
    if (any(ok)) dkrows[[length(dkrows) + 1]] <- data.frame(
      race_id = tgt$race_id, track_type = tgt$track_type,
      n_dom      = sum(dom),
      # cap was too low — we would have blocked an outcome that really happened
      exceed_dom = if (any(dom)) mean(dksp[dom] > cap[dom]) else NA_real_,
      # unused ceiling among real dominators
      slack_dom  = if (any(dom)) mean(pmax(cap[dom] - dksp[dom], 0)) else NA_real_,
      # does the gate discriminate at all? share of field locked out of the
      # race's best profile. Near 0 means DKMax is doing nothing in the sim.
      bind_rate  = mean(cap < top_profile),
      cor_cap    = if (stats::sd(cap) > 0) stats::cor(cap, dksp) else NA_real_,
      mean_cap   = mean(cap)
    )
  }
  cat(sprintf("  [%2d/%2d] %s %s\n", i, nrow(races), tgt$race_season, tgt$track_name))
}

sc <- bind_rows(rows)
if (nrow(sc) == 0) stop("No races scored.")

cat(sprintf("\n=== %d races scored ===\n\n", n_distinct(sc$race_id)))

summ <- sc %>%
  group_by(lab) %>%
  summarise(
    races  = n(),
    brier  = round(mean(brier), 5),
    logloss = round(mean(logloss), 4),
    skill  = round(1 - mean(brier) / mean(brier_base), 4),
    ref    = round(1 - mean(brier_ref) / mean(brier_base), 4),
    edge   = round(1 - mean(brier) / mean(brier_ref), 4),
    pred   = round(mean(mean_p), 4),
    actual = round(mean(mean_y), 4),
    .groups = "drop"
  ) %>%
  mutate(lab = factor(lab, levels = .gts_LABELS)) %>%
  arrange(lab)
print(as.data.frame(summ), row.names = FALSE)

cat(sprintf("\nOverall skill vs base rate: %.4f | start-only ref: %.4f | model edge over ref: %.4f\n",
            1 - sum(sc$brier) / sum(sc$brier_base),
            1 - sum(sc$brier_ref) / sum(sc$brier_base),
            1 - sum(sc$brier) / sum(sc$brier_ref)))

cat("\n--- skill by track type (model vs start-only ref) ---\n")
print(as.data.frame(sc %>% group_by(track_type) %>%
  summarise(races = n_distinct(race_id),
            skill = round(1 - sum(brier) / sum(brier_base), 4),
            ref   = round(1 - sum(brier_ref) / sum(brier_base), 4),
            edge  = round(1 - sum(brier) / sum(brier_ref), 4),
            .groups = "drop") %>% arrange(edge)), row.names = FALSE)

if (length(dkrows)) {
  dk <- bind_rows(dkrows)
  cat(sprintf("\n--- DKMax (scored on drivers with DKSP >= %g) ---\n", DOM_CUT))
  cat(sprintf("  dominators/race : %.1f\n", mean(dk$n_dom)))
  cat(sprintf("  cap exceeded    : %.1f%%   (too low — blocked a real outcome)\n",
              100 * mean(dk$exceed_dom, na.rm = TRUE)))
  cat(sprintf("  unused ceiling  : %.1f pts (slack above what they actually did)\n",
              mean(dk$slack_dom, na.rm = TRUE)))
  cat(sprintf("  gate bind rate  : %.1f%%   (share of field locked out of the top profile)\n",
              100 * mean(dk$bind_rate)))
  cat(sprintf("  cor(cap, DKSP)  : %.3f   (does the cap rank drivers correctly?)\n",
              mean(dk$cor_cap, na.rm = TRUE)))
  cat(sprintf("  mean cap        : %.1f\n", mean(dk$mean_cap)))
}
