# =============================================================================
# sweep_dkmax.R — tune the DKMax eligibility ceiling
# -----------------------------------------------------------------------------
# Walk-forward like backtest.R, but scores ONLY .gts_dkmax(), so it skips the
# Monte Carlo and runs in seconds. Use it to pick .gts_DKMAX_* constants, then
# confirm with backtest.R.
#
#   Rscript sweep_dkmax.R [series] [from_season]
#
# DKMax is a high quantile of a driver's dominator-points distribution: high
# enough not to block what really happens, low enough that the gate still bites.
# That makes PINBALL LOSS at that quantile the proper scoring rule — it prices
# "too tight" against "too loose" at the right exchange rate instead of leaving
# the tradeoff to a hand-picked weight, and it rewards correct ranking.
#
# exceed / slack / cor are reported alongside as diagnostics only.
#
# Compares candidate_dkmax.R against the shipping .gts_dkmax. As of the last
# run the shipping formula wins at matched coverage.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(readxl); library(tidyr) })
source("gts_model.R")
source("candidate_dkmax.R")

args      <- commandArgs(trailingOnly = TRUE)
SERIES    <- if (length(args) >= 1) as.numeric(args[1]) else 1
FROM_SEAS <- if (length(args) >= 2) as.numeric(args[2]) else 2025
DOM_CUT   <- 10

results <- read_xlsx("NascarData.xlsx", sheet = "Results")
races   <- read_xlsx("NascarData.xlsx", sheet = "Races") %>%
  filter(series_id == SERIES, race_season >= FROM_SEAS) %>%
  filter(is.na(race_type_id) | race_type_id == 1) %>%
  arrange(race_season, race_id)
hist_all <- results %>% filter(series_id == SERIES)

# Pre-slice the walk-forward splits once, so the sweep only pays for .gts_dkmax.
cat("Preparing splits ...\n")
splits <- list()
for (i in seq_len(nrow(races))) {
  tgt <- races[i, ]
  act <- hist_all %>% filter(race_id == tgt$race_id)
  if (nrow(act) < 15 || all(is.na(act$DKSP))) next
  prior <- hist_all %>%
    filter(race_season < tgt$race_season |
             (race_season == tgt$race_season & race_id < tgt$race_id))
  if (nrow(prior) < 500) next
  splits[[length(splits) + 1]] <- list(
    tgt = tgt, prior = prior,
    ent = act %>% transmute(Name = Full_Name, Car = car_number,
                            Team = team_name, DK_Salary = NA_real_,
                            Start = start_ps),
    dksp = act$DKSP, nm = as.character(act$Full_Name),
    tmn = as.character(act$team_name))
}
cat(sprintf("%d races\n\n", length(splits)))

TAU <- 0.95   # the quantile DKMax is meant to represent

pinball <- function(y, f, tau) mean(pmax(tau * (y - f), (tau - 1) * (y - f)))

score_cfg <- function(q, mult, wbest, wslot) {
  ex <- sl <- co <- bd <- mc <- pb <- cv <- c()
  for (s in splits) {
    cap <- candidate_dkmax(s$prior, s$ent, s$tgt, s$nm, s$tmn,
                           q = q, mult = mult, w_best = wbest, w_slot = wslot)
    ok <- !is.na(s$dksp) & !is.na(cap)
    if (!any(ok)) next
    cp <- cap[ok]; dk <- s$dksp[ok]; dom <- dk >= DOM_CUT
    pb <- c(pb, pinball(dk, cp, TAU))
    cv <- c(cv, mean(dk <= cp))
    if (any(dom)) {
      ex <- c(ex, mean(dk[dom] > cp[dom]))
      sl <- c(sl, mean(pmax(cp[dom] - dk[dom], 0)))
    }
    bd <- c(bd, mean(cp < max(dk, na.rm = TRUE)))
    if (stats::sd(cp) > 0) co <- c(co, stats::cor(cp, dk))
    mc <- c(mc, mean(cp))
  }
  data.frame(q = q, mult = mult, wbest = wbest, wslot = wslot,
             pinball = mean(pb), coverage = mean(cv), exceed = mean(ex), slack = mean(sl),
             bind = mean(bd), cor = mean(co), mean_cap = mean(mc))
}

grid <- expand.grid(q = c(0.90, 0.93, 0.95),
                    mult = c(1.0, 1.15, 1.3, 1.45, 1.6, 1.8),
                    wbest = c(0, 0.15, 0.3),
                    wslot = c(0.5, 0.75, 1.0, 1.25))
out <- bind_rows(lapply(seq_len(nrow(grid)), function(i)
  score_cfg(grid$q[i], grid$mult[i], grid$wbest[i], grid$wslot[i])))

out <- out %>%
  mutate(pinball = round(pinball, 4), coverage = round(coverage, 3), exceed = round(exceed, 3),
         slack = round(slack, 1), bind = round(bind, 3),
         cor = round(cor, 3), mean_cap = round(mean_cap, 1)) %>%
  arrange(pinball)

cat(sprintf("=== best 15 by pinball loss @ tau=%.2f (lower better) ===\n", TAU))
print(as.data.frame(head(out, 15)), row.names = FALSE)

# -----------------------------------------------------------------------------
# The shipping formula (.gts_dkmax in gts_model.R), scored the same way so the
# comparison with the candidate above is like-for-like.
# -----------------------------------------------------------------------------
ex <- sl <- co <- bd <- mc <- pb <- cv <- c()
for (s in splits) {
  cap <- .gts_dkmax(s$prior, s$ent, s$tgt, s$nm, s$tmn)
  ok <- !is.na(s$dksp) & !is.na(cap); if (!any(ok)) next
  cp <- cap[ok]; dk <- s$dksp[ok]; dom <- dk >= DOM_CUT
  pb <- c(pb, pinball(dk, cp, TAU)); cv <- c(cv, mean(dk <= cp))
  if (any(dom)) { ex <- c(ex, mean(dk[dom] > cp[dom]))
                  sl <- c(sl, mean(pmax(cp[dom] - dk[dom], 0))) }
  bd <- c(bd, mean(cp < max(dk, na.rm = TRUE)))
  if (stats::sd(cp) > 0) co <- c(co, stats::cor(cp, dk))
  mc <- c(mc, mean(cp))
}
cat("
=== SHIPPING formula (.gts_dkmax), same scoring ===
")
cat(sprintf(" pinball %.4f | coverage %.3f | exceed %.3f | slack %.1f | bind %.3f | cor %.3f | mean_cap %.1f
",
            mean(pb), mean(cv), mean(ex), mean(sl), mean(bd), mean(co), mean(mc)))
