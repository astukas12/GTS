# =============================================================================
# candidate_dkmax.R — an alternative DKMax formula, kept for comparison.
# -----------------------------------------------------------------------------
# Rates drivers on their SHARE of the dominator points a race pays out rather
# than on raw DKSP, rescales to the target race's pot, and applies a start-slot
# factor. The motivation was real: the pot swings ~11x across the schedule
# (a 500-lap short track pays ~217 DK dominator points, a road course ~56).
#
# Measured against the shipping formula on pinball loss at tau=0.95, it did NOT
# win at matched coverage — the shipping formula's slot term is already
# track-specific, so it is not actually scale-blind in practice. Kept so the
# result is reproducible rather than folklore. Run: Rscript sweep_dkmax.R
# =============================================================================
candidate_dkmax <- function(hist, entry, tgt, nm, tmn, q, mult, w_best, w_slot) {
  K_DKDRV <- 10; K_DKTEAM <- 30
  hs <- hist[!is.na(hist$DKSP), , drop = FALSE]
  if (!nrow(hs)) return(rep(0, nrow(entry)))
  pool <- hs %>% group_by(race_id) %>%
    summarise(tot = sum(DKSP, na.rm = TRUE), .groups = "drop")
  hs <- hs %>% left_join(pool, by = "race_id") %>%
    filter(is.finite(tot), tot > 0) %>%
    mutate(share = DKSP / tot)
  if (!nrow(hs)) return(rep(0, nrow(entry)))

  # Size of the pot at the target race: same track if we have it, else track
  # type, else the series as a whole.
  pool_at <- function(rows) {
    if (!nrow(rows)) return(NA_real_)
    v <- unique(rows[, c("race_id", "tot")])$tot
    if (!length(v)) NA_real_ else stats::median(v, na.rm = TRUE)
  }
  tot_hat <- pool_at(hs[!is.na(hs$track_name) & hs$track_name == tgt$track_name, , drop = FALSE])
  if (!is.finite(tot_hat))
    tot_hat <- pool_at(hs[!is.na(hs$track_type) & hs$track_type == tgt$track_type, , drop = FALSE])
  if (!is.finite(tot_hat)) tot_hat <- pool_at(hs)
  if (!is.finite(tot_hat)) tot_hat <- 150

  ceil_stat <- function(v) {
    v <- v[is.finite(v)]
    if (!length(v)) return(NA_real_)
    (1 - w_best) * unname(stats::quantile(v, q, na.rm = TRUE)) + w_best * max(v)
  }

  field_share <- ceil_stat(hs$share)
  tq <- hs %>% group_by(team_name) %>%
    summarise(s = ceil_stat(share), n = n(), .groups = "drop")
  tq$est <- mapply(.gts_shrink, tq$s, tq$n,
                   MoreArgs = list(prior = field_share, K = K_DKTEAM))
  team_q <- setNames(tq$est, tq$team_name)

  # Each driver's prior is the team they most recently raced for.
  last_team <- hs %>% group_by(Full_Name) %>%
    slice_max(race_id, n = 1, with_ties = FALSE) %>% ungroup()
  drv_team <- setNames(last_team$team_name, last_team$Full_Name)

  dq <- hs %>% group_by(Full_Name) %>%
    summarise(s = ceil_stat(share), n = n(), .groups = "drop")
  dq$prior <- vapply(dq$Full_Name, function(x) {
    t <- drv_team[[x]]
    if (!is.null(t) && !is.na(t) && t %in% names(team_q)) team_q[[t]] else field_share
  }, numeric(1))
  dq$est <- mapply(.gts_shrink, dq$s, dq$n, dq$prior,
                   MoreArgs = list(K = K_DKDRV))
  drv_q <- setNames(dq$est, dq$Full_Name)

  share_est <- vapply(seq_len(nrow(entry)), function(i) {
    if (nm[i]  %in% names(drv_q))  return(drv_q[[nm[i]]])
    if (tmn[i] %in% names(team_q)) return(team_q[[tmn[i]]])
    field_share
  }, numeric(1))

  # Start-slot factor, as a ratio against the field-wide ceiling share so it
  # scales capability rather than replacing it. w_slot = 0 ignores position,
  # 1 applies it in full.
  if (w_slot > 0) {
    brk  <- c(0, 3, 6, 10, 15, 20, 25, Inf)
    lev  <- levels(cut(numeric(0), brk))
    hss  <- hs[!is.na(hs$start_ps), , drop = FALSE]
    sm   <- setNames(rep(NA_real_, length(lev)), lev)
    if (nrow(hss) >= 40) {
      v <- tapply(hss$share, cut(hss$start_ps, brk), ceil_stat)
      sm[names(v)] <- as.numeric(v)
      # carry the nearest known slot into any empty bucket
      for (i in seq_along(sm)) if (is.na(sm[i]) && i > 1) sm[i] <- sm[i - 1]
      for (i in rev(seq_along(sm))) if (is.na(sm[i]) && i < length(sm)) sm[i] <- sm[i + 1]
    }
    if (all(is.finite(sm)) && field_share > 0) {
      st <- suppressWarnings(as.numeric(entry$Start))
      st[is.na(st) | st <= 0] <- nrow(entry)
      ratio <- as.numeric(sm[as.character(cut(st, brk))]) / field_share
      ratio[!is.finite(ratio) | ratio <= 0] <- 1
      share_est <- share_est * ratio ^ w_slot
    }
  }

  # Headroom above the estimated ceiling — the gate should sit above a driver's
  # good run, or it blocks outcomes that really happen.
  out <- round(share_est * tot_hat * mult)
  out[!is.finite(out)] <- 0
  pmax(out, 0)
}
