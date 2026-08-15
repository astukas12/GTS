# =============================================================================
# AUTO-FILL: finish probabilities (W..T30) + DKMax for the input sheet
# -----------------------------------------------------------------------------
# Hierarchical speed model -> Monte Carlo field orderings -> probabilities.
#
#   speed composite : per-race percentile of SpdRk and ARP (clean races only)
#   hierarchy       : driver -> car -> team -> field, credibility shrunk
#   salary prior    : thin-history drivers lean on DK salary rank
#   recency         : exponential decay, 2-season half life
#   start position  : partial coefficient after speed, fitted per track and
#                     shrunk toward track type; applied only when the entry
#                     list actually has starting positions
#   DNF             : crash (driver/team/track) + mechanical (mostly track)
#   DKMax           : start-slot dominator ceiling, capped by driver/team best
#
# Simulating whole field orderings means W sums to 1, T5 to 5, etc. by
# construction, and each driver's line is monotonic without post-processing.
# =============================================================================

.gts_CRASH   <- c("Accident", "DVP", "Damage")
.gts_THRESH  <- c(1, 3, 5, 10, 15, 20, 25, 30)
.gts_LABELS  <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
.gts_K_DRIVER <- 8; .gts_K_CAR <- 6; .gts_K_TEAM <- 10
.gts_K_CRASH <- 12; .gts_K_MECH <- 40; .gts_K_SAL <- 6
.gts_HALFLIFE <- 2; .gts_K_BETA <- 400
# Headroom the DKMax gate leaves above the 90th-percentile DKSP for a start slot.
.gts_DKMAX_MULT <- 2.2

.gts_shrink <- function(m, n, prior, K) {
  if (is.na(m)) return(prior)
  w <- n / (n + K); w * m + (1 - w) * prior
}

# Per-race percentile composite of SpdRk + ARP, clean (Running) rows only.
.gts_speed_pct <- function(h) {
  keep <- h$is_run & (!is.na(h$SpdRk) | !is.na(h$ARP))
  d <- h[keep, , drop = FALSE]
  if (nrow(d) == 0) return(d)
  pct <- function(x) {
    if (all(is.na(x))) return(rep(NA_real_, length(x)))
    rank(x, na.last = "keep") / sum(!is.na(x))
  }
  d <- d %>%
    group_by(race_id) %>%
    mutate(sp_p = pct(SpdRk), arp_p = pct(ARP)) %>%
    ungroup() %>%
    mutate(spd = rowMeans(cbind(sp_p, arp_p), na.rm = TRUE))
  d$spd[is.nan(d$spd)] <- NA_real_
  d[!is.na(d$spd), , drop = FALSE]
}

.gts_z <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# Fit the latent finish model directly:  fin_pct ~ spd_hat + start_z
#
# Everything the Monte Carlo needs comes out of this one fit on the same scale:
# the two slopes AND the residual sd. The previous version fitted the start
# coefficient against z-scored finish and then rescaled it by the residual sd,
# a different scale than the speed term it was added to, which silently shrank
# the start effect by roughly an order of magnitude.
#
# spd_hat is deliberately the LEAVE-ONE-RACE-OUT speed estimate, not the speed
# the driver actually showed in that race. Fitting against realized speed makes
# b_st the effect of start "after controlling for race pace" — information that
# does not exist on race morning — and the slopes then fail to transfer. Fitting
# against a forecast built the same way the prediction is built keeps them honest.
#
# start is z-scored within race so field size does not change its meaning.
.gts_fit_latent <- function(d, min_n = 60) {
  if (is.null(d) || nrow(d) < min_n) return(NULL)
  dd <- d %>% group_by(race_id) %>%
    mutate(sz = .gts_z(start_ps)) %>% ungroup()
  dd <- dd[stats::complete.cases(dd[, c("fin_p", "spd_hat", "sz")]), , drop = FALSE]
  if (nrow(dd) < min_n) return(NULL)
  fit <- try(stats::lm(fin_p ~ spd_hat + sz, data = dd), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  co <- stats::coef(fit)
  out <- list(a = unname(co[[1]]), b_spd = unname(co[["spd_hat"]]),
              b_st = unname(co[["sz"]]), sd = stats::sd(stats::residuals(fit)),
              n = nrow(dd))
  if (!all(is.finite(unlist(out)))) return(NULL)
  out
}

# Credibility-blend a track-level latent fit toward a broader (type/global) one.
.gts_blend_latent <- function(spec, prior, K) {
  if (is.null(spec)) return(prior)
  if (is.null(prior)) return(spec)
  w <- spec$n / (spec$n + K)
  list(a     = w * spec$a     + (1 - w) * prior$a,
       b_spd = w * spec$b_spd + (1 - w) * prior$b_spd,
       b_st  = w * spec$b_st  + (1 - w) * prior$b_st,
       sd    = w * spec$sd    + (1 - w) * prior$sd,
       n     = spec$n)
}


# =============================================================================
# DKMax — the dominator eligibility ceiling handed to the sim.
# -----------------------------------------------------------------------------
# In the sim a driver can only be assigned a dominator profile worth P if
# DKMax >= P, so this wants a driver's plausible CEILING, not their average.
#
# Ceiling = the 90th-percentile DKSP for the driver's start slot (fitted at this
# track, shrunk toward track type) with headroom, capped by the best the driver
# or their team has ever actually managed. The slot term is track-specific,
# which is what keeps the cap on the right scale — the dominator pot swings ~11x
# across the schedule (a 500-lap short track pays ~217 DK dominator points, a
# road course ~56), and it is also where most of the cap's ranking power lives.
#
# A share-of-pot rewrite was tried and measured against this on pinball loss at
# tau=0.95; it did not beat this at matched coverage. See sweep_dkmax.R, which
# scores candidate formulas walk-forward without paying for the Monte Carlo.
# =============================================================================
.gts_dkmax <- function(hist, entry, tgt, nm, tmn, slot_fallback = NULL) {
  brk <- c(0, 3, 6, 10, 15, 20, 25, Inf)
  hs  <- hist[!is.na(hist$start_ps) & !is.na(hist$DKSP), , drop = FALSE]
  lev <- levels(cut(numeric(0), brk))
  slot_tab <- function(x) {
    if (nrow(x) < 40) return(NULL)
    v <- tapply(x$DKSP, cut(x$start_ps, brk),
                function(z) stats::quantile(z, 0.90, na.rm = TRUE))
    o <- setNames(rep(NA_real_, length(lev)), lev)
    o[names(v)] <- as.numeric(v)
    # carry the nearest known slot forward so no level is left empty
    for (i in seq_along(o)) if (is.na(o[i]) && i > 1) o[i] <- o[i - 1]
    for (i in rev(seq_along(o))) if (is.na(o[i]) && i < length(o)) o[i] <- o[i + 1]
    o[is.na(o)] <- 0
    o
  }
  st_trk   <- slot_tab(hs[hs$track_name == tgt$track_name, , drop = FALSE])
  base_tab <- slot_tab(hs[hs$track_type == tgt$track_type, , drop = FALSE])
  if (is.null(base_tab)) base_tab <- slot_tab(hs)
  ceil_tab <- base_tab
  if (!is.null(st_trk) && !is.null(base_tab)) {
    n <- sum(hs$track_name == tgt$track_name)
    w <- n / (n + 300)
    ceil_tab <- w * st_trk[lev] + (1 - w) * base_tab[lev]
    names(ceil_tab) <- lev
  }

  # Start slot drives the ceiling. Before qualifying the entry list carries no
  # grid at all, and dumping the whole field into the last bucket would collapse
  # every cap to ~1 and switch the sim's dominator mechanism off. So when starts
  # are missing, fall back to the projected speed order as a stand-in grid.
  st <- suppressWarnings(as.numeric(entry$Start))
  has_start <- sum(!is.na(st) & st > 0) >= 0.5 * nrow(entry)
  if (!has_start) {
    st <- if (!is.null(slot_fallback)) as.numeric(slot_fallback)
          else rep(NA_real_, nrow(entry))
  }
  st[is.na(st) | st <= 0] <- nrow(entry)
  slot_ceiling <- if (is.null(ceil_tab)) rep(0, nrow(entry)) else
    as.numeric(ceil_tab[as.character(cut(st, brk))])
  slot_ceiling[is.na(slot_ceiling)] <- 0

  safe_max <- function(v) { v <- v[is.finite(v)]; if (!length(v)) NA_real_ else max(v) }
  dmax <- hist %>% group_by(Full_Name) %>% summarise(v = safe_max(DKSP), .groups = "drop")
  tmax <- hist %>% group_by(team_name)  %>% summarise(v = safe_max(DKSP), .groups = "drop")
  dm <- setNames(dmax$v, dmax$Full_Name); tmx <- setNames(tmax$v, tmax$team_name)
  hist_best <- unname(mapply(function(a, b) {
    x <- c(if (a %in% names(dm)) dm[[a]] else NA_real_,
           if (b %in% names(tmx)) tmx[[b]] else NA_real_)
    x <- x[is.finite(x)]; if (!length(x)) 0 else max(x)
  }, nm, tmn))

  out <- round(pmin(hist_best, slot_ceiling * .gts_DKMAX_MULT))
  out[!is.finite(out)] <- 0
  pmax(out, 0)
}

#' @param hist   historical Results rows for this series (target race excluded)
#' @param entry  entry list (Name, Car, Team, DK_Salary, Start)
#' @param tgt    one-row Races record for the target race
#' @return data.frame: Name, W..T30, DKMax
gts_auto_inputs <- function(hist, entry, tgt) {
  if (is.null(hist) || nrow(hist) == 0 || is.null(entry) || nrow(entry) == 0)
    return(NULL)
  
  hist$is_run   <- !is.na(hist$finishing_status) & hist$finishing_status == "Running"
  hist$is_crash <- !is.na(hist$finishing_status) & hist$finishing_status %in% .gts_CRASH
  hist$is_mech  <- !is.na(hist$finishing_status) & !hist$is_run & !hist$is_crash
  
  d <- .gts_speed_pct(hist)
  if (nrow(d) < 50) return(NULL)
  cur <- suppressWarnings(as.numeric(tgt$race_season))
  if (is.na(cur)) cur <- max(d$race_season, na.rm = TRUE)
  d$rw <- 0.5 ^ ((cur - d$race_season) / .gts_HALFLIFE)
  
  field_mu <- stats::weighted.mean(d$spd, d$rw, na.rm = TRUE)
  
  agg <- function(df, keys) {
    df %>% group_by(across(all_of(keys))) %>%
      summarise(m = stats::weighted.mean(spd, rw), n = sum(rw), .groups = "drop")
  }
  tm <- agg(d, "team_name")
  tm$est <- mapply(.gts_shrink, tm$m, tm$n, MoreArgs = list(prior = field_mu, K = .gts_K_TEAM))
  team_est <- setNames(tm$est, tm$team_name)
  
  cr <- agg(d, c("team_name", "car_number"))
  cr$prior <- ifelse(cr$team_name %in% names(team_est), team_est[cr$team_name], field_mu)
  cr$est <- mapply(.gts_shrink, cr$m, cr$n, cr$prior, MoreArgs = list(K = .gts_K_CAR))
  car_key <- paste(cr$team_name, cr$car_number, sep = "|")
  car_est <- setNames(cr$est, car_key)
  
  dv <- agg(d, "Full_Name")
  main_car <- d %>% group_by(Full_Name, team_name, car_number) %>%
    summarise(w = sum(rw), .groups = "drop") %>%
    group_by(Full_Name) %>% slice_max(w, n = 1, with_ties = FALSE) %>% ungroup() %>%
    mutate(k = paste(team_name, car_number, sep = "|"))
  dv <- dv %>% left_join(main_car[, c("Full_Name", "k")], by = "Full_Name")
  dv$prior <- ifelse(!is.na(dv$k) & dv$k %in% names(car_est), car_est[dv$k], field_mu)
  dv$est <- mapply(.gts_shrink, dv$m, dv$n, dv$prior, MoreArgs = list(K = .gts_K_DRIVER))
  drv_est <- setNames(dv$est, dv$Full_Name)
  drv_n   <- setNames(dv$n,   dv$Full_Name)
  
  # Salary prior: rank of DK salary mapped onto the speed-percentile scale.
  sal <- suppressWarnings(as.numeric(entry$DK_Salary))
  if (sum(!is.na(sal)) >= 3 && length(unique(sal[!is.na(sal)])) >= 3) {
    r <- rank(-sal, na.last = "keep") / sum(!is.na(sal))
    sal_prior <- 0.5 + (r - 0.5) * 0.80
    sal_prior[is.na(sal_prior)] <- field_mu
  } else {
    sal_prior <- rep(field_mu, nrow(entry))
  }
  
  nm <- as.character(entry$Name); tmn <- as.character(entry$Team)
  ck <- paste(tmn, entry$Car, sep = "|")
  spd_est <- numeric(nrow(entry))
  for (i in seq_len(nrow(entry))) {
    sp <- sal_prior[i]
    if (nm[i] %in% names(drv_est)) {
      spd_est[i] <- .gts_shrink(drv_est[[nm[i]]], drv_n[[nm[i]]], sp, .gts_K_SAL)
    } else if (ck[i] %in% names(car_est)) {
      spd_est[i] <- .gts_shrink(car_est[[ck[i]]], 3, sp, .gts_K_SAL)
    } else if (tmn[i] %in% names(team_est)) {
      spd_est[i] <- .gts_shrink(team_est[[tmn[i]]], 3, sp, .gts_K_SAL)
    } else spd_est[i] <- sp
  }
  
  # Latent finish model, fitted at this track and shrunk toward track type and
  # then the whole series. Yields the speed slope, the start slope and the
  # residual spread together, all on the finish-percentile scale.
  d$fin_p <- ave(d$ps, factor(d$race_id), FUN = function(x) {
    if (all(is.na(x))) return(rep(NA_real_, length(x)))
    rank(x, na.last = "keep") / sum(!is.na(x))
  })

  # Leave-one-race-out speed forecast for every historical row: the driver's
  # credibility-shrunk speed with that race's own contribution taken back out,
  # then run through the same two shrink steps the prediction path uses. This is
  # the regressor the latent fit is calibrated on.
  ds  <- d %>% group_by(Full_Name) %>%
    summarise(S = sum(rw * spd), W = sum(rw), .groups = "drop")
  di    <- match(d$Full_Name, ds$Full_Name)
  W_loo <- ds$W[di] - d$rw
  m_loo <- ifelse(W_loo > 0, (ds$S[di] - d$rw * d$spd) / W_loo, NA_real_)
  ck_d  <- paste(d$team_name, d$car_number, sep = "|")
  pr_d  <- ifelse(ck_d %in% names(car_est), car_est[ck_d], field_mu)
  w1 <- W_loo / (W_loo + .gts_K_DRIVER)
  e1 <- ifelse(is.na(m_loo), pr_d, w1 * m_loo + (1 - w1) * pr_d)
  w2 <- W_loo / (W_loo + .gts_K_SAL)
  d$spd_hat <- w2 * e1 + (1 - w2) * field_mu

  trk <- d[!is.na(d$track_name) & d$track_name == tgt$track_name, , drop = FALSE]
  typ <- d[!is.na(d$track_type) & d$track_type == tgt$track_type, , drop = FALSE]

  fit_all <- .gts_fit_latent(d)
  fit_typ <- .gts_blend_latent(.gts_fit_latent(typ), fit_all, .gts_K_BETA)
  lat_fit <- .gts_blend_latent(.gts_fit_latent(trk), fit_typ, .gts_K_BETA)
  if (is.null(lat_fit)) lat_fit <- list(a = 0, b_spd = 1, b_st = 0, sd = 0.15)

  sd_res <- lat_fit$sd
  if (!is.finite(sd_res) || sd_res <= 0) sd_res <- 0.15

  # DNF: crash carries driver/team signal, mechanical is mostly environment.
  base_c <- if (nrow(typ) > 30) mean(hist$is_crash[hist$track_type %in% tgt$track_type], na.rm = TRUE) else mean(hist$is_crash, na.rm = TRUE)
  base_m <- if (nrow(typ) > 30) mean(hist$is_mech[hist$track_type  %in% tgt$track_type], na.rm = TRUE) else mean(hist$is_mech,  na.rm = TRUE)
  htrk <- hist[!is.na(hist$track_name) & hist$track_name == tgt$track_name, , drop = FALSE]
  if (nrow(htrk) > 60) {
    w <- nrow(htrk) / (nrow(htrk) + 150)
    base_c <- w * mean(htrk$is_crash, na.rm = TRUE) + (1 - w) * base_c
    base_m <- w * mean(htrk$is_mech,  na.rm = TRUE) + (1 - w) * base_m
  }
  if (!is.finite(base_c)) base_c <- 0.08
  if (!is.finite(base_m)) base_m <- 0.05
  
  tcrash <- hist %>% group_by(team_name) %>%
    summarise(m = mean(is_crash, na.rm = TRUE), n = n(), .groups = "drop")
  p_crash <- p_mech <- numeric(nrow(entry))
  for (i in seq_len(nrow(entry))) {
    g <- hist[hist$Full_Name == nm[i], , drop = FALSE]
    tp <- base_c
    j <- which(tcrash$team_name == tmn[i])
    if (length(j) == 1 && tcrash$n[j] >= 20)
      tp <- .gts_shrink(tcrash$m[j], tcrash$n[j], base_c, 30)
    p_crash[i] <- if (nrow(g)) .gts_shrink(mean(g$is_crash, na.rm = TRUE), nrow(g), tp, .gts_K_CRASH) else tp
    p_mech[i]  <- if (nrow(g)) .gts_shrink(mean(g$is_mech,  na.rm = TRUE), nrow(g), base_m, .gts_K_MECH) else base_m
  }
  p_crash <- pmin(pmax(p_crash, 0), 0.6); p_mech <- pmin(pmax(p_mech, 0), 0.6)
  
  # ---- Monte Carlo over field orderings -------------------------------------
  starts <- suppressWarnings(as.numeric(entry$Start))
  has_start <- sum(!is.na(starts) & starts > 0) >= 0.5 * nrow(entry)
  F <- nrow(entry); N <- 20000L

  # Latent mean on the finish-percentile scale, straight from the fit. Both
  # slopes and the noise now share one set of units.
  mu <- lat_fit$a + lat_fit$b_spd * spd_est
  if (has_start) {
    sv <- starts; sv[is.na(sv) | sv <= 0] <- mean(sv[!is.na(sv) & sv > 0])
    s_sd <- stats::sd(sv)
    if (is.finite(s_sd) && s_sd > 0)
      mu <- mu + lat_fit$b_st * ((sv - mean(sv)) / s_sd)
  }
  lat <- matrix(mu, N, F, byrow = TRUE) +
    matrix(stats::rnorm(N * F, 0, sd_res), N, F)
  dnf_hit <- (matrix(stats::runif(N * F), N, F) < matrix(p_crash, N, F, byrow = TRUE)) |
    (matrix(stats::runif(N * F), N, F) < matrix(p_mech,  N, F, byrow = TRUE))
  lat[dnf_hit] <- lat[dnf_hit] + 10 + stats::runif(sum(dnf_hit))
  
  # Rank within each simulated race (row-wise) without an explicit loop.
  fin <- matrix(0L, N, F)
  ord <- t(apply(lat, 1, order))
  fin[cbind(rep(seq_len(N), each = F), as.vector(t(ord)))] <-
    rep(seq_len(F), times = N)
  
  res <- as.data.frame(lapply(.gts_THRESH, function(t) colMeans(fin <= t)))
  names(res) <- .gts_LABELS
  
  # Speed order stands in for the grid when qualifying has not run yet.
  dkmax <- .gts_dkmax(hist, entry, tgt, nm, tmn,
                      slot_fallback = rank(spd_est, ties.method = "first"))

  out <- data.frame(Name = entry$Name, stringsAsFactors = FALSE)
  for (l in .gts_LABELS) out[[l]] <- round(res[[l]], 4)
  out$DKMax <- dkmax
  out
}
