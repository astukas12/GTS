# =============================================================================
# player_model.R -- P3: simulated skater lines from simulated team boxes
# -----------------------------------------------------------------------------
#   p3_params(version)               the P3 constants: "fit" (hold-out replay) or "all" (live)
#   roster_frame(gameIds)            the pregame roster frame for historical games
#                                    (08 roles + shares, 09 rates); live is P4's job
#   sim_players(box, roster, P3)     one row per sim x skater: g, a, sog, blk,
#                                    sh_pts, so_g, toi (s), dk
#
# `box` is sim_team_box() output (or a real box in the same columns): one row
# per sim x team with game, sim, is_home, g_es, g_pp, g_sh, g_en, g_ea, g_ot,
# so_goals, S (non-goal SOG), blocks, reg_margin, sec_5v5, sec_eo, sec_pp,
# sec_pk, sec_pulled, sec_opp_pulled, sec_ot.
# `roster` has game, is_home and one row per dressed skater.
#
# Vectorised over sims within a team: N x P matrices. Categorical draws use the
# Gumbel-max trick (argmax of log weight + Gumbel noise) so every goal of every
# sim is drawn at once; draws without replacement are repeated argmaxes.
# The steps are P3_DESIGN sections 1-7 as fitted in R/fit/08..11; P3_FINDINGS
# has the numbers.
#
#   setwd("C:/Users/astuk/OneDrive/Documents/GTS/NHL"); source("R/player_model.R")
# =============================================================================

suppressMessages({ library(data.table) })
# (SimApp: dk_scoring.R / player_core.R are sourced by nhl_engine.R)

p3_params <- function(version = "fit") {
  RT <- readRDS("params/roles_toi.rds"); PR <- readRDS("params/player_rates.rds")
  GA <- readRDS("params/goal_alloc.rds"); SB <- readRDS("params/shots_blocks.rds")
  v <- function(x) x[[version]]
  mult <- dcast(v(PR)$state_mult[what %in% c("sog", "blk")], what + grp ~ state, value.var = "vs_es")
  list(roles = v(RT), rates = v(PR), alloc = v(GA), sb = v(SB), mult = mult,
       two_assist_rates = isTRUE(GA$diag$two_rates_win))
}

roster_frame <- function(gameIds) {
  RT <- readRDS("params/roles_toi.rds"); PR <- readRDS("params/player_rates.rds")
  db <- load_nhl_db()
  r <- RT$roster[gameId %in% gameIds, c("gameId", "teamId", "playerId", "grp", "pos", "slot", "pp", "pk", paste0("mu_", STATES)), with = FALSE]
  r <- merge(r, PR$pred[, !c("season", "date", "grp", "teamId")], by = c("gameId", "playerId"))
  r <- merge(r, db$games[, .(gameId, home_id)], by = "gameId")
  r[, is_home := teamId == home_id][, home_id := NULL]
  for (s in STATES) set(r, which(is.na(r[[paste0("mu_", s)]])), paste0("mu_", s), 0)
  r[]
}

# ---- matrix helpers ----------------------------------------------------------------------------
gumbel <- function(n, p) -log(-log(matrix(runif(n * p), n, p)))
argmax_rows <- function(L) max.col(L, ties.method = "first")
cap_rows <- function(S, n_on) {                  # each row to sum n_on, no cell above 1
  S <- pmax(S, 1e-9); S <- S / rowSums(S) * n_on
  for (it in 1:12) {
    over <- S > 1; if (!any(over)) break
    S[over] <- 1
    free <- !over & S < 1
    f <- (n_on - rowSums(S * !free)) / pmax(rowSums(S * free), 1e-12)
    S <- ifelse(free, S * f, S)
  }
  pmin(S, 1)
}
rmultinom_rows <- function(n, W) {               # one multinomial per row; n: counts, W: weights (N x P)
  N <- nrow(W); P <- ncol(W); out <- matrix(0L, N, P)
  rem <- as.integer(n); wrem <- rowSums(W)
  for (j in seq_len(P - 1L)) {
    p <- ifelse(wrem > 0, pmin(W[, j] / wrem, 1), 0)
    x <- rbinom(N, rem, p); out[, j] <- x
    rem <- rem - x; wrem <- wrem - W[, j]
  }
  out[, P] <- rem
  out
}
# top-k without replacement per row, from log weights L (N x P, -Inf = not eligible); returns a logical mask
topk_mask <- function(L, k) {
  M <- matrix(FALSE, nrow(L), ncol(L)); L <- L + gumbel(nrow(L), ncol(L))
  for (i in seq_len(k)) {
    j <- argmax_rows(L); ok <- is.finite(L[cbind(seq_len(nrow(L)), j)])
    M[cbind(which(ok), j[ok])] <- TRUE; L[cbind(seq_len(nrow(L)), j)] <- -Inf
  }
  M
}

# ---- one team, N sims ------------------------------------------------------------------------------
sim_team_players <- function(bx, ro, P3) {
  N <- nrow(bx); P <- nrow(ro)
  # PP1 D play more ES / PP time than their slot's share gives them (roles$pp1_d_mult, 08_roles_toi.R; none = as before)
  m1d <- P3$roles$pp1_d_mult
  if (!is.null(m1d)) { d1 <- ro$grp == "D" & ro$pp %in% 1L
    ro <- copy(ro); ro[d1, `:=`(mu_es = mu_es * m1d[["es"]], mu_pp = mu_pp * m1d[["pp"]])] }
  al <- P3$alloc; sb <- P3$sb; isD <- ro$grp == "D"; isF <- !isD
  sec <- list(es = pmax(bx$sec_5v5 + bx$sec_eo - bx$sec_ot, 0), pp = bx$sec_pp, pk = bx$sec_pk, ot = bx$sec_ot,
              pul = bx$sec_pulled, opul = bx$sec_opp_pulled)
  # 1. TOI: ES with the blowout shift and the wobble; the rest from fixed shares
  b <- P3$roles$blowout %||% c(0, 0)
  cs <- ifelse(isF, fcoalesce(ro$slot, 4L) - 2.5, fcoalesce(ro$slot, 3L) - 2)
  xb <- pmin(pmax(abs(bx$reg_margin) - 1, 0), 3)
  L_es <- outer(xb, cs * ifelse(isF, b[1], b[2])) + matrix(log(pmax(ro$mu_es, 1e-9)), N, P, byrow = TRUE)
  S_es <- cap_rows(exp(L_es), N_ON[["es"]])
  phi <- sb$wobble_phi %||% NULL
  if (!is.null(phi)) {
    g <- matrix(rgamma(N * P, shape = pmax(phi * S_es / N_ON[["es"]], 1e-6)), N, P)
    S_es <- cap_rows(g, N_ON[["es"]])
  }
  shr <- lapply(setNames(STATES[-1], STATES[-1]), function(s) cap_shares(ro[[paste0("mu_", s)]], N_ON[[s]]))
  TOI <- list(es = S_es * sec$es)
  for (s in names(shr)) TOI[[s]] <- outer(sec[[s]], shr[[s]])
  # 2. rates by state (OT / 6v5 / 5v6 = ES x league multiplier)
  gm <- function(w, s) P3$mult[what == w][.(ro$grp), on = "grp"][[s]]
  rS <- list(es = ro$r_sog_es, pp = ro$r_sog_pp, pk = ro$r_sog_pk, ot = ro$r_sog_es * gm("sog", "ot"),
             pul = ro$r_sog_es * gm("sog", "pul"), opul = ro$r_sog_es * gm("sog", "opul"))
  rB <- list(es = ro$r_blk_es, pp = ro$r_blk_es * gm("blk", "pp"), pk = ro$r_blk_pk, ot = ro$r_blk_es * gm("blk", "ot"),
             pul = ro$r_blk_es * gm("blk", "pul"), opul = ro$r_blk_es * gm("blk", "opul"))
  sweep_r <- function(Tm, r) Tm * matrix(r, N, P, byrow = TRUE)
  Gam <- if (!is.null(sb$alpha)) matrix(rgamma(N * P, sb$alpha, sb$alpha), N, P) else matrix(1, N, P)
  # the team's NON-goal SOG: each state's SOG rate x (1 - his chance a SOG is a goal).
  # D convert under half as often as F, so without this D get ~5% too few (validate_p3,
  # first run). A SOG at an empty net is always a goal: no non-goal share in opul.
  shb <- P3$rates$sh_base
  ng <- lapply(setNames(STATES, STATES), function(s) if (s == "opul") rep(0, P) else
    1 - pmin(shb[state == s][.(ro$grp), on = "grp", shp] * ro$r_shp, 0.9))
  WS <- Reduce(`+`, Map(function(Tm, r, q) sweep_r(Tm, r * q), TOI, rS, ng)) * Gam
  WB <- Reduce(`+`, Map(sweep_r, TOI, rB))
  # 3. non-goal SOG and blocks
  SOG <- rmultinom_rows(bx$S, WS)
  BLK <- rmultinom_rows(bx$blocks, WB)
  # 4. goals, by class
  G <- A <- SH <- matrix(0L, N, P)
  shp <- ro$r_shp
  inv_es <- ro$r_sog_es * shp * P3$rates$sh_base[state == "es"][.(ro$grp), on = "grp", shp] + ro$r_a_es
  inv_pp <- ro$r_sog_pp * shp * P3$rates$sh_base[state == "pp"][.(ro$grp), on = "grp", shp] + ro$r_a_pp
  beta <- al$beta
  classes <- list(es = list(n = bx$g_es, st = "es"), pp = list(n = bx$g_pp, st = "pp"), pk = list(n = bx$g_sh, st = "pk"),
                  ot = list(n = bx$g_ot, st = "ot"), pul = list(n = bx$g_ea, st = "pul"), opul = list(n = bx$g_en, st = "opul"))
  row_of <- function(M, gs) M[gs, , drop = FALSE]
  for (cn in names(classes)) {
    n <- fcoalesce(as.integer(classes[[cn]]$n), 0L); tot <- sum(n); if (!tot) next
    gs <- rep(seq_len(N), n)                                  # the sim of each goal
    K <- length(gs)
    # the on-ice group
    grp_mask <- matrix(FALSE, K, P)
    if (cn == "es") {
      unit_pick <- function(members_by, w_ind, mix, size, eligible, bt, part = 0) {
        units <- sort(unique(members_by[eligible & !is.na(members_by)]))
        wl <- sapply(units, function(u) { m <- eligible & members_by %in% u; mean(ro$mu_es[m]) * max(sum(inv_es[m]), 1e-6)^bt })
        intact <- runif(K) > mix
        M <- matrix(FALSE, K, P)
        if (length(units) && any(intact)) {
          pick <- units[sample.int(length(units), sum(intact), replace = TRUE, prob = wl)]
          M[intact, ] <- outer(pick, members_by, `==`) & matrix(eligible, sum(intact), P, byrow = TRUE)
          M[is.na(M)] <- FALSE
        }
        if (any(!intact)) {
          Lw <- matrix(ifelse(eligible, log(pmax(w_ind, 1e-9)), -Inf), sum(!intact), P, byrow = TRUE)
          M[!intact, ] <- topk_mask(Lw, size)
          # partly intact (al$partial, 10 section 2): size - 1 of one unit, chosen as above, + one other
          ni <- which(!intact); pt <- if (length(units) && part > 0) ni[runif(length(ni)) < part] else integer()
          if (length(pt)) {
            inU <- outer(units[sample.int(length(units), length(pt), replace = TRUE, prob = wl)], members_by, `==`)
            inU[is.na(inU)] <- FALSE
            lw <- matrix(ifelse(eligible, log(pmax(w_ind, 1e-9)), -Inf), length(pt), P, byrow = TRUE)
            M[pt, ] <- topk_mask(ifelse(inU, lw, -Inf), size - 1L) | topk_mask(ifelse(inU, -Inf, lw), 1L)
          }
        }
        M
      }
      # unit rates (mix, partial) are measured against the SAME-NIGHT lines / pairs / PP units (10_goal_alloc.R):
      # they assume the sheet carries the confirmed lines for the game, not last game's
      grp_mask <- unit_pick(ro$slot, ro$mu_es * pmax(inv_es, 1e-6)^(beta[["F_line"]] %||% 0), al$mix$F, 3L, isF, beta[["F_line"]] %||% 0,
                            al$partial$F2 %||% 0) |
                  unit_pick(ro$slot, ro$mu_es * pmax(inv_es, 1e-6)^(beta[["D_pair"]] %||% 0), al$mix$D, 2L, isD, beta[["D_pair"]] %||% 0)
    } else if (cn == "pp") {
      u <- runif(K); p1 <- al$mix$PP1; p2 <- al$mix$PP2
      m1 <- ro$pp %in% 1L; m2 <- ro$pp %in% 2L
      grp_mask[u < p1, ] <- matrix(m1, sum(u < p1), P, byrow = TRUE)
      sel2 <- u >= p1 & u < p1 + p2; grp_mask[sel2, ] <- matrix(m2, sum(sel2), P, byrow = TRUE)
      rest <- u >= p1 + p2 | rowSums(grp_mask) == 0
      if (any(rest)) grp_mask[rest, ] <- topk_mask(matrix(log(pmax(ro$mu_pp, 1e-9)), sum(rest), P, byrow = TRUE), 5L)
      # partly a unit (al$partial): 4 of PP1 / PP2 + one other skater
      q1 <- al$partial$PP1_4 %||% 0; q2 <- al$partial$PP2_4 %||% 0
      if (any(rest) && q1 + q2 > 0) {
        ri <- which(rest); v <- runif(length(ri))
        for (pu in list(list(m = m1, s = ri[v < q1]), list(m = m2, s = ri[v >= q1 & v < q1 + q2]))) {
          if (!length(pu$s) || sum(pu$m) < 4L) next
          lw <- matrix(log(pmax(ro$mu_pp, 1e-9)), length(pu$s), P, byrow = TRUE); inU <- matrix(pu$m, length(pu$s), P, byrow = TRUE)
          grp_mask[pu$s, ] <- topk_mask(ifelse(inU, lw, -Inf), 4L) | topk_mask(ifelse(inU, -Inf, lw), 1L)
        }
      }
    } else {
      k <- c(pk = 4L, ot = 3L, pul = 6L, opul = 5L)[[cn]]
      grp_mask <- topk_mask(matrix(log(pmax(ro[[paste0("mu_", cn)]], 1e-9)), K, P, byrow = TRUE), k)
    }
    # the scorer: rate x sh multiplier (EN: the rate and the role table), exponent adjustments from 10
    cls <- if (cn %in% c("es", "pp")) cn else "other"
    co <- al$scorer[[cls]]
    if (cn == "opul") co <- c(g_rate = 0, g_shp = 0, D = 0)   # EN: the role table carries the D effect (10, section 7)
    lr <- log(pmax(if (cn == "pp") ro$r_sog_pp else if (cn == "pk") ro$r_sog_pk else if (cn == "pul") ro$r_sog_pp else ro$r_sog_es, 1e-6))
    sh0 <- if (cn == "opul") 1 else P3$rates$sh_base[state == cn][.(ro$grp), on = "grp", shp]
    ls <- if (cn == "opul") 0 else log(pmax(shp, 1e-6))
    lb <- if (cn == "opul") 0 else log(pmax(sh0, 1e-6))     # base sh%: part of the offset, not scaled by g_shp
    lw <- (1 + co[["g_rate"]]) * lr + (1 + co[["g_shp"]]) * ls + lb + co[["D"]] * isD
    if (cn == "opul") { role <- ifelse(isD, "D", paste0("F", fcoalesce(ro$slot, 4L))); lw <- lw + log(al$en_role[role]) }
    L <- matrix(lw, K, P, byrow = TRUE) + log(row_of(Gam, gs))
    L[!grp_mask] <- -Inf
    sc <- argmax_rows(L + gumbel(K, P))
    idx <- cbind(gs, sc)
    G <- G + matrix(tabulate((sc - 1L) * N + gs, N * P), N, P)
    # assists: how many, then who
    tab <- al$assist_count
    key <- if ("sgrp" %in% names(tab)) tab[state == cn][.(ro$grp[sc]), on = "sgrp"] else NULL
    pr <- if (is.null(key)) { t <- tab[state == cn][order(na)]; matrix(t$p, K, 3, byrow = TRUE) } else {
      t <- tab[state == cn]; sapply(0:2, function(k) t[na == k][.(ro$grp[sc]), on = "sgrp", p]) }
    u <- runif(K); na <- (u > pr[, 1]) + (u > pr[, 1] + pr[, 2])
    ac <- if (cls == "other") "other" else cls
    dA <- al$assist_D[class == ac]
    two <- isTRUE(P3$two_assist_rates)
    a1r <- if (two) (if (cn == "pp") ro$r_a1_pp else ro$r_a1_es) else (if (cn == "pp") ro$r_a_pp else ro$r_a_es)
    a2r <- if (two) (if (cn == "pp") ro$r_a2_pp else ro$r_a2_es) else a1r
    avail <- grp_mask; avail[cbind(seq_len(K), sc)] <- FALSE
    Lr <- matrix(log(pmax(a1r, 1e-6)) + dA[assist == "A1", D_log] * isD, K, P, byrow = TRUE); Lr[!avail] <- -Inf
    has1 <- na >= 1 & rowSums(avail) > 0
    a1 <- argmax_rows(Lr + gumbel(K, P))
    A <- A + matrix(tabulate(((a1 - 1L) * N + gs)[has1], N * P), N, P)
    avail[cbind(seq_len(K), a1)] <- FALSE
    Lr2 <- matrix(log(pmax(a2r, 1e-6)) + dA[assist == "A2", D_log] * isD, K, P, byrow = TRUE); Lr2[!avail] <- -Inf
    has2 <- na >= 2 & has1 & rowSums(avail) > 0
    a2 <- argmax_rows(Lr2 + gumbel(K, P))
    A <- A + matrix(tabulate(((a2 - 1L) * N + gs)[has2], N * P), N, P)
    if (cn == "pk") {
      SH <- SH + matrix(tabulate((sc - 1L) * N + gs, N * P), N, P) +
        matrix(tabulate(((a1 - 1L) * N + gs)[has1], N * P), N, P) + matrix(tabulate(((a2 - 1L) * N + gs)[has2], N * P), N, P)
    }
  }
  SOG <- SOG + G                                     # every goal carried a SOG
  # 5. shootout goals: distinct shooters, by SO share
  SO <- matrix(0L, N, P)
  kmax <- max(bx$so_goals)
  if (kmax > 0) {
    Lso <- matrix(log(pmax(ro$r_so, 1e-9)), N, P, byrow = TRUE) + gumbel(N, P)
    for (k in seq_len(kmax)) { j <- argmax_rows(Lso); ok <- bx$so_goals >= k
      SO[cbind(which(ok), j[ok])] <- 1L; Lso[cbind(seq_len(N), j)] <- -Inf }
  }
  toi <- Reduce(`+`, TOI)
  data.table(sim = rep(bx$sim, P), playerId = rep(ro$playerId, each = N), grp = rep(ro$grp, each = N),
             slot = rep(ro$slot, each = N), pp = rep(ro$pp, each = N),
             g = as.vector(G), a = as.vector(A), sog = as.vector(SOG), blk = as.vector(BLK), sh_pts = as.vector(SH),
             so_g = as.vector(SO), toi = as.vector(toi))
}

sim_players <- function(box, roster, P3) {
  box <- as.data.table(box); roster <- as.data.table(roster)
  keys <- unique(box[, .(game, is_home)])
  out <- lapply(seq_len(nrow(keys)), function(i) {
    k <- keys[i]
    bx <- box[game == k$game & is_home == k$is_home]; ro <- roster[game == k$game & is_home == k$is_home]
    if (!nrow(ro)) return(NULL)
    x <- sim_team_players(bx, ro, P3); x[, `:=`(game = k$game, is_home = k$is_home)]; x
  })
  out <- rbindlist(out)
  out[, dk := dk_skater_points(g, a, sog, blk, sh_pts, so_g)]
  out[]
}
