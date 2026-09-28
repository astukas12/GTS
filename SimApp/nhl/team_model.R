# =============================================================================
# team_model.R -- P2: simulated team box scores + goalie lines for one game
# -----------------------------------------------------------------------------
#   fit_grid(odds, params)              the game's regulation grid from its lines
#   sim_team_box(game, params, n_sims)  n_sims box scores per game, both teams
#
# params = readRDS("params/team_params.rds")$fit (hold-out replay) or $all (live).
# Every piece is P2_DESIGN sections 1-7 as fitted in R/fit/01..07; P2_FINDINGS
# has the numbers. Vectorised over sims (and over games: `game` may hold many
# rows, each simulated n_sims times).
#
# `game` columns (one row per game):
#   market   p_home, total, p_over      de-vigged full-game 2-way ML (incl OT/SO)
#                                       and the full-game total line + P(over)
#            [live, optional] p_reg_home, p_reg_draw, reg_total, p_reg_over:
#            Pinnacle's regulation (period 6) 3-way and total. When present the
#            tie factor is solved to the draw price and P(home | tie) comes from
#            the 2-way vs the 3-way; otherwise the fixed history tie factor.
#   PP       e_h, e_a     expected PP chances (team drawn x opp taken / league, 04)
#            L_pp         league PP chances per team at the date
#            lt_T         league whistle level for T at the date (05, season credibility)
#            adj_pp_h, adj_pp_a   log PP strength x opp PK weakness (05's S1 term)
#   shots    lrel_h, lrel_a       log SOG matchup, log(sog_for * opp sog_ag / L^2)
#            lvl_S                league S level at the date / params mean_S
#            lrel_mis_h/_a, lrel_blk_h/_a   log team miss / block tendencies (06)
#            lvl_mis, lvl_blk     league attempt levels at the date / norm (06)
#
# Output: one row per sim x team, columns
#   game, sim, is_home; goals (non-shootout), g_es, g_pp, g_sh, g_en, g_ea, g_ot,
#   so_goals, result (W/L/OTL), end (REG/OT/SO), reg_goals, reg_margin;
#   pp_chances; sec_5v5, sec_eo (4v4 + OT 3v3), sec_pp, sec_pk, sec_pulled,
#   sec_opp_pulled, sec_ot; S (own non-goal SOG = the other goalie's saves), sog,
#   missed, blocked, tm_blocked, att, blocks (made); the team's own goalies:
#   relieved, s_sv, s_ga, s_dec, s_so, s_dk, b_sv, b_ga, b_dec, b_dk.
#
#   setwd("C:/Users/astuk/OneDrive/Documents/GTS/NHL"); source("R/team_model.R")
#   P <- readRDS("params/team_params.rds")$all
#   box <- sim_team_box(game, P, 1000)
# =============================================================================

suppressMessages({ library(data.table) })
# (SimApp: update_db.R cut; add_dk_shutout() is in nhl/dk_shutout.R)
# (SimApp: grid_core.R / goalie_core.R are sourced by nhl_engine.R)

# ---- the grid ------------------------------------------------------------------------------
grid_probs <- function(lh, la, lm0, th, sp) {   # grid_P with a per-game tie factor
  th0 <- th; th0[["lm0"]] <- 0
  P <- grid_P(lh, la, th0, sp)
  P[, Mc == 0] <- P[, Mc == 0] * exp(lm0)
  P / rowSums(P)
}
reg_over <- function(P, L) {                    # P(H + A > L), pushes void
  tot <- Hc + Ac
  ov <- drop(P %*% (tot > L))
  if (L == round(L)) ov <- ov / (1 - drop(P %*% (tot == L)))
  ov
}
fit_grid <- function(odds, params) {
  th <- params$grid$theta; sp <- params$grid$spec
  d <- as.data.table(odds)
  live <- if ("p_reg_draw" %in% names(d)) !is.na(d$p_reg_draw) else rep(FALSE, nrow(d))
  out <- data.table(lam_h = NA_real_, lam_a = NA_real_, lm0 = th[["lm0"]], q = q_tie(1, 1, th), maxres = NA_real_, live = live)[rep(1L, nrow(d))]
  out[, live := live]
  if (any(!live)) {
    x <- solve_lam(d[!live], th, sp)
    out[!live, `:=`(lam_h = exp(x[, 1]), lam_a = exp(x[, 2]), maxres = attr(x, "maxres"))]
  }
  for (i in which(live)) {                      # three numbers to three prices: lam_h, lam_a, the tie factor
    g <- d[i]
    tgt <- c(g$p_reg_home, g$p_reg_draw, g$p_reg_over)
    res <- function(x) { P <- grid_probs(exp(x[1]), exp(x[2]), x[3], th, sp)
      c(sum(P[Mc > 0]), sum(P[Mc == 0]), reg_over(P, g$reg_total)) - tgt }
    mu <- g$reg_total + qnorm(g$p_reg_over) * sqrt(g$reg_total)
    s <- 0.35 * qlogis(g$p_reg_home / (1 - g$p_reg_draw))
    x <- c(log(mu / 2) + s, log(mu / 2) - s, th[["lm0"]])
    for (it in 1:80) {
      r <- res(x); if (max(abs(r)) < 1e-10) break
      J <- sapply(1:3, function(k) { e <- replace(numeric(3), k, 1e-6); (res(x + e) - r) / 1e-6 })
      st <- tryCatch(solve(J, r), error = function(e) rep(0, 3))
      x <- x - pmax(pmin(st, 0.5), -0.5)
    }
    # q_g, not q: inside out[...] a bare `q` is out's own column (the history default), not this value.
    # That shadowing kept every live game at q_tie(1, 1) until 27 Sep 2026 (build_slate smoke: EDM .668 vs .714).
    q_g <- if (!is.null(g$p_home) && !is.na(g$p_home)) (g$p_home - g$p_reg_home) / g$p_reg_draw else q_tie(1, 1, th)
    q_g <- min(max(q_g, 0.35), 0.65); mr <- max(abs(res(x)))
    out[i, `:=`(lam_h = exp(x[1]), lam_a = exp(x[2]), lm0 = x[3], q = q_g, maxres = mr)]
  }
  out[]
}

# ---- helpers --------------------------------------------------------------------------------
wsample_by <- function(key, tab, keycol, valcol = "sec", wcol = "w") {   # weighted draw of valcol for each row's key
  out <- rep(NA_real_, length(key))
  for (k in unique(key)) {
    i <- which(key == k); s <- tab[tab[[keycol]] == k]
    if (!nrow(s)) next
    out[i] <- s[[valcol]][sample.int(nrow(s), length(i), replace = TRUE, prob = s[[wcol]])]
  }
  out
}
lin <- function(par, cols) {                    # sum of par[[nm]] * cols[[nm]] over the term names in par
  nm <- setdiff(names(par), c(grep("^r_", names(par), value = TRUE), "lsize"))
  if (!length(nm)) return(0)
  Reduce(`+`, lapply(nm, function(k) { if (is.null(cols[[k]])) stop("no column for term ", k); par[[k]] * cols[[k]] }))
}
rcat <- function(P) {                           # one categorical draw per row of P
  C <- P %*% upper.tri(diag(ncol(P)), diag = TRUE)
  pmin(rowSums(runif(nrow(P)) > C) + 1L, ncol(P))
}
TTs <- 0:40
cmp_pmf <- function(loglam, nu) {
  A <- outer(loglam, TTs) - nu * rep(lgamma(TTs + 1), each = length(loglam))
  A <- exp(A - apply(A, 1, max)); A / rowSums(A)
}

# ---- the simulation -----------------------------------------------------------------------------
sim_team_box <- function(game, params, n_sims, grid = NULL) {
  P <- params; game <- as.data.table(game); ng <- nrow(game); N <- ng * n_sims
  if (is.null(grid)) grid <- fit_grid(game, P)
  gi <- rep(seq_len(ng), each = n_sims)                     # game index per sim row
  th <- P$grid$theta; sp <- P$grid$spec

  # 1. regulation score
  GP <- grid_probs(grid$lam_h, grid$lam_a, grid$lm0, th, sp)
  cell <- unlist(lapply(seq_len(ng), function(j) sample.int(NC, n_sims, replace = TRUE, prob = GP[j, ])))
  rh <- Hc[cell]; ra <- Ac[cell]; M <- rh - ra; tie <- M == 0

  # 2. goal classes: EN / EA worked back from the final regulation score
  C <- P$classes
  en_h <- en_a <- ea_h <- ea_a <- integer(N)
  dec <- which(!tie)
  if (length(dec)) {
    W <- pmax(rh, ra)[dec]; L <- pmin(rh, ra)[dec]; mb <- pmin(W - L, C$MB); lb <- pmin(L, C$LB)
    j <- integer(length(dec))
    for (key in unique(mb * 10L + lb)) {
      i <- which(mb * 10L + lb == key); pr <- C$table[key %/% 10L, key %% 10L + 1L, ]
      j[i] <- sample.int(length(pr), length(i), replace = TRUE, prob = pr)
    }
    wEN <- C$cats$en[j]; lEA <- C$cats$ea[j]
    wEA <- as.integer(W - wEN >= 1 & runif(length(dec)) < C$winner_ea_p)   # delayed-penalty EA for the winner
    hw <- rh[dec] > ra[dec]
    en_h[dec] <- fifelse(hw, wEN, 0L); en_a[dec] <- fifelse(hw, 0L, wEN)
    ea_h[dec] <- fifelse(hw, wEA, lEA); ea_a[dec] <- fifelse(hw, lEA, wEA)
  }
  ti <- which(tie & rh >= 1)
  if (length(ti)) {                                         # EA equalisers in ties, logistic in G
    G <- rh[ti]; tp <- C$tie_par
    z <- cbind(0, tp[1] + tp[2] * G, tp[3] + tp[4] * G); pe <- exp(z - apply(z, 1, max)); pe <- pe / rowSums(pe)
    e <- rcat(pe) - 1L
    side_h <- runif(length(ti)) < 0.5
    same <- e == 2L & runif(length(ti)) < C$tie_two_same & G >= 2L
    ea_h[ti] <- fifelse(e == 1L, as.integer(side_h), fifelse(e == 2L, fifelse(same, 2L * side_h, 1L), 0L))
    ea_a[ti] <- fifelse(e == 1L, as.integer(!side_h), fifelse(e == 2L, fifelse(same, 2L * !side_h, 1L), 0L))
  }

  # 3. OT / SO for ties: the winner from q, then how it ends
  home_wins_tie <- runif(N) < grid$q[gi]
  Lam <- (grid$lam_h + grid$lam_a)[gi]
  oq <- P$ot$q; R <- exp(oq[["c0"]] + oq[["b"]] * log(Lam / 6)); s_ot <- exp(oq[["ls"]])
  u <- runif(N)
  in_ot <- tie & u < 1 - exp(-R)
  t_ot <- ifelse(in_ot, P$ot$len * (-log(1 - runif(N) * (1 - exp(-R))) / R)^(1 / s_ot), 0)
  t_ot[tie & !in_ot] <- P$ot$len
  so <- tie & !in_ot
  so_w <- so_l <- integer(N)
  if (any(so)) { k <- sample.int(nrow(P$so$table), sum(so), replace = TRUE, prob = P$so$table$p)
    so_w[so] <- P$so$table$wg[k]; so_l[so] <- P$so$table$lg[k] }
  ot_h <- as.integer(in_ot & home_wins_tie); ot_a <- as.integer(in_ot & !home_wins_tie)
  home_win <- fifelse(tie, home_wins_tie, M > 0)
  end <- fifelse(!tie, "REG", fifelse(in_ot, "OT", "SO"))

  # 4. PP chances: T through the goals copula, split call by call
  pp <- P$pp; tp <- pp$T$par
  x_T <- log(game$lt_T) + log((game$e_h + game$e_a) / (2 * game$L_pp))
  TP <- cmp_pmf(tp[["b0"]] + x_T, if (pp$T$spec %chin% c("C1", "C2")) exp(tp[["lnu"]]) else 1)
  TC <- t(apply(TP, 1, cumsum))
  tot <- Hc + Ac; GT <- sapply(0:max(tot), function(g) GP %*% (tot == g)); if (ng == 1) GT <- matrix(GT, 1)
  GC <- t(apply(GT, 1, cumsum)); if (ng == 1) GC <- matrix(GC, 1)
  Gr <- rh + ra
  g_hi <- GC[cbind(gi, Gr + 1L)]; g_lo <- g_hi - GT[cbind(gi, Gr + 1L)]
  zG <- qnorm(pmin(pmax(runif(N, g_lo, g_hi), 1e-12), 1 - 1e-12))
  rho <- pp$copula_rho
  uT <- pnorm(rho * zG + sqrt(1 - rho^2) * rnorm(N))
  Tn <- rowSums(uT > TC[gi, , drop = FALSE])
  sc <- pp$split; o <- log(game$e_a / game$e_h)[gi]
  nh <- integer(N); D <- integer(N)
  for (k in seq_len(max(Tn))) {
    act <- Tn >= k
    p <- plogis(sc[["b0"]] + sc[["bo"]] * o + sc[["kappa"]] * pmin(pmax(D, -sc[["dcap"]]), sc[["dcap"]]) +
                sc[["kappa2"]] * (pmin(pmax(D, -2L), 2L) - pmin(pmax(D, -1L), 1L)) + sc[["gamma"]] * M)
    yy <- act & runif(N) < p
    nh <- nh + yy; D <- D + (act & !yy) - yy
  }
  ppc_h <- Tn - nh; ppc_a <- nh                              # home PP = calls against away

  # 5. state times and the ES / PP / SH split of open goals
  nomd <- function(n) { nb <- pmin(n, 5L); u <- numeric(length(n)); i <- which(n > 0)
    u[i] <- wsample_by(nb[i], pp$duration$nom, "nb", "u"); n * u }
  pnom_h <- nomd(ppc_h); pnom_a <- nomd(ppc_a)
  pull_cell <- function(mg, enA, eaF) {
    mbs <- sprintf("%+d", pmax(pmin(mg, 3L), -3L))
    fifelse(mg < 0, sprintf("%s|en%d|ea%d", mbs, as.integer(enA > 0), as.integer(eaF > 0)),
            fifelse(mg == 0, sprintf("0|ea%d", as.integer(eaF > 0)), mbs))
  }
  draw_pull <- function(mg, enA, eaF) {
    cl <- pull_cell(mg, enA, eaF); mbs <- sprintf("%+d", pmax(pmin(mg, 3L), -3L)); mbs[mg == 0] <- "+0"
    thin <- cl %chin% pp$pulled$thin | !(cl %chin% unique(pp$pulled$tab[[pp$pulled$key]]))
    out <- numeric(length(mg))
    out[!thin] <- wsample_by(cl[!thin], pp$pulled$tab, pp$pulled$key)
    if (any(thin)) out[thin] <- wsample_by(mbs[thin], pp$pulled$margin, "mb")
    out[is.na(out)] <- 0
    out
  }
  pul_h <- draw_pull(M, en_a, ea_h); pul_a <- draw_pull(-M, en_h, ea_a)
  oe_reg <- wsample_by(pmin(Tn %/% 2L, 5L), pp$other_even, "Tb"); oe_reg[is.na(oe_reg)] <- 0
  es_sec <- pmax(3600 - pnom_h - pnom_a - pul_h - pul_a, 600)
  stp <- pp$state$par
  split_open <- function(n_open, pnom_own, pnom_opp, adj, chances) {
    w <- cbind(log(es_sec), log(pmax(pnom_own, 1e-6)) + stp[["a_pp"]] + (if ("b_pp" %in% names(stp)) stp[["b_pp"]] * adj else 0),
               log(pmax(pnom_opp, 1e-6)) + stp[["a_sh"]])
    w <- exp(w - apply(w, 1, max)); w <- w / rowSums(w)
    n_pp <- rbinom(N, n_open, w[, 2])
    n_sh <- rbinom(N, n_open - n_pp, pmin(w[, 3] / pmax(1 - w[, 2], 1e-12), 1))
    over <- pmax(n_pp - chances, 0L); n_pp <- n_pp - over      # the cap: minors end on a goal
    list(es = n_open - n_pp - n_sh, pp = n_pp, sh = n_sh)
  }
  open_h <- rh - en_h - ea_h; open_a <- ra - en_a - ea_a
  cls_h <- split_open(open_h, pnom_h, pnom_a, game$adj_pp_h[gi], ppc_h)
  cls_a <- split_open(open_a, pnom_a, pnom_h, game$adj_pp_a[gi], ppc_a)
  tau <- pp$duration$tau
  spp_h <- pmax(pnom_h - tau * cls_h$pp, 0); spp_a <- pmax(pnom_a - tau * cls_a$pp, 0)
  eo_h <- oe_reg + t_ot
  s5_h <- pmax(3600 - spp_h - spp_a - pul_h - pul_a - oe_reg, 0)

  # team-level frame: home rows then away rows
  H <- function(h, a) c(h, a)
  tm <- data.table(
    game = rep(gi, 2L), sim = rep(rep(seq_len(n_sims), ng), 2L), is_home = rep(c(TRUE, FALSE), each = N),
    reg_goals = H(rh, ra), reg_margin = H(M, -M), g_en = H(en_h, en_a), g_ea = H(ea_h, ea_a),
    g_es = H(cls_h$es, cls_a$es), g_pp = H(cls_h$pp, cls_a$pp), g_sh = H(cls_h$sh, cls_a$sh), g_ot = H(ot_h, ot_a),
    so_goals = H(fifelse(so & home_wins_tie, so_w, so_l), fifelse(so & !home_wins_tie, so_w, so_l)),
    win = H(home_win, !home_win), end = rep(end, 2L), pp_chances = H(ppc_h, ppc_a),
    sec_5v5 = rep(s5_h, 2L), sec_eo = rep(eo_h, 2L), sec_pp = H(spp_h, spp_a), sec_pk = H(spp_a, spp_h),
    sec_pulled = H(pul_h, pul_a), sec_opp_pulled = H(pul_a, pul_h), sec_ot = rep(t_ot, 2L))
  tm[, so_goals := fifelse(end == "SO", so_goals, 0L)]
  tm[, goals := reg_goals + g_ot]
  tm[, result := fifelse(win, "W", fifelse(end == "REG", "L", "OTL"))]
  gx <- c(gi, gi); hm <- tm$is_home
  pick2 <- function(h, a) fifelse(hm, game[[h]][gx], game[[a]][gx])
  opp_goals <- c(tm$goals[(N + 1):(2 * N)], tm$goals[1:N])

  # 6. saves faced (S) with the copula between the teams, then attempts
  sh <- P$shots; Sp <- sh$S$par
  secE <- cbind(tm$sec_5v5, tm$sec_eo, tm$sec_pp, tm$sec_pk, tm$sec_pulled) / 3600
  lrel <- pick2("lrel_h", "lrel_a")
  cols <- list(script_lin = pmax(pmin(tm$reg_margin, 3L), -3L), home = as.numeric(hm), own_g = tm$goals)
  mu_S <- exp(lrel + log(game$lvl_S[gx]) + lin(Sp, cols)) * drop(secE %*% exp(Sp[paste0("r_", sh$S$states)]))
  z1 <- rnorm(N); z2 <- sh$rho * z1 + sqrt(1 - sh$rho^2) * rnorm(N)
  size_S <- if (isTRUE(sh$S$pois)) Inf else exp(Sp[["lsize"]])
  tm[, S := qnbinom(pnorm(c(z1, z2)), size = size_S, mu = mu_S)]
  tm[, sog := S + goals]
  at <- sh$attempts
  secA <- cbind(secE, tm$sec_opp_pulled / 3600)
  sogdev <- log((tm$sog + 1) / (mu_S + tm$goals + 1))
  for (what in c("mis", "blk")) {
    A <- at[[what]]; ap <- A$par
    lvl <- game[[paste0("lvl_", what)]][gx]
    tcol <- pick2(paste0("lrel_", what, "_h"), paste0("lrel_", what, "_a"))
    cl2 <- c(cols, list(team = tcol, sogdev = sogdev))
    mu <- exp(lrel + log(lvl) + ap[["r_level"]] + lin(ap, cl2)) * drop(secA %*% A$c[at$states])
    set(tm, j = if (what == "mis") "missed" else "blocked", value = rnbinom(2L * N, size = exp(ap[["lsize"]]), mu = mu))
  }
  tm[, tm_blocked := rbinom(.N, blocked, sh$tm)]
  tm[, att := sog + missed + blocked]
  tm[, blocks := c(blocked[(N + 1):(2 * N)] - tm_blocked[(N + 1):(2 * N)], blocked[1:N] - tm_blocked[1:N])]

  # 7. goalies: this team's goalies face the other team's S and non-EN goals
  opp_S <- c(tm$S[(N + 1):(2 * N)], tm$S[1:N]); opp_en <- c(tm$g_en[(N + 1):(2 * N)], tm$g_en[1:N])
  Gl <- sim_goalies(opp_goals - opp_en, opp_S, tm$goals, tm$result, P$goalies)
  Gl <- goalie_dk(Gl, opp_goals)
  tm[, c("relieved", "s_sv", "s_ga", "s_dec", "s_so", "s_dk", "b_sv", "b_ga", "b_dec", "b_dk") :=
       Gl[, .(relieved, s_sv, s_ga, s_dec, s_so, s_dk, b_sv, b_ga, b_dec, b_dk)]]
  tm[, win := NULL]
  setcolorder(tm, c("game", "sim", "is_home", "goals", "g_es", "g_pp", "g_sh", "g_en", "g_ea", "g_ot", "so_goals", "result", "end",
                    "reg_goals", "reg_margin", "pp_chances"))
  tm[]
}
