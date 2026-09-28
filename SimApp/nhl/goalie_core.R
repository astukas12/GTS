# =============================================================================
# goalie_core.R -- goalie-line functions (P2_DESIGN.md section 7)
# -----------------------------------------------------------------------------
# Shared by R/fit/07_goalies.R (which fits the pieces) and R/team_model.R.
# Definitions only; sourcing it runs nothing.
#
# Per team-game (one row = one sim of one team's goalies):
#   g   = goals against with a goalie in net (the opponent's non-EN goals, OT
#         included, shootout excluded) -- the team's goalie GA
#   S   = the opponent's saves faced (06) -- the team's goalie saves
#   gf  = the team's own goals (non-shootout)
# 1. relieved ~ Bernoulli(plogis(X b)), X from `terms` (g shape, S, gf)
# 2. relieved: backup GA b | g ~ beta-binomial(g, m_b, phi_b), logit m_b = c0 + c1 g;
#    starter GA k = g - b
# 3. relieved: backup saves ~ beta-binomial(S, m_v, phi_v),
#    logit m_v = d0 + d1 * (b + 0.5) / (g + 1)
# 4. relieved: the decision goes to the backup with plogis(e_result + f * (b - k))
# The DK shutout is add_dk_shutout()'s rule (update_db.R): the only goalie who
# played, and the opponent scored no non-shootout goal (EN included).
# =============================================================================

relief_X <- function(d, terms) {
  g <- pmin(d$g, 10L)
  cl <- list(int = rep(1, length(g)))
  if ("g" %in% terms) cl$g <- g
  if ("g2" %in% terms) cl$g2 <- g^2
  if ("hinge" %in% terms) cl$hinge <- pmax(g - 3, 0)
  if ("fac" %in% terms) for (k in 1:8) cl[[paste0("g", k)]] <- as.numeric(pmin(g, 8L) == k)
  if ("S" %in% terms) cl$S <- (d$S - 25) / 10
  if ("gf" %in% terms) cl$gf <- pmin(d$gf, 6L)
  as.matrix(as.data.table(cl))
}
relief_p <- function(d, b, terms) plogis(drop(relief_X(d, terms) %*% b))

# beta-binomial log pmf; phi = Inf is the binomial
lbb <- function(y, n, m, phi) {
  if (is.infinite(phi)) return(dbinom(y, n, m, log = TRUE))
  a <- m * phi; b <- (1 - m) * phi
  lchoose(n, y) + lbeta(y + a, n - y + b) - lbeta(a, b)
}
rbb <- function(n, m, phi) {
  p <- if (is.infinite(phi)) m else rbeta(length(n), m * phi, (1 - m) * phi)
  rbinom(length(n), n, p)
}

# The pull-point split (type "PP"): the starter is pulled after conceding k,
# k ~ pi (k = 0..6, flat beyond 6), and the backup then concedes a Poisson(mu)
# number, so given the total g:  P(k | g) ~ pi_k * dpois(g - k, mu), k = 0..g.
pp_logw <- function(g, sp) {                 # rows: g, cols: k = 0..KMAX
  kk <- 0:10; lpi <- c(0, sp$lpi)[pmin(kk, 6L) + 1L]
  W <- outer(g, kk, function(gg, k) ifelse(k <= gg, dpois(pmax(gg - k, 0), exp(sp$lmu), log = TRUE), -Inf))
  W + rep(lpi, each = length(g))
}
pp_ll <- function(k, g, sp) {                # log P(k | g)
  W <- pp_logw(pmin(g, 10L), sp); m <- apply(W, 1, max)
  W[cbind(seq_along(k), pmin(k, 10L) + 1L)] - (m + log(rowSums(exp(W - m))))
}
r_pull_point <- function(g, sp) {
  W <- pp_logw(pmin(g, 10L), sp); P <- exp(W - apply(W, 1, max)); P <- P / rowSums(P)
  C <- P %*% upper.tri(diag(ncol(P)), diag = TRUE)
  pmin(rowSums(runif(length(g)) > C), g)
}

# Vectorised over rows. par: list(relief = list(b, terms), split = c(c0, c1, lphi) or a "PP" list,
# saves = c(d0, d1, lphi), dec = c(eW, eL, eO, f)). result: "W", "L" or "OTL".
sim_goalies <- function(g, S, gf, result, par) {
  n <- length(g)
  rel <- runif(n) < relief_p(data.table(g = g, S = S, gf = gf), par$relief$b, par$relief$terms)
  b_ga <- integer(n); b_sv <- integer(n); b_dec <- logical(n)
  i <- which(rel)
  if (length(i)) {
    sp <- par$split
    if (is.list(sp) && identical(sp$type, "PP")) b_ga[i] <- g[i] - r_pull_point(g[i], sp)
    else b_ga[i] <- rbb(g[i], plogis(sp[["c0"]] + sp[["c1"]] * g[i]), exp(sp[["lphi"]]))
    sv <- par$saves; mv <- plogis(sv[["d0"]] + sv[["d1"]] * (b_ga[i] + 0.5) / (g[i] + 1))
    b_sv[i] <- rbb(S[i], mv, exp(sv[["lphi"]]))
    dc <- par$dec; e <- c(W = dc[["eW"]], L = dc[["eL"]], OTL = dc[["eO"]])[result[i]]
    b_dec[i] <- runif(length(i)) < plogis(e + dc[["f"]] * (2L * b_ga[i] - g[i]))    # b - k = 2b - g
  }
  dec <- c(W = "W", L = "L", OTL = "O")[result]
  data.table(relieved = rel, s_ga = g - b_ga, s_sv = S - b_sv, b_ga = b_ga, b_sv = b_sv,
             s_dec = fifelse(b_dec, NA_character_, unname(dec)), b_dec = fifelse(b_dec, unname(dec), NA_character_))
}

# DK goalie points for a sim_goalies() table, with add_dk_shutout()'s rule.
# ga_all = the opponent's non-shootout goals INCLUDING empty-net goals.
goalie_dk <- function(G, ga_all) {
  n <- nrow(G)
  gg <- data.table(gameId = rep(seq_len(n), 2L), teamId = 1L, played = c(rep(TRUE, n), G$relieved), who = rep(c("s", "b"), each = n))
  tgx <- data.table(gameId = seq_len(n), teamId = 2L, opp_id = 1L, goals = as.integer(ga_all))
  gg <- add_dk_shutout(gg, tgx)
  setorder(gg, who, gameId)                  # "b" rows first, then "s"
  so_b <- gg[who == "b", dk_shutout]; so_s <- gg[who == "s", dk_shutout]
  G[, `:=`(s_so = so_s, b_so = so_b,
           s_dk = dk_goalie_points(s_dec, s_sv, s_ga, so_s),
           b_dk = fifelse(relieved, dk_goalie_points(b_dec, b_sv, b_ga, so_b), 0))]
  G
}
