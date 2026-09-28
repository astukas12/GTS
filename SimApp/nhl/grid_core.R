# =============================================================================
# grid_core.R -- the regulation score grid's functions (P2_DESIGN.md section 1)
# -----------------------------------------------------------------------------
# Shared by R/fit/01_grid.R (which fits theta) and every later step that needs a
# game's lam_h, lam_a from its closing line (02_goal_classes, 03_ot_so, and
# eventually team_model.R). Definitions only; sourcing it runs nothing.
#
#   G <- readRDS("params/grid.rds")
#   x <- solve_lam(d, G$params$theta_fit, G$params$spec)   # d: p_home, p_over, total
#   lam_h <- exp(x[, 1]); lam_a <- exp(x[, 2])
# =============================================================================

K  <- 15L
cc <- CJ(h = 0:K, a = 0:K)
Hc <- cc$h; Ac <- cc$a; Mc <- Hc - Ac; NC <- nrow(cc)
Fc <- Hc + Ac + (Mc == 0)                 # full-game total
i00 <- which(Hc == 0 & Ac == 0); i01 <- which(Hc == 0 & Ac == 1)
i10 <- which(Hc == 1 & Ac == 0); i11 <- which(Hc == 1 & Ac == 1)

# A spec is a list: mcap (top margin bucket, 4 = "4+"), and which of the
# optional parameters are free. theta is a named vector.
spec_par <- function(sp) {
  nm <- c(if (sp$m0) "lm0", if (sp$mcap >= 2 && sp$mshape) paste0("lm", 2:sp$mcap),
          if (sp$m0tot) "b_m0tot", if (sp$dc) "rho", "a0", if (sp$qslope) "a1")
  setNames(rep(0, length(nm)), nm)
}
tget <- function(th, nm) if (nm %in% names(th)) th[[nm]] else 0

grid_P <- function(lh, la, th, sp) {
  dh <- outer(lh, 0:K, function(l, k) dpois(k, l))
  da <- outer(la, 0:K, function(l, k) dpois(k, l))
  P  <- dh[, Hc + 1L, drop = FALSE] * da[, Ac + 1L, drop = FALSE]
  if (sp$mshape || sp$m0) {
    b  <- pmin(abs(Mc), sp$mcap)
    lm <- vapply(0:sp$mcap, function(k) if (k == 1) 0 else tget(th, paste0("lm", k)), 0)
    P  <- P * rep(exp(lm[b + 1L]), each = length(lh))
  }
  if (sp$m0tot) P[, Mc == 0] <- P[, Mc == 0] * exp(th[["b_m0tot"]] * (lh + la - 6))
  if (sp$dc) {
    r <- th[["rho"]]
    P[, i00] <- P[, i00] * pmax(1 - lh * la * r, 1e-9); P[, i01] <- P[, i01] * pmax(1 + lh * r, 1e-9)
    P[, i10] <- P[, i10] * pmax(1 + la * r, 1e-9);       P[, i11] <- P[, i11] * (1 - r)
  }
  P / rowSums(P)
}
q_tie <- function(lh, la, th) plogis(th[["a0"]] + tget(th, "a1") * log(lh / la))

p_over_model <- function(P, L) {
  out <- numeric(nrow(P))
  for (l in unique(L)) {
    w <- L == l
    ov <- P[w, , drop = FALSE] %*% (Fc > l)
    if (l == round(l)) ov <- ov / (1 - P[w, , drop = FALSE] %*% (Fc == l))   # push voids
    out[w] <- ov
  }
  out
}

# Solve (log lam_h, log lam_a) per game to the 2-way ML and the total, vectorised.
solve_lam <- function(d, th, sp, x0 = NULL) {
  G <- nrow(d)
  if (is.null(x0)) {
    mu <- d$total + 0.5 - 0.2 + qnorm(d$p_over) * sqrt(d$total)
    s  <- 0.35 * qlogis(d$p_home)
    x0 <- cbind(log(mu / 2) + s, log(mu / 2) - s)
  }
  res <- function(x) {
    lh <- exp(x[, 1]); la <- exp(x[, 2]); P <- grid_P(lh, la, th, sp)
    cbind(drop(P %*% (Mc > 0)) + drop(P %*% (Mc == 0)) * q_tie(lh, la, th) - d$p_home,
          p_over_model(P, d$total) - d$p_over)
  }
  x <- x0
  for (it in 1:60) {
    r <- res(x)
    if (any(!is.finite(r))) { attr(x, "maxres") <- Inf; return(x) }   # theta too extreme to solve
    if (max(abs(r)) < 1e-11) break
    e <- 1e-6
    r1 <- res(cbind(x[, 1] + e, x[, 2])); r2 <- res(cbind(x[, 1], x[, 2] + e))
    J11 <- (r1[, 1] - r[, 1]) / e; J21 <- (r1[, 2] - r[, 2]) / e
    J12 <- (r2[, 1] - r[, 1]) / e; J22 <- (r2[, 2] - r[, 2]) / e
    det <- J11 * J22 - J12 * J21
    st  <- cbind(( J22 * r[, 1] - J12 * r[, 2]) / det, (-J21 * r[, 1] + J11 * r[, 2]) / det)
    x   <- x - pmax(pmin(st, 0.5), -0.5)
  }
  attr(x, "maxres") <- max(abs(r))
  x
}

cell_of <- function(h, a) (pmin(h, K)) * (K + 1L) + pmin(a, K) + 1L   # CJ order: h outer
