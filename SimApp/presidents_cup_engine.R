# presidents_cup_engine.R -- DK "Cup" match play, simulated in full.
#
# This is the real engine, not a lookup: every sim run plays a whole Presidents
# Cup week, session by session, from the week's inputs.
#
#   Thu   four-ball  5 matches   pinned to the announced pairings and the
#                                de-vigged match odds
#   Fri   foursomes  5           keep -> fill -> pair -> match
#   SatAM four-ball  4           keep -> fill -> pair -> match
#   SatPM foursomes  4           keep -> fill -> pair -> match
#   Sun   singles    12          everyone plays, opponents random
#
# KEEP   a pair from the last same-format session is replayed at a rate set by
#        its result (won 67%, lost 29%; format change 38% / 12%). Fitted on
#        Ryder 2014-2025 + Presidents 2017-2024.
# FILL   the captain picks k of 12 by conditional logit on skill (the gap to
#        teammates), who sat last, who lost last, plus a hidden per-player,
#        per-format trust term. Everyone must play once before Sunday.
# PAIR   the unpaired selected players are matched all at once -- every perfect
#        matching gets probability proportional to exp(sum of pair
#        compatibility): played together this week, past-Cup pairs and their
#        record, same nationality (INTL), rank closeness (USA), plus any tags.
# MATCH  skill sets a points share; a REAL historical scorecard is drawn from
#        the library reweighted to that share, and its DK points are used as-is.
#        Scorecards are never altered, so momentum and collapses are real.
#
# Partners share a result, which is the correlation that matters for lineups.
#
# Inputs: PresidentsCup_Input.xlsx (players / thursday / pairs / settings / tags)
#         data/presidents_cup_fits.rds (the historical fits + scorecard library)
# Built by GTS/Golf/PresidentsCup/R/export_engine_pack.R.
#
# NOTE ON TEAMS: DK's Cup allows all six from one side, so the side is published
# as `Side`, never `Team` -- drop_single_team_sd() keys on `Team` and would
# otherwise throw away legal six-American rosters.

suppressMessages({ library(data.table); library(readxl) })

PC_FITS_FILE <- "data/presidents_cup_fits.rds"

# ---- input ------------------------------------------------------------------

read_presidents_cup_input <- function(file_path) {
  sheets <- readxl::excel_sheets(file_path)
  need <- c("players", "thursday", "settings")
  miss <- setdiff(need, tolower(sheets))
  if (length(miss)) stop("Presidents Cup workbook is missing sheet(s): ", paste(miss, collapse = ", "))
  get1 <- function(nm) {
    hit <- sheets[tolower(sheets) == nm]
    if (!length(hit)) return(data.table())
    as.data.table(readxl::read_excel(file_path, sheet = hit[1]))
  }
  out <- list(players = get1("players"), thursday = get1("thursday"),
              pairs = get1("pairs"), settings = get1("settings"), tags = get1("tags"),
              takes = get1("takes"))
  p <- out$players
  miss <- setdiff(c("Player", "Side", "Salary", "DGSkill"), names(p))
  if (length(miss)) stop("players sheet is missing: ", paste(miss, collapse = ", "))
  if (nrow(p) != 24) stop("Expected 24 players, found ", nrow(p))
  if (!all(table(p$Side) == 12)) stop("Expected 12 players a side; got ",
                                      paste(names(table(p$Side)), table(p$Side), collapse = ", "))
  out
}

pc_setting <- function(settings, key, default) {
  if (!nrow(settings) || !"Key" %in% names(settings)) return(default)
  v <- settings$Value[settings$Key == key]
  if (!length(v) || is.na(v[1])) default else v[1]
}

# ---- small maths shared with the standalone build ---------------------------

pc_log_esp <- function(w, k) {
  e <- c(1, numeric(k))
  for (x in w) e[2:(k + 1)] <- e[2:(k + 1)] + x * e[1:k]
  log(e[k + 1])
}

# Draw a k-subset with P(S) proportional to prod(w[S]), exactly.
pc_draw_subset <- function(w, k) {
  n <- length(w); E <- matrix(0, n + 2, k + 1); E[n + 1, 1] <- 1
  for (i in n:1) { E[i, 1] <- 1
    for (j in 1:k) E[i, j + 1] <- E[i + 1, j + 1] + w[i] * E[i + 1, j] }
  pick <- logical(n); need <- k
  for (i in seq_len(n)) {
    if (need == 0) break
    if (runif(1) < w[i] * E[i + 1, need] / E[i, need + 1]) { pick[i] <- TRUE; need <- need - 1 }
  }
  pick
}

pc_all_matchings <- function(n) {
  rec <- function(v) {
    if (!length(v)) return(list(matrix(integer(0), 0, 2)))
    a <- v[1]; out <- list()
    for (b in v[-1]) for (r in rec(setdiff(v[-1], b))) out[[length(out) + 1]] <- rbind(c(a, b), r)
    out
  }
  rec(seq_len(n))
}
PC_MATCHINGS <- local({
  m <- lapply(setNames(seq(2, 10, 2), seq(2, 10, 2)), function(n) {
    n <- as.integer(n)
    matrix(t(vapply(pc_all_matchings(n), function(M) (M[, 2] - 1L) * n + M[, 1], integer(n / 2))),
           ncol = n / 2)
  })
  m[["0"]] <- matrix(integer(0), 1, 0)
  m
})

pc_draw_matching <- function(U) {
  n <- nrow(U); if (n == 0) return(matrix(integer(0), 0, 2))
  M <- PC_MATCHINGS[[as.character(n)]]
  sc <- rowSums(matrix(U[as.vector(M)], nrow(M)))
  if (all(!is.finite(sc))) sc <- rowSums(matrix(pmax(U[as.vector(M)], -50), nrow(M)))
  p <- exp(sc - max(sc)); j <- sample.int(nrow(M), 1, prob = p)
  cbind((M[j, ] - 1L) %% n + 1L, (M[j, ] - 1L) %/% n + 1L)
}

# Stroke model: a skill gap -> an expected points share, one scale per format.
PC_SUPPORT <- -3:4
pc_hole_pmf <- function(skill, sigma) {
  mu <- -skill / 18
  p <- diff(c(0, pnorm(head(PC_SUPPORT, -1) + 0.5, mu, sigma), 1))
  p / sum(p)
}
pc_bestball <- function(p1, p2) {
  S1 <- rev(cumsum(rev(p1))); S2 <- rev(cumsum(rev(p2)))
  surv <- S1 * S2; diff(c(surv, 0)) * -1
}
pc_hole_probs <- function(pa, pb) {
  Fb <- cumsum(pb)
  win <- sum(pa * (1 - Fb)); tie <- sum(pa * pb)
  c(win = win, tie = tie, loss = 1 - win - tie)
}
pc_match_dp <- function(hp) {
  pw <- hp[["win"]]; pt <- hp[["tie"]]; pl <- hp[["loss"]]
  v <- c(1); off <- 1; W <- L <- 0
  for (h in 1:18) {
    n <- length(v); nv <- numeric(n + 2)
    nv[1:n] <- nv[1:n] + v * pl; nv[2:(n + 1)] <- nv[2:(n + 1)] + v * pt
    nv[3:(n + 2)] <- nv[3:(n + 2)] + v * pw
    off <- off + 1; v <- nv
    d <- seq_along(v) - off; left <- 18 - h
    W <- W + sum(v[d > left]); L <- L + sum(v[d < -left]); v[abs(d) > left] <- 0
  }
  d <- seq_along(v) - off
  win <- W + sum(v[d > 0]); halve <- sum(v[d == 0])
  win + halve / 2
}
pc_side_pmf <- function(fm, skills, sigma) {
  if (fm == "singles")   return(pc_hole_pmf(skills[1], sigma))
  if (fm == "foursomes") return(pc_hole_pmf(mean(skills), sigma))
  pc_bestball(pc_hole_pmf(skills[1], sigma), pc_hole_pmf(skills[2], sigma))
}
pc_share <- function(fm, sa, sb, sigma, home = 0)
  pc_match_dp(pc_hole_probs(pc_side_pmf(fm, sa + home, sigma), pc_side_pmf(fm, sb, sigma)))

# All pair-vs-pair shares up front: 66 x 66 per pairs format, 12 x 12 singles.
pc_ml_tables <- function(fits, skill, side, scale, home) {
  us <- which(side == side[1]); it <- which(side != side[1])
  pr <- which(upper.tri(matrix(0, 12, 12)), arr.ind = TRUE)
  out <- list()
  for (fm in c("fourball", "foursomes")) {
    su <- skill[us] * scale[[fm]]; si <- skill[it] * scale[[fm]]
    A <- array(NA_real_, c(12, 12, 12, 12))
    for (a in seq_len(nrow(pr))) for (b in seq_len(nrow(pr))) {
      i <- pr[a, 1]; j <- pr[a, 2]; k <- pr[b, 1]; l <- pr[b, 2]
      v <- pc_share(fm, su[c(i, j)], si[c(k, l)], fits$sigma[[fm]], home)
      A[i, j, k, l] <- A[j, i, k, l] <- A[i, j, l, k] <- A[j, i, l, k] <- v
    }
    out[[fm]] <- A
  }
  su <- skill[us] * scale[["singles"]]; si <- skill[it] * scale[["singles"]]
  out$singles <- outer(seq_len(12), seq_len(12), Vectorize(function(i, k)
    pc_share("singles", su[i], si[k], fits$sigma[["singles"]], home)))
  out
}

# Draw library rows for a vector of points shares.
pc_draw_cards <- function(fits, fm, share) {
  C <- fits$grid[[fm]]
  g <- pmin(pmax(round(share * 100), 1), 99)
  u <- runif(length(share)); idx <- integer(length(share))
  for (gi in unique(g)) { k <- which(g == gi); idx[k] <- findInterval(u[k], C[gi, ]) + 1L }
  pmin(idx, ncol(C))
}

# A tie-voided two-way price is P(win | decisive); convert to a points share.
pc_two_way_share <- function(d, q, phi = 0, iters = 25) {
  norm <- function(u) { w <- exp(u - max(u)); w / sum(w) }
  sh <- q
  for (i in seq_len(iters)) {
    y <- (d > 0) + 0.5 * (d == 0); off <- phi * (d == 0)
    t <- uniroot(function(t) sum(norm(t * d + off) * y) - sh, c(-6, 6), tol = 1e-10)$root
    w <- norm(t * d + off); h <- sum(w * (d == 0))
    new <- q * (1 - h) + h / 2
    if (abs(new - sh) < 1e-10) break
    sh <- new
  }
  sh
}
pc_tilt <- function(d, share, phi = 0) {
  norm <- function(u) { w <- exp(u - max(u)); w / sum(w) }
  y <- (d > 0) + 0.5 * (d == 0); off <- phi * (d == 0)
  t <- uniroot(function(t) sum(norm(t * d + off) * y) - share, c(-6, 6), tol = 1e-10)$root
  norm(t * d + off)
}

# ---- one team's session: keep -> fill -> pair --------------------------------

pc_keep_sources <- function(s, formats) {
  if (s == 1) return(list())
  same <- which(formats[seq_len(s - 1)] == formats[s])
  out <- list()
  if (length(same)) out$same <- max(same)
  if (is.null(out$same) || out$same != s - 1) out$change <- s - 1
  out
}

pc_new_state <- function(static_u, tau, pairU) {
  n <- length(static_u)
  list(n = n, static = static_u, pairU = pairU,
       zf = list(fourball = tau * rnorm(n), foursomes = tau * rnorm(n)),
       sat = rep(FALSE, n), lost = rep(FALSE, n), halved = rep(FALSE, n),
       nplayed = integer(n), pairs = list(), res = list(), together = matrix(0L, n, n))
}

pc_team_session <- function(st, s, k, kr, b, tw_coef, formats, forced = NULL) {
  n <- st$n
  u <- st$static + st$zf[[formats[s]]] + b[["sat_last"]] * st$sat +
       b[["lost_last"]] * st$lost + b[["halved_last"]] * st$halved
  if (!is.null(forced)) {
    sel <- rep(FALSE, n); sel[forced] <- TRUE
    return(list(pairs = forced, lw = sum(u[sel]) - pc_log_esp(exp(u), sum(sel))))
  }
  src <- pc_keep_sources(s, formats)
  locked <- integer(0); kept <- matrix(integer(0), 0, 2); veto <- matrix(FALSE, n, n)
  for (rel in c("same", "change")) {
    ss <- src[[rel]]; if (is.null(ss)) next
    P <- st$pairs[[ss]]; R <- st$res[[ss]]
    for (q in seq_len(nrow(P))) {
      pq <- P[q, ]
      if (any(pq %in% locked)) next
      if (runif(1) < kr[[paste(rel, R[q])]]) { kept <- rbind(kept, pq); locked <- c(locked, pq) }
      else veto[pq[1], pq[2]] <- veto[pq[2], pq[1]] <- TRUE
    }
  }
  must <- if (s == length(formats)) setdiff(which(st$nplayed == 0), locked) else integer(0)
  while (nrow(kept) && 2 * nrow(kept) + length(must) > k) {
    drop <- sample.int(nrow(kept), 1); locked <- setdiff(locked, kept[drop, ])
    kept <- kept[-drop, , drop = FALSE]
  }
  if (length(must) > k) must <- must[order(-u[must])][seq_len(k)]
  need <- k - 2 * nrow(kept) - length(must)
  pool <- setdiff(seq_len(n), c(locked, must))
  pick <- if (need > 0) pool[pc_draw_subset(exp(u[pool]), need)] else integer(0)
  newc <- c(must, pick)
  if (length(newc)) {
    U <- st$pairU[newc, newc, drop = FALSE]
    tw <- st$together[newc, newc, drop = FALSE]
    U <- U + tw_coef * (tw > 0 & !(tw %in% unlist(src)))
    U[veto[newc, newc, drop = FALSE]] <- -Inf
    pairs <- rbind(kept, matrix(newc[pc_draw_matching(U)], ncol = 2))
  } else pairs <- kept
  list(pairs = unname(pairs), lw = 0)
}

pc_team_update <- function(st, s, pairs, res) {
  pl <- as.vector(pairs)
  st$sat <- !(seq_len(st$n) %in% pl)
  rr <- rep(NA_character_, st$n); rr[pairs[, 1]] <- res; rr[pairs[, 2]] <- res
  st$lost <- !st$sat & rr == "L"; st$halved <- !st$sat & rr == "H"
  st$lost[is.na(st$lost)] <- FALSE; st$halved[is.na(st$halved)] <- FALSE
  st$nplayed[pl] <- st$nplayed[pl] + 1L
  st$pairs[[s]] <- pairs; st$res[[s]] <- res
  st$together[pairs] <- s; st$together[pairs[, 2:1, drop = FALSE]] <- s
  st
}

# ---- the simulation ----------------------------------------------------------

run_presidents_cup_simulation <- function(input_data, n_sims = 25000, config = NULL,
                                          progress_callback = NULL) {
  pr <- function(v, m) if (!is.null(progress_callback)) progress_callback(m, v)
  t0 <- Sys.time()
  fits_path <- if (file.exists(PC_FITS_FILE)) PC_FITS_FILE else file.path("SimApp", PC_FITS_FILE)
  if (!file.exists(fits_path))
    stop("Missing ", PC_FITS_FILE, " -- the Presidents Cup fits ship with the app.")
  fits <- readRDS(fits_path)
  pr(0.02, "Loading the scorecard library...")

  P <- copy(input_data$players)
  # USA first, because the thursday sheet names its columns usa_* / intl_* and
  # the match tables below index side 1 against side 2.
  sides <- unique(P$Side)
  if (length(sides) != 2) stop("Expected two sides, found: ", paste(sides, collapse = ", "))
  if ("USA" %in% sides) sides <- c("USA", setdiff(sides, "USA"))
  P[, Side := factor(Side, levels = sides)]
  setorder(P, Side, -DGSkill)
  P[, Side := as.character(Side)]
  P[, idx := seq_len(.N), by = Side]
  us_names <- P[Side == sides[1]]$Player; it_names <- P[Side == sides[2]]$Player
  n_players <- nrow(P)

  st <- input_data$settings
  scale <- list(fourball  = as.numeric(pc_setting(st, "scale_fourball", 0.79)),
                foursomes = as.numeric(pc_setting(st, "scale_foursomes", 0.88)),
                singles   = as.numeric(pc_setting(st, "scale_singles", 0.88)))
  home_sg <- as.numeric(pc_setting(st, "home_sg", 0))
  K   <- as.integer(strsplit(pc_setting(st, "session_sizes", "10,10,8,8"), ",")[[1]])
  FMT <- strsplit(pc_setting(st, "formats", "fourball,foursomes,fourball,foursomes,singles"), ",")[[1]]
  n_team_sessions <- length(K)
  labels <- c("Thu", "Fri", "SatAM", "SatPM", "Sun")[seq_len(n_team_sessions + 1)]

  b   <- fits$sel$b; tau <- fits$sel$tau
  kr  <- setNames(as.list(fits$keep$rate), paste(fits$keep$rel, fits$keep$res))
  cf  <- fits$compat
  ml  <- pc_ml_tables(fits, P$DGSkill, P$Side, scale, home_sg)
  lib <- split(fits$lib, fits$lib$format)
  pr(0.10, "Built the matchup table...")

  # static selection utility: skill gap to teammates, plus the trust offset
  # Playing time = the market anchor in `Trust` plus any hand takes on the
  # `takes` sheet, both in captain-trust units (+ plays more). Team sessions are
  # a fixed number of slots, so takes are re-centred inside each side: talking
  # one player up necessarily talks his team-mates down.
  trust <- if ("Trust" %in% names(P)) ifelse(is.na(P$Trust), 0, P$Trust) else rep(0, n_players)
  tk <- input_data$takes
  if (!is.null(tk) && nrow(tk) && all(c("Player", "Bump") %in% names(tk))) {
    bad <- setdiff(tk$Player, P$Player)
    if (length(bad)) stop("takes sheet names not in the players sheet: ", paste(bad, collapse = ", "))
    add <- numeric(n_players)
    add[match(tk$Player, P$Player)] <- as.numeric(tk$Bump)
    for (sd_nm in unique(P$Side)) { i <- P$Side == sd_nm; add[i] <- add[i] - mean(add[i]) }
    trust <- trust + add
    cat(sprintf("Presidents Cup: %d hand takes applied (%s)
", nrow(tk),
                paste(sprintf("%s %+.2f", tk$Player, tk$Bump), collapse = ", ")))
  }
  su_of <- function(side) {
    i <- P$Side == side
    (b[["skill"]] + b[["skill_pres"]]) * P$DGSkill[i] + trust[i]
  }
  stat <- list(su_of(sides[1]), su_of(sides[2])); names(stat) <- sides

  # static pair compatibility, per side
  pkey <- function(a, b) paste(pmin(a, b), pmax(a, b), sep = "|")
  pastdt <- input_data$pairs
  tags <- input_data$tags
  pairU <- lapply(sides, function(side) {
    nm <- P[Side == side]$Player; n <- length(nm)
    U <- matrix(0, n, n); K2 <- outer(nm, nm, pkey)
    if (nrow(pastdt) && all(c("Player1", "Player2") %in% names(pastdt))) {
      pk <- pkey(pastdt$Player1, pastdt$Player2)
      h <- match(K2, pk); ok <- !is.na(h)
      np <- numeric(length(K2)); rec <- numeric(length(K2))
      np[ok]  <- log1p(pastdt$Sessions[h[ok]])
      rec[ok] <- (pastdt$W[h[ok]] - pastdt$L[h[ok]]) / (pastdt$Sessions[h[ok]] + 1)
      U <- U + cf[["past"]] * matrix(np, n) + cf[["prec"]] * matrix(rec, n)
    }
    if ("Country" %in% names(P)) {
      ct <- P[Side == side]$Country
      same <- outer(ct, ct, "==") & !is.na(outer(ct, ct, function(a, b) paste(a, b)))
      same[is.na(same)] <- FALSE
      if (side == sides[2]) U <- U + cf[["nat"]] * same
    }
    if (side == sides[1]) U <- U + cf[["rgap"]] * abs(outer(seq_len(n), seq_len(n), "-")) / 3.45
    if (nrow(tags) && all(c("player1", "player2", "tag") %in% tolower(names(tags)))) {
      setnames(tags, tolower(names(tags)))
      tk <- pkey(tags$player1, tags$player2)
      bump <- ifelse(tags$tag == "veto", -Inf, ifelse(tags$tag == "likely", 1.5, 0.7))
      h <- match(K2, tk); ok <- !is.na(h)
      add <- numeric(length(K2)); add[ok] <- bump[h[ok]]
      U <- U + matrix(add, n)
    }
    U
  })
  names(pairU) <- sides

  # Thursday: pinned pairings, and the de-vigged line where one was posted
  thu <- input_data$thursday
  pinned <- nrow(thu) == K[1] / 2
  if (pinned) {
    cn <- tolower(names(thu)); setnames(thu, cn)
    thuU <- cbind(match(thu$usa_p1, us_names), match(thu$usa_p2, us_names))
    thuI <- cbind(match(thu$intl_p1, it_names), match(thu$intl_p2, it_names))
    if (anyNA(thuU) || anyNA(thuI)) stop("thursday sheet names do not match the players sheet")
    amp <- function(o) ifelse(is.na(o), NA_real_, ifelse(o > 0, 100 / (o + 100), -o / (-o + 100)))
    pu <- amp(thu$usa_odds); pi <- amp(thu$intl_odds)
    pt <- if ("tie_odds" %in% names(thu)) amp(thu$tie_odds) else rep(NA_real_, nrow(thu))
    d4 <- lib$fourball$d; ph <- fits$phi[["fourball"]]
    thu_w <- lapply(seq_len(nrow(thu)), function(q) {
      if (is.na(pu[q]) || is.na(pi[q]))
        return(pc_tilt(d4, ml$fourball[thuU[q, 1], thuU[q, 2], thuI[q, 1], thuI[q, 2]], ph))
      if (is.na(pt[q])) {
        tot <- pu[q] + pi[q]
        pc_tilt(d4, pc_two_way_share(d4, pu[q] / tot, ph), ph)
      } else {
        tot <- pu[q] + pi[q] + pt[q]
        pc_tilt(d4, pu[q] / tot + (pt[q] / tot) / 2, ph)
      }
    })
  }
  pr(0.15, if (pinned) "Thursday pinned to the announced pairings" else "Thursday simulated")

  n_sims <- as.integer(n_sims)
  S <- n_team_sessions + 1L
  pts <- array(0, c(n_sims, n_players, S))
  played <- array(FALSE, c(n_sims, n_players, S))
  partner <- opp1 <- opp2 <- array(0L, c(n_sims, n_players, S))
  resa <- array(NA_integer_, c(n_sims, n_players, S))
  lw <- numeric(n_sims)
  half <- n_players / 2L
  res_code <- function(d) ifelse(d > 0, "W", ifelse(d < 0, "L", "H"))
  report_every <- max(1L, n_sims %/% 20L)

  for (r in seq_len(n_sims)) {
    sU <- pc_new_state(stat[[1]], tau, pairU[[1]])
    sI <- pc_new_state(stat[[2]], tau, pairU[[2]])
    for (s in seq_len(n_team_sessions)) {
      aU <- pc_team_session(sU, s, K[s], kr, b, cf[["tw"]], FMT[seq_len(n_team_sessions)],
                            forced = if (pinned && s == 1) thuU)
      aI <- pc_team_session(sI, s, K[s], kr, b, cf[["tw"]], FMT[seq_len(n_team_sessions)],
                            forced = if (pinned && s == 1) thuI)
      lw[r] <- lw[r] + aU$lw + aI$lw
      PU <- aU$pairs; PI <- aI$pairs
      if (!(pinned && s == 1)) PI <- PI[sample.int(nrow(PI)), , drop = FALSE]
      fm <- FMT[s]; L <- lib[[fm]]
      idx <- if (pinned && s == 1) {
        vapply(seq_len(nrow(PU)), function(q) sample.int(nrow(L), 1, prob = thu_w[[q]]), 1L)
      } else {
        pc_draw_cards(fits, fm, ml[[fm]][cbind(PU[, 1], PU[, 2], PI[, 1], PI[, 2])])
      }
      d <- L$d[idx]
      gU <- as.vector(PU); gI <- as.vector(PI) + half
      pts[r, gU, s] <- rep(L$pts[idx], 2); pts[r, gI, s] <- rep(L$opp_pts[idx], 2)
      resa[r, gU, s] <- rep(sign(d), 2); resa[r, gI, s] <- rep(-sign(d), 2)
      played[r, c(gU, gI), s] <- TRUE
      partner[r, gU, s] <- c(PU[, 2], PU[, 1]); partner[r, gI, s] <- c(PI[, 2], PI[, 1]) + half
      opp1[r, gU, s] <- rep(PI[, 1] + half, 2); opp2[r, gU, s] <- rep(PI[, 2] + half, 2)
      opp1[r, gI, s] <- rep(PU[, 1], 2); opp2[r, gI, s] <- rep(PU[, 2], 2)
      sU <- pc_team_update(sU, s, PU, res_code(d))
      sI <- pc_team_update(sI, s, PI, res_code(-d))
    }
    oi <- sample.int(half)
    idx <- pc_draw_cards(fits, "singles", ml$singles[cbind(seq_len(half), oi)])
    L <- lib$singles; d <- L$d[idx]
    pts[r, seq_len(half), S] <- L$pts[idx]; pts[r, oi + half, S] <- L$opp_pts[idx]
    resa[r, seq_len(half), S] <- sign(d); resa[r, oi + half, S] <- -sign(d)
    played[r, , S] <- TRUE
    opp1[r, seq_len(half), S] <- oi + half; opp1[r, oi + half, S] <- seq_len(half)
    if (r %% report_every == 0)
      pr(0.15 + 0.75 * r / n_sims, sprintf("Simulating week %s of %s...",
                                           format(r, big.mark = ","), format(n_sims, big.mark = ",")))
  }

  # A pinned Thursday is information about captain trust: weight each simulated
  # week by how likely that selection was under its own trust draw, then resample.
  ess <- NA_real_
  if (pinned) {
    w <- exp(lw - max(lw)); ess <- sum(w)^2 / sum(w^2)
    keep <- sample.int(n_sims, n_sims, replace = TRUE, prob = w)
    pts <- pts[keep, , , drop = FALSE]; played <- played[keep, , , drop = FALSE]
    partner <- partner[keep, , , drop = FALSE]; resa <- resa[keep, , , drop = FALSE]
    opp1 <- opp1[keep, , , drop = FALSE]; opp2 <- opp2[keep, , , drop = FALSE]
  }
  pr(0.93, "Scoring...")

  tot <- apply(pts, c(1, 2), sum)
  sim_results <- data.table(
    SimID   = rep(seq_len(n_sims), times = n_players),
    Player  = rep(P$Player, each = n_sims),
    DKScore = as.numeric(tot))

  nm <- apply(played, c(1, 2), sum)
  gv <- function(col, default) if (col %in% names(P)) P[[col]] else rep(default, n_players)
  # DKID is the GOLFER-slot draftable id, CPTID the CAPTAIN-slot one. DK issues
  # both and create_download_showdown() picks CPTID for the captain; a single id
  # (or DK's playerId) uploads as an invalid entry.
  metadata <- data.table(
    Player    = P$Player,
    DKSalary  = as.numeric(P$Salary),
    DKID      = as.character(gv("DKID", "")),
    CPTID     = as.character(gv("CPTID", "")),
    CPTSalary = as.numeric(gv("CPTSalary", NA)),
    DKOwn     = as.numeric(gv("Own", 0)),
    Side      = P$Side,
    E_Matches = round(colMeans(nm), 2))
  metadata[is.na(DKOwn), DKOwn := 0]
  for (s in seq_len(n_team_sessions))
    metadata[[paste0("P_", labels[s])]] <- round(colMeans(played[, , s]), 3)

  projections <- data.table(Player = P$Player, Side = P$Side, Salary = as.numeric(P$Salary),
                            Proj = round(colMeans(tot), 2), SD = round(apply(tot, 2, sd), 2),
                            E_Matches = metadata$E_Matches,
                            CupPts = round(colMeans(apply((resa == 1) + 0.5 * (resa == 0), c(1, 2),
                                                          sum, na.rm = TRUE)), 2))
  setorder(projections, -Proj)

  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("Presidents Cup: %s weeks in %.0fs%s\n", format(n_sims, big.mark = ","), secs,
              if (pinned) sprintf(" (Thursday pinned, importance ESS %.0f)", ess) else ""))
  pr(1, "Done")

  list(sim_results = sim_results, metadata = metadata, projections = projections,
       has_fd = FALSE, has_sd = TRUE, has_classic = FALSE)
}
