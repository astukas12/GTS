# ============================================================================
# LINEUP RULES — one rulebook per sport and format
# Golden Ticket Sims
#
# What makes a DK/FD lineup legal: roster slots and who may fill them, the
# salary cap, and the team / game rules. Written once, read by both sides:
#
#   * the tournament filters in app.R (drop_invalid_classic,
#     drop_single_team_sd, nhl_drop_invalid_classic) drop what fails it
#   * the Cash tab's field sampler (cash_game_module.R) only keeps what passes
#
# Before 29 Sep 2026 each side carried its own copy of the rules and the cash
# field had drifted: CFB, NHL, CBB and Soccer classic fields were drawn with no
# positions at all, and F1's field could captain a constructor.
#
# The optimisers still build their own rules into their solvers (rewriting the
# exact solvers is not needed for this) -- this file is the check both sides
# answer to.
#
#   R <- lineup_rules(config, metadata, platform = "DK", format = "classic")
#   ok <- lineup_legal(M, R)      # M: character matrix, one lineup per row,
#                                 # fixed slots (CPT, A-CPT, ...) first
# ============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' First candidate column present in `nm`, else NULL.
.lr_pick <- function(nm, cands) { h <- cands[cands %in% nm]; if (length(h)) h[1] else NULL }

#' Player -> numeric lookup from a metadata column (NULL if the column is absent).
.lr_map <- function(meta, col) {
  if (is.null(col) || !col %in% names(meta)) return(NULL)
  setNames(suppressWarnings(as.numeric(meta[[col]])), meta$Player)
}

#' Player -> character vector of positions, split on "/" (NBA "PG/SG", Soccer "D/M").
.lr_pos <- function(meta, col) {
  if (is.null(col) || !col %in% names(meta)) return(NULL)
  v <- as.character(meta[[col]]); v[is.na(v)] <- ""
  setNames(strsplit(v, "/", fixed = TRUE), meta$Player)
}

#' Classic slot groups, as list(list(eligible positions or NULL for any, count)).
.lr_classic_slots <- function(sport, platform, config, roster_size) {
  g <- function(pos, n) list(pos, as.integer(n))
  ps <- config$position_slots
  switch(sport,
    NFL_CLASSIC = , NFL_PRESEASON_CLASSIC = , CFB_CLASSIC =
      lapply(names(ps), function(s)
        g(switch(s, FLEX = config$flex_eligible, SFLEX = config$sflex_eligible, s), ps[[s]])),
    NHL = list(g("C", 2), g("W", 3), g("D", 2), g("G", 1), g(c("C", "W", "D"), 1)),
    NBA = if (platform == "FD")
            list(g("PG", 2), g("SG", 2), g("SF", 2), g("PF", 2), g("C", 1))
          else
            list(g("PG", 1), g("SG", 1), g("SF", 1), g("PF", 1), g("C", 1),
                 g(c("PG", "SG"), 1), g(c("SF", "PF"), 1), g(NULL, 1)),
    CBB = if (platform == "FD") list(g("G", 4), g("F", 3), g(NULL, 1))
          else                  list(g("G", 3), g("F", 3), g(NULL, 2)),
    SOCCER = list(g("GK", 1), g("D", 2), g("M", 2), g("F", 2), g(c("D", "M", "F"), 1)),
    list(g(NULL, roster_size)))     # NASCAR, Golf, MMA, Tennis: any N under the cap
}

#' Classic position column for a sport / platform.
.lr_classic_pos_col <- function(sport, platform, nm) {
  switch(sport,
    NBA    = if (platform == "FD") "FDPos" else "DKPos",
    CBB    = if (platform == "FD") .lr_pick(nm, c("FDPosGroup", "PosGroup")) else "PosGroup",
    SOCCER = .lr_pick(nm, c("ClassicPos", "DKPos")),
    NFL_CLASSIC = , NFL_PRESEASON_CLASSIC = , CFB_CLASSIC = , NHL = "Pos",
    NULL)
}

#' Build the rulebook.
#'
#' @param config    sport config (rv$config)
#' @param metadata  player metadata for the slate / slice being checked
#' @param platform  "DK", "FD" or "SD"
#' @param format    "classic", "captain" (CPT + N), "tennis_captain"
#'                  (CPT, A-CPT, P) or "f1" (CPT driver, 4 drivers, constructor).
#'                  NULL picks it from the sport and platform.
#' @param games     optional games table (HomeTeam / AwayTeam) for the game rule
lineup_rules <- function(config, metadata, platform = "DK", format = NULL,
                         games = NULL) {
  meta  <- unique(as.data.table(metadata), by = "Player")
  nm    <- names(meta)
  sport <- config$sport_name %||% ""
  if (is.null(format)) {
    mode <- config$optimization_modes[[platform]] %||% ""
    format <- if (sport == "F1") "f1"
              else if (mode == "enum_tennis_captain") "tennis_captain"
              else if (platform == "SD" || mode %in% c("enum_captain", "combinatorial_captain", "captain")) "captain"
              else "classic"
  }
  pc  <- (config$platform_columns %||% config$platform_cols %||% list())
  pcp <- pc[[platform]] %||% pc$DK %||% list()
  sal_col <- .lr_pick(nm, c(pcp$salary, paste0(platform, "Salary"), "DKSalary", "Salary"))
  roster  <- as.integer(config$roster_sizes[[platform]] %||% config$roster_sizes$DK %||% 6L)

  R <- list(format = format, sport = sport, platform = platform,
            sal_cap = as.numeric(config$salary_caps[[platform]] %||% config$salary_caps$DK %||% 50000),
            sal = .lr_map(meta, sal_col), fixed = list(), slots = NULL, pos = NULL,
            team = if ("Team" %in% nm) setNames(as.character(meta$Team), meta$Player) else NULL,
            game = NULL, min_teams = 0L, min_games = 0L, max_games = NA_integer_,
            max_per_team = NA_integer_, team_skip_pos = NULL,
            max_per_game = NA_integer_, f1_stack = FALSE, type = NULL)

  # Game of each player: the games table where there is one (what
  # drop_invalid_classic always used), else the metadata's GameKey.
  if (!is.null(games) && nrow(games) && all(c("HomeTeam", "AwayTeam") %in% names(games)) &&
      !is.null(R$team)) {
    gk <- paste0(games$AwayTeam, "@", games$HomeTeam)
    t2g <- setNames(rep(gk, 2), c(games$HomeTeam, games$AwayTeam))
    R$game <- setNames(unname(t2g[R$team]), names(R$team))
  } else if ("GameKey" %in% nm) {
    R$game <- setNames(as.character(meta$GameKey), meta$Player)
  }
  n_teams <- if (is.null(R$team)) 0L else uniqueN(na.omit(R$team))
  n_games <- if (is.null(R$game)) 0L else uniqueN(na.omit(R$game))

  if (format == "classic") {
    pos_col <- .lr_classic_pos_col(sport, platform, nm)
    R$pos   <- .lr_pos(meta, pos_col)
    R$slots <- .lr_classic_slots(sport, platform, config, roster)
    if (is.null(R$pos)) R$slots <- list(list(NULL, roster))     # no position column: count only
    switch(sport,
      NFL_CLASSIC = , NFL_PRESEASON_CLASSIC = , CFB_CLASSIC = { R$min_teams <- 2L; R$min_games <- 2L },
      NHL    = { R$min_teams <- 3L; R$min_games <- 2L
                 R$team_skip_pos <- "G" },     # DK: 3 teams among the SKATERS; the goalie doesn't count
      NBA    = if (platform != "FD" && n_games >= 2L) R$max_per_game <- 7L,
      SOCCER = { if (n_games >= 2L) R$max_per_game <- 7L        # as the soccer LP: only with 2+ games,
                 if (n_teams >= 3L) { R$max_per_team <- 5L; R$min_teams <- 3L } },   # 3+ teams
      NULL)

  } else if (format == "captain") {
    sd  <- config$showdown_config[[platform]] %||% config$showdown_config$DK %||% list()
    cm  <- as.numeric(pcp$cpt_multiplier %||% sd$captain_multiplier %||% config$captain_multiplier %||% 1.5)
    flex_col <- .lr_pick(nm, c(pcp$salary, "SDSalary", "DKSalary", "Salary"))
    R$sal <- .lr_map(meta, flex_col)
    # FD's MVP costs mvp_salary_multiplier x salary (1.0 on MMA, 1.5 on NFL);
    # DK's captain carries its own salary column, else 1.5x.
    sm <- as.numeric(if (platform == "FD") sd$mvp_salary_multiplier %||% cm
                     else sd$captain_salary_multiplier %||% cm)
    cpt_col <- if (platform == "FD") NULL
               else .lr_pick(nm, c(pcp$cpt_salary, "CPTSalary", "DKCSalary", "SDCSalary"))
    cs <- .lr_map(meta, cpt_col) %||% (R$sal * sm)
    cs[is.na(cs) | cs <= 0] <- (R$sal * sm)[is.na(cs) | cs <= 0]
    R$fixed <- list(list(name = if (platform == "FD") "MVP" else "Captain", elig = NULL, sal = cs))
    R$slots <- list(list(NULL, roster - 1L))
    # Same guard as the combinatorial_captain optimiser: a slate that is
    # effectively one team (MMA fights, a one-team slice) has no team rule.
    if (n_teams >= 2L) R$min_teams <- 2L
    # A showdown is one game. Metadata that spans a whole night (NHL carries
    # every game's showdown salaries) must not let two games share a lineup.
    if (!is.null(R$game)) R$max_games <- 1L

  } else if (format == "tennis_captain") {
    R$sal   <- .lr_map(meta, .lr_pick(nm, c("Salary", "DKSalary")))
    R$fixed <- list(list(name = "Captain",  elig = NULL, sal = .lr_map(meta, "CPTSalary")),
                    list(name = "ACaptain", elig = NULL, sal = .lr_map(meta, "ACPTSalary")))
    R$slots <- list(list(NULL, roster - 2L))

  } else if (format == "f1") {
    R$type  <- setNames(as.character(meta$PlayerType), meta$Player)
    R$pos   <- setNames(as.list(R$type), meta$Player)
    R$fixed <- list(list(name = "Captain", elig = "Driver", sal = .lr_map(meta, "CptSalary")))
    R$slots <- list(list("Driver", roster - 2L), list("Constructor", 1L))
    R$f1_stack <- TRUE
  }
  R
}

#' Can these players fill these slots? Kuhn's augmenting-path matching.
.lr_match <- function(pos_list, elig_list) {
  np <- length(pos_list); ns <- length(elig_list)
  if (np != ns) return(FALSE)
  A <- matrix(FALSE, np, ns)
  for (j in seq_len(ns)) {
    e <- elig_list[[j]]
    A[, j] <- if (is.null(e)) TRUE else vapply(pos_list, function(p) any(p %in% e), logical(1))
  }
  owner <- rep(0L, ns)
  try_p <- function(i, seen) {
    for (j in which(A[i, ] & !seen$v)) {
      seen$v[j] <- TRUE
      if (owner[j] == 0L || try_p(owner[j], seen)) { owner[j] <<- i; return(TRUE) }
    }
    FALSE
  }
  for (i in seq_len(np)) {
    seen <- new.env(); seen$v <- rep(FALSE, ns)
    if (!try_p(i, seen)) return(FALSE)
  }
  TRUE
}

#' Distinct non-NA values per row.
.lr_ndistinct <- function(X) {
  vapply(seq_len(nrow(X)), function(i) length(unique(X[i, !is.na(X[i, ])])), integer(1))
}

#' Largest count of one value per row (max players from one team / game).
.lr_maxcount <- function(X) {
  vapply(seq_len(nrow(X)), function(i) { v <- X[i, !is.na(X[i, ])]; if (length(v)) max(tabulate(match(v, v))) else 0L },
         integer(1))
}

#' Which lineups are legal under R.
#'
#' @param M  character matrix: the fixed slots of R (Captain, ACaptain) in its
#'           first columns, then the rest of the roster in any order
#' @param check_positions  FALSE skips the slot matching -- for a sampler that
#'           drew each slot from its own eligible players, legal by construction
#' @return logical vector, one per row; attr "why" counts each failure
lineup_legal <- function(M, R, check_positions = TRUE) {
  M <- as.matrix(M); storage.mode(M) <- "character"
  n <- nrow(M); if (!n) return(logical(0))
  nf <- length(R$fixed)
  n_rest <- sum(vapply(R$slots, function(s) as.integer(s[[2L]]), integer(1)))
  why <- c(shape = 0L, unknown = 0L, repeat_player = 0L, salary = 0L, positions = 0L,
           teams = 0L, games = 0L, one_game = 0L, per_team = 0L, per_game = 0L, f1_stack = 0L)
  fail <- function(ok, key) { why[[key]] <<- sum(!ok & keep); keep & ok }

  if (ncol(M) != nf + n_rest) {
    out <- rep(FALSE, n); attr(out, "why") <- replace(why, "shape", n); return(out)
  }
  keep <- rep(TRUE, n)
  keep <- fail(rowSums(is.na(M)) == 0L & rowSums(!matrix(M %in% names(R$sal), n)) == 0L, "unknown")
  S <- t(apply(M, 1L, sort))
  keep <- fail(ncol(M) < 2L | rowSums(S[, -1L, drop = FALSE] == S[, -ncol(S), drop = FALSE], na.rm = TRUE) == 0L,
               "repeat_player")

  # Salary: fixed slots at their own price, the rest at base salary
  tot <- if (n_rest) rowSums(matrix(R$sal[M[, (nf + 1L):ncol(M), drop = FALSE]], n)) else 0
  for (k in seq_len(nf)) tot <- tot + unname(R$fixed[[k]]$sal[M[, k]])
  keep <- fail(!is.na(tot) & tot <= R$sal_cap, "salary")

  # Positions: fixed slots by their own eligibility, the rest by matching
  if (check_positions) {
    ok <- rep(TRUE, n)
    for (k in seq_len(nf)) {
      e <- R$fixed[[k]]$elig
      if (!is.null(e)) ok <- ok & vapply(M[, k], function(p) any(R$pos[[p]] %in% e), logical(1))
    }
    positional <- !is.null(R$pos) && !all(vapply(R$slots, function(s) is.null(s[[1L]]), logical(1)))
    if (positional && n_rest) {
      elig <- unlist(lapply(R$slots, function(s) rep(list(s[[1L]]), s[[2L]])), recursive = FALSE)
      rest <- M[, (nf + 1L):ncol(M), drop = FALSE]
      sig  <- apply(rest, 1L, function(r) paste(sort(vapply(R$pos[r], paste, "", collapse = "/")), collapse = "|"))
      us   <- unique(sig[keep & ok])
      res  <- setNames(vapply(us, function(s) {
        .lr_match(strsplit(strsplit(s, "|", fixed = TRUE)[[1]], "/", fixed = TRUE), elig)
      }, logical(1)), us)
      ok <- ok & (sig %in% us[res])
    }
    keep <- fail(ok, "positions")
  }

  if (!is.null(R$team)) {
    TM <- matrix(R$team[M], n)
    TC <- TM
    if (length(R$team_skip_pos) && !is.null(R$pos))
      TC[matrix(vapply(M, function(p) any(R$pos[[p]] %in% R$team_skip_pos), logical(1)), n)] <- NA
    if (R$min_teams > 0L) keep <- fail(.lr_ndistinct(TC) >= R$min_teams, "teams")
    if (!is.na(R$max_per_team)) keep <- fail(.lr_maxcount(TM) <= R$max_per_team, "per_team")
  }
  if (!is.null(R$game)) {
    GM <- matrix(R$game[M], n)
    if (R$min_games > 0L) keep <- fail(.lr_ndistinct(GM) >= R$min_games, "games")
    if (!is.na(R$max_games)) keep <- fail(.lr_ndistinct(GM) <= R$max_games, "one_game")
    if (!is.na(R$max_per_game)) keep <- fail(.lr_maxcount(GM) <= R$max_per_game, "per_game")
  }

  # F1: at most one driver from the constructor's own team, captain included
  if (isTRUE(R$f1_stack)) {
    TY <- matrix(R$type[M], n); TM <- matrix(R$team[M], n)
    ct <- TM[cbind(seq_len(n), max.col(TY == "Constructor", ties.method = "first"))]
    keep <- fail(rowSums(TY == "Driver" & TM == ct, na.rm = TRUE) <= 1L, "f1_stack")
  }

  attr(keep, "why") <- why[why > 0L]
  keep
}

#' Slot columns of a lineup table in rulebook order (fixed slots first).
lineup_rule_cols <- function(dt, R) {
  nm <- names(dt)
  fixed <- vapply(R$fixed, `[[`, "", "name")
  rest  <- setdiff(grep("^Player[0-9]+$|^Util[0-9]+$", nm, value = TRUE), fixed)
  c(fixed, rest)
}

#' Drop the lineups R rejects from an optimiser's lineup_data, and say why.
drop_illegal_lineups <- function(lineup_data, R, label = "") {
  ul <- lineup_data$unique_lineups
  if (is.null(ul) || !nrow(ul)) return(lineup_data)
  pc <- lineup_rule_cols(ul, R)
  if (!all(pc %in% names(ul))) return(lineup_data)
  ok <- lineup_legal(as.matrix(ul[, ..pc]), R)
  if (any(!ok)) {
    w <- attr(ok, "why")
    cat(sprintf("  [Rules%s] dropped %s illegal lineup(s): %s\n", label,
                format(sum(!ok), big.mark = ","),
                paste(names(w), w, sep = " ", collapse = ", ")))
    lineup_data$unique_lineups <- ul[ok]
  }
  lineup_data
}
