# ==============================================================================
# UNIVERSAL PORTFOLIO HELPERS
# Standardized functions for portfolio exposure across all platforms
# ==============================================================================

library(data.table)

#' Create universal exposure table for any platform
#' Works identically for DK, FD, SD - no sport-specific columns
#' 
#' @param portfolio Portfolio lineups data.table
#' @param metadata Player metadata data.table  
#' @param platform Platform code ("DK", "FD", "SD")
#' @param config Sport configuration list
#' @return Formatted exposure data.table with universal columns only
create_exposure_table_universal <- function(portfolio, metadata, platform, config) {
  setDT(portfolio)
  setDT(metadata)
  
  # Detect player columns based on format
  has_captain <- "Captain" %in% names(portfolio)
  has_mvp <- "MVP" %in% names(portfolio)
  
  if (has_captain) {
    player_cols <- c("Captain", grep("^Util", names(portfolio), value = TRUE))
  } else if (has_mvp) {
    player_cols <- c("MVP", grep("^Player", names(portfolio), value = TRUE))
  } else {
    player_cols <- grep("^Player", names(portfolio), value = TRUE)
  }
  
  # Calculate full portfolio exposure
  all_players <- unlist(portfolio[, ..player_cols])
  exposure_counts <- table(all_players)
  
  # Start with ALL players from metadata
  exposure_table <- data.table(Player = metadata$Player)
  
  # Add full portfolio exposure
  exposure_table[, Exposure := 0]
  for (i in 1:nrow(exposure_table)) {
    player <- exposure_table$Player[i]
    if (player %in% names(exposure_counts)) {
      exposure_table$Exposure[i] <- (as.numeric(exposure_counts[player]) / nrow(portfolio)) * 100
    }
  }
  
  # Get platform-specific columns
  salary_col <- config$platform_columns[[platform]]$salary
  own_col <- config$platform_columns[[platform]]$ownership
  
  # Add metadata - UNIVERSAL columns only (no Starting, Team, Car, etc.)
  metadata_cols <- c("Player", salary_col, own_col)
  
  exposure_table <- merge(
    exposure_table,
    metadata[, ..metadata_cols],
    by = "Player",
    all.x = TRUE
  )
  
  # Standardize column names
  setnames(exposure_table, c(salary_col, own_col), c("Salary", "OwnProj"))
  exposure_table[, OwnProj := OwnProj * 100]
  exposure_table[, Leverage := round(Exposure - OwnProj, 1)]
  
  # Set column order - UNIVERSAL (identical for all platforms)
  final_col_order <- c("Player", "Salary", "Exposure", "OwnProj", "Leverage")
  setcolorder(exposure_table, final_col_order)
  
  # Filter to only players with exposure > 0
  exposure_table <- exposure_table[Exposure > 0]
  
  # Sort by exposure
  setorder(exposure_table, -Exposure)
  
  return(exposure_table)
}

# ==============================================================================
# THE POOL -> PORTFOLIO PATH
# Lifted out of app.R's server on 29 Sep 2026 so the contest review's strategy
# harness (Documents/GTS/Review/playbook/) runs the SAME code on a saved re-sim
# that the Portfolio Builder runs on a live one. A filter setting then means the
# same thing in the review as in the app. app.R calls these; it keeps only the
# parts that read Shiny inputs (slider ids, lock buttons, F1, team split).
# ==============================================================================

#' Sim results + metadata -> the optimiser's input: one row per player x sim,
#' with Salary and FantasyPoints for the platform ("DK", "FD", "SD").
prepare_optimization_data <- function(sim_results, metadata, platform) {
  score_col  <- if (platform == "SD") "DKScore"              else paste0(platform, "Score")
  salary_col <- if (platform == "SD") "SDSalary"             else paste0(platform, "Salary")
  setDT(sim_results); setDT(metadata)
  opt_data <- merge(sim_results, metadata[, .(Player, Salary=get(salary_col))], by="Player")
  opt_data[, FantasyPoints := get(score_col)]
  opt_data[Salary > 0 & !is.na(Salary)]
}

# Row masks: does each lineup hold ALL of `players` / NONE of them, anywhere in
# `cols`. Vectorised column comparisons, no player matrix, no R-level row loop
# (the row-by-row version was the slowest step of the filter chain).
has_all_players <- function(dt, cols, players) {
  if (!length(players) || !length(cols)) return(rep(TRUE, nrow(dt)))
  Reduce(`&`, lapply(players, function(p)
    Reduce(`|`, lapply(cols, function(cc) !is.na(dt[[cc]]) & dt[[cc]] == p))))
}
has_no_players <- function(dt, cols, players) {
  if (!length(players) || !length(cols)) return(rep(TRUE, nrow(dt)))
  !Reduce(`|`, lapply(players, function(p)
    Reduce(`|`, lapply(cols, function(cc) !is.na(dt[[cc]]) & dt[[cc]] == p))))
}

#' The Portfolio Builder's filters on a scored pool, as plain arguments.
#'   min_rates  named list, rate column -> minimum (WinRate, Top1Pct, Top5Pct,
#'              Top10Pct, Top20Pct); NULL or 0 means no minimum
#'   ranges     named list, numeric column -> c(lo, hi) in the column's own
#'              units (TotalSalary in dollars). A column that does not vary
#'              across `lineups` is never filtered: Shiny keeps a slider's last
#'              value after the slider is gone, and a showdown pool (AvgOwn 0
#'              everywhere) would otherwise be emptied by a classic run's range.
#'   lock/excl  players every lineup must hold / must not hold, any slot
#'   lock_cpt/excl_cpt  the same, captain (or MVP) slot only
#' AvgOwn is a geometric mean, so one unowned player zeroes it: 0 means "no
#' ownership data", not "contrarian", and is always kept.
portfolio_filter_pool <- function(lineups, min_rates = NULL, ranges = NULL, lock = NULL, excl = NULL,
                                  lock_cpt = NULL, excl_cpt = NULL) {
  pool <- lineups
  for (rc in names(min_rates)) {
    v <- min_rates[[rc]]
    if (!is.null(v) && v > 0 && rc %in% names(lineups)) lineups <- lineups[get(rc) >= v]
  }
  varies <- function(col) {
    x <- pool[[col]]
    mn <- suppressWarnings(min(x, na.rm=TRUE)); mx <- suppressWarnings(max(x, na.rm=TRUE))
    is.finite(mn) && is.finite(mx) && mn != mx
  }
  for (col in names(ranges)) {
    fv <- ranges[[col]]
    if (is.null(fv) || !col %in% names(lineups) || !varies(col)) next
    lineups <- if (col == "AvgOwn")
      lineups[AvgOwn == 0 | (AvgOwn >= fv[1] & AvgOwn <= fv[2])]
    else lineups[get(col) >= fv[1] & get(col) <= fv[2]]
  }
  pc <- grep("^Player|^Captain|^MVP|^Util", names(lineups), value=TRUE)
  if (length(lock)) lineups <- lineups[has_all_players(lineups, pc, lock)]
  if (length(excl)) lineups <- lineups[has_no_players( lineups, pc, excl)]
  # SHOWDOWN: captain and flex are separate roster spots, so they filter
  # separately -- a player locked at captain is a different constraint from
  # the same player locked anywhere.
  cap_cols <- grep("^Captain$|^MVP$", names(lineups), value = TRUE)
  if (length(cap_cols)) {
    if (length(lock_cpt)) lineups <- lineups[get(cap_cols[1]) %in% lock_cpt]
    if (length(excl_cpt)) lineups <- lineups[!get(cap_cols[1]) %in% excl_cpt]
  }
  lineups
}

#' "Add Build": n lineups drawn uniformly, without replacement, from the
#' filtered pool. NULL when the pool holds fewer than n.
portfolio_draw <- function(filtered, n) {
  if (nrow(filtered) < n) return(NULL)
  filtered[sample(nrow(filtered), n)]
}
