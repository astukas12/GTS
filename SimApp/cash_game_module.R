# ============================================================================
# CASH GAME MODULE — Double Up Simulator
# Golden Ticket Sims
#
# Sport-agnostic: reads roster size and salary cap from rv$config.
# Works for any sport that has DK optimal lineups + DKOwn in metadata.
# ============================================================================


# ============================================================================
# HELPERS
# ============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Detect player slot columns from a lineup data.table.
get_player_cols <- function(lineup_dt) {
  pc <- grep("^Player[0-9]+$", names(lineup_dt), value = TRUE)
  if (length(pc) > 0) return(pc)
  pc <- grep("^Captain$|^Util[0-9]+$", names(lineup_dt), value = TRUE)
  if (length(pc) > 0) return(pc)
  stop("Cannot detect player columns in lineup data.")
}

#' Get DK salary cap and roster size from config with safe fallbacks.
get_dk_constraints <- function(config) {
  salary_cap  <- config$salary_caps$DK  %||% 50000
  roster_size <- config$roster_sizes$DK %||% 6L
  list(salary_cap = salary_cap, roster_size = as.integer(roster_size))
}

get_cash_params <- function(config, platform = "DK") {
  sport <- config$sport_name %||% ""
  if (sport == "NBA") {
    list(n_field = 100L, n_yours = 10L, top_n_ppd = 15L,
         sal_floor = 49000, total_lineups = 110L)
  } else {
    list(n_field = 500L, n_yours = 50L, top_n_ppd = 20L,
         sal_floor = 49000, total_lineups = 550L)
  }
}

# NBA DK slot names for display (Player1..Player8 -> PG/SG/SF/PF/C/G/F/UTIL)
get_slot_labels <- function(config, n_slots) {
  sport <- config$sport_name %||% ""
  if (sport == "NBA") {
    switch(as.character(n_slots),
           "8" = c("PG","SG","SF","PF","C","G","F","UTIL"),
           "9" = c("PG","PG2","SG","SG2","SF","SF2","PF","PF2","C"),
           "6" = c("CPT","UTIL1","UTIL2","UTIL3","UTIL4","UTIL5"),
           paste0("P", seq_len(n_slots))
    )
  } else NULL  # NULL = keep Player1..N
}


# ============================================================================
# FIELD LINEUP GENERATION
#
# Approach:
#   1. Compute ownership-per-dollar (PPD) for every eligible player
#   2. Keep top 20 by PPD — these are the players the field actually builds with
#   3. combn() all combinations of roster_size from those 20
#   4. Vectorized salary filter: salary_floor <= total <= salary_cap
#   5. Compute geometric mean ownership per lineup (same formula as tournament scoring)
#   6. Sort descending by AvgOwn, return top n
#
# This produces deterministic, reproducible field lineups that represent
# realistic chalk construction — no random sampling needed.
# ============================================================================

#' Generate field lineups via PPD filter + exhaustive combination + AvgOwn ranking.
#'
#' @param metadata      data.table: Player, DKSalary, DKOwn (0-100 or 0-1 scale)
#' @param n             integer, how many field lineups to return (default 100)
#' @param salary_cap    numeric
#' @param salary_floor  numeric, minimum total salary (default salary_cap - 1000, i.e. $49k on a $50k cap)
#' @param roster_size   integer
#' @param top_n_ppd     integer, keep top N players by own/dollar (default 20)
#' @return data.table: LineupID, Player1..N, TotalSalary, AvgOwn
generate_field_lineups <- function(metadata,
                                   n            = 500L,
                                   salary_cap   = 50000,
                                   salary_floor = NULL,
                                   roster_size  = 6L,
                                   top_n_ppd    = 20L) {
  
  drivers <- copy(as.data.table(metadata))
  missing <- setdiff(c("Player", "DKSalary", "DKOwn"), names(drivers))
  if (length(missing) > 0) stop("metadata missing: ", paste(missing, collapse = ", "))
  
  # Default salary floor
  if (is.null(salary_floor)) salary_floor <- 49000
  
  # Clean: require valid salary and ownership
  drivers <- drivers[!is.na(DKSalary) & DKSalary > 0 & !is.na(DKOwn) & DKOwn > 0]
  
  # Normalise ownership to 0-100 scale
  own_vals <- drivers$DKOwn
  if (max(own_vals, na.rm = TRUE) <= 1) drivers[, DKOwn := DKOwn * 100]
  
  # ── Step 1: PPD filter ────────────────────────────────────────────────────
  # ownership-per-$1000 of salary.
  # Skip filter automatically when pool is already <= top_n_ppd players
  # (e.g. MMA slates with 22-30 fighters need no further cutting).
  drivers[, PPD := DKOwn / (DKSalary / 1000)]
  setorder(drivers, -PPD)
  
  if (nrow(drivers) <= top_n_ppd) {
    pool <- drivers
    cat(sprintf("  [Field] PPD filter skipped — pool only %d players, using all\n",
                nrow(pool)))
  } else {
    pool <- head(drivers, top_n_ppd)
    cat(sprintf("  [Field] PPD filter applied — kept top %d of %d players\n",
                top_n_ppd, nrow(drivers)))
  }
  
  if (nrow(pool) < roster_size) {
    stop(sprintf("Only %d players in pool — need at least %d.", nrow(pool), roster_size))
  }
  
  cat(sprintf("  [Field] Pool: %d players | salary $%s-$%s | own %.1f%%-%.1f%% | PPD %.2f-%.2f\n",
              nrow(pool),
              format(min(pool$DKSalary), big.mark = ","),
              format(max(pool$DKSalary), big.mark = ","),
              min(pool$DKOwn), max(pool$DKOwn),
              min(pool$PPD),   max(pool$PPD)))
  
  # ── Step 2: Generate all combinations ────────────────────────────────────
  player_names <- pool$Player
  n_pool       <- length(player_names)
  combos       <- combn(n_pool, roster_size, simplify = FALSE)
  
  cat(sprintf("  [Field] %s total combinations from %d players\n",
              format(length(combos), big.mark = ","), n_pool))
  
  # ── Step 3: Vectorised salary calculation ─────────────────────────────────
  sal_lookup <- setNames(pool$DKSalary, pool$Player)
  own_lookup <- setNames(pool$DKOwn,    pool$Player)
  
  # Build matrix: each row = one combo, each col = one slot
  combo_mat  <- do.call(rbind, combos)          # n_combos x roster_size (integer indices)
  sal_mat    <- matrix(sal_lookup[player_names[combo_mat]], nrow = nrow(combo_mat))
  total_sal  <- rowSums(sal_mat)
  
  # ── Step 4: Salary filter ─────────────────────────────────────────────────
  valid_idx  <- which(total_sal >= salary_floor & total_sal <= salary_cap)
  
  if (length(valid_idx) == 0) {
    # Expand floor by $500 increments until we get something
    expanded_floor <- salary_floor
    while (length(valid_idx) == 0 && expanded_floor > salary_cap * 0.80) {
      expanded_floor <- expanded_floor - 500
      valid_idx <- which(total_sal >= expanded_floor & total_sal <= salary_cap)
    }
    if (length(valid_idx) == 0) stop("No valid lineups found within salary constraints.")
    warning(sprintf("Salary floor relaxed to $%s to find valid lineups.",
                    format(expanded_floor, big.mark = ",")))
  }
  
  cat(sprintf("  [Field] %s lineups pass salary filter ($%s-$%s)\n",
              format(length(valid_idx), big.mark = ","),
              format(salary_floor, big.mark = ","),
              format(salary_cap,   big.mark = ",")))
  
  valid_combos <- combo_mat[valid_idx, , drop = FALSE]
  valid_sal    <- total_sal[valid_idx]
  
  # ── Step 5: Geometric mean ownership ─────────────────────────────────────
  # exp(mean(log(own))) — same formula as calculate_distribution_metrics()
  own_mat  <- matrix(own_lookup[player_names[valid_combos]], nrow = nrow(valid_combos))
  # Guard against zeros (shouldn't happen post-filter but be safe)
  own_mat[own_mat <= 0] <- NA
  avg_own  <- exp(rowMeans(log(own_mat), na.rm = TRUE))
  
  # ── Step 6: Sort and take top n ───────────────────────────────────────────
  order_idx    <- order(-avg_own)
  top_idx      <- head(order_idx, n)
  top_combos   <- valid_combos[top_idx, , drop = FALSE]
  top_sal      <- valid_sal[top_idx]
  top_avg_own  <- avg_own[top_idx]
  
  cat(sprintf("  [Field] Returning top %d by AvgOwn (range: %.1f%% - %.1f%%)\n",
              length(top_idx), min(top_avg_own), max(top_avg_own)))
  
  # ── Build output data.table ───────────────────────────────────────────────
  player_cols <- paste0("Player", seq_len(roster_size))
  player_mat  <- matrix(player_names[top_combos], nrow = nrow(top_combos))
  
  dt <- as.data.table(player_mat)
  setnames(dt, player_cols)
  dt[, TotalSalary := top_sal]
  dt[, AvgOwn      := round(top_avg_own, 2)]
  dt[, LineupID    := paste0("F", seq_len(.N))]
  setcolorder(dt, c("LineupID", player_cols, "TotalSalary", "AvgOwn"))
  dt
}


# ============================================================================
# NBA FIELD LINEUP GENERATION — LP on projections
#
# For NBA we skip the PPD+combn approach (too many position constraints) and
# instead build 50 top-projected lineups using the same DK slot structure as
# the tournament optimizer.  Each iteration adds an "exclude at least 1 player
# from the previous lineup" constraint so we get lineup diversity without
# random sampling.
#
# Requires: metadata with DKSalary, DKPos, DKProj (ETR projections), GameKey.
# Calls assign_nba_slots_dk() from nba_engine.R for slot assignment.
# ============================================================================

#' Build NBA field lineups by running LP n times on mean projections.
#'
#' @param metadata   data.table: Player, DKSalary, DKPos, DKProj, GameKey
#' @param n          integer, how many lineups to build (default 50)
#' @param salary_cap numeric (default 50000)
#' @return data.table: LineupID, Player1..8, TotalSalary, AvgProj
generate_field_lineups_nba <- function(metadata,
                                       n            = 100L,
                                       salary_cap   = 50000,
                                       platform     = "DK") {
  
  meta <- copy(as.data.table(metadata))
  
  # Platform-specific column names
  sal_col  <- if (platform == "FD") "FDSalary" else "DKSalary"
  pos_col  <- if (platform == "FD") "FDPos"    else "DKPos"
  proj_col <- if (platform == "FD") "FDProj"   else "DKProj"
  
  missing <- setdiff(c("Player", sal_col, pos_col, proj_col), names(meta))
  if (length(missing) > 0) stop("NBA field gen — metadata missing: ", paste(missing, collapse = ", "))
  
  setnames(meta, c(sal_col, proj_col), c("Sal", "Proj"))
  # Keep original pos column name intact for assign_nba_slots_dk/fd — also alias as Pos for constraints
  meta[, Pos := get(pos_col)]
  meta <- unique(meta[!is.na(Sal) & Sal > 0 & !is.na(Proj)], by = "Player")
  
  # Platform roster constraints
  if (platform == "FD") {
    # FD: 9 players — PG/PG/SG/SG/SF/SF/PF/PF/C — >=4G, >=4F, >=1C
    n_roster  <- 9L
    meta[, g_elig := as.integer(grepl("PG|SG",     Pos))]
    meta[, f_elig := as.integer(grepl("SF|PF",     Pos))]
    meta[, c_elig := as.integer(grepl("^C$|C/|/C", Pos))]
    con_mat <- rbind(rep(1L, nrow(meta)), meta$Sal, meta$g_elig, meta$f_elig, meta$c_elig)
    con_dir <- c("==", "<=", ">=", ">=", ">=")
    con_rhs <- c(9L, salary_cap, 4L, 4L, 1L)
  } else {
    # DK: 8 players — PG/SG/SF/PF/C/G/F/UTIL — >=2G, >=2F, >=1C
    n_roster  <- 8L
    meta[, g_elig := as.integer(grepl("PG|SG",     Pos))]
    meta[, f_elig := as.integer(grepl("SF|PF",     Pos))]
    meta[, c_elig := as.integer(grepl("^C$|C/|/C", Pos))]
    con_mat <- rbind(rep(1L, nrow(meta)), meta$Sal, meta$g_elig, meta$f_elig, meta$c_elig)
    con_dir <- c("==", "<=", ">=", ">=", ">=")
    con_rhs <- c(8L, salary_cap, 2L, 2L, 1L)
  }
  
  n_p       <- nrow(meta)
  obj       <- meta$Proj
  std_cols  <- paste0("Player", seq_len(n_roster))
  
  lineup_list   <- vector("list", n)
  excluded_rows <- list()
  used_sigs     <- character(0)
  successful    <- 0L
  
  for (iter in seq_len(n + 20L)) {
    if (successful >= n) break
    
    if (length(excluded_rows) > 0) {
      full_mat <- rbind(con_mat, do.call(rbind, excluded_rows))
      full_dir <- c(con_dir, rep("<=", length(excluded_rows)))
      full_rhs <- c(con_rhs, rep(n_roster - 1L, length(excluded_rows)))
    } else {
      full_mat <- con_mat
      full_dir <- con_dir
      full_rhs <- con_rhs
    }
    
    res <- tryCatch(
      lp("max", obj, full_mat, full_dir, full_rhs, all.bin = TRUE),
      error = function(e) list(status = 1L)
    )
    if (res$status != 0L) {
      cat(sprintf("  [Field-NBA] LP infeasible after %d lineups — stopping\n", successful))
      break
    }
    
    sel_idx <- which(res$solution == 1L)
    if (length(sel_idx) != n_roster) next
    
    sig <- paste(sort(meta$Player[sel_idx]), collapse = "|")
    
    # Always exclude this exact set before next iteration
    excl_row          <- integer(n_p)
    excl_row[sel_idx] <- 1L
    excluded_rows[[length(excluded_rows) + 1L]] <- excl_row
    
    if (sig %in% used_sigs) next
    used_sigs  <- c(used_sigs, sig)
    successful <- successful + 1L
    
    chosen  <- meta[sel_idx]
    
    # Assign players to named slots using same function as tournament optimizer.
    # This ensures Player1..N order matches PG/SG/SF/PF/C/G/F/UTIL (DK) or
    # PG1/PG2/SG1/SG2/SF1/SF2/PF1/PF2/C (FD) — identical to rv$dk/fd_optimal_lineups.
    pos_col_name <- if (platform == "FD") "FDPos" else "DKPos"
    if (!"game_rank" %in% names(chosen)) chosen[, game_rank := 1L]
    cm     <- chosen[, intersect(c("Player", pos_col_name, "game_rank"), names(chosen)), with = FALSE]
    setnames(cm, pos_col_name, if (platform == "FD") "FDPos" else "DKPos")
    
    slots <- if (platform == "FD") assign_nba_slots_fd(cm) else assign_nba_slots_dk(cm)
    if (is.null(slots)) next   # slot assignment failed — skip, LP already excluded this set
    
    # Map named slots to Player1..N in canonical order
    slot_names <- if (platform == "FD") {
      c("PG1","PG2","SG1","SG2","SF1","SF2","PF1","PF2","C")
    } else {
      c("PG","SG","SF","PF","C","G","F","UTIL")
    }
    
    row_dt <- as.data.table(setNames(lapply(slot_names, function(s) slots[[s]]), std_cols))
    row_dt[, LineupID    := paste0("F", successful)]
    row_dt[, TotalSalary := sum(chosen$Sal)]
    row_dt[, AvgOwn      := NA_real_]
    setcolorder(row_dt, c("LineupID", std_cols, "TotalSalary", "AvgOwn"))
    lineup_list[[successful]] <- row_dt
  }
  
  result <- rbindlist(lineup_list[!sapply(lineup_list, is.null)])
  if (nrow(result) == 0L) stop("NBA field generation produced no valid lineups.")
  
  cat(sprintf("  [Field-NBA] %d projection-optimized lineups (%s) built via LP\n",
              nrow(result), platform))
  result
}


# Resolve which platform's lineups + score column to use for cash.
# Priority: DK > FD (SD showdown excluded from cash for now).
# Returns list(platform, optimal_lineups, score_col, sal_col, id_col)
get_cash_platform_data <- function(rv, platform = NULL) {
  if (is.null(platform)) {
    if (!is.null(rv$dk_optimal_lineups))      platform <- "DK"
    else if (!is.null(rv$fd_optimal_lineups)) platform <- "FD"
    else stop("No scored tournament lineups found. Score DK or FD lineups first.")
  }
  
  opt <- switch(platform,
                DK = rv$dk_optimal_lineups,
                FD = rv$fd_optimal_lineups,
                stop("Unsupported cash platform: ", platform)
  )
  if (is.null(opt)) stop(platform, " tournament lineups not found. Score them first.")
  
  score_col <- if (platform == "FD") "FDScore" else "DKScore"
  sal_col   <- if (platform == "FD") "FDSalary" else "DKSalary"
  id_col    <- if (platform == "FD") "FDID" else "DKID"
  sal_cap   <- if (platform == "FD") 60000 else 50000
  
  list(platform = platform, optimal_lineups = as.data.table(copy(opt)),
       score_col = score_col, sal_col = sal_col, id_col = id_col, sal_cap = sal_cap)
}

# ============================================================================
# SCORING
#
# Uses score_all_lineups() from OptimalLineups_Core.R — the same matrix
# multiply approach the tournament process uses. No cartesian joins.
# ============================================================================

#' Wrap a flat lineup pool into the lineup_data list format score_all_lineups expects.
make_lineup_data <- function(lineup_pool, sim_results, player_cols, score_col = "DKScore") {
  list(
    unique_lineups = lineup_pool[, player_cols, with = FALSE],
    n_sims         = length(unique(sim_results$SimID)),
    config         = list(platform_col = score_col, percentiles = c(0.01, 0.05, 0.10, 0.20)),
    mode           = "standard",
    platform_col   = score_col
  )
}


#' Compute double-up cash rates from a score matrix with field entry weighting.
#'
#' Field lineups are weighted by an exponential decay (F1 appears most, Fn least)
#' to model a real cash field where chalk lineups are entered many times.
#' "Yours" lineups always get weight 1 — one entry each.
#'
#' For each sim, a lineup cashes if the total weighted entries that scored HIGHER
#' is less than cash_rank (top 45% of total weighted entries).
#'
#' @param score_matrix  numeric matrix: n_lineups x n_sims
#' @param lineup_ids    character vector of LineupID (F* = field, Y* = yours)
#' @param cash_pct      numeric 0-1, fraction that cash (0.45 for double up)
#' @param max_weight    integer, copies of the #1 field lineup (default 200)
#' @param min_weight    integer, floor copies for last field lineup (default 5)
#' @param verbose       logical
#' @return data.table: LineupID, MedianScore, CashRate
cash_rate_from_score_matrix <- function(score_matrix, lineup_ids,
                                        cash_pct   = 0.45,
                                        max_weight = 20L,
                                        min_weight = 1L,
                                        verbose    = TRUE) {
  n_lineups <- nrow(score_matrix)
  n_sims    <- ncol(score_matrix)
  
  # ── Build weight vector ───────────────────────────────────────────────────
  is_field  <- grepl("^F", lineup_ids)
  field_idx <- which(is_field)
  yours_idx <- which(!is_field)
  n_field   <- length(field_idx)
  
  weights <- integer(n_lineups)
  weights[yours_idx] <- 1L
  if (n_field > 0) {
    if (n_field == 1L) {
      weights[field_idx] <- max_weight
    } else {
      lam <- log(max_weight / min_weight) / (n_field - 1L)
      weights[field_idx] <- pmax(min_weight,
                                 as.integer(round(max_weight * exp(-lam * seq(0, n_field - 1L)))))
    }
  }
  
  total_entries <- sum(weights)
  cash_rank     <- floor(total_entries * cash_pct)
  
  if (verbose) {
    cat(sprintf(
      "  [Cash] %d lineups | field weighted to %s entries + %d yours | cash line: top %d (%.0f%%)\n",
      n_lineups, format(sum(weights[field_idx]), big.mark=","),
      length(yours_idx), cash_rank, cash_pct * 100))
    flush.console()
  }
  
  # ── Vectorized weighted ranking via sort + cumsum ────────────────────────
  # For each sim: sort lineups by score desc, cumsum weights gives each
  # lineup's weighted rank (total weight of all lineups scoring strictly higher).
  # Lineup cashes if weighted_rank < cash_rank.  O(n log n) per sim.
  
  t0          <- Sys.time()
  chunk_size  <- 2000L
  n_chunks    <- ceiling(n_sims / chunk_size)
  cash_counts <- integer(n_lineups)
  
  for (chunk_idx in seq_len(n_chunks)) {
    chunk_start <- (chunk_idx - 1L) * chunk_size + 1L
    chunk_end   <- min(chunk_idx * chunk_size, n_sims)
    S           <- score_matrix[, chunk_start:chunk_end, drop = FALSE]
    
    chunk_cash <- apply(S, 2L, function(scores) {
      ord           <- order(scores, decreasing = TRUE)
      sorted_w      <- weights[ord]
      # cum_above[i] = total weight of all lineups ranked above position i
      cum_above     <- c(0L, cumsum(sorted_w)[-n_lineups])
      weighted_rank <- integer(n_lineups)
      weighted_rank[ord] <- cum_above
      as.integer(weighted_rank < cash_rank)
    })
    # chunk_cash: n_lineups x chunk matrix of 0/1
    cash_counts <- cash_counts + rowSums(chunk_cash)
    
    if (verbose) {
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      eta     <- if (chunk_idx > 1) (elapsed / chunk_idx) * (n_chunks - chunk_idx) else NA_real_
      if (is.na(eta)) {
        cat(sprintf("\r  [Cash] %3d%%  chunk %d/%d  %.1fs",
                    round(chunk_end / n_sims * 100), chunk_idx, n_chunks, elapsed))
      } else {
        cat(sprintf("\r  [Cash] %3d%%  chunk %d/%d  %.1fs  ETA %.0fs",
                    round(chunk_end / n_sims * 100), chunk_idx, n_chunks, elapsed, eta))
      }
      flush.console()
    }
  }
  
  if (verbose) { cat("\n"); flush.console() }
  elapsed_total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (verbose) {
    cat(sprintf("  [Cash] Complete in %.1fs\n", elapsed_total))
    flush.console()
  }
  
  median_scores <- apply(score_matrix, 1L, median)
  
  data.table(
    LineupID    = lineup_ids,
    MedianScore = round(median_scores, 2),
    CashRate    = round((cash_counts / n_sims) * 100, 1)
  )
}


# ============================================================================
# COMBINED EXPOSURE TABLE
# One row per driver: Player, Salary, OwnProj, PPD, FieldExp%, YourExp%
# ============================================================================

build_combined_exposure <- function(field_pool, your_pool, metadata, player_cols) {
  
  count_exp <- function(pool) {
    n  <- nrow(pool)
    pc <- intersect(player_cols, names(pool))
    tab <- as.data.table(table(unlist(pool[, pc, with = FALSE])))
    setnames(tab, c("Player", "N"))
    tab[, Pct := round(N / n * 100, 1)]
    tab[, .(Player, Pct)]
  }
  
  fe <- count_exp(field_pool)
  ye <- count_exp(your_pool)
  
  all_p <- unique(c(fe$Player, ye$Player))
  base  <- data.table(Player = all_p)
  base  <- merge(base, fe, by = "Player", all.x = TRUE); setnames(base, "Pct", "FieldExp")
  base  <- merge(base, ye, by = "Player", all.x = TRUE); setnames(base, "Pct", "YourExp")
  base[is.na(FieldExp), FieldExp := 0]
  base[is.na(YourExp),  YourExp  := 0]
  
  meta_dt   <- as.data.table(metadata)
  meta_cols <- intersect(c("Player", "DKSalary", "DKOwn"), names(meta_dt))
  meta_sub  <- meta_dt[, meta_cols, with = FALSE]
  
  exp_tbl <- merge(base, meta_sub, by = "Player", all.x = TRUE)
  
  if ("DKOwn" %in% names(exp_tbl)) {
    if (max(exp_tbl$DKOwn, na.rm = TRUE) <= 1) exp_tbl[, DKOwn := round(DKOwn * 100, 1)]
    setnames(exp_tbl, "DKOwn", "OwnProj")
    exp_tbl[, PPD := round(OwnProj / (DKSalary / 1000), 2)]
  }
  if ("DKSalary" %in% names(exp_tbl)) setnames(exp_tbl, "DKSalary", "Salary")
  
  exp_tbl[, TotalExp := FieldExp + YourExp]
  setorder(exp_tbl, -TotalExp)
  exp_tbl[, TotalExp := NULL]
  
  keep <- intersect(c("Player", "Salary", "OwnProj", "PPD", "FieldExp", "YourExp"), names(exp_tbl))
  exp_tbl[, keep, with = FALSE]
}


# ============================================================================
# UI
# ============================================================================

render_cash_game_tab_ui <- function() {
  tagList(
    div(style = "padding:16px;",
        
        # ── Platform selector (shown when FD lineups also available) ─────────────
        uiOutput("du_platform_selector_ui"),
        
        # ── Info strip ──────────────────────────────────────────────────────────
        div(style = paste0("display:flex;align-items:center;gap:0;background:#141414;",
                           "border:1px solid #222;border-radius:6px;overflow:hidden;",
                           "margin-bottom:10px;height:42px;"),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;border-right:1px solid #222;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Mode"),
                uiOutput("du_mode_desc_ui")
            ),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;border-right:1px solid #222;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Field"),
                uiOutput("du_field_desc_ui")
            ),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;border-right:1px solid #222;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Your Pool"),
                uiOutput("du_yours_desc_ui")
            ),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Cash Line"),
                uiOutput("du_cashline_desc_ui")
            )
        ),
        
        # ── Action button ────────────────────────────────────────────────────────
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:16px;",
            actionButton("du_run", "Run Double Up",
                         class = "btn-primary", icon = icon("play"),
                         style = "height:38px;font-size:12px;font-weight:700;")
        ),
        
        uiOutput("du_status_msg"),
        
        conditionalPanel(
          condition = "output.du_has_results == true",
          
          box(width = NULL, title = "Driver Exposure \u2014 Field vs Your Top 50",
              status = "primary", solidHeader = TRUE,
              DTOutput("du_exposure_tbl") %>%
                shinycssloaders::withSpinner(color = "#FFE500", type = 6)
          ),
          
          box(width = NULL,
              title = uiOutput("du_results_title"),
              status = "primary", solidHeader = TRUE,
              div(style = "margin-bottom:10px;",
                  downloadButton("du_download", "Download All Lineups",
                                 class = "btn-primary",
                                 style = "height:32px;font-size:11px;")
              ),
              DTOutput("du_results_tbl") %>%
                shinycssloaders::withSpinner(color = "#FFE500", type = 6)
          )
        )
    )
  )
}


# ============================================================================
# SERVER
# ============================================================================

register_cash_game_observers <- function(input, output, session, rv) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  du_rv <- reactiveValues(
    results     = NULL,
    exposure    = NULL,
    status      = NULL,
    has_results = FALSE,
    platform    = NULL,
    id_col      = NULL
  )
  
  output$du_has_results <- reactive({ isTRUE(du_rv$has_results) })
  outputOptions(output, "du_has_results", suspendWhenHidden = FALSE)
  
  # ── Platform selector ────────────────────────────────────────────────────────
  output$du_platform_selector_ui <- renderUI({
    req(rv$config)
    plats <- intersect(c("DK","FD","SD"), Filter(function(p) {
      !is.null(switch(p, DK=rv$dk_optimal_lineups, FD=rv$fd_optimal_lineups,
                      SD=rv$sd_optimal_lineups, NULL))
    }, c("DK","FD","SD")))
    if (length(plats) <= 1) return(NULL)
    
    div(class = "gts-platform-pills", style = "margin-bottom:12px;",
        span(class = "gts-sr-label", style = "margin-right:6px;", "Platform:"),
        lapply(plats, function(p) {
          tags$button(
            class   = paste("gts-pill du-plat-pill", if (p == plats[1]) "active" else ""),
            onclick = sprintf(
              "Shiny.setInputValue('du_platform','%s',{priority:'event'});
               document.querySelectorAll('.du-plat-pill').forEach(function(b){b.classList.remove('active')});
               this.classList.add('active')", p),
            p
          )
        })
    )
  })
  
  # Resolve active cash platform (default to first available)
  du_platform <- reactive({
    p <- input$du_platform
    if (!is.null(p) && p %in% c("DK","FD","SD")) return(p)
    if (!is.null(rv$dk_optimal_lineups)) return("DK")
    if (!is.null(rv$fd_optimal_lineups)) return("FD")
    if (!is.null(rv$sd_optimal_lineups)) return("SD")
    "DK"
  })
  
  output$du_mode_desc_ui <- renderUI({
    plat_label <- switch(du_platform(), FD = "FanDuel", SD = "Showdown (DK)", "DraftKings")
    span(style = "color:#FFE500;font-weight:700;font-size:13px;",
         paste("Double Up \u2014", plat_label))
  })
  
  output$du_cashline_desc_ui <- renderUI({
    cash_p <- get_cash_params(rv$config %||% list())
    cash_n <- floor(cash_p$total_lineups * 0.45)
    span(style = "color:#aaa;font-size:12px;",
         sprintf("Top 45\u202f\u2022\u202f%d of %d cash", cash_n, cash_p$total_lineups))
  })
  
  output$du_field_desc_ui <- renderUI({
    cash_p <- get_cash_params(rv$config %||% list())
    is_nba <- isTRUE(rv$config$sport_name == "NBA")
    plat   <- du_platform()
    desc   <- if (plat == "SD")     paste0("Top ", cash_p$n_field, " from tournament pool")
    else if (is_nba)      paste0("Top ", cash_p$n_field, " LP-optimized (ETR projections)")
    else                  paste0("Top ", cash_p$n_field, " by AvgOwn (top ", cash_p$top_n_ppd, " PPD players)")
    span(style = "color:#aaa;font-size:12px;", desc)
  })
  
  output$du_yours_desc_ui <- renderUI({
    cash_p <- get_cash_params(rv$config %||% list())
    span(style = "color:#aaa;font-size:12px;",
         paste0("Top ", cash_p$n_yours, " by median score"))
  })
  
  
  # ── Run Double Up (field generation + scoring in one step) ──────────────────
  observeEvent(input$du_run, {
    plat    <- du_platform()
    opt_lus <- switch(plat,
                      DK = rv$dk_optimal_lineups,
                      FD = rv$fd_optimal_lineups,
                      SD = rv$sd_optimal_lineups,
                      NULL)
    req(opt_lus, rv$simulation_results, rv$sim_metadata, rv$config)
    
    du_rv$has_results <- FALSE
    du_rv$results     <- NULL
    du_rv$exposure    <- NULL
    
    progress <- Progress$new(session, min = 0, max = 1)
    progress$set(message = "Running Double Up...", value = 0.02)
    on.exit(progress$close())
    
    tryCatch({
      
      t_total  <- Sys.time()
      cash_p   <- get_cash_params(rv$config)
      is_nba   <- isTRUE(rv$config$sport_name == "NBA")
      
      constraints <- get_dk_constraints(rv$config)
      sal_cap     <- switch(plat,
                            FD = rv$config$salary_caps$FD %||% 60000,
                            SD = rv$config$salary_caps$SD %||% 50000,
                            constraints$salary_cap)
      r_size      <- constraints$roster_size
      sal_floor   <- 49000
      meta_raw    <- as.data.table(copy(rv$sim_metadata))
      
      # ── Step 0: Build field lineups ───────────────────────────────────────
      cat(sprintf("\n  [DoubleUp] Step 0: Building %s field lineups...\n", plat))
      flush.console()
      progress$set(detail = "Step 0: Building field...", value = 0.05)
      
      if (plat == "SD") {
        # SD field: matrix-score all tournament SD lineups on DKScore, take top
        # n_field by true median — same matrix-multiply path as NBA "yours" pool.
        sd_pool <- copy(as.data.table(opt_lus))
        if (!"LineupID" %in% names(sd_pool)) sd_pool[, LineupID := paste0("GPP", seq_len(.N))]
        sd_pc  <- get_player_cols(sd_pool)
        sd_std <- paste0("Player", seq_len(length(sd_pc)))
        if (!identical(sd_pc, sd_std)) setnames(sd_pool, sd_pc, sd_std)
        
        sd_sim_res     <- copy(as.data.table(rv$simulation_results))
        sd_all_players <- unique(sd_sim_res$Player)
        sd_player_idx  <- setNames(seq_along(sd_all_players), sd_all_players)
        sd_n_gpp       <- nrow(sd_pool)
        
        score_wide_sd  <- dcast(sd_sim_res[, .(SimID, Player, DKScore)],
                                Player ~ SimID, value.var = "DKScore", fill = 0)
        score_mat_sd   <- as.matrix(score_wide_sd[, -1, with = FALSE])
        rownames(score_mat_sd) <- score_wide_sd$Player
        
        cat(sprintf("  [DoubleUp] Step 0: Matrix-scoring %d SD lineups...\n", sd_n_gpp))
        flush.console()
        
        chunk_sz_sd <- 500L; n_chunks_sd <- ceiling(sd_n_gpp / chunk_sz_sd)
        sd_med <- numeric(sd_n_gpp)
        for (ci in seq_len(n_chunks_sd)) {
          idx_s <- (ci - 1L) * chunk_sz_sd + 1L
          idx_e <- min(ci * chunk_sz_sd, sd_n_gpp)
          chunk <- sd_pool[idx_s:idx_e]
          mem   <- matrix(0L, nrow = nrow(chunk), ncol = length(sd_all_players))
          colnames(mem) <- sd_all_players
          for (pc in sd_std) {
            p_idx <- sd_player_idx[chunk[[pc]]]; valid <- !is.na(p_idx)
            mem[cbind(which(valid), p_idx[valid])] <- 1L
          }
          sd_med[idx_s:idx_e] <- apply(mem %*% score_mat_sd, 1, median)
        }
        sd_pool[, MedianScore := sd_med]
        setorder(sd_pool, -MedianScore)
        
        field_pool <- head(sd_pool, cash_p$n_field)
        field_pool[, LineupID    := paste0("F", seq_len(.N))]
        if (!"TotalSalary" %in% names(field_pool)) field_pool[, TotalSalary := NA_real_]
        field_pool[, TotalSalary := as.numeric(TotalSalary)]
        field_pool[, AvgOwn      := NA_real_]
        cat(sprintf("  [DoubleUp] Step 0: SD field median range %.1f - %.1f\n",
                    min(field_pool$MedianScore, na.rm=TRUE),
                    max(field_pool$MedianScore, na.rm=TRUE)))
        flush.console()
      } else if (is_nba) {
        proj_col <- if (plat == "FD") "FDProj" else "DKProj"
        if (!proj_col %in% names(meta_raw))
          stop(proj_col, " not found in metadata — NBA field requires ETR projections.")
        field_pool <- generate_field_lineups_nba(
          metadata   = meta_raw,
          n          = cash_p$n_field,
          salary_cap = sal_cap,
          platform   = plat
        )
      } else {
        if (!"DKOwn" %in% names(meta_raw))
          stop("DKOwn not found in metadata.")
        field_pool <- generate_field_lineups(
          metadata     = meta_raw,
          n            = cash_p$n_field,
          salary_cap   = sal_cap,
          salary_floor = sal_floor,
          roster_size  = r_size,
          top_n_ppd    = cash_p$top_n_ppd
        )
      }
      cat(sprintf("  [DoubleUp] Step 0 done — %d field lineups\n", nrow(field_pool)))
      flush.console()
      progress$set(detail = "Step 0: Field built.", value = 0.12)
      
      score_col <- switch(plat, FD = "FDScore", SD = "DKScore", "DKScore")
      sal_col   <- switch(plat, FD = "FDSalary", SD = "SDSalary", "DKSalary")
      id_col    <- switch(plat, FD = "FDID", SD = "DKID", "DKID")
      
      dk_opt  <- copy(as.data.table(opt_lus))
      sim_res <- copy(as.data.table(rv$simulation_results))
      meta    <- copy(meta_raw)
      
      player_cols <- get_player_cols(dk_opt)
      r_size      <- length(player_cols)
      std_cols    <- paste0("Player", seq_len(r_size))
      n_sims      <- length(unique(sim_res$SimID))
      n_gpp       <- nrow(dk_opt)
      
      cat(sprintf("  [DoubleUp] %s | %s sims | %d GPP lineups | %d field lineups\n",
                  plat, format(n_sims, big.mark = ","), n_gpp, nrow(field_pool)))
      flush.console()
      
      if (!"LineupID" %in% names(dk_opt)) dk_opt[, LineupID := paste0("GPP", seq_len(.N))]
      
      # ── Step 1: Rank GPP pool by true median score ────────────────────────
      # For NBA: always matrix-score — WinRate/Top1Count are GPP metrics, not
      # useful proxies for cash median.
      # For other sports: use pre-computed MedianScore/AvgScore if available.
      cat(sprintf("  [DoubleUp] Step 1/5: Ranking %s GPP pool by median score...\n", plat))
      flush.console()
      progress$set(detail = "Step 1/5: Ranking GPP pool...", value = 0.08)
      t1 <- Sys.time()
      
      is_nba_cash <- isTRUE(rv$config$sport_name == "NBA")
      rank_col    <- NULL
      
      if (!is_nba_cash) {
        for (candidate in c("MedianScore", "AvgScore")) {
          if (candidate %in% names(dk_opt)) { rank_col <- candidate; break }
        }
      }
      
      if (!is.null(rank_col)) {
        cat(sprintf("  [DoubleUp] Step 1/5: Using pre-computed '%s' column\n", rank_col))
        flush.console()
        if (!"MedianScore" %in% names(dk_opt)) {
          setnames(dk_opt, rank_col, "MedianScore"); rank_col <- "MedianScore"
        }
      } else {
        if (is_nba_cash) {
          cat(sprintf("  [DoubleUp] Step 1/5: NBA cash — matrix scoring on %s\n", score_col))
        } else {
          cat("  [DoubleUp] Step 1/5: No pre-computed column — matrix scoring\n")
        }
        flush.console()
        
        if (!score_col %in% names(sim_res))
          stop(score_col, " not found in sim results.")
        
        all_players <- unique(sim_res$Player)
        n_players   <- length(all_players)
        player_idx  <- setNames(seq_along(all_players), all_players)
        
        score_wide <- dcast(sim_res[, c("SimID","Player", score_col), with=FALSE],
                            Player ~ SimID, value.var = score_col, fill = 0)
        score_mat  <- as.matrix(score_wide[, -1, with = FALSE])
        rownames(score_mat) <- score_wide$Player
        
        chunk_sz   <- 500L
        n_chunks   <- ceiling(n_gpp / chunk_sz)
        med_scores <- numeric(n_gpp)
        
        for (ci in seq_len(n_chunks)) {
          idx_s <- (ci - 1L) * chunk_sz + 1L
          idx_e <- min(ci * chunk_sz, n_gpp)
          chunk <- dk_opt[idx_s:idx_e]
          
          mem_mat <- matrix(0L, nrow = nrow(chunk), ncol = n_players)
          colnames(mem_mat) <- all_players
          for (pc in player_cols) {
            p_idx <- player_idx[chunk[[pc]]]; valid <- !is.na(p_idx)
            mem_mat[cbind(which(valid), p_idx[valid])] <- 1L
          }
          totals_mat             <- mem_mat %*% score_mat
          med_scores[idx_s:idx_e] <- apply(totals_mat, 1, median)
          
          cat(sprintf("\r  [DoubleUp] Step 1/5: %d%%", round(idx_e / n_gpp * 100)))
          flush.console()
        }
        cat("\n"); flush.console()
        dk_opt[, MedianScore := med_scores]
        rank_col <- "MedianScore"
      }
      
      cat(sprintf("  [DoubleUp] Step 1/5 done in %.1fs\n",
                  as.numeric(difftime(Sys.time(), t1, units = "secs"))))
      flush.console()
      
      # ── Step 2: Select your top N lineups by median ───────────────────────
      cash_p <- get_cash_params(rv$config)
      cat(sprintf("  [DoubleUp] Step 2/5: Selecting your top %d lineups by median...\n", cash_p$n_yours))
      flush.console()
      progress$set(detail = sprintf("Step 2/5: Selecting your top %d by median...", cash_p$n_yours), value = 0.18)
      
      setorder(dk_opt, -MedianScore)
      your_pool <- head(dk_opt, cash_p$n_yours)
      your_pool[, LineupID := paste0("Y", seq_len(.N))]
      
      if (!identical(player_cols, std_cols)) setnames(your_pool, player_cols, std_cols)
      
      if (!"TotalSalary" %in% names(your_pool) && sal_col %in% names(meta)) {
        your_pool[, TotalSalary := rowSums(
          sapply(std_cols, function(col) meta[match(your_pool[[col]], meta$Player), get(sal_col)]),
          na.rm = TRUE
        )]
      } else if (!"TotalSalary" %in% names(your_pool)) {
        your_pool[, TotalSalary := NA_real_]
      }
      
      cat(sprintf("  [DoubleUp] Step 2/5 done — median range: %.1f - %.1f\n",
                  min(your_pool$MedianScore, na.rm = TRUE),
                  max(your_pool$MedianScore, na.rm = TRUE)))
      flush.console()
      
      # ── Step 3: Combine pools ─────────────────────────────────────────────
      cat(sprintf("  [DoubleUp] Step 3/5: Combining %d your + %d field = %d lineups...\n",
                  cash_p$n_yours, cash_p$n_field, cash_p$total_lineups))
      flush.console()
      progress$set(detail = "Step 3/5: Combining lineup pools...", value = 0.25)
      
      field_pool[, TotalSalary := as.numeric(TotalSalary)]
      your_avg_own <- if ("AvgOwn" %in% names(your_pool)) your_pool$AvgOwn else NA_real_
      
      keep_cols <- c("LineupID", std_cols, "TotalSalary")
      combined  <- rbindlist(
        list(your_pool[, keep_cols, with = FALSE],
             field_pool[, keep_cols, with = FALSE]),
        use.names = TRUE
      )
      lineup_ids <- combined$LineupID
      cat(sprintf("  [DoubleUp] Step 3/5 done — %d total lineups\n", nrow(combined)))
      flush.console()
      
      # ── Step 4: Score all lineups ─────────────────────────────────────────
      cat(sprintf("  [DoubleUp] Step 4/5: Scoring %d lineups x %s sims...\n",
                  nrow(combined), format(n_sims, big.mark = ",")))
      flush.console()
      progress$set(detail = sprintf("Step 4/5: Scoring %d lineups x %s sims...",
                                    nrow(combined), format(n_sims, big.mark = ",")),
                   value = 0.30)
      
      lineup_data_for_scoring <- make_lineup_data(combined, sim_res, std_cols, score_col)
      score_mat_cash <- score_all_lineups(lineup_data_for_scoring, sim_res, verbose = TRUE)
      
      progress$set(detail = "Step 4/5: Scoring complete.", value = 0.55)
      
      # ── Step 5: Cash rates ────────────────────────────────────────────────
      cat(sprintf("  [DoubleUp] Step 5/5: Computing cash rates (45%% line)...\n"))
      flush.console()
      progress$set(detail = "Step 5/5: Computing cash rates...", value = 0.57)
      
      metrics <- cash_rate_from_score_matrix(
        score_matrix = score_mat_cash,
        lineup_ids   = lineup_ids,
        cash_pct     = 0.45,
        verbose      = TRUE
      )
      progress$set(detail = "Step 5/5: Cash rates complete.", value = 0.92)
      
      # ── Assemble results ──────────────────────────────────────────────────
      cat("  [DoubleUp] Assembling results table...\n"); flush.console()
      
      results <- merge(combined, metrics, by = "LineupID")
      results[, Source := ifelse(grepl("^Y", LineupID), "Yours", "Field")]
      
      field_own <- field_pool[, .(LineupID, AvgOwn)]
      your_own  <- data.table(LineupID = your_pool$LineupID,
                              AvgOwn   = if (all(is.na(your_avg_own))) NA_real_ else your_avg_own)
      all_own   <- rbindlist(list(your_own, field_own), use.names = TRUE)
      results   <- merge(results, all_own, by = "LineupID", all.x = TRUE)
      
      setcolorder(results, c("LineupID", "Source", std_cols,
                             "TotalSalary", "AvgOwn", "MedianScore", "CashRate"))
      setorder(results, -CashRate, -MedianScore)
      
      # Rename Player1..N to slot labels matching the platform
      slot_labels <- get_slot_labels(rv$config, length(std_cols))
      if (!is.null(slot_labels)) {
        for (i in seq_along(std_cols))
          if (std_cols[i] %in% names(results))
            setnames(results, std_cols[i], slot_labels[i])
      }
      
      # ── Exposure ──────────────────────────────────────────────────────────
      progress$set(detail = "Building exposure table...", value = 0.95)
      cat("  [DoubleUp] Building exposure table...\n"); flush.console()
      
      # For exposure, use platform-appropriate salary/own columns
      exp_meta <- copy(meta)
      if (sal_col %in% names(exp_meta) && sal_col != "DKSalary")
        setnames(exp_meta, sal_col, "DKSalary")
      own_col_exp <- if (plat == "FD") "FDOwn" else "DKOwn"
      if (own_col_exp %in% names(exp_meta) && own_col_exp != "DKOwn")
        setnames(exp_meta, own_col_exp, "DKOwn")
      
      exposure <- build_combined_exposure(field_pool, your_pool, exp_meta, std_cols)
      
      # ── Store & report ────────────────────────────────────────────────────
      du_rv$results     <- results
      du_rv$exposure    <- exposure
      du_rv$platform    <- plat
      du_rv$id_col      <- id_col
      du_rv$has_results <- TRUE
      
      elapsed_total <- as.numeric(difftime(Sys.time(), t_total, units = "secs"))
      status_msg <- sprintf(
        "Double Up complete \u2014 %s | %d lineups (%d field + %d yours) | %s sims | %.1fs",
        plat, nrow(combined), cash_p$n_field, cash_p$n_yours,
        format(n_sims, big.mark = ","), elapsed_total
      )
      du_rv$status <- status_msg
      
      cat(sprintf("  [DoubleUp] Complete in %.1fs\n\n", elapsed_total)); flush.console()
      progress$set(detail = "Done!", value = 1)
      showNotification("Double Up simulation complete!", type = "message")
      
    }, error = function(e) {
      du_rv$status <- paste("Error:", e$message)
      showNotification(paste("Double Up error:", e$message), type = "error", duration = 10)
      cat("  [DoubleUp] ERROR:\n"); print(e)
    })
  })
  
  
  # ── Status ───────────────────────────────────────────────────────────────────
  output$du_status_msg <- renderUI({
    msg <- du_rv$status
    if (is.null(msg)) {
      has_any <- !is.null(rv$dk_optimal_lineups) || !is.null(rv$fd_optimal_lineups)
      if (!has_any)
        return(div(style = "color:#666;font-size:12px;padding:8px 0;",
                   icon("info-circle"),
                   " Score Tournament Lineups first, then Generate Field and Run Double Up."))
      return(div(style = "color:#666;font-size:12px;padding:8px 0;",
                 icon("info-circle"),
                 " Click Run Double Up to build the field and score your lineups."))
    }
    div(class = "gts-sim-done", icon("check-circle"), msg)
  })
  
  
  # ── Results title ─────────────────────────────────────────────────────────────
  output$du_results_title <- renderUI({
    req(du_rv$results)
    n_y <- sum(du_rv$results$Source == "Yours")
    n_f <- sum(du_rv$results$Source == "Field")
    span(sprintf("All %d Lineups Ranked by Cash Rate  (%d Yours / %d Field)",
                 nrow(du_rv$results), n_y, n_f),
         style = "color:#FFE500;")
  })
  
  
  # ── Exposure table ────────────────────────────────────────────────────────────
  output$du_exposure_tbl <- renderDT({
    req(du_rv$exposure)
    dt <- copy(du_rv$exposure)
    datatable(dt, rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE,
                             searching = FALSE, lengthChange = FALSE, dom = "tp"),
              class = "stripe hover compact") %>%
      { if ("Salary"  %in% names(dt)) formatCurrency(., "Salary",  "$", digits = 0) else . } %>%
      { if ("OwnProj" %in% names(dt)) formatRound(.,   "OwnProj", 1)                else . } %>%
      { if ("PPD"     %in% names(dt)) formatRound(.,   "PPD",     2)                else . } %>%
      formatRound(intersect(c("FieldExp", "YourExp"), names(dt)), 1) %>%
      formatStyle("FieldExp",
                  background = styleColorBar(c(0, 100), "rgba(255,229,0,0.35)"),
                  backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "left") %>%
      formatStyle("YourExp",
                  background = styleColorBar(c(0, 100), "rgba(74,144,217,0.45)"),
                  backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "left")
  })
  
  
  # ── Results table ─────────────────────────────────────────────────────────────
  output$du_results_tbl <- renderDT({
    req(du_rv$results)
    dt <- copy(du_rv$results)
    datatable(dt, rownames = FALSE,
              options = list(pageLength = 60, scrollX = TRUE, scrollY = "520px",
                             searching = TRUE, lengthChange = FALSE, dom = "ftp",
                             order = list(list(ncol(dt) - 1L, "desc"))),
              class = "stripe hover compact") %>%
      { if ("TotalSalary" %in% names(dt)) formatCurrency(., "TotalSalary", "$", digits = 0) else . } %>%
      { if ("AvgOwn"      %in% names(dt)) formatRound(.,    "AvgOwn",      2)               else . } %>%
      formatRound("MedianScore", 1) %>%
      # No color bar on CashRate (NBA) — plain numeric display
      formatStyle("Source",
                  color      = styleEqual(c("Yours", "Field"), c("#FFE500", "#aaaaaa")),
                  fontWeight = styleEqual(c("Yours", "Field"), c("700",     "400")))
  })
  
  
  # ── Download ──────────────────────────────────────────────────────────────────
  output$du_download <- downloadHandler(
    filename = function() paste0("GTS_DoubleUp_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content  = function(file) {
      req(du_rv$results, du_rv$exposure, rv$sim_metadata)
      meta   <- as.data.table(rv$sim_metadata)
      dl     <- copy(du_rv$results)
      id_col <- du_rv$id_col %||% "DKID"
      plat   <- du_rv$platform %||% "DK"
      
      # Detect player slot columns by name pattern
      slot_cols <- grep("^(PG|SG|SF|PF|C$|^G$|^F$|UTIL|Player[0-9]+|Captain|Util[0-9]+)",
                        names(dl), value = TRUE)
      if (length(slot_cols) > 0) {
        if (plat == "SD") {
          # SD format: "Name (CPTID)" for Captain, "Name (SDID)" for Util slots
          if ("CPTID" %in% names(meta)) {
            cpt_lu <- setNames(meta$CPTID, meta$Player)
            if ("Captain" %in% names(dl))
              dl$Captain <- paste0(dl$Captain, " (", cpt_lu[dl$Captain], ")")
          }
          if ("SDID" %in% names(meta)) {
            sd_lu <- setNames(meta$SDID, meta$Player)
            for (col in grep("^Util[0-9]+", names(dl), value = TRUE))
              dl[[col]] <- paste0(dl[[col]], " (", sd_lu[dl[[col]]], ")")
          }
        } else if (id_col %in% names(meta)) {
          id_lu <- setNames(meta[[id_col]], meta$Player)
          if (plat == "FD") {
            for (col in slot_cols)
              dl[[col]] <- paste0(id_lu[dl[[col]]], ":", dl[[col]])
          } else {
            for (col in slot_cols)
              dl[[col]] <- paste0(dl[[col]], " (", id_lu[dl[[col]]], ")")
          }
        }
      }
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Lineups Ranked")
      openxlsx::writeData(wb, "Lineups Ranked", as.data.frame(dl))
      openxlsx::addWorksheet(wb, "Exposure")
      openxlsx::writeData(wb, "Exposure", as.data.frame(du_rv$exposure))
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}
# end of cash_game_module.R