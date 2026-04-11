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
# SCORING
#
# Uses score_all_lineups() from OptimalLineups_Core.R — the same matrix
# multiply approach the tournament process uses. No cartesian joins.
# Handles 550 lineups x 50k sims comfortably in memory (~210MB).
#
# cash_rate_from_score_matrix: replaces compute_doubleup_metrics.
# Takes the score_matrix directly, ranks per sim, computes cash rate.
# ============================================================================

#' Wrap a flat lineup pool into the lineup_data list format score_all_lineups expects.
make_lineup_data <- function(lineup_pool, sim_results, player_cols) {
  list(
    unique_lineups = lineup_pool[, player_cols, with = FALSE],
    n_sims         = length(unique(sim_results$SimID)),
    config         = list(platform_col = "DKScore", percentiles = c(0.01, 0.05, 0.10, 0.20)),
    mode           = "standard",
    platform_col   = "DKScore"
  )
}


#' Compute double-up cash rates directly from a score matrix.
#' @param score_matrix  numeric matrix: n_lineups x n_sims (from score_all_lineups)
#' @param lineup_ids    character vector of LineupID in same row order as score_matrix
#' @param cash_pct      numeric 0-1, fraction that cash (0.45 for double up)
#' @param verbose       logical
#' @return data.table: LineupID, MedianScore, CashRate
cash_rate_from_score_matrix <- function(score_matrix, lineup_ids,
                                        cash_pct = 0.45, verbose = TRUE) {
  n_lineups <- nrow(score_matrix)
  n_sims    <- ncol(score_matrix)
  cash_rank <- floor(n_lineups * cash_pct)
  
  if (verbose) {
    cat(sprintf("  [Cash] %d lineups x %s sims | cash line: top %d (%.0f%%)\n",
                n_lineups, format(n_sims, big.mark = ","), cash_rank, cash_pct * 100))
    flush.console()
  }
  
  t0 <- Sys.time()
  
  # Chunk through sims to accumulate cash counts — same pattern as tournament Phase 3
  chunk_size  <- 2000L
  n_chunks    <- ceiling(n_sims / chunk_size)
  cash_counts <- integer(n_lineups)
  
  for (chunk_idx in seq_len(n_chunks)) {
    chunk_start  <- (chunk_idx - 1L) * chunk_size + 1L
    chunk_end    <- min(chunk_idx * chunk_size, n_sims)
    chunk_scores <- score_matrix[, chunk_start:chunk_end, drop = FALSE]
    
    # Rank per sim: rank(-x) gives 1 = best
    rank_mat <- apply(chunk_scores, 2L, function(x) rank(-x, ties.method = "min"))
    # Count how many sims each lineup finished at or above the cash line
    cash_counts <- cash_counts + rowSums(rank_mat <= cash_rank)
    
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
  
  # Median score per lineup — vectorized rowMedians via apply
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
        
        # ── Info strip ──────────────────────────────────────────────────────────
        div(style = paste0("display:flex;align-items:center;gap:0;background:#141414;",
                           "border:1px solid #222;border-radius:6px;overflow:hidden;",
                           "margin-bottom:10px;height:42px;"),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;border-right:1px solid #222;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Mode"),
                span(style = "color:#FFE500;font-weight:700;font-size:13px;", "Double Up \u2014 DraftKings")
            ),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;border-right:1px solid #222;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Field"),
                span(style = "color:#aaa;font-size:12px;", "Top 500 by AvgOwn (top 20 PPD players)")
            ),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;border-right:1px solid #222;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Your Pool"),
                span(style = "color:#aaa;font-size:12px;", "Top 50 by median score")
            ),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Cash Line"),
                span(style = "color:#aaa;font-size:12px;", "Top 45%  \u2022  248 of 550 cash")
            )
        ),
        
        # ── Action buttons ───────────────────────────────────────────────────────
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:16px;",
            actionButton("du_generate_field", "Generate Field",
                         class = "btn-primary", icon = icon("random"),
                         style = "height:38px;font-size:12px;font-weight:700;"),
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
    field_pool  = NULL,
    results     = NULL,
    exposure    = NULL,
    status      = NULL,
    has_results = FALSE
  )
  
  output$du_has_results <- reactive({ isTRUE(du_rv$has_results) })
  outputOptions(output, "du_has_results", suspendWhenHidden = FALSE)
  
  
  # ── Generate Field ──────────────────────────────────────────────────────────
  observeEvent(input$du_generate_field, {
    req(rv$sim_metadata, rv$config)
    
    meta <- as.data.table(copy(rv$sim_metadata))
    if (!"DKOwn" %in% names(meta)) {
      showNotification("DKOwn not found in metadata.", type = "error"); return()
    }
    
    constraints  <- get_dk_constraints(rv$config)
    sal_cap      <- constraints$salary_cap
    r_size       <- constraints$roster_size
    sal_floor    <- 49000  # $49k floor works for both NASCAR and MMA on a $50k cap
    
    du_rv$has_results <- FALSE
    du_rv$results     <- NULL
    du_rv$exposure    <- NULL
    
    progress <- Progress$new(session, min = 0, max = 1)
    progress$set(message = "Generating field lineups...", value = 0.1)
    on.exit(progress$close())
    
    tryCatch({
      progress$set(detail = "Computing PPD, building combinations...", value = 0.2)
      
      field <- generate_field_lineups(
        metadata     = meta,
        n            = 500L,
        salary_cap   = sal_cap,
        salary_floor = sal_floor,
        roster_size  = r_size,
        top_n_ppd    = 20L
      )
      
      du_rv$field_pool <- field
      
      # Build a summary of the eligible pool for the status message
      pool_summary <- sprintf(
        "Field ready: %d lineups generated from top 20 PPD players | AvgOwn range: %.1f%% - %.1f%%",
        nrow(field), min(field$AvgOwn), max(field$AvgOwn)
      )
      du_rv$status <- pool_summary
      progress$set(value = 1, detail = "Done")
      showNotification(sprintf("Field generated: %d lineups.", nrow(field)), type = "message")
      
    }, error = function(e) {
      du_rv$status <- paste("Field generation error:", e$message)
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      cat("Field generation error:\n"); print(e)
    })
  })
  
  
  # ── Run Double Up ───────────────────────────────────────────────────────────
  observeEvent(input$du_run, {
    req(rv$dk_optimal_lineups, rv$simulation_results, rv$sim_metadata, du_rv$field_pool)
    
    du_rv$has_results <- FALSE
    du_rv$results     <- NULL
    du_rv$exposure    <- NULL
    
    progress <- Progress$new(session, min = 0, max = 1)
    progress$set(message = "Running double-up simulation...", value = 0.05)
    on.exit(progress$close())
    
    tryCatch({
      
      t_total <- Sys.time()
      
      dk_opt  <- copy(as.data.table(rv$dk_optimal_lineups))
      sim_res <- copy(as.data.table(rv$simulation_results))
      meta    <- copy(as.data.table(rv$sim_metadata))
      
      player_cols <- get_player_cols(dk_opt)
      r_size      <- length(player_cols)
      std_cols    <- paste0("Player", seq_len(r_size))
      n_sims      <- length(unique(sim_res$SimID))
      n_gpp       <- nrow(dk_opt)
      
      cat(sprintf("\n  [DoubleUp] Starting | %s sims | %d GPP lineups | %d field lineups\n",
                  format(n_sims, big.mark = ","), n_gpp, nrow(du_rv$field_pool)))
      flush.console()
      
      if (!"LineupID" %in% names(dk_opt)) dk_opt[, LineupID := paste0("GPP", seq_len(.N))]
      
      # ── Step 1: Rank GPP pool by median score ─────────────────────────────
      # Prefer pre-computed columns from tournament scoring — no re-scoring needed.
      # AvgScore (mean) is an acceptable proxy for median and is always present.
      # Only fall back to matrix scoring if neither column exists.
      cat("  [DoubleUp] Step 1/5: Ranking GPP pool by median score...\n")
      flush.console()
      progress$set(detail = "Step 1/5: Ranking GPP pool...", value = 0.08)
      t1 <- Sys.time()
      
      rank_col <- NULL
      for (candidate in c("MedianScore", "AvgScore", "Top1Count", "WinRate")) {
        if (candidate %in% names(dk_opt)) { rank_col <- candidate; break }
      }
      
      if (!is.null(rank_col)) {
        # Fast path: use pre-computed tournament metric
        cat(sprintf("  [DoubleUp] Step 1/5: Using pre-computed '%s' column (no re-scoring needed)\n",
                    rank_col))
        flush.console()
        if (!"MedianScore" %in% names(dk_opt)) {
          setnames(dk_opt, rank_col, "MedianScore")
          rank_col <- "MedianScore"
        }
      } else {
        # Fallback: matrix scoring — avoids the cartesian join memory explosion.
        # Score matrix: n_players x n_sims, then lineup membership matrix: n_lineups x n_players.
        cat(sprintf("  [DoubleUp] Step 1/5: No pre-computed column found — using matrix scoring\n"))
        flush.console()
        
        all_players  <- unique(sim_res$Player)
        n_players    <- length(all_players)
        player_idx   <- setNames(seq_along(all_players), all_players)
        
        # Score matrix: players x sims
        score_wide <- dcast(sim_res[, .(SimID, Player, DKScore)],
                            Player ~ SimID, value.var = "DKScore", fill = 0)
        score_mat  <- as.matrix(score_wide[, -1, with = FALSE])
        rownames(score_mat) <- score_wide$Player
        sim_id_cols <- colnames(score_mat)
        
        # Membership matrix: lineups x players (chunk to avoid large alloc)
        chunk_sz  <- 500L
        n_chunks  <- ceiling(n_gpp / chunk_sz)
        med_scores <- numeric(n_gpp)
        
        for (ci in seq_len(n_chunks)) {
          idx_s <- (ci - 1L) * chunk_sz + 1L
          idx_e <- min(ci * chunk_sz, n_gpp)
          chunk <- dk_opt[idx_s:idx_e]
          
          mem_mat <- matrix(0L, nrow = nrow(chunk), ncol = n_players)
          colnames(mem_mat) <- all_players
          for (pc in player_cols) {
            p_idx <- player_idx[chunk[[pc]]]
            valid <- !is.na(p_idx)
            mem_mat[cbind(which(valid), p_idx[valid])] <- 1L
          }
          
          totals_mat        <- mem_mat %*% score_mat   # chunk x n_sims
          med_scores[idx_s:idx_e] <- apply(totals_mat, 1, median)
          
          cat(sprintf("\r  [DoubleUp] Step 1/5: matrix scoring %d%%",
                      round(idx_e / n_gpp * 100)))
          flush.console()
        }
        cat("\n"); flush.console()
        dk_opt[, MedianScore := med_scores]
        rank_col <- "MedianScore"
      }
      
      cat(sprintf("  [DoubleUp] Step 1/5 done in %.1fs\n",
                  as.numeric(difftime(Sys.time(), t1, units = "secs"))))
      flush.console()
      
      # ── Step 2: Select your top 20 ────────────────────────────────────────
      cat("  [DoubleUp] Step 2/5: Selecting your top 50 lineups by median...\n")
      flush.console()
      progress$set(detail = "Step 2/5: Selecting your top 50 by median...", value = 0.18)
      
      setorder(dk_opt, -MedianScore)
      your_pool <- head(dk_opt, 50L)
      your_pool[, LineupID := paste0("Y", seq_len(.N))]
      
      if (!identical(player_cols, std_cols)) setnames(your_pool, player_cols, std_cols)
      
      if (!"TotalSalary" %in% names(your_pool) && "DKSalary" %in% names(meta)) {
        your_pool[, TotalSalary := rowSums(
          sapply(std_cols, function(col) meta[match(your_pool[[col]], meta$Player), DKSalary]),
          na.rm = TRUE
        )]
      } else if (!"TotalSalary" %in% names(your_pool)) {
        your_pool[, TotalSalary := NA_real_]
      }
      
      cat(sprintf("  [DoubleUp] Step 2/5 done — top 50 median range: %.1f - %.1f\n",
                  min(your_pool$MedianScore, na.rm = TRUE),
                  max(your_pool$MedianScore, na.rm = TRUE)))
      flush.console()
      
      # ── Step 3: Combine pools ─────────────────────────────────────────────
      cat("  [DoubleUp] Step 3/5: Combining 50 your + 500 field = 550 lineups...\n")
      flush.console()
      progress$set(detail = "Step 3/5: Combining lineup pools...", value = 0.25)
      
      field_pool   <- copy(du_rv$field_pool)
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
      
      # ── Step 4: Score all lineups via matrix multiply ─────────────────────
      # Uses score_all_lineups() from OptimalLineups_Core.R — same method as
      # tournament scoring. Builds lineup membership matrix x player score matrix.
      # 550 lineups x 50k sims = ~210MB score matrix, no memory issues.
      cat(sprintf("  [DoubleUp] Step 4/5: Scoring %d lineups x %s sims (matrix method)...\n",
                  nrow(combined), format(n_sims, big.mark = ",")))
      flush.console()
      progress$set(detail = sprintf("Step 4/5: Scoring %d lineups x %s sims...",
                                    nrow(combined), format(n_sims, big.mark = ",")),
                   value = 0.30)
      
      lineup_data_for_scoring <- make_lineup_data(combined, sim_res, std_cols)
      score_mat <- score_all_lineups(lineup_data_for_scoring, sim_res, verbose = TRUE)
      
      progress$set(detail = "Step 4/5: Scoring complete.", value = 0.55)
      
      # ── Step 5: Cash rates from score matrix ──────────────────────────────
      cat(sprintf("  [DoubleUp] Step 5/5: Computing cash rates (%.0f%% line)...\n",
                  0.45 * 100))
      flush.console()
      progress$set(detail = "Step 5/5: Computing cash rates...", value = 0.57)
      
      metrics <- cash_rate_from_score_matrix(
        score_matrix = score_mat,
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
                              AvgOwn  = if (all(is.na(your_avg_own))) NA_real_ else your_avg_own)
      all_own   <- rbindlist(list(your_own, field_own), use.names = TRUE)
      results   <- merge(results, all_own, by = "LineupID", all.x = TRUE)
      
      setcolorder(results, c("LineupID", "Source", std_cols,
                             "TotalSalary", "AvgOwn", "MedianScore", "CashRate"))
      setorder(results, -CashRate, -MedianScore)
      
      # ── Exposure ──────────────────────────────────────────────────────────
      progress$set(detail = "Building exposure table...", value = 0.95)
      cat("  [DoubleUp] Building exposure table...\n"); flush.console()
      exposure <- build_combined_exposure(field_pool, your_pool, meta, std_cols)
      
      # ── Store & report ────────────────────────────────────────────────────
      du_rv$results     <- results
      du_rv$exposure    <- exposure
      du_rv$has_results <- TRUE
      
      elapsed_total <- as.numeric(difftime(Sys.time(), t_total, units = "secs"))
      status_msg <- sprintf(
        "Double Up complete \u2014 550 lineups (500 field + 50 yours) | %s sims | %.1fs total",
        format(n_sims, big.mark = ","), elapsed_total
      )
      du_rv$status <- status_msg
      
      cat(sprintf("  [DoubleUp] Complete in %.1fs\n\n", elapsed_total))
      flush.console()
      
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
      if (is.null(rv$dk_optimal_lineups))
        return(div(style = "color:#666;font-size:12px;padding:8px 0;",
                   icon("info-circle"),
                   " Score DK Tournament Lineups first, then Generate Field and Run Double Up."))
      return(div(style = "color:#666;font-size:12px;padding:8px 0;",
                 icon("info-circle"),
                 " Click Generate Field to build opponent lineups, then Run Double Up."))
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
      formatStyle("CashRate",
                  background = styleColorBar(range(dt$CashRate, na.rm = TRUE), "#FFE500"),
                  backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "left") %>%
      formatStyle("Source",
                  color      = styleEqual(c("Yours", "Field"), c("#FFE500", "#aaaaaa")),
                  fontWeight = styleEqual(c("Yours", "Field"), c("700",     "400")))
  })
  
  
  # ── Download ──────────────────────────────────────────────────────────────────
  output$du_download <- downloadHandler(
    filename = function() paste0("GTS_DoubleUp_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content  = function(file) {
      req(du_rv$results, du_rv$exposure)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Lineups Ranked")
      openxlsx::writeData(wb, "Lineups Ranked", as.data.frame(du_rv$results))
      openxlsx::addWorksheet(wb, "Exposure")
      openxlsx::writeData(wb, "Exposure", as.data.frame(du_rv$exposure))
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}
# end of cash_game_module.R