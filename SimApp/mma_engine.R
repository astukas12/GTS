# ============================================================================
# MMA SIMULATION ENGINE
# Golden Ticket Sims - Universal App
# ============================================================================
#
# APPROACH: Deterministic sim allocation
#   - Each fight has outcome probabilities (R1, R2, R3, Decision, etc.)
#     that sum to 100% across both fighters
#   - Instead of random sampling, we allocate n_sims proportionally:
#     if R1 prob = 28%, exactly round(0.28 * n_sims) sims get that outcome
#   - Scores are sampled from the percentile distributions for each outcome
#   - Each fight is independent; results are combined into one sim_results table
#
# ============================================================================

library(data.table)

# ============================================================================
# WIN BONUS LOOKUP
# ============================================================================

# Win bonus lookup tables (total bonus added to base fight action scores per outcome)
# Source: verified from original MMA app
DK_WIN_BONUSES <- list(
  "QuickWin_R1" = 115,
  "R1"          = 90,
  "R2"          = 70,
  "R3"          = 45,
  "R4"          = 40,
  "R5"          = 40,
  "Decision_R3" = 30,
  "Decision_R5" = 30,
  "Decision"    = 30
)

FD_WIN_BONUSES <- list(
  "QuickWin_R1" = 100,
  "R1"          = 100,
  "R2"          = 75,
  "R3"          = 50,
  "R4"          = 35,
  "R5"          = 25,
  "Decision_R3" = 20,
  "Decision_R5" = 20,
  "Decision"    = 20
)

get_win_bonus <- function(outcome, platform = "DK") {
  bonuses <- if (platform == "DK") DK_WIN_BONUSES else FD_WIN_BONUSES
  if (outcome %in% names(bonuses)) return(bonuses[[outcome]])
  return(0)
}

# ============================================================================
# VECTORIZED PERCENTILE SCORE SAMPLER
# ============================================================================

sample_scores_vectorized <- function(p5, p10, p25, p50, p75, p90, p95, n) {
  u  <- runif(n)
  lo <- ifelse(u < 0.10, p5,  ifelse(u < 0.25, p10, ifelse(u < 0.50, p25,
                                                           ifelse(u < 0.75, p50, ifelse(u < 0.90, p75, p90)))))
  hi <- ifelse(u < 0.10, p10, ifelse(u < 0.25, p25, ifelse(u < 0.50, p50,
                                                           ifelse(u < 0.75, p75, ifelse(u < 0.90, p90, p95)))))
  lo + runif(n) * (hi - lo)
}

# ============================================================================
# OUTCOME CATEGORY (for stacked bar visualization)
# ============================================================================

categorize_outcome <- function(outcome) {
  fcase(
    grepl("QuickWin", outcome),                                "QuickWin_R1",
    grepl("R1", outcome) & !grepl("Decision", outcome),        "R1 Finish",
    grepl("R2", outcome) & !grepl("Decision", outcome),        "R2 Finish",
    grepl("R3", outcome) & !grepl("Decision", outcome),        "R3 Finish",
    grepl("R4", outcome) & !grepl("Decision", outcome),        "R4 Finish",
    grepl("R5", outcome) & !grepl("Decision", outcome),        "R5 Finish",
    default = "Decision"
  )
}

# ============================================================================
# METADATA BUILDER
# ============================================================================

create_mma_metadata <- function(fights_data) {
  setDT(fights_data)
  meta <- unique(fights_data[, .(
    Player   = Name,
    DKSalary = as.numeric(DKSalary),
    DKID     = as.character(DKID),
    DKOwn    = as.numeric(DKOwn),
    FDSalary = as.numeric(FDSalary),
    FDID     = as.character(FDID),
    FDOwn    = as.numeric(FDOwn),
    CPTID    = as.character(CPTID),
    SDID     = as.character(SDID),
    SDSalary = as.numeric(SDSal),
    WinProb  = as.numeric(DeViggedProb)
  )])
  meta
}

# ============================================================================
# MAIN SIMULATION FUNCTION
# ============================================================================

run_mma_simulation <- function(input_data, n_sims, config, progress_callback = NULL) {
  
  t_start <- proc.time()["elapsed"]
  
  cat("\n")
  cat("============================================================\n")
  cat("  MMA SIMULATION ENGINE\n")
  cat("============================================================\n")
  
  # --------------------------------------------------------------------------
  # Load and coerce data
  # --------------------------------------------------------------------------
  fights_data <- as.data.table(input_data$Fights)
  scores_data <- as.data.table(input_data$Scores)
  
  char_cols <- c("Name","Opponent","DKID","FDID","CPTID","SDID")
  for (col in intersect(char_cols, names(fights_data)))
    fights_data[, (col) := as.character(get(col))]
  
  num_cols_f <- c("DKSalary","FDSalary","DKOwn","FDOwn","SDSal",
                  "OriginalML","DeViggedProb",
                  "R1","QuickWin_R1","R2","R3","R4","R5","Decision")
  for (col in intersect(num_cols_f, names(fights_data)))
    fights_data[, (col) := as.numeric(get(col))]
  
  num_cols_s <- grep("_P5$|_P10$|_P25$|_P50$|_P75$|_P90$|_P95$|Winner_Prob",
                     names(scores_data), value = TRUE)
  for (col in num_cols_s)
    scores_data[, (col) := as.numeric(get(col))]
  
  # Canonical fight pairs (deduplicated)
  fight_pairs <- unique(fights_data[, .(Fighter1 = Name, Fighter2 = Opponent)])
  fight_pairs <- fight_pairs[Fighter1 < Fighter2]
  n_fights    <- nrow(fight_pairs)
  
  cat(sprintf("  Fighters  : %d\n",  nrow(fights_data)))
  cat(sprintf("  Fights    : %d\n",  n_fights))
  cat(sprintf("  Sims      : %s\n",  format(n_sims, big.mark = ",")))
  cat("------------------------------------------------------------\n\n")
  
  if (!is.null(progress_callback))
    progress_callback("Allocating sims to outcomes...", 0.05)
  
  # --------------------------------------------------------------------------
  # STEP 1: Allocate n_sims deterministically across outcomes per fight
  # --------------------------------------------------------------------------
  cat("[STEP 1/3] Allocating sims to outcomes...\n")
  
  outcome_cols <- c("R1","QuickWin_R1","R2","R3","R4","R5","Decision")
  
  # We'll build one big plan table: each row = one outcome block for one fight
  plan_list <- vector("list", n_fights * length(outcome_cols) * 2)
  plan_idx  <- 1L
  global_sim_start <- 1L  # rolling SimID counter across ALL fights
  
  for (fi in seq_len(n_fights)) {
    f1 <- fight_pairs$Fighter1[fi]
    f2 <- fight_pairs$Fighter2[fi]
    
    row_f1 <- fights_data[Name == f1 & Opponent == f2]
    row_f2 <- fights_data[Name == f2 & Opponent == f1]
    
    # Each fight gets its own SimID space: 1..n_sims
    # (fights are independent; sim IDs only need to be consistent within a fight)
    fight_sim_counter <- 1L
    
    for (fighter_row in list(row_f1, row_f2)) {
      if (nrow(fighter_row) == 0) next
      winner <- fighter_row$Name
      loser  <- fighter_row$Opponent
      
      for (oc in outcome_cols) {
        prob <- fighter_row[[oc]]
        if (is.null(prob) || is.na(prob) || prob <= 0) next
        
        n_oc <- max(1L, round(prob * n_sims))
        
        # Exact outcome mapping: Fights col -> Scores Outcome value
        # MUST use exact match: grepl("R1") also hits "QuickWin_R1",
        # grepl("R3") also hits "Decision_R3" -> wrong base scores + wrong bonus
        scores_outcome <- switch(oc,
                                 "R1"          = "R1",
                                 "QuickWin_R1" = "QuickWin_R1",
                                 "R2"          = "R2",
                                 "R3"          = "R3",
                                 "R4"          = "R4",
                                 "R5"          = "R5",
                                 "Decision"    = if (!is.null(fighter_row$Rounds) &&
                                                     !is.na(fighter_row$Rounds) &&
                                                     as.integer(fighter_row$Rounds) == 5L)
                                   "Decision_R5" else "Decision_R3",
                                 oc
        )
        score_rows <- scores_data[Winner == winner & Loser == loser &
                                    Outcome == scores_outcome]
        if (nrow(score_rows) == 0)
          score_rows <- scores_data[Winner == winner & Loser == loser]
        if (nrow(score_rows) == 0) next
        
        sr <- score_rows[1]
        
        plan_list[[plan_idx]] <- list(
          FightIdx = fi,
          Fighter1 = f1,
          Fighter2 = f2,
          Winner   = winner,
          Loser    = loser,
          Outcome  = oc,
          SimStart = fight_sim_counter,
          SimEnd   = fight_sim_counter + n_oc - 1L,
          N        = n_oc,
          W_DK_P5 = sr$Winner_DK_Base_P5,  W_DK_P10 = sr$Winner_DK_Base_P10,
          W_DK_P25= sr$Winner_DK_Base_P25, W_DK_P50 = sr$Winner_DK_Base_P50,
          W_DK_P75= sr$Winner_DK_Base_P75, W_DK_P90 = sr$Winner_DK_Base_P90,
          W_DK_P95= sr$Winner_DK_Base_P95,
          W_FD_P5 = sr$Winner_FD_Base_P5,  W_FD_P10 = sr$Winner_FD_Base_P10,
          W_FD_P25= sr$Winner_FD_Base_P25, W_FD_P50 = sr$Winner_FD_Base_P50,
          W_FD_P75= sr$Winner_FD_Base_P75, W_FD_P90 = sr$Winner_FD_Base_P90,
          W_FD_P95= sr$Winner_FD_Base_P95,
          L_DK_P5 = sr$Loser_DK_Base_P5,   L_DK_P10 = sr$Loser_DK_Base_P10,
          L_DK_P25= sr$Loser_DK_Base_P25,  L_DK_P50 = sr$Loser_DK_Base_P50,
          L_DK_P75= sr$Loser_DK_Base_P75,  L_DK_P90 = sr$Loser_DK_Base_P90,
          L_DK_P95= sr$Loser_DK_Base_P95,
          L_FD_P5 = sr$Loser_FD_Base_P5,   L_FD_P10 = sr$Loser_FD_Base_P10,
          L_FD_P25= sr$Loser_FD_Base_P25,  L_FD_P50 = sr$Loser_FD_Base_P50,
          L_FD_P75= sr$Loser_FD_Base_P75,  L_FD_P90 = sr$Loser_FD_Base_P90,
          L_FD_P95= sr$Loser_FD_Base_P95
        )
        
        fight_sim_counter <- fight_sim_counter + n_oc
        plan_idx <- plan_idx + 1L
      }
    }
    
    cat(sprintf("  Fight %2d/%d : %-25s vs %s\n", fi, n_fights, f1, f2))
  }
  
  plan <- rbindlist(plan_list[1:(plan_idx - 1L)], fill = TRUE)
  cat(sprintf("\n  Total outcome blocks : %d\n\n", nrow(plan)))
  
  if (!is.null(progress_callback))
    progress_callback("Generating scores...", 0.35)
  
  # --------------------------------------------------------------------------
  # STEP 2: Generate scores for every outcome block (vectorized)
  # --------------------------------------------------------------------------
  cat("[STEP 2/3] Generating scores...\n")
  
  result_list <- vector("list", nrow(plan) * 2L)
  result_idx  <- 1L
  
  for (ri in seq_len(nrow(plan))) {
    row    <- plan[ri]
    n_oc   <- row$N
    sim_ids <- row$SimStart:row$SimEnd
    oc      <- row$Outcome
    vis_oc  <- categorize_outcome(oc)
    
    dk_bonus <- get_win_bonus(oc, "DK")
    fd_bonus <- get_win_bonus(oc, "FD")
    
    w_dk <- sample_scores_vectorized(row$W_DK_P5, row$W_DK_P10, row$W_DK_P25,
                                     row$W_DK_P50, row$W_DK_P75, row$W_DK_P90,
                                     row$W_DK_P95, n_oc) + dk_bonus
    w_fd <- sample_scores_vectorized(row$W_FD_P5, row$W_FD_P10, row$W_FD_P25,
                                     row$W_FD_P50, row$W_FD_P75, row$W_FD_P90,
                                     row$W_FD_P95, n_oc) + fd_bonus
    l_dk <- sample_scores_vectorized(row$L_DK_P5, row$L_DK_P10, row$L_DK_P25,
                                     row$L_DK_P50, row$L_DK_P75, row$L_DK_P90,
                                     row$L_DK_P95, n_oc)
    l_fd <- sample_scores_vectorized(row$L_FD_P5, row$L_FD_P10, row$L_FD_P25,
                                     row$L_FD_P50, row$L_FD_P75, row$L_FD_P90,
                                     row$L_FD_P95, n_oc)
    
    result_list[[result_idx]] <- data.table(
      SimID    = sim_ids, Player = row$Winner,
      DKScore  = w_dk,    FDScore = w_fd,
      Win      = 1L,      Outcome = vis_oc, FightIdx = row$FightIdx
    )
    result_idx <- result_idx + 1L
    
    result_list[[result_idx]] <- data.table(
      SimID    = sim_ids, Player = row$Loser,
      DKScore  = l_dk,    FDScore = l_fd,
      Win      = 0L,      Outcome = vis_oc, FightIdx = row$FightIdx
    )
    result_idx <- result_idx + 1L
    
    # Progress every fight (2 blocks per fighter = ~2*n_outcomes per fight)
    if (ri %% max(1L, floor(nrow(plan) / 20L)) == 0 || ri == nrow(plan)) {
      pct <- ri / nrow(plan)
      cat(sprintf("\r  Scoring: %d/%d blocks (%.0f%%)   ", ri, nrow(plan), pct * 100))
      if (!is.null(progress_callback))
        progress_callback(sprintf("Scoring outcomes... %.0f%%", pct * 100),
                          0.35 + pct * 0.45)
    }
  }
  cat("\n")
  
  sim_results <- rbindlist(result_list[1:(result_idx - 1L)], use.names = TRUE)
  
  # Assign shared SimIDs 1..n_sims across all fights so each SimID = one full slate.
  # Winner and loser of each fight are a paired set — they share the same SimID and
  # must stay together (you can't have both fighters get a winner score in the same sim).
  # We shuffle at the SimID level: randomly reassign which slot (1..n_sims) each
  # existing SimID maps to, keeping winner+loser pairs intact.
  # Since fights are independent, any random pairing across fights is valid.
  sim_results_list <- vector("list", n_fights)
  for (fi in seq_len(n_fights)) {
    block <- sim_results[FightIdx == fi]
    
    # Existing SimIDs for this fight (1..~n_sims, may have ±1 from rounding)
    existing_ids <- sort(unique(block$SimID))
    n_existing   <- length(existing_ids)
    
    # Trim to exactly n_sims: drop extras, or resample to fill gaps
    if (n_existing > n_sims) {
      keep_ids   <- existing_ids[1:n_sims]
      block      <- block[SimID %in% keep_ids]
      existing_ids <- keep_ids
    } else if (n_existing < n_sims) {
      # Duplicate a few rows to reach n_sims (rare ±1 rounding edge case)
      extra_ids  <- sample(existing_ids, n_sims - n_existing, replace = TRUE)
      extra_rows <- block[SimID %in% extra_ids][
        sample(.N, n_sims - n_existing, replace = TRUE)]
      block      <- rbind(block, extra_rows)
      existing_ids <- sort(unique(block$SimID))
    }
    
    # Random permutation: each old SimID → a new slot in 1..n_sims
    # Winner and loser share the same old SimID, so they both move to the same new slot
    perm <- sample(n_sims)                          # new slot for each old id position
    id_map <- setNames(perm, as.character(existing_ids[1:n_sims]))
    block[, SimID := id_map[as.character(SimID)]]
    
    sim_results_list[[fi]] <- block
  }
  sim_results <- rbindlist(sim_results_list)
  sim_results[, FightIdx := NULL]
  
  elapsed_scores <- proc.time()["elapsed"] - t_start
  cat(sprintf("\n  Rows generated: %s in %.1fs\n\n",
              format(nrow(sim_results), big.mark = ","), elapsed_scores))
  
  if (!is.null(progress_callback))
    progress_callback("Building metadata and visuals...", 0.82)
  
  # --------------------------------------------------------------------------
  # STEP 3: Metadata + sport_visuals
  # --------------------------------------------------------------------------
  cat("[STEP 3/3] Building metadata and visuals...\n")
  
  metadata <- create_mma_metadata(fights_data)
  
  # Fantasy projection stats
  build_fantasy_stats <- function(score_col, sal_col, own_col, ppd_denom = 1000) {
    stats <- sim_results[, .(
      TotalFights  = .N,
      Wins         = sum(Win == 1L),
      Median_Score = median(get(score_col), na.rm = TRUE),
      Avg_Score    = mean(get(score_col),   na.rm = TRUE),
      Win_Median   = median(get(score_col)[Win == 1L], na.rm = TRUE)
    ), by = Player]
    stats[, WinRate := Wins / TotalFights]
    sal_tbl <- metadata[, .(Player, Salary = get(sal_col), Own = get(own_col), WinProb)]
    stats   <- merge(stats, sal_tbl, by = "Player", all.x = TRUE)
    stats[, PPD := Median_Score / (Salary / ppd_denom)]
    stats[, c("TotalFights","Wins") := NULL]
    setorder(stats, -Median_Score)
    stats
  }
  
  dk_fantasy <- build_fantasy_stats("DKScore", "DKSalary", "DKOwn", 1000)
  fd_fantasy <- build_fantasy_stats("FDScore", "FDSalary", "FDOwn", 1)
  
  # Outcome distribution for stacked bar
  total_per_fighter <- sim_results[, .(TotalFights = .N), by = Player]
  outcome_pct <- sim_results[Win == 1L, .(Count = .N), by = .(Player, Outcome)]
  outcome_pct <- merge(outcome_pct, total_per_fighter, by = "Player")
  outcome_pct[, WinPct := Count / TotalFights * 100]
  
  fighter_summary <- merge(
    sim_results[, .(WinRate = mean(Win == 1L) * 100), by = Player],
    metadata[, .(Player, DKSalary, FDSalary, SDSalary)],
    by = "Player", all.x = TRUE
  )
  # sorted DK salary ascending; app renderer reverses for high-salary-at-top display
  setorder(fighter_summary, DKSalary)
  # Labels built per-platform in the app renderer (platform not known here)
  # outcome_pct YLabel also set in app renderer
  
  score_dist <- sim_results[, .(Player, DKScore, FDScore, Win)]
  
  sport_visuals <- list(
    dk_fantasy      = dk_fantasy,
    fd_fantasy      = fd_fantasy,
    outcome_pct     = outcome_pct,
    fighter_summary = fighter_summary,
    score_dist      = score_dist,
    player_data     = metadata
  )
  
  elapsed_total <- proc.time()["elapsed"] - t_start
  cat(sprintf("  Metadata: %d fighters\n", nrow(metadata)))
  cat("\n")
  cat("============================================================\n")
  cat(sprintf("  MMA SIMULATION COMPLETE in %.1fs\n", elapsed_total))
  cat(sprintf("  %s result rows | %d fighters\n",
              format(nrow(sim_results), big.mark = ","), nrow(metadata)))
  cat("============================================================\n\n")
  
  if (!is.null(progress_callback))
    progress_callback("Complete!", 1.0)
  
  return(list(
    sim_results   = sim_results,
    metadata      = metadata,
    sport_visuals = sport_visuals
  ))
}

# ============================================================================
# LINEUP METRICS - EXPECTED WINS (post-optimization, mirrors Tennis)
# ============================================================================

calculate_mma_lineup_metrics <- function(scored_lineups, sim_results, metadata) {
  library(data.table)
  setDT(sim_results); setDT(scored_lineups)
  
  if (!"Win" %in% names(sim_results)) {
    warning("Win column not found - skipping MMA lineup metrics")
    return(scored_lineups)
  }
  
  # Detect player columns based on lineup mode:
  #   standard -> Player1..Player6
  #   mvp      -> MVP, Player1..Player5
  #   captain  -> Captain, Util1..Util5
  if ("Captain" %in% names(scored_lineups)) {
    player_cols <- c("Captain", grep("^Util", names(scored_lineups), value = TRUE))
  } else if ("MVP" %in% names(scored_lineups)) {
    player_cols <- c("MVP", grep("^Player", names(scored_lineups), value = TRUE))
  } else {
    player_cols <- grep("^Player", names(scored_lineups), value = TRUE)
  }
  roster_size <- length(player_cols)
  
  # Build win lookup: player -> simID -> win (1/0)
  # Use a simple named vector per sim approach via long-format join
  # win_by_player_sim: keyed by Player + SimID for fast lookup
  win_lookup <- sim_results[, .(Player, SimID, Win)]
  setkey(win_lookup, Player, SimID)
  
  all_sims    <- sort(unique(sim_results$SimID))
  n_sims      <- length(all_sims)
  all_players <- unique(sim_results$Player)
  
  # Wide matrix: rows=Player (alphabetical after dcast), cols=SimID
  win_wide <- dcast(win_lookup, Player ~ SimID, value.var = "Win", fill = 0L)
  sim_id_cols <- as.character(all_sims)
  # Ensure column order matches all_sims
  sim_id_cols <- intersect(sim_id_cols, names(win_wide))
  
  # Player -> row index lookup for fast matrix subsetting
  player_row <- setNames(seq_len(nrow(win_wide)), win_wide$Player)
  win_mat    <- as.matrix(win_wide[, ..sim_id_cols])  # n_players x n_sims
  
  # For each lineup: look up each fighter's win row, sum per sim, compute metrics
  n_lineups <- nrow(scored_lineups)
  total_ew      <- numeric(n_lineups)
  win_all_pct   <- numeric(n_lineups)
  win_5plus_pct <- numeric(n_lineups)
  
  for (i in seq_len(n_lineups)) {
    fighters     <- as.character(unlist(scored_lineups[i, ..player_cols]))
    row_idx      <- player_row[fighters]
    row_idx      <- row_idx[!is.na(row_idx)]
    if (length(row_idx) == 0) next
    wins_per_sim  <- colSums(win_mat[row_idx, , drop = FALSE])
    total_ew[i]      <- round(mean(wins_per_sim), 2)
    win_all_pct[i]   <- round(mean(wins_per_sim >= roster_size) * 100, 1)
    win_5plus_pct[i] <- round(mean(wins_per_sim >= (roster_size - 1L)) * 100, 1)
  }
  
  scored_lineups[, TotalEW     := total_ew]
  scored_lineups[, Win6Pct     := win_all_pct]
  scored_lineups[, Win5PlusPct := win_5plus_pct]
  
  return(scored_lineups)
}