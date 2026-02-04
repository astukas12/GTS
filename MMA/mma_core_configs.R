  # ============================================================================
  # DK CLASSIC CONFIGURATION
  # ============================================================================

mma_dk_optimal_config <- list(
  sport_name = "MMA",
  platform = "DK",
  roster_size = 6,
  salary_cap = 50000,
  player_columns = c("Fighter1", "Fighter2", "Fighter3", "Fighter4", "Fighter5", "Fighter6")
)

mma_dk_builder_config <- list(
  sport_name = "MMA",
  platform = "DK",
  player_columns = c("Fighter1", "Fighter2", "Fighter3", "Fighter4", "Fighter5", "Fighter6"),
  player_label = "Fighter",
  
  filter_options = list(
    numeric_filters = c("Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"),
    player_filters = c("locked", "excluded"),
    default_minimums = list(
      Top1Pct = 0,
      Top5Pct = 0,
      Top10Pct = 0,
      Top20Pct = 0
    )
  )
)

# ============================================================================
# FANDUEL CONFIGURATION
# ============================================================================

mma_fd_optimal_config <- list(
  sport_name = "MMA",
  platform = "FD",
  roster_size = 6,
  salary_cap = 100,
  player_columns = c("MVP", "Fighter1", "Fighter2", "Fighter3", "Fighter4", "Fighter5")
)

mma_fd_builder_config <- list(
  sport_name = "MMA",
  platform = "FD",
  player_columns = c("MVP", "Fighter1", "Fighter2", "Fighter3", "Fighter4", "Fighter5"),
  player_label = "Fighter",
  
  filter_options = list(
    numeric_filters = c("Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"),
    player_filters = c("locked", "excluded"),
    default_minimums = list(
      Top1Pct = 0,
      Top5Pct = 0,
      Top10Pct = 0,
      Top20Pct = 0
    )
  )
)

# ============================================================================
# SHOWDOWN CONFIGURATION
# ============================================================================

mma_sd_optimal_config <- list(
  sport_name = "MMA",
  platform = "SD",
  roster_size = 6,
  salary_cap = 50000,
  showdown_mode = TRUE,
  # Phase 3 outputs these generic slot columns
  player_columns = c("Slot1", "Slot2", "Slot3", "Slot4", "Slot5", "Slot6"),
  
  showdown_config = list(
    captain_score_col = "captain_score",
    captain_salary_col = "CPTSal",
    captain_id_col = "CPTID",
    flex_score_col = "DKScore",
    flex_salary_col = "SDSal",
    flex_id_col = "SDID",
    ownership_col = "DKOwn"
  )
)

mma_sd_builder_config <- list(
  sport_name = "MMA",
  platform = "SD",
  showdown_mode = TRUE,
  # Postprocess creates these final columns
  player_columns = c("Captain", "Fighter1", "Fighter2", "Fighter3", "Fighter4", "Fighter5"),
  player_label = "Fighter",
  
  filter_options = list(
    numeric_filters = c("Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"),
    player_filters = c("locked", "excluded"),
    default_minimums = list(
      Top1Pct = 0,
      Top5Pct = 0,
      Top10Pct = 0,
      Top20Pct = 0
    )
  )
)