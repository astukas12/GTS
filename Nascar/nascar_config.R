# ============================================================================
# NASCAR DFS CONFIGURATION - UNIFIED
# Golden Ticket Sims
# ============================================================================

# ============================================================================
# OPTIMAL LINEUP GENERATION CONFIGS
# ============================================================================

nascar_dk_optimal_config <- list(
  # Roster constraints
  roster_size = 6,
  salary_cap = 50000,
  
  # Optimization settings
  lineups_per_sim = 3,           # 2 lineups per sim (good balance)
  
  # Performance settings
  progress_frequency = 500,      # Update every 500 sims
  verbose = TRUE,
  use_parallel = TRUE,           # PARALLEL ENABLED with fixes
  
  # Memory management
  batch_size = 10000,
  max_lineups = 50000,           # Cap at 50k for speed
  
  # Distribution calculation settings  
  percentiles = c(0.01, 0.05, 0.10, 0.20),
  score_percentiles = c(0.10, 0.25, 0.50, 0.75, 0.90),
  
  # Player columns
  player_col = "Player",
  score_col = "FantasyPoints",
  salary_col = "Salary",
  ownership_col = "Own"
)

nascar_fd_optimal_config <- list(
  roster_size = 5,
  salary_cap = 50000,
  lineups_per_sim = 3,
  progress_frequency = 500,
  verbose = TRUE,
  use_parallel = TRUE,
  batch_size = 10000,
  max_lineups = 20000,
  percentiles = c(0.01, 0.05, 0.10, 0.20),
  score_percentiles = c(0.10, 0.25, 0.50, 0.75, 0.90),
  player_col = "Player",
  score_col = "FantasyPoints",
  salary_col = "Salary",
  ownership_col = "Own"
)

# ============================================================================
# LINEUP BUILDER MODULE CONFIGS
# ============================================================================

nascar_dk_builder_config <- list(
  sport_name = "NASCAR_DK",
  platform = "DraftKings",
  
  # ============================================================================
  # PLAYER COLUMNS
  # ============================================================================
  player_columns = c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6"),
  
  # ============================================================================
  # FILTER OPTIONS - ALL FILTERS INCLUDING OWNERSHIP AND STARTING
  # ============================================================================
  filter_options = list(
    numeric_filters = c(
      "WinRate",       # Contest win probability
      "Top1Pct",       # Top 1% finish rate
      "Top5Pct",       # Top 5% finish rate
      "Top10Pct",      # Top 10% finish rate
      "Top20Pct",      # Top 20% finish rate
      "TotalSalary",   # Total salary used
      "TotalOwn%",     # Cumulative ownership
      "AvgOwn%",       # Average ownership per player
      "TotalStart",    # Cumulative starting position
      "AvgStart"       # Average starting position
    )
  ),
  
  # ============================================================================
  # DISPLAY COLUMNS
  # ============================================================================
  display_columns = c(
    "Player1", "Player2", "Player3", "Player4", "Player5", "Player6",
    "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
    "TotalSalary", "TotalOwn%", "AvgOwn%", "TotalStart", "AvgStart"
  ),
  
  # ============================================================================
  # PLAYER DETAIL COLUMNS
  # ============================================================================
  player_details = list(
    Salary = "Salary",
    Own = "Own%",
    Starting = "Start",
    Car = "Car",
    Team = "Team"
  ),
  
  # ============================================================================
  # FORMATTING SPECIFICATIONS
  # ============================================================================
  formatting = list(
    percentage_cols = c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct", 
                        "TotalOwn%", "AvgOwn%", "Own"),
    percentage_decimals = 2,
    
    currency_cols = c("TotalSalary", "Salary"),
    
    integer_cols = c("TotalStart", "Starting", "Car"),
    
    decimal_cols = c("AvgStart", "AvgOwn%"),
    decimal_places = 1
  ),
  
  # ============================================================================
  # DOWNLOAD CONFIGURATION
  # ============================================================================
  download_columns = c(
    "Player1", "Player2", "Player3", "Player4", "Player5", "Player6",
    "TotalSalary", "BuildLabel"
  ),
  
  strip_dk_ids = TRUE,
  
  dk_upload_mapping = list(
    Player1 = "F",
    Player2 = "F",
    Player3 = "F",
    Player4 = "F",
    Player5 = "F",
    Player6 = "F"
  )
)

#' NASCAR FanDuel Lineup Builder Configuration
nascar_fd_builder_config <- list(
  sport_name = "NASCAR_FD",
  platform = "FanDuel",
  
  player_columns = c("Player1", "Player2", "Player3", "Player4", "Player5"),
  
  filter_options = list(
    numeric_filters = c(
      "WinRate",
      "Top1Pct",
      "Top5Pct",
      "Top10Pct",
      "Top20Pct",
      "TotalSalary",
      "TotalOwn%",
      "AvgOwn%",
      "TotalStart",
      "AvgStart"
    )
  ),
  
  display_columns = c(
    "Player1", "Player2", "Player3", "Player4", "Player5",
    "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
    "TotalSalary", "TotalOwn%", "AvgOwn%", "TotalStart", "AvgStart"
  ),
  
  player_details = list(
    Salary = "Salary",
    Own = "Own%",
    Starting = "Start",
    Car = "Car",
    Team = "Team"
  ),
  
  formatting = list(
    percentage_cols = c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct", 
                        "TotalOwn%", "AvgOwn%", "Own"),
    percentage_decimals = 2,
    
    currency_cols = c("TotalSalary", "Salary"),
    
    integer_cols = c("TotalStart", "Starting", "Car"),
    
    decimal_cols = c("AvgStart", "AvgOwn%"),
    decimal_places = 1
  ),
  
  download_columns = c(
    "Player1", "Player2", "Player3", "Player4", "Player5",
    "TotalSalary", "BuildLabel"
  ),
  
  strip_dk_ids = TRUE,
  
  fd_upload_mapping = list(
    Player1 = "D",
    Player2 = "D",
    Player3 = "D",
    Player4 = "D",
    Player5 = "D"
  )
)

# ============================================================================
# HELPER FUNCTIONS FOR LINEUP BUILDER
# ============================================================================

prepare_driver_details_for_builder <- function(driver_data, platform = "DK") {
  if (platform == "DK") {
    salary_col <- "DKSalary"
    own_col <- "DKOP"
  } else {
    salary_col <- "FDSalary"
    own_col <- "FDOP"
  }
  
  driver_details <- driver_data %>%
    select(
      Player = Name,
      Salary = all_of(salary_col),
      Own = all_of(own_col),
      Starting = Starting
    )
  
  if ("Car" %in% names(driver_data)) {
    driver_details$Car <- driver_data$Car
  }
  if ("Team" %in% names(driver_data)) {
    driver_details$Team <- driver_data$Team
  }
  
  driver_details <- driver_details %>%
    mutate(
      Salary = as.numeric(Salary),
      Own = as.numeric(Own),
      Starting = as.numeric(Starting)
    )
  
  return(driver_details)
}

strip_dk_id <- function(player_name) {
  gsub(" \\([0-9]+\\)$", "", player_name)
}

prepare_optimal_for_builder <- function(optimal_lineups, strip_ids = TRUE, num_players = 6) {
  df <- copy(optimal_lineups)
  setDT(df)
  
  if (strip_ids) {
    player_cols <- paste0("Player", 1:num_players)
    for (col in player_cols) {
      if (col %in% names(df)) {
        df[, (col) := strip_dk_id(get(col))]
      }
    }
  }
  
  # Ensure ownership columns are percentages
  if ("TotalOwn%" %in% names(df)) {
    if (mean(df$`TotalOwn%`, na.rm = TRUE) < 5) {
      df$`TotalOwn%` <- df$`TotalOwn%` * 100
    }
  }
  
  if ("AvgOwn%" %in% names(df)) {
    if (mean(df$`AvgOwn%`, na.rm = TRUE) < 5) {
      df$`AvgOwn%` <- df$`AvgOwn%` * 100
    }
  }
  
  return(df)
}
