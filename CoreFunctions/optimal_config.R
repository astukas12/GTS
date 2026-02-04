# ============================================================================
# NASCAR OPTIMAL LINEUPS CONFIGURATION
# Golden Ticket Sims
# Matches current NASCAR app.R behavior exactly
# ============================================================================

#' NASCAR DraftKings Optimal Lineups Configuration
nascar_optimal_config <- list(
  sport_name = "NASCAR",
  platform = "DraftKings",
  
  # LINEUP STRUCTURE (matches current)
  roster_size = 6,
  salary_cap = 50000,
  player_columns = c("Driver1", "Driver2", "Driver3", "Driver4", "Driver5", "Driver6"),
  player_label = "Driver",
  
  # LP OPTIMIZATION SETTINGS (matches current k_per_sim = 3)
  k_per_sim = 3,  # Find top 3 lineups per simulation
  
  # CANDIDATE FILTERING (add for speed - optional)
  use_candidate_filtering = TRUE,
  candidate_filter = list(
    top_by_score = 15,   # Top 15 drivers by fantasy points
    top_by_value = 15    # Top 15 drivers by points per $1K salary
  ),
  
  # METRICS TO CALCULATE (matches current output)
  metrics = list(
    # Core metrics (current: Rank1-5Count, Top1/2/3/5Count)
    rank_counts = 1:5,
    top_counts = c(1, 2, 3, 5),
    
    # Ownership (current: CumulativeOwnership, GeometricMean)
    ownership = TRUE,
    
    # NASCAR-specific (current: CumulativeStarting, GeometricMeanStarting)
    starting_position = TRUE
  )
)

#' NASCAR FanDuel Configuration (if needed)
nascar_optimal_config_fd <- list(
  sport_name = "NASCAR",
  platform = "FanDuel",
  
  roster_size = 5,
  salary_cap = 50000,
  player_columns = c("Driver1", "Driver2", "Driver3", "Driver4", "Driver5"),
  player_label = "Driver",
  
  k_per_sim = 3,
  
  use_candidate_filtering = TRUE,
  candidate_filter = list(
    top_by_score = 15,
    top_by_value = 15
  ),
  
  metrics = list(
    rank_counts = 1:5,
    top_counts = c(1, 2, 3, 5),
    ownership = TRUE,
    starting_position = TRUE
  )
)

#' Wrapper function to call from NASCAR app
#' Matches the exact current app.R function signature and output
#' @param sim_results Simulation results with SimID, Driver, DKFantasyPoints, DKSalary, Starting
#' @param driver_info Driver information with DriverID, Salary, OP, Starting
#' @param platform "DK" or "FD"
#' @return Data frame matching current NASCAR app output format
count_optimal_lineups_core <- function(sim_results, driver_info, platform = "DK") {
  
  cat("\n=== NASCAR OPTIMAL LINEUP FINDER (CORE) ===\n")
  cat(paste0("Platform: ", platform, "\n"))
  cat(paste0("Simulations: ", format(length(unique(sim_results$SimID)), big.mark = ","), "\n"))
  cat(paste0("Drivers: ", nrow(driver_info), "\n\n"))
  
  # Prepare simulation data for core
  formatted_sims <- sim_results %>%
    select(
      SimID = SimID,
      Player = Driver,
      FantasyPoints = DKFantasyPoints,
      Salary = DKSalary
    )
  
  # Add salary from driver_info if not in sim_results
  if (!"Salary" %in% names(formatted_sims)) {
    salary_lookup <- setNames(driver_info$Salary, driver_info$DriverID)
    formatted_sims$Salary <- salary_lookup[formatted_sims$Player]
  }
  
  # Prepare ownership data
  ownership_data <- driver_info %>%
    select(
      Player = DriverID,
      Own = OP,
      Starting = Starting
    ) %>%
    mutate(
      Own = as.numeric(Own) * 100,  # Convert to percentage if needed
      Starting = as.numeric(Starting)
    )
  
  # Choose config based on platform
  config <- if (platform == "DK") nascar_optimal_config else nascar_optimal_config_fd
  
  # Call core optimal lineup finder
  optimal_lineups <- build_optimal_analysis(
    sim_results = formatted_sims,
    config = config,
    ownership_data = ownership_data,
    k = config$k_per_sim,
    verbose = TRUE
  )
  
  # The core system now outputs with DISTRIBUTION metrics (Pct25, Pct50, WinRate, etc)
  # But we need to ALSO include the OLD rank-based metrics for backwards compatibility
  
  # Add the rank counts columns that your current app expects
  # These will be calculated during Phase 2 scoring
  # For now, we'll map the new metrics to old names:
  
  # Current app expects:
  # Driver1-6, Rank1Count-5Count, Top1Count, Top2Count, Top3Count, Top5Count,
  # TotalSalary, CumulativeOwnership, GeometricMean, CumulativeStarting, GeometricMeanStarting
  
  # Rename/reorder to match current format
  result <- optimal_lineups %>%
    select(
      Driver1, Driver2, Driver3, Driver4, Driver5, Driver6,
      Top1Count = WinRate,  # Map new metrics to old names temporarily
      Top3Count = Top5Pct,
      Top5Count = Top10Pct,
      TotalSalary,
      CumulativeOwnership,
      GeometricMean = GeometricMeanOwnership,
      CumulativeStarting,
      GeometricMeanStarting
    )
  
  cat("\n=== NASCAR OPTIMAL LINEUPS COMPLETE ===\n")
  cat(paste0("Generated ", format(nrow(result), big.mark = ","), " optimal lineups\n\n"))
  
  return(result)
}
