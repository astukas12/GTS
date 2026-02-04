# ============================================================================
# NASCAR CONFIGURATION FOR LINEUP BUILDER
# Golden Ticket Sims
# ============================================================================

#' NASCAR DraftKings Configuration
nascar_config <- list(
  sport_name = "NASCAR",
  
  # PLAYER COLUMNS
  player_columns = c("Driver1", "Driver2", "Driver3", "Driver4", "Driver5", "Driver6"),
  player_label = "Driver",
  
  # METRICS CONFIGURATION
  metrics = list(
    # Top Counts
    list(
      name = "Top1Count",
      type = "numeric",
      label = "Top 1 Count",
      filter = TRUE,
      display = TRUE,
      decimals = 0
    ),
    list(
      name = "Top3Count",
      type = "numeric",
      label = "Top 3 Count",
      filter = TRUE,
      display = TRUE,
      decimals = 0
    ),
    list(
      name = "Top5Count",
      type = "numeric",
      label = "Top 5 Count",
      filter = TRUE,
      display = TRUE,
      decimals = 0
    ),
    
    # Salary
    list(
      name = "TotalSalary",
      type = "numeric",
      label = "Salary",
      filter = TRUE,
      display = TRUE,
      decimals = 0,
      format = "currency"
    ),
    
    # Ownership Metrics
    list(
      name = "CumulativeOwnership",
      type = "numeric",
      label = "Cumulative Own",
      filter = TRUE,
      display = TRUE,
      decimals = 1,
      format = "percent"
    ),
    list(
      name = "GeometricMeanOwnership",
      type = "numeric",
      label = "Geometric Mean Own",
      filter = TRUE,
      display = TRUE,
      decimals = 1,
      format = "percent"
    ),
    
    # Starting Position Metrics (NASCAR-specific)
    list(
      name = "CumulativeStarting",
      type = "numeric",
      label = "Cumulative Start Pos",
      filter = TRUE,
      display = FALSE,  # Don't show in lineup table
      decimals = 0
    ),
    list(
      name = "GeometricMeanStarting",
      type = "numeric",
      label = "Geometric Mean Start",
      filter = TRUE,
      display = FALSE,
      decimals = 1
    )
  ),
  
  # PLAYER DETAIL COLUMNS (for exposure tables)
  # These should match columns in your driver data/projections
  player_details = list(
    list(name = "Salary", label = "Salary", format = "currency"),
    list(name = "Own", label = "Own %", format = "percent"),
    list(name = "Starting", label = "Start Pos", format = "number")
  )
)

#' Prepare NASCAR ownership data for lineup builder
#' @param driver_projections Data frame with driver projections
#' @return Formatted ownership data frame
prepare_nascar_ownership_data <- function(driver_projections) {
  ownership_data <- driver_projections %>%
    select(
      Name = Name,  # Or DKName if that's what's in optimal lineups
      Salary = DKSalary,
      Own = DKOP,
      Starting = Starting
    ) %>%
    mutate(
      Own = as.numeric(Own),
      Salary = as.numeric(Salary),
      Starting = as.numeric(Starting)
    )
  
  return(ownership_data)
}
