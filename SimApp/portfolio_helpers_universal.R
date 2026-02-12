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
