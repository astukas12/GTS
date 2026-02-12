# ============================================================================
# LINEUP BUILDER CORE - UPDATED WITH MVP/CAPTAIN SUPPORT
# Auto-detects column structure for Standard/MVP/Captain formats
# ============================================================================

library(data.table)
library(dplyr)


# ============================================================================
# HELPER: CALCULATE EXPOSURE
# ============================================================================

calculate_exposure <- function(portfolio_lineups, metadata = NULL) {
  
  setDT(portfolio_lineups)
  
  # Detect player columns and special positions
  has_captain <- "Captain" %in% names(portfolio_lineups)
  has_mvp <- "MVP" %in% names(portfolio_lineups)
  
  if (has_captain) {
    player_cols <- c("Captain", grep("^Util", names(portfolio_lineups), value = TRUE))
    special_col <- "Captain"
  } else if (has_mvp) {
    player_cols <- c("MVP", grep("^Player", names(portfolio_lineups), value = TRUE))
    special_col <- "MVP"
  } else {
    player_cols <- grep("^Player", names(portfolio_lineups), value = TRUE)
    special_col <- NULL
  }
  
  n_lineups <- nrow(portfolio_lineups)
  
  # Get all unique players
  all_players <- unique(unlist(portfolio_lineups[, ..player_cols]))
  all_players <- all_players[!is.na(all_players)]
  
  # Calculate exposure for each player
  exposure_list <- list()
  
  for (player in all_players) {
    # Count total appearances
    total_count <- 0
    special_count <- 0  # Captain or MVP count
    
    for (col in player_cols) {
      appearances <- sum(portfolio_lineups[[col]] == player, na.rm = TRUE)
      total_count <- total_count + appearances
      
      # Track special position appearances
      if (!is.null(special_col) && col == special_col) {
        special_count <- appearances
      }
    }
    
    exposure_pct <- (total_count / n_lineups) * 100
    
    player_exposure <- data.table(
      Player = player,
      Count = total_count,
      Exposure = round(exposure_pct, 1)
    )
    
    # Add special position exposure if applicable
    if (!is.null(special_col)) {
      player_exposure[[paste0(special_col, "Count")]] <- special_count
      player_exposure[[paste0(special_col, "Pct")]] <- round((special_count / n_lineups) * 100, 1)
    }
    
    # Add metadata if provided
    if (!is.null(metadata)) {
      setDT(metadata)
      player_meta <- metadata[Player == player]
      
      if (nrow(player_meta) > 0) {
        # Add salary (try different platform columns)
        if ("DKSalary" %in% names(metadata)) {
          player_exposure$Salary <- player_meta$DKSalary
        } else if ("FDSalary" %in% names(metadata)) {
          player_exposure$Salary <- player_meta$FDSalary
        } else if ("SDSalary" %in% names(metadata)) {
          player_exposure$Salary <- player_meta$SDSalary
        }
        
        # Add ownership (try different platform columns)
        if ("DKOwn" %in% names(metadata)) {
          player_exposure$Proj_Own <- round(player_meta$DKOwn * 100, 1)
        } else if ("FDOwn" %in% names(metadata)) {
          player_exposure$Proj_Own <- round(player_meta$FDOwn * 100, 1)
        }
        
        # Calculate leverage if we have both
        if ("Proj_Own" %in% names(player_exposure)) {
          player_exposure$Leverage <- round(player_exposure$Exposure - player_exposure$Proj_Own, 1)
        }
      }
    }
    
    exposure_list[[length(exposure_list) + 1]] <- player_exposure
  }
  
  # Combine all exposure data
  exposure_data <- rbindlist(exposure_list, fill = TRUE)
  
  # Sort by exposure descending
  setorder(exposure_data, -Exposure)
  
  return(exposure_data)
}


# ============================================================================
# HELPER: FILTER LINEUPS
# ============================================================================

filter_lineups <- function(optimal_lineups, filters) {
  
  setDT(optimal_lineups)
  
  # Detect player columns and special positions
  has_captain <- "Captain" %in% names(optimal_lineups)
  has_mvp <- "MVP" %in% names(optimal_lineups)
  
  if (has_captain) {
    player_cols <- c("Captain", grep("^Util", names(optimal_lineups), value = TRUE))
    flex_cols <- grep("^Util", names(optimal_lineups), value = TRUE)
    special_col <- "Captain"
  } else if (has_mvp) {
    player_cols <- c("MVP", grep("^Player", names(optimal_lineups), value = TRUE))
    flex_cols <- grep("^Player", names(optimal_lineups), value = TRUE)
    special_col <- "MVP"
  } else {
    player_cols <- grep("^Player", names(optimal_lineups), value = TRUE)
    flex_cols <- player_cols
    special_col <- NULL
  }
  
  filtered <- copy(optimal_lineups)
  
  # Filter by rate minimums
  if (!is.null(filters$rate_minimums)) {
    for (filter_name in names(filters$rate_minimums)) {
      min_value <- filters$rate_minimums[[filter_name]]
      if (!is.null(min_value) && min_value > 0) {
        col_name <- paste0(filter_name, "Rate")
        if (!col_name %in% names(filtered)) {
          col_name <- paste0(filter_name, "Pct")
        }
        if (col_name %in% names(filtered)) {
          filtered <- filtered[get(col_name) >= min_value]
        }
      }
    }
  }
  
  # Filter by ranges
  if (!is.null(filters$range_filters)) {
    for (filter_name in names(filters$range_filters)) {
      range_value <- filters$range_filters[[filter_name]]
      if (!is.null(range_value) && length(range_value) == 2) {
        min_val <- range_value[1]
        max_val <- range_value[2]
        
        if (filter_name %in% names(filtered)) {
          filtered <- filtered[get(filter_name) >= min_val & get(filter_name) <= max_val]
        }
      }
    }
  }
  
  # Locked players (must be in lineup anywhere)
  if (!is.null(filters$locked_players) && length(filters$locked_players) > 0) {
    for (locked_player in filters$locked_players) {
      player_in_lineup <- rowSums(sapply(player_cols, function(col) {
        filtered[[col]] == locked_player
      }), na.rm = TRUE) > 0
      
      filtered <- filtered[player_in_lineup]
    }
  }
  
  # Locked special position (Captain/MVP must be specific player)
  if (!is.null(special_col) && !is.null(filters$locked_special) && length(filters$locked_special) > 0) {
    for (locked_special in filters$locked_special) {
      filtered <- filtered[get(special_col) == locked_special]
    }
  }
  
  # Excluded players (must NOT be in lineup anywhere)
  if (!is.null(filters$excluded_players) && length(filters$excluded_players) > 0) {
    for (excluded_player in filters$excluded_players) {
      player_in_lineup <- rowSums(sapply(player_cols, function(col) {
        filtered[[col]] == excluded_player
      }), na.rm = TRUE) > 0
      
      filtered <- filtered[!player_in_lineup]
    }
  }
  
  # Excluded special position (Captain/MVP must NOT be specific player)
  if (!is.null(special_col) && !is.null(filters$excluded_special) && length(filters$excluded_special) > 0) {
    for (excluded_special in filters$excluded_special) {
      filtered <- filtered[get(special_col) != excluded_special]
    }
  }
  
  return(filtered)
}


# ============================================================================
# HELPER: BUILD PORTFOLIO FROM SELECTION
# ============================================================================

build_portfolio_from_selection <- function(optimal_lineups, selected_indices, label = NULL) {
  
  setDT(optimal_lineups)
  
  # Detect player columns
  if ("Captain" %in% names(optimal_lineups)) {
    player_cols <- c("Captain", grep("^Util", names(optimal_lineups), value = TRUE))
  } else if ("MVP" %in% names(optimal_lineups)) {
    player_cols <- c("MVP", grep("^Player", names(optimal_lineups), value = TRUE))
  } else {
    player_cols <- grep("^Player", names(optimal_lineups), value = TRUE)
  }
  
  # Select specified lineups
  portfolio <- optimal_lineups[selected_indices]
  
  # Add label if provided
  if (!is.null(label) && nchar(label) > 0) {
    portfolio[, Label := label]
  }
  
  # Add lineup ID
  portfolio[, LineupID := 1:.N]
  
  # Reorder columns: LineupID, Label (if exists), players, metrics
  metric_cols <- setdiff(names(portfolio), c(player_cols, "LineupID", "Label"))
  
  if ("Label" %in% names(portfolio)) {
    col_order <- c("LineupID", "Label", player_cols, metric_cols)
  } else {
    col_order <- c("LineupID", player_cols, metric_cols)
  }
  
  setcolorder(portfolio, col_order)
  
  return(portfolio)
}


# ============================================================================
# HELPER: GET ALL PLAYERS FROM LINEUPS
# ============================================================================

get_all_players_from_lineups <- function(lineup_data) {
  
  setDT(lineup_data)
  
  # Detect player columns
  if ("Captain" %in% names(lineup_data)) {
    player_cols <- c("Captain", grep("^Util", names(lineup_data), value = TRUE))
  } else if ("MVP" %in% names(lineup_data)) {
    player_cols <- c("MVP", grep("^Player", names(lineup_data), value = TRUE))
  } else {
    player_cols <- grep("^Player", names(lineup_data), value = TRUE)
  }
  
  # Get unique players
  all_players <- unique(unlist(lineup_data[, ..player_cols]))
  all_players <- all_players[!is.na(all_players)]
  
  return(sort(all_players))
}


# ============================================================================
# HELPER: DETECT LINEUP FORMAT
# ============================================================================

detect_lineup_format <- function(lineup_data) {
  
  if ("Captain" %in% names(lineup_data)) {
    return(list(
      format = "captain",
      player_cols = c("Captain", grep("^Util", names(lineup_data), value = TRUE)),
      special_col = "Captain",
      special_label = "Captain"
    ))
  } else if ("MVP" %in% names(lineup_data)) {
    return(list(
      format = "mvp",
      player_cols = c("MVP", grep("^Player", names(lineup_data), value = TRUE)),
      special_col = "MVP",
      special_label = "MVP"
    ))
  } else {
    return(list(
      format = "standard",
      player_cols = grep("^Player", names(lineup_data), value = TRUE),
      special_col = NULL,
      special_label = NULL
    ))
  }
}

# ============================================================================
# CUSTOM METRICS - SPORT-SPECIFIC LINEUP METRICS
# ============================================================================

add_custom_metrics <- function(lineup_results, metadata, config) {
  
  if (is.null(config$custom_metrics)) {
    return(lineup_results)
  }
  
  setDT(lineup_results)
  setDT(metadata)
  
  for (metric in config$custom_metrics) {
    if (metric$calculation == "team_stack") {
      lineup_results <- add_team_stack(lineup_results, metadata)
    } else if (metric$calculation == "sum") {
      lineup_results <- add_sum_metric(lineup_results, metadata, metric$source, metric$name)
    } else if (metric$calculation == "mean") {
      lineup_results <- add_mean_metric(lineup_results, metadata, metric$source, metric$name)
    }
  }
  
  return(lineup_results)
}

add_team_stack <- function(lineups, metadata) {
  player_team <- setNames(metadata$Team, metadata$Player)
  teams <- unique(metadata$Team)
  
  if ("Captain" %in% names(lineups)) {
    player_cols <- c("Captain", paste0("Player", 1:5))
  } else if ("MVP" %in% names(lineups)) {
    player_cols <- c("MVP", paste0("Player", 1:5))
  } else {
    player_cols <- grep("^Player", names(lineups), value = TRUE)
  }
  
  lineups[, TeamStack := {
    t <- sapply(player_cols, function(col) player_team[gsub(" \\(.*\\)", "", get(col))])
    c1 <- sum(t == teams[1], na.rm = TRUE)
    c2 <- sum(t == teams[2], na.rm = TRUE)
    paste0(teams[1], " ", c1, "-", c2)
  }, by = 1:nrow(lineups)]
  
  return(lineups)
}

add_sum_metric <- function(lineups, metadata, source_col, metric_name) {
  lookup <- setNames(metadata[[source_col]], metadata$Player)
  player_cols <- grep("^Player|^Captain|^MVP", names(lineups), value = TRUE)
  
  lineups[, (metric_name) := {
    sum(sapply(player_cols, function(col) {
      val <- lookup[gsub(" \\(.*\\)", "", get(col))]
      ifelse(is.na(val), 0, val)
    }))
  }, by = 1:nrow(lineups)]
  
  return(lineups)
}

add_mean_metric <- function(lineups, metadata, source_col, metric_name) {
  lookup <- setNames(metadata[[source_col]], metadata$Player)
  player_cols <- grep("^Player|^Captain|^MVP", names(lineups), value = TRUE)
  
  lineups[, (metric_name) := {
    vals <- sapply(player_cols, function(col) {
      val <- lookup[gsub(" \\(.*\\)", "", get(col))]
      ifelse(is.na(val), 0, val)
    })
    mean(vals[vals > 0])
  }, by = 1:nrow(lineups)]
  
  return(lineups)
}