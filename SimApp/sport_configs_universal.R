# ============================================================================
# UNIVERSAL SPORT CONFIGURATION SYSTEM
# Golden Ticket Sims - Template System
# ============================================================================

SPORT_CONFIGS <- list(
  
  # ==========================================================================
  # NASCAR CONFIGURATION
  # ==========================================================================
  NASCAR = list(
    # Basic Information
    sport_name = "NASCAR",
    sport_display_name = "NASCAR",
    player_label = "Driver",
    player_label_plural = "Drivers",
    
    # Auto-Detection Rules
    detection = list(
      required_sheets = c("Race_Weights", "Race_Profiles"),
      required_columns = c("Starting", "Team", "Car"),
      min_sheet_matches = 2,
      min_column_matches = 2
    ),
    
    # Platforms Available
    platforms = c("DK", "FD"),
    
    # Roster Structure (by platform)
    roster_sizes = list(
      DK = 6,
      FD = 5
    ),
    
    # Salary Caps (by platform)
    salary_caps = list(
      DK = 50000,
      FD = 50000
    ),
    
    # Standard Metrics
    standard_metrics = c(
      "WinRate", "Top1Rate", "Top5Rate", "Top10Rate", "Top20Rate",
      "TotalSalary", "TotalOwn", "AvgOwn"
    ),
    
    # Custom Metrics
    custom_metrics = list(
      list(
        name = "TotalStart",
        source = "Starting",
        calculation = "sum",
        label = "Total Start",
        decimals = 0
      ),
      list(
        name = "AvgStart",
        source = "Starting",
        calculation = "mean",
        label = "Avg Start",
        decimals = 1
      )
    ),
    
    # Metadata Columns
    metadata_columns = list(
      list(
        name = "Starting",
        label = "Start Pos",
        type = "numeric",
        decimals = 0,
        display = TRUE,
        filter = TRUE
      ),
      list(
        name = "Team",
        label = "Team",
        type = "text",
        display = TRUE,
        filter = FALSE
      ),
      list(
        name = "Car",
        label = "Car #",
        type = "text",
        display = TRUE,
        filter = FALSE
      )
    ),
    
    # Portfolio Builder Filter Configuration
    portfolio_filters = list(
      rate_minimums = list(
        list(name = "Win", label = "Win", step = 0.1),
        list(name = "Top1", label = "Top 1", step = 0.1),
        list(name = "Top5", label = "Top 5", step = 0.1),
        list(name = "Top10", label = "Top 10", step = 0.1),
        list(name = "Top20", label = "Top 20", step = 0.1)
      ),
      range_filters = list(
        list(
          name = "Salary",
          label = "Salary (K)",
          column = "Salary",
          step = 0.1,
          format = "salary_k"
        ),
        list(
          name = "TotalOwn",
          label = "Total Own",
          column = "TotalOwn",
          step = 1,
          format = "number"
        ),
        list(
          name = "AvgOwn",
          label = "Avg Own",
          column = "AvgOwn",
          step = 0.1,
          format = "decimal"
        ),
        list(
          name = "TotalStart",
          label = "Total Start",
          column = "TotalStart",
          step = 1,
          format = "number"
        ),
        list(
          name = "AvgStart",
          label = "Avg Start",
          column = "AvgStart",
          step = 0.1,
          format = "decimal"
        )
      )
    ),
    
    # Platform-Specific Column Names
    platform_columns = list(
      DK = list(
        salary = "DKSalary",
        id = "DKID",
        ownership = "DKOwn",
        score = "DKScore"
      ),
      FD = list(
        salary = "FDSalary",
        id = "FDID",
        name = "FDName",
        ownership = "FDOwn",
        score = "FDScore"
      )
    ),
    
    # Download Formats
    download_formats = list(
      DK = "{Name} ({DKID})",
      FD = "{FDID}:{FDName}"
    ),
    
    # Input File Configuration
    input_file = list(
      type = "excel",
      required_sheets = c("Driver", "Race_Weights", "Race_Profiles"),
      player_sheet = "Driver",
      required_columns = list(
        base = c("Name"),
        DK = c("DKSalary", "DKID", "DKOwn"),
        FD = c("FDSalary", "FDID", "FDName", "FDOwn"),
        metadata = c("Starting", "Team", "Car")
      )
    ),
    
    # Simulation Function
    simulation = list(
      function_name = "run_nascar_simulation",
      output_format = list(
        sim_results = c("SimID", "Player", "DKScore", "FDScore"),
        metadata = c("Player", "DKSalary", "DKID", "DKOwn", 
                     "FDSalary", "FDID", "FDName", "FDOwn",
                     "Starting", "Team", "Car")
      )
    )
  ),
  
  
  # ==========================================================================
  # MMA CONFIGURATION
  # ==========================================================================
  MMA = list(
    # Basic Information
    sport_name = "MMA",
    sport_display_name = "MMA",
    player_label = "Fighter",
    player_label_plural = "Fighters",
    
    # Auto-Detection Rules
    detection = list(
      required_sheets = c("Fights", "Scores"),
      required_columns = NULL,
      min_sheet_matches = 2,
      min_column_matches = 0
    ),
    
    # Platforms Available (includes Showdown)
    platforms = c("DK", "FD", "SD"),
    
    # Roster Structure (by platform)
    roster_sizes = list(
      DK = 6,
      FD = 6,
      SD = 6  # 1 Captain + 5 Utility
    ),
    
    # Salary Caps (by platform)
    salary_caps = list(
      DK = 50000,
      FD = 100,
      SD = 50000
    ),
    
    # Standard Metrics (same across all sports)
    standard_metrics = c(
      "WinRate", "Top1Rate", "Top5Rate", "Top10Rate", "Top20Rate",
      "TotalSalary", "TotalOwn", "AvgOwn"
    ),
    
    # Custom Metrics (MMA has none)
    custom_metrics = NULL,
    
    # Metadata Columns (MMA has none beyond salary/own/id)
    metadata_columns = NULL,
    
    # Portfolio Builder Filter Configuration
    portfolio_filters = list(
      rate_minimums = list(
        list(name = "Win", label = "Win", step = 0.1),
        list(name = "Top1", label = "Top 1", step = 0.1),
        list(name = "Top5", label = "Top 5", step = 0.1),
        list(name = "Top10", label = "Top 10", step = 0.1),
        list(name = "Top20", label = "Top 20", step = 0.1)
      ),
      range_filters = list(
        list(
          name = "Salary",
          label = "Salary (K)",
          column = "Salary",
          step = 0.1,
          format = "salary_k"
        ),
        list(
          name = "TotalOwn",
          label = "Total Own",
          column = "TotalOwn",
          step = 1,
          format = "number"
        ),
        list(
          name = "AvgOwn",
          label = "Avg Own",
          column = "AvgOwn",
          step = 0.1,
          format = "decimal"
        )
      )
    ),
    
    # Platform-Specific Column Names
    platform_columns = list(
      DK = list(
        salary = "DKSalary",
        id = "DKID",
        ownership = "DKOwn",
        score = "DKScore"
      ),
      FD = list(
        salary = "FDSalary",
        id = "FDID",
        ownership = "FDOwn",
        score = "FDScore",
        has_mvp = TRUE,              # NEW: FD MMA has MVP mechanic
        mvp_multiplier = 1.5,         # NEW: MVP gets 1.5x points
        mvp_salary_multiplier = 1.0   # NEW: No salary change for MVP
      ),
      SD = list(
        salary = "SDSalary",
        id = "SDID",
        cpt_id = "CPTID",
        ownership = "DKOwn",  # SD uses DK ownership
        score = "DKScore",    # SD uses DK scoring
        cpt_multiplier = 1.5
      )
    ),
    
    # Download Formats
    download_formats = list(
      DK = "{Name} ({DKID})",
      FD = "{FDID}:{Name}",
      SD = "{Name} ({SDID})",
      SD_CPT = "{Name} ({CPTID})"  # Captain format
    ),
    
    # Input File Configuration
    input_file = list(
      type = "excel",
      required_sheets = c("Fights", "Scores"),
      player_sheet = "Fights",
      required_columns = list(
        base = c("Name", "Opponent"),
        DK = c("DKSalary", "DKID", "DKOwn"),
        FD = c("FDSalary", "FDID", "FDOwn"),
        SD = c("CPTID", "SDID", "SDSal")  # Showdown-specific
      )
    ),
    
    # Simulation Function
    simulation = list(
      function_name = "run_mma_simulation",
      output_format = list(
        sim_results = c("SimID", "Player", "DKScore", "FDScore"),
        metadata = c("Player", "DKSalary", "DKID", "DKOwn",
                     "FDSalary", "FDID", "FDOwn",
                     "CPTID", "SDID", "SDSalary")
      )
    ),
    
    # Showdown-specific configuration
    showdown = list(
      enabled = TRUE,
      captain_multiplier = 1.5,
      captain_salary_multiplier = 1.5,
      roster_structure = list(
        captain_slots = 1,
        utility_slots = 5
      )
    )
  ),
  
  NFL = list(
    sport_name = "NFL",
    sport_display_name = "NFL",
    player_label = "Player",
    player_label_plural = "Players",
    
    detection = list(
      custom_detect = function(sheets) {
        # NFL has team-based sheets: TEAM_Rushing, TEAM_Receiving, etc.
        has_rushing <- any(grepl("_Rushing$", sheets))
        has_receiving <- any(grepl("_Receiving$", sheets))
        has_passing <- any(grepl("_Passing$", sheets))
        has_salaries <- "Salaries" %in% sheets
        has_similar <- "Similar_Games" %in% sheets
        
        return(has_rushing && has_receiving && has_passing && has_salaries && has_similar)
      },
      required_sheets = NULL,
      required_columns = NULL,
      min_sheet_matches = 0,
      min_column_matches = 0
    ),
    
    platforms = c("DK", "FD"),
    
    roster_sizes = list(
      DK = 6,  # Showdown: 1 Captain + 5 FLEX
      FD = 6   # MVP: 1 MVP + 5 FLEX
    ),
    
    salary_caps = list(
      DK = 50000,
      FD = 60000
    ),
    
    # Showdown/MVP configurations
    showdown_config = list(
      DK = list(
        enabled = TRUE,
        captain_multiplier = 1.5,
        captain_salary_multiplier = 1.5,
        mode = "captain"  # Use captain mode in optimizer
      ),
      FD = list(
        enabled = TRUE,
        mvp_multiplier = 1.5,
        mvp_salary_multiplier = 1.5,
        mode = "mvp"  # Use mvp mode in optimizer
      )
    ),
    
    standard_metrics = c(
      "WinRate", "Top1Rate", "Top5Rate", "Top10Rate", "Top20Rate",
      "TotalSalary", "TotalOwn", "AvgOwn"
    ),
    
    custom_metrics = list(
      list(
        name = "TeamStack",
        source = "Team",
        calculation = "team_stack",
        label = "Team Stack"
      )
    ),
    
    metadata_columns = list(
      list(
        name = "Pos",
        label = "Position",
        type = "text",
        display = TRUE,
        filter = TRUE
      ),
      list(
        name = "Team",
        label = "Team",
        type = "text",
        display = TRUE,
        filter = FALSE
      )
    ),
    
    portfolio_filters = list(
      rate_minimums = list(
        list(name = "Win", label = "Win", step = 0.1),
        list(name = "Top1", label = "Top 1", step = 0.1),
        list(name = "Top5", label = "Top 5", step = 0.1),
        list(name = "Top10", label = "Top 10", step = 0.1),
        list(name = "Top20", label = "Top 20", step = 0.1)
      ),
      range_filters = list(
        list(
          name = "Salary",
          label = "Salary (K)",
          column = "Salary",
          step = 0.1,
          format = "salary_k"
        ),
        list(
          name = "TotalOwn",
          label = "Total Own",
          column = "TotalOwn",
          step = 1,
          format = "number"
        ),
        list(
          name = "AvgOwn",
          label = "Avg Own",
          column = "AvgOwn",
          step = 0.1,
          format = "decimal"
        )
      )
    ),
    
    platform_columns = list(
      DK = list(
        salary = "DKSalary",
        id = "DKID",
        ownership = "DKOwn",
        score = "DKScore"
      ),
      FD = list(
        salary = "FDSalary",
        id = "FDID",
        ownership = "FDOwn",
        score = "FDScore",
        has_mvp = TRUE,
        mvp_multiplier = 1.5,
        mvp_salary_multiplier = 1.5
      )
    ),
    
    download_formats = list(
      DK = "{Name} ({DKID})",
      FD = "{FDID}:{Name}"
    ),
    
    input_file = list(
      type = "excel",
      load_all_sheets = TRUE,  # NFL needs dynamic team sheets
      required_sheets = c("Salaries", "Similar_Games"),
      player_sheet = "Salaries",
      required_columns = list(
        base = c("Name", "Team"),
        DK = c("DKSal", "DKID", "DKFOwn", "Pos"),
        FD = c("FDSal", "FDID", "FDFOwn"),
        metadata = c("Team")
      )
    ),
    
    simulation = list(
      function_name = "run_nfl_simulation",
      output_format = list(
        sim_results = c("SimID", "Player", "Team", "DKScore", "FDScore"),
        metadata = c("Player", "Team", "Pos",
                     "DKSalary", "DKID", "DKOwn",
                     "FDSalary", "FDID", "FDOwn")
      )
    )
  )
)



# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Auto-detect sport from input file
#' @param file_path Path to input file
#' @return Sport name (e.g., "NASCAR", "MMA")
detect_sport <- function(file_path) {
  file_ext <- tools::file_ext(file_path)
  
  if (file_ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(file_path)
    
    # Try to match against each sport's detection rules
    for (sport_name in names(SPORT_CONFIGS)) {
      config <- SPORT_CONFIGS[[sport_name]]
      detection <- config$detection
      
      # Check for custom detection function first
      if (!is.null(detection$custom_detect)) {
        if (detection$custom_detect(sheets)) {
          return(sport_name)
        }
      }
      
      # Check sheet names
      if (!is.null(detection$required_sheets)) {
        sheet_matches <- sum(detection$required_sheets %in% sheets)
        
        if (sheet_matches >= detection$min_sheet_matches) {
          return(sport_name)
        }
      }
    }
    
    # If no match, try reading first sheet and checking columns
    first_sheet <- readxl::read_excel(file_path, sheet = 1, n_max = 1)
    column_names <- names(first_sheet)
    
    for (sport_name in names(SPORT_CONFIGS)) {
      config <- SPORT_CONFIGS[[sport_name]]
      detection <- config$detection
      
      if (!is.null(detection$required_columns)) {
        column_matches <- sum(detection$required_columns %in% column_names)
        
        if (column_matches >= detection$min_column_matches) {
          return(sport_name)
        }
      }
    }
    
  } else if (file_ext == "csv") {
    # For CSV, check column names
    first_row <- read.csv(file_path, nrows = 1, check.names = FALSE)
    column_names <- names(first_row)
    
    for (sport_name in names(SPORT_CONFIGS)) {
      config <- SPORT_CONFIGS[[sport_name]]
      detection <- config$detection
      
      if (!is.null(detection$required_columns)) {
        column_matches <- sum(detection$required_columns %in% column_names)
        
        if (column_matches >= detection$min_column_matches) {
          return(sport_name)
        }
      }
    }
  }
  
  # If no sport detected, return error with helpful message
  stop(
    "Could not detect sport from input file.\n",
    "Supported sports: ", paste(names(SPORT_CONFIGS), collapse = ", "), "\n",
    "Please check your file format matches one of the supported sports."
  )
}


#' Get sport configuration
#' @param sport Sport name (e.g., "NASCAR", "MMA")
#' @return Sport configuration list
get_sport_config <- function(sport) {
  if (!sport %in% names(SPORT_CONFIGS)) {
    stop(paste("Unknown sport:", sport, "\nAvailable sports:", 
               paste(names(SPORT_CONFIGS), collapse = ", ")))
  }
  return(SPORT_CONFIGS[[sport]])
}


#' Get all metrics (standard + custom) for a sport
#' @param config Sport configuration
#' @return Character vector of all metric names
get_all_metrics <- function(config) {
  metrics <- config$standard_metrics
  
  if (!is.null(config$custom_metrics)) {
    custom_names <- sapply(config$custom_metrics, function(x) x$name)
    metrics <- c(metrics, custom_names)
  }
  
  return(metrics)
}


#' Get platform-specific configuration
#' @param config Sport configuration
#' @param platform Platform code ("DK", "FD", "SD")
#' @return Platform-specific configuration
get_platform_config <- function(config, platform) {
  if (!platform %in% config$platforms) {
    stop(paste("Platform", platform, "not available for", config$sport_name))
  }
  
  list(
    salary_cap = config$salary_caps[[platform]],
    roster_size = config$roster_sizes[[platform]],
    columns = config$platform_columns[[platform]],
    download_format = config$download_formats[[platform]]
  )
}


#' Validate simulation output
#' @param sim_results Simulation results data.table
#' @param metadata Player metadata data.table
#' @param config Sport configuration
#' @return TRUE if valid, error message if not
validate_simulation_output <- function(sim_results, metadata, config) {
  # Check sim results columns
  expected_sim_cols <- config$simulation$output_format$sim_results
  missing_sim_cols <- setdiff(expected_sim_cols, names(sim_results))
  
  if (length(missing_sim_cols) > 0) {
    stop("Missing columns in sim_results: ", paste(missing_sim_cols, collapse = ", "))
  }
  
  # Check metadata columns
  expected_meta_cols <- config$simulation$output_format$metadata
  missing_meta_cols <- setdiff(expected_meta_cols, names(metadata))
  
  if (length(missing_meta_cols) > 0) {
    stop("Missing columns in metadata: ", paste(missing_meta_cols, collapse = ", "))
  }
  
  # Check that players match
  sim_players <- unique(sim_results$Player)
  meta_players <- metadata$Player
  
  if (!all(sim_players %in% meta_players)) {
    missing_players <- setdiff(sim_players, meta_players)
    stop("Players in sim_results not found in metadata: ", 
         paste(missing_players, collapse = ", "))
  }
  
  return(TRUE)
}