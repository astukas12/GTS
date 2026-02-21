# ============================================================================
# UNIVERSAL SPORT CONFIGURATION SYSTEM
# Golden Ticket Sims
# ============================================================================

SPORT_CONFIGS <- list(
  
  # ==========================================================================
  # NASCAR
  # ==========================================================================
  NASCAR = list(
    sport_name         = "NASCAR",
    sport_display_name = "NASCAR",
    player_label       = "Driver",
    player_label_plural = "Drivers",
    
    detection = list(
      required_sheets   = c("Race_Weights", "Race_Profiles"),
      required_columns  = c("Starting", "team", "car"),   # lowercase as they appear in the Excel file
      min_sheet_matches = 2,
      min_column_matches = 2
    ),
    
    platforms          = c("DK", "FD"),
    roster_sizes       = list(DK = 6, FD = 5),
    salary_caps        = list(DK = 50000, FD = 50000),
    optimization_modes = list(DK = "combinatorial", FD = "combinatorial"),  # greedy per-sim, no LP
    max_lineups        = 5000,
    
    standard_metrics = c(
      "WinRate", "Top1Rate", "Top5Rate", "Top10Rate", "Top20Rate",
      "TotalSalary", "TotalOwn", "AvgOwn"
    ),
    
    custom_metrics = list(
      list(name = "TotalStart", source = "Starting", calculation = "sum",  label = "Total Start", decimals = 0),
      list(name = "AvgStart",   source = "Starting", calculation = "mean", label = "Avg Start",   decimals = 1)
    ),
    
    metadata_columns = list(
      list(name = "Starting", label = "Start Pos", type = "numeric", display = TRUE, filter = TRUE),
      list(name = "Team",     label = "Team",      type = "text",    display = TRUE, filter = FALSE),
      list(name = "Car",      label = "Car #",     type = "text",    display = TRUE, filter = FALSE)
    ),
    
    portfolio_filters = list(
      rate_minimums = list(
        list(name = "Win",   label = "Win",   step = 0.1),
        list(name = "Top1",  label = "Top 1", step = 0.1),
        list(name = "Top5",  label = "Top 5", step = 0.1),
        list(name = "Top10", label = "Top 10", step = 0.1),
        list(name = "Top20", label = "Top 20", step = 0.1)
      ),
      range_filters = list(
        list(name = "Salary",    label = "Salary (K)",  column = "TotalSalary",           step = 0.1, format = "salary_k"),
        list(name = "TotalOwn",  label = "Total Own",   column = "CumulativeOwnership",   step = 1,   format = "number"),
        list(name = "AvgOwn",    label = "Avg Own",     column = "GeometricMeanOwnership",step = 0.1, format = "decimal"),
        list(name = "TotalStart",label = "Total Start", column = "CumulativeStarting",    step = 1,   format = "number"),
        list(name = "AvgStart",  label = "Avg Start",   column = "GeometricMeanStarting", step = 0.1, format = "decimal")
      )
    ),
    
    platform_columns = list(
      DK = list(salary = "DKSalary", id = "DKID",  ownership = "DKOwn", score = "DKScore"),
      FD = list(salary = "FDSalary", id = "FDID",  name = "FDName", ownership = "FDOwn", score = "FDScore")
    ),
    
    download_formats = list(DK = "{Name} ({DKID})", FD = "{FDID}:{FDName}"),
    
    input_file = list(
      type            = "excel",
      required_sheets = c("Driver", "Race_Weights", "Race_Profiles"),
      player_sheet    = "Driver",
      required_columns = list(
        base     = c("Name"),
        DK       = c("DKSalary", "DKID", "DKOwn"),
        FD       = c("FDSalary", "FDID", "FDName", "FDOwn"),
        metadata = c("Starting", "Team", "Car")
      )
    ),
    
    simulation = list(
      function_name = "run_nascar_simulation",
      output_format = list(
        sim_results = c("SimID", "Player", "DKScore"),   # FDScore present only when has_fd=TRUE
        metadata    = c("Player", "DKSalary", "DKID", "DKOwn",
                        "Starting", "Team", "Car")             # FD columns validated at runtime via has_fd
      )
    ),
    
    lineup_metrics_function = "calculate_nascar_lineup_metrics"
  ),
  
  
  # ==========================================================================
  # MMA
  # ==========================================================================
  MMA = list(
    sport_name         = "MMA",
    sport_display_name = "MMA",
    player_label       = "Fighter",
    player_label_plural = "Fighters",
    
    detection = list(
      required_sheets   = c("Fights", "Scores"),
      required_columns  = NULL,
      min_sheet_matches = 2,
      min_column_matches = 0
    ),
    
    platforms    = c("DK", "FD", "SD"),
    roster_sizes = list(DK = 6, FD = 6, SD = 6),
    salary_caps  = list(DK = 50000, FD = 100, SD = 50000),
    # Small player pool: greedy optimal per sim, dedupe → ranked by Top1Count
    optimization_modes = list(DK = "combinatorial", FD = "combinatorial_mvp", SD = "combinatorial_captain"),
    max_lineups        = 5000,
    
    standard_metrics = c(
      "WinRate", "Top1Rate", "Top5Rate", "Top10Rate", "Top20Rate",
      "TotalSalary", "TotalOwn", "AvgOwn",
      "TotalEW", "Win6Pct", "Win5PlusPct"
    ),
    
    custom_metrics = list(
      list(name = "TotalEW",     source_column = "TotalEW",     calculation = "custom", label = "Total EW",  display_name = "Total EW",  decimals = 2, format = "decimal"),
      list(name = "Win6Pct",     source_column = "Win6Pct",     calculation = "custom", label = "Win 6 %",   display_name = "Win 6 %",   decimals = 1, format = "percentage"),
      list(name = "Win5PlusPct", source_column = "Win5PlusPct", calculation = "custom", label = "Win 5+ %",  display_name = "Win 5+ %",  decimals = 1, format = "percentage")
    ),
    
    metadata_columns = list(
      list(name = "WinProb", label = "Win Prob", type = "numeric", display = TRUE, filter = FALSE),
      list(name = "Opponent", label = "Opponent", type = "text",    display = TRUE, filter = FALSE)
    ),
    
    lineup_metrics_function = "calculate_mma_lineup_metrics",
    
    portfolio_filters = list(
      rate_minimums = list(
        list(name = "Win",   label = "Win",   step = 0.1),
        list(name = "Top1",  label = "Top 1", step = 0.1),
        list(name = "Top5",  label = "Top 5", step = 0.1),
        list(name = "Top10", label = "Top 10", step = 0.1),
        list(name = "Top20", label = "Top 20", step = 0.1)
      ),
      range_filters = list(
        list(name = "Salary",   label = "Salary (K)", column = "TotalSalary",            step = 0.1, format = "salary_k"),
        list(name = "TotalOwn", label = "Total Own",  column = "CumulativeOwnership",    step = 1,   format = "number"),
        list(name = "AvgOwn",   label = "Avg Own",    column = "GeometricMeanOwnership", step = 0.1, format = "decimal")
      )
    ),
    
    platform_columns = list(
      DK = list(salary = "DKSalary", id = "DKID", ownership = "DKOwn", score = "DKScore"),
      FD = list(salary = "FDSalary", id = "FDID", ownership = "FDOwn", score = "FDScore",
                has_mvp = TRUE, mvp_multiplier = 1.5, mvp_salary_multiplier = 1.0),
      # SD = DK Showdown: uses DK scoring and salaries, captain mode (1.5x)
      SD = list(salary = "SDSalary", id = "SDID", cpt_id = "CPTID",
                ownership = "DKOwn", score = "DKScore", cpt_multiplier = 1.5,
                platform_label = "DK Showdown")
    ),
    
    download_formats = list(
      DK     = "{Name} ({DKID})",
      FD     = "{FDID}:{Name}",
      SD     = "{Name} ({SDID})",      # Showdown utility slot
      SD_CPT = "{Name} ({CPTID})"      # Showdown captain slot
    ),
    
    input_file = list(
      type            = "excel",
      required_sheets = c("Fights", "Scores"),
      player_sheet    = "Fights",
      required_columns = list(
        base = c("Name", "Opponent"),
        DK   = c("DKSalary", "DKID", "DKOwn"),
        FD   = c("FDSalary", "FDID", "FDOwn"),
        SD   = c("CPTID", "SDID", "SDSal")
      )
    ),
    
    simulation = list(
      function_name = "run_mma_simulation",
      output_format = list(
        sim_results = c("SimID", "Player", "DKScore", "FDScore"),
        metadata    = c("Player", "DKSalary", "DKID", "DKOwn",
                        "FDSalary", "FDID", "FDOwn",
                        "CPTID", "SDID", "SDSalary")
      )
    ),
    
    showdown = list(
      enabled           = TRUE,
      captain_multiplier = 1.5,
      captain_salary_multiplier = 1.5,
      roster_structure  = list(captain_slots = 1, utility_slots = 5)
    )
  ),
  
  
  # ==========================================================================
  # TENNIS
  # ==========================================================================
  TENNIS = list(
    sport_name         = "TENNIS",
    sport_display_name = "Tennis",
    player_label       = "Player",
    player_label_plural = "Players",
    
    detection = list(
      required_sheets   = NULL,
      required_columns  = c("Surface", "Tour", "BO"),
      min_sheet_matches = 0,
      min_column_matches = 3
    ),
    
    platforms    = c("DK"),
    roster_sizes = list(DK = 6),
    salary_caps  = list(DK = 50000),
    
    max_lineups = 5000,
    
    standard_metrics = c(
      "WinRate", "Top1Rate", "Top5Rate", "Top10Rate", "Top20Rate",
      "TotalSalary", "TotalOwn", "AvgOwn",
      "TotalEW", "Win6Pct", "Win5PlusPct"
    ),
    
    custom_metrics = list(
      list(name = "TotalEW", calculation = "custom", label = "Total EW", decimals = 2),
      list(name = "AvgEW",   calculation = "custom", label = "Avg EW",   decimals = 2)
    ),
    
    metadata_columns = list(
      list(name = "Match",    label = "Match",    type = "text", display = TRUE, filter = FALSE),
      list(name = "Opponent", label = "Opponent", type = "text", display = TRUE, filter = FALSE),
      list(name = "Surface",  label = "Surface",  type = "text", display = TRUE, filter = TRUE),
      list(name = "Tour",     label = "Tour",     type = "text", display = TRUE, filter = TRUE)
    ),
    
    portfolio_filters = list(
      rate_minimums = list(
        list(name = "Win",   label = "Win",   step = 0.1),
        list(name = "Top1",  label = "Top 1", step = 0.1),
        list(name = "Top5",  label = "Top 5", step = 0.1),
        list(name = "Top10", label = "Top 10", step = 0.1),
        list(name = "Top20", label = "Top 20", step = 0.1)
      ),
      range_filters = list(
        list(name = "Salary",   label = "Salary (K)", column = "TotalSalary",            step = 0.1, format = "salary_k"),
        list(name = "TotalOwn", label = "Total Own",  column = "CumulativeOwnership",    step = 1,   format = "number"),
        list(name = "AvgOwn",   label = "Avg Own",    column = "GeometricMeanOwnership", step = 0.1, format = "decimal")
      )
    ),
    
    platform_columns = list(
      DK = list(salary = "Salary", id = "ID", ownership = "Own", score_column = "DKScore")
    ),
    
    download_formats = list(DK = "{Name} ({ID})"),
    
    input_file = list(
      type            = "excel",
      required_sheets = NULL,
      player_sheet    = NULL,
      required_columns = list(
        base = c("Name", "Salary", "ID", "Own", "Game Info", "ML", "SS", "Surface", "Tour", "BO"),
        DK   = c("Salary", "ID", "Own")
      )
    ),
    
    simulation = list(
      function_name          = "run_tennis_engine",
      requires_historical_data = TRUE,
      historical_data_file   = "tennis_clean_database.xlsx",
      output_format = list(
        sim_results = c("SimID", "Player", "DKScore"),
        metadata    = c("Player", "DKSalary", "DKID", "DKOwn", "Match", "Opponent")
      )
    ),
    
    lineup_metrics_function = "calculate_tennis_lineup_metrics"
  ),
  
  
  # ==========================================================================
  # NFL
  # ==========================================================================
  NFL = list(
    sport_name         = "NFL",
    sport_display_name = "NFL",
    player_label       = "Player",
    player_label_plural = "Players",
    
    detection = list(
      custom_detect = function(sheets) {
        has_rushing   <- any(grepl("_Rushing$",   sheets))
        has_receiving <- any(grepl("_Receiving$", sheets))
        has_passing   <- any(grepl("_Passing$",   sheets))
        has_salaries  <- "Salaries" %in% sheets
        has_similar   <- "Similar_Games" %in% sheets
        has_rushing && has_receiving && has_passing && has_salaries && has_similar
      },
      required_sheets   = NULL,
      required_columns  = NULL,
      min_sheet_matches = 0,
      min_column_matches = 0
    ),
    
    platforms    = c("DK", "FD"),
    roster_sizes = list(DK = 6, FD = 6),
    salary_caps  = list(DK = 50000, FD = 60000),
    
    showdown_config = list(
      DK = list(enabled = TRUE,  captain_multiplier = 1.5, captain_salary_multiplier = 1.5, mode = "captain"),
      FD = list(enabled = TRUE,  mvp_multiplier = 1.5,     mvp_salary_multiplier = 1.5,     mode = "mvp")
    ),
    
    standard_metrics = c(
      "WinRate", "Top1Rate", "Top5Rate", "Top10Rate", "Top20Rate",
      "TotalSalary", "TotalOwn", "AvgOwn"
    ),
    
    custom_metrics = list(
      list(name = "TeamStack", source = "Team", calculation = "team_stack", label = "Team Stack")
    ),
    
    metadata_columns = list(
      list(name = "Pos",  label = "Position", type = "text", display = TRUE, filter = TRUE),
      list(name = "Team", label = "Team",     type = "text", display = TRUE, filter = FALSE)
    ),
    
    portfolio_filters = list(
      rate_minimums = list(
        list(name = "Win",   label = "Win",   step = 0.1),
        list(name = "Top1",  label = "Top 1", step = 0.1),
        list(name = "Top5",  label = "Top 5", step = 0.1),
        list(name = "Top10", label = "Top 10", step = 0.1),
        list(name = "Top20", label = "Top 20", step = 0.1)
      ),
      range_filters = list(
        list(name = "Salary",   label = "Salary (K)", column = "TotalSalary",            step = 0.1, format = "salary_k"),
        list(name = "TotalOwn", label = "Total Own",  column = "CumulativeOwnership",    step = 1,   format = "number"),
        list(name = "AvgOwn",   label = "Avg Own",    column = "GeometricMeanOwnership", step = 0.1, format = "decimal")
      )
    ),
    
    platform_columns = list(
      DK = list(salary = "DKSalary", id = "DKID", ownership = "DKOwn", score = "DKScore"),
      FD = list(salary = "FDSalary", id = "FDID", ownership = "FDOwn", score = "FDScore",
                has_mvp = TRUE, mvp_multiplier = 1.5, mvp_salary_multiplier = 1.5)
    ),
    
    download_formats = list(DK = "{Name} ({DKID})", FD = "{FDID}:{Name}"),
    
    input_file = list(
      type            = "excel",
      load_all_sheets = TRUE,
      required_sheets = c("Salaries", "Similar_Games"),
      player_sheet    = "Salaries",
      required_columns = list(
        base     = c("Name", "Team"),
        DK       = c("DKSal", "DKID", "DKFOwn", "Pos"),
        FD       = c("FDSal", "FDID", "FDFOwn"),
        metadata = c("Team")
      )
    ),
    
    simulation = list(
      function_name = "run_nfl_simulation",
      output_format = list(
        sim_results = c("SimID", "Player", "Team", "DKScore", "FDScore"),
        metadata    = c("Player", "Team", "Pos",
                        "DKSalary", "DKID", "DKOwn",
                        "FDSalary", "FDID", "FDOwn")
      )
    ),
    max_lineups = 5000
  ),
  
  
  # ==========================================================================
  # GOLF
  # ==========================================================================
  GOLF = list(
    sport_name         = "GOLF",
    sport_display_name = "Golf",
    player_label       = "Golfer",
    player_label_plural = "Golfers",
    
    # Detection: Player sheet + DKPts sheet + golf probability columns
    # Checked AFTER Tennis (which also has a Player sheet) because golf
    # needs DKPts and tennis does not.
    detection = list(
      required_sheets   = c("Player", "DKPts"),
      required_columns  = c("W", "T5", "Cut"),   # golf-specific probability cols
      min_sheet_matches = 2,
      min_column_matches = 2
    ),
    
    platforms    = c("DK", "FD"),
    roster_sizes = list(DK = 6, FD = 6),
    salary_caps  = list(DK = 50000, FD = 60000),
    
    # Golf-specific Phase 1 settings
    phase1_n_sample  = 25000L,   # random salary-valid lineups to draw
    max_lineups      = 5000,
    phase1_target    = 5000L,    # top N by ExpectedCuts to keep
    
    standard_metrics = c(
      "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
      "AvgScore", "TotalSalary", "CumulativeOwnership", "GeometricMeanOwnership"
    ),
    
    # Golf custom metrics added after Phase 3
    custom_metrics = list(
      list(name = "ExpectedCuts", calculation = "custom", label = "Exp Cuts",  decimals = 2),
      list(name = "AtLeast6",     calculation = "custom", label = "All 6 Cut%", decimals = 1),
      list(name = "AtLeast5",     calculation = "custom", label = "5+ Cut%",    decimals = 1),
      list(name = "EarlyLateCount", calculation = "custom", label = "EL Count", decimals = 0)
    ),
    
    metadata_columns = list(
      list(name = "TeeTimeGroup", label = "Tee Group", type = "text",    display = TRUE,  filter = TRUE),
      list(name = "CutProb",      label = "Cut%",      type = "numeric", display = TRUE,  filter = FALSE)
    ),
    
    portfolio_filters = list(
      rate_minimums = list(
        list(name = "Win",   label = "Win",   step = 0.1),
        list(name = "Top1",  label = "Top 1", step = 0.5),
        list(name = "Top5",  label = "Top 5", step = 1),
        list(name = "Top10", label = "Top 10", step = 2),
        list(name = "Top20", label = "Top 20", step = 5)
      ),
      range_filters = list(
        list(name = "Salary",   label = "Salary (K)", column = "TotalSalary",            step = 0.1, format = "salary_k"),
        list(name = "TotalOwn", label = "Total Own",  column = "CumulativeOwnership",    step = 1,   format = "number"),
        list(name = "AvgOwn",   label = "Avg Own",    column = "GeometricMeanOwnership", step = 0.1, format = "decimal")
      ),
      # Cut-specific filters (hidden when no_cut = TRUE)
      cut_filters = list(
        list(name = "MinAt6",   label = "Min All 6 Cut%", column = "AtLeast6",     step = 1, default = 0),
        list(name = "MinAt5",   label = "Min 5+ Cut%",    column = "AtLeast5",     step = 1, default = 0),
        list(name = "MinExpCut",label = "Min Exp Cuts",   column = "ExpectedCuts", step = 0.1, default = 0)
      ),
      # Always-visible golf filters
      always_filters = list(
        list(name = "EarlyLate", label = "EarlyLate Count", column = "EarlyLateCount",
             type = "slider", min = 0, max = 6, default = c(0, 6), step = 1)
      )
    ),
    
    platform_columns = list(
      DK = list(salary = "DKSalary", id = "DKID", ownership = "DKOwn", score = "DKScore"),
      FD = list(salary = "FDSalary", id = "FDID", ownership = "FDOwn", score = "FDScore")
    ),
    
    download_formats = list(
      DK = "{Name} ({DKID})",
      FD = "{FDID}:{Name}"
    ),
    
    input_file = list(
      type            = "excel",
      required_sheets = c("Player", "DKPts"),
      player_sheet    = "Player",
      required_columns = list(
        base     = c("Name", "W", "T5", "T10", "Cut"),
        DK       = c("DKSalary", "DKOP"),
        FD       = c("FDSalary", "FDOP"),
        metadata = c("Pool")
      )
    ),
    
    simulation = list(
      function_name = "run_golf_simulation",
      output_format = list(
        sim_results = c("SimID", "Player", "DKScore", "FDScore", "Pool", "FinishPosition"),
        metadata    = c("Player", "Pool", "TeeTimeGroup", "CutProb",
                        "DKSalary", "DKOwn",
                        "FDSalary", "FDOwn")
      )
    ),
    
    # Called by add_custom_metrics() after Phase 3
    lineup_metrics_function = "calculate_golf_lineup_metrics"
  )
  
)


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

detect_sport <- function(file_path) {
  file_ext <- tools::file_ext(file_path)
  
  if (file_ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(file_path)
    
    for (sport_name in names(SPORT_CONFIGS)) {
      config    <- SPORT_CONFIGS[[sport_name]]
      detection <- config$detection
      
      # Custom detect function (NFL)
      if (!is.null(detection$custom_detect)) {
        if (detection$custom_detect(sheets)) return(sport_name)
        next
      }
      
      # Sheet-based detection
      if (!is.null(detection$required_sheets)) {
        n_matches <- sum(detection$required_sheets %in% sheets)
        if (n_matches >= detection$min_sheet_matches) {
          # For sports that also need column matches (e.g. Golf vs Tennis),
          # do a quick column check on the player sheet
          if (!is.null(detection$required_columns) && detection$min_column_matches > 0) {
            player_sheet <- config$input_file$player_sheet
            if (!is.null(player_sheet) && player_sheet %in% sheets) {
              first_row <- suppressMessages(
                readxl::read_excel(file_path, sheet = player_sheet, n_max = 1)
              )
              col_matches <- sum(detection$required_columns %in% names(first_row))
              if (col_matches >= detection$min_column_matches) return(sport_name)
            } else {
              return(sport_name)
            }
          } else {
            return(sport_name)
          }
        }
      }
    }
    
    # Column-only detection fallback (Tennis)
    first_row  <- suppressMessages(readxl::read_excel(file_path, sheet = 1, n_max = 1))
    col_names  <- names(first_row)
    
    for (sport_name in names(SPORT_CONFIGS)) {
      config    <- SPORT_CONFIGS[[sport_name]]
      detection <- config$detection
      if (!is.null(detection$required_columns) && is.null(detection$required_sheets)) {
        col_matches <- sum(detection$required_columns %in% col_names)
        if (col_matches >= detection$min_column_matches) return(sport_name)
      }
    }
    
  } else if (file_ext == "csv") {
    first_row <- read.csv(file_path, nrows = 1, check.names = FALSE)
    col_names <- names(first_row)
    for (sport_name in names(SPORT_CONFIGS)) {
      config    <- SPORT_CONFIGS[[sport_name]]
      detection <- config$detection
      if (!is.null(detection$required_columns)) {
        col_matches <- sum(detection$required_columns %in% col_names)
        if (col_matches >= detection$min_column_matches) return(sport_name)
      }
    }
  }
  
  stop(
    "Could not detect sport from input file.\n",
    "Supported sports: ", paste(names(SPORT_CONFIGS), collapse = ", "), "\n",
    "Please check your file format matches one of the supported sports."
  )
}


get_sport_config <- function(sport) {
  if (!sport %in% names(SPORT_CONFIGS)) {
    stop(paste("Unknown sport:", sport, "\nAvailable sports:",
               paste(names(SPORT_CONFIGS), collapse = ", ")))
  }
  SPORT_CONFIGS[[sport]]
}


get_all_metrics <- function(config) {
  metrics <- config$standard_metrics
  if (!is.null(config$custom_metrics)) {
    custom_names <- sapply(config$custom_metrics, function(x) x$name)
    metrics <- c(metrics, custom_names)
  }
  metrics
}


get_platform_config <- function(config, platform) {
  if (!platform %in% config$platforms) {
    stop(paste("Platform", platform, "not available for", config$sport_name))
  }
  list(
    salary_cap   = config$salary_caps[[platform]],
    roster_size  = config$roster_sizes[[platform]],
    columns      = config$platform_columns[[platform]],
    download_format = config$download_formats[[platform]]
  )
}


validate_simulation_output <- function(sim_results, metadata, config) {
  expected_sim  <- config$simulation$output_format$sim_results
  missing_sim   <- setdiff(expected_sim, names(sim_results))
  if (length(missing_sim) > 0)
    stop("Missing columns in sim_results: ", paste(missing_sim, collapse = ", "))
  
  expected_meta <- config$simulation$output_format$metadata
  missing_meta  <- setdiff(expected_meta, names(metadata))
  if (length(missing_meta) > 0)
    stop("Missing columns in metadata: ", paste(missing_meta, collapse = ", "))
  
  sim_players  <- unique(sim_results$Player)
  meta_players <- metadata$Player
  extra        <- setdiff(sim_players, meta_players)
  if (length(extra) > 0)
    stop("Players in sim_results not found in metadata: ", paste(extra, collapse = ", "))
  
  return(TRUE)
}