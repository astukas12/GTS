# NASCAR Contest Simulation App - Section 1: Basic Structure
# app.R

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(DT)
library(plotly)
library(lpSolve)
library(memoise)
library(shinycssloaders)
library(shinyjs)
library(readxl)

# Custom CSS for black and yellow theme
custom_css <- "
  /* Override dashboard header colors */
  .skin-blue .main-header {
    background-color: #000000;
    border-bottom: 2px solid #FFD700;
  }
  .skin-blue .main-header .logo {
    background-color: #000000;
    color: #FFD700;
    font-weight: bold;
  }
  .skin-blue .main-header .logo:hover {
    background-color: #333333;
    color: #FFD700;
  }
  .skin-blue .main-header .navbar {
    background-color: #000000;
  }
  
  /* Override dashboard sidebar colors */
  .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
    background-color: #1a1a1a;
  }
  .skin-blue .sidebar a {
    color: #FFD700;
  }
  .skin-blue .sidebar-menu > li.active > a, 
  .skin-blue .sidebar-menu > li:hover > a {
    color: #000000;
    background: #FFD700;
    border-left-color: #FFD700;
    font-weight: bold;
  }
  
  /* Customize box headers */
  .box.box-primary .box-header {
    background-color: #FFD700;
    color: #000000;
    font-weight: bold;
  }
  
  /* Style buttons */
  .btn-primary {
    background-color: #FFD700;
    border-color: #E6C200;
    color: #000000;
    font-weight: bold;
  }
  .btn-primary:hover, .btn-primary:focus {
    background-color: #E6C200;
    border-color: #CCAD00;
    color: #000000;
  }
  
  .btn-default {
    background-color: #333333;
    border-color: #FFD700;
    color: #FFD700;
  }
  .btn-default:hover {
    background-color: #FFD700;
    color: #000000;
  }
  
  /* Success/info alerts */
  .alert-success {
    background-color: #2d5a2d;
    border-color: #FFD700;
    color: #FFD700;
  }
  
  .alert-info {
    background-color: #1a3a5c;
    border-color: #FFD700;
    color: #FFD700;
  }
"

# Global constants
DK_ROSTER_SIZE <- 6
FD_ROSTER_SIZE <- 5
DK_SALARY_CAP <- 50000
FD_SALARY_CAP <- 50000

# Contest type definitions
CONTEST_TYPES <- list(
  "Double Up" = list(type = "double_up", payout_pct = 0.45, multiplier = 2),
  "3x" = list(type = "winner_take_all", payout_pct = 1/3, multiplier = 3),
  "4x" = list(type = "winner_take_all", payout_pct = 1/4, multiplier = 4),
  "5x" = list(type = "winner_take_all", payout_pct = 1/5, multiplier = 5),
  "10x" = list(type = "winner_take_all", payout_pct = 1/10, multiplier = 10),
  "H2H" = list(type = "head_to_head", payout_pct = 0.5, multiplier = 2)
)


find_top_lineups_for_sim <- function(sim_data, salary_col, points_col, name_col, roster_size, salary_cap, k = 5) {
  tryCatch({
    n_drivers <- nrow(sim_data)
    
    if(n_drivers < roster_size) {
      return(NULL)
    }
    
    # Prepare data for optimization
    drivers <- sim_data[[name_col]]
    salaries <- sim_data[[salary_col]]
    points <- sim_data[[points_col]]
    
    # Find multiple optimal lineups
    lineups <- list()
    used_combinations <- list()
    
    for(i in 1:k) {
      # Create constraint matrix
      const_rows <- 2 + length(used_combinations)
      const.mat <- matrix(0, nrow = const_rows, ncol = n_drivers)
      
      # Basic constraints
      const.mat[1, ] <- salaries  # Salary cap
      const.mat[2, ] <- 1         # Roster size
      
      # Exclusion constraints for previous lineups
      if(length(used_combinations) > 0) {
        for(j in 1:length(used_combinations)) {
          const.mat[2 + j, used_combinations[[j]]] <- 1
        }
      }
      
      const.dir <- c("<=", "==", rep("<=", length(used_combinations)))
      const.rhs <- c(salary_cap, roster_size, rep(roster_size - 1, length(used_combinations)))
      
      # Add small random noise to break ties
      objective <- points + runif(length(points), -0.01, 0.01)
      
      # Solve optimization
      result <- suppressWarnings(
        lp("max", objective, const.mat, const.dir, const.rhs, all.bin = TRUE)
      )
      
      if(result$status != 0) {
        break
      }
      
      # Get selected drivers
      selected_indices <- which(result$solution > 0.9)
      
      if(length(selected_indices) != roster_size) {
        break
      }
      
      # Create lineup
      lineup_drivers <- drivers[selected_indices]
      lineup_salary <- sum(salaries[selected_indices])
      lineup_points <- sum(points[selected_indices])
      
      # Store lineup
      lineup_data <- data.frame(
        Rank = i,
        TotalSalary = lineup_salary,
        ProjectedPoints = lineup_points,
        stringsAsFactors = FALSE
      )
      
      # Add driver columns
      for(d in 1:roster_size) {
        lineup_data[[paste0("Driver", d)]] <- lineup_drivers[d]
      }
      
      lineups[[i]] <- lineup_data
      used_combinations[[i]] <- selected_indices
    }
    
    if(length(lineups) == 0) {
      return(NULL)
    }
    
    return(do.call(rbind, lineups))
    
  }, error = function(e) {
    return(NULL)
  })
}

# Count lineup appearances across all simulations
count_lineup_appearances <- function(combined_lineups, roster_size) {
  
  # Create lineup signatures (sorted driver names for consistent matching)
  driver_cols <- paste0("Driver", 1:roster_size)
  
  combined_lineups$Lineup <- apply(combined_lineups[, driver_cols], 1, function(row) {
    paste(sort(row), collapse = "|")
  })
  
  # Count appearances by rank
  lineup_table <- table(combined_lineups$Lineup, combined_lineups$Rank)
  
  # Create result dataframe
  lineup_data <- data.frame(
    Lineup = rownames(lineup_table),
    stringsAsFactors = FALSE
  )
  
  # Add individual rank counts
  for(i in 1:5) {
    col_name <- paste0("Rank", i, "Count")
    lineup_data[[col_name]] <- if(as.character(i) %in% colnames(lineup_table)) {
      lineup_table[, as.character(i)]
    } else {
      0
    }
  }
  
  # Add cumulative counts
  lineup_data$Top1Count <- lineup_data$Rank1Count
  lineup_data$Top2Count <- lineup_data$Rank1Count + lineup_data$Rank2Count
  lineup_data$Top3Count <- lineup_data$Rank1Count + lineup_data$Rank2Count + lineup_data$Rank3Count
  lineup_data$Top5Count <- rowSums(lineup_data[, paste0("Rank", 1:5, "Count")])
  
  # Add salary information
  salary_lookup <- combined_lineups[!duplicated(combined_lineups$Lineup), c("Lineup", "TotalSalary")]
  lineup_data <- merge(lineup_data, salary_lookup, by = "Lineup")
  
  # Split driver columns back out
  driver_matrix <- do.call(rbind, strsplit(lineup_data$Lineup, "\\|"))
  colnames(driver_matrix) <- paste0("Driver", 1:roster_size)
  
  # Combine everything
  result <- cbind(
    as.data.frame(driver_matrix),
    lineup_data[, grep("Count$|Salary$", names(lineup_data), value = TRUE)]
  )
  
  # Sort by Top1Count descending, then Top5Count descending
  result <- result[order(-result$Top1Count, -result$Top5Count), ]
  
  return(result)
}

# Calculate frequency statistics
calculate_frequency_stats <- function(lineup_counts) {
  
  freq_stats <- data.frame(
    Metric = c("Total Unique Lineups", "Lineups with Rank 1 Appearances", 
               "Lineups with Top 3 Appearances", "Lineups with Top 5 Appearances",
               "Max Rank 1 Count", "Max Top 5 Count"),
    Value = c(
      nrow(lineup_counts),
      sum(lineup_counts$Top1Count > 0),
      sum(lineup_counts$Top3Count > 0),
      sum(lineup_counts$Top5Count > 0),
      max(lineup_counts$Top1Count),
      max(lineup_counts$Top5Count)
    )
  )
  
  return(freq_stats)
}

# Calculate driver usage statistics
calculate_driver_usage_stats <- function(lineup_counts, roster_size) {
  
  driver_cols <- paste0("Driver", 1:roster_size)
  all_drivers <- unlist(lineup_counts[, driver_cols])
  
  # Count total appearances
  driver_usage <- table(all_drivers)
  
  # Weight by Top1Count
  weighted_usage <- numeric(length(driver_usage))
  names(weighted_usage) <- names(driver_usage)
  
  for(i in 1:nrow(lineup_counts)) {
    lineup_drivers <- unlist(lineup_counts[i, driver_cols])
    weight <- lineup_counts$Top1Count[i]
    
    for(driver in lineup_drivers) {
      weighted_usage[driver] <- weighted_usage[driver] + weight
    }
  }
  
  usage_df <- data.frame(
    Driver = names(driver_usage),
    TotalAppearances = as.numeric(driver_usage),
    WeightedByTop1 = as.numeric(weighted_usage[names(driver_usage)]),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(WeightedByTop1))
  
  return(head(usage_df, 20))  # Top 20 drivers
}

# Calculate lineup popularity based on ownership projections
calculate_lineup_popularity <- function(lineups, driver_data, platform = "DK") {
  # Determine which columns to use based on platform
  if(platform == "DK") {
    driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
    ownership_col <- "DKOP"
    salary_col <- "DKSalary"
    name_col <- "DKName"
  } else {
    driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
    ownership_col <- "FDOP"
    salary_col <- "FDSalary" 
    name_col <- "FDName"
  }
  
  # Create a lookup table for driver ownership and salary
  driver_lookup <- driver_data[, c(name_col, ownership_col, salary_col)]
  names(driver_lookup) <- c("DriverID", "Ownership", "Salary")
  
  # Ensure ownership is in percentage format (0-100)
  if(max(driver_lookup$Ownership, na.rm = TRUE) <= 1) {
    driver_lookup$Ownership <- driver_lookup$Ownership * 100
  }
  
  # Calculate popularity for each lineup
  popularity_scores <- numeric(nrow(lineups))
  
  for(i in 1:nrow(lineups)) {
    lineup_drivers <- unlist(lineups[i, driver_cols])
    
    # Get ownership and salary for each driver in lineup
    lineup_ownership <- numeric(length(lineup_drivers))
    lineup_salaries <- numeric(length(lineup_drivers))
    
    for(j in seq_along(lineup_drivers)) {
      driver_match <- which(driver_lookup$DriverID == lineup_drivers[j])
      if(length(driver_match) > 0) {
        lineup_ownership[j] <- driver_lookup$Ownership[driver_match[1]]
        lineup_salaries[j] <- driver_lookup$Salary[driver_match[1]]
      }
    }
    
    # Calculate weighted average ownership (weight by salary)
    total_salary <- sum(lineup_salaries, na.rm = TRUE)
    if(total_salary > 0) {
      salary_weights <- lineup_salaries / total_salary
      popularity_scores[i] <- sum(lineup_ownership * salary_weights, na.rm = TRUE)
    } else {
      # Fallback to simple average if salary data missing
      popularity_scores[i] <- mean(lineup_ownership, na.rm = TRUE)
    }
  }
  
  return(popularity_scores)
}


# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; font-weight: bold;",
      "Golden Ticket Contest Simulator"
    ),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    useShinyjs(),
    div(
      style = "text-align: center; padding: 10px; margin-bottom: 5px;",
      tags$img(src = "logo.jpg", height = "200px", width = "auto", 
               style = "border: 2px solid #FFD700; border-radius: 10px;")
    ),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Input Data", tabName = "input_data", icon = icon("upload")),
      menuItem("Optimal Lineups", tabName = "optimal_lineups", icon = icon("trophy")),
      menuItem("Contest Simulation", tabName = "contest_sim", icon = icon("chart-line")),
      menuItem("Lineup Selection", tabName = "lineup_selection", icon = icon("check-square")),
      menuItem("Entry Export", tabName = "entry_export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      # Input Data Tab - Just file uploads, no box wrapper
      tabItem(tabName = "input_data",
              # Top row - All DraftKings
              fluidRow(
                column(4,
                       div(
                         style = "border: 2px solid #FFD700; border-radius: 10px; padding: 20px; margin: 10px; background-color: #1a1a1a;",
                         div(
                           style = "text-align: center; margin-bottom: 15px;",
                           h3("DK SimScores", style = "color: #FFD700; margin: 0;"),
                           p("Upload DraftKings simulation data", style = "color: #cccccc; font-size: 14px; margin: 5px 0;")
                         ),
                         fileInput("dk_sim_file", NULL,
                                   accept = c(".rds"),
                                   buttonLabel = "Choose DK SimScores",
                                   placeholder = "DK_SimScore.rds",
                                   width = "100%"),
                         div(id = "dk_sim_status", 
                             style = "text-align: center; margin-top: 10px; display: none;",
                             div(class = "alert alert-success", 
                                 style = "padding: 10px; margin: 0;",
                                 icon("check-circle"), " Loaded Successfully!"))
                       )
                ),
                
                column(4,
                       div(
                         style = "border: 2px solid #FFD700; border-radius: 10px; padding: 20px; margin: 10px; background-color: #1a1a1a;",
                         div(
                           style = "text-align: center; margin-bottom: 15px;",
                           h3("DK Entry", style = "color: #FFD700; margin: 0;"),
                           p("Upload DraftKings entry file", style = "color: #cccccc; font-size: 14px; margin: 5px 0;")
                         ),
                         fileInput("dk_entry_file", NULL,
                                   accept = c(".xlsx", ".csv"),
                                   buttonLabel = "Choose DK Entry File",
                                   placeholder = "DK_entry.xlsx",
                                   width = "100%"),
                         div(id = "dk_entry_status", 
                             style = "text-align: center; margin-top: 10px; display: none;",
                             div(class = "alert alert-success", 
                                 style = "padding: 10px; margin: 0;",
                                 icon("check-circle"), " Loaded Successfully!"))
                       )
                ),
                
                column(4,
                       div(
                         style = "border: 2px solid #FFD700; border-radius: 10px; padding: 20px; margin: 10px; background-color: #1a1a1a;",
                         div(
                           style = "text-align: center; margin-bottom: 15px;",
                           h3("DK Field", style = "color: #FFD700; margin: 0;"),
                           p("Upload DraftKings field lineups", style = "color: #cccccc; font-size: 14px; margin: 5px 0;")
                         ),
                         fileInput("dk_field_file", NULL,
                                   accept = c(".xlsx", ".csv"),
                                   buttonLabel = "Choose DK Field File",
                                   placeholder = "DK_field.xlsx",
                                   width = "100%"),
                         div(id = "dk_field_status", 
                             style = "text-align: center; margin-top: 10px; display: none;",
                             div(class = "alert alert-success", 
                                 style = "padding: 10px; margin: 0;",
                                 icon("check-circle"), " Loaded Successfully!"))
                       )
                )
              ),
              
              # Bottom row - All FanDuel
              fluidRow(
                column(4,
                       div(
                         style = "border: 2px solid #FFD700; border-radius: 10px; padding: 20px; margin: 10px; background-color: #1a1a1a;",
                         div(
                           style = "text-align: center; margin-bottom: 15px;",
                           h3("FD SimScores", style = "color: #FFD700; margin: 0;"),
                           p("Upload FanDuel simulation data", style = "color: #cccccc; font-size: 14px; margin: 5px 0;")
                         ),
                         fileInput("fd_sim_file", NULL,
                                   accept = c(".rds"),
                                   buttonLabel = "Choose FD SimScores",
                                   placeholder = "FD_SimScore.rds",
                                   width = "100%"),
                         div(id = "fd_sim_status", 
                             style = "text-align: center; margin-top: 10px; display: none;",
                             div(class = "alert alert-success", 
                                 style = "padding: 10px; margin: 0;",
                                 icon("check-circle"), " Loaded Successfully!"))
                       )
                ),
                
                column(4,
                       div(
                         style = "border: 2px solid #FFD700; border-radius: 10px; padding: 20px; margin: 10px; background-color: #1a1a1a;",
                         div(
                           style = "text-align: center; margin-bottom: 15px;",
                           h3("FD Entry", style = "color: #FFD700; margin: 0;"),
                           p("Upload FanDuel entry file", style = "color: #cccccc; font-size: 14px; margin: 5px 0;")
                         ),
                         fileInput("fd_entry_file", NULL,
                                   accept = c(".xlsx", ".csv"),
                                   buttonLabel = "Choose FD Entry File",
                                   placeholder = "FD_entry.xlsx",
                                   width = "100%"),
                         div(id = "fd_entry_status", 
                             style = "text-align: center; margin-top: 10px; display: none;",
                             div(class = "alert alert-success", 
                                 style = "padding: 10px; margin: 0;",
                                 icon("check-circle"), " Loaded Successfully!"))
                       )
                ),
                
                column(4,
                       div(
                         style = "border: 2px solid #FFD700; border-radius: 10px; padding: 20px; margin: 10px; background-color: #1a1a1a;",
                         div(
                           style = "text-align: center; margin-bottom: 15px;",
                           h3("FD Field", style = "color: #FFD700; margin: 0;"),
                           p("Upload FanDuel field lineups", style = "color: #cccccc; font-size: 14px; margin: 5px 0;")
                         ),
                         fileInput("fd_field_file", NULL,
                                   accept = c(".xlsx", ".csv"),
                                   buttonLabel = "Choose FD Field File",
                                   placeholder = "FD_field.xlsx",
                                   width = "100%"),
                         div(id = "fd_field_status", 
                             style = "text-align: center; margin-top: 10px; display: none;",
                             div(class = "alert alert-success", 
                                 style = "padding: 10px; margin: 0;",
                                 icon("check-circle"), " Loaded Successfully!"))
                       )
                )
              )
      ),
      
      # Optimal Lineups Tab - Now includes calculation buttons and all tables
      tabItem(tabName = "optimal_lineups",
              # Platform Detection and Calculation Buttons
              fluidRow(
                box(
                  width = 12,
                  title = "Optimal Lineup Generation",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  fluidRow(
                    column(4,
                           h4("Data Summary", style = "color: #FFD700;"),
                           div(id = "optimization_summary", 
                               style = "background-color: #2a2a2a; padding: 15px; border-radius: 5px; border: 1px solid #FFD700;",
                               verbatimTextOutput("optimization_data_summary")
                           )
                    ),
                    column(4,
                           h4("Available Platforms", style = "color: #FFD700;"),
                           div(
                             style = "background-color: #2a2a2a; padding: 15px; border-radius: 5px; border: 1px solid #FFD700;",
                             textOutput("available_platforms_text")
                           )
                    ),
                    column(4,
                           h4("Actions", style = "color: #FFD700;"),
                           div(
                             style = "text-align: center; padding: 10px;",
                             conditionalPanel(
                               condition = "output.has_dk_data == 'true'",
                               actionButton("calculate_dk_optimal_lineups", 
                                            div(
                                              icon("trophy", style = "margin-right: 8px;"),
                                              "Calculate DK Optimal Lineups"
                                            ),
                                            class = "btn-primary",
                                            style = "width: 100%; margin-bottom: 10px; font-weight: bold;")
                             ),
                             conditionalPanel(
                               condition = "output.has_fd_data == 'true'",
                               actionButton("calculate_fd_optimal_lineups", 
                                            div(
                                              icon("trophy", style = "margin-right: 8px;"),
                                              "Calculate FD Optimal Lineups"
                                            ),
                                            class = "btn-primary",
                                            style = "width: 100%; font-weight: bold;")
                             ),
                             conditionalPanel(
                               condition = "output.has_dk_data != 'true' && output.has_fd_data != 'true'",
                               div(class = "alert alert-warning",
                                   style = "margin: 0; padding: 10px; font-size: 12px;",
                                   "Upload simulation data in the Input Data tab first.")
                             )
                           )
                    )
                  )
                )
              ),
              
              # Progress Indicator
              conditionalPanel(
                condition = "output.optimization_running == 'true'",
                fluidRow(
                  box(
                    width = 12,
                    status = "warning",
                    div(
                      style = "text-align: center; padding: 20px;",
                      h4("Processing Simulation Results...", style = "color: #FFD700;"),
                      div(class = "progress progress-striped active",
                          div(class = "progress-bar progress-bar-warning", 
                              style = "width: 100%; background-color: #FFD700;")
                      ),
                      p(textOutput("optimization_progress_text"), style = "margin-top: 10px;")
                    )
                  )
                )
              ),
              
              # DraftKings Results Section
              conditionalPanel(
                condition = "output.has_dk_optimal_lineups == 'true'",
                fluidRow(
                  box(
                    width = 12,
                    title = "DraftKings Optimal Lineups Results",
                    status = "primary",
                    solidHeader = TRUE,
                    
                    fluidRow(
                      column(8,
                             h4(textOutput("dk_lineup_summary_text"), style = "color: #FFD700; margin-top: 0;")
                      ),
                      column(4,
                             div(style = "text-align: right;",
                                 downloadButton('download_dk_optimal_lineups', 'Download DK Lineups',
                                                class = "btn-default",
                                                style = "margin-top: 5px;")
                             )
                      )
                    ),
                    
                    DTOutput("dk_optimal_lineups_table") %>% withSpinner(color = "#FFD700")
                  )
                ),
                
                # DraftKings Statistics
                fluidRow(
                  column(6,
                         box(
                           width = NULL,
                           title = "DK Lineup Frequency Analysis",
                           status = "primary",
                           solidHeader = TRUE,
                           DTOutput("dk_lineup_frequency_stats") %>% withSpinner(color = "#FFD700")
                         )
                  ),
                  column(6,
                         box(
                           width = NULL,
                           title = "DK Driver Usage Analysis",
                           status = "primary",
                           solidHeader = TRUE,
                           DTOutput("dk_driver_usage_stats") %>% withSpinner(color = "#FFD700")
                         )
                  )
                ),
                
                # DraftKings Visualization
                fluidRow(
                  box(
                    width = 12,
                    title = "DK Top Lineups Distribution",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("dk_lineup_distribution_plot", height = "500px") %>% withSpinner(color = "#FFD700")
                  )
                )
              ),
              
              # FanDuel Results Section
              conditionalPanel(
                condition = "output.has_fd_optimal_lineups == 'true'",
                fluidRow(
                  box(
                    width = 12,
                    title = "FanDuel Optimal Lineups Results",
                    status = "primary",
                    solidHeader = TRUE,
                    
                    fluidRow(
                      column(8,
                             h4(textOutput("fd_lineup_summary_text"), style = "color: #FFD700; margin-top: 0;")
                      ),
                      column(4,
                             div(style = "text-align: right;",
                                 downloadButton('download_fd_optimal_lineups', 'Download FD Lineups',
                                                class = "btn-default",
                                                style = "margin-top: 5px;")
                             )
                      )
                    ),
                    
                    DTOutput("fd_optimal_lineups_table") %>% withSpinner(color = "#FFD700")
                  )
                ),
                
                # FanDuel Statistics
                fluidRow(
                  column(6,
                         box(
                           width = NULL,
                           title = "FD Lineup Frequency Analysis",
                           status = "primary",
                           solidHeader = TRUE,
                           DTOutput("fd_lineup_frequency_stats") %>% withSpinner(color = "#FFD700")
                         )
                  ),
                  column(6,
                         box(
                           width = NULL,
                           title = "FD Driver Usage Analysis",
                           status = "primary",
                           solidHeader = TRUE,
                           DTOutput("fd_driver_usage_stats") %>% withSpinner(color = "#FFD700")
                         )
                  )
                ),
                
                # FanDuel Visualization
                fluidRow(
                  box(
                    width = 12,
                    title = "FD Top Lineups Distribution",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("fd_lineup_distribution_plot", height = "500px") %>% withSpinner(color = "#FFD700")
                  )
                )
              )
      ),
      
      
      # Contest Simulation Tab
      tabItem(tabName = "contest_sim",
              h2("Contest Simulation Tab - Coming Next")
      ),
      
      # Lineup Selection Tab
      tabItem(tabName = "lineup_selection",
              h2("Lineup Selection Tab - Coming Next")
      ),
      
      # Entry Export Tab
      tabItem(tabName = "entry_export",
              h2("Entry Export Tab - Coming Next")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store data
  rv <- reactiveValues(
      fd_sim_data = NULL,
      dk_entry_data = NULL,
      fd_entry_data = NULL,
    dk_field_lineups = NULL,
    fd_field_lineups = NULL,
    dk_optimal_lineups = NULL,
    fd_optimal_lineups = NULL,
    dk_lineup_frequency_stats = NULL,
    fd_lineup_frequency_stats = NULL,
    dk_driver_usage_stats = NULL,
    fd_driver_usage_stats = NULL,
    optimization_running = FALSE,
    optimization_progress = "",
    data_loaded = list(dk_sim = FALSE, fd_sim = FALSE, dk_entry = FALSE, fd_entry = FALSE, dk_field = FALSE, fd_field = FALSE)
  )
  
  observeEvent(input$dk_sim_file, {
    req(input$dk_sim_file)
    
    tryCatch({
      loaded_data <- readRDS(input$dk_sim_file$datapath)
      
      if(!is.list(loaded_data) || !"simulation_results" %in% names(loaded_data)) {
        stop("Invalid simulation data file.")
      }
      
      rv$dk_sim_data <- loaded_data
      rv$data_loaded$dk_sim <- TRUE
      
      shinyjs::show("dk_sim_status")
      showNotification("DK SimScores loaded successfully!", type = "message")
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error Loading DK SimScores",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
      rv$data_loaded$dk_sim <- FALSE
    })
  })
  
  # FD SimScores file upload
  observeEvent(input$fd_sim_file, {
    req(input$fd_sim_file)
    
    tryCatch({
      loaded_data <- readRDS(input$fd_sim_file$datapath)
      
      if(!is.list(loaded_data) || !"simulation_results" %in% names(loaded_data)) {
        stop("Invalid simulation data file.")
      }
      
      rv$fd_sim_data <- loaded_data
      rv$data_loaded$fd_sim <- TRUE
      
      shinyjs::show("fd_sim_status")
      showNotification("FD SimScores loaded successfully!", type = "message")
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error Loading FD SimScores",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
      rv$data_loaded$fd_sim <- FALSE
    })
  })
  
  # DK Entry file upload
  observeEvent(input$dk_entry_file, {
    req(input$dk_entry_file)
    
    tryCatch({
      ext <- tools::file_ext(input$dk_entry_file$datapath)
      
      if (ext == "xlsx") {
        dk_data <- read_excel(input$dk_entry_file$datapath)
      } else if (ext == "csv") {
        dk_data <- read.csv(input$dk_entry_file$datapath, stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file format. Please use .xlsx or .csv")
      }
      
      rv$dk_entry_data <- as.data.table(dk_data)
      rv$data_loaded$dk_entry <- TRUE
      
      shinyjs::show("dk_entry_status")
      showNotification(paste("Loaded", nrow(dk_data), "DK entries!"), type = "message")
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error Loading DK Entry File",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
      rv$data_loaded$dk_entry <- FALSE
    })
  })
  
  # FD Entry file upload
  observeEvent(input$fd_entry_file, {
    req(input$fd_entry_file)
    
    tryCatch({
      ext <- tools::file_ext(input$fd_entry_file$datapath)
      
      if (ext == "xlsx") {
        fd_data <- read_excel(input$fd_entry_file$datapath)
      } else if (ext == "csv") {
        fd_data <- read.csv(input$fd_entry_file$datapath, stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file format. Please use .xlsx or .csv")
      }
      
      rv$fd_entry_data <- as.data.table(fd_data)
      rv$data_loaded$fd_entry <- TRUE
      
      shinyjs::show("fd_entry_status")
      showNotification(paste("Loaded", nrow(fd_data), "FD entries!"), type = "message")
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error Loading FD Entry File",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
      rv$data_loaded$fd_entry <- FALSE
    })
  })
  
  # DK Field lineups file upload
  observeEvent(input$dk_field_file, {
    req(input$dk_field_file)
    
    tryCatch({
      ext <- tools::file_ext(input$dk_field_file$datapath)
      
      if (ext == "xlsx") {
        field_data <- read_excel(input$dk_field_file$datapath)
      } else if (ext == "csv") {
        field_data <- read.csv(input$dk_field_file$datapath, stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file format. Please use .xlsx or .csv")
      }
      
      # Basic validation - look for driver columns
      driver_cols <- grep("Driver[1-6]", names(field_data), value = TRUE)
      if (length(driver_cols) < 6) {
        stop("DK field lineup file must have 6 driver columns (Driver1-Driver6).")
      }
      
      rv$dk_field_lineups <- as.data.table(field_data)
      rv$data_loaded$dk_field <- TRUE
      
      shinyjs::show("field_status")
      showNotification(paste("Loaded", nrow(field_data), "DK field lineups!"), type = "message")
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error Loading DK Field Lineups",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
      rv$data_loaded$dk_field <- FALSE
    })
  })
  
  # FD Field lineups file upload
  observeEvent(input$fd_field_file, {
    req(input$fd_field_file)
    
    tryCatch({
      ext <- tools::file_ext(input$fd_field_file$datapath)
      
      if (ext == "xlsx") {
        field_data <- read_excel(input$fd_field_file$datapath)
      } else if (ext == "csv") {
        field_data <- read.csv(input$fd_field_file$datapath, stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file format. Please use .xlsx or .csv")
      }
      
      # Basic validation - look for driver columns
      driver_cols <- grep("Driver[1-5]", names(field_data), value = TRUE)
      if (length(driver_cols) < 5) {
        stop("FD field lineup file must have 5 driver columns (Driver1-Driver5).")
      }
      
      rv$fd_field_lineups <- as.data.table(field_data)
      rv$data_loaded$fd_field <- TRUE
      
      shinyjs::show("field_status")
      showNotification(paste("Loaded", nrow(field_data), "FD field lineups!"), type = "message")
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error Loading FD Field Lineups",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
      rv$data_loaded$fd_field <- FALSE
    })
  })
  

  

  
  # Lineup summary text
  output$lineup_summary_text <- renderText({
    if (!is.null(rv$optimal_lineups)) {
      paste("Found", formatC(nrow(rv$optimal_lineups), format = "d", big.mark = ","), 
            "unique optimal lineups across all simulations")
    } else {
      ""
    }
  })
  
  # Optimal lineups table
  output$optimal_lineups_table <- renderDT({
    req(rv$optimal_lineups)
    
    display_data <- rv$optimal_lineups
    
    # Format the table
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(which(names(display_data) == "Top1Count") - 1, 'desc')),
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Format salary column
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", digits = 0)
    }
    
    # Add color bars for count columns
    count_cols <- grep("Count$", names(display_data), value = TRUE)
    for(col in count_cols) {
      if(max(display_data[[col]], na.rm = TRUE) > 0) {
        dt <- dt %>% formatStyle(
          col,
          background = styleColorBar(c(0, max(display_data[[col]], na.rm = TRUE)), '#FFD700'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      }
    }
    
    dt
  })
  
  # Frequency stats table
  output$lineup_frequency_stats <- renderDT({
    req(rv$lineup_frequency_stats)
    
    datatable(
      rv$lineup_frequency_stats,
      options = list(
        pageLength = -1,
        dom = "t",
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })

  output$dk_optimal_lineups_table <- renderDT({
    req(rv$dk_optimal_lineups)
    
    # Clone lineups for display
    display_data <- as.data.table(rv$dk_optimal_lineups)
    
    # Calculate popularity scores if we have driver data
    if(!is.null(rv$dk_driver_exposure)) {
      popularity_scores <- calculate_lineup_popularity(display_data, rv$dk_driver_exposure, "DK")
      display_data[, Popularity := round(popularity_scores, 1)]
    }
    
    # Format driver columns to show names (existing code)
    if(!is.null(rv$dk_fantasy_analysis)) {
      for(i in 1:DK_ROSTER_SIZE) {
        col <- paste0("Driver", i)
        display_data[[col]] <- sapply(display_data[[col]], function(id) {
          match_idx <- which(rv$dk_fantasy_analysis$DKName == id)
          if(length(match_idx) > 0) {
            rv$dk_fantasy_analysis$Name[match_idx[1]]
          } else {
            id
          }
        })
      }
    }
    
    # Remove Rank columns, keep TopX Count columns and add Popularity
    cols_to_keep <- c(paste0("Driver", 1:DK_ROSTER_SIZE), 
                      grep("^Top[0-9]+Count$", names(display_data), value = TRUE),
                      "TotalSalary")
    
    # Add Popularity if it exists
    if("Popularity" %in% names(display_data)) {
      cols_to_keep <- c(cols_to_keep, "Popularity")
    }
    
    cols_to_keep <- intersect(cols_to_keep, names(display_data))
    display_data <- display_data[, ..cols_to_keep]
    
    # Sort the data by Top1Count, then Top5Count (both descending)
    if("Top1Count" %in% names(display_data) && "Top5Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count, -Top5Count)
    } else if("Top1Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count)
    }
    
    # Create the datatable
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "ftp",
        ordering = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Apply formatting to TotalSalary column
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    # Apply formatting to count columns
    count_cols <- grep("^Top[0-9]+Count$", names(display_data), value = TRUE)
    for(col in count_cols) {
      if(any(!is.na(display_data[[col]]))) {
        max_count <- max(display_data[[col]], na.rm = TRUE)
        if(is.finite(max_count) && max_count > 0) {
          dt <- dt %>% formatStyle(
            col,
            background = styleColorBar(c(0, max_count), 'lightblue'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
        }
      }
    }
    
    # Format Popularity column with color coding
    if("Popularity" %in% names(display_data)) {
      dt <- dt %>% formatStyle(
        'Popularity',
        background = styleColorBar(range(display_data$Popularity, na.rm = TRUE), 'lightcoral'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    }
    
    dt
  })
  
  # Driver usage stats table
  output$driver_usage_stats <- renderDT({
    req(rv$driver_usage_stats)
    
    datatable(
      rv$driver_usage_stats,
      options = list(
        pageLength = -1,
        dom = "t",
        order = list(list(2, 'desc'))
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'WeightedByTop1',
        background = styleColorBar(c(0, max(rv$driver_usage_stats$WeightedByTop1)), '#FFD700'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Lineup distribution plot
  output$lineup_distribution_plot <- renderPlotly({
    req(rv$optimal_lineups)
    
    # Get top 20 lineups by Top1Count
    top_lineups <- head(rv$optimal_lineups[order(-rv$optimal_lineups$Top1Count), ], 20)
    top_lineups$LineupID <- paste("Lineup", 1:nrow(top_lineups))
    
    plot_data <- top_lineups %>%
      select(LineupID, Rank1Count, Rank2Count, Rank3Count, Rank4Count, Rank5Count) %>%
      pivot_longer(cols = -LineupID, names_to = "Rank", values_to = "Count") %>%
      mutate(
        Rank = factor(Rank, levels = paste0("Rank", 1:5, "Count")),
        RankNum = as.numeric(gsub("Rank|Count", "", Rank))
      )
    
    p <- ggplot(plot_data, aes(x = reorder(LineupID, -Count), y = Count, fill = Rank)) +
      geom_col() +
      scale_fill_manual(values = c("#FFD700", "#E6C200", "#CCAD00", "#B39900", "#998600")) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Top Lineups", y = "Appearance Count", title = "Top 20 Lineups by Rank Distribution") +
      theme(
        plot.title = element_text(color = "#FFD700"),
        axis.title = element_text(color = "#FFD700"),
        axis.text = element_text(color = "#666666"),
        legend.title = element_text(color = "#FFD700"),
        legend.text = element_text(color = "#666666")
      )
    
    ggplotly(p)
  })
  
  # Download handler
  output$download_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rv$optimal_lineups, file, row.names = FALSE)
    }
  )
  
  # Track optimization status
  output$optimization_running <- reactive({
    return(tolower(as.character(rv$optimization_running)))
  })
  outputOptions(output, "optimization_running", suspendWhenHidden = FALSE)
  
  # Calculate optimal lineups using the simulation-based ranking method
  observeEvent(input$calculate_optimal_lineups, {
    req(rv$sim_data, rv$data_loaded$sim)
    
    if (!rv$data_loaded$sim) {
      showModal(modalDialog(
        title = "Missing Data",
        "Please load simulation data first in the Input Data tab.",
        easyClose = TRUE
      ))
      return()
    }
    
    rv$optimization_running <- TRUE
    
    withProgress(message = 'Processing simulation results...', value = 0, {
      tryCatch({
        
        # Extract simulation results
        sim_results <- rv$sim_data$simulation_results
        platform <- rv$platform
        n_sims <- rv$sim_data$n_sims
        
        incProgress(0.1, detail = "Setting up optimization parameters...")
        
        # Determine columns based on platform
        if (platform == "DraftKings") {
          salary_col <- "DKSalary"
          points_col <- "DKFantasyPoints"
          name_col <- "DKName"
          roster_size <- DK_ROSTER_SIZE
          salary_cap <- DK_SALARY_CAP
        } else {
          salary_col <- "FDSalary"
          points_col <- "FDFantasyPoints"
          name_col <- "FDName"
          roster_size <- FD_ROSTER_SIZE
          salary_cap <- FD_SALARY_CAP
        }
        
        # Convert to data.table for better performance
        setDT(sim_results)
        
        incProgress(0.2, detail = "Finding optimal lineups for each simulation...")
        
        # Get unique simulation IDs
        sim_ids <- unique(sim_results$SimID)
        total_sims <- length(sim_ids)
        
        # Store all optimal lineups
        all_optimal_lineups <- list()
        
        # Process simulations in chunks for memory efficiency
        chunk_size <- 100
        chunks <- ceiling(total_sims / chunk_size)
        
        for(chunk in 1:chunks) {
          start_idx <- (chunk - 1) * chunk_size + 1
          end_idx <- min(chunk * chunk_size, total_sims)
          chunk_sim_ids <- sim_ids[start_idx:end_idx]
          
          incProgress(0.6 / chunks, detail = paste("Processing chunk", chunk, "of", chunks))
          
          for(sim_id in chunk_sim_ids) {
            # Get data for this simulation
            sim_data <- sim_results[SimID == sim_id]
            
            # Find top 5 lineups for this simulation
            top_lineups <- find_top_lineups_for_sim(sim_data, salary_col, points_col, 
                                                    name_col, roster_size, salary_cap, k = 5)
            
            if(!is.null(top_lineups) && nrow(top_lineups) > 0) {
              top_lineups$SimID <- sim_id
              all_optimal_lineups[[length(all_optimal_lineups) + 1]] <- top_lineups
            }
          }
          
          # Garbage collection every chunk
          gc(verbose = FALSE)
        }
        
        incProgress(0.1, detail = "Combining results...")
        
        if(length(all_optimal_lineups) == 0) {
          stop("No optimal lineups could be generated from the simulation results.")
        }
        
        # Combine all lineups
        combined_lineups <- do.call(rbind, all_optimal_lineups)
        
        incProgress(0.05, detail = "Counting lineup frequencies...")
        
        # Count lineup appearances by rank
        lineup_counts <- count_lineup_appearances(combined_lineups, roster_size)
        
        incProgress(0.025, detail = "Calculating statistics...")
        
        # Calculate frequency and usage statistics
        rv$lineup_frequency_stats <- calculate_frequency_stats(lineup_counts)
        rv$driver_usage_stats <- calculate_driver_usage_stats(lineup_counts, roster_size)
        
        # Store the final results
        rv$optimal_lineups <- lineup_counts
        rv$optimization_running <- FALSE
        
        incProgress(0.025, detail = "Complete!")
        
        showNotification(
          paste("Successfully processed", total_sims, "simulations and found", 
                nrow(rv$optimal_lineups), "unique optimal lineups!"),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        rv$optimization_running <- FALSE
        showModal(modalDialog(
          title = "Optimization Error",
          paste("Error processing simulations:", e$message),
          easyClose = TRUE
        ))
      })
    })
  })
  

  
  # Run optimization button (will redirect to optimal lineups tab)
  observeEvent(input$run_optimization, {
    updateTabItems(session, "sidebar_menu", selected = "optimal_lineups")
  })
  
  output$upload_summary_text <- renderText({
    summary_parts <- c()
    
    if (rv$data_loaded$dk_sim) {
      summary_parts <- c(summary_parts, "✓ DK SimScores: Loaded")
    } else {
      summary_parts <- c(summary_parts, "○ DK SimScores: Not loaded")
    }
    
    if (rv$data_loaded$fd_sim) {
      summary_parts <- c(summary_parts, "✓ FD SimScores: Loaded")
    } else {
      summary_parts <- c(summary_parts, "○ FD SimScores: Not loaded")
    }
    
    if (rv$data_loaded$dk_entry) {
      summary_parts <- c(summary_parts, paste("✓ DK Entry:", nrow(rv$dk_entry_data), "entries"))
    } else {
      summary_parts <- c(summary_parts, "○ DK Entry: Not loaded")
    }
    
    if (rv$data_loaded$fd_entry) {
      summary_parts <- c(summary_parts, paste("✓ FD Entry:", nrow(rv$fd_entry_data), "entries"))
    } else {
      summary_parts <- c(summary_parts, "○ FD Entry: Not loaded")
    }
    
    if (rv$data_loaded$dk_field) {
      summary_parts <- c(summary_parts, paste("✓ DK Field:", nrow(rv$dk_field_lineups), "lineups"))
    } else {
      summary_parts <- c(summary_parts, "○ DK Field: Not loaded")
    }
    
    if (rv$data_loaded$fd_field) {
      summary_parts <- c(summary_parts, paste("✓ FD Field:", nrow(rv$fd_field_lineups), "lineups"))
    } else {
      summary_parts <- c(summary_parts, "○ FD Field: Not loaded")
    }
    
    paste(summary_parts, collapse = "\n")
  })
  
  # Proceed to optimal lineups button
  observeEvent(input$proceed_to_optimal, {
    updateTabItems(session, "sidebar_menu", selected = "optimal_lineups")
  })
  
  # Platform availability checks
  output$has_dk_data <- reactive({
    return(tolower(as.character(rv$data_loaded$dk_sim)))
  })
  outputOptions(output, "has_dk_data", suspendWhenHidden = FALSE)
  
  output$has_fd_data <- reactive({
    return(tolower(as.character(rv$data_loaded$fd_sim)))
  })
  outputOptions(output, "has_fd_data", suspendWhenHidden = FALSE)
  
  # Available platforms text
  output$available_platforms_text <- renderText({
    platforms <- c()
    
    if (rv$data_loaded$dk_sim) {
      platforms <- c(platforms, "DraftKings simulation data available")
    }
    
    if (rv$data_loaded$fd_sim) {
      platforms <- c(platforms, "FanDuel simulation data available")
    }
    
    if (length(platforms) == 0) {
      return("No simulation data loaded")
    } else {
      return(paste(platforms, collapse = "\n"))
    }
  })
  
  # Optimization data summary
  output$optimization_data_summary <- renderText({
    summary_parts <- c()
    
    if (rv$data_loaded$dk_sim) {
      dk_sim_results <- rv$dk_sim_data$simulation_results
      summary_parts <- c(summary_parts, paste0(
        "DraftKings:\n",
        "- Simulations: ", formatC(rv$dk_sim_data$n_sims, format = "d", big.mark = ","), "\n",
        "- Drivers: ", length(unique(dk_sim_results$Name))
      ))
    }
    
    if (rv$data_loaded$fd_sim) {
      fd_sim_results <- rv$fd_sim_data$simulation_results
      summary_parts <- c(summary_parts, paste0(
        "FanDuel:\n",
        "- Simulations: ", formatC(rv$fd_sim_data$n_sims, format = "d", big.mark = ","), "\n",
        "- Drivers: ", length(unique(fd_sim_results$Name))
      ))
    }
    
    if (length(summary_parts) == 0) {
      return("No simulation data available")
    } else {
      return(paste(summary_parts, collapse = "\n\n"))
    }
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)