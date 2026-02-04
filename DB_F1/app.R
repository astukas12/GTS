# F1 Database Review App
# Pattern matching NASCAR app structure
# Analyze historical results and build sim inputs

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(openxlsx)
library(plotly)
library(ggplot2)
library(tidyr)
library(lubridate)

#--------------------- Helper Functions ---------------------#

# Load F1 database
load_f1_database <- function(file_path = "F1_Database.xlsx") {
  if (file.exists(file_path)) {
    data <- read_excel(file_path, sheet = "Sheet 1")
    return(data)
  } else {
    return(NULL)
  }
}

# Get unique circuits from database
get_circuits <- function(data) {
  if (!is.null(data)) {
    circuits <- data %>%
      distinct(circuit_id) %>%
      arrange(circuit_id)
    return(circuits)
  }
  return(NULL)
}

# Calculate laps led probability by starting position
calc_laps_led_prob <- function(data, by = "grid") {
  data %>%
    group_by(!!sym(by)) %>%
    summarise(
      avg_laps_led = mean(laps_led, na.rm = TRUE),
      median_laps_led = median(laps_led, na.rm = TRUE),
      max_laps_led = max(laps_led, na.rm = TRUE),
      pct_led_laps = mean(laps_led > 0, na.rm = TRUE) * 100,
      races = n(),
      .groups = "drop"
    ) %>%
    arrange(!!sym(by))
}

#--------------------- UI Definition ---------------------#

ui <- fluidPage(
  useShinyjs(),
  
  # Custom CSS - Black and Gold Theme
  tags$head(
    tags$style(HTML("
      /* Global styles */
      body {
        background-color: #1a1a1a !important;
        color: #ffffff !important;
        font-family: 'Helvetica Neue', Arial, sans-serif;
      }
      
      /* Header */
      .app-header {
        background-color: #000000;
        padding: 15px 30px;
        border-bottom: 3px solid #FFD700;
        display: flex;
        align-items: center;
        margin-bottom: 0;
      }
      
      .app-title {
        color: #FFD700;
        font-size: 24px;
        font-weight: bold;
        margin: 0;
      }
      
      /* Horizontal tabs */
      .navbar-default {
        background-color: #000000 !important;
        border: none !important;
        border-bottom: 4px solid #FFD700 !important;
        border-radius: 0 !important;
        margin-bottom: 0 !important;
        min-height: 60px !important;
      }
      
      .navbar-default .navbar-nav > li > a {
        color: #FFD700 !important;
        background-color: #000000 !important;
        padding: 20px 30px !important;
        font-weight: 700 !important;
        font-size: 16px !important;
        letter-spacing: 0.5px !important;
        text-transform: uppercase !important;
        transition: all 0.3s ease !important;
        border-right: 1px solid #333333 !important;
      }
      
      .navbar-default .navbar-nav > li:last-child > a {
        border-right: none !important;
      }
      
      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > li > a:focus {
        background-color: #1a1a1a !important;
        color: #FFD700 !important;
      }
      
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: #FFD700 !important;
        color: #000000 !important;
        font-weight: 900 !important;
        box-shadow: 0 4px 8px rgba(255, 215, 0, 0.3) !important;
      }
      
      .navbar-nav {
        margin: 0 !important;
      }
      
      /* Main content */
      .container-fluid {
        padding: 20px 30px;
      }
      
      /* Boxes */
      .box {
        background-color: #2d2d2d !important;
        border: 1px solid #444444 !important;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,.3);
        margin-bottom: 20px;
      }
      
      .box-header {
        background-color: #1a1a1a !important;
        padding: 15px 20px !important;
        border-bottom: 2px solid #FFD700 !important;
        border-radius: 5px 5px 0 0 !important;
      }
      
      .box-title {
        color: #FFD700 !important;
        font-size: 20px !important;
        font-weight: bold !important;
        margin: 0 !important;
      }
      
      .box-body {
        padding: 20px !important;
      }
      
      /* Buttons */
      .btn-primary {
        background-color: #FFD700 !important;
        border-color: #FFD700 !important;
        color: #000000 !important;
        font-weight: bold !important;
      }
      
      .btn-primary:hover {
        background-color: #FFC700 !important;
        border-color: #FFC700 !important;
      }
      
      .btn-success {
        background-color: #FFD700 !important;
        border-color: #FFD700 !important;
        color: #000000 !important;
        font-weight: bold !important;
      }
      
      .btn-success:hover {
        background-color: #FFC700 !important;
        border-color: #FFC700 !important;
      }
      
      /* Form controls */
      .form-control, .selectize-input {
        background-color: #404040 !important;
        border: 1px solid #666666 !important;
        color: #ffffff !important;
      }
      
      .form-control:focus, .selectize-input.focus {
        border-color: #FFD700 !important;
        box-shadow: 0 0 5px rgba(255, 215, 0, 0.5) !important;
      }
      
      .selectize-dropdown {
        background-color: #404040 !important;
        border: 1px solid #666666 !important;
      }
      
      .selectize-dropdown-content .option {
        color: #ffffff !important;
      }
      
      .selectize-dropdown-content .option.active {
        background-color: #FFD700 !important;
        color: #000000 !important;
      }
      
      /* DataTables */
      .dataTables_wrapper {
        color: #ffffff !important;
      }
      
      table.dataTable {
        background-color: #2d2d2d !important;
        color: #ffffff !important;
      }
      
      table.dataTable thead th {
        background-color: #1a1a1a !important;
        color: #FFD700 !important;
        border-bottom: 2px solid #FFD700 !important;
        font-weight: bold !important;
      }
      
      table.dataTable tbody tr {
        background-color: #2d2d2d !important;
      }
      
      table.dataTable tbody tr:hover {
        background-color: #3d3d3d !important;
      }
      
      table.dataTable tbody td {
        border-color: #444444 !important;
      }
      
      .dataTables_info, .dataTables_paginate {
        color: #FFD700 !important;
      }
      
      .paginate_button {
        background-color: #404040 !important;
        color: #FFD700 !important;
        border: 1px solid #666666 !important;
      }
      
      .paginate_button:hover {
        background-color: #FFD700 !important;
        color: #000000 !important;
      }
      
      .paginate_button.current {
        background-color: #FFD700 !important;
        color: #000000 !important;
      }
      
      /* Info box */
      .info-box {
        background-color: #1a1a1a;
        border-left: 4px solid #FFD700;
        padding: 15px;
        margin-bottom: 15px;
        border-radius: 3px;
      }
      
      /* Radio buttons */
      .radio label, .checkbox label {
        color: #ffffff !important;
      }
      
      .radio input[type='radio']:checked + span::before {
        background-color: #FFD700 !important;
        border-color: #FFD700 !important;
      }
      
      /* Slider */
      .irs--shiny .irs-bar {
        background: #FFD700 !important;
        border-top: 1px solid #FFD700 !important;
        border-bottom: 1px solid #FFD700 !important;
      }
      
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background: #FFD700 !important;
        color: #000000 !important;
      }
      
      .irs--shiny .irs-handle {
        background: #FFD700 !important;
        border: 2px solid #DAA520 !important;
      }
      
      .irs--shiny .irs-min, .irs--shiny .irs-max {
        color: #FFD700 !important;
        background: #333333 !important;
      }
      
      .irs--shiny .irs-line {
        background: #555555 !important;
      }
    "))
  ),
  
  # Header
  div(class = "app-header",
      h1("F1 Database & Sim Builder", class = "app-title")
  ),
  
  # Navigation Tabs
  navbarPage(
    title = NULL,
    id = "main_tabs",
    windowTitle = "F1 Database",
    
    # Historical Results Tab
    tabPanel(
      "Historical Results",
      value = "historical",
      
      # Filters
      fluidRow(
        column(12,
               div(class = "box",
                   div(class = "box-header",
                       h3("Filter Data", class = "box-title")
                   ),
                   div(class = "box-body",
                       fluidRow(
                         column(3,
                                selectizeInput("hist_circuit", "Circuit:",
                                               choices = NULL,
                                               multiple = TRUE,
                                               options = list(placeholder = "Select Circuit(s)"))
                         ),
                         column(2,
                                numericInput("hist_start_year", "Start Year:", 
                                             value = 2019, min = 2007, max = 2025, step = 1)
                         ),
                         column(2,
                                numericInput("hist_end_year", "End Year:", 
                                             value = 2024, min = 2007, max = 2025, step = 1)
                         ),
                         column(2,
                                div(style = "margin-top: 25px;",
                                    actionButton("hist_load", "Load Data", 
                                                 class = "btn-primary", 
                                                 style = "width: 100%;"))
                         )
                       )
                   )
               )
        )
      ),
      
      conditionalPanel(
        condition = "output.hist_loaded",
        
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header", style = "display: flex; justify-content: space-between; align-items: center;",
                         h3("Race Results", class = "box-title", style = "margin: 0;"),
                         downloadButton("download_results", 
                                        "Download CSV", 
                                        class = "btn-success",
                                        style = "margin: 0;")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("results_table"))
                           )
                         )
                     )
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "!output.hist_loaded",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("No Data Loaded", class = "box-title")
                     ),
                     div(class = "box-body",
                         p(style = "color: #ffffff; text-align: center; padding: 40px;",
                           "Select filters above and click Load Data.")
                     )
                 )
          )
        )
      )
    ),
    
    # Fastest Laps Tab
    tabPanel(
      "Fastest Laps",
      value = "fastest_laps",
      
      # Filters
      fluidRow(
        column(12,
               div(class = "box",
                   div(class = "box-header",
                       h3("Filter Data", class = "box-title")
                   ),
                   div(class = "box-body",
                       fluidRow(
                         column(3,
                                selectizeInput("fl_circuit", "Circuit:",
                                               choices = NULL,
                                               multiple = TRUE,
                                               options = list(placeholder = "All Circuits"))
                         ),
                         column(2,
                                numericInput("fl_start_year", "Start Year:", 
                                             value = 2007, min = 2007, max = 2025, step = 1)
                         ),
                         column(2,
                                numericInput("fl_end_year", "End Year:", 
                                             value = 2025, min = 2007, max = 2025, step = 1)
                         ),
                         column(2,
                                div(style = "margin-top: 25px;",
                                    actionButton("fl_load", "Load Data", 
                                                 class = "btn-primary", 
                                                 style = "width: 100%;"))
                         )
                       ),
                       fluidRow(
                         column(12,
                                div(class = "info-box", style = "margin-top: 15px;",
                                    p(style = "margin: 0; color: #ffffff;",
                                      strong("Note: "), 
                                      "Points Era = 2019-2024 (1 point if top 10). Non-Points = 2007-2018. 2025 = Standalone year"
                                    )
                                )
                         )
                       )
                   )
               )
        )
      ),
      
      conditionalPanel(
        condition = "output.fl_loaded",
        
        # Grid Position Analysis
        fluidRow(
          column(6,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Fastest Laps by Starting Position", class = "box-title")
                     ),
                     div(class = "box-body",
                         withSpinner(plotlyOutput("fl_by_grid_plot", height = "500px"))
                     )
                 )
          ),
          column(6,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Fastest Laps by Finish Position", class = "box-title")
                     ),
                     div(class = "box-body",
                         withSpinner(plotlyOutput("fl_by_finish_plot", height = "500px"))
                     )
                 )
          )
        ),
        
        # Point Era Comparison
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Points Era vs Non-Points Era", class = "box-title")
                     ),
                     div(class = "box-body",
                         withSpinner(plotlyOutput("fl_era_comparison", height = "500px"))
                     )
                 )
          )
        ),
        
        # Yearly Analysis
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Yearly Fastest Lap Analysis", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(6,
                                  selectInput("fl_yearly_metric", "Metric:",
                                              choices = c(
                                                "Starting Grid Position" = "grid",
                                                "Finish Position" = "position"
                                              ),
                                              selected = "grid")
                           ),
                           column(6,
                                  selectInput("fl_yearly_type", "Chart Type:",
                                              choices = c(
                                                "Box Plot" = "box",
                                                "Violin Plot" = "violin",
                                                "Bar Chart" = "bar"
                                              ),
                                              selected = "box")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(plotlyOutput("fl_yearly_plot", height = "600px"))
                           )
                         )
                     )
                 )
          )
        ),
        
        # Detailed Data Table
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header", style = "display: flex; justify-content: space-between; align-items: center;",
                         h3("All Fastest Laps", class = "box-title", style = "margin: 0;"),
                         downloadButton("download_fl", 
                                        "Download CSV", 
                                        class = "btn-success",
                                        style = "margin: 0;")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("fl_detail_table"))
                           )
                         )
                     )
                 )
          )
        ),
        
        # Analysis Tables Section
        fluidRow(
          column(6,
                 div(class = "box",
                     div(class = "box-header", style = "display: flex; justify-content: space-between; align-items: center;",
                         h3("Fastest Lap Finish Position Rates by Era", class = "box-title", style = "margin: 0;"),
                         downloadButton("download_fl_finish_rates", 
                                        "Download Excel", 
                                        class = "btn-success",
                                        style = "margin: 0;")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("fl_finish_rates_table"))
                           )
                         )
                     )
                 )
          ),
          column(6,
                 div(class = "box",
                     div(class = "box-header", style = "display: flex; justify-content: space-between; align-items: center;",
                         h3("Finish Position by Start Position (2015+)", class = "box-title", style = "margin: 0;"),
                         downloadButton("download_finish_by_grid", 
                                        "Download Excel", 
                                        class = "btn-success",
                                        style = "margin: 0;")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("finish_by_grid_table"))
                           )
                         )
                     )
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "!output.fl_loaded",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("No Data Loaded", class = "box-title")
                     ),
                     div(class = "box-body",
                         p(style = "color: #ffffff; text-align: center; padding: 40px;",
                           "Select filters above and click Load Data.")
                     )
                 )
          )
        )
      )
    ),
    
    # Driver Analysis Tab
    tabPanel(
      "Driver Analysis",
      value = "drivers",
      
      conditionalPanel(
        condition = "output.data_loaded",
        
        # Data table
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header", style = "display: flex; justify-content: space-between; align-items: center;",
                         h3("Driver Statistics", class = "box-title", style = "margin: 0;"),
                         downloadButton("download_driver_stats", 
                                        "Download CSV", 
                                        class = "btn-success",
                                        style = "margin: 0;")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("driver_stats_table"))
                           )
                         )
                     )
                 )
          )
        ),
        
        # Visualizations
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Driver Performance Visualizations", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(6,
                                  selectInput("driver_visual_type", "Select Visualization:",
                                              choices = c(
                                                "Score Distribution by Driver" = "score_dist",
                                                "Finish Position Distribution" = "finish_dist",
                                                "Grid vs Finish Position" = "grid_finish",
                                                "Laps Led Distribution" = "laps_led",
                                                "Classification Rate" = "classification"
                                              ),
                                              selected = "score_dist")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(plotlyOutput("driver_plot", height = "700px"))
                           )
                         )
                     )
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "!output.data_loaded",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("No Data Loaded", class = "box-title")
                     ),
                     div(class = "box-body",
                         p(style = "color: #ffffff; text-align: center; padding: 40px;",
                           "Please go to Data Selection tab and load data first.")
                     )
                 )
          )
        )
      )
    ),
    
    # Laps Led Analysis Tab
    tabPanel(
      "Laps Led Analysis",
      value = "laps_led",
      
      conditionalPanel(
        condition = "output.data_loaded",
        
        # Starting Position Analysis
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Laps Led by Starting Position", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  div(class = "info-box",
                                      p(style = "margin: 0; color: #ffffff;",
                                        strong("Use this data to estimate laps led probability for sim inputs based on starting position.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("laps_led_by_start"))
                           )
                         )
                     )
                 )
          )
        ),
        
        # Visualization
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Laps Led Visualizations", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(6,
                                  selectInput("ll_visual_type", "Select Visualization:",
                                              choices = c(
                                                "Average Laps Led by Grid" = "avg_by_grid",
                                                "% Drivers Who Led Laps by Grid" = "pct_by_grid",
                                                "Laps Led Distribution" = "distribution",
                                                "Laps Led by Finish Position" = "by_finish"
                                              ),
                                              selected = "avg_by_grid")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(plotlyOutput("laps_led_plot", height = "700px"))
                           )
                         )
                     )
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "!output.data_loaded",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("No Data Loaded", class = "box-title")
                     ),
                     div(class = "box-body",
                         p(style = "color: #ffffff; text-align: center; padding: 40px;",
                           "Please go to Data Selection tab and load data first.")
                     )
                 )
          )
        )
      )
    ),
    
    # Sim Input Builder Tab
    tabPanel(
      "Sim Input Builder",
      value = "sim_builder",
      
      conditionalPanel(
        condition = "output.data_loaded",
        
        # Template Selection
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Build Sim Input Template", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  div(class = "info-box",
                                      p(style = "margin: 0; color: #ffffff;",
                                        strong("Instructions: "),
                                        "This section helps you build sim input files using historical averages and trends. Select the metrics you want to include."
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(4,
                                  checkboxGroupInput("sim_metrics", "Select Metrics to Include:",
                                                     choices = c(
                                                       "Average Finish" = "avg_finish",
                                                       "Average Grid Position" = "avg_grid",
                                                       "Average DK Score" = "avg_score",
                                                       "Average Laps Led" = "avg_ll",
                                                       "Classification Rate %" = "class_rate",
                                                       "Fastest Lap Rate %" = "fl_rate",
                                                       "Team Win Rate %" = "tw_rate"
                                                     ),
                                                     selected = c("avg_finish", "avg_score", "avg_ll"))
                           ),
                           column(8,
                                  withSpinner(DT::dataTableOutput("sim_input_preview"))
                           )
                         ),
                         fluidRow(
                           column(12,
                                  div(style = "margin-top: 20px;",
                                      downloadButton("download_sim_template", 
                                                     "Download Sim Template", 
                                                     class = "btn-success",
                                                     style = "width: 300px; padding: 15px; font-size: 16px;")
                                  )
                           )
                         )
                     )
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "!output.data_loaded",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("No Data Loaded", class = "box-title")
                     ),
                     div(class = "box-body",
                         p(style = "color: #ffffff; text-align: center; padding: 40px;",
                           "Please go to Data Selection tab and load data first.")
                     )
                 )
          )
        )
      )
    )
  )
)

#--------------------- Server Function ---------------------#

server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    f1_data = NULL,
    circuits = NULL,
    hist_data = NULL,
    hist_loaded = FALSE,
    fl_data = NULL,
    fl_loaded = FALSE,
    driver_data = NULL,
    driver_loaded = FALSE,
    ll_data = NULL,
    ll_loaded = FALSE,
    sim_data = NULL,
    sim_loaded = FALSE
  )
  
  # Load database on startup
  observe({
    data <- load_f1_database()
    if (!is.null(data)) {
      values$f1_data <- data
      circuits <- get_circuits(data)
      values$circuits <- circuits
      
      # Update circuit choices for all tabs - use circuit_id for both value and label
      circuit_choices <- setNames(circuits$circuit_id, circuits$circuit_id)
      updateSelectizeInput(session, "hist_circuit", choices = circuit_choices)
      updateSelectizeInput(session, "fl_circuit", choices = circuit_choices)
      updateSelectizeInput(session, "driver_circuit", choices = circuit_choices)
      updateSelectizeInput(session, "ll_circuit", choices = circuit_choices)
      updateSelectizeInput(session, "sim_circuit", choices = circuit_choices)
    }
  })
  
  #--------------------- Historical Results Tab ---------------------#
  
  # Load historical data
  observeEvent(input$hist_load, {
    req(values$f1_data)
    
    filtered <- values$f1_data
    
    # Filter by circuits if selected
    if (!is.null(input$hist_circuit) && length(input$hist_circuit) > 0) {
      filtered <- filtered %>%
        filter(circuit_id %in% input$hist_circuit)
    }
    
    # Filter by years
    filtered <- filtered %>%
      filter(
        race_season >= input$hist_start_year,
        race_season <= input$hist_end_year
      )
    
    values$hist_data <- filtered
    values$hist_loaded <- TRUE
  })
  
  output$hist_loaded <- reactive({
    values$hist_loaded
  })
  outputOptions(output, "hist_loaded", suspendWhenHidden = FALSE)
  
  output$results_table <- renderDT({
    req(values$hist_data)
    
    display_data <- values$hist_data %>%
      select(race_season, race_name, driver_name, position, grid, laps, 
             pd, TeamW, FL, LL, classification, DKScore) %>%
      rename(
        Season = race_season,
        Race = race_name,
        Driver = driver_name,
        Finish = position,
        Grid = grid,
        Laps = laps,
        PD = pd,
        TeamW = TeamW,
        FL = FL,
        LL = LL,
        Class = classification,
        DKScore = DKScore
      ) %>%
      arrange(desc(Season), Race, Finish)
    
    datatable(display_data,
              options = list(
                pageLength = 50,
                scrollX = TRUE,
                dom = 'frtip'
              ),
              rownames = FALSE) %>%
      formatRound(columns = c("DKScore"), digits = 1)
  })
  
  # Download results
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("F1_Results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$hist_data, file, row.names = FALSE)
    }
  )
  
  #--------------------- Fastest Laps Tab ---------------------#
  
  # Load fastest laps data
  observeEvent(input$fl_load, {
    req(values$f1_data)
    
    # Filter for only races where someone got fastest lap (FL == 1 in database)
    filtered <- values$f1_data %>%
      filter(FL == 1)
    
    # Filter by circuits if selected
    if (!is.null(input$fl_circuit) && length(input$fl_circuit) > 0) {
      filtered <- filtered %>%
        filter(circuit_id %in% input$fl_circuit)
    }
    
    # Filter by years
    filtered <- filtered %>%
      filter(
        race_season >= input$fl_start_year,
        race_season <= input$fl_end_year
      )
    
    values$fl_data <- filtered
    values$fl_loaded <- TRUE
  })
  
  output$fl_loaded <- reactive({
    values$fl_loaded
  })
  outputOptions(output, "fl_loaded", suspendWhenHidden = FALSE)
  
  # Fastest laps by grid position - with circuit comparison
  output$fl_by_grid_plot <- renderPlotly({
    req(values$fl_data)
    
    # All circuits data
    all_data <- values$fl_data %>%
      mutate(circuit_group = "All Circuits")
    
    # Individual circuit data if filtered
    if (!is.null(input$fl_circuit) && length(input$fl_circuit) > 0) {
      circuit_data <- values$fl_data %>%
        filter(circuit_id %in% input$fl_circuit) %>%
        mutate(circuit_group = circuit_id)
      
      combined_data <- bind_rows(all_data, circuit_data)
    } else {
      combined_data <- all_data
    }
    
    grid_stats <- combined_data %>%
      group_by(circuit_group, grid) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(circuit_group) %>%
      mutate(pct = round(count / sum(count) * 100, 1)) %>%
      ungroup() %>%
      filter(grid <= 20)
    
    plot_ly(grid_stats, x = ~grid, y = ~pct, color = ~circuit_group,
            type = "bar",
            text = ~paste0(pct, "%"),
            textposition = "outside",
            colors = c("All Circuits" = "#FFD700", 
                       setNames(rep("#FF8700", length(input$fl_circuit)), input$fl_circuit))) %>%
      layout(
        title = "Fastest Laps by Starting Grid Position",
        xaxis = list(title = "Starting Grid Position", dtick = 1),
        yaxis = list(title = "Percentage (%)"),
        barmode = "group",
        plot_bgcolor = "#2d2d2d",
        paper_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff"),
        legend = list(font = list(color = "#ffffff"))
      )
  })
  
  # Fastest laps by finish position - with circuit comparison
  output$fl_by_finish_plot <- renderPlotly({
    req(values$fl_data)
    
    # All circuits data
    all_data <- values$fl_data %>%
      mutate(circuit_group = "All Circuits")
    
    # Individual circuit data if filtered
    if (!is.null(input$fl_circuit) && length(input$fl_circuit) > 0) {
      circuit_data <- values$fl_data %>%
        filter(circuit_id %in% input$fl_circuit) %>%
        mutate(circuit_group = circuit_id)
      
      combined_data <- bind_rows(all_data, circuit_data)
    } else {
      combined_data <- all_data
    }
    
    finish_stats <- combined_data %>%
      group_by(circuit_group, position) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(circuit_group) %>%
      mutate(pct = round(count / sum(count) * 100, 1)) %>%
      ungroup() %>%
      filter(position <= 20)
    
    plot_ly(finish_stats, x = ~position, y = ~pct, color = ~circuit_group,
            type = "bar",
            text = ~paste0(pct, "%"),
            textposition = "outside",
            colors = c("All Circuits" = "#FFD700", 
                       setNames(rep("#FF8700", length(input$fl_circuit)), input$fl_circuit))) %>%
      layout(
        title = "Fastest Laps by Finish Position",
        xaxis = list(title = "Finish Position", dtick = 1),
        yaxis = list(title = "Percentage (%)"),
        barmode = "group",
        plot_bgcolor = "#2d2d2d",
        paper_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff"),
        legend = list(font = list(color = "#ffffff"))
      )
  })
  
  # Era comparison plot - Points Era (2019-2024), Non-Points (2007-2018), and 2025
  output$fl_era_comparison <- renderPlotly({
    req(values$fl_data)
    
    era_data <- values$fl_data %>%
      mutate(era = case_when(
        race_season == 2025 ~ "2025",
        race_season >= 2019 & race_season <= 2024 ~ "Points Era (2019-2024)",
        TRUE ~ "Non-Points Era (2007-2018)"
      )) %>%
      group_by(era, position) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(era) %>%
      mutate(pct = round(count / sum(count) * 100, 1)) %>%
      ungroup() %>%
      filter(position <= 20)
    
    plot_ly(era_data, x = ~position, y = ~pct, color = ~era,
            type = "bar",
            text = ~paste0(pct, "%"),
            textposition = "outside",
            colors = c("Points Era (2019-2024)" = "#FFD700", 
                       "Non-Points Era (2007-2018)" = "#00CED1",
                       "2025" = "#FF4500")) %>%
      layout(
        title = "Finish Position by Era",
        xaxis = list(title = "Finish Position", dtick = 1),
        yaxis = list(title = "Percentage (%)"),
        barmode = "group",
        plot_bgcolor = "#2d2d2d",
        paper_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff"),
        legend = list(font = list(color = "#ffffff"))
      )
  })
  
  # Era comparison table - REMOVED
  output$fl_era_table <- renderDT({
    # Empty to remove table
    datatable(data.frame())
  })
  
  # Yearly analysis plot
  output$fl_yearly_plot <- renderPlotly({
    req(values$fl_data)
    
    metric <- input$fl_yearly_metric
    chart_type <- input$fl_yearly_type
    
    metric_name <- if_else(metric == "grid", "Starting Grid Position", "Finish Position")
    
    plot_data <- values$fl_data %>%
      mutate(season_str = as.character(race_season))
    
    if (chart_type == "box") {
      plot_ly(plot_data, x = ~season_str, y = ~get(metric),
              type = "box",
              marker = list(color = "#FFD700"),
              line = list(color = "#FFD700"),
              fillcolor = "rgba(255, 215, 0, 0.3)") %>%
        layout(
          title = paste0(metric_name, " Distribution by Season"),
          xaxis = list(title = "Season", tickangle = -45),
          yaxis = list(title = metric_name, 
                       autorange = if_else(metric == "position", "reversed", TRUE)),
          plot_bgcolor = "#2d2d2d",
          paper_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff")
        )
    } else if (chart_type == "violin") {
      plot_ly(plot_data, x = ~season_str, y = ~get(metric),
              type = "violin",
              box = list(visible = TRUE),
              meanline = list(visible = TRUE),
              fillcolor = "rgba(255, 215, 0, 0.5)",
              line = list(color = "#FFD700"),
              marker = list(color = "#FFD700")) %>%
        layout(
          title = paste0(metric_name, " Distribution by Season"),
          xaxis = list(title = "Season", tickangle = -45),
          yaxis = list(title = metric_name,
                       autorange = if_else(metric == "position", "reversed", TRUE)),
          plot_bgcolor = "#2d2d2d",
          paper_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          showlegend = FALSE
        )
    } else {
      # Bar chart with averages
      yearly_avg <- values$fl_data %>%
        group_by(race_season) %>%
        summarise(avg_val = round(mean(get(metric), na.rm = TRUE), 1), .groups = "drop")
      
      plot_ly(yearly_avg, x = ~race_season, y = ~avg_val,
              type = "bar",
              text = ~avg_val,
              textposition = "outside",
              marker = list(color = "#FFD700")) %>%
        layout(
          title = paste0("Average ", metric_name, " by Season"),
          xaxis = list(title = "Season", dtick = 1),
          yaxis = list(title = paste("Avg", metric_name),
                       autorange = if_else(metric == "position", "reversed", TRUE)),
          plot_bgcolor = "#2d2d2d",
          paper_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff")
        )
    }
  })
  
  # Detailed fastest laps table
  output$fl_detail_table <- renderDT({
    req(values$fl_data)
    
    display_data <- values$fl_data %>%
      mutate(era = case_when(
        race_season == 2025 ~ "2025",
        race_season >= 2019 & race_season <= 2024 ~ "Points Era",
        TRUE ~ "Non-Points"
      )) %>%
      select(race_season, circuit_id, race_name, driver_name, grid, position, era) %>%
      rename(
        Season = race_season,
        Circuit = circuit_id,
        Race = race_name,
        Driver = driver_name,
        `Start` = grid,
        `Finish` = position,
        Era = era
      ) %>%
      arrange(desc(Season), Circuit)
    
    datatable(display_data,
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                dom = 'frtip'
              ),
              rownames = FALSE)
  })
  
  # Download fastest laps
  output$download_fl <- downloadHandler(
    filename = function() {
      paste0("F1_FastestLaps_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$fl_data, file, row.names = FALSE)
    }
  )
  
  # Fastest Lap Finish Position Rates by Era
  fl_finish_rates <- reactive({
    req(values$fl_data)
    
    values$fl_data %>%
      mutate(era = case_when(
        race_season == 2025 ~ "2025",
        race_season >= 2019 & race_season <= 2024 ~ "Points Era (2019-2024)",
        TRUE ~ "Non-Points Era (2007-2018)"
      )) %>%
      group_by(era, position) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(era) %>%
      mutate(
        total = sum(count),
        percentage = round(count / total * 100, 1)
      ) %>%
      ungroup() %>%
      select(era, position, count, percentage) %>%
      arrange(era, position)
  })
  
  output$fl_finish_rates_table <- renderDT({
    req(fl_finish_rates())
    
    datatable(fl_finish_rates(),
              colnames = c("Era", "Finish Position", "Count", "Percentage (%)"),
              options = list(
                pageLength = 20,
                scrollX = TRUE,
                dom = 'frtip'
              ),
              rownames = FALSE)
  })
  
  output$download_fl_finish_rates <- downloadHandler(
    filename = function() {
      paste0("FL_FinishPositionRates_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(fl_finish_rates(), file)
    }
  )
  
  # Finish Position by Start Position (2015+)
  finish_by_grid <- reactive({
    req(values$f1_data)
    
    data_2015_plus <- values$f1_data %>%
      filter(race_season >= 2015)
    
    # Calculate frequency of each finish position for each starting position
    data_2015_plus %>%
      group_by(grid, position) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(grid) %>%
      mutate(
        total = sum(count),
        percentage = round(count / total * 100, 1)
      ) %>%
      ungroup() %>%
      select(grid, position, count, percentage) %>%
      arrange(grid, position)
  })
  
  output$finish_by_grid_table <- renderDT({
    req(finish_by_grid())
    
    datatable(finish_by_grid(),
              colnames = c("Start Position", "Finish Position", "Count", "Percentage (%)"),
              options = list(
                pageLength = 20,
                scrollX = TRUE,
                dom = 'frtip'
              ),
              rownames = FALSE)
  })
  
  output$download_finish_by_grid <- downloadHandler(
    filename = function() {
      paste0("FinishByGrid_2015Plus_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(finish_by_grid(), file)
    }
  )
  
  #--------------------- Driver Analysis Tab ---------------------#
  
  driver_stats <- reactive({
    req(values$filtered_data)
    
    values$filtered_data %>%
      group_by(driver_name, driver_id) %>%
      summarise(
        Races = n(),
        AvgFinish = round(mean(position, na.rm = TRUE), 1),
        AvgGrid = round(mean(grid, na.rm = TRUE), 1),
        AvgScore = round(mean(dk_score, na.rm = TRUE), 1),
        MedianScore = round(median(dk_score, na.rm = TRUE), 1),
        MaxScore = round(max(dk_score, na.rm = TRUE), 1),
        MinScore = round(min(dk_score, na.rm = TRUE), 1),
        AvgLL = round(mean(laps_led, na.rm = TRUE), 1),
        AvgPD = round(mean(pd, na.rm = TRUE), 1),
        Wins = sum(position == 1, na.rm = TRUE),
        Podiums = sum(position <= 3, na.rm = TRUE),
        ClassRate = round(mean(classification, na.rm = TRUE) * 100, 1),
        FLRate = round(mean(fastest_lap, na.rm = TRUE) * 100, 1),
        TWRate = round(mean(team_win, na.rm = TRUE) * 100, 1),
        .groups = "drop"
      ) %>%
      rename(Driver = driver_name) %>%
      arrange(desc(AvgScore))
  })
  
  output$driver_stats_table <- renderDT({
    req(driver_stats())
    
    datatable(driver_stats() %>% select(-driver_id),
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                dom = 'frtip'
              ),
              rownames = FALSE)
  })
  
  # Download driver stats
  output$download_driver_stats <- downloadHandler(
    filename = function() {
      paste0("F1_Driver_Stats_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(driver_stats(), file, row.names = FALSE)
    }
  )
  
  # Driver visualizations
  output$driver_plot <- renderPlotly({
    req(values$filtered_data)
    
    plot_data <- values$filtered_data
    
    p <- switch(input$driver_visual_type,
                "score_dist" = {
                  # Top 15 drivers by avg score
                  top_drivers <- driver_stats() %>%
                    slice_max(order_by = AvgScore, n = 15) %>%
                    pull(Driver)
                  
                  plot_data %>%
                    filter(driver_name %in% top_drivers) %>%
                    plot_ly(x = ~driver_name, y = ~dk_score, type = "box",
                            color = I("#FFD700"),
                            marker = list(color = "#FFD700"),
                            line = list(color = "#FFD700")) %>%
                    layout(
                      title = "DK Score Distribution - Top 15 Drivers",
                      xaxis = list(title = "Driver", tickangle = -45),
                      yaxis = list(title = "DK Score"),
                      plot_bgcolor = "#2d2d2d",
                      paper_bgcolor = "#2d2d2d",
                      font = list(color = "#ffffff")
                    )
                },
                "finish_dist" = {
                  top_drivers <- driver_stats() %>%
                    slice_max(order_by = Races, n = 15) %>%
                    pull(Driver)
                  
                  plot_data %>%
                    filter(driver_name %in% top_drivers) %>%
                    plot_ly(x = ~driver_name, y = ~position, type = "box",
                            color = I("#FFD700"),
                            marker = list(color = "#FFD700"),
                            line = list(color = "#FFD700")) %>%
                    layout(
                      title = "Finish Position Distribution",
                      xaxis = list(title = "Driver", tickangle = -45),
                      yaxis = list(title = "Finish Position", autorange = "reversed"),
                      plot_bgcolor = "#2d2d2d",
                      paper_bgcolor = "#2d2d2d",
                      font = list(color = "#ffffff")
                    )
                },
                "grid_finish" = {
                  plot_data %>%
                    plot_ly(x = ~grid, y = ~position, type = "scatter", mode = "markers",
                            marker = list(color = "#FFD700", opacity = 0.6)) %>%
                    layout(
                      title = "Starting Grid vs Finish Position",
                      xaxis = list(title = "Grid Position"),
                      yaxis = list(title = "Finish Position", autorange = "reversed"),
                      plot_bgcolor = "#2d2d2d",
                      paper_bgcolor = "#2d2d2d",
                      font = list(color = "#ffffff")
                    )
                },
                "laps_led" = {
                  top_drivers <- driver_stats() %>%
                    slice_max(order_by = AvgLL, n = 15) %>%
                    pull(Driver)
                  
                  plot_data %>%
                    filter(driver_name %in% top_drivers) %>%
                    plot_ly(x = ~driver_name, y = ~laps_led, type = "box",
                            color = I("#FFD700"),
                            marker = list(color = "#FFD700"),
                            line = list(color = "#FFD700")) %>%
                    layout(
                      title = "Laps Led Distribution - Top Lap Leaders",
                      xaxis = list(title = "Driver", tickangle = -45),
                      yaxis = list(title = "Laps Led"),
                      plot_bgcolor = "#2d2d2d",
                      paper_bgcolor = "#2d2d2d",
                      font = list(color = "#ffffff")
                    )
                },
                "classification" = {
                  stats <- driver_stats() %>%
                    filter(Races >= 5) %>%
                    arrange(desc(ClassRate)) %>%
                    head(20)
                  
                  plot_ly(stats, x = ~reorder(Driver, ClassRate), y = ~ClassRate,
                          type = "bar",
                          marker = list(color = "#FFD700")) %>%
                    layout(
                      title = "Classification Rate % (Min 5 Races)",
                      xaxis = list(title = "Driver", tickangle = -45),
                      yaxis = list(title = "Classification Rate %"),
                      plot_bgcolor = "#2d2d2d",
                      paper_bgcolor = "#2d2d2d",
                      font = list(color = "#ffffff")
                    )
                }
    )
    
    p
  })
  
  #--------------------- Laps Led Analysis Tab ---------------------#
  
  laps_led_stats <- reactive({
    req(values$filtered_data)
    calc_laps_led_prob(values$filtered_data, by = "grid")
  })
  
  output$laps_led_by_start <- renderDT({
    req(laps_led_stats())
    
    datatable(laps_led_stats() %>%
                rename(
                  `Grid Position` = grid,
                  `Avg Laps Led` = avg_laps_led,
                  `Median Laps Led` = median_laps_led,
                  `Max Laps Led` = max_laps_led,
                  `% Who Led Laps` = pct_led_laps,
                  Races = races
                ),
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                dom = 't'
              ),
              rownames = FALSE) %>%
      formatRound(columns = c("Avg Laps Led", "Median Laps Led", "% Who Led Laps"), digits = 1)
  })
  
  output$laps_led_plot <- renderPlotly({
    req(values$filtered_data)
    
    p <- switch(input$ll_visual_type,
                "avg_by_grid" = {
                  stats <- laps_led_stats()
                  
                  plot_ly(stats, x = ~grid, y = ~avg_laps_led,
                          type = "bar",
                          marker = list(color = "#FFD700")) %>%
                    layout(
                      title = "Average Laps Led by Starting Position",
                      xaxis = list(title = "Grid Position"),
                      yaxis = list(title = "Average Laps Led"),
                      plot_bgcolor = "#2d2d2d",
                      paper_bgcolor = "#2d2d2d",
                      font = list(color = "#ffffff")
                    )
                },
                "pct_by_grid" = {
                  stats <- laps_led_stats()
                  
                  plot_ly(stats, x = ~grid, y = ~pct_led_laps,
                          type = "bar",
                          marker = list(color = "#FFD700")) %>%
                    layout(
                      title = "% of Drivers Who Led Laps by Starting Position",
                      xaxis = list(title = "Grid Position"),
                      yaxis = list(title = "% Who Led Laps"),
                      plot_bgcolor = "#2d2d2d",
                      paper_bgcolor = "#2d2d2d",
                      font = list(color = "#ffffff")
                    )
                },
                "distribution" = {
                  values$filtered_data %>%
                    filter(laps_led > 0) %>%
                    plot_ly(x = ~laps_led, type = "histogram",
                            marker = list(color = "#FFD700")) %>%
                    layout(
                      title = "Distribution of Laps Led (When > 0)",
                      xaxis = list(title = "Laps Led"),
                      yaxis = list(title = "Count"),
                      plot_bgcolor = "#2d2d2d",
                      paper_bgcolor = "#2d2d2d",
                      font = list(color = "#ffffff")
                    )
                },
                "by_finish" = {
                  finish_stats <- values$filtered_data %>%
                    group_by(position) %>%
                    summarise(
                      avg_ll = mean(laps_led, na.rm = TRUE),
                      .groups = "drop"
                    ) %>%
                    filter(position <= 20)
                  
                  plot_ly(finish_stats, x = ~position, y = ~avg_ll,
                          type = "bar",
                          marker = list(color = "#FFD700")) %>%
                    layout(
                      title = "Average Laps Led by Finish Position (Top 20)",
                      xaxis = list(title = "Finish Position"),
                      yaxis = list(title = "Average Laps Led"),
                      plot_bgcolor = "#2d2d2d",
                      paper_bgcolor = "#2d2d2d",
                      font = list(color = "#ffffff")
                    )
                }
    )
    
    p
  })
  
  #--------------------- Sim Input Builder Tab ---------------------#
  
  sim_input_data <- reactive({
    req(values$filtered_data, input$sim_metrics)
    
    base_stats <- driver_stats() %>%
      select(Driver, driver_id, Races)
    
    # Add selected metrics
    if ("avg_finish" %in% input$sim_metrics) {
      base_stats <- base_stats %>%
        left_join(
          driver_stats() %>% select(driver_id, AvgFinish),
          by = "driver_id"
        )
    }
    
    if ("avg_grid" %in% input$sim_metrics) {
      base_stats <- base_stats %>%
        left_join(
          driver_stats() %>% select(driver_id, AvgGrid),
          by = "driver_id"
        )
    }
    
    if ("avg_score" %in% input$sim_metrics) {
      base_stats <- base_stats %>%
        left_join(
          driver_stats() %>% select(driver_id, AvgScore),
          by = "driver_id"
        )
    }
    
    if ("avg_ll" %in% input$sim_metrics) {
      base_stats <- base_stats %>%
        left_join(
          driver_stats() %>% select(driver_id, AvgLL),
          by = "driver_id"
        )
    }
    
    if ("class_rate" %in% input$sim_metrics) {
      base_stats <- base_stats %>%
        left_join(
          driver_stats() %>% select(driver_id, ClassRate),
          by = "driver_id"
        )
    }
    
    if ("fl_rate" %in% input$sim_metrics) {
      base_stats <- base_stats %>%
        left_join(
          driver_stats() %>% select(driver_id, FLRate),
          by = "driver_id"
        )
    }
    
    if ("tw_rate" %in% input$sim_metrics) {
      base_stats <- base_stats %>%
        left_join(
          driver_stats() %>% select(driver_id, TWRate),
          by = "driver_id"
        )
    }
    
    base_stats %>%
      select(-driver_id) %>%
      arrange(desc(Races))
  })
  
  output$sim_input_preview <- renderDT({
    req(sim_input_data())
    
    datatable(sim_input_data(),
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                dom = 'frtip'
              ),
              rownames = FALSE)
  })
  
  # Download sim template
  output$download_sim_template <- downloadHandler(
    filename = function() {
      paste0("F1_Sim_Template_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Sim Input")
      writeData(wb, "Sim Input", sim_input_data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)