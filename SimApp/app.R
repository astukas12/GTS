# ============================================================================
# GOLDEN TICKET SIMS - UNIVERSAL APP
# Single app for all sports with auto-detection
# ============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(readxl)
library(plotly)
library(ggplot2)

# Source configuration and core functions
source("sport_configs_universal.R")
source("OptimalLineups_Core.R")
source("LineupBuilder_Core.R")
source("portfolio_helpers_universal.R")  # Universal exposure table helper


# ============================================================================
# UI
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$span("Simulation App", style = "color: #FFE500 !important; font-weight: 700; font-size: 18px;"),
    titleWidth = 200
  ),
  
  dashboardSidebar(
    width = 200,
    
    # Logo at top of sidebar
    tags$div(
      style = "padding: 15px; text-align: center; background-color: #000000; border-bottom: 2px solid #FFE500;",
      tags$img(src = "logo.jpg", width = "160px")
    ),
    
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Data Input", tabName = "input", icon = icon("file-upload")),
      menuItem("Sim Results", tabName = "sim_results", icon = icon("chart-bar")),
      menuItem("Lineup Scoring", tabName = "scoring", icon = icon("trophy")),
      menuItem("Portfolio Builder", tabName = "portfolio", icon = icon("layer-group"))
    )
  ),
  
  dashboardBody(
    # ========================================================================
    # THEME AND STYLING
    # ========================================================================
    
    # Load base CSS theme
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "gts_theme.css")
    ),
    
    # CRITICAL: Inject overrides with maximum specificity
    tags$style(HTML("
      /* ====================================================================
         GOLDEN TICKET SIMS - THEME OVERRIDES
         Black & Gold theme with NO dropdown interference
         ==================================================================== */
      
      /* ==================================================================
         BOX HEADERS - Gold theme, kill blue/orange
         ================================================================== */
      .box-primary > .box-header, 
      .box.box-primary > .box-header,
      .box-solid.box-primary > .box-header {
        background-color: #2d2d2d !important;
        color: #FFE500 !important;
        border-bottom: 2px solid #FFE500 !important;
      }
      
      .box-primary .box-title {
        color: #FFE500 !important;
        font-weight: 600 !important;
      }
      
      .box-primary, 
      .box.box-primary {
        border-top-color: #404040 !important;
        border-color: #404040 !important;
      }
      
      .box-warning > .box-header {
        background-color: #2d2d2d !important;
        color: #FFE500 !important;
        border-color: #FFE500 !important;
      }
      
      .box-warning .box-title {
        color: #FFE500 !important;
        font-weight: 600 !important;
      }
      
      .box-info > .box-header,
      .box.box-info > .box-header {
        background-color: #2d2d2d !important;
        color: #FFE500 !important;
        border-bottom: 2px solid #FFE500 !important;
      }
      
      .box-info .box-title {
        color: #FFE500 !important;
      }
      
      .box-info,
      .box.box-info {
        border-top-color: #404040 !important;
        border-color: #404040 !important;
        background-color: #1e1e1e !important;
      }
      
      .box-warning {
        background-color: #1e1e1e !important;
        border: 1px solid #FFE500 !important;
      }
      
      /* ==================================================================
         PANELS - Kill orange/warning colors
         ================================================================== */
      .panel-warning > .panel-heading,
      .panel-heading,
      .panel-default > .panel-heading {
        background-color: #2d2d2d !important;
        color: #FFE500 !important;
        border-color: #FFE500 !important;
      }
      
      .panel-title,
      .panel-heading h4 {
        color: #FFE500 !important;
        font-weight: 600 !important;
      }
      
      /* ==================================================================
         SLIDERS - Gold theme
         ================================================================== */
      .irs-bar, 
      .irs-bar-edge, 
      .irs-handle,
      .irs--flat .irs-bar, 
      .irs--modern .irs-bar, 
      .irs--round .irs-bar,
      .irs--flat .irs-handle, 
      .irs--modern .irs-handle,
      .irs--round .irs-handle {
        background: #FFE500 !important;
        border-color: #D4B000 !important;
      }
      
      .irs-from, 
      .irs-to, 
      .irs-single,
      .irs--flat .irs-from,
      .irs--flat .irs-to,
      .irs--flat .irs-single,
      .irs--modern .irs-from,
      .irs--modern .irs-to,
      .irs--modern .irs-single {
        background: #FFE500 !important;
        color: #000000 !important;
        font-weight: 600 !important;
      }
      
      .irs-line {
        background-color: #404040 !important;
      }
      
      .irs-grid-text {
        color: #999999 !important;
      }
      
      /* ==================================================================
         BUTTONS - Gold primary/warning, green success
         ================================================================== */
      .btn-primary,
      .btn-primary:hover,
      .btn-primary:focus,
      .btn-primary:active {
        background-color: #FFE500 !important;
        color: #000000 !important;
        border-color: #D4B000 !important;
        font-weight: 600 !important;
      }
      
      .btn-warning,
      .btn-warning:hover,
      .btn-warning:focus,
      .btn-warning:active {
        background-color: #FFE500 !important;
        color: #000000 !important;
        border-color: #D4B000 !important;
        font-weight: 600 !important;
      }
      
      /* ==================================================================
         PORTFOLIO DROPDOWNS - MINIMAL STYLING
         ================================================================== */
      
      /* Just style colors, don't mess with display/positioning */
      .selectize-input {
        background-color: #1e1e1e !important;
        border: 1px solid #404040 !important;
        color: #ffffff !important;
      }
      
      .selectize-input.focus {
        border-color: #FFE500 !important;
      }
      
      .selectize-input .item {
        background: #FFE500 !important;
        color: #000000 !important;
        font-weight: 600 !important;
      }
      
      .selectize-dropdown {
        background: #1e1e1e !important;
        border: 1px solid #FFE500 !important;
      }
      
      .selectize-dropdown .option {
        color: #ffffff !important;
      }
      
      .selectize-dropdown .option:hover,
      .selectize-dropdown .option.active {
        background: #FFE500 !important;
        color: #000000 !important;
      }
      
      /* ==================================================================
         TABS - Gold theme
         ================================================================== */
      .nav-tabs,
      .nav.nav-tabs {
        border-top-color: transparent !important;
        border-left-color: transparent !important;
        border-right-color: transparent !important;
        border-bottom: 2px solid #FFE500 !important;
        background-color: transparent !important;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #1e1e1e !important;
        color: #FFE500 !important;
        border: 1px solid #FFE500 !important;
        border-bottom-color: #1e1e1e !important;
      }
      
      .nav-tabs > li > a {
        color: #cccccc !important;
        background-color: #2d2d2d !important;
        border: 1px solid #404040 !important;
        border-bottom: none !important;
      }
      
      .nav-tabs > li > a:hover {
        background-color: #404040 !important;
        border-color: #FFE500 !important;
        color: #FFE500 !important;
      }
      
      /* ==================================================================
         ALERTS - Override info/primary colors
         ================================================================== */
      .alert-info,
      .callout-info {
        background-color: rgba(255, 229, 0, 0.15) !important;
        border-color: #FFE500 !important;
        color: #FFF4B3 !important;
      }
      
      .alert-warning {
        background-color: rgba(255, 229, 0, 0.15) !important;
        border-color: #FFE500 !important;
        color: #FFF4B3 !important;
      }
      
      /* ==================================================================
         PROGRESS BARS - Gold instead of blue
         ================================================================== */
      .progress-bar-primary,
      .progress-bar-info {
        background-color: #FFE500 !important;
        color: #000000 !important;
      }
      
      /* ==================================================================
         DATATABLES - Gold highlights
         ================================================================== */
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        background-color: #FFE500 !important;
        color: #000000 !important;
        border-color: #D4B000 !important;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background-color: #404040 !important;
        color: #FFE500 !important;
        border-color: #FFE500 !important;
      }
      
      /* ==================================================================
         KILL BACKGROUND ARTIFACTS - Critical for clean display
         ================================================================== */
      
      /* Remove ALL background images from header */
      .skin-black .main-header,
      .skin-black .main-header .navbar,
      .skin-black .main-header .logo {
        background-image: none !important;
        background-color: #000000 !important;
      }
      
      /* Remove pseudo-elements that create artifacts */
      .skin-black .main-header::before,
      .skin-black .main-header::after,
      .skin-black .main-header .navbar::before,
      .skin-black .main-header .navbar::after {
        display: none !important;
        content: none !important;
      }
      
      /* Clean header border */
      .skin-black .main-header {
        border-bottom: 3px solid #FFE500 !important;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.3) !important;
      }
      
      /* Ensure content wrapper is clean */
      .content-wrapper {
        background-color: #121212 !important;
        background-image: none !important;
      }
      
      .content-wrapper::before,
      .content-wrapper::after {
        display: none !important;
        content: none !important;
      }
      
      /* Kill any background images in boxes */
      .box, 
      .box-body, 
      .box-header, 
      .box-footer {
        background-image: none !important;
      }
      
      /* Ensure sidebar is clean */
      .main-sidebar {
        background-image: none !important;
        background-color: #121212 !important;
      }
      
      .main-sidebar::before,
      .main-sidebar::after {
        display: none !important;
        content: none !important;
      }
      
      /* Kill any bootstrap default backgrounds */
      .panel, 
      .panel-body, 
      .panel-heading, 
      .well {
        background-image: none !important;
        background-color: #2d2d2d !important;
      }
      
      /* Remove teal/cyan from tab-content areas */
      .content-wrapper,
      .tab-content,
      .tab-pane {
        background-color: #121212 !important;
        background-image: none !important;
      }
      
      /* ==================================================================
         PORTFOLIO PAGE SPECIFIC FIXES
         ================================================================== */
    
      
      /* Ensure nested content is also dark */
      #portfolio *,
      #portfolio .box,
      #portfolio .box-body {
        background-image: none !important;
      }
      
      /* Tab panels in portfolio */
      #portfolio .tab-content,
      #portfolio .tab-pane {
        background-color: #121212 !important;
        background-image: none !important;
      }
      
      /* Specific fix for tabsetPanel background */
      .tabbable,
      .tabbable .tab-content {
        background-color: transparent !important;
        background-image: none !important;
      }
      

      /* ==================================================================
         REMOVE ANY LINGERING BLUE FROM RGB VALUES
         ================================================================== */
      *[style*='background-color: rgb(60, 141, 188)'],
      *[style*='background-color: #3c8dbc'],
      *[style*='border-color: rgb(60, 141, 188)'],
      *[style*='border-color: #3c8dbc'],
      *[style*='background-color: rgb(51, 122, 183)'],
      *[style*='background-color: #337ab7'] {
        background-color: #2d2d2d !important;
        border-color: #FFE500 !important;
        color: #FFE500 !important;
      }
      
      /* ==================================================================
         REMOVE ANY LINGERING ORANGE FROM RGB VALUES
         ================================================================== */
      *[style*='background-color: rgb(243, 156, 18)'],
      *[style*='background-color: #f39c12'],
      *[style*='background-color: rgb(255, 152, 0)'],
      *[style*='background-color: #ff9800'] {
        background-color: #2d2d2d !important;
        border-color: #FFE500 !important;
        color: #FFE500 !important;
      }
      
      /* ==================================================================
         RADIO BUTTONS - Style for better visibility
         ================================================================== */
      .radio label,
      .checkbox label {
        color: #ffffff !important;
        font-weight: 500 !important;
      }
      
      .radio input[type='radio']:checked + span::before {
        background-color: #FFE500 !important;
        border-color: #FFE500 !important;
      }
      
      /* ==================================================================
         FORM INPUTS - Basic dark theme
         ================================================================== */
      .form-control {
        background-color: #1e1e1e !important;
        color: #ffffff !important;
        border-color: #404040 !important;
      }
      
      .form-control:focus {
        background-color: #1e1e1e !important;
        border-color: #FFE500 !important;
        box-shadow: 0 0 0 2px rgba(255, 229, 0, 0.2) !important;
      }
      
      /* Labels */
      label {
        color: #ffffff !important;
      }
      
      /* ==================================================================
         LOADING SPINNERS - Gold theme
         ================================================================== */
      .shiny-spinner-message-container {
        background-color: rgba(0, 0, 0, 0.9) !important;
        border: 2px solid #FFE500 !important;
        border-radius: 8px !important;
      }
      
      .shiny-spinner-message {
        color: #FFE500 !important;
        font-weight: 600 !important;
      }
    ")),
    
    # JavaScript to fix portfolio dropdowns
    tags$script(HTML("
      $(document).ready(function() {
        // Wait for Shiny and selectize to initialize
        setTimeout(function() {
          console.log('Initializing dropdown fixes...');
          
          // Find all multi-select dropdowns
          $('select[multiple]').each(function() {
            if (this.selectize) {
              var selectize = this.selectize;
              console.log('Fixed selectize:', this.id);
              
              // Close after adding item
              selectize.on('item_add', function() {
                console.log('Item added, closing...');
                var self = this;
                setTimeout(function() {
                  self.close();
                  self.blur();
                }, 100);
              });
              
              // Close after removing item
              selectize.on('item_remove', function() {
                console.log('Item removed, closing...');
                var self = this;
                setTimeout(function() {
                  self.close();
                  self.blur();
                }, 100);
              });
              
              // Start closed
              selectize.close();
            }
          });
        }, 2500);
        
        // Handle dynamically created selectize elements
        $(document).on('shiny:value', function(event) {
          if (event.name.includes('locked') || event.name.includes('excluded')) {
            setTimeout(function() {
              var elem = document.getElementById(event.name);
              if (elem && elem.selectize) {
                elem.selectize.close();
              }
            }, 200);
          }
        });
      });
    ")),
    
    # ========================================================================
    # MAIN CONTENT TABS
    # ========================================================================
    
    tabItems(
      # ======================================================================
      # TAB 1: DATA INPUT
      # ======================================================================
      tabItem(
        tabName = "input",
        
        # Upload box - full width
        fluidRow(
          column(
            width = 12,
            box(
              title = "Upload Input File",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              
              fileInput(
                "input_file",
                NULL,
                accept = c(".xlsx", ".xls"),
                width = "100%"
              ),
              
              uiOutput("sport_detected_message")
            )
          )
        ),
        
        # Simulation box - full width
        fluidRow(
          column(
            width = 12,
            box(
              title = "Run Simulation",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              
              fluidRow(
                column(4,
                       h5("Sport:", style = "color: #FFE500; margin-top: 0;"),
                       textOutput("sport_display")
                ),
                column(4,
                       h5("Platforms:", style = "color: #FFE500; margin-top: 0;"),
                       textOutput("platforms_display")
                ),
                column(4,
                       numericInput(
                         "n_sims",
                         "Number of Simulations",
                         value = 10000,
                         min = 100,
                         max = 50000,
                         step = 1000,
                         width = "100%"
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       actionButton("run_simulation", "Run Simulation", class = "btn-primary", style = "width: 100%; margin-top: 10px;")
                )
              ),
              
              uiOutput("sim_complete_message")
            )
          )
        )
      ),
      
      # ======================================================================
      # TAB 2: SIM RESULTS
      # ======================================================================
      tabItem(
        tabName = "sim_results",
        
        # Show message if no simulation loaded
        conditionalPanel(
          condition = "output.has_sim_results == false",
          box(
            width = 12,
            title = "No Simulation Results",
            status = "warning",
            solidHeader = TRUE,
            div(
              style = "text-align: center; padding: 40px;",
              icon("chart-bar", class = "fa-3x", style = "color: #FFE500; margin-bottom: 20px;"),
              h3("Run a simulation first", style = "color: #FFE500; font-weight: 600;"),
              p("Upload your data and run a simulation in the Data Input tab to see results here.",
                style = "color: #cccccc; font-size: 16px; margin-top: 15px;")
            )
          )
        ),
        
        # Show results when simulation is loaded
        conditionalPanel(
          condition = "output.has_sim_results == true",
          
          # Platform Selector
          fluidRow(
            column(
              width = 12,
              box(
                width = NULL,
                title = "SELECT PLATFORM",
                status = "primary",
                solidHeader = TRUE,
                div(
                  style = "padding: 5px 0;",
                  radioButtons(
                    "sim_results_platform",
                    label = NULL,
                    choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                    selected = "DK",
                    inline = TRUE
                  )
                )
              )
            )
          ),
          
          # Fantasy Projections Table (NO EXPLANATION BOX)
          fluidRow(
            column(
              width = 12,
              box(
                width = NULL,
                title = "FANTASY PROJECTIONS & DISTRIBUTIONS",
                status = "primary",
                solidHeader = TRUE,
                DTOutput("sim_projections_table") %>% 
                  shinycssloaders::withSpinner(color = "#FFE500", type = 6)
              )
            )
          ),
          
          # NASCAR Charts with Subtabs (ONLY show if NASCAR)
          conditionalPanel(
            condition = "output.sport_detected == \\'NASCAR\\'",
            fluidRow(
              column(
                width = 12,
                box(
                  width = NULL,
                  title = "NASCAR SIMULATION ANALYSIS",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  tabsetPanel(
                    id = "nascar_charts_tabs",
                    type = "tabs",
                    
                    tabPanel(
                      "Finishing Position",
                      div(style = "margin-top: 15px;"),
                      plotlyOutput("finish_distribution_plot", height = "600px") %>% 
                        shinycssloaders::withSpinner(color = "#FFE500", type = 6)
                    ),
                    
                    tabPanel(
                      "Dominator by Driver",
                      div(style = "margin-top: 15px;"),
                      plotlyOutput("dominator_violin_driver", height = "600px") %>% 
                        shinycssloaders::withSpinner(color = "#FFE500", type = 6)
                    ),
                    
                    tabPanel(
                      "Dominator by Position",
                      div(
                        style = "margin-top: 15px; margin-bottom: 15px;",
                        radioButtons(
                          "dominator_position_group",
                          label = "Group By:",
                          choices = c("Starting Position" = "start", "Finish Position" = "finish"),
                          selected = "start",
                          inline = TRUE
                        )
                      ),
                      plotlyOutput("dominator_violin_position", height = "450px") %>% 
                        shinycssloaders::withSpinner(color = "#FFE500", type = 6)
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # ======================================================================
      # TAB 3: LINEUP SCORING
      # ======================================================================
      tabItem(
        tabName = "scoring",
        
        # Platform Sub-Tabs (conditional based on sport)
        uiOutput("scoring_tabs_ui")
      ),
      
      # ======================================================================
      # TAB 4: PORTFOLIO BUILDER
      # ======================================================================
      tabItem(
        tabName = "portfolio",
        
        # Platform Sub-Tabs (conditional based on sport)
        uiOutput("portfolio_tabs_ui")
      )
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    sport = NULL,
    config = NULL,
    input_data = NULL,
    processed_data = NULL,
    simulation_results = NULL,
    sim_metadata = NULL,
    projections = NULL,  
    dk_optimal_lineups = NULL,
    fd_optimal_lineups = NULL,
    sd_optimal_lineups = NULL,
    # Portfolio storage
    dk_portfolio = NULL,
    fd_portfolio = NULL,
    sd_portfolio = NULL,
    dk_builds = list(),
    fd_builds = list(),
    sd_builds = list(),
    dk_build_counter = 0,
    fd_build_counter = 0,
    sd_build_counter = 0
  )
  
  
  # ==========================================================================
  # AUTO-DETECT SPORT FROM FILE AND AUTO-LOAD
  # ==========================================================================
  
  observeEvent(input$input_file, {
    req(input$input_file)
    
    tryCatch({
      # Detect sport
      rv$sport <- detect_sport(input$input_file$datapath)
      rv$config <- get_sport_config(rv$sport)
      
      # Show detection message
      output$sport_detected_message <- renderUI({
        tags$div(
          class = "alert alert-success",
          style = "margin-top: 10px; padding: 8px;",
          icon("check-circle"),
          sprintf(" Detected: %s", rv$config$sport_display_name)
        )
      })
      
      # Display sport and platforms
      output$sport_display <- renderText({
        rv$config$sport_display_name
      })
      
      output$platforms_display <- renderText({
        # For NASCAR, show actual detected platforms after simulation runs
        if (rv$sport == "NASCAR" && !is.null(rv$has_fd)) {
          if (rv$has_fd) {
            "DraftKings, FanDuel"
          } else {
            "DraftKings"
          }
        } else {
          # For other sports or before NASCAR simulation, show config platforms
          platform_names <- sapply(rv$config$platforms, function(p) {
            switch(p, "DK" = "DraftKings", "FD" = "FanDuel", "SD" = "Showdown")
          })
          paste(platform_names, collapse = ", ")
        }
      })
      
      # AUTO-LOAD DATA (no button needed)
      file_path <- input$input_file$datapath
      
      if (rv$config$input_file$type == "excel") {
        # Check if we should load all sheets (for sports like NFL with dynamic team sheets)
        if (!is.null(rv$config$input_file$load_all_sheets) && rv$config$input_file$load_all_sheets) {
          # Load ALL sheets from the file
          all_sheets <- excel_sheets(file_path)
          rv$input_data <- lapply(all_sheets, function(sheet) {
            read_excel(file_path, sheet = sheet)
          })
          names(rv$input_data) <- all_sheets
        } else {
          # Read only required sheets (default behavior for NASCAR, MMA, etc.)
          rv$input_data <- lapply(
            rv$config$input_file$required_sheets,
            function(sheet) {
              read_excel(file_path, sheet = sheet)
            }
          )
          names(rv$input_data) <- rv$config$input_file$required_sheets
        }
      }
      
      # Show data preview
      output$data_preview_ui <- renderUI({
        lapply(names(rv$input_data), function(sheet_name) {
          tagList(
            h4(sheet_name, style = "color: #FFE500;"),
            DTOutput(paste0("preview_", sheet_name))
          )
        })
      })
      
      # Render preview tables
      lapply(names(rv$input_data), function(sheet_name) {
        output[[paste0("preview_", sheet_name)]] <- renderDT({
          rv$input_data[[sheet_name]]
        }, options = list(pageLength = 5, scrollX = TRUE))
      })
      
      showNotification("Data loaded successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      output$sport_detected_message <- renderUI({
        tags$div(
          class = "alert alert-danger",
          style = "margin-top: 10px; padding: 8px;",
          icon("exclamation-triangle"),
          sprintf(" Error: %s", e$message)
        )
      })
    })
  })
  
  
  # ==========================================================================
  # LOAD DATA (REMOVED - Now happens automatically)
  # ==========================================================================
  
  # This observeEvent is no longer needed - data loads automatically
  
  
  # ==========================================================================
  # RUN SIMULATION
  # ==========================================================================
  
  observeEvent(input$run_simulation, {
    req(rv$input_data, rv$config)
    
    # Source sport-specific simulation engine
    engine_file <- paste0(tolower(rv$sport), "_engine.R")
    
    cat("Looking for engine file:", engine_file, "\n")
    cat("Current working directory:", getwd(), "\n")
    
    if (!file.exists(engine_file)) {
      showNotification(
        paste("Simulation engine not found:", engine_file),
        type = "error",
        duration = 10
      )
      return()
    }
    
    source(engine_file)
    
    progress <- Progress$new(session, min = 0, max = 1)
    progress$set(message = "Running simulation...", value = 0, detail = "Initializing...")
    
    on.exit(progress$close())
    
    tryCatch({
      # Call sport-specific simulation function
      sim_function_name <- rv$config$simulation$function_name
      sim_function <- get(sim_function_name)
      
      # Run simulation with progress callback
      result <- sim_function(
        input_data = rv$input_data,
        n_sims = input$n_sims,
        config = rv$config,
        progress_callback = function(detail, value) {
          progress$set(value = value, detail = detail)
        }
      )
      
      # Validate output
      validate_simulation_output(
        result$sim_results,
        result$metadata,
        rv$config
      )
      
      rv$simulation_results <- result$sim_results
      rv$sim_metadata <- result$metadata
      
      # Store platform availability (for NASCAR)
      if (rv$sport == "NASCAR") {
        rv$full_sim_results <- result$full_results
        rv$has_fd <- if (!is.null(result$has_fd)) result$has_fd else TRUE
      } else {
        rv$full_sim_results <- NULL
        rv$has_fd <- TRUE  # Other sports default to having all platforms
      }
      
      cat("Stored simulation results with", nrow(rv$simulation_results), "rows\n")
      cat("Sport:", rv$sport, "\n")
      cat("Full results:", !is.null(rv$full_sim_results), "\n")
      
      if (!is.null(result$projections)) {
        rv$projections <- result$projections
        cat("Stored projections with", nrow(result$projections), "rows\n")
      }
      
      # Show completion message
      output$sim_complete_message <- renderUI({
        tags$div(
          class = "alert alert-success",
          style = "margin-top: 10px;",
          icon("check-circle"),
          sprintf(
            " Simulation complete! %s simulations run with %s %s.",
            input$n_sims,
            nrow(rv$sim_metadata),
            tolower(rv$config$player_label_plural)
          )
        )
      })
      
      showNotification("Simulation complete!", type = "message")
      
    }, error = function(e) {
      showNotification(
        paste("Simulation error:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  
  # ==========================================================================
  # LINEUP OPTIMIZATION EVENT HANDLERS
  # ==========================================================================
  
  # ==========================================================================
  # HELPER FUNCTIONS FOR LINEUP DISPLAY AND DOWNLOAD
  # ==========================================================================
  
  create_display_table <- function(optimal_lineups, metadata, platform) {
    
    # Detect column structure
    if ("Captain" %in% names(optimal_lineups)) {
      # Showdown format
      player_cols <- c("Captain", grep("^Util", names(optimal_lineups), value = TRUE))
    } else if ("MVP" %in% names(optimal_lineups)) {
      # FD MVP format
      player_cols <- c("MVP", grep("^Player", names(optimal_lineups), value = TRUE))
    } else {
      # Standard format
      player_cols <- grep("^Player", names(optimal_lineups), value = TRUE)
    }
    
    display_cols <- c(
      player_cols,
      "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
      "TotalSalary", 
      "CumulativeOwnership", "GeometricMeanOwnership"
    )
    
    # Only include columns that exist
    display_cols <- intersect(display_cols, names(optimal_lineups))
    
    # Create display table
    display_table <- optimal_lineups[, ..display_cols]
    
    # Rename columns for display
    rename_map <- c(
      "WinRate" = "Win",
      "Top1Pct" = "Top1",
      "Top5Pct" = "Top5",
      "Top10Pct" = "Top10",
      "Top20Pct" = "Top20",
      "TotalSalary" = "Salary",
      "CumulativeOwnership" = "TotalOwn",
      "GeometricMeanOwnership" = "AvgOwn"
    )
    
    for (old_name in names(rename_map)) {
      if (old_name %in% names(display_table)) {
        setnames(display_table, old_name, rename_map[[old_name]])
      }
    }
    
    return(display_table)
  }
  
  # Create download table (detects column structure automatically)
  create_download_table <- function(optimal_lineups, metadata, platform) {
    
    # Detect format
    if ("Captain" %in% names(optimal_lineups)) {
      return(create_download_showdown(optimal_lineups, metadata))
    } else if ("MVP" %in% names(optimal_lineups)) {
      return(create_download_mvp(optimal_lineups, metadata))
    } else {
      return(create_download_standard(optimal_lineups, metadata, platform))
    }
  }
  
  # ========================================================================
  # UNIVERSAL PORTFOLIO TABLE DISPLAY
  # ========================================================================
  
  create_portfolio_display_table <- function(portfolio_data, sport_config) {
    
    display_table <- copy(portfolio_data)
    setDT(display_table)
    
    # Add row index for deletion
    display_table[, RowID := .I]
    
    # Standard column renames (universal across all sports)
    rename_map <- c(
      "WinRate" = "Win",
      "Top1Pct" = "Top1",
      "Top5Pct" = "Top5",
      "Top10Pct" = "Top10",
      "Top20Pct" = "Top20",
      "TotalSalary" = "Salary",
      "CumulativeOwnership" = "TotalOwn",
      "GeometricMeanOwnership" = "AvgOwn"
    )
    
    for (old_name in names(rename_map)) {
      if (old_name %in% names(display_table)) {
        setnames(display_table, old_name, rename_map[[old_name]])
      }
    }
    
    # Sport-specific custom metrics (from config)
    if (!is.null(sport_config$custom_metrics)) {
      for (metric in sport_config$custom_metrics) {
        # Handle both 'source' and 'source_column' for backwards compatibility
        source_col <- if (!is.null(metric$source_column)) metric$source_column else metric$source
        display_col <- if (!is.null(metric$display_name)) metric$display_name else metric$label
        
        # Only rename if we have values and they're different
        if (!is.null(source_col) && !is.null(display_col) && 
            source_col %in% names(display_table) && 
            source_col != display_col) {
          setnames(display_table, source_col, display_col)
        }
      }
    }
    
    # Add Delete button as first column
    display_table[, Delete := paste0(
      '<button class="btn btn-danger btn-xs delete-lineup" data-row="', 
      RowID, 
      '">X</button>'
    )]
    
    # Move Delete to front, hide RowID
    cols_order <- c("Delete", setdiff(names(display_table), c("Delete", "RowID")))
    setcolorder(display_table, cols_order)
    
    return(display_table)
  }
  
  # Get columns to format based on what exists in the data
  get_format_columns <- function(display_table, sport_config) {
    
    # Standard percentage columns (always try to format if they exist)
    pct_cols <- c("Win", "Top1", "Top5", "Top10", "Top20", "TotalOwn", "AvgOwn")
    
    # Add custom metric columns from config
    if (!is.null(sport_config$custom_metrics)) {
      for (metric in sport_config$custom_metrics) {
        if (metric$format == "percentage") {
          pct_cols <- c(pct_cols, metric$display_name)
        }
      }
    }
    
    # Only return columns that actually exist
    return(intersect(pct_cols, names(display_table)))
  }
  
  # Standard format download
  create_download_standard <- function(optimal_lineups, metadata, platform) {
    player_cols <- grep("^Player", names(optimal_lineups), value = TRUE)
    roster_size <- length(player_cols)
    
    id_col <- paste0(platform, "ID")
    download_table <- copy(optimal_lineups)
    
    for (i in 1:roster_size) {
      player_col <- paste0("Player", i)
      players <- download_table[[player_col]]
      ids <- metadata[match(players, metadata$Player), get(id_col)]
      
      # Format based on platform
      if (platform == "DK") {
        download_table[[player_col]] <- paste0(players, " (", ids, ")")
      } else {
        # FD format
        download_table[[player_col]] <- paste0(ids, ":", players)
      }
    }
    
    return(download_table)
  }
  
  # Showdown format download
  create_download_showdown <- function(optimal_lineups, metadata) {
    download_table <- copy(optimal_lineups)
    
    # Captain: use CPTID
    if ("Captain" %in% names(download_table)) {
      captain_ids <- metadata[match(download_table$Captain, metadata$Player), CPTID]
      download_table$Captain <- paste0(download_table$Captain, " (", captain_ids, ")")
    }
    
    # Utilities: use SDID
    util_cols <- grep("^Util", names(download_table), value = TRUE)
    for (col in util_cols) {
      players <- download_table[[col]]
      util_ids <- metadata[match(players, metadata$Player), SDID]
      download_table[[col]] <- paste0(players, " (", util_ids, ")")
    }
    
    return(download_table)
  }
  
  # MVP format download (FD MMA)
  create_download_mvp <- function(optimal_lineups, metadata) {
    download_table <- copy(optimal_lineups)
    
    setDT(metadata)
    
    # MVP: "FDID:Name"
    if ("MVP" %in% names(download_table)) {
      mvp_names <- download_table$MVP
      mvp_ids <- metadata[match(mvp_names, metadata$Player), FDID]
      
      # Check for NAs
      if (any(is.na(mvp_ids))) {
        missing <- mvp_names[is.na(mvp_ids)]
        cat("Warning: Missing FDID for MVP:", paste(missing, collapse = ", "), "\n")
      }
      
      download_table$MVP <- paste0(mvp_ids, ":", mvp_names)
    }
    
    # Flex players: "FDID:Name"
    player_cols <- grep("^Player", names(download_table), value = TRUE)
    for (col in player_cols) {
      players <- download_table[[col]]
      flex_ids <- metadata[match(players, metadata$Player), FDID]
      
      # Check for NAs
      if (any(is.na(flex_ids))) {
        missing <- players[is.na(flex_ids)]
        cat("Warning: Missing FDID for flex:", paste(missing, collapse = ", "), "\n")
      }
      
      download_table[[col]] <- paste0(flex_ids, ":", players)
    }
    
    return(download_table)
  }
  
  prepare_optimization_data <- function(sim_results, metadata, platform) {
    # Standardized: OptimalLineups_Core uses "FantasyPoints" and "Salary"
    
    # For Showdown, use DK scoring but SD salary
    if (platform == "SD") {
      score_col <- "DKScore"      # SD uses DK scoring
      salary_col <- "SDSalary"    # SD has its own salary structure
    } else {
      score_col <- paste0(platform, "Score")   # DKScore or FDScore
      salary_col <- paste0(platform, "Salary") # DKSalary or FDSalary
    }
    
    setDT(sim_results)
    setDT(metadata)
    
    # Merge with salary
    opt_data <- merge(
      sim_results,
      metadata[, .(Player, Salary = get(salary_col))],
      by = "Player"
    )
    
    # Create standardized FantasyPoints column
    opt_data[, FantasyPoints := get(score_col)]
    
    # FILTER OUT PLAYERS WITH $0 SALARY (not available on this platform)
    opt_data <- opt_data[Salary > 0 & !is.na(Salary)]
    
    return(opt_data)
  }
  
  # DK Optimization
  observeEvent(input$run_dk_optimization, {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    
    progress <- Progress$new(session)
    progress$set(message = "Finding optimal DraftKings lineups...", value = 0)
    on.exit(progress$close())
    
    tryCatch({
      # Prepare data
      opt_data <- prepare_optimization_data(
        rv$simulation_results,
        rv$sim_metadata,
        "DK"
      )
      
      opt_config <- list(
        roster_size = rv$config$roster_sizes$DK,
        salary_cap = rv$config$salary_caps$DK,
        progress_frequency = 500,
        use_parallel = TRUE,
        percentiles = c(0.01, 0.05, 0.10, 0.20),
        platform_col = "DKScore"
      )
      
      # DK always uses standard mode
      mode <- "standard"
      
      # Phase 1: Find optimal lineups (top 3 per sim)
      progress$set(detail = "Phase 1: Finding optimal lineups...", value = 0.1)
      lineup_data <- find_optimal_lineups(opt_data, opt_config, mode = mode, k = 3, verbose = TRUE)
      
      # Phase 2: Score all unique lineups across all sims
      progress$set(detail = "Phase 2: Scoring lineups across all sims...", value = 0.4)
      score_matrix <- score_all_lineups(lineup_data, opt_data, verbose = TRUE)
      
      # Phase 3: Calculate distribution metrics
      progress$set(detail = "Phase 3: Calculating win rates and percentiles...", value = 0.7)
      
      # Prepare ownership data with platform-specific column name
      ownership_for_phase3 <- copy(rv$sim_metadata)
      own_col <- "DKOwn"
      if (own_col %in% names(ownership_for_phase3)) {
        setnames(ownership_for_phase3, own_col, "Own")
      }
      
      final_results <- calculate_distribution_metrics(
        score_matrix, 
        lineup_data, 
        opt_config,
        ownership_data = ownership_for_phase3,
        verbose = TRUE
      )
      
      # Add custom metrics (TeamStack for NFL, TotalStart/AvgStart for NASCAR)
      final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config)
      
      # Store results
      rv$dk_optimal_lineups <- final_results
      
      progress$set(detail = "Complete!", value = 1.0)
      
      showNotification(
        sprintf("Found %d optimal DraftKings lineups!", nrow(final_results)),
        type = "message"
      )
      
    }, error = function(e) {
      showNotification(
        paste("Optimization error:", e$message),
        type = "error",
        duration = NULL
      )
      cat("Full error:\n")
      print(e)
    })
  })
  
  # FD Optimization
  observeEvent(input$run_fd_optimization, {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    
    progress <- Progress$new(session)
    progress$set(message = "Finding optimal FanDuel lineups...", value = 0)
    on.exit(progress$close())
    
    tryCatch({
      # Prepare data
      opt_data <- prepare_optimization_data(
        rv$simulation_results,
        rv$sim_metadata,
        "FD"
      )
      
      opt_config <- list(
        roster_size = rv$config$roster_sizes$FD,
        salary_cap = rv$config$salary_caps$FD,
        progress_frequency = 500,
        use_parallel = TRUE,
        percentiles = c(0.01, 0.05, 0.10, 0.20),
        platform_col = "FDScore",
        mvp_multiplier = 1.5
      )
      
      # FD MMA uses MVP mode, others use standard
      mode <- if (rv$sport == "MMA") "mvp" else "standard"
      
      # Phase 1: Find optimal lineups
      progress$set(detail = "Phase 1: Finding optimal lineups...", value = 0.1)
      lineup_data <- find_optimal_lineups(opt_data, opt_config, mode = mode, k = 3, verbose = TRUE)
      
      # Phase 2: Score all lineups
      progress$set(detail = "Phase 2: Scoring lineups across all sims...", value = 0.4)
      score_matrix <- score_all_lineups(lineup_data, opt_data, verbose = TRUE)
      
      # Phase 3: Calculate metrics
      progress$set(detail = "Phase 3: Calculating win rates and percentiles...", value = 0.7)
      
      # Prepare ownership data with platform-specific column name
      ownership_for_phase3 <- copy(rv$sim_metadata)
      own_col <- "FDOwn"
      if (own_col %in% names(ownership_for_phase3)) {
        setnames(ownership_for_phase3, own_col, "Own")
      }
      
      final_results <- calculate_distribution_metrics(
        score_matrix,
        lineup_data,
        opt_config,
        ownership_data = ownership_for_phase3,
        verbose = TRUE
      )
      
      # Add custom metrics (TeamStack for NFL, TotalStart/AvgStart for NASCAR)
      final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config)
      
      # Store results
      rv$fd_optimal_lineups <- final_results
      
      progress$set(detail = "Complete!", value = 1.0)
      
      showNotification(
        sprintf("Found %d optimal FanDuel lineups!", nrow(final_results)),
        type = "message"
      )
      
    }, error = function(e) {
      showNotification(
        paste("Optimization error:", e$message),
        type = "error",
        duration = NULL
      )
      cat("Full error:\n")
      print(e)
    })
  })
  
  # SD Optimization
  observeEvent(input$run_sd_optimization, {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    
    progress <- Progress$new(session)
    progress$set(message = "Finding optimal Showdown lineups...", value = 0)
    on.exit(progress$close())
    
    tryCatch({
      # Prepare data (SD uses DK scoring but SD salary)
      opt_data <- prepare_optimization_data(
        rv$simulation_results,
        rv$sim_metadata,
        "SD"
      )
      
      # Get platform-specific config
      opt_config <- list(
        roster_size = rv$config$roster_sizes$SD,
        salary_cap = rv$config$salary_caps$SD,
        progress_frequency = 500,
        use_parallel = TRUE,
        percentiles = c(0.01, 0.05, 0.10, 0.20),
        platform_col = "DKScore",
        cpt_multiplier = 1.5
      )
      
      # SD always uses captain mode
      mode <- "captain"
      
      # Phase 1: Find optimal lineups
      progress$set(detail = "Phase 1: Finding optimal lineups...", value = 0.1)
      lineup_data <- find_optimal_lineups(opt_data, opt_config, mode = mode, k = 3, verbose = TRUE)
      
      # Phase 2: Score all lineups
      progress$set(detail = "Phase 2: Scoring lineups across all sims...", value = 0.4)
      score_matrix <- score_all_lineups(lineup_data, opt_data, verbose = TRUE)
      
      # Phase 3: Calculate metrics
      progress$set(detail = "Phase 3: Calculating win rates and percentiles...", value = 0.7)
      
      ownership_for_phase3 <- copy(rv$sim_metadata)
      own_col <- "DKOwn"
      if (own_col %in% names(ownership_for_phase3)) {
        setnames(ownership_for_phase3, own_col, "Own")
      }
      
      final_results <- calculate_distribution_metrics(
        score_matrix,
        lineup_data,
        opt_config,
        ownership_data = ownership_for_phase3,
        verbose = TRUE
      )
      
      # Add custom metrics (TeamStack for NFL, TotalStart/AvgStart for NASCAR)
      final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config)
      
      rv$sd_optimal_lineups <- final_results
      
      progress$set(detail = "Complete!", value = 1.0)
      
      showNotification(
        sprintf("Found %d optimal Showdown lineups!", nrow(final_results)),
        type = "message"
      )
      
    }, error = function(e) {
      showNotification(
        paste("Optimization error:", e$message),
        type = "error",
        duration = NULL
      )
      cat("Full error:\n")
      print(e)
    })
  })
  
  
  # Render DK table
  # Download handlers
  output$dk_download <- downloadHandler(
    filename = function() {
      paste0("DK_Optimal_Lineups_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      download_table <- create_download_table(rv$dk_optimal_lineups, rv$sim_metadata, "DK")
      fwrite(download_table, file)
    }
  )
  
  output$fd_download <- downloadHandler(
    filename = function() {
      paste0("FD_Optimal_Lineups_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      download_table <- create_download_table(rv$fd_optimal_lineups, rv$sim_metadata, "FD")
      fwrite(download_table, file)
    }
  )
  
  output$sd_download <- downloadHandler(
    filename = function() {
      paste0("SD_Optimal_Lineups_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      download_table <- create_download_table(rv$sd_optimal_lineups, rv$sim_metadata, "SD")
      fwrite(download_table, file)
    }
  )
  
  
  # ==========================================================================
  # DYNAMIC UI: SCORING TABS (Platform Sub-Tabs)
  # ==========================================================================
  
  output$scoring_tabs_ui <- renderUI({
    req(rv$config)
    
    # Single unified box for all platforms
    fluidRow(
      box(
        title = "Lineup Scoring",
        status = "warning",  # Gold/yellow color
        solidHeader = TRUE,
        width = 12,
        
        p("Find and score optimal lineups across all platforms:"),
        
        # Score buttons for each platform
        fluidRow(
          lapply(rv$config$platforms, function(platform) {
            platform_name <- switch(
              platform,
              "DK" = "DraftKings",
              "FD" = "FanDuel",
              "SD" = "Showdown"
            )
            
            column(
              width = 6,  # Changed from 4 to 6 for wider buttons
              actionButton(
                paste0("run_", tolower(platform), "_optimization"),
                paste("Score", platform_name),
                class = "btn-warning btn-block",
                style = "margin-bottom: 10px; font-size: 16px; padding: 12px;"
              )
            )
          })
        ),
        
        hr(),
        
        # Download buttons (only show after optimization completes)
        uiOutput("download_buttons_ui"),
        
        hr(),
        
        # Results table (dynamically switch based on selection)
        div(style = "margin-bottom: 15px;",
            uiOutput("view_platform_ui")
        ),
        
        DTOutput("lineup_results_table")
      )
    )
  })
  
  # Dynamically show download buttons after optimization
  output$download_buttons_ui <- renderUI({
    # Check which platforms have results
    platforms_ready <- c()
    if (!is.null(rv$dk_optimal_lineups)) platforms_ready <- c(platforms_ready, "DK")
    if (!is.null(rv$fd_optimal_lineups)) platforms_ready <- c(platforms_ready, "FD")
    if (!is.null(rv$sd_optimal_lineups)) platforms_ready <- c(platforms_ready, "SD")
    
    if (length(platforms_ready) == 0) {
      return(p("Score lineups to enable downloads", style = "color: #999;"))
    }
    
    # Create download buttons for completed platforms (GREEN, not blue)
    fluidRow(
      lapply(platforms_ready, function(platform) {
        platform_name <- switch(
          platform,
          "DK" = "DraftKings",
          "FD" = "FanDuel",
          "SD" = "Showdown"
        )
        
        column(
          width = 6,
          downloadButton(
            paste0(tolower(platform), "_download"),
            paste("Download All", platform, "Lineups"),
            class = "btn-block",
            style = "background-color: #4caf50 !important; border-color: #4caf50 !important; color: white !important; font-size: 14px; padding: 10px;"
          )
        )
      })
    )
  })
  
  
  output$view_platform_ui <- renderUI({
    # Check which platforms have results
    platforms_ready <- c()
    platform_labels <- c()
    
    if (!is.null(rv$dk_optimal_lineups)) {
      platforms_ready <- c(platforms_ready, "DK")
      platform_labels <- c(platform_labels, "DraftKings")
    }
    if (!is.null(rv$fd_optimal_lineups)) {
      platforms_ready <- c(platforms_ready, "FD")
      platform_labels <- c(platform_labels, "FanDuel")
    }
    if (!is.null(rv$sd_optimal_lineups)) {
      platforms_ready <- c(platforms_ready, "SD")
      platform_labels <- c(platform_labels, "Showdown")
    }
    
    if (length(platforms_ready) == 0) {
      return(
        p("Score lineups to view results", 
          style = "color: #999; font-style: italic; margin-top: 10px;")
      )
    }
    
    choices <- setNames(platforms_ready, platform_labels)
    
    div(
      style = "margin-bottom: 15px;",
      tags$label("View Results:", 
                 style = "color: #FFE500; font-weight: bold; display: block; margin-bottom: 10px;"),
      radioButtons(
        "view_platform",
        label = NULL,
        choices = choices,
        selected = platforms_ready[1],
        inline = TRUE
      )
    )
  })
  
  
  
  # Render the appropriate table based on selection
  output$lineup_results_table <- renderDT({
    req(input$view_platform)
    
    optimal_lineups <- switch(
      input$view_platform,
      "DK" = rv$dk_optimal_lineups,
      "FD" = rv$fd_optimal_lineups,
      "SD" = rv$sd_optimal_lineups
    )
    
    req(optimal_lineups)
    
    display_table <- create_display_table(optimal_lineups, rv$sim_metadata, input$view_platform)
    
    datatable(
      display_table,
      options = list(
        pageLength = 50,
        searching = FALSE,
        lengthChange = FALSE,
        scrollX = TRUE,
        dom = 'tp',
        order = list(list(which(names(display_table) == "Win") - 1, 'desc'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("Win", "Top1", "Top5", "Top10", "Top20"), 1) %>%
      formatRound(c("TotalOwn", "AvgOwn"), 1) %>%
      {
        # Only format TotalStart/AvgStart if they exist (NASCAR-specific)
        if ("TotalStart" %in% names(display_table)) {
          formatRound(., c("TotalStart", "AvgStart"), 1)
        } else {
          .
        }
      } %>%
      formatCurrency("Salary", "$", digits = 0)
  })
  
  
  # ==========================================================================
  # DYNAMIC UI: PORTFOLIO TABS (Platform Sub-Tabs)
  # ==========================================================================
  
  output$portfolio_tabs_ui <- renderUI({
    req(rv$config)
    
    # Create tab panel for each platform
    tab_panels <- lapply(rv$config$platforms, function(platform) {
      
      platform_name <- switch(
        platform,
        "DK" = "DraftKings",
        "FD" = "FanDuel",
        "SD" = "Showdown"
      )
      
      tabPanel(
        title = platform_name,
        
        # PORTFOLIO ACTION BUTTONS (Always visible at top)
        fluidRow(
          column(12,
                 div(style = "text-align: right; margin-bottom: 10px; padding: 5px; background-color: #1a1a1a; border-radius: 4px;",
                     h4(textOutput(paste0(tolower(platform), "_portfolio_count")), 
                        style = "display: inline-block; color: #FFE500; margin-right: 20px; vertical-align: middle; font-size: 16px;"),
                     actionButton(
                       paste0(tolower(platform), "_clear_portfolio"),
                       "CLEAR PORTFOLIO",
                       class = "btn-danger",
                       style = "margin-right: 10px;"
                     ),
                     downloadButton(
                       paste0(tolower(platform), "_download_portfolio"),
                       "DOWNLOAD PORTFOLIO",
                       class = "btn-success"
                     )
                 )
          )
        ),
        
        # SUB-TABS: Filtered Pool, Portfolio Summary, Portfolio Lineups
        tabsetPanel(
          id = paste0("portfolio_", tolower(platform), "_tabs"),
          
          # TAB 1: FILTERED POOL
          tabPanel(
            "Filtered Pool",
            
            # Filters Box
            fluidRow(
              box(
                title = "Lineup Filters",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                
                # ULTRA COMPACT 5-COLUMN LAYOUT
                fluidRow(
                  # Column 1: Rate Minimums (inline labels with smart step sizes)
                  column(2,
                         div(
                           style = "background-color: #2d2d2d; padding: 6px; border-radius: 4px; border: 1px solid #404040;",
                           h6("Rate Minimums", style = "color: #FFE500; font-weight: bold; margin: 0 0 8px 0; font-size: 13px;"),
                           
                           # Win - smallest step (0.1)
                           div(style = "display: flex; align-items: center; margin-bottom: 4px;",
                               tags$label("W:", style = "color: #FFE500; font-size: 11px; margin: 0 5px 0 0; width: 22px;"),
                               numericInput(paste0(tolower(platform), "_min_win"), NULL, value = 0, min = 0, max = 100, step = 0.1, width = "70px")
                           ),
                           # Top 1 - small step (0.5)
                           div(style = "display: flex; align-items: center; margin-bottom: 4px;",
                               tags$label("T1:", style = "color: #FFE500; font-size: 11px; margin: 0 5px 0 0; width: 22px;"),
                               numericInput(paste0(tolower(platform), "_min_top1"), NULL, value = 0, min = 0, max = 100, step = 0.5, width = "70px")
                           ),
                           # Top 5 - medium step (1)
                           div(style = "display: flex; align-items: center; margin-bottom: 4px;",
                               tags$label("T5:", style = "color: #FFE500; font-size: 11px; margin: 0 5px 0 0; width: 22px;"),
                               numericInput(paste0(tolower(platform), "_min_top5"), NULL, value = 0, min = 0, max = 100, step = 1, width = "70px")
                           ),
                           # Top 10 - larger step (2)
                           div(style = "display: flex; align-items: center; margin-bottom: 4px;",
                               tags$label("T10:", style = "color: #FFE500; font-size: 11px; margin: 0 5px 0 0; width: 22px;"),
                               numericInput(paste0(tolower(platform), "_min_top10"), NULL, value = 0, min = 0, max = 100, step = 2, width = "70px")
                           ),
                           # Top 20 - largest step (5)
                           div(style = "display: flex; align-items: center; margin-bottom: 0;",
                               tags$label("T20:", style = "color: #FFE500; font-size: 11px; margin: 0 5px 0 0; width: 22px;"),
                               numericInput(paste0(tolower(platform), "_min_top20"), NULL, value = 0, min = 0, max = 100, step = 5, width = "70px")
                           )
                         )
                  ),
                  
                  # Column 2: Ranges (double-stacked)
                  column(4,
                         h6("Ranges", style = "color: #FFE500; font-weight: bold; margin: 0 0 8px 0; font-size: 13px;"),
                         uiOutput(paste0(tolower(platform), "_range_sliders"))
                  ),
                  
                  # Column 3: Selection (smaller)
                  column(3,
                         h6("Selection", style = "color: #FFE500; font-weight: bold; margin: 0 0 8px 0; font-size: 13px;"),
                         
                         # Locked Players - MMA app pattern
                         selectizeInput(
                           paste0(tolower(platform), "_locked_players"),
                           "Locked:",
                           choices = if(!is.null(rv[[paste0(tolower(platform), "_optimal_lineups")]])) {
                             player_cols <- grep("^Player|^Captain|^MVP", 
                                                 names(rv[[paste0(tolower(platform), "_optimal_lineups")]]), 
                                                 value = TRUE)
                             all_players <- unique(unlist(rv[[paste0(tolower(platform), "_optimal_lineups")]][, ..player_cols]))
                             sort(all_players[!is.na(all_players) & all_players != ""])
                           } else NULL,
                           multiple = TRUE,
                           selected = character(0),
                           options = list(
                             plugins = list('remove_button'),
                             placeholder = 'Lock players (optional)',
                             maxItems = 6
                           ),
                           width = "100%"
                         ),
                         
                         # Excluded Players - MMA app pattern
                         selectizeInput(
                           paste0(tolower(platform), "_excluded_players"),
                           "Exclude:",
                           choices = if(!is.null(rv[[paste0(tolower(platform), "_optimal_lineups")]])) {
                             player_cols <- grep("^Player|^Captain|^MVP", 
                                                 names(rv[[paste0(tolower(platform), "_optimal_lineups")]]), 
                                                 value = TRUE)
                             all_players <- unique(unlist(rv[[paste0(tolower(platform), "_optimal_lineups")]][, ..player_cols]))
                             sort(all_players[!is.na(all_players) & all_players != ""])
                           } else NULL,
                           multiple = TRUE,
                           selected = character(0),
                           options = list(
                             plugins = list('remove_button'),
                             placeholder = 'Exclude players'
                           ),
                           width = "100%"
                         ),
                         
                         h4(textOutput(paste0(tolower(platform), "_filtered_count")),
                            style = "text-align: center; color: #FFE500; font-weight: bold; margin-top: 10px; font-size: 18px;")
                  ),
                  
                  # Column 4: Select Lineups (vertical)
                  column(3,
                         div(
                           style = "background-color: #2d2d2d; padding: 8px; border-radius: 4px; border: 1px solid #FFE500;",
                           h6("Select Lineups", style = "color: #FFE500; font-weight: bold; margin: 0 0 8px 0; font-size: 13px; text-align: center;"),
                           numericInput(
                             paste0(tolower(platform), "_num_lineups"),
                             "Number",
                             value = 20,
                             min = 1,
                             max = 150,
                             width = "100%"
                           ),
                           textInput(
                             paste0(tolower(platform), "_build_label"),
                             "Label",
                             value = "",
                             placeholder = "Optional",
                             width = "100%"
                           ),
                           actionButton(
                             paste0(tolower(platform), "_add_build"),
                             "ADD TO PORTFOLIO",
                             class = "btn-primary",
                             style = "width: 100%; font-weight: bold; margin-top: 4px;"
                           )
                         )
                  )
                )
              )
            ),
            
            # Filtered Pool Exposure
            fluidRow(
              box(
                title = "Player Exposure in Filtered Pool",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                
                DTOutput(paste0(tolower(platform), "_filtered_exposure"))
              )
            )
          ),
          
          # TAB 2: PORTFOLIO SUMMARY
          tabPanel(
            "Portfolio Summary",
            
            # Combined Portfolio Stats & Builds Summary
            fluidRow(
              box(
                title = "Portfolio Overview",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                
                hr(style = "border-color: #FFE500;"),
                
                DTOutput(paste0(tolower(platform), "_builds_summary"))
              )
            ),
            
            # Portfolio Exposure
            fluidRow(
              box(
                title = "Portfolio Player Exposure",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                
                DTOutput(paste0(tolower(platform), "_portfolio_exposure"))
              )
            )
          ),
          
          # TAB 3: PORTFOLIO LINEUPS
          tabPanel(
            "Portfolio Lineups",
            
            fluidRow(
              box(
                title = "All Portfolio Lineups",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                
                DTOutput(paste0(tolower(platform), "_portfolio_lineups"))
              )
            )
          )
        )
      )
    })
    
    # Create tabBox with all platforms
    do.call(tabBox, c(
      list(
        id = "portfolio_platform",
        width = 12
      ),
      tab_panels
    ))
  })
  
  
  # ==========================================================================
  # SIM RESULTS VISUALIZATION (Sport-Specific)
  # ==========================================================================
  
  output$sim_results_ui <- renderUI({
    req(rv$simulation_results, rv$config)
    
    # Check if NASCAR
    if (rv$sport == "NASCAR") {
      # Call NASCAR visualization function
      tagList(
        h4("NASCAR Simulation Results"),
        p(sprintf("%s simulations completed with %s drivers", 
                  input$n_sims, 
                  nrow(rv$sim_metadata))),
        
        # For now, show a simple summary
        # TODO: Call create_finish_rates_table() and create_finish_violin_plot()
        verbatimTextOutput("sim_summary")
      )
    } else {
      # Other sports - placeholder
      tagList(
        h4("Simulation distributions and validation plots will appear here"),
        p("These will be sport-specific and defined in each simulation engine")
      )
    }
  })
  
  output$projections_table <- renderDT({
    req(rv$projections)
    
    # Select only the columns we want
    dt <- rv$projections[, .(
      Player,
      Team, 
      Pos,
      ETR_DK,
      Sim_DK_Mean,
      ETR_FD,
      Sim_FD_Mean
    )]
    
    # Rename for display
    setnames(dt, c(
      "Player",
      "Team",
      "Pos", 
      "ETR DK",
      "Sim DK",
      "ETR FD",
      "Sim FD"
    ))
    
    datatable(
      dt,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'frtip'
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("ETR DK", "Sim DK", "ETR FD", "Sim FD"), 1)
  })
  
  # ==========================================================================
  # SIM RESULTS TAB - OUTPUT INDICATORS
  # ==========================================================================
  
  # Has simulation results indicator
  output$has_sim_results <- reactive({
    !is.null(rv$simulation_results) && nrow(rv$simulation_results) > 0
  })
  outputOptions(output, "has_sim_results", suspendWhenHidden = FALSE)
  
  # Sport detected indicator
  output$sport_detected <- reactive({
    if (!is.null(rv$sport)) {
      return(rv$sport)
    }
    return("")
  })
  outputOptions(output, "sport_detected", suspendWhenHidden = FALSE)
  
  # ==========================================================================
  # SIM RESULTS - FANTASY PROJECTIONS TABLE
  # ==========================================================================
  
  output$sim_projections_table <- renderDT({
    req(rv$simulation_results, rv$sim_metadata, input$sim_results_platform)
    
    platform <- input$sim_results_platform
    score_col <- paste0(platform, "Score")
    salary_col <- paste0(platform, "Salary")
    own_col <- paste0(platform, "Own")
    
    # Calculate summary statistics per player
    projections <- rv$simulation_results[, .(
      Avg = round(mean(get(score_col)), 1),
      Median = round(median(get(score_col)), 1),
      P90 = round(quantile(get(score_col), 0.90), 1),
      P75 = round(quantile(get(score_col), 0.75), 1),
      P25 = round(quantile(get(score_col), 0.25), 1)
    ), by = Player]
    
    # Add metadata - only get columns that exist
    metadata_cols <- c("Player", salary_col, own_col)
    
    # Add sport-specific columns if they exist
    sport_cols <- c("Starting", "Team", "Car", "Position", "Opponent", "Game")
    available_cols <- intersect(sport_cols, names(rv$sim_metadata))
    if (length(available_cols) > 0) {
      metadata_cols <- c(metadata_cols, available_cols)
    }
    
    projections <- merge(
      projections,
      rv$sim_metadata[, ..metadata_cols],
      by = "Player",
      all.x = TRUE
    )
    
    # Rename salary column to just "Salary" and ownership to "Own"
    setnames(projections, old = c(salary_col, own_col), new = c("Salary", "OwnProj"))
    
    # Convert ownership to percentage
    projections[, Own := round(OwnProj * 100, 1)]
    projections[, OwnProj := NULL]  # Remove the decimal version
    
    # Reorder columns - Salary, Own, then sport-specific, then stats
    base_cols <- c("Player", "Salary", "Own")
    sport_specific_cols <- intersect(
      c("Starting", "Team", "Car", "Position", "Opponent", "Game"), 
      names(projections)
    )
    stats_cols <- c("Avg", "Median", "P90", "P75", "P25")
    
    final_cols <- c(base_cols, sport_specific_cols, stats_cols)
    setcolorder(projections, final_cols)
    
    # Sort by average points
    setorder(projections, -Avg)
    
    # Format table - NO SEARCH, NO FILTER, JUST SORT
    dt_output <- datatable(
      projections,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        scrollY = "500px",
        searching = FALSE,        # NO SEARCH
        lengthChange = FALSE,     # NO PAGE SIZE CHANGE
        dom = "t",                # ONLY TABLE (t = table)
        order = list(list(which(names(projections) == "Avg") - 1, "desc")),
        columnDefs = list(
          list(className = "dt-right", targets = which(names(projections) %in% c(stats_cols, "Own")) - 1)
        )
      ),
      rownames = FALSE,
      class = "stripe hover compact"
    ) %>%
      formatRound(c("Avg", "Median", "P90", "P75", "P25", "Own"), 1)
    
    # Format salary with currency
    dt_output <- dt_output %>% formatCurrency("Salary", "$", digits = 0)
    
    dt_output
  })
  
  # ==========================================================================
  # NASCAR-SPECIFIC VISUALIZATIONS
  # ==========================================================================
  
  output$finish_distribution_plot <- renderPlotly({
    req(rv$sport == "NASCAR", rv$full_sim_results, input$sim_results_platform)
    
    if (is.null(rv$full_sim_results) || nrow(rv$full_sim_results) == 0) {
      return(plotly_empty() %>% 
               layout(
                 title = list(text = "No simulation data available", font = list(color = "#FFE500")),
                 paper_bgcolor = "#121212",
                 plot_bgcolor = "#1e1e1e"
               ))
    }
    
    tryCatch({
      # Get data
      plot_data <- copy(rv$full_sim_results)
      
      # Get unique drivers ordered by starting position
      driver_order <- plot_data[, .(Starting = unique(Starting)), by = Name]
      setorder(driver_order, Starting)
      ordered_drivers <- driver_order$Name
      
      # Convert to data.frame
      plot_data <- as.data.frame(plot_data)
      plot_data$Name <- factor(plot_data$Name, levels = rev(ordered_drivers))
      
      # Calculate dynamic height: minimum 600px, or 25px per driver
      num_drivers <- length(ordered_drivers)
      plot_height <- max(600, num_drivers * 25)
      
      # Create box plot - GOLD COLOR, NO VIOLIN
      p <- plot_ly(
        data = plot_data,
        x = ~FinishPosition,
        y = ~Name,
        type = "box",
        orientation = "h",
        marker = list(color = "#FFE500"),
        line = list(color = "#FFE500"),
        fillcolor = "rgba(255, 229, 0, 0.3)",
        hovertemplate = paste(
          "<b>%{y}</b><br>",
          "Median: %{x}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = list(
            text = "Finishing Position Distribution",
            font = list(color = "#FFE500", size = 16)
          ),
          xaxis = list(
            title = "Finish Position",
            gridcolor = "#404040",
            gridwidth = 1,
            dtick = 5,
            showgrid = TRUE,
            range = c(0, 41),
            color = "#FFFFFF"
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = rev(ordered_drivers),
            color = "#FFFFFF"
          ),
          paper_bgcolor = "#121212",
          plot_bgcolor = "#1e1e1e",
          font = list(color = "#FFFFFF", size = 12),
          showlegend = FALSE,
          height = plot_height,  # Dynamic height based on driver count
          margin = list(l = 150, r = 50, t = 50, b = 50)
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
          displaylogo = FALSE
        )
      
      return(p)
      
    }, error = function(e) {
      cat("Error in finish_distribution_plot:", e$message, "\n")
      plotly_empty() %>%
        layout(
          title = list(text = paste("Error:", e$message), font = list(color = "#FFE500")),
          paper_bgcolor = "#121212",
          plot_bgcolor = "#1e1e1e"
        )
    })
  })
  
  # ============================================================================
  # CHART 2: DOMINATOR BY DRIVER - BOX AND WHISKER
  # Replace output$dominator_violin_driver (around line 1920)
  # ============================================================================
  
  output$dominator_violin_driver <- renderPlotly({
    req(rv$sport == "NASCAR", rv$full_sim_results, input$sim_results_platform)
    
    if (is.null(rv$full_sim_results) || nrow(rv$full_sim_results) == 0) {
      return(plotly_empty() %>% 
               layout(
                 title = list(text = "No simulation data available", font = list(color = "#FFE500")),
                 paper_bgcolor = "#121212",
                 plot_bgcolor = "#1e1e1e"
               ))
    }
    
    tryCatch({
      # Get platform-specific dominator column
      dom_col <- if (input$sim_results_platform == "DK") "DKDominatorPoints" else "FDDominatorPoints"
      
      # Get top 15 drivers by average dominator points
      driver_avg <- rv$full_sim_results[, .(AvgDom = mean(get(dom_col))), by = Name]
      setorder(driver_avg, -AvgDom)
      top_drivers <- head(driver_avg$Name, 15)
      
      # Filter data to top 15
      plot_data <- rv$full_sim_results[Name %in% top_drivers]
      
      # Calculate median for ordering
      driver_medians <- plot_data[, .(Median = median(get(dom_col))), by = Name]
      setorder(driver_medians, -Median)
      
      # Convert to data.frame
      plot_data <- as.data.frame(plot_data)
      plot_data$Name <- factor(plot_data$Name, levels = rev(driver_medians$Name))
      plot_data$DomPoints <- plot_data[[dom_col]]
      
      # Create box plot - GOLD COLOR, NO VIOLIN
      p <- plot_ly(
        data = plot_data,
        x = ~DomPoints,
        y = ~Name,
        type = "box",
        orientation = "h",
        marker = list(color = "#FFE500"),
        line = list(color = "#FFE500"),
        fillcolor = "rgba(255, 229, 0, 0.3)",
        hovertemplate = paste(
          "<b>%{y}</b><br>",
          "Median: %{x}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = list(
            text = paste(input$sim_results_platform, "Dominator Points - Top 15 Drivers"),
            font = list(color = "#FFE500", size = 16)
          ),
          xaxis = list(
            title = "Dominator Points",
            gridcolor = "#404040",
            gridwidth = 1,
            showgrid = TRUE,
            color = "#FFFFFF"
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = rev(driver_medians$Name),
            color = "#FFFFFF"
          ),
          paper_bgcolor = "#121212",
          plot_bgcolor = "#1e1e1e",
          font = list(color = "#FFFFFF", size = 12),
          showlegend = FALSE,
          height = 600,
          margin = list(l = 150, r = 50, t = 50, b = 50)
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
          displaylogo = FALSE
        )
      
      return(p)
      
    }, error = function(e) {
      cat("Error in dominator_violin_driver:", e$message, "\n")
      plotly_empty() %>%
        layout(
          title = list(text = paste("Error:", e$message), font = list(color = "#FFE500")),
          paper_bgcolor = "#121212",
          plot_bgcolor = "#1e1e1e"
        )
    })
  })
  
  # Dominator Violin by Position
  output$dominator_violin_position <- renderPlotly({
    req(rv$sport == "NASCAR", rv$full_sim_results, input$sim_results_platform)
    
    # Validate data exists
    if (is.null(rv$full_sim_results) || nrow(rv$full_sim_results) == 0) {
      return(plotly_empty() %>% 
               layout(
                 title = list(text = "No simulation data available", font = list(color = "#FFE500")),
                 paper_bgcolor = "#121212",
                 plot_bgcolor = "#1e1e1e"
               ))
    }
    
    tryCatch({
      # Call NASCAR engine function
      create_dominator_violin_by_position(
        rv$full_sim_results,
        platform = input$sim_results_platform,
        group_by = input$dominator_position_group
      )
    }, error = function(e) {
      cat("Error in dominator_violin_position:", e$message, "\n")
      plotly_empty() %>%
        layout(
          title = list(text = paste("Error:", e$message), font = list(color = "#FFE500")),
          paper_bgcolor = "#121212",
          plot_bgcolor = "#1e1e1e"
        )
    })
  })
  
  # Update driver filter choices when simulation loads
  observe({
    req(rv$sport_name == "NASCAR", rv$sim_metadata)
    
    # Get unique drivers sorted by name
    drivers <- sort(unique(rv$sim_metadata$Player))
    
  })
  
  
  # ==========================================================================
  # PORTFOLIO BUILDER - DRAFTKINGS
  # ==========================================================================
  
  # Dynamic range sliders - DK (2-column layout)
  output$dk_range_sliders <- renderUI({
    req(rv$dk_optimal_lineups)
    
    lineups <- rv$dk_optimal_lineups
    
    # Get numeric columns (excluding Player columns and rate columns)
    numeric_cols <- names(lineups)[sapply(lineups, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, grep("^Player", names(lineups), value = TRUE))
    
    # Filter to only range columns (exclude Win/Top rates)
    range_cols <- setdiff(numeric_cols, c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"))
    
    # Define friendly labels and formatting
    slider_config <- list(
      TotalSalary = list(label = "Salary", format = "k", step = 0.1),
      CumulativeOwnership = list(label = "Total Own", format = "whole", step = 1),
      GeometricMeanOwnership = list(label = "Avg Own", format = "decimal", step = 0.1),
      CumulativeStarting = list(label = "Total Start", format = "whole", step = 1),
      GeometricMeanStarting = list(label = "Avg Start", format = "decimal", step = 0.1)
    )
    
    # Create sliders
    sliders <- lapply(range_cols, function(col_name) {
      config <- slider_config[[col_name]]
      if (is.null(config)) {
        config <- list(label = col_name, format = "decimal", step = 0.1)
      }
      
      min_val <- min(lineups[[col_name]], na.rm = TRUE)
      max_val <- max(lineups[[col_name]], na.rm = TRUE)
      
      # Format based on type
      if (config$format == "k") {
        # Salary in thousands
        min_val <- floor(min_val / 1000)
        max_val <- ceiling(max_val / 1000)
        label <- paste0(config$label, " (K)")
      } else if (config$format == "whole") {
        # Whole numbers
        min_val <- floor(min_val)
        max_val <- ceiling(max_val)
        label <- config$label
      } else {
        # Decimal
        min_val <- floor(min_val * 10) / 10
        max_val <- ceiling(max_val * 10) / 10
        label <- config$label
      }
      
      sliderInput(
        paste0("dk_filter_", col_name),
        label,
        min = min_val,
        max = max_val,
        value = c(min_val, max_val),
        step = config$step,
        width = "100%"
      )
    })
    
    # Arrange in 2 columns
    num_sliders <- length(sliders)
    col1_sliders <- sliders[seq(1, num_sliders, 2)]
    col2_sliders <- if(num_sliders > 1) sliders[seq(2, num_sliders, 2)] else list()
    
    fluidRow(
      column(6, col1_sliders),
      column(6, col2_sliders)
    )
  })
  
  # Dynamic locked players UI - DK
  output$dk_locked_ui <- renderUI({
    req(rv$dk_optimal_lineups)
    
    player_cols <- grep("^Player", names(rv$dk_optimal_lineups), value = TRUE)
    all_players <- unique(unlist(rv$dk_optimal_lineups[, ..player_cols]))
    all_players <- all_players[!is.na(all_players) & all_players != ""]
    
    selectizeInput(
      "dk_locked_players",
      "Locked",
      choices = sort(all_players),
      multiple = TRUE,
      options = list(
        placeholder = "Select players",
        closeAfterSelect = TRUE,
        hideSelected = FALSE,
        maxOptions = 100,
        onDropdownClose = htmlwidgets::JS("function() { this.blur(); }")
      )
    )
  })
  
  # Dynamic excluded players UI - DK
  output$dk_excluded_ui <- renderUI({
    req(rv$dk_optimal_lineups)
    
    player_cols <- grep("^Player", names(rv$dk_optimal_lineups), value = TRUE)
    all_players <- unique(unlist(rv$dk_optimal_lineups[, ..player_cols]))
    all_players <- all_players[!is.na(all_players) & all_players != ""]
    
    selectizeInput(
      "dk_excluded_players",
      "Excluded",
      choices = sort(all_players),
      multiple = TRUE,
      options = list(
        placeholder = "Select players",
        closeAfterSelect = TRUE,
        hideSelected = FALSE,
        maxOptions = 100,
        onDropdownClose = htmlwidgets::JS("function() { this.blur(); }")
      )
    )
  })
  
  # Update player choices - DK (simplified - no longer needed with dynamic UI)
  # The UI itself renders with the choices directly
  
  # Filtered lineups - DK (EXACT copy of working pattern)
  # Filtered lineups - DK
  dk_filtered_lineups <- reactive({
    req(rv$dk_optimal_lineups)
    
    lineups <- copy(rv$dk_optimal_lineups)
    
    # Apply rate minimum filters (hardcoded in UI)
    if (!is.null(input$dk_min_win)) {
      lineups <- lineups[WinRate >= input$dk_min_win]
    }
    if (!is.null(input$dk_min_top1)) {
      lineups <- lineups[Top1Pct >= input$dk_min_top1]
    }
    if (!is.null(input$dk_min_top5)) {
      lineups <- lineups[Top5Pct >= input$dk_min_top5]
    }
    if (!is.null(input$dk_min_top10)) {
      lineups <- lineups[Top10Pct >= input$dk_min_top10]
    }
    if (!is.null(input$dk_min_top20)) {
      lineups <- lineups[Top20Pct >= input$dk_min_top20]
    }
    
    # Apply range filters (dynamic sliders)
    # Handle TotalSalary specially (displayed in K, needs conversion back)
    if (!is.null(input$dk_filter_TotalSalary)) {
      lineups <- lineups[TotalSalary >= input$dk_filter_TotalSalary[1] * 1000 & 
                           TotalSalary <= input$dk_filter_TotalSalary[2] * 1000]
    }
    
    # Apply other range filters
    numeric_cols <- names(lineups)[sapply(lineups, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, grep("^Player", names(lineups), value = TRUE))
    range_cols <- setdiff(numeric_cols, c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct", "TotalSalary"))
    
    for (col_name in range_cols) {
      filter_input <- input[[paste0("dk_filter_", col_name)]]
      if (!is.null(filter_input)) {
        lineups <- lineups[get(col_name) >= filter_input[1] & get(col_name) <= filter_input[2]]
      }
    }
    
    # Apply locked players
    if (!is.null(input$dk_locked_players) && length(input$dk_locked_players) > 0) {
      locked <- input$dk_locked_players[input$dk_locked_players != ""]
      if (length(locked) > 0) {
        player_cols <- grep("^Player", names(lineups), value = TRUE)
        lineups <- lineups[apply(lineups[, ..player_cols], 1, function(row) {
          all(locked %in% row)
        })]
      }
    }
    
    # Apply excluded players
    if (!is.null(input$dk_excluded_players) && length(input$dk_excluded_players) > 0) {
      excluded <- input$dk_excluded_players[input$dk_excluded_players != ""]
      if (length(excluded) > 0) {
        player_cols <- grep("^Player", names(lineups), value = TRUE)
        lineups <- lineups[apply(lineups[, ..player_cols], 1, function(row) {
          !any(excluded %in% row)
        })]
      }
    }
    
    return(lineups)
  })
  
  # Filtered count
  output$dk_filtered_count <- renderText({
    filtered <- dk_filtered_lineups()
    paste0("Filtered Pool: ", nrow(filtered), " lineups")
  })
  
  # Filtered exposure - DK
  output$dk_filtered_exposure <- renderDT({
    req(dk_filtered_lineups(), rv$sim_metadata)
    
    filtered <- dk_filtered_lineups()
    player_cols <- grep("^Player", names(filtered), value = TRUE)
    
    # Count exposure for players in filtered pool
    all_players <- unlist(filtered[, ..player_cols])
    exposure_counts <- table(all_players)
    
    # Start with ALL players from metadata
    exposure_table <- data.table(Player = rv$sim_metadata$Player)
    
    # Add counts (0 for players not in filtered pool)
    exposure_table[, Exposure := 0]
    for (i in 1:nrow(exposure_table)) {
      player <- exposure_table$Player[i]
      if (player %in% names(exposure_counts)) {
        exposure_table$Exposure[i] <- (as.numeric(exposure_counts[player]) / nrow(filtered)) * 100
      }
    }
    
    # Add metadata: Salary, Starting, Team, Car, Own, Leverage
    exposure_table <- merge(
      exposure_table,
      rv$sim_metadata[, .(Player, DKSalary, Starting, Team, Car, DKOwn)],
      by = "Player",
      all.x = TRUE
    )
    
    # Rename and format
    setnames(exposure_table, c("DKSalary", "DKOwn"), c("Salary", "OwnProj"))
    exposure_table[, OwnProj := OwnProj * 100]  # Multiply by 100
    exposure_table[, Leverage := round(Exposure - OwnProj, 1)]
    
    # Reorder columns
    setcolorder(exposure_table, c("Player", "Salary", "Starting", "Team", "Car", "Exposure", "OwnProj", "Leverage"))
    
    datatable(
      exposure_table,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        dom = 'tp',
        order = list(list(which(names(exposure_table) == "Exposure") - 1, 'desc'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("Exposure", "OwnProj", "Leverage"), 1) %>%
      formatCurrency("Salary", "$", digits = 0)
  })
  
  # Add build - DK
  observeEvent(input$dk_add_build, {
    req(dk_filtered_lineups())
    
    filtered <- dk_filtered_lineups()
    num_lineups <- input$dk_num_lineups
    
    if (nrow(filtered) < num_lineups) {
      showNotification(
        paste0("Only ", nrow(filtered), " lineups available. Cannot generate ", num_lineups),
        type = "warning"
      )
      return()
    }
    
    sampled <- filtered[sample(nrow(filtered), num_lineups)]
    
    rv$dk_build_counter <- rv$dk_build_counter + 1
    build_label <- if (input$dk_build_label == "") {
      paste0("Build ", rv$dk_build_counter)
    } else {
      input$dk_build_label
    }
    
    sampled[, Build := build_label]
    
    if (is.null(rv$dk_portfolio)) {
      rv$dk_portfolio <- sampled
    } else {
      rv$dk_portfolio <- rbindlist(list(rv$dk_portfolio, sampled), fill = TRUE)
    }
    
    # Store build with filter settings
    filter_parts <- c()
    
    # Rate minimums (only show if > 0)
    if (!is.null(input$dk_min_win) && input$dk_min_win > 0) {
      filter_parts <- c(filter_parts, paste0("Win≥", input$dk_min_win))
    }
    if (!is.null(input$dk_min_top1) && input$dk_min_top1 > 0) {
      filter_parts <- c(filter_parts, paste0("Top1≥", input$dk_min_top1))
    }
    if (!is.null(input$dk_min_top5) && input$dk_min_top5 > 0) {
      filter_parts <- c(filter_parts, paste0("Top5≥", input$dk_min_top5))
    }
    if (!is.null(input$dk_min_top10) && input$dk_min_top10 > 0) {
      filter_parts <- c(filter_parts, paste0("Top10≥", input$dk_min_top10))
    }
    if (!is.null(input$dk_min_top20) && input$dk_min_top20 > 0) {
      filter_parts <- c(filter_parts, paste0("Top20≥", input$dk_min_top20))
    }
    
    # Salary range (only if not full range)
    if (!is.null(input$dk_filter_TotalSalary)) {
      lineups <- rv$dk_optimal_lineups
      min_possible <- floor(min(lineups$TotalSalary) / 1000)
      max_possible <- ceiling(max(lineups$TotalSalary) / 1000)
      
      if (input$dk_filter_TotalSalary[1] > min_possible || input$dk_filter_TotalSalary[2] < max_possible) {
        filter_parts <- c(filter_parts, paste0("Salary:", input$dk_filter_TotalSalary[1], "-", input$dk_filter_TotalSalary[2], "K"))
      }
    }
    
    # Locked players
    if (length(input$dk_locked_players) > 0) {
      if (length(input$dk_locked_players) <= 3) {
        filter_parts <- c(filter_parts, paste0("Locked:", paste(input$dk_locked_players, collapse = ",")))
      } else {
        filter_parts <- c(filter_parts, paste0("Locked:", length(input$dk_locked_players), " players"))
      }
    }
    
    # Excluded players
    if (length(input$dk_excluded_players) > 0) {
      if (length(input$dk_excluded_players) <= 3) {
        filter_parts <- c(filter_parts, paste0("Excl:", paste(input$dk_excluded_players, collapse = ",")))
      } else {
        filter_parts <- c(filter_parts, paste0("Excl:", length(input$dk_excluded_players), " players"))
      }
    }
    
    filter_summary <- if (length(filter_parts) > 0) {
      paste(filter_parts, collapse = " | ")
    } else {
      "No filters"
    }
    
    rv$dk_builds[[build_label]] <- list(
      label = build_label,
      num_lineups = num_lineups,
      filters = filter_summary
    )
    
    showNotification(
      paste0("Added ", num_lineups, " lineups to portfolio as '", build_label, "'"),
      type = "message"
    )
    
    updateTextInput(session, "dk_build_label", value = "")
  })
  
  # Clear portfolio
  observeEvent(input$dk_clear_portfolio, {
    rv$dk_portfolio <- NULL
    rv$dk_builds <- list()
    rv$dk_build_counter <- 0
    showNotification("DK Portfolio cleared", type = "message")
  })
  
  # Delete single build - triggered by JavaScript
  observeEvent(input$dk_delete_build, {
    req(input$dk_delete_build)
    
    build_to_delete <- input$dk_delete_build
    
    if (build_to_delete %in% names(rv$dk_builds)) {
      # Remove lineups for this build from portfolio
      if (!is.null(rv$dk_portfolio)) {
        rv$dk_portfolio <- rv$dk_portfolio[Build != build_to_delete]
        
        # If portfolio is now empty, clear it
        if (nrow(rv$dk_portfolio) == 0) {
          rv$dk_portfolio <- NULL
        }
      }
      
      # Remove build from builds list
      rv$dk_builds[[build_to_delete]] <- NULL
      
      showNotification(
        paste0("Deleted build: ", build_to_delete),
        type = "message"
      )
    }
  })
  
  # Delete single build
  observeEvent(input$delete_build_name, {
    req(input$delete_build_name, rv$dk_portfolio, rv$dk_builds)
    
    build_to_delete <- input$delete_build_name
    
    # Remove lineups with this build label
    rv$dk_portfolio <- rv$dk_portfolio[Build != build_to_delete]
    
    # Remove from builds list
    rv$dk_builds[[build_to_delete]] <- NULL
    
    # If portfolio is now empty, reset
    if (nrow(rv$dk_portfolio) == 0) {
      rv$dk_portfolio <- NULL
      rv$dk_builds <- list()
      rv$dk_build_counter <- 0
    }
    
    showNotification(
      paste0("Deleted build: ", build_to_delete),
      type = "warning"
    )
  })
  
  # Delete individual lineup
  observeEvent(input$delete_lineup_row, {
    req(input$delete_lineup_row, rv$dk_portfolio)
    
    row_to_delete <- as.integer(input$delete_lineup_row)
    
    # Get the build name before deleting
    deleted_build <- rv$dk_portfolio[row_to_delete, Build]
    
    # Remove the row
    rv$dk_portfolio <- rv$dk_portfolio[-row_to_delete]
    
    # Update builds list - decrement count or remove if no lineups left
    if (deleted_build %in% names(rv$dk_builds)) {
      rv$dk_builds[[deleted_build]]$num_lineups <- rv$dk_builds[[deleted_build]]$num_lineups - 1
      
      # If build has no more lineups, remove it
      if (rv$dk_builds[[deleted_build]]$num_lineups == 0) {
        rv$dk_builds[[deleted_build]] <- NULL
      }
    }
    
    # If portfolio is now empty, reset
    if (nrow(rv$dk_portfolio) == 0) {
      rv$dk_portfolio <- NULL
      rv$dk_builds <- list()
      rv$dk_build_counter <- 0
    }
    
    showNotification("Deleted lineup from portfolio", type = "warning")
  })
  
  # Portfolio count
  output$dk_portfolio_count <- renderText({
    if (is.null(rv$dk_portfolio)) {
      "Portfolio: 0 lineups"
    } else {
      paste0("Portfolio: ", nrow(rv$dk_portfolio), " lineups across ", length(rv$dk_builds), " builds")
    }
  })
  
  # Builds summary - DK
  output$dk_builds_summary <- renderDT({
    req(rv$dk_builds)
    
    builds_df <- data.table(
      Build = names(rv$dk_builds),
      Lineups = sapply(rv$dk_builds, function(b) b$num_lineups),
      Filters = sapply(rv$dk_builds, function(b) b$filters),
      Delete = paste0('<button class="btn btn-danger btn-xs delete-build" data-build="', names(rv$dk_builds), '">Delete</button>')
    )
    
    datatable(
      builds_df,
      options = list(
        dom = 't',
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE
      ),
      selection = list(mode = 'multiple', target = 'row'),
      escape = FALSE,  # Allow HTML in Delete column
      rownames = FALSE
    )
  })
  
  # Portfolio exposure - DK (with selected builds comparison)
  output$dk_portfolio_exposure <- renderDT({
    req(rv$dk_portfolio, rv$sim_metadata)
    
    player_cols <- grep("^Player", names(rv$dk_portfolio), value = TRUE)
    
    # Calculate full portfolio exposure
    all_players <- unlist(rv$dk_portfolio[, ..player_cols])
    exposure_counts <- table(all_players)
    
    # Start with ALL players
    exposure_table <- data.table(Player = rv$sim_metadata$Player)
    
    # Add full portfolio exposure
    exposure_table[, Exposure := 0]
    for (i in 1:nrow(exposure_table)) {
      player <- exposure_table$Player[i]
      if (player %in% names(exposure_counts)) {
        exposure_table$Exposure[i] <- (as.numeric(exposure_counts[player]) / nrow(rv$dk_portfolio)) * 100
      }
    }
    
    # If builds are selected in the table, show comparison columns
    if (!is.null(input$dk_builds_summary_rows_selected) && length(input$dk_builds_summary_rows_selected) > 0) {
      # Get selected build names
      builds_df <- data.table(
        Build = names(rv$dk_builds),
        Lineups = sapply(rv$dk_builds, function(b) b$num_lineups)
      )
      selected_builds <- builds_df$Build[input$dk_builds_summary_rows_selected]
      
      # Calculate exposure WITH selected builds
      with_selected <- rv$dk_portfolio[Build %in% selected_builds]
      if (nrow(with_selected) > 0) {
        with_players <- unlist(with_selected[, ..player_cols])
        with_counts <- table(with_players)
        
        exposure_table[, With_Selected := 0]
        for (i in 1:nrow(exposure_table)) {
          player <- exposure_table$Player[i]
          if (player %in% names(with_counts)) {
            exposure_table$With_Selected[i] <- (as.numeric(with_counts[player]) / nrow(with_selected)) * 100
          }
        }
      }
      
      # Calculate exposure WITHOUT selected builds
      without_selected <- rv$dk_portfolio[!Build %in% selected_builds]
      if (nrow(without_selected) > 0) {
        without_players <- unlist(without_selected[, ..player_cols])
        without_counts <- table(without_players)
        
        exposure_table[, Without_Selected := 0]
        for (i in 1:nrow(exposure_table)) {
          player <- exposure_table$Player[i]
          if (player %in% names(without_counts)) {
            exposure_table$Without_Selected[i] <- (as.numeric(without_counts[player]) / nrow(without_selected)) * 100
          }
        }
      } else {
        exposure_table[, Without_Selected := 0]
      }
      
      # Calculate difference
      exposure_table[, Diff := With_Selected - Without_Selected]
    }
    
    # Add metadata - only get columns that exist
    metadata_cols <- c("Player", "DKSalary", "DKOwn")
    
    # Add sport-specific columns if they exist
    sport_cols <- c("Starting", "Team", "Car")
    available_sport_cols <- intersect(sport_cols, names(rv$sim_metadata))
    metadata_cols <- c(metadata_cols, available_sport_cols)
    
    exposure_table <- merge(
      exposure_table,
      rv$sim_metadata[, ..metadata_cols],
      by = "Player",
      all.x = TRUE
    )
    
    setnames(exposure_table, c("DKSalary", "DKOwn"), c("Salary", "OwnProj"))
    exposure_table[, OwnProj := OwnProj * 100]
    exposure_table[, Leverage := round(Exposure - OwnProj, 1)]
    
    # Set column order - only include columns that exist
    base_cols <- c("Player", "Salary")
    sport_specific_cols <- intersect(c("Starting", "Team", "Car"), names(exposure_table))
    metric_cols <- c("Exposure")
    
    if ("With_Selected" %in% names(exposure_table)) {
      metric_cols <- c(metric_cols, "With_Selected", "Without_Selected", "Diff")
    }
    
    metric_cols <- c(metric_cols, "OwnProj", "Leverage")
    
    final_col_order <- c(base_cols, sport_specific_cols, metric_cols)
    setcolorder(exposure_table, final_col_order)
    
    # Format table
    dt <- datatable(
      exposure_table,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        dom = 'tp'
      ),
      rownames = FALSE
    ) %>%
      formatCurrency("Salary", "$", digits = 0)
    
    # Format numeric columns
    if ("With_Selected" %in% names(exposure_table)) {
      dt <- dt %>%
        formatRound(c("Exposure", "With_Selected", "Without_Selected", "Diff", "OwnProj", "Leverage"), 1) %>%
        formatStyle(
          'Diff',
          color = styleInterval(c(-0.1, 0.1), c('red', 'white', 'green'))
        )
    } else {
      dt <- dt %>%
        formatRound(c("Exposure", "OwnProj", "Leverage"), 1)
    }
    
    dt
  })
  
  output$dk_portfolio_lineups <- renderDT({
    req(rv$dk_portfolio, rv$config)
    
    display_table <- create_portfolio_display_table(rv$dk_portfolio, rv$config)
    
    # Safer format columns detection
    format_cols <- tryCatch({
      cols <- get_format_columns(display_table, rv$config)
      # Filter to only columns that exist
      cols[cols %in% names(display_table)]
    }, error = function(e) {
      # If get_format_columns fails, find numeric columns manually
      numeric_cols <- names(display_table)[sapply(display_table, is.numeric)]
      # Exclude Salary and RowID
      numeric_cols[!numeric_cols %in% c("Salary", "RowID")]
    })
    
    # Only format if we have columns to format
    dt_output <- datatable(
      display_table[, -"RowID"],
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        dom = "tp",
        columnDefs = list(
          list(width = "30px", targets = 0)
        )
      ),
      escape = FALSE,
      rownames = FALSE
    )
    
    # Apply formatting only if columns exist
    if (length(format_cols) > 0) {
      dt_output <- dt_output %>% formatRound(format_cols, 1)
    }
    
    if ("Salary" %in% names(display_table)) {
      dt_output <- dt_output %>% formatCurrency("Salary", "$", digits = 0)
    }
    
    dt_output
  })
  
  # Download portfolio
  output$dk_download_portfolio <- downloadHandler(
    filename = function() {
      paste0("DK_Portfolio_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv$dk_portfolio)
      
      download_table <- copy(rv$dk_portfolio)
      player_cols <- grep("^Player", names(download_table), value = TRUE)
      
      for (col in player_cols) {
        players <- download_table[[col]]
        ids <- rv$sim_metadata[match(players, rv$sim_metadata$Player), DKID]
        download_table[[col]] <- paste0(players, " (", ids, ")")
      }
      
      fwrite(download_table, file)
    }
  )
  
  
  # ==========================================================================
  # PORTFOLIO BUILDER - FANDUEL
  # ==========================================================================
  
  # Dynamic locked players UI - FD
  output$fd_locked_ui <- renderUI({
    req(rv$fd_optimal_lineups)
    
    player_cols <- grep("^Player", names(rv$fd_optimal_lineups), value = TRUE)
    all_players <- unique(unlist(rv$fd_optimal_lineups[, ..player_cols]))
    all_players <- all_players[!is.na(all_players) & all_players != ""]
    
    selectizeInput(
      "fd_locked_players",
      "Locked",
      choices = sort(all_players),
      multiple = TRUE,
      options = list(placeholder = 'Select players')
    )
  })
  
  # Dynamic excluded players UI - FD
  output$fd_excluded_ui <- renderUI({
    req(rv$fd_optimal_lineups)
    
    player_cols <- grep("^Player", names(rv$fd_optimal_lineups), value = TRUE)
    all_players <- unique(unlist(rv$fd_optimal_lineups[, ..player_cols]))
    all_players <- all_players[!is.na(all_players) & all_players != ""]
    
    selectizeInput(
      "fd_excluded_players",
      "Excluded",
      choices = sort(all_players),
      multiple = TRUE,
      options = list(placeholder = 'Select players')
    )
  })
  
  # Dynamic range sliders - FD (2-column layout matching DK)
  output$fd_range_sliders <- renderUI({
    req(rv$fd_optimal_lineups)
    
    lineups <- rv$fd_optimal_lineups
    
    # Get numeric columns (excluding Player columns and rate columns)
    numeric_cols <- names(lineups)[sapply(lineups, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, grep("^Player", names(lineups), value = TRUE))
    
    # Filter to only range columns (exclude Win/Top rates)
    range_cols <- setdiff(numeric_cols, c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"))
    
    # Define friendly labels and formatting
    slider_config <- list(
      TotalSalary = list(label = "Salary", format = "k", step = 0.1),
      CumulativeOwnership = list(label = "Total Own", format = "whole", step = 1),
      GeometricMeanOwnership = list(label = "Avg Own", format = "decimal", step = 0.1),
      CumulativeStarting = list(label = "Total Start", format = "whole", step = 1),
      GeometricMeanStarting = list(label = "Avg Start", format = "decimal", step = 0.1)
    )
    
    # Create sliders
    sliders <- lapply(range_cols, function(col_name) {
      config <- slider_config[[col_name]]
      if (is.null(config)) {
        config <- list(label = col_name, format = "decimal", step = 0.1)
      }
      
      min_val <- min(lineups[[col_name]], na.rm = TRUE)
      max_val <- max(lineups[[col_name]], na.rm = TRUE)
      
      # Format based on type
      if (config$format == "k") {
        min_val <- floor(min_val / 1000)
        max_val <- ceiling(max_val / 1000)
        label <- paste0(config$label, " (K)")
      } else if (config$format == "whole") {
        min_val <- floor(min_val)
        max_val <- ceiling(max_val)
        label <- config$label
      } else {
        min_val <- floor(min_val * 10) / 10
        max_val <- ceiling(max_val * 10) / 10
        label <- config$label
      }
      
      sliderInput(
        paste0("fd_filter_", col_name),
        label,
        min = min_val,
        max = max_val,
        value = c(min_val, max_val),
        step = config$step,
        width = "100%"
      )
    })
    
    # Arrange in 2 columns
    num_sliders <- length(sliders)
    col1_sliders <- sliders[seq(1, num_sliders, 2)]
    col2_sliders <- if(num_sliders > 1) sliders[seq(2, num_sliders, 2)] else list()
    
    fluidRow(
      column(6, col1_sliders),
      column(6, col2_sliders)
    )
  })
  
  observe({
    req(rv$fd_optimal_lineups)
    
    tryCatch({
      cat("\n=== FD Player Dropdown Update ===\n")
      player_cols <- grep("^Player", names(rv$fd_optimal_lineups), value = TRUE)
      
      if (length(player_cols) > 0) {
        all_players_list <- lapply(player_cols, function(col) {
          players <- rv$fd_optimal_lineups[[col]]
          cleaned <- gsub(" \\([0-9]+\\)$", "", players)
          cleaned
        })
        all_players <- unique(unlist(all_players_list))
        all_players <- all_players[!is.na(all_players) & all_players != ""]
        
        cat("Found", length(all_players), "unique players\n")
        
        if (length(all_players) > 0) {
          sorted_players <- sort(all_players)
          updateSelectInput(session, "fd_locked_players", choices = sorted_players)
          updateSelectInput(session, "fd_excluded_players", choices = sorted_players)
          cat("✓ Updated FD dropdowns with", length(sorted_players), "players\n")
        }
      }
      cat("=================================\n\n")
    }, error = function(e) {
      cat("✗ Error updating FD player choices:", e$message, "\n")
    })
  })
  
  fd_filtered_lineups <- reactive({
    req(rv$fd_optimal_lineups)
    
    lineups <- copy(rv$fd_optimal_lineups)
    
    # Apply rate minimum filters (hardcoded in UI)
    if (!is.null(input$fd_min_win)) {
      lineups <- lineups[WinRate >= input$fd_min_win]
    }
    if (!is.null(input$fd_min_top1)) {
      lineups <- lineups[Top1Pct >= input$fd_min_top1]
    }
    if (!is.null(input$fd_min_top5)) {
      lineups <- lineups[Top5Pct >= input$fd_min_top5]
    }
    if (!is.null(input$fd_min_top10)) {
      lineups <- lineups[Top10Pct >= input$fd_min_top10]
    }
    if (!is.null(input$fd_min_top20)) {
      lineups <- lineups[Top20Pct >= input$fd_min_top20]
    }
    
    # Apply range filters (dynamic sliders)
    # Handle TotalSalary specially (displayed in K, needs conversion back)
    if (!is.null(input$fd_filter_TotalSalary)) {
      lineups <- lineups[TotalSalary >= input$fd_filter_TotalSalary[1] * 1000 & 
                           TotalSalary <= input$fd_filter_TotalSalary[2] * 1000]
    }
    
    # Apply other range filters
    numeric_cols <- names(lineups)[sapply(lineups, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, grep("^Player", names(lineups), value = TRUE))
    range_cols <- setdiff(numeric_cols, c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct", "TotalSalary"))
    
    for (col_name in range_cols) {
      filter_input <- input[[paste0("fd_filter_", col_name)]]
      if (!is.null(filter_input)) {
        lineups <- lineups[get(col_name) >= filter_input[1] & get(col_name) <= filter_input[2]]
      }
    }
    
    # Apply locked players (strip IDs for comparison)
    if (length(input$fd_locked_players) > 0) {
      player_cols <- grep("^Player", names(lineups), value = TRUE)
      lineups <- lineups[apply(lineups[, ..player_cols], 1, function(row) {
        cleaned_row <- gsub(" \\([0-9]+\\)$", "", row)
        all(input$fd_locked_players %in% cleaned_row)
      })]
    }
    
    # Apply excluded players (strip IDs for comparison)
    if (length(input$fd_excluded_players) > 0) {
      player_cols <- grep("^Player", names(lineups), value = TRUE)
      lineups <- lineups[apply(lineups[, ..player_cols], 1, function(row) {
        cleaned_row <- gsub(" \\([0-9]+\\)$", "", row)
        !any(input$fd_excluded_players %in% cleaned_row)
      })]
    }
    
    return(lineups)
  })
  
  output$fd_filtered_count <- renderText({
    filtered <- fd_filtered_lineups()
    paste0("Filtered Pool: ", nrow(filtered), " lineups")
  })
  
  output$fd_filtered_exposure <- renderDT({
    req(fd_filtered_lineups(), rv$sim_metadata)
    
    filtered <- fd_filtered_lineups()
    player_cols <- grep("^Player", names(filtered), value = TRUE)
    
    all_players <- unlist(filtered[, ..player_cols])
    exposure_counts <- table(all_players)
    
    exposure_table <- data.table(Player = rv$sim_metadata$Player)
    
    exposure_table[, Exposure := 0]
    for (i in 1:nrow(exposure_table)) {
      player <- exposure_table$Player[i]
      if (player %in% names(exposure_counts)) {
        exposure_table$Exposure[i] <- (as.numeric(exposure_counts[player]) / nrow(filtered)) * 100
      }
    }
    
    exposure_table <- merge(
      exposure_table,
      rv$sim_metadata[, .(Player, FDSalary, Starting, Team, Car, FDOwn)],
      by = "Player",
      all.x = TRUE
    )
    
    setnames(exposure_table, c("FDSalary", "FDOwn"), c("Salary", "OwnProj"))
    exposure_table[, OwnProj := OwnProj * 100]
    exposure_table[, Leverage := round(Exposure - OwnProj, 1)]
    
    setcolorder(exposure_table, c("Player", "Salary", "Starting", "Team", "Car", "Exposure", "OwnProj", "Leverage"))
    
    datatable(
      exposure_table,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        dom = 'tp'
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("Exposure", "OwnProj", "Leverage"), 1) %>%
      formatCurrency("Salary", "$", digits = 0)
  })
  
  observeEvent(input$fd_add_build, {
    req(fd_filtered_lineups())
    
    filtered <- fd_filtered_lineups()
    num_lineups <- input$fd_num_lineups
    
    if (nrow(filtered) < num_lineups) {
      showNotification(
        paste0("Only ", nrow(filtered), " lineups available. Cannot generate ", num_lineups),
        type = "warning"
      )
      return()
    }
    
    sampled <- filtered[sample(nrow(filtered), num_lineups)]
    
    rv$fd_build_counter <- rv$fd_build_counter + 1
    build_label <- if (input$fd_build_label == "") {
      paste0("Build ", rv$fd_build_counter)
    } else {
      input$fd_build_label
    }
    
    sampled[, Build := build_label]
    
    if (is.null(rv$fd_portfolio)) {
      rv$fd_portfolio <- sampled
    } else {
      rv$fd_portfolio <- rbindlist(list(rv$fd_portfolio, sampled), fill = TRUE)
    }
    
    rv$fd_builds[[build_label]] <- list(
      label = build_label,
      num_lineups = num_lineups,
      timestamp = Sys.time()
    )
    
    showNotification(
      paste0("Added ", num_lineups, " lineups to portfolio as '", build_label, "'"),
      type = "message"
    )
    
    updateTextInput(session, "fd_build_label", value = "")
  })
  
  observeEvent(input$fd_clear_portfolio, {
    rv$fd_portfolio <- NULL
    rv$fd_builds <- list()
    rv$fd_build_counter <- 0
    showNotification("FD Portfolio cleared", type = "message")
  })
  
  output$fd_portfolio_count <- renderText({
    if (is.null(rv$fd_portfolio)) {
      "Portfolio: 0 lineups"
    } else {
      paste0("Portfolio: ", nrow(rv$fd_portfolio), " lineups across ", length(rv$fd_builds), " builds")
    }
  })
  
  # Portfolio overview - FD
  output$fd_builds_summary <- renderDT({
    req(rv$fd_builds)
    
    builds_df <- data.table(
      Build = names(rv$fd_builds),
      Lineups = sapply(rv$fd_builds, function(b) b$num_lineups),
      Filters = sapply(rv$fd_builds, function(b) b$filters),
      Delete = paste0('<button class="btn btn-danger btn-xs delete-build" data-build="', names(rv$fd_builds), '">Delete</button>')
    )
    
    datatable(
      builds_df,
      options = list(
        dom = 't',
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE
      ),
      selection = list(mode = 'multiple', target = 'row'),
      escape = FALSE,
      rownames = FALSE
    )
  })
  
  # Portfolio exposure - FD
  output$fd_portfolio_exposure <- renderDT({
    req(rv$fd_portfolio, rv$sim_metadata, rv$config)
    
    # Use universal exposure table helper
    exposure_table <- create_exposure_table_universal(
      rv$fd_portfolio,
      rv$sim_metadata,
      "FD",
      rv$config
    )
    
    # Check if we should use currency formatting (salary_cap >= 1000)
    use_currency <- rv$config$salary_caps$FD >= 1000
    
    dt_output <- datatable(
      exposure_table,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        dom = 'tp',
        order = list(list(2, 'desc'))  # Sort by Exposure column (3rd column, 0-indexed = 2)
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("Exposure", "OwnProj", "Leverage"), 1)
    
    # Format salary based on salary cap
    if (use_currency) {
      dt_output <- dt_output %>% formatCurrency("Salary", "$", digits = 0)
    } else {
      dt_output <- dt_output %>% formatRound("Salary", 0)
    }
    
    dt_output
  })
  
  output$fd_portfolio_lineups <- renderDT({
    req(rv$fd_portfolio, rv$config)
    
    display_table <- create_portfolio_display_table(rv$fd_portfolio, rv$config)
    format_cols <- get_format_columns(display_table, rv$config)
    
    datatable(
      display_table[, -"RowID"],
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        dom = 'tp',
        columnDefs = list(
          list(width = '30px', targets = 0)
        )
      ),
      escape = FALSE,
      rownames = FALSE
    ) %>%
      formatRound(format_cols, 1) %>%
      formatCurrency("Salary", "$", digits = 0)
  })
  
  output$fd_download_portfolio <- downloadHandler(
    filename = function() {
      paste0("FD_Portfolio_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv$fd_portfolio)
      
      download_table <- copy(rv$fd_portfolio)
      
      # Detect column format
      if ("MVP" %in% names(download_table)) {
        download_table <- create_download_mvp(download_table, rv$sim_metadata)
      } else {
        player_cols <- grep("^Player", names(download_table), value = TRUE)
        for (col in player_cols) {
          players <- download_table[[col]]
          ids <- rv$sim_metadata[match(players, rv$sim_metadata$Player), FDID]
          download_table[[col]] <- paste0(ids, ":", players)
        }
      }
      
      fwrite(download_table, file)
    }
  )
  output$sd_portfolio_lineups <- renderDT({
    req(rv$sd_portfolio, rv$config)
    
    display_table <- create_portfolio_display_table(rv$sd_portfolio, rv$config)
    format_cols <- get_format_columns(display_table, rv$config)
    
    datatable(
      display_table[, -"RowID"],
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        dom = 'tp',
        columnDefs = list(
          list(width = '30px', targets = 0)
        )
      ),
      escape = FALSE,
      rownames = FALSE
    ) %>%
      formatRound(format_cols, 1) %>%
      formatCurrency("Salary", "$", digits = 0)
  })
  
  output$sd_download_portfolio <- downloadHandler(
    filename = function() {
      paste0("SD_Portfolio_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv$sd_portfolio)
      
      download_table <- create_download_showdown(rv$sd_portfolio, rv$sim_metadata)
      
      fwrite(download_table, file)
    }
  )
  
  # Portfolio overview - SD
  output$sd_builds_summary <- renderDT({
    req(rv$sd_builds)
    
    builds_df <- data.table(
      Build = names(rv$sd_builds),
      Lineups = sapply(rv$sd_builds, function(b) b$num_lineups),
      Filters = sapply(rv$sd_builds, function(b) b$filters),
      Delete = paste0('<button class="btn btn-danger btn-xs delete-build" data-build="', names(rv$sd_builds), '">Delete</button>')
    )
    
    datatable(
      builds_df,
      options = list(
        dom = 't',
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE
      ),
      selection = list(mode = 'multiple', target = 'row'),
      escape = FALSE,
      rownames = FALSE
    )
  })
  
  # Portfolio exposure - SD
  output$sd_portfolio_exposure <- renderDT({
    req(rv$sd_portfolio, rv$sim_metadata)
    
    # Detect player columns (handles Captain format)
    if ("Captain" %in% names(rv$sd_portfolio)) {
      player_cols <- c("Captain", grep("^Util", names(rv$sd_portfolio), value = TRUE))
    } else {
      player_cols <- grep("^Player", names(rv$sd_portfolio), value = TRUE)
    }
    
    # Calculate full portfolio exposure
    all_players <- unlist(rv$sd_portfolio[, ..player_cols])
    exposure_counts <- table(all_players)
    
    # Start with ALL players
    exposure_table <- data.table(Player = rv$sim_metadata$Player)
    
    # Add full portfolio exposure
    exposure_table[, Exposure := 0]
    for (i in 1:nrow(exposure_table)) {
      player <- exposure_table$Player[i]
      if (player %in% names(exposure_counts)) {
        exposure_table$Exposure[i] <- (as.numeric(exposure_counts[player]) / nrow(rv$sd_portfolio)) * 100
      }
    }
    
    # Add metadata - only get columns that exist
    metadata_cols <- c("Player", "SDSalary", "DKOwn")  # SD uses DK ownership
    
    # Add sport-specific columns if they exist
    sport_cols <- c("Starting", "Team", "Car")
    available_sport_cols <- intersect(sport_cols, names(rv$sim_metadata))
    metadata_cols <- c(metadata_cols, available_sport_cols)
    
    exposure_table <- merge(
      exposure_table,
      rv$sim_metadata[, ..metadata_cols],
      by = "Player",
      all.x = TRUE
    )
    
    setnames(exposure_table, c("SDSalary", "DKOwn"), c("Salary", "OwnProj"))
    exposure_table[, OwnProj := OwnProj * 100]
    exposure_table[, Leverage := round(Exposure - OwnProj, 1)]
    
    # Set column order - only include columns that exist
    base_cols <- c("Player", "Salary")
    sport_specific_cols <- intersect(c("Starting", "Team", "Car"), names(exposure_table))
    metric_cols <- c("Exposure", "OwnProj", "Leverage")
    
    final_col_order <- c(base_cols, sport_specific_cols, metric_cols)
    setcolorder(exposure_table, final_col_order)
    
    # Filter to only players with exposure > 0
    exposure_table <- exposure_table[Exposure > 0]
    
    # Sort by exposure
    setorder(exposure_table, -Exposure)
    
    # Format table
    datatable(
      exposure_table,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        dom = 'tp',
        order = list(list(which(names(exposure_table) == "Exposure") - 1, 'desc'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("Exposure", "OwnProj", "Leverage"), 1) %>%
      formatCurrency("Salary", "$", digits = 0)  # SD uses currency format
  })
  
}



# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui, server)