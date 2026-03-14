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

source("sport_configs_universal.R")
source("OptimalLineups_Core.R")
source("LineupBuilder_Core.R")
source("portfolio_helpers_universal.R")

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$span("Simulation App",
                      style = "color: #FFE500 !important; font-weight: 700; font-size: 18px;"),
    titleWidth = 200
  ),
  
  dashboardSidebar(
    width = 200,
    tags$div(
      style = "padding: 15px; text-align: center; background-color: #000000; border-bottom: 2px solid #FFE500;",
      tags$img(src = "logo.jpg", width = "160px")
    ),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Data Input",        tabName = "input",      icon = icon("file-upload")),
      menuItem("Sim Results",       tabName = "sim_results",icon = icon("chart-bar")),
      menuItem("Lineup Scoring",    tabName = "scoring",    icon = icon("trophy")),
      menuItem("Portfolio Builder", tabName = "portfolio",  icon = icon("layer-group"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "gts_theme.css")),
    
    tags$style(HTML("
      .box-primary > .box-header,.box.box-primary > .box-header,.box-solid.box-primary > .box-header{background-color:#2d2d2d!important;color:#FFE500!important;border-bottom:2px solid #FFE500!important}
      .box-primary .box-title{color:#FFE500!important;font-weight:600!important}
      .box-primary,.box.box-primary{border-top-color:#404040!important;border-color:#404040!important}
      .box-warning>.box-header{background-color:#2d2d2d!important;color:#FFE500!important;border-color:#FFE500!important}
      .box-warning .box-title{color:#FFE500!important;font-weight:600!important}
      .box-info>.box-header,.box.box-info>.box-header{background-color:#2d2d2d!important;color:#FFE500!important;border-bottom:2px solid #FFE500!important}
      .box-info .box-title{color:#FFE500!important}
      .box-info,.box.box-info{border-top-color:#404040!important;border-color:#404040!important;background-color:#1e1e1e!important}
      .box-warning{background-color:#1e1e1e!important;border:1px solid #FFE500!important}
      .panel-warning>.panel-heading,.panel-heading,.panel-default>.panel-heading{background-color:#2d2d2d!important;color:#FFE500!important;border-color:#FFE500!important}
      .panel-title,.panel-heading h4{color:#FFE500!important;font-weight:600!important}
      .irs-bar,.irs-bar-edge,.irs-handle,.irs--flat .irs-bar,.irs--modern .irs-bar,.irs--round .irs-bar,.irs--flat .irs-handle,.irs--modern .irs-handle,.irs--round .irs-handle{background:#FFE500!important;border-color:#D4B000!important}
      .irs-from,.irs-to,.irs-single,.irs--flat .irs-from,.irs--flat .irs-to,.irs--flat .irs-single,.irs--modern .irs-from,.irs--modern .irs-to,.irs--modern .irs-single{background:#FFE500!important;color:#000000!important;font-weight:600!important}
      .irs-line{background-color:#404040!important}
      .irs-grid-text{color:#999999!important}
      .btn-primary,.btn-primary:hover,.btn-primary:focus,.btn-primary:active{background-color:#FFE500!important;color:#000000!important;border-color:#D4B000!important;font-weight:600!important}
      .btn-warning,.btn-warning:hover,.btn-warning:focus,.btn-warning:active{background-color:#FFE500!important;color:#000000!important;border-color:#D4B000!important;font-weight:600!important}
      .selectize-input{background-color:#1e1e1e!important;border:1px solid #404040!important;color:#ffffff!important}
      .selectize-input.focus{border-color:#FFE500!important}
      .selectize-input .item{background:#FFE500!important;color:#000000!important;font-weight:600!important}
      .selectize-dropdown{background:#1e1e1e!important;border:1px solid #FFE500!important}
      .selectize-dropdown .option{color:#ffffff!important}
      .selectize-dropdown .option:hover,.selectize-dropdown .option.active{background:#FFE500!important;color:#000000!important}
      .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{background-color:#1e1e1e!important;color:#FFE500!important;border:1px solid #FFE500!important;border-bottom-color:#1e1e1e!important}
      .nav-tabs>li>a{color:#cccccc!important;background-color:#2d2d2d!important;border:1px solid #404040!important;border-bottom:none!important}
      .nav-tabs>li>a:hover{background-color:#404040!important;border-color:#FFE500!important;color:#FFE500!important}
      .alert-info,.callout-info{background-color:rgba(255,229,0,0.15)!important;border-color:#FFE500!important;color:#FFF4B3!important}
      .alert-warning{background-color:rgba(255,229,0,0.15)!important;border-color:#FFE500!important;color:#FFF4B3!important}
      .progress-bar-primary,.progress-bar-info{background-color:#FFE500!important;color:#000000!important}
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover{background-color:#FFE500!important;color:#000000!important;border-color:#D4B000!important}
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover{background-color:#404040!important;color:#FFE500!important;border-color:#FFE500!important}
      .skin-black .main-header,.skin-black .main-header .navbar,.skin-black .main-header .logo{background-image:none!important;background-color:#000000!important}
      .skin-black .main-header::before,.skin-black .main-header::after,.skin-black .main-header .navbar::before,.skin-black .main-header .navbar::after{display:none!important;content:none!important}
      .skin-black .main-header{border-bottom:3px solid #FFE500!important;box-shadow:0 2px 4px rgba(0,0,0,0.3)!important}
      .content-wrapper{background-color:#121212!important;background-image:none!important}
      .content-wrapper::before,.content-wrapper::after{display:none!important;content:none!important}
      .box,.box-body,.box-header,.box-footer{background-image:none!important}
      .main-sidebar{background-image:none!important;background-color:#121212!important}
      .main-sidebar::before,.main-sidebar::after{display:none!important;content:none!important}
      .panel,.panel-body,.panel-heading,.well{background-image:none!important;background-color:#2d2d2d!important}
      .content-wrapper,.tab-content,.tab-pane{background-color:#121212!important;background-image:none!important}
      #portfolio *,#portfolio .box,#portfolio .box-body{background-image:none!important}
      #portfolio .tab-content,#portfolio .tab-pane{background-color:#121212!important;background-image:none!important}
      .tabbable,.tabbable .tab-content{background-color:transparent!important;background-image:none!important}
      .radio label,.checkbox label{color:#ffffff!important;font-weight:500!important}
      .form-control{background-color:#1e1e1e!important;color:#ffffff!important;border-color:#404040!important}
      .form-control:focus{background-color:#1e1e1e!important;border-color:#FFE500!important;box-shadow:0 0 0 2px rgba(255,229,0,0.2)!important}
      label{color:#ffffff!important}
      .shiny-spinner-message-container{background-color:rgba(0,0,0,0.9)!important;border:2px solid #FFE500!important;border-radius:8px!important}
      .shiny-spinner-message{color:#FFE500!important;font-weight:600!important}
      .delete-build,.delete-lineup{position:relative;z-index:100!important;pointer-events:auto!important;cursor:pointer!important}
    ")),
    
    tags$script(HTML("
      $(document).ready(function() {
        setTimeout(function() {
          $('select[multiple]').each(function() {
            if (this.selectize) {
              var s = this.selectize;
              s.on('item_add',    function() { var self=this; setTimeout(function(){self.close();self.blur();},100); });
              s.on('item_remove', function() { var self=this; setTimeout(function(){self.close();self.blur();},100); });
              s.close();
            }
          });
        }, 2500);
        $(document).on('shiny:value', function(event) {
          if (event.name.includes('locked') || event.name.includes('excluded')) {
            setTimeout(function() {
              var e = document.getElementById(event.name);
              if (e && e.selectize) e.selectize.close();
            }, 200);
          }
        });
      });
      $(document).on('click', '.delete-build', function(e) {
        e.stopPropagation(); e.preventDefault();
        var b = $(this).data('build'), p = null;
        if ($('#portfolio_dk_tabs').is(':visible'))      p = 'DraftKings';
        else if ($('#portfolio_fd_tabs').is(':visible')) p = 'FanDuel';
        else if ($('#portfolio_sd_tabs').is(':visible')) p = 'Showdown';
        if (p==='DraftKings') Shiny.setInputValue('dk_delete_build', b, {priority:'event'});
        else if (p==='FanDuel')   Shiny.setInputValue('fd_delete_build', b, {priority:'event'});
        else if (p==='Showdown')  Shiny.setInputValue('sd_delete_build', b, {priority:'event'});
      });
      // Track selected rows per platform so delete preserves highlights
      $(document).on('click', '.delete-lineup', function(e) {
        e.stopPropagation(); e.preventDefault();
        var r = $(this).data('row'), p = null;
        if ($('#portfolio_dk_tabs').is(':visible'))      p = 'DraftKings';
        else if ($('#portfolio_fd_tabs').is(':visible')) p = 'FanDuel';
        else if ($('#portfolio_sd_tabs').is(':visible')) p = 'Showdown';
        if (p==='DraftKings') Shiny.setInputValue('dk_delete_lineup', r, {priority:'event'});
        else if (p==='FanDuel')   Shiny.setInputValue('fd_delete_lineup', r, {priority:'event'});
        else if (p==='Showdown')  Shiny.setInputValue('sd_delete_lineup', r, {priority:'event'});
      });
    ")),
    
    tabItems(
      
      # ======================================================================
      # TAB 1: DATA INPUT
      # ======================================================================
      tabItem(tabName = "input",
              fluidRow(column(12,
                              box(title="Upload Input File", status="primary", solidHeader=TRUE, width=NULL,
                                  fileInput("input_file", NULL, accept=c(".xlsx",".xls"), width="100%"),
                                  uiOutput("sport_detected_message")
                              )
              )),
              fluidRow(column(12,
                              box(title="Run Simulation", status="primary", solidHeader=TRUE, width=NULL,
                                  fluidRow(
                                    column(4, h5("Sport:",     style="color:#FFE500;margin-top:0;"), textOutput("sport_display")),
                                    column(4, h5("Platforms:", style="color:#FFE500;margin-top:0;"), textOutput("platforms_display")),
                                    column(4, numericInput("n_sims","Number of Simulations",
                                                           value=10000, min=100, max=50000, step=1000, width="100%"))
                                  ),
                                  uiOutput("golf_settings_ui"),
                                  fluidRow(column(12,
                                                  actionButton("run_simulation","Run Simulation",
                                                               class="btn-primary", style="width:100%;margin-top:10px;")
                                  )),
                                  uiOutput("sim_complete_message")
                              )
              ))
      ),
      
      # ======================================================================
      # TAB 2: SIM RESULTS
      # ======================================================================
      tabItem(tabName = "sim_results",
              conditionalPanel(
                condition = "output.has_sim_results == false",
                box(width=12, title="No Simulation Results", status="warning", solidHeader=TRUE,
                    div(style="text-align:center;padding:40px;",
                        icon("chart-bar", class="fa-3x", style="color:#FFE500;margin-bottom:20px;"),
                        h3("Run a simulation first", style="color:#FFE500;font-weight:600;"),
                        p("Upload your data and run a simulation in the Data Input tab.",
                          style="color:#cccccc;font-size:16px;margin-top:15px;")
                    )
                )
              ),
              conditionalPanel(
                condition = "output.has_sim_results == true",
                fluidRow(
                  column(8,
                         box(width=NULL, title="SELECT PLATFORM", status="primary", solidHeader=TRUE,
                             div(style="padding:5px 0;", uiOutput("sim_results_platform_selector"))
                         )
                  ),
                  conditionalPanel(condition="output.sport_detected == 'NASCAR'",
                                   column(4,
                                          box(width=NULL, title="EXPORT DATA", status="primary", solidHeader=TRUE,
                                              div(style="padding:5px 0;",
                                                  downloadButton("download_full_sim_results","Download Full Sim Results",
                                                                 class="btn-primary",
                                                                 style="width:100%;background-color:#FFE500;color:#000000;border:none;font-weight:600;"),
                                                  p("CSV with all drivers, all sims",
                                                    style="font-size:11px;color:#999;margin-top:5px;margin-bottom:0;")
                                              )
                                          )
                                   )
                  ),
                  conditionalPanel(condition="output.sport_detected == 'MMA'",
                                   column(4,
                                          box(width=NULL, title="EXPORT DATA", status="primary", solidHeader=TRUE,
                                              div(style="padding:5px 0;",
                                                  downloadButton("download_mma_sim_results","Download Full Sim Results",
                                                                 class="btn-primary",
                                                                 style="width:100%;background-color:#FFE500;color:#000000;border:none;font-weight:600;"),
                                                  p("CSV with all fighters, all sims",
                                                    style="font-size:11px;color:#999;margin-top:5px;margin-bottom:0;")
                                              )
                                          )
                                   )
                  ),
                  conditionalPanel(condition="output.sport_detected == 'CBB'",
                                   column(4,
                                          box(width=NULL, title="EXPORT DATA", status="primary", solidHeader=TRUE,
                                              div(style="padding:5px 0;",
                                                  downloadButton("download_cbb_sim_results","Download Full Sim Results",
                                                                 class="btn-primary",
                                                                 style="width:100%;background-color:#FFE500;color:#000000;border:none;font-weight:600;"),
                                                  p("CSV with all players, all sims + full statlines",
                                                    style="font-size:11px;color:#999;margin-top:5px;margin-bottom:0;")
                                              )
                                          )
                                   )
                  )
                ),
                fluidRow(column(12,
                                box(width=NULL, title="FANTASY PROJECTIONS & DISTRIBUTIONS",
                                    status="primary", solidHeader=TRUE,
                                    DTOutput("sim_projections_table") %>%
                                      shinycssloaders::withSpinner(color="#FFE500", type=6)
                                )
                )),
                conditionalPanel(
                  condition = "output.has_sport_visuals == true",
                  uiOutput("sport_specific_visuals_ui")
                )
              )
      ),
      
      # ======================================================================
      # TAB 3: LINEUP SCORING
      # ======================================================================
      tabItem(tabName = "scoring",  uiOutput("scoring_tabs_ui")),
      
      # ======================================================================
      # TAB 4: PORTFOLIO BUILDER
      # ======================================================================
      tabItem(tabName = "portfolio", uiOutput("portfolio_tabs_ui"))
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  rv <- reactiveValues(
    sport              = NULL,
    config             = NULL,
    input_data         = NULL,
    simulation_results = NULL,
    sim_metadata       = NULL,
    projections        = NULL,
    dk_optimal_lineups = NULL,
    fd_optimal_lineups = NULL,
    sd_optimal_lineups = NULL,
    dk_portfolio       = NULL,
    fd_portfolio       = NULL,
    sd_portfolio       = NULL,
    dk_builds          = list(),
    fd_builds          = list(),
    sd_builds          = list(),
    dk_build_counter   = 0,
    fd_build_counter   = 0,
    sd_build_counter   = 0,
    golf_no_cut        = FALSE,
    golf_cut_line      = 65,
    has_fd             = TRUE,
    sport_visuals      = NULL,
    full_sim_results   = NULL
  )
  
  
  # ==========================================================================
  # AUTO-DETECT SPORT & LOAD DATA
  # ==========================================================================
  
  observeEvent(input$input_file, {
    req(input$input_file)
    tryCatch({
      rv$sport  <- detect_sport(input$input_file$datapath)
      rv$config <- get_sport_config(rv$sport)
      
      # For NASCAR, detect FD availability from the file itself at upload time
      # so we can hide FD buttons before simulation even runs
      if (rv$sport == "NASCAR") {
        driver_cols <- names(suppressMessages(
          read_excel(input$input_file$datapath, sheet = "Driver", n_max = 1)
        ))
        rv$has_fd <- all(c("FDSalary", "FDID", "FDName") %in% driver_cols)
        # Filter config platforms to only what's available
        if (!rv$has_fd) rv$config$platforms <- setdiff(rv$config$platforms, "FD")
      }
      
      output$sport_detected_message <- renderUI({
        tags$div(class="alert alert-success", style="margin-top:10px;padding:8px;",
                 icon("check-circle"),
                 sprintf(" Detected: %s", rv$config$sport_display_name))
      })
      output$sport_display     <- renderText({ rv$config$sport_display_name })
      output$platforms_display <- renderText({
        paste(sapply(rv$config$platforms, function(p)
          switch(p,"DK"="DraftKings","FD"="FanDuel","SD"="Showdown",p)), collapse=", ")
      })
      
      file_path <- input$input_file$datapath
      
      if (rv$sport == "GOLF") {
        source("golf_engine.R")
        rv$input_data <- read_golf_input(file_path)
      } else if (rv$sport == "F1") {
        source("f1_engine.R")
        rv$input_data <- read_f1_input(file_path)
      } else if (rv$sport == "CBB") {
        source("cbb_engine.R")
        rv$input_data <- read_cbb_input(file_path)
      } else if (!is.null(rv$config$input_file$load_all_sheets) &&
                 rv$config$input_file$load_all_sheets) {
        all_sheets    <- excel_sheets(file_path)
        rv$input_data <- setNames(lapply(all_sheets, function(s) read_excel(file_path, sheet=s)), all_sheets)
      } else if (is.null(rv$config$input_file$required_sheets)) {
        sh            <- excel_sheets(file_path)[1]
        rv$input_data <- setNames(list(read_excel(file_path, sheet=sh)), sh)
      } else {
        sh            <- rv$config$input_file$required_sheets
        rv$input_data <- setNames(lapply(sh, function(s) read_excel(file_path, sheet=s)), sh)
      }
      
      showNotification("Data loaded!", type="message", duration=3)
    }, error=function(e) {
      output$sport_detected_message <- renderUI({
        tags$div(class="alert alert-danger", style="margin-top:10px;padding:8px;",
                 icon("exclamation-triangle"), sprintf(" Error: %s", e$message))
      })
    })
  })
  
  
  # Golf tournament settings UI
  output$golf_settings_ui <- renderUI({
    req(rv$sport == "GOLF")
    div(style="background-color:#333;padding:10px;margin:10px 0;border-radius:5px;",
        h5("Tournament Settings", style="color:#FFE500;margin-top:0;"),
        fluidRow(
          column(6, checkboxInput("golf_no_cut","No Cut Tournament", value=FALSE)),
          column(6, conditionalPanel(condition="!input.golf_no_cut",
                                     numericInput("golf_cut_line","Cut Line (+ ties):",
                                                  value=65, min=50, max=85, step=5, width="100%")))
        )
    )
  })
  
  output$current_sport <- reactive({ rv$sport %||% "" })
  outputOptions(output, "current_sport", suspendWhenHidden=FALSE)
  
  
  # ==========================================================================
  # RUN SIMULATION
  # ==========================================================================
  
  observeEvent(input$run_simulation, {
    req(rv$input_data, rv$config)
    # Clear all downstream state so stale lineups/portfolio never bleed into new sim
    rv$simulation_results <- NULL
    rv$sim_metadata       <- NULL
    rv$dk_optimal_lineups <- NULL
    rv$fd_optimal_lineups <- NULL
    rv$sd_optimal_lineups <- NULL
    rv$dk_portfolio <- NULL; rv$dk_builds <- list(); rv$dk_build_counter <- 0
    rv$fd_portfolio <- NULL; rv$fd_builds <- list(); rv$fd_build_counter <- 0
    rv$sd_portfolio <- NULL; rv$sd_builds <- list(); rv$sd_build_counter <- 0
    rv$sport_visuals      <- NULL
    rv$full_sim_results   <- NULL
    engine_file <- paste0(tolower(rv$sport), "_engine.R")
    if (!file.exists(engine_file)) {
      showNotification(paste("Engine not found:", engine_file), type="error", duration=10)
      return()
    }
    source(engine_file)
    
    progress <- Progress$new(session, min=0, max=1)
    progress$set(message="Running simulation...", value=0)
    on.exit(progress$close())
    
    tryCatch({
      
      if (rv$sport == "GOLF") {
        no_cut   <- if (!is.null(input$golf_no_cut))   input$golf_no_cut   else FALSE
        cut_line <- if (!is.null(input$golf_cut_line)) input$golf_cut_line else 65
        result   <- run_golf_simulation(
          input_data        = rv$input_data,
          n_sims            = input$n_sims,
          cut_line          = cut_line,
          no_cut            = no_cut,
          progress_callback = function(v, m) progress$set(value=v, detail=m)
        )
        rv$simulation_results <- result$sim_results
        rv$sim_metadata       <- result$sim_metadata
        rv$golf_no_cut        <- result$no_cut
        rv$golf_cut_line      <- result$cut_line
        rv$has_fd             <- result$has_fd
        rv$sport_visuals      <- NULL
        rv$full_sim_results   <- NULL
        
      } else {
        sim_function <- get(rv$config$simulation$function_name)
        result       <- sim_function(
          input_data = rv$input_data, n_sims = input$n_sims, config = rv$config,
          progress_callback = function(detail, value) progress$set(value=value, detail=detail)
        )
        validate_simulation_output(result$sim_results, result$metadata, rv$config)
        rv$simulation_results <- result$sim_results
        rv$sim_metadata       <- result$metadata
        
        if (rv$sport == "NASCAR") {
          rv$full_sim_results <- result$full_results
          rv$has_fd           <- if (!is.null(result$has_fd)) result$has_fd else TRUE
          # Keep config$platforms in sync in case simulation disagrees with file-load detection
          if (!rv$has_fd) rv$config$platforms <- setdiff(rv$config$platforms, "FD")
        } else if (rv$sport == "F1") {
          rv$full_sim_results <- NULL
          rv$has_fd           <- FALSE
          rv$config$platforms <- "DK"
        } else {
          rv$full_sim_results <- NULL
          rv$has_fd           <- TRUE
        }
        if (!is.null(result$projections))   rv$projections   <- result$projections
        rv$sport_visuals <- if (!is.null(result$sport_visuals)) result$sport_visuals else NULL
      }
      
      output$sim_complete_message <- renderUI({
        tags$div(class="alert alert-success", style="margin-top:10px;",
                 icon("check-circle"),
                 sprintf(" Simulation complete! %s sims | %s %s.",
                         format(input$n_sims, big.mark=","),
                         nrow(rv$sim_metadata),
                         tolower(rv$config$player_label_plural)))
      })
      showNotification("Simulation complete!", type="message")
      
    }, error=function(e) {
      showNotification(paste("Simulation error:", e$message), type="error", duration=10)
      cat("Simulation error:\n"); print(e)
    })
  })
  
  
  # ==========================================================================
  # HELPER FUNCTIONS
  # ==========================================================================
  
  prepare_optimization_data <- function(sim_results, metadata, platform) {
    score_col  <- if (platform == "SD") "DKScore"              else paste0(platform, "Score")
    salary_col <- if (platform == "SD") "SDSalary"             else paste0(platform, "Salary")
    setDT(sim_results); setDT(metadata)
    opt_data <- merge(sim_results, metadata[, .(Player, Salary=get(salary_col))], by="Player")
    opt_data[, FantasyPoints := get(score_col)]
    opt_data[Salary > 0 & !is.na(Salary)]
  }
  
  create_display_table <- function(optimal_lineups, metadata, platform) {
    if ("Captain" %in% names(optimal_lineups)) {
      player_cols <- c("Captain", grep("^Util", names(optimal_lineups), value=TRUE))
    } else if ("MVP" %in% names(optimal_lineups)) {
      player_cols <- c("MVP", grep("^Player", names(optimal_lineups), value=TRUE))
    } else {
      player_cols <- grep("^Player", names(optimal_lineups), value=TRUE)
    }
    display_cols <- intersect(
      c(player_cols, "WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct",
        "TotalSalary","AvgOwn"),
      names(optimal_lineups))
    display_table <- optimal_lineups[, ..display_cols]
    rename_map <- c("WinRate"="Win","Top1Pct"="Top1","Top5Pct"="Top5","Top10Pct"="Top10","Top20Pct"="Top20",
                    "TotalSalary"="Salary")
    for (o in names(rename_map)) if (o %in% names(display_table)) setnames(display_table, o, rename_map[[o]])
    display_table
  }
  
  create_portfolio_display_table <- function(portfolio_data, sport_config) {
    display_table <- copy(portfolio_data); setDT(display_table)
    display_table[, RowID := .I]
    rename_map <- c("WinRate"="Win","Top1Pct"="Top1","Top5Pct"="Top5","Top10Pct"="Top10","Top20Pct"="Top20",
                    "TotalSalary"="Salary")
    for (o in names(rename_map)) if (o %in% names(display_table)) setnames(display_table, o, rename_map[[o]])
    if (!is.null(sport_config$custom_metrics)) {
      for (m in sport_config$custom_metrics) {
        sc <- if (!is.null(m$source_column)) m$source_column else m$source
        dc <- if (!is.null(m$display_name))  m$display_name  else m$label
        if (!is.null(sc) && !is.null(dc) && sc %in% names(display_table) && sc != dc)
          setnames(display_table, sc, dc)
      }
    }
    display_table[, Delete := paste0(
      '<button class="btn btn-danger btn-xs delete-lineup" data-row="', RowID, '">X</button>')]
    setcolorder(display_table, c("Delete", setdiff(names(display_table), c("Delete","RowID"))))
    display_table
  }
  
  get_format_columns <- function(display_table, sport_config) {
    pct_cols <- c("Win","Top1","Top5","Top10","Top20","AvgOwn")
    if (!is.null(sport_config$custom_metrics)) {
      for (m in sport_config$custom_metrics) {
        if (!is.null(m$format) && m$format == "percentage") {
          cn <- if (!is.null(m$display_name)) m$display_name else m$label
          if (!is.null(cn)) pct_cols <- c(pct_cols, cn)
        }
      }
    }
    intersect(pct_cols, names(display_table))
  }
  
  create_download_standard <- function(optimal_lineups, metadata, platform) {
    player_cols    <- grep("^Player", names(optimal_lineups), value=TRUE)
    id_col         <- paste0(platform, "ID")
    download_table <- copy(optimal_lineups)
    for (col in player_cols) {
      ids <- metadata[match(download_table[[col]], metadata$Player), get(id_col)]
      download_table[[col]] <- if (platform=="DK") paste0(download_table[[col]]," (",ids,")")
      else paste0(ids,":",download_table[[col]])
    }
    download_table
  }
  
  create_download_showdown <- function(optimal_lineups, metadata) {
    dl <- copy(optimal_lineups)
    if ("Captain" %in% names(dl)) {
      ids <- metadata[match(dl$Captain, metadata$Player), CPTID]
      dl$Captain <- paste0(dl$Captain," (",ids,")")
    }
    for (col in grep("^Util", names(dl), value=TRUE)) {
      ids <- metadata[match(dl[[col]], metadata$Player), SDID]
      dl[[col]] <- paste0(dl[[col]]," (",ids,")")
    }
    dl
  }
  
  create_download_f1 <- function(optimal_lineups, metadata) {
    dl <- copy(optimal_lineups); setDT(metadata)
    
    fmt <- function(name, id) paste0(name, " (", id, ")")
    
    # Captain: Name (CptDFSID)
    cpt_ids <- metadata[match(dl$Captain, metadata$Player), CptDFSID]
    cpt_col <- fmt(dl$Captain, cpt_ids)
    
    # Flex drivers Util1-4: Name (DKID)
    flex_cols <- lapply(paste0("Util", 1:4), function(col) {
      ids <- metadata[match(dl[[col]], metadata$Player), DKID]
      fmt(dl[[col]], ids)
    })
    
    # Constructor Util5: Name (DKID)
    con_ids <- metadata[match(dl$Util5, metadata$Player), DKID]
    con_col <- fmt(dl$Util5, con_ids)
    
    # Lineup columns with DK upload names
    lineup_cols <- data.table(
      CPT   = cpt_col,
      D     = flex_cols[[1]],
      D2    = flex_cols[[2]],
      D3    = flex_cols[[3]],
      D4    = flex_cols[[4]],
      CNSTR = con_col
    )
    setnames(lineup_cols, c("CPT", "D", "D", "D", "D", "CNSTR"))
    
    # Append metrics if present
    metric_cols <- intersect(c("WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct",
                               "TotalSalary","AvgOwn","Top1Count","AvgScore"),
                             names(dl))
    if (length(metric_cols) > 0)
      cbind(lineup_cols, dl[, ..metric_cols])
    else
      lineup_cols
  }
  
  create_download_mvp <- function(optimal_lineups, metadata) {
    dl <- copy(optimal_lineups); setDT(metadata)
    if ("MVP" %in% names(dl)) {
      ids <- metadata[match(dl$MVP, metadata$Player), FDID]
      dl$MVP <- paste0(ids,":",dl$MVP)
    }
    for (col in grep("^Player", names(dl), value=TRUE)) {
      ids <- metadata[match(dl[[col]], metadata$Player), FDID]
      dl[[col]] <- paste0(ids,":",dl[[col]])
    }
    dl
  }
  
  create_download_table <- function(optimal_lineups, metadata, platform, sport = NULL) {
    if ("Captain" %in% names(optimal_lineups)) {
      if (identical(sport, "F1"))
        return(create_download_f1(optimal_lineups, metadata))
      return(create_download_showdown(optimal_lineups, metadata))
    }
    if ("MVP" %in% names(optimal_lineups)) return(create_download_mvp(optimal_lineups, metadata))
    create_download_standard(optimal_lineups, metadata, platform)
  }
  
  create_display_table_cbb <- function(optimal_lineups) {
    # Player1-8 are stored internally; rename to position labels for display
    dl <- copy(optimal_lineups)
    pos_rename <- c(Player1="G1", Player2="G2", Player3="G3",
                    Player4="F1", Player5="F2", Player6="F3",
                    Player7="UTIL1", Player8="UTIL2")
    for (o in names(pos_rename)) if (o %in% names(dl)) setnames(dl, o, pos_rename[o])
    slot_cols   <- intersect(c("G1","G2","G3","F1","F2","F3","UTIL1","UTIL2"), names(dl))
    metric_cols <- intersect(c("WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct","TotalSalary","AvgOwn"), names(dl))
    keep_cols   <- c(slot_cols, metric_cols)
    dl <- dl[, ..keep_cols]
    metric_rename <- c("WinRate"="Win","Top1Pct"="Top1","Top5Pct"="Top5","Top10Pct"="Top10","Top20Pct"="Top20","TotalSalary"="Salary")
    for (o in names(metric_rename)) if (o %in% names(dl)) setnames(dl, o, metric_rename[o])
    dl
  }
  
  create_download_cbb <- function(optimal_lineups, metadata) {
    dl <- copy(optimal_lineups)
    # Rename Player1-8 to position labels
    pos_rename <- c(Player1="G1", Player2="G2", Player3="G3",
                    Player4="F1", Player5="F2", Player6="F3",
                    Player7="UTIL1", Player8="UTIL2")
    for (o in names(pos_rename)) if (o %in% names(dl)) setnames(dl, o, pos_rename[o])
    slot_cols <- intersect(c("G1","G2","G3","F1","F2","F3","UTIL1","UTIL2"), names(dl))
    for (col in slot_cols) {
      ids <- metadata[match(dl[[col]], metadata$Player), DKID]
      dl[[col]] <- paste0(dl[[col]], " (", ids, ")")
    }
    metric_rename <- c("WinRate"="Win","Top1Pct"="Top1","Top5Pct"="Top5","Top10Pct"="Top10","Top20Pct"="Top20","TotalSalary"="Salary")
    for (o in names(metric_rename)) if (o %in% names(dl)) setnames(dl, o, metric_rename[o])
    dl
  }
  
  
  # ==========================================================================
  # SPORT-SPECIFIC LINEUP METRICS
  # ==========================================================================
  
  add_custom_metrics <- function(scored_lineups, metadata, config, precalc_metrics=NULL) {
    if (!is.null(precalc_metrics)) {
      for (col in names(precalc_metrics)) scored_lineups[[col]] <- precalc_metrics[[col]]
      return(scored_lineups)
    }
    if (!is.null(config$lineup_metrics_function)) {
      engine_file <- paste0(tolower(config$sport_name), "_engine.R")
      if (file.exists(engine_file)) {
        source(engine_file)
        if (exists(config$lineup_metrics_function)) {
          scored_lineups <- get(config$lineup_metrics_function)(
            scored_lineups = scored_lineups,
            sim_results    = rv$simulation_results,
            metadata       = metadata
          )
        }
      }
    }
    scored_lineups
  }
  
  add_golf_custom_metrics <- function(scored_lineups, no_cut) {
    source("golf_engine.R")
    if (exists("calculate_golf_lineup_metrics")) {
      scored_lineups <- calculate_golf_lineup_metrics(
        scored_lineups = scored_lineups,
        sim_results    = rv$simulation_results,
        sim_metadata   = rv$sim_metadata,
        no_cut         = no_cut
      )
    }
    scored_lineups
  }
  
  
  # ==========================================================================
  # DK OPTIMIZATION
  # ==========================================================================
  
  observeEvent(input$run_dk_optimization, {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    rv$dk_optimal_lineups <- NULL
    rv$dk_portfolio <- NULL; rv$dk_builds <- list(); rv$dk_build_counter <- 0
    progress <- Progress$new(session); on.exit(progress$close())
    tryCatch({
      
      if (rv$sport == "GOLF") {
        progress$set(message="Finding optimal DK Golf lineups...", value=0)
        source("golf_engine.R")
        no_cut    <- rv$golf_no_cut %||% FALSE
        dk_config  <- list(platform="DK", roster_size=rv$config$roster_sizes$DK,
                           salary_cap=rv$config$salary_caps$DK)
        dk_opt_cfg <- list(roster_size=dk_config$roster_size, salary_cap=dk_config$salary_cap,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="DKScore",
                           max_lineups=5000)
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "DK")
        
        if (!no_cut) {
          progress$set(detail="Phase 1: Building cut-optimized candidate pool...", value=0.1)
          lineup_data <- generate_golf_candidate_pool(
            sim_results=rv$simulation_results, sim_metadata=rv$sim_metadata,
            config=dk_config, no_cut=FALSE,
            n_sample=rv$config$phase1_n_sample, target_pool=rv$config$phase1_target, verbose=TRUE)
        } else {
          progress$set(detail="Phase 1: Finding optimal lineups...", value=0.1)
          lineup_data <- find_optimal_lineups(opt_data, dk_opt_cfg, mode="standard", k=1, verbose=TRUE)
        }
        progress$set(detail="Phase 2: Scoring lineups...", value=0.45)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.75)
        own_data <- copy(rv$sim_metadata)
        if ("DKOwn" %in% names(own_data)) setnames(own_data, "DKOwn", "Own")
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, dk_opt_cfg,
                                                        ownership_data=own_data, verbose=TRUE)
        progress$set(detail="Adding golf metrics...", value=0.90)
        final_results <- add_golf_custom_metrics(final_results, no_cut)
        rv$dk_optimal_lineups <- final_results
        
      } else if (rv$sport == "TENNIS") {
        progress$set(message="Finding optimal DK Tennis lineups...", value=0)
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "DK")
        opt_config <- list(roster_size=rv$config$roster_sizes$DK, salary_cap=rv$config$salary_caps$DK,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="DKScore", max_lineups=5000)
        progress$set(detail="Generating lineups (win-based)...", value=0.1)
        lineup_result    <- find_optimal_lineups(opt_data, opt_config, mode="win_based", verbose=TRUE)
        lineup_data      <- list(unique_lineups=lineup_result$unique_lineups,
                                 n_sims=lineup_result$n_sims, mode=lineup_result$mode)
        tennis_precalc   <- lineup_result$win_metrics
        progress$set(detail="Phase 2: Scoring...", value=0.4)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3: Metrics...", value=0.7)
        own_data <- copy(rv$sim_metadata)
        if ("DKOwn" %in% names(own_data)) setnames(own_data, "DKOwn", "Own")
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, opt_config,
                                                        ownership_data=own_data, verbose=TRUE)
        final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config, tennis_precalc)
        rv$dk_optimal_lineups <- final_results
        
      } else if (rv$sport == "F1") {
        source("f1_engine.R")
        n_sims_total <- length(unique(rv$simulation_results$SimID))
        progress$set(message="Finding optimal F1 lineups...", value=0)
        opt_config <- list(
          salary_cap     = rv$config$salary_caps$DK,
          cpt_multiplier = 1.5,
          max_lineups    = 3000L,
          percentiles    = c(0.01, 0.05, 0.10, 0.20),
          platform_col   = "DKScore"
        )
        progress$set(detail=sprintf("Phase 1: Finding optimal lineup per sim (%s sims)...",
                                    format(n_sims_total, big.mark=",")), value=0.05)
        lineup_data <- find_optimal_f1_lineups(
          sim_results = rv$simulation_results,
          metadata    = rv$sim_metadata,
          config      = opt_config,
          verbose     = TRUE
        )
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        cbb_sim_for_scoring <- copy(rv$simulation_results)
        setDT(cbb_sim_for_scoring)
        score_matrix <- score_all_lineups(lineup_data, cbb_sim_for_scoring, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, opt_config,
                                                        ownership_data=NULL, verbose=TRUE)
        rv$dk_optimal_lineups <- final_results
        
      } else if (rv$sport == "CBB") {
        source("cbb_engine.R")
        progress$set(message="Finding optimal CBB lineups...", value=0)
        cbb_opt_config <- list(salary_cap=rv$config$salary_caps$DK,
                               percentiles=c(0.01,0.05,0.10,0.20),
                               platform_col="DKScore", max_lineups=5000)
        progress$set(detail="Phase 1: Building lineup pool (per-sim LP)...", value=0.05)
        lineup_data  <- find_optimal_lineups_cbb(rv$simulation_results, rv$sim_metadata,
                                                 cbb_opt_config, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        cbb_sim_for_scoring <- copy(rv$simulation_results)
        setDT(cbb_sim_for_scoring)
        score_matrix <- score_all_lineups(lineup_data, cbb_sim_for_scoring, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        if ("DKOwn" %in% names(own_data)) setnames(own_data, "DKOwn", "Own")
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, cbb_opt_config,
                                                        ownership_data=own_data, verbose=TRUE)
        rv$dk_optimal_lineups <- final_results
        
      } else {
        dk_mode <- rv$config$optimization_modes$DK %||% "standard"
        progress$set(message="Finding optimal DraftKings lineups...", value=0)
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "DK")
        opt_config <- list(roster_size=rv$config$roster_sizes$DK, salary_cap=rv$config$salary_caps$DK,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="DKScore",
                           progress_frequency=500, use_parallel=TRUE, max_lineups=5000)
        progress$set(detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data <- find_optimal_lineups(opt_data, opt_config, mode=dk_mode, k=1, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        if ("DKOwn" %in% names(own_data)) setnames(own_data, "DKOwn", "Own")
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, opt_config,
                                                        ownership_data=own_data, verbose=TRUE)
        progress$set(detail="Phase 3: Adding custom metrics...", value=0.90)
        final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config)
        rv$dk_optimal_lineups <- final_results
      }
      
      progress$set(detail="Complete!", value=1.0)
      showNotification(sprintf("Found %d optimal DK lineups!", nrow(rv$dk_optimal_lineups)), type="message")
    }, error=function(e) {
      showNotification(paste("DK error:", e$message), type="error", duration=NULL)
      cat("DK error:\n"); print(e)
    })
  })
  
  
  # ==========================================================================
  # FD OPTIMIZATION
  # ==========================================================================
  
  observeEvent(input$run_fd_optimization, {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    rv$fd_optimal_lineups <- NULL
    rv$fd_portfolio <- NULL; rv$fd_builds <- list(); rv$fd_build_counter <- 0
    progress <- Progress$new(session); on.exit(progress$close())
    tryCatch({
      
      if (rv$sport == "GOLF") {
        if (!rv$has_fd) { showNotification("No FD salary data found.", type="warning"); return() }
        source("golf_engine.R")
        no_cut    <- rv$golf_no_cut %||% FALSE
        fd_config  <- list(platform="FD", roster_size=rv$config$roster_sizes$FD,
                           salary_cap=rv$config$salary_caps$FD)
        fd_opt_cfg <- list(roster_size=fd_config$roster_size, salary_cap=fd_config$salary_cap,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="FDScore",
                           max_lineups=5000)
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "FD")
        if (!no_cut) {
          progress$set(message="Finding optimal FD Golf lineups...", value=0,
                       detail="Phase 1: Building cut-optimized pool...")
          lineup_data <- generate_golf_candidate_pool(
            sim_results=rv$simulation_results, sim_metadata=rv$sim_metadata,
            config=fd_config, no_cut=FALSE,
            n_sample=rv$config$phase1_n_sample, target_pool=rv$config$phase1_target, verbose=TRUE)
        } else {
          progress$set(message="Finding optimal FD Golf lineups...", value=0, detail="Phase 1...")
          lineup_data <- find_optimal_lineups(opt_data, fd_opt_cfg, mode="standard", k=1, verbose=TRUE)
        }
        progress$set(detail="Phase 2...", value=0.45)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3...", value=0.75)
        own_data <- copy(rv$sim_metadata)
        if ("FDOwn" %in% names(own_data)) setnames(own_data, "FDOwn", "Own")
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, fd_opt_cfg,
                                                        ownership_data=own_data, verbose=TRUE)
        progress$set(detail="Golf metrics...", value=0.90)
        final_results <- add_golf_custom_metrics(final_results, no_cut)
        rv$fd_optimal_lineups <- final_results
        
      } else {
        if (!is.null(rv$has_fd) && !rv$has_fd) {
          showNotification("No FD salary data in this file.", type="warning"); return()
        }
        fd_mode <- rv$config$optimization_modes$FD %||% "standard"
        progress$set(message="Finding optimal FanDuel lineups...", value=0)
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "FD")
        opt_config <- list(roster_size=rv$config$roster_sizes$FD, salary_cap=rv$config$salary_caps$FD,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="FDScore",
                           mvp_multiplier=1.5, progress_frequency=500, use_parallel=TRUE, max_lineups=5000)
        progress$set(detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data <- find_optimal_lineups(opt_data, opt_config, mode=fd_mode, k=1, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        if ("FDOwn" %in% names(own_data)) setnames(own_data, "FDOwn", "Own")
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, opt_config,
                                                        ownership_data=own_data, verbose=TRUE)
        progress$set(detail="Phase 3: Adding custom metrics...", value=0.90)
        final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config)
        rv$fd_optimal_lineups <- final_results
      }
      
      progress$set(detail="Complete!", value=1.0)
      showNotification(sprintf("Found %d optimal FD lineups!", nrow(rv$fd_optimal_lineups)), type="message")
    }, error=function(e) {
      showNotification(paste("FD error:", e$message), type="error", duration=NULL)
      cat("FD error:\n"); print(e)
    })
  })
  
  
  # ==========================================================================
  # SD OPTIMIZATION
  # ==========================================================================
  
  observeEvent(input$run_sd_optimization, {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    rv$sd_optimal_lineups <- NULL
    rv$sd_portfolio <- NULL; rv$sd_builds <- list(); rv$sd_build_counter <- 0
    progress <- Progress$new(session); on.exit(progress$close())
    tryCatch({
      sd_mode    <- rv$config$optimization_modes$SD %||% "captain"
      opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "SD")
      opt_config <- list(roster_size=rv$config$roster_sizes$SD, salary_cap=rv$config$salary_caps$SD,
                         percentiles=c(0.01,0.05,0.10,0.20), platform_col="DKScore",
                         cpt_multiplier=1.5, progress_frequency=500, use_parallel=TRUE, max_lineups=5000)
      progress$set(message="Finding optimal Showdown lineups...",
                   detail="Phase 1: Building lineup pool...", value=0.05)
      lineup_data  <- find_optimal_lineups(opt_data, opt_config, mode=sd_mode, k=1, verbose=TRUE)
      progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                  format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
      score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
      progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
      own_data <- copy(rv$sim_metadata)
      if ("DKOwn" %in% names(own_data)) setnames(own_data, "DKOwn", "Own")
      final_results <- calculate_distribution_metrics(score_matrix, lineup_data, opt_config,
                                                      ownership_data=own_data, verbose=TRUE)
      progress$set(detail="Phase 3: Adding custom metrics...", value=0.90)
      final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config)
      rv$sd_optimal_lineups <- final_results
      progress$set(detail="Complete!", value=1.0)
      showNotification(sprintf("Found %d optimal Showdown lineups!", nrow(final_results)), type="message")
    }, error=function(e) {
      showNotification(paste("SD error:", e$message), type="error", duration=NULL)
      cat("SD error:\n"); print(e)
    })
  })
  
  
  # ==========================================================================
  # DOWNLOAD HANDLERS - SCORING TAB
  # ==========================================================================
  
  output$dk_download <- downloadHandler(
    filename=function() paste0("DK_Optimal_Lineups_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) { dl <- if (isTRUE(rv$sport == "CBB")) create_download_cbb(rv$dk_optimal_lineups, rv$sim_metadata) else create_download_table(rv$dk_optimal_lineups, rv$sim_metadata, "DK", rv$sport); fwrite(dl, file) })
  output$fd_download <- downloadHandler(
    filename=function() paste0("FD_Optimal_Lineups_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) fwrite(create_download_table(rv$fd_optimal_lineups, rv$sim_metadata, "FD", rv$sport), file))
  output$sd_download <- downloadHandler(
    filename=function() paste0("SD_Optimal_Lineups_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) fwrite(create_download_table(rv$sd_optimal_lineups, rv$sim_metadata, "SD", rv$sport), file))
  output$download_full_sim_results <- downloadHandler(
    filename=function() paste0("NASCAR_Full_Sim_Results_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) { req(rv$sport=="NASCAR", rv$sport_visuals$full_results)
      fwrite(rv$sport_visuals$full_results, file) })
  
  output$download_mma_sim_results <- downloadHandler(
    filename=function() paste0("MMA_Full_Sim_Results_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) { req(rv$sport=="MMA", rv$simulation_results)
      dl <- merge(copy(rv$simulation_results),
                  rv$sim_metadata[, .(Player, DKSalary, FDSalary, DKOwn, FDOwn, WinProb)],
                  by="Player", all.x=TRUE)
      setcolorder(dl, c("Player","SimID","DKScore","FDScore","Win","Outcome",
                        "DKSalary","FDSalary","DKOwn","FDOwn","WinProb"))
      fwrite(dl, file) })
  
  output$download_cbb_sim_results <- downloadHandler(
    filename=function() paste0("CBB_Full_Sim_Results_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) { req(rv$sport=="CBB", rv$simulation_results)
      dl <- merge(copy(rv$simulation_results),
                  unique(rv$sim_metadata[, .(Player, DKSalary, DKOwn, Team, PosGroup)], by="Player"),
                  by="Player", all.x=TRUE)
      front  <- c("Player","Team","PosGroup","DKSalary","DKOwn","SimID","DKScore")
      stats  <- intersect(c("pts","tpm","reb","ast","stl","blk","to"), names(dl))
      setcolorder(dl, c(front, stats))
      fwrite(dl, file) })
  
  
  # ==========================================================================
  # SCORING TAB UI
  # ==========================================================================
  
  output$scoring_tabs_ui <- renderUI({
    req(rv$config)
    fluidRow(box(title="Lineup Scoring", status="warning", solidHeader=TRUE, width=12,
                 p("Find and score optimal lineups across all platforms:"),
                 fluidRow(lapply(rv$config$platforms, function(platform) {
                   pname <- switch(platform,"DK"="DraftKings","FD"="FanDuel","SD"="Showdown")
                   column(6, actionButton(paste0("run_",tolower(platform),"_optimization"),
                                          paste("Score", pname), class="btn-warning btn-block",
                                          style="margin-bottom:10px;font-size:16px;padding:12px;"))
                 })),
                 hr(),
                 uiOutput("download_buttons_ui"),
                 hr(),
                 div(style="margin-bottom:15px;", uiOutput("view_platform_ui")),
                 DTOutput("lineup_results_table")   # static DTOutput - must live outside renderUI to avoid DIV warning
    ))
  })
  
  output$download_buttons_ui <- renderUI({
    ready <- c()
    if (!is.null(rv$dk_optimal_lineups)) ready <- c(ready, "DK")
    if (!is.null(rv$fd_optimal_lineups)) ready <- c(ready, "FD")
    if (!is.null(rv$sd_optimal_lineups)) ready <- c(ready, "SD")
    if (length(ready) == 0) return(p("Score lineups to enable downloads", style="color:#999;"))
    fluidRow(lapply(ready, function(p) {
      pname <- switch(p,"DK"="DraftKings","FD"="FanDuel","SD"="Showdown")
      column(6, downloadButton(paste0(tolower(p),"_download"), paste("Download All",p,"Lineups"),
                               class="btn-block",
                               style="background-color:#4caf50!important;border-color:#4caf50!important;color:white!important;font-size:14px;padding:10px;"))
    }))
  })
  
  output$view_platform_ui <- renderUI({
    ready <- c(); labels <- c()
    if (!is.null(rv$dk_optimal_lineups)) { ready <- c(ready,"DK"); labels <- c(labels,"DraftKings") }
    if (!is.null(rv$fd_optimal_lineups)) { ready <- c(ready,"FD"); labels <- c(labels,"FanDuel") }
    if (!is.null(rv$sd_optimal_lineups)) { ready <- c(ready,"SD"); labels <- c(labels,"Showdown") }
    if (length(ready) == 0) return(p("Score lineups to view results", style="color:#999;font-style:italic;"))
    div(style="margin-bottom:15px;",
        tags$label("View Results:", style="color:#FFE500;font-weight:bold;display:block;margin-bottom:10px;"),
        radioButtons("view_platform", label=NULL,
                     choices=setNames(ready, labels), selected=ready[1], inline=TRUE)
    )
  })
  
  output$lineup_results_table <- renderDT({
    req(input$view_platform)
    optimal <- switch(input$view_platform,
                      "DK"=rv$dk_optimal_lineups, "FD"=rv$fd_optimal_lineups, "SD"=rv$sd_optimal_lineups)
    req(optimal)
    display_table <- if (isTRUE(rv$sport == "CBB")) create_display_table_cbb(optimal) else create_display_table(optimal, rv$sim_metadata, input$view_platform)
    dt <- datatable(display_table,
                    options=list(pageLength=50, searching=FALSE, lengthChange=FALSE, scrollX=TRUE, dom='tp',
                                 order=list(list(which(names(display_table)=="Win")-1,'desc'))),
                    rownames=FALSE) %>%
      formatRound(intersect(c("Win","Top1","Top5","Top10","Top20","AvgOwn"),
                            names(display_table)), 1)
    if ("Salary" %in% names(display_table)) dt <- dt %>% formatCurrency("Salary","$",digits=0)
    if ("TotalStart" %in% names(display_table)) dt <- dt %>% formatRound(c("TotalStart","AvgStart"),1)
    for (gc in intersect(c("ExpectedCuts","AtLeast6","AtLeast5","EarlyLateCount"), names(display_table)))
      dt <- dt %>% formatRound(gc, 1)
    dt
  })
  
  
  # ==========================================================================
  # PORTFOLIO TABS UI
  # ==========================================================================
  
  output$portfolio_tabs_ui <- renderUI({
    req(rv$config)
    
    tab_panels <- lapply(rv$config$platforms, function(platform) {
      lp    <- tolower(platform)
      pname <- switch(platform,"DK"="DraftKings","FD"="FanDuel","SD"="Showdown")
      
      tabPanel(title=pname,
               fluidRow(column(12,
                               div(style="text-align:right;margin-bottom:10px;padding:5px;background-color:#1a1a1a;border-radius:4px;",
                                   h4(textOutput(paste0(lp,"_portfolio_count")),
                                      style="display:inline-block;color:#FFE500;margin-right:20px;vertical-align:middle;font-size:16px;"),
                                   actionButton(paste0(lp,"_clear_portfolio"),"CLEAR PORTFOLIO",
                                                class="btn-danger", style="margin-right:10px;"),
                                   downloadButton(paste0(lp,"_download_portfolio"),"DOWNLOAD PORTFOLIO", class="btn-success")
                               )
               )),
               
               tabsetPanel(id=paste0("portfolio_",lp,"_tabs"),
                           
                           tabPanel("Filtered Pool",
                                    fluidRow(box(title="Lineup Filters", status="warning", solidHeader=TRUE,
                                                 width=12, collapsible=TRUE,
                                                 # ── Row 1: all filter columns ──────────────────────────────────
                                                 fluidRow(
                                                   # Col 1: Rate minimums (always shown, same for all sports)
                                                   column(2,
                                                          div(style="background-color:#2d2d2d;padding:6px;border-radius:4px;border:1px solid #404040;",
                                                              h6("Min Rates", style="color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
                                                              div(style="display:flex;align-items:center;margin-bottom:4px;",
                                                                  tags$label("Win:", style="color:#FFE500;font-size:11px;margin:0 5px 0 0;width:30px;"),
                                                                  numericInput(paste0(lp,"_min_win"),  NULL,value=0,min=0,max=100,step=0.1,width="68px")),
                                                              div(style="display:flex;align-items:center;margin-bottom:4px;",
                                                                  tags$label("Top1:", style="color:#FFE500;font-size:11px;margin:0 5px 0 0;width:30px;"),
                                                                  numericInput(paste0(lp,"_min_top1"), NULL,value=0,min=0,max=100,step=0.5,width="68px")),
                                                              div(style="display:flex;align-items:center;margin-bottom:4px;",
                                                                  tags$label("Top5:", style="color:#FFE500;font-size:11px;margin:0 5px 0 0;width:30px;"),
                                                                  numericInput(paste0(lp,"_min_top5"), NULL,value=0,min=0,max=100,step=1,  width="68px")),
                                                              div(style="display:flex;align-items:center;margin-bottom:4px;",
                                                                  tags$label("Top10:", style="color:#FFE500;font-size:11px;margin:0 5px 0 0;width:30px;"),
                                                                  numericInput(paste0(lp,"_min_top10"),NULL,value=0,min=0,max=100,step=2,  width="68px")),
                                                              div(style="display:flex;align-items:center;margin-bottom:0;",
                                                                  tags$label("Top20:", style="color:#FFE500;font-size:11px;margin:0 5px 0 0;width:30px;"),
                                                                  numericInput(paste0(lp,"_min_top20"),NULL,value=0,min=0,max=100,step=5,  width="68px"))
                                                          )
                                                   ),
                                                   # Col 2: Ranges (auto from data — all sports including golf cols)
                                                   column(4,
                                                          div(style="padding-left:4px;",
                                                              h6("Ranges", style="color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
                                                              uiOutput(paste0(lp,"_range_sliders"))
                                                          )
                                                   ),
                                                   # Col 3: Lock / Exclude players (always shown; F1 gets split captain/driver/constructor)
                                                   column(3,
                                                          div(style="background-color:#2d2d2d;padding:8px;border-radius:4px;border:1px solid #404040;",
                                                              h6("Lock / Exclude", style="color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
                                                              if (isTRUE(rv$sport == "F1")) {
                                                                # F1: three separate captain / driver / constructor controls
                                                                lu <- rv[[paste0(lp,"_optimal_lineups")]]
                                                                drv_choices  <- if (!is.null(lu)) sort(unique(unlist(lu[, grep("^Captain|^Util[1-4]$", names(lu), value=TRUE), with=FALSE]))) else NULL
                                                                con_choices  <- if (!is.null(lu)) sort(unique(unlist(lu[, grep("^Util5$",             names(lu), value=TRUE), with=FALSE]))) else NULL
                                                                tagList(
                                                                  tags$label("Captain Lock:", style="color:#aaa;font-size:11px;"),
                                                                  selectizeInput(paste0(lp,"_locked_captain"),  NULL, choices=drv_choices, multiple=TRUE, selected=character(0),
                                                                                 options=list(plugins=list('remove_button'),placeholder='Lock captain',maxItems=1), width="100%"),
                                                                  tags$label("Captain Exclude:", style="color:#aaa;font-size:11px;"),
                                                                  selectizeInput(paste0(lp,"_excluded_captain"), NULL, choices=drv_choices, multiple=TRUE, selected=character(0),
                                                                                 options=list(plugins=list('remove_button'),placeholder='Exclude captain'), width="100%"),
                                                                  tags$label("Driver Lock:", style="color:#aaa;font-size:11px;"),
                                                                  selectizeInput(paste0(lp,"_locked_players"),  NULL, choices=drv_choices, multiple=TRUE, selected=character(0),
                                                                                 options=list(plugins=list('remove_button'),placeholder='Lock flex driver',maxItems=4), width="100%"),
                                                                  tags$label("Driver Exclude:", style="color:#aaa;font-size:11px;"),
                                                                  selectizeInput(paste0(lp,"_excluded_players"), NULL, choices=drv_choices, multiple=TRUE, selected=character(0),
                                                                                 options=list(plugins=list('remove_button'),placeholder='Exclude flex driver'), width="100%"),
                                                                  tags$label("Constructor Lock:", style="color:#aaa;font-size:11px;"),
                                                                  selectizeInput(paste0(lp,"_locked_constructor"),  NULL, choices=con_choices, multiple=TRUE, selected=character(0),
                                                                                 options=list(plugins=list('remove_button'),placeholder='Lock constructor',maxItems=1), width="100%"),
                                                                  tags$label("Constructor Exclude:", style="color:#aaa;font-size:11px;"),
                                                                  selectizeInput(paste0(lp,"_excluded_constructor"), NULL, choices=con_choices, multiple=TRUE, selected=character(0),
                                                                                 options=list(plugins=list('remove_button'),placeholder='Exclude constructor'), width="100%")
                                                                )
                                                              } else {
                                                                # All other sports: single lock/exclude across all slots
                                                                lu <- rv[[paste0(lp,"_optimal_lineups")]]
                                                                all_players <- if (!is.null(lu)) {
                                                                  pc <- grep("^Player|^Captain|^MVP|^Util", names(lu), value=TRUE)
                                                                  sort(unique(unlist(lu[, ..pc]))[!is.na(unique(unlist(lu[, ..pc]))) & unique(unlist(lu[, ..pc])) != ""])
                                                                } else NULL
                                                                tagList(
                                                                  selectizeInput(paste0(lp,"_locked_players"), "Lock:",
                                                                                 choices=all_players, multiple=TRUE, selected=character(0),
                                                                                 options=list(plugins=list('remove_button'), placeholder='Search to lock players',
                                                                                              maxItems=8), width="100%"),
                                                                  selectizeInput(paste0(lp,"_excluded_players"), "Exclude:",
                                                                                 choices=all_players, multiple=TRUE, selected=character(0),
                                                                                 options=list(plugins=list('remove_button'), placeholder='Search to exclude players'),
                                                                                 width="100%")
                                                                )
                                                              }
                                                          )
                                                   ),
                                                   # Col 4: Add to portfolio (always shown, same for all sports)
                                                   column(3,
                                                          div(style="background-color:#2d2d2d;padding:8px;border-radius:4px;border:1px solid #FFE500;",
                                                              h6("Add to Portfolio", style="color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
                                                              numericInput(paste0(lp,"_num_lineups"), "Lineups:", value=20, min=1, max=150, width="100%"),
                                                              textInput(paste0(lp,"_build_label"), "Label:", value="", placeholder="Optional", width="100%"),
                                                              h5(textOutput(paste0(lp,"_filtered_count")),
                                                                 style="color:#FFE500;font-weight:bold;text-align:center;margin:8px 0 6px 0;"),
                                                              actionButton(paste0(lp,"_add_build"), "ADD TO PORTFOLIO",
                                                                           class="btn-primary", style="width:100%;font-weight:bold;")
                                                          )
                                                   )
                                                 )
                                    )),
                                    fluidRow(box(title="Player Exposure in Filtered Pool",status="info",solidHeader=TRUE,width=12,
                                                 DTOutput(paste0(lp,"_filtered_exposure"))))
                           ),
                           
                           tabPanel("Portfolio Summary",
                                    fluidRow(box(title="Portfolio Overview",status="warning",solidHeader=TRUE,width=12,
                                                 hr(style="border-color:#FFE500;"), DTOutput(paste0(lp,"_builds_summary")))),
                                    fluidRow(box(title="Portfolio Player Exposure",status="info",solidHeader=TRUE,width=12,
                                                 DTOutput(paste0(lp,"_portfolio_exposure"))))
                           ),
                           
                           tabPanel("Portfolio Lineups",
                                    fluidRow(box(title="All Portfolio Lineups",status="info",solidHeader=TRUE,width=12,
                                                 DTOutput(paste0(lp,"_portfolio_lineups"))))
                           )
               )
      )
    })
    
    do.call(tabBox, c(list(id="portfolio_platform", width=12), tab_panels))
  })
  
  
  
  
  
  # ==========================================================================
  # RANGE SLIDERS
  # ==========================================================================
  
  make_range_sliders <- function(lp) {
    renderUI({
      optimal <- rv[[paste0(lp,"_optimal_lineups")]]; req(optimal)
      num_cols  <- names(optimal)[sapply(optimal, is.numeric)]
      num_cols  <- setdiff(num_cols, grep("^Player|^Captain|^MVP", names(optimal), value=TRUE))
      range_cols <- setdiff(num_cols, c("WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct","ExpectedCuts"))
      cfg_map <- list(
        TotalSalary=list(label="Salary",format="k",step=0.1),
        AvgOwn=list(label="Avg Own",format="decimal",step=0.1),
        CumulativeStarting=list(label="Total Start",format="whole",step=1),
        GeometricMeanStarting=list(label="Avg Start",format="decimal",step=0.1),
        AvgStart=list(label="Avg Start",format="decimal",step=0.1),
        AtLeast6=list(label="All 6 Cut%",format="decimal",step=1),
        AtLeast5=list(label="5+ Cut%",format="decimal",step=1),
        EarlyLateCount=list(label="Early/Late Golfers",format="whole",step=1)
      )
      sliders <- Filter(Negate(is.null), lapply(range_cols, function(col) {
        cfg <- cfg_map[[col]] %||% list(label=col,format="decimal",step=0.1)
        mn  <- min(optimal[[col]],na.rm=TRUE); mx <- max(optimal[[col]],na.rm=TRUE)
        if (mn == mx) return(NULL)
        if (cfg$format=="k")     { mn <- floor(mn/1000);  mx <- ceiling(mx/1000); lbl <- paste0(cfg$label," (K)") }
        else if (cfg$format=="whole") { mn <- floor(mn); mx <- ceiling(mx); lbl <- cfg$label }
        else { mn <- floor(mn*10)/10; mx <- ceiling(mx*10)/10; lbl <- cfg$label }
        sliderInput(paste0(lp,"_filter_",col), lbl, min=mn, max=mx, value=c(mn,mx), step=cfg$step, width="100%")
      }))
      n <- length(sliders)
      fluidRow(column(6, sliders[seq(1,n,2)]),
               column(6, if(n>1) sliders[seq(2,n,2)] else list()))
    })
  }
  output$dk_range_sliders <- make_range_sliders("dk")
  output$fd_range_sliders <- make_range_sliders("fd")
  output$sd_range_sliders <- make_range_sliders("sd")
  
  
  # ==========================================================================
  # FILTERED LINEUPS
  # ==========================================================================
  
  make_filtered_lineups <- function(lp) {
    reactive({
      optimal <- rv[[paste0(lp,"_optimal_lineups")]]; req(optimal)
      lineups <- copy(optimal)
      # Rate minimums
      rate_pairs <- list(c("WinRate","win"),c("Top1Pct","top1"),c("Top5Pct","top5"),
                         c("Top10Pct","top10"),c("Top20Pct","top20"))
      for (rp in rate_pairs) {
        v <- input[[paste0(lp,"_min_",rp[2])]]
        if (!is.null(v) && v > 0 && rp[1] %in% names(lineups))
          lineups <- lineups[get(rp[1]) >= v]
      }
      # Salary (K conversion)
      sv <- input[[paste0(lp,"_filter_TotalSalary")]]
      if (!is.null(sv) && "TotalSalary" %in% names(lineups))
        lineups <- lineups[TotalSalary >= sv[1]*1000 & TotalSalary <= sv[2]*1000]
      # Other range sliders
      num_cols   <- names(lineups)[sapply(lineups, is.numeric)]
      num_cols   <- setdiff(num_cols, grep("^Player|^Captain|^MVP",names(lineups),value=TRUE))
      range_cols <- setdiff(num_cols, c("WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct","TotalSalary"))
      for (col in range_cols) {
        fv <- input[[paste0(lp,"_filter_",col)]]
        if (!is.null(fv)) lineups <- lineups[get(col) >= fv[1] & get(col) <= fv[2]]
      }
      
      if (isTRUE(rv$sport == "F1")) {
        # F1: captain, flex driver, and constructor lock/exclude are separate
        cpt_cols  <- grep("^Captain",    names(lineups), value=TRUE)
        flex_cols <- grep("^Util[1-4]$", names(lineups), value=TRUE)
        con_cols  <- grep("^Util5$",     names(lineups), value=TRUE)
        
        locked_cpt <- input[[paste0(lp,"_locked_captain")]];     locked_cpt  <- locked_cpt[!is.null(locked_cpt)  & locked_cpt  != ""]
        excl_cpt   <- input[[paste0(lp,"_excluded_captain")]];   excl_cpt    <- excl_cpt[!is.null(excl_cpt)    & excl_cpt    != ""]
        locked_drv <- input[[paste0(lp,"_locked_players")]];     locked_drv  <- locked_drv[!is.null(locked_drv)  & locked_drv  != ""]
        excl_drv   <- input[[paste0(lp,"_excluded_players")]];   excl_drv    <- excl_drv[!is.null(excl_drv)    & excl_drv    != ""]
        locked_con <- input[[paste0(lp,"_locked_constructor")]]; locked_con  <- locked_con[!is.null(locked_con)  & locked_con  != ""]
        excl_con   <- input[[paste0(lp,"_excluded_constructor")]]; excl_con  <- excl_con[!is.null(excl_con)    & excl_con    != ""]
        
        if (length(locked_cpt) > 0 && length(cpt_cols) > 0)
          lineups <- lineups[apply(lineups[,..cpt_cols],1,function(r) all(locked_cpt %in% r))]
        if (length(excl_cpt)   > 0 && length(cpt_cols) > 0)
          lineups <- lineups[apply(lineups[,..cpt_cols],1,function(r) !any(excl_cpt %in% r))]
        if (length(locked_drv) > 0 && length(flex_cols) > 0)
          lineups <- lineups[apply(lineups[,..flex_cols],1,function(r) all(locked_drv %in% r))]
        if (length(excl_drv)   > 0 && length(flex_cols) > 0)
          lineups <- lineups[apply(lineups[,..flex_cols],1,function(r) !any(excl_drv %in% r))]
        if (length(locked_con) > 0 && length(con_cols) > 0)
          lineups <- lineups[apply(lineups[,..con_cols],1,function(r) all(locked_con %in% r))]
        if (length(excl_con)   > 0 && length(con_cols) > 0)
          lineups <- lineups[apply(lineups[,..con_cols],1,function(r) !any(excl_con %in% r))]
        
      } else {
        # All other sports: unified lock/exclude across all slots
        locked <- input[[paste0(lp,"_locked_players")]]
        locked <- locked[!is.null(locked) & locked != ""]
        if (length(locked) > 0) {
          pc <- grep("^Player|^Captain|^MVP|^Util",names(lineups),value=TRUE)
          lineups <- lineups[apply(lineups[,..pc],1,function(r) all(locked %in% r))]
        }
        excluded <- input[[paste0(lp,"_excluded_players")]]
        excluded <- excluded[!is.null(excluded) & excluded != ""]
        if (length(excluded) > 0) {
          pc <- grep("^Player|^Captain|^MVP|^Util",names(lineups),value=TRUE)
          lineups <- lineups[apply(lineups[,..pc],1,function(r) !any(excluded %in% r))]
        }
      }
      lineups
    })
  }
  dk_filtered_lineups <- make_filtered_lineups("dk")
  fd_filtered_lineups <- make_filtered_lineups("fd")
  sd_filtered_lineups <- make_filtered_lineups("sd")
  
  
  
  output$dk_filtered_count <- renderText({ paste0("Filtered Pool: ", nrow(dk_filtered_lineups()), " lineups") })
  output$fd_filtered_count <- renderText({ paste0("Filtered Pool: ", nrow(fd_filtered_lineups()), " lineups") })
  output$sd_filtered_count <- renderText({ paste0("Filtered Pool: ", nrow(sd_filtered_lineups()), " lineups") })
  
  
  # ==========================================================================
  # FILTERED EXPOSURE
  # ==========================================================================
  
  make_filtered_exposure <- function(filtered_reactive, platform) {
    renderDT({
      req(filtered_reactive(), rv$sim_metadata)
      filtered   <- filtered_reactive()
      salary_col <- paste0(platform,"Salary")
      own_col    <- paste0(platform,"Own")
      is_f1      <- isTRUE(rv$sport == "F1")
      
      # Include all slot columns
      cpt_cols   <- grep("^Captain", names(filtered), value=TRUE)
      flex_cols  <- grep("^Util[1-4]$", names(filtered), value=TRUE)
      con_cols   <- grep("^Util5$",     names(filtered), value=TRUE)
      all_pc     <- grep("^Player|^Captain|^MVP|^Util|^G[123]$|^F[123]$", names(filtered), value=TRUE)
      
      n_lineups  <- nrow(filtered)
      cpt_counts <- if (length(cpt_cols))  table(unlist(filtered[, ..cpt_cols]))  else table(character(0))
      flex_counts <- if (length(flex_cols)) table(unlist(filtered[, ..flex_cols])) else table(character(0))
      con_counts  <- if (length(con_cols))  table(unlist(filtered[, ..con_cols]))  else table(character(0))
      all_counts  <- table(unlist(filtered[, ..all_pc]))
      
      exp_tbl <- data.table(Player = rv$sim_metadata$Player, Exposure = 0)
      for (i in seq_len(nrow(exp_tbl))) {
        p <- exp_tbl$Player[i]
        if (p %in% names(all_counts)) exp_tbl$Exposure[i] <- as.numeric(all_counts[p]) / n_lineups * 100
      }
      
      if (is_f1) {
        exp_tbl[, CptExp  := 0]
        exp_tbl[, FlexExp := 0]
        for (i in seq_len(nrow(exp_tbl))) {
          p <- exp_tbl$Player[i]
          if (p %in% names(cpt_counts))  exp_tbl$CptExp[i]  <- as.numeric(cpt_counts[p])  / n_lineups * 100
          if (p %in% names(flex_counts)) exp_tbl$FlexExp[i] <- as.numeric(flex_counts[p]) / n_lineups * 100
        }
        # Label constructor rows
        exp_tbl[Player %in% rv$sim_metadata$Player[rv$sim_metadata$PlayerType == "Constructor"],
                CptExp := NA_real_]
        exp_tbl[Player %in% rv$sim_metadata$Player[rv$sim_metadata$PlayerType == "Constructor"],
                FlexExp := NA_real_]
      }
      
      meta_cols <- intersect(c("Player","PlayerType",salary_col,own_col,
                               "PosGroup","RGProj","RGMin","GameTime","Starting","Team","Car","Position","Match","Opponent","Surface","Tour","TeeTimeGroup","CutProb"),
                             names(rv$sim_metadata))
      exp_tbl <- merge(exp_tbl, rv$sim_metadata[, ..meta_cols], by="Player", all.x=TRUE)
      if (!is.null(rv$simulation_results)) {
        score_col_sim <- if ("DKScore" %in% names(rv$simulation_results)) "DKScore" else paste0(platform, "Score")
        if (score_col_sim %in% names(rv$simulation_results)) {
          sim_proj <- rv$simulation_results[, .(SimProj = round(mean(get(score_col_sim), na.rm=TRUE), 1)), by=Player]
          exp_tbl  <- merge(exp_tbl, sim_proj, by="Player", all.x=TRUE)
        }
      }
      if (salary_col %in% names(exp_tbl)) setnames(exp_tbl, salary_col, "Salary")
      if (own_col    %in% names(exp_tbl)) {
        setnames(exp_tbl, own_col, "OwnProj")
        exp_tbl[, OwnProj  := OwnProj * 100]
        exp_tbl[, Leverage := round(Exposure - OwnProj, 1)]
      }
      
      base_meta <- c("Player", if (is_f1) "PlayerType" else NULL,
                     "PosGroup","Salary","RGProj","RGMin","SimProj","GameTime","Starting","Team","Car",
                     "Position","Match","Opponent","Surface","Tour","TeeTimeGroup","CutProb")
      meta_order    <- intersect(base_meta, names(exp_tbl))
      f1_exp_cols   <- if (is_f1) intersect(c("CptExp","FlexExp"), names(exp_tbl)) else character(0)
      metrics_order <- intersect(c("Exposure","OwnProj","Leverage"), names(exp_tbl))
      setcolorder(exp_tbl, c(meta_order, f1_exp_cols, metrics_order))
      exp_tbl <- exp_tbl[Exposure > 0]; setorder(exp_tbl, -Exposure)
      
      # Rename columns for display (CBB only)
      if (isTRUE(rv$sport == "CBB")) {
        rename_map <- c(PosGroup="Pos", Salary="Sal", RGMin="Mins", RGProj="Proj", GameTime="Time", SimProj="Sim")
        for (old in names(rename_map)) if (old %in% names(exp_tbl)) setnames(exp_tbl, old, rename_map[[old]])
      }
      
      dt <- datatable(exp_tbl,
                      options=list(pageLength=50,scrollX=TRUE,searching=FALSE,lengthChange=FALSE,dom='tp'),
                      rownames=FALSE)
      rc <- intersect(c("Exposure","CptExp","FlexExp","OwnProj","Leverage","CutProb","RGProj","RGMin","SimProj",
                        "Proj","Mins","Sim"), names(exp_tbl))
      if (length(rc) > 0) dt <- dt %>% formatRound(rc, 1)
      cap <- rv$config$salary_caps[[platform]] %||% 50000
      sal_col <- if ("Sal" %in% names(exp_tbl)) "Sal" else if ("Salary" %in% names(exp_tbl)) "Salary" else NULL
      if (!is.null(sal_col) && cap >= 1000) dt <- dt %>% formatCurrency(sal_col,"$",digits=0)
      dt
    })
  }
  output$dk_filtered_exposure <- make_filtered_exposure(dk_filtered_lineups, "DK")
  output$fd_filtered_exposure <- make_filtered_exposure(fd_filtered_lineups, "FD")
  output$sd_filtered_exposure <- make_filtered_exposure(sd_filtered_lineups, "SD")
  
  
  # ==========================================================================
  # ADD BUILD
  # ==========================================================================
  
  make_add_build <- function(lp) {
    observeEvent(input[[paste0(lp,"_add_build")]], {
      filtered <- switch(lp,"dk"=dk_filtered_lineups(),"fd"=fd_filtered_lineups(),"sd"=sd_filtered_lineups())
      req(filtered)
      n <- input[[paste0(lp,"_num_lineups")]]
      if (nrow(filtered) < n) { showNotification(paste0("Only ",nrow(filtered)," available."),type="warning"); return() }
      sampled <- filtered[sample(nrow(filtered),n)]
      cnt <- paste0(lp,"_build_counter"); rv[[cnt]] <- rv[[cnt]] + 1
      raw <- input[[paste0(lp,"_build_label")]]
      lbl <- if (is.null(raw)||raw=="") paste0("Build ",rv[[cnt]]) else iconv(raw,to="UTF-8",sub="")
      sampled[, Build := lbl]
      pn <- paste0(lp,"_portfolio")
      rv[[pn]] <- if (is.null(rv[[pn]])) sampled else rbindlist(list(rv[[pn]],sampled),fill=TRUE)
      parts <- c()
      for (rp in list(c("win","Win"),c("top1","Top1"),c("top5","Top5"),c("top10","Top10"),c("top20","Top20"))) {
        v <- input[[paste0(lp,"_min_",rp[1])]]; if(!is.null(v)&&v>0) parts <- c(parts,paste0(rp[2],">=",v))
      }
      rv[[paste0(lp,"_builds")]][[lbl]] <- list(label=lbl, num_lineups=n,
                                                filters=if(length(parts)>0) paste(parts,collapse=" | ") else "No filters")
      showNotification(paste0("Added ",n," lineups as '",lbl,"'"),type="message")
      updateTextInput(session, paste0(lp,"_build_label"), value="")
    })
  }
  make_add_build("dk"); make_add_build("fd"); make_add_build("sd")
  
  
  # ==========================================================================
  # CLEAR / DELETE BUILDS / DELETE LINEUPS / COUNTS
  # ==========================================================================
  
  observeEvent(input$dk_clear_portfolio,{rv$dk_portfolio<-NULL;rv$dk_builds<-list();rv$dk_build_counter<-0;showNotification("DK cleared",type="message")})
  observeEvent(input$fd_clear_portfolio,{rv$fd_portfolio<-NULL;rv$fd_builds<-list();rv$fd_build_counter<-0;showNotification("FD cleared",type="message")})
  observeEvent(input$sd_clear_portfolio,{rv$sd_portfolio<-NULL;rv$sd_builds<-list();rv$sd_build_counter<-0;showNotification("SD cleared",type="message")})
  
  make_delete_build <- function(lp) {
    observeEvent(input[[paste0(lp,"_delete_build")]], {
      req(input[[paste0(lp,"_delete_build")]])
      b <- input[[paste0(lp,"_delete_build")]]; bn <- paste0(lp,"_builds"); pn <- paste0(lp,"_portfolio")
      if (b %in% names(rv[[bn]])) {
        if (!is.null(rv[[pn]])) {
          p <- rv[[pn]][Build!=b]
          rv[[pn]] <- if(nrow(p)==0) NULL else p
        }
        rv[[bn]][[b]] <- NULL
        showNotification(paste0("Deleted: ",b),type="message")
      }
    })
  }
  make_delete_build("dk"); make_delete_build("fd"); make_delete_build("sd")
  
  make_delete_lineup <- function(lp) {
    observeEvent(input[[paste0(lp,"_delete_lineup")]], {
      pn <- paste0(lp,"_portfolio"); bn <- paste0(lp,"_builds")
      req(input[[paste0(lp,"_delete_lineup")]], rv[[pn]])
      row <- as.integer(input[[paste0(lp,"_delete_lineup")]])
      db  <- rv[[pn]][row, Build]
      p   <- rv[[pn]][-row]
      if (db %in% names(rv[[bn]])) {
        rv[[bn]][[db]]$num_lineups <- rv[[bn]][[db]]$num_lineups - 1
        if (rv[[bn]][[db]]$num_lineups == 0) rv[[bn]][[db]] <- NULL
      }
      if (nrow(p)==0) { rv[[pn]]<-NULL; rv[[bn]]<-list(); rv[[paste0(lp,"_build_counter")]]<-0 }
      else rv[[pn]] <- p
      showNotification(paste0("Deleted lineup from ",toupper(lp)),type="warning")
    })
  }
  make_delete_lineup("dk"); make_delete_lineup("fd"); make_delete_lineup("sd")
  
  make_portfolio_count <- function(lp) {
    renderText({
      p <- rv[[paste0(lp,"_portfolio")]]
      if(is.null(p)) "Portfolio: 0 lineups"
      else paste0("Portfolio: ",nrow(p)," lineups across ",length(rv[[paste0(lp,"_builds")]])," builds")
    })
  }
  output$dk_portfolio_count <- make_portfolio_count("dk")
  output$fd_portfolio_count <- make_portfolio_count("fd")
  output$sd_portfolio_count <- make_portfolio_count("sd")
  
  
  # ==========================================================================
  # BUILDS SUMMARY / PORTFOLIO EXPOSURE / PORTFOLIO LINEUPS
  # ==========================================================================
  
  make_builds_summary <- function(lp) {
    renderDT({
      builds <- rv[[paste0(lp,"_builds")]]; req(builds)
      builds_df <- data.table(Build=names(builds), Lineups=sapply(builds,function(b)b$num_lineups),
                              Filters=sapply(builds,function(b) b$filters %||% ""),
                              Delete=paste0('<button class="btn btn-danger btn-xs delete-build" data-build="',names(builds),'">Delete</button>'))
      datatable(builds_df, options=list(dom='t',scrollX=TRUE,searching=FALSE,lengthChange=FALSE),
                selection=list(mode='multiple',target='row'), escape=FALSE, rownames=FALSE)
    })
  }
  output$dk_builds_summary <- make_builds_summary("dk")
  output$fd_builds_summary <- make_builds_summary("fd")
  output$sd_builds_summary <- make_builds_summary("sd")
  
  make_portfolio_exposure <- function(lp, platform) {
    renderDT({
      pn <- paste0(lp,"_portfolio"); port <- rv[[pn]]; req(port, rv$sim_metadata)
      salary_col <- if(platform=="SD") "SDSalary" else paste0(platform,"Salary")
      own_col    <- if(platform=="SD") "DKOwn"    else paste0(platform,"Own")
      is_f1      <- isTRUE(rv$sport == "F1")
      
      cpt_cols   <- grep("^Captain",    names(port), value=TRUE)
      flex_cols  <- grep("^Util[1-4]$", names(port), value=TRUE)
      con_cols   <- grep("^Util5$",     names(port), value=TRUE)
      all_pc     <- grep("^Player|^Captain|^MVP|^Util|^G[123]$|^F[123]$", names(port), value=TRUE)
      
      n_lineups   <- nrow(port)
      cpt_counts  <- if (length(cpt_cols))  table(unlist(port[, ..cpt_cols]))  else table(character(0))
      flex_counts <- if (length(flex_cols)) table(unlist(port[, ..flex_cols])) else table(character(0))
      con_counts  <- if (length(con_cols))  table(unlist(port[, ..con_cols]))  else table(character(0))
      all_counts  <- table(unlist(port[, ..all_pc]))
      
      exp_tbl <- data.table(Player=rv$sim_metadata$Player, Exposure=0)
      for (i in seq_len(nrow(exp_tbl))) {
        p <- exp_tbl$Player[i]
        if (p %in% names(all_counts)) exp_tbl$Exposure[i] <- as.numeric(all_counts[p]) / n_lineups * 100
      }
      
      if (is_f1) {
        exp_tbl[, CptExp  := 0]
        exp_tbl[, FlexExp := 0]
        for (i in seq_len(nrow(exp_tbl))) {
          p <- exp_tbl$Player[i]
          if (p %in% names(cpt_counts))  exp_tbl$CptExp[i]  <- as.numeric(cpt_counts[p])  / n_lineups * 100
          if (p %in% names(flex_counts)) exp_tbl$FlexExp[i] <- as.numeric(flex_counts[p]) / n_lineups * 100
        }
        exp_tbl[Player %in% rv$sim_metadata$Player[rv$sim_metadata$PlayerType == "Constructor"],
                c("CptExp","FlexExp") := NA_real_]
      }
      
      mc <- intersect(c("Player","PlayerType",salary_col,own_col,
                        "PosGroup","RGProj","RGMin","GameTime","Starting","Team","Car","Position","Match","Opponent","TeeTimeGroup","CutProb"),
                      names(rv$sim_metadata))
      exp_tbl <- merge(exp_tbl, rv$sim_metadata[,..mc], by="Player", all.x=TRUE)
      if (!is.null(rv$simulation_results)) {
        score_col_sim <- if ("DKScore" %in% names(rv$simulation_results)) "DKScore"
        else paste0(platform, "Score")
        if (score_col_sim %in% names(rv$simulation_results)) {
          sim_proj <- rv$simulation_results[, .(SimProj = round(mean(get(score_col_sim), na.rm=TRUE), 1)), by=Player]
          exp_tbl  <- merge(exp_tbl, sim_proj, by="Player", all.x=TRUE)
        }
      }
      if(salary_col %in% names(exp_tbl)) setnames(exp_tbl, salary_col, "Salary")
      if(own_col    %in% names(exp_tbl)) {
        setnames(exp_tbl, own_col, "OwnProj")
        exp_tbl[,OwnProj:=OwnProj*100]
        exp_tbl[,Leverage:=round(Exposure-OwnProj,1)]
      }
      
      base_meta <- c("Player", if (is_f1) "PlayerType" else NULL,
                     "PosGroup","Salary","RGProj","RGMin","SimProj","GameTime","Starting","Team","Car",
                     "Position","Match","Opponent","Surface","Tour","TeeTimeGroup","CutProb")
      meta_order    <- intersect(base_meta, names(exp_tbl))
      f1_exp_cols   <- if (is_f1) intersect(c("CptExp","FlexExp"), names(exp_tbl)) else character(0)
      metrics_order <- intersect(c("Exposure","OwnProj","Leverage"), names(exp_tbl))
      setcolorder(exp_tbl, c(meta_order, f1_exp_cols, metrics_order))
      exp_tbl <- exp_tbl[Exposure>0]; setorder(exp_tbl,-Exposure)
      
      # Rename columns for display (CBB only)
      if (isTRUE(rv$sport == "CBB")) {
        rename_map <- c(PosGroup="Pos", Salary="Sal", RGMin="Mins", RGProj="Proj", GameTime="Time", SimProj="Sim")
        for (old in names(rename_map)) if (old %in% names(exp_tbl)) setnames(exp_tbl, old, rename_map[[old]])
      }
      
      dt <- datatable(exp_tbl, options=list(pageLength=50,scrollX=TRUE,searching=FALSE,lengthChange=FALSE,dom='tp'), rownames=FALSE)
      rc <- intersect(c("Exposure","CptExp","FlexExp","OwnProj","Leverage","CutProb","RGProj","RGMin","SimProj",
                        "Proj","Mins","Sim"),names(exp_tbl))
      if(length(rc)>0) dt <- dt %>% formatRound(rc,1)
      cap <- rv$config$salary_caps[[platform]] %||% 50000
      sal_col <- if ("Sal" %in% names(exp_tbl)) "Sal" else if ("Salary" %in% names(exp_tbl)) "Salary" else NULL
      if(!is.null(sal_col) && cap>=1000) dt <- dt %>% formatCurrency(sal_col,"$",digits=0)
      dt
    })
  }
  output$dk_portfolio_exposure <- make_portfolio_exposure("dk","DK")
  output$fd_portfolio_exposure <- make_portfolio_exposure("fd","FD")
  output$sd_portfolio_exposure <- make_portfolio_exposure("sd","SD")
  
  make_portfolio_lineups <- function(lp) {
    renderDT({
      port <- rv[[paste0(lp,"_portfolio")]]; req(port, rv$config)
      display_table <- create_portfolio_display_table(port, rv$config)
      format_cols   <- tryCatch(get_format_columns(display_table, rv$config), error=function(e) character(0))
      dt <- datatable(display_table[,-"RowID"],
                      options=list(pageLength=50,scrollX=TRUE,searching=FALSE,lengthChange=FALSE,dom='tp',
                                   columnDefs=list(list(width='30px',targets=0))),
                      escape=FALSE, rownames=FALSE)
      if(length(format_cols)>0) dt <- dt %>% formatRound(format_cols,1)
      if("Salary" %in% names(display_table)) dt <- dt %>% formatCurrency("Salary","$",digits=0)
      dt
    })
  }
  output$dk_portfolio_lineups <- make_portfolio_lineups("dk")
  output$fd_portfolio_lineups <- make_portfolio_lineups("fd")
  output$sd_portfolio_lineups <- make_portfolio_lineups("sd")
  
  
  # ==========================================================================
  # PORTFOLIO DOWNLOADS
  # ==========================================================================
  
  make_portfolio_download <- function(lp, platform) {
    downloadHandler(
      filename=function() paste0(platform,"_Portfolio_",format(Sys.Date(),"%Y%m%d"),".csv"),
      content=function(file) {
        port <- rv[[paste0(lp,"_portfolio")]]; req(port)
        dl <- copy(port)[sample(nrow(port))]
        id_col <- paste0(platform,"ID")
        if ("Captain" %in% names(dl) && isTRUE(rv$sport == "F1")) {
          dl <- create_download_f1(dl, rv$sim_metadata)
        } else if ("Captain" %in% names(dl)) {
          dl <- create_download_showdown(dl, rv$sim_metadata)
        } else if ("MVP" %in% names(dl)) {
          dl <- create_download_mvp(dl, rv$sim_metadata)
        } else if (id_col %in% names(rv$sim_metadata)) {
          for (col in grep("^Player",names(dl),value=TRUE)) {
            ids <- rv$sim_metadata[match(dl[[col]],rv$sim_metadata$Player), get(id_col)]
            dl[[col]] <- if(platform=="DK") paste0(dl[[col]]," (",ids,")") else paste0(ids,":",dl[[col]])
          }
        }
        fwrite(dl, file)
      }
    )
  }
  output$dk_download_portfolio <- make_portfolio_download("dk","DK")
  output$fd_download_portfolio <- make_portfolio_download("fd","FD")
  output$sd_download_portfolio <- make_portfolio_download("sd","SD")
  
  
  # ==========================================================================
  # SIM RESULTS OUTPUTS
  # ==========================================================================
  
  output$has_sim_results   <- reactive({ !is.null(rv$simulation_results) && nrow(rv$simulation_results)>0 })
  outputOptions(output, "has_sim_results",   suspendWhenHidden=FALSE)
  output$sport_detected    <- reactive({ rv$sport %||% "" })
  outputOptions(output, "sport_detected",    suspendWhenHidden=FALSE)
  output$has_sport_visuals <- reactive({
    if (isTRUE(rv$sport == "CBB")) !is.null(rv$simulation_results)
    else !is.null(rv$sport_visuals)
  })
  outputOptions(output, "has_sport_visuals", suspendWhenHidden=FALSE)
  
  output$sim_results_platform_selector <- renderUI({
    req(rv$config)
    # rv$config$platforms is already filtered (FD removed for NASCAR no-FD files at upload)
    platforms <- rv$config$platforms
    platform_choices <- setNames(platforms, sapply(platforms, function(p) {
      switch(p, "DK"="DraftKings", "FD"="FanDuel", "SD"="Showdown", p)
    }))
    radioButtons("sim_results_platform", label=NULL,
                 choices=platform_choices, selected=platforms[1], inline=TRUE)
  })
  
  output$sim_projections_table <- renderDT({
    req(rv$simulation_results, rv$sim_metadata, input$sim_results_platform)
    platform <- input$sim_results_platform
    
    # SD (Showdown) uses DK scoring and SD salary; filter to fighters with SDSalary > 0
    score_col  <- if (platform == "SD") "DKScore"    else paste0(platform, "Score")
    salary_col <- if (platform == "SD") "SDSalary"   else paste0(platform, "Salary")
    own_col    <- if (platform == "SD") "DKOwn"      else paste0(platform, "Own")
    
    sim <- rv$simulation_results
    meta <- rv$sim_metadata
    
    # Showdown: restrict to SD-eligible fighters (salary > 0)
    if (platform == "SD" && "SDSalary" %in% names(meta)) {
      eligible <- meta[SDSalary > 0, Player]
      sim  <- sim[Player %in% eligible]
      meta <- meta[Player %in% eligible]
    }
    
    if (!score_col %in% names(sim)) {
      return(datatable(data.table(Message = "No data for this platform"), rownames=FALSE))
    }
    
    projections <- sim[, .(
      Avg    = round(mean(get(score_col)),           1),
      Median = round(median(get(score_col)),         1),
      P90    = round(quantile(get(score_col), 0.90), 1),
      P75    = round(quantile(get(score_col), 0.75), 1),
      P25    = round(quantile(get(score_col), 0.25), 1)
    ), by = Player]
    
    # MMA: also pull WinProb (implied win odds) from metadata
    winprob_col <- if (!is.null(rv$sport) && rv$sport == "MMA" &&
                       "WinProb" %in% names(meta)) "WinProb" else NULL
    
    meta_cols  <- intersect(c("Player", salary_col, own_col, winprob_col), names(meta))
    sport_cols <- intersect(
      c("Starting","Team","Car","Position","Opponent","Game","Match",
        "Surface","Tour","TeeTimeGroup","CutProb","Pool"),
      names(meta))
    all_meta    <- c(meta_cols, sport_cols)
    projections <- merge(projections, meta[, ..all_meta], by="Player", all.x=TRUE)
    
    if (salary_col %in% names(projections)) setnames(projections, salary_col, "Salary")
    if (own_col    %in% names(projections)) {
      setnames(projections, own_col, "OwnProj")
      projections[, Own := round(OwnProj * 100, 1)]
      projections[, OwnProj := NULL]
    }
    
    base_cols      <- c("Player","Salary","Own")
    if (!is.null(winprob_col) && winprob_col %in% names(projections))
      base_cols <- c(base_cols, "WinProb")
    sport_specific <- intersect(
      c("Starting","Team","Car","Position","Opponent","Game",
        "Match","Surface","Tour","TeeTimeGroup","CutProb","Pool"),
      names(projections))
    stats_cols <- c("Avg","Median","P90","P75","P25")
    
    # CBB: inject RGProj/RGMin from metadata between identity cols and sim stats
    rg_cols <- character(0)
    if (isTRUE(rv$sport == "CBB")) {
      rg_avail <- intersect(c("RGProj","RGMin"), names(meta))
      if (length(rg_avail) > 0) {
        projections <- merge(projections, meta[, c("Player", rg_avail), with=FALSE],
                             by="Player", all.x=TRUE)
        rg_cols <- rg_avail
      }
    }
    
    final_cols <- intersect(c(base_cols, sport_specific, rg_cols, stats_cols), names(projections))
    setcolorder(projections, final_cols)
    setorder(projections, -Avg)
    
    is_cbb <- isTRUE(rv$sport == "CBB")
    dt <- datatable(projections,
                    filter  = if (is_cbb) "top" else "none",
                    options = list(
                      pageLength   = if (is_cbb) 25 else 50,
                      scrollX      = TRUE,
                      scrollY      = if (is_cbb) NULL else "500px",
                      searching    = is_cbb,
                      lengthChange = is_cbb,
                      lengthMenu   = if (is_cbb) list(c(25,50,100,200), c("25","50","100","200")) else NULL,
                      dom          = if (is_cbb) "lftp" else "t",
                      order        = list(list(which(names(projections)=="Avg")-1,"desc")),
                      columnDefs   = list(list(className="dt-right",
                                               targets=which(names(projections) %in%
                                                               c(stats_cols,"Own","WinProb",rg_cols))-1))),
                    rownames=FALSE, class="stripe hover compact") %>%
      formatRound(intersect(c("Avg","Median","P90","P75","P25","Own","CutProb"),
                            names(projections)), 1)
    
    if (length(rg_cols) > 0)
      dt <- dt %>% formatRound(rg_cols, 1)
    
    if (!is.null(winprob_col) && "WinProb" %in% names(projections))
      dt <- dt %>% formatPercentage("WinProb", digits = 1)
    
    cap <- rv$config$salary_caps[[platform]] %||% 50000
    if ("Salary" %in% names(projections) && cap >= 1000)
      dt <- dt %>% formatCurrency("Salary","$",digits=0)
    dt
  })
  
  
  # ==========================================================================
  # SPORT-SPECIFIC VISUALIZATIONS
  # ==========================================================================
  
  output$sport_specific_visuals_ui <- renderUI({
    req(rv$sport)
    # CBB drives visuals from simulation_results directly, not sport_visuals
    if (rv$sport == "CBB") {
      req(rv$simulation_results)
      return(render_cbb_visuals())
    }
    req(rv$sport_visuals)
    if      (rv$sport == "TENNIS")  render_tennis_visuals(rv$sport_visuals)
    else if (rv$sport == "NASCAR")  render_nascar_visuals(rv$sport_visuals, input$sim_results_platform)
    else if (rv$sport == "GOLF")    render_golf_visuals(rv$sport_visuals)
    else if (rv$sport == "MMA")     render_mma_visuals(rv$sport_visuals)
    else if (rv$sport == "F1")      render_f1_visuals(rv$sport_visuals)
    else NULL
  })
  
  
  # ---------- Tennis ----------
  
  render_tennis_visuals <- function(visuals) {
    fluidRow(column(12,
                    box(width=NULL, title="TENNIS SIMULATION ANALYSIS", status="primary", solidHeader=TRUE,
                        tabsetPanel(id="tennis_visuals_tabs", type="tabs",
                                    tabPanel("Match Analysis",    div(style="margin-top:15px;"),
                                             DTOutput("tennis_match_analysis_table") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("All Wins",          div(style="margin-top:15px;"),
                                             plotlyOutput("tennis_all_wins_plot",        height="600px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Straight Sets",     div(style="margin-top:15px;"),
                                             plotlyOutput("tennis_ss_wins_plot",         height="600px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Non-Straight Sets", div(style="margin-top:15px;"),
                                             plotlyOutput("tennis_nss_wins_plot",        height="600px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Salary Analysis",   div(style="margin-top:15px;"),
                                             plotlyOutput("tennis_salary_analysis_plot", height="500px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6))
                        )
                    )
    ))
  }
  
  output$tennis_match_analysis_table <- renderDT({
    req(rv$sport=="TENNIS", rv$sport_visuals$match_analysis)
    datatable(rv$sport_visuals$match_analysis,
              options=list(pageLength=50, scrollX=TRUE, scrollY="500px", searching=FALSE,
                           lengthChange=FALSE, paging=FALSE, dom="t",
                           order=list(list(5,'desc')),
                           columnDefs=list(list(className="dt-right",targets=2:8),
                                           list(width="200px",targets=0))),
              rownames=FALSE,
              colnames=c("Match","Player","Salary","Imp Win%","Sim Win%","Diff","Imp SS%","Sim SS%","Avg (Wins)"),
              class="stripe hover compact nowrap") %>%
      formatCurrency("Salary","$",digits=0) %>%
      formatPercentage(c("ImpliedWin","SimWin","ImpliedSS","SimSS"), 1) %>%
      formatRound(c("WinDiff","AvgWinPts"), 1) %>%
      formatStyle("WinDiff",
                  backgroundColor=styleInterval(c(-5,5), c("#ffcccc","#ffffff","#ccffcc")))
  })
  
  make_tennis_box_plot <- function(data_path, title, color_hex) {
    function() {
      req(rv$sport=="TENNIS")
      plot_data <- rv$sport_visuals$score_distributions[[data_path]]
      req(plot_data)
      setDT(plot_data)
      top_players <- plot_data[, .(Avg=mean(Score)), by=Player][order(-Avg)][1:min(10,.N)]$Player
      plot_data   <- as.data.frame(plot_data[Player %in% top_players])
      plot_data$Player <- factor(plot_data$Player, levels=rev(top_players))
      plot_ly(data=plot_data, x=~Score, y=~Player, type="box", orientation="h",
              marker=list(color=color_hex), line=list(color=color_hex),
              fillcolor=paste0(substr(color_hex,1,7),"4D")) %>%
        layout(
          title=list(text=title, font=list(color="#FFE500",size=16)),
          xaxis=list(title="DK Fantasy Points", gridcolor="#404040", color="#FFFFFF"),
          yaxis=list(title="", color="#FFFFFF"),
          paper_bgcolor="#121212", plot_bgcolor="#1e1e1e",
          font=list(color="#FFFFFF",size=12), showlegend=FALSE, height=600)
    }
  }
  output$tennis_all_wins_plot    <- renderPlotly(make_tennis_box_plot("all_wins","All Winning Scores (Top 10)","#FFE500")())
  output$tennis_ss_wins_plot     <- renderPlotly(make_tennis_box_plot("ss_wins","Straight Sets Wins (Top 10)","#00FF00")())
  output$tennis_nss_wins_plot    <- renderPlotly(make_tennis_box_plot("nss_wins","Non-Straight Sets Wins (Top 10)","#FF8C00")())
  
  output$tennis_salary_analysis_plot <- renderPlotly({
    req(rv$sport=="TENNIS", rv$sport_visuals$score_distributions$all_wins,
        rv$sport_visuals$player_data)
    avg_scores  <- rv$sport_visuals$score_distributions$all_wins[, .(AvgWinScore=mean(Score)), by=Player]
    player_info <- unique(rv$sport_visuals$player_data[, .(Player=Name, Salary)])
    plot_data   <- as.data.frame(merge(avg_scores, player_info, by="Player"))
    plot_ly(data=plot_data, x=~Salary, y=~AvgWinScore, text=~Player,
            type="scatter", mode="markers+text",
            marker=list(size=10, color="#FFE500", line=list(color="#000000",width=1)),
            textposition="top center", textfont=list(color="#FFFFFF",size=10)) %>%
      layout(
        title=list(text="Average Win Score vs Salary", font=list(color="#FFE500",size=16)),
        xaxis=list(title="Salary ($)", gridcolor="#404040", color="#FFFFFF"),
        yaxis=list(title="Avg Win Score (DK Points)", gridcolor="#404040", color="#FFFFFF"),
        paper_bgcolor="#121212", plot_bgcolor="#1e1e1e",
        font=list(color="#FFFFFF"), height=500)
  })
  
  
  # ---------- NASCAR ----------
  
  render_nascar_visuals <- function(visuals, platform) {
    fluidRow(column(12,
                    box(width=NULL, title="NASCAR SIMULATION ANALYSIS", status="primary", solidHeader=TRUE,
                        tabsetPanel(id="nascar_visuals_tabs", type="tabs",
                                    tabPanel("Finishing Position", div(style="margin-top:15px;"),
                                             plotlyOutput("finish_distribution_plot",  height="600px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Dominator by Driver", div(style="margin-top:15px;"),
                                             plotlyOutput("dominator_violin_driver",   height="600px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Dominator by Position",
                                             div(style="margin-top:15px; margin-bottom:15px;",
                                                 radioButtons("dominator_position_group", label="Group By:",
                                                              choices=c("Starting Position"="start","Finish Position"="finish"),
                                                              selected="start", inline=TRUE)),
                                             plotlyOutput("dominator_violin_position", height="450px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6))
                        )
                    )
    ))
  }
  
  output$finish_distribution_plot <- renderPlotly({
    req(rv$sport=="NASCAR", rv$sport_visuals$full_results, input$sim_results_platform)
    tryCatch({
      plot_data       <- copy(rv$sport_visuals$full_results)
      driver_order    <- plot_data[, .(Starting=unique(Starting)), by=Name]
      setorder(driver_order, Starting)
      ordered_drivers <- driver_order$Name
      plot_data       <- as.data.frame(plot_data)
      plot_data$Name  <- factor(plot_data$Name, levels=rev(ordered_drivers))
      plot_height     <- max(600, length(ordered_drivers)*25)
      plot_ly(data=plot_data, x=~FinishPosition, y=~Name, type="box", orientation="h",
              marker=list(color="#FFE500"), line=list(color="#FFE500"),
              fillcolor="rgba(255,229,0,0.3)",
              hovertemplate="<b>%{y}</b><br>Median: %{x}<br><extra></extra>") %>%
        layout(
          title=list(text="Finishing Position Distribution",font=list(color="#FFE500",size=16)),
          xaxis=list(title="Finish Position",gridcolor="#404040",dtick=5,
                     showgrid=TRUE,range=c(0,41),color="#FFFFFF"),
          yaxis=list(title="",categoryorder="array",
                     categoryarray=rev(ordered_drivers),color="#FFFFFF"),
          paper_bgcolor="#121212", plot_bgcolor="#1e1e1e",
          font=list(color="#FFFFFF",size=12), showlegend=FALSE,
          height=plot_height, margin=list(l=150,r=50,t=50,b=50)) %>%
        config(displayModeBar=TRUE,
               modeBarButtonsToRemove=c("select2d","lasso2d","autoScale2d"),
               displaylogo=FALSE)
    }, error=function(e) {
      plotly_empty() %>% layout(
        title=list(text=paste("Error:",e$message),font=list(color="#FFE500")),
        paper_bgcolor="#121212",plot_bgcolor="#1e1e1e")
    })
  })
  
  output$dominator_violin_driver <- renderPlotly({
    req(rv$sport=="NASCAR", rv$sport_visuals$full_results, input$sim_results_platform)
    tryCatch({
      dom_col    <- if (input$sim_results_platform=="DK") "DKDominatorPoints" else "FDDominatorPoints"
      driver_avg <- rv$sport_visuals$full_results[, .(AvgDom=mean(get(dom_col))), by=Name]
      setorder(driver_avg, -AvgDom)
      top_drivers <- head(driver_avg$Name, 15)
      plot_data   <- as.data.frame(rv$sport_visuals$full_results[Name %in% top_drivers])
      medians     <- rv$sport_visuals$full_results[Name %in% top_drivers,
                                                   .(Median=median(get(dom_col))), by=Name]
      setorder(medians, -Median)
      plot_data$Name      <- factor(plot_data$Name, levels=rev(medians$Name))
      plot_data$DomPoints <- plot_data[[dom_col]]
      plot_ly(data=plot_data, x=~DomPoints, y=~Name, type="box", orientation="h",
              marker=list(color="#FFE500"), line=list(color="#FFE500"),
              fillcolor="rgba(255,229,0,0.3)",
              hovertemplate="<b>%{y}</b><br>Median: %{x}<br><extra></extra>") %>%
        layout(
          title=list(text=paste(input$sim_results_platform,"Dominator - Top 15"),
                     font=list(color="#FFE500",size=16)),
          xaxis=list(title="Dominator Points",gridcolor="#404040",showgrid=TRUE,color="#FFFFFF"),
          yaxis=list(title="",categoryorder="array",
                     categoryarray=rev(medians$Name),color="#FFFFFF"),
          paper_bgcolor="#121212", plot_bgcolor="#1e1e1e",
          font=list(color="#FFFFFF",size=12), showlegend=FALSE, height=600,
          margin=list(l=150,r=50,t=50,b=50)) %>%
        config(displayModeBar=TRUE,
               modeBarButtonsToRemove=c("select2d","lasso2d","autoScale2d"),
               displaylogo=FALSE)
    }, error=function(e) {
      plotly_empty() %>% layout(
        title=list(text=paste("Error:",e$message),font=list(color="#FFE500")),
        paper_bgcolor="#121212",plot_bgcolor="#1e1e1e")
    })
  })
  
  output$dominator_violin_position <- renderPlotly({
    req(rv$sport=="NASCAR", rv$sport_visuals$full_results, input$sim_results_platform)
    tryCatch({
      create_dominator_violin_by_position(
        rv$sport_visuals$full_results,
        platform = input$sim_results_platform,
        group_by = input$dominator_position_group)
    }, error=function(e) {
      plotly_empty() %>% layout(
        title=list(text=paste("Error:",e$message),font=list(color="#FFE500")),
        paper_bgcolor="#121212",plot_bgcolor="#1e1e1e")
    })
  })
  
  
  # ---------- MMA ----------
  
  render_mma_visuals <- function(visuals) {
    req(visuals)
    fluidRow(column(12,
                    box(width=NULL, title="MMA SIMULATION ANALYSIS", status="primary", solidHeader=TRUE,
                        tabsetPanel(id="mma_visuals_tabs", type="tabs",
                                    tabPanel("Outcome Distribution", div(style="margin-top:15px;"),
                                             plotlyOutput("mma_outcome_dist_plot", height="1100px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Win Score Distribution", div(style="margin-top:15px;"),
                                             plotlyOutput("mma_score_dist_plot", height="800px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6))
                        )
                    )
    ))
  }
  
  # --- MMA: Outcome distribution stacked bar ---
  output$mma_outcome_dist_plot <- renderPlotly({
    req(rv$sport == "MMA", rv$sport_visuals$outcome_pct, rv$sport_visuals$fighter_summary,
        input$sim_results_platform)
    
    platform <- input$sim_results_platform
    op  <- copy(rv$sport_visuals$outcome_pct)
    fs  <- copy(rv$sport_visuals$fighter_summary)
    setDT(op); setDT(fs)
    
    # Platform-specific salary column and label format (no win %)
    sal_col <- switch(platform,
                      "DK" = "DKSalary",
                      "FD" = "FDSalary",
                      "SD" = "SDSalary"
    )
    
    # For Showdown: filter to SD-eligible fighters only
    if (platform == "SD") {
      eligible <- fs[!is.na(SDSalary) & SDSalary > 0, Player]
      fs <- fs[Player %in% eligible]
      op <- op[Player %in% eligible]
    }
    
    # Sort by selected platform salary descending -> highest at top
    setorderv(fs, sal_col, order = -1L)
    
    # Build label: "Name ($SAL)" for the selected platform only
    fs[, Label := sprintf("%s ($%s)", Player, format(get(sal_col), big.mark = ","))]
    
    name_to_label <- setNames(fs$Label, fs$Player)
    op[, YLabel := name_to_label[Player]]
    
    # label_order: already desc salary, plot reverses for top-to-bottom display
    label_order <- fs$Label
    
    outcome_order <- c("QuickWin_R1","R1 Finish","R2 Finish","R3 Finish","R4 Finish","R5 Finish","Decision")
    win_colors <- c(
      "QuickWin_R1" = "#9932CC",
      "R1 Finish"   = "#1E90FF",
      "R2 Finish"   = "#32CD32",
      "R3 Finish"   = "#FF8C00",
      "R4 Finish"   = "maroon",
      "R5 Finish"   = "#FFFF00",
      "Decision"    = "#DC143C"
    )
    
    p <- plot_ly(height = 1100)
    for (oc in outcome_order) {
      d <- op[Outcome == oc]
      if (nrow(d) == 0) next
      p <- add_trace(p,
                     x = d$WinPct, y = d$YLabel,
                     name = oc, type = "bar", orientation = "h",
                     marker = list(color = win_colors[oc]),
                     hovertemplate = paste0("<b>%{y}</b><br>", oc, ": %{x:.1f}%<extra></extra>")
      )
    }
    
    p %>% layout(
      barmode = "stack",
      title   = list(text = "Win Method Distribution", font = list(color = "#FFE500", size = 16)),
      xaxis   = list(title = "Win Percentage (%)", gridcolor = "#404040", color = "#FFFFFF"),
      yaxis   = list(title = "", categoryorder = "array",
                     categoryarray = rev(label_order), color = "#FFFFFF"),
      paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e",
      font   = list(color = "#FFFFFF", size = 11),
      legend = list(orientation = "h", y = -0.12),
      margin = list(l = 220, r = 30, t = 60, b = 80)
    )
  })
  
  # --- MMA: Win score distribution - reacts to platform radio buttons at top of page ---
  output$mma_score_dist_plot <- renderPlotly({
    req(rv$sport == "MMA", rv$sport_visuals$score_dist, rv$sport_visuals$player_data,
        input$sim_results_platform)
    
    platform <- input$sim_results_platform
    
    # Platform config: score column, salary column for ordering, color, title
    cfg <- switch(platform,
                  "DK" = list(score_col="DKScore", sal_col="DKSalary", color="#FFE500",
                              title="DK Win Score Distribution"),
                  "FD" = list(score_col="FDScore", sal_col="FDSalary", color="#FFE500",
                              title="FD Win Score Distribution"),
                  "SD" = list(score_col="DKScore", sal_col="SDSalary", color="#FFE500",
                              title="Showdown Win Score Distribution (DK Scoring)")
    )
    
    sd_data <- copy(rv$sport_visuals$score_dist)
    meta    <- copy(rv$sport_visuals$player_data)
    setDT(sd_data); setDT(meta)
    
    # Showdown: filter to SD-eligible fighters (SDSalary > 0)
    if (platform == "SD") {
      eligible <- meta[!is.na(SDSalary) & SDSalary > 0, Player]
      sd_data  <- sd_data[Player %in% eligible]
      meta     <- meta[Player %in% eligible]
    }
    
    wins <- sd_data[Win == 1L]
    if (nrow(wins) == 0) return(plotly_empty() %>%
                                  layout(title=list(text="No win data", font=list(color="#FFE500")),
                                         paper_bgcolor="#121212", plot_bgcolor="#1e1e1e"))
    
    # Sort by salary ascending (cheapest at bottom of horizontal chart)
    sal_col  <- cfg$sal_col
    sal_order <- meta[order(get(sal_col)), Player]
    wins[, Player := factor(Player, levels = sal_order)]
    wins <- as.data.frame(wins)
    
    plot_ly(data = wins, x = wins[[cfg$score_col]], y = ~Player,
            height = 800, type = "box", orientation = "h",
            marker    = list(color = cfg$color),
            line      = list(color = cfg$color),
            fillcolor = paste0(substr(cfg$color, 1, 7), "40")) %>%
      layout(
        title  = list(text = cfg$title, font = list(color = "#FFE500", size = 16)),
        xaxis  = list(title = "Fantasy Points (Wins Only)", gridcolor = "#404040", color = "#FFFFFF"),
        yaxis  = list(title = "", color = "#FFFFFF"),
        paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e",
        font = list(color = "#FFFFFF", size = 11),
        showlegend = FALSE,
        margin = list(l = 160, r = 30, t = 60, b = 50)
      )
  })
  
  # ---------- Golf ----------
  
  render_golf_visuals <- function(visuals) {
    req(visuals)
    fluidRow(column(12,
                    box(width=NULL, title="GOLF SIMULATION ANALYSIS", status="primary", solidHeader=TRUE,
                        tabsetPanel(id="golf_visuals_tabs", type="tabs",
                                    tabPanel("Score Distribution", div(style="margin-top:15px;"),
                                             plotlyOutput("golf_score_dist_plot", height="600px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Cut Rates", div(style="margin-top:15px;"),
                                             DTOutput("golf_cut_rates_table") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Salary Analysis", div(style="margin-top:15px;"),
                                             plotlyOutput("golf_salary_plot", height="500px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6))
                        )
                    )
    ))
  }
  
  output$golf_score_dist_plot <- renderPlotly({
    req(rv$sport=="GOLF", rv$simulation_results)
    tryCatch({
      setDT(rv$simulation_results)
      avg_scores  <- rv$simulation_results[, .(Avg=mean(DKScore)), by=Player]
      setorder(avg_scores, -Avg)
      top_players <- head(avg_scores$Player, 20)
      plot_data   <- as.data.frame(rv$simulation_results[Player %in% top_players])
      plot_data$Player <- factor(plot_data$Player, levels=rev(top_players))
      plot_ly(data=plot_data, x=~DKScore, y=~Player, type="box", orientation="h",
              marker=list(color="#FFE500"), line=list(color="#FFE500"),
              fillcolor="rgba(255,229,0,0.3)") %>%
        layout(
          title=list(text="DK Score Distribution (Top 20 Golfers)",
                     font=list(color="#FFE500",size=16)),
          xaxis=list(title="DK Fantasy Points", gridcolor="#404040", color="#FFFFFF"),
          yaxis=list(title="", color="#FFFFFF"),
          paper_bgcolor="#121212", plot_bgcolor="#1e1e1e",
          font=list(color="#FFFFFF",size=12), showlegend=FALSE, height=600,
          margin=list(l=180,r=50,t=50,b=50))
    }, error=function(e) {
      plotly_empty() %>% layout(
        title=list(text=paste("Error:",e$message),font=list(color="#FFE500")),
        paper_bgcolor="#121212",plot_bgcolor="#1e1e1e")
    })
  })
  
  output$golf_cut_rates_table <- renderDT({
    req(rv$sport=="GOLF", rv$sim_metadata)
    meta         <- copy(rv$sim_metadata)
    setDT(meta)
    display_cols <- intersect(c("Player","Pool","TeeTimeGroup","CutProb",
                                "DKSalary","DKOwn","FDSalary","FDOwn"),
                              names(meta))
    meta <- meta[, ..display_cols]
    if ("CutProb" %in% names(meta)) meta[, CutProb := round(CutProb * 100, 1)]
    if ("DKOwn"   %in% names(meta)) meta[, DKOwn   := round(DKOwn   * 100, 1)]
    if ("FDOwn"   %in% names(meta)) meta[, FDOwn   := round(FDOwn   * 100, 1)]
    if ("CutProb" %in% names(meta)) setorder(meta, -CutProb)
    dt <- datatable(meta,
                    options=list(pageLength=50, scrollX=TRUE, searching=FALSE,
                                 lengthChange=FALSE, dom='tp'),
                    rownames=FALSE)
    if ("DKSalary" %in% names(meta)) dt <- dt %>% formatCurrency("DKSalary","$",digits=0)
    if ("FDSalary" %in% names(meta)) dt <- dt %>% formatCurrency("FDSalary","$",digits=0)
    dt
  })
  
  output$golf_salary_plot <- renderPlotly({
    req(rv$sport=="GOLF", rv$sim_metadata)
    meta <- copy(rv$sim_metadata)
    setDT(meta)
    if (!("DKSalary" %in% names(meta) && "CutProb" %in% names(meta)))
      return(plotly_empty())
    meta[, CutPct := round(CutProb * 100, 1)]
    plot_data <- as.data.frame(meta[DKSalary > 0])
    plot_ly(data=plot_data, x=~DKSalary, y=~CutPct, text=~Player,
            type="scatter", mode="markers+text",
            marker=list(size=10, color="#FFE500", line=list(color="#000000",width=1)),
            textposition="top center", textfont=list(color="#FFFFFF",size=9)) %>%
      layout(
        title=list(text="Cut Rate vs DK Salary", font=list(color="#FFE500",size=16)),
        xaxis=list(title="DK Salary ($)", gridcolor="#404040", color="#FFFFFF"),
        yaxis=list(title="Cut Rate (%)", gridcolor="#404040", color="#FFFFFF"),
        paper_bgcolor="#121212", plot_bgcolor="#1e1e1e",
        font=list(color="#FFFFFF"), height=500)
  })
  
  # ---------- CBB ----------
  
  render_cbb_visuals <- function() {
    req(rv$sim_metadata)
    teams <- sort(unique(rv$sim_metadata$Team))
    tabs  <- lapply(teams, function(tm) {
      tabPanel(tm,
               div(style="margin-top:10px;"),
               uiOutput(paste0("cbb_plot_ui_", tm))
      )
    })
    fluidRow(column(12,
                    box(width=NULL, title="COLLEGE BASKETBALL SIMULATION ANALYSIS",
                        status="primary", solidHeader=TRUE,
                        do.call(tabsetPanel, c(list(id="cbb_team_tabs", type="tabs"), tabs))
                    )
    ))
  }
  
  # Register a plot output for each team dynamically
  observe({
    req(rv$sport=="CBB", rv$simulation_results, rv$sim_metadata)
    sim  <- isolate(rv$simulation_results)
    meta <- isolate(rv$sim_metadata)
    teams <- sort(unique(meta$Team))
    
    lapply(teams, function(tm) {
      local({
        local_tm <- tm
        ui_id    <- paste0("cbb_plot_ui_", local_tm)
        plot_id  <- paste0("cbb_plot_", local_tm)
        
        output[[ui_id]] <- renderUI({
          n_players <- nrow(meta[meta$Team == local_tm, ])
          height    <- max(300, n_players * 58)
          plotlyOutput(plot_id, height=paste0(height, "px")) %>%
            shinycssloaders::withSpinner(color="#FFE500", type=6)
        })
        
        output[[plot_id]] <- renderPlotly({
          tryCatch({
            setDT(sim); setDT(meta)
            sim_t <- merge(sim, meta[, .(Player, Team)], by="Player", all.x=TRUE)
            td    <- sim_t[Team == local_tm]
            
            if (nrow(td) == 0) return(plotly_empty() %>% layout(
              paper_bgcolor="#121212", plot_bgcolor="#1e1e1e"))
            
            # Order by avg DK score ascending (highest at top of horizontal chart)
            avg_ord <- td[, .(avg=mean(DKScore, na.rm=TRUE)), by=Player]
            setorder(avg_ord, avg)
            td[, Player := factor(Player, levels=avg_ord$Player)]
            
            plot_ly(data=as.data.frame(td),
                    x=~DKScore, y=~Player,
                    type="box", orientation="h",
                    marker    = list(color="#FFE500"),
                    line      = list(color="#FFE500"),
                    fillcolor = "rgba(255,229,0,0.18)") %>%
              layout(
                title        = list(text=paste0(local_tm, " — DK Fantasy Points"),
                                    font=list(color="#FFE500", size=14)),
                xaxis        = list(title="DK Points", gridcolor="#404040",
                                    color="#FFFFFF", zeroline=FALSE),
                yaxis        = list(title="", color="#FFFFFF",
                                    tickfont=list(size=11), automargin=TRUE),
                paper_bgcolor = "#121212",
                plot_bgcolor  = "#1e1e1e",
                font          = list(color="#FFFFFF", size=11),
                showlegend    = FALSE
              )
          }, error=function(e) {
            plotly_empty() %>% layout(
              title=list(text=paste("Error:", e$message), font=list(color="#FFE500")),
              paper_bgcolor="#121212", plot_bgcolor="#1e1e1e")
          })
        })
      })
    })
  })
  
  output$cbb_stat_breakdown_table <- renderDT({
    req(rv$sport=="CBB", rv$simulation_results, rv$sim_metadata)
    sim  <- copy(rv$simulation_results); setDT(sim)
    meta <- copy(rv$sim_metadata);       setDT(meta)
    
    # Per-player averages across all stats
    stat_cols <- intersect(c("pts","tpm","reb","ast","stl","blk","to"), names(sim))
    agg_exprs <- c(
      list(AvgDK  = quote(round(mean(DKScore, na.rm=TRUE), 1)),
           MedDK  = quote(round(median(DKScore, na.rm=TRUE), 1)),
           P90DK  = quote(round(quantile(DKScore, .90, na.rm=TRUE), 1))),
      setNames(lapply(stat_cols, function(s)
        bquote(round(mean(.(as.name(s)), na.rm=TRUE), 1))), paste0("Avg_", stat_cols))
    )
    avg_stats <- sim[, lapply(agg_exprs, eval), by=Player]
    
    # Pull identity + RG columns from metadata
    rg_meta_cols <- intersect(c("Player","DKSalary","Team","PosGroup","DKOwn","RGProj","RGMin"), names(meta))
    avg_stats <- merge(avg_stats, meta[, ..rg_meta_cols], by="Player", all.x=TRUE)
    avg_stats[, Value := round(AvgDK / (DKSalary / 1000), 2)]
    setorder(avg_stats, -AvgDK)
    
    # Column order: identity → RG projections → our sim outputs
    id_cols  <- intersect(c("Player","Team","PosGroup","DKSalary","DKOwn"), names(avg_stats))
    rg_cols  <- intersect(c("RGProj","RGMin"), names(avg_stats))
    sim_cols <- intersect(c("AvgDK","MedDK","P90DK","Value",
                            paste0("Avg_", c("pts","tpm","reb","ast","stl","blk","to"))),
                          names(avg_stats))
    setcolorder(avg_stats, c(id_cols, rg_cols, sim_cols))
    
    dt <- datatable(avg_stats,
                    filter = "top",
                    options = list(
                      pageLength = 25,
                      scrollX    = TRUE,
                      searching  = TRUE,
                      lengthMenu = c(25, 50, 100, 200),
                      lengthChange = TRUE,
                      dom        = "lftp"
                    ),
                    rownames = FALSE) %>%
      formatCurrency("DKSalary", "$", digits=0) %>%
      formatStyle(c("AvgDK","MedDK"), color="#FFE500", fontWeight="bold") %>%
      formatStyle("Value",
                  backgroundColor=styleInterval(c(4,5), c("#1e1e1e","#2a3a1e","#1a4a1a")))
    if (length(rg_cols) > 0)
      dt <- dt %>% formatRound(rg_cols, 1)
    dt
  })
  
  
  # ============================================================================
  # F1 VISUALS
  # ============================================================================
  
  render_f1_visuals <- function(visuals) {
    fluidRow(column(12,
                    box(width=NULL, title="F1 SIMULATION ANALYSIS", status="primary", solidHeader=TRUE,
                        tabsetPanel(id="f1_visuals_tabs", type="tabs",
                                    tabPanel("Finishing Position", div(style="margin-top:15px;"),
                                             plotlyOutput("f1_finish_dist_plot", height="600px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Fantasy Points", div(style="margin-top:15px;"),
                                             plotlyOutput("f1_fp_dist_plot", height="600px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Laps Led", div(style="margin-top:15px;"),
                                             plotlyOutput("f1_dominator_plot", height="500px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Constructors", div(style="margin-top:15px;"),
                                             plotlyOutput("f1_constructor_plot", height="600px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Driver Stats", div(style="margin-top:15px;"),
                                             DTOutput("f1_driver_stats_table") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Constructor Stats", div(style="margin-top:15px;"),
                                             DTOutput("f1_constructor_stats_table") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6))
                        )
                    )
    ))
  }
  
  output$f1_finish_dist_plot <- renderPlotly({
    req(rv$sport == "F1", rv$sport_visuals$driver_results, rv$sport_visuals$driver_meta)
    tryCatch({
      drv_res  <- rv$sport_visuals$driver_results
      drv_meta <- rv$sport_visuals$driver_meta
      grid_order   <- drv_meta[order(Starting), Player]
      plot_data    <- as.data.frame(drv_res)
      plot_data$Player <- factor(plot_data$Player, levels = rev(grid_order))
      plot_height  <- max(600, length(grid_order) * 28)
      plot_ly(data = plot_data, x = ~Finish, y = ~Player, type = "box", orientation = "h",
              marker  = list(color = "#FFE500"), line = list(color = "#FFE500"),
              fillcolor = "rgba(255,229,0,0.3)",
              hovertemplate = "<b>%{y}</b><br>Median: %{x}<br><extra></extra>") %>%
        layout(
          title  = list(text = "Finishing Position Distribution (Grid Order)",
                        font = list(color = "#FFE500", size = 16)),
          xaxis  = list(title = "Finish Position", gridcolor = "#404040", color = "#FFFFFF",
                        autorange = "reversed", dtick = 2),
          yaxis  = list(title = "", color = "#FFFFFF",
                        categoryorder = "array", categoryarray = rev(grid_order)),
          paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e",
          font = list(color = "#FFFFFF", size = 12), showlegend = FALSE,
          height = plot_height, margin = list(l = 160, r = 50, t = 50, b = 50)) %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
               displaylogo = FALSE)
    }, error = function(e) {
      plotly_empty() %>% layout(
        title = list(text = paste("Error:", e$message), font = list(color = "#FFE500")),
        paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e")
    })
  })
  
  output$f1_fp_dist_plot <- renderPlotly({
    req(rv$sport == "F1", rv$sport_visuals$driver_results, rv$sport_visuals$driver_meta)
    tryCatch({
      drv_res  <- rv$sport_visuals$driver_results
      drv_meta <- rv$sport_visuals$driver_meta
      sal_order  <- drv_meta[order(-DKSalary), Player]
      plot_data  <- as.data.frame(drv_res)
      plot_data$Player <- factor(plot_data$Player, levels = rev(sal_order))
      plot_height <- max(600, length(sal_order) * 28)
      plot_ly(data = plot_data, x = ~DKScore, y = ~Player, type = "box", orientation = "h",
              marker  = list(color = "#FFE500"), line = list(color = "#FFE500"),
              fillcolor = "rgba(255,229,0,0.3)",
              hovertemplate = "<b>%{y}</b><br>Median: %{x:.1f}<br><extra></extra>") %>%
        layout(
          title  = list(text = "DK Fantasy Points Distribution (Salary Order)",
                        font = list(color = "#FFE500", size = 16)),
          xaxis  = list(title = "DK Fantasy Points (Flex)", gridcolor = "#404040", color = "#FFFFFF"),
          yaxis  = list(title = "", color = "#FFFFFF",
                        categoryorder = "array", categoryarray = rev(sal_order)),
          paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e",
          font = list(color = "#FFFFFF", size = 12), showlegend = FALSE,
          height = plot_height, margin = list(l = 160, r = 50, t = 50, b = 50)) %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
               displaylogo = FALSE)
    }, error = function(e) {
      plotly_empty() %>% layout(
        title = list(text = paste("Error:", e$message), font = list(color = "#FFE500")),
        paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e")
    })
  })
  
  output$f1_dominator_plot <- renderPlotly({
    req(rv$sport == "F1", rv$sport_visuals$driver_results)
    tryCatch({
      drv_res <- rv$sport_visuals$driver_results
      ll_avg  <- drv_res[, .(Avg_LL = mean(LapsLed)), by = Player]
      ll_avg  <- ll_avg[Avg_LL > 0.01]
      if (nrow(ll_avg) == 0) {
        return(plotly_empty() %>% layout(
          title = list(text = "No laps led data", font = list(color = "#FFE500")),
          paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e"))
      }
      setorder(ll_avg, -Avg_LL)
      plot_data <- as.data.frame(ll_avg)
      plot_data$Player <- factor(plot_data$Player, levels = rev(ll_avg$Player))
      plot_ly(data = plot_data, x = ~Avg_LL, y = ~Player, type = "bar", orientation = "h",
              marker = list(color = "#FFE500", line = list(color = "#FFE500", width = 1)),
              hovertemplate = "<b>%{y}</b><br>Avg Laps Led: %{x:.1f}<br><extra></extra>") %>%
        layout(
          title  = list(text = "Laps Led - Average Per Driver",
                        font = list(color = "#FFE500", size = 16)),
          xaxis  = list(title = "Avg Laps Led", gridcolor = "#404040", color = "#FFFFFF"),
          yaxis  = list(title = "", color = "#FFFFFF",
                        categoryorder = "array", categoryarray = rev(ll_avg$Player)),
          paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e",
          font = list(color = "#FFFFFF", size = 12), showlegend = FALSE,
          height = 500, margin = list(l = 160, r = 50, t = 50, b = 50)) %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
               displaylogo = FALSE)
    }, error = function(e) {
      plotly_empty() %>% layout(
        title = list(text = paste("Error:", e$message), font = list(color = "#FFE500")),
        paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e")
    })
  })
  
  output$f1_constructor_plot <- renderPlotly({
    req(rv$sport == "F1", rv$sport_visuals$constructor_results)
    tryCatch({
      cnstr_res  <- rv$sport_visuals$constructor_results
      med_order  <- cnstr_res[, .(med = median(DKScore)), by = Player][order(-med), Player]
      plot_data  <- as.data.frame(cnstr_res)
      plot_data$Player <- factor(plot_data$Player, levels = rev(med_order))
      plot_height <- max(400, length(med_order) * 50)
      plot_ly(data = plot_data, x = ~DKScore, y = ~Player, type = "box", orientation = "h",
              marker  = list(color = "#FFE500"), line = list(color = "#FFE500"),
              fillcolor = "rgba(255,229,0,0.3)",
              hovertemplate = "<b>%{y}</b><br>Median: %{x:.1f}<br><extra></extra>") %>%
        layout(
          title  = list(text = "Constructor DK Points Distribution",
                        font = list(color = "#FFE500", size = 16)),
          xaxis  = list(title = "DK Fantasy Points", gridcolor = "#404040", color = "#FFFFFF"),
          yaxis  = list(title = "", color = "#FFFFFF",
                        categoryorder = "array", categoryarray = rev(med_order)),
          paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e",
          font = list(color = "#FFFFFF", size = 12), showlegend = FALSE,
          height = plot_height, margin = list(l = 160, r = 50, t = 50, b = 50)) %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
               displaylogo = FALSE)
    }, error = function(e) {
      plotly_empty() %>% layout(
        title = list(text = paste("Error:", e$message), font = list(color = "#FFE500")),
        paper_bgcolor = "#121212", plot_bgcolor = "#1e1e1e")
    })
  })
  
  output$f1_driver_stats_table <- renderDT({
    req(rv$sport == "F1", rv$sport_visuals$driver_analysis, rv$sport_visuals$driver_meta)
    tryCatch({
      da   <- copy(rv$sport_visuals$driver_analysis)
      meta <- rv$sport_visuals$driver_meta[, .(Player, DKSalary, CptSalary, Starting)]
      da   <- merge(da, meta, by = "Player", all.x = TRUE)
      want_cols <- c("Player","Team","Starting","DKSalary","CptSalary",
                     "Avg_DKScore","Median_DKScore","Avg_CptScore","Median_CptScore",
                     "Win_Rate","Podium_Rate","Points_Rate","Classified_Rate",
                     "Beat_TM_Rate","FL_Rate","Avg_LL",
                     "Avg_FinishPts","Avg_GridPts","Avg_FL_Pts","Avg_LL_Pts",
                     "Avg_BeatTM_Pts","Avg_Cls_Pts","Median_Finish")
      setcolorder(da, intersect(want_cols, names(da)))
      setorder(da, -Avg_DKScore)
      datatable(da, rownames = FALSE,
                options = list(pageLength = 25, scrollX = TRUE, scrollY = "500px",
                               searching = FALSE, lengthChange = FALSE, dom = "t"),
                class = "stripe hover compact") %>%
        formatCurrency(intersect(c("DKSalary","CptSalary"), names(da)), "$", digits = 0) %>%
        formatRound(intersect(c("Avg_DKScore","Median_DKScore","Avg_CptScore","Median_CptScore",
                                "Avg_FinishPts","Avg_GridPts","Avg_FL_Pts","Avg_LL_Pts",
                                "Avg_BeatTM_Pts","Avg_Cls_Pts","Median_Finish","Avg_LL"), names(da)), 1) %>%
        formatStyle("Avg_DKScore",
                    background = styleColorBar(range(da$Avg_DKScore, na.rm = TRUE), "#FFE500"),
                    backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "left")
    }, error = function(e) {
      datatable(data.table(Error = e$message), rownames = FALSE)
    })
  })
  
  output$f1_constructor_stats_table <- renderDT({
    req(rv$sport == "F1", rv$sport_visuals$constructor_analysis)
    tryCatch({
      ca <- copy(rv$sport_visuals$constructor_analysis)
      setorder(ca, -Median_Score)
      datatable(ca, rownames = FALSE,
                options = list(pageLength = 15, scrollX = TRUE,
                               searching = FALSE, lengthChange = FALSE, dom = "t"),
                class = "stripe hover compact") %>%
        formatCurrency("DKSalary", "$", digits = 0) %>%
        formatRound(c("Avg_Score","Median_Score","P75_Score","P90_Score"), 1) %>%
        formatStyle("Median_Score",
                    background = styleColorBar(range(ca$Median_Score, na.rm = TRUE), "#FFE500"),
                    backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "left")
    }, error = function(e) {
      datatable(data.table(Error = e$message), rownames = FALSE)
    })
  })
  
  
} 
 

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui, server)