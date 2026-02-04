# ============================================================================
# LINEUP BUILDER SHINY MODULE - FINAL FIXED VERSION
# For use with GoldenTicketCore
# ============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(shinyjs)
library(data.table)

# ============================================================================
# MODULE: LINEUP BUILDER UI
# ============================================================================

lineupBuilderUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    
    tags$style(HTML("
      .irs-grid-text { 
        font-size: 9px !important; 
        color: #FFD700 !important;
      }
      .irs-min, .irs-max { 
        font-size: 10px !important;
        color: #FFD700 !important;
        background: #000 !important;
      }
      .irs-single {
        display: none !important;
      }
      .irs-from, .irs-to {
        font-size: 11px !important;
        background: #FFD700 !important;
        color: #000 !important;
        font-weight: bold !important;
      }
      .irs-bar {
        background: #FFD700 !important;
        border-top: 1px solid #FFD700 !important;
        border-bottom: 1px solid #FFD700 !important;
      }
      .irs-line {
        background: #333 !important;
        border: 1px solid #555 !important;
      }
      .irs-slider {
        background: #FFD700 !important;
        border: 2px solid #000 !important;
        width: 18px !important;
        height: 18px !important;
      }
      .selectize-input {
        background: #1a1a1a !important;
        color: #FFD700 !important;
        border: 1px solid #FFD700 !important;
      }
      .selectize-dropdown {
        background: #000 !important;
        color: #FFD700 !important;
        border: 1px solid #FFD700 !important;
      }
      .selectize-dropdown-content .option {
        color: #FFD700 !important;
      }
      .selectize-dropdown-content .option.active {
        background: #FFD700 !important;
        color: #000 !important;
      }
      label {
        color: #FFD700 !important;
        font-weight: bold !important;
      }
    ")),
    
    tabsetPanel(
      id = ns("builder_tabs"),
      type = "tabs",
      
      tabPanel(
        "Filtered Pool",
        
        fluidRow(
          column(
            12,
            div(
              class = "box box-solid",
              style = "background-color: #000; border: 2px solid #FFD700; margin: 15px;",
              div(
                class = "box-header",
                style = "background-color: #000; border-bottom: 2px solid #FFD700; padding: 10px;",
                h3("Lineup Filters", style = "color: #FFD700; margin: 0; font-weight: bold;")
              ),
              div(
                class = "box-body",
                style = "background-color: #000; padding: 20px;",
                
                uiOutput(ns("filter_rows")),
                
                hr(style = "border-color: #FFD700; margin: 20px 0;"),
                
                fluidRow(
                  column(6, 
                         div(style = "padding: 10px;",
                             uiOutput(ns("locked_players_ui"))
                         )
                  ),
                  column(6, 
                         div(style = "padding: 10px;",
                             uiOutput(ns("excluded_players_ui"))
                         )
                  )
                ),
                
                hr(style = "border-color: #FFD700;"),
                
                fluidRow(
                  column(12,
                         h4(textOutput(ns("filtered_pool_count")), 
                            style = "color: #FFD700; font-weight: bold; text-align: center;")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            12,
            div(
              class = "box box-solid",
              style = "background-color: #000; border: 2px solid #FFD700; margin: 15px;",
              div(
                class = "box-header",
                style = "background-color: #000; border-bottom: 2px solid #FFD700; padding: 10px;",
                h3("Generate Lineups from Filtered Pool", style = "color: #FFD700; margin: 0; font-weight: bold;")
              ),
              div(
                class = "box-body",
                style = "background-color: #000; padding: 20px;",
                
                fluidRow(
                  column(4, 
                         numericInput(ns("num_lineups"), "Number of Lineups", 
                                      value = 20, min = 1, max = 150,
                                      width = "100%")
                  ),
                  column(4, 
                         textInput(ns("build_label"), "Build Label (optional)", 
                                   value = "",
                                   placeholder = "Auto-numbered if blank",
                                   width = "100%")
                  ),
                  column(2, 
                         br(),
                         actionButton(ns("add_build"), "Add to Portfolio", 
                                      class = "btn-warning",
                                      style = "width: 100%; background-color: #FFD700; color: #000; font-weight: bold; border: none;")
                  ),
                  column(2, 
                         br(),
                         actionButton(ns("clear_portfolio"), "Clear Portfolio", 
                                      class = "btn-danger",
                                      style = "width: 100%; background-color: #8B0000; color: #FFD700; font-weight: bold; border: none;")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            12,
            div(
              class = "box box-solid",
              style = "background-color: #000; border: 2px solid #FFD700; margin: 15px;",
              div(
                class = "box-header",
                style = "background-color: #000; border-bottom: 2px solid #FFD700; padding: 10px;",
                h3("Player Exposure in Filtered Pool", style = "color: #FFD700; margin: 0; font-weight: bold;")
              ),
              div(
                class = "box-body",
                style = "background-color: #000; padding: 20px;",
                DTOutput(ns("filtered_exposure_table"))
              )
            )
          )
        )
      ),
      
      tabPanel(
        "Portfolio Summary",
        
        fluidRow(
          column(
            12,
            div(
              class = "box box-solid",
              style = "background-color: #000; border: 2px solid #FFD700; margin: 15px;",
              div(
                class = "box-header",
                style = "background-color: #000; border-bottom: 2px solid #FFD700; padding: 10px;",
                h3("Portfolio Statistics", style = "color: #FFD700; margin: 0; font-weight: bold;")
              ),
              div(
                class = "box-body",
                style = "background-color: #000; padding: 20px;",
                
                fluidRow(
                  column(6,
                         h4(textOutput(ns("total_portfolio_count")), 
                            style = "color: #FFD700; font-weight: bold;")
                  ),
                  column(6,
                         div(style = "text-align: right;",
                             downloadButton(ns("download_portfolio"), "Download Portfolio CSV", 
                                            style = "background-color: #FFD700; color: #000; font-weight: bold; border: none;")
                         )
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            12,
            div(
              class = "box box-solid",
              style = "background-color: #000; border: 2px solid #FFD700; margin: 15px;",
              div(
                class = "box-header",
                style = "background-color: #000; border-bottom: 2px solid #FFD700; padding: 10px;",
                h3("Builds Summary", style = "color: #FFD700; margin: 0; font-weight: bold;")
              ),
              div(
                class = "box-body",
                style = "background-color: #000; padding: 20px;",
                DTOutput(ns("builds_summary_table"))
              )
            )
          )
        ),
        
        fluidRow(
          column(
            12,
            div(
              class = "box box-solid",
              style = "background-color: #000; border: 2px solid #FFD700; margin: 15px;",
              div(
                class = "box-header",
                style = "background-color: #000; border-bottom: 2px solid #FFD700; padding: 10px;",
                h3("Portfolio Player Exposure", style = "color: #FFD700; margin: 0; font-weight: bold;")
              ),
              div(
                class = "box-body",
                style = "background-color: #000; padding: 20px;",
                DTOutput(ns("portfolio_exposure_table"))
              )
            )
          )
        )
      ),
      
      tabPanel(
        "Portfolio Lineups",
        fluidRow(
          column(
            12,
            div(
              class = "box box-solid",
              style = "background-color: #000; border: 2px solid #FFD700; margin: 15px;",
              div(
                class = "box-header",
                style = "background-color: #000; border-bottom: 2px solid #FFD700; padding: 10px;",
                h3("All Portfolio Lineups", style = "color: #FFD700; margin: 0; font-weight: bold;")
              ),
              div(
                class = "box-body",
                style = "background-color: #000; padding: 20px;",
                DTOutput(ns("portfolio_lineups_table"))
              )
            )
          )
        )
      )
    )
  )
}

# ============================================================================
# MODULE: LINEUP BUILDER SERVER
# ============================================================================

lineupBuilderServer <- function(id, optimal_lineups, config, driver_details = NULL, simulation_results = NULL) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(
      builds = list(),
      portfolio = NULL,
      build_counter = 0
    )
    
    # ========================================================================
    # DYNAMIC FILTER UI
    # ========================================================================
    
    output$filter_rows <- renderUI({
      req(config$filter_options$numeric_filters, optimal_lineups())
      
      ns <- session$ns
      filters <- config$filter_options$numeric_filters
      lineups <- optimal_lineups()
      
      # Clean column names
      colnames(lineups) <- make.names(colnames(lineups), unique = TRUE)
      
      filter_col_map <- list()
      for (f in filters) {
        clean_name <- make.names(f)
        filter_col_map[[f]] <- clean_name
      }
      
      num_filters <- length(filters)
      num_rows <- ceiling(num_filters / 3)
      
      filter_rows <- lapply(1:num_rows, function(row_num) {
        start_idx <- (row_num - 1) * 3 + 1
        end_idx <- min(row_num * 3, num_filters)
        row_filters <- filters[start_idx:end_idx]
        
        fluidRow(
          lapply(row_filters, function(filter_name) {
            clean_col <- filter_col_map[[filter_name]]
            
            if (clean_col %in% names(lineups)) {
              filter_values <- lineups[[clean_col]]
              min_val <- floor(min(filter_values, na.rm = TRUE) * 100) / 100
              max_val <- ceiling(max(filter_values, na.rm = TRUE) * 100) / 100
              
              if (grepl("Salary", filter_name)) {
                step_val <- 100
              } else if (grepl("Pct|Rate|Own", filter_name)) {
                step_val <- 0.01
              } else {
                step_val <- 0.1
              }
              
              column(
                width = 4,
                div(
                  style = "padding: 10px;",
                  sliderInput(
                    ns(paste0("range_", clean_col)),
                    filter_name,
                    min = min_val,
                    max = max_val,
                    value = c(min_val, max_val),
                    step = step_val,
                    width = "100%"
                  )
                )
              )
            }
          })
        )
      })
      
      do.call(tagList, filter_rows)
    })
    
    # FIXED: Locked players UI with proper data.table syntax
    output$locked_players_ui <- renderUI({
      req(optimal_lineups())
      
      ns <- session$ns
      player_cols <- config$player_columns
      
      # FIXED: Use .. prefix for data.table
      lineups_dt <- as.data.table(optimal_lineups())
      all_players <- unique(unlist(lineups_dt[, ..player_cols]))
      
      selectizeInput(ns("locked_players"),
                     "Locked Players (must be in lineup)",
                     choices = sort(all_players),
                     multiple = TRUE,
                     options = list(placeholder = 'Select players to lock'))
    })
    
    # FIXED: Excluded players UI with proper data.table syntax
    output$excluded_players_ui <- renderUI({
      req(optimal_lineups())
      
      ns <- session$ns
      player_cols <- config$player_columns
      
      # FIXED: Use .. prefix for data.table
      lineups_dt <- as.data.table(optimal_lineups())
      all_players <- unique(unlist(lineups_dt[, ..player_cols]))
      
      selectizeInput(ns("excluded_players"),
                     "Excluded Players (must NOT be in lineup)",
                     choices = sort(all_players),
                     multiple = TRUE,
                     options = list(placeholder = 'Select players to exclude'))
    })
    
    # ========================================================================
    # FILTERED POOL
    # ========================================================================
    
    filtered_pool <- reactive({
      req(optimal_lineups())
      
      lineups <- copy(optimal_lineups())
      setDT(lineups)
      
      setnames(lineups, make.names(names(lineups), unique = TRUE))
      
      if (!is.null(config$filter_options$numeric_filters)) {
        for (filter_name in config$filter_options$numeric_filters) {
          clean_col <- make.names(filter_name)
          range_val <- input[[paste0("range_", clean_col)]]
          
          if (!is.null(range_val) && clean_col %in% names(lineups)) {
            lineups <- lineups[get(clean_col) >= range_val[1] & get(clean_col) <= range_val[2]]
          }
        }
      }
      
      if (!is.null(input$locked_players) && length(input$locked_players) > 0) {
        player_cols <- config$player_columns
        for (locked in input$locked_players) {
          has_player <- apply(lineups[, ..player_cols], 1, function(row) {
            locked %in% row
          })
          lineups <- lineups[has_player, ]
        }
      }
      
      if (!is.null(input$excluded_players) && length(input$excluded_players) > 0) {
        player_cols <- config$player_columns
        for (excluded in input$excluded_players) {
          has_player <- apply(lineups[, ..player_cols], 1, function(row) {
            excluded %in% row
          })
          lineups <- lineups[!has_player, ]
        }
      }
      
      return(lineups)
    })
    
    output$filtered_pool_count <- renderText({
      req(filtered_pool())
      sprintf("Filtered Pool: %s lineups", format(nrow(filtered_pool()), big.mark = ","))
    })
    
    # ========================================================================
    # EXPOSURE TABLES
    # ========================================================================
    
    filtered_exposure <- reactive({
      req(filtered_pool())
      
      pool <- filtered_pool()
      player_cols <- config$player_columns
      
      all_players <- unlist(pool[, ..player_cols])
      player_counts <- table(all_players)
      
      exposure <- data.frame(
        Player = names(player_counts),
        FilteredRate = round(as.numeric(player_counts) / nrow(pool) * 100, 2),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(driver_details)) {
        details_data <- driver_details()
        if (!is.null(details_data) && nrow(details_data) > 0) {
          exposure <- merge(exposure, details_data, by = "Player", all.x = TRUE)
          
          if ("Own" %in% names(exposure)) {
            exposure$Own <- exposure$Own * 100
          }
        }
      }
      
      if ("Own" %in% names(exposure)) {
        exposure$Leverage <- round(exposure$FilteredRate - exposure$Own, 2)
      }
      
      if ("Salary" %in% names(exposure)) {
        exposure$SalaryK <- paste0(round(exposure$Salary / 1000, 1), "k")
      }
      
      col_order <- c("Player", "SalaryK", "Starting", "FilteredRate", "Own", "Leverage", "Team", "Car")
      col_order <- intersect(col_order, names(exposure))
      exposure <- exposure[, col_order]
      
      exposure <- exposure[order(-exposure$FilteredRate), ]
      
      return(exposure)
    })
    
    output$filtered_exposure_table <- renderDT({
      req(filtered_exposure())
      
      exp_data <- filtered_exposure()
      
      if ("SalaryK" %in% names(exp_data)) {
        names(exp_data)[names(exp_data) == "SalaryK"] <- "Salary"
      }
      
      dt <- datatable(
        exp_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "tip",
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
      
      if ("Own" %in% names(exp_data)) dt <- dt %>% formatRound('Own', digits = 1)
      if ("FilteredRate" %in% names(exp_data)) dt <- dt %>% formatRound('FilteredRate', digits = 2)
      if ("Leverage" %in% names(exp_data)) dt <- dt %>% formatRound('Leverage', digits = 2)
      if ("Starting" %in% names(exp_data)) dt <- dt %>% formatRound('Starting', digits = 0)
      
      dt
    })
    
    # ========================================================================
    # BUILD MANAGEMENT
    # ========================================================================
    
    observeEvent(input$add_build, {
      req(filtered_pool(), input$num_lineups)
      
      pool <- copy(filtered_pool())
      
      if (nrow(pool) == 0) {
        showNotification("Filtered pool is empty!", type = "error")
        return()
      }
      
      n_lineups <- min(input$num_lineups, nrow(pool))
      selected <- pool[sample(nrow(pool), n_lineups, replace = FALSE), ]
      
      rv$build_counter <- rv$build_counter + 1
      
      if (is.null(input$build_label) || input$build_label == "") {
        build_label <- as.character(rv$build_counter)
      } else {
        build_label <- input$build_label
      }
      
      selected$BuildLabel <- build_label
      selected$BuildTimestamp <- Sys.time()
      
      build_num <- length(rv$builds) + 1
      rv$builds[[build_num]] <- selected
      
      rv$portfolio <- rbindlist(rv$builds, fill = TRUE)
      
      showNotification(
        sprintf("Added %d lineups to portfolio as '%s'", n_lineups, build_label),
        type = "message"
      )
      
      updateTabsetPanel(session, "builder_tabs", selected = "Portfolio Summary")
    })
    
    observeEvent(input$clear_portfolio, {
      rv$builds <- list()
      rv$portfolio <- NULL
      rv$build_counter <- 0
      showNotification("Portfolio cleared", type = "message")
    })
    
    # ========================================================================
    # PORTFOLIO DISPLAY
    # ========================================================================
    
    output$total_portfolio_count <- renderText({
      if (is.null(rv$portfolio)) {
        "Portfolio: 0 lineups"
      } else {
        sprintf("Portfolio: %s lineups across %d builds", 
                format(nrow(rv$portfolio), big.mark = ","),
                length(rv$builds))
      }
    })
    
    output$builds_summary_table <- renderDT({
      req(rv$portfolio)
      
      summary <- rv$portfolio %>%
        group_by(BuildLabel) %>%
        summarise(
          NumLineups = n(),
          Timestamp = format(first(BuildTimestamp), "%Y-%m-%d %H:%M:%S"),
          .groups = 'drop'
        )
      
      datatable(
        summary,
        options = list(
          pageLength = -1,
          dom = 't',
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
    })
    
    output$portfolio_exposure_table <- renderDT({
      req(rv$portfolio)
      
      player_cols <- config$player_columns
      all_players <- unlist(rv$portfolio[, ..player_cols])
      player_counts <- table(all_players)
      
      exposure <- data.frame(
        Player = names(player_counts),
        PortfolioRate = round(as.numeric(player_counts) / nrow(rv$portfolio) * 100, 2),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(driver_details)) {
        details_data <- driver_details()
        if (!is.null(details_data) && nrow(details_data) > 0) {
          exposure <- merge(exposure, details_data, by = "Player", all.x = TRUE)
          
          if ("Own" %in% names(exposure)) {
            exposure$Own <- exposure$Own * 100
          }
        }
      }
      
      if ("Own" %in% names(exposure)) {
        exposure$Leverage <- round(exposure$PortfolioRate - exposure$Own, 2)
      }
      
      if ("Salary" %in% names(exposure)) {
        exposure$SalaryK <- paste0(round(exposure$Salary / 1000, 1), "k")
      }
      
      col_order <- c("Player", "SalaryK", "Starting", "PortfolioRate", "Own", "Leverage", "Team", "Car")
      col_order <- intersect(col_order, names(exposure))
      exposure <- exposure[, col_order]
      
      exposure <- exposure[order(-exposure$PortfolioRate), ]
      
      if ("SalaryK" %in% names(exposure)) {
        names(exposure)[names(exposure) == "SalaryK"] <- "Salary"
      }
      
      dt <- datatable(
        exposure,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "tip",
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
      
      if ("Own" %in% names(exposure)) dt <- dt %>% formatRound('Own', digits = 1)
      if ("PortfolioRate" %in% names(exposure)) dt <- dt %>% formatRound('PortfolioRate', digits = 2)
      if ("Leverage" %in% names(exposure)) dt <- dt %>% formatRound('Leverage', digits = 2)
      if ("Starting" %in% names(exposure)) dt <- dt %>% formatRound('Starting', digits = 0)
      
      dt
    })
    
    output$portfolio_lineups_table <- renderDT({
      req(rv$portfolio)
      
      display_cols <- intersect(config$display_columns, names(rv$portfolio))
      display_cols <- c(display_cols, "BuildLabel")
      
      portfolio_display <- rv$portfolio[, ..display_cols]
      
      dt <- datatable(
        portfolio_display,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "Bfrtip",
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
      
      if (!is.null(config$formatting)) {
        if ("percentage_cols" %in% names(config$formatting)) {
          pct_cols <- intersect(config$formatting$percentage_cols, names(portfolio_display))
          if (length(pct_cols) > 0) {
            dt <- dt %>% formatRound(columns = pct_cols, digits = config$formatting$percentage_decimals)
          }
        }
        
        if ("currency_cols" %in% names(config$formatting)) {
          curr_cols <- intersect(config$formatting$currency_cols, names(portfolio_display))
          if (length(curr_cols) > 0) {
            dt <- dt %>% formatCurrency(columns = curr_cols, currency = "$", digits = 0)
          }
        }
        
        if ("integer_cols" %in% names(config$formatting)) {
          int_cols <- intersect(config$formatting$integer_cols, names(portfolio_display))
          if (length(int_cols) > 0) {
            dt <- dt %>% formatRound(columns = int_cols, digits = 0)
          }
        }
      }
      
      dt
    })
    
    # ========================================================================
    # DOWNLOAD - USING SIMULATION RESULTS FOR DKNAME MAPPING
    # ========================================================================
    
    output$download_portfolio <- downloadHandler(
      filename = function() {
        sprintf("%s_portfolio_%s.csv", 
                config$sport_name,
                format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        req(rv$portfolio)
        req(simulation_results)
        
        df <- copy(rv$portfolio)
        setDT(df)
        
        sim_data <- simulation_results()
        setDT(sim_data)
        
        # FIXED: Map Name -> DKName (same as optimal lineups download)
        name_to_dkname <- setNames(sim_data$DKName, sim_data$Name)
        
        # Replace player names with DKName format (Name (ID))
        player_cols <- config$player_columns
        for (col in player_cols) {
          if (col %in% names(df)) {
            df[[col]] <- name_to_dkname[df[[col]]]
          }
        }
        
        # Randomize order
        df <- df[sample(nrow(df)), ]
        
        # Remove timestamp
        if ("BuildTimestamp" %in% names(df)) {
          df[, BuildTimestamp := NULL]
        }
        
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}