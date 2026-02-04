# Load required packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(readxl)
library(DT)
library(shinycssloaders)
library(shinyjs)

# Set up custom CSS for app theme with black and gold color scheme
custom_css <- "
  /* Override dashboard header colors */
  .skin-blue .main-header {
    background-color: #000000;
  }
  .skin-blue .main-header .logo {
    background-color: #000000;
    color: #FFD700;
  }
  .skin-blue .main-header .logo:hover {
    background-color: #000000;
  }
  .skin-blue .main-header .navbar {
    background-color: #000000;
  }

  /* Override dashboard sidebar colors */
  .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
    background-color: #222222;
  }
  .skin-blue .sidebar a {
    color: #FFD700;
  }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li:hover > a {
    color: #ffffff;
    background: #333333;
    border-left-color: #FFD700;
  }

  /* FORCE BLACK BOX HEADERS - More specific selectors */
  .skin-blue .box.box-primary > .box-header,
  .box.box-primary > .box-header,
  .box-header {
    background-color: #000000 !important;
    color: #FFD700 !important;
  }

  /* Style buttons */
  .btn-primary {
    background-color: #FFD700;
    border-color: #DAA520;
    color: #000000;
  }
  .btn-primary:hover, .btn-primary:focus {
    background-color: #DAA520;
    border-color: #B8860B;
    color: #000000;
  }

  /* Style tabs */
  .nav-tabs-custom > .nav-tabs > li.active {
    border-top-color: #FFD700;
  }

  /* Additional styles for gold accents */
  .pagination > .active > a,
  .pagination > .active > span,
  .pagination > .active > a:hover,
  .pagination > .active > span:hover,
  .pagination > .active > a:focus,
  .pagination > .active > span:focus {
    background-color: #FFD700;
    border-color: #DAA520;
    color: #000000;
  }

  /* Make AV and Bid to Beat text black for better readability */
  #current_av {
    color: #000000 !important;
    font-weight: bold;
  }
  
  .bid-to-beat-header {
    color: #000000 !important;
    font-weight: bold;
  }
  "

# Helper function to calculate AV (Annual Value)
calculate_av <- function(years, dollars) {
  if (years < 1 || years > 5) return(0)
  
  multipliers <- c(1.0, 0.8, 0.6, 0.4, 0.2)
  total_av <- 0
  
  for (i in 1:years) {
    total_av <- total_av + (dollars * multipliers[i])
  }
  
  return(total_av)
}

# Helper function to generate bid-to-beat table
generate_bid_to_beat <- function(current_years, current_dollars) {
  current_av <- calculate_av(current_years, current_dollars)
  
  bid_table <- data.frame(
    Years = 1:5,
    Min_Dollars_Needed = numeric(5)
  )
  
  for (year in 1:5) {
    # Binary search to find minimum dollars needed
    low <- 1
    high <- 1000
    min_dollars <- high
    
    while (low <= high) {
      mid <- floor((low + high) / 2)
      test_av <- calculate_av(year, mid)
      
      if (test_av > current_av) {
        min_dollars <- mid
        high <- mid - 1
      } else {
        low <- mid + 1
      }
    }
    
    bid_table$Min_Dollars_Needed[year] <- min_dollars
    bid_table$AV[year] <- calculate_av(year, min_dollars)
  }
  
  return(bid_table)
}

# Helper function to get next nominating team
get_next_nominating_team <- function(teams_data, current_team_index) {
  # Filter teams that still have resources
  active_teams <- teams_data %>%
    filter(Spots > 0 & Salary > 0 & Years > 0)
  
  if (nrow(active_teams) == 0) {
    return(list(team = NULL, index = NULL))
  }
  
  # Get team names in order
  team_order <- teams_data$Team[teams_data$Team %in% active_teams$Team]
  
  if (is.null(current_team_index) || current_team_index >= length(team_order)) {
    # Start over or initialize
    next_index <- 1
  } else {
    # Move to next team
    next_index <- current_team_index + 1
    if (next_index > length(team_order)) {
      next_index <- 1
    }
  }
  
  return(list(
    team = team_order[next_index],
    index = next_index
  ))
}

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(title = "F1C Draft"),
  
  # Dashboard sidebar
  dashboardSidebar(
    useShinyjs(),
    div(
      style = "text-align: center; padding: 10px; margin-bottom: 5px;",
      tags$img(
        src = "logo.jpg",
        height = "200px",
        width = "auto",
        style = "border: 2px solid #FFD700; border-radius: 10px;"
      )
    ),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem(
        "Available Players",
        tabName = "available_players",
        icon = icon("users")
      ),
      menuItem(
        "Auction Interface", 
        tabName = "auction",
        icon = icon("gavel")
      ),
      menuItem(
        "Drafted Players",
        tabName = "drafted_players",
        icon = icon("trophy")
      )
    )
  ),
  
  # Dashboard body
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      # Available Players Tab
      tabItem(
        tabName = "available_players",
        fluidRow(
          box(
            width = 12,
            title = "Available Players for Draft",
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                6,
                div(
                  id = "nomination_status",
                  style = "background: #333333; color: #FFD700; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                  h3("Next to Nominate: ", 
                     span(id = "next_team", "Upload draft file to begin"),
                     style = "margin: 0; text-align: center;")
                )
              ),
              column(
                3,
                selectInput(
                  "position_filter",
                  "Filter by Position:",
                  choices = c("All Positions" = "", "RB", "WR", "QB", "TE", "K", "DST"),
                  selected = ""
                )
              ),
              column(
                3,
                textInput(
                  "player_search",
                  "Search Players:",
                  placeholder = "Enter player name..."
                )
              )
            ),
            DTOutput("available_players_table") %>% withSpinner(color = "#FFD700")
          )
        )
      ),
      
      # Auction Interface Tab
      tabItem(
        tabName = "auction",
        fluidRow(
          # Left Panel - Bidding Interface (made more vertical)
          box(
            width = 4,
            title = "Auction Bidding",
            status = "primary",
            solidHeader = TRUE,
            height = "700px",
            # Current Player Selection
            fluidRow(
              column(
                12,
                selectInput(
                  "current_player",
                  "Player Up for Auction:",
                  choices = NULL
                )
              )
            ),
            
            # Current Bid Inputs
            fluidRow(
              column(
                6,
                numericInput(
                  "current_years",
                  "Years:",
                  value = 1,
                  min = 1,
                  max = 5,
                  step = 1
                )
              ),
              column(
                6,
                numericInput(
                  "current_dollars",
                  "Dollars:",
                  value = 1,
                  min = 1,
                  max = 500,
                  step = 1
                )
              )
            ),
            
            # Current AV Display
            fluidRow(
              column(
                12,
                div(
                  style = "background: #f4f4f4; padding: 10px; border-radius: 5px; margin: 10px 0;",
                  h4("Current Annual Value (AV): ",
                     span(id = "current_av", "0.0"),
                     style = "margin: 0; text-align: center;")
                )
              )
            ),
            
            # Bid to Beat Table
            fluidRow(
              column(
                12,
                h4("Bid to Beat", class = "bid-to-beat-header"),
                DTOutput("bid_to_beat_table")
              )
            ),
            
            # Team Assignment and Action Buttons
            fluidRow(
              column(
                12,
                selectInput(
                  "assign_to_team",
                  "Assign to Team:",
                  choices = NULL
                ),
                actionButton(
                  "assign_player",
                  "Assign Player",
                  class = "btn-primary",
                  style = "width: 100%; margin-top: 10px;"
                ),
                actionButton(
                  "undo_assignment",
                  "Undo Last Assignment",
                  class = "btn-warning",
                  style = "width: 100%; margin-top: 10px;"
                )
              )
            )
          ),
          
          # Right Panel - Team Status (made bigger)
          box(
            width = 8,
            title = "Team Status",
            status = "primary",
            solidHeader = TRUE,
            height = "700px",
            DTOutput("team_status_table") %>% withSpinner(color = "#FFD700")
          )
        )
      ),
      
      # Drafted Players Tab
      tabItem(
        tabName = "drafted_players",
        fluidRow(
          box(
            width = 12,
            title = "Drafted Players",
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                6,
                h4("Draft Results", style = "margin-top: 10px;")
              ),
              column(
                6,
                div(
                  style = "text-align: right; margin-top: 10px;",
                  downloadButton(
                    "download_drafted",
                    "Download Drafted Players",
                    class = "btn-primary"
                  )
                )
              )
            ),
            DTOutput("drafted_players_table") %>% withSpinner(color = "#FFD700")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store data
  rv <- reactiveValues(
    teams_data = NULL,
    players_data = NULL,
    drafted_players = data.frame(),
    assignment_history = list(),
    current_nominating_team = NULL,
    current_team_index = NULL,
    file_loaded = FALSE
  )
  

  observe({
    # Only run once when app starts
    if (!rv$file_loaded) {
      tryCatch({
        # Load from file in app directory
        # Adjust filename as needed
        file_path <- "draft_data.xlsx"
        
        # Check if file exists
        if (!file.exists(file_path)) {
          showModal(modalDialog(
            title = "File Not Found",
            paste("Draft file 'draft_data.xlsx' not found in app directory.",
                  "Please ensure the file is in the same folder as your app."),
            easyClose = FALSE
          ))
          return()
        }
        
        # Read Teams tab
        teams_tab <- read_excel(file_path, sheet = "Teams")
        colnames(teams_tab) <- c("Team", "Spots", "Salary", "Years")
        
        # Read Players tab  
        players_tab <- read_excel(file_path, sheet = "Players")
        colnames(players_tab) <- c("Player", "Pos")
        
        # Store data
        rv$teams_data <- teams_tab
        rv$players_data <- players_tab
        rv$drafted_players <- data.frame()
        rv$assignment_history <- list()
        rv$file_loaded <- TRUE
        
        # Initialize nomination
        next_nom <- get_next_nominating_team(rv$teams_data, NULL)
        rv$current_nominating_team <- next_nom$team
        rv$current_team_index <- next_nom$index
        
        # Update player choices
        updateSelectInput(
          session,
          "current_player",
          choices = setNames(rv$players_data$Player, 
                             paste(rv$players_data$Player, "-", rv$players_data$Pos))
        )
        
        # Update position filter choices
        positions <- unique(rv$players_data$Pos)
        updateSelectInput(
          session,
          "position_filter", 
          choices = c("All Positions" = "", positions)
        )
        
        cat("Draft data loaded successfully!\n")
        cat("Teams:", nrow(teams_tab), "\n")
        cat("Players:", nrow(players_tab), "\n")
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error Loading Draft File",
          paste("Error reading draft_data.xlsx:", e$message,
                "\n\nPlease check that the file exists and has 'Teams' and 'Players' tabs with the correct format."),
          easyClose = FALSE
        ))
      })
    }
  })
  # Update next nominating team display
  observe({
    if (!is.null(rv$current_nominating_team)) {
      runjs(sprintf(
        "document.getElementById('next_team').textContent = '%s';",
        rv$current_nominating_team
      ))
    }
  })
  
  # Update team assignment choices (only active teams)
  observe({
    if (!is.null(rv$teams_data)) {
      active_teams <- rv$teams_data %>%
        filter(Spots > 0 & Salary > 0 & Years > 0)
      
      if (nrow(active_teams) > 0) {
        updateSelectInput(
          session,
          "assign_to_team",
          choices = setNames(active_teams$Team, active_teams$Team)
        )
      } else {
        updateSelectInput(
          session,
          "assign_to_team", 
          choices = c("No teams available" = "")
        )
      }
    }
  })
  
  # Calculate current AV reactively
  current_av <- reactive({
    req(input$current_years, input$current_dollars)
    calculate_av(input$current_years, input$current_dollars)
  })
  
  # Update current AV display
  observe({
    av_value <- current_av()
    runjs(sprintf(
      "document.getElementById('current_av').textContent = '%.1f';",
      av_value
    ))
  })
  
  # Generate bid to beat table
  output$bid_to_beat_table <- renderDT({
    req(input$current_years, input$current_dollars)
    # Also depend on current player to force refresh
    req(input$current_player)
    
    bid_table <- generate_bid_to_beat(input$current_years, input$current_dollars)
    
    datatable(
      bid_table,
      options = list(
        pageLength = 5,
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        ordering = FALSE,
        dom = "t"
      ),
      rownames = FALSE,
      colnames = c("Years", "Min $ Needed", "AV")
    ) %>%
      formatRound(c("Min_Dollars_Needed", "AV"), 1)
  })
  
  # Available players table with filtering
  available_players_filtered <- reactive({
    req(rv$players_data)
    
    # Start with all undrafted players
    available <- rv$players_data
    
    # Remove drafted players
    if (nrow(rv$drafted_players) > 0) {
      available <- available %>%
        filter(!Player %in% rv$drafted_players$Player)
    }
    
    # Apply position filter
    if (!is.null(input$position_filter) && input$position_filter != "") {
      available <- available %>%
        filter(Pos == input$position_filter)
    }
    
    # Apply search filter
    if (!is.null(input$player_search) && input$player_search != "") {
      available <- available %>%
        filter(grepl(input$player_search, Player, ignore.case = TRUE))
    }
    
    return(available)
  })
  
  # Render available players table
  output$available_players_table <- renderDT({
    req(available_players_filtered())
    
    datatable(
      available_players_filtered(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tip",
        ordering = TRUE
      ),
      rownames = FALSE,
      selection = 'single'
    )
  })
  
  # Handle player selection from available players table
  observeEvent(input$available_players_table_rows_selected, {
    req(input$available_players_table_rows_selected)
    
    selected_row <- input$available_players_table_rows_selected
    filtered_data <- available_players_filtered()
    selected_player <- filtered_data$Player[selected_row]
    
    # Update current player in auction interface
    updateSelectInput(
      session,
      "current_player",
      selected = selected_player
    )
    
    # Switch to auction tab
    updateTabItems(session, "sidebar_menu", "auction")
  })
  
  # Team status table - ALWAYS sorted by salary descending
  output$team_status_table <- renderDT({
    req(rv$teams_data)
    
    # Create display data with status indicators and sort by salary descending
    team_display <- rv$teams_data %>%
      mutate(
        Status = case_when(
          Spots <= 0 | Salary <= 0 | Years <= 0 ~ "Finished",
          TRUE ~ "Active"
        )
      ) %>%
      arrange(desc(Salary))
    
    dt <- datatable(
      team_display,
      options = list(
        pageLength = 15,
        scrollX = FALSE,
        dom = "tip",
        ordering = FALSE,  # Disable user sorting to maintain salary sort
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 2, 3)),
          list(className = 'dt-left', targets = c(0, 4)),
          list(width = '20%', targets = 0),
          list(width = '15%', targets = 1),
          list(width = '15%', targets = 2), 
          list(width = '15%', targets = 3),
          list(width = '15%', targets = 4)
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Status",
        backgroundColor = styleEqual("Finished", "#ffcccc")
      ) %>%
      formatStyle(
        columns = c("Spots", "Salary", "Years"),
        backgroundColor = styleInterval(c(0.5), c("#ffcccc", "white"))
      )
    
    return(dt)
  })
  
  # Drafted players table
  output$drafted_players_table <- renderDT({
    if (nrow(rv$drafted_players) == 0) {
      return(datatable(
        data.frame(Message = "No players drafted yet"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    datatable(
      rv$drafted_players,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "tip",
        ordering = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Player assignment handler
  observeEvent(input$assign_player, {
    req(input$current_player, input$assign_to_team, input$current_years, input$current_dollars)
    req(rv$teams_data, rv$players_data)
    
    # Validate assignment
    selected_team <- input$assign_to_team
    team_row <- which(rv$teams_data$Team == selected_team)
    
    if (length(team_row) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Selected team not found.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Check if team has sufficient resources
    team_spots <- rv$teams_data$Spots[team_row]
    team_salary <- rv$teams_data$Salary[team_row]
    team_years <- rv$teams_data$Years[team_row]
    
    bid_years <- input$current_years
    bid_dollars <- input$current_dollars
    
    if (team_spots < 1) {
      showModal(modalDialog(
        title = "Assignment Error",
        paste("Team", selected_team, "has no roster spots available."),
        easyClose = TRUE
      ))
      return()
    }
    
    if (team_salary < bid_dollars) {
      showModal(modalDialog(
        title = "Assignment Error", 
        paste("Team", selected_team, "does not have enough salary cap.",
              "Available:", team_salary, "Needed:", bid_dollars),
        easyClose = TRUE
      ))
      return()
    }
    
    if (team_years < bid_years) {
      showModal(modalDialog(
        title = "Assignment Error",
        paste("Team", selected_team, "does not have enough contract years.",
              "Available:", team_years, "Needed:", bid_years),
        easyClose = TRUE
      ))
      return()
    }
    
    # Calculate AV for the assignment
    assignment_av <- calculate_av(bid_years, bid_dollars)
    
    # Get player position
    player_pos <- rv$players_data$Pos[rv$players_data$Player == input$current_player]
    
    # Create assignment record
    new_assignment <- data.frame(
      Player = input$current_player,
      Pos = player_pos,
      Team = selected_team,
      Years = bid_years,
      Dollars = bid_dollars,
      AV = assignment_av,
      stringsAsFactors = FALSE
    )
    
    # Store assignment for undo functionality
    assignment_record <- list(
      assignment = new_assignment,
      team_state_before = rv$teams_data[team_row, ],
      team_row = team_row
    )
    rv$assignment_history <- append(rv$assignment_history, list(assignment_record), 0)
    
    # Update team resources
    rv$teams_data$Spots[team_row] <- team_spots - 1
    rv$teams_data$Salary[team_row] <- team_salary - bid_dollars
    rv$teams_data$Years[team_row] <- team_years - bid_years
    
    # Add to drafted players
    rv$drafted_players <- rbind(rv$drafted_players, new_assignment)
    
    # Advance to next nominating team
    next_nom <- get_next_nominating_team(rv$teams_data, rv$current_team_index)
    rv$current_nominating_team <- next_nom$team
    rv$current_team_index <- next_nom$index
    
    # Update current player dropdown to reflect available players
    remaining_players <- rv$players_data %>%
      filter(!Player %in% rv$drafted_players$Player)
    
    if (nrow(remaining_players) > 0) {
      updateSelectInput(
        session,
        "current_player",
        choices = setNames(remaining_players$Player,
                           paste(remaining_players$Player, "-", remaining_players$Pos))
      )
    } else {
      updateSelectInput(
        session,
        "current_player",
        choices = c("No players available" = "")
      )
    }
    
    # Reset bid inputs
    updateNumericInput(session, "current_years", value = 1)
    updateNumericInput(session, "current_dollars", value = 1)
    
    # Show success message and return to available players
    showModal(modalDialog(
      title = "Player Assigned",
      sprintf("%s assigned to %s for %d years, $%d (AV: %.1f)",
              input$current_player, selected_team, bid_years, bid_dollars, assignment_av),
      easyClose = TRUE
    ))
    
    # Switch back to available players tab
    updateTabItems(session, "sidebar_menu", "available_players")
  })
  
  # Undo last assignment
  observeEvent(input$undo_assignment, {
    if (length(rv$assignment_history) == 0) {
      showModal(modalDialog(
        title = "No Assignment to Undo",
        "There are no recent assignments to undo.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Get last assignment
    last_assignment <- rv$assignment_history[[1]]
    rv$assignment_history <- rv$assignment_history[-1]
    
    # Restore team state
    team_row <- last_assignment$team_row
    rv$teams_data[team_row, ] <- last_assignment$team_state_before
    
    # Remove from drafted players
    player_to_remove <- last_assignment$assignment$Player
    rv$drafted_players <- rv$drafted_players %>%
      filter(Player != player_to_remove)
    
    # Update current player dropdown
    remaining_players <- rv$players_data %>%
      filter(!Player %in% rv$drafted_players$Player)
    
    updateSelectInput(
      session,
      "current_player",
      choices = setNames(remaining_players$Player,
                         paste(remaining_players$Player, "-", remaining_players$Pos))
    )
    
    # Move nomination back one team
    if (!is.null(rv$current_team_index) && rv$current_team_index > 1) {
      rv$current_team_index <- rv$current_team_index - 1
    } else {
      # Find total active teams to wrap around
      active_teams <- rv$teams_data %>%
        filter(Spots > 0 & Salary > 0 & Years > 0)
      rv$current_team_index <- nrow(active_teams)
    }
    
    next_nom <- get_next_nominating_team(rv$teams_data, rv$current_team_index - 1)
    rv$current_nominating_team <- next_nom$team
    rv$current_team_index <- next_nom$index
    
    showModal(modalDialog(
      title = "Assignment Undone",
      sprintf("Undid assignment of %s to %s", 
              player_to_remove, last_assignment$assignment$Team),
      easyClose = TRUE
    ))
  })
  
  # Download drafted players handler
  output$download_drafted <- downloadHandler(
    filename = function() {
      paste("drafted_players_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      if (nrow(rv$drafted_players) > 0) {
        write.csv(rv$drafted_players, file, row.names = FALSE)
      } else {
        # Create empty file with headers
        empty_df <- data.frame(
          Player = character(0),
          Pos = character(0),
          Team = character(0),
          Years = numeric(0),
          Dollars = numeric(0),
          AV = numeric(0)
        )
        write.csv(empty_df, file, row.names = FALSE)
      }
    }
  )
  
  # Clean up on session end
  onStop(function() {
    # Any cleanup if needed
  })
}

# Run the application
shinyApp(ui = ui, server = server)