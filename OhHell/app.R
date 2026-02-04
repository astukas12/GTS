# Load required packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(plotly)

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

  /* FORCE BLACK BOX HEADERS */
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
  
  /* Warning box styling */
  .warning-box {
    background: #FFD700;
    color: #000000;
    padding: 15px;
    border-radius: 5px;
    margin: 10px 0;
    font-weight: bold;
    text-align: center;
  }
  
  /* Round info styling */
  .round-info {
    background: #333333;
    color: #FFD700;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 15px;
  }
  
  /* Dealer info styling */
  .dealer-info {
    background: #FFD700;
    color: #000000;
    padding: 10px;
    border-radius: 5px;
    margin: 10px 0;
    font-weight: bold;
    text-align: center;
  }
  
  /* Big bet display */
  .bet-display {
    background: #000000;
    border: 3px solid #FFD700;
    border-radius: 10px;
    padding: 20px;
    margin: 15px 0;
  }
  
  .bet-player-name {
    color: #FFD700;
    font-size: 20px;
    font-weight: bold;
    margin-bottom: 5px;
  }
  
  .bet-value {
    color: #FFD700;
    font-size: 36px;
    font-weight: bold;
  }
  
  .bet-total-display {
    background: #FFD700;
    color: #000000;
    padding: 15px;
    border-radius: 5px;
    margin: 15px 0;
    font-weight: bold;
    text-align: center;
    font-size: 24px;
  }
  
  /* Compact books input */
  .books-input-row {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
    align-items: center;
  }
  
  .books-input-item {
    flex: 1;
    min-width: 120px;
  }
  
  /* Medal colors for scoreboard */
  .gold-medal {
    background-color: #FFD700 !important;
    color: #000000 !important;
    font-weight: bold;
  }
  
  .silver-medal {
    background-color: #C0C0C0 !important;
    color: #000000 !important;
    font-weight: bold;
  }
  
  .bronze-medal {
    background-color: #CD7F32 !important;
    color: #000000 !important;
    font-weight: bold;
  }
"

# Helper function to calculate score
calculate_score <- function(bet, books) {
  if (bet == books) {
    return(bet + 1)
  } else {
    return(-abs(bet - books))
  }
}

# Helper function to generate round sequence
generate_rounds <- function(num_players) {
  max_round <- floor(52 / num_players)
  down_rounds <- seq(max_round, 1, -1)
  up_rounds <- seq(1, max_round, 1)
  return(c(down_rounds, up_rounds))
}

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(title = "Oh Hell Scorer"),
  
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
        "Setup",
        tabName = "setup",
        icon = icon("cog")
      ),
      menuItem(
        "Game Play",
        tabName = "gameplay",
        icon = icon("gamepad")
      ),
      menuItem(
        "Scoreboard",
        tabName = "scoreboard",
        icon = icon("trophy")
      ),
      menuItem(
        "Statistics",
        tabName = "statistics",
        icon = icon("chart-line")
      )
    )
  ),
  
  # Dashboard body
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      # Setup Tab
      tabItem(
        tabName = "setup",
        fluidRow(
          box(
            width = 6,
            title = "Game Setup",
            status = "primary",
            solidHeader = TRUE,
            numericInput(
              "num_players",
              "Number of Players:",
              value = 4,
              min = 2,
              max = 8,
              step = 1
            ),
            uiOutput("player_names_ui"),
            actionButton(
              "start_game",
              "Start New Game",
              class = "btn-primary",
              icon = icon("play"),
              width = "100%"
            )
          ),
          box(
            width = 6,
            title = "Game Info",
            status = "primary",
            solidHeader = TRUE,
            htmlOutput("game_info")
          )
        )
      ),
      
      # Gameplay Tab
      tabItem(
        tabName = "gameplay",
        fluidRow(
          box(
            width = 12,
            title = "Current Round",
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                12,
                div(
                  class = "round-info",
                  h3(textOutput("round_display"), style = "margin: 0; text-align: center;")
                )
              )
            ),
            fluidRow(
              column(
                12,
                uiOutput("warning_display")
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Place Bets",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("betting_ui"),
            actionButton(
              "submit_bets",
              "Submit All Bets",
              class = "btn-primary",
              icon = icon("check"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Record Books Won",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("books_ui"),
            actionButton(
              "submit_books",
              "Submit Books & Calculate Scores",
              class = "btn-primary",
              icon = icon("calculator"),
              width = "100%"
            )
          )
        )
      ),
      
      # Scoreboard Tab
      tabItem(
        tabName = "scoreboard",
        fluidRow(
          box(
            width = 12,
            title = "Current Standings",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("scoreboard_table") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Round History",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("history_table") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Export Data",
            status = "primary",
            solidHeader = TRUE,
            downloadButton(
              "download_game",
              "Download Game Results",
              class = "btn-primary"
            )
          )
        )
      ),
      
      # Statistics Tab
      tabItem(
        tabName = "statistics",
        fluidRow(
          box(
            width = 12,
            title = "Score Progression",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("score_progression_plot", height = "400px") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Player Statistics",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("player_stats_table") %>% withSpinner(color = "#FFD700")
          ),
          box(
            width = 6,
            title = "Betting Patterns",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("bet_distribution_plot", height = "400px") %>% withSpinner(color = "#FFD700")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    game_started = FALSE,
    players = NULL,
    rounds = NULL,
    current_round_idx = 1,
    round_history = data.frame(),
    current_bets = list(),
    current_books = list(),
    total_scores = list(),
    bets_submitted = FALSE,
    books_submitted = FALSE,
    dealer_idx = NULL,
    betting_order = NULL
  )
  
  # Generate player name inputs
  output$player_names_ui <- renderUI({
    req(input$num_players)
    
    lapply(1:input$num_players, function(i) {
      textInput(
        paste0("player_", i),
        paste("Player", i, "Name:"),
        value = paste("Player", i)
      )
    })
  })
  
  # Display game info
  output$game_info <- renderText({
    req(input$num_players)
    
    max_cards <- floor(52 / input$num_players)
    total_rounds <- max_cards * 2
    
    HTML(paste0(
      "<div style='padding: 10px;'>",
      "<p><strong>Cards per player (starting):</strong> ", max_cards, "</p>",
      "<p><strong>Total rounds:</strong> ", total_rounds, "</p>",
      "<p><strong>Round sequence:</strong> ", max_cards, " down to 1, then 1 up to ", max_cards, "</p>",
      "<hr>",
      "<p><em>The last player to bet cannot choose a number that makes the total bets equal to the total books in the round.</em></p>",
      "</div>"
    ))
  })
  
  # Start game
  observeEvent(input$start_game, {
    req(input$num_players)
    
    # Get player names
    player_names <- sapply(1:input$num_players, function(i) {
      input[[paste0("player_", i)]]
    })
    
    # Generate rounds
    rounds <- generate_rounds(input$num_players)
    
    # Randomly assign first dealer
    first_dealer <- sample(1:length(player_names), 1)
    
    # Calculate initial betting order (left of dealer bets first)
    betting_order <- c()
    for (i in 1:length(player_names)) {
      idx <- ((first_dealer - 1 + i) %% length(player_names)) + 1
      betting_order <- c(betting_order, idx)
    }
    
    # Initialize game
    rv$players <- player_names
    rv$rounds <- rounds
    rv$current_round_idx <- 1
    rv$round_history <- data.frame()
    rv$current_bets <- list()
    rv$current_books <- list()
    rv$bets_submitted <- FALSE
    rv$books_submitted <- FALSE
    rv$dealer_idx <- first_dealer
    rv$betting_order <- betting_order
    
    # Initialize total scores
    rv$total_scores <- setNames(rep(0, length(player_names)), player_names)
    
    rv$game_started <- TRUE
    
    # Switch to gameplay tab
    updateTabItems(session, "sidebar_menu", "gameplay")
    
    showModal(modalDialog(
      title = "Game Started!",
      paste0("Game started with ", input$num_players, " players! ",
             player_names[first_dealer], " is the first dealer."),
      easyClose = TRUE
    ))
  })
  
  # Display current round
  output$round_display <- renderText({
    req(rv$game_started)
    
    if (rv$current_round_idx > length(rv$rounds)) {
      return("Game Complete!")
    }
    
    current_books <- rv$rounds[rv$current_round_idx]
    round_direction <- ifelse(rv$current_round_idx <= length(rv$rounds)/2, "Down", "Up")
    dealer_name <- rv$players[rv$dealer_idx]
    
    paste0("Round ", rv$current_round_idx, " of ", length(rv$rounds), 
           " (", round_direction, ") - ", current_books, " Books | Dealer: ", dealer_name)
  })
  
  # Warning display for last bettor
  output$warning_display <- renderUI({
    req(rv$game_started)
    req(!rv$bets_submitted)
    
    if (rv$current_round_idx > length(rv$rounds)) {
      return(NULL)
    }
    
    current_books <- rv$rounds[rv$current_round_idx]
    
    # Calculate current bet total (all bets except the dealer who bets last)
    bet_total <- 0
    for (i in 1:(length(rv$players) - 1)) {
      player_idx <- rv$betting_order[i]
      bet_input <- input[[paste0("bet_", player_idx)]]
      if (!is.null(bet_input)) {
        bet_total <- bet_total + bet_input
      }
    }
    
    forbidden_bet <- current_books - bet_total
    dealer_name <- rv$players[rv$dealer_idx]
    
    if (forbidden_bet >= 0 && forbidden_bet <= current_books) {
      div(
        class = "warning-box",
        paste0("⚠️ WARNING: ", dealer_name, " (Dealer) CANNOT bet ", forbidden_bet, 
               " (would make total equal ", current_books, ")")
      )
    }
  })
  
  # Betting UI
  output$betting_ui <- renderUI({
    req(rv$game_started)
    
    if (rv$current_round_idx > length(rv$rounds)) {
      return(p("Game is complete!"))
    }
    
    if (rv$bets_submitted) {
      # Show big bet display with 2 players side by side
      current_books <- rv$rounds[rv$current_round_idx]
      total_bets <- sum(unlist(rv$current_bets))
      diff <- total_bets - current_books
      diff_text <- if (diff > 0) {
        paste0("OVER by ", diff)
      } else if (diff < 0) {
        paste0("UNDER by ", abs(diff))
      } else {
        "EXACTLY ON!"
      }
      
      # Create rows of 2 players each
      num_players <- length(rv$players)
      rows <- list()
      
      for (i in seq(1, num_players, by = 2)) {
        # Create a row with up to 2 players
        players_in_row <- list()
        
        # First player in row
        player1 <- rv$players[i]
        bet1 <- rv$current_bets[[player1]]
        players_in_row[[1]] <- column(
          6,
          div(
            class = "bet-display",
            div(class = "bet-player-name", player1),
            div(class = "bet-value", paste("BET:", bet1))
          )
        )
        
        # Second player in row (if exists)
        if (i + 1 <= num_players) {
          player2 <- rv$players[i + 1]
          bet2 <- rv$current_bets[[player2]]
          players_in_row[[2]] <- column(
            6,
            div(
              class = "bet-display",
              div(class = "bet-player-name", player2),
              div(class = "bet-value", paste("BET:", bet2))
            )
          )
        }
        
        rows[[length(rows) + 1]] <- fluidRow(players_in_row)
      }
      
      return(div(
        div(
          class = "bet-total-display",
          paste0("Total Bets: ", total_bets, " | Round: ", current_books, " | ", diff_text)
        ),
        rows
      ))
    }
    
    current_books <- rv$rounds[rv$current_round_idx]
    
    # Display in betting order
    lapply(1:length(rv$players), function(i) {
      player_idx <- rv$betting_order[i]
      player_name <- rv$players[player_idx]
      
      # Add dealer notation
      label <- if (player_idx == rv$dealer_idx) {
        paste0(player_name, " (DEALER - bets last)")
      } else {
        paste0(player_name, " (bets ", i, ")")
      }
      
      numericInput(
        paste0("bet_", player_idx),
        paste(label, ":"),
        value = 0,
        min = 0,
        max = current_books,
        step = 1
      )
    })
  })
  
  # Books UI
  output$books_ui <- renderUI({
    req(rv$game_started)
    req(rv$bets_submitted)
    
    if (rv$current_round_idx > length(rv$rounds)) {
      return(p("Game is complete!"))
    }
    
    if (rv$books_submitted) {
      return(div(
        style = "padding: 20px; text-align: center; background: #333333; color: #FFD700; border-radius: 5px;",
        h4("Round Complete! View Scoreboard", style = "margin: 0;")
      ))
    }
    
    current_books <- rv$rounds[rv$current_round_idx]
    
    # Create compact horizontal layout
    book_inputs <- lapply(1:length(rv$players), function(i) {
      bet_value <- rv$current_bets[[rv$players[i]]]
      div(
        class = "books-input-item",
        numericInput(
          paste0("books_", i),
          rv$players[i],
          value = 0,
          min = 0,
          max = current_books,
          step = 1
        ),
        div(style = "text-align: center; color: #FFD700; font-size: 12px; margin-top: -10px;",
            paste0("Bet: ", bet_value))
      )
    })
    
    div(
      h4("Enter Books Won:", style = "color: #FFD700; margin-bottom: 10px;"),
      div(class = "books-input-row", book_inputs)
    )
  })
  
  # Submit bets
  observeEvent(input$submit_bets, {
    req(rv$game_started)
    req(!rv$bets_submitted)
    
    if (rv$current_round_idx > length(rv$rounds)) {
      return()
    }
    
    current_books <- rv$rounds[rv$current_round_idx]
    
    # Collect all bets in player order
    bets <- sapply(1:length(rv$players), function(i) {
      input[[paste0("bet_", i)]]
    })
    
    # Check if dealer bet the forbidden number
    # Dealer is last in betting order, but could be any player index
    bet_total_without_dealer <- 0
    for (i in 1:(length(rv$players) - 1)) {
      player_idx <- rv$betting_order[i]
      bet_total_without_dealer <- bet_total_without_dealer + bets[player_idx]
    }
    
    forbidden_bet <- current_books - bet_total_without_dealer
    dealer_bet <- bets[rv$dealer_idx]
    dealer_name <- rv$players[rv$dealer_idx]
    
    if (forbidden_bet >= 0 && forbidden_bet <= current_books && dealer_bet == forbidden_bet) {
      showModal(modalDialog(
        title = "Invalid Bet!",
        paste0(dealer_name, " (Dealer) cannot bet ", forbidden_bet, 
               " because it would make the total equal to ", current_books, "."),
        easyClose = TRUE
      ))
      return()
    }
    
    # Store bets
    rv$current_bets <- setNames(as.list(bets), rv$players)
    rv$bets_submitted <- TRUE
  })
  
  # Submit books and calculate scores
  observeEvent(input$submit_books, {
    req(rv$game_started)
    req(rv$bets_submitted)
    req(!rv$books_submitted)
    
    if (rv$current_round_idx > length(rv$rounds)) {
      return()
    }
    
    current_books <- rv$rounds[rv$current_round_idx]
    
    # Collect all books
    books <- sapply(1:length(rv$players), function(i) {
      input[[paste0("books_", i)]]
    })
    
    # Validate total books
    if (sum(books) != current_books) {
      showModal(modalDialog(
        title = "Invalid Books!",
        paste0("Total books won must equal ", current_books, ". Currently: ", sum(books)),
        easyClose = TRUE
      ))
      return()
    }
    
    # Store books
    rv$current_books <- setNames(as.list(books), rv$players)
    
    # Calculate scores for this round
    round_scores <- list()
    for (player in rv$players) {
      bet <- rv$current_bets[[player]]
      book <- rv$current_books[[player]]
      score <- calculate_score(bet, book)
      round_scores[[player]] <- score
      
      # Update total scores
      rv$total_scores[[player]] <- rv$total_scores[[player]] + score
    }
    
    # Add to history
    new_row <- data.frame(
      Round = rv$current_round_idx,
      Books = current_books,
      Dealer = rv$players[rv$dealer_idx],
      stringsAsFactors = FALSE
    )
    
    for (player in rv$players) {
      new_row[[paste0(player, "_Bet")]] <- rv$current_bets[[player]]
      new_row[[paste0(player, "_Won")]] <- rv$current_books[[player]]
      new_row[[paste0(player, "_Score")]] <- round_scores[[player]]
    }
    
    rv$round_history <- rbind(rv$round_history, new_row)
    rv$books_submitted <- TRUE
    
    # Auto-switch to scoreboard
    updateTabItems(session, "sidebar_menu", "scoreboard")
  })
  
  # Next round
  observeEvent(input$next_round, {
    req(rv$game_started)
    req(rv$books_submitted)
    
    # Move dealer to the left (next player)
    rv$dealer_idx <- (rv$dealer_idx %% length(rv$players)) + 1
    
    # Recalculate betting order (left of dealer bets first)
    betting_order <- c()
    for (i in 1:length(rv$players)) {
      idx <- ((rv$dealer_idx - 1 + i) %% length(rv$players)) + 1
      betting_order <- c(betting_order, idx)
    }
    rv$betting_order <- betting_order
    
    # Reset for next round
    rv$current_round_idx <- rv$current_round_idx + 1
    rv$current_bets <- list()
    rv$current_books <- list()
    rv$bets_submitted <- FALSE
    rv$books_submitted <- FALSE
    
    if (rv$current_round_idx > length(rv$rounds)) {
      showModal(modalDialog(
        title = "Game Complete!",
        "All rounds finished! Check the scoreboard for final results.",
        easyClose = TRUE
      ))
      updateTabItems(session, "sidebar_menu", "scoreboard")
    }
  })
  
  # Scoreboard table
  output$scoreboard_table <- renderDT({
    req(rv$game_started)
    
    if (length(rv$total_scores) == 0) {
      return(datatable(
        data.frame(Message = "Game not started"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    # Calculate statistics for each player
    stats_list <- lapply(rv$players, function(player) {
      player_rounds <- rv$round_history
      
      if (nrow(player_rounds) == 0) {
        return(data.frame(
          Rank = 0,
          Player = player,
          Total_Score = rv$total_scores[[player]],
          Hit_Rate = 0,
          Avg_Bet = 0,
          Current_Streak = 0,
          stringsAsFactors = FALSE
        ))
      }
      
      # Extract player's data
      bet_col <- paste0(player, "_Bet")
      won_col <- paste0(player, "_Won")
      
      if (!(bet_col %in% names(player_rounds)) || !(won_col %in% names(player_rounds))) {
        return(data.frame(
          Rank = 0,
          Player = player,
          Total_Score = rv$total_scores[[player]],
          Hit_Rate = 0,
          Avg_Bet = 0,
          Current_Streak = 0,
          stringsAsFactors = FALSE
        ))
      }
      
      bets <- player_rounds[[bet_col]]
      books <- player_rounds[[won_col]]
      
      # Calculate hits (correct predictions)
      hits <- bets == books
      hit_count <- sum(hits)
      rounds_played <- length(hits)
      hit_rate <- if (rounds_played > 0) round(100 * hit_count / rounds_played, 1) else 0
      
      # Calculate average bet
      avg_bet <- if (rounds_played > 0) round(mean(bets, na.rm = TRUE), 2) else 0
      
      # Calculate consecutive hits (current streak)
      consecutive <- 0
      for (i in rev(seq_along(hits))) {
        if (hits[i]) {
          consecutive <- consecutive + 1
        } else {
          break
        }
      }
      
      data.frame(
        Rank = 0,
        Player = player,
        Total_Score = rv$total_scores[[player]],
        Hit_Rate = paste0(hit_rate, "%"),
        Avg_Bet = avg_bet,
        Current_Streak = consecutive,
        stringsAsFactors = FALSE
      )
    })
    
    scoreboard <- bind_rows(stats_list) %>%
      arrange(desc(Total_Score))
    
    # Assign ranks
    scoreboard$Rank <- 1:nrow(scoreboard)
    
    dt <- datatable(
      scoreboard,
      options = list(
        pageLength = 10,
        dom = "t",
        ordering = FALSE,
        columnDefs = list(
          list(width = '50px', targets = 0)
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Rank',
        target = 'row',
        backgroundColor = styleEqual(
          c(1, 2, 3),
          c('#FFD700', '#C0C0C0', '#CD7F32')
        )
      ) %>%
      formatStyle(
        c('Rank', 'Player', 'Total_Score'),
        target = 'cell',
        color = styleEqual(
          c(1, 2, 3),
          c('#000000', '#000000', '#000000')
        ),
        backgroundColor = styleEqual(
          c(1, 2, 3),
          c('#FFD700', '#C0C0C0', '#CD7F32')
        )
      ) %>%
      formatStyle(
        c('Hit_Rate', 'Avg_Bet', 'Current_Streak'),
        target = 'cell',
        backgroundColor = 'white'
      ) %>%
      formatRound('Total_Score', 0) %>%
      formatRound('Avg_Bet', 2)
    
    return(dt)
  })
  
  # History table
  output$history_table <- renderDT({
    req(rv$game_started)
    
    if (nrow(rv$round_history) == 0) {
      return(datatable(
        data.frame(Message = "No rounds played yet"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    datatable(
      rv$round_history,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = "tip",
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
  # Score progression plot
  output$score_progression_plot <- renderPlotly({
    req(rv$game_started)
    
    if (nrow(rv$round_history) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No data yet - play some rounds!"))
    }
    
    # Build cumulative scores for each player
    score_data <- data.frame()
    
    for (player in rv$players) {
      score_col <- paste0(player, "_Score")
      
      if (score_col %in% names(rv$round_history)) {
        cumsum_scores <- cumsum(rv$round_history[[score_col]])
        
        player_data <- data.frame(
          Round = rv$round_history$Round,
          Player = player,
          Score = cumsum_scores,
          stringsAsFactors = FALSE
        )
        
        score_data <- rbind(score_data, player_data)
      }
    }
    
    if (nrow(score_data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No score data available"))
    }
    
    plot_ly(score_data, x = ~Round, y = ~Score, color = ~Player,
            type = 'scatter', mode = 'lines+markers',
            line = list(width = 3),
            marker = list(size = 8)) %>%
      layout(
        title = list(text = "Score Progression Throughout Game", 
                     font = list(color = "#FFD700", size = 18)),
        xaxis = list(title = "Round", 
                     gridcolor = "#444444",
                     color = "#FFD700"),
        yaxis = list(title = "Cumulative Score", 
                     gridcolor = "#444444",
                     color = "#FFD700"),
        paper_bgcolor = "#222222",
        plot_bgcolor = "#333333",
        font = list(color = "#FFD700"),
        legend = list(font = list(color = "#FFD700"))
      )
  })
  
  # Player statistics table
  output$player_stats_table <- renderDT({
    req(rv$game_started)
    
    if (nrow(rv$round_history) == 0) {
      return(datatable(
        data.frame(Message = "No rounds played yet"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    # Calculate detailed stats for each player
    stats_list <- lapply(rv$players, function(player) {
      bet_col <- paste0(player, "_Bet")
      won_col <- paste0(player, "_Won")
      score_col <- paste0(player, "_Score")
      
      if (!(bet_col %in% names(rv$round_history))) {
        return(NULL)
      }
      
      bets <- rv$round_history[[bet_col]]
      books <- rv$round_history[[won_col]]
      scores <- rv$round_history[[score_col]]
      
      hits <- bets == books
      
      data.frame(
        Player = player,
        Rounds = length(bets),
        Hits = sum(hits),
        Misses = sum(!hits),
        Hit_Pct = paste0(round(100 * sum(hits) / length(hits), 1), "%"),
        Avg_Bet = round(mean(bets), 2),
        Max_Bet = max(bets),
        Min_Bet = min(bets),
        Avg_Score_Per_Round = round(mean(scores), 2),
        Best_Round = max(scores),
        Worst_Round = min(scores),
        stringsAsFactors = FALSE
      )
    })
    
    stats_df <- bind_rows(stats_list)
    
    datatable(
      stats_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "t",
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
  # Bet distribution plot
  output$bet_distribution_plot <- renderPlotly({
    req(rv$game_started)
    
    if (nrow(rv$round_history) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No data yet - play some rounds!"))
    }
    
    # Collect all bets for each player
    bet_data <- data.frame()
    
    for (player in rv$players) {
      bet_col <- paste0(player, "_Bet")
      
      if (bet_col %in% names(rv$round_history)) {
        player_bets <- data.frame(
          Player = player,
          Bet = rv$round_history[[bet_col]],
          stringsAsFactors = FALSE
        )
        
        bet_data <- rbind(bet_data, player_bets)
      }
    }
    
    if (nrow(bet_data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No bet data available"))
    }
    
    plot_ly(bet_data, x = ~Player, y = ~Bet, color = ~Player,
            type = 'box') %>%
      layout(
        title = list(text = "Betting Distribution by Player", 
                     font = list(color = "#FFD700", size = 18)),
        xaxis = list(title = "Player", 
                     color = "#FFD700"),
        yaxis = list(title = "Bet Size", 
                     gridcolor = "#444444",
                     color = "#FFD700"),
        paper_bgcolor = "#222222",
        plot_bgcolor = "#333333",
        font = list(color = "#FFD700"),
        showlegend = FALSE
      )
  })
  
  # Auto-advance to next round when returning to gameplay tab
  observeEvent(input$sidebar_menu, {
    req(rv$game_started)
    req(rv$books_submitted)
    
    if (input$sidebar_menu == "gameplay" && rv$books_submitted) {
      # Move dealer to the left (next player)
      rv$dealer_idx <- (rv$dealer_idx %% length(rv$players)) + 1
      
      # Recalculate betting order (left of dealer bets first)
      betting_order <- c()
      for (i in 1:length(rv$players)) {
        idx <- ((rv$dealer_idx - 1 + i) %% length(rv$players)) + 1
        betting_order <- c(betting_order, idx)
      }
      rv$betting_order <- betting_order
      
      # Reset for next round
      rv$current_round_idx <- rv$current_round_idx + 1
      rv$current_bets <- list()
      rv$current_books <- list()
      rv$bets_submitted <- FALSE
      rv$books_submitted <- FALSE
      
      if (rv$current_round_idx > length(rv$rounds)) {
        showModal(modalDialog(
          title = "Game Complete!",
          "All rounds finished! Check the scoreboard for final results.",
          easyClose = TRUE
        ))
        updateTabItems(session, "sidebar_menu", "scoreboard")
      }
    }
  })
  
  # Download handler
  output$download_game <- downloadHandler(
    filename = function() {
      paste0("oh_hell_game_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if (nrow(rv$round_history) > 0) {
        write.csv(rv$round_history, file, row.names = FALSE)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)