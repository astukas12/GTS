# =============================================================================
# app.R
# Golden Ticket Research Center — NASCAR Research App
# =============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(openxlsx)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(ggplot2)
library(jsonlite)
library(tidyr)
library(stringr)

# =============================================================================
# CONSTANTS — computed once at startup, used throughout
# =============================================================================
CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
DATA_FILE    <- "NascarData.xlsx"

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

safe_trimws <- function(x) {
  tryCatch({
    x_clean <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
    trimws(x_clean)
  }, error = function(e) x)
}

# Load the Results sheet from NascarData.xlsx
load_nascar_database <- function() {
  if (!file.exists(DATA_FILE)) return(NULL)
  tryCatch({
    read_xlsx(DATA_FILE, sheet = "Results")
  }, error = function(e) {
    message("Error loading Results sheet: ", e$message)
    NULL
  })
}

# Load the Races sheet from NascarData.xlsx
load_races_sheet <- function() {
  if (!file.exists(DATA_FILE)) return(NULL)
  tryCatch({
    read_xlsx(DATA_FILE, sheet = "Races")
  }, error = function(e) {
    message("Error loading Races sheet: ", e$message)
    NULL
  })
}

# Load live entry list from NASCAR API for the upcoming race
load_entry_list <- function(race_season, series_id, race_id) {
  tryCatch({
    url <- sprintf("https://cf.nascar.com/cacher/%d/%d/%d/weekend-feed.json",
                   race_season, series_id, race_id)
    json_data <- fromJSON(url)
    json_data$weekend_race %>%
      unnest(results, names_sep = "_") %>%
      select(
        Start   = results_starting_position,
        Name    = results_driver_fullname,
        Car     = results_car_number,
        Team    = results_team_name,
        CC      = results_crew_chief_fullname,
        Make    = results_car_make,
        Sponsor = results_sponsor
      ) %>%
      mutate(
        Car     = as.integer(Car),
        Start   = as.integer(Start),
        across(c(Name, Team, CC, Sponsor, Make),
               ~iconv(., from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""))
      ) %>%
      arrange(Start)
  }, error = function(e) {
    data.frame(Start = integer(), Name = character(), Car = integer(),
               Team = character(), CC = character(),
               Make = character(), Sponsor = character())
  })
}

calc_dom_points <- function(total_laps, green_laps) {
  list(
    dk = round((0.45 * green_laps) + (0.25 * total_laps), 1),
    fd = round(0.1 * total_laps, 1)
  )
}

calc_finish_rates <- function(data, group_col, group_label) {
  data %>%
    group_by(!!sym(group_col)) %>%
    summarize(
      Races       = n(),
      Win         = round(mean(ps == 1,  na.rm = TRUE) * 100, 1),
      `Top 3`     = round(mean(ps <= 3,  na.rm = TRUE) * 100, 1),
      `Top 5`     = round(mean(ps <= 5,  na.rm = TRUE) * 100, 1),
      `Top 10`    = round(mean(ps <= 10, na.rm = TRUE) * 100, 1),
      `Top 15`    = round(mean(ps <= 15, na.rm = TRUE) * 100, 1),
      `Top 20`    = round(mean(ps <= 20, na.rm = TRUE) * 100, 1),
      `Top 25`    = round(mean(ps <= 25, na.rm = TRUE) * 100, 1),
      `Top 30`    = round(mean(ps <= 30, na.rm = TRUE) * 100, 1),
      `Avg Finish`= round(mean(ps,       na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    rename(!!group_label := !!sym(group_col))
}

# Series id -> display label
series_label <- function(id) {
  switch(as.character(id),
         "1" = "Cup Series",
         "2" = "OReilly Series",
         "3" = "Truck Series",
         "Cup Series"
  )
}

# Series id -> salary file prefix  (DKCup.csv, DKOReilly.csv, DKTrucks.csv)
series_salary_prefix <- function(id) {
  switch(as.character(id),
         "1" = "Cup",
         "2" = "OReilly",
         "3" = "Trucks",
         "Cup"
  )
}

# FanDuel only runs Cup Series slates. OReilly (Xfinity) and Trucks are
# DraftKings-only, so FD salary/stat columns are suppressed for them
# everywhere in the app.
is_cup_series <- function(id) {
  as.character(id) == "1"
}

# Normalize a driver name for matching: lowercase, strip accents, punctuation,
# common suffixes, and collapse whitespace. Handles "van Gisbergen", "Jr.",
# accented characters, hyphens, etc.
normalize_name <- function(x) {
  x <- as.character(x)
  x <- tolower(trimws(x))
  x <- iconv(x, to = "ASCII//TRANSLIT")          # drop accents
  x <- gsub("[.'`]", "", x)                        # drop apostrophes/periods
  x <- gsub("[-_]", " ", x)                        # hyphens -> space
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", x)    # suffixes
  x <- gsub("[^a-z ]", "", x)                       # keep letters + space
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Fuzzy left-join salary onto entry_list by normalized name. Exact normalized
# match first; then first+last-token match (handles middle name vs initial,
# e.g. "John H. Nemechek" <-> "John Hunter Nemechek"); then nearest
# Levenshtein within a length-scaled tolerance. Returns entry_list with the
# salary/id cols.
fuzzy_join_salary <- function(entry_list, sal, sal_cols) {
  if (is.null(sal) || nrow(sal) == 0) return(entry_list)
  el_norm  <- normalize_name(entry_list$Name)
  sal_norm <- normalize_name(sal$Name)
  
  # first+last token key (drops middle names/initials)
  fl_key <- function(v) vapply(strsplit(v, " "), function(p) {
    p <- p[nzchar(p)]
    if (length(p) == 0) "" else if (length(p) == 1) p[1]
    else paste(p[1], p[length(p)])
  }, character(1))
  el_fl  <- fl_key(el_norm)
  sal_fl <- fl_key(sal_norm)
  
  for (cc in sal_cols) entry_list[[cc]] <- NA
  
  used <- rep(FALSE, nrow(sal))
  for (i in seq_len(nrow(entry_list))) {
    j <- which(sal_norm == el_norm[i] & !used)[1]                 # exact
    if (is.na(j) && nzchar(el_fl[i]))
      j <- which(sal_fl == el_fl[i] & !used)[1]                   # first+last
    if (is.na(j)) {
      cand <- which(!used)
      if (length(cand) > 0 && nzchar(el_norm[i])) {
        d <- utils::adist(el_norm[i], sal_norm[cand])[1, ]
        k <- which.min(d)
        tol <- max(2, floor(nchar(el_norm[i]) * 0.25))
        if (length(k) == 1 && d[k] <= tol) j <- cand[k]           # fuzzy
      }
    }
    if (!is.na(j)) {
      used[j] <- TRUE
      for (cc in sal_cols) entry_list[[cc]][i] <- sal[[cc]][j]
    }
  }
  entry_list
}

# =============================================================================
# UI
# =============================================================================

# Shared empty-state panel shown on tabs before races are loaded
no_races_panel <- function() {
  fluidRow(column(12,
                  div(class = "box",
                      div(class = "box-header", h3("No Races Loaded", class = "box-title")),
                      div(class = "box-body",
                          div(class = "empty-state",
                              div(class = "empty-state-icon", "🏁"),
                              p(class = "empty-state-text",
                                "Races will load automatically — or use Race Selection to change filters.")
                          )
                      )
                  )
  ))
}

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    # External CSS — all styles live in gts_theme.css
    tags$link(rel = "stylesheet", type = "text/css", href = "gts_theme.css"),
    tags$script(src = "custom-handlers.js"),
    
    # Page title
    tags$title("Golden Ticket Research Lab")
  ),
  
  # ---- HEADER ----
  div(class = "app-header",
      div(class = "app-header-left",
          img(src = "logo.jpg", class = "app-logo"),
          h1("Golden Ticket Research Labr", class = "app-title")
      )
  ),
  
  navbarPage(
    title       = NULL,
    id          = "main_tabs",
    windowTitle = "Golden Ticket Research Lab",
    
    # =========================================================================
    # SETUP TAB  (merged Race Selection + Entry List; pool builders as sub-tabs)
    # =========================================================================
    tabPanel("Setup", value = "setup",
             
             # ---- Race selection (always visible) ----
             fluidRow(column(12,
                             div(class = "box",
                                 div(class = "box-header", h3("Race Selection", class = "box-title")),
                                 div(class = "box-body",
                                     fluidRow(
                                       column(4, selectizeInput("analysis_series", "Series:",
                                                                choices  = c("Cup Series" = 1, "OReilly Series" = 2, "Truck Series" = 3),
                                                                selected = 1)),
                                       column(4, selectizeInput("analysis_primary_track", "Track:",
                                                                choices = NULL,
                                                                options = list(placeholder = "Select Track"))),
                                       column(4, selectizeInput("analysis_race_id", "Race:",
                                                                choices = NULL,
                                                                options = list(placeholder = "Select Race")))
                                     )
                                 )
                             )
             )),
             
             conditionalPanel(condition = "output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-body", style = "padding-top:8px;",
                                                      tabsetPanel(
                                                        id = "setup_subtabs",
                                                        type = "tabs",
                                                        
                                                        # ---- SUB-TAB: Dominator Pool ----
                                                        tabPanel("Dominator Pool", value = "setup_dom_pool",
                                                                 div(style = "padding-top:14px;",
                                                                     
                                                                     div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:10px;",
                                                                         h3("Dominator Pool Builder", class = "box-title", style = "margin:0;"),
                                                                         uiOutput("dom_build_badge", inline = TRUE)
                                                                     ),
                                                                     
                                                                     # target band panel + slider
                                                                     uiOutput("dom_target_panel"),
                                                                     div(style = "margin-top:6px;", uiOutput("dom_band_slider_ui")),
                                                                     
                                                                     # track exclude pills — click a track off to drop its races
                                                                     div(style = "margin-top:12px;",
                                                                         p(style = "color:#aaaaaa;font-size:12px;font-weight:600;margin:0 0 6px;text-transform:uppercase;letter-spacing:0.5px;",
                                                                           "Tracks in pool — click to exclude"),
                                                                         uiOutput("dom_track_pills_ui")
                                                                     ),
                                                                     
                                                                     hr(style = "border-color:#3a3a3a;margin:14px 0;"),
                                                                     
                                                                     # aggregate preview
                                                                     div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;",
                                                                         h3("Mix Preview — what this pool yields", class = "box-title", style = "margin:0;"),
                                                                         downloadButton("download_dominator_profile", "Download Profile", class = "btn-warning", style = "margin:0;")
                                                                     ),
                                                                     uiOutput("dom_mix_summary"),
                                                                     uiOutput("dom_mix_plot"),
                                                                     
                                                                     hr(style = "border-color:#3a3a3a;margin:14px 0;"),
                                                                     
                                                                     # candidate race bars
                                                                     div(style = "color:#666;font-size:11px;margin-bottom:8px;",
                                                                         HTML("Sorted by closeness of DK dom total to target. ",
                                                                              "<b style='color:#FFE500;'>In-band</b> races are included by default; ",
                                                                              "click any card to toggle. Each bar is a dominator (DK pts), labeled ",
                                                                              "<b>start&rarr;finish</b>.")),
                                                                     withSpinner(uiOutput("dom_profile_cards"))
                                                                 )
                                                        ),
                                                        
                                                        # ---- SUB-TAB: Performance Pool ----
                                                        tabPanel("Performance Pool", value = "setup_perf_pool",
                                                                 div(style = "padding-top:14px;",
                                                                     div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:10px;",
                                                                         h3("Performance Pool Builder", class = "box-title", style = "margin:0;"),
                                                                         uiOutput("perf_build_badge", inline = TRUE)
                                                                     ),
                                                                     
                                                                     div(style = "color:#666;font-size:11px;margin-bottom:12px;",
                                                                         "Feeds Finish Rates, Performance, and Place Differential. Filter the pool by track type, track, and lap count; click any race to toggle it."),
                                                                     
                                                                     # filter controls
                                                                     div(style = "display:flex;align-items:flex-start;gap:24px;flex-wrap:wrap;margin-bottom:8px;",
                                                                         div(
                                                                           p(style = "color:#aaaaaa;font-size:12px;font-weight:600;margin:0 0 6px;text-transform:uppercase;letter-spacing:0.5px;", "Perf Pool"),
                                                                           uiOutput("same_track_toggle_ui")
                                                                         ),
                                                                         div(style = "flex:1;min-width:240px;",
                                                                             p(style = "color:#aaaaaa;font-size:12px;font-weight:600;margin:0 0 6px;text-transform:uppercase;letter-spacing:0.5px;", "Track Types"),
                                                                             uiOutput("track_type_pills_ui")
                                                                         ),
                                                                         div(style = "min-width:130px;",
                                                                             uiOutput("perf_season_from_ui")
                                                                         )
                                                                     ),
                                                                     
                                                                     hr(style = "border-color:#3a3a3a;margin:12px 0;"),
                                                                     
                                                                     uiOutput("perf_pool_summary"),
                                                                     withSpinner(uiOutput("perf_pool_cards"))
                                                                 )
                                                        ),
                                                        
                                                        # ---- SUB-TAB: Entry List ----
                                                        tabPanel("Entry List", value = "setup_entry_list",
                                                                 div(style = "padding-top:14px;",
                                                                     div(class = "box-header",
                                                                         style = "display:flex;justify-content:space-between;align-items:center;padding:0 0 10px;",
                                                                         uiOutput("entry_list_title", inline = TRUE),
                                                                         div(style = "display:flex;gap:8px;align-items:center;",
                                                                             downloadButton("download_entry_list_csv",   "CSV",               class = "btn-success", style = "margin:0;"),
                                                                             downloadButton("download_entry_list_excel", "Excel",             class = "btn-success", style = "margin:0;"),
                                                                             downloadButton("download_input_file",       "Create Input File", class = "btn-warning",  style = "margin:0;")
                                                                         )
                                                                     ),
                                                                     withSpinner(DT::dataTableOutput("entry_list_table"))
                                                                 )
                                                        )
                                                      )
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    ),
    
    # =========================================================================
    # DOMINATOR TAB  (Data / Visualizations sub-tabs)
    # =========================================================================
    tabPanel("Dominator", value = "dominator",
             
             conditionalPanel(condition = "output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-body", style = "padding-top:8px;",
                                                      tabsetPanel(
                                                        id = "dominator_subtabs", type = "tabs",
                                                        
                                                        tabPanel("Data", value = "dom_data",
                                                                 div(style = "padding-top:14px;",
                                                                     div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:10px;",
                                                                         h3("Dominator Data (included pool)", class = "box-title", style = "margin:0;"),
                                                                         downloadButton("download_dominator_csv", "CSV", class = "btn-success", style = "margin:0;")
                                                                     ),
                                                                     withSpinner(DT::dataTableOutput("dominator_data_table"))
                                                                 )
                                                        ),
                                                        
                                                        tabPanel("Visualizations", value = "dom_viz",
                                                                 div(style = "padding-top:14px;",
                                                                     fluidRow(
                                                                       column(6, selectInput("dom_visual_type", "Select Visualization:",
                                                                                             choices = c(
                                                                                               "Score Distribution by Dom Rank"  = "score_dist",
                                                                                               "Dom Rank Finish Ranges"           = "rank_finish",
                                                                                               "Dom Pts by Finish Position"       = "pts_by_finish",
                                                                                               "Dom Pts by Starting Position"     = "dom_pts_start",
                                                                                               "Dom Rank by Starting Position"    = "dom_rank_start",
                                                                                               "Laps Led by Finish Position"      = "laps_led",
                                                                                               "Laps Led by Starting Position"    = "laps_led_start",
                                                                                               "Fast Laps by Finish Position"     = "fast_laps",
                                                                                               "Fast Laps by Starting Position"   = "fast_laps_start",
                                                                                               "Driver Dominator Boxplots"        = "driver_boxplot",
                                                                                               "Team Dominator Boxplots"          = "team_boxplot"
                                                                                             ),
                                                                                             selected = "score_dist"
                                                                       )),
                                                                       column(6, radioButtons("dom_platform", "Platform:",
                                                                                              choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                                                                              selected = "DK", inline = TRUE))
                                                                     ),
                                                                     withSpinner(plotlyOutput("dominator_plot", height = "540px"))
                                                                 )
                                                        )
                                                      )
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    ),
    
    # =========================================================================
    # PERFORMANCE TAB  (Finish Rates / Data / Visualizations sub-tabs)
    # =========================================================================
    tabPanel("Performance", value = "performance",
             
             conditionalPanel(condition = "output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-body", style = "padding-top:8px;",
                                                      tabsetPanel(
                                                        id = "performance_subtabs", type = "tabs",
                                                        
                                                        # ---- SUB-TAB: Finish Rates ----
                                                        tabPanel("Finish Rates", value = "perf_finish_rates",
                                                                 div(style = "padding-top:14px;",
                                                                     div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:10px;",
                                                                         h3("Finish Rate Controls", class = "box-title", style = "margin:0;"),
                                                                         downloadButton("download_finish_rates", "Download", class = "btn-success", style = "margin:0;")
                                                                     ),
                                                                     
                                                                     div(style = "display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin-bottom:10px;",
                                                                         span(style = "color:#aaa;font-size:12px;font-weight:600;text-transform:uppercase;letter-spacing:0.5px;", "View By:"),
                                                                         uiOutput("fr_view_pills_ui")
                                                                     ),
                                                                     div(style = "display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin-bottom:10px;",
                                                                         span(style = "color:#aaa;font-size:12px;font-weight:600;text-transform:uppercase;letter-spacing:0.5px;", "Seasons:"),
                                                                         uiOutput("fr_time_pills_ui")
                                                                     ),
                                                                     
                                                                     conditionalPanel(condition = "output.fr_view_is_tier",
                                                                                      div(style = "background:#222;border:1px solid #555;border-radius:4px;padding:10px;margin-top:4px;",
                                                                                          p(style = "color:#FFE500;font-weight:bold;margin-bottom:8px;", "Team Tier Configuration"),
                                                                                          uiOutput("tier_config_ui"),
                                                                                          div(style = "margin-top:8px;display:flex;gap:8px;",
                                                                                              actionButton("add_tier",    "+ Add Tier",    class = "btn-info",   style = "font-size:12px;padding:4px 10px;"),
                                                                                              actionButton("remove_tier", "- Remove Tier", class = "btn-danger", style = "font-size:12px;padding:4px 10px;")
                                                                                          )
                                                                                      )
                                                                     ),
                                                                     
                                                                     hr(style = "border-color:#3a3a3a;margin:14px 0;"),
                                                                     h3("Finish Rates (%)", class = "box-title"),
                                                                     withSpinner(DT::dataTableOutput("finish_rates_table"))
                                                                 )
                                                        ),
                                                        
                                                        # ---- SUB-TAB: Data ----
                                                        tabPanel("Data", value = "perf_data",
                                                                 div(style = "padding-top:14px;",
                                                                     fluidRow(
                                                                       column(6, uiOutput("perf_time_ui")),
                                                                       column(6, downloadButton("download_performance_csv", "Download CSV",
                                                                                                class = "btn-success", style = "margin-top:0px;"))
                                                                     ),
                                                                     withSpinner(DT::dataTableOutput("performance_data_table"))
                                                                 )
                                                        ),
                                                        
                                                        # ---- SUB-TAB: Visualizations ----
                                                        tabPanel("Visualizations", value = "perf_viz",
                                                                 div(style = "padding-top:14px;",
                                                                     fluidRow(
                                                                       column(6, selectInput("perf_visual_type", "Select Visualization:",
                                                                                             choices = c(
                                                                                               "Driver Speed Rank Distribution" = "driver_speed",
                                                                                               "Team Speed Rank Distribution"   = "team_speed",
                                                                                               "Driver Finish Distribution"     = "driver_finish",
                                                                                               "Team Finish Distribution"       = "team_finish",
                                                                                               "Driver ARP Distribution"        = "driver_arp",
                                                                                               "Team ARP Distribution"          = "team_arp"
                                                                                             ),
                                                                                             selected = "driver_speed"
                                                                       )),
                                                                       column(6, uiOutput("perf_visual_time_ui"))
                                                                     ),
                                                                     withSpinner(plotlyOutput("performance_plot", height = "540px"))
                                                                 )
                                                        )
                                                      )
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    ),
    
    # =========================================================================
    # PLACE DIFFERENTIAL TAB  (Data / Visualizations sub-tabs)
    # =========================================================================
    tabPanel("Place Differential", value = "place_differential",
             
             conditionalPanel(condition = "output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-body", style = "padding-top:8px;",
                                                      tabsetPanel(
                                                        id = "pd_subtabs", type = "tabs",
                                                        
                                                        tabPanel("Data", value = "pd_data",
                                                                 div(style = "padding-top:14px;",
                                                                     div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:10px;",
                                                                         h3("Place Differential Data", class = "box-title", style = "margin:0;"),
                                                                         downloadButton("download_pd_csv", "CSV", class = "btn-success", style = "margin:0;")
                                                                     ),
                                                                     withSpinner(DT::dataTableOutput("pd_data_table"))
                                                                 )
                                                        ),
                                                        
                                                        tabPanel("Visualizations", value = "pd_viz",
                                                                 div(style = "padding-top:14px;",
                                                                     fluidRow(
                                                                       column(4, selectInput("pd_visual_type", "Visualization Type:",
                                                                                             choices = c(
                                                                                               "Start vs Finish Scatter"      = "scatter",
                                                                                               "Position Change Distribution" = "histogram",
                                                                                               "PD by Start Position"         = "boxplot_start",
                                                                                               "PD by Finish Position"        = "boxplot_finish"
                                                                                             )
                                                                       ))
                                                                     ),
                                                                     withSpinner(plotlyOutput("pd_plot", height = "540px"))
                                                                 )
                                                        )
                                                      )
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    ),
    
    # =========================================================================
    # FANTASY SCORING TAB  (Data / Visualizations sub-tabs)
    # =========================================================================
    tabPanel("Fantasy Scoring", value = "fantasy_scoring",
             
             conditionalPanel(condition = "output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-body", style = "padding-top:8px;",
                                                      tabsetPanel(
                                                        id = "fantasy_subtabs", type = "tabs",
                                                        
                                                        tabPanel("Data", value = "fs_data",
                                                                 div(style = "padding-top:14px;",
                                                                     fluidRow(
                                                                       column(6, radioButtons("fs_platform", "Platform:",
                                                                                              choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                                                                              selected = "DK", inline = TRUE)),
                                                                       column(6, downloadButton("download_fantasy_csv", "Download CSV",
                                                                                                class = "btn-success", style = "margin-top:0px;"))
                                                                     ),
                                                                     withSpinner(DT::dataTableOutput("fantasy_data_table"))
                                                                 )
                                                        ),
                                                        
                                                        tabPanel("Visualizations", value = "fs_viz",
                                                                 div(style = "padding-top:14px;",
                                                                     fluidRow(
                                                                       column(6, selectInput("fs_visual_type", "Select Visualization:",
                                                                                             choices = c(
                                                                                               "Score Distribution by Rank"   = "score_dist",
                                                                                               "Scoring Components Breakdown" = "components",
                                                                                               "Score Distribution by Start"  = "score_by_start",
                                                                                               "Score Distribution by Finish" = "score_by_finish"
                                                                                             ),
                                                                                             selected = "score_dist"
                                                                       )),
                                                                       column(6, radioButtons("fs_visual_platform", "Platform:",
                                                                                              choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                                                                              selected = "DK", inline = TRUE))
                                                                     ),
                                                                     withSpinner(plotlyOutput("fantasy_plot", height = "540px"))
                                                                 )
                                                        )
                                                      )
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    )
    
  ) # end navbarPage
) # end fluidPage

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Null-coalescing operator
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
  
  # ---------------------------------------------------------------------------
  # REACTIVE VALUES
  # ---------------------------------------------------------------------------
  values <- reactiveValues(
    nascar_data              = NULL,
    races_sheet              = NULL,
    analysis_filtered_data   = NULL,
    analysis_entry_list      = NULL,
    analysis_races_available = NULL,
    filters_confirmed        = FALSE,
    pool_state               = NULL,   # race_id, dom, perf, dom_manual, perf_manual
    # Perf pool bulk controls
    perf_same_track_only     = FALSE,
    perf_track_types         = character(0),  # track types currently included
    perf_season_from         = 2022L,
    perf_same_track_toggle   = 0L,            # toggle counter for button
    dom_track_exclude        = character(0),  # track names excluded from dom pool
    num_tiers                = 3,
    fr_view_sel              = "driver",    # active view pill
    fr_seasons_sel           = "all",       # active season pills ("all" or char vec of years)
    # Dominator profile builder
    dom_target_lo            = NULL,        # DK dom-total acceptance band (hard gate)
    dom_target_hi            = NULL,
    dom_target_laps          = NULL,        # upcoming race scheduled lap count
    dom_include              = NULL,        # named logical vector keyed by race_id
    dom_band_initialized     = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # STARTUP: load data, populate dropdowns, auto-trigger filter load
  # ---------------------------------------------------------------------------
  observe({
    withProgress(message = "Loading Golden Ticket Database...", {
      
      incProgress(0.1, detail = "Reading database...")
      values$nascar_data <- load_nascar_database()
      values$races_sheet <- load_races_sheet()
      
      req(values$races_sheet)
      incProgress(0.3, detail = "Processing tracks...")
      
      # Tracks with historical results OR an upcoming scheduled race.
      # New venues (e.g. San Diego Street Course) have no Results rows yet,
      # so they must be unioned in from the Races sheet or they vanish.
      tracks_with_data <- unique(values$nascar_data$track_name[!is.na(values$nascar_data$track_name)])
      upcoming_tracks  <- unique(values$races_sheet$track_name[
        !is.na(values$races_sheet$race_date) &
          as.Date(substr(values$races_sheet$race_date, 1, 10)) >= Sys.Date() &
          !is.na(values$races_sheet$track_name)])
      all_tracks <- sort(unique(values$races_sheet$track_name[
        (values$races_sheet$track_name %in% tracks_with_data |
           values$races_sheet$track_name %in% upcoming_tracks) &
          !is.na(values$races_sheet$track_name)]))
      
      # Find next upcoming race for Cup Series (series 1) and pre-select
      upcoming <- values$races_sheet %>%
        filter(!is.na(race_date),
               as.Date(substr(race_date, 1, 10)) >= Sys.Date(),
               series_id == 1) %>%
        arrange(race_date)
      
      if (nrow(upcoming) > 0) {
        next_race <- upcoming[1, ]
        # Pre-select series, track — the series observer will fire and
        # populate the race dropdown, which triggers the pool load
        updateSelectizeInput(session, "analysis_series",
                             selected = as.character(next_race$series_id))
        updateSelectizeInput(session, "analysis_primary_track",
                             choices = all_tracks, selected = next_race$track_name)
      } else {
        updateSelectizeInput(session, "analysis_primary_track", choices = all_tracks)
      }
      
      incProgress(0.8, detail = "Auto-loading races...")
    })
    
  })
  
  # ---------------------------------------------------------------------------
  # DYNAMIC YEAR RADIO BUTTONS
  # Replaces all hardcoded "2025 Only" labels throughout the app
  # ---------------------------------------------------------------------------
  year_radio_choices <- reactive({
    c("Full History" = "all", setNames(as.character(CURRENT_YEAR), paste(CURRENT_YEAR, "Only")))
  })
  
  output$fr_time_ui <- renderUI({
    radioButtons("fr_time", "Time Period:",
                 choices = year_radio_choices(), selected = as.character(CURRENT_YEAR), inline = TRUE)
  })
  
  # FR view pills — JS setInputValue → fr_view_pill
  output$fr_view_pills_ui <- renderUI({
    req(values$filters_confirmed)
    views <- c("Driver" = "driver", "Car" = "car", "Team" = "team",
               "Start Pos" = "start_pos", "Tier" = "tier")
    cur <- values$fr_view_sel %||% "driver"
    div(style = "display:flex;flex-wrap:wrap;gap:4px;",
        tagList(mapply(function(lbl, val) {
          is_on <- identical(cur, val)
          js    <- sprintf("Shiny.setInputValue('fr_view_pill', '%s', {priority:'event'})", val)
          tags$span(onClick = js, style = paste0(
            "display:inline-block;cursor:pointer;user-select:none;",
            "margin:2px;padding:4px 12px;font-size:12px;font-weight:600;border-radius:20px;",
            if (is_on) "background:#FFE500;color:#000;border:2px solid #FFE500;"
            else       "background:#2a2a2a;color:#888;border:2px solid #444;"), lbl)
        }, names(views), views, SIMPLIFY = FALSE)))
  })
  
  observeEvent(input$fr_view_pill, {
    if (!is.null(input$fr_view_pill)) values$fr_view_sel <- input$fr_view_pill
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # FR time pills — JS setInputValue → fr_time_pill
  output$fr_time_pills_ui <- renderUI({
    req(values$analysis_races_available)
    seasons <- sort(unique(values$analysis_races_available$race_season[
      values$analysis_races_available$race_id %in% performance_race_ids_reactive()]))
    cur <- values$fr_seasons_sel %||% "all"
    make_pill <- function(lbl, val) {
      is_on <- if (val == "all") identical(cur, "all") else (!identical(cur,"all") && val %in% cur)
      js    <- sprintf("Shiny.setInputValue('fr_time_pill', '%s', {priority:'event'})", val)
      tags$span(onClick = js, style = paste0(
        "display:inline-block;cursor:pointer;user-select:none;",
        "margin:2px;padding:4px 12px;font-size:12px;font-weight:600;border-radius:20px;",
        if (is_on) "background:#FFE500;color:#000;border:2px solid #FFE500;"
        else       "background:#2a2a2a;color:#888;border:2px solid #444;"), lbl)
    }
    div(style = "display:flex;flex-wrap:wrap;gap:4px;",
        make_pill("All", "all"),
        tagList(lapply(as.character(seasons), function(s) make_pill(s, s))))
  })
  
  observeEvent(input$fr_time_pill, {
    clicked <- input$fr_time_pill
    if (is.null(clicked)) return()
    cur <- values$fr_seasons_sel %||% "all"
    if (clicked == "all") {
      values$fr_seasons_sel <- "all"
    } else if (identical(cur, "all")) {
      values$fr_seasons_sel <- clicked
    } else if (clicked %in% cur) {
      remaining <- setdiff(cur, clicked)
      values$fr_seasons_sel <- if (length(remaining) == 0) "all" else remaining
    } else {
      values$fr_seasons_sel <- c(cur, clicked)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Expose fr_view for conditionalPanel
  output$fr_view_is_tier <- reactive({
    isTRUE(values$fr_view_sel == "tier")
  })
  outputOptions(output, "fr_view_is_tier", suspendWhenHidden = FALSE)
  
  output$perf_time_ui <- renderUI({
    radioButtons("perf_time_filter", "Time Period:",
                 choices = year_radio_choices(), selected = "all", inline = TRUE)
  })
  
  output$perf_visual_time_ui <- renderUI({
    radioButtons("perf_visual_time", "Time Period:",
                 choices = year_radio_choices(), selected = "all", inline = TRUE)
  })
  
  # ---------------------------------------------------------------------------
  # SERIES CHANGE: update track list AND pre-select next race for that series
  # ---------------------------------------------------------------------------
  observeEvent(input$analysis_series, {
    req(input$analysis_series, values$races_sheet, values$nascar_data)
    sid <- as.numeric(input$analysis_series)
    
    tracks_with_data <- values$nascar_data %>%
      filter(series_id == sid) %>%
      pull(track_name) %>%
      unique()
    # Include upcoming scheduled tracks for this series even with no history
    upcoming_tracks <- values$races_sheet %>%
      filter(series_id == sid,
             !is.na(race_date),
             as.Date(substr(race_date, 1, 10)) >= Sys.Date()) %>%
      pull(track_name) %>%
      unique()
    all_tracks <- sort(unique(values$races_sheet$track_name[
      values$races_sheet$track_name %in% c(tracks_with_data, upcoming_tracks)]))
    
    # Find next upcoming race for this series and pre-select its track
    next_up <- values$races_sheet %>%
      filter(series_id == sid,
             !is.na(race_date),
             as.Date(substr(race_date, 1, 10)) >= Sys.Date()) %>%
      arrange(race_date) %>%
      slice(1)
    
    if (nrow(next_up) > 0) {
      updateSelectizeInput(session, "analysis_primary_track",
                           choices = all_tracks, selected = next_up$track_name)
    } else {
      updateSelectizeInput(session, "analysis_primary_track", choices = all_tracks)
    }
  }, ignoreInit = TRUE)
  
  # ---------------------------------------------------------------------------
  # RACE DROPDOWN: upcoming races at selected series + track
  # ---------------------------------------------------------------------------
  observe({
    req(input$analysis_series, input$analysis_primary_track, values$races_sheet)
    
    available <- values$races_sheet %>%
      filter(
        series_id  == as.numeric(input$analysis_series),
        track_name == input$analysis_primary_track,
        !is.na(race_date),
        as.Date(substr(race_date, 1, 10)) >= Sys.Date()
      ) %>%
      arrange(race_date) %>%
      mutate(race_label = paste0(race_season, " — ", race_name))
    
    choices <- setNames(available$race_id, available$race_label)
    updateSelectizeInput(session, "analysis_race_id",
                         choices  = choices,
                         selected = if (length(choices) > 0) choices[1] else NULL)
  })
  
  # ---------------------------------------------------------------------------
  # LOAD RACES: main filter handler (also triggered automatically on startup)
  # ---------------------------------------------------------------------------
  # Fires on series or track change (no button needed)
  observeEvent(list(input$analysis_series, input$analysis_primary_track), {
    req(input$analysis_series, input$analysis_primary_track,
        values$races_sheet, values$nascar_data)
    
    withProgress(message = "Loading races...", {
      incProgress(0.2)
      
      selected_series <- as.numeric(input$analysis_series)
      selected_track  <- input$analysis_primary_track
      
      # Get selected track type for perf pool pre-population
      selected_track_type <- values$races_sheet %>%
        filter(track_name == selected_track) %>%
        pull(track_type) %>%
        first()
      
      # All historical races across the series — every series/track combo
      # that has data is eligible; filter to Historical only
      all_historical <- values$races_sheet %>%
        filter(Historical == "Y",
               series_id  == selected_series)
      
      incProgress(0.4)
      
      # Join race aggregates from Results sheet
      race_aggs <- values$nascar_data %>%
        filter(series_id == selected_series) %>%
        group_by(race_id) %>%
        summarise(
          total_laps   = first(act_laps),
          lead_lap     = sum(LapsDown == 0, na.rm = TRUE),
          crash_dnfs   = sum(finishing_status %in%
                               c("Accident", "DVP", "Damage"), na.rm = TRUE),
          mech_dnfs    = sum(!finishing_status %in%
                               c("Running", "Accident", "DVP", "Damage") &
                               !is.na(finishing_status), na.rm = TRUE),
          DK_Dom_Total = round(sum(DKSP, na.rm = TRUE), 1),
          FD_Dom_Total = round(sum(FDSP, na.rm = TRUE), 1),
          .groups = "drop"
        )
      
      races_available <- all_historical %>%
        inner_join(race_aggs, by = "race_id") %>%
        mutate(
          total_laps    = if_else(is.na(total_laps), scheduled_laps, total_laps),
          is_same_track = track_name == selected_track,
          is_same_type  = !is.na(track_type) & !is.na(selected_track_type) &
            track_type == selected_track_type
        ) %>%
        arrange(desc(is_same_track), desc(race_season))
      
      incProgress(0.6)
      
      # Auto dom pool: ±25% of median DK dom total at this track+series
      # If no same-track history, use full series median
      same_track_dom <- races_available %>%
        filter(is_same_track, DK_Dom_Total > 0) %>%
        pull(DK_Dom_Total)
      # Default range: ±25% of median, but always wide enough to include
      # all same-track races so the selected track's history is in Dom pool
      ref_median <- if (length(same_track_dom) >= 2) {
        median(same_track_dom, na.rm = TRUE)
      } else {
        median(races_available$DK_Dom_Total[races_available$DK_Dom_Total > 0],
               na.rm = TRUE)
      }
      dom_lo <- if (length(same_track_dom) > 0) {
        min(floor(ref_median * 0.75), floor(min(same_track_dom) * 0.95))
      } else {
        floor(ref_median * 0.75)
      }
      dom_hi <- if (length(same_track_dom) > 0) {
        max(ceiling(ref_median * 1.25), ceiling(max(same_track_dom) * 1.05))
      } else {
        ceiling(ref_median * 1.25)
      }
      
      # Default perf pill state: tracks of same type as selected track
      default_perf_tracks <- if (!is.na(selected_track_type)) {
        unique(races_available$track_name[
          races_available$track_type == selected_track_type &
            !is.na(races_available$track_name)])
      } else {
        unique(races_available$track_name[!is.na(races_available$track_name)])
      }
      
      # Build initial pool_state: auto-assign dom and perf flags
      pool_state <- races_available %>%
        transmute(
          race_id,
          dom        = DK_Dom_Total >= dom_lo & DK_Dom_Total <= dom_hi,
          perf       = track_name %in% default_perf_tracks,
          dom_manual = FALSE,
          perf_manual= FALSE
        )
      
      # Store perf control state
      values$perf_same_track_only   <- FALSE
      values$perf_track_types       <- default_perf_tracks
      values$perf_season_from       <- min(races_available$race_season, na.rm = TRUE)
      values$perf_same_track_toggle <- 0L
      values$dom_track_exclude      <- character(0)
      
      incProgress(0.8)
      
      filtered_nascar <- values$nascar_data %>%
        filter(race_id %in% races_available$race_id)
      
      values$analysis_filtered_data   <- filtered_nascar
      values$analysis_entry_list      <- NULL   # reset; entry list observer reloads it
      values$analysis_races_available <- races_available
      values$pool_state               <- pool_state
      values$filters_confirmed        <- TRUE
      
      # Store auto-range for slider
      values$dom_lo <- dom_lo
      values$dom_hi <- dom_hi
      
      incProgress(1.0)
      n_dom  <- sum(pool_state$dom)
      n_perf <- sum(pool_state$perf)
      showNotification(
        sprintf("Loaded %d races. Dom pool: %d | Perf pool: %d",
                nrow(races_available), n_dom, n_perf),
        type = "message", duration = 5)
    })
  })
  
  # ---------------------------------------------------------------------------
  # PERF POOL BULK CONTROLS — pills, toggle, season
  # ---------------------------------------------------------------------------
  
  # Same-track-only toggle button
  output$same_track_toggle_ui <- renderUI({
    req(values$analysis_races_available)
    is_on <- isTRUE(values$perf_same_track_only)
    actionButton("perf_same_track_btn",
                 label  = if (is_on) "★ Same Track Only" else "★ Same Track Only",
                 class  = if (is_on) "btn-primary" else "btn-default",
                 style  = paste0(
                   "font-size:12px;padding:5px 12px;font-weight:600;",
                   if (is_on) "background:#FFE500!important;color:#000!important;border-color:#FFE500!important;"
                   else "background:#333!important;color:#aaa!important;border-color:#555!important;"
                 )
    )
  })
  
  observeEvent(input$perf_same_track_btn, {
    values$perf_same_track_only <- !isTRUE(values$perf_same_track_only)
  })
  
  # Track pills — grouped by type. Clicking a type header toggles all tracks
  # in that type; clicking an individual track pill toggles just that track.
  # values$perf_track_types stores individual track_names (not types).
  # Uses tags$span + Shiny.setInputValue with nonce to avoid duplicate-observer
  # issues from actionButton inside lapply/observe, and to guarantee every click
  # (including double-clicks on the same pill) is processed as a distinct event.
  output$track_type_pills_ui <- renderUI({
    req(values$analysis_races_available)
    ra    <- values$analysis_races_available
    active_tracks <- values$perf_track_types   # stores track names
    
    type_labels <- c(
      short_track   = "Short Track",
      intermediate  = "Intermediate",
      superspeedway = "Superspeedway",
      road_course   = "Road Course",
      atlanta       = "Atlanta",
      dirt          = "Dirt",
      other         = "Other"
    )
    
    types <- sort(unique(ra$track_type[!is.na(ra$track_type)]))
    
    groups <- lapply(types, function(tt) {
      tracks_in_type <- sort(unique(ra$track_name[ra$track_type == tt & !is.na(ra$track_name)]))
      all_active <- all(tracks_in_type %in% active_tracks)
      type_label <- if (tt %in% names(type_labels)) type_labels[[tt]] else tt
      
      # Type header pill — fires typepill_clicked with nonce
      type_js <- sprintf(
        "Shiny.setInputValue('typepill_clicked', {val: '%s', nonce: Date.now()}, {priority: 'event'})",
        gsub("'", "\\\\'", tt)
      )
      type_pill <- tags$span(
        type_label,
        onClick = type_js,
        style   = paste0(
          "cursor:pointer;display:inline-block;margin:2px;padding:4px 14px;",
          "font-size:12px;font-weight:700;border-radius:20px;user-select:none;",
          if (all_active)
            "background:#FFE500;color:#000;border:2px solid #FFE500;"
          else
            "background:#2a2a2a;color:#888;border:2px solid #555;"
        )
      )
      
      # Individual track pills — fire trackpill_clicked with nonce
      track_pills <- lapply(tracks_in_type, function(tn) {
        is_active <- tn %in% active_tracks
        track_js  <- sprintf(
          "Shiny.setInputValue('trackpill_clicked', {val: '%s', nonce: Date.now()}, {priority: 'event'})",
          gsub("'", "\\\\'", tn)
        )
        tags$span(
          tn,
          onClick = track_js,
          style   = paste0(
            "cursor:pointer;display:inline-block;margin:2px;padding:3px 10px;",
            "font-size:11px;font-weight:500;border-radius:20px;user-select:none;",
            if (is_active)
              "background:rgba(255,229,0,0.2);color:#FFE500;border:1px solid #FFE500;"
            else
              "background:#1e1e1e;color:#666;border:1px solid #3a3a3a;"
          )
        )
      })
      
      div(style = "margin-bottom:8px;",
          div(style = "display:flex;flex-wrap:wrap;gap:3px;align-items:center;",
              type_pill,
              tags$span(style = "color:#444;margin:0 4px;font-size:14px;", "▸"),
              tagList(track_pills)
          )
      )
    })
    
    tagList(groups)
  })
  
  # Single observer: type header pill clicked
  observeEvent(input$typepill_clicked, {
    req(values$analysis_races_available)
    tt <- input$typepill_clicked$val
    tracks_in_type <- unique(values$analysis_races_available$track_name[
      values$analysis_races_available$track_type == tt])
    current <- values$perf_track_types
    if (all(tracks_in_type %in% current)) {
      values$perf_track_types <- setdiff(current, tracks_in_type)
    } else {
      values$perf_track_types <- union(current, tracks_in_type)
    }
  }, ignoreInit = TRUE)
  
  # Single observer: individual track pill clicked
  observeEvent(input$trackpill_clicked, {
    tn      <- input$trackpill_clicked$val
    current <- values$perf_track_types
    if (tn %in% current) {
      values$perf_track_types <- setdiff(current, tn)
    } else {
      values$perf_track_types <- c(current, tn)
    }
  }, ignoreInit = TRUE)
  
  # Season from selector
  output$perf_season_from_ui <- renderUI({
    req(values$analysis_races_available)
    seasons <- sort(unique(values$analysis_races_available$race_season))
    selectInput("perf_season_from", "Perf From Season:",
                choices  = seasons,
                selected = values$perf_season_from,
                width    = "130px")
  })
  
  observeEvent(input$perf_season_from, {
    req(input$perf_season_from)
    values$perf_season_from <- as.integer(input$perf_season_from)
  }, ignoreInit = TRUE)
  
  # Recompute pool_state$perf whenever any bulk control changes
  # Respects perf_manual overrides — those rows are immune
  observe({
    req(values$pool_state, values$analysis_races_available)
    same_only   <- isTRUE(values$perf_same_track_only)
    incl_types  <- values$perf_track_types
    season_from <- values$perf_season_from %||% 2022L
    
    ra <- values$analysis_races_available %>%
      select(race_id, track_type, track_name, race_season, is_same_track)
    
    ps <- values$pool_state %>%
      left_join(ra, by = "race_id") %>%
      mutate(
        auto_perf = if (same_only) {
          is_same_track & race_season >= season_from
        } else {
          track_name %in% incl_types & race_season >= season_from
        },
        perf = if_else(!perf_manual, auto_perf, perf)
      ) %>%
      select(-track_type, -track_name, -race_season, -is_same_track, -auto_perf)
    
    values$pool_state <- ps
  })
  
  # Handle individual card/checkbox toggles from JS (both dom and perf pools)
  observeEvent(input$race_pool_toggle, {
    req(values$pool_state)
    toggle  <- input$race_pool_toggle
    rid     <- as.integer(toggle$race_id)
    pool    <- toggle$pool
    checked <- toggle$checked
    ps      <- values$pool_state
    if (pool == "dom") {
      ps$dom[ps$race_id == rid]        <- checked
      ps$dom_manual[ps$race_id == rid] <- TRUE
    } else {
      ps$perf[ps$race_id == rid]         <- checked
      ps$perf_manual[ps$race_id == rid]  <- TRUE
    }
    values$pool_state <- ps
  })
  
  # ===========================================================================
  # PERFORMANCE POOL BUILDER  (card surface mirroring the Dominator pool)
  # Bulk controls (same-track toggle, track-type pills, season-from, lap band)
  # seed pool_state$perf via the existing recompute observer; per-race card
  # clicks emit race_pool_toggle{pool:"perf"} which sets perf_manual = TRUE.
  # ===========================================================================
  
  # Count badge.
  output$perf_build_badge <- renderUI({
    req(values$pool_state)
    n_in  <- sum(values$pool_state$perf, na.rm = TRUE)
    n_all <- nrow(values$pool_state)
    span(style = "background:#FFE500;color:#000;font-weight:700;font-size:12px;padding:4px 12px;border-radius:20px;",
         sprintf("%d of %d races in pool", n_in, n_all))
  })
  
  # Aggregate strip.
  output$perf_pool_summary <- renderUI({
    req(values$pool_state, values$analysis_races_available)
    ids <- values$pool_state %>% filter(perf == TRUE) %>% pull(race_id)
    ra  <- values$analysis_races_available %>% filter(race_id %in% ids)
    n   <- nrow(ra)
    n_types <- length(unique(ra$track_type[!is.na(ra$track_type)]))
    lv  <- ra$scheduled_laps[!is.na(ra$scheduled_laps) & ra$scheduled_laps > 0]
    lap_rng <- if (length(lv) > 0) sprintf("%d\u2013%d", floor(min(lv)), ceiling(max(lv))) else "\u2014"
    stat_box <- function(label, val) {
      div(style = "flex:1;min-width:120px;background:#1e1e1e;border:1px solid #3a3a3a;border-radius:8px;padding:10px 14px;",
          div(style = "color:#888;font-size:11px;text-transform:uppercase;letter-spacing:.5px;", label),
          div(style = "color:#FFE500;font-size:22px;font-weight:700;line-height:1.1;margin-top:2px;", val))
    }
    div(style = "display:flex;gap:12px;flex-wrap:wrap;align-items:stretch;margin-bottom:12px;",
        stat_box("Races", as.character(n)),
        stat_box("Track Types", as.character(n_types)),
        stat_box("Lap Range", lap_rng))
  })
  
  # Per-race cards: track name / type / laps, click to toggle in/out.
  output$perf_pool_cards <- renderUI({
    req(values$pool_state, values$analysis_races_available)
    ps <- values$pool_state %>% select(race_id, perf)
    ra <- values$analysis_races_available %>%
      left_join(ps, by = "race_id") %>%
      arrange(desc(is_same_track), desc(race_season))
    
    cards <- lapply(seq_len(nrow(ra)), function(i) {
      r        <- ra[i, ]
      rid      <- as.character(r$race_id)
      included <- isTRUE(r$perf)
      new_state <- if (included) "false" else "true"
      toggle_js <- sprintf(
        "Shiny.setInputValue('race_pool_toggle', {pool:'perf', race_id:'%s', checked:%s, nonce:Date.now()}, {priority:'event'})",
        rid, new_state)
      
      laps_txt <- if (is.na(r$scheduled_laps)) "? laps" else sprintf("%d laps", as.integer(r$scheduled_laps))
      type_txt <- if (is.na(r$track_type)) "" else r$track_type
      star     <- if (isTRUE(r$is_same_track)) "\u2605 " else ""
      
      div(
        onClick = toggle_js,
        style = paste0(
          "cursor:pointer;user-select:none;border-radius:10px;padding:10px 14px;margin-bottom:8px;",
          "display:flex;justify-content:space-between;align-items:center;transition:all .12s;",
          if (included) "background:#232300;border:2px solid #FFE500;"
          else          "background:#1a1a1a;border:2px solid #333;opacity:.55;"
        ),
        div(
          div(style = "color:#fff;font-weight:600;font-size:13px;",
              sprintf("%s%s %s", star, r$race_season, r$race_name)),
          div(style = "color:#888;font-size:11px;margin-top:2px;",
              sprintf("%s \u00b7 %s \u00b7 %s", r$track_name, type_txt, laps_txt))
        ),
        div(style = sprintf("font-size:11px;font-weight:700;color:%s;",
                            if (included) "#FFE500" else "#666"),
            if (included) "\u2713 INCLUDED" else "EXCLUDED")
      )
    })
    tagList(cards)
  })
  
  # ---------------------------------------------------------------------------
  # FILTER STATE
  # ---------------------------------------------------------------------------
  output$filters_confirmed <- reactive({ values$filters_confirmed })
  outputOptions(output, "filters_confirmed", suspendWhenHidden = FALSE)
  
  # ---------------------------------------------------------------------------
  # ENTRY LIST: fires when race dropdown changes — separate from pool load
  # This ensures race_id is settled before we call the API
  # ---------------------------------------------------------------------------
  observeEvent(input$analysis_race_id, {
    req(input$analysis_race_id, input$analysis_series, values$races_sheet)
    upcoming_race_id <- as.numeric(input$analysis_race_id)
    if (is.na(upcoming_race_id) || length(upcoming_race_id) == 0) {
      values$analysis_entry_list <- data.frame(
        Start = integer(), Name = character(), Car = integer(),
        Team = character(), CC = character(),
        Make = character(), Sponsor = character())
      return()
    }
    race_info <- values$races_sheet %>%
      filter(race_id == upcoming_race_id) %>%
      slice(1)
    if (nrow(race_info) == 0) {
      values$analysis_entry_list <- data.frame(
        Start = integer(), Name = character(), Car = integer(),
        Team = character(), CC = character(),
        Make = character(), Sponsor = character())
      return()
    }
    withProgress(message = "Loading entry list...", {
      values$analysis_entry_list <- load_entry_list(
        race_info$race_season,
        as.numeric(input$analysis_series),
        upcoming_race_id)
    })
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  # ---------------------------------------------------------------------------
  # POOL REACTIVES — downstream tabs read from these
  # ---------------------------------------------------------------------------
  dominator_filtered_races <- reactive({
    req(values$pool_state)
    # The Dominator Profile builder, when active, is the source of truth for
    # which races feed the sim. Fall back to legacy pool_state$dom otherwise.
    excl <- values$dom_track_exclude
    excl_ids <- integer(0)
    if (!is.null(excl) && length(excl) > 0 &&
        !is.null(values$analysis_races_available)) {
      excl_ids <- values$analysis_races_available %>%
        filter(track_name %in% excl) %>% pull(race_id)
    }
    inc <- values$dom_include
    if (!is.null(inc) && length(inc) > 0) {
      ids <- as.integer(names(inc)[inc])
      ids <- setdiff(ids, excl_ids)
      if (length(ids) > 0) return(ids)
    }
    values$pool_state %>% filter(dom == TRUE) %>%
      pull(race_id) %>% setdiff(excl_ids)
  })
  
  performance_race_ids_reactive <- reactive({
    req(values$pool_state)
    values$pool_state %>% filter(perf == TRUE) %>% pull(race_id)
  })
  
  # ---------------------------------------------------------------------------
  # SALARY AUTO-LOADING
  # ---------------------------------------------------------------------------
  entry_list_with_salaries <- reactive({
    req(values$analysis_entry_list, input$analysis_series)
    entry_list  <- values$analysis_entry_list
    prefix      <- series_salary_prefix(input$analysis_series)
    
    load_salary <- function(platforms, col_name, id_name) {
      for (pat in c(paste0(platforms, prefix, ".csv"),
                    paste0(platforms, "Salaries", prefix, ".csv"),
                    paste0(tolower(platforms), tolower(prefix), ".csv"),
                    paste0(platforms, ".csv"),
                    paste0(tolower(platforms), ".csv"))) {
        if (file.exists(pat)) {
          tryCatch({
            sal <- read_csv(pat, show_col_types = FALSE,
                            locale = locale(encoding = "UTF-8"))
            name_col <- if ("Nickname" %in% names(sal)) "Nickname" else "Name"
            id_col   <- if (platforms == "DK") "ID" else "Id"
            out <- sal %>% select(Name = all_of(name_col),
                                  !!col_name := Salary) %>%
              mutate(Name = safe_trimws(Name))
            if (id_col %in% names(sal))
              out[[id_name]] <- sal[[id_col]]
            return(out)
          }, error = function(e) NULL)
        }
      }
      NULL
    }
    
    dk_sal <- load_salary("DK", "DK_Salary", "DKID")
    fd_sal <- if (is_cup_series(input$analysis_series))
      load_salary("FD", "FD_Salary", "FDID") else NULL
    
    # Fuzzy-match salaries onto the entry list by normalized driver name.
    if (!is.null(dk_sal))
      entry_list <- fuzzy_join_salary(entry_list, dk_sal,
                                      intersect(c("DK_Salary", "DKID"), names(dk_sal)))
    if (!is.null(fd_sal))
      entry_list <- fuzzy_join_salary(entry_list, fd_sal,
                                      intersect(c("FD_Salary", "FDID"), names(fd_sal)))
    
    # Column order: base info, then salaries
    base_cols   <- c("Start", "Name", "Car", "Team", "Make", "CC", "Sponsor")
    base_cols   <- intersect(base_cols, names(entry_list))
    salary_cols <- intersect(c("DK_Salary", "FD_Salary", "DKID", "FDID"), names(entry_list))
    entry_list %>% select(all_of(c(base_cols, salary_cols)))
  })
  
  # ---------------------------------------------------------------------------
  # ENTRY LIST OUTPUTS
  # ---------------------------------------------------------------------------
  output$entry_list_title <- renderUI({
    req(values$races_sheet, input$analysis_race_id, input$analysis_series)
    race_info <- values$races_sheet %>%
      filter(race_id == as.numeric(input$analysis_race_id)) %>%
      slice(1)
    race_name <- if (nrow(race_info) > 0) race_info$race_name else "Entry List"
    prefix    <- series_salary_prefix(input$analysis_series)
    salary_exists <- function(p)
      any(file.exists(c(paste0(p, prefix, ".csv"),
                        paste0(p, "Salaries", prefix, ".csv"),
                        paste0(tolower(p), tolower(prefix), ".csv"),
                        paste0(p, ".csv"),
                        paste0(tolower(p), ".csv"))))
    dk_loaded <- salary_exists("DK")
    fd_loaded <- is_cup_series(input$analysis_series) && salary_exists("FD")
    suffix <- case_when(
      dk_loaded & fd_loaded ~ " — DK + FD Salaries Loaded",
      dk_loaded             ~ " — DK Salaries Loaded",
      fd_loaded             ~ " — FD Salaries Loaded",
      TRUE                  ~ ""
    )
    h3(paste0(race_name, " Entry List", suffix), class = "box-title")
  })
  
  output$entry_list_table <- DT::renderDataTable({
    req(entry_list_with_salaries())
    DT::datatable(entry_list_with_salaries(),
                  rownames = FALSE, class = "display nowrap compact",
                  options  = list(
                    pageLength = 40, scrollX = TRUE, dom = "tip",
                    columnDefs = list(list(className = "dt-center", targets = "_all"))
                  )
    )
  })
  
  output$download_entry_list_csv <- downloadHandler(
    filename = function() paste0("Entry_List_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(entry_list_with_salaries())
      write.csv(entry_list_with_salaries(), file, row.names = FALSE)
    }
  )
  
  output$download_entry_list_excel <- downloadHandler(
    filename    = function() paste0("Starting_Grid_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content     = function(file) {
      req(entry_list_with_salaries())
      el <- entry_list_with_salaries()
      wb <- createWorkbook()
      addWorksheet(wb, "Starting Grid")
      writeData(wb, "Starting Grid", el, rowNames = FALSE)
      addStyle(wb, "Starting Grid",
               createStyle(fontSize = 12, fontColour = "#000000", fgFill = "#FFE500",
                           halign = "center", valign = "center", textDecoration = "bold",
                           border = "TopBottomLeftRight", borderColour = "#000000"),
               rows = 1, cols = 1:ncol(el), gridExpand = TRUE)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---------------------------------------------------------------------------
  # FINISH RATES
  # ---------------------------------------------------------------------------
  output$tier_config_ui <- renderUI({
    req(values$analysis_filtered_data)
    n         <- values$num_tiers
    all_teams <- sort(unique(values$analysis_filtered_data$team_name[
      !is.na(values$analysis_filtered_data$team_name)]))
    tagList(lapply(seq_len(n), function(i) {
      div(class = "tier-box",
          div(class = "tier-label", paste("Tier", i)),
          fluidRow(
            column(5, textInput(paste0("tier_name_", i), NULL,
                                value = paste0("Tier ", i), placeholder = "Tier name")),
            column(7, selectizeInput(paste0("tier_teams_", i), NULL,
                                     choices  = all_teams, multiple = TRUE,
                                     options  = list(placeholder = paste("Assign teams to Tier", i))))
          )
      )
    }))
  })
  
  observe({
    req(values$analysis_filtered_data, values$num_tiers)
    n         <- values$num_tiers
    all_teams <- sort(unique(values$analysis_filtered_data$team_name[
      !is.na(values$analysis_filtered_data$team_name)]))
    selections <- lapply(seq_len(n), function(i) input[[paste0("tier_teams_", i)]])
    for (i in seq_len(n)) {
      other_selected <- unlist(selections[-i])
      updateSelectizeInput(session, paste0("tier_teams_", i),
                           choices  = setdiff(all_teams, other_selected),
                           selected = selections[[i]])
    }
  })
  
  # FR view pill observers
  
  
  observeEvent(input$add_tier,    { values$num_tiers <- min(values$num_tiers + 1, 8) })
  observeEvent(input$remove_tier, { values$num_tiers <- max(values$num_tiers - 1, 1) })
  
  finish_rates_data <- reactive({
    req(values$analysis_filtered_data, performance_race_ids_reactive())
    view_sel    <- values$fr_view_sel    %||% "driver"
    seasons_sel <- values$fr_seasons_sel %||% "all"
    
    # Start from Perf pool only
    data <- values$analysis_filtered_data %>%
      filter(race_id %in% performance_race_ids_reactive())
    
    # Season pills narrow further within the Perf pool
    if (!identical(seasons_sel, "all"))
      data <- data %>% filter(race_season %in% as.integer(seasons_sel))
    
    if (nrow(data) == 0) return(NULL)
    
    has_entry <- !is.null(values$analysis_entry_list) &&
      nrow(values$analysis_entry_list) > 0
    
    if (view_sel == "driver") {
      if (has_entry)
        data <- data %>% filter(Full_Name %in% values$analysis_entry_list$Name)
      calc_finish_rates(data, "Full_Name", "Driver") %>% arrange(`Avg Finish`)
      
    } else if (view_sel == "car") {
      data %>%
        mutate(car_entry = paste0("#", car_number, " (", team_name, ")")) %>%
        calc_finish_rates("car_entry", "Car") %>%
        arrange(`Avg Finish`)
      
    } else if (view_sel == "team") {
      calc_finish_rates(data, "team_name", "Team") %>% arrange(`Avg Finish`)
      
    } else if (view_sel == "start_pos") {
      # Group starting positions in buckets of 5
      data %>%
        filter(!is.na(start_ps)) %>%
        mutate(
          start_group = case_when(
            start_ps <=  5 ~ "P1-5",
            start_ps <= 10 ~ "P6-10",
            start_ps <= 15 ~ "P11-15",
            start_ps <= 20 ~ "P16-20",
            start_ps <= 25 ~ "P21-25",
            start_ps <= 30 ~ "P26-30",
            start_ps <= 35 ~ "P31-35",
            TRUE           ~ "P36+"
          )
        ) %>%
        calc_finish_rates("start_group", "Starting Position") %>%
        arrange(factor(`Starting Position`,
                       levels = c("P1-5","P6-10","P11-15","P16-20",
                                  "P21-25","P26-30","P31-35","P36+")))
      
    } else if (view_sel == "tier") {
      n   <- values$num_tiers
      tier_df <- bind_rows(lapply(seq_len(n), function(i) {
        teams <- input[[paste0("tier_teams_", i)]]
        name  <- input[[paste0("tier_name_",  i)]]
        if (is.null(teams) || length(teams) == 0) return(NULL)
        data.frame(team_name = teams, Tier = name, stringsAsFactors = FALSE)
      }))
      if (is.null(tier_df) || nrow(tier_df) == 0) return(NULL)
      data %>%
        inner_join(tier_df, by = "team_name") %>%
        calc_finish_rates("Tier", "Tier") %>%
        arrange(`Avg Finish`)
    }
  })
  
  output$finish_rates_table <- DT::renderDataTable({
    req(finish_rates_data())
    DT::datatable(finish_rates_data(),
                  rownames = FALSE,
                  filter   = "top",
                  class    = "display nowrap compact",
                  options  = list(
                    pageLength = -1, scrollX = TRUE, dom = "tp",
                    columnDefs = list(list(className = "dt-center", targets = "_all"))
                  )
    )
  })
  
  output$download_finish_rates <- downloadHandler(
    filename = function() paste0("finish_rates_", values$fr_view_sel %||% "driver", "_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(finish_rates_data())
      write.csv(finish_rates_data(), file, row.names = FALSE)
    }
  )
  
  # ---------------------------------------------------------------------------
  # DOMINATOR
  # ---------------------------------------------------------------------------
  dominator_data <- reactive({
    req(values$analysis_filtered_data, dominator_filtered_races())
    data <- values$analysis_filtered_data %>%
      filter(race_id %in% dominator_filtered_races())
    # Fall back to computing from raw laps if DKSP/FDSP missing
    if (!"DKSP" %in% names(data) || all(is.na(data$DKSP))) {
      data <- data %>% group_by(race_id) %>%
        mutate(DKSP = fast_laps * 0.45 + lead_laps * 0.25,
               DKDomRank = dense_rank(desc(DKSP))) %>% ungroup()
    }
    if (!"FDSP" %in% names(data) || all(is.na(data$FDSP))) {
      data <- data %>% group_by(race_id) %>%
        mutate(FDSP = lead_laps * 0.1,
               FDDomRank = dense_rank(desc(FDSP))) %>% ungroup()
    }
    data
  })
  
  # ===========================================================================
  # DOMINATOR PROFILE BUILDER
  # Per-race dominator profiles (DK perspective) for curating the sim comp set.
  # Hard gate: DK dom total within an editable target band. Everything else is
  # read visually from each race's profile strip.
  # ===========================================================================
  
  # All candidate races for the loaded series, each reduced to a profile.
  # Scoped to the Race Selection dom pre-filter (pool_state$dom) so the
  # candidate set matches what was filtered in on the selection page.
  dom_profiles <- reactive({
    req(values$analysis_filtered_data, values$analysis_races_available,
        values$pool_state)
    prefilter_ids <- values$pool_state %>%
      filter(dom == TRUE) %>% pull(race_id)
    # Fallback: if the pre-filter is empty, use the full available set so the
    # tab is never blank.
    if (length(prefilter_ids) == 0)
      prefilter_ids <- values$analysis_races_available$race_id
    
    # Track-exclude pills: drop all races at excluded tracks from the candidate
    # set entirely (their cards disappear).
    excl <- values$dom_track_exclude
    if (!is.null(excl) && length(excl) > 0) {
      keep_ids <- values$analysis_races_available %>%
        filter(!(track_name %in% excl)) %>% pull(race_id)
      prefilter_ids <- intersect(prefilter_ids, keep_ids)
    }
    
    df <- values$analysis_filtered_data %>%
      filter(race_id %in% prefilter_ids)
    if (!"DKSP" %in% names(df) || all(is.na(df$DKSP))) {
      df <- df %>% group_by(race_id) %>%
        mutate(DKSP = fast_laps * 0.45 + lead_laps * 0.25) %>% ungroup()
    }
    ra <- values$analysis_races_available %>%
      filter(race_id %in% prefilter_ids) %>%
      select(race_id, race_name, race_season, track_name, track_type,
             act_laps = total_laps,
             cautions = number_of_cautions)
    
    totals <- df %>%
      group_by(race_id) %>%
      summarise(dk_total  = round(sum(DKSP, na.rm = TRUE), 1),
                n_drivers = n(),
                .groups = "drop")
    
    # Ranked dominators per race: who, how big, start -> finish. Keep drivers
    # up to 90% cumulative dom share (min 1, max 6) so a runaway shows one bar
    # and a committee shows several — the bar count is itself part of the shape.
    doms <- df %>%
      filter(DKSP > 0) %>%
      group_by(race_id) %>%
      arrange(desc(DKSP), .by_group = TRUE) %>%
      mutate(rank = row_number(),
             share = DKSP / sum(DKSP, na.rm = TRUE),
             cum_prev = cumsum(share) - share) %>%
      filter(rank == 1 | (cum_prev < 0.90 & rank <= 6)) %>%
      ungroup() %>%
      transmute(race_id, rank,
                driver = Full_Name,
                dk = round(DKSP, 1),
                start = start_ps, finish = ps,
                lead = lead_laps, fast = fast_laps,
                share = round(share, 3))
    
    list(meta = ra %>% inner_join(totals, by = "race_id"),
         doms = doms)
  })
  
  # Seed target band + include set when a new race/track loads.
  observeEvent(list(values$analysis_races_available, input$analysis_race_id), {
    req(values$analysis_races_available, dom_profiles())
    meta <- dom_profiles()$meta
    if (nrow(meta) == 0) return()
    sel_track <- input$analysis_primary_track
    
    # Upcoming race lap count + track type come from the Races sheet, so they
    # resolve even for a brand-new venue with no results history.
    up <- values$races_sheet %>%
      filter(race_id == suppressWarnings(as.numeric(input$analysis_race_id))) %>%
      slice(1)
    get_col <- function(d, nm) if (nm %in% names(d) && nrow(d) > 0) d[[nm]][1] else NA
    al <- get_col(up, "actual_laps")
    sl <- get_col(up, "scheduled_laps")
    upcoming_laps <- if (!is.na(al) && al > 0) al else sl
    sel_type <- get_col(up, "track_type")
    values$dom_target_laps <- upcoming_laps
    
    pos <- meta %>% filter(!is.na(dk_total) & dk_total > 0)
    same_track <- pos$dk_total[pos$track_name == sel_track]
    
    if (length(same_track) >= 2) {
      # Returning track: anchor to this track's own history
      ref_set <- same_track
    } else {
      # New (or near-new) venue: anchor to races with SIMILAR LAP COUNTS,
      # preferring the same track type. Dom points scale with laps, so a
      # 50-lap road course is a far better comp than a 110-lap one.
      cand <- pos
      if (!is.na(sel_type)) {
        st <- cand %>% filter(!is.na(track_type) & track_type == sel_type)
        if (nrow(st) >= 3) cand <- st   # keep type filter only if it leaves enough
      }
      if (!is.na(upcoming_laps) && any(!is.na(cand$act_laps))) {
        cand <- cand %>%
          mutate(lap_gap = abs(act_laps - upcoming_laps)) %>%
          arrange(lap_gap)
        # take races within 20% of target laps; if too few, take nearest 8
        near <- cand %>% filter(!is.na(lap_gap) &
                                  lap_gap <= 0.20 * upcoming_laps)
        if (nrow(near) < 4) near <- head(cand, 8)
        ref_set <- near$dk_total
      } else {
        ref_set <- cand$dk_total
      }
    }
    ref_set <- ref_set[!is.na(ref_set) & ref_set > 0]
    if (length(ref_set) == 0) ref_set <- pos$dk_total  # last-resort fallback
    ref <- median(ref_set, na.rm = TRUE)
    
    if (!is.finite(ref)) {
      # Absolute fallback so the slider never sees NA
      allv <- pos$dk_total
      ref <- if (length(allv) > 0) median(allv, na.rm = TRUE) else 100
    }
    
    lo <- floor(ref * 0.80); hi <- ceiling(ref * 1.20)
    # Widen to cover the comp set we anchored on
    if (length(ref_set) > 0) {
      lo <- min(lo, floor(min(ref_set) * 0.95))
      hi <- max(hi, ceiling(max(ref_set) * 1.05))
    }
    if (!is.finite(lo)) lo <- 0
    if (!is.finite(hi)) hi <- ceiling(ref * 1.5)
    values$dom_target_lo <- lo
    values$dom_target_hi <- hi
    inc <- setNames(meta$dk_total >= lo & meta$dk_total <= hi,
                    as.character(meta$race_id))
    inc[is.na(inc)] <- FALSE
    values$dom_include <- inc
    values$dom_band_initialized <- TRUE
  }, ignoreInit = FALSE)
  
  # Band slider re-gates the include set. It must NOT rewrite dom_target_lo/hi
  # (the slider UI is seeded from those; rewriting them re-renders the slider,
  # which re-fires this observer — an infinite loop). The live band is read
  # from input$dom_band wherever the current gate is needed.
  observeEvent(input$dom_band, {
    req(input$dom_band, dom_profiles())
    lo <- input$dom_band[1]; hi <- input$dom_band[2]
    meta <- dom_profiles()$meta
    inc <- setNames(meta$dk_total >= lo & meta$dk_total <= hi,
                    as.character(meta$race_id))
    inc[is.na(inc)] <- FALSE
    values$dom_include <- inc
  }, ignoreInit = TRUE)
  
  # Per-race include toggle from a profile card click.
  observeEvent(input$dom_toggle_race, {
    req(input$dom_toggle_race, values$dom_include)
    rid <- as.character(input$dom_toggle_race$val)
    inc <- values$dom_include
    if (rid %in% names(inc)) inc[rid] <- !inc[rid]
    values$dom_include <- inc
  }, ignoreInit = TRUE)
  
  # --- helper: races currently included AND their profile rows -------------
  dom_included_meta <- reactive({
    req(dom_profiles(), values$dom_include)
    inc <- values$dom_include
    ids <- as.integer(names(inc)[inc])
    dom_profiles()$meta %>% filter(race_id %in% ids)
  })
  
  # --- build badge: count in pool -----------------------------------------
  output$dom_build_badge <- renderUI({
    req(values$dom_include)
    n_in  <- sum(values$dom_include)
    n_all <- length(values$dom_include)
    span(style = "background:#FFE500;color:#000;font-weight:700;font-size:12px;padding:4px 12px;border-radius:20px;",
         sprintf("%d of %d races in pool", n_in, n_all))
  })
  
  # --- target panel: laps + dom-total target ------------------------------
  output$dom_target_panel <- renderUI({
    req(dom_profiles())
    meta <- dom_profiles()$meta
    sel_track <- input$analysis_primary_track
    laps <- values$dom_target_laps
    st <- meta$dk_total[meta$track_name == sel_track]
    same_track <- st[!is.na(st) & st > 0]
    is_new <- length(same_track) < 2
    
    # For new venues, count how many comp races sit near the target lap count
    n_lapcomp <- NA_integer_
    if (is_new && !is.null(laps) && !is.na(laps)) {
      lc <- meta %>% filter(!is.na(dk_total) & dk_total > 0 & !is.na(act_laps))
      n_lapcomp <- sum(abs(lc$act_laps - laps) <= 0.20 * laps)
    }
    ref_txt <- if (is_new)
      sprintf("races with similar lap counts (~%s laps), preferring same type — no prior race at this track",
              if (!is.null(laps) && !is.na(laps)) round(laps) else "?")
    else "this track's own history"
    
    stat_box <- function(label, val, accent = "#FFE500") {
      div(style = "flex:1;min-width:120px;background:#1e1e1e;border:1px solid #3a3a3a;border-radius:8px;padding:10px 14px;",
          div(style = "color:#888;font-size:11px;text-transform:uppercase;letter-spacing:.5px;", label),
          div(style = sprintf("color:%s;font-size:22px;font-weight:700;line-height:1.1;margin-top:2px;", accent), val))
    }
    ref_val <- if (is_new) {
      if (!is.na(n_lapcomp) && n_lapcomp > 0)
        sprintf("%d lap-comps", n_lapcomp) else "New venue"
    } else sprintf("%g", round(median(same_track)))
    
    div(style = "display:flex;gap:12px;flex-wrap:wrap;align-items:stretch;",
        stat_box("Target Laps",
                 if (is.null(laps) || is.na(laps)) "—" else as.character(round(laps))),
        stat_box("DK Dom-Total Target",
                 if (is.null(values$dom_target_lo) || is.na(values$dom_target_lo)) "—"
                 else sprintf("%g – %g", values$dom_target_lo, values$dom_target_hi)),
        stat_box("Reference", ref_val,
                 accent = if (is_new) "#ff9800" else "#4caf50"),
        div(style = "flex:2;min-width:200px;display:flex;align-items:center;color:#777;font-size:12px;",
            sprintf("Target seeded from %s. Total dom points is the hard gate — drag the band to tighten.", ref_txt))
    )
  })
  
  # --- band slider --------------------------------------------------------
  output$dom_band_slider_ui <- renderUI({
    req(dom_profiles())
    meta <- dom_profiles()$meta
    tot  <- meta$dk_total
    tot  <- tot[!is.na(tot) & tot > 0]
    if (length(tot) == 0) return(
      div(style = "color:#888;", "No historical dom-point data available for this series yet."))
    rng_lo <- floor(min(tot)); rng_hi <- ceiling(max(tot))
    if (rng_hi <= rng_lo) rng_hi <- rng_lo + 1
    cur_lo <- values$dom_target_lo
    cur_hi <- values$dom_target_hi
    if (is.null(cur_lo) || is.na(cur_lo)) cur_lo <- rng_lo
    if (is.null(cur_hi) || is.na(cur_hi)) cur_hi <- rng_hi
    cur_lo <- max(rng_lo, min(cur_lo, rng_hi))
    cur_hi <- min(rng_hi, max(cur_hi, rng_lo))
    sliderInput("dom_band", "DK Dom-Total acceptance band:",
                min = rng_lo, max = rng_hi,
                value = c(cur_lo, cur_hi),
                step = 1, width = "100%")
  })
  
  # --- track-exclude pills: one per track in the band-included dom set --------
  # Derived from the dom pre-filter (NOT from dom_profiles, which already has
  # the exclude applied) so an excluded track keeps its pill for toggling back.
  output$dom_track_pills_ui <- renderUI({
    req(values$pool_state, values$analysis_races_available)
    dom_ids <- values$pool_state %>% filter(dom == TRUE) %>% pull(race_id)
    if (length(dom_ids) == 0)
      dom_ids <- values$analysis_races_available$race_id
    tracks <- values$analysis_races_available %>%
      filter(race_id %in% dom_ids, !is.na(track_name)) %>%
      pull(track_name) %>% unique() %>% sort()
    if (length(tracks) == 0)
      return(div(style = "color:#888;font-size:12px;", "No tracks in pool."))
    excl <- values$dom_track_exclude %||% character(0)
    
    pills <- lapply(tracks, function(tn) {
      is_in <- !(tn %in% excl)
      js <- sprintf(
        "Shiny.setInputValue('dom_track_pill_clicked', {val: '%s', nonce: Date.now()}, {priority: 'event'})",
        gsub("'", "\\\\'", tn))
      tags$span(
        tn, onClick = js,
        style = paste0(
          "cursor:pointer;display:inline-block;margin:2px;padding:3px 10px;",
          "font-size:11px;font-weight:500;border-radius:20px;user-select:none;",
          if (is_in)
            "background:rgba(255,229,0,0.2);color:#FFE500;border:1px solid #FFE500;"
          else
            "background:#1e1e1e;color:#666;border:1px solid #3a3a3a;"
        )
      )
    })
    div(style = "display:flex;flex-wrap:wrap;gap:3px;align-items:center;", tagList(pills))
  })
  
  # Single observer: dom track pill toggled on/off.
  observeEvent(input$dom_track_pill_clicked, {
    tn      <- input$dom_track_pill_clicked$val
    current <- values$dom_track_exclude %||% character(0)
    if (tn %in% current) {
      values$dom_track_exclude <- setdiff(current, tn)
    } else {
      values$dom_track_exclude <- c(current, tn)
    }
  }, ignoreInit = TRUE)
  
  # --- profile cards: one strip per race, sorted by proximity to target ----
  output$dom_profile_cards <- renderUI({
    req(dom_profiles(), values$dom_include)
    meta <- dom_profiles()$meta
    doms <- dom_profiles()$doms
    inc  <- values$dom_include
    # Live band from the slider, falling back to the seeded target.
    band <- input$dom_band
    if (!is.null(band) && length(band) == 2) {
      lo <- band[1]; hi <- band[2]
    } else {
      lo <- values$dom_target_lo; hi <- values$dom_target_hi
    }
    target_mid <- if (!is.null(lo) && !is.na(lo)) (lo + hi) / 2
    else median(meta$dk_total, na.rm = TRUE)
    target_laps <- values$dom_target_laps
    
    # global max dom for bar scaling
    max_dk <- max(doms$dk, na.rm = TRUE)
    if (!is.finite(max_dk) || max_dk <= 0) max_dk <- 1
    
    meta <- meta %>%
      mutate(in_band = !is.na(dk_total) &
               dk_total >= (lo %||% -Inf) & dk_total <= (hi %||% Inf),
             dist = ifelse(is.na(dk_total), Inf, abs(dk_total - target_mid)),
             lap_gap = if (!is.null(target_laps) && !is.na(target_laps))
               abs(act_laps - target_laps) else NA_real_) %>%
      arrange(dist)
    
    cards <- lapply(seq_len(nrow(meta)), function(i) {
      r   <- meta[i, ]
      rid <- as.character(r$race_id)
      included <- isTRUE(rid %in% names(inc) && inc[[rid]])
      d <- doms %>% filter(race_id == r$race_id) %>% arrange(rank)
      
      # dominator bars
      bars <- lapply(seq_len(nrow(d)), function(j) {
        b <- d[j, ]
        w <- max(3, round(100 * b$dk / max_dk))
        div(style = "display:flex;align-items:center;gap:8px;margin:2px 0;",
            div(style = "width:120px;color:#ccc;font-size:11px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                b$driver),
            div(style = "flex:1;background:#161616;border-radius:3px;overflow:hidden;",
                div(style = sprintf("width:%d%%;background:linear-gradient(90deg,#FFE500,#D4B000);height:14px;border-radius:3px;", w))),
            div(style = "width:54px;text-align:right;color:#FFE500;font-size:11px;font-weight:600;",
                sprintf("%.1f", b$dk)),
            div(style = "width:70px;text-align:right;color:#888;font-size:11px;",
                sprintf("P%s\u2192P%s", b$start, b$finish))
        )
      })
      
      band_tag <- if (r$in_band)
        span(style = "color:#4caf50;font-size:10px;font-weight:700;", "IN BAND")
      else
        span(style = "color:#ff5252;font-size:10px;font-weight:700;", "OUT OF BAND")
      
      toggle_js <- sprintf(
        "Shiny.setInputValue('dom_toggle_race', {val: '%s', nonce: Date.now()}, {priority:'event'})",
        rid)
      
      div(
        onClick = toggle_js,
        style = paste0(
          "cursor:pointer;user-select:none;border-radius:10px;padding:12px 14px;margin-bottom:10px;",
          "transition:all .12s;",
          if (included)
            "background:#232300;border:2px solid #FFE500;"
          else
            "background:#1a1a1a;border:2px solid #333;opacity:.55;"
        ),
        div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;",
            div(
              div(style = "color:#fff;font-weight:600;font-size:13px;",
                  sprintf("%s %s", r$race_season, r$race_name)),
              div(style = "color:#777;font-size:11px;",
                  sprintf("%s \u00b7 %s laps \u00b7 %s cautions",
                          r$track_name,
                          ifelse(is.na(r$act_laps), "?", as.character(round(r$act_laps))),
                          ifelse(is.na(r$cautions), "?", as.character(r$cautions)))),
              if (!is.na(r$lap_gap) && !is.null(target_laps) && !is.na(target_laps)) {
                pct <- r$lap_gap / target_laps
                col <- if (pct <= 0.10) "#4caf50" else if (pct <= 0.25) "#ff9800" else "#ff5252"
                div(style = sprintf("display:inline-block;margin-top:3px;font-size:10px;font-weight:700;color:%s;border:1px solid %s;border-radius:10px;padding:1px 7px;", col, col),
                    sprintf("lap match: %+d", as.integer(round(r$act_laps - target_laps))))
              }
            ),
            div(style = "text-align:right;",
                div(style = "color:#FFE500;font-size:20px;font-weight:800;line-height:1;",
                    sprintf("%.0f", r$dk_total)),
                div(style = "color:#888;font-size:10px;", "DK dom total"),
                band_tag,
                div(style = sprintf("margin-top:3px;font-size:10px;font-weight:700;color:%s;",
                                    if (included) "#FFE500" else "#666"),
                    if (included) "\u2713 INCLUDED" else "EXCLUDED")
            )
        ),
        tagList(bars)
      )
    })
    
    # two-column responsive grid
    div(style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(420px,1fr));gap:12px;",
        tagList(cards))
  })
  
  # --- mix summary: aggregate of included pool ----------------------------
  output$dom_mix_summary <- renderUI({
    im <- dom_included_meta()
    if (is.null(im) || nrow(im) == 0)
      return(div(style = "color:#888;padding:8px;", "No races included — widen the band or toggle races below."))
    tot <- im$dk_total
    tot <- tot[!is.na(tot)]
    laps <- im$act_laps[!is.na(im$act_laps)]
    target_laps <- values$dom_target_laps
    if (length(tot) == 0)
      return(div(style = "color:#888;padding:8px;", "Included races have no dom-point data."))
    
    stat <- function(label, val, accent = "#FFE500") {
      div(style = "flex:1;min-width:110px;background:#1e1e1e;border:1px solid #3a3a3a;border-radius:8px;padding:8px 12px;",
          div(style = "color:#888;font-size:10px;text-transform:uppercase;", label),
          div(style = sprintf("color:%s;font-size:18px;font-weight:700;", accent), val))
    }
    lap_flag <- if (length(laps) > 0 && !is.null(target_laps) && !is.na(target_laps)) {
      d <- abs(median(laps) - target_laps) / target_laps
      if (d <= 0.10) "#4caf50" else if (d <= 0.25) "#ff9800" else "#ff5252"
    } else "#FFE500"
    
    div(style = "display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px;",
        stat("Races", nrow(im)),
        stat("Mean DK Dom Total", sprintf("%.0f", mean(tot))),
        stat("Range", sprintf("%.0f\u2013%.0f", min(tot), max(tot))),
        stat("Ceiling (P90)", sprintf("%.0f", as.numeric(quantile(tot, 0.90)))),
        stat("Median Laps",
             if (length(laps) > 0) sprintf("%.0f", median(laps)) else "—",
             accent = lap_flag),
        stat("Equal Weight", sprintf("%.3f", 1 / nrow(im)))
    )
  })
  
  # --- mix plot: dom-total distribution (pure HTML/CSS, no plotly) --------
  output$dom_mix_plot <- renderUI({
    im <- dom_included_meta()
    if (is.null(im) || nrow(im) == 0)
      return(div(style = "color:#888;padding:20px;text-align:center;", "No races included."))
    df <- im[!is.na(im$dk_total), , drop = FALSE]
    if (nrow(df) == 0)
      return(div(style = "color:#888;padding:20px;text-align:center;", "Included races have no dom-point data."))
    df <- df[order(df$dk_total), , drop = FALSE]
    
    labs  <- paste0(df$race_season, " ", substr(df$race_name, 1, 30))
    xvals <- as.numeric(df$dk_total)
    laps  <- df$act_laps
    xmax  <- max(xvals, na.rm = TRUE); if (!is.finite(xmax) || xmax <= 0) xmax <- 1
    
    band <- input$dom_band
    if (!is.null(band) && length(band) == 2) {
      lo <- band[1]; hi <- band[2]
    } else {
      lo <- values$dom_target_lo; hi <- values$dom_target_hi
    }
    have_band <- !is.null(lo) && !is.na(lo)
    
    rows <- lapply(seq_along(labs), function(i) {
      w <- max(2, round(100 * xvals[i] / xmax))
      in_band <- have_band && xvals[i] >= lo && xvals[i] <= hi
      bar_bg <- if (in_band)
        "linear-gradient(90deg,#FFE500,#D4B000)" else "linear-gradient(90deg,#6b6b00,#4a4a00)"
      div(style = "display:flex;align-items:center;gap:10px;margin:3px 0;",
          div(style = "width:180px;color:#bbb;font-size:11px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
              labs[i]),
          div(style = "flex:1;position:relative;background:#161616;border-radius:3px;height:20px;",
              div(style = sprintf("position:absolute;left:0;top:0;height:20px;width:%d%%;background:%s;border-radius:3px;", w, bar_bg)),
              div(style = "position:absolute;left:8px;top:2px;color:#000;font-size:11px;font-weight:700;line-height:16px;",
                  sprintf("%.0f", xvals[i]))),
          div(style = "width:62px;text-align:right;color:#777;font-size:10px;",
              if (!is.na(laps[i])) sprintf("%d laps", as.integer(round(laps[i]))) else "")
      )
    })
    
    hdr <- if (have_band)
      div(style = "color:#777;font-size:11px;margin-bottom:8px;",
          sprintf("DK dom-total band: %g \u2013 %g. ", lo, hi),
          span(style = "color:#FFE500;", "Gold"), " = in band, ",
          span(style = "color:#6b6b00;", "dim"), " = outside.")
    else NULL
    
    div(style = "padding:6px 4px;", hdr, div(tagList(rows)))
  })
  
  
  output$dominator_data_table <- DT::renderDataTable({
    req(dominator_data())
    cup <- is_cup_series(input$analysis_series)
    tbl <- dominator_data() %>%
      filter(DKSP > 0 | FDSP > 0) %>%
      transmute(
        Driver       = Full_Name,
        Start        = start_ps,
        Finish       = ps,
        Team         = team_name,
        `Laps Led`   = lead_laps,
        `Fast Laps`  = fast_laps,
        `DK Dom Pts` = round(DKSP, 1),
        `DK Dom Rank`= DKDomRank,
        `FD Dom Pts` = round(FDSP, 1),
        `FD Dom Rank`= FDDomRank,
        Season       = race_season,
        Race         = race_name,
        Track        = track_name
      ) %>%
      arrange(desc(`DK Dom Pts`))
    if (!cup) tbl <- tbl %>% select(-`FD Dom Pts`, -`FD Dom Rank`)
    tbl %>%
      DT::datatable(
        rownames = FALSE, filter = "top",
        class    = "display nowrap compact",
        options  = list(
          pageLength = 25, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      )
  })
  
  output$download_dominator_csv <- downloadHandler(
    filename = function() paste0("dominator_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(dominator_data())
      dominator_data() %>%
        filter(DKSP > 0 | FDSP > 0) %>%
        transmute(Driver = Full_Name, Start = start_ps, Finish = ps,
                  Team = team_name, `Laps Led` = lead_laps,
                  `Fast Laps` = fast_laps,
                  `DK Dom Pts` = round(DKSP, 1), `DK Dom Rank` = DKDomRank,
                  `FD Dom Pts` = round(FDSP, 1), `FD Dom Rank` = FDDomRank,
                  Season = race_season, Race = race_name, Track = track_name) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  output$download_dominator_profile <- downloadHandler(
    filename    = function() paste0("dominator_profile_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content     = function(file) {
      req(dominator_data(), values$analysis_races_available)
      cup <- is_cup_series(input$analysis_series)
      n_races <- length(dominator_filtered_races())
      race_weights <- values$analysis_races_available %>%
        filter(race_id %in% dominator_filtered_races()) %>%
        transmute(RaceID = race_id, RaceName = race_name,
                  Season = race_season, Track = track_name,
                  Weight = round(1 / n_races, 4)) %>%
        arrange(Season, Track)
      race_profiles <- dominator_data() %>%
        filter(DKSP > 0) %>%
        transmute(RaceID = race_id, StartPos = start_ps, FinPos = ps,
                  LeadLaps = lead_laps, FastLaps = fast_laps,
                  DKDomPoints = round(DKSP, 1), FDDomPoints = round(FDSP, 1),
                  TrackName = track_name, Driver = Full_Name, Team = team_name) %>%
        arrange(desc(DKDomPoints))
      if (!cup) race_profiles <- race_profiles %>% select(-FDDomPoints)
      
      sheets <- list(Race_Weights = race_weights, Race_Profiles = race_profiles)
      if (cup) {
        sheets$FDLaps <- dominator_data() %>%
          filter(!is.na(ps), !is.na(FDLP)) %>%
          group_by(ps) %>%
          summarise(Pt = round(mean(FDLP, na.rm = TRUE), 1), .groups = "drop") %>%
          arrange(ps) %>% rename(PS = ps)
      }
      
      wb <- createWorkbook()
      gold_hdr <- createStyle(fontSize = 11, fontColour = "#000000",
                              fgFill = "#FFE500", halign = "center", valign = "center",
                              textDecoration = "bold", border = "TopBottomLeftRight",
                              borderColour = "#000000")
      for (sname in names(sheets)) {
        df <- sheets[[sname]]
        addWorksheet(wb, sname)
        writeData(wb, sname, df)
        addStyle(wb, sname, gold_hdr, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
        setColWidths(wb, sname, cols = 1:ncol(df), widths = "auto")
      }
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_input_file <- downloadHandler(
    filename    = function() paste0("NASCAR_Sim_Input_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content     = function(file) {
      req(dominator_data(), values$analysis_races_available,
          values$analysis_entry_list)
      el <- entry_list_with_salaries()
      # Filter to drivers with at least one salary
      sal_cols <- intersect(c("DK_Salary", "FD_Salary"), names(el))
      if (length(sal_cols) > 0)
        el <- el %>% filter(if_any(all_of(sal_cols), ~!is.na(.)))
      
      driver_sheet <- el
      if (!"DKID"      %in% names(driver_sheet)) driver_sheet$DKID      <- NA_character_
      if (!"DK_Salary" %in% names(driver_sheet)) driver_sheet$DK_Salary <- NA_real_
      
      # Match the exact column naming used on the existing input sheet:
      # lowercase car/team, "Starting" (not Start), DK name as "Name (DKID)".
      driver_sheet <- driver_sheet %>%
        mutate(
          DKName   = ifelse(is.na(DKID) | DKID == "", Name, paste0(Name, " (", DKID, ")")),
          DKSalary = DK_Salary,
          Starting = Start,
          car      = Car,
          team     = Team,
          DKOP     = NA_real_,
          # Driver-input columns (#2) — placeholders to be filled / auto-computed
          W = NA_real_, T3 = NA_real_, T5 = NA_real_, T10 = NA_real_,
          T15 = NA_real_, T20 = NA_real_, T25 = NA_real_, T30 = NA_real_,
          DKMax = NA_real_
        )
      
      cup <- is_cup_series(input$analysis_series)
      if (cup) {
        # FanDuel columns only relevant for Cup slates.
        # FDName format is "FDID:Name"; FD ids are the FanDuel compound id.
        if (!"FDID"      %in% names(driver_sheet)) driver_sheet$FDID      <- NA_character_
        if (!"FD_Salary" %in% names(driver_sheet)) driver_sheet$FD_Salary <- NA_real_
        driver_sheet <- driver_sheet %>%
          mutate(
            FDName   = ifelse(is.na(FDID) | FDID == "", Name, paste0(FDID, ":", Name)),
            FDSalary = FD_Salary,
            FDOP     = NA_real_,
            FDMax    = NA_real_
          ) %>%
          select(FDName, DKName, Name, DKID, FDID, car, team,
                 DKSalary, FDSalary, Starting, DKOP, FDOP,
                 W, T3, T5, T10, T15, T20, T25, T30, DKMax, FDMax)
      } else {
        driver_sheet <- driver_sheet %>%
          select(DKName, DKID, Name, car, team, DKSalary, Starting, DKOP,
                 W, T3, T5, T10, T15, T20, T25, T30, DKMax)
      }
      
      n_races      <- length(dominator_filtered_races())
      race_profiles <- dominator_data() %>%
        filter(DKSP > 0) %>%
        transmute(RaceID = race_id, StartPos = start_ps, FinPos = ps,
                  LeadLaps = lead_laps, FastLaps = fast_laps,
                  DKDomPoints = round(DKSP, 2), FDDomPoints = round(FDSP, 1),
                  TrackName = track_name, Driver = Full_Name, Team = team_name) %>%
        arrange(desc(DKDomPoints))
      if (!cup) race_profiles <- race_profiles %>% select(-FDDomPoints)
      race_weights <- values$analysis_races_available %>%
        filter(race_id %in% dominator_filtered_races()) %>%
        transmute(RaceID = race_id, RaceName = race_name,
                  Season = race_season, Track = track_name,
                  Weight = round(1 / n_races, 4)) %>%
        arrange(Season, Track)
      
      wb      <- createWorkbook()
      gold_hdr <- createStyle(fontSize = 11, fontColour = "#000000",
                              fgFill = "#FFE500", halign = "center", valign = "center",
                              textDecoration = "bold", border = "TopBottomLeftRight",
                              borderColour = "#000000")
      
      # Sheet order matches the existing input files.
      # Cup:   Driver, FDLaps, Race_Weights, Race_Profiles
      # Other: Driver, Race_Weights, Race_Profiles
      sheets <- list(Driver = driver_sheet)
      if (cup) {
        sheets$FDLaps <- dominator_data() %>%
          filter(!is.na(ps), !is.na(FDLP)) %>%
          group_by(ps) %>%
          summarise(Pt = round(mean(FDLP, na.rm = TRUE), 1), .groups = "drop") %>%
          arrange(ps)
      }
      sheets$Race_Weights  <- race_weights
      sheets$Race_Profiles <- race_profiles
      
      for (sname in names(sheets)) {
        df <- sheets[[sname]]
        addWorksheet(wb, sname)
        writeData(wb, sname, df)
        addStyle(wb, sname, gold_hdr, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
        setColWidths(wb, sname, cols = 1:ncol(df), widths = "auto")
      }
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---------------------------------------------------------------------------
  # DOMINATOR VISUALIZATIONS
  # ---------------------------------------------------------------------------
  # Shared color grouping helpers for all charts
  group_palette <- c(
    "#FFE500", "#4caf50", "#2196f3", "#f44336",
    "#ff9800", "#9c27b0", "#00bcd4", "#e91e63",
    "#8bc34a", "#ff5722", "#607d8b", "#795548"
  )
  
  # Helper: apply color grouping to a plotly horizontal box chart
  # data must have a Grp factor column and the x value column
  # color_by: "none" | "track_type" | "race_season" | "track_name"
  make_colored_box <- function(data, x_col, title_txt, x_label,
                               color_by = "none", margin_l = 150,
                               grp_order = NULL) {
    if (is.null(grp_order))
      grp_order <- levels(data$Grp)
    
    if (color_by == "none") {
      p <- plot_ly(data = data, type = "box",
                   y = ~Grp, x = ~get(x_col), orientation = "h",
                   marker    = list(color = "#FFE500", opacity = 0.6),
                   line      = list(color = "#D4B000"),
                   fillcolor = "rgba(255,229,0,0.25)",
                   showlegend = FALSE)
    } else {
      grp_vals  <- sort(unique(as.character(data[[color_by]])))
      color_map <- setNames(
        group_palette[seq_along(grp_vals) %% length(group_palette) + 1],
        grp_vals)
      traces <- lapply(grp_vals, function(gv) {
        sub <- data %>% filter(as.character(.data[[color_by]]) == gv)
        hex <- color_map[[gv]]
        # convert hex to rgba for fill
        r <- strtoi(substr(hex,2,3),16)
        g <- strtoi(substr(hex,4,5),16)
        b <- strtoi(substr(hex,6,7),16)
        fill_rgba <- sprintf("rgba(%d,%d,%d,0.25)", r, g, b)
        plot_ly(data = sub, type = "box",
                y = ~Grp, x = ~get(x_col), orientation = "h",
                name      = gv,
                marker    = list(color = hex, opacity = 0.7),
                line      = list(color = hex),
                fillcolor = fill_rgba)
      })
      p <- do.call(subplot, c(traces, list(shareX = TRUE, shareY = TRUE, nrows = 1)))
      # rebuild as overlay — subplot doesn't give us what we want; use add_trace
      p <- plot_ly()
      for (gv in grp_vals) {
        sub <- data %>% filter(as.character(.data[[color_by]]) == gv)
        hex <- color_map[[gv]]
        r <- strtoi(substr(hex,2,3),16); g <- strtoi(substr(hex,4,5),16); b <- strtoi(substr(hex,6,7),16)
        fill_rgba <- sprintf("rgba(%d,%d,%d,0.25)", r, g, b)
        p <- p %>% add_trace(data = sub, type = "box",
                             y = ~Grp, x = ~get(x_col), orientation = "h",
                             name      = gv,
                             marker    = list(color = hex, opacity = 0.7),
                             line      = list(color = hex),
                             fillcolor = fill_rgba)
      }
    }
    
    col_label <- switch(color_by,
                        track_type   = "Track Type",
                        race_season  = "Season",
                        track_name   = "Track",
                        NULL)
    
    p %>% layout(
      title       = list(text = title_txt, font = list(size = 18, color = "#FFE500")),
      boxmode     = "overlay",
      xaxis       = list(title = x_label, color = "#ffffff", gridcolor = "#404040",
                         zerolinecolor = "#555555"),
      yaxis       = list(title = "", color = "#ffffff",
                         categoryorder = "array", categoryarray = rev(grp_order)),
      legend      = list(title = list(text = col_label),
                         font = list(color = "#ffffff"),
                         bgcolor = "rgba(0,0,0,0)"),
      paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e",
      font        = list(color = "#ffffff"),
      height      = 900,
      margin      = list(l = margin_l, r = 20, t = 50, b = 40))
  }
  
  output$dominator_plot <- renderPlotly({
    req(dominator_data(), input$dom_visual_type, input$dom_platform)
    plot_data     <- dominator_data()
    platform      <- input$dom_platform
    dom_pts_col   <- if (platform == "DK") "DKSP"      else "FDSP"
    dom_rank_col  <- if (platform == "DK") "DKDomRank" else "FDDomRank"
    platform_name <- if (platform == "DK") "DraftKings" else "FanDuel"
    
    dark_theme <- theme_minimal() + theme(
      plot.title       = element_text(size = 18, face = "bold", color = "#FFE500"),
      axis.title       = element_text(size = 16, color = "#ffffff"),
      axis.text        = element_text(size = 14, color = "#ffffff"),
      panel.background = element_rect(fill = "#1e1e1e"),
      plot.background  = element_rect(fill = "#1e1e1e"),
      panel.grid.major = element_line(color = "#404040"),
      panel.grid.minor = element_line(color = "#333333"))
    
    dark_layout <- function(p) {
      p %>% layout(paper_bgcolor = "#2d2d2d", plot_bgcolor = "#2d2d2d",
                   font   = list(color = "#ffffff"),
                   xaxis  = list(gridcolor = "#404040", zerolinecolor = "#666666"),
                   yaxis  = list(gridcolor = "#404040", zerolinecolor = "#666666"))
    }
    
    vt <- input$dom_visual_type
    
    if (vt == "score_dist") {
      viz <- plot_data %>% filter(!!sym(dom_rank_col) <= 10, !!sym(dom_pts_col) > 0)
      p <- ggplot(viz, aes(x = factor(!!sym(dom_rank_col)), y = !!sym(dom_pts_col))) +
        geom_boxplot(aes(text = sprintf("Dom Rank: %d\nDom Pts: %.1f\nDriver: %s\nTrack: %s",
                                        !!sym(dom_rank_col), !!sym(dom_pts_col), Full_Name, track_name)),
                     fill = "#FFE500", alpha = 0.7) +
        labs(title = paste(platform_name, "Dom Points by Dom Rank (Top 10)"),
             x = "Dom Rank", y = "Dom Points") +
        coord_flip() + scale_x_discrete(limits = factor(10:1)) + dark_theme
      ggplotly(p, tooltip = "text", height = 700) %>% dark_layout()
      
    } else if (vt == "rank_finish") {
      viz <- plot_data %>% filter(!!sym(dom_rank_col) <= 10, !!sym(dom_pts_col) > 0)
      p <- ggplot(viz, aes(x = factor(!!sym(dom_rank_col)), y = ps)) +
        geom_boxplot(aes(text = sprintf("Dom Rank: %d\nFinish: %d\nDriver: %s\nTrack: %s",
                                        !!sym(dom_rank_col), ps, Full_Name, track_name)),
                     fill = "#FFE500", alpha = 0.7) +
        geom_smooth(aes(x = as.numeric(!!sym(dom_rank_col)), y = ps, group = 1),
                    method = "loess", se = FALSE, color = "#FFE500", linewidth = 1.5) +
        labs(title = paste("Where Have Top", platform_name, "Dominators Finished"),
             x = "Dom Rank", y = "Finish Position") +
        scale_x_discrete(limits = factor(1:10)) +
        scale_y_continuous(breaks = seq(0, 40, 5)) + dark_theme
      ggplotly(p, tooltip = "text", height = 700) %>% dark_layout()
      
    } else if (vt %in% c("pts_by_finish", "dom_pts_start", "dom_rank_start",
                         "laps_led", "laps_led_start", "fast_laps", "fast_laps_start")) {
      cfg <- list(
        pts_by_finish   = list(x = "ps",       y = dom_pts_col,  fill = "#FFE500", xt = "Finish Position",   yt = "Dom Points",  ti = paste(platform_name, "Dom Pts by Finish")),
        dom_pts_start   = list(x = "start_ps", y = dom_pts_col,  fill = "#FFE500", xt = "Starting Position", yt = "Dom Points",  ti = paste(platform_name, "Dom Pts by Start")),
        dom_rank_start  = list(x = "start_ps", y = dom_rank_col, fill = "#FFE500", xt = "Starting Position", yt = "Dom Rank",    ti = paste(platform_name, "Dom Rank by Start")),
        laps_led        = list(x = "ps",       y = "lead_laps",  fill = "#DAA520", xt = "Finish Position",   yt = "Laps Led",    ti = "Laps Led by Finish"),
        laps_led_start  = list(x = "start_ps", y = "lead_laps",  fill = "#DAA520", xt = "Starting Position", yt = "Laps Led",    ti = "Laps Led by Start"),
        fast_laps       = list(x = "ps",       y = "fast_laps",  fill = "#FFE500", xt = "Finish Position",   yt = "Fast Laps",   ti = "Fast Laps by Finish"),
        fast_laps_start = list(x = "start_ps", y = "fast_laps",  fill = "#FFE500", xt = "Starting Position", yt = "Fast Laps",   ti = "Fast Laps by Start")
      )
      cc  <- cfg[[vt]]
      viz <- plot_data %>%
        filter(!!sym(cc$x) <= 40, !is.na(!!sym(cc$x)), !is.na(!!sym(cc$y)))
      p <- ggplot(viz, aes(x = factor(!!sym(cc$x)), y = !!sym(cc$y))) +
        geom_boxplot(fill = cc$fill, alpha = 0.7) +
        labs(title = cc$ti, x = cc$xt, y = cc$yt) +
        coord_flip() + scale_x_discrete(limits = factor(40:1)) + dark_theme
      ggplotly(p, height = 900) %>% dark_layout()
      
    } else if (vt %in% c("driver_boxplot", "team_boxplot")) {
      req(values$analysis_entry_list)
      entry_drivers <- values$analysis_entry_list$Name
      entry_teams   <- unique(values$analysis_entry_list$Team)
      
      color_by_dom <- "none"
      
      make_dom_box <- function(data, grp_col, title_txt, margin_l = 150) {
        grp_order <- data %>%
          filter(!is.na(!!sym(dom_pts_col))) %>%
          group_by(!!sym(grp_col)) %>%
          summarise(med = median(!!sym(dom_pts_col), na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(med)) %>% pull(!!sym(grp_col))
        data %>%
          filter(!!sym(grp_col) %in% grp_order, !is.na(!!sym(dom_pts_col))) %>%
          mutate(Grp = factor(!!sym(grp_col), levels = grp_order)) %>%
          make_colored_box(dom_pts_col, title_txt, "Dom Points",
                           color_by = color_by_dom, margin_l = margin_l,
                           grp_order = grp_order)
      }
      
      if (vt == "driver_boxplot") {
        make_dom_box(plot_data %>% filter(Full_Name %in% entry_drivers),
                     "Full_Name", paste(platform_name, "Dom Points by Driver"))
      } else {
        make_dom_box(plot_data %>% filter(team_name %in% entry_teams),
                     "team_name", paste("Team", platform_name, "Dom Points"), margin_l = 180)
      }
    }
  })
  
  # ---------------------------------------------------------------------------
  # PLACE DIFFERENTIAL
  # ---------------------------------------------------------------------------
  pd_data <- reactive({
    req(values$analysis_filtered_data, performance_race_ids_reactive())
    values$analysis_filtered_data %>%
      filter(race_id %in% performance_race_ids_reactive(),
             !is.na(start_ps), !is.na(ps)) %>%
      mutate(PD = start_ps - ps)
  })
  
  output$pd_data_table <- DT::renderDataTable({
    req(pd_data())
    pd_data() %>%
      transmute(Driver = Full_Name, Start = start_ps, Finish = ps, PD,
                Team = team_name, ARP = round(ARP, 1),
                Season = race_season, Race = race_name, Track = track_name) %>%
      arrange(desc(PD)) %>%
      DT::datatable(
        rownames = FALSE, filter = "top",
        class    = "display nowrap compact",
        options  = list(
          pageLength = 25, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      )
  })
  
  output$download_pd_csv <- downloadHandler(
    filename = function() paste0("place_differential_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(pd_data())
      pd_data() %>%
        transmute(Driver = Full_Name, Start = start_ps, Finish = ps, PD,
                  Team = team_name, ARP = round(ARP, 1),
                  Season = race_season, Race = race_name, Track = track_name) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  output$pd_plot <- renderPlotly({
    req(pd_data(), input$pd_visual_type)
    plot_data    <- pd_data()
    color_by_pd  <- "none"
    dark_theme <- theme_minimal() + theme(
      plot.title       = element_text(size = 20, face = "bold", color = "#FFE500"),
      axis.title       = element_text(size = 16, color = "#ffffff"),
      axis.text        = element_text(size = 14, color = "#ffffff"),
      panel.background = element_rect(fill = "#1e1e1e"),
      plot.background  = element_rect(fill = "#1e1e1e"),
      panel.grid.major = element_line(color = "#404040"),
      panel.grid.minor = element_line(color = "#333333"))
    dark_layout <- function(p) p %>% layout(
      paper_bgcolor = "#2d2d2d", plot_bgcolor = "#2d2d2d",
      font  = list(color = "#ffffff"),
      xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
      yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"))
    
    if (input$pd_visual_type == "scatter") {
      viz <- plot_data %>% filter(start_ps <= 40, ps <= 40)
      p <- ggplot(viz, aes(x = start_ps, y = ps, size = abs(PD), color = PD,
                           text = sprintf("Driver: %s\nStart: %d\nFinish: %d\nPD: %d\nTrack: %s",
                                          Full_Name, start_ps, ps, PD, track_name))) +
        geom_point(alpha = 0.6) +
        geom_abline(linetype = "dashed", color = "#FFE500", linewidth = 1.2) +
        scale_color_gradient2(low = "#DC143C", mid = "#cccccc", high = "#FFE500",
                              midpoint = 0, name = "PD") +
        scale_size_continuous(range = c(2, 12)) +
        scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
        scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
        labs(title = "Starting vs Finishing Position",
             x = "Starting Position", y = "Finishing Position") + dark_theme
      ggplotly(p, tooltip = "text", height = 700) %>% dark_layout()
      
    } else if (input$pd_visual_type == "histogram") {
      p <- ggplot(plot_data, aes(x = PD)) +
        geom_histogram(binwidth = 1, fill = "#FFE500", color = "#000000", alpha = 0.8) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "#FF0000", linewidth = 1.5) +
        labs(title = "Position Change Distribution",
             x = "Place Differential", y = "Count") + dark_theme
      ggplotly(p, height = 700) %>% dark_layout()
      
    } else {
      x_col   <- if (input$pd_visual_type == "boxplot_start") "start_ps" else "ps"
      x_label <- if (input$pd_visual_type == "boxplot_start") "Starting Position" else "Finishing Position"
      title   <- paste("Place Differential by", x_label)
      viz     <- plot_data %>%
        filter(!!sym(x_col) <= 40, !is.na(!!sym(x_col))) %>%
        mutate(Grp = factor(as.character(!!sym(x_col)),
                            levels = as.character(1:40)))
      grp_ord <- as.character(1:40)
      make_colored_box(viz, "PD", title, "Place Differential",
                       color_by = color_by_pd, margin_l = 80,
                       grp_order = grp_ord)
    }
  })
  
  # ---------------------------------------------------------------------------
  # PERFORMANCE
  # ---------------------------------------------------------------------------
  performance_data <- reactive({
    req(values$analysis_filtered_data, performance_race_ids_reactive())
    req(!is.null(input$perf_time_filter))
    data <- values$analysis_filtered_data %>%
      filter(race_id %in% performance_race_ids_reactive())
    if (input$perf_time_filter != "all")
      data <- data %>% filter(race_season == as.integer(input$perf_time_filter))
    data
  })
  
  output$performance_data_table <- DT::renderDataTable({
    req(performance_data())
    performance_data() %>%
      select(any_of(c("Full_Name", "start_ps", "ps", "ARP", "SpdRk",
                      "fast_laps", "lead_laps", "DKSP", "FDSP",
                      "DKDomRank", "FDDomRank", "DKPoints", "FDPoints",
                      "car_number", "team_name", "race_season",
                      "track_name", "finishing_status", "LapsDown"))) %>%
      mutate(across(any_of(c("ARP", "DKSP", "FDSP", "DKPoints", "FDPoints")),
                    ~round(., 1))) %>%
      rename_with(~recode(.,
                          Full_Name       = "Driver",   start_ps    = "Start",    ps        = "Finish",
                          fast_laps       = "FL",       lead_laps   = "LL",       DKSP      = "DK Dom Pts",
                          FDSP            = "FD Dom Pts", DKPoints  = "DK Pts",   FDPoints  = "FD Pts",
                          DKDomRank       = "DK Dom Rank", FDDomRank = "FD Dom Rank",
                          car_number      = "Car",      team_name   = "Team",     race_season = "Season",
                          track_name      = "Track",    finishing_status = "Status", LapsDown = "Laps Down"
      )) %>%
      DT::datatable(
        rownames = FALSE, filter = "top",
        class    = "display nowrap compact",
        options  = list(
          pageLength = 25, scrollX = TRUE, dom = "rtip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      )
  })
  
  output$download_performance_csv <- downloadHandler(
    filename = function() paste0("performance_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(performance_data())
      write.csv(performance_data(), file, row.names = FALSE)
    }
  )
  
  output$performance_plot <- renderPlotly({
    req(values$analysis_filtered_data, performance_race_ids_reactive(),
        input$perf_visual_type, values$analysis_entry_list)
    req(!is.null(input$perf_visual_time))
    
    viz_data <- values$analysis_filtered_data %>%
      filter(race_id %in% performance_race_ids_reactive())
    if (input$perf_visual_time != "all")
      viz_data <- viz_data %>% filter(race_season == as.integer(input$perf_visual_time))
    
    time_label    <- if (input$perf_visual_time == "all") "Full History" else input$perf_visual_time
    entry_drivers <- values$analysis_entry_list$Name
    entry_teams   <- unique(values$analysis_entry_list$Team)
    
    color_by_perf <- "none"
    
    make_perf_box <- function(data, grp_col, val_col, title_txt, x_label,
                              ascending = TRUE, margin_l = 150) {
      grp_order <- data %>%
        filter(!is.na(!!sym(val_col))) %>%
        group_by(!!sym(grp_col)) %>%
        summarise(m = mean(!!sym(val_col), na.rm = TRUE), .groups = "drop") %>%
        { if (ascending) arrange(., desc(m)) else arrange(., m) } %>%
        pull(!!sym(grp_col))
      data %>%
        filter(!!sym(grp_col) %in% grp_order, !is.na(!!sym(val_col))) %>%
        mutate(Grp = factor(!!sym(grp_col), levels = grp_order)) %>%
        make_colored_box(val_col,
                         paste(title_txt, "—", time_label), x_label,
                         color_by = color_by_perf, margin_l = margin_l,
                         grp_order = grp_order)
    }
    
    switch(input$perf_visual_type,
           driver_speed  = make_perf_box(viz_data %>% filter(Full_Name %in% entry_drivers, !is.na(SpdRk)),
                                         "Full_Name", "SpdRk", "Speed Rank by Driver",    "Speed Rank"),
           team_speed    = make_perf_box(viz_data %>% filter(team_name %in% entry_teams,  !is.na(SpdRk)),
                                         "team_name", "SpdRk", "Team Speed Rank",          "Speed Rank"),
           driver_finish = make_perf_box(viz_data %>% filter(Full_Name %in% entry_drivers, !is.na(ps)),
                                         "Full_Name", "ps",    "Finish Distribution by Driver", "Finish Position"),
           team_finish   = make_perf_box(viz_data %>% filter(team_name %in% entry_teams,  !is.na(ps)),
                                         "team_name", "ps",    "Team Finish Distribution", "Finish Position"),
           driver_arp    = make_perf_box(viz_data %>% filter(Full_Name %in% entry_drivers, !is.na(ARP)),
                                         "Full_Name", "ARP",   "ARP by Driver",            "Avg Running Position"),
           team_arp      = make_perf_box(viz_data %>% filter(team_name %in% entry_teams,  !is.na(ARP)),
                                         "team_name", "ARP",   "Team ARP Distribution",    "Avg Running Position")
    )
  })
  
  # ---------------------------------------------------------------------------
  # FANTASY SCORING
  # ---------------------------------------------------------------------------
  fantasy_data <- reactive({
    req(values$analysis_filtered_data, dominator_filtered_races())
    values$analysis_filtered_data %>%
      filter(race_id %in% dominator_filtered_races())
  })
  
  output$fantasy_data_table <- DT::renderDataTable({
    req(fantasy_data(), input$fs_platform)
    if (input$fs_platform == "DK") {
      fantasy_data() %>%
        filter(DKRank <= 25) %>%
        transmute(Driver = Full_Name, Rank = DKRank,
                  `Total Pts` = round(DKPoints, 1), `Finish Pts` = round(DKFP, 1),
                  `PD Pts` = round(DKPD, 1), `Dom Pts` = round(DKSP, 1),
                  Finish = ps, Start = start_ps,
                  `Laps Led` = lead_laps, `Fast Laps` = fast_laps,
                  Race = race_name, Track = track_name, Season = race_season) %>%
        arrange(desc(`Total Pts`))
    } else {
      fantasy_data() %>%
        filter(FDRank <= 25) %>%
        transmute(Driver = Full_Name, Rank = FDRank,
                  `Total Pts` = round(FDPoints, 1), `Finish Pts` = round(FDFP, 1),
                  `PD Pts` = round(FDPD, 1), `Dom Pts` = round(FDSP, 1),
                  `Lap Pts` = round(FDLP, 1),
                  Finish = ps, Start = start_ps, `Laps Led` = lead_laps,
                  Race = race_name, Track = track_name, Season = race_season) %>%
        arrange(desc(`Total Pts`))
    } %>%
      DT::datatable(
        rownames  = FALSE,
        class     = "display nowrap compact",
        options   = list(
          pageLength = 25, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      )
  })
  
  output$download_fantasy_csv <- downloadHandler(
    filename = function() paste0("fantasy_", input$fs_platform, "_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(fantasy_data())
      write.csv(fantasy_data(), file, row.names = FALSE)
    }
  )
  
  output$fantasy_plot <- renderPlotly({
    req(fantasy_data(), input$fs_visual_type, input$fs_visual_platform)
    plot_data     <- fantasy_data()
    color_by_fs   <- "none"
    platform      <- input$fs_visual_platform
    points_col    <- if (platform == "DK") "DKPoints" else "FDPoints"
    rank_col      <- if (platform == "DK") "DKRank"   else "FDRank"
    platform_name <- if (platform == "DK") "DraftKings" else "FanDuel"
    
    dark_theme <- theme_minimal() + theme(
      plot.title       = element_text(size = 18, face = "bold", color = "#FFE500"),
      axis.title       = element_text(size = 16, color = "#ffffff"),
      axis.text        = element_text(size = 14, color = "#ffffff"),
      panel.background = element_rect(fill = "#1e1e1e"),
      plot.background  = element_rect(fill = "#1e1e1e"),
      panel.grid.major = element_line(color = "#404040"),
      panel.grid.minor = element_line(color = "#333333"),
      legend.background = element_rect(fill = "#2d2d2d"),
      legend.key        = element_rect(fill = "#2d2d2d"),
      legend.text       = element_text(color = "#ffffff"),
      legend.title      = element_text(color = "#FFE500"))
    dark_layout <- function(p) p %>% layout(
      paper_bgcolor = "#2d2d2d", plot_bgcolor = "#2d2d2d",
      font  = list(color = "#ffffff"),
      xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
      yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"))
    
    vt <- input$fs_visual_type
    
    if (vt == "score_dist") {
      viz <- plot_data %>% filter(!!sym(rank_col) <= 15)
      p <- ggplot(viz, aes(x = factor(!!sym(rank_col)), y = !!sym(points_col))) +
        geom_boxplot(aes(text = sprintf("Rank: %d\nPts: %.1f\nDriver: %s\nTrack: %s",
                                        !!sym(rank_col), !!sym(points_col), Full_Name, track_name)),
                     fill = "#3a6ea5", alpha = 0.8) +
        geom_smooth(aes(x = as.numeric(!!sym(rank_col)), y = !!sym(points_col), group = 1),
                    method = "loess", se = FALSE, color = "#FFE500", linewidth = 1.5) +
        labs(title = paste(platform_name, "Points by Fantasy Rank"),
             x = "Rank", y = "Points") +
        scale_x_discrete(limits = factor(1:15)) + dark_theme
      ggplotly(p, tooltip = "text", height = 700) %>% dark_layout()
      
    } else if (vt == "components") {
      make_comp <- function(d) {
        if (platform == "DK")
          d %>% mutate(
            Finish_Pct = round(DKFP / DKPoints * 100, 1),
            PD_Pct     = round(DKPD / DKPoints * 100, 1),
            Dom_Pct    = round(DKSP / DKPoints * 100, 1))
        else
          d %>% mutate(
            Finish_Pct = round(FDFP / (FDPoints - FDLP) * 100, 1),
            PD_Pct     = round(FDPD / (FDPoints - FDLP) * 100, 1),
            Dom_Pct    = round(FDSP / (FDPoints - FDLP) * 100, 1))
      }
      grp_col <- rank_col
      filt    <- plot_data %>% filter(!!sym(rank_col) <= 15)
      comp_data <- make_comp(filt) %>%
        group_by(!!sym(grp_col)) %>%
        summarise(FP = mean(Finish_Pct, na.rm = TRUE),
                  PD = mean(PD_Pct,     na.rm = TRUE),
                  Dom= mean(Dom_Pct,    na.rm = TRUE), .groups = "drop") %>%
        pivot_longer(cols = c(FP, PD, Dom), names_to = "Type", values_to = "Pct") %>%
        mutate(Type = recode(Type, FP = "Finish Position",
                             PD = "Place Differential", Dom = "Dominator Points"))
      p <- ggplot(comp_data, aes(x = factor(!!sym(grp_col)), y = Pct, fill = Type)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.0f%%", Pct)),
                  position = position_stack(vjust = 0.5),
                  color = "white", fontface = "bold", size = 3) +
        scale_fill_manual(values = c(
          "Finish Position"    = "#1a1a1a",
          "Place Differential" = "#DAA520",
          "Dominator Points"   = "#FFE500")) +
        labs(title = paste(platform_name, "Scoring Components"),
             x = grp_col, y = "%", fill = "Type") + dark_theme
      ggplotly(p, tooltip = c("x", "y", "fill"), height = 700) %>% dark_layout()
      
    } else {
      filt_col  <- if (vt == "score_by_start") "start_ps" else "ps"
      fill_col  <- if (vt == "score_by_start") "#4a6fa5" else "#5a9e6f"
      x_label   <- if (vt == "score_by_start") "Starting Position" else "Finish Position"
      viz <- plot_data %>%
        filter(!!sym(filt_col) <= 40, !is.na(!!sym(filt_col)), !is.na(!!sym(points_col)))
      p <- ggplot(viz, aes(x = factor(!!sym(filt_col)), y = !!sym(points_col))) +
        geom_boxplot(fill = fill_col, color = "#FFE500", alpha = 0.7) +
        labs(title = paste(platform_name, "Points by", x_label),
             x = x_label, y = "Points") +
        scale_x_discrete(limits = factor(1:40)) + dark_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#ffffff"))
      ggplotly(p, height = 700) %>% dark_layout()
    }
  })
  
} # end server

shinyApp(ui = ui, server = server)