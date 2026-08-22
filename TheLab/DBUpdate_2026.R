# =============================================================================
# DBUpdate.R
# Golden Ticket Sims - NASCAR Database Weekly Update
# Reads and writes NascarData.xlsx (Races sheet + Results sheet)
# Additive only — appends new races, never rewrites existing data
# Run from ~/GTS/Nascar/ after each race weekend
# =============================================================================

library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(openxlsx)
library(stringr)


# "~" is the Documents folder under RStudio but the user profile under
# Rscript, so the hard-coded path only resolves in one of them. Move if it is
# there, otherwise assume we were already launched from the right directory.
.gts_wd <- path.expand("~/GitHub/GTS/TheLab")
if (dir.exists(.gts_wd)) setwd(.gts_wd)

# -----------------------------------------------------------------------------
# CONFIGURATION
# -----------------------------------------------------------------------------
CURRENT_YEAR   <- as.integer(format(Sys.Date(), "%Y"))
START_YEAR     <- 2026
DATA_FILE      <- "NascarData.xlsx"       # Single source of truth
FANTASY_FILE   <- "FantasyScoring.xlsx"
TODAY          <- Sys.Date()

log_msg <- function(msg) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
}

# -----------------------------------------------------------------------------
# HELPER: clean_name
# Strips special characters, normalises whitespace. Applied to all name fields.
# -----------------------------------------------------------------------------
clean_name <- function(name) {
  sapply(name, function(n) {
    if (is.na(n)) return(n)
    n <- iconv(n, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    n <- gsub("[^[:alnum:][:space:].-]", "", n)
    n <- gsub("\\s+", " ", n)
    trimws(n)
  }, USE.NAMES = FALSE)
}

# -----------------------------------------------------------------------------
# HELPER: with_retry
# Wraps an expression with one retry after a short delay.
# Catches transient API timeouts that would otherwise silently skip a race.
# -----------------------------------------------------------------------------
with_retry <- function(expr, delay = 2) {
  result <- tryCatch(expr, error = function(e) e)
  if (inherits(result, "error")) {
    Sys.sleep(delay)
    result <- tryCatch(expr, error = function(e) {
      log_msg(sprintf("  Retry failed: %s", e$message))
      NULL
    })
  }
  result
}

# -----------------------------------------------------------------------------
# TRACK TYPE MAP
# -----------------------------------------------------------------------------
track_type_map <- tribble(
  ~track_name,                               ~track_type,
  "Los Angeles Memorial Coliseum",           "short_track",
  "Daytona International Speedway",          "superspeedway",
  "Auto Club Speedway",                      "intermediate",
  "Las Vegas Motor Speedway",                "intermediate",
  "Phoenix Raceway",                         "short_track",
  "Atlanta Motor Speedway",                  "atlanta",
  "Circuit of The Americas",                 "road_course",
  "Richmond Raceway",                        "short_track",
  "Martinsville Speedway",                   "short_track",
  "Bristol Motor Speedway Dirt",             "dirt",
  "Talladega Superspeedway",                 "superspeedway",
  "Dover Motor Speedway",                    "intermediate",
  "Darlington Raceway",                      "intermediate",
  "Kansas Speedway",                         "intermediate",
  "Texas Motor Speedway",                    "intermediate",
  "Charlotte Motor Speedway",                "intermediate",
  "World Wide Technology Raceway",           "intermediate",
  "Sonoma Raceway",                          "road_course",
  "Nashville Superspeedway",                 "intermediate",
  "Road America",                            "road_course",
  "New Hampshire Motor Speedway",            "short_track",
  "Pocono Raceway",                          "intermediate",
  "Indianapolis Motor Speedway Road Course", "road_course",
  "Michigan International Speedway",         "intermediate",
  "Watkins Glen International",              "road_course",
  "Bristol Motor Speedway",                  "short_track",
  "Charlotte Motor Speedway Road Course",    "road_course",
  "Homestead-Miami Speedway",                "intermediate",
  "Portland International Raceway",          "road_course",
  "Knoxville Raceway",                       "dirt",
  "Mid-Ohio Sports Car Course",              "road_course",
  "Lucas Oil Indianapolis Raceway Park",     "short_track",
  "North Wilkesboro Speedway",               "short_track",
  "Chicago Street Race",                     "road_course",
  "Milwaukee Mile Speedway",                 "short_track",
  "Iowa Speedway",                           "short_track",
  "Indianapolis Motor Speedway",             "intermediate",
  "The Milwaukee Mile",                      "short_track",
  "Bowman Gray Stadium",                     "short_track",
  "Autodromo Hermanos Rodriguez",            "road_course",
  "Rockingham Speedway",                     "intermediate",
  "Lime Rock Park",                          "road_course"
)

# -----------------------------------------------------------------------------
# API FUNCTIONS
# -----------------------------------------------------------------------------

pull_race_data <- function(year) {
  url <- sprintf("https://cf.nascar.com/cacher/%d/race_list_basic.json", year)
  with_retry(tryCatch({
    json_data <- fromJSON(url)
    extract_series <- function(s) {
      if (is.null(s) || nrow(s) == 0) return(NULL)
      s
    }
    bind_rows(Filter(Negate(is.null), list(
      extract_series(json_data$series_1),
      extract_series(json_data$series_2),
      extract_series(json_data$series_3)
    )))
  }, error = function(e) {
    log_msg(sprintf("  API error for year %d: %s", year, e$message))
    NULL
  }))
}

get_driver_data <- function() {
  with_retry(tryCatch({
    driver_json <- fromJSON("https://cf.nascar.com/cacher/drivers.json")
    driver_json$response %>%
      select(driver_id = Nascar_Driver_ID, Full_Name = Full_Name) %>%
      mutate(Full_Name = clean_name(Full_Name))
  }, error = function(e) NULL))
}

process_loop_data <- function(race_season, race_id, series_id) {
  url <- sprintf("https://cf.nascar.com/loopstats/prod/%d/%d/%d.json",
                 race_season, series_id, race_id)
  with_retry(tryCatch({
    json_data <- fromJSON(url)
    if (is.null(json_data) || length(json_data) == 0) return(NULL)
    unnest(json_data, drivers) %>%
      select(race_id, race_name, series_id, act_laps, driver_id,
             start_ps, ps, fast_laps, lead_laps, top15_laps, laps) %>%
      mutate(race_name = clean_name(race_name))
  }, error = function(e) NULL))
}

process_lap_data <- function(race_season, race_id, series_id) {
  url <- sprintf("https://cf.nascar.com/cacher/%d/%d/%d/lap-times.json",
                 race_season, series_id, race_id)
  with_retry(tryCatch({
    json_data <- fromJSON(url)
    if (is.null(json_data) || length(json_data) == 0) return(NULL)
    if (!all(c("flags", "laps") %in% names(json_data)))  return(NULL)
    
    FlagStatus <- json_data$flags %>%
      mutate(
        FlagStatusadd1 = lag(FlagState),
        FlagStatusads2 = lag(FlagStatusadd1),
        Status = if_else(
          FlagState %in% c(2, 8) |
            FlagStatusadd1 %in% c(2, 8) |
            FlagStatusads2 %in% c(2, 8),
          "EXCLUDE", "INCLUDE"
        )
      ) %>%
      select(LapsCompleted, Status)
    
    Drivers <- json_data$laps %>% rename(FinishingPosition = RunningPos)
    
    raw_lap_data <- unnest(Drivers, Laps) %>%
      left_join(FlagStatus, by = c("Lap" = "LapsCompleted")) %>%
      mutate(
        Status   = if_else(is.na(Status), "INCLUDE", Status),
        LapSpeed = as.numeric(LapSpeed),
        LapSpeed = if_else(LapSpeed < 50 | LapSpeed > 250 | is.na(LapSpeed), NA_real_, LapSpeed)
      )
    
    if (sum(!is.na(raw_lap_data$LapSpeed)) == 0) return(NULL)
    
    DriverCounts <- raw_lap_data %>%
      filter(Status == "INCLUDE") %>%
      group_by(Lap) %>%
      mutate(
        LapRank    = if_else(!is.na(LapSpeed), rank(-LapSpeed, ties.method = "first"), NA_integer_),
        FastLapEst = if_else(!is.na(LapRank) & LapRank == 1,  1L, 0L),
        T3FL       = if_else(!is.na(LapRank) & LapRank < 4,   1L, 0L),
        T5FL       = if_else(!is.na(LapRank) & LapRank < 6,   1L, 0L),
        T10FL      = if_else(!is.na(LapRank) & LapRank < 11,  1L, 0L),
        LapsLed    = if_else(RunningPos == 1, 1L, 0L),
        T3L        = if_else(RunningPos < 4,  1L, 0L),
        T5L        = if_else(RunningPos < 6,  1L, 0L)
      ) %>%
      group_by(NASCARDriverID) %>%
      summarise(across(c(FastLapEst, LapsLed, T3FL, T5FL, T10FL, T3L, T5L),
                       \(x) sum(x, na.rm = TRUE)), .groups = "drop")
    
    SpeedRanksOvr <- raw_lap_data %>%
      filter(Status == "INCLUDE") %>%
      group_by(Lap) %>%
      mutate(LapRank = if_else(!is.na(LapSpeed), rank(-LapSpeed, ties.method = "first"), NA_integer_)) %>%
      ungroup() %>%
      group_by(NASCARDriverID) %>%
      summarise(
        FinishingPosition = first(FinishingPosition),
        LapsCompleted     = n(),
        ARP               = mean(RunningPos, na.rm = TRUE),
        Speed             = mean(LapSpeed, na.rm = TRUE),
        ASR_raw           = mean(LapRank, na.rm = TRUE),
        ValidSpeedLaps    = sum(!is.na(LapSpeed)),
        .groups           = "drop"
      )
    
    result_laps <- raw_lap_data %>%
      group_by(NASCARDriverID) %>%
      summarise(actual_laps = max(Lap, na.rm = TRUE), .groups = "drop")
    
    SpeedRanksOvr <- SpeedRanksOvr %>%
      left_join(result_laps, by = "NASCARDriverID") %>%
      mutate(actual_laps = if_else(is.na(actual_laps), LapsCompleted, actual_laps))
    
    winner_laps   <- max(SpeedRanksOvr$actual_laps, na.rm = TRUE)
    lap_threshold <- floor(winner_laps * 0.75)
    
    SpeedRanksOvr %>%
      mutate(
        LapCompletionRate = actual_laps / winner_laps,
        ASR = if_else(actual_laps >= lap_threshold & !is.na(ASR_raw), ASR_raw, NA_real_)
      ) %>%
      mutate(SpdRk = if_else(!is.na(ASR), rank(ASR, ties.method = "first"), NA_integer_)) %>%
      select(-ASR_raw, -ValidSpeedLaps, -actual_laps) %>%
      left_join(DriverCounts, by = "NASCARDriverID") %>%
      rename(driver_id = NASCARDriverID)
    
  }, error = function(e) {
    log_msg(sprintf("  Lap data error: %s", e$message))
    NULL
  }))
}

process_weekend_data <- function(race_season, race_id, series_id) {
  url <- sprintf("https://cf.nascar.com/cacher/%d/%d/%d/weekend-feed.json",
                 race_season, series_id, race_id)
  with_retry(tryCatch({
    json_data <- fromJSON(url)
    if (is.null(json_data$weekend_race)) return(NULL)
    json_data$weekend_race %>%
      unnest(results, names_sep = "_") %>%
      select(
        driver_id             = results_driver_id,
        car_number            = results_car_number,
        team_name             = results_team_name,
        finishing_status      = results_finishing_status,
        crew_chief_fullname   = results_crew_chief_fullname,
        points_position       = results_points_position,
        driver_fullname       = results_driver_fullname
      ) %>%
      mutate(
        Full_Name       = clean_name(driver_fullname),
        car_number      = as.numeric(car_number),
        points_position = as.numeric(points_position)
      ) %>%
      select(-driver_fullname)
  }, error = function(e) NULL))
}

calculate_fantasy_points <- function(data, scoring_table) {
  data %>%
    left_join(scoring_table, by = "ps") %>%
    mutate(
      LapsDown = laps - act_laps,
      DKPD     = start_ps - ps,
      FDPD     = 0.5 * (start_ps - ps),
      DKSP     = (0.25 * lead_laps) + (0.45 * fast_laps),
      FDSP     = 0.1 * lead_laps,
      FDLP     = 0.1 * laps,
      DKPoints = DKFP + DKPD + DKSP,
      FDPoints = FDFP + FDPD + FDSP + FDLP
    ) %>%
    group_by(race_id) %>%
    mutate(
      DKRank    = rank(-DKPoints, ties.method = "first"),
      FDRank    = rank(-FDPoints, ties.method = "first"),
      DKDomRank = rank(-DKSP,    ties.method = "first"),
      FDDomRank = rank(-FDSP,    ties.method = "first")
    ) %>%
    ungroup()
}

# Column order matching DBCleanup.R schema
results_col_order <- c(
  "race_id", "race_season", "series_id", "series_name",
  "race_name", "track_name", "track_type", "race_type_id",
  "driver_id", "Full_Name", "car_number", "team_name",
  "crew_chief_fullname", "finishing_status", "points_position",
  "start_ps", "ps", "LapsDown",
  "laps", "act_laps", "lead_laps", "fast_laps", "top15_laps",
  "scheduled_laps", "actual_laps",
  "FinishingPosition", "LapsCompleted", "LapCompletionRate",
  "ARP", "Speed", "ASR", "SpdRk",
  "FastLapEst", "LapsLed",
  "T3FL", "T5FL", "T10FL", "T3L", "T5L",
  "DKFP", "DKPD", "DKSP", "DKPoints", "DKRank", "DKDomRank",
  "FDFP", "FDPD", "FDSP", "FDLP", "FDPoints", "FDRank", "FDDomRank",
  "has_lap_data", "has_asr"
)

# Races sheet column order matching DBCleanup.R schema
races_col_order <- c(
  "race_id", "race_season", "series_id", "series_name", "race_type_id", "race_type",
  "race_name", "track_id", "track_name", "track_type",
  "race_date", "qualifying_date", "date_scheduled",
  "scheduled_laps", "actual_laps", "scheduled_distance", "actual_distance",
  "stage_1_laps", "stage_2_laps", "stage_3_laps",
  "number_of_cars_in_field",
  "number_of_lead_changes", "number_of_leaders",
  "number_of_cautions", "number_of_caution_laps",
  "average_speed", "total_race_time", "margin_of_victory",
  "winner_driver_id", "pole_winner_driver_id", "pole_winner_speed",
  "television_broadcaster", "radio_broadcaster", "satellite_radio_broadcaster",
  "Historical", "Qualifying", "Stages",
  "restrictor_plate", "playoff_round",
  "master_race_id"
)

# =============================================================================
# MAIN EXECUTION
# =============================================================================
log_msg("=== NASCAR DB UPDATE STARTING ===")
log_msg(sprintf("  Current year: %d  |  Today: %s", CURRENT_YEAR, TODAY))

# -----------------------------------------------------------------------------
# 1. LOAD EXISTING DATA FROM NascarData.xlsx
# -----------------------------------------------------------------------------
if (!file.exists(DATA_FILE)) {
  stop(sprintf("%s not found. Run DBCleanup.R first to create it.", DATA_FILE))
}

log_msg(sprintf("Loading %s...", DATA_FILE))
existing_races   <- read_xlsx(DATA_FILE, sheet = "Races")
existing_results <- read_xlsx(DATA_FILE, sheet = "Results")

existing_race_ids   <- unique(existing_races$race_id)
existing_result_ids <- unique(existing_results$race_id)

log_msg(sprintf("  Races sheet:   %d races", length(existing_race_ids)))
log_msg(sprintf("  Results sheet: %d rows across %d races",
                nrow(existing_results), length(existing_result_ids)))

# -----------------------------------------------------------------------------
# 2. PULL CURRENT RACE SCHEDULE FROM NASCAR API (current year only)
# We only need to pull the current year — all prior years are already in the
# Races sheet and won't change. Historical flag updates are date-based.
# -----------------------------------------------------------------------------
log_msg(sprintf("Pulling %d schedule from NASCAR API...", CURRENT_YEAR))

raw_api <- pull_race_data(CURRENT_YEAR)

if (is.null(raw_api) || nrow(raw_api) == 0) {
  stop("No race data returned from API. Check connection and try again.")
}

# Build clean Races rows from API data (current year only)
api_races <- raw_api %>%
  mutate(
    track_name  = clean_name(track_name),
    race_name   = clean_name(race_name),
    series_name = case_when(
      series_id == 1 ~ "Cup Series",
      series_id == 2 ~ "OReilly Series",
      series_id == 3 ~ "Truck Series",
      TRUE           ~ "Unknown"
    ),
    race_type = case_when(
      race_type_id == 1 ~ "points",
      race_type_id == 2 ~ "exhibition",
      TRUE              ~ "other"
    ),
    # Historical = Y if race_date is in the past, N if upcoming
    race_date_parsed = as.Date(substr(race_date, 1, 10)),
    Historical = if_else(race_date_parsed < TODAY, "Y", "N"),
    Qualifying = if_else(!is.na(qualifying_date), "Y", "N"),
    Stages     = if_else(!is.na(stage_1_laps) & stage_1_laps > 0, "Y", "N")
  ) %>%
  left_join(track_type_map, by = "track_name") %>%
  mutate(track_type = coalesce(track_type, "other")) %>%
  select(any_of(races_col_order))

log_msg(sprintf("  API returned %d races for %d", nrow(api_races), CURRENT_YEAR))

# -----------------------------------------------------------------------------
# 3. UPDATE RACES SHEET
# 3a. Flip Historical N -> Y on existing races whose date has now passed
# 3b. Append any brand-new race_ids not yet in the Races sheet
# -----------------------------------------------------------------------------
log_msg("Updating Races sheet...")

# 3a. Update Historical flag on existing rows where date has passed
#     (handles the case where you run the update the Monday after a race)
races_updated <- existing_races %>%
  mutate(
    race_date_parsed = as.Date(substr(race_date, 1, 10)),
    Historical = if_else(
      Historical == "N" & !is.na(race_date_parsed) & race_date_parsed < TODAY,
      "Y",
      Historical
    )
  ) %>%
  select(-race_date_parsed)

n_flipped <- sum(existing_races$Historical == "N", na.rm = TRUE) -
  sum(races_updated$Historical  == "N", na.rm = TRUE)
if (n_flipped > 0) {
  newly_historical <- races_updated %>%
    filter(Historical == "Y") %>%
    anti_join(existing_races %>% filter(Historical == "Y"), by = "race_id") %>%
    pull(race_name)
  log_msg(sprintf("  Flipped Historical N->Y for %d race(s):", n_flipped))
  for (rn in newly_historical) log_msg(sprintf("    -> %s", rn))
}

# 3b. Find brand-new race_ids from the API not yet in the Races sheet
new_race_rows <- api_races %>%
  filter(!race_id %in% existing_race_ids)

if (nrow(new_race_rows) > 0) {
  log_msg(sprintf("  Adding %d new race(s) to Races sheet:", nrow(new_race_rows)))
  for (rn in new_race_rows$race_name) log_msg(sprintf("    -> %s", rn))
  races_updated <- bind_rows(races_updated, new_race_rows) %>%
    arrange(race_season, race_id)
} else {
  log_msg("  No new races to add to Races sheet")
}

# -----------------------------------------------------------------------------
# 4. DETERMINE WHICH RACES NEED RESULTS PROCESSING
# A race needs processing if:
#   - It is marked Historical = Y in the updated Races sheet (date has passed)
#   - Its race_id is NOT already in the Results sheet
# -----------------------------------------------------------------------------
races_needing_results <- races_updated %>%
  filter(Historical == "Y") %>%
  filter(!race_id %in% existing_result_ids)

log_msg(sprintf("Races needing results: %d", nrow(races_needing_results)))

if (nrow(races_needing_results) == 0) {
  log_msg("Results are up to date. Saving any Races sheet changes and exiting.")
  
  # Still need to save if Historical flags changed or new races were added
  if (n_flipped > 0 || nrow(new_race_rows) > 0) {
    log_msg(sprintf("Writing updated %s...", DATA_FILE))
    wb <- loadWorkbook(DATA_FILE)
    removeWorksheet(wb, "Races")
    addWorksheet(wb, "Races", tabColour = "#FFE500")
    writeData(wb, "Races", races_updated)
    header_style <- createStyle(
      fontColour = "#FFFFFF", fgFill = "#1a1a1a", halign = "center",
      fontName = "Calibri", fontSize = 11, textDecoration = "bold",
      border = "Bottom", borderColour = "#FFE500"
    )
    addStyle(wb, "Races", header_style, rows = 1,
             cols = 1:ncol(races_updated), gridExpand = TRUE)
    setColWidths(wb, "Races", cols = 1:ncol(races_updated), widths = "auto")
    freezePane(wb, "Races", firstRow = TRUE)
    saveWorkbook(wb, DATA_FILE, overwrite = TRUE)
    log_msg("  Saved.")
  }
  
  log_msg("=== COMPLETE (no new results to process) ===")
  quit(save = "no")
}

# -----------------------------------------------------------------------------
# 5. LOAD SUPPORT DATA
# -----------------------------------------------------------------------------
Scoring <- read_xlsx(FANTASY_FILE)

Drivers <- get_driver_data()

NameCorrections <- if (file.exists("NameCorrections.xlsx")) {
  read_xlsx("NameCorrections.xlsx") %>%
    mutate(Full_Name = clean_name(Full_Name))
} else {
  NULL
}

# Build name lookup from existing Results — prefer known canonical names
existing_driver_names <- existing_results %>%
  filter(!is.na(Full_Name)) %>%
  distinct(driver_id, Full_Name) %>%
  group_by(driver_id) %>%
  slice_head(n = 1) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 6. PROCESS EACH RACE
# -----------------------------------------------------------------------------
log_msg(sprintf("Processing %d race(s)...", nrow(races_needing_results)))
all_new_results <- list()

for (i in seq_len(nrow(races_needing_results))) {
  race <- races_needing_results[i, ]
  log_msg(sprintf("[%d/%d] %s (race_id=%d, series=%d)",
                  i, nrow(races_needing_results),
                  race$race_name, race$race_id, race$series_id))
  
  loop_data <- process_loop_data(race$race_season, race$race_id, race$series_id)
  
  if (is.null(loop_data) || nrow(loop_data) == 0) {
    log_msg("  No loop data available yet — skipping (will retry next run)")
    next
  }
  
  lap_data     <- process_lap_data(race$race_season, race$race_id, race$series_id)
  weekend_data <- process_weekend_data(race$race_season, race$race_id, race$series_id)
  
  combined <- loop_data
  
  if (!is.null(lap_data))     combined <- left_join(combined, lap_data,     by = "driver_id")
  if (!is.null(weekend_data)) combined <- left_join(combined, weekend_data, by = "driver_id")
  
  if (!"Full_Name" %in% names(combined)) combined$Full_Name <- NA_character_
  
  # Name resolution priority: NameCorrections > existing DB > weekend feed > API drivers
  if (!is.null(NameCorrections)) {
    combined <- combined %>%
      left_join(NameCorrections %>% rename(Name_Correction = Full_Name), by = "driver_id") %>%
      mutate(Full_Name = coalesce(Name_Correction, Full_Name)) %>%
      select(-Name_Correction)
  }
  
  combined <- combined %>%
    left_join(existing_driver_names %>% rename(Name_Existing = Full_Name), by = "driver_id") %>%
    mutate(Full_Name = coalesce(Full_Name, Name_Existing)) %>%
    select(-Name_Existing)
  
  if (!is.null(Drivers)) {
    # Deduplicate Drivers by driver_id before joining — the NASCAR API occasionally
    # returns the same driver_id more than once (e.g. multi-team entries like Finchum,
    # Gase). Without this, a many-to-one join fans out to many-to-many and every
    # affected driver gets a duplicate row, producing off-by-one DKRank/FDRank values.
    drivers_deduped <- Drivers %>%
      group_by(driver_id) %>%
      slice_head(n = 1) %>%
      ungroup()
    combined <- combined %>%
      left_join(drivers_deduped %>% rename(Name_API = Full_Name), by = "driver_id") %>%
      mutate(Full_Name = coalesce(Full_Name, Name_API)) %>%
      select(-Name_API)
  }
  
  # Calculate fantasy points
  race_results <- calculate_fantasy_points(combined, Scoring)
  
  # Add race metadata + new columns
  race_results <- race_results %>%
    mutate(
      race_season   = race$race_season,
      track_name    = clean_name(race$track_name),
      track_type    = race$track_type,
      race_type_id  = race$race_type_id,
      scheduled_laps = race$scheduled_laps,
      actual_laps   = race$actual_laps,
      series_name   = case_when(
        series_id == 1 ~ "Cup Series",
        series_id == 2 ~ "OReilly Series",
        series_id == 3 ~ "Truck Series",
        TRUE           ~ "Unknown"
      ),
      has_lap_data  = !is.na(FinishingPosition) & !is.na(LapsCompleted) & !is.na(Speed),
      has_asr       = !is.na(ASR)
    )
  
  # Enforce column order — add any missing cols as NA so bind_rows stays clean
  for (col in results_col_order) {
    if (!col %in% names(race_results)) race_results[[col]] <- NA
  }
  race_results <- race_results %>% select(all_of(results_col_order))
  
  all_new_results[[i]] <- race_results
  log_msg(sprintf("  Done — %d drivers", nrow(race_results)))
}

# -----------------------------------------------------------------------------
# 7. WRITE UPDATED NascarData.xlsx
# Races sheet:   updated Historical flags + any new race rows
# Results sheet: existing rows + new race results appended
# -----------------------------------------------------------------------------
if (length(all_new_results) == 0) {
  log_msg("No new results were processable this run (API data not ready yet).")
  log_msg("=== COMPLETE ===")
  quit(save = "no")
}

new_results <- bind_rows(all_new_results)
final_results <- bind_rows(existing_results, new_results)

log_msg(sprintf("Writing %s...", DATA_FILE))
log_msg(sprintf("  Races sheet:   %d rows", nrow(races_updated)))
log_msg(sprintf("  Results sheet: %d rows (+%d new)", nrow(final_results), nrow(new_results)))

header_style <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#1a1a1a", halign = "center",
  fontName = "Calibri", fontSize = 11, textDecoration = "bold",
  border = "Bottom", borderColour = "#FFE500"
)

wb <- loadWorkbook(DATA_FILE)

# Races sheet — replace entirely (small, always rewrite cleanly)
removeWorksheet(wb, "Races")
addWorksheet(wb, "Races", tabColour = "#FFE500")
writeData(wb, "Races", races_updated)
addStyle(wb, "Races", header_style, rows = 1,
         cols = 1:ncol(races_updated), gridExpand = TRUE)
setColWidths(wb, "Races", cols = 1:ncol(races_updated), widths = "auto")
freezePane(wb, "Races", firstRow = TRUE)

# Results sheet — replace entirely with existing + new appended
removeWorksheet(wb, "Results")
addWorksheet(wb, "Results", tabColour = "#1a1a1a")
writeData(wb, "Results", final_results)
addStyle(wb, "Results", header_style, rows = 1,
         cols = 1:ncol(final_results), gridExpand = TRUE)
setColWidths(wb, "Results", cols = 1:ncol(final_results), widths = "auto")
freezePane(wb, "Results", firstRow = TRUE)

saveWorkbook(wb, DATA_FILE, overwrite = TRUE)
log_msg(sprintf("  Saved %s", DATA_FILE))

# -----------------------------------------------------------------------------
# 8. SUMMARY
# -----------------------------------------------------------------------------
log_msg("")
log_msg("=== UPDATE COMPLETE ===")
log_msg(sprintf("  Historical flags flipped:  %d", n_flipped))
log_msg(sprintf("  New races added to sheet:  %d", nrow(new_race_rows)))
log_msg(sprintf("  New result races written:  %d", length(all_new_results)))
log_msg(sprintf("  New result rows written:   %d", nrow(new_results)))
log_msg(sprintf("  Total results in DB:       %d rows across %d races",
                nrow(final_results), n_distinct(final_results$race_id)))