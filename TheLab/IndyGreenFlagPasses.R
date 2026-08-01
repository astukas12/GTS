# =============================================================================
# IndyGreenFlagPasses.R
# Golden Ticket Sims - One-off green-flag pass analysis
# 2026 Brickyard 400 (Cup, Indianapolis Motor Speedway)
#
# Pulls lap-by-lap running order from cf.nascar.com/.../lap-times.json,
# detects every position-order pass (A gains a spot on a specific B),
# classifies each lap's flag condition + restart bucket, and flags
# pit in/out laps via lap-speed anomaly so they can be excluded.
#
# Output: IndyPasses.xlsx with sheets:
#   Passes     - every detected pass, one row per A-over-B pair
#   LapStatus  - per-lap flag condition, restart bucket, field pace
#   PitLaps    - the laps flagged as pit in/out per driver
# =============================================================================

library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(openxlsx)

# -----------------------------------------------------------------------------
# CONFIG
# -----------------------------------------------------------------------------
RACE_SEASON <- 2026
SERIES_ID   <- 1                       # 1 = Cup
TRACK_MATCH <- "Indianapolis Motor Speedway"   # excludes the Road Course variant below
OUT_FILE    <- "IndyPasses.xlsx"

# Pit in/out-lap detection thresholds (hybrid: must satisfy BOTH).
# A green Cup lap at Indy is ~50-51s; a stop adds ~25-40s+ to the in AND out lap,
# so the separation is large. Percent guards against flagging a merely-slow
# traffic lap; the absolute floor guards against a driver whose own median is
# already inflated (start/park, damage).
PIT_PCT_THRESHOLD <- 1.07    # lap > 107% of driver's own rolling green median
PIT_ABS_SECONDS   <- 8       # AND lap is at least this many seconds over that median

log_msg <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))

# -----------------------------------------------------------------------------
# 1. FIND THE RACE_ID  (same discovery pattern as DBUpdate.R)
# -----------------------------------------------------------------------------
log_msg("Locating Indianapolis Cup race_id...")
race_list <- fromJSON(sprintf("https://cf.nascar.com/cacher/%d/race_list_basic.json", RACE_SEASON))
cup_races <- race_list$series_1

indy <- cup_races %>%
  filter(str_detect(track_name, fixed(TRACK_MATCH)),
         !str_detect(track_name, regex("road course", ignore_case = TRUE)))

if (nrow(indy) == 0) stop("No Indianapolis Cup race found in race_list_basic.json")
# If multiple (unlikely for the oval), take the most recent by date.
if (nrow(indy) > 1 && "race_date" %in% names(indy)) {
  indy <- indy %>% arrange(desc(race_date)) %>% slice_head(n = 1)
} else {
  indy <- indy %>% slice_head(n = 1)
}

RACE_ID <- indy$race_id[1]
log_msg(sprintf("  Found: %s  (race_id=%s)", indy$race_name[1], RACE_ID))

# -----------------------------------------------------------------------------
# 2. PULL LAP-BY-LAP FEED
# -----------------------------------------------------------------------------
lap_url <- sprintf("https://cf.nascar.com/cacher/%d/%d/%d/lap-times.json",
                   RACE_SEASON, SERIES_ID, RACE_ID)
log_msg(sprintf("Fetching %s", lap_url))
feed <- fromJSON(lap_url)
if (!all(c("flags", "laps") %in% names(feed))) stop("Feed missing 'flags' or 'laps'")

# -----------------------------------------------------------------------------
# 3. LAP STATUS  -  flag condition + restart bucket, per lap
#    FlagState codes (per DBUpdate.R): 2 = yellow, 8 = red/other caution.
#    Green = anything else. Restart bucket = green laps since the last caution.
# -----------------------------------------------------------------------------
flags_raw <- feed$flags %>%
  arrange(LapsCompleted) %>%
  mutate(is_caution = FlagState %in% c(2, 8))

# Walk the lap sequence, resetting a counter at each caution.
# GreenSinceRestart: 1 on the first green lap after a yellow, 2 next, etc.
# Laps that are themselves under caution get GreenSinceRestart = 0.
gsr <- integer(nrow(flags_raw))
counter <- NA_integer_          # NA until we've seen the first green
for (k in seq_len(nrow(flags_raw))) {
  if (flags_raw$is_caution[k]) {
    counter <- 0L               # caution lap; next green will be restart+1
    gsr[k]  <- 0L
  } else {
    counter <- if (is.na(counter)) 1L else counter + 1L
    gsr[k]  <- counter
  }
}

lap_status <- flags_raw %>%
  mutate(
    GreenSinceRestart = gsr,
    FlagCondition = case_when(
      is_caution                      ~ "Caution",
      GreenSinceRestart <= 3L         ~ "Restart 1-3",
      GreenSinceRestart <= 7L         ~ "Restart 4-7",
      TRUE                            ~ "Green (8+)"    # settled green racing
    )
  ) %>%
  select(Lap = LapsCompleted, FlagState, FlagCondition, GreenSinceRestart)

caution_laps <- lap_status %>% filter(FlagCondition == "Caution") %>% pull(Lap)
log_msg(sprintf("  %d cautions laps; %d green laps",
                length(caution_laps),
                sum(lap_status$FlagCondition != "Caution")))

# -----------------------------------------------------------------------------
# 4. EXPAND PER-DRIVER LAP DATA  (running position + lap speed per lap)
# -----------------------------------------------------------------------------
driver_lookup <- feed$laps %>% distinct(NASCARDriverID, Number, FullName)

laps_long <- feed$laps %>%
  select(NASCARDriverID, FullName, Number, Laps) %>%
  unnest(Laps) %>%
  transmute(
    driver_id = NASCARDriverID,
    driver    = FullName,
    car       = Number,
    lap       = as.integer(Lap),
    pos       = as.integer(RunningPos),
    lap_speed = as.numeric(LapSpeed)
  ) %>%
  filter(!is.na(lap), !is.na(pos)) %>%
  arrange(driver_id, lap)

# -----------------------------------------------------------------------------
# 5. PIT / ANOMALY LAP DETECTION  (per driver, hybrid threshold)
#    Convert lap_speed -> lap_time so thresholds read in seconds. Indy is 2.5mi.
#    green median is taken over a driver's own non-anomalous-looking laps.
# -----------------------------------------------------------------------------
TRACK_MILES <- 2.5
laps_long <- laps_long %>%
  mutate(lap_time = if_else(!is.na(lap_speed) & lap_speed > 0,
                            TRACK_MILES / lap_speed * 3600, NA_real_))

# Tag each lap's flag condition so the green baseline is built from GREEN laps
# only. Caution laps run at ~half speed and would otherwise inflate every
# driver's median, causing mass false pit flags.
green_only_laps <- lap_status %>%
  transmute(lap = Lap, is_green = FlagCondition != "Caution")

pit_flags <- laps_long %>%
  left_join(green_only_laps, by = "lap") %>%
  mutate(is_green = coalesce(is_green, TRUE)) %>%
  group_by(driver_id) %>%
  mutate(
    # baseline: median of this driver's green laps only
    green_median = median(lap_time[is_green], na.rm = TRUE),
    over_pct     = lap_time / green_median,
    over_abs     = lap_time - green_median,
    # a pit in/out lap is a GREEN lap that is far slower than green pace.
    # (caution laps are excluded from passes separately, so we don't flag them here.)
    is_pit_lap   = !is.na(lap_time) & is_green &
      over_pct >= PIT_PCT_THRESHOLD &
      over_abs >= PIT_ABS_SECONDS
  ) %>%
  ungroup()

pit_lap_lookup <- pit_flags %>%
  select(driver_id, lap, is_pit_lap)

pit_laps_out <- pit_flags %>%
  filter(is_pit_lap) %>%
  select(driver, car, lap, lap_time, green_median) %>%
  arrange(driver, lap)

log_msg(sprintf("  %d driver-laps flagged as pit in/out laps", nrow(pit_laps_out)))

# -----------------------------------------------------------------------------
# 6. PASS DETECTION
#    For each lap transition N-1 -> N, a pass A-over-B exists when A was behind
#    B on N-1 and ahead of B on N. We record every such crossing pair.
#    Then tag each pass with the flag condition of lap N and whether either
#    driver had a pit/anomaly lap on N-1 or N (the in/out laps).
# -----------------------------------------------------------------------------
log_msg("Detecting passes...")

# Wide matrix: rows = laps, cols = drivers, value = running position.
pos_wide <- laps_long %>%
  select(lap, driver_id, pos) %>%
  pivot_wider(names_from = driver_id, values_from = pos) %>%
  arrange(lap)

driver_ids <- setdiff(names(pos_wide), "lap")
all_laps   <- pos_wide$lap

detect_lap_passes <- function(prev_row, cur_row, cur_lap) {
  ids   <- driver_ids
  prevp <- as.numeric(prev_row[ids])
  curp  <- as.numeric(cur_row[ids])
  names(prevp) <- ids; names(curp) <- ids
  keep <- !is.na(prevp) & !is.na(curp)
  ids  <- ids[keep]; prevp <- prevp[keep]; curp <- curp[keep]
  if (length(ids) < 2) return(NULL)
  
  out <- list()
  for (a in ids) {
    # drivers A gained on: B was ahead last lap (lower pos) and is now behind A
    gained_on <- ids[prevp[a] > prevp & curp[a] < curp]
    for (b in gained_on) {
      out[[length(out) + 1]] <- data.frame(
        lap        = cur_lap,
        passer_id  = a,
        passed_id  = b,
        passer_prev = prevp[a], passer_pos = curp[a],
        passed_prev = prevp[b], passed_pos = curp[b],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0) return(NULL)
  bind_rows(out)
}

passes <- map_dfr(2:nrow(pos_wide), function(r) {
  detect_lap_passes(pos_wide[r - 1, ], pos_wide[r, ], all_laps[r])
})

log_msg(sprintf("  %d raw position-order passes detected", nrow(passes)))

# -----------------------------------------------------------------------------
# 7. ANNOTATE PASSES  -  names, flag condition, pit involvement
# -----------------------------------------------------------------------------
name_of <- driver_lookup %>%
  transmute(driver_id = as.character(NASCARDriverID), driver = FullName, car = Number)

# IDs coming out of the position matrix (via pivot_wider column names) are
# character; make the join keys and the pit lookup match.
passes <- passes %>%
  mutate(passer_id = as.character(passer_id),
         passed_id = as.character(passed_id))
pit_lap_lookup <- pit_lap_lookup %>%
  mutate(driver_id = as.character(driver_id))

# pit involvement: either driver's in-lap (N) or out-lap accounted for by
# checking pit flag on lap N and N-1 for both passer and passed.
pit_on <- function(id, lp) {
  any(pit_lap_lookup$is_pit_lap[pit_lap_lookup$driver_id == id &
                                  pit_lap_lookup$lap %in% c(lp, lp - 1)])
}

passes_annot <- passes %>%
  left_join(name_of %>% rename(passer = driver, passer_car = car), by = c("passer_id" = "driver_id")) %>%
  left_join(name_of %>% rename(passed = driver, passed_car = car), by = c("passed_id" = "driver_id")) %>%
  left_join(lap_status, by = c("lap" = "Lap")) %>%
  rowwise() %>%
  mutate(
    passer_pit = pit_on(passer_id, lap),
    passed_pit = pit_on(passed_id, lap),
    pit_involved = passer_pit | passed_pit
  ) %>%
  ungroup() %>%
  mutate(
    PassCategory = case_when(
      FlagCondition == "Caution"                 ~ "Caution (excluded)",
      pit_involved                               ~ "Pit-cycle (excluded)",
      FlagCondition == "Restart 1-3"             ~ "Restart 1-3",
      FlagCondition == "Restart 4-7"             ~ "Restart 4-7",
      TRUE                                       ~ "True Green (8+)"
    ),
    TrueGreenPass = PassCategory == "True Green (8+)"
  ) %>%
  select(lap, FlagCondition, GreenSinceRestart, PassCategory, TrueGreenPass,
         passer, passer_car, passer_prev, passer_pos,
         passed, passed_car, passed_prev, passed_pos,
         passer_pit, passed_pit) %>%
  arrange(lap, passer_pos)

# -----------------------------------------------------------------------------
# 8. WRITE OUTPUT
# -----------------------------------------------------------------------------
log_msg(sprintf("Writing %s...", OUT_FILE))

header_style <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#1a1a1a", halign = "center",
  fontName = "Calibri", fontSize = 11, textDecoration = "bold",
  border = "Bottom", borderColour = "#FFE500"
)
green_style <- createStyle(fgFill = "#E8F5E9")

wb <- createWorkbook()

addWorksheet(wb, "Passes", tabColour = "#FFE500")
writeData(wb, "Passes", passes_annot)
addStyle(wb, "Passes", header_style, rows = 1, cols = 1:ncol(passes_annot), gridExpand = TRUE)
# shade true-green rows
tg_rows <- which(passes_annot$TrueGreenPass) + 1
if (length(tg_rows) > 0)
  addStyle(wb, "Passes", green_style, rows = tg_rows,
           cols = 1:ncol(passes_annot), gridExpand = TRUE, stack = TRUE)
setColWidths(wb, "Passes", cols = 1:ncol(passes_annot), widths = "auto")
freezePane(wb, "Passes", firstRow = TRUE)

addWorksheet(wb, "LapStatus", tabColour = "#1a1a1a")
writeData(wb, "LapStatus", lap_status)
addStyle(wb, "LapStatus", header_style, rows = 1, cols = 1:ncol(lap_status), gridExpand = TRUE)
setColWidths(wb, "LapStatus", cols = 1:ncol(lap_status), widths = "auto")
freezePane(wb, "LapStatus", firstRow = TRUE)

addWorksheet(wb, "PitLaps", tabColour = "#1a1a1a")
writeData(wb, "PitLaps", pit_laps_out)
addStyle(wb, "PitLaps", header_style, rows = 1, cols = 1:ncol(pit_laps_out), gridExpand = TRUE)
setColWidths(wb, "PitLaps", cols = 1:ncol(pit_laps_out), widths = "auto")
freezePane(wb, "PitLaps", firstRow = TRUE)

saveWorkbook(wb, OUT_FILE, overwrite = TRUE)

# -----------------------------------------------------------------------------
# 9. SUMMARY
# -----------------------------------------------------------------------------
cat("\n")
log_msg("=== COMPLETE ===")
summary_tbl <- passes_annot %>% count(PassCategory, name = "passes") %>% arrange(desc(passes))
print(summary_tbl)
cat(sprintf("\nCaution laps: %s\n", paste(caution_laps, collapse = ", ")))