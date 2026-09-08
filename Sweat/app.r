# Golden Ticket Contest Sweat Tool
#
# Multi-sport DraftKings contest analyser. Reads a DK contest-standings export,
# works out which sport it is from the roster-slot labels in the lineup string,
# and - when the matching sim input workbook is supplied - joins that sport's
# own metadata onto the contest so exposure can be sliced the way that sport
# actually thinks.
#
# Adding a sport means adding one entry to SPORTS below. Nothing else changes.
#
# The pre-2026 MMA-only version of this app, including its Live Sweat and
# Simulation Analysis tabs, is kept alongside as app_legacy_mma.R.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, dplyr, tidyr, ggplot2, DT, plotly, shinyWidgets,
               data.table, stringr, readxl, stringdist)

options(shiny.maxRequestSize = 100*1024^2)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Every roster-slot label DraftKings uses across the sports we handle. The
# lineup parser treats any of these as a delimiter, so this list is the union
# of all sports rather than any one sport's slots.
SLOT_TOKENS <- c("CNSTR", "CPT", "FLEX", "S-FLEX", "UTIL",
                 "QB", "RB", "WR", "TE", "DST", "K",
                 "PG", "SG", "SF", "PF", "C", "G", "F", "D", "P")

LOCKED_TOKEN <- "LOCKED"

# Jaro-Winkler cutoff for the last-resort name match in resolve_metadata().
#
# A real spelling variant between DK and an input sheet is very close indeed.
# Measured across NFL, NASCAR, MMA, MMA Showdown and CFB with correctly paired
# files, only two names ever needed fuzzy matching at all:
#   "British Brooks"      -> "Brittish Brooks"    0.013
#   "Joshua Pitsenberger" -> "Josh Pitsenberger"  0.021
# Whereas an input file for the *wrong* slate produces confident nonsense an
# order of magnitude further out ("Ben Black III" -> "Benji Blackburn" 0.093,
# "William Watson III" -> "Will Wilson" 0.105). There is a clean gap between
# the two populations, so sit in it: generous for genuine variants, nowhere
# near loose enough to invent a match out of a mismatched file.
FUZZY_MAX_DIST <- 0.05

# DK prints NFL team defenses by nickname ("Texans"); sim input files call them
# "HOU D/ST" with Team = HOU. This bridges the two.
NFL_NICKNAMES <- c(
  "cardinals" = "ARI", "falcons" = "ATL", "ravens"     = "BAL", "bills"      = "BUF",
  "panthers"  = "CAR", "bears"   = "CHI", "bengals"    = "CIN", "browns"     = "CLE",
  "cowboys"   = "DAL", "broncos" = "DEN", "lions"      = "DET", "packers"    = "GB",
  "texans"    = "HOU", "colts"   = "IND", "jaguars"    = "JAX", "chiefs"     = "KC",
  "raiders"   = "LV",  "chargers"= "LAC", "rams"       = "LAR", "dolphins"   = "MIA",
  "vikings"   = "MIN", "patriots"= "NE",  "saints"     = "NO",  "giants"     = "NYG",
  "jets"      = "NYJ", "eagles"  = "PHI", "steelers"   = "PIT", "49ers"      = "SF",
  "seahawks"  = "SEA", "buccaneers" = "TB", "titans"   = "TEN", "commanders" = "WAS"
)

# ---------------------------------------------------------------------------
# Generic helpers
# ---------------------------------------------------------------------------

# Normalise a name so "C.J. Stroud" and "CJ Stroud" collide, and
# "Albert Okwuegbunam Jr." matches "Albert Okwuegbunam".
norm_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  # Fold accents rather than letting the strip below turn them into spaces,
  # which would split a surname in two ("rodriguez" -> "rodr guez").
  x <- chartr("àáâãäåçèéêëìíîïñòóôõöùúûüýÿ",
             "aaaaaaceeeeiiiinooooouuuuyy", x)
  x <- gsub("[.'`]", "", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("[[:space:]]+", " ", trimws(x))
  x <- gsub(" (jr|sr|ii|iii|iv)$", "", x)
  trimws(x)
}

# Split a DK Lineup string into (SlotIdx, Slot, PlayerRaw).
# Walks whitespace tokens and treats any known slot label as a delimiter, so
# one function covers NASCAR ("D x D y ..."), NFL Classic
# ("DST Texans FLEX ... QB ..."), Showdown ("CPT ... FLEX ...") and the rest
# without knowing in advance which sport it is looking at.
parse_lineup_slots <- function(lineup) {
  if (is.na(lineup) || !nzchar(lineup)) return(NULL)
  toks <- strsplit(trimws(lineup), "[[:space:]]+")[[1]]
  slots <- character(0); nms <- character(0)
  cur <- NA_character_; buf <- character(0)
  for (t in toks) {
    if (t %in% SLOT_TOKENS) {
      if (!is.na(cur)) { slots <- c(slots, cur); nms <- c(nms, paste(buf, collapse = " ")) }
      cur <- t; buf <- character(0)
    } else {
      buf <- c(buf, t)
    }
  }
  if (!is.na(cur)) { slots <- c(slots, cur); nms <- c(nms, paste(buf, collapse = " ")) }
  if (!length(slots)) return(NULL)
  data.table(SlotIdx = seq_along(slots), Slot = slots, PlayerRaw = trimws(nms))
}

# Read a DK contest-standings export. These files are ragged: the lineup block
# runs the full length of the file while the player-ownership block only covers
# the first ~100 rows, which makes fread's column autodetect unreliable (it
# guesses the wrong width and can stop early). Force eleven character columns
# and name them ourselves.
read_contest_csv <- function(path) {
  dt <- fread(path, sep = ",", header = FALSE, skip = 1, fill = 11,
              na.strings = c("", "NA", "NULL"), colClasses = "character",
              showProgress = FALSE)
  if (ncol(dt) < 6) stop("Unrecognised contest file: fewer than 6 columns.")
  std <- c("Rank", "EntryId", "EntryName", "TimeRemaining", "Points", "Lineup",
           "Spacer", "Player", "RosterPosition", "Drafted", "FPTS")
  if (ncol(dt) < length(std)) {
    for (i in (ncol(dt) + 1):length(std)) dt[, (std[i]) := NA_character_]
  }
  setnames(dt, seq_along(std), std)
  dt
}

# Shared plotly cosmetics so every chart in the app looks the same.
gt_layout <- function(p, ytitle = "Leverage (%)", xtitle = "") {
  layout(p,
         plot_bgcolor  = "#1a1a1a",
         paper_bgcolor = "#000000",
         font   = list(color = "#FFFFFF"),
         xaxis  = list(title = xtitle, tickangle = -45, gridcolor = "#333333"),
         yaxis  = list(title = ytitle, gridcolor = "#333333"),
         margin = list(b = 130))
}

pct <- function(x, d = 1) ifelse(is.na(x), "-", paste0(formatC(x, format = "f", digits = d), "%"))

# Bucket a numeric column into labelled ranges, used for starting spot and
# salary tiers. Returns an ordered factor so "P6-10" sorts before "P11-15"
# rather than lexically after it.
bucket_range <- function(x, width, prefix = "", suffix = "") {
  if (all(is.na(x))) return(factor(rep(NA_character_, length(x))))
  lo  <- floor((x - 1) / width) * width + 1
  lab <- paste0(prefix, lo, "-", lo + width - 1, suffix)
  ord <- sort(unique(lo[!is.na(lo)]))
  factor(lab, levels = paste0(prefix, ord, "-", ord + width - 1, suffix), ordered = TRUE)
}

# Quantile tiers, also ordered - cut() already returns levels low-to-high.
bucket_quantile <- function(x, n = 5,
                            fmt = function(v) formatC(round(v), format = "d", big.mark = ",")) {
  if (all(is.na(x))) return(factor(rep(NA_character_, length(x))))
  br <- unique(stats::quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE))
  if (length(br) < 2) return(factor(rep(fmt(br[1]), length(x))))
  cut(x, breaks = br, include.lowest = TRUE, ordered_result = TRUE,
      labels = paste0(fmt(head(br, -1)), " - ", fmt(br[-1])))
}

# Input sheets are inconsistent about whether probabilities are 0-1 or 0-100.
# Scale to percent only when the column is clearly a fraction.
as_pct <- function(v) {
  if (all(is.na(v))) return(v)
  if (max(v, na.rm = TRUE) <= 1.5) v * 100 else v
}

# ---------------------------------------------------------------------------
# Sport adapters
#
# Each entry describes one sport: how its lineups are shaped, what its sim
# input workbook looks like, and which columns are worth grouping by or
# showing. `read_input` must return a data.table with at least Player and Key
# (the normalised name); everything else is optional metadata.
#
# group_dims maps a display label -> column name. Those become the choices in
# the Breakdown tab, which is how "reframe each sport to its own input sheet"
# actually happens: NFL groups by position/team/game, NASCAR by org/starting
# spot/salary tier.
# ---------------------------------------------------------------------------

# --- NASCAR ---------------------------------------------------------------
# Driver sheet: DKName, DKID, Name, car, team, DKSalary, Starting, DKOP,
# W/T3/T5/T10/T15/T20/T25/T30, DKMax.
read_input_nascar <- function(path, sheets) {
  if (!"Driver" %in% sheets) stop("NASCAR input needs a 'Driver' sheet.")
  d <- as.data.table(readxl::read_excel(path, sheet = "Driver"))
  name_col <- intersect(c("Name", "DKName", "Driver"), names(d))[1]
  if (is.na(name_col)) stop("'Driver' sheet has no Name column.")

  num <- function(cl) if (cl %in% names(d)) suppressWarnings(as.numeric(d[[cl]])) else NA_real_
  chr <- function(cl) if (cl %in% names(d)) trimws(as.character(d[[cl]])) else NA_character_

  m <- data.table(
    Player  = trimws(as.character(d[[name_col]])),
    Car     = chr("car"),
    Team    = chr("team"),
    Salary  = num("DKSalary"),
    Start   = num("Starting"),
    ProjOwn = as_pct(num("DKOP")),
    WinPct  = as_pct(num("W")),
    Top5    = as_pct(num("T5")),
    Top10   = as_pct(num("T10")),
    DKMax   = num("DKMax")
  )
  m <- m[!is.na(Player) & nzchar(Player)]
  # DK sometimes carries the id in the name: "Layne Riggs (43876735)".
  m[, Player := trimws(sub("\\s*\\([0-9]+\\)$", "", Player))]

  m[, StartGroup := bucket_range(Start, 5, prefix = "P")]
  m[, SalaryTier := bucket_quantile(
      Salary, 5,
      fmt = function(v) paste0("$", formatC(round(v), format = "d", big.mark = ",")))]
  m[, Key := norm_name(Player)]
  unique(m, by = "Key")
}

# --- NFL ------------------------------------------------------------------
# IDs sheet: Player, Team, Pos, DK_ID, ETR_Proj. Games sheet: GameKey +
# Home/Away. The per-team sheets carry depth chart and status notes, which are
# folded in when they are present.
read_input_nfl <- function(path, sheets) {
  if (!"IDs" %in% sheets) stop("NFL input needs an 'IDs' sheet.")
  ids <- as.data.table(readxl::read_excel(path, sheet = "IDs"))
  name_col <- intersect(c("Player", "Name"), names(ids))[1]
  if (is.na(name_col)) stop("'IDs' sheet has no Player/Name column.")
  if (!"Team" %in% names(ids)) stop("'IDs' sheet has no Team column.")

  m <- data.table(
    Player = trimws(as.character(ids[[name_col]])),
    Team   = toupper(trimws(as.character(ids$Team))),
    Pos    = if ("Pos" %in% names(ids)) toupper(trimws(as.character(ids$Pos))) else NA_character_,
    Proj   = if ("ETR_Proj" %in% names(ids)) suppressWarnings(as.numeric(ids$ETR_Proj)) else NA_real_
  )
  m <- m[!is.na(Player) & nzchar(Player)]

  if ("Games" %in% sheets) {
    g <- as.data.table(readxl::read_excel(path, sheet = "Games"))
    home <- intersect(c("HomeTeam", "FavTeam"), names(g))[1]
    away <- intersect(c("AwayTeam", "DogTeam"), names(g))[1]
    if (!is.na(home) && !is.na(away) && "GameKey" %in% names(g)) {
      gl <- unique(rbind(
        data.table(Team = toupper(trimws(g[[home]])), Game = as.character(g$GameKey)),
        data.table(Team = toupper(trimws(g[[away]])), Game = as.character(g$GameKey))
      ))
      m <- merge(m, gl, by = "Team", all.x = TRUE)
    }
    # Per-team sheets add depth-chart slot and active/out status.
    team_sheets <- intersect(sheets, unique(m$Team))
    if (length(team_sheets)) {
      depth <- rbindlist(lapply(team_sheets, function(ts) {
        td <- tryCatch(as.data.table(readxl::read_excel(path, sheet = ts)), error = function(e) NULL)
        if (is.null(td) || !"Player" %in% names(td)) return(NULL)
        data.table(
          Player    = trimws(as.character(td$Player)),
          DepthSlot = if ("DepthSlot" %in% names(td)) trimws(as.character(td$DepthSlot)) else NA_character_,
          Status    = if ("Status" %in% names(td)) trimws(as.character(td$Status)) else NA_character_
        )
      }), fill = TRUE)
      if (!is.null(depth) && nrow(depth)) {
        depth[, Key := norm_name(Player)]
        depth <- unique(depth[, .(Key, DepthSlot, Status)], by = "Key")
        m[, Key := norm_name(Player)]
        m <- merge(m, depth, by = "Key", all.x = TRUE)
      }
    }
  }
  if (!"Game" %in% names(m))      m[, Game := NA_character_]
  if (!"DepthSlot" %in% names(m)) m[, DepthSlot := NA_character_]
  if (!"Status" %in% names(m))    m[, Status := NA_character_]
  m[, Key := norm_name(Player)]
  unique(m, by = "Key")
}

# --- MMA ------------------------------------------------------------------
# Fights sheet: Name, Opponent, WeightClass, Rounds, DKSalary, DKOwn,
# OriginalML, DeViggedProb. The Opponent column is what enables the head-to-
# head logic behind the Live Lineups tab.
read_input_mma <- function(path, sheets, showdown = FALSE) {
  if (!"Fights" %in% sheets) stop("MMA input needs a 'Fights' sheet.")
  d <- as.data.table(readxl::read_excel(path, sheet = "Fights"))
  name_col <- intersect(c("Name", "Fighter", "Player"), names(d))[1]
  if (is.na(name_col)) stop("'Fights' sheet has no Name column.")
  if (!"Opponent" %in% names(d)) stop("'Fights' sheet has no Opponent column.")

  num <- function(cl) if (cl %in% names(d)) suppressWarnings(as.numeric(d[[cl]])) else NA_real_
  chr <- function(cl) if (cl %in% names(d)) trimws(as.character(d[[cl]])) else NA_character_

  m <- data.table(
    Player      = trimws(as.character(d[[name_col]])),
    Opponent    = trimws(as.character(d$Opponent)),
    WeightClass = chr("WeightClass"),
    Rounds      = num("Rounds"),
    Salary      = num("DKSalary"),
    ProjOwn     = as_pct(num("DKOwn")),
    ML          = num("OriginalML"),
    WinProb     = as_pct(num("DeViggedProb"))
  )
  m <- m[!is.na(Player) & nzchar(Player)]

  if (showdown) {
    # The Showdown slate is a subset of the classic card and prices it
    # separately (SDSal). Fighters not on it are left with no salary rather
    # than being shown their classic price.
    sd <- num("SDSal")
    m[, Salary := fifelse(is.na(sd) | sd <= 0, NA_real_, sd)]
    # DKOwn is projected ownership for the *classic* slate. Showdown ownership
    # is a different animal entirely, so it is deliberately dropped rather
    # than shown as if it applied.
    m[, ProjOwn := NA_real_]
  }

  # Favourite / underdog is the split MMA players actually think in.
  m[, Role := fifelse(is.na(WinProb), NA_character_,
                      fifelse(WinProb >= 50, "Favourite", "Underdog"))]
  # A canonical key for the bout, identical for both corners, so "which fights
  # am I overweight in" is a single grouping.
  m[, Fight := vapply(seq_len(.N), function(i)
        paste(sort(c(Player[i], Opponent[i])), collapse = " vs "), character(1))]
  m[, SalaryTier := bucket_quantile(
      Salary, 4,
      fmt = function(v) paste0("$", formatC(round(v), format = "d", big.mark = ",")))]
  m[, Key := norm_name(Player)]
  unique(m, by = "Key")
}

# --- College football -----------------------------------------------------
# Slate workbooks come in two shapes and both are in use:
#
#   older ("..._CFB_ALL")  team sheets carry dk_pos + salary_util;
#                          `projections` is player/etr/own.
#   newer ("..._CFB_THU")  team sheets carry usage only;
#                          `projections` is player/dk_pos/salary_util/own and a
#                          separate `etr` sheet gives player/team/etr_pts.
#
# So each field is gathered from whichever sheet happens to hold it and then
# coalesced, rather than assuming one layout.
read_input_cfb <- function(path, sheets, showdown = FALSE) {
  rd <- function(s) suppressMessages(as.data.table(readxl::read_excel(path, sheet = s)))
  lower <- tolower(sheets)
  gsheet <- sheets[lower == "game"][1]
  if (is.na(gsheet)) stop("CFB input needs a 'game' sheet.")
  psheet <- sheets[lower == "projections"][1]
  esheet <- sheets[lower == "etr"][1]
  team_sheets <- sheets[!lower %in% c("game", "projections", "etr")]

  # Always return a full-length column so a missing one can still be combined.
  chr <- function(x, cl) if (cl %in% names(x)) trimws(as.character(x[[cl]]))
                         else rep(NA_character_, nrow(x))
  num <- function(x, cl) if (cl %in% names(x)) suppressWarnings(as.numeric(x[[cl]]))
                         else rep(NA_real_, nrow(x))
  # Take a where present, otherwise b; either side may be absent entirely,
  # because the two workbook layouts each omit different columns.
  pick <- function(a, b) {
    if (is.null(a) && is.null(b)) return(NA)
    if (is.null(a)) return(b)
    if (is.null(b)) return(a)
    fifelse(is.na(a), b, a)
  }

  # One sheet per team, keyed by the sheet name.
  rost <- rbindlist(lapply(team_sheets, function(tm) {
    x <- tryCatch(rd(tm), error = function(e) NULL)
    if (is.null(x) || !"player" %in% names(x)) return(NULL)
    # These sheets can carry trailing field/value note rows with no player.
    x <- x[!is.na(player) & nzchar(trimws(as.character(player)))]
    if (!nrow(x)) return(NULL)
    data.table(Player = trimws(as.character(x$player)), Team = toupper(tm),
               Pos = toupper(chr(x, "dk_pos")), Salary = num(x, "salary_util"))
  }), fill = TRUE)
  if (is.null(rost) || !nrow(rost)) rost <- data.table(
    Player = character(0), Team = character(0), Pos = character(0), Salary = numeric(0))
  rost[, Key := norm_name(Player)]
  rost <- unique(rost, by = "Key")

  prj <- NULL
  if (!is.na(psheet)) {
    x <- tryCatch(rd(psheet), error = function(e) NULL)
    if (!is.null(x) && "player" %in% names(x)) {
      prj <- data.table(PlayerX = trimws(as.character(x$player)),
                        PosX = toupper(chr(x, "dk_pos")), SalaryX = num(x, "salary_util"),
                        ProjX = num(x, "etr"), ProjOwn = as_pct(num(x, "own")),
                        # Showdown workbooks also project captain ownership
                        # separately (NA in a classic sheet).
                        CptOwnX = as_pct(num(x, "cpt_own")))
      prj <- prj[!is.na(PlayerX) & nzchar(PlayerX)]
      prj[, Key := norm_name(PlayerX)]
      prj <- unique(prj, by = "Key")
    }
  }

  etr <- NULL
  if (!is.na(esheet)) {
    x <- tryCatch(rd(esheet), error = function(e) NULL)
    if (!is.null(x) && "player" %in% names(x)) {
      etr <- data.table(PlayerY = trimws(as.character(x$player)),
                        TeamY = toupper(chr(x, "team")),
                        ProjY = pick(num(x, "etr_pts"), num(x, "etr")))
      etr <- etr[!is.na(PlayerY) & nzchar(PlayerY)]
      etr[, Key := norm_name(PlayerY)]
      etr <- unique(etr, by = "Key")
    }
  }

  m <- rost
  if (!is.null(prj)) m <- merge(m, prj, by = "Key", all = TRUE)
  if (!is.null(etr)) m <- merge(m, etr, by = "Key", all = TRUE)
  if (!nrow(m)) stop("No player rows found in this workbook.")

  gc <- function(nm) if (nm %in% names(m)) m[[nm]] else NULL
  m[, Player := pick(pick(Player, gc("PlayerX")), gc("PlayerY"))]
  m[, Team   := pick(Team,   gc("TeamY"))]
  m[, Pos    := pick(Pos,    gc("PosX"))]
  m[, Salary := pick(Salary, gc("SalaryX"))]
  m[, Proj   := pick(gc("ProjX"), gc("ProjY"))]
  if (!"ProjOwn" %in% names(m)) m[, ProjOwn := NA_real_]
  if (showdown) m[, CptOwn := gc("CptOwnX")]
  for (cl in intersect(c("PlayerX", "PlayerY", "TeamY", "PosX", "SalaryX", "ProjX", "ProjY",
                         "CptOwnX"),
                       names(m))) m[, (cl) := NULL]

  g <- rd(gsheet)
  for (cl in c("away", "home")) if (!cl %in% names(g)) stop("'game' sheet has no ", cl, " column.")
  gkey <- paste0(trimws(as.character(g$away)), "@", trimws(as.character(g$home)))
  fav  <- toupper(chr(g, "fav"))
  gl <- rbind(
    data.table(Team = toupper(trimws(as.character(g$away))), Game = gkey,
               Spread = num(g, "spread"), Total = num(g, "total"),
               Kick = num(g, "start_order"), Fav = fav),
    data.table(Team = toupper(trimws(as.character(g$home))), Game = gkey,
               Spread = num(g, "spread"), Total = num(g, "total"),
               Kick = num(g, "start_order"), Fav = fav)
  )
  m <- merge(m, unique(gl[!is.na(Team)], by = "Team"), by = "Team", all.x = TRUE)
  m[, Fav := NULL]

  m[, SalaryTier := bucket_quantile(
      Salary, 5,
      fmt = function(v) paste0("$", formatC(round(v), format = "d", big.mark = ",")))]
  m <- m[!is.na(Player) & nzchar(Player)]
  m[, Key := norm_name(Player)]
  unique(m, by = "Key")
}

SPORTS <- list(
  NASCAR = list(
    label      = "NASCAR",
    entity     = "Driver",
    slots      = c("D"),
    read_input = read_input_nascar,
    input_hint = "Driver sheet (DKName, team, car, DKSalary, Starting, DKOP)",
    group_dims = c("Team / Org" = "Team", "Starting Spot" = "StartGroup",
                   "Salary Tier" = "SalaryTier"),
    # ProjOwn is deliberately not listed here: the exposure table adds it as
    # "Proj %" plus a "Field vs Proj" column via proj_own below.
    extra_cols = c("Car" = "Car", "Team" = "Team", "Salary" = "Salary",
                   "Start" = "Start"),
    proj_own   = "ProjOwn"
  ),
  F1 = list(
    label = "Formula 1", entity = "Driver", slots = c("CNSTR", "CPT", "D"),
    read_input = NULL, input_hint = NULL,
    group_dims = character(0), extra_cols = character(0), proj_own = NULL
  ),
  NFL = list(
    label      = "NFL Classic",
    entity     = "Player",
    slots      = c("QB", "RB", "WR", "TE", "FLEX", "DST"),
    read_input = read_input_nfl,
    input_hint = "IDs sheet (Player, Team, Pos) + Games sheet",
    group_dims = c("Position" = "Pos", "Team" = "Team", "Game" = "Game",
                   "Depth Slot" = "DepthSlot"),
    extra_cols = c("Pos" = "Pos", "Team" = "Team", "Game" = "Game",
                   "Depth" = "DepthSlot", "Proj" = "Proj"),
    proj_own   = NULL
  ),
  CFB = list(
    label      = "College Football",
    entity     = "Player",
    slots      = c("QB", "RB", "WR", "FLEX", "S-FLEX"),
    read_input = read_input_cfb,
    input_hint = "slate workbook (game sheet + one sheet per team + projections)",
    group_dims = c("Position" = "Pos", "Team" = "Team", "Game" = "Game",
                   "Salary Tier" = "SalaryTier"),
    extra_cols = c("Pos" = "Pos", "Team" = "Team", "Game" = "Game",
                   "Salary" = "Salary", "Proj" = "Proj", "Total" = "Total"),
    proj_own   = "ProjOwn"
  ),
  `CFB-SD` = list(
    label      = "CFB Showdown",
    entity     = "Player",
    slots      = c("CPT", "UTIL"),
    read_input = function(path, sheets) read_input_cfb(path, sheets, showdown = TRUE),
    input_hint = "the single-game showdown slate workbook (game + team sheets + projections)",
    group_dims = c("Position" = "Pos", "Team" = "Team", "Salary Tier" = "SalaryTier"),
    extra_cols = c("Pos" = "Pos", "Team" = "Team", "Salary" = "Salary", "Total" = "Total"),
    # Proj / Proj Own % / Proj CPT % render in their own section under the
    # exposure table, not inline - see the My Sweat projections block.
    proj_own   = "ProjOwn"
  ),
  NBA = list(
    label = "NBA", entity = "Player",
    slots = c("PG", "SG", "SF", "PF", "C", "G", "F", "UTIL"),
    read_input = NULL, input_hint = NULL,
    group_dims = character(0), extra_cols = character(0), proj_own = NULL
  ),
  CBB = list(
    label = "College Basketball", entity = "Player", slots = c("G", "F", "UTIL"),
    read_input = NULL, input_hint = NULL,
    group_dims = character(0), extra_cols = character(0), proj_own = NULL
  ),
  MMA = list(
    label      = "MMA",
    entity     = "Fighter",
    slots      = c("F"),
    read_input = read_input_mma,
    input_hint = "Fights sheet (Name, Opponent, DKSalary, DKOwn, DeViggedProb)",
    group_dims = c("Weight Class" = "WeightClass", "Fight" = "Fight",
                   "Favourite / Dog" = "Role", "Salary Tier" = "SalaryTier"),
    extra_cols = c("Opponent" = "Opponent", "Salary" = "Salary",
                   "Win %" = "WinProb", "ML" = "ML"),
    proj_own   = "ProjOwn"
  ),
  `MMA-SD` = list(
    label      = "MMA Showdown",
    entity     = "Fighter",
    slots      = c("CPT", "F"),
    read_input = function(path, sheets) read_input_mma(path, sheets, showdown = TRUE),
    input_hint = "the same Fights sheet as classic (SDSal is used for salary)",
    group_dims = c("Weight Class" = "WeightClass", "Fight" = "Fight",
                   "Salary Tier" = "SalaryTier"),
    extra_cols = c("Opponent" = "Opponent", "SD Salary" = "Salary",
                   "Win %" = "WinProb", "ML" = "ML"),
    # No showdown ownership projection exists in the input sheet; see
    # read_input_mma().
    proj_own   = NULL
  ),
  Tennis = list(
    label = "Tennis", entity = "Player", slots = c("P"),
    read_input = NULL, input_hint = NULL,
    group_dims = character(0), extra_cols = character(0), proj_own = NULL
  ),
  Golf = list(
    label = "Golf", entity = "Golfer", slots = c("G"),
    read_input = NULL, input_hint = NULL,
    group_dims = character(0), extra_cols = character(0), proj_own = NULL
  ),
  Showdown = list(
    label = "Showdown", entity = "Player", slots = c("CPT", "FLEX", "UTIL"),
    read_input = NULL, input_hint = NULL,
    group_dims = character(0), extra_cols = character(0), proj_own = NULL
  ),
  Unknown = list(
    label = "Unknown", entity = "Player", slots = character(0),
    read_input = NULL, input_hint = NULL,
    group_dims = character(0), extra_cols = character(0), proj_own = NULL
  )
)

# Identify the sport from the set of slot labels present in the lineups.
# Ordering matters: F1 lineups contain "D" like NASCAR, and NBA lineups contain
# "G"/"F"/"UTIL" like CBB, so the more specific test has to run first.
detect_sport <- function(slot_tokens) {
  tk <- unique(slot_tokens)
  has <- function(...) all(c(...) %in% tk)
  only <- function(...) setequal(tk, c(...))

  if (has("CNSTR"))                       return("F1")
  if (only("D"))                          return("NASCAR")
  # College football is QB/RB/WR/FLEX like the NFL but adds a superflex, so it
  # has to be tested first or it would come back as NFL.
  if (has("S-FLEX"))                      return("CFB")
  if (any(c("QB", "DST") %in% tk))        return("NFL")
  if (any(c("PG", "SG", "PF") %in% tk))   return("NBA")
  # MMA Showdown is "CPT <name> F <name> ..." - the F is what separates it
  # from CBB Showdown (CPT/UTIL) and from NFL/Soccer Showdown (CPT/FLEX).
  if (only("CPT", "F"))                   return("MMA-SD")
  # CFB Showdown is CPT/UTIL, shape-identical to CBB Showdown - only the input
  # workbook can tell them apart, which sport_key() does when one is supplied.
  if (has("CPT"))                         return("Showdown")
  if (has("UTIL"))                        return("CBB")
  if (only("F"))                          return("MMA")
  if (only("P"))                          return("Tennis")
  if (only("G"))                          return("Golf")
  "Unknown"
}

# The input workbook names the sport, more reliably than the roster slots can:
# CFB Showdown and CBB Showdown share the CPT/UTIL shape, so the slot set alone
# is ambiguous. Each adapter's sim workbook has a distinctive sheet signature.
identify_workbook_family <- function(sheets) {
  low <- tolower(sheets)
  if ("game" %in% low && "projections" %in% low) return("CFB")
  if ("fights" %in% low)                         return("MMA")
  if ("ids" %in% low)                            return("NFL")
  if ("driver" %in% low)                         return("NASCAR")
  NA_character_
}

# Combine the workbook's sport family with the contest's classic/showdown shape
# (a CPT roster slot means showdown). NFL and NASCAR have no showdown adapter,
# so they stay classic regardless.
family_to_key <- function(family, showdown) {
  if (is.na(family)) return(NA_character_)
  if (family == "CFB") return(if (showdown) "CFB-SD" else "CFB")
  if (family == "MMA") return(if (showdown) "MMA-SD" else "MMA")
  family
}

# Resolve DK display names to input-sheet metadata. Exact match on the
# normalised name first, NFL nickname lookup for defenses, then a conservative
# Jaro-Winkler fallback. Returns one row per input name, in order.
resolve_metadata <- function(dk_names, dk_pos, m, sport) {
  out <- data.table(Player = dk_names, DKPos = dk_pos, Key = norm_name(dk_names))
  out[, MatchedKey := NA_character_]
  if (is.null(m) || !nrow(m)) return(out)

  hit <- match(out$Key, m$Key)
  out[!is.na(hit), MatchedKey := m$Key[hit[!is.na(hit)]]]

  if (identical(sport, "NFL")) {
    dst_rows <- m[!is.na(Pos) & Pos == "DST"]
    need <- which(is.na(out$MatchedKey) &
                    (out$DKPos %in% c("DST", "D") | out$Key %in% names(NFL_NICKNAMES)))
    if (length(need) && nrow(dst_rows)) {
      abbr <- unname(NFL_NICKNAMES[out$Key[need]])
      k    <- dst_rows$Key[match(abbr, dst_rows$Team)]
      out[need[!is.na(k)], MatchedKey := k[!is.na(k)]]
    }
  }

  # Never fuzzy-match an NFL defense onto a skill player.
  tgt <- if (identical(sport, "NFL") && "Pos" %in% names(m)) m[is.na(Pos) | Pos != "DST"] else m
  rest <- which(is.na(out$MatchedKey) & !(out$DKPos %in% c("DST", "D")))
  if (length(rest) && nrow(tgt)) {
    a  <- stringdist::amatch(out$Key[rest], tgt$Key, method = "jw", p = 0.1,
                             maxDist = FUZZY_MAX_DIST)
    ok <- !is.na(a)
    if (any(ok)) out[rest[ok], MatchedKey := tgt$Key[a[ok]]]
  }
  out
}
# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(

  # Custom CSS - Golden Ticket Theme
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;600;700&display=swap');
      
      body {
        font-family: 'Poppins', sans-serif;
        background-color: #000000;
        color: #FFFFFF;
      }
      
      .header-container {
        background: linear-gradient(135deg, #000000 0%, #1a1a1a 100%);
        padding: 20px;
        margin-bottom: 30px;
        border-bottom: 3px solid #FFE500;
        box-shadow: 0 4px 6px rgba(255, 229, 0, 0.1);
      }
      
      .logo-title {
        display: flex;
        align-items: center;
        gap: 20px;
      }
      
      .app-title {
        color: #FFE500;
        font-size: 32px;
        font-weight: 700;
        margin: 0;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
      }
      
      .app-subtitle {
        color: #CCCCCC;
        font-size: 14px;
        margin: 5px 0 0 0;
      }
      
      .well {
        background-color: #1a1a1a;
        border: 1px solid #FFE500;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 2px 4px rgba(255, 229, 0, 0.1);
      }
      
      .upload-panel {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border: 2px solid #FFE500;
        border-radius: 12px;
        padding: 30px;
        margin-bottom: 20px;
        box-shadow: 0 4px 8px rgba(255, 229, 0, 0.2);
      }
      
      .form-control, .selectize-input {
        background-color: #2a2a2a !important;
        border: 1px solid #FFE500;
        color: #FFFFFF !important;
        border-radius: 4px;
      }
      
      .form-control:focus, .selectize-input.focus {
        background-color: #333333 !important;
        border-color: #FFE500;
        box-shadow: 0 0 8px rgba(255, 229, 0, 0.4);
        color: #FFFFFF !important;
      }
      
      .selectize-input input {
        color: #FFFFFF !important;
      }
      
      .selectize-input .item {
        background-color: #FFE500;
        color: #000000;
        border: none;
        padding: 2px 8px;
        border-radius: 3px;
      }
      
      .selectize-dropdown {
        background-color: #2a2a2a;
        border: 1px solid #FFE500;
        color: #FFFFFF;
      }
      
      .selectize-dropdown-content .option {
        background-color: #2a2a2a;
        color: #FFFFFF;
        padding: 8px 12px;
      }
      
      .selectize-dropdown-content .option:hover,
      .selectize-dropdown-content .option.active {
        background-color: #FFE500;
        color: #000000;
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #FFE500 0%, #FFA500 100%);
        border: none;
        color: #000000;
        font-weight: 600;
        border-radius: 6px;
        padding: 10px 24px;
        transition: all 0.3s ease;
        box-shadow: 0 2px 4px rgba(255, 229, 0, 0.3);
      }
      
      .btn-primary:hover {
        background: linear-gradient(135deg, #FFA500 0%, #FFE500 100%);
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(255, 229, 0, 0.4);
        color: #000000;
      }
      
      .btn-secondary {
        background: linear-gradient(135deg, #888888 0%, #666666 100%);
        border: none;
        color: #FFFFFF;
        font-weight: 600;
        border-radius: 6px;
        padding: 10px 24px;
        transition: all 0.3s ease;
      }
      
      .btn-secondary:hover {
        background: linear-gradient(135deg, #666666 0%, #888888 100%);
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(136, 136, 136, 0.4);
        color: #FFFFFF;
      }
      
      .nav-tabs {
        border-bottom: 2px solid #FFE500;
        background-color: #1a1a1a;
        border-radius: 8px 8px 0 0;
        padding: 10px 10px 0 10px;
      }
      
      .nav-tabs > li > a {
        color: #CCCCCC;
        background-color: #2a2a2a;
        border: 1px solid #444444;
        margin-right: 5px;
        border-radius: 6px 6px 0 0;
        font-weight: 600;
      }
      
      .nav-tabs > li > a:hover {
        background-color: #333333;
        border-color: #FFE500;
        color: #FFE500;
      }
      
      .nav-tabs > li.active > a {
        background-color: #000000;
        border-color: #FFE500;
        border-bottom-color: transparent;
        color: #FFE500;
      }
      
      .tab-content {
        background-color: #000000;
        border: 2px solid #FFE500;
        border-top: none;
        border-radius: 0 0 8px 8px;
        padding: 20px;
      }
      
      .dataTables_wrapper {
        color: #FFFFFF;
      }
      
      table.dataTable {
        background-color: #1a1a1a;
        color: #FFFFFF;
        border: 1px solid #FFE500;
      }
      
      table.dataTable thead th {
        background: linear-gradient(135deg, #FFE500 0%, #FFA500 100%);
        color: #000000;
        font-weight: 700;
        border-bottom: 2px solid #FFE500;
      }
      
      table.dataTable tbody tr {
        background-color: #1a1a1a;
        color: #FFFFFF;
      }
      
      table.dataTable tbody tr:hover {
        background-color: #2a2a2a !important;
      }
      
      table.dataTable tbody tr.even {
        background-color: #252525;
      }
      
      .dataTables_filter input,
      .dataTables_length select {
        background-color: #2a2a2a;
        color: #FFFFFF;
        border: 1px solid #FFE500;
        border-radius: 4px;
      }
      
      .stat-box {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border: 2px solid #FFE500;
        border-radius: 8px;
        padding: 20px;
        text-align: center;
        margin-bottom: 15px;
        box-shadow: 0 2px 4px rgba(255, 229, 0, 0.2);
      }
      
      .stat-value {
        font-size: 36px;
        font-weight: 700;
        color: #FFE500;
        margin: 10px 0;
      }
      
      .stat-label {
        font-size: 14px;
        color: #CCCCCC;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      .player-chip {
        display: inline-block;
        background-color: #2a2a2a;
        color: #FFFFFF;
        padding: 8px 16px;
        margin: 4px;
        border-radius: 20px;
        border: 1px solid #FFE500;
        font-size: 14px;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .player-chip:hover {
        background-color: #FFE500;
        color: #000000;
        transform: scale(1.05);
      }
      
      .player-chip.selected {
        background-color: #FFE500;
        border-color: #FFE500;
        color: #000000;
        font-weight: 600;
      }
      
      .js-plotly-plot {
        background-color: #1a1a1a;
      }
      
      h3, h4, h5 {
        color: #FFE500;
        font-weight: 700;
      }
    "))
  ),

  # Extra styling for badges, notes and the filter bar
  tags$head(tags$style(HTML("
      .gt-note {
        background-color: #141414;
        border-left: 4px solid #FFE500;
        color: #CCCCCC;
        padding: 10px 14px;
        margin: 10px 0 18px 0;
        border-radius: 0 6px 6px 0;
        font-size: 13px;
      }
      .gt-note.warn { border-left-color: #FFA500; }
      .gt-badge {
        display: inline-block;
        padding: 3px 10px;
        border-radius: 12px;
        font-size: 12px;
        font-weight: 600;
        margin-left: 8px;
      }
      .gt-badge.live   { background-color: #66BB6A; color: #000000; }
      .gt-badge.locked { background-color: #555555; color: #FFFFFF; }
      .filter-bar {
        background-color: #141414;
        border: 1px solid #333333;
        border-radius: 8px;
        padding: 12px 16px;
        margin-bottom: 18px;
      }
  "))),

  # Header
  div(class = "header-container",
      div(class = "logo-title",
          tags$img(src = "logo.jpg", height = "60px",
                   style = "border: 2px solid #FFE500; box-shadow: 0 4px 8px rgba(255, 229, 0, 0.3);"),
          div(
            h1(class = "app-title", "Contest Sweat Tool", uiOutput("sport_badge", inline = TRUE)),
            p(class = "app-subtitle", "Golden Ticket DFS Analytics")
          )
      )
  ),

  # Upload panel
  div(class = "upload-panel",
      fluidRow(
        column(4,
               fileInput("file",
                         label = div(style = "color: #FFE500; font-weight: 600; font-size: 16px;",
                                     "Upload DK Contest CSV"),
                         accept = c("text/csv", ".csv"),
                         buttonLabel = "Browse...",
                         placeholder = "No file selected")
        ),
        column(4,
               fileInput("input_file",
                         label = div(style = "color: #FFE500; font-weight: 600; font-size: 16px;",
                                     "Upload Sim Input File (.xlsx)"),
                         accept = c(".xlsx", ".xlsm"),
                         buttonLabel = "Browse...",
                         placeholder = "Optional - adds sport metadata")
        ),
        column(2,
               selectInput("sport_override",
                           label = div(style = "color: #FFE500; font-weight: 600;", "Sport"),
                           choices = c("Auto-detect", names(SPORTS)),
                           selected = "Auto-detect", width = "100%")
        ),
        column(2,
               selectizeInput("username",
                              label = div(style = "color: #FFE500; font-weight: 600;", "Your Username"),
                              choices = NULL,
                              options = list(placeholder = 'Username...',
                                             maxOptions = 50000, maxItems = 1),
                              width = "100%")
        )
      ),
      uiOutput("meta_status")
  ),

  tabsetPanel(
    id = "main_tabs",
    tabPanel("My Sweat",       uiOutput("my_sweat_content")),
    # Live Lineups is shown only for head-to-head sports (see the has_h2h
    # observer in the server); it is hidden outright otherwise.
    tabPanel("Live Lineups",   uiOutput("live_content")),
    tabPanel("Breakdown",      uiOutput("breakdown_content")),
    tabPanel("Lock Status",    uiOutput("lock_content")),
    tabPanel("Combo Analysis", uiOutput("combo_content")),
    tabPanel("Dupe Analysis",  uiOutput("dupe_analysis_content"))
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  addResourcePath("www", "www")

  # --- Contest file -------------------------------------------------------

  contest <- eventReactive(input$file, {
    req(input$file)
    withProgress(message = "Loading contest data...", value = 0, {

      incProgress(0.2, detail = "Reading CSV...")
      dt <- read_contest_csv(input$file$datapath)

      incProgress(0.3, detail = "Parsing lineups...")
      entries <- dt[!is.na(Lineup) & nzchar(trimws(Lineup)), .(
        EntryId  = EntryId,
        Username = trimws(sub(" \\(.*", "", EntryName)),
        Lineup   = trimws(Lineup),
        Points   = suppressWarnings(as.numeric(Points)),
        # For MMA this is the number of fighters in the lineup who have not
        # fought yet, which is used to sanity-check the derived fight results.
        Remaining = suppressWarnings(as.numeric(TimeRemaining))
      )]
      entries <- entries[!is.na(Username) & nzchar(Username) & Username != "EntryName"]
      if (!nrow(entries)) stop("No lineups found in this file.")
      entries[, EntryIdx := .I]

      uniq_lineups <- unique(entries$Lineup)
      parsed <- rbindlist(lapply(uniq_lineups, function(l) {
        p <- parse_lineup_slots(l)
        if (is.null(p)) return(NULL)
        p[, Lineup := l]
        p
      }))
      if (!nrow(parsed)) {
        stop("Could not parse any lineups. The roster-slot labels in this export are not recognised.")
      }

      incProgress(0.3, detail = "Building slot table...")
      slots <- merge(entries[, .(EntryIdx, Username, Lineup)], parsed,
                     by = "Lineup", allow.cartesian = TRUE)
      slots[, Locked := (PlayerRaw == LOCKED_TOKEN | !nzchar(PlayerRaw))]
      slots[, Player := fifelse(Locked, NA_character_, PlayerRaw)]

      incProgress(0.2, detail = "Reading ownership block...")
      pl <- dt[!is.na(Player) & nzchar(trimws(Player)) & trimws(Player) != "Player"]
      dkplayers <- NULL
      if (nrow(pl)) {
        pl[, `:=`(Player         = trimws(Player),
                  RosterPosition = trimws(RosterPosition),
                  Own            = suppressWarnings(as.numeric(gsub("%", "", Drafted))),
                  Pts            = suppressWarnings(as.numeric(FPTS)))]
        # DK reports a player once per roster slot he was used in (a RB shows up
        # under RB and again under FLEX). Sum them: the same guy in any slot is
        # one exposure.
        dkplayers <- pl[, .(
          DKFieldPct = sum(Own, na.rm = TRUE),
          DKSlots    = paste(sort(unique(RosterPosition)), collapse = "/"),
          DKPos      = {
            np <- unique(RosterPosition[!RosterPosition %in% c("FLEX", "CPT", "UTIL")])
            if (length(np)) np[1] else NA_character_
          },
          FPTS       = if (all(is.na(Pts))) NA_real_ else max(Pts, na.rm = TRUE),
          # In Showdown, DK reports the captain's FPTS already multiplied by
          # 1.5. Comparing a captain's score against a flex score would then
          # pick the wrong winner in head-to-head sports, so keep an unscaled
          # figure taken from the non-captain rows for that logic to use.
          BaseFPTS   = {
            b <- Pts[!RosterPosition %in% c("CPT")]
            if (!length(b) || all(is.na(b))) NA_real_ else max(b, na.rm = TRUE)
          }
        ), by = Player]
      }
      if (is.null(dkplayers)) {
        dkplayers <- data.table(Player = character(0), DKFieldPct = numeric(0),
                                DKSlots = character(0), DKPos = character(0),
                                FPTS = numeric(0), BaseFPTS = numeric(0))
      }

      list(entries     = entries,
           slots       = slots,
           dkplayers   = dkplayers,
           slot_tokens = sort(unique(parsed$Slot)),
           detected    = detect_sport(parsed$Slot))
    })
  })

  # --- Sport selection ----------------------------------------------------

  sport_key <- reactive({
    ct <- contest(); req(ct)
    if (!is.null(input$sport_override) && input$sport_override != "Auto-detect") {
      return(input$sport_override)
    }
    # A supplied input workbook identifies the sport; the contest's slot set
    # says whether this is its classic or its showdown variant.
    if (!is.null(input$input_file)) {
      fam <- tryCatch(identify_workbook_family(readxl::excel_sheets(input$input_file$datapath)),
                      error = function(e) NA_character_)
      k <- family_to_key(fam, "CPT" %in% ct$slot_tokens)
      if (!is.na(k) && k %in% names(SPORTS)) return(k)
    }
    ct$detected
  })

  adapter <- reactive({
    k <- sport_key()
    if (k %in% names(SPORTS)) SPORTS[[k]] else SPORTS$Unknown
  })

  output$sport_badge <- renderUI({
    ct <- contest()
    ad <- adapter()
    span(class = "gt-badge live", ad$label)
  })

  # --- Sim input workbook (optional, per sport) ---------------------------

  meta <- reactive({
    if (is.null(input$input_file)) return(NULL)
    ad <- adapter()
    if (is.null(ad$read_input)) return(NULL)
    tryCatch({
      path   <- input$input_file$datapath
      sheets <- readxl::excel_sheets(path)
      m <- ad$read_input(path, sheets)
      if (is.null(m) || !nrow(m)) stop("No rows read from the input workbook.")
      m
    }, error = function(e) {
      showNotification(paste("Sim input file:", conditionMessage(e)),
                       type = "error", duration = 10)
      NULL
    })
  })

  # --- Player universe ----------------------------------------------------

  players <- reactive({
    ct <- contest(); req(ct)
    dkp <- copy(ct$dkplayers)
    seen <- unique(ct$slots[Locked == FALSE, Player])
    seen <- seen[!is.na(seen)]

    # Anyone appearing in a lineup but missing from DK's ownership block.
    extra <- setdiff(seen, dkp$Player)
    if (length(extra)) {
      dkp <- rbind(dkp, data.table(Player = extra, DKFieldPct = NA_real_,
                                   DKSlots = NA_character_, DKPos = NA_character_,
                                   FPTS = NA_real_, BaseFPTS = NA_real_), fill = TRUE)
    }
    if (!nrow(dkp)) return(dkp)

    m <- meta()
    res <- resolve_metadata(dkp$Player, dkp$DKPos, m, sport_key())
    dkp[, MatchedKey := res$MatchedKey]

    if (!is.null(m) && nrow(m)) {
      mm <- copy(m)
      setnames(mm, "Player", "InputName")
      dkp <- merge(dkp, mm, by.x = "MatchedKey", by.y = "Key", all.x = TRUE)
    }
    dkp[, Matched  := !is.na(MatchedKey)]
    dkp[, Revealed := Player %in% seen]
    setkey(dkp, Player)
    dkp[]
  })

  # Which group dimensions this sport actually has data for. Dimensions that
  # are absent, all-NA, or single-valued are dropped so the UI never offers a
  # breakdown that would produce one row.
  group_dims <- reactive({
    ad <- adapter(); pl <- players(); req(pl)
    dims <- ad$group_dims
    dims <- dims[dims %in% names(pl)]
    if (length(dims)) {
      keep <- vapply(dims, function(cl) {
        v <- pl[[cl]]
        sum(!is.na(v)) > 0 && length(unique(v[!is.na(v)])) > 1
      }, logical(1))
      dims <- dims[keep]
    }
    if (!length(dims) && "DKPos" %in% names(pl) &&
        length(unique(pl$DKPos[!is.na(pl$DKPos)])) > 1) {
      dims <- c("DK Roster Slot" = "DKPos")
    }
    dims
  })

  # Extra metadata columns worth showing in tables, for this sport.
  extra_cols <- reactive({
    ad <- adapter(); pl <- players(); req(pl)
    ex <- ad$extra_cols
    ex <- ex[ex %in% names(pl)]
    if (!length(ex)) return(character(0))
    keep <- vapply(ex, function(cl) sum(!is.na(pl[[cl]])) > 0, logical(1))
    ex[keep]
  })

  # --- Lock status --------------------------------------------------------
  #
  # DK hides a player in every lineup - yours included - until his game starts.
  # When a whole game is locked DK also omits those players from the ownership
  # block entirely, so the contest file alone cannot even say who is left. The
  # sim input file can, which is why the slate is enumerated from it when
  # available.
  lock_info <- reactive({
    ct <- contest(); pl <- players(); req(ct, pl)
    m <- meta()

    slot_state <- ct$slots[, .(
      Slots       = .N,
      LockedSlots = sum(Locked),
      LockedPct   = sum(Locked) / .N * 100
    ), by = Slot][order(Slot)]

    has_game <- "Game" %in% names(pl)
    game_state <- NULL
    locked_games <- character(0)
    if (has_game) {
      seen_g <- pl[!is.na(Game), .(ContestPlayers = .N, RevealedCount = sum(Revealed)), by = Game]
      if (!is.null(m) && "Game" %in% names(m)) {
        all_g <- unique(m[!is.na(Game), .(Game)])
        game_state <- merge(all_g, seen_g, by = "Game", all.x = TRUE)
        game_state[is.na(ContestPlayers), ContestPlayers := 0]
        game_state[is.na(RevealedCount),  RevealedCount  := 0]
      } else {
        game_state <- seen_g
      }
      game_state[, Status := fifelse(RevealedCount > 0, "Live / Final", "Locked - not started")]
      locked_games <- game_state[Status != "Live / Final", Game]
    }

    # Players DK has not revealed. When a whole game is locked DK omits them,
    # so fall back to the input roster for those games.
    lp <- pl[Revealed == FALSE]
    if (!nrow(lp) && length(locked_games) && !is.null(m) && "Game" %in% names(m)) {
      lp <- copy(m[Game %in% locked_games])
      lp[, DKFieldPct := NA_real_]
    }

    list(slots           = slot_state,
         games           = game_state,
         locked_games    = locked_games,
         locked_players  = lp,
         locked_slot_pct = mean(ct$slots$Locked) * 100)
  })

  # --- Username dropdown --------------------------------------------------

  observeEvent(contest(), {
    ct <- contest()
    updateSelectizeInput(session, "username",
                         choices = sort(unique(ct$entries$Username)),
                         options = list(maxOptions = 50000))
  })

  output$meta_status <- renderUI({
    if (is.null(input$file)) return(NULL)
    ct <- contest(); ad <- adapter(); pl <- players(); m <- meta()

    if (is.null(ad$read_input)) {
      return(div(class = "gt-note",
        paste0("Detected ", ad$label, ". Exposure, slots, combos and dupes all work, ",
               "but there is no input-sheet adapter for this sport yet, so there is no ",
               "team / salary / position metadata to break down by. Send a sim input ",
               "file plus a contest export for this sport and it gets one.")))
    }
    if (is.null(m)) {
      return(div(class = "gt-note",
        paste0("Detected ", ad$label, ". Upload the matching sim input file - ",
               ad$input_hint, " - to unlock the metadata breakdowns.")))
    }

    n_tot  <- nrow(pl)
    n_hit  <- sum(pl$Matched)
    missed <- pl[Matched == FALSE, Player]
    if (!length(missed)) {
      div(class = "gt-note",
          paste0("Sim input matched all ", n_tot, " contest ",
                 tolower(ad$entity), "s."))
    } else {
      # A large miss rate almost always means the input file is for a
      # different slate, which is worth saying out loud rather than quietly
      # dropping those rows from every table.
      bad <- length(missed) / max(n_tot, 1) > 0.2
      div(class = if (bad) "gt-note warn" else "gt-note",
          paste0(if (bad) "Check the file pairing. " else "",
                 "Sim input matched ", n_hit, " of ", n_tot, " contest ",
                 tolower(ad$entity), "s. Unmatched: ",
                 paste(head(missed, 12), collapse = ", "),
                 if (length(missed) > 12) paste0(" (+", length(missed) - 12, " more)") else "",
                 if (bad) " - that is a big enough gap that this input file is probably for a different slate." else ""))
    }
  })

  # --- Core exposure calculation -----------------------------------------

  exposure <- reactive({
    ct <- contest(); pl <- players()
    u  <- input$username
    req(ct, pl, u, nzchar(u))

    s <- ct$slots
    user_idx  <- unique(s[Username == u, EntryIdx])
    field_idx <- unique(s[Username != u, EntryIdx])
    n_user  <- length(user_idx)
    n_field <- max(length(field_idx), 1)
    req(n_user > 0)

    # A player counts once per lineup no matter which slot holds him, so RB /
    # WR / TE / FLEX (or CPT / UTIL) all roll into one number here.
    ue <- s[Username == u & Locked == FALSE, .(UserN  = .N), by = Player]
    fe <- s[Username != u & Locked == FALSE, .(FieldN = .N), by = Player]

    out <- merge(copy(pl), ue, by = "Player", all.x = TRUE)
    out <- merge(out,      fe, by = "Player", all.x = TRUE)
    out[is.na(UserN),  UserN  := 0]
    out[is.na(FieldN), FieldN := 0]

    # For revealed players both sides are exact and computed from lineups. For
    # a player whose game has not started nobody's lineup shows him, so the
    # only honest field number is DK's %Drafted and our own is unknown.
    out[, UserExp  := fifelse(Revealed, UserN / n_user * 100, NA_real_)]
    out[, FieldExp := fifelse(Revealed, FieldN / n_field * 100, DKFieldPct)]
    out[, Leverage := UserExp - FieldExp]

    # Showdown: also carry per-slot exposure so the My Sweat table can break the
    # pooled number into captain vs flex. The flex slot is whichever of the
    # adapter's two slots is not CPT (UTIL for CFB/CBB, F for MMA, FLEX generic).
    sd_slots <- adapter()$slots
    if ("CPT" %in% sd_slots) {
      flex_slot <- setdiff(sd_slots, "CPT")[1]
      slot_exp <- function(who, slot, denom) {
        cnt <- s[who & Locked == FALSE & Slot == slot, .(N = .N), by = Player]
        out[cnt, on = "Player", N := i.N]
        v <- fifelse(is.na(out$N), 0, out$N) / denom * 100
        out[, N := NULL]
        fifelse(out$Revealed, v, NA_real_)
      }
      out[, CptUserExp   := slot_exp(s$Username == u, "CPT",     n_user)]
      out[, CptFieldExp  := slot_exp(s$Username != u, "CPT",     n_field)]
      out[, FlexUserExp  := slot_exp(s$Username == u, flex_slot, n_user)]
      out[, FlexFieldExp := slot_exp(s$Username != u, flex_slot, n_field)]
      out[, CptLeverage  := CptUserExp  - CptFieldExp]
      out[, FlexLeverage := FlexUserExp - FlexFieldExp]
    }

    # Sports whose input sheet carries a projected ownership number get a
    # second axis: how far the field landed from projection, and where you sit
    # against the projection rather than against the field.
    po <- adapter()$proj_own
    if (!is.null(po) && po %in% names(out)) {
      out[, ProjOwnPct   := get(po)]
      out[, FieldVsProj  := FieldExp - ProjOwnPct]
      out[, YourVsProj   := UserExp  - ProjOwnPct]
    }

    list(tbl = out[order(-UserExp, -FieldExp)],
         n_user = n_user, n_field = n_field,
         user_idx = user_idx, field_idx = field_idx)
  })

  # --- Head-to-head fight tracking (Live Lineups) -------------------------
  #
  # Any sport whose input sheet carries an Opponent column gets this: two
  # competitors, one of whom loses, so a lineup holding the loser is dead.
  #
  # DK does not publish results, but it does publish each competitor's FPTS.
  # A bout counts as decided once either corner has scored, and the higher
  # score is the winner. That inference is then checked against DK's own
  # TimeRemaining column, which for MMA is the count of a lineup's fighters
  # who have yet to compete - if the two disagree, the app says so rather than
  # quietly showing wrong "alive" numbers.
  has_h2h <- reactive({
    pl <- players()
    !is.null(pl) && "Opponent" %in% names(pl) && any(!is.na(pl$Opponent))
  })

  # Live Lineups only means anything for a head-to-head sport, so the tab is
  # removed entirely rather than left to render an explanatory empty state.
  observe({
    h <- tryCatch(isTRUE(has_h2h()), error = function(e) FALSE)
    if (h) showTab("main_tabs", "Live Lineups")
    else   hideTab("main_tabs", "Live Lineups")
  })

  derived_fights <- reactive({
    pl <- players(); req(pl, has_h2h())
    # Use the unscaled score so a captain's 1.5x figure is never compared
    # against an opponent's flex figure.
    st <- pl[!is.na(Opponent), .(Player, Opponent,
                                 Fight = if ("Fight" %in% names(pl)) Fight else NA_character_,
                                 FPTS = fifelse(is.na(BaseFPTS), FPTS, BaseFPTS))]
    st[, OppFPTS := st$FPTS[match(norm_name(Opponent), norm_name(st$Player))]]
    st[is.na(FPTS),    FPTS    := 0]
    st[is.na(OppFPTS), OppFPTS := 0]
    st[, Decided := FPTS > 0 | OppFPTS > 0]
    # On an exact tie both corners come out "Won", so nobody is killed off -
    # the conservative way to be wrong.
    st[, Outcome := fifelse(!Decided, "Not started",
                            fifelse(FPTS >= OppFPTS, "Won", "Lost"))]
    st[]
  })

  # One row per bout, with whatever the user has overridden applied.
  fight_status <- reactive({
    # copy() matters: `:=` below would otherwise mutate the cached value of
    # derived_fights() by reference, so overrides would accumulate.
    st <- copy(derived_fights()); req(st)
    if (is.na(st$Fight[1])) {
      st[, Fight := vapply(seq_len(.N), function(i)
        paste(sort(c(Player[i], Opponent[i])), collapse = " vs "), character(1))]
    }
    bouts <- unique(st$Fight)
    for (b in bouts) {
      ov <- input[[paste0("fight_", make.names(b))]]
      if (is.null(ov) || !nzchar(ov) || ov == "Auto") next
      idx <- which(st$Fight == b)
      if (ov == "Not started") {
        st[idx, Outcome := "Not started"]
      } else {
        st[idx, Outcome := fifelse(Player == ov, "Won", "Lost")]
      }
    }
    st
  })

  live_state <- reactive({
    ct <- contest(); req(ct)
    if (!isTRUE(has_h2h())) return(NULL)
    st <- fight_status(); req(st)
    ex <- tryCatch(exposure(), error = function(e) NULL)

    losers  <- st[Outcome == "Lost", Player]
    decided <- st[Outcome != "Not started", Player]

    dead_idx  <- unique(ct$slots[Player %in% losers, EntryIdx])
    all_idx   <- unique(ct$slots$EntryIdx)
    alive_idx <- setdiff(all_idx, dead_idx)

    # Cross-check against DK's own remaining count.
    per_entry <- ct$slots[, .(Done = sum(Player %in% decided)), by = EntryIdx]
    per_entry <- merge(per_entry, ct$entries[, .(EntryIdx, Remaining)], by = "EntryIdx")
    roster <- ct$slots[, .N, by = EntryIdx][, max(N)]
    agrees <- NA
    if (!all(is.na(per_entry$Remaining))) {
      agrees <- all(per_entry$Remaining == roster - per_entry$Done, na.rm = TRUE)
    }

    list(fights = st, losers = losers, decided = decided,
         alive_idx = alive_idx, dead_idx = dead_idx,
         n_alive = length(alive_idx), n_total = length(all_idx),
         user_alive  = if (is.null(ex)) integer(0) else intersect(alive_idx, ex$user_idx),
         user_dead   = if (is.null(ex)) integer(0) else intersect(dead_idx,  ex$user_idx),
         bouts_done  = length(unique(st[Outcome != "Not started", Fight])),
         bouts_total = length(unique(st$Fight)),
         dk_agrees   = agrees)
  })

  # Filters shown above the My Sweat table, generated from whatever group
  # dimensions this sport has.
  filtered_exposure <- reactive({
    ex <- exposure(); req(ex)
    tb <- ex$tbl
    dims <- group_dims()
    for (i in seq_along(dims)) {
      sel <- input[[paste0("filter_", i)]]
      if (!is.null(sel) && length(sel)) tb <- tb[get(dims[i]) %in% sel]
    }
    if (isTRUE(input$hide_locked)) tb <- tb[Revealed == TRUE]
    tb
  })

  # --- Shared renderers ---------------------------------------------------

  msg_table <- function(msg) {
    datatable(data.table(Message = msg), rownames = FALSE, options = list(dom = "t"))
  }

  # One consistent look for every table in the app. Rounds whatever numeric
  # columns are present rather than assuming a fixed schema, because the
  # columns differ by sport.
  gt_table <- function(tb, bar_col = NULL, page = 25, digits = 1) {
    if (is.null(tb) || !nrow(tb)) return(msg_table("Nothing to show for this filter."))
    num_cols <- names(tb)[vapply(tb, is.numeric, logical(1))]
    d <- datatable(tb, rownames = FALSE,
                   options = list(pageLength = page, dom = "frtip",
                                  columnDefs = list(list(className = "dt-center",
                                                         targets = "_all"))))
    for (lv in intersect(c("Leverage", "CPT Lev", "FLEX Lev"), names(tb))) {
      d <- d %>% formatStyle(lv,
                             color = styleInterval(0, c("#dc3545", "#28a745")),
                             fontWeight = "bold")
    }
    if (!is.null(bar_col) && bar_col %in% names(tb)) {
      d <- d %>% formatStyle(bar_col,
                             background = styleColorBar(c(0, 100), "#FFE500"),
                             backgroundSize = "100% 90%",
                             backgroundRepeat = "no-repeat",
                             backgroundPosition = "center")
    }
    # Salary-style columns read better without decimals.
    whole <- intersect(c("Salary", "Salary ($K)", "Start", "DKMax", "Slots", "Locked Slots",
                         "Total Slots", "Times Used", "Contest Players",
                         "Revealed", "Entries"), num_cols)
    frac  <- setdiff(num_cols, whole)
    if (length(frac))  d <- d %>% formatRound(frac, digits)
    if (length(whole)) d <- d %>% formatRound(whole, 0)
    d
  }

  # Turn the exposure table into display columns for whichever sport is loaded.
  # slot_split = TRUE (showdown only) replaces the pooled Your % / Field % pair
  # with captain and flex columns; the pooled numbers are still one radio click
  # away.
  display_exposure <- function(tb, slot_split = FALSE, include_proj = !slot_split) {
    if (is.null(tb) || !nrow(tb)) return(NULL)
    ex   <- extra_cols()
    keep <- c("Player", unname(ex))
    d    <- tb[, keep, with = FALSE]
    setnames(d, c(adapter()$entity, names(ex)))
    # Salary reads as $K, whole numbers - no cents, no thousands separators.
    if ("Salary" %in% names(d)) {
      d[, Salary := round(Salary / 1000)]
      setnames(d, "Salary", "Salary ($K)")
    }
    if (any(!tb$Revealed)) d[, Status := fifelse(tb$Revealed, "Revealed", "Locked")]
    split_on <- slot_split && all(c("CptUserExp", "FlexUserExp") %in% names(tb))
    if (split_on) {
      d[, `Your CPT %`  := tb$CptUserExp]
      d[, `Field CPT %` := tb$CptFieldExp]
      d[, `CPT Lev`     := tb$CptLeverage]
      d[, `Your FLEX %`  := tb$FlexUserExp]
      d[, `Field FLEX %` := tb$FlexFieldExp]
      d[, `FLEX Lev`     := tb$FlexLeverage]
    } else {
      d[, `Your %`  := tb$UserExp]
      d[, `Field %` := tb$FieldExp]
      d[, Leverage  := tb$Leverage]
    }
    if (include_proj && "ProjOwnPct" %in% names(tb)) {
      d[, `Proj %`        := tb$ProjOwnPct]
      d[, `Field vs Proj` := tb$FieldVsProj]
    }
    d[, `DK Field %` := tb$DKFieldPct]
    # Show the unscaled score: in Showdown the max across roster rows is the
    # captain's 1.5x figure, which is a lineup-level bonus rather than what
    # the competitor actually scored.
    fp <- if ("BaseFPTS" %in% names(tb)) fifelse(is.na(tb$BaseFPTS), tb$FPTS, tb$BaseFPTS) else tb$FPTS
    # Before a contest scores, DK fills FPTS with zeros - a column of nothing.
    if (any(!is.na(fp) & fp != 0)) d[, FPTS := fp]
    if (split_on) d[order(-`Your CPT %`, -`Your FLEX %`)] else d[order(-`Your %`, -`Field %`)]
  }

  leverage_plot <- function(tb, direction = c("positive", "negative"),
                            value_col = "Leverage") {
    direction <- match.arg(direction)
    if (is.null(tb) || !nrow(tb) || !value_col %in% names(tb)) return(plotly_empty())
    d <- tb[!is.na(get(value_col))]
    d <- if (direction == "positive") d[get(value_col) > 0] else d[get(value_col) < 0]
    if (!nrow(d)) return(plotly_empty())
    d <- if (direction == "positive") d[order(-get(value_col))] else d[order(get(value_col))]
    d <- head(d, 30)
    lab <- data.table(Label = as.character(d$Player),
                      Value = as.numeric(d[[value_col]]))
    # Keep the bars in the order we just sorted them into.
    lab[, Label := factor(Label, levels = Label)]
    p <- plot_ly(lab, x = ~Label, y = ~Value, type = "bar",
                 marker = list(color = if (direction == "positive") "#28a745" else "#dc3545",
                               line = list(color = "#FFE500", width = 1)),
                 hovertemplate = paste0("<b>%{x}</b><br>", value_col,
                                        ": %{y:.1f}%<extra></extra>"))
    gt_layout(p)
  }

  # --- My Sweat -----------------------------------------------------------

  output$my_sweat_content <- renderUI({
    if (is.null(input$file)) {
      return(div(class = "gt-note",
                 "Upload a DraftKings contest-standings CSV to get started. The sport is ",
                 "detected from the lineup format; add that sport's sim input file to ",
                 "unlock its metadata breakdowns."))
    }
    li <- lock_info()
    # Some sports - NASCAR above all - lock the whole field at once, so an
    # export pulled before the green flag has nothing revealed at all. Say so
    # rather than showing a page of empty tables.
    if (li$locked_slot_pct >= 100) {
      return(div(class = "gt-note warn",
        paste0("Every roster spot in this export is still LOCKED - DraftKings has not ",
               "revealed any lineups yet. Pull the contest export again once the ",
               "event is under way and this page fills in.")))
    }
    ex <- exposure(); pl <- players()
    ad <- adapter(); dims <- group_dims()

    filters <- lapply(seq_along(dims), function(i) {
      vals <- sort(unique(pl[[dims[i]]]))
      vals <- vals[!is.na(vals)]
      column(max(3, floor(12 / max(length(dims) + 1, 1))),
             selectizeInput(paste0("filter_", i), names(dims)[i],
                            choices = vals, selected = NULL, multiple = TRUE,
                            options = list(placeholder = paste("All", names(dims)[i]))))
    })

    tagList(
      fluidRow(
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Your Entries"),
                      div(class = "stat-value", ex$n_user))),
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Field Entries"),
                      div(class = "stat-value", ex$n_field))),
        column(3, div(class = "stat-box",
                      div(class = "stat-label", paste(ad$entity, "Pool")),
                      div(class = "stat-value", nrow(pl)))),
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Roster Slots Locked"),
                      div(class = "stat-value", pct(li$locked_slot_pct, 0))))
      ),

      if (li$locked_slot_pct > 0) div(class = "gt-note",
        paste0(round(li$locked_slot_pct), "% of roster slots in this contest are still ",
               "LOCKED because those games have not started. DraftKings hides those ",
               "players in every lineup - yours included - so their exposure cannot be ",
               "computed yet",
               if (length(li$locked_games))
                 paste0(" (still to play: ", paste(li$locked_games, collapse = ", "), ")")
               else "",
               ". Every number below is computed from revealed slots only, on both your ",
               "side and the field's, so the comparison stays fair. See the Lock Status tab.")),

      if (length(dims) || li$locked_slot_pct > 0) div(class = "filter-bar",
          fluidRow(
            filters,
            if (li$locked_slot_pct > 0)
              column(3, div(style = "margin-top: 28px;",
                            checkboxInput("hide_locked", "Hide locked", value = FALSE)))
          )
      ),

      h4(paste(ad$entity, "Exposure vs Field")),
      if ("CPT" %in% ad$slots) tagList(
        div(class = "filter-bar",
            radioButtons("sd_exposure_view", NULL, inline = TRUE,
                         choices = c("Captain / Flex" = "split", "Pooled (any slot)" = "pooled"),
                         selected = "split")),
        p(style = "color:#CCCCCC;",
          "Captain / Flex splits each lineup count by the slot the player filled; ",
          "Pooled counts him once per lineup in either slot.")
      ) else p(style = "color:#CCCCCC;",
        paste0("One count per lineup regardless of which slot the ", tolower(ad$entity),
               " fills - RB, WR, TE and FLEX (or CPT and UTIL) are pooled together.")),
      DTOutput("exposure_table"),

      if ("CPT" %in% ad$slots && "ProjOwnPct" %in% names(ex$tbl)) tagList(
        br(),
        h4("Projections"),
        p(style = "color:#CCCCCC;",
          "Points and ownership projections from your input sheet, with how far ",
          "the field landed from them."),
        DTOutput("proj_table")
      ),

      br(),
      h4("Positive Leverage"),
      plotlyOutput("positive_leverage_plot", height = "430px"),

      br(),
      h4("Negative Leverage"),
      plotlyOutput("negative_leverage_plot", height = "430px"),

      if ("ProjOwnPct" %in% names(ex$tbl)) tagList(
        br(),
        h4("Field vs Projected Ownership"),
        p(style = "color:#CCCCCC;",
          "Where the field actually landed relative to the projected ownership in your ",
          "input sheet. Green means the field came in over projection, red means under."),
        plotlyOutput("field_vs_proj_plot", height = "430px")
      )
    )
  })

  output$exposure_table <- renderDT({
    tb <- filtered_exposure(); req(tb)
    showdown <- "CPT" %in% adapter()$slots
    split <- showdown && identical(input$sd_exposure_view, "split")
    # Showdown keeps projections out of the exposure table entirely - they get
    # their own section below.
    gt_table(display_exposure(tb, slot_split = split, include_proj = !showdown),
             bar_col = if (split) "Your CPT %" else "Your %")
  })

  # Projections section - showdown only. Keeps the exposure table lean and puts
  # every projection-derived number (points, own %, captain own %, field-vs-proj)
  # in one place.
  output$proj_table <- renderDT({
    tb <- filtered_exposure(); req(tb, "CPT" %in% adapter()$slots)
    d <- data.table(Player = tb$Player)
    if ("Proj" %in% names(tb))       d[, Proj := tb$Proj]
    if ("ProjOwnPct" %in% names(tb)) d[, `Proj Own %` := tb$ProjOwnPct]
    if ("CptOwn" %in% names(tb))     d[, `Proj CPT %` := tb$CptOwn]
    d[, `Field Own %` := tb$FieldExp]
    if ("FieldVsProj" %in% names(tb)) d[, `Field vs Proj` := tb$FieldVsProj]
    setnames(d, "Player", adapter()$entity)
    ord <- if ("Proj" %in% names(d)) order(-d$Proj) else order(-d$`Field Own %`)
    gt_table(d[ord], bar_col = if ("Proj Own %" %in% names(d)) "Proj Own %" else NULL)
  })

  output$positive_leverage_plot <- renderPlotly({ leverage_plot(filtered_exposure(), "positive") })
  output$negative_leverage_plot <- renderPlotly({ leverage_plot(filtered_exposure(), "negative") })

  output$field_vs_proj_plot <- renderPlotly({
    tb <- filtered_exposure(); req(tb, "FieldVsProj" %in% names(tb))
    d <- tb[!is.na(FieldVsProj)][order(-FieldVsProj)]
    if (!nrow(d)) return(plotly_empty())
    d <- data.table(Label = as.character(d$Player), Value = d$FieldVsProj)
    d[, Label := factor(Label, levels = Label)]
    p <- plot_ly(d, x = ~Label, y = ~Value, type = "bar",
                 marker = list(color = ~ifelse(Value >= 0, "#28a745", "#dc3545"),
                               line = list(color = "#FFE500", width = 1)),
                 hovertemplate = "<b>%{x}</b><br>Field minus projection: %{y:.1f}%<extra></extra>")
    gt_layout(p, ytitle = "Field % - Projected %")
  })

  # --- Breakdown (the per-sport group views) ------------------------------

  # Average number of roster spots a group fills per lineup, plus how often a
  # lineup has at least one. Locked slots are excluded from both sides so the
  # comparison stays apples to apples.
  group_stats <- function(group_col) {
    ex <- exposure(); ct <- contest(); pl <- players(); req(ex, ct, pl)
    if (!group_col %in% names(pl)) return(NULL)
    s <- merge(ct$slots[Locked == FALSE],
               pl[, c("Player", group_col), with = FALSE], by = "Player")
    s <- s[!is.na(get(group_col))]
    if (!nrow(s)) return(NULL)
    s[, IsUser := EntryIdx %in% ex$user_idx]

    u <- s[IsUser == TRUE,  .(UserN = .N,  UserEntries  = uniqueN(EntryIdx)), by = c(group_col)]
    f <- s[IsUser == FALSE, .(FieldN = .N, FieldEntries = uniqueN(EntryIdx)), by = c(group_col)]
    out <- merge(u, f, by = group_col, all = TRUE)
    for (cl in c("UserN", "UserEntries", "FieldN", "FieldEntries")) out[is.na(get(cl)), (cl) := 0]
    out[, `:=`(UserPer  = UserN  / ex$n_user,
               FieldPer = FieldN / ex$n_field,
               UserAny  = UserEntries  / ex$n_user  * 100,
               FieldAny = FieldEntries / ex$n_field * 100)]
    out[, Leverage := UserPer - FieldPer]
    out[order(-UserPer)]
  }

  output$breakdown_content <- renderUI({
    req(input$file)
    ad <- adapter(); dims <- group_dims()
    if (!length(dims)) {
      return(div(class = "gt-note",
        paste0("No metadata to break down by yet. ",
               if (is.null(ad$read_input))
                 paste0("There is no input-sheet adapter for ", ad$label, " yet.")
               else
                 paste0("Upload the ", ad$label, " sim input file - ", ad$input_hint, "."))))
    }
    tagList(
      div(class = "gt-note",
          paste0("Groups come from the ", ad$label, " input sheet, so a ",
                 tolower(ad$entity), " is counted under his real grouping no matter ",
                 "which roster slot he filled.")),
      div(class = "filter-bar",
          selectInput("group_dim", "Break down by",
                      choices = dims, selected = dims[1], width = "300px")),
      h4("Roster Spots per Lineup"),
      p(style = "color:#CCCCCC;",
        "How many of your roster spots each group fills on average, against the field."),
      DTOutput("group_summary"),
      br(),
      h4("Group Leverage"),
      plotlyOutput("group_plot", height = "430px"),
      br(),
      h4(textOutput("group_detail_title", inline = TRUE)),
      div(class = "filter-bar",
          selectizeInput("group_value", "Show only", choices = NULL, multiple = TRUE,
                         options = list(placeholder = "All groups"), width = "400px")),
      DTOutput("group_detail_table")
    )
  })

  observe({
    dims <- group_dims()
    gd <- input$group_dim
    req(gd, gd %in% dims)
    pl <- players()
    vals <- sort(unique(pl[[gd]]))
    updateSelectizeInput(session, "group_value", choices = vals[!is.na(vals)], selected = character(0))
  })

  output$group_summary <- renderDT({
    gd <- input$group_dim
    # The selector is repopulated when the sport changes, so a value from the
    # previous sport can arrive here before the update lands.
    req(gd, gd %in% group_dims())
    tb <- group_stats(gd); req(tb)
    lbl <- names(group_dims())[match(gd, group_dims())]
    disp <- tb[, .(
      Group              = get(gd),
      `Your per Lineup`  = UserPer,
      `Field per Lineup` = FieldPer,
      Leverage           = Leverage,
      `Your % w/ 1+`     = UserAny,
      `Field % w/ 1+`    = FieldAny
    )]
    setnames(disp, "Group", lbl)
    gt_table(disp, page = 20, digits = 2)
  })

  output$group_plot <- renderPlotly({
    gd <- input$group_dim; req(gd, gd %in% group_dims())
    tb <- group_stats(gd); req(tb)
    d <- tb[order(-Leverage)]
    d <- data.table(Label = as.character(d[[gd]]), Value = d$Leverage)
    d[, Label := factor(Label, levels = Label)]
    p <- plot_ly(d, x = ~Label, y = ~Value, type = "bar",
                 marker = list(color = ~ifelse(Value >= 0, "#28a745", "#dc3545"),
                               line = list(color = "#FFE500", width = 1)),
                 hovertemplate = "<b>%{x}</b><br>Leverage: %{y:.2f} spots/lineup<extra></extra>")
    gt_layout(p, ytitle = "Roster spots per lineup vs field")
  })

  output$group_detail_title <- renderText({
    gd <- input$group_dim; req(gd, gd %in% group_dims())
    lbl <- names(group_dims())[match(gd, group_dims())]
    paste(adapter()$entity, "Detail by", lbl)
  })

  output$group_detail_table <- renderDT({
    ex <- exposure(); gd <- input$group_dim; req(ex, gd)
    tb <- ex$tbl
    if (!gd %in% names(tb)) return(msg_table("No data for that grouping."))
    if (!is.null(input$group_value) && length(input$group_value)) {
      tb <- tb[get(gd) %in% input$group_value]
    }
    gt_table(display_exposure(tb), bar_col = "Your %")
  })

  # --- Lock Status --------------------------------------------------------

  output$lock_content <- renderUI({
    req(input$file)
    li <- lock_info()
    if (li$locked_slot_pct == 0) {
      return(div(class = "gt-note",
        "Nothing is locked - every roster spot in this contest has been revealed, so all ",
        "exposure numbers in this app are complete."))
    }
    tagList(
      h4("Roster Slots Still Locked"),
      p(style = "color:#CCCCCC;",
        "DraftKings hides a player in every lineup - yours included - until his game starts."),
      DTOutput("lock_slot_table"),
      if (!is.null(li$games)) tagList(
        br(), h4("Game Status"), DTOutput("lock_game_table")
      ),
      br(),
      h4("Not Yet Revealed"),
      p(style = "color:#CCCCCC;",
        "Neither your exposure nor the field's can be read out of the export for these ",
        "players. Where DraftKings still reports a %Drafted it is shown; when a whole game ",
        "is locked DK omits it and the column is blank."),
      DTOutput("lock_player_table")
    )
  })

  output$lock_slot_table <- renderDT({
    li <- lock_info(); req(li)
    gt_table(li$slots[, .(Slot,
                          `Total Slots`  = Slots,
                          `Locked Slots` = LockedSlots,
                          `Locked %`     = LockedPct)], page = 12)
  })

  output$lock_game_table <- renderDT({
    li <- lock_info(); req(li, li$games)
    gt_table(li$games[, .(Game, Status,
                          `Contest Players` = ContestPlayers,
                          Revealed = RevealedCount)], page = 15)
  })

  output$lock_player_table <- renderDT({
    li <- lock_info(); req(li)
    lp <- li$locked_players
    if (is.null(lp) || !nrow(lp)) return(msg_table("Everyone in this contest has been revealed."))
    keep <- intersect(c("Player", "InputName", unname(extra_cols()), "DKFieldPct"), names(lp))
    disp <- lp[, keep, with = FALSE]
    if ("DKFieldPct" %in% names(disp)) setnames(disp, "DKFieldPct", "DK Field %")
    if ("InputName" %in% names(disp))  setnames(disp, "InputName", "Player")
    gt_table(disp, page = 25)
  })

  # --- Live Lineups -------------------------------------------------------

  output$live_content <- renderUI({
    req(input$file)
    ad <- adapter()
    if (!isTRUE(has_h2h())) {
      return(div(class = "gt-note",
        paste0("Live Lineups needs head-to-head data - an input sheet with an Opponent ",
               "column - so it can tell who has lost. ",
               if (is.null(ad$read_input))
                 paste0("There is no input-sheet adapter for ", ad$label, " yet.")
               else if (identical(ad$label, "MMA"))
                 "Upload the UFC simulation input file to switch this on."
               else
                 paste0(ad$label, " is not a head-to-head sport, so every lineup stays alive ",
                        "until the event finishes."))))
    }
    ls <- live_state(); req(ls)
    ex <- exposure()
    alive_pct_you   <- length(ls$user_alive) / max(ex$n_user, 1) * 100
    field_alive_pct <- (ls$n_alive - length(ls$user_alive)) /
                        max(ls$n_total - ex$n_user, 1) * 100

    tagList(
      fluidRow(
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Bouts Complete"),
                      div(class = "stat-value", paste0(ls$bouts_done, "/", ls$bouts_total)))),
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Your Lineups Alive"),
                      div(class = "stat-value",
                          paste0(length(ls$user_alive), "/", ex$n_user)))),
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Your Alive %"),
                      div(class = "stat-value", pct(alive_pct_you, 0)))),
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Field Alive %"),
                      div(class = "stat-value", pct(field_alive_pct, 0))))
      ),

      if (isTRUE(ls$dk_agrees)) div(class = "gt-note",
        paste0("Which bouts are complete is confirmed against DraftKings' own count of ",
               "fighters still to compete, and the two agree for every entry here. ",
               "Inside a completed bout the winner is taken to be the higher scorer - ",
               "almost always right, but a loser who goes the distance can out-score a ",
               "first-round finish. That case is the one to override below."))
      else if (identical(ls$dk_agrees, FALSE)) div(class = "gt-note warn",
        paste0("The bouts inferred as complete do NOT line up with DraftKings' count of ",
               "fighters still to compete, so the alive/dead split below is unreliable. ",
               "Set the affected bouts by hand."))
      else div(class = "gt-note",
        paste0("This export carries no remaining-fighter count to check against, so ",
               "results are inferred from per-fighter scoring alone. Override any bout below.")),

      h4("Bout Results"),
      p(style = "color:#CCCCCC;",
        "Set to Auto to use the inferred result, or name the winner yourself."),
      uiOutput("fight_overrides"),

      br(),
      h4("Alive Leaderboard"),
      p(style = "color:#CCCCCC;",
        "The contest leaderboard with dead lineups removed. Your entries are marked."),
      DTOutput("alive_leaderboard"),

      br(),
      h4("Your Lineups"),
      DTOutput("your_live_lineups"),

      br(),
      h4("Exposure Among Alive Lineups"),
      p(style = "color:#CCCCCC;",
        paste0("Recomputed over surviving lineups only - who you still have live versus ",
               "what the surviving field still has live.")),
      DTOutput("alive_exposure_table")
    )
  })

  output$fight_overrides <- renderUI({
    st <- derived_fights(); req(st)
    bouts <- unique(st$Fight)
    fluidRow(
      lapply(bouts, function(b) {
        rows <- st[Fight == b]
        auto <- rows[Outcome == "Won", Player]
        lbl  <- if (length(auto)) paste0(b, "  (auto: ", auto[1], ")") else paste0(b, "  (auto: not started)")
        column(4, selectInput(paste0("fight_", make.names(b)), lbl,
                              choices = c("Auto", "Not started", rows$Player),
                              selected = "Auto", width = "100%"))
      })
    )
  })

  output$alive_leaderboard <- renderDT({
    ct <- contest(); ls <- live_state(); ex <- exposure()
    req(ct, ls, ex)
    lb <- ct$entries[EntryIdx %in% ls$alive_idx][order(-Points)]
    if (!nrow(lb)) return(msg_table("No lineups are still alive."))
    lb <- head(lb, 200)
    disp <- lb[, .(`Alive Rank` = seq_len(.N),
                   User = fifelse(Username == input$username, paste0("** ", Username), Username),
                   Points,
                   `Left` = Remaining,
                   Lineup)]
    datatable(disp, rownames = FALSE,
              options = list(pageLength = 25, dom = "frtip",
                             columnDefs = list(list(width = "45%", targets = 4)))) %>%
      formatRound("Points", 2) %>%
      formatStyle("User", target = "row",
                  backgroundColor = styleEqual(paste0("** ", input$username), "#3a3a1a"))
  })

  output$your_live_lineups <- renderDT({
    ct <- contest(); ls <- live_state(); ex <- exposure()
    req(ct, ls, ex)
    mine <- ct$entries[EntryIdx %in% ex$user_idx]
    if (!nrow(mine)) return(msg_table("No entries for that username."))
    mine[, Status := fifelse(EntryIdx %in% ls$alive_idx, "ALIVE", "dead")]
    disp <- mine[order(-Points), .(Status, Points, Left = Remaining, Lineup)]
    datatable(disp, rownames = FALSE,
              options = list(pageLength = 25, dom = "frtip",
                             columnDefs = list(list(width = "55%", targets = 3)))) %>%
      formatRound("Points", 2) %>%
      formatStyle("Status",
                  color = styleEqual(c("ALIVE", "dead"), c("#28a745", "#dc3545")),
                  fontWeight = "bold")
  })

  output$alive_exposure_table <- renderDT({
    ct <- contest(); pl <- players(); ls <- live_state(); ex <- exposure()
    req(ct, pl, ls, ex)
    user_alive  <- ls$user_alive
    field_alive <- setdiff(ls$alive_idx, ex$user_idx)
    if (!length(user_alive) && !length(field_alive)) {
      return(msg_table("Nothing is still alive."))
    }
    s <- ct$slots[Locked == FALSE]
    u <- s[EntryIdx %in% user_alive,  .(UserN  = .N), by = Player]
    f <- s[EntryIdx %in% field_alive, .(FieldN = .N), by = Player]
    tb <- merge(u, f, by = "Player", all = TRUE)
    tb[is.na(UserN), UserN := 0][is.na(FieldN), FieldN := 0]
    keep <- intersect(c("Player", unname(extra_cols())), names(pl))
    tb <- merge(tb, pl[, keep, with = FALSE], by = "Player", all.x = TRUE)
    tb[, `:=`(UserExp  = if (length(user_alive))  UserN  / length(user_alive)  * 100 else NA_real_,
              FieldExp = if (length(field_alive)) FieldN / length(field_alive) * 100 else NA_real_)]
    tb[, Leverage := UserExp - FieldExp]
    # A fighter who has already lost cannot appear in a surviving lineup.
    tb <- tb[!Player %in% ls$losers]

    ex_cols <- extra_cols()
    disp <- tb[, c("Player", unname(ex_cols)), with = FALSE]
    setnames(disp, c(adapter()$entity, names(ex_cols)))
    disp[, `Your Alive %`  := tb$UserExp]
    disp[, `Field Alive %` := tb$FieldExp]
    disp[, Leverage        := tb$Leverage]
    gt_table(disp[order(-`Your Alive %`, -`Field Alive %`)], bar_col = "Your Alive %")
  })

  # --- Combo Analysis -----------------------------------------------------

  # In showdown the combo is slot-qualified: "X (CPT) + Y (FLEX)" is a different
  # question from "X and Y anywhere". Non-showdown sports keep plain names.
  combo_showdown <- reactive("CPT" %in% adapter()$slots)

  output$combo_content <- renderUI({
    req(input$file)
    pl <- players(); ad <- adapter()
    revealed <- sort(pl[Revealed == TRUE, Player])
    choices <- if (isTRUE(combo_showdown()))
      sort(c(paste0(revealed, " (CPT)"), paste0(revealed, " (FLEX)"))) else revealed
    tagList(
      div(class = "well",
          h4(paste("Select", ad$entity, "Combination"), style = "margin-top: 0;"),
          p(paste0("Shows the lineups that contain every selected ", tolower(ad$entity),
                   if (isTRUE(combo_showdown()))
                     " in the chosen slot (CPT or FLEX)." else ".",
                   " Locked players cannot be used here - nobody's lineup shows them yet."),
            style = "color: #CCCCCC;"),
          selectizeInput("combo_players", NULL, choices = choices, selected = NULL,
                         multiple = TRUE, width = "100%",
                         options = list(placeholder = "Start typing names...",
                                        maxOptions = 5000))
      ),
      uiOutput("combo_results")
    )
  })

  combo_sets <- reactive({
    ct <- contest(); req(ct)
    if (isTRUE(combo_showdown())) {
      ct$slots[Locked == FALSE, .(Players = list(
        paste0(Player, " (", fifelse(Slot == "CPT", "CPT", "FLEX"), ")"))), by = EntryIdx]
    } else {
      ct$slots[Locked == FALSE, .(Players = list(Player)), by = EntryIdx]
    }
  })

  combo_hits <- reactive({
    ex <- exposure(); req(ex)
    sel <- input$combo_players
    req(length(sel) > 0)
    cs <- combo_sets()
    cs[, Hit := vapply(Players, function(p) all(sel %in% p), logical(1))]
    hit_idx <- cs[Hit == TRUE, EntryIdx]
    list(user  = intersect(hit_idx, ex$user_idx),
         field = setdiff(hit_idx, ex$user_idx))
  })

  output$combo_results <- renderUI({
    req(length(input$combo_players) > 0)
    tagList(
      h4(paste("Combo:", paste(input$combo_players, collapse = " + "))),
      fluidRow(
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Your Lineups"),
                      div(class = "stat-value", textOutput("combo_user_count", inline = TRUE)))),
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Your %"),
                      div(class = "stat-value", textOutput("combo_user_pct", inline = TRUE)))),
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Field Lineups"),
                      div(class = "stat-value", textOutput("combo_field_count", inline = TRUE)))),
        column(3, div(class = "stat-box",
                      div(class = "stat-label", "Field %"),
                      div(class = "stat-value", textOutput("combo_field_pct", inline = TRUE))))
      ),
      br(),
      h4("Exposure Inside Combo Lineups"),
      DTOutput("combo_exposure_table")
    )
  })

  output$combo_user_count  <- renderText({ length(combo_hits()$user) })
  output$combo_field_count <- renderText({ length(combo_hits()$field) })
  output$combo_user_pct    <- renderText({ pct(length(combo_hits()$user)  / exposure()$n_user  * 100) })
  output$combo_field_pct   <- renderText({ pct(length(combo_hits()$field) / exposure()$n_field * 100) })

  output$combo_exposure_table <- renderDT({
    ct <- contest(); pl <- players(); ch <- combo_hits()
    req(ct, pl, ch)
    nu <- length(ch$user); nf <- length(ch$field)
    if (nu == 0 && nf == 0) return(msg_table("No lineup contains that combination."))

    s <- ct$slots[Locked == FALSE]
    u <- s[EntryIdx %in% ch$user,  .(UserN  = .N), by = Player]
    f <- s[EntryIdx %in% ch$field, .(FieldN = .N), by = Player]
    tb <- merge(u, f, by = "Player", all = TRUE)
    tb[is.na(UserN), UserN := 0][is.na(FieldN), FieldN := 0]
    keep <- intersect(c("Player", unname(extra_cols())), names(pl))
    tb <- merge(tb, pl[, keep, with = FALSE], by = "Player", all.x = TRUE)
    tb[, `:=`(UserExp  = if (nu > 0) UserN  / nu * 100 else NA_real_,
              FieldExp = if (nf > 0) FieldN / nf * 100 else NA_real_)]
    tb[, Leverage := UserExp - FieldExp]

    ex_cols <- extra_cols()
    disp <- tb[, c("Player", unname(ex_cols)), with = FALSE]
    setnames(disp, c(adapter()$entity, names(ex_cols)))
    disp[, `Your %`  := tb$UserExp]
    disp[, `Field %` := tb$FieldExp]
    disp[, Leverage  := tb$Leverage]
    gt_table(disp[order(-`Your %`, -`Field %`)], bar_col = "Your %")
  })

  # --- Dupe Analysis ------------------------------------------------------

  output$dupe_analysis_content <- renderUI({
    req(input$file)
    li <- lock_info()
    tagList(
      if (li$locked_slot_pct > 0) div(class = "gt-note warn",
          "Duplication is measured on the lineup string DraftKings exports. While games are ",
          "still locked, two different lineups can serialise identically, so these counts ",
          "are an upper bound until every game has started."),
      h4("Your Lineup Duplication"),
      plotlyOutput("user_dupe_chart", height = "400px"),
      br(),
      h4("Most Duplicated Lineups in the Contest"),
      DTOutput("dupe_table")
    )
  })

  output$user_dupe_chart <- renderPlotly({
    ct <- contest(); u <- input$username
    req(ct, u, nzchar(u))
    counts <- ct$entries[, .N, by = Lineup]
    mine   <- merge(ct$entries[Username == u, .(Lineup)], counts, by = "Lineup")
    if (!nrow(mine)) return(plotly_empty())
    dist <- mine[, .(Lineups = .N), by = .(Dupes = N)][order(Dupes)]
    dist[, Dupes := factor(Dupes, levels = Dupes)]
    p <- plot_ly(dist, x = ~Dupes, y = ~Lineups, type = "bar",
                 marker = list(color = "#FFE500", line = list(color = "#FFA500", width = 1)),
                 hovertemplate = "<b>%{x} copies</b><br>Your lineups: %{y}<extra></extra>")
    gt_layout(p, ytitle = "Number of Your Lineups", xtitle = "Copies in Contest") %>%
      layout(xaxis = list(tickangle = 0))
  })

  output$dupe_table <- renderDT({
    ct <- contest(); req(ct)
    counts <- ct$entries[, .N, by = Lineup][order(-N)]
    top <- head(counts[N > 1], 50)
    if (!nrow(top)) return(msg_table("No duplicated lineups."))
    disp <- top[, .(Lineup,
                    `Times Used`   = N,
                    `% of Contest` = N / nrow(ct$entries) * 100)]
    datatable(disp, rownames = FALSE,
              options = list(pageLength = 25, dom = "frtip",
                             columnDefs = list(list(width = "60%", targets = 0),
                                               list(className = "dt-center", targets = 1:2)))) %>%
      formatRound("% of Contest", 2) %>%
      formatStyle("Times Used",
                  background = styleColorBar(c(0, max(top$N)), "#FFE500"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
