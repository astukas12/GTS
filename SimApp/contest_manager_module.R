# ============================================================================
# CONTEST ENTRIES MODULE
# Golden Ticket Sims
#
# The customer downloads their entries CSV from DraftKings (Lineups -> Upload
# Lineups / Edit Entries -> Download CSV), loads it here, the app fills every
# entry from the Portfolio Builder, and they upload the file back to DK.
# The same file also gives the exposure report: dollars per player, per
# contest type, per player combination and per team stack.
#
# One DK entries file is one slate and one game style (classic OR showdown),
# and it carries the contest's own player list (Name + ID, Roster Position,
# Game Info) to the right of the entries. Everything here is driven off that
# list, so no sport needs its own branch:
#   - a portfolio lineup is a set of DK ids (the app's normal download already
#     knows each sport's id quirks: CPT ids, NHL UTIL ids, tennis A-CPT ...)
#   - which slot each id may fill comes from its Roster Position
#   - a lineup whose ids are not in the contest's list is from another slate
#
# Data model (what late swap builds on):
#   entries  one row per entry: EntryID, ContestName, ContestID, EntryFee,
#            Fee (numeric), ContestType, MaxEntries
#   cells    one row per entry x roster slot: EntryID, SlotIdx, Slot, ID,
#            Locked. A slot DK has locked arrives as "Name (ID) (LOCKED)".
#   pool     the contest's player list: ID, Name, NameID, Position,
#            RosterPosition, Salary, Game, Team, Start (POSIXct, ET)
# Late swap stage 1 only has to: freeze cells whose player's Start has
# passed, re-sim the rest, and rewrite the unlocked cells of each entry with
# write_dk_upload(). fill_entries() already never touches a locked entry, and
# arrange_lineup() already puts the latest-starting player in the flex slot.
#
# Replaces the unfinished DKEntries-sheet version of this file (Andrew, 2026):
# the entries now come from DK's own CSV instead of a tab in the input sheet.
# ============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

CM_CONTEST_TYPES <- c("Cash", "Satellite", "SE / 3-Max", "Mid MME", "Large MME")


# ============================================================================
# READING DK'S ENTRIES CSV
# ============================================================================

# "Josh Allen (12345)", "Josh Allen (12345) (LOCKED)", "12345" or "" -> id.
cm_cell_id <- function(x) {
  x  <- trimws(as.character(x))
  id <- ifelse(grepl("^[0-9]+$", x), x,
               sub("^.*\\(([0-9]+)\\).*$", "\\1",
                   ifelse(grepl("\\([0-9]+\\)", x), x, NA_character_)))
  id[is.na(x) | x == ""] <- NA_character_
  id
}

cm_cell_locked <- function(x) grepl("\\(LOCKED\\)", as.character(x), ignore.case = TRUE)

# DK Game Info: "BUF@MIA 09/28/2025 01:00PM ET". Odd rows ("Cancelled",
# "In Progress") return NA start, which reads as "time unknown".
cm_game_start <- function(game_info) {
  ts <- regmatches(game_info, regexpr("[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{1,2}:[0-9]{2}[AP]M",
                                      game_info))
  out <- rep(as.POSIXct(NA, tz = "America/New_York"), length(game_info))
  hit <- grepl("[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{1,2}:[0-9]{2}[AP]M", game_info)
  if (any(hit)) out[hit] <- as.POSIXct(ts, format = "%m/%d/%Y %I:%M%p", tz = "America/New_York")
  out
}

#' Classify a DK contest from its name.
#' DK writes the entry limit into the name: "[Single Entry]", "[3 Entry Max]",
#' "[150 Entry Max]". Cash contests say so: Double Up, 50/50, Head-to-Head,
#' Triple Up ... Anything else is a tournament, sized by its entry limit.
#' @return data.table(ContestType, MaxEntries)
classify_contest <- function(contest_name) {
  nm  <- as.character(contest_name)
  mx  <- suppressWarnings(as.integer(sub("^.*\\[([0-9,]+) Entry Max\\].*$", "\\1",
                                         gsub(",", "", nm))))
  mx[!grepl("Entry Max\\]", nm, ignore.case = TRUE)] <- NA_integer_
  mx[grepl("Single Entry|Head-to-Head|H2H", nm, ignore.case = TRUE)] <- 1L

  is_cash <- grepl("Double Up|50/50|Head-to-Head|H2H|Triple Up|Quadruple Up|Quintuple Up|Multiplier| vs\\.? ",
                   nm, ignore.case = TRUE)
  is_sat  <- grepl("Satellite|Qualifier|Ticket|Super Sat", nm, ignore.case = TRUE)

  type <- ifelse(is_cash, "Cash",
          ifelse(is_sat,  "Satellite",
          ifelse(!is.na(mx) & mx <= 3L,  "SE / 3-Max",
          ifelse(!is.na(mx) & mx <= 20L, "Mid MME", "Large MME"))))
  data.table(ContestType = type, MaxEntries = mx)
}

#' Read a DraftKings entries CSV.
#'
#' Layout (as DK writes it): row 1 is the header -- Entry ID, Contest Name,
#' Contest ID, Entry Fee, one column per roster slot (RB repeats), a blank
#' column, then instructions. Further down, starting in the column after the
#' blank, sits the player list with its own header row (Position, Name + ID,
#' Name, ID, Roster Position, Salary, Game Info, TeamAbbrev, AvgPointsPerGame).
#'
#' @param file_path path to the CSV
#' @return list(entries, cells, pool, slots) -- see the data model above
read_dk_entries <- function(file_path) {
  con   <- file(file_path, encoding = "UTF-8-BOM")    # DK writes a byte-order mark
  lines <- readLines(con, warn = FALSE); close(con)
  lines <- lines[nzchar(trimws(gsub(",", "", lines)))]
  if (length(lines) < 2) stop("The file is empty.")

  n_f <- suppressWarnings(max(count.fields(textConnection(lines), sep = ",",
                                           quote = "\"", comment.char = ""),
                              na.rm = TRUE))
  raw <- read.csv(text = lines, header = FALSE, colClasses = "character",
                  fill = TRUE, col.names = paste0("V", seq_len(n_f)),
                  na.strings = character(0), quote = "\"", comment.char = "",
                  check.names = FALSE, strip.white = TRUE, encoding = "UTF-8")
  m <- unname(as.matrix(raw))
  m[is.na(m)] <- ""

  hdr <- m[1, ]
  if (!identical(tolower(hdr[1]), "entry id"))
    stop("This doesn't look like a DraftKings entries file (first column should be 'Entry ID').")
  fee_col <- match("entry fee", tolower(hdr))
  if (is.na(fee_col)) stop("No 'Entry Fee' column in the header row.")
  slot_end <- fee_col
  while (slot_end < length(hdr) && nzchar(hdr[slot_end + 1]) &&
         !grepl("^instructions", hdr[slot_end + 1], ignore.case = TRUE))
    slot_end <- slot_end + 1
  if (slot_end == fee_col) stop("No roster slot columns after 'Entry Fee'.")
  slot_cols <- (fee_col + 1):slot_end
  slots     <- hdr[slot_cols]

  # ── Player list ──────────────────────────────────────────────────────────
  hit <- which(m == "Name + ID", arr.ind = TRUE)
  if (!nrow(hit)) stop("The file has no player list (no 'Name + ID' header). Download it again from DK's upload page.")
  r0 <- hit[1, "row"]; c0 <- hit[1, "col"]
  ph <- m[r0, ]
  pcol <- function(name) { j <- which(ph == name & seq_along(ph) >= c0 - 1L)[1]; if (is.na(j)) NULL else j }
  need <- c("Name", "ID", "Roster Position")
  miss <- need[vapply(need, function(n) is.null(pcol(n)), logical(1))]
  if (length(miss)) stop("Player list is missing: ", paste(miss, collapse = ", "))
  prow <- if (r0 < nrow(m)) (r0 + 1):nrow(m) else integer(0)
  get  <- function(name) if (is.null(j <- pcol(name))) rep("", length(prow)) else m[prow, j]
  pool <- data.table(
    ID             = get("ID"),
    Name           = get("Name"),
    NameID         = get("Name + ID"),
    Position       = get("Position"),
    RosterPosition = get("Roster Position"),
    Salary         = suppressWarnings(as.numeric(gsub("[^0-9.]", "", get("Salary")))),
    GameInfo       = get("Game Info"),
    Team           = get("TeamAbbrev"),
    AvgPts         = suppressWarnings(as.numeric(get("AvgPointsPerGame")))
  )
  pool <- pool[grepl("^[0-9]+$", ID)]
  pool <- unique(pool, by = "ID")
  pool[!nzchar(NameID), NameID := paste0(Name, " (", ID, ")")]
  pool[, Game  := sub(" .*$", "", GameInfo)]
  pool[, Start := cm_game_start(GameInfo)]

  # ── Entries ──────────────────────────────────────────────────────────────
  erow <- which(grepl("^[0-9]+$", m[, 1]))
  erow <- erow[erow > 1]
  if (!length(erow)) stop("No entries found. Enter your contests on DK first, then download the entries CSV.")
  entries <- data.table(
    EntryID     = m[erow, 1],
    ContestName = m[erow, 2],
    ContestID   = m[erow, 3],
    EntryFee    = m[erow, fee_col]
  )
  entries[, Fee := suppressWarnings(as.numeric(gsub("[^0-9.]", "", EntryFee)))]
  entries[is.na(Fee), Fee := 0]
  entries <- cbind(entries, classify_contest(entries$ContestName))

  cell_txt <- m[erow, slot_cols, drop = FALSE]
  cells <- data.table(
    EntryID = rep(entries$EntryID, times = length(slots)),
    SlotIdx = rep(seq_along(slots), each = length(erow)),
    Slot    = rep(slots, each = length(erow)),
    ID      = cm_cell_id(as.vector(cell_txt)),
    Locked  = cm_cell_locked(as.vector(cell_txt))
  )
  setorder(cells, EntryID, SlotIdx)

  list(entries = entries, cells = cells, pool = pool, slots = slots)
}


# ============================================================================
# PORTFOLIO -> LINEUPS OF DK IDS
# ============================================================================

#' Turn the app's portfolio upload table (the same table the Portfolio
#' Builder's DOWNLOAD PORTFOLIO writes, cells "Name (ID)") into id sets.
#' @param upload_tbl data.table from the app's portfolio upload formatter
#' @param pool       the entries file's player list
#' @param n_slots    roster size of the contest
#' @return list(lineups = data.table(LineupKey, IDs (list), Build, metrics),
#'              n_total, n_off_slate)
lineups_from_upload <- function(upload_tbl, pool, n_slots) {
  dt <- as.data.table(upload_tbl)
  if (!nrow(dt)) return(list(lineups = NULL, n_total = 0L, n_off_slate = 0L))
  id_cols <- which(vapply(dt, function(v) is.character(v) &&
                            mean(grepl("\\([0-9]+\\)\\s*$", v)) > 0.5, logical(1)))
  ids <- lapply(seq_len(nrow(dt)), function(i)
    cm_cell_id(unlist(dt[i, id_cols, with = FALSE], use.names = FALSE)))
  ok_len  <- vapply(ids, function(v) length(v) == n_slots && !anyNA(v), logical(1))
  on_pool <- vapply(ids, function(v) all(v %in% pool$ID), logical(1))
  keep    <- ok_len & on_pool

  metric_cols <- intersect(c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
                             "AvgOwn", "TotalSalary"), names(dt))
  out <- data.table(Build = if ("Build" %in% names(dt)) as.character(dt$Build) else "Portfolio")
  if (length(metric_cols)) out <- cbind(out, dt[, metric_cols, with = FALSE])
  out[, IDs := ids]
  out <- out[keep]
  out[, LineupKey := vapply(IDs, function(v) paste(sort(v), collapse = "-"), character(1))]
  # The same roster drawn into two builds is one lineup; keep its first build.
  out <- unique(out, by = "LineupKey")
  list(lineups = out, n_total = nrow(dt), n_off_slate = sum(ok_len & !on_pool),
       n_bad_shape = sum(!ok_len))
}


# ============================================================================
# SLOT ARRANGEMENT
# ============================================================================

#' Put a lineup's ids into the contest's roster slots.
#'
#' Each id may fill the slots its Roster Position lists ("RB/FLEX",
#' "PG/G/UTIL", "CPT"). Among the legal arrangements it picks the one that
#' puts the LATEST-starting players in the widest slots (FLEX, UTIL, G, F):
#' that keeps late-swap options open, which is how DFS players arrange by
#' hand.
#' @return character ids in slot order, or NULL if no legal arrangement
arrange_lineup <- function(ids, slots, pool) {
  n  <- length(slots)
  if (length(ids) != n) return(NULL)
  pidx <- match(ids, pool$ID)
  if (anyNA(pidx)) return(NULL)
  elig_pos <- strsplit(pool$RosterPosition[pidx], "/", fixed = TRUE)
  ok <- matrix(FALSE, n, n)                     # ok[player, slot]
  for (p in seq_len(n)) ok[p, ] <- slots %in% elig_pos[[p]]

  # Slot width: how many of the pool's roster positions can fill it.
  all_rp <- strsplit(unique(pool$RosterPosition), "/", fixed = TRUE)
  width  <- vapply(slots, function(s) sum(vapply(all_rp, function(r) s %in% r, logical(1))),
                   numeric(1))
  st <- as.numeric(pool$Start[pidx]); st[is.na(st)] <- 0
  late <- rank(st, ties.method = "average")

  best <- NULL; best_score <- -Inf; n_seen <- 0L
  assign <- integer(n); used <- logical(n)
  ord <- order(colSums(ok))                     # most constrained slot first
  rec <- function(k) {
    if (n_seen >= 5000L) return(invisible())
    if (k > n) {
      n_seen <<- n_seen + 1L
      sc <- sum(width * late[assign])
      if (sc > best_score) { best_score <<- sc; best <<- assign }
      return(invisible())
    }
    s <- ord[k]
    for (p in which(ok[, s] & !used)) {
      assign[s] <<- p; used[p] <<- TRUE
      rec(k + 1L)
      used[p] <<- FALSE
    }
  }
  rec(1L)
  if (is.null(best)) return(NULL)
  ids[best]
}


# ============================================================================
# FILLING ENTRIES
# ============================================================================

#' Fill entries with portfolio lineups.
#'
#' Rules, in order:
#'   - an entry with any locked slot is never touched (late swap owns it)
#'   - keep_existing = TRUE leaves already-complete entries alone
#'   - each contest draws from the builds mapped to its contest type
#'     (build_map: named list ContestType -> character builds; NULL/"All" = all)
#'   - no lineup twice in one contest unless the contest has more entries
#'     than the pool has lineups
#'   - across contests, the least-used lineup goes first (ties broken at
#'     random), so exposure follows the portfolio rather than piling onto
#'     its first rows. Biggest contests are filled first.
#' @return list(cells = updated cells, assigned = data.table(EntryID, LineupKey,
#'         Build), messages = character)
fill_entries <- function(parsed, lineups, build_map = NULL, keep_existing = FALSE,
                         seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  ent   <- copy(parsed$entries)
  cells <- copy(parsed$cells)
  msgs  <- character(0)

  st <- cells[, .(AnyLocked = any(Locked), Complete = all(!is.na(ID))), by = EntryID]
  ent <- merge(ent, st, by = "EntryID", sort = FALSE)
  todo <- ent[!AnyLocked & !(keep_existing & Complete)]
  n_locked <- sum(ent$AnyLocked)
  if (n_locked) msgs <- c(msgs, sprintf("%d entries have locked players and were left as they are.", n_locked))

  if (!nrow(todo) || is.null(lineups) || !nrow(lineups))
    return(list(cells = cells, assigned = data.table(EntryID = character(0),
                LineupKey = character(0), Build = character(0)), messages = msgs))

  # Arrange each lineup into slot order once.
  arranged <- lapply(lineups$IDs, arrange_lineup, slots = parsed$slots, pool = parsed$pool)
  bad <- vapply(arranged, is.null, logical(1))
  if (any(bad)) msgs <- c(msgs, sprintf("%d lineups don't fit this contest's roster slots and were skipped.", sum(bad)))
  lu <- copy(lineups)[!bad]
  lu[, Slots := arranged[!bad]]
  if (!nrow(lu)) return(list(cells = cells, assigned = NULL, messages = c(msgs, "No usable lineups.")))

  use <- setNames(integer(nrow(lu)), lu$LineupKey)
  contests <- todo[, .(N = .N, ContestType = ContestType[1]), by = ContestID][order(-N)]
  assigned <- vector("list", nrow(contests))

  for (ci in seq_len(nrow(contests))) {
    cid   <- contests$ContestID[ci]
    ctype <- contests$ContestType[ci]
    eids  <- todo[ContestID == cid, EntryID]
    builds <- build_map[[ctype]]
    cand <- if (is.null(builds) || !length(builds) || "All" %in% builds) seq_len(nrow(lu))
            else which(lu$Build %in% builds)
    if (!length(cand)) {
      msgs <- c(msgs, sprintf("No lineups in the builds picked for %s; %s left unfilled.",
                              ctype, todo[ContestID == cid, ContestName[1]]))
      next
    }
    pick <- integer(0)
    while (length(pick) < length(eids)) {
      left <- setdiff(cand, pick)
      if (!length(left)) left <- cand          # more entries than lineups: wrap
      o <- left[order(use[left], runif(length(left)))]
      take <- o[seq_len(min(length(o), length(eids) - length(pick)))]
      pick <- c(pick, take)
      use[take] <- use[take] + 1L
    }
    if (length(eids) > length(cand))
      msgs <- c(msgs, sprintf("%s: %d entries but only %d lineups, so some lineups repeat.",
                              todo[ContestID == cid, ContestName[1]], length(eids), length(cand)))
    assigned[[ci]] <- data.table(EntryID = eids, LineupKey = lu$LineupKey[pick],
                                 Build = lu$Build[pick])
  }
  assigned <- rbindlist(assigned)
  if (!nrow(assigned)) return(list(cells = cells, assigned = assigned, messages = msgs))

  slot_ids <- lu$Slots[match(assigned$LineupKey, lu$LineupKey)]
  new_cells <- data.table(
    EntryID = rep(assigned$EntryID, each = length(parsed$slots)),
    SlotIdx = rep(seq_along(parsed$slots), times = nrow(assigned)),
    NewID   = unlist(slot_ids, use.names = FALSE)
  )
  cells[new_cells, on = .(EntryID, SlotIdx), ID := i.NewID]
  list(cells = cells, assigned = assigned, messages = msgs)
}


# ============================================================================
# WRITING DK'S UPLOAD FILE
# ============================================================================

#' Write the entries back in DK's upload layout: Entry ID, Contest Name,
#' Contest ID, Entry Fee, then one "Name (ID)" cell per roster slot. Locked
#' cells keep DK's "(LOCKED)" tag so a late-swap upload reads as DK wrote it.
write_dk_upload <- function(parsed, cells, file) {
  q <- function(x) { x <- as.character(x); x[is.na(x)] <- ""
    ifelse(grepl("[\",]", x), paste0("\"", gsub("\"", "\"\"", x), "\""), x) }
  nm <- parsed$pool$NameID[match(cells$ID, parsed$pool$ID)]
  txt <- ifelse(is.na(cells$ID), "", ifelse(is.na(nm), cells$ID, nm))
  txt <- ifelse(cells$Locked & nzchar(txt), paste(txt, "(LOCKED)"), txt)
  wide <- matrix(txt[order(match(cells$EntryID, parsed$entries$EntryID), cells$SlotIdx)],
                 ncol = length(parsed$slots), byrow = TRUE)
  e <- parsed$entries
  body <- cbind(q(e$EntryID), q(e$ContestName), q(e$ContestID), q(e$EntryFee),
                matrix(q(wide), ncol = ncol(wide)))
  out <- c(paste(q(c("Entry ID", "Contest Name", "Contest ID", "Entry Fee", parsed$slots)),
                 collapse = ","),
           apply(body, 1, paste, collapse = ","))
  writeLines(out, file, useBytes = TRUE)
  invisible(file)
}


# ============================================================================
# EXPOSURE
# ============================================================================

#' One row per entry x player, with the entry's dollars and contest type.
#' Works on any cells -- as loaded (lineups already on DK) or after a fill.
entry_players_long <- function(parsed, cells) {
  long <- cells[!is.na(ID), .(EntryID, Slot, ID, Locked)]
  long <- merge(long, parsed$entries[, .(EntryID, ContestID, ContestName, ContestType, Fee)],
                by = "EntryID")
  pl <- parsed$pool[, .(ID, Name, Position, Team, Game, Salary)]
  long <- merge(long, pl, by = "ID", all.x = TRUE)
  long[is.na(Name), Name := ID]
  # Showdown: CPT and FLEX are different ids for one player -- exposure is
  # about the player, so key on name + team and keep the slot for CPT splits.
  long[, PlayerKey := paste(Name, Team)]
  long[]
}

#' Player exposure: entries, % of entries, dollars, % of dollars, plus a
#' dollars column per contest type.
exposure_players <- function(long, n_entries, total_fee) {
  if (!nrow(long)) return(NULL)
  base <- long[, .(Pos = Position[1], Team = Team[1], Salary = max(Salary, na.rm = TRUE),
                   Entries = uniqueN(EntryID), Dollars = sum(Fee),
                   CPT = sum(Slot %in% c("CPT", "MVP", "CAPT"))),
               by = .(PlayerKey, Player = Name)]
  base[!is.finite(Salary), Salary := NA_real_]
  base[, EntryPct  := round(100 * Entries / n_entries, 1)]
  base[, DollarPct := if (total_fee > 0) round(100 * Dollars / total_fee, 1) else NA_real_]
  by_type <- dcast(long, PlayerKey ~ ContestType, value.var = "Fee", fun.aggregate = sum, fill = 0)
  out <- merge(base, by_type, by = "PlayerKey", all.x = TRUE)
  if (all(out$CPT == 0)) out[, CPT := NULL]
  out[, PlayerKey := NULL]
  setcolorder(out, intersect(c("Player", "Pos", "Team", "Salary", "Entries", "EntryPct",
                               "Dollars", "DollarPct", "CPT"), names(out)))
  setorder(out, -Dollars, -Entries)
  out[]
}

#' Player combinations (k = 2 pairs, 3 triples) by dollars and entries.
exposure_combos <- function(long, k = 2L, top = 100L, n_entries, total_fee) {
  if (!nrow(long)) return(NULL)
  per <- long[, .(P = list(sort(unique(Name))), Fee = Fee[1]), by = EntryID]
  per <- per[lengths(P) >= k]
  if (!nrow(per)) return(NULL)
  combos <- rbindlist(lapply(seq_len(nrow(per)), function(i) {
    cm <- combn(per$P[[i]], k)
    data.table(Combo = apply(cm, 2, paste, collapse = " + "), EntryID = per$EntryID[i],
               Fee = per$Fee[i])
  }))
  out <- combos[, .(Entries = .N, Dollars = sum(Fee)), by = Combo]
  out[, EntryPct  := round(100 * Entries / n_entries, 1)]
  out[, DollarPct := if (total_fee > 0) round(100 * Dollars / total_fee, 1) else NA_real_]
  setorder(out, -Dollars, -Entries)
  head(out, top)
}

#' Team stacks: each entry's shape, e.g. "BUF 4 + MIA 2" (teams with 2+).
exposure_stacks <- function(long, n_entries, total_fee) {
  if (!nrow(long) || all(is.na(long$Team) | long$Team == "")) return(NULL)
  per <- long[, .N, by = .(EntryID, Team, Fee)][N >= 2L]
  if (!nrow(per)) return(NULL)
  setorder(per, EntryID, -N, Team)
  sig <- per[, .(Stack = paste(paste(Team, N), collapse = " + "), Fee = Fee[1]), by = EntryID]
  out <- sig[, .(Entries = .N, Dollars = sum(Fee)), by = Stack]
  out[, EntryPct  := round(100 * Entries / n_entries, 1)]
  out[, DollarPct := if (total_fee > 0) round(100 * Dollars / total_fee, 1) else NA_real_]
  setorder(out, -Dollars, -Entries)
  out[]
}

#' One row per contest for the summary table.
summarise_contests <- function(parsed, cells) {
  filled <- cells[, .(Filled = all(!is.na(ID)), Locked = any(Locked)), by = EntryID]
  e <- merge(parsed$entries, filled, by = "EntryID")
  out <- e[, .(Contest = ContestName[1], Type = ContestType[1], MaxEntries = MaxEntries[1],
               Fee = Fee[1], Entries = .N, Dollars = sum(Fee),
               Filled = sum(Filled), Locked = sum(Locked)), by = ContestID]
  setorder(out, -Dollars)
  out[]
}


# ============================================================================
# SHINY UI
# ============================================================================

render_contest_manager_ui <- function() {
  lbl <- function(x) tags$label(x, style = "color:#FFE500;font-weight:700;font-size:12px;")
  tagList(
    fluidRow(
      shinydashboard::box(width = 4, title = "DraftKings Entries", status = "primary",
          solidHeader = TRUE,
          p(style = "color:#888;font-size:12px;",
            "On DraftKings: Lineups, then Upload Lineups / Edit Entries, pick the slate, ",
            "and Download the CSV. Load that file here."),
          fileInput("cm_entries_file", NULL, accept = c(".csv", "text/csv"),
                    buttonLabel = "Load entries CSV"),
          uiOutput("cm_file_status")
      ),
      shinydashboard::box(width = 8, title = "Fill Entries", status = "primary",
          solidHeader = TRUE,
          uiOutput("cm_fill_controls")
      )
    ),
    uiOutput("cm_summary_strip"),
    fluidRow(
      shinydashboard::box(width = 12, title = "Contests", status = "primary",
          solidHeader = TRUE, collapsible = TRUE,
          DTOutput("cm_contests_tbl"))
    ),
    fluidRow(
      shinydashboard::tabBox(id = "cm_exposure_tabs", width = 12,
          title = downloadButton("cm_export_exposure", "Exposure CSV",
                                 style = "font-size:11px;padding:3px 8px;"),
          tabPanel("Players",  DTOutput("cm_exp_players")),
          tabPanel("Pairs",    DTOutput("cm_exp_pairs")),
          tabPanel("Triples",  DTOutput("cm_exp_triples")),
          tabPanel("Team Stacks", DTOutput("cm_exp_stacks")),
          tabPanel("Entries",  DTOutput("cm_entries_tbl"))
      )
    )
  )
}


# ============================================================================
# SERVER
# ============================================================================

#' @param upload_table function(portfolio, platform) -> the app's portfolio
#'        upload table (cells "Name (ID)"); app.R's portfolio_upload_table().
register_contest_manager_observers <- function(input, output, session, rv, upload_table) {

  cm_rv <- reactiveValues(parsed = NULL, cells = NULL, assigned = NULL,
                          file_name = NULL, error = NULL, messages = character(0))

  # DK classic lineups live in the dk portfolio, DK showdown in sd. A showdown
  # entries file has a CPT slot.
  cm_portfolio <- reactive({
    p <- cm_rv$parsed; req(p)
    lps <- if (any(p$slots %in% c("CPT", "CAPT"))) c("sd", "dk") else c("dk", "sd")
    for (lp in lps) {
      port <- rv[[paste0(lp, "_portfolio")]]
      if (is.null(port) || !nrow(port)) next
      platform <- toupper(lp)
      tbl <- tryCatch(upload_table(port, platform), error = function(e) NULL)
      if (is.null(tbl)) next
      res <- lineups_from_upload(tbl, p$pool, length(p$slots))
      if (!is.null(res$lineups) && nrow(res$lineups)) return(c(res, list(lp = lp)))
    }
    NULL
  })

  observeEvent(input$cm_entries_file, {
    f <- input$cm_entries_file; req(f)
    cm_rv$error <- NULL; cm_rv$assigned <- NULL; cm_rv$messages <- character(0)
    parsed <- tryCatch(read_dk_entries(f$datapath), error = function(e) e)
    if (inherits(parsed, "error")) {
      cm_rv$parsed <- NULL; cm_rv$cells <- NULL; cm_rv$error <- conditionMessage(parsed)
      return()
    }
    cm_rv$parsed    <- parsed
    cm_rv$cells     <- parsed$cells
    cm_rv$file_name <- f$name
    cat(sprintf("  [CM] Loaded %d DK entries, %d contests, %d players\n",
                nrow(parsed$entries), uniqueN(parsed$entries$ContestID), nrow(parsed$pool)))
  })

  output$cm_file_status <- renderUI({
    if (!is.null(cm_rv$error))
      return(div(style = "color:#e06c6c;font-size:12px;", icon("exclamation-triangle"), " ", cm_rv$error))
    p <- cm_rv$parsed
    if (is.null(p)) return(NULL)
    div(style = "color:#aaa;font-size:12px;",
        div(strong(cm_rv$file_name)),
        div(sprintf("%s entries in %s contests. Roster: %s.",
                    nrow(p$entries), uniqueN(p$entries$ContestID), paste(p$slots, collapse = " "))),
        div(sprintf("%d players on the slate.", nrow(p$pool))))
  })

  output$cm_fill_controls <- renderUI({
    p <- cm_rv$parsed
    if (is.null(p)) return(p(style = "color:#666;font-size:12px;", "Load a DK entries CSV first."))
    src <- cm_portfolio()
    if (is.null(src))
      return(p(style = "color:#e0a84a;font-size:12px;",
               "No portfolio lineups match this slate yet. Build a portfolio in the Portfolio Builder ",
               "for the same slate and game style (classic or showdown), then come back."))
    builds <- unique(src$lineups$Build)
    types  <- intersect(CM_CONTEST_TYPES, unique(p$entries$ContestType))
    warn <- if (src$n_off_slate > 0)
      div(style = "color:#e0a84a;font-size:11px;margin-bottom:6px;",
          sprintf("%d portfolio lineups use players who aren't in this contest and were left out.",
                  src$n_off_slate))
    tagList(
      div(style = "color:#aaa;font-size:12px;margin-bottom:8px;",
          sprintf("%d unique lineups from the %s portfolio (%d builds).",
                  nrow(src$lineups), toupper(src$lp), length(builds))),
      warn,
      if (length(builds) > 1) tagList(
        tags$label("Builds for each contest type:", style = "color:#FFE500;font-weight:700;font-size:12px;"),
        fluidRow(lapply(types, function(t) column(width = max(2, floor(12 / length(types))),
          selectizeInput(paste0("cm_build_", gsub("[^A-Za-z0-9]", "_", t)), t,
                         choices = c("All", builds), selected = "All", multiple = TRUE))))
      ),
      checkboxInput("cm_keep_existing", "Keep lineups already in the file", value = FALSE),
      div(style = "display:flex;gap:10px;",
          actionButton("cm_fill", "Fill Entries", class = "btn-primary", icon = icon("magic"),
                       style = "font-weight:700;"),
          downloadButton("cm_download_upload", "Download DK Upload File", class = "btn-success",
                         style = "font-weight:700;")),
      if (length(cm_rv$messages))
        div(style = "color:#e0a84a;font-size:11px;margin-top:8px;",
            lapply(cm_rv$messages, div))
    )
  })

  observeEvent(input$cm_fill, {
    p <- cm_rv$parsed; req(p)
    src <- cm_portfolio()
    if (is.null(src)) { showNotification("No portfolio lineups match this slate.", type = "warning"); return() }
    types <- unique(p$entries$ContestType)
    build_map <- setNames(lapply(types, function(t) input[[paste0("cm_build_", gsub("[^A-Za-z0-9]", "_", t))]]),
                          types)
    res <- fill_entries(p, src$lineups, build_map = build_map,
                        keep_existing = isTRUE(input$cm_keep_existing))
    cm_rv$cells    <- res$cells
    cm_rv$assigned <- res$assigned
    cm_rv$messages <- res$messages
    n <- if (is.null(res$assigned)) 0L else nrow(res$assigned)
    showNotification(sprintf("Filled %d of %d entries.", n, nrow(p$entries)),
                     type = if (n == nrow(p$entries)) "message" else "warning")
  })

  output$cm_download_upload <- downloadHandler(
    filename = function() paste0("DKEntries_GTS_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content  = function(file) {
      req(cm_rv$parsed, cm_rv$cells)
      write_dk_upload(cm_rv$parsed, cm_rv$cells, file)
    }
  )

  # ── Exposure (on whatever the entries hold now) ──────────────────────────
  cm_long <- reactive({
    req(cm_rv$parsed, cm_rv$cells)
    entry_players_long(cm_rv$parsed, cm_rv$cells)
  })
  cm_totals <- reactive({
    l <- cm_long()
    e <- cm_rv$parsed$entries[EntryID %in% l$EntryID]
    list(n = max(1L, nrow(e)), fee = sum(e$Fee))
  })

  output$cm_summary_strip <- renderUI({
    p <- cm_rv$parsed; req(p, cm_rv$cells)
    s <- summarise_contests(p, cm_rv$cells)
    tile <- function(label, value, col = "#FFE500")
      div(style = "padding:12px 20px;background:#1a1a1a;border:1px solid #333;border-radius:4px;flex:1;",
          div(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#777;", label),
          div(style = paste0("font-size:22px;font-weight:700;color:", col, ";"), value))
    div(style = "display:flex;gap:16px;margin-bottom:16px;",
        tile("Contests", nrow(s)),
        tile("Entries", sum(s$Entries)),
        tile("Filled", paste0(sum(s$Filled), " / ", sum(s$Entries))),
        tile("Total Investment", paste0("$", formatC(sum(s$Dollars), format = "f", digits = 2, big.mark = ",")),
             "#4A90D9"))
  })

  dt_opts <- function(page = 25) list(pageLength = page, scrollX = TRUE, dom = "ftp")

  output$cm_contests_tbl <- renderDT({
    req(cm_rv$parsed, cm_rv$cells)
    s <- summarise_contests(cm_rv$parsed, cm_rv$cells)
    s[, ContestID := NULL]
    datatable(s, rownames = FALSE, options = dt_opts(20), class = "hover compact") %>%
      formatCurrency(c("Fee", "Dollars"), "$", digits = 2)
  })

  money_tbl <- function(x) {
    if (is.null(x) || !nrow(x)) return(datatable(data.table(Message = "Nothing to show yet.")))
    num <- names(x)[vapply(x, is.numeric, logical(1))]
    dol <- setdiff(intersect(c("Dollars", CM_CONTEST_TYPES), num), character(0))
    d <- datatable(x, rownames = FALSE, options = dt_opts(), class = "hover compact")
    if (length(dol)) d <- formatCurrency(d, dol, "$", digits = 0)
    if ("Salary" %in% num) d <- formatCurrency(d, "Salary", "$", digits = 0)
    if ("Dollars" %in% names(x))
      d <- formatStyle(d, "Dollars",
                       background = styleColorBar(range(c(0, x$Dollars), na.rm = TRUE), "rgba(74,144,217,0.4)"),
                       backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
                       backgroundPosition = "left")
    d
  }

  output$cm_exp_players <- renderDT({
    t <- cm_totals(); money_tbl(exposure_players(cm_long(), t$n, t$fee))
  })
  output$cm_exp_pairs <- renderDT({
    t <- cm_totals(); money_tbl(exposure_combos(cm_long(), 2L, 200L, t$n, t$fee))
  })
  output$cm_exp_triples <- renderDT({
    t <- cm_totals(); money_tbl(exposure_combos(cm_long(), 3L, 200L, t$n, t$fee))
  })
  output$cm_exp_stacks <- renderDT({
    t <- cm_totals(); money_tbl(exposure_stacks(cm_long(), t$n, t$fee))
  })

  output$cm_entries_tbl <- renderDT({
    p <- cm_rv$parsed; req(p, cm_rv$cells)
    c2 <- copy(cm_rv$cells)
    c2[, Txt := p$pool$Name[match(ID, p$pool$ID)]]
    c2[is.na(Txt), Txt := ""]
    c2[Locked == TRUE, Txt := paste0(Txt, " \U0001F512")]
    w <- dcast(c2, EntryID ~ SlotIdx, value.var = "Txt", fun.aggregate = function(v) v[1], fill = "")
    setnames(w, as.character(seq_along(p$slots)), make.unique(p$slots))
    w <- merge(p$entries[, .(EntryID, Contest = ContestName, Type = ContestType)], w, by = "EntryID")
    if (!is.null(cm_rv$assigned) && nrow(cm_rv$assigned))
      w <- merge(w, cm_rv$assigned[, .(EntryID, Build)], by = "EntryID", all.x = TRUE)
    datatable(w, rownames = FALSE, options = dt_opts(), class = "hover compact")
  })

  output$cm_export_exposure <- downloadHandler(
    filename = function() paste0("GTS_Exposure_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content  = function(file) {
      t <- cm_totals(); l <- cm_long()
      parts <- list(Players = exposure_players(l, t$n, t$fee),
                    Pairs   = exposure_combos(l, 2L, 200L, t$n, t$fee),
                    Triples = exposure_combos(l, 3L, 200L, t$n, t$fee),
                    Stacks  = exposure_stacks(l, t$n, t$fee))
      parts <- parts[!vapply(parts, is.null, logical(1))]
      out <- rbindlist(lapply(names(parts), function(n) {
        x <- copy(parts[[n]]); nm1 <- names(x)[1]
        setnames(x, nm1, "Item"); x[, Section := n]; x
      }), fill = TRUE)
      setcolorder(out, "Section")
      fwrite(out, file)
    }
  )

  invisible(list(state = cm_rv, portfolio = cm_portfolio, long = cm_long))
}
# end of contest_manager_module.R
