# ============================================================================
# CONTEST MANAGER MODULE
# Golden Ticket Sims
#
# Handles: reading DKEntries/FDEntries/SDEntries from input file,
# contest grouping + labeling, lineup assignment, DK bulk upload export,
# investment tracking.
# ============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b


# ============================================================================
# ENTRY FILE READER
# Called after load_sport_input() — reads optional entry sheets.
# Returns NULL silently if sheet doesn't exist.
# ============================================================================

#' Try to read an entry sheet from the input file.
#' @param file_path path to the Excel file
#' @param platform  "DK", "FD", or "SD"
#' @return data.table with EntryID, ContestName, ContestID, EntryFee
#'         or NULL if sheet absent / empty
read_entry_sheet <- function(file_path, platform = "DK") {
  sheet_name <- paste0(platform, "Entries")
  sheets     <- tryCatch(readxl::excel_sheets(file_path), error = function(e) character(0))
  if (!sheet_name %in% sheets) return(NULL)

  raw <- tryCatch(
    suppressMessages(readxl::read_excel(file_path, sheet = sheet_name)),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  dt <- as.data.table(raw)

  # Flexible column matching — DK exports use slightly different casing sometimes
  col_map <- list(
    EntryID     = c("Entry ID", "EntryID", "entry_id", "entry id"),
    ContestName = c("Contest Name", "ContestName", "contest_name", "contest name"),
    ContestID   = c("Contest ID", "ContestID", "contest_id", "contest id"),
    EntryFee    = c("Entry Fee", "EntryFee", "entry_fee", "entry fee")
  )

  result <- data.table(
    EntryID     = NA_character_,
    ContestName = NA_character_,
    ContestID   = NA_character_,
    EntryFee    = NA_character_
  )[0]

  for (std_name in names(col_map)) {
    candidates <- col_map[[std_name]]
    found      <- intersect(candidates, names(dt))
    if (length(found) > 0) {
      result[[std_name]] <- as.character(dt[[found[1]]])
    } else {
      result[[std_name]] <- rep(NA_character_, nrow(dt))
    }
  }

  if (nrow(result) == 0) {
    result <- data.table(
      EntryID     = as.character(dt[[1]]),
      ContestName = if (ncol(dt) >= 2) as.character(dt[[2]]) else NA_character_,
      ContestID   = if (ncol(dt) >= 3) as.character(dt[[3]]) else NA_character_,
      EntryFee    = if (ncol(dt) >= 4) as.character(dt[[4]]) else NA_character_
    )
  } else {
    result <- data.table(
      EntryID     = as.character(dt[[intersect(col_map$EntryID,     names(dt))[1] %||% names(dt)[1]]]),
      ContestName = as.character(dt[[intersect(col_map$ContestName, names(dt))[1] %||% names(dt)[2]]]),
      ContestID   = as.character(dt[[intersect(col_map$ContestID,   names(dt))[1] %||% names(dt)[3]]]),
      EntryFee    = as.character(dt[[intersect(col_map$EntryFee,    names(dt))[1] %||% names(dt)[4]]])
    )
  }

  # Drop rows where EntryID is blank/NA — those are reference rows not entry rows
  result <- result[!is.na(EntryID) & nchar(trimws(EntryID)) > 0]
  if (nrow(result) == 0) return(NULL)

  # Parse entry fee to numeric
  result[, EntryFeeNum := as.numeric(gsub("[^0-9.]", "", EntryFee))]
  result[, Platform    := platform]
  result[, ContestType := NA_character_]   # user will label this
  result[, AssignedLineupID := NA_character_]

  result
}


#' Read all entry sheets from the input file.
#' @return named list: DK = data.table or NULL, FD = ..., SD = ...
read_all_entry_sheets <- function(file_path) {
  list(
    DK = read_entry_sheet(file_path, "DK"),
    FD = read_entry_sheet(file_path, "FD"),
    SD = read_entry_sheet(file_path, "SD")
  )
}


# ============================================================================
# CONTEST GROUPING
# ============================================================================

#' Summarise entries into contest groups (one row per ContestID).
#' @param entries data.table from read_entry_sheet
#' @return data.table: ContestID, ContestName, EntryFee, N_Entries, TotalInvestment, ContestType
summarise_contests <- function(entries) {
  if (is.null(entries) || nrow(entries) == 0) return(NULL)
  setDT(entries)
  grp <- entries[, .(
    ContestName    = ContestName[1],
    EntryFee       = EntryFee[1],
    EntryFeeNum    = EntryFeeNum[1],
    N_Entries      = .N,
    TotalInvestment = sum(EntryFeeNum, na.rm = TRUE),
    ContestType    = ContestType[1] %||% NA_character_
  ), by = ContestID]
  setorder(grp, ContestName)
  grp
}


# ============================================================================
# LINEUP ASSIGNMENT
# ============================================================================

CONTEST_TYPE_LEVELS <- c("Cash / Double Up", "Multiplier", "Satellite", "Tournament", "Other")

#' Assign lineups to entries.
#'
#' @param entries       data.table of entries for ONE contest group (same ContestID)
#' @param lineup_pool   data.table with LineupID + player cols — the eligible lineup pool
#' @param mode          "random" or "targeted" (targeted = sorted by sort_col desc)
#' @param sort_col      column name to sort by in targeted mode (e.g. "CashRate", "WinRate")
#' @param allow_dupes_within_contest  logical — can the same lineup appear >1x in this contest?
#' @return entries data.table with AssignedLineupID filled in
assign_lineups <- function(entries,
                            lineup_pool,
                            mode      = "random",
                            sort_col  = "CashRate",
                            allow_dupes_within_contest = TRUE) {

  setDT(entries); setDT(lineup_pool)
  n_entries <- nrow(entries)
  n_lineups <- nrow(lineup_pool)

  if (n_lineups == 0) {
    warning("assign_lineups: lineup_pool is empty")
    entries[, AssignedLineupID := NA_character_]
    return(entries)
  }

  # Sort pool
  if (mode == "targeted" && sort_col %in% names(lineup_pool)) {
    pool_ordered <- lineup_pool[order(-get(sort_col))]
  } else {
    pool_ordered <- lineup_pool[sample(.N)]
  }

  ids <- pool_ordered$LineupID

  if (allow_dupes_within_contest) {
    # Round-robin with duplication — distribute as evenly as possible
    # e.g. 50 entries, 8 lineups → each lineup gets 6 or 7 assignments
    assigned <- ids[((seq_len(n_entries) - 1L) %% n_lineups) + 1L]
  } else {
    # No duplication within contest — cycle through unique lineups
    # If more entries than lineups, wrap around (lineup appears in multiple contests, not same)
    if (n_entries <= n_lineups) {
      assigned <- ids[seq_len(n_entries)]
    } else {
      # More entries than lineups — can't fill without duplication
      # Use available unique lineups and leave remainder unassigned
      assigned <- c(ids, rep(NA_character_, n_entries - n_lineups))
      warning(sprintf("assign_lineups: %d entries but only %d unique lineups available — %d entries unassigned",
                      n_entries, n_lineups, n_entries - n_lineups))
    }
  }

  entries[, AssignedLineupID := assigned]
  entries
}


#' Assign lineups across all contest groups for a platform.
#'
#' @param all_entries      data.table — all entries for this platform
#' @param tournament_pool  data.table — DK optimal lineups (tournament)
#' @param cash_pool        data.table — cash/double-up lineups
#' @param mode             "random" or "targeted"
#' @return all_entries with AssignedLineupID filled in per contest type rules
assign_all_contests <- function(all_entries, tournament_pool, cash_pool, mode = "random") {
  setDT(all_entries)
  result <- copy(all_entries)
  result[, AssignedLineupID := NA_character_]

  contest_ids <- unique(result$ContestID)

  for (cid in contest_ids) {
    idx     <- which(result$ContestID == cid)
    entries <- result[idx]
    ctype   <- entries$ContestType[1] %||% "Tournament"

    if (is.na(ctype)) ctype <- "Tournament"

    is_cash <- grepl("Cash|Double", ctype, ignore.case = TRUE)

    pool <- if (is_cash && !is.null(cash_pool) && nrow(cash_pool) > 0) {
      cash_pool[Source == "Yours"]   # only your lineups, not field
    } else if (!is.null(tournament_pool) && nrow(tournament_pool) > 0) {
      tournament_pool
    } else {
      NULL
    }

    if (is.null(pool) || nrow(pool) == 0) next

    sort_col <- if (is_cash) "CashRate" else "WinRate"
    if (!sort_col %in% names(pool)) sort_col <- names(pool)[1]

    entries_assigned <- assign_lineups(
      entries   = entries,
      lineup_pool = pool,
      mode      = mode,
      sort_col  = sort_col,
      allow_dupes_within_contest = is_cash
    )

    result[idx, AssignedLineupID := entries_assigned$AssignedLineupID]
  }

  result
}


# ============================================================================
# EXPORT — DK BULK UPLOAD FORMAT
# ============================================================================

#' Build the DK bulk upload data.frame from assigned entries.
#'
#' Output format matches DK's bulk entry CSV:
#' Entry ID, Contest Name, Contest ID, Entry Fee, <slot1>, <slot2>, ...
#' Player cells use "Name (ID)" format — same as existing portfolio download.
#'
#' @param assigned_entries  data.table with EntryID, ContestName, ContestID,
#'                          EntryFee, AssignedLineupID
#' @param all_lineups       data.table — combined pool with LineupID + player cols
#' @param metadata          data.table — rv$sim_metadata with Player + DKID etc
#' @param config            sport config (for dk_export_slots and platform_columns)
#' @param platform          "DK", "FD", or "SD"
#' @return data.frame ready for write.csv()
build_dk_export <- function(assigned_entries, all_lineups, metadata, config, platform = "DK") {

  setDT(assigned_entries); setDT(all_lineups); setDT(metadata)

  # Get export slot headers from config
  slot_headers <- config$dk_export_slots[[platform]]
  if (is.null(slot_headers)) {
    # Fallback: generic P1..Pn headers
    rs <- config$roster_sizes[[platform]] %||% 6L
    slot_headers <- paste0("P", seq_len(rs))
  }
  n_slots <- length(slot_headers)

  # Get the ID column for this platform
  id_col <- config$platform_columns[[platform]]$id %||% "DKID"

  # Build player ID lookup: Player -> "Name (ID)" string
  if (id_col %in% names(metadata)) {
    id_lookup <- setNames(
      paste0(metadata$Player, " (", metadata[[id_col]], ")"),
      metadata$Player
    )
  } else {
    # No ID column — use name only
    id_lookup <- setNames(metadata$Player, metadata$Player)
  }

  # Player columns in the lineup pool
  player_cols <- grep("^Player[0-9]+$|^Captain$|^Util[0-9]+$|^MVP$",
                      names(all_lineups), value = TRUE)

  # Build output rows
  rows <- lapply(seq_len(nrow(assigned_entries)), function(i) {
    entry <- assigned_entries[i]
    lid   <- entry$AssignedLineupID

    # Base columns
    row <- data.frame(
      `Entry ID`    = entry$EntryID,
      `Contest Name` = entry$ContestName,
      `Contest ID`  = entry$ContestID,
      `Entry Fee`   = entry$EntryFee,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Player slots
    if (!is.na(lid) && lid %in% all_lineups$LineupID) {
      lineup_row <- all_lineups[LineupID == lid]
      for (s in seq_len(n_slots)) {
        pc    <- if (s <= length(player_cols)) player_cols[s] else NA
        pname <- if (!is.na(pc) && pc %in% names(lineup_row)) {
          as.character(lineup_row[[pc]][1])
        } else ""
        row[[slot_headers[s]]] <- if (nchar(pname) > 0 && pname %in% names(id_lookup)) {
          id_lookup[[pname]]
        } else pname
      }
    } else {
      for (s in seq_len(n_slots)) row[[slot_headers[s]]] <- ""
    }

    row
  })

  do.call(rbind, rows)
}


# ============================================================================
# INVESTMENT SUMMARY
# ============================================================================

#' Compute per-player dollar exposure across all assigned entries.
#'
#' @param assigned_entries  data.table with AssignedLineupID, EntryFeeNum
#' @param all_lineups       data.table with LineupID + player cols
#' @param metadata          data.table with Player, DKSalary, DKOwn
#' @param player_cols       character vector of player column names
#' @return data.table: Player, Salary, OwnProj, Lineups, TotalInvestment, AvgInvestment
build_investment_summary <- function(assigned_entries, all_lineups, metadata, player_cols) {

  setDT(assigned_entries); setDT(all_lineups); setDT(metadata)

  # Join entries to their lineup
  joined <- merge(
    assigned_entries[!is.na(AssignedLineupID), .(LineupID = AssignedLineupID, EntryFeeNum)],
    all_lineups[, c("LineupID", player_cols), with = FALSE],
    by = "LineupID", all.x = TRUE
  )

  # Melt to long: one row per player per entry
  long <- melt(joined, id.vars = c("LineupID", "EntryFeeNum"),
               measure.vars = player_cols,
               variable.name = "Slot", value.name = "Player")
  long <- long[!is.na(Player) & nchar(Player) > 0]

  # Aggregate per player
  inv <- long[, .(
    Lineups         = uniqueN(LineupID),
    TotalInvestment = sum(EntryFeeNum, na.rm = TRUE),
    AvgInvestment   = round(mean(EntryFeeNum, na.rm = TRUE), 2)
  ), by = Player]

  # Join metadata
  meta_cols <- intersect(c("Player", "DKSalary", "DKOwn"), names(metadata))
  inv <- merge(inv, metadata[, meta_cols, with = FALSE], by = "Player", all.x = TRUE)

  if ("DKOwn" %in% names(inv)) {
    if (max(inv$DKOwn, na.rm = TRUE) <= 1) inv[, DKOwn := round(DKOwn * 100, 1)]
    setnames(inv, "DKOwn", "OwnProj")
  }
  if ("DKSalary" %in% names(inv)) setnames(inv, "DKSalary", "Salary")

  setorder(inv, -TotalInvestment)
  inv
}


# ============================================================================
# SHINY UI
# ============================================================================

render_contest_manager_ui <- function() {
  tagList(
    div(style = "padding:4px 0 16px;",

      # ── Contest summary strip ────────────────────────────────────────────
      uiOutput("cm_summary_strip"),

      # ── Contest groups table + labeling ─────────────────────────────────
      fluidRow(
        shinydashboard::box(width = 8, title = "Contest Groups",
            status = "primary", solidHeader = TRUE,
            p(style = "color:#888;font-size:12px;margin-bottom:8px;",
              "Contests parsed from your DKEntries sheet. Set the type for each group."),
            DTOutput("cm_contest_groups_tbl")
        ),
        shinydashboard::box(width = 4, title = "Assignment Controls",
            status = "primary", solidHeader = TRUE,
            div(style = "margin-bottom:12px;",
                tags$label("Assignment Mode:", style = "color:#FFE500;font-weight:700;font-size:12px;"),
                radioButtons("cm_assign_mode", NULL,
                             choices = c("Random" = "random", "Targeted (best first)" = "targeted"),
                             selected = "random", inline = FALSE)
            ),
            div(style = "margin-bottom:12px;",
                tags$label("Lineup Source:", style = "color:#FFE500;font-weight:700;font-size:12px;"),
                uiOutput("cm_lineup_source_ui")
            ),
            hr(style = "border-color:#333;"),
            actionButton("cm_assign_all", "Assign All Contests",
                         class = "btn-primary",
                         icon  = icon("magic"),
                         style = "width:100%;margin-bottom:8px;font-weight:700;"),
            actionButton("cm_clear_assignments", "Clear Assignments",
                         class = "btn-danger",
                         style = "width:100%;font-size:11px;")
        )
      ),

      # ── Assignment preview + export ──────────────────────────────────────
      conditionalPanel(
        condition = "output.cm_has_assignments == true",
        fluidRow(
          shinydashboard::box(width = 12, title = "Assigned Entries",
              status = "primary", solidHeader = TRUE,
              div(style = "margin-bottom:10px;display:flex;gap:10px;",
                  downloadButton("cm_export_dk", "Download DK Upload File",
                                 class = "btn-primary",
                                 style = "font-weight:700;"),
                  downloadButton("cm_export_investment", "Download Investment Report",
                                 class = "btn-primary",
                                 style = "font-weight:700;background:#4A90D9!important;border-color:#357ab7!important;")
              ),
              DTOutput("cm_assigned_tbl")
          )
        ),
        fluidRow(
          shinydashboard::box(width = 12, title = "Player Investment Summary",
              status = "info", solidHeader = TRUE,
              DTOutput("cm_investment_tbl")
          )
        )
      )
    )
  )
}


# ============================================================================
# SERVER
# ============================================================================

register_contest_manager_observers <- function(input, output, session, rv) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  cm_rv <- reactiveValues(
    entries         = NULL,   # data.table: all entries for current platform
    contest_groups  = NULL,   # data.table: one row per ContestID with type label
    assigned        = NULL,   # data.table: entries with AssignedLineupID filled
    has_assignments = FALSE
  )

  output$cm_has_assignments <- reactive({ isTRUE(cm_rv$has_assignments) })
  outputOptions(output, "cm_has_assignments", suspendWhenHidden = FALSE)


  # ── Load entries when input file changes ───────────────────────────────────
  observeEvent(rv$input_data, {
    req(rv$input_data)
    fp <- rv$input_file_path
    if (is.null(fp)) return()

    entries <- tryCatch(read_entry_sheet(fp, "DK"), error = function(e) NULL)
    if (!is.null(entries) && nrow(entries) > 0) {
      cm_rv$entries        <- entries
      cm_rv$contest_groups <- summarise_contests(entries)
      cm_rv$assigned       <- NULL
      cm_rv$has_assignments <- FALSE
      cat(sprintf("  [CM] Loaded %d DK entries across %d contests\n",
                  nrow(entries), nrow(cm_rv$contest_groups)))
    } else {
      cm_rv$entries        <- NULL
      cm_rv$contest_groups <- NULL
    }
  }, ignoreNULL = TRUE)


  # ── Summary strip ──────────────────────────────────────────────────────────
  output$cm_summary_strip <- renderUI({
    grp <- cm_rv$contest_groups
    if (is.null(grp) || nrow(grp) == 0) {
      return(div(style = "padding:12px;background:#1a1a1a;border:1px solid #333;border-radius:4px;margin-bottom:16px;",
                 icon("info-circle", style = "color:#555;margin-right:8px;"),
                 span("No DKEntries sheet found in input file. Add a DKEntries tab to enable contest management.",
                      style = "color:#555;font-size:12px;")))
    }
    total_entries    <- sum(grp$N_Entries)
    total_investment <- sum(grp$TotalInvestment, na.rm = TRUE)
    n_contests       <- nrow(grp)
    div(style = "display:flex;gap:16px;margin-bottom:16px;",
        div(style = "padding:12px 20px;background:#1a1a1a;border:1px solid #333;border-radius:4px;flex:1;",
            div(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#555;", "Contests"),
            div(style = "font-size:22px;font-weight:700;color:#FFE500;", n_contests)
        ),
        div(style = "padding:12px 20px;background:#1a1a1a;border:1px solid #333;border-radius:4px;flex:1;",
            div(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#555;", "Total Entries"),
            div(style = "font-size:22px;font-weight:700;color:#FFE500;", total_entries)
        ),
        div(style = "padding:12px 20px;background:#1a1a1a;border:1px solid #333;border-radius:4px;flex:1;",
            div(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#555;", "Total Investment"),
            div(style = "font-size:22px;font-weight:700;color:#4A90D9;",
                paste0("$", formatC(total_investment, format = "f", digits = 2, big.mark = ",")))
        )
    )
  })


  # ── Contest groups table ──────────────────────────────────────────────────
  output$cm_contest_groups_tbl <- renderDT({
    req(cm_rv$contest_groups)
    grp <- copy(cm_rv$contest_groups)

    # Add type selector as HTML
    grp[, ContestType := ifelse(is.na(ContestType), "Tournament", ContestType)]

    type_html <- sapply(seq_len(nrow(grp)), function(i) {
      cid <- grp$ContestID[i]
      sel <- grp$ContestType[i]
      opts <- paste(sapply(CONTEST_TYPE_LEVELS, function(t) {
        sprintf('<option value="%s"%s>%s</option>', t, if (!is.na(sel) && t == sel) " selected" else "", t)
      }), collapse = "")
      sprintf(
        '<select onchange="Shiny.setInputValue(\'cm_type_%s\',this.value,{priority:\'event\'})" style="background:#1e1e1e;color:#fff;border:1px solid #333;padding:3px 6px;border-radius:3px;font-size:11px;width:100%%;">%s</select>',
        gsub("[^A-Za-z0-9]", "_", cid), opts
      )
    })

    disp <- data.table(
      Contest    = grp$ContestName,
      `Entry Fee` = grp$EntryFee,
      Entries    = grp$N_Entries,
      Investment = paste0("$", formatC(grp$TotalInvestment, format = "f", digits = 2)),
      Type       = type_html
    )

    datatable(disp, escape = FALSE, rownames = FALSE,
              options = list(pageLength = 20, dom = "t", scrollX = TRUE,
                             searching = FALSE, lengthChange = FALSE),
              class = "stripe hover compact")
  })


  # ── Observe type changes from inline dropdowns ───────────────────────────
  observe({
    grp <- cm_rv$contest_groups
    req(grp)
    for (i in seq_len(nrow(grp))) {
      local({
        cid      <- grp$ContestID[i]
        input_id <- paste0("cm_type_", gsub("[^A-Za-z0-9]", "_", cid))
        val      <- input[[input_id]]
        if (!is.null(val) && !is.na(val)) {
          cm_rv$contest_groups[ContestID == cid, ContestType := val]
          if (!is.null(cm_rv$entries))
            cm_rv$entries[ContestID == cid, ContestType := val]
        }
      })
    }
  })


  # ── Lineup source selector ────────────────────────────────────────────────
  output$cm_lineup_source_ui <- renderUI({
    has_dk   <- !is.null(rv$dk_optimal_lineups)
    has_cash <- !is.null(rv$cash_results)
    if (!has_dk && !has_cash) {
      return(p("No lineups available. Run Analyze Lineups first.",
               style = "color:#666;font-size:11px;"))
    }
    choices <- c()
    if (has_dk)   choices <- c(choices, "Tournament (DK Optimal)" = "tournament")
    if (has_cash) choices <- c(choices, "Cash (Double Up)" = "cash")
    choices <- c(choices, "Auto (by contest type)" = "auto")
    radioButtons("cm_lineup_source", NULL, choices = choices,
                 selected = "auto", inline = FALSE)
  })


  # ── Assign All ────────────────────────────────────────────────────────────
  observeEvent(input$cm_assign_all, {
    req(cm_rv$entries)

    entries <- copy(cm_rv$entries)

    # Apply current contest type labels
    grp <- cm_rv$contest_groups
    if (!is.null(grp)) {
      for (i in seq_len(nrow(grp))) {
        entries[ContestID == grp$ContestID[i], ContestType := grp$ContestType[i]]
      }
    }

    source_sel <- input$cm_lineup_source %||% "auto"
    mode_sel   <- input$cm_assign_mode   %||% "random"

    # Build lineup pools
    tournament_pool <- if (!is.null(rv$dk_optimal_lineups)) {
      dl <- copy(as.data.table(rv$dk_optimal_lineups))
      if (!"LineupID" %in% names(dl)) dl[, LineupID := paste0("T", seq_len(.N))]
      dl
    } else NULL

    cash_pool <- if (!is.null(rv$cash_results)) {
      copy(as.data.table(rv$cash_results))
    } else NULL

    # Override pools based on source selection
    if (source_sel == "tournament") cash_pool <- NULL
    if (source_sel == "cash")       tournament_pool <- NULL

    assigned <- assign_all_contests(
      all_entries     = entries,
      tournament_pool = tournament_pool,
      cash_pool       = cash_pool,
      mode            = mode_sel
    )

    cm_rv$assigned       <- assigned
    cm_rv$has_assignments <- any(!is.na(assigned$AssignedLineupID))

    n_assigned <- sum(!is.na(assigned$AssignedLineupID))
    showNotification(sprintf("Assigned %d of %d entries.", n_assigned, nrow(assigned)),
                     type = "message")
  })


  # ── Clear assignments ─────────────────────────────────────────────────────
  observeEvent(input$cm_clear_assignments, {
    cm_rv$assigned       <- NULL
    cm_rv$has_assignments <- FALSE
  })


  # ── Assigned entries table ────────────────────────────────────────────────
  output$cm_assigned_tbl <- renderDT({
    req(cm_rv$assigned)
    dt <- copy(cm_rv$assigned)
    keep <- intersect(c("EntryID", "ContestName", "EntryFee", "ContestType",
                        "AssignedLineupID"), names(dt))
    dt <- dt[, keep, with = FALSE]
    datatable(dt, rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE,
                             searching = TRUE, dom = "ftp"),
              class = "stripe hover compact") %>%
      formatStyle("AssignedLineupID",
                  color = styleEqual(NA, "#666"),
                  fontWeight = "600")
  })


  # ── Investment table ──────────────────────────────────────────────────────
  output$cm_investment_tbl <- renderDT({
    req(cm_rv$assigned, rv$dk_optimal_lineups %||% rv$cash_results, rv$sim_metadata)

    assigned <- cm_rv$assigned[!is.na(AssignedLineupID)]
    if (nrow(assigned) == 0) return(datatable(data.table(Message = "No assignments yet.")))

    # Combine all lineups
    all_lu <- rbindlist(list(
      if (!is.null(rv$dk_optimal_lineups)) {
        dl <- copy(as.data.table(rv$dk_optimal_lineups))
        if (!"LineupID" %in% names(dl)) dl[, LineupID := paste0("T", seq_len(.N))]
        dl
      } else NULL,
      if (!is.null(rv$cash_results)) copy(as.data.table(rv$cash_results)) else NULL
    ), fill = TRUE, use.names = TRUE)

    if (nrow(all_lu) == 0) return(NULL)

    player_cols <- grep("^Player[0-9]+$|^Captain$|^Util[0-9]+$|^MVP$",
                        names(all_lu), value = TRUE)

    inv <- build_investment_summary(assigned, all_lu, rv$sim_metadata, player_cols)

    datatable(inv, rownames = FALSE,
              options = list(pageLength = 30, scrollX = TRUE,
                             searching = FALSE, dom = "tp"),
              class = "stripe hover compact") %>%
      { if ("Salary"  %in% names(inv)) formatCurrency(., "Salary",  "$", digits = 0) else . } %>%
      { if ("OwnProj" %in% names(inv)) formatRound(.,   "OwnProj", 1)                else . } %>%
      formatCurrency("TotalInvestment", "$", digits = 2) %>%
      formatStyle("TotalInvestment",
                  background = styleColorBar(range(inv$TotalInvestment, na.rm = TRUE),
                                             "rgba(74,144,217,0.4)"),
                  backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "left")
  })


  # ── Export DK upload file ─────────────────────────────────────────────────
  output$cm_export_dk <- downloadHandler(
    filename = function() {
      paste0("GTS_DKUpload_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(cm_rv$assigned, rv$sim_metadata, rv$config)

      assigned <- cm_rv$assigned[!is.na(AssignedLineupID)]
      if (nrow(assigned) == 0) {
        write.csv(data.frame(Error = "No assignments"), file, row.names = FALSE)
        return()
      }

      # Combine all lineups
      all_lu <- rbindlist(list(
        if (!is.null(rv$dk_optimal_lineups)) {
          dl <- copy(as.data.table(rv$dk_optimal_lineups))
          if (!"LineupID" %in% names(dl)) dl[, LineupID := paste0("T", seq_len(.N))]
          dl
        } else NULL,
        if (!is.null(rv$cash_results)) copy(as.data.table(rv$cash_results)) else NULL
      ), fill = TRUE, use.names = TRUE)

      export_df <- tryCatch(
        build_dk_export(assigned, all_lu, rv$sim_metadata, rv$config, "DK"),
        error = function(e) {
          data.frame(Error = paste("Export failed:", e$message))
        }
      )

      write.csv(export_df, file, row.names = FALSE, na = "")
    }
  )


  # ── Export investment report ──────────────────────────────────────────────
  output$cm_export_investment <- downloadHandler(
    filename = function() paste0("GTS_Investment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(cm_rv$assigned, rv$sim_metadata)

      assigned <- cm_rv$assigned[!is.na(AssignedLineupID)]
      all_lu <- rbindlist(list(
        if (!is.null(rv$dk_optimal_lineups)) {
          dl <- copy(as.data.table(rv$dk_optimal_lineups))
          if (!"LineupID" %in% names(dl)) dl[, LineupID := paste0("T", seq_len(.N))]
          dl
        } else NULL,
        if (!is.null(rv$cash_results)) copy(as.data.table(rv$cash_results)) else NULL
      ), fill = TRUE, use.names = TRUE)

      player_cols <- grep("^Player[0-9]+$|^Captain$|^Util[0-9]+$|^MVP$",
                          names(all_lu), value = TRUE)
      inv <- build_investment_summary(assigned, all_lu, rv$sim_metadata, player_cols)
      write.csv(inv, file, row.names = FALSE, na = "")
    }
  )

}
# end of contest_manager_module.R
