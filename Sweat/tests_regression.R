setwd("C:/Users/astuk/OneDrive/Documents/GitHub/GTS/Sweat")
source("app.r", echo = FALSE, print.eval = FALSE)
# Forward slashes: this path gets written into R source below, where a Windows
# backslash would be read as an escape.
SP  <- gsub("\\\\", "/", tempdir())
LOG <- file.path(SP, "regress_log.txt"); unlink(LOG)
say <- function(...) {
  msg <- paste(...)
  write(msg, file = LOG, append = TRUE)
  cat(msg, "\n"); flush.console()   # also to stdout so a piped run shows progress
}
FAILS <- 0
chk <- function(l, e) {
  r <- tryCatch({ force(e); "ok" }, error = function(x) paste("FAIL:", conditionMessage(x)))
  if (!identical(r, "ok")) FAILS <<- FAILS + 1
  say("   ", sprintf("%-22s", l), r)
}

# ---- rebuild the NFL mid-contest fixture (SF@LAC locked) ----
nfl_csv  <- "C:/Users/astuk/Downloads/contest-standings-193845373.csv"
nfl_xlsx <- "C:/Users/astuk/OneDrive/Documents/GTS/NFL/Preseason/input_preseason_wk2_thu.xlsx"
nfl_lock <- file.path(SP, "nfl_locked_fixture.csv")
mm <- read_input_nfl(nfl_xlsx, readxl::excel_sheets(nfl_xlsx))
lk <- c(mm[Team %in% c("SF","LAC"), Key], names(NFL_NICKNAMES)[NFL_NICKNAMES %in% c("SF","LAC")])
ln <- readLines(nfl_csv, warn = FALSE)
bd <- vapply(strsplit(ln[-1], ",", fixed = TRUE), function(f) {
  if (length(f) < 6) return(paste(f, collapse = ","))
  if (nzchar(f[6])) {
    p <- parse_lineup_slots(f[6])
    if (!is.null(p)) {
      p[, PlayerRaw := fifelse(norm_name(PlayerRaw) %in% lk, "LOCKED", PlayerRaw)]
      f[6] <- paste(paste(p$Slot, p$PlayerRaw), collapse = " ")
    }
  }
  if (length(f) >= 11 && nzchar(f[8]) && norm_name(f[8]) %in% lk) f[8:11] <- ""
  paste(f, collapse = ",")
}, character(1))
writeLines(c(ln[1], bd), nfl_lock)
say("rebuilt NFL locked fixture, LOCKED lines:", length(grep("LOCKED", bd)))

# ---- rebuild the MMA-SD fixture from the repo script ----
sd_out <- file.path(SP, "mma_sd_fixture.csv")
local({
  e <- new.env(); e$OUT_OVERRIDE <- sd_out
  src <- readLines("tests_mma_sd_fixture.R", warn = FALSE)
  src <- sub('^out  <- .*$', paste0('out  <- "', sd_out, '"'), src)
  eval(parse(text = paste(src, collapse = "\n")), envir = e)
})
say("rebuilt MMA-SD fixture")

run <- function(name, csv, xlsx, dims = character(0),
                expect_sport = NULL, expect_key = NULL, expect_conserve = NULL) {
  say("\n=====", name, "=====")
  shiny::testServer(server, {
    session$setInputs(file = list(datapath = csv, name = "c.csv"),
                      sport_override = "Auto-detect")
    ct <- contest()
    say("  detected:", ct$detected, "| entries:", nrow(ct$entries),
        "| slots:", paste(sort(unique(ct$slots$Slot)), collapse = ","))
    if (!is.null(expect_sport) && !identical(ct$detected, expect_sport)) {
      FAILS <<- FAILS + 1; say("   FAIL: expected sport", expect_sport)
    }
    if (!is.null(xlsx)) session$setInputs(input_file = list(datapath = xlsx, name = "x.xlsx"))
    if (!is.null(expect_key)) {
      say("  sport_key:", sport_key(), "expected", expect_key)
      if (!identical(sport_key(), expect_key)) { FAILS <<- FAILS + 1; say("   FAIL: sport_key") }
    }
    pl <- players()
    say("  pool:", nrow(pl), "| matched:", sum(pl$Matched),
        "| dims:", paste(names(group_dims()), collapse = "/"))
    li <- lock_info(); say("  locked slot pct:", round(li$locked_slot_pct, 1))
    top <- ct$entries[, .N, by = Username][order(-N)][1]
    session$setInputs(username = top$Username)
    ex <- exposure()
    say("  user:", top$Username, "| n_user:", ex$n_user, "| n_field:", ex$n_field)
    if (!is.null(expect_conserve)) {
      got <- round(sum(ex$tbl$FieldExp, na.rm = TRUE), 0)
      say("  conservation:", got, "expected", expect_conserve)
      if (abs(got - expect_conserve) > 1) { FAILS <<- FAILS + 1; say("   FAIL: conservation") }
    }
    for (o in c("sport_badge","meta_status","my_sweat_content","exposure_table",
                "positive_leverage_plot","negative_leverage_plot","breakdown_content",
                "lock_content","lock_slot_table","lock_player_table",
                "live_content","combo_content","dupe_analysis_content",
                "user_dupe_chart","dupe_table")) chk(o, output[[o]])
    # Only exercise dimensions the app actually offers: a dimension with a
    # single distinct value (e.g. Game when only one game has kicked off) is
    # deliberately withheld.
    avail <- group_dims()
    skipped <- setdiff(dims, avail)
    if (length(skipped)) say("  (dims withheld by design:", paste(skipped, collapse = ","), ")")
    for (d in intersect(dims, avail)) { session$setInputs(group_dim = d)
      chk(paste("group", d), output$group_summary); chk(paste("plot", d), output$group_plot) }
  })
}

run("NASCAR (today + TrucksNH)",
    "C:/Users/astuk/Downloads/contest-standings-194007416.csv",
    "C:/Users/astuk/OneDrive/Documents/GTS/Nascar/InputFiles/TrucksNH.xlsx",
    dims = c("Team","StartGroup","SalaryTier"),
    expect_sport = "NASCAR", expect_key = "NASCAR", expect_conserve = 600)

run("NASCAR (no input file)",
    "C:/Users/astuk/Downloads/contest-standings-194007416.csv", NULL,
    expect_sport = "NASCAR", expect_conserve = 600)

run("NFL final export", nfl_csv, nfl_xlsx,
    dims = c("Pos","Team","Game","DepthSlot"),
    expect_sport = "NFL", expect_key = "NFL", expect_conserve = 900)

run("NFL mid-contest (SF@LAC locked)", nfl_lock, nfl_xlsx,
    dims = c("Pos","Team","Game"),
    expect_sport = "NFL")

run("MMA classic",
    "C:/Users/astuk/Downloads/contest-standings-193845215.csv",
    "C:/Users/astuk/OneDrive/Documents/GTS/MMA/UFC_Simulation_Input_8-22.xlsx",
    dims = c("WeightClass","Role","Fight","SalaryTier"),
    expect_sport = "MMA", expect_key = "MMA", expect_conserve = 600)

run("MMA Showdown (real export)",
    "C:/Users/astuk/Downloads/contest-standings-194181193.csv",
    "C:/Users/astuk/OneDrive/Documents/GTS/MMA/UFC_Simulation_Input_8-22.xlsx",
    dims = c("WeightClass","Fight","SalaryTier"),
    expect_sport = "MMA-SD", expect_key = "MMA-SD", expect_conserve = 600)

run("MMA Showdown (captain-trap fixture)", sd_out,
    "C:/Users/astuk/OneDrive/Documents/GTS/MMA/UFC_Simulation_Input_8-22.xlsx",
    expect_sport = "MMA-SD", expect_key = "MMA-SD", expect_conserve = 600)

run("CFB (matching slate)",
    "C:/Users/astuk/Downloads/contest-standings-194610521.csv",
    "C:/Users/astuk/OneDrive/Documents/GTS/CFB/slates/2026-09-03_CFB_THU.xlsx",
    dims = c("Pos","Team","Game","SalaryTier"),
    expect_sport = "CFB", expect_key = "CFB")

run("CFB (wrong slate file)",
    "C:/Users/astuk/Downloads/contest-standings-194610521.csv",
    "C:/Users/astuk/OneDrive/Documents/GTS/CFB/slates/2026-08-29_CFB_ALL.xlsx",
    expect_sport = "CFB")

run("CFB Showdown (FSU@SMU)",
    "C:/Users/astuk/Downloads/contest-standings-195045051.csv",
    "C:/Users/astuk/OneDrive/Documents/GTS/CFB/slates/2026-09-07_FSU_SMU.xlsx",
    dims = c("Pos","Team","SalaryTier"),
    expect_sport = "Showdown", expect_key = "CFB-SD", expect_conserve = 600)

run("Soccer classic (UCL 9 Sep)",
    "C:/Users/astuk/Downloads/contest-standings-195376207.csv",
    "C:/Users/astuk/OneDrive/Documents/GTS/Soccer/Soccer_Input_Combined.xlsx",
    dims = c("Pos","Team","Opp","Game","SalaryTier"),
    expect_sport = "Soccer", expect_key = "Soccer", expect_conserve = 800)

run("NFL Showdown (SEA@NE)",
    "C:/Users/astuk/Downloads/contest-standings-193391013.csv",
    "C:/Users/astuk/OneDrive/Documents/GTS/NFL/slates/2026-09-09_SEA_NE_SD.xlsx",
    dims = c("Pos","Team","SalaryTier"),
    expect_sport = "Showdown", expect_key = "NFL-SD", expect_conserve = 600)

# ---- targeted assertions ----
say("\n===== targeted checks =====")
shiny::testServer(server, {
  session$setInputs(file = list(datapath = sd_out, name = "sd.csv"),
                    input_file = list(datapath = "C:/Users/astuk/OneDrive/Documents/GTS/MMA/UFC_Simulation_Input_8-22.xlsx",
                                      name = "m.xlsx"), sport_override = "Auto-detect")
  st <- fight_status()
  w <- st[Player == "Anthony Wint", Outcome]
  say("  captain-scaling trap: Anthony Wint =", w, "(expect Won)")
  if (!identical(w, "Won")) { FAILS <<- FAILS + 1; say("   FAIL") }
})
shiny::testServer(server, {
  session$setInputs(file = list(datapath = "C:/Users/astuk/Downloads/contest-standings-194610521.csv", name = "c.csv"),
                    input_file = list(datapath = "C:/Users/astuk/OneDrive/Documents/GTS/CFB/slates/2026-08-29_CFB_ALL.xlsx",
                                      name = "w.xlsx"), sport_override = "Auto-detect")
  n <- sum(players()$Matched)
  say("  fuzzy cutoff vs wrong CFB slate: matched =", n, "(expect 0)")
  if (n != 0) { FAILS <<- FAILS + 1; say("   FAIL") }
  ms <- paste(as.character(output$meta_status), collapse = " ")
  say("  wrong CFB slate still flagged 'Check the file pairing':", grepl("Check the file pairing", ms))
  if (!grepl("Check the file pairing", ms)) { FAILS <<- FAILS + 1; say("   FAIL") }
})
shiny::testServer(server, {
  # Soccer's input sheet is modelled players only, so ~26% of the contest pool
  # (all sub-2% drafted) is unmatched. The ownership-weighted check must NOT
  # cry wrong-slate on that.
  session$setInputs(file = list(datapath = "C:/Users/astuk/Downloads/contest-standings-195376207.csv", name = "c.csv"),
                    input_file = list(datapath = "C:/Users/astuk/OneDrive/Documents/GTS/Soccer/Soccer_Input_Combined.xlsx",
                                      name = "s.xlsx"), sport_override = "Auto-detect")
  pl <- players()
  say("  soccer matched:", sum(pl$Matched), "of", nrow(pl))
  ms <- paste(as.character(output$meta_status), collapse = " ")
  flagged <- grepl("Check the file pairing", ms)
  say("  soccer NOT flagged wrong-slate:", !flagged)
  if (flagged) { FAILS <<- FAILS + 1; say("   FAIL") }
})
shiny::testServer(server, {
  # NFL Showdown: workbook is the CFB-style game+team+projections layout, told
  # apart from CFB by the NE/SEA team sheet names. Position comes off route_base
  # (+ DST). Modelled players only, so it must not be flagged wrong-slate.
  session$setInputs(file = list(datapath = "C:/Users/astuk/Downloads/contest-standings-193391013.csv", name = "c.csv"),
                    input_file = list(datapath = "C:/Users/astuk/OneDrive/Documents/GTS/NFL/slates/2026-09-09_SEA_NE_SD.xlsx",
                                      name = "n.xlsx"), sport_override = "Auto-detect")
  say("  NFL-SD sport_key:", sport_key(), "(expect NFL-SD)")
  if (!identical(sport_key(), "NFL-SD")) { FAILS <<- FAILS + 1; say("   FAIL") }
  pl <- players()
  poss <- sort(unique(pl$Pos[pl$Matched & !is.na(pl$Pos)]))
  say("  NFL-SD matched:", sum(pl$Matched), "of", nrow(pl), "| positions:", paste(poss, collapse = ","))
  if (!all(c("QB","RB","WR","TE","DST") %in% poss)) { FAILS <<- FAILS + 1; say("   FAIL: positions") }
  ms <- paste(as.character(output$meta_status), collapse = " ")
  say("  NFL-SD NOT flagged wrong-slate:", !grepl("Check the file pairing", ms))
  if (grepl("Check the file pairing", ms)) { FAILS <<- FAILS + 1; say("   FAIL") }
  session$setInputs(username = contest()$entries$Username[1])
  ex <- exposure()
  if (!all(c("CptUserExp","FlexUserExp") %in% names(ex$tbl))) { FAILS <<- FAILS + 1; say("   FAIL: no CPT/FLEX split") }
  say("  NFL-SD CPT/FLEX split present:", all(c("CptUserExp","FlexUserExp") %in% names(ex$tbl)))
})
shiny::testServer(server, {
  session$setInputs(file = list(datapath = "C:/Users/astuk/Downloads/contest-standings-195045051.csv", name = "c.csv"),
                    input_file = list(datapath = "C:/Users/astuk/OneDrive/Documents/GTS/CFB/slates/2026-09-07_FSU_SMU.xlsx",
                                      name = "x.xlsx"), sport_override = "Auto-detect")
  top <- contest()$entries[, .N, by = Username][order(-N)][1]
  session$setInputs(username = top$Username)
  ex <- exposure()
  has_split <- all(c("CptUserExp","CptFieldExp","FlexUserExp","FlexFieldExp") %in% names(ex$tbl))
  say("  CFB-SD per-slot exposure columns present:", has_split, "(expect TRUE)")
  if (!has_split) { FAILS <<- FAILS + 1; say("   FAIL") }
  # Captain slot fills exactly one seat per lineup, so field CPT exposure sums to 100.
  cpt_sum <- round(sum(ex$tbl$CptFieldExp, na.rm = TRUE), 0)
  say("  CFB-SD field CPT exposure sum:", cpt_sum, "(expect 100)")
  if (abs(cpt_sum - 100) > 1) { FAILS <<- FAILS + 1; say("   FAIL") }

  session$setInputs(sd_exposure_view = "split")
  chk("exposure_table (split)", output$exposure_table)
  session$setInputs(sd_exposure_view = "pooled")
  chk("exposure_table (pooled)", output$exposure_table)
  chk("proj_table", output$proj_table)

  # Showdown exposure table: salary shows as $K to 1dp, projections and the game
  # Total are gone (projections live in proj_table now), no CPT-salary column.
  de <- display_exposure(ex$tbl, slot_split = TRUE, include_proj = FALSE)
  ok_cols <- "Salary ($K)" %in% names(de) && !("Salary" %in% names(de)) &&
             !any(c("Proj %","Field vs Proj","Proj","CPT Salary","Proj CPT %","Total") %in% names(de))
  sal_k <- all(de$`Salary ($K)` == round(de$`Salary ($K)`, 1), na.rm = TRUE) &&
           max(de$`Salary ($K)`, na.rm = TRUE) < 100
  say("  CFB-SD exposure cols lean + $K salary:", ok_cols && sal_k, "(expect TRUE)")
  if (!(ok_cols && sal_k)) { FAILS <<- FAILS + 1; say("   FAIL") }

  # Combo is slot-qualified: choices carry "(CPT)" / "(FLEX)" and the hit test
  # matches slot+player, not player anywhere.
  cs <- combo_sets()
  tagged <- all(grepl("\\((CPT|FLEX)\\)$", unlist(cs$Players[1])))
  say("  CFB-SD combo entries slot-qualified:", tagged, "(expect TRUE)")
  if (!tagged) { FAILS <<- FAILS + 1; say("   FAIL") }
  cap <- unlist(cs$Players)[grepl("\\(CPT\\)$", unlist(cs$Players))][1]
  session$setInputs(combo_players = cap)
  ch <- combo_hits()
  say("  CFB-SD combo on", cap, "-> user+field lineups:", length(ch$user) + length(ch$field))
  chk("combo_results (slot combo)", output$combo_results)
  chk("combo_exposure_table (slot combo)", output$combo_exposure_table)
})

# Live Lineups tab is hidden for a non-h2h sport (CFB-SD) and shown for MMA.
shiny::testServer(server, {
  session$setInputs(file = list(datapath = "C:/Users/astuk/Downloads/contest-standings-195045051.csv", name = "c.csv"),
                    input_file = list(datapath = "C:/Users/astuk/OneDrive/Documents/GTS/CFB/slates/2026-09-07_FSU_SMU.xlsx",
                                      name = "x.xlsx"), sport_override = "Auto-detect")
  session$setInputs(username = contest()$entries$Username[1])
  say("  CFB-SD has_h2h:", isTRUE(has_h2h()), "(expect FALSE -> Live Lineups tab hidden)")
  if (isTRUE(has_h2h())) { FAILS <<- FAILS + 1; say("   FAIL") }
})

say("\n=========== TOTAL FAILURES:", FAILS, "===========")
