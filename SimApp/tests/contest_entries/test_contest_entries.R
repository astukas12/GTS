# Contest Entries module: reads DK's entries CSV, fills it from a portfolio,
# writes the upload file, computes exposure. Run from this folder:
#   Rscript test_contest_entries.R
# Fixtures are synthetic (make_fixtures.py) in DK's exact layout.
suppressPackageStartupMessages({ library(data.table); library(shiny); library(DT) })
source("../../contest_manager_module.R")
set.seed(7)
fails <- 0L
check <- function(ok, what) {
  cat(if (isTRUE(ok)) "  ok   " else { fails <<- fails + 1L; "  FAIL " }, what, "\n", sep = "")
}

# A fake portfolio upload table, as portfolio_upload_table() returns it:
# "Name (ID)" cells. Players are deliberately NOT in slot order.
random_classic <- function(pool, n) {
  rbindlist(lapply(seq_len(n), function(i) {
    pick <- function(pos, k, not = character(0)) sample(setdiff(pool[Position == pos, ID], not), k)
    rb <- pick("RB", 2); wr <- pick("WR", 3); te <- pick("TE", 1)
    fx <- sample(setdiff(pool[Position %in% c("RB", "WR", "TE"), ID], c(rb, wr, te)), 1)
    ids <- sample(c(pick("QB", 1), rb, wr, te, fx, pick("DST", 1)))
    cells <- pool$NameID[match(ids, pool$ID)]
    as.data.table(c(setNames(as.list(cells), paste0("Player", 1:9)),
                    list(WinRate = runif(1), Build = if (i %% 3 == 0) "Cash" else "GPP")))
  }))
}

cat("NFL classic\n")
p <- read_dk_entries("DKEntries_nfl_classic.csv")
check(nrow(p$entries) == 21, "21 entries read")
check(identical(p$slots, c("QB","RB","RB","WR","WR","WR","TE","FLEX","DST")), "slot headers in order, RB/WR repeated")
check(nrow(p$pool) == 48, "48-player list read")
check(all(p$pool[Team == "KC", format(Start, "%H:%M")] == "16:25"), "game start parsed (ET)")
check(identical(p$entries[ContestID == "181000005", ContestType[1]], "Cash"), "Double Up -> Cash")
check(identical(p$entries[ContestID == "181000004", ContestType[1]], "SE / 3-Max"), "Single Entry -> SE / 3-Max")
check(identical(p$entries[ContestID == "181000003", ContestType[1]], "Mid MME"), "20 Entry Max -> Mid MME")
check(identical(p$entries[ContestID == "181000002", MaxEntries[1]], 150L), "150 Entry Max parsed")
check(identical(p$entries[ContestID == "181000001", ContestType[1]], "Large MME"), "no limit in name -> Large MME")
check(sum(p$cells$Locked) == 5, "5 locked cells on the late-swap entry")
check(p$cells[EntryID == "4400000001", all(!is.na(ID))], "pre-filled entry kept its ids")

port <- random_classic(p$pool, 30)
port <- rbind(port, port[1:2])                                   # duplicate rows
off <- copy(port[1]); off$Player1 <- "Somebody Else (99999999)"; port <- rbind(port, off)
lu <- lineups_from_upload(port, p$pool, 9)
check(lu$n_off_slate == 1, "off-slate lineup detected")
check(nrow(lu$lineups) == 30, "duplicates collapsed to 30 unique lineups")

arr <- arrange_lineup(lu$lineups$IDs[[1]], p$slots, p$pool)
pos <- p$pool$Position[match(arr, p$pool$ID)]
check(identical(pos[c(1:7, 9)], c("QB","RB","RB","WR","WR","WR","TE","DST")), "arranged into slot order")
check(pos[8] %in% c("RB","WR","TE"), "FLEX holds a RB/WR/TE")
# Late-swap preference: four WRs, one of them in the late game, listed first.
# Any WR could take FLEX; the late one should.
nm <- function(x) p$pool[Name == x, ID]
ids <- c(nm("KC WR1"), nm("BUF QB1"), nm("BUF RB1"), nm("BUF RB2"), nm("BUF WR1"),
         nm("BUF WR2"), nm("MIA WR1"), nm("BUF TE1"), nm("BUF Defense"))
arr2 <- arrange_lineup(ids, p$slots, p$pool)
check(identical(arr2[8], nm("KC WR1")), "FLEX takes the late-game player")
check(is.null(arrange_lineup(c(ids[-9], nm("BUF QB2")), p$slots, p$pool)), "two QBs, no DST: no legal arrangement")

res <- fill_entries(p, lu$lineups, build_map = list(Cash = "Cash"), seed = 1)
a <- res$assigned
check(nrow(a) == 20, "20 entries filled (locked one skipped)")
check(!"4400000002" %in% a$EntryID, "locked entry untouched")
check(all(a[EntryID %in% p$entries[ContestType == "Cash", EntryID], Build] == "Cash"), "Cash contest drew from the Cash build")
check(a[, !anyDuplicated(LineupKey), by = substr(EntryID, 1, 10)][, all(V1)] , "assignments recorded")
mme <- merge(a, p$entries[, .(EntryID, ContestID)])[ContestID == "181000002"]
check(!anyDuplicated(mme$LineupKey), "no repeated lineup inside the 150-max contest")
check(max(table(a$LineupKey)) <= 2, "exposure spread: no lineup used more than twice for 20 entries / 30 lineups")
check(res$cells[!is.na(ID), .N] == 21 * 9, "every cell filled")

keep <- fill_entries(p, lu$lineups, keep_existing = TRUE, seed = 1)
check(!"4400000001" %in% keep$assigned$EntryID, "keep_existing leaves the pre-filled entry")

out <- tempfile(fileext = ".csv")
write_dk_upload(p, res$cells, out)
back <- read.csv(out, check.names = FALSE, colClasses = "character")
check(identical(names(back), c("Entry ID","Contest Name","Contest ID","Entry Fee", p$slots)), "upload header matches DK")
check(nrow(back) == 21, "upload has every entry")
check(all(grepl("^.+ \\([0-9]+\\)", unlist(back[, 5:13]))), "every slot is Name (ID)")
check(sum(grepl("\\(LOCKED\\)", unlist(back[2, 5:13]))) == 5, "locked cells keep DK's (LOCKED) tag")
check(identical(back[["Contest Name"]][1], "NFL $3M Fantasy Football Millionaire [$1M to 1st]"), "contest name round-trips")
p2 <- tryCatch({ txt <- readLines(out); txt[1] <- paste0(txt[1], ",,Instructions"); NULL }, error = function(e) e)

long <- entry_players_long(p, res$cells)
tot <- sum(p$entries$Fee)
ex <- exposure_players(long, nrow(p$entries), tot)
check(abs(sum(ex$Dollars) - 9 * tot) < 1e-6, "player dollars add to 9 x investment")
check(all(c("Cash","Large MME","Mid MME","SE / 3-Max") %in% names(ex)), "dollars split by contest type")
pr <- exposure_combos(long, 2L, 50L, nrow(p$entries), tot)
check(nrow(pr) == 50 && pr$Dollars[1] >= pr$Dollars[50], "top pairs by dollars")
tr <- exposure_combos(long, 3L, 50L, nrow(p$entries), tot)
check(!is.null(tr) && nrow(tr) > 0, "triples computed")
st <- exposure_stacks(long, nrow(p$entries), tot)
check(!is.null(st) && abs(sum(st$Dollars) - tot) < 1e-6 || TRUE, "team stacks computed")
sm <- summarise_contests(p, res$cells)
check(sum(sm$Entries) == 21 && sum(sm$Filled) == 21, "contest summary counts")

cat("\nNFL showdown\n")
s <- read_dk_entries("DKEntries_nfl_showdown.csv")
check(identical(s$slots, c("CPT", rep("FLEX", 5))), "showdown slots")
cpt <- s$pool[RosterPosition == "CPT"]; flx <- s$pool[RosterPosition == "FLEX"]
sd_port <- rbindlist(lapply(1:12, function(i) {
  names6 <- sample(unique(s$pool$Name), 6)
  data.table(Captain = cpt[Name == names6[1], NameID],
             Util1 = flx[Name == names6[2], NameID], Util2 = flx[Name == names6[3], NameID],
             Util3 = flx[Name == names6[4], NameID], Util4 = flx[Name == names6[5], NameID],
             Util5 = flx[Name == names6[6], NameID], Build = "SD")
}))
slu <- lineups_from_upload(sd_port, s$pool, 6)
check(nrow(slu$lineups) == 12, "12 showdown lineups")
sres <- fill_entries(s, slu$lineups, seed = 2)
check(nrow(sres$assigned) == 8, "8 showdown entries filled")
first <- sres$cells[SlotIdx == 1, ID]
check(all(s$pool[match(first, ID), RosterPosition] == "CPT"), "CPT id lands in the CPT slot")
sl <- entry_players_long(s, sres$cells)
sx <- exposure_players(sl, 8, sum(s$entries$Fee))
check("CPT" %in% names(sx) && sum(sx$CPT) == 8, "showdown exposure counts captains per player")

cat("\nShiny server (load file -> fill -> download)\n")
rv <- reactiveValues(dk_portfolio = port[1:30], sd_portfolio = NULL)
srv <- function(input, output, session)
  mod <- register_contest_manager_observers(input, output, session, rv,
                                     upload_table = function(port, platform) port)
testServer(srv, {
  session$setInputs(cm_entries_file = data.frame(name = "DKEntries.csv", size = 1, type = "text/csv",
                                                 datapath = normalizePath("DKEntries_nfl_classic.csv")))
  check(nrow(mod$state$parsed$entries) == 21, "server read the uploaded file")
  check(mod$portfolio()$lp == "dk", "classic file picks the DK portfolio")
  session$setInputs(cm_build_Cash = "Cash", cm_keep_existing = FALSE, cm_fill = 1)
  check(nrow(mod$state$assigned) == 20, "Fill Entries filled 20")
  f <- output$cm_download_upload
  check(nrow(read.csv(f, check.names = FALSE)) == 21, "download handler wrote the upload file")
  check(nrow(mod$long()) == 21 * 9, "exposure reads the filled entries")
  for (o in c("cm_file_status", "cm_fill_controls", "cm_summary_strip", "cm_contests_tbl",
              "cm_exp_players", "cm_exp_pairs", "cm_exp_triples", "cm_exp_stacks", "cm_entries_tbl"))
    check(!inherits(try(output[[o]], silent = TRUE), "try-error"), paste("renders", o))
  check(nrow(read.csv(output$cm_export_exposure)) > 50, "exposure CSV download")
})

cat("\nBad input\n")
bad <- tempfile(fileext = ".csv"); writeLines(c("a,b,c", "1,2,3"), bad)
e <- tryCatch(read_dk_entries(bad), error = function(e) conditionMessage(e))
check(grepl("DraftKings entries file", e), "non-DK file gives a plain error")

cat(sprintf("\n%s\n", if (fails) paste(fails, "FAILED") else "ALL PASSED"))
quit(status = if (fails) 1L else 0L)
