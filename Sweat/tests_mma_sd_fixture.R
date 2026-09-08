# Synthesise an MMA Showdown contest export in DK's documented shape:
#   Lineup  = "CPT <name> F <name> F <name> F <name> F <name> F <name>"
#   block   = one row per (player, roster position) with CPT rows carrying the
#             1.5x captain score.
# Built so one decided bout has the LOSER captained and the WINNER never
# captained - the case where comparing raw max(FPTS) picks the wrong winner.
suppressPackageStartupMessages({library(data.table); library(readxl)})
set.seed(22)

xlsx <- "C:/Users/astuk/OneDrive/Documents/GTS/MMA/UFC_Simulation_Input_8-22.xlsx"
out  <- file.path(tempdir(), "mma_sd_fixture.csv")

f  <- as.data.table(read_excel(xlsx, sheet = "Fights"))
sd <- f[SDSal > 0]
cat("showdown fighters:", nrow(sd), "bouts:", nrow(sd) / 2, "\n")
pool <- sd$Name

# Base (flex) scores. Two bouts decided, the rest not started.
base <- setNames(rep(0, length(pool)), pool)
base["Anthony Wint"]     <- 40.0   # wins
base["Terrance Chatman"] <- 30.0   # loses, but gets captained
base["Carli Judice"]     <- 12.0   # loses
base["Jeisla Chaves"]    <- 20.0   # wins

# Anthony Wint is never captained; Terrance Chatman is. So max(FPTS) across
# roster rows gives Chatman 45.0 vs Wint 40.0 - the wrong winner.
never_cpt <- "Anthony Wint"
cpt_pool  <- setdiff(pool, never_cpt)

n <- 3000
lineups <- vapply(seq_len(n), function(i) {
  six <- sample(pool, 6)
  cpt <- six[six %in% cpt_pool][1]
  if (is.na(cpt)) cpt <- setdiff(six, never_cpt)[1]
  rest <- setdiff(six, cpt)
  paste0("CPT ", cpt, " ", paste(paste("F", rest), collapse = " "))
}, character(1))

parse_one <- function(l) {
  toks <- strsplit(l, " ")[[1]]
  cur <- NA; buf <- c(); slot <- c(); nm <- c()
  for (t in toks) {
    if (t %in% c("CPT", "F")) {
      if (!is.na(cur)) { slot <- c(slot, cur); nm <- c(nm, paste(buf, collapse = " ")) }
      cur <- t; buf <- c()
    } else buf <- c(buf, t)
  }
  slot <- c(slot, cur); nm <- c(nm, paste(buf, collapse = " "))
  data.table(Slot = slot, Player = nm)
}
allp <- rbindlist(lapply(seq_along(lineups), function(i) {
  p <- parse_one(lineups[i]); p[, E := i]; p
}))

own <- allp[, .(Pct = .N / n * 100), by = .(Player, Slot)]
own[, FPTS := fifelse(Slot == "CPT", base[Player] * 1.5, base[Player])]
own <- own[order(-Pct)]
cat("ownership rows:", nrow(own), "\n")
cat("Wint slots:", paste(own[Player == "Anthony Wint", Slot], collapse = ","), "\n")
cat("Chatman rows:\n"); print(own[Player == "Terrance Chatman"])

pts <- allp[, .(P = sum(fifelse(Slot == "CPT", base[Player] * 1.5, base[Player]))), by = E]
decided <- names(base)[base > 0]
rem <- allp[, .(R = 6 - sum(Player %in% decided)), by = E]

hdr <- "Rank,EntryId,EntryName,TimeRemaining,Points,Lineup,,Player,Roster Position,%Drafted,FPTS"
users <- paste0("user", sprintf("%03d", sample(1:120, n, replace = TRUE)))
rows <- character(n)
for (i in seq_len(n)) {
  tail6 <- if (i <= nrow(own))
    paste0(own$Player[i], ",", own$Slot[i], ",",
           sprintf("%.2f%%", own$Pct[i]), ",", sprintf("%.2f", own$FPTS[i]))
  else ",,,"
  rows[i] <- paste0(i, ",", 6000000 + i, ",", users[i], ",", rem$R[i], ",",
                    sprintf("%.2f", pts$P[i]), ",", lineups[i], ",,", tail6)
}
writeLines(c(hdr, rows), out)
cat("wrote", out, "\n")
cat("expected: Anthony Wint WON, Terrance Chatman LOST, Jeisla Chaves WON, Carli Judice LOST\n")
