# Style-hold prototype test (branch cfb-style-hold). Read-only on tonight's sheet: works on a copy.
suppressMessages({ library(data.table); library(openxlsx) })
WT <- "C:/Users/astuk/GTS-cfb-stylehold/SimApp"; MAIN <- "C:/Users/astuk/OneDrive/Documents/GitHub/GTS/SimApp"
SRC <- "C:/Users/astuk/OneDrive/Documents/GTS/CFB/slates/2026-10-10_CFB_SAT_MAIN.xlsx"
OUTD <- file.path(WT, "ab_style_hold"); N <- as.integer(Sys.getenv("N", "4000"))
cp <- file.path(OUTD, "sheet_copy.xlsx"); file.copy(SRC, cp, overwrite = TRUE)
# flag WVU on its team tab: a style_hold row under the field/value block
wb <- loadWorkbook(cp); x <- read.xlsx(wb, "WVU", colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
pos <- which(x == "field", arr.ind = TRUE)[1, ]; col <- pos[["col"]]; fr <- which(!is.na(x[[col]]))
HW <- Sys.getenv("W", "")
fl <- data.frame(a = c("style_hold", if (nzchar(HW)) "style_hold_w"), b = c("2026 run-heavy profile (Andrew 10 Oct)", if (nzchar(HW)) HW))
writeData(wb, "WVU", fl, startCol = col, startRow = max(fr) + 1, colNames = FALSE)
cp2 <- file.path(OUTD, "sheet_copy_flag.xlsx"); saveWorkbook(wb, cp2, overwrite = TRUE)
one <- function(eng_dir, sheet, game_away, seed = 7) {
  E <- new.env(); old <- setwd(eng_dir); on.exit(setwd(old))
  suppressMessages(sys.source("cfb_engine.R", envir = E))
  inp <- E$read_cfb_input(sheet, slate = "classic_main")
  inp$game <- as.data.table(inp$game)[away == game_away]
  tms <- c(inp$game$away, inp$game$home)
  inp$players <- as.data.table(inp$players)[team %in% tms]; inp$team <- as.data.table(inp$team)[team %in% tms]
  r <- E$run_cfb_simulation(inp, n_sims = N, seed = seed, keep_components = TRUE)
  r
}
tl <- function(r) as.data.table(r$sport_visuals$team_line %||% r$sport_visuals$teams)
`%||%` <- function(a, b) if (is.null(a)) b else a
# 1. byte-identity: main vs branch, no flag, two games
for (g in c("ARIZ", "IU")) {
  a <- one(MAIN, cp, g); b <- one(WT, cp, g)
  same <- identical(a$sim_components, b$sim_components) && identical(a$sim_results, b$sim_results)
  cat(sprintf("byte-identical unflagged %s game: %s\n", g, same))
}
# 2. flagged WVU
base <- one(WT, cp, "ARIZ"); flag <- one(WT, cp2, "ARIZ")
print(names(flag$sport_visuals))
sm2 <- function(r) { d <- as.data.table(r$sport_visuals$team_dist)[Metric == "Points"]; x <- dcast(d, SimID ~ team, value.var = "Value"); sprintf("total %.1f margin %.1f", mean(x$ARIZ + x$WVU), mean(x$ARIZ - x$WVU)) }
cat("
points: base", sm2(base), " | flag", sm2(flag), "
")
saveRDS(list(base = base$sport_visuals, flag = flag$sport_visuals,
             base_c = base$sim_components, flag_c = flag$sim_components), file.path(OUTD, "wvu_test.rds"))
sm <- function(r) { C <- as.data.table(r$sim_components)
  C[, .(rec = sum(rec), ryds = sum(ryds), car = sum(car), cyds = sum(cyds), pyds = sum(pyds)), by = .(SimID, team)][
    , .(catches = round(mean(rec), 1), pass_yds = round(mean(pyds)), carries = round(mean(car), 1), rush_yds = round(mean(cyds))), by = team] }
cat("\nBASE (no flag):\n"); print(sm(base)); cat("FLAG (WVU style_hold):\n"); print(sm(flag))
pl <- function(r) { C <- as.data.table(r$sim_components); C[team == "WVU" & player %in% c("Michael Hawkins Jr.","Cam Cook","DJ Epps","Jaden Bray","John Neider"), .(rec = round(mean(rec), 1), ryds = round(mean(ryds)), car = round(mean(car), 1), cyds = round(mean(cyds)), pyds = round(mean(pyds)), dk = round(mean(dk), 1)), by = player][order(-dk)] }
cat("\nWVU players BASE:\n"); print(pl(base)); cat("WVU players FLAG:\n"); print(pl(flag))
