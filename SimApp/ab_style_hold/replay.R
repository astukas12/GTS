# =============================================================================
# replay.R -- style hold vs today's engine on would-be-flagged team-games, 2023-26 (branch cfb-style-hold).
# -----------------------------------------------------------------------------
# WOULD-BE-FLAGGED, no hindsight: from Review/engine/cfb_pys_backtest.rds features (as-of kickoff):
#   n1 >= 4 games this season and season-to-date pys (s1 / n1) more than .15 BELOW the cross-fitted regression ask
#   ("run" group; the mirror, .15 ABOVE, is reported as "pass" for reference).
# Per target team-game (team T, game g):
#   pool = shipped cfb_pool.rds minus g; market = g's own closing total / |spread| (pool columns total / absp);
#   style asks = g's pre-game reads (fO_pys / dO_pys), T's side replaced by the engine's 0.5 read + 0.5 reg ask.
#   BASE  = today's engine: E[T-side stat] under cfb_calibrate's weights (exact expectation, no sampling).
#   HOLD  = style hold: for each drawn game i, T's side comes from team-game j with prob ~ ws_j x N(pts_j - pts_i, sd 7),
#           ws = .5 own earlier games this season (game_id < g) + .5 look-alikes by realised pys / pass rate.
# Stats per T side: pass yds, catches, carries, rush yds (from cfb_events), and the game total (other side + T side pts).
# Errors vs what happened in g. OUT: replay_rows.csv, replay_summary.csv, REPLAY.md (this folder).
# RUN  Rscript replay.R    (from this folder; reads the worktree engine)
# =============================================================================
suppressMessages(library(data.table))
WT <- "C:/Users/astuk/GTS-cfb-stylehold/SimApp"; OUTD <- file.path(WT, "ab_style_hold")
E <- new.env(); old <- setwd(WT); suppressMessages(sys.source("cfb_engine.R", envir = E)); setwd(old)
P <- readRDS(file.path(WT, "cfb_data/cfb_pool.rds")); setDT(P)
P <- P[!(fteam %in% E$CFB_OPTION_TEAMS | dteam %in% E$CFB_OPTION_TEAMS)]
EV <- readRDS(file.path(WT, "cfb_data/cfb_events.rds")); setDT(EV)
AG <- EV[, .(catches = sum(kind == E$CFB_EVT_CMP), car = sum(kind == E$CFB_EVT_RUN),
             ryds = sum(yds[kind == E$CFB_EVT_RUN], na.rm = TRUE)), by = .(game_id, team = pos_team)]
TG <- rbind(P[, .(game_id, season, team = fteam, side = "f", pts = ptsF, pyds = fpyds, pys = fO_pys_out, pr = fO_pr_out, sw)],
            P[, .(game_id, season, team = dteam, side = "d", pts = ptsD, pyds = dpyds, pys = dO_pys_out, pr = dO_pr_out, sw)])
TG <- merge(TG, AG, by = c("game_id", "team"), all.x = TRUE)
TG <- TG[is.finite(pys) & is.finite(pr) & is.finite(pyds) & is.finite(catches)]
setkey(TG, game_id, side)
spys <- max(.25 * sd(TG$pys), .03); spr <- max(.25 * sd(TG$pr), .03)

X <- as.data.table(readRDS("C:/Users/astuk/OneDrive/Documents/GTS/Review/engine/cfb_pys_backtest.rds")$features)
X <- X[n1 >= as.integer(Sys.getenv("MINN", "4")) & is.finite(reg) & season >= 2023 & game_id %in% P$game_id]
X[, read := s1 / n1]
THR <- as.numeric(Sys.getenv("THR", ".15")); X[, grp := fifelse(read < reg - THR, "run", fifelse(read > reg + THR, "pass", NA_character_))]
T0 <- X[!is.na(grp)]
cat(sprintf("targets: %d run-heavy, %d pass-heavy team-games (2023-26, n1 >= 4)\n", T0[grp == "run", .N], T0[grp == "pass", .N]))

rows <- vector("list", nrow(T0))
for (i in seq_len(nrow(T0))) {
  t <- T0[i]; g <- P[game_id == t$game_id]; if (nrow(g) != 1) next
  s <- if (g$fteam == t$team) "f" else if (g$dteam == t$team) "d" else next
  Pi <- P[game_id != t$game_id]
  ask <- E$cfb_pys_blend(t$read, t$reg)
  tg <- list(total = g$total, absp = g$absp, fO_pr = .52, fO_pys = g$fO_pys, dO_pr = .52, dO_pys = g$dO_pys)
  tg[[paste0(s, "O_pys")]] <- ask
  cal <- tryCatch(E$cfb_calibrate(Pi, tg, list(total = g$total, margin = g$absp)), error = function(e) NULL)
  if (is.null(cal)) next
  w <- cal$w
  side_tg <- TG[J(Pi$game_id, s), on = .(game_id, side), mult = "first"]          # T-side stats of each drawn game
  oth_pts <- if (s == "f") Pi$ptsD else Pi$ptsF
  ok <- is.finite(side_tg$pyds) & is.finite(side_tg$catches)
  wb <- w * ok; wb <- wb / sum(wb)
  base <- c(pyds = sum(wb * side_tg$pyds, na.rm = TRUE), catches = sum(wb * side_tg$catches, na.rm = TRUE),
            car = sum(wb * side_tg$car, na.rm = TRUE), ryds = sum(wb * side_tg$ryds, na.rm = TRUE),
            total = sum(wb * (oth_pts + side_tg$pts), na.rm = TRUE))
  # style pool: own earlier games this season + look-alikes; never game g
  S <- TG[game_id != t$game_id]
  own <- S$team == t$team & S$season == t$season & S$game_id < t$game_id
  if (sum(own) < 2) next
  mp <- mean(S$pys[own]); mr <- mean(S$pr[own])
  wl <- exp(-((S$pys - mp)^2 / (2 * spys^2) + (S$pr - mr)^2 / (2 * spr^2))) * S$sw; wl[own] <- 0
  ws <- .5 * wl / sum(wl) + .5 * own / sum(own)
  keep <- which(ws > 1e-6 * max(ws)); S <- S[keep]; ws <- ws[keep]
  dpts <- if (s == "f") Pi$ptsF else Pi$ptsD
  live <- which(wb > 0)
  K <- exp(-outer(dpts[live], S$pts, "-")^2 / (2 * 7^2)) * rep(ws, each = length(live))
  K <- K / rowSums(K)
  ex <- function(v) sum(wb[live] * (K %*% v))
  hold <- c(pyds = ex(S$pyds), catches = ex(S$catches), car = ex(S$car), ryds = ex(S$ryds),
            total = sum(wb[live] * (oth_pts[live] + K %*% S$pts)))
  act_tg <- TG[J(t$game_id, s), on = .(game_id, side)]
  act <- c(pyds = act_tg$pyds, catches = act_tg$catches, car = act_tg$car, ryds = act_tg$ryds, total = g$ptsF + g$ptsD)
  rows[[i]] <- data.table(game_id = t$game_id, season = t$season, team = t$team, grp = t$grp, read = round(t$read, 3),
                          reg = round(t$reg, 3), ess = round(cal$ess), stat = names(act),
                          actual = act, base = base[names(act)], hold = hold[names(act)])
  if (i %% 25 == 0) cat(sprintf("  %d / %d\n", i, nrow(T0)))
}
R <- rbindlist(rows)
fwrite(R, file.path(OUTD, "replay_rows.csv"))
SM <- R[is.finite(actual), .(n = .N, rmse_base = round(sqrt(mean((base - actual)^2)), 2), rmse_hold = round(sqrt(mean((hold - actual)^2)), 2),
                             bias_base = round(mean(base - actual), 2), bias_hold = round(mean(hold - actual), 2),
                             hold_better = round(mean(abs(hold - actual) < abs(base - actual)), 3)), by = .(grp, stat)][order(grp, stat)]
fwrite(SM, file.path(OUTD, "replay_summary.csv"))
md <- c("# Style hold replay (2023-26, would-be-flagged team-games; expectation, no sampling)", "",
        "| group | stat | n | RMSE today | RMSE hold | bias today | bias hold | hold closer |", "| --- | --- | --- | --- | --- | --- | --- | --- |",
        SM[, sprintf("| %s | %s | %d | %.2f | %.2f | %+.2f | %+.2f | %.0f%% |", grp, stat, n, rmse_base, rmse_hold, bias_base, bias_hold, 100 * hold_better)])
writeLines(md, file.path(OUTD, "REPLAY.md")); cat(md, sep = "\n")
