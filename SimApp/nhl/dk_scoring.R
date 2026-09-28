# =============================================================================
# dk_scoring.R -- DraftKings NHL classic scoring
# -----------------------------------------------------------------------------
# CONFIRMED 24 Sep 2026 against the DK classic rules page (pasted by Andrew;
# the page refuses automated fetches). Goalies score their goals and assists
# too. The shutout is DK's definition, computed in update_db.R
# (add_dk_shutout), not the NHL stat. Every component is stored on the row, so
# a change here is a rescore, not a rebuild: run rescore_dk() in update_db.R.
#
# Showdown is NOT yet confirmed (the pasted page was classic only). Assumed
# same points with a 1.5x captain, which would be a lineup rule, not scoring.
# =============================================================================

DK_NHL <- list(
  goal = 8.5, assist = 5, sog = 1.5, block = 1.3,
  sh_point = 2,          # bonus per short-handed goal or assist, on top of G/A
  so_goal = 1.5,         # shootout goal; SO goals are not goals for anything else
  hat_trick = 3,         # 3+ goals (shootout goals do not count)
  sog5 = 3,              # 5+ shots on goal
  blk3 = 3,              # 3+ blocked shots
  pts3 = 3,              # 3+ points
  # goalies
  win = 6, otl = 2,      # decision W (incl. OT/SO win) / O (OT or SO loss)
  save = 0.7, ga = -3.5, # GA excludes empty-net goals: the goalie was not in net
  shutout = 4,           # DK: whole game, only goalie of record, 0 GA in reg + OT
  saves35 = 3
)

# Vectorised over a data.table's columns. NAs count as 0: a stat the feed did
# not report was not recorded, and DK scores what was recorded.
dk_skater_points <- function(g, a, sog, blk, sh_pts, so_g, s = DK_NHL) {
  z <- function(x) fifelse(is.na(x), 0, as.numeric(x))
  g <- z(g); a <- z(a); sog <- z(sog); blk <- z(blk); sh_pts <- z(sh_pts); so_g <- z(so_g)
  s$goal * g + s$assist * a + s$sog * sog + s$block * blk +
    s$sh_point * sh_pts + s$so_goal * so_g +
    s$hat_trick * (g >= 3) + s$sog5 * (sog >= 5) + s$blk3 * (blk >= 3) + s$pts3 * (g + a >= 3)
}

dk_goalie_points <- function(decision, saves, ga, shutout, g = 0, a = 0, s = DK_NHL) {
  z <- function(x) fifelse(is.na(x), 0, as.numeric(x))
  saves <- z(saves)
  s$win * (decision %chin% "W") + s$otl * (decision %chin% "O") +
    s$save * saves + s$ga * z(ga) + s$shutout * (z(shutout) > 0) + s$saves35 * (saves >= 35) +
    s$goal * z(g) + s$assist * z(a)
}
