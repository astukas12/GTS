# add_dk_shutout(), copied verbatim from GTS/NHL/R/update_db.R (the only piece of it the sim needs).
# The only goalie who played the whole game for his team, with 0 GA, gets the DK shutout.
add_dk_shutout <- function(gg, tg) {
  ga_team <- tg[, .(gameId, teamId = opp_id, ga_team = fifelse(is.na(goals), 0L, goals))]
  gg <- merge(gg, ga_team, by = c("gameId", "teamId"), all.x = TRUE)
  gg[, n_played := sum(played), by = .(gameId, teamId)]
  gg[, dk_shutout := as.integer(played & n_played == 1L & ga_team %in% 0L)]
  gg[, c("n_played") := NULL]
  gg
}
