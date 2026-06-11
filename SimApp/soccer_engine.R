# ============================================================================
# SOCCER SIMULATION ENGINE — Golden Ticket Sims
# ============================================================================
# Matrix-based: n_players × n_sims. Scorelines vectorized, team totals batched
# by goal count, independent stats via vectorized NegBin. ~50-100x faster than
# per-sim loop version.
# ============================================================================

library(data.table)

# ── PARAMETERS (from Big 5 + WC 2022 research) ──────────────────────────────

SOCCER_P <- list(
  rho = -0.13, max_goals = 5,
  phi_excess_sot = 9.0, phi_off_target = 8.5, phi_fouls = 47.5,
  phi_crosses = 8.2, phi_passes = 7.2, phi_cc = 3.0,
  phi_tackles = 5.0, phi_int = 5.0, phi_fd = 5.0,
  shots_scale   = c(0.857, 0.962, 1.048, 1.164, 1.281, 1.426),
  fouls_scale   = c(1.005, 1.020, 1.002, 0.975, 0.930, 0.867),
  excess_sot_mu = c(2.7, 2.9, 3.0, 3.3, 3.5, 3.7),
  p_assist = 0.709, yc_per_foul = 0.174,
  p_second_yc = 0.018, p_straight_red = 0.0026
)

# ── HELPERS ──────────────────────────────────────────────────────────────────

tau_dc <- function(x, y, l1, l2, rho) {
  if (x==0 && y==0) return(1 - l1*l2*rho)
  if (x==0 && y==1) return(1 + l1*rho)
  if (x==1 && y==0) return(1 + l2*rho)
  if (x==1 && y==1) return(1 - rho)
  1
}

build_grid <- function(lh, la, rho=SOCCER_P$rho, mg=SOCCER_P$max_goals) {
  g <- CJ(hg=0:mg, ag=0:mg)
  g[, prob := dpois(hg,lh)*dpois(ag,la)*
      mapply(tau_dc, hg, ag, MoreArgs=list(l1=lh,l2=la,rho=rho))]
  g[, prob := prob/sum(prob)]; g
}

rnb <- function(n, mu, phi) {
  if (length(mu)==1 && mu<=0) return(rep(0L,n))
  mu <- pmax(mu, 0)
  if (phi==Inf) return(rpois(n, mu))
  as.integer(rnbinom(n, size=phi, mu=mu))
}

# Normalize raw draw matrix to team totals (NBA pattern)
norm_to_total <- function(raw, totals, n_p) {
  cs <- colSums(raw)
  cs[cs==0] <- 1
  scaled <- sweep(raw, 2, totals/cs, `*`)
  fl <- matrix(as.integer(floor(scaled)), n_p, length(totals))
  resid <- as.integer(round(totals)) - colSums(fl)
  frac <- scaled - fl
  for (s in seq_along(totals)) {
    r <- resid[s]; if (r==0 || !is.finite(r)) next
    if (r>0) { top <- order(frac[,s], decreasing=TRUE)[seq_len(min(r,n_p))]
    fl[top,s] <- fl[top,s]+1L
    } else {  elig <- which(fl[,s]>0); if(!length(elig)) next
    ord <- elig[order(frac[elig,s])]; sub_n <- seq_len(min(abs(r),length(ord)))
    fl[ord[sub_n],s] <- fl[ord[sub_n],s]-1L }
  }; fl
}

# ── DK SCORING (vectorized) ─────────────────────────────────────────────────

dk_score_soccer_v <- function(pos, goals, assists, shots, sot, cc, passes,
                              crosses, tklw, ints, fd, fc, yc, rc,
                              cs, gk_saves, gk_gc, gk_win, mins) {
  s <- goals*10 + assists*6 + shots + sot + cc + passes*0.02 +
    crosses*0.7 + tklw + ints*0.5 + fd - fc*0.5
  # Cards: yc=1 → -1.5; yc=2 (double yellow) → -3.0
  s <- s + ifelse(yc>=2, -3.0, ifelse(yc==1, -1.5, 0))
  s <- s + rc * (-3.0)  # straight red on top
  # Clean sheet (D, 60+ min)
  is_d <- grepl("D", pos) & !grepl("G", pos)
  s <- s + ifelse(is_d & mins>=60 & cs==1, 3, 0)
  # GK
  is_gk <- grepl("^G", pos)
  s <- s + ifelse(is_gk, gk_saves*2 + gk_gc*(-2), 0)
  s <- s + ifelse(is_gk & mins>=60 & cs==1, 5, 0)
  s <- s + ifelse(is_gk & mins>=90 & gk_win==1, 5, 0)
  s
}

# ── INPUT READER ─────────────────────────────────────────────────────────────

read_soccer_input <- function(file_path) {
  sheets <- readxl::excel_sheets(file_path)
  data <- setNames(lapply(sheets, function(s) {
    dt <- as.data.table(readxl::read_excel(file_path, sheet=s))
    setnames(dt, trimws(names(dt))); dt
  }), sheets)
  
  pl <- as.data.table(data$Players)
  gm <- as.data.table(data$Games)
  
  num_p <- c("MIN","G","A","S","SOG","CC","P","CR","INT","TKLW","FS","FC","Y","R",
             "CS","GC","SV","Goal_Share","Assist_Share","Shot_Share",
             "SOG_Share","Foul_Share","CC_Share","DK_Salary")
  for (col in intersect(num_p, names(pl))) pl[[col]] <- as.numeric(pl[[col]])
  num_g <- c("Home_Lambda","Away_Lambda","Total","Home_Shots","Away_Shots",
             "Home_SOT","Away_SOT","Home_Fouls","Away_Fouls",
             "Home_Crosses","Away_Crosses","Home_Passes","Away_Passes")
  for (col in intersect(num_g, names(gm))) gm[[col]] <- as.numeric(gm[[col]])
  
  sd_tabs <- list()
  for (sn in grep("^SD\\d+_IDs$", sheets, value=TRUE)) {
    sd_tabs[[sub("_IDs$","",sn)]] <- data[[sn]]
  }
  
  # Add ShowdownFile + GameKey to Games for SD pill selector
  gm[, GameKey := Game]
  gm[, ShowdownFile := ""]
  for (sdn in names(sd_tabs)) {
    sd_dt <- as.data.table(sd_tabs[[sdn]])
    team_col <- intersect(c("Team","TeamAbbrev"), names(sd_dt))[1]
    if (!is.na(team_col)) {
      sd_teams <- unique(sd_dt[[team_col]])
      for (r in seq_len(nrow(gm))) {
        if (gm$Home[r] %in% sd_teams || gm$Away[r] %in% sd_teams) {
          gm[r, ShowdownFile := sdn]
        }
      }
    }
  }
  
  cat(sprintf("Soccer: %d players | %d games | %d SD tabs\n",
              nrow(pl), nrow(gm), length(sd_tabs)))
  list(Players=pl, Games=gm, IDs=data$IDs, sd_tabs=sd_tabs,
       games=gm,  # lowercase 'games' for app SD selector
       all_sheets=data)
}

# ── MAIN ENGINE ──────────────────────────────────────────────────────────────

run_soccer_simulation <- function(input_data, n_sims=10000, config=NULL,
                                  progress_callback=NULL) {
  t0 <- proc.time()["elapsed"]
  cb <- function(d,v) {
    if (!is.null(progress_callback)) progress_callback(d,v)
    cat(sprintf("  [%.0f%%] %s\n", v*100, d)); flush.console()
  }
  cb("Loading input...", 0.01)
  
  # ── Read input ──
  if ("Players" %in% names(input_data)) {
    players <- as.data.table(input_data$Players)
    games   <- as.data.table(input_data$Games)
  } else {
    players <- as.data.table(input_data[["Players"]])
    games   <- as.data.table(input_data[["Games"]])
  }
  
  num_p <- c("MIN","G","A","S","SOG","CC","P","CR","INT","TKLW","FS","FC","Y","R",
             "CS","GC","SV","Goal_Share","Assist_Share","Shot_Share",
             "SOG_Share","Foul_Share","CC_Share","DK_Salary")
  for (col in intersect(num_p, names(players))) players[[col]] <- as.numeric(players[[col]])
  num_g <- c("Home_Lambda","Away_Lambda","Home_Shots","Away_Shots","Home_Fouls","Away_Fouls")
  for (col in intersect(num_g, names(games))) games[[col]] <- as.numeric(games[[col]])
  
  players <- players[!is.na(MIN) & MIN > 0]
  n_games <- nrow(games)
  cat(sprintf("  %d players | %d games | %s sims\n",
              nrow(players), n_games, format(n_sims, big.mark=",")))
  
  # ── SD detection ──
  sd_tabs <- if ("sd_tabs" %in% names(input_data)) input_data$sd_tabs else list()
  # Also check for SD tabs from generic app loader
  if (!length(sd_tabs)) {
    for (nm in names(input_data)) {
      if (grepl("^SD\\d+", nm)) sd_tabs[[nm]] <- as.data.table(input_data[[nm]])
    }
  }
  has_sd <- length(sd_tabs) > 0
  if (has_sd) {
    tryCatch({
      sd_all <- rbindlist(sd_tabs, fill=TRUE)
      # Use whatever column names exist — don't force rename
      # Find the name, ID, roster position, and salary columns
      nm_col  <- intersect(c("Name","Player"), names(sd_all))[1]
      id_col  <- intersect(c("ID","DKID"), names(sd_all))[1]
      rp_col  <- intersect(c("RosterPos","Roster Position","Roster.Position"), names(sd_all))[1]
      sal_col <- intersect(c("Salary","DKSalary"), names(sd_all))[1]
      
      if (!is.na(nm_col) && !is.na(id_col) && !is.na(rp_col) && !is.na(sal_col)) {
        sd_all[, c("sd_name","sd_id","sd_rp","sd_sal") := .(
          as.character(get(nm_col)), as.character(get(id_col)),
          as.character(get(rp_col)), as.numeric(get(sal_col))
        )]
        sd_lookup <- unique(sd_all[sd_rp == "CPT", .(sd_name, CPTID = sd_id, CPTSalary = sd_sal)])
        sd_flex   <- unique(sd_all[sd_rp == "FLEX", .(sd_name, SDID = sd_id, SDSalary = sd_sal)])
        sd_merged <- merge(sd_lookup, sd_flex, by="sd_name", all=TRUE)
        if ("DK_Name" %in% names(players)) {
          players <- merge(players, sd_merged, by.x="DK_Name", by.y="sd_name", all.x=TRUE)
        }
        cat(sprintf("  SD merged: %d CPT + %d FLEX entries\n", nrow(sd_lookup), nrow(sd_flex)))
      }
    }, error = function(e) {
      cat(sprintf("  SD merge warning: %s\n", conditionMessage(e)))
    })
  }
  
  # ── Per-game setup: scoreline grids ──
  cb("Building scoreline grids...", 0.03)
  game_info <- list()
  
  # Check for scoreline odds tabs in the input data
  all_sheet_names <- names(input_data)
  
  for (gi in seq_len(n_games)) {
    g <- games[gi]
    
    # Look for a matching odds tab (e.g., "MEXvsRSA" or "MEX vs RSA")
    odds_tab_name <- paste0(g$Home, "vs", g$Away)
    odds_tab_alt  <- paste0(g$Home, " vs ", g$Away)
    odds_tab <- NULL
    for (tn in c(odds_tab_name, odds_tab_alt, g$Game)) {
      if (tn %in% all_sheet_names) {
        odds_tab <- as.data.table(input_data[[tn]])
        break
      }
    }
    
    if (!is.null(odds_tab) && nrow(odds_tab) > 1) {
      # Use scoreline odds from input sheet
      # Format: col1=home_goals, col2=away_goals, col3=american_odds
      setnames(odds_tab, c("hg", "ag", "odds"))
      odds_tab[, hg := as.integer(hg)]
      odds_tab[, ag := as.integer(ag)]
      odds_tab[, odds := as.numeric(odds)]
      odds_tab <- odds_tab[!is.na(hg) & !is.na(ag) & !is.na(odds)]
      
      # Convert American odds to implied probability and devig
      odds_tab[, implied := ifelse(odds > 0, 100 / (odds + 100), abs(odds) / (abs(odds) + 100))]
      odds_tab[, prob := implied / sum(implied)]
      
      # Draw scorelines from devigged odds
      draws <- sample.int(nrow(odds_tab), n_sims, replace = TRUE, prob = odds_tab$prob)
      hg_vec <- odds_tab$hg[draws]
      ag_vec <- odds_tab$ag[draws]
      
      cat(sprintf("  %s: ODDS-BASED (%d scorelines) | implied total %.2f\n",
                  g$Game, nrow(odds_tab),
                  sum(odds_tab$hg * odds_tab$prob) + sum(odds_tab$ag * odds_tab$prob)))
    } else {
      # Fall back to Dixon-Coles from projection lambdas
      grid <- build_grid(g$Home_Lambda, g$Away_Lambda)
      draws <- sample.int(nrow(grid), n_sims, replace = TRUE, prob = grid$prob)
      hg_vec <- grid$hg[draws]
      ag_vec <- grid$ag[draws]
      
      cat(sprintf("  %s: DIXON-COLES (lambda %.2f vs %.2f)\n",
                  g$Game, g$Home_Lambda, g$Away_Lambda))
    }
    
    hp <- players[Team == g$Home & Opp == g$Away]
    ap <- players[Team == g$Away & Opp == g$Home]
    
    game_info[[gi]] <- list(
      game=g, hg=hg_vec, ag=ag_vec,
      hp=hp, ap=ap, n_hp=nrow(hp), n_ap=nrow(ap)
    )
    cat(sprintf("    %d+%d players | Home win %.1f%% Draw %.1f%% Away %.1f%%\n",
                nrow(hp), nrow(ap),
                mean(hg_vec > ag_vec)*100, mean(hg_vec == ag_vec)*100,
                mean(hg_vec < ag_vec)*100))
  }
  
  # ── Allocate stat matrices ──
  all_players_list <- rbindlist(lapply(game_info, function(gi) rbind(gi$hp, gi$ap)))
  # Deduplicate (same player shouldn't appear twice, but just in case)
  n_total <- nrow(all_players_list)
  stat_names <- c("Goals","Assists","Shots","SOT","CC","Passes","Crosses",
                  "TKLW","INT","FD","FC","YC","RC","GK_Saves","GK_GC","CS","GK_Win")
  # Master matrices: n_total_players × n_sims
  mats <- setNames(lapply(stat_names, function(s) matrix(0, n_total, n_sims)), stat_names)
  dk_mat <- matrix(0, n_total, n_sims)
  
  cb("Simulating matches...", 0.08)
  
  player_offset <- 0L
  
  for (gi in seq_len(n_games)) {
    gd <- game_info[[gi]]
    pct_base <- 0.08 + (gi-1)/n_games * 0.72
    
    # Process each side
    for (side in c("home","away")) {
      pl <- if (side=="home") gd$hp else gd$ap
      n_p <- nrow(pl)
      if (n_p == 0) next
      
      team_goals <- if (side=="home") gd$hg else gd$ag
      opp_goals  <- if (side=="home") gd$ag else gd$hg
      shots_mu   <- if (side=="home") gd$game$Home_Shots else gd$game$Away_Shots
      fouls_mu   <- if (side=="home") gd$game$Home_Fouls else gd$game$Away_Fouls
      
      pidx <- (player_offset+1):(player_offset+n_p)
      mins <- pl$MIN
      ms   <- mins / 90
      
      # ── GOALS: multinomial batched by goal count ──
      g_shares <- pmax(pl$Goal_Share, 0.001)
      g_shares <- g_shares / sum(g_shares)
      goals_m  <- matrix(0L, n_p, n_sims)
      
      for (gc in 0:SOCCER_P$max_goals) {
        sim_idx <- which(team_goals == gc)
        if (!length(sim_idx) || gc == 0) next
        alloc <- rmultinom(length(sim_idx), size=gc, prob=g_shares)
        goals_m[, sim_idx] <- alloc
      }
      mats$Goals[pidx, ] <- goals_m
      
      # ── ASSISTS: ~71% of goals get one ──
      a_shares <- pmax(pl$Assist_Share, 0.001)
      a_shares <- a_shares / sum(a_shares)
      assists_m <- matrix(0L, n_p, n_sims)
      
      for (s in seq_len(n_sims)) {
        tg <- team_goals[s]; if (tg == 0) next
        n_ast <- rbinom(1, tg, SOCCER_P$p_assist)
        if (n_ast == 0) next
        assists_m[, s] <- rmultinom(1, size=n_ast, prob=a_shares)[,1]
      }
      mats$Assists[pidx, ] <- assists_m
      
      # ── TEAM SHOT TOTALS (vectorized) ──
      gi_vec <- pmin(team_goals, 5L) + 1L
      excess_mu <- SOCCER_P$excess_sot_mu[gi_vec]
      excess_sot <- rnb(n_sims, excess_mu, SOCCER_P$phi_excess_sot)
      t_sot <- team_goals + excess_sot
      offtgt_mu <- pmax(shots_mu * SOCCER_P$shots_scale[gi_vec] - t_sot, 1)
      t_off <- rnb(n_sims, offtgt_mu, SOCCER_P$phi_off_target)
      t_shots <- t_sot + t_off
      
      # SOT allocation: goals are SOT, distribute remaining (saved shots)
      saved_shots <- t_sot - team_goals  # vector of length n_sims
      s_shares <- pmax(pl$Shot_Share, 0.001)
      s_shares <- s_shares / sum(s_shares)
      
      # Draw raw shots per player, normalize to saved_shots
      raw_sot <- matrix(rnb(n_p*n_sims, rep(pl$SOG*ms, n_sims), SOCCER_P$phi_excess_sot),
                        n_p, n_sims)
      saved_sot_m <- norm_to_total(raw_sot, saved_shots, n_p)
      sot_m <- goals_m + saved_sot_m
      mats$SOT[pidx, ] <- sot_m
      
      # Off-target allocation
      raw_off <- matrix(rnb(n_p*n_sims, rep(pl$S*ms*0.6, n_sims), SOCCER_P$phi_off_target),
                        n_p, n_sims)
      off_m <- norm_to_total(raw_off, t_off, n_p)
      mats$Shots[pidx, ] <- sot_m + off_m
      
      # ── FOULS (team total drawn, allocated to players) ──
      t_fouls <- rnb(n_sims, fouls_mu * SOCCER_P$fouls_scale[gi_vec], SOCCER_P$phi_fouls)
      raw_fc <- matrix(rnb(n_p*n_sims, rep(pl$FC*ms, n_sims), SOCCER_P$phi_fouls),
                       n_p, n_sims)
      fc_m <- norm_to_total(raw_fc, t_fouls, n_p)
      mats$FC[pidx, ] <- fc_m
      
      # ── INDEPENDENT STATS (vectorized NegBin per player) ──
      mats$CC[pidx, ]      <- matrix(rnb(n_p*n_sims, rep(pl$CC  *ms, n_sims), SOCCER_P$phi_cc),      n_p, n_sims)
      mats$Passes[pidx, ]  <- matrix(rnb(n_p*n_sims, rep(pl$P   *ms, n_sims), SOCCER_P$phi_passes),  n_p, n_sims)
      mats$Crosses[pidx, ] <- matrix(rnb(n_p*n_sims, rep(pl$CR  *ms, n_sims), SOCCER_P$phi_crosses), n_p, n_sims)
      mats$TKLW[pidx, ]    <- matrix(rnb(n_p*n_sims, rep(pl$TKLW*ms, n_sims), SOCCER_P$phi_tackles), n_p, n_sims)
      mats$INT[pidx, ]     <- matrix(rnb(n_p*n_sims, rep(pl$INT *ms, n_sims), SOCCER_P$phi_int),     n_p, n_sims)
      mats$FD[pidx, ]      <- matrix(rnb(n_p*n_sims, rep(pl$FS  *ms, n_sims), SOCCER_P$phi_fd),      n_p, n_sims)
      
      # ── REALISTIC CAPS (historical per-player-per-game maximums) ──
      # Prevents NegBin tail from producing impossible stat lines
      mats$CC[pidx, ]      <- pmin(mats$CC[pidx, ],      8L)   # max chances created
      mats$Passes[pidx, ]  <- pmin(mats$Passes[pidx, ],  130L) # extreme possession mid
      mats$Crosses[pidx, ] <- pmin(mats$Crosses[pidx, ], 18L)  # extreme wingback
      mats$TKLW[pidx, ]    <- pmin(mats$TKLW[pidx, ],    10L)  # extreme defensive game
      mats$INT[pidx, ]     <- pmin(mats$INT[pidx, ],     8L)
      mats$FD[pidx, ]      <- pmin(mats$FD[pidx, ],      8L)
      mats$Shots[pidx, ]   <- pmin(mats$Shots[pidx, ],   12L)  # per player shot cap
      mats$SOT[pidx, ]     <- pmin(mats$SOT[pidx, ],     8L)
      fc_m                 <- pmin(fc_m,                  6L)   # 6+ fouls → almost certainly sent off
      mats$FC[pidx, ]      <- fc_m
      
      # ── CARDS (vectorized, conditional on fouls) ──
      fc_flat <- as.vector(fc_m)
      p_yc <- 1 - (1 - SOCCER_P$yc_per_foul) ^ fc_flat
      yc1 <- as.integer(runif(n_p*n_sims) < p_yc)
      yc2 <- as.integer(yc1 & (runif(n_p*n_sims) < SOCCER_P$p_second_yc))
      str_red <- as.integer(runif(n_p*n_sims) < SOCCER_P$p_straight_red)
      mats$YC[pidx, ] <- matrix(yc1 + yc2, n_p, n_sims)
      mats$RC[pidx, ] <- matrix(pmin(str_red + yc2, 1L), n_p, n_sims)  # cap at 1
      
      # ── CLEAN SHEET + GK ──
      cs_vec <- as.integer(opp_goals == 0)
      mats$CS[pidx, ] <- matrix(rep(cs_vec, each=n_p), n_p, n_sims)
      
      win_vec <- as.integer(team_goals > opp_goals)
      gk_idx_local <- which(grepl("^G", pl$Pos) & mins >= 60)
      if (length(gk_idx_local)) {
        pk <- gk_idx_local[1]  # primary GK
        # Opponent SOT for saves: need to compute from other side
        opp_gi <- pmin(opp_goals, 5L) + 1L
        opp_excess <- rnb(n_sims, SOCCER_P$excess_sot_mu[opp_gi], SOCCER_P$phi_excess_sot)
        opp_sot <- opp_goals + opp_excess
        gk_saves_vec <- opp_sot - opp_goals  # = opp_excess
        mats$GK_Saves[pidx[pk], ] <- gk_saves_vec
        mats$GK_GC[pidx[pk], ]    <- opp_goals
      }
      mats$GK_Win[pidx, ] <- matrix(rep(win_vec, each=n_p), n_p, n_sims)
      
      player_offset <- player_offset + n_p
    }
    cb(sprintf("Game %d/%d: %s complete", gi, n_games, gd$game$Game),
       pct_base + 0.72/n_games)
  }
  
  # ── DK SCORES ──
  cb("Calculating DK scores...", 0.82)
  pos_vec <- all_players_list$Pos
  mins_vec <- all_players_list$MIN
  
  dk_mat <- dk_score_soccer_v(
    pos   = rep(pos_vec, n_sims),
    goals = as.vector(mats$Goals), assists = as.vector(mats$Assists),
    shots = as.vector(mats$Shots), sot = as.vector(mats$SOT),
    cc = as.vector(mats$CC), passes = as.vector(mats$Passes),
    crosses = as.vector(mats$Crosses), tklw = as.vector(mats$TKLW),
    ints = as.vector(mats$INT), fd = as.vector(mats$FD),
    fc = as.vector(mats$FC), yc = as.vector(mats$YC), rc = as.vector(mats$RC),
    cs = as.vector(mats$CS), gk_saves = as.vector(mats$GK_Saves),
    gk_gc = as.vector(mats$GK_GC), gk_win = as.vector(mats$GK_Win),
    mins = rep(mins_vec, n_sims)
  )
  dk_mat <- matrix(dk_mat, n_total, n_sims)
  
  # ── BUILD sim_results (long format with all stat components) ──
  # dk_mat is n_total × n_sims. as.vector() reads column-major:
  # [p1_s1, p2_s1, ..., pN_s1, p1_s2, p2_s2, ...] which matches
  # SimID = rep(1:n_sims, each=n_total) and Player = rep(names, n_sims)
  # DO NOT transpose — t() would give [p1_s1, p1_s2, ..., p1_sN, p2_s1, ...] 
  # which scrambles the alignment.
  cb("Building results table...", 0.88)
  sim_results <- data.table(
    SimID    = rep(seq_len(n_sims), each=n_total),
    Player   = rep(all_players_list$Player, n_sims),
    Team     = rep(all_players_list$Team, n_sims),
    Pos      = rep(all_players_list$Pos, n_sims),
    DKScore  = as.vector(dk_mat),
    Goals    = as.vector(mats$Goals),
    Assists  = as.vector(mats$Assists),
    Shots    = as.vector(mats$Shots),
    SOT      = as.vector(mats$SOT),
    CC       = as.vector(mats$CC),
    Passes   = as.vector(mats$Passes),
    Crosses  = as.vector(mats$Crosses),
    TKLW     = as.vector(mats$TKLW),
    INT      = as.vector(mats$INT),
    FD       = as.vector(mats$FD),
    FC       = as.vector(mats$FC),
    YC       = as.vector(mats$YC),
    RC       = as.vector(mats$RC),
    GK_Saves = as.vector(mats$GK_Saves),
    GK_GC    = as.vector(mats$GK_GC),
    CS       = as.vector(mats$CS),
    GK_Win   = as.vector(mats$GK_Win)
  )
  
  # ── METADATA ──
  cb("Building metadata...", 0.92)
  
  meta_cols <- c("Player","Team","Opp","Pos","DK_Salary","DK_ID","DK_Name","DK_RosterPos")
  if (has_sd) meta_cols <- c(meta_cols, "CPTID","CPTSalary","SDID","SDSalary")
  avail <- intersect(meta_cols, names(all_players_list))
  
  metadata <- unique(all_players_list[, ..avail], by="Player")
  setnames(metadata, c("DK_Salary","DK_ID"), c("DKSalary","DKID"), skip_absent=TRUE)
  metadata[, DKOwn := 0]
  metadata[, PosGroup := Pos]
  # DKPos for optimizer uses DK roster position (F/UTIL, M/F/UTIL, D/UTIL, GK)
  if ("DK_RosterPos" %in% names(metadata)) {
    metadata[, DKPos := DK_RosterPos]
  } else {
    metadata[, DKPos := Pos]
  }
  metadata[, GameKey := paste0(Team, " vs ", Opp)]
  
  # GameRank from Games tab order (row 1 = earliest game = rank 1)
  game_rank_map <- data.table(
    Team_h = games$Home,
    Team_a = games$Away,
    rank   = seq_len(nrow(games))
  )
  metadata[, GameRank := {
    r <- game_rank_map[Team_h == Team | Team_a == Team, rank]
    if (length(r)) r[1] else 1L
  }, by = Player]
  
  # ShowdownFile: map players to their SD game (SD1, SD2, etc.)
  metadata[, ShowdownFile := ""]
  if (has_sd) {
    sd_names <- names(sd_tabs)
    for (sdn in sd_names) {
      sd_dt <- as.data.table(sd_tabs[[sdn]])
      sd_team_col <- intersect(c("Team","TeamAbbrev"), names(sd_dt))[1]
      if (!is.na(sd_team_col)) {
        sd_teams <- unique(sd_dt[[sd_team_col]])
        metadata[Team %in% sd_teams & ShowdownFile == "", ShowdownFile := sdn]
      }
    }
  }
  
  # ── SPORT VISUALS ──
  cb("Building visualizations...", 0.95)
  
  player_names <- all_players_list$Player
  player_teams <- all_players_list$Team
  
  # Player means (stat averages across sims)
  player_means <- data.table(
    Player   = player_names,
    Team     = player_teams,
    Pos      = all_players_list$Pos,
    Salary   = all_players_list$DK_Salary,
    DKAvgFP  = round(rowMeans(dk_mat), 2),
    AvgFP    = round(rowMeans(dk_mat), 2),
    SDFP     = round(apply(dk_mat, 1, sd), 2),
    P10      = round(apply(dk_mat, 1, quantile, 0.10), 2),
    P25      = round(apply(dk_mat, 1, quantile, 0.25), 2),
    P50      = round(apply(dk_mat, 1, function(x) median(x)), 2),
    P75      = round(apply(dk_mat, 1, quantile, 0.75), 2),
    P90      = round(apply(dk_mat, 1, quantile, 0.90), 2),
    Ceiling  = round(apply(dk_mat, 1, quantile, 0.99), 2),
    Floor    = round(apply(dk_mat, 1, quantile, 0.01), 2),
    AvgGoals = round(rowMeans(mats$Goals), 3),
    AvgAst   = round(rowMeans(mats$Assists), 3),
    AvgShots = round(rowMeans(mats$Shots), 1),
    AvgSOT   = round(rowMeans(mats$SOT), 1),
    AvgCC    = round(rowMeans(mats$CC), 1),
    AvgPass  = round(rowMeans(mats$Passes), 0),
    AvgCross = round(rowMeans(mats$Crosses), 1),
    AvgTKLW  = round(rowMeans(mats$TKLW), 1),
    AvgINT   = round(rowMeans(mats$INT), 1),
    AvgFD    = round(rowMeans(mats$FD), 1),
    AvgFC    = round(rowMeans(mats$FC), 1),
    AvgYC    = round(rowMeans(mats$YC), 3),
    AvgSaves = round(rowMeans(mats$GK_Saves), 2)
  )
  player_means[, Value := round(DKAvgFP / pmax(Salary/1000, 1), 2)]
  
  # DK score component breakdown (avg pts from each stat category)
  player_means[, `:=`(
    Pts_Goals   = round(AvgGoals * 10, 2),
    Pts_Assists = round(AvgAst * 6, 2),
    Pts_Shots   = round(AvgShots * 1, 2),
    Pts_SOT     = round(AvgSOT * 1, 2),
    Pts_CC      = round(AvgCC * 1, 2),
    Pts_Passes  = round(AvgPass * 0.02, 2),
    Pts_Crosses = round(AvgCross * 0.7, 2),
    Pts_TKLW    = round(AvgTKLW * 1, 2),
    Pts_INT     = round(AvgINT * 0.5, 2),
    Pts_FD      = round(AvgFD * 1, 2),
    Pts_FC      = round(AvgFC * -0.5, 2),
    Pts_YC      = round(AvgYC * -1.5, 2),
    Pts_Saves   = round(AvgSaves * 2, 2)
  )]
  
  setorder(player_means, -DKAvgFP)
  
  # Team means
  teams <- sort(unique(player_teams))
  team_means <- rbindlist(lapply(teams, function(tm) {
    idx <- which(player_teams == tm)
    data.table(
      Team     = tm,
      DKAvgFP  = round(mean(colSums(dk_mat[idx,,drop=FALSE])), 1),
      AvgFP    = round(mean(colSums(dk_mat[idx,,drop=FALSE])), 1),
      AvgGoals = round(mean(colSums(mats$Goals[idx,,drop=FALSE])), 2),
      AvgShots = round(mean(colSums(mats$Shots[idx,,drop=FALSE])), 1),
      AvgSOT   = round(mean(colSums(mats$SOT[idx,,drop=FALSE])), 1),
      AvgAst   = round(mean(colSums(mats$Assists[idx,,drop=FALSE])), 2),
      AvgFouls = round(mean(colSums(mats$FC[idx,,drop=FALSE])), 1),
      AvgYC    = round(mean(colSums(mats$YC[idx,,drop=FALSE])), 2),
      AvgCross = round(mean(colSums(mats$Crosses[idx,,drop=FALSE])), 1),
      AvgPass  = round(mean(colSums(mats$Passes[idx,,drop=FALSE])), 0)
    )
  }))
  setorder(team_means, -DKAvgFP)
  
  # Score distributions for box plots (sample for manageable size)
  max_vis <- min(n_sims, 2000)
  vis_cols <- sample.int(n_sims, max_vis)
  score_dist <- data.table(
    Player  = rep(player_names, max_vis),
    Team    = rep(player_teams, max_vis),
    DKScore = as.vector(dk_mat[, vis_cols])
  )
  
  # Stat distributions for validation box plots
  stat_dist <- data.table(
    Player  = rep(player_names, max_vis),
    Team    = rep(player_teams, max_vis),
    Goals   = as.vector(mats$Goals[, vis_cols]),
    Shots   = as.vector(mats$Shots[, vis_cols]),
    SOT     = as.vector(mats$SOT[, vis_cols]),
    Crosses = as.vector(mats$Crosses[, vis_cols]),
    Passes  = as.vector(mats$Passes[, vis_cols]),
    TKLW    = as.vector(mats$TKLW[, vis_cols]),
    FC      = as.vector(mats$FC[, vis_cols]),
    FD      = as.vector(mats$FD[, vis_cols])
  )
  
  # Scoreline distributions per game
  scoreline_data <- rbindlist(lapply(game_info, function(gd) {
    data.table(
      Game = gd$game$Game,
      HG   = gd$hg, AG = gd$ag,
      Scoreline = paste0(gd$hg, "-", gd$ag)
    )
  }))
  
  sport_visuals <- list(
    teams          = teams,
    player_means   = player_means,
    team_means     = team_means,
    score_dist     = score_dist,
    stat_dist      = stat_dist,
    scoreline_data = scoreline_data,
    player_data    = metadata,
    games          = games
  )
  
  # ── Console validation ──
  cat("\n  ========================================\n")
  cat("  VALIDATION REPORT\n")
  cat("  ========================================\n")
  
  cat("\n  --- Team Totals: Projection vs Sim ---\n")
  cat(sprintf("  %-4s %8s %8s %8s %8s %8s %8s %8s %8s\n",
              "Team", "G_proj", "G_sim", "S_proj", "S_sim", "FC_proj", "FC_sim", "CR_proj", "CR_sim"))
  for (tm in teams) {
    cat(sprintf("  %-4s %8.2f %8.2f %8.1f %8.1f %8.1f %8.1f %8.1f %8.1f\n",
                tm,
                sum(players[Team==tm]$G, na.rm=TRUE),  team_means[Team==tm]$AvgGoals,
                sum(players[Team==tm]$S, na.rm=TRUE),  team_means[Team==tm]$AvgShots,
                sum(players[Team==tm]$FC, na.rm=TRUE), team_means[Team==tm]$AvgFouls,
                sum(players[Team==tm]$CR, na.rm=TRUE), team_means[Team==tm]$AvgCross))
  }
  
  cat("\n  --- Scoreline Distributions ---\n")
  for (gi in seq_len(n_games)) {
    gd <- game_info[[gi]]
    sl <- table(paste0(gd$hg, "-", gd$ag))
    sl <- sort(sl, decreasing=TRUE)
    hw <- mean(gd$hg > gd$ag); dr <- mean(gd$hg == gd$ag); aw <- mean(gd$hg < gd$ag)
    cat(sprintf("\n  %s (%.2f vs %.2f): Home %.0f%% | Draw %.0f%% | Away %.0f%%\n",
                gd$game$Game, gd$game$Home_Lambda, gd$game$Away_Lambda,
                hw*100, dr*100, aw*100))
    for (s in names(head(sl, 8)))
      cat(sprintf("    %-5s %5.1f%%\n", s, sl[s]/n_sims*100))
  }
  
  cat("\n  --- Top 15 Players: DK Score Breakdown ---\n")
  top15 <- player_means[1:min(15, nrow(player_means))]
  cat(sprintf("  %-18s %-4s %5s %5s | %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s | %5s\n",
              "Player","Pos","Sal","Mean",
              "Goals","Ast","Shots","SOT","CC","Pass","Cross","Tkl","FD","FC","Saves"))
  for (i in seq_len(nrow(top15))) {
    r <- top15[i]
    cat(sprintf("  %-18s %-4s %5.0f %5.1f | %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f | %5.2f\n",
                substr(r$Player, 1, 18), r$Pos, r$Salary/1000, r$DKAvgFP,
                r$Pts_Goals, r$Pts_Assists, r$Pts_Shots, r$Pts_SOT,
                r$Pts_CC, r$Pts_Passes, r$Pts_Crosses, r$Pts_TKLW,
                r$Pts_FD, r$Pts_FC, r$Pts_Saves))
  }
  
  cat("\n  --- GK Breakdown ---\n")
  gk_rows <- player_means[grepl("^G", Pos)]
  if (nrow(gk_rows)) {
    cat(sprintf("  %-18s %5s | %5s %5s %5s\n",
                "GK","Mean","AvgSv","AvgGC","CS%"))
    for (i in seq_len(nrow(gk_rows))) {
      r <- gk_rows[i]
      pk <- which(player_names == r$Player)[1]
      avg_gc <- if (!is.null(pk) && !is.na(pk)) mean(mats$GK_GC[pk, ]) else 0
      cs_pct <- if (!is.null(pk) && !is.na(pk)) mean(mats$CS[pk, ] == 1) * 100 else 0
      cat(sprintf("  %-18s %5.1f | %5.2f %5.2f %5.1f%%\n",
                  substr(r$Player, 1, 18), r$DKAvgFP,
                  r$AvgSaves, avg_gc, cs_pct))
    }
  }
  
  elapsed <- round((proc.time()["elapsed"] - t0)[1], 1)
  cat(sprintf("\n  Complete: %d players | %s sims | %.1fs (%.0f sims/sec)\n",
              n_total, format(n_sims, big.mark=","), elapsed, n_sims/elapsed))
  cb("Complete", 1.0)
  
  list(
    sim_results   = sim_results,
    metadata      = metadata,
    sport_visuals = sport_visuals,
    has_fd        = FALSE,
    has_sd        = has_sd
  )
}

# ── LINEUP METRICS ───────────────────────────────────────────────────────────

calculate_soccer_lineup_metrics <- function(scored_lineups, sim_results, metadata) {
  scored_lineups
}


# ============================================================================
# POSITION ASSIGNMENT — LATE-SWAP OPTIMIZED
# Sorted by game_rank (earliest game first) so UTIL gets latest-game player
# Slots: GK, D, D, M, M, F, F, UTIL
# ============================================================================

assign_soccer_slots_dk <- function(cm) {
  setorder(cm, game_rank, Player)
  slots <- list(F1=NA_character_, F2=NA_character_,
                M1=NA_character_, M2=NA_character_,
                D1=NA_character_, D2=NA_character_,
                GK=NA_character_, UTIL=NA_character_)
  
  fill_slot <- function(player, pos) {
    cands <- character(0)
    if (grepl("GK|^G$", pos))                      cands <- c(cands, "GK")
    if (grepl("^F$|^F/|F/M|F/UTIL|M/F", pos))     cands <- c(cands, "F1", "F2")
    if (grepl("^M$|^M/|M/F|M/D|M/UTIL", pos))     cands <- c(cands, "M1", "M2")
    if (grepl("^D$|^D/|D/UTIL", pos))              cands <- c(cands, "D1", "D2")
    # Everything except GK can go to UTIL
    if (!grepl("GK|^G$", pos)) cands <- c(unique(cands), "UTIL")
    for (sl in cands) {
      if (sl %in% names(slots) && is.na(slots[[sl]])) {
        slots[[sl]] <<- player; return(TRUE)
      }
    }
    FALSE
  }
  
  for (idx in seq_len(nrow(cm))) {
    if (!fill_slot(cm$Player[idx], cm$DKPos[idx])) return(NULL)
  }
  if (any(sapply(slots, is.na))) return(NULL)
  slots
}


# ============================================================================
# SOCCER DK CLASSIC OPTIMIZER (per-sim LP)
# 8 players: GK, D, D, M, M, F, F, UTIL | $50K | 3+ teams | 2+ games
# ============================================================================

find_optimal_lineups_soccer <- function(sim_results, metadata, config, verbose=TRUE) {
  if (verbose) cat("\nPhase 1: Soccer DK lineups (per-sim LP)...\n")
  setDT(sim_results); setDT(metadata)
  salary_cap  <- config$salary_cap
  max_lineups <- config$max_lineups %||% 5000L
  
  meta <- unique(metadata[, .(Player, DKSalary, DKPos, Team, GameKey)], by = "Player")
  # Position eligibility from DK roster position
  meta[, gk_elig := as.integer(grepl("GK|^G$", DKPos))]
  meta[, d_elig  := as.integer(grepl("D", DKPos) & !grepl("GK", DKPos))]
  meta[, m_elig  := as.integer(grepl("M", DKPos))]
  meta[, f_elig  := as.integer(grepl("F", DKPos) & !grepl("FLEX", DKPos))]
  
  # Game rank for late-swap assignment
  if ("GameRank" %in% names(metadata)) {
    meta <- merge(meta, unique(metadata[, .(Player, GameRank)]), by = "Player", all.x = TRUE)
    meta[, game_rank := GameRank][is.na(game_rank), game_rank := 1L][, GameRank := NULL]
  } else {
    meta[, game_rank := 1L]
  }
  
  opt_data <- merge(sim_results[, .(SimID, Player, FantasyPoints = DKScore)],
                    meta[, .(Player, Salary = DKSalary, gk_elig, d_elig, m_elig, f_elig,
                             game_rank, DKPos, Team, GameKey)],
                    by = "Player")
  opt_data <- opt_data[Salary > 0 & !is.na(Salary) & !is.na(FantasyPoints)]
  setkey(opt_data, SimID)
  
  sim_ids <- unique(opt_data$SimID); n_sims <- length(sim_ids)
  if (verbose) cat(sprintf("  %d players | %s sims | $%s cap\n",
                           nrow(meta), format(n_sims, big.mark = ","),
                           format(salary_cap, big.mark = ",")))
  
  start_t <- Sys.time(); prog_freq <- max(1L, n_sims %/% 20L)
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid <- sim_ids[i]; pool <- opt_data[.(sid)]; n_p <- nrow(pool)
    if (n_p < 8L) next
    
    # Game constraints (max 7 from any game → forces 2+ games)
    gkp <- unique(pool$GameKey)
    gc <- if (length(gkp) >= 2L) lapply(gkp, function(gk) as.integer(pool$GameKey == gk)) else list()
    
    # Team constraints (max 5 from any team → helps force 3+ teams)
    teams <- unique(pool$Team)
    tc <- if (length(teams) >= 3L) lapply(teams, function(t) as.integer(pool$Team == t)) else list()
    
    f_con <- rbind(
      rep(1L, n_p),           # total players
      pool$Salary,            # salary
      pool$gk_elig,           # GK
      pool$d_elig,            # D
      pool$m_elig,            # M
      pool$f_elig,            # F
      if (length(gc)) do.call(rbind, gc) else matrix(nrow = 0, ncol = n_p),
      if (length(tc)) do.call(rbind, tc) else matrix(nrow = 0, ncol = n_p)
    )
    
    f_dir <- c("==", "<=", "==", ">=", ">=", ">=",
               rep("<=", length(gc)),
               rep("<=", length(tc)))
    f_rhs <- c(8L, salary_cap, 1L, 2L, 2L, 2L,
               rep(7L, length(gc)),
               rep(5L, length(tc)))
    
    sol <- tryCatch(
      lp("max", pool$FantasyPoints, f_con, f_dir, f_rhs, all.bin = TRUE)$solution,
      error = function(e) NULL
    )
    if (is.null(sol) || sum(sol) < 8L) next
    
    chosen <- pool[sol == 1]
    # Verify 3+ teams
    if (length(unique(chosen$Team)) < 3L) next
    
    sig <- paste(sort(chosen$Player), collapse = "|")
    lineup_list[[i]] <- data.table(Lineup = sig, TotalSalary = sum(chosen$Salary),
                                   TotalScore = sum(chosen$FantasyPoints))
    
    if (verbose && i %% prog_freq == 0L) {
      cat(sprintf("\r  Phase 1: %d%% | %.1fs", round(i / n_sims * 100),
                  as.numeric(difftime(Sys.time(), start_t, units = "secs"))))
      flush.console()
    }
  }
  if (verbose) cat("\n")
  
  valid <- lineup_list[!sapply(lineup_list, is.null)]
  if (!length(valid)) stop("No valid Soccer DK lineups found.")
  all_dt <- rbindlist(valid)
  
  counts <- all_dt[, .(Top1Count = .N, TotalSalary = TotalSalary[1],
                       AvgScore = mean(TotalScore)), by = Lineup]
  counts[, rand := runif(.N)]; setorder(counts, -Top1Count, rand); counts[, rand := NULL]
  
  # Assign slots with late-swap optimization
  slot_list <- vector("list", nrow(counts))
  for (li in seq_len(nrow(counts))) {
    players <- strsplit(counts$Lineup[li], "\\|")[[1]]
    cm <- meta[Player %in% players, .(Player, DKPos, game_rank)]
    slots <- assign_soccer_slots_dk(cm)
    if (!is.null(slots))
      slot_list[[li]] <- as.data.table(c(list(Lineup = counts$Lineup[li]), slots))
  }
  
  slot_dt <- rbindlist(slot_list[!sapply(slot_list, is.null)])
  counts <- merge(counts, slot_dt, by = "Lineup", all.x = TRUE)
  
  unique_lineups <- counts[!is.na(F1), .(
    TotalSalary, Top1Count, AvgScore,
    Player1 = F1, Player2 = F2,
    Player3 = M1, Player4 = M2,
    Player5 = D1, Player6 = D2,
    Player7 = GK, Player8 = UTIL
  )]
  if (nrow(unique_lineups) > max_lineups) unique_lineups <- unique_lineups[1:max_lineups]
  
  elapsed <- as.numeric(difftime(Sys.time(), start_t, units = "secs"))
  if (verbose) cat(sprintf("  ✓ %s DK lineups | %.1fs\n",
                           format(nrow(unique_lineups), big.mark = ","), elapsed))
  
  list(unique_lineups = unique_lineups, n_sims = n_sims, config = config, mode = "soccer_dk")
}


# ============================================================================
# SOCCER SHOWDOWN OPTIMIZER
# CPT x1.5 + 5 FLEX | $50K | both teams required
# ============================================================================

find_optimal_lineups_soccer_sd <- function(sim_results, metadata, config, verbose=TRUE) {
  if (verbose) cat("\nPhase 1: Soccer Showdown lineups (per-sim greedy)...\n")
  setDT(sim_results); setDT(metadata)
  salary_cap  <- config$salary_cap
  max_lineups <- config$max_lineups %||% 5000L
  cpt_mult    <- 1.5
  
  meta <- unique(metadata[!is.na(CPTSalary) & CPTSalary > 0 & !is.na(SDSalary) & SDSalary > 0,
                          .(Player, Team, CPTSalary, SDSalary, GameKey)], by = "Player")
  if (!nrow(meta)) stop("No SD-eligible players. Check CPTSalary/SDSalary columns.")
  if (length(unique(meta$Team)) < 2) warning("Soccer SD: fewer than 2 teams.")
  
  opt_data <- merge(sim_results[, .(SimID, Player, DKScore)],
                    meta[, .(Player, Team, CPTSalary, SDSalary)], by = "Player")
  opt_data <- opt_data[!is.na(DKScore)]; setkey(opt_data, SimID)
  
  sim_ids <- unique(opt_data$SimID); n_sims <- length(sim_ids)
  if (verbose) cat(sprintf("  %d players | %s sims | $%s cap\n",
                           nrow(meta), format(n_sims, big.mark = ","),
                           format(salary_cap, big.mark = ",")))
  
  start_t <- Sys.time(); prog_freq <- max(1L, n_sims %/% 20L)
  lineup_list <- vector("list", n_sims)
  
  for (i in seq_along(sim_ids)) {
    sid <- sim_ids[i]; sd <- opt_data[.(sid)]; setorder(sd, -DKScore)
    best_score <- -Inf; best_lineup <- NULL
    
    for (ci in seq_len(min(nrow(sd), 15L))) {  # try top 15 as CPT
      cpt_player <- sd$Player[ci]; cpt_sal <- sd$CPTSalary[ci]
      cpt_score  <- sd$DKScore[ci] * cpt_mult
      if (cpt_sal > salary_cap) next
      
      rem_cap <- salary_cap - cpt_sal
      flex <- sd[Player != cpt_player]; setorder(flex, -DKScore)
      picked <- character(5L); n_pk <- 0L; sal_used <- 0; flex_score <- 0
      
      for (j in seq_len(nrow(flex))) {
        if (n_pk == 5L) break
        if (sal_used + flex$SDSalary[j] <= rem_cap) {
          n_pk <- n_pk + 1L; picked[n_pk] <- flex$Player[j]
          sal_used <- sal_used + flex$SDSalary[j]
          flex_score <- flex_score + flex$DKScore[j]
        }
      }
      if (n_pk == 5L) {
        all_p  <- c(cpt_player, picked[1:5])
        lteams <- sd$Team[match(all_p, sd$Player)]
        if (length(unique(lteams)) < 2L) next
        total <- cpt_score + flex_score
        if (total > best_score) {
          best_score <- total
          best_lineup <- list(Captain = cpt_player, Flex = sort(picked),
                              TotalSalary = cpt_sal + sal_used, TotalScore = total)
        }
      }
    }
    
    if (!is.null(best_lineup)) {
      lineup_list[[i]] <- data.table(
        Lineup = paste(c(best_lineup$Captain, best_lineup$Flex), collapse = "|"),
        TotalSalary = best_lineup$TotalSalary, TotalScore = best_lineup$TotalScore,
        Captain = best_lineup$Captain, Util1 = best_lineup$Flex[1],
        Util2 = best_lineup$Flex[2], Util3 = best_lineup$Flex[3],
        Util4 = best_lineup$Flex[4], Util5 = best_lineup$Flex[5])
    }
    
    if (verbose && i %% prog_freq == 0L) {
      cat(sprintf("\r  Phase 1: %d%% | %.1fs", round(i / n_sims * 100),
                  as.numeric(difftime(Sys.time(), start_t, units = "secs"))))
      flush.console()
    }
  }
  if (verbose) cat("\n")
  
  valid <- lineup_list[!sapply(lineup_list, is.null)]
  if (!length(valid)) stop("No valid Soccer SD lineups found.")
  all_dt <- rbindlist(valid)
  
  counts <- all_dt[, .(Top1Count = .N, TotalSalary = TotalSalary[1],
                       AvgScore = mean(TotalScore),
                       Captain = Captain[1], Util1 = Util1[1], Util2 = Util2[1],
                       Util3 = Util3[1], Util4 = Util4[1], Util5 = Util5[1]),
                   by = Lineup]
  counts[, rand := runif(.N)]; setorder(counts, -Top1Count, rand); counts[, rand := NULL]
  if (nrow(counts) > max_lineups) counts <- counts[1:max_lineups]
  
  unique_lineups <- counts[, .(TotalSalary, Top1Count, AvgScore,
                               Captain, Util1, Util2, Util3, Util4, Util5)]
  elapsed <- as.numeric(difftime(Sys.time(), start_t, units = "secs"))
  if (verbose) cat(sprintf("  ✓ %s SD lineups | %.1fs\n",
                           format(nrow(unique_lineups), big.mark = ","), elapsed))
  
  list(unique_lineups = unique_lineups, n_sims = n_sims, config = config, mode = "captain")
}