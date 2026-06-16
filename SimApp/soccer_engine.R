# ============================================================================
# SOCCER SIMULATION ENGINE v3 — Golden Ticket Sims
# ============================================================================
# Market-driven: scorelines, shots, SOT, tackles, fouls, corners→crosses
# WC-calibrated: SOT rate by scoreline, CC/shot ratio, card rates, possession
# Position-weighted: crosses, passes, INT from player data + position defaults
# ============================================================================

library(data.table)

SOCCER_P <- list(
  rho = -0.13, max_goals = 5,
  
  # Fallback NegBin phi values
  phi_shots = 10.5, phi_crosses = 8.2, phi_fouls = 47.5,
  phi_tackles = 8.0, phi_def = 5.0,
  tackle_win_rate = 0.60,  # market shows attempts, DK scores wins only
  
  # Scoreline scaling (indexed by own_goals+1: 0G,1G,2G,3G,4G,5G)
  shots_scale = c(0.857, 0.962, 1.048, 1.164, 1.281, 1.426),
  opp_shots_scale = c(1.04, 1.02, 1.00, 0.94, 0.88),  # indexed by opp_goals+1
  
  # SOT rate scaling by own goals (from WC data)
  # 0G: 24.7%, 1G: 35.3%, 2G: 38.0%, 3G: 46.7%
  sot_rate_scale = c(0.716, 1.023, 1.101, 1.354, 1.400, 1.400),
  sot_kappa = 7.7,
  
  # CC = shots × rate
  cc_rate = 0.74, cc_kappa = 10,
  
  # Cards (WC calibrated)
  yc_per_foul = 0.141,  # WC rate
  p_second_yc = 0.018, p_straight_red = 0.0026,
  p_assist = 0.709,
  
  # YC frustration by opp goals (indexed opp_goals+1)
  # Conceded 0:1.38, 1:1.58, 2:2.13, 3:2.62 → relative to mean 1.77
  yc_frustration = c(0.78, 0.89, 1.20, 1.48, 1.48, 1.48),
  
  # Possession lookup (from WC data, indexed by shot_share bins)
  # shot_share: 0.20→30%, 0.30→37%, 0.40→43%, 0.50→50%, 0.60→57%, 0.70→63%, 0.80→70%
  poss_shot_shares = c(0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80),
  poss_values      = c(30,   37,   43,   50,   57,   63,   70),
  # Passes by possession (from WC data)
  poss_for_passes = c(30, 40, 50, 60, 70),
  passes_by_poss  = c(220, 290, 430, 550, 750),
  # INT by possession (inverse)
  int_by_poss     = c(8.0, 7.0, 6.0, 4.5, 3.0),
  
  # Caps
  team_max_shots = 30L, team_min_shots = 3L, team_max_sot = 15L,
  team_max_crosses = 40L, team_min_crosses = 2L,
  
  # Tempo
  tempo_sd = 0.15,
  
  # Position weights (from Big 5 player data)
  cross_wt = c(F=0.60, W=2.50, AM=1.00, CM=0.70, DM=0.40, WB=5.00, CB=0.15, GK=0.00),
  int_wt   = c(F=0.15, W=0.20, AM=0.25, CM=0.55, DM=0.80, WB=0.45, CB=0.70, GK=0.00),
  pass_wt  = c(F=17,   W=22,   AM=28,   CM=42,   DM=45,   WB=38,   CB=48,   GK=26)
)


# ── HELPERS ──────────────────────────────────────────────────────────────────

tau_dc <- function(x,y,l1,l2,rho) {
  if(x==0&&y==0) return(1-l1*l2*rho); if(x==0&&y==1) return(1+l1*rho)
  if(x==1&&y==0) return(1+l2*rho); if(x==1&&y==1) return(1-rho); 1
}

build_grid <- function(lh, la, rho=SOCCER_P$rho, mg=SOCCER_P$max_goals) {
  g <- CJ(hg=0:mg, ag=0:mg)
  g[, prob := dpois(hg,lh)*dpois(ag,la)*mapply(tau_dc,hg,ag,MoreArgs=list(l1=lh,l2=la,rho=rho))]
  g[, prob := prob/sum(prob)]; g
}

rnb <- function(n, mu, phi) {
  mu <- pmax(mu, 0.01); if(length(mu)==1 && mu<=0.01) return(rep(0L,n))
  if(phi==Inf) return(rpois(n,mu)); as.integer(rnbinom(n, size=phi, mu=mu))
}

norm_to_total <- function(raw, totals, n_p) {
  cs <- colSums(raw); cs[cs==0] <- 1
  scaled <- sweep(raw, 2, totals/cs, `*`)
  fl <- matrix(as.integer(floor(scaled)), n_p, length(totals))
  resid <- as.integer(round(totals)) - colSums(fl)
  frac <- scaled - fl
  for(s in seq_along(totals)) {
    r <- resid[s]; if(r==0||!is.finite(r)) next
    if(r>0) { top <- order(frac[,s],decreasing=TRUE)[seq_len(min(r,n_p))]; fl[top,s] <- fl[top,s]+1L
    } else { elig <- which(fl[,s]>0); if(!length(elig)) next
    ord <- elig[order(frac[elig,s])]; fl[ord[seq_len(min(abs(r),length(ord)))],s] <- fl[ord[seq_len(min(abs(r),length(ord)))],s]-1L }
  }; fl
}

# Interpolation helper
interp <- function(x, xp, yp) approx(xp, yp, xout=pmin(pmax(x, min(xp)), max(xp)), rule=2)$y


# ── DK SCORING ───────────────────────────────────────────────────────────────

dk_score_soccer_v <- function(dk_pos, goals, assists, shots, sot, cc,
                              crosses, tackles, fd, fc, passes,
                              ints, yc, rc, cs, gk_saves, gk_gc, gk_win, mins) {
  s <- goals*10 + assists*6 + shots + sot + cc + crosses*0.7 +
    tackles + fd - fc*0.5 + passes*0.02 + ints*0.5
  s <- s + ifelse(yc>=2, -3.0, ifelse(yc==1, -1.5, 0))
  s <- s + rc * (-3.0)
  is_d <- grepl("D", dk_pos) & !grepl("GK", dk_pos)
  s <- s + ifelse(is_d & mins>=60 & cs==1, 3, 0)
  is_gk <- grepl("GK", dk_pos)
  s <- s + ifelse(is_gk, gk_saves*2 + gk_gc*(-2), 0)
  s <- s + ifelse(is_gk & mins>=60 & cs==1, 5, 0)
  s <- s + ifelse(is_gk & mins>=90 & gk_win==1, 5, 0)
  s
}


# ── INPUT READER ─────────────────────────────────────────────────────────────

read_soccer_input <- function(file_path) {
  sheets <- getSheetNames(file_path)
  data <- setNames(lapply(sheets, function(s) as.data.table(read.xlsx(file_path, sheet=s))), sheets)
  pl <- as.data.table(data$Players); gm <- as.data.table(data$Games)
  
  # Numeric coercion
  num_p <- c("MIN","DK_Salary","Goal_Share","Assist_Share","Shot_Share","SOT_Share",
             "Tackle_Share","Foul_Share","FD_Share","YC_Share","Cross_Share","INT_Share","Pass_Share")
  for(col in intersect(num_p, names(pl))) pl[[col]] <- as.numeric(pl[[col]])
  num_g <- c("Home_Lambda","Away_Lambda","Home_Shots","Away_Shots","Home_SOT","Away_SOT")
  for(col in intersect(num_g, names(gm))) gm[[col]] <- as.numeric(gm[[col]])
  
  # SD tabs
  sd_tabs <- list()
  for(sn in grep("^SD\\d+_IDs$", sheets, value=TRUE)) sd_tabs[[sub("_IDs$","",sn)]] <- data[[sn]]
  gm[, GameKey := Game]; gm[, ShowdownFile := ""]
  for(sdn in names(sd_tabs)) {
    sd_dt <- as.data.table(sd_tabs[[sdn]]); tc <- intersect(c("Team","TeamAbbrev"), names(sd_dt))[1]
    if(!is.na(tc)) { st <- unique(sd_dt[[tc]])
    for(r in seq_len(nrow(gm))) if(gm$Home[r] %in% st || gm$Away[r] %in% st) gm[r, ShowdownFile := sdn] }
  }
  
  # Distributions (market PMFs)
  distributions <- NULL
  if("Distributions" %in% sheets) { distributions <- as.data.table(data[["Distributions"]]); cat(sprintf("  Market PMFs: %d\n", nrow(distributions))) }
  
  # WC bootstrap
  wc_bootstrap <- NULL
  wc_path <- "~/GTS/Soccer/data/wc22_game_flat.parquet"
  if(file.exists(wc_path)) { wc_bootstrap <- as.data.table(arrow::read_parquet(wc_path)); cat(sprintf("  WC bootstrap: %d games\n", nrow(wc_bootstrap))) }
  
  cat(sprintf("Soccer: %d players | %d games | %d SD tabs\n", nrow(pl), nrow(gm), length(sd_tabs)))
  list(Players=pl, Games=gm, IDs=data$IDs, sd_tabs=sd_tabs, games=gm,
       distributions=distributions, wc_bootstrap=wc_bootstrap, all_sheets=data)
}


# ── MAIN SIMULATION ─────────────────────────────────────────────────────────

run_soccer_simulation <- function(input_data, n_sims=10000, config=NULL, progress_callback=NULL) {
  t0 <- proc.time()["elapsed"]
  cb <- function(d,v) { if(!is.null(progress_callback)) progress_callback(d,v); cat(sprintf("  [%.0f%%] %s\n", v*100, d)); flush.console() }
  cb("Loading input...", 0.01)
  
  players <- as.data.table(input_data$Players); games <- as.data.table(input_data$Games)
  
  # Market PMFs
  market_dists <- list()
  if(!is.null(input_data$distributions) && nrow(input_data$distributions) > 0) {
    dd <- input_data$distributions; k_cols <- grep("^k\\d+$", names(dd), value=TRUE)
    for(r in seq_len(nrow(dd))) {
      key <- paste(dd$Game[r], dd$Team[r], dd$Stat[r], sep="|")
      probs <- as.numeric(dd[r, ..k_cols]); probs[is.na(probs)] <- 0
      ks <- as.integer(gsub("^k","",k_cols)); valid <- probs > 0
      if(sum(valid)>1) market_dists[[key]] <- list(k=ks[valid], prob=probs[valid]/sum(probs[valid]))
    }
    if(length(market_dists)) cat(sprintf("  Market PMFs: %d\n", length(market_dists)))
  }
  
  wc_boot <- input_data$wc_bootstrap; has_wc <- !is.null(wc_boot) && nrow(wc_boot) > 0
  
  # SD merge
  has_sd <- FALSE
  if(length(input_data$sd_tabs)) {
    for(sdn in names(input_data$sd_tabs)) {
      sd_dt <- as.data.table(input_data$sd_tabs[[sdn]])
      nm_col <- intersect(c("Name","Nickname"), names(sd_dt))[1]; if(is.na(nm_col)) next
      rp_col <- grep("Roster.*Pos|roster.*pos", names(sd_dt), value=TRUE, ignore.case=TRUE)[1]; if(is.na(rp_col)) next
      id_col_sd <- grep("^ID$", names(sd_dt), value=TRUE)[1]; if(is.na(id_col_sd)) next
      sal_col_sd <- grep("^Salary$", names(sd_dt), value=TRUE, ignore.case=TRUE)[1]; if(is.na(sal_col_sd)) next
      for(j in seq_len(nrow(sd_dt))) {
        pn <- sd_dt[[nm_col]][j]; rp <- sd_dt[[rp_col]][j]
        if(is.na(pn) || is.na(rp) || !nchar(as.character(rp))) next
        pi <- which(players$DK_Name == pn); if(!length(pi)) pi <- which(players$Player == pn)
        if(!length(pi)) next
        if(grepl("CPT", rp)) { players[pi[1], CPTID := as.character(sd_dt[[id_col_sd]][j])]; players[pi[1], CPTSalary := as.numeric(sd_dt[[sal_col_sd]][j])] }
        if(grepl("FLEX", rp)) { players[pi[1], SDID := as.character(sd_dt[[id_col_sd]][j])]; players[pi[1], SDSalary := as.numeric(sd_dt[[sal_col_sd]][j])] }
      }; has_sd <- TRUE
    }
    if(has_sd) cat("  SD merged\n")
  }
  
  n_games <- nrow(games); all_players_list <- copy(players); n_total <- nrow(all_players_list)
  cat(sprintf("  %d players | %d games | %s sims\n", n_total, n_games, format(n_sims, big.mark=",")))
  
  # ── Scoreline setup ──
  cb("Building scoreline grids...", 0.03)
  game_info <- list()
  
  for(gi in seq_len(n_games)) {
    g <- games[gi]; otn <- paste0(g$Home,"vs",g$Away)
    odds_tab <- NULL
    for(tn in c(otn, paste0(g$Away,"vs",g$Home), g$Game)) {
      if("all_sheets" %in% names(input_data) && tn %in% names(input_data$all_sheets)) { odds_tab <- as.data.table(input_data$all_sheets[[tn]]); break }
    }
    if(!is.null(odds_tab) && nrow(odds_tab)>1) {
      setnames(odds_tab, c("hg","ag","odds"))
      odds_tab[, hg := as.integer(hg)]; odds_tab[, ag := as.integer(ag)]; odds_tab[, odds := as.numeric(odds)]
      odds_tab <- odds_tab[!is.na(hg) & !is.na(ag) & !is.na(odds)]
      odds_tab[, implied := ifelse(odds>0, 100/(odds+100), abs(odds)/(abs(odds)+100))]
      odds_tab[, prob := implied/sum(implied)]
      draws <- sample.int(nrow(odds_tab), n_sims, replace=TRUE, prob=odds_tab$prob)
      hg_vec <- odds_tab$hg[draws]; ag_vec <- odds_tab$ag[draws]
      cat(sprintf("  %s: ODDS-BASED (%d scorelines)\n", g$Game, nrow(odds_tab)))
    } else {
      grid <- build_grid(g$Home_Lambda, g$Away_Lambda)
      draws <- sample.int(nrow(grid), n_sims, replace=TRUE, prob=grid$prob)
      hg_vec <- grid$hg[draws]; ag_vec <- grid$ag[draws]
      cat(sprintf("  %s: DIXON-COLES\n", g$Game))
    }
    hp <- players[Team==g$Home & Opp==g$Away]; ap <- players[Team==g$Away & Opp==g$Home]
    game_info[[gi]] <- list(game=g, hg=hg_vec, ag=ag_vec, hp=hp, ap=ap)
    cat(sprintf("    %d+%d players | H%.0f%% D%.0f%% A%.0f%%\n", nrow(hp), nrow(ap),
                mean(hg_vec>ag_vec)*100, mean(hg_vec==ag_vec)*100, mean(hg_vec<ag_vec)*100))
  }
  
  # ── Allocate matrices ──
  stat_names <- c("Goals","Assists","Shots","SOT","CC","Crosses","Tackles","FD","FC",
                  "Passes","INT","YC","RC","GK_Saves","GK_GC","CS","GK_Win","MIN")
  mats <- setNames(lapply(stat_names, function(s) matrix(0, n_total, n_sims)), stat_names)
  
  cb("Simulating matches...", 0.08)
  player_offset <- 0L
  
  for(gi in seq_len(n_games)) {
    gd <- game_info[[gi]]; pct_base <- 0.08 + (gi-1)/n_games*0.72
    
    # Game environment: tempo tied to scoreline
    tg_sim <- gd$hg + gd$ag
    goals_z <- (tg_sim - mean(tg_sim)) / max(sd(tg_sim), 0.5)
    z_tempo <- 0.4*goals_z + sqrt(1-0.16)*rnorm(n_sims)
    tempo <- exp(SOCCER_P$tempo_sd * z_tempo)
    
    for(side in c("home","away")) {
      pl <- if(side=="home") gd$hp else gd$ap
      n_p <- nrow(pl); if(n_p==0) next
      team_goals <- if(side=="home") gd$hg else gd$ag
      opp_goals  <- if(side=="home") gd$ag else gd$hg
      pidx <- (player_offset+1):(player_offset+n_p)
      
      # Minutes: fixed from input
      mins <- pl$MIN; ms <- mins/90
      mats$MIN[pidx,] <- matrix(rep(mins, n_sims), n_p, n_sims)
      
      # Scoreline indices
      gi_vec <- pmin(team_goals,5L)+1L
      opp_gi <- pmin(opp_goals,4L)+1L
      
      # Market PMF lookup keys
      tn <- if(side=="home") gd$game$Home else gd$game$Away
      gk1 <- paste0(gd$game$Home,"vs",gd$game$Away); gk2 <- paste0(gd$game$Away,"vs",gd$game$Home)
      get_pmf <- function(stat) {
        p <- market_dists[[paste(gk1,tn,stat,sep="|")]]
        if(is.null(p)) p <- market_dists[[paste(gk2,tn,stat,sep="|")]]
        p
      }
      
      # ── TEAM SHOTS (market PMF × scoreline scale) ──
      sp <- get_pmf("Shots")
      shots_mu <- if(side=="home") gd$game$Home_Shots else gd$game$Away_Shots
      if(!is.null(sp)) {
        bs <- sample(sp$k, n_sims, replace=TRUE, prob=sp$prob)
        em <- SOCCER_P$shots_scale[gi_vec] * SOCCER_P$opp_shots_scale[opp_gi]
        t_shots <- as.integer(round(bs * em / mean(em)))
      } else {
        t_shots <- rnb(n_sims, shots_mu*SOCCER_P$shots_scale[gi_vec]*SOCCER_P$opp_shots_scale[opp_gi], SOCCER_P$phi_shots)
      }
      t_shots <- pmax(t_shots, team_goals, SOCCER_P$team_min_shots)
      t_shots <- pmin(t_shots, SOCCER_P$team_max_shots)
      
      # ── TEAM SOT (derived from shots × accuracy rate × scoreline) ──
      sot_mu <- if(side=="home") gd$game$Home_SOT else gd$game$Away_SOT
      base_sot_rate <- pmin(pmax(sot_mu / pmax(shots_mu, 1), 0.15), 0.60)
      # Scale rate by scoreline (teams that score more are more clinical)
      sim_sot_rate <- base_sot_rate * SOCCER_P$sot_rate_scale[gi_vec]
      sim_sot_rate <- pmin(pmax(sim_sot_rate, 0.10), 0.70)
      # Add Beta variance
      alpha_s <- sim_sot_rate * SOCCER_P$sot_kappa
      beta_s <- (1-sim_sot_rate) * SOCCER_P$sot_kappa
      sot_rates <- pmin(pmax(rbeta(n_sims, alpha_s, beta_s), 0.10), 0.70)
      t_sot <- rbinom(n_sims, t_shots, sot_rates)
      t_sot <- pmax(t_sot, team_goals)
      t_sot <- pmin(t_sot, t_shots, SOCCER_P$team_max_sot)
      
      # ── TEAM CC (74% of shots) ──
      cc_rates <- rbeta(n_sims, SOCCER_P$cc_rate*SOCCER_P$cc_kappa,
                        (1-SOCCER_P$cc_rate)*SOCCER_P$cc_kappa)
      cc_rates <- pmin(pmax(cc_rates, 0.40), 0.95)
      t_cc <- pmin(as.integer(round(t_shots * cc_rates)), t_shots)
      
      # ── TEAM TACKLES (market PMF, independent) ──
      tkp <- get_pmf("Tackles")
      if(!is.null(tkp)) {
        t_tackles <- as.integer(sample(tkp$k, n_sims, replace=TRUE, prob=tkp$prob))
      } else {
        tkl_mu <- if("Home_Tackles" %in% names(gd$game)) {
          if(side=="home") gd$game$Home_Tackles else gd$game$Away_Tackles
        } else 6.0
        t_tackles <- rnb(n_sims, tkl_mu, SOCCER_P$phi_tackles)
      }
      t_tackles <- pmax(t_tackles, 1L); t_tackles <- pmin(t_tackles, 20L)
      # Convert attempted → won (market shows attempts, DK scores wins)
      t_tackles <- as.integer(round(t_tackles * SOCCER_P$tackle_win_rate))
      
      # ── TEAM FOULS (market PMF, independent) ──
      flp <- get_pmf("Fouls")
      if(!is.null(flp)) {
        t_fouls <- as.integer(sample(flp$k, n_sims, replace=TRUE, prob=flp$prob))
      } else if(has_wc) {
        # Bootstrap from WC
        t_fouls <- numeric(n_sims)
        for(gc in 0:SOCCER_P$max_goals) {
          si <- which(team_goals==gc); if(!length(si)) next
          pool <- wc_boot[goals==min(gc,4)]; if(!nrow(pool)) pool <- wc_boot
          t_fouls[si] <- pool$fouls[sample.int(nrow(pool), length(si), replace=TRUE)]
        }
        t_fouls <- as.integer(t_fouls)
      } else { t_fouls <- rnb(n_sims, 12.5, SOCCER_P$phi_fouls) }
      t_fouls <- pmax(t_fouls, 2L); t_fouls <- pmin(t_fouls, 25L)
      
      # ── TEAM CORNERS → CROSSES (linear model from WC data) ──
      crp <- get_pmf("Corners")
      if(!is.null(crp)) {
        t_corners <- as.integer(sample(crp$k, n_sims, replace=TRUE, prob=crp$prob))
      } else {
        corner_mu <- if(side=="home" && "Home_Corners" %in% names(gd$game)) gd$game$Home_Corners
        else if(side=="away" && "Away_Corners" %in% names(gd$game)) gd$game$Away_Corners
        else 4.5
        t_corners <- rnb(n_sims, corner_mu, 7.5)
      }
      # Linear model: crosses ≈ 9.5 + 2.0 × corners (from WC data, r=0.694)
      t_crosses <- as.integer(round(9.5 + 2.0*t_corners + rnorm(n_sims, 0, 4)))
      t_crosses <- pmax(t_crosses, SOCCER_P$team_min_crosses)
      t_crosses <- pmin(t_crosses, SOCCER_P$team_max_crosses)
      
      # ── PASSES + INT (from possession proxy via shot ratio) ──
      opp_shots_mu <- if(side=="home") gd$game$Away_Shots else gd$game$Home_Shots
      opp_sp <- get_pmf(if(side=="home") paste(gk1,gd$game$Away,"Shots",sep="|") else "")
      # Use team shot means for possession proxy
      total_shot_mu <- shots_mu + opp_shots_mu
      shot_share <- pmin(pmax(shots_mu / pmax(total_shot_mu, 1), 0.20), 0.80)
      poss_est <- interp(shot_share, SOCCER_P$poss_shot_shares, SOCCER_P$poss_values)
      t_passes <- as.integer(round(interp(poss_est, SOCCER_P$poss_for_passes, SOCCER_P$passes_by_poss) *
                                     tempo + rnorm(n_sims, 0, 30)))
      t_passes <- pmax(t_passes, 100L); t_passes <- pmin(t_passes, 800L)
      t_int <- as.integer(round(interp(poss_est, SOCCER_P$poss_for_passes, SOCCER_P$int_by_poss) +
                                  rnorm(n_sims, 0, 1.5)))
      t_int <- pmax(t_int, 0L); t_int <- pmin(t_int, 15L)
      
      # ── CARDS (from team fouls × rate × frustration) ──
      frustration <- SOCCER_P$yc_frustration[opp_gi]
      yc_rate_adj <- SOCCER_P$yc_per_foul * frustration
      # Team YC ~ Binomial(fouls, adjusted rate)
      t_yc <- rbinom(n_sims, t_fouls, pmin(yc_rate_adj, 0.40))
      
      # ══════════════════════════════════════════════════════════
      # PLAYER ALLOCATION
      # ══════════════════════════════════════════════════════════
      
      # Share vectors (all pre-computed in InputMaker)
      get_share <- function(col) {
        v <- if(col %in% names(pl)) as.numeric(pl[[col]]) else rep(1/n_p, n_p)
        v[is.na(v)] <- 0; v <- pmax(v, 0)
        if(sum(v)==0) v <- rep(1/n_p, n_p)
        v / sum(v)
      }
      
      gs <- get_share("Goal_Share"); as_s <- get_share("Assist_Share")
      ss <- get_share("Shot_Share"); sots <- get_share("SOT_Share")
      tks <- get_share("Tackle_Share"); fcs <- get_share("Foul_Share")
      fds <- get_share("FD_Share"); ycs <- get_share("YC_Share")
      crs <- get_share("Cross_Share"); ints <- get_share("INT_Share")
      pas <- get_share("Pass_Share")
      
      # GKs only get passes and FD — zero out everything else
      is_gk <- grepl("GK", pl$DK_RosterPos)
      if(any(is_gk)) {
        for(sh in c("gs","as_s","ss","sots","tks","fcs","crs","ints","ycs")) {
          v <- get(sh); v[is_gk] <- 0
          if(sum(v)>0) assign(sh, v/sum(v)) else assign(sh, v)
        }
      }
      
      # Diagnostic: print shares for first game/side
      if(gi==1 && side=="home") {
        cat("\n  SHARE DIAGNOSTIC (first team):\n")
        cat(sprintf("  %-25s %6s %6s %6s %6s\n", "Player", "Goal", "Shot", "Cross", "Tackle"))
        for(p in seq_len(n_p)) {
          cat(sprintf("  %-25s %6.3f %6.3f %6.3f %6.3f\n", pl$Player[p], gs[p], ss[p], crs[p], tks[p]))
        }
        cat(sprintf("  Cross_Share in names: %s\n", "Cross_Share" %in% names(pl)))
        cat(sprintf("  Cross_Share raw values: %s\n", paste(round(pl$Cross_Share, 4), collapse=", ")))
      }
      
      # Goals (multinomial per sim)
      goals_m <- matrix(0L, n_p, n_sims)
      for(s in seq_len(n_sims)) {
        tg <- team_goals[s]; if(tg==0) next
        goals_m[,s] <- rmultinom(1, size=tg, prob=gs)[,1]
      }
      mats$Goals[pidx,] <- goals_m
      
      # Assists (per goal, exclude scorer)
      ast_m <- matrix(0L, n_p, n_sims)
      for(s in seq_len(n_sims)) {
        tg <- team_goals[s]; if(tg==0) next
        gr <- goals_m[,s]
        for(g_i in seq_len(tg)) {
          sc <- which(gr>0); if(!length(sc)) next
          scorer <- if(length(sc)==1) sc else sample(sc,1,prob=gr[sc])
          gr[scorer] <- gr[scorer]-1L
          if(runif(1)>SOCCER_P$p_assist) next
          ap <- as_s; ap[scorer] <- 0; if(sum(ap)==0) next
          ast_m[sample.int(n_p,1,prob=ap), s] <- ast_m[sample.int(n_p,1,prob=ap), s] + 1L
        }
      }
      mats$Assists[pidx,] <- ast_m
      
      # Shots
      raw <- matrix(rnb(n_p*n_sims, rep(ss*mean(t_shots), n_sims), SOCCER_P$phi_shots), n_p, n_sims)
      mats$Shots[pidx,] <- pmin(norm_to_total(raw, t_shots, n_p), 12L)
      
      # SOT (saved = SOT - goals, allocate saved by SOT share, add goals back)
      saved <- pmax(t_sot - colSums(goals_m), 0)
      raw_sot <- matrix(rnb(n_p*n_sims, rep(sots*mean(saved), n_sims), SOCCER_P$phi_shots), n_p, n_sims)
      mats$SOT[pidx,] <- goals_m + norm_to_total(raw_sot, saved, n_p)
      
      # CC (allocate by assist share)
      raw_cc <- matrix(rnb(n_p*n_sims, rep(as_s*mean(t_cc), n_sims), SOCCER_P$phi_def), n_p, n_sims)
      mats$CC[pidx,] <- pmin(norm_to_total(raw_cc, t_cc, n_p), 8L)
      
      # Crosses
      raw_cr <- matrix(rnb(n_p*n_sims, rep(crs*mean(t_crosses), n_sims), SOCCER_P$phi_crosses), n_p, n_sims)
      mats$Crosses[pidx,] <- pmin(norm_to_total(raw_cr, t_crosses, n_p), 15L)
      
      # Tackles
      raw_tk <- matrix(rnb(n_p*n_sims, rep(tks*mean(t_tackles), n_sims), SOCCER_P$phi_tackles), n_p, n_sims)
      mats$Tackles[pidx,] <- pmin(norm_to_total(raw_tk, t_tackles, n_p), 10L)
      
      # Fouls committed
      raw_fc <- matrix(rnb(n_p*n_sims, rep(fcs*mean(t_fouls), n_sims), SOCCER_P$phi_fouls), n_p, n_sims)
      player_fc <- pmin(norm_to_total(raw_fc, t_fouls, n_p), 6L)
      mats$FC[pidx,] <- player_fc
      
      # Passes
      raw_pa <- matrix(rnb(n_p*n_sims, rep(pas*mean(t_passes), n_sims), 7.0), n_p, n_sims)
      mats$Passes[pidx,] <- norm_to_total(raw_pa, t_passes, n_p)
      
      # INT
      raw_in <- matrix(rnb(n_p*n_sims, rep(ints*mean(t_int), n_sims), SOCCER_P$phi_def), n_p, n_sims)
      mats$INT[pidx,] <- pmin(norm_to_total(raw_in, t_int, n_p), 8L)
      
      # Cards (YC allocated by YC share from market, constrained by fouls)
      yc_mat <- matrix(0L, n_p, n_sims)
      rc_mat <- matrix(0L, n_p, n_sims)
      # Allocate team YC to players by YC_Share
      raw_yc <- matrix(rnb(n_p*n_sims, rep(ycs*mean(t_yc), n_sims), 5.0), n_p, n_sims)
      yc_mat <- pmin(norm_to_total(raw_yc, t_yc, n_p), 2L)
      # Straight reds (rare, from high-foul players)
      fc_flat <- as.vector(player_fc)
      rc_flat <- as.integer(runif(length(fc_flat)) < SOCCER_P$p_straight_red & fc_flat > 0)
      rc_mat <- matrix(rc_flat, n_p, n_sims)
      mats$YC[pidx,] <- yc_mat; mats$RC[pidx,] <- rc_mat
      
      # CS + GK Win
      mats$CS[pidx,] <- matrix(rep(as.integer(opp_goals==0), each=n_p), n_p, n_sims)
      mats$GK_Win[pidx,] <- matrix(rep(as.integer(team_goals>opp_goals), each=n_p), n_p, n_sims)
      
      # Store for cross-reference
      if(side=="home") {
        game_info[[gi]]$home_t_sot <- t_sot; game_info[[gi]]$home_t_fouls <- t_fouls
        game_info[[gi]]$home_pidx <- pidx
        game_info[[gi]]$home_gk <- which(grepl("GK", pl$DK_RosterPos) & mins >= 60)
        game_info[[gi]]$home_fd_share <- get_share("FD_Share")
      } else {
        game_info[[gi]]$away_t_sot <- t_sot; game_info[[gi]]$away_t_fouls <- t_fouls
        game_info[[gi]]$away_pidx <- pidx
        game_info[[gi]]$away_gk <- which(grepl("GK", pl$DK_RosterPos) & mins >= 60)
        game_info[[gi]]$away_fd_share <- get_share("FD_Share")
      }
      player_offset <- player_offset + n_p
    }
    
    # ── CROSS-REFERENCE: GK saves + Fouls drawn ──
    gd <- game_info[[gi]]
    if(length(gd$home_gk) && !is.null(gd$away_t_sot)) { pk<-gd$home_pidx[gd$home_gk[1]]; mats$GK_Saves[pk,]<-pmax(gd$away_t_sot-gd$ag, 0); mats$GK_GC[pk,]<-gd$ag }
    if(length(gd$away_gk) && !is.null(gd$home_t_sot)) { pk<-gd$away_pidx[gd$away_gk[1]]; mats$GK_Saves[pk,]<-pmax(gd$home_t_sot-gd$hg, 0); mats$GK_GC[pk,]<-gd$hg }
    
    # FD: your fouls drawn = opponent's fouls committed
    if(!is.null(gd$away_t_fouls)&&!is.null(gd$home_fd_share)) {
      hp<-gd$home_pidx; nh<-length(hp); hs<-gd$home_fd_share
      raw_fd<-matrix(rnb(nh*n_sims,rep(hs*mean(gd$away_t_fouls),n_sims),SOCCER_P$phi_fouls),nh,n_sims)
      mats$FD[hp,] <- pmin(norm_to_total(raw_fd, gd$away_t_fouls, nh), 8L)
    }
    if(!is.null(gd$home_t_fouls)&&!is.null(gd$away_fd_share)) {
      ap<-gd$away_pidx; na_p<-length(ap); as2<-gd$away_fd_share
      raw_fd<-matrix(rnb(na_p*n_sims,rep(as2*mean(gd$home_t_fouls),n_sims),SOCCER_P$phi_fouls),na_p,n_sims)
      mats$FD[ap,] <- pmin(norm_to_total(raw_fd, gd$home_t_fouls, na_p), 8L)
    }
    
    cb(sprintf("Game %d/%d complete", gi, n_games), pct_base+0.72/n_games)
  }
  
  # ── DK SCORES ──
  cb("Calculating DK scores...", 0.82)
  dk_pos_vec <- if("DK_RosterPos" %in% names(all_players_list)) all_players_list$DK_RosterPos else all_players_list$Pos
  dk_pos_vec[is.na(dk_pos_vec)] <- all_players_list$Pos[is.na(dk_pos_vec)]
  
  dk_mat <- dk_score_soccer_v(
    dk_pos=rep(dk_pos_vec,n_sims), goals=as.vector(mats$Goals), assists=as.vector(mats$Assists),
    shots=as.vector(mats$Shots), sot=as.vector(mats$SOT), cc=as.vector(mats$CC),
    crosses=as.vector(mats$Crosses), tackles=as.vector(mats$Tackles),
    fd=as.vector(mats$FD), fc=as.vector(mats$FC),
    passes=as.vector(mats$Passes), ints=as.vector(mats$INT),
    yc=as.vector(mats$YC), rc=as.vector(mats$RC),
    cs=as.vector(mats$CS), gk_saves=as.vector(mats$GK_Saves),
    gk_gc=as.vector(mats$GK_GC), gk_win=as.vector(mats$GK_Win),
    mins=as.vector(mats$MIN))
  dk_mat <- matrix(dk_mat, n_total, n_sims)
  
  # ── RESULTS ──
  cb("Building results...", 0.88)
  sim_results <- data.table(
    SimID=rep(seq_len(n_sims), each=n_total), Player=rep(all_players_list$Player, n_sims),
    Team=rep(all_players_list$Team, n_sims), Pos=rep(all_players_list$Pos, n_sims),
    DKScore=as.vector(dk_mat),
    Goals=as.vector(mats$Goals), Assists=as.vector(mats$Assists),
    Shots=as.vector(mats$Shots), SOT=as.vector(mats$SOT), CC=as.vector(mats$CC),
    Crosses=as.vector(mats$Crosses), Tackles=as.vector(mats$Tackles),
    FD=as.vector(mats$FD), FC=as.vector(mats$FC),
    Passes=as.vector(mats$Passes), INT=as.vector(mats$INT),
    YC=as.vector(mats$YC), RC=as.vector(mats$RC),
    GK_Saves=as.vector(mats$GK_Saves), GK_GC=as.vector(mats$GK_GC),
    CS=as.vector(mats$CS), GK_Win=as.vector(mats$GK_Win), MIN=as.vector(mats$MIN))
  
  # ── METADATA ──
  cb("Building metadata...", 0.92)
  mc <- c("Player","Team","Opp","Pos","DK_Salary","DK_ID","DK_Name","DK_RosterPos")
  if(has_sd) mc <- c(mc, "CPTID","CPTSalary","SDID","SDSalary")
  avail <- intersect(mc, names(all_players_list))
  metadata <- unique(all_players_list[, ..avail], by="Player")
  setnames(metadata, c("DK_Salary","DK_ID"), c("DKSalary","DKID"), skip_absent=TRUE)
  metadata[, DKOwn := 0]; metadata[, PosGroup := Pos]
  metadata[, DKPos := if("DK_RosterPos" %in% names(.SD)) DK_RosterPos else Pos, .SDcols=names(metadata)]
  metadata[, GameKey := { r<-games[Home==Team|Away==Team]; if(nrow(r)) r$Game[1] else paste0(Team," vs ",Opp) }, by=Player]
  grm <- data.table(Th=games$Home, Ta=games$Away, rk=seq_len(nrow(games)))
  metadata[, GameRank := { r<-grm[Th==Team|Ta==Team, rk]; if(length(r)) r[1] else 1L }, by=Player]
  metadata[, ShowdownFile := ""]
  if(has_sd) for(sdn in names(input_data$sd_tabs)) {
    sd_dt<-as.data.table(input_data$sd_tabs[[sdn]]); tc<-intersect(c("Team","TeamAbbrev"),names(sd_dt))[1]
    if(!is.na(tc)) { st<-unique(sd_dt[[tc]]); metadata[Team %in% st & ShowdownFile=="", ShowdownFile := sdn] }
  }
  
  # ── VISUALS ──
  cb("Building visualizations...", 0.95)
  pm <- data.table(Player=all_players_list$Player, Team=all_players_list$Team,
                   Pos=all_players_list$Pos, Salary=all_players_list$DK_Salary,
                   DKAvgFP=round(rowMeans(dk_mat),2), SDFP=round(apply(dk_mat,1,sd),2),
                   P10=round(apply(dk_mat,1,quantile,0.10),2), P50=round(apply(dk_mat,1,quantile,0.50),2),
                   P90=round(apply(dk_mat,1,quantile,0.90),2), Ceiling=round(apply(dk_mat,1,quantile,0.99),2),
                   AvgGoals=round(rowMeans(mats$Goals),3), AvgAst=round(rowMeans(mats$Assists),3),
                   AvgShots=round(rowMeans(mats$Shots),1), AvgSOT=round(rowMeans(mats$SOT),1),
                   AvgCC=round(rowMeans(mats$CC),1), AvgCross=round(rowMeans(mats$Crosses),1),
                   AvgTackles=round(rowMeans(mats$Tackles),1), AvgFD=round(rowMeans(mats$FD),1),
                   AvgFC=round(rowMeans(mats$FC),1), AvgPasses=round(rowMeans(mats$Passes),0),
                   AvgINT=round(rowMeans(mats$INT),1), AvgYC=round(rowMeans(mats$YC),2),
                   AvgSaves=round(rowMeans(mats$GK_Saves),2))
  setorder(pm, -DKAvgFP)
  
  sl <- rbindlist(lapply(seq_len(n_games), function(gi) { gd<-game_info[[gi]]; data.table(Game=gd$game$Game, HG=gd$hg, AG=gd$ag) }))
  tm <- sim_results[, .(Goals=mean(Goals), Shots=mean(Shots), SOT=mean(SOT), CC=mean(CC),
                        Crosses=mean(Crosses), Tackles=mean(Tackles), FD=mean(FD), FC=mean(FC),
                        Passes=mean(Passes), INT=mean(INT)), by=Team]
  
  elapsed <- as.numeric(proc.time()["elapsed"]-t0)
  cat(sprintf("\n  Complete: %d players | %s sims | %.1fs (%.0f sims/sec)\n", n_total, format(n_sims,big.mark=","), elapsed, n_sims/elapsed))
  cb("Complete", 1.0)
  
  list(sim_results=sim_results, metadata=metadata, dk_mat=dk_mat,
       sport_visuals=list(player_means=pm, team_means=tm, scoreline_data=sl, games=games))
}

# ── LINEUP FUNCTIONS (unchanged from v2) ─────────────────────────────────────

calculate_soccer_lineup_metrics <- function(scored_lineups, sim_results, metadata) { scored_lineups }

assign_soccer_slots_dk <- function(cm) {
  setorder(cm, game_rank, Player)
  slots <- list(F1=NA_character_,F2=NA_character_,M1=NA_character_,M2=NA_character_,
                D1=NA_character_,D2=NA_character_,GK=NA_character_,UTIL=NA_character_)
  fill <- function(player, pos) {
    cands <- character(0)
    if(grepl("GK",pos)) cands<-c(cands,"GK")
    if(grepl("F",pos)&!grepl("FLEX",pos)) cands<-c(cands,"F1","F2")
    if(grepl("M",pos)) cands<-c(cands,"M1","M2")
    if(grepl("D",pos)&!grepl("GK",pos)) cands<-c(cands,"D1","D2")
    if(!grepl("GK",pos)) cands<-c(unique(cands),"UTIL")
    for(sl in cands) if(sl %in% names(slots) && is.na(slots[[sl]])) { slots[[sl]]<<-player; return(TRUE) }
    FALSE
  }
  for(i in seq_len(nrow(cm))) if(!fill(cm$Player[i], cm$DKPos[i])) return(NULL)
  if(any(sapply(slots, is.na))) return(NULL); slots
}

find_optimal_lineups_soccer <- function(sim_results, metadata, config, verbose=TRUE) {
  if(verbose) cat("\nPhase 1: Soccer DK lineups...\n")
  setDT(sim_results); setDT(metadata)
  sc <- config$salary_cap; ml <- config$max_lineups %||% 5000L
  meta <- unique(metadata[, .(Player,DKSalary,DKPos,Team,GameKey)], by="Player")
  meta[, gk_elig := as.integer(grepl("GK",DKPos))]
  meta[, d_elig := as.integer(grepl("D",DKPos)&!grepl("GK",DKPos))]
  meta[, m_elig := as.integer(grepl("M",DKPos))]
  meta[, f_elig := as.integer(grepl("F",DKPos)&!grepl("FLEX",DKPos))]
  if("GameRank" %in% names(metadata)) meta<-merge(meta,unique(metadata[,.(Player,GameRank)]),by="Player",all.x=TRUE)
  meta[, game_rank := fifelse(is.na(GameRank),1L,GameRank)][, GameRank := NULL]
  od <- merge(sim_results[,.(SimID,Player,FP=DKScore)], meta[,.(Player,Sal=DKSalary,gk_elig,d_elig,m_elig,f_elig,game_rank,DKPos,Team,GameKey)], by="Player")
  od <- od[Sal>0&!is.na(Sal)&!is.na(FP)]; setkey(od, SimID)
  sids <- unique(od$SimID); ns <- length(sids)
  if(verbose) cat(sprintf("  %d players | %s sims\n", nrow(meta), format(ns,big.mark=",")))
  st <- Sys.time(); pf <- max(1L, ns%/%20L); ll <- vector("list", ns)
  for(i in seq_along(sids)) {
    sid<-sids[i]; pool<-od[.(sid)]; np<-nrow(pool); if(np<8L) next
    gkp<-unique(pool$GameKey); gc<-if(length(gkp)>=2) lapply(gkp,function(g) as.integer(pool$GameKey==g)) else list()
    tms<-unique(pool$Team); tc<-if(length(tms)>=3) lapply(tms,function(t) as.integer(pool$Team==t)) else list()
    fc<-rbind(rep(1L,np),pool$Sal,pool$gk_elig,pool$d_elig,pool$m_elig,pool$f_elig,
              if(length(gc)) do.call(rbind,gc) else matrix(nrow=0,ncol=np),
              if(length(tc)) do.call(rbind,tc) else matrix(nrow=0,ncol=np))
    fd<-c("==","<=","==",">=",">=",">=",rep("<=",length(gc)),rep("<=",length(tc)))
    fr<-c(8L,sc,1L,2L,2L,2L,rep(7L,length(gc)),rep(5L,length(tc)))
    sol<-tryCatch(lpSolve::lp("max",pool$FP,fc,fd,fr,all.bin=TRUE)$solution, error=function(e) NULL)
    if(is.null(sol)||sum(sol)<8L) next; ch<-pool[sol==1]
    if(length(unique(ch$Team))<3L) next
    ll[[i]]<-data.table(Lineup=paste(sort(ch$Player),collapse="|"),TotalSalary=sum(ch$Sal),TotalScore=sum(ch$FP))
    if(verbose&&i%%pf==0L) { cat(sprintf("\r  Phase 1: %d%%",round(i/ns*100))); flush.console() }
  }
  if(verbose) cat("\n")
  valid<-ll[!sapply(ll,is.null)]; if(!length(valid)) stop("No valid lineups.")
  ad<-rbindlist(valid); ct<-ad[,.(Top1Count=.N,TotalSalary=TotalSalary[1],AvgScore=mean(TotalScore)),by=Lineup]
  ct[,rand:=runif(.N)]; setorder(ct,-Top1Count,rand); ct[,rand:=NULL]
  sl2<-vector("list",nrow(ct))
  for(li in seq_len(nrow(ct))) { ps<-strsplit(ct$Lineup[li],"\\|")[[1]]; cm<-meta[Player%in%ps,.(Player,DKPos,game_rank)]
  s<-assign_soccer_slots_dk(cm); if(!is.null(s)) sl2[[li]]<-as.data.table(c(list(Lineup=ct$Lineup[li]),s)) }
  sd2<-rbindlist(sl2[!sapply(sl2,is.null)]); ct<-merge(ct,sd2,by="Lineup",all.x=TRUE)
  ul<-ct[!is.na(F1),.(TotalSalary,Top1Count,AvgScore,Player1=F1,Player2=F2,Player3=M1,Player4=M2,Player5=D1,Player6=D2,Player7=GK,Player8=UTIL)]
  if(nrow(ul)>ml) ul<-ul[1:ml]
  el<-as.numeric(difftime(Sys.time(),st,units="secs"))
  if(verbose) cat(sprintf("  Done: %s lineups | %.1fs\n", format(nrow(ul),big.mark=","), el))
  list(unique_lineups=ul, n_sims=ns, config=config, mode="soccer_dk")
}

find_optimal_lineups_soccer_sd <- function(sim_results, metadata, config, verbose=TRUE) {
  if(verbose) cat("\nPhase 1: Soccer SD lineups...\n")
  setDT(sim_results); setDT(metadata)
  sc<-config$salary_cap; ml<-config$max_lineups%||%5000L; cm<-1.5
  meta<-unique(metadata[!is.na(CPTSalary)&CPTSalary>0&!is.na(SDSalary)&SDSalary>0,.(Player,Team,CPTSalary,SDSalary,GameKey)],by="Player")
  if(!nrow(meta)) stop("No SD players.")
  np<-nrow(meta); pl<-meta$Player; cs<-as.numeric(meta$CPTSalary); ss<-as.numeric(meta$SDSalary); tid<-as.integer(factor(meta$Team))
  opt<-merge(sim_results[,.(SimID,Player,DKScore)],meta[,.(Player)],by="Player"); opt<-opt[!is.na(DKScore)]
  sids<-sort(unique(opt$SimID)); ns<-length(sids)
  sm<-matrix(0,np,ns); sm[cbind(match(opt$Player,pl),match(opt$SimID,sids))]<-opt$DKScore
  if(verbose) cat(sprintf("  %d players | %s sims\n",np,format(ns,big.mark=",")))
  st<-Sys.time(); ro<-apply(sm,2,order,decreasing=TRUE)
  bs<-rep(-Inf,ns); bc<-integer(ns); bf<-matrix(0L,5,ns); bsal<-numeric(ns)
  co<-order(-rowMeans(sm))
  for(ci in seq_len(min(np,12L))) {
    cx<-co[ci]; if(cs[cx]>sc) next; rc<-sc-cs[cx]; ctid<-tid[cx]; csc<-sm[cx,]*cm
    for(s in seq_len(ns)) {
      rk<-ro[,s]; fsal<-0;fsc<-0;npk<-0L;fi<-integer(5);ho<-FALSE
      for(ri in seq_len(np)) { if(npk==5L) break; pi<-rk[ri]; if(pi==cx) next; if(fsal+ss[pi]>rc) next
      npk<-npk+1L; fi[npk]<-pi; fsal<-fsal+ss[pi]; fsc<-fsc+sm[pi,s]; if(tid[pi]!=ctid) ho<-TRUE }
      if(npk<5L||!ho) next; tot<-csc[s]+fsc
      if(tot>bs[s]) { bs[s]<-tot; bc[s]<-cx; bf[,s]<-fi; bsal[s]<-cs[cx]+fsal }
    }
  }
  valid<-which(bc>0); if(!length(valid)) stop("No valid SD lineups.")
  lu<-cn<-u1<-u2<-u3<-u4<-u5<-character(length(valid))
  for(vi in seq_along(valid)) { s<-valid[vi]; fp<-sort(pl[bf[,s]]); cn[vi]<-pl[bc[s]]
  lu[vi]<-paste(c(cn[vi],fp),collapse="|"); u1[vi]<-fp[1];u2[vi]<-fp[2];u3[vi]<-fp[3];u4[vi]<-fp[4];u5[vi]<-fp[5] }
  ad<-data.table(Lineup=lu,TotalSalary=bsal[valid],TotalScore=bs[valid],Captain=cn,Util1=u1,Util2=u2,Util3=u3,Util4=u4,Util5=u5)
  ct<-ad[,.(Top1Count=.N,TotalSalary=TotalSalary[1],AvgScore=mean(TotalScore),Captain=Captain[1],Util1=Util1[1],Util2=Util2[1],Util3=Util3[1],Util4=Util4[1],Util5=Util5[1]),by=Lineup]
  ct[,rand:=runif(.N)]; setorder(ct,-Top1Count,rand); ct[,rand:=NULL]
  if(nrow(ct)>ml) ct<-ct[1:ml]
  ul<-ct[,.(TotalSalary,Top1Count,AvgScore,Captain,Util1,Util2,Util3,Util4,Util5)]
  el<-as.numeric(difftime(Sys.time(),st,units="secs"))
  if(verbose) cat(sprintf("  Done: %s SD lineups | %.1fs\n",format(nrow(ul),big.mark=","),el))
  list(unique_lineups=ul, n_sims=ns, config=config, mode="captain")
}