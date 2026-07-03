# ============================================================================
# SOCCER SIMULATION ENGINE v3 — Golden Ticket Sims
# ============================================================================
# Market-driven: scorelines, shots, SOT, tackles, fouls, corners→crosses
# WC-calibrated: SOT rate by scoreline, CC/shot ratio, card rates, possession
# Position-weighted: crosses, passes, INT from player data + position defaults
# ============================================================================

library(data.table)
if(!requireNamespace("openxlsx", quietly=TRUE)) install.packages("openxlsx", repos="https://cloud.r-project.org")
library(openxlsx)

SOCCER_P <- list(
  rho = -0.13, max_goals = 5,
  
  # Fallback NegBin phi values
  phi_shots = 10.5, phi_crosses = 8.2, phi_fouls = 6.5,
  phi_tackles = 8.0, phi_def = 5.0,
  tackle_win_rate = 0.60,  # market shows attempts, DK scores wins only
  
  # Scoreline scaling (indexed by own_goals+1: 0G,1G,2G,3G,4G,5G)
  shots_scale = c(0.857, 0.962, 1.048, 1.164, 1.281, 1.426),
  opp_shots_scale = c(1.04, 1.02, 1.00, 0.94, 0.88),  # indexed by opp_goals+1
  
  # SOT rate scaling by own goals (from WC data) — fallback path only
  # 0G: 24.7%, 1G: 35.3%, 2G: 38.0%, 3G: 46.7%
  sot_rate_scale = c(0.716, 1.023, 1.101, 1.354, 1.400, 1.400),
  sot_kappa = 7.7,
  
  # Team shots/SOT/goals copula correlations (normal-space).
  # Used to couple the market-PMF draws so high-shot sims are high-SOT and
  # high-goal sims. Defaults below are overridden per-slate by the Correlations
  # sheet written by InputMaker (computed from team_game_log / wc22_game_flat).
  corr_shots_goals = 0.55,
  corr_sot_goals   = 0.62,
  corr_sot_shots   = 0.78,
  
  # CC = shots × rate
  cc_rate = 0.74, cc_kappa = 10,
  
  # Cards (WC calibrated)
  yc_per_foul = 0.141,  # WC rate
  p_second_yc = 0.018, p_straight_red = 0.0026,
  p_assist = 0.709,
  
  # ── SUBSTITUTIONS ──────────────────────────────────────────────────────
  # Subs are real but unrostered: they absorb a share of the team's production
  # in the minutes they play, and that production LEAVES the rosterable pool
  # (lowering starters' totals). Modeled as a phantom "sub bucket" that takes a
  # per-sim share of each team total, then is discarded. The mean fraction of
  # outfield minutes played by subs ~ 5 subs × ~18 min / (10 × 90) ≈ 0.10–0.16.
  sub_share_mean = 0.14,   # mean fraction of team production absorbed by subs
  sub_share_sd   = 0.05,   # per-sim variation (Beta-distributed around the mean)
  # Position skew of WHAT subs absorb: teams chase games with attackers, so the
  # sub bucket takes disproportionately more attacking output and less defensive.
  # Multipliers on the base sub share, by stat family (1.0 = neutral).
  sub_skew_attack  = 1.45,  # goals/shots/SOT/CC/crosses (subs are mostly attackers)
  sub_skew_passes  = 1.00,  # passes (neutral — subs play across the pitch)
  sub_skew_defense = 0.65,  # tackles/INT/fouls (defenders rarely subbed)
  
  # Goal allocation: plain multinomial over goal-share (team total fixed by
  # scoreline). No per-sim Dirichlet resampling, no scorer boost — correlation
  # between attacking stats comes from the mechanical nesting (goals ⊂ SOT ⊂
  # shots; CC/assists attach to OTHER players' shots), not artificial multipliers.
  
  # YC frustration by opp goals (indexed opp_goals+1)
  # Conceded 0:1.38, 1:1.58, 2:2.13, 3:2.62 → relative to mean 1.77
  # ── EXTRA TIME (knockout) ──────────────────────────────────────────────
  # Applied ONLY to sims tied after 90' (hg==ag), identified from the scoreline
  # draw. The fraction of sims that trigger ET is therefore the correct-score
  # market's draw mass — self-scaling per game (lopsided games rarely trigger).
  et_enable = FALSE,        # master switch for ET modeling
  et_minutes = 30,         # length of extra time
  # ── ET per-minute intensity vs regulation, by stat ──
  # Grounded in ET sports-science findings (Field/Harper systematic review):
  #  - goals ~ regulation per-minute rate (EURO2020: 0.0292 vs 0.0294/min)
  #  - shooting velocity/volume down modestly; passes ~ -30%; dribbles ~ -36%;
  #    tackling ratio lower; ball-in-play ~ -16% (a floor on all event drops).
  # Values lean CONSERVATIVE (under-add) on every stat except goals, where the
  # data is firm that ET ~ regulation rate. All applied only to tied sims.
  et_goal_intensity = 0.70,
  et_int_shots  = 0.52,
  et_int_sot    = 0.52,
  et_int_tackle = 0.78,
  et_int_foul   = 0.80,
  et_int_pass   = 0.70,
  et_int_cross  = 0.68,
  et_int_int    = 0.78,
  et_int_default= 0.78,    # any stat not listed
  # Penalties are NEVER simulated: if still level after ET, no GK win is awarded.
  
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
  cross_wt = c(F=0.60, W=2.50, AM=1.00, CM=0.70, DM=0.40, WB=6.00, FB=2.50, CB=0.15, GK=0.00),
  int_wt   = c(F=0.15, W=0.20, AM=0.25, CM=0.55, DM=0.80, WB=0.45, FB=0.55, CB=0.70, GK=0.00),
  pass_wt  = c(F=17,   W=22,   AM=28,   CM=42,   DM=45,   WB=38,   FB=35,   CB=48,   GK=26)
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

# Inverse-CDF sampler: given uniforms u and a PMF (support k, prob p), return the
# count whose CDF first reaches u. Preserves the PMF as the exact marginal.
inv_cdf_pmf <- function(u, k, p) {
  cdf <- cumsum(p/sum(p))
  idx <- findInterval(u, cdf, left.open=FALSE) + 1L
  idx <- pmin(pmax(idx, 1L), length(k))
  k[idx]
}

# Draw team shots & SOT from their market PMFs, coupled to the (already-drawn)
# team goals via a Gaussian copula so the three move together at historical
# correlation. Goals are NOT redrawn — they are rank-mapped to a latent normal
# and shots/SOT are drawn conditional on it. Returns list(shots, sot).
#  team_goals : integer vector (fixed, from scoreline)
#  shots_pmf, sot_pmf : list(k=, prob=) market PMFs (may be NULL -> caller falls back)
copula_shots_sot <- function(team_goals, shots_pmf, sot_pmf,
                             rsg, rtg, rst, n_sims) {
  # latent for goals: rank -> uniform -> normal (jitter breaks ties)
  jit <- team_goals + runif(n_sims, 0, 1e-6)
  u_g <- (rank(jit, ties.method="first") - 0.5) / n_sims
  z_g <- qnorm(u_g)
  # conditional MVN for (z_shots, z_sot) given z_g, target corr matrix R
  R_ba <- c(rsg, rtg)                       # corr of (shots,sot) with goals
  R_bb <- matrix(c(1, rst, rst, 1), 2, 2)   # corr between shots & sot
  cov_b <- R_bb - outer(R_ba, R_ba)         # R_aa = 1
  # guard PD (clamp if user-supplied corrs are inconsistent)
  ev <- eigen(cov_b, symmetric=TRUE, only.values=TRUE)$values
  if(min(ev) <= 1e-6) cov_b <- cov_b + diag(2) * (1e-6 - min(ev))
  L <- chol(cov_b)                          # upper-tri; t(L) is lower
  eps <- matrix(rnorm(n_sims*2), n_sims, 2) %*% L
  z_s <- z_g * R_ba[1] + eps[,1]
  z_t <- z_g * R_ba[2] + eps[,2]
  u_s <- pnorm(z_s); u_t <- pnorm(z_t)
  shots <- as.integer(inv_cdf_pmf(u_s, shots_pmf$k, shots_pmf$prob))
  sot   <- as.integer(inv_cdf_pmf(u_t, sot_pmf$k,   sot_pmf$prob))
  list(shots=shots, sot=sot)
}

# Allocate per-column totals by raw weights, but cap each player at caps[,s]
# (used for nested subsets: SOT<=Shots, Goals<=SOT). Overflow beyond a player's
# cap is redistributed to players with remaining headroom, weighted by `share`.
# Guarantees: result <= caps elementwise, colSums(result) == round(totals)
# whenever total capacity colSums(caps) >= totals (true for our nested totals).
alloc_capped <- function(raw, totals, caps, share, n_p) {
  alloc <- norm_to_total(raw, totals, n_p)
  over <- pmax(alloc - caps, 0L); alloc <- alloc - over
  spill <- colSums(over)
  if(any(spill > 0)) {
    headroom <- caps - alloc
    for(s in which(spill > 0)) {
      need <- spill[s]; cap <- headroom[,s]
      elig <- which(cap > 0); if(!length(elig)) next
      w <- share[elig]; if(sum(w)==0) w <- rep(1, length(elig))
      add <- pmin(cap[elig], as.integer(round(need * w/sum(w))))
      place <- min(need, sum(cap[elig]))
      d <- place - sum(add)
      if(d != 0) {
        ord <- elig[order(-(cap[elig]-add))]; i <- 1L
        while(d != 0 && i <= length(ord)) {
          j <- match(ord[i], elig)
          if(d > 0 && add[j] < cap[ord[i]]) { add[j] <- add[j]+1L; d <- d-1L }
          else if(d < 0 && add[j] > 0)      { add[j] <- add[j]-1L; d <- d+1L }
          i <- i+1L; if(i > length(ord)) i <- 1L
        }
      }
      alloc[elig, s] <- alloc[elig, s] + add
    }
  }
  alloc
}


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
  setDT(pl); setDT(gm)  # reset internal selfref
  
  # Numeric coercion
  num_p <- c("MIN","DK_Salary","Goal_Share","Assist_Share","Shot_Share","SOT_Share",
             "Tackle_Share","Foul_Share","FD_Share","YC_Share","Cross_Share","INT_Share","Pass_Share","Set_Pct")
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
  
  # Correlations (shots/SOT/goals coupling, computed by InputMaker from history)
  correlations <- NULL
  if("Correlations" %in% sheets) {
    correlations <- as.data.table(data[["Correlations"]])
    cat(sprintf("  Correlations: %d rows\n", nrow(correlations)))
  }
  
  # WC bootstrap
  wc_bootstrap <- NULL
  wc_path <- "~/GTS/Soccer/data/wc22_game_flat.parquet"
  if(file.exists(wc_path)) { wc_bootstrap <- as.data.table(arrow::read_parquet(wc_path)); cat(sprintf("  WC bootstrap: %d games\n", nrow(wc_bootstrap))) }
  
  cat(sprintf("Soccer: %d players | %d games | %d SD tabs\n", nrow(pl), nrow(gm), length(sd_tabs)))
  list(Players=pl, Games=gm, IDs=data$IDs, sd_tabs=sd_tabs, games=gm,
       distributions=distributions, correlations=correlations,
       wc_bootstrap=wc_bootstrap, all_sheets=data,
       has_sd = length(sd_tabs) > 0)
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
  
  # Resolve shots/SOT/goals copula correlations: per-slate values from the
  # Correlations sheet override the SOCCER_P defaults. Expected long format with
  # columns Stat1, Stat2, Rho (e.g. Shots/SOT/0.78). Missing -> default.
  corr_sg <- SOCCER_P$corr_shots_goals
  corr_tg <- SOCCER_P$corr_sot_goals
  corr_ts <- SOCCER_P$corr_sot_shots
  if(!is.null(input_data$correlations) && nrow(input_data$correlations) > 0) {
    cdt <- as.data.table(input_data$correlations)
    cn <- names(cdt)
    s1 <- grep("^Stat1$", cn, ignore.case=TRUE, value=TRUE)[1]
    s2 <- grep("^Stat2$", cn, ignore.case=TRUE, value=TRUE)[1]
    rc <- grep("^Rho$|^Corr",  cn, ignore.case=TRUE, value=TRUE)[1]
    if(!is.na(s1) && !is.na(s2) && !is.na(rc)) {
      lk <- function(a, b) {
        v <- cdt[(toupper(get(s1))==a & toupper(get(s2))==b) |
                   (toupper(get(s1))==b & toupper(get(s2))==a), as.numeric(get(rc))]
        if(length(v) && is.finite(v[1])) v[1] else NA_real_
      }
      v <- lk("SHOTS","GOALS"); if(!is.na(v)) corr_sg <- v
      v <- lk("SOT","GOALS");   if(!is.na(v)) corr_tg <- v
      v <- lk("SOT","SHOTS");   if(!is.na(v)) corr_ts <- v
      cat(sprintf("  Correlations: shots~goals=%.2f sot~goals=%.2f sot~shots=%.2f\n",
                  corr_sg, corr_tg, corr_ts))
    }
  }
  # Clamp to valid range
  corr_sg <- min(max(corr_sg, -0.95), 0.95)
  corr_tg <- min(max(corr_tg, -0.95), 0.95)
  corr_ts <- min(max(corr_ts, -0.95), 0.95)
  
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
      
      # Get actual row indices in all_players_list
      tn <- if(side=="home") gd$game$Home else gd$game$Away
      opp_tn <- if(side=="home") gd$game$Away else gd$game$Home
      pidx <- which(all_players_list$Team == tn & all_players_list$Opp == opp_tn)
      if(length(pidx) != n_p) {
        cat(sprintf("  WARNING: pidx mismatch for %s: expected %d, got %d\n", tn, n_p, length(pidx)))
        next
      }
      
      # ── MINUTES & SUBSTITUTIONS ──────────────────────────────────────────
      # Expected minutes per starter from input (position-aware, set in
      # InputMaker). Each starter's REALIZED minutes vary per sim (a striker
      # hooked at 65', a CB playing all 90), so within the starters the split
      # tilts toward those who stayed on.
      exp_mins <- pmin(pmax(as.numeric(pl$MIN), 1), 90)
      mins <- exp_mins                          # expected, for the >=60 gates below
      mins_mat <- matrix(90L, n_p, n_sims)
      for(p in seq_len(n_p)) {
        em <- exp_mins[p]
        if(em >= 89) { mins_mat[p,] <- 90L; next }   # nailed-on (e.g. GK)
        mu_frac <- em/90
        kappa   <- 6 + 14*mu_frac                 # higher expected min => tighter
        fr <- rbeta(n_sims, mu_frac*kappa, (1-mu_frac)*kappa)
        mins_mat[p,] <- as.integer(pmin(pmax(round(fr*90), 0L), 90L))
      }
      mats$MIN[pidx,] <- mins_mat
      ms_mat <- mins_mat / 90                     # per-sim minutes weight for shares
      
      # Per-sim team SUB SHARE: the fraction of team production absorbed by
      # (unrostered) substitutes this sim. Beta around the mean so it varies.
      ssm <- SOCCER_P$sub_share_mean; sssd <- SOCCER_P$sub_share_sd
      sub_kappa <- max(ssm*(1-ssm)/(sssd^2) - 1, 2)
      sub_f <- rbeta(n_sims, ssm*sub_kappa, (1-ssm)*sub_kappa)   # length n_sims
      # Stat-family absorbed fractions (subs skew attacking). Clamp < 0.6 so the
      # starters always retain the majority of any team total.
      f_atk <- pmin(sub_f * SOCCER_P$sub_skew_attack,  0.60)
      f_pas <- pmin(sub_f * SOCCER_P$sub_skew_passes,  0.60)
      f_def <- pmin(sub_f * SOCCER_P$sub_skew_defense, 0.60)
      # Retained-by-starters multipliers (what's left after subs take their cut)
      keep_atk <- 1 - f_atk; keep_pas <- 1 - f_pas; keep_def <- 1 - f_def
      
      # Scoreline indices
      gi_vec <- pmin(team_goals,5L)+1L
      opp_gi <- pmin(opp_goals,4L)+1L
      
      # Market PMF lookup keys
      gk1 <- paste0(gd$game$Home,"vs",gd$game$Away); gk2 <- paste0(gd$game$Away,"vs",gd$game$Home)
      get_pmf <- function(stat) {
        p <- market_dists[[paste(gk1,tn,stat,sep="|")]]
        if(is.null(p)) p <- market_dists[[paste(gk2,tn,stat,sep="|")]]
        p
      }
      
      # ── TEAM SHOTS & SOT (market PMFs coupled to goals via Gaussian copula) ──
      # When both PMFs exist, draw shots and SOT directly from the market
      # distributions, correlated with each other and with the (already-drawn)
      # team goals at historical strength. Preserves both marginals exactly.
      sp <- get_pmf("Shots")
      tp <- get_pmf("SOT")
      shots_mu <- if(side=="home") gd$game$Home_Shots else gd$game$Away_Shots
      sot_mu   <- if(side=="home") gd$game$Home_SOT   else gd$game$Away_SOT
      
      if(!is.null(sp) && !is.null(tp)) {
        cop <- copula_shots_sot(team_goals, sp, tp,
                                corr_sg, corr_tg, corr_ts, n_sims)
        t_shots <- cop$shots
        t_sot   <- cop$sot
        # Light scoreline tilt on shots (teams that score more tend to shoot more)
        em <- SOCCER_P$shots_scale[gi_vec] * SOCCER_P$opp_shots_scale[opp_gi]
        t_shots <- as.integer(round(t_shots * em / mean(em)))
      } else if(!is.null(sp)) {
        # Shots PMF only — SOT via accuracy rate fallback
        bs <- sample(sp$k, n_sims, replace=TRUE, prob=sp$prob)
        em <- SOCCER_P$shots_scale[gi_vec] * SOCCER_P$opp_shots_scale[opp_gi]
        t_shots <- as.integer(round(bs * em / mean(em)))
        base_sot_rate <- pmin(pmax(sot_mu / pmax(shots_mu, 1), 0.15), 0.60)
        sim_sot_rate <- pmin(pmax(base_sot_rate * SOCCER_P$sot_rate_scale[gi_vec], 0.10), 0.70)
        sot_rates <- pmin(pmax(rbeta(n_sims, sim_sot_rate*SOCCER_P$sot_kappa,
                                     (1-sim_sot_rate)*SOCCER_P$sot_kappa), 0.10), 0.70)
        t_sot <- rbinom(n_sims, pmax(t_shots,0L), sot_rates)
      } else {
        # No PMFs — full parametric fallback (original behavior)
        t_shots <- rnb(n_sims, shots_mu*SOCCER_P$shots_scale[gi_vec]*SOCCER_P$opp_shots_scale[opp_gi], SOCCER_P$phi_shots)
        base_sot_rate <- pmin(pmax(sot_mu / pmax(shots_mu, 1), 0.15), 0.60)
        sim_sot_rate <- pmin(pmax(base_sot_rate * SOCCER_P$sot_rate_scale[gi_vec], 0.10), 0.70)
        sot_rates <- pmin(pmax(rbeta(n_sims, sim_sot_rate*SOCCER_P$sot_kappa,
                                     (1-sim_sot_rate)*SOCCER_P$sot_kappa), 0.10), 0.70)
        t_sot <- rbinom(n_sims, pmax(t_shots,0L), sot_rates)
      }
      
      # ── Enforce team-level hierarchy: goals <= SOT <= shots, plus caps ──
      t_shots <- pmax(t_shots, team_goals, SOCCER_P$team_min_shots)
      t_shots <- pmin(t_shots, SOCCER_P$team_max_shots)
      t_sot   <- pmax(t_sot, team_goals)               # every goal is on target
      t_sot   <- pmin(t_sot, t_shots, SOCCER_P$team_max_sot)
      
      # ── TEAM CC (74% of shots) ──
      cc_rates <- rbeta(n_sims, SOCCER_P$cc_rate*SOCCER_P$cc_kappa,
                        (1-SOCCER_P$cc_rate)*SOCCER_P$cc_kappa)
      cc_rates <- pmin(pmax(cc_rates, 0.40), 0.95)
      t_cc <- pmin(as.integer(round(t_shots * cc_rates)), t_shots)
      # Team-style anchor: if Team.csv gave a real CC-per-game rate, recentre the
      # team CC total on it (keeping the per-sim variance), since team chance-
      # creation style is better captured by the season rate than shots alone.
      cc_anchor <- if(side=="home" && "Home_CC_Base" %in% names(gd$game)) gd$game$Home_CC_Base
      else if(side=="away" && "Away_CC_Base" %in% names(gd$game)) gd$game$Away_CC_Base
      else NA_real_
      if(!is.na(cc_anchor) && cc_anchor > 0) {
        cur_mu <- mean(t_cc); if(cur_mu > 0) t_cc <- pmin(as.integer(round(t_cc * (cc_anchor/cur_mu))), t_shots)
      }
      
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
      } else {
        # Fallback: use market team-fouls mean from Games tab if present
        fl_mu <- if("Home_Fouls" %in% names(gd$game)) {
          v <- if(side=="home") gd$game$Home_Fouls else gd$game$Away_Fouls
          if(is.na(v)) 12.5 else v
        } else 12.5
        t_fouls <- rnb(n_sims, fl_mu, SOCCER_P$phi_fouls)
      }
      t_fouls <- pmax(t_fouls, 2L); t_fouls <- pmin(t_fouls, 25L)
      
      # ── TEAM CORNERS → CROSSES (team style × corner activity) ──
      crp <- get_pmf("Corners")
      corner_mu <- if(side=="home" && "Home_Corners" %in% names(gd$game)) gd$game$Home_Corners
      else if(side=="away" && "Away_Corners" %in% names(gd$game)) gd$game$Away_Corners
      else 4.5
      if(!is.null(crp)) {
        t_corners <- as.integer(sample(crp$k, n_sims, replace=TRUE, prob=crp$prob))
      } else {
        t_corners <- rnb(n_sims, corner_mu, 7.5)
      }
      # Team cross base from player data (sum of starters' crosses_p90)
      cross_base <- if(side=="home" && "Home_Cross_Base" %in% names(gd$game)) gd$game$Home_Cross_Base
      else if(side=="away" && "Away_Cross_Base" %in% names(gd$game)) gd$game$Away_Cross_Base
      else 18.3  # WC average fallback
      # Scale by corner activity: more corners → more crosses
      corner_ratio <- t_corners / pmax(corner_mu, 1)
      t_crosses <- as.integer(round(cross_base * corner_ratio + rnorm(n_sims, 0, 3)))
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
      # Team-style anchor: recentre INT on the real Team.csv per-game rate if given.
      int_anchor <- if(side=="home" && "Home_INT_Base" %in% names(gd$game)) gd$game$Home_INT_Base
      else if(side=="away" && "Away_INT_Base" %in% names(gd$game)) gd$game$Away_INT_Base
      else NA_real_
      if(!is.na(int_anchor) && int_anchor > 0) {
        cur_mu <- mean(t_int); if(cur_mu > 0) t_int <- as.integer(round(t_int * (int_anchor/cur_mu)))
      }
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
      
      # ════════════════════════════════════════════════════════════════════
      # ATTACKING CASCADE — shots first, then carve subsets out of them.
      # Hierarchy: GOALS ⊂ SOT ⊂ SHOTS. A goal is one of the shots a player was
      # already going to take that happened to go in — NOT a shot added on top
      # of his share. This avoids inflating shooters' volume on scoring sims;
      # the (correct) shots/goals correlation emerges from selection, not a boost.
      # ════════════════════════════════════════════════════════════════════
      
      # ── SUB THINNING: subs absorb a per-sim attacking share. We allocate only
      # the STARTERS' portion of each team total; the rest is scored/taken by
      # (unrostered) subs and leaves the pool. Integer counts preserved via
      # binomial thinning. Starters' own minutes (ms_mat) then tilt the split
      # toward players who stayed on the pitch.
      ts_shots <- rbinom(n_sims, t_shots, keep_atk)
      ts_sot   <- pmin(rbinom(n_sims, t_sot, keep_atk), ts_shots)
      ts_goals <- pmin(rbinom(n_sims, team_goals, keep_atk), ts_sot)
      
      # ── SHOTS: allocate the starters' portion by shot share × per-sim minutes ──
      raw <- matrix(rnb(n_p*n_sims, rep(ss*mean(pmax(ts_shots,1)), n_sims), SOCCER_P$phi_shots), n_p, n_sims) * ms_mat
      shots_m <- pmin(norm_to_total(raw, ts_shots, n_p), 10L)
      deficit <- ts_shots - colSums(shots_m)
      if(any(deficit > 0)) {
        head_sh <- 10L - shots_m
        shots_m <- shots_m + alloc_capped(
          matrix(rep(ss, n_sims), n_p, n_sims) * ms_mat * (head_sh > 0),
          pmax(deficit, 0L), head_sh, ss, n_p)
      }
      mats$Shots[pidx,] <- shots_m
      
      # ── SOT ⊂ SHOTS ──
      raw_sot <- matrix(rnb(n_p*n_sims, rep(sots*mean(pmax(ts_sot,1)), n_sims), SOCCER_P$phi_shots), n_p, n_sims) * ms_mat
      sot_m <- alloc_capped(raw_sot, ts_sot, shots_m, sots, n_p)
      mats$SOT[pidx,] <- sot_m
      
      # ── GOALS ⊂ SOT (starters' portion; sub-scored goals already removed) ──
      raw_g <- matrix(rnb(n_p*n_sims, rep(gs*pmax(mean(ts_goals),0.01), n_sims), SOCCER_P$phi_def), n_p, n_sims)
      raw_g <- raw_g * (sot_m > 0L) * ms_mat   # must have an SOT this sim, weight by minutes
      goals_m <- alloc_capped(raw_g, as.numeric(ts_goals), sot_m, gs, n_p)
      mats$Goals[pidx,] <- goals_m
      
      # ── ASSISTS: assist on each STARTER goal, credited to a starter != scorer.
      # (Goals scored by subs, and their assists, have already left the pool.) ──
      ast_m <- matrix(0L, n_p, n_sims)
      for(s in seq_len(n_sims)) {
        tg <- ts_goals[s]; if(tg==0) next
        gr <- goals_m[,s]
        for(g_i in seq_len(tg)) {
          sc <- which(gr>0); if(!length(sc)) next
          scorer <- if(length(sc)==1) sc else sample(sc,1,prob=gr[sc])
          gr[scorer] <- gr[scorer]-1L
          if(runif(1)>SOCCER_P$p_assist) next        # not every goal is assisted
          ap <- as_s * ms_mat[,s]; ap[scorer] <- 0    # assister != scorer, weight by minutes
          if(sum(ap)==0) next
          ast_m[sample.int(n_p,1,prob=ap), s] <- ast_m[sample.int(n_p,1,prob=ap), s] + 1L
        }
      }
      mats$Assists[pidx,] <- ast_m
      
      # ── CC (chances created attach to OTHER players' shots) ──
      # A created chance is the pass/cross feeding a shot taken by a DIFFERENT
      # player. Allocate team CC by creation (assist) share, but suppress each
      # player's CC by how much of the team's shooting they did this sim — the
      # primary finisher creates few of his own chances. This enforces the
      # shooter != creator linkage at the player level without a per-chance loop.
      total_shots_sim <- colSums(mats$Shots[pidx,,drop=FALSE])
      shot_frac <- sweep(mats$Shots[pidx,,drop=FALSE], 2,
                         pmax(total_shots_sim, 1), `/`)   # each player's share of shots
      create_weight <- matrix(rep(as_s, n_sims), n_p, n_sims) * (1 - shot_frac) * ms_mat
      # renormalize per sim; if a column collapses, fall back to flat creation share
      cw_cs <- colSums(create_weight)
      for(s in which(cw_cs <= 0)) create_weight[,s] <- as_s
      ts_cc <- rbinom(n_sims, t_cc, keep_atk)            # subs absorb attacking CC
      raw_cc <- matrix(rnb(n_p*n_sims, rep(as_s*mean(pmax(ts_cc,1)), n_sims), SOCCER_P$phi_def), n_p, n_sims)
      raw_cc <- raw_cc * create_weight
      mats$CC[pidx,] <- pmin(norm_to_total(raw_cc, ts_cc, n_p), 8L)
      # Assist is a subset of CC: a player's CC must be at least their assists.
      mats$CC[pidx,] <- pmax(mats$CC[pidx,], ast_m)
      
      # Crosses: split into set piece (corners) and open play
      set_shares <- if("Set_Pct" %in% names(pl)) as.numeric(pl$Set_Pct) else rep(0, n_p)
      set_shares[is.na(set_shares)] <- 0
      has_set <- sum(set_shares) > 0
      
      if(has_set) {
        # Set piece crosses = corners, allocated by SET% × minutes
        set_crosses <- matrix(0L, n_p, n_sims)
        for(s in seq_len(n_sims)) {
          nc <- t_corners[s]; if(nc == 0) next
          w <- set_shares * ms_mat[,s]
          if(sum(w) <= 0) w <- set_shares
          set_crosses[,s] <- rmultinom(1, size=nc, prob=w/sum(w))[,1]
        }
        # Open play crosses = total - corners (attacking → subs absorb a share)
        open_play <- pmax(t_crosses - t_corners, 0L)
        open_play <- rbinom(n_sims, open_play, keep_atk)
        raw_cr <- matrix(rnb(n_p*n_sims, rep(crs*mean(pmax(open_play,1)), n_sims), SOCCER_P$phi_crosses), n_p, n_sims) * ms_mat
        open_crosses <- norm_to_total(raw_cr, open_play, n_p)
        mats$Crosses[pidx,] <- pmin(set_crosses + open_crosses, 15L)
      } else {
        # No set piece data — allocate all crosses by Cross_Share (attacking)
        tc_cr <- rbinom(n_sims, t_crosses, keep_atk)
        raw_cr <- matrix(rnb(n_p*n_sims, rep(crs*mean(pmax(tc_cr,1)), n_sims), SOCCER_P$phi_crosses), n_p, n_sims) * ms_mat
        mats$Crosses[pidx,] <- pmin(norm_to_total(raw_cr, tc_cr, n_p), 15L)
      }
      
      # Tackles (defensive → subs absorb little)
      ts_tk <- rbinom(n_sims, t_tackles, keep_def)
      raw_tk <- matrix(rnb(n_p*n_sims, rep(tks*mean(pmax(ts_tk,1)), n_sims), SOCCER_P$phi_tackles), n_p, n_sims) * ms_mat
      mats$Tackles[pidx,] <- pmin(norm_to_total(raw_tk, ts_tk, n_p), 10L)
      
      # Fouls committed (defensive). alloc_capped redistributes the per-player
      # cap overflow instead of clipping it (clipping leaked fouls and broke the
      # FC=FD identity). Cap raised to 8 to match the FD cap basis.
      ts_fc <- rbinom(n_sims, t_fouls, keep_def)
      raw_fc <- matrix(rnb(n_p*n_sims, rep(fcs*mean(pmax(ts_fc,1)), n_sims), SOCCER_P$phi_fouls), n_p, n_sims) * ms_mat
      player_fc <- alloc_capped(raw_fc, ts_fc, matrix(8L,n_p,n_sims), fcs, n_p)
      mats$FC[pidx,] <- player_fc
      
      # Passes (neutral — subs absorb their minutes share)
      ts_pa <- rbinom(n_sims, t_passes, keep_pas)
      raw_pa <- matrix(rnb(n_p*n_sims, rep(pas*mean(pmax(ts_pa,1)), n_sims), 7.0), n_p, n_sims) * ms_mat
      mats$Passes[pidx,] <- norm_to_total(raw_pa, ts_pa, n_p)
      
      # INT (defensive)
      ts_in <- rbinom(n_sims, t_int, keep_def)
      raw_in <- matrix(rnb(n_p*n_sims, rep(ints*mean(pmax(ts_in,1)), n_sims), SOCCER_P$phi_def), n_p, n_sims) * ms_mat
      mats$INT[pidx,] <- pmin(norm_to_total(raw_in, ts_in, n_p), 8L)
      
      # Cards (YC allocated by YC share from market, constrained by fouls)
      yc_mat <- matrix(0L, n_p, n_sims)
      rc_mat <- matrix(0L, n_p, n_sims)
      # Allocate team YC to players by YC_Share × minutes (benched players can't be booked)
      raw_yc <- matrix(rnb(n_p*n_sims, rep(ycs*mean(t_yc), n_sims), 5.0), n_p, n_sims) * ms_mat
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
        game_info[[gi]]$home_ts_sot <- ts_sot  # rostered (sub-thinned) regulation SOT
        game_info[[gi]]$home_ts_fc <- ts_fc  # rostered committed fouls (sub-thinned)
        game_info[[gi]]$home_pidx <- pidx
        game_info[[gi]]$home_gk <- which(grepl("GK", pl$DK_RosterPos) & mins >= 60)
        game_info[[gi]]$home_fd_share <- get_share("FD_Share")
        game_info[[gi]]$home_gs <- gs; game_info[[gi]]$home_ss <- ss
        game_info[[gi]]$home_sots <- sots; game_info[[gi]]$home_ms <- ms_mat
        game_info[[gi]]$home_et_ok <- as.numeric(pl$MIN) >= 90  # finishes match -> plays ET
      } else {
        game_info[[gi]]$away_t_sot <- t_sot; game_info[[gi]]$away_t_fouls <- t_fouls
        game_info[[gi]]$away_ts_sot <- ts_sot  # rostered (sub-thinned) regulation SOT
        game_info[[gi]]$away_ts_fc <- ts_fc  # rostered committed fouls (sub-thinned)
        game_info[[gi]]$away_pidx <- pidx
        game_info[[gi]]$away_gk <- which(grepl("GK", pl$DK_RosterPos) & mins >= 60)
        game_info[[gi]]$away_fd_share <- get_share("FD_Share")
        game_info[[gi]]$away_gs <- gs; game_info[[gi]]$away_ss <- ss
        game_info[[gi]]$away_sots <- sots; game_info[[gi]]$away_ms <- ms_mat
        game_info[[gi]]$away_et_ok <- as.numeric(pl$MIN) >= 90  # finishes match -> plays ET
      }
    }
    
    # ── CROSS-REFERENCE: GK saves + Fouls drawn ──
    gd <- game_info[[gi]]
    # Compute saves from ACTUAL allocated SOT (after SOT ≤ Shots constraint)
    if(length(gd$home_gk) && !is.null(gd$away_pidx)) {
      pk <- gd$home_pidx[gd$home_gk[1]]
      actual_away_sot <- colSums(mats$SOT[gd$away_pidx, , drop=FALSE])
      mats$GK_Saves[pk,] <- pmax(actual_away_sot - gd$ag, 0)
      mats$GK_GC[pk,] <- gd$ag
    }
    if(length(gd$away_gk) && !is.null(gd$home_pidx)) {
      pk <- gd$away_pidx[gd$away_gk[1]]
      actual_home_sot <- colSums(mats$SOT[gd$home_pidx, , drop=FALSE])
      mats$GK_Saves[pk,] <- pmax(actual_home_sot - gd$hg, 0)
      mats$GK_GC[pk,] <- gd$hg
    }
    
    # FD: your fouls drawn = opponent's fouls committed. Normalize to the
    # opponent's ROSTERED committed total (ts_fc, sub-thinned) so that across the
    # rostered players FD == FC at the game level. alloc_capped redistributes the
    # per-player cap overflow rather than clipping (clipping leaked fouls).
    away_fc_tot <- gd$away_ts_fc %||% gd$away_t_fouls
    home_fc_tot <- gd$home_ts_fc %||% gd$home_t_fouls
    if(!is.null(away_fc_tot)&&!is.null(gd$home_fd_share)) {
      hp<-gd$home_pidx; nh<-length(hp); hs<-gd$home_fd_share
      raw_fd<-matrix(rnb(nh*n_sims,rep(hs*mean(away_fc_tot),n_sims),SOCCER_P$phi_fouls),nh,n_sims)
      capm<-matrix(8L,nh,n_sims)
      mats$FD[hp,] <- alloc_capped(raw_fd, away_fc_tot, capm, hs, nh)
    }
    if(!is.null(home_fc_tot)&&!is.null(gd$away_fd_share)) {
      ap<-gd$away_pidx; na_p<-length(ap); as2<-gd$away_fd_share
      raw_fd<-matrix(rnb(na_p*n_sims,rep(as2*mean(home_fc_tot),n_sims),SOCCER_P$phi_fouls),na_p,n_sims)
      capm<-matrix(8L,na_p,n_sims)
      mats$FD[ap,] <- alloc_capped(raw_fd, home_fc_tot, capm, as2, na_p)
    }
    
    # ── EXTRA TIME (game-level; tied sims only) ──────────────────────────
    if(isTRUE(SOCCER_P$et_enable)) {
      tied <- which(gd$hg == gd$ag)
      if(!exists("et_went")) et_went <- logical(n_sims)  # per-sim ET flag (any game)
      if(length(tied)) {
        et_went[tied] <- TRUE
        hl <- gd$game$Home_Lambda; al <- gd$game$Away_Lambda
        gsc <- (SOCCER_P$et_minutes/90) * SOCCER_P$et_goal_intensity   # goal-rate scale
        ssc <- (SOCCER_P$et_minutes/90) * SOCCER_P$et_int_shots        # shot exposure scale
        # Team ET shots over the 30', from each team's regulation shot level.
        hsh <- gd$game$Home_Shots %||% (hl*8); ash <- gd$game$Away_Shots %||% (al*8)
        # ── COUPLED ET DRAW (preserve goals<->shots correlation) ──
        # Draw team ET shots first, then ET goals CONDITIONAL on those shots, so
        # a team with more ET shots is proportionally more likely to score in ET
        # (the same goals-within-shots nesting the regulation pass uses).
        et_h_shots <- integer(n_sims); et_a_shots <- integer(n_sims)
        et_hg <- integer(n_sims); et_ag <- integer(n_sims)
        nt <- length(tied)
        et_h_shots[tied] <- rpois(nt, max(hsh,1)*ssc)
        et_a_shots[tied] <- rpois(nt, max(ash,1)*ssc)
        # conversion rate = team goal-rate / team shot-rate (goals per shot), so
        # E[ET goals] = ET shots * conv = matches the intended ET goal expectation,
        # but now realized goals scale WITH the realized ET shots (coupling).
        conv_h <- min(max((hl*gsc) / max(hsh*ssc, 1e-6), 0), 0.6)
        conv_a <- min(max((al*gsc) / max(ash*ssc, 1e-6), 0), 0.6)
        et_hg[tied] <- rbinom(nt, et_h_shots[tied], conv_h)
        et_ag[tied] <- rbinom(nt, et_a_shots[tied], conv_a)
        
        # Allocate ET goals/shots to players who FINISH the match. A player is on
        # for extra time iff his input MIN >= 90 (the user lowers MIN to mark a
        # starter as subbed off). Players under 90 get ZERO ET production. Within
        # the finishers, distribute by stat share (no minutes weighting needed —
        # they all play the full 30).
        alloc_et <- function(pidx, gs, ss, sots, et_ok, et_g, et_sh) {
          if(is.null(pidx) || !length(pidx)) return(invisible(NULL))
          np <- length(pidx)
          if(is.null(et_ok)) et_ok <- rep(TRUE, np)
          gate <- as.numeric(et_ok)                    # 1 if finishes match, else 0
          if(sum(gate) <= 0) return(invisible(NULL))   # nobody left on -> no ET production
          wsh <- ss * gate; wg <- gs * gate
          for(s in tied) {
            nsh <- et_sh[s]
            if(nsh > 0L && sum(wsh) > 0) {
              sa <- sample.int(np, nsh, replace=TRUE, prob=wsh/sum(wsh))
              for(z in sa) mats$Shots[pidx[z], s] <- mats$Shots[pidx[z], s] + 1L
            }
            ng <- et_g[s]; if(ng == 0L) next
            if(sum(wg) <= 0) next
            sc <- sample.int(np, ng, replace=TRUE, prob=wg/sum(wg))
            for(z in sc) {
              mats$Goals[pidx[z], s] <- mats$Goals[pidx[z], s] + 1L
              mats$SOT[pidx[z], s]   <- mats$SOT[pidx[z], s] + 1L
              if(mats$Shots[pidx[z], s] < mats$SOT[pidx[z], s])
                mats$Shots[pidx[z], s] <- mats$SOT[pidx[z], s]
            }
          }
        }
        alloc_et(gd$home_pidx, gd$home_gs, gd$home_ss, gd$home_sots, gd$home_et_ok, et_hg, et_h_shots)
        alloc_et(gd$away_pidx, gd$away_gs, gd$away_ss, gd$away_sots, gd$away_et_ok, et_ag, et_a_shots)
        
        # Extend exposure for the extra 30' for non-goal-coupled counting stats,
        # ONLY for players who finish the match (input MIN>=90). Each stat uses
        # its own research-grounded ET intensity (passes/crosses fall hardest,
        # tackles/fouls moderately, per the ET sports-science findings).
        et_factor <- function(stat) {
          switch(stat,
                 Tackles = SOCCER_P$et_int_tackle,
                 FC      = SOCCER_P$et_int_foul,
                 Passes  = SOCCER_P$et_int_pass,
                 Crosses = SOCCER_P$et_int_cross,
                 INT     = SOCCER_P$et_int_int,
                 SOCCER_P$et_int_default)
        }
        bump_side <- function(pidx, et_ok) {
          if(is.null(pidx) || !length(pidx)) return(invisible(NULL))
          if(is.null(et_ok)) et_ok <- rep(TRUE, length(pidx))
          gate <- as.numeric(et_ok)                    # 1 if finishes, else 0
          for(stat in c("Tackles","Passes","INT","Crosses","FC")) {
            M <- mats[[stat]]; if(is.null(M)) next
            et_bump <- (SOCCER_P$et_minutes/90) * et_factor(stat)
            sub <- M[pidx, tied, drop=FALSE]
            pr  <- pmin(et_bump * gate, 0.95)          # per-player bump prob (0 if subbed off)
            prmat <- matrix(rep(pr, length(tied)), nrow(sub), ncol(sub))
            add <- rbinom(length(sub), as.integer(sub), as.vector(prmat))
            M[pidx, tied] <- sub + matrix(add, nrow(sub), ncol(sub))
            mats[[stat]] <<- M
          }
        }
        bump_side(gd$home_pidx, gd$home_et_ok)
        bump_side(gd$away_pidx, gd$away_et_ok)
        # SOT grows with the ET shots a player picked up (on-target fraction),
        # in the tied sims only, then enforce nesting goals<=SOT<=shots.
        for(pidx in list(gd$home_pidx, gd$away_pidx)) {
          if(is.null(pidx)||!length(pidx)) next
          # headroom = shots not yet counted as SOT; convert ~38% of it to SOT
          head <- pmax(mats$Shots[pidx,tied,drop=FALSE] - mats$SOT[pidx,tied,drop=FALSE], 0L)
          addsot <- rbinom(length(head), as.integer(head), 0.38)
          mats$SOT[pidx,tied] <- mats$SOT[pidx,tied,drop=FALSE] + matrix(addsot, nrow(head), ncol(head))
          mats$SOT[pidx,]   <- pmin(mats$SOT[pidx,], mats$Shots[pidx,])
          mats$Goals[pidx,] <- pmin(mats$Goals[pidx,], mats$SOT[pidx,])
        }
        
        # Final score after ET, and terminal GK-win / clean-sheet rules.
        final_h <- gd$hg + et_hg; final_a <- gd$ag + et_ag
        game_info[[gi]]$et_hg <- et_hg; game_info[[gi]]$et_ag <- et_ag  # for keeper conceded
        # CS holds through full match (incl ET); ET goal conceded wipes it.
        if(!is.null(gd$home_pidx))
          mats$CS[gd$home_pidx,] <- matrix(rep(as.integer(final_a==0), each=length(gd$home_pidx)), length(gd$home_pidx), n_sims)
        if(!is.null(gd$away_pidx))
          mats$CS[gd$away_pidx,] <- matrix(rep(as.integer(final_h==0), each=length(gd$away_pidx)), length(gd$away_pidx), n_sims)
        # GK win from FINAL score; if STILL tied after ET -> penalties -> no win.
        home_win <- as.integer(final_h > final_a)   # 0 when still level (penalties)
        away_win <- as.integer(final_a > final_h)
        if(!is.null(gd$home_pidx))
          mats$GK_Win[gd$home_pidx,] <- matrix(rep(home_win, each=length(gd$home_pidx)), length(gd$home_pidx), n_sims)
        if(!is.null(gd$away_pidx))
          mats$GK_Win[gd$away_pidx,] <- matrix(rep(away_win, each=length(gd$away_pidx)), length(gd$away_pidx), n_sims)
        # GK conceded updated to full-match (post-ET). Saves are recomputed once,
        # after all games, from final SOT and final goals (single source of truth,
        # consistent with the validation frame) — see post-loop saves pass.
        if(length(gd$home_gk) && !is.null(gd$home_pidx)) {
          pk <- gd$home_pidx[gd$home_gk[1]]; mats$GK_GC[pk,] <- final_a
        }
        if(length(gd$away_gk) && !is.null(gd$away_pidx)) {
          pk <- gd$away_pidx[gd$away_gk[1]]; mats$GK_GC[pk,] <- final_h
        }
        et_rate <- length(tied)/n_sims
        cat(sprintf("  %s: ET in %.1f%% of sims (tied after 90)\n", gd$game$Game, 100*et_rate))
      }
    }
    
    cb(sprintf("Game %d/%d complete", gi, n_games), pct_base+0.72/n_games)
  }
  
  # ── FINAL GK SAVES (single source of truth, FULL team totals) ─────────────
  # The keeper is rostered and on the pitch the whole match, so he faces the
  # ENTIRE opposing attack — rostered starters AND the unrostered sub bucket.
  # His saves/conceded therefore derive from the FULL team SOT and FULL team
  # goals (market totals incl ET), NOT the sub-thinned rostered SOT. The sub
  # bucket = full regulation total minus the rostered (thinned) total; ET
  # production goes only to finishers so it is already in the rostered colSums.
  for(gi in seq_len(n_games)) {
    gd <- game_info[[gi]]; ht <- gd$game$Home; at <- gd$game$Away
    h_idx <- which(all_players_list$Team==ht); a_idx <- which(all_players_list$Team==at)
    if(!length(h_idx) || !length(a_idx)) next
    # rostered SOT/goals after all passes (includes ET, which only finishers get)
    h_sot_r <- colSums(mats$SOT[h_idx,,drop=FALSE]); a_sot_r <- colSums(mats$SOT[a_idx,,drop=FALSE])
    h_g_r   <- colSums(mats$Goals[h_idx,,drop=FALSE]); a_g_r <- colSums(mats$Goals[a_idx,,drop=FALSE])
    # sub-bucket SOT (regulation only) = full regulation total - rostered regulation
    h_sub_sot <- pmax((gd$home_t_sot %||% h_sot_r) - (gd$home_ts_sot %||% h_sot_r), 0)
    a_sub_sot <- pmax((gd$away_t_sot %||% a_sot_r) - (gd$away_ts_sot %||% a_sot_r), 0)
    # FULL team SOT the opposing keeper faces = rostered(final, incl ET) + sub bucket
    h_sot_full <- h_sot_r + h_sub_sot
    a_sot_full <- a_sot_r + a_sub_sot
    # FULL team goals conceded = final scoreline (incl ET), which already counts
    # any sub-bucket goals (scoreline is the market total, not the rostered sum)
    h_g_full <- gd$hg + (if(!is.null(gd$et_hg)) gd$et_hg else 0L)
    a_g_full <- gd$ag + (if(!is.null(gd$et_ag)) gd$et_ag else 0L)
    h_gk <- which(all_players_list$Team==ht & grepl("GK", all_players_list$DK_RosterPos))
    a_gk <- which(all_players_list$Team==at & grepl("GK", all_players_list$DK_RosterPos))
    # home keeper faces away attack; away keeper faces home attack
    if(length(h_gk)) { mats$GK_Saves[h_gk[1],] <- pmax(a_sot_full - a_g_full, 0); mats$GK_GC[h_gk[1],] <- a_g_full }
    if(length(a_gk)) { mats$GK_Saves[a_gk[1],] <- pmax(h_sot_full - h_g_full, 0); mats$GK_GC[a_gk[1],] <- h_g_full }
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
    Team=rep(all_players_list$Team, n_sims), Pos=rep(all_players_list$DK_RosterPos, n_sims),
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
  metadata[, DKOwn := 0]; metadata[, PosGroup := DK_RosterPos]
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
                   Pos=all_players_list$DK_RosterPos, Salary=all_players_list$DK_Salary,
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
  
  sl <- rbindlist(lapply(seq_len(n_games), function(gi) { gd<-game_info[[gi]]
  data.table(Game=gd$game$Game, HG=gd$hg, AG=gd$ag, Scoreline=paste0(gd$hg,"-",gd$ag)) }))
  tm <- sim_results[, .(Goals=mean(Goals), Shots=mean(Shots), SOT=mean(SOT), CC=mean(CC),
                        Crosses=mean(Crosses), Tackles=mean(Tackles), FD=mean(FD), FC=mean(FC),
                        Passes=mean(Passes), INT=mean(INT)), by=Team]
  
  # ── COMPREHENSIVE VISUALS ──
  team_list <- unique(all_players_list$Team)
  vis_sims <- min(n_sims, 1000); vis_idx <- sort(sample.int(n_sims, vis_sims))
  
  # Score distribution (sampled for plots)
  score_dist <- data.table(
    Player=rep(all_players_list$Player, vis_sims),
    Team=rep(all_players_list$Team, vis_sims),
    DKScore=as.vector(dk_mat[, vis_idx]))
  
  # Stat distribution for validation
  stat_dist <- data.table(
    Player=rep(all_players_list$Player, vis_sims),
    Team=rep(all_players_list$Team, vis_sims),
    Goals=as.vector(mats$Goals[, vis_idx]),
    Assists=as.vector(mats$Assists[, vis_idx]),
    Shots=as.vector(mats$Shots[, vis_idx]),
    SOT=as.vector(mats$SOT[, vis_idx]),
    CC=as.vector(mats$CC[, vis_idx]),
    Crosses=as.vector(mats$Crosses[, vis_idx]),
    TKLW=as.vector(mats$Tackles[, vis_idx]),
    FD=as.vector(mats$FD[, vis_idx]),
    FC=as.vector(mats$FC[, vis_idx]),
    Passes=as.vector(mats$Passes[, vis_idx]),
    INT=as.vector(mats$INT[, vis_idx]),
    YC=as.vector(mats$YC[, vis_idx]),
    GK_Saves=as.vector(mats$GK_Saves[, vis_idx]))
  
  # Game overview: scoreline grid, outcomes, CS rates
  game_overview <- lapply(seq_len(n_games), function(gi) {
    gd <- game_info[[gi]]; ht <- gd$game$Home; at <- gd$game$Away
    hg <- gd$hg; ag <- gd$ag
    # Scoreline grid (probability heatmap)
    grid <- data.table(HG=hg, AG=ag)[, .(Prob=round(.N/n_sims*100,1)), by=.(HG,AG)]
    # Outcomes
    h_win <- round(mean(hg>ag)*100,1); draw <- round(mean(hg==ag)*100,1); a_win <- round(mean(hg<ag)*100,1)
    # Clean sheet rates
    h_cs <- round(mean(ag==0)*100,1); a_cs <- round(mean(hg==0)*100,1)
    # Total goals distribution
    tg <- hg+ag; tg_dist <- data.table(TotalGoals=tg)[, .(Prob=round(.N/n_sims*100,1)), by=TotalGoals][order(TotalGoals)]
    # Avg goals
    avg_hg <- round(mean(hg),2); avg_ag <- round(mean(ag),2)
    list(game=gd$game$Game, home=ht, away=at, grid=grid,
         h_win=h_win, draw=draw, a_win=a_win,
         h_cs=h_cs, a_cs=a_cs, avg_hg=avg_hg, avg_ag=avg_ag,
         total_goals_dist=tg_dist)
  })
  
  # Team stat distributions (per-sim team totals for histograms)
  team_sim_stats <- list()
  for(tname in team_list) {
    tidx <- which(all_players_list$Team == tname)
    team_sim_stats[[tname]] <- data.table(
      Team=tname,
      Goals=colSums(mats$Goals[tidx, vis_idx, drop=FALSE]),
      Shots=colSums(mats$Shots[tidx, vis_idx, drop=FALSE]),
      SOT=colSums(mats$SOT[tidx, vis_idx, drop=FALSE]),
      CC=colSums(mats$CC[tidx, vis_idx, drop=FALSE]),
      Crosses=colSums(mats$Crosses[tidx, vis_idx, drop=FALSE]),
      TKLW=colSums(mats$Tackles[tidx, vis_idx, drop=FALSE]),
      FD=colSums(mats$FD[tidx, vis_idx, drop=FALSE]),
      FC=colSums(mats$FC[tidx, vis_idx, drop=FALSE]),
      Passes=colSums(mats$Passes[tidx, vis_idx, drop=FALSE]),
      INT=colSums(mats$INT[tidx, vis_idx, drop=FALSE]),
      YC=colSums(mats$YC[tidx, vis_idx, drop=FALSE]))
  }
  team_sim_dt <- rbindlist(team_sim_stats)
  
  # Player goal frequency (0G/1G/2G/3G+ per player)
  goal_freq <- data.table(
    Player=all_players_list$Player, Team=all_players_list$Team,
    Pos=all_players_list$DK_RosterPos, Salary=all_players_list$DK_Salary,
    G0=round(rowMeans(mats$Goals==0)*100,1), G1=round(rowMeans(mats$Goals==1)*100,1),
    G2=round(rowMeans(mats$Goals==2)*100,1), G3plus=round(rowMeans(mats$Goals>=3)*100,1),
    A0=round(rowMeans(mats$Assists==0)*100,1), A1plus=round(rowMeans(mats$Assists>=1)*100,1))
  setorder(goal_freq, G0)
  
  # Cross-reference validation
  xref <- list()
  for(gi in seq_len(n_games)) {
    gd <- game_info[[gi]]; ht <- gd$game$Home; at <- gd$game$Away
    h_idx <- which(all_players_list$Team==ht); a_idx <- which(all_players_list$Team==at)
    h_sot <- colSums(mats$SOT[h_idx,,drop=FALSE]); a_sot <- colSums(mats$SOT[a_idx,,drop=FALSE])
    h_g <- colSums(mats$Goals[h_idx,,drop=FALSE]); a_g <- colSums(mats$Goals[a_idx,,drop=FALSE])
    h_fc <- colSums(mats$FC[h_idx,,drop=FALSE]); a_fc <- colSums(mats$FC[a_idx,,drop=FALSE])
    h_fd <- colSums(mats$FD[h_idx,,drop=FALSE]); a_fd <- colSums(mats$FD[a_idx,,drop=FALSE])
    # FULL team totals (rostered + sub bucket, incl ET) — what the keeper faces.
    h_sub_sot <- pmax((gd$home_t_sot %||% h_sot) - (gd$home_ts_sot %||% h_sot), 0)
    a_sub_sot <- pmax((gd$away_t_sot %||% a_sot) - (gd$away_ts_sot %||% a_sot), 0)
    h_sot_full <- h_sot + h_sub_sot; a_sot_full <- a_sot + a_sub_sot
    h_g_full <- gd$hg + (if(!is.null(gd$et_hg)) gd$et_hg else 0L)
    a_g_full <- gd$ag + (if(!is.null(gd$et_ag)) gd$et_ag else 0L)
    h_gk_idx <- which(all_players_list$Team==ht & grepl("GK",all_players_list$DK_RosterPos))
    a_gk_idx <- which(all_players_list$Team==at & grepl("GK",all_players_list$DK_RosterPos))
    h_sv <- if(length(h_gk_idx)) rowMeans(mats$GK_Saves[h_gk_idx,,drop=FALSE]) else 0
    a_sv <- if(length(a_gk_idx)) rowMeans(mats$GK_Saves[a_gk_idx,,drop=FALSE]) else 0
    xref[[gi]] <- data.table(
      Game=gd$game$Game,
      Check=c("FC↔FD", "FC↔FD", "SOT-G=Sv", "SOT-G=Sv", "CC/Shots", "CC/Shots"),
      Team=c(ht, at, ht, at, ht, at),
      # SOT-G=Sv now checks FULL opponent attack vs keeper saves (keeper faces all).
      Value=round(c(mean(h_fc), mean(a_fc), mean(h_sot_full)-mean(h_g_full), mean(a_sot_full)-mean(a_g_full),
                    mean(colSums(mats$CC[h_idx,,drop=FALSE]))/mean(h_sot+h_g), mean(colSums(mats$CC[a_idx,,drop=FALSE]))/mean(a_sot+a_g)),2),
      ShouldEqual=round(c(mean(a_fd), mean(h_fd), a_sv, h_sv,
                          0.74, 0.74),2),
      Match=c(abs(mean(h_fc)-mean(a_fd))<0.5, abs(mean(a_fc)-mean(h_fd))<0.5,
              abs(mean(h_sot_full)-mean(h_g_full)-a_sv)<0.3, abs(mean(a_sot_full)-mean(a_g_full)-h_sv)<0.3,
              TRUE, TRUE))
  }
  xref_dt <- rbindlist(xref)
  
  elapsed <- as.numeric(proc.time()["elapsed"]-t0)
  cat(sprintf("\n  Complete: %d players | %s sims | %.1fs (%.0f sims/sec)\n", n_total, format(n_sims,big.mark=","), elapsed, n_sims/elapsed))
  cb("Complete", 1.0)
  
  # ── CORRELATION VALIDATION ───────────────────────────────────────────────
  # Compute realized correlations from the simulated per-sim totals, split by
  # whether the sim went to extra time, so ET fidelity can be verified.
  if(!exists("et_went")) et_went <- logical(n_sims)
  corr_diag <- tryCatch({
    sr <- sim_results
    # team totals per (Team, SimID)
    tt <- sr[, .(G=sum(Goals), Sh=sum(Shots), ST=sum(SOT)), by=.(Team, SimID)]
    teams_u <- unique(tt$Team)
    et_by_sim <- data.table(SimID=seq_len(n_sims), ET=et_went)
    tt <- merge(tt, et_by_sim, by="SimID")
    block <- function(dt, lbl) {
      if(nrow(dt) < 50) return(NULL)
      data.table(Subset=lbl, N=nrow(dt),
                 cor_G_Sh = round(suppressWarnings(cor(dt$G, dt$Sh)), 3),
                 cor_G_ST = round(suppressWarnings(cor(dt$G, dt$ST)), 3),
                 cor_Sh_ST= round(suppressWarnings(cor(dt$Sh, dt$ST)), 3))
    }
    team_corr <- rbindlist(list(
      block(tt, "ALL"),
      block(tt[ET==FALSE], "NoET"),
      block(tt[ET==TRUE],  "ET")), use.names=TRUE, fill=TRUE)
    # opponent anti-correlation: home goals vs away goals, per game per sim
    opp <- NULL
    g_meta <- unique(sr[, .(Team)])
    # team goals wide per sim
    gw <- dcast(tt, SimID + ET ~ Team, value.var="G")
    if(length(teams_u) >= 2) {
      pairs_list <- list()
      for(a in 1:(length(teams_u)-1)) for(b in (a+1):length(teams_u)) {
        ta <- teams_u[a]; tb <- teams_u[b]
        x <- gw[[ta]]; y <- gw[[tb]]
        ok <- !is.na(x) & !is.na(y)
        if(sum(ok) > 50 && sd(x[ok])>0 && sd(y[ok])>0) {
          pairs_list[[length(pairs_list)+1]] <- data.table(
            TeamA=ta, TeamB=tb,
            cor_goals_all = round(cor(x[ok], y[ok]), 3),
            cor_goals_ET  = { e<-gw$ET&ok; if(sum(e)>50 && sd(x[e])>0 && sd(y[e])>0) round(cor(x[e],y[e]),3) else NA_real_ })
        }
      }
      opp <- rbindlist(pairs_list, fill=TRUE)
    }
    # Per-stat means / p95 ceilings, split ET vs non-ET (player-level), to check
    # ET isn't distorting ceilings unexpectedly.
    sr2 <- merge(sr, et_by_sim, by="SimID")
    stat_split <- sr2[, .(
      mean_DK = round(mean(DKScore),2),
      p95_DK  = round(quantile(DKScore, .95),1),
      mean_Sh = round(mean(Shots),2),
      mean_G  = round(mean(Goals),3)
    ), by=ET][order(ET)]
    
    out <- list(team_corr=team_corr, opp_corr=opp, stat_split=stat_split,
                et_rate=mean(et_went), n_sims=n_sims)
    # ── Console dump (copy/paste to verify behavior) ──
    cat("\n================ CORRELATION DIAGNOSTIC ================\n")
    cat(sprintf("ET rate (sims that went to extra time): %.1f%%\n", 100*mean(et_went)))
    cat("\nTeam stat correlations (split by ET):\n")
    print(team_corr)
    if(!is.null(opp) && nrow(opp)) { cat("\nOpponent goals correlation (NOTE: ET-subset is naturally HIGH — tied sims have hg==ag by definition, ET pulls it down from 1.0; read the ALL column):\n"); print(opp) }
    cat("\nPer-stat means / ceilings (ET vs non-ET sims):\n")
    print(stat_split)
    cat("=======================================================\n")
    cat("To validate ET: run once with SOCCER_P$et_enable=FALSE and once TRUE,\n")
    cat("then send both dumps. NoET correlations should be IDENTICAL between runs;\n")
    cat("only the ET-subset rows should differ.\n")
    cat("=======================================================\n\n")
    out
  }, error=function(e) { cat(sprintf("\n[corr_diag error: %s]\n", conditionMessage(e))); list(error=conditionMessage(e)) })
  
  list(sim_results=sim_results, metadata=metadata, dk_mat=dk_mat,
       corr_diag=corr_diag,
       sport_visuals=list(
         player_means=pm, team_means=tm, scoreline_data=sl, games=games,
         teams=team_list, score_dist=score_dist, stat_dist=stat_dist,
         game_overview=game_overview, team_sim_stats=team_sim_dt,
         goal_freq=goal_freq, xref=xref_dt),
       has_sd=has_sd)
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
    n_slate_teams <- length(unique(meta$Team))
    if(n_slate_teams >= 3 && length(unique(ch$Team))<3L) next
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