# ============================================================================
# NBA SIMULATION ENGINE — Golden Ticket Sims
# ============================================================================
# Stat model: Negative Binomial draws per player per stat, normalized to
# sim-row team totals. fg3_rate drawn from Beta(alpha,beta) per player per sim.
# See InputMaker_NBA.R for parameter estimation details.
# ============================================================================

library(data.table); library(readxl); library(lpSolve)

# ── INPUT READER ──────────────────────────────────────────────────────────────

read_nba_input <- function(file_path) {
  sheets <- excel_sheets(file_path)
  ids    <- as.data.table(read_excel(file_path, sheet="IDs"))
  setnames(ids, trimws(names(ids))); setnames(ids, "Name", "Player")
  games  <- as.data.table(read_excel(file_path, sheet="Games"))
  setnames(games, trimws(names(games)))
  games[, SimKey := paste0(HomeTeam,"_vs_",AwayTeam)]
  team_game_lu <- rbind(
    games[, .(Team=HomeTeam,SimKey,GameKey,GameTime,GameRank,OverUnder,HomeSpread,ShowdownFile)],
    games[, .(Team=AwayTeam,SimKey,GameKey,GameTime,GameRank,OverUnder,HomeSpread,ShowdownFile)]
  )
  sim_names <- grep("^Sim_",sheets,value=TRUE)
  if (!length(sim_names)) stop("No Sim_ sheets found.")
  sim_games <- setNames(lapply(sim_names,function(s) as.data.table(read_excel(file_path,sheet=s))),
                        sub("^Sim_","",sim_names))
  sd_names <- grep("^SD\\d+_IDs$",sheets,value=TRUE)
  sd_ids <- if (length(sd_names)) setNames(lapply(sd_names,function(s){
    dt<-as.data.table(read_excel(file_path,sheet=s)); setnames(dt,trimws(names(dt))); dt
  }), sub("_IDs$","",sd_names)) else list()
  team_sheet_names <- setdiff(sheets,c("IDs","Games",sd_names,sim_names))
  if (!length(team_sheet_names)) stop("No team tabs found.")
  team_data <- setNames(lapply(team_sheet_names,function(s){
    dt<-as.data.table(read_excel(file_path,sheet=s)); setnames(dt,trimws(names(dt)))
    drop<-intersect(c("DKSal","FDSal","DKSalary","FDSalary","DKProj","FDProj",
                      "RGProj","RGFDProj","Mins","DKOwn","FDOwn"),names(dt))
    if (length(drop)) dt[,(drop):=NULL]; dt
  }), team_sheet_names)
  slate <- merge(ids, team_game_lu, by="Team", all.x=TRUE)
  if ("DKPos" %in% names(slate)) {
    slate[,dk_g_elig:=grepl("PG|SG",DKPos)][,dk_f_elig:=grepl("SF|PF",DKPos)]
    slate[,dk_c_elig:=grepl("^C$|C/|/C",DKPos)]
  }
  if ("FDPos" %in% names(slate)) {
    slate[,fd_g_elig:=grepl("PG|SG",FDPos)][,fd_f_elig:=grepl("SF|PF",FDPos)]
    slate[,fd_c_elig:=grepl("^C$|C/|/C",FDPos)]
  }
  slate <- unique(slate, by="Player")
  cat(sprintf("NBA Input: %d players | %d games | %d team tabs | %d sim sheets\n",
              nrow(slate),nrow(games),length(team_data),length(sim_games)))
  list(slate=slate,sim_games=sim_games,team_data=team_data,games=games,sd_ids=sd_ids)
}

# ── SCORING ───────────────────────────────────────────────────────────────────

dk_score_nba <- function(pts,tpm,reb,ast,stl,blk,to) {
  base <- pts+tpm*0.5+reb*1.25+ast*1.5+stl*2+blk*2-to*0.5
  cats <- (pts>=10)+(reb>=10)+(ast>=10)+(blk>=10)+(stl>=10)
  base+ifelse(cats>=3,3.0,ifelse(cats>=2,1.5,0.0))
}
fd_score_nba <- function(pts,fgm,tpm,reb,ast,stl,blk,to)
  pts+fgm*2+tpm+reb*1.2+ast*1.5+stl*3+blk*3-to

# ── NEGBIN DRAW ───────────────────────────────────────────────────────────────
# NegBin(size=phi, mu=mu): mean=mu, var=mu+mu^2/phi
draw_negbin <- function(mu,phi,n) as.integer(rnbinom(n,size=max(phi,0.5),mu=max(mu,0)))

# ── MAIN SIMULATION ───────────────────────────────────────────────────────────

run_nba_simulation <- function(input_data, n_sims=10000, config=NULL, progress_callback=NULL) {
  
  slate<-input_data$slate; sim_games<-input_data$sim_games; team_data<-input_data$team_data
  cb<-function(d,v){if(!is.null(progress_callback))progress_callback(d,v)
    cat(sprintf("  [%.0f%%] %s\n",v*100,d));flush.console()}
  start_time<-proc.time(); team_abbrevs<-unique(slate$Team)
  ASSIST_RATE_3PM<-0.80; ASSIST_RATE_2PM<-0.49
  share_stats<-c("fgm","ftm","reb","ast","stl","blk","to")
  sim_col<-c(fgm="fgm",ftm="ftm",reb="reb",ast="ast",stl="stl",blk="blk",to="to")
  mu_cols <-c(fgm="fgm_mu_pm",ftm="ftm_mu_pm",reb="reb_mu_pm",ast="ast_mu_pm",
              stl="stl_mu_pm",blk="blk_mu_pm",to="to_mu_pm")
  phi_cols<-c(fgm="fgm_phi",  ftm="ftm_phi",  reb="reb_phi",  ast="ast_phi",
              stl="stl_phi",  blk="blk_phi",  to="to_phi")
  for (ta in team_abbrevs) if (!ta %in% names(team_data)) stop(sprintf("No team tab: %s",ta))
  
  # ── Player list ──
  cb("Building player roster...",0.03)
  slate_cols<-c("Player","DKID","FDID","DKSalary","FDSalary","DKPos","FDPos","DKOwn","FDOwn",
                "GameKey","SimKey","GameTime","GameRank","OverUnder","HomeSpread",
                "DKProj","FDProj","Mins","Team")
  player_list<-rbindlist(lapply(team_abbrevs,function(team){
    tab<-team_data[[team]]; sl<-slate[Team==team,intersect(slate_cols,names(slate)),with=FALSE]
    m<-tab[Name %in% sl$Player]; if(!nrow(m)) return(NULL)
    merged<-merge(m,sl,by.x="Name",by.y="Player",all.x=TRUE)
    needed<-c("DKProj","FDProj","Mins","fg3_mean","fg3_alpha","fg3_beta",
              "pot_ast_share","ast_conv",unname(mu_cols),unname(phi_cols))
    for(col in needed) if(!col %in% names(merged)) merged[,(col):=NA_real_]
    merged[,Team:=team]; merged
  }),fill=TRUE)
  if (!nrow(player_list)) stop("No players matched.")
  n_players<-nrow(player_list); player_names<-player_list$Name; player_teams<-player_list$Team
  cat(sprintf("  Active players: %d\n",n_players))
  
  # ── Sim sheets & row sampling ──
  game_keys<-unique(player_list$SimKey)
  game_sim_dt<-setNames(lapply(game_keys,function(gk){
    if(gk %in% names(sim_games)) return(sim_games[[gk]])
    parts<-strsplit(gk,"_vs_")[[1]]; rk<-paste0(parts[2],"_vs_",parts[1])
    if(rk %in% names(sim_games)) return(sim_games[[rk]])
    stop(sprintf("No Sim_ sheet: %s",gk))
  }),game_keys)
  cb("Sampling game rows...",0.06)
  game_row_idx<-setNames(lapply(game_keys,function(gk)
    sample.int(nrow(game_sim_dt[[gk]]),n_sims,replace=TRUE)),game_keys)
  
  # ── Per-team prep ──
  cb("Prepping team data...",0.08)
  team_data_prepped<-setNames(lapply(team_abbrevs,function(team){
    pidx<-which(player_teams==team); n_team<-length(pidx)
    gk<-player_list[pidx[1],SimKey]; dt<-game_sim_dt[[gk]]; ri<-game_row_idx[[gk]]
    totals<-setNames(lapply(share_stats,function(s){
      col<-paste0(team,"_",sim_col[s])
      if(!col %in% names(dt)) stop(sprintf("Sim sheet missing: %s",col))
      as.numeric(dt[[col]])[ri]
    }),share_stats)
    tpm_cn<-paste0(team,"_tpm")
    totals[["tpm_sim"]]<-if(tpm_cn %in% names(dt)) as.numeric(dt[[tpm_cn]])[ri] else rep(0,n_sims)
    # Fixed minutes (no variance this version)
    pm<-as.numeric(player_list$Mins[pidx])
    fb<-if("minutes_avg" %in% names(player_list)) as.numeric(player_list$minutes_avg[pidx]) else rep(NA_real_,length(pidx))
    pm<-ifelse(is.na(pm)|pm<=0,ifelse(is.na(fb)|fb<=0,24,fb),pm)
    # NegBin param matrices
    mu_mat <-matrix(0.0,n_team,length(share_stats),dimnames=list(NULL,share_stats))
    phi_mat<-matrix(4.0,n_team,length(share_stats),dimnames=list(NULL,share_stats))
    for(j in seq_along(share_stats)){
      s<-share_stats[j]
      mu_v<-if(mu_cols[s] %in% names(player_list)){v<-as.numeric(player_list[[mu_cols[s]]][pidx]);v[is.na(v)]<-0;v} else rep(0,n_team)
      phi_v<-if(phi_cols[s] %in% names(player_list)){v<-as.numeric(player_list[[phi_cols[s]]][pidx]);v[is.na(v)]<-4;v} else rep(4,n_team)
      mu_mat[,j]<-mu_v*(pm/36); phi_mat[,j]<-phi_v
    }
    fg3_alpha<-as.numeric(player_list$fg3_alpha[pidx]); fg3_alpha[is.na(fg3_alpha)]<-0.9
    fg3_beta <-as.numeric(player_list$fg3_beta[pidx]);  fg3_beta[is.na(fg3_beta)]<-2.1
    pas<-as.numeric(player_list$pot_ast_share[pidx]); pas[is.na(pas)]<-1/n_team
    acv<-as.numeric(player_list$ast_conv[pidx]);       acv[is.na(acv)]<-0.35
    aw_raw<-pas*acv; aw_sum<-sum(aw_raw,na.rm=TRUE)
    aw<-if(aw_sum>0) aw_raw/aw_sum else rep(1/n_team,n_team)
    list(pidx=pidx,n_team=n_team,totals=totals,mu_mat=mu_mat,phi_mat=phi_mat,
         fg3_alpha=fg3_alpha,fg3_beta=fg3_beta,ast_weight=aw)
  }),team_abbrevs)
  
  # ── NegBin draws + team-total normalization ──
  # For each stat: draw NegBin(mu_i, phi_i) per player, then normalize to
  # sim-row team total. NegBin gives correct tail width; normalization anchors
  # to the game environment. Together they replace the old empirical-percentile
  # interp_shares() approach that compressed variance.
  cb("Drawing stats (NegBin)...",0.15)
  stat_mats<-setNames(lapply(share_stats,function(s) matrix(0L,n_players,n_sims)),share_stats)
  tpm_mat<-matrix(0L,n_players,n_sims)
  
  normalize_to_total <- function(raw_mat, team_tot, mu_vec, n_team) {
    col_sums<-colSums(raw_mat)
    zero_sims<-which(col_sums==0)
    if(length(zero_sims)){
      mu_sum<-sum(mu_vec); shares<-if(mu_sum>0) mu_vec/mu_sum else rep(1/n_team,n_team)
      for(zs in zero_sims){raw_mat[,zs]<-as.integer(round(shares*team_tot[zs]));col_sums[zs]<-max(sum(raw_mat[,zs]),1)}
    }
    scaled <-sweep(raw_mat,2,team_tot/col_sums,`*`)
    floored<-matrix(as.integer(floor(scaled)),n_team,n_sims)
    resid  <-as.integer(round(team_tot))-colSums(floored)
    frac   <-scaled-floored
    for(si in seq_len(n_sims)){
      r<-resid[si]; if(r==0||!is.finite(r)) next
      if(r>0){
        top<-order(frac[,si],decreasing=TRUE)[seq_len(min(r,n_team))]
        floored[top,si]<-floored[top,si]+1L
      } else {
        elig<-which(floored[,si]>0); if(!length(elig)) next
        ord<-elig[order(frac[elig,si])]; sub_n<-seq_len(min(abs(r),length(ord)))
        floored[ord[sub_n],si]<-floored[ord[sub_n],si]-1L
      }
    }
    floored
  }
  
  for(team in team_abbrevs){
    td<-team_data_prepped[[team]]; pidx<-td$pidx
    for(j in seq_along(share_stats)){
      s<-share_stats[j]; mu_vec<-td$mu_mat[,j]; phi_vec<-td$phi_mat[,j]; team_tot<-td$totals[[s]]
      raw_mat<-matrix(0L,td$n_team,n_sims)
      for(pi in seq_len(td$n_team)) raw_mat[pi,]<-draw_negbin(mu_vec[pi],phi_vec[pi],n_sims)
      stat_mats[[s]][pidx,]<-normalize_to_total(raw_mat,team_tot,mu_vec,td$n_team)
    }
  }
  
  # ── Beta fg3_rate -> tpm derivation ──
  # Beta(alpha,beta) per player per sim. Bounded [0,1] by construction.
  # Captures real shot-mix variability (some nights all 2s, some nights all 3s).
  # Normalized to sim-row team tpm. Hard constraint: tpm <= fgm.
  cb("Deriving 3-pointers (Beta fg3_rate)...",0.55)
  for(team in team_abbrevs){
    td<-team_data_prepped[[team]]; pidx<-td$pidx; n_t<-td$n_team
    fgm_t<-stat_mats[["fgm"]][pidx,,drop=FALSE]; sim_tpm<-td$totals[["tpm_sim"]]
    fg3_mat<-matrix(0.0,n_t,n_sims)
    for(pi in seq_len(n_t)) fg3_mat[pi,]<-rbeta(n_sims,max(td$fg3_alpha[pi],0.1),max(td$fg3_beta[pi],0.1))
    tpm_nat<-fgm_t*fg3_mat; nat_sum<-colSums(tpm_nat); nat_sum[nat_sum==0]<-1
    tpm_sc <-sweep(tpm_nat,2,sim_tpm/nat_sum,`*`)
    tpm_rnd<-pmin(matrix(as.integer(round(tpm_sc)),n_t,n_sims),fgm_t)
    tpm_frac<-tpm_sc-floor(tpm_sc); tgt<-as.integer(round(sim_tpm))
    for(s in seq_len(n_sims)){
      diff<-tgt[s]-sum(tpm_rnd[,s])
      if(diff==0||all(fgm_t[,s]==0)) next
      if(diff>0){
        elig<-which(tpm_rnd[,s]<fgm_t[,s]); if(!length(elig)) next
        ord<-elig[order(tpm_frac[elig,s],decreasing=TRUE)]; add<-seq_len(min(diff,length(ord)))
        tpm_rnd[ord[add],s]<-tpm_rnd[ord[add],s]+1L
      } else {
        elig<-which(tpm_rnd[,s]>0); if(!length(elig)) next
        ord<-elig[order(tpm_frac[elig,s])]; sub_n<-seq_len(min(abs(diff),length(ord)))
        tpm_rnd[ord[sub_n],s]<-tpm_rnd[ord[sub_n],s]-1L
      }
    }
    tpm_mat[pidx,]<-tpm_rnd
  }
  
  # ── pts ──
  pts_mat<-2L*stat_mats[["fgm"]]+tpm_mat+stat_mats[["ftm"]]
  
  # ── Assist reallocation (scorer != assister) ──
  cb("Assigning assists...",0.75)
  for(team in team_abbrevs){
    td<-team_data_prepped[[team]]; pidx<-td$pidx; n_t<-td$n_team
    fgm_t<-stat_mats[["fgm"]][pidx,,drop=FALSE]; tpm_t<-tpm_mat[pidx,,drop=FALSE]
    twom_t<-pmax(fgm_t-tpm_t,0L); ast_t<-stat_mats[["ast"]][pidx,,drop=FALSE]
    assistable<-round(tpm_t*ASSIST_RATE_3PM+twom_t*ASSIST_RATE_2PM)
    team_ast<-colSums(ast_t); working_ast<-pmin(team_ast,colSums(assistable))
    working_ast[is.na(working_ast)]<-0L
    scale<-ifelse(!is.na(team_ast)&team_ast>0,working_ast/team_ast,1); scale[is.na(scale)]<-1
    ast_sc<-sweep(ast_t,2,scale,`*`)
    ast_fl<-matrix(as.integer(floor(ast_sc)),n_t,n_sims)
    resid <-working_ast-colSums(ast_fl); resid[is.na(resid)]<-0L; frac<-ast_sc-ast_fl
    for(s in seq_len(n_sims)){r<-as.integer(resid[s]);if(!is.na(r)&&r>0L){
      top<-order(frac[,s],decreasing=TRUE)[seq_len(r)]; ast_fl[top,s]<-ast_fl[top,s]+1L}}
    aw<-td$ast_weight; new_ast<-matrix(0L,n_t,n_sims)
    for(i in seq_len(n_t)){
      bi<-assistable[i,]; if(all(bi==0)) next
      w<-matrix(aw,n_t,n_sims); w[i,]<-0; csw<-colSums(w); zc<-csw==0
      if(any(zc)){w[,zc]<-1/(n_t-1L);w[i,zc]<-0;csw[zc]<-1}
      w<-sweep(w,2,csw,`/`); cr<-sweep(w,2,bi,`*`); cf<-matrix(as.integer(floor(cr)),n_t,n_sims)
      cfs<-cr-cf; cresid<-bi-colSums(cf); cresid[is.na(cresid)]<-0L
      for(s in seq_len(n_sims)){r<-as.integer(cresid[s]);if(!is.na(r)&&r>0L){
        elig<-setdiff(order(cfs[,s],decreasing=TRUE),i); top<-elig[seq_len(min(r,length(elig)))]
        cf[top,s]<-cf[top,s]+1L}}
      new_ast<-new_ast+cf
    }
    stat_mats[["ast"]][pidx,]<-new_ast
  }
  
  # ── Score ──
  cb("Scoring...",0.90)
  dk_mat<-dk_score_nba(pts_mat,tpm_mat,stat_mats[["reb"]],stat_mats[["ast"]],stat_mats[["stl"]],stat_mats[["blk"]],stat_mats[["to"]])
  fd_mat<-fd_score_nba(pts_mat,stat_mats[["fgm"]],tpm_mat,stat_mats[["reb"]],stat_mats[["ast"]],stat_mats[["stl"]],stat_mats[["blk"]],stat_mats[["to"]])
  
  # ── Assemble ──
  cb("Assembling results...",0.93)
  sim_results<-data.table(SimID=rep(seq_len(n_sims),each=n_players),
                          Player=rep(player_names,times=n_sims),
                          DKScore=as.vector(dk_mat),FDScore=as.vector(fd_mat))
  for(s in share_stats) sim_results[[s]]<-as.integer(as.vector(stat_mats[[s]]))
  sim_results[["tpm"]] <-as.integer(as.vector(tpm_mat))
  sim_results[["pts"]] <-as.integer(as.vector(pts_mat))
  sim_results[["twom"]]<-as.integer(as.vector(pmax(stat_mats[["fgm"]]-tpm_mat,0L)))
  
  # ── Metadata ──
  cb("Building metadata...",0.96)
  keep_cols<-intersect(c("Name","DKID","FDID","DKSalary","FDSalary","DKPos","FDPos",
                         "DKOwn","FDOwn","Team","GameKey","SimKey","GameTime","GameRank",
                         "OverUnder","HomeSpread","DKProj","FDProj","Mins"),names(player_list))
  metadata<-unique(player_list[,..keep_cols],by="Name"); setnames(metadata,"Name","Player")
  metadata[,GameTimeSort:=as.numeric(as.POSIXct(paste(Sys.Date(),GameTime),
                                                format="%Y-%m-%d %I:%M %p",tz="America/New_York"))]
  
  if(length(input_data$sd_ids)){
    game_sd_lu<-unique(rbind(input_data$games[,.(Team=HomeTeam,ShowdownFile)],
                             input_data$games[,.(Team=AwayTeam,ShowdownFile)]))
    metadata<-merge(metadata,game_sd_lu,by="Team",all.x=TRUE)
    sd_all<-rbindlist(lapply(names(input_data$sd_ids),function(n){dt<-copy(input_data$sd_ids[[n]]);dt[,SDFile:=n];dt}),fill=TRUE)
    setnames(sd_all,"Name","Player"); sd_all[,Player:=trimws(iconv(Player,to="ASCII//TRANSLIT"))]
    sd_sub<-sd_all[,.(Player,Team,SDFile,CPTID=CPT_ID,CPTSalary=as.numeric(CPT_Salary),
                      SDID=as.character(UTIL_ID),SDSalary=as.numeric(UTIL_Salary))]
    if(!"ShowdownFile" %in% names(metadata)) metadata[,ShowdownFile:=NA_character_]
    missing_sf<-is.na(metadata$ShowdownFile)
    if(any(missing_sf)){lu_vec<-setNames(game_sd_lu$ShowdownFile,game_sd_lu$Team);metadata[missing_sf,ShowdownFile:=lu_vec[Team]]}
    metadata<-merge(metadata,sd_sub,by.x=c("Player","Team","ShowdownFile"),by.y=c("Player","Team","SDFile"),all.x=TRUE)
  }
  sim_results<-sim_results[Player %in% metadata$Player]
  has_fd<-"FDSalary" %in% names(metadata)&&any(!is.na(metadata$FDSalary)&metadata$FDSalary>0)
  has_sd<-"CPTSalary" %in% names(metadata)&&any(!is.na(metadata$CPTSalary)&metadata$CPTSalary>0)
  elapsed<-round((proc.time()-start_time)["elapsed"],1)
  cat(sprintf("  NBA sim complete: %d sims | %d players | %.1fs\n",n_sims,nrow(metadata),elapsed))
  
  # ── Visuals ──
  cb("Building visuals...",0.98)
  teams<-sort(unique(metadata$Team)); twom_v<-pmax(stat_mats[["fgm"]]-tpm_mat,0L)
  pos_lu<-unique(player_list[,.(Player=Name,
                                DKPos=if("DKPos" %in% names(player_list)) DKPos else NA_character_,
                                FDPos=if("FDPos" %in% names(player_list)) FDPos else NA_character_)])
  player_means<-data.table(Player=player_names,Team=player_teams,
                           DKAvgFP=round(rowMeans(dk_mat),1),FDAvgFP=round(rowMeans(fd_mat),1),AvgFP=round(rowMeans(dk_mat),1),
                           pts=round(rowMeans(pts_mat),1),tpm=round(rowMeans(tpm_mat),1),twom=round(rowMeans(twom_v),1),
                           ftm=round(rowMeans(stat_mats[["ftm"]]),1),reb=round(rowMeans(stat_mats[["reb"]]),1),
                           ast=round(rowMeans(stat_mats[["ast"]]),1),stl=round(rowMeans(stat_mats[["stl"]]),1),
                           blk=round(rowMeans(stat_mats[["blk"]]),1),to=round(rowMeans(stat_mats[["to"]]),1))
  player_means<-merge(player_means,pos_lu,by="Player",all.x=TRUE)
  setorder(player_means,Team,-DKAvgFP)
  team_means<-rbindlist(lapply(teams,function(tm){
    idx<-which(player_teams==tm)
    data.table(Team=tm,
               DKAvgFP=round(mean(colSums(dk_mat[idx,,drop=FALSE])),1),
               FDAvgFP=round(mean(colSums(fd_mat[idx,,drop=FALSE])),1),
               AvgFP  =round(mean(colSums(dk_mat[idx,,drop=FALSE])),1),
               pts=round(mean(colSums(pts_mat[idx,,drop=FALSE])),1),
               tpm=round(mean(colSums(tpm_mat[idx,,drop=FALSE])),1),
               twom=round(mean(colSums(twom_v[idx,,drop=FALSE])),1),
               ftm=round(mean(colSums(stat_mats[["ftm"]][idx,,drop=FALSE])),1),
               reb=round(mean(colSums(stat_mats[["reb"]][idx,,drop=FALSE])),1),
               ast=round(mean(colSums(stat_mats[["ast"]][idx,,drop=FALSE])),1),
               stl=round(mean(colSums(stat_mats[["stl"]][idx,,drop=FALSE])),1),
               blk=round(mean(colSums(stat_mats[["blk"]][idx,,drop=FALSE])),1),
               to =round(mean(colSums(stat_mats[["to"]] [idx,,drop=FALSE])),1))
  })); setorder(team_means,-DKAvgFP)
  sport_visuals<-list(teams=teams,player_means=player_means,team_means=team_means)
  list(sim_results=sim_results,metadata=metadata,has_fd=has_fd,has_sd=has_sd,sport_visuals=sport_visuals)
}

# ============================================================================
# NBA SLOT ASSIGNMENT
# DK: PG/SG/SF/PF/C/G/F/UTIL   FD: PG/PG/SG/SG/SF/SF/PF/PF/C
# ============================================================================

assign_nba_slots_dk <- function(cm) {
  setorder(cm, game_rank, Player)
  slots <- list(PG=NA_character_,SG=NA_character_,SF=NA_character_,PF=NA_character_,
                C=NA_character_,G=NA_character_,F=NA_character_,UTIL=NA_character_)
  fill_slot <- function(player, pos) {
    cands <- character(0)
    if (grepl("PG",pos))        cands <- c(cands,"PG")
    if (grepl("SG",pos))        cands <- c(cands,"SG")
    if (grepl("SF",pos))        cands <- c(cands,"SF")
    if (grepl("PF",pos))        cands <- c(cands,"PF")
    if (grepl("^C$|C/|/C",pos)) cands <- c(cands,"C")
    if (grepl("PG|SG",pos))     cands <- c(cands,"G")
    if (grepl("SF|PF",pos))     cands <- c(cands,"F")
    cands <- c(unique(cands),"UTIL")
    for (sl in cands) if (sl %in% names(slots)&&is.na(slots[[sl]])){slots[[sl]]<<-player;return(TRUE)}
    FALSE
  }
  for (idx in seq_len(nrow(cm)))
    if (!fill_slot(cm$Player[idx],cm$DKPos[idx])) return(NULL)
  if (any(sapply(slots,is.na))) return(NULL)
  slots
}

assign_nba_slots_fd <- function(cm) {
  setorder(cm, game_rank, Player)
  slots <- list(PG1=NA_character_,PG2=NA_character_,SG1=NA_character_,SG2=NA_character_,
                SF1=NA_character_,SF2=NA_character_,PF1=NA_character_,PF2=NA_character_,C=NA_character_)
  fill_slot <- function(player, pos) {
    cands <- character(0)
    if (grepl("PG",pos))        cands <- c(cands,"PG1","PG2")
    if (grepl("SG",pos))        cands <- c(cands,"SG1","SG2")
    if (grepl("SF",pos))        cands <- c(cands,"SF1","SF2")
    if (grepl("PF",pos))        cands <- c(cands,"PF1","PF2")
    if (grepl("^C$|C/|/C",pos)) cands <- c(cands,"C")
    cands <- unique(cands)
    for (sl in cands) if (sl %in% names(slots)&&is.na(slots[[sl]])){slots[[sl]]<<-player;return(TRUE)}
    FALSE
  }
  for (idx in seq_len(nrow(cm)))
    if (!fill_slot(cm$Player[idx],cm$FDPos[idx])) return(NULL)
  if (any(sapply(slots,is.na))) return(NULL)
  slots
}


# ============================================================================
# NBA DK CLASSIC OPTIMIZER
# 8 players: PG/SG/SF/PF/C/G/F/UTIL  $50K  >=2G >=2F >=1C  <=7 from one game
# ============================================================================

find_optimal_lineups_nba <- function(sim_results, metadata, config, verbose=TRUE) {
  if (verbose) cat("\nPhase 1: NBA DK lineups (per-sim LP)...\n")
  setDT(sim_results); setDT(metadata)
  salary_cap  <- config$salary_cap
  max_lineups <- if (!is.null(config$max_lineups)) config$max_lineups else 5000L
  meta <- unique(metadata[,.(Player,DKSalary,DKPos,GameKey)],by="Player")
  meta[,g_elig:=grepl("PG|SG",DKPos)][,f_elig:=grepl("SF|PF",DKPos)]
  meta[,c_elig:=grepl("^C$|C/|/C",DKPos)]
  if ("GameRank" %in% names(metadata)){
    meta<-merge(meta,unique(metadata[,.(Player,GameRank)]),by="Player",all.x=TRUE)
    meta[,game_rank:=GameRank][is.na(game_rank),game_rank:=1L][,GameRank:=NULL]
  } else meta[,game_rank:=1L]
  if (!"GameKey" %in% names(meta)) meta[,GameKey:="G1"]
  meta[is.na(GameKey),GameKey:="G1"]
  opt_data<-merge(sim_results[,.(SimID,Player,FantasyPoints=DKScore)],
                  meta[,.(Player,Salary=DKSalary,g_elig,f_elig,c_elig,game_rank,GameKey)],by="Player")
  opt_data<-opt_data[Salary>0&!is.na(Salary)&!is.na(FantasyPoints)]
  setkey(opt_data,SimID)
  sim_ids<-unique(opt_data$SimID); n_sims<-length(sim_ids)
  start_t<-Sys.time(); prog_freq<-max(1L,n_sims%/%20L)
  if (verbose) cat(sprintf("  %d players | %s sims | $%s cap\n",nrow(meta),
                           format(n_sims,big.mark=","),format(salary_cap,big.mark=",")))
  lineup_list<-vector("list",n_sims)
  for (i in seq_along(sim_ids)){
    sid<-sim_ids[i]; pool<-opt_data[.(sid)]; n_p<-nrow(pool); if(n_p<8L) next
    gkp<-unique(pool$GameKey)
    gc<-if(length(gkp)>=2L) lapply(gkp,function(gk) as.integer(pool$GameKey==gk)) else list()
    n_gc<-length(gc)
    f_con<-rbind(rep(1L,n_p),pool$Salary,as.integer(pool$g_elig),as.integer(pool$f_elig),
                 as.integer(pool$c_elig),if(n_gc) do.call(rbind,gc) else matrix(nrow=0,ncol=n_p))
    sol<-tryCatch(lp("max",pool$FantasyPoints,f_con,c("==","<=",">=",">=",">=",rep("<=",n_gc)),
                     c(8L,salary_cap,2L,2L,1L,rep(7L,n_gc)),all.bin=TRUE)$solution,error=function(e)NULL)
    if (is.null(sol)||sum(sol)<8L) next
    chosen<-pool[sol==1]; sig<-paste(sort(chosen$Player),collapse="|")
    lineup_list[[i]]<-data.table(Lineup=sig,TotalSalary=sum(chosen$Salary),TotalScore=sum(chosen$FantasyPoints))
    if(verbose&&i%%prog_freq==0L){cat(sprintf("\r  Phase 1: %d%% | %.1fs",round(i/n_sims*100),
                                              as.numeric(difftime(Sys.time(),start_t,units="secs"))));flush.console()}
  }
  if(verbose) cat("\n")
  valid<-lineup_list[!sapply(lineup_list,is.null)]; if(!length(valid)) stop("No valid NBA DK lineups")
  all_dt<-rbindlist(valid)
  counts<-all_dt[,.(Top1Count=.N,TotalSalary=TotalSalary[1],AvgScore=mean(TotalScore)),by=Lineup]
  counts[,rand:=runif(.N)]; setorder(counts,-Top1Count,rand); counts[,rand:=NULL]
  slot_list<-vector("list",nrow(counts))
  for(li in seq_len(nrow(counts))){
    players<-strsplit(counts$Lineup[li],"\\|")[[1]]
    cm_cols<-intersect(c("Player","DKPos","game_rank"),names(meta))
    cm<-meta[Player %in% players,..cm_cols]; slots<-assign_nba_slots_dk(cm)
    if(!is.null(slots)) slot_list[[li]]<-as.data.table(c(list(Lineup=counts$Lineup[li]),slots))
  }
  slot_dt<-rbindlist(slot_list[!sapply(slot_list,is.null)])
  counts<-merge(counts,slot_dt,by="Lineup",all.x=TRUE)
  unique_lineups<-counts[!is.na(PG),.(TotalSalary,Top1Count,AvgScore,
                                      Player1=PG,Player2=SG,Player3=SF,Player4=PF,Player5=C,Player6=G,Player7=F,Player8=UTIL)]
  if(nrow(unique_lineups)>max_lineups) unique_lineups<-unique_lineups[1:max_lineups]
  elapsed<-as.numeric(difftime(Sys.time(),start_t,units="secs"))
  if(verbose) cat(sprintf("  \u2713 %s DK lineups | %.1fs\n",format(nrow(unique_lineups),big.mark=","),elapsed))
  list(unique_lineups=unique_lineups,n_sims=n_sims,config=config,mode="nba_dk")
}


# ============================================================================
# NBA FD CLASSIC OPTIMIZER
# 9 players: PG/PG/SG/SG/SF/SF/PF/PF/C  $60K  >=2 each pos >=1C
# ============================================================================

find_optimal_lineups_nba_fd <- function(sim_results, metadata, config, verbose=TRUE) {
  if(verbose) cat("\nPhase 1: NBA FD lineups (per-sim LP)...\n")
  setDT(sim_results); setDT(metadata)
  salary_cap<-config$salary_cap; max_lineups<-if(!is.null(config$max_lineups)) config$max_lineups else 5000L
  meta<-unique(metadata[!is.na(FDSalary)&FDSalary>0,.(Player,FDSalary,FDPos,GameKey)],by="Player")
  if("GameRank" %in% names(metadata)){
    meta<-merge(meta,unique(metadata[,.(Player,GameRank)]),by="Player",all.x=TRUE)
    meta[,game_rank:=GameRank][is.na(game_rank),game_rank:=1L][,GameRank:=NULL]
  } else meta[,game_rank:=1L]
  meta[,pg_elig:=grepl("PG",FDPos)][,sg_elig:=grepl("SG",FDPos)]
  meta[,sf_elig:=grepl("SF",FDPos)][,pf_elig:=grepl("PF",FDPos)]
  meta[,c_elig:=grepl("^C$|C/|/C",FDPos)]
  opt_data<-merge(sim_results[,.(SimID,Player,FantasyPoints=FDScore)],
                  meta[,.(Player,Salary=FDSalary,pg_elig,sg_elig,sf_elig,pf_elig,c_elig,game_rank,FDPos,GameKey)],by="Player")
  opt_data<-opt_data[Salary>0&!is.na(Salary)&!is.na(FantasyPoints)]
  setkey(opt_data,SimID)
  sim_ids<-unique(opt_data$SimID); n_sims<-length(sim_ids)
  start_t<-Sys.time(); prog_freq<-max(1L,n_sims%/%20L); lineup_list<-vector("list",n_sims)
  for(i in seq_along(sim_ids)){
    sid<-sim_ids[i]; pool<-opt_data[.(sid)]; if(nrow(pool)<9L) next
    f_con<-rbind(rep(1L,nrow(pool)),pool$Salary,as.integer(pool$pg_elig),as.integer(pool$sg_elig),
                 as.integer(pool$sf_elig),as.integer(pool$pf_elig),as.integer(pool$c_elig))
    sol<-tryCatch(lp("max",pool$FantasyPoints,f_con,c("==","<=",">=",">=",">=",">=",">="),
                     c(9L,salary_cap,2L,2L,2L,2L,1L),all.bin=TRUE)$solution,error=function(e)NULL)
    if(is.null(sol)||sum(sol)<9L) next
    chosen<-pool[sol==1]; sig<-paste(sort(chosen$Player),collapse="|")
    lineup_list[[i]]<-data.table(Lineup=sig,TotalSalary=sum(chosen$Salary),TotalScore=sum(chosen$FantasyPoints))
    if(verbose&&i%%prog_freq==0L){cat(sprintf("\r  Phase 1: %d%% | %.1fs",round(i/n_sims*100),
                                              as.numeric(difftime(Sys.time(),start_t,units="secs"))));flush.console()}
  }
  if(verbose) cat("\n")
  valid<-lineup_list[!sapply(lineup_list,is.null)]; if(!length(valid)) stop("No valid NBA FD lineups")
  all_dt<-rbindlist(valid)
  counts<-all_dt[,.(Top1Count=.N,TotalSalary=TotalSalary[1],AvgScore=mean(TotalScore)),by=Lineup]
  counts[,rand:=runif(.N)]; setorder(counts,-Top1Count,rand); counts[,rand:=NULL]
  slot_list<-vector("list",nrow(counts))
  for(li in seq_len(nrow(counts))){
    players<-strsplit(counts$Lineup[li],"\\|")[[1]]
    cm_cols<-intersect(c("Player","FDPos","game_rank"),names(meta))
    cm<-meta[Player %in% players,..cm_cols]; slots<-assign_nba_slots_fd(cm)
    if(!is.null(slots)) slot_list[[li]]<-as.data.table(c(list(Lineup=counts$Lineup[li]),slots))
  }
  slot_dt<-rbindlist(slot_list[!sapply(slot_list,is.null)])
  counts<-merge(counts,slot_dt,by="Lineup",all.x=TRUE)
  unique_lineups<-counts[!is.na(PG1),.(TotalSalary,Top1Count,AvgScore,
                                       Player1=PG1,Player2=PG2,Player3=SG1,Player4=SG2,
                                       Player5=SF1,Player6=SF2,Player7=PF1,Player8=PF2,Player9=C)]
  if(nrow(unique_lineups)>max_lineups) unique_lineups<-unique_lineups[1:max_lineups]
  elapsed<-as.numeric(difftime(Sys.time(),start_t,units="secs"))
  if(verbose) cat(sprintf("  \u2713 %s FD lineups | %.1fs\n",format(nrow(unique_lineups),big.mark=","),elapsed))
  list(unique_lineups=unique_lineups,n_sims=n_sims,config=config,mode="nba_fd")
}


# ============================================================================
# NBA SHOWDOWN OPTIMIZER
# CPT x1.5 + 5 FLEX  $50K  both teams required
# ============================================================================

find_optimal_lineups_nba_sd <- function(sim_results, metadata, config, verbose=TRUE) {
  if(verbose) cat("\nPhase 1: NBA Showdown lineups (per-sim greedy)...\n")
  setDT(sim_results); setDT(metadata)
  salary_cap<-config$salary_cap; max_lineups<-if(!is.null(config$max_lineups)) config$max_lineups else 5000L
  cpt_mult<-1.5
  meta<-unique(metadata[!is.na(CPTSalary)&CPTSalary>0&!is.na(SDSalary)&SDSalary>0,
                        .(Player,Team,CPTSalary,SDSalary,GameKey)],by="Player")
  if(!nrow(meta)) stop("No SD-eligible players.")
  if(length(unique(meta$Team))<2) warning("NBA SD: fewer than 2 teams.")
  opt_data<-merge(sim_results[,.(SimID,Player,DKScore)],meta[,.(Player,Team,CPTSalary,SDSalary)],by="Player")
  opt_data<-opt_data[!is.na(DKScore)]; setkey(opt_data,SimID)
  sim_ids<-unique(opt_data$SimID); n_sims<-length(sim_ids)
  start_t<-Sys.time(); prog_freq<-max(1L,n_sims%/%20L); lineup_list<-vector("list",n_sims)
  for(i in seq_along(sim_ids)){
    sid<-sim_ids[i]; sd<-opt_data[.(sid)]; setorder(sd,-DKScore)
    best_score<- -Inf; best_lineup<-NULL
    for(ci in seq_len(nrow(sd))){
      cpt_player<-sd$Player[ci]; cpt_sal<-sd$CPTSalary[ci]
      cpt_score<-sd$DKScore[ci]*cpt_mult; if(cpt_sal>salary_cap) next
      rem_cap<-salary_cap-cpt_sal; flex<-sd[Player!=cpt_player]; setorder(flex,-DKScore)
      picked_f<-character(5L); n_picked<-0L; sal_used<-0; flex_score<-0
      for(j in seq_len(nrow(flex))){
        if(n_picked==5L) break
        if(sal_used+flex$SDSalary[j]<=rem_cap){
          n_picked<-n_picked+1L; picked_f[n_picked]<-flex$Player[j]
          sal_used<-sal_used+flex$SDSalary[j]; flex_score<-flex_score+flex$DKScore[j]
        }
      }
      if(n_picked==5L){
        all_p<-c(cpt_player,picked_f[1:5]); lteams<-sd$Team[match(all_p,sd$Player)]
        if(length(unique(lteams))<2L) next
        total<-cpt_score+flex_score
        if(total>best_score){best_score<-total
        best_lineup<-list(Captain=cpt_player,Flex=sort(picked_f),
                          TotalSalary=cpt_sal+sal_used,TotalScore=total)}
      }
    }
    if(!is.null(best_lineup)){
      lineup_list[[i]]<-data.table(
        Lineup=paste(c(best_lineup$Captain,best_lineup$Flex),collapse="|"),
        TotalSalary=best_lineup$TotalSalary,TotalScore=best_lineup$TotalScore,
        Captain=best_lineup$Captain,Util1=best_lineup$Flex[1],Util2=best_lineup$Flex[2],
        Util3=best_lineup$Flex[3],Util4=best_lineup$Flex[4],Util5=best_lineup$Flex[5])
    }
    if(verbose&&i%%prog_freq==0L){cat(sprintf("\r  Phase 1: %d%% | %.1fs",round(i/n_sims*100),
                                              as.numeric(difftime(Sys.time(),start_t,units="secs"))));flush.console()}
  }
  if(verbose) cat("\n")
  valid<-lineup_list[!sapply(lineup_list,is.null)]; if(!length(valid)) stop("No valid NBA SD lineups")
  all_dt<-rbindlist(valid)
  counts<-all_dt[,.(Top1Count=.N,TotalSalary=TotalSalary[1],AvgScore=mean(TotalScore),
                    Captain=Captain[1],Util1=Util1[1],Util2=Util2[1],Util3=Util3[1],
                    Util4=Util4[1],Util5=Util5[1]),by=Lineup]
  counts[,rand:=runif(.N)]; setorder(counts,-Top1Count,rand); counts[,rand:=NULL]
  if(nrow(counts)>max_lineups) counts<-counts[1:max_lineups]
  unique_lineups<-counts[,.(TotalSalary,Top1Count,AvgScore,Captain,Util1,Util2,Util3,Util4,Util5)]
  elapsed<-as.numeric(difftime(Sys.time(),start_t,units="secs"))
  if(verbose) cat(sprintf("  \u2713 %s SD lineups | %.1fs\n",format(nrow(unique_lineups),big.mark=","),elapsed))
  list(unique_lineups=unique_lineups,n_sims=n_sims,config=config,mode="captain")
}


# ============================================================================
# LINEUP METRICS PLACEHOLDER
# ============================================================================

calculate_nba_lineup_metrics <- function(scored_lineups, sim_results, metadata) {
  scored_lineups
}