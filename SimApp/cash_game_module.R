# ============================================================================
# CONTEST MODULE — Multi-Tier Cash / Multiplier Simulator
# Golden Ticket Sims
#
# Simulates Double Up, 3x, 5x and 10x contests off a single sim + single
# scoring pass.  Field is built once at the widest settings and each contest
# tier derives its field as a subset.
#
# Sport-agnostic: reads roster size and salary cap from rv$config.
# Works for any sport that has DK/FD/SD optimal lineups + Own in metadata.
# ============================================================================


# ============================================================================
# HELPERS
# ============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Detect player slot columns from a lineup data.table.
get_player_cols <- function(lineup_dt) {
  pc <- grep("^Player[0-9]+$", names(lineup_dt), value = TRUE)
  if (length(pc) > 0) return(pc)
  pc <- grep("^Captain$|^Util[0-9]+$", names(lineup_dt), value = TRUE)
  if (length(pc) > 0) return(pc)
  stop("Cannot detect player columns in lineup data.")
}

#' Get DK salary cap and roster size from config with safe fallbacks.
get_dk_constraints <- function(config) {
  salary_cap  <- config$salary_caps$DK  %||% 50000
  roster_size <- config$roster_sizes$DK %||% 6L
  list(salary_cap = salary_cap, roster_size = as.integer(roster_size))
}

get_cash_params <- function(config, platform = "DK") {
  sport <- config$sport_name %||% ""
  if (sport == "NBA") {
    list(n_field = 100L, n_yours = 10L, top_n_ppd = 15L,
         sal_floor = 49000, total_lineups = 110L)
  } else if (sport == "MMA") {
    list(n_field = 100L, n_yours = 10L, top_n_ppd = 20L,
         sal_floor = 49000, total_lineups = 110L)
  } else {
    list(n_field = 500L, n_yours = 50L, top_n_ppd = 20L,
         sal_floor = 49000, total_lineups = 550L)
  }
}

# NBA DK slot names for display (Player1..Player8 -> PG/SG/SF/PF/C/G/F/UTIL)
get_slot_labels <- function(config, n_slots) {
  sport <- config$sport_name %||% ""
  if (sport == "NBA") {
    switch(as.character(n_slots),
           "8" = c("PG","SG","SF","PF","C","G","F","UTIL"),
           "9" = c("PG","PG2","SG","SG2","SF","SF2","PF","PF2","C"),
           "6" = c("CPT","UTIL1","UTIL2","UTIL3","UTIL4","UTIL5"),
           paste0("P", seq_len(n_slots))
    )
  } else NULL  # NULL = keep Player1..N
}


# ============================================================================
# CONTEST TYPE SPECIFICATIONS
#
# cash_pct   fraction of the weighted field that cashes
# mult       payout multiplier on entry fee
# n_field    how many field lineups this tier uses
# n_core     how many greedy-chalk lineups seed the field
# top_n_ppd  pool cut for the combn fill portion
# field_style "chalk" (AvgOwn sort) | "mixed" (half own / half proj) |
#             "wide" (ownership^alpha weighted sample)
# max_weight / min_weight  exponential entry-duplication decay.  Flattens as
#             the multiplier rises — a 10x field has far less duplication.
# ============================================================================

CONTEST_TYPES <- list(
  double_up = list(key = "double_up", label = "Double Up", short = "2x",
                   cash_pct = 0.45, mult = 2.0,
                   n_field = 100L, n_core = 50L, top_n_ppd = 25L,
                   field_style = "chalk", max_weight = 20L, min_weight = 1L),
  
  triple_up = list(key = "triple_up", label = "Triple Up", short = "3x",
                   cash_pct = 0.30, mult = 3.0,
                   n_field = 150L, n_core = 40L, top_n_ppd = 28L,
                   field_style = "chalk", max_weight = 12L, min_weight = 1L),
  
  five_x    = list(key = "five_x", label = "5x Multiplier", short = "5x",
                   cash_pct = 0.20, mult = 5.0,
                   n_field = 250L, n_core = 25L, top_n_ppd = 32L,
                   field_style = "mixed", max_weight = 6L, min_weight = 1L),
  
  ten_x     = list(key = "ten_x", label = "10x Multiplier", short = "10x",
                   cash_pct = 0.10, mult = 10.0,
                   n_field = 500L, n_core = 15L, top_n_ppd = 40L,
                   field_style = "wide", max_weight = 3L, min_weight = 1L)
)

#' Scale contest field sizes down for small slates (MMA/NBA use smaller pools).
scale_contest_specs <- function(config) {
  cash_p <- get_cash_params(config)
  sport  <- config$sport_name %||% ""
  # Ratio of this sport's default field to the 500-lineup baseline
  ratio  <- max(0.2, cash_p$n_field / 500)
  lapply(CONTEST_TYPES, function(s) {
    s$n_field <- max(40L, as.integer(round(s$n_field * ratio)))
    s$n_core  <- max(10L, as.integer(round(s$n_core  * ratio)))
    s
  })
}


# ============================================================================
# COMBINATION MATRIX — capped exhaustive, sampled above the cap
# ============================================================================

MAX_COMBOS <- 3e6

#' Build a combination index matrix, sampling when exhaustive is too large.
build_combo_matrix <- function(n_pool, roster_size, seed = 42L) {
  total <- choose(n_pool, roster_size)
  
  if (is.finite(total) && total <= MAX_COMBOS) {
    cat(sprintf("  [Field] Exhaustive combos: %s\n", format(total, big.mark = ",")))
    return(do.call(rbind, combn(n_pool, roster_size, simplify = FALSE)))
  }
  
  set.seed(seed)
  n_draw <- as.integer(MAX_COMBOS)
  cat(sprintf("  [Field] Sampling %s of %s combos\n",
              format(n_draw, big.mark = ","),
              format(total, big.mark = ", ", scientific = FALSE)))
  
  # Vectorized: oversample rows of independent draws, drop rows with repeats,
  # sort within row, dedup.  ~10-20x faster than a per-row apply/loop.
  draw_block <- function(nr) {
    mm <- matrix(sample.int(n_pool, nr * roster_size, replace = TRUE),
                 nrow = nr, ncol = roster_size)
    mm <- t(apply(mm, 1L, sort))
    # keep only rows with roster_size distinct players
    keep <- apply(mm, 1L, function(r) !any(diff(r) == 0L))
    mm[keep, , drop = FALSE]
  }
  
  acc  <- draw_block(ceiling(n_draw * 1.4))
  # top up if rejection thinned us below target
  tries <- 0L
  while (nrow(unique(acc)) < n_draw && tries < 6L) {
    acc   <- rbind(acc, draw_block(ceiling(n_draw * 0.5)))
    tries <- tries + 1L
  }
  acc <- unique(acc)
  if (nrow(acc) > n_draw) acc <- acc[seq_len(n_draw), , drop = FALSE]
  acc
}


# ============================================================================
# STAGE 1: GREEDY CHALK CORE
#
# The lineups the field actually mass-enters: highest raw ownership players,
# salary-repaired into legality.  Runs on the FULL pool at zero combinatorial
# cost, so top_n_ppd only governs the fill stage.
# ============================================================================

#' Greedy chalk-core lineup construction from raw ownership ranking.
#'
#' @param pool         data.table with Player, DKSalary, DKOwn (own-sorted internally)
#' @param roster_size  integer
#' @param salary_cap   numeric
#' @param salary_floor numeric
#' @param n_core       integer, target number of core lineups
#' @return integer matrix, n_core x roster_size of pool row indices
build_chalk_core <- function(pool, roster_size, salary_cap, salary_floor,
                             n_core = 50L) {
  
  p <- copy(pool)
  setorder(p, -DKOwn)
  sal <- p$DKSalary
  n_p <- nrow(p)
  
  if (n_p < roster_size) return(matrix(integer(0), nrow = 0, ncol = roster_size))
  
  cores <- vector("list", n_core)
  k     <- 0L
  seen  <- character(0)
  
  max_start <- min(n_core * 2L, n_p - roster_size + 1L)
  
  for (start in seq_len(max_start)) {
    if (k >= n_core) break
    
    idx  <- start:(start + roster_size - 1L)
    tot  <- sum(sal[idx])
    tries <- 0L
    
    # Repair salary: swap toward legality, always preferring the chalkiest
    # available candidate (pool is ownership-sorted, so lowest index = chalkiest)
    while ((tot > salary_cap || tot < salary_floor) && tries < 40L) {
      if (tot > salary_cap) {
        drop <- idx[which.max(sal[idx])]
        cand <- setdiff(which(sal < sal[drop]), idx)
      } else {
        drop <- idx[which.min(sal[idx])]
        cand <- setdiff(which(sal > sal[drop]), idx)
      }
      if (length(cand) == 0L) break
      idx   <- c(setdiff(idx, drop), cand[1L])
      tot   <- sum(sal[idx])
      tries <- tries + 1L
    }
    
    if (tot >= salary_floor && tot <= salary_cap) {
      s_idx <- sort(idx)
      sig   <- paste(s_idx, collapse = "-")
      if (!sig %in% seen) {
        seen  <- c(seen, sig)
        k     <- k + 1L
        cores[[k]] <- s_idx
      }
    }
  }
  
  if (k == 0L) return(matrix(integer(0), nrow = 0, ncol = roster_size))
  
  # Map back to ORIGINAL pool row order (p was re-sorted by DKOwn)
  orig_idx <- match(p$Player, pool$Player)
  m <- do.call(rbind, cores[seq_len(k)])
  m <- matrix(orig_idx[m], nrow = nrow(m))
  t(apply(m, 1L, sort))
}


# ============================================================================
# FIELD LINEUP GENERATION — two stage
#
#   Stage 1: greedy chalk core from full pool (build_chalk_core)
#   Stage 2: fill remainder from capped/sampled combos on PPD-cut pool,
#            selected by field_style
#
# This replaces the old pure-AvgOwn sort.  Geometric-mean ownership favours
# lineups where every player is moderately owned over lineups with a real
# 60% chalk anchor plus mid-tier support — the field mass-enters the latter.
# ============================================================================

#' Clean and prepare the player pool.
prep_pool <- function(metadata, own_col = "DKOwn", sal_col = "DKSalary") {
  d <- copy(as.data.table(metadata))
  missing <- setdiff(c("Player", sal_col, own_col), names(d))
  if (length(missing) > 0) stop("metadata missing: ", paste(missing, collapse = ", "))
  
  if (sal_col != "DKSalary") setnames(d, sal_col, "DKSalary")
  if (own_col != "DKOwn")    setnames(d, own_col, "DKOwn")
  
  d <- d[!is.na(DKSalary) & DKSalary > 0 & !is.na(DKOwn) & DKOwn > 0]
  d <- unique(d, by = "Player")
  
  if (nrow(d) == 0L) stop("No players with valid salary and ownership.")
  if (max(d$DKOwn, na.rm = TRUE) <= 1) d[, DKOwn := DKOwn * 100]
  
  d[, PPD := DKOwn / (DKSalary / 1000)]
  d[]
}

#' Assemble the final field data.table from a combo index matrix.
finalize_field <- function(combos, pool, roster_size, id_prefix = "F") {
  if (nrow(combos) == 0L) stop("Field generation produced no lineups.")
  
  player_names <- pool$Player
  sal_v <- pool$DKSalary
  own_v <- pool$DKOwn
  
  sal_mat <- matrix(sal_v[combos], nrow = nrow(combos))
  own_mat <- matrix(own_v[combos], nrow = nrow(combos))
  own_mat[own_mat <= 0] <- NA_real_
  
  tot_sal <- rowSums(sal_mat)
  avg_own <- exp(rowMeans(log(own_mat), na.rm = TRUE))
  
  player_cols <- paste0("Player", seq_len(roster_size))
  dt <- as.data.table(matrix(player_names[combos], nrow = nrow(combos)))
  setnames(dt, player_cols)
  dt[, TotalSalary := tot_sal]
  dt[, AvgOwn      := round(avg_own, 2)]
  
  setorder(dt, -AvgOwn)
  dt[, LineupID := paste0(id_prefix, seq_len(.N))]
  setcolorder(dt, c("LineupID", player_cols, "TotalSalary", "AvgOwn"))
  dt[]
}

#' Generate field lineups: greedy chalk core + style-driven combo fill.
#'
#' @param metadata     data.table: Player, DKSalary, DKOwn
#' @param n            integer, total field lineups to return
#' @param salary_cap   numeric
#' @param salary_floor numeric
#' @param roster_size  integer
#' @param top_n_ppd    integer, pool cut for the FILL stage only
#' @param n_core       integer, greedy chalk-core lineups
#' @param field_style  "chalk" | "mixed" | "wide"
#' @param alpha        numeric, ownership exponent for "wide" sampling
#' @param proj_col     character, projection column for "mixed" style
#' @param seed         integer
#' @return data.table: LineupID, Player1..N, TotalSalary, AvgOwn
generate_field_lineups <- function(metadata,
                                   n            = 500L,
                                   salary_cap   = 50000,
                                   salary_floor = NULL,
                                   roster_size  = 6L,
                                   top_n_ppd    = 40L,
                                   n_core       = 50L,
                                   field_style  = "chalk",
                                   alpha        = 1.5,
                                   proj_col     = "DKProj",
                                   seed         = 42L,
                                   own_col      = "DKOwn",
                                   sal_col      = "DKSalary") {
  
  if (is.null(salary_floor)) salary_floor <- 49000
  
  pool <- prep_pool(metadata, own_col = own_col, sal_col = sal_col)
  
  if (nrow(pool) < roster_size)
    stop(sprintf("Only %d players in pool — need at least %d.", nrow(pool), roster_size))
  
  cat(sprintf("  [Field] Pool: %d players | salary $%s-$%s | own %.1f%%-%.1f%% | style=%s\n",
              nrow(pool),
              format(min(pool$DKSalary), big.mark = ","),
              format(max(pool$DKSalary), big.mark = ","),
              min(pool$DKOwn), max(pool$DKOwn), field_style))
  
  # ── Stage 1: greedy chalk core (full pool) ────────────────────────────────
  core_mat <- build_chalk_core(pool, roster_size, salary_cap, salary_floor,
                               n_core = min(n_core, n))
  cat(sprintf("  [Field] Chalk core: %d lineups\n", nrow(core_mat)))
  
  # ── Stage 2: fill remainder from combos ───────────────────────────────────
  n_fill <- n - nrow(core_mat)
  
  if (n_fill <= 0L) {
    combos <- core_mat[seq_len(min(n, nrow(core_mat))), , drop = FALSE]
    return(finalize_field(combos, pool, roster_size))
  }
  
  # Take the top-PPD slice WITHOUT reordering `pool`. setorder() sorts by
  # reference, and core_mat already holds row indices into the pool as it stands
  # here — re-sorting silently repointed every one of them at a different
  # player, so a core lineup that passed the salary check came back out as a
  # different combination entirely. That is how lineups landed over the cap.
  n_cut <- min(top_n_ppd, nrow(pool))
  map   <- order(-pool$PPD)[seq_len(n_cut)]   # sub index -> pool index

  cmat <- build_combo_matrix(n_cut, roster_size, seed = seed)
  cmat <- matrix(map[cmat], nrow = nrow(cmat))
  
  # Salary filter, relaxing the floor if nothing passes
  tot_sal <- rowSums(matrix(pool$DKSalary[cmat], nrow = nrow(cmat)))
  keep    <- which(tot_sal >= salary_floor & tot_sal <= salary_cap)
  
  if (length(keep) == 0L) {
    ef <- salary_floor
    while (length(keep) == 0L && ef > salary_cap * 0.80) {
      ef   <- ef - 500
      keep <- which(tot_sal >= ef & tot_sal <= salary_cap)
    }
    if (length(keep) > 0L)
      warning(sprintf("Salary floor relaxed to $%s for fill stage.",
                      format(ef, big.mark = ",")))
  }
  
  if (length(keep) == 0L) {
    cat("  [Field] Fill stage found no legal combos — returning chalk core only\n")
    return(finalize_field(core_mat, pool, roster_size))
  }
  
  cmat <- cmat[keep, , drop = FALSE]
  
  # Drop anything already in the core
  if (nrow(core_mat) > 0L) {
    core_sig <- apply(core_mat, 1L, paste, collapse = "-")
    fill_sig <- apply(cmat,     1L, paste, collapse = "-")
    cmat     <- cmat[!fill_sig %in% core_sig, , drop = FALSE]
  }
  
  if (nrow(cmat) == 0L) return(finalize_field(core_mat, pool, roster_size))
  
  own_mat <- matrix(pool$DKOwn[cmat], nrow = nrow(cmat))
  own_mat[own_mat <= 0] <- NA_real_
  aown    <- exp(rowMeans(log(own_mat), na.rm = TRUE))
  
  n_take <- min(n_fill, nrow(cmat))
  
  sel <- switch(
    field_style,
    
    "wide" = {
      set.seed(seed)
      w <- pmax(aown, 1e-6) ^ alpha
      sample.int(length(aown), n_take, prob = w, replace = FALSE)
    },
    
    "mixed" = {
      n_half  <- ceiling(n_take / 2)
      idx_own <- head(order(-aown), n_half)
      if (proj_col %in% names(pool)) {
        prj <- pool[[proj_col]]
        prj[is.na(prj)] <- 0
        tot_proj <- rowSums(matrix(prj[cmat], nrow = nrow(cmat)))
        idx_prj  <- head(setdiff(order(-tot_proj), idx_own), n_take - n_half)
      } else {
        # No projections available — fall back to salary-max as the sharp proxy
        tot_s   <- rowSums(matrix(pool$DKSalary[cmat], nrow = nrow(cmat)))
        idx_prj <- head(setdiff(order(-tot_s), idx_own), n_take - n_half)
      }
      c(idx_own, idx_prj)
    },
    
    # default "chalk"
    head(order(-aown), n_take)
  )
  
  combos <- rbind(core_mat, cmat[sel, , drop = FALSE])

  cat(sprintf("  [Field] %d core + %d fill = %d lineups\n",
              nrow(core_mat), length(sel), nrow(combos)))

  out <- finalize_field(combos, pool, roster_size)

  # Every lineup passed a cap test before it got here, so anything over the cap
  # now means the indices and the pool disagree — the failure that produced
  # $60,500 NASCAR lineups against a $50,000 cap. Drop them and say so rather
  # than handing the user a field that cannot be entered.
  bad <- which(out$TotalSalary > salary_cap)
  if (length(bad)) {
    warning(sprintf("Dropped %d generated lineup(s) over the $%s cap (max $%s).",
                    length(bad), format(salary_cap, big.mark = ","),
                    format(max(out$TotalSalary[bad]), big.mark = ",")))
    out <- out[-bad]
    if (nrow(out) == 0L) stop("Every generated lineup exceeded the salary cap.")
    out[, LineupID := paste0("F", seq_len(.N))]
  }
  out
}


# ============================================================================
# FIELD TIER DERIVATION
#
# One master build at the widest settings; each contest tier takes its
# n_core from the ownership-ranked head plus its fill from the remainder.
# Master is already AvgOwn-sorted, so all subsetting is free.
# ============================================================================

#' Derive a per-contest field from the master field.
derive_tier_field <- function(master, spec, seed = 42L) {
  n <- min(spec$n_field, nrow(master))
  if (n <= 0L) return(master[0])
  
  n_core <- min(spec$n_core, n, nrow(master))
  core   <- master[seq_len(n_core)]
  rest   <- master[!LineupID %in% core$LineupID]
  n_fill <- n - nrow(core)
  
  if (n_fill <= 0L || nrow(rest) == 0L) return(copy(core))
  
  fill <- switch(
    spec$field_style,
    
    "wide" = {
      set.seed(seed)
      w <- pmax(rest$AvgOwn, 1e-6) ^ 1.5
      rest[sample.int(nrow(rest), min(n_fill, nrow(rest)), prob = w, replace = FALSE)]
    },
    
    "mixed" = {
      n_half <- ceiling(n_fill / 2)
      a <- head(rest, min(n_half, nrow(rest)))
      b <- rest[!LineupID %in% a$LineupID]
      if (nrow(b) > 0L) {
        set.seed(seed)
        b <- b[sample.int(nrow(b), min(n_fill - nrow(a), nrow(b)), replace = FALSE)]
        rbind(a, b)
      } else a
    },
    
    # default "chalk"
    head(rest, min(n_fill, nrow(rest)))
  )
  
  out <- rbind(core, fill)
  setorder(out, -AvgOwn)
  out[]
}

#' Build the master field once, then derive every contest tier from it.
#'
#' @return named list of data.tables, one per contest key
build_field_tiers <- function(metadata, specs, salary_cap, salary_floor,
                              roster_size, seed = 42L,
                              proj_col = "DKProj",
                              own_col  = "DKOwn",
                              sal_col  = "DKSalary") {
  
  n_master   <- max(sapply(specs, `[[`, "n_field")) * 3L
  core_master<- max(sapply(specs, `[[`, "n_core"))
  ppd_master <- max(sapply(specs, `[[`, "top_n_ppd"))
  
  cat(sprintf("\n  [Field] Master build: n=%d, core=%d, top_n_ppd=%d\n",
              n_master, core_master, ppd_master))
  
  master <- generate_field_lineups(
    metadata     = metadata,
    n            = n_master,
    salary_cap   = salary_cap,
    salary_floor = salary_floor,
    roster_size  = roster_size,
    top_n_ppd    = ppd_master,
    n_core       = core_master,
    field_style  = "wide",
    seed         = seed,
    proj_col     = proj_col,
    own_col      = own_col,
    sal_col      = sal_col
  )
  setorder(master, -AvgOwn)
  master[, LineupID := paste0("F", seq_len(.N))]
  
  tiers <- lapply(specs, function(s) derive_tier_field(master, s, seed = seed))
  names(tiers) <- names(specs)
  
  for (k in names(tiers)) {
    cat(sprintf("  [Field] %-14s %4d lineups (core %d, style %s)\n",
                specs[[k]]$label, nrow(tiers[[k]]),
                specs[[k]]$n_core, specs[[k]]$field_style))
  }
  cat("\n")
  
  list(master = master, tiers = tiers)
}


# ============================================================================
# NBA FIELD LINEUP GENERATION — LP on projections
#
# For NBA the positional constraints make combn impractical, so the field is
# built by repeated LP with an "exclude at least 1 from the previous lineup"
# constraint.  A randomized objective (noise scaled by contest tier) widens
# the field for the higher multipliers.
#
# Requires: metadata with DKSalary, DKPos, DKProj (ETR projections).
# Calls assign_nba_slots_dk()/fd() from nba_engine.R for slot assignment.
# ============================================================================

#' Build NBA field lineups by running LP n times on (optionally noisy) projections.
#'
#' @param metadata   data.table: Player, DKSalary, DKPos, DKProj
#' @param n          integer, how many lineups to build
#' @param salary_cap numeric
#' @param platform   "DK" or "FD"
#' @param noise_sd   numeric, sd of gaussian noise added to the objective as a
#'                   fraction of mean projection.  0 = pure chalk-optimal.
#' @param seed       integer
#' @return data.table: LineupID, Player1..N, TotalSalary, AvgOwn
generate_field_lineups_nba <- function(metadata,
                                       n            = 100L,
                                       salary_cap   = 50000,
                                       platform     = "DK",
                                       noise_sd     = 0,
                                       seed         = 42L) {
  
  meta <- copy(as.data.table(metadata))
  
  sal_col  <- if (platform == "FD") "FDSalary" else "DKSalary"
  pos_col  <- if (platform == "FD") "FDPos"    else "DKPos"
  proj_col <- if (platform == "FD") "FDProj"   else "DKProj"
  own_col  <- if (platform == "FD") "FDOwn"    else "DKOwn"
  
  missing <- setdiff(c("Player", sal_col, pos_col, proj_col), names(meta))
  if (length(missing) > 0)
    stop("NBA field gen — metadata missing: ", paste(missing, collapse = ", "))
  
  setnames(meta, c(sal_col, proj_col), c("Sal", "Proj"))
  meta[, Pos := get(pos_col)]
  meta <- unique(meta[!is.na(Sal) & Sal > 0 & !is.na(Proj)], by = "Player")
  
  has_own <- own_col %in% names(meta)
  if (has_own) {
    ov <- meta[[own_col]]
    if (max(ov, na.rm = TRUE) <= 1) meta[, (own_col) := get(own_col) * 100]
  }
  
  if (platform == "FD") {
    n_roster <- 9L
    meta[, g_elig := as.integer(grepl("PG|SG",     Pos))]
    meta[, f_elig := as.integer(grepl("SF|PF",     Pos))]
    meta[, c_elig := as.integer(grepl("^C$|C/|/C", Pos))]
    con_mat <- rbind(rep(1L, nrow(meta)), meta$Sal, meta$g_elig, meta$f_elig, meta$c_elig)
    con_dir <- c("==", "<=", ">=", ">=", ">=")
    con_rhs <- c(9L, salary_cap, 4L, 4L, 1L)
  } else {
    n_roster <- 8L
    meta[, g_elig := as.integer(grepl("PG|SG",     Pos))]
    meta[, f_elig := as.integer(grepl("SF|PF",     Pos))]
    meta[, c_elig := as.integer(grepl("^C$|C/|/C", Pos))]
    con_mat <- rbind(rep(1L, nrow(meta)), meta$Sal, meta$g_elig, meta$f_elig, meta$c_elig)
    con_dir <- c("==", "<=", ">=", ">=", ">=")
    con_rhs <- c(8L, salary_cap, 2L, 2L, 1L)
  }
  
  n_p       <- nrow(meta)
  base_obj  <- meta$Proj
  noise_amt <- noise_sd * mean(base_obj, na.rm = TRUE)
  std_cols  <- paste0("Player", seq_len(n_roster))
  
  set.seed(seed)
  
  lineup_list   <- vector("list", n)
  excluded_rows <- list()
  used_sigs     <- character(0)
  successful    <- 0L
  
  for (iter in seq_len(n + 40L)) {
    if (successful >= n) break
    
    obj <- if (noise_amt > 0) base_obj + rnorm(n_p, 0, noise_amt) else base_obj
    
    if (length(excluded_rows) > 0) {
      full_mat <- rbind(con_mat, do.call(rbind, excluded_rows))
      full_dir <- c(con_dir, rep("<=", length(excluded_rows)))
      full_rhs <- c(con_rhs, rep(n_roster - 1L, length(excluded_rows)))
    } else {
      full_mat <- con_mat
      full_dir <- con_dir
      full_rhs <- con_rhs
    }
    
    res <- tryCatch(
      lp("max", obj, full_mat, full_dir, full_rhs, all.bin = TRUE),
      error = function(e) list(status = 1L)
    )
    if (res$status != 0L) {
      cat(sprintf("  [Field-NBA] LP infeasible after %d lineups — stopping\n", successful))
      break
    }
    
    sel_idx <- which(res$solution == 1L)
    if (length(sel_idx) != n_roster) next
    
    sig <- paste(sort(meta$Player[sel_idx]), collapse = "|")
    
    excl_row          <- integer(n_p)
    excl_row[sel_idx] <- 1L
    excluded_rows[[length(excluded_rows) + 1L]] <- excl_row
    
    if (sig %in% used_sigs) next
    used_sigs  <- c(used_sigs, sig)
    
    chosen <- meta[sel_idx]
    
    pos_col_name <- if (platform == "FD") "FDPos" else "DKPos"
    if (!"game_rank" %in% names(chosen)) chosen[, game_rank := 1L]
    cm <- chosen[, intersect(c("Player", pos_col_name, "game_rank"), names(chosen)),
                 with = FALSE]
    
    slots <- if (platform == "FD") assign_nba_slots_fd(cm) else assign_nba_slots_dk(cm)
    if (is.null(slots)) next
    
    slot_names <- if (platform == "FD") {
      c("PG1","PG2","SG1","SG2","SF1","SF2","PF1","PF2","C")
    } else {
      c("PG","SG","SF","PF","C","G","F","UTIL")
    }
    
    successful <- successful + 1L
    
    # Geometric-mean ownership so NBA tiers can derive like every other sport
    aown <- if (has_own) {
      ov <- chosen[[own_col]]
      ov[is.na(ov) | ov <= 0] <- NA_real_
      exp(mean(log(ov), na.rm = TRUE))
    } else NA_real_
    
    row_dt <- as.data.table(setNames(lapply(slot_names, function(s) slots[[s]]), std_cols))
    row_dt[, LineupID    := paste0("F", successful)]
    row_dt[, TotalSalary := sum(chosen$Sal)]
    row_dt[, AvgOwn      := round(aown, 2)]
    setcolorder(row_dt, c("LineupID", std_cols, "TotalSalary", "AvgOwn"))
    lineup_list[[successful]] <- row_dt
  }
  
  result <- rbindlist(lineup_list[!sapply(lineup_list, is.null)])
  if (nrow(result) == 0L) stop("NBA field generation produced no valid lineups.")
  
  if (!all(is.na(result$AvgOwn))) setorder(result, -AvgOwn)
  result[, LineupID := paste0("F", seq_len(.N))]
  
  cat(sprintf("  [Field-NBA] %d LP lineups (%s, noise_sd=%.2f)\n",
              nrow(result), platform, noise_sd))
  result[]
}

#' Build NBA field tiers: one wide LP master, then derive per contest.
build_field_tiers_nba <- function(metadata, specs, salary_cap, platform = "DK",
                                  seed = 42L) {
  n_master <- max(sapply(specs, `[[`, "n_field")) * 2L
  
  cat(sprintf("\n  [Field-NBA] Master LP build: n=%d\n", n_master))
  
  master <- generate_field_lineups_nba(
    metadata   = metadata,
    n          = n_master,
    salary_cap = salary_cap,
    platform   = platform,
    noise_sd   = 0.15,     # wide enough to seed every tier
    seed       = seed
  )
  
  has_own <- !all(is.na(master$AvgOwn))
  if (!has_own) {
    # No ownership — derive tiers by simple head() on projection order
    tiers <- lapply(specs, function(s) head(master, min(s$n_field, nrow(master))))
  } else {
    tiers <- lapply(specs, function(s) derive_tier_field(master, s, seed = seed))
  }
  names(tiers) <- names(specs)
  
  for (k in names(tiers))
    cat(sprintf("  [Field-NBA] %-14s %4d lineups\n", specs[[k]]$label, nrow(tiers[[k]])))
  cat("\n")
  
  list(master = master, tiers = tiers)
}


# ============================================================================
# PLATFORM RESOLUTION
# ============================================================================

#' Resolve which platform's lineups + score column to use.
get_cash_platform_data <- function(rv, platform = NULL) {
  if (is.null(platform)) {
    if (!is.null(rv$dk_optimal_lineups))      platform <- "DK"
    else if (!is.null(rv$fd_optimal_lineups)) platform <- "FD"
    else stop("No scored tournament lineups found. Score DK or FD lineups first.")
  }
  
  opt <- switch(platform,
                DK = rv$dk_optimal_lineups,
                FD = rv$fd_optimal_lineups,
                SD = rv$sd_optimal_lineups,
                stop("Unsupported cash platform: ", platform))
  
  if (is.null(opt)) stop(platform, " tournament lineups not found. Score them first.")
  
  score_col <- if (platform == "FD") "FDScore"  else "DKScore"
  sal_col   <- switch(platform, FD = "FDSalary", SD = "SDSalary", "DKSalary")
  id_col    <- if (platform == "FD") "FDID"     else "DKID"
  sal_cap   <- if (platform == "FD") 60000      else 50000
  
  list(platform = platform, optimal_lineups = as.data.table(copy(opt)),
       score_col = score_col, sal_col = sal_col, id_col = id_col, sal_cap = sal_cap)
}


# ============================================================================
# SCORING
#
# Uses score_all_lineups() from OptimalLineups_Core.R — the same matrix
# multiply approach the tournament process uses.  ONE pass over the union of
# every tier's field plus your lineups; tiers then index into the result.
# ============================================================================

#' Wrap a flat lineup pool into the lineup_data list format score_all_lineups expects.
make_lineup_data <- function(lineup_pool, sim_results, player_cols, score_col = "DKScore") {
  list(
    unique_lineups = lineup_pool[, player_cols, with = FALSE],
    n_sims         = length(unique(sim_results$SimID)),
    config         = list(platform_col = score_col, percentiles = c(0.01, 0.05, 0.10, 0.20)),
    mode           = "standard",
    platform_col   = score_col
  )
}


# ============================================================================
# CONTEST EVALUATION
#
# Weighted-rank primitive: for each sim, sort lineups by score desc and cumsum
# the entry weights.  A lineup's weighted rank is the total weight of all
# entries scoring strictly higher, +1.  Cash rate is P(rank <= cash line);
# ROI is (cash_rate * mult - 1).
#
# The rank matrix is never materialized — counters accumulate inside the chunk
# loop, so peak memory is one chunk regardless of sim count.
# ============================================================================

#' Build the entry-weight vector (exponential decay across field lineups).
#'
#' Field lineups are weighted to model real entry duplication: F1 appears
#' max_weight times, the last field lineup min_weight times.  "Yours" lineups
#' always get weight 1 — one entry each.
build_weights <- function(lineup_ids, max_weight = 20L, min_weight = 1L) {
  n         <- length(lineup_ids)
  is_field  <- grepl("^F", lineup_ids)
  field_idx <- which(is_field)
  n_field   <- length(field_idx)
  
  w <- integer(n)
  w[!is_field] <- 1L
  
  if (n_field > 0L) {
    if (n_field == 1L) {
      w[field_idx] <- as.integer(max_weight)
    } else {
      lam <- log(max_weight / min_weight) / (n_field - 1L)
      w[field_idx] <- pmax(as.integer(min_weight),
                           as.integer(round(max_weight * exp(-lam * seq(0, n_field - 1L)))))
    }
  }
  w
}

#' Evaluate one contest from a score matrix, chunked over sims.
#'
#' @param S       numeric matrix: n_lineups x n_sims
#' @param weights integer vector of entry weights, length n_lineups
#' @param spec    contest spec from CONTEST_TYPES
#' @param chunk   integer, sims per chunk
#' @return data.table: CashRate, ROI, AvgFinish, MedianScore
evaluate_contest <- function(S, weights, spec, chunk = 2000L, verbose = TRUE,
                             median_scores = NULL) {
  n_l <- nrow(S)
  n_s <- ncol(S)
  
  total_entries <- sum(weights)
  cash_line     <- floor(total_entries * spec$cash_pct)
  
  if (verbose) {
    cat(sprintf("  [%s] %d lineups | %s weighted entries | cash line: top %s (%.0f%%)\n",
                spec$short, n_l, format(total_entries, big.mark = ","),
                format(cash_line, big.mark = ","), spec$cash_pct * 100))
    flush.console()
  }
  
  cash_ct  <- integer(n_l)
  rank_sum <- numeric(n_l)
  
  t0       <- Sys.time()
  n_chunks <- ceiling(n_s / chunk)
  ci       <- 0L
  
  for (cs in seq(1L, n_s, by = chunk)) {
    ci <- ci + 1L
    ce <- min(cs + chunk - 1L, n_s)
    
    blk <- apply(S[, cs:ce, drop = FALSE], 2L, function(sc) {
      ord <- order(sc, decreasing = TRUE)
      ca  <- c(0L, cumsum(weights[ord])[-n_l])
      wr  <- integer(n_l)
      wr[ord] <- ca + 1L
      wr
    })
    if (is.null(dim(blk))) blk <- matrix(blk, nrow = n_l)
    
    cash_ct  <- cash_ct  + rowSums(blk <= cash_line)
    rank_sum <- rank_sum + rowSums(blk)
    
    if (verbose && (ci %% 5L == 0L || ce == n_s)) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      cat(sprintf("\r  [%s] %3d%%  %.1fs", spec$short, round(ce / n_s * 100), el))
      flush.console()
    }
  }
  if (verbose) { cat("\n"); flush.console() }
  
  p <- cash_ct / n_s
  
  med <- if (!is.null(median_scores)) median_scores else apply(S, 1L, median)
  
  data.table(
    CashRate    = round(p * 100, 1),
    ROI         = round((p * spec$mult - 1) * 100, 1),
    AvgFinish   = round(rank_sum / n_s),
    MedianScore = round(med, 2)
  )
}

#' Run every contest tier off one scoring pass.
#'
#' @param S_all      score matrix for the union pool, rownames = LineupID
#' @param tiers      named list of field data.tables
#' @param your_ids   character vector of your LineupIDs
#' @param specs      named list of contest specs
#' @return list(long = per-contest results, wide = ROI pivot)
run_all_contests <- function(S_all, tiers, your_ids, specs,
                             progress = NULL, prog_from = 0.55, prog_to = 0.90) {
  
  out    <- list()
  n_ct   <- length(specs)
  i_ct   <- 0L
  
  # Median score is identical across contests for a given lineup — compute once
  all_medians <- setNames(apply(S_all, 1L, median), rownames(S_all))
  
  for (k in names(specs)) {
    i_ct <- i_ct + 1L
    spec <- specs[[k]]
    
    ids <- c(tiers[[k]]$LineupID, your_ids)
    ids <- ids[ids %in% rownames(S_all)]
    if (length(ids) == 0L) next
    
    if (!is.null(progress)) {
      progress$set(detail = sprintf("Evaluating %s...", spec$label),
                   value = prog_from + (prog_to - prog_from) * (i_ct - 1L) / n_ct)
    }
    
    S_ct <- S_all[ids, , drop = FALSE]
    w    <- build_weights(ids, spec$max_weight, spec$min_weight)
    res  <- evaluate_contest(S_ct, w, spec, median_scores = all_medians[ids])
    
    res[, LineupID := ids]
    res[, Contest  := spec$label]
    res[, ContestKey := k]
    res[, Source   := ifelse(grepl("^Y", LineupID), "Yours", "Field")]
    
    out[[k]] <- res
  }
  
  long <- rbindlist(out, use.names = TRUE)
  
  # Wide ROI pivot across YOUR lineups only
  yours <- long[Source == "Yours"]
  wide  <- NULL
  if (nrow(yours) > 0L) {
    roi_w  <- dcast(yours, LineupID ~ Contest, value.var = "ROI")
    cash_w <- dcast(yours, LineupID ~ Contest, value.var = "CashRate")
    setnames(cash_w, setdiff(names(cash_w), "LineupID"),
             paste0(setdiff(names(cash_w), "LineupID"), " Cash%"))
    wide <- merge(roi_w, cash_w, by = "LineupID")
    
    # Column order follows spec order (2x/3x/5x/10x), not dcast's alphabetical
    spec_labels <- sapply(specs, `[[`, "label")
    ct_cols     <- spec_labels[spec_labels %in% names(wide)]
    cash_labels <- paste0(ct_cols, " Cash%")
    cash_labels <- cash_labels[cash_labels %in% names(wide)]
    
    if (length(ct_cols) > 0L) {
      # BestContest: index of max ROI per row, NA-safe
      roi_mat <- as.matrix(wide[, ct_cols, with = FALSE])
      roi_mat[is.na(roi_mat)] <- -Inf
      best_i  <- max.col(roi_mat, ties.method = "first")
      wide[, BestContest := ct_cols[best_i]]
      wide[, BestROI := apply(roi_mat, 1L, function(r) { m <- max(r); if (is.finite(m)) m else NA_real_ })]
      
      setcolorder(wide, c("LineupID", ct_cols, cash_labels, "BestContest", "BestROI"))
      setorder(wide, -BestROI)
    }
  }
  
  list(long = long, wide = wide)
}


# ============================================================================
# COMBINED EXPOSURE TABLE
# One row per player: Player, Salary, OwnProj, PPD, FieldExp%, YourExp%
# ============================================================================

build_combined_exposure <- function(field_pool, your_pool, metadata, player_cols) {
  
  count_exp <- function(pool) {
    n  <- nrow(pool)
    if (n == 0L) return(data.table(Player = character(0), Pct = numeric(0)))
    pc <- intersect(player_cols, names(pool))
    tab <- as.data.table(table(unlist(pool[, pc, with = FALSE])))
    setnames(tab, c("Player", "N"))
    tab[, Pct := round(N / n * 100, 1)]
    tab[, .(Player, Pct)]
  }
  
  fe <- count_exp(field_pool)
  ye <- count_exp(your_pool)
  
  all_p <- unique(c(fe$Player, ye$Player))
  base  <- data.table(Player = all_p)
  base  <- merge(base, fe, by = "Player", all.x = TRUE); setnames(base, "Pct", "FieldExp")
  base  <- merge(base, ye, by = "Player", all.x = TRUE); setnames(base, "Pct", "YourExp")
  base[is.na(FieldExp), FieldExp := 0]
  base[is.na(YourExp),  YourExp  := 0]
  
  meta_dt   <- as.data.table(metadata)
  meta_cols <- intersect(c("Player", "DKSalary", "DKOwn"), names(meta_dt))
  meta_sub  <- meta_dt[, meta_cols, with = FALSE]
  
  exp_tbl <- merge(base, meta_sub, by = "Player", all.x = TRUE)
  
  if ("DKOwn" %in% names(exp_tbl)) {
    if (max(exp_tbl$DKOwn, na.rm = TRUE) <= 1) exp_tbl[, DKOwn := round(DKOwn * 100, 1)]
    setnames(exp_tbl, "DKOwn", "OwnProj")
    exp_tbl[, PPD := round(OwnProj / (DKSalary / 1000), 2)]
  }
  if ("DKSalary" %in% names(exp_tbl)) setnames(exp_tbl, "DKSalary", "Salary")
  
  exp_tbl[, TotalExp := FieldExp + YourExp]
  setorder(exp_tbl, -TotalExp)
  exp_tbl[, TotalExp := NULL]
  
  keep <- intersect(c("Player", "Salary", "OwnProj", "PPD", "FieldExp", "YourExp"),
                    names(exp_tbl))
  exp_tbl[, keep, with = FALSE]
}


# ============================================================================
# BACK-COMPAT SHIM
# Old callers of cash_rate_from_score_matrix() still work — routes through the
# new primitive using the double_up spec.
# ============================================================================

cash_rate_from_score_matrix <- function(score_matrix, lineup_ids,
                                        cash_pct   = 0.45,
                                        max_weight = 20L,
                                        min_weight = 1L,
                                        verbose    = TRUE) {
  spec <- CONTEST_TYPES$double_up
  spec$cash_pct <- cash_pct
  w   <- build_weights(lineup_ids, max_weight, min_weight)
  res <- evaluate_contest(score_matrix, w, spec, verbose = verbose)
  data.table(LineupID    = lineup_ids,
             MedianScore = res$MedianScore,
             CashRate    = res$CashRate)
}


# ============================================================================
# UI
# ============================================================================

render_cash_game_tab_ui <- function() {
  tagList(
    div(style = "padding:16px;",
        
        # ── Platform selector (shown when multiple platforms available) ──────
        uiOutput("du_platform_selector_ui"),
        
        # ── Contest tier selector ────────────────────────────────────────────
        div(class = "gts-platform-pills", style = "margin-bottom:12px;",
            span(class = "gts-sr-label", style = "margin-right:6px;", "Contests:"),
            lapply(names(CONTEST_TYPES), function(k) {
              tags$button(
                class   = "gts-pill du-ct-pill active",
                `data-ct` = k,
                onclick =
                  "this.classList.toggle('active');
                   var sel=[];
                   document.querySelectorAll('.du-ct-pill.active').forEach(function(b){
                     sel.push(b.getAttribute('data-ct'));
                   });
                   Shiny.setInputValue('du_contests', sel.join(','), {priority:'event'});",
                CONTEST_TYPES[[k]]$short
              )
            })
        ),
        
        # ── Info strip ───────────────────────────────────────────────────────
        div(style = paste0("display:flex;align-items:center;gap:0;background:#141414;",
                           "border:1px solid #222;border-radius:6px;overflow:hidden;",
                           "margin-bottom:10px;height:42px;"),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;border-right:1px solid #222;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Mode"),
                uiOutput("du_mode_desc_ui")
            ),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;border-right:1px solid #222;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Field"),
                uiOutput("du_field_desc_ui")
            ),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;border-right:1px solid #222;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Your Pool"),
                uiOutput("du_yours_desc_ui")
            ),
            div(style = "display:flex;align-items:center;padding:0 18px;height:42px;flex-shrink:0;",
                span(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px;", "Cash Lines"),
                uiOutput("du_cashline_desc_ui")
            )
        ),
        
        # ── Action button ────────────────────────────────────────────────────
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:16px;",
            actionButton("du_run", "Run Contests",
                         class = "btn-primary", icon = icon("play"),
                         style = "height:38px;font-size:12px;font-weight:700;")
        ),
        
        uiOutput("du_status_msg"),
        
        conditionalPanel(
          condition = "output.du_has_results == true",
          
          # ── Contest comparison: your lineups x contest ROI ──────────────────
          box(width = NULL, title = "Contest Comparison \u2014 ROI by Tier",
              status = "primary", solidHeader = TRUE,
              div(style = "color:#777;font-size:11px;margin-bottom:8px;",
                  "ROI% per contest type for each of your lineups. ",
                  "BestContest flags the tier where the lineup earns most."),
              DTOutput("du_compare_tbl") %>%
                shinycssloaders::withSpinner(color = "#FFE500", type = 6)
          ),
          
          # ── Per-contest ranked lineups ─────────────────────────────────────
          box(width = NULL,
              title = uiOutput("du_results_title"),
              status = "primary", solidHeader = TRUE,
              div(style = "display:flex;align-items:center;gap:10px;margin-bottom:10px;",
                  uiOutput("du_view_pills_ui"),
                  downloadButton("du_download", "Download All",
                                 class = "btn-primary",
                                 style = "height:32px;font-size:11px;")
              ),
              DTOutput("du_results_tbl") %>%
                shinycssloaders::withSpinner(color = "#FFE500", type = 6)
          ),
          
          box(width = NULL, title = "Player Exposure \u2014 Field vs Yours",
              status = "primary", solidHeader = TRUE,
              DTOutput("du_exposure_tbl") %>%
                shinycssloaders::withSpinner(color = "#FFE500", type = 6)
          )
        )
    )
  )
}


# ============================================================================
# SERVER
# ============================================================================

register_cash_game_observers <- function(input, output, session, rv) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  du_rv <- reactiveValues(
    long        = NULL,   # per-contest results (all lineups)
    wide        = NULL,   # your lineups x contest ROI pivot
    tiers       = NULL,   # field data.tables per contest
    combined    = NULL,   # union pool with slot columns
    exposure    = NULL,
    status      = NULL,
    has_results = FALSE,
    platform    = NULL,
    id_col      = NULL,
    specs       = NULL,
    view        = NULL,   # which contest tab is being viewed
    std_cols    = NULL
  )
  
  output$du_has_results <- reactive({
    # Hide stale results if the underlying scored lineups were cleared (re-upload)
    has_lus <- !is.null(rv$dk_optimal_lineups) || !is.null(rv$fd_optimal_lineups) ||
      !is.null(rv$sd_optimal_lineups)
    isTRUE(du_rv$has_results) && has_lus
  })
  outputOptions(output, "du_has_results", suspendWhenHidden = FALSE)
  
  # ── Which contests are selected ──────────────────────────────────────────
  du_contests <- reactive({
    sel <- input$du_contests
    if (is.null(sel) || !nzchar(sel)) return(names(CONTEST_TYPES))
    keys <- strsplit(sel, ",", fixed = TRUE)[[1]]
    keys <- intersect(keys, names(CONTEST_TYPES))
    if (length(keys) == 0L) names(CONTEST_TYPES) else keys
  })
  
  # ── Platform selector ────────────────────────────────────────────────────
  output$du_platform_selector_ui <- renderUI({
    req(rv$config)
    plats <- Filter(function(p) {
      !is.null(switch(p, DK = rv$dk_optimal_lineups, FD = rv$fd_optimal_lineups,
                      SD = rv$sd_optimal_lineups, NULL))
    }, c("DK","FD","SD"))
    if (length(plats) <= 1) return(NULL)
    
    div(class = "gts-platform-pills", style = "margin-bottom:12px;",
        span(class = "gts-sr-label", style = "margin-right:6px;", "Platform:"),
        lapply(plats, function(p) {
          tags$button(
            class   = paste("gts-pill du-plat-pill", if (p == plats[1]) "active" else ""),
            onclick = sprintf(
              "Shiny.setInputValue('du_platform','%s',{priority:'event'});
               document.querySelectorAll('.du-plat-pill').forEach(function(b){b.classList.remove('active')});
               this.classList.add('active')", p),
            p
          )
        })
    )
  })
  
  du_platform <- reactive({
    p <- input$du_platform
    if (!is.null(p) && p %in% c("DK","FD","SD")) return(p)
    if (!is.null(rv$dk_optimal_lineups)) return("DK")
    if (!is.null(rv$fd_optimal_lineups)) return("FD")
    if (!is.null(rv$sd_optimal_lineups)) return("SD")
    "DK"
  })
  
  output$du_mode_desc_ui <- renderUI({
    plat_label <- switch(du_platform(), FD = "FanDuel", SD = "Showdown (DK)", "DraftKings")
    n_ct <- length(du_contests())
    span(style = "color:#FFE500;font-weight:700;font-size:13px;",
         sprintf("%d Contests \u2014 %s", n_ct, plat_label))
  })
  
  output$du_cashline_desc_ui <- renderUI({
    keys <- du_contests()
    txt  <- paste(sapply(keys, function(k)
      sprintf("%s: %.0f%%", CONTEST_TYPES[[k]]$short, CONTEST_TYPES[[k]]$cash_pct * 100)),
      collapse = "  \u2022  ")
    span(style = "color:#aaa;font-size:12px;", txt)
  })
  
  output$du_field_desc_ui <- renderUI({
    specs  <- scale_contest_specs(rv$config %||% list())
    keys   <- du_contests()
    is_nba <- isTRUE(rv$config$sport_name == "NBA")
    sizes  <- paste(sapply(keys, function(k) specs[[k]]$n_field), collapse = "/")
    desc   <- if (du_platform() == "SD") paste0("Tournament pool subsets (", sizes, ")")
    else if (is_nba)          paste0("LP-optimized master, tiers ", sizes)
    else                      paste0("Chalk core + fill, tiers ", sizes)
    span(style = "color:#aaa;font-size:12px;", desc)
  })
  
  output$du_yours_desc_ui <- renderUI({
    cash_p <- get_cash_params(rv$config %||% list())
    span(style = "color:#aaa;font-size:12px;",
         paste0("Top ", cash_p$n_yours, " by median score"))
  })
  
  # ── View pills (which contest's ranked table to show) ─────────────────────
  output$du_view_pills_ui <- renderUI({
    req(du_rv$specs)
    keys <- names(du_rv$specs)
    cur  <- du_rv$view %||% keys[1]
    div(class = "gts-platform-pills", style = "display:inline-flex;",
        lapply(keys, function(k) {
          tags$button(
            class = paste("gts-pill du-view-pill", if (k == cur) "active" else ""),
            onclick = sprintf(
              "Shiny.setInputValue('du_view','%s',{priority:'event'});
               document.querySelectorAll('.du-view-pill').forEach(function(b){b.classList.remove('active')});
               this.classList.add('active')", k),
            du_rv$specs[[k]]$short
          )
        })
    )
  })
  
  observeEvent(input$du_view, {
    if (!is.null(input$du_view)) du_rv$view <- input$du_view
  })
  
  
  # ── Run contests ─────────────────────────────────────────────────────────
  observeEvent(input$du_run, {
    plat    <- du_platform()
    opt_lus <- switch(plat,
                      DK = rv$dk_optimal_lineups,
                      FD = rv$fd_optimal_lineups,
                      SD = rv$sd_optimal_lineups,
                      NULL)
    req(opt_lus, rv$simulation_results, rv$sim_metadata, rv$config)
    
    du_rv$has_results <- FALSE
    du_rv$long <- NULL; du_rv$wide <- NULL
    du_rv$tiers <- NULL; du_rv$exposure <- NULL
    
    progress <- Progress$new(session, min = 0, max = 1)
    progress$set(message = "Running contest sims...", value = 0.02)
    on.exit(progress$close())
    
    tryCatch({
      
      t_total <- Sys.time()
      cash_p  <- get_cash_params(rv$config)
      is_nba  <- isTRUE(rv$config$sport_name == "NBA")
      
      all_specs <- scale_contest_specs(rv$config)
      specs     <- all_specs[du_contests()]
      
      constraints <- get_dk_constraints(rv$config)
      sal_cap     <- switch(plat,
                            FD = rv$config$salary_caps$FD %||% 60000,
                            SD = rv$config$salary_caps$SD %||% 50000,
                            constraints$salary_cap)
      sal_floor <- cash_p$sal_floor %||% 49000
      meta_raw  <- as.data.table(copy(rv$sim_metadata))
      
      score_col <- switch(plat, FD = "FDScore", SD = "DKScore", "DKScore")
      sal_col   <- switch(plat, FD = "FDSalary", SD = "SDSalary", "DKSalary")
      own_col   <- if (plat == "FD") "FDOwn"  else "DKOwn"
      proj_col  <- if (plat == "FD") "FDProj" else "DKProj"
      id_col    <- switch(plat, FD = "FDID", SD = "DKID", "DKID")
      
      dk_opt  <- copy(as.data.table(opt_lus))
      sim_res <- copy(as.data.table(rv$simulation_results))
      
      player_cols <- get_player_cols(dk_opt)
      r_size      <- length(player_cols)
      std_cols    <- paste0("Player", seq_len(r_size))
      n_sims      <- length(unique(sim_res$SimID))
      n_gpp       <- nrow(dk_opt)
      
      cat(sprintf("\n  [Contests] %s | %s sims | %d GPP lineups | %d contests\n",
                  plat, format(n_sims, big.mark = ","), n_gpp, length(specs)))
      flush.console()
      
      if (!"LineupID" %in% names(dk_opt)) dk_opt[, LineupID := paste0("GPP", seq_len(.N))]
      
      # ── Step 1: Build field tiers ────────────────────────────────────────
      cat("  [Contests] Step 1/4: Building field tiers...\n"); flush.console()
      progress$set(detail = "Step 1/4: Building field tiers...", value = 0.05)
      
      if (plat == "SD") {
        # SD: rank the tournament SD pool by true median, tiers = head() subsets
        sd_pool <- copy(as.data.table(opt_lus))
        if (!"LineupID" %in% names(sd_pool)) sd_pool[, LineupID := paste0("GPP", seq_len(.N))]
        sd_pc  <- get_player_cols(sd_pool)
        if (!identical(sd_pc, std_cols)) setnames(sd_pool, sd_pc, std_cols)
        
        sd_all <- unique(sim_res$Player)
        sd_idx <- setNames(seq_along(sd_all), sd_all)
        sw     <- dcast(sim_res[, .(SimID, Player, DKScore)],
                        Player ~ SimID, value.var = "DKScore", fill = 0)
        sm     <- as.matrix(sw[, -1, with = FALSE]); rownames(sm) <- sw$Player
        
        n_sd <- nrow(sd_pool); csz <- 500L
        med  <- numeric(n_sd)
        for (ci in seq_len(ceiling(n_sd / csz))) {
          i1 <- (ci - 1L) * csz + 1L; i2 <- min(ci * csz, n_sd)
          ch <- sd_pool[i1:i2]
          mm <- matrix(0L, nrow = nrow(ch), ncol = length(sd_all))
          colnames(mm) <- sd_all
          for (pc in std_cols) {
            pi <- sd_idx[ch[[pc]]]; ok <- !is.na(pi)
            mm[cbind(which(ok), pi[ok])] <- 1L
          }
          med[i1:i2] <- apply(mm %*% sm, 1, median)
        }
        sd_pool[, MedianScore := med]
        setorder(sd_pool, -MedianScore)
        sd_pool[, LineupID := paste0("F", seq_len(.N))]
        if (!"TotalSalary" %in% names(sd_pool)) sd_pool[, TotalSalary := NA_real_]
        sd_pool[, TotalSalary := as.numeric(TotalSalary)]
        if (!"AvgOwn" %in% names(sd_pool)) sd_pool[, AvgOwn := NA_real_]
        
        master <- sd_pool
        tiers  <- lapply(specs, function(s) head(master, min(s$n_field, nrow(master))))
        names(tiers) <- names(specs)
        
      } else if (is_nba) {
        if (!proj_col %in% names(meta_raw))
          stop(proj_col, " not found in metadata — NBA field requires ETR projections.")
        ft     <- build_field_tiers_nba(meta_raw, specs, sal_cap, platform = plat)
        master <- ft$master
        tiers  <- ft$tiers
        
      } else {
        if (!own_col %in% names(meta_raw))
          stop(own_col, " not found in metadata.")
        ft <- build_field_tiers(
          metadata     = meta_raw,
          specs        = specs,
          salary_cap   = sal_cap,
          salary_floor = sal_floor,
          roster_size  = r_size,
          proj_col     = proj_col,
          own_col      = own_col,
          sal_col      = sal_col
        )
        master <- ft$master
        tiers  <- ft$tiers
      }
      
      progress$set(detail = "Step 1/4: Fields built.", value = 0.15)
      
      # ── Step 2: Rank your GPP pool, select top N ─────────────────────────
      cat(sprintf("  [Contests] Step 2/4: Ranking %d GPP lineups by median...\n", n_gpp))
      flush.console()
      progress$set(detail = "Step 2/4: Ranking your lineups...", value = 0.18)
      
      rank_col <- NULL
      if (!is_nba) {
        for (cand in c("MedianScore", "AvgScore"))
          if (cand %in% names(dk_opt)) { rank_col <- cand; break }
      }
      
      if (!is.null(rank_col)) {
        if (!"MedianScore" %in% names(dk_opt)) setnames(dk_opt, rank_col, "MedianScore")
      } else {
        if (!score_col %in% names(sim_res)) stop(score_col, " not found in sim results.")
        all_pl <- unique(sim_res$Player)
        pl_idx <- setNames(seq_along(all_pl), all_pl)
        sw <- dcast(sim_res[, c("SimID","Player", score_col), with = FALSE],
                    Player ~ SimID, value.var = score_col, fill = 0)
        sm <- as.matrix(sw[, -1, with = FALSE]); rownames(sm) <- sw$Player
        
        csz <- 500L; med <- numeric(n_gpp)
        for (ci in seq_len(ceiling(n_gpp / csz))) {
          i1 <- (ci - 1L) * csz + 1L; i2 <- min(ci * csz, n_gpp)
          ch <- dk_opt[i1:i2]
          mm <- matrix(0L, nrow = nrow(ch), ncol = length(all_pl))
          colnames(mm) <- all_pl
          for (pc in player_cols) {
            pi <- pl_idx[ch[[pc]]]; ok <- !is.na(pi)
            mm[cbind(which(ok), pi[ok])] <- 1L
          }
          med[i1:i2] <- apply(mm %*% sm, 1, median)
          cat(sprintf("\r  [Contests] Step 2/4: %d%%", round(i2 / n_gpp * 100)))
          flush.console()
        }
        cat("\n"); flush.console()
        dk_opt[, MedianScore := med]
      }
      
      setorder(dk_opt, -MedianScore)
      your_pool <- head(dk_opt, cash_p$n_yours)
      your_pool[, LineupID := paste0("Y", seq_len(.N))]
      if (!identical(player_cols, std_cols)) setnames(your_pool, player_cols, std_cols)
      
      if (!"TotalSalary" %in% names(your_pool) && sal_col %in% names(meta_raw)) {
        your_pool[, TotalSalary := rowSums(
          sapply(std_cols, function(cl)
            meta_raw[match(your_pool[[cl]], meta_raw$Player), get(sal_col)]),
          na.rm = TRUE)]
      } else if (!"TotalSalary" %in% names(your_pool)) {
        your_pool[, TotalSalary := NA_real_]
      }
      if (!"AvgOwn" %in% names(your_pool)) your_pool[, AvgOwn := NA_real_]
      
      # ── Step 3: ONE scoring pass over the union pool ─────────────────────
      union_field <- unique(rbindlist(tiers, use.names = TRUE, fill = TRUE),
                            by = "LineupID")
      keep_cols   <- c("LineupID", std_cols, "TotalSalary", "AvgOwn")
      combined    <- rbindlist(
        list(your_pool[, keep_cols, with = FALSE],
             union_field[, keep_cols, with = FALSE]),
        use.names = TRUE)
      
      cat(sprintf("  [Contests] Step 3/4: Scoring %d unique lineups x %s sims...\n",
                  nrow(combined), format(n_sims, big.mark = ",")))
      flush.console()
      progress$set(detail = sprintf("Step 3/4: Scoring %d lineups...", nrow(combined)),
                   value = 0.28)
      
      ld  <- make_lineup_data(combined, sim_res, std_cols, score_col)
      S   <- score_all_lineups(ld, sim_res, verbose = TRUE)
      if (nrow(S) != nrow(combined))
        stop(sprintf("Scoring row mismatch: %d scored vs %d lineups.",
                     nrow(S), nrow(combined)))
      rownames(S) <- combined$LineupID
      
      progress$set(detail = "Step 3/4: Scoring complete.", value = 0.55)
      
      # ── Step 4: Evaluate each contest ────────────────────────────────────
      cat("  [Contests] Step 4/4: Evaluating contests...\n"); flush.console()
      
      ctr <- run_all_contests(S, tiers, your_pool$LineupID, specs,
                              progress = progress)
      
      progress$set(detail = "Building exposure...", value = 0.93)
      
      # ── Exposure: widest tier's field vs your pool ───────────────────────
      widest <- names(specs)[which.max(sapply(specs, `[[`, "n_field"))]
      exp_meta <- copy(meta_raw)
      if (sal_col %in% names(exp_meta) && sal_col != "DKSalary")
        setnames(exp_meta, sal_col, "DKSalary")
      if (own_col %in% names(exp_meta) && own_col != "DKOwn")
        setnames(exp_meta, own_col, "DKOwn")
      
      exposure <- build_combined_exposure(tiers[[widest]], your_pool, exp_meta, std_cols)
      
      # ── Store ────────────────────────────────────────────────────────────
      du_rv$long     <- ctr$long
      du_rv$wide     <- ctr$wide
      du_rv$tiers    <- tiers
      du_rv$combined <- combined
      du_rv$exposure <- exposure
      du_rv$platform <- plat
      du_rv$id_col   <- id_col
      du_rv$specs    <- specs
      du_rv$std_cols <- std_cols
      du_rv$view     <- names(specs)[1]
      du_rv$has_results <- TRUE
      
      elapsed <- as.numeric(difftime(Sys.time(), t_total, units = "secs"))
      du_rv$status <- sprintf(
        "Contests complete \u2014 %s | %d tiers | %d unique lineups | %s sims | %.1fs",
        plat, length(specs), nrow(combined), format(n_sims, big.mark = ","), elapsed)
      
      cat(sprintf("  [Contests] Complete in %.1fs\n\n", elapsed)); flush.console()
      progress$set(detail = "Done!", value = 1)
      showNotification("Contest simulation complete!", type = "message")
      
    }, error = function(e) {
      du_rv$status <- paste("Error:", e$message)
      showNotification(paste("Contest error:", e$message), type = "error", duration = 10)
      cat("  [Contests] ERROR:\n"); print(e)
    })
  })
  
  
  # ── Status ───────────────────────────────────────────────────────────────
  output$du_status_msg <- renderUI({
    msg <- du_rv$status
    if (is.null(msg)) {
      has_any <- !is.null(rv$dk_optimal_lineups) || !is.null(rv$fd_optimal_lineups) ||
        !is.null(rv$sd_optimal_lineups)
      if (!has_any)
        return(div(style = "color:#666;font-size:12px;padding:8px 0;",
                   icon("info-circle"),
                   " Score Tournament Lineups first, then Run Contests."))
      return(div(style = "color:#666;font-size:12px;padding:8px 0;",
                 icon("info-circle"),
                 " Select contest tiers and click Run Contests."))
    }
    div(class = "gts-sim-done", icon("check-circle"), msg)
  })
  
  
  # ── Contest comparison table ─────────────────────────────────────────────
  output$du_compare_tbl <- renderDT({
    req(du_rv$wide)
    dt <- copy(du_rv$wide)
    
    roi_cols  <- intersect(sapply(du_rv$specs, `[[`, "label"), names(dt))
    cash_cols <- grep(" Cash%$", names(dt), value = TRUE)
    
    datatable(dt, rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE,
                             searching = FALSE, lengthChange = FALSE, dom = "tp"),
              class = "stripe hover compact") %>%
      formatRound(intersect(c(roi_cols, cash_cols, "BestROI"), names(dt)), 1) %>%
      formatStyle(roi_cols,
                  color = styleInterval(c(-0.001, 0.001), c("#e06666", "#888", "#7fd18f")),
                  fontWeight = "600") %>%
      formatStyle("BestContest", color = "#FFE500", fontWeight = "700")
  })
  
  
  # ── Results title ────────────────────────────────────────────────────────
  output$du_results_title <- renderUI({
    req(du_rv$long, du_rv$view, du_rv$specs)
    k   <- du_rv$view
    lbl <- du_rv$specs[[k]]$label
    sub <- du_rv$long[ContestKey == k]
    span(sprintf("%s \u2014 %d Lineups Ranked by ROI  (%d Yours / %d Field)",
                 lbl, nrow(sub),
                 sum(sub$Source == "Yours"), sum(sub$Source == "Field")),
         style = "color:#FFE500;")
  })
  
  
  # ── Per-contest results table ────────────────────────────────────────────
  output$du_results_tbl <- renderDT({
    req(du_rv$long, du_rv$view, du_rv$combined)
    k   <- du_rv$view
    sub <- du_rv$long[ContestKey == k]
    if (nrow(sub) == 0L) return(NULL)
    
    std_cols <- du_rv$std_cols
    base     <- du_rv$combined[, c("LineupID", std_cols, "TotalSalary", "AvgOwn"),
                               with = FALSE]
    dt <- merge(sub[, .(LineupID, Source, MedianScore, AvgFinish, CashRate, ROI)],
                base, by = "LineupID")
    
    setcolorder(dt, c("LineupID", "Source", std_cols, "TotalSalary", "AvgOwn",
                      "MedianScore", "AvgFinish", "CashRate", "ROI"))
    setorder(dt, -ROI, -CashRate)
    
    slot_labels <- get_slot_labels(rv$config, length(std_cols))
    if (!is.null(slot_labels)) {
      for (i in seq_along(std_cols))
        if (std_cols[i] %in% names(dt)) setnames(dt, std_cols[i], slot_labels[i])
    }
    
    datatable(dt, rownames = FALSE,
              options = list(pageLength = 60, scrollX = TRUE, scrollY = "520px",
                             searching = TRUE, lengthChange = FALSE, dom = "ftp"),
              class = "stripe hover compact") %>%
      { if ("TotalSalary" %in% names(dt)) formatCurrency(., "TotalSalary", "$", digits = 0) else . } %>%
      { if ("AvgOwn" %in% names(dt)) formatRound(., "AvgOwn", 2) else . } %>%
      formatRound(c("MedianScore", "CashRate", "ROI"), 1) %>%
      formatStyle("ROI",
                  color = styleInterval(c(-0.001, 0.001), c("#e06666", "#888", "#7fd18f")),
                  fontWeight = "600") %>%
      formatStyle("Source",
                  color      = styleEqual(c("Yours", "Field"), c("#FFE500", "#aaaaaa")),
                  fontWeight = styleEqual(c("Yours", "Field"), c("700", "400")))
  })
  
  
  # ── Exposure table ───────────────────────────────────────────────────────
  output$du_exposure_tbl <- renderDT({
    req(du_rv$exposure)
    dt <- copy(du_rv$exposure)
    datatable(dt, rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE,
                             searching = FALSE, lengthChange = FALSE, dom = "tp"),
              class = "stripe hover compact") %>%
      { if ("Salary"  %in% names(dt)) formatCurrency(., "Salary", "$", digits = 0) else . } %>%
      { if ("OwnProj" %in% names(dt)) formatRound(., "OwnProj", 1) else . } %>%
      { if ("PPD"     %in% names(dt)) formatRound(., "PPD", 2) else . } %>%
      formatRound(intersect(c("FieldExp", "YourExp"), names(dt)), 1) %>%
      formatStyle("FieldExp",
                  background = styleColorBar(c(0, 100), "rgba(255,229,0,0.35)"),
                  backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "left") %>%
      formatStyle("YourExp",
                  background = styleColorBar(c(0, 100), "rgba(74,144,217,0.45)"),
                  backgroundSize = "90% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "left")
  })
  
  
  # ── Download ─────────────────────────────────────────────────────────────
  output$du_download <- downloadHandler(
    filename = function() paste0("GTS_Contests_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content  = function(file) {
      req(du_rv$long, du_rv$combined, rv$sim_metadata)
      meta     <- as.data.table(rv$sim_metadata)
      id_col   <- du_rv$id_col %||% "DKID"
      plat     <- du_rv$platform %||% "DK"
      std_cols <- du_rv$std_cols
      
      add_ids <- function(dl) {
        slot_cols <- intersect(std_cols, names(dl))
        if (length(slot_cols) == 0L) return(dl)
        if (plat == "SD") {
          if ("CPTID" %in% names(meta) && length(slot_cols) >= 1L) {
            lu <- setNames(meta$CPTID, meta$Player)
            dl[[slot_cols[1]]] <- paste0(dl[[slot_cols[1]]], " (", lu[dl[[slot_cols[1]]]], ")")
          }
          if ("SDID" %in% names(meta) && length(slot_cols) > 1L) {
            lu <- setNames(meta$SDID, meta$Player)
            for (cl in slot_cols[-1])
              dl[[cl]] <- paste0(dl[[cl]], " (", lu[dl[[cl]]], ")")
          }
        } else if (id_col %in% names(meta)) {
          lu <- setNames(meta[[id_col]], meta$Player)
          if (plat == "FD") {
            for (cl in slot_cols) dl[[cl]] <- paste0(lu[dl[[cl]]], ":", dl[[cl]])
          } else {
            for (cl in slot_cols) dl[[cl]] <- paste0(dl[[cl]], " (", lu[dl[[cl]]], ")")
          }
        }
        dl
      }
      
      wb <- openxlsx::createWorkbook()
      
      if (!is.null(du_rv$wide)) {
        openxlsx::addWorksheet(wb, "Contest Comparison")
        openxlsx::writeData(wb, "Contest Comparison", as.data.frame(du_rv$wide))
      }
      
      for (k in names(du_rv$specs)) {
        sub <- du_rv$long[ContestKey == k]
        if (nrow(sub) == 0L) next
        base <- du_rv$combined[, c("LineupID", std_cols, "TotalSalary", "AvgOwn"),
                               with = FALSE]
        dl <- merge(sub[, .(LineupID, Source, MedianScore, AvgFinish, CashRate, ROI)],
                    base, by = "LineupID")
        setorder(dl, -ROI, -CashRate)
        dl <- add_ids(copy(dl))
        
        sheet <- substr(du_rv$specs[[k]]$label, 1, 28)
        openxlsx::addWorksheet(wb, sheet)
        openxlsx::writeData(wb, sheet, as.data.frame(dl))
      }
      
      if (!is.null(du_rv$exposure)) {
        openxlsx::addWorksheet(wb, "Exposure")
        openxlsx::writeData(wb, "Exposure", as.data.frame(du_rv$exposure))
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}
# end of cash_game_module.R