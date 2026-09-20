# ============================================================================
# lineup_lab_module.R -- "Lineup Lab" tab (20 Sep 2026)
# ============================================================================
# Re-optimises a FINISHED sim under a user-chosen constraint. The user names
# players to force in (and optionally some to keep out), says how well that set
# has to have done for a sim to count, and gets a small pool of the best
# lineups containing it -- scored, filterable and addable to the same portfolio
# the normal process feeds.
#
# Nothing is re-simulated. The draws are the ones already sitting in
# rv$simulation_results, so a Lineup Lab pool and the main pool are two views of
# one simulation and their metrics mean the same thing.
#
# WHY. Control over which players a pool is built around, and a rest-of-roster
# chosen by the sim rather than by a stacking rule. The main pool is capped at
# 5,000 lineups out of a far larger space, so a particular player can be thin
# in it or present only in rosters built for a different script.
#
# It is NOT a fix for "lineups do not separate". Measured on the live W2 Sunday
# sheet at 20,000 sims, the normal pool has 34,217 distinct lineups with max
# Top1Count = 1; locking a QB, a QB + WR, or a QB + WR + bring-back all still
# leave ~20,000 distinct lineups with max repeats 2. Repeat counts cannot rank
# a big classic either way -- WinRate / Top1Pct are the columns that do, and
# they are on this table already. See the header of
# find_optimal_lineups_nfl_classic_locked() for the full numbers.
#
# The conditioning IS a real effect: inside the draws where the locked set
# produced, game environments correlate and its pass catchers recur on their
# own, so the stack comes out of the sim (same-team players per lineup 1.48 ->
# 1.74 going from cond 100% to cond 15% on a QB lock).
#
# SCOPE. NFL classic only for now (DK and FD). The solver reads DK/FD classic
# slot rules; every other sport needs its own bounds before it can be offered,
# so the tab hides itself rather than pretending.
#
# Pairs with find_optimal_lineups_nfl_classic_locked() in OptimalLineups_Core.R,
# which is where the constraint maths lives and is commented in full.
# ============================================================================

# Local NULL-coalesce. The engines each define their own `%||%` at global scope
# with slightly different NA/length handling and are sourced after this file,
# so nothing here relies on which one wins.
.ll_or <- function(a, b) if (is.null(a)) b else a

.LL_MIN_FILTERS <- list(
  c("win",   "Win",   "WinRate",  "0.01"),
  c("top1",  "Top1",  "Top1Pct",  "0.5"),
  c("top5",  "Top5",  "Top5Pct",  "1"),
  c("top10", "Top10", "Top10Pct", "2"),
  c("top20", "Top20", "Top20Pct", "5")
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
render_lineup_lab_tab_ui <- function() {
  tagList(
    tags$style(HTML("
      .gts-llchips{display:flex;flex-wrap:wrap;gap:4px;align-items:center}
      .gts-llchip{display:inline-flex;align-items:center;height:22px;padding:0 8px;
        border:1px solid #3a3a3a;border-radius:11px;background:#222;color:#999;
        font-size:10px;font-weight:700;letter-spacing:.03em;cursor:pointer;user-select:none}
      .gts-llchip:hover{background:#2a2a2a;color:#ddd;border-color:#555}
      .gts-llchip.on{background:#FFE500;color:#111;border-color:#FFE500}
      .gts-llsearch{width:100%;height:24px;background:#1c1c1c;border:1px solid #3a3a3a;
        border-radius:3px;color:#ddd;font-size:11px;padding:0 7px;outline:none}
      .gts-llsearch:focus{border-color:#FFE500}
      .gts-llboard{display:flex;flex-wrap:wrap;gap:4px;margin-top:7px;max-height:190px;
        overflow-y:auto;padding:3px;background:#1c1c1c;border:1px solid #333;border-radius:3px}
      .gts-llpill{display:inline-flex;align-items:center;gap:5px;height:24px;padding:0 8px;
        border:1px solid #3a3a3a;border-radius:12px;background:#242424;color:#bbb;
        font-size:11px;cursor:pointer;user-select:none;white-space:nowrap}
      .gts-llpill:hover{background:#2e2e2e;color:#eee;border-color:#555}
      .gts-llpill .llp-pos{color:#666;font-size:9px;font-weight:700;letter-spacing:.04em}
      .gts-llpill .llp-sal{color:#666;font-weight:700;font-size:10px}
      .gts-llpill.lock{background:rgba(76,175,80,0.18);color:#dff5df;border-color:#4caf50}
      .gts-llpill.lock .llp-pos,.gts-llpill.lock .llp-sal{color:#a9d9ab}
      .gts-llpill.excl{background:rgba(255,107,107,0.14);color:#ffdcdc;border-color:#ff6b6b;
        text-decoration:line-through}
      .gts-llpill.excl .llp-pos,.gts-llpill.excl .llp-sal{color:#e0a5a5}
      .gts-llchosen{margin-top:7px;font-size:11px;color:#ccc;line-height:1.6}
      .gts-llclear{color:#666;font-size:10px;font-weight:700;letter-spacing:.06em;
        cursor:pointer;margin-left:14px}
      .gts-llclear:hover{color:#ff6b6b}
    ")),
    conditionalPanel(
      condition = "output.ll_available == false",
      div(style = "text-align:center;padding:60px 40px;",
          icon("flask", class = "fa-3x", style = "color:#333;margin-bottom:20px;"),
          h4("Lineup Lab", style = "color:#FFE500;"),
          uiOutput("ll_unavailable_why"))
    ),
    conditionalPanel(
      condition = "output.ll_available == true",
      fluidRow(
        box(title = "Constraint", status = "warning", solidHeader = TRUE, width = 12,
            collapsible = TRUE,
            uiOutput("ll_controls"))
      ),
      uiOutput("ll_summary_ui"),
      uiOutput("ll_results_ui")
    )
  )
}

# ---------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------
# `helpers` carries the four functions that live in app.R's server scope
# (prepare_optimization_data, drop_invalid_classic, add_custom_metrics and the
# sim-results getter). Passing them keeps one definition of each rather than a
# second copy drifting in this file.
register_lineup_lab_observers <- function(input, output, session, rv, helpers) {

  ll_sport_ok <- reactive(isTRUE(rv$sport == "NFL_CLASSIC"))

  # Platforms whose NORMAL pool has already been built. The Lab adds to the
  # same portfolio, and the Portfolio Builder only renders a platform tab once
  # that platform has optimal lineups -- so offering the Lab first would put
  # lineups somewhere the user cannot see them.
  ll_platforms <- reactive({
    Filter(function(p) !is.null(rv[[paste0(tolower(p), "_optimal_lineups")]]),
           c("DK", "FD"))
  })

  output$ll_available <- reactive(ll_sport_ok() && length(ll_platforms()) > 0)
  outputOptions(output, "ll_available", suspendWhenHidden = FALSE)

  output$ll_unavailable_why <- renderUI({
    msg <- if (!ll_sport_ok())
      "Lineup Lab is NFL classic only for now. Load an NFL classic sheet to use it."
    else
      paste("Run a simulation and build the normal lineup pool first --",
            "the Lab re-solves that finished sim under your constraint.")
    p(msg, style = "color:#555;font-size:14px;margin-top:10px;")
  })

  # ---- controls ------------------------------------------------------------
  # Filter down to a game / position / name, then click the player. Two
  # multi-selectize boxes were the first cut and they were the wrong control:
  # a DFS player picks a game stack or a position group, not a name out of an
  # alphabetical list of 317. The pill language (green = in, red = out) is the
  # one already used by the exposure tables and the team-split row, so there is
  # nothing new to learn.

  # Player -> "AWAY @ HOME", so a game chip can filter the board.
  ll_team_game <- reactive({
    g <- tryCatch(as.data.table(rv$input_data$game), error = function(e) NULL)
    if (is.null(g) || !nrow(g) || !all(c("away", "home") %in% names(g)))
      return(character(0))
    lab <- paste0(g$away, " @ ", g$home)
    setNames(rep(lab, 2), c(g$away, g$home))
  })

  ll_players <- reactive({
    md <- rv$sim_metadata
    req(md)
    md <- as.data.table(md)
    plat <- .ll_or(input$ll_platform, ll_platforms()[1])
    sal_col <- paste0(plat, "Salary")
    if (!sal_col %in% names(md)) sal_col <- "DKSalary"
    d <- md[!is.na(get(sal_col)) & get(sal_col) > 0,
            .(Player, Pos, Team, Salary = get(sal_col))]
    t2g <- ll_team_game()
    d[, Game := if (length(t2g)) t2g[as.character(Team)] else NA_character_]
    setorder(d, -Salary)
    d
  })

  # One chip. `val` is what the filter input is set to when clicked.
  .ll_chip <- function(label, val, input_id, active) {
    tags$span(class = paste("gts-llchip", if (identical(as.character(active), as.character(val))) "on" else ""),
              onclick = sprintf(
                "Shiny.setInputValue('%s',%s,{priority:'event'});",
                input_id, jsonlite::toJSON(val, auto_unbox = TRUE)),
              label)
  }

  output$ll_controls <- renderUI({
    plats <- ll_platforms()
    req(length(plats) > 0)
    d <- ll_players()
    games <- sort(unique(d$Game[!is.na(d$Game)]))
    f_game <- .ll_or(input$ll_f_game, "ALL")
    f_pos  <- .ll_or(input$ll_f_pos,  "ALL")

    fluidRow(
      column(
        7,
        div(style = "background-color:#2d2d2d;padding:10px;border-radius:4px;border:1px solid #404040;",
            div(style = "display:flex;justify-content:space-between;align-items:baseline;margin-bottom:6px;",
                h6("Build around these players",
                   style = "color:#FFE500;font-weight:bold;margin:0;font-size:13px;"),
                tags$span("click = lock · click again = keep out · again = clear",
                          style = "color:#777;font-size:10px;")),

            # Game chips.
            div(class = "gts-llchips",
                .ll_chip("ALL GAMES", "ALL", "ll_f_game", f_game),
                lapply(games, function(g) .ll_chip(g, g, "ll_f_game", f_game))),
            # Position chips.
            div(class = "gts-llchips", style = "margin-top:5px;",
                .ll_chip("ALL", "ALL", "ll_f_pos", f_pos),
                lapply(c("QB", "RB", "WR", "TE", "DST"),
                       function(p) .ll_chip(p, p, "ll_f_pos", f_pos))),
            div(style = "margin-top:6px;",
                tags$input(id = "ll_f_search", type = "text", class = "gts-llsearch",
                           placeholder = "or type a name...",
                           value = .ll_or(input$ll_f_search, ""),
                           oninput = "Shiny.setInputValue('ll_f_search',this.value,{priority:'event'});")),

            uiOutput("ll_board"),
            uiOutput("ll_chosen")
        )
      ),
      column(
        3,
        div(style = "background-color:#2d2d2d;padding:10px;border-radius:4px;border:1px solid #404040;",
            h6("Which sims count",
               style = "color:#FFE500;font-weight:bold;margin:0 0 6px 0;font-size:13px;"),
            sliderInput("ll_cond", NULL, min = 5, max = 100, value = 30, step = 5,
                        post = "%", width = "100%"),
            p(textOutput("ll_cond_hint", inline = TRUE),
              style = "color:#888;font-size:11px;margin:4px 0 0 0;line-height:1.4;")
        )
      ),
      column(
        2,
        div(style = "background-color:#2d2d2d;padding:10px;border-radius:4px;border:1px solid #FFE500;",
            # One platform needs no chooser -- every read of input$ll_platform
            # falls back to plats[1] through .ll_or.
            if (length(plats) > 1)
              radioButtons("ll_platform", "Site:", choices = plats,
                           selected = .ll_or(input$ll_platform, plats[1]), inline = TRUE),
            numericInput("ll_pool", "Pool size:", value = 300, min = 20, max = 5000,
                         step = 50, width = "100%"),
            actionButton("ll_run", "BUILD POOL", class = "btn-primary",
                         style = "width:100%;font-weight:bold;margin-top:4px;")
        )
      )
    )
  })

  # The pill board: whatever survives the three filters, richest first.
  output$ll_board <- renderUI({
    d <- ll_players()
    f_game <- .ll_or(input$ll_f_game, "ALL")
    f_pos  <- .ll_or(input$ll_f_pos,  "ALL")
    q      <- trimws(.ll_or(input$ll_f_search, ""))
    if (!identical(f_game, "ALL")) d <- d[Game == f_game]
    if (!identical(f_pos,  "ALL")) d <- d[Pos  == f_pos]
    if (nzchar(q)) d <- d[grepl(q, Player, ignore.case = TRUE, fixed = FALSE)]
    if (!nrow(d))
      return(div(class = "gts-llboard",
                 tags$span(style = "color:#666;font-size:11px;", "No players match.")))
    # A whole slate is ~317 pills. Cap the DOM and say so rather than rendering
    # a wall -- any real pick is reachable in one chip click or three keystrokes.
    capped <- nrow(d) > 120L
    if (capped) d <- head(d, 120L)
    lk <- .ll_or(rv$ll_lock_set, character(0))
    ex <- .ll_or(rv$ll_excl_set, character(0))
    pills <- lapply(seq_len(nrow(d)), function(i) {
      p  <- d$Player[i]
      st <- if (p %chin% lk) "lock" else if (p %chin% ex) "excl" else ""
      tags$span(
        class = paste("gts-llpill", st), title = paste0(d$Pos[i], " ", d$Team[i]),
        onclick = sprintf(
          "Shiny.setInputValue('ll_pill',{p:%s,nonce:Math.random()},{priority:'event'});",
          jsonlite::toJSON(p, auto_unbox = TRUE)),
        tags$span(class = "llp-pos", d$Pos[i]),
        p,
        tags$span(class = "llp-sal", sprintf("%.1f", d$Salary[i] / 1000)))
    })
    tagList(
      div(class = "gts-llboard", pills),
      if (capped)
        tags$span(style = "color:#666;font-size:10px;",
                  sprintf("showing the 120 highest-salary matches — pick a game or type a name to narrow")))
  })

  # What is currently set, always visible so a pill colour is never the only
  # record of a constraint.
  output$ll_chosen <- renderUI({
    lk <- .ll_or(rv$ll_lock_set, character(0))
    ex <- .ll_or(rv$ll_excl_set, character(0))
    if (!length(lk) && !length(ex)) return(NULL)
    div(class = "gts-llchosen",
        if (length(lk)) tags$span(tags$b("LOCKED: ", style = "color:#4caf50;"),
                                  paste(lk, collapse = ", ")),
        if (length(ex)) tags$span(style = "margin-left:14px;",
                                  tags$b("OUT: ", style = "color:#ff6b6b;"),
                                  paste(ex, collapse = ", ")),
        tags$span(class = "gts-llclear",
                  onclick = "Shiny.setInputValue('ll_clear',Math.random(),{priority:'event'});",
                  "clear"))
  })

  # off -> lock -> keep out -> off. One big click target beats a 15px x.
  observeEvent(input$ll_pill, {
    p <- input$ll_pill$p
    req(p)
    lk <- .ll_or(rv$ll_lock_set, character(0))
    ex <- .ll_or(rv$ll_excl_set, character(0))
    if (p %chin% lk) {
      rv$ll_lock_set <- setdiff(lk, p); rv$ll_excl_set <- union(ex, p)
    } else if (p %chin% ex) {
      rv$ll_excl_set <- setdiff(ex, p)
    } else {
      rv$ll_lock_set <- union(lk, p); rv$ll_excl_set <- setdiff(ex, p)
    }
  })

  observeEvent(input$ll_clear, {
    rv$ll_lock_set <- character(0); rv$ll_excl_set <- character(0)
  })

  output$ll_cond_hint <- renderText({
    f <- .ll_or(input$ll_cond, 30)
    if (f >= 100)
      "Every sim counts, including the ones where the lock busted."
    else
      sprintf(paste("Only the %d%% of sims where the locked players did best.",
                    "The other slots get chosen in that world."), f)
  })

  # ---- the solve -----------------------------------------------------------
  observeEvent(input$ll_run, {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    plat <- .ll_or(input$ll_platform, ll_platforms()[1])
    lock <- .ll_or(rv$ll_lock_set, character(0))
    if (!length(lock)) {
      showNotification("Pick at least one player to lock.", type = "warning")
      return()
    }
    rv$ll_results <- NULL
    # A refine from the previous pool must not silently shrink the new one.
    rv$ll_pool_lock <- character(0); rv$ll_pool_excl <- character(0)
    progress <- Progress$new(session); on.exit(progress$close())
    progress$set(message = "Re-solving the sim under your lock...", value = 0)

    tryCatch({
      md <- as.data.table(rv$sim_metadata)
      opt_data <- helpers$prepare_optimization_data(rv$simulation_results, md, plat)
      opt_data <- merge(opt_data, md[, .(Player, Pos)], by = "Player", all.x = TRUE)
      # Only players the site will actually accept an upload for.
      id_col <- paste0(plat, "ID")
      if (id_col %in% names(md)) {
        live <- md[!is.na(get(id_col)) & get(id_col) != "" & get(id_col) != "NA", Player]
        opt_data <- opt_data[Player %in% live]
      }

      cfg <- list(
        salary_cap     = rv$config$salary_caps[[plat]],
        roster_size    = rv$config$roster_sizes[[plat]],
        percentiles    = c(0.01, 0.05, 0.10, 0.20),
        platform_col   = paste0(plat, "Score"),
        max_lineups    = as.integer(.ll_or(input$ll_pool, 300)),
        lock_players   = lock,
        exclude_players = .ll_or(rv$ll_excl_set, character(0)),
        cond_frac      = .ll_or(input$ll_cond, 30) / 100,
        use_parallel   = TRUE,
        pool_spread    = .ll_or(rv$config$pool_spread, 0)
      )

      progress$set(detail = "Phase 1: constrained solve...", value = 0.1)
      ld <- find_optimal_lineups_nfl_classic_locked(opt_data, cfg, verbose = TRUE)

      # Same DK/FD classic legality rule the normal pool gets: >= 2 teams and
      # >= 2 games. A tight lock can otherwise leave a one-game roster.
      gtab <- tryCatch(as.data.table(rv$input_data$game), error = function(e) NULL)
      if (!is.null(gtab) && nrow(gtab))
        ld <- helpers$drop_invalid_classic(ld, md,
                gtab[, .(AwayTeam = away, HomeTeam = home)])
      if (!nrow(ld$unique_lineups))
        stop("every lineup with that lock used only one game -- loosen it")

      progress$set(detail = sprintf("Phase 2: scoring %s lineups...",
                                    format(nrow(ld$unique_lineups), big.mark = ",")),
                   value = 0.45)

      # Two things have to be true for a Lab number to mean what the same
      # number means on the main pool.
      #
      # 1. SAME SIMS. Scored against the full sim table, never the conditioned
      #    subset -- otherwise every rate is measured inside the slice of the
      #    world that was selected for the lock doing well.
      # 2. SAME FIELD. Win% and Top n% are POOL-RELATIVE: score_all_lineups
      #    ranks each lineup against the others in its own matrix. A 300-lineup
      #    Lab pool scored alone posts far better rates purely for having less
      #    competition -- measured on the W2 sheet, best WinRate 0.78% alone vs
      #    0.055% for the best lock-containing lineup in the 5,000 pool, a ~14x
      #    artefact and not an edge.
      #
      # The fast way to get (2) is the cached field: when the main pool was
      # scored, field_reference() kept its per-sim best score and percentile
      # cut-offs (~800KB at 20k sims). Scoring only this pool against those is
      # the same comparison for ~1/17th of the work, because it stops
      # re-scoring 5,000 lineups the app already scored minutes ago.
      #
      # Fall back to scoring the two pools in one matrix when there is no
      # cached field -- the memory-efficient scoring path never builds one, and
      # a sim count that no longer matches means the reference is stale.
      lkey <- function(d) {
        pcc <- grep("^Player[0-9]+$", names(d), value = TRUE)
        apply(as.matrix(d[, ..pcc]), 1L, function(r) paste(sort(r), collapse = "|"))
      }
      lab_ul <- ld$unique_lineups
      lab_k  <- lkey(lab_ul)
      pcx    <- grep("^Player[0-9]+$", names(lab_ul), value = TRUE)
      keepc  <- intersect(c(pcx, "TotalSalary"), names(lab_ul))
      ref     <- rv[[paste0(tolower(plat), "_field_ref")]]
      main_ul <- rv[[paste0(tolower(plat), "_optimal_lineups")]]
      use_ref <- !is.null(ref) && identical(as.integer(ref$n_sims), as.integer(ld$n_sims))

      own <- copy(md)
      own_col <- paste0(plat, "Own")
      if (own_col %in% names(own)) {
        setnames(own, own_col, "Own")
        if (max(own$Own, na.rm = TRUE) > 1) own[, Own := Own / 100]
      }

      if (use_ref) {
        cd <- list(unique_lineups = lab_ul[, ..keepc], n_sims = ld$n_sims,
                   config = cfg, mode = ld$mode)
        sm <- score_all_lineups(cd, opt_data, verbose = TRUE)
        progress$set(detail = "Phase 3: metrics vs the main pool...", value = 0.75)
        # Everything except the rates comes from the normal metrics call (cheap
        # on a few hundred lineups); the rates themselves are then replaced with
        # the ones measured against the cached field.
        fr <- calculate_distribution_metrics(sm, cd, cfg, ownership_data = own, verbose = TRUE)
        if (is.matrix(sm)) {
          rt <- rates_vs_field(sm, ref)
          fr[, WinRate := rt$win_rate]
          pcols <- c("Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct")
          for (i in seq_along(ref$percentiles))
            if (pcols[i] %in% names(fr)) fr[, (pcols[i]) := rt$top_pcts[, i]]
        }
      } else {
        comb <- lab_ul[, ..keepc]
        if (!is.null(main_ul) && all(pcx %in% names(main_ul))) {
          mk <- lkey(main_ul)
          mc <- intersect(keepc, names(main_ul))
          comb <- rbindlist(list(comb, main_ul[!(mk %chin% lab_k), ..mc]),
                            use.names = TRUE, fill = TRUE)
        }
        cd <- list(unique_lineups = comb, n_sims = ld$n_sims, config = cfg, mode = ld$mode)
        sm <- score_all_lineups(cd, opt_data, verbose = TRUE)
        progress$set(detail = "Phase 3: metrics...", value = 0.75)
        fr <- calculate_distribution_metrics(sm, cd, cfg, ownership_data = own, verbose = TRUE)
        fr <- fr[lkey(fr) %chin% lab_k]        # drop the reference field again
      }
      if (!nrow(fr)) stop("scoring returned nothing for the constrained pool")
      fr <- helpers$add_custom_metrics(fr, md, rv$config)
      for (wc in intersect(c("TotalEW", "Win6Pct", "Win5PlusPct"), names(fr)))
        fr[, (wc) := NULL]

      # calculate_distribution_metrics drops Top1Count. Put it back as a shown
      # column -- but do NOT rank on it: on a big classic it is 1 for nearly
      # every lineup whether or not anything is locked (see the table in the
      # solver header), so sorting by it would just be noise wearing the shape
      # of a ranking. Top1Pct, which is measured across every sim, is the one
      # that actually orders the pool. Matched on the sorted player set rather
      # than row position, so it cannot mis-align if metrics ever reorders.
      if ("Top1Count" %in% names(ld$unique_lineups)) {
        src <- ld$unique_lineups
        fr[, Top1Count := src$Top1Count[match(lkey(fr), lkey(src))]]
      }
      setorder(fr, -Top1Pct, -WinRate)

      rv$ll_results  <- fr
      rv$ll_platform <- plat
      rv$ll_info     <- ld$lock_info
      showNotification(sprintf("Built %s lineups locking %s",
                               format(nrow(fr), big.mark = ","),
                               paste(lock, collapse = " + ")), type = "message")
    }, error = function(e) {
      rv$ll_results <- NULL
      showNotification(paste("Lineup Lab:", conditionMessage(e)),
                       type = "error", duration = 12)
    })
  })

  # ---- summary -------------------------------------------------------------
  output$ll_summary_ui <- renderUI({
    info <- rv$ll_info; res <- rv$ll_results
    if (is.null(info) || is.null(res)) return(NULL)
    chip <- function(lab, val) div(
      style = "display:inline-block;margin-right:22px;",
      span(lab, style = "color:#888;font-size:11px;display:block;"),
      span(val, style = "color:#FFE500;font-size:17px;font-weight:bold;"))
    fluidRow(box(
      status = "primary", solidHeader = FALSE, width = 12,
      div(style = "padding:4px 2px;",
          chip("LOCKED", paste(info$locked, collapse = " + ")),
          chip("SITE", rv$ll_platform),
          chip("SIMS USED", sprintf("%s (top %.0f%%)",
                                    format(info$n_cond_sims, big.mark = ","),
                                    info$cond_frac * 100)),
          chip("LINEUPS", format(nrow(res), big.mark = ",")),
          if (length(info$excluded))
            chip("KEPT OUT", paste(info$excluded, collapse = ", "))
      )
    ))
  })

  # ---- filters -------------------------------------------------------------
  ll_filtered <- reactive({
    res <- rv$ll_results
    if (is.null(res) || !nrow(res)) return(NULL)
    out <- copy(res)
    for (f in .LL_MIN_FILTERS) {
      v <- input[[paste0("ll_min_", f[1])]]
      if (!is.null(v) && v > 0 && f[3] %in% names(out))
        out <- out[get(f[3]) >= v]
    }
    # Row-level LOCK / EXCL from the exposure table, applied to the built
    # pool. Cheap set membership over the nine slot columns.
    pcz <- grep("^Player[0-9]+$", names(out), value = TRUE)
    plk <- .ll_or(rv$ll_pool_lock, character(0))
    pex <- .ll_or(rv$ll_pool_excl, character(0))
    if (nrow(out) && (length(plk) || length(pex))) {
      mm <- as.matrix(out[, ..pcz])
      keep <- rep(TRUE, nrow(out))
      if (length(plk)) keep <- keep & apply(mm, 1L, function(r) all(plk %chin% r))
      if (length(pex)) keep <- keep & apply(mm, 1L, function(r) !any(pex %chin% r))
      out <- out[keep]
    }
    out
  })

  output$ll_filtered_count <- renderText({
    n <- if (is.null(ll_filtered())) 0L else nrow(ll_filtered())
    paste0("Filtered: ", format(n, big.mark = ","), " lineups")
  })

  output$ll_results_ui <- renderUI({
    if (is.null(rv$ll_results)) return(NULL)
    tagList(
      fluidRow(box(
        title = "Filter & Add", status = "warning", solidHeader = TRUE, width = 12,
        collapsible = TRUE,
        fluidRow(
          column(8, div(
            style = "background-color:#2d2d2d;padding:8px;border-radius:4px;border:1px solid #404040;",
            h6("Min Rates", style = "color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
            div(style = "display:flex;flex-wrap:wrap;gap:10px;",
                lapply(.LL_MIN_FILTERS, function(f)
                  div(style = "display:flex;align-items:center;",
                      tags$label(paste0(f[2], ":"),
                                 style = "color:#FFE500;font-size:11px;margin:0 5px 0 0;"),
                      numericInput(paste0("ll_min_", f[1]), NULL, value = 0, min = 0,
                                   max = 100, step = as.numeric(f[4]), width = "68px"))))
          )),
          column(4, div(
            style = "background-color:#2d2d2d;padding:8px;border-radius:4px;border:1px solid #FFE500;",
            h6("Add to Portfolio",
               style = "color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
            numericInput("ll_num_lineups", "Lineups:", value = 20, min = 1, max = 150,
                         width = "100%"),
            textInput("ll_build_label", "Label:", value = "", placeholder = "Optional",
                      width = "100%"),
            h5(textOutput("ll_filtered_count"),
               style = "color:#FFE500;font-weight:bold;text-align:center;margin:8px 0 6px 0;"),
            actionButton("ll_add_build", "ADD TO PORTFOLIO", class = "btn-primary",
                         style = "width:100%;font-weight:bold;")
          ))
        )
      )),
      fluidRow(box(title = "Player Exposure in Filtered Pool", status = "info",
                   solidHeader = TRUE, width = 12, collapsible = TRUE,
                   DTOutput("ll_exposure"))),
      fluidRow(box(title = "Lineups", status = "primary", solidHeader = TRUE, width = 12,
                   DTOutput("ll_lineups")))
    )
  })

  # ---- exposure ------------------------------------------------------------
  # LOCK / EXCL here refine the pool that was already BUILT -- no re-solve.
  # Same two-button idiom as the main exposure table, and the same meaning:
  # keep only lineups containing the player, or drop every lineup with him.
  # The constraint the SOLVE used is the pill board above; this is the cheap
  # second pass you do after seeing who actually showed up.
  .ll_btn <- function(players, active, kind) {
    cls  <- if (kind == "lock") "gts-lock" else "gts-excl"
    icon <- if (kind == "lock") '<i class="fa-solid fa-lock"></i>'
            else                '<i class="fa-solid fa-xmark"></i>'
    ifelse(players %in% active,
           paste0('<span class="gts-btn ', cls, ' on">',  icon, '</span>'),
           paste0('<span class="gts-btn ', cls, ' off">', icon, '</span>'))
  }

  output$ll_exposure <- renderDT({
    d <- ll_filtered(); md <- rv$sim_metadata
    req(d, md, nrow(d) > 0)
    md <- as.data.table(md)
    plat <- .ll_or(rv$ll_platform, "DK")
    pc <- grep("^Player[0-9]+$", names(d), value = TRUE)
    tab <- table(unlist(d[, ..pc], use.names = FALSE))
    ex <- data.table(Player = names(tab), Exp = as.numeric(tab) / nrow(d) * 100)
    own_col <- paste0(plat, "Own"); sal_col <- paste0(plat, "Salary")
    cols <- c("Player", "Pos", "Team", intersect(c(sal_col, own_col), names(md)))
    ex <- merge(ex, md[, ..cols], by = "Player", all.x = TRUE)
    if (own_col %in% names(ex)) {
      setnames(ex, own_col, "Own")
      # Sheets carry ownership either as a percent or as a fraction. Exp is a
      # percent, so normalise before differencing -- otherwise Lev is just Exp
      # (0.035 vs 31.3 gave Lev 100 on the W2 sheet). Same guard, same reason,
      # as the main exposure table at app.R:3109.
      if (max(ex$Own, na.rm = TRUE) <= 1) ex[, Own := Own * 100]
      ex[, Own := round(Own, 1)]
      ex[, Lev := round(Exp - Own, 1)]
    }
    if (sal_col %in% names(ex)) setnames(ex, sal_col, "Salary")
    ex[, Exp := round(Exp, 1)]
    setorder(ex, -Exp)

    plk <- .ll_or(rv$ll_pool_lock, character(0))
    pex <- .ll_or(rv$ll_pool_excl, character(0))
    lk  <- .ll_or(rv$ll_info$locked,   character(0))
    xk  <- .ll_or(rv$ll_info$excluded, character(0))
    ex[, LOCK := .ll_btn(Player, plk, "lock")]
    ex[, EXCL := .ll_btn(Player, pex, "excl")]
    # Players the SOLVE was built around are marked so the 100% rows read as
    # "by construction", not as a pool finding.
    ex[, Player := fifelse(Player %chin% lk, paste0("★ ", Player), Player)]
    ex[, .rowstate := fifelse(gsub("^★ ", "", Player) %chin% plk, "lock",
                       fifelse(gsub("^★ ", "", Player) %chin% pex, "excl", ""))]

    keep <- intersect(c("LOCK", "EXCL", "Player", "Pos", "Team", "Salary",
                        "Exp", "Own", "Lev", ".rowstate"), names(ex))
    ex <- ex[, ..keep]
    nm <- names(ex)
    ixof <- function(cn) if (cn %in% nm) which(nm == cn) - 1L else integer(0)
    defs <- list(
      list(targets = as.list(ixof("LOCK")), className = "gts-btncell",
           searchable = FALSE, orderable = FALSE),
      list(targets = as.list(ixof("EXCL")), className = "gts-btncell gts-exclcell",
           searchable = FALSE, orderable = FALSE),
      list(targets = as.list(ixof(".rowstate")), visible = FALSE, searchable = FALSE))
    esc <- setdiff(seq_along(nm), match(c("LOCK", "EXCL"), nm))
    cap_txt <- if (length(lk))
      sprintf("★ = built around (locked into the solve): %s%s",
              paste(lk, collapse = ", "),
              if (length(xk)) sprintf("  ·  kept out of the solve: %s",
                                      paste(xk, collapse = ", ")) else "")
      else "Click the lock or x on any row to refine this pool -- no re-solve."
    datatable(ex, rownames = FALSE, selection = "none", escape = esc,
              caption = cap_txt,
              options = list(pageLength = 15, dom = "tip", scrollX = TRUE,
                             columnDefs = defs),
              callback = DT::JS(paste0(
                "table.on('click','td.gts-btncell',function(){",
                  "var d=table.row(this).data(); if(!d) return;",
                  "var act=$(this).hasClass('gts-exclcell')?'excl':'lock';",
                  "Shiny.setInputValue('ll_pool_toggle',",
                    "{player:d[", which(nm == "Player") - 1L, "],action:act,",
                     "nonce:Math.random()},{priority:'event'});",
                "});")))
  })

  # Setting one clears the other, so a player is never both.
  observeEvent(input$ll_pool_toggle, {
    tg <- input$ll_pool_toggle; req(tg$player)
    p <- sub("^★ ", "", tg$player)
    lk <- .ll_or(rv$ll_pool_lock, character(0))
    ex <- .ll_or(rv$ll_pool_excl, character(0))
    if (identical(tg$action, "lock")) {
      rv$ll_pool_lock <- if (p %chin% lk) setdiff(lk, p) else union(lk, p)
      rv$ll_pool_excl <- setdiff(ex, p)
    } else {
      rv$ll_pool_excl <- if (p %chin% ex) setdiff(ex, p) else union(ex, p)
      rv$ll_pool_lock <- setdiff(lk, p)
    }
  })

  observeEvent(input$ll_pool_clear, {
    rv$ll_pool_lock <- character(0); rv$ll_pool_excl <- character(0)
  })
  # ---- lineups -------------------------------------------------------------
  output$ll_lineups <- renderDT({
    d <- ll_filtered()
    req(d, nrow(d) > 0)
    pc <- grep("^Player[0-9]+$", names(d), value = TRUE)
    show <- intersect(c(pc, "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
                        "TotalSalary", "AvgOwn"), names(d))
    out <- copy(d[, ..show])
    ren <- c(WinRate = "Win", Top1Pct = "Top1", Top5Pct = "Top5", Top10Pct = "Top10",
             Top20Pct = "Top20", TotalSalary = "Salary",
             AvgOwn = "Own")
    for (nm in intersect(names(ren), names(out))) setnames(out, nm, ren[[nm]])
    dt <- datatable(out, rownames = FALSE, selection = "none",
                    options = list(pageLength = 25, dom = "tip", scrollX = TRUE))
    rc <- intersect(c("Win", "Top1", "Top5", "Top10", "Top20", "Own"), names(out))
    if (length(rc)) dt <- formatRound(dt, rc, 2)
    dt
  })

  # ---- add to portfolio ----------------------------------------------------
  # Deliberately the same shape as the normal Add Build: a random draw from the
  # filtered pool, tagged with a build label. The lineups land in the SAME
  # portfolio object, so a portfolio can mix normal and constrained builds and
  # the exposure/download views treat them alike.
  observeEvent(input$ll_add_build, {
    d <- ll_filtered()
    req(d)
    lp <- tolower(.ll_or(rv$ll_platform, "DK"))
    n  <- as.integer(.ll_or(input$ll_num_lineups, 20))
    if (nrow(d) < n) {
      showNotification(paste0("Only ", nrow(d), " available."), type = "warning")
      return()
    }
    sampled <- d[sample(nrow(d), n)]
    cnt <- paste0(lp, "_build_counter")
    rv[[cnt]] <- .ll_or(rv[[cnt]], 0) + 1
    raw <- input$ll_build_label
    lbl <- if (is.null(raw) || raw == "") {
      sprintf("Lab %d: %s (top %.0f%%)", rv[[cnt]],
              paste(rv$ll_info$locked, collapse = "+"), rv$ll_info$cond_frac * 100)
    } else iconv(raw, to = "UTF-8", sub = "")
    # A repeated label would merge two different builds into one row.
    bn <- paste0(lp, "_builds")
    if (lbl %in% names(rv[[bn]])) lbl <- paste0(lbl, " #", rv[[cnt]])
    sampled[, Build := lbl]
    pn <- paste0(lp, "_portfolio")
    rv[[pn]] <- if (is.null(rv[[pn]])) sampled
                else rbindlist(list(rv[[pn]], sampled), fill = TRUE)
    parts <- c()
    for (f in .LL_MIN_FILTERS) {
      v <- input[[paste0("ll_min_", f[1])]]
      if (!is.null(v) && v > 0) parts <- c(parts, paste0(f[2], ">=", v))
    }
    desc <- paste0("LOCK ", paste(rv$ll_info$locked, collapse = "+"),
                   " | top ", round(rv$ll_info$cond_frac * 100), "% sims",
                   if (length(parts)) paste0(" | ", paste(parts, collapse = " ")) else "")
    rv[[bn]][[lbl]] <- list(label = lbl, num_lineups = n, filters = desc)
    showNotification(sprintf("Added %d lineups to the %s portfolio as '%s'",
                             n, toupper(lp), lbl), type = "message")
    updateTextInput(session, "ll_build_label", value = "")
  })
}
