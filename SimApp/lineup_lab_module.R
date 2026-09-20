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
  # Choices are rebuilt whenever the sim changes so a stale slate's names can
  # never be locked into a new one.
  ll_choices <- reactive({
    md <- rv$sim_metadata
    req(md)
    md <- as.data.table(md)
    plat <- .ll_or(input$ll_platform, ll_platforms()[1])
    sal_col <- paste0(plat, "Salary")
    if (!sal_col %in% names(md)) sal_col <- "DKSalary"
    d <- md[!is.na(get(sal_col)) & get(sal_col) > 0]
    setorderv(d, sal_col, -1L)
    setNames(d$Player,
             sprintf("%s  (%s, %s, $%s)", d$Player, d$Pos, d$Team,
                     format(d[[sal_col]], big.mark = ",", trim = TRUE)))
  })

  output$ll_controls <- renderUI({
    plats <- ll_platforms()
    req(length(plats) > 0)
    fluidRow(
      column(
        5,
        div(style = "background-color:#2d2d2d;padding:10px;border-radius:4px;border:1px solid #404040;",
            h6("Lock into every lineup",
               style = "color:#FFE500;font-weight:bold;margin:0 0 6px 0;font-size:13px;"),
            selectizeInput("ll_lock", NULL, choices = ll_choices(), multiple = TRUE,
                           width = "100%",
                           options = list(placeholder = "Pick one or more players...")),
            h6("Keep out",
               style = "color:#FFE500;font-weight:bold;margin:8px 0 6px 0;font-size:13px;"),
            selectizeInput("ll_excl", NULL, choices = ll_choices(), multiple = TRUE,
                           width = "100%",
                           options = list(placeholder = "Optional..."))
        )
      ),
      column(
        4,
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
        3,
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
    lock <- .ll_or(input$ll_lock, character(0))
    if (!length(lock)) {
      showNotification("Pick at least one player to lock.", type = "warning")
      return()
    }
    rv$ll_results <- NULL
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
        exclude_players = .ll_or(input$ll_excl, character(0)),
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
      #    artefact and not an edge. These lineups go into the SAME portfolio as
      #    normal builds, so they are scored against the main pool as the
      #    reference field and only the Lab rows are kept afterwards.
      lkey <- function(d) {
        pcc <- grep("^Player[0-9]+$", names(d), value = TRUE)
        apply(as.matrix(d[, ..pcc]), 1L, function(r) paste(sort(r), collapse = "|"))
      }
      lab_ul <- ld$unique_lineups
      lab_k  <- lkey(lab_ul)
      pcx    <- grep("^Player[0-9]+$", names(lab_ul), value = TRUE)
      keepc  <- intersect(c(pcx, "TotalSalary"), names(lab_ul))
      comb   <- lab_ul[, ..keepc]
      main_ul <- rv[[paste0(tolower(plat), "_optimal_lineups")]]
      if (!is.null(main_ul) && all(pcx %in% names(main_ul))) {
        mk <- lkey(main_ul)
        mc <- intersect(keepc, names(main_ul))
        comb <- rbindlist(list(comb, main_ul[!(mk %chin% lab_k), ..mc]),
                          use.names = TRUE, fill = TRUE)
      }
      cd <- list(unique_lineups = comb, n_sims = ld$n_sims, config = cfg, mode = ld$mode)
      sm <- score_all_lineups(cd, opt_data, verbose = TRUE)

      progress$set(detail = "Phase 3: metrics...", value = 0.75)
      own <- copy(md)
      own_col <- paste0(plat, "Own")
      if (own_col %in% names(own)) {
        setnames(own, own_col, "Own")
        if (max(own$Own, na.rm = TRUE) > 1) own[, Own := Own / 100]
      }
      fr <- calculate_distribution_metrics(sm, cd, cfg, ownership_data = own, verbose = TRUE)
      fr <- fr[lkey(fr) %chin% lab_k]          # drop the reference field again
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
          chip("MOST REPEATED",
               if ("Top1Count" %in% names(res))
                 format(max(res$Top1Count), big.mark = ",") else "-"),
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
          column(5, div(
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
          column(3, div(
            style = "padding-left:4px;",
            h6("Note", style = "color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
            p(paste("Win%/Top% are measured over every sim AND against the",
                    "main pool as the competing field, so they mean the same",
                    "thing here as on the normal pool. Rank on those.",
                    "'Repeats' is near-constant on a big slate -- shown for",
                    "information, not for sorting."),
              style = "color:#888;font-size:11px;line-height:1.5;")
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
  output$ll_exposure <- renderDT({
    d <- ll_filtered(); md <- rv$sim_metadata
    req(d, md, nrow(d) > 0)
    md <- as.data.table(md)
    plat <- .ll_or(rv$ll_platform, "DK")
    pc <- grep("^Player[0-9]+$", names(d), value = TRUE)
    tab <- table(unlist(d[, ..pc], use.names = FALSE))
    ex <- data.table(Player = names(tab), Exp = as.numeric(tab) / nrow(d) * 100)
    own_col <- paste0(plat, "Own"); sal_col <- paste0(plat, "Salary")
    cols <- c("Player", "Pos", "Team",
              intersect(c(sal_col, own_col), names(md)))
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
    # Locked players sit at 100% by construction; flag rather than hide them so
    # the table still reconciles to the roster.
    lk <- .ll_or(rv$ll_info$locked, character(0))
    ex[, Player := fifelse(Player %chin% lk, paste0("* ", Player), Player)]
    keep <- intersect(c("Player", "Pos", "Team", "Salary", "Exp", "Own", "Lev"), names(ex))
    datatable(ex[, ..keep], rownames = FALSE, selection = "none",
              options = list(pageLength = 15, dom = "tip", scrollX = TRUE),
              caption = "* = locked into every lineup")
  })

  # ---- lineups -------------------------------------------------------------
  output$ll_lineups <- renderDT({
    d <- ll_filtered()
    req(d, nrow(d) > 0)
    pc <- grep("^Player[0-9]+$", names(d), value = TRUE)
    show <- intersect(c(pc, "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
                        "Top1Count", "TotalSalary", "AvgOwn"), names(d))
    out <- copy(d[, ..show])
    ren <- c(WinRate = "Win", Top1Pct = "Top1", Top5Pct = "Top5", Top10Pct = "Top10",
             Top20Pct = "Top20", Top1Count = "Repeats", TotalSalary = "Salary",
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
