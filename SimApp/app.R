# ============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(readxl)
library(plotly)
library(ggplot2)

source("sport_configs_universal.R")
source("OptimalLineups_Core.R")
source("portfolio_helpers_universal.R")
source("cash_game_module.R")

# Source all sport engines once at startup.
# Never re-source inside reactive observers — re-sourcing re-executes all
# top-level code on every upload/sim run.
local({
  engines <- c("nascar", "mma", "tennis", "golf", "f1", "nfl", "cbb", "nba", "soccer")
  for (e in engines) {
    f <- paste0(e, "_engine.R")
    if (file.exists(f)) source(f) else warning("Engine not found at startup: ", f)
  }
})


# ============================================================================
# HELPERS (outside server so they are available at parse time)
# ============================================================================

# Config-driven input loader.
# Sports with dedicated read_*_input() functions use those (Golf, F1, CBB).
# All other sports are handled generically from config$input_file fields.
load_sport_input <- function(file_path, sport, config) {
  reader_map <- list(
    GOLF = read_golf_input,
    F1   = read_f1_input,
    CBB  = read_cbb_input,
    NBA  = read_nba_input,
    SOCCER = read_soccer_input
  )
  if (sport %in% names(reader_map)) {
    return(reader_map[[sport]](file_path))
  }
  input_cfg <- config$input_file
  if (isTRUE(input_cfg$load_all_sheets)) {
    sheets <- readxl::excel_sheets(file_path)
    return(setNames(lapply(sheets, function(s) readxl::read_excel(file_path, sheet = s)), sheets))
  }
  sheets <- if (!is.null(input_cfg$required_sheets)) input_cfg$required_sheets else readxl::excel_sheets(file_path)[1]
  setNames(lapply(sheets, function(s) readxl::read_excel(file_path, sheet = s)), sheets)
}

# FontAwesome icon name per sport (used in the sport info badge)
sport_icon_name <- function(sport) {
  switch(sport,
         NASCAR = "flag-checkered",
         MMA    = "fist-raised",
         TENNIS = "table-tennis",
         GOLF   = "golf-ball",
         F1     = "car",
         NFL    = "football-ball",
         CBB    = "basketball-ball",
         NBA    = "basketball-ball",
         SOCCER = "futbol",
         "circle"
  )
}


# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  skin = "black",
  
  # Logo serves as the brand — no text title needed
  dashboardHeader(
    title = tags$span(),
    titleWidth = 200
  ),
  
  dashboardSidebar(
    width = 200,
    tags$div(
      style = "padding:15px;text-align:center;background:#000;border-bottom:2px solid #FFE500;",
      tags$img(src = "logo.jpg", width = "160px", id = "gts_logo")
    ),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Data Input",         tabName = "input",       icon = icon("file-upload")),
      menuItem("Sim Results",        tabName = "sim_results", icon = icon("chart-bar")),
      menuItem("Tournament Lineups", tabName = "scoring",     icon = icon("trophy")),
      menuItem("Cash Games",         tabName = "cash_games",  icon = icon("coins")),
      menuItem("Portfolio Builder",  tabName = "portfolio",   icon = icon("layer-group"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "gts_theme.css")),
    
    tags$style(HTML("
      .box-primary > .box-header,.box.box-primary > .box-header,.box-solid.box-primary > .box-header{background-color:#2d2d2d!important;color:#FFE500!important;border-bottom:2px solid #FFE500!important}
      .box-primary .box-title{color:#FFE500!important;font-weight:600!important}
      .box-primary,.box.box-primary{border-top-color:#404040!important;border-color:#404040!important}
      .box-warning>.box-header{background-color:#2d2d2d!important;color:#FFE500!important;border-color:#FFE500!important}
      .box-warning .box-title{color:#FFE500!important;font-weight:600!important}
      .box-info>.box-header,.box.box-info>.box-header{background-color:#2d2d2d!important;color:#FFE500!important;border-bottom:2px solid #FFE500!important}
      .box-info .box-title{color:#FFE500!important}
      .box-info,.box.box-info{border-top-color:#404040!important;border-color:#404040!important;background-color:#1e1e1e!important}
      .box-warning{background-color:#1e1e1e!important;border:1px solid #FFE500!important}
      .panel-warning>.panel-heading,.panel-heading,.panel-default>.panel-heading{background-color:#2d2d2d!important;color:#FFE500!important;border-color:#FFE500!important}
      .panel-title,.panel-heading h4{color:#FFE500!important;font-weight:600!important}
      .irs-bar,.irs-bar-edge,.irs-handle,.irs--flat .irs-bar,.irs--modern .irs-bar,.irs--round .irs-bar,.irs--flat .irs-handle,.irs--modern .irs-handle,.irs--round .irs-handle{background:#FFE500!important;border-color:#D4B000!important}
      .irs-from,.irs-to,.irs-single,.irs--flat .irs-from,.irs--flat .irs-to,.irs--flat .irs-single,.irs--modern .irs-from,.irs--modern .irs-to,.irs--modern .irs-single{background:#FFE500!important;color:#000000!important;font-weight:600!important}
      .irs-line{background-color:#404040!important}
      .irs-grid-text{color:#999999!important}
      .btn-primary,.btn-primary:hover,.btn-primary:focus,.btn-primary:active{background-color:#FFE500!important;color:#000000!important;border-color:#D4B000!important;font-weight:600!important}
      .btn-warning,.btn-warning:hover,.btn-warning:focus,.btn-warning:active{background-color:#FFE500!important;color:#000000!important;border-color:#D4B000!important;font-weight:600!important}
      .selectize-input{background-color:#1e1e1e!important;border:1px solid #404040!important;color:#ffffff!important}
      .selectize-input.focus{border-color:#FFE500!important}
      .selectize-input .item{background:#FFE500!important;color:#000000!important;font-weight:600!important}
      .selectize-dropdown{background:#1e1e1e!important;border:1px solid #FFE500!important}
      .selectize-dropdown .option{color:#ffffff!important}
      .selectize-dropdown .option:hover,.selectize-dropdown .option.active{background:#FFE500!important;color:#000000!important}
      .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{background-color:#1e1e1e!important;color:#FFE500!important;border:1px solid #FFE500!important;border-bottom-color:#1e1e1e!important}
      .nav-tabs>li>a{color:#cccccc!important;background-color:#2d2d2d!important;border:1px solid #404040!important;border-bottom:none!important}
      .nav-tabs>li>a:hover{background-color:#404040!important;border-color:#FFE500!important;color:#FFE500!important}
      .alert-info,.callout-info{background-color:rgba(255,229,0,0.15)!important;border-color:#FFE500!important;color:#FFF4B3!important}
      .alert-warning{background-color:rgba(255,229,0,0.15)!important;border-color:#FFE500!important;color:#FFF4B3!important}
      .progress-bar-primary,.progress-bar-info{background-color:#FFE500!important;color:#000000!important}
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover{background-color:#FFE500!important;color:#000000!important;border-color:#D4B000!important}
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover{background-color:#404040!important;color:#FFE500!important;border-color:#FFE500!important}
      .skin-black .main-header,.skin-black .main-header .navbar,.skin-black .main-header .logo{background-image:none!important;background-color:#000000!important}
      .skin-black .main-header::before,.skin-black .main-header::after,.skin-black .main-header .navbar::before,.skin-black .main-header .navbar::after{display:none!important;content:none!important}
      .skin-black .main-header{border-bottom:3px solid #FFE500!important;box-shadow:0 2px 4px rgba(0,0,0,0.3)!important}
      .content-wrapper{background-color:#121212!important;background-image:none!important}
      .content-wrapper::before,.content-wrapper::after{display:none!important;content:none!important}
      .box,.box-body,.box-header,.box-footer{background-image:none!important}
      .main-sidebar{background-image:none!important;background-color:#121212!important}
      .main-sidebar::before,.main-sidebar::after{display:none!important;content:none!important}
      .panel,.panel-body,.panel-heading,.well{background-image:none!important;background-color:#2d2d2d!important}
      .content-wrapper,.tab-content,.tab-pane{background-color:#121212!important;background-image:none!important}
      .tab-content>.tab-pane,.tab-content>.active{border:none!important;padding:0!important}
      .tab-pane>.row{margin:0!important}
      #portfolio *,#portfolio .box,#portfolio .box-body{background-image:none!important}
      #portfolio .tab-content,#portfolio .tab-pane{background-color:#121212!important;background-image:none!important}
      .tabbable,.tabbable .tab-content{background-color:transparent!important;background-image:none!important}
      .radio label,.checkbox label{color:#ffffff!important;font-weight:500!important}
      .form-control{background-color:#1e1e1e!important;color:#ffffff!important;border-color:#404040!important}
      .form-control:focus{background-color:#1e1e1e!important;border-color:#FFE500!important;box-shadow:0 0 0 2px rgba(255,229,0,0.2)!important}
      label{color:#ffffff!important}
      .shiny-spinner-message-container{background-color:rgba(0,0,0,0.9)!important;border:2px solid #FFE500!important;border-radius:8px!important}
      .shiny-spinner-message{color:#FFE500!important;font-weight:600!important}
      .delete-build,.delete-lineup{position:relative;z-index:100!important;pointer-events:auto!important;cursor:pointer!important}

      /* GTS colour system
         Gold  #FFE500  primary action / brand
         Blue  #4A90D9  informational / platform / status
         Red   #C0392B  errors / destructive only             */

      /* Remove AdminLTE header - logo is the brand */
      .main-header{display:none!important}
      .skin-black .wrapper,.skin-black .main-sidebar,.skin-black .left-side{padding-top:0!important;top:0!important}
      .content-wrapper,.main-footer{margin-left:200px!important;transition:margin-left .2s ease}
      .sidebar-collapse .content-wrapper,.sidebar-collapse .main-footer{margin-left:50px!important}
      .main-sidebar{top:0!important;height:100vh;transition:width .2s ease}
      .sidebar-collapse .main-sidebar{width:50px!important;overflow:hidden}
      .sidebar-collapse .sidebar-menu>li>a>span{display:none!important}
      .sidebar-collapse .sidebar-menu>li>a{padding:12px 15px!important}
      .sidebar-collapse #gts_logo{width:24px!important}
      .sidebar-menu>li>.treeview-menu{background:#0a0a0a}

      /* Sidebar collapse tab — fixed to right edge of sidebar */
      #gts_sb_tab{position:fixed;top:50%;left:200px;transform:translateY(-50%);z-index:9999;
        background:#111;border:1px solid #2a2a2a;border-left:none;border-radius:0 6px 6px 0;
        color:#444;font-size:13px;width:16px;height:48px;padding:0;cursor:pointer;line-height:48px;
        text-align:center;transition:left .2s ease,color .15s,background .15s,border-color .15s}
      #gts_sb_tab:hover{color:#FFE500;background:#1a1a1a;border-color:#444}
      .sidebar-collapse #gts_sb_tab{left:50px}

      /* Platform pill selector — used in sim results and lineup scoring */
      .gts-platform-pills{display:flex;align-items:center;gap:6px;flex-wrap:wrap}
      .gts-pill{height:32px;padding:0 16px;font-size:11px;font-weight:700;letter-spacing:.06em;background:#1a1a1a;color:#555;border:1px solid #2a2a2a;border-radius:16px;cursor:pointer;white-space:nowrap;transition:background .12s,color .12s,border-color .12s}
      .gts-pill:hover{background:#2a2a2a;color:#ccc;border-color:#444}
      .gts-pill.active{background:rgba(255,229,0,0.1);color:#FFE500;border-color:#FFE500}

      /* Sim results control bar */
      .gts-sr-bar{display:flex;align-items:center;gap:0;background:#141414;border:1px solid #222;border-radius:6px;overflow:hidden;margin-bottom:16px}
      .gts-sr-seg{display:flex;align-items:center;padding:0 18px;height:42px;border-right:1px solid #222;flex-shrink:0}
      .gts-sr-seg:last-child{border-right:none}
      .gts-sr-spacer{flex:1;border-right:1px solid #222}
      .gts-sr-label{font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-right:10px}

      /* Download button in bar */
      .gts-dl-btn{height:42px;padding:0 18px;font-size:11px;font-weight:700;letter-spacing:.06em;background:transparent;color:#4A90D9;border:none;cursor:pointer;display:flex;align-items:center;gap:7px;white-space:nowrap;transition:color .12s}
      .gts-dl-btn:hover{color:#6aaee8}
      .gts-dl-btn-real,.gts-dl-btn-real:hover,.gts-dl-btn-real:focus,.gts-dl-btn-real:active{height:42px;padding:0 18px;font-size:11px;font-weight:700;letter-spacing:.06em;background:transparent!important;color:#4A90D9!important;border:none!important;box-shadow:none!important;display:flex;align-items:center;gap:7px;white-space:nowrap}
      .gts-dl-btn-real:hover{color:#6aaee8!important}
      .gts-dl-btn-real .fa{font-size:12px}

      /* Chart player filter */
      .gts-chart-filter{display:flex;align-items:center;gap:10px;margin-bottom:12px;flex-wrap:wrap}
      .gts-chart-filter-label{font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;white-space:nowrap}
      .gts-chart-filter .selectize-input{min-height:30px!important;padding:3px 8px!important;font-size:11px!important;background:#111!important;border:1px solid #2a2a2a!important}
      .gts-chart-filter .selectize-input .item{background:#2a2a2a!important;color:#ccc!important;font-size:10px!important;padding:1px 6px!important}

      /* Data Input page */
      .gts-input-wrap{padding:20px 16px 0}

      /* Instructions header above bar */
      .gts-instructions{margin-bottom:10px}
      .gts-instructions-title{font-size:16px;font-weight:700;color:#ffffff;letter-spacing:.01em;margin-bottom:5px}
      .gts-instructions-sub{font-size:13px;color:#aaaaaa;line-height:1.5}

      /* Full-width segmented control bar */
      .gts-ctrl-bar{display:flex;align-items:stretch;width:100%;height:52px;background:#141414;border:1px solid #222;border-radius:6px;overflow:hidden}
      .gts-ctrl-seg{display:flex;align-items:center;padding:0 22px;border-right:1px solid #222;flex-shrink:0;height:52px;box-sizing:border-box}
      .gts-ctrl-seg:last-child{border-right:none}
      /* shiny-html-output wrappers must also flex so content centers */
      .gts-ctrl-seg>.shiny-html-output,.gts-ctrl-seg>.shiny-bound-output{display:flex;align-items:center;height:100%}
      /* Spacer pushes sims+button to the right */
      .gts-ctrl-spacer{flex:1;min-width:0;border-right:1px solid #222;height:52px}

      /* Label style */
      .gts-seg-label{font-size:11px;font-weight:700;letter-spacing:.06em;text-transform:uppercase;color:#666;margin-right:7px;white-space:nowrap;user-select:none}

      /* File segment */
      .gts-file-seg{padding:0;display:flex;align-items:stretch;flex-shrink:0}
      .gts-file-wrap{display:flex;align-items:center;height:52px}
      #gts_shiny_file_container{position:absolute;opacity:0;pointer-events:none;width:0;height:0;overflow:hidden}
      .gts-file-btn{height:52px;padding:0 20px;font-size:12px;font-weight:700;letter-spacing:.06em;background:#FFE500;color:#000;border:none;cursor:pointer;white-space:nowrap;flex-shrink:0;transition:background .12s}
      .gts-file-btn:hover{background:#d4b800}
      .gts-fname{height:52px;line-height:52px;padding:0 20px;font-size:13px;color:#555;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:240px;border-right:1px solid #222}
      .gts-fname.loaded{color:#ddd}

      /* Sport + sites values */
      .gts-detect-sport{color:#FFE500;font-weight:700;font-size:13px}
      .gts-detect-sites{color:#4A90D9;font-weight:600;font-size:13px}

      /* Sims input */
      .gts-sims-seg .form-group{margin-bottom:0}
      .gts-sims-seg input.form-control{height:32px!important;width:84px!important;font-size:13px;text-align:center;padding:0 6px;border:1px solid #333!important;border-radius:4px!important;background:#0a0a0a!important;color:#ccc!important}

      /* Simulate button — gold fill, rightmost */
      .gts-sim-btn-seg{padding:0;background:#FFE500;flex-shrink:0;transition:background .15s}
      .gts-sim-btn-seg:hover{background:#d4b800}
      .gts-sim-btn-seg .btn{height:52px;padding:0 30px;font-size:13px;font-weight:700;letter-spacing:.08em;background:transparent!important;color:#000!important;border:none!important;border-radius:0;white-space:nowrap;display:flex;align-items:center;gap:8px}

      /* Golf extra row */
      .gts-golf-row{display:flex;align-items:center;gap:14px;margin-top:10px;flex-wrap:wrap}
      .gts-golf-row .form-group{margin-bottom:0}

      /* Post-sim status */
      .gts-sim-done{display:inline-flex;align-items:center;gap:8px;margin-top:12px;padding:7px 16px;background:rgba(74,144,217,0.06);border:1px solid rgba(74,144,217,0.2);border-radius:4px;color:#4A90D9;font-size:12px;font-weight:600}

      /* Sim complete toast */
      #gts-toast{position:fixed;bottom:32px;left:50%;transform:translateX(-50%) translateY(20px);opacity:0;z-index:9999;display:flex;align-items:center;gap:12px;padding:14px 24px;background:#141414;border:1px solid #FFE500;border-radius:6px;box-shadow:0 4px 24px rgba(0,0,0,0.6);pointer-events:none;transition:opacity .25s ease,transform .25s ease;white-space:nowrap}
      #gts-toast.show{opacity:1;transform:translateX(-50%) translateY(0)}
      #gts-toast .toast-icon{width:18px;height:18px;border-radius:50%;background:#FFE500;display:flex;align-items:center;justify-content:center;flex-shrink:0}
      #gts-toast .toast-icon svg{width:11px;height:11px;fill:#000}
      #gts-toast .toast-title{font-size:13px;font-weight:700;color:#FFE500;letter-spacing:.02em}
      #gts-toast .toast-sub{font-size:11px;color:#666;margin-top:1px}
      .gts-sim-error{display:inline-flex;align-items:center;gap:8px;margin-top:12px;padding:7px 16px;background:rgba(192,57,43,0.07);border:1px solid rgba(192,57,43,0.35);border-radius:4px;color:#e74c3c;font-size:12px}

      /* Data validation tabs */
      .gts-validation-wrap{margin-top:20px}
      .gts-validation-wrap .nav-tabs{border-bottom:1px solid #222!important;margin-bottom:0}
      .gts-validation-wrap .nav-tabs>li>a{background:#0d0d0d!important;color:#555!important;border-color:#222!important;font-size:11px;font-weight:600;letter-spacing:.05em;padding:6px 14px}
      .gts-validation-wrap .nav-tabs>li.active>a{background:#141414!important;color:#FFE500!important;border-bottom-color:#141414!important}
      .gts-validation-wrap .tab-content{background:#0d0d0d;border:1px solid #222;border-top:none;border-radius:0 0 4px 4px}
      .gts-validation-wrap table.dataTable{background:#0d0d0d!important;border:none!important;font-size:11px;width:100%!important}
      .gts-validation-wrap table.dataTable thead th{background:#141414!important;color:#666!important;font-size:10px;font-weight:700;letter-spacing:.07em;text-transform:uppercase;border-bottom:1px solid #222!important;padding:8px 12px;white-space:nowrap}
      .gts-validation-wrap table.dataTable tbody tr{background:#0d0d0d!important}
      .gts-validation-wrap table.dataTable tbody tr:nth-child(even){background:#111!important}
      .gts-validation-wrap table.dataTable tbody td{color:#cccccc!important;border-color:#1a1a1a!important;padding:6px 12px}
      .gts-validation-wrap .dataTables_wrapper{background:transparent!important;padding:8px}
      .gts-validation-wrap .dataTables_length,.gts-validation-wrap .dataTables_filter,
      .gts-validation-wrap .dataTables_info,.gts-validation-wrap .dataTables_paginate{display:none!important}
    ")),
    
    tags$script(HTML(r"(
            // Filename — use shiny:inputchanged as primary (most reliable across versions)
      // This fires after Shiny has fully processed the upload
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name !== 'input_file') return;
        var el = document.getElementById('gts_fname');
        if (!el) return;
        var val = e.value;
        var name = null;
        if (Array.isArray(val) && val.length > 0) name = val[0].name;
        else if (val && typeof val === 'object' && val.name) name = val.name;
        else if (typeof val === 'string' && val.length > 0) name = val;
        if (name) { el.textContent = name; el.classList.add('loaded'); }
      });
      // Native change event as immediate visual feedback before Shiny processes
      $(document).on('change', 'input[type=file]', function() {
        var f = this.files[0];
        if (!f) return;
        var el = document.getElementById('gts_fname');
        if (el) { el.textContent = f.name; el.classList.add('loaded'); }
      });

      // Sim complete toast
      document.body.insertAdjacentHTML('beforeend', '<div id="gts-toast"><div class="toast-icon"><svg viewBox="0 0 12 12"><polyline points="1.5,6 4.5,9.5 10.5,2.5" stroke="#000" stroke-width="1.8" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg></div><div><div class="toast-title" id="gts-toast-title"></div><div class="toast-sub" id="gts-toast-sub"></div></div></div>');
    window.gtsShowToast = function(title, sub, duration) {
      document.getElementById('gts-toast-title').textContent = title;
      document.getElementById('gts-toast-sub').textContent   = sub || '';
      var el = document.getElementById('gts-toast');
      el.classList.add('show');
      clearTimeout(window._gtsToastTimer);
      window._gtsToastTimer = setTimeout(function() { el.classList.remove('show'); }, duration || 4000);
    };
    
    $(document).on('shiny:message', function(e) {
      if (e.message && e.message.custom && e.message.custom.gts_toast) {
        var t = e.message.custom.gts_toast;
        window.gtsShowToast(t.title, t.sub, t.duration);
      }
    });
    
    // ── Sidebar collapse toggle ──────────────────────────────────────────
    // Injects a small ◄/► tab fixed to the right edge of the sidebar.
    // State persists in localStorage across page refreshes.
    (function() {
      function applyState(collapsed) {
        var body = document.querySelector('body');
        var btn  = document.getElementById('gts_sb_tab');
        if (collapsed) {
          body.classList.add('sidebar-collapse');
          if (btn) btn.textContent = '\u25BA';
        } else {
          body.classList.remove('sidebar-collapse');
          if (btn) btn.textContent = '\u25C4';
        }
      }

      var btn = document.createElement('button');
      btn.id  = 'gts_sb_tab';
      btn.title = 'Toggle sidebar (or press [)';
      btn.onclick = function() {
        var collapsed = !document.querySelector('body').classList.contains('sidebar-collapse');
        applyState(collapsed);
        localStorage.setItem('gts_sb', collapsed ? '1' : '0');
      };
      document.body.appendChild(btn);

      // Restore saved state
      applyState(localStorage.getItem('gts_sb') === '1');

      // Keyboard shortcut: [ to toggle
      document.addEventListener('keydown', function(e) {
        if (e.key === '[' && !e.target.matches('input,textarea,select')) {
          btn.click();
        }
      });
    })();

    $(document).ready(function() {
      setTimeout(function() {
        $('select[multiple]').each(function() {
          if (this.selectize) {
            var s = this.selectize;
            s.on('item_add',    function() { var self=this; setTimeout(function(){self.close();self.blur();},100); });
            s.on('item_remove', function() { var self=this; setTimeout(function(){self.close();self.blur();},100); });
            s.close();
          }
        });
      }, 2500);
      $(document).on('shiny:value', function(event) {
        if (event.name.includes('locked') || event.name.includes('excluded')) {
          setTimeout(function() {
            var e = document.getElementById(event.name);
            if (e && e.selectize) e.selectize.close();
          }, 200);
        }
      });
    });

    ))")),
    
    tabItems(
      
      # ======================================================================
      # TAB 1: DATA INPUT
      # ======================================================================
      tabItem(tabName = "input",
              div(class = "gts-input-wrap",
                  
                  # Instructions header
                  div(class = "gts-instructions",
                      div(class = "gts-instructions-title", "Upload Input File"),
                      div(class = "gts-instructions-sub",
                          "Select your sport-specific Excel file. Sport and available DFS sites will be detected automatically.")
                  ),
                  
                  div(class = "gts-ctrl-bar",
                      
                      # Segment 1: File picker
                      # Shiny fileInput is hidden but fully functional.
                      # Our gold Browse button sits in front and triggers a click on it.
                      # JS watches Shiny's input value and updates the filename label.
                      div(class = "gts-ctrl-seg gts-file-seg",
                          div(id = "gts_file_wrap", class = "gts-file-wrap",
                              # Shiny handles the actual upload — we just hide it visually
                              div(id = "gts_shiny_file_container",
                                  fileInput("input_file", NULL, accept = c(".xlsx", ".xls"))
                              ),
                              # Our visible controls
                              tags$button(
                                id = "gts_browse_btn",
                                class = "gts-file-btn",
                                type = "button",
                                onclick = "document.querySelector('#gts_shiny_file_container input[type=file]').click()",
                                "Browse"
                              ),
                              tags$span(id = "gts_fname", class = "gts-fname", "No file selected")
                          )
                      ),
                      
                      # Spacer — pushes sims+button to right
                      div(class = "gts-ctrl-spacer"),
                      
                      # Segment 2: Sport / Sites (appears after detection)
                      uiOutput("sport_inline_info"),
                      
                      # Segment 3: Sims count (appears after detection)
                      uiOutput("sim_controls_ui"),
                      
                      # Segment 4: Simulate button (appears after detection)
                      uiOutput("simulate_btn_ui")
                  ),
                  
                  # Golf-only: no-cut + cut line below bar
                  uiOutput("golf_extra_ui"),
                  
                  # Post-sim status
                  uiOutput("sim_complete_message"),
                  
                  # Player data preview
                  uiOutput("file_preview_ui")
              )
      ),
      
      # ======================================================================
      # TAB 2: SIM RESULTS
      # ======================================================================
      tabItem(tabName = "sim_results",
              conditionalPanel(
                condition = "output.has_sim_results == false",
                div(style="text-align:center;padding:60px 40px;",
                    icon("chart-bar", class="fa-3x", style="color:#333;margin-bottom:20px;"),
                    p("Run a simulation first.", style="color:#555;font-size:14px;margin-top:10px;")
                )
              ),
              conditionalPanel(
                condition = "output.has_sim_results == true",
                div(style="padding:16px 16px 0;",
                    # Hidden input to initialize sim_results_platform — pills use setInputValue
                    # but we need the input to exist before the first click
                    tags$div(style="display:none",
                             uiOutput("sim_platform_init")
                    ),
                    
                    # Compact control bar: platform pills + export
                    uiOutput("sim_results_control_bar"),
                    
                    # Projections table
                    box(width=NULL, title="Fantasy Projections",
                        status="primary", solidHeader=TRUE,
                        DTOutput("sim_projections_table") %>%
                          shinycssloaders::withSpinner(color="#FFE500", type=6)
                    ),
                    
                    # Sport-specific visualizations (engine-driven)
                    conditionalPanel(
                      condition = "output.has_sport_visuals == true",
                      uiOutput("sport_specific_visuals_ui")
                    )
                )
              )
      ),
      
      # ======================================================================
      # TAB 3: TOURNAMENT LINEUPS (formerly Lineup Scoring)
      # ======================================================================
      tabItem(tabName = "scoring",  uiOutput("scoring_tabs_ui")),
      
      # ======================================================================
      # TAB 4: CASH GAMES — Double Up Simulator
      # ======================================================================
      tabItem(tabName = "cash_games",
              conditionalPanel(
                condition = "output.has_sim_results == false",
                div(style = "text-align:center;padding:60px 40px;",
                    icon("coins", class = "fa-3x", style = "color:#333;margin-bottom:20px;"),
                    p("Run a simulation first, then score DK Tournament Lineups.",
                      style = "color:#555;font-size:14px;margin-top:10px;")
                )
              ),
              conditionalPanel(
                condition = "output.has_sim_results == true",
                render_cash_game_tab_ui()
              )
      ),
      
      # ======================================================================
      # TAB 5: PORTFOLIO BUILDER
      # ======================================================================
      tabItem(tabName = "portfolio", uiOutput("portfolio_tabs_ui"))
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ==========================================================================
  # RESET ALL STATE
  # Called on every new file upload AND at the start of every sim run.
  # Wipes all downstream state (sim results, lineups, portfolios, lock counters)
  # while preserving rv$sport, rv$config, rv$input_data (set by the caller).
  # ==========================================================================
  reset_all_state <- function() {
    rv$simulation_results  <- NULL
    rv$sim_metadata        <- NULL
    rv$projections         <- NULL
    rv$dk_optimal_lineups  <- NULL
    rv$fd_optimal_lineups  <- NULL
    rv$sd_optimal_lineups  <- NULL
    rv$dk_portfolio        <- NULL;  rv$dk_builds <- list();  rv$dk_build_counter <- 0
    rv$fd_portfolio        <- NULL;  rv$fd_builds <- list();  rv$fd_build_counter <- 0
    rv$sd_portfolio        <- NULL;  rv$sd_builds <- list();  rv$sd_build_counter <- 0
    rv$sport_visuals       <- NULL
    rv$full_sim_results    <- NULL
    # has_dk/fd/sd persist from upload-time detection - not reset here
    rv$dk_lock_v           <- 0L
    rv$fd_lock_v           <- 0L
    rv$sd_lock_v           <- 0L
    rv$dk_slider_v         <- 0L
    rv$fd_slider_v         <- 0L
    rv$sd_slider_v         <- 0L
  }
  
  
  rv <- reactiveValues(
    sport              = NULL,
    config             = NULL,
    input_data         = NULL,
    simulation_results = NULL,
    sim_metadata       = NULL,
    projections        = NULL,
    dk_optimal_lineups = NULL,
    fd_optimal_lineups = NULL,
    sd_optimal_lineups = NULL,
    dk_lock_v          = 0L,
    fd_lock_v          = 0L,
    sd_lock_v          = 0L,
    dk_slider_v        = 0L,
    fd_slider_v        = 0L,
    sd_slider_v        = 0L,
    dk_portfolio       = NULL,
    fd_portfolio       = NULL,
    sd_portfolio       = NULL,
    dk_builds          = list(),
    fd_builds          = list(),
    sd_builds          = list(),
    dk_build_counter        = 0,
    fd_build_counter        = 0,
    sd_build_counter        = 0,
    dk_selected_builds      = character(0),
    fd_selected_builds      = character(0),
    sd_selected_builds      = character(0),
    golf_no_cut        = FALSE,
    golf_cut_line      = 65,
    has_fd             = TRUE,
    has_sd             = TRUE,
    has_dk             = TRUE,
    sport_visuals      = NULL,
    full_sim_results   = NULL
  )
  
  
  # ==========================================================================
  # AVAILABLE PLATFORMS REACTIVE
  # Single derived reactive — never mutate rv$config$platforms.
  # All downstream consumers read this instead of rv$config$platforms.
  # ==========================================================================
  available_platforms <- reactive({
    req(rv$config)
    plats <- rv$config$platforms
    if (!isTRUE(rv$has_dk)) plats <- setdiff(plats, "DK")
    if (!isTRUE(rv$has_fd)) plats <- setdiff(plats, "FD")
    if (!isTRUE(rv$has_sd)) plats <- setdiff(plats, "SD")
    plats
  })
  
  
  # ==========================================================================
  # AUTO-DETECT SPORT, CHECK PLATFORM AVAILABILITY, LOAD DATA
  # ==========================================================================
  observeEvent(input$input_file, {
    req(input$input_file)
    tryCatch({
      reset_all_state()
      rv$sport  <- detect_sport(input$input_file$datapath)
      rv$config <- get_sport_config(rv$sport)
      
      if (rv$sport == "NASCAR") {
        driver_cols <- names(suppressMessages(
          readxl::read_excel(input$input_file$datapath, sheet = "Driver", n_max = 1)
        ))
        rv$has_dk <- TRUE
        rv$has_fd <- all(c("FDSalary", "FDID", "FDName") %in% driver_cols)
      } else if (rv$sport == "SOCCER") {
        # Soccer supports Showdown (always) and DK Classic (when Classic_IDs sheet
        # is present in the combined input file).
        rv$has_fd <- FALSE
        rv$has_sd <- TRUE
        soccer_sheets <- tryCatch(readxl::excel_sheets(input$input_file$datapath), error=function(e) character(0))
        rv$has_dk <- "Classic_IDs" %in% soccer_sheets
      } else {
        # Default each platform flag from the sport's DECLARED platforms in the
        # config (the source of truth). available_platforms() starts from
        # rv$config$platforms and only removes a platform when its flag is FALSE,
        # so a sport's required platforms (e.g. tennis = DK only) must stay TRUE.
        decl <- rv$config$platforms %||% c("DK")
        rv$has_dk <- "DK" %in% decl
        rv$has_fd <- "FD" %in% decl
        rv$has_sd <- "SD" %in% decl
        # Refine the OPTIONAL platforms (FD/SD) by checking the player sheet for
        # their salary columns — present columns keep the flag, absent ones drop
        # it. DK is the base format and is never downgraded here. Only applies to
        # sheet-based sports that carry per-platform salary columns (e.g. MMA's
        # Fights sheet); sports without such columns keep their declared flags.
        psheet <- rv$config$input_file$player_sheet %||% NA_character_
        if (!is.na(psheet) && (rv$has_fd || rv$has_sd)) {
          pcols <- tryCatch(
            names(readxl::read_excel(input$input_file$datapath, sheet = psheet, n_max = 1)),
            error = function(e) NULL
          )
          if (!is.null(pcols)) {
            if (rv$has_fd) rv$has_fd <- any(c("FDSalary","FDSal","FDID") %in% pcols)
            if (rv$has_sd) rv$has_sd <- any(c("SDSal","SDSalary","SDID","CPTID") %in% pcols)
          }
        }
      }
      
      rv$input_data <- load_sport_input(input$input_file$datapath, rv$sport, rv$config)
      
    }, error = function(e) {
      rv$sport  <- NULL
      rv$config <- NULL
      rv$input_data <- NULL
      showNotification(paste("Upload error:", e$message), type = "error", duration = 8)
    })
  })
  
  
  # ── Sport + Sites (appears after detection) ─────────────────────────────
  output$sport_inline_info <- renderUI({
    req(rv$sport, rv$config)
    plat_labels <- c(DK = "DraftKings", FD = "FanDuel", SD = "Showdown")
    sites_str   <- paste(sapply(available_platforms(), function(p) plat_labels[p] %||% p),
                         collapse = ", ")
    div(class = "gts-ctrl-seg",
        span(class = "gts-seg-label", "Sport: "),
        span(class = "gts-detect-sport", rv$config$sport_display_name),
        tags$span(style = "width:1px;height:20px;background:#333;margin:0 16px;display:inline-block;"),
        span(class = "gts-seg-label", "Sites: "),
        span(class = "gts-detect-sites", sites_str)
    )
  })
  
  
  # ── Sims to Run (appears after detection) ────────────────────────────────
  output$sim_controls_ui <- renderUI({
    req(rv$sport)
    div(class = "gts-ctrl-seg gts-sims-seg",
        span(class = "gts-seg-label", "Sims to Run: "),
        numericInput("n_sims", NULL, value = 50000,
                     min = 1000, max = 150000, step = 1000, width = "80px")
    )
  })
  
  
  # ── Simulate button segment (segment 4, appears after detection) ─────────
  output$simulate_btn_ui <- renderUI({
    req(rv$sport)
    btn_label <- switch(rv$sport,
                        MMA    = "Simulate Fights",
                        NASCAR = "Simulate Race",
                        F1     = "Simulate Race",
                        TENNIS = "Simulate Matches",
                        "Simulate Games"
    )
    div(class = "gts-ctrl-seg gts-sim-btn-seg",
        actionButton("run_simulation", btn_label,
                     class = "btn-primary",
                     icon  = icon("play"))
    )
  })
  
  
  # ── Golf extra options (below bar, Golf only) ────────────────────────────
  output$golf_extra_ui <- renderUI({
    req(rv$sport == "GOLF")
    div(class = "gts-golf-row",
        checkboxInput("golf_no_cut", "No-cut tournament", value = FALSE),
        conditionalPanel(
          condition = "!input.golf_no_cut",
          numericInput("golf_cut_line", "Cut line (+ ties):",
                       value = 65, min = 50, max = 85, step = 5, width = "130px")
        )
    )
  })
  
  
  # ── Data validation: all loaded sheets as tabs, full rows ──────────────
  output$file_preview_ui <- renderUI({
    req(rv$input_data, rv$config)
    
    sheet_names <- names(rv$input_data)
    
    # Build one tab per sheet
    tab_list <- lapply(seq_along(sheet_names), function(i) {
      sn <- sheet_names[[i]]
      tabPanel(
        title = sn,
        div(style = "padding:4px 0;",
            DT::DTOutput(paste0("validation_sheet_", i))
        )
      )
    })
    
    div(class = "gts-validation-wrap",
        do.call(tabsetPanel, c(list(type = "tabs"), tab_list))
    )
  })
  
  # Render each sheet's DT — dynamic observers registered when data loads
  observe({
    req(rv$input_data)
    sheet_names <- names(rv$input_data)
    lapply(seq_along(sheet_names), function(i) {
      local({
        idx <- i
        sn  <- sheet_names[[idx]]
        output_id <- paste0("validation_sheet_", idx)
        output[[output_id]] <- DT::renderDT({
          df <- rv$input_data[[sn]]
          if (is.null(df) || (!is.data.frame(df) && !is.data.table(df))) return(NULL)
          DT::datatable(
            as.data.frame(df),
            rownames  = FALSE,
            options   = list(
              dom        = "t",
              paging     = FALSE,
              scrollX    = TRUE,
              ordering   = TRUE,
              searching  = FALSE
            )
          )
        })
      })
    })
  })
  
  
  output$current_sport <- reactive({ rv$sport %||% "" })
  outputOptions(output, "current_sport", suspendWhenHidden = FALSE)
  
  
  # ==========================================================================
  # RUN SIMULATION
  # ==========================================================================
  observeEvent(input$run_simulation, {
    req(rv$input_data, rv$config)
    
    # Clear all downstream state so stale lineups/portfolio never bleed into new sim.
    reset_all_state()
    
    progress <- Progress$new(session, min = 0, max = 1)
    progress$set(message = "Running simulation...", value = 0)
    on.exit(progress$close())
    
    tryCatch({
      if (rv$sport == "GOLF") {
        no_cut   <- if (!is.null(input$golf_no_cut))   input$golf_no_cut   else FALSE
        cut_line <- if (!is.null(input$golf_cut_line)) input$golf_cut_line else 65
        result   <- run_golf_simulation(
          input_data        = rv$input_data,
          n_sims            = input$n_sims,
          cut_line          = cut_line,
          no_cut            = no_cut,
          progress_callback = function(v, m) progress$set(value = v, detail = m)
        )
        rv$simulation_results <- result$sim_results
        rv$sim_metadata       <- result$sim_metadata
        rv$golf_no_cut        <- result$no_cut
        rv$golf_cut_line      <- result$cut_line
        rv$has_fd             <- isTRUE(result$has_fd)
        rv$sport_visuals      <- NULL
        rv$full_sim_results   <- NULL
        
      } else {
        sim_function <- get(rv$config$simulation$function_name)
        result       <- sim_function(
          input_data        = rv$input_data,
          n_sims            = input$n_sims,
          config            = rv$config,
          progress_callback = function(detail, value) progress$set(value = value, detail = detail)
        )
        validate_simulation_output(result$sim_results, result$metadata, rv$config)
        rv$simulation_results <- result$sim_results
        rv$sim_metadata       <- result$metadata
        
        
        # ── Update platform flags WITHOUT touching rv$config ────────────────
        if (rv$sport == "NASCAR") {
          rv$full_sim_results <- result$full_results
          rv$has_fd           <- isTRUE(result$has_fd)
        } else if (rv$sport == "F1") {
          rv$full_sim_results <- NULL
          rv$has_fd           <- FALSE
          rv$has_sd           <- FALSE
        } else if (rv$sport == "SOCCER") {
          rv$full_sim_results <- NULL
          rv$has_fd <- FALSE
          rv$has_sd <- TRUE
          rv$has_dk <- isTRUE(result$has_classic)
        } else {
          rv$full_sim_results <- NULL
          # Keep platform flags from upload-time detection
        }
        
        if (!is.null(result$projections))   rv$projections   <- result$projections
        rv$sport_visuals <- result$sport_visuals %||% NULL
      }
      
      # ── Post-sim status strip ────────────────────────────────────────────
      output$sim_complete_message <- renderUI({
        div(class = "gts-sim-done",
            icon("check-circle"),
            sprintf("Simulation complete — %s sims | %s %s",
                    format(input$n_sims, big.mark = ","),
                    nrow(rv$sim_metadata),
                    tolower(rv$config$player_label_plural))
        )
      })
      session$sendCustomMessage("gts_toast", list(
        title    = "Simulation Complete",
        sub      = paste0(format(input$n_sims, big.mark = ","), " sims | ",
                          nrow(rv$sim_metadata), " ",
                          tolower(rv$config$player_label_plural %||% "players")),
        duration = 4000
      ))
      cat(sprintf("  Simulation complete: %s sims | %d %s\n",
                  format(input$n_sims, big.mark=","),
                  nrow(rv$sim_metadata),
                  tolower(rv$config$player_label_plural %||% "players")))
      
    }, error = function(e) {
      output$sim_complete_message <- renderUI({
        div(class = "gts-sim-error",
            icon("exclamation-triangle"),
            paste("Simulation error:", e$message))
      })
      showNotification(paste("Simulation error:", e$message), type = "error", duration = 10)
      cat("Simulation error:\n"); print(e)
    })
  })
  
  
  # ==========================================================================
  # HELPER FUNCTIONS
  # ==========================================================================
  
  prepare_optimization_data <- function(sim_results, metadata, platform) {
    score_col  <- if (platform == "SD") "DKScore"              else paste0(platform, "Score")
    salary_col <- if (platform == "SD") "SDSalary"             else paste0(platform, "Salary")
    setDT(sim_results); setDT(metadata)
    opt_data <- merge(sim_results, metadata[, .(Player, Salary=get(salary_col))], by="Player")
    opt_data[, FantasyPoints := get(score_col)]
    opt_data[Salary > 0 & !is.na(Salary)]
  }
  
  create_display_table <- function(optimal_lineups, metadata, platform) {
    if ("Captain" %in% names(optimal_lineups)) {
      player_cols <- c("Captain", grep("^Util", names(optimal_lineups), value=TRUE))
    } else if ("MVP" %in% names(optimal_lineups)) {
      player_cols <- c("MVP", grep("^Player", names(optimal_lineups), value=TRUE))
    } else {
      player_cols <- grep("^Player", names(optimal_lineups), value=TRUE)
    }
    display_cols <- intersect(
      c(player_cols, "WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct",
        "TotalSalary","AvgOwn","CumulativeStarting","AvgStart"),
      names(optimal_lineups))
    display_table <- optimal_lineups[, ..display_cols]
    rename_map <- c("WinRate"="Win","Top1Pct"="Top1","Top5Pct"="Top5","Top10Pct"="Top10","Top20Pct"="Top20",
                    "TotalSalary"="Salary","CumulativeStarting"="TotalStart")
    for (o in names(rename_map)) if (o %in% names(display_table)) setnames(display_table, o, rename_map[[o]])
    display_table
  }
  
  create_portfolio_display_table <- function(portfolio_data, sport_config, platform="") {
    display_table <- copy(portfolio_data); setDT(display_table)
    display_table[, RowID := .I]
    rename_map <- c("WinRate"="Win","Top1Pct"="Top1","Top5Pct"="Top5","Top10Pct"="Top10","Top20Pct"="Top20",
                    "TotalSalary"="Salary")
    for (o in names(rename_map)) if (o %in% names(display_table)) setnames(display_table, o, rename_map[[o]])
    if (!is.null(sport_config$custom_metrics)) {
      for (m in sport_config$custom_metrics) {
        sc <- if (!is.null(m$source_column)) m$source_column else m$source
        dc <- if (!is.null(m$display_name))  m$display_name  else m$label
        if (!is.null(sc) && !is.null(dc) && sc %in% names(display_table) && sc != dc)
          setnames(display_table, sc, dc)
      }
    }
    display_table
  }
  
  get_format_columns <- function(display_table, sport_config) {
    pct_cols <- c("Win","Top1","Top5","Top10","Top20","AvgOwn")
    if (!is.null(sport_config$custom_metrics)) {
      for (m in sport_config$custom_metrics) {
        if (!is.null(m$format) && m$format == "percentage") {
          cn <- if (!is.null(m$display_name)) m$display_name else m$label
          if (!is.null(cn)) pct_cols <- c(pct_cols, cn)
        }
      }
    }
    intersect(pct_cols, names(display_table))
  }
  
  create_download_standard <- function(optimal_lineups, metadata, platform) {
    player_cols    <- grep("^Player", names(optimal_lineups), value=TRUE)
    id_col         <- paste0(platform, "ID")
    download_table <- copy(optimal_lineups)
    for (col in player_cols) {
      ids <- metadata[match(download_table[[col]], metadata$Player), get(id_col)]
      download_table[[col]] <- if (platform=="DK") paste0(download_table[[col]]," (",ids,")")
      else paste0(ids,":",download_table[[col]])
    }
    download_table
  }
  
  create_download_showdown <- function(optimal_lineups, metadata) {
    dl <- copy(optimal_lineups)
    if ("Captain" %in% names(dl)) {
      ids <- metadata[match(dl$Captain, metadata$Player), CPTID]
      dl$Captain <- paste0(dl$Captain," (",ids,")")
    }
    for (col in grep("^Util", names(dl), value=TRUE)) {
      ids <- metadata[match(dl[[col]], metadata$Player), SDID]
      dl[[col]] <- paste0(dl[[col]]," (",ids,")")
    }
    dl
  }
  
  create_download_f1 <- function(optimal_lineups, metadata) {
    dl <- copy(optimal_lineups); setDT(metadata)
    fmt <- function(name, id) paste0(name, " (", id, ")")
    cpt_ids  <- metadata[match(dl$Captain, metadata$Player), CptDFSID]
    cpt_col  <- fmt(dl$Captain, cpt_ids)
    flex_cols <- lapply(paste0("Util", 1:4), function(col) {
      ids <- metadata[match(dl[[col]], metadata$Player), DKID]
      fmt(dl[[col]], ids)
    })
    con_ids <- metadata[match(dl$Util5, metadata$Player), DKID]
    con_col <- fmt(dl$Util5, con_ids)
    lineup_cols <- data.table(
      CPT   = cpt_col,
      D     = flex_cols[[1]],
      D2    = flex_cols[[2]],
      D3    = flex_cols[[3]],
      D4    = flex_cols[[4]],
      CNSTR = con_col
    )
    setnames(lineup_cols, c("CPT", "D", "D", "D", "D", "CNSTR"))
    metric_cols <- intersect(c("WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct",
                               "TotalSalary","AvgOwn","Top1Count","AvgScore"),
                             names(dl))
    if (length(metric_cols) > 0) cbind(lineup_cols, dl[, ..metric_cols]) else lineup_cols
  }
  
  create_download_mvp <- function(optimal_lineups, metadata) {
    dl <- copy(optimal_lineups); setDT(metadata)
    if ("MVP" %in% names(dl)) {
      ids <- metadata[match(dl$MVP, metadata$Player), FDID]
      dl$MVP <- paste0(ids,":",dl$MVP)
    }
    for (col in grep("^Player", names(dl), value=TRUE)) {
      ids <- metadata[match(dl[[col]], metadata$Player), FDID]
      dl[[col]] <- paste0(ids,":",dl[[col]])
    }
    dl
  }
  
  create_download_table <- function(optimal_lineups, metadata, platform, sport = NULL) {
    if ("Captain" %in% names(optimal_lineups)) {
      if (identical(sport, "F1"))
        return(create_download_f1(optimal_lineups, metadata))
      return(create_download_showdown(optimal_lineups, metadata))
    }
    if ("MVP" %in% names(optimal_lineups)) return(create_download_mvp(optimal_lineups, metadata))
    create_download_standard(optimal_lineups, metadata, platform)
  }
  
  create_display_table_cbb <- function(optimal_lineups, platform = "DK") {
    dl <- copy(optimal_lineups)
    if ("Captain" %in% names(dl)) {
      slot_cols <- c("Captain", grep("^Util", names(dl), value=TRUE))
    } else if (platform == "FD") {
      pos_rename <- c(Player1="G1", Player2="G2", Player3="G3", Player4="G4",
                      Player5="F1", Player6="F2", Player7="F3",
                      Player8="UTIL1")
      for (o in names(pos_rename)) if (o %in% names(dl)) setnames(dl, o, pos_rename[o])
      slot_cols <- intersect(c("G1","G2","G3","G4","F1","F2","F3","UTIL1"), names(dl))
    } else {
      pos_rename <- c(Player1="G1", Player2="G2", Player3="G3",
                      Player4="F1", Player5="F2", Player6="F3",
                      Player7="UTIL1", Player8="UTIL2")
      for (o in names(pos_rename)) if (o %in% names(dl)) setnames(dl, o, pos_rename[o])
      slot_cols <- intersect(c("G1","G2","G3","F1","F2","F3","UTIL1","UTIL2"), names(dl))
    }
    metric_cols <- intersect(c("WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct","TotalSalary","AvgOwn"), names(dl))
    keep <- c(slot_cols, metric_cols)
    dl <- dl[, ..keep]
    metric_rename <- c("WinRate"="Win","Top1Pct"="Top1","Top5Pct"="Top5","Top10Pct"="Top10","Top20Pct"="Top20","TotalSalary"="Salary")
    for (o in names(metric_rename)) if (o %in% names(dl)) setnames(dl, o, metric_rename[o])
    if ("AvgOwn" %in% names(dl)) dl[, AvgOwn := round(AvgOwn, 1)]
    dl
  }
  
  create_download_cbb <- function(optimal_lineups, metadata, platform = "DK") {
    dl <- copy(optimal_lineups)
    if ("Captain" %in% names(dl)) {
      return(create_download_showdown(dl, metadata))
    } else if (platform == "FD") {
      pos_rename <- c(Player1="G1", Player2="G2", Player3="G3", Player4="G4",
                      Player5="F1", Player6="F2", Player7="F3",
                      Player8="UTIL1")
      for (o in names(pos_rename)) if (o %in% names(dl)) setnames(dl, o, pos_rename[o])
      slot_cols <- intersect(c("G1","G2","G3","G4","F1","F2","F3","UTIL1"), names(dl))
      for (col in slot_cols) {
        ids <- metadata[match(dl[[col]], metadata$Player), FDID]
        dl[[col]] <- paste0(ids, ":", dl[[col]])
      }
    } else {
      pos_rename <- c(Player1="G1", Player2="G2", Player3="G3",
                      Player4="F1", Player5="F2", Player6="F3",
                      Player7="UTIL1", Player8="UTIL2")
      for (o in names(pos_rename)) if (o %in% names(dl)) setnames(dl, o, pos_rename[o])
      slot_cols <- intersect(c("G1","G2","G3","F1","F2","F3","UTIL1","UTIL2"), names(dl))
      for (col in slot_cols) {
        ids <- metadata[match(dl[[col]], metadata$Player), DKID]
        dl[[col]] <- paste0(dl[[col]], " (", ids, ")")
      }
    }
    metric_rename <- c("WinRate"="Win","Top1Pct"="Top1","Top5Pct"="Top5","Top10Pct"="Top10","Top20Pct"="Top20","TotalSalary"="Salary")
    for (o in names(metric_rename)) if (o %in% names(dl)) setnames(dl, o, metric_rename[o])
    dl
  }
  
  
  # ==========================================================================
  # NBA DISPLAY + DOWNLOAD FUNCTIONS
  # DK:  PG / SG / SF / PF / C / G / F / UTIL  (8 players)
  # FD:  PG / PG / SG / SG / SF / SF / PF / PF / C  (9 players)
  # SD:  Captain + Util1..5
  # ==========================================================================
  
  create_display_table_nba <- function(optimal_lineups, platform = "DK") {
    dl <- copy(optimal_lineups)
    if ("Captain" %in% names(dl)) {
      slot_cols <- c("Captain", grep("^Util", names(dl), value=TRUE))
    } else if (platform == "FD") {
      pos_rename <- c(Player1="PG1", Player2="PG2", Player3="SG1", Player4="SG2",
                      Player5="SF1", Player6="SF2", Player7="PF1", Player8="PF2",
                      Player9="C")
      for (o in names(pos_rename)) if (o %in% names(dl)) setnames(dl, o, pos_rename[o])
      slot_cols <- intersect(c("PG1","PG2","SG1","SG2","SF1","SF2","PF1","PF2","C"), names(dl))
    } else {
      pos_rename <- c(Player1="PG", Player2="SG", Player3="SF", Player4="PF",
                      Player5="C", Player6="G", Player7="F", Player8="UTIL")
      for (o in names(pos_rename)) if (o %in% names(dl)) setnames(dl, o, pos_rename[o])
      slot_cols <- intersect(c("PG","SG","SF","PF","C","G","F","UTIL"), names(dl))
    }
    metric_cols <- intersect(c("WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct",
                               "TotalSalary","AvgOwn"), names(dl))
    keep <- c(slot_cols, metric_cols)
    dl <- dl[, ..keep]
    metric_rename <- c("WinRate"="Win","Top1Pct"="Top1","Top5Pct"="Top5",
                       "Top10Pct"="Top10","Top20Pct"="Top20","TotalSalary"="Salary")
    for (o in names(metric_rename)) if (o %in% names(dl)) setnames(dl, o, metric_rename[o])
    if ("AvgOwn" %in% names(dl)) dl[, AvgOwn := round(AvgOwn, 1)]
    dl
  }
  
  create_download_nba <- function(optimal_lineups, metadata, platform = "DK") {
    dl <- copy(optimal_lineups)
    if ("Captain" %in% names(dl)) {
      return(create_download_showdown(dl, metadata))
    } else if (platform == "FD") {
      pos_rename <- c(Player1="PG1", Player2="PG2", Player3="SG1", Player4="SG2",
                      Player5="SF1", Player6="SF2", Player7="PF1", Player8="PF2",
                      Player9="C")
      for (o in names(pos_rename)) if (o %in% names(dl)) setnames(dl, o, pos_rename[o])
      slot_cols <- intersect(c("PG1","PG2","SG1","SG2","SF1","SF2","PF1","PF2","C"), names(dl))
      for (col in slot_cols) {
        ids <- metadata[match(dl[[col]], metadata$Player), FDID]
        dl[[col]] <- paste0(ids, ":", dl[[col]])
      }
    } else {
      pos_rename <- c(Player1="PG", Player2="SG", Player3="SF", Player4="PF",
                      Player5="C", Player6="G", Player7="F", Player8="UTIL")
      for (o in names(pos_rename)) if (o %in% names(dl)) setnames(dl, o, pos_rename[o])
      slot_cols <- intersect(c("PG","SG","SF","PF","C","G","F","UTIL"), names(dl))
      for (col in slot_cols) {
        ids <- metadata[match(dl[[col]], metadata$Player), DKID]
        dl[[col]] <- paste0(dl[[col]], " (", ids, ")")
      }
    }
    metric_rename <- c("WinRate"="Win","Top1Pct"="Top1","Top5Pct"="Top5",
                       "Top10Pct"="Top10","Top20Pct"="Top20","TotalSalary"="Salary")
    for (o in names(metric_rename)) if (o %in% names(dl)) setnames(dl, o, metric_rename[o])
    dl
  }
  
  
  # ==========================================================================
  # SPORT-SPECIFIC LINEUP METRICS
  # ==========================================================================
  
  add_custom_metrics <- function(scored_lineups, metadata, config, precalc_metrics=NULL) {
    if (!is.null(precalc_metrics)) {
      for (col in names(precalc_metrics)) scored_lineups[[col]] <- precalc_metrics[[col]]
      return(scored_lineups)
    }
    if (!is.null(config$lineup_metrics_function)) {
      if (exists(config$lineup_metrics_function)) {
        scored_lineups <- get(config$lineup_metrics_function)(
          scored_lineups = scored_lineups,
          sim_results    = rv$simulation_results,
          metadata       = metadata
        )
      }
    }
    scored_lineups
  }
  
  add_golf_custom_metrics <- function(scored_lineups, no_cut) {
    if (exists("calculate_golf_lineup_metrics")) {
      scored_lineups <- calculate_golf_lineup_metrics(
        scored_lineups = scored_lineups,
        sim_results    = rv$simulation_results,
        sim_metadata   = rv$sim_metadata,
        no_cut         = no_cut
      )
    }
    scored_lineups
  }
  
  
  # ==========================================================================
  # DK OPTIMIZATION
  # ==========================================================================
  
  observeEvent(input$run_dk_optimization, {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    rv$dk_optimal_lineups <- NULL
    rv$dk_portfolio <- NULL; rv$dk_builds <- list(); rv$dk_build_counter <- 0
    rv$dk_lock_v <- 0L
    rv$dk_slider_v <- 0L
    progress <- Progress$new(session); on.exit(progress$close())
    tryCatch({
      
      if (rv$sport == "GOLF") {
        progress$set(message="Finding optimal DK Golf lineups...", value=0)
        no_cut    <- rv$golf_no_cut %||% FALSE
        dk_config  <- list(platform="DK", roster_size=rv$config$roster_sizes$DK,
                           salary_cap=rv$config$salary_caps$DK)
        dk_opt_cfg <- list(roster_size=dk_config$roster_size, salary_cap=dk_config$salary_cap,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="DKScore",
                           max_lineups=5000)
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "DK")
        
        if (!no_cut) {
          progress$set(detail="Phase 1: Building cut-optimized candidate pool...", value=0.1)
          lineup_data <- generate_golf_candidate_pool(
            sim_results=rv$simulation_results, sim_metadata=rv$sim_metadata,
            config=dk_config, no_cut=FALSE,
            n_sample=rv$config$phase1_n_sample, target_pool=rv$config$phase1_target, verbose=TRUE)
        } else {
          progress$set(detail="Phase 1: Finding optimal lineups...", value=0.1)
          lineup_data <- find_optimal_lineups(opt_data, dk_opt_cfg, mode="standard", k=1, verbose=TRUE)
        }
        progress$set(detail="Phase 2: Scoring lineups...", value=0.45)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.75)
        own_data <- copy(rv$sim_metadata)
        if ("DKOwn" %in% names(own_data)) { setnames(own_data, "DKOwn", "Own"); if (max(own_data$Own, na.rm=TRUE) > 1) own_data[, Own := Own / 100] }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, dk_opt_cfg,
                                                        ownership_data=own_data, verbose=TRUE)
        progress$set(detail="Adding golf metrics...", value=0.90)
        final_results <- add_golf_custom_metrics(final_results, no_cut)
        rv$dk_optimal_lineups <- final_results
        
      } else if (rv$sport == "TENNIS") {
        progress$set(message="Finding optimal DK Tennis lineups...", value=0)
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "DK")
        opt_config <- list(roster_size=rv$config$roster_sizes$DK, salary_cap=rv$config$salary_caps$DK,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="DKScore", max_lineups=5000)
        progress$set(detail="Generating lineups (win-based)...", value=0.1)
        lineup_result    <- find_optimal_lineups(opt_data, opt_config, mode="win_based", verbose=TRUE)
        lineup_data      <- list(unique_lineups=lineup_result$unique_lineups,
                                 n_sims=lineup_result$n_sims, mode=lineup_result$mode)
        tennis_precalc   <- lineup_result$win_metrics
        progress$set(detail="Phase 2: Scoring...", value=0.4)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3: Metrics...", value=0.7)
        own_data <- copy(rv$sim_metadata)
        if ("DKOwn" %in% names(own_data)) { setnames(own_data, "DKOwn", "Own"); if (max(own_data$Own, na.rm=TRUE) > 1) own_data[, Own := Own / 100] }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, opt_config,
                                                        ownership_data=own_data, verbose=TRUE)
        final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config, tennis_precalc)
        rv$dk_optimal_lineups <- final_results
        
      } else if (rv$sport == "F1") {
        n_sims_total <- length(unique(rv$simulation_results$SimID))
        progress$set(message="Finding optimal F1 lineups...", value=0)
        opt_config <- list(
          salary_cap     = rv$config$salary_caps$DK,
          cpt_multiplier = 1.5,
          max_lineups    = 3000L,
          percentiles    = c(0.01, 0.05, 0.10, 0.20),
          platform_col   = "DKScore"
        )
        progress$set(detail=sprintf("Phase 1: Finding optimal lineup per sim (%s sims)...",
                                    format(n_sims_total, big.mark=",")), value=0.05)
        lineup_data <- find_optimal_f1_lineups(
          sim_results = rv$simulation_results,
          metadata    = rv$sim_metadata,
          config      = opt_config,
          verbose     = TRUE
        )
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        cbb_sim_for_scoring <- copy(rv$simulation_results)
        setDT(cbb_sim_for_scoring)
        score_matrix <- score_all_lineups(lineup_data, cbb_sim_for_scoring, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, opt_config,
                                                        ownership_data=NULL, verbose=TRUE)
        rv$dk_optimal_lineups <- final_results
        
      } else if (rv$sport == "CBB") {
        progress$set(message="Finding optimal CBB lineups...", value=0)
        cbb_opt_config <- list(salary_cap=rv$config$salary_caps$DK,
                               percentiles=c(0.01,0.05,0.10,0.20),
                               platform_col="DKScore", max_lineups=5000)
        progress$set(detail="Phase 1: Building lineup pool (per-sim LP)...", value=0.05)
        lineup_data  <- find_optimal_lineups_cbb(rv$simulation_results, rv$sim_metadata,
                                                 cbb_opt_config, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        cbb_sim_for_scoring <- copy(rv$simulation_results)
        setDT(cbb_sim_for_scoring)
        score_matrix <- score_all_lineups(lineup_data, cbb_sim_for_scoring, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        if ("DKOwn" %in% names(own_data)) {
          setnames(own_data, "DKOwn", "Own")
          own_data[, Own := Own / 100]
        }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, cbb_opt_config,
                                                        ownership_data=own_data, verbose=TRUE)
        if ("AvgOwn" %in% names(final_results)) final_results[, AvgOwn := round(AvgOwn, 1)]
        rv$dk_optimal_lineups <- final_results
        
      } else if (rv$sport == "NBA") {
        progress$set(message="Finding optimal NBA DK lineups...", value=0)
        nba_dk_config <- list(salary_cap   = rv$config$salary_caps$DK,
                              max_lineups  = 5000,
                              percentiles  = c(0.01, 0.05, 0.10, 0.20),
                              platform_col = "DKScore")
        progress$set(detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data <- find_optimal_lineups_nba(rv$simulation_results, rv$sim_metadata,
                                                nba_dk_config, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        nba_sim_dk <- copy(rv$simulation_results); setDT(nba_sim_dk)
        score_matrix <- score_all_lineups(lineup_data, nba_sim_dk, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        if ("DKOwn" %in% names(own_data)) { setnames(own_data, "DKOwn", "Own"); own_data[, Own := Own / 100] }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, nba_dk_config,
                                                        ownership_data=own_data, verbose=TRUE)
        if ("AvgOwn" %in% names(final_results)) final_results[, AvgOwn := round(AvgOwn, 1)]
        rv$dk_optimal_lineups <- final_results
        
      } else if (rv$sport == "SOCCER") {
        progress$set(message="Finding optimal Soccer DK lineups...", value=0)
        soccer_dk_config <- list(salary_cap   = rv$config$salary_caps$DK,
                                 max_lineups  = 5000,
                                 percentiles  = c(0.01, 0.05, 0.10, 0.20),
                                 platform_col = "DKScore")
        progress$set(detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data <- find_optimal_lineups_soccer(rv$simulation_results, rv$sim_metadata,
                                                   soccer_dk_config, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        soccer_sim_dk <- copy(rv$simulation_results); setDT(soccer_sim_dk)
        score_matrix <- score_all_lineups(lineup_data, soccer_sim_dk, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        if ("DKOwn" %in% names(own_data)) { setnames(own_data, "DKOwn", "Own"); own_data[, Own := Own / 100] }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, soccer_dk_config,
                                                        ownership_data=own_data, verbose=TRUE)
        if ("AvgOwn" %in% names(final_results)) final_results[, AvgOwn := round(AvgOwn, 1)]
        rv$dk_optimal_lineups <- final_results
        
      } else {
        dk_mode <- rv$config$optimization_modes$DK %||% "standard"
        progress$set(message="Finding optimal DraftKings lineups...", value=0)
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "DK")
        opt_config <- list(roster_size=rv$config$roster_sizes$DK, salary_cap=rv$config$salary_caps$DK,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="DKScore",
                           progress_frequency=500, use_parallel=TRUE,
                           max_lineups=rv$config$max_lineups %||% 5000L)
        progress$set(detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data <- find_optimal_lineups(opt_data, opt_config, mode=dk_mode, k=1, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        if ("DKOwn" %in% names(own_data)) { setnames(own_data, "DKOwn", "Own"); if (max(own_data$Own, na.rm=TRUE) > 1) own_data[, Own := Own / 100] }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, opt_config,
                                                        ownership_data=own_data, verbose=TRUE)
        progress$set(detail="Phase 3: Adding custom metrics...", value=0.90)
        final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config)
        # Strip MMA win-count metrics (not needed for lineup building)
        for (wc in intersect(c("TotalEW","Win6Pct","Win5PlusPct"), names(final_results)))
          final_results[, (wc) := NULL]
        rv$dk_optimal_lineups <- final_results
      }
      
      progress$set(detail="Complete!", value=1.0)
      showNotification(sprintf("Found %d optimal DK lineups!", nrow(rv$dk_optimal_lineups)), type="message")
    }, error=function(e) {
      showNotification(paste("DK error:", e$message), type="error", duration=NULL)
      cat("DK error:\n"); print(e)
    })
  })
  
  
  # ==========================================================================
  # FD OPTIMIZATION
  # ==========================================================================
  
  observeEvent(input$run_fd_optimization, {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    rv$fd_optimal_lineups <- NULL
    rv$fd_portfolio <- NULL; rv$fd_builds <- list(); rv$fd_build_counter <- 0
    rv$fd_lock_v <- 0L
    rv$fd_slider_v <- 0L
    progress <- Progress$new(session); on.exit(progress$close())
    tryCatch({
      
      if (rv$sport == "GOLF") {
        if (!rv$has_fd) { showNotification("No FD salary data found.", type="warning"); return() }
        no_cut    <- rv$golf_no_cut %||% FALSE
        fd_config  <- list(platform="FD", roster_size=rv$config$roster_sizes$FD,
                           salary_cap=rv$config$salary_caps$FD)
        fd_opt_cfg <- list(roster_size=fd_config$roster_size, salary_cap=fd_config$salary_cap,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="FDScore",
                           max_lineups=5000)
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "FD")
        if (!no_cut) {
          progress$set(message="Finding optimal FD Golf lineups...", value=0,
                       detail="Phase 1: Building cut-optimized pool...")
          lineup_data <- generate_golf_candidate_pool(
            sim_results=rv$simulation_results, sim_metadata=rv$sim_metadata,
            config=fd_config, no_cut=FALSE,
            n_sample=rv$config$phase1_n_sample, target_pool=rv$config$phase1_target, verbose=TRUE)
        } else {
          progress$set(message="Finding optimal FD Golf lineups...", value=0, detail="Phase 1...")
          lineup_data <- find_optimal_lineups(opt_data, fd_opt_cfg, mode="standard", k=1, verbose=TRUE)
        }
        progress$set(detail="Phase 2...", value=0.45)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3...", value=0.75)
        own_data <- copy(rv$sim_metadata)
        if ("FDOwn" %in% names(own_data)) { setnames(own_data, "FDOwn", "Own"); if (max(own_data$Own, na.rm=TRUE) > 1) own_data[, Own := Own / 100] }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, fd_opt_cfg,
                                                        ownership_data=own_data, verbose=TRUE)
        progress$set(detail="Golf metrics...", value=0.90)
        final_results <- add_golf_custom_metrics(final_results, no_cut)
        rv$fd_optimal_lineups <- final_results
        
      } else if (rv$sport == "CBB") {
        progress$set(message="Finding optimal CBB FD lineups...", value=0)
        cbb_fd_config <- list(salary_cap  = rv$config$salary_caps$FD,
                              max_lineups = 5000,
                              percentiles = c(0.01, 0.05, 0.10, 0.20),
                              platform_col = "FDScore")
        lineup_data <- find_optimal_lineups_cbb_fd(rv$simulation_results, rv$sim_metadata,
                                                   cbb_fd_config, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        opt_data_fd  <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "FD")
        score_matrix <- score_all_lineups(lineup_data, opt_data_fd, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        if ("FDOwn" %in% names(own_data)) {
          setnames(own_data, "FDOwn", "Own")
          own_data[, Own := Own / 100]
        }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, cbb_fd_config,
                                                        ownership_data=own_data, verbose=TRUE)
        if ("AvgOwn" %in% names(final_results)) final_results[, AvgOwn := round(AvgOwn, 1)]
        rv$fd_optimal_lineups <- final_results
        
      } else if (rv$sport == "NBA") {
        progress$set(message="Finding optimal NBA FD lineups...", value=0)
        nba_fd_config <- list(salary_cap   = rv$config$salary_caps$FD,
                              max_lineups  = 5000,
                              percentiles  = c(0.01, 0.05, 0.10, 0.20),
                              platform_col = "FDScore")
        progress$set(detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data <- find_optimal_lineups_nba_fd(rv$simulation_results, rv$sim_metadata,
                                                   nba_fd_config, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        nba_sim_fd <- copy(rv$simulation_results); setDT(nba_sim_fd)
        score_matrix <- score_all_lineups(lineup_data, nba_sim_fd, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        if ("FDOwn" %in% names(own_data)) { setnames(own_data, "FDOwn", "Own"); own_data[, Own := Own / 100] }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, nba_fd_config,
                                                        ownership_data=own_data, verbose=TRUE)
        if ("AvgOwn" %in% names(final_results)) final_results[, AvgOwn := round(AvgOwn, 1)]
        rv$fd_optimal_lineups <- final_results
        
      } else {
        if (!isTRUE(rv$has_fd)) {
          showNotification("No FD salary data in this file.", type="warning"); return()
        }
        fd_mode <- rv$config$optimization_modes$FD %||% "standard"
        progress$set(message="Finding optimal FanDuel lineups...", value=0)
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "FD")
        opt_config <- list(roster_size=rv$config$roster_sizes$FD, salary_cap=rv$config$salary_caps$FD,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="FDScore",
                           mvp_multiplier=1.5, progress_frequency=500, use_parallel=TRUE, max_lineups=5000)
        progress$set(detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data <- find_optimal_lineups(opt_data, opt_config, mode=fd_mode, k=1, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        if ("FDOwn" %in% names(own_data)) { setnames(own_data, "FDOwn", "Own"); if (max(own_data$Own, na.rm=TRUE) > 1) own_data[, Own := Own / 100] }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, opt_config,
                                                        ownership_data=own_data, verbose=TRUE)
        progress$set(detail="Phase 3: Adding custom metrics...", value=0.90)
        final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config)
        for (wc in intersect(c("TotalEW","Win6Pct","Win5PlusPct"), names(final_results)))
          final_results[, (wc) := NULL]
        rv$fd_optimal_lineups <- final_results
      }
      
      progress$set(detail="Complete!", value=1.0)
      showNotification(sprintf("Found %d optimal FD lineups!", nrow(rv$fd_optimal_lineups)), type="message")
    }, error=function(e) {
      showNotification(paste("FD error:", e$message), type="error", duration=NULL)
      cat("FD error:\n"); print(e)
    })
  })
  
  
  # ==========================================================================
  # SD OPTIMIZATION
  # ==========================================================================
  
  observeEvent(input$run_sd_optimization, {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    rv$sd_optimal_lineups <- NULL
    rv$sd_portfolio <- NULL; rv$sd_builds <- list(); rv$sd_build_counter <- 0
    rv$sd_lock_v <- 0L
    rv$sd_slider_v <- 0L
    progress <- Progress$new(session); on.exit(progress$close())
    tryCatch({
      if (rv$sport == "CBB") {
        sd_meta <- copy(rv$sim_metadata); setDT(sd_meta)
        if (!"CPTSalary" %in% names(sd_meta) || all(is.na(sd_meta$CPTSalary)))
          stop("No CPTSalary in metadata. Check CPT_Salary column in SD_IDs sheets.")
        if (!"SDSalary" %in% names(sd_meta) || all(is.na(sd_meta$SDSalary)))
          stop("No SDSalary in metadata. Check UTIL_Salary column in SD_IDs sheets.")
        
        selected_sd <- if (!is.null(input$sd_game_select)) input$sd_game_select else {
          rv$input_data$games[!is.na(ShowdownFile) & ShowdownFile != "", ShowdownFile[1]]
        }
        sd_meta <- sd_meta[ShowdownFile == selected_sd]
        if (nrow(sd_meta) == 0) stop(sprintf("No players found for Showdown game: %s", selected_sd))
        sd_sim   <- rv$simulation_results[Player %in% sd_meta$Player]
        
        sd_config <- list(salary_cap     = rv$config$salary_caps$SD,
                          max_lineups    = 5000,
                          percentiles    = c(0.01, 0.05, 0.10, 0.20),
                          platform_col   = "DKScore",
                          cpt_multiplier = 1.5)
        progress$set(message=sprintf("Finding optimal CBB Showdown lineups (%s)...", selected_sd),
                     detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data <- find_optimal_lineups_cbb_sd(sd_sim, sd_meta, sd_config, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        opt_data_sd  <- prepare_optimization_data(sd_sim, sd_meta, "SD")
        score_matrix <- score_all_lineups(lineup_data, opt_data_sd, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, sd_config,
                                                        ownership_data=NULL, verbose=TRUE)
        if ("AvgOwn" %in% names(final_results)) final_results[, AvgOwn := NULL]
        rv$sd_optimal_lineups <- final_results
        
      } else if (rv$sport == "NBA") {
        sd_meta <- copy(rv$sim_metadata); setDT(sd_meta)
        if (!all(c("CPTSalary","SDSalary") %in% names(sd_meta)))
          stop("Missing CPTSalary/SDSalary in metadata. Check SD_IDs sheets.")
        selected_sd <- if (!is.null(input$sd_game_select)) input$sd_game_select else
          rv$input_data$games[!is.na(ShowdownFile) & ShowdownFile != "", ShowdownFile[1]]
        sd_meta <- sd_meta[ShowdownFile == selected_sd]
        if (nrow(sd_meta) == 0) stop(sprintf("No players found for showdown: %s", selected_sd))
        sd_sim <- rv$simulation_results[Player %in% sd_meta$Player]
        nba_sd_config <- list(salary_cap    = rv$config$salary_caps$SD,
                              max_lineups   = 5000,
                              percentiles   = c(0.01, 0.05, 0.10, 0.20),
                              platform_col  = "DKScore",
                              cpt_multiplier = 1.5)
        progress$set(message=sprintf("Finding optimal NBA Showdown lineups (%s)...", selected_sd),
                     detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data  <- find_optimal_lineups_nba_sd(sd_sim, sd_meta, nba_sd_config, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        nba_sd_sim   <- copy(sd_sim); setDT(nba_sd_sim)
        score_matrix <- score_all_lineups(lineup_data, nba_sd_sim, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, nba_sd_config,
                                                        ownership_data=NULL, verbose=TRUE)
        if ("AvgOwn" %in% names(final_results)) final_results[, AvgOwn := NULL]
        rv$sd_optimal_lineups <- final_results
        
      } else if (rv$sport == "SOCCER") {
        sd_meta <- copy(rv$sim_metadata); setDT(sd_meta)
        if (!all(c("CPTSalary","SDSalary") %in% names(sd_meta)))
          stop("Missing CPTSalary/SDSalary. Ensure SD files were included in the input.")
        selected_sd <- if (!is.null(input$sd_game_select)) input$sd_game_select else {
          sdf <- unique(sd_meta[!is.na(ShowdownFile) & ShowdownFile != "", ShowdownFile])
          if (length(sdf)) sdf[1] else stop("No ShowdownFile found in metadata.")
        }
        sd_meta_filt <- sd_meta[ShowdownFile == selected_sd]
        if (nrow(sd_meta_filt) == 0) stop(sprintf("No players for showdown: %s", selected_sd))
        sd_sim <- rv$simulation_results[Player %in% sd_meta_filt$Player]
        soccer_sd_config <- list(salary_cap     = rv$config$salary_caps$SD,
                                 max_lineups   = 5000,
                                 percentiles   = c(0.01, 0.05, 0.10, 0.20),
                                 platform_col  = "DKScore",
                                 cpt_multiplier = 1.5)
        progress$set(message=sprintf("Finding optimal Soccer Showdown lineups (%s)...", selected_sd),
                     detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data <- find_optimal_lineups_soccer_sd(sd_sim, sd_meta_filt, soccer_sd_config, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        soccer_sd_sim <- copy(sd_sim); setDT(soccer_sd_sim)
        score_matrix  <- score_all_lineups(lineup_data, soccer_sd_sim, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, soccer_sd_config,
                                                        ownership_data=NULL, verbose=TRUE)
        if ("AvgOwn" %in% names(final_results)) final_results[, AvgOwn := NULL]
        rv$sd_optimal_lineups <- final_results
        
      } else {
        sd_mode    <- rv$config$optimization_modes$SD %||% "captain"
        opt_data   <- prepare_optimization_data(rv$simulation_results, rv$sim_metadata, "SD")
        # MMA showdown: small player pool (14 players) so parallel overhead hurts.
        # Trim percentiles to the two meaningful contest structures for SD.
        # max_lineups capped at actual universe size to avoid duplicate work.
        n_sd_players  <- length(unique(opt_data$Player))
        n_flex        <- (rv$config$roster_sizes$SD %||% 6L) - 1L  # roster minus CPT slot
        sd_universe   <- n_sd_players * choose(n_sd_players - 1L, n_flex)
        sd_cap         <- rv$config$max_lineups %||% 5000L
        sd_max_lineups <- min(sd_cap, max(500L, sd_universe))
        opt_config <- list(roster_size=rv$config$roster_sizes$SD, salary_cap=rv$config$salary_caps$SD,
                           percentiles=c(0.01,0.05,0.10,0.20), platform_col="DKScore",
                           cpt_multiplier=1.5, progress_frequency=500,
                           use_parallel=FALSE, max_lineups=sd_max_lineups)
        progress$set(message="Finding optimal Showdown lineups...",
                     detail="Phase 1: Building lineup pool...", value=0.05)
        lineup_data  <- find_optimal_lineups(opt_data, opt_config, mode=sd_mode, k=1, verbose=TRUE)
        progress$set(detail=sprintf("Phase 2: Scoring %s lineups...",
                                    format(nrow(lineup_data$unique_lineups), big.mark=",")), value=0.35)
        score_matrix <- score_all_lineups(lineup_data, opt_data, verbose=TRUE)
        progress$set(detail="Phase 3: Calculating metrics...", value=0.70)
        own_data <- copy(rv$sim_metadata)
        # Use SDOwn for showdown ownership; fall back to DKOwn for standard slates
        own_col_sd <- if ("SDOwn" %in% names(own_data) && !all(is.na(own_data$SDOwn))) {
          "SDOwn"
        } else if ("DKOwn" %in% names(own_data) && !all(is.na(own_data$DKOwn))) {
          "DKOwn"
        } else { NULL }
        if (!is.null(own_col_sd)) {
          setnames(own_data, own_col_sd, "Own")
          if (max(own_data$Own, na.rm=TRUE) > 1) own_data[, Own := Own / 100]
        }
        final_results <- calculate_distribution_metrics(score_matrix, lineup_data, opt_config,
                                                        ownership_data=own_data, verbose=TRUE)
        progress$set(detail="Phase 3: Adding custom metrics...", value=0.90)
        final_results <- add_custom_metrics(final_results, rv$sim_metadata, rv$config)
        for (wc in intersect(c("TotalEW","Win6Pct","Win5PlusPct"), names(final_results)))
          final_results[, (wc) := NULL]
        rv$sd_optimal_lineups <- final_results
      }
      progress$set(detail="Complete!", value=1.0)
      showNotification(sprintf("Found %d optimal Showdown lineups!", nrow(rv$sd_optimal_lineups)), type="message")
    }, error=function(e) {
      showNotification(paste("SD error:", e$message), type="error", duration=NULL)
      cat("SD error:\n"); print(e)
    })
  })
  
  
  # ==========================================================================
  # DOWNLOAD HANDLERS - SCORING TAB
  # ==========================================================================
  
  output$dk_download <- downloadHandler(
    filename=function() paste0("DK_Optimal_Lineups_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) { dl <- if (isTRUE(rv$sport == "CBB")) create_download_cbb(rv$dk_optimal_lineups, rv$sim_metadata, "DK") else if (isTRUE(rv$sport == "NBA")) create_download_nba(rv$dk_optimal_lineups, rv$sim_metadata, "DK") else create_download_table(rv$dk_optimal_lineups, rv$sim_metadata, "DK", rv$sport); fwrite(dl, file) })
  output$fd_download <- downloadHandler(
    filename=function() paste0("FD_Optimal_Lineups_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) { dl <- if (isTRUE(rv$sport == "CBB")) create_download_cbb(rv$fd_optimal_lineups, rv$sim_metadata, "FD") else if (isTRUE(rv$sport == "NBA")) create_download_nba(rv$fd_optimal_lineups, rv$sim_metadata, "FD") else create_download_table(rv$fd_optimal_lineups, rv$sim_metadata, "FD", rv$sport); fwrite(dl, file) })
  output$sd_download <- downloadHandler(
    filename=function() paste0("SD_Optimal_Lineups_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) fwrite(create_download_table(rv$sd_optimal_lineups, rv$sim_metadata, "SD", rv$sport), file))
  output$download_full_sim_results <- downloadHandler(
    filename=function() paste0("NASCAR_Full_Sim_Results_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) { req(rv$sport=="NASCAR", rv$sport_visuals$full_results)
      fwrite(rv$sport_visuals$full_results, file) })
  
  output$download_mma_sim_results <- downloadHandler(
    filename=function() paste0("MMA_Full_Sim_Results_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) { req(rv$sport=="MMA", rv$simulation_results)
      dl <- merge(copy(rv$simulation_results),
                  rv$sim_metadata[, .(Player, DKSalary, FDSalary, DKOwn, FDOwn, WinProb)],
                  by="Player", all.x=TRUE)
      setcolorder(dl, c("Player","SimID","DKScore","FDScore","Win","Outcome",
                        "DKSalary","FDSalary","DKOwn","FDOwn","WinProb"))
      fwrite(dl, file) })
  
  output$download_cbb_sim_results <- downloadHandler(
    filename=function() paste0("CBB_Full_Sim_Results_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content=function(file) { req(rv$sport=="CBB", rv$simulation_results)
      dl <- merge(copy(rv$simulation_results),
                  unique(rv$sim_metadata[, .(Player, DKSalary, DKOwn, Team, PosGroup)], by="Player"),
                  by="Player", all.x=TRUE)
      front  <- c("Player","Team","PosGroup","DKSalary","DKOwn","SimID","DKScore")
      stats  <- intersect(c("pts","tpm","reb","ast","stl","blk","to"), names(dl))
      setcolorder(dl, c(front, stats))
      fwrite(dl, file) })
  
  
  # ==========================================================================
  # SCORING TAB UI
  # ==========================================================================
  
  output$scoring_tabs_ui <- renderUI({
    req(rv$config)
    sd_game_selector <- if (isTRUE(rv$sport %in% c("CBB","NBA","SOCCER")) && "SD" %in% rv$config$platforms &&
                            !is.null(rv$input_data$games)) {
      games_with_sd <- rv$input_data$games[!is.na(ShowdownFile) & ShowdownFile != ""]
      if (nrow(games_with_sd) > 1) {
        choices <- setNames(games_with_sd$ShowdownFile, games_with_sd$GameKey)
        div(id = "sd_game_pills", style = "margin-bottom:14px;",
            span(class = "gts-sr-label",
                 style = "margin-right:10px;color:#FFE500;font-size:11px;font-weight:700;letter-spacing:.06em;",
                 "SD GAME:"),
            lapply(seq_along(choices), function(i) {
              tags$button(
                class   = paste("gts-pill", if (i == 1) "active" else ""),
                onclick = sprintf(
                  "Shiny.setInputValue('sd_game_select','%s',{priority:'event'});
                   document.querySelectorAll('#sd_game_pills .gts-pill').forEach(function(b){b.classList.remove('active')});
                   this.classList.add('active')", choices[i]),
                names(choices)[i]
              )
            })
        )
      }
    }
    # Use available_platforms() so FD/SD are hidden when data is absent
    active_plats <- available_platforms()
    fluidRow(box(title="Lineup Scoring", status="warning", solidHeader=TRUE, width=12,
                 p("Find and score optimal lineups across all platforms:"),
                 sd_game_selector,
                 fluidRow(lapply(active_plats, function(platform) {
                   pname <- switch(platform,"DK"="DraftKings","FD"="FanDuel","SD"="Showdown")
                   column(6, actionButton(paste0("run_",tolower(platform),"_optimization"),
                                          paste("Score", pname), class="btn-warning btn-block",
                                          style="margin-bottom:10px;font-size:16px;padding:12px;"))
                 })),
                 hr(),
                 uiOutput("download_buttons_ui"),
                 hr(),
                 div(style="margin-bottom:15px;", uiOutput("view_platform_ui")),
                 DTOutput("lineup_results_table")
    ))
  })
  
  output$download_buttons_ui <- renderUI({
    ready <- c()
    if (!is.null(rv$dk_optimal_lineups)) ready <- c(ready, "DK")
    if (!is.null(rv$fd_optimal_lineups)) ready <- c(ready, "FD")
    if (!is.null(rv$sd_optimal_lineups)) ready <- c(ready, "SD")
    if (length(ready) == 0) return(p("Score lineups to enable downloads", style="color:#999;"))
    fluidRow(lapply(ready, function(p) {
      pname <- switch(p,"DK"="DraftKings","FD"="FanDuel","SD"="Showdown")
      column(6, downloadButton(paste0(tolower(p),"_download"), paste("Download All",p,"Lineups"),
                               class="btn-block",
                               style="background-color:#4caf50!important;border-color:#4caf50!important;color:white!important;font-size:14px;padding:10px;"))
    }))
  })
  
  output$view_platform_ui <- renderUI({
    ready <- c(); labels <- c()
    if (!is.null(rv$dk_optimal_lineups)) { ready <- c(ready,"DK"); labels <- c(labels,"DraftKings") }
    if (!is.null(rv$fd_optimal_lineups)) { ready <- c(ready,"FD"); labels <- c(labels,"FanDuel") }
    if (!is.null(rv$sd_optimal_lineups)) { ready <- c(ready,"SD"); labels <- c(labels,"Showdown") }
    if (length(ready) == 0) return(p("Score lineups to view results", style="color:#999;font-style:italic;"))
    div(style="margin-bottom:15px;",
        tags$label("View Results:", style="color:#FFE500;font-weight:bold;display:block;margin-bottom:10px;"),
        radioButtons("view_platform", label=NULL,
                     choices=setNames(ready, labels), selected=ready[1], inline=TRUE)
    )
  })
  
  output$lineup_results_table <- renderDT({
    req(input$view_platform)
    optimal <- switch(input$view_platform,
                      "DK"=rv$dk_optimal_lineups, "FD"=rv$fd_optimal_lineups, "SD"=rv$sd_optimal_lineups)
    req(optimal)
    display_table <- if (isTRUE(rv$sport == "CBB")) create_display_table_cbb(optimal, input$view_platform) else if (isTRUE(rv$sport == "NBA")) create_display_table_nba(optimal, input$view_platform) else create_display_table(optimal, rv$sim_metadata, input$view_platform)
    dt <- datatable(display_table,
                    options=list(pageLength=50, searching=FALSE, lengthChange=FALSE, scrollX=TRUE, dom='tp',
                                 order=list(list(which(names(display_table)=="Win")-1,'desc'))),
                    rownames=FALSE) %>%
      formatRound(intersect(c("Win","Top1","Top5","Top10","Top20","AvgOwn"),
                            names(display_table)), 1)
    if ("Salary" %in% names(display_table)) dt <- dt %>% formatCurrency("Salary","$",digits=0)
    if ("TotalStart" %in% names(display_table)) dt <- dt %>% formatRound(c("TotalStart","AvgStart"),1)
    for (gc in intersect(c("ExpectedCuts","AtLeast6","AtLeast5","EarlyLateCount"), names(display_table)))
      dt <- dt %>% formatRound(gc, 1)
    dt
  })
  
  
  # ==========================================================================
  # PORTFOLIO TABS UI
  # ==========================================================================
  
  output$portfolio_tabs_ui <- renderUI({
    req(rv$config)
    
    active_platforms <- Filter(function(p) {
      lp <- tolower(p)
      !is.null(rv[[paste0(lp, "_optimal_lineups")]])
    }, available_platforms())
    if (length(active_platforms) == 0) active_platforms <- available_platforms()
    
    tab_panels <- lapply(active_platforms, function(platform) {
      lp    <- tolower(platform)
      pname <- switch(platform,"DK"="DraftKings","FD"="FanDuel","SD"="Showdown")
      
      tabPanel(title=pname,
               fluidRow(column(12,
                               div(style="text-align:right;margin-bottom:10px;padding:5px;background-color:#1a1a1a;border-radius:4px;",
                                   h4(textOutput(paste0(lp,"_portfolio_count")),
                                      style="display:inline-block;color:#FFE500;margin-right:20px;vertical-align:middle;font-size:16px;"),
                                   actionButton(paste0(lp,"_clear_portfolio"),"CLEAR PORTFOLIO",
                                                class="btn-danger", style="margin-right:10px;"),
                                   downloadButton(paste0(lp,"_download_portfolio"),"DOWNLOAD PORTFOLIO", class="btn-success")
                               )
               )),
               
               tabsetPanel(id=paste0("portfolio_",lp,"_tabs"),
                           
                           tabPanel("Filtered Pool",
                                    fluidRow(box(title="Lineup Filters", status="warning", solidHeader=TRUE,
                                                 width=12, collapsible=TRUE,
                                                 fluidRow(
                                                   column(2,
                                                          div(style="background-color:#2d2d2d;padding:6px;border-radius:4px;border:1px solid #404040;",
                                                              h6("Min Rates", style="color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
                                                              div(style="display:flex;align-items:center;margin-bottom:4px;",
                                                                  tags$label("Win:", style="color:#FFE500;font-size:11px;margin:0 5px 0 0;width:30px;"),
                                                                  numericInput(paste0(lp,"_min_win"),  NULL,value=0,min=0,max=100,step=0.01,width="68px")),
                                                              div(style="display:flex;align-items:center;margin-bottom:4px;",
                                                                  tags$label("Top1:", style="color:#FFE500;font-size:11px;margin:0 5px 0 0;width:30px;"),
                                                                  numericInput(paste0(lp,"_min_top1"), NULL,value=0,min=0,max=100,step=0.5,width="68px")),
                                                              div(style="display:flex;align-items:center;margin-bottom:4px;",
                                                                  tags$label("Top5:", style="color:#FFE500;font-size:11px;margin:0 5px 0 0;width:30px;"),
                                                                  numericInput(paste0(lp,"_min_top5"), NULL,value=0,min=0,max=100,step=1,  width="68px")),
                                                              div(style="display:flex;align-items:center;margin-bottom:4px;",
                                                                  tags$label("Top10:", style="color:#FFE500;font-size:11px;margin:0 5px 0 0;width:30px;"),
                                                                  numericInput(paste0(lp,"_min_top10"),NULL,value=0,min=0,max=100,step=2,  width="68px")),
                                                              div(style="display:flex;align-items:center;margin-bottom:0;",
                                                                  tags$label("Top20:", style="color:#FFE500;font-size:11px;margin:0 5px 0 0;width:30px;"),
                                                                  numericInput(paste0(lp,"_min_top20"),NULL,value=0,min=0,max=100,step=5,  width="68px"))
                                                          )
                                                   ),
                                                   column(4,
                                                          div(style="padding-left:4px;",
                                                              h6("Ranges", style="color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
                                                              uiOutput(paste0(lp,"_range_sliders"))
                                                          )
                                                   ),
                                                   column(3, uiOutput(paste0(lp, "_lock_exclude_ui"))),
                                                   column(3,
                                                          div(style="background-color:#2d2d2d;padding:8px;border-radius:4px;border:1px solid #FFE500;",
                                                              h6("Add to Portfolio", style="color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
                                                              numericInput(paste0(lp,"_num_lineups"), "Lineups:", value=20, min=1, max=150, width="100%"),
                                                              textInput(paste0(lp,"_build_label"), "Label:", value="", placeholder="Optional", width="100%"),
                                                              h5(textOutput(paste0(lp,"_filtered_count")),
                                                                 style="color:#FFE500;font-weight:bold;text-align:center;margin:8px 0 6px 0;"),
                                                              actionButton(paste0(lp,"_add_build"), "ADD TO PORTFOLIO",
                                                                           class="btn-primary", style="width:100%;font-weight:bold;")
                                                          )
                                                   )
                                                 )
                                    )),
                                    fluidRow(box(title="Player Exposure in Filtered Pool",status="info",solidHeader=TRUE,width=12,
                                                 DTOutput(paste0(lp,"_filtered_exposure"))))
                           ),
                           
                           tabPanel("Portfolio Summary",
                                    fluidRow(box(title="Portfolio Overview",status="warning",solidHeader=TRUE,width=12,
                                                 hr(style="border-color:#FFE500;"),
                                                 div(style="margin-bottom:8px;",
                                                     actionButton(paste0(lp,"_delete_selected_builds"), "DELETE SELECTED BUILDS",
                                                                  class="btn-danger btn-sm", style="font-weight:bold;")),
                                                 DTOutput(paste0(lp,"_builds_summary")))),
                                    fluidRow(box(title="Portfolio Player Exposure",status="info",solidHeader=TRUE,width=12,
                                                 DTOutput(paste0(lp,"_portfolio_exposure"))))
                           ),
                           
                           tabPanel("Portfolio Lineups",
                                    fluidRow(box(title="All Portfolio Lineups",status="info",solidHeader=TRUE,width=12,
                                                 div(style="margin-bottom:8px;",
                                                     actionButton(paste0(lp,"_delete_selected_lineups"), "DELETE SELECTED LINEUPS",
                                                                  class="btn-danger btn-sm", style="font-weight:bold;")),
                                                 DTOutput(paste0(lp,"_portfolio_lineups"))))
                           )
               )
      )
    })
    
    do.call(tabBox, c(list(id="portfolio_platform", width=12), tab_panels))
  })
  
  
  # ==========================================================================
  # LOCK / EXCLUDE UI
  # ==========================================================================
  
  make_lock_exclude_ui <- function(lp) {
    renderUI({
      lu  <- rv[[paste0(lp, "_optimal_lineups")]]
      div(style="background-color:#2d2d2d;padding:8px;border-radius:4px;border:1px solid #404040;",
          h6("Lock / Exclude", style="color:#FFE500;font-weight:bold;margin:0 0 8px 0;font-size:13px;"),
          if (isTRUE(rv$sport == "F1")) {
            drv_choices <- if (!is.null(lu)) sort(unique(unlist(lu[, grep("^Captain|^Util[1-4]$", names(lu), value=TRUE), with=FALSE]))) else NULL
            con_choices <- if (!is.null(lu)) sort(unique(unlist(lu[, grep("^Util5$",             names(lu), value=TRUE), with=FALSE]))) else NULL
            tagList(
              tags$label("Captain Lock:",      style="color:#aaa;font-size:11px;"),
              selectizeInput(paste0(lp,"_locked_captain"),      NULL, choices=drv_choices, multiple=TRUE, selected=character(0),
                             options=list(plugins=list('remove_button'), placeholder='Lock captain', maxItems=1), width="100%"),
              tags$label("Captain Exclude:",   style="color:#aaa;font-size:11px;"),
              selectizeInput(paste0(lp,"_excluded_captain"),    NULL, choices=drv_choices, multiple=TRUE, selected=character(0),
                             options=list(plugins=list('remove_button'), placeholder='Exclude captain'), width="100%"),
              tags$label("Driver Lock:",       style="color:#aaa;font-size:11px;"),
              selectizeInput(paste0(lp,"_locked_players"),      NULL, choices=drv_choices, multiple=TRUE, selected=character(0),
                             options=list(plugins=list('remove_button'), placeholder='Lock flex driver', maxItems=4), width="100%"),
              tags$label("Driver Exclude:",    style="color:#aaa;font-size:11px;"),
              selectizeInput(paste0(lp,"_excluded_players"),    NULL, choices=drv_choices, multiple=TRUE, selected=character(0),
                             options=list(plugins=list('remove_button'), placeholder='Exclude flex driver'), width="100%"),
              tags$label("Constructor Lock:",  style="color:#aaa;font-size:11px;"),
              selectizeInput(paste0(lp,"_locked_constructor"),  NULL, choices=con_choices, multiple=TRUE, selected=character(0),
                             options=list(plugins=list('remove_button'), placeholder='Lock constructor', maxItems=1), width="100%"),
              tags$label("Constructor Exclude:", style="color:#aaa;font-size:11px;"),
              selectizeInput(paste0(lp,"_excluded_constructor"), NULL, choices=con_choices, multiple=TRUE, selected=character(0),
                             options=list(plugins=list('remove_button'), placeholder='Exclude constructor'), width="100%")
            )
          } else {
            ver <- rv[[paste0(lp, "_lock_v")]]
            all_players <- if (!is.null(lu)) {
              pc <- grep("^Player|^Captain|^MVP|^Util", names(lu), value=TRUE)
              sort(unique(unlist(lu[, ..pc]))[!is.na(unique(unlist(lu[, ..pc]))) & unique(unlist(lu[, ..pc])) != ""])
            } else NULL
            tagList(
              selectizeInput(paste0(lp, "_locked_players_v",  ver), "Lock:",
                             choices=all_players, multiple=TRUE, selected=character(0),
                             options=list(plugins=list('remove_button'), placeholder='Search to lock players', maxItems=8),
                             width="100%"),
              selectizeInput(paste0(lp, "_excluded_players_v", ver), "Exclude:",
                             choices=all_players, multiple=TRUE, selected=character(0),
                             options=list(plugins=list('remove_button'), placeholder='Search to exclude players'),
                             width="100%")
            )
          }
      )
    })
  }
  output$dk_lock_exclude_ui <- make_lock_exclude_ui("dk")
  output$fd_lock_exclude_ui <- make_lock_exclude_ui("fd")
  output$sd_lock_exclude_ui <- make_lock_exclude_ui("sd")
  
  
  # ==========================================================================
  # RANGE SLIDERS
  # ==========================================================================
  
  make_range_sliders <- function(lp) {
    renderUI({
      optimal <- rv[[paste0(lp,"_optimal_lineups")]]; req(optimal)
      ver <- rv[[paste0(lp,"_slider_v")]]
      num_cols  <- names(optimal)[sapply(optimal, is.numeric)]
      num_cols  <- setdiff(num_cols, grep("^Player|^Captain|^MVP", names(optimal), value=TRUE))
      range_cols <- setdiff(num_cols, c("WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct","ExpectedCuts",
                                        "TotalEW","Win6Pct","Win5PlusPct"))
      cfg_map <- list(
        TotalSalary=list(label="Salary",format="k",step=0.1),
        AvgOwn=list(label="Avg Own",format="decimal",step=0.1),
        CumulativeStarting=list(label="Total Start",format="whole",step=1),
        GeometricMeanStarting=list(label="Avg Start",format="decimal",step=0.1),
        AvgStart=list(label="Avg Start",format="decimal",step=0.1),
        AtLeast6=list(label="All 6 Cut%",format="decimal",step=1),
        AtLeast5=list(label="5+ Cut%",format="decimal",step=1),
        EarlyLateCount=list(label="Early/Late Golfers",format="whole",step=1),
        TotalEW=list(label="Exp Wins",format="decimal",step=0.1),
        Win6Pct=list(label="All Win%",format="decimal",step=1),
        Win5PlusPct=list(label="5+ Win%",format="decimal",step=1)
      )
      sliders <- Filter(Negate(is.null), lapply(range_cols, function(col) {
        cfg <- cfg_map[[col]] %||% list(label=col,format="decimal",step=0.1)
        mn  <- min(optimal[[col]],na.rm=TRUE); mx <- max(optimal[[col]],na.rm=TRUE)
        if (mn == mx) return(NULL)
        if (cfg$format=="k")     { mn <- floor(mn/1000);  mx <- ceiling(mx/1000); lbl <- paste0(cfg$label," (K)") }
        else if (cfg$format=="whole") { mn <- floor(mn); mx <- ceiling(mx); lbl <- cfg$label }
        else { mn <- floor(mn*10)/10; mx <- ceiling(mx*10)/10; lbl <- cfg$label }
        sliderInput(paste0(lp,"_filter_",col,"_v",ver), lbl, min=mn, max=mx, value=c(mn,mx), step=cfg$step, width="100%")
      }))
      n <- length(sliders)
      fluidRow(column(6, sliders[seq(1,n,2)]),
               column(6, if(n>1) sliders[seq(2,n,2)] else list()))
    })
  }
  output$dk_range_sliders <- make_range_sliders("dk")
  output$fd_range_sliders <- make_range_sliders("fd")
  output$sd_range_sliders <- make_range_sliders("sd")
  
  
  # ==========================================================================
  # VERSION-BASED LOCK/EXCLUDE RESET
  # ==========================================================================
  
  observeEvent(rv$dk_optimal_lineups, { rv$dk_lock_v <- rv$dk_lock_v + 1L; rv$dk_slider_v <- rv$dk_slider_v + 1L })
  observeEvent(rv$fd_optimal_lineups, { rv$fd_lock_v <- rv$fd_lock_v + 1L; rv$fd_slider_v <- rv$fd_slider_v + 1L })
  observeEvent(rv$sd_optimal_lineups, { rv$sd_lock_v <- rv$sd_lock_v + 1L; rv$sd_slider_v <- rv$sd_slider_v + 1L })
  
  
  # ==========================================================================
  # FILTERED LINEUPS
  # ==========================================================================
  
  make_filtered_lineups <- function(lp) {
    reactive({
      optimal <- rv[[paste0(lp,"_optimal_lineups")]]; req(optimal)
      lineups <- copy(optimal)
      rate_pairs <- list(c("WinRate","win"),c("Top1Pct","top1"),c("Top5Pct","top5"),
                         c("Top10Pct","top10"),c("Top20Pct","top20"))
      for (rp in rate_pairs) {
        v <- input[[paste0(lp,"_min_",rp[2])]]
        if (!is.null(v) && v > 0 && rp[1] %in% names(lineups))
          lineups <- lineups[get(rp[1]) >= v]
      }
      slider_ver <- rv[[paste0(lp,"_slider_v")]]
      sv <- input[[paste0(lp,"_filter_TotalSalary_v",slider_ver)]]
      if (!is.null(sv) && "TotalSalary" %in% names(lineups))
        lineups <- lineups[TotalSalary >= sv[1]*1000 & TotalSalary <= sv[2]*1000]
      num_cols   <- names(lineups)[sapply(lineups, is.numeric)]
      num_cols   <- setdiff(num_cols, grep("^Player|^Captain|^MVP",names(lineups),value=TRUE))
      range_cols <- setdiff(num_cols, c("WinRate","Top1Pct","Top5Pct","Top10Pct","Top20Pct","TotalSalary"))
      for (col in range_cols) {
        fv <- input[[paste0(lp,"_filter_",col,"_v",slider_ver)]]
        if (!is.null(fv)) lineups <- lineups[get(col) >= fv[1] & get(col) <= fv[2]]
      }
      
      if (isTRUE(rv$sport == "F1")) {
        cpt_cols  <- grep("^Captain",    names(lineups), value=TRUE)
        flex_cols <- grep("^Util[1-4]$", names(lineups), value=TRUE)
        con_cols  <- grep("^Util5$",     names(lineups), value=TRUE)
        locked_cpt <- input[[paste0(lp,"_locked_captain")]];     locked_cpt  <- locked_cpt[!is.null(locked_cpt)  & locked_cpt  != ""]
        excl_cpt   <- input[[paste0(lp,"_excluded_captain")]];   excl_cpt    <- excl_cpt[!is.null(excl_cpt)    & excl_cpt    != ""]
        locked_drv <- input[[paste0(lp,"_locked_players")]];     locked_drv  <- locked_drv[!is.null(locked_drv)  & locked_drv  != ""]
        excl_drv   <- input[[paste0(lp,"_excluded_players")]];   excl_drv    <- excl_drv[!is.null(excl_drv)    & excl_drv    != ""]
        locked_con <- input[[paste0(lp,"_locked_constructor")]]; locked_con  <- locked_con[!is.null(locked_con)  & locked_con  != ""]
        excl_con   <- input[[paste0(lp,"_excluded_constructor")]]; excl_con  <- excl_con[!is.null(excl_con)    & excl_con    != ""]
        if (length(locked_cpt) > 0 && length(cpt_cols) > 0)
          lineups <- lineups[apply(lineups[,..cpt_cols],1,function(r) all(locked_cpt %in% r))]
        if (length(excl_cpt)   > 0 && length(cpt_cols) > 0)
          lineups <- lineups[apply(lineups[,..cpt_cols],1,function(r) !any(excl_cpt %in% r))]
        if (length(locked_drv) > 0 && length(flex_cols) > 0)
          lineups <- lineups[apply(lineups[,..flex_cols],1,function(r) all(locked_drv %in% r))]
        if (length(excl_drv)   > 0 && length(flex_cols) > 0)
          lineups <- lineups[apply(lineups[,..flex_cols],1,function(r) !any(excl_drv %in% r))]
        if (length(locked_con) > 0 && length(con_cols) > 0)
          lineups <- lineups[apply(lineups[,..con_cols],1,function(r) all(locked_con %in% r))]
        if (length(excl_con)   > 0 && length(con_cols) > 0)
          lineups <- lineups[apply(lineups[,..con_cols],1,function(r) !any(excl_con %in% r))]
      } else {
        ver    <- rv[[paste0(lp,"_lock_v")]]
        locked <- input[[paste0(lp,"_locked_players_v",ver)]]
        locked <- locked[!is.null(locked) & locked != ""]
        if (length(locked) > 0) {
          pc <- grep("^Player|^Captain|^MVP|^Util",names(lineups),value=TRUE)
          lineups <- lineups[apply(lineups[,..pc],1,function(r) all(locked %in% r))]
        }
        excluded <- input[[paste0(lp,"_excluded_players_v",ver)]]
        excluded <- excluded[!is.null(excluded) & excluded != ""]
        if (length(excluded) > 0) {
          pc <- grep("^Player|^Captain|^MVP|^Util",names(lineups),value=TRUE)
          lineups <- lineups[apply(lineups[,..pc],1,function(r) !any(excluded %in% r))]
        }
      }
      lineups
    })
  }
  dk_filtered_lineups <- make_filtered_lineups("dk")
  fd_filtered_lineups <- make_filtered_lineups("fd")
  sd_filtered_lineups <- make_filtered_lineups("sd")
  
  output$dk_filtered_count <- renderText({ paste0("Filtered Pool: ", nrow(dk_filtered_lineups()), " lineups") })
  output$fd_filtered_count <- renderText({ paste0("Filtered Pool: ", nrow(fd_filtered_lineups()), " lineups") })
  output$sd_filtered_count <- renderText({ paste0("Filtered Pool: ", nrow(sd_filtered_lineups()), " lineups") })
  
  
  # ==========================================================================
  # FILTERED EXPOSURE
  # ==========================================================================
  
  make_filtered_exposure <- function(filtered_reactive, platform) {
    renderDT({
      req(filtered_reactive(), rv$sim_metadata)
      filtered   <- filtered_reactive()
      is_f1      <- isTRUE(rv$sport == "F1")
      is_cbb     <- isTRUE(rv$sport %in% c("CBB","NBA"))
      is_nba     <- isTRUE(rv$sport == "NBA")
      is_sd      <- platform == "SD"
      salary_col <- if (is_sd) "DKSalary" else paste0(platform, "Salary")
      own_col    <- if (is_sd) NULL        else paste0(platform, "Own")
      cpt_cols  <- grep("^Captain", names(filtered), value=TRUE)
      util_cols <- grep("^Util",    names(filtered), value=TRUE)
      all_pc    <- grep("^Player|^Captain|^MVP|^Util|^G[1-4]$|^F[1-3]$|^C1$", names(filtered), value=TRUE)
      has_captain <- length(cpt_cols) > 0
      n_lineups  <- nrow(filtered)
      all_counts <- table(unlist(filtered[, ..all_pc]))
      meta_players <- if (is_sd) {
        rv$sim_metadata[!is.na(SDSalary) & SDSalary > 0, Player]
      } else {
        rv$sim_metadata$Player
      }
      exp_tbl <- data.table(Player = meta_players, Exposure = 0)
      for (i in seq_len(nrow(exp_tbl))) {
        p <- exp_tbl$Player[i]
        if (p %in% names(all_counts)) exp_tbl$Exposure[i] <- as.numeric(all_counts[p]) / n_lineups * 100
      }
      if (has_captain || length(util_cols) > 0) {
        cpt_counts  <- if (length(cpt_cols))  table(unlist(filtered[, ..cpt_cols]))  else table(character(0))
        util_counts <- if (length(util_cols)) table(unlist(filtered[, ..util_cols])) else table(character(0))
        exp_tbl[, CptExp  := 0]
        exp_tbl[, UtilExp := 0]
        for (i in seq_len(nrow(exp_tbl))) {
          p <- exp_tbl$Player[i]
          if (p %in% names(cpt_counts))  exp_tbl$CptExp[i]  <- as.numeric(cpt_counts[p])  / n_lineups * 100
          if (p %in% names(util_counts)) exp_tbl$UtilExp[i] <- as.numeric(util_counts[p]) / n_lineups * 100
        }
        if (is_f1) {
          exp_tbl[Player %in% rv$sim_metadata$Player[rv$sim_metadata$PlayerType == "Constructor"],
                  c("CptExp","UtilExp") := NA_real_]
        }
        # Round exposure percentages to 1 decimal
        if ("CptExp"  %in% names(exp_tbl)) exp_tbl[, CptExp  := round(CptExp, 1)]
        if ("UtilExp" %in% names(exp_tbl)) exp_tbl[, UtilExp := round(UtilExp, 1)]
      }
      
      # ── Build metadata column list ─────────────────────────────────────────
      if (is_nba && is_sd) {
        nba_sd_meta <- intersect(c("Player","Team","SDSalary",
                                   "CPTOwn","DKOwn"),
                                 names(rv$sim_metadata))
        meta_cols <- nba_sd_meta
      } else if (is_nba) {
        pos_col_nba <- if (platform == "FD") "FDPos" else "DKPos"
        nba_meta <- intersect(c("Player", pos_col_nba, salary_col, own_col, "Team"),
                              names(rv$sim_metadata))
        meta_cols <- nba_meta
      } else {
        meta_cols <- intersect(c("Player","PlayerType",salary_col,own_col,
                                 "PosGroup","RGProj","RGMin","GameTime","Starting","Team","Car",
                                 "Position","Match","Opponent","Surface","Tour","TeeTimeGroup","CutProb"),
                               names(rv$sim_metadata))
        meta_cols <- meta_cols[!is.na(meta_cols)]
      }
      
      exp_tbl <- merge(exp_tbl, rv$sim_metadata[Player %in% meta_players, ..meta_cols],
                       by="Player", all.x=TRUE)
      
      if (is_nba && is_sd) {
        if ("SDSalary" %in% names(exp_tbl)) setnames(exp_tbl, "SDSalary", "Salary")
        if ("CPTOwn" %in% names(exp_tbl)) {
          if (max(exp_tbl$CPTOwn, na.rm=TRUE) <= 1) exp_tbl[, CPTOwn := CPTOwn * 100]
          setnames(exp_tbl, "CPTOwn", "CptOwn")
        }
        if ("DKOwn" %in% names(exp_tbl)) {
          if (max(exp_tbl$DKOwn, na.rm=TRUE) <= 1) exp_tbl[, DKOwn := DKOwn * 100]
          setnames(exp_tbl, "DKOwn", "UtlOwn")
        }
        if ("UtilExp" %in% names(exp_tbl)) setnames(exp_tbl, "UtilExp", "UtlExp")
        if (all(c("CptExp","CptOwn") %in% names(exp_tbl)))
          exp_tbl[, CptLev := round(CptExp - CptOwn, 1)]
        if (all(c("UtlExp","UtlOwn") %in% names(exp_tbl)))
          exp_tbl[, UtlLev := round(UtlExp - UtlOwn, 1)]
        exp_tbl[, TotExp := round(Exposure, 1)]
        if (all(c("CptOwn","UtlOwn") %in% names(exp_tbl)))
          exp_tbl[, TotOwn := round(CptOwn + UtlOwn, 1)]
        if (all(c("TotExp","TotOwn") %in% names(exp_tbl)))
          exp_tbl[, TotLev := round(TotExp - TotOwn, 1)]
        exp_tbl[, Exposure := NULL]
        meta_order <- intersect(c("Player","Team","Salary"), names(exp_tbl))
        split_cols <- intersect(c("CptExp","CptOwn","CptLev",
                                  "UtlExp","UtlOwn","UtlLev",
                                  "TotExp","TotOwn","TotLev"), names(exp_tbl))
        setcolorder(exp_tbl, c(meta_order, split_cols))
      } else if (is_nba) {
        pos_col_nba <- if (platform == "FD") "FDPos" else "DKPos"
        if (salary_col %in% names(exp_tbl)) setnames(exp_tbl, salary_col, "Sal")
        if (!is_sd && !is.null(own_col) && own_col %in% names(exp_tbl)) {
          setnames(exp_tbl, own_col, "OwnProj")
          if (max(exp_tbl$OwnProj, na.rm = TRUE) <= 1) exp_tbl[, OwnProj := OwnProj * 100]
          exp_tbl[, OwnProj  := round(OwnProj, 1)]
          exp_tbl[, Leverage := round(Exposure - OwnProj, 1)]
        }
        meta_order    <- intersect(c("Player", pos_col_nba, "Sal", "Team"), names(exp_tbl))
        split_cols    <- intersect(c("CptExp","UtilExp","FlexExp"), names(exp_tbl))
        metrics_order <- intersect(c("Exposure","OwnProj","Leverage"), names(exp_tbl))
        setcolorder(exp_tbl, c(meta_order, split_cols, metrics_order))
      } else {
        if (salary_col %in% names(exp_tbl)) setnames(exp_tbl, salary_col, "Salary")
        if (!is_sd && !is.null(own_col) && own_col %in% names(exp_tbl)) {
          setnames(exp_tbl, own_col, "OwnProj")
          if (max(exp_tbl$OwnProj, na.rm = TRUE) <= 1) exp_tbl[, OwnProj := OwnProj * 100]
          exp_tbl[, OwnProj  := round(OwnProj, 1)]
          exp_tbl[, Leverage := round(Exposure - OwnProj, 1)]
        }
        base_meta     <- c("Player", if (is_f1) "PlayerType" else NULL,
                           "PosGroup","Salary","RGProj","RGMin","SimProj","GameTime","Starting","Team","Car",
                           "Position","Match","Opponent","Surface","Tour","TeeTimeGroup","CutProb")
        meta_order    <- intersect(base_meta, names(exp_tbl))
        split_cols    <- intersect(c("CptExp","UtilExp","FlexExp"), names(exp_tbl))
        metrics_order <- intersect(c("Exposure","OwnProj","Leverage"), names(exp_tbl))
        setcolorder(exp_tbl, c(meta_order, split_cols, metrics_order))
        if (is_cbb) {
          rename_map <- c(PosGroup="Pos", Salary="Sal", RGMin="Mins", RGProj="Proj", GameTime="Time", SimProj="GTS")
          for (old in names(rename_map)) if (old %in% names(exp_tbl)) setnames(exp_tbl, old, rename_map[[old]])
        }
      }
      
      # For NBA-SD, Exposure was replaced by TotExp
      exp_sort_col <- if ("Exposure" %in% names(exp_tbl)) "Exposure" else "TotExp"
      exp_tbl <- exp_tbl[get(exp_sort_col) > 0]
      setorderv(exp_tbl, exp_sort_col, order = -1L)
      dt <- datatable(exp_tbl,
                      options=list(pageLength=50,scrollX=TRUE,searching=FALSE,lengthChange=FALSE,dom='tp'),
                      rownames=FALSE)
      rc <- intersect(c("CptExp","CptOwn","CptLev",
                        "UtlExp","UtlOwn","UtlLev",
                        "TotExp","TotOwn","TotLev",
                        "Exposure","FlexExp","OwnProj","Leverage",
                        "CutProb","RGProj","RGMin","Proj","Sim"), names(exp_tbl))
      if (length(rc) > 0) dt <- dt %>% formatRound(rc, 1)
      cap <- rv$config$salary_caps[[platform]] %||% 50000
      sal_col_disp <- if ("Sal" %in% names(exp_tbl)) "Sal" else if ("Salary" %in% names(exp_tbl)) "Salary" else NULL
      if (!is.null(sal_col_disp) && cap >= 1000) dt <- dt %>% formatCurrency(sal_col_disp,"$",digits=0)
      dt
    })
  }
  output$dk_filtered_exposure <- make_filtered_exposure(dk_filtered_lineups, "DK")
  output$fd_filtered_exposure <- make_filtered_exposure(fd_filtered_lineups, "FD")
  output$sd_filtered_exposure <- make_filtered_exposure(sd_filtered_lineups, "SD")
  
  
  # ==========================================================================
  # ADD BUILD
  # ==========================================================================
  
  make_add_build <- function(lp) {
    observeEvent(input[[paste0(lp,"_add_build")]], {
      filtered <- switch(lp,"dk"=dk_filtered_lineups(),"fd"=fd_filtered_lineups(),"sd"=sd_filtered_lineups())
      req(filtered)
      n <- input[[paste0(lp,"_num_lineups")]]
      if (nrow(filtered) < n) { showNotification(paste0("Only ",nrow(filtered)," available."),type="warning"); return() }
      sampled <- filtered[sample(nrow(filtered),n)]
      cnt <- paste0(lp,"_build_counter"); rv[[cnt]] <- rv[[cnt]] + 1
      raw <- input[[paste0(lp,"_build_label")]]
      lbl <- if (is.null(raw)||raw=="") paste0("Build ",rv[[cnt]]) else iconv(raw,to="UTF-8",sub="")
      sampled[, Build := lbl]
      pn <- paste0(lp,"_portfolio")
      rv[[pn]] <- if (is.null(rv[[pn]])) sampled else rbindlist(list(rv[[pn]],sampled),fill=TRUE)
      parts <- c()
      for (rp in list(c("win","Win"),c("top1","Top1"),c("top5","Top5"),c("top10","Top10"),c("top20","Top20"))) {
        v <- input[[paste0(lp,"_min_",rp[1])]]; if(!is.null(v)&&v>0) parts <- c(parts,paste0(rp[2],">=",v))
      }
      rv[[paste0(lp,"_builds")]][[lbl]] <- list(label=lbl, num_lineups=n,
                                                filters=if(length(parts)>0) paste(parts,collapse=" | ") else "No filters")
      showNotification(paste0("Added ",n," lineups as '",lbl,"'"),type="message")
      updateTextInput(session, paste0(lp,"_build_label"), value="")
    })
  }
  make_add_build("dk"); make_add_build("fd"); make_add_build("sd")
  
  
  # ==========================================================================
  # CLEAR / DELETE BUILDS / DELETE LINEUPS / COUNTS
  # ==========================================================================
  
  observeEvent(input$dk_clear_portfolio,{rv$dk_portfolio<-NULL;rv$dk_builds<-list();rv$dk_build_counter<-0;rv$dk_selected_builds<-character(0);showNotification("DK cleared",type="message")})
  observeEvent(input$fd_clear_portfolio,{rv$fd_portfolio<-NULL;rv$fd_builds<-list();rv$fd_build_counter<-0;rv$fd_selected_builds<-character(0);showNotification("FD cleared",type="message")})
  observeEvent(input$sd_clear_portfolio,{rv$sd_portfolio<-NULL;rv$sd_builds<-list();rv$sd_build_counter<-0;rv$sd_selected_builds<-character(0);showNotification("SD cleared",type="message")})
  
  make_delete_build <- function(lp) {
    observeEvent(input[[paste0(lp,"_delete_build")]], {
      req(input[[paste0(lp,"_delete_build")]])
      b <- input[[paste0(lp,"_delete_build")]]; bn <- paste0(lp,"_builds"); pn <- paste0(lp,"_portfolio")
      if (b %in% names(rv[[bn]])) {
        if (!is.null(rv[[pn]])) {
          p <- rv[[pn]][Build!=b]
          rv[[pn]] <- if(nrow(p)==0) NULL else p
        }
        rv[[bn]][[b]] <- NULL
        rv[[paste0(lp,"_selected_builds")]] <- character(0)
        showNotification(paste0("Deleted: ",b),type="message")
      }
    })
  }
  make_delete_build("dk"); make_delete_build("fd"); make_delete_build("sd")
  
  make_delete_lineup <- function(lp) {
    observeEvent(input[[paste0(lp,"_delete_lineup")]], {
      pn <- paste0(lp,"_portfolio"); bn <- paste0(lp,"_builds")
      req(input[[paste0(lp,"_delete_lineup")]], rv[[pn]])
      row <- as.integer(input[[paste0(lp,"_delete_lineup")]])
      db  <- rv[[pn]][row, Build]
      p   <- rv[[pn]][-row]
      if (db %in% names(rv[[bn]])) {
        rv[[bn]][[db]]$num_lineups <- rv[[bn]][[db]]$num_lineups - 1
        if (rv[[bn]][[db]]$num_lineups == 0) rv[[bn]][[db]] <- NULL
      }
      if (nrow(p)==0) { rv[[pn]]<-NULL; rv[[bn]]<-list(); rv[[paste0(lp,"_build_counter")]]<-0 }
      else rv[[pn]] <- p
      showNotification(paste0("Deleted lineup from ",toupper(lp)),type="warning")
    })
  }
  make_delete_lineup("dk"); make_delete_lineup("fd"); make_delete_lineup("sd")
  
  # Delete selected builds via "DELETE SELECTED BUILDS" button
  make_delete_selected_builds <- function(lp) {
    observeEvent(input[[paste0(lp, "_delete_selected_builds")]], {
      sel_rows <- input[[paste0(lp, "_builds_summary_rows_selected")]]
      if (length(sel_rows) == 0) { showNotification("No builds selected.", type="warning"); return() }
      bn <- paste0(lp, "_builds"); pn <- paste0(lp, "_portfolio")
      builds_to_del <- names(rv[[bn]])[sel_rows]
      for (b in builds_to_del) {
        if (!is.null(rv[[pn]])) {
          p <- rv[[pn]][!(Build %in% builds_to_del)]
          rv[[pn]] <- if (nrow(p) == 0) NULL else p
        }
        rv[[bn]][[b]] <- NULL
      }
      rv[[paste0(lp, "_selected_builds")]] <- character(0)
      showNotification(paste0("Deleted ", length(builds_to_del), " build(s)."), type="message")
    })
  }
  make_delete_selected_builds("dk"); make_delete_selected_builds("fd"); make_delete_selected_builds("sd")
  
  # Delete selected lineups via "DELETE SELECTED LINEUPS" button
  make_delete_selected_lineups <- function(lp) {
    observeEvent(input[[paste0(lp, "_delete_selected_lineups")]], {
      sel_rows <- input[[paste0(lp, "_portfolio_lineups_rows_selected")]]
      if (length(sel_rows) == 0) { showNotification("No lineups selected.", type="warning"); return() }
      pn <- paste0(lp, "_portfolio"); bn <- paste0(lp, "_builds")
      port <- rv[[pn]]; req(port)
      del_builds <- port[sel_rows, Build]
      p <- port[-sel_rows]
      # Update build lineup counts
      for (b in names(rv[[bn]])) {
        remaining <- if (nrow(p) > 0) sum(p$Build == b) else 0L
        if (remaining == 0L) rv[[bn]][[b]] <- NULL
        else rv[[bn]][[b]]$num_lineups <- remaining
      }
      rv[[pn]] <- if (nrow(p) == 0) NULL else p
      if (is.null(rv[[pn]])) rv[[paste0(lp, "_build_counter")]] <- 0
      showNotification(paste0("Deleted ", length(sel_rows), " lineup(s)."), type="message")
    })
  }
  make_delete_selected_lineups("dk"); make_delete_selected_lineups("fd"); make_delete_selected_lineups("sd")
  
  make_portfolio_count <- function(lp) {
    renderText({
      p <- rv[[paste0(lp,"_portfolio")]]
      if(is.null(p)) "Portfolio: 0 lineups"
      else paste0("Portfolio: ",nrow(p)," lineups across ",length(rv[[paste0(lp,"_builds")]])," builds")
    })
  }
  output$dk_portfolio_count <- make_portfolio_count("dk")
  output$fd_portfolio_count <- make_portfolio_count("fd")
  output$sd_portfolio_count <- make_portfolio_count("sd")
  
  
  # ==========================================================================
  # BUILDS SUMMARY / PORTFOLIO EXPOSURE / PORTFOLIO LINEUPS
  # ==========================================================================
  
  make_builds_summary <- function(lp) {
    renderDT({
      builds <- rv[[paste0(lp,"_builds")]]
      # Show empty table when no builds yet (req(list()) would blank the tab)
      if (is.null(builds) || length(builds) == 0) {
        builds_df <- data.table(Build=character(0), Lineups=integer(0), Filters=character(0))
      } else {
        builds_df <- data.table(Build=names(builds), Lineups=sapply(builds,function(b)b$num_lineups),
                                Filters=sapply(builds,function(b) b$filters %||% ""))
      }
      datatable(builds_df, options=list(dom='t',scrollX=TRUE,searching=FALSE,lengthChange=FALSE),
                selection=list(mode='multiple', target='row'), escape=FALSE, rownames=FALSE)
    })
  }
  output$dk_builds_summary <- make_builds_summary("dk")
  output$fd_builds_summary <- make_builds_summary("fd")
  output$sd_builds_summary <- make_builds_summary("sd")
  
  make_portfolio_exposure <- function(lp, platform) {
    renderDT({
      pn <- paste0(lp,"_portfolio"); port <- rv[[pn]]; req(port, rv$sim_metadata)
      is_f1  <- isTRUE(rv$sport == "F1")
      is_cbb <- isTRUE(rv$sport %in% c("CBB","NBA"))
      is_nba <- isTRUE(rv$sport == "NBA")
      is_sd  <- platform == "SD"
      salary_col <- if (is_sd) "DKSalary" else paste0(platform, "Salary")
      own_col    <- if (is_sd) NULL        else paste0(platform, "Own")
      cpt_cols  <- grep("^Captain", names(port), value=TRUE)
      util_cols <- grep("^Util",    names(port), value=TRUE)
      all_pc    <- grep("^Player|^Captain|^MVP|^Util|^G[1-4]$|^F[1-3]$|^C1$", names(port), value=TRUE)
      has_captain <- length(cpt_cols) > 0
      meta_players <- if (is_sd) {
        rv$sim_metadata[!is.na(SDSalary) & SDSalary > 0, Player]
      } else rv$sim_metadata$Player
      
      # Helper: compute per-player exposure % for a subset of lineups
      compute_exp <- function(sub_port) {
        n <- nrow(sub_port)
        if (n == 0) return(setNames(rep(0, length(meta_players)), meta_players))
        cnt <- table(unlist(sub_port[, ..all_pc]))
        sapply(meta_players, function(p) if (p %in% names(cnt)) as.numeric(cnt[p]) / n * 100 else 0)
      }
      # Helper: same, but restricted to a specific set of columns (e.g. just
      # the Captain slot columns, or just the Util slot columns)
      compute_exp_cols <- function(sub_port, cols) {
        n <- nrow(sub_port)
        if (n == 0 || length(cols) == 0) return(setNames(rep(0, length(meta_players)), meta_players))
        cnt <- table(unlist(sub_port[, ..cols]))
        sapply(meta_players, function(p) if (p %in% names(cnt)) as.numeric(cnt[p]) / n * 100 else 0)
      }
      
      # ── Build metadata column list ─────────────────────────────────────────
      if (is_nba && is_sd) {
        mc <- intersect(c("Player","Team","SDSalary",
                          "CPTOwn","DKOwn"),
                        names(rv$sim_metadata))
      } else if (is_nba) {
        pos_col_nba <- if (platform == "FD") "FDPos" else "DKPos"
        mc <- intersect(c("Player", pos_col_nba, salary_col, own_col, "Team"),
                        names(rv$sim_metadata))
      } else {
        mc <- intersect(c("Player","PlayerType",salary_col,own_col,
                          "PosGroup","RGProj","RGMin","GameTime","Starting","Team","Car",
                          "Position","Match","Opponent","TeeTimeGroup","CutProb"),
                        names(rv$sim_metadata))
        mc <- mc[!is.na(mc)]
      }
      
      # Check whether any builds are selected for in/out split
      sel_builds <- rv[[paste0(lp, "_selected_builds")]]
      in_out_mode <- length(sel_builds) > 0 && all(sel_builds %in% names(rv[[paste0(lp, "_builds")]]))
      
      if (in_out_mode) {
        # ── IN / OUT split mode ──────────────────────────────────────────────
        port_in  <- port[Build %in% sel_builds]
        port_out <- port[!(Build %in% sel_builds)]
        n_in  <- nrow(port_in);  n_out <- nrow(port_out)
        # NOTE: keep the total column named "Exposure" (not the pretty display
        # name) through all the downstream sport-specific processing below,
        # since that code references `Exposure` directly (OwnProj/Leverage/
        # TotExp calcs). We rename to the display labels at the very end,
        # right before building the datatable.
        exp_tbl  <- data.table(
          Player   = meta_players,
          ExpIN    = round(compute_exp(port_in),  1),
          ExpOUT   = round(compute_exp(port_out), 1),
          Exposure = round(compute_exp(port),     1)
        )
        exp_tbl[, Diff := round(ExpIN - ExpOUT, 1)]
        
        if (has_captain || length(util_cols) > 0) {
          # Total Cpt/Util exposure (kept under these exact names since the
          # sport-specific blocks below reference CptExp/UtilExp directly for
          # Leverage/TotExp calcs), plus separate IN/OUT columns for display.
          exp_tbl[, CptExp     := round(compute_exp_cols(port,     cpt_cols),  1)]
          exp_tbl[, CptExpIN   := round(compute_exp_cols(port_in,  cpt_cols),  1)]
          exp_tbl[, CptExpOUT  := round(compute_exp_cols(port_out, cpt_cols),  1)]
          exp_tbl[, UtilExp    := round(compute_exp_cols(port,     util_cols), 1)]
          exp_tbl[, UtilExpIN  := round(compute_exp_cols(port_in,  util_cols), 1)]
          exp_tbl[, UtilExpOUT := round(compute_exp_cols(port_out, util_cols), 1)]
        }
        
        exp_tbl <- merge(exp_tbl, rv$sim_metadata[Player %in% meta_players, ..mc], by="Player", all.x=TRUE)
        if (salary_col %in% names(exp_tbl)) setnames(exp_tbl, salary_col, "Salary")
        
        if (is_nba && is_sd) {
          if ("SDSalary" %in% names(exp_tbl)) setnames(exp_tbl, "SDSalary", "Salary")
          if ("CPTOwn" %in% names(exp_tbl)) {
            if (max(exp_tbl$CPTOwn, na.rm=TRUE) <= 1) exp_tbl[, CPTOwn := CPTOwn * 100]
            setnames(exp_tbl, "CPTOwn", "CptOwn")
          }
          if ("DKOwn" %in% names(exp_tbl)) {
            if (max(exp_tbl$DKOwn, na.rm=TRUE) <= 1) exp_tbl[, DKOwn := DKOwn * 100]
            setnames(exp_tbl, "DKOwn", "UtlOwn")
          }
          if ("UtilExp" %in% names(exp_tbl)) setnames(exp_tbl, "UtilExp", "UtlExp")
          if (all(c("CptExp","CptOwn") %in% names(exp_tbl)))
            exp_tbl[, CptLev := round(CptExp - CptOwn, 1)]
          if (all(c("UtlExp","UtlOwn") %in% names(exp_tbl)))
            exp_tbl[, UtlLev := round(UtlExp - UtlOwn, 1)]
          exp_tbl[, TotExp := round(Exposure, 1)]
          if (all(c("CptOwn","UtlOwn") %in% names(exp_tbl)))
            exp_tbl[, TotOwn := round(CptOwn + UtlOwn, 1)]
          if (all(c("TotExp","TotOwn") %in% names(exp_tbl)))
            exp_tbl[, TotLev := round(TotExp - TotOwn, 1)]
          exp_tbl[, Exposure := NULL]
          meta_order <- intersect(c("Player","Team","Salary"), names(exp_tbl))
          split_cols <- intersect(c("CptExp","CptOwn","CptLev",
                                    "UtlExp","UtlOwn","UtlLev",
                                    "TotExp","TotOwn","TotLev"), names(exp_tbl))
          setcolorder(exp_tbl, c(meta_order, split_cols))
        } else if (is_nba) {
          pos_col_nba <- if (platform == "FD") "FDPos" else "DKPos"
          if (salary_col %in% names(exp_tbl)) setnames(exp_tbl, salary_col, "Sal")
          if (!is_sd && !is.null(own_col) && own_col %in% names(exp_tbl)) {
            setnames(exp_tbl, own_col, "OwnProj")
            if (max(exp_tbl$OwnProj, na.rm = TRUE) <= 1) exp_tbl[, OwnProj := OwnProj * 100]
            exp_tbl[, OwnProj  := round(OwnProj, 1)]
            exp_tbl[, Leverage := round(Exposure - OwnProj, 1)]
          }
          meta_order    <- intersect(c("Player", pos_col_nba, "Sal", "Team"), names(exp_tbl))
          split_cols    <- intersect(c("CptExp","UtilExp","FlexExp"), names(exp_tbl))
          metrics_order <- intersect(c("Exposure","OwnProj","Leverage"), names(exp_tbl))
          setcolorder(exp_tbl, c(meta_order, split_cols, metrics_order))
        } else {
          if (salary_col %in% names(exp_tbl)) setnames(exp_tbl, salary_col, "Salary")
          if (!is_sd && !is.null(own_col) && own_col %in% names(exp_tbl)) {
            setnames(exp_tbl, own_col, "OwnProj")
            if (max(exp_tbl$OwnProj, na.rm = TRUE) <= 1) exp_tbl[, OwnProj := OwnProj * 100]
            exp_tbl[, OwnProj  := round(OwnProj, 1)]
            exp_tbl[, Leverage := round(Exposure - OwnProj, 1)]
          }
          base_meta  <- c("Player", if (is_f1) "PlayerType" else NULL,
                          "PosGroup","Salary","RGProj","RGMin","SimProj","GameTime","Starting","Team","Car",
                          "Position","Match","Opponent","Surface","Tour","TeeTimeGroup","CutProb")
          meta_order    <- intersect(base_meta, names(exp_tbl))
          split_cols    <- intersect(c("CptExp","UtilExp","FlexExp"), names(exp_tbl))
          metrics_order <- intersect(c("Exposure","OwnProj","Leverage"), names(exp_tbl))
          setcolorder(exp_tbl, c(meta_order, split_cols, metrics_order))
          if (is_cbb) {
            rename_map <- c(PosGroup="Pos", Salary="Sal", RGMin="Mins", RGProj="Proj", GameTime="Time", SimProj="GTS")
            for (old in names(rename_map)) if (old %in% names(exp_tbl)) setnames(exp_tbl, old, rename_map[[old]])
          }
        }
        
        exp_sort_col <- if ("Exposure" %in% names(exp_tbl)) "Exposure" else "TotExp"
        exp_tbl <- exp_tbl[get(exp_sort_col) > 0]
        setorderv(exp_tbl, exp_sort_col, order = -1L)
        
        # ── Rename to display labels now that all Exposure-dependent calcs are done ──
        in_lab  <- paste0("IN (",  n_in,  "L)")
        out_lab <- paste0("OUT (", n_out, "L)")
        tot_lab <- paste0("Total (", nrow(port), "L)")
        setnames(exp_tbl, c("ExpIN","ExpOUT","Diff"), c(in_lab, out_lab, "IN-OUT"))
        if ("Exposure" %in% names(exp_tbl)) setnames(exp_tbl, "Exposure", tot_lab)
        
        cpt_in_lab  <- paste0("Cpt IN (",  n_in,  "L)")
        cpt_out_lab <- paste0("Cpt OUT (", n_out, "L)")
        utl_in_lab  <- paste0("Utl IN (",  n_in,  "L)")
        utl_out_lab <- paste0("Utl OUT (", n_out, "L)")
        if ("CptExpIN"   %in% names(exp_tbl)) setnames(exp_tbl, "CptExpIN",   cpt_in_lab)
        if ("CptExpOUT"  %in% names(exp_tbl)) setnames(exp_tbl, "CptExpOUT",  cpt_out_lab)
        if ("UtilExpIN"  %in% names(exp_tbl)) setnames(exp_tbl, "UtilExpIN",  utl_in_lab)
        if ("UtilExpOUT" %in% names(exp_tbl)) setnames(exp_tbl, "UtilExpOUT", utl_out_lab)
        
        front_cols <- intersect(c("Player", in_lab, out_lab, tot_lab, "IN-OUT"), names(exp_tbl))
        cpt_util_group <- intersect(c("CptExp", cpt_in_lab, cpt_out_lab, "CptOwn","CptLev",
                                      "UtlExp","UtilExp", utl_in_lab, utl_out_lab, "UtlOwn","UtlLev"), names(exp_tbl))
        setcolorder(exp_tbl, c(front_cols, cpt_util_group))
        
        dt <- datatable(exp_tbl, options=list(pageLength=50,scrollX=TRUE,searching=FALSE,lengthChange=FALSE,dom='tp'), rownames=FALSE)
        rc <- intersect(c("CptExp","CptOwn","CptLev",
                          "UtlExp","UtlOwn","UtlLev",
                          "TotExp","TotOwn","TotLev",
                          "FlexExp","OwnProj","Leverage",
                          "CutProb","RGProj","RGMin","Proj","Sim",
                          in_lab, out_lab, tot_lab, "IN-OUT",
                          cpt_in_lab, cpt_out_lab, utl_in_lab, utl_out_lab), names(exp_tbl))
        if (length(rc) > 0) dt <- dt %>% formatRound(rc, 1)
        cap <- rv$config$salary_caps[[platform]] %||% 50000
        sal_col_disp <- if ("Sal" %in% names(exp_tbl)) "Sal" else if ("Salary" %in% names(exp_tbl)) "Salary" else NULL
        if (!is.null(sal_col_disp) && cap >= 1000) dt <- dt %>% formatCurrency(sal_col_disp,"$",digits=0)
        dt
      } else {
        # ── Normal mode: single exposure column across full portfolio ─────────
        n_lineups  <- nrow(port)
        all_counts <- table(unlist(port[, ..all_pc]))
        exp_tbl <- data.table(Player = meta_players, Exposure = 0)
        for (i in seq_len(nrow(exp_tbl))) {
          p <- exp_tbl$Player[i]
          if (p %in% names(all_counts)) exp_tbl$Exposure[i] <- as.numeric(all_counts[p]) / n_lineups * 100
        }
        if (has_captain || length(util_cols) > 0) {
          cpt_counts  <- if (length(cpt_cols))  table(unlist(port[, ..cpt_cols]))  else table(character(0))
          util_counts <- if (length(util_cols)) table(unlist(port[, ..util_cols])) else table(character(0))
          exp_tbl[, CptExp  := 0]
          exp_tbl[, UtilExp := 0]
          for (i in seq_len(nrow(exp_tbl))) {
            p <- exp_tbl$Player[i]
            if (p %in% names(cpt_counts))  exp_tbl$CptExp[i]  <- as.numeric(cpt_counts[p])  / n_lineups * 100
            if (p %in% names(util_counts)) exp_tbl$UtilExp[i] <- as.numeric(util_counts[p]) / n_lineups * 100
          }
        }
        
        exp_tbl <- merge(exp_tbl, rv$sim_metadata[Player %in% meta_players, ..mc], by="Player", all.x=TRUE)
        
        if (is_nba && is_sd) {
          if ("SDSalary" %in% names(exp_tbl)) setnames(exp_tbl, "SDSalary", "Salary")
          if ("CPTOwn" %in% names(exp_tbl)) {
            if (max(exp_tbl$CPTOwn, na.rm=TRUE) <= 1) exp_tbl[, CPTOwn := CPTOwn * 100]
            setnames(exp_tbl, "CPTOwn", "CptOwn")
          }
          if ("DKOwn" %in% names(exp_tbl)) {
            if (max(exp_tbl$DKOwn, na.rm=TRUE) <= 1) exp_tbl[, DKOwn := DKOwn * 100]
            setnames(exp_tbl, "DKOwn", "UtlOwn")
          }
          if ("UtilExp" %in% names(exp_tbl)) setnames(exp_tbl, "UtilExp", "UtlExp")
          if (all(c("CptExp","CptOwn") %in% names(exp_tbl)))
            exp_tbl[, CptLev := round(CptExp - CptOwn, 1)]
          if (all(c("UtlExp","UtlOwn") %in% names(exp_tbl)))
            exp_tbl[, UtlLev := round(UtlExp - UtlOwn, 1)]
          exp_tbl[, TotExp := round(Exposure, 1)]
          if (all(c("CptOwn","UtlOwn") %in% names(exp_tbl)))
            exp_tbl[, TotOwn := round(CptOwn + UtlOwn, 1)]
          if (all(c("TotExp","TotOwn") %in% names(exp_tbl)))
            exp_tbl[, TotLev := round(TotExp - TotOwn, 1)]
          exp_tbl[, Exposure := NULL]
          meta_order <- intersect(c("Player","Team","Salary"), names(exp_tbl))
          split_cols <- intersect(c("CptExp","CptOwn","CptLev",
                                    "UtlExp","UtlOwn","UtlLev",
                                    "TotExp","TotOwn","TotLev"), names(exp_tbl))
          setcolorder(exp_tbl, c(meta_order, split_cols))
        } else if (is_nba) {
          pos_col_nba <- if (platform == "FD") "FDPos" else "DKPos"
          if (salary_col %in% names(exp_tbl)) setnames(exp_tbl, salary_col, "Sal")
          if (!is_sd && !is.null(own_col) && own_col %in% names(exp_tbl)) {
            setnames(exp_tbl, own_col, "OwnProj")
            if (max(exp_tbl$OwnProj, na.rm = TRUE) <= 1) exp_tbl[, OwnProj := OwnProj * 100]
            exp_tbl[, OwnProj  := round(OwnProj, 1)]
            exp_tbl[, Leverage := round(Exposure - OwnProj, 1)]
          }
          meta_order    <- intersect(c("Player", pos_col_nba, "Sal", "Team"), names(exp_tbl))
          split_cols    <- intersect(c("CptExp","UtilExp","FlexExp"), names(exp_tbl))
          metrics_order <- intersect(c("Exposure","OwnProj","Leverage"), names(exp_tbl))
          setcolorder(exp_tbl, c(meta_order, split_cols, metrics_order))
        } else {
          if (salary_col %in% names(exp_tbl)) setnames(exp_tbl, salary_col, "Salary")
          if (!is_sd && !is.null(own_col) && own_col %in% names(exp_tbl)) {
            setnames(exp_tbl, own_col, "OwnProj")
            if (max(exp_tbl$OwnProj, na.rm = TRUE) <= 1) exp_tbl[, OwnProj := OwnProj * 100]
            exp_tbl[, OwnProj  := round(OwnProj, 1)]
            exp_tbl[, Leverage := round(Exposure - OwnProj, 1)]
          }
          base_meta  <- c("Player", if (is_f1) "PlayerType" else NULL,
                          "PosGroup","Salary","RGProj","RGMin","SimProj","GameTime","Starting","Team","Car",
                          "Position","Match","Opponent","Surface","Tour","TeeTimeGroup","CutProb")
          meta_order    <- intersect(base_meta, names(exp_tbl))
          split_cols    <- intersect(c("CptExp","UtilExp","FlexExp"), names(exp_tbl))
          metrics_order <- intersect(c("Exposure","OwnProj","Leverage"), names(exp_tbl))
          setcolorder(exp_tbl, c(meta_order, split_cols, metrics_order))
          if (is_cbb) {
            rename_map <- c(PosGroup="Pos", Salary="Sal", RGMin="Mins", RGProj="Proj", GameTime="Time", SimProj="GTS")
            for (old in names(rename_map)) if (old %in% names(exp_tbl)) setnames(exp_tbl, old, rename_map[[old]])
          }
        }
        
        exp_sort_col <- if ("Exposure" %in% names(exp_tbl)) "Exposure" else "TotExp"
        exp_tbl <- exp_tbl[get(exp_sort_col) > 0]
        setorderv(exp_tbl, exp_sort_col, order = -1L)
        dt <- datatable(exp_tbl, options=list(pageLength=50,scrollX=TRUE,searching=FALSE,lengthChange=FALSE,dom='tp'), rownames=FALSE)
        rc <- intersect(c("CptExp","CptOwn","CptLev",
                          "UtlExp","UtlOwn","UtlLev",
                          "TotExp","TotOwn","TotLev",
                          "Exposure","FlexExp","OwnProj","Leverage",
                          "CutProb","RGProj","RGMin","Proj","Sim"), names(exp_tbl))
        if (length(rc) > 0) dt <- dt %>% formatRound(rc, 1)
        cap <- rv$config$salary_caps[[platform]] %||% 50000
        sal_col_disp <- if ("Sal" %in% names(exp_tbl)) "Sal" else if ("Salary" %in% names(exp_tbl)) "Salary" else NULL
        if (!is.null(sal_col_disp) && cap >= 1000) dt <- dt %>% formatCurrency(sal_col_disp,"$",digits=0)
        dt
      }
    })
  }
  output$dk_portfolio_exposure <- make_portfolio_exposure("dk","DK")
  output$fd_portfolio_exposure <- make_portfolio_exposure("fd","FD")
  output$sd_portfolio_exposure <- make_portfolio_exposure("sd","SD")
  
  # Build row selection → update rv$xx_selected_builds so exposure table reacts
  make_build_selection_observer <- function(lp) {
    observeEvent(input[[paste0(lp, "_builds_summary_rows_selected")]], {
      sel_rows <- input[[paste0(lp, "_builds_summary_rows_selected")]]
      builds   <- rv[[paste0(lp, "_builds")]]
      if (length(sel_rows) == 0 || is.null(builds)) {
        rv[[paste0(lp, "_selected_builds")]] <- character(0)
      } else {
        rv[[paste0(lp, "_selected_builds")]] <- names(builds)[sel_rows]
      }
    }, ignoreNULL = FALSE)
  }
  make_build_selection_observer("dk")
  make_build_selection_observer("fd")
  make_build_selection_observer("sd")
  
  make_portfolio_lineups <- function(lp) {
    renderDT({
      port <- rv[[paste0(lp,"_portfolio")]]; req(port, rv$config)
      display_table <- create_portfolio_display_table(port, rv$config, lp)
      format_cols   <- tryCatch(get_format_columns(display_table, rv$config), error=function(e) character(0))
      dt <- datatable(display_table[,-"RowID"],
                      options=list(pageLength=50,scrollX=TRUE,searching=FALSE,lengthChange=FALSE,dom='tp'),
                      selection=list(mode='multiple', target='row'),
                      escape=FALSE, rownames=FALSE)
      if(length(format_cols)>0) dt <- dt %>% formatRound(format_cols,1)
      if("Salary" %in% names(display_table)) dt <- dt %>% formatCurrency("Salary","$",digits=0)
      dt
    })
  }
  output$dk_portfolio_lineups <- make_portfolio_lineups("dk")
  output$fd_portfolio_lineups <- make_portfolio_lineups("fd")
  output$sd_portfolio_lineups <- make_portfolio_lineups("sd")
  
  
  # ==========================================================================
  # PORTFOLIO DOWNLOADS
  # ==========================================================================
  
  make_portfolio_download <- function(lp, platform) {
    downloadHandler(
      filename=function() paste0(platform,"_Portfolio_",format(Sys.Date(),"%Y%m%d"),".csv"),
      content=function(file) {
        port <- rv[[paste0(lp,"_portfolio")]]; req(port)
        dl <- copy(port)[sample(nrow(port))]
        id_col <- paste0(platform,"ID")
        if ("Captain" %in% names(dl) && isTRUE(rv$sport == "F1")) {
          dl <- create_download_f1(dl, rv$sim_metadata)
        } else if ("Captain" %in% names(dl)) {
          dl <- create_download_showdown(dl, rv$sim_metadata)
        } else if ("MVP" %in% names(dl)) {
          dl <- create_download_mvp(dl, rv$sim_metadata)
        } else if (isTRUE(rv$sport == "CBB")) {
          dl <- if (rv$sport == "NBA") create_download_nba(dl, rv$sim_metadata, platform) else create_download_cbb(dl, rv$sim_metadata, platform)
        } else if (id_col %in% names(rv$sim_metadata)) {
          for (col in grep("^Player",names(dl),value=TRUE)) {
            ids <- rv$sim_metadata[match(dl[[col]],rv$sim_metadata$Player), get(id_col)]
            dl[[col]] <- if(platform=="DK") paste0(dl[[col]]," (",ids,")") else paste0(ids,":",dl[[col]])
          }
        }
        fwrite(dl, file)
      }
    )
  }
  output$dk_download_portfolio <- make_portfolio_download("dk","DK")
  output$fd_download_portfolio <- make_portfolio_download("fd","FD")
  output$sd_download_portfolio <- make_portfolio_download("sd","SD")
  
  
  # ==========================================================================
  # SIM RESULTS OUTPUTS
  # ==========================================================================
  
  output$has_sim_results   <- reactive({ !is.null(rv$simulation_results) && nrow(rv$simulation_results)>0 })
  
  # Initialize sim_results_platform so pills work before first click
  output$sim_platform_init <- renderUI({
    req(rv$config)
    plats <- available_platforms()
    radioButtons("sim_results_platform", NULL, choices=plats, selected=plats[1], inline=TRUE)
  })
  outputOptions(output, "has_sim_results",   suspendWhenHidden=FALSE)
  output$sport_detected    <- reactive({ rv$sport %||% "" })
  outputOptions(output, "sport_detected",    suspendWhenHidden=FALSE)
  output$has_sport_visuals <- reactive({
    !is.null(rv$sport_visuals)
  })
  outputOptions(output, "has_sport_visuals", suspendWhenHidden=FALSE)
  
  # ── Sim results control bar (platform pills + export) ──────────────────
  output$sim_results_control_bar <- renderUI({
    req(rv$config)
    platforms   <- available_platforms()
    
    plat_labels <- c(DK = "DraftKings", FD = "FanDuel", SD = "Showdown")
    selected    <- if (!is.null(input$sim_results_platform)) input$sim_results_platform else platforms[1]
    
    pill_btns <- lapply(platforms, function(p) {
      is_active <- isTRUE(p == selected)
      tags$button(
        class = paste("gts-pill", if (is_active) "active" else ""),
        onclick = sprintf("Shiny.setInputValue('sim_results_platform','%s',{priority:'event'})", p),
        plat_labels[p] %||% p
      )
    })
    
    div(class = "gts-sr-bar",
        div(class = "gts-sr-seg",
            span(class = "gts-sr-label", "Platform"),
            div(class = "gts-platform-pills", pill_btns)
        ),
        div(class = "gts-sr-spacer"),
        div(class = "gts-sr-seg",
            downloadButton("download_projections_csv", "Projections CSV",
                           icon  = icon("download"),
                           class = "gts-dl-btn-real")
        ),
        div(class = "gts-sr-seg",
            downloadButton("download_sim_sample", "Sim Sample",
                           icon  = icon("download"),
                           class = "gts-dl-btn-real")
        )
    )
  })
  
  # Keep sim_results_platform as a reactive value initialized to first platform
  observe({
    req(rv$config)
    plats <- available_platforms()
    cur   <- input$sim_results_platform
    if (length(plats) > 0 && (is.null(cur) || !cur %in% plats))
      updateRadioButtons(session, "sim_results_platform", selected = plats[1])
  })
  
  
  # ── Build projections data (cached — recomputed only when sim runs) ──────
  # Sport metadata per sport, driven by what's available in sim_metadata.
  # Standard: Player, Salary, Own%
  # Sport-specific: pulled from config$metadata_columns names
  # Stats display order: Avg, Median, P90, P75, P20
  # Download order:      Avg, Median, P20, P75, P90  (ascending percentiles)
  
  build_projections <- function(platform) {
    req(rv$simulation_results, rv$sim_metadata, rv$config)
    
    score_col  <- if (platform == "SD") "DKScore"  else paste0(platform, "Score")
    salary_col <- if (platform == "SD") "SDSalary" else paste0(platform, "Salary")
    own_col    <- if (platform == "SD") "SDOwn"    else paste0(platform, "Own")
    
    sim  <- copy(rv$simulation_results);  setDT(sim)
    meta <- copy(rv$sim_metadata);        setDT(meta)
    
    # Filter to platform-eligible players for SD
    if (platform == "SD" && "SDSalary" %in% names(meta)) {
      eligible <- meta[!is.na(SDSalary) & SDSalary > 0, Player]
      if (length(eligible) > 0) {
        sim  <- sim[Player %in% eligible]
        meta <- meta[Player %in% eligible]
      }
    }
    
    if (!score_col %in% names(sim)) return(NULL)
    
    # Compute stats once — this is the expensive step at 50k sims
    proj <- sim[, .(
      Avg    = round(mean(get(score_col)),            1),
      Median = round(median(get(score_col)),          1),
      P90    = round(quantile(get(score_col), 0.90),  1),
      P75    = round(quantile(get(score_col), 0.75),  1),
      P20    = round(quantile(get(score_col), 0.20),  1)
    ), by = Player]
    
    # Standard columns: Salary + Own
    std_cols <- intersect(c("Player", salary_col, own_col), names(meta))
    proj <- merge(proj, meta[, ..std_cols], by = "Player", all.x = TRUE)
    if (salary_col %in% names(proj)) setnames(proj, salary_col, "Salary")
    if (own_col %in% names(proj)) {
      setnames(proj, own_col, "Own")
      # Auto-detect decimal format (e.g. 0.25) vs percentage (25) — multiply if needed
      if (max(proj$Own, na.rm = TRUE) <= 1) proj[, Own := Own * 100]
      proj[, Own := round(Own, 1)]
    }
    
    # Sport metadata columns — from config$metadata_columns + sport-specific extras
    sport_meta_cols <- if (!is.null(rv$config$metadata_columns)) {
      sapply(rv$config$metadata_columns, function(x) x$name)
    } else character(0)
    
    # Add CBB/NBA projection + minutes columns
    if (isTRUE(rv$sport == "CBB")) {
      sport_meta_cols <- c(sport_meta_cols, intersect(c("RGProj","RGMin"), names(meta)))
    }
    if (isTRUE(rv$sport == "NBA")) {
      nba_proj_col <- if (platform == "FD") "FDProj" else "DKProj"
      nba_pos_col  <- if (platform == "FD") "FDPos"  else "DKPos"
      sport_meta_cols <- c(sport_meta_cols, intersect(c(nba_proj_col, nba_pos_col, "Mins"), names(meta)))
      # Remove PosGroup from sport_meta_cols — we'll use granular DKPos/FDPos instead
      sport_meta_cols <- sport_meta_cols[sport_meta_cols != "PosGroup"]
    }
    
    # Pull all sport metadata that actually exists in sim_metadata
    available_sport_cols <- intersect(sport_meta_cols, names(meta))
    if (length(available_sport_cols) > 0) {
      meta_pull <- unique(meta[, c("Player", available_sport_cols), with = FALSE])
      proj <- merge(proj, meta_pull, by = "Player", all.x = TRUE)
    }
    
    # Rename for display
    if ("RGProj" %in% names(proj)) setnames(proj, "RGProj", "Proj")
    if ("RGMin"  %in% names(proj)) setnames(proj, "RGMin",  "Mins")
    if ("DKProj" %in% names(proj)) setnames(proj, "DKProj", "ETR")
    if ("FDProj" %in% names(proj)) setnames(proj, "FDProj", "ETR")
    if ("DKPos"  %in% names(proj)) setnames(proj, "DKPos",  "Pos")
    if ("FDPos"  %in% names(proj)) setnames(proj, "FDPos",  "Pos")
    if ("PosGroup" %in% names(proj)) setnames(proj, "PosGroup", "Pos")
    
    setorder(proj, -Avg)
    proj
  }
  
  
  # ── Projections table display ────────────────────────────────────────────
  output$sim_projections_table <- renderDT({
    req(rv$simulation_results, rv$sim_metadata)
    plat_sel <- input$sim_results_platform
    avail    <- available_platforms()
    platform <- if (!is.null(plat_sel) && nchar(plat_sel) > 0 && plat_sel %in% avail)
      plat_sel else if (length(avail) > 0) avail[1] else "DK"
    proj <- build_projections(platform)
    req(proj)
    
    # Display column order: Player, Salary, Own, [sport meta], Avg, Median, P90, P75, P20
    base_cols  <- intersect(c("Player","Salary","Own"), names(proj))
    stat_cols  <- intersect(c("Avg","Median","P90","P75","P20"), names(proj))
    skip_cols  <- c(base_cols, stat_cols, "Proj","Mins")
    sport_cols <- setdiff(names(proj), skip_cols)
    # NASCAR: Car -> Starting -> Team order
    if (rv$sport == "NASCAR") {
      nascar_order <- c("Car","Starting","Team")
      sport_cols <- c(intersect(nascar_order, sport_cols),
                      setdiff(sport_cols, nascar_order))
    }
    # Put CBB Proj/ETR/Mins after salary
    cbb_extra  <- intersect(c("Proj","ETR","Mins"), names(proj))
    display_order <- c(base_cols, cbb_extra, sport_cols, stat_cols)
    display_order <- intersect(display_order, names(proj))
    proj <- proj[, ..display_order]
    
    num_targets <- which(names(proj) %in% c(stat_cols, "Own", "CutProb", "WinProb", "Proj", "Mins")) - 1
    
    dt <- datatable(proj,
                    filter   = "none",
                    options  = list(
                      dom        = "t",
                      paging     = FALSE,
                      scrollX    = TRUE,
                      scrollY    = "420px",
                      order      = list(list(which(names(proj) == "Avg") - 1, "desc")),
                      columnDefs = list(list(className = "dt-right", targets = num_targets))
                    ),
                    rownames = FALSE,
                    class    = "stripe hover compact"
    )
    
    # Format columns
    if ("Salary" %in% names(proj)) {
      cap <- rv$config$salary_caps[[platform]] %||% 50000
      if (cap >= 1000) dt <- dt %>% formatCurrency("Salary", "$", digits = 0)
    }
    if ("Own" %in% names(proj))
      dt <- dt %>% formatString("Own", suffix = "%")
    if ("CutProb" %in% names(proj))
      dt <- dt %>% formatRound("CutProb", 1) %>% formatString("CutProb", suffix = "%")
    if ("WinProb" %in% names(proj))
      dt <- dt %>% formatPercentage("WinProb", digits = 1)
    
    round_cols <- intersect(c("Avg","Median","P90","P75","P20","Proj","Mins"), names(proj))
    if (length(round_cols) > 0) dt <- dt %>% formatRound(round_cols, 1)
    dt
  })
  
  
  # ── Projections CSV download ─────────────────────────────────────────────
  # Download column order: Player, Salary, Own, [sport meta], Avg, Median, P20, P75, P90
  output$download_projections_csv <- downloadHandler(
    filename = function() {
      sport <- rv$sport %||% "sim"
      plat  <- input$sim_results_platform %||% "DK"
      paste0(sport, "_", plat, "_Projections_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      proj <- build_projections(input$sim_results_platform %||% "DK")
      req(proj)
      # Download order: Avg, Median, P20, P75, P90
      base_cols  <- intersect(c("Player","Salary","Own"), names(proj))
      dl_stats   <- intersect(c("Avg","Median","P20","P75","P90"), names(proj))
      skip_cols  <- c(base_cols, c("Avg","Median","P90","P75","P20","Proj","Mins"))
      sport_cols <- setdiff(names(proj), skip_cols)
      cbb_extra  <- intersect(c("Proj","ETR","Mins"), names(proj))
      dl_order   <- c(base_cols, cbb_extra, sport_cols, dl_stats)
      dl_order   <- intersect(dl_order, names(proj))
      fwrite(proj[, ..dl_order], file)
    }
  )
  
  
  
  
  # ── Sim sample download — 1000 randomly sampled sims, all sports ─────────
  output$download_sim_sample <- downloadHandler(
    filename = function() {
      sport <- rv$sport %||% "sim"
      paste0(sport, "_SimSample_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv$simulation_results, rv$sim_metadata)
      sim  <- copy(rv$simulation_results);  setDT(sim)
      meta <- copy(rv$sim_metadata);        setDT(meta)
      
      # Sample up to 1000 unique sim IDs
      all_ids     <- unique(sim$SimID)
      sample_ids  <- sample(all_ids, min(1000L, length(all_ids)))
      sim_sample  <- sim[SimID %in% sample_ids]
      
      # Join key metadata: salary + own for the active platform
      platform   <- input$sim_results_platform %||% "DK"
      salary_col <- if (platform == "SD") "SDSalary" else paste0(platform, "Salary")
      own_col    <- if (platform == "SD") "SDOwn"    else paste0(platform, "Own")
      
      meta_cols  <- intersect(c("Player", salary_col, own_col), names(meta))
      # Add sport metadata columns from config
      sport_meta <- if (!is.null(rv$config$metadata_columns)) {
        sapply(rv$config$metadata_columns, function(x) x$name)
      } else character(0)
      meta_cols  <- c(meta_cols, intersect(sport_meta, names(meta)))
      meta_cols  <- unique(meta_cols)
      
      dl <- merge(sim_sample, unique(meta[, ..meta_cols]), by = "Player", all.x = TRUE)
      
      # Clean column order: Player, metadata, SimID, scores
      score_cols <- intersect(c("DKScore","FDScore"), names(dl))
      id_cols    <- intersect(c("Player", salary_col, own_col), names(dl))
      meta_extra <- setdiff(meta_cols, c("Player", salary_col, own_col))
      meta_extra <- intersect(meta_extra, names(dl))
      col_order  <- c(id_cols, meta_extra, "SimID", score_cols,
                      setdiff(names(dl), c(id_cols, meta_extra, "SimID", score_cols)))
      col_order  <- intersect(col_order, names(dl))
      setcolorder(dl, col_order)
      
      fwrite(dl, file)
    }
  )
  
  
  # ==========================================================================
  # SPORT-SPECIFIC VISUALIZATIONS
  # ==========================================================================
  
  output$sport_specific_visuals_ui <- renderUI({
    req(rv$sport)
    if (rv$sport %in% c("CBB","NBA")) {
      req(rv$sport_visuals)
      return(render_cbb_visuals(rv$sport_visuals))
    }
    if (rv$sport == "SOCCER") {
      req(rv$sport_visuals)
      return(render_soccer_visuals(rv$sport_visuals))
    }
    req(rv$sport_visuals)
    if      (rv$sport == "TENNIS")  render_tennis_visuals(rv$sport_visuals)
    else if (rv$sport == "NASCAR")  render_nascar_visuals(rv$sport_visuals, input$sim_results_platform)
    else if (rv$sport == "GOLF")    render_golf_visuals(rv$sport_visuals)
    else if (rv$sport == "MMA")     render_mma_visuals(rv$sport_visuals)
    else if (rv$sport == "F1")      render_f1_visuals(rv$sport_visuals)
    else NULL
  })
  
  
  # ---------- Tennis ----------
  
  render_tennis_visuals <- function(visuals) {
    all_players <- if (!is.null(visuals$score_distributions$all_wins)) {
      avgs <- visuals$score_distributions$all_wins[, .(Avg=mean(Score)), by=Player]
      setorder(avgs, -Avg); head(avgs$Player, 15)
    } else character(0)
    fluidRow(column(12,
                    box(width=NULL, title="TENNIS SIMULATION ANALYSIS", status="primary", solidHeader=TRUE,
                        div(class="gts-chart-filter",
                            span(class="gts-chart-filter-label", "Players:"),
                            selectizeInput("tennis_player_filter", NULL,
                                           choices=all_players, selected=all_players, multiple=TRUE,
                                           options=list(plugins=list("remove_button"), placeholder="Select players"),
                                           width="600px")
                        ),
                        tabsetPanel(id="tennis_visuals_tabs", type="tabs",
                                    tabPanel("Match Analysis", div(style="margin-top:15px;"),
                                             DTOutput("tennis_match_analysis_table") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Score Distribution", div(style="margin-top:15px;"),
                                             plotlyOutput("tennis_all_wins_plot", height="auto") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Straight Sets", div(style="margin-top:15px;"),
                                             plotlyOutput("tennis_ss_wins_plot", height="auto") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Non-Straight Sets", div(style="margin-top:15px;"),
                                             plotlyOutput("tennis_nss_wins_plot", height="auto") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Salary Analysis", div(style="margin-top:15px;"),
                                             plotlyOutput("tennis_salary_analysis_plot", height="500px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6))
                        )
                    )
    ))
  }
  
  output$tennis_match_analysis_table <- renderDT({
    req(rv$sport=="TENNIS", rv$sport_visuals$match_analysis)
    datatable(rv$sport_visuals$match_analysis,
              options=list(pageLength=50, scrollX=TRUE, scrollY="500px", searching=FALSE,
                           lengthChange=FALSE, paging=FALSE, dom="t",
                           order=list(list(5,'desc')),
                           columnDefs=list(list(className="dt-right",targets=2:8),
                                           list(width="200px",targets=0))),
              rownames=FALSE,
              colnames=c("Match","Player","Salary","Imp Win%","Sim Win%","Diff","Imp SS%","Sim SS%","Avg (Wins)"),
              class="stripe hover compact nowrap") %>%
      formatCurrency("Salary","$",digits=0) %>%
      formatPercentage(c("ImpliedWin","SimWin","ImpliedSS","SimSS"), 1) %>%
      formatRound(c("WinDiff","AvgWinPts"), 1) %>%
      formatStyle("WinDiff",
                  backgroundColor=styleInterval(c(-5,5), c("#ffcccc","#ffffff","#ccffcc")))
  })
  
  make_tennis_box_plot <- function(data_path, title, color_hex, filter_input) {
    function() {
      req(rv$sport=="TENNIS")
      plot_data <- rv$sport_visuals$score_distributions[[data_path]]
      req(plot_data)
      setDT(plot_data)
      selected <- input[[filter_input]]
      if (length(selected) == 0) {
        selected <- plot_data[, .(Avg=mean(Score)), by=Player][order(-Avg)][1:min(15,.N)]$Player
      }
      plot_data <- as.data.frame(plot_data[Player %in% selected])
      # Order by median descending
      med_order <- plot_data |> tapply(plot_data$Player, FUN=function(x) median(x$Score)) |> sort(decreasing=TRUE) |> names()
      plot_data$Player <- factor(plot_data$Player, levels=rev(med_order))
      n_players <- length(unique(plot_data$Player))
      h <- max(300, n_players * 42)
      plot_ly(data=plot_data, x=~Score, y=~Player, type="box", orientation="h",
              marker=list(color=color_hex, size=3), line=list(color=color_hex),
              fillcolor=paste0(substr(color_hex,1,7),"33")) %>%
        layout(
          title=list(text=title, font=list(color="#FFE500",size=14)),
          xaxis=list(title="DK Fantasy Points", gridcolor="#2a2a2a", color="#888"),
          yaxis=list(title="", color="#ccc", tickfont=list(size=11)),
          paper_bgcolor="#121212", plot_bgcolor="#141414",
          font=list(color="#FFFFFF",size=11), showlegend=FALSE,
          margin=list(l=160,r=30,t=40,b=50),
          height=h)
    }
  }
  output$tennis_all_wins_plot  <- renderPlotly(make_tennis_box_plot("all_wins","Score Distribution — All Wins","#FFE500","tennis_player_filter")())
  output$tennis_ss_wins_plot   <- renderPlotly(make_tennis_box_plot("ss_wins","Score Distribution — Straight Sets","#4A90D9","tennis_player_filter")())
  output$tennis_nss_wins_plot  <- renderPlotly(make_tennis_box_plot("nss_wins","Score Distribution — Non-Straight Sets","#5DCAA5","tennis_player_filter")())
  
  output$tennis_salary_analysis_plot <- renderPlotly({
    req(rv$sport=="TENNIS", rv$sport_visuals$score_distributions$all_wins,
        rv$sport_visuals$player_data)
    avg_scores  <- rv$sport_visuals$score_distributions$all_wins[, .(AvgWinScore=mean(Score)), by=Player]
    player_info <- unique(rv$sport_visuals$player_data[, .(Player=Name, Salary)])
    plot_data   <- as.data.frame(merge(avg_scores, player_info, by="Player"))
    plot_ly(data=plot_data, x=~Salary, y=~AvgWinScore, text=~Player,
            type="scatter", mode="markers+text",
            marker=list(size=10, color="#FFE500", line=list(color="#000000",width=1)),
            textposition="top center", textfont=list(color="#FFFFFF",size=10)) %>%
      layout(
        title=list(text="Average Win Score vs Salary", font=list(color="#FFE500",size=16)),
        xaxis=list(title="Salary ($)", gridcolor="#2a2a2a", color="#FFFFFF"),
        yaxis=list(title="Avg Win Score (DK Points)", gridcolor="#2a2a2a", color="#FFFFFF"),
        paper_bgcolor="#121212", plot_bgcolor="#141414",
        font=list(color="#FFFFFF"))
  })
  
  
  # ---------- NASCAR ----------
  
  render_nascar_visuals <- function(visuals, platform) {
    fluidRow(column(12,
                    box(width = NULL, title = "NASCAR Simulation Analysis",
                        status = "primary", solidHeader = TRUE,
                        tabsetPanel(id = "nascar_visuals_tabs", type = "tabs",
                                    tabPanel("Fantasy Points", div(style = "margin-top:15px;"),
                                             plotlyOutput("nascar_fantasy_plot", height = "auto") %>%
                                               shinycssloaders::withSpinner(color = "#FFE500", type = 6)),
                                    tabPanel("Finishing Position", div(style = "margin-top:15px;"),
                                             plotlyOutput("nascar_finish_plot", height = "auto") %>%
                                               shinycssloaders::withSpinner(color = "#FFE500", type = 6)),
                                    tabPanel("Dominator by Driver", div(style = "margin-top:15px;"),
                                             plotlyOutput("dominator_violin_driver", height = "auto") %>%
                                               shinycssloaders::withSpinner(color = "#FFE500", type = 6))
                        )
                    )
    ))
  }
  
  
  # Helper: build a NASCAR box plot from precomputed quantile stats (P5/P25/P50/P75/P95)
  nascar_box_plot <- function(dt, color, title_x, range_x = NULL, sort_desc = FALSE, dtick_x = NULL) {
    setDT(dt)
    if (sort_desc) setorder(dt, -P50)
    ordered <- as.character(dt$Name)
    h_px    <- max(300, nrow(dt) * 34)
    p <- plot_ly()
    for (i in seq_len(nrow(dt))) {
      row <- dt[i]
      p <- add_trace(p, type = "box", orientation = "h", name = as.character(row$Name),
                     lowerfence = list(row$P5),  q1 = list(row$P25), median = list(row$P50),
                     q3 = list(row$P75), upperfence = list(row$P95), y = list(as.character(row$Name)),
                     marker = list(color = color), line = list(color = color),
                     fillcolor = paste0(color, "30"), showlegend = FALSE)
    }
    x_layout <- list(title = title_x, gridcolor = "#2a2a2a", color = "#888", zeroline = FALSE)
    if (!is.null(range_x))  x_layout$range <- range_x
    if (!is.null(dtick_x))  x_layout$dtick <- dtick_x
    p %>% layout(
      xaxis = x_layout,
      yaxis = list(title = "", categoryorder = "array", categoryarray = ordered,
                   color = "#ccc", automargin = TRUE, tickfont = list(size = 11)),
      paper_bgcolor = "#121212", plot_bgcolor = "#141414",
      font = list(color = "#FFFFFF", size = 11), showlegend = FALSE,
      height = h_px, margin = list(l = 160, r = 30, t = 20, b = 50)
    ) %>% config(displayModeBar = FALSE)
  }
  
  
  output$nascar_fantasy_plot <- renderPlotly({
    req(rv$sport == "NASCAR", rv$sport_visuals$fp_dk)
    tryCatch({
      platform <- if (!is.null(input$sim_results_platform) && nchar(input$sim_results_platform) > 0) input$sim_results_platform else "DK"
      dt <- if (platform == "FD" && !is.null(rv$sport_visuals$fp_fd)) copy(rv$sport_visuals$fp_fd) else copy(rv$sport_visuals$fp_dk)
      setorder(dt, -DKSalary)
      nascar_box_plot(dt, color = "#FFE500", title_x = paste(platform, "Fantasy Points"), sort_desc = FALSE, dtick_x = 5)
    }, error = function(e) { plotly_empty() })
  })
  
  
  output$nascar_finish_plot <- renderPlotly({
    req(rv$sport == "NASCAR", rv$sport_visuals$finish)
    tryCatch({
      dt <- copy(rv$sport_visuals$finish)
      setorder(dt, Starting)
      nascar_box_plot(dt, color = "#FFE500", title_x = "Finish Position", range_x = c(0, 41))
    }, error = function(e) { plotly_empty() })
  })
  
  
  output$dominator_violin_driver <- renderPlotly({
    req(rv$sport == "NASCAR", rv$sport_visuals$dom_dk)
    tryCatch({
      platform <- if (!is.null(input$sim_results_platform) && nchar(input$sim_results_platform) > 0) input$sim_results_platform else "DK"
      dt <- if (platform == "FD" && !is.null(rv$sport_visuals$dom_fd)) copy(rv$sport_visuals$dom_fd) else copy(rv$sport_visuals$dom_dk)
      nascar_box_plot(dt, color = "#4A90D9", title_x = paste(platform, "Dominator Points"), sort_desc = TRUE)
    }, error = function(e) { plotly_empty() })
  })
  
  
  # ---------- MMA ----------
  
  render_mma_visuals <- function(visuals) {
    req(visuals)
    fluidRow(column(12,
                    box(width = NULL, title = "MMA Simulation Analysis",
                        status = "primary", solidHeader = TRUE,
                        tabsetPanel(id = "mma_visuals_tabs", type = "tabs",
                                    tabPanel("Win Methods", div(style = "margin-top:15px;"),
                                             plotlyOutput("mma_outcome_dist_plot", height = "auto") %>%
                                               shinycssloaders::withSpinner(color = "#FFE500", type = 6)),
                                    tabPanel("Win Score Range", div(style = "margin-top:15px;"),
                                             plotlyOutput("mma_score_dist_plot", height = "auto") %>%
                                               shinycssloaders::withSpinner(color = "#FFE500", type = 6)),
                                    tabPanel("Fight Combined Score", div(style = "margin-top:15px;"),
                                             plotlyOutput("mma_fight_score_plot", height = "auto") %>%
                                               shinycssloaders::withSpinner(color = "#FFE500", type = 6))
                        )
                    )
    ))
  }
  
  
  # ── Win Methods stacked bar ──────────────────────────────────────────────
  output$mma_outcome_dist_plot <- renderPlotly({
    req(rv$sport == "MMA", rv$sport_visuals$outcome_pct, rv$sport_visuals$fighter_summary)
    platform <- if (!is.null(input$sim_results_platform)) input$sim_results_platform else "DK"
    op  <- copy(rv$sport_visuals$outcome_pct);     setDT(op)
    fs  <- copy(rv$sport_visuals$fighter_summary); setDT(fs)
    
    sal_col <- switch(platform, "DK" = "DKSalary", "FD" = "FDSalary", "SDSalary")
    if (platform == "SD") {
      eligible <- fs[!is.na(SDSalary) & SDSalary > 0, Player]
      fs <- fs[Player %in% eligible]; op <- op[Player %in% eligible]
    }
    setorderv(fs, sal_col, order = -1L)
    fs[, Label := sprintf("%s ($%s)", Player, format(get(sal_col), big.mark = ","))]
    name_to_label <- setNames(fs$Label, fs$Player)
    op[, YLabel := name_to_label[Player]]
    label_order   <- fs$Label
    
    outcome_order <- c("QuickWin_R1","R1 Finish","R2 Finish","R3 Finish","R4 Finish","R5 Finish","Decision")
    win_colors    <- c(
      "QuickWin_R1" = "#9932CC", "R1 Finish" = "#1E90FF", "R2 Finish" = "#32CD32",
      "R3 Finish"   = "#FF8C00", "R4 Finish" = "#8B0000", "R5 Finish" = "#FFE500",
      "Decision"    = "#DC143C"
    )
    n_fighters <- length(label_order)
    h_px       <- max(280, n_fighters * 44)
    
    p <- plot_ly()
    for (oc in outcome_order) {
      d <- op[Outcome == oc]
      if (nrow(d) == 0) next
      p <- add_trace(p, x = d$WinPct, y = d$YLabel, name = oc,
                     type = "bar", orientation = "h",
                     marker = list(color = win_colors[oc]),
                     hovertemplate = paste0("<b>%{y}</b><br>", oc, ": %{x:.1f}%<extra></extra>"))
    }
    p %>% layout(
      barmode = "stack",
      xaxis   = list(title = "Win %", gridcolor = "#2a2a2a", color = "#888"),
      yaxis   = list(title = "", categoryorder = "array",
                     categoryarray = rev(label_order), color = "#ccc",
                     tickfont = list(size = 11)),
      paper_bgcolor = "#121212", plot_bgcolor = "#141414",
      font          = list(color = "#FFFFFF", size = 11),
      legend        = list(orientation = "h", y = -0.15, font = list(size = 11, color = "#ccc")),
      height        = h_px,
      margin        = list(l = 220, r = 30, t = 20, b = 80)
    ) %>% config(displayModeBar = FALSE)
  })
  
  
  # ── Win Score Range box plot ─────────────────────────────────────────────
  output$mma_score_dist_plot <- renderPlotly({
    req(rv$sport == "MMA", rv$sport_visuals$score_dist, rv$sport_visuals$player_data)
    platform <- if (!is.null(input$sim_results_platform)) input$sim_results_platform else "DK"
    score_col <- if (platform == "FD") "FDScore" else "DKScore"
    sal_col   <- switch(platform, "DK" = "DKSalary", "FD" = "FDSalary", "SDSalary")
    
    sd_data <- copy(rv$sport_visuals$score_dist); setDT(sd_data)
    meta    <- copy(rv$sport_visuals$player_data); setDT(meta)
    
    if (platform == "SD") {
      eligible <- meta[!is.na(SDSalary) & SDSalary > 0, Player]
      sd_data <- sd_data[Player %in% eligible]; meta <- meta[Player %in% eligible]
    }
    wins <- sd_data[as.integer(Win) == 1L]
    if (nrow(wins) == 0) return(plotly_empty())
    
    # Order by median score descending
    med_ord <- wins[, .(Med = median(get(score_col))), by = Player]
    setorder(med_ord, Med)
    wins[, Player := factor(Player, levels = med_ord$Player)]
    
    n_fighters <- length(unique(wins$Player))
    h_px       <- max(280, n_fighters * 42)
    
    plot_ly(data = as.data.frame(wins),
            x = wins[[score_col]], y = ~Player,
            type = "box", orientation = "h",
            marker    = list(color = "#FFE500", size = 3),
            line      = list(color = "#FFE500"),
            fillcolor = "rgba(255,229,0,0.18)") %>%
      layout(
        xaxis = list(title = "Fantasy Points (wins only)", gridcolor = "#2a2a2a",
                     color = "#888", zeroline = FALSE),
        yaxis = list(title = "", color = "#ccc", tickfont = list(size = 11),
                     automargin = TRUE),
        paper_bgcolor = "#121212", plot_bgcolor = "#141414",
        font          = list(color = "#FFFFFF", size = 11),
        showlegend    = FALSE, height = h_px,
        margin        = list(l = 160, r = 30, t = 20, b = 50)
      ) %>% config(displayModeBar = FALSE)
  })
  
  
  # ── Fight Combined Score box plot ────────────────────────────────────────
  output$mma_fight_score_plot <- renderPlotly({
    req(rv$sport == "MMA", rv$sport_visuals$fight_scores)
    platform  <- if (!is.null(input$sim_results_platform)) input$sim_results_platform else "DK"
    score_col <- if (platform == "FD") "CombinedFD" else "CombinedDK"
    label     <- if (platform == "FD") "FD" else "DK"
    
    fs <- copy(rv$sport_visuals$fight_scores); setDT(fs)
    if (nrow(fs) == 0) return(plotly_empty())
    
    # Order fights by median combined score descending
    med_ord <- fs[, .(Med = median(get(score_col))), by = FightLabel]
    setorder(med_ord, Med)
    fs[, FightLabel := factor(FightLabel, levels = med_ord$FightLabel)]
    
    n_fights <- length(unique(fs$FightLabel))
    h_px     <- max(200, n_fights * 60)
    
    plot_ly(data = as.data.frame(fs),
            x = fs[[score_col]], y = ~FightLabel,
            type = "box", orientation = "h",
            marker    = list(color = "#4A90D9", size = 3),
            line      = list(color = "#4A90D9"),
            fillcolor = "rgba(74,144,217,0.18)") %>%
      layout(
        xaxis = list(title = paste(label, "Combined Fight Score"),
                     gridcolor = "#2a2a2a", color = "#888", zeroline = FALSE),
        yaxis = list(title = "", color = "#ccc", tickfont = list(size = 11),
                     automargin = TRUE),
        paper_bgcolor = "#121212", plot_bgcolor = "#141414",
        font          = list(color = "#FFFFFF", size = 11),
        showlegend    = FALSE, height = h_px,
        margin        = list(l = 220, r = 30, t = 20, b = 50)
      ) %>% config(displayModeBar = FALSE)
  })
  
  
  # ---------- Golf ----------
  
  render_golf_visuals <- function(visuals) {
    req(visuals)
    all_golfers <- if (!is.null(rv$simulation_results)) {
      avgs <- rv$simulation_results[, .(Avg=mean(DKScore)), by=Player]
      setorder(avgs, -Avg); avgs$Player
    } else character(0)
    top15 <- head(all_golfers, 15)
    fluidRow(column(12,
                    box(width=NULL, title="GOLF SIMULATION ANALYSIS", status="primary", solidHeader=TRUE,
                        div(class="gts-chart-filter",
                            span(class="gts-chart-filter-label", "Golfers:"),
                            selectizeInput("golf_player_filter", NULL,
                                           choices=all_golfers, selected=top15, multiple=TRUE,
                                           options=list(plugins=list("remove_button"), placeholder="Select golfers"),
                                           width="600px")
                        ),
                        tabsetPanel(id="golf_visuals_tabs", type="tabs",
                                    tabPanel("Score Distribution", div(style="margin-top:15px;"),
                                             plotlyOutput("golf_score_dist_plot", height="auto") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Cut Rates", div(style="margin-top:15px;"),
                                             DTOutput("golf_cut_rates_table") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6)),
                                    tabPanel("Salary Analysis", div(style="margin-top:15px;"),
                                             plotlyOutput("golf_salary_plot", height="500px") %>%
                                               shinycssloaders::withSpinner(color="#FFE500",type=6))
                        )
                    )
    ))
  }
  
  output$golf_score_dist_plot <- renderPlotly({
    req(rv$sport=="GOLF", rv$simulation_results)
    tryCatch({
      setDT(rv$simulation_results)
      selected <- input$golf_player_filter
      if (length(selected) == 0) {
        avgs <- rv$simulation_results[, .(Avg=mean(DKScore)), by=Player]
        setorder(avgs, -Avg); selected <- head(avgs$Player, 15)
      }
      plot_data <- as.data.frame(rv$simulation_results[Player %in% selected])
      med_ord <- tapply(plot_data$DKScore, plot_data$Player, median)
      plot_data$Player <- factor(plot_data$Player, levels=rev(names(sort(med_ord))))
      h_px <- max(300, length(selected) * 34)
      plot_ly(data=plot_data, x=~DKScore, y=~Player, type="box", orientation="h",
              marker=list(color="#FFE500", size=3), line=list(color="#FFE500"),
              fillcolor="rgba(255,229,0,0.25)") %>%
        layout(
          title=list(text="DK Score Distribution", font=list(color="#FFE500",size=14)),
          xaxis=list(title="DK Fantasy Points", gridcolor="#2a2a2a", color="#888"),
          yaxis=list(title="", color="#ccc", tickfont=list(size=11)),
          paper_bgcolor="#121212", plot_bgcolor="#141414",
          font=list(color="#FFFFFF",size=11), showlegend=FALSE,
          height=h_px, margin=list(l=180,r=30,t=40,b=50))
    }, error=function(e) { plotly_empty() })
  })
  
  output$golf_cut_rates_table <- renderDT({
    req(rv$sport=="GOLF", rv$sim_metadata)
    meta         <- copy(rv$sim_metadata)
    setDT(meta)
    display_cols <- intersect(c("Player","Pool","TeeTimeGroup","CutProb",
                                "DKSalary","DKOwn","FDSalary","FDOwn"),
                              names(meta))
    meta <- meta[, ..display_cols]
    if ("CutProb" %in% names(meta)) meta[, CutProb := round(CutProb * 100, 1)]
    if ("DKOwn"   %in% names(meta)) meta[, DKOwn   := round(DKOwn   * 100, 1)]
    if ("FDOwn"   %in% names(meta)) meta[, FDOwn   := round(FDOwn   * 100, 1)]
    if ("CutProb" %in% names(meta)) setorder(meta, -CutProb)
    dt <- datatable(meta,
                    options=list(pageLength=50, scrollX=TRUE, searching=FALSE,
                                 lengthChange=FALSE, dom='tp'),
                    rownames=FALSE)
    if ("DKSalary" %in% names(meta)) dt <- dt %>% formatCurrency("DKSalary","$",digits=0)
    if ("FDSalary" %in% names(meta)) dt <- dt %>% formatCurrency("FDSalary","$",digits=0)
    dt
  })
  
  output$golf_salary_plot <- renderPlotly({
    req(rv$sport=="GOLF", rv$sim_metadata)
    meta <- copy(rv$sim_metadata)
    setDT(meta)
    if (!("DKSalary" %in% names(meta) && "CutProb" %in% names(meta)))
      return(plotly_empty())
    meta[, CutPct := round(CutProb * 100, 1)]
    plot_data <- as.data.frame(meta[DKSalary > 0])
    plot_ly(data=plot_data, x=~DKSalary, y=~CutPct, text=~Player,
            type="scatter", mode="markers+text",
            marker=list(size=10, color="#FFE500", line=list(color="#000000",width=1)),
            textposition="top center", textfont=list(color="#FFFFFF",size=9)) %>%
      layout(
        title=list(text="Cut Rate vs DK Salary", font=list(color="#FFE500",size=16)),
        xaxis=list(title="DK Salary ($)", gridcolor="#2a2a2a", color="#FFFFFF"),
        yaxis=list(title="Cut Rate (%)", gridcolor="#2a2a2a", color="#FFFFFF"),
        paper_bgcolor="#121212", plot_bgcolor="#141414",
        font=list(color="#FFFFFF"))
  })
  
  
  # ---------- CBB ----------
  
  # ==========================================================================
  # CBB VISUALS — pre-aggregated for performance at 50k sims
  # ==========================================================================
  
  render_cbb_visuals <- function(visuals) {
    req(visuals)
    teams  <- visuals$teams
    is_nba <- isTRUE(rv$sport == "NBA")
    box_title <- if (is_nba) "NBA Analysis" else "College Basketball Analysis"
    
    fluidRow(column(12,
                    box(width = NULL, title = box_title, status = "primary", solidHeader = TRUE,
                        
                        # ── Team averages at TOP ─────────────────────────────────────────
                        div(style = "margin-bottom:20px;",
                            tags$p(style = "font-size:10px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#444;margin-bottom:8px;",
                                   "Team Averages"),
                            DTOutput("cbb_team_table")
                        ),
                        
                        tags$hr(style="border-color:#2a2a2a;margin:16px 0;"),
                        
                        # ── NBA: Mode toggle (By Team / By Position) ─────────────────────
                        if (is_nba)
                          div(id = "nba_mode_pills", style = "margin-bottom:12px;",
                              span(class = "gts-sr-label", style = "margin-right:10px;", "View:"),
                              tags$button(
                                class   = "gts-pill active",
                                onclick = "Shiny.setInputValue('nba_view_mode','team',{priority:'event'});
                             document.querySelectorAll('#nba_mode_pills .gts-pill').forEach(function(b){b.classList.remove('active')});
                             this.classList.add('active')",
                             "By Team"
                              ),
                             tags$button(
                               class   = "gts-pill",
                               onclick = "Shiny.setInputValue('nba_view_mode','position',{priority:'event'});
                             document.querySelectorAll('#nba_mode_pills .gts-pill').forEach(function(b){b.classList.remove('active')});
                             this.classList.add('active')",
                             "By Position"
                             )
                          ),
                        
                        # ── Team pills (shown in team mode) ─────────────────────────────
                        div(id = "cbb_team_pills", style = "margin-bottom:10px;",
                            span(class = "gts-sr-label", style = "margin-right:10px;", "Team:"),
                            lapply(seq_along(teams), function(i) {
                              tags$button(
                                class   = paste("gts-pill", if (i == 1) "active" else ""),
                                onclick = sprintf(
                                  "Shiny.setInputValue('cbb_selected_team','%s',{priority:'event'});
                     document.querySelectorAll('#cbb_team_pills .gts-pill').forEach(function(b){b.classList.remove('active')});
                     this.classList.add('active')", teams[i]),
                     teams[i]
                              )
                            })
                        ),
                     
                     # ── Position pills (shown in position mode, NBA only) ────────────
                     if (is_nba)
                       div(id = "nba_pos_pills", style = "margin-bottom:14px;",
                           span(class = "gts-sr-label", style = "margin-right:10px;", "Position:"),
                           lapply(c("PG","SG","SF","PF","C"), function(pos) {
                             tags$button(
                               class   = paste("gts-pill", if (pos == "PG") "active" else ""),
                               onclick = sprintf(
                                 "Shiny.setInputValue('nba_selected_pos','%s',{priority:'event'});
                       document.querySelectorAll('#nba_pos_pills .gts-pill').forEach(function(b){b.classList.remove('active')});
                       this.classList.add('active')", pos),
                       pos
                             )
                           })
                       ),
                     
                     # ── FP bar chart ─────────────────────────────────────────────────
                     plotlyOutput("cbb_fp_chart", height = "auto") %>%
                       shinycssloaders::withSpinner(color = "#FFE500", type = 6),
                     
                     # ── Per-player stat table ─────────────────────────────────────────
                     div(style = "margin-top:16px;",
                         DTOutput("cbb_stat_table")
                     )
                    )
    ))
  }
  
  
  output$cbb_fp_chart <- renderPlotly({
    req(rv$sport_visuals)
    pm     <- rv$sport_visuals$player_means
    is_nba <- isTRUE(rv$sport == "NBA")
    
    # Platform-aware FP column
    plat     <- if (!is.null(input$sim_results_platform) && nchar(input$sim_results_platform) > 0) input$sim_results_platform else "DK"
    fp_col   <- if (is_nba && plat == "FD" && "FDAvgFP" %in% names(pm)) "FDAvgFP" else if ("DKAvgFP" %in% names(pm)) "DKAvgFP" else "AvgFP"
    fp_label <- if (is_nba) paste(plat, "Fantasy Points Distribution") else "Avg DK Fantasy Points"
    
    # Determine mode and filter data
    view_mode <- if (is_nba && !is.null(input$nba_view_mode)) input$nba_view_mode else "team"
    
    if (is_nba && view_mode == "position") {
      sel_pos <- if (!is.null(input$nba_selected_pos) && nchar(input$nba_selected_pos) > 0) input$nba_selected_pos else "PG"
      req("DKPos" %in% names(pm))
      td <- pm[grepl(paste0("(^|/)", sel_pos, "(/|$)"), DKPos)]
    } else {
      team <- if (!is.null(input$cbb_selected_team) && input$cbb_selected_team %in% rv$sport_visuals$teams)
        input$cbb_selected_team else rv$sport_visuals$teams[1]
      td <- pm[Team == team]
    }
    
    req(nrow(td) > 0)
    
    # ── NBA: box plot from raw sim distributions ──────────────────────────────
    if (is_nba) {
      req(rv$simulation_results)
      score_col <- if (plat == "FD" && "FDScore" %in% names(rv$simulation_results)) "FDScore" else "DKScore"
      
      players_to_show <- td$Player
      sim_dt <- as.data.table(rv$simulation_results)[Player %in% players_to_show,
                                                     .(Player, Score = get(score_col))]
      
      # Cap at 2000 sims per player to keep plotly responsive
      n_sims_total <- length(unique(rv$simulation_results$SimID))
      if (n_sims_total > 2000) {
        keep_sims <- unique(rv$simulation_results$SimID)[seq_len(2000)]
        sim_dt    <- as.data.table(rv$simulation_results)[Player %in% players_to_show &
                                                            SimID %in% keep_sims,
                                                          .(Player, Score = get(score_col))]
      }
      
      req(nrow(sim_dt) > 0)
      
      # Order players by median score ascending (so highest median is at top)
      med_order <- sim_dt[, .(med = median(Score)), by = Player][order(med), Player]
      sim_dt[, Player := factor(Player, levels = med_order)]
      h_px <- max(200, length(med_order) * 48)
      
      plot_ly(data = as.data.frame(sim_dt),
              x = ~Score, y = ~Player,
              type = "box", orientation = "h",
              marker      = list(color = "rgba(255,229,0,0.5)",
                                 outliercolor = "rgba(255,229,0,0.15)",
                                 line  = list(color = "rgba(255,229,0,0.3)", width = 1)),
              line        = list(color = "#FFE500"),
              fillcolor   = "rgba(255,229,0,0.15)",
              hovertemplate = "<b>%{y}</b><br>Median: %{median:.1f}<br>Q1: %{q1:.1f}  Q3: %{q3:.1f}<extra></extra>") %>%
        layout(
          xaxis = list(title = fp_label, gridcolor = "#2a2a2a",
                       color = "#888", zeroline = FALSE),
          yaxis = list(title = "", color = "#ccc", tickfont = list(size = 11),
                       automargin = TRUE,
                       categoryorder = "array", categoryarray = med_order),
          paper_bgcolor = "#121212", plot_bgcolor = "#141414",
          font          = list(color = "#FFFFFF", size = 11),
          showlegend    = FALSE, height = h_px,
          margin        = list(l = 160, r = 60, t = 20, b = 40)
        ) %>% config(displayModeBar = FALSE)
      
    } else {
      # ── CBB: bar chart of averages (unchanged) ──────────────────────────────
      setorderv(td, fp_col, order = 1L)
      h_px <- max(200, nrow(td) * 44)
      
      plot_ly(data = as.data.frame(td),
              x = as.formula(paste0("~", fp_col)),
              y = ~factor(Player, levels = Player),
              type = "bar", orientation = "h",
              marker = list(color = "#FFE500",
                            line  = list(color = "#d4b800", width = 0.5)),
              text  = as.formula(paste0("~round(", fp_col, ", 1)")), textposition = "outside",
              textfont = list(color = "#ccc", size = 11)) %>%
        layout(
          xaxis = list(title = fp_label, gridcolor = "#2a2a2a",
                       color = "#888", zeroline = FALSE),
          yaxis = list(title = "", color = "#ccc", tickfont = list(size = 11),
                       automargin = TRUE),
          paper_bgcolor = "#121212", plot_bgcolor = "#141414",
          font          = list(color = "#FFFFFF", size = 11),
          showlegend    = FALSE, height = h_px,
          margin        = list(l = 160, r = 60, t = 20, b = 40)
        ) %>% config(displayModeBar = FALSE)
    }
  })
  
  
  output$cbb_stat_table <- renderDT({
    req(rv$sport_visuals)
    pm     <- rv$sport_visuals$player_means
    is_nba <- isTRUE(rv$sport == "NBA")
    plat   <- if (!is.null(input$sim_results_platform) && nchar(input$sim_results_platform) > 0) input$sim_results_platform else "DK"
    fp_col <- if (is_nba && plat == "FD" && "FDAvgFP" %in% names(pm)) "FDAvgFP" else if ("DKAvgFP" %in% names(pm)) "DKAvgFP" else "AvgFP"
    
    view_mode <- if (is_nba && !is.null(input$nba_view_mode)) input$nba_view_mode else "team"
    
    if (is_nba && view_mode == "position") {
      sel_pos <- if (!is.null(input$nba_selected_pos) && nchar(input$nba_selected_pos) > 0) input$nba_selected_pos else "PG"
      req("DKPos" %in% names(pm))
      td <- pm[grepl(paste0("(^|/)", sel_pos, "(/|$)"), DKPos)]
    } else {
      team <- if (!is.null(input$cbb_selected_team) && input$cbb_selected_team %in% rv$sport_visuals$teams)
        input$cbb_selected_team else rv$sport_visuals$teams[1]
      td <- pm[Team == team]
    }
    
    req(nrow(td) > 0)
    setorderv(td, fp_col, order = -1L)
    
    if (is_nba) {
      fp_display <- if (fp_col %in% names(td)) fp_col else "AvgFP"
      show_cols  <- intersect(c("Player","DKPos","Team",fp_display,"pts","tpm","twom","ftm","reb","ast","stl","blk","to"),
                              names(td))
      td <- td[, ..show_cols]
      fp_name   <- paste0("Avg ", plat, " FP")
      setnames(td, fp_display, fp_name)
      old_names <- intersect(c("DKPos","Team","pts","tpm","twom","ftm","reb","ast","stl","blk","to"), names(td))
      new_names <- c("Pos","Team","Pts","3PM","2PM","FTM","Reb","Ast","Stl","Blk","TO")[seq_along(old_names)]
      setnames(td, old_names, new_names)
      highlight_col <- fp_name
    } else {
      show_cols <- intersect(c("Player","AvgFP","pts","tpm","twom","ftm","reb","ast","stl","blk","to"), names(td))
      td <- td[, ..show_cols]
      setnames(td, old = intersect(c("AvgFP","pts","tpm","twom","ftm","reb","ast","stl","blk","to"), names(td)),
               new = c("Avg FP","Pts","3PM","2PM","FTM","Reb","Ast","Stl","Blk","TO")[seq_len(
                 length(intersect(c("AvgFP","pts","tpm","twom","ftm","reb","ast","stl","blk","to"), names(td))))])
      highlight_col <- "Avg FP"
    }
    
    datatable(td,
              rownames = FALSE,
              options  = list(dom = "t", paging = FALSE, scrollX = TRUE, ordering = TRUE),
              class    = "stripe compact"
    ) %>% formatStyle(highlight_col, color = "#FFE500", fontWeight = "bold")
  })
  
  
  output$cbb_team_table <- renderDT({
    req(rv$sport_visuals$team_means)
    is_nba <- isTRUE(rv$sport == "NBA")
    tm     <- copy(rv$sport_visuals$team_means)
    plat   <- if (!is.null(input$sim_results_platform) && nchar(input$sim_results_platform) > 0) input$sim_results_platform else "DK"
    
    if (is_nba) {
      fp_col    <- if (plat == "FD" && "FDAvgFP" %in% names(tm)) "FDAvgFP" else if ("DKAvgFP" %in% names(tm)) "DKAvgFP" else "AvgFP"
      show_cols <- intersect(c("Team", fp_col, "pts","tpm","twom","ftm","reb","ast","stl","blk","to"), names(tm))
      tm        <- tm[, ..show_cols]
      fp_name   <- paste0("Avg ", plat, " FP")
      setnames(tm, fp_col, fp_name)
      old_nms   <- intersect(c("pts","tpm","twom","ftm","reb","ast","stl","blk","to"), names(tm))
      setnames(tm, old_nms, c("Pts","3PM","2PM","FTM","Reb","Ast","Stl","Blk","TO")[seq_along(old_nms)])
      highlight_col <- fp_name
    } else {
      show_cols <- intersect(c("Team","AvgFP","pts","tpm","twom","ftm","reb","ast","stl","blk","to"), names(tm))
      tm <- tm[, ..show_cols]
      setnames(tm,
               old = intersect(c("AvgFP","pts","tpm","twom","ftm","reb","ast","stl","blk","to"), names(tm)),
               new = c("Avg FP","Pts","3PM","2PM","FTM","Reb","Ast","Stl","Blk","TO")[seq_len(
                 length(intersect(c("AvgFP","pts","tpm","twom","ftm","reb","ast","stl","blk","to"), names(tm))))]
      )
      highlight_col <- "Avg FP"
    }
    datatable(tm, rownames = FALSE,
              options = list(dom = "t", paging = FALSE, scrollX = TRUE, ordering = TRUE),
              class   = "stripe compact"
    ) %>% formatStyle(highlight_col, color = "#FFE500", fontWeight = "bold")
  })
  
  
  
  # ---------- F1 ----------
  
  render_f1_visuals <- function(visuals) {
    all_drivers <- if (!is.null(visuals$driver_meta)) {
      sort(visuals$driver_meta$Player)
    } else character(0)
    top15 <- if (!is.null(visuals$driver_results)) {
      avgs <- visuals$driver_results[, .(Avg=mean(DKScore)), by=Player]
      setorder(avgs, -Avg); head(avgs$Player, 15)
    } else all_drivers
    fluidRow(column(12,
                    box(width=NULL, title="F1 SIMULATION ANALYSIS", status="primary", solidHeader=TRUE,
                        div(class="gts-chart-filter",
                            span(class="gts-chart-filter-label", "Drivers:"),
                            selectizeInput("f1_driver_filter", NULL,
                                           choices=all_drivers, selected=top15, multiple=TRUE,
                                           options=list(plugins=list("remove_button"), placeholder="Select drivers"),
                                           width="500px")
                        ),
                        tabsetPanel(id="f1_visuals_tabs", type="tabs",
                                    tabPanel("Finishing Position", div(style="margin-top:15px;"),
                                             plotlyOutput("f1_finish_dist_plot", height="auto") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Fantasy Points", div(style="margin-top:15px;"),
                                             plotlyOutput("f1_fp_dist_plot", height="auto") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Laps Led", div(style="margin-top:15px;"),
                                             plotlyOutput("f1_dominator_plot", height="auto") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Constructors", div(style="margin-top:15px;"),
                                             plotlyOutput("f1_constructor_plot", height="auto") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Driver Stats", div(style="margin-top:15px;"),
                                             DTOutput("f1_driver_stats_table") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6)),
                                    tabPanel("Constructor Stats", div(style="margin-top:15px;"),
                                             DTOutput("f1_constructor_stats_table") %>%
                                               shinycssloaders::withSpinner(color="#FFE500", type=6))
                        )
                    )
    ))
  }
  
  output$f1_finish_dist_plot <- renderPlotly({
    req(rv$sport == "F1", rv$sport_visuals$driver_results, rv$sport_visuals$driver_meta)
    tryCatch({
      drv_res  <- rv$sport_visuals$driver_results
      drv_meta <- rv$sport_visuals$driver_meta
      grid_order  <- drv_meta[order(Starting), Player]
      plot_data   <- as.data.frame(drv_res)
      plot_data$Player <- factor(plot_data$Player, levels = rev(grid_order))
      plot_ly(data=plot_data, x=~Finish, y=~Player, type="box", orientation="h",
              marker=list(color="#FFE500"), line=list(color="#FFE500"),
              fillcolor="rgba(255,229,0,0.3)",
              hovertemplate="<b>%{y}</b><br>Median: %{x}<br><extra></extra>") %>%
        layout(
          title=list(text="Finishing Position Distribution (Grid Order)",
                     font=list(color="#FFE500", size=16)),
          xaxis=list(title="Finish Position", gridcolor="#2a2a2a", color="#FFFFFF",
                     autorange="reversed", dtick=2),
          yaxis=list(title="", color="#FFFFFF",
                     categoryorder="array", categoryarray=rev(grid_order)),
          paper_bgcolor="#121212", plot_bgcolor="#141414",
          font=list(color="#FFFFFF", size=12), showlegend=FALSE,
          margin=list(l=160, r=50, t=50, b=50)) %>%
        config(displayModeBar=TRUE,
               modeBarButtonsToRemove=c("select2d","lasso2d","autoScale2d"),
               displaylogo=FALSE)
    }, error=function(e) { plotly_empty() })
  })
  
  output$f1_fp_dist_plot <- renderPlotly({
    req(rv$sport == "F1", rv$sport_visuals$driver_results, rv$sport_visuals$driver_meta)
    tryCatch({
      drv_res  <- rv$sport_visuals$driver_results
      drv_meta <- rv$sport_visuals$driver_meta
      sal_order  <- drv_meta[order(-DKSalary), Player]
      plot_data  <- as.data.frame(drv_res)
      plot_data$Player <- factor(plot_data$Player, levels=rev(sal_order))
      plot_ly(data=plot_data, x=~DKScore, y=~Player, type="box", orientation="h",
              marker=list(color="#FFE500"), line=list(color="#FFE500"),
              fillcolor="rgba(255,229,0,0.3)",
              hovertemplate="<b>%{y}</b><br>Median: %{x:.1f}<br><extra></extra>") %>%
        layout(
          title=list(text="DK Fantasy Points Distribution (Salary Order)",
                     font=list(color="#FFE500", size=16)),
          xaxis=list(title="DK Fantasy Points (Flex)", gridcolor="#2a2a2a", color="#FFFFFF"),
          yaxis=list(title="", color="#FFFFFF",
                     categoryorder="array", categoryarray=rev(sal_order)),
          paper_bgcolor="#121212", plot_bgcolor="#141414",
          font=list(color="#FFFFFF", size=12), showlegend=FALSE,
          margin=list(l=160, r=50, t=50, b=50)) %>%
        config(displayModeBar=TRUE,
               modeBarButtonsToRemove=c("select2d","lasso2d","autoScale2d"),
               displaylogo=FALSE)
    }, error=function(e) { plotly_empty() })
  })
  
  output$f1_dominator_plot <- renderPlotly({
    req(rv$sport == "F1", rv$sport_visuals$driver_results)
    tryCatch({
      drv_res <- rv$sport_visuals$driver_results
      ll_avg  <- drv_res[, .(Avg_LL = mean(LapsLed)), by=Player]
      ll_avg  <- ll_avg[Avg_LL > 0.01]
      if (nrow(ll_avg) == 0) return(plotly_empty())
      setorder(ll_avg, -Avg_LL)
      plot_data <- as.data.frame(ll_avg)
      plot_data$Player <- factor(plot_data$Player, levels=rev(ll_avg$Player))
      plot_ly(data=plot_data, x=~Avg_LL, y=~Player, type="bar", orientation="h",
              marker=list(color="#FFE500", line=list(color="#FFE500", width=1)),
              hovertemplate="<b>%{y}</b><br>Avg Laps Led: %{x:.1f}<br><extra></extra>") %>%
        layout(
          title=list(text="Laps Led - Average Per Driver",
                     font=list(color="#FFE500", size=16)),
          xaxis=list(title="Avg Laps Led", gridcolor="#2a2a2a", color="#FFFFFF"),
          yaxis=list(title="", color="#FFFFFF",
                     categoryorder="array", categoryarray=rev(ll_avg$Player)),
          paper_bgcolor="#121212", plot_bgcolor="#141414",
          font=list(color="#FFFFFF", size=12), showlegend=FALSE,
          margin=list(l=160, r=50, t=50, b=50)) %>%
        config(displayModeBar=TRUE,
               modeBarButtonsToRemove=c("select2d","lasso2d","autoScale2d"),
               displaylogo=FALSE)
    }, error=function(e) { plotly_empty() })
  })
  
  output$f1_constructor_plot <- renderPlotly({
    req(rv$sport == "F1", rv$sport_visuals$constructor_results)
    tryCatch({
      cnstr_res  <- rv$sport_visuals$constructor_results
      med_order  <- cnstr_res[, .(med=median(DKScore)), by=Player][order(-med), Player]
      plot_data  <- as.data.frame(cnstr_res)
      plot_data$Player <- factor(plot_data$Player, levels=rev(med_order))
      plot_ly(data=plot_data, x=~DKScore, y=~Player, type="box", orientation="h",
              marker=list(color="#FFE500"), line=list(color="#FFE500"),
              fillcolor="rgba(255,229,0,0.3)",
              hovertemplate="<b>%{y}</b><br>Median: %{x:.1f}<br><extra></extra>") %>%
        layout(
          title=list(text="Constructor DK Points Distribution",
                     font=list(color="#FFE500", size=16)),
          xaxis=list(title="DK Fantasy Points", gridcolor="#2a2a2a", color="#FFFFFF"),
          yaxis=list(title="", color="#FFFFFF",
                     categoryorder="array", categoryarray=rev(med_order)),
          paper_bgcolor="#121212", plot_bgcolor="#141414",
          font=list(color="#FFFFFF", size=12), showlegend=FALSE,
          margin=list(l=160, r=50, t=50, b=50)) %>%
        config(displayModeBar=TRUE,
               modeBarButtonsToRemove=c("select2d","lasso2d","autoScale2d"),
               displaylogo=FALSE)
    }, error=function(e) { plotly_empty() })
  })
  
  output$f1_driver_stats_table <- renderDT({
    req(rv$sport == "F1", rv$sport_visuals$driver_analysis, rv$sport_visuals$driver_meta)
    tryCatch({
      da   <- copy(rv$sport_visuals$driver_analysis)
      meta <- rv$sport_visuals$driver_meta[, .(Player, DKSalary, CptSalary, Starting)]
      da   <- merge(da, meta, by="Player", all.x=TRUE)
      want_cols <- c("Player","Team","Starting","DKSalary","CptSalary",
                     "Avg_DKScore","Median_DKScore","Avg_CptScore","Median_CptScore",
                     "Win_Rate","Podium_Rate","Points_Rate","Classified_Rate",
                     "Beat_TM_Rate","FL_Rate","Avg_LL",
                     "Avg_FinishPts","Avg_GridPts","Avg_FL_Pts","Avg_LL_Pts",
                     "Avg_BeatTM_Pts","Avg_Cls_Pts","Median_Finish")
      setcolorder(da, intersect(want_cols, names(da)))
      setorder(da, -Avg_DKScore)
      datatable(da, rownames=FALSE,
                options=list(pageLength=25, scrollX=TRUE, scrollY="500px",
                             searching=FALSE, lengthChange=FALSE, dom="t"),
                class="stripe hover compact") %>%
        formatCurrency(intersect(c("DKSalary","CptSalary"), names(da)), "$", digits=0) %>%
        formatRound(intersect(c("Avg_DKScore","Median_DKScore","Avg_CptScore","Median_CptScore",
                                "Avg_FinishPts","Avg_GridPts","Avg_FL_Pts","Avg_LL_Pts",
                                "Avg_BeatTM_Pts","Avg_Cls_Pts","Median_Finish","Avg_LL"), names(da)), 1) %>%
        formatStyle("Avg_DKScore",
                    background=styleColorBar(range(da$Avg_DKScore, na.rm=TRUE), "#FFE500"),
                    backgroundSize="90% 70%", backgroundRepeat="no-repeat",
                    backgroundPosition="left")
    }, error=function(e) {
      datatable(data.table(Error=e$message), rownames=FALSE)
    })
  })
  
  output$f1_constructor_stats_table <- renderDT({
    req(rv$sport == "F1", rv$sport_visuals$constructor_analysis)
    tryCatch({
      ca <- copy(rv$sport_visuals$constructor_analysis)
      setorder(ca, -Median_Score)
      datatable(ca, rownames=FALSE,
                options=list(pageLength=15, scrollX=TRUE,
                             searching=FALSE, lengthChange=FALSE, dom="t"),
                class="stripe hover compact") %>%
        formatCurrency("DKSalary", "$", digits=0) %>%
        formatRound(c("Avg_Score","Median_Score","P75_Score","P90_Score"), 1) %>%
        formatStyle("Median_Score",
                    background=styleColorBar(range(ca$Median_Score, na.rm=TRUE), "#FFE500"),
                    backgroundSize="90% 70%", backgroundRepeat="no-repeat",
                    backgroundPosition="left")
    }, error=function(e) {
      datatable(data.table(Error=e$message), rownames=FALSE)
    })
  })
  
  
  # ---------- Soccer ----------
  
  # ============================================================================
  # SOCCER SIMULATION ANALYSIS
  # ============================================================================
  
  render_soccer_visuals <- function(visuals) {
    req(visuals)
    fluidRow(column(12,
                    box(width = NULL, title = "SOCCER SIMULATION ANALYSIS",
                        status = "primary", solidHeader = TRUE,
                        tabsetPanel(id = "soccer_visuals_tabs", type = "tabs",
                                    
                                    # TAB 1: GAME OVERVIEW
                                    tabPanel("Game Overview", div(style = "margin-top:15px;"),
                                             uiOutput("soccer_game_overview_container"),
                                             div(style = "margin-top:20px;"),
                                             uiOutput("soccer_scoreline_container")),
                                    
                                    # TAB 2: PLAYER PROJECTIONS
                                    tabPanel("Player Projections", div(style = "margin-top:15px;"),
                                             DT::DTOutput("soccer_proj_table"),
                                             div(style = "margin-top:20px;"),
                                             DT::DTOutput("soccer_stat_table")),
                                    
                                    # TAB 3: SCORE DISTRIBUTIONS
                                    tabPanel("Score Distributions", div(style = "margin-top:15px;"),
                                             uiOutput("soccer_violin_container")),
                                    
                                    # TAB 4: TEAM STATS
                                    tabPanel("Team Stats", div(style = "margin-top:15px;"),
                                             DT::DTOutput("soccer_team_table"),
                                             div(style = "margin-top:20px;"),
                                             uiOutput("soccer_team_hist_container"),
                                             div(style = "margin-top:20px;"),
                                             tags$h4(style = "color:#FFE500;", "Cross-Reference Validation"),
                                             DT::DTOutput("soccer_xref_table")),
                                    
                                    # TAB 5: STAT DISTRIBUTIONS
                                    tabPanel("Stat Distributions", div(style = "margin-top:15px;"),
                                             tags$p(style = "color:#888;", "Simulated stat distributions — top 10 players per stat"),
                                             uiOutput("soccer_stat_violin_container")),
                                    
                                    # TAB 6: GOAL ANALYSIS
                                    tabPanel("Goal Analysis", div(style = "margin-top:15px;"),
                                             tags$h4(style = "color:#FFE500;", "Goal Probability by Player"),
                                             DT::DTOutput("soccer_goal_freq_table"),
                                             div(style = "margin-top:20px;"),
                                             plotlyOutput("soccer_goal_freq_plot", height = "500px") %>%
                                               shinycssloaders::withSpinner(color = "#FFE500", type = 6))
                        )
                    )
    ))
  }
  
  # ── CONTAINERS ──
  
  output$soccer_game_overview_container <- renderUI({
    req(rv$sport == "SOCCER", rv$sport_visuals$game_overview)
    go <- rv$sport_visuals$game_overview
    tagList(lapply(seq_along(go), function(i) {
      g <- go[[i]]
      fluidRow(
        column(4,
               tags$div(style = "background:#1a1a1a; padding:15px; border-radius:8px; margin-bottom:15px;",
                        tags$h4(style = "color:#FFE500; margin-top:0;", g$game),
                        tags$p(style = "color:#ccc; font-size:16px;",
                               sprintf("%s %.2f — %.2f %s", g$home, g$avg_hg, g$avg_ag, g$away)),
                        tags$p(style = "color:#888;",
                               sprintf("Win: %.0f%% | Draw: %.0f%% | Loss: %.0f%%", g$h_win, g$draw, g$a_win)),
                        tags$p(style = "color:#888;",
                               sprintf("CS: %s %.0f%% | %s %.0f%%", g$home, g$h_cs, g$away, g$a_cs))
               )),
        column(8,
               plotlyOutput(paste0("soccer_total_goals_", i), height = "200px") %>%
                 shinycssloaders::withSpinner(color = "#FFE500", type = 6))
      )
    }))
  })
  
  output$soccer_violin_container <- renderUI({
    req(rv$sport == "SOCCER", rv$sport_visuals)
    teams <- rv$sport_visuals$teams
    tagList(lapply(seq_along(teams), function(i) {
      n_pl <- length(unique(rv$sport_visuals$score_dist[Team == teams[i]]$Player))
      plotlyOutput(paste0("soccer_violin_", i), height = paste0(max(250, n_pl * 38), "px")) %>%
        shinycssloaders::withSpinner(color = "#FFE500", type = 6)
    }))
  })
  
  output$soccer_scoreline_container <- renderUI({
    req(rv$sport == "SOCCER", rv$sport_visuals$scoreline_data)
    games <- unique(rv$sport_visuals$scoreline_data$Game)
    tagList(lapply(seq_along(games), function(i) {
      plotlyOutput(paste0("soccer_scoreline_", i), height = "400px") %>%
        shinycssloaders::withSpinner(color = "#FFE500", type = 6)
    }))
  })
  
  output$soccer_stat_violin_container <- renderUI({
    req(rv$sport == "SOCCER", rv$sport_visuals$stat_dist)
    st <- rv$sport_visuals$stat_dist
    val_stats <- intersect(c("Goals","Shots","SOT","CC","Crosses","TKLW","FD","FC","Passes","INT"), names(st))
    tagList(lapply(seq_along(val_stats), function(i) {
      plotlyOutput(paste0("soccer_stat_violin_", i), height = "380px") %>%
        shinycssloaders::withSpinner(color = "#FFE500", type = 6)
    }))
  })
  
  output$soccer_team_hist_container <- renderUI({
    req(rv$sport == "SOCCER", rv$sport_visuals$team_sim_stats)
    hist_stats <- c("Goals","Shots","SOT","Crosses","TKLW","Passes")
    tagList(
      fluidRow(
        lapply(seq_along(hist_stats), function(i) {
          column(4, plotlyOutput(paste0("soccer_team_hist_", i), height = "250px") %>%
                   shinycssloaders::withSpinner(color = "#FFE500", type = 6))
        })
      )
    )
  })
  
  # ── TABLES ──
  
  output$soccer_proj_table <- DT::renderDT({
    req(rv$sport == "SOCCER", rv$sport_visuals)
    pm <- rv$sport_visuals$player_means
    cols <- intersect(c("Player","Team","Pos","Salary",
                        "DKAvgFP","SDFP","P10","P50","P90","Ceiling"), names(pm))
    datatable(pm[, ..cols],
              options = list(pageLength = 25, scrollX = TRUE, dom = "ftip",
                             order = list(list(which(cols=="DKAvgFP")-1, "desc"))),
              class = "compact stripe", rownames = FALSE
    ) %>% formatRound(intersect(cols[5:length(cols)], cols), 2)
  })
  
  output$soccer_stat_table <- DT::renderDT({
    req(rv$sport == "SOCCER", rv$sport_visuals)
    pm <- rv$sport_visuals$player_means
    cols <- intersect(c("Player","Team","Pos","Salary","DKAvgFP",
                        "AvgGoals","AvgAst","AvgShots","AvgSOT","AvgCC",
                        "AvgCross","AvgTackles","AvgINT",
                        "AvgFD","AvgFC","AvgPasses","AvgYC","AvgSaves"), names(pm))
    datatable(pm[, ..cols],
              options = list(pageLength = 25, scrollX = TRUE, dom = "ftip",
                             order = list(list(which(cols=="DKAvgFP")-1, "desc"))),
              class = "compact stripe", rownames = FALSE
    ) %>% formatRound(intersect(cols[5:length(cols)], cols), 2)
  })
  
  output$soccer_team_table <- DT::renderDT({
    req(rv$sport == "SOCCER", rv$sport_visuals)
    datatable(rv$sport_visuals$team_means,
              options = list(dom = "t", scrollX = TRUE),
              class = "compact stripe", rownames = FALSE
    ) %>% formatRound(setdiff(names(rv$sport_visuals$team_means), "Team"), 1)
  })
  
  output$soccer_xref_table <- DT::renderDT({
    req(rv$sport == "SOCCER", rv$sport_visuals$xref)
    xr <- rv$sport_visuals$xref
    xr[, Status := ifelse(Match, "\u2713", "\u2717")]
    datatable(xr[, .(Check, Team, Value, ShouldEqual, Status)],
              options = list(dom = "t", pageLength = 20),
              class = "compact stripe", rownames = FALSE)
  })
  
  output$soccer_goal_freq_table <- DT::renderDT({
    req(rv$sport == "SOCCER", rv$sport_visuals$goal_freq)
    gf <- rv$sport_visuals$goal_freq
    gf_show <- gf[G0 < 100, .(Player, Team, Pos, Salary,
                              `0 Goals %`=G0, `1 Goal %`=G1,
                              `2 Goals %`=G2, `3+ Goals %`=G3plus,
                              `1+ Assist %`=A1plus)]
    datatable(gf_show,
              options = list(pageLength = 25, scrollX = TRUE, dom = "ftip",
                             order = list(list(4, "asc"))),
              class = "compact stripe", rownames = FALSE)
  })
  
  # ── PLOTS ──
  
  # Total goals distribution (one per game)
  observe({
    req(rv$sport == "SOCCER", rv$sport_visuals$game_overview)
    go <- rv$sport_visuals$game_overview
    for (i in seq_along(go)) { local({
      gi <- i; g <- go[[gi]]
      output[[paste0("soccer_total_goals_", gi)]] <- renderPlotly({
        td <- g$total_goals_dist
        plot_ly(data=as.data.frame(td), x=~TotalGoals, y=~Prob,
                type="bar", marker=list(color="#FFE500")) %>%
          layout(title=list(text="Total Goals Distribution",
                            font=list(color="#FFE500", size=12)),
                 xaxis=list(title="Total Goals", color="#888", dtick=1),
                 yaxis=list(title="Probability (%)", color="#888"),
                 paper_bgcolor="transparent", plot_bgcolor="#141414",
                 font=list(color="#FFFFFF", size=10),
                 margin=list(l=50, r=20, t=35, b=40)) %>%
          config(displayModeBar=FALSE)
      })
    })}
  })
  
  # DK Score box plots (one per team)
  observe({
    req(rv$sport == "SOCCER", rv$sport_visuals)
    teams <- rv$sport_visuals$teams
    sd <- rv$sport_visuals$score_dist
    for (i in seq_along(teams)) { local({
      ti <- i; tm <- teams[ti]
      output[[paste0("soccer_violin_", ti)]] <- renderPlotly({
        td <- sd[Team == tm]
        med_ord <- td[, .(med=median(DKScore)), by=Player][order(-med)]$Player
        td[, Player := factor(Player, levels=rev(med_ord))]
        n_pl <- length(unique(td$Player))
        plot_ly(data=as.data.frame(td), y=~Player, x=~DKScore,
                type="box", orientation="h",
                marker=list(color="#FFE500", size=2),
                line=list(color="#FFE500"),
                fillcolor="rgba(255,229,0,0.2)") %>%
          layout(title=list(text=paste(tm, "— DK Score Distribution"),
                            font=list(color="#FFE500", size=14)),
                 xaxis=list(title="DK Points", gridcolor="#2a2a2a", color="#888", zeroline=FALSE),
                 yaxis=list(title="", color="#ccc", tickfont=list(size=11), automargin=TRUE),
                 paper_bgcolor="#121212", plot_bgcolor="#141414",
                 font=list(color="#FFFFFF", size=11), showlegend=FALSE,
                 height=max(250, n_pl*38),
                 margin=list(l=180, r=30, t=40, b=50)) %>%
          config(displayModeBar=FALSE)
      })
    })}
  })
  
  # Scoreline distribution plots (one per game)
  observe({
    req(rv$sport == "SOCCER", rv$sport_visuals$scoreline_data)
    sl <- rv$sport_visuals$scoreline_data
    games <- unique(sl$Game)
    for (i in seq_along(games)) { local({
      gi <- i; gm <- games[gi]
      output[[paste0("soccer_scoreline_", gi)]] <- renderPlotly({
        gd <- sl[Game == gm]; n_t <- nrow(gd)
        tab <- gd[, .(Count=.N, Pct=round(.N/n_t*100,1)), by=Scoreline]
        setorder(tab, -Count); tab <- head(tab, 15)
        tab[, Scoreline := factor(Scoreline, levels=rev(tab$Scoreline))]
        hw <- round(mean(gd$HG > gd$AG)*100,1)
        dr <- round(mean(gd$HG == gd$AG)*100,1)
        aw <- round(mean(gd$HG < gd$AG)*100,1)
        plot_ly(data=as.data.frame(tab), y=~Scoreline, x=~Pct,
                type="bar", orientation="h",
                marker=list(color="#FFE500"),
                text=~paste0(Pct,"%"), textposition="outside",
                textfont=list(color="#ccc", size=11)) %>%
          layout(title=list(text=sprintf("%s  |  Home %.0f%%  Draw %.0f%%  Away %.0f%%",
                                         gm, hw, dr, aw),
                            font=list(color="#FFE500", size=14)),
                 xaxis=list(title="Probability (%)", gridcolor="#2a2a2a", color="#888",
                            range=c(0, max(tab$Pct)*1.4)),
                 yaxis=list(title="", color="#ccc", tickfont=list(size=11)),
                 paper_bgcolor="#121212", plot_bgcolor="#141414",
                 font=list(color="#FFFFFF", size=11), showlegend=FALSE,
                 margin=list(l=80, r=50, t=50, b=50)) %>%
          config(displayModeBar=FALSE)
      })
    })}
  })
  
  # Team stat histograms (side-by-side per stat)
  observe({
    req(rv$sport == "SOCCER", rv$sport_visuals$team_sim_stats)
    tss <- rv$sport_visuals$team_sim_stats
    hist_stats <- intersect(c("Goals","Shots","SOT","Crosses","TKLW","Passes"), names(tss))
    teams <- unique(tss$Team)
    colors <- c("#FFE500", "#888888")
    for (i in seq_along(hist_stats)) { local({
      si <- i; sn <- hist_stats[si]
      output[[paste0("soccer_team_hist_", si)]] <- renderPlotly({
        p <- plot_ly()
        for(ti in seq_along(teams)) {
          vals <- tss[Team==teams[ti]][[sn]]
          p <- p %>% add_histogram(x=vals, name=teams[ti],
                                   marker=list(color=colors[ti]),
                                   opacity=0.7, nbinsx=20)
        }
        p %>% layout(title=list(text=sn, font=list(color="#FFE500", size=12)),
                     barmode="overlay",
                     xaxis=list(title=sn, color="#888"),
                     yaxis=list(title="Frequency", color="#888"),
                     paper_bgcolor="transparent", plot_bgcolor="#141414",
                     font=list(color="#FFFFFF", size=10),
                     legend=list(font=list(color="#ccc")),
                     margin=list(l=50, r=20, t=35, b=40)) %>%
          config(displayModeBar=FALSE)
      })
    })}
  })
  
  # Stat validation box plots (top 10 per stat)
  observe({
    req(rv$sport == "SOCCER", rv$sport_visuals$stat_dist)
    st <- rv$sport_visuals$stat_dist
    val_stats <- intersect(c("Goals","Shots","SOT","CC","Crosses","TKLW","FD","FC","Passes","INT"), names(st))
    for (i in seq_along(val_stats)) { local({
      si <- i; sn <- val_stats[si]
      output[[paste0("soccer_stat_violin_", si)]] <- renderPlotly({
        sdata <- st[, .(Player, Team, Value = get(sn))]
        top10 <- sdata[, .(Mean=mean(Value)), by=Player][order(-Mean)][1:min(10,.N)]$Player
        sdata <- sdata[Player %in% top10]
        sdata[, Player := factor(Player, levels=rev(top10))]
        tm_colors <- setNames(c("#FFE500", "#888888"), unique(sdata$Team))
        plot_ly(data=as.data.frame(sdata), y=~Player, x=~Value, color=~Team,
                colors=tm_colors,
                type="box", orientation="h",
                line=list(width=1.5),
                fillcolor="rgba(255,229,0,0.15)") %>%
          layout(title=list(text=paste(sn, "— Distribution (Top 10)"),
                            font=list(color="#FFE500", size=14)),
                 xaxis=list(title=sn, gridcolor="#2a2a2a", color="#888", zeroline=FALSE),
                 yaxis=list(title="", color="#ccc", tickfont=list(size=11), automargin=TRUE),
                 paper_bgcolor="#121212", plot_bgcolor="#141414",
                 font=list(color="#FFFFFF", size=11),
                 legend=list(font=list(color="#ccc"), orientation="h", y=-0.15),
                 height=380, margin=list(l=180, r=30, t=40, b=60)) %>%
          config(displayModeBar=FALSE)
      })
    })}
  })
  
  # Goal frequency plot
  output$soccer_goal_freq_plot <- renderPlotly({
    req(rv$sport == "SOCCER", rv$sport_visuals$goal_freq)
    gf <- rv$sport_visuals$goal_freq[G0 < 100]  # exclude GKs
    setorder(gf, G0)
    gf[, Player := factor(Player, levels=Player)]
    plot_ly(data=as.data.frame(gf)) %>%
      add_trace(y=~Player, x=~G1, type="bar", orientation="h",
                name="1 Goal", marker=list(color="#FFE500")) %>%
      add_trace(y=~Player, x=~G2, type="bar", orientation="h",
                name="2 Goals", marker=list(color="#FF8C00")) %>%
      add_trace(y=~Player, x=~G3plus, type="bar", orientation="h",
                name="3+ Goals", marker=list(color="#FF4444")) %>%
      layout(title=list(text="Goal Scoring Probability (%)",
                        font=list(color="#FFE500", size=14)),
             barmode="stack",
             xaxis=list(title="Probability (%)", color="#888", range=c(0, 100)),
             yaxis=list(title="", color="#ccc", tickfont=list(size=10), automargin=TRUE),
             paper_bgcolor="#121212", plot_bgcolor="#141414",
             font=list(color="#FFFFFF", size=11),
             legend=list(font=list(color="#ccc"), orientation="h", y=-0.1),
             height=max(350, nrow(gf)*25),
             margin=list(l=180, r=30, t=40, b=60)) %>%
      config(displayModeBar=FALSE)
  })
  
  
  # CASH GAME MODULE
  # ==========================================================================
  register_cash_game_observers(input, output, session, rv)
  
  
}


# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui, server)