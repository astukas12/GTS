# TheLab

"Golden Ticket Research Center" — a NASCAR research app and the surface where
NASCAR input sheets get built.

Deployed to shinyapps.io as `TheLab` (account `goldenticketsims`), live at
https://goldenticketsims.shinyapps.io/TheLab/.

## Shape

- `app.R` — ~150KB, single file. Uses the tidyverse stack (`dplyr`, `tidyr`,
  `stringr`, `readr`) plus `openxlsx` for writing sheets. This is a **different
  idiom from SimApp**, which is `data.table`-based. Do not carry conventions
  across between the two.
- `DBUpdate_2026.R` — the NASCAR database update script.
- `gts_model.R`, `backtest.R`, `sweep_dkmax.R`, `candidate_dkmax.R` — modelling
  and backtesting.
- `IndyGreenFlagPasses.R`, `IndyPassReport.Rmd` — one-off analysis.
- `InputFiles/` — 72 real NASCAR input sheets (`CupIowa.xlsx`,
  `CupAtlanta.xlsx`, …) plus a `2025/` subfolder. **Untracked.** These are the
  best available fixtures for testing NASCAR input loading.

## Facts that bite

- `DATA_FILE <- "NascarData.xlsx"` is resolved relative to the working
  directory, and `CURRENT_YEAR` is computed from `Sys.Date()` at startup — so
  behaviour changes at the new year without any code change.
- `safe_trimws()` exists because the source data carries invalid UTF-8. Use it
  rather than bare `trimws()` on anything read from the spreadsheets.
- `.claude/launch.json` here launches **SimApp**, not TheLab. It is misfiled but
  it works; leave it unless asked.

## Where this is heading

TheLab is planned to become the general input-sheet studio for every sport
rather than a NASCAR-only side app (project A13). Prefer changes that
generalize over changes that deepen the NASCAR coupling.
