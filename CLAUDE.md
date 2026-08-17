# GTS

Four R/Shiny apps for Golden Ticket Sims, a daily-fantasy sports simulation
operation. Solo-maintained.

| App | What it is | Deployed |
| --- | --- | --- |
| `SimApp/` | The simulator. 10 sport engines behind one dashboard. | local only |
| `TheLab/` | NASCAR research + input-sheet builder. | shinyapps.io |
| `AuctionDraft/` | Status unknown — do not assume it is live. | — |
| `OhHell/` | Status unknown — do not assume it is live. | — |

## The wider operation

Work here almost always touches one or both of these, and both are already in
scope — no need to request access:

- `C:\Users\astuk\OneDrive\Documents\GTS` — the working-data tree. Per-sport
  folders holding InputMaker scripts, database-update scripts, and real input
  sheets. Has its own CLAUDE.md.
- `C:\Users\astuk\OneDrive\Documents\GitHub\NicheSportSimsPublic\Sweat` — the
  MMA sweat tool. Separate repo, its own CLAUDE.md.

## Environment

- R 4.4.2 at `C:\Program Files\R\R-4.4.2\bin\Rscript.exe`.
- Windows. Paths in R code use forward slashes or escaped backslashes.
- Deployment is `rsconnect` → shinyapps.io, account `goldenticketsims`. It
  publishes from local files and **never reads GitHub**, so repo state and
  deployed state are independent.
- Both repos are currently public on GitHub. Keep credentials, tokens, and
  customer data out of every file here.

## Standing rules

**Branch before structural work.** Any change touching multiple files or
restructuring a file gets a git branch first. Andrew runs live slates off this
working copy — `main` must stay runnable at all times.

**Verification bar: launch and load.** An app change is not done until the app
has been started and a real input sheet for an affected sport has been loaded
successfully. Say exactly what was checked. There is no test suite yet, so a
change that has only been read is not a change that has been verified.

**Match the surrounding file.** These files have drifted in style over time.
New code follows whatever the file it lives in already does — do not introduce
a new idiom or a new dependency to a file that does not use it.

**Design before edits on anything structural.** For engine contracts, module
splits, schema work, or pipeline design: agree the target shape first, then
build. Plan mode is the right tool for this.

## Known state (August 2026)

- **No tests anywhere.** Nothing can be verified safe by automation yet.
- No `.gitignore`. Three `.bak` files sit in `SimApp/` — `app.R.bak` and
  `sport_configs_universal.R.bak` are **committed to git**, `app.R.bak-20260815-000739`
  is untracked. `TheLab/InputFiles/` (72 sheets) is untracked.
- `tennis_clean_database.xlsx` (8.4MB) is committed in-repo.
- `SimApp/app.R` is ~4,940 lines in one file. Read the part you need — grep for
  the function, then read that range. Do not read it whole.
