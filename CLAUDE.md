# GTS

Four R/Shiny apps for Golden Ticket Sims, a daily-fantasy sports simulation
operation. Solo-maintained.

| App | What it is | How it reaches users |
| --- | --- | --- |
| `SimApp/` | The simulator. 10 sport engines behind one dashboard. | `runGitHub` off `main` — see below |
| `TheLab/` | NASCAR research + input-sheet builder. | shinyapps.io |
| `AuctionDraft/` | Status unknown — do not assume it is live. | — |
| `OhHell/` | Status unknown — do not assume it is live. | — |

## `main` is production

`GTS2026Launch.R` (in `Documents\`, outside this repo) is what customers run:

```r
shiny::runGitHub(repo = "GTS", username = "astukas12",
                 subdir = "SimApp", ref = "main")
```

Three consequences, and they govern everything else in this file:

1. **There is no release step.** Whatever is on `main` is what the next customer
   runs, the moment it is pushed. Never push a broken `main`.
2. **This repo must stay public.** `runGitHub` fetches the public archive.
   Making it private breaks every customer.
3. **`runGitHub` downloads the whole repo**, then runs the `SimApp` subdir. Every
   tracked file anywhere in the repo is on the customer's download path — 16MB
   today for roughly 1MB of app. Adding large files here has a direct cost.

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
- `TheLab` deploys separately via `rsconnect` → shinyapps.io, account
  `goldenticketsims`. That path publishes from local files and does not read
  GitHub, so TheLab's deployed state and repo state are independent. **SimApp is
  the opposite** — see "`main` is production" above.
- This repo is public and must stay so. Keep credentials, tokens, and customer
  data out of every file here.

## Standing rules

**Branch before structural work.** Any change touching multiple files or
restructuring a file gets a git branch first. Andrew runs live slates off this
working copy and customers run `main` directly — `main` must stay runnable at
all times.

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
