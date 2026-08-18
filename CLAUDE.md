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

The launcher is not tracked in this repo — it lives on Andrew's machine, and
each customer holds their own copy with `ref = "main"` baked in. It cannot be
repointed after the fact, so **`main` is the release branch and that is fixed.**

Three consequences, and they govern everything else in this file:

1. **This repo must stay public.** `runGitHub` fetches the public archive.
   Making it private breaks every customer.
2. **`runGitHub` downloads the whole repo**, then runs the `SimApp` subdir. Every
   tracked file anywhere in the repo is on the customer's download path — 15.7MB
   today for roughly 1MB of app. Adding large files here has a direct cost.
3. **Never commit to `main`.** See below.

## Branching: `dev` is where work happens

`main` ships to customers, so it is not a working branch.

```
git checkout dev          # everything happens here
git commit ...            # freely

git checkout main         # only to ship
git merge --no-ff dev     # this is the release
git push                  # customers get it now
```

Feature branches come off `dev` and merge back to `dev`. `main` only ever
receives merges from `dev`, and only when the change has been verified.

A `pre-commit` hook in `.git/hooks/` refuses direct commits on `main` and
explains why. It allows merge commits, since that is the ship action. It is
local and untracked, so it does not reach customers. `--no-verify` overrides it
if you genuinely mean to.

## The wider operation

Work here almost always touches one or both of these, and both are already in
scope — no need to request access:

- `C:\Users\astuk\OneDrive\Documents\GTS` — the working-data tree. Per-sport
  folders holding InputMaker scripts, database-update scripts, and real input
  sheets. Has its own CLAUDE.md.
- `C:\Users\astuk\OneDrive\Documents\GitHub\NicheSportSimsPublic` — the previous
  generation: 15 Shiny apps behind their own customer launcher. Sims run through
  `SimApp` now, so treat it as legacy. Two things in it still matter — a working
  CFB app, and `Sweat/`. Its own CLAUDE.md has the detail.

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

**The operation is live.** Andrew builds input sheets for real slates from this
working copy while the overhaul is happening. Tennis runs daily; MMA and NASCAR
on weekends; NFL preseason for three weeks in August; NFL and CFB from early
autumn. Three rules follow:

1. **Never end a session with the working copy unable to produce a sheet.** This
   outranks finishing whatever refactor is in progress.
2. **Ask what is on the slate before touching a sport's engine or InputMaker.**
   Tennis is daily, so tennis code is effectively always in production.
3. **Structural work belongs in a git worktree**, so the copy Andrew builds
   sheets from is never mid-surgery. A branch alone is not enough when one
   directory is doing both jobs. Prefer additive change during live weeks — new
   file alongside old, switch when verified — over in-place restructuring.

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

- **No tests anywhere.** Nothing can be verified safe by automation yet. The
  bar is the manual launch-and-load check above.
- `SimApp/app.R` is 4,939 lines in one file. Read the part you need — grep for
  the function, then read that range. Do not read it whole.
- Three `.bak` files sit in `SimApp/`, untracked and gitignored. They are
  Andrew's older safety net; leave them alone. Use a branch instead.
- `SimApp/tennis_clean_database.xlsx` (8.6MB) is a real runtime dependency of
  `tennis_engine.R`, so it has to stay tracked — but it is on every customer's
  download path. Converting it to `.rds` would shrink it sharply; that is a code
  change and wants a test around it.
- `TheLab/NascarData.xlsx` (4.7MB) is used only by TheLab, yet SimApp customers
  download it too. Splitting TheLab out is the fix, and it is not scoped yet.
