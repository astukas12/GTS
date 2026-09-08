# Sweat

The Golden Ticket contest sweat tool — multi-sport DFS contest analysis. Lives
in the `GTS` repo alongside `SimApp/` and `TheLab/`; see the repo-root CLAUDE.md
for why the repo is public and must stay so.

Customers run it exactly like SimApp — a standalone `GTSSweatLaunch.R` on their
machine calls `shiny::runGitHub(repo = "GTS", username = "astukas12",
subdir = "Sweat", ref = "main")`. So `main` is production for this app too:
work on it directly, and keep it runnable. Moved here from
`NicheSportSimsPublic/Sweat` in Sept 2026; the old shinyapps.io deploy and that
repo's launcher slot are retired.

## Shape

- `app.r` — the whole app, ~2,100 lines. Note the **lowercase `.r`**; some
  tooling is case-sensitive, and on Windows `app.R` is the *same file*.
- `app_legacy_mma.R` — the pre-2026 MMA-only app (5,000 lines). Kept because it
  still holds the **Simulation Analysis** tab (sim-optimals upload), which has
  not been ported to the new engine. Its Live Sweat tab *has* been replaced, by
  the Live Lineups tab described below. Do not delete it until Sim Analysis
  moves across.
- `tests_regression.R` — the regression suite (see Testing below).
- `tests_mma_sd_fixture.R` — generates the synthetic MMA Showdown export used
  by that suite.
- `STATUS.md` — where the rewrite stands and what is queued next. Read it
  before starting a new wave of changes.
- `www/gts_theme.css`, `www/logo.JPG` — branding assets.
- The `nflsweat/` NFL-only prototype and the `MMAReview/` post-contest tool that
  preceded this engine stayed behind in `NicheSportSimsPublic`; both are fully
  subsumed here.

Roughly: constants and helpers to ~line 175, the per-sport `read_input_*`
adapters to ~430, the `SPORTS` registry and `detect_sport()` to ~600, UI to
~965, then the server.

## Architecture

The app is a generic engine plus one adapter per sport.

1. `parse_lineup_slots()` splits a DK `Lineup` string into (slot, player) pairs
   by walking whitespace tokens and treating any label in `SLOT_TOKENS` as a
   delimiter. `SLOT_TOKENS` is the **union across all sports**, so one parser
   covers NASCAR (`D`), NFL Classic (`QB/RB/WR/TE/FLEX/DST`), Showdown
   (`CPT/FLEX`), NBA, CBB, MMA, Tennis and Golf.
2. `detect_sport()` identifies the sport from the *set* of slot labels present.
   Order matters — F1 lineups contain `D` like NASCAR, NBA contains `G`/`F`/
   `UTIL` like CBB — so specific tests run first. Where slots are ambiguous,
   `sport_key()` refines the guess: a supplied input workbook names the sport
   via `identify_workbook_family()` (sheet signature) and the contest's slot
   set says classic vs showdown — this is how `CFB-SD` is separated from CBB
   Showdown, both `CPT`/`UTIL`. A UI dropdown overrides everything, which is
   also how NFL Showdown and Soccer Showdown get told apart (`CPT`/`FLEX`).
3. `SPORTS` is the adapter registry. **Adding a sport means adding one entry.**
   Each supplies `slots`, `entity` (Player/Driver/Fighter), a `read_input`
   function for that sport's sim workbook, `group_dims` (display label → column)
   and `extra_cols`. `group_dims` is what drives the Breakdown tab, so it is
   where "reframe each sport to its own input sheet" actually lives.
4. `resolve_metadata()` joins DK display names to the input sheet: exact match
   on `norm_name()`, then NFL nickname lookup for defenses, then a conservative
   Jaro-Winkler fallback. It never fuzzy-matches a defense onto a skill player.

   The fuzzy cutoff is `FUZZY_MAX_DIST`, and the comment there carries the
   measurement behind it. The short version: with correctly paired files, a
   genuine spelling variant is *very* close — across all five sports only two
   names have ever needed fuzzy at all ("British Brooks" → "Brittish Brooks",
   0.013; "Joshua Pitsenberger" → "Josh Pitsenberger", 0.021). An input file
   for the wrong slate invents matches an order of magnitude further out
   ("Ben Black III" → "Benji Blackburn", 0.093). Those two populations do not
   overlap, so a plain distance cutoff at 0.05 separates them; do not reach for
   extra heuristics without re-measuring.

   `norm_name()` folds accents via `chartr()` rather than stripping them, so
   "Rodríguez" normalises to one surname rather than being split into
   "rodr guez".

Currently adapted: **NASCAR** (Driver sheet — team/org, car, salary, starting
spot, DKOP projected ownership), **NFL** (IDs + Games + per-team depth sheets),
**MMA** (Fights sheet — opponent, weight class, salary, DKOwn, de-vigged win
probability), **MMA-SD** (captain mode) and **CFB**. Every other sport parses
and produces exposure, slot, combo and dupe views, but has no metadata until
someone writes its `read_input`.

### CFB

DK college football is QB/RB/WR/FLEX **plus a superflex**, whose slot label is
the single hyphenated token `S-FLEX`. Detection tests for it *before* NFL,
which the roster would otherwise match.

`read_input_cfb()` handles **two live workbook layouts** and must keep doing so:

| | older (`*_CFB_ALL`) | newer (`*_CFB_THU`) |
| --- | --- | --- |
| `dk_pos`, `salary_util` | on the team sheets | on `projections` |
| team | the sheet name | also a lowercase `etr` sheet (player/team) |
| projection | `projections.etr` | `etr.etr_pts` |
| ownership | `projections.own` | `projections.own` |

Every field is therefore gathered from whichever sheet holds it and coalesced
with `pick()`, which tolerates either side being absent. Team sheets are "every
sheet that is not `game`, `projections` or `etr`" (case-insensitive) — get that
wrong and `etr` is silently ingested as a team called ETR.

**CFB Showdown** (`CFB-SD`) reuses the same reader via
`read_input_cfb(showdown = TRUE)`. A single-game showdown workbook prices only
the showdown slate, so `salary_util` **is** the flex price (no separate classic
price, unlike MMA's `SDSal`); `salary_cpt` is exactly 1.5× it and `cpt_own` is
carried too. Detection: `{CPT,UTIL}` is shape-identical to CBB Showdown, so
`detect_sport()` returns generic `Showdown` and `sport_key()` upgrades to
`CFB-SD` when `identify_workbook_family()` sees a CFB workbook
(`game`+`projections` sheets) alongside a CPT-slot contest.

### Showdown / captain mode

`MMA-SD` reuses the *same* `Fights` sheet as classic via
`read_input_mma(showdown = TRUE)`, with two deliberate differences: salary
comes from `SDSal` (the Showdown slate is a subset of the card and is priced
separately — 16 of 26 fighters on the 8-22 sheet), and `ProjOwn` is **dropped**,
because `DKOwn` projects classic ownership and would be plain wrong for a
six-from-sixteen captain slate.

**The captain-scaling trap.** DK reports a captain's FPTS already multiplied by
1.5, as a separate `CPT` row in the ownership block — confirmed exactly on
contest 194181193, where every scored fighter's `CPT` row is 1.500x his `F` row
(110.06 → 165.09, 106.86 → 160.29, 100.80 → 151.20). Taking `max(FPTS)` across a
competitor's rows therefore returns the boosted number, and comparing a
captained fighter against a never-captained one picks the wrong winner. So
`dkplayers` also carries **`BaseFPTS`**, the max over non-`CPT` rows, and
`derived_fights()` and the FPTS display column both use it.
`tests_mma_sd_fixture.R` generates a synthetic Showdown export that reproduces
the trap (loser captained, winner never captained) — keep that property if you
rewrite it, because a real export only exposes the bug when some fighter goes
uncaptained across the whole field.

Ownership: DK writes one `CPT` row and one `F` row per fighter, and summing them
gives true any-slot ownership (Wint 45.30 + 7.52 = 52.82, matching DK). The
generic `dkplayers` sum already does this.

Detection order matters here too: `only("CPT", "F")` is tested *before* the
generic `has("CPT")`, since MMA-SD's `F` is the only thing separating it from
CBB Showdown (`CPT`/`UTIL`) and NFL/Soccer Showdown (`CPT`/`FLEX`).

## Live Lineups

Any sport whose adapter yields an **`Opponent` column** switches this tab on —
the logic is head-to-head, not MMA-specific, so tennis would get it free. The
tab itself is `showTab`/`hideTab`'d off `has_h2h()` against the `main_tabs`
`tabsetPanel` id, so it is absent (not just empty) for non-h2h sports.

The old **Slot Breakdown** tab is gone. Per-slot exposure survives only as the
captain-vs-flex view in **My Sweat**: when the adapter's slots include `CPT`,
`exposure()` also computes `Cpt*Exp` / `Flex*Exp`, `display_exposure(slot_split
= TRUE)` renders them, and the `sd_exposure_view` radio toggles back to pooled.
**Combo Analysis** entries are slot-qualified for showdown (`Name (CPT)` /
`Name (FLEX)`), matched against a `combo_sets()` that tags each roster row.

DK never publishes results, so a bout counts as decided once either corner has
non-zero FPTS, and the higher scorer is taken as the winner. A lineup is *alive*
if it holds nobody who lost.

That inference gets cross-checked. For MMA, DK's `TimeRemaining` column is the
count of a lineup's fighters who have **not yet competed** (verified exactly
across 21,709 entries on the 8-22 UFC slate). `live_state()$dk_agrees` compares
`roster - decided` against it per entry. Know what that does and does not
prove: it validates *which bouts are complete*, **not who won** — flipping a
winner leaves it TRUE. Winner inference can genuinely be wrong when a losing
fighter goes the distance against a first-round finish, which is why every bout
has a manual override (`fight_<make.names(bout)>`). On an exact score tie both
corners are marked "Won" so nobody is wrongly killed off.

`fight_status()` must `copy()` the value of `derived_fights()` — `:=` on a
reactive's cached data.table mutates it by reference and overrides would
otherwise accumulate across invalidations.

## Facts that bite

- `pacman::p_load(...)` at the top installs packages at runtime, on whatever
  machine runs the app. Adding to that line changes what gets installed for
  customers. Treat it as deployment config.
- Stack is `dplyr`/`tidyr` **and** `data.table`, plus `stringdist`. Names from
  different sources do not match exactly — that is what `stringdist` is for.
  Do not replace it with exact joins.
- **DK exports are ragged.** The lineup block runs the file's full length while
  the ownership block covers only the first ~100 rows. `fread`'s autodetect
  guesses the wrong width and can stop early mid-file. `read_contest_csv()`
  forces 11 character columns; do not "simplify" it back to a plain `fread`.
- **DK lists a player once per roster slot** in the ownership block (a RB
  appears under both `RB` and `FLEX`). Those rows are summed — one player in any
  slot is one exposure.
- **Locking is per game, not per player.** DK hides a player in *every* lineup,
  including your own, until his game starts; when a whole game is locked DK
  omits those players from the ownership block entirely, so the contest file
  alone cannot say who is left. That is why the input file is used to enumerate
  the slate. All exposure is computed from revealed slots on both sides so the
  comparison stays fair. NASCAR and MMA lock the entire field at once, so they
  are either 0% or 100% locked — there is a guard for the 100% case.
- The CSS is a large inline `tags$style(HTML(...))` block in `app.r`, not
  `www/gts_theme.css`. Check both when changing appearance.
- `options(shiny.maxRequestSize = 100*1024^2)` — contest exports are large by
  design.
- The header references `src = "logo.jpg"` but the file on disk is `logo.JPG`.
  This works on Windows and is inherited from the legacy app; it may be broken
  on the Linux shinyapps.io host. Verify before "fixing" it.

## Testing

`tests_regression.R` drives the real server reactives with
`shiny::testServer()` across every adapted sport and prints a failure count —
run it after any change to the engine:

```
Rscript tests_regression.R
```

It expects the contest exports in `~/Downloads` and the input workbooks in
their `GTS` locations, and it rebuilds two fixtures on the fly: an NFL
mid-contest export (SF@LAC re-locked, to exercise the lock paths once the real
file has gone final) and the MMA-SD captain-scaling fixture from
`tests_mma_sd_fixture.R`.

The invariant it asserts is that the sum of every competitor's exposure equals
roster size × 100% — 600 for a six-slot sport, 900 for NFL Classic. It is only
checked where nothing is locked, since locked slots legitimately reduce it.

`source("app.r")` does not run the app, so the server function can be driven
directly.

## Where this is heading

Sweat is the contest database and the surface for reviewing sim runs against
real results — validation infrastructure rather than a standalone product.
Sports get adapters one at a time as file pairs become available. Simulation
Analysis still needs porting from `app_legacy_mma.R`.
