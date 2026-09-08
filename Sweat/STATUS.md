# Sweat — where things stand

Last updated 2026-09-07. Companion to `CLAUDE.md`, which carries the durable
architecture notes. This file is the shorter question: what changed, what is
proven, what is queued.

## What happened

`Sweat/app.r` was replaced. It used to be a 5,000-line MMA-only tool; it is now
a ~2,100-line multi-sport engine plus one adapter per sport.

The old app is preserved verbatim as `app_legacy_mma.R` and is **not** dead
weight — it still holds the Simulation Analysis tab, which has not been ported.

Order of work, for context on why the code looks the way it does:

1. Rebuilt the NFL sweat tool (`nflsweat/`) around a generic slot parser.
2. Generalised that engine into `Sweat/app.r` as the main app, with a `SPORTS`
   adapter registry, and folded NFL in.
3. Added NASCAR, then MMA classic, then MMA Showdown, then CFB.
4. Added the Live Lineups tab (replacing the legacy Live Sweat).

## Status

| | State |
| --- | --- |
| Location | Moved into the **`GTS` repo** (`GTS/Sweat/`), Sept 2026, from `NicheSportSimsPublic/Sweat`. |
| Customer launch | `GTSSweatLaunch.R` → `runGitHub(repo="GTS", subdir="Sweat", ref="main")`, same pattern as SimApp. `main` is production. |
| Committed | Pending — the move + the CFB-Showdown / UI wave land in one commit. |
| shinyapps.io | Retired. `runGitHub` off `main` is the only delivery path now. |
| Regression suite | `Rscript tests_regression.R` → **0 failures**, 10 cases (+ CFB Showdown, per-slot exposure, slot combo, tab-visibility checks). |

## Sports

| Sport | Detection | Metadata | Verified against |
| --- | --- | --- | --- |
| NASCAR | `only("D")` | org, car, salary, starting spot, DKOP proj. own | contest 194007416 + `TrucksNH.xlsx` (36/38) |
| NFL Classic | `QB`/`DST` present | pos, team, game, depth slot, ETR proj | contest 193845373 + preseason wk2 (122/122) |
| CFB | `S-FLEX` present | pos, team, game, fav/dog, salary, ETR, own | contest 194610521 + `2026-09-03_CFB_THU.xlsx` (72/72) |
| MMA | `only("F")` | opponent, weight class, salary, ML, de-vigged win prob, DKOwn | contest 193845215 + UFC 8-22 (26/26) |
| MMA Showdown | `only("CPT","F")` | as MMA, but `SDSal`; proj. own deliberately dropped | contest 194181193 + UFC 8-22 (16/16) |
| CFB Showdown | `{CPT,UTIL}` + CFB workbook | as CFB; `salary_util` is the showdown flex price, `salary_cpt` / `cpt_own` also carried | contest 195045051 + `2026-09-07_FSU_SMU.xlsx` (46/46) |

Everything else (F1, NBA, CBB, Tennis, Golf, generic Showdown) parses and gives
exposure / slots / combos / dupes, but has **no** `read_input`, so no metadata
breakdowns. Adding one is a single `SPORTS` entry plus a reader.

`{CPT,UTIL}` is shape-ambiguous (CFB-SD vs CBB-SD), so `detect_sport()` alone
returns generic `Showdown`. `sport_key()` then upgrades it: when an input
workbook is supplied, `identify_workbook_family()` names the sport from the
sheet signature (`game`+`projections` ⇒ CFB, `Fights` ⇒ MMA, …) and the
contest's slot set picks classic vs showdown. Manual override in the Sport
dropdown (`CFB-SD` etc.) still wins.

## Tabs

My Sweat · Live Lineups · Breakdown · Lock Status · Combo Analysis · Dupe Analysis

The two doing real work beyond the old app:

- **Breakdown** — per-sport group dimensions from that sport's own input sheet.
  This is the "reframe each sport to its own sheet" idea; `group_dims` in the
  adapter is the whole mechanism.
- **Live Lineups** — filter to lineups with no losers. Switched on by an
  `Opponent` column, so it is head-to-head generally, not MMA-specific. The tab
  is now `showTab`/`hideTab`'d off `has_h2h()` (id `main_tabs`), so it is gone
  entirely for sports that cannot use it rather than rendering an empty state.

**Slot Breakdown** was deleted. Its per-slot count (a player counted only in
the slot he occupies) lived on only for captain-vs-flex, which is now folded
into **My Sweat**: for showdown formats the exposure table defaults to
`Your/Field CPT %` and `Your/Field FLEX %` columns, with a radio to fall back
to the pooled any-slot number. **Combo Analysis** is likewise slot-qualified
for showdown — entries read `Name (CPT)` / `Name (FLEX)` and the hit test
matches slot + player.

**Favourite / Dog** (`Role`) is now an **MMA-classic-only** group dimension —
removed from CFB and MMA Showdown, and no longer computed in
`read_input_cfb()`.

## Things that were genuinely hard, so don't undo them casually

Each of these is a bug that was found and fixed with real data. `CLAUDE.md` has
the full reasoning; this is the index.

- **Ragged DK exports** — `read_contest_csv()` forces 11 character columns.
  Plain `fread` mis-detects the width and stops early mid-file.
- **One player, many roster rows** — DK lists a RB under both `RB` and `FLEX`.
  Summed, so any slot is one exposure.
- **Locking is per game** — and a fully locked game is omitted from the
  ownership block entirely, so the contest file cannot enumerate the slate. The
  input file does. All exposure is computed from revealed slots on both sides.
- **Captain scaling** — DK pre-multiplies a captain's FPTS by 1.5 (confirmed
  exactly, 1.500x, on contest 194181193). `BaseFPTS` exists so head-to-head win
  inference never compares a boosted score against an unboosted one.
- **Fuzzy name matching** — cutoff is 0.05, and the constant's comment carries
  the measurement. Real variants land ≤0.021; wrong-slate garbage lands ≥0.093.
  Do not add heuristics on top without re-measuring.
- **CFB has two live workbook layouts** and the adapter must keep reading both.

## Queued / open

1. **Port Simulation Analysis** from `app_legacy_mma.R`. Explicitly deferred.
   Needs a sim-optimals file to test against.
2. **More sports** — one at a time, each needs a matching contest export + input
   workbook pair. F1 and CBB are the obvious next ones.
3. **`nflsweat/`** at the repo root is now fully subsumed and redundant. Left on
   disk deliberately (untracked, nobody else has it) — delete when ready.
4. **Logo casing** — the header asks for `logo.jpg`, the file is `logo.JPG`.
   Inherited from the legacy app, works on Windows, may be broken on the Linux
   shinyapps.io host. Check before "fixing".
5. **Launcher label** — `LaunchScriptv2.R:315` calls slot 7 "NASCAR Live Sweat
   Tool". It was wrong for the MMA app and is only accidentally half-right now.

## Running it

```
Rscript -e "shiny::runApp('.', port=7799, launch.browser=TRUE)"
```

Upload a DK contest CSV plus that sport's input workbook; leave Sport on
Auto-detect. The note under the upload row reports the match rate — if it turns
orange saying to check the pairing, the input file is for a different slate.
That warning has already caught two genuinely mismatched pairs.
