# SimApp

The simulator. One Shiny dashboard driving 10 sport engines.

## Startup contract

`app.R` sources, in this order:

1. `sport_configs_universal.R` — `SPORT_CONFIGS`, the driver for everything
2. `OptimalLineups_Core.R`
3. `portfolio_helpers_universal.R`
4. `cash_game_module.R`
5. `lineup_lab_module.R`
6. All 10 engines, in a `local({...})` loop at the top of `app.R`

**Engines are sourced once at startup and never re-sourced inside a reactive
observer.** Re-sourcing re-executes every top-level statement in the engine on
each upload or sim run. The rule is already written at `app.R:17` — keep it
true.

`contest_manager_module.R` exists but is not sourced by `app.R`. Check whether
it is wired in before assuming it runs.

## Structure of app.R

~4,940 lines, three sections:

| Lines | Section |
| --- | --- |
| 29–80 | Helpers, defined outside `server` so they exist at parse time |
| 83–532 | `ui` — a `dashboardPage` |
| 535–4936 | `server` |

Grep for what you need and read that range. Reading the whole file costs a
large fraction of a context window and is almost never necessary.

## SPORT_CONFIGS

`sport_configs_universal.R` (~1,270 lines) defines 11 entries: NASCAR, MMA,
TENNIS, NFL, GOLF, F1, CBB, NFL_PRESEASON, NFL_PRESEASON_CLASSIC, NBA, SOCCER.
There is no WNBA entry.

Adding a sport should mean adding a config entry, not adding a branch in
`app.R`. Public helpers: `detect_sport()`, `get_sport_config()`,
`get_all_metrics()`, `get_platform_config()`, `validate_simulation_output()`.

## The reader_map quirk

`load_sport_input()` (`app.R:36`) keeps a `reader_map` that routes five sports
to dedicated readers — GOLF, F1, CBB, NBA, SOCCER — while everything else goes
through the generic `config$input_file` path.

The comment above it says three sports. It is stale; the map has five. Trust
the code.

This split is the main thing standardizing the engine interface (project S1)
has to resolve. Anything touching input loading will hit it.

## Engines

`nascar` `mma` `tennis` `golf` `f1` `nfl` `nfl_preseason` `cbb` `nba` `soccer`

They have drifted — each implements its own de facto contract. There is no
single documented interface yet.

Note `nfl_preseason_engine.R` here duplicates `nfl_engine.R` in the Preseason
fork under `Documents\GTS\NFL\Preseason\`. Fixes to one do not reach the other.

## Showdown captain/MVP ownership (`nfl_engine.R` + Portfolio Builder)

The `projections` tab may carry `cptown` (DK Captain-slot ownership) and
`mvpown` (FD MVP-slot ownership) alongside `dkown` / `fdown`. `nfl_engine.R`
reads them into `meta$CPTOwn` / `meta$MVPOwn` (both default 0). When present and
non-zero, the Portfolio Builder splits the exposure tables into
`CptOwn/CptLev · UtlOwn/UtlLev · TotOwn/TotLev` (the same view CFB and NBA
showdown use); absent, it keeps the flat `OwnProj`/`Leverage` view. On the FD
tab the "Cpt*" columns are the MVP slot. Gate in `app.R` is `has_nfl_cptown`
inside `make_filtered_exposure` / `make_portfolio_exposure`.

## Showdown optimizers guarantee ≥ 2 teams

`find_optimal_lineups_combinatorial_captain` / `_combinatorial_mvp`
(`OptimalLineups_Core.R`) drop any single-team roster before ranking — a 6-0
lineup is an invalid DK/FD upload. No-op when the engine emits no `Team` column
or the slate is one team. The LP modes (`find_optimal_lineups_captain` / `_mvp`,
used by MMA/NBA/tennis SD) do not yet enforce this; the download-layer
`drop_single_team_sd` still backs them up.

## Lineup Lab (`lineup_lab_module.R`, 20 Sep 2026) — NFL classic only

A sixth tab that re-solves a **finished** sim under a user lock. Pick players to
force into every lineup, say how well that set has to have done for a sim to
count, get a small scored pool, filter it, and add a random draw of it to the
same portfolio the normal process feeds. Nothing is re-simulated.

**Why it exists.** Control: a pool built around players the user picked, with
the rest of the roster filled by the sim rather than by a stacking rule. The
main pool is capped at 5,000 lineups out of a far larger space, so a given
player can be thin in it or present only in rosters built for another script.

**It does not fix "lineups don't separate" — that was tested and it is false.**
Measured on the live 2026-09-20 W2 Sunday sheet at 20,000 sims:

| lock | cond | sims | distinct | max repeats | % >1 win |
| --- | --- | --- | --- | --- | --- |
| (none) | 100% | 20,000 | 34,217 | 1 | 0.00% |
| QB | 100% | 20,000 | 19,999 | 2 | 0.01% |
| QB + team WR | 100% | 20,000 | 19,999 | 2 | 0.01% |
| QB + WR + bring-back | 100% | 20,000 | 19,988 | 2 | 0.06% |
| QB + WR + bring-back | 25% | 5,000 | 4,999 | 2 | 0.02% |

Six free slots across ~300 priced players is still vastly more rosters than
there are sims, so one sim still yields one distinct optimum no matter how tight
the lock. **`Top1Count` cannot rank a big classic.** The columns that do
separate are `WinRate` / `Top1Pct` / `Top5Pct`, which come from scoring every
pool lineup against every sim — already computed, on both pools.

**Two knobs, different jobs.**

- `lock_players` — feasibility. Forces those players in.
- `cond_frac` — relevance. Keeps only the top fraction of sims ranked by the
  **mean percentile rank** of the locked players within their own distributions
  (scale-free, so a DST and a QB combine sensibly). The other slots are then
  chosen in the world where the lock hit. This is what makes the stack emerge
  from the sim's own correlations rather than from a stacking rule.

**The forcing mechanism** is a reduced problem, not a new solver and not
post-filtering: locked players are removed from the candidate pool, and cap,
roster size and per-position bounds are all decremented by what they occupy.
`.classic_exact_chunk` solves that smaller instance exactly and the locked
players are added back. Two edge cases the unconstrained path never reached had
to be fixed in that solver — `k == 0` (a position with no slots left) and
`hi[P] == 0` in the dominance prune. Both are inert for existing callers, whose
minimums are all >= 1; verified by re-running the unconstrained NFL classic
solve and getting a bit-identical 1,821-lineup pool.

**Metrics are scored against every sim, not the conditioned subset**, so Win% /
Top1% on a Lab lineup mean exactly what they mean on the main pool. `n_sims`
returned by the solver is deliberately the full count. `Top1Count` is
re-attached after `calculate_distribution_metrics` (which drops it) and shown as
`Repeats`, but per the table above it is near-constant at 1–2 and should not be
ranked on; it is there so the degeneracy is visible rather than hidden.

**Gating.** The tab hides itself unless the sport is `NFL_CLASSIC` *and* that
platform's normal pool already exists, because the Portfolio Builder only
renders a platform tab once `rv$<lp>_optimal_lineups` is set. Lab pools are
cleared by `reset_all_state()` like any other sim-derived state.

`find_optimal_lineups_nfl_classic_locked()` in `OptimalLineups_Core.R` holds the
constraint maths and is commented in full. Other sports need their own slot
bounds before the tab can be offered to them.

## Running it

A working launch config lives at `TheLab/.claude/launch.json` (name: `simapp`)
— it starts SimApp on port 7788. Real input sheets for testing are in
`Documents\GTS\<Sport>\` and `TheLab/InputFiles/`.
