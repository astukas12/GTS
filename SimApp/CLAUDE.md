# SimApp

The simulator. One Shiny dashboard driving 10 sport engines.

## Startup contract

`app.R` sources, in this order:

1. `sport_configs_universal.R` — `SPORT_CONFIGS`, the driver for everything
2. `OptimalLineups_Core.R`
3. `portfolio_helpers_universal.R`
4. `cash_game_module.R`
5. All 10 engines, in a `local({...})` loop at the top of `app.R`

**Engines are sourced once at startup and never re-sourced inside a reactive
observer.** Re-sourcing re-executes every top-level statement in the engine on
each upload or sim run. The rule is already written at `app.R:16` — keep it
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

## Running it

A working launch config lives at `TheLab/.claude/launch.json` (name: `simapp`)
— it starts SimApp on port 7788. Real input sheets for testing are in
`Documents\GTS\<Sport>\` and `TheLab/InputFiles/`.
