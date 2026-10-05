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

## NFL QB-routed runs (27 Sep 2026)

`nfl_engine.R` hands a drawn game's kneel-downs and scrambles to the team's
passer (`cf$qb`, who takes the passing line) with their real yardage and TDs;
only designed runs are dealt by `carry_usage` / `sy_share` / `gl_share`. It keys
off `kneel` / `scramble` columns in `slim_<y>_era.rds` (GTS/NFL
`build_templates.R`). Untagged era files deal every run by share, bit-identical
to before. **The sheet contract changed with it:** a QB's `carry_usage` is his
share of DESIGNED runs. An old-contract sheet on tagged data gives the QB his
scrambles twice (~+2.2 carries). Readbacks `car_qr` / `cyds_qr` in
`sim_components`. Record: GTS/NFL/slates/review/2026-09-27_W3_SUN/KNEEL_FIX.md.

## NFL team carries target (1 Oct 2026)

The game tab may carry `car_target_away` / `car_target_home`: a team's ALL runs
(designed + scrambles + kneels, the pool's `carries`), built from the board by
`GTS/NFL/R/team_carries.R`. `nfl_engine.R` delivers it as a pool quantity like
completions (`NFL_POOL_W_CAR`), per side, opt-in, relaxed first on the ESS ladder.
Absent columns = bit-identical output. Why: the pool matched PIT's pass yards,
completions and attempts yet dealt 26.4 carries vs 21-23 real (extra PLAYS), so
Warren ran 58% over his attempt line and Rodgers' TD passes sat low. Caution: the
pool's two sides' carries correlate -0.54 -- a one-side ask moves the other team
the other way; both low on a low total can stall. The balanced matcher ignores it.

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

**Metrics need the same sims AND the same field.** Same sims: scored against
every sim, never the conditioned subset, so `n_sims` from the solver is
deliberately the full count. Same field: Win% / Top n% are **pool-relative**
(`score_all_lineups` ranks each lineup against the others in its own matrix), so
a 300-lineup Lab pool scored alone posts ~14x better rates purely for having
less competition. The Lab is therefore measured against the **main pool as its
field**.

That used to mean scoring both pools together — correct but ~94% waste, since
the main pool was already scored minutes earlier. `field_reference()` now
summarises a scored pool into its per-sim best score and percentile cut-offs
(**782 KB** at 20k sims vs the ~800 MB matrix), stashed as
`rv$dk_field_ref` / `rv$fd_field_ref`; `rates_vs_field()` measures the Lab
against it. Measured at 20k sims: **44.4s → 4.2s, a 10.7x speedup**, with
rho 0.993–0.9997 and 94–100% top-50 overlap against the combined-scoring
answer, the cached side reading +0.08 to +0.26 pts higher exactly as predicted
(its field excludes the Lab's own good lineups). One deliberate difference:
against a cached field two lineups that both beat it are both credited, where
scoring together credits only the single best — the cached statistic is the
more stable one, since it does not move when the user asks for 500 lineups
instead of 300. Falls back to scoring together when there is no cached field
(memory-efficient scoring path, or a stale sim count).

`Top1Count` is re-attached after `calculate_distribution_metrics` (which drops
it) but is **not displayed** — per the table above it is 1–2 for everything, and
showing it invited sorting on noise.

**The picker is a pill board, not a dropdown.** Filter by game chip / position
chip / name, then click a player: once locks (green), twice keeps out (red),
three times clears. Two multi-selectize boxes were the first cut and were the
wrong control — a DFS player picks a game stack or a position group, not a name
from an alphabetical list of 317. State lives in `rv$ll_lock_set` /
`rv$ll_excl_set`; the board caps at 120 pills and says so. The exposure table
additionally carries **LOCK / EXCL** columns that refine the pool **already
built**, with no re-solve (`rv$ll_pool_lock` / `rv$ll_pool_excl`) — the same
two-button idiom as the main exposure table. ★ marks players the solve was
built around, so their 100% rows read as construction rather than a finding.

**Gating.** The tab hides itself unless the sport is `NFL_CLASSIC` *and* that
platform's normal pool already exists, because the Portfolio Builder only
renders a platform tab once `rv$<lp>_optimal_lineups` is set. Lab pools are
cleared by `reset_all_state()` like any other sim-derived state.

`find_optimal_lineups_nfl_classic_locked()` in `OptimalLineups_Core.R` holds the
constraint maths and is commented in full. Other sports need their own slot
bounds before the tab can be offered to them.

## `dcast` must always name `fun.aggregate` (25 Sep 2026)

`dcast(x, Player ~ SimID, value.var = ...)` with no `fun.aggregate` does not
merely warn on a duplicate key — **one** duplicated `Player x SimID` makes it
aggregate with `length()` for **every cell**, so a whole score matrix silently
becomes row counts (10/20/30/40 -> 2/1/1/1). `OptimalLineups_Core.R` already
collapsed duplicates before two of its dcasts ("multiple rows per player per sim
in some configurations"); four other sites did not, and `mma_engine.R` was in
fact producing duplicates — its pad-to-`n_sims` branch copied rows while keeping
their SimID. Cause fixed in the engine (copy whole sims onto fresh ids) and the
four sites guarded: `mma_engine.R` win matrix, `OptimalLineups_Core.R` gate
scoring and the `TotalEW`/`Win6Pct`/`Win5PlusPct` win matrix, and both
`cash_game_module.R` score matrices. `mean` for scores, `max` for the 0/1 `Win`
flag — identities when there is no duplicate, so no other sport's output moves.
`tennis_engine.R:588` and `soccer_engine.R:1316` are still unguarded; they have
not been seen to produce duplicates.

## Cash tab on a classic with no ownership (25 Sep 2026)

Classic sub-slates normally carry no ownership (ETR publishes it for the main
classic only). `prep_pool()` needs positive ownership, so the Cash tab used to
stop with "No players with valid salary and ownership" on every such slate.
`synth_classic_ownership()` (`cash_game_module.R`) now fills it when the slate
has NO positive ownership at all: the showdown field chain applied to a classic
roster (sheet projection, else sim median; value-tilted weight; sums to 100% x
roster slots; capped at `FIELD_MAX_FLEX`), and the field label says
SYNTHESIZED. A sheet with any real ownership takes the old path unchanged.
Every classic path now falls back the same way (see the rulebook below); NBA
keeps its own LP field.

## One lineup rulebook: tournaments and the Cash tab (29 Sep 2026)

`lineup_rules.R` says what makes a DK/FD lineup legal, per sport and format:
roster slots and who may fill them (matched, so NBA "PG/SG" and Soccer "D/M"
work), salary cap (captain at its own salary; FD MVP at
`mvp_salary_multiplier`), and team / game rules (NFL/CFB classic 2 teams +
2 games, NHL 3 teams + 2 games, Soccer <= 5 per team / <= 7 per game /
3 teams, showdown >= 2 teams, F1 one driver at most from the constructor's
team). `lineup_rules()` builds it, `lineup_legal()` checks a matrix of lineups.

Both sides answer to it. The tournament filters in `app.R`
(`drop_invalid_classic`, `drop_single_team_sd`, and NHL's classic filter) are
now thin calls into it. The Cash tab builds its field with
`build_field_tiers_rules()` (every positional classic, F1, Tennis Short Slate,
FD MVP) and runs `field_keep_legal()` over every field, so the log always says
how many field lineups were checked. The optimisers keep their own
built-in rules; this is the check both sides must pass.

Before this, the Cash field had drifted from the tournament rules: CFB, NHL, CBB
and Soccer classic drew fields with no positions (0-QB CFB "lineups"), F1 could
captain a constructor, and `get_player_cols()` dropped Tennis Short Slate's
A-CPT and FD's MVP.

## Worker count and memory limits (20 Sep 2026)

Two hard-coded constants were sized for a big desktop and are now probed.

`.opt_workers()` replaces six copies of `min(detectCores() - 1, 7)`. That
expression **crashed** on a single-core machine (`min(0, 7)` →
`makeCluster(0)`) and when `detectCores()` returns `NA`, which it is documented
to be allowed to do — an error instead of lineups, on every sport. It now
floors at 1 and skips the cluster entirely at one worker, where a one-process
PSOCK cluster pays full serialisation for no parallelism. Identical on 3+ cores.

It is deliberately **not** capped by RAM. That cap was written and reverted:
benchmarked on the 7.6 GB dev machine at 6,000 sims, more workers won cleanly —
7 workers 12.5s, 5 15.4s, 3 16.3s, 1 37.2s.

`.opt_matrix_budget_gb()` replaces `score_all_lineups`' hard-coded 4 GB trigger
for its memory-efficient path with a quarter of actual RAM
(`.opt_total_ram_gb()`, cached once per session). The dev machine has **7.6 GB
total and ~1.5 GB free with a sim loaded**, so a 4 GB trigger never fired while
the app starved around it — R segfaulted reading a 20k sim cache during this
work. The efficient path is **numerically identical** (all five metrics matched
to 1e-10 on a 3,000-lineup / 6,000-sim pool) and ~2.2x slower, so it is a pure
exact memory-for-speed trade. Note `field_reference()` returns NULL on that
path, so the Lab falls back to scoring both pools together.

`.opt_total_ram_gb()` probes PowerShell CIM first: `wmic` is gone on current
Windows 11 and returned nothing, silently reporting the fallback 8 GB for a
7.6 GB machine. Unknown platforms still fall back to 8 GB rather than throttle.

## NHL (`nhl_engine.R` + `nhl/`, 27 Sep 2026) — DK classic + showdown

The input is the nightly workbook from `GTS/NHL/R/live/build_slate.R` (Games /
Players / Goalies / IDs_<dg>); the whole frame (roles, rates, market, each
game's solved grid) is computed there, so the engine only simulates. `nhl/`
holds GTS/NHL's model **copied verbatim** (team_model, player_model, grid /
goalie / player cores, dk_scoring, add_dk_shutout) plus `nhl_params.rds`
(60 KB). Fixes in GTS/NHL do not reach here until re-copied. The model is
sourced into `NHL_ENV`, not globalenv — it defines short helper names (`lin`,
`cmp`, `H`, `STATES`) that would collide with other engines.

Parity: same workbook, same seed, GTS's own `sim_team_box`/`sim_players` vs
`run_nhl_simulation` — bit-identical (358k skater + 20k goalie rows, 27 Sep).
Re-run that check after any re-copy.

- **DK gives each player a separate ID per roster slot**: showdown CPT + FLEX,
  classic skaters position + UTIL. Metadata carries DKID / DKUID and SDID /
  SDCID; `nhl_classic_download()` puts DKUID in the UTIL column.
- Classic: `find_optimal_lineups_nhl_classic()` (NFL classic's method, C 2-3 /
  W 3-4 / D 2-3 / G 1) then DK's 3-team / 2-game rule. A MAIN / LATE pill row
  re-points the classic ids without re-simming.
- Showdown: `enum_captain` with `.band_cut = TRUE` on the first pass — a
  38-man game puts 1.2M rosters in the default band (128s at 1k sims); the top
  100k by salary is ~$49k+ (29s).
- Starting goalies only (DFO's starter), scored off the box's starter line. No
  ownership source yet, so no AvgOwn / leverage.
- Tournament Lineups: one SLATE pill row (Main / Late classic, each game's
  showdown) and one Score DraftKings button, as on NFL (`rv$nhl_slate`).
- Sim Results is a validation tab. `nhl_sim_visuals()` summarises the box
  scores into games vs market, goalies, skaters vs the Pinnacle SOG line
  (`sog_line` / `sog_p_over` on Players, written by build_slate.R from 28 Sep;
  older sheets show no prop check), DK share by line / PP unit, DK-point
  correlation by relation + per-game heatmap, and score ranges. `SheetDK` is
  the builder's smoke-sim mean -- a parity check on the app.

## Running it

A working launch config lives at `.claude/launch.json` in the repo root (name:
`simapp`) — it starts SimApp on port 7788. Real input sheets for testing are in
`Documents\GTS\<Sport>\`, with 72 older NASCAR ones in
`Documents\GTS\Nascar\InputFiles\archive\`.

## Golf engine v2 (`golf_engine_v2.R`, 30 Sep 2026) — round-score sim

Design record: `GTS/Golf/ENGINE.md`. `golf_engine.R` keeps v1's reader and
helpers, reads the optional `Event` tab (Par, Level, LevelSD, CutN, CutAfter),
and sources `golf_engine_v2.R` at its end, which **redefines
`run_golf_simulation()`**. v1 survives only as `run_golf_simulation_v1()` for
the P4 bench — Andrew's call: v2 is the only engine customers run.

Skill (one number per golfer) is fitted so the sim reproduces the sheet's
W/T5..T40/Cut ladder (common random numbers, 25 probit steps, centred on the
field); rounds are simulated with shared conditions, waves from tee times and
skewed personal noise; each round's DK/FD points are a real round drawn at the
same score to par (`golf/round_pool.rds`, `golf/noise_q.rds`, ~100 KB).
Tie for 1st = playoff.

**Field, Level spread and cut (5 Oct 2026, branch golf-engine-v2-review).**
The field is the sheet's golfers: Event `FieldSize` is ignored and there are no
unnamed fillers. `LevelSD` on the Event tab sets the sd of the event-level shock
(BuildSheet writes sqrt(0.308 + 0.487 / editions) from course history, 1.90 for
comps; 0.87 if absent). The cut comes **only** from the Event tab's CutN /
CutAfter, which `BuildSheet_Golf.R`'s `golf_cut_rule()` writes per event (majors,
54-hole pro-ams, no-cut events, else 65 above 100 golfers, 50 at 100 or fewer).
The app has no cut controls; it shows the rule with "(from the sheet)". An older
sheet without the Event tab or a cut cell falls back to top 65 & ties after R2
with more than 100 golfers, else top 50 (`golf_v2_cut_fallback()`), and the app
shows an amber note under the bar, in the sim status strip and as a warning
toast. The fallback does not know majors: an old US Open sheet gets 65, not 60.
`sim_results` carries `MadeCut` (1 = played the weekend). `CutProb` is the sim's own cut rate.
`keep_rounds = TRUE` adds `round_results` (for showdown, P5) — off in the app,
it is ~12M rows at 25k sims.

Bank of Utah 2026, 10k sims: ladder RMS gap W .001 → T40 .024, cut .014;
golfers tied at one finish differ by 4.95 DK (real 4.9; v1 0); 17 s.

**Lineup pool (P2, 30 Sep 2026).** `generate_golf_candidate_pool()` pools every
sim's exact optimal 6 (NASCAR's knapsack DP, `find_optimal_lineups_combinatorial`);
cut and no-cut alike, POOL = Y restricts the solve. Every sim's optimum is
distinct, so above `max_lineups` (10,000) it keeps the best by top-5% rate
(`ps_top_frac`, as NHL/NFL classic). Cut metrics are columns, not the selection
rule. Golf scores in 2,000-sim batches (`sims_per_batch`) to hold memory at 25k.
