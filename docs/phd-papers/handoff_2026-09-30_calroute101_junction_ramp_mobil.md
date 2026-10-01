# Handoff — 2026-09-30 — CalRoute101_3: junction holds, ramp drawing, MOBIL

Continues `handoff_2026-09-28_calroute101_micro_mixed_meso.md` (read that first for the three models,
eval mains, output folders and reports). Paths are under `src/main/scala/scalation/simulation/process/`.

## 1. Lane 2 problem — SOLVED (cause: 1 s junction hold)

- Every `Junction` defaulted to `Sharp(1.0)` (`Junction.scala:59-60`). A micro car jumping a sensor stays in
  the lane DLL for that 1 s, so the follower stops and restarts → ~1,516 veh/h per lane (2.37 s/car).
- Lane 2 is the only lane whose PeMS demand exceeds that (peak 2,168 veh/h, 35 of 48 bins) → queue from seg 0
  to sensor 4, ~2,000 cars piled at the seg 0 entrance (density 2,111 veh/km), flow R² 0.04 → −0.70.
- Not the data (PeMS consistent at all 5 detectors), not the ramp (joins lane 4 at segs 2/5), not lane
  length/setup (all lanes identical), not lane changes (lane totals unchanged S3→S4).
- **Proven by test:** sensors at `Sharp(0.0)` → lane 2 R² 0.86, Sim mean = PeMS mean, micro fitness
  36.54 → 8.15, travel time 1,135 → 150 s, run 681 → 61 s.

**Current junction rule (approved "option A"), same in all three model files:**
```scala
    // Mainline junctions hold 0 s; ramp merges keep the default 1 s until gap acceptance exists.
    for i <- junc.indices do
        if juncNames(i).startsWith ("onR_merge") then
            junc(i) = new Junction (juncNames(i), xy = aniCoords_Main(i), nt = nt, nl = numLanes)
        else
            junc(i) = new Junction (juncNames(i), Sharp (0.0), aniCoords_Main(i), nt, numLanes)
        end if
```
warm_up + sensors 0 s; onR_merge1/2 and ramp sensors keep the 1 s default. `Junction` class unchanged.
The ramp 1 s is a placeholder — the merge is a blind insert (no gap acceptance).

## 2. Results (runs 2026-09-29, mainline 0 s, merges 1 s, ramp length 100)

| | Micro | Mixed | Full meso |
|---|---|---|---|
| Mean travel time | 148.94 s | 109.55 s | 88.01 s |
| Avg flow NRMSE×100 | 2.09 | 2.11 | 2.13 |
| Avg speed NRMSE×100 | 9.67 | 22.95 | 40.52 |
| Fitness | 8.15 | 18.78 | 32.84 |

Run folders: `log/simulation/{micro,mixed,meso}/` — loose files = these runs;
`baseline/` = original 1 s everywhere; `detector_fix/` = sensors 0 s only.
`SideBySideTable.scala` (`flowMesoVsMicro`) points at the loose runs.

Meso and mixed are identical at sensor 1 (seg 0 is the same meso link) — verified 192/192 cells; they differ
from S2 on, which is why fitness differs.

**Remaining gap = meso speed.** Mixed speed R² is negative exactly at S1/S3 (after its meso links); full meso
negative everywhere. Cause: micro cars get the PeMS speed as `vmax` (`VSource.scala:158-159`), meso ignores it
(`IdmFlux` v0 fixed 33.528 m/s, capacity ~2,615 veh/h/lane > demand → ~32 m/s all day). Open choice:
- A: cap meso speed at `actor.vmax` in `MesoVTransport.move` (both models use PeMS speed as input), or
- B: stop feeding PeMS speed to micro too (speeds become purely model-produced; cleaner experiment).

## 3. Ramp drawing — FIXED (`config/NetworkConfig.scala` only)

- Bug: `getRampCoordinates` projected the 2 ramp GPS points on their own (scale 4.47 vs mainline 1.34 px/m),
  throwing the ramps to the canvas corners.
- Now: each ramp ends exactly on lane 4 (outer right lane) at its merge junction (Route's lane shift for
  lane numLanes-1), and starts `RAMP_LEN` away, back along the road and 30° outward (road direction taken at
  each ramp's own merge). New private helpers `LANE_GAP`, `RAMP_LEN`, `RAMP_ANGLE`, `unit`.
- **`RAMP_LEN = 200.0`** (doubled after the runs in §2). The drawn length IS the ramp VTransport length, so
  results shift slightly → **all three evals need a rerun** (not yet done).

## 4. MOBIL — implemented in `Dynamics.scala`, NOT wired (called by nobody)

- `trait Dynamics.laneChange (car, route, politeness = 0.5, threshold = 0.2, bSafe = 4.0): Int` → −1/0/+1.
  Full Treiber–Kesting rule (safety on new follower; incentive incl. old follower). Decision only.
- Private `accelFor` dispatches like `updateV` to `accelOf` in `IDMDynamics` (reuses `iDM`/`iDMFree`),
  `GippsDynamics` ((gipps − v)/rt), `KraussDynamics` ((krauss − v)/rt; draws from its noise RV).
- Neighbours from the lane DLL / `corridorPos` (`leaderAt`, `follower`, `getFirst`). No abstract members
  (Vehicle mixes in Dynamics). Call on the model object: `motion.laneChange (car, route)`.
- Old MOBIL no longer exists in the repo; Korede's pasted copy was outdated (missing Vehicle props,
  `route.laneExistsAt`, swapped leader/follower from segment deques, segment-local gaps, duplicate IDM).
- Today's only active lane change: once per segment, if leader < 0.1·vmax, try one lane over with a 20 m
  gap check (`CalRoute101_3:233`, `withMeso:265`). Real lane 2 gains ~28 veh/15 min S1→S5; model gains none.

## Found, not fixed (report separately)

- `Route.changeLane`: when nobody is ahead in the target lane, uses `toPath.getLast` (furthest-back car) as the
  new follower; should be `getFirst` (front car).
- Segment lengths are screen pixels, not metres (model 2,886 vs real 2,150 m; seg 0 1,007 vs 750 m) —
  `VTransport.length = curve.length`, no `segmentLengths` passed. Affects travel time/density, not flow fit.
- Eval never exits (skipped VSources parked; `Coroutine.shutdown` joins forever) — stop after "fitness value".
- Only one PeMS day (11/4/2025) exists for Donald Doyle.

## Open decisions / next steps

1. Rerun `eval`, `evalWithMeso`, `evalFullMeso` (ramp length 200).
2. Meso speed: option A (cap at `actor.vmax`) or B (no PeMS speed input anywhere).
3. Wire MOBIL repo-wide: decision `motion.laneChange`, move `Route.changeLane` (one safety rule), one
   engine-level check point (rate-limited / event-driven), HUD lane-change counter on the `Route.changeLane`
   success branch (`Route`, `Model.pushHudStats`, `DgAnimator`). Foundational → design review first.
4. Forecast validation: get multiple PeMS days; drive with forecast demand (ScalaTion
   `modeling/forecasting`: seasonal naive baseline, SARIMA, exp. smoothing) via `RowTimeLoader`; validate on
   D+2 with a second `PeMSDemand`; report forecast error and model error separately.
5. Restore the commented `autoRun` blocks when eval work is done; later rename models.
