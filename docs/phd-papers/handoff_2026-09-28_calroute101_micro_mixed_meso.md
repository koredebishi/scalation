# Handoff — 2026-09-28 — CalRoute101_3: micro vs mixed vs full meso

Session 2026-09-24 → 09-28. Code is Korede-compiled and Korede-run unless noted.
All paths below are under `src/main/scala/scalation/simulation/process/` unless they start with `log/` or `data/`.

## Where we are

Three versions of the same US-101 Donald Doyle model now exist and run through one evaluation:

| Model | File | Eval main | Output folder |
|---|---|---|---|
| Micro | `model/calroute101/CalRoute101_3.scala` | `eval` | `log/simulation/micro/` |
| Mixed (segs 0, 3, 4 meso) | `model/calroute101/CalRoute101_3_withMeso.scala` | `evalWithMeso` | `log/simulation/mixed/` |
| Full meso (all segs + ramps meso) | `model/calroute101/CalRoute101_3_fullMeso.scala` | `evalFullMeso` | `log/simulation/meso/` |

Run: `sbt "runMain scalation.simulation.process.model.calroute101.<main>"`, one at a time.
The run never exits by itself (see "Known issues"); stop it once "The fitness value is" prints.

Plots: `model/calroute101/SideBySideTable.scala` → `@main flowMesoVsMicro` (sensor 1, per lane, PeMS vs models;
ScalaTion-native `MatrixD.load` + `?` slicing + `PlotM`, zero baseline row so the y-axis starts at 0).

## Latest results (2026-09-28 runs, after the meso speed fix)

| | Micro | Mixed | Full meso |
|---|---|---|---|
| Wall time (run) | 681.2 s | 86.1 s | 5.9 s |
| Mean travel time | 1134.87 s | 216.27 s | 93.86 s |
| Throughput | 4466.84 veh/h | 4965.47 veh/h | 4968.43 veh/h |
| Avg flow NRMSE×100 | 3.4735 | 3.1435 | 2.1253 |
| Avg speed NRMSE×100 | 32.6337 | 40.1981 | 40.5285 |
| Fitness | 36.5365 | 39.2221 | 32.8478 |

Same arrivals in all three (59,750 cars, Erlang2S, identical streams). Micro is deterministic run to run.

## What changed this session (all approved)

- **Mixed model = micro except the meso swap.** Removed a double `junc(seg).jump()` at meso→micro re-entry
  (double-counted sensor1, added a jump at onR_merge2).
- **Density at sensors after meso links:** `Junction.jump (linkDensity: Option [Double] = None)`; the mixed/meso
  models pass `Some (link.density)` (whole-link density, same definition as `VTransport.density`).
- **Meso speed fix:** `MesoVTransport.move` now sets `actor.velocity = length / linkTime` at release.
  Before, sensors after meso links recorded the car's creation speed (the PeMS input) — fake speed accuracy.
- **Full meso model** (new). `RampMeso` got an optional `flowDiagram` so ramps use the same `IdmFlux`.
- **Calibration adapter:** `CalRouteReadings` trait (4 getters) + `enum CalRouteVariant { Micro, Mixed, FullMeso }`;
  `CalibrateCalRoute101_3 (variant = …)`; `evalRun (variant)` times setup/run with ScalaTion `gauge`.
- **Reports:** summary is now a `RUN SUMMARY` + `SINKS` table (with wall time and flow/speed NRMSE, fitness);
  validation report shows **PeMS mean and Sim mean** side by side in MACRO and MICRO tables;
  `exportRun (…, folder)` writes each model into its own folder.
- The `autoRun` block (nanoTime wall print + `Model.shutdown`) is **commented out** in all three model files, so the
  standalone `runCalRoute101_3*` mains only build the model. Restore after the eval work.
- CLAUDE.md rewritten short (rules kept, AGENTS.md's newer rules merged). Stale memories deleted.
- PeMS: the Redwood Creek folder was renamed `data/Mainline_VDS_Redwood_Creek_US101-N-OLD-NOTINUSEROAD/` (ramps moved
  inside). CalRoute101_3 uses only `data/Mainline_VDS_Donald_Doyle/` and `data/Ramps_VDS_Donald_Doyle/`.

## Findings (from the run data)

1. **Micro lane 2 at sensor 1 is the one bad fit** (R² 0.04, Sim mean 369.17 vs PeMS 410.71). Not the ramp
   (ramps join at segs 2 and 5, lane 4). Cause: the **1 s sensor jump** (`jTime = Sharp(1.0)`) holds each car at the
   segment end while it is still in the lane DLL, so the follower stops and restarts → about **1,500 veh/h per lane**
   (peaks ~1,680). Lane 2 PeMS demand is above that in 35 of 48 intervals (peak 2,168) → queue in seg 0 all day
   (~1,000 s of the micro travel time). Lanes 1, 3, 4 stay under it. Meso/mixed don't have it (seg 0 is meso;
   a car jumping after a meso link is in no DLL).
2. **Meso never congests.** `IdmFlux` from the fixed `Vehicle.vmax` (33.528 m/s) gives ~2,615 veh/h/lane capacity;
   full meso records ~32 m/s at every sensor while PeMS averages ~24 m/s.
3. **Sensor 1 is also the demand anchor** (`1-401112ML.csv` = demand, starting speed, and sensor-1 scoring). Where a
   lane does not congest, sensor 1 flow just echoes the input; flow means match by construction.
4. Mixed/meso travel times differ from micro because of findings 1 and 2, not a reporting error.

## Known issues (reported, not fixed)

- **Eval never exits:** after the 12 h stop time `Model.scala:380` skips (dequeues) two `VSource`s without resuming
  them; `Coroutine.shutdown` (`Coroutine.scala:208`) joins every virtual thread and waits forever. All files are
  written before this. Fix would be in `Model` or `Coroutine` (foundational — needs review).
- Two leftover `java.*` uses could become plain Scala: `System.exit` → `sys.exit` (`CalibrationFramework.scala`),
  `System.getProperty` → `sys.props` (`SimulationReport.scala`). Timestamps / host name / run UUID have no
  ScalaTion or Scala equivalent.
- `Dynamics.easyW` / `VTransport.easyW` log files are never flushed (always empty).

## Open decisions for Korede

1. **Sensor jump in micro:** keep the 1 s hold, set sensor `jTime` to 0, or take the car out of the lane DLL while
   it jumps. (Jump is meant as data collection + future signal/control point.) Changes micro results.
2. **Meso capacity/speed:** meso links use the fixed 33.528 m/s `IdmFlux`; micro cars use per-bin PeMS `vmax`.
3. Restore the commented `autoRun` blocks when eval work is done.
4. Later: rename to `CalRoute101_Micro`, `CalRoute101_Meso`, `CalRoute101_Meso_Micro_Mixed`.

## Next step when resuming

Pick decision 1 (sensor jump). I will write the design review (files: `Junction`, possibly `VTransport`/models)
before any edit. Then rerun `eval` and compare lane 2 at sensor 1 again with `flowMesoVsMicro`.
