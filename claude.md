# ScalaTion Traffic Simulation

Microscopic traffic simulation using ScalaTion 2.0 (Scala 3).  
US-101 corridor with IDM/Gipps/Krauss car-following dynamics.

**IDE:** IntelliJ IDEA  
**HPC:** Sapelo2 (krb84578@uga.edu)

## Core Rules
- **Always ask permission before writing or modifying code**
- **Do not plagiarize** - all paper text must be original
- **Do not hallucinate** - if unsure, say so
- **ScalaTion-native first** — use ScalaTion's own RNG/math (e.g. `Discrete`, `VectorD`) before `java.util.Random` or Scala functional chains
- **Imperative style** — prefer `cfor`, indexed loops, direct assignment over `.map().toArray`, `:+`, functional chains
- **Scaladoc with examples** — every public method's Scaladoc must include a concrete example with numbers showing what goes in and what comes out (see `OffRampSpec.buildExitDist` / `assignExit` in `Route.scala` as the gold standard)

## Chat Continuation Protocol

When the context window is nearly full (~80%), **before the conversation is cut off**:

1. Write a `## Session State` section at the bottom of this file with:
   - **Date** of session
   - **What was completed** (list of changes, files touched)
   - **What was in progress** (unfinished task + exact next step)
   - **Known bugs/issues** found but not yet fixed
   - **Key decisions made** (design choices, rejected alternatives)
2. Tell the user: *"Context is nearly full. I've saved session state to `CLAUDE.md`. Paste this into your next chat to continue."*

This ensures zero loss of continuity between chat sessions.

## Context Files

| Need | File |
|------|------|
| Behavioral guidelines | `context/rules.md` |
| Coding style & key files | `context/scalation-style.md` |
| IDM params & domain knowledge | `context/traffic-simulation.md` |
| HPC Sapelo2 commands | `context/hpc.md` |
| Variable lane count problem | `context/variable-lane-architecture.md` |
| Ramp physics + density lane assignment | `context/ramp-physics-and-density-lane-assignment.md` |
| Off-ramp exit design | `context/offramp-exit-design.md` |
| Model template (standard) | `context/model-template.md` |
| OSM road geometry plan | `context/osm-road-geometry-plan.md` |

## Papers

| Paper | Status | File |
|-------|--------|------|
| ANNSIM 2026 | ✅ Submitted | `context/papers/annsim-2026.md` |
| WSC 2026 — Wildfire/Contraflow | ❌ Deadline missed | `context/papers/wsc-2026.md` |

## Dissertation Studies

| Study | Title | Status |
|-------|-------|--------|
| Study 1 | Structural Sensitivity Analysis (integrators, arrivals) | ✅ ANNSIM 2026 Submitted |
| Study 2 | Wildfire Evacuation & Contraflow on I-10 (Palisades Fire) | ❌ WSC 2026 deadline missed — retarget to next venue |
| Study 3 | Unified Agentic Architecture (long-term, internal) | 🔄 Internal Vision — Not for committee or PI yet |

**NOTE: Calibration is a supporting result, not a standalone paper. PI is not interested in calibration as a paper.**

## Active Focus
**Simulation engine development** — rendering, ramp physics, lane change, calibration.  
Next paper target TBD.

## Quick Reference

### Run Simulation
```bash
# US-101 (CalRoute101_3)
sbt "runMain scalation.simulation.process.model.runCalRoute101_3"

# Eaton Fire (I-210 + SR-134)
sbt "runMain scalation.simulation.process.model.runEatonFireModel"
```

### Key Entry Points
- US-101 Model: `CalRoute101_3.scala`
- Eaton Model: `EatonFireModel.scala` (I-210 WB + SR-134 WB, dual-corridor)
- Calibration: `CalibrateCalRoute101.scala`
- Dynamics: `Dynamics.scala` (IDM, Gipps, Krauss)
- OSM Download: `src/main/scala/scalation/simulation/scripts/download_osm_geometry.py`

## Animation / Rendering Architecture (Current)

### Layer order in DgAnimator.paintComponent:
```
Layer M-1:  OSM background roads (polylines from JSON)
Layer M-1b: OSM place labels (cities, suburbs, neighbourhoods from JSON)
Layer 0:    Road polygons (filled asphalt, edge-grouped by segment)
Layer 1:    Lane markings (solid edge, dashed interior)
Layer 2:    Vehicles (rotated car shapes, velocity-colored)
Layer 3:    HUD overlay (model name, clock, scale bar)
```

### OSM Pipeline (auto-download):
```
Model.loadOsmBackground(jsonPath, gpsAnchors, dims)
  → if JSON missing: compute bbox from anchors, shell out to Python script
  → OsmRoadNetwork.load(jsonPath, anchors, dims)
    → parse roads[] + places[] from JSON
    → project GPS → screen via Coordinates (shared anchor frame)
  → dgAni.setBackgroundRoads(polylines, roadTypes)
  → dgAni.setBackgroundPlaces(places)
```

### Key rendering classes:
| Class | File | Role |
|-------|------|------|
| `OsmRoadNetwork` | `animation/OsmRoadNetwork.scala` | Load OSM JSON, project GPS→screen |
| `OsmPlace` | `animation/OsmRoadNetwork.scala` | Place label: name, type, screen x/y |
| `DgAnimator` | `animation/DgAnimator.scala` | All rendering — roads, vehicles, labels, HUD |

### What NOT to touch (Dr. Miller's core):
- `Dgraph.scala`, `Animator.scala`, `AnimateCommand.scala`

## Completed Milestones

### Phase 2: Road Rendering (R-Series) — ✅ ALL COMPLETE
- R1: Road polygon rendering (filled asphalt)
- R2: Lane markings (solid edge, dashed interior)
- R3: Vehicle rotation (heading from position delta)
- R4: Ramp surface (tapered polygons)
- R5: Shoulder lines

### OSM Background Map — ✅ COMPLETE
- Python download script (roads + places from Overpass API)
- Auto-download on first run (bbox from GPS anchors)
- `OsmRoadNetwork` loader (JSON → screen-space projection)
- Place labels from OSM data (city/town/suburb/neighbourhood)
- Works for both EatonFireModel and CalRoute101_3

### Infrastructure — ✅ COMPLETE
- Invisible nodes (alpha-gate in DgAnimator)
- `displayColor` on Junction, Source, Sink
- Graph-derived ramp positioning (`rampAttachPoint` + `perpVec`)
- Variable-lane DLL unification (Tasks 0–11)
- HUD overlay, vehicle inspector, replay system

### MOBIL Lane-Change Model — ✅ COMPLETE
- `object MOBIL` in `Dynamics.scala` — full Treiber & Kesting (2007) implementation
- Safety criterion: `ã_f ≥ -b_safe` (new follower won't brake too hard)
- Incentive criterion: `ã_s - a_s + p·(ã_f - a_f) > Δa_th`
- 3 MOBIL params added to `Vehicle.def_prop`: `p_mobil=0.2`, `da_th=0.2`, `b_safe=4.0`
- `idmAccelFor()` — hypothetical IDM acceleration for any follower-leader pair
- Wired into `VTransport.move()` — every timestep after `updateV()`
- 3-second cooldown (`lastLaneChangeTime`) prevents oscillation
- `Pathway.parentRoute` back-reference → `Vehicle.myRoute` for Route access
- Old model-level lane-change calls removed (CalRoute101_3 + EatonFireModel)
- **One place, one decision, one execution** — models don't touch lane changes

## Known Bugs / Issues

| Issue | Status |
|-------|--------|
| No gap acceptance at merge | 🟡 Major — both models unconditionally insert (no safe-gap wait). If you yield to director while waiting, follower ramp vehicles are blind and drive over the waiting vehicle. |
| Animation teleportation at merge | 🟡 Cosmetic — car jumps from ramp endpoint to mainline segment (no smooth visual transition). Fix: interpolate screen position over ~0.5s or use a short auxiliary VTransport bridging ramp end → mainline lane. ~30 lines. |
| Ramp `gap = -4` from VTransport coroutine yield | 🟡 Benign — multiple ramp vehicles at `disp=0` on same ramp DLL. IDM clamp holds at `v=0` until leader clears. Root cause: VSource spawns while prior car is still inside `move()` yield. Not a physics error — just queuing. |
| Outer lane crowding after ramp merge | 🟡 Minor — ramp vehicles merge into outermost lane, MOBIL moves them inward but throughput limited by 3s cooldown. Multiple lane changes needed (4→3→2) take 6+ seconds. Realistic behavior — real highways show same pattern near on-ramps. |
| vdeque density mismatch after mid-segment lane change | 🟡 Minor — when MOBIL triggers `changeLane` inside `move()`, vehicle stays in old VTransport's `vdeque` but moves to new lane's DLL. Density stats off for that segment. Won't crash. |
| SR-134 OR CSV has zero flow | Data quality — sensors not reporting |
| Fire-day data not yet wired | Need `PeMSDemand.I210_WB_FireDay_Anchor()` |

## Key Design Decisions (Preserved)

- **Java2D only** — no JavaFX, no 3D. Graphics2D + Path2D + AffineTransform.
- **OSM for rendering, PeMS for simulation** — lane counts, demand, ramp joins from PeMS. OSM provides visual context only.
- **Auto-download, not manual** — `loadOsmBackground` downloads on first run if JSON missing. Cached forever after.
- **Data-driven labels** — place names come from OSM `place` nodes, not hardcoded. Works for any location worldwide.
- **`OsmPlace` not `OsmLabel`** — simple case class from data. No deduplication heuristics, no abbreviation logic.
- **Separate rendering from simulation geometry** — simulation graph stays ~20 junctions. OSM polyline has 4000+ segments.
- **`LeaderResult` enum** for findLeader — compiler-enforced gap computation (not yet implemented).
- **`b_emergency` separate from `bmax`** — comfortable vs physical max deceleration (not yet implemented).
- **`forceMerge` = simple lane-drop** — target = `availLanes.last`, full job in one call.
- **MOBIL lives in `VTransport.move()`, not in model `act()`** — lane-change is engine-level, not model-level. Any model using Route+VTransport gets MOBIL for free.
- **MOBIL + changeLane double safety check** — MOBIL checks acceleration-based safety (b_safe), changeLane checks physical gap (safeDisp). Belt and suspenders — intentional redundancy.
- **3s cooldown prevents oscillation** — without it, MOBIL flip-flops every timestep because each lane change alters DLL composition, reversing incentive.
- **Per-vehicle MOBIL params** — `p_mobil`, `da_th`, `b_safe` stored in `Vehicle.prop` map. Future: use `Uniform` selectors for heterogeneous driver aggression.
- **No per-vehicle calibration data yet** — uniform params are enforced guesses without calibration data. Park it.

## What Is In Progress — ACTIVE NEXT TASKS

1. ✅ **MOBIL lane change** — implemented, tested, working. Vehicles spread across lanes.
2. 🔄 **Visual verification** — run both models, confirm spreading + no crashes over full simulation
3. 🔄 **LOS road coloring** — color road polygons by density (green→yellow→red)
4. ⏸ **Gap acceptance at merge** — ramp vehicles wait for safe gap before inserting (coroutine yield issue)
5. ⏸ **Animation teleportation fix** — smooth visual transition at ramp→mainline merge
6. ⏸ **Density-based lane assignment (Phase B)** — design complete, not yet implemented

## Session State — 2026-04-15

### Date
April 15, 2026

### What Was Completed
- **MOBIL lane-change model** — full implementation across 5 files:
  - `Dynamics.scala`: `object MOBIL` with `idmAccelFor`, `mobilIncentive`, `checkLaneChange` (~90 lines, full Scaladoc with equations)
  - `Vehicle.scala`: `myRoute` field, `lastLaneChangeTime` cooldown, 3 MOBIL params (`p_mobil`, `da_th`, `b_safe`) + inline accessors
  - `VTransport.scala`: MOBIL call after `updateV()` with 3s cooldown guard
  - `Pathway.scala`: `parentRoute` back-reference, wired in `addToAlist`
  - `Route.scala`: `pathway(i).parentRoute = this` in constructor loop
- **Removed old lane-change calls** from CalRoute101_3 (lines 214-219) and EatonFireModel (commented block lines 407-415)
- **Fixed MOBIL oscillation** — added 3s cooldown timer (`lastLaneChangeTime`) after discovering vehicles ping-ponged between lanes every timestep, causing simulation to hang
- **Ran EatonFireModel** — vehicles are spreading across lanes (MOBIL working). Outer lane still crowded near ramps (realistic).

### What Was In Progress
- **Visual verification** — need to run both models for full simulation duration and watch for edge cases
- **Outer lane crowding** — ramp vehicles spread but slowly (3s cooldown × multiple lane changes needed). May reduce cooldown to 2s or add strategic incentive for ramp vehicles.

### Known Bugs Found
- **MOBIL oscillation without cooldown** — discovered and fixed. Without the 3s cooldown, `changeLane` alters DLL composition which reverses MOBIL incentive, causing infinite flip-flop inside `move()`.
- **vdeque density mismatch** — when MOBIL changes lane mid-segment, vehicle remains in old VTransport's `vdeque` but moves to new lane's DLL. Density tracking wrong for that segment. Non-critical.

### Key Decisions Made
- **MOBIL in engine, not model** — lane-change logic in `VTransport.move()`, removed from model `act()`. Any model gets MOBIL for free.
- **Keep `changeLane` gap check** — redundant with MOBIL safety criterion but serves as physical safety net.
- **Rejected per-vehicle Uniform selectors** — no calibration data to parameterize. All vehicles share same MOBIL params for now.
- **3-second cooldown** — standard anti-oscillation (SUMO uses 2-5s). Prevents flip-flop but limits throughput to 1 lane change per 3s per vehicle.
