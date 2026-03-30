# ScalaTion Traffic Simulation

Microscopic traffic simulation using ScalaTion 2.0 (Scala 3).  
US-101 corridor with IDM/Gipps/Krauss car-following dynamics.

**IDE:** IntelliJ IDEA  
**HPC:** Sapelo2 (krb84578@uga.edu)

## Core Rules
- **Always ask permission before writing or modifying code**
- **Do not plagiarize** - all paper text must be original
- **Do not hallucinate** - if unsure, say so

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

## Papers

| Paper | Status | File |
|-------|--------|------|
| ANNSIM 2026 | ✅ Submitted | `context/papers/annsim-2026.md` |
| WSC 2026 — Wildfire/Contraflow | 🔄 Active Target | `context/papers/wsc-2026.md` |

## Dissertation Studies

| Study | Title | Status |
|-------|-------|--------|
| Study 1 | Structural Sensitivity Analysis (integrators, arrivals) | ✅ ANNSIM 2026 Submitted |
| Study 2 | Wildfire Evacuation & Contraflow on I-10 (Palisades Fire) | 🔄 WSC 2026 Climate Resilience Track — Active |
| Study 3 | Unified Agentic Architecture (long-term, internal) | 🔄 Internal Vision — Not for committee or PI yet |

**NOTE: Calibration is a supporting result, not a standalone paper. PI is not interested in calibration as a paper. WSC 2026 target is the Wildfire/Contraflow study aligned with Climate Resilience theme.**

## Active Focus
**WSC 2026 (Wildfire/Contraflow)** - Deadline: April 5, 2026  
See `context/papers/wsc-2026.md` for timeline and checklists.

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

## Session State — Last Updated: 2026-03-30

### What was completed
- ✅ Cleaned I-210 anchor sensor CSV (`717653-i210-firstSensor-baseline.csv`): 13 cols, 73 rows, 5-min bins 17:00–23:00 inclusive
- ✅ Added `I210_MainlineLayout` and `I210_TimeWindow` to `PeMSDataLoader.scala`
- ✅ Added `getSpeedMatrixFromFile` helper to `RowTimeLoader` companion object
- ✅ Overrode `rowTime`, `rowTimeSlice`, `nextRow` in EatonFireModel for 5-min bins (300s)
- ✅ Fixed VSource laneID bug: `actor.laneID = subtype` + `safeLane` clamp at speed lookup
- ✅ **Mainline arrivals now use CalRoute101_3 pattern**: Added `window`/`layout` fields to `PeMSDemand` (backward-compatible defaults). Added `PeMSDemand.I210_WB_Anchor()` factory pointing to cleaned anchor CSV. `PeMSArrivalSource` now passes `demand.window`/`demand.layout` to `PeMSDataHelper`. Factory uses `demand.window.binSeconds` for rowTime (not hardcoded 900). EatonFireModel uses `ArrivalSource.allSources(pems210, nLanesAnchor)` — **same pattern as CalRoute101_3**.
- ✅ **Deleted dead code**: Removed `AnchorFlowArrivalSource` class, `fromAnchorAndRamps` factory, `loadFlowAndSpeedFromFile` helper. All were unnecessary — existing `PeMSArrivalSource` handles Eaton data via demand config.
- ✅ **Fixed I-210 lane count**: Changed `EatonCorridorConfig.buildLayoutFromCoords` from `modeLanes()` (statistical mode=4) to entry station's lane count (station 717653 = 5 lanes). For WB, entry = highest PM. Now `numLanes210=5` matches anchor CSV.
- ✅ **Fixed off-by-one in TimeWindow**: `endRow` changed from 72 to 73. `MatrixD.load` `stop` param is exclusive — CSV has 73 data rows (17:00–23:00 inclusive). Updated `nt=73` in EatonFireModel. **nStop values now match manual calculation exactly**: L0=5326, L1=5174, L2=4019, L3=3281, L4=1873.
- ✅ **Verified I-210 ramp data**: 22 ramps from aggregated OR CSV. 10 have traffic (2234, 2165, 1960, 3522, 744, 1539, 549, 2315, 5456, 53), 12 have zero flow. All match CSV data exactly.

### What is in progress
- 🔄 **End-to-end run with `synthetic=false`** — nStop verified, full simulation run not yet attempted
- 🔄 **SR-134 ramp data quality** — all 7 SR-134 on-ramp sensors report zero flow across ALL time bins (entire CSV = 0). Need alternate data source or synthetic ramp demand for SR-134.
- 🔄 **Fire-day anchor CSV** (`717653-i210-firstSensor-fireday.csv`) exists but not yet cleaned or wired — needs `PeMSDemand.I210_WB_FireDay_Anchor()` factory

### Known bugs / issues
| Issue | File | Status |
|-------|------|--------|
| SR-134 OR CSV has zero flow everywhere | `eaton_134_W_baseline_Dec03-10-17_OR.csv` | **Data quality** — sensors not reporting |
| `srcPrefix` hardcoded for cases `0\|1\|2\|3\|4` | `VSource.scala:61` | Works for 5 lanes but fragile |
| Fire-day data not yet wired | `DemandConfig.scala` | Need `PeMSDemand.I210_WB_FireDay_Anchor()` |

### Key decisions made
- **Mainline arrivals follow CalRoute101_3 pattern**: `PeMSDemand` → `PeMSArrivalSource` → `PeMSDataHelper`. No new arrival source classes needed.
- **`PeMSDemand` carries `window`/`layout`**: backward-compatible defaults (900s/4-lane). Eaton overrides via `I210_WB_Anchor()`.
- **Ramps from aggregated OR CSV**: `AggregatedRampArrivalSource` correct for multi-station ramp data.
- **Entry station defines corridor lane count**: replaces `modeLanes()` which picked mode=4. Anchor sensor 717653 has 5 lanes → corridor has 5 lanes.
- **`endRow` is exclusive**: `MatrixD.load` `stop` param is exclusive. 73 data rows → `endRow=73`.

### Verified nStop values (baseline, synthetic=false)
```
I-210 Mainline (5 lanes): L0=5326, L1=5174, L2=4019, L3=3281, L4=1873
I-210 Ramps (22): 0,2234,2165,1960,3522,0,0,0,0,0,744,0,0,0,1539,549,0,2315,0,5456,53,0
SR-134 Ramps (7): all zero (data quality issue)
```
