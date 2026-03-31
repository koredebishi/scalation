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

### What was completed (previous sessions)
- ✅ All I-210 data pipeline work (anchor CSV, PeMSDemand, arrivals, ramp data) — see git history
- ✅ Fixed lane count, off-by-one, nStop verification

### What was completed (this session — Variable-Lane DLL Unification)
- ✅ **Task 0**: Design doc written to `context/variable-lane-dll-unification.md`
- ✅ **Task 1**: VTransport owns per-segment DLL — `vList: DoublyLinkedList`, `addToAlist`/`removeFromAlist`
- ✅ **Task 2**: Pathway delegates DLL ops to VTransport, backward-compat overloads preserved
- ✅ **Task 3**: Dynamics `findLeader()` cross-boundary lookup via VTransport, removed `segId` patches
- ✅ **Task 4**: CalRoute101_3 `driveHighway` uses explicit DLL re-insertion at segment transitions
- ✅ **Task 5**: sbt compile checkpoint (green)
- ✅ **Task 6**: Route accepts `lanesPerSeg: Array[Int]`, adds `laneExistsAt()`/`lanesAt()`/`forceMerge()`
- ✅ **Task 7**: Pathway sparse seg array — `null` where lane doesn't physically exist
- ✅ **Task 8**: `MainlineSpec.lanesPerSeg: Option[Array[Int]]`; EatonCorridorConfig computes per-segment lane counts from PeMS station data (min of adjacent stations)
- ✅ **Task 9**: CorridorBuilder passes `lanesPerSeg` to Route
- ✅ **Task 10**: EatonFireModel `driveHighway` lane-end detection with `forceMerge`
- ✅ **Task 11**: Ramp/FFConnector merge-point lane-existence guards in `actOnCorridor` and FF diversion
- ✅ **Full sbt compile**: 31 Scala sources, zero errors
- ✅ **Git commit**: `8bdf7ae12` on branch `feature/variable-lane-dll-unification`

### Files touched (this session)
| File | Changes |
|------|---------|
| `VTransport.scala` | Owns DLL per-segment; `addToAlist`/`removeFromAlist`/`getLast` |
| `Pathway.scala` | Delegates DLL ops to VTransport; sparse seg array; backward-compat overloads |
| `Route.scala` | `lanesPerSeg` param; `laneExistsAt`/`lanesAt`/`forceMerge` |
| `Dynamics.scala` | `findLeader()` cross-boundary via VTransport; removed segId patches |
| `CalRoute101_3.scala` | Explicit DLL re-insertion in driveHighway |
| `EatonFireModel.scala` | Lane-end detection + forceMerge; lane-existence guards at entry/ramp/FF |
| `CorridorBuilder.scala` | Passes `lanesPerSeg` to Route |
| `NetworkConfig.scala` | `MainlineSpec.lanesPerSeg: Option[Array[Int]]` |
| `EatonCorridorConfig.scala` | Per-segment lane counts from PeMS station data |
| `context/variable-lane-dll-unification.md` | Full design document |

### What is in progress
- 🔄 **End-to-end run with `synthetic=false`** — nStop verified, full simulation run not yet attempted
- 🔄 **Runtime testing of variable-lane corridor** — I-210 has 4→5 lane transitions; needs end-to-end run to verify lane-end merges work
- 🔄 **SR-134 ramp data quality** — all 7 SR-134 on-ramp sensors report zero flow across ALL time bins. Need alternate data source or synthetic ramp demand.
- 🔄 **Fire-day anchor CSV** (`717653-i210-firstSensor-fireday.csv`) exists but not yet cleaned or wired

### Known bugs / issues
| Issue | File | Status |
|-------|------|--------|
| SR-134 OR CSV has zero flow everywhere | `eaton_134_W_baseline_Dec03-10-17_OR.csv` | **Data quality** — sensors not reporting |
| `srcPrefix` hardcoded for cases `0\|1\|2\|3\|4` | `VSource.scala:61` | Works for 5 lanes but fragile |
| Fire-day data not yet wired | `DemandConfig.scala` | Need `PeMSDemand.I210_WB_FireDay_Anchor()` |
| `forceMerge` is random lane pick | `Route.scala` | Works but could be improved with gap-based selection |

### Key decisions made
- **VTransport owns its DLL**: each VTransport segment has its own `DoublyLinkedList[Vehicle]`, not the Pathway
- **Pathway uses sparse seg array**: `seg(i)` returns null if lane i doesn't exist at that segment
- **Route.forceMerge**: picks random available lane when a lane ends; acceptable for now
- **Per-segment lane count = min(upstream station lanes, downstream station lanes)**: conservative approach
- **MainlineSpec.lanesPerSeg is Option[Array[Int]]**: None = uniform lanes (backward compatible)
- **Entry station defines corridor max lane count**: for Route array sizing
- **`endRow` is exclusive**: `MatrixD.load` `stop` param is exclusive. 73 data rows → `endRow=73`.

### Verified nStop values (baseline, synthetic=false)
```
I-210 Mainline (5 lanes): L0=5326, L1=5174, L2=4019, L3=3281, L4=1873
I-210 Ramps (22): 0,2234,2165,1960,3522,0,0,0,0,0,744,0,0,0,1539,549,0,2315,0,5456,53,0
SR-134 Ramps (7): all zero (data quality issue)
```
