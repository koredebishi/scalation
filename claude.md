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

## Session State — Last Updated: 2026-03-31

### What was completed (previous sessions)
- ✅ All I-210 data pipeline work (anchor CSV, PeMSDemand, arrivals, ramp data) — see git history
- ✅ Fixed lane count, off-by-one, nStop verification
- ✅ Variable-Lane DLL Unification (Tasks 0–11) — commit `8bdf7ae12`

### What was completed (this session — Graph-Derived Ramp Positioning + Visual Fixes)
- ✅ Off-ramp same-side positioning (was on opposite side of mainline)
- ✅ FR/OR collision nudge for same-PM stations (50px → replaced by graph-derived)
- ✅ Shortened all labels: route `I-210-W_Rte_0_seg19` → `I210W_RL0s19`, ramps `I210_OR6` etc.
- ✅ Ramp label declutter: 3 labels per ramp → 1 (VTransport only, Junction/VSource/Sink blanked)
- ✅ **Graph-derived ramp positioning** — `Route.rampAttachPoint(seg)` + `perpVec`
  - Task 0: `Route.scala` — added `rampAttachPoint`, `perpVec`, `_points` array
  - Task 1: `CorridorBuilder.scala` — reordered: Route built first, ramp junctions derived from geometry
  - Task 2: `EatonFireModel.scala` — ramp VSource positions use `rampAttachPoint + perpVec * RAMP_LEN`
  - Task 3: `EatonCorridorConfig.scala` — removed `rampShift` from all methods, steps 11/12 now dummy
  - Task 5: `sbt compile` — zero errors
- ✅ **Side swap fix** — negated `perpVec` so ramps are on outermost lane side (not lane 0)
- ✅ **Same-seg FR/OR nudge** — off-ramps shifted 30px downstream along road direction when sharing joinSeg with on-ramp
- ✅ **DTA Blueprint** — `docs/2026_WSC_paper/dta-blueprint.md`
- ✅ **Git push**: `f3515ee8d` on branch `feature/variable-lane-dll-unification`

### Files touched (this session)
| File | Changes |
|------|---------|
| `Route.scala` | `rampAttachPoint(seg)`, `perpVec`, `_points` array; pathway naming `L$i`; VTransport naming `s$i` |
| `CorridorBuilder.scala` | Route-first build order; ramp positions from `rampAttachPoint`; road-direction nudge for same-seg FR/OR; shortened prefix; blanked Junction/Sink names |
| `EatonFireModel.scala` | Ramp VSource positions from `rampAttachPoint + perpVec * RAMP_LEN`; removed `getVSourceCenterAndOffsets`; shortened all component names |
| `EatonCorridorConfig.scala` | Removed `rampShift` from all 3 methods; steps 11/12 replaced with dummy arrays |
| `Pathway.scala` | VTransport naming `s$i` instead of `_seg$i` |
| `context/graph-derived-ramp-positioning.md` | Design document |
| `docs/2026_WSC_paper/dta-blueprint.md` | DTA implementation blueprint |

### What is in progress
- 🔄 **Visual verification** — need to run EatonFireModel and confirm ramp positions are correct after graph-derived changes
- 🔄 **Uncommitted changes** — graph-derived ramp impl + side swap + nudge + DTA blueprint (need git commit)
- 🔄 **End-to-end run with `synthetic=false`** — nStop verified, full simulation run not yet attempted
- 🔄 **SR-134 ramp data quality** — all 7 SR-134 on-ramp sensors report zero flow
- 🔄 **DTA Phase 1** — FireGrid + SmokeGrid (standalone, no traffic dependency)

### Known bugs / issues
| Issue | File | Status |
|-------|------|--------|
| SR-134 OR CSV has zero flow everywhere | `eaton_134_W_baseline_Dec03-10-17_OR.csv` | **Data quality** — sensors not reporting |
| `srcPrefix` hardcoded for cases `0\|1\|2\|3\|4` | `VSource.scala:61` | Works for 5 lanes but fragile |
| Fire-day data not yet wired | `DemandConfig.scala` | Need `PeMSDemand.I210_WB_FireDay_Anchor()` |
| `forceMerge` is random lane pick | `Route.scala` | Works but could be improved with gap-based selection |
| Ramp side may need visual tuning | `Route.perpVec` | Negated for outermost lane — needs visual confirmation |

### Key decisions made (this session)
- **Graph-derived ramp positioning**: `rampAttachPoint(seg)` computes outermost lane edge from `lanesAt(seg) * GAP`. Eliminates all `rampShift` magic numbers.
- **perpVec negated**: points away from lane 0 (toward ramp side of freeway)
- **Same-seg FR/OR nudge = 30px downstream**: uses road direction vector, not perpendicular
- **RAMP_LEN = 150px**: consistent between CorridorBuilder (sinks) and EatonFireModel (VSources)
- **Labels: VTransport only per ramp**: Junction/VSource/Sink names blanked to avoid clutter
- **Prefix shortened**: `I-210-W_` → `I210W_` via `filter(_.isLetterOrDigit)`
- **DTA architecture**: Junction = decision engine, VTransport = data provider, Route = minimal

### Verified nStop values (baseline, synthetic=false)
```
I-210 Mainline (5 lanes): L0=5326, L1=5174, L2=4019, L3=3281, L4=1873
I-210 Ramps (22): 0,2234,2165,1960,3522,0,0,0,0,0,744,0,0,0,1539,549,0,2315,0,5456,53,0
SR-134 Ramps (7): all zero (data quality issue)
```
