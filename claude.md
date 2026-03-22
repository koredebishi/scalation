# ScalaTion Traffic Simulation

Microscopic traffic simulation using ScalaTion 2.0 (Scala 3).  
US-101 corridor with IDM/Gipps/Krauss car-following dynamics.

**IDE:** IntelliJ IDEA  
**HPC:** Sapelo2 (krb84578@uga.edu)

## Core Rules
- **Always ask permission before writing or modifying code**
- **Do not plagiarize** - all paper text must be original
- **Do not hallucinate** - if unsure, say so

## Context Files

| Need | File |
|------|------|
| Behavioral guidelines | `context/rules.md` |
| Coding style & key files | `context/scalation-style.md` |
| IDM params & domain knowledge | `context/traffic-simulation.md` |
| HPC Sapelo2 commands | `context/hpc.md` |

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
sbt "runMain scalation.simulation.process.example_1.CalRoute101_2"
```

### Key Entry Points
- Simulation: `CalRoute101_2.scala`
- Calibration: `CalibrateCalRoute101.scala`
- Dynamics: `Dynamics.scala` (IDM, Gipps, Krauss)

