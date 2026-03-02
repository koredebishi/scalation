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
| WSC 2026 — Calibration | 🔄 In Progress | `context/papers/wsc-2026.md` |
| WSC 2026 — Wildfire/Contraflow | 🔄 Proposed | `context/papers/wsc-2026.md` |

## Dissertation Studies

| Study | Title | Status |
|-------|-------|--------|
| Study 1 | Structural Sensitivity Analysis (integrators, arrivals) | ✅ ANNSIM 2026 Submitted |
| Study 2 | Constrained Calibration: IDM vs Gipps × 5 optimizers | 🔄 WSC 2026 In Progress |
| Study 3 | Wildfire Evacuation & Contraflow on I-10 (Palisades Fire) | 🔄 Proposed — WSC 2026 Climate Resilience Track |

**NOTE: The LLM-orchestrated causal calibration proposal was NOT accepted. Study 3 is the Wildfire/Contraflow simulation study.**

## Active Focus
**WSC 2026 (Paper 2 — Calibration)** - Deadline: April 5, 2026  
**WSC 2026 (Paper 3 — Wildfire)** - Deadline: April 5, 2026  
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

