# ScalaTion Traffic Simulation — Refactoring Status Tracker

**Master Design Doc:** `docs/TRAFFIC_SIMULATION_REFACTORING_PLAN.md`  
**Last Updated:** March 26, 2026

---

## ✅ IMPLEMENTATION STATUS

### Steps 1-7: COMPLETE ✅

| Step | What | Status |
|---|---|---|
| 1 | `FlowDirection` enum + `FFConnectorSpec` | ✅ Done in `NetworkConfig.scala` |
| 2 | `DemandFlag` enum | ✅ Done in `NetworkConfig.scala` |
| 3 | `MultiCorridorConfig` case class | ✅ Done in `config/MultiCorridorConfig.scala` |
| 4 | `junctionPMs` + PM lookup methods | ✅ Done in `EatonCorridorConfig.scala` |
| 5 | `AggregatedDemand` case class | ✅ Done in `DemandConfig.scala` |
| 6 | `CorridorBuilder` + `BuiltCorridor` + `BuiltNetwork` | ✅ Done in `builder/CorridorBuilder.scala` |
| 7 | `buildMulti()` for multi-corridor + FF | ✅ Done (merged into Step 6) |

### Steps 8-9: COMPLETE ✅

| Step | What | Status |
|---|---|---|
| 8 | Refactor `EatonFireModel` to use builder | ✅ Done — uses `CorridorBuilder.buildMulti()` |
| 9 | Refactor `CalRoute101_3` to use builder | ✅ Done — uses `TrafficModelBuilder` |

### ArrivalSource Unification: COMPLETE ✅

| Component | Status |
|---|---|
| `SyntheticArrivalSource` | ✅ Done — fixed counts for debugging |
| `AggregatedArrivalSource` | ✅ Done — loads Eaton CSV mainline data |
| `AggregatedRampArrivalSource` | ✅ Done — loads Eaton CSV ramp data |
| `ArrivalSource.syntheticSources()` | ✅ Done — factory for synthetic mode |
| `ArrivalSource.fromAggregated()` | ✅ Done — factory for Eaton data mode |
| `EatonFireModel` unified with `ArrivalSource` | ✅ Done — uses `synthetic` flag to switch |

---

## 📁 Backup Status (March 26, 2026)

### ✅ Backed Up Today (Orphan Files)

| File | Reason |
|---|---|
| `builder/Highway.scala.bak` | Abandoned abstraction — never imported |
| `process/RampNetWorkConfig.scala.bak` | Old hardcoded config — never imported |
| `process/CalibrateCalRoute101.scala.bak` | Fully commented out — referenced deleted CalRoute101_2 |
| `process/CalibratableModel.scala.bak` | Duplicated trait (exists in CalibrationFramework) |
| `example_1/korede.scala.bak` | Old scratch file — commented code |

### 📂 Previously Backed Up (Earlier Phases)

| File | Reason |
|---|---|
| `CalRoute101.scalaa.bak` | Original model — replaced by CalRoute101_3 |
| `CalRoute101_2.scalaa.bak` | v2 model — replaced by CalRoute101_3 |
| `TrafficConfig.scalaa.bak` | Old config — replaced by NetworkConfig + DemandConfig |
| `TrafficConfig2.scalaa.bak` | Old config — replaced by config layer |
| `Dynamics.scala.bak` | Old version before IDM/Gipps/Krauss refactor |
| `Vehicle.scala.bak` | Old version |
| `VTransport.scala.bak` | Old version |
| `Route.scala.bak` | Old version |
| `Pathway.scala.bak` | Old version |
| `Motion.scala.bak` | Renamed to Dynamics |

### 🔒 Files to KEEP (Still Active)

| File | Used By |
|---|---|
| `TrafficModelBuilder.scala` | CalRoute101_3, CalibrationFramework |
| `EatonCorridorConfig.scala` | MultiCorridorConfig, CorridorBuilder |

---

## ✅ What's Working Today

| Model | Uses | Status |
|---|---|---|
| `EatonFireModel` | `CorridorBuilder.buildMulti()` + `MultiCorridorConfig.EatonFire_WB()` | ✅ Works |
| `CalRoute101_3` | `TrafficModelBuilder.US101_DonaldDoyle_PeMS()` | ✅ Works |

---

## 🎯 REMAINING WORK

### A. Unify `CalRoute101_3` with `CorridorBuilder` Pattern

CalRoute101_3 uses the older `TrafficModelBuilder` while EatonFireModel uses the newer `CorridorBuilder`. To unify:

1. Create `CorridorLayout` wrapper for DonaldDoyle (or minimal `NetworkConfig`-only version)
2. Add `MultiCorridorConfig.US101_DonaldDoyle()` factory method
3. Migrate `CalRoute101_3` to use `CorridorBuilder.build()` instead of `TrafficModelBuilder`
4. Backup `TrafficModelBuilder.scala` → `.bak` after migration

### B. PeMS Demand Integration for Eaton (CRITICAL)

EatonFireModel uses placeholder `Exponential(MINUTE/10)` arrivals. Need:

1. Parse Eaton PeMS CSV format (different column layout than US-101 Donald Doyle)
2. Create `PeMSArrivalSource` for aggregated format
3. Wire into EatonFireModel replacing placeholder

### C. Off-Ramp Activation

- `RampSpec(mode=Off)` exists in config
- `Ramp` class supports Off mode
- NOT wired in `EatonFireModel.Car.act()` — needs diverge logic

---

## 📚 Reference

For full architecture details, layer specifications, and design rationale, see:

**`docs/TRAFFIC_SIMULATION_REFACTORING_PLAN.md`** (664 lines)

- Executive Summary
- Scope Clarification (Physics-Informed vs Core ScalaTion)
- Current State Analysis
- Proposed Architecture (5-Layer Diagram)
- Layer Specifications (NetworkConfig, DemandConfig, DynamicsConfig, TrafficModelBuilder, SimulationReport)
- Migration Path
- Usage Examples
