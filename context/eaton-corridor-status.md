# Eaton Corridor — Where We Are

**Last updated:** March 23, 2026  
**Corridors:** I-210 Westbound + SR-134 Westbound (Eaton fire evacuation)  
**Target:** WSC 2026 — Climate Resilience Track (Deadline: April 5, 2026)

---

## 🏗️ Architecture Overview

```
PeMS station_map.csv
        │
        ▼
EatonCorridorConfig.scala ──► CorridorLayout (I210_WB, SR134_WB, I210_EB)
        │                         │
        │                         ▼
        │                    EatonFireModel.scala
        │                    ┌─────────────────────────────┐
        │                    │  I-210 WB (27 junc, 26 seg) │
        │                    │       │ FF diverge           │
        │                    │       ▼ (30% split)          │
        │                    │  SR-134 WB (13 junc, 12 seg) │
        │                    │       │                      │
        │                    │  FFConnector.scala            │
        │                    │  (VTransport + DLL)           │
        │                    └─────────────────────────────┘
        │
        ▼
PeMS Flow CSVs (5 date sets)  ──► PeMSDemand (NOT YET WIRED)
```

---

## ✅ Done

### 1. Station Map Loader (`EatonCorridorConfig.scala`)
- Reads `station_map.csv` (194 stations: 77 I-210 W, 73 I-210 E, 43 SR-134 W)
- Parses into `StationRecord` case class (stationId, freeway, direction, laneType, lat, lon, absPM, location, lanes)
- GPS → screen coordinate conversion via `Coordinates` class
- Filtering by freeway/direction and lane type, sorted by postmile

### 2. CSV-to-Model Translation Protocol
- Designed and documented: `Lane Type` column drives component creation
- **ML** → Junctions (mainline sensor nodes)
- **OR** → On-Ramps (`RampSpec` with `mode=On`)
- **FR** → Off-Ramps (`RampSpec` with `mode=Off`)
- **FF** → Freeway-freeway connectors (now modeled via `FFConnector`)
- **HV** → HOV lane sensors (ignored initially)

### 3. `CorridorLayout` Case Class (`EatonCorridorConfig.scala`)
- Intermediate representation between CSV and model
- Fields: `config` (NetworkConfig), `junctionNames`, `mainlineScreenXY`, `onRampScreenXY`, `offRampScreenXY`, `segmentLengths`, `ffStations`
- Methods: `getVSourceCenterAndOffsets`, `summary()`, `numJunctions`, `numOnRamps`, `numOffRamps`

### 4. `buildCorridorLayout()` Method (`EatonCorridorConfig.scala`)
- Full 16-step pipeline: load → filter → classify → map postmile→segment → bundle
- Postmile binary search for ramp→segment mapping (`findJoinSegment`)
- Lane count via statistical mode (`modeLanes`)
- Segment lengths from postmile differences (miles × 1609.34 → meters)
- Ramp screen positions shifted laterally (configurable `rampShift` parameter)

### 5. Pre-built Lazy Val Corridor Presets
- `EatonCorridorConfig.I210_WB` — I-210 Westbound (27 junctions, 26 segments, 22 on-ramps, 18 off-ramps, 15.90 mi)
- `EatonCorridorConfig.I210_EB` — I-210 Eastbound (future contraflow)
- `EatonCorridorConfig.SR134_WB` — SR-134 Westbound (13 junctions, 12 segments, 7 on-ramps, 8 off-ramps, 6.78 mi)

### 6. `EatonFireModel.scala` — Dual-Corridor Working Model
- **I-210 W** (27 junctions, 26 segments, 22 on-ramps) + **SR-134 W** (13 junctions, 12 segments, 7 on-ramps)
- Both corridors share ONE coordinate frame via `buildSharedWBLayouts`
- Each corridor has its own Route, junctions, sources, sinks, ramps
- `Car` entity uses `actOnCorridor()` — parameterized to drive either corridor
- Subtype encoding: 0..3 = I-210 mainline, 4..25 = I-210 ramps, 100..103 = SR-134 mainline, 104..110 = SR-134 ramps
- Animation dims: 5000×3000 pixels (scaled up for spacing)
- Placeholder demand: `Exponential(MINUTE/10)` arrivals
- IDM dynamics with Ballistic integrator
- Compiles and runs with `simulate()` / `waitFinished()` / `Model.shutdown()`

### 7. `FFConnector.scala` — Freeway-to-Freeway Connector ✨ NEW
- **Created March 23, 2026** — dedicated framework component for inter-corridor transfers
- Models physical connector ramp between two freeway corridors at an interchange
- Single `VTransport` lane with its own DLL for car-following on the connector
- Configurable `splitRatio` (probability of diversion, default 0.30)
- Methods: `addToAlist`, `removeFromAlist`, `getFirst`, `getLast`
- Registered as a `Component` with animation support (orange edge)

### 8. FF Interchange Logic in EatonFireModel ✨ NEW
- **I-210 WB → SR-134 WB** at Pasadena interchange (PM 24.49)
- Diverge point: WINONA WAY junction on I-210 (matched by name search)
- Merge point: ORANGE GROVE junction on SR-134 (easternmost SR-134 junction)
- Probabilistic split: 30% of I-210 cars divert to SR-134 (placeholder, calibrate later)
- Full `driveHighway()` diversion flow:
  1. Exit I-210 pathway → 2. Drive FFConnector lane → 3. Enter SR-134 at merge → 4. Continue on SR-134
- SR-134 has **NO mainline VSources** — all mainline traffic enters via FF from I-210
- Only SR-134 on-ramp VSources feed local traffic mid-corridor

### 9. PeMS Data Downloaded (5 Date Sets)
- **Baseline** (Dec 3–17): I-210 W/E (ML+HV, OR, FR, FF), SR-134 W (ML+HV, OR, FR, FF)
- **Fire Day** (Jan 7, 2025): I-210 W/E + SR-134 W (all lane types)
- **3rd** (Jan 3): I-210 + SR-134
- **10th** (Jan 10): I-210 + SR-134
- **17th** (Jan 17): I-210 + SR-134
- Each date set has separate CSVs per lane type (ML_HV, OR, FR, FF)
- Analysis notebook: `data/WSC-Pems-Data-Eaton-Fire/data-eaton/analysis/eaton_fire_analysis.ipynb`

### 10. Tests
- `testEatonCorridorCoords` — validates station loading and GPS→screen mapping
- `testBuildCorridorLayout` — validates full pipeline output for I-210 W and SR-134 W

---

## 🔶 Partially Done

### FF Connector (was "Not Done", now partially done)
- [x] `FFConnector.scala` framework class created
- [x] Wired into `EatonFireModel` at WINONA/ORANGE GROVE interchange
- [x] `Car.driveHighway()` handles probabilistic diversion
- [ ] Split ratio is placeholder (0.30) — needs calibration from PeMS FF station `775725`
- [ ] FF connector data from PeMS not yet parsed for validation
- [ ] Animation rendering not fully tested

---

## ❌ Not Done Yet

### A. PeMS Demand Integration (🔴 CRITICAL — Blocks everything)
- [x] PeMS flow CSVs downloaded for all corridors and dates ✅
- [ ] Parse Eaton PeMS CSV format (different column layout than US-101 Donald Doyle)
- [ ] Create `PeMSDemand.I210_Westbound()` factory method
- [ ] Create `PeMSDemand.SR134_Westbound()` factory method
- [ ] Configure `ColumnLayout` / `PeMSDataHelper` for Eaton CSV format
- [ ] Wire `PeMSArrivalSource` into `EatonFireModel` replacing placeholder `Exponential`
- [ ] Implement real `getMuForSource()` from PeMS flow data
- [ ] Implement real `getSpeedMatrix()` from PeMS speed data
- [ ] Decide which date set = baseline (Dec 3–17) vs evacuation (Jan 7)

### B. Off-Ramp Activation (Medium Priority)
- [x] 18 FR stations mapped to `RampSpec(mode=Off)` with correct `joinSegment`
- [x] Off-ramp screen coordinates (`offRampScreenXY`) computed
- [ ] **Not wired yet**: need `Ramp(mode=Off)` + `Sink` per off-ramp in the model
- [ ] `Car.act()` needs off-ramp diverge logic (probability-based exit)
- [ ] Off-ramp exit probabilities from PeMS FR flow data

### C. Corridor Subsetting (Low Priority)
- [ ] Full I-210 W = 27 junctions (15.9 mi) — may be too large for initial testing
- [ ] Add PM range filter to `buildCorridorLayout` (e.g., PM 22.0–26.5 for focused area)
- [ ] Would reduce to ~5–8 junctions, matching CalRoute101_3 scale

### D. Calibration Framework for Eaton (Depends on A)
- [ ] Adapt `CalibrateCalRoute101` / `CalibrationFramework` for `EatonFireModel`
- [ ] Requires PeMS demand first (item A above)
- [ ] Fitness function: compare sim junction counts/speeds vs PeMS sensor data
- [ ] SPSA or grid search over IDM params (s, amax, bmax, T, rt)

### E. Contraflow Modeling (WSC 2026 Core Contribution)
- [ ] Use `I210_EB` layout for contraflow direction
- [ ] Design contraflow activation logic (time-triggered lane reversal)
- [ ] Extend `EatonFireModel` with contraflow scenario flag
- [ ] Compare evacuation times: baseline vs contraflow scenarios
- [ ] Metrics: clearance time, max queue length, throughput at corridor exit

### F. Wildfire / Smoke / DTA Coupling (WSC 2026 Stretch Goal)
- [ ] Rothermel fire spread model (cellular grid)
- [ ] Advection-diffusion smoke propagation
- [ ] Hazard-aware link cost function (smoke density + fire proximity)
- [ ] Time-dependent DTA (shortest path routing under dynamic costs)
- [ ] Smoke-aware IDM extension (speed/capacity reduction)
- [ ] Design doc exists: `docs/2026_WSC_paper/idea.md`

### G. Known CFM Issues (from `context/CFM_GAPS_AND_FIXES.md`)
- [ ] IDM `FREERANGE = 50 m` blinds IDM to real leaders 51–150 m ahead → raise to 150 m
- [ ] IDM lacks `segId` guard for cross-segment leaders (Gipps has it)
- [ ] `Vehicle.prop` is a global singleton — no per-vehicle parameters
- [ ] `v0 = 4.0 m/s` never resets on congested entry

---

## 📊 What Works Today (March 23, 2026)

| Capability | Status |
|------------|--------|
| Load PeMS station map → CorridorLayout | ✅ Working |
| I-210 WB corridor (27 junctions) | ✅ Runs with placeholder demand |
| SR-134 WB corridor (13 junctions) | ✅ Runs with placeholder demand |
| Dual-corridor shared coordinate frame | ✅ Working |
| FF interchange I-210 → SR-134 | ✅ Probabilistic diversion working |
| IDM car-following dynamics | ✅ Working (Ballistic integrator) |
| On-ramp merging (22 + 7 ramps) | ✅ Working |
| PeMS data-driven demand | ❌ Not wired yet |
| Off-ramp diverging | ❌ Not wired yet |
| Calibration (Eaton) | ❌ Not started |
| Contraflow scenario | ❌ Not started |
| Fire/smoke coupling | ❌ Design only |

---

## 🎯 Critical Path to WSC 2026 (April 5 Deadline)

```
Week of Mar 23:  A. PeMS Demand → parse Eaton CSVs, wire into model
                 ↓
Week of Mar 30:  B. Off-ramps + basic calibration
                 E. Contraflow scenario (I210_EB layout)
                 ↓
Week of Apr 1:   Run baseline vs contraflow experiments
                 Generate comparison tables/figures
                 ↓
Apr 5:           Submit WSC 2026 paper
```

**Minimum viable paper:** Baseline I-210+SR-134 evacuation vs contraflow, with PeMS-calibrated demand.  
**Stretch:** Add fire/smoke coupling (item F).

---

## Key Files

| File | Purpose |
|------|---------|
| `src/.../config/EatonCorridorConfig.scala` | Station map loader, CorridorLayout, buildCorridorLayout |
| `src/.../config/NetworkConfig.scala` | NetworkConfig, MainlineSpec, RampSpec, SensorSpec |
| `src/.../model/EatonFireModel.scala` | Working dual-corridor simulation model |
| `src/.../process/FFConnector.scala` | FF connector framework class ✨ |
| `src/.../builder/TrafficModelBuilder.scala` | Config-driven model assembly (US-101 reference) |
| `src/.../config/DemandConfig.scala` | PeMSDemand, PeMSArrivals definitions |
| `src/.../config/PeMSDataHelper.scala` | PeMS CSV loading utilities |
| `src/.../arrival/ArrivalSource.scala` | PeMSArrivalSource, PeMSRampArrivalSource |
| `context/CFM_GAPS_AND_FIXES.md` | Known IDM/Gipps issues and proposed fixes |
| `docs/2026_WSC_paper/idea.md` | WSC paper design (fire+smoke+DTA vision) |

## PeMS Data Inventory

| Date Set | Directory | Contents |
|----------|-----------|----------|
| Master station map | `eaton-corridor/station_map.csv` | 194 stations (I-210 W/E + SR-134 W) |
| **Baseline** (Dec 3–17) | `BaselineData_Dec03-10-17/` | I-210 W/E + SR-134 W: ML_HV, OR, FR, FF |
| **Fire Day** (Jan 7) | `7thData-FireDay/` | I-210 W/E + SR-134 W: ML_HV, OR, FR, FF |
| Jan 3 | `3rdData/` | I-210 + SR-134 |
| Jan 10 | `10thData/` | I-210 + SR-134 |
| Jan 17 | `17thData/` | I-210 + SR-134 |

**Data root:** `data/WSC-Pems-Data-Eaton-Fire/data-eaton/pems/eaton-corridor/`

## Run Commands

```bash
# Test corridor layout builder (no simulation, just prints config)
sbt "runMain scalation.simulation.process.config.testBuildCorridorLayout"

# Test station map coordinates
sbt "runMain scalation.simulation.process.config.testEatonCorridorCoords"

# Run EatonFireModel simulation (placeholder demand)
sbt "runMain scalation.simulation.process.model.runEatonFireModel"
```

