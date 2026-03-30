# ScalaTion Style Guide

## Language & Framework
- Scala 3 with ScalaTion 2.0
- Use `cfor` loops for performance
- Use `VectorD` and `MatrixD` for numerical work

## Key Files

### Simulation Model
| File | Purpose |
|------|---------|
| `src/main/scala/scalation/simulation/process/example_1/CalRoute101_2.scala` | Main simulation model |
| `src/main/scala/scalation/simulation/process/model/CalRoute101_3.scala` | Config-driven US-101 model |
| `src/main/scala/scalation/simulation/process/model/EatonFireModel.scala` | I-210 + SR-134 dual-corridor Eaton fire model |
| `src/main/scala/scalation/simulation/process/Vehicle.scala` | Vehicle properties and CF parameters |
| `src/main/scala/scalation/simulation/process/Dynamics.scala` | CF models: IDM, Gipps, Krauss |
| `src/main/scala/scalation/simulation/process/VSource.scala` | Vehicle source with Erlang2S arrivals |
| `src/main/scala/scalation/simulation/process/MultiVSource.scala` | Multi-lane vehicle source |
| `src/main/scala/scalation/simulation/process/RowTimeLoader.scala` | Trait for time-varying mu/speed; companion has helpers |

### Config Layer (corridor-agnostic)
| File | Purpose |
|------|---------|
| `src/main/scala/scalation/simulation/process/config/PeMSDataLoader.scala` | Generic CSV loader with ColumnLayout/TimeWindow |
| `src/main/scala/scalation/simulation/process/config/PeMSDataHelper.scala` | Corridor-agnostic wrapper over PeMSDataLoader |
| `src/main/scala/scalation/simulation/process/config/DemandConfig.scala` | PeMSDemand + AggregatedDemand configs |
| `src/main/scala/scalation/simulation/process/config/EatonCorridorConfig.scala` | Station map → CorridorLayout for I-210/SR-134 |
| `src/main/scala/scalation/simulation/process/config/MultiCorridorConfig.scala` | Top-level network config (EatonFire_WB) |
| `src/main/scala/scalation/simulation/process/config/NetworkConfig.scala` | MainlineSpec, RampSpec, SensorSpec |

### Builder & Arrival
| File | Purpose |
|------|---------|
| `src/main/scala/scalation/simulation/process/builder/TrafficModelBuilder.scala` | Builder for single-corridor models (CalRoute101_3) |
| `src/main/scala/scalation/simulation/process/builder/CorridorBuilder.scala` | Topology builder from CorridorLayout (EatonFireModel) |
| `src/main/scala/scalation/simulation/process/arrival/ArrivalSource.scala` | `PeMSArrivalSource` (per-sensor CSV), `AggregatedArrivalSource` (multi-station CSV), `AggregatedRampArrivalSource`, `SyntheticArrivalSource`, factory methods |

### Data Flow: Mainline Arrivals (CalRoute101_3 & EatonFireModel)
```
PeMSDemand (anchorFile, dataDir, window, layout)
  → ArrivalSource.allSources(demand, nLanes)
    → PeMSArrivalSource(demand.mainline, demand, laneIdx, nLanes, demand.window.binSeconds)
      → PeMSDataHelper.loadMainlineSensor(demand, 0, demand.window, demand.layout)
        → PeMSDataLoader.loadMainlineSensor(filePath, window, layout)
          → MatrixD.load(filePath, skip, skipCol, stop)
```

### Data Flow: Eaton Ramp Arrivals
```
AggregatedDemand (dataDir, orFile, startTime)
  → AggregatedRampArrivalSource(demand, rampIdx, rowTime)
    → parseRampCSV(demand, orFile)  [cached]
      → sorted by PM descending (WB: entry→exit order)
```

### Calibration
| File | Purpose |
|------|---------|
| `src/main/scala/scalation/simulation/process/CalibrateCalRoute101.scala` | Optimization entry points |
| `src/main/scala/scalation/simulation/process/TrafficConfig2.scala` | PeMS data loading |
| `src/main/scala/scalation/simulation/process/TrafficOptimization.scala` | Objective function wrapper |

### ODE Solver
| File | Purpose |
|------|---------|
| `src/main/scala/scalation/dynamics/DormandPrince.scala` | DOPRI5 integrator |

## Code-to-Paper Variable Mapping

| Code Variable | Paper Symbol | Meaning |
|---------------|--------------|---------|
| `car.t_disp` | $x_n$ | cumulative displacement from origin |
| `car.velocity` | $v_n$ | current velocity |
| `car.vmax` | $v_{\max}$ | desired/maximum velocity |
| `amax` | $a_{\max}$ | maximum acceleration |
| `bmax` | $b$ | comfortable deceleration (negative in code) |
| `s` | $s_0$ | minimum gap |
| `T` | $T$ | desired time headway |
| `rt` | $\tau$ | reaction time |
| `del` | $\delta$ | acceleration exponent (typically 4) |
