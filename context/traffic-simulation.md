# Traffic Simulation Domain Knowledge

## IDM Parameters

Order: `[s, amax, bmax, T, rt]`

| Parameter | Symbol | Meaning | Unit |
|-----------|--------|---------|------|
| `s` | $s_0$ | minimum gap | meters |
| `amax` | $a_{\max}$ | max acceleration | m/s² |
| `bmax` | $b$ | max deceleration (negative) | m/s² |
| `T` | $T$ | safe time headway | seconds |
| `rt` | $\tau$ | reaction time | seconds |

### Known Good Starting Point
```scala
VectorD(5.0, 4.0, -2.0, 3.0, 0.5)
```

### Parameter Bounds for Optimization
```scala
val lower = VectorD(2.0, 1.5, -3.0, 1.0, 0.3)
val upper = VectorD(8.0, 6.0, -1.0, 5.0, 1.5)
```

## Car-Following Models

| Model | Type | Key Feature |
|-------|------|-------------|
| IDM | Acceleration-based | Continuous, ODE-derived |
| Gipps | Velocity-based | Discrete, deterministic |
| Krauss | Velocity-based | Discrete, stochastic (σ=0.5) |

**Integrator:** Ballistic for all models (ANNSIM 2026 showed no significant difference vs DOPRI5)

## PeMS Data

### US-101 (CalRoute101_3)
- **Location:** `data/Mainline_VDS_Donald_Doyle/` (5 mainline sensors)
- **Ramps:** `data/Ramps_VDS_Donald_Doyle/` (2 on-ramp sensors)
- **Format:** 48 rows (15-min intervals, 6AM-6PM), 4 lanes per sensor
- **Columns:** timestamp, L1_flow, L1_speed, L2_flow, L2_speed, ...
- **Demand config:** `PeMSDemand.US101_DonaldDoyle()` (default window/layout)

### I-210 / SR-134 (EatonFireModel)
- **Location:** `data/WSC-Pems-Data-Eaton-Fire/data-eaton/pems/eaton-corridor/`
- **Mainline:** Cleaned anchor sensor CSV (`717653-i210-firstSensor-baseline.csv`)
- **Ramps:** Aggregated OR CSVs (22 I-210 ramps, 7 SR-134 ramps)
- **Format:** 73 rows (5-min intervals, 17:00–23:00 inclusive), 5 lanes
- **Demand config:** `PeMSDemand.I210_WB_Anchor()` (mainline), `AggregatedDemand.I210_WB_Baseline` (ramps)

## Fitness Function

- **Metric:** Normalized RMSE (NRMSE) - scale invariant
- **Formula:** `0.5 * avgCountNRMSE + 0.5 * avgSpeedNRMSE`
- **Lower is better**

## Calibration Insight — Flow vs Speed Separation

- **tau (arrival process headway)** controls flow and vehicle spacing. Because tau is fitted directly from PeMS data (data-driven arrival process), flow accuracy is already handled before calibration begins.
- **IDM parameters** (T, amax, bmax, s, rt) govern speed dynamics only. Calibration improves speed prediction but does not materially improve flow.
- **Implication:** Calibrating tau separately for flow is theoretically possible but not a research priority. The structural separation between arrival process (flow) and car-following (speed) is a key finding from Study 1 and a supporting result of Study 2.
- **Calibration is a supporting result, not a standalone paper.** The PI is not interested in calibration as a paper contribution.

## Known Issues

| Issue | Explanation |
|-------|-------------|
| `ERROR @ Variate.init: parameter tau must be less than mu` | Expected from ramp Erlang2S, not a problem |
| `Skipping Source actor due to time limit` | Happens near end of simulation window, normal |
| Sensor 4 lanes 2-3 anomalous | PeMS data quality issue, flagged in validation |

## Eaton Fire Model - FF Connector Data

### Ghost FF Stations (No PeMS Data)
| Station | Freeway | PM | Location | Status |
|---------|---------|-----|----------|--------|
| 775725 | I-210 W | 24.49 | WB 210 CON | Ghost - kept in station_map for coordinates only |
| 773979 | SR-134 W | 9.213 | OAKVIEW | Ghost - Route 2 connector, no flow data |
| 774057 | SR-134 W | 8.403 | FM 2 SB TO 134 | Ghost - Route 2 connector, no flow data |

### Removed FF Stations
| Station | Reason |
|---------|--------|
| 770388 | Feeds Eastbound I-210, not relevant for WB model |
| 773980 | Duplicate of 773979 (OAKVIEW 1-lane HOV) |

### Split Ratio Calculation (Data-Driven Alternative)
Since station 775725 (FF diverge) is a ghost, use ML stations:

```
Upstream (all traffic):  717634 (LAKE 1, PM 26.12, 5 lanes)
FF merge on SR-134:      717603 (ORANGE GROVE, PM 12.763, 5 lanes)

splitRatio = Flow_717603 / Flow_717634
```

**Note:** SR-134 WB has NO mainline VSources - all mainline flow at ORANGE GROVE (717603) represents FF traffic from I-210.

### FF CSV Files (All Moved to Unused)
| Folder | Files Moved | Reason |
|--------|-------------|--------|
| eaton-i210 (5 folders) | `*_FF.csv` | Ghost 775725, irrelevant 770388 |
| eaton-134 (5 folders) | `*_FF.csv` | Route 2 connectors (773979, 774057) covered by OR data |

**Total: 14 FF files moved to Unused/** - No FF CSV files needed for model ingestion.

## Eaton Fire Model — I-210 / SR-134 PeMS Data

### I-210 Data Format
- **Location:** `data/WSC-Pems-Data-Eaton-Fire/data-eaton/pems/eaton-corridor/`
- **Anchor sensor:** 717653 (COLORADO 2, PM 29.879, 5 ML lanes)
- **Baseline CSV:** `717653-i210-firstSensor-baseline.csv` — cleaned, no header
- **Format:** 73 rows (5-min intervals, 17:00–23:00 inclusive), 13 columns
- **Columns:** `Timestamp,TotalFlow,AvgSpeed,L1Flow,L1Speed,...,L5Flow,L5Speed`
- **ColumnLayout:** `I210_MainlineLayout` — `flowCols=VectorI(3,5,7,9,11)`, `speedCols=VectorI(4,6,8,10,12)`
- **TimeWindow:** `startRow=0, endRow=73 (exclusive), binSeconds=300`
- **Demand config:** `PeMSDemand.I210_WB_Anchor()` — same loading pattern as CalRoute101_3

### Verified nStop values (baseline, synthetic=false)
```
I-210 Mainline (5 lanes): L0=5326, L1=5174, L2=4019, L3=3281, L4=1873
I-210 Ramps (22): 0,2234,2165,1960,3522,0,0,0,0,0,744,0,0,0,1539,549,0,2315,0,5456,53,0
SR-134 Ramps (7): all zero (data quality issue — sensors not reporting)
```

### SR-134 Data Format
- Same 5-min bins, same column structure
- **ALL 7 on-ramp sensors report zero flow** across all time bins (entire OR CSV = 0) — persistent PeMS outage
- HOV data on fire day is PeMS-imputed (matches baseline exactly) — flagged, do not use

### RowTimeLoader Interface
Models implementing `RowTimeLoader` (CalRoute101_3, EatonFireModel) provide:
- `getMuForSource(subtype)(row)` — inter-arrival time from ArrivalSource objects
- `getSpeedMatrix()` — `MatrixD` of lane speeds from anchor CSV
- `getCurrentRow(clock)` — maps sim clock to PeMS time-bin row index
- `getDataDimension` — number of rows in the data
- `nextRow(clock)` — advance internal row pointer if needed
- `rowTime` / `rowTimeSlice` — bin width in sim seconds (300 for I-210, 900 for US-101)
