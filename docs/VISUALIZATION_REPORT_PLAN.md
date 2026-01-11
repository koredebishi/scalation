# Visualization and Report Plan for ANNSIM 2026 Paper

## Overview
This document outlines all graphs and tables needed for the paper, organized by micro and macro levels.

---

## Data Sources
- **Simulated Data:** `log/experiments/unoptimized_run/*_data.csv`
- **PeMS Data:** `data/Mainline_VDS_Redwood_Creek_US101-N/*.csv`
- **Fitness Files:** `log/experiments/unoptimized_run/*_fitness.txt`
- **Analysis Summary:** `log/experiments/unoptimized_run/offline_analysis_summary.txt`

---

## MACRO-LEVEL VISUALIZATIONS (Sensor Aggregates)

### 1. Flow (Vehicle Count) Time Series — 6 AM to 6 PM
**Purpose:** Compare simulated vs observed vehicle counts at each sensor  
**X-axis:** Time of day (5-min intervals, 6:00 AM - 6:00 PM)  
**Y-axis:** Vehicle count (vehicles per 5 min)  
**Lines:** PeMS Observed (solid), Simulated (dashed)  
**Subplots:** 5 sensors (1 row × 5 columns OR 5 rows × 1 column)  
**File:** `flow_timeseries_macro.png`

### 2. Speed Time Series — 6 AM to 6 PM
**Purpose:** Compare simulated vs observed speeds at each sensor  
**X-axis:** Time of day (5-min intervals)  
**Y-axis:** Speed (mph)  
**Lines:** PeMS Observed (solid), Simulated (dashed)  
**Subplots:** 5 sensors  
**File:** `speed_timeseries_macro.png`

### 3. Flow Scatter Plot (Simulated vs Observed)
**Purpose:** Show correlation between simulated and observed counts  
**X-axis:** PeMS Observed Count  
**Y-axis:** Simulated Count  
**Points:** All 5 sensors × 48 time intervals = 240 points  
**Reference Line:** y = x (perfect prediction)  
**Annotation:** R² value  
**File:** `flow_scatter_macro.png`

### 4. Speed Scatter Plot (Simulated vs Observed)
**Purpose:** Show correlation for speed  
**Same format as Flow Scatter**  
**File:** `speed_scatter_macro.png`

### 5. Fitness Comparison Bar Chart — Integrators
**Purpose:** Compare fitness across numerical integrators  
**X-axis:** Integrator (Ballistic, Euler, Heun, RK2, RK3, RK4, DOPRI5)  
**Y-axis:** Fitness (lower is better)  
**Color:** Highlight Ballistic (best)  
**File:** `fitness_by_integrator.png`

### 6. Fitness Comparison Bar Chart — Arrival Process
**Purpose:** Compare Erlang2S vs Poisson  
**X-axis:** Experiment (grouped by arrival process)  
**Y-axis:** Fitness  
**Color:** Erlang2S (green), Poisson (red)  
**File:** `fitness_by_arrival.png`

### 7. Runtime vs Fitness Scatter
**Purpose:** Show efficiency vs accuracy trade-off  
**X-axis:** Runtime (minutes)  
**Y-axis:** Fitness  
**Points:** Each integrator labeled  
**File:** `runtime_vs_fitness.png`

---

## MICRO-LEVEL VISUALIZATIONS (Lane Detail)

### 8. Flow by Lane — Heatmap
**Purpose:** Show flow prediction accuracy per sensor per lane  
**Rows:** Sensors (1-5)  
**Columns:** Lanes (1-4)  
**Color:** R² value (green = high, red = low)  
**File:** `flow_r2_heatmap_micro.png`

### 9. Speed by Lane — Heatmap
**Purpose:** Show speed prediction accuracy per sensor per lane  
**Same format as Flow heatmap**  
**File:** `speed_r2_heatmap_micro.png`

### 10. Flow Time Series by Lane (Selected Sensor)
**Purpose:** Detailed lane-level comparison for best/worst sensor  
**X-axis:** Time of day  
**Y-axis:** Vehicle count  
**Lines:** 4 lanes (different colors), PeMS vs Simulated  
**File:** `flow_timeseries_micro_sensor1.png` (repeat for each sensor)

### 11. Speed Time Series by Lane (Selected Sensor)
**Purpose:** Detailed lane-level speed comparison  
**Same format as Flow by Lane**  
**File:** `speed_timeseries_micro_sensor1.png`

---

## CONGESTION VISUALIZATIONS

### 12. Speed Contour Plot (Time-Space Diagram)
**Purpose:** Show congestion propagation along corridor  
**X-axis:** Time of day (6 AM - 6 PM)  
**Y-axis:** Sensor location (1-5, representing distance)  
**Color:** Speed (red = congested < 40 mph, green = free flow > 60 mph)  
**Two panels:** Observed vs Simulated  
**File:** `speed_contour_timespace.png`

### 13. Congestion Duration Comparison
**Purpose:** Compare how long each sensor experiences congestion  
**X-axis:** Sensor  
**Y-axis:** Minutes in congestion (speed < 40 mph)  
**Bars:** Observed vs Simulated (side by side)  
**File:** `congestion_duration.png`

---

## TABLES FOR PAPER

### Table 1: Numerical Integrator Comparison (Already Done)
| Integrator | Duration (s) | Duration (min) | Fitness |

### Table 2: Macro-Level Metrics by Sensor
| Sensor | Flow R² | Flow NRMSE | Flow SMAPE | Speed R² | Speed NRMSE | Speed SMAPE |

### Table 3: Arrival Process Comparison
| Arrival | Best Integrator | Fitness | Avg Flow R² | Avg Speed R² |

### Table 4: Micro-Level Metrics (Appendix)
| Sensor | Lane | Flow R² | Flow NRMSE | Speed R² | Speed NRMSE |

---

## DIRECTORY STRUCTURE

```
log/
└── experiments/
    └── figures/
        ├── flow_timeseries_macro.png
        ├── speed_timeseries_macro.png
        ├── flow_scatter_macro.png
        ├── speed_scatter_macro.png
        ├── fitness_by_integrator.png
        ├── fitness_by_arrival.png
        ├── runtime_vs_fitness.png
        ├── flow_r2_heatmap_micro.png
        ├── speed_r2_heatmap_micro.png
        ├── speed_contour_timespace.png
        └── congestion_duration.png
```

---

## PYTHON SCRIPTS NEEDED

1. `plot_timeseries.py` — Generate flow/speed time series (macro + micro)
2. `plot_scatter.py` — Generate scatter plots with R²
3. `plot_fitness_comparison.py` — Bar charts for integrator/arrival comparison
4. `plot_heatmaps.py` — R² heatmaps for micro-level
5. `plot_congestion.py` — Time-space diagram and congestion analysis

---

## PRIORITY ORDER

1. **Flow Time Series (Macro)** — Most important, shows model fit
2. **Speed Time Series (Macro)** — Second most important
3. **Fitness Bar Charts** — Supports key claims
4. **Scatter Plots** — Shows correlation
5. **Heatmaps** — Micro-level detail
6. **Congestion Plots** — Nice to have


