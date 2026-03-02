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

- **Location:** `data/Mainline_VDS_Donald_Doyle/` (5 mainline sensors)
- **Ramps:** `data/Ramps_VDS_Donald_Doyle/` (2 on-ramp sensors)
- **Format:** 48 rows (15-min intervals, 6AM-6PM), 4 lanes per sensor
- **Columns:** timestamp, L1_flow, L1_speed, L2_flow, L2_speed, ...

## Fitness Function

- **Metric:** Normalized RMSE (NRMSE) - scale invariant
- **Formula:** `0.5 * avgCountNRMSE + 0.5 * avgSpeedNRMSE`
- **Lower is better**

## Known Issues

| Issue | Explanation |
|-------|-------------|
| `ERROR @ Variate.init: parameter tau must be less than mu` | Expected from ramp Erlang2S, not a problem |
| `Skipping Source actor due to time limit` | Happens near end of simulation window, normal |
| Sensor 4 lanes 2-3 anomalous | PeMS data quality issue, flagged in validation |

