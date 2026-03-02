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
| `src/main/scala/scalation/simulation/process/Vehicle.scala` | Vehicle properties and CF parameters |
| `src/main/scala/scalation/simulation/process/Dynamics.scala` | CF models: IDM, Gipps, Krauss |
| `src/main/scala/scalation/simulation/process/VSource.scala` | Vehicle source with Erlang2S arrivals |
| `src/main/scala/scalation/simulation/process/MultiVSource.scala` | Multi-lane vehicle source |

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

