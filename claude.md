# ScalaTion Traffic Simulation - Claude Instructions

## Project Overview
Microscopic traffic simulation using ScalaTion 2.0 framework (Scala 3). Simulates US-101 corridor (CalRoute101_2 model) with IDM car-following dynamics integrated via Dormand-Prince ODE solver, validated against PeMS sensor data.

## Rules
- **Always ask permission before writing or modifying code**
- Simulation may be running on HPC - don't interrupt with code changes unless asked
- Use ScalaTion 2.0 style: Scala 3, `cfor` loops, `VectorD`/`MatrixD`
- Do not hallucinate - if unsure, say so

## Paper Status (ANNSIM 2026)
- **Deadline:** January 11, 2026
- **Template:** `docs/2026_annsim_paper_2026/bishi_annsim2026.tex`
- **Title:** "HIGHER-ORDER IDM INTEGRATION FOR MICROSCOPIC TRAFFIC SIMULATION: A DORMAND-PRINCE APPROACH WITH MULTI-SCALE VALIDATION"

### Sections Status:
- Section 1 (Introduction): ✅ Written
- Section 2 (Related Work): ✅ Outline done
- Section 3 (Methodology): ✅ Written - verified against code
- Section 4 (Experimental Setup): ✅ Written
- Section 5 (Results): ❌ BLOCKED - waiting for optimization results
- Section 6 (Conclusion): ✅ Draft done

### Paper-to-Code Verification (Methodology):
| Paper Element | Code Location | Status |
|---------------|---------------|--------|
| IDM Equation 1 | `Dynamics.scala` lines 527-530 | ✅ Verified |
| Desired gap s* Eq 2 | `Dynamics.scala` line 529 | ⚠️ Code omits max(0,...) |
| State vector y=[x,v] | `Dynamics.scala` line 399 | ✅ Verified |
| ODE system Eq 6 | `Dynamics.scala` lines 378-381 | ✅ Verified |
| Leader snapshot | `Dynamics.scala` lines 304-314 | ✅ Verified |
| Dormand-Prince | `DormandPrince.scala` integrateVV | ✅ Verified |
| Algorithm 1 | `Dynamics.scala` updateM lines 298-432 | ✅ Verified |

## Key Files

### Simulation Model
- `src/main/scala/scalation/simulation/process/example_1/CalRoute101_2.scala` - Main simulation model
- `src/main/scala/scalation/simulation/process/Vehicle.scala` - Vehicle properties and IDM parameters
- `src/main/scala/scalation/simulation/process/Dynamics.scala` - IDM/Gipps car-following models with Dormand-Prince integration
- `src/main/scala/scalation/simulation/process/VSource.scala` - Vehicle source with Erlang2S arrivals
- `src/main/scala/scalation/simulation/process/MultiVSource.scala` - Multi-lane vehicle source

### Calibration
- `src/main/scala/scalation/simulation/process/CalibrateCalRoute101.scala` - Optimization entry points (SPSA, SPSA_Mo, Nelder-Mead, GA, DE)
- `src/main/scala/scalation/simulation/process/TrafficConfig2.scala` - PeMS data loading and validation
- `src/main/scala/scalation/simulation/process/TrafficOptimization.scala` - Objective function wrapper

### ODE Solver
- `src/main/scala/scalation/dynamics/DormandPrince.scala` - DOPRI5 integrator (`integrateVV` method)
- Butcher tableau coefficients: lines 37-65
- 7-stage RK evaluation: lines 103-109

### Random Variates
- `src/main/scala/scalation/random/Variate.scala` - Contains `Erlang2S` shifted arrival distribution

## IDM Parameters
Order: `[s, amax, bmax, T, rt]`
- `s` - minimum gap (meters) - paper: $s_0$
- `amax` - max acceleration (m/s²) - paper: $a_{\max}$
- `bmax` - max deceleration (m/s², negative) - paper: $b$
- `T` - safe time headway (seconds) - paper: $T$
- `rt` - reaction time τ (seconds) - paper: $\tau$

### Known Good Starting Point
```scala
VectorD(5.0, 4.0, -2.0, 3.0, 0.5)
```

### Parameter Bounds for Optimization
```scala
val lower = VectorD(2.0, 1.5, -3.0, 1.0, 0.3)
val upper = VectorD(8.0, 6.0, -1.0, 5.0, 1.5)
```

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

## HPC (Sapelo2)

### Directories
- Home (git): `/home/krb84578/scalation_2.0`
- SPSA runner: `/scratch/krb84578/workDir/scalation_2.0`
- Nelder-Mead/MO runner: `/scratch/krb84578/workDir/scalation_MO`

### Commands
```bash
# Load Java
module load Java/21.0.5

# Compile
cd /scratch/krb84578/workDir/scalation_2.0
sbt clean package

# Submit jobs
sbatch run_SPSA.sbatch
sbatch run_NelderMead.sbatch

# Monitor
tail -f log/spsa/spsa_*.out
tail -f log/nm/nm_*.out
squeue -u krb84578

# Kill job
scancel <job_id>
```

## PeMS Data
- Location: `data/Mainline_VDS_Donald_Doyle/` (5 mainline sensors)
- Ramps: `data/Ramps_VDS_Donald_Doyle/` (2 on-ramp sensors)
- Format: 48 rows (15-min intervals, 6AM-6PM), 4 lanes per sensor
- Columns: timestamp, L1_flow, L1_speed, L2_flow, L2_speed, ...

## Fitness Function
- Metric: Normalized RMSE (NRMSE) - scale invariant
- Formula: `0.5 * avgCountNRMSE + 0.5 * avgSpeedNRMSE`
- Lower is better

## Known Issues
- `ERROR @ Variate.init: parameter tau must be less than mu` - Expected from ramp Erlang2S, not a problem
- `Skipping Source actor due to time limit` - Happens near end of simulation window, normal
- Sensor 4 lanes 2-3 have anomalous PeMS data - flagged in validation

## AI Disclosure (Required for ANNSIM)
Must disclose in acknowledgements per SCS policy:
> "We used Claude to assist with drafting portions of Section 3 (Methodology) and structuring the manuscript."

