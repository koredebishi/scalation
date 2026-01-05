# Experiment Game Plan for ANNSIM Paper

## PI Feedback Requirements
1. **ODE Integrator Comparison**: Test DOPRI5 vs RK4 vs Ballistic with Erlang2S arrivals
2. **Arrival Process Comparison**: Test Erlang2S vs Poisson with DOPRI5 integrator

## Experimental Matrix (4 experiments total)

| # | Arrival Process | Integrator | Purpose |
|---|-----------------|------------|---------|
| 1 | Erlang2S | DOPRI5 | **Baseline** (current best) |
| 2 | Erlang2S | RK4 | Show accuracy vs speed tradeoff |
| 3 | Erlang2S | Ballistic | Show simple method performance |
| 4 | Poisson | DOPRI5 | Show Erlang2S wins on headway |

## Implementation Status

### Phase 1: Integrator Switching ✅ COMPLETE
- Added `IntegratorType` enum in `Dynamics.scala`
  - DOPRI5, RK4, RK3, RK2, Ballistic
- Added `IDMDynamics.integratorType` variable
- Modified `updateM` to switch integrator based on setting

### Phase 2: Arrival Switching ✅ COMPLETE
- Added `ArrivalType` enum in `Dynamics.scala`
  - Erlang2S, Poisson
- Added `IDMDynamics.arrivalType` variable
- Modified `CalRoute101_2.scala` to switch arrival process based on setting

### Phase 3: Experiment Runners ✅ COMPLETE
- Added `runExperiment(integrator, arrival, params)` function
- Added `runAllExperiments` main - runs all 4 experiments sequentially
- Added `runSingleExperiment` main - for HPC job array

## How to Run Experiments

### Local (all 4 experiments sequentially):
```bash
sbt "runMain scalation.simulation.process.runAllExperiments"
```

### HPC (individual experiments):
```bash
# Experiment 1: Baseline
sbt "runMain scalation.simulation.process.runSingleExperiment DOPRI5 Erlang2S"

# Experiment 2: RK4 comparison
sbt "runMain scalation.simulation.process.runSingleExperiment RK4 Erlang2S"

# Experiment 3: Ballistic comparison
sbt "runMain scalation.simulation.process.runSingleExperiment Ballistic Erlang2S"

# Experiment 4: Poisson comparison
sbt "runMain scalation.simulation.process.runSingleExperiment DOPRI5 Poisson"
```

## Expected Output
- Fitness (NRMSE) for each configuration
- Summary table comparing all configurations
- Lower fitness = better

## Paper Claims to Validate
1. DOPRI5 provides better micro-level speed accuracy than lower-order methods
2. Erlang2S produces more realistic flow patterns than Poisson arrivals
3. Combined approach (DOPRI5 + Erlang2S) achieves best overall fitness

