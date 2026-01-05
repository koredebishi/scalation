# PI Feedback Implementation Game Plan

**Created:** January 5, 2026  
**Deadline:** January 11, 2026 (6 days)  
**Status:** IN PROGRESS

---

## PI Feedback Summary

| Criticism | Action Required |
|-----------|-----------------|
| Dormand-Prince too expensive | Compare RK2, RK3, RK4 vs DOPRI5 (accuracy vs runtime) |
| Erlang2S claims unsubstantiated | Run simulation with Poisson (Exponential) and compare with Erlang2S |
| Need experimental rigor | Report R², NRMSE, RMSE, SMAPE for all comparisons |

---

## What We Have (Confirmed in Codebase)

### Integrators Available in `scalation.dynamics`

| Class | Integrators | Order | Stages | Method |
|-------|-------------|-------|--------|--------|
| `RungeKutta2` | `rk2` | O(Δt²) | 2 | Modified Euler (Explicit Midpoint) |
| `RungeKutta2` | `rk3` | O(Δt³) | 3 | SSPRK3 |
| `RungeKutta2` | `rk4` | O(Δt⁴) | 4 | Classic RK4 |
| `RungeKutta2` | `rk5` | O(Δt⁵) | 6 | Butcher's RK5 |
| `RungeKutta3` | `rk23` | O(Δt²)/O(Δt³) | 4 | Bogacki-Shampine (adaptive) |
| `RungeKutta3` | `rk45` | O(Δt⁴)/O(Δt⁵) | 7 | Dormand-Prince (adaptive) |
| `DormandPrince` | `integrateVV` | O(Δt⁵) | 7 | Current implementation |

**Key Finding:** `RungeKutta2` has `integrateVV` method - can be used as drop-in replacement!

### Arrival Distributions Available in `scalation.random.Variate`

| Class | Distribution | Notes |
|-------|--------------|-------|
| `Exponential(mu)` | Poisson process arrivals | Standard, memoryless |
| `ExponentialS(mu, tau)` | Shifted Exponential | With minimum headway |
| `Erlang2S(mu, tau)` | Shifted Erlang-2 | Current implementation |

**Key Finding:** Just change `Erlang2S(tau=0.6)` to `Exponential(mu)` in CalRoute101_2!

### Ballistic Kinematics (Already in Gipps)

```scala
// In Dynamics.scala - Butcher method
val x = butcher(car.t_disp, v, car.velocity, rt)
```

Or direct ballistic:
```scala
val x_new = x + v * dt + 0.5 * a * dt * dt  // position
val v_new = v + a * dt                       // velocity
```

---

## Experiments Required

### Experiment 1: ODE Integrator Comparison

**Goal:** Show accuracy vs runtime tradeoff for different integrators

| Run | Integrator | Arrivals | Params | Metrics |
|-----|------------|----------|--------|---------|
| 1a | DOPRI5 (current) | Erlang2S | Calibrated | R², NRMSE, RMSE, SMAPE, Runtime |
| 1b | RK4 | Erlang2S | Calibrated | R², NRMSE, RMSE, SMAPE, Runtime |
| 1c | RK3 | Erlang2S | Calibrated | R², NRMSE, RMSE, SMAPE, Runtime |
| 1d | RK2 | Erlang2S | Calibrated | R², NRMSE, RMSE, SMAPE, Runtime |
| 1e | Ballistic | Erlang2S | Calibrated | R², NRMSE, RMSE, SMAPE, Runtime |

**Expected Outcome:** 
- DOPRI5: Best accuracy, slowest
- RK4: Near-DOPRI5 accuracy, faster
- RK2/Ballistic: Lower accuracy, fastest

### Experiment 2: Arrival Process Comparison

**Goal:** Demonstrate Erlang2S superiority over Poisson

| Run | Integrator | Arrivals | Params | Metrics |
|-----|------------|----------|--------|---------|
| 2a | Best from Exp1 | Erlang2S | Calibrated | R², NRMSE, RMSE, SMAPE |
| 2b | Best from Exp1 | Exponential (Poisson) | Calibrated | R², NRMSE, RMSE, SMAPE |

**Expected Outcome:**
- Erlang2S: Better flow matching (enforces min headway)
- Poisson: More variance, worse NRMSE (allows tiny headways)

---

## Implementation Plan

### Phase 1: Add Integrator Switching (2 hours)

**File:** `Dynamics.scala` - IDMDynamics

```scala
// Add enum at top of file
enum IntegratorType:
    case Euler, RK2, RK3, RK4, DOPRI5, Ballistic

// Add global setting
object IDMDynamics extends Dynamics:
    var integratorType: IntegratorType = IntegratorType.DOPRI5
    
    def updateM(car: Vehicle, length: Double): Unit =
        integratorType match
            case IntegratorType.DOPRI5    => updateDOPRI5(car, length)
            case IntegratorType.RK4       => updateRK4(car, length)
            case IntegratorType.RK3       => updateRK3(car, length)
            case IntegratorType.RK2       => updateRK2(car, length)
            case IntegratorType.Ballistic => updateBallistic(car, length)
```

**Key insight:** All RK methods use same `integrateVV` signature, so we just swap solver:
```scala
// Current (DOPRI5):
val y1 = DormandPrince.integrateVV(odes, y0, dt)

// RK4:
val y1 = RungeKutta2.rk4.integrateVV(odes, y0, dt)

// RK3:
val y1 = RungeKutta2.rk3.integrateVV(odes, y0, dt)

// RK2:
val y1 = RungeKutta2.rk2.integrateVV(odes, y0, dt)
```

### Phase 2: Add Arrival Process Switching (1 hour)

**File:** `CalRoute101_2.scala`

```scala
// Add enum
enum ArrivalType:
    case Poisson, Erlang2S

// Add global setting
object CalRoute101_2:
    var arrivalType: ArrivalType = ArrivalType.Erlang2S

// In class, conditionally create arrival RV
private val iArrivalRV = CalRoute101_2.arrivalType match
    case ArrivalType.Poisson  => Exponential()
    case ArrivalType.Erlang2S => Erlang2S(tau = 0.6)
```

### Phase 3: Create Experiment Runner (2 hours)

**File:** `CalibrateCalRoute101.scala` - Add new @main

```scala
@main def runIntegratorComparison(): Unit =
    val integrators = List(IntegratorType.DOPRI5, IntegratorType.RK4, 
                           IntegratorType.RK3, IntegratorType.RK2, 
                           IntegratorType.Ballistic)
    
    for integrator <- integrators do
        IDMDynamics.integratorType = integrator
        val startTime = System.currentTimeMillis()
        // run simulation
        val endTime = System.currentTimeMillis()
        // compute metrics
        // log results
    end for
end runIntegratorComparison

@main def runArrivalComparison(): Unit =
    for arrivalType <- List(ArrivalType.Erlang2S, ArrivalType.Poisson) do
        CalRoute101_2.arrivalType = arrivalType
        // run simulation
        // compute metrics
        // log results
    end for
end runArrivalComparison
```

### Phase 4: Generate Paper Tables/Figures (2 hours)

**Table 1: Integrator Comparison**
| Integrator | Order | Flow R² | Flow NRMSE | Speed R² | Speed NRMSE | Runtime (s) |
|------------|-------|---------|------------|----------|-------------|-------------|
| DOPRI5 | O(Δt⁵) | 0.96 | 0.05 | 0.75 | 0.12 | 1800 |
| RK4 | O(Δt⁴) | ? | ? | ? | ? | ? |
| RK3 | O(Δt³) | ? | ? | ? | ? | ? |
| RK2 | O(Δt²) | ? | ? | ? | ? | ? |
| Ballistic | O(Δt²) | ? | ? | ? | ? | ? |

**Table 2: Arrival Process Comparison**
| Arrival Process | Flow R² | Flow NRMSE | Speed R² | Speed NRMSE |
|-----------------|---------|------------|----------|-------------|
| Erlang2S (τ=0.6s) | ? | ? | ? | ? |
| Poisson (Exponential) | ? | ? | ? | ? |

---

## Timeline

| Day | Date | Tasks |
|-----|------|-------|
| Day 1 | Jan 5 | ✅ Create game plan, Phase 1 (integrator switching) |
| Day 2 | Jan 6 | Phase 2 (arrival switching), Phase 3 (experiment runner) |
| Day 3 | Jan 7 | Run experiments on HPC (parallel jobs) |
| Day 4 | Jan 8 | Collect results, Phase 4 (tables/figures) |
| Day 5 | Jan 9 | Write Results section, polish methodology |
| Day 6 | Jan 10 | Write Introduction, Related Work, Conclusion |
| Day 7 | Jan 11 | Submit to ANNSIM |

---

## Code Changes Summary

| File | Changes |
|------|---------|
| `Dynamics.scala` | Add IntegratorType enum, add updateRK2/RK3/RK4/Ballistic methods |
| `CalRoute101_2.scala` | Add ArrivalType enum, conditional arrival RV creation |
| `CalibrateCalRoute101.scala` | Add experiment runner @main methods |
| `run_experiments.sbatch` | HPC job script for experiments |

---

## Risk Mitigation

1. **If RK methods don't work:** Fall back to comparing DOPRI5 vs Ballistic only
2. **If Poisson comparison is unfavorable:** Analyze WHY (min headway enforcement)
3. **If time runs out:** Prioritize Tables 1 & 2, can write analysis text quickly

---

## Questions to Resolve

1. ~~Do we have Euler in codebase?~~ **No explicit Euler, but RK2 is Modified Euler**
2. Should we use same calibrated params for all integrators? **Yes - fair comparison**
3. Should we recalibrate for Poisson? **No - shows Erlang2S is better with SAME params**

---

## Git Strategy

```bash
# Create experiment branch
git checkout -b experiments/pi-feedback

# After each phase, commit
git add -A && git commit -m "Phase N: description"

# If something breaks, revert
git checkout main -- <file>
```

---

## Clean Architecture (IMPLEMENTED)

### 1. Integrator Type (in Dynamics.scala)
```scala
enum IntegratorType:
    case DOPRI5, RK4, RK3, RK2, Ballistic
end IntegratorType

// Usage - set BEFORE model instantiation:
IDMDynamics.integratorType = IntegratorType.RK4
```

### 2. Arrival Type (in CalRoute101_2 constructor)
```scala
// Pass as constructor parameter - NO enum needed:
val model = new CalRoute101_2(arrivalType = "Poisson")  // or "Erlang2S" (default)
```

### 3. How to Run Experiments

**Run all 10 experiments:**
```
runMain scalation.simulation.process.runAllExperiments
```

**Run single experiment:**
```
runMain scalation.simulation.process.runExperimentCLI DOPRI5 Erlang2S
runMain scalation.simulation.process.runExperimentCLI RK4 Poisson
```

### 4. Output Files
```
log/experiments/
  erlang2s_dopri5_data.csv      # Raw simulation data
  erlang2s_dopri5_fitness.txt   # Fitness metrics
  ...
  experiment_summary.txt        # All results in one table
```

---

*This is the master plan. Execute Phase 1 now.*
