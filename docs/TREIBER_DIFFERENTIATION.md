# Differentiation from Treiber & Kanagaraj (2015)

## Paper Framing

> *"Treiber and Kanagaraj (2015) established that Heun's method provides optimal accuracy-efficiency trade-off for car-following integration. However, their analysis used decoupled position-velocity updates on synthetic single-vehicle trajectories. We extend this work in three directions: (1) coupled ODE formulation that respects the simultaneous dependence of IDM acceleration on position and velocity, (2) evaluation through calibration fitness rather than trajectory error, using real PeMS sensor data, and (3) analysis of arrival process effects—a dimension absent from prior integration studies. Our results reveal that while integrator order has minimal impact (< 1% fitness difference), arrival process selection dominates calibration quality, with Shifted Erlang-2 achieving 11% lower NRMSE than Poisson arrivals."*

---

## Treiber & Kanagaraj (2015) - "Comparing numerical integration schemes for time-continuous car-following models"

### What They Compared:

| Integrator | Order | Their Finding |
|------------|-------|---------------|
| **Euler** | O(Δt¹) | Poor - not recommended |
| **Heun** | O(Δt²) | **Best trade-off** - recommended |
| **RK4** | O(Δt⁴) | Diminishing returns due to model discontinuities |
| **Ballistic** | O(Δt²) | Good for constant acceleration phases |

**4 integrators total** - and they recommended Heun as optimal.

### Their Key Findings:

1. **Higher-order ≠ better accuracy** for car-following because:
   - Braking events create discontinuities
   - Lane changes break smoothness assumptions
   - Error dominated by model events, not integration order

2. **Step size matters more** than integration order for practical accuracy

3. **They did NOT vary arrival processes** - focused purely on integration

---

## Differentiation Angles

### 1. **Coupled ODE Formulation** (Major Contribution)

Treiber (2015) integrated position and velocity **separately**:
```
x(t+τ) = x(t) + v(t)·τ           // position update
v(t+τ) = v(t) + a(t)·τ           // velocity update (decoupled)
```

**Our approach** uses a **coupled ODE system**:
```scala
val odes: Array[DerivativeV] = Array(
    (t, y) => y(1),                    // dx/dt = v
    (t, y) => idmAccel(y(0), y(1))     // dv/dt = a(x, v)  ← acceleration depends on BOTH
)
```

This is fundamentally different because:
- IDM acceleration depends on **gap** (`x_leader - x_n`), which changes as x changes
- Decoupled updates ignore this coupling within a timestep
- Our coupled formulation captures the **simultaneous** evolution of (x, v)

---

### 2. **Calibration-Focused Evaluation** (Different Goal)

| Treiber (2015) | Our Work |
|----------------|-----------|
| Evaluated **trajectory accuracy** (how close is simulated path to ground truth?) | Evaluated **calibration fitness** (how well do aggregated metrics match sensors?) |
| Used synthetic trajectories | Used **real PeMS sensor data** |
| Goal: numerical accuracy | Goal: **parameter estimation quality** |

This is a different research question:
> *"Does integrator choice affect the optimizer's ability to find good parameters?"*

---

### 3. **Discrete-Event + Continuous Dynamics Hybrid**

Treiber (2015) focused on pure continuous simulation. Our work integrates:
- **Discrete-event simulation** (ScalaTion process model, vehicle arrivals)
- **Continuous ODE integration** (within each timestep)
- **Stochastic arrivals** (Poisson, Shifted Erlang-2)

This is a **hybrid simulation** approach rarely studied in traffic literature.

---

### 4. **Multi-Segment Network** (Not Just Single Road)

Treiber's experiments used single-lane, single-segment scenarios. Our CalRoute101 model has:
- 5 mainline VDS sensors
- Multiple segments with transitions
- Ramp merging (VSource, Ramp classes)

This introduces **boundary effects** that affect integrator behavior differently.

---

### 5. **Step Size Context**

Treiber (2015) varied step size (Δt = 0.1s to 1.0s) to study error accumulation.

Our work uses **fixed reaction time** (`rt = 0.5s`) as the integration step, which is:
- Realistic (human reaction time)
- Larger than typical ODE solver steps
- Tests integrators under "practical" conditions, not ideal small-step scenarios

---

## Summary Comparison Table

| Dimension | Treiber & Kanagaraj (2015) | Our Work |
|-----------|---------------------------|-----------|
| **Integrators** | 4 (Euler, Heun, RK4, Ballistic) | **8** (Euler, RK2, Heun, RK3, RK4, DOPRI5, Butcher, Ballistic) |
| **ODE Formulation** | Decoupled (x, v updated separately) | **Coupled** (simultaneous ODE system) |
| **Arrival Process** | Not studied | **Poisson vs. Shifted Erlang-2** |
| **Validation Data** | Synthetic trajectories | **Real PeMS sensors** |
| **Goal** | Trajectory accuracy | **Calibration fitness (NRMSE, R²)** |
| **Simulation Type** | Pure continuous | **Hybrid discrete-event + continuous** |
| **Network** | Single segment | **Multi-segment with ramps** |
| **Step Size** | Variable (0.1s - 1.0s) | Fixed rt = 0.5s (realistic) |

---

## Full Integrator Inventory (ScalaTion)

| IntegratorType | Method | Order | Citation/Notes |
|----------------|--------|-------|----------------|
| `Euler` | Forward Euler | O(Δt¹) | SUMO default |
| `RK2` | Modified Euler (Midpoint) | O(Δt²) | |
| `Heun` | Explicit Trapezoidal | O(Δt²) | Treiber & Kanagaraj (2015) recommended |
| `RK3` | SSPRK3 | O(Δt³) | |
| `RK4` | Classic RK4 | O(Δt⁴) | |
| `DOPRI5` | Dormand-Prince adaptive | O(Δt⁵) | |
| `butcher` | Butcher's quadrature (2x) | O(Δt⁵) | J.C. Butcher |
| `Ballistic` | Kinematic equations | O(Δt²) | |

---

## References

- Treiber, M., & Kanagaraj, V. (2015). Comparing numerical integration schemes for time-continuous car-following models. *Physica A: Statistical Mechanics and its Applications*, 419, 183-195.
- Butcher, J.C. (2016). *Numerical Methods for Ordinary Differential Equations* (3rd ed.). Wiley.
- Kesting, A., & Treiber, M. (2008). Calibrating car-following models by using trajectory data: Methodological study. *Transportation Research Record*, 2088(1), 148-156.

