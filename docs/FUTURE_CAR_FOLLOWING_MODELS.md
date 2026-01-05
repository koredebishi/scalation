# Future Work: Additional Car-Following Models and Integrator Architecture

**Created:** January 5, 2026  
**Status:** DEFERRED - Post-paper submission  
**Priority:** Low (after ANNSIM 2026 deadline)

---

## Overview

This document captures the discussion on adding Krauss, Newell, and AV/CACC car-following models to ScalaTion, along with a proposed integrator abstraction architecture.

---

## Proposed New Models

### 1. Krauss Model (SUMO-style)
- Discrete-time, stochastic
- Safe-speed formulation with noise
- Reference: Krauß (1998)

### 2. Newell Model
- Kinematic, shockwave-consistent
- Simplified car-following (no acceleration computation)
- Reference: Newell (2002)

### 3. AV/CACC Model
- Cooperative Adaptive Cruise Control
- Uses leader acceleration (V2V communication)
- Gains: k1 (spacing), k2 (speed difference), k3 (leader acceleration)

---

## Current Integrators in ScalaTion

| Integrator | Order | Stages | Speed | Current Usage |
|------------|-------|--------|-------|---------------|
| Euler (manual) | O(Δt) | 1 | Fastest | None |
| Butcher (RK2) | O(Δt²) | 2 | Fast | Gipps |
| RungeKutta (RK4) | O(Δt⁴) | 4 | Medium | Available |
| DormandPrince (RK45) | O(Δt⁵) | 7 | Slow | IDM |

---

## Proposed Architecture: Option C (Recommended)

Separate car-following models from integrators for clean composition.

### CarFollowingModel Trait
```scala
trait CarFollowingModel:
    def computeAcceleration(car: Vehicle, leader: Vehicle, dt: Double): Double
```

### Integrator Trait
```scala
trait Integrator:
    def integrate(car: Vehicle, accelFunc: (Double, Double) => Double, dt: Double): (Double, Double)
```

### Implementations
```scala
object IDM extends CarFollowingModel:
    def computeAcceleration(...): Double = // IDM formula

object Gipps extends CarFollowingModel:
    def computeAcceleration(...): Double = // Gipps formula

object EulerIntegrator extends Integrator:
    def integrate(...): (Double, Double) = // Euler update

object DormandPrinceIntegrator extends Integrator:
    def integrate(...): (Double, Double) = // DOPRI5 update
```

### Composition
```scala
object Dynamics:
    var cfModel: CarFollowingModel = IDM
    var integrator: Integrator = DormandPrinceIntegrator
    
    def updateM(car: Vehicle, length: Double): Unit =
        val (x_new, v_new) = integrator.integrate(car, cfModel.computeAcceleration, dt)
        // commit state...
```

**Benefits:**
- Add new CF model = implement one method
- Add new integrator = implement one class
- Mix and match: IDM+Euler, Gipps+RK4, etc.

---

## Junior Code Review Issues (For Reference)

### Critical Issues Found in Junior's Implementation:
1. Parameter access: `private val s0 = len` captures default at class load, not per-vehicle
2. Krauss formula wrong: `v_p + gap/dt` is not the correct safe speed
3. Krauss position update uses old velocity instead of new
4. AVDynamics: `T` undefined in scope
5. NewellDynamics: missing `o_acc` and `acc` state updates

### Correct Krauss Safe Speed Formula:
```scala
val tau = dt
val b = abs(bmax)
val v_safe = 
    if gap <= 0.0 then 0.0
    else {
        val discriminant = b * b * tau * tau + v_p * v_p + 2.0 * b * gap
        if discriminant < 0 then 0.0
        else -b * tau + sqrt(discriminant)
    }
```

---

## Questions to Resolve Later

1. Which integrator for which model?
   - IDM: DormandPrince vs RK4 vs Butcher?
   - Gipps: Keep Butcher?
   - Krauss: Euler (discrete-time by design)?
   - Newell: Euler (kinematic)?
   - AV/CACC: DormandPrince (smooth dynamics)?

2. Runtime vs compile-time integrator switching?

3. Krauss stochasticity: Seeded RNG for reproducibility?

---

## Next Steps (Post-Paper)

1. Implement Option C architecture
2. Write corrected Krauss, Newell, AV models
3. Add integrator switching capability
4. Benchmark: accuracy vs speed tradeoffs
5. Document recommended model+integrator combinations

---

*This discussion is deferred. Focus now on ANNSIM 2026 paper submission.*

