# ScalaTion Traffic: A Process-Oriented Microscopic Traffic Simulator with Higher-Order Vehicle Dynamics

## Proposed Title Options

1. **ScalaTion Traffic: Coupled ODE Integration for Calibration-Ready Microscopic Simulation**
2. **From Coroutines to Calibration: A Scala-Based Traffic Simulator with Dormand-Prince Vehicle Dynamics**
3. **Higher-Order IDM Integration in Process-Oriented Traffic Simulation: Architecture and Calibration**

---

## Abstract (250 words)

Microscopic traffic simulation requires faithful reproduction of vehicle dynamics for meaningful calibration against empirical sensor data. Existing simulators such as SUMO employ first-order Euler integration, which introduces discretization noise that compounds during parameter estimation. We present **ScalaTion Traffic**, a process-oriented microscopic traffic simulator implemented in Scala that advances vehicle states using a coupled Dormand-Prince (DOPRI5) integrator, achieving O(Δt⁵) accuracy for both position and velocity updates.

Our architecture exploits Scala's coroutine-based process interaction to model each vehicle as an independent actor scheduled by a priority-queue director. This design naturally enforces snapshot semantics: each vehicle observes frozen leader state during its integration step, preserving physical causality without requiring global state vectors. Lane changes and segment transitions occur between integration steps, ensuring the ODE solver operates on smooth trajectories within each timestep.

Vehicle arrivals follow shifted Erlang-2 distributions (`Erlang2S`) parameterized per lane, capturing the reduced variance of real traffic headways compared to Poisson assumptions. A multi-lane `VSource` component generates vehicles with lane-specific flow rates derived from California PeMS detector data.

We demonstrate calibration of Intelligent Driver Model (IDM) parameters using Simultaneous Perturbation Stochastic Approximation (SPSA) against five mainline sensors on US-101 (Redwood Creek corridor). The higher-order integrator reduces numerical noise in the objective function, improving optimizer convergence compared to Euler-based baselines.

ScalaTion Traffic is open-source, built on the ScalaTion simulation framework, and designed for researchers requiring numerically rigorous vehicle dynamics without sacrificing the modularity of discrete-event simulation.

---

## Key Contributions

### 1. Coupled ODE Formulation for IDM (Novel Integration Approach)

**What we did:**
- Formulated IDM as a coupled 2D ODE system: `y = [x, v]`, `y' = [v, a(x,v)]`
- Integrated position and velocity together using `DormandPrince.integrateVV`
- Achieved O(Δt⁵) local truncation error vs. O(Δt) for Euler

**Why it matters:**
- Prior IDM implementations (including SUMO) use decoupled Euler updates
- Decoupled updates create temporal inconsistency: position uses "new" velocity
- Coupled integration maintains mathematical consistency of the ODE system

**Literature gap filled:**
- Treiber's IDM papers describe the continuous model but don't specify integration
- SUMO documentation shows Euler stepping as implementation choice
- We show higher-order integration is architecturally feasible in DES

---

### 2. Process-Oriented Architecture with Snapshot Semantics (Architectural Innovation)

**What we did:**
- Each vehicle is a Scala coroutine (`SimActor`)
- Director schedules actors via priority queue ordered by activation time
- Leader state is snapshotted at `updateM` entry, frozen during integration

**Why it matters:**
- Preserves physical causality: driver responds to observed (past) leader state
- No global state vector required — per-vehicle ODE call suffices
- Lane changes occur between `move()` calls, not during integration

**Literature gap filled:**
- Claims that "adaptive ODE solvers require global state redesign" are false for coroutine architectures
- We demonstrate per-vehicle DOPRI5 is valid when snapshot semantics hold

---

### 3. Shifted Erlang-2 Inter-Arrival Times (Stochastic Modeling)

**What we did:**
- Implemented `Erlang2S(μ, τ)`: shifted Erlang-2 with minimum headway τ
- `gen = τ - μ * log(r₁ * r₂)` ensures arrivals ≥ τ seconds apart
- Per-lane parameterization via `MultiVSource`

**Why it matters:**
- Poisson arrivals (Exponential headways) allow arbitrarily small gaps
- Real traffic has physical minimum headway (reaction time + vehicle length)
- Erlang-2 has lower variance than Exponential, matching observed headway distributions

**Literature gap filled:**
- Most simulators use Exponential or fixed headways
- Shifted Erlang captures both minimum gap and realistic variance

---

### 4. SPSA Calibration with PeMS Sensor Data (Validation Framework)

**What we did:**
- Calibrate 5 IDM parameters: `[s, amax, bmax, T, τ]`
- Objective: minimize SMAPE across 5 PeMS mainline sensors
- Use SPSA for gradient-free optimization with noisy objective

**Why it matters:**
- Higher-order integration reduces numerical noise in fitness function
- Cleaner gradients → faster SPSA convergence
- Validates simulator against real California freeway data

---

## Positioning for ANNSIM

### Conference Fit
ANNSIM (Annual Modeling and Simulation Conference) focuses on:
- Simulation methodology
- Model validation and verification
- Applied simulation in transportation

This paper fits the **methodology + application** track:
- Novel integration approach (methodology)
- Calibration against real data (validation)
- Open-source implementation (reproducibility)

### Differentiation from Existing Work

| Aspect | SUMO | VISSIM | This Work |
|--------|------|--------|-----------|
| Integration | Euler | Proprietary | DOPRI5 |
| Architecture | C++ event-driven | Closed | Scala coroutines |
| Headway model | Exponential | Various | Shifted Erlang-2 |
| Calibration | External tools | Built-in | SPSA integrated |
| Open source | ✅ | ❌ | ✅ |

### Novelty Statement (for reviewers)

> "To our knowledge, this is the first microscopic traffic simulator to employ per-vehicle Dormand-Prince integration within a process-oriented discrete-event architecture, demonstrating that higher-order ODE solvers are compatible with coroutine-based simulation without requiring global state redesign."

---

## Suggested Paper Structure

1. **Introduction** (1 page)
   - Motivation: calibration requires numerical accuracy
   - Gap: Euler integration introduces noise
   - Contribution: coupled DOPRI5 in process-oriented DES

2. **Background** (1.5 pages)
   - IDM equations
   - Dormand-Prince method
   - Process-oriented simulation

3. **Architecture** (2 pages)
   - Coroutine-based vehicle actors
   - Snapshot semantics for leader state
   - Coupled ODE formulation
   - Shifted Erlang-2 arrivals

4. **Implementation** (1.5 pages)
   - ScalaTion framework overview
   - `DormandPrince.integrateVV` integration
   - `MultiVSource` lane-specific generation

5. **Calibration Study** (2 pages)
   - US-101 Redwood Creek corridor
   - PeMS sensor data
   - SPSA optimization
   - Convergence comparison (Euler vs. DOPRI5)

6. **Conclusions** (0.5 page)
   - Summary of contributions
   - Future work: lane-changing models, GPU acceleration

---

## Keywords

Microscopic traffic simulation, Intelligent Driver Model, Dormand-Prince integration, process-oriented simulation, SPSA calibration, PeMS validation, ScalaTion

---

**Target Venue:** ANNSIM 2025/2026  
**Page Limit:** 8-10 pages (check CFP)  
**Submission Category:** Full Paper (Methodology + Application)

