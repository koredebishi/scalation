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



**Fresh update:
SECTION: Introduction

SECTION: Background and Related Work

SECTION: Methodology
SUBSECTION: Erlang2S Random Variate for Vehicle Arrival Process
- We used Erlang2S to generate counting and arrival for this work
- Arrival process controls how vehicles enter lanes
- Shifted Erlang-2 distribution for realistic headway modeling
- Applications: network server packet transmission, customer arrivals at banks, fast food drive-through service rates
- Pseudocode implementation of Erlang2S class with parameters mu (mean), tau (time shift), stream (random number stream)
- Parameter constraints: tau > 0 and tau < mu
- Mean formula: mean = tau + 2*mu
- PDF formula: λ^2 * (z-tau) * exp(-λ*(z-tau)) for z >= tau
- Generation formula: tau - mu * log(r.gen * r.gen)

    SUBSECTION: Microscopic Traffic Flow Model
        - IDM (Intelligent Driver Model) for car-following behavior
        - Describes individual vehicle interactions on the road
        - IDM parameters: desired speed, minimum spacing, time headway, maximum acceleration, comfortable deceleration
        - Acceleration calculation based on leader-follower dynamics
        - Integration methods for position and acceleration:
            - Dormand-Prince (DOPRI5) for positional and acceleration calculation (primary method)
            - Butcher tableau integration method implemented as alternative updateV method for IDM dynamics
            - Both methods called twice per time step for full dynamics
        - Vehicle state: position, speed, acceleration
        - Leader snapshot semantics to prevent inconsistencies in coroutine-based simulation

    SUBSECTION: Highway Network Model Structure
        - CalRoute101_2 simulation model covering entire US-101 highway segment
        - 5 sensors, 4 lanes, 48 time intervals
        - Mainline segments modeled using doubly-linked list (DLL) data structure for cars
        - Each car maintains forward and backward pointers for efficient leader-follower queries
        - Ramps modeled with entry/exit merge points
        - Junction class for handling vehicle transitions between segments
        - Route class with path method for segment navigation
        - Segment class with move method for position updates
        - Transport entity representing road segments

    SUBSECTION: Data Preparation and Validation
        - PeMS (Performance Measurement System) data from California highway sensors
        - Data conversion from mph to m/s for proper integration with simulation model (conversion factor: 0.44704)
        - 5 sensors × 4 lanes = 20 data streams
        - 48 time intervals (5-minute aggregation)
        - Lane-level validation to identify and handle sensor anomalies (lanes 3, 4 of sensor 4 identified as problematic)
        - Data format: CSV with speed, flow, occupancy per lane per sensor per time interval
        - Matrix loading for sensor data into simulation model

    SUBSECTION: Vehicle Generation and Sources
        - VSource class for single-lane vehicle generation
        - MultiVSource class for multi-lane vehicle generation
        - Lane-specific arrival processes using Erlang2S
        - Vehicle generation loop in act() method
        - Parameterized vehicle types with different driving characteristics
        - Initial spacing and headway enforcement

SECTION: Simulation Architecture
SUBSECTION: Process-Oriented Discrete Event Simulation
- ScalaTion 2.0 framework
- Coroutine-based vehicle actors
- Each vehicle is an independent process
- Event scheduling via act() methods
- Simulation clock management
- Schedule method for future events

    SUBSECTION: Traffic Configuration System
        - TrafficConfig2 class for highway network setup
        - Object-oriented design with Transport entities
        - Junction class for merge/diverge logic
        - Route class for path management
        - Segment class for road sections
        - Lane change logic (not fully implemented in current version)

    SUBSECTION: Car Behavior Implementation
        - Car class extending Actor
        - driveHighway() method as main behavior loop
        - While-loop for micro-step simulation
        - route.path().seg().move() for position updates (60-70% of runtime)
        - Leader detection and following logic
        - Speed and acceleration updates via IDM
        - Jump method for junction transitions

SECTION: Calibration and Optimization
SUBSECTION: Parameter Optimization Approach
- 5 IDM parameters to calibrate: desired speed, minimum spacing, time headway, max acceleration, comfortable deceleration
- Vehicle class parameters used as starting point in SPSA_MO optimizer
- Initial parameters: VectorD(5.0, 4.0, -1.5, 3.0, 1.0)
- SPSA (Simultaneous Perturbation Stochastic Approximation)
- SPSA_MO with momentum for improved convergence
- TrafficOptimization class wrapping CalibrateCalRoute101 model adapter
- Fitness function based on error metrics between simulated and real sensor data
- Iterative parameter updates based on gradient estimates
- Convergence criteria for termination

    SUBSECTION: Calibration Model Adapter
        - CalibrateCalRoute101 class implementing model interface
        - evaluate() method running simulation and computing fitness
        - Fitness computation across all sensors and lanes
        - Error metrics: MAE, RMSE, SMAPE, R²
        - Aggregation of per-lane errors into single fitness value
        - Sensor output comparison: simulated vs. real-world PeMS data

    SUBSECTION: HPC Parallelization Strategy
        - Designed SPSA optimization to run multiple trials in parallel
        - Sapelo2 HPC cluster deployment using SBATCH array jobs
        - Parallel parameter search across different initial conditions and perturbation sequences
        - Independent SPSA runs with different random seeds
        - Job array for multiple simultaneous calibration runs
        - Reduced iteration count (e.g., 40 iterations) for faster turnaround
        - Results aggregation across parallel runs
        - Best parameter selection from ensemble of runs

SECTION: Performance Optimization
SUBSECTION: Code Optimization for Production Runs
- Removed non-critical print statements across entire codebase
- Scope: CalRoute101_2, VSource, MultiVSource, TrafficCong2, and calibration classes
- Retained critical output: initial fitness, intermediate fitness values, intermediate parameter samples, final fitness, final parameters
- Reduced I/O overhead during simulation runs
- CSV writing optimization for sensor data recording
- Eliminated redundant calculations

    SUBSECTION: Computational Hotpath Identification
        - Top computational bottlenecks identified:
            - Car.driveHighway() → route.path().seg().move() consuming 60-70% of runtime
            - Junction.jump() → record() consuming 15-20% of runtime
            - VSource.act() vehicle generation loop consuming 10-15% of startup time
            - DormandPrince.integrateVV identified as single most expensive operation in micro-step simulation loop
        - Hotpath in micro-step while-loop inside driveHighway()
        - Per-vehicle integration called at every micro time step
        - Potential for macro-step batch processing instead of micro-steps

SECTION: Evaluation
SUBSECTION: Evaluation Metrics
- MAE (Mean Absolute Error)
- RMSE (Root Mean Squared Error)
- SMAPE (Symmetric Mean Absolute Percentage Error)
- R² (Coefficient of Determination)
- Per-lane error computation
- Per-sensor error computation
- Time-series comparison at 5-minute intervals

    SUBSECTION: Macroscopic Evaluation
        - Macroscopic measures: average travel time, average speed, traffic density
        - Aggregated flow rates across lanes
        - Total throughput measurement
        - High-level overview of traffic flow
        - Useful for identifying overall trends and patterns
        - Comparison with PeMS aggregate statistics

    SUBSECTION: Microscopic Evaluation
        - Microscopic measures: vehicle trajectories, headways, lane-changing behavior
        - Detailed view of traffic flow leveraging simulation model accuracy
        - Per-lane accuracy measurement using metrics: MAE, RMSE, SMAPE, R²
        - Individual vehicle tracking
        - Sensor-level speed comparisons
        - Lane-level flow validation
        - Useful for identifying specific issues and areas for improvement
        - Identification of problematic sensors (e.g., sensor 4 lanes 3-4)

SECTION: Results and Discussion
SUBSECTION: Calibration Results
- Final optimized parameter values
- Convergence behavior of SPSA_MO
- Number of iterations required
- Final fitness value achieved
- Comparison of initial vs. final fitness
- Intermediate parameter trajectories

    SUBSECTION: Simulation Validation
        - Comparison of simulated sensor outputs vs. PeMS data
        - Per-sensor accuracy metrics
        - Per-lane accuracy metrics
        - Time-series plots showing fit quality
        - Discussion of sensor anomalies and their impact
        - Overall R² values across sensors

    SUBSECTION: Performance Analysis
        - Simulation runtime (baseline: ~2 hours)
        - Expected SPSA optimization time for multiple trials
        - HPC speedup from parallelization
        - Computational cost per iteration

SECTION: Conclusion and Future Work
SUBSECTION: Contributions Summary
- Erlang2S arrival process for realistic vehicle generation
- DOPRI5 integration for accurate IDM dynamics
- Coroutine-based process-oriented simulation architecture
- Doubly-linked list for efficient car-following
- Multi-lane, multi-sensor highway calibration
- SPSA_MO optimization with HPC parallelization
- Comprehensive microscopic and macroscopic validation

    SUBSECTION: Future Work
        - Lane-changing model implementation
        - GPU acceleration for parallel vehicle updates
        - Macro-step integration to replace micro-step loops
        - Extended highway network with more sensors
        - Real-time traffic prediction capabilities
        - Additional car-following models beyond IDM

