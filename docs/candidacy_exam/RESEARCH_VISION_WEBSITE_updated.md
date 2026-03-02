# Korede R. Bishi
**Ph.D. Student, Computer Science, University of Georgia**

**Research Area:** Discrete Event Simulation, Calibration, and Infrastructure Resilience  
**Application Domain:** Microscopic Traffic Simulation

---

## Research Vision

My dissertation investigates a fundamental question in simulation science:

> **Can we build microscopic traffic simulation models trustworthy enough to evaluate infrastructure policy under extreme conditions — where real-world experimentation is impossible?**

Answering this requires solving three interconnected problems: (1) understanding which modeling decisions actually affect simulation accuracy, (2) developing constrained calibration methods that produce physically valid parameters, and (3) applying the validated model to a high-stakes policy question where simulation is the only feasible evaluation method.

I address these through three studies, each building on the previous:

- **Study 1** establishes which modeling choices matter — and which do not.
- **Study 2** develops and evaluates calibration methodology for the choices that matter.
- **Study 3** applies the validated, calibrated model to evaluate wildfire evacuation resilience on a real freeway corridor.

All three studies extend the **ScalaTion framework** (developed by Dr. John A. Miller and collaborators) and validate against real-world sensor data from the California Performance Measurement System (PeMS).

---

## Study 1: Which Modeling Decisions Matter?
*ANNSIM 2026 (Submitted)*

**Title:** "Beyond Corridor Averages: Lane-Level Validation of Microscopic Freeway Simulation with Data-Driven Arrivals"

**Motivation:**  
Microscopic traffic simulators require many modeling choices — numerical integration schemes, vehicle arrival processes, time-step resolution — yet the sensitivity of simulation accuracy to these choices is poorly understood. Practitioners often adopt defaults without systematic evaluation.

**Approach:**  
We systematically varied two key modeling decisions — numerical integrator (8 methods, from Euler to Dormand-Prince) and vehicle arrival process (Poisson, Erlang-2, shifted Erlang-2) — and evaluated their impact on lane-level flow and speed accuracy across five PeMS detector stations on a US-101 freeway corridor.

**Key Findings:**
- Numerical integrator choice has **<1% impact** on simulation accuracy — simple ballistic integration suffices
- Vehicle **arrival process modeling substantially affects fidelity** — the shifted Erlang-2 distribution reduces flow error by ~28% compared to Poisson by enforcing a realistic minimum headway
- **Lane-level validation** reveals dynamics that corridor-level aggregation obscures

**Significance:**  
These findings direct calibration effort toward the modeling decisions that matter (arrival processes) and away from those that do not (integrators), informing the constrained calibration approach in Study 2.

---

## Study 2: Constrained Calibration of Car-Following Models
*WSC 2026 (In Progress)*

**Title:** "Comparative Analysis of Car-Following Models and Optimization Algorithms for Multi-Lane Traffic Simulation Calibration"

**Motivation:**  
Building on Study 1's finding that arrival processes govern flow accuracy while car-following parameters govern speed accuracy, this study asks: which combination of car-following model and optimization algorithm produces the best-calibrated simulation — and can constrained optimization improve speed prediction without degrading the flow accuracy already achieved by the arrival process?

**Approach:**
- 2 car-following models (IDM, Gipps) × 4 optimization algorithms (SPSA, SPSA with momentum, Nelder-Mead, Genetic Algorithm) = 8 experimental conditions
- Physically constrained parameter bounds centered on empirically validated defaults
- Flow-protected fitness function: optimizer is penalized for degrading flow accuracy beyond baseline
- HPC deployment on Georgia Advanced Computing Resource Center (8 parallel calibration jobs)
- Corridor-level validation: 5 detector stations × 4 lanes on US-101

**Preliminary Findings:**
- Car-following parameters primarily control speed dynamics; arrival processes control flow — confirming the structural separation identified in Study 1
- Constrained optimization achieves significant speed improvement while preserving flow accuracy
- Different optimizers converge to qualitatively different regions of parameter space, suggesting multiple local optima in the calibration landscape

**Significance:**  
This study establishes a calibrated, validated simulation model suitable for scenario analysis — the prerequisite for Study 3's application to evacuation resilience.

---

## Study 3: Wildfire Evacuation Resilience
*Dissertation Study (In Progress)*

**Title:** "Evaluating Evacuation Resilience Under Wildfire Disruption: A PeMS-Calibrated Microscopic Simulation of I-10 During the 2025 Palisades Fire"

**Motivation:**  
On January 7, 2025, the Palisades Fire triggered mass evacuation along I-10 eastbound in Los Angeles. Severe congestion and smoke degraded corridor performance for hours. A recurring policy question — whether directional lane reallocation (contraflow) would have improved evacuation throughput — cannot be answered through real-world experimentation. Simulation provides the only feasible evaluation method.

**Approach:**
1. **Baseline calibration:** Reproduce normal-day I-10 traffic dynamics using PeMS data and the calibration methodology from Studies 1–2
2. **Fire-day reconstruction:** Detect demand surge timing from PeMS, reconstruct the congestion event in simulation
3. **Smoke-behavior modeling:** Translate smoke exposure into driving behavior degradation (reduced desired speed, increased headway, reduced lane-change aggressiveness) using visibility-impaired driving literature
4. **Counterfactual scenarios:** Evaluate evacuation performance under multiple capacity configurations — baseline, partial contraflow, full contraflow, and contraflow under smoke

**Evaluation Metrics:**
- Evacuation throughput (vehicles/hour)
- Mean corridor speed
- Congestion clearance time
- Resilience index: R = 1 − (performance loss area / baseline area)

**Expected Contributions:**
- First PeMS-calibrated microscopic reconstruction of the 2025 Palisades Fire evacuation
- Smoke-as-behavioral-degradation module for microscopic DES
- Quantitative counterfactual evaluation of contraflow effectiveness under visibility impairment
- Identification of conditions under which capacity expansion alone is insufficient — requiring behavioral adaptation

**Target:** Winter Simulation Conference 2026 — *Simulation for Climate Resilience* track

---

## Technical Contributions to ScalaTion

Extending the ScalaTion simulation framework, I have contributed:

| Contribution | Description |
|---|---|
| **Lane-level validation** | Per-lane flow and speed recording with automated PeMS data comparison |
| **Constrained calibration framework** | Flow-protected fitness function with physically grounded parameter bounds |
| **Car-following model suite** | IDM, Gipps, and Krauss dynamics with configurable ODE solvers |
| **Route abstraction** | Doubly-linked segment structure for multi-lane freeway corridors |
| **Ramp modeling** | On-ramp merge behavior using VTransport |
| **HPC calibration pipeline** | SLURM array job orchestration for parallel optimizer evaluation |
| **Simulation reporting** | Automated CSV/TXT export of per-sensor, per-lane validation metrics |

---

## Publications

### Submitted
1. **Bishi, K.R.**, Bowman, J., Miller, J.A. (2026). "Beyond Corridor Averages: Lane-Level Validation of Microscopic Freeway Simulation with Data-Driven Arrivals." *Annual Modeling and Simulation Conference (ANNSIM)*. [Under Review]

### In Preparation
2. **Bishi, K.R.**, Miller, J.A. (2026). "Comparative Analysis of Car-Following Models and Optimization Algorithms for Multi-Lane Traffic Simulation Calibration." *Winter Simulation Conference (WSC)*. [Target: April 2026]

3. **Bishi, K.R.**, Miller, J.A. (2026). "Evaluating Evacuation Resilience Under Wildfire Disruption: A PeMS-Calibrated Microscopic Simulation of I-10 During the 2025 Palisades Fire." *Winter Simulation Conference (WSC) — Simulation for Climate Resilience*. [Target: 2026]

---

## About ScalaTion

**ScalaTion** is a Scala-based framework for simulation, optimization, and analytics developed by Dr. John A. Miller and collaborators at the University of Georgia. The framework supports:

- **Multiple simulation paradigms:** Process-oriented, event-driven, and time-stepped simulation
- **Continuous-time models within discrete-event frameworks:** e.g., IDM car-following integrated via configurable ODE solvers
- **Native optimization:** SPSA, Nelder-Mead, Differential Evolution, Genetic Algorithms
- **Analytics:** Statistical modeling, machine learning, and database connectivity

My work extends ScalaTion's traffic simulation capabilities for microscopic freeway modeling with real-world validation.

---

## Education

**Ph.D. Computer Science** (In Progress)  
University of Georgia  
Advisor: Dr. John A. Miller  
Track: Simulation and Analytics

**Relevant Coursework:**
- Algorithms, Software Engineering, Computer Networks
- Cloud Computing, Advanced Distributed Systems
- Trustworthy Machine Learning, Advanced Representation Learning
- Transportation Planning (Spring 2026)

---

## Contact

**Email:** krb84578@uga.edu  
**Advisor:** Dr. John A. Miller  
**Lab:** ScalaTion Research Group, University of Georgia

© 2026 Korede R. Bishi | University of Georgia


