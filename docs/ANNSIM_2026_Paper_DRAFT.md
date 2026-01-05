# Higher-Order IDM Integration for Microscopic Traffic Simulation: A Dormand-Prince Approach with Multi-Scale Validation

**Authors:** [Your Name], [Advisor Name]  
**Affiliation:** University of Georgia, Department of Computer Science  
**Email:** krb84578@uga.edu

---

## ABSTRACT

Microscopic traffic simulation requires accurate vehicle dynamics for meaningful calibration against empirical data. We extend the ScalaTion simulation framework with three contributions for freeway traffic modeling: (1) per-vehicle coupled ODE integration using Dormand-Prince (DOPRI5), achieving O(Δt⁵) accuracy for Intelligent Driver Model dynamics within a coroutine-based discrete-event architecture; (2) a shifted Erlang-2 arrival process with data-driven parameterization, enforcing physical minimum headways while matching observed headway variance; and (3) a multi-scale validation framework comparing simulated vehicle counts and speeds against 40 PeMS data streams at 15-minute temporal resolution. We demonstrate calibration of 5 IDM parameters using SPSA with momentum, minimizing Normalized RMSE across the US-101 Donald Doyle corridor. Results show strong flow agreement (R² > 0.82) with lane-level speed validation.

**Keywords:** microscopic traffic simulation, Intelligent Driver Model, Dormand-Prince integration, discrete-event simulation, calibration, PeMS validation

---

## 1. INTRODUCTION

[~0.75 pages - Write this LAST after everything else is done]

Microscopic traffic simulation is essential for evaluating traffic management strategies, infrastructure planning, and autonomous vehicle testing. The fidelity of such simulations depends critically on two factors: (1) the accuracy of vehicle dynamics models, and (2) the realism of vehicle arrival processes.

The Intelligent Driver Model (IDM) [Treiber et al., 2000] is widely used for car-following behavior. However, most simulators—including SUMO—integrate IDM using first-order Euler methods, introducing O(Δt) discretization error that compounds during parameter estimation. When the goal is micro-level validation—matching speeds and flows at individual lanes and 15-minute intervals—this numerical noise obscures the signal we seek to calibrate.

**Our contributions:**
1. **Per-vehicle coupled ODE integration** using Dormand-Prince (DOPRI5), achieving O(Δt⁵) accuracy for IDM dynamics—enabling reliable speed and flow reporting at micro-level temporal resolution
2. **Shifted Erlang-2 arrivals** with data-driven parameterization, enforcing physical minimum headways while matching observed traffic demand patterns
3. **Multi-scale validation framework** comparing simulated counts and speeds against 40 PeMS data streams at lane-level, 15-minute resolution

The remainder of this paper is organized as follows. Section 2 reviews related work. Section 3 presents our methodology. Section 4 describes the experimental setup. Section 5 presents results. Section 6 concludes.

---

## 2. RELATED WORK

[~0.5 pages]

### 2.1 Car-Following Models

The Intelligent Driver Model (IDM) [Treiber et al., 2000] computes acceleration based on desired velocity, safe following distance, and approach rate. Variants include the Improved IDM [Treiber and Kesting, 2013] and ACC models.

### 2.2 Numerical Integration in Traffic Simulation

SUMO uses ballistic/Euler integration [Lopez et al., 2018]. Higher-order methods like Runge-Kutta have been applied to macroscopic models but rarely to microscopic, per-vehicle dynamics within discrete-event frameworks.

### 2.3 Arrival Processes

Most simulators use Poisson (exponential) arrivals. The limitation is that exponential distributions allow arbitrarily small headways, which is physically unrealistic.

### 2.4 Calibration Methods

SPSA [Spall, 1998] is gradient-free and suitable for noisy simulation-based optimization. Extensions with momentum improve convergence [Qian, 1999].

---

## 3. METHODOLOGY

[~2.5 pages - This is your MAIN section]

### 3.1 Intelligent Driver Model

The IDM computes acceleration $a_n$ for vehicle $n$ following predecessor $p$:

$$a_n = a_{\max} \left[ 1 - \left( \frac{v_n}{v_0} \right)^\delta - \left( \frac{s^*(v_n, \Delta v)}{s_n} \right)^2 \right]$$

where the desired gap $s^*$ is:

$$s^*(v_n, \Delta v) = s_0 + v_n T + \frac{v_n \Delta v}{2\sqrt{a_{\max} b}}$$

Parameters: $a_{\max}$ (max acceleration), $b$ (comfortable deceleration), $v_0$ (desired velocity), $T$ (time headway), $s_0$ (minimum gap), $\delta$ (acceleration exponent, typically 4).

### 3.2 The Integration Problem

Our goal is to model vehicle speeds and congestion dynamics at the micro level—individual lanes, 15-minute intervals—and validate against empirical sensor data. This requires accurate velocity trajectories, not just aggregate throughput.

At each simulation step, vehicle $n$ must update its state $(x_n, v_n)$ based on the IDM acceleration $a_n$. The naive approach uses Euler integration:

$$x_n^{t+\Delta t} = x_n^t + v_n^t \cdot \Delta t$$
$$v_n^{t+\Delta t} = v_n^t + a_n^t \cdot \Delta t$$

This is simple but introduces O(Δt) local truncation error. For micro-level speed validation, this matters because:

1. **IDM is nonlinear:** The acceleration $a_n$ depends on gap $s_n = x_p - x_n$ and approach rate $\Delta v = v_n - v_p$, both of which change as the vehicle moves
2. **Error compounds:** Small errors in velocity propagate to position, affecting gap calculations for following vehicles
3. **Speed reporting sensitivity:** Lane-level average speeds are directly affected by individual vehicle velocity errors
4. **Calibration sensitivity:** Parameter estimation amplifies numerical noise—a 5% velocity error can lead to 20% parameter bias

### 3.3 Coupled ODE Formulation

To apply higher-order integration, we first reformulate the problem. Instead of treating position and velocity updates separately, we define the vehicle state as a vector:

$$\mathbf{y} = \begin{bmatrix} x \\ v \end{bmatrix}$$

The dynamics become a coupled initial value problem:

$$\frac{d\mathbf{y}}{dt} = \mathbf{f}(\mathbf{y}) = \begin{bmatrix} v \\ a_{\text{IDM}}(x, v) \end{bmatrix}$$

where $a_{\text{IDM}}$ is the acceleration from Equation (1). This formulation has a key property: the position derivative depends on velocity, and the velocity derivative (acceleration) depends on both position and velocity through the gap term.

**The leader state problem.** In continuous time, the leader's state $(x_p, v_p)$ evolves simultaneously. However, within a discrete-event simulation step, we cannot update all vehicles truly simultaneously. Our solution is to **snapshot** the leader state at step entry:

$$\frac{d\mathbf{y}}{dt} = \begin{bmatrix} v \\ a_{\text{IDM}}(x, v; \tilde{x}_p, \tilde{v}_p) \end{bmatrix}$$

where $(\tilde{x}_p, \tilde{v}_p)$ are frozen values. This preserves discrete-event semantics while enabling higher-order integration within each step.

### 3.4 Dormand-Prince Integration

With the coupled ODE formulation, we can apply the Dormand-Prince method (DOPRI5), a 7-stage embedded Runge-Kutta scheme. At each stage $i$, the solver evaluates:

$$\mathbf{k}_i = \mathbf{f}\left( t_n + c_i h, \, \mathbf{y}_n + h \sum_{j=1}^{i-1} a_{ij} \mathbf{k}_j \right)$$

The final update combines all stages:

$$\mathbf{y}_{n+1} = \mathbf{y}_n + h \sum_{i=1}^{7} b_i \mathbf{k}_i$$

This achieves O(Δt⁵) local truncation error—five orders of magnitude better than Euler for the same step size.

**Why this works for IDM.** Each stage evaluation calls $a_{\text{IDM}}$ with *trial* values of $(x, v)$, not the original state. This allows the solver to "feel" the nonlinearity of the IDM function and adjust accordingly. The snapshotted leader state $(\tilde{x}_p, \tilde{v}_p)$ remains constant across all 7 stages, ensuring a well-posed ODE within each step.

Algorithm 1 summarizes the complete per-vehicle update procedure.

---

**Algorithm 1: Per-Vehicle IDM Update with Dormand-Prince Integration**

```
Input: vehicle n, segment length L, time step Δt
Output: updated position x_n, velocity v_n

1.  Snapshot leader state: (x̃_p, ṽ_p) ← (x_p, v_p)
2.  Define ODE system:
      dy/dt = [v, a_IDM(x, v; x̃_p, ṽ_p)]ᵀ
3.  Set initial state: y₀ ← [x_n, v_n]ᵀ
4.  Solve: y₁ ← DOPRI5.integrate(dy/dt, y₀, Δt)
5.  Extract: x_new ← y₁[0], v_new ← y₁[1]
6.  Clamp velocity: v_new ← max(0, min(v_new, v_max))
7.  Update segment displacement:
      Δx ← x_new - x_n
      disp ← min(disp + Δx, L)
8.  Update vehicle state: x_n ← x_new, v_n ← v_new
```

---

### 3.5 Shifted Erlang-2 Arrivals

We introduce a shift parameter $\tau > 0$ to enforce minimum headway:

$$Y = \tau + X, \quad X \sim \text{Erlang-2}(\mu)$$

**PDF:**
$$f_Y(y) = \begin{cases} \lambda^2 (y - \tau) e^{-\lambda(y - \tau)} & y \geq \tau \\ 0 & y < \tau \end{cases}$$

**Random variate generation:**
$$Y = \tau - \mu \ln(U_1 \cdot U_2)$$

The mean inter-arrival time $\mu_{\ell,t}$ is computed per lane $\ell$ and time interval $t$ from PeMS flow data:

$$\mu_{\ell,t} = \frac{\Delta T}{N_{\ell,t}}$$

### 3.6 Multi-Scale Validation

**Metric selection.** We use different metrics for calibration versus reporting, each chosen for its specific purpose.

**NRMSE for calibration.** The optimization objective uses Normalized Root Mean Square Error:

$$\text{NRMSE} = \frac{\text{RMSE}}{y_{\max} - y_{\min}}$$

We chose NRMSE for the fitness function because:
1. **Scale invariance:** Normalization by range allows combining count errors (vehicles) and speed errors (m/s) into a single objective
2. **Symmetric penalty:** Over- and under-prediction are penalized equally
3. **Stability:** Unlike R², which depends on observed variance (SST), NRMSE normalizes by range, providing consistent behavior across sensors with different traffic patterns

**R² and RMSE for reporting.** In results, we report two complementary metrics:

$$R^2 = 1 - \frac{SSE}{SST} = 1 - \frac{\sum(y_i - \hat{y}_i)^2}{\sum(y_i - \bar{y})^2}$$

$$\text{RMSE} = \sqrt{\frac{1}{m}\sum_{i=1}^{m}(y_i - \hat{y}_i)^2}$$

R² provides interpretability—values near 1 indicate good fit, while negative values signal failure. RMSE provides absolute error magnitude in original units (vehicles for counts, m/s for speeds). Together, they answer: "How well does the simulation fit?" and "How large are the errors?"

**Aggregate fitness.** The calibration objective combines both modalities across $S=5$ sensors:

$$\mathcal{F}(\theta) = 0.5 \cdot \frac{1}{S} \sum_{s=1}^{S} \text{NRMSE}_s^{\text{count}} + 0.5 \cdot \frac{1}{S} \sum_{s=1}^{S} \text{NRMSE}_s^{\text{speed}}$$

Equal weighting (0.5/0.5) reflects our goal of matching both flow and speed dynamics.

### 3.7 SPSA with Momentum

We use the Simultaneous Perturbation Stochastic Approximation (SPSA) algorithm [Spall, 1998] with momentum for gradient-free optimization. The gradient is estimated via simultaneous perturbation:

$$\hat{g}_k(\theta_k) = \frac{f(\theta_k + c_k \Delta_k) - f(\theta_k - c_k \Delta_k)}{2 c_k} \Delta_k^{-1}$$

where $\Delta_k \in \{-1, +1\}^p$ is a Bernoulli perturbation vector. Parameter updates use momentum [Qian, 1999]:

$$m_{k+1} = \beta \cdot m_k + (1 - \beta) \cdot \hat{g}_k$$
$$\theta_{k+1} = \theta_k - a_k \cdot m_{k+1}$$

The gain sequences $a_k = a/(A+k)^\alpha$ and $c_k = c/k^\gamma$ follow standard SPSA practice with $\alpha = 0.602$, $\gamma = 0.101$. We set momentum $\beta = 0.9$ and clamp parameters to physically meaningful bounds after each update.

---

### 3.8 System Architecture

**[INSERT FIGURE 1: Methodology Diagram]**

Figure 1 illustrates the simulation architecture. The system consists of:

- **Vehicle Sources (VSource):** Per-lane sources generating vehicles via Shifted Erlang-2 with data-driven μ from PeMS
- **Route/Pathway:** Multi-lane highway segments with doubly-linked list vehicle ordering
- **IDM Dynamics:** Per-vehicle Dormand-Prince integration at each segment traversal
- **Sensors (Junction):** Recording counts and speeds at 15-minute intervals
- **Calibration Loop:** SPSA optimizer adjusting IDM parameters to minimize NRMSE

```
┌─────────────────────────────────────────────────────────────────────┐
│                        CALIBRATION LOOP (SPSA)                      │
│   θ = [s₀, amax, b, T, τ]  ←──────────────────────────────┐        │
└─────────────────────────────────────────────────────────────────────┘
                                                             │
        ┌────────────────────────────────────────────────────┘
        ▼
┌───────────────┐    ┌───────────────┐    ┌───────────────┐
│   VSource     │    │   VSource     │    │   VSource     │
│   Lane 0      │    │   Lane 1-3    │    │   Ramps       │
│ Erlang2S(τ,μ) │    │ Erlang2S(τ,μ) │    │ Erlang2S(τ,μ) │
└───────┬───────┘    └───────┬───────┘    └───────┬───────┘
        │                    │                    │
        ▼                    ▼                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│                     ROUTE (4-lane highway)                          │
│  ┌─────────┐   ┌─────────┐   ┌─────────┐   ┌─────────┐   ┌───────┐ │
│  │ Sensor1 │──▶│ Sensor2 │──▶│ Sensor3 │──▶│ Sensor4 │──▶│Sensor5│ │
│  └─────────┘   └─────────┘   └─────────┘   └─────────┘   └───────┘ │
│       │             │             │             │             │     │
│       ▼             ▼             ▼             ▼             ▼     │
│   IDM+DOPRI     IDM+DOPRI     IDM+DOPRI     IDM+DOPRI     IDM+DOPRI │
│   updateM()     updateM()     updateM()     updateM()     updateM() │
└─────────────────────────────────────────────────────────────────────┘
        │                                                       │
        ▼                                                       ▼
┌───────────────┐                                     ┌───────────────┐
│   Recorder    │                                     │     Sink      │
│ counts/speeds │                                     │   (exit)      │
└───────┬───────┘                                     └───────────────┘
        │
        ▼
┌───────────────────────────────────────────────────────────────────┐
│                    VALIDATION (NRMSE)                             │
│   Compare: Sim counts/speeds vs PeMS counts/speeds                │
│   Fitness = 0.5·NRMSE_count + 0.5·NRMSE_speed                     │
└───────────────────────────────────────────────────────────────────┘
```

*Figure 1: System architecture showing vehicle generation, highway traversal with IDM+DOPRI5 dynamics, sensor recording, and SPSA calibration loop.*

---

## 4. EXPERIMENTAL SETUP

[~0.75 pages]

### 4.1 Study Corridor

US-101 Donald Doyle corridor, California. 5 mainline sensors, 4 lanes each, 2 on-ramps.

### 4.2 Data

PeMS sensor data: 15-minute aggregated counts and speeds. 48 time intervals (6am-6pm). 40 total data streams (5 sensors × 4 lanes × 2 modalities).

### 4.3 Simulation Model

Implemented in ScalaTion (Scala-based simulation framework). Process-oriented discrete-event simulation with coroutine-based actors.

### 4.4 Calibration Parameters

5 IDM parameters calibrated: [s₀, amax, b, T, τ] = [min gap, max accel, max decel, time headway, reaction time]

Initial values from literature: (2.0, 1.0, -1.5, 1.5, 0.6)

Bounds: s₀ ∈ [2,8], amax ∈ [1.5,6], b ∈ [-3,-1], T ∈ [1,5], τ ∈ [0.3,1.5]

### 4.5 Optimization Settings

SPSA with momentum: 70 iterations, β = 0.9

---

## 5. RESULTS

[~1.5 pages]

### 5.1 Calibration Convergence

[INSERT: Fitness vs. iteration plot from SPSA run]

Best fitness achieved: [X.XX] after [N] iterations.

Calibrated parameters: [s₀, amax, b, T, τ] = [X.X, X.X, X.X, X.X, X.X]

### 5.2 Macro-Level Validation

| Sensor | Flow R² | Flow RMSE | Speed R² | Speed RMSE |
|--------|---------|-----------|----------|------------|
| 1      | 0.97    | 12.8      | 0.75     | 3.3        |
| 2      | 0.92    | 20.1      | 0.54     | 4.2        |
| 3      | 0.89    | 24.6      | 0.58     | 3.6        |
| 4      | 0.85    | 28.5      | 0.46     | 3.8        |
| 5      | 0.82    | 31.1      | 0.31     | 3.7        |

### 5.3 Micro-Level Validation

[INSERT: Lane-level breakdown table or select representative sensor]

### 5.4 Discussion

Flow matching is strong (R² > 0.82). Speed matching is moderate, reflecting known challenges in matching congestion dynamics. Downstream sensors show degradation due to cumulative error.

---

## 6. CONCLUSIONS

[~0.5 pages]

We presented three contributions for microscopic traffic simulation, motivated by the need for accurate micro-level dynamics reporting:

1. **Per-vehicle Dormand-Prince integration** achieves O(Δt⁵) accuracy for IDM dynamics within a process-oriented DES. By formulating each vehicle's state as a coupled ODE and snapshotting leader state, we enable higher-order integration without requiring global state vectors or architectural redesign.

2. **Shifted Erlang-2 arrivals** enforce physical minimum headways ($\tau > 0$) while matching observed traffic demand through data-driven parameterization of $\mu_{\ell,t}$ from PeMS flow counts.

3. **Multi-scale validation** compares both counts and speeds at lane-level, 15-minute resolution against 40 empirical data streams, using NRMSE for calibration and R²/RMSE for interpretable reporting.

**Toward digital twins.** Accurate micro-level dynamics—speeds and flows at fine temporal and spatial resolution—are foundational for traffic digital twins, where simulation must mirror reality closely enough to support real-time decision-making. Our results demonstrate that higher-order integration (Dormand-Prince) combined with realistic arrival processes (Shifted Erlang-2) can achieve strong flow agreement (R² > 0.82) with lane-level speed validation. This accuracy at the micro level moves simulation closer to digital twin requirements, where per-lane, per-interval fidelity is essential.

**Future work:** Lane-change model calibration, GPU acceleration for large-scale networks, real-time data assimilation for online digital twin operation.

---

## ACKNOWLEDGEMENTS

This work was supported by [funding source if applicable].

**AI Disclosure:** In accordance with SCS policy, we disclose the use of a Large Language Model (Claude) to assist with drafting portions of Section 3 (Methodology) and structuring the manuscript. All technical content, experimental results, and scientific conclusions are the authors' own work.

---

## REFERENCES

[Dormand and Prince, 1980] Dormand, J. R., & Prince, P. J. (1980). A family of embedded Runge-Kutta formulae. Journal of Computational and Applied Mathematics, 6(1), 19-26.

[Treiber et al., 2000] Treiber, M., Hennecke, A., & Helbing, D. (2000). Congested traffic states in empirical observations and microscopic simulations. Physical Review E, 62(2), 1805.

[Treiber and Kesting, 2013] Treiber, M., & Kesting, A. (2013). Traffic flow dynamics. Springer.

[Lopez et al., 2018] Lopez, P. A., et al. (2018). Microscopic traffic simulation using SUMO. IEEE ITSC.

[Spall, 1998] Spall, J. C. (1998). Implementation of the simultaneous perturbation algorithm for stochastic optimization. IEEE Transactions on Aerospace and Electronic Systems, 34(3), 817-823.

[Qian, 1999] Qian, N. (1999). On the momentum term in gradient descent learning algorithms. Neural Networks, 12(1), 145-151.

---

## APPENDIX (if needed)

### A. Butcher Tableau for DOPRI5

| Stage | c_i | Coefficients |
|-------|-----|--------------|
| 1 | 0 | — |
| 2 | 1/5 | 1/5 |
| 3 | 3/10 | 3/40, 9/40 |
| ... | ... | ... |

---

**Word Count Target:** ~4000-5000 words (6-8 pages in SCS format)


