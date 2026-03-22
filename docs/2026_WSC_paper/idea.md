# Executive Summary: Wildfire–Smoke Coupled Microscopic Traffic Simulation with Dynamic Traffic Assignment

## 1. Objective
We develop a lane-level microscopic evacuation model that integrates:
- Wildfire spread dynamics
- Smoke propagation
- Dynamic Traffic Assignment (DTA)

Application: Eaton Fire replication on I-210 evacuation.

---

## 2. Wildfire Spread Model (Rothermel-Based / DEVS-FIRE)

We adopt a **cellular fire spread model** grounded in the **Rothermel (1972) fire spread equation**, widely used in operational wildfire simulators.

### 2.1 Rate of Spread (Rothermel Model)

R = (I_R * ξ * (1 + φ_w + φ_s)) / (ρ_b * ε * Q_ig)

Where:
- R = forward rate of spread (m/s)
- I_R = reaction intensity
- ξ = propagating flux ratio
- φ_w = wind factor
- φ_s = slope factor
- ρ_b = bulk density
- ε = effective heating number
- Q_ig = heat of pre-ignition

### 2.2 Implementation (Discrete Grid Approximation)

For cell i:
Fire_i(t+Δt) = Fire_i(t) + R_i * Δt

Fire spreads to neighbors based on:
- wind direction bias
- fuel availability

### Reference
- Rothermel, R.C. (1972). *A Mathematical Model for Predicting Fire Spread in Wildland Fuels*. USDA Forest Service.
- Hu, X. et al. (2009). *Integrated Simulation and Optimization for Wildfire Containment*.

---

## 3. Smoke Propagation Model (Advection–Diffusion Equation)

We model smoke as a **passive scalar field** transported by wind.

### 3.1 Governing Equation (Advection–Diffusion PDE)

∂C/∂t + u · ∇C = D ∇²C + S

Where:
- C(x,t) = smoke concentration
- u = wind velocity vector
- D = diffusion coefficient
- S = source term (fire emission)

### 3.2 Source Term (Fire Coupling)

S(x,t) = α * FireIntensity(x,t)

### 3.3 Discrete Update (Simulation-Friendly)

C_{t+1}(i) = C_t(i)
+ α * FireIntensity(i)
- β * C_t(i)
+ Advection
+ Diffusion

Where:
- α = emission coefficient
- β = decay rate

### Reference
- Seinfeld, J.H. & Pandis, S.N. (2016). *Atmospheric Chemistry and Physics*.
- Mandel, J. et al. (2011). *Coupled Atmosphere-Wildland Fire Modeling with WRF-Fire*.

---

## 4. Dynamic Traffic Assignment (DTA)

We use **time-dependent shortest path routing**.

### 4.1 Link Travel Time (BPR Function)

t_e = t₀ * (1 + α (v/c)^β)

Where:
- t_e = travel time
- t₀ = free-flow time
- v = volume
- c = capacity
- α, β = parameters (typically 0.15, 4)

### 4.2 Hazard-Aware Cost Function

c_e(t) = t_e(t)
+ λ₁ * SmokeDensity_e(t)
+ λ₂ * FireProximity_e(t)

### 4.3 Routing Problem

min ∑ c_e(t)

Solved via:
- time-dependent Dijkstra

### Reference
- Peeta, S. & Ziliaskopoulos, A. (2001). *Foundations of Dynamic Traffic Assignment*.
- TRB (2011). *Dynamic Traffic Assignment Primer*.

---

## 5. Microscopic Traffic Coupling (IDM Extension)

We extend the **Intelligent Driver Model (IDM)**.

### 5.1 Standard IDM

a = a_max [1 - (v/v₀)^δ - (s*/s)^2]

Where:
- s* = s₀ + vT + (vΔv)/(2√(a_max b))

### 5.2 Smoke-Aware Speed Reduction

v_eff = v * (1 - γ * C)

Where:
- C = smoke concentration
- γ = sensitivity factor

### 5.3 Capacity Reduction

c_eff = c * (1 - θ * C)

---

## 6. Integrated System

### Feedback Loop

Fire → Smoke → Network Cost → Routing → Traffic → Congestion → Fire Interaction

---

## 7. Contribution

This framework integrates:

1. Rothermel wildfire spread
2. Advection–diffusion smoke transport
3. Hazard-aware DTA
4. Lane-level microscopic simulation

---

## 8. References (ALL REQUIRED)

### Wildfire / Fire Modeling
- Rothermel, R.C. (1972). *A Mathematical Model for Predicting Fire Spread in Wildland Fuels*. USDA Forest Service.
- Hu, X., et al. (2009). *Integrated Simulation and Optimization for Wildfire Containment*. INFORMS Journal.

### Smoke / Atmospheric Modeling
- Seinfeld, J.H., Pandis, S.N. (2016). *Atmospheric Chemistry and Physics*. Wiley.
- Mandel, J., et al. (2011). *Coupled Atmosphere-Wildland Fire Modeling with WRF-Fire*. Geoscientific Model Development.

### Traffic / DTA
- Peeta, S., Ziliaskopoulos, A. (2001). *Foundations of Dynamic Traffic Assignment*. Transportation Research B.
- TRB (2011). *Dynamic Traffic Assignment: A Primer*. Transportation Research Board.

### Car-Following Model
- Treiber, M., Kesting, A. (2013). *Traffic Flow Dynamics*. Springer.

---

## 9. YouTube (Intuition Only – Not for Citation)

- Dynamic Traffic Assignment (PTV Vissim)
  https://www.youtube.com/watch?v=8Sm2Gbm-pew

- Regional DTA Model Overview
  https://www.youtube.com/watch?v=5mM5RgCTAhA

---

## 10. Key Takeaway

This is a **coupled hazard-aware digital twin**:
- Fire drives smoke
- Smoke alters traffic
- Traffic adapts via DTA

Result:
Realistic evacuation under wildfire conditions.

---

## 11. Algorithm (Pseudocode: Coupled Wildfire–Smoke–DTA Simulation)


# 1. Update Fire Spread (Rothermel-based)
For each cell i:
Compute R_i using Rothermel equation
F_i(t+Δt) = F_i(t) + R_i * Δt

# 2. Update Smoke Field (Advection–Diffusion)
For each cell i:
S_i = α * FireIntensity_i(t)
C_i(t+Δt) = C_i(t)
+ S_i
- β * C_i(t)
+ Advection(u, C)
+ Diffusion(C)

# 3. Update Network Edge Costs
For each edge e in E:
Compute travel time t_e via BPR
Compute smoke density C_e from grid
Compute fire proximity F_e
c_e(t) = t_e + λ₁ * C_e + λ₂ * F_e

# 4. Dynamic Traffic Assignment (Routing)
For each vehicle v in Vh:
If (departure time OR reroute trigger):
Compute shortest path using c_e(t)
Assign route_v

# 5. Microscopic Simulation (IDM with Smoke)
For each vehicle v:
Get local smoke concentration C
Adjust speed: v_eff = v * (1 - γC)
Update acceleration using IDM
Move vehicle along assigned route

# 6. Update Congestion
Update volumes v_e on all edges