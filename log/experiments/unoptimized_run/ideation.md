Below is a **clean, implementation-oriented Markdown file** capturing the **micro-level hybrid GNN + simulation architecture**, exactly as discussed.
You can drop this straight into a repo as `hybrid_traffic_architecture.md`.

---

```md
# Hybrid Traffic Simulation + Learning Architecture (Micro-Level)

## Purpose

This document specifies a **low-level, implementable architecture** for combining
microscopic traffic simulation (e.g., IDM-based) with learning, **without violating
traffic physics or causality**.

The key principle is strict separation:
- **Simulation enforces traffic laws**
- **Learning infers latent human behavior parameters**
- **Learning never predicts traffic states**

---

## Non-Negotiable Design Rule

> Learning must never update traffic states directly.  
> Learning may only update **behavioral parameters** used by the simulator.

If this rule is violated, the system will:
- smooth discontinuities,
- break FIFO and spillback,
- fail under distribution shift.

---

## High-Level Loop (Slow Time Scale)

Learning operates at a **much slower time scale** than simulation.

```

for calibration_round = 1..K:

```
θ = current_behavioral_parameters

simulated_data = run_simulator(θ)

error = compare(simulated_data, real_data)

Δθ = learner(error, context)

θ ← θ + Δθ
```

```

- Simulation: seconds
- Measurement aggregation: minutes
- Learning update: hours / days

---

## Simulator (Immutable Core)

### Responsibilities
- Vehicle conservation
- Collision avoidance
- FIFO lane ordering
- Queue formation
- Shockwave propagation
- Spillback and blocking
- Merge logic
- Geometry and topology

### Example
- IDM car-following model
- Lane-based queues
- Explicit merge rules

### Constraint
The simulator **must function correctly even if learning is disabled**.

---

## Learnable vs Non-Learnable Components

### Must Never Be Learned
- Traffic flow conservation
- Capacity constraints
- Shockwave direction
- Queue spillback logic
- Road geometry
- Lane topology

### Learnable (Latent Human Behavior)
These are **parameters**, not laws:

- Desired time headway (T)
- Reaction time variability
- Acceleration / deceleration comfort
- Lane preference under congestion
- Merge aggressiveness
- Gap acceptance
- Stochastic noise in acceleration

---

## Error Analyzer (Deterministic)

### Inputs
- Simulated flow and speed
- Observed flow and speed (e.g., PeMS)

### Outputs
Error tensor:
```

e[sensor, lane, time] = sim − real

```

Derived indicators:
- Early vs late congestion onset
- Over-smoothed recovery
- Lane-specific bias
- Upstream/downstream mismatch

No learning happens here.

---

## Learning Module (GNN Lives Here)

### Role
The learner performs **parameter attribution**, not prediction.

It answers:
> Which behavioral assumptions caused the observed mismatch?

---

## Graph Definition

### Nodes
- (sensor, lane)

### Edges
- Upstream influence
- Downstream spillback
- Merge interaction
- Same-location lane coupling

This is a **causal influence graph**, not a traffic flow graph.

---

## Node Features

Per node (sensor, lane):

- Mean error
- Error variance
- Time-of-day encoding
- Density regime
- Congestion state
- Proximity to merge

---

## Outputs (NO SOFTMAX)

The learner outputs **parameter corrections**, not probabilities:

```

ΔT   = desired headway correction
Δa   = acceleration correction
Δb   = deceleration correction
Δmerge = merge aggressiveness correction

```

Properties:
- Additive
- Bounded
- Interpretable
- No normalization across nodes

---

## Why Softmax Is Forbidden

Softmax:
- enforces competition between neighbors
- smooths discontinuities
- destroys causality
- violates queue formation

All aggregation must be **linear or gated**, never normalized.

---

## Parameter Update Strategy

Parameter updates are:
- small
- slow
- constrained

Example:
```

T_lane3 ← T_lane3 + clip(ΔT, −ε, +ε)

```

No backpropagation through simulation.
No end-to-end gradients.

---

## Relation to Classical Calibration

Classical approach:
```

error → optimizer → single θ*

```

Hybrid approach:
```

error patterns → learned mapping → contextual θ updates

```

Learning replaces **repeated blind optimization**, not physics.

---

## Why Graph WaveNet Is Insufficient

Graph WaveNet improves:
- static adjacency → learned adjacency

But still performs:
```

state → state prediction

```

Therefore:
- mitigates spatial blindness
- does not solve causality
- still smooths shockwaves

---

## Minimal Viable Implementation Path

1. Existing simulator (IDM + merges)
2. Error analyzer (arrays / tensors)
3. Simple learner (even linear or tree-based initially)
4. Slow outer calibration loop
5. Replace learner with GNN later if needed

---

## Sanity Check (Litmus Test)

> If the learning module fails entirely, does the simulator still obey traffic laws?

If **yes** → architecture is correct  
If **no** → architecture is invalid

---

## One-Line Summary

Traffic laws are simulated.  
Human behavior is inferred.  
Learning explains mismatch — it never replaces causality.

---

## 17) OPEN QUESTIONS & WISHFUL THINKING (Must Be Resolved Before Implementation)

This section consolidates all assumptions, unstated details, and gaps that must be answered.

---

### 17.1 Gradient Estimation (Critical Gap)

**Problem:** Section 11 says `φ ← update(φ, ∇φ L)` but simulation is non-differentiable.

**Unanswered:**
- [ ] How is ∇φ computed? Options: REINFORCE, Evolution Strategy (ES), SPSA on φ, or finite differences?
- [ ] If REINFORCE: what is the variance reduction strategy (baseline subtraction)?
- [ ] If ES: population size? σ for perturbations?
- [ ] If SPSA: same perturbation for φ as for θ, or separate?

---

### 17.2 Scenario Definition (Ambiguous)

**Problem:** "Scenario" is used throughout but never defined precisely.

**Unanswered:**
- [ ] Is a scenario a 12-hour full day?
- [ ] Or a 2-3 hour time block (AM peak, PM peak)?
- [ ] Or a single 15-min interval?
- [ ] Does scenario include corridor identity, or is corridor fixed?

---

### 17.3 Graph Edge Specification (Vague)

**Problem:** Section 5 lists edge types but doesn't specify construction rules.

**Unanswered:**
- [ ] Are edges static (defined by geometry) or learned (attention)?
- [ ] What determines edge weight? Distance? Learned? Unit weight?
- [ ] How many hops for GNN message passing? 1? 2? All reachable?
- [ ] Are edges directed or undirected? (Upstream influence is directional)
- [ ] What is the adjacency matrix construction rule? (e.g., "edge exists if sensor j is within 500m upstream of sensor i")

---

### 17.4 Lane-Level vs Vehicle-Level Parameters (Unstated Assumption)

**Problem:** IDM parameters (T, a, b) are per-vehicle, but learner outputs per-lane corrections.

**Unanswered:**
- [ ] Does lane-level Δθ shift the mean of all vehicles in that lane?
- [ ] Or does it modify a distribution (mean + variance)?
- [ ] How do heterogeneous vehicles (trucks vs cars) fit? Separate parameters?
- [ ] Is there a vehicle class dimension in θ?

---

### 17.5 Stochastic Simulation Handling (Not Mentioned)

**Problem:** Erlang2S arrivals are stochastic. Two runs with same θ give different results.

**Unanswered:**
- [ ] Is simulation deterministic (fixed seed per scenario)?
- [ ] Or stochastic with ensemble averaging (3-5 runs per θ)?
- [ ] If stochastic: how is J variance handled in the improvement loss?
- [ ] Does learner see single-run error or ensemble-averaged error?

---

### 17.6 Regularization Weight (Unspecified)

**Problem:** Section 7 says regularization is required. Section 11 mentions `regularizers(θ_candidate, θ0)` but no weight.

**Unanswered:**
- [ ] What is λ for `λ ||θ - θ0||^2`?
- [ ] Is λ fixed or annealed during training?
- [ ] Is λ the same for all parameter groups (T, a, b, merge) or different?
- [ ] How is λ tuned? Cross-validation on held-out days?

---

### 17.7 Failure Mode Recovery (Not Addressed)

**Problem:** Algorithm assumes learner eventually works. No fallback.

**Unanswered:**
- [ ] What if acceptance rate drops below 10%? Reset learner? Increase ε?
- [ ] What if θ oscillates wildly between epochs? Increase regularization?
- [ ] What if Tier 3 (new corridor) fails completely? Fallback to corridor-specific learner?
- [ ] How is "failure" detected automatically?

---

### 17.8 Learnable Parameter List (Aspirational vs Actual)

**Problem:** Section 1.2 lists many learnable parameters, but current simulator may not support all.

**Unanswered:**
- [ ] Which of these are already in the simulator?
  - [ ] T (desired headway) — yes (IDM)
  - [ ] a (max accel) — yes (IDM)
  - [ ] b (comfortable decel) — yes (IDM)
  - [ ] v0 (desired speed) — yes (IDM)
  - [ ] mergeAgg (merge aggressiveness) — exists?
  - [ ] gap acceptance — exists?
  - [ ] lane preference — exists?
  - [ ] arrival params (τ, μ) — yes (Erlang2S)
- [ ] If some don't exist, is this document proposing to add them?

---

### 17.9 Option A vs Option B (No Commitment)

**Problem:** Section 9 says "Option A is usually better" but Algorithm 1 doesn't commit.

**Unanswered:**
- [ ] Is the implementation using Option A (all-at-once + projection)?
- [ ] Or Option B (sequential coordinate updates)?
- [ ] If Option A: what is the projection algorithm? Simple clipping? QP solver?

---

### 17.10 Error Feature Engineering (Under-Specified)

**Problem:** Section 5 says learner sees "error features" but list is vague.

**Unanswered:**
- [ ] Exact feature vector specification:
  - [ ] Scalar mean error per node?
  - [ ] Time series of errors (48 values per node)?
  - [ ] Summary statistics (mean, std, skew)?
  - [ ] Derived features (onset delay, recovery time)?
- [ ] How are timing features computed?
  - [ ] Threshold-based congestion detection?
  - [ ] Slope of speed drop?
- [ ] Is context (time-of-day, day-of-week) one-hot or continuous?

---

### 17.11 Multi-Objective Handling (Implicit)

**Problem:** J(sim, real) is mentioned as "can be multi-term" but not specified.

**Unanswered:**
- [ ] Is J a weighted sum? `J = w1*J_flow + w2*J_speed`?
- [ ] Or Pareto-style multi-objective?
- [ ] How are weights chosen? Fixed? Learned? Annealed?
- [ ] Is there a timing component (onset/recovery mismatch) in J?

---

### 17.12 Imitation Dataset Construction (Practical Gap)

**Problem:** Stage 1 (imitation) requires dataset of (features → optimizer Δθ).

**Unanswered:**
- [ ] How many optimizer runs are needed to build this dataset?
- [ ] What optimizer is the "teacher"? SPSA? GA? Nelder-Mead?
- [ ] Is Δθ the final optimum, or the trajectory of updates?
- [ ] How is context variation ensured (different days/times)?

---

### 17.13 GNN Architecture (Completely Unspecified)

**Problem:** "GNN" is mentioned but no architecture details.

**Unanswered:**
- [ ] What GNN variant? GCN? GAT? GraphSAGE? Message Passing NN?
- [ ] How many layers?
- [ ] Hidden dimension?
- [ ] Activation function?
- [ ] Readout: per-node output or global pooling + broadcast?
- [ ] Is attention used? (Contradicts "no softmax" if so)

---

### 17.14 Compute Budget (Partially Specified)

**Problem:** Section 13 gives cost per update but not total budget.

**Unanswered:**
- [ ] How many epochs? How many scenarios per epoch?
- [ ] Total wall-clock time for Stage 1? Stage 2? Stage 3?
- [ ] HPC resources assumed? (nodes, GPUs, cores)
- [ ] Is simulation parallelizable across scenarios?

---

### 17.15 Success Criteria Thresholds (Not Defined)

**Problem:** Section 6 lists evaluation tiers but no pass/fail thresholds.

**Unanswered:**
- [ ] What GoF reduction counts as "success"? 5%? 10%? 20%?
- [ ] What NRMSE is acceptable for lane-level validation?
- [ ] Is there a minimum R² threshold?
- [ ] How is "stable parameters" defined quantitatively? (variance across epochs?)

---

## 18) Summary: Document Status

| Aspect | Status |
|--------|--------|
| Core philosophy | ✅ Complete |
| Supervision sources | ✅ Complete |
| Evaluation splits | ✅ Complete |
| Identifiability | ✅ Complete |
| Algorithm pseudocode | ⚠️ Missing gradient spec |
| Graph specification | ❌ Incomplete |
| Feature engineering | ❌ Incomplete |
| GNN architecture | ❌ Not specified |
| Stochasticity handling | ❌ Not mentioned |
| Failure recovery | ❌ Not addressed |
| Compute budget | ⚠️ Partial |
| Success thresholds | ❌ Not defined |

**This document is ~70% complete.** The remaining 30% is the difference between "good idea" and "publishable/implementable system."

---


========================================
Below is CHATGPT Lie:
# Hybrid Simulation-in-the-Loop Learning for Microscopic Traffic (IDM) — Micro-Level Spec (Resolved)

This update **closes the critical gaps** raised in Section 17 by making **explicit design commitments** and giving **implementable defaults**. Where multiple options exist, this document **chooses an MVP** (minimum viable plan) and lists alternatives only when necessary.

---

## 17) OPEN QUESTIONS & WISHFUL THINKING — RESOLVED (Implementation Commitments)

---

### 17.1 Gradient Estimation (Critical Gap) — **RESOLVED**

**Problem:** The simulator is non-differentiable, so `∇φ` cannot be obtained by backprop through simulation.

**Commitment:** Use a **two-stage training strategy** that avoids needing simulator gradients for most of the work:

#### Stage 1 (Primary): Supervised imitation (differentiable)
Train the learner (GNN) using supervised labels from a trusted optimizer (teacher).  
This is standard backprop (fully differentiable) because the loss is between predicted `Δθ` and teacher `Δθ_teacher`.

- Loss: `L_sup = || Δθ_pred − Δθ_teacher ||^2 + regularizers`
- Gradient: computed normally with autodiff (no simulator gradients needed).

✅ **This solves the main “how do we train φ” question.**

#### Stage 2 (Optional refinement): Gradient-free policy improvement (black-box)
If you want to refine beyond the teacher, use **Evolution Strategies (ES)** on φ with simulator returns.

**ES commit (defaults):**
- Population size: `P = 16` (start), `P = 32` (if noisy)
- Perturbation std: `σφ = 0.01` (scale per-layer if needed)
- Antithetic sampling: yes (`+ε`, `−ε`)
- Baseline subtraction: yes (mean return of population)
- Update: `φ ← φ + α * (1/(P*σφ)) * Σ (R_i − b) * ε_i`

Where:
- `R_i = −J(θ_candidate, scenario)` is the reward (lower J = higher reward)
- `b = mean(R_i)` reduces variance

**Why ES and not REINFORCE?**
- ES treats the whole simulator+projection as a black box and is stable.
- REINFORCE is viable but higher variance and requires careful baselines and entropy tricks.

**SPSA on φ?** Not used in MVP. ES is simpler and proven in black-box settings.

**Status:**
- [x] Training φ without simulator gradients (Stage 1 imitation)
- [x] Optional simulator-in-the-loop refinement (Stage 2 ES)

---

### 17.2 Scenario Definition (Ambiguous) — **RESOLVED**

**Commitment:** A **scenario is a 2–3 hour time block** from a specific day on a specific corridor.

Rationale:
- Full 12-hour days are too slow and nonstationary for iterative learning.
- 15-min single intervals are too short to express queues and spillback.

**Scenario definition (MVP):**
- `scenario = (corridor_id, date, start_time, end_time, seed_id)`
- Duration: `ΔT = 3 hours` (e.g., 6–9 AM, 12–3 PM, 3–6 PM)

**Status:**
- [x] Scenario = 2–3 hour block
- [x] Scenario includes corridor identity

---

### 17.3 Graph Edge Specification (Vague) — **RESOLVED**

**Commitment:** Edges are **static** and derived from geometry/topology.  
No learned attention. No softmax.

#### Nodes
- Node = `(sensor s, lane ℓ)`  → total nodes = `#sensors * #lanes`

#### Directed edges (MVP rules)
Let `pos(s)` be sensor location along the corridor.

For each lane ℓ:
- **Upstream influence edge:** if `pos(j) < pos(i)` and j is the nearest upstream sensor of i  
  add `j→i`
- **Downstream feedback edge:** add `i→j` for nearest downstream (spillback coupling)  
  (still directed; spillback is causal opposite direction)

#### Merge coupling edges
If ramp r merges between sensors `s_up` and `s_down`:
- add edges from ramp node(s) to downstream mainline lane nodes near merge:
  `r→(s_down, ℓ_merge)` for affected lanes
- optionally add back-coupling for queue interaction:
  `(s_down, ℓ_merge)→r`

#### Same-location lane coupling
At a sensor s:
- add undirected (or paired directed) edges between adjacent lanes:
  `(s, ℓ) ↔ (s, ℓ+1)`

#### Edge weights (no normalization)
Use **fixed, non-normalized** weights:
- upstream/downstream: `w = 1.0`
- lane adjacency: `w = 0.5`
- merge edges: `w = 1.0`

No softmax. No “weights sum to 1.”

#### Message passing depth
- MVP: `K = 2` layers (2 hops max)

**Status:**
- [x] Edges are static, geometry-defined
- [x] Directed edges for upstream/downstream
- [x] K=2 message-passing depth
- [x] Adjacency construction rule specified

---

### 17.4 Lane-Level vs Vehicle-Level Parameters — **RESOLVED**

**Problem:** IDM parameters are per-vehicle, but learner outputs per-lane corrections.

**Commitment (MVP):** Lane-level `Δθ` updates the **mean behavioral parameters** for vehicles *currently assigned to that lane/region*. Vehicles then sample around that mean (optional) or use the mean deterministically.

#### MVP implementation
- Maintain lane-level parameter means:
  - `T̄[s,ℓ], ā[s,ℓ], b̄[s,ℓ], v̄0[s,ℓ]`
- Each vehicle uses:
  - deterministic: `T = T̄[current_node]` etc.  **(MVP)**
  - OR stochastic: `T ~ Normal(T̄, σ_T)` (optional extension)

#### Heterogeneous vehicles (trucks vs cars)
Not included in MVP unless your simulator already has vehicle classes + data to support calibration.
If included later:
- add class dimension: `T̄[s,ℓ,c]` where `c ∈ {car, truck}`

**Status:**
- [x] Lane-level Δθ shifts mean parameters
- [ ] Vehicle-class dimension (optional extension)

---

### 17.5 Stochastic Simulation Handling — **RESOLVED**

**Problem:** Erlang2S arrivals are stochastic; repeated runs differ.

**Commitment:** Use **Common Random Numbers (CRN)** for training stability, and **ensembles** for evaluation.

#### Training (MVP)
- Fix a seed per scenario: `seed_id`
- When comparing `θ` vs `θ_candidate`, use the **same seed** to reduce variance:
  - `SIM(θ, scenario, seed_id)`
  - `SIM(θ_candidate, scenario, seed_id)`

This makes improvement comparisons meaningful.

#### Evaluation
- Use `M = 5` seeds per scenario and report mean ± std of J and lane-level errors.

#### Improvement loss with noise
Use a margin and/or smoothing:
- `L = max(0, J1 − J0 + margin)` with `margin = 0.01 * J0` (MVP)

**Status:**
- [x] Deterministic-by-seed training (CRN)
- [x] Ensemble evaluation with 5 seeds

---

### 17.6 Regularization Weights — **RESOLVED**

Regularization is required for identifiability and stability.

**Commitment (MVP):** Use **group-specific** regularizers and tune on held-out days.

Let:
- `θ0` = nominal parameters (literature or prior best)
- `θ` = current candidate

Regularizer:
- `R = λ_T ||T̄ − T̄0||^2 + λ_ab (||ā − ā0||^2 + ||b̄ − b̄0||^2) + λ_v ||v̄0 − v̄00||^2 + λ_arr ||arrival − arrival0||^2 + λ_merge ||merge − merge0||^2`
- plus smoothness across lanes: `λ_smooth Σ_adj ||θ_i − θ_j||^2` (graph Laplacian style)

**Default λ (starting point):**
- `λ_T = 1.0`
- `λ_ab = 0.5`
- `λ_v = 0.2`
- `λ_arr = 1.0`
- `λ_merge = 0.5`
- `λ_smooth = 0.2`

**Tuning:**
- Cross-validate on held-out days (Tier 1 split)
- Choose λ that maximizes held-out improvement and parameter stability

**Status:**
- [x] λ specified
- [x] λ tuned by held-out days

---

### 17.7 Failure Mode Recovery — **RESOLVED**

You need automatic guards.

#### Definitions
- Acceptance rate = fraction of scenarios where `J(θ_candidate) < J(θ)`
- Oscillation = `Var(θ)` across epochs exceeds threshold

#### MVP failure policies
- If acceptance rate < 20% for an epoch:
  - reduce step size / trust region radius `r ← 0.5 r`
  - increase regularization `λ ← 1.2 λ`
  - optionally revert learner to last checkpoint `φ ← φ_best`

- If θ oscillates (per-parameter CV > 10% across recent epochs):
  - increase smoothness `λ_smooth`
  - reduce per-update magnitude `r`
  - switch to sequential updates (Option B fallback) for 1–2 epochs

- If Tier 3 (new corridor) fails:
  - fallback to corridor-specific adaptor:
    - keep a global learner + small corridor embedding
    - or fine-tune only final layer on corridor B (few-shot)

**Failure detection is automatic** using acceptance rate + parameter variance.

**Status:**
- [x] Acceptance-rate guardrails
- [x] Oscillation guardrails
- [x] Transfer failure fallback

---

### 17.8 Learnable Parameter List (Aspirational vs Actual) — **RESOLVED**

**Commitment (MVP):** Only learn parameters that already exist or can be added with minimal code.

#### Already present (assumed in IDM + arrivals)
- [x] `T` (IDM)
- [x] `a` (IDM)
- [x] `b` (IDM)
- [x] `v0` (IDM)
- [x] arrival parameters (Erlang2S: shift + shape/scale or equivalent)

#### Merge behavior (depends on your simulator)
- [ ] `mergeAgg` as gap-acceptance threshold / aggressiveness scalar
  - If not present: add one scalar that modifies merge acceptance rule.

**MVP parameter vector θ includes:**
- Per-node (sensor,lane) means: `T̄, ā, b̄, v̄0`
- Per-ramp arrivals: Erlang2S params
- Optional mergeAgg per merge zone (if supported)

---

### 17.9 Option A vs Option B (No Commitment) — **RESOLVED**

**Commitment (MVP):** Use **Option A**: all-at-once `Δθ` + deterministic projection.

#### Projection algorithm (MVP, no QP solver)
1) Component-wise clipping to bounds
2) Trust-region scaling: `Δθ ← Δθ * min(1, r / ||Δθ||)`
3) Smoothness pass: Laplacian smoothing on lane-adjacent parameters:
   - `θ ← θ − η_smooth * L_graph * θ` (1–3 iterations)

This is stable, easy to implement, and avoids conflicting corrections blowing up.

Option B remains as a fallback if oscillations occur.

**Status:**
- [x] Option A selected
- [x] Projection method specified (clip + trust region + smoothing)

---

### 17.10 Error Feature Engineering — **RESOLVED**

**Commitment (MVP):** Features are **summaries over the scenario window**, not full 48-step sequences.

Per node `(s,ℓ)` compute:

#### Core error stats (flow & speed)
- `μ_e_flow`: mean(sim_flow − real_flow)
- `σ_e_flow`: std(sim_flow − real_flow)
- `μ_e_spd`: mean(sim_spd − real_spd)
- `σ_e_spd`: std(sim_spd − real_spd)

#### Timing features (computed via threshold congestion detection)
Define congestion when speed < `κ * freeflow_speed`, with `κ = 0.7` (MVP).
- `t_onset_real`, `t_onset_sim`: first time congestion occurs
- `Δt_onset = t_onset_sim − t_onset_real`
- `t_recover_real`, `t_recover_sim`: first recovery after congestion
- `Δt_recover = t_recover_sim − t_recover_real`

#### Shape features
- `min_spd_error` (most negative speed error)
- `max_flow_error`
- `corr(flow_error, speed_error)` (captures inconsistent regimes)

#### Context features
- time-of-day encoded as continuous `sin/cos(2π t/24h)`
- day-of-week one-hot (optional)
- ramp demand regime indicator (low/med/high from observed ramp flow)

**Feature vector per node size (MVP):** ~14–20 scalars/node.

**Status:**
- [x] Exact feature list
- [x] Timing feature computation specified

---

### 17.11 Multi-Objective Handling — **RESOLVED**

**Commitment (MVP):** Weighted sum objective (single scalar) to keep optimization stable.

Let:
- `J_flow = mean over nodes of NRMSE(flow)`
- `J_spd  = mean over nodes of NRMSE(speed)`
- `J_time = mean over nodes of |Δt_onset| + |Δt_recover|` (scaled)

**MVP objective:**
- `J = 0.5 * J_flow + 0.5 * J_spd + 0.2 * J_time`

(If you already use a fitness like `0.5*flow + 0.5*speed`, keep that and add timing later.)

**Status:**
- [x] J specified as weighted sum
- [x] Includes timing penalty (optional but defined)

---

### 17.12 Imitation Dataset Construction — **RESOLVED**

**Teacher optimizer (MVP):** SPSA (since it’s already aligned with your calibration workflow)

**What is the label Δθ_teacher?**
- Use **final delta**: `Δθ_teacher = θ_best − θ_init` for the scenario.
- Optionally include trajectory steps later for richer supervision.

**How many teacher runs?**
- MVP dataset: `N = 100 scenarios`
  - e.g., 10 days × 3 blocks/day × multiple seeds (or multiple initial θ’s)

**Context variation**
- Ensure scenarios include:
  - AM peak blocks
  - Midday blocks
  - PM peak blocks
  - at least 2–3 different congestion regimes

**Status:**
- [x] Teacher optimizer specified
- [x] Label definition specified
- [x] Dataset size target specified

---

### 17.13 GNN Architecture — **RESOLVED**

**Commitment (MVP):** Use a **message passing network with sigmoid gating**, no attention, no softmax.

#### Layer equation (per edge j→i)
- Message: `m_ij = W_m h_j`
- Gate: `g_ij = sigmoid( w_g^T [h_i || h_j || e_ij] )`
- Aggregate: `M_i = Σ_{j∈N(i)} (g_ij * m_ij)`  (NOT normalized)
- Update: `h_i ← ReLU( W_s h_i + M_i )`

#### Depth / width
- Layers: `K = 2`
- Hidden dim: `d = 64`
- Activation: ReLU

#### Output head
- Per-node output: `Δθ_node = W_o h_i`
- Map node outputs to parameter groups:
  - lane-wise params: keep per-node
  - global params (arrival): pool over relevant nodes then output

**Status:**
- [x] GNN type defined (gated MPNN)
- [x] No attention/softmax
- [x] Layer count and dims defined

---

### 17.14 Compute Budget — **RESOLVED (MVP numbers)**

Assume:
- `Tsim = 17 min` per 12h run (example), but scenario is 3h ⇒ likely less; keep conservative.

MVP training:
- Stage 1 imitation: cost dominated by teacher optimizations:
  - 100 scenarios × (optimizer steps)
- Stage 2 improvement fine-tune:
  - per scenario update uses `Nsims/update = 3` runs (CRN)
  - if 1 update/scenario/epoch:
    - 100 scenarios × 3 sims = 300 sims/epoch

If each scenario sim costs ~5–10 minutes:
- 300 sims ≈ 25–50 hours serial, but trivially parallel across scenarios on HPC.

**Parallelization:**
- scenarios are independent → embarrassingly parallel

**Status:**
- [x] Stage-level budget structure defined
- [x] Parallelization plan defined

---

### 17.15 Success Criteria Thresholds — **RESOLVED**

Define success per tier:

#### Tier 1 (held-out days)
- **Primary:** ≥ 10% reduction in J vs baseline calibrated-without-learner
- **Safety:** no lane/node worsens by > 2% in NRMSE (avoid “average win, local loss”)

#### Tier 2 (held-out sensors)
- ≥ 5% reduction in J on held-out sensors vs baseline

#### Parameter stability
- coefficient of variation (CV) of key parameters across epochs:
  - CV(T̄) < 10%
  - CV(arrival params) < 10%

If stability fails, system is not trustworthy even if J improves.

**Status:**
- [x] Pass/fail thresholds defined

---

## 18) Summary: Document Status (Updated)

| Aspect | Status |
|--------|--------|
| Core philosophy | ✅ Complete |
| Supervision sources | ✅ Complete |
| Evaluation splits | ✅ Complete |
| Identifiability | ✅ Complete |
| Algorithm pseudocode | ✅ Gradient approach specified via Stage 1 + optional ES |
| Graph specification | ✅ Construction rules specified |
| Feature engineering | ✅ Specified |
| GNN architecture | ✅ Specified (no softmax) |
| Stochasticity handling | ✅ CRN + ensemble evaluation |
| Failure recovery | ✅ Guardrails specified |
| Compute budget | ✅ MVP plan defined |
| Success thresholds | ✅ Defined |

---

## 19) MVP Implementation Checklist (Ordered)

1) **Define scenario blocks** (3-hour windows) and seeds
2) **Implement error feature extractor** (Section 17.10)
3) **Run teacher optimizer** per scenario to build imitation dataset (Section 17.12)
4) **Train gated MPNN** on `(features → Δθ_teacher)` (Stage 1)
5) **Integrate projection layer** (clip + trust region + smoothing) (Section 17.9)
6) **Evaluate Tier 1 held-out days**
7) Optional: **ES fine-tuning** on φ with improvement reward (Section 17.1)
8) Evaluate Tier 2 sensors and Tier 3 corridor transfer

---

## 20) One-Line Reality Check (Still Applies)

If the learner outputs garbage, the simulator must still obey traffic laws.
If not, the architecture is invalid.


