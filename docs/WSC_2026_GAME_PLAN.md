# Winter Simulation Conference (WSC) 2026 - Game Plan

**Conference Date:** March 2026
**Created:** January 9, 2026
**Author:** Korede Bishi

---

## 📌 PAPER CONCEPT

**Working Title:**
*"Comparative Evaluation of Car-Following Models and Arrival Processes for Microscopic Traffic Simulation"*

**Key Differentiator from ANNSIM:**
- ANNSIM: Focused on numerical integrators + Erlang2S arrival
- WSC: Expand to multiple car-following models + multiple arrival processes

---

## 🎯 PROPOSED CONTRIBUTIONS

### Contribution 1: Multiple Car-Following Models

| Model | Type | Status | Notes |
|-------|------|--------|-------|
| IDM | Continuous | ✅ Done | Current workhorse |
| Gipps | Rule-based | ✅ Done | Already in Dynamics.scala |
| Krause/FVD | Continuous | ❌ TODO | Full Velocity Difference Model |
| Wiedemann | Psychophysical | ❌ Optional | VISSIM-style |

**Krause/FVD Model Equation:**
```
a_n = a_max[1 - (v_n/v_0)^δ - (s*/s_n)²] + λΔv
```
Where `λΔv` is the additional velocity difference term vs IDM.

---

### Contribution 2: Multiple Arrival Processes

| Model | Description | Status | Priority |
|-------|-------------|--------|----------|
| Poisson | Exponential inter-arrivals | ✅ Done | Baseline |
| Erlang2S | Shifted Erlang k=2 | ✅ Done | Current best |
| Erlang3S | Shifted Erlang k=3 | ❌ TODO | High |
| Erlang4S | Shifted Erlang k=4 | ❌ TODO | Medium |
| Log-Normal | Skewed, heavy-tailed | ❌ TODO | High |
| Shifted Exponential | Exponential + min headway | ❌ TODO | Medium |
| Gamma | Generalized Erlang | ❌ TODO | Low |
| Weibull | Flexible hazard rate | ❌ TODO | Low |

---

### Contribution 3: Expanded Validation

| Corridor | Status | Sensors |
|----------|--------|---------|
| US-101 Donald Doyle | ✅ Done | 5 mainline + 2 ramps |
| US-101 Redwood Creek | ❌ TODO | Data exists in `data/` folder |

---

## 📊 EXPERIMENT MATRIX

### Full Comparison (if time permits)

| Car-Following | Arrival | Integrator | Total Experiments |
|---------------|---------|------------|-------------------|
| IDM | 6 arrivals | 7 integrators | 42 |
| Krause | 6 arrivals | Ballistic only | 6 |
| **Total** | | | **48 experiments** |

### Minimum Viable Comparison

| Car-Following | Arrival | Integrator | Total Experiments |
|---------------|---------|------------|-------------------|
| IDM | 4 arrivals | Ballistic | 4 |
| Krause | 4 arrivals | Ballistic | 4 |
| **Total** | | | **8 experiments** |

---

## 📅 TIMELINE (8 Weeks)

| Week | Dates | Tasks |
|------|-------|-------|
| 1 | Jan 13-19 | Finish ANNSIM, rest, review |
| 2 | Jan 20-26 | Implement Krause model in Dynamics.scala |
| 3 | Jan 27 - Feb 2 | Implement new arrival models (Erlang3S, LogNormal, ShiftedExp) |
| 4 | Feb 3-9 | Run experiments (all model/arrival combos) |
| 5 | Feb 10-16 | Write Introduction + Methodology |
| 6 | Feb 17-23 | Write Results + Discussion |
| 7 | Feb 24 - Mar 2 | Generate figures, polish tables |
| 8 | Mar 3-9 | Final review, submit |

---

## 🔧 IMPLEMENTATION TASKS

### Task 1: Krause Model (Dynamics.scala)
- [ ] Add `IntegratorType.krause` or separate `KrauseDynamics`
- [ ] Implement Krause acceleration formula
- [ ] Add λ (lambda) parameter to Vehicle properties
- [ ] Test single vehicle following scenario

### Task 2: New Arrival Models (Variate.scala)
- [ ] `Erlang3S` - Shifted Erlang with k=3
- [ ] `Erlang4S` - Shifted Erlang with k=4
- [ ] `LogNormalS` - Shifted Log-Normal
- [ ] `ShiftedExponential` - Exponential with minimum headway
- [ ] Update `MultiVSource.scala` to support new variates

### Task 3: Experiments
- [ ] Create experiment runner for all combinations
- [ ] Run on Sapelo2 (faster for large experiment matrix)
- [ ] Collect fitness.txt files for all experiments

### Task 4: Analysis
- [ ] Create comparison tables (like ANNSIM)
- [ ] Statistical significance tests (if needed)
- [ ] Generate time series figures

---

## 📁 FILE LOCATIONS

| Component | File |
|-----------|------|
| Car-following models | `src/main/scala/scalation/simulation/process/Dynamics.scala` |
| Arrival distributions | `src/main/scala/scalation/random/Variate.scala` |
| Vehicle source | `src/main/scala/scalation/simulation/process/MultiVSource.scala` |
| Experiment runner | `src/main/scala/scalation/simulation/process/CalibrateCalRoute101.scala` |
| Redwood Creek data | `data/Mainline_VDS_Redwood_Creek_US101-N/` |

---

## 📝 REFERENCES TO ADD

- Krause original paper
- FVD model papers
- Log-Normal headway distribution papers (Li & Chen 2017)
- Gamma/Weibull headway papers

---

## 🎯 SUCCESS CRITERIA

1. **Krause model** implemented and validated
2. **At least 2 new arrival models** (Erlang3S + LogNormal)
3. **Clear comparison table** showing which combo is best
4. **Submitted on time** to WSC

---

## 📋 NOTES

- Ballistic is fastest integrator → use for model comparisons
- ANNSIM found integrator choice doesn't matter (<1% variation)
- Focus WSC on car-following model and arrival process choices
- Keep using unoptimized parameters for consistency (no negative R² issues)

---

## 🚀 NEXT ACTION

After ANNSIM submission (Jan 12):
1. Read Krause/FVD papers
2. Sketch implementation in Dynamics.scala
3. Discuss scope with PI

