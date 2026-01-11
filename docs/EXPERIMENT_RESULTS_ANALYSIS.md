# Experimental Results Analysis: Integrators × Arrival Processes

**Date:** January 7, 2026  
**Total Runtime:** ~7.5 hours (10 experiments)  
**Parameters:** s₀=5.0, a=4.0, b=-2.0, T=3.0, τ=0.5

---

## Executive Summary

We conducted a systematic comparison of **5 numerical integrators** × **2 arrival processes** = **10 experiments** to evaluate their impact on microscopic traffic simulation accuracy.

### Key Findings

1. **Arrival process dominates accuracy:** Erlang2S achieves 11% lower fitness (better) than Poisson across all integrators
2. **Integrator choice has negligible impact:** Within each arrival type, fitness varies by only 1%
3. **Ballistic is 3× faster:** Equivalent accuracy at one-third the computational cost
4. **DOPRI5 slightly underperforms:** Higher-order accuracy does not translate to better simulation results

---

## Results Summary Table

### Ranked by Fitness (Lower is Better)

| Rank | Experiment | Fitness | Count R² | Speed R² | Count NRMSE | Speed NRMSE | Duration (s) | Duration (min) |
|------|------------|---------|----------|----------|-------------|-------------|--------------|----------------|
| 1 | erlang2s_ballistic | 0.1567 | 0.8367 | 0.4846 | 0.0766 | 0.2371 | 1,051 | 17.5 |
| 2 | erlang2s_rk4 | 0.1572 | 0.8384 | 0.4795 | 0.0758 | 0.2386 | 3,055 | 50.9 |
| 3 | erlang2s_rk3 | 0.1574 | 0.8376 | 0.4782 | 0.0759 | 0.2388 | 2,883 | 48.0 |
| 4 | erlang2s_rk2 | 0.1575 | 0.8391 | 0.4764 | 0.0756 | 0.2393 | 2,903 | 48.4 |
| 5 | erlang2s_dopri5 | 0.1582 | 0.8341 | 0.4741 | 0.0766 | 0.2399 | 3,066 | 51.1 |
| 6 | poisson_rk2 | 0.1754 | 0.7341 | 0.4527 | 0.1054 | 0.2453 | 2,922 | 48.7 |
| 7 | poisson_rk3 | 0.1756 | 0.7318 | 0.4528 | 0.1057 | 0.2454 | 3,132 | 52.2 |
| 8 | poisson_rk4 | 0.1756 | 0.7316 | 0.4523 | 0.1057 | 0.2455 | 3,101 | 51.7 |
| 9 | poisson_ballistic | 0.1759 | 0.7341 | 0.4465 | 0.1052 | 0.2467 | 594 | 9.9 |
| 10 | poisson_dopri5 | 0.1765 | 0.7295 | 0.4483 | 0.1062 | 0.2467 | 3,611 | 60.2 |

---

## Excel-Compatible Table (Tab-Separated)

Copy the block below and paste directly into Excel:

```
Rank	Experiment	Fitness	Count_R2	Speed_R2	Count_NRMSE	Speed_NRMSE	Duration_sec	Duration_min
1	erlang2s_ballistic	0.1567	0.8367	0.4846	0.0766	0.2371	1051	17.5
2	erlang2s_rk4	0.1572	0.8384	0.4795	0.0758	0.2386	3055	50.9
3	erlang2s_rk3	0.1574	0.8376	0.4782	0.0759	0.2388	2883	48.0
4	erlang2s_rk2	0.1575	0.8391	0.4764	0.0756	0.2393	2903	48.4
5	erlang2s_dopri5	0.1582	0.8341	0.4741	0.0766	0.2399	3066	51.1
6	poisson_rk2	0.1754	0.7341	0.4527	0.1054	0.2453	2922	48.7
7	poisson_rk3	0.1756	0.7318	0.4528	0.1057	0.2454	3132	52.2
8	poisson_rk4	0.1756	0.7316	0.4523	0.1057	0.2455	3101	51.7
9	poisson_ballistic	0.1759	0.7341	0.4465	0.1052	0.2467	594	9.9
10	poisson_dopri5	0.1765	0.7295	0.4483	0.1062	0.2467	3611	60.2
```

---

## Comparison by Arrival Process

### Erlang2S (Shifted Erlang-2)

| Integrator | Fitness | Count R² | Speed R² | Duration (s) |
|------------|---------|----------|----------|--------------|
| Ballistic | **0.1567** | 0.8367 | **0.4846** | **1,051** |
| RK4 | 0.1572 | **0.8384** | 0.4795 | 3,055 |
| RK3 | 0.1574 | 0.8376 | 0.4782 | 2,883 |
| RK2 | 0.1575 | 0.8391 | 0.4764 | 2,903 |
| DOPRI5 | 0.1582 | 0.8341 | 0.4741 | 3,066 |

**Range:** 0.1567 – 0.1582 (Δ = 0.0015, **<1% variation**)

### Poisson (Exponential)

| Integrator | Fitness | Count R² | Speed R² | Duration (s) |
|------------|---------|----------|----------|--------------|
| RK2 | **0.1754** | 0.7341 | 0.4527 | 2,922 |
| RK3 | 0.1756 | 0.7318 | 0.4528 | 3,132 |
| RK4 | 0.1756 | **0.7316** | 0.4523 | 3,101 |
| Ballistic | 0.1759 | 0.7341 | 0.4465 | **594** |
| DOPRI5 | 0.1765 | 0.7295 | 0.4483 | 3,611 |

**Range:** 0.1754 – 0.1765 (Δ = 0.0011, **<0.7% variation**)

---

## Comparison by Integrator

| Integrator | Erlang2S Fitness | Poisson Fitness | Δ (Erlang2S advantage) |
|------------|------------------|-----------------|------------------------|
| Ballistic | 0.1567 | 0.1759 | -0.0192 (10.9%) |
| RK2 | 0.1575 | 0.1754 | -0.0179 (10.2%) |
| RK3 | 0.1574 | 0.1756 | -0.0182 (10.4%) |
| RK4 | 0.1572 | 0.1756 | -0.0184 (10.5%) |
| DOPRI5 | 0.1582 | 0.1765 | -0.0183 (10.4%) |

**Conclusion:** Erlang2S consistently outperforms Poisson by ~10-11% across all integrators.

---

## Computational Efficiency

| Integrator | Avg Duration (s) | Relative Speed |
|------------|------------------|----------------|
| Ballistic | 823 | **1.0× (baseline)** |
| RK2 | 2,913 | 3.5× slower |
| RK3 | 3,008 | 3.7× slower |
| RK4 | 3,078 | 3.7× slower |
| DOPRI5 | 3,339 | 4.1× slower |

**Ballistic is 3-4× faster than all other integrators with equivalent or better accuracy.**

---

## Sensor-Level Breakdown (Erlang2S + DOPRI5)

| Sensor | Count NRMSE | Speed NRMSE | Count R² | Speed R² |
|--------|-------------|-------------|----------|----------|
| S1 | 0.0425 | 0.2216 | 0.9519 | 0.6140 |
| S2 | 0.0647 | 0.2334 | 0.8669 | 0.5648 |
| S3 | 0.0818 | 0.2398 | 0.8224 | 0.4811 |
| S4 | 0.0920 | 0.2474 | 0.7767 | 0.4072 |
| S5 | 0.1019 | 0.2571 | 0.7524 | 0.3034 |

**Observation:** Accuracy degrades downstream (S1 → S5), likely due to error accumulation and on-ramp merge effects.

---

## Conclusions

### 1. Arrival Process Matters Most
- Erlang2S achieves **11% lower NRMSE** than Poisson
- The minimum headway constraint in Erlang2S prevents unrealistic bunching
- This validates the shifted Erlang-2 methodological contribution

### 2. Integrator Choice is Secondary
- All integrators produce **within 1% of each other**
- Higher-order methods (DOPRI5, RK4) do not improve practical accuracy
- Consistent with Treiber et al. findings on braking discontinuities

### 3. Ballistic is the Pragmatic Choice
- **3× faster** than higher-order methods
- **Best or near-best fitness** in both arrival categories
- Recommended for production simulations and calibration runs

### 4. DOPRI5 Provides Theoretical Rigor
- Appropriate for validation studies requiring provable numerical accuracy
- Not recommended for routine calibration due to computational cost

---

## Paper Framing

> While we initially hypothesized that higher-order Dormand-Prince integration would improve micro-level accuracy, our experimental comparison across five integrators reveals that accuracy differences are within 1% (Table X). In contrast, the choice of arrival process dominates: Shifted Erlang-2 achieves 11% lower NRMSE than Poisson arrivals across all integrators. Ballistic integration, despite its O(Δt) theoretical error, performs competitively while running 3× faster—consistent with findings by Treiber et al. that braking discontinuities limit practical gains from higher-order schemes.

---

## Raw Data Files

All experimental data is saved in `log/experiments/`:

| File | Description |
|------|-------------|
| `erlang2s_dopri5_data.csv` | Raw 15-min sensor data |
| `erlang2s_dopri5_fitness.txt` | Fitness metrics |
| `experiment_summary.txt` | Final ranking table |
| ... | (10 experiments total) |

