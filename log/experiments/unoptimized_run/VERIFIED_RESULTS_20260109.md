# VERIFIED UNOPTIMIZED RESULTS - January 9, 2026
## Source: log/experiments/unoptimized_run/*_fitness.txt
## Parameters: VectorD(5.0, 4.0, -2.0, 3.0, 0.5)

---

## COMPLETE RESULTS TABLE (All 16 Experiments)

| Rank | Experiment | Arrival | Integrator | Fitness | Flow R² | Speed R² | Flow NRMSE | Speed NRMSE | Duration (s) | Duration (min) |
|------|------------|---------|------------|---------|---------|----------|------------|-------------|--------------|----------------|
| 1 | erlang2s_ballistic | Erlang2S | Ballistic | 0.1567 | 0.8367 | 0.4846 | 0.0762 | 0.2371 | 1051 | 17.5 |
| 2 | erlang2s_rk4 | Erlang2S | RK4 | 0.1572 | 0.8384 | 0.4795 | 0.0758 | 0.2386 | 3055 | 50.9 |
| 3 | erlang2s_rk3 | Erlang2S | RK3 | 0.1574 | 0.8376 | 0.4782 | 0.0759 | 0.2388 | 2883 | 48.1 |
| 4 | erlang2s_rk2 | Erlang2S | RK2 | 0.1575 | 0.8391 | 0.4764 | 0.0756 | 0.2393 | 2903 | 48.4 |
| 5 | erlang2s_heun | Erlang2S | Heun | 0.1578 | 0.8372 | 0.4752 | 0.0760 | 0.2396 | 2689 | 44.8 |
| 6 | erlang2s_euler | Erlang2S | Euler | 0.1579 | 0.8331 | 0.4778 | 0.0769 | 0.2388 | 2655 | 44.3 |
| 7 | erlang2s_dopri5 | Erlang2S | DOPRI5 | 0.1582 | 0.8341 | 0.4741 | 0.0766 | 0.2399 | 3066 | 51.1 |
| 8 | poisson_rk2 | Poisson | RK2 | 0.1754 | 0.7341 | 0.4527 | 0.1054 | 0.2453 | 2922 | 48.7 |
| 9 | poisson_heun | Poisson | Heun | 0.1755 | 0.7332 | 0.4523 | 0.1056 | 0.2455 | 3452 | 57.5 |
| 10 | poisson_rk3 | Poisson | RK3 | 0.1756 | 0.7318 | 0.4528 | 0.1057 | 0.2454 | 3132 | 52.2 |
| 11 | poisson_rk4 | Poisson | RK4 | 0.1756 | 0.7316 | 0.4523 | 0.1057 | 0.2455 | 3101 | 51.7 |
| 12 | poisson_ballistic | Poisson | Ballistic | 0.1759 | 0.7341 | 0.4465 | 0.1051 | 0.2467 | 594 | 9.9 |
| 13 | poisson_euler | Poisson | Euler | 0.1761 | 0.7320 | 0.4500 | 0.1058 | 0.2464 | 2623 | 43.7 |
| 14 | poisson_dopri5 | Poisson | DOPRI5 | 0.1765 | 0.7295 | 0.4483 | 0.1062 | 0.2467 | 3611 | 60.2 |
| 15 | erlang2s_butcher | Erlang2S | Butcher | 0.1998 | 0.1841 | 0.4757 | 0.1648 | 0.2348 | 1199 | 20.0 |
| 16 | poisson_butcher | Poisson | Butcher | 0.2055 | 0.0989 | 0.4952 | 0.1808 | 0.2302 | 1157 | 19.3 |

---

## KEY FINDINGS

### Finding 1: Integrator Choice Has Minimal Impact
**Erlang2S experiments (excluding Butcher):**
- Best: Ballistic (0.1567)
- Worst: DOPRI5 (0.1582)
- Range: 0.0015 (< 1% variation)

**Poisson experiments (excluding Butcher):**
- Best: RK2 (0.1754)
- Worst: DOPRI5 (0.1765)
- Range: 0.0011 (< 1% variation)

### Finding 2: Arrival Process Matters Significantly
- Best Erlang2S: 0.1567 (erlang2s_ballistic)
- Best Poisson: 0.1754 (poisson_rk2)
- **Improvement: 10.7%** ((0.1754 - 0.1567) / 0.1754 × 100)

### Finding 3: Butcher Implementation Has Issues
- erlang2s_butcher: Flow R² = 0.1841 (very poor)
- poisson_butcher: Flow R² = 0.0989 (extremely poor)
- **Recommendation: Exclude Butcher from main analysis**

---

## TABLE FOR PAPER (Erlang2S Only, Excluding Butcher)

| Integrator | Duration (min) | Fitness | Flow R² | Speed R² |
|------------|----------------|---------|---------|----------|
| **Ballistic** | **17.5** | **0.1567** | 0.8367 | 0.4846 |
| RK4 | 50.9 | 0.1572 | 0.8384 | 0.4795 |
| RK3 | 48.1 | 0.1574 | 0.8376 | 0.4782 |
| RK2 | 48.4 | 0.1575 | 0.8391 | 0.4764 |
| Heun | 44.8 | 0.1578 | 0.8372 | 0.4752 |
| Euler | 44.3 | 0.1579 | 0.8331 | 0.4778 |
| DOPRI5 | 51.1 | 0.1582 | 0.8341 | 0.4741 |

**Key insight:** Ballistic is 3x faster and has the best fitness.

---

## ERLANG2S VS POISSON COMPARISON

| Metric | Erlang2S (best) | Poisson (best) | Improvement |
|--------|-----------------|----------------|-------------|
| Fitness | 0.1567 | 0.1754 | 10.7% |
| Flow R² | 0.8367 | 0.7341 | +14.0% |
| Speed R² | 0.4846 | 0.4527 | +7.0% |
| Flow NRMSE | 0.0762 | 0.1051 | 27.5% better |
| Speed NRMSE | 0.2371 | 0.2467 | 3.9% better |

---

## DATA VERIFICATION CHECKLIST

- [x] All 16 experiments have fitness files
- [x] poisson_euler now shows "Arrival: Poisson" (FIXED)
- [x] All experiments use same parameters: VectorD(5.0, 4.0, -2.0, 3.0, 0.5)
- [x] No negative R² values in aggregate metrics
- [x] Butcher has known issues (poor Flow R²) - document separately

