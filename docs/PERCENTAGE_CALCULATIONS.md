# Percentage Improvement Calculations

**Date:** January 8, 2026  
**Purpose:** Transparent calculations for all improvement claims

---

## Excel-Ready Tables (Tab-Separated - Copy & Paste to Excel)

### Table 1: Baseline vs Optimized (Butcher) - Overall Metrics

```
Metric	Baseline (Literature Defaults)	Optimized (Butcher)	Improvement	Formula
Fitness	0.1567	0.1123	28.3%	=(0.1567-0.1123)/0.1567*100
Count R²	0.8367	0.8216	-1.8%	=(0.8216-0.8367)/0.8367*100
Speed R²	0.4846	0.7346	51.6%	=(0.7346-0.4846)/0.4846*100
Count NRMSE	0.0766	0.0794	-3.7%	=(0.0766-0.0794)/0.0766*100
Speed NRMSE	0.2371	0.1453	38.7%	=(0.2371-0.1453)/0.2371*100
Duration (sec)	1051	297	71.7%	=(1051-297)/1051*100
```

---

### Table 2: Butcher vs Ballistic (Same Parameters) - Speed R² Per Sensor

```
Sensor	Ballistic Speed R²	Butcher Speed R²	Absolute Diff	% Improvement	Formula
0	0.6519	0.9740	0.3221	49.4%	=(0.9740-0.6519)/0.6519*100
1	0.5399	0.9205	0.3806	70.5%	=(0.9205-0.5399)/0.5399*100
2	0.4986	0.8221	0.3235	64.9%	=(0.8221-0.4986)/0.4986*100
3	0.4078	0.6731	0.2653	65.1%	=(0.6731-0.4078)/0.4078*100
4	0.3249	0.2832	-0.0417	-12.8%	=(0.2832-0.3249)/0.3249*100
Avg	0.4846	0.7346	0.2500	51.6%	=(0.7346-0.4846)/0.4846*100
```

---

### Table 3: Erlang2S vs Poisson (Ballistic Integrator)

```
Metric	Poisson	Erlang2S	Improvement	Formula
Fitness	0.1759	0.1567	10.9%	=(0.1759-0.1567)/0.1759*100
Count R²	0.7341	0.8367	14.0%	=(0.8367-0.7341)/0.7341*100
Speed R²	0.4465	0.4846	8.5%	=(0.4846-0.4465)/0.4465*100
Count NRMSE	0.1052	0.0766	27.2%	=(0.1052-0.0766)/0.1052*100
Speed NRMSE	0.2467	0.2371	3.9%	=(0.2467-0.2371)/0.2467*100
```

---

### Table 4: Per-Sensor Metrics (Best Configuration: erlang2s_butcher)

```
Sensor	Count NRMSE	Speed NRMSE	Count R²	Speed R²	Count SMAPE	Speed SMAPE
0	0.0421	0.0563	0.9514	0.9740	3.27%	5.39%
1	0.0677	0.0981	0.8574	0.9205	5.51%	8.11%
2	0.0847	0.1356	0.8115	0.8221	7.77%	10.15%
3	0.0961	0.1781	0.7563	0.6731	8.72%	11.95%
4	0.1061	0.2584	0.7313	0.2832	9.48%	13.56%
Avg	0.0794	0.1453	0.8216	0.7346	6.95%	9.83%
```

---

### Table 5: Full Experiment Results (Ranked by Fitness)

```
Rank	Experiment	Fitness	Count R²	Speed R²	Count NRMSE	Speed NRMSE	Duration (sec)	Duration (min)
1	erlang2s_butcher	0.1123	0.8216	0.7346	0.0794	0.1453	297	4.9
2	erlang2s_ballistic	0.1567	0.8367	0.4846	0.0766	0.2371	1051	17.5
3	erlang2s_rk4	0.1572	0.8384	0.4795	0.0758	0.2386	3055	50.9
4	erlang2s_rk3	0.1574	0.8376	0.4782	0.0759	0.2388	2883	48.0
5	erlang2s_rk2	0.1575	0.8391	0.4764	0.0756	0.2393	2903	48.4
6	erlang2s_dopri5	0.1582	0.8341	0.4741	0.0766	0.2399	3066	51.1
7	poisson_rk2	0.1754	0.7341	0.4527	0.1054	0.2453	2922	48.7
8	poisson_rk3	0.1756	0.7318	0.4528	0.1057	0.2454	3132	52.2
9	poisson_rk4	0.1756	0.7316	0.4523	0.1057	0.2455	3101	51.7
10	poisson_ballistic	0.1759	0.7341	0.4465	0.1052	0.2467	594	9.9
11	poisson_dopri5	0.1765	0.7295	0.4483	0.1062	0.2467	3611	60.2
```

---

### Table 6: Key Claims with Formulas

```
Claim	Value A	Value B	Calculation	Result	Formula in Excel
Fitness improvement (Butcher vs Defaults)	0.1567	0.1123	(A-B)/A	28.3%	=(A2-B2)/A2*100
Speed R² improvement (Butcher vs Defaults)	0.4846	0.7346	(B-A)/A	51.6%	=(B2-A2)/A2*100
Fitness improvement (Erlang2S vs Poisson)	0.1759	0.1567	(A-B)/A	10.9%	=(A2-B2)/A2*100
Runtime improvement (Butcher vs RK4)	3055	297	(A-B)/A	90.3%	=(A2-B2)/A2*100
Integrator effect (within Erlang2S)	0.1582	0.1567	(A-B)/A	0.9%	=(A2-B2)/A2*100
Arrival process effect	0.1759	0.1567	(A-B)/A	10.9%	=(A2-B2)/A2*100
```

---

## Summary of Key Percentages

| Claim | Percentage | Verified By |
|-------|------------|-------------|
| **Fitness improvement (Butcher vs Literature Defaults)** | **28.3%** | (0.1567 - 0.1123) / 0.1567 = 0.283 |
| **Speed R² improvement (Butcher vs Literature Defaults)** | **51.6%** | (0.7346 - 0.4846) / 0.4846 = 0.516 |
| **Fitness improvement (Erlang2S vs Poisson)** | **10.9%** | (0.1759 - 0.1567) / 0.1759 = 0.109 |
| **Integrator effect (within same arrival process)** | **<1%** | Max difference among RK2/RK3/RK4/DOPRI5 |
| **Butcher vs Ballistic (same params, Erlang2S)** | **28.3%** | (0.1567 - 0.1123) / 0.1567 = 0.283 |

---

## Notes for PI

1. **Baseline**: Literature defaults from Treiber & Kesting (2013) using Ballistic integrator with Erlang2S arrivals
2. **Fitness formula**: `0.5 × Count_NRMSE + 0.5 × Speed_NRMSE` (lower is better)
3. **R² interpretation**: Higher is better (1.0 = perfect fit)
4. **NRMSE interpretation**: Lower is better (0.0 = perfect fit)
5. **All percentages calculated as**: `(Old - New) / Old × 100` for metrics where lower is better, or `(New - Old) / Old × 100` for metrics where higher is better

---

## How to Use in Excel

1. Copy any table between the ``` marks
2. Paste into Excel (Ctrl+V)
3. Select Data → Text to Columns → Delimited → Tab
4. Verify formulas in column F match your calculations

