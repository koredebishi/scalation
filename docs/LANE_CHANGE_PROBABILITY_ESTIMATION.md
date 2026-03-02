# Lane-Change Probability Estimation (15-Minute PEMS Data)

---

## ⚡ Performance Notes

| Configuration | Runtime (12-hour sim) |
|---------------|----------------------|
| Default threads | ~9 min |
| Virtual threads (`virtualThread = true`) | ~3 min (**3x speedup**) |

**Recommendation:** Always enable virtual threads for calibration runs.

---

## 🎯 Fitness Function (Current Implementation)

$$\text{Fitness} = 0.5 \times \text{NRMSE}_{\text{flow}} + 0.5 \times \text{NRMSE}_{\text{speed}}$$

Where NRMSE is computed as percentage (×100) and averaged across 5 sensors.

**Alternative fitness options to consider:**
- SMAPE-based: `0.5 * (SMAPE_flow + SMAPE_speed)`
- R²-based (maximize): `1 - 0.5 * (R²_flow + R²_speed)`
- Time-weighted (see sampling below)

---

## ⏱️ Time-Period Sampling Strategy

Full 12-hour simulation (6am-6pm) is computationally expensive for calibration. 

**PI's Suggested Sampling Approach:**

| Period | Time Range | Weight | Rationale |
|--------|------------|--------|-----------|
| AM Peak | 7:00 - 9:00 | 40% | Congestion build-up |
| Midday | 11:00 - 13:00 | 20% | Free-flow validation |
| PM Peak | 16:00 - 18:00 | 40% | Congestion dissipation |

**Weighted Fitness:**
$$\text{Fitness}_{\text{weighted}} = 0.4 \cdot f_{\text{AM}} + 0.2 \cdot f_{\text{midday}} + 0.4 \cdot f_{\text{PM}}$$

**My Recommendations:**

1. **For initial calibration:** Use AM + PM peaks only (skip midday)
   - Covers congestion dynamics
   - Reduces runtime by ~50%

2. **For validation:** Run full 12-hour after optimization converges
   - Confirms parameters generalize across all periods

3. **Warm-start strategy:**
   - First: Optimize on AM peak alone (fast, 2-3 intervals)
   - Then: Use best params as starting point for full-period optimization

---

## 🧹 Data Preparation
- [ ] Collect **upstream and downstream lane flows** for all 4 lanes
- [ ] Verify **15-minute aggregation** is consistent across detectors
- [ ] Check flow conservation:
    $$\sum_l q_l^{up} \approx \sum_l q_l^{down}$$
- [ ] Flag time bins with ramps, incidents, or missing data

## 📊 Flow Imbalance Computation
- [ ] Compute lane imbalance per 15-min bin:
    $$\Delta q_l = q_l^{down} - q_l^{up}$$
- [ ] Validate:
    $$\sum_l \Delta q_l \approx 0$$ (discard or smooth if violated)

## 🔁 Lane-Change Flow Model (Flows Only)
- [ ] Assume **adjacent-lane changes only**
- [ ] Define lane-change outflow:
    $$F_{l,\text{out}} = p_l \cdot q_l^{up}$$
- [ ] Set up lane balance equations for 4 lanes
- [ ] Estimate $p_l$ by constrained least squares ($p_l \ge 0$)

## 📈 Probability Post-Processing
- [ ] Interpret $p_l$ as **15-min average lane-change probability**
- [ ] Apply aggregation correction factor (start with **×1.3**)
- [ ] Clip to realistic bounds (e.g., $p_l \le 0.1$)

## 🔄 Conversion for Microsimulation
- [ ] Keep $p_l$ **piecewise constant** per 15-min interval
- [ ] Convert to per-second probability if required:
    $$p_l^{sec} = 1 - (1 - p_l)^{1/900}$$
- [ ] Map probabilities to simulator lane-change parameters

## 🚦 Simulation Validation
- [ ] Run AM peak only → check lane flow splits
- [ ] Run PM peak only → check queue dissipation
- [ ] Run full 6am–6pm simulation
- [ ] Compare simulated vs PEMS:
  - [ ] Lane flows
  - [ ] Lane speeds
  - [ ] Congestion duration
- [ ] Adjust correction factor if:
  - Too much weaving → reduce
  - Persistent imbalance → increase

## 📝 Documentation
- [ ] State identifiability limits (flows only)
- [ ] Document assumptions:
  - Adjacent lane changes only
  - Stationary within 15-min bins
- [ ] Report probabilities as **interval averages**, not instantaneous rates
