# SAZO Experiment Plan
## Shift-Aware Zeroth-Order Adaptation for Robust ETA Under Sensor & Distribution Shift

> **Purpose:** Viability check before committing to full paper write-up.
> **Decision rule:** If SAZO beats Adam-under-shift by ≥10% MAE reduction on at least 2 datasets → proceed to paper.
> **Current date:** March 2026
> **Target venue (if viable):** IEEE BigData 2026 → fallback ICDM 2026

---

## 1. The Story (Ward for Ward)

### Ward 1 — The Problem (1 paragraph, 1 figure)
> First-order optimizers (Adam, AdamW) for ETA assume that the gradient is an unbiased estimator of the true loss direction. Under sensor dropout, latency spikes, and demand shifts, this assumption breaks. The gradient becomes a biased, noisy signal — and Adam does not know this. It keeps updating. The updates actively worsen the model.

**Figure 1:** Learning curve of Adam on METR-LA with and without 20% sensor dropout injected at step 500. Show the collapse. This is the entire motivation.

---

### Ward 2 — Why Existing Fixes Are Not Enough (1 table, 1 paragraph)
> DRO requires knowing the shift set ahead of time. MAML requires meta-training. Test-time fine-tuning with Adam re-introduces the same noisy gradient problem. Data augmentation helps in training but not at deployment. None of these address the root cause: **the gradient itself is unreliable during shift.**

| Method | Knows shift structure? | Works at test time? | Gradient-free? |
|---|---|---|---|
| DRO | ✅ Required | ❌ | ❌ |
| MAML | ✅ Required | ✅ | ❌ |
| TTT (Test-Time Training) | ❌ | ✅ | ❌ |
| Data Augmentation | ❌ | ❌ | ❌ |
| **SAZO (ours)** | ❌ Not needed | ✅ | ✅ |

---

### Ward 3 — The Idea (1 algorithm box, 1 diagram)
> We propose SAZO: a **mode-switching wrapper** around any optimizer. During stationarity, Adam runs normally. When a distributional alarm fires (based on a sliding window KL divergence or MMD test on input features), SAZO switches Adam off and runs a zeroth-order random directional update. When stationarity is restored, Adam resumes.

**Key insight:** You do not replace Adam. You protect it from poisoned gradients.

```
SAZO Algorithm (Pseudocode)
─────────────────────────────────────────────────────────────────
Input: model θ, optimizer Adam, window W, threshold τ, radius δ
─────────────────────────────────────────────────────────────────
At each step t:
  1. Observe batch (x_t, y_t)
  2. Compute shift score S_t = MMD(W_t, W_{t-W})   ← sliding window
  3. IF S_t < τ:
       → STATIONARY MODE: run Adam step normally
  4. ELSE:
       → SHIFT MODE:
         a. Sample random direction u ~ N(0, I)           ← Nesterov-Spokoiny Gaussian smoothing
         b. Compute ZO gradient estimate (Nesterov-Spokoiny):
            g_ZO = (d / 2δ) · [L(θ + δu) - L(θ - δu)] · u
         c. Update: θ ← θ - η * g_ZO
         d. Do NOT update Adam's momentum buffers
  5. Update window W_t
─────────────────────────────────────────────────────────────────
```

**Why this works:** Under gradient bias ε, the ZO estimator error grows as O(δ²) while the Adam error grows as O(ε·t) unboundedly. For bounded shift duration, ZO wins.

---

### Ward 4 — Experiments (see full plan below)

---

### Ward 5 — Results & Claim
> SAZO reduces ETA degradation under sensor shift by X% over Adam with zero overhead during stationarity and no prior knowledge of shift structure required.

---

## 2. Datasets

### Primary (Must Use Both)

#### Dataset 1: METR-LA
- **What:** 207 loop detectors on Los Angeles highways, 4 months (Mar–Jun 2012)
- **Granularity:** 5-minute intervals, speed readings (mph)
- **Why reviewers accept it:** Standard benchmark since DCRNN (ICLR 2018). Every traffic ML paper uses it.
- **Shift scenarios you can inject:**
  - Sensor dropout: randomly zero out 10%, 20%, 30% of sensors per step
  - Gaussian noise injection: add N(0, σ²) to speed readings
  - Latency: shift sensor readings by 1–3 time steps
- **Download:** https://github.com/liyaguang/DCRNN (data/ folder)
- **Format:** `.h5` file, adjacency matrix included
- **Size:** ~50MB, easy to work with

#### Dataset 2: PEMS-BAY
- **What:** 325 sensors in Bay Area, 6 months (Jan–May 2017)
- **Granularity:** 5-minute intervals, speed readings
- **Why reviewers accept it:** Companion to METR-LA. Used in 100+ papers. Geographically distinct from METR-LA so you get spatial shift for free.
- **Shift scenarios you can inject:**
  - Node removal (road closure simulation): remove 5–15 nodes from graph
  - Corrupted sensor: replace 20% of readings with constant or zero
  - Temporal shift: train on Jan–Mar, test on Apr–May (seasonal demand change)
- **Download:** https://github.com/liyaguang/DCRNN (same repo)
- **Format:** `.h5` file, adjacency matrix included
- **Size:** ~100MB

### Secondary (Use if Time Allows — Strengthens Paper)

#### Dataset 3: NYC Taxi (TLC Trip Record Data)
- **What:** Yellow cab trip records, multi-year (2018–2023)
- **Why:** Enables genuine temporal shift experiment (train 2018–2019, test 2022–2023, COVID gap in between)
- **Shift scenarios:** Real temporal shift, no injection needed. COVID = natural demand spike.
- **Download:** https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page
- **Format:** Parquet files per month
- **Warning:** Large. Start with 3 months train + 1 month test.

#### Dataset 4: DiDi GAIA (Optional, Strong for AAAI)
- **What:** Large-scale ride-hailing data from Chengdu and Xi'an
- **Why:** Cross-city transfer (train Chengdu, test Xi'an = spatial distribution shift)
- **Download:** https://outreach.didichuxing.com/research/opendata/
- **Note:** Requires registration. Allow 1–2 days for access approval.

---

## 3. Baseline Tree

This is the exact comparison structure. Present this as a tree in the paper.

```
ETA Model (fixed architecture: LSTM or GNN+LSTM)
│
├── No Adaptation (frozen model at test time)
│   └── Baseline-0: Static Model  ← lower bound, should perform worst under shift
│
├── First-Order Optimizers (standard, gradient-based)
│   ├── Baseline-1: SGD (test-time fine-tuning)
│   ├── Baseline-2: Adam (test-time fine-tuning)       ← PRIMARY BASELINE
│   ├── Baseline-3: AdamW (test-time fine-tuning)
│   └── Baseline-4: RMSProp (test-time fine-tuning)
│
├── Gradient-Free / ZO Optimizers
│   ├── Baseline-5: Nesterov ZO (Gaussian smoothing, always ZO)   ← closest to SAZO
│   └── Baseline-6: CMA-ES (evolution strategy)
│
├── Robustness-Aware Methods
│   ├── Baseline-7: Adversarial Training (PGD)
│   ├── Baseline-8: DRO (Group DRO, oracle shift info)
│   └── Baseline-9: MAML (meta-learned initialization)
│
└── SAZO (Ours)
    ├── SAZO-KL   (shift detector: KL divergence on input window)
    └── SAZO-MMD  (shift detector: Maximum Mean Discrepancy)
```

### Minimum Viable Baseline Set (for IEEE BigData / ICDM)
If time is short, you MUST have:
- Baseline-0 (Static)
- Baseline-2 (Adam) ← the one that matters most to reviewers
- Baseline-5 (Nesterov ZO) ← closest competitor to SAZO
- Baseline-8 (DRO) ← robustness oracle
- SAZO

That's 4 baselines + yours. Sufficient for acceptance.

---

## 4. Model Architecture (Keep Fixed — Optimizer Is the Contribution)

Use one of the following. **Do not change architecture between experiments.**

**Option A (Recommended for speed):** LSTM
- Input: last 12 time steps of speed readings
- Hidden: 64 units, 2 layers
- Output: next 3 / 6 / 12 step speed prediction
- Why: Simple, fast to train, reviewers accept it for optimizer papers

**Option B (Stronger paper):** DCRNN (Diffusion Convolutional RNN)
- Already trained on METR-LA by original authors
- Pretrained weights available in the DCRNN repo
- You can start from pretrained weights and only run adaptation
- Why: More realistic, higher impact factor

**Recommendation:** Start with LSTM to validate viability fast. Switch to DCRNN for final submission.

---

## 5. Experiment Schedule (Ward by Ward)

### Experiment 1 — The Killer Experiment (Do This First)
**Goal:** Does SAZO beat Adam under sensor dropout?

```
Setup:
- Dataset: METR-LA
- Model: LSTM (train clean, 80/20 split)
- Shift: inject 20% sensor dropout at step 500 of test
- Adaptation: 100 steps of online adaptation after shift
- Compare: Static | Adam | Nesterov ZO | SAZO-MMD

Metrics:
- MAE before shift (steps 1–499)     ← should be equal for all
- MAE after shift  (steps 500–600)   ← this is where SAZO should win
- Relative degradation (%) = (MAE_after - MAE_before) / MAE_before * 100

Decision: If SAZO degradation < Adam degradation by ≥10% → idea is viable
```

### Experiment 2 — Noise Robustness
```
Setup:
- Dataset: METR-LA
- Shift: Gaussian noise N(0, σ²) for σ ∈ {0.1, 0.5, 1.0, 2.0}
- Run all baselines across σ values
- Plot: MAE vs σ (robustness curve)
```

### Experiment 3 — Adversarial Demand Spike
```
Setup:
- Dataset: PEMS-BAY
- Shift: multiply all speed readings by 0.5 for 200 steps (simulate congestion spike)
- Compare all baselines
- Metric: worst-case MAE over spike window
```

### Experiment 4 — Temporal Shift
```
Setup:
- Dataset: NYC Taxi OR PEMS-BAY seasonal
- Train: months 1–3
- Test: months 5–6 (skip month 4 as buffer)
- No injection needed — real temporal shift
- Metric: MAE, RMSE, degradation gap
```

### Experiment 5 — Ablation
```
Remove each component of SAZO independently:
- SAZO (full)
- SAZO - shift detector (always ZO, no switching)    ← should be worse
- SAZO - adaptive radius (fixed δ)                   ← should be slightly worse
- SAZO - ZO (just shift detector + Adam resume)      ← should collapse
```

---

## 6. Metrics Table (Use This Exact Table in Paper)

| Metric | Formula | Purpose |
|---|---|---|
| MAE | mean\|y - ŷ\| | Primary accuracy |
| RMSE | √mean(y-ŷ)² | Penalizes large errors |
| Degradation (%) | (MAE_shift - MAE_clean) / MAE_clean × 100 | Main robustness metric |
| Worst-case MAE | max MAE over shift window | Adversarial robustness |
| Stability variance | Var(MAE) over shift window | Update stability |
| Runtime (ms/step) | Wall clock per adaptation step | Scalability |

**The number reviewers will look at first:** Degradation (%). You need SAZO ≤ (Adam - 10%).

---

## 7. What You Need to See to Know It's Worth Writing the Paper

| Result | Interpretation | Decision |
|---|---|---|
| SAZO degradation < Adam degradation by ≥15% on METR-LA dropout | Strong signal | ✅ Write the paper |
| SAZO degradation < Adam degradation by 10–15% | Weak signal | ⚠️ Try PEMS-BAY too before deciding |
| SAZO degradation < Adam degradation by <10% | Not enough | ❌ Pivot or fix the algorithm |
| SAZO degradation > Adam degradation | Algorithm is broken | ❌ Stop, rethink ZO update rule |
| Nesterov ZO matches SAZO within 2% | SAZO has no novelty | ❌ Need stronger shift-switching argument |

---

## 8. Theoretical Hook (For AISTATS / AAAI — Optional for BigData)

**One theorem you need:**

> **Theorem:** Under sensor corruption that induces gradient bias ε_t at step t, the expected parameter error of Adam after k steps grows as O(ε · k), while SAZO's ZO update error is bounded by O(δ² + σ²/k) where δ is the smoothing radius and σ² is noise variance. For shift durations k < ε·something, SAZO dominates.

You do not need to prove this rigorously for BigData. For AISTATS, you do.

---

## 9. Paper Structure (If Viable)

```
1. Introduction          (problem + gap + claim, Figure 1 = Adam collapse)
2. Related Work          (ZO optimization, distribution shift, traffic ETA)
3. Problem Formulation   (formal setup, shift model, objective)
4. SAZO Algorithm        (pseudocode + shift detector + ZO update)
5. Theoretical Analysis  (optional: bias-variance bound)
6. Experiments           (5 experiments above)
7. Ablation Study        (Experiment 5)
8. Conclusion
```

---

## 10. Timeline Estimate

| Week | Task |
|---|---|
| Week 1 | Download METR-LA + PEMS-BAY, train baseline LSTM, reproduce clean accuracy |
| Week 2 | Implement sensor dropout injection, run Adam + SPSA baselines under shift |
| Week 3 | Implement SAZO (shift detector + ZO switch), run Experiment 1 |
| Week 4 | **Decision point.** If viable: run remaining experiments. If not: pivot. |
| Week 5–6 | Full experiment suite (Experiments 2–5), ablation |
| Week 7–8 | Write paper |

---

## 11. One-Line Pitch (For Abstract Opening)

> *"We show that first-order optimizers actively worsen ETA predictions during sensor shift due to biased gradient estimation, and propose SAZO — a lightweight mode-switching wrapper that replaces gradient updates with zeroth-order perturbations exactly when the gradient cannot be trusted, achieving X% lower degradation than Adam with zero overhead at stationarity."*

---

*Last updated: March 2026 — Pre-experiment viability check version*

