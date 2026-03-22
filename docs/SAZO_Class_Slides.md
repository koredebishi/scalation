# SAZO — Class Presentation
**Format:** 7-minute talk · 4 slides · Casual update tone
**Framing:** "Here's what I'm working on and why it matters"

---

## Slide 1 — The Problem (1 min)

**Title:** *"Test-Time Adaptation Has a Blind Spot"*

**On the slide:**
- Traffic ETA models are often fine-tuned during deployment — sensors fail, traffic spikes, patterns shift
- First-order adaptation (SGD, Adam, AdamW) all share the same assumption: **the gradient points in the right direction**
- Under distribution shift, that assumption breaks — the gradient is biased, and first-order updates actively worsen the model
- Simple sketch: MAE flat before step 500 → sensor dropout injected → MAE spikes upward ❌

**What to say:**
> "ETA models are often fine-tuned at test time. The standard choice is any first-order optimizer — SGD, Adam, AdamW. But they all share the same blind spot: they apply gradient updates without checking if those gradients are trustworthy. When something goes wrong — a sensor dies, a road closes — the gradients become biased. The optimizer doesn't know this. It keeps updating, and the updates make things worse."

---

## Slide 2 — The Idea: SAZO (2 min)

**Title:** *"SAZO: Don't Update When You Can't Trust the Gradient"*

**On the slide:**
- Full name: **Shift-Aware Zeroth-Order Adaptation**
- Two-mode diagram:
  ```
  Monitor input window with MMD (sliding window)
       │
       ├── Stationary? → Run first-order update normally   (zero overhead)
       │
       └── Shift detected? → Run ZO update                (no backprop, 2 forward passes)
                              DON'T update momentum buffers
  ```
- ZO update (Nesterov-Spokoiny Gaussian smoothing):
  `g_ZO = (d / 2δ) · [L(θ + δu) − L(θ − δu)] · u`   where `u ~ N(0, I)`

- Key point: **"You're not replacing the optimizer. You're protecting it."**

**What to say:**
> "SAZO is a wrapper around any first-order optimizer. It monitors a sliding window of input features using MMD — a kernel-based test that measures how different two batches of data look. When stationary, the first-order optimizer runs normally with zero overhead. When shift is detected, it switches to a zeroth-order update: just two forward passes with a random perturbation, no backprop. And critically — it does not update the momentum buffers during shift, so when stationarity returns, the optimizer picks back up from a clean state."

---

## Slide 3 — Experiment Plan (2.5 min)

**Title:** *"One Killer Experiment First"*

**On the slide:**
- The killer experiment:
  ```
  Dataset:  METR-LA (207 sensors, LA highways)
  Model:    DCRNN (pretrained on clean data, Li et al. ICLR 2018)
  Shift:    20% sensor dropout injected at step 500
  Compare:  Static | First-order (SGD/Adam/AdamW) | Nesterov ZO | SAZO
  Metric:   Degradation % = (MAE_after − MAE_before) / MAE_before × 100
  Decision: SAZO < first-order by ≥10% → proceed to paper
  ```

- Baseline comparison:

  | Method | Gradient-free? | Shift-aware? |
  |---|---|---|
  | Static (frozen) | — | — |
  | SGD / Adam / AdamW | ❌ | ❌ |
  | Nesterov ZO (always ZO) | ✅ | ❌ |
  | **SAZO (ours)** | ✅ when shifted | ✅ |

**What to say:**
> "Before writing anything, I need to know the idea works. The first experiment is simple: use DCRNN pretrained on METR-LA, inject 20% sensor dropout at step 500, and compare SAZO against first-order optimizers and always-on Nesterov ZO. Nesterov ZO is the honest competitor — it's always zeroth-order but has no shift awareness. If SAZO can't beat always-on Nesterov ZO, there's no novelty. If it beats first-order optimizers by at least 10% degradation reduction, the idea is viable."

---

## Slide 4 — Status & Next Steps (1 min)

**Title:** *"Current Status"*

**On the slide:**
- ✅ Algorithm designed (SAZO pseudocode + shift detector)
- ✅ Datasets identified: METR-LA + PEMS-BAY
- 🔄 **Now:** Setting up DCRNN pretrained weights + dropout injection
- ⬜ **Next:** Run Experiment 1 (the killer experiment)
- ⬜ **Decision point (Week 4):** If viable → write paper (target: IEEE BigData 2026)

**What to say:**
> "I have the algorithm. I know what data to use. Right now I'm setting up DCRNN with the pretrained weights. The next concrete milestone is Experiment 1 within the next two weeks. That result tells me whether this is worth writing up."

---

## Rehearsal Notes

- **This is a class update — not a defense.** Speak casually.
- **Key sentence to nail on Slide 1:** "They all share the same blind spot — they apply gradient updates without checking if those gradients are trustworthy."
- **Key sentence to nail on Slide 2:** "You're not replacing the optimizer. You're protecting it."
- **If asked why not just always use Nesterov ZO:** "Always-on Nesterov ZO pays gradient-free overhead on every step. SAZO is zero overhead at stationarity — you only pay the cost when you need it."
- **If asked what MMD is:** "A kernel-based test that measures how different two batches of data look — no distribution assumption needed."

