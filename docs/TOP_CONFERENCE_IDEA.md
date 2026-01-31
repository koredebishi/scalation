# Adjacency Matters: A Controlled Robustness Study of STGNN Graph Construction Under Distribution Shift

**Target:** KDD 2027 (Primary) / AAAI 2027 (Stretch) / IEEE BigData 2027 (Backup)
**Deadline:** February 2027 (KDD), August 2026 (AAAI), September 2026 (BigData)
**Status:** IDEA PHASE

---

## 1. Problem Statement

Learned-adjacency STGNNs (Graph WaveNet, AGCRN, MTGNN) achieve strong performance on traffic benchmarks. However, they learn graph structure from **training data distributions** and may fail under:

1. **Sensor dropout/failure** - nodes disappear at test time
2. **Incident conditions** - non-recurrent congestion patterns
3. **Temporal distribution shift** - holiday vs. weekday, COVID vs. normal
4. **Spatial transfer** - model trained on region A, tested on region B

**Research Question:** Do learned-adjacency methods degrade more than non-learned dynamic graphs under distribution shift? If so, when and why?

**Our Hypothesis:** Correlation-based dynamic adjacency (recomputed from recent observations) is more robust to distribution shift than learned adjacency, because it adapts to the *current* data rather than memorizing training patterns.

**Note on Prior Work:** Dynamic adjacency from data-dependent similarity is not new (DGCRN, dynamic graph convolution methods exist). Our contribution is an **empirical robustness study** comparing learned vs. non-learned graphs under systematic distribution shift, not a claim of architectural novelty.

**Key Threat to Address:** DGCRN produces time-varying graphs via a learned RNN. If DGCRN is also robust under shift, the "learned vs. non-learned" dichotomy breaks down. We explicitly include DGCRN as a **dynamic learned** baseline to test whether learning temporal graph dynamics provides similar robustness to non-learned recomputation. This is a key comparison.

---

## 2. Method: Correlation-Based Dynamic Adjacency (CDA)

We use a **standard** correlation-based dynamic adjacency operator. This is not novel - similar constructions appear in DGCRN, dynamic graph convolution, and correlation-based traffic methods. We use it as a **probe** to study robustness.

### The CDA Operator

**Input:**
- X ∈ ℝ^(n × d × W): Window of W timesteps, n sensors, d features (speed, flow, occupancy)
- M ∈ {0,1}^(n × n): Topology mask (physical connectivity constraint)

**Output:**
- A_t ∈ ℝ^(n × n): Dynamic adjacency matrix at time t

**Algorithm:**
```
CDA(X, M, τ, ε):
────────────────────────────────────────
1. Flatten each sensor: X̃_i = flatten(X[i, :, :]) ∈ ℝ^(d·W)
2. Compute similarity: S[i,j] = cosine(X̃_i, X̃_j)  ∀i,j
3. Apply topology mask: S = S ⊙ M
4. Soft sparsification: A_t = sparsemax(S / τ) + εI
5. Return A_t
────────────────────────────────────────
```

**Note:** We use **sparsemax** (Martins & Astudillo, 2016) instead of top-k to ensure Lipschitz continuity. Top-k has discontinuities that break stability guarantees.

**Hyperparameters:**
- W: window size (we test sensitivity)
- τ: temperature (we test sensitivity)  
- ε: self-loop weight
- Similarity function: cosine (we also test Pearson, DTW)

These are hyperparameters, not learned parameters. We do **not** claim "parameter-free" as a novelty - we claim **non-learned graph structure**.

---

## 3. Model Architecture: CDA-GAT

```
┌─────────────────────────────────────────────────────────────────┐
│                     CDA-GAT Architecture                        │
│              (Standard STGNN with CDA adjacency)                │
└─────────────────────────────────────────────────────────────────┘

Input: X ∈ ℝ^(B × n × T × d)   [batch, nodes, time, features]
       M ∈ {0,1}^(n × n)       [topology mask]

                    ┌─────────────────┐
                    │  Input Window   │
                    │  X[t-W:t]       │
                    └────────┬────────┘
                             │
              ┌──────────────┴──────────────┐
              │                             │
              ▼                             ▼
    ┌─────────────────┐           ┌─────────────────┐
    │  CDA Operator   │           │  Temporal Enc   │
    │  A_t = CDA(X,M) │           │  GRU / TCN      │
    │  (non-learned)  │           │                 │
    └────────┬────────┘           └────────┬────────┘
             │                             │
             │         A_t                 │  H ∈ ℝ^(n × h)
             │                             │
             └──────────────┬──────────────┘
                            │
                            ▼
              ┌─────────────────────────────┐
              │     Graph Attention Layer   │
              │     GAT(H, A_t)             │
              │     - Multi-head attention  │
              │     - A_t masks attention   │
              └─────────────┬───────────────┘
                            │
                            ▼
              ┌─────────────────────────────┐
              │     Prediction Head         │
              │     MLP: h → horizon × d    │
              └─────────────┬───────────────┘
                            │
                            ▼
              ┌─────────────────────────────┐
              │  Output: Ŷ ∈ ℝ^(n × H × d)  │
              │  [nodes, horizon, features] │
              └─────────────────────────────┘
```

**Baselines we compare against:**
- Static distance adjacency: STGCN, DCRNN
- Learned adjacency: Graph WaveNet, AGCRN, MTGNN
- Dynamic learned: DGCRN

All use the **same GRU+GAT backbone** - only the adjacency construction differs.

---

## 4. Contribution: Empirical Robustness Study (THE REAL NOVELTY)

**We do NOT claim architectural novelty.** Correlation-based dynamic adjacency exists.

**Our Contribution:** A systematic empirical study answering:

> **"Under what distribution shifts do learned-adjacency STGNNs fail, and does non-learned dynamic adjacency provide robustness?"**

### 4.1 Distribution Shift Scenarios We Test

| Scenario | Description | How We Simulate |
|----------|-------------|-----------------|
| **Sensor Dropout** | Random nodes fail at test time | Mask 10%, 20%, 30% of nodes |
| **Incident Days** | Non-recurrent congestion | Test only on PeMS incident-flagged days |
| **Temporal Shift** | Holiday/special event | Train on weekdays, test on weekends/holidays |
| **Spatial Transfer** | New region, same model | Train on METR-LA, test on PEMS-BAY (CDA vs Static only) |
| **Missing Data** | Partial observations | Random 20% feature masking |
| **Concept Drift** | Gradual pattern change | Train Jan-Feb 2020, test April 2020 (COVID lockdown) - if data available |

### 4.2 What We Measure

For each (model, shift scenario):
- **Accuracy degradation:** MAE_shift / MAE_normal
- **Variance increase:** Std(error) under shift
- **Failure modes:** When does learned adjacency catastrophically fail?

### 4.3 Why This is Publishable

| Aspect | Value |
|--------|-------|
| **Practical impact** | Tells practitioners which methods to use in real deployments |
| **Novel empirical scope** | No existing paper systematically tests 5 shift types × 5+ models |
| **Actionable insight** | "Use CDA under X conditions, use learned under Y conditions" |
| **Reproducible** | Standard datasets, clear protocol |

### Contribution Statement (for paper):
> "We study robustness of traffic forecasting STGNNs through the lens of graph construction. Using a controlled backbone and unified shift protocols, we quantify how static, learned, dynamic learned, and non-learned dynamic graphs degrade under sensor failures, incidents, and temporal regime shifts. Our analysis isolates adjacency as a key factor in robustness and yields practical guidance for deployment. **Regardless of which strategy wins, our characterization enables informed model selection.**"

---

## 5. Theoretical Analysis (Minimal, Defensible Claims Only)

We do NOT claim novel theory. We provide only **basic properties** of the CDA operator to justify its use in robustness experiments.

### 5.1 Continuity (with Sparsemax)

**Claim:** The CDA operator with sparsemax is **Lipschitz continuous** in the input X.

**Justification:** 
- Cosine similarity is Lipschitz (standard result)
- Sparsemax is Lipschitz with constant 1 (Martins & Astudillo, 2016)
- Composition of Lipschitz functions is Lipschitz

**Note:** This does NOT hold for top-k sparsification, which has discontinuities.

### 5.2 Adaptivity

**Claim:** CDA adjacency changes when input distribution changes.

**Why this matters for robustness:** 
- Learned adjacency is fixed after training → cannot adapt to new patterns
- CDA recomputes from current window → automatically adapts

This is not a theorem, just a design property.

### 5.3 Complexity

| Component | Time | Space |
|-----------|------|-------|
| CDA Operator | O(n² · d · W) | O(n²) |
| GAT Layer | O(n · k · h) | O(n · h) |

**Note:** Same asymptotic cost as computing distance-based adjacency.

---

## 6. Experimental Plan (Robustness-Focused)

### 6.1 Datasets

| Dataset | Nodes | Edges | Time | Interval | Use |
|---------|-------|-------|------|----------|-----|
| METR-LA | 207 | 1,515 | 4 months | 5 min | Primary |
| PEMS-BAY | 325 | 2,369 | 6 months | 5 min | Transfer target |
| PeMSD4 | 307 | 340 | 2 months | 5 min | Incident study |
| PeMSD8 | 170 | 295 | 2 months | 5 min | Secondary |

### 6.2 Models Compared (All Same Backbone)

| Model | Adjacency Type | Description |
|-------|----------------|-------------|
| Static-GAT | Distance-based, fixed | Baseline |
| GWN-GAT | Learned adaptive (Graph WaveNet style) | Learned |
| AGCRN-GAT | Node embedding product | Learned |
| DGCRN-GAT | Dynamic learned (RNN on graph) | Dynamic learned |
| **CDA-GAT** | Correlation-based, non-learned | **Ours** |

**Key:** Same GRU+GAT backbone, only adjacency differs → fair comparison.

### 6.3 Experiment 1: Baseline Accuracy (In-Distribution)

Standard train/val/test split. Confirm all models achieve similar accuracy when no shift.

**Expected:** Learned methods slightly better (they can specialize).

### 6.4 Experiment 2: Sensor Dropout

**Protocol:**
1. Train all models on full graph
2. At test time, randomly drop 10%, 20%, 30% of nodes
3. For dropped nodes: features = 0, adjacency = masked out
4. Measure MAE degradation

**Hypothesis:** Learned adjacency degrades more (it memorized full topology).

### 6.5 Experiment 3: Incident Days

**Protocol:**
1. Use PeMS incident flags to identify abnormal days
2. Train on normal days only
3. Test on incident days

**Hypothesis:** CDA adapts to abnormal patterns; learned methods fail.

### 6.6 Experiment 4: Temporal Shift

**Protocol:**
1. Train on weekdays (Mon-Thu)
2. Test on weekends + holidays

**Hypothesis:** CDA handles different temporal patterns better.

### 6.7 Experiment 5: Spatial Transfer (Generalization, NOT "Transfer Learning")

**Protocol:**
1. Train on METR-LA
2. Test on PEMS-BAY (different city, different topology)
3. **Only compare CDA-GAT vs. Static-GAT** (both can transfer)
4. Learned adjacency methods (GWN, AGCRN, DGCRN) are **excluded** - they cannot transfer by design (node embeddings are METR-LA specific)

**Note:** This is NOT a fair comparison against learned methods. We frame this as: "Among transferable graph strategies, which generalizes better?" This avoids the strawman critique.

**Hypothesis:** CDA outperforms static distance because it adapts to PEMS-BAY's traffic patterns.

### 6.8 Experiment 6: Missing Data

**Protocol:**
1. At test time, randomly mask 20% of features
2. Impute with last-observed value
3. Measure degradation

**Hypothesis:** CDA more robust to noisy/missing inputs.

### 6.9 Experiment 7: Concept Drift (COVID, Optional)

**Protocol:**
1. Use PeMS data spanning 2020 (if available)
2. Train on Jan-Feb 2020 (pre-COVID normal traffic)
3. Test on April 2020 (lockdown - dramatically different patterns)
4. This tests gradual/sudden concept drift

**Hypothesis:** CDA adapts to new traffic regime; learned adjacency memorized pre-COVID patterns.

**Note:** This experiment is optional and depends on data availability. If PeMS 2020 data is not accessible, we skip this scenario.

### 6.10 Metrics

For each experiment:
- **MAE, RMSE, MAPE** (standard)
- **Degradation ratio:** MAE_shift / MAE_normal
- **Variance of error** under shift
- **Worst-case error** (95th percentile)

---

## 7. Expected Results

### 7.1 In-Distribution (Baseline)

| Model | METR-LA MAE | PEMS-BAY MAE |
|-------|-------------|--------------|
| Static-GAT | 2.75 | 1.68 |
| GWN-GAT | **2.68** | **1.62** |
| AGCRN-GAT | 2.70 | 1.64 |
| DGCRN-GAT | 2.69 | 1.63 |
| CDA-GAT | 2.72 | 1.65 |

**Expected:** Learned methods slightly better in-distribution. This is fine.

### 7.2 Robustness Results (The Main Finding)

**Degradation Ratio = MAE_shift / MAE_normal** (lower is better)

| Model | Sensor Dropout 20% | Incident Days | Temporal Shift | Spatial Transfer |
|-------|-------------------|---------------|----------------|------------------|
| Static-GAT | 1.15 | 1.12 | 1.08 | 1.25 |
| GWN-GAT | **1.35** | **1.28** | **1.18** | N/A |
| AGCRN-GAT | 1.32 | 1.25 | 1.15 | N/A |
| DGCRN-GAT | 1.28 | 1.22 | 1.12 | N/A |
| **CDA-GAT** | **1.12** | **1.10** | **1.06** | **1.18** |

**Hypothesis:** CDA degrades 10-20% less than learned methods under shift.


| Deployment Scenario | Recommended Method | Why |
|--------------------|-------------------|-----|
| Stable, full sensors | GWN/AGCRN | Best accuracy |
| Sensor failures possible | CDA | Robust to dropout |
| Incident-prone corridor | CDA | Adapts to non-recurrent |
| Multi-region deployment | CDA | Transfers without retraining |
| Single stable region | Learned | Can specialize |

**This table IS the contribution** - actionable guidance.

### 7.4 Concrete Decision Rules (Target Output)

We aim to produce quantitative thresholds, not just qualitative recommendations:

- **Sensor dropout:** "If expected sensor failure rate > X%, prefer CDA over learned adjacency"
- **Incident frequency:** "For corridors with > Y% non-recurrent congestion days, CDA degrades Z% less"
- **Accuracy-robustness tradeoff:** "Learned methods gain A% accuracy in-distribution but lose B% under shift"

If we cannot produce precise thresholds, we downgrade to: "We characterize trade-offs; practitioners can apply findings to their specific contexts."

---

## 8. Implementation Plan

### 8.1 Phase 1: Baseline Setup (2 weeks)
- [ ] Set up METR-LA, PEMS-BAY data pipelines
- [ ] Implement shared GRU+GAT backbone
- [ ] Implement 5 adjacency variants (Static, GWN, AGCRN, DGCRN, CDA)
- [ ] Verify in-distribution accuracy matches literature

### 8.2 Phase 2: Robustness Experiments (4 weeks)
- [ ] Implement sensor dropout protocol
- [ ] Identify and extract incident days from PeMS
- [ ] Implement temporal shift (weekday→weekend) split
- [ ] Implement spatial transfer (METR-LA→PEMS-BAY)
- [ ] Implement missing data protocol
- [ ] Run all 5 models × 5 shift scenarios = 25 experiments

### 8.3 Phase 3: Analysis (2 weeks)
- [ ] Compute degradation ratios
- [ ] Statistical significance tests
- [ ] Visualize adjacency matrices under shift (CDA vs learned)
- [ ] Identify failure cases

### 8.4 Phase 4: Paper Writing (2 weeks)
- [ ] Introduction: motivation from real-world deployment issues
- [ ] Related work: position against robustness literature
- [ ] Method: describe CDA (honestly, not as novel)
- [ ] Experiments: present robustness findings
- [ ] Conclusion: practitioner recommendations

**Total: 10 weeks → AAAI 2027 deadline (Aug 2026) feasible**

---

## 9. Risk Assessment

| Risk | Likelihood | Mitigation |
|------|------------|------------|
| CDA doesn't show robustness advantage | Medium | Still publishable as "learned methods ARE robust" (negative result) |
| Learned methods also robust | Medium | Identify WHICH shifts matter; nuanced findings still valuable |
| Reviewers say "just an empirical study" | High | Emphasize practical impact, practitioner guidance |
| Not enough novelty for AAAI | Medium | Target KDD (more empirical), or IEEE TITS (journal) |
| Results dataset-specific | Medium | Test on 4 datasets to show generality |

### Fallback Options:

1. **If CDA wins:** "Non-learned dynamic adjacency enables robust STGNN deployment"
2. **If learned wins:** "Learned adjacency methods are robust to distribution shift - a reassessment"
3. **If mixed:** "When to use learned vs. non-learned graphs: an empirical guide" (still valuable)

---

## 10. Paper Outline

### Title Options:
1. "Adjacency Matters: A Controlled Robustness Study of STGNN Graph Construction Under Shift"
2. "How Do Learned and Non-Learned Graphs Behave Under Distribution Shift in Traffic Forecasting?"
3. "Robustness of Graph Construction Choices in Traffic Forecasting Under Distribution Shift"

### Abstract (Draft):
> Spatiotemporal graph neural networks (STGNNs) with learned adjacency matrices achieve state-of-the-art traffic forecasting accuracy on standard benchmarks. However, real-world deployments face distribution shifts: sensor failures, traffic incidents, temporal variations, and spatial transfer to new regions. We present a systematic robustness study comparing learned-adjacency STGNNs (Graph WaveNet, AGCRN, MTGNN) against correlation-based dynamic adjacency (CDA) across five realistic shift scenarios. Our experiments on four benchmark datasets reveal that [KEY FINDING: e.g., "learned adjacency degrades 15-25% more under sensor dropout and incidents, while CDA maintains stable performance"]. We provide actionable recommendations for practitioners selecting STGNN architectures for robust real-world deployment.

### Sections:
1. **Introduction** (1 page)
   - Motivation: gap between benchmark and deployment
   - Research question: robustness of learned vs. non-learned graphs
   
2. **Related Work** (1 page)
   - Spatiotemporal GNNs for traffic
   - Adaptive/learned graph methods
   - Robustness studies in other domains
   - (Acknowledge dynamic adjacency exists - not our novelty)
   
3. **Method** (1.5 pages)
   - Problem formulation
   - CDA operator (presented as baseline, not novelty)
   - Unified backbone for fair comparison
   - Distribution shift scenarios
   
4. **Experiments** (3 pages)
   - In-distribution results (Table 1)
   - Sensor dropout (Table 2, Figure 1)
   - Incident days (Table 3)
   - Temporal shift (Table 4)
   - Spatial transfer (Table 5)
   - Summary and practitioner recommendations
   
5. **Analysis** (0.5 pages)
   - Why does CDA help under shift?
   - Visualization of adjacency under normal vs. incident
   
6. **Conclusion** (0.5 pages)
   - Summary of findings
   - Limitations
   - Future work: hybrid approaches

---

## 11. Honest Reviewer Assessment

### Strengths (Why This Could Get Accepted):
1. **Practical relevance** - Addresses real deployment concerns, not just benchmark chasing
2. **Novel experimental scope** - First systematic robustness study across 5 shift types
3. **Actionable findings** - Practitioner recommendations table
4. **Fair comparison** - Same backbone, only adjacency differs
5. **No overclaims** - We don't claim CDA is novel, we claim the study is novel

### Weaknesses (What Reviewers Will Attack):
1. **"Just an empirical study"** - No new algorithm
   - *Rebuttal:* Empirical studies are valuable; similar papers appear at KDD, AAAI
   
2. **"Expected result"** - Of course non-learned adapts better
   - *Rebuttal:* We quantify HOW MUCH better and under WHICH conditions
   
3. **"Limited technical depth"** 
   - *Rebuttal:* Depth is in experimental design, not theory
   
4. **"Why not test more methods?"**
   - *Rebuttal:* We test representative methods from each category; exhaustive is not possible

### Venue Fit:

| Venue | Fit | Notes |
|-------|-----|-------|
| **KDD 2027** | ⭐⭐⭐⭐⭐ | Best fit - empirical, applied, practical |
| **AAAI 2027** | ⭐⭐⭐⭐ | Good - has empirical track |
| **IJCAI 2027** | ⭐⭐⭐ | Okay - prefers more theory |
| **NeurIPS 2027** | ⭐⭐ | Hard - needs stronger novelty |
| **IEEE TITS** (Journal) | ⭐⭐⭐⭐⭐ | Backup - definitely publishable |

**Recommendation:** Target **KDD 2027** (deadline ~Feb 2027) as primary, **AAAI 2027** (deadline ~Aug 2026) as stretch.

---

## 12. Code Skeleton

```python
import torch
import torch.nn as nn
import torch.nn.functional as F
from entmax import sparsemax  # pip install entmax

class CDAOperator(nn.Module):
    """Correlation-based Dynamic Adjacency operator.
    
    NOT claimed as novel - this is a standard construction.
    Used as a probe for robustness experiments.
    """
    
    def __init__(self, tau: float = 0.5, eps: float = 0.1):
        super().__init__()
        self.tau = tau
        self.eps = eps
    
    def forward(self, X: torch.Tensor, M: torch.Tensor) -> torch.Tensor:
        """
        Args:
            X: (batch, n, d, W) - sensor window
            M: (n, n) - topology mask
        Returns:
            A: (batch, n, n) - dynamic adjacency
        """
        B, n, d, W = X.shape
        
        # 1. Flatten temporal dimension
        X_flat = X.reshape(B, n, d * W)  # (B, n, d*W)
        
        # 2. Normalize for cosine similarity
        X_norm = F.normalize(X_flat, dim=-1)  # (B, n, d*W)
        
        # 3. Compute similarity matrix
        S = torch.bmm(X_norm, X_norm.transpose(1, 2))  # (B, n, n)
        
        # 4. Apply topology mask
        S = S * M.unsqueeze(0)  # (B, n, n)
        
        # 5. Sparsemax (continuous, Lipschitz - unlike top-k)
        S_scaled = S / self.tau
        A = sparsemax(S_scaled, dim=-1)  # (B, n, n)
        
        # 6. Add self-loop and renormalize
        A = A + self.eps * torch.eye(n, device=A.device).unsqueeze(0)
        A = A / A.sum(dim=-1, keepdim=True)
        
        return A


class CDAGAT(nn.Module):
    """CDA-GAT model for robustness experiments."""
    
    def __init__(self, in_dim: int, hid_dim: int, out_dim: int, 
                 horizon: int, n_heads: int = 4, tau: float = 0.5):
        super().__init__()
        
        # CDA operator (non-learned)
        self.cda = CDAOperator(tau=tau)
        
        # Temporal encoder (shared across all adjacency variants)
        self.temporal = nn.GRU(in_dim, hid_dim, batch_first=True)
        
        # Graph attention layer
        self.gat = GATLayer(hid_dim, hid_dim, n_heads)
        
        # Prediction head
        self.pred_head = nn.Linear(hid_dim, horizon * out_dim)
        self.horizon = horizon
        self.out_dim = out_dim
    
    def forward(self, X: torch.Tensor, M: torch.Tensor) -> torch.Tensor:
        """
        Args:
            X: (batch, n, T, d) - historical observations
            M: (n, n) - topology mask
        Returns:
            Y: (batch, n, horizon, d) - predictions
        """
        B, n, T, d = X.shape
        
        # Compute dynamic adjacency
        X_window = X.permute(0, 1, 3, 2)  # (B, n, d, T)
        A = self.cda(X_window, M)  # (B, n, n)
        
        # Temporal encoding per node
        X_flat = X.reshape(B * n, T, d)
        H, _ = self.temporal(X_flat)
        H = H[:, -1, :].reshape(B, n, -1)  # (B, n, hid)
        
        # Graph attention
        H = self.gat(H, A)  # (B, n, hid)
        
        # Predict
        out = self.pred_head(H)
        return out.reshape(B, n, self.horizon, self.out_dim)


# For fair comparison, implement other adjacency variants:

class StaticAdjacency(nn.Module):
    """Distance-based static adjacency (baseline)."""
    def __init__(self, A_static: torch.Tensor):
        super().__init__()
        self.register_buffer('A', A_static)
    
    def forward(self, X: torch.Tensor, M: torch.Tensor) -> torch.Tensor:
        B = X.shape[0]
        return self.A.unsqueeze(0).expand(B, -1, -1)


class LearnedAdjacency(nn.Module):
    """Graph WaveNet-style learned adjacency."""
    def __init__(self, n_nodes: int, emb_dim: int = 16):
        super().__init__()
        self.E1 = nn.Parameter(torch.randn(n_nodes, emb_dim))
        self.E2 = nn.Parameter(torch.randn(n_nodes, emb_dim))
    
    def forward(self, X: torch.Tensor, M: torch.Tensor) -> torch.Tensor:
        A = torch.softmax(torch.relu(self.E1 @ self.E2.T), dim=-1)
        return A.unsqueeze(0).expand(X.shape[0], -1, -1)


class GATLayer(nn.Module):
    """Standard GAT layer."""
    
    def __init__(self, in_dim: int, out_dim: int, n_heads: int = 4):
        super().__init__()
        self.n_heads = n_heads
        self.head_dim = out_dim // n_heads
        
        self.W = nn.Linear(in_dim, out_dim)
        self.a = nn.Linear(2 * self.head_dim, 1)
        self.out_proj = nn.Linear(out_dim, out_dim)
    
    def forward(self, H: torch.Tensor, A: torch.Tensor) -> torch.Tensor:
        B, n, _ = H.shape
        
        H_proj = self.W(H).reshape(B, n, self.n_heads, self.head_dim)
        
        H_i = H_proj.unsqueeze(2).expand(-1, -1, n, -1, -1)
        H_j = H_proj.unsqueeze(1).expand(-1, n, -1, -1, -1)
        
        attn = self.a(torch.cat([H_i, H_j], dim=-1)).squeeze(-1)
        attn = attn.permute(0, 3, 1, 2)  # (B, heads, n, n)
        
        # Mask with adjacency
        attn = attn.masked_fill(A.unsqueeze(1) == 0, float('-inf'))
        attn = F.softmax(attn, dim=-1)
        attn = torch.nan_to_num(attn, 0.0)
        
        H_heads = H_proj.permute(0, 2, 1, 3)
        out = torch.matmul(attn, H_heads)
        out = out.permute(0, 2, 1, 3).reshape(B, n, -1)
        
        return F.relu(self.out_proj(out)) + self.W(H)  # residual
```

---

## 13. Next Steps

1. **Your decision:** Is this robustness study angle acceptable?
2. **Data setup:** Download METR-LA, PEMS-BAY, identify incident days
3. **Implement baseline models** with shared backbone
4. **Run in-distribution experiments** first (sanity check)
5. **Run robustness experiments** (5 shift types × 5 models)
6. **Analyze and write**

---

## 14. Summary of Changes from Original DYNA-GAT

| Original Claim | Revised Claim |
|----------------|---------------|
| "First parameter-free dynamic adjacency" | CDA is not novel; robustness study is novel |
| Theorem 1 (Lipschitz with top-k) | Use sparsemax; claim only continuity, not tight bound |
| Theorem 2 (universal approximation) | **Removed** - indefensible |
| "Beats baselines on accuracy" | May be slightly worse in-distribution; focus on robustness |
| Architectural novelty | Empirical study novelty |

**Key shift:** From "we invented something new" to "we measured something important that nobody has systematically studied."

---

**Created:** January 18, 2026  
**Revised:** January 18, 2026 (Post-Critique Revision)  
**Author:** [Your Name]  
**Advisor:** [Advisor Name]

