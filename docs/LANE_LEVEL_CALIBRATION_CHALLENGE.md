# Lane-Level Calibration: Analysis & Solution

## Network Topology

### Real Physical Network (PeMS Detectors)

```latex
\begin{figure}[H]
\centering
\begin{tikzpicture}[scale=0.9, >=Latex]
% ---- Road boundaries (ONLY these are solid) ----
\draw[thick,->] (0,0) -- (14,0);     % bottom boundary
\draw[thick,->] (0,2.0) -- (14,2.0); % top boundary

% ---- Lane separators (dashed) ----
\foreach \y in {0.5,1.0,1.5} {
    \draw[dashed] (0,\y) -- (14,\y);
}

% ---- Lane labels ----
\node[left] at (0,1.75) {\scriptsize L1};
\node[left] at (0,1.25) {\scriptsize L2};
\node[left] at (0,0.75) {\scriptsize L3};
\node[left] at (0,0.25) {\scriptsize L4};

% ---- Rectangular boxes at each sensor (one per lane) ----
\foreach \x in {1,3.5,6,9.5,12} {
    \foreach \y in {1.75,1.25,0.75,0.25} {
        \draw[fill=white] (\x-0.1,\y-0.05) rectangle (\x+0.1,\y+0.05);
    }
}

% ---- Sensors ----
\foreach \x/\lab in {1/S1,3.5/S2,6/S3,9.5/S4,12/S5} {
    \draw[blue, thick] (\x,-0.2) -- (\x,2.2);
    \node[above] at (\x,2.3) {\scriptsize \lab};
}

% ---- On-ramp 1 (two boundaries + dashed center) ----
\draw[thick] (1.6,-1.2) -- (2.4,-0.4);
\draw[thick] (1.9,-1.2) -- (2.7,-0.4);
\draw[dashed] (1.75,-1.2) -- (2.55,-0.4);
\draw[thick] (2.4,-0.4) -- (2.9,0.0);
\draw[thick] (2.7,-0.4) -- (3.6,0.0);
\node[below] at (1.75,-1.2) {\scriptsize Ramp 1};

% ---- On-ramp 2 (two boundaries + dashed center) ----
\draw[thick] (7.2,-1.2) -- (8.0,-0.4);
\draw[thick] (7.5,-1.2) -- (8.3,-0.4);
\draw[dashed] (7.35,-1.2) -- (8.15,-0.4);
\draw[thick] (8.0,-0.4) -- (8.5,0.0);
\draw[thick] (8.3,-0.4) -- (9.2,0.0);
\node[below] at (7.35,-1.2) {\scriptsize Ramp 2};

% ---- Distance annotations ----
\draw[<->] (1,3.0) -- (3.5,3.0);
\node[above] at (2.25,3.0) {\scriptsize 280 m};

\draw[<->] (3.5,3.0) -- (6,3.0);
\node[above] at (4.75,3.0) {\scriptsize 310 m};

\draw[<->] (6,3.0) -- (9.5,3.0);
\node[above] at (7.75,3.0) {\scriptsize 470 m};

\draw[<->] (9.5,3.0) -- (12,3.0);
\node[above] at (10.75,3.0) {\scriptsize 350 m};

% ---- Direction arrow ----
\draw[very thick,->] (14.2,1.0) -- (15.2,1.0);
\node[right] at (15.2,1.0) {\scriptsize N};
\end{tikzpicture}
\end{figure}
```

**Key observations from network:**
- 4 lanes (L1 = leftmost/fast, L4 = rightmost/slow)
- 5 PeMS sensors (S1–S5)
- Ramp 1 merges into L4 between S1 and S2
- Ramp 2 merges into L4 between S3 and S4
- Total corridor length: 280 + 310 + 470 + 350 = **1,410 m**

### Simulation Network (With Synthetic Merge Points)

```
[warm_up]──[sensor1]──[onR_merge1]──[sensor2]──[sensor3]──[onR_merge2]──[sensor4]──[sensor5]
                           ↑                                    ↑
                      (synthetic)                          (synthetic)
```

Synthetic merge points exist for:
1. Capturing ramp vehicle counts via `jump()`
2. Enabling lane changes at junction heads (deque structure constraint)
3. Proper insertion of ramp vehicles into mainline lane lists

---

## Segment Classification

| Segment | Ramp Effect | Use for LC Estimation? |
|---------|-------------|------------------------|
| S1 → S2 | +Ramp1 | ❌ Confounded |
| S2 → S3 | None | ✅ Pure LC signal |
| S3 → S4 | +Ramp2 | ❌ Confounded |
| S4 → S5 | None | ✅ Pure LC signal |

---

## Data Analysis (6:00 AM Sample)

### Mainline Flows

| Sensor | L1 | L2 | L3 | L4 | Total |
|--------|----|----|----|----|-------|
| S1 | 34 | 190 | 144 | 71 | 439 |
| S2 | 35 | 198 | 148 | 88 | 469 |
| S3 | 38 | 210 | 153 | 83 | 484 |
| S4 | 35 | 200 | 152 | 80 | 467 |
| S5 | 36 | 196 | 152 | 80 | 464 |

### Ramp Flows

| Time | Ramp1 | Ramp2 |
|------|-------|-------|
| 6:00 | 12 | 2 |

---

## Pure LC Segments Analysis

### S2 → S3 (No Ramp)

| Lane | S2 | S3 | Δ |
|------|----|----|---|
| L1 | 35 | 38 | +3 |
| L2 | 198 | 210 | +12 |
| L3 | 148 | 153 | +5 |
| L4 | 88 | 83 | -5 |
| **Total** | 469 | 484 | +15 |

Note: ΔTotal = +15 suggests measurement timing offset (no ramp here).

### S4 → S5 (No Ramp)

| Lane | S4 | S5 | Δ |
|------|----|----|---|
| L1 | 35 | 36 | +1 |
| L2 | 200 | 196 | -4 |
| L3 | 152 | 152 | 0 |
| L4 | 80 | 80 | 0 |
| **Total** | 467 | 464 | -3 |

ΔTotal ≈ 0 (good conservation). Observed LC pattern:
- L2 lost ~4 vehicles → likely moved to L1 or L3
- L1 gained ~1 vehicle

**Estimated LC Rate (S4→S5):**
```
L2 outflow ≈ 4/200 = 2.0% per segment
```

---

## Ramp Segments Analysis

### S1 → S2 (Ramp1 = 12 vehicles into L4)

| Lane | S1 | S2 | Δ_observed | Ramp | Δ_LC |
|------|----|----|------------|------|------|
| L1 | 34 | 35 | +1 | 0 | +1 |
| L2 | 190 | 198 | +8 | 0 | +8 |
| L3 | 144 | 148 | +4 | 0 | +4 |
| L4 | 71 | 88 | +17 | +12 | +5 |

After subtracting Ramp1 from L4:
- L4 net LC = +5 (vehicles changed INTO L4 from L3)
- Or: Ramp vehicles immediately dispersing to other lanes

### S3 → S4 (Ramp2 = 2 vehicles into L4)

| Lane | S3 | S4 | Δ_observed | Ramp | Δ_LC |
|------|----|----|------------|------|------|
| L1 | 38 | 35 | -3 | 0 | -3 |
| L2 | 210 | 200 | -10 | 0 | -10 |
| L3 | 153 | 152 | -1 | 0 | -1 |
| L4 | 83 | 80 | -3 | +2 | -5 |

After subtracting Ramp2 from L4:
- L4 net LC = -5 (vehicles left L4)
- L2 lost 10, L1 lost 3 → likely off-ramp or measurement noise

---

## Key Insights

1. **Pure LC segments (S2→S3, S4→S5)** show small lane redistributions (~2-5% per segment)

2. **Ramp segments** require subtracting ramp flow from L4 before estimating LC

3. **ΔTotal ≠ 0** in ramp-free segments indicates measurement noise (~3-5%)

4. **Dominant pattern:** Vehicles tend to move LEFT (L4→L3→L2→L1) over the corridor

---

## Recommended LC Parameters for Simulation

Based on S4→S5 (cleanest segment):

| Parameter | Value | Derivation |
|-----------|-------|------------|
| LC probability per segment | 2% | 4/200 from L2 outflow |
| LC direction bias | Left-preferring | Net flow toward L1 |
| Min gap for LC | Literature default | Cannot infer from aggregate data |

