# RAE-DiT: Dual-Head Diffusion Graph Forecasting with Representation Autoencoders# PF–DGR (Parameter–Free Dynamic Graph Re-weighting)



This repository contains the paper and reference implementation of **RAE-DiT** (Representation Autoencoder Diffusion Transformer), a unified framework for traffic forecasting that combines:This repository contains LaTeX for the paper and a minimal, well-documented

- **Numeric sensor-level predictions** (speed, flow, occupancy)reference implementation of the PF–DGR operator and model components.

- **Generative traffic map synthesis** (visual density/speed maps)

- Paper sources: `paper/main.tex`

## Key Features- Operator (NumPy): `src/lanes/dynamic_adjacency.py`

- Model components (PyTorch): `src/models/pf_dgr.py`

- **Frozen Pretrained Encoders**: Leverages DINOv2/MAE for semantically rich latent representations

- **Dual-Head Architecture**: Unified backbone with parallel numeric and generative heads## Quick Usage (Operator)

- **Graph-Conditioned Diffusion**: Road network topology integrated via graph embeddings

- **Dimension-Aware Noise Scheduling**: Stable high-dimensional latent diffusion```python

- **Noise-Augmented Decoder**: Robust reconstruction bridging discrete and continuous latentsimport numpy as np

from itsc26_lc_explain.src.lanes import PFReweightingOperator, PFDGROptions

## Paper

# Fake window: n nodes, d features, W window

Paper sources: `paper/main.tex` and `paper/paper1_pfdgr.tex`n, d, W = 50, 3, 24

X_window = np.random.randn(n, d, W)

## Architecture OverviewM = np.ones((n, n), dtype=np.int32)  # topology mask; set M[i, j] = 0 to forbid



```op = PFReweightingOperator(PFDGROptions(k=8, epsilon=0.1, similarity="pearson"))

                  ┌───────────────────────────────────────────────────┐A_t, S = op.compute(X_window, M)

                  │   Historical Traffic Inputs (xₜ₋ₖ … xₜ)           │print(A_t.shape, S.shape)  # (n, n), (n, n)

                  │   • Sensor series or traffic maps                 │```

                  └───────────────────────────────────────────────────┘

                                     │## Quick Usage (Model backbone + exits)

                                     ▼

        ┌────────────────────────────────────────────────────────────┐```python

        │  Representation Autoencoder (RAE)                          │import torch

        │  ├─ Frozen Pretrained Encoder E (e.g., DINOv2 / MAE)       │from itsc26_lc_explain.src.models import BackboneConfig, PFDGRBackbone, select_anytime_exit

        │  └─ Lightweight Decoder D (trained with noise augmentation)│

        │      zₜ = E(xₜ),  x̂ₜ = D(zₜ + n),  n ~ N(0, σ²I)         │n, din, dout = 50, 3, 1

        └────────────────────────────────────────────────────────────┘cfg = BackboneConfig(in_dim=din, hid_dims=[32, 32], out_dim=dout, act="relu", clip=6.0)

                                     │backbone = PFDGRBackbone(cfg, n_exits=2)

                        Latent Tokens Zₜ ∈ ℝⁿˣᵈ

                                     │A = torch.from_numpy(A_t).float()          # from operator

                                     ▼X_t = torch.randn(n, din)                  # current features

        ┌────────────────────────────────────────────────────────────┐preds, confs, depths = backbone(A, X_t)

        │  Graph Conditioning Module                                 │exit_idx = select_anytime_exit(confs, thresholds=[0.7], budget_ms=10.0, layer_cost_ms=5.0)

        │  ├─ Compute adjacency/diffusion kernel Aₜ                  │Y_hat = preds[exit_idx]

        │  ├─ Embed graph structure φ(Gₜ)                            │```

        │  └─ Concatenate [Zₜ ; φ(Gₜ)] as DiT input                 │

        └────────────────────────────────────────────────────────────┘Refer to `paper/main.tex` Section “Method: PF–DGR” for algorithmic details

                                     │and guarantees (stability, bounded variation, selective risk).

                                     ▼

        ┌────────────────────────────────────────────────────────────┐## Requirements

        │  Diffusion Transformer (DiT / DiTDH)                       │

        │  • Hidden width w ≥ token dim d                            │- NumPy

        │  • Dimension-dependent noise schedule shift (α-scaling)    │- PyTorch

        │  • Predict denoised latent trajectory Ẑₜ₊₁:ₜ₊ₕ            │

        └────────────────────────────────────────────────────────────┘See `requirements.txt` for versions.

                          ├───────────────┬────────────────┤
                          │               │                │
                          ▼               ▼                ▼
        ┌────────────────────────┐   ┌─────────────────────────────┐
        │ Numeric Flow Head      │   │ Generative Map Head          │
        │ • Pool + MLP regression│   │ • Diffusion sampling in Z-space│
        │   → flow/speed values  │   │ • Decode maps via D(z)       │
        │   (MAE / RMSE loss)   │   │   (L1 + SSIM loss)           │
        └────────────────────────┘   └─────────────────────────────┘
```

## Quick Usage

### RAE-DiT Model

```python
import torch
from itsc26_lc_explain.src.models import RAEDiTModel, RAEDiTConfig

# Configure model
config = RAEDiTConfig(
    encoder_name="dinov2_small",  # or "mae", "custom"
    token_dim=384,
    hidden_width=512,  # w >= d requirement
    n_layers=3,
    n_heads=6,
    freeze_encoder=True
)

# Initialize model
model = RAEDiTModel(config)

# Historical traffic data (batch, time, height, width, channels)
x_hist = torch.randn(8, 12, 64, 64, 3)
# Road network adjacency
A = torch.randn(50, 50)  # n_nodes x n_nodes

# Forward pass - get both numeric and generative predictions
numeric_pred, map_pred = model(x_hist, A, horizon=12)
# numeric_pred: (batch, n_nodes, horizon, features)
# map_pred: (batch, horizon, height, width, channels)
```

### Graph Conditioning Module

```python
import torch
from itsc26_lc_explain.src.lanes import GraphConditioner

# Create graph conditioning module
conditioner = GraphConditioner(n_nodes=50, embed_dim=384)

# Compute adjacency (can use distance, connectivity, or learned)
A = torch.randn(50, 50)

# Get graph embeddings
G_embed = conditioner(A)
# G_embed: (50, 384) - ready to concatenate with latent tokens
```

## Methodology

### 1. Latent Representation Encoder (RAE)

Frozen pretrained encoder E (e.g., DINOv2, MAE) produces latent tokens:
```
zₜ = E(xₜ) ∈ ℝⁿˣᵈ
```

Lightweight decoder D trained with noise augmentation:
```
x̂ₜ = D(zₜ + n), n ~ N(0, σ²I)
```

### 2. Diffusion Transformer Backbone

Hidden width w satisfies **w ≥ d** for convergence in high-dimensional latent spaces.

### 3. Dimension-Dependent Noise Schedule

```
tₘ = (α tₙ) / (1 + (α - 1) tₙ), α = √(m/n)
```
where n = 4096 (base dimension), m = number of tokens × token dim.

### 4. Graph Conditioning

Graph features Gₜ = f(Aₜ) projected to token-space:
```
Zₜ = [zₜ ; φ(Gₜ)]
```

### 5. Dual-Head Forecasting

**Numeric Flow Head**:
```
X̂ₜ₊₁:ₜ₊ₕ = Wf · Pool(D(Zₜ₊₁:ₜ₊ₕ))
```
Loss: MAE + RMSE + MAPE

**Generative Map Head**:
```
zₜ₊₁:ₜ₊ₕ ~ pθ(z | Z₁:ₜ)
x̂ₜ₊ₕ = D(zₜ₊ₕ)
```
Loss: L1 + SSIM + PSNR

## Design Innovations

1. **RAE Latents**: Semantically rich frozen encoders outperform VAEs
2. **Width ≥ Dim Rule**: Ensures stable diffusion convergence
3. **Dimension-Shifted Schedule**: Adapts noise to latent dimensionality
4. **Noise-Augmented Decoder**: Bridges training/generation distribution gap
5. **Graph-Conditioned Dual Heads**: Unifies numeric and visual forecasting

## Datasets

- **METR-LA**: 207 detectors, Los Angeles highways
- **PEMS-BAY**: 325 detectors, Bay Area
- **PeMSD4**: 307 detectors, flow data
- **PeMSD8**: 170 detectors, flow data
- **Highway Camera Maps**: Real-world traffic camera images

## Results

### Numeric Forecasting (12-step horizon)

| Model | METR-LA MAE | PEMS-BAY MAE | PeMSD4 MAE |
|-------|-------------|--------------|------------|
| Graph WaveNet | 2.99 | 1.95 | 22.37 |
| AGCRN | 2.87 | 1.89 | - |
| **RAE-DiT** | **2.74** | **1.83** | **21.12** |

4-8% MAE reduction over best baselines.

### Generative Map Quality

| Model | SSIM ↑ | PSNR ↑ | FID ↓ |
|-------|--------|--------|-------|
| VAE-LDM | 0.742 | 24.3 | 35.2 |
| Standard DiT | 0.781 | 26.1 | 28.7 |
| **RAE-DiT** | **0.823** | **28.4** | **21.3** |

Significant improvements in visual generation quality.

## Key Ablations

| Configuration | MAE | SSIM |
|---------------|-----|------|
| RAE-DiT (full) | **2.74** | **0.823** |
| - w/o noise-aug decoder | 2.91 (+6.2%) | 0.792 |
| - w/o dim-aware schedule | 2.88 (+5.1%) | 0.801 |
| - w/o graph conditioning | 3.02 (+10.2%) | 0.785 |
| - use VAE instead of RAE | 3.15 (+15.0%) | 0.742 |
| - width w < d | 2.97 (+8.4%) | 0.769 |

Each component contributes significantly to overall performance.

## Requirements

- Python 3.8+
- PyTorch 2.0+
- NumPy
- timm (for pretrained encoders)
- transformers (optional, for additional encoders)

See `requirements.txt` for complete dependencies.

## Installation

```bash
# Clone repository
git clone https://github.com/koredebishi/itsc26-lc-explain.git
cd itsc26-lc-explain

# Create virtual environment
python -m venv venv
source venv/bin/activate  # or `venv\Scripts\activate` on Windows

# Install dependencies
pip install -r requirements.txt
```

## Training

```bash
# Train RAE-DiT model on METR-LA
python scripts/train.py \
    --config configs/pf_dgr_pems_lane.yaml \
    --dataset METR-LA \
    --encoder dinov2_small \
    --epochs 200 \
    --batch_size 64
```

## Citation

If you use this code or methodology, please cite:

```bibtex
@inproceedings{raedit2025,
  title={Dual-Head Diffusion Graph Forecasting with Representation Autoencoders for Traffic Prediction},
  author={Your Name},
  booktitle={IEEE ITSC},
  year={2025}
}
```

## License

See `LICENSE` file for details.

## Contact

For questions or collaboration: your.email@uga.edu

---

Refer to `paper/main.tex` for complete algorithmic details including:
- Dimension-aware noise scheduling formulations
- Width ≥ dimension convergence guarantees
- Noise-augmented decoder training procedures
- Dual-head forecasting objective functions
