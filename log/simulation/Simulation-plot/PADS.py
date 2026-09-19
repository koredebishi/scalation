"""Plot replicated one-way pathway summaries emitted by the Scala models.

Bars show the mean over measured replications; thin grey whiskers show the
95% CI half-width ("ci95"). The script also prints each CI to the terminal
for writing the caption.
"""
import json
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Patch

HERE = Path(__file__).resolve().parent

# (display name, summary file, bar colour)
MODELS = [
    ("Fixed meso", "fixed_meso.json", "#1f5fa8"),
    ("IDM meso", "idm_meso.json", "#c0392b"),
    ("Micro IDM", "micro_idm.json", "0.45"),
]

# (panel title, summary key, value label format)
TRAFFIC_PANELS = [
    ("Link 0 speed\n(m/s)", "link0SpeedMps"),
    ("Mean travel\ntime (s)", "meanTravelTimeS"),
    ("Simulation\ntime (s)", "simulationTimeS"),
]
COST_PANELS = [
    ("Computation\ntime (ms)", "computationTimeMs"),
    ("Activations\nper vehicle", "activationsPerVehicle"),
]

# Whisker style: light, thin, no caps, so bars stay the focus.
ERROR_STYLE = dict(elinewidth=0.6, ecolor="0.35")


def load_summary(name, filename):
    path = HERE / filename
    if not path.is_file():
        raise FileNotFoundError(f"Missing {path}. Run the {name} model first.")
    summary = json.loads(path.read_text(encoding="utf-8"))
    if summary.get("model") != name:
        raise ValueError(f"{path} is for {summary.get('model')!r}, expected {name!r}.")
    if summary.get("measuredReplications", 0) < 1:
        raise ValueError(f"{path} contains no measured replications.")
    return summary


def metric(summaries, key):
    """Return (means, CI half-widths) for one metric; missing CIs become 0."""
    means = np.array([s[key]["mean"] for s in summaries], dtype=float)
    ci = np.array([s[key].get("ci95") or 0.0 for s in summaries], dtype=float)
    return means, ci


def draw_panel(ax, title, means, ci, colors):
    x = np.arange(len(means))
    ax.bar(x, means, width=0.7, color=colors, yerr=ci, capsize=2,
           error_kw=ERROR_STYLE)

    top = (means + ci).max()
    ax.set_ylim(0, 1.18 * top)
    ax.set_xticks([])
    ax.set_title(title, fontsize=8)
    ax.tick_params(axis="y", labelsize=6.5)
    ax.spines[["top", "right"]].set_visible(False)


def report_ci(summaries, key):
    """Print mean ± CI per model (terminal only, for writing the caption)."""
    parts = []
    for (name, _, _), s in zip(MODELS, summaries):
        mean, ci = s[key]["mean"], s[key].get("ci95")
        if ci is None:
            parts.append(f"{name}: {mean:.3g} (no CI)")
        else:
            parts.append(f"{name}: {mean:.3g} ± {ci:.3g} ({100 * ci / abs(mean):.1f}%)")
    print(f"{key:24s} " + " | ".join(parts))


def main():
    summaries = [load_summary(name, file) for name, file, _ in MODELS]
    colors = [color for _, _, color in MODELS]

    print("Replications:", ", ".join(
        f"{name}={s['measuredReplications']}" for (name, _, _), s in zip(MODELS, summaries)))

    plt.rcParams.update({"font.size": 8, "font.family": "serif"})
    fig = plt.figure(figsize=(7.0, 2.2))
    gs = fig.add_gridspec(1, 6, width_ratios=[1, 1, 1, 0.25, 1, 1], wspace=0.45)
    axes = [fig.add_subplot(gs[0, i]) for i in (0, 1, 2, 4, 5)]

    for ax, (title, key) in zip(axes, TRAFFIC_PANELS + COST_PANELS):
        means, ci = metric(summaries, key)
        draw_panel(ax, title, means, ci, colors)
        report_ci(summaries, key)

    fig.text(0.30, 1.08, "(a) Traffic outputs", ha="center", fontsize=9, weight="bold")
    fig.text(0.80, 1.08, "(b) Cost", ha="center", fontsize=9, weight="bold")
    fig.legend(handles=[Patch(color=c, label=n) for n, _, c in MODELS],
               loc="lower center", ncol=len(MODELS), frameon=False,
               bbox_to_anchor=(0.5, -0.08), fontsize=8)

    for ext, kwargs in (("pdf", {}), ("png", {"dpi": 300})):
        fig.savefig(HERE / f"pathway_bars.{ext}", bbox_inches="tight", **kwargs)


if __name__ == "__main__":
    main()
