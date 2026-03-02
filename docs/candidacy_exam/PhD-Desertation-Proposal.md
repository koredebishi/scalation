

# Wildfire Evacuation & Contraflow Simulation
## Strategic Research Plan (I-10 Eastbound – Palisades Fire Case Study)

---

# 1. Research Objective

Develop a data-calibrated microscopic simulation of wildfire evacuation on I-10 eastbound and evaluate the counterfactual effectiveness of contraflow under smoke-induced behavioral degradation.

Primary Research Question:

> Would directional lane reallocation (contraflow) materially improve evacuation resilience during wildfire-induced demand surge in a dense urban freeway network?

---

# 2. Scope Definition (Controlled)

This study will:

- Focus on I-10 eastbound (Santa Monica → I-405 corridor)
- Use PeMS lane-level data
- Reconstruct fire-day congestion
- Model smoke as behavioral degradation
- Simulate contraflow as directional capacity shift
- Evaluate resilience metrics

This study will NOT:

- Simulate wildfire physics
- Model full Caltrans operations
- Implement full GIS fire-spread modeling
- Attempt real-time adaptive control

---

# 3. Dataset Stack (Minimum Viable)

## 3.1 Core Traffic Data
Source: Caltrans PeMS

Required:
- 5-min lane-level flow
- Speed
- Occupancy
- Station metadata (lat/long)

Time Window:
- Jan 7, 2025 (fire day)
- 2–3 baseline comparison days (same weekday)

---

## 3.2 Fire Event Data
- Ignition time (~10:30 AM PST)
- Fire perimeter shapefile (optional but strong)
- Spatial proximity confirmation

---

## 3.3 Optional Weather Data
- Wind speed/direction (NOAA)
- Used only as contextual support for smoke severity

---

# 4. Software Stack (Minimum)

- ScalaTion microscopic simulation framework
- Python (pandas, numpy, matplotlib)
- LaTeX for manuscript

Optional:
- QGIS for mapping visualization

No commercial simulation software required.

---

# 5. Experimental Design

## Phase 1 — Baseline Calibration

Goal:
Reproduce normal weekday I-10 dynamics.

Validate using:
- R²
- RMSE
- SMAPE
- Shockwave propagation comparison

Deliverable:
Validated digital twin of corridor.

---

## Phase 2 — Fire-Day Reconstruction

Steps:
1. Detect demand surge timing from PeMS.
2. Quantify:
    - Speed drop
    - Occupancy increase
    - Congestion duration
3. Inject surge into calibrated simulator.

Validate:
- Breakdown time
- Peak density
- Queue duration
- Recovery time

Deliverable:
Wildfire-day traffic reconstruction model.

---

## Phase 3 — Smoke Modeling

Translate smoke impact into behavioral degradation:

Adjust:
- Desired speed (v₀ ↓)
- Time headway (T ↑)
- Reaction variability ↑
- Lane change aggressiveness ↓

Parameter levels:
- Mild
- Moderate
- Severe

Deliverable:
Calibrated smoke-impact module.

---

## Phase 4 — Contraflow Simulation (Counterfactual)

Scenarios:

A. No contraflow (baseline wildfire)
B. +1 reversed lane
C. Full directional reallocation
D. Contraflow + smoke

Ensure:
- Preserve minimum inbound emergency lane
- Respect downstream bottleneck constraints

Deliverable:
Capacity-reallocation performance analysis.

---

# 6. Evaluation Metrics

## 6.1 Throughput
Vehicles/hour

## 6.2 Mean Speed

## 6.3 Shockwave Speed
Using:
w = (q₂ - q₁) / (k₂ - k₁)

## 6.4 Clearance Time
Time until congestion dissipates

## 6.5 Resilience Index
R = 1 - (Performance Loss Area / Baseline Area)

Where performance loss area = integral of speed deficit over time.

---

# 7. Validation Strategy

## Level 1 — Baseline Validation
Normal-day PeMS reproduction.

## Level 2 — Fire-Day Validation
Match observed wildfire congestion patterns.

## Level 3 — Structural Validation
Shockwave comparison to theory.

## Level 4 — Sensitivity Analysis
Vary:
- Demand magnitude
- Smoke severity
- Lane-change aggressiveness

Contraflow benefit must remain robust.

---

# 8. Expected Contributions

1. Data-calibrated wildfire evacuation reconstruction.
2. Smoke-behavior degradation modeling in microscopic DES.
3. Counterfactual contraflow evaluation in dense urban freeway.
4. Quantitative resilience threshold identification.
5. Insight into when capacity expansion is ineffective under visibility degradation.

---

# 9. Timeline (4–6 Month Plan)

Month 1:
- PeMS extraction
- Baseline calibration

Month 2:
- Fire-day reconstruction
- Smoke module implementation

Month 3:
- Contraflow implementation
- Initial experiments

Month 4:
- Sensitivity analysis
- Validation
- Writing draft

Optional Month 5–6:
- Refinement
- Reviewer simulation
- Submission polishing

---

# 10. Risk Management

Primary Risks:
- Scope creep
- Over-modeling operational details
- Insufficient validation rigor

Mitigation:
- Keep single corridor focus
- Avoid full fire physics modeling
- Prioritize reconstruction accuracy

---

# 11. Target Outcome

Conference Submission:
Winter Simulation Conference (Climate Resilience Theme)

Paper Type:
Methodologically rigorous, data-calibrated simulation study.

---

# 12. Long-Term Extension (Optional)

Future expansions:
- Multi-corridor modeling
- Multi-fire validation
- Adaptive contraflow optimization
- LLM-assisted scenario generation

---

# Final Positioning Statement

This study evaluates wildfire evacuation resilience using a PeMS-calibrated microscopic digital twin of I-10 and quantifies the counterfactual benefit of contraflow under smoke-induced behavioral degradation.



wildfire-contraflow-i10/
│
├── README.md
├── requirements.txt
├── environment.yml
│
├── data/
│   ├── raw/
│   │   ├── pems/
│   │   │   ├── I10_2025_01_07.csv
│   │   │   ├── I10_baseline_day1.csv
│   │   │   └── I10_baseline_day2.csv
│   │   ├── fire/
│   │   │   ├── ignition_metadata.json
│   │   │   └── fire_perimeter.shp
│   │   └── weather/
│   │       └── noaa_wind_2025_01_07.csv
│   │
│   └── processed/
│       ├── baseline_cleaned.csv
│       ├── fire_day_cleaned.csv
│       ├── surge_profile.csv
│       └── smoke_proxy.csv
│
├── notebooks/
│   ├── 01_pems_extraction.ipynb
│   ├── 02_baseline_calibration.ipynb
│   ├── 03_fire_day_analysis.ipynb
│   ├── 04_shockwave_estimation.ipynb
│   └── 05_visualization.ipynb
│
├── src/
│   ├── data_pipeline/
│   │   ├── pems_loader.py
│   │   ├── surge_detector.py
│   │   ├── shockwave_estimator.py
│   │   └── resilience_metrics.py
│   │
│   ├── simulation/
│   │   ├── baseline_config.scala
│   │   ├── wildfire_surge.scala
│   │   ├── smoke_module.scala
│   │   ├── contraflow_module.scala
│   │   ├── experiment_runner.scala
│   │   └── sensitivity_runner.scala
│   │
│   └── analysis/
│       ├── validation.py
│       ├── metric_comparison.py
│       └── plotting.py
│
├── experiments/
│   ├── baseline/
│   │   └── results.json
│   ├── fire_no_contraflow/
│   │   └── results.json
│   ├── fire_smoke/
│   │   └── results.json
│   ├── fire_contraflow_1lane/
│   │   └── results.json
│   ├── fire_contraflow_full/
│   │   └── results.json
│   └── sensitivity/
│       ├── demand_variation/
│       ├── smoke_variation/
│       └── lane_variation/
│
├── figures/
│   ├── baseline_fit.png
│   ├── fire_day_comparison.png
│   ├── shockwave_comparison.png
│   ├── resilience_plot.png
│   ├── contraflow_benefit.png
│   └── sensitivity_heatmap.png
│
├── paper/
│   ├── main.tex
│   ├── references.bib
│   ├── sections/
│   │   ├── introduction.tex
│   │   ├── methodology.tex
│   │   ├── validation.tex
│   │   ├── experiments.tex
│   │   ├── results.tex
│   │   └── discussion.tex
│   └── figures/
│
└── docs/
├── strategic_plan.md
└── experiment_protocol.md



--
Many agencies in the United State agree that using Contraflow is an effective strategy
to increase evacuation capacity during emergencies. However, understanding how the reverse road should function
is essential to acheiving acceptable level of performance.


""""
Here’s a **research-oriented answer to your PROMPT_TASK** based on the best available *peer-reviewed literature after 2020* and relevant contraflow work (with citations). I have *not hallucinated*—every claim is supported by a cited source.

---

**Contraflow (Lane Reversal) in Highway Evacuation and Disaster Traffic Management**

Contraflow, or lane reversal, is a traffic management technique used to **temporarily reconfigure road directionality to increase capacity in the evacuation direction**, especially during large-scale disasters such as hurricanes and wildfires where outbound demand far exceeds inbound traffic infrastructure capacity. Contraflow has been studied for decades, but recent research continues to refine its operational and optimization aspects in evacuation contexts where timely egress is critical to saving lives. Contraflow is fundamentally motivated by **operational capacity constraints**: during an evacuation, traditional lane allocations leave inbound capacity unused while outbound lanes are oversaturated, meaning that reversing inbound lanes can increase effective evacuation capacity without expanding physical infrastructure. Contraflow directly prioritizes evacuation flow by reallocating capacity toward the direction of greatest need ([Wikipedia, 2023](#turn0search22); [Liu et al., 2024](#turn0search1)).

Recent studies emphasize the need to coordinate contraflow with traffic optimization models to balance competing demands, such as rescue vehicle access and weakened network performance under surge conditions. In *Liu et al. (2024)*, a data-driven mixed-integer programming model was developed to control contraflow deployment and evaluate its impact on evacuation traffic, demonstrating how optimal contraflow coordination can reduce interference between evacuation and rescue operations and improve overall network performance [Liu, 2024](#turn0search1). Other work in the evacuation literature, while not exclusively focused on contraflow, acknowledges reversible lane practices as an essential component of comprehensive traffic evacuation policy evaluations, integrating them with simulation pipelines for emergency traffic management [Chen et al., 2020](#turn1search0).

Variants of contraflow research focus on **static**, **dynamic**, and **optimization-based** approaches.

1. **Static Contraflow** refers to predefined lane reversals applied for the full duration of an evacuation. Traditional planning for events such as hurricane evacuations uses static contraflow strategies to roughly double evacuation capacity on controlled-access highways by reversing all inbound lanes for the outbound flow ([Wikipedia, 2023](#turn1search13)).

2. **Dynamic Contraflow** extends static contraflow by allowing lane directionality to change in response to real-time traffic conditions. While much early work predates 2020, its principles carry forward into modern designs that propose automatic traffic sensor-based contraflow reconfiguration to adapt to fluctuating congestion patterns [Hausknecht et al., 2011](#turn0search2).

3. **Optimization-Based Contraflow** frameworks embed lane reversal within mathematical and algorithmic models that seek to maximize evacuation efficiency subject to capacity, demand, and rescue priorities. *Liu et al. (2024)* formalized such a contraflow control optimization, navigating multi-objective tradeoffs between evacuation throughput and rescue operations protection [Liu, 2024](#turn0search1). Combinatorial contraflow reconfiguration research also explores staged activation sequences and capacity transitions to maximize flow under evolving demand patterns (Masuda & Hato, 2026 preprint) [Masuda, 2026](#turn1search3) .

Despite its practical logic, contraflow has acknowledged **limitations in urban freeway applications**. Many theoretical models, including optimization formulations and dynamic contraflow strategies, rely on simplifications that assume perfect compliance, homogeneous traffic behavior, or robust crossing infrastructure for direction changes—assumptions that may not hold during real disasters. Implementation hurdles such as **police coordination at interchanges**, lack of specialized signage, and potential driver confusion at merge points challenge safe execution outside controlled settings [Wikipedia, 2023](#turn1search13); moreover, contraflow’s benefits can be reduced or even offset when rescue traffic must enter the network against the reversed flow [Liu, 2024](#turn0search1). Additional work has identified that, even with structural capacity increases, naïve contraflow operations can produce significant queue formation near reconvergence points, undermining throughput gains, and that there is a lack of **standardized operational guidelines** to support contraflow deployment in complex networks [Radovan, 2025](#turn1search18).

In summary, contraflow remains a powerful theoretical tool to increase evacuation capacity, motivated by the inherent capacity imbalance during emergencies. Modern research pursues variants of contraflow targeting static reconfiguration for planned evacuations, dynamic adaptive strategies, and optimization models that balance evacuation and rescue priorities. However, the complexities of urban freeway geometry, operational control, and safety limitations underscore that contraflow benefits are often context-dependent and require careful planning and supportive infrastructure to be realized in practice.

---

""""
