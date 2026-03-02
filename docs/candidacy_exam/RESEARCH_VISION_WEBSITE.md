research.md

"""---
layout: homepage
title: "Research Overview"
permalink: /research/
---
<div class="research-page-header">
  <h1>Korede R. Bishi</h1>
  <p><strong>Ph.D. Student, Computer Science, University of Georgia</strong></p>
  <p><strong>Research Area:</strong> Discrete Event Simulation, Calibration, and Infrastructure Resilience &nbsp;|&nbsp; <strong>Application Domain:</strong> Microscopic Traffic Simulation</p>
  <p>Advisor: <a href="https://openreview.net/profile?id=~John_A._Miller1">Dr. John A. Miller</a> &nbsp;·&nbsp; <a href="https://github.com/scalation/scalation_2.0">ScalaTion Framework</a> &nbsp;·&nbsp; <a href="https://pems.dot.ca.gov/">PeMS Sensor Data</a></p>
</div>

<main class="research-page-content">
  <section class="problem-statement-section">
    <h2>1. Formal Problem Statement</h2>
    <p>Let <strong>M(θ)</strong> denote a microscopic traffic simulation model parameterized by a vector <strong>θ</strong> containing car-following parameters, arrival process parameters, and structural modeling parameters. Let <strong>D</strong> denote empirical observations from the California Performance Measurement System (PeMS), recorded at lane-level resolution.</p>
    <p>The calibration objective is:</p>
    <blockquote><p style="text-align:center; font-size:1.1em;"><strong>L(θ) = Error( M(θ), D )</strong></p></blockquote>
    <p>where error is measured using lane-level flow and speed discrepancies. The dissertation investigates three interconnected questions:</p>
    <ol>
      <li>Which modeling components materially affect <strong>L(θ)</strong> — and which do not?</li>
      <li>How do we calibrate <strong>θ</strong> under physically meaningful constraints so that speed accuracy is improved without degrading flow fidelity?</li>
      <li>Can a validated <strong>M(θ*)</strong> support counterfactual resilience analysis under extreme disruption where real-world experimentation is infeasible?</li>
    </ol>
  </section>

  <section class="modeling-foundations-section">
    <h2>2. Modeling Foundations</h2>

    <section class="car-following-models-section">
      <h3>2.1 Car-Following Models</h3>
      <p>Two established car-following formulations are implemented and evaluated:</p>
      <ul>
        <li><strong>Intelligent Driver Model (IDM)</strong>: continuous acceleration function based on desired speed, spacing, and relative velocity</li>
        <li><strong>Gipps Model</strong>: safe-speed formulation with explicit reaction time and braking distance parameters</li>
      </ul>
      <p>The dissertation does not introduce new car-following theory. It evaluates calibration behavior, lane-level predictive performance, and interaction with arrival processes across both models under physically constrained parameter bounds on the US-101 corridor.</p>
    </section>

    <section class="arrival-processes-section">
      <h3>2.2 Arrival Processes</h3>
      <p>Vehicle arrivals are modeled using three distributions:</p>
      <ul>
        <li><strong>Poisson process</strong>: memoryless, no minimum headway constraint</li>
        <li><strong>Erlang-2 distribution</strong>: reduced variance relative to Poisson</li>
        <li><strong>Shifted Erlang-2 distribution</strong>: enforces a realistic minimum inter-arrival headway</li>
      </ul>
      <p>Study 1 establishes that arrival-process choice is a <em>structural</em> modeling decision, not a calibration afterthought. The shifted Erlang-2 distribution reduces flow prediction error by approximately <strong>28%</strong> compared to Poisson by enforcing minimum headway — a finding consistent with prior work on headway distributions in freeway microsimulation.</p>
      <p>Arrival processes therefore govern flow accuracy independently of car-following parameter calibration.</p>
    </section>

    <section class="numerical-integration-section">
      <h3>2.3 Numerical Integration</h3>
      <p>Vehicle dynamics are integrated using eight numerical schemes ranging from Explicit Euler to Dormand–Prince (RK45). Study 1 demonstrates that integrator choice produces <strong>&lt;1% variation</strong> in lane-level predictive accuracy across all eight methods.</p>
      <p>This confirms prior findings (Treiber &amp; Kanagaraj, 2015; Přikryl &amp; Vaniš, 2017) that simple ballistic integration suffices for car-following dynamics, and justifies prioritizing arrival process modeling over integrator refinement in calibration effort.</p>
    </section>
  </section>

  <section class="study1-section">
    <h2>3. Study 1 — Structural Sensitivity Analysis <em style="font-weight:400; font-size:0.9em;">(ANNSIM 2026, Submitted)</em></h2>
    <p><strong>Title:</strong> "Beyond Corridor Averages: Lane-Level Validation of Microscopic Freeway Simulation with Data-Driven Arrivals"</p>

    <h3>Motivation</h3>
    <p>Microscopic traffic simulators require many modeling choices — numerical integration schemes, vehicle arrival processes, time-step resolution — yet the sensitivity of simulation accuracy to these choices is poorly understood. Practitioners often adopt defaults without systematic evaluation.</p>

    <h3>Approach</h3>
    <p>We systematically varied two key modeling decisions — numerical integrator (8 methods, from Euler to Dormand–Prince) and vehicle arrival process (Poisson, Erlang-2, shifted Erlang-2) — and evaluated their impact on lane-level flow and speed accuracy across five PeMS detector stations on a US-101 freeway corridor.</p>

    <h3>Key Findings</h3>
    <ul>
      <li>Numerical integrator choice has <strong>&lt;1% impact</strong> on simulation accuracy — simple ballistic integration suffices</li>
      <li>Vehicle <strong>arrival process modeling substantially affects fidelity</strong> — the shifted Erlang-2 distribution reduces flow error by ~28% compared to Poisson by enforcing a realistic minimum headway</li>
      <li><strong>Lane-level validation</strong> reveals dynamics that corridor-level aggregation obscures</li>
    </ul>

    <h3>Significance</h3>
    <p>These findings direct calibration effort toward the modeling decisions that matter (arrival processes) and away from those that do not (integrators), informing the constrained calibration approach in Study 2.</p>
  </section>

  <section class="study2-section">
    <h2>4. Study 2 — Constrained Calibration Framework <em style="font-weight:400; font-size:0.9em;">(WSC 2026, In Progress)</em></h2>
    <p><strong>Title:</strong> "Comparative Analysis of Car-Following Models and Optimization Algorithms for Multi-Lane Traffic Simulation Calibration"</p>

    <h3>Motivation</h3>
    <p>Building on Study 1's finding that arrival processes govern flow accuracy while car-following parameters govern speed accuracy, this study asks: which combination of car-following model and optimization algorithm produces the best-calibrated simulation — and can constrained optimization improve speed prediction without degrading the flow accuracy already achieved by the arrival process?</p>

    <h3>Approach</h3>
    <ul>
      <li><strong>2 car-following models (IDM, Gipps) × 4 optimizers = 8 experimental conditions</strong>, all using corridor-level (MACRO) fitness</li>
      <li>Physically constrained parameter bounds centered on empirically validated defaults from Study 1</li>
      <li>Flow-protected fitness function — optimizer penalized for degrading flow accuracy beyond baseline (threshold 2.5% NRMSE)</li>
      <li>Fitness weighting: 0.2 × flow NRMSE + 0.8 × speed NRMSE — prioritizes speed calibration while protecting flow</li>
      <li>HPC deployment on <a href="https://gacrc.uga.edu/">Georgia Advanced Computing Resource Center (GACRC)</a> — 8 parallel SLURM array jobs</li>
      <li>Corridor-level validation: 5 detector stations × 4 lanes = 20 individual lane observation points on US-101</li>
    </ul>

    <h3>Calibration as Optimization</h3>
    <blockquote><p style="text-align:center; font-size:1.1em;"><strong>θ* = argmin<sub>θ ∈ Θ</sub> L(θ)</strong></p></blockquote>
    <p>where <strong>Θ</strong> represents physically plausible bounds and <strong>L(θ)</strong> incorporates corridor-level speed error and a flow-protection penalty.</p>

    <h3>Preliminary Findings</h3>
    <ul>
      <li>Car-following parameters primarily control speed dynamics; arrival processes control flow — confirming the structural separation identified in Study 1</li>
      <li>Constrained optimization achieves significant speed improvement while preserving flow accuracy</li>
      <li><strong>Genetic Algorithm</strong> achieves the best fitness among evaluated optimizers for both IDM and Gipps models</li>
      <li>Different optimizers converge to qualitatively different regions of parameter space, suggesting multiple local optima in the calibration landscape</li>
      <li>Flow-protection constraint is necessary — unconstrained speed optimization degrades flow fidelity</li>
    </ul>

    <h3>Significance</h3>
    <p>This study establishes a calibrated, validated simulation model suitable for scenario analysis — the prerequisite for Study 3's application to evacuation resilience.</p>
  </section>

  <section class="validation-framework-section">
    <h2>5. Validation Framework</h2>
    <p>All studies validate against <a href="https://pems.dot.ca.gov/">California PeMS</a> loop-detector data on the US-101 freeway corridor (5 stations × 4 lanes = 20 lane-level observation points).</p>
    <p>Primary metrics:</p>
    <ul>
      <li><strong>RMSE / NRMSE</strong> — normalized root mean squared error for cross-station comparability</li>
      <li><strong>Flow error</strong> — vehicles per hour per lane, compared against 5-minute PeMS aggregates</li>
      <li><strong>Speed error</strong> — mean speed per lane per time interval</li>
    </ul>
    <p>Lane-level validation reveals dynamics that corridor aggregation masks, particularly lane-specific speed variation, flow heterogeneity across lanes, and differential calibration sensitivity by lane position. This granularity is what motivates the entire lane-level validation philosophy of this dissertation.</p>
  </section>

  <section class="study3-section">
    <h2>6. Study 3 — Wildfire Disruption &amp; Resilience Modeling <em style="font-weight:400; font-size:0.9em;">(Proposed — Future Work)</em></h2>
    <p><strong>Title:</strong> "Evaluating Evacuation Resilience Under Wildfire Disruption: A PeMS-Calibrated Microscopic Simulation of I-10 During the 2025 Palisades Fire"</p>

    <h3>Motivation</h3>
    <p>On January 7, 2025, the Palisades Fire triggered mass evacuation along I-10 eastbound in Los Angeles. Severe congestion and smoke degraded corridor performance for hours. A recurring policy question — whether directional lane reallocation (contraflow) would have improved evacuation throughput — cannot be answered through real-world experimentation. Simulation provides the only feasible evaluation method.</p>
    <p><strong>This study is proposed and has not yet been implemented.</strong></p>

    <h3>Approach</h3>
    <ol>
      <li><strong>Baseline calibration:</strong> Reproduce normal-day I-10 traffic dynamics using PeMS data and the calibration methodology from Studies 1–2</li>
      <li><strong>Fire-day reconstruction:</strong> Detect demand surge timing from PeMS, reconstruct the congestion event in simulation</li>
      <li><strong>Smoke-behavior modeling:</strong> Translate smoke exposure into driving behavior degradation (reduced desired speed, increased headway, reduced lane-change aggressiveness) using visibility-impaired driving literature</li>
      <li><strong>Counterfactual scenarios:</strong> Evaluate evacuation performance under multiple capacity configurations — baseline, partial contraflow, full contraflow, and contraflow under smoke</li>
    </ol>

    <h3>Evaluation Metrics</h3>
    <ul>
      <li>Evacuation throughput (vehicles/hour)</li>
      <li>Mean corridor speed</li>
      <li>Congestion clearance time</li>
      <li>Resilience index: <strong>R = 1 − (performance loss area / baseline area)</strong></li>
    </ul>

    <h3>Expected Contributions</h3>
    <ul>
      <li>First PeMS-calibrated microscopic reconstruction of the 2025 Palisades Fire evacuation</li>
      <li>Smoke-as-behavioral-degradation module for microscopic DES</li>
      <li>Quantitative counterfactual evaluation of contraflow effectiveness under visibility impairment</li>
      <li>Identification of conditions under which capacity expansion alone is insufficient — requiring behavioral adaptation</li>
    </ul>
  </section>

  <section class="contributions-section">
    <h2>7. Technical Contributions to <a href="https://github.com/scalation/scalation_2.0">ScalaTion</a></h2>
    <p>All implementation extends the <a href="https://github.com/scalation/scalation_2.0">ScalaTion 2.0</a> simulation framework developed by <a href="https://openreview.net/profile?id=~John_A._Miller1">Dr. John A. Miller</a> and collaborators at the University of Georgia.</p>
    <table>
      <thead><tr><th>Contribution</th><th>Description</th></tr></thead>
      <tbody>
        <tr><td><strong>Lane-level validation infrastructure</strong></td><td>Per-lane flow and speed recording with automated PeMS data comparison</td></tr>
        <tr><td><strong>Multi-level fitness functions</strong></td><td>Macro (corridor) and micro (lane) calibration objectives with flow-protection constraint</td></tr>
        <tr><td><strong>Car-following model suite</strong></td><td>IDM, Gipps, and Krauss dynamics with configurable ODE solvers</td></tr>
        <tr><td><strong>Route abstraction</strong></td><td>Doubly-linked segment structure for multi-lane freeway corridors</td></tr>
        <tr><td><strong>Ramp modeling</strong></td><td>On-ramp merge behavior using VTransport</td></tr>
        <tr><td><strong>HPC calibration pipeline</strong></td><td>SLURM array job orchestration for parallel optimizer evaluation on GACRC</td></tr>
        <tr><td><strong>Simulation reporting</strong></td><td>Automated CSV/TXT export of per-sensor, per-lane validation metrics</td></tr>
      </tbody>
    </table>
  </section>

  <section class="publications-section">
    <h2>8. Publications</h2>
    <h3>Submitted</h3>
    <ol>
      <li><strong>Bishi, K.R.</strong>, Bowman, J., Miller, J.A. (2026). "Beyond Corridor Averages: Lane-Level Validation of Microscopic Freeway Simulation with Data-Driven Arrivals." <em>Annual Modeling and Simulation Conference (ANNSIM)</em>. [Under Review]</li>
    </ol>
    <h3>In Preparation</h3>
    <ol start="2">
      <li><strong>Bishi, K.R.</strong>, Miller, J.A. (2026). "Comparative Analysis of Car-Following Models and Optimization Algorithms for Multi-Lane Traffic Simulation Calibration." <em>Winter Simulation Conference (WSC)</em>. [Target: April 2026]</li>
      <li><strong>Bishi, K.R.</strong>, Miller, J.A. (2026). "Evaluating Evacuation Resilience Under Wildfire Disruption: A PeMS-Calibrated Microscopic Simulation of I-10 During the 2025 Palisades Fire." <em>Winter Simulation Conference (WSC) — Simulation for Climate Resilience track</em>. [Proposed]</li>
    </ol>
  </section>

  <section class="examination-areas-section">
    <h2>9. Expected Examination Areas</h2>
    <p>This dissertation spans the following domains — each area below maps directly to a study or methodological component above:</p>
    <ul>
      <li><strong>Discrete-event and time-stepped simulation theory</strong>: foundations of the ScalaTion framework and car-following integration</li>
      <li><strong>Simulation-based optimization</strong>: calibration as black-box optimization over a stochastic simulator</li>
      <li><strong>Stochastic approximation</strong>: SPSA and SPSA with momentum; gradient estimation under noise</li>
      <li><strong>Derivative-free optimization</strong>: Nelder–Mead simplex method; convergence properties</li>
      <li><strong>Metaheuristic optimization</strong>: Genetic Algorithm; population-based search</li>
      <li><strong>Car-following model dynamics</strong>: IDM and Gipps formulations; physical parameter interpretation</li>
      <li><strong>Calibration identifiability</strong>: nonconvex landscape, multiple local optima, parameter sensitivity</li>
      <li><strong>Lane-level validation philosophy</strong>: why corridor aggregation is insufficient; granularity trade-offs</li>
      <li><strong>Resilience metric formulation</strong>: performance-loss-area index; counterfactual scenario design</li>
    </ul>
  </section>

  <div class="research-nav-buttons" style="clear:both; margin-top:2.5rem;">
    <a class="research-nav-btn" href="/">← Back to Homepage</a>
    <a class="research-nav-btn research-nav-btn-accent" href="/demos/">View Simulation Demos →</a>
  </div>

  <div class="page-footer" style="clear:both; text-align:center; padding:1rem 0;">
    <p>© 2026 Korede R. Bishi | University of Georgia</p>
  </div>
</main>
"""

index.md
"""---
layout: homepage
---


<div class="page-header">
  <h1>Korede R. Bishi</h1>
  <p><strong>Ph.D. Student, Computer Science, University of Georgia</strong></p>
  <p><strong>Research Area:</strong> Discrete Event Simulation, Calibration, and Infrastructure Resilience<br><strong>Application Domain:</strong> Microscopic Traffic Simulation</p>
</div>

<main class="homepage-content">
  <section class="about-section">
    <h2>About</h2>
    <p>I am a third-year Ph.D. student in Computer Science at the University of Georgia working in the Modeling, Simulation &amp; Analytics Lab (MSAL) under the supervision of <a href="https://openreview.net/profile?id=~John_A._Miller1">Dr. John A. Miller</a>.</p>
    <p>My research focuses on discrete-event and time-stepped simulation, simulation-based optimization, and lane-level validation of microscopic traffic models using empirical sensor data from the <a href="https://pems.dot.ca.gov/">California Performance Measurement System (PeMS)</a>.</p>
    <p>I extend the <a href="https://github.com/scalation/scalation_2.0">ScalaTion 2.0</a> simulation framework with lane-level validation, constrained calibration infrastructure, and structural intervention modeling for high-stakes infrastructure scenarios.</p>
  </section>

  <section class="dissertation-theme-section">
    <h2>Dissertation Theme</h2>
    <p>My dissertation investigates the following central question:</p>
    <blockquote>
      <p><strong>Can we construct lane-level microscopic traffic simulations that are sufficiently validated and calibrated to support counterfactual resilience analysis under extreme disruption — where real-world experimentation is infeasible?</strong></p>
    </blockquote>
    <p>This research integrates:</p>
    <ul>
      <li>Discrete-event and time-stepped simulation</li>
      <li>Simulation-based optimization</li>
      <li>Lane-level empirical validation of microscopic traffic models</li>
      <li>Infrastructure resilience modeling</li>
    </ul>
    <p>The objective is to build data-calibrated microscopic traffic digital twins of urban freeway corridors capable of evaluating structural interventions under high-stakes conditions.</p>
  </section>

  <div class="research-banner">
    <div class="research-banner-label">Candidacy Research</div>
    <h3>Discrete Event Simulation, Calibration &amp; Resilience<br><span style="font-size:0.85em; font-weight:600;">Lane-Level Microscopic Traffic Modeling with PeMS Validation</span></h3>
    <p>Three connected studies: (1) identifying which modeling choices materially affect accuracy, (2) constrained calibration of car-following models, and (3) wildfire evacuation resilience analysis under disruption scenarios.</p>
    <a class="research-banner-btn" href="/research/"><strong>Read Full Research Overview</strong></a>
  </div>

</main>
"""