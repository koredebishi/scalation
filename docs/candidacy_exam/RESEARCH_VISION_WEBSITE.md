research.md

"""---
layout: homepage
title: "Research Overview"
permalink: /research/
---
<div class="research-page-header">
  <h1>Korede R. Bishi</h1>
  <p><strong>Ph.D. Student, Computer Science, University of Georgia</strong></p>
  <p><strong>Research Area:</strong> Discrete Event Simulation, Infrastructure Resilience, and Agentic Simulation Systems &nbsp;|&nbsp; <strong>Application Domain:</strong> Microscopic Traffic Simulation</p>
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
    <h2>3. Study 1 — Structural Sensitivity Analysis <em style="font-weight:400; font-size:0.9em;">(ANNSIM 2026, Accepted)</em></h2>
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
    <h2>4. Study 2 — Wildfire Evacuation Resilience &amp; Contraflow Evaluation <em style="font-weight:400; font-size:0.9em;">(WSC 2026, Active Target)</em></h2>
    <p><strong>Title:</strong> "Evaluating Evacuation Resilience Under Wildfire Disruption: A PeMS-Calibrated Microscopic Simulation of I-10 During the 2025 Palisades Fire"</p>

    <h3>Motivation</h3>
    <p>On January 7, 2025, the Palisades Fire triggered mass evacuation along I-10 eastbound in Los Angeles. Severe congestion and smoke degraded corridor performance for hours. A recurring policy question: whether directional lane reallocation (contraflow) would have improved evacuation throughput, cannot be answered through real-world experimentation. Simulation provides the only feasible evaluation method.</p>

    <h3>Approach</h3>
    <ol>
      <li><strong>Baseline calibration:</strong> Reproduce normal-day I-10 traffic dynamics using PeMS data and the validated arrival process methodology from Study 1</li>
      <li><strong>Fire-day reconstruction:</strong> Detect demand surge timing from PeMS, reconstruct the congestion event in simulation</li>
      <li><strong>Smoke-behavior modeling:</strong> Translate smoke exposure into driving behavior degradation (reduced desired speed, increased headway, reduced lane-change aggressiveness)</li>
      <li><strong>Counterfactual scenarios:</strong> Evaluate evacuation performance under multiple capacity configurations: baseline, partial contraflow, full contraflow, and contraflow under smoke</li>
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
      <li>Identification of conditions under which capacity expansion alone is insufficient, requiring behavioral adaptation</li>
    </ul>
    <p><strong>Target:</strong> Winter Simulation Conference 2026 — <em>Simulation for Climate Resilience</em> track</p>
  </section>

  <section class="study3-section" id="study-3-proposed-agentic-simulation-architecture">
    <h2>5. Study 3 — Proposed Agentic Simulation Architecture <em style="font-weight:400; font-size:0.9em;">(Proposed — Long-Term Dissertation Vision)</em></h2>

    <h3>Motivation</h3>
    <p>Studies 1 and 2 establish that high-fidelity, empirically validated microscopic simulation is achievable. The next challenge is scale: manual experiment design limits how much of the scenario space a researcher can explore. This study proposes a unified agentic architecture where AI-driven agents autonomously design, execute, and refine simulation experiments, grounded in the validated digital twin developed in Studies 1 and 2.</p>

    <h3>The Core Idea</h3>
    <p>Rather than a researcher manually specifying each simulation scenario, an LLM-driven agent reasons over a knowledge graph of the road network, generates structured simulation scenarios via a domain-specific language (DSL), validates them before execution, runs them through the ScalaTion engine, and iteratively refines experiments based on results. This enables scientific discovery at a scale and speed impossible through manual experimentation.</p>

    <h3>Proposed Architecture — Four Layers</h3>
    <ul>
      <li><strong>Knowledge Layer:</strong> PeMS sensor data, OpenStreetMap road topology, Neo4j knowledge graph</li>
      <li><strong>Agent Layer:</strong> GraphRAG retrieval provides network context to an LLM agent that autonomously proposes simulation scenarios</li>
      <li><strong>Simulation Layer:</strong> Scenarios are expressed as a Scala DSL, validated before execution, then run through the ScalaTion microscopic simulator</li>
      <li><strong>Evaluation Layer:</strong> Runtime invariant checks protect simulation correctness; metrics feed back to the agent for iterative refinement</li>
    </ul>

    <h3>Expected Contributions</h3>
    <ul>
      <li>Agentic experimentation loop for microscopic traffic simulation</li>
      <li>DSL-based scenario generation that separates LLM reasoning from simulation execution</li>
      <li>Runtime invariant framework ensuring physical validity of agent-generated scenarios</li>
      <li>Scalable exploration of evacuation and infrastructure resilience scenarios</li>
    </ul>
    <p><strong>Status:</strong> Proposed. This architecture is the long-term dissertation vision and the subject of the candidacy proposal. See the live architecture diagram above.</p>
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
    <h3>Accepted</h3>
    <ol>
      <li><strong>Bishi, K.R.</strong>, Bowman, J., Miller, J.A. (2026). "Beyond Corridor Averages: Lane-Level Validation of Microscopic Freeway Simulation with Data-Driven Arrivals." <em>Annual Modeling and Simulation Conference (ANNSIM)</em>. [Accepted]</li>
    </ol>
    <h3>In Preparation</h3>
    <ol start="2">
      <li><strong>Bishi, K.R.</strong>, Miller, J.A. (2026). "Evaluating Evacuation Resilience Under Wildfire Disruption: A PeMS-Calibrated Microscopic Simulation of I-10 During the 2025 Palisades Fire." <em>Winter Simulation Conference (WSC) — Simulation for Climate Resilience track</em>. [Target: April 2026]</li>
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
  <p><strong>Research Area:</strong> Discrete Event Simulation, Empirical Validation, and Agentic Simulation Systems<br><strong>Application Domain:</strong> Microscopic Traffic Simulation</p>
</div>

<main class="homepage-content">
  <section class="about-section">
    <h2>About</h2>
    <p>I am a third-year Ph.D. student in Computer Science at the University of Georgia working in the Modeling, Simulation &amp; Analytics Lab (MSAL) under the supervision of <a href="https://openreview.net/profile?id=~John_A._Miller1">Dr. John A. Miller</a>.</p>
    <p>I build validated digital twins of real transportation systems — and I am proposing an agentic architecture where LLM-driven agents autonomously design and evaluate simulation experiments. My work sits at the intersection of discrete-event simulation, empirical validation, and agentic AI.</p>
    <p>I extend the <a href="https://github.com/scalation/scalation_2.0">ScalaTion 2.0</a> simulation framework with lane-level validation, constrained calibration infrastructure, and structural intervention modeling for high-stakes infrastructure scenarios — validated against empirical sensor data from the <a href="https://pems.dot.ca.gov/">California Performance Measurement System (PeMS)</a>.</p>
  </section>

  <section class="dissertation-theme-section">
    <h2>Dissertation Theme</h2>
    <p>My dissertation investigates the following central question:</p>
    <blockquote>
      <p><strong>Can we build empirically validated microscopic simulation models that are trustworthy enough for counterfactual infrastructure policy evaluation under extreme disruption, and can agentic AI systems autonomously design, execute, and refine such experiments at scale?</strong></p>
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
    <h3>Discrete Event Simulation, Empirical Validation &amp; Agentic AI<br><span style="font-size:0.85em; font-weight:600;">Lane-Level Microscopic Traffic Modeling with PeMS Validation</span></h3>
    <p>Three connected studies: (1) identifying which modeling choices materially affect simulation accuracy, (2) wildfire evacuation resilience and contraflow evaluation on I-10, and (3) a proposed agentic architecture for autonomous simulation-based experimentation.</p>
    <a class="research-banner-btn" href="/research/"><strong>Read Full Research Overview</strong></a>
  </div>

</main>
"""