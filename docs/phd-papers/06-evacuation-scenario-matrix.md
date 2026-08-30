# Paper 6 — Which Assumptions Change the Recommendation? A Scenario-Matrix Study of Wildfire Evacuation and Contraflow on I-210/SR-134

**Type:** Domain study (Study 2 retargeted after the WSC 2026 miss)
**Source material:** q2.tex (Q2.1, Q2.3), Zhao & Wong Berkeley study, Pel behavioral decomposition, Ronchi uncertainty framing
**Dissertation chapter:** Evacuation & contraflow study (Study 2)

---

## Paper Outline

1. **Introduction** — the Eaton/Palisades-fire setting; the question is not whether a
   simulator can reproduce a known reduction, but whether the benefit survives later
   departures, lower compliance, higher household vehicle demand, and faster fire
   spread.
2. **Behavioral assumptions as visible scenario inputs** — Pel's decomposition
   (participation + departure timing → dynamic demand; destination → trip
   distribution; route + en-route switching → assignment). Each assumption stays a
   scenario input with a literature-grounded range, never a calibration constant.
   No panic switch: panic flight is rare (Quarantelli); if stress enters, it enters
   through bounded driver parameters (reaction time, headway, deceleration limits)
   the engine and monitor can interpret.
3. **The scenario matrix** — Y = f(v_f, θ_T, r, u, φ, h): fire spread, departure
   timing, route-choice regime, contraflow plan, phasing policy, household vehicle
   demand. Minimal publishable slice: fix r (hybrid route choice) and φ (no phasing);
   sweep v_f × θ_T × u = 3 × 3 × 3 = 27 cells × replications.
4. **Corridor model** — I-210 WB + SR-134 WB dual corridor (EatonFireModel), PeMS
   lane counts and demand, fire-day demand anchor, fire spread as time-varying link
   availability, contraflow as capacity/direction change.
5. **Outcomes** — clearance time, exposed vehicles, queue duration, bottleneck
   location; repeated runs and uncertainty ranges per cell (Ronchi: one run is not
   enough, and the user must not silently pick a representative curve).
6. **Results as mechanism statements** — the deliverable is which factor moves the
   recommendation: does contraflow still reduce exposure when departures are late?
   does rerouting reduce exposure or relocate the bottleneck? Zhao & Wong's 53%/73%
   Berkeley reductions used as comparison shape, never transferred as numbers.
7. **Limitations** — behavioral inputs remain assumptions with ranges; what a survey
   instrument would have to measure to narrow them.

## Working Tree

```
src/main/scala/scalation/simulation/process/model/
├── EatonFireModel.scala              [TOUCH — scenario-input plumbing:
│                                       fire-spread schedule -> link availability,
│                                       contraflow plan -> capacity/direction,
│                                       departure-curve parameter theta_T]
├── PeMSDemand.scala (or equivalent)  [TOUCH — wire I210_WB_FireDay_Anchor()
│                                       (currently a known-missing item)]
└── scenario/
    └── EvacScenarioRunner.scala      [NEW — 27-cell matrix x N replications,
                                        per-cell outcome logging]

src/main/scala/scalation/simulation/process/
└── (merge gap acceptance)            [FIX REQUIRED — known major issue:
                                        both models unconditionally insert at merge.
                                        Under evacuation demand this distorts every
                                        outcome. Fix via the downstream-signal
                                        release throttle from Paper 5, or a minimal
                                        safe-gap wait]

Known data issue: SR-134 OR CSV has zero flow (sensors not reporting) — document
and route around; do not silently impute.
```

---

## Abstract

Evacuation simulations rest on behavioral assumptions — compliance, departure timing,
en-route rerouting, household vehicle demand — that standard detector data do not
observe. Detectors record flows, speeds, and counts; they do not record whether a
household believed a warning or why a departure was late. We therefore refuse to bury
these behaviors inside calibration constants. Each one enters our study as an explicit
scenario input with a literature-grounded range, and the study itself is a scenario
matrix over fire spread speed, departure timing, and contraflow extent on a dual-
corridor microscopic model of I-210 and SR-134 westbound, the freeway spine of the
Eaton fire area. Outcomes are clearance time, exposed vehicles, queue duration, and
bottleneck location, reported with uncertainty ranges over repeated stochastic runs.
The deliverable is not a calibrated clearance time. It is a set of mechanism
statements: under which departure-timing and compliance ranges contraflow still
reduces exposed vehicles, and when dynamic rerouting genuinely reduces exposure versus
relocating congestion to a secondary bottleneck. Prior work reports large contraflow
benefits — 53 to 73 percent reductions in exposed vehicles in the Berkeley case — and
we treat those as shapes to interrogate, not numbers to transfer: the question is
whether such a benefit survives when the assumptions it rests on move within their
plausible ranges.

## Motivation

Pel, Bliemer, and Hoogendoorn make the framing explicit: the success of an evacuation
strongly depends on warning time, response time, information dissemination, evacuation
routes, and dynamic control measures — none of which detector data observe directly.
Zhao and Wong show what survey data can and cannot do: their post-disaster surveys
ground household vehicle use (41–45 percent of evacuees used two vehicles; 9–17
percent used three or more), but the model still has to choose fire spread, destination
rules, rerouting access, and contraflow design. And their own limitation section says
the quiet part: simulations are not perfect representatives of real-life behavior, and
behavioral variables are currently created via assumptions, expert opinions, and
hypothesized distributions. My own microscopic work showed me the same problem at a
different scale — an aggregate result can look acceptable while the vehicle-level
mechanism is wrong, and in evacuation the behavioral part is even less observed. So
the honest product is not one polished evacuation trace, which would be too easy to
overfit. It is knowing which assumptions actually move the decision.

## Methodology

1. **Corridor and data.** I-210 WB + SR-134 WB dual corridor (EatonFireModel):
   PeMS lane counts, ramp joins, and demand; fire-day demand anchor wired in;
   IDM car-following with MOBIL lane changing (engine-level); Shifted-Erlang
   arrivals. The SR-134 on-ramp data gap is documented, not imputed.
2. **Scenario inputs, with ranges and sources stated in one table.** Fire spread
   {slow, medium, fast} as a time-varying link-availability schedule; departure
   timing θ_T {early, middle, late} as response-curve shift/compression; contraflow
   u {none, partial, full/extended} as capacity and direction changes with crossover
   locations stated (Dixit: crossover design changes the evaluation). Route choice
   fixed at the hybrid regime (pre-trip route + en-route switching with a visible
   improvement threshold); phasing off; household vehicle demand at the
   survey-grounded baseline — all three held factors varied later in the full
   dissertation matrix.
3. **Execution.** 27 cells × N seeds (N set by the variance of clearance time to a
   stated confidence half-width). Merge gap-acceptance fix lands before any
   production run, since unconditional insertion distorts merge outcomes under
   evacuation demand.
4. **Outcomes and reporting.** Clearance time, exposed vehicles (fire front vs
   vehicle positions over time), queue duration, bottleneck location; per-cell
   distributions, not single traces (Ronchi). Main-effect and interaction reporting
   over the three factors; every trace passes the Paper 2 invariant checks before it
   counts.
5. **Knowledge claims.** Stated as conditions, not points: e.g., whether extended
   contraflow helps only when enough drivers can reach and use the reversed links,
   and whether rerouting reduces exposure or shifts it — with the assumption ranges
   under which each statement holds.

## Implementation Diagram

```
  SCENARIO INPUTS (explicit, ranged, sourced)        HELD FIXED (this slice)
  fire spread v_f : slow | medium | fast             route regime r = hybrid
  departure  th_T : early | middle | late            phasing    phi = none
  contraflow  u   : none | partial | full            veh demand h  = survey base
        |
        v
  +--------------------------------------------------+
  | EvacScenarioRunner: 27 cells x N seeds            |
  |                                                   |
  |  cell(v_f, th_T, u)                               |
  |    fire schedule --> link availability(t)         |
  |    th_T --> departure events (response curve)     |
  |    u   --> contraflow capacity/direction          |
  |         |                                         |
  |         v                                         |
  |  EatonFireModel (I-210 WB + SR-134 WB)            |
  |  IDM + MOBIL + gap-accepting merge                |
  |         |                                         |
  |         v                                         |
  |  RIM invariant checks (Paper 2) -- gate           |
  |         |                                         |
  |         v                                         |
  |  outcomes: clearance time | exposed vehicles      |
  |            queue duration | bottleneck location   |
  +------------------------+-------------------------+
                           |
                           v
   per-cell distributions -> main effects + interactions
   -> mechanism statements: WHICH assumption moves the recommendation
```

## Target Venues

| Venue | Fit | Notes |
|---|---|---|
| **TRB Annual Meeting** | Best fit | Deadline traditionally Aug 1 — verify; realistic target is TRB 2028 (submit ~Aug 1 2027) |
| *Fire Technology* | Strong | Wildfire evacuation modeling is core scope |
| Transportation Research Part D | Strong | Journal depth for the full matrix |
| ANNSIM 2027 | Good | Faster conference outlet for the 27-cell slice |
| *Natural Hazards Review* | Alternative | Policy-adjacent framing |
