# Paper 2 — RIM: Runtime Invariant Monitoring for Agent-Generated Discrete-Event Traffic Simulation

**Type:** Implementation + evaluation paper (core dissertation novelty)
**Source material:** q1.tex (Q1.2, Q1.3), committee follow-up on quarantine specification
**Dissertation chapter:** The Runtime Invariant Monitor
**Depends on:** Paper 1 stakes the framing; this paper cites it and carries the implementation burden.

---

## Paper Outline

1. **Introduction** — the invalid-versus-emergent distinction is first a physical
   admissibility question, not a classification problem.
2. **Related work** — runtime verification (Zhou et al. probabilistic RV), AgentGuard
   (AMDP + probabilistic model checking), AIVV (mathematical gate + adjudication
   council), classical V&V trace checking (Balci, Law).
3. **The five invariant families** — flow conservation, collision exclusion, kinematic
   bounds, lane integrity, event ordering — each stated as a checkable predicate over
   a ScalaTion trace.
4. **The three-way verdict** — reject / quarantine / escalate, and why a binary
   pass/fail monitor becomes a confirmation engine that suppresses physically valid
   but surprising traffic behavior.
5. **The quarantine test** (the committee's question, answered) — how many replications
   distinguish artifact from emergence, and what statistical test makes the call:
   persistence of the flagged behavior across n random seeds, time-step refinement,
   and boundary-condition perturbation, with an explicit stability criterion.
6. **Implementation in ScalaTion** — monitor as an observer over the engine's
   time-ordered agenda and the per-lane vehicle DLLs; where each check hooks in.
7. **Case studies (both verdict branches, from real experience):**
   - Reject: Poisson arrivals placing two vehicles on the same discrete tick →
     artificial shockwave (the artifact that took weeks to attribute by hand).
   - Escalate: lane-level results diverging from corridor-level results → not invalid;
     it became the finding of our first validation paper.
8. **Results** — detection latency, false-quarantine rate under known-clean scenarios,
   overhead per simulated vehicle-second.
9. **Limitations & human boundary** — the monitor never declares a discovery.

## Working Tree

```
src/main/scala/scalation/simulation/process/
├── monitor/                          [NEW package]
│   ├── RuntimeInvariantMonitor.scala [NEW — observer over trace events]
│   ├── InvariantSpec.scala           [NEW — the 5 predicate families]
│   ├── Verdict.scala                 [NEW — Reject | Quarantine | Escalate enum]
│   └── QuarantineRunner.scala        [NEW — n-seed replication + stability test]
├── Vehicle.scala                     [TOUCH — expose position/speed/accel snapshot]
├── VTransport.scala                  [TOUCH — emit check events from move();
│                                       vList (DLL) is the flow-conservation source]
├── Route.scala                       [TOUCH — lane-integrity hook in changeLane]
└── model/
    ├── CalRoute101_3.scala           [TOUCH — monitor wiring, demo scenarios]
    └── EatonFireModel.scala          [TOUCH — second corridor for generality]

src/main/scala/scalation/simulation/scripts/
└── rim_experiments.scala             [NEW — seed sweeps, latency/overhead tables]
```

Constraints: engine-level, not model-level (same principle as MOBIL placement —
one place, one decision, one execution). Do not touch Dgraph.scala, Animator.scala,
AnimateCommand.scala.

---

## Abstract

An anomaly detector can flag an unusual simulation trace, but it cannot decide whether
the trace is a real traffic finding or a modeling error. When an LLM agent generates
discrete-event simulation experiments autonomously, that decision can no longer wait
for a researcher to walk the event agenda by hand. We present RIM, a runtime invariant
monitor for microscopic traffic simulation that turns the invalid-versus-emergent
distinction into a three-way verdict. A trace that violates a hard physical
invariant — flow conservation, collision exclusion, kinematic bounds, lane integrity,
or event ordering — is rejected regardless of how interesting it looks. A trace whose
invariants hold but whose behavior disappears under different random seeds, smaller
time steps, or boundary-condition changes is quarantined as a probable artifact. A
trace whose invariants hold and whose behavior is stable across those perturbations is
escalated to the researcher, because stop-and-go waves, bottleneck breakdowns, and
merge-induced oscillations can be surprising without being invalid. We give the
quarantine step an explicit statistical criterion, implement the monitor inside the
ScalaTion process-interaction engine, and evaluate it on a US-101 corridor model using
two failure cases we first met the hard way: a same-tick arrival artifact that
manual inspection took weeks to attribute, and a lane-versus-corridor divergence that
turned out to be the scientific result.

## Motivation

Our first lane-level validation paper showed me why both sides of this distinction
matter. We started with the Gipps model, but it could not reproduce the acceleration
profile of a steep stop-and-go regime, so we moved to IDM. The same pattern appeared
with arrivals: a Poisson process allowed two vehicles to enter the engine at the same
discrete schedule time, and the artificial shockwave it produced took weeks of trace
inspection to attribute to the arrival generator rather than the car-following model.
Those were invalid-scenario catches. Later, lane-level results diverged from
corridor-level results — and that mismatch was not invalid; it became the finding.
A monitor that had rejected it as an anomaly would have suppressed the paper. A monitor
that had accepted the shockwave would have laundered an artifact into a result. The
committee asked the right question: how many replications distinguish artifact from
emergence, and what statistical test makes that call. This paper is the answer.

## Methodology

1. **Predicate formalization.** Each invariant family is stated over the executed
   trace: collision exclusion as x_j(t) − x_i(t) − ℓ_i > 0 for every leader j and
   follower i; kinematic bounds as 0 ≤ v_i(t) ≤ v_max and −b_max ≤ a_i(t) ≤ a_max;
   flow conservation as vehicles-in = vehicles-out + vehicles-remaining per segment
   (source of truth: the per-lane vehicle DLL, `VTransport.vList`); lane integrity as
   no non-adjacent lane jumps and merge order consistent with queuing discipline;
   event ordering as timestamp-ordered activation through the engine agenda.
2. **Verdict procedure.** Hard-invariant failure → reject with a structured reason
   code returned to the proposer. Invariants hold + behavior flagged as anomalous →
   quarantine: rerun with n seeds, halved time step, perturbed boundary conditions;
   the behavior is promoted only if it persists (stability criterion and n chosen by
   power analysis on the flagged statistic, reported explicitly). Persistent behavior
   → escalate to the researcher with the full evidence bundle.
3. **Evaluation.** (a) Seeded-fault detection: known artifacts (same-tick arrivals,
   sub-length spacing injections) measured for detection latency and miss rate.
   (b) Known-clean scenarios measured for false-quarantine rate. (c) Overhead measured
   as wall-clock cost per simulated vehicle-second with the monitor on and off.
   (d) The two historical case studies replayed end-to-end.

## Implementation Diagram

```
  Scenario (DSL / model config)
          |
          v
  +---------------------+     structured reason code
  | FORMAL VALIDATOR    |----------------------------> back to proposer
  | (pre-run: schema,   |   (reject before run)
  |  jam density, ticks)|
  +---------+-----------+
            | admissible scenario
            v
  +---------------------+
  | ScalaTion engine    |
  | agenda -> events    |
  | VTransport.move()   |
  +---------+-----------+
            | trace stream (positions, speeds, lane ops, agenda order)
            v
  +-----------------------------------------------+
  | RUNTIME INVARIANT MONITOR                     |
  |  [1] flow conservation   (vList counts)       |
  |  [2] collision exclusion (gap > 0)            |
  |  [3] kinematic bounds    (v, a in range)      |
  |  [4] lane integrity      (adjacency, merges)  |
  |  [5] event ordering      (timestamps)         |
  +---+-------------------+-------------------+---+
      |                   |                   |
   hard fail         anomaly, invariants   invariants hold,
      |              hold                  behavior stable
      v                   v                across seeds/dt/BCs
  REJECT            QUARANTINE                 |
  reason code       n-seed replication         v
  to agent          + stability test       ESCALATE
                    |         |            to researcher
                 vanishes   persists       (discovery / artifact
                    |         |             / defect — human call)
                    v         +----------------^
                 artifact,
                 logged
```

## Target Venues

| Venue | Fit | Notes |
|---|---|---|
| **ACM SIGSIM-PADS 2027** | Best fit | Principles-of-simulation audience; deadline typically ~Jan 2027 |
| ANNSIM 2027 | Strong | Natural successor to the ANNSIM 2026 submission |
| WSC 2027 | Strong | Analysis-methodology track; deadline ~April 2027 |
| ACM TOMACS | Journal extension | After the conference version |
