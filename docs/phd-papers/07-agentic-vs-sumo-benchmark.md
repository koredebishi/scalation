# Paper 7 — Compressing the Apprenticeship: Benchmarking an Agentic Scenario-Construction Workflow Against a Standard SUMO Workflow

**Type:** Benchmark / evaluation paper (largest lift; committee-mandated)
**Source material:** q2.tex (Q2.2), committee follow-up ("Since SUMO can already do these tasks, the framework seems to contribute efficiency rather than new functions. This should be validated by benchmarking it against a standard SUMO workflow.")
**Dissertation chapter:** AutoTrafficSim evaluation chapter — this produces the headline number
**Depends on:** Papers 1–3 (framing, monitor, gate evidence); do this last.

---

## Paper Outline

1. **Introduction** — the committee's challenge, taken at face value: if the
   contribution is efficiency, then measure it honestly against the strongest
   standard baseline.
2. **What is actually being benchmarked** — not simulator vs simulator. SUMO can run
   any scenario handed to it; the slow part of scientific simulation work is
   building the scenario: model selection, parameter choice, trace inspection,
   revision. Today that is an apprenticeship measured in years before a student can
   produce a microscopic model defensible in review. The benchmark measures
   scenario-construction workflows, with the engine held as a substitution point.
3. **The two arms:**
   - **Arm S (standard):** a competent human following the documented SUMO workflow
     (netconvert/OSM import, demand from counts, car-following choice, calibration
     via established tools, manual trace inspection).
   - **Arm A (agentic):** the minimal AutoTrafficSim loop — agent proposes scenario
     in the DSL, Formal Validator gates pre-run, RIM gates the trace (Paper 2),
     structured reason codes drive revision, human interprets escalations.
4. **Tasks** — three graded scenario-construction tasks on held-out corridors with
   ground truth (PeMS): (T1) freeway corridor reproducing observed flow/speed within
   stated tolerance and passing all invariants; (T2) same corridor with an on-ramp
   bottleneck reproducing the observed breakdown location; (T3) a counterfactual
   (lane closure) requiring a defensible prediction, judged blind by domain raters.
5. **Measures:**
   - Time-to-defensible-scenario (wall clock and human-attention minutes — separated).
   - Error catches: seeded-defect detection rate (inadmissible arrivals, wrong lane
     count, unit errors) caught by each workflow before "submission."
   - Quality: fit metrics + invariant pass rate + blind expert rubric score.
   - Cost: compute + token spend for Arm A, reported plainly.
6. **Results and honest accounting** — where the agentic arm wins (throughput of
   checked revisions, defect catches at machine speed), where it does not (novel
   judgment calls, escalation interpretation still human-paced).
7. **Threats to validity** — human-skill confound (multiple operators per arm),
   task selection bias, the engine difference (ScalaTion vs SUMO both run in Arm A
   for the substitution-point claim), and Goodhart risk in the benchmark itself
   (the rubric is multi-metric by construction — Paper 3's lesson applied to our
   own evaluation).

## Working Tree

```
src/main/scala/scalation/simulation/agentic/     [NEW package — minimal loop]
├── ScenarioDSL.scala                 [NEW — schema-checked scenario language;
│                                       the agent writes DSL, never engine code]
├── FormalValidator.scala             [NEW — pre-run gate: schema, jam density,
│                                       same-tick arrivals, topology mismatches]
├── AgentLoop.scala                   [NEW — propose -> validate -> run -> RIM ->
│                                       reason code -> revise; explicit stopping
│                                       criterion and iteration budget (the
│                                       committee's termination-logic question)]
├── ReasonCode.scala                  [NEW — structured failure feedback]
└── Lineage.scala                     [NEW — hypothesis/revision log with pruning
                                        policy (the committee's state-growth and
                                        audit-trail questions, answered in code)]

src/main/scala/scalation/simulation/process/monitor/
└── (RIM, InvariantSpec)              [REUSE from Paper 2 — unchanged]

benchmark/
├── tasks/T1_corridor/  T2_bottleneck/  T3_counterfactual/
│                                     [NEW — task specs, ground truth, tolerances]
├── sumo-baseline/                    [NEW — documented Arm S workflow configs,
│                                       operator logs, seeded-defect variants]
└── scoring/rubric.md + scorer        [NEW — blind rubric, multi-metric scoring]
```

Note: this working tree is deliberately the *minimal* agent loop — DSL, two gates,
reason codes, lineage with pruning. It is also the first real implementation of the
AutoTrafficSim architecture, so this paper doubles as the architecture's existence
proof for the dissertation defense.

---

## Abstract

Mature microscopic traffic simulators can reproduce observed traffic once a competent
modeler has built the scenario: chosen the car-following law, the arrival process, and
the lane configuration, calibrated the parameters, and inspected the traces for the
failures that experience teaches one to spot. That construction work — not simulator
runtime — is the expensive part of simulation science, and today it is transmitted by
apprenticeship. We benchmark an agentic scenario-construction workflow against the
standard SUMO workflow on three graded tasks with held-out ground truth: reproducing a
freeway corridor, reproducing an on-ramp bottleneck breakdown, and producing a
defensible lane-closure counterfactual. The agentic arm is deliberately minimal: an
LLM agent writes scenarios in a schema-checked DSL — never executable code — a formal
validator rejects scenarios that are wrong by construction, a runtime invariant
monitor rejects physically inadmissible traces, and structured reason codes bound each
revision. We measure time-to-defensible-scenario with human attention separated from
wall clock, seeded-defect catch rates, blind expert quality ratings, and full compute
cost. We report where the agentic workflow compresses the work, where it does not, and
what the human still decides. The benchmark design applies our own findings to
ourselves: because a single target metric invites gaming, quality is scored against a
multi-metric rubric with invariant compliance as a hard floor.

## Motivation

My committee put the challenge plainly: since SUMO with calibration and optimization
can already reproduce observed data, the framework seems to contribute efficiency
rather than new functions — so validate it by benchmarking against a standard SUMO
workflow. I accept the framing, with one correction that the benchmark itself will
test. SUMO does not shorten the apprenticeship; it assumes the apprenticeship has
already happened. The question is who builds the scenario: who picks the car-following
law and the arrival process, and who recognizes when a parameter setting produces a
physically inadmissible trajectory. Those are the failures that took me years of trace
inspection to learn to spot — the same-tick arrival artifact alone cost weeks. If an
agentic loop with the right gates can catch that class of failure at machine speed and
deliver a defensible scenario in hours instead of an apprenticeship, that efficiency
*is* the contribution, and it deserves a number. If it cannot, the dissertation should
say so with the same clarity.

## Methodology

1. **Task battery.** Three tasks of increasing judgment content (T1 fit, T2
   mechanism localization, T3 counterfactual), each with held-out PeMS ground
   truth, stated tolerances, and three seeded-defect variants (inadmissible arrival
   config, wrong lane count, unit error in demand).
2. **Arm S.** Multiple human operators (to bound the skill confound) follow the
   documented SUMO pipeline; every action logged with timestamps; operators are not
   told which variants contain seeded defects.
3. **Arm A.** The minimal loop above, fixed iteration budget and explicit stopping
   criterion (tolerance met + all invariants pass + no open escalations, or budget
   exhausted — the termination logic stated in advance, per the committee's
   requirement). Escalations go to a human whose attention time is metered.
4. **Substitution-point check.** Arm A runs with ScalaTion as primary engine and
   SUMO behind the same DSL for T1, to separate workflow effect from engine effect.
5. **Scoring.** Blind multi-metric rubric (fit, invariant compliance as hard floor,
   defensibility of stated assumptions) by raters not affiliated with the project;
   defect catch rates scored mechanically; costs reported in both dollars and
   human-minutes.
6. **Analysis.** Per-task comparisons with uncertainty over operators/seeds;
   explicit reporting of Arm A failures and escalation content, not only wins.

## Implementation Diagram

```
        ARM S (standard SUMO)              ARM A (agentic, minimal loop)
  +---------------------------+      +------------------------------------+
  | human operator            |      |  LLM agent                         |
  |  OSM import / netconvert  |      |    writes Scenario DSL only        |
  |  demand from counts       |      |         |                          |
  |  car-following choice     |      |         v                          |
  |  calibrate (est. tools)   |      |  FORMAL VALIDATOR (pre-run gate)   |
  |  manual trace inspection  |      |         | pass                     |
  |  revise by judgment       |      |         v                          |
  +------------+--------------+      |  engine: ScalaTion (SUMO for T1    |
               |                     |          substitution check)       |
               |                     |         |                          |
               |                     |         v                          |
               |                     |  RIM (Paper 2): reject/quarantine/ |
               |                     |       escalate                     |
               |                     |     |reject          |escalate     |
               |                     |     v                v             |
               |                     |  reason code     human (metered    |
               |                     |  -> bounded         attention)     |
               |                     |     revision                       |
               |                     |  lineage log + pruning policy      |
               |                     |  stopping criterion: tol met AND   |
               |                     |  invariants pass AND no open esc.  |
               |                     +---------------+--------------------+
               |                                     |
               v                                     v
        +---------------------------------------------------+
        | SAME TASKS: T1 corridor | T2 bottleneck | T3 c/f   |
        | SAME SCORING: time-to-defensible | defect catches  |
        |   blind rubric | invariant floor | full cost       |
        +---------------------------------------------------+
```

## Target Venues

| Venue | Fit | Notes |
|---|---|---|
| **WSC 2027/2028** | Best fit | Benchmark + methodology profile; the simulation community is the audience that must be convinced |
| ANNSIM | Strong | Committee-visible venue |
| ACM SIGSIM-PADS | Possible | If framed around the workflow-as-system evaluation |
| ACM TOMACS / SIMULATION | Journal extension | Full battery + replication package |
