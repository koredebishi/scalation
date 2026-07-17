# Paper 3 — When Calibration Lies: Goodhart's Law and Output-Space Admissibility in Microscopic Traffic Calibration

**Type:** Short empirical paper (cheapest empirical result in the portfolio)
**Source material:** q1.tex (Q1.4), my SPSA calibration experience, the "bounds are not sufficient" argument
**Dissertation chapter:** Motivation section for the Runtime Invariant Monitor chapter
**Depends on:** Feeds Paper 2 (RIM) — this paper is the evidence that the gate must sit on outputs.

---

## Paper Outline

1. **Introduction** — a calibrated model can match a downstream count and still be
   wrong about why that count occurred.
2. **The sharpened claim** — box constraints live in input space; admissibility lives
   in output space. Bounds on every parameter are necessary and not sufficient,
   because admissibility is an emergent property of the parameter combination
   interacting with the scenario.
3. **Three mechanisms** by which a bounded optimizer produces inadmissible trajectories:
   - Joint infeasibility inside the box (every parameter individually plausible,
     the combination inadmissible under dense-merge transients).
   - Compensating errors — the metric sees 5-min detector aggregates, not
     trajectories; tighter-than-physical packing raises throughput and buys RMSE
     (Punzo & Montanino identifiability line).
   - Numerical exploitation — Euler integration artifacts at coarse dt that the
     optimizer learns to use.
4. **Experimental design** — two arms, otherwise identical:
   - Arm A: box-constrained SPSA (literature-range bounds, projection), metric-only
     objective. This is standard best practice, not a strawman.
   - Arm B: same SPSA + trajectory-invariant gate (RIM predicates) rejecting
     inadmissible iterates.
5. **Results** — (one figure, one table, one corridor):
   - % of Arm A iterates and final optima violating trajectory invariants,
     by violation type.
   - The admissibility cost: RMSE(Arm B) − RMSE(Arm A).
   - Worked example of a compensating-error optimum: which error paid for which.
6. **Implications** — Goodhart's Law (Manheim & Garrabrant) realized inside a bounded,
   correctly configured calibration; why agent-paced calibration reproduces this
   failure faster than a human did by hand.

## Working Tree

```
src/main/scala/scalation/simulation/process/
├── model/
│   └── CalibrateCalRoute101.scala    [TOUCH — two-arm harness:
│                                       armA: bounded SPSA, metric-only
│                                       armB: bounded SPSA + invariant gate]
├── monitor/
│   └── InvariantSpec.scala           [REUSE from Paper 2 — same predicates]
└── model/CalRoute101_3.scala         [REUSE — the corridor under calibration]

src/main/scala/scalation/simulation/scripts/
├── goodhart_arms.scala               [NEW — runs both arms, N restarts each,
│                                       logs per-iterate: theta, RMSE,
│                                       violation flags by family]
└── goodhart_tables.py or .scala      [NEW — violation-rate table, RMSE-cost figure]
```

Pre-experiment audit (required before any run): recover the original SPSA
configuration and verify whether and how bounds were enforced (projection vs
penalty). If the historical runs were unbounded, they remain a motivating anecdote
only; all published numbers come from the new bounded runs.

---

## Abstract

Best practice in microscopic traffic calibration bounds every parameter to a
literature-sanctioned range before optimization begins. We show that this is not
enough. Calibrating IDM parameters on a US-101 corridor with box-constrained SPSA
against detector-level RMSE, the optimizer repeatedly returned parameter sets in which
every individual value was plausible while the resulting trajectories were physically
inadmissible: vehicles spaced closer than their own length and decelerations beyond the
configured comfortable limit. The mechanism is not an optimizer wandering out of range.
Box constraints act on input space, while admissibility is a property of output
space — an emergent consequence of parameter combinations interacting with the
scenario — and an aggregate objective that never inspects trajectories will pay for
metric improvements with compensating physical errors. We quantify how often a
correctly bounded calibration lands in inadmissible regions, decompose the violations
by type, and measure the admissibility cost: the RMSE gap between metric-only
calibration and calibration gated by trajectory-level invariant checks. The result is a
concrete instance of Goodhart's Law inside a standard workflow, and a direct argument
that calibration gates must sit on simulation outputs, not parameter inputs.

## Motivation

I saw the underlying problem during manual calibration of the ScalaTion microscopic
traffic simulator, before any agent layer existed. Using SPSA on IDM parameters, the
optimizer returned parameter sets with a good RMSE while the microscopic vehicle
trajectories were still physically inadmissible. My first instinct matched the obvious
objection: if the bounds are set correctly, why should the optimizer wander off? The
answer took me longer to articulate than to observe. The bounds were doing their job —
every parameter stayed plausible. What no bound could express is that the feasible
region is a nonconvex blob inside the box, defined implicitly by the simulator's own
output; you would need to run the simulation to know where it is. And the metric could
not see the violation, because a corridor-level RMSE is computed from detector
aggregates while the damage was at the vehicle level. A model can match the count for
the wrong reason. An optimizer will find the wrong reason if it is cheaper. An
agent-paced calibration loop would find it faster than I did by hand.

## Methodology

1. **Setup.** US-101 corridor (CalRoute101_3), IDM car-following, Shifted-Erlang
   arrivals, PeMS detector aggregates as ground truth. Calibration parameters and
   their literature-range boxes stated in a table (v0, T, s0, a, b at minimum).
2. **Arm A (baseline, standard practice).** SPSA with projection onto the box;
   objective = RMSE over detector flows/speeds. N random restarts.
3. **Arm B (gated).** Identical to Arm A, plus the RIM invariant predicates
   (Paper 2) evaluated on each candidate's trace; inadmissible iterates rejected
   (penalty or resample — both reported).
4. **Measures.** Per-iterate and final-optimum violation rates by invariant family;
   RMSE of accepted optima in both arms; the admissibility cost ΔRMSE; a dissected
   compensating-error example showing which modeling error the inadmissible packing
   was paying for.
5. **Controls.** Same seeds, same restarts, same iteration budget across arms;
   integration time step halved in a robustness check to separate numerical
   exploitation from genuine joint infeasibility.

## Implementation Diagram

```
                    theta box (literature bounds)
                    +---------------------------+
                    |     .  admissible blob    |   <- nonconvex, defined only
                    |    ####  (output-space)   |      by running the simulator
                    |   ######      .           |
                    |    ###   x <- Arm A optimum: inside the BOX,
                    |     .         outside the BLOB                        
                    +---------------------------+

  ARM A (metric-only)                 ARM B (gated)
  ---------------------               -------------------------------
  SPSA (projected)                    SPSA (projected)
    |                                   |
    v                                   v
  simulate theta_k                    simulate theta_k
    |                                   |
    v                                   v
  RMSE vs PeMS  --> accept step       RIM predicates on trace
                                        |-- violate --> reject iterate (log family)
                                        |-- pass ----> RMSE vs PeMS --> accept step

  Reported: violation rate by family | final RMSE A vs B | Delta-RMSE = cost of admissibility
```

## Target Venues

| Venue | Fit | Notes |
|---|---|---|
| **IEEE ITSC** | Best near-term fit | Calibration audience; deadline typically early in the year |
| TRB Annual Meeting | Strong | Deadline traditionally Aug 1 — verify; TRB 2027 (~Aug 1 2026) is a sprint, TRB 2028 comfortable |
| ANNSIM 2027 | Good | Pairs naturally with Paper 2 at the same venue |
| Transportation Research Part C (short paper) | Journal option | If results generalize past one corridor |
