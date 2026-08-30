# Paper 5 — Links as Agents: Decentralized Flow Sensing and Spillback Signaling in a Process-Interaction DES Engine

**Type:** Engine implementation + experiments paper
**Source material:** The CLAUDE.md concept (1, 2, 3), Mobiliti link-agent state (dual linked-list window), existing variable-lane DLL + MOBIL machinery
**Dissertation chapter:** Engine architecture chapter; substrate for the HPDES implementation (Paper 4 follow-up)

---

## Paper Outline

1. **Introduction** — the core idea: if links (roads) are agents that send and
   schedule vehicle events between each other, then because these nodes are linked:
   1. the flow rate inside each link is knowable — length of the link's vehicle
      list over a time window — plus a spillback rate as the congestion effect;
   2. the connecting downstream rate (congestion signal) can be sensed;
   3. one can maintain a traffic signal — dynamic routing at the lane level.
2. **Related work** — Mobiliti's link-agent state (two linked lists of arrival times
   maintaining a window-W invariant), CTM's downstream-supply term as the macroscopic
   ancestor of the spillback signal, DTA literature for routing on sensed state.
3. **Link state in ScalaTion** — `VTransport.vList` (the per-lane vehicle DLL,
   single source of truth since the vdeque removal) as the arrival/occupancy record;
   O(1) `vCount`; windowed flow q = |arrivals in W| / W; density k = vCount / L;
   speed from q = k·v.
4. **The downstream congestion signal** — each link exposes (q, k, spillback flag)
   to its upstream neighbors through the graph; upstream links throttle release or
   re-weight lane assignment when the signal crosses threshold. Relation to the
   known merge problems (gap acceptance, outer-lane crowding after ramp merge).
5. **Lane-level dynamic routing** — junction-level decision: vehicles check severity
   ahead (the sensed signal) and choose lane/branch; interaction with MOBIL
   (strategic incentive term fed by the downstream signal, versus the current purely
   local incentive).
6. **Experiments (US-101 + Eaton corridors):**
   - Accuracy: windowed link flow vs detector-style ground truth at varying W.
   - Responsiveness: spillback signal lead time ahead of queue arrival upstream.
   - Effect: lane-level routing on/off — throughput, merge queue length,
     outer-lane crowding relaxation time.
7. **Why this is the HPDES substrate** — a link that owns its state and communicates
   by events is already a logical process; this paper builds the LP discipline
   sequentially so the parallel version (Paper 4 follow-up) changes the scheduler,
   not the model.

## Working Tree

```
src/main/scala/scalation/simulation/process/
├── LinkState.scala                   [NEW — windowed flow, density, spillback flag;
│                                       Mobiliti-style window invariant over
│                                       arrival timestamps]
├── VTransport.scala                  [TOUCH — record arrival time on addToAlist;
│                                       expose vList-derived (q, k) via LinkState;
│                                       consult downstream signal before release]
├── Pathway.scala                     [TOUCH — parentRoute already exists; add
│                                       downstream LinkState reference]
├── Route.scala                       [TOUCH — aggregate lane-level LinkStates;
│                                       lane-level routing hook at junctions]
├── Dynamics.scala                    [TOUCH — optional: MOBIL strategic incentive
│                                       term fed by downstream congestion signal]
└── model/
    ├── CalRoute101_3.scala           [TOUCH — experiments, signal thresholds]
    └── EatonFireModel.scala          [TOUCH — second corridor]

src/main/scala/scalation/simulation/scripts/
└── linkstate_experiments.scala       [NEW — W sweep, lead-time measurement,
                                        routing on/off comparison]
```

Design constraints carried over from the engine work: one place, one decision, one
execution (signal logic lives in the engine, models get it for free, same as MOBIL);
do not touch Dgraph.scala / Animator.scala / AnimateCommand.scala; `vList` DLL remains
the single source of truth — no parallel bookkeeping structures.

---

## Abstract

In a process-interaction discrete-event traffic simulation, a road link already owns
everything needed to know its own traffic state: vehicles arrive as events, reside on
the link's vehicle list, and depart as scheduled events to the downstream link. We make
that ownership explicit. Each link maintains a windowed arrival record over its
existing vehicle list, from which it computes its own flow rate, density, and a
spillback flag — no detectors, no global observer, no extra bookkeeping structure.
Because links are connected, each link can also sense its downstream neighbor's state
and act on it: throttling release into a congested link, and informing lane-level
routing decisions at junctions so that vehicles respond to conditions ahead rather
than only to their immediate leader. The mechanism mirrors the link-agent state of
city-scale parallel simulators, where links are actors and vehicles are timestamped
events, but we implement it inside a sequential microscopic engine where car-following
and lane-changing detail is preserved. On US-101 and I-210/SR-134 corridor models we
measure the accuracy of windowed link flow against detector-style ground truth, the
lead time by which the spillback signal anticipates upstream queue arrival, and the
effect of signal-informed lane-level routing on merge throughput and outer-lane
crowding. The result is a decentralized state-estimation layer that costs O(1) per
vehicle event, and a link-as-logical-process discipline that a parallel hybrid engine
can adopt without changing the model.

## Motivation

This concept has been sitting at the top of my project notes waiting for exactly this
treatment: if agents are connected links that send and schedule vehicle events between
each other, then the flow rate inside each link is knowable — the length of the link's
vehicle list over a window — the downstream rate can be sensed as a congestion signal,
and one can maintain dynamic routing at the lane level. Reading Mobiliti confirmed the
shape of it: their link agents keep two linked lists of vehicle arrival times with a
window invariant, so that the list length *is* the windowed flow. Our engine is already
most of the way there. Since the vdeque removal, `VTransport.vList` is the single
source of truth for a lane's vehicles with O(1) counting, and MOBIL already made
lane-change a link-level engine decision. What is missing is the sensing and signaling
layer — and two of our known issues are exactly the problems it addresses: unconditional
merge insertion with no gap acceptance (a link releasing blind into a congested
neighbor) and outer-lane crowding after ramp merges (vehicles with no knowledge of
conditions ahead). The same layer is the substrate HPDES needs, because a link that
owns its state and talks by events is already a logical process.

## Methodology

1. **Windowed flow.** On each `addToAlist`, record the arrival timestamp in the
   link's window list; migrate entries older than t − W (Mobiliti's invariant).
   Then q(t) = |window| / W, k(t) = vCount / L, and the spillback flag fires when
   k crosses a threshold fraction of jam density or when the queue tail reaches the
   link entrance.
2. **Signal propagation.** Downstream LinkState is readable by the immediate
   upstream link through the existing graph references (Pathway/Route). Two uses:
   release throttling at merges (address the no-gap-acceptance issue as a sensed
   condition, not a hardcoded wait) and junction-level lane choice.
3. **Validation of the sensor.** Compare windowed q against ground-truth counts on
   the same segments across W ∈ {10s, 30s, 60s, 300s}; report bias/variance
   trade-off in W (small W: noisy; large W: laggy).
4. **Lead-time experiment.** Induce a downstream bottleneck; measure time between
   spillback-flag assertion and queue arrival at the upstream link entrance, across
   demand levels.
5. **Routing experiment.** Lane-level routing on/off on US-101 and Eaton: merge
   throughput, queue length distributions, time for ramp vehicles to relax off the
   outer lane (interaction with the MOBIL 3s cooldown reported explicitly).
6. **Replication.** All comparisons across seeds with the same replication and
   reporting discipline as Paper 2.

## Implementation Diagram

```
   upstream link                     downstream link
  +-------------------------+      +-------------------------+
  | VTransport              |      | VTransport              |
  |  vList (DLL) ========== |      |  vList (DLL) ====       |
  |                         |      |                         |
  | LinkState               |      | LinkState               |
  |  window: [t1..tn] <= W  |      |  q, k, SPILLBACK=true   |
  |  q = |window| / W       |      +-----------+-------------+
  |  k = vCount / L         |                  |
  +-----------+-------------+                  |
              |         ^                      |
   release    |         |   congestion signal  |
   decision <-+         +----------------------+
   (throttle if downstream spillback)
              |
              v
  junction: lane-level routing
   vehicle checks severity ahead (sensed q,k on branches)
   -> lane / branch choice   -> optional MOBIL strategic bias

  vehicle events -->  [link agent] --schedule--> [link agent] --> ...
  (arrival at t0, service ~ L/v(k), departure event at t0 + L/v(k))
```

## Target Venues

| Venue | Fit | Notes |
|---|---|---|
| **ANNSIM 2027** | Best fit | Engine + experiments profile matches; deadline typically ~Jan 2027 |
| WSC 2027 | Strong | Modeling-methodology track |
| IEEE ITSC | Good | If framed toward the routing/control results |
| SIGSIM-PADS | Possible | If framed as the LP-discipline substrate for HPDES |
