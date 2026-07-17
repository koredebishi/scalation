# Paper 4 — Rollback Across Resolution: Synchronizing Optimistic PDES Mesoscopic Models with Discrete-Time Microscopic Windows (HPDES)

**Type:** Problem-formulation / design-space paper (writing + cost model; full implementation deferred)
**Source material:** q3.tex (Q3.1.1 HPDES section, Q3.3.3), Written-Question-PhD Fujimoto/Mobiliti notes, committee follow-up ("Is PDES needed fundamentally or only for speed?")
**Dissertation chapter:** Hybrid meso-micro chapter, forward-looking section

---

## Paper Outline

1. **Introduction** — city-scale mesoscopic PDES exists (Mobiliti); the sequential
   meso-micro interface exists (Burghout's MiMe: virtual links, common module,
   multi-regime loading, virtual vehicle). Nobody has put them together, and the
   junction contains a genuinely open question.
2. **Background** —
   - Burghout's five consistency requirements and the virtual-link mechanism;
     each micro time step enters the meso event list as an external event.
   - PDES synchronization: local causality constraint, conservative null messages
     with lookahead, optimistic Time Warp with GVT and anti-messages (Fujimoto).
   - Mobiliti: links as LPs, vehicles as timestamped events, asynchronous GVT,
     unexecute closures.
3. **The problem statement** — when an optimistic mesoscopic LP rolls back an event
   that has already been delivered into a discrete-time microscopic window, what
   happens? The micro model has advanced hundreds of ticks; its state (car-following,
   lane changes, MOBIL decisions) is not an event list that can be anti-messaged.
4. **The design space, formalized:**
   - **D1 Conservative boundary.** The micro window's virtual link has a natural
     lookahead: minimum traversal time = link length / v_max (Fujimoto's minimum
     service time bound). Meso releases only causally safe events; null messages
     carry the lower bound. Cost: lookahead-limited parallelism at exactly the
     congested windows where micro detail matters most.
   - **D2 Optimistic with micro checkpointing.** The micro window snapshots state
     every k ticks; a boundary rollback restores the last snapshot ≤ the rollback
     time and replays. Cost model: snapshot size (per-vehicle state × vehicle count)
     × frequency vs expected rollback depth.
   - **D3 Mixed / risk-bounded.** Meso runs optimistic globally; boundary events are
     committed to the micro window only when GVT passes them (the micro window sees
     only committed events — a GVT-gated moving window). Cost: added boundary latency,
     bounded by GVT lag.
5. **Cost model and break-even analysis** — analytical + trace-driven estimates of
   rollback depth at boundaries, using published Mobiliti event statistics and our
   US-101 micro window sizes; when does D2 beat D1, when does D3 dominate both.
6. **Answering the committee** — is PDES fundamental or only speed? Position: for
   city-scale evacuation with microscopic windows at critical merges, sequential
   execution makes iterative scenario studies (Paper 6's matrix) computationally
   infeasible, so PDES is *practically* fundamental to the scientific workflow even
   though it is formally an efficiency mechanism.
7. **Proposed protocol sketch + North-LA feasibility** — the committee's suggested
   scale: meso background for North Los Angeles, micro windows on I-210/SR-134
   merge sections.

## Working Tree

Primarily writing. Optional supporting artifacts:

```
src/main/scala/scalation/simulation/scripts/
└── hpdes_cost_model.scala            [NEW, optional — break-even calculator:
                                        snapshot cost vs lookahead loss vs GVT lag,
                                        parameterized by vehicle count, window size,
                                        boundary event rate]
docs/phd-papers/assets/
└── hpdes-boundary-figures/           [NEW — diagrams for D1/D2/D3]
```

No engine changes in this paper. The implementation paper comes after Paper 5
(links-as-agents) provides the link-LP substrate in ScalaTion.

---

## Abstract

Hybrid meso-micro traffic simulation solves the resolution problem: a mesoscopic
network gives city-scale coverage while microscopic windows resolve the merges,
weaves, and signals where vehicle interaction determines capacity. Parallel
discrete-event simulation solves the scale problem: city-scale mesoscopic models such
as Mobiliti execute billions of link events across processors under Time Warp. The two
solutions have not been combined, and their junction is not an engineering detail.
Burghout's sequential interface delivers each microscopic time step into the
mesoscopic event list as an external event; under optimistic execution, that event can
be rolled back after the microscopic window has already consumed it and advanced
hundreds of ticks of car-following and lane-changing state that no anti-message can
undo. We formalize this boundary as a synchronization problem, and analyze three
protocols: a conservative boundary whose lookahead comes from the virtual link's
minimum traversal time, an optimistic boundary with periodic microscopic state
checkpointing, and a GVT-gated boundary in which microscopic windows consume only
committed events. We give a cost model for each — lookahead-limited parallelism at
congested windows, checkpoint volume against expected rollback depth, and GVT lag —
and identify the traffic-specific structure that makes the problem tractable: boundary
event rates are bounded by physical flow capacity. We close with a protocol sketch
for a North Los Angeles deployment with microscopic windows on I-210 and SR-134.

## Motivation

Reading Fujimoto against Burghout made the gap obvious to me. Fujimoto's null-message
bound is exactly Burghout's virtual link wearing a different hat: if a queue server has
a minimum service time T, the timestamp of any future departure must be at least T
larger than any arrival — and a micro window's virtual link has precisely such a bound,
its length divided by free-flow speed. That is the conservative answer, and it is
almost free. But Mobiliti, the only demonstration of city-scale mesoscopic PDES, is
optimistic: links are actors, vehicles are timestamped events, and causality is
repaired by rollback. Rolling back a mesoscopic link means calling an unexecute
closure. Rolling back a microscopic window means un-living hundreds of ticks of
car-following, MOBIL lane changes, and merge decisions — state with no natural inverse.
Whether the boundary must therefore be conservative, or can be optimistic at
acceptable checkpoint cost, is an open question I have not found answered anywhere in
the hybrid or PDES literature, and it decides whether HPDES is buildable. It also
answers a question my committee asked directly: whether PDES is needed fundamentally
or only for speed.

## Methodology

This is a formulation paper; the contribution is a precise problem statement, a
design-space analysis, and a cost model — not a system.

1. **Formalization.** Model the hybrid as LPs: mesoscopic links as event-driven LPs
   (Mobiliti-style), each microscopic window as one composite LP that internally
   advances by fixed dt but externally exchanges timestamped boundary events
   (vehicle transfer with signature: id, type, path, length, timestamp; blocking /
   unblocking; density feedback; virtual-vehicle info — Burghout's four message
   types). State the local causality constraint at the boundary.
2. **Protocol analysis.** For D1/D2/D3: prove the safety condition (no committed
   micro state depends on an uncommitted meso event), derive the parallelism bound
   (D1: lookahead = L/v_max; D3: GVT lag), and derive the overhead expression
   (D2: checkpoint bytes/s = per-vehicle state × window occupancy × frequency;
   expected replay = rollback rate × mean depth × micro tick cost).
3. **Trace-driven estimates.** Instantiate the cost model with published Mobiliti
   rollback statistics and with vehicle counts / boundary flows from our US-101 and
   Eaton corridor models to locate the break-even points.
4. **Traffic-specific structure.** Argue the bound that makes this tractable:
   boundary event rate ≤ physical capacity (~2,200 veh/h/lane), so anti-message
   traffic into a micro window is bounded in a way generic PDES workloads are not.

## Implementation Diagram

```
        PARALLEL MESO BACKGROUND (optimistic, Time Warp)
   proc 0                 proc 1                 proc 2
  +--------+  events  +--------+   events   +--------+
  | link   |--------->| link   |----------->| link   |
  | LPs    |<---------| LPs    |<-----------| LPs    |
  +--------+  anti-   +---+----+            +--------+
              messages    |
                          |  boundary events (vehicle signature,
                          |  block/unblock, density, virtual vehicle)
                          v
              +---------------------------+
              |  VIRTUAL LINK (boundary)  |
              |  lookahead = L / v_max    |
              +------------+--------------+
                           |
                           v
              +---------------------------+
              | MICRO WINDOW (discrete dt)|
              | IDM + MOBIL, lane DLLs    |
              | advances 100s of ticks    |
              | per meso event            |
              +---------------------------+

  THE QUESTION: meso LP rolls back a boundary event already
  consumed by the micro window. Micro state has no anti-message.

  D1 CONSERVATIVE          D2 OPTIMISTIC+CKPT        D3 GVT-GATED
  release only safe        checkpoint micro every    micro consumes only
  events; null msgs        k ticks; restore+replay   events with ts < GVT
  carry L/v_max bound      on boundary rollback      (committed only)
  cost: parallelism        cost: snapshot volume     cost: boundary latency
  stalls at congested      x rollback depth          = GVT lag
  windows
```

## Target Venues

| Venue | Fit | Notes |
|---|---|---|
| **ACM SIGSIM-PADS 2027** | Best fit — the PDES venue | Deadline typically ~Jan 2027; formulation papers with cost models are accepted here |
| WSC 2027 (advanced methods track) | Strong | Broader simulation audience |
| ACM TOMACS | Journal target | For the eventual implementation follow-up |
