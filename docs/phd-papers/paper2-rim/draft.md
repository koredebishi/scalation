# RIM: Runtime Invariant Monitoring for Agent-Generated Discrete-Event Traffic Simulation

**Target:** ACM SIGSIM-PADS 2027 (deadline ~Jan 2027) — this is a PADS paper, period · journal extension TOMACS
**Type:** Implementation + evaluation (core dissertation novelty)
**Depends on:** Paper 1 stakes the framing; this paper carries the implementation burden.
**Bib:** `refs.bib` (four entries at the bottom are marked VERIFY — check before citing)
**Status:** ☐ skeleton → ☐ monitor package built → ☐ experiments run → ☐ prose → ☐ LaTeX port → ☐ submitted

---

## Abstract (working version)

An anomaly detector can flag an unusual simulation trace, but it cannot decide
whether the trace is a real traffic finding or a modeling error. When an LLM agent
generates DES experiments autonomously, that decision can no longer wait for a
researcher to walk the event agenda by hand. We present RIM, a runtime invariant
monitor for microscopic traffic simulation that turns the invalid-versus-emergent
distinction into a three-way verdict: **reject** on hard physical invariant
violation, **quarantine** when invariants hold but the behavior vanishes under
seed/timestep/boundary perturbation, **escalate** when it persists — because
stop-and-go waves can be surprising without being invalid
[@sugiyama2008traffic]. The verdict is computed, never opined: no LLM sits in the
verdict path. We give the quarantine step an explicit statistical criterion,
implement the monitor inside the ScalaTion process-interaction engine
[@miller2024computational], and evaluate on a US-101 corridor [@bishi2026idm]
using two failure cases met the hard way: a same-tick arrival artifact that took
weeks to attribute by hand, and a lane-versus-corridor divergence that turned out
to be the scientific result.

---

## What already EXISTS in code (as of 2026-07-18 — the paper's evidence base)

| Paper claim | Implemented artifact | Proof |
|---|---|---|
| Flow conservation checkable | `RoadGraph` counters: `finished + exits == spawned` | asserted in 4 passing tests (Merge/Diverge/Connector/Measures) |
| Sensor substrate for per-edge occupancy | `RoadGraph` adjacency + `vCount` per `VTransport` | `runRoadGraphTest` |
| Snapshots-not-streams (I/O lesson) | `EngineTrace.on` gating after a trace-printing run became I/O-bound (killed at 75 min, 226 MB log) | session records, 2026-07 |
| Exact-match regression is possible | deterministic fingerprint: fitness 121.530785, counts (59757, 59753, 59750) reproduced exactly | `data/reference/calroute101_3_reference.md` |
| Edie measures per edge (q, k, v_s with q=k·v_s exact) | `VTransport.edieFlow/edieDensity/edieSpeed` | `runTrafficMeasuresTest` (identity asserted to 1e-9) |
| Shockwave sensing (LWR) | `RoadGraph.waveSpeed` = Rankine–Hugoniot Δq/Δk [@lighthill1955kinematic; @richards1956shock] | same test |
| Gated merge as engine-level admissibility | `tryMergeFromRamp` + park-and-ping | MergeHazardTest (101/101, parks=12) |

**Still to build (the paper's working tree):** `monitor/` package —
`RuntimeMonitor` (snapshot layer), `RuntimeInvariantMonitor` (verdict layer),
`InvariantSpec` (5 predicate families), `Verdict` enum, `QuarantineRunner`
(n-seed stability test), plus `rim_experiments` sweep script.

---

## 1. Introduction

**Claim.** Invalid-versus-emergent is first a physical admissibility question,
not a classification problem.

**Evidence.** The two historical cases (shockwave artifact vs lane-level finding)
— one paragraph each, stated as lived experience.

## 2. Related work

Runtime verification under uncertainty [@zhou2022runtime]; AgentGuard
[@koohestani2025agentguard]; AIVV's gate + adjudication council [@kwon2026aivv];
DURA-CPS role orchestration [@srinivasan2025dura]; classical trace checking
[@balci1998vv; @law1991simulation]. Position: they verify *agents*; RIM verifies
*the physics of what agents produce*.

## 3. The formal model and the five invariant families

**Structure (PADS style): Definitions → Lemma → Theorem → Propositions.
One theorem earns the word; lemmas only in its service; no padding.**

**Definitions (1–4).**
- *Def. 1 — Trace:* the time-ordered event sequence of a run; state = positions,
  speeds, lane assignments as functions of time.
- *Def. 2 — Snapshot:* the state sample the monitor pulls at checkpoint k
  (clock, per-edge occupancy, per-vehicle kinematics, entry/exit counts).
- *Def. 3 — Invariant predicates* (the five families, each over the snapshot/trace):
  1. **Flow conservation** — spawned = finished + exits + on-network, per edge
     (source of truth: the per-lane DLL `VTransport.vList` / `vCount`).
  2. **Collision exclusion** — x_j − x_i − ℓ_i > 0 for every leader j, follower i
     (exact form already on oral slide p.1).
  3. **Kinematic bounds** — 0 ≤ v ≤ v_max, −b_max ≤ a ≤ a_max [@treiber2000idm].
  4. **Lane integrity** — only adjacent-lane changes [@kesting2007mobil]; merge
     order consistent with the gated-merge queueing contract.
  5. **Event ordering** — timestamp-ordered activation through the engine agenda.
- *Def. 4 — Verdict function:* V(trace) ∈ {Reject, Quarantine, Escalate}, two
  tiers (hard/soft). Include per-predicate checking cost (O(vehicles) per snapshot).

**Lemma 1 (Membership).** At every yield point, each vehicle belongs to exactly
one lane-list (route segment, ramp, or connector DLL) and its state references
agree with it. *Proof sketch:* the `drive()` traversal maintains this by
construction (remove-after-move, add-before-jump; failed merge keeps the waiter
as ramp-DLL head). Enforced in one engine method, exercised by all tests.

**Theorem 1 (Soundness of Reject).** Every Reject verdict corresponds to a real
violation of the physics or the engine contract — the monitor never falsely
rejects. *Proof:* each hard predicate is a pure function of the snapshot
(no estimation, no sampling); conservation soundness follows from Lemma 1;
collision/kinematic/ordering predicates are direct reads of exact state.

**Proposition 1 (Determinism).** Same trace ⇒ same verdict: the verdict path
contains only pure predicates, no LLM. *Empirical witness:* the reproduced
run fingerprint (fitness 121.530785, exact coroutine counts, across runs).

**Proposition 2 (No upgrade).** Once a hard invariant fails, no later evidence
(replications, agent argument) can move the verdict past Reject: evidence
enters the pipeline only after the hard gate.

**Not theorems, on purpose:** LWR, Edie, and the sequential t-procedure are
citations [@lighthill1955kinematic; @richards1956shock; @law1991simulation] —
re-proving textbooks is reviewer bait.

## 4. The three-way verdict

**Claim.** Binary pass/fail turns a monitor into a confirmation engine that
suppresses valid-but-surprising behavior [@sugiyama2008traffic]. Reject /
quarantine / escalate; computed, never opined.

## 5. The watcher's material

**Claim.** The monitor must be deterministic *because* the modeler is stochastic:
a watcher adds assurance only if its failure modes are independent of the watched
[@rabanser2026towards]. The diagnostic LLM sits outside the RIM — consumes
verdicts, narrates, proposes; cannot overrule a red invariant. Two verdict tiers:
hard (blocking) and soft (advisory, e.g. face validity — no deterministic oracle).

## 6. The quarantine procedure (the committee's question, answered)

**A procedure with cited guarantees — NOT a theorem.** Three legs, all owned:

1. **Presence across seeds** — does the flagged behavior appear at all?
   Binomial framing: artifact hypothesis (appears with prob ≤ p₀) vs emergence
   (≥ p₁); n from error targets α, β — or better, Wald's sequential test (SPRT)
   to stop early on clear cases with bounded expected replications.
   *This is the one NEW derivation to write.*
2. **Magnitude stability** — the oral-slide rule (oral_equation.pdf p.2,
   already presented to the committee): n₀ = 10, replicate until
   t_{n−1,1−α/2}·s_n/√n ≤ δ·|X̄_n| — Law & Kelton's sequential
   relative-precision procedure [@law1991simulation], applied to the flagged
   statistic. DONE — reuse verbatim with the same notation.
3. **Timestep refinement** — an integration artifact shrinks as Δt → 0 at the
   integrator's convergence order: our own ANNSIM result plugs in directly
   [@bishi2026idm]. An emergent behavior survives refinement; an artifact decays.

Promotion rule: escalate only if the behavior passes all three legs; report
(n used, δ, α, refinement levels) in the evidence bundle.

**Also from the same slide deck:** the constrained objective
θ* = argmin Σ wᵢMᵢ(θ) subject to RIM(θ) = PASS (oral slide p.3) — the stopping
criterion; RIM as a hard constraint makes invalid regions unreachable, not
merely penalized. Cite it here, use it fully in Paper 1's §6.

## 7. Implementation in ScalaTion

**Claim.** Monitor as passive observer (RoadGraph style — owns nothing) over the
agenda + DLLs; **pull-based snapshots at checkpoints, never streaming traces**
(measured lesson, see evidence table). Per-edge occupancy and merge-competitor
sensing via the RoadGraph; Edie's definitions give internally consistent q, k,
v_s per edge; wave speeds via Rankine–Hugoniot.

## 8. Case studies (both verdict branches, real)

- **Reject:** Poisson same-tick arrivals → artificial shockwave (weeks to
  attribute by hand; RIM catches at spawn).
- **Escalate:** lane-level vs corridor-level divergence → became the finding of
  the first validation paper [@bishi2026idm].

## 9. Results (to produce)

Detection latency · miss rate on seeded faults · false-quarantine rate on
known-clean scenarios · overhead per simulated vehicle-second (monitor on/off).
**Blocked on:** the runtime regression fix (runs must cost minutes, not hours).

## 10. Limitations & the human boundary

Monitor never declares a discovery; agent never issues a verdict; only the human
promotes an escalation to a finding.

## 11. RIM as a protocol, not a component

Snapshot schema + invariant predicate signatures + verdict grammar = the narrow
waist. ScalaTion as reference implementation; `rim-py` SUMO adapter
[@lopez2018sumo] as the engine-agnosticism proof (full experiment in Paper 8);
conformance suite of seeded-defect + known-clean traces. Precedent: FMI
[@blochwitz2011fmi] — standardize the waist, leave the layers above competitive.

---

## Figures

1. RIM pipeline (validator → engine → monitor → three-way verdict → agent
   outside the boundary) — ASCII version in the brief, redraw.
2. The two case studies as space-time plots (needs the fast engine).
3. Wave-speed sensing at a merge (new — the Edie/LWR instrumentation).

## Porting plan

`acmart` (sigconf) template for SIGSIM-PADS; `[@key]` → `\cite{key}`;
refs.bib drops in unchanged after resolving the four VERIFY entries.
Bold "**Theorem 1 (…).**" markers here become `\begin{theorem}…\end{theorem}` —
acmart ships the definition/lemma/theorem/proposition environments; numbering
is automatic. Short proofs inline, long ones to the appendix.
