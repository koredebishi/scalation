# Graph Network Architecture — Takeover Review & Migration Spec

**Status:** SPEC — no code written yet. This document is the contract for the
`[GRAPH-P1]` implementation effort.
**Date:** 2026-07-16
**Testbed model:** `CalRoute101_3_NewParadigm` (copy of CalRoute101_3; original is frozen as regression baseline)
**Branch plan:** work happens on `feature/graph-network-paradigm`, cut from `feature/variable-lane-dll-unification` after a save-state commit + tag `pre-graph-paradigm`
**Related:** `docs/phd-papers/04-hpdes-rollback-across-resolution.md` and `05-links-as-agents.md`
depend on this architecture (the RoadGraph is Wilco Burghout's "common module" and the
substrate LinkState attaches to).

**Naming convention:** ScalaTion-native, never SUMO's. The graph vocabulary follows
the codebase's own lineage: Dr. Miller's `Dgraph` (animation package) already uses
inner `Node`/`Edge` classes, and `VTransport.scala:25` states that components form a
"graph." SUMO/AIMSUN terms appear in this document only as comparative analysis of
competing products, never as names for our code.

---

## 1. Why this document exists

The codebase is being made consistent with the meso–micro paradigm used by real traffic
simulation software (SUMO, AIMSUN, Mezzo/MiMe). Those systems are graph-first: nodes
(junctions) and edges (links) with an explicit connection layer, over which routing, OD
demand, spillback, and hybrid meso–micro boundaries are defined. This codebase currently
ships **corridors pretending to be networks**. Every feature on the dissertation path —
OD matrices, behavioral mapping, route choice, contraflow for the Eaton fire model, the
meso boundary, HPDES — is blocked by the same single absence: **junctions that know
their edges.**

Framing used for this review: *"AIMSUN bought this codebase and wants to make it a
competing product to SUMO. No hiding rot."*

---

## 2. Findings (with evidence)

### Finding 1 — There is no graph. There is a comment claiming a graph, and an array behaving like a corridor.

- `VTransport.scala:25-26` states the components "conceptually form a 'graph'".
  The word *conceptually* is doing all the work.
- Edges half-know the graph: `VTransport` extends `Transport`, which stores `from`
  and `to`. An edge knows its endpoint nodes.
- **Nodes know nothing.** `Junction.scala:32-91` — a Junction holds a name, a
  location, statistics, and `jump()`. No incoming-edge list, no outgoing-edge list,
  no connections. You cannot stand at a Junction and ask "what roads leave here?"
- The real structure is a positional array. `Pathway.scala:43-44`:
  `points = from +: junc.toList :+ to`; `seg = Array.ofDim[VTransport](points.length-1)`.
  A Pathway is a fixed linear sequence indexed by integer. A Route is parallel
  Pathways sharing one junction array (`Route.scala:124-134`). **A Route cannot
  branch by construction.**
- SUMO comparison: SUMO's network = nodes + edges + **connections**
  (`<connection from= fromLane= to= toLane=/>`) — the connection layer is what makes
  it a navigable graph and defines legal lane-to-lane movements. AIMSUN: sections +
  nodes + turning movements. Here: edges with endpoints, nothing else.
  **The graph is one-directional: edges → nodes, never nodes → edges.**

### Finding 2 — Roads do not know their up/downstream connections. The thing that connects roads is the vehicle's program counter.

- `CalRoute101_3.scala:213-228` — what connects segment `i` to segment `i+1` is the
  `cfor (joinSeg, highway_length)` loop inside `Car.act()`: move, remove from seg's
  DLL, add to seg+1's DLL, jump. **Topology lives in the actor's script, not in the
  network.**
- Everywhere downstream knowledge is needed, it is caller-side index arithmetic:
  - `Route.changeLane` (`Route.scala:190-191`): `vAhead = toPath.seg(seg+1).getLast`
    — the caller knows seg+1 is downstream; the segment itself doesn't.
  - `Pathway.selector` (`Pathway.scala:177-190`) carries Dr. Miller's own comment:
    *"FIX - this won't work in general"* — without node adjacency, direction choice
    at a junction has nothing to consult.
- The on-ramp is the damning case. `Ramp` (`Ramp.scala`) is not part of any Route and
  not connected to any Junction's edge list — a free-floating component with its own
  duplicate DLL. Its relationship to the mainline is soldered on by the model after
  construction: `CalRoute101_3.scala:162-165` sets `ramps(i).targetPathway` and
  `targetSegId` by hand. That is a wire, not a graph edge.
- Consequences for the three know-hows:
  - **Flow:** each VTransport senses itself (`snapshotDensity()`,
    `VTransport.scala:116-118` — genuinely good) but cannot see its downstream
    neighbor's density.
  - **Congestion/spillback:** impossible to signal upstream — "upstream" is not a
    queryable relation.
  - **Shockwave:** propagates correctly through the DLL *within* a lane-segment; at
    segment boundaries it depends on whichever call site did the seg+1 lookup; at a
    merge boundary it does not propagate at all.

### Finding 3 — The blind merge is confirmed in code. The collision fear is justified.

- `Route.mergeFromRamp` (`Route.scala:232-238`): remove from ramp → `getLast` on the
  target segment → insert. **No follower-gap check, no acceptance condition, no wait.**
- The comment at `CalRoute101_3.scala:206` — *"P2: gap acceptance — wait for safe gap
  before merging"* — is **aspirational**; the code below it calls `mergeFromRamp`
  unconditionally. Commented-out sketch at `CalRoute101_3.scala:339-353` acknowledges
  the "BLIND insert".
- A mainline follower can find a merged vehicle materialized inside its safety gap;
  only IDM emergency braking absorbs it. The CLAUDE.md known-issue entry is accurate.

### Finding 4 — No OD matrix, and structurally there *cannot* be one yet.

- A vehicle has no destination. Mainline cars drive to the single sink
  (`sinks.head.leave()`, `CalRoute101_3.scala:233`). Exit behavior is a spawn-time
  dice roll (`OffRampSpec.assignExit` — a turning-fraction model, not OD).
- `exitCheckAndSteer` (`Route.scala:285-316`) is a hand-rolled special case of
  *strategic lane changing toward a route target* — the general thing MOBIL + a real
  path would provide. The special case exists because the general substrate (a path
  over a graph) does not.
- Dependency chain: **no node adjacency → no path concept → no OD matrix → MOBIL
  stays tactical-only → Eaton cannot do route choice, rerouting, or contraflow.**
  OD is not a missing feature; it is a missing substrate.

---

## 3. Rot ranking (what blocks what)

1. **No node-side adjacency** (Junction knows nothing). Blocks: OD, routing, spillback
   signaling, merge arbitration, meso boundary, HPDES LPs. Everything traces here.
2. **Topology compiled into `act()`** — the `cfor` loop and the `subtype` encoding
   (`subtype < numLanes` = mainline lane, else ramp index —
   `CalRoute101_3.scala:183-196`). Network description and vehicle behavior are fused;
   consistency across models is impossible while this stands.
3. **`Ramp` as a bolt-on class** duplicating Pathway's DLL machinery, glued on by
   post-construction field assignment. In a graph model, a ramp is an ordinary
   one-lane edge whose downstream node has two incoming edges.
4. **Blind merge insert** — a physics bug, but structurally a symptom of 1+3.
5. **Sensors conflated with topology** — junctions named `"sensor1"`, `"warm_up"`,
   `"onR_merge1"` (`CalRoute101_3.scala:107-108`); `driveHighway` branches on
   `name.startsWith("sensor")`. SUMO separates detectors (devices on lanes) from
   junctions (topology). Bites hardest at the meso boundary, where boundary nodes
   must be topological, not wherever PeMS put a loop.
6. **Geometry in pixels, entangled with topology** — junction positions from animation
   coordinates on a 3000×2000 canvas; lane `GAP = 50.0` *pixels*; segment `length`
   from screen-curve length. A competing product keeps the network in world meters
   and projects for display.

## 4. What is genuinely good (the asset being acquired — do not throw away)

- **Per-lane, per-segment DLL as single source of truth** (`vList`, O(1) `vCount`).
  This *is* SUMO's per-lane vehicle container. Solid.
- Variable lanes per segment (`lanesPerSeg`), MOBIL living in the engine,
  Dynamics separation (IDM/Gipps/Krauss swappable), `Recorder` on junctions.
- **The process-interaction DES paradigm itself.** SUMO is globally time-stepped;
  the event-driven actor engine is the differentiator, and the meso side
  (event-based, Mezzo-style) plugs into it most naturally. This is the moat, not
  the rot.

---

## 5. Target architecture (ScalaTion-native vocabulary)

Naming follows the `Dgraph.Node`/`Dgraph.Edge` inner-class pattern already in
ScalaTion, and method names follow Miller's short-verb style (`at`, `jump`, `leave`).

- **`RoadGraph.Node`** (junction role): wraps a topology `Component`
  (Junction/VSource/Sink); holds `incoming: VEC[Edge]`, `outgoing: VEC[Edge]`;
  later phases add lane-to-lane movement rules and conflict/priority handling
  (the role SUMO's connection layer plays, named our way).
- **`RoadGraph.Edge`** (link role): wraps a `VTransport`; knows `fromNode`, `toNode`,
  `laneId`, `segId`, owner (Pathway or Ramp). Up/downstream discoverable through
  its nodes.
- **`RoadGraph`**: id→node and id→edge maps; adjacency queries (`incoming`,
  `outgoing`, `downstream`, `upstream`, `mergeTargets`); later phases add
  shortest path (Dijkstra over travel times). This object is, verbatim, Burghout's
  *common module* (network graph + travel-time table) — the meso side plugs into
  the same structure later. It is also what Paper 5's LinkState hangs the downstream
  congestion signal on. Deliberately distinct from animation's `Dgraph`, which
  belongs to rendering and is untouchable.
- **Trip/OD (later phase)**: OD matrix → per-vehicle (origin, destination,
  path = list of edge ids); `act()` becomes generic graph traversal:
  `while (edge != last) { drive(edge); node.transfer(this) }`.
- **Merge at node (this phase's proof)**: gap acceptance at the junction using
  existing machinery (`safetydist`, `idmAccelFor`, dual-leader peek) — composed
  for the first time into a wait-for-gap.
- **Contraflow (later phase, Eaton)**: reversing a link = adjacency + capacity
  mutation on the graph. Unimplementable in the current structure (direction baked
  into geometry + act script); near-trivial in the graph structure.

## 6. Migration phases (models never stop running)

- **Phase 0 — freeze behavior.** CalRoute101_3 and Eaton runs become regression
  baselines (sensor flows, densities). Every phase must reproduce them before merge.
- **Phase 1 — build the graph as an index, not a rewrite.** `RoadGraph` populated
  by walking the existing objects (every `route.pathway(l).seg(i)`, every `Ramp`
  registers). Nothing moves; every "who is downstream of me?" becomes answerable.
- **Phase 2 — merge arbitration at nodes.** Replace blind insert with gap acceptance
  at the merge node, competitor edges found by network query (not `targetPathway`).
  Kills the collision risk.
- **Phase 3 — vehicle carries a path.** `act()` becomes graph traversal. The
  `subtype` encoding, the `cfor` loop, and `pathway.seg` hardcoding die here. Models
  collapse into network description + demand — the consistency goal in one sentence.
- **Phase 4 — OD matrix + route choice.** Dijkstra pre-trip; en-route re-evaluation
  at nodes later (Pel's hybrid route choice — what the fire model needs). Contraflow
  becomes a graph mutation.
- **Phase 5 — meso.** Same RoadGraph; a meso link is an edge whose runtime is a
  queue-server + speed–density function instead of a DLL. Resolution becomes an edge
  attribute; Wilco's virtual-link boundary finally has a home.

---

## 7. MVP scope — the 80/20 (Phases 1 + 2 on a cloned model)

**In scope:**
1. **`RoadGraph` adjacency index** — passive, additive, populated by walking
   already-built objects. Deliverable: the printable topology audit, e.g.
   ```
   node onR_merge1: in  = [RteL0.s1 … RteL4.s1, OR1.lane]
                    out = [RteL0.s2 … RteL4.s2]
   ```
   eyeballed against the system diagram. When this dump is correct, adjacency is real.
2. **One consumer to prove it: gap-accepting merge** —
   `Route.tryMergeFromRamp(...): Boolean`, competitor mainline edge found via
   `RoadGraph.mergeTargets(ramp)` (name continues the existing `Ramp.targetPathway`
   vocabulary it replaces), gaps checked against `safetydist`
   (optionally IDM hypothetical deceleration via existing `idmAccelFor`), insert only
   if safe, `false` → caller waits and retries.
3. **Regression gate** — original CalRoute101_3 vs NewParadigm, same seeds: sensor
   flows/densities. Merge behavior is allowed to change (that is the point — it was
   wrong); through-traffic on an empty-ramp scenario must not.

**Explicitly OUT of the MVP** (scope cannot creep): no `act()` rewrite, no
path-carrying vehicles, no OD, no Dijkstra, no sensor/junction separation, no
meters-vs-pixels fix, no meso. Phases 3–5 start only after the audit dump and the
merge consumer are proven.

**Testbed principle:** `CalRoute101_3_NewParadigm` uses the index and the new merge;
the original stays byte-for-byte frozen. Engine classes are shared between both
models, so the MVP rule is: **engine changes must be additive** — new symbols only,
never edits to paths the old model executes. Same engine, two wirings — that is what
makes "if it works there it works everywhere" true.

---

## 8. File manifest

### NEW files

| File | New symbols | Est. size | Purpose |
|---|---|---|---|
| `src/main/scala/scalation/simulation/process/RoadGraph.scala` | `class RoadGraph` with inner `class Node` and `class Edge` (Dgraph inner-class pattern), companion `object RoadGraph`, and `@main def runRoadGraphTest` in the same file (Rule 0, §11) | ~150–250 lines | The passive adjacency index. `register(route: Route)`, `register(ramp: Ramp)`, queries `downstream(edge)`, `upstream(edge)`, `incoming(node)`, `outgoing(node)`, `mergeTargets(ramp)`, and `audit(): String`. Wraps existing objects; owns nothing; changes nothing. |
| `src/main/scala/scalation/simulation/process/model/CalRoute101_3_NewParadigm.scala` | `class CalRoute101_3_NewParadigm`, its `Car` with modified `act()`/`driveHighway()`, `@main def runCalRoute101_3_NewParadigm` | copy + ~40 changed lines | The testbed: builds the RoadGraph after construction, prints `audit()` at init, merge call uses `tryMergeFromRamp` with retry-wait. All divergences follow tracing Rule B. |
| `src/main/scala/scalation/simulation/scripts/graph_regression.scala` *(optional)* | `@main def runGraphRegression` | ~60 lines | Same-seed diff of sensor flow/density reports between the two models. May be replaced by manual `SimulationReport` comparison. |

### ADJUSTED files (additive only, tagged, old paths stay live)

| File | Addition | Untouched in same file |
|---|---|---|
| `src/main/scala/scalation/simulation/process/Route.scala` | New method `tryMergeFromRamp(lane, seg, car, fromRamp, graph: RoadGraph): Boolean`; `[GRAPH-P1] LEGACY-PATH` tag comment above existing `mergeFromRamp` (line ~232) — old body unchanged, still executed by baseline. | `changeLane`, `forceMerge`, `exitCheckAndSteer`, `OffRampSpec`, constructor, geometry helpers. |
| `src/main/scala/scalation/simulation/process/Ramp.scala` | Comment-only: `[GRAPH-P1] LEGACY-PATH` note on `targetPathway`/`targetSegId` (lines ~41-42) — superseded by `RoadGraph.mergeTargets` in the new paradigm, kept for baseline. Zero behavior change. | Everything. |

### UNTOUCHED files (explicit boundary — auditable)

`Junction.scala` (adjacency lives in the index; Junction gets edge lists in a later
phase, if ever), `VTransport.scala`, `Pathway.scala`, `Vehicle.scala`, `Model.scala`,
`Transport.scala`, `Dynamics.scala`, `VSource.scala`, `Sink.scala`,
`CalRoute101_3.scala` (the frozen baseline), `EatonFireModel.scala` (migrates only
after the testbed proves out), and Dr. Miller's core: `Dgraph.scala`,
`Animator.scala`, `AnimateCommand.scala`.

**Escalation rule:** if implementation genuinely cannot avoid touching an
"untouched" file, that is a stop-and-report moment, not a quiet edit.

---

## 9. Tracing convention

**Rule A — shared engine files: old code stays LIVE, not commented.** The baseline
model must keep executing the old paths (regression). Nothing deleted or commented in
engine files; additions are tagged:

```
// [GRAPH-P1] NEW: <what and why>
// [GRAPH-P1] LEGACY-PATH: kept live — original CalRoute101_3 baseline executes this.
//            See context/graph-network-architecture.md
```

`grep GRAPH-P1` shows every line this effort touched. Nothing untagged changed.

**Rule B — the new model file: replaced lines commented in place above the
replacement**, so old and new read side by side:

```
// [GRAPH-P1] WAS (blind insert):
//     route.mergeFromRamp (laneID, joinSeg, this, onRamp)
// [GRAPH-P1] NOW (gap-accepted via network query):
```

---

## 10. Known design hazard (highest-risk spot in the MVP)

The wait-and-retry merge interacts with a documented bug: *"if you yield to director
while waiting, follower ramp vehicles are blind and drive over the waiting vehicle"*
(CLAUDE.md known issues). The gated merge makes vehicles wait at the ramp end **by
design**, so this goes from theoretical to guaranteed unless handled.

**Design answer:** the waiting vehicle stays in the ramp's DLL as its head during the
wait (removal from the ramp happens only on successful insertion — same deferred
removal as today), so followers' IDM sees it as a stopped leader and queues behind
it, like the benign `gap = -4` queuing case. This keeps the fix inside existing
mechanics — but it is the one spot where a subtle coroutine-ordering bug can hide.
It gets the most careful reading and a **dedicated test scenario: two ramp vehicles
arriving while the mainline is dense.**

---

## 11. Testing discipline (deliberately minimal — no over-engineering)

Two practices only. Everything heavier (guard scripts, hooks, sbt aliases, CI) is
OUT unless the work proves we need it.

- **Rule 0 — Miller's test convention is mandatory.** ScalaTion source files end with
  `@main def runX` test/demo functions in the same file (see `Transport.scala`,
  `Source.scala`). No new file without one. `RoadGraph.scala` ships with
  `@main def runRoadGraphTest`: builds a toy network (two junctions, one Route, one
  Ramp), prints `audit()`, asserts adjacency facts.
- **The reference run — NOT "golden."** Before any engine change, run the original
  CalRoute101_3 once (headless, fixed seed) and save its report output as a
  *reference of current behavior, correctness unknown*. The current output cannot be
  called correct — it contains known bugs (the blind merge). The reference answers
  exactly one question: "did I change something I did not intend to change?" It has
  no authority on "is this right?" — physics tests and the researcher judge that.
  Where behavior is SUPPOSED to change (the merge), the reference is explicitly not
  the judge.
- **Known-bugs → scenario tests, as touched.** When a known issue is worked on, it
  gets a Miller-style `@main` scenario first: the §10 merge hazard (two ramp
  vehicles, dense mainline) is the first and, for this MVP, the only one. These
  double as early RIM invariants (see
  `docs/phd-papers/02-runtime-invariant-monitor.md`).

Deferred (documented so they are a decision, not an omission): guard scripts for
untouchable files and tag-audits, pre-commit hooks, sbt aliases, `.gitignore` for
`target/**`, scoped lint. The untouchable-files rule is enforced the simple way:
the implementer does not touch them, and diffs are reviewed.

## 12. Save-state protocol (before any code)

1. Commit current working tree on `feature/variable-lane-dll-unification`.
2. Tag: `pre-graph-paradigm`.
3. Branch: `feature/graph-network-paradigm` — all new-paradigm work lives there.
4. CLAUDE.md session-state entry per the continuation protocol.

(Side note, decoupled from this effort: `target/scala-3.8.2/**` build artifacts are
tracked in git; worth a `.gitignore` cleanup someday.)
