# ScalaTion Traffic Simulation

Microscopic traffic simulation using ScalaTion 2.0 (Scala 3).  
US-101 corridor with IDM/Gipps/Krauss car-following dynamics.

**IDE:** IntelliJ IDEA  
**HPC:** Sapelo2 (krb84578@uga.edu)

## Core Rules
- **Always ask permission before writing or modifying code**
- **Do not plagiarize** - all paper text must be original
- **Do not hallucinate** - if unsure, say so

## Chat Continuation Protocol

When the context window is nearly full (~80%), **before the conversation is cut off**:

1. Write a `## Session State` section at the bottom of this file with:
   - **Date** of session
   - **What was completed** (list of changes, files touched)
   - **What was in progress** (unfinished task + exact next step)
   - **Known bugs/issues** found but not yet fixed
   - **Key decisions made** (design choices, rejected alternatives)
2. Tell the user: *"Context is nearly full. I've saved session state to `CLAUDE.md`. Paste this into your next chat to continue."*

This ensures zero loss of continuity between chat sessions.

## Context Files

| Need | File |
|------|------|
| Behavioral guidelines | `context/rules.md` |
| Coding style & key files | `context/scalation-style.md` |
| IDM params & domain knowledge | `context/traffic-simulation.md` |
| HPC Sapelo2 commands | `context/hpc.md` |
| Variable lane count problem | `context/variable-lane-architecture.md` |
| Ramp physics + density lane assignment | `context/ramp-physics-and-density-lane-assignment.md` |

## Papers

| Paper | Status | File |
|-------|--------|------|
| ANNSIM 2026 | ✅ Submitted | `context/papers/annsim-2026.md` |
| WSC 2026 — Wildfire/Contraflow | 🔄 Active Target | `context/papers/wsc-2026.md` |

## Dissertation Studies

| Study | Title | Status |
|-------|-------|--------|
| Study 1 | Structural Sensitivity Analysis (integrators, arrivals) | ✅ ANNSIM 2026 Submitted |
| Study 2 | Wildfire Evacuation & Contraflow on I-10 (Palisades Fire) | 🔄 WSC 2026 Climate Resilience Track — Active |
| Study 3 | Unified Agentic Architecture (long-term, internal) | 🔄 Internal Vision — Not for committee or PI yet |

**NOTE: Calibration is a supporting result, not a standalone paper. PI is not interested in calibration as a paper. WSC 2026 target is the Wildfire/Contraflow study aligned with Climate Resilience theme.**

## Active Focus
**WSC 2026 (Wildfire/Contraflow)** - Deadline: April 5, 2026  
See `context/papers/wsc-2026.md` for timeline and checklists.

## Quick Reference

### Run Simulation
```bash
# US-101 (CalRoute101_3)
sbt "runMain scalation.simulation.process.model.runCalRoute101_3"

# Eaton Fire (I-210 + SR-134)
sbt "runMain scalation.simulation.process.model.runEatonFireModel"
```

### Key Entry Points
- US-101 Model: `CalRoute101_3.scala`
- Eaton Model: `EatonFireModel.scala` (I-210 WB + SR-134 WB, dual-corridor)
- Calibration: `CalibrateCalRoute101.scala`
- Dynamics: `Dynamics.scala` (IDM, Gipps, Krauss)

## Session State — Last Updated: 2026-03-31

### What was completed (previous sessions)
- ✅ All I-210 data pipeline work (anchor CSV, PeMSDemand, arrivals, ramp data) — see git history
- ✅ Fixed lane count, off-by-one, nStop verification
- ✅ Variable-Lane DLL Unification (Tasks 0–11) — commit `8bdf7ae12`

### What was completed (this session — Graph-Derived Ramp Positioning + Visual Fixes)
- ✅ Off-ramp same-side positioning (was on opposite side of mainline)
- ✅ FR/OR collision nudge for same-PM stations (50px → replaced by graph-derived)
- ✅ Shortened all labels: route `I-210-W_Rte_0_seg19` → `I210W_RL0s19`, ramps `I210_OR6` etc.
- ✅ Ramp label declutter: 3 labels per ramp → 1 (VTransport only, Junction/VSource/Sink blanked)
- ✅ **Graph-derived ramp positioning** — `Route.rampAttachPoint(seg)` + `perpVec`
  - Task 0: `Route.scala` — added `rampAttachPoint`, `perpVec`, `_points` array
  - Task 1: `CorridorBuilder.scala` — reordered: Route built first, ramp junctions derived from geometry
  - Task 2: `EatonFireModel.scala` — ramp VSource positions use `rampAttachPoint + perpVec * RAMP_LEN`
  - Task 3: `EatonCorridorConfig.scala` — removed `rampShift` from all methods, steps 11/12 now dummy
  - Task 5: `sbt compile` — zero errors
- ✅ **Side swap fix** — negated `perpVec` so ramps are on outermost lane side (not lane 0)
- ✅ **Same-seg FR/OR nudge** — off-ramps shifted 30px downstream along road direction when sharing joinSeg with on-ramp
- ✅ **DTA Blueprint** — `docs/2026_WSC_paper/dta-blueprint.md`
- ✅ **Git push**: `f3515ee8d` on branch `feature/variable-lane-dll-unification`

### Files touched (this session)
| File | Changes |
|------|---------|
| `Route.scala` | `rampAttachPoint(seg)`, `perpVec`, `_points` array; pathway naming `L$i`; VTransport naming `s$i` |
| `CorridorBuilder.scala` | Route-first build order; ramp positions from `rampAttachPoint`; road-direction nudge for same-seg FR/OR; shortened prefix; blanked Junction/Sink names |
| `EatonFireModel.scala` | Ramp VSource positions from `rampAttachPoint + perpVec * RAMP_LEN`; removed `getVSourceCenterAndOffsets`; shortened all component names |
| `EatonCorridorConfig.scala` | Removed `rampShift` from all 3 methods; steps 11/12 replaced with dummy arrays |
| `Pathway.scala` | VTransport naming `s$i` instead of `_seg$i` |
| `context/graph-derived-ramp-positioning.md` | Design document |
| `docs/2026_WSC_paper/dta-blueprint.md` | DTA implementation blueprint |

### What is in progress
- 🔄 **Ramp physics fixes (Phase A)** — 6 bugs diagnosed, implementation plan in `context/ramp-physics-and-density-lane-assignment.md`
- 🔄 **Density-based lane assignment (Phase B)** — design complete, not yet implemented
- 🔄 **Visual verification** — need to run EatonFireModel and confirm ramp positions are correct after graph-derived changes
- 🔄 **End-to-end run with `synthetic=false`** — nStop verified, full simulation run not yet attempted
- 🔄 **SR-134 ramp data quality** — all 7 SR-134 on-ramp sensors report zero flow
- 🔄 **DTA Phase 1** — FireGrid + SmokeGrid (standalone, no traffic dependency)

### Known bugs / issues
| Issue | File | Status |
|-------|------|--------|
| `findLeader` returns null on ramp → free-flow | `Dynamics.scala:58-72` | ✅ **Fixed** — wired `targetPathway`/`targetSegId` in EatonFireModel |
| `t_disp` domain mismatch at ramp→mainline | `Dynamics.scala:345-351` | 🔴 **Critical** — IDM sees phantom gaps after merge |
| No gap acceptance at merge | `EatonFireModel.scala:370` | 🟡 **Major** — unconditional DLL insertion |
| No lane change in EatonFireModel | `EatonFireModel.scala:384` | 🟡 **Major** — 20K ramp vehicles stuck in outermost lane |
| Animation teleportation at merge | `EatonFireModel.scala:444-451` | 🟡 **Major** — car jumps from ramp to mainline |
| Stale `segId` comparison in IDM | `Dynamics.scala:348` | 🟡 Minor — `segId <` doesn't work across domains |
| SR-134 OR CSV has zero flow everywhere | `eaton_134_W_baseline_Dec03-10-17_OR.csv` | **Data quality** — sensors not reporting |
| `srcPrefix` hardcoded for cases `0\|1\|2\|3\|4` | `VSource.scala:61` | Works for 5 lanes but fragile |
| Fire-day data not yet wired | `DemandConfig.scala` | Need `PeMSDemand.I210_WB_FireDay_Anchor()` |
| `forceMerge` was over-complex + double insertion | `Route.scala` | ✅ **Fixed** — simple lane-drop: target = `availLanes.last` |
| Ramp side may need visual tuning | `Route.perpVec` | Negated for outermost lane — needs visual confirmation |

### Key decisions made (this session)
- **Ramp physics: 6 bugs identified** — traced full lifecycle: `driveRamp` → `addToAlist` → `move()` → `findLeader`
- **Density-based lane assignment over speed-based**: density is macroscopic LOS indicator, doesn't require per-lane PeMS data
- **Junction = decision engine**: shared cross-section senses `snapshotDensity()` across all lanes at each segment boundary
- **±1 lane constraint**: only adjacent lane changes, with existing `Route.changeLane()` safety check
- **Implementation order**: P0 → P5 → P1 → P2 → D0 → D1 → D2 → P3 → P4 → D3
- **t_disp rebase via `route.toCumulative(joinSeg, 0.0)`**: aligns ramp vehicle to mainline coordinate system
- **Dual-leader for ramp**: `findLeader` will peek at mainline DLL when `myRamp != null` (Treiber §11.3)
- **Ramp stores merge target**: `Ramp.targetPathway` + `Ramp.targetSegId` fields (cleaner than Vehicle fields)

### Key decisions made (session 2026-04-04 — Ramp Physics Fix)

#### Coding Style: No Surgical Fixes
- **Separate `b` (comfortable) from `b_emergency` (physical max):** IDM's `bmax = -2.0 m/s²` is the comfortable deceleration used in the `s*` desired-gap formula. The emergency braking floor `b_emergency = -9.0 m/s²` (0.9g) is a separate physical quantity. Declared at the same level as `bmax` in `Vehicle.def_prop` with its own `inline def` accessor. All three models (IDM, Gipps, Krauss) use `b_emergency` for clamps. Treiber & Kesting 2013, Table 11.1 distinguishes these explicitly.
- **`LeaderResult` enum replaces naked `Vehicle` return from `findLeader`:** The root cause of the cross-domain gap bug is that `findLeader` returns `Vehicle | null` with no context about which coordinate system the leader's `disp` lives in. The caller (`updateM`) cannot distinguish same-segment, next-segment, or cross-domain leaders — leading to `segId` type confusion. Fix: `findLeader` returns `enum LeaderResult` with exhaustive `match/case`. The compiler enforces all cases are handled. No if/else branching. Each variant carries exactly the data needed for gap computation. This protects all three car-following models uniformly.
- **No deep if/else nesting for domain logic.** Use Scala 3 `enum` + pattern matching to make the contract between `findLeader` and its callers explicit and compiler-enforced.
- **Parameters declared at the level of their peers.** New physics constants (like `b_emergency`) go in `Vehicle.def_prop` alongside `bmax`, `amax`, etc. — not as local vals inside functions.

### Key decisions made (session 2026-04-08 — forceMerge Simplification)

#### Lane-Drop forceMerge: Simple English
A highway goes from 5 lanes to 4 lanes. One lane dies (dead-ends).
Every vehicle in that dying lane has exactly **one option**: merge into the adjacent surviving lane.
It's a **zipper/bottleneck** — vehicles funnel into the next lane, like a highway lane closure with cones.
There is no "best lane search." There is no scanning lanes 0–2. The dying lane feeds into its neighbor. Period.

- `forceMerge` does the **full job**: remove from dead lane DLL, insert into surviving lane behind leader, return new laneID.
- Caller just uses the returned laneID and updates `myPathway`. No DLL work by caller.
- Old code did DLL work inside AND the caller did it again → **double insertion bug**, now fixed.
- Target lane = `availLanes.last` = highest surviving lane = adjacent to the dead one.
- For 5→4: car in lane 4 → target lane 3. Done.

### Previous session decisions (preserved)
- **Graph-derived ramp positioning**: `rampAttachPoint(seg)` computes outermost lane edge from `lanesAt(seg) * GAP`. Eliminates all `rampShift` magic numbers.
- **perpVec negated**: points away from lane 0 (toward ramp side of freeway)
- **Same-seg FR/OR nudge = 30px downstream**: uses road direction vector, not perpendicular
- **RAMP_LEN = 150px**: consistent between CorridorBuilder (sinks) and EatonFireModel (VSources)
- **Labels: VTransport only per ramp**: Junction/VSource/Sink names blanked to avoid clutter
- **Prefix shortened**: `I-210-W_` → `I210W_` via `filter(_.isLetterOrDigit)`
- **DTA architecture**: Junction = decision engine, VTransport = data provider, Route = minimal

### Verified nStop values (baseline, synthetic=false)
```
I-210 Mainline (5 lanes): L0=5326, L1=5174, L2=4019, L3=3281, L4=1873
I-210 Ramps (22): 0,2234,2165,1960,3522,0,0,0,0,0,744,0,0,0,1539,549,0,2315,0,5456,53,0
SR-134 Ramps (7): all zero (data quality issue)
```

## Session State — 2026-04-13 (Road Rendering & Physics Audit)

### What was completed this session
- ✅ **Full codebase audit of animation engine** — read every rendering line in `DgAnimator.scala` (1138 lines), `Animator.scala`, `Dgraph.scala`, `VTransport.scala`, `Vehicle.scala`, `Route.scala`, `EatonFireModel.scala`
- ✅ **Diagnosed ramp `gap = -4` root cause** — NOT a braking/clamp issue. Root cause: `VTransport.move()` line 135 sets `actor.disp = 0.0` for every vehicle entering. When VSource spawns ramp vehicles faster than the ramp drains them, multiple vehicles sit at `disp=0.0` → `gap = 0 - 0 - 4(len) = -4`. The collision clamp computes `x_safe = 0 - 4 - 4 = -8 → max(0, -8) = 0` — can't fix it. This is a **spawning/insertion problem**, not a dynamics problem. Fix = VSource back-pressure (don't emit if no space).
- ✅ **Created implementation plan** — `context/visual-physics-upgrade-plan.md` (10 tasks, 3 phases, ~175 lines total)
- ✅ **Identified existing features often assumed missing**:
  - Speed-based velocity coloring ALREADY LIVE: `Vehicle.velocityColor()` (HSB red→green) called in `VTransport.move()` line 160
  - Vehicle shape ALREADY car-shaped: `VSource.scala` line 118 uses `RoundRectangle2D.Double(0, 0, 14, 7, 4, 4)`, not 8×8 Ellipse
  - HUD overlay, vehicle inspector, replay system — all functional
  - `scala3d/` directory = dead prototype by Jacobi Coleman, `.scalaa` extension (won't compile), has merge conflicts — leave it alone

### What is in progress — ACTIVE NEXT TASK
- 🔄 **R1: Road polygon rendering** — THE priority. Replace spaghetti-line roads with filled asphalt polygons.
  - **File:** `DgAnimator.scala` — `Canvas.paintComponent()` (~line 557)
  - **Current state:** Each lane is a separate edge drawn with `pavementStroke = BasicStroke(10.0f)` — roads look like individual strands, not a surface
  - **Target:** Insert Layer 0 before existing Layer 1. Group edges by segment (parse label pattern `L{lane}s{seg}`). For each segment group, compute a filled `Path2D` polygon covering full road width. Fill with `pavementColor = Color(50, 50, 58)`.
  - **Also:** R2 (lane markings: solid white edges, dashed white interior) depends on same edge-grouping

### Known architecture for the rendering (so next chat doesn't need to re-discover)

#### How DgAnimator.paintComponent renders (current layer order):
```
Layer 1: pavementStroke (BasicStroke 10px) on each edge → dark strip per lane
Layer 2: dashStroke (dashed 1px) on each edge → dashed line per lane  
Layer 3: roadStroke (BasicStroke 2.5px) in edge.color → thin colored outline per lane
Layer 4: edge labels + tokens (vehicle dots) + vehicle count badges
Layer 5: free tokens
Layer 6: HUD overlay (screen-space)
```

#### Key rendering constants (DgAnimator.Canvas, ~line 329):
```scala
pavementStroke = BasicStroke(10.0f, CAP_ROUND, JOIN_ROUND)
dashStroke     = BasicStroke(1.0f, CAP_BUTT, JOIN_MITER, 10.0f, Array(12.0f, 8.0f), 0.0f)
roadStroke     = BasicStroke(2.5f, CAP_ROUND, JOIN_ROUND)
pavementColor  = Color(50, 50, 58)
dashColor      = Color(200, 200, 210, 160)
```

#### How vehicles are drawn:
- Created in `VSource.act()` line 118: `RoundRectangle2D.Double(0, 0, 14, 7, 4, 4)` + `velocityColor`
- Moved in `VTransport.move()` line 161: `director.animate(actor, MoveToken, vColor, null, cp)`
- Rendered in `DgAnimator.paintComponent` line 590: `g2d.fill(token.shape)` with glow behind
- Token default size = 8.0 (in `Animator.createToken`) but VSource overrides to 14×7

#### Edge (road lane) data flow:
- `Pathway.display()` calls `director.animate(lane, CreateEdge, ...)` for each VTransport
- Each VTransport is a `QCurve` from junction[i] to junction[i+1]
- Label pattern: `"L{lane}s{seg}"` (e.g., `"L0s0"`, `"L4s19"`)
- Stored in `Dgraph.edges` as flat list — no segment grouping
- Edge shape = `QCurve` with start/end points accessible via `shape.getP1()`, `shape.getP2()`

#### Route geometry available:
- `Route._points` array: junction (x,y) positions
- `Route.rampAttachPoint(seg)`: outermost lane edge position
- `Route.perpVec`: perpendicular to road direction (outward from lane 0)
- `Route.GAP = 50.0` pixels between lanes
- `Route.lanesAt(seg)`: lane count at each segment

#### What NOT to touch:
- `Dgraph.scala` — Dr. Miller's graph data structure
- `Animator.scala` — Dr. Miller's command processor  
- `AnimateCommand.scala` — command protocol
- `Model.scala` — simulation engine core

### Decisions made this session

#### Road rendering approach (approved direction):
1. **Java2D only** — no JavaFX, no ScalaFX, no 3D. Java2D `Graphics2D` + `Path2D` + `AffineTransform` gives everything SUMO-gui does at top-down 2D.
2. **Enhance DgAnimator in-place** — don't create a new animation class. The command queue, replay, inspector all work. The problem is **what it draws**, not how.
3. **Edge-grouping by label parsing** — group `Dgraph.edges` by segment using label pattern `L{lane}s{seg}`. Zero changes to `Dgraph.scala`.
4. **Vehicle rotation via position delta** (Approach A) — compute heading from consecutive MoveToken positions, store in HashMap inside DgAnimator. No changes to `Animator.scala` or `AnimateCommand` protocol.

#### Ramp gap = -4 diagnosis (corrected):
- **P0 in the plan was WRONG** — the `-4` gap on ramp vehicles at `disp=0, v=0` is a spawning problem, not braking
- **Correct fix** = VSource back-pressure (don't emit if ramp tail hasn't cleared `len + s` from position 0)
- **Deferred** — user chose to focus on road rendering first

### Files created this session
| File | Purpose |
|------|---------|
| `context/visual-physics-upgrade-plan.md` | Full implementation plan (10 tasks, 3 phases) |

### Implementation plan summary (for reference)
| Phase | Tasks | Status |
|-------|-------|--------|
| Phase 1: Physics | P0 (corrected: VSource back-pressure), P1 (lane change), P2 (gap acceptance) | ⏸ Deferred |
| Phase 2: Road Rendering | R1 (road polygon), R2 (lane markings), R3 (vehicle rotation), R4 (ramp surface) | 🔄 **ACTIVE — Start R1** |
| Phase 3: Polish | M1 (smooth merge), R5 (shoulders), Labels (shields) | ⏸ Deferred |
