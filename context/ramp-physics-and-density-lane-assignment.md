# Ramp Merge Physics Fix + Density-Based Dynamic Lane Assignment

**Author:** Bishi  
**Date:** 2026-03-31  
**Status:** Design — Not yet implemented  
**Relates to:** WSC 2026, DTA Blueprint (`docs/2026_WSC_paper/dta-blueprint.md`)

---

## Motivation

Two coupled problems produce unrealistic animation and non-physical vehicle behavior
in the EatonFireModel (and CalRoute101_3):

1. **Ramp merge physics is broken** — vehicles on the ramp cannot see mainline
   traffic, merge unconditionally (no gap check), and carry a `t_disp` from a
   different coordinate domain that confuses IDM's gap computation.

2. **No lane change is enabled** — all ~20,500 ramp vehicles enter the outermost
   lane (`nLanes - 1`) and remain there for the entire corridor.  The inner lanes
   flow freely while the merge lane is a solid wall.

A naïve lane-change heuristic (CalRoute101_3's "if car ahead is slow, change lane")
is **not acceptable** for this model because:

- It is not calibrated: we do not know from PeMS how many vehicles change lanes.
- It couples the fix to upstream sensor data that doesn't exist per-lane.
- It does not use the information that is available: **segment density at the
  shared junction cross-section**.

Instead, the lane-change decision should be a **density-sensing dynamic assignment**
made at each junction, using the infrastructure that already exists: every junction
is shared across all lanes, and every VTransport already has `snapshotDensity()`.

This document specifies the full task flow.

---

## Architecture: How It Works Today

```
Ramp VSource ──→ Ramp VTransport (own DLL) ──→ Junction ──→ Mainline VTransport (own DLL)
                     ↑                              ↑               ↑
               myRamp = this                  jump()          myPathway = Pathway
               myPathway = null                               segId = joinSeg
               findLeader → null → FREE FLOW                  disp = 0
                                                               t_disp = ramp accumulation (WRONG)
```

### Six Identified Bugs

| # | Problem | Severity | Root Cause |
|---|---------|----------|------------|
| 1 | `findLeader` returns `null` on ramp → free-flow | 🔴 Critical | `myPathway = null` on ramp; no `myRamp` path in `findLeader` |
| 2 | `t_disp` discontinuity at merge | 🔴 Critical | IDM uses cumulative position; ramp domain ≠ mainline domain |
| 3 | No gap acceptance at merge | 🟡 Major | Unconditional `addToAlist` — vehicles teleport into occupied space |
| 4 | Animation teleportation | 🟡 Major | No visual interpolation between ramp endpoint and mainline entry |
| 5 | Velocity preserved without physics | 🟡 Major | Car enters mainline at ramp free-flow speed, no transition |
| 6 | Stale `segId` comparison in IDM/Krauss | 🟡 Minor | `car_ahead.segId < car.segId` doesn't work across domains |

---

## Phase A: Ramp Merge Physics Fixes

### P0 — Eliminate `t_disp` from Physics (Segment-Local `disp` for All Models) ✅ DONE

**The problem in detail:**

With the old Pathway-spanning DLL, `findLeader` could return a car 5, 10, 15
segments ahead.  `t_disp` (cumulative displacement from lane start) was needed so
the gap `leader.t_disp - follower.t_disp - L_veh` was meaningful.

With per-segment DLLs (DLL inside VTransport), the leader is always in the **same
or adjacent segment** (±1).  The gap can always be expressed in segment-local `disp`:

- Same segment: `leader.disp - follower.disp - L_veh`
- Next segment: `(length - follower.disp) + leader.disp - L_veh`

**`t_disp` has no business in the physics anymore.**  It is a relic of the old
Pathway-spanning DLL architecture.

IDM was the only model still using `t_disp` for gap computation and ODE state.
This caused the ramp→mainline discontinuity: ramp vehicle `t_disp` ≈ 150 m,
mainline leader `t_disp` ≈ 5000 m → IDM saw a 4850 m gap → free-flow hallucination.

**Fix (implemented 2026-03-31):**

Switched ALL three car-following models to segment-local `disp`:

| Model | Gap computation | Position integration | `t_disp` role |
|-------|----------------|---------------------|---------------|
| **Gipps** | `disp` ✅ (was `t_disp` in free-flow case) | `disp` ✅ (was `t_disp`) | Derived stat only |
| **Krauss** | `disp` ✅ (already was) | `disp` ✅ (was `t_disp`) | Derived stat only |
| **IDM** | `disp` ✅ (was `t_disp` everywhere) | `disp` ✅ (was `t_disp`) | Derived stat only |

All three models now use the identical pattern for leader snapshot:
```scala
val xl = if car_ahead.segId == car.segId then car_ahead.disp
         else length + car_ahead.disp       // next segment
```

`t_disp` is still updated after each step as `car.t_disp += new_disp - car.disp`
for logging/statistics, but is never read during physics computation.

**What this solves:**
- ✅ P0: Ramp→mainline `t_disp` discontinuity — eliminated (no rebase needed)
- ✅ P5: Stale `segId < car.segId` comparison in IDM — eliminated (uses `segId ==` like Krauss)
- ✅ Consistent coordinate system across all three models

**Files changed:** `Dynamics.scala` (GippsDynamics.updateM, GippsDynamics.gipps,
KraussDynamics.updateM, IDMDynamics.updateM, IDMDynamics.iDM convenience method)  
**Effort:** ~60 lines changed  
**Compile:** ✅ zero errors

---

### P1 — Dual-Leader `findLeader` for Ramp Vehicles

**The problem in detail:**

`findLeader()` (Dynamics.scala:58–72):

```scala
protected def findLeader(car: Vehicle): Vehicle =
    val ref = car.myPathNode.ahead          // Step 1: within-DLL
    if ref != null then return ref.elem
    val pw = car.myPathway                  // Step 2: cross-boundary
    if pw != null then ...                  // pw is NULL on ramps!
    null                                     // → free-flow
```

When a ramp vehicle is the head of its ramp DLL, `myPathNode.ahead == null` and
`myPathway == null`.  `findLeader` returns `null`.  The car-following model treats
it as free-flow: the vehicle accelerates to `vmax` on the ramp.

Real drivers on an acceleration lane look at **both** ramp traffic ahead AND mainline
traffic at the merge point.

**Fix — Dual-leader approach (Treiber & Kesting 2013, §11.3):**

1. Add to `Ramp.scala` two fields set at construction time:
   ```
   var targetPathway: Pathway = null   // mainline lane at merge
   var targetSegId: Int = -1            // segment index at merge
   ```
   Set these in the model when creating ramps (we already know `joinSegs`).

2. Extend `findLeader` with a third step:
   ```
   // Step 3: ramp → peek at mainline merge target
   val ramp = car.myRamp
   if ramp != null && ramp.targetPathway != null then
       val mainlineSeg = ramp.targetPathway.seg(ramp.targetSegId)
       if mainlineSeg != null then return mainlineSeg.getLast
   end if
   ```

3. The car-following model now decelerates on the ramp to match mainline speed
   before arriving at the merge point.

**Files:** `Dynamics.scala`, `Ramp.scala`, `EatonFireModel.scala`, `CalRoute101_3.scala`  
**Effort:** ~25 lines  
**Depends on:** P0  
**Ref:** Treiber & Kesting 2013 §11.3 — "On-Ramp Model"

---

### P2 — Gap Acceptance at Merge (MOBIL Safety Criterion)

**The problem in detail:**

```scala
// Current code — unconditional insertion:
val carAhead = route.pathway(laneID).seg(joinSeg).getLast
route.pathway(laneID).addToAlist(this, carAhead, joinSeg)
```

No check that there's room.  Vehicles can overlap.

**Fix — `Route.mergeFromRamp()` with gap check:**

New method in `Route.scala`:

```
def mergeFromRamp(targetLane: Int, seg: Int, car: Vehicle): Boolean
```

1. Query the target lane's DLL at `seg` for `vAhead` and `vBehind`
2. Compute `leadGap = vAhead.disp - car.disp - Vehicle.len`
3. Compute `lagGap  = car.disp - vBehind.disp - Vehicle.len`
4. If both gaps ≥ `safeGap = Vehicle.s + car.velocity * Vehicle.T`:
   → insert and return `true`
5. If gap insufficient:
   → try adjacent lane (`targetLane - 1`) if it exists at `seg`
   → if still no gap, vehicle waits (yield, re-check next tick)
6. After timeout (~10 s sim time) → force-merge (cooperative deceleration)

MOBIL safety criterion (Kesting et al. 2007):
```
ã_follower(after merge) ≥ -b_safe
```
The new follower in the target lane must not need to brake harder than `b_safe`.

**Files:** `Route.scala`, `EatonFireModel.scala`, `CalRoute101_3.scala`  
**Effort:** ~40 lines  
**Depends on:** P1 (leader lookup must work on ramp for vehicle to slow down while waiting)  
**Ref:** Kesting, Treiber, Helbing 2007 — MOBIL

---

### P3 — Post-Merge Relaxation

**The problem in detail:**

After merging, the vehicle's accepted headway may be shorter than the IDM equilibrium
headway.  Without relaxation, the first timestep after merge produces a large
deceleration as IDM tries to restore its desired gap — jarring and unrealistic.

**Fix — Time-varying desired headway (Leclercq et al. 2007):**

```
T(t) = T_min + (T_eq - T_min) · (1 - e^{-(t - t_merge) / τ_relax})
```

- `T_min ≈ 0.6 * T` — accepted headway at merge instant
- `T_eq  = Vehicle.T` — normal safe time headway
- `τ_relax ≈ 20 s` — relaxation time constant

Implementation: add `var mergeRelaxCountdown: Int = 0` to `Vehicle.scala`.
In `IDMDynamics.updateM`, if `countdown > 0`, use `T_relax` instead of `T`.

**Files:** `Vehicle.scala`, `Dynamics.scala` (`idmAccel`)  
**Effort:** ~15 lines  
**Depends on:** P2  
**Ref:** Leclercq et al. 2007; Laval & Leclercq 2008

---

### P4 — Animation Interpolation at Merge

**The problem in detail:**

The car token visually teleports from the ramp endpoint to the mainline segment
start.  No smooth transition.

**Fix:**

After `driveRamp()` returns and before `addToAlist` on the mainline, insert a
single `director.animate(actor, MoveToken, ...)` call using the junction's
coordinates as the intermediate position.  This mirrors what `Junction.jump()`
does internally.

**Files:** `EatonFireModel.scala`, `CalRoute101_3.scala`  
**Effort:** ~5 lines  
**Depends on:** P0

---

### P5 — `findLeader` Stale-Check Cleanup ✅ SOLVED BY P0

The old `car_ahead.segId < car.segId` and `car_ahead.t_disp - car.t_disp > FREERANGE`
checks are gone.  IDM now uses the same `segId ==` pattern as Krauss/Gipps, which is
correct for per-segment DLLs where the leader is always same-seg or next-seg.

---

## Phase B: Density-Based Dynamic Lane Assignment

### Core Idea

> At each junction (shared cross-section), sense the **density of the downstream
> segment** across all lanes.  Compare densities.  If an adjacent lane (±1) has
> meaningfully lower density and the lane change is safe, execute it.  If adjacent
> lanes are equally dense or denser, stay.  This is **junction-level DTA** — the
> junction is the decision engine, the VTransport is the data provider.

This replaces CalRoute101_3's "if car ahead is slow → change lane" with a
**data-driven, density-aware** decision that:

- Does not require per-lane PeMS data (which we don't have)
- Uses only what the simulation already computes: `VTransport.snapshotDensity()`
- Respects physics: only ±1 lane change, with gap-based safety check
- Is self-regulating: as vehicles spread across lanes, density equalizes,
  lane changes stop naturally

### D0 — `Route.downstreamDensity(seg: Int): Array[Double]`

New method in `Route.scala`:

```scala
/** Return density of segment `seg` for each lane.
 *  Returns Double.MaxValue for lanes that don't exist at this segment.
 */
def downstreamDensity(seg: Int): Array[Double] =
    Array.tabulate(numLanes) { lane =>
        if laneExistsAt(lane, seg) then pathway(lane).seg(seg).snapshotDensity()
        else Double.MaxValue
    }
```

This is the **sensor function**.  It queries the existing `snapshotDensity()` that
each VTransport already provides (`vdeque.size / length`).

**Files:** `Route.scala`  
**Effort:** ~8 lines  
**Depends on:** Nothing (uses existing infrastructure)

---

### D1 — `Route.densityBasedLaneChoice(currentLane: Int, seg: Int, car: Vehicle): Int`

New method in `Route.scala`.  The **decision engine**:

```
1. density[] = downstreamDensity(seg + 1)     // look AHEAD to next segment
2. myDensity = density[currentLane]
3. candidates = []
4. if currentLane - 1 ≥ 0 AND laneExistsAt(currentLane - 1, seg):
       if density[currentLane - 1] < myDensity - Δk_min:
           candidates += (currentLane - 1, density[currentLane - 1])
5. if currentLane + 1 < numLanes AND laneExistsAt(currentLane + 1, seg):
       if density[currentLane + 1] < myDensity - Δk_min:
           candidates += (currentLane + 1, density[currentLane + 1])
6. if candidates.isEmpty: return currentLane   // no improvement → stay
7. target = candidate with lowest density
8. if changeLane(currentLane, target, car, seg):   // safety check (existing)
       return target
   else:
       return currentLane                       // safety failed → stay
```

**Parameters:**
- `Δk_min` — minimum density differential to trigger a change (e.g., 0.005 veh/m).
  Prevents oscillation when densities are nearly equal.
- Adjacency enforced: only ±1 lane change per segment.
- Safety enforced: delegates to existing `Route.changeLane()` which checks
  `safetydist` gaps via DLL inspection.

**Why density, not speed:**

Speed-based heuristics ("car ahead is slow") are reactive and local — they see
one car.  Density-based sensing sees the **aggregate state of the segment**.  A lane
can have a slow lead car but low density (gap forming ahead), or high density but
moderate speed (platoon).  Density is the macroscopic variable that directly governs
level-of-service (LOS), making it the correct input for lane assignment.

**Variable-lane awareness:**

EatonFireModel has lanes that appear/disappear at different segments.
`laneExistsAt()` and `lanesAt()` already handle this.  The density method returns
`Double.MaxValue` for non-existent lanes, so they are never selected.

**Files:** `Route.scala`  
**Effort:** ~30 lines  
**Depends on:** D0

---

### D2 — Wire into `driveHighway` Loop

Insert the density-based lane choice into the segment loop, **before the DLL hop**
(after `move()` and `jump()`):

```
EatonFireModel.driveHighway:

while seg < hwLen && !diverted do
    route.pathway(laneID).seg(seg).move()
    junc(seg + 1).jump()

    // ── FF diversion (existing) ──
    ...

    // ── NEW: Density-based lane choice at this junction ──
    if !diverted && seg + 1 < hwLen then
        val newLane = route.densityBasedLaneChoice(laneID, seg, this)
        if newLane != laneID then laneID = newLane
    end if

    // ── DLL hop (existing) ──
    ...
end while
```

**Timing rationale:** The lane-change decision is made **after** the vehicle has
traversed the current segment (`move()`) and **after** the junction tallies
(`jump()`), but **before** the DLL hop to the next segment.  This means:

1. The vehicle has current velocity/position from car-following on this segment
2. The junction has recorded density data for this timestep
3. The DLL hop transfers the vehicle to the (possibly new) lane's next segment

The existing DLL hop code already handles the transfer:
```scala
route.pathway(laneID).seg(seg).removeFromAlist(this)    // old lane
val nextVT = route.pathway(laneID).seg(seg + 1)         // uses updated laneID
nextVT.addToAlist(this, ahead)
```

Wait — `Route.changeLane()` already does the DLL transfer internally.  So if
`densityBasedLaneChoice` succeeds, the vehicle is already in the new lane's DLL
at `seg`.  The DLL hop then moves it to `seg + 1` of the new lane.  This is correct.

**Files:** `EatonFireModel.scala`, `CalRoute101_3.scala`  
**Effort:** ~6 lines per model  
**Depends on:** D1, Phase A (P0–P2 minimum)

---

### D3 — Configurable Parameters

Add to `DynamicsConfig.scala` or a new `LaneChangeConfig` case class:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `densityThreshold` | 0.005 veh/m | Minimum Δk to trigger lane change |
| `minSegInterval` | 2 | Don't change again within N segments of last change |
| `lookaheadSegs` | 1 | How many downstream segments to average density over |

These are threaded through to `Route.densityBasedLaneChoice` at construction.

**Files:** `DynamicsConfig.scala` or new `LaneChangeConfig.scala`, `Route.scala`  
**Effort:** ~20 lines  
**Depends on:** D1

---

## Dependency Graph & Implementation Order

```
Phase A (Ramp Physics):

  P0 (t_disp rebase)
   ├──→ P1 (dual findLeader)
   │     └──→ P2 (gap acceptance)
   │           └──→ P3 (relaxation)
   ├──→ P5 (stale-check cleanup)
   └──→ P4 (animation)

Phase B (Lane Assignment):

  D0 (downstreamDensity)
   └──→ D1 (densityBasedLaneChoice)
         ├──→ D2 (wire into driveHighway)    ← requires Phase A P0–P2
         └──→ D3 (config parameters)
```

**Recommended order:**

```
P0 ✅ → P5 ✅ → P1 → P2 → D0 → D1 → D2 → P3 → P4 → D3
```

P0 and P5 are done (correct segment-local coordinates).
P1 and P2 fix merge physics.
D0–D2 enable density-based lane assignment.
P3, P4, D3 are refinements.

---

## Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Ramp target info | Fields on `Ramp` (`targetPathway`, `targetSegId`) | Cleaner than Vehicle fields; ramp knows its merge point |
| Gap acceptance fallback | Timeout + reduced gap (not full MOBIL) | Simpler; upgrade to MOBIL later if needed |
| Lane change signal | Downstream segment density | Not speed-based; density is macroscopic LOS indicator |
| Density scope | Single lookahead (`seg + 1`) | Start simple; parameterize to multi-segment average later |
| Lane change constraint | ±1 only, with existing `changeLane` safety | Physics-correct; no teleporting across lanes |
| Density for missing lanes | `Double.MaxValue` | Never selected; variable-lane safe |

---

## Literature References

| Ref | Used In | Citation |
|-----|---------|----------|
| Treiber & Kesting 2013 | P0, P1 | *Traffic Flow Dynamics*, Springer, §11.1–11.3 |
| Kesting et al. 2007 | P2 | "General lane-changing model MOBIL", *TRR* 1999(1), 86–94 |
| Leclercq et al. 2007 | P3 | "Relaxation phenomenon after lane changing", *T&TT* |
| Laval & Leclercq 2008 | P3 | "Microscopic modeling of the relaxation phenomenon", *TRB* 42(6) |
| Ahmed 1999 | P2 | *Modeling Drivers' Acceleration and Lane Changing*, MIT PhD |
| Hidas 2005 | P2 | "Modelling vehicle interactions in merging", *TRC* 13(1) |

---

## Acceptance Criteria

- [ ] P0: After merge, `t_disp` matches `route.toCumulative(joinSeg, 0.0)` — verify with print
- [ ] P1: Ramp vehicles decelerate before merge point — verify velocity trace
- [ ] P2: No vehicle overlap at merge — verify `leadGap ≥ 0` invariant
- [ ] P3: Post-merge deceleration is smooth — verify velocity curve
- [ ] P4: No visual teleportation at merge — verify animation
- [ ] P5: No stale-leader artifacts after lane change — verify IDM gap
- [ ] D0: `downstreamDensity` returns correct values — unit test
- [ ] D1: Lane choice moves vehicles toward lower-density lanes — verify counts
- [ ] D2: Outermost lane density drops within 3 segments of merge — verify density profile
- [ ] D3: Parameters are configurable without recompilation

---

## Session Notes

Created: 2026-03-31  
This document captures the user's core insight: **the junction is the decision
engine for lane assignment, using density as the sensing signal**.  This is a
microscopic form of DTA — each junction acts as a local controller that balances
lane utilization based on real-time segment state.

