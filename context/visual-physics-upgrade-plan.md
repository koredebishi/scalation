# Visual & Physics Upgrade Plan — Implementation Spec

**Created:** 2026-04-13  
**Status:** PENDING APPROVAL — No code will be written until you say "go"  
**Scope:** 3 phases, 10 tasks, 6 files touched

---

## Phase 1: Physics Fixes (Make Cars Stop Overlapping)

### P0: Hard Minimum Gap Enforcement in IDM
- **File:** `Dynamics.scala` — `idmAccel()` method (~line 420)
- **What changes:**
  - **Before:** `if a < -b then a = -b` (comfortable decel clamp, `b = 2.0 m/s²`)
  - **After:** `if a < b_emergency then a = b_emergency` (emergency braking, `b_emergency = -9.0 m/s²`)
- **Why:** IDM currently can't brake hard enough to prevent overlap. The comfortable clamp means if a vehicle needs -5 m/s² to avoid collision, IDM caps it at -2.0 m/s², the position clamp fires every tick, and you get `gap = -4.0` (cars inside each other).
- **Lines changed:** ~3
- **Risk:** LOW — `b_emergency` is already declared in `Vehicle.def_prop`. Gipps/Krauss position clamps already use `s` correctly.
- **Owner:** Bishi's code (recent IDM rewrite)
- **Depends on:** Nothing
- **Test:** Run EatonFireModel, grep for `gap < 0` in STEP7 output → should be zero

### P1: Enable Lane Change on Mainline
- **File:** `EatonFireModel.scala` — `driveHighway()` (~line 400)
- **What changes:**
  - **Before:** Lane change block is commented out (lines 402–408)
  - **After:** Uncomment + add trigger: if a car has a leader AND leader velocity < 50% of `vmax` AND adjacent lane has a larger gap, attempt `route.changeLane(laneID, target, this, seg)`
  - ±1 lane constraint (already in `Route.changeLane`)
  - No lane change at ramp merge segments (prevent conflicts)
  - Hysteresis: don't change back for 3 segments (prevent oscillation)
- **Lines changed:** ~15–20
- **Risk:** MEDIUM — lane changes interact with DLL ordering. `Route.changeLane` already handles atomic remove+insert. Main risk is oscillation (mitigated by hysteresis).
- **Owner:** Bishi's code
- **Depends on:** P0 (gap enforcement must work first)
- **Test:** Run simulation → vehicles should spread across all 5 lanes. Per-lane throughput should be balanced within 20%.

### P2: Gap Acceptance at Ramp Merge
- **File:** `EatonFireModel.scala` — `actOnCorridor()`, ramp branch (~line 384)
- **What changes:**
  - **Before:** Blind `addToAlist(this, carAhead, joinSeg)` — unconditional merge
  - **After:** Check mainline gap before merging:
    1. Get `ahead` and compute gap in target lane at `joinSeg`
    2. If `gap < Vehicle.s * 3` (conservative safe gap), **wait**: `schedule(Vehicle.rt); yieldToDirector(); retry`
    3. Max 20 retries (10 seconds), then force-merge (with position clamp from P0 protecting)
- **Lines changed:** ~15–20
- **Risk:** MEDIUM — retry loop must be bounded to prevent deadlock. Vehicles waiting on ramp must stay in ramp DLL so following ramp cars see them.
- **Owner:** Bishi's code
- **Depends on:** P0
- **Test:** No `gap < 0` at merge points. Ramp throughput ≥ 90% of current.

---

## Phase 2: Road Surface Rendering (Make It Look Like a Road)

### R1: Road Polygon (Filled Pavement Surface)
- **File:** `DgAnimator.scala` — `Canvas.paintComponent()` (~line 557)
- **What changes:**
  - **Before:** Each lane is a 10px-wide stroke drawn individually → road looks like spaghetti
  - **After:** Insert a **Layer 0** before existing Layer 1. For each group of edges sharing the same from→to nodes (= lanes of one road segment), compute a filled `Path2D` polygon covering the full road width:
    - Left boundary = leftmost edge path offset by half `pavementStroke` width
    - Right boundary = rightmost edge path offset by half `pavementStroke` width
    - Fill with `pavementColor` (already `Color(50, 50, 58)`)
  - **Edge grouping:** Build a `Map[String, List[Edge]]` keyed on `"${from.label}→${to.label}"` when edges are created. No change to `Dgraph.scala`.
- **Lines changed:** ~25–30 new lines
- **Risk:** MEDIUM — requires identifying which edges belong to the same road segment. Edge labels follow pattern `L{lane}s{seg}` which is parseable.
- **Owner:** Bishi's code (all rendering layers are Bishi additions)
- **Depends on:** Nothing
- **Test:** Visual — road appears as solid dark surface, not individual lines

### R2: Proper Lane Markings
- **File:** `DgAnimator.scala` — `Canvas.paintComponent()` (~line 569, Layer 2)
- **What changes:**
  - **Before:** All edges get dashed stroke (same treatment for edge lanes and interior lanes)
  - **After:** 
    - Outermost edges in each segment group → **solid white** edge line
    - Interior edges → **dashed white** lane divider (existing `dashStroke`)
  - Requires same edge-grouping as R1
- **Lines changed:** ~15 (modify existing Layer 2 loop)
- **Risk:** LOW
- **Owner:** Bishi's code
- **Depends on:** R1 (uses same edge-grouping)

### R3: Vehicle Shape — Oriented Rectangles
- **File:** `DgAnimator.scala` — token rendering in `paintComponent()` (~line 590)
- **Current state:** `VSource.scala` already creates tokens as `RoundRectangle2D.Double(0, 0, 14, 7, 4, 4)` — already car-shaped! The 8×8 Ellipse is only used by old non-traffic models.
- **What changes:** Add heading-based rotation when drawing vehicle tokens:
  - **Approach A (preferred):** In `paintComponent`, for each token, compute heading from consecutive frame positions (store previous xy per token eid in a small HashMap). Apply `AffineTransform.rotate(heading, cx, cy)` before `g2d.fill(token.shape)`. **No change to Animator.scala or AnimateCommand protocol.**
  - **Approach B (cleaner but riskier):** Pass heading as `pts(2)` in MoveToken commands from `VTransport.move()`. Changes Dr. Miller's animation protocol.
  - **Recommendation:** Approach A — self-contained in `DgAnimator`, no protocol changes.
- **Lines changed:** ~20 in `DgAnimator`
- **Risk:** LOW (Approach A) / MEDIUM (Approach B)
- **Owner:** Bishi's code
- **Depends on:** Nothing (independent)

### R4: Ramp Surface (Tapered Polygon)
- **File:** `DgAnimator.scala` — `paintComponent()`
- **What changes:** Identify ramp edges (label contains `"OR"` or `"FR"`). Draw tapered polygon: wider at VSource/Sink end, narrowing to merge junction. Fill with same `pavementColor`.
- **Lines changed:** ~15–20
- **Risk:** LOW — purely additive rendering
- **Owner:** Bishi's code
- **Depends on:** R1 (layering order — ramp drawn before mainline polygon)

---

## Phase 3: Polish

### M1: Smooth Merge Animation
- **File:** `EatonFireModel.scala` — `actOnCorridor()` ramp branch
- **What changes:** After ramp vehicle merges into mainline DLL, emit 3–5 interpolated `MoveToken` commands between ramp exit position and mainline entry position. Prevents visual teleport.
- **Lines changed:** ~10–15
- **Risk:** LOW — animation only, no physics
- **Depends on:** P2 (merge logic), R3 (rotation makes interpolation smooth)

### R5: Road Shoulders
- **File:** `DgAnimator.scala`
- **What changes:** After road polygon (R1), draw 3–5px darker strips on each side using outermost edge paths offset outward.
- **Lines changed:** ~10
- **Risk:** VERY LOW
- **Depends on:** R1

### Labels: Freeway Shields & Exit Numbers
- **File:** `DgAnimator.scala`
- **What changes:** Draw shield-shaped badges at corridor start nodes ("I-210" green shield, "SR-134" green shield). Draw exit numbers at off-ramp diverge junctions. Pure overlay graphics.
- **Lines changed:** ~20
- **Risk:** VERY LOW
- **Depends on:** Nothing

---

## Dependency Graph

```
         ┌─── P0 (gap clamp) ───→ P1 (lane change) ───→ P2 (gap acceptance)
         │                                                      │
         │                                                      ▼
PARALLEL │                                               M1 (smooth merge)
         │
         │    R1 (road polygon) ──→ R2 (lane markings)
         │         │
         │         ├──→ R4 (ramp surface)
         │         └──→ R5 (shoulders)
         │
         │    R3 (vehicle rotation) ─── independent
         │
         └─── Labels (shields) ────── independent
```

**Implementation order (recommended):**

| Week | Tasks | Why |
|------|-------|-----|
| 1 | P0 + R1 | Fix physics foundation + draw road surface (parallel, independent) |
| 2 | P1 + R2 + R3 | Enable lane change + lane markings + vehicle rotation |
| 3 | P2 + R4 + R5 | Gap acceptance + ramp surface + shoulders |
| 4 | M1 + Labels | Polish merge animation + freeway shields |

---

## Files Touched Summary

| File | Author | Tasks | Lines Changed |
|------|--------|-------|---------------|
| `Dynamics.scala` | Miller/Casey → Bishi | P0 | ~3 |
| `EatonFireModel.scala` | Bishi | P1, P2, M1 | ~50 |
| `Route.scala` | Miller → Bishi | P1 (review only) | 0 |
| `DgAnimator.scala` | Miller → Bishi | R1, R2, R3, R4, R5, Labels | ~120 |
| `VSource.scala` | Miller → Bishi | — | 0 (already car-shaped) |
| `VTransport.scala` | Miller/Casey → Bishi | — | 0 (Approach A) |
| `Animator.scala` | Miller | — | 0 (Approach A) |

**Total estimated new/changed lines:** ~175

---

## Open Design Questions (Need Your Decision)

1. **R3 rotation approach:** Approach A (compute heading from position delta, self-contained in DgAnimator) vs Approach B (pass heading through AnimateCommand, cleaner but touches Dr. Miller's Animator). **Recommendation: A**

2. **P1 lane-change trigger:** Simple "leader is slow" trigger (fast to implement, may need tuning) vs full density-based sensing from `ramp-physics-and-density-lane-assignment.md` (correct design, more code). **Recommendation: Simple trigger first, upgrade later**

3. **R1 edge-grouping strategy:** Parse edge labels (`L{lane}s{seg}`) to group edges by segment vs add explicit segment metadata to DgAnimator. **Recommendation: Parse labels — zero changes to Dgraph.scala**

---

## What I Will NOT Touch

- `Dgraph.scala` — Dr. Miller's graph data structure
- `Animator.scala` — Dr. Miller's animation command processor (with Approach A)
- `AnimateCommand.scala` — command protocol
- `Model.scala` — simulation engine core
- `Pathway.scala` — pathway structure
- `CorridorBuilder.scala` — topology builder
- `EatonCorridorConfig.scala` — PeMS data pipeline

---

**Status: Awaiting your review. Tell me which tasks to proceed with, any changes to the approach, or questions about specific decisions.**

