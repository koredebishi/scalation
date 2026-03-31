# Graph-Derived Ramp Positioning

**Created:** 2026-03-31  
**Status:** 🟡 Design Complete — Ready to Implement  
**Affects:** CorridorBuilder, Route, EatonFireModel, CalRoute101_3 (optional)

---

## Problem

Ramp positions are computed **independently** from mainline lane geometry using a magic pixel shift (`rampShift`). This means:

- On a 5-lane section, ramps float near lane 2 instead of hugging the edge
- On a 3-lane section, ramps are positioned further from the edge than they should be
- `rampShift`, `nudge`, and sink offsets are educated guesses, not derived from data
- Every lane-count change requires re-tuning pixel constants

**Current flow (disconnected):**
```
EatonCorridorConfig:  GPS → screen → add rampShift(15, -18) → store in CorridorLayout
CorridorBuilder:      reads onRampScreenXY/offRampScreenXY → creates Junction at those coords
Route:                independently computes lane positions via calcShift2 * GAP
Result:               ramp and lane edge are unrelated pixel positions
```

## Solution

**Ramp positions are derived from Route lane geometry after the Route is built.** The Route already knows exactly where each lane edge is at every segment. A ramp connects to the outermost lane edge at its join segment.

**New flow (graph-derived):**
```
CorridorBuilder:
  1. Build junctions (same as now)
  2. Build Route (same as now) — establishes lane geometry
  3. NEW: Query Route for ramp attach points at each joinSeg
  4. Create ramp sensor junctions at those derived positions
  5. Create VSource/Sink extending outward from attach point
```

---

## Key Insight: Route Already Knows Lane Edge Positions

```scala
// Route.scala line 64-65 — lane shift computation
val physicalLane = numLanes - 1 - i
val shift = calcShift2 * ((physicalLane - (numLanes - 1) / 2.0) * GAP)
```

The outermost lane (lane 0) is at:
```
centerline + calcShift2 * ((numLanes - 1) / 2.0) * GAP
```

The lane edge (where a ramp should connect) is half a GAP further:
```
centerline + calcShift2 * ((numLanes - 1) / 2.0 + 0.5) * GAP
```

For variable lanes, `numLanes` at a given segment = `lanesAt(seg)`. So the edge position adapts automatically.

---

## Architecture

### New Method: `Route.rampAttachPoint`

```
Route.rampAttachPoint(seg: Int, side: Side = Right): (Double, Double)

  1. junction = points(seg)  // the junction at this segment boundary
  2. nLanesHere = lanesAt(seg)
  3. perpVec = calcShift2    // unit vector perpendicular to road
  4. edgeOffset = ((nLanesHere - 1) / 2.0 + 0.5) * GAP
  5. attachXY = (junction.at(0) + perpVec(0) * edgeOffset,
                 junction.at(1) + perpVec(1) * edgeOffset)
  6. return attachXY
```

This automatically adapts to the lane count at each segment:
```
5-lane section → edge at 2.5 * GAP = 125 px from centerline
4-lane section → edge at 2.0 * GAP = 100 px from centerline
3-lane section → edge at 1.5 * GAP =  75 px from centerline
2-lane section → edge at 1.0 * GAP =  50 px from centerline
```

### Outward Direction for VSource/Sink

The VSource (on-ramp) or Sink (off-ramp) is placed further outward along the same perpendicular:

```
outwardXY = (attachXY._1 + perpVec(0) * RAMP_LENGTH,
             attachXY._2 + perpVec(1) * RAMP_LENGTH)
```

Where `RAMP_LENGTH = 150.0` px (configurable) — the visual length of the ramp VTransport line.

---

## What Gets Eliminated

| Removed | Was In | Why |
|---|---|---|
| `rampShift` parameter | `EatonCorridorConfig`, `buildLayoutFromCoords`, `buildSharedWBLayouts` | Replaced by `rampAttachPoint` |
| `onRampScreenXY` field | `CorridorLayout` | Computed at build time from Route geometry |
| `offRampScreenXY` field | `CorridorLayout` | Computed at build time from Route geometry |
| `nudge` / collision detection | `EatonCorridorConfig` step 12 | Unnecessary — different `joinSeg` = different attach point |
| Off-ramp sink offset `(+200, -100)` | `CorridorBuilder` | Replaced by outward vector × RAMP_LENGTH |
| `getVSourceCenterAndOffsets` | `CorridorLayout`, `NetworkConfig` | Replaced by Route-derived positions |

---

## Task Breakdown

### Task 0: Add `rampAttachPoint` to Route ✅ (pure addition)
**File:** `Route.scala`

Add method that returns the screen (x, y) at the outermost lane edge of a given segment:

```scala
def rampAttachPoint (seg: Int): (Double, Double) =
    val juncPt  = pathway(0).points(seg)   // junction at segment boundary
    val nHere   = lanesAt(seg)
    val perp    = calcShift2               // perpendicular unit vector
    val offset  = ((nHere - 1) / 2.0 + 0.5) * GAP
    (juncPt.at(0) + perp(0) * offset,
     juncPt.at(1) + perp(1) * offset)
end rampAttachPoint
```

Also expose the perpendicular vector:

```scala
def rampOutwardVec: (Double, Double) = (calcShift2(0), calcShift2(1))
```

**Test:** Call `route.rampAttachPoint(5)` after building — verify (x, y) is at lane edge.

---

### Task 1: CorridorBuilder creates ramp junctions from Route geometry ✅ (modify)
**File:** `CorridorBuilder.scala`

Move ramp junction creation to **after** Route is built (currently Steps 2/7 happen before Step 3).

**New order:**
1. Build mainline junctions (same)
2. Build Route (same — Step 3)
3. **NEW:** Compute ramp positions from Route
4. Create ramp sensor junctions at derived positions
5. Create ramp sinks at outward positions

```scala
// After Route is built:
val RAMP_LEN = 150.0   // visual ramp VTransport length (px)
val outward  = route.rampOutwardVec

// On-ramp sensor junctions (road-edge end of ramp)
cfor (0, nOnRamps) { i =>
    val seg = rampJoinSegs(i)
    val (ax, ay) = route.rampAttachPoint(seg)
    rampSensors(i) = new Junction (s"", xy = (ax, ay), nt = nt, nl = nLanes)
}

// Off-ramp sensor junctions
cfor (0, nOffRamps) { r =>
    val seg = offRampJoinSegs(r)
    val (ax, ay) = route.rampAttachPoint(seg)
    offRampSensors(r) = new Junction (s"", xy = (ax, ay), nt = nt, nl = nLanes)
}

// Off-ramp sinks (extend outward from attach point)
cfor (0, nOffRamps) { r =>
    val (ax, ay) = (offRampSensors(r).at(0), offRampSensors(r).at(1))
    offRampSinks(r) = new Sink (s"", (ax + outward._1 * RAMP_LEN,
                                       ay + outward._2 * RAMP_LEN))
}
```

**Dependency:** Route must be built first → ramp junctions come after. Reorder Steps 2, 3 within `build()`.

---

### Task 2: On-ramp VSource positions derived from Route ✅ (modify)
**File:** `EatonFireModel.scala`

Replace `getVSourceCenterAndOffsets` + hardcoded offsets with Route-derived positions:

```scala
// For each ramp VSource: position = attach point + outward * RAMP_LEN
val outward210 = route210.rampOutwardVec
cfor (0, nOnRamps210) { r =>
    val seg = rampJoinSeg210(r)
    val (ax, ay) = route210.rampAttachPoint(seg)
    val loc = Array(ax + outward210._1 * RAMP_LEN,
                    ay + outward210._2 * RAMP_LEN, 20.0, 20.0)
    // ... create VSource at loc
}
```

This eliminates `getVSourceCenterAndOffsets`, `center210`, `offsets210`, `center134`, `offsets134`.

---

### Task 3: Remove rampShift from EatonCorridorConfig ✅ (cleanup)
**File:** `EatonCorridorConfig.scala`

- Remove `rampShift` parameter from `buildCorridorLayout` and `buildSharedWBLayouts`
- Remove steps 11 and 12 (on-ramp / off-ramp screen coordinate computation)
- Remove `onRampScreenXY` and `offRampScreenXY` from `CorridorLayout`
- Keep `rampJoinSegs` in config (needed by builder to know which segment each ramp joins)

**Alternative (backward compat):** Keep `onRampScreenXY`/`offRampScreenXY` in `CorridorLayout` but fill them with dummy values. CorridorBuilder ignores them and uses Route-derived positions instead. Less invasive.

---

### Task 4: Mainline VSource positions derived from Route ✅ (optional cleanup)
**File:** `EatonFireModel.scala`

The mainline VSource positioning (lines 210-231) already uses `junc210(0).at` + perpendicular math — which is essentially the same computation as `rampAttachPoint`. Can be simplified to:

```scala
// upstream of junc(0), aligned per lane
val (ax, ay) = route210.rampAttachPoint(0)  // or a new lanePoint(seg, lane)
```

Lower priority — the current code works, just duplicates the math.

---

### Task 5: Compile + visual verification ✅
- `sbt compile` — zero errors
- Run EatonFireModel synthetic — verify:
  - All ramps hug the outermost lane edge
  - Ramps on 5-lane sections are further from centerline than on 4-lane sections
  - On-ramp and off-ramp VTransport lines are same length
  - No overlap between ramps at shared interchanges

---

## Execution Order

```
Task 0 → Task 1 → Task 2 → Task 3 → Task 5
                                ↑
                          Task 4 (optional)
```

Tasks 0-1 are the core change. Task 2 brings EatonFireModel in line. Task 3 is cleanup. Task 4 is polish.

---

## Risk Assessment

| Risk | Mitigation |
|---|---|
| Route not built when ramp junctions needed | Reorder Steps in `build()` — Route before ramp junctions |
| `calcShift2` is private in Route | Make it package-private or expose via `rampOutwardVec` |
| `pathway(0).points(seg)` may be null (sparse) | Use junction array directly (always non-null) |
| CalRoute101_3 uses `NetworkConfig.getRampCoordinates` | Leave as-is (separate code path), or migrate later |
| Off-ramp nudge for same-PM OR/FR | Eliminated — different `joinSeg` = different attach point automatically. Same-seg ramps get same attach point (acceptable — they're the same interchange) |

---

## Files Modified

| File | Change | Task |
|---|---|---|
| `Route.scala` | Add `rampAttachPoint`, `rampOutwardVec` | 0 |
| `CorridorBuilder.scala` | Reorder steps, derive ramp positions from Route | 1 |
| `EatonFireModel.scala` | Replace offset-based VSource positions with Route-derived | 2 |
| `EatonCorridorConfig.scala` | Remove `rampShift`, simplify or remove `onRampScreenXY`/`offRampScreenXY` | 3 |

## Files NOT Modified

| File | Reason |
|---|---|
| `CalRoute101_3.scala` | Uses `NetworkConfig.getRampCoordinates` — separate path, not broken |
| `Ramp.scala` | No change — VTransport endpoints come from from/to Components |
| `Pathway.scala` | No change |
| `VTransport.scala` | No change |

