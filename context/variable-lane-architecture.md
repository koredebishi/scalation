# Variable Lane Count Architecture Problem

**Created:** 2026-03-30  
**Status:** 🔴 Architectural Constraint Identified — Not Yet Implemented  
**Affects:** EatonFireModel (I-210 WB + SR-134 WB), any future corridor with non-uniform lanes

---

## Problem Statement

Real freeways change width. I-210 WB goes 5→6→4→2→4→5→4 lanes along its length. The simulation currently forces **one lane count for the entire corridor**, drawing phantom lanes where no road exists.

The data already contains per-sensor lane counts. The simulation engine cannot consume them.

---

## Evidence: I-210 WB Lane Profile (from `station_map.csv`)

Traffic flows high PM → low PM (Westbound entry = PM 29.879):

```
PM 29.879  ROSEMEAD 1              5 lanes  ← ENTRY
PM 29.17   SIERRA MADRE V1         5 lanes
PM 28.27   SAN GABRIEL             5 lanes
PM 28.03   ALTADENA                5 lanes
PM 26.8    HILL                    5 lanes
PM 26.12   LAKE 1                  5 lanes
PM 25.68   MARENGO                 6 lanes  ← WIDENS
PM 25.4    FAIR OAKS 1             4 lanes  ← NARROWS
PM 24.9    WALNUT                  2 lanes  ← SEVERE NARROWING
PM 24.442  WINONA WAY              4 lanes  ← FF DIVERGE POINT
PM 23.9    MOUNTAIN 1              5 lanes
PM 23.79   HAMMOND ST.             4 lanes
PM 23.0    LINCOLN 1               4 lanes
PM 22.3    ARROYO 1                4 lanes
PM 21.39   BERKSHIRE               4 lanes
PM 20.98   FOOTHILL                4 lanes
PM 20.4    GOULD                   4 lanes
PM 19.78   ANGELES CREST HWY NB   4 lanes
PM 19.58   ANGELES CREST HWY SB   4 lanes
PM 19.08   EB/NB 2 TO EB 210 CN   4 lanes
PM 18.32   EB/NB 2 TO WB 210 CN   4 lanes
PM 17.88   OCEAN VIEW              6 lanes  ← WIDENS AGAIN
PM 17.38   LA CRESCENTA            4 lanes
PM 16.58   PENNSYLVANIA            5 lanes
PM 15.28   HONOLULU                4 lanes
PM 14.18   LA TUNA CANYON NB       4 lanes
PM 13.98   LA TUNA CANYON SB       4 lanes  ← EXIT
```

SR-134 WB also varies: **5→4→4→5→4→4**.

---

## Root Cause: Three Classes Enforce Uniform Width

### 1. `MainlineSpec` — single integer for entire corridor

```
File: config/NetworkConfig.scala line 75
```
```scala
case class MainlineSpec (id: String, segments: Int, lanesPerSegment: Int, ...)
//                                                  ^^^^^^^^^^^^^^^^
//                                                  ONE number, not per-segment
```

### 2. `Route` — creates N identical full-length Pathways

```
File: process/Route.scala lines 35, 42
```
```scala
class Route (name: String, numLanes: Int, ...)
    val pathway = Array.ofDim[Pathway](numLanes)   // N pathways, ALL full-length
```

### 3. `Pathway` — every pathway spans ALL segments

```
File: process/Pathway.scala lines 44-45
```
```scala
val points = from +: junc.toList :+ to                    // ALL junctions
val seg = Array.ofDim[VTransport](points.length - 1)      // one VTransport per segment
```

So lane 5 exists at ROSEMEAD (5-lane road) **and also** at WALNUT (2-lane road). The simulation draws lanes where no road exists.

---

## Where the Data Is Already Available (But Discarded)

`EatonCorridorConfig.buildLayoutFromCoords()` already reads per-station lane counts:

```
File: config/EatonCorridorConfig.scala lines 372-375
```
```scala
val laneCounts = new Array[Int](nML)
cfor (0, nML) { i => laneCounts(i) = mlStations(i).record.lanes }  // ← HAS the data
val entryIdx = if flowDir == FlowDirection.Descending then nML - 1 else 0
val lanesPerSegment = laneCounts(entryIdx)                          // ← THROWS IT AWAY, picks one
```

The `laneCounts` array contains `[4,4,5,2,4,5,5,5,5,5,5,6,4,4,4,4,4,4,4,4,4,4,4,5,6,4,4]` — the full per-sensor lane profile. It is reduced to the single value `5`.

---

## Will OSM Fix This?

**No.** OpenStreetMap provides the same lane-count-per-segment information that `station_map.csv` already has. The bottleneck is not the data source — it is the simulation engine's Route/Pathway architecture.

---

## Two Candidate Fix Approaches

### Approach A: Per-Segment Lane Counts (Larger Refactor)

**Idea:** Change `MainlineSpec.lanesPerSegment` from `Int` to `Array[Int]`. Route creates Pathways that start/end at different segments.

**Changes required:**
- `MainlineSpec.lanesPerSegment` → `lanesPerSegment: Array[Int]` (or keep max + add array)
- `Route` creates partial-span Pathways: lane 5 only exists seg 0–5, not seg 0–26
- `Pathway` constructor takes `(startSeg, endSeg)` range, not all junctions
- `Car.act()` handles **mandatory lane change** when current lane is about to end
- VSource emits into lane subtypes that know which segments they cover
- `buildLayoutFromCoords` passes `laneCounts` array instead of single int

**Pros:** Physically correct model — lanes exist only where the road has them  
**Cons:** Touches Route, Pathway, VTransport, Car.act(), VSource — deep refactor of core ScalaTion process components

### Approach B: Max-Lanes + Lane Availability Mask (Simpler)

**Idea:** Keep `Route(numLanes = max(laneCounts))` but add a per-segment boolean mask saying which lanes are open.

**New data structure:**
```scala
// laneOpen(seg)(lane) = true if lane exists at that segment
val laneOpen: Array[Array[Boolean]]
```

**Changes required:**
- Add `laneOpen` mask to Route or CorridorLayout
- `Car.act()` checks mask before entering a segment — if current lane closes, forced lane change
- VSource only emits into lanes that exist at the entry segment
- Animation optionally hides closed-lane VTransports

**Pros:** Minimal core refactor — Route/Pathway unchanged, logic lives in Car.act()  
**Cons:** Phantom VTransport objects still exist (just unused); forced-lane-change logic needed either way

### Common to Both Approaches

Both need:
- Forced lane-change logic at narrowing points (mandatory merge)
- Decision about what happens at widening points (new lane opens — do cars spread?)
- `station_map.csv` `Lanes` column is already loaded — no new data source needed

---

## Key Files

| File | Role |
|------|------|
| `config/NetworkConfig.scala` | `MainlineSpec.lanesPerSegment: Int` — the single-int constraint |
| `config/EatonCorridorConfig.scala` | `buildLayoutFromCoords()` — reads `laneCounts` array, discards to one value |
| `process/Route.scala` | `Route(numLanes: Int)` — creates N full-length Pathways |
| `process/Pathway.scala` | `Pathway` spans all junctions — no partial-corridor support |
| `model/EatonFireModel.scala` | `Car.act()` — would need forced-lane-change at narrowing |
| `data/.../station_map.csv` | Source of truth — `Lanes` column per ML station |

---

## Decision Needed

- [ ] Which approach (A or B)?
- [ ] Priority relative to WSC 2026 deadline (April 5, 2026)?
- [ ] Does the 2-lane anomaly at WALNUT (PM 24.9) need investigation? (Could be a PeMS metadata error — 2 mainline lanes seems very low for I-210)
- [ ] Should this be deferred until after the end-to-end `synthetic=false` run works?

---

## Session Notes (2026-03-30)

### What was discussed
- User asked how OSM would be wired for the Eaton model
- Traced the full architecture: `station_map.csv` → `EatonCorridorConfig` → `CorridorBuilder` → `EatonFireModel`
- Identified that the real issue is **variable lane counts**, not missing data
- Confirmed OSM would NOT solve this — the data exists, the engine can't consume it
- Identified the three-class constraint chain: `MainlineSpec` → `Route` → `Pathway`
- Proposed two fix approaches (per-segment lanes vs. max-lanes + mask)
- Deep structural analysis: identified `Pathway.vList` (DLL) as the load-bearing constraint
- Discovered the **dual data structure redundancy**: `Pathway.vList` (DLL, lane-spanning) duplicates `VTransport.vdeque` (deque, segment-scoped)
- Proposed **DLL Unification**: move the DLL from Pathway into VTransport, unifying the two structures
- See **`context/variable-lane-dll-unification.md`** for full visual explainer

### What was NOT done
- No code was written or modified (read-only analysis per user instruction)
- No approach was selected
- No prioritization against WSC 2026 timeline

