# Dynamic Traffic Assignment (DTA) — Implementation Blueprint

**Created:** 2026-03-31  
**Status:** 🟡 Blueprint Complete — Ready to Implement  
**Paper:** WSC 2026 — Wildfire/Contraflow (Climate Resilience Track)  
**Deadline:** April 5, 2026  
**Prerequisite:** Graph-derived ramp positioning ✅ complete

---

## Goal

Implement the coupled feedback loop from `idea.md`:

```
Fire → Smoke → Network Cost → Routing → Traffic → Congestion → Fire Interaction
```

Three new subsystems + integration into existing ScalaTion classes.

---

## Architecture Overview

```
┌──────────────────────────────────────────────────────────────┐
│                    COARSE TIME STEP (60s)                     │
│                                                              │
│  FireGrid.update(Δt)                                         │
│       │                                                      │
│       ▼                                                      │
│  SmokeGrid.update(Δt, wind, fireGrid)                       │
│       │                                                      │
│       ▼                                                      │
│  For each VTransport:                                        │
│    vt.smokeDensity = SmokeGrid.concentrationAt(vt.midpoint)  │
│    vt.fireProximity = FireGrid.distanceToNearest(vt.midpoint)│
│       │                                                      │
│       ▼                                                      │
│  For each Junction (where ramp/FF decision exists):          │
│    Recompute edge costs → update routing probabilities       │
│                                                              │
├──────────────────────────────────────────────────────────────┤
│                  FINE TIME STEP (sub-second)                  │
│                                                              │
│  IDM: a = a_max [1 - (v/v_eff)^δ - (s*/s)^2]               │
│       v_eff = v₀ * (1 - γ·C)   ← smoke-reduced speed       │
│                                                              │
│  Junction.actOnCorridor:                                     │
│       rand.gen < splitRatio(t, cost) → divert or continue    │
│                                                              │
│  Off-ramp decision:                                          │
│       hazardCost(ahead) > threshold → take off-ramp          │
└──────────────────────────────────────────────────────────────┘
```

---

## Subsystem 1: FireGrid

**New file:** `src/main/scala/scalation/simulation/process/hazard/FireGrid.scala`

### Data Structure

```
class FireGrid (nRows: Int, nCols: Int, cellSize: Double,
                originLat: Double, originLon: Double):

    val state: Array [Array [FireState]]    // Burning, Unburned, Burned
    val intensity: Array [Array [Double]]   // kW/m² (reaction intensity)
    val ignitionTime: Array [Array [Double]]// when each cell ignited (-1 if not)
```

### Core Method

```
def update (dt: Double, wind: (Double, Double)): Unit =
    For each cell (i, j) where state == Burning:
      R = rothermelROS(fuel(i,j), wind, slope(i,j))
      Spread to neighbors based on R * dt vs cellSize
      If spread distance >= cellSize → ignite neighbor
      If fuel exhausted → state = Burned
```

### Rothermel ROS (simplified for simulation)

```
def rothermelROS (fuel: FuelModel, wind: (Double, Double), slope: Double): Double =
    val R0 = fuel.baseROS                       // from fuel model lookup table
    val phiW = fuel.windFactor * windSpeed       // wind enhancement
    val phiS = fuel.slopeFactor * slope          // slope enhancement
    R0 * (1 + phiW + phiS)
```

### Fuel Models

Use standard NFFL 13 fuel models (lookup table). For Eaton Fire area:
- Chaparral (NFFL Model 4): dominant in Altadena foothills
- Short grass (NFFL Model 1): urban interface

### GPS ↔ Grid Mapping

```
def cellAt (lat: Double, lon: Double): (Int, Int)    // GPS → grid cell
def gpsAt (row: Int, col: Int): (Double, Double)      // grid cell → GPS center
def screenAt (row: Int, col: Int): (Double, Double)   // grid cell → animation screen XY
```

The fire grid covers the area north of I-210 (Altadena foothills → corridor).
Origin and extent from the Eaton Fire perimeter data.

### Files to Reference

- Eaton Fire ignition point: 34.1897°N, -118.0753°W (approx)
- Wind data: Santa Ana winds ~60 mph NE→SW (historical for Jan 7, 2025)

---

## Subsystem 2: SmokeGrid

**New file:** `src/main/scala/scalation/simulation/process/hazard/SmokeGrid.scala`

### Data Structure

```
class SmokeGrid (fireGrid: FireGrid):

    val concentration: Array [Array [Double]]   // C(x,t) in arbitrary units [0..1]
```

### Core Method (Advection-Diffusion, discrete)

```
def update (dt: Double, wind: (Double, Double)): Unit =
    For each cell (i, j):
      // Source: fire emission
      S = α * fireGrid.intensity(i)(j)

      // Decay
      decay = β * concentration(i)(j)

      // Advection (upwind scheme)
      adv = windAdvection(i, j, wind, dt)

      // Diffusion (4-neighbor Laplacian)
      diff = D * laplacian(i, j) * dt

      concentration(i)(j) += S - decay + adv + diff
      concentration(i)(j) = max(0.0, min(1.0, concentration(i)(j)))
```

### Parameters

| Symbol | Meaning | Default |
|--------|---------|---------|
| α | Emission coefficient | 0.01 |
| β | Decay rate per step | 0.005 |
| D | Diffusion coefficient | 0.1 |

### Query Interface (used by VTransport)

```
def concentrationAt (lat: Double, lon: Double): Double
def concentrationAtScreen (sx: Double, sy: Double): Double  // for animation coupling
```

---

## Subsystem 3: Hazard-Aware Edge Cost

**Modified file:** `VTransport.scala`

### New Fields

```scala
var smokeDensity: Double = 0.0      // C ∈ [0, 1], updated each coarse step
var fireProximity: Double = 1e9     // meters to nearest burning cell
```

### New Method

```scala
/** Hazard-aware edge cost: BPR travel time + smoke + fire penalty.
 *  @param lambda1  smoke weight
 *  @param lambda2  fire proximity weight
 */
def edgeCost (lambda1: Double = 1.0, lambda2: Double = 0.5): Double =
    // BPR travel time
    val volume   = vdeque.size.toDouble
    val capacity = ???  // from lanesAt(seg) * per-lane capacity
    val t0       = length / freeFlowSpeed
    val bpr      = t0 * (1.0 + 0.15 * math.pow (volume / capacity.max(1), 4))

    // Smoke-reduced capacity
    val cEff     = capacity * (1.0 - 0.5 * smokeDensity)   // θ = 0.5

    // Hazard cost
    val firePenalty = if fireProximity < 500.0 then (500.0 - fireProximity) / 500.0 else 0.0
    bpr + lambda1 * smokeDensity + lambda2 * firePenalty
end edgeCost
```

---

## Subsystem 4: Routing Decisions at Junction

**Modified file:** `EatonFireModel.scala` (Car.actOnCorridor)

### Current: Static Split Ratio

```scala
// Current: rand.gen < splitRatios(timeIdx)
```

### Proposed: Cost-Based Routing

```scala
// At FF diverge junction:
val costAhead210 = sumEdgeCosts(route210, currentSeg, hwLen210)  // stay on I-210
val costFF134    = sumEdgeCosts(route134, 0, hwLen134)           // divert to SR-134
val totalCost    = costAhead210 + costFF134
val diversionProb = costAhead210 / totalCost   // higher I-210 cost → more likely to divert
// Blend with PeMS baseline ratio:
val blendedRatio = (1 - hazardWeight) * splitRatios(timeIdx) + hazardWeight * diversionProb
if rand.gen < blendedRatio then divert to SR-134
```

When `hazardWeight = 0` → pure PeMS-driven (baseline). When `hazardWeight = 1` → pure DTA.
This allows smooth transition from calibrated baseline to fire scenario.

### Off-Ramp Decision

```scala
// At off-ramp junction:
val costAhead = sumEdgeCosts(route, currentSeg, currentSeg + 3)  // look 3 segs ahead
if costAhead > offRampThreshold then
    take off-ramp  // evacuate to surface streets
```

### Helper: Sum Edge Costs

```scala
def sumEdgeCosts (route: Route, fromSeg: Int, toSeg: Int): Double =
    var cost = 0.0
    for seg <- fromSeg until toSeg do
        val vt = route.pathway(0).seg(seg)   // any lane — costs are segment-level
        if vt != null then cost += vt.edgeCost ()
    cost
end sumEdgeCosts
```

---

## Subsystem 5: Smoke-Aware IDM

**Modified file:** `Dynamics.scala`

### Current IDM

```scala
val a = aMax * (1 - pow(v / v0, delta) - pow(sStar / s, 2))
```

### Proposed: Smoke-Reduced Desired Speed

```scala
val smokeC = actor.currentVTransport.smokeDensity   // 0..1
val vEff   = v0 * (1.0 - gamma * smokeC)            // gamma = 0.5 default
val a = aMax * (1 - pow(v / vEff, delta) - pow(sStar / s, 2))
```

When `smokeC = 0` → standard IDM. When `smokeC = 0.5` → 25% speed reduction.

---

## Integration: Coarse-Step Scheduler

**Modified file:** `EatonFireModel.scala`

```scala
// In simulate() or via a periodic Coroutine:
val COARSE_DT = 60.0   // seconds

class HazardUpdater extends SimActor (...):
    def act (): Unit =
        while director.clock < simEndTime do
            fireGrid.update (COARSE_DT, wind)
            smokeGrid.update (COARSE_DT, wind)
            updateVTransportHazards ()     // push smoke/fire to all VTransports
            director.schedule (this, COARSE_DT)
            yieldToDirector ()

private def updateVTransportHazards (): Unit =
    for lane <- 0 until numLanes210 do
        for seg <- 0 until numSegments210 do
            val vt = route210.pathway(lane).seg(seg)
            if vt != null then
                val (lat, lon) = vt.midpointGPS   // need GPS mapping
                vt.smokeDensity  = smokeGrid.concentrationAt (lat, lon)
                vt.fireProximity = fireGrid.distanceToNearest (lat, lon)
    // Same for route134
```

---

## Task Breakdown

### Phase 1: Fire + Smoke Grid (standalone, testable independently)

| Task | File | Description |
|------|------|-------------|
| 1.1 | `hazard/FuelModel.scala` | Enum/case class for NFFL fuel types, lookup table |
| 1.2 | `hazard/FireGrid.scala` | Grid, Rothermel ROS, cell state machine, GPS mapping |
| 1.3 | `hazard/SmokeGrid.scala` | Advection-diffusion update, concentration query |
| 1.4 | `hazard/HazardTest.scala` | Standalone test: fire ignites, smoke spreads, print grid |

### Phase 2: Edge Cost + VTransport Integration

| Task | File | Description |
|------|------|-------------|
| 2.1 | `VTransport.scala` | Add `smokeDensity`, `fireProximity`, `edgeCost()` |
| 2.2 | `Dynamics.scala` | Smoke-aware `vEff` in IDM |
| 2.3 | Test: run baseline with zero smoke → verify identical results |

### Phase 3: Routing at Junction

| Task | File | Description |
|------|------|-------------|
| 3.1 | `EatonFireModel.scala` | `sumEdgeCosts()` helper |
| 3.2 | `EatonFireModel.scala` | Cost-based FF split ratio (blended with PeMS) |
| 3.3 | `EatonFireModel.scala` | Cost-based off-ramp diversion |
| 3.4 | Test: set `hazardWeight=0` → verify matches PeMS baseline |

### Phase 4: Coupled Simulation

| Task | File | Description |
|------|------|-------------|
| 4.1 | `EatonFireModel.scala` | HazardUpdater coroutine (coarse step scheduler) |
| 4.2 | `EatonFireModel.scala` | `updateVTransportHazards()` — push smoke/fire to segments |
| 4.3 | GPS ↔ screen coordinate mapping for VTransport midpoints |
| 4.4 | Full run: Eaton Fire ignition → smoke → DTA → evacuation |

### Phase 5: Contraflow (WSC paper scope)

| Task | File | Description |
|------|------|-------------|
| 5.1 | `Route.scala` or `EatonFireModel.scala` | Contraflow toggle: reverse direction on selected segments |
| 5.2 | Compare: no-contraflow vs contraflow evacuation clearance time |

---

## Execution Order

```
Phase 1 (Fire+Smoke) → Phase 2 (Edge Cost) → Phase 3 (Routing) → Phase 4 (Coupled) → Phase 5 (Contraflow)
         │                      │                     │                    │
    Standalone test        Zero-smoke test       hazardWeight=0         Full fire run
    (no traffic)           (identical to          (identical to          (WSC result)
                            baseline)              PeMS baseline)
```

Each phase has a **regression gate**: the previous behavior must be unchanged when hazard inputs are zero.

---

## Parameters Summary

| Symbol | Name | Default | Where Used |
|--------|------|---------|------------|
| α | Smoke emission coefficient | 0.01 | SmokeGrid |
| β | Smoke decay rate | 0.005 | SmokeGrid |
| D | Diffusion coefficient | 0.1 | SmokeGrid |
| γ (gamma) | Speed sensitivity to smoke | 0.5 | Dynamics (IDM) |
| θ (theta) | Capacity reduction from smoke | 0.5 | VTransport.edgeCost |
| λ₁ | Smoke weight in edge cost | 1.0 | VTransport.edgeCost |
| λ₂ | Fire proximity weight | 0.5 | VTransport.edgeCost |
| hazardWeight | Blend DTA vs PeMS baseline | 0.0 (baseline) / 1.0 (fire) | EatonFireModel |
| COARSE_DT | Hazard update interval | 60.0 s | HazardUpdater |

---

## File Map

| File | New/Modified | Phase |
|------|-------------|-------|
| `hazard/FuelModel.scala` | **New** | 1 |
| `hazard/FireGrid.scala` | **New** | 1 |
| `hazard/SmokeGrid.scala` | **New** | 1 |
| `hazard/HazardTest.scala` | **New** | 1 |
| `VTransport.scala` | Modified | 2 |
| `Dynamics.scala` | Modified | 2 |
| `EatonFireModel.scala` | Modified | 3, 4 |
| `Route.scala` | Possibly modified | 5 |

---

## Key Design Decisions

1. **Coarse/fine time step split**: Fire+smoke update every 60s, traffic updates sub-second. No performance concern — grids are small (~100×100 cells).

2. **Blend DTA with PeMS baseline**: `hazardWeight` parameter allows smooth transition. At `0` = calibrated baseline (paper validation). At `1` = full DTA (fire scenario).

3. **Junction is the decision engine**: All routing logic stays in `Car.actOnCorridor` inside EatonFireModel. Junction doesn't need new methods — the Car queries VTransport costs and makes decisions at junction boundaries.

4. **VTransport is the data provider**: Smoke/fire state lives on VTransport. `edgeCost()` is computed locally. No global routing table needed.

5. **No separate Router class**: For a corridor network (I-210 + SR-134 with one FF connector), routing reduces to: "stay or divert?" at one junction. Full Dijkstra is overkill. Cost comparison at the FF diverge point is sufficient.

6. **Contraflow is a topology toggle**: Reverse selected Pathway segments. Does not require new classes — just direction reversal in Route.

---

## Session State

### Completed This Session
- ✅ Graph-derived ramp positioning (Tasks 0–5)
- ✅ Side swap fix (perpVec negation)
- ✅ FR/OR same-seg downstream nudge (30px)
- ✅ Shortened labels (route/pathway/VTransport + ramp components)
- ✅ Removed `rampShift` from all config methods
- ✅ DTA blueprint (`docs/2026_WSC_paper/dta-blueprint.md`)

### Git State
- Branch: `feature/variable-lane-dll-unification`
- Last commit: `f3515ee8d` (visual fixes + graph-derived design doc)
- Uncommitted: graph-derived ramp implementation + side swap + nudge + DTA blueprint

### Next Steps When You Wake Up
1. **Commit current changes** — graph-derived ramp + DTA blueprint
2. **Run EatonFireModel** — visual verification of ramp positioning
3. **Start Phase 1** — FireGrid + SmokeGrid (standalone, no traffic dependency)



My ideas dumps:
i do not wan to enable lane change first. Yes it's important but It's artifical fix to me based on the pemps upstream sensor. we do not know what amount of vehicle lane chnages and this is the dynamic traffic assignment that I am talking about.
at each segment, sensing has to happen at the junction since they share same unified junction.
the junction can say. what is the density of this segment. compare to all the lanes entering that segment. say. what is the density of the ahead segments compare and make dynamic vehicle reasignment based on lane chnage safe logic (a vehicle in lane 1 can't chnage to lane 4) it has to be +1 or -1 lane change or stay in your lane if the density of that upconneting lane is heavy so there needs to be some density information calculation here.
I need to document all these Idea I am putting out to make this work a solid work.
we need a task flow of what we want to do to fix all these physics one after the other pls


One last fundamental request. Ramp entering is more like a vibe now. No literature backed mathodology. no safety check, I mean if it exist I need to see it.
virtually, what I see is vehicle entering from onramp and does not care if there is a close car at the mainline already. there should be something that slows vehicle down at that point and let is behave using the IDM physics. if we do not have this then this is just some fancy animation code and not Micro simulation according to Treiber on what a real micro simulatione engine must obey.
A vehicle can't overun a leader vehicle... the follower must be braking according to it's leader and that should be across board.
Car following model cars about leader and follower dynamics and this DLL is a good way to do that. if you are in a node, and you kept moving. you can't over take your leader ..
You should stop or willing to stop if your leader is stoping or slowing down.
Vsource kept producing cars even wen the segment is filled up: where is the dynamic traffic here?
onramp just enter without caring if there is a car that is close. they can only enter when the cost is clear or enter in a systemic manner by waiting at their junction
these are fundamentals. We need to investigate what we have and what we don't have.
read only investigation