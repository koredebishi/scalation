# CFM Completeness — Gaps and Fixes
**Codebase: ScalaTion 2.0 / CalRoute101_3**
**Date: March 2, 2026**

---

## Gap 1 — IDM Ignores a Real Leader Between 50 m and ~150 m Ahead

### How the DES Scheduler Prevents Overtaking (Why the Previous Claim Was Wrong)

This is a coroutine-based DES. There is **one active coroutine at a time**.  
Inside `VTransport.move()`, every vehicle executes this loop:

```scala
while actor.disp < length && !done do
    motion.updateV(actor, length)   // CFM: advance this vehicle one rt tick
    actor.schedule(Vehicle.rt)      // re-queue this vehicle rt seconds from now
    actor.yieldToDirector()         // SUSPEND — director picks next vehicle
end while
```

Every vehicle yields after **one time tick (`rt = 0.5 s`)**. The director resumes the next vehicle from its priority queue. All vehicles on a segment advance in lock-step. **A follower vehicle cannot physically overtake a leader vehicle within the same segment.** DLL insertion order = physical road order, and the coroutine scheduler enforces this. The earlier claim in this document about overtaking was incorrect.

### The Real Problem — `FREERANGE = 50 m` Blinds IDM to Real Leaders

The DLL is per-lane and spans the **entire route**. A vehicle stays in the DLL from route entry (`addToAlist`) to route exit (`removeFromAlist`). The leader can therefore be on the same segment or one segment ahead — the `segId` and `t_disp` fields on each node tell the CFM exactly where the leader is.

**Gipps handles cross-segment leaders correctly:**
```scala
// Dynamics.scala — GippsDynamics.gipps
if cp == null || cp.segId < cn.segId then
    // phantom: leader is behind or gone → free flow
else if cp.segId == cn.segId then
    cp_r_disp = cp.disp                  // same segment
else
    cp_r_disp = length + cp.disp         // leader is in next segment
```
When vehicle-A is on segment 3 and vehicle-B is on segment 2, `cp.segId (3) > cn.segId (2)` → Gipps uses `length + cp.disp` as the leader's effective position. **Working correctly.**

**IDM does not have a `segId` guard. It uses only raw `t_disp` distance:**
```scala
// Dynamics.scala — IDMDynamics.updateM
private val FREERANGE = 50.0   // ← hardcoded 50 m

val (x_leader, v_leader) =
    if car_ahead == null || car_ahead.t_disp - car.t_disp > FREERANGE then
        (car.t_disp + 1000.0, car.velocity)   // phantom: ignores the real leader
    else
        (car_ahead.t_disp, car_ahead.velocity) // real leader
```

At 30 m/s with T = 3 s, the IDM desired gap `s* = s0 + v*T ≈ 5 + 90 = 95 m`. A real leader sitting 60–150 m ahead is entirely invisible to IDM — `t_disp` difference exceeds 50 m so IDM switches to the phantom 1000 m leader and accelerates to v_max instead of following and braking.

### Fix — `Dynamics.scala`, `IDMDynamics` object

```scala
// Replace the hardcoded 50 m with a value that covers real highway headways
private val FREERANGE = 150.0   // covers s* at highway speed; or use Vehicle.vmax * Vehicle.T * 2
```

Also add the `segId` guard consistent with Gipps, so cross-segment leaders are handled by position, not cut off by distance:

```scala
// IDMDynamics.updateM — replace the current leader-detection block with:
val (x_leader, v_leader): (Double, Double) =
    if car_ahead == null then
        (car.t_disp + 1000.0, car.velocity)                         // no one ahead: free flow
    else if car_ahead.segId < car.segId then
        (car.t_disp + 1000.0, car.velocity)                         // leader is behind (stale DLL): free flow
    else if car_ahead.t_disp - car.t_disp > FREERANGE then
        (car.t_disp + 1000.0, car.velocity)                         // leader is very far: free flow
    else
        (car_ahead.t_disp, car_ahead.velocity)                      // real leader: use it
```

---

## Gap 2 — Red Light / Stopped Obstacle Does Not Enter the CFM

### What
In `TrafficDyn.scala` (the example with gates) a vehicle stops at a red light like this:

```scala
if light(i).shut then queue(i).waitIn()   // coroutine suspends
```

The vehicle's coroutine is simply paused. The CFM never runs. No vehicle behind this suspended car sees it as a leader with `velocity = 0`.

In `CalRoute101_3` there are **no gates at all**. Congestion must emerge purely from CFM interaction. For that to work every following vehicle must read an accurate `v_leader`. When a vehicle is stationary at the front of a queue its velocity in `Dynamics.scala` must be readable as 0, not the default `v0 = 4.0 m/s`.

### Why It Is a Problem
`v0 = 4.0 m/s` is set in `Vehicle.def_prop` and never reset to 0 during a simulation run. A vehicle that decelerates to near-zero through the CFM equations will have `velocity ≈ 0` only in its own state — but if it re-enters `VTransport.move()` on the next segment, the line:

```scala
actor.disp = 0.0
// no reset of actor.velocity
```

preserves whatever velocity the last segment left it with. There is no "start from rest" on segment entry.

If the desired behaviour is a queue forming behind a slow lead vehicle, the lead vehicle must be stationary in the DLL with `velocity = 0` and the followers must reach it via the CFM gap term — which works mathematically in IDM and Gipps — but only if the leader is found (Gap 1).

### Where to Fix
**File:** `Vehicle.scala` — add `v0 = 0.0` as the default starting velocity for congested entry, or allow `VSource` to set it.  
**File:** `VTransport.move()` — do not preserve velocity across segment entry when the vehicle was queued/stopped.  
**For gate-style stops** — instead of `queue.waitIn()`, insert a virtual stopped vehicle at the end of the upstream segment with `velocity = 0` so the CFM sees it and brakes naturally. This is the Treiber-compliant approach.

---

## Gap 3 — Per-Vehicle Parameters Are Global

### What
`Vehicle.prop` is a single mutable `Map[String, Double]` shared by the entire object:

```scala
// Vehicle.scala
private [process] var prop = def_prop   // one map, all vehicles
```

`amax`, `bmax`, `T`, `s`, `vmax` are all read via `Vehicle.prop(key)`.

### Why It Is a Problem
`setParams(params)` replaces the entire map. A calibration call mid-simulation changes every vehicle's parameters simultaneously, including vehicles already on the road. There is no per-vehicle state for reaction time or desired gap.

### Where to Fix
**File:** `Vehicle.scala`  
Each `Vehicle` instance needs its own copy of the relevant parameters. The cleanest fix is to add instance fields that shadow the global defaults:

```scala
abstract class Vehicle(...):
    var v_amax = Vehicle.amax   // instance copy
    var v_bmax = Vehicle.bmax
    var v_T    = Vehicle.T
    var v_s    = Vehicle.s
```

Then `Dynamics.scala` reads `car.v_amax` instead of the global `Vehicle.amax`. The global map becomes a source of defaults only.

---

## Summary — What to Fix and Where

| # | Problem | File | Line / Method | Fix |
|---|---------|------|---------------|-----|
| 1a | `FREERANGE = 50 m` blinds IDM to real leaders 51–150 m ahead | `Dynamics.scala` | `IDMDynamics` object, `updateM` | Raise to `150.0` (minimum) or `Vehicle.vmax * Vehicle.T * 2` |
| 1b | IDM has no `segId` guard — stale cross-segment DLL node used as leader | `Dynamics.scala` | `IDMDynamics.updateM` leader-detection block | Add `car_ahead.segId < car.segId → phantom` guard, matching Gipps |
| 2a | `v0 = 4.0 m/s` never resets to 0 on congested entry | `Vehicle.scala` | `def_prop` | Set `v0 = 0.0` or allow per-entry override in `VTransport.move()` |
| 2b | Gate/red-light stop bypasses CFM entirely | `TrafficDyn.scala` / architecture | `Car.act()` | For CFM-compliant stops, insert a virtual stopped leader; do not use `waitIn()` on the moving segment |
| 3 | `Vehicle.prop` is a global singleton map | `Vehicle.scala` | `prop` field + `Dynamics.scala` | Add per-instance parameter fields; global map becomes defaults only |



