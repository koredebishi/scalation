# Vehicle Token Labeling and Position Measures — Q&A

Date: 2025-09-17
Owner: Simulation Process (scalation 2.0)

Purpose
- Document how the model answers the three core questions about vehicle position, with direct code references and minimal, practical guidance.

Questions and Solutions

Question 1. How far into the segment is any car?
Solution 1. Use segDisp (aka disp): the segment-local progress in meters within the current VTransport segment. It ranges in [0, segment.length] and resets to 0 when entering a new segment.
- Where it’s controlled
  - VTransport.scala — reset and per-segment loop
    /**
     * Segment-local displacement reset and animation loop.
     * @uses Dynamics.updateV to advance disp within [0, length].
     */
    // on entering this VTransport
    actor.disp = 0
    while actor.disp < length && !done do
      motion.updateV(actor, length)
      // ... animate and schedule ...
    end while
  - Dynamics.scala (GippsDynamics.updateM) — clamped update
    /**
     * Clamp per-segment displacement and advance segment-local disp.
     * @param length current segment length (meters)
     * @effect car.disp set to new_disp within [0, length]
     */
    val prevDisp = car.disp
    val dxRaw    = x - car.t_disp
    val new_disp = if prevDisp + dxRaw <= length then prevDisp + dxRaw else length
    val dSeg     = new_disp - prevDisp
    car.disp = new_disp

- Where you can see it
  - Vehicle.toString prints segDisp (was disp).

Question 2. From where a car started, how far have you travelled?
Solution 2. Use pathDisp (aka t_disp): the path-local cumulative distance from this vehicle’s own route origin; and odo: a pure cumulative odometer that never resets.
- Where it’s updated
  - Dynamics.scala (GippsDynamics.updateM)
    /**
     * Maintain path-local cumulative and a true cumulative odometer.
     * @effect car.t_disp += dSeg; car.odo += dSeg
     */
    car.t_disp += dSeg
    car.odo    += dSeg
  - Vehicle.scala — odometer field and display
    /**
     * @field odo cumulative distance traveled (never resets)
     * @visibility used in logs and debugging
     */
    var odo: Double = 0.0

    /**
     * Log-friendly rendering clarifying segment vs path vs odometer.
     */
    override def toString: String =
      s"Vehicle ($label at $actTime:sec, actor_id= $id, segDisp=$disp m, pathDisp=$t_disp m, odo=$odo m, lane=$laneID, path=$pathInfo)"

- Notes
  - pathDisp (t_disp) is path-local: comparable within the same route; not globally comparable across different sources without a mapping.
  - odo is always increasing and route-agnostic; useful for totals and sanity checks.

Question 3. Given different start places (mainline vs. ramps), is that a problem?
Solution 3. Yes, if you compare path-local t_disp across different routes. The correct approach is to compare in a common longitudinal frame s_abs.
- Current behavior
  - Gipps uses xp, xn from t_disp. When vehicles are on different routes (pre-merge), t_disp values differ by route origin, so numeric comparisons may misorder ramp vs mainline vehicles.
- Recommended approach (design; not yet implemented here)
  - Define s_abs aligned to the main corridor:
    - On mainline: s_abs = prefixOffset(link) + disp.
    - On ramps (pre-merge): s_abs = s_mergePoint − remainingDistanceToMerge.
  - Use s_abs for xp/xn gaps, CarAhead comparisons, and logs.
  - Keep segDisp for drawing and pathDisp for per-route metrics.

Why ramp displacements seem “larger” in logs
- Mainline is segmented (Pathway → multiple VTransport with smaller lengths). Ramps are one VTransport (single large length). Without s_abs, t_disp near a merge can be larger on the ramp than on the mainline, even if the ramp vehicle is physically behind.

Who uses what
- VTransport.move calls Dynamics.updateV to advance disp (segment-local).
- GippsDynamics.updateM updates velocity via Gipps, proposes x via Butcher, then advances disp/t_disp/odo consistently.
- Vehicle.toString centralizes what’s printed in terminal/recorded logs.

Minimal references (files and symbols)
- src/main/scala/scalation/simulation/process/VTransport.scala
  - actor.disp = 0; while (actor.disp < length) { motion.updateV(actor, length) }
- src/main/scala/scalation/simulation/process/Dynamics.scala
  - GippsDynamics.updateM: prevDisp, new_disp, dSeg; car.disp, car.t_disp, car.odo updates.
- src/main/scala/scalation/simulation/process/Vehicle.scala
  - var odo: Double; toString shows segDisp, pathDisp, odo.

Glossary
- segDisp (disp): distance traveled within the current segment.
- pathDisp (t_disp): cumulative distance along this vehicle’s current route.
- odo: overall cumulative distance traveled by the vehicle (never resets).
- s_abs: recommended global longitudinal coordinate for cross-route comparisons (design).

Validation checklist
- segDisp remains within [0, segment.length] and resets at segment entry.
- pathDisp and odo are monotone increasing; odo never resets.
- Terminal logs show segDisp/pathDisp/odo consistently; animation labels unaffected.

