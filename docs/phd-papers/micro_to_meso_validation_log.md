# Micro-to-Meso Validation Log

Companion record for [final_micro_to_meso_blueprint.md](final_micro_to_meso_blueprint.md).

This is the single durable record for micro-to-meso validation runs, derived
comparisons, gate decisions, and remaining work.  It records meaningful run
results rather than complete IDE launch commands or cleanup output.

## Measurement definitions

- **Whole-run throughput** = completed vehicles x 3600 / simulation duration.
- **Agenda activations** = actors dequeued by the shared `Model` scheduler during
  the simulation scheduling loop.  This includes source and vehicle work; it is
  deliberately not called movement activations.
- **Activations per completed vehicle** = agenda activations / sink completions.
- **Activation reduction** = 1 - meso agenda activations / micro agenda activations.
- **Corrected saturated capacity estimate** =
  `(nStop - 1) x 3600 / (simulation duration - first free-flow completion time)`.
- Whole-run throughput is not itself saturated discharge capacity because it
  includes initial free-flow traversal and final drainage.

## Gate 1 - Single-Link Validation

### G1.1 Deterministic mesoscopic correctness

Configuration and results:

| Measure | Expected | Observed |
|---|---:|---:|
| Entered vehicles | 30 | 30 |
| Released vehicles | 30 | 30 |
| Completed vehicles | 30 | 30 |
| Final occupancy | 0 | 0 |
| Free-flow time | 50.0 s | 50.0 s |
| Release headway | 2.0 s | 2.0 s |
| First release | 50.0 s | 50.0 s |
| Last release | 108.0 s | 108.0 s |

**Decision: PASS.** FIFO release timing, completion accounting, and final
occupancy matched their exact expected values.

### G1.2 Low-demand micro-to-meso comparison

Configuration:

| Parameter | Value |
|---|---:|
| Arrival rate (`lambda`) | 10 veh/h |
| Vehicles (`nStop`) | 30 |
| Intermediate junctions | 0 |
| Transport segments | 1 |
| Lanes | 1 |
| Segment length | 1400 m |
| Nominal free-flow speed | 68 mph = 108.948 km/h |

Results:

| Measure | Micro | Meso |
|---|---:|---:|
| Generated vehicles | 30 | 30 |
| Completed vehicles | 30 | 30 |
| Mean travel time | 49.183333 s | 46.260529 s |
| Simulation duration | 11380.003259 s | 11388.763788 s |
| Whole-run throughput | 9.490331 veh/h | 9.483031 veh/h |
| Mean traversal speed | 102.474 km/h | 108.948 km/h |
| Whole-run mean density | 0.093 veh/km/lane | 0.087 veh/km/lane |
| Agenda activations | 3011 | 90 |
| Activations/completed vehicle | 100.366667 | 3.000000 |

Derived comparison:

- Meso mean travel time is 5.94% below micro.
- Whole-run throughput differs by approximately 0.08%.
- Meso reduces agenda activations by 97.01%.
- Micro uses 33.46 times as many agenda activations as meso.

**Decision: PASS.** Both models complete the same low-demand workload with
nearly identical throughput.  The travel-time difference is consistent with
discrete IDM/Ballistic micro movement versus fixed mesoscopic free-flow
traversal.  The activation result confirms the intended sparse mesoscopic
execution.

### G1.3 Offered demand above configured mesoscopic capacity

Configuration:

| Parameter | Value |
|---|---:|
| Arrival rate (`lambda`) | 2400 veh/h |
| Mean arrival headway | 1.5 s |
| Vehicles (`nStop`) | 300 |
| Intermediate junctions | 0 |
| Transport segments | 1 |
| Lanes | 1 |
| Segment length | 1400 m |
| Configured meso capacity | 1800 veh/h |
| Meso release headway | 2.0 s |

Recorded mesoscopic results:

| Measure | Value |
|---|---:|
| Generated vehicles | 300 |
| Completed vehicles | 300 |
| Mean travel time | 128.839212 s |
| Simulation duration | 644.260529 s |
| Whole-run throughput | 1676.340472 veh/h |
| Agenda activations | 1496 |
| Activations/completed vehicle | 4.986667 |

The mesoscopic duration agrees with `46.260529 + 299 x 2.0 = 644.260529`
seconds.  This is evidence that the link remained continuously constrained
after its first release and enforced its configured two-second discharge
headway.

Recorded micro findings from the matched run:

| Measure | Value |
|---|---:|
| Generated vehicles | 300 |
| Completed vehicles | 300 |
| Mean travel time | 67.463333 s |
| Simulation duration | 499.496956 s |
| Whole-run throughput | 2162.175341 veh/h |
| Mean traversal speed | 74.707 km/h |
| Whole-run mean density | 28.942 veh/km/lane |
| Agenda activations | 41078 |
| Activations/completed vehicle | 136.926667 |

The micro run discharged faster than the mesoscopic link configured at 1800
veh/h.  Therefore this run establishes that 1800 veh/h is not the matching
micro capacity.  The exact micro capacity must be measured before saturated
micro-to-meso fidelity can be judged.

**Mesoscopic capacity enforcement: PASS.**

**Saturated micro-to-meso fidelity: PENDING CAPACITY CALIBRATION.**

Matched results after raising offered demand to 3600 veh/h while retaining
`nStop = 300`:

| Measure | Micro | Meso |
|---|---:|---:|
| Generated vehicles | 300 | 300 |
| Completed vehicles | 300 | 300 |
| Mean arrival interval | 1.0 s | 1.0 s |
| Mean travel time | 130.253333 s | 200.979651 s |
| Simulation duration | 523.664637 s | 644.260529 s |
| Whole-run throughput | 2062.388643 veh/h | 1676.340472 veh/h |
| Mean traversal speed | 38.694 km/h | 25.077 km/h |
| Whole-run mean density | 53.300 veh/km/lane | 66.847 veh/km/lane |
| Agenda activations | 78752 | 1497 |
| Activations/completed vehicle | 262.506667 | 4.990000 |

At this demand, meso reduces agenda activations by 98.10%; micro uses 52.61
times as many activations.  The mesoscopic last-completion time remains fixed
by its configured 1800 veh/h discharge capacity, while its mean travel time
rises because vehicles arrive earlier and wait longer.

Matched results at 4800 veh/h offered demand and `nStop = 300`:

| Measure | Micro | Meso |
|---|---:|---:|
| Generated vehicles | 300 | 300 |
| Completed vehicles | 300 | 300 |
| Mean arrival interval | 0.75 s | 0.75 s |
| Mean travel time | 182.328333 s | 237.049871 s |
| Simulation duration | 551.498478 s | 644.260529 s |
| Whole-run throughput | 1958.300963 veh/h | 1676.340472 veh/h |
| Mean traversal speed | 27.642 km/h | 21.261 km/h |
| Whole-run mean density | 70.844 veh/km/lane | 78.845 veh/km/lane |
| Agenda activations | 109997 | 1497 |
| Activations/completed vehicle | 366.656667 | 4.990000 |

Meso reduces agenda activations by 98.64% at this demand; micro uses 73.48
times as many activations.  The mesoscopic run was repeated and produced the
same reported values, confirming reproducibility for the same parameters and
random-number stream.

### G1.4 Micro effective-capacity sweep

Keep `nStop = 300` and run the micro model at successively higher offered
demand.  The micro capacity is identified when two successive corrected
capacity estimates differ by no more than 1-2% while mean travel time continues
to rise.

Use the measured micro free-flow completion time of `49.183333 s` in:

`corrected capacity = 299 x 3600 / (simulation duration - 49.183333)`

| Lambda (veh/h) | nStop | Duration (s) | Whole-run throughput (veh/h) | Corrected capacity (veh/h) | Mean travel time (s) | Agenda activations | Activations/vehicle | Interpretation |
|---:|---:|---:|---:|---:|---:|---:|---:|---|
| 2400 | 300 | 499.496956 | 2162.175341 | 2390.334085 | 67.463333 | 41078 | 136.926667 | Demand-limited/lower-congestion estimate |
| 3000 | 300 | pending | pending | pending | pending | pending | pending | Pending |
| 3600 | 300 | 523.664637 | 2062.388643 | 2268.582538 | 130.253333 | 78752 | 262.506667 | Saturated; estimate is 5.09% below 2400 run |
| 4800 | 300 | 551.498478 | 1958.300963 | 2142.877857 | 182.328333 | 109997 | 366.656667 | Saturated; estimate is 5.54% below 3600 run |

The corrected estimates decrease from 2390.33 to 2268.58 to 2142.88 veh/h as
offered demand rises.  This is not a constant-capacity plateau.  Higher forced
entry density is causing lower IDM discharge, so continuing to raise `lambda`
would measure congestion-induced capacity drop rather than identify a single
maximum-capacity parameter.  The maximum observed corrected flow is currently
2390.33 veh/h near the 2400 veh/h offered-demand case.

### G1.5 Saturated comparison after meso capacity calibration

The mesoscopic capacity was set to `2390 veh/h`, the rounded maximum corrected
micro flow observed in the demand sweep.  Offered demand was returned to
`lambda = 3600 veh/h` with `nStop = 300`.  The mesoscopic result is compared
against the already-recorded micro run at the same demand.

| Measure | Micro | Calibrated meso | Meso relative to micro |
|---|---:|---:|---:|
| Generated vehicles | 300 | 300 | 0.00% |
| Completed vehicles | 300 | 300 | 0.00% |
| Mean arrival interval | 1.0 s | 1.0 s | 0.00% |
| Mean travel time | 130.253333 s | 127.167936 s | -2.37% |
| Simulation duration | 523.664637 s | 496.637098 s | -5.16% |
| Whole-run throughput | 2062.388643 veh/h | 2174.626107 veh/h | +5.44% |
| Mean traversal speed | 38.694 km/h | 39.633 km/h | +2.43% |
| Whole-run mean density | 53.300 veh/km/lane | 54.870 veh/km/lane | +2.95% |
| Agenda activations | 78752 | 1497 | -98.10% |
| Activations/completed vehicle | 262.506667 | 4.990000 | -98.10% |

Micro uses 52.61 times as many agenda activations as calibrated meso.  All 300
vehicles complete in both models.  The macroscopic measures remain within
approximately 5.5%, while mean travel time, speed, and density remain within
3%.

**Working single-capacity calibration: PASS at 2390 veh/h.**  This parameter
matches the maximum observed micro flow for the present one-link experiment;
it does not claim that IDM has a demand-independent discharge rate under every
congestion level.

**Matched saturated micro-to-meso fidelity: PASS.**

## Gate 1 status

| Requirement | Status |
|---|---|
| Deterministic standalone meso correctness | PASS |
| Low-demand travel fidelity | PASS |
| Sparse-activation reduction | PASS |
| Meso discharge-capacity enforcement | PASS |
| Working single-capacity calibration | PASS (2390 veh/h) |
| Matched saturated micro-to-meso fidelity | PASS |
| Finite-storage and spillback validation | PENDING |

**Gate 1 overall: OPEN.**
