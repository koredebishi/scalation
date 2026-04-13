# ScalaTion Traffic Simulation - Architecture Updates (March 2026)

## What Changed and Why

| Area | Before | Now | Why It Matters |
|---|---|---|---|
| **Lane counts** | Fixed, one count for entire corridor (e.g., 5 lanes everywhere) | Variable per segment via `lanesPerSeg` array (I-210: 5, 6, 4, 2, 4, 5, 4) | Matches real freeway geometry. No phantom lanes drawn where no road exists |
| **Vehicle tracking (DLL)** | Pathway owned one DLL across all segments in a lane | VTransport owns its own per-segment DLL | O(1) car-following lookup. Density per segment now directly comparable to PeMS sensor data |
| **Lane ends mid-corridor** | Not handled. Vehicles drove on phantom lanes | `forceMerge()` merges vehicles into the best adjacent lane when their lane ends | Realistic mandatory lane-change behavior at taper points |
| **Ramp positioning** | Hardcoded pixel offsets, manually tuned per corridor | Graph-derived from lane geometry using `rampAttachPoint` | Adapts automatically to lane count at each segment. No manual tuning needed |
| **Ramp labels** | 3 overlapping labels per ramp (source, junction, transport) | 1 label per ramp (VTransport only) | Clean, readable animation output |
| **Model scope** | Single corridor (US-101, 5 lanes uniform) | Dual-corridor: I-210 WB + SR-134 WB, variable lanes, 22 on-ramps, 18 off-ramps, 2-lane FF connector | Supports the Eaton Fire evacuation network for WSC 2026 |

## OpenStreetMap Readiness

| What OSM Provides | Where It Plugs In | What It Gives Us |
|---|---|---|
| Road polyline (actual curves) | `CorridorLayout.mainlineScreenXY` | Visually accurate road shape instead of straight sensor-to-sensor lines |
| `lanes` tag per way segment | `CorridorLayout.config.lanesPerSeg` | Accurate lane counts without manual overrides |
| Node-to-node haversine distance | `CorridorLayout.segmentLengths` | Exact road length, not straight-line postmile difference |
| `motorway_link` connectivity | `RampSpec.joinSegment` | Ramp connections from graph topology, not postmile matching |
| Interchange geometry | FF connector routing | Actual I-210/SR-134 interchange shape |

One new loader (`OSMCorridorLoader`) produces the same `CorridorLayout` type. Everything downstream (CorridorBuilder, Route, rampAttachPoint, simulation engine) requires zero changes.

## Next: WSC 2026, Wildfire Evacuation + Contraflow

Fire/smoke grid, edge cost, dynamic traffic assignment, evacuation clearance time comparison.
