# OSM Road Geometry Integration Plan

## Status: PLANNED — Not yet implemented
## Created: 2026-04-13
## Updated: 2026-04-14 — Redesigned as library-level, model-agnostic architecture

---

## Problem Statement

Our current road rendering uses **PeMS sensor positions** (~21 junctions for I-210, ~11 for SR-134) connected by straight `QCurve` segments. This produces circuit-board-looking roads — straight lines with sharp angles. The I-210/SR-134 interchange looks like two crossing sticks instead of a real freeway interchange with curved flyover ramps.

**SUMO-gui** looks realistic because it imports **OpenStreetMap polylines** with 200–500 intermediate points per corridor. The visual quality comes from **data density**, not a fancy renderer.

Our `DgAnimator` polygon renderer (R1–R5) already handles curves and filled polygons. The gap is **data**, not rendering.

---

## Design Principle: Two Independent Layers

**Layer A (visual context):** Full OSM road network for the area — ALL roads (freeways, arterials, local streets) rendered as thin gray background lines. NOT simulated. Just painted scenery that makes it look like a map.

**Layer B (simulation ground truth):** PeMS sensor corridors where cars actually drive. Rendered as thick filled polygons with lane markings. Positions from `station_map.csv`. This is what exists today — UNCHANGED.

The two layers are completely independent:
- Layer A is pure rendering (visual context)
- Layer B is pure simulation (physics + demand)
- They overlap geographically because they cover the same area
- The thick simulation polygon sits ON TOP of the thin OSM background line

---

## Architecture: Component Diagram

```
┌─────────────────────────────────────────────────────┐
│  scripts/download_osm_geometry.py                    │  ← Python, run once offline
│  CLI: --south --west --north --east --name           │
│  (Overpass API → data/osm/<name>_roads.json)         │
└──────────────────────┬──────────────────────────────┘
                       │ JSON file on disk (committed to repo)
                       ▼
┌─────────────────────────────────────────────────────┐
│  scalation.animation.OsmRoadNetwork                  │  ← NEW Scala class (library)
│  OsmRoadNetwork.load(jsonPath, gpsAnchors, dims)     │
│  → parses JSON, projects GPS→screen via Coordinates  │
│  → returns Array[Array[(Double, Double)]] polylines  │
└──────────────────────┬──────────────────────────────┘
                       │ screen-space polylines
                       ▼
┌─────────────────────────────────────────────────────┐
│  Model.loadOsmBackground(jsonPath, gpsAnchors, dims) │  ← 1 new method in Model.scala
│  → calls OsmRoadNetwork.load(...)                    │
│  → pushes result to dgAni.setBackgroundRoads(...)    │
└──────────────────────┬──────────────────────────────┘
                       │ setBackgroundRoads(polylines)
                       ▼
┌─────────────────────────────────────────────────────┐
│  DgAnimator.paintComponent Layer M-1                 │  ← thin gray lines
│  (draws background road network BEFORE everything)   │
│  Existing layers (polygons, markings, vehicles)      │
│  sit on top                                          │
└─────────────────────────────────────────────────────┘
```

---

## Component 1: Python Download Script

### File: `scripts/download_osm_geometry.py` — NEW

### Purpose
CLI tool that downloads road network for ANY bounding box. Model-agnostic — works for any area on Earth.

### Usage
```bash
# Eaton Fire corridor (I-210 / SR-134 area)
python scripts/download_osm_geometry.py \
  --south 34.13 --west -118.28 --north 34.19 --east -118.07 \
  --name eaton

# US-101 corridor (Mountain View)
python scripts/download_osm_geometry.py \
  --south 37.38 --west -122.10 --north 37.44 --east -121.96 \
  --name us101

# Any future corridor
python scripts/download_osm_geometry.py \
  --south <lat> --west <lon> --north <lat> --east <lon> \
  --name <area_name>
```

### Overpass API Query
```
[out:json][timeout:60];
(
  way["highway"~"motorway|trunk|primary|secondary|tertiary|motorway_link"]
    ({south},{west},{north},{east});
);
out body;
>;
out skel qt;
```

Downloads ALL road types within the bounding box — freeways, arterials, local streets. This gives the full visual context.

### Output Format: `data/osm/<name>_roads.json`
```json
{
  "bbox": {"south": 34.13, "west": -118.28, "north": 34.19, "east": -118.07},
  "generated": "2026-04-14T12:00:00",
  "roads": [
    {
      "osm_id": 12345,
      "highway": "motorway",
      "ref": "210",
      "name": "Foothill Freeway",
      "points": [[34.1567, -118.0812], [34.1568, -118.0815], ...]
    },
    {
      "osm_id": 23456,
      "highway": "primary",
      "ref": null,
      "name": "Colorado Blvd",
      "points": [[34.1445, -118.1501], [34.1446, -118.1505], ...]
    }
  ]
}
```

### Dependencies
- `requests` (pip install if needed — or stdlib `urllib`)
- `json` (stdlib)
- `argparse` (stdlib)
- **No osmnx, no networkx** — raw Overpass API only

---

## Component 2: OsmRoadNetwork Scala Class

### File: `src/main/scala/scalation/animation/OsmRoadNetwork.scala` — NEW
### Package: `scalation.animation` (alongside DgAnimator, Dgraph, Animator)

```scala
package scalation.animation

case class OsmRoadNetwork(
    polylines:  Array[Array[(Double, Double)]],  // screen-space (x, y) per road
    roadTypes:  Array[String]                     // "motorway", "primary", etc.
)

object OsmRoadNetwork:
    def load(jsonPath: String,
             gpsAnchors: Array[(Double, Double)],
             dims: (Double, Double)): OsmRoadNetwork = ...
```

### Key Design: Shared Projection
`gpsAnchors` ensures OSM GPS points and simulation junction GPS points go through the **same** `Coordinates` projection. Without this, the background would be offset from simulation roads.

---

## Component 3: DgAnimator Modifications

### File: `src/main/scala/scalation/animation/DgAnimator.scala` — MODIFY

### New Field
```scala
@volatile private var backgroundRoads: Array[Array[(Double, Double)]] = null
@volatile private var backgroundRoadTypes: Array[String] = null
```

### New Setter
```scala
def setBackgroundRoads(roads: Array[Array[(Double, Double)]],
                        roadTypes: Array[String] = null): Unit =
    backgroundRoads = roads
    backgroundRoadTypes = roadTypes
```

### New Rendering Layer M-1 (before everything)
```
Layer M-1:  background OSM road network (thin gray lines)    ← NEW
Layer M0:   ground plane (terrain fill)                      (existing)
Layer M1:   geographic labels                                (existing)
Layer M2:   highway shields                                  (existing)
Layer 0:    filled road polygons for simulated corridors     (existing)
Layer 1:    ramp surface polygons                            (existing)
Layer 2a:   solid edge lines                                 (existing)
Layer 2b:   dashed interior lane dividers                    (existing)
Layer 3:    colored outline (non-bundled edges)              (existing)
Layer 4:    edge labels + tokens (vehicles)                  (existing)
Layer 5:    free tokens                                      (existing)
Layer 6:    HUD overlay                                      (existing)
```

---

## Component 4: Model.scala Modification

### File: `src/main/scala/scalation/simulation/process/Model.scala` — MODIFY

```scala
protected def loadOsmBackground(jsonPath: String,
                                 gpsAnchors: Array[(Double, Double)],
                                 dims: (Double, Double)): Unit =
    if dgAni != null then
        val net = OsmRoadNetwork.load(jsonPath, gpsAnchors, dims)
        dgAni.setBackgroundRoads(net.polylines, net.roadTypes)
```

---

## Component 5: Model Wiring (ONE line per model)

### EatonFireModel.scala
```scala
loadOsmBackground("data/osm/eaton_roads.json", allGpsCoords, (5000, 3000))
```

### CalRoute101_3.scala (future)
```scala
loadOsmBackground("data/osm/us101_roads.json", gpsCoords, (4000, 3000))
```

### Any future model
```scala
loadOsmBackground("data/osm/<area>_roads.json", myGpsPoints, myDims)
```

---

## Implementation Order

| Step | Task | Effort | Files |
|------|------|--------|-------|
| 1 | Write `download_osm_geometry.py` | 1 hour | `scripts/download_osm_geometry.py` |
| 2 | Run script for Eaton bbox | 5 min | `data/osm/eaton_roads.json` |
| 3 | Create `OsmRoadNetwork.scala` | 1 hour | `src/.../animation/OsmRoadNetwork.scala` |
| 4 | Add `setBackgroundRoads` + Layer M-1 to DgAnimator | 30 min | `DgAnimator.scala` |
| 5 | Add `loadOsmBackground` to Model | 10 min | `Model.scala` |
| 6 | Wire EatonFireModel (1 line) | 5 min | `EatonFireModel.scala` |
| 7 | Visual verification + tuning | 30 min | — |

**Total: ~3-4 hours**

---

## File Summary

| File | Action | Package | Purpose |
|------|--------|---------|---------|
| `scripts/download_osm_geometry.py` | **CREATE** | — | CLI: bbox → Overpass API → JSON |
| `data/osm/eaton_roads.json` | **CREATE** (gen) | — | Cached Eaton area road network |
| `src/.../animation/OsmRoadNetwork.scala` | **CREATE** | `scalation.animation` | JSON → screen polylines (library) |
| `src/.../animation/DgAnimator.scala` | **MODIFY** | `scalation.animation` | +backgroundRoads, +Layer M-1 |
| `src/.../process/Model.scala` | **MODIFY** | `scalation.simulation.process` | +loadOsmBackground() |
| `src/.../process/model/EatonFireModel.scala` | **MODIFY** | — | 1 line: loadOsmBackground(...) |

---

## Risk Assessment

| Risk | Mitigation |
|------|-----------|
| Overpass API down/slow | Download once, commit JSON — no runtime dependency |
| Too many roads (slow render) | Filter by highway type in query; skip `residential` |
| Projection mismatch | Shared `Coordinates` call via `gpsAnchors` |
| JSON too large | Eaton bbox ~5MB max. Prune `residential` if needed |
| Breaks existing models | All new code is additive. No existing behavior changes |

---

## Key Decisions

1. **Library-level, not model-specific**: `OsmRoadNetwork` in `scalation.animation`, `loadOsmBackground` in `Model`. Any model can use them.
2. **Full area network, not just simulation corridors**: Download ALL roads in bbox. Cross-streets and arterials give map context.
3. **OSM for rendering only, PeMS for simulation**: Cars drive on PeMS sensor positions. OSM is the painted background.
4. **Python for download, Scala for rendering**: Python simpler for HTTP + JSON. No runtime HTTP from Scala.
5. **Download once, commit JSON**: No runtime dependency on Overpass API.
6. **Shared projection via gpsAnchors**: Ensures OSM background and simulation junctions align in screen space.
