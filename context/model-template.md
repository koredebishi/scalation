# ScalaTion Traffic Model Template

Standard template for creating a new microscopic traffic simulation model.  
Any corridor, any location. Download OSM data first, then fill in the blanks.

---

## Step 0: OSM Data (Automatic)

The OSM road network + place names are **auto-downloaded on first run**.  
If `data/osm/<area_name>_roads.json` exists → cached, no download.  
If missing → `loadOsmBackground` computes bbox from your GPS anchors,  
calls the Python script, downloads from Overpass API, caches the result.

Manual download (optional, if you want to pre-cache):
```bash
python src/main/scala/scalation/simulation/scripts/download_osm_geometry.py \
    --south <LAT_S> --west <LON_W> --north <LAT_N> --east <LON_E> \
    --name <area_name>
```

Output: `data/osm/<area_name>_roads.json`

---

## Model Template

```scala
package scalation
package simulation
package process
package model

import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.{Exponential, Uniform}
import scalation.simulation.process.{IntegratorType, IDMDynamics, GippsDynamics, KraussDynamics}
import scalation.simulation.process.config._
import scalation.simulation.process.builder.{CorridorBuilder, BuiltNetwork}
import scalation.simulation.process.arrival._

//──────────────────────────────────────────────────────────────────────────────
// Runner
//──────────────────────────────────────────────────────────────────────────────

@main def runMyModel (): Unit = new MyModel (synthetic = true)


//──────────────────────────────────────────────────────────────────────────────
// Model
//──────────────────────────────────────────────────────────────────────────────

class MyModel (name: String = "MyModel", reps: Int = 1,
               animating: Boolean = true, aniRatio: Double = 500.0,
               synthetic: Boolean = true)
  extends Model (name, reps, animating, aniRatio)
    with RowTimeLoader:

    // ── Time ────────────────────────────────────────────────────────
    private val nt = ???                          // number of time intervals
    // For 15-min intervals: nt = 48 (12 hours)
    // For 5-min intervals:  nt = 73 (6 hours)
    // Override rowTime / rowTimeSlice if not 15-min default
    setTime (nt * rowTime)

    // ── Dynamics ────────────────────────────────────────────────────
    // Pick ONE car-following model:
    //   IDMDynamics    — Intelligent Driver Model (Treiber 2000)
    //   GippsDynamics  — Gipps safe-distance model (Gipps 1981)
    //   KraussDynamics — Krauss stochastic model (Krauss 1998)
    private val motion = IDMDynamics
    IDMDynamics.integratorType = IntegratorType.Ballistic

    // ── Demand ──────────────────────────────────────────────────────
    // Option A: Synthetic (fixed counts, for testing)
    //   ArrivalSource.syntheticSources(mlCount, rampCount, nLanes, nRamps, rv)
    //
    // Option B: PeMS (real sensor data)
    //   PeMSDemand.I210_WB_Anchor()  — or define your own PeMSDemand config
    //   AggregatedDemand.I210_WB_Baseline — for ramps

    // ── Topology ────────────────────────────────────────────────────
    // Option A: Single corridor — use CorridorBuilder
    //   val config = CorridorConfig(...)
    //   val built  = CorridorBuilder.build(config, motion, nt)
    //
    // Option B: Multi-corridor (e.g., interchange) — use MultiCorridorConfig
    //   val multiConfig = MultiCorridorConfig(...)
    //   val net = CorridorBuilder.buildMulti(multiConfig, motion, nt)
    //
    // Option C: Manual junctions + Route (like CalRoute101_3)
    //   val junc  = Array.ofDim[Junction](n)
    //   val route = Route("Rte", numLanes, intermediateJunc, first, last, motion)

    // ── Sources ─────────────────────────────────────────────────────
    // Create VSource per lane (mainline) + VSource per on-ramp
    // Wire nStop from ArrivalSource.getTotalVehicles(lane)

    // ── Ramps ───────────────────────────────────────────────────────
    // Create Ramp objects linking VSource → Junction (merge point)
    // Wire merge targets:
    //   ramp.targetPathway = route.pathway(outermost_lane)
    //   ramp.targetSegId   = joinSegment

    // ── Sinks ───────────────────────────────────────────────────────
    // At least one Sink at the downstream end

    // ── Register ────────────────────────────────────────────────────
    // addComponents(sources, junctions, sinks, ramps)
    // route.pathway.foreach(addComponent(_))

    // ── Car ─────────────────────────────────────────────────────────
    case class Car () extends Vehicle ("c", this):
        override def act (): Unit =
            // Mainline vs ramp entry based on subtype
            // Drive segment by segment: move() → jump() → DLL hop
            ???
    end Car

    // ── RowTimeLoader ───────────────────────────────────────────────
    def getDataDimension: Int = nt
    def getMuForSource (sourceIdx: Int): VectorD = ???
    def getSpeedMatrix (): MatrixD = ???

    // ── OSM Background ──────────────────────────────────────────────
    // GPS anchors = your junction/sensor GPS coordinates
    private val osmGpsAnchors: Array[(Double, Double)] = ???
    loadOsmBackground ("data/osm/<area_name>_roads.json", osmGpsAnchors, (width, height))

    // ── Run ─────────────────────────────────────────────────────────
    simulate ()
    waitFinished ()
    Model.shutdown ()

end MyModel
```

---

## Existing Examples

| Model | Corridors | Demand | Topology | File |
|-------|-----------|--------|----------|------|
| CalRoute101_3 | 1 (US-101 NB) | PeMS | Manual junctions + Route | `CalRoute101_3.scala` |
| EatonFireModel | 2 (I-210 WB + SR-134 WB) | PeMS + Aggregated | CorridorBuilder.buildMulti | `EatonFireModel.scala` |

## Key Classes

| Class | Purpose |
|-------|---------|
| `Model` | Base simulation model — animation, clock, agenda |
| `RowTimeLoader` | Time-varying demand (mu per interval) |
| `Route` | Multi-lane road: pathways, lane changes, force merge |
| `Junction` | Segment boundary — density counting, animation jump |
| `VSource` | Vehicle generator — nStop vehicles, arrival distribution |
| `Ramp` | On/off ramp — single-segment link between VSource and Junction |
| `Vehicle` | Car entity — laneID, velocity, displacement, segId |
| `IDMDynamics` / `GippsDynamics` / `KraussDynamics` | Car-following acceleration |
| `CorridorBuilder` | Builds topology from config (junctions, route, ramps, sinks) |
| `OsmRoadNetwork` | Loads OSM JSON → projects GPS to screen space |

