//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Mixed-resolution replica of the CalRoute101_3 Donald D. Doyle corridor. */

package scalation.simulation.process.model.calrouteMix101

import scalation.animation.OsmRoadNetwork
import scalation.mathstat.{MatrixD, VectorD}
import scalation.simulation.process.arrival.ArrivalSource
import scalation.simulation.process.builder.*
import scalation.simulation.process.config.{BoundingBox, CarFollowingModel, OsmSpec, PeMSDataHelper}
import scalation.simulation.process.*
import scalation.simulation.process.model.calroute101.CalRouteReadings

import scala.collection.mutable.ArrayBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run CalRouteMix101_3 simulation. */
@main def runCalRouteMix101_3 (): Unit = new CalRouteMix101_3 ()

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Mixed-resolution CalRoute model using the prepared OSM network as topology. */
class CalRouteMix101_3 (name: String = "CalRouteMix101_3", reps: Int = 1,
                        animating: Boolean = true, aniRatio: Double = 500.0,
                        autoRun: Boolean = true, traceSpeeds: Boolean = false)
      extends Model (name, reps, animating, aniRatio)
         with RowTimeLoader with CalRouteReadings:

    //--------------------------------------------------
    // Load only the CalRouteMix data specification; OSM supplies topology.
    private val demand = CalRouteMixData.demand
    private val dynamics = CalRouteMixData.dynamics
    private val nt = demand.window.endRow - demand.window.startRow
    private val freeFlowSpeed = dynamics.vmax
    private val motion = dynamics.carFollowing match
        case CarFollowingModel.IDM    => IDMDynamics
        case CarFollowingModel.Gipps  => GippsDynamics
        case CarFollowingModel.Krause => KraussDynamics

    motion match
        case IDMDynamics    => IDMDynamics.integratorType = IntegratorType.Ballistic
        case GippsDynamics  => GippsDynamics.integratorType = IntegratorType.Ballistic
        case KraussDynamics => KraussDynamics.integratorType = IntegratorType.Ballistic
        case _              =>

    // Every OSM lane segment would draw its name; hide that text so the cars stay readable
    if animating && dgAni != null then dgAni.setEdgeLabels (false)

    //--------------------------------------------------
    // Prepare the OSM artifact from the CalRoute GPS anchors
    private val gps = CalRouteMixData.mainlineGPS
    private val osmBounds = BoundingBox (gps.map (_._1).min, gps.map (_._2).min,
                                         gps.map (_._1).max, gps.map (_._2).max)
    private val osm = OsmSpec ("calroute101_mix", osmBounds, marginMeters = 500.0)
    private val osmEdges = NetworkBuilder.read (osm.ensurePrepared ())
    // The classic corridor has no local streets: keep the freeway and its ramps only
    private val roadEdges = osmEdges.filter (e => e.highway == "motorway" || e.highway == "motorway_link")
    private val viewScale  = 1.0                                        // one drawing scale for the whole road picture
    private val widthScale = 2.0                                        // lanes drawn this many times wider than real; lengths kept
    private val project = OsmRoadNetwork.buildProjector (
        Array ((osmBounds.north, osmBounds.west), (osmBounds.south, osmBounds.east)),
        (3000.0 * viewScale, 2000.0 * viewScale), "CalRouteMix101_3")

    //--------------------------------------------------
    // Match the corridor on OSM: direction-aware, sensors and merge points on edges
    private def isMotorway (e: NetEdge): Boolean = e.highway == "motorway"
    private def isLink (e: NetEdge): Boolean     = e.highway == "motorway_link"
    private val corridorHeading = NetworkBuilder.heading (gps(1), gps.last)      // S1 -> S5 travel direction

    // Source: start of the motorway edge matched at the warm-up point
    private val sourceSpot   = NetworkBuilder.matchEdge (roadEdges, gps.head, corridorHeading, isMotorway)
    private val sourceNodeId = roadEdges (sourceSpot.edge).u

    // Split the corridor at the classic CalRoute points; each split gets a new negative node id:
    //   sensors S1..S5 -> -1..-5, merge points 1, 2 -> -6, -7, sink (SINK_GAP past S5) -> -8.
    // Ramp cars join at the classic merge points, as classic's Ramp does; the real OSM
    // ramp roads stay drawn but carry no cars.
    private val sensorNodeIds = Array.tabulate (CalRouteMixData.sensors.length) (i => -(i + 1).toLong)
    private val mergeNodeIds  = Array.tabulate (CalRouteMixData.rampGPS.length) (r => -(sensorNodeIds.length + r + 1).toLong)
    private val sinkNodeId    = -(sensorNodeIds.length + mergeNodeIds.length + 1).toLong
    private val SINK_GAP      = 10.0                                    // metres S5 -> sink (S5 must stay a recording Junction)

    private val splitEdges =
        var out    = roadEdges
        val points = CalRouteMixData.sensors.map (s => gps (s._2)) ++ CalRouteMixData.rampGPS
        val ids    = sensorNodeIds ++ mergeNodeIds
        for i <- points.indices do
            val spot = NetworkBuilder.matchEdge (out, points(i), corridorHeading, isMotorway)
            out = NetworkBuilder.splitAt (out, spot, ids(i))
        end for
        val afterS5 = out.indexWhere (e => e.u == sensorNodeIds.last && isMotorway (e))
        require (afterS5 >= 0, "the motorway does not continue past S5")
        NetworkBuilder.splitAt (out, EdgeSpot (afterS5, SINK_GAP, 0.0, 0.0), sinkNodeId)

    // Corridor: follow the motorway from the source to the sink
    private def nextOnMotorway (node: Long): Option [NetEdge] = splitEdges.find (e => e.u == node && isMotorway (e))
    private val corridor =
        val path = ArrayBuffer (sourceNodeId)
        var next = nextOnMotorway (sourceNodeId)
        while path.last != sinkNodeId && next.nonEmpty && ! path.contains (next.get.v) do
            path += next.get.v
            next = nextOnMotorway (next.get.v)
        end while
        path.toArray
    require (corridor.last == sinkNodeId, "the corridor does not reach the sink")

    // The classic road ends at S5: drop the motorway past the sink (not built, not drawn)
    private val pastSink =
        val drop = ArrayBuffer [NetEdge] ()
        var next = nextOnMotorway (sinkNodeId)
        while next.nonEmpty && ! drop.contains (next.get) do
            drop += next.get
            next = nextOnMotorway (next.get.v)
        end while
        drop
    private val edges = splitEdges.filterNot (pastSink.contains)
    require (! edges.exists (_.u == sinkNodeId), s"sink $sinkNodeId has an outgoing edge")
    require ((sensorNodeIds ++ mergeNodeIds).forall (corridor.contains), "a sensor or merge point is not on the corridor")
    private val networkLanes = edges.filter (e => (e.u == sourceNodeId || e.v == sourceNodeId) && e.highway == "motorway")
        .map (_.lanes).max
    private val demandLanes = PeMSDataHelper.loadMainlineSensor (demand, 0, demand.window, demand.layout)._1.dim2
    // PeMS supplies demand only for measured lanes; unused OSM lanes remain empty.
    private val numLanes = math.min (networkLanes, demandLanes)
    require (numLanes > 0, "CalRouteMix mainline demand has no lane columns")
    private val (mainlineArrivalSources, rampArrivalSources) = ArrivalSource.allSources (demand, numLanes)

    //--------------------------------------------------
    // Supply CalRoute's resolution policy to the generic OSM builder.
    // Mirror the classic mixed model (mesoSegs = 0, 3, 4) by position on the corridor:
    //   meso source -> S1, micro S1 -> S2, meso S2 -> merge 2, micro merge 2 -> S5
    private def place (node: Long): Int = corridor.indexOf (node)       // travel order on the corridor (-1 = off it)

    private val microStretches = Array (
        (place (sensorNodeIds(0)), place (sensorNodeIds(1))),            // S1 -> S2 (merge 1 inside)
        (place (mergeNodeIds(1)),  place (sensorNodeIds.last)))          // merge 2 -> S5

    private def onMicroStretch (edge: NetEdge): Boolean =
        val a = place (edge.u)
        val b = place (edge.v)
        a >= 0 && b == a + 1 && microStretches.exists ((lo, hi) => a >= lo && b <= hi)
    end onMicroStretch

    private val edgePolicy: EdgePolicy = new EdgePolicy:
        def profile (edge: NetEdge): EdgeProfile =
            val micro = isLink (edge) ||                                  // ramps micro: cars visible on them
                        isMotorway (edge) && onMicroStretch (edge)
            EdgeProfile (s"calroute-${edge.highway}", micro, freeFlowSpeed)
    end edgePolicy

    //--------------------------------------------------
    // Destination policy and vehicle actor
    case class Car (originId: Long, destinationId: Long) extends Vehicle ("c", this):
        override def act (): Unit =
            if mergeNodeIds.contains (originId) then                    // ramp car: joins the outer lane at the merge, as classic
                laneID   = numLanes - 1
                fromRamp = true
            graph.drive (this, graph.nodeIndex (originId), graph.nodeIndex (destinationId))
        end act
    end Car

    //--------------------------------------------------
    // Create PeMS-driven mainline and ramp sources at OSM nodes
    private val drawn = NetworkBuilder.drawAt (edges, project, widthScale)   // the builder's node positions

    private def nodePosition (id: Long): (Double, Double) =
        drawn.getOrElse (id, throw new IllegalArgumentException (s"OSM node $id has no incident edge"))
    end nodePosition

    private val (sourceX, sourceY) = nodePosition (sourceNodeId)

    // Same perpendicular-offset formula NetworkBuilder uses to place its built
    // lanes (laneGap), against the actual first edge out of sourceNodeId,
    // so a lane's VSource sits on the lane it feeds instead of a flat pixel fan.
    private val laneGap = NetworkBuilder.laneGap (edges, project, widthScale)   // the builder's drawn lane width in pixels
    private val sourceEdge = edges.find (e => e.u == sourceNodeId && e.highway == "motorway").getOrElse (
        throw new IllegalArgumentException (s"OSM node $sourceNodeId has no outgoing motorway edge"))
    private val (sourceHeadX, sourceHeadY) = nodePosition (sourceEdge.v)
    private val sourceDx   = sourceHeadX - sourceX
    private val sourceDy   = sourceHeadY - sourceY
    private val sourceHyp  = math.hypot (sourceDx, sourceDy)
    private val sourcePerp = if sourceHyp > 0.0 then VectorD (sourceDy / sourceHyp, -sourceDx / sourceHyp)
                             else VectorD (0.0, 0.0)

    private def sourceLaneShift (lane: Int): VectorD =
        val edgeLanes = sourceEdge.lanes
        val physicalLane = edgeLanes - 1 - lane
        sourcePerp * ((physicalLane - (edgeLanes - 1) / 2.0) * laneGap)
    end sourceLaneShift

    private val mainlineSources = Array.tabulate (numLanes) { lane =>
        val shift = sourceLaneShift (lane)
        new VSource (s"vsrcML_L$lane", this, () => Car (sourceNodeId, sinkNodeId), lane,
                     mainlineArrivalSources(lane).getTotalVehicles (lane),
                     mainlineArrivalSources(lane).getDistribution,
                     Array (sourceX + shift (0), sourceY + shift (1), 20.0, 20.0), s"M$lane")
    }

    require (mergeNodeIds.length == rampArrivalSources.length,
             s"CalRoute has ${mergeNodeIds.length} merge points, PeMS has ${rampArrivalSources.length} ramps")
    private val rampSources = Array.tabulate (mergeNodeIds.length) { r =>
        val (x, y) = nodePosition (mergeNodeIds(r))
        new VSource (s"srcRamp${r + 1}", this, () => Car (mergeNodeIds(r), sinkNodeId),
                     numLanes + r, rampArrivalSources(r).getTotalVehicles (0),
                     rampArrivalSources(r).getDistribution,
                     Array (x, y, 20.0, 20.0), s"R${r + 1}")
    }

    //--------------------------------------------------
    // Build the OSM graph, route the sink, and register components.
    // Every node stays the builder's Junction; sources feed their node, they never replace it.
    private lazy val graph: BuiltGraph = NetworkBuilder.build (
        edges, Array (sinkNodeId), project, motion, nt, edgePolicy, widthScale = widthScale)

    graph.routeTo (Array (graph.nodeIndex (sinkNodeId)))
    addComponent (graph.components*)
    addComponent (mainlineSources*)
    addComponent (rampSources*)

    //--------------------------------------------------
    // Supply the RowTimeLoader contract from PeMS data
    override val rowTimeSlice: Double = demand.window.binSeconds
    override def getDataDimension: Int = nt
    override def getMuForSource (sourceIdx: Int): VectorD =
        RowTimeLoader.getMuForSourceDefault (mainlineArrivalSources, rampArrivalSources,
                                             numLanes, nt, sourceIdx)
    private val anchorSpeed = RowTimeLoader.getSpeedMatrixDefault (demand)
    override def getSpeedMatrix (): MatrixD = anchorSpeed

    //--------------------------------------------------
    // Report junctions through the generic graph representation
    private lazy val graphJunctions = graph.node.collect { case j: Junction => j }
    private lazy val pemsToJunc = sensorNodeIds.map { nodeId =>
        val graphIndex = graph.nodeIndex (nodeId)
        graph.node.indices.filter (i => graph.node(i).isInstanceOf [Junction]).indexOf (graphIndex)
    }
    require (pemsToJunc.forall (_ >= 0), "a CalRoute PeMS sensor did not map to an OSM junction")
    def getJunctions: Array [Junction] = graphJunctions
    def getPemsSensorIndices: Array [Int] = pemsToJunc
    def getDemandConfig = demand
    def getDynamicsConfig = dynamics

    //--------------------------------------------------
    // Run the model when used as a standalone entry point
    if autoRun then
        simulate ()
        waitFinished ()
        Model.shutdown ()
end CalRouteMix101_3
