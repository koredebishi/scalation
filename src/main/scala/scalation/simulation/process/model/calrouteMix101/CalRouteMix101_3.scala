//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Mixed-resolution replica of the CalRoute101_3 Donald D. Doyle corridor. */

package scalation.simulation.process.model.calrouteMix101

import scalation.animation.OsmRoadNetwork
import scalation.mathstat.{MatrixD, VectorD}
import scalation.simulation.process.arrival.ArrivalSource
import scalation.simulation.process.builder.*
import scalation.simulation.process.config.{BoundingBox, CarFollowingModel, OsmSpec, PeMSDataHelper}
import scalation.simulation.process.*

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
         with RowTimeLoader:

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
    // Match the corridor on OSM: direction-aware, sensors on edges, ramps from topology
    private def squaredDistance (a: (Double, Double), b: (Double, Double)): Double =
        val dLat = a._1 - b._1
        val dLon = a._2 - b._2
        dLat * dLat + dLon * dLon
    end squaredDistance

    private def isMotorway (e: NetEdge): Boolean = e.highway == "motorway"
    private def isLink (e: NetEdge): Boolean     = e.highway == "motorway_link"
    private val corridorHeading = NetworkBuilder.heading (gps(1), gps.last)      // S1 -> S5 travel direction

    // Source: start of the motorway edge matched at the warm-up point
    private val sourceSpot   = NetworkBuilder.matchEdge (roadEdges, gps.head, corridorHeading, isMotorway)
    private val sourceNodeId = roadEdges (sourceSpot.edge).u

    // Sensors: split the matched edge at each sensor; sensor i gets the new node id -(i+1)
    private val sensorNodeIds = Array.tabulate (CalRouteMixData.sensors.length) (i => -(i + 1).toLong)
    private val edges =
        var out = roadEdges
        for i <- CalRouteMixData.sensors.indices do
            val point = gps (CalRouteMixData.sensors(i)._2)
            val spot  = NetworkBuilder.matchEdge (out, point, corridorHeading, isMotorway)
            out = NetworkBuilder.splitAt (out, spot, sensorNodeIds(i))
        end for
        out

    // Corridor: follow the motorway from the source until it ends; the end is the sink
    private def nextOnMotorway (node: Long): Option [NetEdge] = edges.find (e => e.u == node && isMotorway (e))
    private val corridor =
        val path = ArrayBuffer (sourceNodeId)
        var next = nextOnMotorway (sourceNodeId)
        while next.nonEmpty && ! path.contains (next.get.v) do
            path += next.get.v
            next = nextOnMotorway (next.get.v)
        end while
        path.toArray
    private val sinkNodeId = corridor.last
    require (! edges.exists (_.u == sinkNodeId), s"corridor end $sinkNodeId has an outgoing edge, so it cannot be a sink")
    require (sensorNodeIds.forall (corridor.contains), "a sensor is not on the corridor")

    // On-ramps: each motorway_link entering the corridor, walked back to the ramp's first node
    private def rampStart (link: NetEdge): Long =
        val seen = ArrayBuffer (link.v)
        var node = link.u
        var back = edges.find (e => e.v == node && isLink (e))
        while back.nonEmpty && ! seen.contains (back.get.u) do
            seen += node
            node = back.get.u
            back = edges.find (e => e.v == node && isLink (e))
        end while
        node
    end rampStart
    private val rampNodeIds = edges.filter (e => isLink (e) && corridor.contains (e.v))
                                   .sortBy (e => corridor.indexOf (e.v))        // travel order = ramp 1, ramp 2
                                   .map (rampStart)
    require (rampNodeIds.distinct.length == rampNodeIds.length, "CalRoute ramp sources share one OSM node")
    private val networkLanes = edges.filter (e => (e.u == sourceNodeId || e.v == sourceNodeId) && e.highway == "motorway")
        .map (_.lanes).max
    private val demandLanes = PeMSDataHelper.loadMainlineSensor (demand, 0, demand.window, demand.layout)._1.dim2
    // PeMS supplies demand only for measured lanes; unused OSM lanes remain empty.
    private val numLanes = math.min (networkLanes, demandLanes)
    require (numLanes > 0, "CalRouteMix mainline demand has no lane columns")
    private val (mainlineArrivalSources, rampArrivalSources) = ArrivalSource.allSources (demand, numLanes)

    //--------------------------------------------------
    // Supply CalRoute's resolution policy to the generic OSM builder
    private val microSegments = Set (1, 2, 5, 6)

    private def nearestAnchor (edge: NetEdge): Int =
        val point = if edge.geometry.nonEmpty then edge.geometry(edge.geometry.length / 2)
                    else (0.5 * (edge.uLat + edge.vLat), 0.5 * (edge.uLon + edge.vLon))
        gps.indices.minBy (i => squaredDistance (point, gps(i)))
    end nearestAnchor

    private val edgePolicy: EdgePolicy = new EdgePolicy:
        def profile (edge: NetEdge): EdgeProfile =
            val anchor = nearestAnchor (edge)
            val micro = edge.highway == "motorway" && microSegments.contains (anchor)
            EdgeProfile (s"calroute-$anchor-${edge.highway}", micro, freeFlowSpeed)
    end edgePolicy

    //--------------------------------------------------
    // Destination policy and vehicle actor
    case class Car (originId: Long, destinationId: Long) extends Vehicle ("c", this):
        override def act (): Unit =
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

    require (rampNodeIds.length == rampArrivalSources.length,
             s"OSM has ${rampNodeIds.length} on-ramps into the corridor, PeMS has ${rampArrivalSources.length}")
    private val rampSources = Array.tabulate (rampNodeIds.length) { r =>
        val (x, y) = nodePosition (rampNodeIds(r))
        new VSource (s"srcRamp${r + 1}", this, () => Car (rampNodeIds(r), sinkNodeId),
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
