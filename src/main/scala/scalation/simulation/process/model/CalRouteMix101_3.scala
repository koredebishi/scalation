//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Mixed-resolution replica of the CalRoute101_3 Donald D. Doyle corridor. */

package scalation
package simulation
package process
package meso
package model

import scala.collection.immutable.Map

import scalation.animation.OsmRoadNetwork
import scalation.mathstat.{MatrixD, VectorD}
import scalation.simulation.process.builder.{EdgePolicy, EdgeProfile, BuiltGraph, NetEdge, NetworkBuilder, TrafficModelBuilder}
import scalation.simulation.process.config.{BoundingBox, CarFollowingModel, NetworkConfig, OsmSpec}
import scalation.simulation.process.{Component, GippsDynamics, IDMDynamics, IntegratorType, Junction, KraussDynamics, Model, RowTimeLoader, Vehicle}

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
    // Load CalRoute demand and dynamics configuration
    private val builder = TrafficModelBuilder.US101_DonaldDoyle_PeMS ()
    private val networkConfig = builder.getNetwork
    private val demand = builder.getDemand
    private val dynamics = builder.getDynamics
    private val numLanes = builder.numLanes
    private val nt = 48
    private val mainlineArrivalSources = builder.mainlineArrivalSources
    private val rampArrivalSources = builder.rampArrivalSources
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
    private val gps = NetworkConfig.mainlineGPSAnchors
    private val osmBounds = BoundingBox (gps.map (_._1).min, gps.map (_._2).min,
                                         gps.map (_._1).max, gps.map (_._2).max)
    private val osm = OsmSpec ("calroute101_mix", osmBounds, marginMeters = 500.0)
    private val edges = NetworkBuilder.read (osm.ensurePrepared ())
    private val outgoingNodeIds = edges.iterator.map (_.u).toSet
    private val project = OsmRoadNetwork.buildProjector (
        Array ((osmBounds.north, osmBounds.west), (osmBounds.south, osmBounds.east)),
        (3000.0, 2000.0), "CalRouteMix101_3")

    //--------------------------------------------------
    // Locate OSM nodes nearest to the measured corridor anchors
    private def squaredDistance (a: (Double, Double), b: (Double, Double)): Double =
        val dLat = a._1 - b._1
        val dLon = a._2 - b._2
        dLat * dLat + dLon * dLon
    end squaredDistance

    private def edgeDistance (edge: NetEdge, point: (Double, Double)): Double =
        if edge.geometry.nonEmpty then edge.geometry.map (p => squaredDistance (p, point)).min
        else math.min (squaredDistance ((edge.uLat, edge.uLon), point),
                       squaredDistance ((edge.vLat, edge.vLon), point))
    end edgeDistance

    private def nearestNode (point: (Double, Double), highway: String => Boolean): Long =
        val candidates = edges.filter (e => highway (e.highway))
        require (candidates.nonEmpty, "OSM artifact has no candidate corridor edges")
        candidates.flatMap (e => Array ((e.u, (e.uLat, e.uLon)), (e.v, (e.vLat, e.vLon))))
            .minBy (x => squaredDistance (x._2, point))._1
    end nearestNode

    private def nearestTerminalNode (point: (Double, Double), highway: String => Boolean): Long =
        val candidates = edges.filter (e => highway (e.highway))
            .flatMap (e => Array ((e.u, (e.uLat, e.uLon)), (e.v, (e.vLat, e.vLon))))
            .distinct.filterNot (x => outgoingNodeIds.contains (x._1))
        require (candidates.nonEmpty, "OSM artifact has no terminal corridor node")
        candidates.minBy (x => squaredDistance (x._2, point))._1
    end nearestTerminalNode

    private def nearestRampNode (point: (Double, Double)): Long =
        val edge = edges.filter (_.highway == "motorway_link").minBy (e => edgeDistance (e, point))
        edge.u
    end nearestRampNode

    private val sourceNodeId = nearestNode (gps.head, _ == "motorway")
    private val sinkNodeId = nearestTerminalNode (gps.last, _ == "motorway")
    private val rampNodeIds = Array (nearestRampNode (gps(2)), nearestRampNode (gps(5)))
    require (rampNodeIds.distinct.length == rampNodeIds.length, "CalRoute ramp sources share one OSM node")

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
    private def nodePosition (id: Long): (Double, Double) =
        val edge = edges.find (e => e.u == id || e.v == id).getOrElse (
            throw new IllegalArgumentException (s"OSM node $id has no incident edge"))
        if edge.u == id then project (edge.uLat, edge.uLon) else project (edge.vLat, edge.vLon)
    end nodePosition

    private val (sourceX, sourceY) = nodePosition (sourceNodeId)
    private val mainlineSources = Array.tabulate (numLanes) { lane =>
        new VSource (s"vsrcML_L$lane", this, () => Car (sourceNodeId, sinkNodeId), lane,
                     mainlineArrivalSources(lane).getTotalVehicles (lane),
                     mainlineArrivalSources(lane).getDistribution,
                     Array (sourceX + lane * 12.0, sourceY, 20.0, 20.0), s"M$lane")
    }

    private val rampSources = Array.tabulate (rampNodeIds.length) { r =>
        val (x, y) = nodePosition (rampNodeIds(r))
        new VSource (s"srcRamp${r + 1}", this, () => Car (rampNodeIds(r), sinkNodeId),
                     numLanes + r, rampArrivalSources(r).getTotalVehicles (0),
                     rampArrivalSources(r).getDistribution,
                     Array (x, y, 20.0, 20.0), s"R${r + 1}")
    }

    //--------------------------------------------------
    // Build the OSM graph, route the sink, and register components
    private val sourceComponents: Map [Long, Component] =
        Map (sourceNodeId -> mainlineSources(0)) ++
            rampSources.zip (rampNodeIds).map ((s, id) => id -> s)

    private lazy val graph: BuiltGraph = NetworkBuilder.build (
        edges, Array (sinkNodeId), project, motion, nt, edgePolicy, sourceComponents = sourceComponents)

    graph.routeTo (Array (graph.nodeIndex (sinkNodeId)))
    addComponent (graph.components*)
    addComponent (mainlineSources.tail*)

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
    private lazy val pemsToJunc = networkConfig.sensors.map { sensor =>
        val nodeId = nearestNode (gps(sensor.segment), _ == "motorway")
        val graphIndex = graph.nodeIndex (nodeId)
        graph.node.indices.filter (i => graph.node(i).isInstanceOf [Junction]).indexOf (graphIndex)
    }.toArray
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
