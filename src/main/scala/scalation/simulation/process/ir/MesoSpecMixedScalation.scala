//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishi
 *  @version 2.0
 *  @date    Thu Sep 17 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    MesoSpec to Same-Actor Mixed Micro-Meso ScalaTion Translation
 */

package scalation
package simulation
package process
package ir

import scalation.mathstat.VectorD
import scalation.simulation.process.config.{CarFollowingModel, ODESolverType}
import scalation.simulation.process.meso.MesoVTransport

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A first mixed-resolution ScalaTion translation.
 *  The supported slice is one microscopic source and route, with one interior
 *  MesoLink replacing a contiguous run of that route's microscopic segments.
 *  The remaining route segments execute as native Pathway components.
 *
 *  This translator owns only the translation wiring.  Vehicle scheduling,
 *  microscopic DLLs, mesoscopic storage, and the shared agenda remain owned by
 *  their existing Scalation classes.
 */
class MesoSpecMixedScalationModel (spec: MesoSpec, micro: MicroIR,
                                   name: String = "MesoSpecMixedScalationModel",
                                   reps: Int = 1, animating: Boolean = false,
                                   aniRatio: Double = 500.0, val nStop: Int = 4,
                                   stream: Int = 0)
      extends Model (name, reps, animating, aniRatio):

    private val (microRoute, microSource, linkSpec, prefixSegments, mesoSegments,
                 suffixSegments) = MesoSpecMixedScalation.check (spec, micro)

    private val dynamics = micro.vehicleDynamics
    private val parameters = dynamics.parameters.map (p => p.name -> p.value).toMap
    private val savedVehicleProps = Vehicle.prop.clone ()
    private val savedIntegrator = IDMDynamics.integratorType

    private def configureVehicle (): Unit =
        val required = Array ("initialSpeed", "maximumSpeed", "maximumAcceleration",
                              "comfortableDeceleration", "timeHeadway", "minimumGap",
                              "vehicleLength", "delta")
        for key <- required do
            require (parameters.contains (key), "MicroIR is missing IDM parameter '" + key + "'")
        require (parameters("maximumAcceleration") > 0.0,
                 "MicroIR maximumAcceleration must be positive")
        require (parameters("comfortableDeceleration") > 0.0,
                 "MicroIR comfortableDeceleration must be positive")
        require (parameters("initialSpeed") >= 0.0 &&
                 parameters("initialSpeed") <= parameters("maximumSpeed"),
                 "MicroIR initialSpeed must be in [0, maximumSpeed]")
        require (parameters("maximumSpeed") > 0.0 &&
                 parameters("timeHeadway") > 0.0 &&
                 parameters("minimumGap") >= 0.0 &&
                 parameters("vehicleLength") > 0.0 &&
                 parameters("delta") > 0.0,
                 "MicroIR IDM parameters are outside their valid ranges")
        val props = Vehicle.def_prop.clone ()
        props("rt")   = dynamics.updateInterval
        props("amax") = parameters("maximumAcceleration")
        props("bmax") = -parameters("comfortableDeceleration")
        props("v0")   = parameters("initialSpeed")
        props("vmax") = parameters("maximumSpeed")
        props("T")    = parameters("timeHeadway")
        props("s")    = parameters("minimumGap")
        props("len")  = parameters("vehicleLength")
        props("del")  = parameters("delta")
        Vehicle.setProps (props)
        IDMDynamics.integratorType = MesoSpecMixedScalation.integrator (dynamics.odeSolver)
    end configureVehicle

    configureVehicle ()

    //--------------------------------------------------
    // Materialize the route's original node coordinates as runtime junctions.

    private val microSegments = micro.network.segments.map (s => s.id -> s).toMap
    private val routeNodes =
        Array (microSegments(microRoute.segments(0)).from) ++
        microRoute.segments.map (id => microSegments(id).to)
    private val nodeById = micro.network.nodes.map (n => n.id -> n).toMap

    private def routeDistance (nodeIndex: Int): Double =
        microRoute.segments.take (nodeIndex).map (id => microSegments(id).length).sum
    end routeDistance

    private def position (nodeIndex: Int): (Double, Double) =
        (100.0 + routeDistance (nodeIndex) / 2.0, 300.0)
    end position

    private def sourceCoordinates (nodeIndex: Int): (Int, Int) =
        val at = position (nodeIndex)
        (at._1.toInt, at._2.toInt)
    end sourceCoordinates

    private val routeJunctionEntries = (1 until routeNodes.length - 1).map { i =>
        val nodeId = routeNodes(i)
        require (nodeById(nodeId).kind == MicroNodeKind.Junction,
                 "intermediate MicroIR route node '" + nodeId + "' must be a junction")
        nodeId -> Junction (nodeId, position (i), nt = 2, nl = 1)
    }
    private val routeJunctions = routeJunctionEntries.toMap

    private val prefixCount = prefixSegments.length
    private val mesoEndNodeIndex = prefixCount + mesoSegments.length
    private val boundaryIn = routeJunctions(routeNodes(prefixCount))
    private val boundaryOut = routeJunctions(routeNodes(mesoEndNodeIndex))

    private val source = VSource (microSource.id, this, () => Car (), 0, nStop,
                                  MesoSpecScalation.arrivalVariate (microSource.arrival, stream),
                                  sourceCoordinates (0))
    private val sink = Sink (microRoute.destination, sourceCoordinates (routeNodes.length - 1))

    private val prefixJunctions =
        (1 until prefixCount).map (i => routeJunctions(routeNodes(i))).toArray
    private val suffixJunctions =
        (mesoEndNodeIndex + 1 until routeNodes.length - 1)
            .map (i => routeJunctions(routeNodes(i))).toArray

    val microIn = Pathway ("micro-in", prefixJunctions, source, boundaryIn, IDMDynamics,
                           laneShift = VectorD (0.0, 0.0))
    val meso = MesoVTransport (linkSpec.id, boundaryIn, boundaryOut,
                               linkSpec.length / linkSpec.freeFlowTime, linkSpec.capacity,
                               storageCapacity = linkSpec.storageCapacity,
                               flowDiagram = MesoSpecScalation.flowDiagram (spec, micro))
    val microOut = Pathway ("micro-out", suffixJunctions, boundaryOut, sink, IDMDynamics,
                            laneShift = VectorD (0.0, 0.0))

    for i <- prefixSegments.indices do microIn.seg(i).length = prefixSegments(i).length
    meso.length = linkSpec.length
    for i <- suffixSegments.indices do microOut.seg(i).length = suffixSegments(i).length

    addComponent (source)
    routeJunctionEntries.foreach (entry => addComponent (entry._2))
    addComponent (sink)                                        // register endpoints before transport
    addComponent (microIn, meso, microOut)

    //--------------------------------------------------
    // Keep one Vehicle actor as it moves through both native representations.

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            val originalId = id

            microIn.joinAt (this, microIn.getLast, 0)
            for i <- microIn.seg.indices do
                microIn.seg(i).move ()
                if i < microIn.junc.length then microIn.junc(i).jump ()
            end for
            boundaryIn.jump ()
            microIn.removeFromAlist (this)
            require (myPathNode == null && myPathway == null,
                     "vehicle must leave the microscopic DLL before mesoscopic traversal")

            meso.move ()
            require (id == originalId, "vehicle identity changed in the mesoscopic link")
            require (myPathNode == null && myPathway == null,
                     "mesoscopic traversal must not create microscopic DLL membership")

            boundaryOut.jump ()
            microOut.joinAt (this, microOut.mergeLeaderAt (0), 0)
            for i <- microOut.seg.indices do
                microOut.seg(i).move ()
                if i < microOut.junc.length then microOut.junc(i).jump ()
            end for
            microOut.removeFromAlist (this)
            require (id == originalId, "vehicle identity changed before sink completion")
            sink.leave ()
        end act

    end Car

    try
        simulate ()
        waitFinished ()
    finally
        Vehicle.setProps (savedVehicleProps)
        IDMDynamics.integratorType = savedIntegrator
    end try

    val completed: Int = sink.durationStat.num.toInt
    require (completed == nStop, "completed " + completed + " vehicles, expected " + nStop)
    require (meso.enteredCount == nStop, "mesoscopic entry count does not match generated demand")
    require (meso.releasedCount == nStop, "mesoscopic release count does not match generated demand")
    require (meso.occupancy == 0 && meso.blockedCount == 0,
             "mesoscopic link did not drain after the run")
    require (microIn.vList.isEmpty && microOut.vList.isEmpty,
             "a vehicle remains in a microscopic DLL after the run")

end MesoSpecMixedScalationModel


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Translation checks and the small first-slice boundary are local to this
 *  backend; MesoSpec and MicroIR retain their own general-purpose validation.
 */
private object MesoSpecMixedScalation:

    def check (spec: MesoSpec, micro: MicroIR):
            (MicroRoute, MicroSource, MesoLink, Array [MicroSegment],
             Array [MicroSegment], Array [MicroSegment]) =
        spec.validateAgainst (micro)
        require (micro.vehicleDynamics.model == CarFollowingModel.IDM,
                 "mixed translation currently requires IDM microscopic dynamics")
        require (spec.speedRelation == MesoSpeedRelation.IdmEquilibrium,
                 "mixed translation currently requires the IDM equilibrium speed relation")
        require (spec.links.length == 1 && spec.movements.isEmpty,
                 "mixed translation currently supports one MesoLink and no mesoscopic movements")
        require (micro.demand.sources.length == 1 && micro.demand.routes.length == 1,
                 "mixed translation currently supports one MicroIR source and route")
        require (spec.demand.sources.length == 1 && spec.demand.routes.length == 1,
                 "mixed translation currently supports one MesoSpec source and route")

        val route = micro.demand.routes(0)
        val source = micro.demand.sources(0)
        val link = spec.links(0)
        val mesoSource = spec.demand.sources(0)
        val mesoRoute = spec.demand.routes(0)
        require (sameArrival (source.arrival, mesoSource.arrival),
                 "MesoSpec arrival must match the MicroIR source arrival")
        require (math.abs (route.probability - 1.0) <= 1E-9 &&
                 math.abs (mesoRoute.probability - 1.0) <= 1E-9,
                 "the single route in each representation must have probability 1")
        require (mesoRoute.links.sameElements (Array (link.id)),
                 "the MesoSpec route must traverse its sole MesoLink")

        val segmentMap = micro.network.segments.map (s => s.id -> s).toMap
        val mappedIndices = link.microSegments.map (id => route.segments.indexOf (id))
        require (mappedIndices.nonEmpty && mappedIndices.forall (_ >= 0),
                 "the MesoLink must map to segments on the MicroIR route")
        val start = mappedIndices(0)
        require (mappedIndices.sameElements (Array.range (start, start + mappedIndices.length)),
                 "the MesoLink mapping must be a contiguous ordered route block")
        require (start > 0 && start + mappedIndices.length < route.segments.length,
                 "the MesoLink must have microscopic segments before and after it")

        val mappedSegments = route.segments.map (segmentMap)
        require (mappedSegments.forall (_.lanes == 1),
                 "mixed Pathway translation currently supports one lane")
        val prefix = mappedSegments.take (start)
        val middle = mappedSegments.slice (start, start + mappedIndices.length)
        val suffix = mappedSegments.drop (start + mappedIndices.length)
        val nodeMap = micro.network.nodes.map (n => n.id -> n).toMap
        require (nodeMap(prefix.last.to).kind == MicroNodeKind.Junction &&
                 nodeMap(suffix.head.from).kind == MicroNodeKind.Junction,
                 "both micro-meso boundaries must be microscopic junctions")
        (route, source, link, prefix, middle, suffix)
    end check

    private def sameArrival (x: MicroArrival, y: MicroArrival): Boolean =
        x.model == y.model &&
        x.parameters.map (p => p.name -> p.value).sortBy (_._1)
            .sameElements (y.parameters.map (p => p.name -> p.value).sortBy (_._1))
    end sameArrival

    def integrator (solver: ODESolverType): IntegratorType = solver match
        case ODESolverType.Ballistic => IntegratorType.Ballistic
        case ODESolverType.Euler     => IntegratorType.Euler
        case ODESolverType.Heun      => IntegratorType.Heun
        case ODESolverType.RK2       => IntegratorType.RK2
        case ODESolverType.RK3       => IntegratorType.RK3
        case ODESolverType.RK4       => IntegratorType.RK4
        case ODESolverType.DOPRI5    => IntegratorType.DOPRI5
        case ODESolverType.Butcher   => IntegratorType.butcher
    end integrator

end MesoSpecMixedScalation


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Build and run one small MicroIR -> MesoSpec -> mixed ScalaTion example.
 *  This entry point is in the same source file, following the repository's
 *  executable-test convention.
 */
@main def runMesoSpecMixedScalation (): Unit =

    banner ("Mixed MesoSpec to ScalaTion test")

    val arrival = MicroArrival (MicroArrivalModel.Deterministic,
                                Array (MicroParameter ("x", 1.0)))
    val nodes = Array (
        MicroNode ("entry", MicroNodeKind.Source),
        MicroNode ("boundary-in", MicroNodeKind.Junction),
        MicroNode ("boundary-out", MicroNodeKind.Junction),
        MicroNode ("exit", MicroNodeKind.Sink))
    val segments = Array (
        MicroSegment ("micro-in-seg", "entry", "boundary-in", 300.0, 1, 30.0),
        MicroSegment ("meso-seg", "boundary-in", "boundary-out", 500.0, 1, 30.0),
        MicroSegment ("micro-out-seg", "boundary-out", "exit", 300.0, 1, 30.0))
    val source = MicroSource ("entry-demand", "entry", arrival)
    val route = MicroRoute ("full-route", "entry", "exit",
                            segments.map (_.id), 1.0)
    val dynamics = MicroVehicleDynamics (
        CarFollowingModel.IDM,
        ODESolverType.Ballistic,
        Array (MicroParameter ("initialSpeed", 20.0),
               MicroParameter ("maximumSpeed", 30.0),
               MicroParameter ("maximumAcceleration", 4.0),
               MicroParameter ("comfortableDeceleration", 2.0),
               MicroParameter ("timeHeadway", 1.0),
               MicroParameter ("minimumGap", 2.0),
               MicroParameter ("vehicleLength", 4.0),
               MicroParameter ("delta", 4.0)),
        0.5)
    val micro = MicroIR (
        MicroIR.version,
        MicroNetwork (nodes, segments),
        MicroDemand (Array (source), Array (route)),
        dynamics,
        Array (MicroObservable (MicroMeasure.Flow, "meso-seg"),
               MicroObservable (MicroMeasure.TravelTime, "full-route")))
    micro.validate ()

    val mesoSource = MesoSource ("meso-entry-demand", "boundary-in", arrival)
    val mesoRoute = MesoRoute ("meso-route", "boundary-in", "boundary-out",
                               Array ("meso-link"), 1.0)
    val spec = MesoSpec (
        MesoSpec.version,
        micro.version,
        micro.sha256,
        MesoSpeedRelation.IdmEquilibrium,
        Array (MesoNode ("boundary-in", MicroNodeKind.Source),
               MesoNode ("boundary-out", MicroNodeKind.Sink)),
        Array (MesoLink ("meso-link", Array ("meso-seg"), "boundary-in", "boundary-out",
                         500.0, 500.0 / 30.0, 1800.0, 8)),
        Array.ofDim [MesoMovement] (0),
        MesoDemand (Array (mesoSource), Array (mesoRoute)),
        Array (MesoObservable (MicroMeasure.Flow, "meso-link"),
               MesoObservable (MicroMeasure.TravelTime, "meso-route")),
        MesoSpec.eventOrder)

    spec.validateAgainst (micro)
    val model = new MesoSpecMixedScalationModel (spec, micro)
    require (model.completed == model.nStop, "not all vehicles completed the mixed route")
    require (model.meso.flowDiagram.nonEmpty, "the IDM-derived flux table was not attached")

    banner ("Mixed MesoSpec to ScalaTion test: PASS")
    println ("mesospec_sha256 = " + spec.sha256)
    println ("micro_in_segments=" + model.microIn.seg.length +
             ", meso_links=1, micro_out_segments=" + model.microOut.seg.length)
    println ("completed=" + model.completed +
             ", meso entered=" + model.meso.enteredCount +
             ", meso released=" + model.meso.releasedCount)
    println ("agenda activations=" + model.agendaActivations)
    Model.shutdown ()

end runMesoSpecMixedScalation
