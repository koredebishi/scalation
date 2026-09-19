//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishi
 *  @version 2.0
 *  @date    Sat Sep 19 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    MesoSpec to Same-Actor Multi-Link Mixed ScalaTion Translation
 */

package scalation
package simulation
package process
package ir

import scalation.mathstat.VectorD
import scalation.simulation.process.config.{CarFollowingModel, ODESolverType}
import scalation.simulation.process.meso.MesoVTransport

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Execute a linear MicroIR route with a microscopic prefix and suffix around
 *  two consecutive IDM-flux mesoscopic links.  The two mesoscopic links are
 *  separate storage/FIFO owners, but the vehicle remains the same actor on the
 *  one Model agenda throughout the route.
 */
class MesoSpecMixedMultiScalationModel (spec: MesoSpec, micro: MicroIR,
                                        name: String = "MesoSpecMixedMultiScalationModel",
                                        reps: Int = 1, animating: Boolean = false,
                                        aniRatio: Double = 500.0, val nStop: Int = 4,
                                        stream: Int = 0)
      extends Model (name, reps, animating, aniRatio):

    private val (microRoute, microSource, mesoSpecs, prefixSegment, suffixSegment) =
        MesoSpecMixedMultiScalation.check (spec, micro)
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
        IDMDynamics.integratorType = MesoSpecMixedMultiScalation.integrator (dynamics.odeSolver)
    end configureVehicle

    configureVehicle ()

    private val segmentMap = micro.network.segments.map (s => s.id -> s).toMap
    private val routeNodes =
        Array (segmentMap(microRoute.segments(0)).from) ++
        microRoute.segments.map (id => segmentMap(id).to)
    private val nodeMap = micro.network.nodes.map (n => n.id -> n).toMap

    private def position (nodeIndex: Int): (Double, Double) =
        val distance = microRoute.segments.take (nodeIndex).map (id => segmentMap(id).length).sum
        (100.0 + distance / 2.0, 300.0)
    end position

    private def integerPosition (nodeIndex: Int): (Int, Int) =
        val xy = position (nodeIndex)
        (xy._1.toInt, xy._2.toInt)
    end integerPosition

    private val runtimeJunctions = (1 until routeNodes.length - 1).map { i =>
        val id = routeNodes(i)
        require (nodeMap(id).kind == MicroNodeKind.Junction,
                 "intermediate MicroIR route node '" + id + "' must be a junction")
        id -> Junction (id, position (i), nt = 2, nl = 1)
    }.toMap

    private val boundaryIn  = runtimeJunctions(routeNodes(1))
    private val mesoMiddle  = runtimeJunctions(routeNodes(2))
    private val boundaryOut = runtimeJunctions(routeNodes(3))

    private val source = VSource (microSource.id, this, () => Car (), 0, nStop,
                                  MesoSpecScalation.arrivalVariate (microSource.arrival, stream),
                                  integerPosition (0))
    private val sink = Sink (microRoute.destination, integerPosition (routeNodes.length - 1))

    val microIn = Pathway ("micro-in", Array.empty [Junction], source, boundaryIn, IDMDynamics,
                           laneShift = VectorD (0.0, 0.0))
    val mesoLinks = mesoSpecs.map { linkSpec =>
        val from = if linkSpec.from == mesoSpecs(0).from then boundaryIn else mesoMiddle
        val to   = if linkSpec.to == mesoSpecs(1).to then boundaryOut else mesoMiddle
        val link = MesoVTransport (linkSpec.id, from, to,
                                   linkSpec.length / linkSpec.freeFlowTime,
                                   linkSpec.capacity,
                                   storageCapacity = linkSpec.storageCapacity,
                                   flowDiagram = MesoSpecScalation.flowDiagram (spec, micro))
        link.length = linkSpec.length
        link
    }
    val microOut = Pathway ("micro-out", Array.empty [Junction], boundaryOut, sink, IDMDynamics,
                            laneShift = VectorD (0.0, 0.0))

    microIn.seg(0).length = prefixSegment.length
    microOut.seg(0).length = suffixSegment.length

    addComponent (source, boundaryIn, mesoMiddle, boundaryOut, sink)
    addComponent (microIn)
    mesoLinks.foreach (addComponent (_))
    addComponent (microOut)

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            val originalId = id

            microIn.joinAt (this, microIn.getLast, 0)
            microIn.seg(0).move ()
            boundaryIn.jump ()
            microIn.removeFromAlist (this)
            require (myPathNode == null && myPathway == null,
                     "vehicle must leave the microscopic DLL before mesoscopic traversal")

            mesoLinks(0).move ()
            mesoMiddle.jump ()
            mesoLinks(1).move ()
            require (id == originalId, "vehicle identity changed in the mesoscopic links")
            require (myPathNode == null && myPathway == null,
                     "mesoscopic traversal must not create microscopic DLL membership")

            boundaryOut.jump ()
            microOut.joinAt (this, microOut.mergeLeaderAt (0), 0)
            microOut.seg(0).move ()
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
    for link <- mesoLinks do
        require (link.enteredCount == nStop, "mesoscopic link did not admit all vehicles")
        require (link.releasedCount == nStop, "mesoscopic link did not release all vehicles")
        require (link.occupancy == 0 && link.blockedCount == 0,
                 "mesoscopic link did not drain after the run")
    require (microIn.vList.isEmpty && microOut.vList.isEmpty,
             "a vehicle remains in a microscopic DLL after the run")

end MesoSpecMixedMultiScalationModel


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Boundary checks for the first multi-link mixed translation slice. */
private object MesoSpecMixedMultiScalation:

    def check (spec: MesoSpec, micro: MicroIR):
            (MicroRoute, MicroSource, Array [MesoLink], MicroSegment, MicroSegment) =
        spec.validateAgainst (micro)
        require (micro.vehicleDynamics.model == CarFollowingModel.IDM,
                 "mixed translation currently requires IDM microscopic dynamics")
        require (spec.speedRelation == MesoSpeedRelation.IdmEquilibrium,
                 "mixed translation currently requires the IDM equilibrium speed relation")
        require (micro.demand.sources.length == 1 && micro.demand.routes.length == 1,
                 "mixed translation currently supports one MicroIR source and route")
        require (spec.demand.sources.length == 1 && spec.demand.routes.length == 1,
                 "mixed translation currently supports one MesoSpec source and route")
        require (spec.links.length == 2 && spec.movements.length == 1,
                 "multi-link mixed translation requires two links and one movement")

        val route = micro.demand.routes(0)
        val source = micro.demand.sources(0)
        val mesoLinks = spec.demand.routes(0).links.map (id => spec.links.find (_.id == id).get)
        require (mesoLinks.length == 2 && mesoLinks(0).to == mesoLinks(1).from,
                 "the two mesoscopic links must be consecutive")
        require (route.segments.length == 4,
                 "the first multi-link slice requires micro/meso/meso/micro")
        require (mesoLinks(0).microSegments.sameElements (Array (route.segments(1))) &&
                 mesoLinks(1).microSegments.sameElements (Array (route.segments(2))),
                 "the two MesoLinks must map to the two middle MicroIR segments")
        require (spec.demand.routes(0).links.sameElements (mesoLinks.map (_.id)),
                 "MesoSpec route order must match its two links")
        require (math.abs (route.probability - 1.0) <= 1E-9 &&
                 math.abs (spec.demand.routes(0).probability - 1.0) <= 1E-9,
                 "the single route in each representation must have probability 1")
        require (sameArrival (source.arrival, spec.demand.sources(0).arrival),
                 "MesoSpec arrival must match the MicroIR source arrival")
        val segments = route.segments.map (micro.network.segments.map (s => s.id -> s).toMap)
        require (segments.forall (_.lanes == 1), "mixed translation currently supports one lane")
        (route, source, mesoLinks, segments(0), segments(3))
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

end MesoSpecMixedMultiScalation


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Deterministic smoke test for two consecutive IDM-flux mesoscopic links. */
@main def runMesoSpecMixedMultiScalation (): Unit =

    banner ("Multi-link mixed MesoSpec to ScalaTion test")

    val arrival = MicroArrival (MicroArrivalModel.Deterministic,
                                Array (MicroParameter ("x", 1.0)))
    val nodes = Array (MicroNode ("entry", MicroNodeKind.Source),
                       MicroNode ("boundary-in", MicroNodeKind.Junction),
                       MicroNode ("meso-middle", MicroNodeKind.Junction),
                       MicroNode ("boundary-out", MicroNodeKind.Junction),
                       MicroNode ("exit", MicroNodeKind.Sink))
    val segments = Array (
        MicroSegment ("micro-in-seg", "entry", "boundary-in", 250.0, 1, 30.0),
        MicroSegment ("meso-seg-0", "boundary-in", "meso-middle", 400.0, 1, 30.0),
        MicroSegment ("meso-seg-1", "meso-middle", "boundary-out", 400.0, 1, 30.0),
        MicroSegment ("micro-out-seg", "boundary-out", "exit", 250.0, 1, 30.0))
    val source = MicroSource ("entry-demand", "entry", arrival)
    val route = MicroRoute ("full-route", "entry", "exit", segments.map (_.id), 1.0)
    val dynamics = MicroVehicleDynamics (
        CarFollowingModel.IDM, ODESolverType.Ballistic,
        Array (MicroParameter ("initialSpeed", 20.0),
               MicroParameter ("maximumSpeed", 30.0),
               MicroParameter ("maximumAcceleration", 4.0),
               MicroParameter ("comfortableDeceleration", 2.0),
               MicroParameter ("timeHeadway", 1.0),
               MicroParameter ("minimumGap", 2.0),
               MicroParameter ("vehicleLength", 4.0),
               MicroParameter ("delta", 4.0)), 0.5)
    val micro = MicroIR (MicroIR.version,
                         MicroNetwork (nodes, segments),
                         MicroDemand (Array (source), Array (route)),
                         dynamics,
                         Array (MicroObservable (MicroMeasure.Flow, "meso-seg-0"),
                                MicroObservable (MicroMeasure.TravelTime, "full-route")))
    micro.validate ()

    val mesoSource = MesoSource ("meso-entry-demand", "boundary-in", arrival)
    val mesoRoute = MesoRoute ("meso-route", "boundary-in", "boundary-out",
                               Array ("meso-link-0", "meso-link-1"), 1.0)
    val mesoLink0 = MesoLink ("meso-link-0", Array ("meso-seg-0"),
                              "boundary-in", "meso-middle", 400.0, 400.0 / 30.0,
                              1800.0, 8)
    val mesoLink1 = MesoLink ("meso-link-1", Array ("meso-seg-1"),
                              "meso-middle", "boundary-out", 400.0, 400.0 / 30.0,
                              1800.0, 8)
    val movement = MesoMovement ("meso-move", "meso-middle", "meso-link-0",
                                 "meso-link-1", Array ("meso-route"))
    val spec = MesoSpec (MesoSpec.version, micro.version, micro.sha256,
                         MesoSpeedRelation.IdmEquilibrium,
                         Array (MesoNode ("boundary-in", MicroNodeKind.Source),
                                MesoNode ("meso-middle", MicroNodeKind.Junction),
                                MesoNode ("boundary-out", MicroNodeKind.Sink)),
                         Array (mesoLink0, mesoLink1), Array (movement),
                         MesoDemand (Array (mesoSource), Array (mesoRoute)),
                         Array (MesoObservable (MicroMeasure.Flow, "meso-link-0"),
                                MesoObservable (MicroMeasure.TravelTime, "meso-route")),
                         MesoSpec.eventOrder)
    spec.validateAgainst (micro)

    val model = new MesoSpecMixedMultiScalationModel (spec, micro)
    require (model.mesoLinks.forall (_.flowDiagram.nonEmpty),
             "each mesoscopic link must receive the IDM flux relation")
    banner ("Multi-link mixed MesoSpec to ScalaTion test: PASS")
    println ("mesospec_sha256 = " + spec.sha256)
    println ("micro segments=2, meso links=" + model.mesoLinks.length + ", micro segments=2")
    println ("completed=" + model.completed + ", entries=" +
             model.mesoLinks.map (_.enteredCount).mkString ("/"))
    println ("releases=" + model.mesoLinks.map (_.releasedCount).mkString ("/"))
    println ("agenda activations=" + model.agendaActivations)
    Model.shutdown ()

end runMesoSpecMixedMultiScalation
