//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Sep 13 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Deterministic MesoSpec to ScalaTion Translation
 */

package scalation
package simulation
package process
package ir

import scalation.random.{Erlang, Erlang2S, Exponential, Sharp, Variate}
import scalation.dynamics.IdmFlux
import scalation.simulation.process.meso.MesoVTransport
import scalation.simulation.process.config.{CarFollowingModel, ODESolverType}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoScalationNetwork` class contains the native ScalaTion components
 *  produced by the first one-link translation.
 *  @param sources  the translated vehicle sources
 *  @param links    the translated mesoscopic links
 *  @param sinks    the translated sinks
 */
case class MesoScalationNetwork (sources: Array [VSource],
                                 links: Array [MesoVTransport],
                                 sinks: Array [Sink])


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoSpecScalation` object translates an accepted `MesoSpec` into
 *  native ScalaTion process components.  The first translation slice supports
 *  one source, one mesoscopic link and one sink.
 */
object MesoSpecScalation:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Translate a one-link MesoSpec into native ScalaTion components.
     *  Run controls and screen coordinates belong to the executable model,
     *  rather than to the semantic MesoSpec.
     *  @param spec        the accepted mesoscopic specification
     *  @param micro       the source MicroIR supplying IDM parameters
     *  @param director    the ScalaTion process model that owns the components
     *  @param makeEntity  the vehicle factory used by the source
     *  @param nStop       the number of vehicles generated for this run
     *  @param stream      the random-number stream used by the arrival process
     *  @param sourceXY    the display coordinates for the source
     *  @param sinkXY      the display coordinates for the sink
     */
    def oneLink (spec: MesoSpec, micro: MicroIR, director: Model, makeEntity: () => Vehicle,
                 nStop: Int, stream: Int = 0,
                 sourceXY: (Int, Int) = (100, 290),
                 sinkXY: (Int, Int) = (600, 290)): MesoScalationNetwork =

        spec.validateAgainst (micro)                            // bind to its claimed microscopic model
        checkOneLink (spec)                                     // enforce this translator slice

        val sourceSpec = spec.demand.sources(0)                 // sole source declaration
        val linkSpec   = spec.links(0)                          // sole mesoscopic link
        val sinkSpec   = spec.nodes.find (_.kind == MicroNodeKind.Sink).get
        val iArrivalRV = arrivalVariate (sourceSpec.arrival, stream)

        val source = VSource (sourceSpec.id, director, makeEntity, 0, nStop,
                              iArrivalRV, sourceXY)
        val sink = Sink (sinkSpec.id, sinkXY)
        val speed = linkSpec.length / linkSpec.freeFlowTime     // preserve specified traversal time
        val link = MesoVTransport (linkSpec.id, source, sink, speed, linkSpec.capacity,
                                   storageCapacity = linkSpec.storageCapacity,
                                   flowDiagram = flowDiagram (spec, micro))
        link.length = linkSpec.length                           // use physical rather than drawing length

        MesoScalationNetwork (Array (source), Array (link), Array (sink))
    end oneLink

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the one IDM equilibrium relation shared by all links in a translation.
     *  The MicroIR is the parameter authority; the MesoSpec only selects its use.
     */
    private [ir] def flowDiagram (spec: MesoSpec, micro: MicroIR): Option [IdmFlux] =
        spec.speedRelation match
            case MesoSpeedRelation.Fixed => None
            case MesoSpeedRelation.IdmEquilibrium =>
                val parameters = micro.vehicleDynamics.parameters.map (p => p.name -> p.value).toMap
                Some (new IdmFlux (parameters("maximumSpeed"), parameters("timeHeadway"),
                                   parameters("minimumGap"), parameters("vehicleLength"),
                                   parameters("delta")))
    end flowDiagram

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the structural boundary of the first translator slice.
     *  @param spec  the legal mesoscopic specification
     */
    private def checkOneLink (spec: MesoSpec): Unit =
        val sourceNodes = spec.nodes.count (_.kind == MicroNodeKind.Source)
        val sinkNodes   = spec.nodes.count (_.kind == MicroNodeKind.Sink)
        val junctions   = spec.nodes.count (_.kind == MicroNodeKind.Junction)

        if sourceNodes != 1 || sinkNodes != 1 || junctions != 0 ||
           spec.links.length != 1 || spec.movements.nonEmpty ||
           spec.demand.sources.length != 1 || spec.demand.routes.length != 1 then
            throw new UnsupportedOperationException (
                "MesoSpecScalation.oneLink supports exactly one source, one link, one route and one sink")
    end checkOneLink

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Translate a semantic arrival declaration into its native ScalaTion
     *  random variate.
     *  @param arrival  the preserved arrival-process declaration
     *  @param stream   the random-number stream for this run
     */
    private [ir] def arrivalVariate (arrival: MicroArrival, stream: Int): Variate =
        def parameter (name: String): Double =
            arrival.parameters.find (_.name == name).get.value
        end parameter

        arrival.model match
            case MicroArrivalModel.Deterministic => Sharp (parameter ("x"), stream)
            case MicroArrivalModel.Exponential   => Exponential (parameter ("mu"), stream)
            case MicroArrivalModel.Erlang        =>
                Erlang (parameter ("mu"), parameter ("k").toInt, stream)
            case MicroArrivalModel.Erlang2S      =>
                Erlang2S (parameter ("mu"), parameter ("tau"), stream)
    end arrivalVariate

end MesoSpecScalation


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoSpecScalationModel` class executes the first deterministic
 *  MesoSpec-to-ScalaTion translation slice.
 *  @param spec       the accepted one-link MesoSpec
 *  @param micro      the MicroIR bound to that specification
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed to animation speed
 *  @param nStop      the number of vehicles generated for this run
 *  @param stream     the random-number stream used for arrivals
 */
class MesoSpecScalationModel (spec: MesoSpec, micro: MicroIR,
                              name: String = "MesoSpecScalationModel",
                              reps: Int = 1, animating: Boolean = false,
                              aniRatio: Double = 500.0, val nStop: Int = 4,
                              stream: Int = 0)
      extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Translate and Register Model Components

    val network = MesoSpecScalation.oneLink (spec, micro, this, () => Car (), nStop, stream)
    val source  = network.sources(0)                            // generated vehicle source
    val link    = network.links(0)                              // translated mesoscopic link
    val sink    = network.sinks(0)                              // terminal sink

    addComponent (source, sink, link)                           // endpoints precede transport

    //--------------------------------------------------
    // Specify the Vehicle Script

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            link.move ()                                       // traverse translated link
            sink.leave ()                                      // terminate at translated sink
        end act

    end Car

    simulate ()
    waitFinished ()

end MesoSpecScalationModel


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runMesoSpecScalation` main function tests deterministic translation of
 *  a one-link MesoSpec into an executable ScalaTion model.
 *  > runMain scalation.simulation.process.ir.runMesoSpecScalation
 */
@main def runMesoSpecScalation (): Unit =

    banner ("MesoSpec to ScalaTion test")

    val arrival     = MicroArrival (MicroArrivalModel.Deterministic,
                                    Array (MicroParameter ("x", 0.5))) // deterministic arrivals
    val microNodes  = Array (MicroNode ("entry", MicroNodeKind.Source),
                             MicroNode ("exit", MicroNodeKind.Sink))
    val microSegment = MicroSegment ("micro-link-0", "entry", "exit",
                                     500.0, 1, 10.0)            // source road segment
    val microSource  = MicroSource ("entry", "entry", arrival) // source demand
    val microRoute   = MicroRoute ("route-0", "entry", "exit",
                                   Array ("micro-link-0"), 1.0) // microscopic route
    val dynamics     = MicroVehicleDynamics (
        CarFollowingModel.IDM,
        ODESolverType.Ballistic,
        Array (MicroParameter ("timeHeadway", 1.5),
               MicroParameter ("minimumGap", 2.0)),
        0.5)
    val micro = MicroIR (
        MicroIR.version,
        MicroNetwork (microNodes, Array (microSegment)),
        MicroDemand (Array (microSource), Array (microRoute)),
        dynamics,
        Array (MicroObservable (MicroMeasure.Flow, "micro-link-0"),
               MicroObservable (MicroMeasure.TravelTime, "route-0")))

    val source = MesoSource ("entry", "entry", arrival)        // translated source demand
    val route  = MesoRoute ("route-0", "entry", "exit",
                            Array ("link-0"), 1.0)               // translated route
    val spec = MesoSpec (
        MesoSpec.version,
        micro.version,
        micro.sha256,
        MesoSpeedRelation.Fixed,
        Array (MesoNode ("entry", MicroNodeKind.Source),
               MesoNode ("exit", MicroNodeKind.Sink)),
        Array (MesoLink ("link-0", Array ("micro-link-0"), "entry", "exit",
                         500.0, 50.0, 1800.0, 2)),
        Array.ofDim [MesoMovement] (0),
        MesoDemand (Array (source), Array (route)),
        Array (MesoObservable (MicroMeasure.Flow, "link-0"),
               MesoObservable (MicroMeasure.TravelTime, "route-0")),
        MesoSpec.eventOrder)

    spec.validateAgainst (micro)                               // accept this abstraction
    val model = new MesoSpecScalationModel (spec, micro)
    val completed = model.sink.durationStat.num                // completed translated trips

    assert (model.link.enteredCount == model.nStop)
    assert (model.link.releasedCount == model.nStop)
    assert (completed == model.nStop)
    assert (model.link.occupancy == 0)
    assert (model.link.blockedCount == 0)
    assert (model.link.persistentStat.max <= model.link.storageCapacity)
    assert (model.link.flowDiagram.isEmpty)                     // fixed-speed one-link baseline stays unchanged

    banner ("MesoSpec to ScalaTion test: PASS")
    println (s"mesospec_sha256 = ${spec.sha256}")
    println (s"entered=${model.link.enteredCount}, released=${model.link.releasedCount}, " +
             s"completed=$completed")
    println (s"maximum occupancy=${model.link.persistentStat.max.toInt}, " +
             s"storage=${model.link.storageCapacity}")
    Model.shutdown ()

end runMesoSpecScalation
