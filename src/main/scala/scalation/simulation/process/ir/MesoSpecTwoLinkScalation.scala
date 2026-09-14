//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Sep 13 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Deterministic Multi-Link MesoSpec to ScalaTion Translation
 */

package scalation
package simulation
package process
package ir

import scalation.random.{Erlang, Erlang2S, Exponential, Sharp, Variate}
import scalation.simulation.process.config.{CarFollowingModel, ODESolverType}
import scalation.simulation.process.meso.MesoVTransport

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoScalationLinearNetwork` class contains the native ScalaTion
 *  components for one translated linear route.
 *  @param source     the translated vehicle source
 *  @param junctions  the ordered intermediate junctions
 *  @param links      the ordered mesoscopic links
 *  @param sink       the translated terminal sink
 */
case class MesoScalationLinearNetwork (source: VSource,
                                       junctions: Array [Junction],
                                       links: Array [MesoVTransport],
                                       sink: Sink)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoSpecTwoLinkScalation` object translates an accepted linear
 *  `MesoSpec` containing two or more links into native ScalaTion components.
 */
object MesoSpecTwoLinkScalation:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Translate one linear MesoSpec route containing two or more links.
     *  Screen coordinates and run controls remain executable-model concerns.
     *  @param spec        the accepted mesoscopic specification
     *  @param director    the ScalaTion process model that owns the components
     *  @param makeEntity  the vehicle factory used by the source
     *  @param nStop       the number of vehicles generated for this run
     *  @param stream      the random-number stream used by the arrival process
     *  @param nt          the number of junction recording intervals
     *  @param sourceXY    the display coordinates for the source
     *  @param sinkXY      the display coordinates for the sink
     */
    def linear (spec: MesoSpec, director: Model, makeEntity: () => Vehicle,
                nStop: Int, stream: Int = 0, nt: Int = 1,
                sourceXY: (Int, Int) = (100, 290),
                sinkXY: (Int, Int) = (900, 290)): MesoScalationLinearNetwork =

        spec.validate ()                                        // accept only a legal MesoSpec
        checkLinear (spec)                                      // enforce this translator slice

        val sourceSpec = spec.demand.sources(0)                 // sole source declaration
        val routeSpec  = spec.demand.routes(0)                  // sole ordered route
        val linkMap    = spec.links.map (e => e.id -> e).toMap  // specification link lookup
        val linkSpecs  = routeSpec.links.map (linkMap)           // links in traversal order
        val iArrivalRV = arrivalVariate (sourceSpec.arrival, stream)

        val source = VSource (sourceSpec.id, director, makeEntity, 0, nStop,
                              iArrivalRV, sourceXY)
        val sink = Sink (routeSpec.destination, sinkXY)

        val junctions = Array.ofDim [Junction] (linkSpecs.length - 1)
        cfor (0, junctions.length) { i =>
            val nodeId = linkSpecs(i).to                       // shared link boundary
            val xy = nodeXY (i + 1, linkSpecs.length, sourceXY, sinkXY)
            junctions(i) = Junction (nodeId, Sharp (0.0), xy, nt, 1)
        }
        val junctionMap = junctions.map (j => j.name -> j).toMap // runtime node lookup

        def component (nodeId: String): Component =
            if nodeId == sourceSpec.node then source
            else if nodeId == routeSpec.destination then sink
            else junctionMap(nodeId)
        end component

        val links = Array.ofDim [MesoVTransport] (linkSpecs.length)
        cfor (0, links.length) { i =>
            val linkSpec = linkSpecs(i)                         // semantic link declaration
            val speed = linkSpec.length / linkSpec.freeFlowTime // preserve traversal time
            links(i) = MesoVTransport (linkSpec.id,
                component (linkSpec.from), component (linkSpec.to), speed,
                linkSpec.capacity, storageCapacity = linkSpec.storageCapacity)
            links(i).length = linkSpec.length                   // use physical road length
        }

        MesoScalationLinearNetwork (source, junctions, links, sink)
    end linear

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the structural boundary of the linear translator.
     *  @param spec  the legal mesoscopic specification
     */
    private def checkLinear (spec: MesoSpec): Unit =
        val sourceNodes = spec.nodes.count (_.kind == MicroNodeKind.Source)
        val sinkNodes   = spec.nodes.count (_.kind == MicroNodeKind.Sink)
        val junctions   = spec.nodes.count (_.kind == MicroNodeKind.Junction)
        val routes      = spec.demand.routes

        val wrongShape = sourceNodes != 1 || sinkNodes != 1 ||
                         spec.demand.sources.length != 1 || routes.length != 1
        val wrongPath = !wrongShape &&
                        (routes(0).links.length < 2 ||
                         routes(0).links.distinct.length != routes(0).links.length ||
                         spec.links.map (_.id).toSet != routes(0).links.toSet ||
                         junctions != routes(0).links.length - 1)
        if wrongShape || wrongPath then
            throw new UnsupportedOperationException (
                "MesoSpecTwoLinkScalation supports one linear route containing two or more links")
    end checkLinear

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Translate a semantic arrival declaration into its native ScalaTion
     *  random variate.
     *  @param arrival  the preserved arrival-process declaration
     *  @param stream   the random-number stream for this run
     */
    private def arrivalVariate (arrival: MicroArrival, stream: Int): Variate =
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the interpolated display coordinates for an ordered route node.
     *  @param index      the node position from zero through linkCount
     *  @param linkCount  the number of links along the route
     *  @param sourceXY   the source coordinates
     *  @param sinkXY     the sink coordinates
     */
    private def nodeXY (index: Int, linkCount: Int,
                        sourceXY: (Int, Int), sinkXY: (Int, Int)): (Int, Int) =
        val fraction = index.toDouble / linkCount               // relative route position
        val x = sourceXY._1 + ((sinkXY._1 - sourceXY._1) * fraction).toInt
        val y = sourceXY._2 + ((sinkXY._2 - sourceXY._2) * fraction).toInt
        (x, y)
    end nodeXY

end MesoSpecTwoLinkScalation


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoSpecTwoLinkScalationModel` class executes a translated linear
 *  MesoSpec containing two or more links.
 *  @param spec       the accepted linear MesoSpec
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed to animation speed
 *  @param nStop      the number of vehicles generated for this run
 *  @param stream     the random-number stream used for arrivals
 */
class MesoSpecTwoLinkScalationModel (spec: MesoSpec,
                                     name: String = "MesoSpecTwoLinkScalationModel",
                                     reps: Int = 1, animating: Boolean = false,
                                     aniRatio: Double = 500.0, val nStop: Int = 4,
                                     stream: Int = 0)
      extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Translate and Register Model Components

    val network   = MesoSpecTwoLinkScalation.linear (spec, this, () => Car (), nStop, stream)
    val source    = network.source                            // generated vehicle source
    val junctions = network.junctions                         // ordered junctions
    val links     = network.links                             // ordered mesoscopic links
    val sink      = network.sink                              // terminal sink

    addComponent (source)
    cfor (0, junctions.length) { i => addComponent (junctions(i)) }
    addComponent (sink)                                       // register every node first
    cfor (0, links.length) { i => addComponent (links(i)) }

    //--------------------------------------------------
    // Specify the Vehicle Script

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            cfor (0, links.length) { i =>
                links(i).move ()                              // traverse the next link
                if i < junctions.length then junctions(i).jump ()
            }
            sink.leave ()                                     // terminate after the final link
        end act

    end Car

    simulate ()
    waitFinished ()

end MesoSpecTwoLinkScalationModel


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runMesoSpecTwoLinkScalation` main function tests deterministic
 *  translation of a three-link MesoSpec to show that the translator is not
 *  hard-coded to two links.
 *  > runMain scalation.simulation.process.ir.runMesoSpecTwoLinkScalation
 */
@main def runMesoSpecTwoLinkScalation (): Unit =

    banner ("Multi-link MesoSpec to ScalaTion test")

    val arrival = MicroArrival (MicroArrivalModel.Deterministic,
                                Array (MicroParameter ("x", 0.5))) // deterministic arrivals
    val microNodes = Array (MicroNode ("entry", MicroNodeKind.Source),
                            MicroNode ("j1", MicroNodeKind.Junction),
                            MicroNode ("j2", MicroNodeKind.Junction),
                            MicroNode ("exit", MicroNodeKind.Sink))
    val microSegments = Array (
        MicroSegment ("micro-0", "entry", "j1", 200.0, 1, 10.0),
        MicroSegment ("micro-1", "j1", "j2", 300.0, 1, 10.0),
        MicroSegment ("micro-2", "j2", "exit", 400.0, 1, 10.0))
    val microSource = MicroSource ("entry", "entry", arrival) // source demand
    val microRoute = MicroRoute ("route-0", "entry", "exit",
                                 Array ("micro-0", "micro-1", "micro-2"), 1.0)
    val dynamics = MicroVehicleDynamics (
        CarFollowingModel.IDM,
        ODESolverType.Ballistic,
        Array (MicroParameter ("timeHeadway", 1.5),
               MicroParameter ("minimumGap", 2.0)),
        0.5)
    val micro = MicroIR (
        MicroIR.version,
        MicroNetwork (microNodes, microSegments),
        MicroDemand (Array (microSource), Array (microRoute)),
        dynamics,
        Array (MicroObservable (MicroMeasure.Flow, "micro-2"),
               MicroObservable (MicroMeasure.TravelTime, "route-0")))

    val links = Array (
        MesoLink ("link-0", Array ("micro-0"), "entry", "j1", 200.0, 20.0, 1800.0, 2),
        MesoLink ("link-1", Array ("micro-1"), "j1", "j2", 300.0, 30.0, 1800.0, 2),
        MesoLink ("link-2", Array ("micro-2"), "j2", "exit", 400.0, 40.0, 1800.0, 2))
    val movements = Array (
        MesoMovement ("move-0", "j1", "link-0", "link-1", Array ("route-0")),
        MesoMovement ("move-1", "j2", "link-1", "link-2", Array ("route-0")))
    val source = MesoSource ("entry", "entry", arrival)        // translated source demand
    val route = MesoRoute ("route-0", "entry", "exit",
                           Array ("link-0", "link-1", "link-2"), 1.0)
    val spec = MesoSpec (
        MesoSpec.version,
        micro.version,
        micro.sha256,
        Array (MesoNode ("entry", MicroNodeKind.Source),
               MesoNode ("j1", MicroNodeKind.Junction),
               MesoNode ("j2", MicroNodeKind.Junction),
               MesoNode ("exit", MicroNodeKind.Sink)),
        links,
        movements,
        MesoDemand (Array (source), Array (route)),
        Array (MesoObservable (MicroMeasure.Flow, "link-2"),
               MesoObservable (MicroMeasure.TravelTime, "route-0")),
        MesoSpec.eventOrder)

    spec.validateAgainst (micro)                               // accept this abstraction
    val model = new MesoSpecTwoLinkScalationModel (spec)
    val completed = model.sink.durationStat.num                // completed translated trips

    cfor (0, model.links.length) { i =>
        assert (model.links(i).enteredCount == model.nStop)
        assert (model.links(i).releasedCount == model.nStop)
        assert (model.links(i).occupancy == 0)
        assert (model.links(i).blockedCount == 0)
        assert (model.links(i).persistentStat.max <= model.links(i).storageCapacity)
    }
    assert (completed == model.nStop)

    banner ("Multi-link MesoSpec to ScalaTion test: PASS")
    println (s"mesospec_sha256 = ${spec.sha256}")
    println (s"links=${model.links.length}, junctions=${model.junctions.length}, " +
             s"completed=$completed")
    Model.shutdown ()

end runMesoSpecTwoLinkScalation
