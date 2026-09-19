//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    EatonMiniTest: the full cleaned Eaton network, built by `NetworkBuilder`
 *
 *  Every node and directed edge of `data/network/eaton/edges.geojson` becomes a
 *  simulation component: motorway and motorway_link edges microscopic, every other
 *  class mesoscopic.  There is no background layer: what is drawn is the network
 *  that is simulated.  Vehicles are routed over the whole graph from one source
 *  node to one sink node (a real dead end of the network), by the same `Vehicle`
 *  across both resolutions under one agenda.
 */

package scalation
package simulation
package process
package model

import scalation.animation.OsmRoadNetwork
import scalation.random.Sharp
import scalation.simulation.process.{IntegratorType, IDMDynamics}
import scalation.simulation.process.builder.NetworkBuilder

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run the EatonMiniTest simulation.
 *  > runMain scalation.simulation.process.model.runEatonMiniTest
 */
@main def runEatonMiniTest (): Unit = new EatonMiniTest ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EatonMiniTest` class sends vehicles across the full Eaton network.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed versus animation speed
 *  @param nStop      the number of arrivals before stopping
 *  @param stream     the base random-number stream
 */
class EatonMiniTest (name: String = "EatonMiniTest", reps: Int = 1, animating: Boolean = true,
                     aniRatio: Double = 200.0, nStop: Int = 30, stream: Int = 0)
      extends Model (name, reps, animating, aniRatio):

    IDMDynamics.integratorType = IntegratorType.Ballistic

    // the one origin and destination of this test (OSM node ids); change them to route elsewhere
    private val sourceId = 1687098935L       // start of the I-210 WB exit chain used in the first proof
    private val sinkId   = 1262444667L       // a dead end of the network (no outgoing edge), reached over a mesoscopic edge

    // the whole network, straight from the GeoJSON
    private val edges = NetworkBuilder.read ("data/network/eaton/edges.geojson")

    // the frame is the network itself: its own nodes plus a 5% margin (no background layer)
    private val lats   = edges.flatMap (e => Array (e.uLat, e.vLat))
    private val lons   = edges.flatMap (e => Array (e.uLon, e.vLon))
    private val padLat = 0.05 * (lats.max - lats.min)
    private val padLon = 0.05 * (lons.max - lons.min)
    private val anchors = Array ((lats.min - padLat, lons.min - padLon), (lats.max + padLat, lons.max + padLon))
    private val project = OsmRoadNetwork.buildProjector (anchors, (30000.0, 18000.0), "EatonMiniTest")   // 6 x the Eaton frame: node discs, lane gaps, fonts and lines all shrink 6 x against the roads

    private val graph = NetworkBuilder.build (edges, Array (sinkId), project, IDMDynamics, nt = 2, stream = stream)
    private val startNode = graph.nodeIndex (sourceId)      // not `start`: a Vehicle is a coroutine and has its own `start`
    private val destNode  = graph.nodeIndex (sinkId)
    graph.routeTo (Array (destNode))

    private val sourceEdge = edges.find (_.u == sourceId).get
    private val (sx, sy)   = project (sourceEdge.uLat, sourceEdge.uLon)
    val entry = VSource ("SO", this, () => Car (), 0, nStop, Sharp (5.0, stream), (sx.toInt, sy.toInt))

    addComponent ((entry :: graph.components)*)

    case class Car () extends Vehicle ("c", this):
        override def act (): Unit =
            graph.drive (this, startNode, destNode)
        end act
    end Car

    banner ("EatonMiniTest: the full Eaton network, micro and meso")
    println (s"nodes=${graph.nodeId.length}, edges=${edges.length} " +
             s"(micro=${edges.count (_.group.micro)}, meso=${edges.count (! _.group.micro)})")
    println (s"components registered=${graph.components.length + 1}")
    println (s"source node=$sourceId, sink node=$sinkId, vehicles=$nStop")

    simulate ()
    waitFinished ()

    private val exit = graph.node (destNode).asInstanceOf [Sink]
    require (exit.durationStat.num == nStop, s"completed=${exit.durationStat.num}, expected=$nStop")
    banner ("EatonMiniTest: PASS")
    println (s"completed=${exit.durationStat.num}, mean time in system=${exit.durationStat.mean}")
    println (s"agenda activations=$agendaActivations")
    Model.shutdown ()

end EatonMiniTest
