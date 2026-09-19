//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Mixed-resolution destination routing in the small-city diamond.
 *  Ingress, East, and crossover corridors are mesoscopic; the North branch is
 *  a contiguous microscopic pocket with two VTransport segments.
 */
package scalation
package simulation
package process
package meso
package example_meso

import scala.collection.immutable.Map
import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.Sharp

@main def runDestinationRoutingMesoMicro (): Unit = new DestinationRoutingMesoMicro ()

class DestinationRoutingMesoMicro (name: String = "DestinationRoutingMesoMicro", reps: Int = 1,
                                   animating: Boolean = true, aniRatio: Double = 500.0,
                                   nStop: Int = 30, stream: Int = 0)
    extends Model (name, reps, animating, aniRatio):

    require (nStop > 0 && nStop % 2 == 0, "nStop must be a positive even number")
    require (reps == 1, "this single-run routing experiment assumes reps = 1")
    private val debug = debugf ("DestinationRoutingMesoMicro", true)
    private val interval = 5.0
    private val splitNode = 0; private val upperNode = 1; private val lowerNode = 2
    private val mergeNode = 3; private val northNode = 4; private val eastNode = 5
    private val westNode = 6; private val southNode = 7; private val pocketNode = 8
    private val nodeCount = 9; private val destinations = Array (northNode, eastNode)
    val freeFlowSpeed = 68.0 / 2.23694; val mesoCapacity = 1800.0; val storageCapacity = 10
    Vehicle.setInitialSpeed (freeFlowSpeed)

    private def makeCar (origin: Int): Vehicle =
        val i = math.round (director.clock / interval).toInt
        Car (origin, destinations(i % destinations.length))

    val sourceWest = VSource ("source-west", this, () => makeCar (westNode), 0, nStop, Sharp (interval, stream), (100, 600))
    val sourceSouth = VSource ("source-south", this, () => makeCar (southNode), 0, nStop, Sharp (interval, stream + 1), (600, 1100))
    val split = Junction ("split", xy = (600.0, 600.0), nt = 2, nl = 1)
    val upper = Junction ("upper", xy = (850.0, 350.0), nt = 2, nl = 1)
    val lower = Junction ("lower", xy = (850.0, 850.0), nt = 2, nl = 1)
    val merge = Junction ("merge", xy = (1100.0, 350.0), nt = 2, nl = 1)
    val pocketJunction = Junction ("north-pocket", xy = (1300.0, 150.0), nt = 2, nl = 1)
    val sinkNorth = Sink ("sink-north", (1500, 100)); val sinkEast = Sink ("sink-east", (1350, 600))

    private def meso (n: String, a: Component, b: Component, len: Double): MesoVTransport =
        val x = MesoVTransport (n, a, b, freeFlowSpeed, mesoCapacity, storageCapacity = storageCapacity)
        x.length = len; x

    val fromWest = meso ("from-west", sourceWest, split, 300.0)
    val fromSouth = meso ("from-south", sourceSouth, split, 350.0)
    val splitUpper = meso ("split-upper", split, upper, 300.0)
    val splitLower = meso ("split-lower", split, lower, 300.0)
    val lowerEast = meso ("lower-east", lower, sinkEast, 300.0)
    val upperMerge = meso ("upper-merge", upper, merge, 500.0)
    val lowerMerge = meso ("lower-merge", lower, merge, 500.0)
    val mergeNorth = meso ("merge-north", merge, sinkNorth, 500.0)
    val mergeEast = meso ("merge-east", merge, sinkEast, 500.0)
    val northPocket = Pathway ("north-pocket", Array (pocketJunction), upper, sinkNorth, IDMDynamics,
                               laneShift = VectorD (0.0, 0.0))
    northPocket.seg(0).length = 300.0; northPocket.seg(1).length = 300.0

    private val roads: Map [(Int, Int), (Component, Double)] = Map (
        (westNode, splitNode) -> (fromWest, fromWest.freeFlowTime), (southNode, splitNode) -> (fromSouth, fromSouth.freeFlowTime),
        (splitNode, upperNode) -> (splitUpper, splitUpper.freeFlowTime), (splitNode, lowerNode) -> (splitLower, splitLower.freeFlowTime),
        (upperNode, northNode) -> (northPocket, northPocket.cumLen.last / freeFlowSpeed),
        (lowerNode, eastNode) -> (lowerEast, lowerEast.freeFlowTime), (upperNode, mergeNode) -> (upperMerge, upperMerge.freeFlowTime),
        (lowerNode, mergeNode) -> (lowerMerge, lowerMerge.freeFlowTime), (mergeNode, northNode) -> (mergeNorth, mergeNorth.freeFlowTime),
        (mergeNode, eastNode) -> (mergeEast, mergeEast.freeFlowTime))
    private val cost = new MatrixD (nodeCount, nodeCount)
    for ((u, v), (_, edgeCost)) <- roads do cost(u, v) = edgeCost
    private val routingTable = new RoutingTable ("DestinationRoutingMesoMicro", cost, destinations)
    split.setRouting (routingTable, splitNode)
    upper.setRouting (routingTable, upperNode)
    lower.setRouting (routingTable, lowerNode)
    merge.setRouting (routingTable, mergeNode)

    private val components: Seq [Component] = Seq (sourceWest, sourceSouth, split, upper, lower, merge,
        pocketJunction, sinkNorth, sinkEast, fromWest, fromSouth, splitUpper, splitLower, lowerEast,
        upperMerge, lowerMerge, mergeNorth, mergeEast, northPocket)
    addComponent (components*)

    case class Car (origin: Int, destination: Int) extends Vehicle ("c", this):
        override def act (): Unit =
            val ingress = if origin == westNode then fromWest else fromSouth
            ingress.move (); split.jump ()
            var at = splitNode; var here: Component = split
            while at != destination do
                val next = here.asInstanceOf [Junction].nextNode (destination)
                val selected = roads((at, next))._1
                selected match
                case m: MesoVTransport =>
                    require (m.from eq here, s"${m.name} does not leave node $at")
                    m.move ()
                    m.to match
                    case j: Junction => j.jump (); here = j; at = next
                    case s: Sink => s.leave (); at = destination
                case p: Pathway =>
                    require (p.from eq here, s"${p.name} does not leave node $at")
                    p.joinAt (this, p.mergeLeaderAt (0), 0)
                    for i <- p.seg.indices do
                        p.seg(i).move ()
                        if i < p.junc.length then p.junc(i).jump ()
                    end for
                    p.removeFromAlist (this); destinationSink(destination).leave (); at = destination
                case other => throw new IllegalStateException (s"unsupported link $other")
                debug ("ROUTE", s"t=${director.clock}, actor=$displayLabel, id=$id, origin=$origin, destination=$destination, selected=${selected.name}")
            end while
        private def destinationSink (d: Int): Sink = if d == northNode then sinkNorth else sinkEast
    end Car

    banner ("G2.3B mixed-resolution diamond routing")
    println ("two meso ingress links -> split -> meso upper/lower arms -> micro North pocket or meso East/crossover")
    println (s"vehicles per source=$nStop, total vehicles=${2 * nStop}")
    simulate (); waitFinished ()
    val total = 2 * nStop; val each = total / 2
    require (sinkNorth.durationStat.num == each && sinkEast.durationStat.num == each, "sink conservation failed")
    require (split.durationStat.num == total, s"split crossings=${split.durationStat.num}")
    require (northPocket.vList.isEmpty, "North microscopic pocket did not drain")
    for m <- Array (fromWest, fromSouth, splitUpper, splitLower, lowerEast, upperMerge, lowerMerge, mergeNorth, mergeEast) do
        require (m.occupancy == 0 && m.blockedCount == 0, s"${m.name} did not drain")
    banner ("Mixed-resolution diamond routing: PASS")
    println (s"completed=$total, micro North=$each, meso East=$each, agenda activations=$agendaActivations")
    Model.shutdown ()
end DestinationRoutingMesoMicro
