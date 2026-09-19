//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Destination-aware routing over a small diamond network.
 *  Uses Scalation's native ShortestPath to precompute one next-hop tree per sink.
 *  `short-direct` selects the short upper/lower arms; `cheap-detour` makes the
 *  North route through `merge` cheaper.
 */
package scalation
package simulation
package process
package meso
package example_meso

import scala.collection.immutable.Map
import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.Sharp

@main def runDestinationRoutingMixed (): Unit = new DestinationRoutingMixed ()
@main def runDestinationRoutingMixedCheapDetour (): Unit =
    new DestinationRoutingMixed (costProfile = "cheap-detour")

class DestinationRoutingMixed (name: String = "DestinationRoutingMixed", reps: Int = 1,
                               animating: Boolean = true, aniRatio: Double = 500.0,
                               nStop: Int = 30, stream: Int = 0,
                               costProfile: String = "short-direct")
    extends Model (name, reps, animating, aniRatio):

    require (nStop > 0 && nStop % 2 == 0,
             "destination-routing experiment requires a positive even number of vehicles per source")
    require (reps == 1, "this single-run routing experiment currently assumes reps = 1")
    require (costProfile == "short-direct" || costProfile == "cheap-detour",
             s"unknown routing cost profile: $costProfile")

    private val debug = debugf ("DestinationRoutingMixed", true)
    private val arrivalInterval = 5.0
    // split, upper, lower, merge, north sink, east sink, west source, south source
    private val splitNode = 0; private val upperNode = 1; private val lowerNode = 2
    private val mergeNode = 3; private val sinkNorthNode = 4; private val sinkEastNode = 5
    private val sourceWestNode = 6; private val sourceSouthNode = 7; private val nodeCount = 8
    private val destinations = Array (sinkNorthNode, sinkEastNode)

    val freeFlowSpeed = 68.0 / 2.23694
    val mesoCapacity = 1800.0
    val storageCapacity = 10
    Vehicle.setInitialSpeed (freeFlowSpeed)

    // Both sources assign the same destination in each clock round. Clock-based
    // alternation is deterministic and resets with every replication.
    private def makeCar (origin: Int): Vehicle =
        val i = math.round (director.clock / arrivalInterval).toInt
        Car (origin, destinations(i % destinations.length))
    end makeCar

    val sourceWest = VSource ("source-west", this, () => makeCar (sourceWestNode), 0, nStop,
                              Sharp (arrivalInterval, stream), (100, 600))
    val sourceSouth = VSource ("source-south", this, () => makeCar (sourceSouthNode), 0, nStop,
                               Sharp (arrivalInterval, stream + 1), (600, 1100))
    val split = Junction ("split", xy = (600.0, 600.0), nt = 2, nl = 1)
    val upper = Junction ("upper", xy = (850.0, 350.0), nt = 2, nl = 1)
    val lower = Junction ("lower", xy = (850.0, 850.0), nt = 2, nl = 1)
    val merge = Junction ("merge", xy = (1100.0, 350.0), nt = 2, nl = 1)
    val sinkNorth = Sink ("sink-north", (1100, 100)); val sinkEast = Sink ("sink-east", (1350, 600))

    private def link (name: String, from: Component, to: Component, length: Double,
                      shift1: VectorD = VectorD (0.0, 0.0), shift2: VectorD = VectorD (0.0, 0.0)): MesoVTransport =
        val x = MesoVTransport (name, from, to, freeFlowSpeed, mesoCapacity,
                                shift1 = shift1, shift2 = shift2, storageCapacity = storageCapacity)
        x.length = length; x
    end link

    val fromWest = link ("from-west", sourceWest, split, 300.0, shift2 = VectorD (-20.0, 0.0))
    val fromSouth = link ("from-south", sourceSouth, split, 350.0, shift2 = VectorD (0.0, 20.0))
    val splitUpper = link ("split-upper", split, upper, 300.0)
    val upperNorth = link ("upper-north", upper, sinkNorth, 300.0)
    val splitLower = link ("split-lower", split, lower, 300.0)
    val lowerEast = link ("lower-east", lower, sinkEast, 300.0)
    val upperMerge = link ("upper-merge", upper, merge, 500.0)
    val lowerMerge = link ("lower-merge", lower, merge, 500.0)
    val mergeNorth = link ("merge-north", merge, sinkNorth, 500.0)
    val mergeEast = link ("merge-east", merge, sinkEast, 500.0)
    if costProfile == "cheap-detour" then upperNorth.length = 1200.0
    private val allLinks = Array (fromWest, fromSouth, splitUpper, upperNorth, splitLower, lowerEast,
                                  upperMerge, lowerMerge, mergeNorth, mergeEast)
    private val sinks = Array (sinkNorth, sinkEast)
    private val destinationSink: Map [Int, Sink] = Map (sinkNorthNode -> sinkNorth, sinkEastNode -> sinkEast)

    // Every edge in this experiment is mesoscopic, so retain the concrete
    // type here to use its native free-flow travel-time accessor.
    private val roads: Map [(Int, Int), MesoVTransport] = Map (
        (sourceWestNode, splitNode) -> fromWest, (sourceSouthNode, splitNode) -> fromSouth,
        (splitNode, upperNode) -> splitUpper, (upperNode, sinkNorthNode) -> upperNorth,
        (splitNode, lowerNode) -> splitLower, (lowerNode, sinkEastNode) -> lowerEast,
        (upperNode, mergeNode) -> upperMerge, (lowerNode, mergeNode) -> lowerMerge,
        (mergeNode, sinkNorthNode) -> mergeNorth, (mergeNode, sinkEastNode) -> mergeEast)

    // Build the native cost matrix once; RoutingTable runs ShortestPath per destination.
    private val cost = new MatrixD (nodeCount, nodeCount)
    for ((u, v), transport) <- roads do cost(u, v) = transport.freeFlowTime
    private val routingTable = new RoutingTable ("DestinationRouting", cost, destinations)
    split.setRouting (routingTable, splitNode)
    upper.setRouting (routingTable, upperNode)
    lower.setRouting (routingTable, lowerNode)
    merge.setRouting (routingTable, mergeNode)
    split.setOutgoing (upperNode -> splitUpper, lowerNode -> splitLower)
    upper.setOutgoing (sinkNorthNode -> upperNorth, mergeNode -> upperMerge)
    lower.setOutgoing (sinkEastNode -> lowerEast, mergeNode -> lowerMerge)
    merge.setOutgoing (sinkNorthNode -> mergeNorth, sinkEastNode -> mergeEast)

    private val components: Seq [Component] =
        Seq (sourceWest, sourceSouth, split, upper, lower, merge, sinkNorth, sinkEast) ++ allLinks.toSeq
    addComponent (components*)

    case class Car (origin: Int, destination: Int) extends Vehicle ("c", this):
        override def act (): Unit =
            // Origin selects the fixed ingress; Dijkstra begins at split.
            val ingress = if origin == sourceWestNode then fromWest else fromSouth
            val ingressSource = if origin == sourceWestNode then sourceWest else sourceSouth
            require (ingress.from eq ingressSource, s"invalid ingress for origin $origin")
            ingress.move ()
            var movement: Component = split.jumpTo (destination)
            while movement != null do
                movement match
                case transport: MesoVTransport =>
                    transport.move ()
                    transport.to match
                    case j: Junction =>
                        movement = j.jumpTo (destination)
                        debug ("ROUTE", s"t=${director.clock}, actor=$displayLabel, id=$id, origin=$origin, destination=$destination, ${transport.name} -> ${j.name}")
                    case sink: Sink =>
                        require (destinationSink(destination) eq sink, s"unexpected sink ${sink.name}")
                        sink.leave (); movement = null
                    case other => throw new IllegalStateException (s"route ended at non-junction component $other")
                case other => throw new IllegalStateException (s"unsupported movement $other")
            end while
        end act
    end Car

    banner (s"G2.3A destination-aware Dijkstra branching [$costProfile]")
    println ("two Sources -> split -> diamond alternatives -> two Sinks")
    println (s"vehicles per source=$nStop, total vehicles=${2 * nStop}, destinations=${destinations.mkString ("[", ",", "]")}")
    println (s"split next hops: North=${split.nextNode (sinkNorthNode)}, East=${split.nextNode (sinkEastNode)}")
    require (split.nextNode (sinkNorthNode) == upperNode && split.nextNode (sinkEastNode) == lowerNode,
             "Dijkstra did not select the expected destination-specific first arms")
    val northAfterUpper = upper.nextNode (sinkNorthNode)
    require (northAfterUpper == (if costProfile == "short-direct" then sinkNorthNode else mergeNode),
             s"North route after Upper=$northAfterUpper under profile $costProfile")

    simulate (); waitFinished ()
    val totalVehicles = 2 * nStop; val expectedPerSink = totalVehicles / destinations.length
    val completed = sinks.map (_.durationStat.num).sum
    require (completed == totalVehicles, s"completed=$completed, expected=$totalVehicles")
    for sink <- sinks do require (sink.durationStat.num == expectedPerSink, s"${sink.name} completions=${sink.durationStat.num}, expected=$expectedPerSink")
    require (split.durationStat.num == totalVehicles, s"split crossings=${split.durationStat.num}")
    require (upper.durationStat.num == expectedPerSink && lower.durationStat.num == expectedPerSink, s"direct-arm crossings upper/lower=${upper.durationStat.num}/${lower.durationStat.num}")
    val expectedMerge = if costProfile == "short-direct" then 0 else expectedPerSink
    require (merge.durationStat.num == expectedMerge,
             s"merge crossings=${merge.durationStat.num}, expected=$expectedMerge")
    require (fromWest.enteredCount == nStop && fromSouth.enteredCount == nStop,
             s"ingress entries west/south=${fromWest.enteredCount}/${fromSouth.enteredCount}")
    for link <- allLinks do require (link.occupancy == 0 && link.blockedCount == 0, s"${link.name} not drained")
    banner (s"Destination-aware Dijkstra branching [$costProfile]: PASS")
    println (s"completed=$completed, sink completions=${sinks.map (_.durationStat.num).mkString ("[", ",", "]")}")
    println (s"split/upper/lower/merge crossings=${split.durationStat.num}/${upper.durationStat.num}/${lower.durationStat.num}/${merge.durationStat.num}")
    println (s"agenda activations=$agendaActivations")
    Model.shutdown ()
end DestinationRoutingMixed
