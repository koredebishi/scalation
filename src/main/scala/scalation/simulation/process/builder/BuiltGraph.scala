//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Built mixed-resolution graph and its runtime traversal. */

package scalation
package simulation
package process
package builder

import scalation.simulation.process.meso.MesoVTransport

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A simulation link built for one directed network edge. */
sealed trait BuiltLink:
    def laneCount: Int
    def components: List [Component]
end BuiltLink

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A microscopic link represented by one grouped multi-segment Route. */
final case class MicroLink (route: Route, group: Array [Int]) extends BuiltLink:
    def laneCount: Int = route.pathway.length
    def components: List [Component] = List (route)
end MicroLink

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A mesoscopic link represented by one transport per lane. */
final case class MesoLink (lanes: Array [MesoVTransport]) extends BuiltLink:
    def laneCount: Int = lanes.length
    def components: List [Component] = lanes.toList
end MesoLink

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The graph components built from a set of directed OSM edges. */
class BuiltGraph (val nodeId: Array [Long], val node: Array [Component], val edge: Array [NetEdge],
                  val from: Array [Int], val to: Array [Int], val link: Array [BuiltLink],
                  val cost: Array [Double], val profile: Array [EdgeProfile], stream: Int):

    private val outStart = new Array [Int] (nodeId.length + 1)
    private val outEdge  = new Array [Int] (edge.length)
    cfor (0, edge.length) { e => outStart (from(e) + 1) += 1 }
    cfor (0, nodeId.length) { i => outStart (i + 1) += outStart (i) }
    private val fill = outStart.clone ()
    cfor (0, edge.length) { e => outEdge (fill (from(e))) = e; fill (from(e)) += 1 }

    /** OSM node id -> node index. */
    private val indexOf: Map [Long, Int] = nodeId.indices.map (i => nodeId(i) -> i).toMap

    /** The routing table used by `drive`; set by `routeTo`. */
    var table: RoutingTable = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of a node given its OSM id. */
    def nodeIndex (id: Long): Int =
        require (indexOf.contains (id), s"node $id is not in this graph")
        indexOf (id)
    end nodeIndex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the routing table for the given destination node indices. */
    def routeTo (destinations: Array [Int]): Unit =
        table = new RoutingTable ("RT", nodeId.length, from, to, cost, destinations)
    end routeTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return every component to hand to `Model.addComponent`. */
    def components: List [Component] =
        node.toList ++ link.toList.flatMap (_.components).distinct
    end components

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the cheapest parallel edge from node `a` to node `b`. */
    private def edgeBetween (a: Int, b: Int): Int =
        var best = -1
        cfor (outStart (a), outStart (a + 1)) { k =>
            val e = outEdge (k)
            if to (e) == b && (best < 0 || cost (e) < cost (best)) then best = e
        }
        require (best >= 0, s"no edge from node ${nodeId (a)} to node ${nodeId (b)}")
        best
    end edgeBetween

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Choose a lane at a graph-edge boundary (lane 0 = median side).
     *   - first edge (`previousEdge < 0`): a source that already assigned a valid
     *     lane keeps it; a car with none (e.g. a ramp's subtype) draws one from its id
     *   - on-ramp merge (a `motorway_link` into a non-link edge): the outer lane,
     *     as the classic `Ramp` does; rule 2 then spreads the car inward
     *   - any other boundary: keep the lane; a car in a dropped lane shifts over
     *  @param car           the vehicle
     *  @param previousEdge  the edge just driven (-1 on the first edge)
     *  @param e             the edge about to be driven
     */
    private def handoffLane (car: Vehicle, previousEdge: Int, e: Int): Int =
        val targetLanes = link (e).laneCount
        require (targetLanes > 0, "a transport must have at least one lane")
        if previousEdge < 0 then
            if car.laneID >= 0 && car.laneID < targetLanes then car.laneID
            else Math.floorMod (car.id, targetLanes)
        else if edge (previousEdge).highway == "motorway_link" && edge (e).highway != "motorway_link" then
            targetLanes - 1
        else math.min (math.max (car.laneID, 0), targetLanes - 1)
    end handoffLane

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rule 2 on a meso edge, the same as `Route.rampLaneMove`'s meso branch: a car
     *  that joined from an on-ramp moves one lane inward when the inner lane's entry
     *  speed is higher than its own lane's and the inner lane has room.
     *  (Duplicates Route's meso branch until Route owns the OSM meso lanes, step 5.)
     *  @param car    the vehicle about to enter the edge
     *  @param lanes  the edge's meso lanes
     */
    private def rampLaneMove (car: Vehicle, lanes: Array [MesoVTransport]): Int =
        val lane = car.laneID
        if ! car.fromRamp || lane == 0 then return lane
        val mine  = lanes (lane)
        val inner = lanes (lane - 1)
        if inner.entrySpeed (car.vmax) > mine.entrySpeed (car.vmax) && inner.hasRoom then lane - 1 else lane
    end rampLaneMove

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Drive one vehicle from a start node to a destination sink. */
    def drive (car: Vehicle, startNode: Int, destination: Int): Unit =
        require (table != null, "call routeTo before drive")
        require (node (destination).isInstanceOf [Sink],
                 s"destination node ${nodeId (destination)} is not a Sink")
        var here = startNode
        var previousEdge = -1
        var factor = Double.NaN                                 // driver speed factor = source speed / first edge limit
        while here != destination do
            val next = table.next (here, destination)
            require (next >= 0, s"no route from node ${nodeId (here)} to node ${nodeId (destination)}")
            val e = edgeBetween (here, next)
            car.laneID = handoffLane (car, previousEdge, e)
            if factor.isNaN then                                // first edge
                factor = car.vmax / profile (e).speed
                if edge (e).highway == "motorway_link" then car.fromRamp = true   // starts on an on-ramp
            car.vmax = factor * profile (e).speed               // the driver keeps its factor on every edge

            link (e) match
                case MicroLink (route, group) =>
                    val pathway = route.pathway (car.laneID)
                    val first = group.indexOf (e)
                    val firstSegment = if first >= 0 then first else 0
                    pathway.joinAt (car, pathway.mergeLeaderAt (firstSegment), firstSegment)
                    var i = firstSegment
                    while i < group.length do
                        car.vmax = factor * profile (group (i)).speed
//                      route.driveSegment (car, i)             // MOBIL lane choice + car-follow this segment
                        route.driveSegmentSimple (car, i)       // SIMPLE lane choice + car-follow this segment
                        if i < pathway.junc.length then pathway.junc (i).jump ()
                        i += 1
                    end while
                    here = to (group.last)
                    previousEdge = group.last
                    route.pathway (car.laneID).removeFromAlist (car)   // the car may have changed lanes
                    node (here) match                                   // cross the route's end node
                        case j: Junction => j.jump ()
                        case _           =>
                case MesoLink (lanes) =>
                    car.laneID = rampLaneMove (car, lanes)      // rule 2: ramp cars spread inward
                    lanes (car.laneID).move ()
                    here = next
                    previousEdge = e
                    node (here) match
                        case j: Junction => j.jump ()
                        case _           =>
        end while
        node (destination).asInstanceOf [Sink].leave ()
    end drive
end BuiltGraph
