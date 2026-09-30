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

    /** The routing table used by `drive`; set by `routeTo`. */
    var table: RoutingTable = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of a node given its OSM id. */
    def nodeIndex (id: Long): Int =
        val i = java.util.Arrays.binarySearch (nodeId, id)
        require (i >= 0, s"node $id is not in this graph")
        i
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
    /** Choose a lane at a graph-edge boundary.  On the first edge (`previousLanes
     *  <= 0`), a source that already assigned a valid lane keeps it; a car with
     *  no such lane (or a convention value out of this edge's range, e.g. a
     *  ramp's `numLanes - 1`) draws one from its id.  On a later edge, a same
     *  width keeps the lane and a width change redistributes deterministically.
     */
    private def handoffLane (car: Vehicle, previousLanes: Int, targetLanes: Int): Int =
        require (targetLanes > 0, "a transport must have at least one lane")
        val validIncoming = car.laneID >= 0 && car.laneID < targetLanes
        if previousLanes <= 0 then
            if validIncoming then car.laneID else Math.floorMod (car.id, targetLanes)
        else if car.laneID < 0 then Math.floorMod (car.id, targetLanes)
        else if previousLanes == targetLanes then math.min (car.laneID, targetLanes - 1)
        else Math.floorMod (car.id + car.laneID, targetLanes)
    end handoffLane

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Drive one vehicle from a start node to a destination sink. */
    def drive (car: Vehicle, startNode: Int, destination: Int): Unit =
        require (table != null, "call routeTo before drive")
        require (node (destination).isInstanceOf [Sink],
                 s"destination node ${nodeId (destination)} is not a Sink")
        var here = startNode
        var previousLanes = 0
        while here != destination do
            val next = table.next (here, destination)
            require (next >= 0, s"no route from node ${nodeId (here)} to node ${nodeId (destination)}")
            val e = edgeBetween (here, next)
            val lanes = link (e).laneCount
            car.laneID = handoffLane (car, previousLanes, lanes)
            previousLanes = lanes
            car.vmax = profile (e).speed

            link (e) match
                case MicroLink (route, group) =>
                    val pathway = route.pathway (car.laneID)
                    val first = group.indexOf (e)
                    val firstSegment = if first >= 0 then first else 0
                    pathway.joinAt (car, pathway.mergeLeaderAt (firstSegment), firstSegment)
                    var i = firstSegment
                    while i < group.length do
                        car.vmax = profile (group (i)).speed
                        pathway.seg (i).move ()
                        if i < pathway.junc.length then pathway.junc (i).jump ()
                        i += 1
                    end while
                    here = to (group.last)
                    pathway.removeFromAlist (car)
                case MesoLink (lanes) =>
                    lanes (car.laneID).move ()
                    here = next
                    node (here) match
                        case j: Junction => j.jump ()
                        case _           =>
        end while
        node (destination).asInstanceOf [Sink].leave ()
    end drive
end BuiltGraph
