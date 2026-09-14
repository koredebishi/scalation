//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishi
 *  @version 2.0
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Destination-indexed next-hop table for network routing
 */

package scalation
package simulation
package process

import scalation.database.graph_pm.ShortestPath
import scalation.mathstat.MatrixD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RoutingTable` class precomputes destination-specific next hops.
 *
 *  Edge costs are supplied as a `MatrixD`, where entry `(i, j)` is the cost
 *  from node `i` to node `j`.  Zero means "no edge", so every real edge must
 *  be positive.  The table reverses those edges and runs the native
 *  `ShortestPath` algorithm once per destination.  A junction performs only a
 *  constant-time lookup during the simulation.
 *
 *  @param name         the name of this routing table
 *  @param cost         the directed edge-cost matrix (square, zero = no edge)
 *  @param destinations destination node identifiers to precompute
 */
class RoutingTable (name: String, cost: MatrixD, destinations: Array [Int]):

    private val debug = debugf ("RoutingTable", true)                  // diagnostic tracing
    private val flaw  = flawf ("RoutingTable")                         // construction warnings
    private val nodeCount = cost.dim                                    // number of graph nodes

    if nodeCount <= 0 then flaw ("init", "nodeCount must be positive")
    if destinations.isEmpty then flaw ("init", "destinations must not be empty")

    /** `nextHop(node, destination)` stores the next node plus one; zero means
     *  that no route was precomputed for the pair.
     */
    private [process] val nextHop = new MatrixD (nodeCount, nodeCount)

    private val edgeCount =
        var c = 0
        cfor (0, nodeCount) { i => cfor (0, nodeCount) { j => if cost(i, j) > 0.0 then c += 1 } }
        c

    // ShortestPath treats zero as "no edge" and mutates its source row, so run
    // it on a fresh transposed copy for each destination.
    private val reverseCost = cost.transpose

    cfor (0, destinations.length) { i =>
        val destination = destinations(i)
        if destination < 0 || destination >= nodeCount then
            flaw ("init", s"destination $destination is outside node range")
        else
            val (distance, predecessor) = new ShortestPath (reverseCost.copy, destination).spath ()
            cfor (0, nodeCount) { node =>
                if distance(node) < Double.MaxValue / 2 then
                    nextHop (node, destination) =
                        (if node == destination then destination else predecessor(node)) + 1.0
            }
    }

    debug ("init", s"name = $name, nodes = $nodeCount, edges = $edgeCount, " +
                    s"destinations = ${destinations.mkString ("[", ",", "]")}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next node on the shortest path to a destination, or -1 when
     *  the pair is out of range or no route was precomputed.  The destination
     *  itself maps to itself.
     *  @param atNode      the current node
     *  @param destination the desired destination node
     */
    def next (atNode: Int, destination: Int): Int =
        if atNode < 0 || atNode >= nodeCount || destination < 0 || destination >= nodeCount then -1
        else if nextHop (atNode, destination) > 0.0 then nextHop (atNode, destination).toInt - 1 else -1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the encoded next-hop table for diagnostics and validation.
     *  Entry `(node, destination)` stores the next node plus one; zero means
     *  that no route was precomputed.
     */
    def table: MatrixD = nextHop

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next hop using function-call syntax.
     */
    def apply (atNode: Int, destination: Int): Int = next (atNode, destination)

end RoutingTable

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runRoutingTableTest` main function tests the `RoutingTable` class.
 *  The graph includes competing paths, two destinations, and unreachable nodes.
 *  > runMain scalation.simulation.process.runRoutingTableTest
 */
@main def runRoutingTableTest (): Unit =
    banner ("RoutingTable test")

    val cost = MatrixD ((7, 7),
        0.0, 1.0, 2.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 1.0, 0.0, 10.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    val destinations = Array (4, 5)
    val routingTable = new RoutingTable ("TestRouting", cost, destinations)

    // Destination 4: 0 -> 1 -> 3 -> 4 is the shortest path.
    assert (routingTable.next (0, 4) == 1)
    assert (routingTable.next (1, 4) == 3)
    assert (routingTable.next (3, 4) == 4)
    assert (routingTable.next (4, 4) == 4)

    // Destination 5: 0 -> 1 -> 3 -> 5 is shorter than 0 -> 2 -> 5.
    assert (routingTable.next (0, 5) == 1)
    assert (routingTable.next (1, 5) == 3)
    assert (routingTable.next (3, 5) == 5)
    assert (routingTable.next (5, 5) == 5)

    // Node 6 is isolated and destination 6 was not precomputed.
    assert (routingTable.next (6, 4) == -1)
    assert (routingTable.next (0, 6) == -1)

    println (s"next hop 0 -> 4: ${routingTable.next (0, 4)}")
    println (s"next hop 0 -> 5: ${routingTable.next (0, 5)}")
    println (s"next hop 6 -> 4: ${routingTable.next (6, 4)}")
    banner ("RoutingTable test: PASS")

end runRoutingTableTest
