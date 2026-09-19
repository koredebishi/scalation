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

import scala.collection.mutable.{ArrayBuffer, PriorityQueue}

import scalation.database.graph_pm.ShortestPath
import scalation.mathstat.MatrixD
import scalation.random.Uniform

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RoutingTable` class precomputes destination-specific next hops.
 *
 *  Two ways to build one:
 *   - dense:  edge costs as a `MatrixD`, where entry `(i, j)` is the cost from
 *     node `i` to node `j`.  Zero means "no edge", so every real edge must be
 *     positive.  The table reverses those edges and runs the native
 *     `ShortestPath` algorithm once per destination.  Needs N x N memory, so it
 *     suits small networks.
 *   - sparse: edges as parallel arrays `from`, `to`, `edgeCost`.  Dijkstra runs
 *     over per-node edge lists, so memory is proportional to the number of
 *     edges, not N x N.  Use this for a whole street network.
 *
 *  Either way the result is stored as N x (number of destinations), and a
 *  junction performs only a constant-time lookup during the simulation.
 *
 *  @param name         the name of this routing table
 *  @param nodeCount    the number of graph nodes
 *  @param destinations destination node identifiers to precompute
 *  @param hop          `hop (node, d)` = next node plus one toward `destinations (d)`;
 *                      zero means no route was precomputed for the pair
 */
class RoutingTable private (name: String, nodeCount: Int, destinations: Array [Int],
                            hop: MatrixD):

    /** `column (destination)` is the column of `hop` holding that destination,
     *  or -1 when the node is not a precomputed destination.
     */
    private val column = Array.fill (math.max (nodeCount, 0)) (-1)
    cfor (0, destinations.length) { d =>
        val destination = destinations (d)
        if destination >= 0 && destination < nodeCount then column (destination) = d
    }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build from a dense directed edge-cost matrix (square, zero = no edge).
     *  @param name         the name of this routing table
     *  @param cost         the directed edge-cost matrix
     *  @param destinations destination node identifiers to precompute
     */
    def this (name: String, cost: MatrixD, destinations: Array [Int]) =
        this (name, cost.dim, destinations, RoutingTable.denseHops (name, cost, destinations))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build from edge arrays: edge `e` runs `from (e)` -> `to (e)` at cost `edgeCost (e)`.
     *  @param name         the name of this routing table
     *  @param nodeCount    the number of graph nodes (node ids are 0 .. nodeCount-1)
     *  @param from         the tail node of each edge
     *  @param to           the head node of each edge
     *  @param edgeCost     the positive cost of each edge
     *  @param destinations destination node identifiers to precompute
     */
    def this (name: String, nodeCount: Int, from: Array [Int], to: Array [Int],
              edgeCost: Array [Double], destinations: Array [Int]) =
        this (name, nodeCount, destinations,
              RoutingTable.sparseHops (name, nodeCount, from, to, edgeCost, destinations))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next node on the shortest path to a destination, or -1 when
     *  the pair is out of range or no route was precomputed.  The destination
     *  itself maps to itself.
     *  @param atNode      the current node
     *  @param destination the desired destination node
     */
    def next (atNode: Int, destination: Int): Int =
        if atNode < 0 || atNode >= nodeCount || destination < 0 || destination >= nodeCount then -1
        else
            val d = column (destination)
            if d >= 0 && hop (atNode, d) > 0.0 then hop (atNode, d).toInt - 1 else -1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the encoded next-hop table for diagnostics and validation, as a
     *  fresh N x N matrix (allocated on each call, so not for use in a run).
     *  Entry `(node, destination)` stores the next node plus one; zero means
     *  that no route was precomputed.
     */
    def table: MatrixD =
        val t = new MatrixD (nodeCount, nodeCount)
        cfor (0, destinations.length) { d =>
            val destination = destinations (d)
            if destination >= 0 && destination < nodeCount then
                cfor (0, nodeCount) { node => t (node, destination) = hop (node, d) }
        }
        t

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next hop using function-call syntax.
     */
    def apply (atNode: Int, destination: Int): Int = next (atNode, destination)

end RoutingTable


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RoutingTable` companion object holds the two next-hop builders.
 */
object RoutingTable:

    private val debug = debugf ("RoutingTable", true)                  // diagnostic tracing
    private val flaw  = flawf ("RoutingTable")                         // construction warnings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** A vertex and its distance from the destination, ordered so the priority
     *  queue yields the smallest distance first.
     *  @param id  the vertex id
     *  @param dd  the vertex's distance to the destination
     */
    private case class Item (id: Int, dd: Double) extends Ordered [Item]:
        def compare (v: Item) = v.dd compare dd
    end Item

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute next hops from a dense cost matrix with the native `ShortestPath`.
     *  Returns `hop (node, d)` = next node plus one toward `destinations (d)`.
     *  @param name         the name of the routing table
     *  @param cost         the directed edge-cost matrix (square, zero = no edge)
     *  @param destinations destination node identifiers to precompute
     */
    private def denseHops (name: String, cost: MatrixD, destinations: Array [Int]): MatrixD =
        val nodeCount = cost.dim                                        // number of graph nodes

        if nodeCount <= 0 then flaw ("init", "nodeCount must be positive")
        if destinations.isEmpty then flaw ("init", "destinations must not be empty")

        val hop = new MatrixD (nodeCount, math.max (1, destinations.length))

        val edgeCount =
            var c = 0
            cfor (0, nodeCount) { i => cfor (0, nodeCount) { j => if cost(i, j) > 0.0 then c += 1 } }
            c

        // ShortestPath treats zero as "no edge" and mutates its source row, so run
        // it on a fresh transposed copy for each destination.
        val reverseCost = cost.transpose

        cfor (0, destinations.length) { i =>
            val destination = destinations(i)
            if destination < 0 || destination >= nodeCount then
                flaw ("init", s"destination $destination is outside node range")
            else
                val (distance, predecessor) = new ShortestPath (reverseCost.copy, destination).spath ()
                cfor (0, nodeCount) { node =>
                    if distance(node) < Double.MaxValue / 2 then
                        hop (node, i) =
                            (if node == destination then destination else predecessor(node)) + 1.0
                }
        }

        debug ("init", s"name = $name, nodes = $nodeCount, edges = $edgeCount, " +
                        s"destinations = ${destinations.mkString ("[", ",", "]")}")
        hop
    end denseHops

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute next hops from edge arrays with Dijkstra over per-node edge lists.
     *  Edges are stored reversed (grouped by head node) so one search from each
     *  destination gives every node's next hop toward it.
     *  Returns `hop (node, d)` = next node plus one toward `destinations (d)`.
     *  @param name         the name of the routing table
     *  @param nodeCount    the number of graph nodes
     *  @param from         the tail node of each edge
     *  @param to           the head node of each edge
     *  @param edgeCost     the positive cost of each edge
     *  @param destinations destination node identifiers to precompute
     */
    private def sparseHops (name: String, nodeCount: Int, from: Array [Int], to: Array [Int],
                            edgeCost: Array [Double], destinations: Array [Int]): MatrixD =
        require (nodeCount > 0, "nodeCount must be positive")
        require (from.length == to.length && from.length == edgeCost.length,
                 "from, to and edgeCost must have one entry per edge")
        if destinations.isEmpty then flaw ("init", "destinations must not be empty")

        val nEdges = from.length
        cfor (0, nEdges) { e =>
            require (from(e) >= 0 && from(e) < nodeCount && to(e) >= 0 && to(e) < nodeCount,
                     s"edge $e has an endpoint outside 0 until $nodeCount")
            require (edgeCost(e) > 0.0, s"edge $e must have a positive cost")
        }

        // reversed edge lists in flat arrays: the edges entering node v are
        // rev(start(v)) until rev(start(v+1)), each stored as its tail node u
        val start = new Array [Int] (nodeCount + 1)
        cfor (0, nEdges) { e => start(to(e) + 1) += 1 }
        cfor (0, nodeCount) { v => start(v + 1) += start(v) }
        val fill = start.clone ()
        val rev  = new Array [Int] (nEdges)                             // tail node u of the reversed edge v -> u
        val w    = new Array [Double] (nEdges)                          // its cost
        cfor (0, nEdges) { e =>
            val k = fill(to(e))
            rev(k) = from(e); w(k) = edgeCost(e)
            fill(to(e)) = k + 1
        }

        val hop  = new MatrixD (nodeCount, math.max (1, destinations.length))
        val dist = new Array [Double] (nodeCount)                       // distance to the current destination
        val pred = new Array [Int] (nodeCount)                          // next node toward it

        cfor (0, destinations.length) { i =>
            val destination = destinations(i)
            if destination < 0 || destination >= nodeCount then
                flaw ("init", s"destination $destination is outside node range")
            else
                java.util.Arrays.fill (dist, Double.MaxValue)
                java.util.Arrays.fill (pred, -1)
                dist(destination) = 0.0
                val q = PriorityQueue.empty [Item]
                q += Item (destination, 0.0)
                while q.nonEmpty do
                    val v = q.dequeue ()
                    if v.dd <= dist(v.id) then                          // skip stale queue entries
                        cfor (start(v.id), start(v.id + 1)) { k =>
                            val u   = rev(k)
                            val alt = v.dd + w(k)
                            if alt < dist(u) then
                                dist(u) = alt; pred(u) = v.id; q += Item (u, alt)
                        }
                end while
                cfor (0, nodeCount) { node =>
                    if dist(node) < Double.MaxValue / 2 then
                        hop (node, i) = (if node == destination then destination else pred(node)) + 1.0
                }
        }

        debug ("init", s"name = $name, nodes = $nodeCount, edges = $nEdges, " +
                        s"destinations = ${destinations.mkString ("[", ",", "]")}")
        hop
    end sparseHops

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


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runRoutingTableSparseTest` main function checks that the edge-array
 *  constructor gives the same next hop as the dense constructor for every
 *  (node, destination) pair of the `runRoutingTableTest` graph.
 *  > runMain scalation.simulation.process.runRoutingTableSparseTest
 */
@main def runRoutingTableSparseTest (): Unit =
    banner ("RoutingTable sparse test")

    val cost = MatrixD ((7, 7),
        0.0, 1.0, 2.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 1.0, 0.0, 10.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    // the same graph as edge arrays: 0->1, 0->2, 1->3, 2->3, 2->5, 3->4, 3->5
    val from     = Array (0, 0, 1, 2, 2, 3, 3)
    val to       = Array (1, 2, 3, 3, 5, 4, 5)
    val edgeCost = Array (1.0, 2.0, 1.0, 1.0, 10.0, 1.0, 2.0)

    val destinations = Array (4, 5)
    val dense  = new RoutingTable ("DenseRouting", cost, destinations)
    val sparse = new RoutingTable ("SparseRouting", 7, from, to, edgeCost, destinations)

    for node <- 0 until 7; destination <- 0 until 7 do
        assert (sparse.next (node, destination) == dense.next (node, destination),
                s"next ($node, $destination): sparse = ${sparse.next (node, destination)}, " +
                s"dense = ${dense.next (node, destination)}")

    assert (sparse.next (0, 4) == 1 && sparse.next (3, 5) == 5 && sparse.next (6, 4) == -1)
    banner ("RoutingTable sparse test: PASS")

end runRoutingTableSparseTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the edges of a `rows` x `cols` grid: each cell is joined to its right
 *  and lower neighbour in both directions.  Node id = row * cols + col.
 *  @param rows    the number of grid rows
 *  @param cols    the number of grid columns
 *  @param weight  supplies the cost of each directed edge
 */
private def gridEdges (rows: Int, cols: Int, weight: () => Double): (Array [Int], Array [Int], Array [Double]) =
    val from = ArrayBuffer [Int] ()
    val to   = ArrayBuffer [Int] ()
    val cost = ArrayBuffer [Double] ()

    def join (u: Int, v: Int): Unit =
        from += u; to += v; cost += weight ()
        from += v; to += u; cost += weight ()

    cfor (0, rows) { r =>
        cfor (0, cols) { c =>
            val u = r * cols + c
            if c + 1 < cols then join (u, u + 1)
            if r + 1 < rows then join (u, u + cols)
        }
    }
    (from.toArray, to.toArray, cost.toArray)
end gridEdges


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runRouteTable5000Node` main function tests the edge-array `RoutingTable`
 *  at street-network size.
 *   1. Equivalence: on a 3 x 3 grid with random costs, sparse and dense agree
 *      for every (node, destination) pair.  (The dense path prints `ShortestPath`
 *      debug output, so the grid is kept tiny.)
 *   2. Scale: a 50 x 100 grid (5000 nodes, 19,700 edges) with unit costs and 5
 *      destinations.  Every route is followed hop by hop from every node; each hop
 *      must be to an adjacent cell and the route length must equal the Manhattan
 *      distance, which is the exact shortest distance on a unit-cost grid.
 *  > runMain scalation.simulation.process.runRouteTable5000Node
 */
@main def runRouteTable5000Node (): Unit =

    // 1. equivalence with the dense constructor on a 3 x 3 grid, random costs
    //    (kept tiny: the dense ShortestPath prints its whole matrix at every step)
    banner ("RoutingTable 5000-node test: 1. sparse = dense on a 3 x 3 grid")
    val small    = 3
    val rand     = Uniform (1.0, 10.0, 0)
    val (sFrom, sTo, sCost) = gridEdges (small, small, () => rand.gen)
    val nSmall   = small * small
    val denseCost = new MatrixD (nSmall, nSmall)
    cfor (0, sFrom.length) { e => denseCost (sFrom(e), sTo(e)) = sCost(e) }
    val smallDest = Array (0, small - 1, nSmall - 1)
    val denseRt   = new RoutingTable ("Dense9", denseCost, smallDest)
    val sparseRt  = new RoutingTable ("Sparse9", nSmall, sFrom, sTo, sCost, smallDest)
    for node <- 0 until nSmall; destination <- 0 until nSmall do
        assert (sparseRt.next (node, destination) == denseRt.next (node, destination),
                s"next ($node, $destination): sparse = ${sparseRt.next (node, destination)}, " +
                s"dense = ${denseRt.next (node, destination)}")

    // 2. scale: 5000 nodes, every route walked and checked against Manhattan distance
    banner ("RoutingTable 5000-node test: 2. 50 x 100 grid, 5 destinations")
    val rows = 50
    val cols = 100
    val n    = rows * cols
    val (from, to, cost) = gridEdges (rows, cols, () => 1.0)
    val destinations = Array (0, cols - 1, (rows - 1) * cols, n - 1, (rows / 2) * cols + cols / 2)

    val (table, millis) = timed { new RoutingTable ("Grid5000", n, from, to, cost, destinations) }

    def row (v: Int): Int = v / cols
    def col (v: Int): Int = v % cols

    var routes = 0
    for destination <- destinations; start <- 0 until n do
        var node  = start
        var steps = 0
        while node != destination do
            val nxt = table.next (node, destination)
            assert (nxt >= 0, s"no next hop at node $node toward $destination")
            assert (math.abs (row (node) - row (nxt)) + math.abs (col (node) - col (nxt)) == 1,
                    s"hop $node -> $nxt is not to an adjacent cell")
            node = nxt
            steps += 1
            assert (steps <= n, s"route from $start to $destination loops")
        end while
        val manhattan = math.abs (row (start) - row (destination)) + math.abs (col (start) - col (destination))
        assert (steps == manhattan, s"route $start -> $destination takes $steps hops, shortest is $manhattan")
        routes += 1
    end for
    assert (table.next (destinations(0), destinations(0)) == destinations(0))

    val denseMB  = n.toLong * n * 8 / 1.0e6
    val sparseKB = n.toLong * destinations.length * 8 / 1.0e3
    println (f"nodes = $n, edges = ${from.length}, destinations = ${destinations.length}")
    println (f"routes walked and verified = $routes (every node to every destination)")
    println (f"built in $millis%.1f ms; hop table = $sparseKB%.0f KB")
    println (f"the dense path needs a $n x $n matrix = $denseMB%.0f MB, and holds four at once")
    banner ("RoutingTable 5000-node test: PASS")

end runRouteTable5000Node


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runRouteTableDynamic` main function tries dynamic routing.  The edge
 *  costs change from minute to minute (the loop index stands in for the simulated
 *  clock), the table is rebuilt from the edge arrays each minute, and the next hop
 *  must follow the costs.  No `RoutingTable` change is needed: only the cost array
 *  changes and a new table is built from it.
 *   1. Diamond A -> top -> D and A -> bottom -> D.  While the top edge is congested
 *      (minutes 3 to 5) the next hop at A must switch to the bottom, then switch back.
 *   2. The 50 x 100 grid, rebuilt every minute while a congested column band drifts
 *      east.  Every rebuild is timed, and routes from a sample of nodes are walked
 *      to check that they still reach every destination.
 *  > runMain scalation.simulation.process.runRouteTableDynamic
 */
@main def runRouteTableDynamic (): Unit =

    // 1. diamond: node 0 = A, 1 = top, 2 = bottom, 3 = D
    banner ("RoutingTable dynamic test: 1. diamond, congestion comes and goes")
    val from     = Array (0, 0, 1, 2)
    val to       = Array (1, 2, 3, 3)
    val base     = Array (10.0, 12.0, 10.0, 12.0)              // top path costs 20, bottom path 24
    val topEdge  = 0                                           // edge A -> top
    val edgeCost = base.clone ()

    for minute <- 0 until 9 do
        val congested = minute >= 3 && minute < 6
        edgeCost (topEdge) = if congested then 5.0 * base (topEdge) else base (topEdge)
        val table = new RoutingTable ("Diamond", 4, from, to, edgeCost, Array (3))
        val hop   = table.next (0, 3)
        println (s"minute $minute: A -> top costs ${edgeCost (topEdge)}, next hop at A toward D = " +
                 s"$hop (${if hop == 1 then "top" else "bottom"})")
        val expected = if congested then 2 else 1
        assert (hop == expected, s"minute $minute: next hop $hop, expected $expected")
    end for

    // 2. the 5000-node grid rebuilt every minute, a congested column band drifting east
    banner ("RoutingTable dynamic test: 2. 50 x 100 grid rebuilt every minute")
    val rows = 50
    val cols = 100
    val n    = rows * cols
    val (gFrom, gTo, unitCost) = gridEdges (rows, cols, () => 1.0)
    val gCost        = unitCost.clone ()
    val destinations = Array (0, cols - 1, (rows - 1) * cols, n - 1, (rows / 2) * cols + cols / 2)

    def row (v: Int): Int = v / cols
    def col (v: Int): Int = v % cols

    var totalMillis = 0.0
    for minute <- 0 until 6 do
        val band = 10 + 15 * minute                            // the congested column this minute
        for e <- gFrom.indices do                              // vertical moves inside the band cost 10
            gCost (e) = if col (gFrom (e)) == band && col (gTo (e)) == band then 10.0 else 1.0
        val (table, millis) = timed { new RoutingTable ("GridDynamic", n, gFrom, gTo, gCost, destinations) }
        totalMillis += millis

        var walks = 0
        for destination <- destinations; start <- 0 until n by 25 do
            var node  = start
            var steps = 0
            while node != destination do
                val nxt = table.next (node, destination)
                assert (nxt >= 0, s"minute $minute: no next hop at node $node toward $destination")
                assert (math.abs (row (node) - row (nxt)) + math.abs (col (node) - col (nxt)) == 1,
                        s"minute $minute: hop $node -> $nxt is not to an adjacent cell")
                node = nxt
                steps += 1
                assert (steps <= n, s"minute $minute: route from $start to $destination loops")
            end while
            walks += 1
        end for
        println (f"minute $minute: congested column = $band, rebuilt in $millis%.1f ms, $walks routes checked")
    end for

    println (f"6 rebuilds of the $n-node table took $totalMillis%.1f ms in total")
    banner ("RoutingTable dynamic test: PASS")

end runRouteTableDynamic
