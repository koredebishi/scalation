//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** NetworkBuilder -- GeoJSON network authority
 *
 *  PURPOSE
 *    Turn the cleaned OSM network (data/network/<name>/edges.geojson, written by
 *    scripts/build_osm_network.py) into simulation components.  Topology and
 *    resolution only; demand (VSource rates) stays in the model, like CorridorBuilder.
 *
 *  OWNERSHIP RULE
 *    NetworkBuilder is the only authority for GeoJSON topology, directed edge identity,
 *    projected geometry, shared graph nodes, and micro/meso edge construction. A model
 *    or TrafficModelBuilder must not recreate those roads from a second topology table;
 *    it may only attach demand, dynamics, destinations, and observations.
 *
 *  DATA FLOW
 *
 *    edges.geojson --read (Gson)--> List[NetEdge] --classify--> RoadGroup
 *                                                       |
 *                        +------------------------------+-----------------+
 *                        | resolution + speed from the RoadGroup table    |
 *                        v                                                v
 *                 MICRO edge                                        MESO edge
 *          Route(name, lanes, junc, from, to, motion)     PathwayMeso(name, junc=[], from, to,
 *          set VTransport length = segment length                VectorD(length), v0, capacity,
 *                                                              storageCapacity, flowDiagram)
 *                        |                                                |
 *                        +--------------------+---------------------------+
 *                                             v
 *          one Junction per graph NODE (u/v ids), shared by every edge touching it
 *                                             v
 *                     BuiltNetwork-style result handed to the Model
 *                     (junctions, pathways/routes by edge, in/out edges per node)
 *
 *  BUILDER DOES                          BUILDER DOES NOT
 *    read edges, classify, build           create VSource / arrival rates (model owns demand)
 *    Junction / Route / PathwayMeso        store speed/lanes/length a second time (derive from NetEdge)
 *    derive capacity + storage             change Route, Pathway, VTransport, MesoVTransport, Model
 *    report which values were defaulted    choose routes (ShortestPath/RoutingTable are used later)
 *
 *  ROAD GROUPS (7).  Speed = dominant (mode) maxspeed tag of that class in the
 *  Eaton edges.geojson; per-edge maxspeed tags are ignored by design.
 *
 *    group          OSM classes folded in                     resolution  speed (mph -> m/s)   mode share
 *    MOTORWAY       motorway                                  MICRO       car.vmax = 65 mph    65 (95.3%)
 *    MOTORWAY_LINK  motorway_link                             MICRO       car.vmax = 65 mph    65 (93.4%, n=61)
 *    PRIMARY        primary                                   MESO        35 -> 15.65          56.2%
 *    SECONDARY      secondary                                 MESO        35 -> 15.65          64.4%
 *    TERTIARY       tertiary                                  MESO        30 -> 13.41          36.3%
 *    RESIDENTIAL    residential                               MESO        25 -> 11.18          83.3%
 *    OTHER          unclassified, living_street, escape,      MESO        25 -> 11.18 (approved)
 *                   primary/secondary/tertiary_link
 *
 *  A VTransport takes no per-link speed (only a Dynamics object), but IDM reads
 *  the vehicle's own `vmax`, so `drive` sets car.vmax to the group speed on
 *  entry to every edge: the same speed law on both sides of a handoff.  The
 *  default Vehicle.vmax is 33.528 m/s (75 mph).  (mph -> m/s: x 0.44704)
 *
 *  MESO PARAMETERS, all derived from existing code, nothing invented
 *    fd       = IdmFlux (v0 = group speed, tau = Vehicle.T, s0 = Vehicle.s,
 *                        len = Vehicle.len, delta = Vehicle.del)   -- dynamics/LWR.scala:85
 *    capacity = lanes * fd.q_max * 3600            (veh/h)
 *    storage  = floor (lanes * length * fd.rho_m)  (vehicles; rho_m = 1/(s0+len))
 *    lanes    = lanes_effective from the file (flag edges where lanes_is_default)
 *    one IdmFlux per group is built once and reused.
 *
 *  SLICE 1 (first proof; real chain found in edges.geojson, an I-210 WB exit)
 *
 *    node 1687098935 --[MOTORWAY  Foothill Freeway, 6 lanes, 316 m]--> node 322563657
 *    node 322563657  --[MOTORWAY_LINK, 2 lanes, 312 m]--------------> node 316289321
 *    node 316289321  --[SECONDARY West Mountain Street, 2 lanes (default), 160 m]--> node 316289570
 *    node 316289570  --[RESIDENTIAL Chapman Avenue, 1 lane (default), 298 m]------> node 479223714
 *
 *    MICRO, MICRO | MESO, MESO  -- one micro->meso handoff at the ramp end.
 *    A VSource feeds node 1687098935; a Sink sits at node 479223714.
 *
 *  OPEN DECISIONS / RISKS (need your answer before code)
 *    (1 decided: OTHER group speed = 25 mph, approved.)
 *    (Lanes decided: meso edge = one MesoVTransport PER LANE, since a MesoVTransport
 *     is one lane; micro edge = Route, one Pathway/DLL per lane.  A car draws its
 *     lane once, at the source (EatonFireModel.scala:406 style), then keeps the
 *     lane index, limited to each edge's lane count.  Meso lanes are drawn with
 *     the same sideways offset as Route lanes.)
 *    (Routing as built: the graph owns the RoutingTable and looks up the next node
 *     itself, then finds the edge to it.  Junction.setOutgoing holds ONE component
 *     per next node, which cannot hold a set of parallel meso lanes, so it is not
 *     used.  A side effect: swapping in a rebuilt table is one assignment.)
 *
 *    2. Junction lane count (APPROVED: nl = largest lane count of the edges at
 *       the node).  Recorder is NOT changed: nLanes is already its constructor
 *       parameter (Recorder.scala:26) and only sizes three nt x nLanes matrices
 *       and the per-lane Statistic array.  Too small is unsafe: record()
 *       (line 111, lane_stat(lane)) has no bounds guard and throws;
 *       recordDensity() (line 92) is guarded and silently drops the sample.
 *       Known limit: a column is the lane index on whichever road the car
 *       arrived from, so lane 1 of a 2-lane ramp and lane 1 of a 6-lane road
 *       share a column at a shared node.  Totals stay right; the per-lane split
 *       at such a node mixes roads.  Unchecked: every Recorder builds its own
 *       EasyWriter (line 33), which may cost at 10k junctions.
 *
 *    3. Micro -> meso handoff (Rule 8).  Read from FreewayCorridorMixed, not
 *       DestinationRoutingMixed (which is all meso).  See HOW VEHICLES MOVE.
 *
 *    4. Junction screen position.  Each Junction needs an on-screen x,y so the
 *       animation can draw it.  It must use the SAME lat/lon -> x,y projection
 *       as the background map, or junctions float off the roads.  That
 *       projection is OsmRoadNetwork.buildProjector.  DONE: it stays private but
 *       is now `private [scalation]` (narrowest level reaching this package,
 *       same style as `private [process]` / `private [simulation]` elsewhere).
 *       The builder takes the projector as a parameter from the model.
 *
 *    5. Micro grouping.  Consecutive degree-two micro edges with the same road
 *       group and lane count are one Route/Pathway with multiple VTransport segments, preserving
 *       one DLL across the chain.  Ramps, branches, lane changes, and resolution
 *       boundaries remain separate transitions.
 *       READ (Pathway.scala:37, 92-96, 120-125; EatonFireModel.scala:353-418):
 *       a Pathway is ONE lane with its own DLL (vList); a Route is numLanes
 *       Pathways.  Equal-width handoffs preserve the lane; width changes use
 *       deterministic vehicle-id mapping before the caller joins it:
 *         laneID = <lane>
 *         route.pathway (laneID).joinAt (car, route.pathway (laneID).mergeLeaderAt (0), 0)
 *         ... route.pathway (laneID).seg (i).move () ...
 *         route.pathway (laneID).removeFromAlist (car)
 *       joinAt sets segId and disp itself.  Precedent for a lane-count change:
 *       Lane identity is preserved across edges and clamped when the next edge
 *       has fewer lanes; the builder does not invent a random lane change.
 *
 *    6. Destination.  Searched: Vehicle has no destination field.  SimActor has
 *       `subtype` (Int, "e.g. for turning choices") and `nextTransport`, but
 *       nothing in Junction/Pathway/Route reads either, and the corridor
 *       examples already use subtype as the lane.  The one working mechanism is
 *       the model's own Car (origin, destination) carrying the destination node
 *       index, given to Junction.jumpTo (destination) and RoutingTable.  Plan:
 *       keep that (no foundational edit).  A Vehicle.destination field would be
 *       a Rule 9 change, not proposed now.
 *
 *    7. Scale.  RoutingTable holds four dense N x N matrices at once (input cost,
 *       its transpose, nextHop, and one .copy per destination); ShortestPath
 *       itself is dense (database/graph_pm/ShortestPath.scala, c: MatrixD).
 *       For N = 10,127 each is about 820 MB.  No sparse graph or sparse
 *       shortest-path class exists in scalation (searched).
 *       PROPOSED FIX (needs approval; RoutingTable.scala only): add a second
 *       RoutingTable constructor that takes the edge arrays (from, to, cost)
 *       instead of a MatrixD, runs Dijkstra over per-node edge lists with the
 *       priority queue ShortestPath already uses, and stores nextHop as
 *       N x (number of destinations).  The dense constructor, ShortestPath and
 *       every current example stay untouched.  The builder is written against
 *       the sparse form from day one, so slice 1 and the county run the same code.
 *
 *  HOW VEHICLES MOVE (read from the existing examples, not assumed)
 *    DestinationRoutingMixed is ALL meso (its own comment, line 86); despite the
 *    name it shows routing, not a resolution handoff.  Routing pattern from it:
 *      node ids as Int, roads Map[(u,v) -> transport], cost matrix -> RoutingTable,
 *      junction.setRouting / setOutgoing, vehicle loop: transport.move, junction.jumpTo.
 *    The handoff pattern is in FreewayCorridorMixed (Rule 8, explicit both ways):
 *      meso     : link.move ; junction.jump
 *      micro in : pathway.joinAt (car, pathway.mergeLeaderAt (0), 0)
 *      micro    : seg(i).move ; junc(i).jump  (per segment)
 *      micro out: pathway.removeFromAlist (car)
 *    Both examples hardcode node constants, one setRouting/setOutgoing line per
 *    junction, and a hand-written act() per topology.  Reusable version, in
 *    plain ScalaTion style (arrays and a MatrixD, no Map abstraction):
 *      nodeId : Array [Long]      OSM id of node i (sorted; id -> i by binary search)
 *      junc   : Array [Junction]  junc(i) is node i
 *      edge   : Array [NetEdge]   one record per edge (case class, like StationRecord)
 *      from,to: Array [Int]       node indices of edge e
 *      comp   : Array [Component] comp(e) = MesoVTransport or micro Route of edge e
 *      cost   : Array [Double]    cost(e) = length / speed  (per edge, sparse; see item 7)
 *      Loops over e build every junction's setRouting / setOutgoing; no per-node lines.
 *    ONE generic walk (car, destination): match the next component
 *        MesoVTransport -> move ; Junction -> jumpTo (destination) ; micro Route ->
 *        joinAt, move segments, removeFromAlist.
 *    Nothing is stored twice: speed, lanes, length are read from edge(e).
 *
 *  NOT TOUCHED: Model, PathwayMeso, VTransport, MesoVTransport, Junction,
 *    CorridorBuilder, EatonFireModel, OsmRoadNetwork, Vehicle.
 */

package scalation
package simulation
package process
package builder

import scala.collection.immutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

import com.google.gson.JsonParser as GsonJsonParser

import scalation.dynamics.IdmFlux
import scalation.mathstat.VectorD
import scalation.random.Sharp
import scalation.simulation.process.meso.MesoVTransport

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RoadGroup` enumeration folds OSM highway classes into the seven groups
 *  that decide resolution and speed.  The speed is the dominant (mode) `maxspeed`
 *  tag of the class in the Eaton network; per-edge `maxspeed` tags are ignored.
 *  @param micro     whether edges of this group run microscopic (IDM car-following)
 *  @param speedMph  the group's speed in miles per hour
 */
enum RoadGroup (val micro: Boolean, val speedMph: Double):
    case Motorway     extends RoadGroup (true,  65.0)
    case MotorwayLink extends RoadGroup (true,  65.0)
    case Primary      extends RoadGroup (false, 35.0)
    case Secondary    extends RoadGroup (false, 35.0)
    case Tertiary     extends RoadGroup (false, 30.0)
    case Residential  extends RoadGroup (false, 25.0)
    case Other        extends RoadGroup (false, 25.0)

    /** The group's speed in metres per second. */
    def speed: Double = speedMph * 0.44704
end RoadGroup


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RoadGroup` companion object maps an OSM highway class to its group.
 */
object RoadGroup:

    def of (highway: String): RoadGroup = highway match
        case "motorway"      => Motorway
        case "motorway_link" => MotorwayLink
        case "primary"       => Primary
        case "secondary"     => Secondary
        case "tertiary"      => Tertiary
        case "residential"   => Residential
        case _               => Other

end RoadGroup


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NetEdge` case class is one directed road segment read from `edges.geojson`.
 *  Speed and resolution are derived from `highway`, never stored here.
 *  @param u        OSM id of the tail node
 *  @param v        OSM id of the head node
 *  @param highway  OSM highway class
 *  @param lanes    lanes in this direction (`lanes_effective`)
 *  @param length   length in metres
 *  @param uLat     latitude of the tail node
 *  @param uLon     longitude of the tail node
 *  @param vLat     latitude of the head node
 *  @param vLon     longitude of the head node
 *  @param name     the street name (may be empty)
 *  @param geometry complete directed centerline in (latitude, longitude) order
 *  @param ref      road reference, e.g., I 210 or CA 134 (may be empty)
 *  @param key      parallel-edge key; (u, v, key) identifies the directed edge
 *  @param osmid    original OSM way id(s), semicolon-separated for merged ways (may be empty)
 */
case class NetEdge (u: Long, v: Long, highway: String, lanes: Int, length: Double,
                    uLat: Double, uLon: Double, vLat: Double, vLon: Double, name: String,
                    geometry: Array [(Double, Double)] = Array.empty,
                    ref: String = "", key: Int = 0, osmid: String = ""):

    def group: RoadGroup = RoadGroup.of (highway)

end NetEdge


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BuiltGraph` class holds the components built from a set of edges and
 *  moves vehicles through them.
 *
 *  Node `i` is `node(i)`, a `Junction` or, for a sink node, a `Sink`.  Edge `e`
 *  runs from node `from(e)` to node `to(e)` at cost `cost(e)` and is either micro
 *  (`route(e)` non-null) or meso (`lane(e)` non-null, one link per lane).
 *  @param nodeId  sorted OSM node ids
 *  @param node    the component of each node
 *  @param edge    the edges
 *  @param from    tail node index of each edge
 *  @param to      head node index of each edge
 *  @param route   micro `Route` containing the edge, null for a meso edge
 *  @param microGroup  grouped micro-edge indices for each micro route
 *  @param lane    meso links of each edge (one per lane), null for a micro edge
 *  @param cost    travel-time cost of each edge (length / group speed)
 *  @param stream  retained for builder-call compatibility
 */
class BuiltGraph (val nodeId: Array [Long], val node: Array [Component], val edge: Array [NetEdge],
                  val from: Array [Int], val to: Array [Int], val route: Array [Route],
                  val lane: Array [Array [MesoVTransport]], val microGroup: Array [Array [Int]],
                  val cost: Array [Double], stream: Int):

    // edges leaving each node: outEdge (outStart (i)) until outEdge (outStart (i + 1))
    private val outStart = new Array [Int] (nodeId.length + 1)
    private val outEdge  = new Array [Int] (edge.length)
    cfor (0, edge.length) { e => outStart (from(e) + 1) += 1 }
    cfor (0, nodeId.length) { i => outStart (i + 1) += outStart (i) }
    private val fill = outStart.clone ()
    cfor (0, edge.length) { e => outEdge (fill (from(e))) = e; fill (from(e)) += 1 }

    /** The routing table used by `drive`; set by `routeTo`, replaceable at any time. */
    var table: RoutingTable = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of a node given its OSM id.
     *  @param id  the OSM node id
     */
    def nodeIndex (id: Long): Int =
        val i = java.util.Arrays.binarySearch (nodeId, id)
        require (i >= 0, s"node $id is not in this graph")
        i
    end nodeIndex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the routing table for the given destination node indices, from the
     *  current `cost` array.  Call again after changing `cost` to reroute.
     *  @param destinations  destination node indices (sink nodes)
     */
    def routeTo (destinations: Array [Int]): Unit =
        table = new RoutingTable ("RT", nodeId.length, from, to, cost, destinations)
    end routeTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return every component to hand to `Model.addComponent`.  A grouped micro
     *  chain is registered once as its `Route`; `Model.simulate` gives a registered
     *  component and all of its descendants a director.
     */
    def components: List [Component] =
        node.toList ++
        route.toList.filter (_ != null).distinct ++
        lane.toList.filter (_ != null).flatMap (_.toList)
    end components

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the edge leaving node `a` for node `b`.  When parallel edges join the
     *  same two nodes, return the cheapest, the one the routing table costed.
     *  @param a  the tail node index
     *  @param b  the head node index
     */
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
    /** Choose the lane at a graph-edge boundary.  Equal-width links preserve
     *  lane identity; a width change uses the vehicle id to distribute entries
     *  reproducibly over the new lanes.
     */
    private def handoffLane (car: Vehicle, previousLanes: Int, targetLanes: Int): Int =
        require (targetLanes > 0, "a transport must have at least one lane")
        if previousLanes <= 0 || car.laneID < 0 then Math.floorMod (car.id, targetLanes)
        else if previousLanes == targetLanes then math.min (car.laneID, targetLanes - 1)
        else Math.floorMod (car.id + car.laneID, targetLanes)
    end handoffLane

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Drive one vehicle from a start node to a destination sink, following the
     *  routing table.  Call from the vehicle's `act`.
     *  On every edge the car's IDM desired speed (`vmax`) is set to that edge's
 *  group speed.  Equal-width links preserve lane identity; width changes use
 *  deterministic vehicle-id handoff to distribute vehicles over target lanes.
     *   meso edge:  `move`, then cross the end junction
     *   micro edge: `joinAt` its DLL, `move`, cross the end junction,
     *               then `removeFromAlist` (the order used in `FreewayCorridorMixed`)
     *  @param car          the vehicle (the actor running this call)
     *  @param startNode    index of the node it starts at
     *  @param destination  index of the destination sink node
     */
    def drive (car: Vehicle, startNode: Int, destination: Int): Unit =
        require (table != null, "call routeTo before drive")
        require (node (destination).isInstanceOf [Sink], s"destination node ${nodeId (destination)} is not a Sink")
        var here  = startNode
        var previousLanes = 0
        while here != destination do
            val next = table.next (here, destination)
            require (next >= 0, s"no route from node ${nodeId (here)} to node ${nodeId (destination)}")
            val e = edgeBetween (here, next)

            val lanes = if route (e) != null then route (e).pathway.length else lane (e).length
            car.laneID = handoffLane (car, previousLanes, lanes)
            previousLanes = lanes
            car.vmax = edge (e).group.speed                             // one speed law: this edge's group speed

            var pathway: Pathway = null                                 // set only for a micro edge
            if route (e) != null then
                pathway = route (e).pathway (car.laneID)
                val group = microGroup (e)
                val first = group.indexOf (e)
                val firstSegment = if first >= 0 then first else 0
                pathway.joinAt (car, pathway.mergeLeaderAt (firstSegment), firstSegment)
                var i = firstSegment
                while i < group.length do
                    car.vmax = edge (group (i)).group.speed
                    pathway.seg (i).move ()
                    if i < pathway.junc.length then pathway.junc (i).jump ()
                    i += 1
                end while
                here = to (group.last)
            else
                lane (e)(car.laneID).move ()
                here = next
                node (here) match
                    case j: Junction => j.jump ()
                    case _           =>
            end if
            if pathway != null then pathway.removeFromAlist (car)
        end while
        node (destination).asInstanceOf [Sink].leave ()
    end drive

end BuiltGraph


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NetworkBuilder` object reads a cleaned OSM network and builds simulation
 *  components from it.  Topology and resolution only; the model owns demand.
 *  See the blueprint above.
 */
object NetworkBuilder:

    private val LANE_GAP = 10.0                                         // pixels between drawn lanes: passed to Route as laneGap and used for meso lanes
    private val NODE     = 4.0                                          // drawn size of a junction or sink disc, in pixels (the default is 20)
    private val JUNCTION_DELAY = 0.0                                    // ordinary imported nodes have no uncalibrated delay

    private val flux = new Array [IdmFlux] (RoadGroup.values.length)   // one IdmFlux per group, built on demand

    /** The IDM equilibrium diagram of a group, from the shared vehicle parameters. */
    private def fluxOf (g: RoadGroup): IdmFlux =
        if flux (g.ordinal) == null then
            flux (g.ordinal) = new IdmFlux (g.speed, Vehicle.T, Vehicle.s, Vehicle.len, Vehicle.del)
        flux (g.ordinal)
    end fluxOf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read every edge of an `edges.geojson` file written by `build_osm_network.py`.
     *  @param edgesPath  path to `data/network/<name>/edges.geojson`
     */
    def read (edgesPath: String): Array [NetEdge] =
        val src = scala.io.Source.fromFile (edgesPath, "UTF-8")
        val raw = try src.mkString finally src.close ()
        val features = GsonJsonParser.parseString (raw).getAsJsonObject.getAsJsonArray ("features").asScala

        val out = ArrayBuffer [NetEdge] ()
        for feature <- features do
            val obj    = feature.getAsJsonObject
            val p      = obj.getAsJsonObject ("properties")
            val coords = obj.getAsJsonObject ("geometry").getAsJsonArray ("coordinates")
            val first  = coords.get (0).getAsJsonArray                   // GeoJSON order: [lon, lat]
            val last   = coords.get (coords.size - 1).getAsJsonArray
            val geometry = Array.tabulate (coords.size) { i =>
                val point = coords.get (i).getAsJsonArray
                (point.get (1).getAsDouble, point.get (0).getAsDouble)
            }

            def text (key: String): String =
                val v = p.get (key)
                if v == null || v.isJsonNull then "" else v.getAsString

            out += NetEdge (p.get ("u").getAsLong, p.get ("v").getAsLong,
                            text ("highway").split (";").headOption.getOrElse (""),
                            p.get ("lanes_effective").getAsInt, p.get ("length").getAsDouble,
                            first.get (1).getAsDouble, first.get (0).getAsDouble,
                            last.get (1).getAsDouble, last.get (0).getAsDouble, text ("name"), geometry,
                            ref = text ("ref"), key = p.get ("key").getAsInt, osmid = text ("osmid"))
        end for
        out.toArray
    end read

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the edges that run along a path of node ids, in order.
     *  @param all   the edges to search
     *  @param path  consecutive OSM node ids
     */
    def edgesAlong (all: Array [NetEdge], path: Array [Long]): Array [NetEdge] =
        Array.tabulate (path.length - 1) { i =>
            all.find (e => e.u == path (i) && e.v == path (i + 1)).getOrElse (
                throw new IllegalArgumentException (s"no edge from node ${path (i)} to node ${path (i + 1)}"))
        }
    end edgesAlong

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the components for a set of edges.
     *   - one `Junction` per node (or a `Sink` for a sink node), with `nl` = the largest
     *     lane count of the edges touching it
     *   - a micro edge becomes a `Route` of `lanes` pathways; a meso edge becomes
     *     `lanes` `MesoVTransport`s, each with the group's `IdmFlux`, capacity =
     *     the diagram's `q_max` per lane
     *  @param edges      the edges to build
     *  @param sinkNodes  OSM ids of the sink nodes; they must have no outgoing edge
     *  @param project    latitude, longitude -> screen (x, y), the frame of the background map
     *  @param motion     the car-following dynamics for micro edges
     *  @param nt         number of time intervals for junction recording
     *  @param stream     retained for builder-call compatibility
     *  @param sourceComponents optional source components keyed by their OSM node id;
     *                          these replace the generated node at that location
     */
    def build (edges: Array [NetEdge], sinkNodes: Array [Long], project: (Double, Double) => (Double, Double),
               motion: Dynamics, nt: Int, stream: Int = 0,
               sourceComponents: Map [Long, Component] = Map.empty): BuiltGraph =
        require (edges.nonEmpty, "no edges to build")

        val nodeId = (edges.map (_.u) ++ edges.map (_.v)).distinct.sorted
        val n      = nodeId.length
        val at     = new Array [(Double, Double)] (n)                   // screen position of each node
        val nl     = Array.fill (n) (1)                                 // junction lane count
        val from   = new Array [Int] (edges.length)
        val to     = new Array [Int] (edges.length)

        cfor (0, edges.length) { e =>
            val ed = edges (e)
            from(e) = java.util.Arrays.binarySearch (nodeId, ed.u)
            to(e)   = java.util.Arrays.binarySearch (nodeId, ed.v)
            at (from(e)) = project (ed.uLat, ed.uLon)
            at (to(e))   = project (ed.vLat, ed.vLon)
            nl (from(e)) = math.max (nl (from(e)), ed.lanes)
            nl (to(e))   = math.max (nl (to(e)), ed.lanes)
        }

        val node = new Array [Component] (n)
        cfor (0, n) { i =>
            node(i) =
                if sourceComponents.contains (nodeId(i)) then sourceComponents (nodeId(i))
                else if sinkNodes.contains (nodeId(i)) then new Sink ("SK", Array (at(i)._1, at(i)._2, NODE, NODE))
                else new Junction ("JC", Sharp (JUNCTION_DELAY), Array (at(i)._1, at(i)._2, NODE, NODE), nt, nl(i))
        }

        val route = new Array [Route] (edges.length)
        val lane  = new Array [Array [MesoVTransport]] (edges.length)
        val outgoing = Array.fill (n) (ArrayBuffer [Int] ())
        val incoming = Array.fill (n) (ArrayBuffer [Int] ())
        val cost  = new Array [Double] (edges.length)

        cfor (0, edges.length) { e =>
            outgoing (from(e)) += e
            incoming (to(e)) += e
        }

        // Group only degree-two, same-lane microscopic chains.  Branches,
        // ramps, lane changes, and resolution boundaries remain graph edges.
        val microGroup = Array.fill [Array [Int]] (edges.length) (null)
        val visited = Array.fill (edges.length) (false)
        cfor (0, edges.length) { e =>
            val tail = from(e)
            val hasMicroPredecessor = incoming(tail).exists { f =>
                edges(f).group == edges(e).group && edges(f).lanes == edges(e).lanes
            }
            val startsChain = sourceComponents.contains (nodeId(tail)) ||
                              incoming(tail).size != 1 || outgoing(tail).size != 1 || !hasMicroPredecessor
            if edges(e).group.micro && !visited(e) && startsChain then
                val chain = ArrayBuffer [Int] (e)
                visited(e) = true
                var current = e
                var extend = true
                while extend do
                    val nodeAt = to(current)
                    val next = outgoing(nodeAt).filter { f =>
                        edges(f).group == edges(current).group && edges(f).lanes == edges(current).lanes
                    }
                    val prev = incoming(nodeAt).filter { f =>
                        edges(f).group == edges(current).group && edges(f).lanes == edges(current).lanes
                    }
                    if outgoing(nodeAt).size == 1 && incoming(nodeAt).size == 1 &&
                       next.length == 1 && prev.length == 1 &&
                       !sourceComponents.contains (nodeId(nodeAt)) then
                        val f = next(0)
                        if !visited(f) then
                            chain += f
                            visited(f) = true
                            current = f
                        else extend = false
                    else extend = false
                end while
                val grouped = chain.toArray
                for f <- grouped do microGroup(f) = grouped
            end if
        }
        cfor (0, edges.length) { e =>
            if edges(e).group.micro && microGroup(e) == null then microGroup(e) = Array (e)
        }

        cfor (0, edges.length) { e =>
            val ed = edges (e)
            val g  = ed.group
            require (! node(from(e)).isInstanceOf [Sink], s"sink node ${ed.u} must have no outgoing edge")
            cost(e) = ed.length / g.speed

            if g.micro then
                val grouped = microGroup(e)
                if grouped.head == e then
                    val last  = edges (grouped.last)
                    val junctions = grouped.dropRight (1).map (f => node(to(f)).asInstanceOf [Junction])
                    val segmentGeometry = grouped.map (f => edges(f).geometry.map (p => project (p._1, p._2)))
                    val r = Route ("MR", ed.lanes, junctions, node(from(e)), node(to(grouped.last)), motion,
                                   laneGap = LANE_GAP, segmentGeometry = segmentGeometry)
                    for i <- grouped.indices do
                        for pathway <- r.pathway do pathway.seg(i).length = edges(grouped(i)).length
                    for f <- grouped do route(f) = r
                end if
            else
                val fd   = fluxOf (g)
                val dx   = at(to(e))._1 - at(from(e))._1                // lane offset as in Route: perpendicular to the edge
                val dy   = at(to(e))._2 - at(from(e))._2
                val hyp  = math.hypot (dx, dy)
                val perp = if hyp > 0.0 then VectorD (dy / hyp, -dx / hyp) else VectorD (0.0, 0.0)
                lane(e) = Array.tabulate (ed.lanes) { k =>
                    val physicalLane = ed.lanes - 1 - k                 // lane k is Route's pathway (k)
                    val shift = perp * ((physicalLane - (ed.lanes - 1) / 2.0) * LANE_GAP)
                    val centerline = ed.geometry.map (p => project (p._1, p._2))
                    val laneGeometry = centerline.map (p => (p._1 + shift (0), p._2 + shift (1)))
                    val link = new MesoVTransport ("ML", node(from(e)), node(to(e)),
                                                   g.speed, fd.q_max * 3600.0,
                                                   shift1 = shift, shift2 = shift, flowDiagram = Some (fd),
                                                   geometry = laneGeometry)
                    link.length = ed.length
                    link
                }
            end if
        }

        new BuiltGraph (nodeId, node, edges, from, to, route, lane, microGroup, cost, stream)
    end build

end NetworkBuilder
