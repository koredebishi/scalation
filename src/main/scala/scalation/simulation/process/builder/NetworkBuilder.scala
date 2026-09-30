//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** NetworkBuilder -- GeoJSON network authority
 *
 *  PURPOSE
 *    Turn the cleaned OSM network (data/network/<name>/edges.geojson, written by
 *    scripts/build_osm_network.py) into simulation components.  Topology and
 *    resolution only; demand (VSource rates) stays in the consuming model.
 *
 *  OWNERSHIP RULE
 *    NetworkBuilder is the reusable OSM topology API and the only authority for GeoJSON topology, directed edge identity,
 *    projected geometry, shared graph nodes, and micro/meso edge construction. A model
 *    or TrafficModelBuilder must not recreate those roads from a second topology table;
 *    it may only attach demand, dynamics, destinations, and observations.
 *
 *  DATA FLOW
 *
 *    edges.geojson --read (Gson)--> List[NetEdge] --classify--> EdgeProfile
 *                                                       |
 *                        +------------------------------+-----------------+
 *                        | resolution + speed from the caller's policy     |
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
 *                     BuiltGraph result handed to the Model
 *                     (junctions, pathways/routes by edge, in/out edges per node)
 *
 *  BUILDER DOES                          BUILDER DOES NOT
 *    read edges, classify, build           create VSource / arrival rates (model owns demand)
 *    Junction / Route / PathwayMeso        store speed/lanes/length a second time (derive from NetEdge)
 *    derive capacity + storage             change Route, Pathway, VTransport, MesoVTransport, Model
 *    report which values were defaulted    choose routes (ShortestPath/RoutingTable are used later)
 *
 *  API BOUNDARY
 *    A model declares `OsmSpec`, prepares the named artifact, and supplies its own
 *    demand, destinations, and projector.  This builder consumes the artifact and
 *    returns a `BuiltGraph`; `BuiltGraph.drive` is generic mixed-resolution graph
 *    traversal, while source and destination policy remains in the model.
 *
 *  EdgeProfile is supplied by the consuming model.  The profile controls
 *  resolution, speed, and the grouping identity used to preserve micro DLLs.
 *  The builder derives the mesoscopic parameters from that speed:
 *    fd       = IdmFlux (v0 = profile.speed, tau = Vehicle.T, s0 = Vehicle.s,
 *                        len = Vehicle.len, delta = Vehicle.del)   -- dynamics/LWR.scala:85
 *    capacity = lanes * fd.q_max * 3600            (veh/h)
 *    storage  = floor (lanes * length * fd.rho_m)  (vehicles; rho_m = 1/(s0+len))
 *    lanes    = lanes_effective from the file (flag edges where lanes_is_default)
 *    one IdmFlux per group is built once and reused.
 *
 *  CONSTRUCTION GUARANTEES
 *    - Each graph node is represented by one shared Junction or Sink.
 *    - A microscopic chain is one Route with one Pathway/DLL per lane.
 *    - A mesoscopic edge creates one MesoVTransport per lane and derives its
 *      flow diagram, capacity, and storage from the supplied profile speed.
 *    - Resolution boundaries, branches, ramps, lane changes, and source nodes
 *      stop microscopic grouping so that handoffs remain explicit.
 *    - The supplied projector is used for every node and edge geometry, keeping
 *      the simulation components aligned with the model's background map.
 *
 *  VEHICLE POLICY
 *    BuiltGraph provides the generic mixed-resolution traversal primitive.
 *    The consuming model supplies arrivals, destinations, routing, and source
 *    components; those policies are not inferred from OSM topology here.
 *
 *  SCALATION COMPONENTS NOT MODIFIED
 *    Model, PathwayMeso, VTransport, MesoVTransport, Junction, Vehicle, and
 *    the native statistics and routing components remain their own authorities.
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
/** The `NetworkBuilder` object reads a cleaned OSM network and builds simulation
 *  components from it.  Topology and resolution only; the model owns demand.
 *  See the blueprint above.
 */
object NetworkBuilder:

    private val LANE_GAP = 10.0                                         // pixels between drawn lanes: passed to Route as laneGap and used for meso lanes
    private val NODE     = 4.0                                          // drawn size of a junction or sink disc, in pixels (the default is 20)
    private val JUNCTION_DELAY = 0.0                                    // ordinary imported nodes have no uncalibrated delay

    private val flux = scala.collection.mutable.Map [Double, IdmFlux] ()

    /** The IDM equilibrium diagram for a supplied edge speed. */
    private def fluxOf (speed: Double): IdmFlux =
        flux.getOrElseUpdate (speed,
            new IdmFlux (speed, Vehicle.T, Vehicle.s, Vehicle.len, Vehicle.del))
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
     *     `lanes` `MesoVTransport`s, each with the profile's `IdmFlux`, capacity =
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
               motion: Dynamics, nt: Int, edgePolicy: EdgePolicy, stream: Int = 0,
               sourceComponents: Map [Long, Component] = Map.empty): BuiltGraph =
        require (edges.nonEmpty, "no edges to build")
        val profile = edges.map (edgePolicy.profile)

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

        val builtLink = new Array [BuiltLink] (edges.length)
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
                profile(f) == profile(e) && edges(f).lanes == edges(e).lanes
            }
            val startsChain = sourceComponents.contains (nodeId(tail)) ||
                              incoming(tail).size != 1 || outgoing(tail).size != 1 || !hasMicroPredecessor
            if profile(e).micro && !visited(e) && startsChain then
                val chain = ArrayBuffer [Int] (e)
                visited(e) = true
                var current = e
                var extend = true
                while extend do
                    val nodeAt = to(current)
                    val next = outgoing(nodeAt).filter { f =>
                        profile(f) == profile(current) && edges(f).lanes == edges(current).lanes
                    }
                    val prev = incoming(nodeAt).filter { f =>
                        profile(f) == profile(current) && edges(f).lanes == edges(current).lanes
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
            if profile(e).micro && microGroup(e) == null then microGroup(e) = Array (e)
        }

        cfor (0, edges.length) { e =>
            val ed = edges (e)
            val p  = profile (e)
            require (! node(from(e)).isInstanceOf [Sink], s"sink node ${ed.u} must have no outgoing edge")
            cost(e) = ed.length / p.speed

            if p.micro then
                val grouped = microGroup(e)
                if grouped.head == e then
                    val last  = edges (grouped.last)
                    val junctions = grouped.dropRight (1).map (f => node(to(f)).asInstanceOf [Junction])
                    val segmentGeometry = grouped.map (f => edges(f).geometry.map (p => project (p._1, p._2)))
                    val r = Route ("MR", ed.lanes, junctions, node(from(e)), node(to(grouped.last)), motion,
                                   laneGap = LANE_GAP, segmentGeometry = segmentGeometry)
                    for i <- grouped.indices do
                        for pathway <- r.pathway do pathway.seg(i).length = edges(grouped(i)).length
                    for f <- grouped do builtLink(f) = MicroLink (r, grouped)
                end if
            else
                val fd   = fluxOf (p.speed)
                val dx   = at(to(e))._1 - at(from(e))._1                // lane offset as in Route: perpendicular to the edge
                val dy   = at(to(e))._2 - at(from(e))._2
                val hyp  = math.hypot (dx, dy)
                val perp = if hyp > 0.0 then VectorD (dy / hyp, -dx / hyp) else VectorD (0.0, 0.0)
                val lanes = Array.tabulate (ed.lanes) { k =>
                    val physicalLane = ed.lanes - 1 - k                 // lane k is Route's pathway (k)
                    val shift = perp * ((physicalLane - (ed.lanes - 1) / 2.0) * LANE_GAP)
                    val centerline = ed.geometry.map (p => project (p._1, p._2))
                    val laneGeometry = centerline.map (p => (p._1 + shift (0), p._2 + shift (1)))
                    val link = new MesoVTransport ("ML", node(from(e)), node(to(e)),
                                                   p.speed, fd.q_max * 3600.0,
                                                   shift1 = shift, shift2 = shift, flowDiagram = Some (fd),
                                                   geometry = laneGeometry)
                    link.length = ed.length
                    link
                }
                builtLink(e) = MesoLink (lanes)
            end if
        }

        new BuiltGraph (nodeId, node, edges, from, to, builtLink, cost, profile, stream)
    end build

end NetworkBuilder
