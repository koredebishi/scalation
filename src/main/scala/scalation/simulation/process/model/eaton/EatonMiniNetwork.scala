//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** EatonMini network-assembly ownership.
 *
 *  This file will contain Eaton-specific network inputs: the prepared
 *  OsmSpec, projection, source placement, destinations, and the call to the
 *  reusable NetworkBuilder API. It must not duplicate graph construction.
 */

package scalation
package simulation
package process
package model
package eaton

import scala.collection.mutable.{ArrayBuffer, PriorityQueue, Map => MMap, Set => MSet}

import scalation.animation.OsmRoadNetwork
import scalation.simulation.process.Dynamics
import scalation.simulation.process.builder.{BuiltGraph, EdgePolicy, EdgeProfile, EdgeSpot, NetEdge, NetworkBuilder}
import scalation.simulation.process.config.{EatonCorridorConfig, StationRecord}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** EatonMini OSM network assembly and projection owner. */
class EatonMiniNetwork (stream: Int = 0):

    private val osm = EatonMiniConfig.osm
    private val osmEdges = NetworkBuilder.read (osm.ensurePrepared ())

    //--------------------------------------------------
    // Sinks (area exits): every node with no outgoing edge, plus every two-way road
    // cut by the box edge; the cut end's outbound edge is dropped so it can be a sink
    private def nearBoxEdge (lat: Double, lon: Double): Boolean =
        val b = osm.bounds
        val gap = math.min (math.min (lat - b.south, b.north - lat), math.min (lon - b.west, b.east - lon))
        gap < EatonMiniConfig.boxEdgeTolerance
    end nearBoxEdge

    private val cutEnds: Set [Long] =
        val outOf = osmEdges.groupBy (_.u)
        val into  = osmEdges.groupBy (_.v)
        outOf.keys.filter { n =>
            val out = outOf (n).map (_.v).distinct
            val in  = into.getOrElse (n, Array.empty [NetEdge]).map (_.u).distinct
            val end = outOf (n)(0)
            out.length == 1 && in.length == 1 && out(0) == in(0) && nearBoxEdge (end.uLat, end.uLon)
        }.toSet

    private val trimmed = osmEdges.filterNot (e => cutEnds.contains (e.u))

    //--------------------------------------------------
    // Ramp influence zones (HCM, 1,500 ft): the freeway runs meso except the stretch
    // `rampInfluence` m downstream of each merge and upstream of each diverge, which runs
    // micro with the ramps; a zone that ends inside an edge cuts it there (`splitAt`)
    private def isRamp (e: NetEdge): Boolean    = e.highway == "motorway_link"
    private def isFreeway (e: NetEdge): Boolean = e.highway == "motorway"

    private val reviewedEdges: Set [(Long, Long)] =      // reviewed station edges keep their ids: never cut
        EatonCorridorConfig.loadStationEdges (EatonCorridorConfig.loadStationMap (), trimmed)
            .collect { case (_, Some (e), _) => (e.u, e.v) }.toSet

    private val minPiece = 5.0                           // metres: no cut this close to a node
    private var zoned    = trimmed
    private val zone     = MSet [(Long, Long)] ()          // micro freeway edges
    private val reach    = MMap [(Long, Boolean), Double] ()   // most zone left on arriving at a node
    private var nextId   = -1L                           // cut nodes get negative ids

    private def cover (node: Long, left: Double, downstream: Boolean): Unit =
        if left >= minPiece && reach.getOrElse ((node, downstream), 0.0) < left then
            reach ((node, downstream)) = left
            val along = zoned.filter (e => isFreeway (e) && (if downstream then e.u == node else e.v == node))
                             .map (e => (e.u, e.v))
            for (u, v) <- along do
                val k = zoned.indexWhere (e => e.u == u && e.v == v)
                if k >= 0 then
                    val e    = zoned(k)
                    val next = if downstream then v else u
                    if e.length <= left + minPiece || zone.contains ((u, v)) || reviewedEdges.contains ((u, v)) then
                        zone += ((u, v))                                 // the whole edge is in the zone
                        cover (next, left - e.length, downstream)
                    else
                        val offset = if downstream then left else e.length - left
                        zoned = NetworkBuilder.splitAt (zoned, EdgeSpot (k, offset, 0.0, 0.0), nextId)
                        zone += (if downstream then (u, nextId) else (nextId, v))
                        nextId -= 1
                    end if
                end if
            end for
        end if
    end cover

    private val merges   = trimmed.filter (isRamp).map (_.v).toSet.filter (n => trimmed.exists (e => isFreeway (e) && e.u == n))
    private val diverges = trimmed.filter (isRamp).map (_.u).toSet.filter (n => trimmed.exists (e => isFreeway (e) && e.v == n))
    for n <- merges do cover (n, EatonMiniConfig.rampInfluence, downstream = true)
    for n <- diverges do cover (n, EatonMiniConfig.rampInfluence, downstream = false)

    val edges: Array [NetEdge] = zoned

    private val freewayKm = edges.filter (isFreeway).map (_.length).sum / 1000.0
    private val microKm   = edges.filter (e => zone.contains ((e.u, e.v))).map (_.length).sum / 1000.0
    println (f"EatonMiniNetwork: ${merges.size} merges, ${diverges.size} diverges; micro freeway " +
             f"$microKm%.1f of $freewayKm%.1f km (${-nextId - 1} cuts)")

    //--------------------------------------------------
    // Resolution: ramps and influence zones micro, everything else meso; speeds from the config
    private val zonePolicy: EdgePolicy = new EdgePolicy:
        def profile (edge: NetEdge): EdgeProfile =
            EatonMiniConfig.edgePolicy.profile (edge).copy (micro = isRamp (edge) || zone.contains ((edge.u, edge.v)))
    end zonePolicy

    val sinks: Array [Long]    = (edges.map (_.v).toSet -- edges.map (_.u).toSet).toArray.sorted
    println (s"EatonMiniNetwork: ${sinks.length} sinks (${cutEnds.size} cut roads at the box edge)")

    //--------------------------------------------------
    // Projection and the I-210 WB source position
    private val sourceEdge = edges.find (_.u == EatonMiniConfig.sourceId).get
    private val pxPerM = 2.9
    private val boxW = 1000.0
    private val boxH = 600.0
    private val westOfSrc = 114.0
    private val northOfSrc = 69.0
    private val mLat = 111320.0
    private val mLon = 111320.0 * math.cos (math.toRadians (sourceEdge.uLat))
    private val nwLat = sourceEdge.uLat + northOfSrc / mLat
    private val nwLon = sourceEdge.uLon - westOfSrc / mLon
    private val anchors = Array ((nwLat, nwLon),
                                (nwLat - boxH / mLat, nwLon + boxW / mLon))

    val project = OsmRoadNetwork.buildProjector (
        anchors, (boxW * pxPerM + 40.0, boxH * pxPerM + 40.0), "EatonMiniTest")
    val sourcePosition = project (sourceEdge.uLat, sourceEdge.uLon)

    //--------------------------------------------------
    // Place a PeMS station on an edge: its reviewed binding when there is one, else the
    // nearest edge of its road type heading the freeway's way (printed for review)
    // (OR on a ramp edge with the freeway nearer ahead than behind, FR the reverse; ramps
    // at an interchange often join each other, so reachability alone cannot tell them apart)
    private def rampDistance (seeds: Set [Long], ahead: Boolean): Map [Long, Double] =
        val step = edges.filter (isRamp).groupBy (e => if ahead then e.v else e.u)
        val dist = MMap [Long, Double] ()
        val pq   = PriorityQueue.empty [(Double, Long)] (using Ordering.by [(Double, Long), Double] (-_._1))
        for s <- seeds do
            dist (s) = 0.0
            pq.enqueue ((0.0, s))
        while pq.nonEmpty do
            val (d, n) = pq.dequeue ()
            if d <= dist (n) then
                for e <- step.getOrElse (n, Array.empty [NetEdge]) do
                    val m  = if ahead then e.u else e.v
                    val dm = d + e.length
                    if dm < dist.getOrElse (m, Double.MaxValue) then
                        dist (m) = dm
                        pq.enqueue ((dm, m))
        end while
        dist.toMap
    end rampDistance

    private val toFreeway   = rampDistance (edges.filter (isFreeway).map (_.u).toSet, ahead = true)    // metres along ramps
    private val fromFreeway = rampDistance (edges.filter (isFreeway).map (_.v).toSet, ahead = false)
    private def aheadOf (e: NetEdge): Double  = toFreeway.getOrElse (e.v, Double.MaxValue)
    private def behindOf (e: NetEdge): Double = fromFreeway.getOrElse (e.u, Double.MaxValue)

    private val onRampEdges  = edges.filter (e => isRamp (e) && aheadOf (e) < Double.MaxValue && aheadOf (e) <= behindOf (e))
                                    .map (e => (e.u, e.v)).toSet
    private val offRampEdges = edges.filter (e => isRamp (e) && behindOf (e) < Double.MaxValue && behindOf (e) <= aheadOf (e))
                                    .map (e => (e.u, e.v)).toSet

    def placeStation (station: StationRecord, heading: Double, reviewed: Option [NetEdge]): Option [NetEdge] =
        reviewed.orElse {
            val mainline = station.laneType == "ML"
            val wanted: NetEdge => Boolean =
                if mainline then isFreeway
                else if station.laneType == "OR" then e => onRampEdges.contains ((e.u, e.v))
                else e => offRampEdges.contains ((e.u, e.v))
            val maxTurn  = if mainline then 45.0 else EatonMiniConfig.rampTurn
            val spot = NetworkBuilder.matchEdge (edges, (station.latitude, station.longitude), heading,
                                                 wanted, maxTurn)
            val edge = edges (spot.edge)
            val ok   = spot.distance <= EatonMiniConfig.maxStationDistance
            println (f"  station ${station.stationId} ${station.freeway}${station.direction} ${station.laneType}%-2s " +
                     f"${station.location}%-20s -> edge ${edge.u}->${edge.v}  ${spot.distance}%.0f m" +
                     (if ok then "" else "  REJECTED (too far)"))
            if ok then Some (edge) else None
        }
    end placeStation

    //--------------------------------------------------
    // The routing table's fixed path between two node indices (empty when there is none)
    def path (graph: BuiltGraph, from: Int, to: Int): Array [Int] =
        val nodes = ArrayBuffer (from)
        var here  = from
        while here != to && here >= 0 && nodes.length <= graph.nodeId.length do
            here = graph.table.next (here, to)
            if here >= 0 then nodes += here
        end while
        if here == to then nodes.toArray else Array.empty
    end path

    //--------------------------------------------------
    // For one origin, group the sinks by the off-ramp where the fixed path to each first
    // leaves the freeway (key -1: no monitored off-ramp on the path, or no path)
    def exitGroups (graph: BuiltGraph, origin: Int, sinkNodes: Array [Int],
                    offRamps: Map [(Int, Int), Int]): Map [Int, Array [Int]] =
        sinkNodes.groupBy { s =>
            val p = path (graph, origin, s)
            var k = -1
            var i = 0
            while k < 0 && i < p.length - 1 do
                k = offRamps.getOrElse ((p(i), p(i + 1)), -1)
                i += 1
            end while
            if p.isEmpty then -1 else k
        }
    end exitGroups

    //--------------------------------------------------
    // The freeway end reached from a node: keep to the same road (`ref`, e.g. "I 210"),
    // which OSM may tag as motorway_link through an interchange; with no such edge, take
    // the motorway edge that turns least; stop at a sink
    def freewayEnd (start: Long): Long =
        val sinkSet = sinks.toSet
        var here    = start
        var heading = Double.NaN
        var ref     = ""
        val seen    = ArrayBuffer (start)
        while ! sinkSet.contains (here) do
            val out  = edges.filter (e => e.u == here && e.highway.startsWith ("motorway"))
            val same = out.filter (e => ref.nonEmpty && e.ref == ref)
            val pool = if same.nonEmpty then same else out.filter (_.highway == "motorway")
            require (pool.nonEmpty, s"the freeway from $start ends at $here, which is not a sink")
            val pick = if heading.isNaN then pool(0)
                       else pool.minBy (e => turn (NetworkBuilder.heading ((e.uLat, e.uLon), (e.vLat, e.vLon)), heading))
            heading = NetworkBuilder.heading ((pick.uLat, pick.uLon), (pick.vLat, pick.vLon))
            if pick.ref.nonEmpty then ref = pick.ref
            require (! seen.contains (pick.v), s"the motorway from $start loops at ${pick.v}")
            seen += pick.v
            here = pick.v
        end while
        here
    end freewayEnd

    private def turn (a: Double, b: Double): Double =
        val d = math.abs (a - b) % 360.0
        if d > 180.0 then 360.0 - d else d
    end turn

    def build (motion: Dynamics, nt: Int): BuiltGraph =
        NetworkBuilder.build (edges, sinks, project, motion, nt, zonePolicy, stream)
    end build
end EatonMiniNetwork
