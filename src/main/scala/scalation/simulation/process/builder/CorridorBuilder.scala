
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Tue Mar 25 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    CorridorBuilder — Builds Topology from CorridorLayout + FlowDirection
 *
 *  Standardizes the 10-step assembly pattern used by both CalRoute101_3 and
 *  EatonFireModel.  Handles junction ordering, segment remapping, route
 *  creation, and sink positioning based on FlowDirection.
 *
 *  The builder handles TOPOLOGY ONLY (junctions, route, sinks, ramp sensors).
 *  The model handles DEMAND (VSources, arrival rates, Car factory).
 *  Ramp objects also remain model-level because they require VSource as input.
 *
 *  @see config-layer-standard.md Section 4c
 */

package scalation
package simulation
package process
package builder

import scalation.mathstat.VectorD
import scalation.simulation.process.config.{CorridorLayout, FlowDirection, MultiCorridorConfig}
import scalation.simulation.process.config.{RampMode => ConfigRampMode}
import scala.collection.mutable.ListBuffer
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BuiltCorridor` case class bundles the topology products of building
 *  one corridor.  The model uses these fields directly in `Car.act()`.
 *
 *  On-ramps are NOT included — they require VSource (demand) as the `from`
 *  component, so the model creates them after creating its sources.
 *  Off-ramp sinks and join segments ARE included — the model creates off-ramp
 *  Ramp objects using `junc(offRampJoinSegs(r))` as `from` and
 *  `offRampSinks(r)` as `to`.
 *
 *  @param junctions        mainline junctions, ordered by flow direction
 *  @param rampSensors      on-ramp merge point junctions (positioned at on-ramp screen XY)
 *  @param offRampSensors   off-ramp diverge point junctions (positioned at off-ramp screen XY)
 *  @param route            multi-lane route across all segments
 *  @param sinks            sink(s) at the exit end of the corridor
 *  @param rampJoinSegs     segment indices where on-ramps merge (remapped by direction)
 *  @param offRampSinks     sink(s) for off-ramps (one per off-ramp)
 *  @param offRampJoinSegs  segment indices where off-ramps diverge (remapped by direction)
 *  @param hwLen            number of segments = junctions.length - 1 (for act() loop bound)
 *  @param numLanes         number of lanes per segment
 *  @param numSegments      number of segments
 */
case class BuiltCorridor (junctions:       Array [Junction],
                          rampSensors:     Array [Junction],
                          offRampSensors:  Array [Junction],
                          route:           Route,
                          sinks:           List [Sink],
                          rampJoinSegs:    Array [Int],
                          offRampSinks:    Array [Sink],
                          offRampJoinSegs: Array [Int],
                          hwLen:           Int,
                          numLanes:        Int,
                          numSegments:     Int)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BuiltNetwork` case class bundles the topology products of building
 *  a multi-corridor network.  Keyed by corridor ID.
 *
 *  @param corridors     map of corridor ID → BuiltCorridor
 *  @param ffConnectors  cross-corridor FF connector objects
 */
case class BuiltNetwork (corridors:    Map [String, BuiltCorridor],
                         ffConnectors: List [FFConnector])


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CorridorBuilder` object provides factory methods to build corridor
 *  topology from declarative config objects.
 *
 *  Two entry points:
 *    - `build()`      — single corridor → `BuiltCorridor`
 *    - `buildMulti()` — multi-corridor network → `BuiltNetwork`
 */
object CorridorBuilder:

    private val debug = debugf ("CorridorBuilder", true)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build topology for a single corridor.
     *  Creates junctions (direction-ordered), ramp sensor junctions, route,
     *  and sinks.  Does NOT create VSources or Ramps — the model owns demand.
     *
     *  @param layout     the corridor layout (topology + screen coordinates)
     *  @param direction  the flow direction (Ascending = NB/EB, Descending = WB/SB)
     *  @param motion     the car-following dynamics model
     *  @param nt         number of time intervals for Junction recording
     *  @param prefix     naming prefix for components (e.g., "I210", "SR134")
     */
    def build (layout: CorridorLayout, direction: FlowDirection,
               motion: Dynamics, nt: Int,
               prefix: String = ""): BuiltCorridor =

        val config      = layout.config
        val nLanes      = config.mainline.lanesPerSegment
        val lpsRaw      = config.mainline.lanesPerSeg.orNull   // per-segment lane counts (null if uniform)
        // Reverse lanesPerSeg for Descending direction (same as junctions and ramp join segments)
        val lps: Array[Int] = if lpsRaw == null then null
            else direction match
                case FlowDirection.Ascending  => lpsRaw
                case FlowDirection.Descending => lpsRaw.reverse
        val nSegments   = config.mainline.segments
        val nJunc       = layout.numJunctions
        val nOnRamps    = layout.numOnRamps
        val pfx         = if prefix.nonEmpty then s"${prefix.filter (_.isLetterOrDigit)}_" else ""

        debug ("build", s"corridor='${config.mainline.id}' dir=$direction " +
                        s"juncs=$nJunc segs=$nSegments lanes=$nLanes onRamps=$nOnRamps" +
                        s" lanesPerSeg=${if lps != null then lps.mkString("[",",","]") else "uniform"}")

        // ── Step 1: Mainline junctions ──────────────────────────────────────
        // Ascending: junctions in postmile order (junc(0) = low PM = entry)
        // Descending: reversed (junc(0) = high PM = entry for WB/SB traffic)

        val junc = Array.ofDim [Junction] (nJunc)
        direction match
            case FlowDirection.Ascending =>
                cfor (0, nJunc) { i =>
                    junc(i) = new Junction (s"${pfx}${layout.junctionNames(i)}",
                                            xy = layout.mainlineScreenXY(i), nt = nt, nl = nLanes)
                }
            case FlowDirection.Descending =>
                cfor (0, nJunc) { i =>
                    val ri = nJunc - 1 - i                     // reverse index
                    junc(i) = new Junction (s"${pfx}${layout.junctionNames(ri)}",
                                            xy = layout.mainlineScreenXY(ri), nt = nt, nl = nLanes)
                }
        end match

        debug ("build", s"junctions: ${junc.map (_.name).mkString (", ")}")

        // ── Step 2: Route ───────────────────────────────────────────────────
        // Built BEFORE ramp junctions so we can derive ramp positions from
        // Route lane geometry (rampAttachPoint).

        val intermediateJunc = junc.slice (1, junc.length - 1)
        val route = Route (s"${pfx}R", nLanes, intermediateJunc,
                           junc(0), junc.last, motion, lanesPerSeg = lps)

        debug ("build", s"route: ${route.pathway.length} pathways, " +
                        s"${intermediateJunc.length} intermediate junctions")

        // ── Step 3: Sinks ───────────────────────────────────────────────────

        val exitXY = junc.last.at
        val sinks = Sink.group (
            (exitXY(0).toInt - 100, exitXY(1).toInt - 100),
            (s"${pfx}sink", (0, 0))
        )

        // ── Step 4: Ramp join segments ──────────────────────────────────────

        val onRampConfigs = config.ramps.filter (_.mode == ConfigRampMode.On)
        val rampJoinSegs  = new Array [Int] (onRampConfigs.length)
        direction match
            case FlowDirection.Ascending =>
                cfor (0, onRampConfigs.length) { r =>
                    rampJoinSegs(r) = onRampConfigs(r).joinSegment
                }
            case FlowDirection.Descending =>
                cfor (0, onRampConfigs.length) { r =>
                    rampJoinSegs(r) = nSegments - 1 - onRampConfigs(r).joinSegment
                }
        end match

        val offRampConfigs  = config.ramps.filter (_.mode == ConfigRampMode.Off)
        val nOffRamps       = offRampConfigs.length
        val offRampJoinSegs = new Array [Int] (nOffRamps)
        direction match
            case FlowDirection.Ascending =>
                cfor (0, nOffRamps) { r =>
                    offRampJoinSegs(r) = offRampConfigs(r).joinSegment
                }
            case FlowDirection.Descending =>
                cfor (0, nOffRamps) { r =>
                    offRampJoinSegs(r) = nSegments - 1 - offRampConfigs(r).joinSegment
                }
        end match

        debug ("build", s"rampJoinSegs: ${rampJoinSegs.mkString (", ")}")
        debug ("build", s"offRamps=$nOffRamps, offRampJoinSegs: ${offRampJoinSegs.mkString (", ")}")

        // ── Step 5: Ramp junctions — derived from Route lane geometry ───────
        // rampAttachPoint(seg) returns the outermost lane edge at that segment.
        // Ramp VTransport extends outward from there by RAMP_LEN pixels.

        val RAMP_LEN  = 150.0                                   // visual ramp length (px)
        val FR_NUDGE  = 30.0                                    // downstream nudge for off-ramps at same seg as on-ramp (px)
        val (perpX, perpY) = route.perpVec                       // outward unit vector

        // Road direction unit vector (downstream = from → to)
        val rdx = junc.last.at(0) - junc(0).at(0)
        val rdy = junc.last.at(1) - junc(0).at(1)
        val rhyp = math.hypot (rdx, rdy).max (1e-9)
        val roadDirX = rdx / rhyp
        val roadDirY = rdy / rhyp

        // Build set of on-ramp join segments for collision detection
        val onRampSegSet = rampJoinSegs.toSet

        // On-ramp sensor junctions (road-edge end of ramp)
        val rampSensors = Array.ofDim [Junction] (nOnRamps)
        cfor (0, nOnRamps) { i =>
            val (ax, ay) = route.rampAttachPoint (rampJoinSegs(i))
            rampSensors(i) = new Junction (s"", xy = (ax, ay), nt = nt, nl = nLanes)
        }

        // Off-ramp sensor junctions (road-edge end of off-ramp)
        // Nudged downstream if an on-ramp shares the same joinSeg.
        val offRampSensors = Array.ofDim [Junction] (nOffRamps)
        cfor (0, nOffRamps) { r =>
            val (ax, ay) = route.rampAttachPoint (offRampJoinSegs(r))
            val nudge = if onRampSegSet.contains (offRampJoinSegs(r)) then FR_NUDGE else 0.0
            offRampSensors(r) = new Junction (s"",
                xy = (ax + roadDirX * nudge, ay + roadDirY * nudge), nt = nt, nl = nLanes)
        }

        // Off-ramp sinks (extend outward from attach point, with same nudge)
        val offRampSinks = new Array [Sink] (nOffRamps)
        cfor (0, nOffRamps) { r =>
            val (ax, ay) = route.rampAttachPoint (offRampJoinSegs(r))
            val nudge = if onRampSegSet.contains (offRampJoinSegs(r)) then FR_NUDGE else 0.0
            offRampSinks(r) = new Sink (s"", (ax + perpX * RAMP_LEN + roadDirX * nudge,
                                               ay + perpY * RAMP_LEN + roadDirY * nudge))
        }

        BuiltCorridor (
            junctions       = junc,
            rampSensors     = rampSensors,
            offRampSensors  = offRampSensors,
            route           = route,
            sinks           = sinks,
            rampJoinSegs    = rampJoinSegs,
            offRampSinks    = offRampSinks,
            offRampJoinSegs = offRampJoinSegs,
            hwLen           = nJunc - 1,
            numLanes        = nLanes,
            numSegments     = nSegments
        )
    end build

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build topology for a multi-corridor network.
     *  Calls `build()` per corridor, then creates FF connectors from specs.
     *
     *  @param config   the multi-corridor network configuration
     *  @param motion   the car-following dynamics model
     *  @param nt       number of time intervals (overrides config.nt if provided)
     */
    def buildMulti (config: MultiCorridorConfig,
                    motion: Dynamics, nt: Int = -1): BuiltNetwork =

        val timeIntervals = if nt > 0 then nt else config.nt

        debug ("buildMulti", s"corridors=${config.numCorridors} " +
                             s"interchanges=${config.numInterchanges}")

        // ── Build each corridor ─────────────────────────────────────────────

        val corridorMap = scala.collection.mutable.Map [String, BuiltCorridor] ()
        for entry <- config.corridors do
            val dir = entry.layout.config.mainline.direction
            val built = build (entry.layout, dir, motion, timeIntervals, entry.id)
            corridorMap(entry.id) = built
            debug ("buildMulti", s"built corridor '${entry.id}' " +
                                 s"(${built.numLanes} lanes, ${built.numSegments} segs)")
        end for

        // ── Build FF connectors from specs ──────────────────────────────────
        // Look up fromJunction and toJunction by name in the built corridors.
        // Create `spec.lanes` parallel connectors with visual offsets.

        val ffList = ListBuffer [FFConnector] ()
        for spec <- config.interchanges do
            val fromCorridor = corridorMap.getOrElse (spec.fromCorridorId,
                throw new IllegalArgumentException (
                    s"CorridorBuilder.buildMulti: corridor '${spec.fromCorridorId}' not found"))
            val toCorridor = corridorMap.getOrElse (spec.toCorridorId,
                throw new IllegalArgumentException (
                    s"CorridorBuilder.buildMulti: corridor '${spec.toCorridorId}' not found"))

            // Find junction by name substring match (same pattern as EatonFireModel.findJuncIdx)
            val fromIdx = findJuncIdx (fromCorridor.junctions, spec.fromJunction)
            val toIdx   = findJuncIdx (toCorridor.junctions, spec.toJunction)

            if fromIdx >= 0 && toIdx >= 0 then
                val fromJunc = fromCorridor.junctions(fromIdx)
                val toJunc   = toCorridor.junctions(toIdx)

                // Compute perpendicular unit vector for lane separation
                val dx = toJunc.at(0) - fromJunc.at(0)
                val dy = toJunc.at(1) - fromJunc.at(1)
                val hyp = math.hypot (dx, dy).max (1e-9)
                val perpX =  dy / hyp                       // 90-degree perpendicular
                val perpY = -dx / hyp

                // Create spec.lanes parallel FF connectors with visual offset
                val LANE_GAP = 25.0  // pixel offset between parallel FF lanes
                cfor (0, spec.lanes) { laneIdx =>
                    // Bend varies slightly per lane for visual separation
                    val bendOffset = 0.02 * (laneIdx - (spec.lanes - 1) / 2.0)
                    // Perpendicular offset to separate parallel lanes visually
                    val laneOffset = (laneIdx - (spec.lanes - 1) / 2.0) * LANE_GAP
                    val laneShift  = VectorD (perpX * laneOffset, perpY * laneOffset)
                    val laneName = if spec.lanes > 1 then s"${spec.id}_L$laneIdx" else spec.id
                    val ff = new FFConnector (laneName,
                                              fromJunc, toJunc,
                                              motion,
                                              splitRatio = spec.splitRatio / spec.lanes,  // split ratio per lane
                                              bend = 0.25 + bendOffset,
                                              laneShift = laneShift)
                    ffList += ff
                }
                debug ("buildMulti", s"FF '${spec.id}': " +
                       s"${fromJunc.name} → ${toJunc.name} " +
                       s"split=${spec.splitRatio}, lanes=${spec.lanes}")
            else
                debug ("buildMulti", s"WARNING: FF '${spec.id}' junction not found " +
                       s"(fromIdx=$fromIdx, toIdx=$toIdx)")
            end if
        end for

        BuiltNetwork (
            corridors    = corridorMap.toMap,
            ffConnectors = ffList.toList
        )
    end buildMulti

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the index of a junction whose name contains the given substring.
     *  Returns -1 if not found.
     *  @param juncs         the junction array to search
     *  @param nameContains  the substring to match against junction names
     */
    private def findJuncIdx (juncs: Array [Junction], nameContains: String): Int =
        var i = 0
        while i < juncs.length do
            if juncs(i).name.contains (nameContains) then return i
            i += 1
        end while
        -1                                                     // not found
    end findJuncIdx

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print a summary of a built corridor.
     *  @param id     the corridor identifier
     *  @param built  the built corridor result
     */
    def summary (id: String, built: BuiltCorridor): Unit =
        println ("\n" + "=" * 60)
        println (s"BUILT CORRIDOR: $id")
        println ("=" * 60)
        println (s"  Junctions:     ${built.junctions.length}")
        println (s"  Lanes:         ${built.numLanes}")
        println (s"  Segments:      ${built.numSegments}")
        println (s"  Ramp sensors:  ${built.rampSensors.length}")
        println (s"  Ramp join segs: [${built.rampJoinSegs.mkString (", ")}]")
        println (s"  Off-ramp sinks: ${built.offRampSinks.length}")
        println (s"  Off-ramp join segs: [${built.offRampJoinSegs.mkString (", ")}]")
        println (s"  Sinks:         ${built.sinks.length}")
        println (s"  hwLen:         ${built.hwLen}")
        println ("  Junction names:")
        cfor (0, built.junctions.length) { i =>
            println (f"    j$i%3d  ${built.junctions(i).name}")
        }
        println ("=" * 60)
    end summary

end CorridorBuilder

