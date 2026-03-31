//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sat Mar 22 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Eaton Corridor Configuration — Station Map & GPS → Animation Coordinates
 *
 *  Reads PeMS station_map.csv for the Eaton fire corridor (I-210 + SR-134),
 *  categorizes stations by lane type (ML, HV, OR, FR, FF), and converts GPS
 *  coordinates to animation screen positions using the Coordinates class.
 *
 *  All stations are fed into a single Coordinates call so that I-210 and SR-134
 *  share one consistent animation coordinate frame.
 */

package scalation
package simulation
package process
package config

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scalation.banner                                    // ScalaTion's formatted section header printer
import scalation.mathstat.{VectorD, Plot}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StationRecord` case class represents one row from a PeMS station_map.csv.
 *  @param stationId  the PeMS station ID (e.g., 717615)
 *  @param freeway    the freeway number (e.g., 210, 134)
 *  @param direction  the direction ("W" or "E")
 *  @param laneType   the lane type ("ML", "HV", "OR", "FR", "FF")
 *  @param latitude   the GPS latitude
 *  @param longitude  the GPS longitude
 *  @param absPM      the absolute postmile (physical road ordering)
 *  @param location   the location name (e.g., "FAIR OAKS 1")
 *  @param lanes      the number of lanes at this station
 */
case class StationRecord (stationId: Int, freeway: Int, direction: String,
                          laneType: String, latitude: Double, longitude: Double,
                          absPM: Double, location: String, lanes: Int)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StationWithCoords` case class pairs a station record with its computed
 *  animation screen coordinates.
 *  @param record    the station metadata from PeMS
 *  @param screenXY  the (x, y) animation screen position
 */
case class StationWithCoords (record: StationRecord, screenXY: (Double, Double))


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CorridorLayout` case class bundles everything a traffic model needs
 *  to instantiate Junction, Route, VSource, Sink, and Ramp objects from a
 *  PeMS station_map.csv file.
 *
 *  This is the intermediate representation between the raw PeMS CSV and
 *  the simulation model — a single flat file keyed on the `Lane Type` column.
 *
 *  @param config            the NetworkConfig topology (segments, ramps, sensors)
 *  @param junctionNames     human-readable names from the CSV Location field
 *  @param mainlineScreenXY  screen (x, y) positions for mainline Junction nodes
 *  @param onRampScreenXY    screen (x, y) positions for on-ramp Junction nodes (shifted)
 *  @param offRampScreenXY   screen (x, y) positions for off-ramp Junction nodes (shifted)
 *  @param segmentLengths    physical segment lengths in meters (from postmile differences)
 *  @param ffStations        freeway-to-freeway connector station records from PeMS
 *  @param junctionPMs       absolute postmiles for each mainline junction (empty for non-PeMS corridors)
 */
case class CorridorLayout (config: NetworkConfig,
                           junctionNames: Array [String],
                           mainlineScreenXY: Array [(Double, Double)],
                           onRampScreenXY: Array [(Double, Double)],
                           offRampScreenXY: Array [(Double, Double)],
                           segmentLengths: VectorD,
                           ffStations: Array [StationRecord],
                           junctionPMs: Array [Double] = Array.empty [Double]):

    /** All ramp screen positions (on-ramps first, then off-ramps). */
    def allRampScreenXY: Array [(Double, Double)] = onRampScreenXY ++ offRampScreenXY

    /** Number of mainline junctions. */
    def numJunctions: Int = mainlineScreenXY.length

    /** Number of on-ramps. */
    def numOnRamps: Int = onRampScreenXY.length

    /** Number of off-ramps. */
    def numOffRamps: Int = offRampScreenXY.length

    /** Whether this layout has postmile data (PeMS-derived corridors do, DonaldDoyle does not). */
    def hasPMs: Boolean = junctionPMs.nonEmpty

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the name of the nearest mainline junction for a given absolute postmile.
     *  Uses linear scan to find the junction with the smallest PM distance.
     *  Only valid for PeMS-derived corridors that have postmile data.
     *  @param pm  the absolute postmile to match against
     */
    def findJunctionByPM (pm: Double): String =
        assert (hasPMs, s"findJunctionByPM: no postmile data for corridor '${config.mainline.id}'")
        var bestIdx  = 0                                        // index of closest junction
        var bestDist = math.abs (junctionPMs(0) - pm)           // distance to closest junction
        cfor (1, junctionPMs.length) { i =>
            val dist = math.abs (junctionPMs(i) - pm)
            if dist < bestDist then
                bestDist = dist
                bestIdx  = i
        }
        junctionNames(bestIdx)
    end findJunctionByPM

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the segment index where a given postmile falls.
     *  Returns the largest junction index whose PM <= the given PM,
     *  clamped to valid segment range [0, numSegments - 1].
     *  @param pm  the absolute postmile to locate
     */
    def findSegmentByPM (pm: Double): Int =
        assert (hasPMs, s"findSegmentByPM: no postmile data for corridor '${config.mainline.id}'")
        var i = junctionPMs.length - 1
        while i >= 0 && junctionPMs(i) > pm do i -= 1
        math.max (0, math.min (i, config.mainline.segments - 1))
    end findSegmentByPM

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute VSource center position and offsets for animation layout.
     *  Center is near the first mainline junction.  Each ramp VSource is offset
     *  relative to this center using a shift so it does not overlap the road.
     *  @param dx  horizontal shift from on-ramp screen position (default 200)
     *  @param dy  vertical shift from on-ramp screen position (default -100)
     */
    def getVSourceCenterAndOffsets (dx: Double = 200.0, dy: Double = -100.0): ((Int, Int), Array [(Int, Int)]) =
        val centerPos = ((mainlineScreenXY(0)._1 - 50.0).toInt,
                         (mainlineScreenXY(0)._2 + 50.0).toInt)
        val nOR     = onRampScreenXY.length
        val offsets = new Array [(Int, Int)] (1 + nOR)
        offsets(0) = (0, 0)                                    // mainline offset
        cfor (0, nOR) { r =>
            offsets(r + 1) = (
                (onRampScreenXY(r)._1 + dx).toInt - centerPos._1,
                (onRampScreenXY(r)._2 + dy).toInt - centerPos._2
            )
        }
        (centerPos, offsets)
    end getVSourceCenterAndOffsets

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print a summary of this corridor layout.
     */
    def summary (): Unit =
        println ("\n" + "=" * 70)
        println ("CORRIDOR LAYOUT SUMMARY")
        println ("=" * 70)
        println (s"Corridor:     ${config.mainline.id}")
        println (s"Junctions:    ${mainlineScreenXY.length}")
        println (s"Segments:     ${config.mainline.segments}")
        println (s"Lanes:        ${config.mainline.lanesPerSegment}")
        println (s"On-Ramps:     ${onRampScreenXY.length}")
        println (s"Off-Ramps:    ${offRampScreenXY.length}")
        println (s"FF Connectors: ${ffStations.length}")
        println (s"Sensors:      ${config.sensors.length}")
        println (f"Total Length:  ${segmentLengths.sum}%.0f m  (${segmentLengths.sum / 1609.34}%.2f mi)")
        println ("=" * 70)

        println ("\nJunctions (by postmile):")
        cfor (0, junctionNames.length) { i =>
            val (sx, sy) = mainlineScreenXY(i)
            val pmStr = if hasPMs then f"  PM=${junctionPMs(i)}%7.3f" else ""
            println (f"  j$i%3d  ${junctionNames(i)}%-28s$pmStr  Screen($sx%7.1f, $sy%7.1f)")
        }

        println (s"\nOn-Ramps (${config.ramps.count (_.mode == RampMode.On)}):")
        for r <- config.ramps if r.mode == RampMode.On do
            println (f"  ${r.id}%-35s  joinSeg=${r.joinSegment}%3d  [${r.lanes} lane(s)]")

        println (s"\nOff-Ramps (${config.ramps.count (_.mode == RampMode.Off)}):")
        for r <- config.ramps if r.mode == RampMode.Off do
            println (f"  ${r.id}%-35s  joinSeg=${r.joinSegment}%3d  [${r.lanes} lane(s)]")

        if ffStations.nonEmpty then
            println (s"\nFF Connectors (${ffStations.length}):")
            cfor (0, ffStations.length) { i =>
                val ff = ffStations(i)
                println (f"  ${ff.stationId}%6d  ${ff.location}%-28s  PM=${ff.absPM}%7.3f")
            }
        println ("=" * 70)
    end summary

end CorridorLayout


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EatonCorridorConfig` object loads the Eaton fire corridor station map
 *  and computes animation coordinates for all stations.
 *
 *  Pipeline:  station_map.csv → StationRecord → GPS (lat, lon)
 *             → Coordinates class → screen (x, y)
 *
 *  Usage:
 *  {{{
 *      val stations = EatonCorridorConfig.loadStationMap ()
 *      val all      = EatonCorridorConfig.computeAllCoordinates (stations)
 *      val i210wML  = EatonCorridorConfig.filterByFreewayDir (all, 210, "W")
 *                       |> EatonCorridorConfig.filterByLaneType (_, "ML")
 *  }}}
 */
object EatonCorridorConfig:

    // Master station map — contains ALL stations for I-210 (W+E) and SR-134 (W)
    // 194 rows: 77 I-210 W + 73 I-210 E + 43 SR-134 W
    private val STATION_MAP_PATH =
        "data/WSC-Pems-Data-Eaton-Fire/data-eaton/pems/eaton-corridor/station_map.csv"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load all station records from a PeMS station_map.csv file.
     *  @param path  the path to station_map.csv (default: Eaton corridor master file)
     */
    def loadStationMap (path: String = STATION_MAP_PATH): Array [StationRecord] =
        val source = Source.fromFile (path)
        val buf    = ArrayBuffer [StationRecord] ()
        try
            val lines = source.getLines ().drop (1)            // skip header row
            for line <- lines if line.trim.nonEmpty do
                val c = line.split (",", -1).map (_.trim)      // split CSV, keep empty trailing fields
                buf += StationRecord (
                    stationId = c(0).toInt,                    // PeMS VDS ID (e.g., 717615)
                    freeway   = c(1).toInt,                    // 210 or 134
                    direction = c(2),                          // "W" or "E"
                    laneType  = c(3),                          // "ML", "HV", "OR", "FR", "FF"
                    latitude  = c(4).toDouble,                 // GPS lat
                    longitude = c(5).toDouble,                 // GPS lon
                    absPM     = c(6).toDouble,                 // absolute postmile — defines road order
                    location  = c(7),                          // human-readable name (e.g., "FAIR OAKS 1")
                    lanes     = if c.length > 8 && c(8).nonEmpty then c(8).toInt else 0  // lane count (0 if missing)
                )
            end for
        finally source.close ()
        buf.toArray
    end loadStationMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute animation coordinates for all stations in one shared coordinate frame.
     *  All GPS points go into a single `Coordinates` call so that I-210 and SR-134
     *  are positioned correctly relative to each other on screen.
     *  @param stations  all station records to convert
     *  @param dims      animation window dimensions (width, height) in pixels
     */
    def computeAllCoordinates (stations: Array [StationRecord],
                               dims: (Double, Double) = (1500.0, 1500.0)): Array [StationWithCoords] =
        val n      = stations.length
        val gps    = Array.ofDim [(Double, Double)] (n)        // collect all GPS pairs
        cfor (0, n) { i => gps(i) = (stations(i).latitude, stations(i).longitude) }

        // ONE Coordinates call — all stations share the same animation frame
        // so I-210 and SR-134 appear in correct spatial relationship
        val coords = new scalation.Coordinates (dims._1, dims._2, gps)   // GPS → CTM → screen (x, y)
        val result = Array.ofDim [StationWithCoords] (n)
        cfor (0, n) { i => result(i) = StationWithCoords (stations(i), coords.aniCoords(i)) }
        result
    end computeAllCoordinates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter paired stations by freeway number and direction, sorted by postmile.
     *  @param all      all paired station-coordinate records
     *  @param freeway  the freeway number (210 or 134)
     *  @param dir      the direction ("W" or "E")
     */
    def filterByFreewayDir (all: Array [StationWithCoords],
                            freeway: Int, dir: String): Array [StationWithCoords] =
        val buf = ArrayBuffer [StationWithCoords] ()
        cfor (0, all.length) { i =>
            val s = all(i)
            if s.record.freeway == freeway && s.record.direction == dir then buf += s
        }
        buf.sortBy (_.record.absPM).toArray                    // sort by postmile = physical road order
    end filterByFreewayDir

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter paired stations by lane type, sorted by postmile.
     *  @param corridor  stations for a specific corridor
     *  @param laneType  the lane type ("ML", "HV", "OR", "FR", "FF")
     */
    def filterByLaneType (corridor: Array [StationWithCoords],
                          laneType: String): Array [StationWithCoords] =
        val buf = ArrayBuffer [StationWithCoords] ()
        cfor (0, corridor.length) { i =>
            if corridor(i).record.laneType == laneType then buf += corridor(i)
        }
        buf.sortBy (_.record.absPM).toArray                    // maintain road order within type
    end filterByLaneType

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a complete `CorridorLayout` for a single freeway corridor.
     *  Computes coordinates independently (own frame).  For a shared frame
     *  across multiple corridors use `buildSharedWBLayouts` instead.
     *
     *  @param freeway    the freeway number (e.g., 210, 134)
     *  @param direction  the direction (e.g., "W", "E")
     *  @param corridorId the identifier string (e.g., "I-210-W")
     *  @param dims       animation window dimensions (width, height) in pixels
     *  @param rampShift  lateral pixel shift for ramp junctions so they don't overlap mainline
     */
    def buildCorridorLayout (freeway: Int, direction: String,
                             corridorId: String,
                             dims: (Double, Double) = (5000.0, 3000.0),
                             rampShift: (Double, Double) = (30.0, -40.0)): CorridorLayout =
        val flowDir = if direction == "W" || direction == "S"
                      then FlowDirection.Descending else FlowDirection.Ascending
        val allRecords      = loadStationMap ()
        val corridorRecords = allRecords.filter (s => s.freeway == freeway && s.direction == direction)
        val corridorWithCoords = computeAllCoordinates (corridorRecords, dims)
        buildLayoutFromCoords (corridorWithCoords, corridorId, rampShift, flowDir)
    end buildCorridorLayout

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build both WB corridor layouts (I-210 W + SR-134 W) in ONE shared
     *  coordinate frame so they appear in correct spatial relationship.
     *  This is the preferred entry point for the Eaton fire model.
     *
     *  @param dims       animation window dimensions (width, height) in pixels
     *  @param rampShift  lateral pixel shift for ramp junctions
     */
    def buildSharedWBLayouts (dims: (Double, Double) = (5000.0, 3000.0),
                              rampShift: (Double, Double) = (30.0, -40.0)): (CorridorLayout, CorridorLayout) =
        val allRecords = loadStationMap ()
        val wbRecords  = allRecords.filter (s =>
            (s.freeway == 210 || s.freeway == 134) && s.direction == "W")
        val allWithCoords = computeAllCoordinates (wbRecords, dims)
        val i210  = filterByFreewayDir (allWithCoords, 210, "W")
        val sr134 = filterByFreewayDir (allWithCoords, 134, "W")
        (buildLayoutFromCoords (i210,  "I-210-W",  rampShift, FlowDirection.Descending),
         buildLayoutFromCoords (sr134, "SR-134-W", rampShift, FlowDirection.Descending))
    end buildSharedWBLayouts

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a `CorridorLayout` from pre-computed `StationWithCoords`.
     *  This is the topology-building core shared by `buildCorridorLayout` and
     *  `buildSharedWBLayouts`.
     *
     *  Algorithm:
     *    1. Classify by Lane Type → ML, OR, FR, FF
     *    2. ML stations sorted by absPM → junction positions and segments
     *    3. OR/FR stations matched to nearest ML segment by absPM → RampSpec
     *    4. Bundle into CorridorLayout
     *
     *  @param corridorWithCoords  stations with screen coordinates (one corridor)
     *  @param corridorId          the identifier string
     *  @param rampShift           lateral pixel shift for ramp junctions
     *  @param flowDir             the flow direction (Ascending for NB/EB, Descending for WB/SB)
     */
    private def buildLayoutFromCoords (corridorWithCoords: Array [StationWithCoords],
                                       corridorId: String,
                                       rampShift: (Double, Double),
                                       flowDir: FlowDirection = FlowDirection.Ascending): CorridorLayout =

        // 1. Classify by lane type (each returns sorted by absPM)
        val mlStations = filterByLaneType (corridorWithCoords, "ML")
        val orStations = filterByLaneType (corridorWithCoords, "OR")
        val frStations = filterByLaneType (corridorWithCoords, "FR")
        val ffStations = filterByLaneType (corridorWithCoords, "FF")

        val nML = mlStations.length
        assert (nML >= 2, s"buildLayoutFromCoords ($corridorId): need >= 2 ML stations, found $nML")

        // 2. ML postmiles define junction positions (sorted ascending)
        val mlPMs = new Array [Double] (nML)
        cfor (0, nML) { i => mlPMs(i) = mlStations(i).record.absPM }

        // 3. Lane counts per station and per segment
        //    Per-station lane counts from PeMS sensor data
        //    OVERRIDE known bad PeMS stations where Lanes != through-lanes:
        //      717627 WALNUT      : PeMS=2 (weaving detector), actual=4 GP
        //      769926 OCEAN VIEW  : PeMS=6 (includes aux/collector), actual=4 GP
        //      764137 MARENGO     : PeMS=6 (includes aux), actual=5 (4 GP + 1 HOV)
        val laneOverrides: Map [Int, Int] = Map (717627 -> 4, 769926 -> 4, 764137 -> 5)
        val laneCounts = new Array [Int] (nML)
        cfor (0, nML) { i =>
            val sid = mlStations(i).record.stationId
            laneCounts(i) = laneOverrides.getOrElse (sid, mlStations(i).record.lanes)
        }
        //    Per-segment: segment i spans station i to station i+1 → min lanes
        val nSegments = nML - 1
        val segLaneCounts = new Array [Int] (nSegments)
        cfor (0, nSegments) { i => segLaneCounts(i) = math.min (laneCounts(i), laneCounts(i + 1)) }
        val lanesPerSegment = segLaneCounts.max    // max for Route array sizing

        // 4. Junction names from Location field
        val junctionNames = new Array [String] (nML)
        cfor (0, nML) { i => junctionNames(i) = mlStations(i).record.location }

        // 5. Segment lengths from PM differences (miles → meters)
        val segLens   = new VectorD (nSegments)
        cfor (0, nSegments) { i => segLens(i) = (mlPMs(i + 1) - mlPMs(i)) * 1609.34 }

        // 6. Map OR stations → RampSpec (mode = On)
        val onRamps = ArrayBuffer [RampSpec] ()
        cfor (0, orStations.length) { r =>
            val pm  = orStations(r).record.absPM
            val seg = findJoinSegment (pm, mlPMs)
            val loc = orStations(r).record.location.replace (" ", "_")
            onRamps += RampSpec (
                id          = s"onramp_${orStations(r).record.stationId}_$loc",
                joinSegment = seg,
                mode        = RampMode.On,
                lanes       = orStations(r).record.lanes
            )
        }

        // 7. Map FR stations → RampSpec (mode = Off)
        val offRamps = ArrayBuffer [RampSpec] ()
        cfor (0, frStations.length) { r =>
            val pm  = frStations(r).record.absPM
            val seg = findJoinSegment (pm, mlPMs)
            val loc = frStations(r).record.location.replace (" ", "_")
            offRamps += RampSpec (
                id          = s"offramp_${frStations(r).record.stationId}_$loc",
                joinSegment = seg,
                mode        = RampMode.Off,
                lanes       = frStations(r).record.lanes
            )
        }

        // 8. Sensors: one per ML station
        val sensors = ArrayBuffer [SensorSpec] ()
        cfor (0, nML) { i =>
            sensors += SensorSpec (
                id      = s"sensor_${mlStations(i).record.stationId}",
                segment = i
            )
        }

        // 9. Assemble NetworkConfig
        val networkConfig = NetworkConfig (
            mainline = MainlineSpec (
                id              = corridorId,
                segments        = nSegments,
                lanesPerSegment = lanesPerSegment,
                segmentLengths  = Some (segLens),
                direction       = flowDir,
                lanesPerSeg     = Some (segLaneCounts)
            ),
            ramps   = onRamps.toList ++ offRamps.toList,
            sensors = sensors.toList
        )

        // 10. Screen coordinates — mainline
        val mainlineScreenXY = new Array [(Double, Double)] (nML)
        cfor (0, nML) { i => mainlineScreenXY(i) = mlStations(i).screenXY }

        // 11. Screen coordinates — on-ramps with lateral shift
        val onRampScreenXY = new Array [(Double, Double)] (orStations.length)
        cfor (0, orStations.length) { i =>
            val (sx, sy) = orStations(i).screenXY
            onRampScreenXY(i) = (sx + rampShift._1, sy + rampShift._2)
        }

        // 12. Screen coordinates — off-ramps (same side as on-ramps)
        //     FR stations that share a PM with an OR (±0.02) get a 50 px x-nudge
        //     so they don't overlap.  All others get standard rampShift only.
        val orPMs = new Array [Double] (orStations.length)
        cfor (0, orStations.length) { i => orPMs(i) = orStations(i).record.absPM }

        val offRampScreenXY = new Array [(Double, Double)] (frStations.length)
        cfor (0, frStations.length) { i =>
            val (sx, sy) = frStations(i).screenXY
            val frPM     = frStations(i).record.absPM
            var collides = false
            cfor (0, orPMs.length) { j =>
                if math.abs (frPM - orPMs(j)) < 0.02 then collides = true
            }
            val nudge = if collides then 50.0 else 0.0         // 50 px downstream separation
            offRampScreenXY(i) = (sx + rampShift._1 + nudge, sy + rampShift._2)
        }

        // 13. FF station records (metadata)
        val ffRecords = new Array [StationRecord] (ffStations.length)
        cfor (0, ffStations.length) { i => ffRecords(i) = ffStations(i).record }

        // 14. Bundle
        CorridorLayout (
            config           = networkConfig,
            junctionNames    = junctionNames,
            mainlineScreenXY = mainlineScreenXY,
            onRampScreenXY   = onRampScreenXY,
            offRampScreenXY  = offRampScreenXY,
            segmentLengths   = segLens,
            ffStations       = ffRecords,
            junctionPMs      = mlPMs                            // preserve postmiles for PM-based lookups
        )
    end buildLayoutFromCoords

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the segment index where a ramp (OR/FR) merges or diverges.
     *  Uses binary search: find the largest ML index whose postmile <= rampPM.
     *  @param rampPM  the ramp's absolute postmile
     *  @param mlPMs   the sorted array of ML station postmiles
     */
    private def findJoinSegment (rampPM: Double, mlPMs: Array [Double]): Int =
        var i = mlPMs.length - 1
        while i >= 0 && mlPMs(i) > rampPM do i -= 1
        math.max (0, math.min (i, mlPMs.length - 2))
    end findJoinSegment

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the statistical mode (most frequent value) of an integer array.
     *  @param arr  the array of integer values
     */
    private def modeLanes (arr: Array [Int]): Int =
        val counts = scala.collection.mutable.Map [Int, Int] ()
        cfor (0, arr.length) { i =>
            val v = arr(i)
            counts(v) = counts.getOrElse (v, 0) + 1
        }
        counts.maxBy (_._2)._1
    end modeLanes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Pre-built corridor layouts (lazy — loaded on first access)
    // I-210 W and SR-134 W share ONE coordinate frame via buildSharedWBLayouts
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private lazy val _sharedWB: (CorridorLayout, CorridorLayout) = buildSharedWBLayouts ()

    /** I-210 Westbound — shared frame with SR-134 W. */
    lazy val I210_WB: CorridorLayout = _sharedWB._1

    /** SR-134 Westbound — shared frame with I-210 W. */
    lazy val SR134_WB: CorridorLayout = _sharedWB._2

    /** I-210 Eastbound — contraflow direction (own frame, future). */
    lazy val I210_EB: CorridorLayout = buildCorridorLayout (210, "E", "I-210-E")

end EatonCorridorConfig


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Plot station GPS positions as a spatial scatter (longitude vs latitude).
 *  Produces a bird's-eye view comparable to Google Maps.
 *  @param stations  the stations to plot
 *  @param title     the plot window title
 */
private def plotGPSMap (stations: Array [StationWithCoords], title: String): Unit =
    val n = stations.length
    val lon = new VectorD (n)                                  // x-axis: longitude (east-west)
    val lat = new VectorD (n)                                  // y-axis: latitude (north-south)
    cfor (0, n) { i =>
        lon(i) = stations(i).record.longitude
        lat(i) = stations(i).record.latitude
    }
    new Plot (lon, lat, null, title, lines = false)
end plotGPSMap



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Print stations grouped by lane type for a given corridor.
 *  @param corridor   the filtered corridor stations with screen coordinates
 *  @param laneTypes  the lane types to report
 */
private def printStationsByType (corridor: Array [StationWithCoords],
                                 laneTypes: Array [String]): Unit =
    cfor (0, laneTypes.length) { t =>
        val typed = EatonCorridorConfig.filterByLaneType (corridor, laneTypes(t))
        if typed.nonEmpty then
            println (s"\n  ${laneTypes(t)} — ${typed.length} stations:")
            cfor (0, typed.length) { i =>
                val r        = typed(i).record
                val (sx, sy) = typed(i).screenXY               // animation pixel position
                println (f"    ${r.stationId}%6d  ${r.location}%-25s  PM=${r.absPM}%7.3f  " +
                         f"GPS(${r.latitude}%.6f, ${r.longitude}%.6f)  →  " +
                         f"Screen($sx%7.1f, $sy%7.1f)  [${r.lanes} lanes]")
            }
        end if
    }
end printStationsByType


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test the Eaton corridor GPS → animation coordinate pipeline.
 *  Loads station_map.csv, computes screen coordinates for all stations in one
 *  shared frame, and prints the GPS → Screen mapping by corridor and lane type.
 *
 *  > runMain scalation.simulation.process.config.testEatonCorridorCoords
 */
@main def testEatonCorridorCoords (): Unit =

    import EatonCorridorConfig.*

    banner ("Eaton Corridor: Station Map → Animation Coordinates")

    // 1. Load all 194 stations from master station_map.csv
    val stations = loadStationMap ()
    println (s"Total stations loaded: ${stations.length}")

    // 2. Compute aniCoords — ALL stations in one Coordinates call
    //    so I-210 W, I-210 E, and SR-134 W share one animation frame
    val all = computeAllCoordinates (stations)
    println (s"Coordinates computed: ${all.length} stations")

    // 3. Report: for each corridor (I-210 WB, I-210 EB, SR-134 WB),
    //    print stations grouped by lane type (ML, HV, OR, FR, FF)
    val corridors = Array ((210, "W"), (134, "W"))              // WB only — evacuation direction
    val laneTypes = Array ("ML", "HV", "OR", "FR", "FF")      // ML=mainline, HV=HOV, OR=on-ramp, FR=off-ramp, FF=freeway-freeway

    cfor (0, corridors.length) { c =>
        val (fwy, dir) = corridors(c)
        val label    = if fwy == 134 then s"SR-$fwy" else s"I-$fwy"
        banner (s"$label ${dir}B")
        val corridor = filterByFreewayDir (all, fwy, dir)      // filter to this corridor, sorted by PM
        println (s"Total stations in corridor: ${corridor.length}")
        printStationsByType (corridor, laneTypes)               // print each lane type group
    }

    // 4. Single combined GPS spatial map — WB corridors only (evacuation direction)
    val wbOnly = filterByFreewayDir (all, 210, "W") ++ filterByFreewayDir (all, 134, "W")
    plotGPSMap (wbOnly, "Eaton Corridor — I-210 WB + SR-134 WB")

end testEatonCorridorCoords


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test the CSV-to-Model corridor layout builder.
 *  Builds a `CorridorLayout` for I-210 Westbound and prints the full summary
 *  including junction positions, ramp→segment mappings, and segment lengths.
 *
 *  > runMain scalation.simulation.process.config.testBuildCorridorLayout
 */
@main def testBuildCorridorLayout (): Unit =


    banner ("CorridorLayout Builder: I-210 Westbound")

    // Build the layout — this runs the full CSV-to-Model pipeline
    val layout = EatonCorridorConfig.I210_WB
    layout.summary ()

    // Verify segment lengths
    banner ("Segment Lengths")
    cfor (0, layout.segmentLengths.dim) { i =>
        val name0 = layout.junctionNames(i)
        val name1 = layout.junctionNames(i + 1)
        println (f"  seg $i%3d: $name0%-28s → $name1%-28s  ${layout.segmentLengths(i)}%8.1f m  (${layout.segmentLengths(i) / 1609.34}%.3f mi)")
    }

    // Verify VSource center and offsets
    banner ("VSource Positions")
    val (center, offsets) = layout.getVSourceCenterAndOffsets ()
    println (s"  Center: $center")
    cfor (0, offsets.length) { i =>
        val label = if i == 0 then "mainline" else s"onRamp$i"
        println (f"  $label%-12s  offset = ${offsets(i)}")
    }

    // Verify NetworkConfig compatibility
    banner ("NetworkConfig")
    val nc = layout.config
    println (s"  mainline.id         = ${nc.mainline.id}")
    println (s"  mainline.segments   = ${nc.mainline.segments}")
    println (s"  mainline.lanes      = ${nc.mainline.lanesPerSegment}")
    println (s"  ramps (total)       = ${nc.ramps.length}")
    println (s"  ramps (On)          = ${nc.ramps.count (_.mode == RampMode.On)}")
    println (s"  ramps (Off)         = ${nc.ramps.count (_.mode == RampMode.Off)}")
    println (s"  sensors             = ${nc.sensors.length}")
    println (s"  numJunctions        = ${nc.numJunctions}")
    println (s"  pemsSensorIndices   = ${nc.pemsSensorIndices.mkString (", ")}")

    banner ("SR-134 Westbound (quick check)")
    val sr134 = EatonCorridorConfig.SR134_WB
    sr134.summary ()

end testBuildCorridorLayout
