
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Mon Nov 9 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    I-210 Westbound, Pasadena (Sierra Madre Villa to Hill) corridor
 *           configuration: topology, demand, real segment lengths, screen
 *           coordinates and off-ramp exit fractions.  Built on the
 *           `RedwoodCreekConfig` pattern for 5 lanes, 3 on-ramps and 3 off-ramps,
 *           5-minute PeMS bins, and one data folder per day.
 */

package scalation
package simulation
package process
package model
package i210w

import scalation.LatLong
import scalation.mathstat.VectorD
import scalation.random.Erlang2S
import scalation.simulation.process.config.{ColumnLayout, MainlineSpec, NetworkConfig, PeMSArrivals,
                                            PeMSDataHelper, PeMSDataLoader, PeMSDemand, RampMode,
                                            RampSpec, SensorSpec, TimeWindow}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** I-210 W Pasadena constants.  Configuration only -- no simulation logic.
 *
 *  Westbound means decreasing postmile (PM).  Detector order along the drive and
 *  the ramp order between detectors follow the PeMS daily flow balance of
 *  2024-12-10 (S1 - SMoff + SMon - SGoff + SGon = 97.7k vs S2 99.0k; S2 vs S3
 *  -1.7%; S3 + ALTon - ALLoff = 98.4k vs S4 94.7k):
 *
 *   warm_up -> S1 -> SMoff -> SMon -> SGoff -> SGon -> S2 -> S3 -> ALTon -> ALLoff -> S4 -> sink
 *             29.17   |        |       28.58    |     28.27  28.03   |       27.64   26.80
 *                  717648   716594   768927  716592              716591  764346
 *
 *  Junction indices: 0 warm_up, 1 sensor1, 2 offR_diverge1, 3 onR_merge1, 4 offR_diverge2,
 *                    5 onR_merge2, 6 sensor2, 7 sensor3, 8 onR_merge3, 9 offR_diverge3, 10 sensor4
 *
 *  Ramp junctions that share a detector's postmile have no GPS of their own in the
 *  station map.  They are placed along the road by distance from that detector:
 *  SM off 120 m and SM on 240 m past S1, SG on 150 m before S2, Altadena on 150 m
 *  past S3.  Placements are documented assumptions, not measurements.
 *
 *  Not modelled: Hill on-ramp 716588 (joins downstream of S4, the last detector),
 *  the HOV lane, and the dead detectors from Lake downstream.
 */
object I210WConfig:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // GPS from data/WSC-Pems-Data-Eaton-Fire/.../eaton-i210/station_map.csv
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val sensor1_gps = (34.148074, -118.083770)      // 717649 Sierra Madre Villa, PM 29.17
    private val sgOff_gps   = (34.152365, -118.092642)      // 768927 San Gabriel off,     PM 28.58
    private val sensor2_gps = (34.152727, -118.098063)      // 717644 San Gabriel,         PM 28.27
    private val sensor3_gps = (34.152700, -118.102255)      // 717642 Altadena,            PM 28.03
    private val allenOff_gps = (34.152648, -118.109062)     // 764346 Allen off,           PM 27.64
    private val sensor4_gps = (34.152357, -118.123714)      // 717637 Hill,                PM 26.80

    /** Metres between two GPS points (ScalaTion `LatLong`, great-circle). */
    def metres (p: (Double, Double), q: (Double, Double)): Double =
        new LatLong (p).distance (new LatLong (q))

    /** The point `d` metres from p towards q, along the straight line between them. */
    private def along (p: (Double, Double), q: (Double, Double), d: Double): (Double, Double) =
        val f = d / metres (p, q)
        (p._1 + f * (q._1 - p._1), p._2 + f * (q._2 - p._2))
    end along

    private val WARMUP_M    = 750.0                         // warm-up length, as the other corridors (about 0.0074 deg)
    private val warmup_gps  = along (sensor1_gps, sgOff_gps, -WARMUP_M)            // upstream of S1
    private val smOff_gps   = along (sensor1_gps, sgOff_gps, 120.0)                 // SM off: 120 m past S1
    private val smOn_gps    = along (sensor1_gps, sgOff_gps, 240.0)                 // SM on:  240 m past S1
    private val sgOn_gps    = along (sensor2_gps, sgOff_gps, 150.0)                 // SG on:  150 m before S2
    private val altOn_gps   = along (sensor3_gps, allenOff_gps, 150.0)              // ALT on: 150 m past S3

    private val mainlineGPS = Array (warmup_gps, sensor1_gps, smOff_gps, smOn_gps, sgOff_gps, sgOn_gps,
                                     sensor2_gps, sensor3_gps, altOn_gps, allenOff_gps, sensor4_gps)
    private val onRampGPS   = Array (smOn_gps, sgOn_gps, altOn_gps)                 // in network.ramps On order
    private val offRampGPS  = Array (smOff_gps, sgOff_gps, allenOff_gps)            // in network.ramps Off order

    /** Real length of each segment in metres (segment s runs from junction s to s + 1). */
    val segmentLengthsM: VectorD =
        val n = mainlineGPS.length - 1
        new VectorD (n, Array.tabulate (n) { s => metres (mainlineGPS(s), mainlineGPS(s + 1)) })

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Topology
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    val network: NetworkConfig = NetworkConfig (
        mainline = MainlineSpec (
            id = "I-210-W-Pasadena",
            segments = 10,
            lanesPerSegment = 5,
            segmentLengths = Some (segmentLengthsM)
        ),
        ramps = List (
            RampSpec ("onramp1",  joinSegment = 3, mode = RampMode.On),    // 716594 Sierra Madre on
            RampSpec ("onramp2",  joinSegment = 5, mode = RampMode.On),    // 716592 San Gabriel on
            RampSpec ("onramp3",  joinSegment = 8, mode = RampMode.On),    // 716591 Altadena on
            RampSpec ("offramp1", joinSegment = 2, mode = RampMode.Off),   // 717648 Sierra Madre off
            RampSpec ("offramp2", joinSegment = 4, mode = RampMode.Off),   // 768927 San Gabriel off
            RampSpec ("offramp3", joinSegment = 9, mode = RampMode.Off)    // 764346 Allen off
        ),
        sensors = List (
            SensorSpec ("sensor1", segment = 1),    // 717649
            SensorSpec ("sensor2", segment = 6),    // 717644
            SensorSpec ("sensor3", segment = 7),    // 717642
            SensorSpec ("sensor4", segment = 10)    // 717637
        )
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Time and data: 5-minute bins, 06:00-23:00, one folder per day
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    val BIN_SECONDS = 300.0
    val N_BINS      = 205
    val window: TimeWindow     = TimeWindow (startRow = 0, endRow = N_BINS, binSeconds = BIN_SECONDS)
    val layout: ColumnLayout   = PeMSDataLoader.I210_MainlineLayout            // time, total, avg, 5 flow/speed pairs

    val DAYS = Array ("2024-12-03", "2024-12-10", "2024-12-17")
    val DEFAULT_DAY = "2024-12-03"                          // calibration day; the other two are held out

    def mainlineDir (day: String): String = s"Mainline_VDS_I210W_Pasadena/$day"
    def rampDir (day: String): String     = s"Ramps_VDS_I210W_Pasadena/$day"

    val mainlineFiles = List ("1-717649ML.csv", "2-717644ML.csv", "3-717642ML.csv", "4-717637ML.csv")
    val onRampFiles   = List ("1-716594OR.csv", "2-716592OR.csv", "3-716591OR.csv")      // network.ramps On order
    val offRampFiles  = List ("1-717648FR.csv", "2-768927FR.csv", "3-764346FR.csv")      // network.ramps Off order

    /** Demand: mainline anchor S1 per lane, plus the three on-ramps.  Off-ramps are not sources.
     *  @param day  the data folder (one of DAYS)
     *  @param tau  the Erlang2S shift for mainline arrivals
     */
    def demand (day: String = DEFAULT_DAY, tau: Double = 0.6): PeMSDemand = PeMSDemand (
        mainline = PeMSArrivals (mainlineFiles.head, Erlang2S (tau), perLane = true),
        ramps    = onRampFiles.map (f => PeMSArrivals (f, Erlang2S (4.0), perLane = false)),
        dataDir  = mainlineDir (day),
        window   = window,
        layout   = layout)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Exit fraction per 5-min row for cars in the outer lane (L5) at each off-ramp,
     *  p_r(k) = FR_r(k) / S_up_L5(k), with S_up the last detector upstream of the
     *  diverge: S1 for the Sierra Madre and San Gabriel off-ramps, S3 for Allen.
     *  On 2024-12-10 the median share is 0.28-0.38 and only one bin of one ramp
     *  exceeds 1 (Sierra Madre, 1.36), so the share is capped at 1.
     *  @param dem  the demand (day folder, window and layout)
     *  @return one VectorD per off-ramp, in network.ramps Off order
     */
    def exitFractions (dem: PeMSDemand): Array [VectorD] =
        val outer    = network.mainline.lanesPerSegment - 1                    // L5 (lane 0 = median side)
        val upstream = Array (0, 0, 2)                                          // sensor index per off-ramp
        Array.tabulate (offRampFiles.length) { r =>
            val fr = PeMSDataLoader.loadRampSensor (s"${rampDir (dem.dataDir.split ('/').last)}/${offRampFiles(r)}",
                                                     dem.window)(?, 0)
            val up = PeMSDataHelper.loadMainlineSensor (dem, upstream(r), dem.window, dem.layout)._1(?, outer)
            new VectorD (up.dim, Array.tabulate (up.dim) { k =>
                if up(k) > 0.0 then math.min (fr(k) / up(k), 1.0) else 0.0 })
        }
    end exitFractions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Screen coordinates (as RedwoodCreekConfig, generalised to 3 on- and 3 off-ramps)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val LANE_GAP   = 50.0                       // Route's default laneGap
    private val RAMP_LEN   = 200.0                      // drawn ramp length
    private val RAMP_ANGLE = math.toRadians (30.0)      // angle between ramp and road

    /** Unit vector from p to q. */
    private def unit (p: (Double, Double), q: (Double, Double)): (Double, Double) =
        val (dx, dy) = (q._1 - p._1, q._2 - p._2)
        val hyp = math.hypot (dx, dy).max (1.0)
        (dx / hyp, dy / hyp)
    end unit

    /** Mainline junction screen coordinates (drawing only; physics uses segmentLengthsM).
     *  @param dims  screen dimensions (width, height)
     */
    def getMainlineCoordinates (dims: (Double, Double)): Array [(Double, Double)] =
        val coords = new scalation.Coordinates (dims._1, dims._2, mainlineGPS)
        coords.aniCoords.map (a => (a(0), a(1)))
    end getMainlineCoordinates

    /** Point on the outer (rightmost) lane at mainline junction m -- Route's shift for lane numLanes-1. */
    private def outerLaneAt (dims: (Double, Double), m: Int): (Double, Double) =
        val main     = getMainlineCoordinates (dims)
        val (ux, uy) = unit (main.head, main.last)                         // corridor direction, as Route.calcShift2
        val outer    = (network.mainline.lanesPerSegment - 1) / 2.0 * LANE_GAP
        (main(m)._1 - uy * outer, main(m)._2 + ux * outer)
    end outerLaneAt

    /** On-ramp end points: on the outer lane at each merge junction. */
    def getRampCoordinates (dims: (Double, Double)): Array [(Double, Double)] =
        onRampGPS.map (gps => outerLaneAt (dims, mainlineGPS.indexOf (gps)))
    end getRampCoordinates

    /** VSource center position and offsets (offsets(0) mainline, then one per on-ramp). */
    def getVSourceCenterAndOffsets (dims: (Double, Double)): ((Int, Int), Array [(Int, Int)]) =
        val main  = getMainlineCoordinates (dims)
        val ramps = getRampCoordinates (dims)
        val centerPos = ((main(0)._1 - 50.0).toInt, (main(0)._2 + 50.0).toInt)
        val offsets = new Array [(Int, Int)] (1 + ramps.length)
        offsets(0) = (0, 0)
        for r <- ramps.indices do
            val m        = mainlineGPS.indexOf (onRampGPS(r))              // merge junction index
            val (lx, ly) = unit (main(m - 1), main(m))                     // road direction where this ramp joins
            val back     = RAMP_LEN * math.cos (RAMP_ANGLE)                // upstream along the road
            val out      = RAMP_LEN * math.sin (RAMP_ANGLE)                // out to the right of travel
            val sourceX  = ramps(r)._1 - back * lx - out * ly
            val sourceY  = ramps(r)._2 - back * ly + out * lx
            offsets(r + 1) = ((sourceX - centerPos._1).toInt, (sourceY - centerPos._2).toInt)
        end for
        (centerPos, offsets)
    end getVSourceCenterAndOffsets

    /** Off-ramp r start point: on the outer lane at its diverge junction. */
    def getOffRampStart (dims: (Double, Double), r: Int): (Double, Double) =
        outerLaneAt (dims, mainlineGPS.indexOf (offRampGPS(r)))
    end getOffRampStart

    /** Off-ramp r end point (where its sink sits): downstream and to the right of the diverge. */
    def getOffRampEnd (dims: (Double, Double), r: Int): (Int, Int) =
        val main     = getMainlineCoordinates (dims)
        val m        = mainlineGPS.indexOf (offRampGPS(r))                 // diverge junction index
        val (ox, oy) = outerLaneAt (dims, m)
        val (lx, ly) = unit (main(m), main(m + 1))                         // road direction leaving the diverge
        val fwd      = RAMP_LEN * math.cos (RAMP_ANGLE)                    // downstream along the road
        val out      = RAMP_LEN * math.sin (RAMP_ANGLE)                    // out to the right of travel
        ((ox + fwd * lx - out * ly).toInt, (oy + fwd * ly + out * lx).toInt)
    end getOffRampEnd

end I210WConfig


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Print the corridor's junction order, real segment lengths and total length.
 *  > runMain scalation.simulation.process.model.i210w.i210wConfigTest
 */
@main def i210wConfigTest (): Unit =
    val names = Array ("warm_up", "sensor1", "offR_diverge1", "onR_merge1", "offR_diverge2", "onR_merge2",
                       "sensor2", "sensor3", "onR_merge3", "offR_diverge3", "sensor4")
    val L = I210WConfig.segmentLengthsM
    println ("I-210 W Pasadena: segment lengths (m)")
    for s <- 0 until L.dim do println (f"  seg $s%2d  ${names(s)}%-14s -> ${names(s + 1)}%-14s ${L(s)}%7.1f m")
    val s1s4 = L(1 until L.dim).sum                                              // sensor1 to sensor4, without the warm-up
    println (f"  total ${L.sum}%.0f m, sensor1 to sensor4 $s1s4%.0f m (${s1s4 / 1609.344}%.2f mi)")
end i210wConfigTest
