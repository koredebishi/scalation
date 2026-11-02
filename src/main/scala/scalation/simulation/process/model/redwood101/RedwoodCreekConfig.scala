
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Oct 25 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Redwood Creek (US-101 N) corridor configuration: topology, demand,
 *           screen coordinates and off-ramp exit fraction.  Copied from the
 *           Donald Doyle pieces of `NetworkConfig` / `PeMSDemand` and modified
 *           for 5 lanes, 2 on-ramps and 1 off-ramp.
 */

package scalation
package simulation
package process
package model
package redwood101

import scalation.mathstat.VectorD
import scalation.random.Erlang2S
import scalation.simulation.process.config.{MainlineSpec, NetworkConfig, PeMSArrivals, PeMSDataHelper,
                                            PeMSDataLoader, PeMSDemand, RampMode, RampSpec, SensorSpec}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Redwood Creek constants.  Configuration only -- no simulation logic.
 *
 *  Layout (order confirmed by PeMS flow balance: S2 - FR = S3, S3 + OR1 = S4, S4 + OR2 = S5):
 *  warm_up -> sensor1 -> sensor2 -> offR_merge -> sensor3 -> onR_merge1 -> sensor4 -> onR_merge2 -> sensor5
 *                                        |                        |                        |
 *                                  offramp 410094            onramp 410095            onramp 410093
 *
 *  Junction indices: 0 warm_up, 1 sensor1, 2 sensor2, 3 offR_merge, 4 sensor3,
 *                    5 onR_merge1, 6 sensor4, 7 onR_merge2, 8 sensor5
 */
object RedwoodCreekConfig:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // GPS (from example_1/Roadcood.scala latlong, the map the earlier Redwood model used)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val sensor1_gps = (37.491812, -122.219838)                       // VDS 404531
    private val sensor2_gps = (37.493958158096504, -122.22470162714667)      // VDS 404532
    private val offR_gps    = (37.49547, -122.22961)                         // off-ramp 410094 diverge
    private val sensor3_gps = (37.49611371865748, -122.23194509331292)       // VDS 401834
    private val onR1_gps    = (37.499945980000035, -122.238211603498155)     // on-ramp 410095 merge
    private val sensor4_gps = (37.50377824134259, -122.24447811368339)       // VDS 401833
    private val onR2_gps    = (37.50533368033694, -122.24635858526254)       // on-ramp 410093 merge
    private val sensor5_gps = (37.510000, -122.252000)                       // VDS 401929 (rounded in Roadcood)

    private val WARMUP_DEG = 0.00743                       // Donald Doyle warm_up -> sensor1 distance (degrees)

    /** Point `dist` degrees upstream of p, along the q -> p heading. */
    private def upstream (p: (Double, Double), q: (Double, Double), dist: Double): (Double, Double) =
        val (dx, dy) = (p._1 - q._1, p._2 - q._2)
        val hyp = math.hypot (dx, dy)
        (p._1 + dx / hyp * dist, p._2 + dy / hyp * dist)
    end upstream

    private val warmup_gps = upstream (sensor1_gps, sensor2_gps, WARMUP_DEG)

    private val mainlineGPS = Array (warmup_gps, sensor1_gps, sensor2_gps, offR_gps, sensor3_gps,
                                     onR1_gps, sensor4_gps, onR2_gps, sensor5_gps)
    private val rampGPS     = Array (onR1_gps, onR2_gps)                    // on-ramps, in network.ramps order

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Topology
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    val network: NetworkConfig = NetworkConfig (
        mainline = MainlineSpec (
            id = "US-101-N-RedwoodCreek",
            segments = 8,
            lanesPerSegment = 5
        ),
        ramps = List (
            RampSpec ("onramp1", joinSegment = 5, mode = RampMode.On),    // VDS 410095
            RampSpec ("onramp2", joinSegment = 7, mode = RampMode.On),    // VDS 410093
            RampSpec ("offramp1", joinSegment = 3, mode = RampMode.Off)   // VDS 410094
        ),
        sensors = List (
            SensorSpec ("sensor1", segment = 1),   // VDS 404531
            SensorSpec ("sensor2", segment = 2),   // VDS 404532
            SensorSpec ("sensor3", segment = 4),   // VDS 401834
            SensorSpec ("sensor4", segment = 6),   // VDS 401833
            SensorSpec ("sensor5", segment = 8)    // VDS 401929
        )
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Demand: mainline + the two on-ramps (the off-ramp is not a source)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def demand (tau: Double = 0.6): PeMSDemand = PeMSDemand (
        mainline = PeMSArrivals ("1-404531ML.csv", Erlang2S (tau), perLane = true),
        ramps = List (
            PeMSArrivals ("1-410095OR.csv", Erlang2S (4.0), perLane = false),
            PeMSArrivals ("2-410093OR.csv", Erlang2S (4.0), perLane = false)),
        dataDir = "Mainline_VDS_Redwood_Creek_US101-N",
        layout  = PeMSDataLoader.I210_MainlineLayout)       // time, total, avg speed, 5 lane flow/speed pairs

    val offRampFile = "Ramps_VDS_Redwood_Creek_US101-N/1-410094FR.csv"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Off-ramp exit fraction per 15-min row: p(k) = FR(k) / S2(k), where S2 is
     *  the last mainline sensor before the diverge.
     *  @param dem  the demand (gives S2's file, window and layout)
     */
    def exitFraction (dem: PeMSDemand): VectorD =
        val fr = PeMSDataLoader.loadRampSensor (offRampFile, dem.window)(?, 0)
        val s2 = PeMSDataHelper.loadMainlineSensor (dem, 1, dem.window, dem.layout)._1.sumVr
        val n  = s2.dim
        new VectorD (n, Array.tabulate (n) { k => if s2(k) > 0.0 then fr(k) / s2(k) else 0.0 })
    end exitFraction

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Screen coordinates (copied from NetworkConfig, modified for 5 lanes + off-ramp)
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

    /** Mainline junction screen coordinates.
     *  @param dims  screen dimensions (width, height)
     */
    def getMainlineCoordinates (dims: (Double, Double)): Array [(Double, Double)] =
        val coords = new scalation.Coordinates (dims._1, dims._2, mainlineGPS)
        coords.aniCoords.map (a => (a(0), a(1)))
    end getMainlineCoordinates

    /** Point on the outer (rightmost) lane at mainline junction m -- Route's shift for lane numLanes-1.
     *  @param dims  screen dimensions (width, height)
     *  @param m     the mainline junction index
     */
    private def outerLaneAt (dims: (Double, Double), m: Int): (Double, Double) =
        val main     = getMainlineCoordinates (dims)
        val (ux, uy) = unit (main.head, main.last)                         // corridor direction, as Route.calcShift2
        val outer    = (network.mainline.lanesPerSegment - 1) / 2.0 * LANE_GAP
        (main(m)._1 - uy * outer, main(m)._2 + ux * outer)
    end outerLaneAt

    /** On-ramp end points: on the outer lane at each merge junction.
     *  @param dims  screen dimensions (width, height)
     */
    def getRampCoordinates (dims: (Double, Double)): Array [(Double, Double)] =
        rampGPS.map (gps => outerLaneAt (dims, mainlineGPS.indexOf (gps)))
    end getRampCoordinates

    /** VSource center position and offsets (offsets(0) mainline, then one per on-ramp).
     *  @param dims  screen dimensions (width, height)
     */
    def getVSourceCenterAndOffsets (dims: (Double, Double)): ((Int, Int), Array [(Int, Int)]) =
        val main  = getMainlineCoordinates (dims)
        val ramps = getRampCoordinates (dims)
        val centerPos = ((main(0)._1 - 50.0).toInt, (main(0)._2 + 50.0).toInt)
        val offsets = new Array [(Int, Int)] (1 + ramps.length)
        offsets(0) = (0, 0)
        for r <- ramps.indices do
            val m        = mainlineGPS.indexOf (rampGPS(r))                // merge junction index
            val (lx, ly) = unit (main(m - 1), main(m))                     // road direction where this ramp joins
            val back     = RAMP_LEN * math.cos (RAMP_ANGLE)                // upstream along the road
            val out      = RAMP_LEN * math.sin (RAMP_ANGLE)                // out to the right of travel
            val sourceX  = ramps(r)._1 - back * lx - out * ly
            val sourceY  = ramps(r)._2 - back * ly + out * lx
            offsets(r + 1) = ((sourceX - centerPos._1).toInt,
                              (sourceY - centerPos._2).toInt)
        end for
        (centerPos, offsets)
    end getVSourceCenterAndOffsets

    /** Off-ramp start point: on the outer lane at the diverge junction (as the on-ramps end).
     *  @param dims  screen dimensions (width, height)
     */
    def getOffRampStart (dims: (Double, Double)): (Double, Double) =
        outerLaneAt (dims, mainlineGPS.indexOf (offR_gps))
    end getOffRampStart

    /** Off-ramp end point (where its sink sits): downstream and to the right of the diverge.
     *  @param dims  screen dimensions (width, height)
     */
    def getOffRampEnd (dims: (Double, Double)): (Int, Int) =
        val main     = getMainlineCoordinates (dims)
        val m        = mainlineGPS.indexOf (offR_gps)                      // diverge junction index
        val (ox, oy) = outerLaneAt (dims, m)
        val (lx, ly) = unit (main(m), main(m + 1))                         // road direction leaving the diverge
        val fwd      = RAMP_LEN * math.cos (RAMP_ANGLE)                    // downstream along the road
        val out      = RAMP_LEN * math.sin (RAMP_ANGLE)                    // out to the right of travel
        ((ox + fwd * lx - out * ly).toInt, (oy + fwd * ly + out * lx).toInt)
    end getOffRampEnd

end RedwoodCreekConfig
