
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Fri Jan 31 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Network Configuration for Traffic Simulation (Topology Only)
 */

package scalation
package simulation
package process
package config

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Enumeration for ramp modes: On (merge onto mainline) or Off (diverge from mainline).
 */
enum RampMode:
    case On, Off
end RampMode

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MainlineSpec` class specifies the mainline road segment properties.
 *  @param id                the identifier for this mainline (e.g., "US-101-N")
 *  @param segments          the number of segments (junctions - 1)
 *  @param lanesPerSegment   the number of lanes per segment
 *  @param segmentLengths    optional segment lengths in meters
 */
case class MainlineSpec (id: String, segments: Int, lanesPerSegment: Int,
                         segmentLengths: Option [VectorD] = None)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RampSpec` class specifies an on-ramp or off-ramp.
 *  @param id           the identifier for this ramp (e.g., "onramp1")
 *  @param joinSegment  the segment index where this ramp merges/diverges
 *  @param mode         On (merge) or Off (diverge)
 *  @param lanes        the number of lanes on this ramp
 */
case class RampSpec (id: String, joinSegment: Int, mode: RampMode, lanes: Int = 1)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SensorSpec` class specifies a sensor location for data collection.
 *  @param id        the identifier for this sensor (e.g., "sensor1", matches PeMS VDS ID)
 *  @param segment   the segment index where this sensor is located
 *  @param position  position within segment (0.0 = start, 1.0 = end)
 */
case class SensorSpec (id: String, segment: Int, position: Double = 1.0)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NetworkConfig` class contains topology for a traffic corridor.
 *  Pure data class — no simulation logic.
 *  @param mainline  the mainline road specification
 *  @param ramps     list of ramp specifications
 *  @param sensors   list of sensor specifications
 */
case class NetworkConfig (mainline: MainlineSpec, ramps: List [RampSpec],
                          sensors: List [SensorSpec]):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the total number of junctions (segments + 1).
     */
    def numJunctions: Int = mainline.segments + 1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return sensor indices that correspond to PeMS comparison points.
     *  Excludes merge points (which are operational only).
     */
    def pemsSensorIndices: Array [Int] = sensors.map (_.segment).toArray

end NetworkConfig


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NetworkConfig` companion object provides predefined network configurations.
 */
object NetworkConfig:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // US-101 Donald Doyle GPS coordinates (from Roadcood2)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private def interpolate (p1: (Double, Double), p2: (Double, Double), f: Double): (Double, Double) =
        (p1._1 + (p2._1 - p1._1) * f, p1._2 + (p2._2 - p1._2) * f)

    private val warmup_gps  = (37.826729, -121.999645)
    private val sensor1_gps = (37.832229, -122.004645)
    private val sensor2_gps = (37.833874, -122.007206)
    private val sensor3_gps = (37.835529, -122.009979)
    private val sensor4_gps = (37.838067, -122.014224)
    private val sensor5_gps = (37.839933, -122.017269)
    private val merge1_gps  = interpolate (sensor1_gps, sensor2_gps, 0.75)
    private val merge2_gps  = interpolate (sensor3_gps, sensor4_gps, 0.75)

    private val mainlineGPS = Array (warmup_gps, sensor1_gps, merge1_gps, sensor2_gps,
                                     sensor3_gps, merge2_gps, sensor4_gps, sensor5_gps)
    private val rampGPS     = Array (merge1_gps, merge2_gps)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert GPS to screen coordinates for animation.
     *  @param dims  screen dimensions (width, height)
     */
    def getMainlineCoordinates (dims: (Double, Double)): Array [(Double, Double)] =
        val coords = new scalation.Coordinates (dims._1, dims._2, mainlineGPS)
        coords.aniCoords.map (a => (a(0), a(1))).toArray
    end getMainlineCoordinates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get ramp coordinates for animation (with shift offset).
     *  @param dims  screen dimensions (width, height)
     */
    def getRampCoordinates (dims: (Double, Double)): Array [(Double, Double)] =
        val coords = new scalation.Coordinates (dims._1, dims._2, rampGPS)
        val (sx, sy) = (65.0, -70.0)
        coords.aniCoords.map (a => (a(0) + sx, a(1) + sy)).toArray
    end getRampCoordinates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get VSource center position and offsets for animation.
     *  @param dims  screen dimensions (width, height)
     */
    def getVSourceCenterAndOffsets (dims: (Double, Double)): ((Int, Int), Array [(Int, Int)]) =
        val main  = getMainlineCoordinates (dims)
        val ramps = getRampCoordinates (dims)
        val centerPos = ((main(0)._1 - 50.0).toInt, (main(0)._2 + 50.0).toInt)
        val (dx, dy) = (800.0, -350.0)
        val offsets = Array (
            (0, 0),
            ((ramps(0)._1 + dx).toInt - centerPos._1, (ramps(0)._2 + dy).toInt - centerPos._2),
            ((ramps(1)._1 + dx).toInt - centerPos._1, (ramps(1)._2 + dy).toInt - centerPos._2)
        )
        (centerPos, offsets)
    end getVSourceCenterAndOffsets

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** US-101 Northbound, Donald Doyle corridor.
     *  Layout: warm_up -> sensor1 -> onR_merge1 -> sensor2 -> sensor3 -> onR_merge2 -> sensor4 -> sensor5
     *
     *  Segment indices (0-based):
     *    0: warm_up -> sensor1
     *    1: sensor1 -> onR_merge1
     *    2: onR_merge1 -> sensor2
     *    3: sensor2 -> sensor3
     *    4: sensor3 -> onR_merge2
     *    5: onR_merge2 -> sensor4
     *    6: sensor4 -> sensor5
     *
     *  Junction indices (0-based):
     *    0: warm_up
     *    1: sensor1
     *    2: onR_merge1
     *    3: sensor2
     *    4: sensor3
     *    5: onR_merge2
     *    6: sensor4
     *    7: sensor5
     */
    val US101_DonaldDoyle: NetworkConfig = NetworkConfig (
        mainline = MainlineSpec (
            id = "US-101-N-DonaldDoyle",
            segments = 7,
            lanesPerSegment = 4
        ),
        ramps = List (
            RampSpec ("onramp1", joinSegment = 2, mode = RampMode.On),
            RampSpec ("onramp2", joinSegment = 5, mode = RampMode.On)
        ),
        sensors = List (
            SensorSpec ("sensor1", segment = 1),   // VDS 401112
            SensorSpec ("sensor2", segment = 3),   // VDS 401104
            SensorSpec ("sensor3", segment = 4),   // VDS 400712
            SensorSpec ("sensor4", segment = 6),   // VDS 400450
            SensorSpec ("sensor5", segment = 7)    // VDS 407463
        )
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** US-101 Northbound, Redwood Creek corridor (future).
     *  Placeholder for second corridor.
     */
    val US101_RedwoodCreek: NetworkConfig = NetworkConfig (
        mainline = MainlineSpec (
            id = "US-101-N-RedwoodCreek",
            segments = 5,
            lanesPerSegment = 4
        ),
        ramps = List (
            RampSpec ("onramp1", joinSegment = 1, mode = RampMode.On),
            RampSpec ("offramp1", joinSegment = 3, mode = RampMode.Off)
        ),
        sensors = List (
            SensorSpec ("sensor1", segment = 0),
            SensorSpec ("sensor2", segment = 2),
            SensorSpec ("sensor3", segment = 4),
            SensorSpec ("sensor4", segment = 5)
        )
    )

end NetworkConfig
