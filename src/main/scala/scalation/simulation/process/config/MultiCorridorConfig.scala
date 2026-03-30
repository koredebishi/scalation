
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Mon Mar 24 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Multi-Corridor Network Configuration (Top-Level)
 *
 *  The master configuration that defines a complete freeway network:
 *  one or more corridors, their FF interconnections, dynamics, and demand flags.
 *  A single declarative object from which a builder can construct the entire
 *  simulation model.
 *
 *  @see config-layer-standard.md Section 3b, 8 step 3
 */

package scalation
package simulation
package process
package config

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CorridorEntry` class pairs a corridor layout with its demand flag.
 *  @param id          the corridor identifier (e.g., "I-210-W")
 *  @param layout      the corridor layout (topology + screen coordinates)
 *  @param demandFlag  whether this corridor has independent VSources or is fed by FF
 */
case class CorridorEntry (id: String, layout: CorridorLayout,
                          demandFlag: DemandFlag = DemandFlag.Independent)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MultiCorridorConfig` class defines a complete freeway network.
 *  Pure data class — no simulation logic.
 *
 *  @param corridors      the list of corridor entries (id + layout + demand flag)
 *  @param interchanges   the list of FF connector specs linking corridors
 *  @param dynamics       the shared vehicle dynamics configuration
 *  @param nt             the number of time intervals (e.g., 48 for 15-min over 12 hours)
 *  @param rowTime        the duration of each time interval in seconds (e.g., 900.0 for 15-min)
 */
case class MultiCorridorConfig (corridors: List [CorridorEntry],
                                interchanges: List [FFConnectorSpec],
                                dynamics: DynamicsConfig = DynamicsConfig.idmDefault,
                                nt: Int = 48,
                                rowTime: Double = 900.0):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of corridors in this network.
     */
    def numCorridors: Int = corridors.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of FF interchanges in this network.
     */
    def numInterchanges: Int = interchanges.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Look up a corridor entry by its ID string.
     *  @param id  the corridor identifier
     */
    def corridor (id: String): CorridorEntry =
        corridors.find (_.id == id).getOrElse (
            throw new IllegalArgumentException (s"MultiCorridorConfig: corridor '$id' not found")
        )
    end corridor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the corridor IDs that have independent mainline demand.
     */
    def independentCorridors: List [String] =
        corridors.filter (_.demandFlag == DemandFlag.Independent).map (_.id)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the corridor IDs that receive derived demand via FF.
     */
    def derivedCorridors: List [String] =
        corridors.filter (_.demandFlag == DemandFlag.Derived).map (_.id)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print a summary of this multi-corridor network.
     */
    def summary (): Unit =
        println ("\n" + "=" * 70)
        println ("MULTI-CORRIDOR NETWORK CONFIGURATION")
        println ("=" * 70)
        println (s"Corridors:       ${corridors.length}")
        for c <- corridors do
            val dir = c.layout.config.mainline.direction
            val segs = c.layout.config.mainline.segments
            val lanes = c.layout.config.mainline.lanesPerSegment
            println (f"  ${c.id}%-20s  ${dir}%-12s  segs=$segs%3d  lanes=$lanes%2d  demand=${c.demandFlag}")
        end for
        println (s"Interchanges:    ${interchanges.length}")
        for ff <- interchanges do
            println (f"  ${ff.id}%-25s  ${ff.fromCorridorId} → ${ff.toCorridorId}  " +
                     f"split=${ff.splitRatio}%.2f  lanes=${ff.lanes}  PeMS=${ff.stationId}")
        end for
        println (s"Dynamics:        ${dynamics.carFollowing} / ${dynamics.odeSolver}")
        println (s"Time intervals:  $nt × ${rowTime.toInt}s = ${(nt * rowTime / 3600.0).toInt}h")
        println ("=" * 70)
    end summary

end MultiCorridorConfig


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MultiCorridorConfig` companion object provides predefined network
 *  configurations (factory methods).
 *  Each method returns a complete `MultiCorridorConfig` instance for a specific real-world network.
 *  These can be used directly or as templates for further customization.
 *  
 */
object MultiCorridorConfig:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** US-101 Northbound, Donald Doyle — single corridor (no FF).
     *  Wraps the existing NetworkConfig.US101_DonaldDoyle in the multi-corridor
     *  format for compatibility with the builder standard.
     *  @param dynamics  the dynamics configuration (default: IDM)
     */
    def US101_DonaldDoyle (dynamics: DynamicsConfig = DynamicsConfig.idmDefault): MultiCorridorConfig =
        // US-101 uses NetworkConfig directly (no CorridorLayout from CSV).
        // Create a minimal CorridorLayout from the existing NetworkConfig.
        val net = NetworkConfig.US101_DonaldDoyle
        val nJunc = net.numJunctions
        val dummyNames = Array.ofDim [String] (nJunc)
        cfor (0, nJunc) { i => dummyNames(i) = s"junc_$i" }
        val dummyXY = Array.ofDim [(Double, Double)] (nJunc)
        cfor (0, nJunc) { i => dummyXY(i) = (0.0, 0.0) }
        val layout = CorridorLayout (
            config           = net,
            junctionNames    = dummyNames,
            mainlineScreenXY = dummyXY,
            onRampScreenXY   = Array.ofDim [(Double, Double)] (net.ramps.count (_.mode == RampMode.On)),
            offRampScreenXY  = Array.ofDim [(Double, Double)] (net.ramps.count (_.mode == RampMode.Off)),
            segmentLengths   = net.mainline.segmentLengths.getOrElse (
                                   scalation.mathstat.VectorD.fill (net.mainline.segments)(500.0)),
            ffStations       = Array.empty [StationRecord]
        )
        MultiCorridorConfig (
            corridors    = List (CorridorEntry ("US-101-N", layout, DemandFlag.Independent)),
            interchanges = List.empty,
            dynamics     = dynamics
        )
    end US101_DonaldDoyle

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Eaton Fire WB — dual corridor: I-210 WB + SR-134 WB with FF interchange.
     *  I-210 has independent mainline demand.
     *  SR-134 has derived mainline demand (fed by FF from I-210 at Pasadena interchange).
     *  @param dynamics    the dynamics configuration (default: IDM)
     *  @param splitRatio  the FF split ratio at the I-210 → SR-134 interchange
     */
    def EatonFire_WB (dynamics: DynamicsConfig = DynamicsConfig.idmDefault,
                      splitRatio: Double = 0.30): MultiCorridorConfig =
        val layout210 = EatonCorridorConfig.I210_WB
        val layout134 = EatonCorridorConfig.SR134_WB
        MultiCorridorConfig (
            corridors = List (
                CorridorEntry ("I-210-W",  layout210, DemandFlag.Independent),
                CorridorEntry ("SR-134-W", layout134, DemandFlag.Derived)     
            ),
            interchanges = List (
                FFConnectorSpec (
                    id              = "FF_I210_to_SR134",
                    fromCorridorId  = "I-210-W",
                    toCorridorId    = "SR-134-W",
                    fromJunction    = "WINONA",             // I-210 diverge at PM 24.442
                    toJunction      = "ORANGE",             // SR-134 merge at PM 12.763
                    splitRatio      = splitRatio,
                    lanes           = 2,                    // PeMS station 775725: 2-lane connector
                    stationId       = 775725
                )
            ),
            dynamics = dynamics
        )
    end EatonFire_WB

end MultiCorridorConfig


