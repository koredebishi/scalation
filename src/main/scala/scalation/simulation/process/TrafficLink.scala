
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Korede Bishi
 *  @version 2.0
 *  @date    Mon Oct 5 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Traffic State Shared by Every Road Link (micro and meso)
 */

package scalation
package simulation
package process

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrafficLink` trait is the traffic state every road link owns, whatever
 *  way it moves vehicles (`VTransport` step by step, `MesoVTransport` by FIFO).
 *  One link is one lane x one section, so every quantity here is PER LANE
 *  (see context/traffic-state-spec.md).
 *  A link supplies only what is truly its own: how many vehicles it holds and
 *  how its space-mean speed is defined.  Length, density and flow are the same
 *  for every link and live here once.  Readers (e.g., the animation HUD) ask any
 *  `TrafficLink` without knowing which kind it is.
 */
trait TrafficLink:
    self: Transport =>

    /** Physical road length (metres); builder/config may set it, else the drawn length.
     */
    var length: Double = curve.length

    /** Number of vehicles on this link right now.
     */
    def vehicleCount: Int

    /** Space-mean speed on this link (m/s); stopped vehicles count as 0.
     */
    def spaceMeanSpeed: Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Instantaneous traffic density on this link.
     *      k = n / L
     *  where n is the number of vehicles on the link right now and L its length.
     *  @return density in veh/km per lane
     */
    def density: Double =
        val lenKm = length / 1000.0                        // metres -> km
        if lenKm > 0.0 then vehicleCount.toDouble / lenKm else 0.0
    end density

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Modelled traffic flow on this link, from the fundamental relation
     *      q = k * v_s
     *  The 3.6 converts veh/km * m/s to veh/h.
     *  @return flow in veh/h per lane
     */
    def flow: Double = density * spaceMeanSpeed * 3.6

end TrafficLink

