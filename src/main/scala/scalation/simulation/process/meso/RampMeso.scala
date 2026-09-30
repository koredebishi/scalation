package scalation
package simulation
package process
package meso

import scalation.dynamics.IdmFlux
import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RampMesoMode` enum defines the ramp direction.
 */
enum RampMesoMode:
    case On, Off
end RampMesoMode

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RampMeso` class defines an on-ramp or off-ramp with one mesoscopic link.
 *  The link handles travel and discharge capacity; merging is handled separately.
 *  @param name           the name of the ramp
 *  @param from           the starting component
 *  @param to             the ending component
 *  @param length         physical ramp length (metres)
 *  @param freeFlowSpeed  free-flow speed (metres/second)
 *  @param capacity       discharge capacity (vehicles/hour for this lane)
 *  @param mode           on-ramp or off-ramp
 *  @param bend           curvature of the displayed ramp
 *  @param offset         lateral display offset
 *  @param storageCapacity maximum admitted vehicles on the ramp link
 *  @param flowDiagram    optional density-sensitive equilibrium speed relation for the ramp link;
 *                        when absent, the link keeps the fixed `freeFlowSpeed`
 */
class RampMeso (name: String, val from: Component, val to: Component,
                length: Double, freeFlowSpeed: Double, capacity: Double, val mode: RampMesoMode,
                bend: Double = 0.10, offset: Double = 0.0,
                storageCapacity: Int = Int.MaxValue,
                flowDiagram: Option [IdmFlux] = None)
    extends Component:

    private val shift = calculateShift ()

    val lane = new MesoVTransport (name, from, to, freeFlowSpeed, capacity,
                                  bend, shift, shift, storageCapacity, flowDiagram)
    if length > 0.0 then lane.length = length          // else keep the drawing length

    subpart += lane
    initComponent (name, at)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the lateral shift for displaying this ramp.
     */
    private def calculateShift (): VectorD =
        val dx   = to.at(0) - from.at(0)
        val dy   = to.at(1) - from.at(1)
        val mag  = math.hypot (dx, dy).max (1E-9)
        val nx   = dy / mag
        val ny   = -dx / mag
        val sign = if mode == RampMesoMode.On then 1.0 else -1.0
        VectorD (nx * offset * sign, ny * offset * sign)
    end calculateShift

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the link's starting point as the ramp location.
     */
    override def at: Array [Double] =
        val xy = lane.at
        Array (xy(0), xy(1), 0.0, 0.0)                   // add width and height
    end at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display this ramp's link.
     */
    override def display (): Unit = lane.display ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if this is an on-ramp.
     */
    inline def isOn: Boolean = mode == RampMesoMode.On

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if this is an off-ramp.
     */
    inline def isOff: Boolean = mode == RampMesoMode.Off

end RampMeso

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RampMeso` companion object provides a group method for creating ramps.
 */
object RampMeso:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of ramps with shared travel parameters.
     *  @param length         physical length of each ramp (metres)
     *  @param freeFlowSpeed  free-flow speed for all ramps (metres/second)
     *  @param capacity       discharge capacity for each ramp (vehicles/hour)
     *  @param RampMesoInfo   ramp name, endpoints, mode, bend and offset
     */
    def group (length: Double, freeFlowSpeed: Double, capacity: Double,
               RampMesoInfo: (String, Component, Component, RampMesoMode, Double, Double)*): Array [RampMeso] =
        RampMesoInfo.map { case (name, from, to, mode, bend, offset) =>
            new RampMeso (name, from, to, length, freeFlowSpeed, capacity, mode, bend, offset)
        }.toArray
    end group

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of finite-storage ramps with shared travel parameters.
     *  @param length           physical length of each ramp (metres)
     *  @param freeFlowSpeed    free-flow speed for all ramps (metres/second)
     *  @param capacity         discharge capacity for each ramp (vehicles/hour)
     *  @param storageCapacity  maximum admitted vehicles on each ramp
     *  @param RampMesoInfo     ramp name, endpoints, mode, bend and offset
     */
    def group (length: Double, freeFlowSpeed: Double, capacity: Double, storageCapacity: Int,
               RampMesoInfo: (String, Component, Component, RampMesoMode, Double, Double)*): Array [RampMeso] =
        RampMesoInfo.map { case (name, from, to, mode, bend, offset) =>
            new RampMeso (name, from, to, length, freeFlowSpeed, capacity, mode,
                          bend, offset, storageCapacity)
        }.toArray
    end group

end RampMeso
