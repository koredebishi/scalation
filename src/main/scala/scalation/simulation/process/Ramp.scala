////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

package scalation
package simulation
package process

import scalation.mathstat.VectorD
import scalation.scala2d.Colors.*
import scalation.animation.CommandType.CreateEdge

//import scala.math.{hypot}
/** Enum for ramp directionality. */
enum RampMode:
    case On, Off
end RampMode
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Ramp` class defines an on-ramp or off-ramp as a specialized single-segment `Pathway`.
 *  It inherits from `Pathway` to reuse infrastructure like `vList`, `seg`, `display`, etc.
 *  @param name     the name of the ramp
 *  @param from     the source component (VSource or Trunk)
 *  @param junc     the junction used for merging or diverging
 *  @param to       the destination component (Trunk or Sink)
 *  @param motion   the dynamics model
 *  @param mode     RampMode.On or RampMode.Off
 *  @param offset   lateral spacing from trunk
 *  @param bend     optional curvature
 */
class Ramp(name: String, val from: Component, val to: Component,motion: Dynamics, val mode: RampMode, isSpeed: Boolean = false,
           bend: Double = 0.10, offset: Double = 0.0)
    extends Component:

    private val debug = debugf("Ramp", false)
    debug("init", s"Ramp [$name] direction: ${from.name} → ${to.name} , $mode")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Mainline merge target — set by the model after construction.
     *  Enables dual-leader car-following: ramp vehicles peek at the mainline
     *  DLL at (targetPathway, targetSegId) to see approaching traffic.
     *  @see Treiber & Kesting 2013, §11.3
     */
    var targetPathway: Pathway = null
    var targetSegId  : Int     = -1


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate lateral shift vector for ramp visualization.
     */
    private val shift = calculateShift()

    private def calculateShift(): VectorD =
        val dx = to.at(0) - from.at(0)
        val dy = to.at(1) - from.at(1)
        val mag = math.hypot(dx, dy).max(1e-9)
        val nx = dy / mag
        val ny = -dx / mag
        val sign = if mode == RampMode.On then 1.0 else -1.0
        VectorD(nx * offset * sign, ny * offset * sign)
        //VectorD(nx * offset, ny * offset )
    end calculateShift

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The ramp's single transport lane.
     */
    val lane = new VTransport(s"$name", from, to, motion, isSpeed, bend, shift, shift, 0)

    subpart += lane
    initComponent(name, Array())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Doubly linked list to maintain vehicles on this ramp.
     */
    val vList = DoublyLinkedList[Vehicle]
    
    // Enhanced DLL identification for debugging
    val dllId = s"DLL_${name}_${mode}Ramp"
    private def logDLLOperation(operation: String, vehicle: Vehicle, details: String = ""): Unit =
        debug(s"$operation", s"[$dllId] Vehicle ${vehicle.name} $details | DLL size: ${vList.size}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a vehicle to the ramp's vehicle list.
     *  @param actor the vehicle to add
     *  @param other the vehicle to follow (null if none)
    */
    def addToAlist(actor: Vehicle, other: Vehicle): Unit =
        val otherNode = if other != null then other.myPathNode.asInstanceOf[vList.Node] else null
        actor.myRamp = this
        actor.myPathway = null                  // not on a pathway
        actor.myFFConnector = null                   // not on an FFConnector
        actor.myPathNode = vList.add(actor, otherNode)
        actor.pathInfo = s"${dllId}" // Update path info with clear DLL identifier
    end addToAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove a vehicle from the ramp's vehicle list.
     *  @param actor the vehicle to remove
     */
    def removeFromAlist(actor: Vehicle): Unit =
        vList.remove(actor.myPathNode.asInstanceOf[vList.Node])
        actor.myPathNode = null
        actor.myRamp = null
    end removeFromAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the first vehicle in the ramp.
     */
    def getFirst: Vehicle = if vList.isEmpty then null else vList.head

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the last vehicle in the ramp.
     */
    def getLast: Vehicle = if vList.isEmpty then null else vList.last

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the location of this ramp for display (use transport's starting point).
     */
    override def at: Array[Double] = lane.at


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display this ramp visually using animation.
     */
    def display(): Unit =
        director.animate(lane, CreateEdge, blue, lane.curve, from, to,
            Array(lane.p1(0), lane.p1(1), lane.pc(0), lane.pc(1), lane.p2(0), lane.p2(1)))
    end display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if this is an on-ramp.
     */
    inline def isOn: Boolean = mode == RampMode.On

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if this is an off-ramp.
     */
    inline def isOff: Boolean = mode == RampMode.Off

end Ramp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Ramp` companion object provides a group method for creating multiple ramps. */
object Ramp:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related ramps.
     *  @param motion    the dynamics model for all ramps
     *  @param rampInfo  repeated ramp specific info: name, from, to, mode, bend, offset
     */
    def group(motion: Dynamics, rampInfo: (String, Component, Component, RampMode, Double, Double)*): Array[Ramp] =
        rampInfo.map { case (name, from, to, mode, bend, offset) =>
            new Ramp(name, from, to, motion, mode, false, bend, offset)
        }.toArray
    end group

end Ramp