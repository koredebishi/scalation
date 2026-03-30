//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Mar 23 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    FFConnector: Freeway-to-Freeway Connector Between Two Corridors
 *
 *  Models a physical connector ramp linking two freeway corridors at an
 *  interchange (e.g., I-210 WB → SR-134 WB at the Pasadena interchange).
 *
 *  Unlike a Ramp (which connects a VSource/Sink to the mainline), an
 *  FFConnector transfers vehicles **between two Routes** on different
 *  corridors.  It has:
 *    - A single VTransport lane (the connector road between the freeways)
 *    - A DLL for car-following on the connector
 *    - A split ratio governing what fraction of traffic diverts
 *
 *  The model's Car entity is responsible for:
 *    1. Checking the split ratio at the diverge segment on corridor A
 *    2. Removing the car from corridor A's pathway
 *    3. Driving the FFConnector lane
 *    4. Adding the car to corridor B's pathway at the merge segment
 */

package scalation
package simulation
package process

import scalation.mathstat.VectorD
import scalation.scala2d.Colors.*
import scalation.animation.CommandType.CreateEdge

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FFConnector` class defines a freeway-to-freeway connector as a single
 *  VTransport segment linking two junctions on different corridors.
 *
 *  @param name        the name of the connector (e.g., "FF_I210_to_SR134")
 *  @param fromJunc    the junction on the source corridor (diverge point)
 *  @param toJunc      the junction on the destination corridor (merge point)
 *  @param motion      the car-following dynamics model
 *  @param splitRatio  the probability that a vehicle takes this connector (0.0–1.0)
 *  @param isSpeed     whether speed or trip-time is used for motion
 *  @param bend        curvature of the connector for animation
 *  @param laneShift   perpendicular offset for visual separation of parallel FF lanes
 */
class FFConnector (name: String, val fromJunc: Component, val toJunc: Component,
                   motion: Dynamics, var splitRatio: Double = 0.30,
                   isSpeed: Boolean = false, bend: Double = 0.15,
                   laneShift: VectorD = VectorD (0, 0))
    extends Component:

    private val debug = debugf ("FFConnector", true)
    debug ("init", s"FFConnector [$name]: ${fromJunc.name} → ${toJunc.name}, splitRatio=$splitRatio, shift=$laneShift")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The connector's single VTransport lane.
     *  Uses laneShift to offset both endpoints for visual separation.
     */
    val lane = new VTransport (s"$name", fromJunc, toJunc, motion, isSpeed, bend,
                               laneShift, laneShift, 0)

    subpart += lane
    initComponent (name, Array ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Doubly linked list to maintain vehicles on this connector.
     */
    val vList = DoublyLinkedList [Vehicle]

    // Enhanced DLL identification for debugging
    val dllId = s"DLL_${name}_FF"
    private def logDLLOperation (operation: String, vehicle: Vehicle, details: String = ""): Unit =
        debug (s"$operation", s"[$dllId] Vehicle ${vehicle.name} $details | DLL size: ${vList.size}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a vehicle to the connector's vehicle list.
     *  @param actor  the vehicle to add
     *  @param other  the vehicle to follow (null if none)
     */
    def addToAlist (actor: Vehicle, other: Vehicle): Unit =
        val otherNode = if other != null then other.myPathNode.asInstanceOf [vList.Node] else null
        actor.myRamp = null                     // not on a ramp
        actor.myPathway = null                  // not on a pathway
        actor.myFFConnector = this                   // on this FFConnector
        actor.myPathNode = vList.add (actor, otherNode)
        actor.pathInfo = dllId
    end addToAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove a vehicle from the connector's vehicle list.
     *  @param actor  the vehicle to remove
     */
    def removeFromAlist (actor: Vehicle): Unit =
        vList.remove (actor.myPathNode.asInstanceOf [vList.Node])
        actor.myPathNode = null
        actor.myFFConnector = null                // no longer on this FFConnector
    end removeFromAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the first vehicle on this connector.
     */
    def getFirst: Vehicle = if vList.isEmpty then null else vList.head

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the last vehicle on this connector.
     */
    def getLast: Vehicle = if vList.isEmpty then null else vList.last

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the location of this connector for display.
     */
    override def at: Array [Double] = lane.at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display this connector visually using animation.
     */
    def display (): Unit =
        director.animate (lane, CreateEdge, orange, lane.curve, fromJunc, toJunc,
            Array (lane.p1(0), lane.p1(1), lane.pc(0), lane.pc(1), lane.p2(0), lane.p2(1)))
    end display

end FFConnector

