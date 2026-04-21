//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Tue Feb  4 14:56:34 EST 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Variable Speed Transport is a Pathway between Components
 */

package scalation
package simulation
package process
//import scalation.animation.DgAnimator.VehicleState


//import scala.collection.mutable.ArrayDeque
//import scala.runtime.ScalaRunTime.stringOf

import scalation.animation.CommandType._
//import scalation.database.BpTreeMap
import scalation.mathstat._
import scala.collection.mutable.ArrayDeque

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VTransport` class provides a variable-speed pathway between two other components.
 *  The components in a `Model` conceptually form a 'graph' in which the edges
 *  are `VTransport`s and the nodes are other `Component`s.
 *  @see `animation.Dgraph.move2Boundary` that aligns edge with node boundaries.
 *  @param name      the name of the variable-speed transport
 *  @param from      the starting component
 *  @param to        the ending component
 *  @param motion    the dynamics model for the speed/trip-time for motion down the `VTransport`
 *  @param isSpeed   whether speed or trip-time is used for motion
 *  @param bend      the bend or curvature of the `VTransport` (0 => line)
 *  @param shift1    the x-y shift for the transport's first end-point (from-side)
 *  @param shift2    the x-y shift for the transport's second end-point (to-side)
 */
class VTransport (name: String, from_ : Component, to_ : Component,
                  motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0,
                  shift1: VectorD = VectorD (0, 0), shift2: VectorD = VectorD (0, 0), segIndex: Int = -1)
    extends Transport (name, from_, to_, null, isSpeed, bend, shift1, shift2):

    private val debug = debugf ("VTransport", false)                     // debug function
    //debug ("init", s"name = $name, p1 = $p1, pc = $pc, p2 = $p2, located at ${stringOf (at)}")

    var length = 0.0    //  The actual length of the road segment.
    if length <= 0.0 then length = curve.length
    val safetydist = 20.0
    

    private [process] val vdeque = ArrayDeque [Vehicle] ()               // Array Deque for density tracking (entry order)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Per-segment doubly linked list for car-following.
     *  Each VTransport owns its own DLL — the car-following backbone.
     *  Replaces the lane-spanning Pathway.vList for leader lookup.
     */
    private [process] val vList = DoublyLinkedList [Vehicle]

    /** Debug label for this segment's DLL. */
    val dllId = s"DLL_${name}"

    private[process] val easyW = new EasyWriter("simulation", "OnewayVehicle2LModel.txt")
    easyW.off()


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a vehicle to this segment's doubly linked list.
     *  @param actor  the vehicle to add
     *  @param other  the vehicle ahead (null if none)
     */
    def addToAlist (actor: Vehicle, other: Vehicle): Unit =
        val otherNode = if other != null then other.myPathNode.asInstanceOf [vList.Node]
                        else null
        actor.myPathNode = vList.add (actor, otherNode)
        actor.pathInfo = dllId
    end addToAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove a vehicle from this segment's doubly linked list.
     *  @param actor  the vehicle to remove
     */
    def removeFromAlist (actor: Vehicle): Unit =
        vList.remove (actor.myPathNode.asInstanceOf [vList.Node])
        actor.myPathNode = null
    end removeFromAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the first vehicle in this segment (lead car, head of DLL).
     */
    def getFirst: Vehicle =
        val first: Vehicle = if vList.isEmpty then null else vList.head
        debug ("getFirst", s"the first vehicle = $first")
        first
    end getFirst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the last vehicle in this segment (most recently entered, tail of DLL).
     */
    def getLast: Vehicle =
        val last: Vehicle = if vList.isEmpty then null else vList.last
        debug ("getLast", s"the last vehicle = $last")
        last
    end getLast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the instantaneous traffic density on this segment.
     *  density k = number of vehicles on segment / segment length  (veh/m)
     *  Called from driveHighway after each move() to snapshot the segment state.
     */
    def snapshotDensity (): Double =
        if length > 0.0 then vdeque.size.toDouble / length else 0.0
    end snapshotDensity


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move the entity (SimActor) smoothly down this VTransport (e.g., road).
     *  Repeatedely move it along the VTransport/Edge/QCurve.
     *  Caveat: tokens coordinates are computed using a shadow QCurve (same coordinates
     *  as the one that will be created by the animation engine).
     */

    override def move(): Unit =


        val actor = director.theActor.asInstanceOf[Vehicle]
        val lastSeg        = actor.segId                 // segment before entering this one
        val lastEnterTime  = actor.segmentEnterTime      // time entered previous segment
        val now            = director.clock
        actor.segmentEnterTime = now                      // update the current actor's segmentEnterTime with now


        //actor.prevSegId    = lastSeg
        actor.segId        = segIndex                // update the current actor's segment_ID with
        actor.disp = 0.0
        actor.pathInfo = name     //update the current actor's lane_ID with the Vtransport_ID it is moving in

        //val segTT = if lastSeg >= 0 then f"${now - actor.segmentEnterTime}%.2f" else "N/A"
        //easyW.println(s"[t=$now] Vehicle ${actor.displayLabel} enters seg=${actor.segId} (from seg=$lastSeg), enterT=$now")

        //debug("move", s"actor = $actor, disp=${actor.disp} along the VTransport")
        vdeque += actor

        tally(Vehicle.rt)

        var done = false
        while actor.disp < length && !done do
            //director.log.trace(this, "moves for " + Vehicle.rt, actor, director.clock)

            motion.updateV(actor, length) // update actor/vehicle's motion/position

            // --- Lane-change decisions (MOBIL + exit steering) ---
            val route = actor.myRoute
            if route != null && actor.laneID >= 0 && actor.myRamp == null then
                // 1. MOBIL discretionary (3s cooldown)
                if (director.clock - actor.lastLaneChangeTime) > 3.0 then
                    val dir = MOBIL.checkLaneChange (actor, route, actor.segId, actor.laneID)
                    if dir != 0 then
                        val ok = route.changeLane (actor.laneID, actor.laneID + dir, actor, actor.segId)
                        if ok then actor.lastLaneChangeTime = director.clock
                    end if
                end if
                // 2. Exit steering — runs independently, has its own cooldown
                if actor.exitRampIdx >= 0 then
                    route.exitCheckAndSteer (actor, actor.segId, director.clock)
                end if
            end if

            val cp = calcPoint2(actor.disp)
            //debug ("move", s"${actor.displayLabel}, check if actor.disp = ${actor.disp} >= curve.length = ${curve.length}")
            if actor.disp >= length then
                done = true
                vdeque -= actor
            end if

            if !done then
                val vColor = Vehicle.velocityColor (actor.velocity)
                director.animate(actor, MoveToken, vColor, null, cp)
            actor.schedule(Vehicle.rt)
            actor.yieldToDirector()
        end while

        //println(f"[move EXIT] ${actor.displayLabel}%-12s seg=${actor.segId} disp=${actor.disp}%.2f v=${actor.velocity}%.2f length=${length}%.2f vt=$name")
    end move

    def calcPoint2(s: Double): Array[Double] =
        curve.traj = s / curve.length // percentage of the curve the car has traveled thus far.
        val xy = curve.eval()
        Array(xy.x, xy.y)
    end calcPoint2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Calculate the (x, y) point in the simulation space for the vehicle.
     *
     * @param s the current displacement along the road of the vehicle.
     */
    def calcPoint(s: Double): Array[Double] =
        val prop = s / curve.length
        val x = p1(0) + (p2(0) - p1(0)) * prop
        val y = p1(1) + (p2(1) - p1(1)) * prop
        Array(x - RAD, y - RAD)
    end calcPoint

end VTransport