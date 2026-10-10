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
                  shift1: VectorD = VectorD (0, 0), shift2: VectorD = VectorD (0, 0), segIndex: Int = -1,
                  geometry: Array [(Double, Double)] = Array.empty)
    extends Transport (name, from_, to_, null, isSpeed, bend, shift1, shift2, geometry), TrafficLink:

    private val debug = debugf ("VTransport", false)                     // debug function
    //debug ("init", s"name = $name, p1 = $p1, pc = $pc, p2 = $p2, located at ${stringOf (at)}")

    val safetydist = 20.0
    


    private [process] val vdeque = ArrayDeque [Vehicle] ()               // Array Deque for finding vehicles based on entry order

    private[process] val easyW = new EasyWriter("simulation", "OnewayVehicle2LModel.txt")
    easyW.off()


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the first vehicle in `Vtransport` (the first element in vtree).
     */
    def getFirst: Vehicle =
        val first: Vehicle = if vdeque.isEmpty then null else vdeque.head
        debug ("getFirst", s"the first vehicle = $first")
        first
    end getFirst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the last vehicle in `Vtransport` (the last element in vtree).
     */
    def getLast: Vehicle =
        val last: Vehicle = if vdeque.isEmpty then null else vdeque.last
        debug ("getLast", s"the last vehicle = $last")
        last
    end getLast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // TRAFFIC STATE  (see context/traffic-state-spec.md, `TrafficLink`)
    // length, density and flow come from `TrafficLink`; this link supplies its count and speed.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Number of vehicles on this segment right now (the lane's `vdeque`).
     */
    def vehicleCount: Int = vdeque.size

    /** Space-mean speed on this segment.
     *      v_s = (1/n) * sum v_i        over the n vehicles on the segment now
     *  The arithmetic mean is correct because the sample is spatial -- averaging
     *  the speeds of the cars occupying a section at one instant IS the
     *  space-mean speed.  Do not substitute a time-mean speed (mean of spot
     *  speeds at a detector): v_t >= v_s (Wardrop), and only v_s satisfies
     *  q = k * v.  Stopped vehicles belong in the average.
     *  @return space-mean speed in m/s (0.0 when the segment is empty)
     */
    def spaceMeanSpeed: Double =
        if vdeque.isEmpty then 0.0
        else
            var sum = 0.0
            for v <- vdeque do sum += v.velocity
            sum / vdeque.size
    end spaceMeanSpeed


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
        easyW.println(s"[t=$now] Vehicle ${actor.displayLabel} enters seg=${actor.segId} (from seg=$lastSeg), enterT=$now")

        //debug("move", s"actor = $actor, disp=${actor.disp} along the VTransport")
        accum (vdeque.size)                                // existing time-persistent occupancy statistic
        vdeque += actor

        tally(Vehicle.rt)

        var done = false
        while actor.disp < length && !done do
            //director.log.trace(this, "moves for " + Vehicle.rt, actor, director.clock)

            motion.updateV(actor, length) // update actor/vehicle's motion/position

            val cp = calcPoint2(actor.disp)
            //debug ("move", s"${actor.displayLabel}, check if actor.disp = ${actor.disp} >= curve.length = ${curve.length}")
            if actor.disp >= length then
                done = true
                accum (vdeque.size)                        // occupancy held since the preceding entry/exit event
                vdeque -= actor
            end if

            if !done then
                val vColor = Vehicle.velocityColor (actor.velocity)
                director.animate(actor, MoveToken, vColor, null, cp)
            actor.schedule(Vehicle.rt)
            actor.yieldToDirector()
        end while

        //debug("moveFinal", s" $actor: Final actor displacement: t_disp = ${actor.t_disp}")
    end move

    def calcPoint2(s: Double): Array[Double] =
        // Fraction of the segment travelled.  Normalise by `length` (the PHYSICAL
        // length, metres where the config supplies it) -- not by curve.length,
        // which is the on-screen pixel length.  Using the wrong one sends traj
        // outside [0, 1] and the token leaves the road.
        curve.traj = if length > 0.0 then s / length else 0.0
        val xy = curve.eval()
        Array(xy.x, xy.y)
    end calcPoint2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Calculate the (x, y) point in the simulation space for the vehicle.
     *
     * @param s the current displacement along the road of the vehicle.
     */
    def calcPoint(s: Double): Array[Double] =
        val prop = if length > 0.0 then s / length else 0.0     // physical length, not pixels
        val x = p1(0) + (p2(0) - p1(0)) * prop
        val y = p1(1) + (p2(1) - p1(1)) * prop
        Array(x - RAD, y - RAD)
    end calcPoint

end VTransport
