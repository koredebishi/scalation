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

//import scala.collection.mutable.ArrayDeque
import scala.runtime.ScalaRunTime.stringOf

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
                  shift1: VectorD = VectorD (0, 0), shift2: VectorD = VectorD (0, 0))
  extends Transport (name, from_, to_, null, isSpeed, bend, shift1, shift2):

    private val debug = debugf ("VTransport", true)                     // debug function
    debug ("init", s"name = $name, p1 = $p1, pc = $pc, p2 = $p2, located at ${stringOf (at)}")

    var length = 0.0    //  The actual length of the road segment.
    if length <= 0.0 then length = curve.length
    val safetydist = 20.0


    private [process] val vdeque = ArrayDeque [Vehicle] ()               // Array Deque for finding vehicles based on entry order

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
        debug ("getLast", s"the last vehivle = $last")
        last
    end getLast


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move the entity (SimActor) smoothly down this VTransport (e.g., road).
     *  Repeatedely move it along the VTransport/Edge/QCurve.
     *  Caveat: tokens coordinates are computed using a shadow QCurve (same coordinates
     *  as the one that will be created by the animation engine).
     */

    override def move(): Unit =


        val actor = director.theActor.asInstanceOf[Vehicle]

        actor.disp = 0
        actor.pathInfo = this.name     //update the current actor's lane_ID with the Vtransport_ID it is moving in

        debug("move", s"actor = $actor, disp=${actor.disp} along the VTransport")
        vdeque += actor

        tally(Vehicle.rt)

        var done = false
        while actor.disp < length && !done do
            director.log.trace(this, "moves for " + Vehicle.rt, actor, director.clock)

            motion.updateV(actor, length) // update actor/vehicle's motion/position
            val cp = calcPoint2(actor.disp)
            debug ("move", s"${actor.name}, check if actor.disp = ${actor.disp} >= curve.length = ${curve.length}")
            if actor.disp >= length then
                done = true
                vdeque -= actor
            end if

            if !done then
                director.animate(actor, MoveToken, null, null, cp)
            actor.schedule(Vehicle.rt)
            actor.yieldToDirector()
        end while

        debug("moveFinal", s" $actor: Final actor displacement: t_disp = ${actor.t_disp}")
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

