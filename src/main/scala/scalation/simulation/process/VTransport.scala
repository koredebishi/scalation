//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** @author  John Miller, Casey Bowman
// *  @version 2.0
// *  @date    Tue Feb  4 14:56:34 EST 2020
// *  @see     LICENSE (MIT style license file).
// *
// *  @note    Variable Speed Transport is a Pathway between Components
// */
//
//package scalation
//package simulation
//package process
//
//import scala.collection.mutable.ArrayDeque
//import scala.runtime.ScalaRunTime.stringOf
//
//import scalation.animation.CommandType._
////import scalation.database.BpTreeMap
//import scalation.mathstat._
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** The `VTransport` class provides a variable-speed pathway between two other components.
// *  The components in a `Model` conceptually form a 'graph' in which the edges
// *  are `VTransport`s and the nodes are other `Component`s.
// *  @see `animation.Dgraph.move2Boundary` that aligns edge with node boundaries.
// *  @param name      the name of the variable-speed transport
// *  @param from      the starting component
// *  @param to        the ending component
// *  @param motion    the dynamics model for the speed/trip-time for motion down the `VTransport`
// *  @param isSpeed   whether speed or trip-time is used for motion
// *  @param bend      the bend or curvature of the `VTransport` (0 => line)
// *  @param shift1    the x-y shift for the transport's first end-point (from-side)
// *  @param shift2    the x-y shift for the transport's second end-point (to-side)
// */
//class VTransport (name: String, from_ : Component, to_ : Component,
//                  motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0,
//                  shift1: VectorD = VectorD (0, 0), shift2: VectorD = VectorD (0, 0))
//  extends Transport (name, from_, to_, null, isSpeed, bend, shift1, shift2):
//
//    private val debug = debugf ("VTransport", true)                     // debug function
//    //  private val flaw  = flawf ("VTransport")                            // flaw function
//
//    private [process] val vtree = ArrayDeque [Vehicle] ()               // Array Deque for finding vehicles based on entry order
//    //  private [process] val vtree = new BpTreeMap [Vehicle] (name)        // B+Tree map for finding vehicles by their displacement
//
//    debug ("init", s"name = $name, p1 = $p1, pc = $pc, p2 = $p2, located at ${stringOf (at)}")
//
//    var length = 0.0    //  The actual length of the road segment.
//    if length <= 0.0 then length = curve.length
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Get the first vehicle in `Vtransport` (the first element in vtree).
//     */
//    def getFirst: Vehicle =
//        val first: Vehicle = if vtree.isEmpty then null else vtree.head
//        debug ("getFirst", s"the first vehivle = $first")
//        first
//    end getFirst
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Get the last vehicle in `Vtransport` (the last element in vtree).
//     */
//    def getLast: Vehicle =
//        val last: Vehicle = if vtree.isEmpty then null else vtree.last
//        debug ("getLast", s"the last vehivle = $last")
//        last
//    end getLast
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Move the entity (SimActor) smoothly down this VTransport (e.g., road).
//     *  Repeatedely move it along the VTransport/Edge/QCurve.
//     *  Caveat: tokens coordinates are computed using a shadow QCurve (same coordinates
//     *  as the one that will be created by the animation engine).
//     */
//    override def move(): Unit =
//
//        println("Move in the Vtransport")
//        println(s"The director is $director")
//        val actor = director.theActor.asInstanceOf[Vehicle]
//
//        println("Move in the Vtransport after Actor")
//
//        actor.disp = 0
//        actor.laneID = this.name     //update the current actor's lane_ID with the Vtransport_ID it is moving in
//
//        debug("move", s"actor = $actor, disp=${actor.disp} total dis=${actor.t_disp} this along the VTransport with length = $length")
//        vtree += actor
//        println(s"car: $actor added to vdeque:${vtree.toString()}")
//        tally(Vehicle.rt)
//
//        var done = false
//        while actor.disp < length && !done do
//            director.log.trace(this, "moves for " + Vehicle.rt, actor, director.clock)
//            debug("move", s"actor = $actor to be moved by motion = $motion")
//
//            motion.updateV(actor, length) // update actor/vehicle's motion/position
//            val cp = calcPoint2(actor.disp)
//            debug ("move", s"${actor.name}, check if actor.disp = ${actor.disp} >= curve.length = ${curve.length}")
//            if actor.disp >= length then
//                done = true
//                vtree -= actor
//            end if
//
//            if !done then
//                director.animate(actor, MoveToken, null, null, cp)
//            actor.schedule(Vehicle.rt)
//            actor.yieldToDirector()
//        end while
//
//        debug("moveFinal", s"Final actor displacement: t_disp = ${actor.t_disp}")
//    end move
//
//    def calcPoint2(s: Double): Array[Double] =
//        curve.traj = s / curve.length // percentage of the curve the car has traveled thus far.
//        val xy = curve.eval()
//        Array(xy.x, xy.y)
//    end calcPoint2
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//    /** Calculate the (x, y) point in the simulation space for the vehicle.
//     *
//     * @param s the current displacement along the road of the vehicle.
//     */
//    def calcPoint(s: Double): Array[Double] =
//        val prop = s / curve.length
//        val x = p1(0) + (p2(0) - p1(0)) * prop
//        val y = p1(1) + (p2(1) - p1(1)) * prop
//        Array(x - RAD, y - RAD)
//    end calcPoint
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Find the car ahead of the actor in the same VTransport using the vtree deque.
//     *  @param actor  the actor for which to find the car ahead
//     */
//    def findCarAhead(actor: Vehicle): Vehicle =
//        val car_index = vtree.indexOf(actor)                                             // Get the index of the actor in the vtree
//        if car_index >= 0 && car_index < vtree.size - 1 then                            // Check if the actor is in the vtree
//            vtree(car_index + 1)                                                        // Get the carAhead of the index_actor
//        else null                                                                       // Return null if not found or no carAhead
//    end findCarAhead
//
//
//end VTransport
//



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

        debug("moveFinal", s"Final actor displacement: t_disp = ${actor.t_disp}")
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




//    def findCarAhead(actor: Vehicle): Vehicle =
//        val car_index = vtree.indexOf(actor)                                             // Get the index of the actor in the vtree
//        if car_index >= 0 && car_index < vtree.size - 1 then                            // Check if the actor is in the vtree
//            vtree(car_index + 1)                                                        // Get the carAhead of the index_actor
//        else null                                                                       // Return null if not found or no carAhead
//    end findCarAhead
//


//1----2-----3---4----5-----6
//get the segment ahead of you
//get the last car in that segment
//add yourself behind the car in that segment

// the head from the
//                deque1                      deque2
//Source--------Vtran1-------hunkCarjunc-----------Vtran2-------Sink
//find the Car (last) car in deque1:
//gap = length - car_disp
// if gap >= safetyDisp then
//Car(last) in vdeque1 .   ahead inside the doublylinkedlist// to get the first car inside the Vdeque2
//

//Car(last)Vdeque1--------laneChangedCar--------------Car(last).ahead using doublylinkedlist
//remove lchangeCar from old Vdeque
//Add lChange into the new Vdeque
//remove from the old doublylinkedlist
//add yourself to the new doublylinkedlist

//1. update Vtranport with vdeque; each Vtransport should have a vdeque     : Done
//2. move method, use the same logic for updating adding and removing a vehicle from a vdeque: Done
//3. with a reference vehicle:

//Test by printing without actually doing the main lane change structural change.
//Source--------Vtran1-------hunkCarjunc-----------Vtran2-------Sink
//find the Car (lead) car in deque1:
//use getFirst in the vdeque
//gap = length - car_disp
// use the curve.lengh for lengh - car_disp

// (
// laneChange logic: true
// if gap >= safetyDisp then
//remove lchangeCar from old Vdeque
//Add lChange into the new Vdeque
//remove from the old doublylinkedlist
//add yourself to the new doublylinkedlist
//)
// junc.jump
//pathway(j)(1).move()
//
//Car(last) in vdeque1 .   ahead inside the doublylinkedlist// to get the first car inside the Vdeque2

//else
//pathway(i)(1).move  // no lanechange


