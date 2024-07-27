
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


import scala.runtime.ScalaRunTime.stringOf
import scalation.animation.CommandType.*
import scalation.database.BpTreeMap
import scalation.mathstat.*
import scalation.simulation.process

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
    private val flaw  = flawf ("VTransport")                            // flaw function

    //To find out where you are use the B+tree
    //To keep track of the car ahead of you use the DoublyLinkList
    private [process] val vtree = new BpTreeMap [Vehicle] ()            // B+Tree map for finding vehicles by their displacement






    var length = 0.0    //  The actual length of the road segment.
    if length <= 0.0 then length = curve.length

    debug ("init", s"name = $name, p1 = $p1, pc = $pc, p2 = $p2, length = $length, located at ${stringOf (at)}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the first vehicle in `Vtransport` (the first element in vtree).
     */
    //def getFirst: Vehicle = vList.head
    def getFirst: Vehicle = vtree.getFirst  // get the first car in this vtree (B+tree)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move the entity (SimActor) smoothly down this VTransport (e.g., road).
     *  Repeatedely move it along the VTransport/Edge/QCurve.
     *  Caveat: tokens coordinates are computed using a shadow QCurve (same coordinates
     *  as the one that will be created by the animation engine).
     *  @param fraction  the fraction of the remaining transform to move along (defaults to 1.0)
     */
    override def move(): Unit =
        val actor = director.theActor.asInstanceOf[Vehicle]
        println("The move statement @@@")
        debug("move", s"actor = $actor this along the VTransport with length = $length")
        vtree.put(actor.t_disp, actor)                                                    // put vehicle into vtree map by initial diplacement

        val car_ahead = Vehicle.vlist.getSucc(actor.myCarNode)
        if car_ahead == null then Vehicle.vlist.add(actor)
        else Vehicle.vlist.addBefore(actor, car_ahead)
        tally(Vehicle.rt)

        actor.disp = 0

        var done = false
        while actor.disp < length && !done do
            director.log.trace(this, "moves for " + Vehicle.rt, actor, director.clock)
            debug("move", s"actor = $actor to be moved by motion = $motion")

            motion.updateV(actor)                                                       // update actor/vehicle's motion/position
            debug("move", s"actor = $actor has moved and fraction ")
            //actor.t_disp += actor.disp

            vtree.update(actor.t_disp, actor)                                             // reposition vehicle in vtree by new position
            debug("move", s"${actor.name},@@car= $actor @@displacement = ${actor.t_disp}")
            val cp = calcPoint2(actor.disp )
            val cpx = cp(0)
            val cpy = cp(1)

            if actor.disp >= length then // curve.length then
                debug("actorCondition", s"actor.disp=${actor.disp}, actor = $actor actor.t_disp =${actor.t_disp} , done = $done")
                done = true                                                                                 // done as actor/vehicle at end


//                if !vtree.checkedRemove(actor.t_disp, actor) then                                             // remove from vtree and check that it is the correct vehicle
//                    vtree.showLink()
//                    flaw("move", s"removed the wrong vehicle from vtree actor = $actor, actor.disp = ${actor.disp}, done = $done")
                vtree.remove(actor.t_disp)
                Vehicle.vlist.remove(actor.myCarNode)
            end if

            if !done then
                director.animate(actor, MoveToken, null, null, cp)

            actor.schedule(Vehicle.rt)
            actor.yieldToDirector()
        end while

        actor.t_disp += actor.disp

    end move

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the (x, y) point in the simulation space for the vehicle.
     *  @param s  the current displacement along the road of the vehicle.
     */
//    def calcPoint (s: Double): Array [Double] =
//        val prop = s / curve.length
//        val x = p1(0) + (p2(0) - p1(0)) * prop     // fixed a small typo bug (p1(0))
//        val y = p1(1) + (p2(1) - p1(1)) * prop
//        Array (x - RAD, y - RAD)
//    end calcPoint

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Calculate the (x, y) point in the simulation space for the vehicle.
     *
     * @param s the current displacement along the road of the vehicle.
     */
    def calcPoint2(s: Double): Array[Double] =
        curve.traj = s / curve.length // percentage of the curve the car has traveled thus far.
        val xy = curve.eval()
        Array(xy.x, xy.y)
    end calcPoint2

end VTransport
