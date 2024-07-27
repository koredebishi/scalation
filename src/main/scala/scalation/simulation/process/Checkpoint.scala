
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Nov 21 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Checkpoint/Connector between Two Pathways (Transport, VTransport, Path, or Route)
 */

package scalation
package simulation
package process

import scala.collection.mutable.ArrayBuffer as VEC
//import scala.runtime.ScalaRunTime.{array_clone, stringOf}
import scala.runtime.ScalaRunTime.stringOf
import scalation.animation.CommandType.*
import scalation.random.{Sharp, Variate}
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Checkpoint` class provides a connector between two pathways.
 *  Since `Lines` and `QCurves` have limitations (e.g., hard to make a loop back),
 *  a Checkpoint may be needed.  Also, may be used to model road segments connected
 *  by Checkpoints or placements of sensors.
 *  @param name   the name of the Checkpoint
 *  @param jTime  the jump-time through the Checkpoint
 *  @param at     the location of the Checkpoint (x, y, w, h)
 */
class Checkpoint (name: String, jTime: Variate, at: Array [Double])
    extends Component
        with Recorder ():

    initComponent (name, at)

    private val debug = debugf ("Checkpoint", true)                    // debug function 

    debug ("init", s"name = $name, located at ${stringOf (at)}")

    private var onCheckpoint = 0                                       // number of entities/actors on this Checkpoint

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width w and height h,
     *  as well as jTime.
     *  @param name   the name of the Checkpoint
     *  @param jTime  the jump-time through the Checkpoint
     *  @param xy     the (x, y) coordinates for the top-left corner of the Checkpoint
     */
    def this (name: String, jTime: Variate = Sharp (1.0), xy: (Double, Double)) =
        this (name, jTime, Array (xy._1, xy._2, 10.0, 10.0))
    end this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Checkpoint.
     */
    def display (): Unit =
        director.animate (this, CreateNode, purple, Ellipse (), at)
    end display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Jump the entity `SimActor` from the incoming "from" pathway to the
     *  the middle of the Checkpoint.
     */
    def jump (l1: Int, l2: Int, lane: Array[VTransport] ): Boolean =

        val actor    = director.theActor.asInstanceOf[Vehicle]
        val duration = jTime.gen
        val ctime    = director.clock                                // clock time
        tally (duration)                                             // tally duration
        accum (onCheckpoint)                                           // accumulate on Checkpoint stats
        record (actor, ctime)                                        // record actor flow
        onCheckpoint += 1
        director.log.trace (this, s"jump for $duration", actor, director.clock)

        director.animate (actor, MoveToken, null, null, Array (at(0) + RAD, at(1) + RAD))
        actor.schedule (duration)
        actor.yieldToDirector ()
        accum (onCheckpoint)
        onCheckpoint -= 1

        checkLane(l1,l2, actor,lane)

    end jump

    /**
     * @param l1    the current lane of the actor
     * @param l2    the target lane to check
     * @param actor : The actor that needs a lane check
     * @return
     */
    def checkLane(l1: Int, l2: Int, actor: Vehicle, lane: Array[VTransport]): Boolean =
        println(s"This Vehicle_id: ${actor.id}, displacement: ${actor.disp} is currently in lane, $l1")
        val safetyDist = 10.0

        // Query the vtree to see if there are other vehicles in the lane (l2) at the current displacement +/- safety distance
        val otherCars = lane(l2).vtree.range((actor.t_disp - safetyDist).asInstanceOf[ValueType], (actor.t_disp + safetyDist).asInstanceOf[ValueType])

        println(s"There are ${otherCars.size} vehicles in lane $l2 near vehicle: ${actor.id} in lane: $l1 in the range query: $otherCars")

        if otherCars.isEmpty then
            //changeLane(l1, l2, actor,lane)
            true
        else
            println(s"The closest vehicle in lane $l2 to the actor is: ${otherCars.minBy { case (k, _) => (k.asInstanceOf[Double] - actor.disp).abs }._2}")
            false
    end checkLane


//    def changeLane(l1: Int, l2: Int, actor: Vehicle, lane: Array[VTransport]): Unit =
//        println(s"Changing from lane $l1 to lane $l2 for Vehicle_id: ${actor.id} to new displacement:")
//
//        // Remove actor from the current lane's doubly linked list via the SimActor dll module
//        SimActor.removeFromAlist(actor)
//
//        //remove the same actor from the B+tree lane too (from lane 1)
//        lane(l1).vtree.remove(actor.t_disp)
//        println("CheckRemove from the changeLane method")
//        //lane(l1).vtree.checkedRemove(actor.disp, actor)
//
//
//        // Insert actor in the new lane's doubly linked list at the correct position using the SimActor dll module
//        //<------ Add code here
//        //We will need to create a method inside the SimActor class that can add node at exact position
//        // of the doublylinkedlist using the disp as a reference key of insertion
//        // May need to use the ra
//        //actor.disp = newDisp
//
//        lane(l2).vtree.put(actor.t_disp, actor)  // Put actor in the lane2 B+tree with his updated displacement
//        //lane(l2).vtree.update(actor.t_disp, actor) // is this needed at this stage
//
//        SimActor.addBeforeAlist(actor, l2, lane) // Put actor in List of SimActor car
//
//    end changeLane



end Checkpoint



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Checkpoint` companion object provides a factory method for Checkpoints.
 */
object Checkpoint:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Checkpoint using defaults for width w and height h.
     *  @param name   the name of the Checkpoint
     *  @param jTime  the jump-time through the Checkpoint
     *  @param xy     the (x, y) coordinates for the top-left corner of the Checkpoint
     */
    def apply (name: String, jTime: Variate, xy: (Int, Int)): Checkpoint =
        new Checkpoint (name, jTime, Array (xy._1.toDouble, xy._2.toDouble, 10.0, 10.0))
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related Checkpoints using defaults for width w and height h.
     *  @param jTime     the jump-time through the Checkpoint
     *  @param xy        the (x, y) coordinates for the top-left corner of the reference Checkpoint.
     *  @param jnt       repeated Checkpoint specific info: name, offset
     */
    def group (jTime: Variate, xy: (Int, Int),
               jnt: (String, (Int, Int))*): List [Checkpoint] =
        val CheckpointGroup = new VEC [Checkpoint] ()
        for j <- jnt do CheckpointGroup += Checkpoint (j._1, jTime, (xy._1 + j._2._1, xy._2 + j._2._2))
        CheckpointGroup.toList
    end group

end Checkpoint

