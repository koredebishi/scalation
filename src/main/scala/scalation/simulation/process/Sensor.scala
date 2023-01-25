
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Thu Sep 23 14:12:05 EDT 2021
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package simulation
package process

import java.io._

import scala.collection.mutable.ListBuffer

import scalation.animation.CommandType._
import scalation.random.{Sharp, Uniform, Variate}
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._
//import scalation.util.Monitor.trace
import scalation.mathstat._


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Sensor` class provides a connector between two `Transport`s/`Route`s.
 *  Since `Lines` and `QCurves` have limitations (e.g., hard to make a loop back),
 *  a junction may be needed.
 *  @param name      the name of the junction
 *  @param director  the director controlling the model
 *  @param jTime     the jump-time through the junction
 *  @param at        the location of the junction (x, y, w, h)
 *  @param nt        the number of time intervals
 */
class Sensor (name: String,  at: Array [Double], nt: Int, jTime: Variate = Sharp(0))
      extends Junction(name,  jTime, at):
    //initComponent (name, at)

    private var onJunction = 0        // the number of entities/sim-actors on this Junction

    val counts = new VectorD (nt)    //5:45 am  to 6:15 pm: 50. 15 minutes time units

    val speeds = new VectorD (nt)

    val speedStat = new Statistic ("speed stat")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name      the name of the junction
     *  @param director  the director controlling the model
     *  @param jTime     the jump-time through the junction
     *  @param xy        the (x, y) coordinates for the top-left corner of the junction
     */
    def this (name: String, director: Model, xy:  (Double, Double),  nt: Int, jTime: Variate) =
      this (name,  Array (xy._1, xy._2, 4.0, 4.0),  nt, jTime)
    end this
    // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Junction.
     */
    override def display (): Unit =
        director.animate (this, CreateNode, purple, Ellipse (), at)
    end display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Jump the entity `SimActor` from the incoming "from" transport to the
     *  the middle of the junction.
     */
    override def jump (): Unit =
        val actor    = director.theActor.asInstanceOf [Vehicle]
        val duration = jTime.gen
        val dc = director.clock
        val i = Math.floor (dc / timeConv).toInt
        val vel = actor.velocity
//        println (this.name + ": "  + actor.name + ": " + vel)
//        actor.println (this.name + ":\t" + dc)
        val cnt = counts(i) + 1
        counts(i) = cnt
        speeds(i) = (speeds(i) * (cnt - 1) + vel) / cnt
        speedStat.tally (vel)
        tally (duration)
        accum (onJunction)
        onJunction += 1
        director.log.trace(this, "jump for " + duration, actor, director.clock)
        
        director.animate (actor, MoveToken, null, null, Array (at(0) + RAD, at(1) + RAD))
        actor.schedule (duration)
        actor.yieldToDirector ()
        accum (onJunction)
        onJunction -= 1
    end jump

    def jump (t1: Int): Unit =

        val actor    = director.theActor
        val duration = jTime.gen
        val dc = director.clock
        val i = t1 + Math.floor (dc / timeConv).toInt
        counts(i) += 1
        tally (duration)
        accum (onJunction)
        onJunction += 1
        director.log.trace (this, "jump for " + duration, actor, director.clock)
        
        director.animate (actor, MoveToken, null, null, Array (at(0) + RAD, at(1) + RAD))
        actor.schedule (duration)
        actor.yieldToDirector ()
        accum (onJunction)
        onJunction -= 1
    end jump

    def jump (pw: PrintWriter): Unit =

        val actor    = director.theActor
        val duration = jTime.gen
        tally (duration)
        accum (onJunction)
        onJunction += 1
        val dc = director.clock
        director.log.trace (this, "jump for " + duration, actor, dc)
        pw.println (name + "\t" + f"$dc%1.4f")
        pw.flush()
        director.animate (actor, MoveToken, null, null, Array (at(0) + RAD, at(1) + RAD))
        actor.schedule (duration)
        actor.yieldToDirector ()
        accum (onJunction)
        onJunction -= 1
    end jump

    def getCounts: VectorD = counts

    def getSpeeds: VectorD = speeds

end Sensor


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Junction` companion object provides a builder method for sinks.
 */
object Sensor:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a junction using defaults for width 'w' and height 'h'.
     *  @param name      the name of the sensor
     *  @param director  the director controlling the model
     *  @param jTime     the jump-time through the sensor
     *  @param xy        the (x, y) coordinates for the top-left corner of the junction
     */
    def apply (name: String,  xy: (Double, Double) ,nt:Int,  jTime: Variate): Sensor =

        new Sensor (name,  Array (xy._1, xy._2, 10.0, 10.0),nt, jTime)
    end apply

    def apply(name: String,  xy: (Double, Double), nt:Int): Sensor =

        new Sensor(name,  Array(xy._1, xy._2, 10.0, 10.0), nt)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related junctions using defaults for width 'w' and height 'h'.
     *  @param director  the director controlling the model
     *  @param jTime     the jump-time through the junction
     *  @param xy        the (x, y) coordinates for the top-left corner of the reference junction.
     *  @param jnt       repeated junction specific info: name, offset
     */
    def group (jTime: Variate, xy: Tuple2 [Int, Int], nt: Int,
               jnt: Tuple2 [String, Tuple2 [Int, Int]]*): List [Sensor] =

        val sensorGroup = new ListBuffer [Sensor] ()
        for (j <- jnt) sensorGroup += Sensor (j._1, (xy._1 + j._2._1, xy._2 + j._2._2),nt,  jTime)
        sensorGroup.toList
    end group

end Sensor

@main def SensorTest(): Unit =

    val numInterval = 96
    new RoadModel ("Road", 3, Uniform (10, 50), Uniform (29, 30))
    class RoadModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
          extends Model (name, animating = true, aniRatio = 120):

 //       val counts   = Array.ofDim [Int] (96)
        val onRamp   = new Source ("onRamp", this, () => Car (), 0, nArrivals, iArrivalRV, (100.0, 200.0))
        val offRamp  = new Sink ("offRamp", (700.0, 200.0))
        val junction = new Sensor ("junction",  Array(400.0, 200.0), nt = numInterval)
        val road1    = new Transport ("road1", onRamp, junction, moveRV, false, 0.25)
        val road2    = new Transport ("road2", junction, offRamp, moveRV, false, 0.25)
  
        addComponent (onRamp, offRamp, junction, road1, road2)        // Caveat: must add from and to before transport!!        

        case class Car () extends Vehicle ("c", this):

            def act (): Unit =
                println("this car has been activated!")
                road1.move ()
                junction.jump ()
                road2.move ()
                offRamp.leave ()
            end act

        end Car
        simulate ()
        waitFinished ()
        Model.shutdown ()

    end RoadModel

end SensorTest














