
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 1.6
 *  @date    Thu Sep 23 13:04:33 EDT 2021
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package simulation.agent

import java.io.*
import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
import scalation.mathstat.*
import scalation.animation.CommandType.*
import scalation.random.*
import scalation.database.graph.*
import scalation.scala2d.Colors.{limegreen, randomColor}
import scalation.scala2d.Ellipse

//import scalation.simulation.Temporal
//import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Entrance` class is used to periodically inject entities (`SimActors`) into a
 *  running simulation model.  May act as an arrival generator.  Source is both
 *  a simulation `Component` and special `SimActor` and therefore runs in own thread.
 *  @param name          the name of the source
 *  @param director      the director controlling the model
 *  @param starttime     starting time
 *  @param makeEntity    the function to make entities of a specified type
 *  @param esubtype      indicator of the subtype of the entities to me made
 *  @param units         the number of entities to make
 *  @param iArrivalTime  the inter-arrival time distribution
 *  @param loc           the location of the source (x, y, w, h)
 *  @param nt            the number of time intervals to save counts on
 */
class Entrance (_name: String, director: Model, starttime:Double, makeEntity: () => Vehicle, units: Int, esubtype: Int,
              iArrivalTime: Variate, pos:VectorD =  null, nt: Int = 288, prop: Property = null )
      extends Source (_name, director,starttime, iArrivalTime,
              makeEntity,units,  esubtype, prop, pos):
    //val at = loc
    //Source.add(this)
    val cnts = new VectorD(nt)

    val speeds = new VectorD(nt)

    var pw: PrintWriter = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults a tuple of double double for location
     *  @param xy            the (x, y) coordinates for the top-left corner of the source.
     */

    def this (_name: String, director: Model, starttime:Double, makeEntity: () => Vehicle, units: Int, esubtype: Int,
              iArrivalTime: Variate, xy:  (Double, Double), nt: Int) =
        this (_name, director, starttime, makeEntity, units, esubtype,
              iArrivalTime, VectorD (xy._1, xy._2, 30.0, 30.0), nt)
    end this
     // constructor*/

    //    def addMakeEntity (me: () => SimActor) { makeEntity = me }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display 'this' source as a node on the animation canvas.
     */



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Entrance`s as special `SimActor` will act over time to make entities
     *  (other `SimActor`s).
     */
    override def act(): Unit =

        this.printString("There will be " + units + " vehicles created")
        for rep <- 1 to director.reps do // major loop - replications
          //actTime = director.clock // set to model start time
          //comment out: 1. process can access but agent doesn't, 2. did not see it was used later
          var myClock = 0.0
          breakable {
            for i <- 1 to units do // minor loop - make agents
              if !director.simulating then break() // terminate source, simulation ended

              val agent = makeEntity() // make new agent
              //			    if (agent.name.equals ("c_3005")) println (this.name + " makes " + agent.name)
              val j = Math.floor(myClock / timeConv).toInt //here no need to use mkInt
              val cnt = cnts(j) + 1
              cnts(j) = cnt
              speeds(j) = (speeds(j) * (cnt - 1) + agent.asInstanceOf[Vehicle].velocity) / cnt
              //agent.mySource = this // agent's source
              //what is this for?
              agent.subtype = esubtype // set the entity subtype
              agent.time    = director.clock
              agent.setPos (pos(0), pos(1))                            // FIX - put at boundary, not center
              director.log.trace(this, "generates", agent, director.clock)
              director.animate(agent, CreateToken, randomColor(agent.id), Ellipse())
              //, Array(loc(0) + loc(2) + RAD / 2.0, loc(1) + loc(3) / 2.0 - RAD)) // no need
              director.schedule(agent)
              //				println ("i = " + i + ", units = " + units)
              //println(s" units are $units")
              if i < units then
                val duration = iArrivalTime.gen
                tallyStats (duration)
                myClock += duration
                time = director.clock + duration //bug to be fixed
                //println(s" current this(agent).time is $time and director ${director.clock}")
                director.schedule(this)
                //println(s"direct schedules this = $this")
                yieldToDirector() // yield and wait duration time units
              end if
            //                println ("I created " + agent + " at " + dc)
            end for
          } //for break


          if rep < director.reps then
            director.log.trace(this, "wait for next rep", director, director.clock)
            yieldToDirector() // yield and wait for next replication
          end if

        end for
        //        println ("hello hello")
        director.log.trace(this, "terminates", null, director.clock)
        yieldToDirector(true) // yield and terminate
    end act

    def getCounts: VectorD = cnts

    def getSpeeds: VectorD = speeds

    def openPrint(path: String): Unit = {
      pw = new PrintWriter(new FileWriter(new File(path)))
    }

    def printString(s: String): Unit =

        if pw != null then
          pw.println(s)
          pw.flush()
    end printString


    def closePrint(): Unit = pw.close()

end Entrance





/*
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Entrance` companion object provides a builder method for sources.
 */
object Entrance
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a source using defaults for width 'w' and height 'h'.
     *  @param name          the name of the source
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param esubtype      indicator of the subtype of the entities to me made
     *  @param units         the number of entities to make
     *  @param iArrivalTime  the inter-arrival time distribution
     *  @param xy            the (x, y) coordinates for the top-left corner of the source.
     */
    def apply (name: String, director: Model, makeEntity: () => SimActor, esubtype: Int, units: Int,
              iArrivalTime: Variate, xy: Tuple2 [Int, Int]): Source =
    {
        new Entrance (name, director, makeEntity, esubtype, units, iArrivalTime,
                    Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0))
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related sources using defaults for width 'w' and height 'h'.
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param units         the number of entities to make
     *  @param xy            the (x, y) coordinates for the top-left corner of the reference source.
     *  @param src           repeated source specific info: name, subtype, distribution, offset
     */
    def group (director: Model, makeEntity: () => SimActor, units: Int, xy: Tuple2 [Int, Int],
               src: Tuple4 [String, Int, Variate, Tuple2 [Int, Int]]*): List [Source] =
    {
        val sourceGroup = new ListBuffer [Source] ()
        for (s <- src) sourceGroup += Entrance (s._1, director, makeEntity, s._2, units, s._3,
                                             (xy._1 + s._4._1, xy._2 + s._4._2))
        sourceGroup.toList
    } // group

} // Entrance object
*/












