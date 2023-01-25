
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 1.6
 *  @date    Thu Sep 23 13:04:33 EDT 2021
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package simulation
package process

import java.io._
import java.util.concurrent.ConcurrentLinkedQueue 

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

import scalation.animation.CommandType._
import scalation.random.Variate
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._
import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Entrance` class is used to periodically inject entities (`SimActors`) into a
 *  running simulation model.  May act as an arrival generator.  Source is both
 *  a simulation `Component` and special `SimActor` and therefore runs in own thread.
 *  @param name          the name of the source
 *  @param director      the director controlling the model
 *  @param makeEntity    the function to make entities of a specified type
 *  @param esubtype      indicator of the subtype of the entities to me made
 *  @param units         the number of entities to make
 *  @param iArrivalTime  the inter-arrival time distribution
 *  @param loc           the location of the source (x, y, w, h)
 *  @param nt            the number of time intervals to save counts on
 */
class Entrance (name: String, director: Model, makeEntity: () => Vehicle, esubtype: Int, units: Int,
              iArrivalTime: Variate, loc: Array [Double], nt: Int)
      extends Source (name, director, makeEntity, esubtype, units, iArrivalTime, loc):
//      extends SimActor (name, director) with Component

    //initStats (name)

    //at = loc

    val cnts = new VectorD (nt)
 
    val speeds = new VectorD (nt)

    var pw: PrintWriter = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name          the name of the source
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param esubtype      indicator of the subtype of the entities to me made
     *  @param units         the number of entities to make
     *  @param iArrivalTime  the inter-arrival time distribution
     *  @param xy            the (x, y) coordinates for the top-left corner of the source.
     */
    def this (name: String, director: Model, makeEntity: () => Vehicle, esubtype: Int, units: Int,
              iArrivalTime: Variate, xy:  (Double, Double), nt: Int) =

        this (name, director, makeEntity, esubtype, units, iArrivalTime, Array (xy._1, xy._2, 30.0, 30.0), nt)
    end this

//    def addMakeEntity (me: () => SimActor) { makeEntity = me }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display 'this' source as a node on the animation canvas.
     */
    override def display (): Unit =

        director.animate (this, CreateNode, limegreen, Ellipse (), loc)
    end display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Entrance`s as special `SimActor` will act over time to make entities
     *  (other `SimActor`s).
     */
    override def act (): Unit =

        this.printString ("There will be " + units + " vehicles created")
        for rep <- 1 to director.reps do                                 // major loop - replications
            actTime = director.clock                                      // set to model start time
            var myClock = 0.0
            breakable {
                for i <- 1 to units do                          // minor loop - make actors
                    if director.stopped then break ()                              // terminate source, simulation ended

                    val actor = makeEntity ()                                 // make new actor
    //			    if (actor.name.equals ("c_3005")) println (this.name + " makes " + actor.name)
                    val dc = director.clock
                    val j  = Math.floor (myClock / timeConv).toInt //here no need to use mkInt
                    val cnt = cnts(j) + 1
                    cnts(j) = cnt
                    speeds(j) = (speeds(j) * (cnt - 1) + actor.asInstanceOf[Vehicle].velocity) / cnt
                    actor.mySource = this                                     // actor's source
                    actor.subtype  = esubtype                                 // set the entity subtype
                    director.log.trace (this, "generates", actor, dc)
                    director.animate (actor, CreateToken, randomColor (actor.id), Ellipse (),
                             Array (loc(0) + loc(2) + RAD / 2.0, loc(1) + loc(3) / 2.0 - RAD))
                    actor.schedule (0.0)
    //				println ("i = " + i + ", units = " + units)
                    if i < units then
                        val duration = iArrivalTime.gen
                        myClock += duration
                        tally (duration)
                        schedule (duration)
                        yieldToDirector ()                            // yield and wait duration time units
                    end if
    //                println ("I created " + actor + " at " + dc)
                end for
            } //for break


            if rep < director.reps then
                director.log.trace (this, "wait for next rep", director, director.clock)
                yieldToDirector ()                                // yield and wait for next replication				
            end if

        end for
//        println ("hello hello")
        director.log.trace (this, "terminates", null, director.clock)
        yieldToDirector (true)                                    // yield and terminate
    end act

    def getCounts: VectorD = cnts

    def getSpeeds: VectorD = speeds

    def openPrint (path: String): Unit = { pw = new PrintWriter (new FileWriter (new File (path))) }

    def printString (s: String): Unit =

        if pw != null then
            pw.println (s)
            pw.flush ()
    end printString


    def closePrint (): Unit =  pw.close()

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












