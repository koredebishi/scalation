
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    VSource Creates Entities/SimActors -> Vehicles
 */

package scalation
package simulation
package process

import scala.collection.mutable.ArrayBuffer as VEC
import scala.runtime.ScalaRunTime.stringOf
import scala.util.control.Breaks.{break, breakable}
import scalation.animation.CommandType.*
import scalation.random.*
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors.*
import scalation.simulation.process.example_1.OneWayVehicle2L


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VSource` class is used to periodically inject entities (`Vehicle`s) into a
 *  running simulation model.  May act as an arrival generator.  VSource is both
 *  a simulation `Component` and special `SimActor` and therefore runs in own thread.
 *  @param name          the name of the source
 *  @param director      the director controlling the model
 *  @param makeEntity    the function to make entities of a specified type
 *  @param esubtype      indicator of the subtype of the entities to be made
 *  @param units         the number of entities to make
 *  @param iArrivalTime  the inter-arrival time distribution
 *  @param loc           the location of the source (x, y, w, h)
 */
class VSource (name: String, director: Model, makeEntity: () => Vehicle,
               esubtype: Int, units: Int,
              iArrivalTime: Variate, loc: Array [Double])
      extends Source (name, director, null, esubtype, units, iArrivalTime, loc):

/*
      extends SimActor (name, director)
         with Component
         with Recorder ():
*/
//    initStats (name)
//    at = loc
//    println(s"${Console.RED} initializing $this again inside the VSource: ${stringOf (at)} ${Console.RESET}")

    private val debug = debugf ("VSource", false)                             // debug function

    private[process] val ew = new EasyWriter("recorder", "VsourceWriter.txt")


    debug ("Init", s"name = $name, located at ${stringOf (at)}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name          the name of the source
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param esubtype      indicator of the subtype of the entities to me made
     *  @param units         the number of entities to make
     *  @param iArrivalTime  the inter-arrival time distribution
     *  @param xy            the (x, y) coordinates for the top-left corner of the source.
    def this (name: String, director: Model, makeEntity: () => Vehicle, esubtype: Int,
              units: Int, iArrivalTime: Variate, xy: (Double, Double)) =
        this (name, director, makeEntity, esubtype, units, iArrivalTime,
              Array (xy._1, xy._2, 20.0, 20.0))
    end this
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display this source as a node on the animation canvas.
    def display (): Unit =
        director.animate (this, CreateNode, limegreen, Ellipse (), at)
    end display
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `VSource`s as special `Vehicle` will act over time to make entities
     *  (other `Vehicle`s).
     */
    override def act (): Unit =
        for rep <- 1 to director.reps do                                     // MAJOR LOOP - replications
            actTime = director.clock                                         // set to model start time

            breakable {
                debug ("act", s"start making $units Vehicles")
                for i <- 1 to units do                                       // MINOR LOOP - make actors
                    if director.stopped then
                        println (s"VSource.act: simulation unexpectedly ended at ${director.clock}")
                        break ()                                             // terminate source, simulation ended
                    debug ("act", s"make entity/vehicle $i")
                    val actor = makeEntity ()                                // make new actor/vehicle
                    debug ("act", s"after make entity/vehicle $i: actor = $actor")
                    actor.mySource = this                                    // actor's source
                    actor.subtype  = esubtype                                // set the entity subtype
                    director.numActors += 1                                  // number of actors created by all sources, so far
                    if director.isAnimating then director.dgAni.updateActorCount(director.numActors)  // korede
                    director.log.trace (this, "generates", actor, director.clock)
                    director.animate (actor, CreateToken, randomColor (actor.id), Ellipse (),
                                      Array (at(0) + at(2) + RAD / 2.0, at(1) + at(3) / 2.0 - RAD))
                    debug ("act", s"schedule actor $i")
                    actor.schedule (0.0)
                    debug ("act", s"after schedule actor $i")

                    //this mechanism helps the RowTimeLoader trait to conveniently switch to the next row of the dataset
                    //Using this allows us to adjust the intensity rate of the Expo2 function
                    if director.isInstanceOf[RowTimeLoader] then
                        val rowManager = director.asInstanceOf[RowTimeLoader]
                        println(s"I director clock executed at this time: ${director.clock}")
                        rowManager.nextRow(director.clock)
                        //ew.write(s"\n [VSource] directorclock: ${director.clock}: curRow = ${rowManager.curRow},mu:  ${iArrivalTime.mean}")
                        println(s"I executed")
                    end if

                    if director.isInstanceOf[OneWayVehicle2L] then
                        val rowManager = director.asInstanceOf[OneWayVehicle2L]
                        println(s"I director OneWayVehicle2L clock executed at this time: ${director.clock}")
                        rowManager.arrivalCount(rowManager.curRow) += 1 //just a count
                        println("I worked too")
                        if i < units then
                            println(s"I worked too $i")
                            val mu = rowManager.muVal(rowManager.curRow)// should be curRow
                            ew.write(s"\n mu $mu and i_unit = \n")
                            val gen = if iArrivalTime.isInstanceOf[Erlang] then iArrivalTime.gen1(mu/2) else iArrivalTime.gen1(mu)
                            println("I worked too muchhh")
                            val duration = gen
                            //val duration = iArrivalTime
                            val ctime    = director.clock                        // clock time
                            tally (duration)                                     // tally duration

                            //record (actor, ctime)                                // record actor flow
                            schedule (duration)
                            debug ("act", s"actor $i yields to director")
                            yieldToDirector ()                                   // yield and wait duration time units
                            debug ("act", s"after yield actor $i")
                end for
            } // breakable

            ew.flush()
            if rep < director.reps then
                director.log.trace (this, "wait for next rep", director, director.clock)
                yieldToDirector ()                                           // yield and wait for next replication
        end for

        director.log.trace (this, "terminates", null, director.clock)
        yieldToDirector (true)                                               // yield and terminate
    end act

end VSource


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VSource` companion object provides a builder method for sources.
 */
object VSource:

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
    def apply (name: String, director: Model, makeEntity: () => Vehicle, esubtype: Int, units: Int,
               iArrivalTime: Variate, xy: (Int, Int)): VSource =
        new VSource (name, director, makeEntity, esubtype, units, iArrivalTime,
                    Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0))
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related sources using defaults for width 'w' and height 'h'.
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param units         the number of entities to make
     *  @param xy            the (x, y) coordinates for the top-left corner of the reference source.
     *  @param src           repeated source specific info: name, subtype, distribution, offset
     */
    def group (director: Model, makeEntity: () => Vehicle, units: Int, xy: (Int, Int),
               src: (String, Int, Variate, (Int, Int))*): List [VSource] =
        val sourceGroup = new VEC [VSource] ()
        for s <- src do sourceGroup += VSource (s._1, director, makeEntity, s._2, units, s._3,
                                              (xy._1 + s._4._1, xy._2 + s._4._2))
        sourceGroup.toList
    end group

end VSource


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `vSourceTest` main function tests the `VSource` class by generating several Car objects.
 *  > runMain scalation.simulation.process.vSourceTest
 */
@main def vSourceTest (): Unit =

    import scalation.random.Uniform

    object CarModel extends Model ("CarModel"):

        val maker = VSource ("maker", CarModel, () => Car (), 0, 5, Uniform (2000, 5000), (100, 200))

        addComponent (maker)

        case class Car () extends Vehicle ("c", CarModel):
            override def act (): Unit = println ("act")
        end Car

    end CarModel

    CarModel.simulate ()
    CarModel.waitFinished ()
    Model.shutdown ()

end vSourceTest

