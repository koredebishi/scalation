
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Tue Sep 21 09:30:30 EDT 2021
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package simulation
package process

import scalation.animation.CommandType.*
import scalation.mathstat._
//import scalation.simulation.agent.{Model, Sink, Vehicle}

import java.io.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

/** The `Exit` class (a sink plus a sensor) is to
 *  @param name      the name of this sink
 *  @param director  the director controlling the model
 *  @param at       the position (Euclidean coordinate) of this exit
 *  @param mm        Mile mark, where on the road to take this exit.
 *  @param nt        how many data points per time interval. e.g.  there are 288 data points in 24 hours time resolution of 5 minutes ;
 */
class Exit (name: String,  at: Array [Double],nt: Int, mm:Double=Double.MaxValue )
      extends Sink (name, at):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name  the name of the sink
     *  @param xy    the (x, y) coordinates for the top-left corner of the sink.
     */
    def this (name: String, xy: (Double, Double), mm:Double, nt: Int) =
        this (name, Array (xy._1, xy._2, 20.0, 20.0), nt, mm)
    end this

    val counts = new VectorD (nt)  //96 is column 24 hours per day, 15 minute , there 96 x 15 mins

    val speeds = new VectorD (nt)

    def leave (agent: Vehicle): Unit =
        val i = Math.floor (director.clock / timeConv).toInt //count the i-th
        val cnt = counts(i) + 1
        counts(i) = cnt
        speeds(i) = (speeds(i) * (cnt - 1) + agent.velocity) / cnt
        super.leave()
    end leave

    def getCounts: VectorD = counts

    def getSpeeds: VectorD = speeds

end Exit

