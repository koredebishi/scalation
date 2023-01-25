
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 1.6
 *  @date    Tue Sep 21 09:30:30 EDT 2021
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package simulation.agent

import scalation.animation.CommandType.*
import scalation.mathstat.VectorD
//import scalation.simulation.agent.{Model, Sink, Vehicle}

import java.io.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Exit` class is to
 *  @param name      the name of this sink
 *  @param director  the director controlling the model
 *  @param prop      the properties of this exit
 *  @param pos       the position (Euclidean coordinate) of this exit
 *  @param mm        Mile mark, where on the road to take this exit.
 *  @param nt        how many data points per time interval. e.g.  there are 288 data points in 24 hours time resolution of 5 minutes ;
 */
class Exit (name: String, director:Model, prop: Property = null, pos: VectorD, mm:Double, nt: Int = 288)
      extends Sink (name, director, prop, pos):

    val counts = new VectorD(nt) //96 is column 24 hours per day, 15 minute , there 96 x 15 mins

    val speeds = new VectorD(nt)

    def leave(agent: Vehicle): Unit =
        val i = Math.floor(director.clock / timeConv).toInt //count the i-th
        val cnt = counts(i) + 1
        counts(i) = cnt
        speeds(i) = (speeds(i) * (cnt - 1) + agent.velocity) / cnt
        super.leave(agent)
    end leave

    def getCounts: VectorD = counts

    def getSpeeds: VectorD = speeds

end Exit

