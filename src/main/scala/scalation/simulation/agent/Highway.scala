//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Casey Bowman
 *  @version 1.6
 *  @date    Mon Nov 2 20:50:23 EST 2020
 *  @see     LICENSE (MIT style license file).
 */


package scalation
package simulation.agent

import math.{abs, floor, hypot, min, sqrt}
import java.io.*
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import scalation.animation.CommandType.*
import scalation.database.graph.Vertex
import scalation.mathstat.VectorD
import scalation.mathstat.Plot
import scalation.random.{Discrete, Sharp, Uniform, Variate}
import scalation.scala2d.{Line, QCurve, R2}
import scalation.scala2d.QCurve.calcControlPoint
import scalation.scala2d.Colors
import scalation.simulation.Identifiable
//import scalation.util.Monitor.trace

class Highway (_name: String, director: Model, val from: Vertex, val to: Vertex, numLanes: Int,
               vmax: Double, deltaT: Double = 1.0, ratio: Double = 1.0)
      extends Identifiable ():

    name = _name
    val lanes = Array.ofDim [Road] (numLanes)
    val hw_width = 25.0
    val nudges = calcNudges ()
    private val GAP = 10.0 // gap between lanes
    private val delta = calcShift // amount of shift in x and y directions


    val length = sqrt((from.pos(0) - to.pos(0))*(from.pos(0) - to.pos(0))
                 + (from.pos(1) - to.pos(1))*(from.pos(1) - to.pos(1)))

//    println (name + ", length = " + length)


    for i <- 0 until numLanes do
        val shift = VectorD((i - (numLanes - 1) / 2.0) * delta(0), (i - (numLanes - 1) / 2.0) * delta(1))


        lanes(i) = new Road(_name + "_" + i, director,from, to, null,
                            shift1 = shift, shift2 = shift)
        //println(s"we creating $i th road with $shift")
        //vmax, deltaT, nudge = nudges(i), ratio, length)
        //subpart += lanes(i) //it will auto add
    end for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** move an agent on a target road.
     * @i     :the target road
     * @agent :the agent
     */
    def move(i: Int, agent: Vehicle, x: Double = 0.0): Unit = lanes(i).move(agent,x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the amount of shift in the x and y directions.
     */
    private def calcShift: VectorD =
        val xdist = from.pos(0) - to.pos(0)
        val ydist = from.pos(1) - to.pos(1)
        val hyp = hypot(xdist, ydist)
        VectorD((ydist / hyp) * GAP, -(xdist / hyp) * GAP)
    end calcShift

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Give the location of the curve to be its starting point.
     */
    //override def at: Array[Double] = lanes(0).at

    def display(): Unit = ??? //no need in agent schema?


    def calcNudges (): Array[Double] =

        val λ  = hw_width / (numLanes - 1)
        val x0 = -hw_width / 2.0
        val nudges = Array.ofDim [Double] (numLanes)
        nudges(0) = x0
        for (i <- 1 until numLanes) nudges(i) = nudges(i - 1) + λ
        nudges
    end calcNudges

end Highway






