//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Casey Bowman
 *  @version 1.6
 *  @date    Mon Nov 2 20:50:23 EST 2020
 *  @see     LICENSE (MIT style license file).
 */


package scalation
package simulation
package process

import math.{abs, floor, min, sqrt,hypot}
import java.io.*
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import scalation.animation.CommandType.*
import scalation.mathstat.VectorD
import scalation.mathstat.Plot
import scalation.random.{Discrete, Sharp, Uniform, Variate}
import scalation.scala2d.{Line, QCurve}
import scalation.scala2d.QCurve.calcControlPoint
import scalation.scala2d.Colors
import scalation.scala2d.Colors.blue

import java.awt.geom.Point2D.Double as R2


class Highway (name: String, val from: Component, val to: Component, numLanes: Int,
                ratio: Double = 1.0)
      extends Component:

    initComponent (name, Array ())
    val lanes    = Array.ofDim [Road] (numLanes)
    val hw_width = 25.0
    val nudges   = calcNudges ()
    val fromAt   = from.at
    val toAt     = to.at
    val p1       = R2(fromAt(0), fromAt(1))
    val p2       = R2(toAt(0),    to.at(1))
    private val GAP   = 10.0                        // gap between lanes
    private val delta = calcShift                   // amount of shift in x and y directions
    //val moveRV   = Uniform(7.0,9.0)

    val length = sqrt((p2.getX() - p1.getX()) * (p2.getX() - p1.getX()) + (p2.getY() - p1.getY()) * (p2.getY() - p1.getY()))

//    println (name + ", length = " + length)

    for i <- 0 until numLanes do
        val shift = VectorD ((i - (numLanes - 1) / 2.0) * delta(0), (i - (numLanes - 1) / 2.0) * delta(1))
        lanes(i) = new Road (name + "_" + i, from, to, null,shift1 = shift,shift2 =shift)
        //vmax, deltaT, nudge = nudges(i), ratio, length)
        subpart +=lanes(i)
    end for


    def move (i: Int = 0, x: Double = 0.0): Unit = lanes(i).move(x)


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Calculate the amount of shift in the x and y directions.
     */
    private def calcShift: VectorD =
        val xdist = from.at(0) - to.at(0)
        val ydist = from.at(1) - to.at(1)
        val hyp = hypot(xdist, ydist)
        VectorD((ydist / hyp) * GAP, -(xdist / hyp) * GAP)
    end calcShift

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Give the location of the curve to be its starting point.
     */
    override def at: Array[Double] = lanes(0).at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Tell the animation engine to display this `Transport`.
     */
    def display(): Unit =
    //      director.animate (this, CreateEdge, blue, QCurve (), from, to, Array (bend))
        for l <- lanes do
            director.animate(l, CreateEdge, blue, l.curve, l.from, l.to,
                Array(l.p1(0), l.p1(1), l.pc(0), l.pc(1), l.p2(0), l.p2(1)))
        end for
    end display

    def calcNudges (): Array[Double] =

        val λ  = hw_width / (numLanes - 1)
        val x0 = -hw_width / 2.0
        val nudges = Array.ofDim [Double] (numLanes)
        nudges(0) = x0
        for (i <- 1 until numLanes) nudges(i) = nudges(i - 1) + λ
        nudges
    end calcNudges

end Highway






