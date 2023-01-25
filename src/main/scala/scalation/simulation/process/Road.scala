//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 1.6
 *  @date    Tue Feb  4 14:56:34 EST 2020
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package simulation
package process

import math.{abs, floor, min, sqrt}

import java.io._

import scala.collection.mutable.{ArrayBuffer, Queue}

import scalation.animation.CommandType._
import scalation.mathstat.VectorD
import scalation.mathstat.Plot
import scalation.random.{Discrete, Sharp, Uniform, Variate}
import scalation.scala2d.{Line, QCurve}
import scalation.scala2d.QCurve.calcControlPoint
import scalation.scala2d.Colors
//import scalation.util.Monitor.trace
import scalation.database.graph.Vertex
import java.awt.geom.Point2D.Double as R2

class Road (name: String, from: Component, to: Component,
            moveRV: Variate = null, prop: Property = null,
            shift1: VectorD = VectorD (0.0, 0.0), shift2: VectorD = VectorD (0.0, 0.0),
            shift: Int = 0,
            nudge: Double = 0.0,
            ratio: Double = 1.0, var length: Double = -1.0)
      extends Transport (name,from, to, moveRV,shift1 = shift1,shift2 = shift2):

    val fromAt   = from.at
    val toAt     = to.at
    val pp1       = R2(fromAt(0) + fromAt (2) / 2 +shift1(0), fromAt(1) + fromAt(3) / 2+shift1(1))
    val pp2       = R2(toAt(0)   + toAt(2) / 2+shift2(0),    to.at(1)  + toAt(3) / 2 +shift2(1))

    val m      = (pp1.getY() - pp2.getY()) / (pp1.getX() - pp2.getX())

//    var pw: PrintWriter = null

    if nudge != 0.0 then
        if m < 0.0 then nudgeAt (-nudge)
        if m > 0.0 then nudgeAt (nudge)
    end if

    if length < 0.0 then length = sqrt((pp2.getX() - pp1.getX()) * (pp2.getX() - pp1.getX()) + (pp2.getY() - pp1.getY()) * (pp2.getY() - pp1.getY()))


    //override def at: Array [Double] = Array (pp1.getX(), pp1.getY())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move the entity (Vehicle) smoothly down this Transport.  Repeatedely
     *  move it along the Transport/Edge/QCurve.  Caveat: tokens coordinates
     *  are computed using a shadow QCurve (same coordinates as the one that
     *  will be created by the animation engine).
     */
    def move ( x: Double): Unit =
        val timeStart = director.clock
        val actor = director.theActor.asInstanceOf [Vehicle]
        actor.disp = x

//        actor.println ("I am on road " + this.name + " and my .t_disp is " + actor.t_disp)
//        println (actor.name + " before while")
        while actor.disp < ratio * length && !actor.done do
            director.log.trace (this, "moves for " + actor.τ, actor, director.clock)
            actor.update()
//            println (actor.name + " x = " + actor.disp + ", road = " + this.name + ", road length = " + (ratio * length))
            director.animate (actor, MoveToken, null, null, calcPoint (actor.disp / ratio))
            /*if agent.dest.mm <= agent.disp then //these are for on ramp
                agent.done = true
                /*
                var p: Vehicle = null
                var s: Vehicle = null
                if agent.pred != null then p = agent.pred
                if agent.succ != null then s = agent.succ
                if p != null && s != null then
                    p.succ = s
                    s.pred = p
                else if p != null && s == null then
                    p.succ = null
                else if p == null && s != null then
                    s.pred = null
                end if
                */
                val (p,s) = (agent.pred, agent.succ)
                if p != null then p.succ = s
                if s != null then s.pred = p
                agent.pred = null
                agent.succ = null
            end if
            */

            actor.schedule (actor.τ)
//            if (actor.name.equals("c_3005")) println (actor.name + ": before yield")
            actor.yieldToDirector ()
        end while
        tally(director.clock - timeStart)
    end move

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the (x,y) point in the simulation space for the vehicle.
     *  @param s  the current displacement along the road of the vehicle.
     */
    def calcPoint (s: Double): Array[Double] =
        val prop = s / length
        val x = pp1.getX() + (pp2.getX() - pp1.getX()) * prop
        val y = pp1.getY() + (pp2.getY() - pp1.getY()) * prop
        Array (x - RAD, y - RAD)
    end calcPoint


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Method to nudge the road over a little bit for use with multi-lane
     *  highways. If nudge fagent is zero then the road will not be nudged.
     *  @param λ  the nudge fagent
     */
    def nudgeAt (λ: Double): Unit =
        val a1 = pp1.getX()
        val b1 = pp1.getY()
        val a2 = pp2.getX()
        val b2 = pp2.getY()
        if m == Double.PositiveInfinity || m == Double.NegativeInfinity then
            pp1.setLocation (a1 + λ, b1)
            pp2.setLocation (a2 + λ, b2)
        else if m == 0.0 then
            pp1.setLocation (a1, b1 + λ)
            pp2.setLocation (a2, b2 + λ)
        else
            val α  = λ * Math.sqrt (1.0 / (m * m + 1))
            val c1 = a1 - m * α
            val d1 = b1 + α
            val c2 = a2 - m * α
            val d2 = b2 + α
            pp1.setLocation (c1, d1)
            pp2.setLocation (c2, d2)
        end if
    end nudgeAt


//    def openPrint (path: String) { pw = new PrintWriter (new FileWriter (new File (path))) }

/*    def println (s: String)
    {
        if (pw != null) {
            pw.println (s)
            pw.flush ()
        }
    }

    def closePrint () { pw.close() }
*/
end Road