
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 1.6
 *  @date    Tue Feb  4 14:56:34 EST 2020
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package simulation.agent

import math.{abs, atan2, floor, min, sqrt}
import java.io.*
import scala.collection.mutable.{ArrayBuffer, Queue}
import scalation.animation.CommandType.*
import scalation.mathstat.VectorD
import scalation.mathstat.Plot
import scalation.random.{Discrete, Sharp, Uniform, Variate}
import scalation.scala2d.{Line, QCurve, R2}
import scalation.scala2d.QCurve.calcControlPoint
import scalation.scala2d.Colors
//import scalation.util.Monitor.trace
import scalation.database.graph.Vertex
import java.awt.geom.Point2D.Double as R2

class Road (name: String, director: Model, from: Vertex, to: Vertex,
            moveRV: Variate = null, prop: Property = null,
            shift1: VectorD = VectorD (0.0, 0.0), shift2: VectorD = VectorD (0.0, 0.0),
            shift: Int = 0,
            nudge: Double = 0.0,
            ratio: Double = 1.0, var length: Double = -1.0)
      extends Transport (name,director,from,to, moveRV, prop, shift1, shift2, shift):

    val fromAt   = from.pos
    val toAt     = to.pos
    val p1       = R2(fromAt(0) + shift1(0) + fromAt (2) / 2, fromAt(1) + shift2(0) + fromAt(3) / 2)
    val p2       = R2(toAt(0) + shift1(1)  + toAt(2) / 2,    to.pos(1)+ shift2(1)  + toAt(3) / 2)
    private val pc = calcEndPoints
    //private val pp1 = pp._1 + shift1
    private val diff = to.pos(0 to 2) - from.pos(0 to 2) // the difference between the end points
    private val distance = diff.norm // the distance from p1 to p2 (length of transport)
    private val angle       = atan2 (diff(1), diff(0))             // the angle from p1 to p2

    val m      = (p1.getY() - p2.getY()) / (p1.getX() - p2.getX())

//    var pw: PrintWriter = null

    if nudge != 0.0 then
        if m < 0.0 then nudgeAt (-nudge)
        if m > 0.0 then nudgeAt (nudge)
    end if

    if length < 0.0 then length = sqrt((p2.getX() - p1.getX()) * (p2.getX() - p1.getX()) + (p2.getY() - p1.getY()) * (p2.getY() - p1.getY()))




    //override def at: Array [Double] = Array (p1.getX(), p1.getY())


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move the entity (Vehicle) smoothly down this Transport.  Repeatedely
     *  move it along the Transport/Edge/QCurve.  Caveat: tokens coordinates
     *  are computed using a shadow QCurve (same coordinates as the one that
     *  will be created by the animation engine).
     */
    def move ( agent: Vehicle, x: Double ): Unit =
        agent.disp = x
        agent.lastLoc = 0.0 //Yulong added to reset each separate road or lane or transport segment
        agent.setPos (pc._1(0), pc._1(1))
        tallyStats (agent.time)
//        agent.println ("I am on road " + this.name + " and my .t_disp is " + agent.t_disp)
//        println (agent.name + " before while")
        while agent.disp < ratio * length && !agent.done do
            director.log.trace (this, "moves for " + agent.time, agent, director.clock)
            agent.update()
//            println (agent.name + " x = " + agent.disp + ", road = " + this.name + ", road length = " + (ratio * length))
            //calcPoint (agent.disp / ratio)

            val step = agent.disp-agent.lastLoc
            agent.updateLoc((agent.disp-agent.lastLoc)/ ratio)

            agent.updatePos ((agent.disp-agent.lastLoc)/ ratio,angle)
            //if agent.me =="Car.c_1.47" then
            //    println(s"move with $step")
            agent.lastLoc = agent.disp
            director.animate (agent, MoveToken)
            //println("got here?")
            /*if agent.dest.mm <= agent.disp then
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

            director.schedule (agent,agent.τ)
//            if (agent.name.equals("c_3005")) println (agent.name + ": before yield")
            agent.yieldToDirector ()
        end while
    end move

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the (x,y) point in the simulation space for the vehicle.
     *  @param s  the current displacement along the road of the vehicle.
     */

    def calcPoint (s: Double): Array[Double] =
        val prop = s / length
        /** Radius of a token (for animating entities)
         */
        val RAD = 5.0
        val x = p1.getX() + (p2.getX() - p1.getX()) * prop
        val y = p1.getY() + (p2.getY() - p1.getY()) * prop
        Array (x - RAD, y - RAD)
    end calcPoint

    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Method to nudge the road over a little bit for use with multi-lane 
     *  highways. If nudge fagent is zero then the road will not be nudged.
     *  @param λ  the nudge fagent
     */
    def nudgeAt (λ: Double): Unit =
        val a1 = p1.getX()
        val b1 = p1.getY()
        val a2 = p2.getX()
        val b2 = p2.getY()
        if m == Double.PositiveInfinity || m == Double.NegativeInfinity then
            p1.setLocation (a1 + λ, b1)
            p2.setLocation (a2 + λ, b2)
        else if m == 0.0 then
            p1.setLocation (a1, b1 + λ)
            p2.setLocation (a2, b2 + λ)
        else
            val α  = λ * Math.sqrt (1.0 / (m * m + 1))        
            val c1 = a1 - m * α
            val d1 = b1 + α
            val c2 = a2 - m * α
            val d2 = b2 + α
            p1.setLocation (c1, d1)
            p2.setLocation (c2, d2)
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

@main def RoadTest (): Unit=
    new RoadModel ("Road", 10, Uniform (5, 10), 30.0, 25.0, 40.0)

    class RoadModel (name: String, nArrivals: Int, iArrivalRV: Variate, speedLimit: Double, green: Double, red: Double)
      extends Model (name, animating = true, aniRatio = 150.0): //, width = 1500, height = 800
        log.trace (this, "hello", this, 0)

        val onRamp  = new Source ("entry", this,0.0, iArrivalRV,
            () => Car (), nArrivals,0,   pos = Source.at(500.0, 200.0))
        val offRamp = new Sink ("exit",this, pos= Sink.at(500.0, 600.0)) //2D pos can work
        val wq = new WaitQueue("w8q", this,pos = WaitQueue.at(500.0, 280)) //2D pos can work
        val onTimeV = Sharp (500,5)
        val offTimeV = Sharp(600,6)
        val moveRV = Uniform (20,7)
        val light   = new Gate ("Light", this, 0.0, wq, onTimeV,offTimeV,
            pos = Gate.at(500.0, 300.0))
        val road    = new Transport("road", this,  onRamp.vert, light.vert, moveRV)
        //val road2   = new Transport ("road2",this,  light.vert, offRamp, moveRV)
        val road2   = new Road ("road2",this,  light.vert, offRamp, moveRV)


        case class Car () extends Vehicle ("c",director.clock,  this):

            def act (): Unit =
                road.move (this,10,1)
                road2.move (this,10,1)
                offRamp.leave (this)
            end act
        end Car
        simulate ()
        waitFinished ()
        Model.shutdown ()

    end RoadModel

end RoadTest
/*
@main def RoadTest2 (): Unit=
    val rm = new RoadModel ("Road", 10, Uniform (5, 10), 30.0, 25.0, 40.0)
    rm.simulate ()
    rm.waitFinished ()
    Model.shutdown ()

    class RoadModel (name: String, nArrivals: Int, iArrivalRV: Variate, speedLimit: Double, green: Double, red: Double)
      extends Model (name, animating = false, aniRatio = 100.0, width = 10000, height = 7000)
    {
        log.trace (this, "hello", this, 0)



        val onRamp  = new Source ("onRamp", this, Car, 0, nArrivals, iArrivalRV, (500.0, 100.0))
        val offRamp = new Sink ("offRamp", (500.0, 6000.0))
        val road    = new Road ("road",  onRamp, offRamp, speedLimit, 1.0)

        var count = 0

        addComponent (onRamp, offRamp, road)

        case class Car () extends Vehicle ("c_" + count, this)
        {
            def act (): Unit =
            {
                count += 1
                //                println ()
                //                println (this.name + ": pred = " + this.pred)
                //                println ()
                road.move ()
                offRamp.leave ()
            }
        }
    }
end RoadTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** RoadTest3 attempts to use Junctions as sensors in an
 *  interstate situation.
 *  Note:  It would be advantageous to develop an application to
 *         take GPS coordinates from csv files and create the
 *         road structure from them.
 */
@main def RoadTest3 (): Unit=
    val rm = new RoadModel ("Road", 10, Uniform (5, 10), 30.0)
    rm.simulate ()
    rm.waitFinished ()
    Model.shutdown ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** RoadModel class creates the simulation of the traffic system.
     *  Note: The inter-arrival variate should be updated to use an
     *        NHPP based on the traffic volume data in the data files
     */
    class RoadModel (name: String, nArrivals: Int, iArrivalRV: Variate, speedLimit: Double)
      extends Model (name, animating = true, aniRatio = 100.0, width = 10000, height = 7000)
    {

        val pw = new PrintWriter (new FileWriter (new File ("counts.txt")))

        val source = new Source   ("SOURCE", this, Car, 0, nArrivals, iArrivalRV, (50.0, 50.0))
        val sink   = new Sink     ("SINK", (500.0, 600.0))
        val S_1    = new Junction ("S_1", this, Sharp (0.0), (100.0, 150.0))
        val S_2    = new Junction ("S_2", this, Sharp (0.0), (250.0, 200.0))
        val R_1    = new Road     ("R_1", source, S_1,  speedLimit, 1.0)
        val R_2    = new Road     ("R_2", S_1,    S_2,  speedLimit, 1.0)
        val R_3    = new Road     ("R_3", S_2,    sink, speedLimit, 1.0)

        addComponent (source, sink, S_1, S_2, R_1, R_2, R_3)

        case class Car () extends Vehicle ("c", this)
        {
            def act (): Unit =
            {
                R_1.move ()
                S_1.jump (pw)
                R_2.move ()
                S_2.jump (pw)
                R_3.move ()
                sink.leave ()
            }
        }
    }
end RoadTest3

@main def RoadTest4 (): Unit=
    val rm = new RoadModel ("Road", 10, Uniform (5, 10), 30.0)
    rm.simulate ()
    rm.waitFinished ()
    rm.printCarNames ()
    Model.shutdown ()

    class RoadModel (name: String, nArrivals: Int, iArrivalRV: Variate, speedLimit: Double)
      extends Model (name, animating = true, aniRatio = 100.0, weight = 10000, width = 7000)
    {
        val cars   = new ArrayBuffer [Vehicle] ()
        val source = new Source   ("SOURCE",   this, Car, 0, nArrivals, iArrivalRV, (50.0, 50.0))
        val sink   = new Sink     ("SINK",     (500.0, 600.0))
        val junc   = new Junction ("JUNCTION", this,   Sharp (0.0), (200.0, 200.0))
        val road1  = new Road     ("ROAD1",    source, junc, speedLimit, 1.0)
        val road2  = new Road     ("ROAD2",    junc,   sink, speedLimit, 1.0)

        var count = 0

        addComponent (source, sink, junc, road1, road2)

        def printCarNames (): Unit = { for (c <- cars) println (c.name) }

        case class Car () extends Vehicle ("c_" + count, this)
        {
            def act (): Unit =
            {
                count += 1
                cars += this
                road1.move()
                junc.jump ()
                road2.move()
                sink.leave()
                cars.remove (0)
            }
        }
    }
end RoadTest4


@main def RoadTest5 (): Unit=
    val t1 = VectorD.range (1,20) * 2
    val t2 = VectorD.range (1,10) * 4
    val rm = new RoadModel ("Road", 20, 10, SharpV (t1), SharpV (t2))
    rm.simulate ()
    rm.waitFinished ()
    Model.shutdown ()

    class RoadModel (name: String, nArrivals1: Int, nArrivals2: Int, iArrivalRV1: Variate, iArrivalRV2: Variate)
      extends Model (name, animating = true, aniRatio = 1000.0)
    {
        val src     = new Source ("src",    this, Car1, 0, nArrivals1, iArrivalRV1, (100.0, 200.0))
        val onRamp  = new Source ("onRamp", this, Car2, 0, nArrivals2, iArrivalRV2, (150.0, 250.0))
        val offRamp = new Sink   ("offRamp", (2000.0, 200.0))
        val road    = new Road   ("road", src, offRamp, 33.528, 1.0)

        var count1 = 0
        var count2 = 0

        addComponent (src, onRamp, offRamp, road)        // Caveat: must add from and to before transport!!

        case class Car1 () extends Vehicle ("c1_" + count1, this)
        {
            def act (): Unit =
            {
                count1 += 1
                this.velocity = 33.258
                road.move ()
                offRamp.leave ()
            } // act

        } // Car1

        case class Car2 () extends Vehicle ("c2_" + count2, this)
        {
            def act (): Unit =
            {
                count2 += 1
                this.velocity = 33.258
                road.move (100.0)
                offRamp.leave ()
            }
        }

    } // RoadModel class

end RoadTest5


*/