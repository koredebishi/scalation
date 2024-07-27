
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jan 25 19:44:16 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Path for Modeling Multi-Lane Pathway
 */

package scalation
package simulation
package process

import scala.math.hypot
import scalation.animation.CommandType.*
import scalation.mathstat.VectorD
import scalation.random.Variate
import scalation.scala2d.Colors.*
import scala.reflect.ClassTag


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Path` class provides a multi-lane pathway between two other components.
 *  The components in a `Model` conceptually form a graph in which the edges
 *  are `Transport`s or `VTransport`s and the nodes are other `Component`s.
 *  A `Path` is a composite component that bundles several `Transport`s or `VTransport`s.
 *  @param name      the name of the path
 *  @param k         the number of lanes/transports in the path
 *  @param from      the starting component
 *  @param to        the ending component
 *  @param motion    the variate or dynamics model for the speed/trip-time for motion down the `Path`
 *  @param isSpeed   whether speed or trip-time is used for motion
 *  @param bend      the bend or curvature of the `Path` (0 => line)
 */
class Route (name: String ="Route", k: Int, val from: Component, val to: Component,
            motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0)
    extends Component:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val debug = debugf("Route", true) // debug function
    private val flaw = flawf("Route") // flaw function


    val lane = Array.ofDim[VTransport](k)  // declare the lane array first


    private val GAP = 20.0 // gap between lanes
    private val delta = calcShift // amount of shift in x and y directions


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Calculate the amount of shift in the x and y directions.
     */
    private def calcShift: VectorD =
        val xdist = from.at(0) - to.at(0)
        val ydist = from.at(1) - to.at(1)

        val hyp = hypot(xdist, ydist)
        println(s"${Console.RED} xdist= $xdist, ydist= $ydist, hyp=$hyp ${Console.RESET}")
        VectorD((ydist / hyp) * GAP, -(xdist / hyp) * GAP)
    end calcShift


    debug("init", s"name = $name, from = ${from.name}, to = ${to.name}, delta = $delta")

    for i <- lane.indices do
        val shift = VectorD((i - (k - 1) / 2.0) * delta(0), (i - (k - 1) / 2.0) * delta(1))
        lane(i) = new VTransport(s"${name}_$i", from, to, motion, isSpeed, bend, shift, shift)
        subpart += lane(i)  // Add each lanes to subpart
    end for

    //Initialize the components now after the array is populated
    initComponent(name, Array())




    /** Return the number of lanes.
     */
    def lanes: Int =  k

    /** Give the location of the curve to be its starting point.
     */
    override def at: Array[Double] = lane(0).at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the direction/turn random variate to determine next the direction.
     *  This allows an application model to select the next component.
     *  The decision is delegated to this path's lane(0) transport.
     */
    def selector: Variate = lane(0).selector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the direction/turn random variate in this path's lane(0) transport.
     *  @param selectorRV  the random variate used to select the direction
     */
    def selector_= (selector: Variate): Unit = lane(0).selector = selector




    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this `Transport`.
     */
    def display (): Unit =
        //      director.animate (this, CreateEdge, blue, QCurve (), from, to, Array (bend))
        for l <- lane do
            director.animate (l, CreateEdge, blue, l.curve, l.from, l.to,
                Array (l.p1(0), l.p1(1), l.pc(0), l.pc(1), l.p2(0), l.p2(1)))
    end display

end Route



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `pathTest` main function is used to test the `Path` class, which is a composite
 *  class.  It simulates a two-lane road in one direction.
 *  > runMain scalation.simulation.process.routeTest
 */
@main def routeTest (): Unit =

    import scalation.random.{Bernoulli, Uniform}


    class RouteModel (name: String, nArrivals: Int, iArrivalRV: Variate, motion: Dynamics)
        extends Model (name):


        val rng     = Bernoulli ()
        val onRamp  = new Source ("onRamp", this, () => Car (), 0, nArrivals, iArrivalRV, (100.0, 200.0))
        val offRamp = new Sink ("offRamp", (400.0, 200.0))
        val road    = new Route ("lane", 2, onRamp, offRamp, motion, false)



        println("I have just executed")

        addComponent (onRamp, offRamp, road)


        case class Car () extends SimActor ("c", this):

            override def act (): Unit =
                val i = rng.igen             // choose a lane
                road.lane(i).move ()         // move along the lane
                offRamp.leave ()             // exit
            end act

        end Car

    end RouteModel

    val motion = GippsDynamics

    val rm = new RouteModel ("road", 10, Uniform (4000, 6000), motion)

    rm.simulate ()
    Model.shutdown ()

end routeTest


