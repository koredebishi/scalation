//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Tue Feb  4 14:56:34 EST 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Vehicle Is Enhanced SimActor Supporting Changing Velocities
 */

package scalation
package simulation
package process


import scala.collection.mutable.Map
import scalation.random.Uniform
import scalation.mathstat.VectorD
import scalation.scala2d.Colors.Color



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vehicle` class extends the `SimActor` and represents a vehicle in motion.
 *  @param name_     the name of the vehicle
 *  @param director  the model to which this vehicle belongs
 */
abstract class Vehicle (name_ : String, director: Model)
    extends SimActor (name_, director)
        with Dynamics:

    // Vehicle's target free-flow speed (m/s). Set at creation from PEMS data.]
    //What this is doing is counter intuitive - it is setting the generic vmax to a random value
    // I get the logic but we do not need this as it's introducing bug into our code because IDM
    // is using it directly  in Dynamics line
    //       var a = iDM(amax, b, len, car.vmax, x_n, v_n,
    //                        x_leader, v_leader, T, s, del)
    // So it's blockin our set pems speed.
    // this is only valuable for generic simulation where we want to randomize vehicle speeds. where
    //some vehicles are fast some are slow. (trucs and cars)
    //Hower, we need this overriden in VSource when we set the pems speed. so we can cars not going above pems speed.
    var vmax = Vehicle.speedGen.gen    // this is for generic. needs pems version.


    var key  = -0.0
    var laneID: Int = -1
    var pathInfo : String = ""
    var segId : Int = -1
    var prevSegId: Int = -1                // previous segment id (before current segId)
    var segmentEnterTime: Double = -1.0    // simulation clock when entering current segment


    // Derived: live time spent on current segment (call with director.clock)
    def segmentTravelTime(now: Double): Double =
        if segmentEnterTime >= 0.0 then now - segmentEnterTime else 0.0
    end segmentTravelTime


    // Human-friendly label to use in logs/console; animator label should match this.
    private[process] var displayLabel: String = ""
    def setDisplayLabel(lbl: String): Unit =
        if lbl != null then displayLabel = lbl
    end setDisplayLabel


    var myRamp        : Ramp        = null   // current ramp this vehicle is on (null if not on a ramp)
    var myPathway     : Pathway     = null   // current pathway/lane this vehicle is on (null if not on a pathway)
    var myFFConnector : FFConnector = null   // current FF connector this vehicle is on (null if not on an FF)
    private [process] var myPathNode: DoublyLinkedList[Vehicle]#Node = null   // DLL node: pred <-> me <-> succ



    inline def getCarAhead(car: Vehicle): Vehicle =
        val ref = car.myPathNode.ahead
        if ref != null then ref.elem else null
    end getCarAhead
    
    inline def getCarBehind(car: Vehicle): Vehicle =
        val ref = car.myPathNode.behind
        if ref != null then ref.elem else null
    end getCarBehind
    
    inline def gapToLeader: Double =
        val leader = getCarAhead(this)
        if leader != null then leader.disp - this.disp - Vehicle.len else 0.0
    end gapToLeader
        


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The abstract method, 'act', is defined in each subclass to provide specific
     *  behavior.
     */
    override def act (): Unit = println (s"Vehicle.act method should be overridden")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the `SimActor`s full name and activation time.
     */
    override def toString: String =
        val label = if displayLabel != null && displayLabel.nonEmpty then displayLabel else me
        s"Vehicle($label )"//=$id segId=$segId prevSeg=$prevSegId disp=$disp tDisp=$t_disp  enterT=$segmentEnterTime path=$pathInfo)"
end Vehicle



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vehicle` object contains driver/vehicle characteristics/properties.
 */
object Vehicle:

    val speedGen = Uniform(20.0 , 40.0)

    /** defaults values for driver/vehicle characteristics/properties (PUBLIC access required)
     *  @see https://en.wikipedia.org/wiki/Intelligent_driver_model
     */
    val def_prop = Map ("rt"   -> 0.5,                       // driver reaction time
        "amax" -> 4.0,                       // max acceleration
        "bmax" -> -2.0,                      // comfortable deceleration (IDM s* formula) use to be -1.5
        "b_emergency" -> -9.0,               // physical max braking (0.9g) — clamp floor for all CFMs
        "v0"   -> 4.0,                       // starting velocity // v0 should be adjustable to 0
        "vmax" -> 33.528,                    // max velocity
        "T"    -> 3.0,                       // safe min time headway
        "s"    -> 4.0,                       // safe min distance headway (bumped from 5.0 for visual spacing)
        "len"  -> 4.0,                       // length of the vehicles
        "del"  -> 4.0)                       // acceleration exponent (delta)

    /** current values for driver/vehicle characteristics/properties
     */
    private [process] var prop = def_prop


    //    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current value for 'prop("rt") the driver reaction time
     */
    inline def rt: Double   = prop("rt")                     // driver reaction time
    inline def amax: Double = prop("amax")                   // max acceleration
    inline def bmax: Double = prop("bmax")                   // comfortable deceleration (IDM s* formula)
    inline def b_emergency: Double = prop("b_emergency")     // physical max braking (0.9g) — clamp floor
    inline def v0: Double   = prop("v0")                     // starting velocity
    inline def vmax: Double = prop("vmax")                   // max velocity
    inline def T: Double    = prop("T")                      // min time headway
    inline def s: Double    = prop("s")                      // min distance headway
    inline def len: Double  = prop("len")                    // length of the vehicles
    inline def del: Double  = prop("del")                    // acceleration exponent (delta)


    def setInitialSpeed(v0: Double):Unit =
        prop("v0")  = v0
    end setInitialSpeed


    //the parameters to be optimized by the objective function
    //s:Double, amax:Double, bmax:Double, T:Double ,τ:Double
    def setParams(params:VectorD):Map[String, Double] =
        //println("setParams called @@@@@")

        val new_prop = Map("rt" -> params(4), // driver reaction time
            "amax" -> params(1), // max acceleration
            "bmax" -> params(2), // comfortable deceleration
            "b_emergency" -> def_prop("b_emergency"), // physical max braking (not calibrated)
            "v0" -> def_prop("v0"), // starting velocity // v0 should be adjustable to 0
            "vmax" -> def_prop("vmax"), // max velocity
            "T" -> params(3), // safe min time headway
            "s" -> params(0), // safe min distance headway
            "len" -> def_prop("len"), // length of the vehicles
            "del" -> def_prop("del")) // acceleration exponent (delta)

        new_prop
    end setParams



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the driver/vehicle characteristics/properties to the new property values.
     *  @param new_prop  the new property values
     */
    private [process] def setProps (new_prop: Map [String, Double]): Unit = prop = new_prop

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Butcher's Method (fifth order) for numerically solving an ordinary differential equation.
     *  @param Ft     the "original" function value at time t
     *  @param ft     the "derivative" function value at time t
     *  @param ft_rt  the "derivative" function value at time t - rt
     *  @param rt     the time difference (reaction time)   // it's a 5th order Runge-Kutta method like D
     *
     *  FIX - integrate into Dynamics package
    */

    def butcher(Ft: Double, ft: Double, ft_rt: Double, rt: Double): Double =
        val _1_by_90 = 1.0 / 90.0

        val k1 = ft_rt
        //val k2 = ft_rt + 0.25 * (ft - ft_rt)
        val k3 = ft_rt + 0.25 * (ft - ft_rt)
        val k4 = ft_rt + 0.50 * (ft - ft_rt)
        val k5 = ft_rt + 0.75 * (ft - ft_rt)
        val k6 = ft

        Ft + _1_by_90 * (7 * k1 + 32 * k3 + 12 * k4 + 32 * k5 + 7 * k6) * rt
    end butcher

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map a vehicle's velocity to a color on a red → yellow → green gradient.
     *  Stopped vehicles are red, free-flow vehicles are green.
     *  Uses HSB color space for smooth transitions.
     *  @param v  the current velocity (m/s)
     *  @return   a Color from red (0 m/s) through yellow to green (vmax)
     */
    def velocityColor (v: Double): Color =
        val ratio = math.max (0.0, math.min (1.0, v / vmax))   // 0.0 = stopped, 1.0 = free-flow
        val hue   = (ratio * 120.0 / 360.0).toFloat            // 0° red → 60° yellow → 120° green
        java.awt.Color.getHSBColor (hue, 0.85f, 0.95f)
    end velocityColor

end Vehicle