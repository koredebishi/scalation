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
    // var vmax = Vehicle.speedGen.gen                         // heterogeneous speeds retained for later
    var vmax = Vehicle.vmax                                    // common maximum speed for every vehicle


    var key  = -0.0
    var laneID: Int = -1
    var pathInfo : String = ""
    var segId : Int = -1
    var prevSegId: Int = -1                // previous segment id (before current segId)
    var segmentEnterTime: Double = -1.0    // simulation clock when entering current segment

    // DIAG (temporary): previous tick's leader and spacing, so `Dynamics.updateM`
    // can print the single tick where spacing first goes negative instead of
    // every tick afterwards.  Remove with the [flip] block in Dynamics.
    var o_leaderLabel: String = ""         // leader on the previous tick ("" = none)
    var o_spacing: Double = Double.NaN     // spacing on the previous tick (NaN = none)


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



    /** The vehicle immediately AHEAD of this one in the structure holding it --
     *  one hop in the doubly linked list.  This is the only correct way to read
     *  a leader: the list is the single file for the whole lane, so the leader
     *  may sit in any segment, and a per-segment `vdeque` cannot answer this.
     *  @return the leader, or null when this vehicle is at the front (or in no list)
     */
    inline def getCarAhead (car: Vehicle): Vehicle =
        val node = car.myPathNode
        if node == null then null                    // not currently in any list
        else
            val ref = node.ahead
            if ref != null then ref.elem else null
    end getCarAhead

    /** The vehicle immediately BEHIND this one -- the mirror of `getCarAhead`.
     *  @return the follower, or null when this vehicle is at the back (or in no list)
     */
    inline def getCarBehind (car: Vehicle): Vehicle =
        val node = car.myPathNode
        if node == null then null                    // not currently in any list
        else
            val ref = node.behind
            if ref != null then ref.elem else null
    end getCarBehind

    /** This vehicle's own leader / follower -- `getCarAhead(this)` reads worse
     *  than `leader` at the call site.
     */
    inline def leader:   Vehicle = getCarAhead (this)
    inline def follower: Vehicle = getCarBehind (this)
    
    /** Position within the structure that currently holds this vehicle, in a
     *  frame shared by every vehicle in that structure -- so the difference
     *  between two of them is a true spacing.
     *
     *  On a lane: distance from the lane entry, `cumLen(segId) + disp`.
     *  On a `Ramp` or `FFConnector`: those own a single VTransport, and `move`
     *  zeroes `disp` on entry, so `disp` is already that shared frame.
     *
     *  `t_disp` cannot serve either case: it counts from each vehicle's own
     *  source, so vehicles that entered at different points carry unrelated
     *  origins.  On an FF connector, where cars arrive from every lane after
     *  driving different distances, differencing t_disp is meaningless.
     */
    def corridorPos: Double =
        if myPathway != null then myPathway.corridorPos (segId, disp)
        else disp                          // ramp / FF connector: one segment
    end corridorPos

    /** Bumper-to-bumper gap to the leader.  Both positions are read in the
     *  `corridorPos` frame: `disp` is segment-local, so differencing it across
     *  two segments is meaningless.
     *  @return the gap in metres, or `Double.PositiveInfinity` when there is no leader
     */
    inline def gapToLeader: Double =
        val ldr = leader
        if ldr != null then ldr.corridorPos - corridorPos - Vehicle.len
        else Double.PositiveInfinity
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
        "bmax" -> -2.0,                      // max deceleration use to be -1.5
        "v0"   -> 4.0,                       // starting velocity // v0 should be adjustable to 0
        "vmax" -> 33.528,                    // max velocity
        "T"    -> 1.0,                       // safe min time headway (was 3.0 -- see below)
        "s"    -> 2.0,                       // safe min standstill gap (was 5.0 -- see below)
        "len"  -> 4.0,                       // length of the vehicles
        "del"  -> 4.0)                       // acceleration exponent (delta)

    /** Why T is 1.0 and s is 2.0.
     *
     *  The number that matters for a corridor whose entry backs up is not
     *  CRUISING capacity but SATURATION FLOW -- the rate a standing queue
     *  discharges.  Cars leaving a queue accelerate from rest, so the binding
     *  headway is the one at LOW speed:
     *
     *      headway = T + (s + len) / v
     *
     *  At v = 10 m/s, T = 1.2 and s = 5.0 give 2.10 s -> 1714 veh/h, against a
     *  2169 veh/h peak demand on lane 1.  Short by ~450 veh/h; over a two-hour
     *  peak that is ~900 surplus vehicles, which is what piled up at seg 0
     *  (observed: 1006 veh/km on a 1.006 km segment -- four times jam density,
     *  i.e. vehicles literally stacked).
     *
     *  Note `s` dominates at low speed -- (s + len)/v is 0.90 s of the 2.10 at
     *  10 m/s -- so tuning T alone cannot fix queue discharge.  s = 5.0 was also
     *  well above Treiber's standstill gap of 2.0 m.
     *
     *  T = 1.0 with s = 2.0 gives 1.60 s -> 2250 veh/h at 10 m/s, clearing the
     *  peak with headroom, and both sit in the usual IDM range.
     *
     *  ---- original note, on cruising capacity ----
     *
     *  IDM's equilibrium space headway is `s + v*T + len`, so a lane can pass at
     *  most `3600 * v / (s + v*T + len)` veh/h.  At T = 3.0 that is ~1090 veh/h
     *  per lane -- about half a real freeway lane.  The PeMS demand for US-101
     *  Donald Doyle (1-401112ML.csv) exceeds it on lane 1 in 46 of 48 time bins,
     *  peaking at 2169 veh/h against a 1090 capacity at the 7:30 bin.
     *
     *  That overload used to be invisible: a car whose gap went negative fell
     *  into the free-flow branch of `IDMDynamics.updateM` and drove through its
     *  leader, venting the excess by letting vehicles overlap.  With that branch
     *  fixed the overload is real and the lane deadlocks at the entry instead.
     *
     *  Solving `T + (s + len)/v <= mu` at the binding bin (lane 1, mu = 1.66 s,
     *  v = 26.5 m/s) gives T <= 1.32 s.  T = 1.2 is the largest round value that
     *  clears every bin on every lane, leaving ~30% headroom at the peak, and it
     *  sits in the usual IDM range (Treiber uses 1.0-1.6 s).
     */

    /** current values for driver/vehicle characteristics/properties
     */
    private [process] var prop = def_prop


    //    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current value for 'prop("rt") the driver reaction time
     */
    inline def rt: Double   = prop("rt")                     // driver reaction time
    inline def amax: Double = prop("amax")                   // max acceleration
    inline def bmax: Double = prop("bmax")                   // max deceleration
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
            "bmax" -> params(2), // max deceleration
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
