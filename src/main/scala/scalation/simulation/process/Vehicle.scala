
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Tue Feb  4 14:56:34 EST 2020
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package simulation
package process

import scalation.mathstat.VectorD

val vehicleProps = Map ("τ" -> 1.0,                                 // reaction time
                        "amax" -> 2.0,                              // max acceleration
                        "bmax" -> -1.5,                             // max deceleration
                        "v0"   -> 0.0,                              // starting velocity
                        "vmax" -> 33.528,                           // max velocity
                        "T"    -> 3.0,                              // min time headway
                        "len"    -> 5.0,                              // length of a vehicle in meters.
                        "delta"  -> 4.0,                            // Delta parameter for IDM
                        "minDist" -> 40.0)                          // min distance headway for the vehicle.

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

/** The Vehicle class extends the `SimAgent` and represents a vehicle on a road.
 *  @param label     the label/name of the vehicle
 *  @param director  the model to which this vehicle belongs
 *  @param prop      the property map governing the motion of the vehicle
 */
abstract class Vehicle(label: String, director: Model,
                       var params: VectorD = null)
         extends SimActor (label, director):

    if params == null then
        params = VectorD(vehicleProps("τ"), vehicleProps("amax") ,vehicleProps("bmax")  ,
                         vehicleProps("v0") , vehicleProps("vmax") , vehicleProps("T") ,
                         vehicleProps("minDist"), vehicleProps("len"), vehicleProps("delta") )     // defaults
    end if

    /** Variable representing previous total displacement value for the vehicle.
     */
    private var _o_t_disp = t_disp

    /** Variable representing previous velocity value for the vehicle
     */
    private var _o_velocity = velocity

    /** Variable representing previous acceleration value for the vehicle
     */
    private var _o_acc = acc

    /** Variable representing the lane the vehicle is in (useful for traffic models).
     */
    private var _lane = 0

    /** Variable representing the reaction time for the vehicle.
     */
    private var _τ = params(0)

    /** Variable representing the maximum acceleration for the vehicle.
     */
    private var _amax = params(1)

    /** Variable representing the max deceleration for the vehicle.
     */
    private var _bmax = params(2)

    /** Variable representing the desired velocity for the vehicle.
     */
    private var _vmax = params(4)

    /** Variable representing the min time headway for the vehicle.
     */
    private var _T = params(5)

    /** Variable representing the min distance headway for the vehicle.
     */
    private var _s = params(6)

    /** The length of this vehicle in meters.
     */
    val length = params(7)

    /** Delta parameter for IDM
     */
    private var _δ = params(8)

    /** The Sink which is the destination of this vehicle.
     */
    private var _dest: Sink = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the previous total displacement value for the vehicle.
     */
    def o_t_disp: Double = _o_t_disp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the previous velocity for the vehicle.
     */
    def o_velocity: Double = _o_velocity

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the previous acceleration for the vehicle.
     */
    def o_acc: Double = _o_acc

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the current lane of 'this' `Vehicle`.
     */
    def lane: Int = _lane

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the current reaction time for 'this' `Vehicle`.
     */
    def τ: Double = _τ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the max acceleration for 'this' `Vehicle`.
     */
    def amax: Double = _amax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the max deceleration for 'this' `Vehicle`.
     */
    def bmax: Double = _bmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the desired velocity for 'this' `Vehicle`.
     */
    def vmax: Double = _vmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the min time headway for 'this' `Vehicle`.
     */
    def T: Double = _T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the min distance headway for 'this' `Vehicle`.
     */
    def s: Double = _s

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the delta IDM parameter for 'this' `Vehicle`.
     */
    def δ: Double = _δ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get the destination Sink for this `Vehicle`.
     */
    def dest: Sink = _dest




    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the old total displacement of the vehicle
     *
     * @param o_t_disp the old total displacement for the `Vehicle`
     */
    def o_t_disp_=(o_t_disp: Double): Unit = _o_t_disp = o_t_disp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the old velocity for the vehicle
     *
     * @param o_velocity the old velocity for the `Vehicle`
     */
    def o_velocity_=(o_velocity: Double): Unit = _o_velocity = o_velocity


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the old acceleration for the vehicle
     *
     * @param o_acc the old acceleration for the `Vehicle`
     */
    def o_acc_=(o_acc: Double): Unit = _o_acc = o_acc


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the lane of 'this' `Vehicle`.
     *
     * @param lane the new lane for the `Vehicle`
     */
    def lane_=(lane: Int): Unit = _lane = lane

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the reaction time for 'this' `Vehicle`.
     *
     * @param τ the new reaction time for the `Vehicle`
     */
    def τ_=(τ: Double): Unit = _τ = τ


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the max acceleration for 'this' `Vehicle`.
     *
     * @param amax the new max acceleration for the `Vehicle`
     */
    def amax_=(amax: Double): Unit = _amax = amax


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the max deceleration for 'this' `Vehicle`.
     *
     * @param bmax the new max deceleration for the `Vehicle`
     */
    def bmax_=(bmax: Double): Unit =  _bmax = bmax


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the desired velocity for 'this' `Vehicle`.
     *
     * @param vmax the new desired velocity for the `Vehicle`
     */
    def vmax_=(vmax: Double): Unit = _vmax = vmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the time headway for 'this' `Vehicle`.
     *
     * @param T the new time headway for the `Vehicle`
     */
    def T_=(T: Double): Unit =  _T = T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the distance headway for 'this' `Vehicle`.
     *
     * @param s the new min distance headway for the `Vehicle`
     */
    def s_=(s: Double): Unit = _s = s


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the delta IDM parameter for 'this' `Vehicle`.
     *
     * @param s the new delta IDM parameter value for the `Vehicle`
     */
    def δ_=(δ: Double): Unit =  _δ = δ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the destination for 'this' `Vehicle`.
     *
     * @param s the destination for the `Vehicle`
     */
    def dest_=(d: Sink): Unit = _dest = d


    var acc: Double = 0.0 //the current acceleration of 'this' `SimActor`.
    var t_disp   = 0.0                                              // set initial total displacememt to 0
    var disp: Double = 0.0
    var velocity = vehicleProps("v0")                                       // set initial velocity to v0
    var pred: Vehicle = null                                        // this is vehicle ahead;   needs a better way
    var succ: Vehicle = null                                        // this is vehicle behind;
    var done: Boolean = false                                       // the vehicle has completed the route







   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Update the values of the vehicle: velocity, displacement, lane
    *  according to the models being used.
    *  @param choice  choice of car-following model to use.
    */
    def update (choice: Int = 1): Unit =
        if      choice == 1 then IDMUpdate   ()
        else if choice == 2 then GippsUpdate ()
     //else if (choice == 3) OVMUpdate   ()
    end update

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the vehicle's acceleration, velocity, and position using the
     *  Intelligent Driver Model (located in Motion.scala) and butcher's method
     *  for solving ordinary differential equations.
     */
    def IDMUpdate (): Unit =
        var a = Motion.IDM (this, pred.asInstanceOf [Vehicle], δ)
        //        println (this.name + ":\tthe new acceleration is: " + a)
        if a.isNaN then   a = 0.0
        if a.isNegInfinity then a = bmax
        if a.isPosInfinity then a = amax
        if a < 0.0 && a < bmax then
          val r = math.log(a) / math.log (bmax)
          if r > 5.0 then a = 3.0 * bmax
          else         a = bmax
          end if
        end if

        if a > 0.0 && a > amax then a = amax
        var v = Motion.butcher (velocity, a, acc, τ)
        //        println (this.name + ":\tthe new velocity is: " + v)
        if v < 0.0 then v = 1.0
        val x = Motion.butcher (t_disp, v, velocity, τ)
        //        println (this.name + ":\tthe new position is: " + v)
        val p = if pred == null then "null" else pred.name + "_" + pred.subtype
        val s = if succ == null then "null" else succ.name + "_" + succ.subtype
        //        println (name + ": a = " + a + ", v = " + v + ", x = " + x +
        //                ", t = " + director.clock + ", pred = " + p + ", succ = " + s +
        //                ", lane = " + this.lane + ", sink = " + this.dest.name + ", vmax = " + vmax)
        o_acc = acc
        acc = a
        o_velocity = velocity
        velocity = v
        val dx = x - t_disp
        disp += dx
        o_t_disp = t_disp
        t_disp = x
    end IDMUpdate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the vehicle's velocity and position using Gipps' Model
     *  (located in Motion.scalaa) and butcher's method for solving ordinary
     *  differential equations.
     */
    def GippsUpdate (): Unit =
        // UNDER DEVELOPMENT
        val v = Motion.gipps   (this, pred.asInstanceOf [Vehicle])
        val x = Motion.butcher (t_disp, v, velocity, τ)
        o_velocity = velocity
        velocity = v
        val dx = x - t_disp
        disp += dx
        o_t_disp = t_disp
        t_disp = x
    end GippsUpdate
end Vehicle

