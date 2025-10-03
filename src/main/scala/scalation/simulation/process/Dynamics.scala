//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Tue Feb  4 14:56:34 EST 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Supports Physics Models for Motion of Vehicles
 */

package scalation
package simulation
package process


import scala.math.{log, sqrt, min, max}

import Vehicle._


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Dynamics` trait supports physics models for the motion of vehicles, e.g.,
 *  car-following models.
 */
trait Dynamics:

    private [process] var disp       = 0.0                          // set initial current displacement to 0
    private [process] var t_disp     = 0.0                          // set initial total displacement to 0
    private [process] var velocity   = v0                           // set initial velocity to v0
    private [process] var o_t_disp   = t_disp                       // set initial old total displacement t_disp
    private [process] var o_velocity = velocity                     // set initial old velocity to velocity
    private [process] var acc        = 0.0                          // set initial acceleration to 0
    private [process] var o_acc      = acc                          // set initial old acceleration acc
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the values of the vehicle: velocity, displacement, lane according
     *  to the car-following model being used.
     *  @param motion  the dynamics/physics determining the motion (e.g., car-following model)
     *  @param car     the vehivle to move
 
     */
    def updateV (car: Vehicle, maxDisp: Double): Unit =
//        println (s"Dynamics.updateV: called $car")
        this match
            case GippsDynamics => { GippsDynamics.updateM (car, maxDisp) }
            case _             => { println ("IDM");   IDMDynamics.updateM (car,  maxDisp) }
    end updateV

end Dynamics


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GippsDynamics` object provides equations for the Gipps car-following model.
 *  @see https://en.wikipedia.org/wiki/Gipps%27_model
 */
object GippsDynamics
    extends Dynamics:

    private val debug = debugf ("GippsDynamics", false)              // debug function
    private[process] val easyW = new EasyWriter("simulation", "OnewayVehicle2LModel.txt")
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Update the vehicle's velocity and position using Gipps' Model (located in `Motion`)
     * and Butcher's method for solving ordinary differential equations.
     *
     * @param car the car/vehicle whose velocity and position is being updated
     */
    def updateM(car: Vehicle, length: Double): Unit =
        //debug("updateM", s"car = $car with car.myNode = ${car.myPathNode}")
        //val ref = car.myNode.ahead
        val ref = car.myPathNode.ahead
        //val car_ahead = if ref == null then null else ref.elem

        val car_ahead = if ref != null then ref.elem else null

        //debug("updateM", s"car = ${car.displayLabel} (velocity and position) based on car_ahead = $car_ahead")
        //easyW.println(s"updateM car = ${car.displayLabel} (velocity and position) based on car_ahead = $car_ahead")

        val v = gipps(car, car_ahead, length) + EPSILON // determine new velocity
        //debug("updateM", s"car = $car \t the new VELOCITY is: $v")
        easyW.println(s"UpdateM car = $car  \t the new VELOCITY is: $v")



        val x = butcher(car.t_disp, v, car.velocity, prop("rt")) // new proposed position for car
        //debug("updateM", s"car = $car \t the new POSITION is: $x")
        easyW.println(s"UpdateM car = $car  \t the new POSITION is: $x")

        car.o_velocity = car.velocity // save the old velocity
        car.velocity = v // assign new velocity

        car.o_t_disp = car.t_disp // save old car position
        val dx = x - car.t_disp // change in car's position
        val new_disp = if car.disp + dx <= length then car.disp + dx // new car displacement on road
        else length

        car.t_disp += new_disp - car.disp // new car position
        car.disp = new_disp // displacement on road
        //debug("updateM", s"car.disp = ${car.disp}, car.t_disp = ${car.t_disp}")
    end updateM



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the velocity of the vehicle based on Gipps' model for a vehicle and its predecessor.
     *  @param cn  the current vehicle
     *  @param cp  the predecessor of the current vehicle
     */
    def gipps (cn: Vehicle, cp: Vehicle, length: Double): Double =
        if cp == null then
            //easyW.println(s"vehicle_ahead value $cp, current_vehicle value = $cn ${prop("rt")} prop")
            gipps (amax, bmax, len, cn.vmax, cn.t_disp, cn.velocity, cn.t_disp + 1000, cn.vmax, prop("rt"))   // All vehicles initialized should use this first (that means every vehicle needs to keep track of his ahead vehicle
        else
            val cp_r_disp = if cp.segId == cn.segId then cp.disp
                            else length + cp.disp
            gipps (amax, bmax, len, cn.vmax, cn.disp, cn.velocity, cp_r_disp, cp.velocity, prop("rt"))
    end gipps

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the velocity of the vehicle based on Gipps' model.
     *  @param an  the max acceleration of drivers
     *  @param bn  the max deceleration of drivers (negative #)
     *  @param sp  the size of vehicles
     *  @param Vn  the desired velocity of driver n
     *  @param xn  the current position of driver n
     *  @param vn  the current velocity of driver n
     *  @param xp  the current position of the predecessor  //ahead
     *  @param vp  the current velocity of the predecessor
     *  @param rt  the reaction time of drivers
     */
    private def gipps (an: Double, bn: Double, sp: Double, Vn: Double, xn: Double,
                       vn: Double, xp: Double, vp: Double, rt: Double): Double =
        val free = vn + (2.5 * an * rt * (1.0 - vn / Vn)) * sqrt (0.025 + vn / Vn)

        val left_1 = 2 * (xp - sp - xn)
        val left_2 = (vn * rt) - (vp * vp / bn)
        var right_side  =  bn * (2 * (xp - sp - xn) - (vn * rt) - (vp * vp / bn))
        val inner_exp = (bn * bn * rt * rt) - bn * (2 * (xp - sp - xn) - (vn * rt) - (vp * vp / bn))
        if inner_exp < 0 then easyW.println(s"Shouldn't be negative: $inner_exp , $right_side, left_1: $left_1, left_2: $left_2")
        val cong = (bn * rt) + sqrt(max(0.0, inner_exp))

        easyW.println(s"The free $free and the Cong $cong")
        min (free, cong)
    end gipps


end GippsDynamics

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IDMDynamics` object provides equations for the Intelligent Driver Model (IDM)
 *  car-following model.
 *  @see https://en.wikipedia.org/wiki/Intelligent_driver_model
 */object IDMDynamics
    extends Dynamics:

    private val debug = debugf ("IDMDynamics", true)                // debug function

    private val FREERANGE = 50.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the vehicle's acceleration, velocity, and position using the
     *  Intelligent Driver Model (located in `Motion`) and Butcher's method
     *  for solving ordinary differential equations.
     *  @param car  the car/vehicle whose acceleration, velocity, and position is being updated
     */
    def updateM (car: Vehicle, length: Double): Unit =
        debug ("updateM", s"car = $car")
        var a = iDM (car, car.myNode.ahead.asInstanceOf [Vehicle], del)
        debug ("updateM", s"car = $car \t the new ACCELERATION is: $a")
        if a.isNaN then         a = 0.0
        if a.isNegInfinity then a = bmax                            // max braking acceleration
        if a.isPosInfinity then a = amax                            // max forward acceleration
        if a < 0.0 && a < bmax then
            val r = log(a) / log (bmax)
            a = if r > 5.0 then 3.0 * bmax else bmax                // FIX - unclear
        if a > 0.0 && a > amax then a = amax

        var v = butcher (car.velocity, a, car.acc, rt)
        debug ("updateM", s"car = $car \t the new VELOCITY is: $v")
        if v < 0.0 then v = 1.0                                     // move slowly, not stopped

        val x = butcher (car.t_disp, v, car.velocity, rt)
        debug ("updateM", s"car = $car \t the new POSITION is: $x")

        car.o_acc = car.acc
        car.acc   = a
        car.o_velocity = car.velocity
        car.velocity = v
        val dx    = x - car.t_disp
        car.disp += dx
        car.o_t_disp = car.t_disp
        car.t_disp   = x
    end updateM


    // How far into the segment is any Car.  //
    //From where you started, how far have you travelled?  //
    //Given that they started from different places, is that going to cause a problem?

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the acceleration of the vehicle based on the Intelligent Driver Model
     *  for a vehicle and its predecessor.
     *  @param cn   the current vehicle
     *  @param cp   the predecessor of the current vehicle
     *  @param del  the acceleration exponenent (defualts to 4)
     */
    def iDM (cn: Vehicle, cp: Vehicle, del: Double = 4.0): Double =
        if cp == null then
            iDMFree (amax, cn.velocity, cn.vmax, del)
        else if cp.t_disp - cn.t_disp > FREERANGE then
            iDMFree (amax, cn.velocity, cn.vmax, del)
        else
            iDM (amax, -bmax, len, cn.vmax, cn.t_disp, cn.velocity, cp.t_disp, cp.velocity, T, s, del)
    end iDM

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the acceleration of the vehicle based on the Intelligent Driver Model.
     *  @param an   the max acceleration of drivers
     *  @param bn   the max deceleration of drivers (negative #)
     *  @param sp   the size of vehicles
     *  @param Vn   the desired velocity of driver n
     *  @param xn   the current position of driver n
     *  @param vn   the current velocity of driver n
     *  @param xp   the current position of the predecessor
     *  @param vp   the current velocity of the predecessor
     *  @param T    the safe min time headway
     *  @param s0   the safe min distance headway
     *  @param del  the acceleration exponenent (defualts to 4)
     */
    private def iDM (an: Double, bn: Double, sp: Double, Vn: Double, xn: Double, vn: Double,
                     xp: Double, vp: Double, T: Double, s0: Double, del: Double): Double =
        val Δx = xp - xn - sp
        val Δv = vn - vp
        val ss = s0 + vn * T + (vn * Δv) / (2.0 * sqrt (an * bn))
        an * (1.0 - (vn / Vn) ~^ del - (ss / Δx) ~^ 2.0)
    end iDM

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the acceleration of the vehicle based on the Intelligent Driver Model
     *  when there is no predecessor.
     *  @param an   the max acceleration of drivers
     *  @param vn   the current velocity of driver n
     *  @param Vn   the desired velocity of driver n
     *  @param del  the acceleration exponenent (defualts to 4)
     */
    private def iDMFree (an: Double, vn: Double, Vn: Double, del: Double = 4.0): Double =
        an * (1.0 - (vn / Vn) ~^ del)
    end iDMFree

end IDMDynamics
