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


import scala.math.{ sqrt, abs, max, min}//, min, max, abs}
import scalation.mathstat.*
import scalation.dynamics.*
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
            case _             => { IDMDynamics.updateM (car,  maxDisp) }
    end updateV

end Dynamics


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GippsDynamics` object provides equations for the Gipps car-following model.
 *  @see https://en.wikipedia.org/wiki/Gipps%27_model
 */
object GippsDynamics
    extends Dynamics:

    private val debug = debugf ("GippsDynamics", false)              // debug function
    private[process] val easyW = new EasyWriter("simulation", "CalRoute101Model.txt")
    //easyW.off()
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


    /** Update the vehicle's velocity and position using Gipps' Model (located in `Motion`)
     * and Butcher's method for solving ordinary differential equations.
     *
     * @param car the car/vehicle whose velocity and position is being updated
     */
    def updateM(car: Vehicle, length: Double): Unit =
        val ref = car.myPathNode.ahead
        val car_ahead = if ref != null then ref.elem else null

        // ------------------------------------------------------------------
        // WRONG (OLD LOGIC — DO NOT USE)
        //
        // val v = gipps(car, car_ahead, length) + EPSILON
        // val x = butcher(car.t_disp, v, car.velocity, prop("rt"))
        //
        // PROBLEM:
        // - `v` here is v(t+τ)
        // - Butcher expects ft = v(t), ft_rt = v(t−τ)
        // - Passing v(t+τ) into the integrator violates causality
        // - This mixes future velocity prediction with position reconstruction
        // ------------------------------------------------------------------

        // ------------------------------------------------------------------
        // CORRECT LOGIC (REPLACEMENT)
        //
        // Step 1: compute next velocity using Gipps (discrete rule)
        // This gives v(t+τ) but is NOT used inside the Butcher integrator.
        // ------------------------------------------------------------------
        val v = gipps(car, car_ahead, length) + EPSILON // v(t+τ)

        println(s"car_id = ${car.displayLabel} : velocity = ${car.velocity} -> $v")

        // ------------------------------------------------------------------
        // Step 2: update position using ONLY past velocity samples
        //          Can also (replace this with)  kinematics.
        //        // --- Position update using kinematics ---
        //        // x(t+τ) = x(t) + v(t)τ + ½aτ²
        //        val dx = v_old * rt + 0.5 * a * rt * rt
        // butcher(
        //   Ft     = x(t),
        //   ft     = v(t),
        //   ft_rt  = v(t−τ),
        //   rt     = τ
        // )
        //
        // This matches Wikipedia's Butcher method exactly.
        // ------------------------------------------------------------------
        val x = butcher(
            car.t_disp, // x(t)
            car.velocity, // v(t)
            car.o_velocity, // v(t−τ)
            prop("rt")
        )

        println(s"car_id = ${car.displayLabel} : position = ${car.t_disp} -> $x")

        // ------------------------------------------------------------------
        // Step 3: commit velocity state AFTER position integration
        // ------------------------------------------------------------------
        car.o_velocity = car.velocity // store v(t) as old velocity
        car.velocity = v // assign v(t+τ)

        // ------------------------------------------------------------------
        // Step 4: update displacement (segment-bounded)
        // ------------------------------------------------------------------
        car.o_t_disp = car.t_disp
        val dx = x - car.t_disp

        val new_disp =
            if car.disp + dx <= length then car.disp + dx
            else length

        car.t_disp += new_disp - car.disp
        car.disp = new_disp
    end updateM


    //    def updateM(car: Vehicle, length: Double): Unit =
//        val ref = car.myPathNode.ahead
//        val car_ahead = if ref != null then ref.elem else null
//
//        val v = gipps(car, car_ahead, length) + EPSILON // determine new velocity
//
//        val x = butcher(car.t_disp, v, car.velocity, prop("rt")) // new proposed position for car
//
//        car.o_velocity = car.velocity // save the old velocity
//        car.velocity = v // assign new velocity
//
//
//        car.o_t_disp = car.t_disp // save old car position
//        val dx = x - car.t_disp // change in car's position
//        val new_disp = if car.disp + dx <= length then car.disp + dx // new car displacement on road
//        else length
//
//        car.t_disp += new_disp - car.disp // new car position
//        car.disp = new_disp // displacement on road
//        //debug("updateM", s"car.disp = ${car.disp}, car.t_disp = ${car.t_disp}")
//    end updateM



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the velocity of the vehicle based on Gipps' model for a vehicle and its predecessor.
     *  @param cn  the current vehicle
     *  @param cp  the predecessor of the current vehicle
     */
    // FIX: DLL 'ahead' tracks insertion order, NOT physical position.
    // When cp.segId < cn.segId, the "leader" is actually BEHIND the follower physically.
    // In this case, ignore the phantom leader and use free-flow velocity.
    def gipps (cn: Vehicle, cp: Vehicle, length: Double): Double =
        if cp == null || cp.segId < cn.segId then
            gipps (amax, bmax, len, cn.vmax, cn.t_disp, cn.velocity, cn.t_disp + 1000, cn.vmax, prop("rt"))
        else
            // Leader is in same segment or ahead segment apply car-following
            val cp_r_disp = if cp.segId == cn.segId then cp.disp
                            else length + cp.disp
            gipps (amax, bmax, len, cn.vmax, cn.disp, cn.velocity, cp_r_disp, cp.velocity, prop("rt"))
    end gipps
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Return the velocity of the vehicle based on Gipps' model.
     *
     * @param an the max acceleration of drivers
     * @param bn the max deceleration of drivers (may be negative)
     * @param sp the size of vehicles
     * @param Vn the desired velocity of driver n
     * @param xn the current position of driver n
     * @param vn the current velocity of driver n
     * @param xp the current position of the predecessor (ahead)
     * @param vp the current velocity of the predecessor
     * @param rt the reaction time of drivers
     */
    private def gipps(an: Double, bn: Double, sp: Double, Vn: Double, xn: Double,
                      vn: Double, xp: Double, vp: Double, rt: Double): Double =
        // --------------- Parameters ---------------------
        val b = abs(bn) // use positive magnitude of deceleration
        val b_hat = b * 1.8 // expected braking ability of leader (default 0.8×b)   b_hat >= b

        // --------------- Free-flow branch ---------------
        // v_free = v_n + 2.5 * a_n * τ * (1 - v_n/V_n) * sqrt(0.025 + v_n/V_n)
        val free = vn + (2.5 * an * rt * (1.0 - vn / Vn)) * sqrt(0.025 + vn / Vn)

        // --------------- Congested (safety) branch -------
        // v_cong = b * τ + sqrt(b^2 * τ^2 - b[2(gap) - v_nτ - v_p^2/b̂])
        val gap = xp - sp - xn                                          // effective front-to-front gap
        val phi = 2.0 * gap - (vn * rt) - (vp * vp / b_hat)             // safety term inside brackets

        // When phi >= 0, spacing is large enough that braking constraint is inactive.
        // In that case, use free-flow velocity directly (skip sqrt computation).
        if phi >= 0.0 then return free

        // Otherwise, apply the congested/safety branch.
        val inner_exp = (b * b * rt * rt) - b * phi // value inside sqrt
        val cong = (b * rt) + sqrt(max(0.0, inner_exp)) // braking (congested) velocity
        min(free, cong)
    end gipps


end GippsDynamics

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IDMDynamics` object provides equations for the Intelligent Driver Model (IDM)
 *  car-following model.
 *  @see https://en.wikipedia.org/wiki/Intelligent_driver_model
 */

object IDMDynamics
    extends Dynamics:

    private val debug = debugf ("IDMDynamics", true)                // debug function

    private val FREERANGE = 50.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the vehicle's acceleration, velocity, and position using the
     *  Intelligent Driver Model (located in `Motion`) and Butcher's method
     *  for solving ordinary differential equations.
     *  @param car  the car/vehicle whose acceleration, velocity, and position is being updated
     */
//    def updateM (car: Vehicle, length: Double): Unit =
//        //debug ("updateM", s"car = $car")
//        var a = iDM (car, car.myNode.ahead.asInstanceOf [Vehicle], del)
//        //debug ("updateM", s"car = $car \t the new ACCELERATION is: $a")
//        if a.isNaN then         a = 0.0
//        if a.isNegInfinity then a = bmax                            // max braking acceleration
//        if a.isPosInfinity then a = amax                            // max forward acceleration
//        if a < 0.0 && a < bmax then
//            val r = log(a) / log (bmax)
//            a = if r > 5.0 then 3.0 * bmax else bmax                // FIX - unclear
//        if a > 0.0 && a > amax then a = amax
//
//        //make it a bit:
//        // rather than invoking the solver 2x, We can.
//        // Vectorization and call it once.
//        var v = butcher (car.velocity, a, car.acc, rt)      // determine new velocity
//        // dormand prince
//        //debug ("updateM", s"car = $car \t the new VELOCITY is: $v")
//        if v < 0.0 then v = 1.0                                     // move slowly, not stopped
//
//        val x = butcher (car.t_disp, v, car.velocity, rt)   // new proposed position for car
//        //debug ("updateM", s"car = $car \t the new POSITION is: $x")
//
//        car.o_acc = car.acc
//        car.acc   = a
//        car.o_velocity = car.velocity
//        car.velocity = v
//        var dx    = x - car.t_disp
//        car.disp += dx
//        car.o_t_disp = car.t_disp
//        car.t_disp   = x
//    end updateM//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**
     *  @param car     the vehicle to update
     *  @param length  the segment length
     */
    def updateM(car: Vehicle, length: Double): Unit =
        // ─────────────────────────────────────────────────────────────────────────
        // STEP 1: Snapshot leader state BEFORE integration
        // ─────────────────────────────────────────────────────────────────────────
        // This is the key to preserving simultaneity without global state vectors.
        // The leader's position and velocity are treated as CONSTANTS during this
        // vehicle's integration step. This is physically justified because:
        //   - Reaction time τ means driver responds to observed (past) leader state
        //   - For Δt ≈ τ, leader motion during Δt is second-order effect
        //   - Same assumption underlies discrete stepping (snapshot semantics)
        // ─────────────────────────────────────────────────────────────────────────
        val ref = car.myPathNode.ahead
        val car_ahead = if ref != null then ref.elem else null
        val dt = rt

        // Snapshot leader state (frozen during integration)
        val (x_leader, v_leader): (Double, Double) =                // leader's (position, velocity)
            if car_ahead == null || car_ahead.t_disp - car.t_disp > FREERANGE then
                (car.t_disp + 1000.0, car.velocity)  // Phantom leader: free-flow
            else
                (car_ahead.t_disp, car_ahead.velocity)  // Real leader: snapshot

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 2: Save old state for history tracking
        // ─────────────────────────────────────────────────────────────────────────
        val v_old = car.velocity
        val x_old = car.t_disp

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 3: Define the coupled ODE system as Array[DerivativeV]
        // ─────────────────────────────────────────────────────────────────────────
        // State vector: y = [x, v] where x = position, v = velocity
        // Derivatives:  y' = [v, a] where a = IDM acceleration
        //
        // integrateVV expects Array[DerivativeV] where DerivativeV = (Double, VectorD) => Double
        // Each element returns the derivative for one dimension:
        //   odes(0) = dx/dt = v
        //   odes(1) = dv/dt = a (IDM acceleration)
        // ─────────────────────────────────────────────────────────────────────────

        // Helper function to compute IDM acceleration

        /**
         * Compute IDM acceleration using the existing iDM method.
         * This avoids duplicating the IDM formula - single source of truth.
         *
         * ═══════════════════════════════════════════════════════════════════════
         * WHY THIS HELPER EXISTS (NOT REDUNDANT WITH iDM):
         * ═══════════════════════════════════════════════════════════════════════
         *
         * The Dormand-Prince ODE solver calls this function MULTIPLE TIMES per
         * timestep with DIFFERENT trial values. These are NOT the vehicle's
         * actual state - they are intermediate Runge-Kutta stage evaluations.
         *
         * Dormand-Prince 7-Stage Evaluation:
         * ┌─────────────────────────────────────────────────────────────────────┐
         * │ Stage 1: y = [x₀, v₀]                  idmAccel(x₀, v₀)           │
         * │ Stage 2: y = [x₀ + k₁·h/5, ...]        idmAccel(x_trial, v_trial) │
         * │ Stage 3: y = [x₀ + k₁·3h/40 + ..., ...]idmAccel(x_trial, v_trial) │
         * │ Stage 4: y = [x₀ + k₁·44h/45 + ...,...]idmAccel(x_trial, v_trial) │
         * │ Stage 5: y = [x₀ + ..., ...]           idmAccel(x_trial, v_trial) │
         * │ Stage 6: y = [x₀ + ..., ...]           idmAccel(x_trial, v_trial) │
         * │ Stage 7: y = [x₀ + ..., ...]           idmAccel(x_trial, v_trial) │
         * └─────────────────────────────────────────────────────────────────────┘
         *
         * At each stage, y(0) and y(1) are DIFFERENT trial values computed by
         * the solver. The existing iDM(cn, cp, del) method reads directly from
         * Vehicle objects (car.t_disp, car.velocity), which would always return
         * the ORIGINAL state - defeating the purpose of higher-order integration.
         *
         * This helper accepts (x_n, v_n) as parameters so the ODE solver can
         * pass its internal trial values, while using SNAPSHOTTED leader state
         * (x_leader, v_leader) from the enclosing scope.
         *
         * ═══════════════════════════════════════════════════════════════════════
         * RELATIONSHIP TO EXISTING iDM METHODS:
         * ═══════════════════════════════════════════════════════════════════════
         *
         * iDM(cn, cp, del)           Wrapper for Vehicle objects (discrete stepping)
         * iDM(an, bn, sp, ...)       Core IDM formula (called by this helper)
         * iDMFree(an, vn, Vn, del)   Free-flow case (no leader)
         * idmAccel(x_n, v_n)         ODE-compatible wrapper (this method)
         *
         * This method calls iDM(an, bn, sp, ...) internally, ensuring the IDM
         * formula is defined in exactly ONE place.
         *
         * @param x_n  Current position of driver n (from ODE solver trial values, NOT car.t_disp)
         * @param v_n  Current velocity of driver n (from ODE solver trial values, NOT car.velocity)
         * @return     IDM acceleration clamped to physical bounds [-bmax, amax]
         */
        def idmAccel(x_n: Double, v_n: Double): Double =
            val b = abs(bmax)

            // Call existing iDM with snapshotted leader state
            // iDM(an, bn, sp, Vn, xn, vn, xp, vp, T, s0, del)
            var a = iDM(amax, b, len, car.vmax, x_n, v_n,
                        x_leader, v_leader, T, s, del)

            // Clamp acceleration to physical bounds
            if a.isNaN || a.isInfinity then a = 0.0
            if a < -b then a = -b
            if a > amax then a = amax
            a
        end idmAccel

        // Array of derivative functions: [dx/dt, dv/dt]
        // dx/dt = v (velocity)         returned directly from state vector
        // dv/dt = a (IDM acceleration) returned by idmAccel helper function
        val odes: Array[DerivativeV] = Array(
            (t: Double, y: VectorD) => y(1),                    // dx/dt = v (velocity)
            (t: Double, y: VectorD) => idmAccel(y(0), y(1))     // dv/dt = a (IDM acceleration)
        )

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 4: Solve the coupled system with Dormand-Prince
        // ─────────────────────────────────────────────────────────────────────────
        // integrateVV solves: y(t + dt) given y(t) and dy/dt = odes(t, y)
        // Single call, consistent state evolution, O(Δt⁵) accuracy
        // ─────────────────────────────────────────────────────────────────────────
        val y0 = VectorD(car.t_disp, car.velocity)                      // initial state [x(t), v(t)]
        val y1 = DormandPrince.integrateVV(odes, y0, dt)                // solve to get [x(t+dt), v(t+dt)]

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 5: Extract and clamp results
        // ─────────────────────────────────────────────────────────────────────────
        val x_new = y1(0)                                                 // new position x(t + dt)
        var v_new = y1(1)                                                 // new velocity v(t + dt)

        // Physical constraints: velocity must be non-negative and bounded
        if v_new < 0.0 then v_new = 0.0                                 // no backward motion
        if v_new > car.vmax then v_new = car.vmax                       // respect max speed

        // Back-calculate acceleration for state storage
        val a = (v_new - v_old) / dt                                    // average acceleration over dt

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 6: Update vehicle state
        // ─────────────────────────────────────────────────────────────────────────
        car.o_acc      = car.acc                                        // save old acceleration
        car.acc        = a                                              // update acceleration
        car.o_velocity = v_old                                          // save old velocity
        car.velocity   = v_new                                          // update velocity
        car.o_t_disp   = x_old                                          // save old position

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 7: Segment-bounded displacement update
        // ─────────────────────────────────────────────────────────────────────────
        val dx = x_new - x_old                                         // change in position
        val proposed_disp = car.disp + dx                              // proposed new displacement
        val new_disp = if proposed_disp <= length then proposed_disp else length           // clamp to segment length

        car.t_disp += new_disp - car.disp                              // update total position
        car.disp    = new_disp                                         // update displacement on segment
    end updateM

//
//    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Update the vehicle's acceleration, velocity, and position using the
//     *  Intelligent Driver Model and Butcher's method (simpler/faster than Dormand-Prince).
//     *  Uses 2 Butcher calls vs 14 derivative evaluations in Dormand-Prince.
//     *  @param car     the vehicle to update
//     *  @param length  the segment length
//     */
//    def updateM(car: Vehicle, length: Double): Unit =
//        val ref = car.myPathNode.ahead
//        val car_ahead = if ref != null then ref.elem else null
//
//        // ─────────────────────────────────────────────────────────────────────────
//        // STEP 1: Compute IDM acceleration (single evaluation)
//        // ─────────────────────────────────────────────────────────────────────────
//        var a = iDM(car, car_ahead, del)
//
//        // Clamp acceleration to physical bounds
//        if a.isNaN then a = 0.0
//        if a.isNegInfinity then a = bmax
//        if a.isPosInfinity then a = amax
//        if a < bmax then a = bmax
//        if a > amax then a = amax
//
//        // ─────────────────────────────────────────────────────────────────────────
//        // STEP 2: Integrate velocity using Butcher's method
//        // v(t+τ) = butcher(v(t), a(t), a(t-τ), τ)
//        // ─────────────────────────────────────────────────────────────────────────
//        var v = butcher(car.velocity, a, car.o_acc, rt)
//
//        // Physical constraints
//        if v < 0.0 then v = 0.0
//        if v > car.vmax then v = car.vmax
//
//        // ─────────────────────────────────────────────────────────────────────────
//        // STEP 3: Integrate position using Butcher's method
//        // x(t+τ) = butcher(x(t), v(t+τ), v(t), τ)
//        // ─────────────────────────────────────────────────────────────────────────
//        val x = butcher(car.t_disp, v, car.velocity, rt)
//
//        // ─────────────────────────────────────────────────────────────────────────
//        // STEP 4: Update vehicle state
//        // ─────────────────────────────────────────────────────────────────────────
//        car.o_acc      = car.acc
//        car.acc        = a
//        car.o_velocity = car.velocity
//        car.velocity   = v
//        car.o_t_disp   = car.t_disp
//
//        // ─────────────────────────────────────────────────────────────────────────
//        // STEP 5: Segment-bounded displacement update
//        // ─────────────────────────────────────────────────────────────────────────
//        val dx = x - car.t_disp
//        val proposed_disp = car.disp + dx
//        val new_disp = if proposed_disp <= length then proposed_disp else length
//
//        car.t_disp += new_disp - car.disp
//        car.disp    = new_disp
//    end updateM


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
            iDM (amax, abs(bmax), len, cn.vmax, cn.t_disp, cn.velocity, cp.t_disp, cp.velocity, T, s, del)
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
        val b = abs(bn)             // use positive magnitude of deceleration
        val Δx = xp - xn - sp       // front to rear gap
        val Δv = vn - vp             // approach rate
        val ss = s0 + vn * T + (vn * Δv) / (2.0 * sqrt (an * b))     // desired gap
        an * (1.0 - (vn / Vn) ~^ del - (ss / Δx) ~^ 2.0)            // acceleration
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
