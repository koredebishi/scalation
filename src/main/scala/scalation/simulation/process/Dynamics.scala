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


import scala.math.{abs, max, min, sqrt}
import scalation.mathstat.*
import scalation.dynamics.*
import Vehicle.*


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Enumeration for selecting the ODE integrator type for IDM dynamics.
 *  Used for experimental comparison of numerical integration methods.
 */
enum IntegratorType:
    case DOPRI5      // Dormand-Prince (4,5) adaptive - O(Δt⁵) - current default
    case RK4         // Classic Runge-Kutta 4th order - O(Δt⁴)
    case RK3         // SSPRK3 - O(Δt³)
    case RK2         // Modified Euler (Explicit Midpoint) - O(Δt²)
    case Heun        // Heun's method (Explicit Trapezoidal) - O(Δt²) - Treiber recommended
    case Euler       // Forward Euler - O(Δt¹) - what SUMO uses
    case butcher     // Butcher 5th order - O(Δt⁵)
    case Ballistic   // Kinematic equations - O(Δt²)
end IntegratorType


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
            case KraussDynamics => { KraussDynamics.updateM (car, maxDisp) }
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
    
    /** Integrator type for position update (configurable, default Ballistic) */
    var integratorType: IntegratorType = IntegratorType.Ballistic

    /** Update the vehicle's velocity and position using Gipps' Model.
     *  Position update method is configurable via integratorType.
     *
     * @param car the car/vehicle whose velocity and position is being updated
     */
    def updateM(car: Vehicle, length: Double): Unit =
        val ref = car.myPathNode.ahead
        val car_ahead = if ref != null then ref.elem else null
        val dt = prop("rt")

        // Step 1: compute next velocity using Gipps (discrete rule)
        val v = gipps(car, car_ahead, length) + EPSILON

        // Step 2: update position based on configured integrator type
        val x = integratorType match
            case IntegratorType.Ballistic =>
                // Ballistic: x(t+τ) = x(t) + v(t) * τ
                car.t_disp + car.velocity * dt
            case IntegratorType.butcher =>
                // Butcher 5th order using past velocities
                butcher(car.t_disp, car.velocity, car.o_velocity, dt)
            case _ =>
                // Default to Ballistic for any other type
                car.t_disp + car.velocity * dt

        // Step 3: commit velocity state AFTER position integration
        car.o_velocity = car.velocity
        car.velocity = v

        // Step 4: update displacement (segment-bounded)
        car.o_t_disp = car.t_disp
        val dx = x - car.t_disp

        val new_disp =
            if car.disp + dx <= length then car.disp + dx
            else length

        car.t_disp += new_disp - car.disp
        car.disp = new_disp
    end updateM

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



object KraussDynamics
    extends Dynamics:

    private val debug = debugf("KraussDynamics", false)
    
    /** Krauss stochastic imperfection magnitude (m/s)
     *  Must be small relative to amax*tau to allow net acceleration
     */
    private val sigma = 0.2
    
    /** Uniform random variate for stochastic noise */
    private val noiseRV = scalation.random.Uniform(0.0, sigma)
    
    /** Integrator type for position update (configurable, default Ballistic) */
    var integratorType: IntegratorType = IntegratorType.Ballistic

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the vehicle's velocity and position using Krauss Model.
     *  Position update method is configurable via integratorType.
     *  @param car    the vehicle to update
     *  @param length the segment length
     */
    def updateM(car: Vehicle, length: Double): Unit =
        val ref = car.myPathNode.ahead
        val car_ahead = if ref != null then ref.elem else null
        val dt = prop("rt")

        // Compute leader position and velocity
        val (xp, vp): (Double, Double) =
            if car_ahead == null || car_ahead.segId < car.segId then
                (car.disp + 1000.0, car.vmax)  // phantom leader (free flow)
            else if car_ahead.segId == car.segId then
                (car_ahead.disp, car_ahead.velocity)
            else
                (length + car_ahead.disp, car_ahead.velocity)

        // Step 1: Compute next velocity using Krauss
        val v_new = krauss(
            amax, bmax, len, car.vmax,
            car.disp, car.velocity,
            xp, vp, dt, s
        )

        // Step 2: Position update based on configured integrator type
        val x_new = integratorType match
            case IntegratorType.Ballistic =>
                car.t_disp + car.velocity * dt
            case IntegratorType.butcher =>
                butcher(car.t_disp, car.velocity, car.o_velocity, dt)
            case _ =>
                car.t_disp + car.velocity * dt

        // Step 3: Commit velocity state AFTER position update
        car.o_velocity = car.velocity
        car.velocity = v_new

        // Step 4: Segment-bounded displacement update
        car.o_t_disp = car.t_disp
        val dx = x_new - car.t_disp
        val new_disp = if car.disp + dx <= length then car.disp + dx else length

        car.t_disp += new_disp - car.disp
        car.disp = new_disp
    end updateM

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute next velocity using Krauss car-following model.
     *  Safety is enforced BEFORE stochastic noise is applied.
     *
     *  @param an  max acceleration
     *  @param bn  max deceleration (negative)
     *  @param sp  vehicle length
     *  @param Vn  desired speed
     *  @param xn  follower position
     *  @param vn  follower velocity
     *  @param xp  leader position
     *  @param vp  leader velocity
     *  @param rt  reaction time (timestep)
     *  @param s0  minimum spacing
     */
    private def krauss(an: Double, bn: Double, sp: Double, Vn: Double,
                       xn: Double, vn: Double, xp: Double, vp: Double,
                       rt: Double, s0: Double): Double =
        val b = abs(bn)

        // Netto gap (front-to-rear)
        val gap = xp - xn - sp
        if gap <= 0.0 then return 0.0  // collision avoidance

        // Krauss safe speed: v_safe = sqrt(v_leader^2 + 2*b*(gap - s0))
        val inner = vp * vp + 2.0 * b * (gap - s0)
        val v_safe = sqrt(max(0.0, inner))

        // Acceleration constraint: v_cap = v + a_max * tau
        val v_cap = vn + an * rt

        // Desired speed constraint
        val v_des = Vn

        // Deterministic candidate (minimum of all constraints)
        val v_star = min(v_des, min(v_safe, v_cap))

        // Stochastic imperfection applied AFTER safety
        val eps = noiseRV.gen
        max(0.0, v_star - eps)
    end krauss

end KraussDynamics    
    

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IDMDynamics` object provides equations for the Intelligent Driver Model (IDM)
 *  car-following model.
 *  @see https://en.wikipedia.org/wiki/Intelligent_driver_model
 */

object IDMDynamics
    extends Dynamics:

    private val debug = debugf ("IDMDynamics", true)                // debug function

    private val FREERANGE = 150.0

    /** Configurable integrator type for ODE solving - default Ballistic */
    var integratorType: IntegratorType = IntegratorType.Ballistic

    /** Flag to print integrator type only once per simulation run */
    private var integratorPrinted: Boolean = false

    /** Reset the print flag - call this before each new simulation */
    def resetIntegratorPrintFlag(): Unit = integratorPrinted = false



    /**
     *  @param car     the vehicle to update
     *  @param length  the segment length
     */
    def updateM(car: Vehicle, length: Double): Unit =
        val ref = car.myPathNode.ahead
        val car_ahead = if ref != null then ref.elem else null
        val dt = rt

        // Snapshot leader state (frozen during integration)
        val (x_leader, v_leader): (Double, Double) =                // leader's (position, velocity)
            if car_ahead == null then
                (car.t_disp + 1000.0, car.velocity)                 // no leader: free-flow
            else if car_ahead.segId < car.segId then
                (car.t_disp + 1000.0, car.velocity)                 // leader is behind (stale DLL node): free-flow
            else if car_ahead.t_disp - car.t_disp > FREERANGE then
                (car.t_disp + 1000.0, car.velocity)                 // leader is far ahead: free-flow
            else
                (car_ahead.t_disp, car_ahead.velocity)              // real leader: use it

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 2: Save old state for history tracking
        // ─────────────────────────────────────────────────────────────────────────
        val v_old = car.velocity
        val x_old = car.t_disp

        /**
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
        // STEP 4: Solve the coupled system with selected integrator
        // ─────────────────────────────────────────────────────────────────────────
        val y0 = VectorD(car.t_disp, car.velocity)                      // initial state [x(t), v(t)]

        // Print integrator type ONCE per simulation (from inside updateM, not from caller)
        if !integratorPrinted then
            println(s"[IDMDynamics.updateM] INTEGRATOR IN USE: $integratorType")
            integratorPrinted = true

        // Select integrator based on integratorType setting
        val y1: VectorD = integratorType match
            case IntegratorType.DOPRI5    => DormandPrince.integrateVV(odes, y0, dt)
            case IntegratorType.RK4       => RungeKutta2.rk4.integrateVV(odes, y0, dt)
            case IntegratorType.RK3       => RungeKutta2.rk3.integrateVV(odes, y0, dt)
            case IntegratorType.RK2       => RungeKutta2.rk2.integrateVV(odes, y0, dt)
            case IntegratorType.Heun      => RungeKutta2.heun.integrateVV(odes, y0, dt)
            case IntegratorType.Euler     => RungeKutta2.euler.integrateVV(odes, y0, dt)
            case IntegratorType.butcher   =>
                // Butcher's 5th-order method (J.C. Butcher) - a quadrature rule using historical samples.
                // Apply twice: once for velocity (using acceleration history), once for position (using velocity history).
                // This maintains 5th-order accuracy for BOTH state variables.
                
                // Step 1: Velocity via Butcher using acceleration history: a(t), a(t−τ)
                // v(t+τ) = v(t) + (1/90)(7k1 + 32k3 + 12k4 + 32k5 + 7k6)τ  where k's interpolate a(t-τ) to a(t)
                val a_idm = idmAccel(car.t_disp, car.velocity)        // a(t) from IDM
                val v_new_b = Vehicle.butcher(
                    car.velocity,    // v(t)   - current velocity
                    a_idm,           // a(t)   - current acceleration
                    car.o_acc,       // a(t−τ) - previous acceleration
                    dt
                )
                
                // Step 2: Position via Butcher using velocity history: v(t), v(t−τ)
                // x(t+τ) = x(t) + (1/90)(7k1 + 32k3 + 12k4 + 32k5 + 7k6)τ  where k's interpolate v(t-τ) to v(t)
                val x_new_b = Vehicle.butcher(
                    car.t_disp,      // x(t)   - current position
                    car.velocity,    // v(t)   - current velocity
                    car.o_velocity,  // v(t−τ) - previous velocity
                    dt
                )
                VectorD(x_new_b, v_new_b)
            case IntegratorType.Ballistic =>
                // Ballistic: compute IDM acceleration once, then kinematic update
                val a_idm = idmAccel(car.t_disp, car.velocity)
                val v_ball = car.velocity + a_idm * dt   // v(t + dt) the new velocity
                val x_ball = car.t_disp + car.velocity * dt + 0.5 * a_idm * dt * dt  // x(t + dt)
                VectorD(x_ball, v_ball)  // construct new state vector

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
