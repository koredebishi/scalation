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
    /** Find the leader of the given vehicle using per-VTransport DLLs.
     *  Step 1: within-segment leader (myPathNode.ahead).
     *  Step 2: cross-boundary — next segment's DLL tail (mainline vehicles).
     *  Step 3: ramp dual-leader — peek at mainline merge target (Treiber §11.3).
     *  @param car  the vehicle whose leader we need
     *  @return the leader vehicle, or null if free-flow
     */
    protected def findLeader (car: Vehicle): Vehicle =
        val onRamp = car.myRamp != null                                   // DIAG

        if car.myPathNode == null then return null                        // between DLLs → free-flow

        // Step 1: within-segment leader (O(1) DLL lookup)
        val ref = car.myPathNode.ahead
        if ref != null then
//            if onRamp then
//                val ldr = ref.elem
//                val mePath = if car.myPathway != null then car.myPathway.name else "null"
//                val meRamp = if car.myRamp != null then car.myRamp.name else "null"
//                val ldrPath = if ldr.myPathway != null then ldr.myPathway.name else "null"
//                val ldrRamp = if ldr.myRamp != null then ldr.myRamp.name else "null"
//                println(f"[findLeader] ${car.displayLabel}%-12s STEP1 seg=${car.segId} path=$mePath ramp=$meRamp | ldr=${ldr.displayLabel} ldr.seg=${ldr.segId} ldr.path=$ldrPath ldr.ramp=$ldrRamp | me.disp=${car.disp}%.2f me.v=${car.velocity}%.2f ldr.disp=${ldr.disp}%.2f ldr.v=${ldr.velocity}%.2f")
//            end if  // DIAG
            return ref.elem

        // Step 2: cross-boundary — look at next segment's DLL tail
        val pw = car.myPathway
        if pw != null then
            val segs = pw.seg
            val nextIdx = car.segId + 1
            if nextIdx < segs.length && segs(nextIdx) != null then
                return segs(nextIdx).getLast
        end if

        // Step 3: ramp → peek at mainline merge segment (dual-leader)
        val ramp = car.myRamp
        if ramp != null && ramp.targetPathway != null && ramp.targetSegId >= 0 then
            val targetVT = ramp.targetPathway.seg(ramp.targetSegId)
            if targetVT != null then
                val ml = targetVT.getLast
                if ml != null then return ml
            end if
        end if

        null
    end findLeader

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
    /** Update the vehicle's velocity and position using Gipps' Model.
     *  All gap and position computations use segment-local `disp` — consistent with
     *  per-segment DLLs where the leader is always in the same or adjacent segment.
     *  `t_disp` is updated as a derived statistic only.
     */
    def updateM(car: Vehicle, length: Double): Unit =
        val car_ahead = findLeader (car)
        val dt = prop("rt")

        // Step 1: compute next velocity using Gipps (discrete rule)
        val v = gipps(car, car_ahead, length) + EPSILON

        // Step 2: update position in segment-local coordinates
        val x = integratorType match
            case IntegratorType.Ballistic =>
                car.disp + car.velocity * dt
            case IntegratorType.butcher =>
                butcher(car.disp, car.velocity, car.o_velocity, dt)
            case _ =>
                car.disp + car.velocity * dt

        // Step 3: commit velocity state AFTER position integration
        car.o_velocity = car.velocity
        car.velocity = v

        // Step 4: segment-bounded + collision-free displacement update
        car.o_t_disp = car.t_disp

        // --- Collision-free position clamp (SUMO/VISSIM/Aimsun standard practice) ---
        // Even if the velocity model overshoots, clamp position to maintain gap ≥ s0
        // behind leader.  Uses segment-local leader position (cross-seg already handled).
        var x_safe = x
        if car_ahead != null then
            val xl = if car_ahead.segId == car.segId then car_ahead.disp
                     else length + car_ahead.disp
            if xl - x_safe - len < s then x_safe = xl - len - s      // park at min gap behind leader
        end if
        // val new_disp = max(0.0, if x <= length then x else length)   // OLD: no collision-free clamp
        val new_disp = max(0.0, if x_safe <= length then x_safe else length)

        car.t_disp += new_disp - car.disp                           // derive t_disp from clamped delta
        car.disp = new_disp
    end updateM

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the velocity of the vehicle based on Gipps' model for a vehicle and its predecessor.
     *  With per-segment DLLs, leader is always same-seg or next-seg.  All positions are segment-local.
     *  @param cn  the current vehicle
     *  @param cp  the predecessor of the current vehicle (from findLeader — always same or next segment)
     */
    def gipps (cn: Vehicle, cp: Vehicle, length: Double): Double =
        if cp == null then
            gipps (amax, bmax, len, cn.vmax, cn.disp, cn.velocity, cn.disp + 1000, cn.vmax, prop("rt"))
        else
            // Leader is in same segment or adjacent next segment
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
     *  All gap and position computations use segment-local `disp` — consistent with
     *  per-segment DLLs where the leader is always in the same or adjacent segment.
     *  `t_disp` is updated as a derived statistic only.
     *  @param car    the vehicle to update
     *  @param length the segment length
     */
    def updateM(car: Vehicle, length: Double): Unit =
        val car_ahead = findLeader (car)
        val dt = prop("rt")

        // Compute leader position and velocity (segment-local)
        val (xp, vp): (Double, Double) =
            if car_ahead == null then
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

        // Step 2: Position update in segment-local coordinates
        val x_new = integratorType match
            case IntegratorType.Ballistic =>
                car.disp + car.velocity * dt
            case IntegratorType.butcher =>
                butcher(car.disp, car.velocity, car.o_velocity, dt)
            case _ =>
                car.disp + car.velocity * dt

        // Step 3: Commit velocity state AFTER position update
        car.o_velocity = car.velocity
        car.velocity = v_new

        // Step 4: Segment-bounded + collision-free displacement update
        car.o_t_disp = car.t_disp

        // --- Collision-free position clamp (SUMO/VISSIM/Aimsun standard practice) ---
        var x_safe = x_new
        if car_ahead != null && xp - x_safe - len < s then
            x_safe = xp - len - s                                    // park at min gap behind leader
        end if
        // val new_disp = max(0.0, if x_new <= length then x_new else length) // OLD: no collision-free clamp
        val new_disp = max(0.0, if x_safe <= length then x_safe else length)

        car.t_disp += new_disp - car.disp                           // derive t_disp from clamped delta
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
     *  Update the vehicle's velocity and position using the Intelligent Driver Model.
     *  All gap and position computations use segment-local `disp` — consistent with
     *  per-segment DLLs where the leader is always in the same or adjacent segment.
     *  `t_disp` is updated as a derived statistic only.
     *  @param car     the vehicle to update
     *  @param length  the segment length
     */
    def updateM(car: Vehicle, length: Double): Unit =
        val car_ahead = findLeader (car)
        val dt = rt

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 1: Snapshot leader state in SEGMENT-LOCAL coordinates.
        // With per-segment DLLs, findLeader returns same-seg or next-seg only.
        // ─────────────────────────────────────────────────────────────────────────
        val (x_leader, v_leader): (Double, Double) =
            if car_ahead == null then
                (car.disp + 1000.0, car.velocity)                    // no leader: free-flow
            else
                val xl = if car_ahead.segId == car.segId then car_ahead.disp
                         else length + car_ahead.disp                // next segment
                // Fix 2: skip FREERANGE for ramp cars — ramp must always track the
                // mainline dual-leader to decelerate before merge (Treiber §11.3).
                if xl - car.disp > FREERANGE && car.myRamp == null then
                    (car.disp + 1000.0, car.velocity)                // leader far ahead: free-flow
                else
                    (xl, car_ahead.velocity)                          // real leader

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 2: Save old state for history tracking
        // ─────────────────────────────────────────────────────────────────────────
        val v_old = car.velocity
        val x_old = car.disp                                          // segment-local

        /**
         * @param x_n  Current position of driver n (segment-local, from ODE solver trial values)
         * @param v_n  Current velocity of driver n (from ODE solver trial values)
         * @return     IDM acceleration clamped to physical bounds [-b_emergency, amax]
        */
        def idmAccel(x_n: Double, v_n: Double): Double =
            val b = abs(bmax)                                            // comfortable decel for IDM s* formula

            var a = iDM(amax, b, len, car.vmax, x_n, v_n,
                        x_leader, v_leader, T, s, del)

            val a_raw = a                                                 // save before clamp
            if a.isNaN || a.isInfinity then a = 0.0
//            if a < -b then a = -b                                     // OLD: comfortable clamp (bmax = 2.0)
            val a_floor = max(b_emergency, -v_n / dt)                    // SUMO-style: can't decel past stopping
            if a < a_floor then a = a_floor                              // allows b_emergency at high v, gentler at low v
            if a > amax then a = amax
            a
        end idmAccel

        // Array of derivative functions: [dx/dt, dv/dt]
        val odes: Array[DerivativeV] = Array(
            (t: Double, y: VectorD) => y(1),                    // dx/dt = v (velocity)
            (t: Double, y: VectorD) => idmAccel(y(0), y(1))     // dv/dt = a (IDM acceleration)
        )

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 4: Solve the coupled system in segment-local coordinates
        // ─────────────────────────────────────────────────────────────────────────
        val y0 = VectorD(car.disp, car.velocity)                      // segment-local initial state

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
                val a_idm = idmAccel(car.disp, car.velocity)
                val v_new_b = Vehicle.butcher(
                    car.velocity,    // v(t)
                    a_idm,           // a(t)
                    car.o_acc,       // a(t−τ)
                    dt
                )
                val x_new_b = Vehicle.butcher(
                    car.disp,        // x(t)   — segment-local
                    car.velocity,    // v(t)
                    car.o_velocity,  // v(t−τ)
                    dt
                )
                VectorD(x_new_b, v_new_b)
            case IntegratorType.Ballistic =>
                val a_idm = idmAccel(car.disp, car.velocity)
                val v_ball = car.velocity + a_idm * dt
                val x_ball = car.disp + car.velocity * dt + 0.5 * a_idm * dt * dt
                VectorD(x_ball, v_ball)

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 5: Extract and clamp results
        // ─────────────────────────────────────────────────────────────────────────
        val x_new = y1(0)                                                 // new segment-local position
        var v_new = y1(1)                                                 // new velocity

        // Physical constraints: velocity must be non-negative and bounded
        if v_new < 0.0 then v_new = 0.0                                 // no backward motion
        if v_new > car.vmax then v_new = car.vmax                       // respect max speed

        // Back-calculate acceleration for state storage
        val a = (v_new - v_old) / dt

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 6: Update vehicle state
        // ─────────────────────────────────────────────────────────────────────────
        car.o_acc      = car.acc
        car.acc        = a
        car.o_velocity = v_old
        car.velocity   = v_new
        car.o_t_disp   = car.t_disp                                     // track old t_disp for stats

        // ─────────────────────────────────────────────────────────────────────────
        // STEP 7: Segment-bounded + collision-free displacement update
        // ─────────────────────────────────────────────────────────────────────────
        // --- Collision-free position clamp (SUMO/VISSIM/Aimsun standard practice) ---
        // Even if IDM + integrator overshoots, clamp position to maintain gap ≥ s0
        // behind leader.  Uses x_leader from STEP 1 (cross-seg already resolved).
        // Breaks deadlock: overshoot → clamp to safe gap → IDM gives positive accel
        // next timestep when leader moves.  No backward motion possible.
        var x_safe = x_new
        if car_ahead != null && x_leader - x_safe - len < s then     // would violate min gap?
            x_safe = x_leader - len - s                               // park at min gap behind leader
        end if
        // val new_disp = max(0.0, if x_new <= length then x_new else length) // OLD: no collision-free clamp
        val new_disp = max(0.0, if x_safe <= length then x_safe else length)

        car.t_disp += new_disp - car.disp                               // derive t_disp from clamped delta
        car.disp    = new_disp

        // DIAG: final state for ramp vehicles — detect overtaking
        // (disabled — high-volume console output starves the animator's EDT)
//        if car.myRamp != null then
//            val ldrD = if car_ahead != null then car_ahead.disp else -1.0
//            val gapF = if car_ahead != null then ldrD - new_disp - len else 999.0
//            val flag = if car_ahead != null && car_ahead.segId == car.segId && new_disp > ldrD then " *** OVERTOOK ***" else ""
//            val mePath = if car.myPathway != null then car.myPathway.name else "null"
//            val meRamp = if car.myRamp != null then car.myRamp.name else "null"
//            val ldrPath = if car_ahead != null && car_ahead.myPathway != null then car_ahead.myPathway.name else "null"
//            val ldrRamp = if car_ahead != null && car_ahead.myRamp != null then car_ahead.myRamp.name else "null"
//            val ldrName = if car_ahead != null then car_ahead.displayLabel else "null"
//            println(f"[STEP7] ${car.displayLabel}%-12s seg=${car.segId} path=$mePath ramp=$meRamp | ldr=$ldrName ldr.seg=${if car_ahead != null then car_ahead.segId else -1} ldr.path=$ldrPath ldr.ramp=$ldrRamp | disp=${new_disp}%.2f v=${car.velocity}%.2f ldrD=${ldrD}%.2f gap=${gapF}%.2f$flag")
//        end if  // DIAG
    end updateM

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the acceleration of the vehicle based on the Intelligent Driver Model
     *  for a vehicle and its predecessor.  Uses segment-local `disp` for gap computation.
     *  NOTE: updateM uses the raw iDM directly; this is a convenience for external callers.
     *  @param cn   the current vehicle
     *  @param cp   the predecessor of the current vehicle
     *  @param del  the acceleration exponenent (defualts to 4)
     */
    def iDM (cn: Vehicle, cp: Vehicle, del: Double = 4.0): Double =
        if cp == null then
            iDMFree (amax, cn.velocity, cn.vmax, del)
        else
            iDM (amax, abs(bmax), len, cn.vmax, cn.disp, cn.velocity, cp.disp, cp.velocity, T, s, del)
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


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MOBIL` object implements the MOBIL (Minimize Overall Braking Induced by
 *  Lane changes) lane-change decision model (Treiber & Kesting, 2007).
 *
 *  MOBIL uses IDM accelerations to evaluate whether a lane change is both
 *  safe and beneficial.  Two criteria must be satisfied:
 *
 *  '''Safety criterion''' (hard constraint):
 *  {{{
 *      ã_f  ≥  -b_safe
 *  }}}
 *  where `ã_f` is the IDM acceleration of the new follower in the target lane
 *  after the subject vehicle cuts in, and `b_safe` is the maximum acceptable
 *  braking imposed on that follower.
 *
 *  '''Incentive criterion''' (soft):
 *  {{{
 *      ã_s − a_s  +  p · (ã_f − a_f)  >  Δa_th
 *  }}}
 *  where:
 *    - `a_s`   = subject's IDM acceleration in current lane (with current leader)
 *    - `ã_s`  = subject's IDM acceleration in target lane (with target leader)
 *    - `a_f`   = target follower's current IDM acceleration (before cut-in)
 *    - `ã_f`  = target follower's IDM acceleration after cut-in (subject becomes leader)
 *    - `p`     = politeness factor ∈ [0,1].  p=0 selfish, p=1 fully altruistic
 *    - `Δa_th` = acceleration threshold to prevent frivolous lane changes
 *
 *  @see Treiber, M. & Kesting, A. (2007). "Modeling lane-changing decisions with MOBIL."
 *       Traffic and Granular Flow '07, pp. 211–221, Springer.
 *  @see Treiber, M. & Kesting, A. (2013). Traffic Flow Dynamics. Springer, Ch. 11.
 */
object MOBIL:

    import Vehicle.{amax, bmax, len, T, s, del, p_mobil, da_th, b_safe}

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute IDM acceleration for a hypothetical leader-follower pair.
     *  Uses the raw IDM equation: a = amax * [1 - (v/v0)^δ - (s*(v,Δv)/Δx)^2]
     *  @param follower  the vehicle whose acceleration we compute
     *  @param leader    the vehicle ahead (null → free-flow)
     *  @param segLen    segment length (for cross-segment gap adjustment)
     *  @return IDM acceleration (m/s²)
     */
    private def idmAccelFor (follower: Vehicle, leader: Vehicle, segLen: Double): Double =
        if follower == null then return 0.0
        val an = amax
        val bn = abs (bmax)
        if leader == null then
            // free-flow: a = amax * [1 - (v/v0)^δ]
            return an * (1.0 - (follower.velocity / follower.vmax) ~^ del)
        end if
        // bumper-to-bumper gap (segment-local)
        val xl = if leader.segId == follower.segId then leader.disp
                 else segLen + leader.disp
        val gap = xl - follower.disp - len
        if gap <= 0.0 then return -bn                     // touching → full comfortable braking
        val dv  = follower.velocity - leader.velocity      // approach rate
        val ss  = s + follower.velocity * T + (follower.velocity * dv) / (2.0 * sqrt (an * bn))
        an * (1.0 - (follower.velocity / follower.vmax) ~^ del - (ss / gap) ~^ 2.0)
    end idmAccelFor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate MOBIL for a single candidate target lane.
     *  @param car      the subject vehicle considering a lane change
     *  @param route    the Route containing all Pathways
     *  @param seg      the segment index the vehicle is currently in
     *  @param curLane  the vehicle's current lane index
     *  @param tgtLane  the candidate target lane index
     *  @param segLen   the segment length
     *  @return the MOBIL incentive value (positive = beneficial), or -∞ if unsafe
     */
    private def mobilIncentive (car: Vehicle, route: Route, seg: Int,
                                curLane: Int, tgtLane: Int, segLen: Double): Double =
        // --- Target lane neighbors ---
        val tgtVT = route.pathway(tgtLane).seg(seg)
        if tgtVT == null then return Double.MinValue

        // Leader in target lane: last vehicle in this segment's DLL (ahead of insertion point)
        val tgtLeader   = tgtVT.getLast
        // Follower in target lane: first vehicle in this segment's DLL (behind insertion point)
        val tgtFollower = tgtVT.getFirst

        // --- Current lane leader (same as findLeader would give) ---
        val curVT = route.pathway(curLane).seg(seg)
        val curLeader = if curVT != null then
            val ref = car.myPathNode
            if ref != null && ref.ahead != null then ref.ahead.elem else null
        else null

        // --- 4 IDM acceleration computations ---
        // a_s:  subject's acceleration in CURRENT lane (with current leader)
        val a_s  = idmAccelFor (car, curLeader, segLen)

        // ã_s: subject's acceleration in TARGET lane (with target leader)
        val a_s_tilde = idmAccelFor (car, tgtLeader, segLen)

        // a_f:  target follower's CURRENT acceleration (before cut-in)
        val a_f  = idmAccelFor (tgtFollower, tgtLeader, segLen)

        // ã_f: target follower's acceleration AFTER cut-in (subject becomes their new leader)
        val a_f_tilde = idmAccelFor (tgtFollower, car, segLen)

        // --- Safety criterion: ã_f ≥ -b_safe ---
        if a_f_tilde < -b_safe then return Double.MinValue

        // --- Incentive criterion: ã_s - a_s + p·(ã_f - a_f) > Δa_th ---
        val incentive = (a_s_tilde - a_s) + p_mobil * (a_f_tilde - a_f)
        incentive
    end mobilIncentive

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check both adjacent lanes and return the best lane-change direction.
     *  Called every timestep from VTransport.move() after updateV().
     *  @param car    the vehicle evaluating lane change
     *  @param route  the parent Route
     *  @param seg    the current segment index
     *  @param lane   the current lane index
     *  @return -1 (move to lane-1), 0 (stay), +1 (move to lane+1)
     */
    def checkLaneChange (car: Vehicle, route: Route, seg: Int, lane: Int): Int =
        if car.myRamp != null then return 0                    // ramp vehicles don't lane-change
        val segLen = route.pathway(lane).seg(seg).length

        var bestDir   = 0
        var bestValue = da_th                                  // must exceed threshold to trigger

        // Check left (lane - 1)
        if lane - 1 >= 0 && route.laneExistsAt (lane - 1, seg) then
            val inc = mobilIncentive (car, route, seg, lane, lane - 1, segLen)
            if inc > bestValue then
                bestDir   = -1
                bestValue = inc
            end if
        end if

        // Check right (lane + 1)
        if lane + 1 < route.maxLanes && route.laneExistsAt (lane + 1, seg) then
            val inc = mobilIncentive (car, route, seg, lane, lane + 1, segLen)
            if inc > bestValue then
                bestDir   = +1
                bestValue = inc
            end if
        end if

        bestDir
    end checkLaneChange

end MOBIL

