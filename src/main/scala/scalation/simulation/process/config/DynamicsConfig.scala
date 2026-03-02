//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Fri Jan 31 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Dynamics Configuration for Traffic Simulation (Physics Only)
 */

package scalation
package simulation
package process
package config

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Enumeration for car-following models.
 */
enum CarFollowingModel:
    case IDM, Gipps, Krause
end CarFollowingModel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Enumeration for ODE solvers.
 *  Must match IntegratorType in Dynamics.scala
 */
enum ODESolverType:
    case Ballistic, Euler, Heun, RK2, RK3, RK4, DOPRI5, Butcher
end ODESolverType

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CFParams` class specifies car-following model parameters.
 *  Parameters are model-agnostic; each model uses what it needs.
 *  @param s0    minimum gap / jam distance (meters)
 *  @param amax  maximum acceleration (m/s^2)
 *  @param bmax  comfortable deceleration (m/s^2, negative value)
 *  @param T     safe time headway (seconds)
 *  @param tau   reaction time / shift parameter (seconds)
 *  @param delta acceleration exponent (IDM only, default 4.0)
 */
case class CFParams (s0: Double = 2.0, amax: Double = 1.0, bmax: Double = -1.5,
                     T: Double = 1.5, tau: Double = 0.6, delta: Double = 4.0):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert parameters to a VectorD for optimization.
     */
    def toVector: VectorD = VectorD (s0, amax, bmax, T, tau)

end CFParams


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CFParams` companion object provides factory methods.
 */
object CFParams:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create CFParams from a VectorD (for optimization).
     *  @param v  parameter vector [s0, amax, bmax, T, tau]
     */
    def fromVector (v: VectorD): CFParams =
        require (v.dim >= 5, s"CFParams.fromVector: need 5 params, got ${v.dim}")
        CFParams (s0 = v(0), amax = v(1), bmax = v(2), T = v(3), tau = v(4))
    end fromVector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** IDM literature defaults (Treiber & Kesting, 2013).
     */
    // val idmDefault: CFParams = CFParams (s0 = 2.0, amax = 1.0, bmax = -1.5, T = 1.5, tau = 0.6)  // literature defaults (Treiber & Kesting, 2013)
    val idmDefault: CFParams = CFParams (s0 = 5.0, amax = 4.0, bmax = -2.0, T = 3.0, tau = 0.5)   // empirical defaults matching Vehicle.def_prop (ANNSIM 2026)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Gipps model defaults.
     */
    val gippsDefault: CFParams = CFParams (s0 = 2.0, amax = 1.5, bmax = -3.0, T = 1.0, tau = 0.67)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Krause model defaults.
     *  Using IDM empirical params since Krauss-specific values underperform.
     */
    val krauseDefault: CFParams = CFParams (s0 = 5.0, amax = 4.0, bmax = -2.0, T = 3.0, tau = 0.5)

end CFParams


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DynamicsConfig` class specifies vehicle dynamics for simulation.
 *  @param carFollowing    the car-following model (IDM, Gipps, Krause)
 *  @param odeSolver       the ODE solver type
 *  @param cfParams        the car-following model parameters
 *  @param laneChangeProb  probability of lane change when conditions met
 *  @param vmax            maximum velocity (m/s)
 */
case class DynamicsConfig (carFollowing: CarFollowingModel = CarFollowingModel.IDM,
                           odeSolver: ODESolverType = ODESolverType.Ballistic,
                           cfParams: CFParams = CFParams.idmDefault,
                           laneChangeProb: Double = 0.6,
                           vmax: Double = 35.0)

end DynamicsConfig


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DynamicsConfig` companion object provides predefined dynamics configurations.
 */
object DynamicsConfig:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** IDM with Ballistic solver (default per ANNSIM 2026 findings).
     */
    val idmDefault: DynamicsConfig = DynamicsConfig (
        carFollowing = CarFollowingModel.IDM,
        odeSolver = ODESolverType.Ballistic,
        cfParams = CFParams.idmDefault
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Gipps with Ballistic solver.
     */
    val gippsDefault: DynamicsConfig = DynamicsConfig (
        carFollowing = CarFollowingModel.Gipps,
        odeSolver = ODESolverType.Ballistic,
        cfParams = CFParams.gippsDefault
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Krause with Ballistic solver.
     */
    val krauseDefault: DynamicsConfig = DynamicsConfig (
        carFollowing = CarFollowingModel.Krause,
        odeSolver = ODESolverType.Ballistic,
        cfParams = CFParams.krauseDefault
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create IDM configuration with custom parameters.
     *  @param params        the car-following parameters
     *  @param laneChangeP   the lane change probability
     *  @param maxVelocity   the maximum velocity (m/s)
     */
    def idm (params: CFParams = CFParams.idmDefault,
             laneChangeP: Double = 0.6,
             maxVelocity: Double = 35.0): DynamicsConfig =
        DynamicsConfig (
            carFollowing = CarFollowingModel.IDM,
            odeSolver = ODESolverType.Ballistic,
            cfParams = params,
            laneChangeProb = laneChangeP,
            vmax = maxVelocity
        )
    end idm

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create Gipps configuration with custom parameters.
     *  @param params        the car-following parameters
     *  @param laneChangeP   the lane change probability
     *  @param maxVelocity   the maximum velocity (m/s)
     */
    def gipps (params: CFParams = CFParams.gippsDefault,
               laneChangeP: Double = 0.6,
               maxVelocity: Double = 35.0): DynamicsConfig =
        DynamicsConfig (
            carFollowing = CarFollowingModel.Gipps,
            odeSolver = ODESolverType.Ballistic,
            cfParams = params,
            laneChangeProb = laneChangeP,
            vmax = maxVelocity
        )
    end gipps

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create Krause configuration with custom parameters.
     *  @param params        the car-following parameters
     *  @param laneChangeP   the lane change probability
     *  @param maxVelocity   the maximum velocity (m/s)
     */
    def krause (params: CFParams = CFParams.krauseDefault,
                laneChangeP: Double = 0.6,
                maxVelocity: Double = 35.0): DynamicsConfig =
        DynamicsConfig (
            carFollowing = CarFollowingModel.Krause,
            odeSolver = ODESolverType.Ballistic,
            cfParams = params,
            laneChangeProb = laneChangeP,
            vmax = maxVelocity
        )
    end krause

end DynamicsConfig
