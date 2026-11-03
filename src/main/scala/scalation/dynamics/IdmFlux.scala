//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  <you>
 *  @version 2.0
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Equilibrium Fundamental Diagram (Flux Table) derived from IDM
 */

package scalation
package dynamics

import scala.math.{abs, min, pow, sqrt}

import scalation.mathstat.VectorD


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IdmFlux` class builds the fundamental diagram from the EXACT equilibrium
 *  solution of the Intelligent Driver Model, so that the macroscopic(meso) model and
 *  the microscopic model describe the same road with the same parameters.
 *
 *  Page numbers are from Treiber & Kesting, Traffic Flow Dynamics, 2nd ed., 2025.
 *
 *  IDM acceleration (Eqs. 12.22-12.23, p. 254):
 *      dv/dt = a [ 1 - (v/v0)^delta - (s*(v, dv) / s)^2 ]
 *      s*    = s0 + max (0, v*tau + v*dv / (2 sqrt (a b)))
 *  where v = speed, s = gap to the leader, dv = v - leader's speed, s* = desired gap,
 *  a = maximum acceleration, b = comfortable deceleration.
 *
 *  At equilibrium (p. 218) dv/dt = 0 and dv = 0, so s* / s = sqrt (1 - (v/v0)^delta) and
 *
 *      s_e (v) = (s0 + v tau) / sqrt (1 - (v/v0)^delta)                    (1)  gap at speed v (Eq. 12.29, p. 261)
 *      rho (v) = 1 / (s_e (v) + len)                                       (2)  density at speed v (Eq. 11.16, p. 219)
 *      q   (v) = rho (v) * v                                               (3)  flow at speed v (Eq. 11.18, p. 219)
 *
 *  Note that (1) is NOT simply s0 + v tau.  Dropping the free-acceleration term
 *  gives Newell's triangular diagram, which overstates
 *  capacity by roughly 15% at typical freeway parameters.  The exact form is
 *  used here because a mutation to tau, s0 or v0 in the microscopic model must
 *  move the macroscopic diagram too, or the cross-fidelity check goes blind to
 *  exactly the fault family it exists to catch.
 *
 *  Since rho (v) is strictly decreasing on [0, v0), the relation is inverted by
 *  bisection onto a uniform rho grid once at construction, then interpolated.
 *  The inverse is v (rho), the speed at density rho.
 *
 *  @param v0     the free-flow (desired) speed in m/s
 *  @param tau    the desired time headway in sec
 *  @param s0     the minimum bumper-to-bumper gap in m
 *  @param len    the vehicle length in m
 *  @param delta  the free acceleration exponent (4 is standard)
 *  @param nTab   the number of tabulated densities
 */
class IdmFlux (val v0: Double = 30.0, val tau: Double = 1.5, val s0: Double = 2.0,
               val len: Double = 5.0, val delta: Double = 4.0, nTab: Int = 2001):

    val rho_m = 1.0 / (s0 + len)                                // jam density, at v = 0
    private val drho = rho_m / (nTab - 1)                       // grid spacing
    private val rhoT = new VectorD (nTab)                       // tabulated densities
    private val qT   = new VectorD (nTab)                       // tabulated flows
    private val vT   = new VectorD (nTab)                       // tabulated speeds

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the IDM equilibrium gap at speed v, equation (1).
     */
    def gapEq (v: Double): Double = (s0 + v * tau) / sqrt (1.0 - pow (v / v0, delta))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the IDM equilibrium density at speed v, equation (2).
     */
    def rhoEq (v: Double): Double = 1.0 / (gapEq (v) + len)            // equation (2)

    // IDM gives rho(v), the density at speed v, so construct its inverse v(rho), the speed
    // at density rho, on a density grid.
    cfor (0, nTab) { k =>
        val r = k * drho
        rhoT(k) = r
        if k == 0 then
            vT(0) = v0                                          // empty road: free-flow speed
            qT(0) = 0.0
        else
            var lo = 0.0; var hi = v0 * (1.0 - 1E-12)
            cfor (0, 100) { _ =>                                // bisection finds the speed for density r
                val mid = 0.5 * (lo + hi)                       // mid is the candidate speed
                if rhoEq (mid) > r then lo = mid else hi = mid
            }
            vT(k) = 0.5 * (lo + hi)                             // numerical inverse: v(r), the speed at density r
            qT(k) = r * vT(k)                                   // traffic identity: q = rho * v
    }

    private val kc = qT.argmax ()                               // index of capacity
    val rho_c = rhoT(kc)                                        // density at capacity
    val q_max = qT(kc)                                          // capacity: largest flow in the table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the flow q at density rho by linear interpolation in the table.
     * this is the flow formular: q (rho) = rho * v (rho), the flow at density rho
     */
    def q (rho: Double): Double =
        val r = min (max0 (rho), rho_m)
        val x = r / drho
        val k = min (x.toInt, nTab - 2)
        val f = x - k
        qT(k) * (1.0 - f) + qT(k + 1) * f
    end q


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the equilibrium speed at density rho by interpolating v(rho), the speed at density rho.
     * this is the speed formular, and the one the meso link reads
     *
     * */
    def speed (rho: Double): Double =
        val r = min (max0 (rho), rho_m)
        val x = r / drho
        val k = min (x.toInt, nTab - 2)
        val f = x - k
        vT(k) * (1.0 - f) + vT(k + 1) * f
    end speed
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Read-only views of the table, for plotting and for comparing one diagram
    // against another on a common grid.  Copies, so a caller cannot corrupt the
    // table the interpolators read.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def rhoGrid: VectorD = rhoT.copy                            // tabulated densities (veh/m)
    def qGrid: VectorD   = qT.copy                              // tabulated flows (veh/s)
    def vGrid: VectorD   = vT.copy                              // tabulated speeds (m/s)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the same diagram (tau, s0, len, delta, grid) for another desired
     *  speed v, e.g., one vehicle's own v0.  Tables are built once and reused.
     *  @param v  the desired (free-flow) speed v0 (m/s)
     */
    def withV0 (v: Double): IdmFlux =
        if v == v0 then this else IdmFlux.of (v, tau, s0, len, delta, nTab)
    end withV0

end IdmFlux


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IdmFlux` companion object keeps one table per parameter set, so a family
 *  of diagrams differing only in v0 (one per vehicle desired speed) is built once.
 */
object IdmFlux:

    private val tables = scala.collection.mutable.Map [(Double, Double, Double, Double, Double, Int), IdmFlux] ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the `IdmFlux` for these parameters, building it the first time only.
     */
    def of (v0: Double, tau: Double, s0: Double, len: Double, delta: Double, nTab: Int = 2001): IdmFlux =
        tables.getOrElseUpdate ((v0, tau, s0, len, delta, nTab), new IdmFlux (v0, tau, s0, len, delta, nTab))
    end of

end IdmFlux




//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Check the IDM equilibrium table independently of the meso model.
 *  The test prints representative density, speed, and flow rows so that the
 *  analytical construction and its numerical inverse can be inspected directly.
 *
 *  > runMain scalation.dynamics.idmFluxTest
 */
@main def idmFluxTest (): Unit =

    // Build the table from one explicit set of IDM parameters.
    val fd = new IdmFlux (v0 = 33.528, tau = 1.0, s0 = 2.0, len = 4.0, delta = 4.0)
    val rho = fd.rhoGrid
    val speed = fd.vGrid
    val flow = fd.qGrid

    // At zero density, the equilibrium speed is the desired IDM speed.
    require (abs (rho(0)) < 1E-12, "the empty-road density must be zero")
    require (abs (speed(0) - fd.v0) < 1E-12, "the empty-road speed must equal v0")
    // At jam density, the table reaches the maximum physically admissible density.
    require (abs (rho(rho.dim - 1) - fd.rho_m) < 1E-12,
        "the table must reach the IDM jam density")
    require (speed(speed.dim - 1) >= 0.0, "equilibrium speed must not be negative")

    // The IDM equilibrium speed must fall as density increases.
    var monotone = true
    cfor (1, speed.dim) { k => if speed(k) > speed(k - 1) + 1E-10 then monotone = false }
    require (monotone, "IDM equilibrium speed must decrease with density")

    // Every stored flow row must satisfy the traffic identity q = rho * v.
    cfor (0, flow.dim) { k =>
        require (abs (flow(k) - rho(k) * speed(k)) < 1E-10,
            s"flow identity failed at table row $k")
    }

    // Print selected rows so the inverse construction can be checked by inspection.
    banner ("IDM equilibrium table test")
    println (s"parameters: v0=${fd.v0} m/s, T=${fd.tau} s, s0=${fd.s0} m, " +
             s"length=${fd.len} m, delta=${fd.delta}")
    println (s"jam density (rho_m): ${fd.rho_m} veh/m " +
             s"(${fd.rho_m * 1000.0} veh/km)")
    println ("  maximum represented density; vehicles are at the minimum equilibrium spacing")
    println (s"critical density (rho_c): ${fd.rho_c} veh/m " +
             s"(${fd.rho_c * 1000.0} veh/km)")
    println ("  density at which the equilibrium flow is highest")
    println (s"maximum flow (q_max): ${fd.q_max} veh/s " +
             s"(${fd.q_max * 3600.0} veh/h)")
    println ("  traffic capacity corresponding to the critical density")
    println ("density (veh/m) | speed (m/s) | flow (veh/s)")
    for target <- Array (0.0, 0.01, 0.012, 0.02, fd.rho_c, fd.rho_m) do
        println (f"${target}%.9f | ${fd.speed (target)}%.9f | ${fd.q (target)}%.9f")
    end for
    println ()
    println (s"full lookup table (${rho.dim} rows):")
    println ("row | density (veh/m) | speed (m/s) | flow (veh/s)")
    for k <- 0 until rho.dim do
        println (f"$k%4d | ${rho(k)}%.9f | ${speed(k)}%.9f | ${flow(k)}%.9f")
    end for
    banner ("IDM equilibrium table test: PASS")

end idmFluxTest
