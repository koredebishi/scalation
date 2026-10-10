//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  <you>
 *  @version 2.0
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    LWR Traffic Flow PDE with Fundamental Diagram derived from IDM
 */

package scalation
package dynamics

import scala.math.{abs, ceil, max, min, pow, sqrt}

import scalation.mathstat.{MatrixD, Plot, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FundamentalDiagram` trait defines the flow-density relationship q(rho)
 *  required by the LWR macroscopic traffic flow model, together with the
 *  quantities a Godunov (Cell Transmission) flux needs: the critical density,
 *  the capacity, and the sending/receiving flows.
 */
trait FundamentalDiagram:

    def rho_m: Double                                           // jam density (veh/m)
    def rho_c: Double                                           // critical density (veh/m)
    def q_max: Double                                           // capacity (veh/s)
    def q (rho: Double): Double                                 // flow at density rho
    def dq (rho: Double): Double                                // characteristic speed dq/drho
    def maxWaveSpeed: Double                                    // max |dq/drho|, for CFL

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sending flow (demand) of an upstream cell at density rho.
     */
    def demand (rho: Double): Double = if rho <= rho_c then q (rho) else q_max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the receiving flow (supply) of a downstream cell at density rho.
     */
    def supply (rho: Double): Double = if rho <= rho_c then q_max else q (rho)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the analytical shock speed between two states, from the
     *  Rankine-Hugoniot condition  s = [q] / [rho].
     */
    def shockSpeed (rhoL: Double, rhoR: Double): Double =
        if abs (rhoR - rhoL) < 1E-12 then dq (rhoL)
        else (q (rhoR) - q (rhoL)) / (rhoR - rhoL)
    end shockSpeed

end FundamentalDiagram


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IdmFlux` class builds the fundamental diagram from the EXACT equilibrium
 *  solution of the Intelligent Driver Model, so that the macroscopic(meso) model and
 *  the microscopic model describe the same road with the same parameters.
 *
 *  IDM acceleration:
 *      dv/dt = a [ 1 - (v/v0)^delta - (s*(v, dv) / s)^2 ]
 *      s*    = s0 + max (0, v*tau + v*dv / (2 sqrt (a b)))
 *
 *  At equilibrium dv/dt = 0 and dv = 0, so s* / s = sqrt (1 - (v/v0)^delta) and
 *
 *      s_e (v) = (s0 + v tau) / sqrt (1 - (v/v0)^delta)                    (1)
 *      rho (v) = 1 / (s_e (v) + len)                                       (2)
 *      q   (v) = rho (v) * v                                               (3)
 *
 *  Note that (1) is NOT simply s0 + v tau.  Dropping the free-acceleration term
 *  gives Newell's triangular diagram (see `TriangularFlux`), which overstates
 *  capacity by roughly 15% at typical freeway parameters.  The exact form is
 *  used here because a mutation to tau, s0 or v0 in the microscopic model must
 *  move the macroscopic diagram too, or the cross-fidelity check goes blind to
 *  exactly the fault family it exists to catch.
 *
 *  Since rho (v) is strictly decreasing on [0, v0), the relation is inverted by
 *  bisection onto a uniform rho grid once at construction, then interpolated.
 *
 *  @param v0     the free-flow (desired) speed in m/s
 *  @param tau    the desired time headway in sec
 *  @param s0     the minimum bumper-to-bumper gap in m
 *  @param len    the vehicle length in m
 *  @param delta  the free acceleration exponent (4 is standard)
 *  @param nTab   the number of tabulated densities
 */
class IdmFlux (val v0: Double = 30.0, val tau: Double = 1.5, val s0: Double = 2.0,
               val len: Double = 5.0, val delta: Double = 4.0, nTab: Int = 2001)
      extends FundamentalDiagram:

    private val flaw  = flawf ("IdmFlux")                       // flaw function
    private val debug = debugf ("IdmFlux", false)               // debug function

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

    // IDM gives rho(v), so construct its inverse v(rho) on a density grid.
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
            vT(k) = 0.5 * (lo + hi)                             // numerical inverse: v(r)
            qT(k) = r * vT(k)                                   // traffic identity: q = rho * v
    }

    private val kc = qT.argmax ()                               // index of capacity
    val rho_c = rhoT(kc)
    val q_max = qT(kc)
    val v_c   = vT(kc)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the flow q at density rho by linear interpolation in the table.
     */
    def q (rho: Double): Double =
        val r = min (max0 (rho), rho_m)
        val x = r / drho
        val k = min (x.toInt, nTab - 2)
        val f = x - k
        qT(k) * (1.0 - f) + qT(k + 1) * f
    end q


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the equilibrium speed at density rho by interpolating v(rho). */
    def speed (rho: Double): Double =
        val r = min (max0 (rho), rho_m)
        val x = r / drho
        val k = min (x.toInt, nTab - 2)
        val f = x - k
        vT(k) * (1.0 - f) + vT(k + 1) * f
    end speed
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the characteristic speed dq/drho by central difference.
     */
    def dq (rho: Double): Double =
        val h = drho
        (q (min (rho + h, rho_m)) - q (max0 (rho - h))) / (2.0 * h)
    end dq

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The maximum characteristic speed over the whole diagram, for the CFL test.
     */
    val maxWaveSpeed: Double =
        var m = 0.0
        cfor (1, nTab - 1) { k => m = max (m, abs ((qT(k+1) - qT(k-1)) / (2.0 * drho))) }
        m

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return true if the tabulated diagram is concave.  Concavity is what makes
     *  the supply-demand flux the EXACT Riemann solver; a non-concave diagram
     *  admits composite waves the flux would not resolve.  Worth asserting
     *  rather than assuming after any parameter change.
     */
    def isConcave: Boolean =
        var ok = true
        cfor (1, nTab - 1) { k => if qT(k+1) - 2.0 * qT(k) + qT(k-1) > 1E-12 then ok = false }
        ok
    end isConcave

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Read-only views of the table, for plotting and for comparing one diagram
    // against another on a common grid.  Copies, so a caller cannot corrupt the
    // table the interpolators read.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def rhoGrid: VectorD = rhoT.copy                            // tabulated densities (veh/m)
    def qGrid: VectorD   = qT.copy                              // tabulated flows (veh/s)
    def vGrid: VectorD   = vT.copy                              // tabulated speeds (m/s)

    // report the table once it is built, and complain if it is not usable
    debug ("init", s"nTab = $nTab, drho = $drho, kc = $kc, rho_c = $rho_c, q_max = $q_max, v_c = $v_c")
    if ! isConcave then
        flaw ("init", "tabulated diagram is not concave - the supply-demand flux is no longer the exact Riemann solver")

    override def toString: String =
        s"IdmFlux (v0 = $v0, tau = $tau, s0 = $s0, len = $len, delta = $delta)\n" +
        s"  rho_m = ${"%.6f".format (rho_m)} veh/m = ${"%.1f".format (rho_m * 1000)} veh/km\n" +
        s"  rho_c = ${"%.6f".format (rho_c)} veh/m = ${"%.1f".format (rho_c * 1000)} veh/km" +
        s"  (v_c = ${"%.2f".format (v_c)} m/s)\n" +
        s"  q_max = ${"%.6f".format (q_max)} veh/s = ${"%.0f".format (q_max * 3600)} veh/hr\n" +
        s"  max |dq/drho| = ${"%.3f".format (maxWaveSpeed)} m/s,  concave = $isConcave"

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
/** Check the IDM equilibrium table independently of the LWR PDE solver.
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




















//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TriangularFlux` class is the closed-form limit of `IdmFlux` obtained by
 *  dropping the free-acceleration term, i.e. taking s_e (v) = s0 + v tau.  This
 *  is Newell's triangular diagram.  Retained for comparison: it is cheaper and
 *  analytic, but overstates capacity, so it is not used for the cross-fidelity
 *  tier.
 */
class TriangularFlux (val v0: Double = 30.0, val tau: Double = 1.5,
                      val s0: Double = 2.0, val len: Double = 5.0)
      extends FundamentalDiagram:

    private val sp = s0 + len
    val rho_m = 1.0 / sp
    val rho_c = 1.0 / (v0 * tau + sp)
    val q_max = rho_c * v0
    val w     = sp / tau                                        // backward wave speed magnitude

    def q (rho: Double): Double =
        val r = min (max0 (rho), rho_m)
        if r <= rho_c then r * v0 else (1.0 - r * sp) / tau
    end q

    def dq (rho: Double): Double = if rho <= rho_c then v0 else -w
    val maxWaveSpeed: Double     = max (v0, w)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return this diagram sampled on someone else's density grid, so the two
     *  can be plotted or differenced on common support.
     *  @param rg  the density grid to sample on
     */
    def qOn (rg: VectorD): VectorD = VectorD (cfor (rg.dim) { k => q (rg(k)) })

    override def toString: String =
        s"TriangularFlux: rho_c = ${"%.1f".format (rho_c * 1000)} veh/km, " +
        s"q_max = ${"%.0f".format (q_max * 3600)} veh/hr, w = ${"%.3f".format (w)} m/s"

end TriangularFlux


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LwrPDE` class solves the Lighthill-Whitham-Richards traffic flow
 *  equation, a first order quasi-linear conservation law
 *
 *      rho_t + q (rho)_x = 0
 *
 *  This cannot reuse the linear advection solver in `FirstOrderPDE`, for two
 *  reasons.  First, the wave speed dq/drho depends on the solution, so it
 *  cannot be supplied as a fixed field v(x, t).  Second, the update must be in
 *  CONSERVATIVE flux-difference form
 *
 *      rho_i^(n+1) = rho_i^n - (dt/dx) (F_(i+1/2) - F_(i-1/2))
 *
 *  or discontinuities travel at the wrong speed, since a non-conservative
 *  discretization does not satisfy Rankine-Hugoniot.
 *
 *  Two fluxes are provided so the difference can be measured rather than
 *  asserted:
 *      "godunov" - exact Riemann solver via the supply-demand (Daganzo,
 *                  Lebacque) formulation; identical to the Cell Transmission
 *                  Model flux; monotone, positivity preserving, first order
 *      "lw"      - Richtmyer two-step Lax-Wendroff; conservative and second
 *                  order, but not TVD, so it fails at shocks
 *
 *  Stability requires  dt <= dx / max |dq/drho|.
 *
 *  @param fd      the fundamental diagram supplying q and dq/drho
 *  @param dt      delta 't' in sec
 *  @param dx      delta 'x' in m
 *  @param xm      the corridor length in m
 *  @param ic      the initial density profile rho (x, 0)
 *  @param inflow  the upstream boundary density rho (0, t)
 */
class LwrPDE (fd: FundamentalDiagram, dt: Double, dx: Double, xm: Double,
              ic: FunctionS2S, inflow: FunctionS2S):

    private val flaw  = flawf ("LwrPDE")                        // flaw function
    private val debug = debugf ("LwrPDE", false)                // debug function

    private val nx   = (ceil (xm / dx)).toInt + 1
    private val r    = dt / dx
    private var u    = new VectorD (nx)                         // old density vector
    private var uu   = new VectorD (nx)                         // new density vector
    private val flux = new VectorD (nx - 1)                     // interface fluxes
    private var rhoXT: MatrixD = null                           // space-time field, when recorded

    debug ("init", s"nx = $nx, dx = $dx, dt = $dt, r = $r, cfl = $cfl, cflOK = $cflOK")

    def cfl: Double    = dt * fd.maxWaveSpeed / dx
    def cflOK: Boolean = cfl <= 1.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The grid of cell positions, for plotting a profile against x.
     */
    val xGrid: VectorD = VectorD (cfor (nx) { i => i * dx })

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The recorded space-time density field rho (t, x), one row per time step,
     *  or null when the last `solveLwr` was called without `record`.  Row j is
     *  the profile at t = j*dt, so a column is the density history at one point
     *  and the whole matrix is what a heat map or a shock-trajectory check reads.
     */
    def field: MatrixD = rhoXT

    inline private def fluxGodunov (rhoL: Double, rhoR: Double): Double =
        min (fd.demand (rhoL), fd.supply (rhoR))

    inline private def fluxLW (rhoL: Double, rhoR: Double): Double =
        fd.q (0.5 * (rhoL + rhoR) - 0.5 * r * (fd.q (rhoR) - fd.q (rhoL)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the LWR equation to time 'te' and return the density profile.
     *  @param te      the time the solution is desired
     *  @param scheme  the numerical flux, "godunov" (default) or "lw"
     *  @param trace   whether to print the profile at each step
     *  @param record  whether to keep the whole space-time field, see `field`
     */
    def solveLwr (te: Double, scheme: String = "godunov", trace: Boolean = false,
                  record: Boolean = false): VectorD =

        if ! cflOK then flaw ("solveLwr", s"CFL = $cfl > 1, the solution will be unstable")

        val nt = (ceil (te / dt)).toInt + 1
        cfor (0, nx) { i => u(i) = ic (i * dx) }
        u(0) = inflow (0.0)

        rhoXT = if record then new MatrixD (nt, nx) else null
        if record then rhoXT(0) = u                             // row assignment copies, no aliasing

        cfor (1, nt) { j =>
            val t = j * dt

            cfor (0, nx - 1) { i =>                             // flux at interface i+1/2
                flux(i) = if scheme == "lw" then fluxLW (u(i), u(i+1))
                          else fluxGodunov (u(i), u(i+1))
            }

            cfor (1, nx - 1) { i =>                             // conservative update
                uu(i) = u(i) - r * (flux(i) - flux(i-1))
            }

            uu(0)      = inflow (t)                             // upstream: imposed demand
            uu(nx - 1) = uu(nx - 2)                             // downstream: zero-gradient outflow

            if trace then println (s"at t = $t: \tuu = $uu")
            val ut = uu; uu = u; u = ut                         // ping-pong: u now holds the new profile
            if record then rhoXT(j) = u
        }
        u
    end solveLwr

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vehicle count on the corridor, sum (rho) * dx.  Cell 0 is set
     *  by the boundary condition rather than by a flux difference, so this is
     *  conserved only up to the net boundary flux - account for that before
     *  using it as a V1 invariant.
     */
    def vehicleCount (rho: VectorD): Double = rho.sum * dx

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the front position, the last grid point below the midpoint of the
     *  two Riemann states.
     */
    def frontPosition (rho: VectorD, rhoL: Double, rhoR: Double): Double =
        val mid = 0.5 * (rhoL + rhoR)
        var k = 0
        cfor (0, nx) { i => if rho(i) < mid then k = i }
        k * dx
    end frontPosition

end LwrPDE


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lwrPDETest` main function tests `LwrPDE` on a Riemann problem with a
 *  known analytical solution: free-flowing traffic upstream running into a
 *  standing queue downstream.  The shock propagates upstream at the
 *  Rankine-Hugoniot speed s = [q] / [rho].
 *
 *  Expected (v0 = 30, tau = 1.5, s0 = 2, len = 5, delta = 4;
 *            dx = 10 m, dt = 0.25 s, CFL = 0.75, rhoL = 0.010, rhoR = 0.100):
 *      analytical shock speed  = -0.9127 m/s
 *      godunov                 = -0.9333 m/s, 2.3% error, rho in [0.010, 0.100]
 *      lax-wendroff            diverges, rho reaching +/- 14 veh/m
 *
 *  Lax-Wendroff does not merely oscillate here - it destroys the solution while
 *  compiling, running to completion, and returning a well-formed VectorD.
 *
 *  > runMain scalation.dynamics.lwrPDETest
 */
@main def lwrPDETest (): Unit =

    val fd  = new IdmFlux ()
    val tri = new TriangularFlux ()
    banner ("Fundamental Diagram from exact IDM equilibrium")
    println (fd)
    println (tri)

    val rg = fd.rhoGrid
    new Plot (rg, fd.qGrid, tri.qOn (rg), "q vs. rho (black-IDM exact, red-triangular)", true)
    new Plot (rg, fd.vGrid, null, "equilibrium speed vs. rho (IDM exact)", true)

    val dx = 10.0; val dt = 0.25; val xm = 2000.0
    val te = 300.0; val x0 = 1000.0
    val rhoL = 0.010; val rhoR = 0.100

    def ic (x: Double): Double     = if x < x0 then rhoL else rhoR
    def inflow (t: Double): Double = rhoL

    val sA = fd.shockSpeed (rhoL, rhoR)
    banner ("Riemann problem")
    println (s"q(rhoL) = ${"%.5f".format (fd.q (rhoL))},  q(rhoR) = ${"%.5f".format (fd.q (rhoR))}")
    println (s"analytical shock speed = ${"%.4f".format (sA)} m/s")
    println (s"predicted front at t = $te: ${"%.1f".format (x0 + sA * te)} m")

    val schemes = Array ("godunov", "lw")
    val profile = Array.ofDim [VectorD] (schemes.length)
    var xg: VectorD = null

    cfor (0, schemes.length) { k =>
        val scheme = schemes(k)
        banner (s"LWR - $scheme")
        val pde = new LwrPDE (fd, dt, dx, xm, ic, inflow)
        println (s"CFL = ${"%.3f".format (pde.cfl)}, stable = ${pde.cflOK}")
        val rho = pde.solveLwr (te, scheme)
        val xf  = pde.frontPosition (rho, rhoL, rhoR)
        val sN  = (xf - x0) / te
        println (s"front = ${"%.1f".format (xf)} m,  speed = ${"%.4f".format (sN)} m/s,  " +
                 s"error = ${"%.2f".format (100 * abs (sN - sA) / abs (sA))}%")
        println (s"rho in [${"%.6f".format (rho.min)}, ${"%.6f".format (rho.max)}]   " +
                 s"(jam = ${"%.6f".format (fd.rho_m)})")
        profile(k) = rho
        xg         = pde.xGrid
    }

    new Plot (xg, profile(0), null, s"godunov: rho vs. x at t = ${"%.0f".format (te)} s", true)
    new Plot (xg, profile(0), profile(1), "rho vs. x (black-godunov, red-lax-wendroff)", true)

end lwrPDETest



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lwrPDETest2` main function sweeps dt across the CFL boundary.  Every run
 *  compiles, executes and returns a density profile; only some are physically
 *  meaningful.  The violation is detectable statically from dt, dx and the
 *  maximum wave speed, without running anything - which is why it belongs in
 *  the static tier of the validation battery.
 *
 *  > runMain scalation.dynamics.lwrPDETest2
 */
@main def lwrPDETest2 (): Unit =

    val fd = new IdmFlux ()
    val dx = 10.0; val xm = 2000.0; val te = 60.0; val x0 = 1000.0
    val rhoL = 0.010; val rhoR = 0.100

    def ic (x: Double): Double     = if x < x0 then rhoL else rhoR
    def inflow (t: Double): Double = rhoL

    banner ("CFL sweep")
    println (s"max wave speed = ${"%.3f".format (fd.maxWaveSpeed)} m/s")
    println (s"CFL requires dt <= dx / maxWaveSpeed = ${"%.4f".format (dx / fd.maxWaveSpeed)} s\n")

    for dt <- Array (0.10, 0.25, 0.33, 0.50, 1.00) do
        val pde = new LwrPDE (fd, dt, dx, xm, ic, inflow)
        val rho = pde.solveLwr (te, "godunov")
        println (s"dt = ${"%4.2f".format (dt)}  CFL = ${"%5.3f".format (pde.cfl)}  " +
                 s"ok = ${"%-5s".format (pde.cflOK)}  " +
                 s"rho in [${"%10.4f".format (rho.min)}, ${"%10.4f".format (rho.max)}]  " +
                 s"veh = ${"%8.2f".format (pde.vehicleCount (rho))}")
    end for

end lwrPDETest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lwrPDETest3` main function records the whole space-time density field
 *  and tracks the shock through it, so the Rankine-Hugoniot speed is checked at
 *  every step rather than only at the final time.  A single end-state agreement
 *  can hide a front that arrived late and then over-ran; the trajectory cannot.
 *
 *  > runMain scalation.dynamics.lwrPDETest3
 */
@main def lwrPDETest3 (): Unit =

    val fd = new IdmFlux ()
    val dx = 10.0; val dt = 0.25; val xm = 2000.0
    val te = 300.0; val x0 = 1000.0
    val rhoL = 0.010; val rhoR = 0.100

    def ic (x: Double): Double     = if x < x0 then rhoL else rhoR
    def inflow (t: Double): Double = rhoL

    val sA  = fd.shockSpeed (rhoL, rhoR)
    val pde = new LwrPDE (fd, dt, dx, xm, ic, inflow)
    pde.solveLwr (te, "godunov", record = true)

    val rhoXT = pde.field                                       // nt x nx space-time field
    val nt    = rhoXT.dim
    val tt    = VectorD (cfor (nt) { j => j * dt })             // time grid
    val xf    = VectorD (cfor (nt) { j => pde.frontPosition (rhoXT(j), rhoL, rhoR) })
    val xa    = VectorD (cfor (nt) { j => x0 + sA * (j * dt) }) // analytical trajectory

    banner ("Shock trajectory vs. Rankine-Hugoniot")
    println (s"analytical shock speed = ${"%.4f".format (sA)} m/s")
    println (s"max |x_numeric - x_analytic| = ${"%.2f".format ((xf - xa).abs.max)} m " +
             s"over ${"%.0f".format (te)} s")
    println (s"vehicles at t = 0: ${"%.2f".format (pde.vehicleCount (rhoXT(0)))}, " +
             s"at t = ${"%.0f".format (te)}: ${"%.2f".format (pde.vehicleCount (rhoXT(nt-1)))}")

    new Plot (tt, xf, xa, "shock position vs. t (black-godunov, red-Rankine-Hugoniot)", true)

end lwrPDETest3
