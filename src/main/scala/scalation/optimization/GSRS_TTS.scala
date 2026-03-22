
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishi
 *  @version 2.0
 *  @date    Sunday Feb 28 14:32:52 EDT 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Gaussian Smooth Random Search (GSRS_TTS) Algorithm for Optimization
 *
 */

package scalation
package optimization

import scala.math.pow
import scalation.mathstat.{FunctionV2S, VectorD}
import scalation.random.Normal


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GSRS_TTS` class implements a Two-Time-Scale Gaussian Smooth Random Search
 *  algorithm using a two-point central difference gradient estimator with
 *  separate fast and slow step-size schedules.
 *
 *  Algorithm (Clarke two-time-scale stochastic approximation):
 *
 *      for n = 0, 1, 2, ..., N-1 do
 *
 *          1. Sample Gaussian vector:
 *                 U_n ~ N(0, I_d)
 *
 *          2. Obtain noisy function values:
 *                 F_plus  = F(x_n + λ U_n, ζ_n^1)
 *                 F_minus = F(x_n - λ U_n, ζ_n^2)
 *
 *          3. Compute subgradient approximation (Equation 11):
 *                 g_tilde = ((F_plus - F_minus) / (2λ)) * U_n
 *
 *          4. Fast time-scale update (Equation 12):
 *                 y_{n+1} = y_n + β(n) * (g_tilde - y_n)
 *
 *          5. Slow projected update (Equation 12):
 *                 x_{n+1} = P_X( x_n - α(n) * y_n )
 *
 *      end for
 *
 *      minimize f(x)
 *
 *  @param f         the vector to scalar function whose approximate gradient is sought
 *  @param max_iter  the maximum number of iterations
 *  @param checkCon  whether to check bounds constraints
 *  @param lower     the lower bounds vector
 *  @param upper     the upper bounds vector
 *  @param debug_    whether to call in debug mode (does tracing)
 */
class GSRS_TTS (f: FunctionV2S, max_iter: Int = 100, checkCon: Boolean = false,
            lower: VectorD = null, upper: VectorD = null, debug_ : Boolean = false)
    extends Minimizer
        with BoundsConstraint (lower, upper)
        with MonitorEpochs:

    private val debug = debugf ("GSRS_TTS", debug_)                        // debug function
    private val flaw  = flawf ("GSRS_TTS")                                 // flaw function

    private val EPS   = 1E-6
    private val norm  = Normal (0.0, 1.0)              // N(0,1) generator for u_k ~ N(0, I)
    private var alpha = 0.602                          // decay exponent for step size h_k  — borrowed from Spall (1998) DOI: 10.1109/7.705889, no standard for GSRS_TTS
    private var gamma = 0.101                          // decay exponent for smoothing μ    — borrowed from Spall (1998), no standard for GSRS_TTS
    private var A      = 0.1 * max_iter                // stability constant (~10% of max_iter per Spall (1998))
    private var c      = 1.0                           // initial smoothing scale λ₀         — tune for problem
    private var a_slow = 0.16                          // slow time-scale step size scale α(n)
    private var a_fast = 0.5                           // fast time-scale step size scale β(n) — must satisfy β(n)/α(n) → ∞

    private var f_best = Double.MaxValue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the parameters.
     *  @param params  the given starting parameters of a VectorD
     */
    def reset (params: VectorD = VectorD (0.602, 0.101, 0.1 * max_iter, 0.16, 0.5, 1.0)): Unit =
        if params.length != 6 then flaw ("reset", "failed! did not pass 6 parameters")
        alpha  = params(0)
        gamma  = params(1)
        A      = params(2)
        a_slow = params(3)
        a_fast = params(4)
        c      = params(5)
    end reset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This method is not supported.
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
        throw new UnsupportedOperationException ("lineSearch: not provided by this optimizer")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for an optimal point by moving a distance ak in the -ghat direction.
     *  @param x0     initial point
     *  @param step   steps for iteration
     *  @param toler  tolerance
     */
    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPS): FuncVec =

        var x_old  = x0.copy                                           // old point
        var x_best = x0.copy                                           // best point so far
        val x      = x0.copy                                           // current iterate x_n
        var y      = new VectorD (x0.dim)                              // fast time-scale gradient tracker y_n, initialized to zero

        var (k, go) = (1, true)

        val elapsedTimeMs = gauge {
            cfor (k <= max_iter && go, k += 1) {
                val alpha_n = a_slow / pow (A + k + 1, alpha)          // slow step size α(n) — decays as k^{-0.602}
                val beta_n  = a_fast / pow (k + 1, gamma)              // fast step size β(n) — decays as k^{-0.101}, β(n)/α(n) → ∞
//              val lambda  = c / pow (k + 1, alpha)                   // decaying smoothing — tightens over time (alternative)
                val lambda  = c                                        // fixed smoothing parameter λ = c — stable exploration width

                val u       = VectorD (for i <- 0 until x0.dim yield norm.gen)   // U_n ~ N(0, I)
                val x_fwd   = x + u * lambda                           // x_n + λ U_n
                val x_bwd   = x - u * lambda                           // x_n - λ U_n
                val F_plus  = f(x_fwd)                                 // F(x_n + λ U_n, ζ_n^1)
                val F_minus = f(x_bwd)                                 // F(x_n - λ U_n, ζ_n^2)

                val g_tilde = u * (F_plus - F_minus) / (2 * lambda)    // subgradient approximation (Eq. 11)

                y        = y + (g_tilde - y) * beta_n                  // fast time-scale update (Eq. 12)
                x_old    = x.copy                                      // save previous x_n
                x       -= y * alpha_n                                 // slow projected update (Eq. 12)

                if checkCon then constrain (x)                         // enforce bounds constraint P_X(...)

                val f_x = f(x)                                         // new functional value
                debug ("solve", s"iteration k = $k, x = $x, f(x) = $f_x vs. f_best = $f_best")

                if f_x < f_best then
                    x_best = x.copy                                    // copy by value
                    f_best = f_x
                end if

                updateMonitoring (k, f_best)                           // update epoch monitoring
                if (x - x_old).norm < toler then go = false            // stopping rule
            } // cfor
        } // gauge

        val elapsedTimeSec = elapsedTimeMs * MS_PER_SEC                // elapsed time in seconds

        finalizeMonitoring()
        println()
        println(sline(70).trim)
        println("GSRS_TTS: OPTIMIZATION SUMMARY")
        println(sline(70).trim)
        println(f"${"Metric"}%-25s | ${"Value"}%s")
        println(sline(70).trim)
        println(f"${"Final position (x_last)"}%-25s | $x")
        println(f"${"Loss at final position"}%-25s | ${f(x)}%.8f")
        println(f"${"Best position found"}%-25s | $x_best")
        println(f"${"Best loss achieved"}%-25s | $f_best%.8f")
        println(f"${"Total iterations"}%-25s | ${epochLoss.size}")
        println(f"${"Elapsed time"}%-25s | ${elapsedTimeSec}%.4f seconds (${elapsedTimeMs}%.2f ms)")
        println(f"${"Time per iteration"}%-25s | ${elapsedTimeMs / epochLoss.size}%.2f ms")
        println(sline(70).trim)

        (f_best, x_best)
    end solve

end GSRS_TTS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GSRS_TTSTest` main function tests the `GSRS_TTS` class.
 *  > runMain scalation.optimization.GSRS_TTSTest
 */
@main def GSRS_TTSTest (): Unit =

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

    val noise = Normal (0.0, 0.1)                                      // standard Gaussian noise — matched to GSRS for honest comparison

    def f (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1 + noise.gen     // the function you seak to optimize

    val x0 = VectorD (1, 2)        // initial starting value for the optimizer to look at 

    println ("\n=== Example 2: Advanced usage with custom callbacks ===")
    val optimizer2 = new GSRS_TTS (f, max_iter = 100)
    optimizer2.reset ()
    optimizer2.setVerbose (1)
    optimizer2.setPrintEvery (20)
    val opt = optimizer2.solve (x0)

end GSRS_TTSTest

