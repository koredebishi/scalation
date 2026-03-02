
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishi
 *  @version 2.0
 *  @date    Sunday Feb 28 14:32:52 EDT 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Gaussian Smooth Random Search (GSRS) Algorithm for Optimization
 *          
 */

package scalation
package optimization

import scala.math.pow
import scalation.mathstat.{FunctionV2S, VectorD}
import scalation.random.Normal


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GSRS` class implements the Simultaneous Perturbation Stochastic Approximation
 *  algorithm for rough approximation of gradients.
 *
 *  @see Found Comput Math (2017) 17:527–566
 *       DOI 10.1007/s10208-015-9296-2
 *
 *       The paper presented Random Gradient-Free Minimization of Convex
 *       Functions
 *       The below implementation is adapted from Section 4 of the paper, using the
 *       two-point central difference variant of the Gaussian smoothing estimator
 *       (lower variance than the one-point estimator in Section 4).
 *
 *       Initialize x0 ∈ Q
 *
 *       For k = 0,1,2,...
 *          1. Generate uk ~ N(0, I)
 *          2. Evaluate f(xk + μ uk) and f(xk - μ uk)
 *
 *              Compute two-point gradient estimator:
 *              gμ(xk) = ((f(xk + μ uk) - f(xk - μ uk)) / (2μ)) * uk
 *
 *          3. Update:
 *              x_{k+1} = x_k − h_k * gμ(x_k)
 *
 *       End For
 *
 *      minimize f(x)
 *  @param f         the vector to scalar function whose approximate gradient is sought
 *  @param max_iter  the maximum number of iterations
 *  @param checkCon  whether to check bounds contraints
 *  @param lower     the lower bounds vector
 *  @param upper     the upper bounds vector
 *  @param debug_    the whether to call in debug mode (does tracing)j
 */
class GSRS (f: FunctionV2S, max_iter: Int = 100, checkCon: Boolean = false,
            lower: VectorD = null, upper: VectorD = null, debug_ : Boolean = false)
    extends Minimizer
        with BoundsConstraint (lower, upper)
        with MonitorEpochs:

    private val debug = debugf ("GSRS", debug_)                        // debug function
    private val flaw  = flawf ("GSRS")                                 // flaw function

    private val EPS   = 1E-6
    private val norm  = Normal (0.0, 1.0)              // N(0,1) generator for u_k ~ N(0, I)
    private var alpha = 0.602                          // decay exponent for step size h_k  — borrowed from Spall (1998) DOI: 10.1109/7.705889, no standard for GSRS
    private var gamma = 0.101                          // decay exponent for smoothing μ    — borrowed from Spall (1998), no standard for GSRS
    private var A     = 0.1 * max_iter                 // stability constant for h_k schedule (~10% of max_iter per Spall (1998))
    private var a     = 0.16                           // step size scale for h_k            — borrowed from Spall (1998), tune for problem
    private var c     = 1.0                            // initial smoothing scale μ₀          — tune for problem

    private var f_best = Double.MaxValue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the parameters.
     *  @param params  the given starting parameters of a VectorD
     */
    def reset (params: VectorD = VectorD (0.602, 0.101, 0.1 * max_iter, 0.16, 1.0)): Unit =
        if params.length != 5 then flaw ("reset", "failed! did not pass 5 parameters")
        alpha  = params(0)
        gamma  = params(1)
        A      = params(2)
        a      = params(3)
        c      = params(4)
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
        val x      = x0.copy                                           // new point

        var (k, go) = (1, true)

        val elapsedTimeMs = gauge {
            cfor (k <= max_iter && go, k += 1) {
                val hk      = a / pow (A + k + 1, alpha)               // step size h_k (Nesterov Section 4)
                val mu      = c / pow (k + 1, gamma)                   // smoothing parameter μ
                val u       = VectorD (for i <- 0 until x0.dim yield norm.gen)   // u_k ~ N(0, I)
                val x_fwd   = x + u * mu                               // x + μ * u (forward point)
                val x_bwd   = x - u * mu                               // x - μ * u (backward point)
                val y_fwd   = f(x_fwd)                                 // f(x + μ * u)
//              val y_0     = f(x)                                     // f(x) — Nesterov one-point estimator
                val y_bwd   = f(x_bwd)                                 // f(x - μ * u)

//              val g_mu = u * (y_fwd - y_0) / mu                     // one-point estimator — Nesterov Section 4 (higher variance)
                val g_mu = u * (y_fwd - y_bwd) / (2 * mu)             // two-point central difference variant (lower variance)
                x_old    = x.copy                                      // save previous location x
                x       -= g_mu * hk                                   // update: x_{k+1} = x_k - h_k * gμ(x_k)

                if checkCon then constrain (x)                         // enforce contraints, may move x

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
        println("GSRS: OPTIMIZATION SUMMARY")
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

end GSRS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GSRSTest` main function tests the `GSRS` class.
 *  > runMain scalation.optimization.GSRSTest
 */
@main def GSRSTest (): Unit =

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

        val noise = Normal (0.0, 0.03)                                      // standard Gaussian noise — no tuning to problem

    def f (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1 + noise.gen     // the function you seak to optimize

    val x0 = VectorD (1, 2)        // initial starting value for the optimizer to look at 

    println ("\n=== Example 2: Advanced usage with custom callbacks ===")
    val optimizer2 = new GSRS (f)
    optimizer2.reset ()
    optimizer2.setVerbose (1)          // Disable built-in output
    optimizer2.setPrintEvery (20)      // Print every 10 epochs
    val opt = optimizer2.solve (x0)



//    println (s"][ optimal solution (f(x), x) = $opt")

//optimizer.plotLoss ()

end GSRSTest

