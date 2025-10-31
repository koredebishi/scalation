//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Yulong Wang, Korede Bishi
 *  @version 2.0
 *  @date    Thursday October 17 13:32:52 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Simultaneous Perturbation Stochastic Approximation with Momentum
 */

package scalation
package optimization

import scala.math.pow
import Minimize.hp

import scalation.mathstat.{FunctionV2S, VectorD}
import scalation.random.{Bernoulli, Uniform}
//import scalation.random.{Bernoulli, Normal, Uniform}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SPSA_Mo` class implements the Simultaneous Perturbation Stochastic Approximation wihh Momentum
 *  algorithm for rough approximation of gradients.
 *  @see https://www.jhuapl.edu/SPSA_Mo/PDF-SPSA_Mo/Matlab-SPSA_Mo_Alg.pdf
 *  @see
 *
 *      minimize f(x)
 *
 *  @param f         the vector to scalar function whose approximate gradient is sought
 *  @param max_iter  the maximum number of iterations
 *  @param checkCon  whether to check bounds contraints
 *  @param lower     the lower bounds vector
 *  @param upper     the upper bounds vector
 *  @param debug_    the whether to call in debug mode (does tracing)j
 *  @param hparam    the momentum hyper-parameters
 */
class SPSA_Mo (f: FunctionV2S, max_iter: Int = 100,hparam: HyperParameter = hp, checkCon: Boolean = false,
            lower: VectorD = null, upper: VectorD = null, debug_ : Boolean = false)
    extends Minimizer
        with BoundsConstraint (lower, upper)
        with StoppingRule (hparam("upLimit").toInt)
        with MonitorEpochs:

    private val debug = debugf ("SPSA_Mo", debug_)                        // debug function
    private val flaw  = flawf ("SPSA_Mo")                                 // flaw function

    private val EPS   = 1E-6
    private val coin  = Bernoulli ()                                   // Bernoulli (0/1) RVG
    private var alpha = 0.602
    private var gamma = 0.101
    private var A     = 100.0
    private var a     = 0.16       // these numbers are from Spall (1998) DOI: 10.1109/7.705889
    private var c     = 1.0


    // Added by Korede for momentum
    private val β     = hparam("beta").toDouble                         // momentum hyper-parameter
    private val v     = hparam("nu").toDouble                           // 0 => SGD, 1 => (normalized) SHB


    private var f_best = Double.MaxValue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the parameters.
     *  @param params  the given starting parameters of a VectorD
     */
    def reset (params: VectorD = VectorD (0.602, 0.101, 10.0, 0.16, 1.0)): Unit =
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
    /** Return a random vector of {-1, 1} values.
     *  @param n       the size of the vector
     *  @param p       the probability of 1
     *  @param stream  the random number stream
     */
    def bernoulliVec (n: Int, p: Double = 0.5, stream: Int = 0): VectorD =
        VectorD (for i <- 0 until n yield 2.0 * coin.gen - 1.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for an optimal point by moving a distance ak in the -ghat direction.
     *  Uses hybrid monitoring system with automatic progress tracking.
     *  @param x0     initial point
     *  @param step   steps for iteration
     *  @param toler  tolerance
     */
    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPS): FuncVec =

        var x_old  = x0.copy                                           // old point
        var x_best = x0.copy                                           // best point so far
        val x      = x0.copy                                           // new point

        var (k, go) = (1, true)

        var p = new VectorD (x0.dim)                                 // momentum-based aggregated gradient

        // Start timing
        val startTime = System.nanoTime()

        // Initialize monitoring )
        initializeMonitoring (max_iter)                       // Called at start of solve(), from the MonitorEpochs trait

        cfor (k <= max_iter && go, k += 1) {
            val ak      = a / pow (A + k + 1, alpha)                   // how far to move along gradient
            val ck      = c / pow (k + 1, gamma)                       // for x distance
            val delta   = bernoulliVec (x0.dim)                        // random direction
            val x_plus  = x + delta * ck                               // x moved + delta direction
            val x_minus = x - delta * ck                               // x moved - delta direction
            val y_plus  = f(x_plus)                                    // functional value for x_plus
            val y_minus = f(x_minus)                                   // functional value for x_minus

            //Momentum update by Korede 10/16/2025
            val ghat = delta * (y_plus - y_minus) / (2 * ck)            // rough/approx. gradient
            p = ghat * (1 - β) + p * β                                  // accumulate momentum
            x_old = x.copy                                              // save previous location x
            x -= (ghat * (1 - v) + p * v) * ak                          // update x with momentum

            if checkCon then constrain (x)                             // enforce contraints, may move x

            val f_x = f(x)                                             // new functional value
            debug ("solve", s"iteration k = $k, x = $x, f(x) = $f_x vs. f_best = $f_best")

            if f_x < f_best then
                x_best = x.copy                                        // copy by value
                f_best = f_x
            end if

            // Update monitoring (replaces manual printEpoch/printProgressBar calls)
            updateMonitoring (k, f_best)                           // Called once per epoch, from the MonitorEpochs trait

            if (x - x_old).norm < toler then go = false                // stopping rule
        } // cfor

        // Calculate elapsed time
        val endTime = System.nanoTime()
        val elapsedTimeMs = (endTime - startTime) / 1e6  // Convert nanoseconds to milliseconds
        val elapsedTimeSec = elapsedTimeMs / 1000.0      // Convert to seconds

        // Finalize monitoring (replaces manual printFooter call)
        finalizeMonitoring ()

        // Print clean, formatted optimization summary
        println ()
        println (sline (70).trim)
        println ("SPSA_MO: OPTIMIZATION SUMMARY")
        println (sline (70).trim)
        println (f"${"Metric"}%-25s | ${"Value"}%s")
        println (sline (70).trim)
        println (f"${"Final position (x_last)"}%-25s | $x")
        println (f"${"Loss at final position"}%-25s | ${f(x)}%.8f")
        println (f"${"Best position found"}%-25s | $x_best")
        println (f"${"Best loss achieved"}%-25s | $f_best%.8f")
        println (f"${"Total iterations"}%-25s | ${epochLoss.size}")
        println (f"${"Elapsed time"}%-25s | ${elapsedTimeSec}%.4f seconds (${elapsedTimeMs}%.2f ms)")
        println (f"${"Time per iteration"}%-25s | ${elapsedTimeMs / epochLoss.size}%.2f ms")
        println (sline (70).trim)

        (f_best, x_best)
    end solve

end SPSA_Mo


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SPSA_MoTest` main function tests the `SPSA_Mo` class with hybrid monitoring.
 *  Demonstrates both simple usage (verbose flag) and advanced usage (custom callbacks).
 *  > runMain scalation.optimization.SPSA_MoTest
 */
@main def SPSA_MoTest (): Unit =

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

    val noise = Uniform (-0.1, 0.1)    // noise term for stochastic objective

    def f (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1 + noise.gen

    val x0 = VectorD (1, 2)            // initial starting point

    println ("\n=== Example 2: Advanced usage with custom callbacks ===")
    val optimizer1 = new SPSA_Mo (f)
    optimizer1.reset ()
    optimizer1.setVerbose (1)          // Disable built-in output
    optimizer1.setPrintEvery (20)      // Print every 10 epochs
    val opt1 = optimizer1.solve (x0)


    // Plot the loss convergence
    //optimizer1.plotLoss ()

end SPSA_MoTest
