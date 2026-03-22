//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishi
 *  @version 2.0
 *  @date    October 17, 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Metamodel-based Derivative-Free Trust-Region Algorithm
 *
 *  @see     Osorio, C., & Bierlaire, M. (2013). "Simulation-Based Optimization
 *           for Urban Transportation Problems." Operations Research, 61(6), 1333–1345.
 */

package scalation
package optimization

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.math.{min, max, sqrt}

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MetaModelTR` class implements the metamodel-based DF-TR (Derivative-Free
 *  Trust-Region) algorithm from Osorio & Bierlaire (2013), Section 3 (metamodel)
 *  and Section 4.2 (Algorithm), designed for simulation-based optimization under
 *  tight computational budgets.
 *
 *  Metamodel Structure (Section 3):
 *  {{{
 *    m(x, y; q) = λ·T(x, y; q) + φ(x; θ)
 *  }}}
 *  where:
 *    - T(x,y;q) is an analytical queueing model
 *    - φ(x;θ) is a diagonal quadratic correction: θ₀ + Σⱼ θⱼ·xⱼ + Σⱼ θ_{d+j}·xⱼ²
 *    - λ scales the queueing model
 *    - θ = [θ₀, θ₁...θ_d, θ_{d+1}...θ_{2d}] are quadratic coefficients
 *
 *  Parameter Fitting:
 *  At each iteration k, fit [λ, θ] by weighted least squares on all accumulated
 *  samples. Weights are inversely proportional to distance from current iterate x_k
 *  (local regression as specified in the paper).
 *
 *  Trust-Region Algorithm (Section 4.2):
 *  {{{
 *    0) Initialize: x₀, Δ₀, n₀=1, u₀=0; evaluate T and f̂ at x₀; fit m₀
 *    1) Criticality: if σ_k ≤ σ_c → enter conservative mode
 *    2) Step calculation: solve argmin m_k(x) s.t. ||x - x_k|| ≤ Δ_k
 *    3) Acceptance: compute ρ_k = [f̂(x_k) - f̂(x_k+s_k)] / [m_k(x_k) - m_k(x_k+s_k)]
 *         if ρ_k ≥ η₁: accept step (x_{k+1} = x_k + s_k), reset u_k = 0
 *         else: reject step (x_{k+1} = x_k), increment u_k
 *       In all cases: add new simulation, update weights, refit m_{k+1}
 *    4) Model improvement: compute ξ_{k+1} = ||η_{k+1} - η_k|| / ||η_k||
 *         if ξ_{k+1} < ξ̄: sample new point (§4.3), evaluate, refit
 *    5) TR radius update (three cases):
 *         if ρ_k > η₂:                  Δ_{k+1} = min(γ_inc·Δ_k, Δ_max)
 *         else if ρ_k ≤ η₁ AND u_k ≥ ū: Δ_{k+1} = max(Δ_k, d̄); u_k = 0
 *         else:                         Δ_{k+1} = Δ_k
 *       If Δ_{k+1} ≤ d̄ → enter conservative mode
 *       Update (n_{k+1}, u_{k+1}, k ← k+1); Stop when n_k ≥ n_max
 *  }}}
 *
 *  Extension Hooks:
 *    - criticalityMeasure(x_k): User-supplied criticality σ_k (default: disabled)
 *    - enterConservativeMode(): User policy for conservative mode (default: no-op)
 *    - sampleForModelImprovement(x_k, Δ_k): §4.3 sampling (default: local TR sampler)
 *
 *  @param simF      the stochastic simulator f̂(x) to minimize
 *  @param queueT    analytical queueing model T(x, y; q)
 *  @param solveY    solves for queue state y(x, q)
 *  @param q         exogenous queueing parameters
 *  @param d         dimension of decision variable x
 *  @param maxIter   maximum number of iterations (default: 150)
 *  @param budget    maximum simulation budget n_max (default: 150)
 *  @param debug_    enable debug output (default: false)
 *  @param lower     lower bounds on x (default: null = unbounded)
 *  @param upper     upper bounds on x (default: null = unbounded)
 *
 *  Example usage:
 *  {{{
 *  val optimizer = new MetaModelTR(simF, queueT, solveY, q, d=2, budget=200)
 *  val (fBest, xBest) = optimizer.solve(x0)
 *  }}}
 */
class MetaModelTR (
                      simF:    VectorD => Double,                                 // simulator: f̂(x)
                      queueT:  (VectorD, VectorD, VectorD) => Double,             // analytic T(x,y;q)
                      solveY:  (VectorD, VectorD) => VectorD,                     // y(x,q)
                      q:       VectorD,                                           // exogenous queueing parameters
                      d:       Int,                                               // dimension of x
                      maxIter: Int = 150,
                      budget:  Int = 150,                                         // total sim calls (n_max)
                      debug_ : Boolean = false,
                      lower:   VectorD = null,
                      upper:   VectorD = null
                  ) extends Minimizer
    with BoundsConstraint (lower, upper)
    with MonitorEpochs:

    private val debug = debugf ("MetaModelTR", debug_)
    private val flaw  = flawf ("MetaModelTR")
    private val EPS   = 1e-10

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Algorithm constants from Osorio & Bierlaire (2013), Section 4.2 */
    private val eta1   = 1e-3        // acceptance threshold η₁ (0 < η₁ < 1)
    private val eta2   = 0.90        // high-agreement threshold η₂ (η₁ < η₂ < 1)
    private val gInc   = 2.0         // γ_inc: TR expansion factor
    private val dBar   = 1e-2        // d̄: lower bound for Δ
    private val uBar   = 10          // ū: successive rejections threshold
    private val Dmax   = 1e1         // Δ_max: maximum TR radius
    private val D0     = 1e0         // Δ₀: initial TR radius
    private val xiBar  = 1e-3        // ξ̄: model-improvement trigger
    private val sigC   = 1e-6        // σ_c: criticality threshold
    private val Ncand  = min(40, 10*d)  // candidate count inside TR for step calculation
    private val rng    = new Random(1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Ridge regularization weight for early fit stability (encourages λ≈1, θ≈0) */
    private val w0     = 1e-2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sample storage: each sample contains x, f̂(x), and y(x,q) */
    private case class Sample(x: VectorD, fhat: Double, y: VectorD)
    private var samples = ArrayBuffer[Sample]()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Metamodel parameters: η = [λ, θ], where θ = [θ₀, θ₁...θ_d, θ_{d+1}...θ_{2d}] */
    private var lam: Double            = 1.0                    // λ: scaling for T(x,y;q)
    private var theta: VectorD         = new VectorD(2*d + 1)  // θ: quadratic coefficients
    private var lamPrev: Double        = lam                    // previous λ for ξ calculation
    private var thetaPrev: VectorD     = theta.copy             // previous θ for ξ calculation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the diagonal quadratic correction term φ(x; θ).
     *  {{{
     *    φ(x; θ) = θ₀ + Σⱼ θⱼ·xⱼ + Σⱼ θ_{d+j}·xⱼ²
     *  }}}
     *  @param x  the decision variable vector
     *  @return   the quadratic correction value
     */
    private inline def phi (x: VectorD): Double =
        val b = theta(0)                        // intercept θ₀
        var sum = b
        var j = 0
        while j < d do
            sum += theta(1 + j) * x(j) + theta(d + 1 + j) * x(j) * x(j)
            j += 1
        end while
        sum
    end phi

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the metamodel prediction m(x) = λ·T(x, y; q) + φ(x; θ).
     *  @param x  the decision variable vector
     *  @return   the metamodel prediction
     */
    private inline def mPred (x: VectorD): Double =
        val y = solveY(x, q)
        lam * queueT(x, y, q) + phi(x)
    end mPred

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute inverse-distance weight for sample i around current iterate x_k.
     *  Local regression weights as specified in Osorio & Bierlaire (2013).
     *  {{{
     *    w_i = 1 / (1 + ||x_i - x_k||²)
     *  }}}
     *  @param xk  the current iterate
     *  @param xi  the i-th sample location
     *  @return    the weight for sample i
     */
    private inline def wDist (xk: VectorD, xi: VectorD): Double =
        1.0 / (1.0 + (xi - xk).normSq)
    end wDist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build design matrix row for [λ, θ] fitting.
     *  Row structure: [T(x,y;q), 1, x₁, ..., x_d, x₁², ..., x_d²]
     *  @param x   the sample location
     *  @param Ty  the pre-computed T(x, y; q) value
     *  @return    the design row vector
     */
    private def designRow (x: VectorD, Ty: Double): VectorD =
        val row = new VectorD(2*d + 2)
        row(0) = Ty        // λ coefficient
        row(1) = 1.0       // θ₀ (intercept)
        var j = 0
        while j < d do { row(2 + j) = x(j); j += 1 }            // linear terms
        j = 0
        while j < d do { row(2 + d + j) = x(j) * x(j); j += 1 } // quadratic terms
        row
    end designRow

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fit metamodel parameters [λ, θ] using weighted least squares.
     *  Uses all accumulated samples with weights based on distance from x_k.
     *  Includes ridge regularization to stabilize early fits.
     *  @param xk  the current iterate (center of local regression)
     */
    private def fitModel (xk: VectorD): Unit =
        lamPrev   = lam                     // save for ξ calculation
        thetaPrev = theta.copy

        val n = samples.size
        val P = 2*d + 2                     // parameters: [λ, θ₀, θ₁...θ_d, θ_{d+1}...θ_{2d}]
        val X = new MatrixD(n + P, P)       // design matrix (with ridge rows)
        val z = new VectorD(n + P)          // response vector
        val w = new VectorD(n + P)          // weights

        // Add sample rows
        var r = 0
        while r < n do
            val s  = samples(r)
            val Ty = queueT(s.x, s.y, q)
            X(r) = designRow(s.x, Ty)
            z(r) = s.fhat
            w(r) = wDist(xk, s.x)
            r += 1
        end while

        // Ridge regularization rows: encourage λ≈1 and θ≈0 for stability
        val ridLam = new VectorD(P); ridLam(0) = 1.0
        X(r) = ridLam; z(r) = 1.0; w(r) = w0; r += 1
        var t = 1
        while t < P do
            val rr = new VectorD(P); rr(t) = 1.0
            X(r) = rr; z(r) = 0.0; w(r) = w0
            r += 1; t += 1
        end while

        // Weighted least squares: min ||W^(1/2)(Xβ - z)||²
        val W12 = new MatrixD(n + P, n + P)         // W^(1/2): diagonal matrix of sqrt(weights)
        cfor (0, n + P) { i => W12(i, i) = sqrt(w(i)) }
        val Xt  = W12 * X
        val zt  = W12 * z
        val fac = new Fac_QR (Xt)                   // QR factorization
        fac.factor ()
        val beta = fac.solve (zt)                   // solve Xt * beta = zt

        lam   = beta(0)
        theta = beta(1 until P)
    end fitModel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sample uniformly inside TR ball: ||x_c - x_k|| ≤ Δ, with bound clipping.
     *  @param xk  the center of the trust region
     *  @param Δ   the trust-region radius
     *  @return    a random point inside the TR
     */
    private def sampleInTR (xk: VectorD, Δ: Double): VectorD =
        var s = new VectorD(d)
        var nrm = 0.0
        while
            var j = 0
            while j < d do
                val u = 2.0 * rng.nextDouble() - 1.0
                s(j) = u
                j += 1
            end while
            nrm = s.norm
            nrm < EPS                          // continue if norm too small
        do ()                                  // loop body (empty)
        s = s * (rng.nextDouble() * Δ / nrm)  // scale to random radius ≤ Δ
        val xNew = xk + s
        constrain(xNew)                        // apply bounds (modifies xNew in place)
        xNew
    end sampleInTR

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sample a point for model improvement (Section 4.3 of the paper).
     *  Default implementation: sample locally within current trust region.
     *  Users can override for custom sampling strategies.
     *  @param xk  the current iterate
     *  @param Δ   the current trust-region radius
     *  @return    a sample point for model improvement
     */
    protected def sampleForModelImprovement (xk: VectorD, Δ: Double): VectorD =
        sampleInTR(xk, Δ)
    end sampleForModelImprovement

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute criticality measure σ_k (optional, user-defined).
     *  Default implementation returns +∞ (criticality check disabled).
     *  Override to implement custom criticality tests.
     *  @param xk  the current iterate
     *  @return    the criticality measure (σ_k ≤ σ_c triggers conservative mode)
     */
    protected def criticalityMeasure (xk: VectorD): Double = Double.PositiveInfinity

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Enter conservative mode hook (optional, user-defined).
     *  Called when σ_k ≤ σ_c or Δ_k ≤ d̄.
     *  Default implementation: no-op. Override to implement custom conservative policies.
     */
    protected def enterConservativeMode (): Unit = { /* user may override */ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an inexact line search (not used by this algorithm).
     *  This method is required by the Minimizer trait but not used in DF-TR.
     *  @param x     the current point
     *  @param dir   the direction to search
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double = step

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate simulator at x and add sample to design.
     *  @param x  the point to evaluate
     *  @return   the sample with f̂(x) and y(x,q)
     */
    private def evalAndAdd (x: VectorD): Sample =
        val y = solveY(x, q)
        val fhat = simF(x)
        val s = Sample(x, fhat, y)
        samples += s
        s
    end evalAndAdd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the optimization problem using the DF-TR algorithm (Section 4.2).
     *  Implements all five steps of the trust-region loop with proper TR update logic.
     *  @param x0     the initial point
     *  @param step   step size (unused in this algorithm)
     *  @param toler  convergence tolerance
     *  @return       (best objective value, best point found)
     */
    override def solve (x0: VectorD, step: Double = STEP, toler: Double = 1e-6): FuncVec =
        require (x0.dim == d, s"Dimension mismatch: x0.dim=${x0.dim}, expected d=$d")

        initializeMonitoring (maxIter)

        var xk    = x0.copy
        constrain(xk)                   // apply bounds to initial point
        var fk    = simF(xk)
        var yk    = solveY(xk, q)
        samples.clear(); samples += Sample(xk, fk, yk)

        var Δ     = D0
        var k     = 0
        var calls = 1
        var uk    = 0                   // successive rejections counter

        var xBest = xk.copy
        var fBest = fk

        // Step 0: Initial fit m₀ at x₀
        fitModel(xk)
        debug ("solve", s"Initial: x0=$xk, f0=$fk, Δ0=$Δ")

        while k < maxIter && calls < budget do
            // ========================================================================
            // Step 1: Criticality check
            // ========================================================================
            val sigmaK = criticalityMeasure(xk)
            if sigmaK <= sigC then
                debug ("solve", s"Criticality: σ_k=$sigmaK ≤ σ_c=$sigC")
                enterConservativeMode()
            end if

            // ========================================================================
            // Step 2: Step calculation - solve argmin m_k(x) s.t. ||x - x_k|| ≤ Δ
            // ========================================================================
            val mk_xk = mPred(xk)
            var mBest = Double.PositiveInfinity
            var xTry  = xk
            var i = 0
            while i < Ncand do
                val xc = sampleInTR(xk, Δ)
                val mc = mPred(xc)
                if mc < mBest then { mBest = mc; xTry = xc }
                i += 1
            end while

            // ========================================================================
            // Step 3: Acceptance test
            // ========================================================================
            val sTry  = xTry - xk
            val fTry  = simF(xTry); calls += 1
            val yTry  = solveY(xTry, q)
            samples += Sample(xTry, fTry, yTry)

            val mRed = mk_xk - mBest        // predicted reduction
            val fRed = fk    - fTry         // actual reduction
            val rho  = if math.abs(mRed) < 1e-12 then -1.0 else fRed / mRed

            val accepted = rho >= eta1
            if accepted then
                xk = xTry; fk = fTry; yk = yTry; uk = 0
                if fk < fBest then { fBest = fk; xBest = xk.copy }
                debug ("solve", s"k=$k: ACCEPT, ρ=$rho, f=$fk, Δ=$Δ")
            else
                uk += 1
                debug ("solve", s"k=$k: REJECT, ρ=$rho, u_k=$uk, Δ=$Δ")
            end if

            // Refit model with all samples (weights centered at current xk)
            fitModel(xk)

            // ========================================================================
            // Step 4: Model improvement check
            // ========================================================================
            val etaNormPrev = max(EPS, sqrt(lamPrev*lamPrev + thetaPrev.normSq))
            val dLam  = lam - lamPrev
            val dThet = (theta - thetaPrev).norm
            val xi    = sqrt(dLam*dLam + dThet*dThet) / etaNormPrev

            if xi < xiBar && calls < budget then
                debug ("solve", s"Model improvement: ξ=$xi < ξ̄=$xiBar, sampling...")
                val xImp = sampleForModelImprovement(xk, Δ)
                val fImp = simF(xImp); calls += 1
                val yImp = solveY(xImp, q)
                samples += Sample(xImp, fImp, yImp)
                fitModel(xk)                // refit with improvement sample
                if fImp < fBest then { fBest = fImp; xBest = xImp.copy }
            end if

            // ========================================================================
            // Step 5: TR radius update (per §4.2 — three cases)
            // ========================================================================
            if rho > eta2 then
                // Excellent agreement: expand TR
                Δ = min(gInc * Δ, Dmax)
                debug ("solve", s"TR expand: ρ=$rho > η₂, Δ ← $Δ")
            else if rho > eta1 then
                // Moderate agreement: maintain TR (paper: no change)
                debug ("solve", s"TR maintain: η₁ < ρ=$rho ≤ η₂, Δ = $Δ")
            else if uk >= uBar then
                // Too many rejections: reset as max(Δ, d̄), then reset counter
                Δ = max(Δ, dBar)
                uk = 0
                debug ("solve", s"TR reset: u_k ≥ ū, Δ ← $Δ, u_k ← 0")
            end if
            // else: Δ unchanged

            // Check for conservative mode (applies regardless of update case)
            if Δ <= dBar then
                debug ("solve", s"TR too small: Δ=$Δ ≤ d̄=$dBar")
                enterConservativeMode()
            end if

            updateMonitoring (k+1, fBest)

            // Convergence check - only check after sufficient iterations
            k += 1
            if k > 1 && (xk - xBest).norm < toler then
                debug ("solve", s"Converged: ||x_k - x_best|| < $toler")
                return (fBest, xBest)
            end if
        end while

        finalizeMonitoring ()

        // Print optimization summary
        println ()
        println (sline (70).trim)
        println ("METAMODEL-TR: OPTIMIZATION SUMMARY")
        println (sline (70).trim)
        println (f"${"Metric"}%-30s | ${"Value"}%s")
        println (sline (70).trim)
        println (f"${"Best objective value"}%-30s | $fBest%.8f")
        println (f"${"Best solution"}%-30s | $xBest")
        println (f"${"Final iterate"}%-30s | $xk")
        println (f"${"Total iterations"}%-30s | $k")
        println (f"${"Simulation calls used"}%-30s | $calls / $budget")
        println (f"${"Final TR radius"}%-30s | $Δ%.6f")
        println (f"${"Samples collected"}%-30s | ${samples.size}")
        println (f"${"Final λ"}%-30s | $lam%.6f")
        println (sline (70).trim)

        (fBest, xBest)
    end solve

end MetaModelTR


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `metaModelTRTest` main function tests the `MetaModelTR` class.
 *  Uses a simple test case with a quadratic objective function and noise.
 *  > runMain scalation.optimization.metaModelTRTest
 */
@main def metaModelTRTest (): Unit =

    import scalation.random.Uniform

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

    val noise = Uniform (-0.1, 0.1)    // noise term for stochastic objective

    // Stochastic simulator
    def simF (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1 + noise.gen

    // Simplified analytical model (no actual queueing, just a quadratic approximation)
    def queueT (x: VectorD, y: VectorD, q: VectorD): Double =
        (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1.0

    // Trivial state solver (for this simple test)
    def solveY (x: VectorD, q: VectorD): VectorD = new VectorD(2)

    val q  = new VectorD(2)             // dummy exogenous parameters
    val x0 = VectorD (1, 2)             // initial point
    val d  = 2                          // dimension

    println ("\n=== Test 1: Standard TR optimization ===")
    val optimizer1 = new MetaModelTR (simF, queueT, solveY, q, d, maxIter=100, budget=100)
    optimizer1.setVerbose (1)
    optimizer1.setPrintEvery (10)
    val (fBest1, xBest1) = optimizer1.solve (x0)
    println (f"Result: f*=$fBest1%.6f, x*=$xBest1")

    println ("\n=== Test 2: Tighter budget ===")
    val optimizer2 = new MetaModelTR (simF, queueT, solveY, q, d, maxIter=100, budget=100)
    optimizer2.setVerbose (0)            // silent mode
    val (fBest2, xBest2) = optimizer2.solve (x0)
    println (f"Result: f*=$fBest2%.6f, x*=$xBest2")

    // Plot convergence (optional)
    // optimizer1.plotLoss()

    println ("\n" + "=" * 70)
    println ("TEST SUMMARY")
    println ("=" * 70)
    println ("True optimum: x* = [3, 4], f(x*) ≈ 1.0")
    println (f"Test 1 result: f* = $fBest1%.6f, x* = $xBest1")
    println (f"Test 2 result: f* = $fBest2%.6f, x* = $xBest2")
    println ("=" * 70)

end metaModelTRTest
