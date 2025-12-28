
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Yousef Fekri Dabanloo
 *  @version 2.0
 *  @date    Tue Jul  1 17:54:49 EDT 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Bridge Regression (Lq for q > 0 Shrinkage/Regularization)
 *
 *  Caveat: currently only supports L0.5 Regularization (Bridge Regression with q = 0.5)
 *  Implements iterative re-weighted least squares (IRLS) to handle non-convex L0.5 penalty.
 *
 *  Model: minimize ||y - Xb||^2 + lambda * \sum |b_i|^0.5
 *
 *  Reference:
 *    - S. K. M. Wong, "Bridge regression models and IRLS",
 *      Journal of Statistical Computation and Simulation, 1995.
 *    - Hastie, Tibshirani & Friedman (2009), Elements of Statistical Learning, Sec. on Bridge.
 */
package scalation
package modeling

import scala.math.{abs, pow}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BridgeRegression` class supports L0.5 Regularization (Bridge Regression with q = 0.5)
 *  using Iterative Re-weighted Least Squares (IRLS) to handle non-convex L0.5 penalty.
 *  @param x       the centered data/input m-by-n matrix
 *  @param y       the centered response/output m-vector
 *  @param fname_  feature names
 *  @param hparam  hyper-parameters: "lambda" (penalty), "maxIter", "tol", "eps"
 */
class BridgeRegression (x: MatrixD, y: VectorD, fname_ : Array [String] = null,
                        hparam: HyperParameter = RidgeRegression.hp)
    extends Predictor (x, y, fname_, hparam)
        with Fit (dfr = x.dim2, df = x.dim - x.dim2 - 1):

    private val debug   = debugf ("BridgeRegression", false)
    private val lambda  = hparam("lambda").toDouble                  // shrinkage parameter
    private val sparse  = hparam("sparse").toInt == 1                // whether to sparsify
    private val maxIter = hparam("maxIter").toInt                    // maximum number of iterations for IWLS
    private val tol     = hparam("tol").toDouble                     // tolerance for convergence
    private val eps     = hparam("eps").toDouble                     // small constant to avoid division by zero
    private val q       = hparam("pow").toDouble                     // exponent/L_q norm
    private val qq      = 2 - q

    _modelName = s"BridgeRegression_$q"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train via IRLS: iterate solving weighted ridge until convergence.
     *  @param x_  the training/full data/input matrix (defaults to full x)
     *  @param y_  the training/full response/output vector (defaults to full y)
     */
    def train (x_ : MatrixD = x, y_ : VectorD = y): Unit = 
        var b_old = new VectorD (x_.dim2)                            // initialize at zero
        b         = b_old.copy                                       // initial b-vector
        val xtX   = x_.ᵀ * x_                                // form modified normal equations: X^T X + λ W
        val xty   = x_.ᵀ * y_

        var (go, it) = (true, 1)
        while go && it <= maxIter do
            val xtX_ = xtX.copy
            val w = b.map (e => pow (abs (e) + eps, qq))             // compute weights w_i = (|b_i| + eps)^(2 - q)
            for i <- w.indices do xtX_(i, i) += lambda * w(i)        // add λ * w(i) to diagonal

            val fac = new Fac_Cholesky (xtX_)                        // solve for b via Cholesky
            fac.factor ()
            b = fac.solve (xty)
            if (b - b_old).norm < tol then                           // check convergence
                debug ("train", s"converged after $it iterations")
                go = false
            b_old = b.copy
            it   += 1
        end while
        if go then debug ("train", s"completed $maxIter iterations without convergence")
        if sparse then LassoRegression.sparsify (b)
    end train
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test a predictive model y_ = f(x_) + e and return its QoF vector.
     *  Testing may be be in-sample (on the training set) or out-of-sample
     *  (on the testing set) as determined by the parameters passed in.
     *  Note: must call train before test.
     *  @param x_  the testing/full data/input matrix (defaults to full x)
     *  @param y_  the testing/full response/output vector (defaults to full y)
     */
    def test (x_ : MatrixD = x, y_ : VectorD = y): (VectorD, VectorD) =
        val yp = predict (x_)                                        // make predictions
        (yp, diagnose (y_, yp))                                      // return predictions and QoF vector
    end test
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the response: y = X b
     *  @param x_  the matrix to use for making predictions, one for each row
     */
    override def predict (x_ : MatrixD): VectorD = x_ * b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Quality-of-Fit summary reuses Fit.summary
     *  @param x_      the testing/full data/input matrix
     *  @param fname_  the array of feature/variable names
     *  @param b_      the parameters/coefficients for the model
     *  @param vifs    the Variance Inflation Factors (VIFs)
     */
    override def summary (x_ : MatrixD = getX,
                          fname_ : Array [String] = fname,
                          b_ : VectorD = b,
                          vifs: VectorD = vif ()): String =
        super.summary (x_, fname_, b_, vifs)
    end summary

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a sub-model that is restricted to the given columns of the data matrix.
     *  @param x_cols  the columns that the new model is restricted to
     *  @param fname2  the variable/feature names for the new model (defaults to null)
     */
    def buildModel (x_cols: MatrixD, fname2: Array [String] = null): BridgeRegression =
        debug ("buildModel", s"${x_cols.dim} by ${x_cols.dim2}")
        new BridgeRegression (x_cols, y, fname2, hparam)
    end buildModel
    
end BridgeRegression


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BridgeRegression` companion object defines hyper-parameters and factory methods.
 */
object BridgeRegression:

    val hp = RidgeRegression.hp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `BridgeRegression` object from an xy matrix and center the data.
     *  @param xy      the uncentered data/input m-by-n matrix, NOT augmented with a first column of ones
     *                     and the uncentered response m-vector (combined)
     *  @param fname   the feature/variable names (defaults to null)
     *  @param hparam  includes the shrinkage hyper-parameter
     *  @param col     the designated response column (defaults to the last column)
     */
    def apply (xy: MatrixD, fname: Array [String] = null,
               hparam: HyperParameter = hp)(col: Int = xy.dim2 - 1): BridgeRegression = 
        val (x, y) = (xy.not(?, col), xy(?, col))
        val mu_x = x.mean
        val mu_y = y.mean
        val x_c  = x - mu_x
        val y_c  = y - mu_y
        new BridgeRegression (x_c, y_c, fname, hparam)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `BridgeRegression` object from an x matrix and y vector and center the data.
     *  @param x       the uncentered data/input m-by-n matrix, NOT augmented with a first column of ones
     *  @param y       the uncentered response/output vector
     *  @param fname   the feature/variable names (defaults to null)
     *  @param hparam  includes the shrinkage hyper-parameter
     */
    def center (x: MatrixD, y: VectorD, fname: Array [String] = null,
                hparam: HyperParameter = hp): BridgeRegression = 
        val mu_x = x.mean
        val mu_y = y.mean
        val x_c  = x - mu_x
        val y_c  = y - mu_y
        new BridgeRegression (x_c, y_c, fname, hparam)
    end center    

end BridgeRegression


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bridgeRegressionTest` main function tests the `bridgeRegression` class
 *  on the AutoMPG dataset.
 *  > runMain scalation.modeling.bridgeRegressionTest
 */
@main def bridgeRegressionTest(): Unit =

    import Example_AutoMPG._

    banner ("AutoMPG Regression")
    val reg = new Regression (ox, y, ox_fname)                       // create a regression model (with intercept)
    reg.trainNtest ()()                                              // train and test the model
    println (reg.summary ())                                         // parameter/coefficient statistics
    
    banner ("AutoMPG Bridge Regression")
    val mod = new BridgeRegression (x, y, x_fname)                   // create a bridge regression model (no intercept)
    mod.trainNtest ()()                                              // train and test the model
    println (mod.summary ())

end bridgeRegressionTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bridgeRegressionTest2` main function tests the multi-collinearity method in
 *  the `bridgeRegression` class using the following regression equation.
 *      y  =  b dot x  =  b_1*x_1 + b_2*x_2
 *  Contour Plots for see, L2 penalty, see + L2 penalty, L1 penalty, sse + L1 penalty
 *                         L.5 penalty, see _ L.5 penalty
 *  > runMain scalation.modeling.bridgeRegressionTest2
 */
@main def bridgeRegressionTest2 (): Unit =

    val rvg = random.RandomVecD (100)
    val nrm = random.NormalVec_c (100, 0, 50)
    val x_1 = rvg.gen
    val x_2 = rvg.gen
    val x   = MatrixD (x_1, x_2).ᵀ

    val b_ = VectorD (4, 5)
    val y  = x * b_ + nrm.gen
    val xy = x :^+ y
    println (s"Correlation matrix for xy: rho = ${xy.corr}")

    val x_c = x - x.mean
    val y_c = y - y.mean

    banner ("Regression Model")
    val mod = new Regression (x_c, y_c)
    mod.trainNtest ()()
    println (mod.summary ())
    FitM.showQofStatTable (mod.crossValidate ())
    var lambda = 0.0

    banner ("Ridge Regression Model")
    for i <- 1 to 10 do
        lambda = 200.0 * i
        RidgeRegression.hp("lambda") = lambda
        val mod2 = new RidgeRegression (x_c, y_c)
        mod2.trainNtest ()()
        println (mod2.summary ())
        FitM.showQofStatTable (mod2.crossValidate ())
    end for

    banner ("Lasso Regression Model")
    for i <- 1 to 10 do
        lambda = 2000.0 * i
        RidgeRegression.hp("lambda") = lambda
        val mod2 = new LassoRegression (x_c, y_c)
        mod2.trainNtest ()()
        println (mod2.summary ())
        FitM.showQofStatTable (mod2.crossValidate ())
    end for

    banner ("Bridge Regression Model")
    for i <- 1 to 10 do
        lambda = 4000.0 * i
        RidgeRegression.hp("lambda") = lambda
        val mod2 = new BridgeRegression (x_c, y_c)
        mod2.trainNtest ()()
        println (mod2.summary ())
        FitM.showQofStatTable (mod2.crossValidate ())
    end for

    def f(b: VectorD):  Double = (y - x * b).normSq
    def f2(b: VectorD): Double = b.normSq * 2000.0
    def f3(b: VectorD): Double = f(b) + f2(b)
    def f4(b: VectorD): Double = b.norm1 * 20000.0
    def f5(b: VectorD): Double = f(b) + f4(b)
    def f6(b: VectorD): Double = b.norm_qq (0.5) * 40000.0
    def f7(b: VectorD): Double = f(b) + f6(b)

    val lb = VectorD (3, 4)
    val ub = VectorD (5, 6)
    new PlotC (f,  lb, ub, title = "Contour plot of sse")
    new PlotC (f2, lb, ub, title = "Contour plot of L2 penalty")
    new PlotC (f3, lb, ub, title = "Contour Plot of sse + L2 penalty")
    new PlotC (f4, lb, ub, title = "Contour Plot of L1 penalty")
    new PlotC (f5, lb, ub, title = "Contour Plot of sse + L1 penalty")
    new PlotC (f6, lb, ub, title = "Contour Plot of L.5 penalty")
    new PlotC (f7, lb, ub, title = "Contour Plot of sse + L.5 penalty")

end bridgeRegressionTest2

