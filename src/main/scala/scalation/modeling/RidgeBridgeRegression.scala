
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Yousef Fekri Dabanloo
 *  @version 2.0
 *  @date    Thu Jul 24 11:23:31 EST 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Multiple Linear Regression with Ridge-Bridge Regularization
 */

package scalation
package modeling

import scala.math.abs
import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeBridgeRegression` class supports multiple linear regression with a hybrid
 *  of Ridge (L2) and Bridge (Lq with 0 < q < 1) regularization. It solves:
 *      y = Xb + e
 *  by minimizing:
 *      ||y - Xb||^2 + lambda * ||b||^2 + beta * sum(|b_j|^q)
 *  @param x       the centered data/input matrix
 *  @param y       the centered response/output vector
 *  @param fname_  the feature/variable names (defaults to null)
 *  @param hparam  the regularization hyper-parameters (lambda for ridge, beta for bridge, q)
 */
class RidgeBridgeRegression (x: MatrixD, y: VectorD, fname_ : Array [String] = null,
                             hparam: HyperParameter = RidgeRegression.hp)
      extends Predictor (x, y, fname_, hparam)
         with Fit (dfr = x.dim2, df = x.dim - x.dim2 - 1):

//  private val debug   = debugf ("RidgeBridgeRegression", false)
    private val lambda  = hparam("lambda").toDouble                      // the L_2 shrinkage parameter
    private val beta    = hparam("beta").toDouble                        // the L_q shrinkage parameter
    private val sparse  = hparam("sparse").toInt == 1                    // whether to sparsify
    private val maxIter = hparam("maxIter").toInt                        // maximum number of iterations for IRR
    private val tol     = hparam("tol").toDouble                         // tolerance for convergence
    private val eps     = hparam("eps").toDouble                         // small constant to avoid division by zero
    private val maxW    = 1E6                                            // maximum weight
    private val q       = hparam("pow").toDouble                         // exponent/L_q norm
    private val q_2     = q - 2.0

    _modelName = s"RidgeBridgeRegression_$q"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model using Iterative Reweighted Ridge Regression (IRR).
     *  @param x_  the input/data matrix
     *  @param y_  the output/response vector
     */
    def train (x_ : MatrixD = x, y_ : VectorD = y): Unit =
        val xtX = x_.ᵀ * x_
        val xty = x_.ᵀ * y_
        val n   = x_.dim2
        val w   = new MatrixD (n, n)                            // diagonal weight matrix

        val ridgeMod = RidgeRegression.center (x_, y_, fname_, hparam)
        ridgeMod.trainNtest ()()
        b = ridgeMod.parameter

        var (iter, diff) = (0, Double.MaxValue)
        while iter < maxIter && diff > tol do
            cfor (0, n) { j =>
                val wj  = if abs(b(j)) > eps then (q / 2.0) * abs(b(j)) ~^ q_2 else maxW
                w(j, j) = beta * wj + lambda
            } // cfor

            val fac   = Fac_Cholesky (xtX + w).factor ()
            val b_new = fac.solve (xty)

            diff  = (b_new - b).norm
            b     = b_new
            iter += 1
        end while
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
        val yp = predict (x_)                                   // make predictions
        (yp, diagnose (y_, yp))                                 // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of vector y = f(x_, b).
     *  @param x_  the matrix to use for making predictions, one for each row
     */
    override def predict (x_ : MatrixD): VectorD = x_ * b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for each predictor 'x_j'
     *  and the overall Quality of Fit (QoF).
     *  @param x_      the testing/full data/input matrix
     *  @param fname_  the array of feature/variable names
     *  @param b_      the parameters/coefficients for the model
     *  @param vifs    the Variance Inflation Factors (VIFs)
     */
    override def summary (x_ : MatrixD = getX, fname_ : Array [String] = fname, b_ : VectorD = b,
                         vifs: VectorD = vif ()): String =
        super.summary (x_, fname_, b_, vifs)                    // summary from `Fit`
    end summary

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a sub-model that is restricted to the given columns of the data matrix.
     *  @param x_cols  the columns that the new model is restricted to
     *  @param fname2  the variable/feature names for the new model (defaults to null)
     */
    override def buildModel (x_cols: MatrixD, fname2: Array [String] = null): RidgeBridgeRegression =
        new RidgeBridgeRegression (x_cols, y, fname2, hparam)
    end buildModel

end RidgeBridgeRegression


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeBridgeRegression` companion object provides default hyper-parameters
 *  and convenience factory methods.
 */
object RidgeBridgeRegression:

    val hp = RidgeRegression.hp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a RidgeBridgeRegression from a combined xy matrix.
     *  @param xy      the centered combines x and y matrix
     *  @param fname_  the feature/variable names (defaults to null)
     *  @param hparam  the regularization hyper-parameters (lambda for ridge, beta for bridge, q)
     *  @param col     the column used for response variable
     */
    def apply (xy: MatrixD, fname: Array [String] = null,
               hparam: HyperParameter = hp)(col: Int = xy.dim2 - 1): RidgeBridgeRegression =
        val (x, y) = (xy.not(?, col), xy(?, col))
        val mu_x = x.mean
        val mu_y = y.mean
        val x_c  = x - mu_x
        val y_c  = y - mu_y
        new RidgeBridgeRegression (x_c, y_c, fname, hparam)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a RidgeBridgeRegression from an x matrix and y vector.
     *  @param x       the centered data/input matrix
     *  @param y       the centered response/output vector
     *  @param fname_  the feature/variable names (defaults to null)
     *  @param hparam  the regularization hyper-parameters (lambda for ridge, beta for bridge, q)
     */
    def center (x: MatrixD, y: VectorD, fname: Array [String] = null,
                hparam: HyperParameter = hp): RidgeBridgeRegression =
        val mu_x = x.mean
        val mu_y = y.mean
        val x_c  = x - mu_x
        val y_c  = y - mu_y
        new RidgeBridgeRegression (x_c, y_c, fname, hparam)
    end center

end RidgeBridgeRegression


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ridgeBridgeRegressionTest` main function tests the `RidgeBridgeRegression` class using
 *  the AutoMPG dataset.  Assumes no missing values.
 *  It also combines feature selection with cross-validation and plots
 *  R^2, R^2 bar and R^2 cv vs. the instance index.
 *  Note, since x0 is automatically included in feature selection, make it an important variable.
 *  > runMain scalation.modeling.ridgeBridgeRegressionTest
 */

@main def ridgeBridgeRegressionTest (): Unit =

    import scalation.modeling.Example_AutoMPG._          // import sample dataset (x, y, x_fname, etc.)
    import RidgeRegression.hp

    hp("beta") = 10.0
    banner("AutoMPG Regression")
    val reg = new Regression(ox, y, ox_fname)
    reg.trainNtest()()
    println(reg.summary())

    banner("AutoMPG Ridge Regression")
    val mod1 = RidgeRegression.center (x, y, x_fname)
    mod1.trainNtest ()()
    println (mod1.summary ())

    banner("AutoMPG Ridge + Bridge Regression")
    val mod2 = RidgeBridgeRegression.center (x, y, x_fname)
    mod2.trainNtest ()()
    println (mod2.summary ())

end ridgeBridgeRegressionTest

