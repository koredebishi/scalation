
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Jan 17 15:04:21 EST 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Abstract Class for Forecasters that utilize Regression
 *
 *  @see `scalation.modeling.Regression`
 */

package scalation
package modeling
package forecasting

import scala.collection.mutable.LinkedHashSet
import scala.math.{max, min}
import scala.util.control.Breaks.{break, breakable}

import scalation.mathstat._
import scalation.modeling.{Regression => REGRESSION}
//import scalation.modeling.{RidgeRegression => REGRESSION}
//import scalation.modeling.{LassoRegression => REGRESSION}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Forecaster_Reg` abstract class provides base methods for use by extending classes
 *  that utilize regression for time series forecasting.
 *  @param x        the data/input matrix (lagged columns of y and xe) @see `ARX.apply`
 *  @param y        the response/output vector (time series data)
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param fname    the feature/variable names
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 */
abstract class Forecaster_Reg (x: MatrixD, y: VectorD, hh: Int, fname: Array [String],
                               tRng: Range = null, hparam: HyperParameter = MakeMatrix4TS.hp,
                               bakcast: Boolean = false)
      extends Forecaster (y, hh, tRng, hparam, bakcast)
         with FeatureSelection:

    private   val debug = debugf ("Forecaster_Reg", false)                // debug function
    private   val flaw  = flawf ("Forecaster_Reg")                        // debug function
    protected val reg   = new REGRESSION (x, y, fname, hparam)            // delegate training to regression
    protected val nneg  = hparam("nneg").toInt == 1                       // 0 => unrestricted, 1 => predictions must be non-negative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the data/input matrix built from lagged y (optionally xe) values.
     */
    override def getX: MatrixD = x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARY` model to the times-series data in vector y_.
     *  Estimate the coefficient vector b for a p-th order Auto-Regressive ARY(p) model.
     *  Uses OLS Matrix Fatorization to determine the coefficients, i.e., the b (φ) vector.
     *  @param x_  the data/input matrix (e.g., full x)
     *  @param y_  the training/full response vector (e.g., full y)
     */
    override def train (x_ : MatrixD, y_ : VectorD): Unit =
        debug ("train", s"$modelName, x_.dims = ${x_.dims}, y_.dim = ${y_.dim}")
        reg.train (x_, y_)                                              // train the regression model
        b = reg.parameter                                               // coefficients from regression
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train and test the forecasting model y_ = f(y-past) + e and report its QoF
     *  and plot its predictions.  Return the predictions and QoF.
     *  NOTE: must use `trainNtest_x` when an x matrix is used, such as in `ARY`.
     *  @param x_  the training/full data/input matrix (defaults to full x)
     *  @param y_  the training/full response/output vector (defaults to full y)
     *  @param xx  the testing/full data/input matrix (defaults to full x)
     *  @param yy  the testing/full response/output vector (defaults to full y)
     */
    def trainNtest_x (x_ : MatrixD = x, y_ : VectorD = y)
                     (xx: MatrixD = x, yy: VectorD = y): (VectorD, VectorD) =
        val y_tr = y_.drop () :+ -0.0                                   // skip the first, add placeholder past end
        train (x_, y_tr)                                                // train the model on training set
        val (yp, qof) = test (xx, yy)                                   // test the model on testing set
        println (report (qof))                                          // report on Quality of Fit (QoF)
        (yp, qof)
    end trainNtest_x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a forecasting model y_ = f(lags (y_)) + e
     *  and RETURN (1) aligned actual values, (2) its forecasts and (3) QoF vector.
     *  Testing may be in-sample (on the training set) or out-of-sample (on the testing set)
     *  as determined by the parameters passed in.  Note: must call train and forecastAll
     *  before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     */
    override def testF (h: Int, y_ : VectorD): (VectorD, VectorD, VectorD) =
        val h_  = h - 1
        val yy  = y_(h_ until y_.dim)                                   // align the actual values
        val yfh = yf(?, h)(0 until y_.dim-h_)                           // column h of the forecast matrix
        println (s"yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      Forecaster.differ (yy, yfh)                                     // uncomment for debugging
        assert (yy.dim == yfh.dim)                                      // make sure the vector sizes agree

        new Plot (null, yy, yfh, s"testF: yy, yfh vs. t for $modelName @h = $h", lines = true)
        mod_resetDF (yy.dim)                                            // reset the degrees of freedom
        (yy, yfh, diagnose (yy, yfh))                                   // return actual, forecasted and QoF vectors
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t using the 1-step ahead forecast.
     *
     *      y_t = b_0 + b_1 y_t-1 + b_2 y_t-2 + ... + b_p y_t-p = b dot x_t
     *
     *  Caveat:  parameter order is in conflict with AR models.
     *  @see `modeling.rectify` define in `Predictor.scala`
     *  @param t   the time point being predicted
     *  @param y_  the actual values to use in making predictions (ignored)
     */
    override def predict (t: Int, y_ : VectorD): Double =
        val yp = rectify (reg.predict (x(t-1)), nneg)
        if t < y_.dim then
            debug ("predict", s"@t = $t, b = $b dot x(t-1) = ${x(t-1)} = yp = $yp vs. y_ = ${y_(t)}")
        yp
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forge a new vector from the first spec values of x, the last p-h+1 values
     *  of x (past values) and recent values 1 to h-1 from the forecasts.
     *  @param xx  the t-th row of the input matrix (lagged actual values)
     *  @param yy  the t-th row of the forecast matrix (forecasted future values)
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forge (xx: VectorD, yy: VectorD, h: Int): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size hh, h = 1 to hh-steps ahead forecasts for the model,
     *  i.e., forecast the following time points:  t+1, ..., t+h.
     *  Intended to work with rolling validation (analog of predict method).
     *  @param t   the time point from which to make forecasts
     *  @param y_  the actual values to use in making predictions
     */
    override def forecast (t: Int, y_ : VectorD = y): VectorD =
        val yh = new VectorD (hh)                                       // hold forecasts for each horizon
        for h <- 1 to hh do
            val xy   = forge (x(min (t+1, x.dim-1)), yf(t), h)          // FIX - why t+1
            val pred = rectify (reg.predict (xy), nneg)                 // slide in prior forecasted values
//          debug ("forecast", s"h = $h, @t = $t, xy = $xy, yp = $pred, y_ = ${y_(t)}")
            yf(t, h) = pred                                             // record in forecast matrix
            yh(h-1)  = pred                                             // record forecasts for each horizon
        yh                                                              // return forecasts for all horizons
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign into FORECAST MATRIX and return the h-steps ahead forecast.
     *  Note, `predictAll` provides predictions for h = 1.
     *  @see `forecastAll` method in `Forecaster` trait.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the actual values to use in making forecasts
     */
    override def forecastAt (h: Int, y_ : VectorD = y): VectorD =
        if h < 2 then flaw ("forecastAt", s"horizon h = $h must be at least 2")

        for t <- y_.indices do                                          // make forecasts over all time points for horizon h
            val xy = forge (x(t), yf(t), h)
            val pred = rectify (reg.predict (xy), nneg)
//          debug ("forecastAt", s"h = $h, @t = $t, xy = $xy, yp = $pred, y_ = ${y_(t)}")
            yf(t, h) = pred                                             // record in forecast matrix
        yf(?, h)                                                        // return the h-step ahead forecast vector
    end forecastAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Models need to provide a means for updating the Degrees of Freedom (DF).
     *  @param size  the size of dataset (full, train, or test)
     */
    override def mod_resetDF (size: Int): Unit =
        val dfm = max (1, parameter.size - 1)                           // degrees of freedom for model/dataset
        debug ("mod_resetDF", s"dfm = $dfm, df = ${size-dfm}")
        resetDF (dfm, size - dfm)
    end mod_resetDF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for each predictor 'x_j'
     *  and the overall Quality of Fit (QoF).
     *  @param x_      the testing/full data/input matrix
     *  @param fname_  the array of feature/variable names
     *  @param b_      the parameters/coefficients for the model
     *  @param vifs    the Variance Inflation Factors (VIFs)
     */
    override def summary (x_ : MatrixD = getX, fname_ : Array [String] = fname,
                          b_ : VectorD = b, vifs: VectorD = reg.vif ()): String =
        super.summary (x_, fname_, b_, vifs)                             // summary from `Fit`
    end summary

//  F E A T U R E   S E L E C T I O N

    private var theBest = BestStep ()()                                      // record the best model from feature selection

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the best-step to default
     */
    def resetBest (): Unit = theBest = BestStep ()()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the best model found from feature selection.
     */
    def getBest: BestStep = theBest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** When the new best-step is better than theBest, replace theBest.
     *  Note: for QoF where smaller if better, must switch to '<'.
     *  @param best  new best-step found during feature selection
     *  @param qk    index of Quality of Fit (QoF) to use for comparing quality
     */
    private def updateBest (best: BestStep, qk: Int = QoF.rSqBar.ordinal): Unit =
        if best.qof != null then
            if theBest.qof == null || best.qof(qk) > theBest.qof(qk) then theBest = best
    end updateBest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variable to add the
     *  existing model, returning the variable to add and the new model.
     *  May be called repeatedly.
     *  @see `Fit` for index of QoF measures.
     *  @param cols  the lags/columns currently included in the existing model (currently ignored)
     *  @param qk    index of Quality of Fit (QoF) to use for comparing quality
     */
    override def forwardSel (cols: LinkedHashSet [Int], qk: Int = QoF.smape.ordinal): BestStep =
        banner ("Forecaster.forwardSel: adapt and delegate to `reg`")
        // adapt from regression to time series forecsting
//      reg.forwardSel (cols, qk)                                            // delegate - may not work

        var best = BestStep ()()                                             // best step so far

        for j <- x.indices2 if ! (cols contains j) do
            val cols_j = cols union LinkedHashSet (j)                        // try adding variable/column x_j
            val x_cols = x(?, cols_j)                                        // x projected onto cols_j columns
            val mod_j  = reg.buildModel (x_cols)                             // regress with x_j added
            val y_tr = y.drop () :+ -0.0                                     // skip the first, add placeholder past end
            mod_j.train (x_cols, y_tr)                                       // train model
            best = best.better (j, mod_j.test ()._2, mod_j)                  // which is better
        end for

        if best.col == -1 then
            flaw ("forwardSel", "could not find a variable x_j to add: best.col = -1")
        best
    end forwardSel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variables to have
     *  in the model, returning the variables added and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `modeling.Fit` for index of QoF measures.
     *  @see `modeling.Predictor` for more information
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     *  @param cross  whether to include the cross/roll-validation QoF measure
     */
    override def forwardSelAll (qk: Int = QoF.smape.ordinal, cross: Boolean = false):
                               (LinkedHashSet [Int], MatrixD) =
        resetBest ()
        val rSq  = new MatrixD (x.dim2, Fit.qofVectorSize)                   // QoF: R^2, R^2 Bar, sMAPE, R^2 cv
        val cols = LinkedHashSet (0)                                         // start with x_0 in model (e.g., intercept)
        updateQoF (rSq, 0, cross, reg.select0 (qk))                          // update Qof results for 0-th variable

        banner (s"forwardSelAll: (l = 0) INITIAL variable (0, ${fname(0)}) => cols = $cols")

        breakable {
            for l <- 1 until x.dim2 do
                val best = forwardSel (cols, qk)                             // add most predictive variable
                if best.col == -1 then break ()                              // could not find variable to add
                updateBest (best)
                cols += best.col                                             // add variable x_j
                updateQoF (rSq, l, cross, best)                              // update QoF results for l-th variable
                val (jj, jj_qof) = (best.col, best.qof(qk))
                banner (s"forwardSelAll: (l = $l) ADD variable ($jj, ${fname(jj)}) => cols = $cols @ $jj_qof")
            end for
        } // breakable

        (cols, rSq)
    end forwardSelAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to find the least predictive variables to remove
     *  from the full model, returning the variables left and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `modeling.Fit` for index of QoF measures.
     *  @see `modeling.Predictor` for more information
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     *  @param first  first variable to consider for elimination
     *  @param cross  whether to include the cross/roll-validation QoF measure
     */
    def backwardElimAll (qk: Int = QoF.smape.ordinal, first: Int = 1, cross: Boolean = false):
                        (LinkedHashSet [Int], MatrixD) = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform stepwise regression to find the most predictive variables to have
     *  in the model, returning the variables left and the new Quality of Fit (QoF)
     *  measures for all steps.  At each step it calls forwardSel and backwardElim
     *  and takes the best of the two actions.  Stops when neither action yields improvement.
     *  @see `modeling.Fit` for index of QoF measures.
     *  @see `modeling.Predictor` for more information
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     *  @param cross  whether to include the cross/roll-validation QoF measure
     *  @param swap   whether to allow a swap step (swap out a feature for a new feature in one step)
     */
    def stepwiseSelAll (qk: Int = QoF.smape.ordinal, cross: Boolean = false, swap: Boolean = true):
                       (LinkedHashSet [Int], MatrixD) = ???

end Forecaster_Reg

