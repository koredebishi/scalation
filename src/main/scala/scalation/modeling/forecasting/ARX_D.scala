
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jun 30 13:27:00 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive on lagged y and xe (ARX_D) using OLS - Direct Forecasting
 */

package scalation
package modeling
package forecasting

import scala.math.min

import scalation.mathstat._
import scalation.modeling.neuralnet.{RegressionMV => REGRESSION}

import Example_Covid.{loadData, response}
import MakeMatrix4TS._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_D` class provides basic time series analysis capabilities for
 *  ARX_D models.  ARX_D models are often used for forecasting.
 *  `ARX_D` uses DIRECT (as opposed to RECURSIVE) multi-horizon forecasting.
 *  Given time series data stored in vector y, its next value y_t = combination of last p values.
 *
 *      y_t = b dot x_t + e_t
 *
 *  where y_t is the value of y at time t and e_t is the residual/error term.
 *  @param x        the data/input matrix (lagged columns of y) @see `ARX_D.apply`
 *  @param y        the response/output matrix (column per horizon) (time series data) 
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param n_exo    the number of exogenous variables
 *  @param fname    the feature/variable names
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 */
class ARX_D (x: MatrixD, y: MatrixD, hh: Int, n_exo: Int, fname: Array [String] = null,
             tRng: Range = null, hparam: HyperParameter = hp,
             bakcast: Boolean = false)
      extends Forecaster_D (x, y, hh, tRng, hparam, bakcast):           // no automatic backcasting, @see `ARX_D.apply`

    private val debug = debugf ("ARX_D", true)                          // debug function
//  private val flaw  = flawf ("ARX_D")                                 // flaw function
    private val p     = hparam("p").toInt                               // use the last p endogenous values (p lags)
    private val q     = hparam("q").toInt                               // use the last q exogenous values (q lags)
    private val spec  = hparam("spec").toInt                            // trend terms: 0 - none, 1 - constant, 2 - linear, 3 - quadratic
                                                                        //              4 - sine, 5 cosine
    private val nneg  = hparam("nneg").toInt == 1                       // 0 => unrestricted, 1 => predictions must be non-negative
    private val reg   = new REGRESSION (x, y, fname, hparam)            // delegate training to multi-variate regression

    modelName = s"ARX_D($p, $q, $n_exo)"

    debug ("init", s"$modelName with $n_exo exogenous variables and additional term spec = $spec")
//  debug ("init", s"[ x | y ] = ${x ++^ y}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARX_D` model to the times-series data in vector y_.
     *  Estimate the coefficient mattrix bb for a p-th order Auto-Regressive ARX_D(p) model.
     *  Uses OLS Matrix Fatorization to determine the coefficients, i.e., the bb matrix.
     *  @param x_  the data/input matrix (e.g., full x)
     *  @param y_  the training/full response vector (e.g., full y)
     */
    def train_x (x_ : MatrixD, y_ : MatrixD): Unit =
        debug ("train_x", s"$modelName, x_.dim = ${x_.dim}, y_.dim = ${y_.dim}")
        reg.train (x_, y_)                                              // train the multi-variate regression model
        bb = reg.parameter                                              // coefficients from regression
    end train_x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for each predictor 'x_j'
     *  and the overall Quality of Fit (QoF).
     *  @param x_      the testing/full data/input matrix
     *  @param fname_  the array of feature/variable names
     *  @param b_      the parameters/coefficients for the model
     *  @param vifs    the Variance Inflation Factors (VIFs)
     */
    override def summary (x_ : MatrixD = getX, fname_ : Array [String] = reg.getFname,
                          b_ : VectorD = b, vifs: VectorD = reg.vif ()): String =
        super.summary (x_, fname_, b_, vifs)                             // summary from `Fit`
    end summary

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t using the 1-step ahead forecast.
     *
     *      y_t = b_0 + b_1 y_t-1 + b_2 y_t-2 + ... + b_p y_t-p = b dot x_t
     *
     *  FIX - parameter order is in conflict with AR models.
     *  @param t   the time point being predicted
     *  @param y_  the actual values to use in making predictions (ignored)
     */
    def predict (t: Int, y_ : MatrixD): VectorD =
        val yp = rectify (reg.predict (x(t)), nneg)                   // FIX - change to x(t-1)
        if t < y_.dim then
            debug ("predict", s"@t = $t, x(t-1) = ${x(t-1)}, yp = $yp vs. y_ = ${y_(t)}")
        yp
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size hh, h = 1 to hh-steps ahead forecasts for the model,
     *  i.e., forecast the following time points:  t+1, ..., t+h.
     *  Intended to work with rolling validation (analog of predict method).
     *  @param t   the time point from which to make forecasts
     *  @param y_  the actual values to use in making predictions
     */
    override def forecast (t: Int, y_ : VectorD): VectorD =
//      val pred = reg.predict (x(min (t+1, x.dim-1)))               // FIX - why t+1
        val pred = predict (t, MatrixD (y_).transpose)
        for h <- 1 to hh do yf(t-1, h) = pred(h-1)                   // FIX - why t-1
        pred                                                         // yh is pred
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points all horizons h (h-steps ahead).
     *  Assign into FORECAST MATRIX and return the forecast matrix.
     *  @param y_  the matrix of actual response values
     */
    override def forecastAll (y_ : MatrixD): MatrixD =
        for t <- y_.indices do
            val pred = predict (t, y_)
            for h <- 1 to hh do yf(t, h) = pred(h-1)
//          for h <- 1 to hh do yf(max0 (t-1), h) = pred(h-1)        // FIX - why -1
        yf
    end forecastAll

end ARX_D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_D` companion object provides factory methods for the `ARX_D` class.
 */
object ARX_D:

    private val debug = debugf ("ARX_D", true)                          // debug function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_D` object by building an input matrix x and then calling the constructor.
     *  @param xe      the matrix of exogenous variable values
     *  @param y       the response vector (time series data)
     *  @param hh      the maximum forecasting horizon (h = 1 to hh)
     *  @param fname   the feature/variable names
     *  @param tRng    the time range, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters (defaults for `MakeMatrix4TS.hp`)
     */
    def apply (xe: MatrixD, y: VectorD, hh: Int, fname: Array [String] = null,
               tRng: Range = null, hparam: HyperParameter = hp): ARX_D =
        val p     = hparam("p").toInt                                   // use the last p endogenous values
        val q     = hparam("q").toInt                                   // use the last q exogenous values
        val spec  = hparam("spec").toInt                                // 0 - none, 1 - constant, 2 - linear, 3 -quadratic, 4 - sin, 5 = cos
        val lwave = hparam("lwave").toDouble                            // wavelength (distance between peaks)
        val (x, yy) = buildMatrix4TS (xe, y, p, q, spec, lwave, hh)
        debug ("apply", s"x.dims = ${x.dims}, yy.dims = ${yy.dims}")
        new ARX_D (x, yy, hh, xe.dim2, fname, tRng, hparam)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a response vector (time series) y, build and return an input/predictor MATRIX x
     *  and a forecasting target MATRIX yy for today and all future horizons up to hh.
     *  @param y     the given output/response vector
     *  @param p     the maximum lag included (inclusive)
     *  @param hh    the maximum forecasting horizon (h = 1, 2, ... hh)
     *  @param spec  the specification for adding columns (0 => none, 1 => constant 2 => linear)
     *
    def buildMatrix4TS (y: VectorD, p: Int, hh: Int, spec: Int = 1): (MatrixD, MatrixD) =
        val yb = WeightedMovingAverage.backcast (y) +: y                // y prepended with one backcast
        val m  = yb.dim
        val x  = new MatrixD (m, spec + p)                              // columns for spec + each lag
        val yy = new MatrixD (m, hh)                                    // yy = [ y_h ] for h = 1 to hh
        if spec >= 1 then x(?, 0) = VectorD.one (m)                     // intercept/constant term 
        if spec == 2 then x(?, 1) = VectorD.range (0, m)                // time trend

        for t <- x.indices do
            for j <- 1 to p do      x(t, spec + p - j) = yb(max0 (t + 1 - j))          // x  -> lags
            for h <- yy.indices2 do yy(t, h) = if t+h+1 >= m then -0.0 else yb(t+h+1)  // yy -> actual and horizons

        println (s"buildMatrix4TS: x.dims = ${x.dims}, yy.dims = ${yy.dims}")
//      println (x ++^ yy)
        (x, yy)
    end buildMatrix4TS
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the input matrix by combining the p + spec columns for the trend and
     *  endogenous variable with the q * xe.dim2 columns for the exogenous variables.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the response vector (time series data)
     *  @param p        the number of lags for the endogenous variable (lags 1 to p)
     *  @param q        the number of lags for each exogenous variable (lags 1 to q)
     *  @param spec     the number of trend terms (added columns)
     *                      0 - none, 1 - constant 2 - linear, 3 - quadratic, 4 - sine, 5 - cosine
     *  @param lwave    the wavelength (distance between peaks)
     *  @param hh       the maximum forecasting horizon (h = 1, 2, ... hh)
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def buildMatrix4TS (xe: MatrixD, y: VectorD, p: Int, q: Int, spec: Int, lwave: Double,
                        hh: Int, bakcast: Boolean = false): (MatrixD, MatrixD) =
        println (s"xe.dims = ${xe.dims}, y.dim = ${y.dim}, p = $p, q = $q")
        val xx = ARX.buildMatrix4TS (xe, y, p, q, spec, lwave, bakcast)

        val yb = if bakcast then WeightedMovingAverage.backcast (y) +: y   // y prepended with one backcast
                 else y
        val m  = y.dim
        val yy = new MatrixD (m, hh)                                    // yy = [ y_h ] for h = 1 to hh
        for t <- yy.indices do
            for h <- yy.indices2 do
                yy(t, h) = if t+h+1 >= m then -0.0 else yb(t+h+1)       // yy -> actual and horizons
        (xx, yy)
    end buildMatrix4TS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate the quality of point and optionally interval forecast for horizon (h = 1 to hh).
     *  @param mod   the forecasting model to be evaluated
     *  @param yy    the complete shifted per horizon actual time series values
     *  @param hh    the maximum forecasting horizon (h = 1 to hh)
     *  @param ints  whether to evaluate prediction interval forecasts as well as point forecasts
     */
    def evalForecasts (mod: Forecaster, yy: MatrixD, hh: Int, ints: Boolean = false): Unit =
        val ftMat = new MatrixD (hh, Fit.N_QoF)
        banner (s"Evaluate ${mod.modelName}'s QoF for horizons 1 to $hh:")
        val m = yy.dim

        for h <- 1 to hh do
            val yh  = yy(0 until m-h, h-1)                                // h-steps ahead actual values
            val yfh = mod.getYf(0 until m-h, h)                           // h-steps ahead forecast
            val qof = mod.diagnose (yh, yfh)
            ftMat(h-1) = qof
//          println (FitM.fitMap (qof, qoF_names))                        // evaluate h-steps ahead forecasts
            new Plot (null, yh, yfh, s"evalForecast: Plot of yh, yfh for ${mod.modelName} vs. t @h = $h", true)

/*
            if ints then
                val (low, up) = mod.forecastAtI (yy, yfh, h)              // prediction interval forecasts
                val qof_all   = mod.diagnose_ (yy, yfh, low, up)          // fully evaluate h-steps ahead forecasts
                mod.show_interval_forecasts (yy, yfh, low, up, qof_all, h)
*/
        end for

        println ("fitMap     qof = ")
        println (FitM.showFitMap (ftMat.transpose, QoF.values.map (_.toString)))
    end evalForecasts

end ARX_D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_DTest` main function tests the `ARX_D` class on real data:
 *  Forecasting Lake Levels using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_DTest
 *
@main def aRX_DTest (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX_D (y, hh)                                             // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    ARX_D.evalForecasts (mod, mod.getYy, hh)
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

end aRX_DTest
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_DTest2` main function tests the `ARX_D` class on real data:
 *  Forecasting Lake Levels using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_DTest2
 *
@main def aRX_DTest2 (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX_D (y, hh)                                             // create model for time series data
    banner (s"TnT Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.rollValidate ()                                                 // TnT with Rolling Validation
    println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")

end aRX_DTest2
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_DTest3` main function tests the `ARX_D` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_DTest3
 */
@main def aRX_DTest3 (): Unit =

//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val exo_vars  = Array ("icu_patients", "hosp_patients")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    for p <- 1 to 10; s <- 1 to 5 do                                    // number of lags; trend
        hp("p")    = p                                                  // mumber of endo lags
        hp("q")    = p                                                  // mumber of exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX_D (xe, y, hh)                                     // create model for time series data
        banner (s"In-ST Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // train and test on full dataset
//      println (mod.summary ())                                        // statistical summary of fit  FIX - crashes

        mod.forecastAll (mod.getYy)                                     // forecast h-steps ahead (h = 1 to hh) for all y
        mod.diagnoseAll (y, mod.getYf)                                  // FIX - diagnoseAll and evalForecasts should agree
        ARX_D.evalForecasts (mod, mod.getYy, hh)
        println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")
    end for

end aRX_DTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_DTest4` main function tests the `ARX_D` class on real data:
 *  Forecasting COVID-19 using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_DTest4
 */
@main def aRX_DTest4 (): Unit =

//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val exo_vars  = Array ("icu_patients", "hosp_patients")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    for p <- 1 to 10; s <- 1 to 5 do                                    // number of lags; trend
        hp("p")    = p                                                  // number of endo lags
//      hp("q")    = p                                                  // number of exo lags
        hp("q")    = min (2, p)                                         // try various rules
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX_D (xe, y, hh)                                     // create model for time series data
        banner (s"TnT Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x

        mod.setSkip (0)
        mod.rollValidate ()
//      println (s"After Roll TnT Forecast Matrix yf = ${mod.getYf}")
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))        // only diagnose on the testing set
//      println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")
    end for

end aRX_DTest4


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_DTest5` main function tests the `ARX_D` object's ability to build input
 *  matrices.  Build an input/predictor data matrix for the COVID-19 dataset.
 *  > runMain scalation.modeling.forecasting.aRX_DTest5
 */
@main def aRX_DTest5 (): Unit =

//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val exo_vars  = Array ("icu_patients", "hosp_patients")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe    = xxe                                                      // full
    val xe    = xxe(0 until 116)                                         // clip the flat end
//  val y     = yy                                                       // full
    val y     = yy(0 until 116)                                          // clip the flat end
    val p     = 3                                                        // the number of endo lags
    val q     = 2                                                        // the number of exo lags
    val spec  = 1                                                        // additional terms
    val lwave = 20                                                       // wavelength (distance between peaks)
    val hh    = 2                                                        // maximum forecasting horizon

    println (s"y = $y")

    val (x, y_) = ARX_D.buildMatrix4TS (xe, y, p, q, hh, spec, lwave)

    println (s"y.dim = ${y.dim}, x.dims = ${x.dims}, y_.dims = ${y_.dims}")

end aRX_DTest5

