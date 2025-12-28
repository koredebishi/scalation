
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jun 30 13:27:00 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive on lagged y and xe (ARX) using OLS
 *
 *  @see `scalation.modeling.Regression`
 *  @see `scalation.modeling.forecasting.ARY` when no exogenous variable are needed
 */

package scalation
package modeling
package forecasting

import scala.collection.mutable.ArrayBuffer
import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._

import MakeMatrix4TS._
import TransformT._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX` class provides basic time series analysis capabilities for ARX models.
 *  ARX models build on `ARY` by including one or more exogenous (xe) variables.
 *  Given time series data stored in vector y, its next value y_t = combination of
 *  last p values of y and the last q values of each exogenous variable xe_j.
 *
 *      y_t = b dot x_t + e_t
 *
 *  where y_t is the value of y at time t and e_t is the residual/error term.
 *  @param x        the data/input matrix (lagged columns of y and xe) @see `ARX.apply`
 *  @param y        the response/output vector (time series data) 
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param n_exo    the number of exogenous variables
 *  @param fname    the feature/variable names
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 *  @param tForms   the map of transformations applied
 */
class ARX (x: MatrixD, y: VectorD, hh: Int, n_exo: Int, fname: Array [String],
           tRng: Range = null, hparam: HyperParameter = hp,
           bakcast: Boolean = false,
           tForms: TransformMap = Map ("tForm_y" -> null))
      extends Forecaster_Reg (x, y, hh, fname, tRng, hparam, bakcast):

    private   val debug = debugf ("ARX", true)                          // debug function
    protected val p     = hparam("p").toInt                             // use the last p endogenous values (p lags)
    protected val q     = hparam("q").toInt                             // use the last q exogenous values (q lags)
    protected val spec  = hparam("spec").toInt                          // trend terms: 0 - none, 1 - constant, 2 - linear, 3 - quadratic
                                                                        //              4 - sine, 5 cosine
    _modelName = s"ARX_${p}_${q}_$n_exo"
    yForm      = tForms("tForm_y").asInstanceOf [Transform]

    debug ("init", s"$modelName with $n_exo exogenous variables and additional term spec = $spec, x.dims = ${x.dims}")
//  debug ("init", s"[ x | y ] = ${x :^+ y}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forge a new vector from the first spec values of x, the last p-h+1 values
     *  of x (past values), values 1 to h-1 from the forecasts, and available values
     *  from exogenous variables.
     *  @param xx  the t-th row of the input matrix (lagged actual values)
     *  @param yy  the t-th row of the forecast matrix (forecasted future values)
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forge (xx: VectorD, yy: VectorD, h: Int): VectorD =
        // add terms for the endogenous variable
        val n_endo  = spec + p                                       // number of trend + endogenous values
        val x_act   = xx(n_endo-(p+1-h) until n_endo)                // get actual lagged y-values (endogenous)
        val nyy     = p - x_act.dim                                  // number of forecasted values needed
        val x_fcast = yy(h-nyy until h)                              // get forecasted y-values

        var xy = x_act ++ x_fcast
        if n_exo > 0 and q > 0 then
            for j <- 0 until n_exo do                                // for the j-th exogenous variable
                xy = xy ++ hide (xx(n_endo + j*q until n_endo + (j+1)*q), h)
        xx(0 until spec) ++ xy
    end forge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Hide values at the end of vector z (last h-1 values) as the increasing horizon  
     *  turns them in future values (hence unavailable).  Set these values to either
     *  zero (the default) or the last available value.
     *  @param z     the vector to shift
     *  @param h     the current horizon (number of steps ahead to forecast)
     *  @param fill  whether to backfill with the rightmost value (true) or with 0 (false)
     */
    def hide (z: VectorD, h: Int, fill: Boolean = true): VectorD =
        val zl = z(z.dim - 1)                                        // last available z value per horizon
        val z_ = new VectorD (z.dim)
        for k <- z.indices do
            z_(k) = if k <= z.dim - h then z(k+h-1) else if fill then zl else 0.0
        z_
    end hide

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the underlying Regression Model to an `ARX` Forecasting Model.
     *  @param mod  the model to convert, e.g., the best model after feature selection
     */
    def convertReg2Forc (mod: Model_FS = getBest.mod): ARX =
        new ARX (mod.getX, mod.getY, 1, n_exo, fname, tRng, hparam, bakcast, tForms)
    end convertReg2Forc

end ARX


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX` companion object provides factory methods for the `ARX` class.
 */
object ARX extends MakeMatrix4TS:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX` object by building an input matrix xy and then calling the
     *  `ARX` constructor.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the response vector (time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param fname_   the feature/variable names
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param fEndo    the array of functions used to transform endogenous variables
     *  @param fExo     the array of functions used to transform exogenous variables
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def apply (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
               tRng: Range = null, hparam: HyperParameter = hp,
               fEndo: Array [Transform] = null, fExo: Array [Transform] = null,
               bakcast: Boolean = false): ARX =

        val (xy, tForms) = buildMatrix (xe, y, hparam, bakcast)
        val fname = formNames (xe.dim2, hparam, fname_)
        new ARX (xy, y, hh, xe.dim2, fname, tRng, hparam, bakcast, tForms)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX` object by building an input matrix xy and then calling the
     *  `ARX` constructor.  Also rescale the input data.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the endogenous/response vector (main time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param fname_   the feature/variable names
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param fEndo    the array of functions used to transform endogenous variables
     *  @param fExo     the array of functions used to transform exogenous variables
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     *  @param tForm    the transform (e.g., rescale to range or standard normal z)
     */
    def rescale (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
                 tRng: Range = null, hparam: HyperParameter = hp,
                 fEndo: Array [Transform] = null, fExo: Array [Transform] = null,
                 bakcast: Boolean = false,
                 tForm: VectorD | MatrixD => Transform = MinMax.form): ARX =

        val (xy, tForms) = buildMatrix (xe, y, hparam, bakcast, tForm)
        if tForms("tForm_y").getClass.getSimpleName == "NormForm" then hp("nneg") = 0
        val y_scl = tForms("tForm_y").f(y)
        val fname = formNames (xe.dim2, hparam, fname_)
        new ARX (xy, y_scl, hh, xe.dim2, fname, tRng, hparam, bakcast, tForms)
    end rescale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the input matrix by combining the p + spec columns for the trend and
     *  endogenous variable with the q * xe.dim2 columns for the exogenous variables.
     *  @param xe       the matrix of exogenous variable values
     *  @param y_org    the original response vector (time series data)
     *  @param hp_      the hyper-parameters
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def buildMatrix (xe: MatrixD, y_org: VectorD, hp_ : HyperParameter, bakcast: Boolean,
                     tFormScale: VectorD | MatrixD => Transform = null): (MatrixD, TransformMap) =

        val (p, q, spec, lwave) = (hp_("p").toInt, hp_("q").toInt, hp_("spec").toInt, hp_("lwave").toDouble)
        val tr_size = Model.trSize (y_org.dim)

        // rescale y
        var y = y_org
        val tForms: TransformMap =
            if tFormScale != null then
                val tForm_y = tFormScale (y_org(0 until tr_size))    // use (mean, std) of training set for both In-sample and TnT
                y = tForm_y.f(y_org)
                Map("tForm_y" -> tForm_y)
            else
                Map("tForm_y" -> null)

        var xy = makeMatrix4T (y_org, spec, lwave, bakcast) ++^      // trend terms
                 makeMatrix4L (y, p, bakcast)                        // regular lag terms

        if xe.dim2 > 0 and q > 0 then
            var xe_bfill = backfill (xe)
            if tFormScale != null then
                val tForm_exo = tFormScale (xe_bfill(0 until tr_size))
                xe_bfill = tForm_exo.f(xe_bfill)
            xy = xy ++^ makeMatrix4L (xe_bfill, q, bakcast)

        (xy, tForms)
    end buildMatrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form an array of names for the features included in the model.
     *  @param n_exo    the number of exogenous variable
     *  @param hp_      the hyper-parameters
     *  @param xe_name  the names of the exogenous variables (defaults to null)
     *  @param n_fEn    the number of functions used to map endogenous variables (none for `ARX`)
     *  @param n_fEx    the number of functions used to map exogenous variables (none for `ARX`)
     */
    def formNames (n_exo: Int, hp_ : HyperParameter, xe_name: Array [String] = null,
                   n_fEn: Int = 0, n_fEx: Int = 0): Array [String] =
        val (p, q, spec) = (hp_("p").toInt, hp_("q").toInt, hp_("spec").toInt)
        val names = ArrayBuffer [String] ()
        if xe_name != null then
            for j <- 0 until n_exo; k <- q to 1 by -1 do names += s"${xe_name(j)}l$k"
        else
            for j <- 0 until n_exo; k <- q to 1 by -1 do names += s"xe${j}l$k"
        MakeMatrix4TS.formNames (spec, p) ++ names.toArray
    end formNames

end ARX

import Example_Covid._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest` main function tests the `ARX` class on real data:
 *  Forecasting Lake Levels using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRXTest
 *
@main def aRXTest (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX (y, hh)                                               // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    mod.diagnoseAll (mod.getY, mod.getYf)
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

end aRXTest
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest2` main function tests the `ARX` class on real data:
 *  Forecasting Lake Levels using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRXTest2
 *
@main def aRXTest2 (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX (y, hh)                                               // create model for time series data
    banner (s"TnT Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.rollValidate ()                                                 // TnT with Rolling Validation
    println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")

end aRXTest2
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest3` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRXTest3
 */
@main def aRXTest3 (): Unit =

//  val exo_vars  = NO_EXO
    val exo_vars  = Array ("icu_patients")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    new Plot (null, y, null, s"y (new_deaths) vs. t", lines = true)
    for j <- exo_vars.indices do
        new Plot (null, xe(?, j), null, s"x_$j (${exo_vars(j)}) vs. t", lines = true)

    for p <- 6 to 6; q <- 4 to 4; s <- 1 to 1 do                        // number of endo lags; exo lags; trend
        hp("p")    = p                                                  // number of endo lags  
        hp("q")    = q                                                  // number of exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX (xe, y, hh)                                       // create model for time series data
        mod.inSample_Test ()                                            // In-sample Testing
        println (mod.summary ())                                        // statistical summary of fit
    end for

    banner ("Test NormForm Transform")
    val yform  = NormForm (y)
    val y_     = yform.f (y)
    val xeform = NormForm (xe(?, 0))
    val xe_    = xeform.f (xe(?, 0))
    new Plot (null, y_, xe_, s"y (new_deaths) vs. t", lines = true)

end aRXTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest4` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRXTest4
 */
@main def aRXTest4 (): Unit =

    val exo_vars  = Array ("icu_patients", "positive_rate")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    for p <- 6 to 6; s <- 1 to 1; q <- 4 to 4 do                        // number of lags; trend
        hp("p")    = p                                                  // number of endo lags
        hp("q")    = q                                                  // number of exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
//      val mod = ARX (xe, y, hh)                                       // create model for time series data
        val mod = ARX.rescale (xe, y, hh)
        banner (s"TnT Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x

        mod.setSkip (0)
//      mod.rollValidate (rc = 200)                                     // TnT with Rolling Validation
        mod.rollValidate ()                                             // TnT with Rolling Validation default rc = 2
        mod.diagnoseAll (mod.getY, mod.getYf, Forecaster.teRng (y.dim))   // only diagnose on the testing set
//      println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")
    end for

end aRXTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest5` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  This version performs feature selection.
 *  > runMain scalation.modeling.forecasting.aRXTest5
 */
@main def aRXTest5 (): Unit =

    val exo_vars  = Array ("icu_patients", "positive_rate")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("p")     = 6                                                     // endo lags
    hp("q")     = 4                                                     // exo lags
    hp("spec")  = 1                                                     // trend specification: 0, 1, 2, 3, 5
    hp("lwave") = 20                                                    // wavelength (distance between peaks)
    RidgeRegression.hp("lambda") = 6.0

    val mod = ARX (xe, y, hh)                                           // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on COVID-19 Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset
    println (mod.summary ())                                            // statistical summary of fit

    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    mod.diagnoseAll (mod.getY, mod.getYf)
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

    for tech <- SelectionTech.values do
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech, "none")             // R^2, R^2 bar, sMAPE, R^2 cv
        val k = cols.size
        println (s"k = $k")

        val modBest = mod.getBest.mod                                   // regress on this x
        println (stringOf (mod.getFname))
        println (stringOf (modBest.getFname))

        new PlotM (null, rSq.ᵀ, Regression.metrics, s"R^2 vs k for ${mod.modelName} with $tech", lines = true)
        banner (s"Feature Importance with $tech")
        println (s"$tech: rSq = $rSq")
        val imp = mod.importance (cols.toArray, rSq)
        println (s"feature importance imp = $imp")
//      for (c, r) <- imp do println (s"col = $c, \t ${ox_fname(c)}, \t importance = $r")
    end for

end aRXTest5

