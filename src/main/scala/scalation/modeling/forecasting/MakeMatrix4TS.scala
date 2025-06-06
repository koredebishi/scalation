
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jan 11 14:15:07 EST 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework:  Build matrices for time series regression models
 *
 *  @see `scalation.modeling.Regression`
 */

package scalation
package modeling
package forecasting

import scala.collection.mutable.ArrayBuffer
import scala.math.{cos, sin}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MakeMatrix4TS` object provides methods for making/building matrices from
 *  lagged endogenous and exogenous variables.
 */
object MakeMatrix4TS:

    /** Base hyper-parameter specification for regression based time series models.
     */
    val hp = new HyperParameter
    hp += ("p",  1, 1)                              // number of lags for the endogenous variable
    hp += ("sp", 7, 7)                              // the seasonal period (time units between repetitive behavior)
    hp += ("ps", 2, 2)                              // number of seasonal lags for the endogenous variable
    hp += ("pp", 2.0, 2.0)                          // the power (defaults to quadratic) to raise the lags of the endogenous variable to
    hp += ("pr", 0.5, 0.5)                          // the root (defaults to sqrt) to take of the lags of the endogenous variable
    hp += ("q",  1, 1)                              // number of lags for the exogenous variables
    hp += ("qp", 2.0, 2.0)                          // the power (defaults to quadratic) to raise the lags of the exogenous variables to
    hp += ("qr", 0.5, 0.5)                          // the root (defaults to sqrt) to take of the lags of the exogenous variables
    hp += ("spec", 1, 1)                            // trend terms: 0 - none, 1 - constant, 2 - linear, 3 - quadratic,
                                                    //              4 - sine, 5 - cosine
    hp += ("lwave", 7, 7)                           // wavelength for sine/cosine (distance between peaks)
    hp += ("cross", 0, 0)                           // 0 - no ENDO-EXO cross terms, 1 - include ENDO-EXO cross terms

    hp += ("nneg", 1, 1)                            // 0 - unrestricted, 1 - predictions must be non-negative
    hp += ("factorization", "Fac_QR", "Fac_QR")     // type of matrix factorization
    hp += ("lambda", 0.1, 0.1)                      // shrinkage/regularization parameter

    /*-------------------------------------------------------------------------------
    Usage:
    ARY:       p, spec, lwave                          lags of endogenous variable
    ARY_Quad:  p, pp, spec, lwave                      lags of endogenous variable with power/quadratic terms
    ARX:       p, q, spec, lwave                       lags of endogenous variable and exogenous variable
    ARX_Quad:  p, pp, q, spec, lwave                   lags of endogenous variable with power/quadratic terms and exogenous variable
    ARX_Symb:  p, pp, pr, q, qp, qr, spec, lwave       supports powers and roots for all variables
    for DIRECT forecasting use ARY_D, ARY_Quad_D, ARX_D, ARX_Quad_D, ARX_Symb_D
    NOTE: Last three can be used any any such model
    -------------------------------------------------------------------------------*/

    private val flaw  = flawf ("MakeMatrix4TS")                         // flaw function
    val trend         = Array ("const", "lin", "quad", "sin", "cos")    // types of trends

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form an array of names for the features included in the model.  Handles all
     *  `*ARY*` models.  The `*ARX*` models require custom `formNames` methods.
     *  @param spec   the number of trend terms
     *  @param p      the number of lags for the endogenous variable (lags 1 to p)
     *  @param pwr    whether to raise the lagged endogenous values to a power (defaults to false)
     *  @param sp     the seasonal period (time units until repetitive behavior)
     *  @param start  the first seasonal lag to use (not subsumed by regular lags)
     *  @param ps     the number of seasonal lags for the endogenous variable (lags 1 to ps)
     */
    def formNames (spec: Int, p: Int, pwr: Boolean = false,
                   sp: Int = -1, start: Int = 1, ps: Int = 0): Array [String] =
        val names = ArrayBuffer [String] ()
        for j <- 0 until spec do names += s"${trend(j)}"                // trend terms
        for j <- ps to start by -1 do names += s"yl${j*sp}"             // seasonal lags terms
        for j <- p to 1 by -1 do names += s"yl$j"                       // regular lags terms
        if pwr then for j <- p to 1 by -1 do names += s"yl$j^"          // power lags terms
        println (s"formNames: $names")
        names.toArray
    end formNames

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the relevant given values, i.e., the actual time series values, or the
     *  same prepended with one backcasted value.
     *  @see `WeightedMovingAverage`
     *  @param y        the given output/response vector
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    private inline def getYb (y: VectorD, bakcast: Boolean = false): VectorD =
        if bakcast then WeightedMovingAverage.backcast (y) +: y         // y prepended with one backcast
        else y
    end getYb

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a response vector (time series) y, make/build and return an input/predictor
     *  MATRIX x for the TREND terms.
     *  @param y        the given output/response vector
     *  @param spec     the number of trend terms (added columns)
     *                      0 - none, 1 - constant 2 - linear, 3 - quadratic, 4 - sine, 5 - cosine
     *  @param lwave    the wavelength (distance between peaks)
     *  @param bakcast  whether a backcasted value is prepended to the time series
     */
    def makeMatrix4T (y: VectorD, spec: Int, lwave: Double, bakcast: Boolean): MatrixD =
        val m    = y.dim
        val m2   = m / 2.0
        val w    = _2Pi / lwave                                         // 2 Pi over wavelength
        val x    = new MatrixD (m, spec)
        val t_0m = VectorD.range (0, m)                                 // vector 0, 1, ..., m-1
    
        if spec >= 1 then x(?, 0) = VectorD.one (m)                     // intercept/constant term
        if spec >= 2 then x(?, 1) = t_0m / m                            // time trend (linear)
        if spec >= 3 then x(?, 2) = (t_0m-m2)~^2 / m2~^2                // time-squared trend (quadratic)
        if spec >= 4 then
            x(?, 3) = t_0m.map ((t: Double) => sin (t * w))             // sine wave trend
        if spec == 5 then
            x(?, 4) = t_0m.map ((t: Double) => cos (t * w))             // cosine wave trend
        x
    end makeMatrix4T

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a response vector (time series) y, make/build and return an input/predictor
     *  MATRIX x for the linear LAG terms.
     *  @param y        the given output/response vector
     *  @param p        the maximum lag included (inclusive)
     *  @param spec     the number of trend terms (added columns)
     *                      0 - none, 1 - constant 2 - linear, 3 - quadratic, 4 - sine, 5 - cosine
     *  @param lwave    the wavelength (distance between peaks)
     *  @param bakcast  whether a backcasted value is prepended to the time series
     */
    def makeMatrix4L (y: VectorD, p: Int, bakcast: Boolean): MatrixD =
        val x  = new MatrixD (y.dim, p)
        val yb = getYb (y, bakcast)
        for t <- x.indices; j <- 1 to p do
            x(t, p - j) = yb(max0 (t + 1 - j))
        x
    end makeMatrix4L

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a response vector (time series) y, make/build and return an input/predictor
     *  MATRIX x for the SEASONALLY lagged terms.
     *  @param y        the given output/response vector
     *  @param p        the maximum lag included (inclusive)
     *  @param sp       the seasonal period (time units until repetitive behavior)
     *  @param ps       the number of seasonal lags
     *  @param bakcast  whether a backcasted value is prepended to the time series
     */
    def makeMatrix4S (y: VectorD, p: Int, sp: Int, ps: Int, bakcast: Boolean): MatrixD =
        val n = if sp > p then ps else ps - 1                           // number of seasonal lags to use
        if n <= 0 || 2 * sp <= p then
            flaw ("makeMatrix4S", "seasonality subsumed in regular lags, so it's ignored")
            return null
        val x  = new MatrixD (y.dim, n)
        val yb = getYb (y, bakcast)

        val start = if n < ps then 2 else 1 
        for t <- x.indices; j <- start to ps do
            val js = j * sp
            x(t, ps - j) = yb(max0 (t + 1 - js))
        x
    end makeMatrix4S

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a response vector (time series) y, make/build and return an input/predictor
     *  MATRIX x for the POWER (defaults to  quadratic) terms.
     *  @param y        the given output/response vector
     *  @param p        the maximum lag included (inclusive)
     *  @param xp       the power to raise the lags to (defaults to quadratic)
     *  @param bakcast  whether a backcasted value is prepended to the time series
     */
    def makeMatrix4P (y: VectorD, p: Int, xp: Double = 2.0, bakcast: Boolean): MatrixD =
        val x  = new MatrixD (y.dim, p)
        val yb = getYb (y, bakcast)
        for t <- x.indices; j <- 1 to p do
            x(t, p - j) = yb(max0 (t + 1 - j)) ~^ xp                    // raise each lagged value to xp power
        x
    end makeMatrix4P

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make/build a part of the input matrix consisting of the q * xe.dim2 columns
     *  for the exogenous variables.
     *  @param xe       the matrix of exogenous variable values
     *  @param q        the number of lags for each exogenous variable (lags 1 to q)
     *  @param qp       the power to raise the exogensous lags to
     *  @param bakcast  whether a backcasted value is prepended to the time series
     */
    def makeMatrix4EXO (xe: MatrixD, q: Int, qp: Double, bakcast: Boolean): MatrixD =
        var xx: MatrixD = makeMatrix_exo_col (xe(?, 0), q, qp, bakcast)
        for j <- 1 until xe.dim2 do
            xx = xx ++^ makeMatrix_exo_col (xe(?, j), q, qp, bakcast)
        xx
    end makeMatrix4EXO

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the j-th exogenous variable vector xe_j, build and return an input/predictor
     *  MATRIX xx.
     *  The first lag responses can't be predicted due to missing past values.
     *  Therefore the number of rows in xx is reduced to xe.dim - 1.
     *  @param xej      the j-th exogenous variable vector
     *  @param q        the maximum lag included (inclusive) for the exogenous variable (1 to q)
     *  @param qp       the power to raise the lags to
     *  @param bakcast  whether a backcasted value is prepended to the time series
     *  FIX: apply bakcast if true
     */
    private def makeMatrix_exo_col (xej: VectorD, q: Int, qp: Double, bakcast: Boolean): MatrixD =
        if bakcast then
           println ("FIX handle backcast")
        val xe_j = backfill (xej)
        val xx   = new MatrixD (xe_j.dim, q)
        for i <- xx.indices; k <- xx.indices2 do
            xx(i, q - 1 - k) = xe_j(max0(i - 1 - k)) ~^ qp
        xx
    end makeMatrix_exo_col

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backfill the zero prefix of exogenous variable j (xej) by backcasting.  The zero
     *  prefix will be at least of size 1 as 0.0 is initially prepended.
     *  @param xej  the j-th exogenous variable vector
     */
    private def backfill (xej: VectorD): VectorD =
        val xe_j = xej :+ 0.0                                           // prepend with zero
        val ii = xej.indexWhere (_ != 0.0)                              // find the first non-zero value
        println (s"backfill: from index ii = $ii")
        for i <- ii-1 to 0 by -1 do                                     // replace zero prefix with backcasted values
            xe_j(i) = WeightedMovingAverage.backcast (xe_j, i)          // backcast from index i
        xe_j
    end backfill

end MakeMatrix4TS

