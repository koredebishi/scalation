
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell, Hao Peng
 *  @version 2.0
 *  @date    Tue May 2 21:45:58 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Fourier Basis Functions (sin and cos)
 *
 *  @see https://en.wikipedia.org/wiki/Fourier_series
 */

package scalation
package calculus

import scala.Double.NaN
import scala.math.{Pi, cos, sin}

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fourier` class provides Fourier basis functions. Such basis functions
 *  are useful are useful for fitting periodic data in Functional Data Analysis.
 *  @see en.wikipedia.org/wiki/Fourier_series
 *  @param w     the fundamental frequency parameter
 *  @param mMax  the number of sin/cos pairs to be used in the basis function
 */
class Fourier (w: Double = 2.0 * Pi, mMax: Int = 4)
      extends BasisFunction:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of the m-th order j-th basis function at time t.
     *  Or alternatively, obtain the basis function by calling bf(m)(j) only.
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def bf (m: Int = mMax)(j: Int)(t: Double): Double =
        val c = (j+1)/2 * w
        if      j > 2*m    then NaN                           // invalid input, j must be <= 2m
        else if j == 0     then 1.0
        else if j % 2 == 0 then sin (c * t)
        else cos (c * t)
    end bf

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The number of usable Fourier basis functions for a specified order,
     *  including one constant term and 'm' sin/cos terms.
     *  @param m  the order of the spline
     */
    def size (m: Int = mMax): Int = 2 * m + 1

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The range for the Fourier basis functions.
     *  @param m  the order of the spline
     */
    def range (m: Int = mMax): Range = 0 until size (m)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieves the order of the this Fourier basis function
     */
    def getOrder: Int = mMax

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** String representation of the Fourier Basis Functions
     */
    override def toString =
        var s = s"Fourier (m = $mMax, w = $w) \n"
        for j <- range () do
            if j == 0          then s += "   1.0 \n"
            else if j % 2 == 0 then s += s" + sin (${(j + 1) / 2} * w * t) \n"
            else s += s" + cos (${(j + 1) / 2} * w * t)"
        end for
        s
    end toString

end Fourier


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `fourierTest` main function is used to test the `Fourier` class.
 *  > runMain scalation.calculus.fourierTest
 */
@main def fourierTest (): Unit =

    import Fac_LU.inverse

    // census data (1790--1990)
    val t = VectorD.range (1790, 1991) * 10.0
    println (t)
    val y = VectorD (  3.9000,
                       5.3000,
                       7.2000,
                       9.6000,
                      12.9000,
                      17.1000,
                      23.1000,
                      31.4000,
                      38.6000,
                      50.2000,
                      62.9000,
                      76.0000,
                      92.0000,
                     105.7000,
                     122.8000,
                     131.7000,
                     150.7000,
                     179.0000,
                     205.0000,
                     226.5000,
                     248.7000)

    new Plot (t, y)

    val m    = 8                                                // 2k+1 terms in fourier expansion
    val L    = y.max - y.min                                    // period length
    val w    = 2.0 * Pi / L                                     // fundamental frequency estimate
    val four = new Fourier (w)                                  // Fourier Basis Function
    val Φ    = four.abf (m)(t)                                  // design matrix of basis functions
    val I    = MatrixD.eye (t.dim, t.dim)                       // identity matrix 
    val λ    = 1.0E-10                                          // ridge parameter
    val inv  = inverse ((Φ.transpose * Φ) + (I * λ))()
    val c    = inv * Φ.transpose * y                            // model coefficients

    def x (tt: Double) = (0 until 2*m+1).map (j => c(j) * four (m)(j)(tt)).sum

    val z    = t.map (x)                                        // predicted response
    val e    = y - z                                            // residuals
    val sse  = e dot e                                          // sum of squared errors

    println (s"   y = $y")    
    println (s"   Φ = $Φ")
    println (s"four = $four")
    println (s"   c = $c")
    println (s"   z = $z")
    println (s"   e = $e")
    println (s" sse = $sse")

//  new FPlot (t(0) to t(t.dim-1) by 1, Seq(x), lines = true)
//  new FPlot (NumericRange.inclusive (t(0), t(t.dim-1), 1.0), Seq(x), lines = true)   // FIX

//  val yp = new DFourier (w)
//  new Plot (t, y, yp, s"y vs yp", lines = true)

end fourierTest

