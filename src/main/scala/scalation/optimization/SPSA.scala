//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author Yulong, Casey, John A. Miller
  * @version 2.0
  * @date Thursday Feb 17 13:32:52 EDT 2022
  * @see LICENSE (MIT style license file).
  * @title Simultaneous perturbation stochastic approximation
  * link https://www.jhuapl.edu/spsa/PDF-SPSA/Matlab-SPSA_Alg.pdf
  */
package scalation
package optimization

import scalation.mathstat.*
import scalation.random.{Bernoulli, Normal, Uniform}

import javax.naming.OperationNotSupportedException
import scala.collection.mutable.ArrayBuffer
import scala.math.pow
import scala.util.control.Breaks.{break, breakable}



//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SPSA` stands for simultaneous perturbation stochastic approximation (SPSA) method
 *  it is considered an efficient algorithm to approximate for high-dimension problems
 *
 *  minimize    f(x)
 *
 *  @param f  the vector-to-scalar objective function
 *  @param debug_  The boolean parameter to enable the debug printing out information
 */
class SPSA (f: FunctionV2S, val debug_ : Boolean = false)
  extends Minimizer (50)
     with MonitorEpochs:

  val flaw = flawf("SPSA")
  val debug = debugf("SPSA", debug_)

  println ("SPSA is running")
  private val EPS       = 0.000001
  //private val TOL       = 0.001
  private var alpha     = -0.0
  private var gamma     = -0.0
  private var A         = -0.0
  private var a         = -0.0 //these numbers are from Spall (1998) DOI: 10.1109/7.705889
  private var c         = -0.0
  private val bernoully = Bernoulli(0.5,5)
  private var fcount    = 0
  private var diff      = 0.0
  private val epochs    = new ArrayBuffer [Double] ()

  //    var x_best: VectorD = null                    //best theta or parameter to get the lowest error from loss function
  var f_best = Double.MaxValue
  //    var x = x0.copy //copy by value

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  /** Initialize the parameters
   *
   * @param params   the given starting parameters of a VectorD
   */
  def init(params:VectorD = VectorD(0.602, 0.101, 10.0, 0.16, 1.0)):SPSA =
  //def init(params: Array[Double] = Array(0.602, 0.101, 10.0, 0.16, 1.0)):Unit =
    if params.length != 5 then flaw("init","initialization failed! did not pass 5 parameters")
    alpha  = params(0)
    gamma  = params(1)
    A      = params(2)
    a      = params(3)
    c      = params(4)
    this
  end init

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  /** Inherit method implementation
   *
   * @param no use
   */
  def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
    throw new OperationNotSupportedException("lineSearch is not needed for GA")
  end lineSearch


  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  /** The main solve method
   *
   * @param x0 initial points
   * @param step steps for iteration
   * @param toler tolerance
   */
  def solve (x0: VectorD, step: Double = STEP, toler: Double = EPS): FuncVec =

    var x_old  = x0.copy
    var x_best = x0.copy
    var x = x0.copy                 //copy by value
    breakable {
      for k <- 1 to MAX_ITER do
          println ("Step " + k + ", current x = " + x + ", current y = " + f_best)
          val ak = a / pow(A + k + 1, alpha)
          val ck = c /pow(k + 1,gamma)
          val delta = getDelta (x0.dim)
          val thetaplus = x + delta * ck
          val thetaminus = x - delta * ck
          val yplus = f(thetaplus)
          val yminus = f(thetaminus)
          val dy = yplus - yminus
          val ghat = delta * dy / (2 * ck)

          x_old = x.copy
          val akghat = ghat * ak
          x -= akghat
          val xn = (x - x_old).norm
          val f_x = f(x)
          if f_x < f_best then
            fcount = 0
            diff = f_best - f_x
            x_best = x.copy         //copy by value
            f_best = f_x
          else
            fcount += 1
          end if
          epochs += f_best
          if xn < toler then break()
      end for
    }//breakable
    debug("solve", s"x_last is $x and y(x_last) at the end  is ${f(x)} and \n " + s"lowest is $x_best and $f_best")
    (f_best,x_best)
  end solve


  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  /** gen the delta
   *
   * @param n    size
   */
  def getDelta (n: Int): VectorD =
    val c = new VectorD (n)
    for (i <- c.indices) c(i) = 2 * bernoully.igen - 1
    c
  end getDelta


  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** gen the Y
   */
  def getY (): Double = f_best

end SPSA


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `nelderMeadSimplexTest` main function is used to test the `NelderMeadSimplex` class.
 *  > runMain scalation.optimization.nelderMeadSimplTest
 */
@main def SPSATest (): Unit =

  // banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

  val noise = Uniform (-0.1, 0.1)
  def f (x: VectorD): Double = (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0 + noise.gen
  val n= VectorD(1,2)
  val optimizer = new SPSA(f)
  optimizer.init()
  val opt = optimizer.solve(n)
  println (s"optimal solution (f(x), x) = $opt")


end SPSATest

