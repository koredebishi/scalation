
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Praveen Rangavajhula
 *  @version 2.0
 *  @date    Fri April 25 19:47:13 EDT 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Autograd: Variabl Class for Automatic Differentiation
 *  @see     `Variable` in `scalation.modeling`
 */

package scalation
package modeling
package autograd

import scalation.mathstat.TensorD

import scala.annotation.targetName

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Variabl` case class represents a tensor with automatic differentiation capability.
 *  It tracks operations applied to it for backward gradient propagation.
 *  Variabls can be combined using arithmetic operations, activation functions,
 *  and loss functions. Backpropagation is triggered via the `backward` method.
 *  @param data    the tensor data for this variable.
 *  @param gradFn  an optional function for backpropagation.
 *  @param name    an optional name for this variable.
 *  @param ops     the implicit autograd operations for tensor computations.
 */
case class Variabl (var data: TensorD, gradFn: Option [Function] = None, name: Option [String] = None)
                    (using ops: AutogradOps):

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a string representation of the variable.
     *  If a name is defined, it is included in the output.
     *  @return a string containing the name (if available) and data.
     */
    override def toString: String =
        if name.isDefined then s"name: ${name.get}, data: $data"
        else s"data: $data"
    end toString

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The gradient tensor associated with this variable.
     *  Initially set to a tensor of zeros with the same shape as data.
     */
    var grad: TensorD = ops.zerosLike (data)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs backpropagation with a default gradient of ones.
     */
    inline def backward (): Unit = backward (ops.onesLike (data))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs backpropagation using the specified output gradient.
     *  @param gradOutput  the gradient tensor to propagate.
     */
    inline def backward (gradOutput: TensorD): Unit =
        grad += gradOutput
        gradFn.foreach (fn => fn.backward (gradOutput))
    end backward

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Detaches the variable from the computation graph, returning a new variable with the same data.
     *  @param name  an optional new name for the detached variable.
     *  @return a new variable with identical data but no gradient function.
     */
    def detach (name: Option [String] = None): Variabl = Variabl (data, name = name)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the shape of the tensor data as a list of dimensions.
     *  @return a List [Int] representing the dimensions of the data.
     */
    inline def shape: List [Int] = ops.shape (data)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a new variable with data filled with zeros and the same shape as this variable.
     *  @return a Variabl with zeros.
     */
    inline def zerosLike (): Variabl = Variabl (ops.zerosLike (data))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a new variable with data filled with ones and the same shape as this variable.
     *  @return a Variabl with ones.
     */
    inline def onesLike (): Variabl = Variabl (ops.onesLike (data))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a new variable with data filled with the specified value and the same shape as this variable.
     *  @param value  the value to fill the new variable with.
     *  @return a Variabl with the specified value.
     */
    inline def fullLike(value: Double): Variabl = Variabl (ops.fullLike (data, value))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Computes the sum of elements along the specified axis and returns the result as a new variable.
     *  @param axis  the axis along which to compute the sum.
     *  @return a Variabl representing the sum along the axis.
     */
    inline def sumAlongAxis (axis: Int): Variabl = Variabl (ops.sumAlongAxis(data, axis))

    // ------------------- ARITHMETIC OPS -------------------

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adds this variable with another variable.
     *  @param other  the variable to add.
     *  @return a new Variabl representing the element-wise addition.
     */
    @targetName ("Autograd_Add")
    inline def + (other: Variabl): Variabl = Add (this, other).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adds a constant to this variable.
     *  @param s  the constant to add.
     *  @return a new Variabl representing the result.
     */
    @targetName ("Autograd_AddConstant")
    inline def + (s: Double): Variabl = AddConstant (this, s).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtracts another variable from this variable.
     *  @param other  the variable to subtract.
     *  @return a new Variabl representing the element-wise subtraction.
     */
    @targetName ("Autograd_Sub")
    inline def - (other: Variabl): Variabl = Sub (this, other).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtracts a constant from this variable.
     *  @param s  the constant to subtract.
     *  @return a new Variabl representing the result.
     */
    @targetName("Autograd_SubConstant")
    inline def - (s: Double): Variabl = SubConstant (this, s).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiplies this variable with another variable element-wise.
     *  @param other  the variable to multiply.
     *  @return a new Variabl representing the multiplication.
     */
    @targetName ("Autograd_Mul")
    inline def * (other: Variabl): Variabl = Mul (this, other).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiplies this variable by a constant.
     *  @param s  the constant multiplier.
     *  @return a new Variabl representing the scaled variable.
     */
    @targetName("Autograd_MulConstant")
    inline def * (s: Double): Variabl = MulConstant (this, s).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divides this variable by another variable element-wise.
     *  @param other  the variable divisor.
     *  @return a new Variabl representing the division.
     */
    @targetName ("Autograd_Div")
    inline def / (other: Variabl): Variabl = Div (this, other).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divides this variable by a constant.
     *  @param s  the constant divisor.
     *  @return a new Variabl representing the scaled division.
     */
    @targetName ("Autograd_DivConstant")
    inline def / (s: Double): Variabl = DivConstant (this, s).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raises this variable to the power of the given exponent.
     *  @param s  the exponent to raise this variable to.
     *  @return a new Variabl representing the power operation.
     */
    @targetName ("Autograd_Pow")
    inline def ~^ (s: Int): Variabl = Pow (this, s).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a new variable representing the square root of this variable.
     *  @return a Variabl after applying square root.
     */
    @targetName ("Autograd_Sqrt")
    inline def sqrt: Variabl = Sqrt (this).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a new variable representing the natural logarithm of this variable.
     *  @return a Variabl after applying logarithm.
     */
    @targetName ("Autograd_Log")
    inline def log: Variabl = Log (this).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a new variable representing the exponential of this variable.
     *  @return a Variabl after applying exponential.
     */
    @targetName ("Autograd_Exp")
    inline def exp: Variabl = Exp (this).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a new variable representing the absolute value of this variable.
     *  @return a Variabl after applying absolute.
     */
    @targetName ("Autograd_Abs")
    inline def abs: Variabl = Abs (this).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a new variable representing the negation of this variable.
     *  @return a Variabl after applying negation.
     */
    @targetName("Autograd_Neg")
    inline def unary_- : Variabl = Neg (this).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a new variable representing the reciprocal of this variable.
     *  @return a Variabl after applying reciprocal.
     */
    @targetName("Autograd_Reciprocal")
    inline def reciprocal: Variabl = Reciprocal (this).forward ()

    // ------------------- TENSOR OPERATIONS -------------------

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Computes the dot product of this variable with another variable.
     *  @param other  the variable to perform the dot product with.
     *  @return a Variabl representing the dot product.
     */
    @targetName ("Autograd_Dot")
    inline def dot (other: Variabl): Variabl = Dot (this, other).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs matrix multiplication of this variable with another variable.
     *  @param other  the variable to multiply matrices with.
     *  @return a Variabl representing the matrix multiplication result.
     */
    @targetName ("Autograd_Matmul")
    inline def matmul (other: Variabl): Variabl = MatMul (this, other).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs batched matrix multiplication of this variable with another variable.
     *  @param other  the variable to multiply in batches.
     *  @return a Variabl representing the batch matrix multiplication.
     */
    @targetName ("Autograd_BatchMatmul")
    inline def bmm (other: Variabl): Variabl = BatchMatMul(this, other).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a new variable representing the mean of the elements in this variable.
     *  @return a Variabl after computing the mean.
     */
    @targetName ("Autograd_Mean")
    inline def mean: Variabl = Mean (this).forward ()

    // ------------------- ACTIVATION FUNCTIONS -------------------

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Applies the ReLU activation function to this variable.
     *  @return a Variabl after applying ReLU.
     */
    @targetName ("Autograd_ReLU")
    inline def relu: Variabl = ReLU (this).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Applies the LeakyReLU activation function to this variable.
     *  @param alpha  the slope for negative inputs (default is 0.2).
     *  @return a Variabl after applying LeakyReLU.
     */
    @targetName ("Autograd_LeakyReLU")
    inline def leakyReLU (alpha: Double = 0.2): Variabl = LeakyReLU (this, alpha).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Applies the ELU activation function to this variable.
     *  @param alpha  the ELU scaling parameter (default is 1.0).
     *  @return a Variabl after applying ELU.
     */
    @targetName ("Autograd_ELU")
    inline def elu (alpha: Double = 1.0): Variabl = ELU (this, alpha).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Applies the tanh activation function to this variable.
     *  @return a Variabl after applying tanh.
     */
    @targetName ("Autograd_Tanh")
    inline def tanh: Variabl = Tanh (this).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Applies the sigmoid activation function to this variable.
     *  @return a Variabl after applying sigmoid.
     */
    @targetName ("Autograd_Sigmoid")
    inline def sigmoid: Variabl = Sigmoid (this).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Applies the GeLU activation function to this variable.
     * @return a Variabl after applying GeLU.
     */
    @targetName ("Autograd_GeLU")
    inline def gelu: Variabl = GeLU (this).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Applies the softmax activation function to this variable.
     *  @return a Variabl after applying softmax.
     */
    @targetName ("Autograd_Softmax")
    inline def softmax: Variabl = Softmax (this).forward ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Chains the provided function with this variable.
     *  @param f  a function that takes a Variabl and returns a Variabl.
     *  @return the result of applying the function to this variable.
     */
    @targetName ("Autograd_Chain")
    inline def ~> (f: Variabl => Variabl): Variabl = f (this)

end Variabl


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Top-level helper functions and implicit conversions for autograd.
 *  Includes shortcut activation functions (e.g., relu, sigmoid) and
 *  implicit conversions to treat modules as functions for cleaner composition.
 */

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the Rectified Linear Unit (ReLU) activation for the input variable.
 *  @param v  the input variable.
 *  @return a new variable after applying ReLU.
 */
def relu (v: Variabl): Variabl = v.relu

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the Leaky ReLU activation for the input variable.
 *  @param v      the input variable.
 *  @param alpha  the slope for negative inputs, default is 0.01.
 *  @return a new variable after applying Leaky ReLU.
 */
def leakyReLU (v: Variabl, alpha: Double = 0.01): Variabl = v.leakyReLU (alpha)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the hyperbolic tangent (tanh) activation for the input variable.
 *  @param v  the input variable.
 *  @return a new variable after applying tanh.
 */
def tanh (v: Variabl): Variabl = v.tanh

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the Sigmoid activation for the input variable.
 *  @param v  the input variable.
 *  @return a new variable after applying sigmoid.
 */
def sigmoid (v: Variabl): Variabl = v.sigmoid

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the exponential (exp) of the input variable.
 *  @param v  the input variable.
 *  @return a new variable after applying the exponential function.
 */
def exp (v: Variabl): Variabl = v.exp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the softmax activation for the input variable.
 *  @param v  the input variable.
 *  @return a new variable after applying softmax.
 */
def softmax (v: Variabl): Variabl = v.softmax

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the Gaussian Error Linear Unit (GeLU) activation for the input variable.
 *  @param v  the input variable.
 *  @return a new variable after applying GeLU.
 */
def gelu (v: Variabl): Variabl = v.gelu

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the Exponential Linear Unit (ELU) activation for the input variable.
 *  @param  v the input variable.
 *  @param  alpha the ELU scaling parameter, default is 1.0.
 *  @return a new variable after applying ELU.
 */
def elu (v: Variabl, alpha: Double = 1.0): Variabl = v.elu(alpha)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the Mean Squared Error (MSE) loss between two variables.
 *  @param x  the predictions variable.
 *  @param y  the target variable.
 *  @return a variable representing the computed MSE loss.
 */
def mseLoss (x: Variabl, y: Variabl): Variabl = MSELoss (x, y).forward ()

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Provides an implicit conversion from a Module to a function that maps a Variabl to a Variabl.
 *  This allows using a Module directly as a function.
 */
given Conversion [Module, Variabl => Variabl] with
    def apply (m: Module): Variabl => Variabl = m.apply

