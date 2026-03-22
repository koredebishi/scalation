
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Praveen Rangavajhula
 *  @version 2.0
 *  @date    Fri April 25 19:46:13 EDT 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Autograd: Base Trait and Operations for Differentiable Functions
 */

package scalation
package modeling
package autograd

import scalation.mathstat.TensorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Function` base trait for all differentiable operations in the autograd system.
 *  A Function encapsulates both the forward computation (producing outputs)
 *  and the backward computation (propagating gradients).
 *  It also provides utility methods for handling unbroadcasting of shapes
 *  during the backward pass, ensuring correct gradient flow.
 *  Every custom operation should extend this trait and implement `forward` and `backward`.
 */
trait Function (using ops: AutogradOps):

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs the forward pass to compute the output variable.
     *  @return a Variabl containing the output data and gradient function.
     */
    def forward (): Variabl

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs the backward pass given the upstream gradient.
     *  @param gradOutput  the gradient tensor from the next layer.
     */
    def backward (gradOutput: TensorD): Unit

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Unbroadcasts a variable's tensor data to a specified old shape.
     *  @param v         the variable to unbroadcast.
     *  @param oldShape  the target shape.
     *  @return a new Variabl with data unbroadcasted.
     */
    def unbroadcast (v: Variabl, oldShape: List [Int]): Variabl =
        Variabl (unbroadcast (data = v.data, oldShape = oldShape))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Unbroadcasts a tensor to a given shape by summing across reduced dimensions.
     *  @param data      the tensor data.
     *  @param oldShape  the original shape.
     *  @return a TensorD with shape adjusted to oldShape.
     *  @throws Exception if unbroadcasting is not feasible.
     */
    def unbroadcast (data: TensorD, oldShape: List [Int]): TensorD =
        val currentShape = ops.shape(data)
        var cur = data
        for i <- oldShape.indices do
            val (oldDim, newDim) = (oldShape(i), currentShape(i))
            if oldDim == newDim then {}                          // no change if dimensions match
            else if oldDim == 1 then
                cur = ops.sumAlongAxis (cur, i)                  // reduce dimension i by summing
            else if oldDim != newDim then
                throw new Exception (
                    s"Cannot unbroadcast from shape $currentShape to $oldShape at axis $i")
        cur
    end unbroadcast

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backpropagates gradients for functions with two inputs.
     *  @param v1            the first input variable.
     *  @param v2            the second input variable.
     *  @param gradOutput    the upstream gradient tensor.
     *  @param computeGrad1  function to compute the gradient for v1.
     *  @param computeGrad2  function to compute the gradient for v2.
     */
    def backpropForTwoInputs (v1: Variabl, v2: Variabl, gradOutput: TensorD,
                              computeGrad1: TensorD => TensorD, computeGrad2: TensorD => TensorD): Unit =
        v1.backward (unbroadcast (computeGrad1 (gradOutput), v1.shape))
        v2.backward (unbroadcast (computeGrad2 (gradOutput), v2.shape))
    end backpropForTwoInputs

end Function


// -----------------------------------------------------------------------
// ------------------------- ARITHMETIC FUNCTIONS ------------------------
// -----------------------------------------------------------------------

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the element-wise absolute value of a variable.
 *  @param v  the input variable.
 */
case class Abs (v: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the absolute value of v.
     *  @return a new Variabl containing |v|.
     */
    override def forward (): Variabl = Variabl (ops.abs (v.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: applies the chain rule using the sign of v.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit = v.backward (ops.mul (gradOutput, ops.sign(v.data)))

end Abs


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the negation of a variable.
 *  @param v  the input variable.
 */
case class Neg (v: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the negation of v.
     *  @return a Variabl containing -v.
     */
    override def forward (): Variabl = Variabl (ops.neg (v.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates the negated gradient.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit = v.backward (ops.neg (gradOutput))

end Neg


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the square root of a variable.
 *  @param v  the input variable.
 */
case class Sqrt (v: Variabl)(using ops: AutogradOps) extends Function:

    private var sqrtCache: Option[TensorD] = None

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the square root of v.
     *  @return a Variabl containing sqrt(v).
     */
    override def forward (): Variabl =
        sqrtCache = Some (ops.sqrt (v.data))
        Variabl (sqrtCache.get, gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates gradient using the derivative of sqrt.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward (ops.div (gradOutput, ops.mulScalar (sqrtCache.get, 2.0)))

end Sqrt


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the natural logarithm of a variable.
 *  @param v  the input variable.
 */
case class Log (v: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes log(v).
     *  @return a Variabl containing log(v).
     */
    override def forward (): Variabl = Variabl (ops.log (v.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: applies the derivative 1/v.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward (ops.div (gradOutput, v.data))

end Log


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the reciprocal of a variable.
 *  @param v  the input variable.
 */
case class Reciprocal (v: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the reciprocal of v.
     *  @return a Variabl containing 1/v.
     */
    override def forward (): Variabl = Variabl (ops.reciprocal (v.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: computes the derivative -1/v&#94;2.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward (ops.div (ops.neg (gradOutput), ops.pow(v.data, 2)))

end Reciprocal


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the logarithm of a variable with a specified base.
 *  @param v     the input variable.
 *  @param base  the base for the logarithm.
 */
case class LogBase (v: Variabl, base: Double)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes log base 'base' of v.
     *  @return a Variabl containing log_base(v).
     */
    override def forward (): Variabl = Variabl (ops.logBase (v.data, base), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: adjusts the gradient by dividing by (v * log(base)).
     *  @param gradOutput  the upstream gradient.
     */
    override def backward(gradOutput: TensorD): Unit =
        val denominator = ops.mulScalar (v.data, math.log (base))
        v.backward (ops.div (gradOutput, denominator))

end LogBase


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes element-wise addition of two variables.
 *  @param v1  the first variable.
 *  @param v2  the second variable.
 */
case class Add (v1: Variabl, v2: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes v1 + v2.
     *  @return a Variabl containing the sum.
     */
    override def forward (): Variabl = Variabl (ops.add (v1.data, v2.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates gradient to both inputs (unbroadcast if necessary).
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        val g1 = unbroadcast (gradOutput, v1.shape)
        v1.backward (g1)
        val g2 = unbroadcast (gradOutput, v2.shape)
        v2.backward (g2)

end Add


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Adds a constant value to a variable.
 *  @param v  the input variable.
 *  @param d  the constant to add.
 */
case class AddConstant (v: Variabl, d: Double)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes v + d.
     *  @return a Variabl with the constant added.
     */
    override def forward (): Variabl = Variabl (ops.addScalar (v.data, d), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: simply propagates the upstream gradient.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit = v.backward (gradOutput)

end AddConstant


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes element-wise subtraction of two variables.
 *  @param v1  the minuend.
 *  @param v2  the subtrahend.
 */
case class Sub (v1: Variabl, v2: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes v1 - v2.
     *  @return a Variabl containing the difference.
     */
    override def forward (): Variabl = Variabl (ops.sub (v1.data, v2.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates gradient to v1 normally and to v2 as negative.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        val g1 = unbroadcast (gradOutput, v1.shape)
        v1.backward (g1)
        val g2 = unbroadcast (gradOutput, v2.shape)
        v2.backward (ops.neg (g2))

end Sub


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Subtracts a constant value from a variable.
 *  @param v  the input variable.
 *  @param d  the constant to subtract.
 */
case class SubConstant (v: Variabl, d: Double)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes v - d.
     *  @return a Variabl with the constant subtracted.
     */
    override def forward (): Variabl = Variabl (ops.subScalar (v.data, d), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: simply propagates the upstream gradient.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit = v.backward (gradOutput)

end SubConstant

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes element-wise multiplication of two variables.
 *  @param v1  the first variable.
 *  @param v2  the second variable.
 */
case class Mul (v1: Variabl, v2: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes v1 * v2.
     *  @return a Variabl containing the product.
     */
    override def forward (): Variabl = Variabl (ops.mul (v1.data, v2.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: uses the chain rule to propagate gradients.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        backpropForTwoInputs (v1, v2, gradOutput,
                             (g: TensorD) => ops.mul (g, v2.data),
                             (g: TensorD) => ops.mul (v1.data, g))

end Mul


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Multiplies a variable by a constant.
 *  @param v  the input variable.
 *  @param d  the constant multiplier.
 */
case class MulConstant (v: Variabl, d: Double)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes v * d.
     *  @return a Variabl with data scaled by d.
     */
    override def forward (): Variabl = Variabl (ops.mulScalar (v.data, d), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: multiplies the gradient by d.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        val g = ops.mulScalar (gradOutput, d)
        v.backward (g)

end MulConstant


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes element-wise division of two variables.
 *  @param v1  the dividend.
 *  @param v2  the divisor.
 */
case class Div (v1: Variabl, v2: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes v1 / v2.
     *  @return a Variabl with divided data.
     */
    override def forward (): Variabl = Variabl (ops.div(v1.data, v2.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates gradients with appropriate adjustments.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        backpropForTwoInputs (v1, v2, gradOutput,
                             (g: TensorD) => ops.div (g, v2.data),
                             (g: TensorD) => ops.div (ops.mul (ops.neg (v1.data), g), ops.pow (v2.data, 2)))

end Div


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Divides a variable by a constant.
 *  @param v  the input variable.
 *  @param d  the constant divisor.
 */
case class DivConstant (v: Variabl, d: Double)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes v / d.
     *  @return a Variabl with data divided by d.
     */
    override def forward (): Variabl = Variabl (ops.divScalar (v.data, d), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates gradient scaled by 1/d.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        val g = ops.divScalar (gradOutput, d)
        v.backward (g)

end DivConstant

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Raises a variable to an integer power.
 *  @param v  the input variable.
 *  @param s  the exponent.
 */
case class Pow (v: Variabl, s: Int)(using ops: AutogradOps) extends Function:

    private var powCache: Option [TensorD] = None

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes v raised to the power s.
     *  @return a Variabl with powered data.
     */
    override def forward (): Variabl =
        powCache = Some (ops.pow (v.data, s))
        Variabl (powCache.get, gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: applies the derivative of the power function.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        val factor = ops.div (powCache.get, v.data)
        v.backward(ops.mul (ops.mulScalar (gradOutput, s), factor))

end Pow


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the exponential of a variable.
 *  @param v  the input variable.
 */
case class Exp (v: Variabl)(using ops: AutogradOps) extends Function:

    private var expCache: Option [TensorD] = None

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes exp(v).
     *  @return a Variabl containing the exponential.
     */
    override def forward (): Variabl =
        expCache = Some (ops.exp(v.data))
        Variabl (expCache.get, gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates gradient scaled by the exponential.
     * @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward(ops.mul (gradOutput, expCache.get))
        expCache = None

end Exp


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the mean of all elements in a variable.
 *  @param v  the input variable.
 */
case class Mean (v: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the mean and fills a tensor with it.
     *  @return a Variabl with data filled by the mean value.
     */
    override def forward (): Variabl =
        val out = ops.mean(v.data)
        Variabl (ops.fullLike (v.data, out), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: scales the gradient and fills a tensor accordingly.
     *  @param gradOutput the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        val scale = 1.0 / ops.shape (v.data).product
        val grad = ops.mulScalar (gradOutput, scale)
        v.backward(ops.fullLike (v.data, grad(0)(0)(0)))

end Mean

// -----------------------------------------------------------------------
// ----------------------- ACTIVATION FUNCTIONS --------------------------
// -----------------------------------------------------------------------

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Applies the identity activation function.
 *  @param v  the input variable.
 */
case class Identity (v: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: returns v unchanged.
     *  @return a Variabl with the same data as v.
     */
    override def forward (): Variabl = Variabl (ops.id_ (v.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates the upstream gradient using the identity derivative.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward (ops.mul (gradOutput, ops.idD_ (v.data)))

end Identity


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Applies the ReLU activation function.
 *  @param v the input variable.
 */
case class ReLU (v: Variabl)(using ops: AutogradOps) extends Function:

    private var reluCache: Option [TensorD] = None

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the ReLU activation of v.
     *  @return a Variabl with ReLU applied.
     */
    override def forward (): Variabl =
        reluCache = Some (ops.reLU_(v.data))
        Variabl (reluCache.get, gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates the gradient through ReLU.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward (ops.mul (gradOutput, ops.reLUD_ (reluCache.get)))

end ReLU


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Applies the LeakyReLU activation function.
 *  @param v      the input variable.
 *  @param alpha  the negative slope coefficient.
 */
case class LeakyReLU (v: Variabl, alpha: Double = 0.2)(using ops: AutogradOps) extends Function:

    private var leakyReLUCache: Option [TensorD] = None

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the LeakyReLU activation of v.
     *  @return a Variabl with LeakyReLU applied.
     */
    override def forward (): Variabl =
        leakyReLUCache = Some (ops.lreLU_(v.data, alpha))
        Variabl (leakyReLUCache.get, gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates the gradient using the LeakyReLU derivative.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward (ops.mul (gradOutput, ops.lreLUD_ (leakyReLUCache.get)))

end LeakyReLU


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Applies the ELU activation function.
 *  @param v      the input variable.
 *  @param alpha  the ELU scaling parameter.
 */
case class ELU (v: Variabl, alpha: Double = 1.0)(using ops: AutogradOps) extends Function:

    private var eluCache: Option [TensorD] = None

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the ELU activation of v.
     *  @return a Variabl with ELU applied.
     */
    override def forward (): Variabl =
        eluCache = Some (ops.eLU_ (v.data, alpha))
        Variabl (eluCache.get, gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**
     * Backward pass: propagates the gradient using the ELU derivative.
     *
     * @param gradOutput  the upstream gradient.
     */
    override def backward(gradOutput: TensorD): Unit =
        v.backward(ops.mul(gradOutput, ops.eLUD_(eluCache.get, alpha)))
end ELU


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Applies the tanh activation function.
 *  @param v  the input variable.
 */
case class Tanh (v: Variabl)(using ops: AutogradOps) extends Function:

    private var tanhCache: Option[TensorD] = None

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes tanh(v).
     *  @return a Variabl with tanh applied.
     */
    override def forward (): Variabl =
        tanhCache = Some (ops.tanh_ (v.data))
        Variabl (tanhCache.get, gradFn = Some(this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates the gradient using the tanh derivative.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward (ops.mul (gradOutput, ops.tanhD_ (tanhCache.get)))

end Tanh


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Applies the sigmoid activation function.
 *  @param v  the input variable.
 */
case class Sigmoid (v: Variabl)(using ops: AutogradOps) extends Function:

    private var sigmoidCache: Option [TensorD] = None

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes sigmoid(v).
     *  @return a Variabl with sigmoid applied.
     */
    override def forward (): Variabl =
        sigmoidCache = Some (ops.sigmoid_(v.data))
        Variabl (sigmoidCache.get, gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates the gradient using the sigmoid derivative.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward (ops.mul (gradOutput, ops.sigmoidD_ (sigmoidCache.get)))

end Sigmoid


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Applies the GeLU activation function.
 *  @param v  the input variable.
 */
case class GeLU (v: Variabl)(using ops: AutogradOps) extends Function:

    private var geluCache: Option [TensorD] = None

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes GeLU(v).
     *  @return a Variabl with GeLU applied.
     */
    override def forward (): Variabl =
        geluCache = Some (ops.geLU_ (v.data))
        Variabl (geluCache.get, gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**  Backward pass: uses the GeLU derivative to propagate the gradient.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward (ops.mul (gradOutput, ops.geLUD_ (geluCache.get)))

end GeLU


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Applies the softmax activation function.
 *  @param v  the input variable.
 */
case class Softmax (v: Variabl)(using ops: AutogradOps) extends Function:

    private var softmaxCache: Option [TensorD] = None

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes softmax(v).
     *  @return a Variabl with softmax applied.
     */
    override def forward (): Variabl =
        softmaxCache = Some (ops.softmax_ (v.data))
        Variabl (softmaxCache.get, gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates the gradient using the softmax derivative.
     * @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        v.backward (ops.mul (gradOutput, ops.softmaxD_ (softmaxCache.get)))

end Softmax

// -----------------------------------------------------------------------
// ------------------------- LOSS FUNCTIONS ------------------------------
// -----------------------------------------------------------------------

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the Sum of Squared Errors (SSE) loss.
 *  @param pred    the prediction variable.
 *  @param target  the target variable.
 */
case class SSELoss (pred: Variabl, target: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the SSE loss.
     *  @return a Variabl with loss data.
     */
    override def forward (): Variabl =
        val loss = ops.sseLoss (pred.data, target.data)
        Variabl (ops.fullLike (pred.data, loss), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates the gradient scaled by 2*(pred - target).
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        val grad = ops.mulScalar (ops.sub(pred.data, target.data), 2)
        val gFinal = ops.mul (gradOutput, grad)
        pred.backward (gFinal)

end SSELoss


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the Mean Squared Error (MSE) loss.
 *  @param pred    the prediction variable.
 *  @param target  the target variable.
 */
case class MSELoss (pred: Variabl, target: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the MSE loss.
     *  @return a Variabl with loss data.
     */
    override def forward (): Variabl =
        val loss = ops.mseLoss (pred.data, target.data)
        Variabl (ops.fullLike (pred.data, loss), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: scales the gradient by 2*(pred-target)/batchSize.
     *  @param gradOutput the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        val grad = ops.mulScalar (ops.sub (pred.data, target.data), 2)
        val batchSize = pred.shape.head
        val gProd = ops.mul (gradOutput, grad)
        val gFinal = ops.divScalar (gProd, batchSize)
        pred.backward (gFinal)

end MSELoss


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the Mean Absolute Error (MAE) loss.
 *  @param pred   the prediction variable.
 *  @param target  the target variable.
 */
case class MAELoss (pred: Variabl, target: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes the MAE loss.
     *  @return a Variabl with loss data.
     */
    override def forward (): Variabl =
        val loss = ops.maeLoss (pred.data, target.data)
        Variabl (ops.fullLike (pred.data, loss), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates the gradient using the sign of (pred-target).
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        val grad = ops.sign (ops.sub(pred.data, target.data))
        val prod = ops.mul (gradOutput, grad)
        val gFinal = ops.divScalar (prod, pred.shape.product)
        pred.backward (gFinal)

end MAELoss

// -----------------------------------------------------------------------
// ------------------------- TENSOR OPERATIONS ---------------------------
// -----------------------------------------------------------------------

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the dot product of two variables.
 *  @param v1  the first variable.
 *  @param v2  the second variable.
 */
case class Dot (v1: Variabl, v2: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes dot(v1, v2).
     *  @return a Variabl containing the dot product.
     */
    override def forward (): Variabl =
        Variabl (ops.dot (v1.data, v2.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates gradients for dot product.
     *  @param gradOutput  the upstream gradient.
     */
    override def backward (gradOutput: TensorD): Unit =
        backpropForTwoInputs (v1, v2, gradOutput,
                             (g: TensorD) => ops.mul (g, v2.data),
                             (g: TensorD) => ops.mul (v1.data, g))

end Dot


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the matrix multiplication of two variables.
 *  @param v1  the first variable.
 *  @param v2  the second variable.
 */
case class MatMul (v1: Variabl, v2: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes matrix multiplication of v1 and v2.
     *  @return a Variabl with the result.
     */
    override inline def forward(): Variabl =
        Variabl (ops.matmul (v1.data, v2.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates gradients using transposed matrices.
     *  @param gradOutput  the upstream gradient.
     */
    override inline def backward (gradOutput: TensorD): Unit =
        backpropForTwoInputs (v1, v2, gradOutput,
            (g: TensorD) => ops.matmul (g, ops.transpose (v2.data, 1, 2)),
            (g: TensorD) => ops.matmul (ops.transpose (v1.data, 1 , 2), g))

end MatMul


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Computes the batched matrix multiplication of two variables.
 *  @param v1  the first variable.
 *  @param v2  the second variable.
 */
case class BatchMatMul (v1: Variabl, v2: Variabl)(using ops: AutogradOps) extends Function:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: computes batched matrix multiplication.
     *  @return a Variabl with the batched result.
     */
    override inline def forward (): Variabl =
        Variabl (ops.bmm (v1.data, v2.data), gradFn = Some (this))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward pass: propagates the gradients for batched matrix multiplication, unbroadcasting as necessary.
     *  @param gradOutput  the upstream gradient.
     */
    override inline def backward (gradOutput: TensorD): Unit =
        val v1T = ops.transpose (v1.data, 1, 2)
        val v2T = ops.transpose (v2.data, 1, 2)

        val gradA = ops.bmm (gradOutput, v2T)
        val gradB = ops.bmm (v1T, gradOutput)

        val gradAFinal = unbroadcast (gradA, v1.shape)
        val gradBFinal = unbroadcast (gradB, v2.shape)

        v1.backward (gradAFinal)
        v2.backward (gradBFinal)
    end backward

end BatchMatMul

// ----------------------------------------------------------------------------------
// TODO: Add Transpose and Permute Functions in the future for more flexibility...
// TODO: Add Author tags and documentations as well...
// ----------------------------------------------------------------------------------

