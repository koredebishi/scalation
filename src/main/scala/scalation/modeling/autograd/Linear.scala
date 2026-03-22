
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Praveen Rangavajhula
 *  @version 2.0
 *  @date    Fri April 25 19:48:13 EDT 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Autograd: Fully Connected (Linear) Layer for Neural Networks
 */

package scalation
package modeling
package autograd

import scalation.mathstat.TensorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A fully connected linear (affine) layer: output =weight.bmm(input) + bias
 *  Computes a linear transformation of the input tensor:
 *   - Weight shape: (1, outFeatures, inFeatures)
 *   - Bias shape: (1, outFeatures, 1)
 *   - Input shape: (batch, inFeatures, 1)
 *   - Output shape: (batch, outFeatures, 1)
 *  The weight and bias are learnable parameters wrapped in `Variabl`.
 *  Internally uses batched matrix multiplication and broadcasting for bias addition.
 *  @param inFeatures   the number of input features
 *  @param outFeatures  the number of output features
 */
class Linear (inFeatures: Int, outFeatures: Int)(using ops: AutogradOps)
      extends Module:

    private val weightData: TensorD = TensorD.fromMatrix (Initializer.weightMat (outFeatures, inFeatures))
    private val biasData: TensorD = TensorD.fromVector (Initializer.weightVec (outFeatures), axis = 1)

    val weight: Variabl = Variabl (weightData, name = Some ("weight"))
    val bias: Variabl   = Variabl (biasData, name = Some ("bias"))

    override def parameters: IndexedSeq [Variabl] = IndexedSeq (weight, bias)

    override def forward (input: Variabl): Variabl = weight.bmm (input) + bias

end Linear


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Linear` companion object for `Linear` to provide an easier construction API.
 */
object Linear:

    def apply (inFeatures: Int, outFeatures: Int)(using ops: AutogradOps): Linear =
        new Linear (inFeatures, outFeatures)

end Linear

