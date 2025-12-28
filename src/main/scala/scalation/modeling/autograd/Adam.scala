
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Praveen Rangavajhula
 *  @version 2.0
 *  @date    Fri April 25 20:01:00 EDT 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Autograd: Adam Optimizer for Parameter Updates
 */

package scalation
package modeling
package autograd

import scalation.mathstat.TensorD

// FIX -- switch to using hyper-parameters

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Implements the Adam optimization algorithm for updating model parameters.
 *  @param parameters  Indexed sequence of Variables representing model parameters.
 *  @param lr          Learning rate for updating the parameters.
 *  @param beta1       Exponential decay rate for the first moment estimates.
 *  @param beta2       Exponential decay rate for the second moment estimates.
 *  @param eps         Small constant added for numerical stability.
 */
case class Adam (parameters: IndexedSeq [Variabl], lr: Double = 0.001,
                 beta1: Double = 0.9, beta2: Double = 0.999, eps: Double = 1e-8)
    extends Optimizer (parameters):

    /** First moment estimates for each parameter, initialized to zeros with the same shape as the parameter data.
     */
    private val m = parameters.map (p => TensorD.zerosLike (p.data))

    /** Second moment estimates for each parameter, initialized to zeros with the same shape as the parameter data.
     */
    private val v = parameters.map (p => TensorD.zerosLike (p.data))

    /** Time step counter that tracks the number of updates made.
     */
    private var t = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs a single optimization step using the Adam algorithm.
     *  The step method increments the time step counter, then for each parameter:
     *   - Updates the biased first moment estimate.
     *   - Updates the biased second moment estimate.
     *   - Computes bias-corrected moment estimates.
     *   - Updates the parameter data using the computed moments.
     */
    override def step (): Unit =
        t          += 1                                                // Increment time step
        val beta1_t = (1 - beta1~^t)
        val beta2_t = (1 - beta2~^t)

        for i <- parameters.indices do
            val p   = parameters(i)
            var m_i = m(i)
            var v_i = v(i)

            if p.grad != null then
                val grad = p.grad

                m_i *= beta1                                           // Update biased first moment estimate
                m_i += grad * (1 - beta1)

                v_i *= beta2                                           // Update biased second moment estimate
                v_i += grad * grad * (1 - beta2)

                val m_hat = m_i / beta1_t                              // Compute bias-corrected moment estimates
                val v_hat = v_i / beta2_t

                p.data -= m_hat * lr / (v_hat.map_ (math.sqrt) + eps)  // Update the parameter with the computed moments
        end for
    end step

end Adam

