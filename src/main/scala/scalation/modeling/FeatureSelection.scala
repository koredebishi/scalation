
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Sep 27 20:58:20 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Support for Feature Selection
 *
 *  @see     bookdown.org/max/FES/selection.html
 */

package scalation
package modeling

import scala.collection.mutable.LinkedHashSet

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SelectionTech` enumeration indicates the available feature selection
 *  techniques.
 */
enum SelectionTech:

     case Forward, Backward, Stepwise

end SelectionTech


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FeatureSelection` trait establishes a framework for feature selection,
 *  i.e., selecting the features (e.g., variable x_j, cross term x_j x_k, or
 *  functional form x_j^2) to include in the model.
 */
trait FeatureSelection:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform feature selection to find the most predictive features/variables
     *  to  have in the model, returning the features/variables added and the new
     *  Quality of Fit (QoF) measures/metrics for all steps.
     *  @see `Fit` for index of QoF measures/metrics.
     *  @param tech   the feature selection technique to apply
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     *  @param cross  whether to include the cross-validation QoF measure
     */
    def selectFeatures (tech: SelectionTech, qk: Int = QoF.rSqBar.ordinal, cross: Boolean = true):
                       (LinkedHashSet [Int], MatrixD) =
        tech match
        case SelectionTech.Forward  => forwardSelAll (qk, cross)
        case SelectionTech.Backward => backwardElimAll (qk, 1, cross)
        case SelectionTech.Stepwise => stepwiseSelAll (qk, cross)
    end selectFeatures

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform FORWARD SELECTION to find the MOST predictive features/variables
     *  to ADD into the model, returning the features/variables added and the new
     *  Quality of Fit (QoF) measures/metrics for all steps.
     *  @see `Fit` for index of QoF measures/metrics.
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     *  @param cross  whether to include the cross-validation QoF measure
     */
    def forwardSelAll (qk: Int = QoF.rSqBar.ordinal, cross: Boolean = true):
                      (LinkedHashSet [Int], MatrixD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform BACKWARD ELIMINATION to find the LEAST predictive features/variables
     *  to REMOVE from the full model, returning the features/variables left and the
     *  new Quality of Fit (QoF)  measures/metrics for all steps.
     *  @see `Fit` for index of QoF measures/metrics.
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     *  @param first  first variable to consider for elimination
     *  @param cross  whether to include the cross-validation QoF measure
     */
    def backwardElimAll (qk: Int = QoF.rSqBar.ordinal, first: Int = 1, cross: Boolean = true):
                        (LinkedHashSet [Int], MatrixD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform STEPWISE SELECTION to find a GOOD COMBINATION of predictive features/variables
     *  to have in the model, returning the features/variables left and the new Quality of Fit
     *  (QoF) measures/metrics for all steps.  At each step, it calls forward and backward
     *  and takes the best of the two actions.  Stops when neither action yields improvement.
     *  @see `Fit` for index of QoF measures/metrics.
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     *  @param cross  whether to include the cross-validation QoF measure
     *  @param swap   whether to allow a swap step (swap out a feature for a new feature in one step)
     */
    def stepwiseSelAll (qk: Int = QoF.rSqBar.ordinal, cross: Boolean = true, swap: Boolean = true):
                       (LinkedHashSet [Int], MatrixD)

end FeatureSelection


type Model_FS = (Predictor | neuralnet.PredictorMV) & Fit


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BestStep` is used to record the best improvement step found so far during
 *   feature selection.  Note, best depends on whether maximizing or minimizing
 *  @param col    the column/variable to ADD/REMOVE for this step
 *  @param qk     the index for the Quality of Fit (QoF) measure/metric used for comparison
 *  @param qof    the Quality of Fit (QoF) for this step
 *  @param mod    the model including selected features/variables for this step
 *  @param bestq  the best QoF for metric qk so far
 */
case class BestStep (col: Int = -1, qk: Int = QoF.rSqBar.ordinal, qof: VectorD = null,
                     mod: Model_FS = null)
                    (bestq: Double = if Fit.maxi.contains (qk) then -MAX_VALUE else MAX_VALUE):

    private val debug = debugf ("BestStep", true)
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the better between this and candidate step.
     *  @param cand  the new candidate
     */
    def better (cand: BestStep): BestStep =
        debug ("better", s"cand = $cand vs. this = $this")
        if qof == null then cand
        else if Fit.maxi.contains (qk) then 
            if cand.qof(qk) > qof(qk) then cand else this             // maximize, e.g., R^2
        else
            if cand.qof(qk) < qof(qk) then cand else this             // minimize, e.g., mse, smape
    end better

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the better between this and the to be formed candidate step.
     *  @param j      the index of the feature/variable
     *  @param qof_j  the QoF for mod_j
     *  @param mod_j  the model with j
     */
    def better (j: Int, qof_j: VectorD, mod_j: Model_FS): BestStep =
        better (BestStep (j, qk, qof_j, mod_j)(qof_j(qk)))
    end better

end BestStep


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Update the rSq-based and smape QoF results for the l-th iteration of feature
 *  selection.
 *  @see `Predictor`
 *  @param rSq    the matrix contain information about r-Sq-based QoF measures
 *  @param l      the l-th iteration
 *  @param cross  indicator of whether cross-validation are to be included
 *  @param fit_l  the fit vector for the l-th iteration
 *  @param mod_l  the predictive model for the l-th iteration
 */
def updateQoF (rSq: MatrixD, l: Int, cross: Boolean, best: BestStep): Unit =
    rSq(l) =
        if cross then
            Fit.qofVector (best.qof, best.mod.crossValidate ())       // results for model mod_l, with cross-validation
        else
            Fit.qofVector (best.qof, null)                            // results for model mod_l, no cross-validation
end updateQoF

