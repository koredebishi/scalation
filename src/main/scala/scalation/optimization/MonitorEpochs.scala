//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Korede Bishi
 *  @version 2.0
 *  @date    Sun Jun 25 16:30:31 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Monitor Epochs - Track Loss Function Over Training Epochs
 *
 *  @see     http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2097904
 */

package scalation
package optimization

import scala.collection.mutable.ArrayBuffer

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MonitorEpochs` trait provides simple epoch monitoring for optimization.
 *  It tracks loss over epochs and provides built-in console output options.
 *
 *  Verbose levels:
 *    0 = Silent (no output)
 *    1 = Standard progress (default, shows table of epochs)
 *    2 = Detailed (includes progress bar + table)
 *
 *  Example usage:
 *  {{{
 *  val optimizer = new SPSA_Mo(f, max_iter = 100)
 *  optimizer.setVerbose(1)        // Standard table output
 *  optimizer.setPrintEvery(10)    // Print every 10 epochs
 *  optimizer.solve(x0)            // Automatic monitoring
 *  optimizer.plotLoss()           // Visualize convergence
 *  }}}
 */
trait MonitorEpochs:

    /** Buffer storing the best loss value at each epoch */
    protected val epochLoss = new ArrayBuffer[Double]()

    /** Verbosity level: 0=silent, 1=standard, 2=detailed */
    protected var verbose = 1

    /** Print frequency for built-in console output (every N epochs) */
    protected var printEvery = 1

    /** Maximum number of epochs (for progress calculation) */
    protected var maxEpochs = 100

    /** Best loss value seen so far */
    protected var bestLoss = Double.MaxValue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the verbosity level for built-in console output.
     *  @param level  0=silent, 1=standard table output, 2=detailed with progress bar
     */
    def setVerbose (level: Int): Unit = verbose = level

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the frequency of printing epoch information.
     *  @param n  print every n epochs (e.g., 10 means print epochs 1, 10, 20, ...)
     */
    def setPrintEvery (n: Int): Unit = printEvery = n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize monitoring at the start of training.
     *  Called automatically by the optimizer's solve() method.
     *  @param maxEpochs  the total number of epochs
     */
    protected def initializeMonitoring (maxEpochs: Int): Unit =
        this.maxEpochs = maxEpochs
        this.bestLoss  = Double.MaxValue
        epochLoss.clear()

        // Print table header if verbose
        if verbose == 1 || verbose == 2 then
            println (sline (70).trim)
            println (f"${"Epoch"}%8s | ${"Loss"}%15s | ${"Best Loss"}%15s | ${"Progress"}%10s")
            println (sline (70).trim)
    end initializeMonitoring

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update monitoring for the current epoch with loss information.
     *  Called automatically by the optimizer once per epoch.
     *  @param epoch  the current epoch number (1-indexed)
     *  @param loss   the loss/objective value at this epoch
     */
    protected def updateMonitoring (epoch: Int, loss: Double): Unit =
        // Update best loss tracker
        if loss < bestLoss then bestLoss = loss

        // Record to history
        epochLoss += bestLoss

        // Verbose level 1: Table only
        if verbose == 1 then
            if epoch % printEvery == 0 || epoch == 1 then
                val progress = f"${epoch * 100.0 / maxEpochs}%5.1f%%"
                val improved = if loss <= bestLoss then "✓" else " "
                println (f"$epoch%8d | $loss%15.8f | $bestLoss%15.8f | $progress%10s $improved")

        // Verbose level 2: Table + Progress bar
        else if verbose == 2 then
            if epoch % printEvery == 0 || epoch == 1 then
                val progress = f"${epoch * 100.0 / maxEpochs}%5.1f%%"
                val improved = if loss <= bestLoss then "✓" else " "
                println (f"$epoch%8d | $loss%15.8f | $bestLoss%15.8f | $progress%10s $improved")

            // Progress bar updates every epoch
            val barWidth = 50
            val progress = epoch.toDouble / maxEpochs
            val pos      = (barWidth * progress).toInt
            val bar      = "█" * pos + "░" * (barWidth - pos)
            print (f"\r[$bar] ${progress * 100}%5.1f%%")
            if epoch == maxEpochs then println ()
    end updateMonitoring

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Finalize monitoring at the end of training.
     *  Called automatically by the optimizer's solve() method.
     */
    protected def finalizeMonitoring (): Unit =
        if verbose > 0 then
            println (sline (70).trim)
            println (s"Training completed! Total epochs: ${epochLoss.size}")
            if epochLoss.nonEmpty then
                println (f"Final Loss: ${epochLoss.last}%.8f")
                println (f"Best Loss:  ${epochLoss.min}%.8f")
            println (sline (70).trim)
    end finalizeMonitoring

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the complete loss history for all epochs.
     *  Useful for plotting or post-training analysis.
     *  @return  array buffer containing best loss at each epoch
     */
    def lossPerEpochs (): ArrayBuffer[Double] = epochLoss

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot the loss function over epochs using scalation's Plot utility.
     *  Provides a visual representation of convergence behavior.
     */
    def plotLoss (): Unit =
        val el = new VectorD (epochLoss.size, epochLoss.toArray)
        new Plot (null, el, null, "Loss Function vs. Epoch")
    end plotLoss

end MonitorEpochs
