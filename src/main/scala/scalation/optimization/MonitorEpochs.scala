
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun May  5 13:13:42 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     http://www.scholarpedia.org/article/Nelder-Mead_algorithm
 *  @see     http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2097904
 */

package scalation
package optimization

import scala.collection.mutable.ArrayBuffer

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MonitorEpochs` trait is used to monitor the loss function over the epochs.
 */
trait MonitorEpochs:

    protected val epochLoss = new ArrayBuffer [Double] () // record each functional value for each epoch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the loss function for each epoch.
     */
    def lossPerEpoch(): ArrayBuffer[Double] = epochLoss

end MonitorEpochs
