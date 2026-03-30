package scalation
package simulation
package process

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Trait for simulation models that can be calibrated through optimization.
 *  Models implementing this trait can be used with the TrafficOptimization framework.
 *  
 *  The calibration process follows the pattern:
 *    1. Apply parameters to model configuration (applyParameters)
 *    2. Run the simulation (runSimulation)
 *    3. Compute fitness metric (computeFitness)
 */
trait CalibratableModel:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the given parameter vector to configure the model before simulation.
     *  This method sets up model-specific state (e.g., vehicle properties, arrival rates).
     *  
     *  @param params  the parameter vector to apply (e.g., [s, amax, bmax, T, τ])
     */
    def applyParameters(params: VectorD): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation model with the currently applied parameters.
     *  This executes the full simulation and collects necessary statistics.
     */
    def runSimulation(): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the fitness metric for the completed simulation run.
     *  Lower values indicate better fit (e.g., SMAPE, RMSE, MAE).
     *  
     *  @return the fitness value (objective function output) - lower is better
     */
    def computeFitness(): Double

end CalibratableModel

