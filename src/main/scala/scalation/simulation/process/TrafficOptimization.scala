package scalation
package simulation
package process


import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** General-purpose optimization framework for calibrating simulation models.
 *  This class uses trait-based dependency injection to work with any CalibratableModel,
 *  making it reusable across different traffic simulation scenarios.
 *  
 *  The optimization process:
 *    1. Objective function receives parameter vector θ
 *    2. Applies parameters to the calibratable model
 *    3. Runs simulation
 *    4. Computes and returns fitness metric (e.g., SMAPE)
 *  
 *  Usage pattern (see CalibrateCalRoute101.scala for real-world example):
 *    {{{
 *    class MyModelAdapter extends CalibratableModel:
 *        def applyParameters(params: VectorD): Unit = // configure model
 *        def runSimulation(): Unit = // run simulation
 *        def computeFitness(): Double = // compute fitness metric
 *    
 *    val modelAdapter = new MyModelAdapter()
 *    val simOpt = new TrafficOptimization(modelAdapter)
 *    val optimizer = new NelderMeadSimplex2(simOpt.func, params.dim)
 *    val opt = optimizer.solve(params)
 *    }}}
 *  
 *  @param model  a CalibratableModel instance (trait mixing pattern)
 */
class TrafficOptimization(model: CalibratableModel):
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Objective function for optimization: y = f(θ) + ε
     *  Evaluates model fitness for given parameter vector.
     *  
     *  @param params  parameter vector θ (e.g., [s, amax, bmax, T, τ] for IDM vehicle model)
     *  @return        fitness metric value (lower is better)
     */
    def objFunc(params: VectorD): Double =
        println(s"Evaluating parameters: $params")
        
        // Configure model with parameters
        model.applyParameters(params)
        
        // Execute simulation
        model.runSimulation()
        
        // Compute fitness metric
        val fitness = model.computeFitness()
        
        println(s"Fitness value: $fitness")
        fitness  // return fitness (objective to minimize)
        
    end objFunc

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Function wrapper for optimizer compatibility (FunctionV2S interface) */
    val func: FunctionV2S = (params: VectorD) => objFunc(params)

end TrafficOptimization



