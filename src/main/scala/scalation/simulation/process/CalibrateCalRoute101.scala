package scalation
package simulation
package process

import scalation.mathstat.VectorD
import scalation.optimization.{NelderMeadSimplex2, SPSA_Mo, SPSA, DifferentialEvolution}
import scalation.random.{Uniform, Variate}
import scalation.simulation.process.example_1.CalRoute101


val easyW = new EasyWriter("simulation", "Home_14-CalibrateCalRoute101.txt")


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Adapter to make CalRoute101 compatible with CalibratableModel trait.
 *  Encapsulates CalRoute101-specific parameter mapping and fitness computation.
 *  Follows scalation 2.0 trait mixing pattern (extends/with).
 */
class CalibrateCalRoute101 extends CalibratableModel:

    private var model: CalRoute101 = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply IDM parameters to Vehicle singleton and create model instance.
     *  @param params  [s, amax, bmax, T, τ] - IDM vehicle behavior parameters
     */
    def applyParameters(params: VectorD): Unit =
        // Map parameter vector to Vehicle properties
        Vehicle.setProps(Vehicle.setParams(params))       // The parameters to be optimized from the vehicle
        // Create new model instance with updated Vehicle settings
        model = new CalRoute101()       // Re-instantiate model to apply new vehicle properties
    end applyParameters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the CalRoute101 simulation */
    def runSimulation(): Unit =
        model.simulate()  // Run the simulation
    end runSimulation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute average RMSE across mainline sensors only.
     *  Uses only the first 3 sensors (afterOfframp1, afterOnramp1, afterOnramp2) for optimization.
     *  Also computes SMAPE for monitoring (not logged in hot path to avoid I/O overhead).
     *  @return average RMSE value for mainline sensors (used by optimizer)
     */
    def computeFitness(): Double =
         val qofMetrics = model.getQoFMetrics()  // Get full QoF metrics for all 5 sensors

         // Extract RMSE (index 5) from first 3 mainline sensors
         val mainlineRMSE = qofMetrics.slice(0, 3).map(qof => qof(5))
         val avgRMSE = mainlineRMSE.sum / mainlineRMSE.length

         avgRMSE  // Return RMSE for optimization (SMAPE available at index 7 if needed)
    end computeFitness
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Get final metrics summary (RMSE and SMAPE) for reporting after optimization.
//     *  @return tuple of (avgRMSE, Array of individual SMAPE values for 3 mainline sensors)
//     *          SMAPE array: [afterOfframp1, afterOnramp1, afterOnramp2]
//     */
//    def getFinalMetrics(): (Double, Array[Double]) =
//        val qofMetrics = model.getQoFMetrics()
//        val mainlineRMSE = qofMetrics.slice(0, 3).map(qof => qof(5))
//        val mainlineSMAPE = qofMetrics.slice(0, 3).map(qof => qof(7))
//        val avgRMSE = mainlineRMSE.sum / mainlineRMSE.length
//        (avgRMSE, mainlineSMAPE.toArray)  // Return avg RMSE and individual SMAPE values
//    end getFinalMetrics

end CalibrateCalRoute101

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Calibration driver for CalRoute101 traffic simulation model.
 *  Uses the general TrafficOptimization framework to optimize IDM vehicle parameters.
 *
 *  Optimization Strategy:
 *    - Initial approach: SPSA (Simultaneous Perturbation Stochastic Approximation)
 *      * Fastest optimizer - perturbs all parameters simultaneously
 *      * Good for initial exploration with 1000s of evaluations
 *      * Monitor stopping criteria to avoid premature convergence
 *
 *    - Alternative: NelderMeadSimplex2 (currently active)
 *      * Derivative-free optimization
 *      * Suitable for noisy objective functions
 *
 *  Execution Context:
 *    - Multi-core parallel runs recommended
 *    - Each simulation: 12-hour traffic period
 *    - Thousands of evaluations expected
 *    - Filter logs to show only optimization progress
 *
 *  Parameters Being Optimized (IDM vehicle model):
 *    @param s     safe minimum distance headway (meters)
 *    @param amax  maximum acceleration (m/s²)
 *    @param bmax  maximum deceleration (m/s²) [negative value]
 *    @param T     safe minimum time headway (seconds)
 *    @param τ     driver reaction time (seconds)
 *
 *  Objective: Minimize average SMAPE across 7 PeMS sensors
 */
@main def runCalibrateCalRoute101 (): Unit =

    // Define search ranges for GA: [s, amax, bmax, T, τ]
    val randVars: Array[Variate] = Array(
        Uniform(3.0, 10.0), // s:    safe distance headway (3-10 meters)
        Uniform(1.0, 10.0), // amax: max acceleration (1-10 m/s²)
        Uniform(-10.0, -1.0), // bmax: max deceleration (-10 to -1 m/s²)
        Uniform(1.0, 5.0), // T:    safe time headway (1-5 seconds)
        Uniform(0.5, 3.0) // τ:    reaction time (0.5-3 seconds)
    )


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create CalRoute101 model adapter (implements CalibratableModel trait) */
    val modelAdapter = new CalibrateCalRoute101()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create general optimizer with CalRoute101 model adapter */
    val simOpt = new TrafficOptimization(modelAdapter)
   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initial parameter guess for Gipps model: [s, amax, bmax, T, τ] */
    val params: VectorD = VectorD(5.0, 4.0, -1.5, 3.0, 1.0)  // shared params
    //4.99888,      3.99888,        -1.49888,       2.99888,        1.00112

    

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate initial fitness before optimization */
    //val smapeValue: Double = simOpt.func(params)
    //easyW.println(s"The smape value for the 7 sensor is : $smapeValue")

     //:::::::::::::::::::::::::::::::::::::::::::::::::
    val optimizer1 = new SPSA(simOpt.func, params.dim).solve(params)
    //easyW.println(s"optimal solution = (f(x), x) = $optimizer1")
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //val optimizer2 = new NelderMeadSimplex2(simOpt.func, params.dim).solve(params)
    //easyW.println(s"optimal solution = (f(x), x) = $optimizer2")



//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    // Genetic Algorithm Optimizer - Population-based global search
//    // Random variates define search bounds for each parameter
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    easyW.println("\n[3/3] Running Genetic Algorithm Optimizer...")
//    easyW.println("-" * 80)
//    val gaOptimizer = new GeneticAlgorithm(simOpt.func, randVars)
//    val gaResult = gaOptimizer.solve2() // Note: GA uses solve2() method
//    easyW.println(s"Genetic Algorithm Result: fitness = ${gaResult._1}, params = ${gaResult._2}")
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::a:::::::
    /** Clean up simulation resources */
    Model.shutdown()

end runCalibrateCalRoute101


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run SPSA Optimizer Only - For SLURM Job Array
 *  > runMain scalation.simulation.process.runCalibrate_SPSA
 */
@main def runCalibrate_SPSA(): Unit =
    banner("SPSA OPTIMIZER - CalRoute101 Calibration")

    val modelAdapter = new CalibrateCalRoute101()
    val simOpt = new TrafficOptimization(modelAdapter)
    val params: VectorD = VectorD(5.0, 4.0, -1.5, 3.0, 1.0)

    println(s"Starting SPSA Optimization at ${java.time.LocalDateTime.now()}")
    println(s"Initial parameters: $params")

    val spsaOptimizer = new SPSA(simOpt.func, params.dim)
    val startTime = System.currentTimeMillis()
    val result    = spsaOptimizer.solve(params)
    val endTime   = System.currentTimeMillis()
    val duration  = (endTime - startTime) / 1000.0

//    // Get final metrics (RMSE and individual SMAPE values)
//    val (finalRMSE, smapeArray) = modelAdapter.getFinalMetrics()
//
//    println("\n" + "="*80)
//    println("SPSA OPTIMIZATION COMPLETE")
//    println("="*80)
    println(s"Best Fitness : ${result._1}")
//    println(s"SMAPE - afterOfframp1: ${smapeArray(0)}")
//    println(s"SMAPE - afterOnramp1:  ${smapeArray(1)}")
//    println(s"SMAPE - afterOnramp2:  ${smapeArray(2)}")
    println(s"Best Parameters: ${result._2}")
//    println(s"Execution Time: $duration seconds")
//    println("="*80)

    //easyW.println(s"SPSA: RMSE=${result._1}, SMAPE=[${smapeArray(0)},${smapeArray(1)},${smapeArray(2)}], params=${result._2}, time=${duration}s")

    Model.shutdown()
end runCalibrate_SPSA




@main def runCalibrate_SPSA_Mo(): Unit =
    banner("SPSA OPTIMIZER - CalRoute101 Calibration")

    val modelAdapter = new CalibrateCalRoute101()
    val simOpt = new TrafficOptimization(modelAdapter)
    val params: VectorD = VectorD(5.0, 4.0, -1.5, 3.0, 1.0)

    println(s"Starting SPSA Optimization at ${java.time.LocalDateTime.now()}")
    println(s"Initial parameters: $params")

    val spsaOptimizer = new SPSA_Mo(simOpt.func, params.dim)
    val startTime = System.currentTimeMillis()
    val result = spsaOptimizer.solve(params)
    val endTime = System.currentTimeMillis()
    val duration = (endTime - startTime) / 1000.0

    // Get final metrics (RMSE and individual SMAPE values)
    //val (finalRMSE, smapeArray) = modelAdapter.getFinalMetrics()
//
//    println("\n" + "="*80)
//    println("SPSA_Mo OPTIMIZATION COMPLETE")
//    println("="*80)
    println(s"Best Fitness: ${result._1}")
//    println(s"SMAPE - afterOfframp1: ${smapeArray(0)}")
//    println(s"SMAPE - afterOnramp1:  ${smapeArray(1)}")
//    println(s"SMAPE - afterOnramp2:  ${smapeArray(2)}")
    println(s"Best Parameters: ${result._2}")
//    println(s"Execution Time: $duration seconds")
//    println("="*80)

    //easyW.println(s"SPSA: RMSE=${result._1}, SMAPE=[${smapeArray(0)},${smapeArray(1)},${smapeArray(2)}], params=${result._2}, time=${duration}s")

    Model.shutdown()
end runCalibrate_SPSA_Mo


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run Nelder-Mead Simplex Optimizer Only - For SLURM Job Array
 * > runMain scalation.simulation.process.runCalibrate_NelderMead
 */
@main def runCalibrate_NelderMead(): Unit =
    banner("NELDER-MEAD SIMPLEX OPTIMIZER - CalRoute101 Calibration")

    val modelAdapter = new CalibrateCalRoute101()
    val simOpt = new TrafficOptimization(modelAdapter)
    val params: VectorD = VectorD(5.0, 4.0, -1.5, 3.0, 1.0)

    println(s"Starting Nelder-Mead Optimization at ${java.time.LocalDateTime.now()}")
    println(s"Initial parameters: $params")

    val nelderMeadOptimizer = new NelderMeadSimplex2(simOpt.func, params.dim)
    val startTime = System.currentTimeMillis()
    val result = nelderMeadOptimizer.solve(params)
    val endTime = System.currentTimeMillis()
    val duration = (endTime - startTime) / 1000.0

    println(s"Best Fitness : ${result._1}")
    println(s"Best Parameters: ${result._2}")
    println(s"Execution Time: $duration seconds")

    Model.shutdown()
end runCalibrate_NelderMead


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run Differential Evolution Optimizer Only - For SLURM Job Array
 *  > runMain scalation.simulation.process.runCalibrate_DifferentialEvolution
 */
@main def runCalibrate_DifferentialEvolution(): Unit =

    banner("DIFFERENTIAL EVOLUTION OPTIMIZER - CalRoute101 Calibration")

    val modelAdapter = new CalibrateCalRoute101()
    val simOpt = new TrafficOptimization(modelAdapter)
    val params: VectorD = VectorD(5.0, 4.0, -1.5, 3.0, 1.0)

    println(s"Starting Differential Evolution Optimization at ${java.time.LocalDateTime.now()}")
    println(s"Initial parameters: $params")

    // Optimized bounds matching your parameter ranges
    val bounds = (-10.0, 10.0)  
    
    // DE Settings for FASTEST convergence:
    // maxGen = 200 (reduced from default 400, early stopping will exit sooner anyway)
    // F = 0.8 (good balance), CR = 0.9 (high crossover for faster convergence)
    // popSize = 50 (10*dim, larger population = fewer generations needed)
    
    val startTime = System.currentTimeMillis()
    val result = DifferentialEvolution.optimize(simOpt.func, params.dim, bounds, maxGen = 200, F = 0.8, CR = 0.9)(popSize = 50)
    val endTime = System.currentTimeMillis()
    val duration = (endTime - startTime) / 1000.0

    println(s"Best Fitness : ${result._2}")
    println(s"Best Parameters: ${result._1}")
    println(s"Total Duration: $duration seconds")

    Model.shutdown()
end runCalibrate_DifferentialEvolution


////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** Run Genetic Algorithm Optimizer Only - For SLURM Job Array
// *  > runMain scalation.simulation.process.runCalibrate_GA
// */
//@main def runCalibrate_GA(): Unit =
//    banner("GENETIC ALGORITHM OPTIMIZER - CalRoute101 Calibration")
//
//    val modelAdapter = new CalibrateCalRoute101()
//    val simOpt = new TrafficOptimization(modelAdapter)
//    val params: VectorD = VectorD(5.0, 4.0, -1.5, 3.0, 1.0)
//
//    println(s"Starting Genetic Algorithm Optimization at ${java.time.LocalDateTime.now()}")
//    println(s"Initial parameters: $params")
//
//    // Define search ranges for GA: [s, amax, bmax, T, τ]
//    val randVars: Array[Variate] = Array(
//        Uniform(3.0, 10.0),   // s:    safe distance headway (3-10 meters)
//        Uniform(1.0, 10.0),   // amax: max acceleration (1-10 m/s²)
//        Uniform(-10.0, -1.0), // bmax: max deceleration (-10 to -1 m/s²)
//        Uniform(1.0, 5.0),    // T:    safe time headway (1-5 seconds)
//        Uniform(0.5, 3.0)     // τ:    reaction time (0.5-3 seconds)
//    )
//
//    val gaOptimizer = new GeneticAlgorithm(simOpt.func, randVars)
//    val startTime = System.currentTimeMillis()
//    val result = gaOptimizer.solve2()
//    val endTime = System.currentTimeMillis()
//    val duration = (endTime - startTime) / 1000.0
//
//    // Get final metrics (RMSE and individual SMAPE values)
//    val (finalRMSE, smapeArray) = modelAdapter.getFinalMetrics()
//
//    println("\n" + "="*80)
//    println("GENETIC ALGORITHM OPTIMIZATION COMPLETE")
//    println("="*80)
//    println(s"Best Fitness (RMSE): ${result._1}")
//    println(s"SMAPE - afterOfframp1: ${smapeArray(0)}")
//    println(s"SMAPE - afterOnramp1:  ${smapeArray(1)}")
//    println(s"SMAPE - afterOnramp2:  ${smapeArray(2)}")
//    println(s"Best Parameters: ${result._2}")
//    println(s"Execution Time: $duration seconds")
//    println("="*80)
//
//    easyW.println(s"GA: RMSE=${result._1}, SMAPE=[${smapeArray(0)},${smapeArray(1)},${smapeArray(2)}], params=${result._2}, time=${duration}s")
//
//    Model.shutdown()
//end runCalibrate_GA

