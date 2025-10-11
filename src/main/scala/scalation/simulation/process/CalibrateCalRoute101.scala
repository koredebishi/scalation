package scalation
package simulation
package process

import scalation.mathstat.VectorD
import scalation.optimization.{GeneticAlgorithm, NelderMeadSimplex2, SPSA}
import scalation.random.{Uniform, Variate}
import scalation.simulation.process.example_1.CalRoute101


val easyW = new EasyWriter("simulation", "CalibrateCalRoute101.txt")

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
        easyW.println(s"Vehicle properties set to: ${Vehicle.prop}")   // Debug print to verify properties

        // Create new model instance with updated Vehicle settings
        model = new CalRoute101()       // Re-instantiate model to apply new vehicle properties
    end applyParameters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the CalRoute101 simulation */
    def runSimulation(): Unit =
        model.simulate()  // Run the simulation
    end runSimulation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute average SMAPE across all sensors.
     *  @return average SMAPE value (placeholder until simRunVsPemsRun is re-enabled)
     */
    def computeFitness(): Double =
         //Future implementation (when simRunVsPemsRun is uncommented in CalRoute101):
         val smapeArray = model.simRunVsPemsRun()
         val avgSmape = smapeArray.sum / smapeArray.length
         avgSmape
        //val avgSmape = 0.0  // Placeholder - allows optimizer to run and suggest parameters
    end computeFitness

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
    
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate initial fitness before optimization */
    val smapeValue: Double = simOpt.func(params)
    easyW.println(s"The smape value for the 7 sensor is : $smapeValue")

     //:::::::::::::::::::::::::::::::::::::::::::::::::
    val optimizer1 = new SPSA(simOpt.func, params.dim).solve(params)
    easyW.println(s"optimal solution = (f(x), x) = $optimizer1")
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    val optimizer2 = new NelderMeadSimplex2(simOpt.func, params.dim).solve(params)
    easyW.println(s"optimal solution = (f(x), x) = $optimizer2")




    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Genetic Algorithm Optimizer - Population-based global search
    // Random variates define search bounds for each parameter
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    easyW.println("\n[3/3] Running Genetic Algorithm Optimizer...")
    easyW.println("-" * 80)
    val gaOptimizer = new GeneticAlgorithm(simOpt.func, randVars)
    val gaResult = gaOptimizer.solve2() // Note: GA uses solve2() method
    easyW.println(s"Genetic Algorithm Result: fitness = ${gaResult._1}, params = ${gaResult._2}")
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::a:::::::
    /** Clean up simulation resources */
    Model.shutdown()

end runCalibrateCalRoute101


@main def runCalibrateCalROute101_OptimizerCompare():Unit =

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create CalRoute101 model adapter (implements CalibratableModel trait) */
    val modelAdapter = new CalibrateCalRoute101()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create general optimizer with CalRoute101 model adapter */
    val simOpt = new TrafficOptimization(modelAdapter)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initial parameter guess for Gipps model: [s, amax, bmax, T, τ] */
    val params: VectorD = VectorD(5.0, 4.0, -1.5, 3.0, 1.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate initial fitness before optimization */
    val smapeValue: Double = simOpt.func(params)
    easyW.println(s"Initial SMAPE value for the 7 sensors: $smapeValue\n")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run multiple optimizers and compare results */
    easyW.println("\n" + "="*80)
    easyW.println("MULTI-OPTIMIZER CALIBRATION COMPARISON")
    easyW.println("="*80 + "\n")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // SPSA Optimizer - Fast stochastic approximation
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    easyW.println("\n[1/3] Running SPSA Optimizer...")
    easyW.println("-" * 80)
    val spsaOptimizer = new SPSA(simOpt.func, params.dim)
    val spsaResult = spsaOptimizer.solve(params)
    easyW.println(s"SPSA Result: fitness = ${spsaResult._1}, params = ${spsaResult._2}")
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Nelder-Mead Simplex Optimizer - Robust derivative-free method
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    easyW.println("\n[2/3] Running Nelder-Mead Simplex Optimizer...")
    easyW.println("-" * 80)
    val nelderMeadOptimizer = new NelderMeadSimplex2(simOpt.func, params.dim)
    val nelderMeadResult = nelderMeadOptimizer.solve(params)
    easyW.println(s"Nelder-Mead Result: fitness = ${nelderMeadResult._1}, params = ${nelderMeadResult._2}")
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Genetic Algorithm Optimizer - Population-based global search
    // Random variates define search bounds for each parameter
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    easyW.println("\n[3/3] Running Genetic Algorithm Optimizer...")
    easyW.println("-" * 80)
    
    // Define search ranges for GA: [s, amax, bmax, T, τ]
    val randVars: Array[Variate] = Array(
        Uniform(3.0, 10.0),   // s:    safe distance headway (3-10 meters)
        Uniform(1.0, 10.0),   // amax: max acceleration (1-10 m/s²)
        Uniform(-10.0, -1.0), // bmax: max deceleration (-10 to -1 m/s²)
        Uniform(1.0, 5.0),    // T:    safe time headway (1-5 seconds)
        Uniform(0.5, 3.0)     // τ:    reaction time (0.5-3 seconds)
    )
    
    val gaOptimizer = new GeneticAlgorithm(simOpt.func, randVars)
    val gaResult = gaOptimizer.solve2()  // Note: GA uses solve2() method
    easyW.println(s"Genetic Algorithm Result: fitness = ${gaResult._1}, params = ${gaResult._2}")
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Compare results and identify best optimizer
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    easyW.println("\n" + "="*80)
    easyW.println("OPTIMIZATION RESULTS SUMMARY")
    easyW.println("="*80)
    
    // Collect all results
    val results = Array(
        ("SPSA", spsaResult._1, spsaResult._2),
        ("Nelder-Mead", nelderMeadResult._1, nelderMeadResult._2),
        ("Genetic Algorithm", gaResult._1, gaResult._2)
    )
    
    // Find best result (lowest fitness/SMAPE)
    val bestResult = results.minBy(_._2)
    
    easyW.println(s"\n${"Algorithm"} ${"Fitness (SMAPE)"}")
    easyW.println("-" * 80)
    for (name, fitness, _) <- results do
        val marker = if name == bestResult._1 then " ← BEST" else ""
        easyW.println(s"$name $fitness  $marker")
    
    easyW.println("\n" + "="*80)
    easyW.println(s"Best Algorithm: ${bestResult._1}")
    easyW.println(s"Best Fitness:   ${bestResult._2}")
    easyW.println(s"Best Parameters: ${bestResult._3}")
    easyW.println("="*80 + "\n")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clean up simulation resources */
    Model.shutdown()

end runCalibrateCalROute101_OptimizerCompare