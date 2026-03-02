//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Mon Feb 03 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Calibration Framework for Traffic Simulation Models
 *           Provides trait-based interface and optimization wrapper for model calibration
 */

package scalation
package simulation
package process
package builder

import scalation.mathstat.{FunctionV2S, VectorD}
import scalation.simulation.process.config.{CarFollowingModel, CFParams, DynamicsConfig, ODESolverType}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Trait for simulation models that can be calibrated through optimization.
 *  Models implementing this trait can be used with the CalibrationFramework.
 *  
 *  The calibration process follows the pattern:
 *    1. Apply parameters to model configuration (applyParameters)
 *    2. Run the simulation (runSimulation)
 *    3. Compute fitness metric (computeFitness)
 *  
 *  Example usage with CalRoute101_3:
 *    {{{
 *    class CalibrateCalRoute101_3(demand: PeMSDemand) extends CalibratableModel:
 *        private var model: CalRoute101_3 = null
 *        
 *        def applyParameters(params: VectorD): Unit =
 *            Vehicle.setProps(Vehicle.setParams(params))
 *            model = new CalRoute101_3(demand)
 *        end applyParameters
 *        
 *        def runSimulation(): Unit = model.simulate()
 *        
 *        def computeFitness(): Double =
 *            val report = SimulationReport(model.getFlowData(), model.getSpeedData(), demand)
 *            report.fitness()
 *        end computeFitness
 *    end CalibrateCalRoute101_3
 *    }}}
 */
trait CalibratableModel:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the given parameter vector to configure the model before simulation.
     *  This method sets up model-specific state (e.g., vehicle properties, arrival rates).
     *  
     *  @param params  the parameter vector to apply (e.g., [s, amax, bmax, T, τ] for IDM)
     */
    def applyParameters(params: VectorD): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation model with the currently applied parameters.
     *  This executes the full simulation and collects necessary statistics.
     */
    def runSimulation(): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the fitness metric for the completed simulation run.
     *  Lower values indicate better fit to observed data (e.g., SMAPE, RMSE, NRMSE).
     *  
     *  @return the fitness value (objective function output) - lower is better
     */
    def computeFitness(): Double

end CalibratableModel


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** General-purpose optimization framework for calibrating simulation models.
 *  This class uses trait-based dependency injection to work with any CalibratableModel,
 *  making it reusable across different traffic simulation scenarios and corridors.
 *  
 *  The optimization process:
 *    1. Objective function receives parameter vector θ
 *    2. Applies parameters to the calibratable model
 *    3. Runs simulation
 *    4. Computes and returns fitness metric
 *  
 *  Usage with optimization algorithms:
 *    {{{
 *    // 1. Create model adapter
 *    val modelAdapter = new CalibrateCalRoute101_3(demand)
 *    val optimizer = new ModelOptimizer(modelAdapter)
 *    
 *    // 2. Use with any optimizer from scalation.optimization
 *    val params = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)  // [s, amax, bmax, T, τ]
 *    
 *    // SPSA
 *    val spsaOpt = new SPSA(optimizer.func, maxIter = 100)
 *    val result = spsaOpt.solve(params)
 *    
 *    // Nelder-Mead
 *    val nmOpt = new NelderMeadSimplex2(optimizer.func, params.dim)
 *    val result = nmOpt.solve(params)
 *    
 *    // Genetic Algorithm
 *    val gaOpt = new GeneticAlgorithm(optimizer.func, randVars)
 *    val result = gaOpt.solve2()
 *    }}}
 *  
 *  @param model  a CalibratableModel instance (implements the trait)
 */
class ModelOptimizer(model: CalibratableModel):
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Objective function for optimization: y = f(θ) + ε
     *  Evaluates model fitness for given parameter vector.
     *  
     *  @param params  parameter vector θ (e.g., [s, amax, bmax, T, τ] for IDM vehicle model)
     *  @return        fitness metric value (lower is better)
     */
    def objFunc(params: VectorD): Double =
        println(s"[ModelOptimizer] Evaluating parameters: $params")
        
        val (fitness, elapsed) = timed {
            // Step 1: Configure model with parameters
            model.applyParameters(params)

            // Step 2: Execute simulation
            model.runSimulation()

            // Step 3: Compute fitness metric
            model.computeFitness()
        }

        println(f"[ModelOptimizer] Fitness: $fitness%.4f | Elapsed: ${elapsed / 1000.0}%.2f sec")
        fitness  // return fitness (objective to minimize)
        
    end objFunc

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Function wrapper for optimizer compatibility (FunctionV2S interface).
     *  This allows the optimizer to be used with any scalation optimization algorithm
     *  that expects a VectorD => Double function.
     */
    val func: FunctionV2S = (params: VectorD) => objFunc(params)

end ModelOptimizer


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Common parameter bounds and defaults for traffic simulation calibration.
 *  Based on IDM literature (Treiber & Kesting, 2013) and empirical validation.
 */
object CalibrationDefaults:
    
    // IDM parameter order: [s, amax, bmax, T, τ]
    // Literature defaults: s₀=2m, a=1.0m/s², b=1.5m/s², T=1.5s, τ=0.6s
    
    /** Literature-standard starting point for IDM parameters */
    val literatureParams: VectorD = VectorD(2.0, 1.0, -1.5, 1.5, 0.6)
    
    /** Empirically validated starting point for CalRoute101 (from SPSA results) */
    val empiricalParams: VectorD = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)
    
    // Tight bounds centered on IDM empirical defaults (ANNSIM 2026 validated: s=5, a=4, b=-2, T=3, τ=0.5)
    // Also covers Gipps defaults (s=2, a=1.5, b=-3, T=1, τ=0.67)
    // Rationale: previous wide bounds (s up to 8, b up to -0.5) allowed physically unrealistic
    // parameter combinations that distorted flow counts. These tighter bounds keep the optimizer
    // in the physically meaningful region while still allowing meaningful speed improvement.
    //
    //   s₀ (min gap):      3.0 - 7.0 m    (±2m around IDM default=5; covers Gipps=2 at lower edge)
    //   amax (max accel):  1.0 - 5.0 m/s² (physical: 1-5 m/s²; IDM=4, Gipps=1.5)
    //   bmax (decel):     -3.5 - -0.8 m/s² (must brake meaningfully; IDM=-2, Gipps=-3)
    //   T (headway):       0.8 - 4.0 s    (literature: 0.8-4s; IDM=3, Gipps=1)
    //   τ (reaction):      0.3 - 1.2 s    (human reaction time: 0.3-1.2s; IDM=0.5, Gipps=0.67)

    /** Parameter lower bounds - tight, physically grounded (Feb 2026) */
    val lowerBounds: VectorD = VectorD(3.0, 1.0, -3.5, 0.8, 0.3)

    /** Parameter upper bounds - tight, physically grounded (Feb 2026) */
    val upperBounds: VectorD = VectorD(7.0, 5.0, -0.8, 4.0, 1.2)

    // Previous wide bounds (kept for reference, DO NOT reuse - caused flow degradation):
    // val lowerBounds: VectorD = VectorD(1.0, 0.8, -4.0, 0.8, 0.2)
    // val upperBounds: VectorD = VectorD(8.0, 6.0, -0.5, 5.0, 2.0)
    
    /** Parameter names for reporting */
    val paramNames: Array[String] = Array("s₀ (m)", "a (m/s²)", "b (m/s²)", "T (s)", "τ (s)")
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a bounded objective function that clamps parameters to valid ranges.
     *  Prevents optimizer from exploring physically meaningless parameter space.
     *  
     *  @param func   the original objective function
     *  @param lower  lower bounds for each parameter
     *  @param upper  upper bounds for each parameter
     *  @return       bounded objective function
     */
    def boundedObjective(func: VectorD => Double, 
                        lower: VectorD = lowerBounds, 
                        upper: VectorD = upperBounds): VectorD => Double =
        (params: VectorD) =>
            val clamped = VectorD(for i <- params.indices yield 
                math.max(lower(i), math.min(upper(i), params(i))))
            func(clamped)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clamp a parameter vector to [lowerBounds, upperBounds] per element.
     *  Used by GA wrapper to intercept mutated/crossed params before simulation.
     *  @param params  raw parameter vector (may be out of bounds after mutation)
     *  @return        clamped parameter vector guaranteed within physical bounds
     */
    def clamp(params: VectorD,
              lower: VectorD = lowerBounds,
              upper: VectorD = upperBounds): VectorD =
        VectorD(for i <- params.indices yield math.max(lower(i), math.min(upper(i), params(i))))

end CalibrationDefaults


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//  CALIBRATION ADAPTER FOR CalRoute101_3
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.simulation.process.model.CalRoute101_3
import scalation.simulation.process.config.SimulationReport

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Calibration adapter for CalRoute101_3 model.
 *  Implements CalibratableModel trait to enable optimization.
 */
class CalibrateCalRoute101_3 extends CalibratableModel:
    
    private var model: CalRoute101_3 = null
    
    def applyParameters(params: VectorD): Unit =
        Vehicle.setProps(Vehicle.setParams(params))
        model = new CalRoute101_3(animating = false)  // No animation during calibration
    end applyParameters
    
    def runSimulation(): Unit =
        model.simulate()
        model.waitFinished()
    end runSimulation
    
    def computeFitness(): Double =
        val report = SimulationReport.fromJunctions(
            model.getJunctions,
            model.getPemsSensorIndices,
            model.getDemandConfig
        )
        report.fitness()
    end computeFitness

    /** Get the simulation report for detailed output/export */
    def getReport(): SimulationReport =
        SimulationReport.fromJunctions(
            model.getJunctions,
            model.getPemsSensorIndices,
            model.getDemandConfig
        )
    end getReport

    /** Get the car-following model name (idm, gipps, krause) */
    def getModelName(): String =
        model.getDynamicsConfig.carFollowing.toString.toLowerCase
    end getModelName

    /** Shutdown the model */
    def shutDown(): Unit = Model.shutdown()

end CalibrateCalRoute101_3


@main def eval (): Unit =
    banner ("EVALUATION RUN - CalRoute101_3 (Unoptimized)")
    
    // Get the dynamics config to determine which model and its default params
    val builder = TrafficModelBuilder.US101_DonaldDoyle_PeMS()
    val dynamicsConfig = builder.getDynamics
    val modelName = dynamicsConfig.carFollowing.toString.toLowerCase
    
    // Use model-appropriate default parameters from DynamicsConfig
    val defaultParams = dynamicsConfig.cfParams.toVector
    println (s"Model: $modelName")
    println (s"Using model-specific parameters: $defaultParams")
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create CalRoute101_3 model adapter (implements CalibratableModel trait) */
    val modelAdapter = new CalibrateCalRoute101_3()

    println (s"Applying parameters: $defaultParams")
    modelAdapter.applyParameters(defaultParams)

    println ("Running simulation...")
    modelAdapter.runSimulation()

    println ("Computing fitness...")
    val fitness = modelAdapter.computeFitness()
    println (s"Fitness value: $fitness")


    // Get report and export CSV and TXT (like old unoptimized run)
    println (s"\nGenerating report and exporting files for model: $modelName...")
    val report = modelAdapter.getReport()
    report.report()
    report.exportCSV(s"unoptimized_${modelName}_data.csv")
    report.exportReport(s"unoptimized_${modelName}_report.txt")

    println (s"\nThe fitness value is: $fitness")
    modelAdapter.shutDown()

end eval

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//  OPTIMIZER RUNNERS - Live implementations for CalRoute101_3
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.optimization.{SPSA, SPSA_Mo, NelderMeadSimplex2, DifferentialEvolution, GeneticAlgorithm, GSRS_TTS}
import scalation.random.{Uniform, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run SPSA Optimizer
 *  > runMain scalation.simulation.process.builder.runCalibrate_SPSA
 */
@main def runCalibrate_SPSA(): Unit =
    banner("SPSA OPTIMIZER - CalRoute101_3 Calibration")

    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer = new ModelOptimizer(modelAdapter)
    
    val params = CalibrationDefaults.empiricalParams

    val spsaOptimizer = new SPSA(optimizer.func, 20, checkCon = true,
                                  lower = CalibrationDefaults.lowerBounds,
                                  upper = CalibrationDefaults.upperBounds)
    spsaOptimizer.setVerbose(1)
    spsaOptimizer.setPrintEvery(5)

    val (result, duration) = timed { spsaOptimizer.solve(params) }

    println(s"Best Fitness: ${result._1}")
    println(s"Best Parameters: ${result._2}")
    println(f"Duration: $duration%.2f seconds")
    
    // Generate final report with optimized parameters
    println("\nGenerating final report with optimized parameters...")
    val finalAdapter = new CalibrateCalRoute101_3()
    finalAdapter.applyParameters(result._2)
    finalAdapter.runSimulation()
    val report = finalAdapter.getReport()
    report.report()
    report.exportCSV("spsa_optimized_data.csv")
    report.exportReport("spsa_optimized_report.txt")
    println("Exported: spsa_optimized_data.csv, spsa_optimized_report.txt")

    Model.shutdown()
end runCalibrate_SPSA


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run SPSA_Mo Optimizer (momentum variant)
 *  > runMain scalation.simulation.process.builder.runCalibrate_SPSA_Mo
 */
@main def runCalibrate_SPSA_Mo(): Unit =
    banner("SPSA_MO OPTIMIZER - CalRoute101_3 Calibration")
    
    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer = new ModelOptimizer(modelAdapter)
    
    val params = CalibrationDefaults.empiricalParams

    val spsa_mo_Optimizer = new SPSA_Mo(optimizer.func, 70, checkCon = true,
                                         lower = CalibrationDefaults.lowerBounds,
                                         upper = CalibrationDefaults.upperBounds)
    spsa_mo_Optimizer.setVerbose(1)
    spsa_mo_Optimizer.setPrintEvery(5)

    val (result, duration) = timed { spsa_mo_Optimizer.solve(params) }

    println(f"Best Fitness: ${result._1}%.4f")
    println(f"Best Parameters: ${result._2}")
    println(f"Duration: ${duration}%.2f seconds")

    println("\nGenerating final report with optimized parameters...")
    val finalAdapter = new CalibrateCalRoute101_3()
    finalAdapter.applyParameters(result._2)
    finalAdapter.runSimulation()
    val report = finalAdapter.getReport()
    report.report()
    report.exportCSV("spsa_mo_optimized_data.csv")
    report.exportReport("spsa_mo_optimized_report.txt")
    println("Exported: spsa_mo_optimized_data.csv, spsa_mo_optimized_report.txt")

    Model.shutdown()
end runCalibrate_SPSA_Mo


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run Nelder-Mead Simplex Optimizer
 *  > runMain scalation.simulation.process.builder.runCalibrate_NelderMead
 */
@main def runCalibrate_NelderMead(): Unit =
    banner("NELDER-MEAD SIMPLEX OPTIMIZER - CalRoute101_3 Calibration")
    
    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer = new ModelOptimizer(modelAdapter)
    
    val params = CalibrationDefaults.empiricalParams
    val boundedFunc = CalibrationDefaults.boundedObjective(optimizer.func)

    val nelderMeadOptimizer = new NelderMeadSimplex2(boundedFunc, params.dim)

    val (result, duration) = timed { nelderMeadOptimizer.solve(params) }

    println(s"Best Fitness: ${result._1}")
    println(s"Best Parameters: ${result._2}")
    println(f"Duration: $duration%.2f seconds")

    println("\nGenerating final report with optimized parameters...")
    val finalAdapter = new CalibrateCalRoute101_3()
    finalAdapter.applyParameters(result._2)
    finalAdapter.runSimulation()
    val report = finalAdapter.getReport()
    report.report()
    report.exportCSV("neldermead_optimized_data.csv")
    report.exportReport("neldermead_optimized_report.txt")
    println("Exported: neldermead_optimized_data.csv, neldermead_optimized_report.txt")

    Model.shutdown()
end runCalibrate_NelderMead


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run Differential Evolution Optimizer
 *  > runMain scalation.simulation.process.builder.runCalibrate_DifferentialEvolution
 */
@main def runCalibrate_DifferentialEvolution(): Unit =
    banner("DIFFERENTIAL EVOLUTION OPTIMIZER - CalRoute101_3 Calibration")
    
    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer = new ModelOptimizer(modelAdapter)
    
    val params = CalibrationDefaults.empiricalParams
    val boundedFunc = CalibrationDefaults.boundedObjective(optimizer.func)
    
    // Bounds for DE search space (expanded Feb 2026)
    val bounds = (0.2, 8.0)

    // 40 generations × 20 population = 800 evaluations (~24 hrs)
    val (result, duration) = timed { DifferentialEvolution.optimize(
            boundedFunc, params.dim, bounds,
            maxGen = 40, F = 0.8, CR = 0.9)(popSize = 20)
    }
    
    println(s"Best Fitness: ${result._2}")
    println(s"Best Parameters: ${result._1}")
    println(s"Total Duration: $duration seconds")

    println("\nGenerating final report with optimized parameters...")
    val finalAdapter = new CalibrateCalRoute101_3()
    finalAdapter.applyParameters(result._1)
    finalAdapter.runSimulation()
    val report = finalAdapter.getReport()
    report.report()
    report.exportCSV("de_optimized_data.csv")
    report.exportReport("de_optimized_report.txt")
    println("Exported: de_optimized_data.csv, de_optimized_report.txt")

    Model.shutdown()
end runCalibrate_DifferentialEvolution


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run Genetic Algorithm Optimizer
 *  > runMain scalation.simulation.process.builder.runCalibrate_GA
 */
@main def runCalibrate_GA(): Unit =
    banner("GENETIC ALGORITHM OPTIMIZER - CalRoute101_3 Calibration")
    
    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer = new ModelOptimizer(modelAdapter)
    
    // Tight bounds matching CalibrationDefaults (Feb 2026) - centered on ANNSIM validated params
    val randVars: Array[Variate] = Array(
        Uniform(3.0, 7.0),    // s₀: min gap (3-7 m, centered on IDM default=5)
        Uniform(1.0, 5.0),    // a: max acceleration (1-5 m/s²)
        Uniform(-3.5, -0.8),  // b: comfortable deceleration (-3.5 to -0.8 m/s²)
        Uniform(0.8, 4.0),    // T: safe time headway (0.8-4.0 s)
        Uniform(0.3, 1.2)     // τ: reaction time (0.3-1.2 s)
    )

    // CRITICAL: wrap f so every GA evaluation (mutate, crossover, fresh) is clamped.
    // GA.mutate() does x(i) *= (1 ± 0.2) with no internal bounds check.
    // Wrapping here is the only safe interception point.
    val boundedFunc: FunctionV2S = (params: VectorD) =>
        optimizer.func(CalibrationDefaults.clamp(params))

    // 40 generations × 20 population = 800 evaluations (~24 hrs)
    val gaOptimizer = new GeneticAlgorithm(boundedFunc, randVars, maxGen = 40, popSize = 20)

    val (result, duration) = timed { gaOptimizer.solve2() }

    println(f"Best Fitness: ${result._1}%.4f")
    println(f"Best Parameters: ${result._2}")
    println(f"Duration: ${duration}%.2f seconds")

    println("\nGenerating final report with optimized parameters...")
    val finalAdapter = new CalibrateCalRoute101_3()
    finalAdapter.applyParameters(result._2)
    finalAdapter.runSimulation()
    val report = finalAdapter.getReport()
    report.report()
    report.exportCSV("ga_optimized_data.csv")
    report.exportReport("ga_optimized_report.txt")
    println("Exported: ga_optimized_data.csv, ga_optimized_report.txt")

    Model.shutdown()
end runCalibrate_GA


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run GSRS_TTS Optimizer (Two-Time-Scale Gaussian Smooth Random Search)
 *  > runMain scalation.simulation.process.builder.runCalibrate_GSRS_TTS
 */
@main def runCalibrate_GSRS_TTS(): Unit =
    banner("GSRS_TTS OPTIMIZER - CalRoute101_3 Calibration")

    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer    = new ModelOptimizer(modelAdapter)

    val params = CalibrationDefaults.empiricalParams

    val gsrsTTSOptimizer = new GSRS_TTS(optimizer.func, 20, checkCon = true,
                                         lower = CalibrationDefaults.lowerBounds,
                                         upper = CalibrationDefaults.upperBounds)
    gsrsTTSOptimizer.setVerbose(1)
    gsrsTTSOptimizer.setPrintEvery(5)

    val (result, duration) = timed { gsrsTTSOptimizer.solve(params) }

    println(s"Best Fitness: ${result._1}")
    println(s"Best Parameters: ${result._2}")
    println(f"Duration: $duration%.2f seconds")

    println("\nGenerating final report with optimized parameters...")
    val finalAdapter = new CalibrateCalRoute101_3()
    finalAdapter.applyParameters(result._2)
    finalAdapter.runSimulation()
    val report = finalAdapter.getReport()
    report.report()
    report.exportCSV("gsrs_tts_optimized_data.csv")
    report.exportReport("gsrs_tts_optimized_report.txt")
    println("Exported: gsrs_tts_optimized_data.csv, gsrs_tts_optimized_report.txt")

    Model.shutdown()
end runCalibrate_GSRS_TTS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run SPSA and GSRS_TTS sequentially — for local comparison runs.
 *  Both use 20 iterations. Leave running, come back to both results.
 *  > runMain scalation.simulation.process.builder.runCalibrate_Compare
 */
@main def runCalibrate_Compare(): Unit =

    val params = CalibrationDefaults.empiricalParams

    // ── SPSA ──────────────────────────────────────────────────────────────────
    banner("ROUND 1 — SPSA (20 iterations)")
    val spsa_adapter  = new CalibrateCalRoute101_3()
    val spsa_opt      = new ModelOptimizer(spsa_adapter)
    val spsaOptimizer = new SPSA(spsa_opt.func, 20, checkCon = true,
                                  lower = CalibrationDefaults.lowerBounds,
                                  upper = CalibrationDefaults.upperBounds)
    spsaOptimizer.setVerbose(1)
    spsaOptimizer.setPrintEvery(5)

    val (spsaResult, spsaDuration) = timed { spsaOptimizer.solve(params) }

    println(f"\nSPSA  | Best Fitness: ${spsaResult._1}%.4f | Duration: $spsaDuration%.2f s")
    println(s"SPSA  | Best Params:  ${spsaResult._2}")

    val spsaAdapter2 = new CalibrateCalRoute101_3()
    spsaAdapter2.applyParameters(spsaResult._2)
    spsaAdapter2.runSimulation()
    val spsaReport = spsaAdapter2.getReport()
    spsaReport.report()
    spsaReport.exportCSV("spsa_compare_data.csv")
    spsaReport.exportReport("spsa_compare_report.txt")
    println("Exported: spsa_compare_data.csv, spsa_compare_report.txt")
    Model.shutdown()                                                   // shutdown SPSA model before starting GSRS_TTS

    // ── GSRS_TTS ──────────────────────────────────────────────────────────────
    banner("ROUND 2 — GSRS_TTS (20 iterations)")
    val tts_adapter      = new CalibrateCalRoute101_3()
    val tts_opt          = new ModelOptimizer(tts_adapter)
    val gsrsTTSOptimizer = new GSRS_TTS(tts_opt.func, 20, checkCon = true,
                                         lower = CalibrationDefaults.lowerBounds,
                                         upper = CalibrationDefaults.upperBounds)
    gsrsTTSOptimizer.setVerbose(1)
    gsrsTTSOptimizer.setPrintEvery(5)

    val (ttsResult, ttsDuration) = timed { gsrsTTSOptimizer.solve(params) }

    println(f"\nGSRS_TTS | Best Fitness: ${ttsResult._1}%.4f | Duration: $ttsDuration%.2f s")
    println(s"GSRS_TTS | Best Params:  ${ttsResult._2}")

    val ttsAdapter2 = new CalibrateCalRoute101_3()
    ttsAdapter2.applyParameters(ttsResult._2)
    ttsAdapter2.runSimulation()
    val ttsReport = ttsAdapter2.getReport()
    ttsReport.report()
    ttsReport.exportCSV("gsrs_tts_compare_data.csv")
    ttsReport.exportReport("gsrs_tts_compare_report.txt")
    println("Exported: gsrs_tts_compare_data.csv, gsrs_tts_compare_report.txt")
    Model.shutdown()                                                   // shutdown GSRS_TTS model before summary

    // ── SUMMARY ───────────────────────────────────────────────────────────────
    banner("COMPARISON SUMMARY")
    println(f"${"Algorithm"}%-12s | ${"Best Fitness"}%12s | ${"Duration (s)"}%12s | Best Params")
    println("-" * 80)
    println(f"${"SPSA"}%-12s | ${spsaResult._1}%12.4f | $spsaDuration%12.2f | ${spsaResult._2}")
    println(f"${"GSRS_TTS"}%-12s | ${ttsResult._1}%12.4f | $ttsDuration%12.2f | ${ttsResult._2}")
    println("-" * 80)
    val winner = if spsaResult._1 < ttsResult._1 then "SPSA" else "GSRS_TTS"
    println(s"Winner: $winner")

end runCalibrate_Compare


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Example usage with CalRoute101_3 - showing how to create a calibratable adapter.
 *  
 *  Usage pattern:
 *    {{{
 *    import scalation.simulation.process.builder._
 *    import scalation.simulation.process.config._
 *    import scalation.optimization.SPSA
 *    
 *    // 1. Create demand configuration (decoupled from any specific corridor)
 *    val demand = PeMSDemand(
 *        corridorName = "Donald Doyle",
 *        dataDirectory = "Mainline_VDS_Donald_Doyle",
 *        anchorFile = "1-401112ML.csv",
 *        nMainlineSensors = 5,
 *        mainlineFiles = List("1-401112ML.csv", "2-401104ML.csv", ...),
 *        rampDirectory = "Ramps_VDS_Donald_Doyle",
 *        rampFiles = List("1-403157OR.csv", "2-403108OR.csv")
 *    )
 *    
 *    // 2. Create calibratable model adapter
 *    class CalibrateCalRoute101_3(demand: PeMSDemand) extends CalibratableModel:
 *        private var model: CalRoute101_3 = null
 *        
 *        def applyParameters(params: VectorD): Unit =
 *            Vehicle.setProps(Vehicle.setParams(params))
 *            model = new CalRoute101_3(demand)  // Re-instantiate with new vehicle params
 *        end applyParameters
 *        
 *        def runSimulation(): Unit =
 *            model.simulate()
 *        end runSimulation
 *        
 *        def computeFitness(): Double =
 *            // Use SimulationReport for validation
 *            val flowData = model.junc.map(_.getRecorderMat._1)
 *            val speedData = model.junc.map(_.getRecorderMat._2)
 *            val report = new SimulationReport(flowData, speedData, demand)
 *            report.fitness()  // Returns combined fitness metric
 *        end computeFitness
 *    end CalibrateCalRoute101_3
 *    
 *    // 3. Run optimization
 *    val modelAdapter = new CalibrateCalRoute101_3(demand)
 *    val optimizer = new ModelOptimizer(modelAdapter)
 *    
 *    // Use bounded objective to keep parameters physically meaningful
 *    val boundedFunc = CalibrationDefaults.boundedObjective(optimizer.func)
 *    
 *    // Run SPSA optimization
 *    val params = CalibrationDefaults.empiricalParams
 *    val spsaOpt = new SPSA(boundedFunc, maxIter = 100)
 *    val (bestFitness, bestParams) = spsaOpt.solve(params)
 *    
 *    println(s"Best Fitness: $bestFitness")
 *    println(s"Best Parameters: $bestParams")
 *    
 *    // 4. Switch corridors by changing ONE line
 *    val newDemand = PeMSDemand(
 *        corridorName = "Redwood Creek",
 *        dataDirectory = "Mainline_VDS_Redwood_Creek_US101-N",
 *        anchorFile = "1-404531ML.csv",
 *        ...
 *    )
 *    val newAdapter = new CalibrateCalRoute101_3(newDemand)  // Everything else stays the same!
 *    }}}
 */


//Two models IDM and Gipps.
//SPSA, SPSA_Mo, Nelder-Mead, Differential Evolution, (Genetic Algorithm ??)
//Each lane validation
//A typical weighting that works well:
//
//AM peak: 40%
//
//Midday: 20%
//
//PM peak: 40%
//warm start
//fitness = 0.5 * (SMAPE_flow + SMAPE_speed) + 0.5


//// l1  prob1 l2 prob2  l3 prob3  l4
//    1        2          3       4
//    1        2          3       4
//    0        3          2       5  (lane change)  // I can determine that the lane change happenned @ 1-2 and 3-4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//  UNIFIED HPC ARRAY JOB ENTRY POINT
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Unified entry point for SLURM array jobs.
 *  Dispatches to the correct (model, optimizer, fitnessMode) combination based on array index.
 *
 *  Array Index Mapping (1-12, MACRO only):
 *  | Index | Model  | Optimizer      | Fitness |
 *  |-------|--------|----------------|---------|
 *  |   1   | IDM    | SPSA           | MACRO   |
 *  |   2   | IDM    | SPSA_Mo        | MACRO   |
 *  |   3   | IDM    | NelderMead     | MACRO   |
 *  |   4   | IDM    | GA             | MACRO   |
 *  |   5   | IDM    | DE             | MACRO   |
 *  |   6   | IDM    | GSRS_TTS       | MACRO   |
 *  |   7   | Gipps  | SPSA           | MACRO   |
 *  |   8   | Gipps  | SPSA_Mo        | MACRO   |
 *  |   9   | Gipps  | NelderMead     | MACRO   |
 *  |  10   | Gipps  | GA             | MACRO   |
 *  |  11   | Gipps  | DE             | MACRO   |
 *  |  12   | Gipps  | GSRS_TTS       | MACRO   |
 *
 *  Micro runs dropped - macro is aligned with ANNSIM 2026 paper baseline.
 *  Fitness: 0.2*flowNRMSE + 0.8*speedNRMSE + flowPenalty(threshold=2.5)
 *
 *  Usage on HPC:
 *    #SBATCH --array=1-10
 *    java -Xmx28G -jar scalation-assembly.jar
 *
 *  > runMain scalation.simulation.process.builder.runCalibrationArrayJob
 */
@main def runCalibrationArrayJob(): Unit =
    // Get array index from SLURM environment variable, default to 1 for local testing
    val arrayIndex = sys.env.getOrElse("SLURM_ARRAY_TASK_ID", "1").toInt

    // Model and optimizer arrays - IDM and Gipps only, all MACRO (jobs 1-10)
    val models         = Array(CarFollowingModel.IDM, CarFollowingModel.Gipps)
    val modelNames     = Array("idm", "gipps")
    val optimizerNames = Array("spsa", "spsa_mo", "neldermead", "ga", "de", "gsrs_tts")

    // Decode: jobs 1-6 = IDM, jobs 7-12 = Gipps, optimizer cycles 0-5
    val modelIndex     = (arrayIndex - 1) / 6     // 0 = IDM, 1 = Gipps
    val optimizerIndex = (arrayIndex - 1) % 6     // 0-5

    val model         = models(modelIndex)
    val modelName     = modelNames(modelIndex)
    val optimizerName = optimizerNames(optimizerIndex)
    val fitnessMode   = "macro"                   // MACRO only - aligned with ANNSIM 2026

    banner(s"CALIBRATION ARRAY JOB - Task $arrayIndex")
    println(s"Model:        $modelName")
    println(s"Optimizer:    $optimizerName")
    println(s"Fitness Mode: $fitnessMode (corridor-level)")
    println(s"Timestamp:    ${java.time.LocalDateTime.now()}")
    println(s"Host:         ${java.net.InetAddress.getLocalHost.getHostName}")
    println("=" * 60)

    // Set fitness mode for SimulationReport
    System.setProperty("FITNESS_MODE", fitnessMode)

    // Get model-specific default parameters (IDM and Gipps only)
    val defaultParams = model match
        case CarFollowingModel.IDM    => CFParams.idmDefault
        case CarFollowingModel.Gipps  => CFParams.gippsDefault
        case _ => throw new IllegalArgumentException(s"Unsupported model: $model")

    // Configure dynamics for this model
    val dynamicsConfig = DynamicsConfig(
        carFollowing = model,
        odeSolver = ODESolverType.Ballistic,
        cfParams = defaultParams
    )

    // Set global dynamics config (used by TrafficModelBuilder)
    TrafficModelBuilder.setDynamicsConfig(dynamicsConfig)

    println(s"Starting parameters: ${defaultParams.toVector}")

    // Run the appropriate optimizer
    val (bestFitness, bestParams, duration) = optimizerName match
        case "spsa"       => runSPSAOptimizer(defaultParams.toVector)
        case "spsa_mo"    => runSPSA_MoOptimizer(defaultParams.toVector)
        case "neldermead" => runNelderMeadOptimizer(defaultParams.toVector)
        case "ga"         => runGAOptimizer()
        case "de"         => runDEOptimizer(defaultParams.toVector)
        case "gsrs_tts"   => runGSRS_TTSOptimizer(defaultParams.toVector)

    // Generate final report with optimized parameters
    println("\n" + "=" * 60)
    println("GENERATING FINAL REPORT WITH OPTIMIZED PARAMETERS")
    println("=" * 60)

    val finalAdapter = new CalibrateCalRoute101_3()
    finalAdapter.applyParameters(bestParams)
    finalAdapter.runSimulation()
    val report = finalAdapter.getReport()
    report.report()

    // Export with model, optimizer, and fitness mode prefix
    val csvFile = s"${modelName}_${optimizerName}_${fitnessMode}_optimized_data.csv"
    val txtFile = s"${modelName}_${optimizerName}_${fitnessMode}_optimized_report.txt"
    report.exportCSV(csvFile)
    report.exportReport(txtFile)

    println(s"\nExported: $csvFile, $txtFile")
    println(s"Best Fitness: $bestFitness")
    println(s"Best Parameters: $bestParams")
    println(f"Total Duration: $duration%.2f seconds")

    Model.shutdown()
    println(s"\n=== Job $arrayIndex ($modelName + $optimizerName + $fitnessMode) completed at ${java.time.LocalDateTime.now()} ===")
    
    // Force JVM exit to prevent hanging on background threads
    System.exit(0)
end runCalibrationArrayJob


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//  OPTIMIZER HELPER FUNCTIONS FOR ARRAY JOB
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

/** Run SPSA optimizer, returns (bestFitness, bestParams, durationSec) */
private def runSPSAOptimizer(startParams: VectorD): (Double, VectorD, Double) =
    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer = new ModelOptimizer(modelAdapter)

    val spsaOptimizer = new SPSA(optimizer.func, 100, checkCon = true,
                                  lower = CalibrationDefaults.lowerBounds,
                                  upper = CalibrationDefaults.upperBounds)
    spsaOptimizer.setVerbose(1)
    spsaOptimizer.setPrintEvery(5)

    val (result, duration) = timed { spsaOptimizer.solve(startParams) }
    (result._1, result._2, duration)
end runSPSAOptimizer

/** Run SPSA_Mo optimizer, returns (bestFitness, bestParams, durationSec) */
private def runSPSA_MoOptimizer(startParams: VectorD): (Double, VectorD, Double) =
    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer = new ModelOptimizer(modelAdapter)

    val spsaMoOptimizer = new SPSA_Mo(optimizer.func, 100, checkCon = true,
                                       lower = CalibrationDefaults.lowerBounds,
                                       upper = CalibrationDefaults.upperBounds)
    spsaMoOptimizer.setVerbose(1)
    spsaMoOptimizer.setPrintEvery(5)

    val (result, duration) = timed { spsaMoOptimizer.solve(startParams) }
    (result._1, result._2, duration)
end runSPSA_MoOptimizer

/** Run Nelder-Mead optimizer, returns (bestFitness, bestParams, durationSec) */
private def runNelderMeadOptimizer(startParams: VectorD): (Double, VectorD, Double) =
    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer = new ModelOptimizer(modelAdapter)
    val boundedFunc = CalibrationDefaults.boundedObjective(optimizer.func)

    val nmOptimizer = new NelderMeadSimplex2(boundedFunc, startParams.dim)

    val (result, duration) = timed { nmOptimizer.solve(startParams) }
    (result._1, result._2, duration)
end runNelderMeadOptimizer

/** Run Genetic Algorithm optimizer, returns (bestFitness, bestParams, durationSec) */
private def runGAOptimizer(): (Double, VectorD, Double) =
    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer = new ModelOptimizer(modelAdapter)

    // Tight bounds matching CalibrationDefaults - centered on ANNSIM validated params
    // NOTE: previous wide bounds (s 1-8, b -4 to -0.5) caused GA to wander into
    // physically unrealistic regions (e.g. b≈0, s>7) which degraded flow counts
    val randVars: Array[Variate] = Array(
        Uniform(3.0, 7.0),    // s₀: min gap (3-7 m, centered on IDM default=5)
        Uniform(1.0, 5.0),    // a: max acceleration (1-5 m/s²)
        Uniform(-3.5, -0.8),  // b: comfortable deceleration (-3.5 to -0.8 m/s²)
        Uniform(0.8, 4.0),    // T: safe time headway (0.8-4.0 s)
        Uniform(0.3, 1.2)     // τ: reaction time (0.3-1.2 s)
    )

    // CRITICAL: Wrap objective function with per-evaluation clamp.
    // GA.mutate() applies a multiplicative factor (1 ± 0.2) AFTER crossover with
    // no bounds check - every evaluation inside GA must be clamped before the
    // simulation runs, not just the final result.
    val boundedFunc: FunctionV2S = (params: VectorD) =>
        optimizer.func(clampParams(params))

    // 40 generations × 20 population = 800 evaluations (~24 hrs)
    val gaOptimizer = new GeneticAlgorithm(boundedFunc, randVars, maxGen = 40, popSize = 20)

    val (result, duration) = timed { gaOptimizer.solve2() }

    // Also clamp final result (belt-and-suspenders)
    val clampedResult = clampParams(result._2)
    (result._1, clampedResult, duration)
end runGAOptimizer

/** Run Differential Evolution optimizer, returns (bestFitness, bestParams, durationSec) */
private def runDEOptimizer(startParams: VectorD): (Double, VectorD, Double) =
    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer = new ModelOptimizer(modelAdapter)
    
    // Use bounded objective function to clamp parameters to valid physical ranges
    // This prevents DE from exploring nonsensical parameter space (e.g., negative s0, positive bmax)
    val boundedFunc = CalibrationDefaults.boundedObjective(optimizer.func)
    
    // Bounds for DE search space - slightly wider than physical bounds to allow exploration
    // Parameters: [s0, amax, bmax, T, tau]
    // Tight bounds (Feb 2026): s0(3-7), amax(1-5), bmax(-3.5 to -0.8), T(0.8-4), tau(0.3-1.2)
    // boundedFunc clamps to exact CalibrationDefaults ranges before evaluation
    val bounds = (0.3, 7.0)  // outer envelope - boundedFunc clamps to exact per-param ranges

    // 40 generations × 20 population = 800 evaluations (~24 hrs)
    val (result, duration) = timed {
        DifferentialEvolution.optimize(
            boundedFunc, startParams.dim, bounds,
            maxGen = 40, F = 0.8, CR = 0.9)(popSize = 20)
    }

    // Clamp final result to ensure it's within valid bounds
    val clampedResult = clampParams(result._1)
    (result._2, clampedResult, duration)
end runDEOptimizer

/** Run GSRS_TTS optimizer, returns (bestFitness, bestParams, durationSec) */
private def runGSRS_TTSOptimizer(startParams: VectorD): (Double, VectorD, Double) =
    val modelAdapter = new CalibrateCalRoute101_3()
    val optimizer    = new ModelOptimizer(modelAdapter)

    val gsrsTTSOptimizer = new GSRS_TTS(optimizer.func, 100, checkCon = true,
                                         lower = CalibrationDefaults.lowerBounds,
                                         upper = CalibrationDefaults.upperBounds)
    gsrsTTSOptimizer.setVerbose(1)
    gsrsTTSOptimizer.setPrintEvery(5)

    val (result, duration) = timed { gsrsTTSOptimizer.solve(startParams) }
    (result._1, result._2, duration)
end runGSRS_TTSOptimizer

/** Clamp parameters to valid bounds — used by GA and DE final results */
private def clampParams(params: VectorD): VectorD =
    VectorD(for i <- params.indices yield
        math.max(CalibrationDefaults.lowerBounds(i),
                 math.min(CalibrationDefaults.upperBounds(i), params(i))))
end clampParams



