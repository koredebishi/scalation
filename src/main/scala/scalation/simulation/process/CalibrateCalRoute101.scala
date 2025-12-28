package scalation
package simulation
package process

import scalation.mathstat.{MatrixD, VectorD}
import scalation.modeling.Fit
import scalation.optimization.{DifferentialEvolution, GeneticAlgorithm, NelderMeadSimplex2, SPSA, SPSA_Mo}
import scalation.random.{Uniform, Variate}
import scalation.simulation.process.example_1.{CalRoute101_2}


val easyW = new EasyWriter("simulation", "Home_14-CalibrateCalRoute101_2.txt")
// Parameter order: [s, amax, bmax, T, rt] - matches Vehicle.setParams
// Good def_prop values: s=5.0, amax=4.0, bmax=-2.0, T=3.0, rt=0.5
val params: VectorD = VectorD(5.0, 4.0, -2.0, 3.0, 0.5) // shared params

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Adapter to make CalRoute101 compatible with CalibratableModel trait.
 *  Encapsulates CalRoute101-specific parameter mapping and fitness computation.
 *  Follows scalation 2.0 trait mixing pattern (extends/with).
 */
class CalibrateCalRoute101 extends CalibratableModel:

    private var model: CalRoute101_2 = null


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply IDM parameters to Vehicle singleton and create model instance.
     *  @param params  [s, amax, bmax, T, τ] - IDM vehicle behavior parameters
     */
    def applyParameters(params: VectorD): Unit =
        // Map parameter vector to Vehicle properties
        Vehicle.setProps(Vehicle.setParams(params))       // The parameters to be optimized from the vehicle
        // Create new model instance with updated Vehicle settings
        model = new CalRoute101_2()       // Re-instantiate model to apply new vehicle properties
    end applyParameters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the CalRoute101 simulation */
    def runSimulation(): Unit =
        model.simulate()  // Run the simulation

    end runSimulation

    def shutDown(): Unit =
        Model.shutdown()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute fitness value for optimization.
     *  Returns weighted average SMAPE across all 5 sensors for both counts and speeds.
     *  Lower values = better fit.
     *  @return fitness value (weighted SMAPE) for optimizer to minimize
     */
    def computeFitness(): Double =
        val mats = for s <- model.junc yield s.getRecorderMat
        val simCounts = mats.map(_._1)
        val simSpeeds = mats.map(_._2)

        // Name unification: sensor = PEMS data, simSensor = simulation data
        val pemsSensorIndices = model.pemsToJunc
        val simSensor_counts = pemsSensorIndices.map(i => simCounts(i))
        val simSensor_speeds = pemsSensorIndices.map(i => simSpeeds(i))  // already in m/s

        // Load PEMS data (from CSV files) - use object method, not instance
        val pemsData: IndexedSeq[(MatrixD, MatrixD)] =
            (0 until 5).map(i => TrafficConfig2.getPemsCountMatrix(i))   // 5 sensors
        val sensor_counts = pemsData.map(_._1)
        val sensor_speeds = pemsData.map(_._2)  // already converted to m/s in getPemsCountMatrix

        // Fit diagnostics setup
        val nt = sensor_counts(0).dim  // number of time intervals from data
        val nparams = params.length
        object TestFit extends Fit(dfr = nparams, df = nt - nparams)

        // Compute metrics for all 5 sensors
        var totalCountSMAPE = 0.0
        var totalSpeedSMAPE = 0.0
        var totalCountRMSE = 0.0
        var totalSpeedRMSE = 0.0

        for i <- 0 until 5 do
            val cqof = TestFit.diagnose_mat(sensor_counts(i), simSensor_counts(i))
            val sqof = TestFit.diagnose_mat(sensor_speeds(i), simSensor_speeds(i))

            // Extract metrics using matrix indexing (rmse=6, smape=8)
            totalCountSMAPE += cqof(8, 0)
            totalSpeedSMAPE += sqof(8, 0)
            totalCountRMSE += cqof(6, 0)
            totalSpeedRMSE += sqof(6, 0)

            // Print individual sensor results
            println(s"Sensor $i: Count RMSE=${cqof(6,0)}, Count SMAPE=${cqof(8,0)}, Speed RMSE=${sqof(6,0)}, Speed SMAPE=${sqof(8,0)}")
        end for

        // Compute averages
        val avgCountSMAPE = totalCountSMAPE / 5.0
        val avgSpeedSMAPE = totalSpeedSMAPE / 5.0
        val avgCountRMSE = totalCountRMSE / 5.0
        val avgSpeedRMSE = totalSpeedRMSE / 5.0

        // Print summary
        println("\n" + "=" * 60)
        println("SUMMARY (Average across 5 sensors)")
        println("=" * 60)
        println(f"Counts - Avg RMSE: $avgCountRMSE%.4f, Avg SMAPE: $avgCountSMAPE%.2f")
        println(f"Speeds - Avg RMSE: $avgSpeedRMSE%.4f, Avg SMAPE: $avgSpeedSMAPE%.2f")
        println("=" * 60)

        // Return fitness value for optimization
        // Weighted combination of count and speed SMAPE
        val countWeight = 0.5
        val speedWeight = 0.5
        val fitness = countWeight * avgCountSMAPE + speedWeight * avgSpeedSMAPE

         println(f"FITNESS (${countWeight}*countSMAPE + ${speedWeight}*speedSMAPE): $fitness%.4f")


        fitness
    end computeFitness


end CalibrateCalRoute101



@main def eval (): Unit =
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create CalRoute101 model adapter (implements CalibratableModel trait) */
    val modelAdapter = new CalibrateCalRoute101()

    modelAdapter.applyParameters(params)

    modelAdapter.runSimulation()

    val fitness = modelAdapter.computeFitness()

    println(s"The fitness value is : $fitness")
    modelAdapter.shutDown()

end eval

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run SPSA Optimizer Only - For SLURM Job Array
 *  > runMain scalation.simulation.process.runCalibrate_SPSA
 */
@main def runCalibrate_SPSA(): Unit =
    banner("SPSA OPTIMIZER - CalRoute101 Calibration")

    val modelAdapter = new CalibrateCalRoute101()
    val simOpt = new TrafficOptimization(modelAdapter)
    modelAdapter.applyParameters(params)
    //val params: VectorD = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)
    println(s"Initial parameters: $params")

    val spsaOptimizer = new SPSA(simOpt.func, 20)
    val startTime = System.currentTimeMillis()
    val result    = spsaOptimizer.solve(params)
    val endTime   = System.currentTimeMillis()
    val duration  = (endTime - startTime) / 1000.0

    println(s"Best Fitness : ${result._1}")
    println(s"Best Parameters: ${result._2}")
    Model.shutdown()
end runCalibrate_SPSA




@main def runCalibrate_SPSA_Mo(): Unit =
    banner("SPSA_MO OPTIMIZER - CalRoute101 Calibration")

    val modelAdapter = new CalibrateCalRoute101()
    val simOpt = new TrafficOptimization(modelAdapter)
    val params: VectorD = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)

    println(f"Initial parameters: $params")

    val spsaOptimizer = new SPSA_Mo(simOpt.func, 20)
    val startTime = System.currentTimeMillis()
    val result = spsaOptimizer.solve(params)
    val endTime = System.currentTimeMillis()
    val duration = (endTime - startTime) / 1000.0

    println(f"Best Fitness: ${result._1}%.4f")
    println(f"Best Parameters: ${result._2}")
    println(f"Duration: ${duration}%.2f seconds")

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
    val params: VectorD = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)

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
    val params: VectorD = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)

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


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run Genetic Algorithm Optimizer Only - For SLURM Job Array
 *  > runMain scalation.simulation.process.runCalibrate_GA
 */
@main def runCalibrate_GA(): Unit =
    banner("GENETIC ALGORITHM OPTIMIZER - CalRoute101 Calibration")

    val modelAdapter = new CalibrateCalRoute101()
    val simOpt = new TrafficOptimization(modelAdapter)
    val params: VectorD = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)

    println(f"Initial parameters: $params")

    // Define search ranges for GA: [s, amax, bmax, T, τ]
    val randVars: Array[Variate] = Array(
        Uniform(3.0, 10.0),   // s:    safe distance headway (3-10 meters)
        Uniform(1.0, 10.0),   // amax: max acceleration (1-10 m/s²)
        Uniform(-10.0, -1.0), // bmax: max deceleration (-10 to -1 m/s²)
        Uniform(1.0, 5.0),    // T:    safe time headway (1-5 seconds)
        Uniform(0.5, 3.0)     // τ:    reaction time (0.5-3 seconds)
    )

    val gaOptimizer = new GeneticAlgorithm(simOpt.func, randVars)
    val startTime = System.currentTimeMillis()
    val result = gaOptimizer.solve2()
    val endTime = System.currentTimeMillis()
    val duration = (endTime - startTime) / 1000.0

    println(f"Best Fitness: ${result._1}%.4f")
    println(f"Best Parameters: ${result._2}")
    println(f"Duration: ${duration}%.2f seconds")

    Model.shutdown()
end runCalibrate_GA
//
//
//// Print comparison for all 5 sensors
//println("\n" + "=" * 80)
//println("DATA COMPARISON - PEMS (top) vs SIMULATION (bottom)")
//println("=" * 80)
//
//for i <- 0 until 5 do
//    println(s"\n========== SENSOR $i ==========")
//    //
//    //            println(s"\n--- PEMS count (from CSV file) ---")
//    //            println(sensor_counts(i))
//
//    //            println(s"\n--- PEMS speed in mph (from CSV file) ---")
//    //            println(sensor_speeds(i))
//
//    println(s"\n--- SIMULATION count (generated) ---")
//    println(simSensor_counts(i))
//    println(s"\n--- SIMULATION speed in mph (generated) ---")
//    println(simSensor_speeds(i))
//end for
//
//println("\n" + "=" * 80 + "\n")
//
//// Write simulation output to CSV file in the same format as resultShifted_parameters_changed.csv
//val csvWriter = new EasyWriter("recorder", "simulation_output.csv")
//
//// Build header: S1L1_Flow,S1L2_Flow,...,S1L1_Speed,S1L2_Speed,...
//val header = (for s <- 1 to 5; l <- 1 to 4 yield s"S${s}L${l}_Flow").mkString(",") + "," +
//    (for s <- 1 to 5; l <- 1 to 4 yield s"S${s}L${l}_Speed").mkString(",")
//csvWriter.println(header)
//
//// Get number of time rows (assuming all matrices have same number of rows)
//val numRows = simSensor_counts(0).dim
//
//// Write each time row
//for row <- 0 until numRows do
//    val flowValues = for i <- 0 until 5 yield
//        val countRow = simSensor_counts(i)(row) // Get row as VectorD
//        (for lane <- 0 until 4 yield countRow(lane).toInt).mkString(",")
//
//    val speedValues = for i <- 0 until 5 yield
//        val speedRow = simSensor_speeds(i)(row) //* 2.23694  // Convert m/s to mph
//        (for lane <- 0 until 4 yield f"${speedRow(lane)}%.1f").mkString(",")
//
//    val rowData = flowValues.mkString(",") + "," + speedValues.mkString(",")
//    csvWriter.println(rowData)
//end for
//
//csvWriter.flush()
//csvWriter.close()
//println(s"Simulation output written to: log/recorder/simulation_output.csv")
//
////        // Fit diagnostics
////        val nt = model.config.dim
////        val nparams = params.length
////        object TestFit extends Fit(dfr = nparams, df = nt - nparams)
//
//
////
////        // Validate all 5 sensors
////        for i <- 0 until 5 do
////
////
//////            val cqof = TestFit.diagnose_mat(sensor_counts(i), simSensor_counts(i))
//////            val sqof = TestFit.diagnose_mat(sensor_speeds(i), simSensor_speeds(i))
//////
//////            banner("Quality of Fit (QoF) for counts ")
//////            println(Fit.showFitMap(cqof))
//////
//////            banner("Quality of Fit (QoF) for speeds")
//////            println(Fit.showFitMap(sqof))
////
////
//////
//////            val cqof = TestFit.diagnose_mat(sensor_counts(i), simSensor_counts(i))
//////            val sqof = TestFit.diagnose_mat(sensor_speeds(i), simSensor_speeds(i))
//////
//////            println(s"\nSensor $i:")
//////            println(s"  Counts  - R²: ${cqof(0, 0)}, SSE: ${cqof(3, 0)}, RMSE: ${cqof(6, 0)}, SMAPE: ${cqof(8, 0)}")
//////            println(s"  Speeds  - R²: ${sqof(0, 0)}, SSE: ${sqof(3, 0)}, RMSE: ${sqof(6, 0)}, SMAPE: ${sqof(8, 0)}")
////        end for
