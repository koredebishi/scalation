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
// IDM Literature defaults (Treiber & Kesting, 2013): s₀=2m, a=1.0m/s², b=1.5m/s², T=1.5s, τ=0.6s
val params: VectorD = VectorD(2.0, 1.0, -1.5, 1.5, 0.6) // literature-standard starting point

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
        var totalCountNRMSE = 0.0
        var totalSpeedNRMSE = 0.0

        for i <- 0 until 5 do
            val cqof = TestFit.diagnose_mat(sensor_counts(i), simSensor_counts(i))
            val sqof = TestFit.diagnose_mat(sensor_speeds(i), simSensor_speeds(i))

            // Extract metrics using matrix indexing (rmse=6, smape=8, nrmse=9)
            totalCountSMAPE += cqof(8, 0)
            totalSpeedSMAPE += sqof(8, 0)
            totalCountRMSE += cqof(6, 0)
            totalSpeedRMSE += sqof(6, 0)
            totalCountNRMSE += cqof(9, 0)
            totalSpeedNRMSE += sqof(9, 0)

            // Print individual sensor results
            //println(s"Sensor $i: Count RMSE=${cqof(6,0)}, Count SMAPE=${cqof(8,0)}, Count NRMSE=${cqof(9,0)}, Speed RMSE=${sqof(6,0)}, Speed SMAPE=${sqof(8,0)}, Speed NRMSE=${sqof(9,0)}")
        end for

        // Compute averages
        val avgCountSMAPE = totalCountSMAPE / 5.0
        val avgSpeedSMAPE = totalSpeedSMAPE / 5.0
        val avgCountRMSE = totalCountRMSE / 5.0
        val avgSpeedRMSE = totalSpeedRMSE / 5.0
        val avgCountNRMSE = totalCountNRMSE / 5.0
        val avgSpeedNRMSE = totalSpeedNRMSE / 5.0

        // Print summary
        println("\n" + "=" * 60)
        println("SUMMARY (Average across 5 sensors)")
        println("=" * 60)
        println(f"Counts - Avg RMSE: $avgCountRMSE%.4f, Avg SMAPE: $avgCountSMAPE%.2f, Avg NRMSE: $avgCountNRMSE%.4f")
        println(f"Speeds - Avg RMSE: $avgSpeedRMSE%.4f, Avg SMAPE: $avgSpeedSMAPE%.2f, Avg NRMSE: $avgSpeedNRMSE%.4f")
        println("=" * 60)

        // Return fitness value for optimization
        // Weighted combination of count and speed NRMSE (scale-invariant)
        val countWeight = 0.5
        val speedWeight = 0.5
        val fitness = countWeight * avgCountNRMSE + speedWeight * avgSpeedNRMSE
        println(f"FITNESS (${countWeight}*countNRMSE + ${speedWeight}*speedNRMSE): $fitness%.4f")

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
 *  Uses bounded objective to keep parameters in physically meaningful ranges
 *  centered around known-good values: [s=5.0, amax=4.0, bmax=-2.0, T=3.0, rt=0.5]
 */
@main def runCalibrate_SPSA(): Unit =
    banner("SPSA OPTIMIZER - CalRoute101 Calibration")

    val modelAdapter = new CalibrateCalRoute101()
    val simOpt = new TrafficOptimization(modelAdapter)

    println(s"Initial parameters: $params")

    // Parameter bounds: [s, amax, bmax, T, rt]
    // Centered around good values with reasonable exploration range
    val lower = VectorD(2.0, 1.5, -3.0, 1.0, 0.3)   // lower bounds
    val upper = VectorD(8.0, 6.0, -1.0, 5.0, 1.5)   // upper bounds

    // Bounded objective function: clamps parameters before evaluation
    def boundedFunc(x: VectorD): Double =
        val clamped = VectorD(for i <- x.indices yield math.max(lower(i), math.min(upper(i), x(i))))
        simOpt.func(clamped)
    end boundedFunc

    val spsaOptimizer = new SPSA(boundedFunc, 100)
    spsaOptimizer.setVerbose(1)        // use built-in table output
    spsaOptimizer.setPrintEvery(5)     // print every 5 epochs
    val startTime = System.currentTimeMillis()
    val result    = spsaOptimizer.solve(params)
    val endTime   = System.currentTimeMillis()
    val duration  = (endTime - startTime) / 1000.0

    // Clamp final result for reporting
    val clampedResult = VectorD(for i <- result._2.indices yield math.max(lower(i), math.min(upper(i), result._2(i))))

    println(s"Best Fitness : ${result._1}")
    println(s"Best Parameters (bounded): $clampedResult")
    println(f"Duration: $duration%.2f seconds")

    Model.shutdown()
end runCalibrate_SPSA




@main def runCalibrate_SPSA_Mo(): Unit =
    banner("SPSA_MO OPTIMIZER - CalRoute101 Calibration")

    val modelAdapter = new CalibrateCalRoute101()
    val simOpt = new TrafficOptimization(modelAdapter)
    val params: VectorD = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)

    println(f"Initial parameters: $params")

    // Parameter bounds: [s, amax, bmax, T, rt]
    val lower = VectorD(2.0, 1.5, -4.0, 1.0, 0.3)
    val upper = VectorD(8.0, 6.0, -0.5, 5.0, 1.5)

    // Bounded objective function
    def boundedFunc(x: VectorD): Double =
        val clamped = VectorD(for i <- x.indices yield math.max(lower(i), math.min(upper(i), x(i))))
        simOpt.func(clamped)
    end boundedFunc

    val spsaOptimizer = new SPSA_Mo(boundedFunc, 70)
    spsaOptimizer.setVerbose(1)
    spsaOptimizer.setPrintEvery(5)
    val startTime = System.currentTimeMillis()
    val result = spsaOptimizer.solve(params)
    val endTime = System.currentTimeMillis()
    val duration = (endTime - startTime) / 1000.0

    // Clamp final result for reporting
    val clampedResult = VectorD(for i <- result._2.indices yield math.max(lower(i), math.min(upper(i), result._2(i))))

    println(f"Best Fitness: ${result._1}%.4f")
    println(f"Best Parameters (bounded): $clampedResult")
    println(f"Duration: ${duration}%.2f seconds")

    // Note: Use spsaOptimizer.lossPerEpochs() to get convergence history if needed
    // plotLoss() requires GUI - not available on HPC

    Model.shutdown()
end runCalibrate_SPSA_Mo


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run Nelder-Mead Simplex Optimizer Only - For SLURM Job Array
 * > runMain scalation.simulation.process.runCalibrate_NelderMead
 *  Uses bounded objective to keep parameters in physically meaningful ranges
 *  centered around known-good values: [s=5.0, amax=4.0, bmax=-2.0, T=3.0, rt=0.5]
 */
@main def runCalibrate_NelderMead(): Unit =
    banner("NELDER-MEAD SIMPLEX OPTIMIZER - CalRoute101 Calibration")

    val modelAdapter = new CalibrateCalRoute101()
    val simOpt = new TrafficOptimization(modelAdapter)

    // Same starting point as SPSA for fair comparison
    val startParams: VectorD = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)

    println(s"Starting Nelder-Mead Optimization at ${java.time.LocalDateTime.now()}")
    println(s"Initial parameters: $startParams")

    // Parameter bounds: [s, amax, bmax, T, rt]
    // Same bounds as SPSA for consistency
    val lower = VectorD(2.0, 1.5, -3.0, 1.0, 0.3)   // lower bounds
    val upper = VectorD(8.0, 6.0, -1.0, 5.0, 1.5)   // upper bounds

    // Bounded objective function: clamps parameters before evaluation
    def boundedFunc(x: VectorD): Double =
        val clamped = VectorD(for i <- x.indices yield math.max(lower(i), math.min(upper(i), x(i))))
        simOpt.func(clamped)
    end boundedFunc

    val nelderMeadOptimizer = new NelderMeadSimplex2(boundedFunc, startParams.dim)
    val startTime = System.currentTimeMillis()
    val result = nelderMeadOptimizer.solve(startParams)
    val endTime = System.currentTimeMillis()
    val duration = (endTime - startTime) / 1000.0

    // Clamp final result for reporting
    val clampedResult = VectorD(for i <- result._2.indices yield math.max(lower(i), math.min(upper(i), result._2(i))))

    println(s"Best Fitness : ${result._1}")
    println(s"Best Parameters (bounded): $clampedResult")
    println(f"Duration: $duration%.2f seconds")

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
    // Literature-based + SPSA-informed bounds (Treiber & Kesting, 2013)
    // SPSA best: (3.8, 2.9, -0.64, 1.9, 1.6) and (3.1, 4.0, -0.05, 3.0, 2.3)
    val randVars: Array[Variate] = Array(
        Uniform(1.0, 5.0),    // s₀:   min gap (1-5 m) - literature default: 2m
        Uniform(0.5, 2.5),    // a:    max acceleration (0.5-2.5 m/s²) - literature: 1.0-1.5 m/s²
        Uniform(-3.0, -1.0),  // b:    comfortable deceleration (-3 to -1 m/s²) - literature: 1.5-2.0 m/s²
        Uniform(0.8, 2.5),    // T:    safe time headway (0.8-2.5 s) - literature: 1.5s
        Uniform(0.5, 1.5)     // τ:    reaction time (0.5-1.5 s) - literature: 0.6-1.0s
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



// Numerical ODE choice (Dormand-P)--> RK4/5;  ()// (2,3) (4,4)          (Use more and test for the bragging right and rigor of the experimentation)
// using Poisson also and compare with erlang2S for result analysis.
// Micro level simulation calibration  Rsq, nrmse, rmse, smape


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run a single experiment with specific integrator and arrival type.
 *  Saves raw data CSV and fitness to log/experiments/ directory.
 *
 *  @param integratorType  the ODE integrator (DOPRI5, RK4, RK3, RK2, Ballistic)
 *  @param arrivalType     the arrival process ("Erlang2S" or "Poisson")
 *  @param experimentParams parameter vector [s, amax, bmax, T, rt]
 */
def runSingleExperiment(integratorType: IntegratorType, arrivalType: String,
                        experimentParams: VectorD): Double =
    // Ensure experiments directory exists
    val experimentsDir = new java.io.File(LOG_DIR + "experiments")
    if !experimentsDir.exists() then experimentsDir.mkdirs()

    val experimentName = s"${arrivalType.toLowerCase}_${integratorType.toString.toLowerCase}"
    println(s"\n${"=" * 70}")
    println(s"EXPERIMENT: $experimentName")
    println(s"Integrator: $integratorType, Arrival: $arrivalType")
    println(s"Parameters: $experimentParams")
    println(s"${"=" * 70}")

    // Set integrator BEFORE model instantiation
    IDMDynamics.integratorType = integratorType

    // Apply vehicle parameters and create model with specified arrival type
    Vehicle.setProps(Vehicle.setParams(experimentParams))
    val model = new CalRoute101_2(arrivalType = arrivalType)
    model.simulate()

    // Extract simulation data
    val mats = for s <- model.junc yield s.getRecorderMat
    val simCounts = mats.map(_._1)
    val simSpeeds = mats.map(_._2)

    val pemsSensorIndices = model.pemsToJunc
    val simSensor_counts = pemsSensorIndices.map(i => simCounts(i))
    val simSensor_speeds = pemsSensorIndices.map(i => simSpeeds(i))

    // Load PEMS data for comparison
    val pemsData = (0 until 5).map(i => TrafficConfig2.getPemsCountMatrix(i))
    val sensor_counts = pemsData.map(_._1)
    val sensor_speeds = pemsData.map(_._2)

    // --- Save raw simulation data to CSV ---
    val dataWriter = new EasyWriter("experiments", s"${experimentName}_data.csv")

    // Header: S1L1_Flow,...,S5L4_Flow,S1L1_Speed,...,S5L4_Speed
    val header = (for s <- 1 to 5; l <- 1 to 4 yield s"S${s}L${l}_Flow").mkString(",") + "," +
        (for s <- 1 to 5; l <- 1 to 4 yield s"S${s}L${l}_Speed").mkString(",")
    dataWriter.println(header)

    // Write each time row
    val numRows = simSensor_counts(0).dim
    for row <- 0 until numRows do
        val flowValues = for i <- 0 until 5 yield
            val countRow = simSensor_counts(i)(row)
            (for lane <- 0 until 4 yield countRow(lane).toInt).mkString(",")

        val speedValues = for i <- 0 until 5 yield
            val speedRow = simSensor_speeds(i)(row)
            (for lane <- 0 until 4 yield f"${speedRow(lane)}%.2f").mkString(",")

        dataWriter.println(flowValues.mkString(",") + "," + speedValues.mkString(","))
    end for
    dataWriter.flush()
    dataWriter.close()
    println(s"Raw data saved: log/experiments/${experimentName}_data.csv")

    // --- Compute fitness metrics ---
    val nt = sensor_counts(0).dim
    val nparams = experimentParams.length
    object TestFit extends Fit(dfr = nparams, df = nt - nparams)

    var totalCountNRMSE = 0.0
    var totalSpeedNRMSE = 0.0
    var totalCountSMAPE = 0.0
    var totalSpeedSMAPE = 0.0
    var totalCountRMSE = 0.0
    var totalSpeedRMSE = 0.0

    val fitnessWriter = new EasyWriter("experiments", s"${experimentName}_fitness.txt")
    fitnessWriter.println(s"Experiment: $experimentName")
    fitnessWriter.println(s"Integrator: $integratorType")
    fitnessWriter.println(s"Arrival: $arrivalType")
    fitnessWriter.println(s"Parameters: $experimentParams")
    fitnessWriter.println(s"Timestamp: ${java.time.LocalDateTime.now()}")
    fitnessWriter.println("=" * 60)

    for i <- 0 until 5 do
        val cqof = TestFit.diagnose_mat(sensor_counts(i), simSensor_counts(i))
        val sqof = TestFit.diagnose_mat(sensor_speeds(i), simSensor_speeds(i))

        val countNRMSE = cqof(9).mean   // nrmse index
        val speedNRMSE = sqof(9).mean
        val countSMAPE = cqof(8).mean
        val speedSMAPE = sqof(8).mean
        val countRMSE = cqof(6).mean
        val speedRMSE = sqof(6).mean

        totalCountNRMSE += countNRMSE
        totalSpeedNRMSE += speedNRMSE
        totalCountSMAPE += countSMAPE
        totalSpeedSMAPE += speedSMAPE
        totalCountRMSE += countRMSE
        totalSpeedRMSE += speedRMSE

        val sensorLine = f"Sensor $i: CountNRMSE=$countNRMSE%.4f, SpeedNRMSE=$speedNRMSE%.4f, CountSMAPE=$countSMAPE%.2f, SpeedSMAPE=$speedSMAPE%.2f"
        println(sensorLine)
        fitnessWriter.println(sensorLine)
    end for

    val avgCountNRMSE = totalCountNRMSE / 5.0
    val avgSpeedNRMSE = totalSpeedNRMSE / 5.0
    val avgCountSMAPE = totalCountSMAPE / 5.0
    val avgSpeedSMAPE = totalSpeedSMAPE / 5.0
    val fitness = 0.5 * avgCountNRMSE + 0.5 * avgSpeedNRMSE

    fitnessWriter.println("=" * 60)
    fitnessWriter.println(f"Avg Count NRMSE: $avgCountNRMSE%.6f")
    fitnessWriter.println(f"Avg Speed NRMSE: $avgSpeedNRMSE%.6f")
    fitnessWriter.println(f"Avg Count SMAPE: $avgCountSMAPE%.4f")
    fitnessWriter.println(f"Avg Speed SMAPE: $avgSpeedSMAPE%.4f")
    fitnessWriter.println(f"FITNESS (0.5*countNRMSE + 0.5*speedNRMSE): $fitness%.6f")
    fitnessWriter.flush()
    fitnessWriter.close()

    println(f"\nFITNESS: $fitness%.6f")
    println(s"Fitness saved: log/experiments/${experimentName}_fitness.txt")

    Model.shutdown()
    fitness
end runSingleExperiment


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run all experiments for paper comparison.
 *  Tests all combinations of integrators and arrival types.
 *
 *  > runMain scalation.simulation.process.runAllExperiments
 */
@main def runAllExperiments(): Unit =
    banner("EXPERIMENTAL COMPARISON - Integrators & Arrival Processes")

    // Best parameters from initial testing
    val bestParams = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)

    // Define experiments: (integratorType, arrivalType)
    val experiments = Seq(
        // Erlang2S with all integrators
        (IntegratorType.DOPRI5, "Erlang2S"),
        (IntegratorType.RK4, "Erlang2S"),
        (IntegratorType.RK3, "Erlang2S"),
        (IntegratorType.RK2, "Erlang2S"),
        (IntegratorType.Ballistic, "Erlang2S"),
        // Poisson with all integrators
        (IntegratorType.DOPRI5, "Poisson"),
        (IntegratorType.RK4, "Poisson"),
        (IntegratorType.RK3, "Poisson"),
        (IntegratorType.RK2, "Poisson"),
        (IntegratorType.Ballistic, "Poisson")
    )

    val results = scala.collection.mutable.ArrayBuffer[(String, Double)]()

    for (integrator, arrival) <- experiments do
        val experimentName = s"${arrival.toLowerCase}_${integrator.toString.toLowerCase}"
        val fitness = runSingleExperiment(integrator, arrival, bestParams)
        results += (experimentName -> fitness)
    end for

    // Write summary
    val summaryWriter = new EasyWriter("experiments", "experiment_summary.txt")
    summaryWriter.println("=" * 70)
    summaryWriter.println("EXPERIMENTAL RESULTS SUMMARY")
    summaryWriter.println(s"Timestamp: ${java.time.LocalDateTime.now()}")
    summaryWriter.println(s"Parameters: $bestParams")
    summaryWriter.println("=" * 70)
    summaryWriter.println(f"${"Configuration"}%-30s | ${"Fitness (NRMSE)"}%15s")
    summaryWriter.println("-" * 50)

    for (config, fitness) <- results.sortBy(_._2) do
        val line = f"$config%-30s | $fitness%15.6f"
        println(line)
        summaryWriter.println(line)
    end for

    summaryWriter.println("=" * 70)
    summaryWriter.println("Lower fitness = better fit to PEMS data")
    summaryWriter.flush()
    summaryWriter.close()

    println(s"\nSummary saved: log/experiments/experiment_summary.txt")
    println("All experiment data saved in log/experiments/")
end runAllExperiments


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run a single experiment from command line (for individual runs).
 *
 *  Usage: runMain scalation.simulation.process.runExperimentCLI <integrator> <arrival>
 *  Where: integrator = DOPRI5, RK4, RK3, RK2, Ballistic
 *         arrival    = Erlang2S, Poisson
 */
@main def runExperimentCLI(integratorName: String, arrivalType: String): Unit =
    val integrator = integratorName match
        case "DOPRI5"    => IntegratorType.DOPRI5
        case "RK4"       => IntegratorType.RK4
        case "RK3"       => IntegratorType.RK3
        case "RK2"       => IntegratorType.RK2
        case "Ballistic" => IntegratorType.Ballistic
        case _           =>
            println(s"Unknown integrator: $integratorName, using DOPRI5")
            IntegratorType.DOPRI5

    val arrival = arrivalType match
        case "Poisson"  => "Poisson"
        case "Erlang2S" => "Erlang2S"
        case _          =>
            println(s"Unknown arrival: $arrivalType, using Erlang2S")
            "Erlang2S"

    val bestParams = VectorD(5.0, 4.0, -2.0, 3.0, 0.5)
    runSingleExperiment(integrator, arrival, bestParams)
end runExperimentCLI
