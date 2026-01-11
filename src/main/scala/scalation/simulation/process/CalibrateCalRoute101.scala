package scalation
package simulation
package process

import scalation.mathstat.{MatrixD, VectorD, VectorI}
import scalation.modeling.{Fit}
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

    // Record start time
    val startTime = System.currentTimeMillis()
    val startTimestamp = java.time.LocalDateTime.now()
    println(s"START TIME: $startTimestamp")

    // Set integrator BEFORE model instantiation
    IDMDynamics.integratorType = integratorType
    // Reset print flag so updateM will print the integrator it's actually using
    IDMDynamics.resetIntegratorPrintFlag()

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
    var totalCountRsq = 0.0
    var totalSpeedRsq = 0.0

    val fitnessWriter = new EasyWriter("experiments", s"${experimentName}_fitness.txt")
    fitnessWriter.println(s"Experiment: $experimentName")
    fitnessWriter.println(s"Integrator: $integratorType")
    fitnessWriter.println(s"Arrival: $arrivalType")
    fitnessWriter.println(s"Parameters: $experimentParams")
    fitnessWriter.println(s"Start Time: $startTimestamp")
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
        val countRsq = cqof(0).mean    // R² index
        val speedRsq = sqof(0).mean

        totalCountNRMSE += countNRMSE
        totalSpeedNRMSE += speedNRMSE
        totalCountSMAPE += countSMAPE
        totalSpeedSMAPE += speedSMAPE
        totalCountRMSE += countRMSE
        totalSpeedRMSE += speedRMSE
        totalCountRsq += countRsq
        totalSpeedRsq += speedRsq

        val sensorLine = f"Sensor $i: CountNRMSE=$countNRMSE%.4f, SpeedNRMSE=$speedNRMSE%.4f, CountSMAPE=$countSMAPE%.2f, SpeedSMAPE=$speedSMAPE%.2f, CountR²=$countRsq%.4f, SpeedR²=$speedRsq%.4f"
        println(sensorLine)
        fitnessWriter.println(sensorLine)
    end for

    val avgCountNRMSE = totalCountNRMSE / 5.0
    val avgSpeedNRMSE = totalSpeedNRMSE / 5.0
    val avgCountSMAPE = totalCountSMAPE / 5.0
    val avgSpeedSMAPE = totalSpeedSMAPE / 5.0
    val avgCountRsq = totalCountRsq / 5.0
    val avgSpeedRsq = totalSpeedRsq / 5.0
    val fitness = 0.5 * avgCountNRMSE + 0.5 * avgSpeedNRMSE

    // Record end time and duration
    val endTime = System.currentTimeMillis()
    val endTimestamp = java.time.LocalDateTime.now()
    val durationSeconds = (endTime - startTime) / 1000.0

    fitnessWriter.println("=" * 60)
    fitnessWriter.println(f"Avg Count NRMSE: $avgCountNRMSE%.6f")
    fitnessWriter.println(f"Avg Speed NRMSE: $avgSpeedNRMSE%.6f")
    fitnessWriter.println(f"Avg Count SMAPE: $avgCountSMAPE%.4f")
    fitnessWriter.println(f"Avg Speed SMAPE: $avgSpeedSMAPE%.4f")
    fitnessWriter.println(f"Avg Count R²: $avgCountRsq%.4f")
    fitnessWriter.println(f"Avg Speed R²: $avgSpeedRsq%.4f")
    fitnessWriter.println(f"FITNESS (0.5*countNRMSE + 0.5*speedNRMSE): $fitness%.6f")
    fitnessWriter.println(s"End Time: $endTimestamp")
    fitnessWriter.println(f"Duration: $durationSeconds%.2f seconds")
    fitnessWriter.flush()
    fitnessWriter.close()

    println(f"\nFITNESS: $fitness%.6f")
    println(f"Count R²: $avgCountRsq%.4f, Speed R²: $avgSpeedRsq%.4f")
    println(f"Duration: $durationSeconds%.2f seconds")
    println(s"END TIME: $endTimestamp")
    println(s"Fitness saved: log/experiments/${experimentName}_fitness.txt")

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
    //val bestParams = VectorD(2.00000, 1.50000, -1.00000, 1.00000, 1.24832)


    // Define experiments: (integratorType, arrivalType)
    val experiments = Seq(
         //Erlang2S with all integrators
        //(IntegratorType.DOPRI5, "Erlang2S"),
        //(IntegratorType.RK4, "Erlang2S"),
        //(IntegratorType.RK3, "Erlang2S"),
        //(IntegratorType.RK2, "Erlang2S"),
        //(IntegratorType.Ballistic, "Erlang2S"),
        (IntegratorType.Euler, "Erlang2S"),
        //(IntegratorType.Heun, "Erlang2S"),
        //(IntegratorType.butcher, "Erlang2S"),
         //Poisson with all integrators
        //(IntegratorType.DOPRI5, "Poisson"),
        //(IntegratorType.RK4, "Poisson"),
        //(IntegratorType.RK3, "Poisson"),
        //(IntegratorType.RK2, "Poisson"),
        //(IntegratorType.Ballistic, "Poisson"),
        (IntegratorType.Euler, "Poisson")
        //(IntegratorType.Heun, "Poisson"),
        //(IntegratorType.butcher, "Poisson")
        // Additional Erlang2S runs for completeness
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
    Model.shutdown()   // this is the issue
end runAllExperiments

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Offline Analysis: Read all experiment CSVs and compute macro/micro validation.
 *  Produces same output as TrafficConfigTest2 for all 10 experiment configurations.
 *  All results written to a single file: log/experiments/offline_analysis_summary.txt
 *
 *  > runMain scalation.simulation.process.analyzeAllExperiments
 */
@main def analyzeAllExperiments(): Unit =
    banner("OFFLINE ANALYSIS - All Experiment Configurations")
    
    val nSensors = 5
    val nLanes   = 4
    val nRows    = 48
    val nParams  = 5
    
    // Create Fit object for diagnostics - same as runSingleExperiment
    object TestFit extends Fit(dfr = nParams, df = nRows - nParams)

    // PEMS data paths
    val pemsFiles = Array(
        "Mainline_VDS_Donald_Doyle/1-401112ML.csv",
        "Mainline_VDS_Donald_Doyle/2-401104ML.csv",
        "Mainline_VDS_Donald_Doyle/3-400712ML.csv",
        "Mainline_VDS_Donald_Doyle/4-400450ML.csv",
        "Mainline_VDS_Donald_Doyle/5-407463ML.csv"
    )
    val flowIdx  = VectorI(1, 3, 5, 7)
    val speedIdx = VectorI(2, 4, 6, 8)
    
    // Load PEMS data once (shared across all experiments)
    val pemsData = new Array[MatrixD](nSensors)
    cfor(0, nSensors) { s => pemsData(s) = MatrixD.load(pemsFiles(s)) }
    
    def pemsFlow(s: Int): MatrixD  = pemsData(s)(?, flowIdx)
    def pemsSpeed(s: Int): MatrixD = pemsData(s)(?, speedIdx) * 0.44704  // mph → m/s
    
    // Define all experiment configurations
    val experiments = Seq(
        ("erlang2s", "dopri5"),
        ("erlang2s", "rk4"),
        ("erlang2s", "rk3"),
        ("erlang2s", "rk2"),
        ("erlang2s", "ballistic"),
        ("erlang2s", "butcher"),
        ("erlang2s", "euler"),
        ("erlang2s", "heun"),
        ("poisson", "dopri5"),
        ("poisson", "rk4"),
        ("poisson", "rk3"),
        ("poisson", "rk2"),
        ("poisson", "ballistic"),
        ("poisson", "butcher"),
        ("poisson", "euler"),
        ("poisson", "heun")
    )
    
    // Output file
    val outputWriter = new EasyWriter("experiments", "offline_analysis_summary.txt")
    outputWriter.println("=" * 140)
    outputWriter.println("OFFLINE ANALYSIS - All Experiment Configurations")
    outputWriter.println(s"Timestamp: ${java.time.LocalDateTime.now()}")
    outputWriter.println("=" * 140)
    
    // Macro validation helper - uses TestFit.diagnose_mat for consistency with runSingleExperiment
    // Returns: (R², SMAPE, RMSE, NRMSE) - all as mean across lanes
    def macroValidation(sim: MatrixD, pms: MatrixD): (Double, Double, Double, Double) =
        val qof = TestFit.diagnose_mat(pms, sim)
        val r2    = qof(0).mean   // R² index
        val rmse  = qof(6).mean   // RMSE index
        val smape = qof(8).mean   // SMAPE index
        val nrmse = qof(9).mean   // NRMSE index
        (r2, smape, rmse, nrmse)
    end macroValidation
    
    // Micro validation helper - uses TestFit.diagnose_mat for consistency with runSingleExperiment
    // Returns per-lane: (R², SMAPE, RMSE, NRMSE)
    def microValidation(sim: MatrixD, pms: MatrixD): Array[(Double, Double, Double, Double)] =
        val qof = TestFit.diagnose_mat(pms, sim)
        val results = new Array[(Double, Double, Double, Double)](nLanes)
        cfor(0, nLanes) { lane =>
            results(lane) = (qof(0, lane), qof(8, lane), qof(6, lane), qof(9, lane))  // R², SMAPE, RMSE, NRMSE
        }
        results
    end microValidation
    
    // Process each experiment
    for (arrival, integrator) <- experiments do
        val experimentName = s"${arrival}_${integrator}"
        val simFile = s"C:/Simulation/scalation_2.0/log/experiments/unoptimized_run/${experimentName}_data.csv"
        
        outputWriter.println(s"\n${"#" * 140}")
        outputWriter.println(s"EXPERIMENT: $experimentName (Arrival: $arrival, Integrator: $integrator)")
        outputWriter.println(s"${"#" * 140}")
        
        // Check if file exists
        val file = new java.io.File(simFile)
        if !file.exists() then
            outputWriter.println(s"  [SKIPPED] File not found: $simFile")
            println(s"[SKIPPED] $experimentName - file not found")
        else
            println(s"Processing: $experimentName")
            
            // Load simulation data
            val simData = MatrixD.load(simFile, skip = 1, fullPath = true)
            
            // Slice helpers
            def simFlow(s: Int): MatrixD  = simData(?, VectorI.range(s * nLanes, (s + 1) * nLanes))
            def simSpeed(s: Int): MatrixD = simData(?, VectorI.range((nSensors + s) * nLanes, (nSensors + s + 1) * nLanes))
            
            // Arrays for macro metrics
            val flowR2     = new Array[Double](nSensors)
            val flowSmape  = new Array[Double](nSensors)
            val flowRmse   = new Array[Double](nSensors)
            val flowNrmse  = new Array[Double](nSensors)
            val speedR2    = new Array[Double](nSensors)
            val speedSmape = new Array[Double](nSensors)
            val speedRmse  = new Array[Double](nSensors)
            val speedNrmse = new Array[Double](nSensors)

            // Compute macro validation
            cfor(0, nSensors) { s =>
                val (fr2, fsm, frm, fnr) = macroValidation(simFlow(s), pemsFlow(s))
                flowR2(s) = fr2; flowSmape(s) = fsm; flowRmse(s) = frm; flowNrmse(s) = fnr
                val (sr2, ssm, srm, snr) = macroValidation(simSpeed(s), pemsSpeed(s))
                speedR2(s) = sr2; speedSmape(s) = ssm; speedRmse(s) = srm; speedNrmse(s) = snr
            }
            
            // Compute micro validation
            val flowMicro  = new Array[Array[(Double, Double, Double, Double)]](nSensors)
            val speedMicro = new Array[Array[(Double, Double, Double, Double)]](nSensors)
            cfor(0, nSensors) { s =>
                flowMicro(s)  = microValidation(simFlow(s), pemsFlow(s))
                speedMicro(s) = microValidation(simSpeed(s), pemsSpeed(s))
            }
            
            // ─── MACRO-LEVEL OUTPUT ───
            outputWriter.println(s"\n${"=" * 160}")
            outputWriter.println("MACRO-LEVEL VALIDATION (Sensor Aggregates)")
            outputWriter.println("=" * 160)
            outputWriter.println(f"${"Sensor"}%-10s ${"Flow R²"}%-12s ${"Flow SMAPE"}%-14s ${"Flow RMSE"}%-14s ${"Flow NRMSE"}%-14s ${"Speed R²"}%-12s ${"Speed SMAPE"}%-14s ${"Speed RMSE"}%-14s ${"Speed NRMSE"}%-14s")
            outputWriter.println("-" * 160)
            cfor(0, nSensors) { s =>
                outputWriter.println(f"${s + 1}%-10d ${flowR2(s)}%-12.4f ${flowSmape(s)}%-14.2f ${flowRmse(s)}%-14.2f ${flowNrmse(s)}%-14.4f ${speedR2(s)}%-12.4f ${speedSmape(s)}%-14.2f ${speedRmse(s)}%-14.2f ${speedNrmse(s)}%-14.4f")
            }
            outputWriter.println("=" * 160)

            // ─── MICRO-LEVEL OUTPUT ───
            outputWriter.println(s"\n${"=" * 180}")
            outputWriter.println("MICRO-LEVEL VALIDATION (Lane Detail)")
            outputWriter.println("=" * 180)
            outputWriter.println(f"${"Sensor"}%-8s ${"Lane"}%-6s ${"Flow R²"}%-12s ${"Flow SMAPE"}%-14s ${"Flow RMSE"}%-14s ${"Flow NRMSE"}%-14s ${"Speed R²"}%-12s ${"Speed SMAPE"}%-14s ${"Speed RMSE"}%-14s ${"Speed NRMSE"}%-14s")
            outputWriter.println("-" * 180)
            cfor(0, nSensors) { s =>
                cfor(0, nLanes) { l =>
                    val (fR2, fSm, fRm, fNr) = flowMicro(s)(l)
                    val (sR2, sSm, sRm, sNr) = speedMicro(s)(l)
                    val label = if l == 0 then s"${s + 1}" else ""
                    outputWriter.println(f"$label%-8s ${l + 1}%-6d $fR2%-12.4f $fSm%-14.2f $fRm%-14.2f $fNr%-14.4f $sR2%-12.4f $sSm%-14.2f $sRm%-14.2f $sNr%-14.4f")
                }
                if s < nSensors - 1 then outputWriter.println("-" * 180)
            }
            outputWriter.println("=" * 180)

            // ─── DIAGNOSE_MAT OUTPUT ───
            outputWriter.println(s"\n${"=" * 120}")
            outputWriter.println("DIAGNOSE_MAT VALIDATION (Full Quality of Fit Metrics)")
            outputWriter.println("=" * 120)
            cfor(0, nSensors) { s =>
                val flowQof  = TestFit.diagnose_mat(pemsFlow(s), simFlow(s))
                val speedQof = TestFit.diagnose_mat(pemsSpeed(s), simSpeed(s))
                
                outputWriter.println(s"\n--- Sensor ${s + 1} - Flow Quality of Fit ---")
                outputWriter.println(Fit.showFitMap(flowQof))
                
                outputWriter.println(s"\n--- Sensor ${s + 1} - Speed Quality of Fit ---")
                outputWriter.println(Fit.showFitMap(speedQof))
            }
            
            // ─── Compute overall fitness (average NRMSE) for summary ───
            var totalCountNRMSE = 0.0
            var totalSpeedNRMSE = 0.0
            cfor(0, nSensors) { s =>
                val flowQof  = TestFit.diagnose_mat(pemsFlow(s), simFlow(s))
                val speedQof = TestFit.diagnose_mat(pemsSpeed(s), simSpeed(s))
                totalCountNRMSE += flowQof(9).mean   // NRMSE index
                println(s"totalCountNRMSE: $totalCountNRMSE")
                totalSpeedNRMSE += speedQof(9).mean  //
                println(s"totalSpeedNRMSE: $totalSpeedNRMSE")
            }
            val fitness = (0.5 * (totalCountNRMSE / 5.0)) + (0.5 * (totalSpeedNRMSE / 5.0))
            println(s"fitness: $fitness")

            outputWriter.println(s"\n average_totalSpeedNRMSE: $totalSpeedNRMSE ; average_totalFlowNRMSE: $totalCountNRMSE OVERALL FITNESS: $fitness")
        end if
    end for
    
    outputWriter.println(s"\n${"=" * 140}")
    outputWriter.println("END OF OFFLINE ANALYSIS")
    outputWriter.println(s"${"=" * 140}")
    outputWriter.flush()
    outputWriter.close()
    
    println(s"\nOffline analysis complete!")
    println(s"Results written to: log/experiments/offline_analysis_summary.txt")
    
end analyzeAllExperiments
