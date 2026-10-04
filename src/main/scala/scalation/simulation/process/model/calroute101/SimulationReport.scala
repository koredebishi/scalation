//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Fri Jan 31 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Simulation Report for Traffic Simulation (Statistics and Validation)
 */

package scalation
package simulation
package process
package model
package calroute101

import scalation.{cfor, debugf}
import scalation.mathstat.*
import scalation.modeling.{Fit, FitM}
import scalation.simulation.process.config.PeMSDemand
import scalation.simulation.process.config.PeMSDataHelper

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, sqrt}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MacroMetrics` case class holds macro-level validation metrics per sensor.
 *  @param r2      R-squared (coefficient of determination)
 *  @param mae     Mean Absolute Error
 *  @param rmse    Root Mean Squared Error
 *  @param mean    Mean of observed (PeMS) values
 *  @param nrmse   Normalized RMSE (as percentage, *100)
 *  @param smape   Symmetric Mean Absolute Percentage Error
 *  @param simMean Mean of simulated values, computed the same way as `mean`
 */
case class MacroMetrics (r2: Double, mae: Double, rmse: Double, mean: Double, nrmse: Double, smape: Double,
                         simMean: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SensorValidation` case class holds validation results for one sensor.
 *  @param sensorIdx    the sensor index (0-based)
 *  @param flowMetrics  macro metrics for flow comparison
 *  @param speedMetrics macro metrics for speed comparison
 *  @param flowMicro    per-lane metrics for flow
 *  @param speedMicro   per-lane metrics for speed
 */
case class SensorValidation (sensorIdx: Int, flowMetrics: MacroMetrics,
                             speedMetrics: MacroMetrics,
                             flowMicro: Array [MacroMetrics],
                             speedMicro: Array [MacroMetrics])

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ValidationResult` case class holds all validation results.
 *  @param sensors         per-sensor validation results
 *  @param avgFlowNRMSE    average flow NRMSE*100 across all sensors
 *  @param avgSpeedNRMSE   average speed NRMSE*100 across all sensors
 *  @param fitness         combined fitness value for optimization
 */
case class ValidationResult (sensors: Array [SensorValidation],
                             avgFlowNRMSE: Double, avgSpeedNRMSE: Double,
                             fitness: Double)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimulationReport` class generates statistics and validation reports.
 *  Works with PeMS data-driven mode only.
 *  @param simFlow      simulated flow matrices per sensor (Array of MatrixD)
 *  @param simSpeed     simulated speed matrices per sensor (Array of MatrixD)
 *  @param demand       the PeMS demand configuration
 *  @param nSensors     number of sensors
 *  @param nLanes       number of lanes
 *  @param nRows        number of time intervals
 */
class SimulationReport (simFlow: Array [MatrixD], simSpeed: Array [MatrixD],
                        simDensity: Array [MatrixD],
                        demand: PeMSDemand, nSensors: Int = 5,
                        nLanes: Int = 4, nRows: Int = 48):

    private val debug = debugf ("SimulationReport", false)
    private val nParams = 5                                          // number of fitted parameters

    require (nSensors > 0 && simFlow.length == nSensors && simSpeed.length == nSensors &&
             simDensity.length == nSensors, "report requires one set of matrices per sensor")
    cfor (0, nSensors) { s =>
        require (simFlow(s).dim == nRows && simFlow(s).dim2 == nLanes &&
                 simSpeed(s).dim == nRows && simSpeed(s).dim2 == nLanes &&
                 simDensity(s).dim == nRows && simDensity(s).dim2 == nLanes,
                 "report matrices must share the configured row and lane dimensions")
    }

    // Fit object for matrix-level diagnostics
    private object TestFit extends Fit (dfr = nParams, df = nRows - nParams)

    // FitM for per-lane diagnostics
    private val fitM = new FitM {}

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute macro-level validation metrics for two matrices.
     *  @param sim     simulated data matrix
     *  @param obs     observed data matrix (PeMS)
     *  @param isFlow  true for flow (sum across lanes), false for speed (average across lanes)
     */
    private def macroValidation (sim: MatrixD, obs: MatrixD, isFlow: Boolean = true): MacroMetrics =
        val n = obs.dim * obs.dim2
        val e = obs - sim
        
        // Mean of observed values:
        //   Flow  → sum across lanes (total corridor flow per interval), then average over time
        //   Speed → average across lanes (mean corridor speed per interval), then average over time
        val obsRowSums = obs.sumVr                                   // sum across lanes per row (VectorD)
        val obsMeanTotal = if isFlow then obsRowSums.mean            // flow: mean of row sums
                           else obsRowSums.mean / nLanes             // speed: mean of row averages
        val simRowSums   = sim.sumVr                                 // same aggregation for the simulated side
        val simMeanTotal = if isFlow then simRowSums.mean
                           else simRowSums.mean / nLanes
        
        // R² - Coefficient of determination (use per-cell calculation)
        val obsCellMean = obs.mean.sum / obs.dim2                    // per-cell mean for R²
        val sse = e.normFSq
        val sst = (obs - obsCellMean).normFSq
        val r2 = 1.0 - sse / sst
        
        // MAE - Mean Absolute Error (per cell)
        var maeSum = 0.0
        cfor (0, obs.dim) { i =>
            cfor (0, obs.dim2) { j =>
                maeSum += abs(e(i, j))
            }
        }
        val mae = maeSum / n
        
        // RMSE - Root Mean Squared Error (per cell)
        val rmse = sqrt(sse / n)
        
        // NRMSE*100 - Normalized RMSE as percentage (use obsMeanTotal to match displayed mean)
        val nrmse100 = if obsMeanTotal != 0.0 then (rmse / obsMeanTotal) * 100.0 else rmse * 100.0

        // SMAPE - Symmetric Mean Absolute Percentage Error
        var smapeSum = 0.0
        cfor (0, obs.dim) { i =>
            cfor (0, obs.dim2) { j =>
                val o = obs(i, j)
                val s = sim(i, j)
                if (o + s) > 0 then smapeSum += 2.0 * abs(o - s) / (abs(o) + abs(s))
            }
        }
        val smape = 100.0 * smapeSum / n

        MacroMetrics(r2, mae, rmse, obsMeanTotal, nrmse100, smape, simMeanTotal)
    end macroValidation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute micro-level (per-lane) validation metrics.
     *  @param sim  simulated data matrix
     *  @param obs  observed data matrix (PeMS)
     */
    private def microValidation (sim: MatrixD, obs: MatrixD): Array [MacroMetrics] =
        val results = new Array [MacroMetrics] (nLanes)
        cfor (0, nLanes) { lane =>
            val obsCol = obs.col(lane)
            val simCol = sim.col(lane)
            val d = fitM.diagnose(obsCol, simCol)
            // FitM.diagnose returns: (0)rSq, (1)sst, (2)sse, (3)sde, (4)mse0, (5)rmse, (6)mae, (7)smape, (8)m
            val r2 = d(0)
            val mae = d(6)
            val rmse = d(5)
            val smape = d(7)
            val obsMean = obsCol.mean
            val nrmse100 = if obsMean != 0.0 then (rmse / obsMean) * 100.0 else rmse * 100.0
            results(lane) = MacroMetrics(r2, mae, rmse, obsMean, nrmse100, smape, simCol.mean)
        }
        results
    end microValidation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute validation results against PeMS data.
     */
    def validate (): ValidationResult =
        debug ("validate", s"Starting validation: $nSensors sensors, $nLanes lanes, $nRows rows")
        val sensorResults = new Array [SensorValidation] (nSensors)
        var totalFlowNRMSE = 0.0
        var totalSpeedNRMSE = 0.0

        cfor (0, nSensors) { s =>
            debug ("validate", s"  Processing sensor $s...")
            // Load PeMS data for this sensor using PeMSDataHelper (corridor-agnostic)
            val (pemsFlow, pemsSpeed) = PeMSDataHelper.loadMainlineSensor (demand, s, demand.window, demand.layout)
            require (pemsFlow.dim == simFlow(s).dim && pemsFlow.dim2 == simFlow(s).dim2 &&
                     pemsSpeed.dim == simSpeed(s).dim && pemsSpeed.dim2 == simSpeed(s).dim2,
                     "observed and simulated sensor matrices must have matching dimensions")
            debug ("validate", s"    PeMS loaded: flow=${pemsFlow.dim}x${pemsFlow.dim2}, speed=${pemsSpeed.dim}x${pemsSpeed.dim2}")
            debug ("validate", s"    Sim data: flow=${simFlow(s).dim}x${simFlow(s).dim2}, speed=${simSpeed(s).dim}x${simSpeed(s).dim2}")

            val flowMacro = macroValidation (simFlow(s), pemsFlow, isFlow = true)
            val speedMacro = macroValidation (simSpeed(s), pemsSpeed, isFlow = false)
            val flowMicro = microValidation (simFlow(s), pemsFlow)
            val speedMicro = microValidation (simSpeed(s), pemsSpeed)

            sensorResults(s) = SensorValidation (s, flowMacro, speedMacro, flowMicro, speedMicro)
            totalFlowNRMSE += flowMacro.nrmse
            totalSpeedNRMSE += speedMacro.nrmse
        }

        val avgFlowNRMSE = totalFlowNRMSE / nSensors
        val avgSpeedNRMSE = totalSpeedNRMSE / nSensors
//
//        // Fitness: 20/80 weighted combination (speed dominates; flow already near-optimal via Erlang2S)
//        // Flow protection: soft penalty when flow NRMSE exceeds 2.5 (well above unoptimized baseline of 2.03)
//        // This prevents the optimizer from trading flow accuracy for speed gains
//        val flowThreshold = 2.5
//        val flowPenalty   = if avgFlowNRMSE > flowThreshold then 10.0 * (avgFlowNRMSE - flowThreshold) else 0.0
        val fitness       = 0.2 * avgFlowNRMSE + 0.8 * avgSpeedNRMSE

        ValidationResult (sensorResults, avgFlowNRMSE, avgSpeedNRMSE, fitness)
    end validate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute micro-level (lane-level) fitness.
     *  Averages fitness across all sensors and all lanes individually.
     *  This captures lane-specific behavior that corridor-level aggregation may miss.
     */
    def microLevelFitness (): Double =
        val vr = validate()
        var totalLaneMetric = 0.0
        var count = 0
        for sv <- vr.sensors do
            for lane <- 0 until nLanes do
                val flowNRMSE = sv.flowMicro(lane).nrmse
                val speedNRMSE = sv.speedMicro(lane).nrmse
                totalLaneMetric += 0.2 * flowNRMSE + 0.8 * speedNRMSE
                count += 1
            end for
        end for
        totalLaneMetric / count
    end microLevelFitness

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return fitness value for optimization (NRMSE-based, lower is better).
     *  Checks FITNESS_MODE system property: "micro" uses lane-level, else corridor-level.
     */
    def fitness (): Double =
        val mode = System.getProperty("FITNESS_MODE", "macro")
        if mode == "micro" then microLevelFitness() else validate().fitness
    end fitness

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the validation report to console.
     */
    def report (): Unit =
        debug ("report", "Starting validation...")
        val vr = validate()
        debug ("report", s"Validation complete: fitness=${vr.fitness}")
        printValidationReport(vr)
        debug ("report", "Report printed")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print detailed validation report.
     *  @param vr  the validation result
     */
    private def printValidationReport (vr: ValidationResult): Unit =
        println ("\n" + "=" * 248)
        println ("MACRO-LEVEL VALIDATION (Sensor Aggregates)")
        println ("=" * 248)
        println (f"${"Sensor"}%-9s ${"Flow R²"}%-12s ${"Flow MAE"}%-12s ${"Flow RMSE"}%-12s ${"Flow PeMS Mean"}%-15s ${"Flow Sim Mean"}%-15s ${"Flow NRMSE*100"}%-16s ${"Flow SMAPE"}%-14s ${"Speed R²"}%-12s ${"Speed MAE"}%-12s ${"Speed RMSE"}%-12s ${"Speed PeMS Mean"}%-15s ${"Speed Sim Mean"}%-15s ${"Speed NRMSE*100"}%-16s ${"Speed SMAPE"}%-14s")
        println ("-" * 248)

        for sv <- vr.sensors do
            val s = sv.sensorIdx + 1
            val fm = sv.flowMetrics
            val sm = sv.speedMetrics
            println (f"$s%-9d ${fm.r2}%-12.4f ${fm.mae}%-12.2f ${fm.rmse}%-12.2f ${fm.mean}%-15.2f ${fm.simMean}%-15.2f ${fm.nrmse}%-16.2f ${fm.smape}%-14.2f ${sm.r2}%-12.4f ${sm.mae}%-12.2f ${sm.rmse}%-12.2f ${sm.mean}%-15.2f ${sm.simMean}%-15.2f ${sm.nrmse}%-16.2f ${sm.smape}%-14.2f")
        end for
        println ("=" * 248)

        println ("\n" + "=" * 258)
        println ("MICRO-LEVEL VALIDATION (Lane Detail)")
        println ("=" * 258)
        println (f"${"Sensor"}%-8s ${"Lane"}%-6s ${"Flow R²"}%-12s ${"Flow MAE"}%-12s ${"Flow RMSE"}%-12s ${"Flow PeMS Mean"}%-15s ${"Flow Sim Mean"}%-15s ${"Flow NRMSE*100"}%-16s ${"Flow SMAPE"}%-14s ${"Speed R²"}%-12s ${"Speed MAE"}%-12s ${"Speed RMSE"}%-12s ${"Speed PeMS Mean"}%-15s ${"Speed Sim Mean"}%-15s ${"Speed NRMSE*100"}%-16s ${"Speed SMAPE"}%-14s")
        println ("-" * 258)

        for sv <- vr.sensors do
            cfor (0, nLanes) { l =>
                val fm = sv.flowMicro(l)
                val sm = sv.speedMicro(l)
                val label = if l == 0 then s"${sv.sensorIdx + 1}" else ""
                println (f"$label%-8s ${l + 1}%-6d ${fm.r2}%-12.4f ${fm.mae}%-12.2f ${fm.rmse}%-12.2f ${fm.mean}%-15.2f ${fm.simMean}%-15.2f ${fm.nrmse}%-16.2f ${fm.smape}%-14.2f ${sm.r2}%-12.4f ${sm.mae}%-12.2f ${sm.rmse}%-12.2f ${sm.mean}%-15.2f ${sm.simMean}%-15.2f ${sm.nrmse}%-16.2f ${sm.smape}%-14.2f")
            }
            if sv.sensorIdx < nSensors - 1 then println ("-" * 258)
        end for
        println ("=" * 258)

        println ("\n" + "=" * 60)
        println ("SUMMARY")
        println ("=" * 60)
        println (f"Average Flow NRMSE*100:  ${vr.avgFlowNRMSE}%.4f")
        println (f"Average Speed NRMSE*100: ${vr.avgSpeedNRMSE}%.4f")
        println (f"Fitness (0.2 flow + 0.8 speed NRMSE): ${vr.fitness}%.4f")
        println ("=" * 60)
    end printValidationReport

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate LaTeX tables for the validation results.
     */
    def latexTables (): String = generateLatexTables(validate())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate LaTeX macro table.
     *  @param vr  the validation result
     */
    private def generateLatexTables (vr: ValidationResult): String =
        val sb = new StringBuilder

        sb.append ("\\begin{table}[htbp]\n")
        sb.append ("\\centering\n")
        sb.append ("\\caption{Macro-Level Validation Results}\n")
        sb.append ("\\begin{tabular}{ccccccccccccc}\n")
        sb.append ("\\hline\n")
        sb.append ("Sensor & Flow $R^2$ & Flow MAE & Flow RMSE & Flow Mean & Flow NRMSE & Flow SMAPE & Speed $R^2$ & Speed MAE & Speed RMSE & Speed Mean & Speed NRMSE & Speed SMAPE \\\\\n")
        sb.append ("\\hline\n")

        for sv <- vr.sensors do
            val s = sv.sensorIdx + 1
            val fm = sv.flowMetrics
            val sm = sv.speedMetrics
            sb.append (f"$s & ${fm.r2}%.4f & ${fm.mae}%.2f & ${fm.rmse}%.2f & ${fm.mean}%.2f & ${fm.nrmse}%.2f & ${fm.smape}%.2f & ${sm.r2}%.4f & ${sm.mae}%.2f & ${sm.rmse}%.2f & ${sm.mean}%.2f & ${sm.nrmse}%.2f & ${sm.smape}%.2f \\\\\n")
        end for

        sb.append ("\\hline\n")
        sb.append ("\\end{tabular}\n")
        sb.append ("\\end{table}\n")

        sb.toString
    end generateLatexTables

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Export simulation flow and speed data to CSV file.
     *  Format matches old TrafficConfig2 output: S1L1_Flow,...,S5L4_Flow,S1L1_Speed,...,S5L4_Speed
     *  @param filename  the output CSV filename (relative to log/simulation/)
     */
    def exportCSV (filename: String = "simulation_data.csv"): Unit =
        debug ("exportCSV", s"Exporting to log/simulation/$filename")
        val csvWriter = new EasyWriter ("simulation", filename)

        // Build header: S1L1_Flow,...,S5L4_Flow,S1L1_Speed,...,S5L4_Speed,S1L1_Density,...,S5L4_Density
        val header = (for s <- 1 to nSensors; l <- 1 to nLanes yield s"S${s}L${l}_Flow").mkString(",") + "," +
                     (for s <- 1 to nSensors; l <- 1 to nLanes yield s"S${s}L${l}_Speed").mkString(",") + "," +
                     (for s <- 1 to nSensors; l <- 1 to nLanes yield s"S${s}L${l}_Density").mkString(",")
        csvWriter.println (header)

        val numRows = simFlow(0).dim

        cfor (0, numRows) { row =>
            val flowValues = for s <- 0 until nSensors yield
                val flowRow = simFlow(s)(row)
                (for lane <- 0 until nLanes yield flowRow(lane).toInt).mkString(",")

            val speedValues = for s <- 0 until nSensors yield
                val speedRow = simSpeed(s)(row)
                (for lane <- 0 until nLanes yield f"${speedRow(lane)}%.2f").mkString(",")

            val densityValues = for s <- 0 until nSensors yield
                val densityRow = simDensity(s)(row)
                (for lane <- 0 until nLanes yield f"${densityRow(lane)}%.6f").mkString(",")

            val rowData = flowValues.mkString(",") + "," + speedValues.mkString(",") + "," + densityValues.mkString(",")
            csvWriter.println (rowData)
        }

        csvWriter.flush ()
        csvWriter.close ()
        debug ("exportCSV", s"Exported $numRows rows to log/simulation/$filename")
        println (s"Simulation data exported to: log/simulation/$filename")
    end exportCSV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Export validation report to a .txt file.
     *  @param filename  the output filename (relative to log/simulation/)
     */
    def exportReport (filename: String = "validation_report.txt"): Unit =
        debug ("exportReport", s"Exporting report to log/simulation/$filename")
        val vr = validate()
        val writer = new EasyWriter ("simulation", filename)
        
        writer.println ("=" * 248)
        writer.println ("MACRO-LEVEL VALIDATION (Sensor Aggregates)")
        writer.println ("=" * 248)
        writer.println (f"${"Sensor"}%-9s ${"Flow R²"}%-12s ${"Flow MAE"}%-12s ${"Flow RMSE"}%-12s ${"Flow PeMS Mean"}%-15s ${"Flow Sim Mean"}%-15s ${"Flow NRMSE*100"}%-16s ${"Flow SMAPE"}%-14s ${"Speed R²"}%-12s ${"Speed MAE"}%-12s ${"Speed RMSE"}%-12s ${"Speed PeMS Mean"}%-15s ${"Speed Sim Mean"}%-15s ${"Speed NRMSE*100"}%-16s ${"Speed SMAPE"}%-14s")
        writer.println ("-" * 248)

        for sv <- vr.sensors do
            val s = sv.sensorIdx + 1
            val fm = sv.flowMetrics
            val sm = sv.speedMetrics
            writer.println (f"$s%-9d ${fm.r2}%-12.4f ${fm.mae}%-12.2f ${fm.rmse}%-12.2f ${fm.mean}%-15.2f ${fm.simMean}%-15.2f ${fm.nrmse}%-16.2f ${fm.smape}%-14.2f ${sm.r2}%-12.4f ${sm.mae}%-12.2f ${sm.rmse}%-12.2f ${sm.mean}%-15.2f ${sm.simMean}%-15.2f ${sm.nrmse}%-16.2f ${sm.smape}%-14.2f")
        end for
        writer.println ("=" * 248)

        writer.println ("\n" + "=" * 258)
        writer.println ("MICRO-LEVEL VALIDATION (Lane Detail)")
        writer.println ("=" * 258)
        writer.println (f"${"Sensor"}%-8s ${"Lane"}%-6s ${"Flow R²"}%-12s ${"Flow MAE"}%-12s ${"Flow RMSE"}%-12s ${"Flow PeMS Mean"}%-15s ${"Flow Sim Mean"}%-15s ${"Flow NRMSE*100"}%-16s ${"Flow SMAPE"}%-14s ${"Speed R²"}%-12s ${"Speed MAE"}%-12s ${"Speed RMSE"}%-12s ${"Speed PeMS Mean"}%-15s ${"Speed Sim Mean"}%-15s ${"Speed NRMSE*100"}%-16s ${"Speed SMAPE"}%-14s")
        writer.println ("-" * 258)

        for sv <- vr.sensors do
            cfor (0, nLanes) { l =>
                val fm = sv.flowMicro(l)
                val sm = sv.speedMicro(l)
                val label = if l == 0 then s"${sv.sensorIdx + 1}" else ""
                writer.println (f"$label%-8s ${l + 1}%-6d ${fm.r2}%-12.4f ${fm.mae}%-12.2f ${fm.rmse}%-12.2f ${fm.mean}%-15.2f ${fm.simMean}%-15.2f ${fm.nrmse}%-16.2f ${fm.smape}%-14.2f ${sm.r2}%-12.4f ${sm.mae}%-12.2f ${sm.rmse}%-12.2f ${sm.mean}%-15.2f ${sm.simMean}%-15.2f ${sm.nrmse}%-16.2f ${sm.smape}%-14.2f")
            }
            if sv.sensorIdx < nSensors - 1 then writer.println ("-" * 258)
        end for
        writer.println ("=" * 258)

        writer.println ("\n" + "=" * 60)
        writer.println ("SUMMARY")
        writer.println ("=" * 60)
        writer.println (f"Average Flow NRMSE*100:  ${vr.avgFlowNRMSE}%.4f")
        writer.println (f"Average Speed NRMSE*100: ${vr.avgSpeedNRMSE}%.4f")
        writer.println (f"Fitness (0.2 flow + 0.8 speed NRMSE): ${vr.fitness}%.4f")
        writer.println ("=" * 60)
        
        writer.flush ()
        writer.close ()
        debug ("exportReport", s"Report exported to log/simulation/$filename")
        println (s"Validation report exported to: log/simulation/$filename")
    end exportReport

end SimulationReport


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimulationReport` companion object provides factory methods.
 */
object SimulationReport:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** A model's components and all their subparts, in registration order. */
    private def walkComponents (model: process.Model): List [process.Component] =
        def visit (p: process.Component): List [process.Component] =
            p :: (0 until p.subpart.size).toList.flatMap (i => visit (p.subpart(i)))
        model.getComponents.toList.flatMap (visit)
    end walkComponents

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save lightweight accounting and all registered recorder matrices after a run.
     *  Completed count and mean travel time come from each `Sink`'s own duration
     *  statistic (pooled via `Statistic.aggregate`); throughput is that count over
     *  the model's run length.  Returns a unique prefix (with `folder/` in front when
     *  given) that callers may also use for validation exports.
     *  @param model       completed model (one replication)
     *  @param dynamics    car-following model and solver description
     *  @param parameters  actual parameters applied by the caller, in dynamics order
     *  @param wallTime    caller-measured wall-clock line (omitted when empty)
     *  @param validation  the run's validation result, printed as NRMSE/fitness rows when given
     *  @param folder      existing sub-folder of log/simulation/ to write into ("" = log/simulation/ itself)
     */
    def exportRun (model: process.Model, dynamics: String, parameters: VectorD,
                   wallTime: String = "", validation: Option [ValidationResult] = None,
                   folder: String = ""): String =
        require (model.reps == 1, "run accounting export currently requires one replication")
        val comps      = walkComponents (model)
        val sinks      = comps.collect { case s: process.Sink => s }
        val sinkDur    = ArrayBuffer.from (sinks.map (_.durationStat))
        val agg        = if sinkDur.nonEmpty then Statistic.aggregate (sinkDur, "sinks") else null
        val completed  = if agg != null then agg.num else 0
        val meanTravel = if agg != null then agg.mean else 0.0
        val duration   = model.simDuration
        val throughput = if duration > 0.0 then completed * 3600.0 / duration else 0.0
        val runId  = s"run_${java.time.Instant.now.toEpochMilli}_${java.util.UUID.randomUUID}"
        val prefix = if folder.isEmpty then runId else s"$folder/$runId"
        val writer = new EasyWriter ("simulation", s"${prefix}_summary.txt")
        def emit (line: String): Unit =
            println (line)
            writer.println (line)
        end emit

        def row (label: String, value: String): Unit = emit (f"$label%-30s $value")
        val params = (0 until parameters.dim).map (i => f"${parameters(i)}%.4f").mkString (", ")

        try
            emit ("=" * 60)
            emit ("RUN SUMMARY")
            emit ("=" * 60)
            row ("Model", model.name)
            row ("Dynamics", dynamics)
            row ("Parameters (dynamics order)", params)
            row ("Animated", s"${model.isAnimating}")
            if wallTime.nonEmpty then row ("Wall time", wallTime)
            emit ("-" * 60)
            row ("Simulation duration (s)", f"$duration%.2f")
            row ("Generated actors", s"${model.getNumActors}")
            row ("Completed at sinks", s"$completed")
            row ("Generated minus completed", s"${model.getNumActors - completed}")
            row ("Mean travel time (s)", f"$meanTravel%.2f")
            row ("Throughput (veh/h)", f"$throughput%.2f")
            for vr <- validation do
                emit ("-" * 60)
                row ("Avg flow NRMSE*100", f"${vr.avgFlowNRMSE}%.4f")
                row ("Avg speed NRMSE*100", f"${vr.avgSpeedNRMSE}%.4f")
                row ("Fitness (0.2 flow+0.8 speed)", f"${vr.fitness}%.4f")
            emit ("=" * 60)

            emit ("\n" + "=" * 60)
            emit ("SINKS")
            emit ("=" * 60)
            emit (f"${"Sink"}%-14s ${"Id"}%-6s ${"Completed"}%-12s ${"Mean travel time (s)"}%-20s")
            emit ("-" * 60)
            for s <- sinks do
                val st   = s.durationStat
                val mean = if st.num > 0 then st.mean else 0.0
                emit (f"${s.name}%-14s ${s.id}%-6d ${st.num}%-12d $mean%-20.2f")
            emit ("=" * 60)

            emit ("\nNotes:")
            emit ("- Generated minus completed counts vehicles unfinished or otherwise unaccounted.")
            emit ("- Recorder CSV covers each recorder's configured time window; counts may therefore differ from full-run passage totals.")
            emit ("- Speed is mean detector speed; density is the latest recorded sample in the interval, not an interval mean.")
            emit ("- A zero recorder value may indicate no sample. These aggregates do not establish physical gap safety.")
        finally writer.finish ()

        val csv = new EasyWriter ("simulation", s"${prefix}_recorders.csv")
        def quote (s: String): String = "\"" + s.replace ("\"", "\"\"") + "\""
        try
            csv.println ("recorder_index,component,interval_start_s,interval_end_s,lane_index,count,mean_detector_speed_m_s,latest_density_veh_km_lane")
            var r = 0
            for c <- comps do c match
                case rec: process.Recorder =>
                    val (counts, speeds) = rec.getRecorderMat
                    val density = rec.getDensityMat
                    cfor (0, counts.dim) { i =>
                        cfor (0, counts.dim2) { l =>
                            csv.println (s"$r,${quote (c.name)},${i * rec.rowTime},${(i + 1) * rec.rowTime},$l,${counts(i, l)},${speeds(i, l)},${density(i, l)}")
                        }
                    }
                    r += 1
                case _ =>
        finally csv.finish ()
        println (s"Run accounting saved to log/simulation/${prefix}_summary.txt and ${prefix}_recorders.csv")
        prefix
    end exportRun

    private val debug = debugf ("SimulationReport.companion", false)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a SimulationReport from junction recorder data.
     *  @param junctions  array of junctions with recorder data
     *  @param pemsIndices  indices mapping to PeMS sensors
     *  @param demand     the PeMS demand configuration
     */
    def fromJunctions (junctions: Array [process.Junction],
                       pemsIndices: Array [Int],
                       demand: PeMSDemand): SimulationReport =
        debug ("fromJunctions", s"Creating report: ${junctions.length} junctions, ${pemsIndices.length} sensors")
        debug ("fromJunctions", s"pemsIndices = ${pemsIndices.mkString(",")}")

        val nSensors = pemsIndices.length
        require (nSensors > 0, "at least one sensor is required for a validation report")
        val simFlow    = new Array [MatrixD] (nSensors)
        val simSpeed   = new Array [MatrixD] (nSensors)
        val simDensity = new Array [MatrixD] (nSensors)

        cfor (0, nSensors) { s =>
            val juncIdx = pemsIndices(s)
            debug ("fromJunctions", s"  sensor $s: juncIdx=$juncIdx, junction=${junctions(juncIdx).name}")
            val (flow, speed) = junctions(juncIdx).getRecorderMat
            val density       = junctions(juncIdx).getDensityMat
            debug ("fromJunctions", s"    flow: ${flow.dim}x${flow.dim2}, speed: ${speed.dim}x${speed.dim2}, density: ${density.dim}x${density.dim2}")
            simFlow(s)    = flow
            simSpeed(s)   = speed
            simDensity(s) = density
        }

        debug ("fromJunctions", "Report created successfully")
        new SimulationReport (simFlow, simSpeed, simDensity, demand, nSensors,
                              simFlow(0).dim2, simFlow(0).dim)
    end fromJunctions

end SimulationReport


//Actual flow overtime vs microModelflow vs MesoModelFlow