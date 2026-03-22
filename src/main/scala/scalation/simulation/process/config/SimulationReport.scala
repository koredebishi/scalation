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
package config

import scalation.mathstat.*
import scalation.modeling.{Fit, FitM}

import scala.math.{abs, sqrt}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BasicStats` case class holds basic simulation statistics.
 *  Always generated regardless of demand mode.
 *  @param totalVehicles   total number of vehicles that completed the simulation
 *  @param avgTravelTime   average travel time across all vehicles (seconds)
 *  @param throughput      vehicles per unit time
 *  @param simDuration     total simulation duration (seconds)
 */
case class BasicStats (totalVehicles: Int, avgTravelTime: Double,
                       throughput: Double, simDuration: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MacroMetrics` case class holds macro-level validation metrics per sensor.
 *  @param r2      R-squared (coefficient of determination)
 *  @param mae     Mean Absolute Error
 *  @param rmse    Root Mean Squared Error
 *  @param mean    Mean of observed values
 *  @param nrmse   Normalized RMSE (as percentage, *100)
 *  @param smape   Symmetric Mean Absolute Percentage Error
 */
case class MacroMetrics (r2: Double, mae: Double, rmse: Double, mean: Double, nrmse: Double, smape: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SensorValidation` case class holds validation results for one sensor.
 *  @param sensorIdx    the sensor index (0-based)
 *  @param flowMetrics  macro metrics for flow comparison
 *  @param speedMetrics macro metrics for speed comparison
 *  @param flowMicro    per-lane metrics for flow (Array of 4 lanes)
 *  @param speedMicro   per-lane metrics for speed (Array of 4 lanes)
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

        MacroMetrics(r2, mae, rmse, obsMeanTotal, nrmse100, smape)
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
            results(lane) = MacroMetrics(r2, mae, rmse, obsMean, nrmse100, smape)
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
            val (pemsFlow, pemsSpeed) = PeMSDataHelper.loadMainlineSensor(demand, s)
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

        // Fitness: 20/80 weighted combination (speed dominates; flow already near-optimal via Erlang2S)
        // Flow protection: soft penalty when flow NRMSE exceeds 2.5 (well above unoptimized baseline of 2.03)
        // This prevents the optimizer from trading flow accuracy for speed gains
        val flowThreshold = 2.5
        val flowPenalty   = if avgFlowNRMSE > flowThreshold then 10.0 * (avgFlowNRMSE - flowThreshold) else 0.0
        val fitness       = 0.2 * avgFlowNRMSE + 0.8 * avgSpeedNRMSE + flowPenalty

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
        println ("\n" + "=" * 210)
        println ("MACRO-LEVEL VALIDATION (Sensor Aggregates)")
        println ("=" * 210)
        println (f"${"Sensor"}%-9s ${"Flow R²"}%-12s ${"Flow MAE"}%-12s ${"Flow RMSE"}%-12s ${"Flow Mean"}%-12s ${"Flow NRMSE*100"}%-16s ${"Flow SMAPE"}%-14s ${"Speed R²"}%-12s ${"Speed MAE"}%-12s ${"Speed RMSE"}%-12s ${"Speed Mean"}%-12s ${"Speed NRMSE*100"}%-16s ${"Speed SMAPE"}%-14s")
        println ("-" * 210)

        for sv <- vr.sensors do
            val s = sv.sensorIdx + 1
            val fm = sv.flowMetrics
            val sm = sv.speedMetrics
            println (f"$s%-9d ${fm.r2}%-12.4f ${fm.mae}%-12.2f ${fm.rmse}%-12.2f ${fm.mean}%-12.2f ${fm.nrmse}%-16.2f ${fm.smape}%-14.2f ${sm.r2}%-12.4f ${sm.mae}%-12.2f ${sm.rmse}%-12.2f ${sm.mean}%-12.2f ${sm.nrmse}%-16.2f ${sm.smape}%-14.2f")
        end for
        println ("=" * 210)

        println ("\n" + "=" * 220)
        println ("MICRO-LEVEL VALIDATION (Lane Detail)")
        println ("=" * 220)
        println (f"${"Sensor"}%-8s ${"Lane"}%-6s ${"Flow R²"}%-12s ${"Flow MAE"}%-12s ${"Flow RMSE"}%-12s ${"Flow Mean"}%-12s ${"Flow NRMSE*100"}%-16s ${"Flow SMAPE"}%-14s ${"Speed R²"}%-12s ${"Speed MAE"}%-12s ${"Speed RMSE"}%-12s ${"Speed Mean"}%-12s ${"Speed NRMSE*100"}%-16s ${"Speed SMAPE"}%-14s")
        println ("-" * 220)

        for sv <- vr.sensors do
            cfor (0, nLanes) { l =>
                val fm = sv.flowMicro(l)
                val sm = sv.speedMicro(l)
                val label = if l == 0 then s"${sv.sensorIdx + 1}" else ""
                println (f"$label%-8s ${l + 1}%-6d ${fm.r2}%-12.4f ${fm.mae}%-12.2f ${fm.rmse}%-12.2f ${fm.mean}%-12.2f ${fm.nrmse}%-16.2f ${fm.smape}%-14.2f ${sm.r2}%-12.4f ${sm.mae}%-12.2f ${sm.rmse}%-12.2f ${sm.mean}%-12.2f ${sm.nrmse}%-16.2f ${sm.smape}%-14.2f")
            }
            if sv.sensorIdx < nSensors - 1 then println ("-" * 220)
        end for
        println ("=" * 220)

        println ("\n" + "=" * 60)
        println ("SUMMARY")
        println ("=" * 60)
        println (f"Average Flow NRMSE*100:  ${vr.avgFlowNRMSE}%.4f")
        println (f"Average Speed NRMSE*100: ${vr.avgSpeedNRMSE}%.4f")
        println (f"Fitness (avg NRMSE*100): ${vr.fitness}%.4f")
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
        
        writer.println ("=" * 210)
        writer.println ("MACRO-LEVEL VALIDATION (Sensor Aggregates)")
        writer.println ("=" * 210)
        writer.println (f"${"Sensor"}%-9s ${"Flow R²"}%-12s ${"Flow MAE"}%-12s ${"Flow RMSE"}%-12s ${"Flow Mean"}%-12s ${"Flow NRMSE*100"}%-16s ${"Flow SMAPE"}%-14s ${"Speed R²"}%-12s ${"Speed MAE"}%-12s ${"Speed RMSE"}%-12s ${"Speed Mean"}%-12s ${"Speed NRMSE*100"}%-16s ${"Speed SMAPE"}%-14s")
        writer.println ("-" * 210)

        for sv <- vr.sensors do
            val s = sv.sensorIdx + 1
            val fm = sv.flowMetrics
            val sm = sv.speedMetrics
            writer.println (f"$s%-9d ${fm.r2}%-12.4f ${fm.mae}%-12.2f ${fm.rmse}%-12.2f ${fm.mean}%-12.2f ${fm.nrmse}%-16.2f ${fm.smape}%-14.2f ${sm.r2}%-12.4f ${sm.mae}%-12.2f ${sm.rmse}%-12.2f ${sm.mean}%-12.2f ${sm.nrmse}%-16.2f ${sm.smape}%-14.2f")
        end for
        writer.println ("=" * 210)

        writer.println ("\n" + "=" * 220)
        writer.println ("MICRO-LEVEL VALIDATION (Lane Detail)")
        writer.println ("=" * 220)
        writer.println (f"${"Sensor"}%-8s ${"Lane"}%-6s ${"Flow R²"}%-12s ${"Flow MAE"}%-12s ${"Flow RMSE"}%-12s ${"Flow Mean"}%-12s ${"Flow NRMSE*100"}%-16s ${"Flow SMAPE"}%-14s ${"Speed R²"}%-12s ${"Speed MAE"}%-12s ${"Speed RMSE"}%-12s ${"Speed Mean"}%-12s ${"Speed NRMSE*100"}%-16s ${"Speed SMAPE"}%-14s")
        writer.println ("-" * 220)

        for sv <- vr.sensors do
            cfor (0, nLanes) { l =>
                val fm = sv.flowMicro(l)
                val sm = sv.speedMicro(l)
                val label = if l == 0 then s"${sv.sensorIdx + 1}" else ""
                writer.println (f"$label%-8s ${l + 1}%-6d ${fm.r2}%-12.4f ${fm.mae}%-12.2f ${fm.rmse}%-12.2f ${fm.mean}%-12.2f ${fm.nrmse}%-16.2f ${fm.smape}%-14.2f ${sm.r2}%-12.4f ${sm.mae}%-12.2f ${sm.rmse}%-12.2f ${sm.mean}%-12.2f ${sm.nrmse}%-16.2f ${sm.smape}%-14.2f")
            }
            if sv.sensorIdx < nSensors - 1 then writer.println ("-" * 220)
        end for
        writer.println ("=" * 220)

        writer.println ("\n" + "=" * 60)
        writer.println ("SUMMARY")
        writer.println ("=" * 60)
        writer.println (f"Average Flow NRMSE*100:  ${vr.avgFlowNRMSE}%.4f")
        writer.println (f"Average Speed NRMSE*100: ${vr.avgSpeedNRMSE}%.4f")
        writer.println (f"Fitness (avg NRMSE*100): ${vr.fitness}%.4f")
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
        new SimulationReport (simFlow, simSpeed, simDensity, demand, nSensors)
    end fromJunctions

end SimulationReport
