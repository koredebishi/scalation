//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Fri Jan 31 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    PeMS Data Plotter for Traffic Simulation (Standalone Visualization)
 */

package scalation
package simulation
package process
package config

import scalation.mathstat.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PeMSDataPlotter` object provides standalone visualization of PeMS sensor data.
 *  No simulation required — pure data exploration.
 */
object PeMSDataPlotter:

    private val debug = debugf ("PeMSDataPlotter", true)

    private val nSensors = 5                                         // number of mainline sensors
    private val nLanes = 4                                           // lanes per sensor
    private val nRows = 48                                           // time intervals (15-min, 6am-6pm)
    private val factor = 0.44704                                     // mph to m/s

    // Time axis: 0, 1, 2, ..., 47 (15-min intervals)
    private val timeAxis = VectorD.range (0, nRows)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot flow and speed for a single sensor.
     *  @param sensorIdx  sensor index (0-based, 0 to 4)
     *  @param demand     PeMS demand configuration specifying data source
     */
    def plotSensor (sensorIdx: Int, demand: PeMSDemand): Unit =
        require (sensorIdx >= 0 && sensorIdx < nSensors,
                 s"sensorIdx must be 0-${nSensors - 1}, got $sensorIdx")

        val (flowMat, speedMat) = PeMSDataHelper.loadMainlineSensor (demand, sensorIdx)

        // Aggregate: total flow and average speed per time interval
        val totalFlow = flowMat.sumVr                                // sum across lanes
        val avgSpeed = VectorD (for i <- 0 until speedMat.dim yield speedMat(i).sum / speedMat.dim2)

        val sensorName = s"Sensor ${sensorIdx + 1}"

        new Plot (timeAxis, totalFlow, null,
                  s"$sensorName: Total Flow (vehicles per 15-min)", lines = true)

        new Plot (timeAxis, avgSpeed, null,
                  s"$sensorName: Average Speed (m/s)", lines = true)

        println (s"[PeMSDataPlotter] Plotted $sensorName")
        println (s"  Flow range: ${totalFlow.min.toInt} - ${totalFlow.max.toInt} vehicles")
        println (s"  Speed range: ${avgSpeed.min.toInt} - ${avgSpeed.max.toInt} m/s")
    end plotSensor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot all sensors on the same chart for comparison.
     *  @param demand  PeMS demand configuration specifying data source
     */
    def plotAllSensors (demand: PeMSDemand): Unit =
        // Collect total flow for all sensors
        val flowVectors = new Array [VectorD] (nSensors)
        val speedVectors = new Array [VectorD] (nSensors)

        cfor (0, nSensors) { s =>
            val (flowMat, speedMat) = PeMSDataHelper.loadMainlineSensor (demand, s)
            flowVectors(s) = flowMat.sumVr
            speedVectors(s) = VectorD (for i <- 0 until speedMat.dim yield speedMat(i).sum / speedMat.dim2)
        }

        // Plot first two sensors for comparison
        new Plot (timeAxis, flowVectors(0), flowVectors(1),
                  "All Sensors: Flow Comparison (S1=blue, S2=red)", lines = true)

        new Plot (timeAxis, speedVectors(0), speedVectors(1),
                  "All Sensors: Speed Comparison (S1=blue, S2=red)", lines = true)

        println ("[PeMSDataPlotter] Plotted all sensors comparison")
        cfor (0, nSensors) { s =>
            println (f"  Sensor ${s + 1}: Flow ${flowVectors(s).sum.toInt}%6d total, Speed ${speedVectors(s).mean}%5.1f m/s avg")
        }
    end plotAllSensors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot a range of sensors.
     *  @param from    starting sensor index (0-based, inclusive)
     *  @param to      ending sensor index (0-based, inclusive)
     *  @param demand  PeMS demand configuration specifying data source
     */
    def plotSensorRange (from: Int, to: Int, demand: PeMSDemand): Unit =
        require (from >= 0 && from < nSensors, s"from must be 0-${nSensors - 1}")
        require (to >= from && to < nSensors, s"to must be >= from and < $nSensors")

        cfor (from, to + 1) { s =>
            plotSensor (s, demand)
        }
        println (s"[PeMSDataPlotter] Plotted sensors ${from + 1} to ${to + 1}")
    end plotSensorRange

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot lane-by-lane comparison for a single sensor.
     *  @param sensorIdx  sensor index (0-based)
     *  @param demand     PeMS demand configuration specifying data source
     */
    def plotLaneComparison (sensorIdx: Int, demand: PeMSDemand): Unit =
        require (sensorIdx >= 0 && sensorIdx < nSensors,
                 s"sensorIdx must be 0-${nSensors - 1}")

        val (flowMat, speedMat) = PeMSDataHelper.loadMainlineSensor (demand, sensorIdx)

        val sensorName = s"Sensor ${sensorIdx + 1}"

        // Plot lanes 1 vs 2
        new Plot (timeAxis, flowMat.col (0), flowMat.col (1),
                  s"$sensorName: Flow by Lane (L1=blue, L2=red)", lines = true)

        // Plot lanes 3 vs 4
        new Plot (timeAxis, flowMat.col (2), flowMat.col (3),
                  s"$sensorName: Flow by Lane (L3=blue, L4=red)", lines = true)

        println (s"[PeMSDataPlotter] Plotted lane comparison for $sensorName")
        cfor (0, nLanes) { l =>
            println (f"  Lane ${l + 1}: Flow ${flowMat.col (l).sum.toInt}%5d total, Speed ${speedMat.col (l).mean}%5.1f m/s avg")
        }
    end plotLaneComparison

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print summary statistics for all sensors (no plots).
     *  @param demand  PeMS demand configuration specifying data source
     */
    def printSummary (demand: PeMSDemand): Unit =
        println ("\n" + "=" * 80)
        println ("PeMS DATA SUMMARY")
        println ("=" * 80)
        println (f"${"Sensor"}%-10s ${"Total Flow"}%-15s ${"Avg Speed (m/s)"}%-18s ${"Peak Flow"}%-12s ${"Min Speed"}%-12s")
        println ("-" * 80)

        cfor (0, nSensors) { s =>
            val (flowMat, speedMat) = PeMSDataHelper.loadMainlineSensor (demand, s)
            val totalFlow = flowMat.sumVr
            val avgSpeed = VectorD (for i <- 0 until speedMat.dim yield speedMat(i).sum / speedMat.dim2)

            println (f"${s + 1}%-10d ${totalFlow.sum.toInt}%-15d ${avgSpeed.mean}%-18.2f ${totalFlow.max.toInt}%-12d ${avgSpeed.min}%-12.2f")
        }
        println ("=" * 80)
    end printSummary

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot time-of-day patterns showing traffic evolution.
     *  Groups data into morning, midday, and afternoon periods.
     *  @param demand  PeMS demand configuration specifying data source
     */
    def plotTimeOfDayPatterns (demand: PeMSDemand): Unit =
        // Periods: 6-9am (0-12), 9am-3pm (12-36), 3-6pm (36-48)
        val morningIdx = (0 until 12).toArray
        val middayIdx = (12 until 36).toArray
        val afternoonIdx = (36 until 48).toArray

        println ("\n" + "=" * 80)
        println ("TIME-OF-DAY PATTERNS")
        println ("=" * 80)
        println (f"${"Sensor"}%-10s ${"Morning (6-9am)"}%-18s ${"Midday (9am-3pm)"}%-20s ${"Afternoon (3-6pm)"}%-18s")
        println (f"${""}%-10s ${"Flow / Speed"}%-18s ${"Flow / Speed"}%-20s ${"Flow / Speed"}%-18s")
        println ("-" * 80)

        cfor (0, nSensors) { s =>
            val (flowMat, speedMat) = PeMSDataHelper.loadMainlineSensor (demand, s)
            val totalFlow = flowMat.sumVr
            val avgSpeed = VectorD (for i <- 0 until speedMat.dim yield speedMat(i).sum / speedMat.dim2)

            val morningFlow = morningIdx.map (totalFlow(_)).sum / morningIdx.length
            val morningSpeed = morningIdx.map (avgSpeed(_)).sum / morningIdx.length

            val middayFlow = middayIdx.map (totalFlow(_)).sum / middayIdx.length
            val middaySpeed = middayIdx.map (avgSpeed(_)).sum / middayIdx.length

            val afternoonFlow = afternoonIdx.map (totalFlow(_)).sum / afternoonIdx.length
            val afternoonSpeed = afternoonIdx.map (avgSpeed(_)).sum / afternoonIdx.length

            println (f"${s + 1}%-10d ${morningFlow.toInt}%5d / ${morningSpeed}%5.1f      ${middayFlow.toInt}%5d / ${middaySpeed}%5.1f        ${afternoonFlow.toInt}%5d / ${afternoonSpeed}%5.1f")
        }
        println ("=" * 80)
    end plotTimeOfDayPatterns

end PeMSDataPlotter


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run PeMS data visualization.
 *  > runMain scalation.simulation.process.config.runPlotPeMS
 */
@main def runPlotPeMS (): Unit =
    banner ("PeMS Data Visualization - Donald Doyle Corridor")

    // Use Donald Doyle corridor configuration
    val demand = PeMSDemand.US101_DonaldDoyle ()

    // Print summary first
    PeMSDataPlotter.printSummary (demand)

    // Time-of-day analysis
    PeMSDataPlotter.plotTimeOfDayPatterns (demand)

    // Plot sensor 1 (entry point)
    PeMSDataPlotter.plotSensor (0, demand)

    // Plot lane comparison for sensor 1
    PeMSDataPlotter.plotLaneComparison (0, demand)

    println ("\n[runPlotPeMS] Visualization complete. Close plot windows to exit.")
end runPlotPeMS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run full PeMS data exploration for all sensors.
 *  > runMain scalation.simulation.process.config.runPlotPeMSAll
 */
@main def runPlotPeMSAll (): Unit =
    banner ("PeMS Full Data Exploration - Donald Doyle Corridor")

    // Use Donald Doyle corridor configuration
    val demand = PeMSDemand.US101_DonaldDoyle ()

    PeMSDataPlotter.printSummary (demand)
    PeMSDataPlotter.plotTimeOfDayPatterns (demand)

    // Plot all sensors
    cfor (0, 5) { s =>
        PeMSDataPlotter.plotSensor (s, demand)
    }

    println ("\n[runPlotPeMSAll] All sensors plotted. Close windows to exit.")
end runPlotPeMSAll
