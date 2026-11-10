//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Nov  9 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    I-210 W Pasadena side-by-side plots: PeMS vs Micro vs Meso vs Mixed.
 *           Copied from redwood101/RedwoodSideBySide and changed for 4 stations,
 *           the Pasadena PeMS files (5-minute bins) and the replication mean CSVs
 *           written by `repsI210W` (10-replication means, same layout as a run CSV).
 */

package scalation
package simulation
package process
package model
package i210w

import scalation.mathstat.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Files and column layout shared by the Pasadena plots.
 *  The run files are the replication means of `repsI210W 2024-12-03`.
 */
object I210WRuns:

    val nLanes   = 5
    val nSensors = 4
    val binMin   = 5.0                                          // minutes per PeMS row

    // Paths are relative to DATA_DIR (data/)
    val pemsFiles = Array ("1-717649ML.csv", "2-717644ML.csv", "3-717642ML.csv", "4-717637ML.csv")   // S1..S4
    val pemsDir   = "Mainline_VDS_I210W_Pasadena/2024-12-03/"

    val repsDir    = "../log/simulation/pads_reps/i210w/2024-12-03/"
    val micro_file = repsDir + "micro_mean_validation_data.csv"   // I210W
    val meso_file  = repsDir + "meso_mean_validation_data.csv"    // I210W_fullMeso
    val mixed_file = repsDir + "mixed_mean_validation_data.csv"   // I210W_withMeso

    // PeMS columns: time, total, avg speed, then (flow, speed) per lane
    val pemsFlowCols  = VectorI (3, 5, 7, 9, 11)
    val pemsSpeedCols = VectorI (4, 6, 8, 10, 12)

    /** Sim CSV lane-flow columns for sensor s (1..4): S1 = 0-4, S2 = 5-9, ... */
    def simFlowCols (s: Int): Range = (s - 1) * nLanes until s * nLanes

    /** Sim CSV lane-speed columns for sensor s (1..4): speeds start after all flows (col 20). */
    def simSpeedCols (s: Int): Range = nSensors * nLanes + (s - 1) * nLanes until nSensors * nLanes + s * nLanes

end I210WRuns

import I210WRuns.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Flow for one sensor (1..4): one plot per lane, PeMS vs Micro vs Meso vs Mixed.
 *  > runMain scalation.simulation.process.model.i210w.i210wFlowBySensor
 */
@main def i210wFlowBySensor (): Unit =

    val sensor = 3                                              // which detector to plot: 1 .. 4

    val readPems  = MatrixD.load (pemsDir + pemsFiles(sensor - 1))
    val readMicro = MatrixD.load (micro_file, skip = 1)         // skip the header row
    val readMeso  = MatrixD.load (meso_file, skip = 1)
    val readMixed = MatrixD.load (mixed_file, skip = 1)

    val cols      = simFlowCols (sensor)
    val pemsFlow  = readPems(?, pemsFlowCols)
    val microFlow = readMicro(?, cols)
    val mesoFlow  = readMeso(?, cols)
    val mixedFlow = readMixed(?, cols)

    val time = VectorD.range (0, pemsFlow.dim) * binMin         // minutes after 6:00, one point per 5-min bin
    val zero = new VectorD (time.dim)                           // baseline so the y-axis starts at 0

    for lane <- 0 until nLanes do
        new PlotM (time, MatrixD (pemsFlow(?, lane), microFlow(?, lane), mesoFlow(?, lane), mixedFlow(?, lane), zero),
                   Array ("PeMS", "Micro", "Meso", "Mixed"),
                   s"I-210 W Sensor $sensor Lane ${lane + 1}: Flow Over Time (veh / 5 min)", lines = true)

end i210wFlowBySensor


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Speed for one sensor (1..4): one plot per lane, all in m/s.
 *  > runMain scalation.simulation.process.model.i210w.i210wSpeedBySensor
 */
@main def i210wSpeedBySensor (): Unit =

    val sensor = 3                                              // which detector to plot: 1 .. 4

    val readPems  = MatrixD.load (pemsDir + pemsFiles(sensor - 1))
    val readMicro = MatrixD.load (micro_file, skip = 1)
    val readMeso  = MatrixD.load (meso_file, skip = 1)
    val readMixed = MatrixD.load (mixed_file, skip = 1)

    val cols       = simSpeedCols (sensor)
    val pemsSpeed  = readPems(?, pemsSpeedCols) * 0.44704       // PeMS lane speeds, mph -> m/s (sim is m/s)
    val microSpeed = readMicro(?, cols)
    val mesoSpeed  = readMeso(?, cols)
    val mixedSpeed = readMixed(?, cols)

    val time = VectorD.range (0, pemsSpeed.dim) * binMin        // minutes after 6:00, one point per 5-min bin
    val zero = new VectorD (time.dim)                           // baseline so the y-axis starts at 0

    for lane <- 0 until nLanes do
        new PlotM (time, MatrixD (pemsSpeed(?, lane), microSpeed(?, lane), mesoSpeed(?, lane), mixedSpeed(?, lane), zero),
                   Array ("PeMS", "Micro", "Meso", "Mixed"),
                   s"I-210 W Sensor $sensor Lane ${lane + 1}: Speed Over Time (m/s)", lines = true)

end i210wSpeedBySensor


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Mean flow by sensor for one lane: one point per sensor (veh per 5 min).
 *  > runMain scalation.simulation.process.model.i210w.i210wFlowProfile
 */
@main def i210wFlowProfile (): Unit =

    val lane = 4                                                // 0..4: which lane (4 = lane 5, the ramp lane)

    val pems      = for s <- 1 to nSensors yield MatrixD.load (pemsDir + pemsFiles(s - 1))
    val micro_Mat = MatrixD.load (micro_file, skip = 1)
    val meso_Mat  = MatrixD.load (meso_file, skip = 1)
    val mixed_Mat = MatrixD.load (mixed_file, skip = 1)

    val pems_prof  = new VectorD (nSensors)
    val micro_prof = new VectorD (nSensors)
    val meso_prof  = new VectorD (nSensors)
    val mixed_prof = new VectorD (nSensors)

    for s <- 1 to nSensors do
        val cols = simFlowCols (s)
        pems_prof(s - 1)  = pems(s - 1)(?, pemsFlowCols).mean(lane)      // column means, then pick the lane
        micro_prof(s - 1) = micro_Mat(?, cols).mean(lane)
        meso_prof(s - 1)  = meso_Mat(?, cols).mean(lane)
        mixed_prof(s - 1) = mixed_Mat(?, cols).mean(lane)
    end for

    val x    = VectorD (1, 2, 3, 4)                              // sensor number along the corridor
    val zero = new VectorD (nSensors)                            // baseline so the y-axis starts at 0
    new PlotM (x, MatrixD (pems_prof, micro_prof, meso_prof, mixed_prof, zero),
               Array ("PeMS", "Micro", "Meso", "Mixed"), s"I-210 W Lane ${lane + 1}: Mean Flow by Sensor (veh / 5 min)", lines = true)

end i210wFlowProfile


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Mean speed by sensor for one lane: one point per sensor (m/s).
 *  > runMain scalation.simulation.process.model.i210w.i210wSpeedProfile
 */
@main def i210wSpeedProfile (): Unit =

    val lane = 4                                                // 0..4: which lane (4 = lane 5, the ramp lane)

    val pems      = for s <- 1 to nSensors yield MatrixD.load (pemsDir + pemsFiles(s - 1))
    val micro_Mat = MatrixD.load (micro_file, skip = 1)
    val meso_Mat  = MatrixD.load (meso_file, skip = 1)
    val mixed_Mat = MatrixD.load (mixed_file, skip = 1)

    val pems_prof  = new VectorD (nSensors)
    val micro_prof = new VectorD (nSensors)
    val meso_prof  = new VectorD (nSensors)
    val mixed_prof = new VectorD (nSensors)

    for s <- 1 to nSensors do
        val cols = simSpeedCols (s)
        pems_prof(s - 1)  = (pems(s - 1)(?, pemsSpeedCols) * 0.44704).mean(lane)   // PeMS mph -> m/s
        micro_prof(s - 1) = micro_Mat(?, cols).mean(lane)
        meso_prof(s - 1)  = meso_Mat(?, cols).mean(lane)
        mixed_prof(s - 1) = mixed_Mat(?, cols).mean(lane)
    end for

    val x    = VectorD (1, 2, 3, 4)                              // sensor number along the corridor
    val zero = new VectorD (nSensors)                            // baseline so the y-axis starts at 0
    new PlotM (x, MatrixD (pems_prof, micro_prof, meso_prof, mixed_prof, zero),
               Array ("PeMS", "Micro", "Meso", "Mixed"), s"I-210 W Lane ${lane + 1}: Mean Speed by Sensor (m/s)", lines = true)

end i210wSpeedProfile
