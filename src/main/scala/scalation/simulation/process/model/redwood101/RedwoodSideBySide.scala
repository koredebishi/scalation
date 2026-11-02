//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Oct 25 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Redwood Creek side-by-side plots: PeMS vs Micro vs Meso vs Mixed.
 *           Copied from calroute101/SideBySideTable and modified for 5 lanes,
 *           the Redwood PeMS files/columns and the sim CSV offsets.
 */

package scalation
package simulation
package process
package model
package redwood101

import scalation.mathstat.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Files and column layout shared by the Redwood plots.
 *  Paste the three run files after `evalRedwood`, `evalRedwoodWithMeso`, `evalRedwoodFullMeso`.
 */
object RedwoodRuns:

    val nLanes   = 5
    val nSensors = 5

    // Paths are relative to DATA_DIR (data/)
    val pemsFiles = Array ("1-404531ML.csv", "2-404532ML.csv", "3-401834ML.csv",
                           "4-401833ML.csv", "5-401929ML.csv")                 // sensor 1..5 (Redwood Creek)
    val pemsDir   = "Mainline_VDS_Redwood_Creek_US101-N/"

    val micro_file = "../log/simulation/redwood/micro/run_1792987801968_77700e7f-f7f7-401f-80f1-cd1ebb5c0ace_validation_data.csv"   // RedwoodCreek101
    val meso_file  = "../log/simulation/redwood/meso/run_1792988125267_dd670ff8-0577-4fbf-9eea-27952733d060_validation_data.csv"    // RedwoodCreek101_fullMeso
    val mixed_file = "../log/simulation/redwood/mixed/run_1792988053686_c4630ce3-04e7-4bef-8441-5041e4d0f9d7_validation_data.csv"   // RedwoodCreek101_withMeso

    // PeMS columns: time, total, avg speed, then (flow, speed) per lane
    val pemsFlowCols  = VectorI (3, 5, 7, 9, 11)
    val pemsSpeedCols = VectorI (4, 6, 8, 10, 12)

    /** Sim CSV lane-flow columns for sensor s (1..5): S1 = 0-4, S2 = 5-9, ... */
    def simFlowCols (s: Int): Range = (s - 1) * nLanes until s * nLanes

    /** Sim CSV lane-speed columns for sensor s (1..5): speeds start after all flows (col 25). */
    def simSpeedCols (s: Int): Range = nSensors * nLanes + (s - 1) * nLanes until nSensors * nLanes + s * nLanes

end RedwoodRuns

import RedwoodRuns.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Flow for one sensor (1..5): one plot per lane, PeMS vs Micro vs Meso vs Mixed.
 *  > runMain scalation.simulation.process.model.redwood101.redwoodFlowBySensor
 */
@main def redwoodFlowBySensor (): Unit =

    val sensor = 2                                              // which detector to plot: 1 .. 5

    val readPems  = MatrixD.load (pemsDir + pemsFiles(sensor - 1))
    val readMicro = MatrixD.load (micro_file, skip = 1)         // skip the header row
    val readMeso  = MatrixD.load (meso_file, skip = 1)
    val readMixed = MatrixD.load (mixed_file, skip = 1)

    val cols      = simFlowCols (sensor)
    val pemsFlow  = readPems(?, pemsFlowCols)
    val microFlow = readMicro(?, cols)
    val mesoFlow  = readMeso(?, cols)
    val mixedFlow = readMixed(?, cols)

    val time = VectorD.range (0, pemsFlow.dim) * 15.0           // minutes after 6:00, one point per 15-min bin
    val zero = new VectorD (time.dim)                           // baseline so the y-axis starts at 0

    for lane <- 0 until nLanes do
        new PlotM (time, MatrixD (pemsFlow(?, lane), microFlow(?, lane), mesoFlow(?, lane), mixedFlow(?, lane), zero),
                   Array ("PeMS", "Micro", "Meso", "Mixed"),
                   s"Redwood Sensor $sensor Lane ${lane + 1}: Flow Over Time", lines = true)

end redwoodFlowBySensor


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Speed for one sensor (1..5): one plot per lane, all in m/s.
 *  > runMain scalation.simulation.process.model.redwood101.redwoodSpeedBySensor
 */
@main def redwoodSpeedBySensor (): Unit =

    val sensor = 2                                              // which detector to plot: 1 .. 5

    val readPems  = MatrixD.load (pemsDir + pemsFiles(sensor - 1))
    val readMicro = MatrixD.load (micro_file, skip = 1)
    val readMeso  = MatrixD.load (meso_file, skip = 1)
    val readMixed = MatrixD.load (mixed_file, skip = 1)

    val cols       = simSpeedCols (sensor)
    val pemsSpeed  = readPems(?, pemsSpeedCols) * 0.44704       // PeMS lane speeds, mph -> m/s (sim is m/s)
    val microSpeed = readMicro(?, cols)
    val mesoSpeed  = readMeso(?, cols)
    val mixedSpeed = readMixed(?, cols)

    val time = VectorD.range (0, pemsSpeed.dim) * 15.0          // minutes after 6:00, one point per 15-min bin
    val zero = new VectorD (time.dim)                           // baseline so the y-axis starts at 0

//    for lane <- 0 until nLanes do
//        new PlotM (time, MatrixD (pemsSpeed(?, lane), microSpeed(?, lane), mesoSpeed(?, lane), mixedSpeed(?, lane), zero),
//                   Array ("PeMS", "Micro", "Meso", "Mixed"),
//                   s"Redwood Sensor $sensor Lane ${lane + 1}: Speed Over Time (m/s)", lines = true)
    for lane <- 0 until nLanes do
        new PlotM (time, MatrixD (pemsSpeed(?, lane), microSpeed(?, lane), mesoSpeed(?, lane), zero),
            Array ("PeMS", "Micro", "Meso"),
            s"Redwood Sensor $sensor Lane ${lane + 1}: Speed Over Time (m/s)", lines = true)

end redwoodSpeedBySensor


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Mean flow by sensor for one lane: one point per sensor (veh per 15 min).
 *  > runMain scalation.simulation.process.model.redwood101.redwoodFlowProfile
 */
@main def redwoodFlowProfile (): Unit =

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

    val x    = VectorD (1, 2, 3, 4, 5)                           // sensor number along the corridor
    val zero = new VectorD (nSensors)                            // baseline so the y-axis starts at 0
    new PlotM (x, MatrixD (pems_prof, micro_prof, meso_prof, mixed_prof, zero),
               Array ("PeMS", "Micro", "Meso", "Mixed"), s"Redwood Lane ${lane + 1}: Mean Flow by Sensor (veh/15 min)", lines = true)

end redwoodFlowProfile


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Mean speed by sensor for one lane: one point per sensor (m/s).
 *  > runMain scalation.simulation.process.model.redwood101.redwoodSpeedProfile
 */
@main def redwoodSpeedProfile (): Unit =

    val lane = 1                                                // 0..4: which lane (1 = lane 2)

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

    val x    = VectorD (1, 2, 3, 4, 5)                           // sensor number along the corridor
    val zero = new VectorD (nSensors)                            // baseline so the y-axis starts at 0
    new PlotM (x, MatrixD (pems_prof, micro_prof, meso_prof, mixed_prof, zero),
               Array ("PeMS", "Micro", "Meso", "Mixed"), s"Redwood Lane ${lane + 1}: Mean Speed by Sensor (m/s)", lines = true)

end redwoodSpeedProfile
