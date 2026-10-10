package scalation
package simulation
package process
package model
package calroute101

import scalation.mathstat.*


@main def flowMesoVsMicro (): Unit =


    // Paths are relative to DATA_DIR (data/).  Sensor 1 of CalRoute101_3 is VDS 401112 (Donald Doyle).
    val pems_sensor1 = "Mainline_VDS_Donald_Doyle/1-401112ML.csv"
//    val meso_flow  = "../log/simulation/meso/run_1791042159414_225a3726-1923-4084-b9f6-8da344f5f31c_validation_data.csv"    // CalRoute101_3_fullMeso
//    val micro_flow = "../log/simulation/micro/run_1791041931586_155e8ba9-b6f4-4649-b119-3348d5948a65_validation_data.csv"   // CalRoute101_3 (asymmetric MOBIL)


    val meso_flow = "../log/simulation/meso/baseline/run_1790647033619_0fa8971d-7647-49f1-bd18-d342645cee83_validation_data.csv" // CalRoute101_3_fullMeso
    val micro_flow = "../log/simulation/micro/baseline/run_1790646441479_53003381-ec03-4d22-8b60-71050120ba06_validation_data.csv"
    //val mixed_flow = "../log/simulation/mixed/run_1791042146806_e4eb2909-4d50-4882-91c1-3b9ac1eabe12_validation_data.csv"   // CalRoute101_3_withMeso (asymmetric MOBIL)

    val readPems  = MatrixD.load (pems_sensor1)                 // col 0 = date, then (flow, speed) per lane, total, avg
    val readMeso  = MatrixD.load (meso_flow, skip = 1)          // skip the header row; cols 0-3 = S1L1..S1L4_Flow
    val readMicro = MatrixD.load (micro_flow, skip = 1)
    //val readMixed = MatrixD.load (mixed_flow, skip = 1)

    val pemsFlow  = readPems(?, VectorI (1, 3, 5, 7))           // lane 1-4 flows, the same columns the validation uses
    val mesoFlow  = readMeso(?, 0 until 4)
    val microFlow = readMicro(?, 0 until 4)
    //val mixedFlow = readMixed(?, 0 until 4)

    val time = VectorD.range (0, pemsFlow.dim) * 15.0           // minutes after 6:00, one point per 15-min bin
    val zero = new VectorD (time.dim)                           // baseline so the y-axis starts at 0 (PlotM uses the data min)

    // one plot per lane: PeMS vs Micro vs Meso
    for lane <- 0 until 4 do
        new PlotM (time, MatrixD (pemsFlow(?, lane), microFlow(?, lane), mesoFlow(?, lane), zero),
                   Array ("PeMS", "Micro", "Meso"), s"Sensor 1 Lane ${lane + 1}: Flow Over Time", lines = true)


//    // one plot per lane: PeMS vs Micro
//    for lane <- 0 until 4 do
//        new PlotM (time, MatrixD (pemsFlow(?, lane), microFlow(?, lane), zero),
//                   Array ("PeMS", "Micro"), s"Sensor 1 Lane ${lane + 1}: PeMS vs Micro Flow", lines = true)

//    // one plot per lane: PeMS vs Meso
//    for lane <- 0 until 4 do
//        new PlotM (time, MatrixD (pemsFlow(?, lane), mesoFlow(?, lane), zero),
//                   Array ("PeMS", "Meso"), s"Sensor 1 Lane ${lane + 1}: PeMS vs Meso Flow", lines = true)
//
//    // one plot per lane: PeMS vs Mixed
//    for lane <- 0 until 4 do
//        new PlotM (time, MatrixD (pemsFlow(?, lane), mixedFlow(?, lane), zero),
//                   Array ("PeMS", "Mixed"), s"Sensor 1 Lane ${lane + 1}: PeMS vs Mixed Flow", lines = true

    // all four lanes per source
//    new PlotM (time, pemsFlow.transpose,  Array ("L1", "L2", "L3", "L4"), "Sensor 1 PeMS flow by lane", lines = true)
//    new PlotM (time, microFlow.transpose, Array ("L1", "L2", "L3", "L4"), "Sensor 1 Micro flow by lane", lines = true)
//    new PlotM (time, mesoFlow.transpose,  Array ("L1", "L2", "L3", "L4"), "Sensor 1 Meso flow by lane", lines = true)


end flowMesoVsMicro


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Same plots as `flowMesoVsMicro`, but for ANY sensor (1..5): one plot per lane,
 *  PeMS vs Micro vs Meso vs Mixed.  Meso and Mixed only overlap at sensor 1
 *  (seg 0 is the same meso link in both); from sensor 2 on they separate.
 *  > runMain scalation.simulation.process.model.calroute101.flowBySensor
 */
@main def flowBySensor (): Unit =

    val sensor = 1                                              // which detector to plot: 1 .. 5

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //  Files: PeMS file for this sensor + the three simulation runs
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    val pemsFiles  = Array ("1-401112ML.csv", "2-401104ML.csv", "3-400712ML.csv",
                            "4-400450ML.csv", "5-407463ML.csv")              // sensor 1..5 (Donald Doyle)
    val pems_file  = "Mainline_VDS_Donald_Doyle/" + pemsFiles(sensor - 1)    // pick this sensor's file
//    val meso_flow  = "../log/simulation/meso/run_1791042159414_225a3726-1923-4084-b9f6-8da344f5f31c_validation_data.csv"    // CalRoute101_3_fullMeso
//    val micro_flow = "../log/simulation/micro/run_1791041931586_155e8ba9-b6f4-4649-b119-3348d5948a65_validation_data.csv"   // CalRoute101_3 (asymmetric MOBIL)
//    val mixed_flow = "../log/simulation/mixed/run_1791042146806_e4eb2909-4d50-4882-91c1-3b9ac1eabe12_validation_data.csv"   // CalRoute101_3_withMeso (asymmetric MOBIL)


//  val meso_flow = "../log/simulation/meso/baseline/run_1790647033619_0fa8971d-7647-49f1-bd18-d342645cee83_validation_data.csv" // baseline full meso
//  val micro_flow = "../log/simulation/micro/baseline/run_1790646441479_53003381-ec03-4d22-8b60-71050120ba06_validation_data.csv"  // baseline micro
    val meso_flow  = "../log/simulation/meso/run_1791222291028_bfac4e3a-74e3-4ee1-a7d5-aab9ec8c1240_validation_data.csv"    // CalRoute101_3_fullMeso (car v0, running part, queue bound)
    val micro_flow = "../log/simulation/micro/run_1791212068490_d2a4030c-d320-479a-8344-2ab535ce0104_validation_data.csv"   // CalRoute101_3 (simple lane rule)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //  Load and slice: 4 lane-flow columns for this sensor
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    val readPems  = MatrixD.load (pems_file)                    // col 0 = date, then (flow, speed) per lane
    val readMeso  = MatrixD.load (meso_flow, skip = 1)          // skip the header row
    val readMicro = MatrixD.load (micro_flow, skip = 1)
    //val readMixed = MatrixD.load (mixed_flow, skip = 1)

    val cols      = (sensor - 1) * 4 until sensor * 4           // this sensor's lane flows: S1 = 0-3, S2 = 4-7, …

    val pemsFlow  = readPems(?, VectorI (1, 3, 5, 7))           // same columns in every PeMS file
    val mesoFlow  = readMeso(?, cols)
    val microFlow = readMicro(?, cols)
    //val mixedFlow = readMixed(?, cols)

    // actual - mean

    val lane1flowV = pemsFlow(?, 0)
    val lane2flowV = pemsFlow(?, 1)
    val lane3flowV = pemsFlow(?, 2)
    val lane4flowV = pemsFlow(?, 3)


    // ::::::::::::::::::::::::::::::
    val lane1flowVP = microFlow(?, 0)
    val lane2flowVP = microFlow(?, 1)
    val lane3flowVP = microFlow(?, 2)
    val lane4flowVP = microFlow(?, 3)
//
//    println(s"the flow $lane1flowV")
//
    val sst_lane1 = (lane1flowV - lane1flowV.mean).normSq
    val sst_lane2 = (lane2flowV - lane2flowV.mean).normSq
    val sst_lane3 = (lane3flowV - lane3flowV.mean).normSq
    val sst_lane4 = (lane4flowV - lane4flowV.mean).normSq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::
    val sse_laneM1 = (lane1flowV - lane1flowVP).normSq
    val sse_laneM2 = (lane2flowV - lane2flowVP).normSq
    val sse_laneM3 = (lane3flowV - lane3flowVP).normSq
    val sse_laneM4 = (lane4flowV - lane4flowVP).normSq

    val r_sq1 = 1 - sse_laneM1 / sst_lane1
    val r_sq2 = 1 - sse_laneM2 / sst_lane2
    val r_sq3 = 1 - sse_laneM3 / sst_lane3
    val r_sq4 = 1 - sse_laneM4 / sst_lane4

    

//    println(s" the sst for each lanes $sst_lane1 , $sst_lane2, $sst_lane3, $sst_lane4")
//    println(s" the sse for each lanes $sse_laneM1 , $sse_laneM2, $sse_laneM3, $sse_laneM4")
//    println(s" the r_sq for each lanes $r_sq1 , $r_sq2, $r_sq3, $r_sq4")


    val time = VectorD.range (0, pemsFlow.dim) * 15.0           // minutes after 6:00, one point per 15-min bin
    val zero = new VectorD (time.dim)                           // baseline so the y-axis starts at 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //  Plot: one window per lane
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    for lane <- 0 until 4 do
        new PlotM (time, MatrixD (pemsFlow(?, lane), microFlow(?, lane), mesoFlow(?, lane), zero),
                   Array ("PeMS", "Micro", "Meso"), s"Sensor $sensor Lane ${lane + 1}: Flow Over Time", lines = true)

end flowBySensor


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Speed comparison for ANY sensor (1..5): one plot per lane, PeMS vs Micro vs Meso,
 *  all in m/s.  Uses the same runs as `flowBySensor`.
 *  > runMain scalation.simulation.process.model.calroute101.speedBySensor
 */
@main def speedBySensor (): Unit =

    val sensor = 3                                              // which detector to plot: 1 .. 5

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //  Files: PeMS file for this sensor + the simulation runs (meso speed cap, simple lane rule)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    val pemsFiles  = Array ("1-401112ML.csv", "2-401104ML.csv", "3-400712ML.csv",
                            "4-400450ML.csv", "5-407463ML.csv")              // sensor 1..5 (Donald Doyle)
    val pems_file  = "Mainline_VDS_Donald_Doyle/" + pemsFiles(sensor - 1)    // pick this sensor's file
//  val meso_file  = "../log/simulation/meso/baseline/run_1790647033619_0fa8971d-7647-49f1-bd18-d342645cee83_validation_data.csv"   // baseline full meso
//  val micro_file = "../log/simulation/micro/baseline/run_1790646441479_53003381-ec03-4d22-8b60-71050120ba06_validation_data.csv"  // baseline micro
    val meso_file  = "../log/simulation/meso/run_1791222291028_bfac4e3a-74e3-4ee1-a7d5-aab9ec8c1240_validation_data.csv"    // CalRoute101_3_fullMeso (car v0, running part, queue bound)
    val micro_file = "../log/simulation/micro/run_1791212068490_d2a4030c-d320-479a-8344-2ab535ce0104_validation_data.csv"   // CalRoute101_3 (simple lane rule)
    val mixed_file = "../log/simulation/mixed/run_1791222266530_a76f5cae-06c7-46e9-96cf-108f273fe97e_validation_data.csv"   // CalRoute101_3_withMeso (car v0, running part, queue bound)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //  Load and slice: 4 lane-speed columns for this sensor
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    val readPems  = MatrixD.load (pems_file)                    // col 0 = date, then (flow, speed) per lane
    val readMeso  = MatrixD.load (meso_file, skip = 1)          // skip the header row
    val readMicro = MatrixD.load (micro_file, skip = 1)
    val readMixed = MatrixD.load (mixed_file, skip = 1)

    val colSpeed   = 20 + (sensor - 1) * 4 until 20 + sensor * 4   // sim lane speeds: S1 = 20-23, S2 = 24-27, …
    val pemsSpeed  = readPems(?, VectorI (2, 4, 6, 8)) * 0.44704   // PeMS lane speeds, mph -> m/s (sim is m/s)
    val mesoSpeed  = readMeso(?, colSpeed)
    val microSpeed = readMicro(?, colSpeed)
    val mixedSpeed = readMixed(?, colSpeed)

    val time = VectorD.range (0, pemsSpeed.dim) * 15.0          // minutes after 6:00, one point per 15-min bin
    val zero = new VectorD (time.dim)                           // baseline so the y-axis starts at 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //  Plot: one window per lane
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    for lane <- 0 until 4 do
        new PlotM (time, MatrixD (pemsSpeed(?, lane), microSpeed(?, lane), mesoSpeed(?, lane), mixedSpeed(?, lane), zero),
                   Array ("PeMS", "Micro", "Meso", "Mixed"), s"Sensor $sensor Lane ${lane + 1}: Speed Over Time (m/s)", lines = true)

end speedBySensor




@main def flowMap(): Unit =

    // The path to the cvs
    val data = "Mainline_VDS_Donald_Doyle/1-401112ML.csv"

    val flowLine = MatrixD.load(data, skipCol = 1)

    val flowCol  = flowLine(?, 5)

    println(s"The flowLine $flowLine")
    // load the csv

    // use the csv

    // graph the csv