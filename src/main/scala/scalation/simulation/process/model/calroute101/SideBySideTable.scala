package scalation
package simulation
package process
package model
package calroute101

import scalation.mathstat.*


@main def flowMesoVsMicro (): Unit =


    // Paths are relative to DATA_DIR (data/).  Sensor 1 of CalRoute101_3 is VDS 401112 (Donald Doyle).
    val pems_sensor1 = "Mainline_VDS_Donald_Doyle/1-401112ML.csv"
    val meso_flow  = "../log/simulation/meso/run_1791042159414_225a3726-1923-4084-b9f6-8da344f5f31c_validation_data.csv"    // CalRoute101_3_fullMeso
    val micro_flow = "../log/simulation/micro/run_1791041931586_155e8ba9-b6f4-4649-b119-3348d5948a65_validation_data.csv"   // CalRoute101_3 (asymmetric MOBIL)
    val mixed_flow = "../log/simulation/mixed/run_1791042146806_e4eb2909-4d50-4882-91c1-3b9ac1eabe12_validation_data.csv"   // CalRoute101_3_withMeso (asymmetric MOBIL)

    val readPems  = MatrixD.load (pems_sensor1)                 // col 0 = date, then (flow, speed) per lane, total, avg
    val readMeso  = MatrixD.load (meso_flow, skip = 1)          // skip the header row; cols 0-3 = S1L1..S1L4_Flow
    val readMicro = MatrixD.load (micro_flow, skip = 1)
    val readMixed = MatrixD.load (mixed_flow, skip = 1)

    val pemsFlow  = readPems(?, VectorI (1, 3, 5, 7))           // lane 1-4 flows, the same columns the validation uses
    val mesoFlow  = readMeso(?, 0 until 4)
    val microFlow = readMicro(?, 0 until 4)
    val mixedFlow = readMixed(?, 0 until 4)

    val time = VectorD.range (0, pemsFlow.dim) * 15.0           // minutes after 6:00, one point per 15-min bin
    val zero = new VectorD (time.dim)                           // baseline so the y-axis starts at 0 (PlotM uses the data min)

    // one plot per lane: PeMS vs Micro vs Meso
    for lane <- 0 until 4 do
        new PlotM (time, MatrixD (pemsFlow(?, lane), microFlow(?, lane), mesoFlow(?, lane), mixedFlow(?, lane), zero),
                   Array ("PeMS", "Micro", "Meso", "Mixed"), s"Sensor 1 Lane ${lane + 1}: Flow Over Time", lines = true)

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

    val sensor = 2                                              // which detector to plot: 1 .. 5

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //  Files: PeMS file for this sensor + the three simulation runs
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    val pemsFiles  = Array ("1-401112ML.csv", "2-401104ML.csv", "3-400712ML.csv",
                            "4-400450ML.csv", "5-407463ML.csv")              // sensor 1..5 (Donald Doyle)
    val pems_file  = "Mainline_VDS_Donald_Doyle/" + pemsFiles(sensor - 1)    // pick this sensor's file
    val meso_flow  = "../log/simulation/meso/run_1791042159414_225a3726-1923-4084-b9f6-8da344f5f31c_validation_data.csv"    // CalRoute101_3_fullMeso
    val micro_flow = "../log/simulation/micro/run_1791041931586_155e8ba9-b6f4-4649-b119-3348d5948a65_validation_data.csv"   // CalRoute101_3 (asymmetric MOBIL)
    val mixed_flow = "../log/simulation/mixed/run_1791042146806_e4eb2909-4d50-4882-91c1-3b9ac1eabe12_validation_data.csv"   // CalRoute101_3_withMeso (asymmetric MOBIL)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //  Load and slice: 4 lane-flow columns for this sensor
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    val readPems  = MatrixD.load (pems_file)                    // col 0 = date, then (flow, speed) per lane
    val readMeso  = MatrixD.load (meso_flow, skip = 1)          // skip the header row
    val readMicro = MatrixD.load (micro_flow, skip = 1)
    val readMixed = MatrixD.load (mixed_flow, skip = 1)

    val cols      = (sensor - 1) * 4 until sensor * 4           // this sensor's lane flows: S1 = 0-3, S2 = 4-7, …
    val pemsFlow  = readPems(?, VectorI (1, 3, 5, 7))           // same columns in every PeMS file
    val mesoFlow  = readMeso(?, cols)
    val microFlow = readMicro(?, cols)
    val mixedFlow = readMixed(?, cols)

    val time = VectorD.range (0, pemsFlow.dim) * 15.0           // minutes after 6:00, one point per 15-min bin
    val zero = new VectorD (time.dim)                           // baseline so the y-axis starts at 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //  Plot: one window per lane
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    for lane <- 0 until 4 do
        new PlotM (time, MatrixD (pemsFlow(?, lane), microFlow(?, lane), mesoFlow(?, lane), mixedFlow(?, lane), zero),
                   Array ("PeMS", "Micro", "Meso", "Mixed"), s"Sensor $sensor Lane ${lane + 1}: Flow Over Time", lines = true)

end flowBySensor




@main def flowMap(): Unit =

    // The path to the cvs
    val data = "Mainline_VDS_Donald_Doyle/1-401112ML.csv"

    val flowLine = MatrixD.load(data, skipCol = 1)

    val flowCol  = flowLine(?, 5)

    println(s"The flowLine $flowLine")
    // load the csv

    // use the csv

    // graph the csv