package scalation
package simulation
package process
package model
package calroute101

import scalation.mathstat.*


@main def flowMesoVsMicro (): Unit =


    // Paths are relative to DATA_DIR (data/).  Sensor 1 of CalRoute101_3 is VDS 401112 (Donald Doyle).
    val pems_sensor1 = "Mainline_VDS_Donald_Doyle/1-401112ML.csv"
    val meso_flow  = "../log/simulation/meso/run_1790701830742_5499a33a-2284-456d-add3-3c8f55f4c541_validation_data.csv"    // CalRoute101_3_fullMeso
    val micro_flow = "../log/simulation/micro/run_1790701754732_098913a8-1cc3-4b18-b8e1-0044fa6f0969_validation_data.csv"   // CalRoute101_3
    val mixed_flow = "../log/simulation/mixed/run_1790701810202_06a61b3a-4733-44f1-8373-37f64ad93a6b_validation_data.csv"   // CalRoute101_3_withMeso

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
//                   Array ("PeMS", "Mixed"), s"Sensor 1 Lane ${lane + 1}: PeMS vs Mixed Flow", lines = true)



    // all four lanes per source
//    new PlotM (time, pemsFlow.transpose,  Array ("L1", "L2", "L3", "L4"), "Sensor 1 PeMS flow by lane", lines = true)
//    new PlotM (time, microFlow.transpose, Array ("L1", "L2", "L3", "L4"), "Sensor 1 Micro flow by lane", lines = true)
//    new PlotM (time, mesoFlow.transpose,  Array ("L1", "L2", "L3", "L4"), "Sensor 1 Meso flow by lane", lines = true)


end flowMesoVsMicro