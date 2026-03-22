package scalation
package simulation
package process


import scalation.mathstat.*
import scalation.simulation.process.example_1.Roadcood
import scala.util.Using
import scala.io.Source


class TrafficConfig(anchorSensorId: String ="1-404531ML" , rowTime: Double, stream: Int = 0):

    private[process] val ew = new EasyWriter("recorder", "TrafficConfigText.txt")

    private val loadStart: Long = System.nanoTime()


    val t1: Int = 0 // target 6am
    val t2: Int = 48// target 6pm


    private val rowOffset = t1
    private val laneIdx = VectorI(3, 5, 7, 9, 11) // mainline lane FLOW columns
    private val ramplaneIdx = VectorI(1) // ramp/offramp TOTAL FLOW column


    // ----------------------------------------------------------------------
    // Unified Sensor Map: declare once, use everywhere
    private val sensorMap: Map[String, String] = Map(
        // mainline sensors
        "1-404531ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/1-404531ML.csv", // driver source   // needs to be updated to 404532
        "2-404532ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/2-404532ML.csv", //
        "3-401834ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/3-401834ML.csv", // eval after offramp
        "4-401833ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/4-401833ML.csv", // eval after onramp1
        "5-401929ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/5-401929ML.csv",
        // ramps
        "1-410095OR" -> s"Ramps_VDS_Redwood_Creek_US101-N/1-410095OR.csv", // onramp1
        "2-410093OR" -> s"Ramps_VDS_Redwood_Creek_US101-N/2-410093OR.csv", // onramp2
        "1-410094FR" -> s"Ramps_VDS_Redwood_Creek_US101-N/1-410094FR.csv" // offramp
    )

    // ----------------------------------------------------------------------
    // Load all sensor data once as a Map  // A map of all sensors
    private val allSensorData: Map[String, MatrixD] =
        sensorMap.view.mapValues(path => MatrixD.load(path, t1, t2)).toMap



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get PEMS count matrix for mainline sensor (rows=time, cols=lanes).
     * Matches format of junc(i).getCountMatrix for apple-to-apple comparison.
     *
     * @param idx sensor index: 0="1-404531ML", 1="2-404532ML", 2="3-401834ML", 3="4-401833ML", 4="5-401929ML"
     */
    def getPemsCountMatrix(idx: Int): MatrixD =
        val mainlineIds = Array("1-404531ML", "2-404532ML", "3-401834ML", "4-401833ML", "5-401929ML")
        val mainlineData = allSensorData(mainlineIds(idx))
        val flowMainlineData = mainlineData(?, laneIdx)
        // row0 : [lane1, lane2, lane3, lane4,lane5]
        // row1 : [lane1, lane2, lane3,lane4,lane5]
        // for our full data Matrix Dimention is 48 rows x 5 lanes_flow columns.
        flowMainlineData   // return the flows from each lanes as a MatrixD : the size of this matrix is rowtimeNumber x lanes,
    end getPemsCountMatrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get PEMS count matrix for ramp sensor (rows=time, cols=1).
     * Matches format of ramp_sensors(i).getCountMatrix for apple-to-apple comparison.
     * @param idx ramp index: 0="1-410095OR" (onramp1), 1="2-410093OR" (onramp2), 2="1-410094FR" (offramp)
     */
    def getPemsCountRampMatrix(idx: Int): MatrixD =
        val rampIds = Array("1-410095OR", "2-410093OR", "1-410094FR")
        val rampData = allSensorData(rampIds(idx))
        val flowRampData = rampData(?, ramplaneIdx)
        // row0 : [lane1]     //  the total lane flow for ramp @ col 1
        // row2 : [lane1]
        flowRampData   // return the flows from ramp as a MatrixD
    end getPemsCountRampMatrix


    // Direct sensor ID usage (no path parsing)
    private[process] val anchorData: MatrixD = allSensorData(anchorSensorId)      //ID: 1-404531 the sensor that we used in driving this simulation

    // the dimension of the data matrix (number of rows) in ID 1-404531
    def dim: Int = anchorData.dim


    // Extract mainline lane flows: shape (rows x 5)   // is this per row or column?????
    private val mainlineLaneTotalsPerRow: MatrixD = anchorData(?, laneIdx)      // vectorized column extraction for each lanes (5 lanes)

    private val muMainlineLanes: MatrixD = new MatrixD(5, anchorData.dim)
    cfor(0, 5) { lane =>
        cfor(0, anchorData.dim) { row =>
            val count = mainlineLaneTotalsPerRow(row, lane)
            muMainlineLanes(lane, row) = 
                if count > 0.0 then rowTime / count
                else Double.MaxValue  // ← No vehicles this interval (infinite wait)
        }
    }


    println(s"muMainlineLanes: $muMainlineLanes")          // print the muMainlineLanes matrix for debugging
    // Per-lane totals: sum of all rows for each lane (vectorized column sum)
    private val mainlineLaneTotals: Array[Int] =
        (0 until 5).map(lane => mainlineLaneTotalsPerRow(?, lane).sum.toInt).toArray

    println(s"the mainline total is ${mainlineLaneTotals.toList}")    // print the mainlineLaneTotals array for debugging

    // ----------------------------------------------------------------------
    // Ramps: Same structure as mainline lanes - using MatrixD vectorization

    private val onRampIds = Array("1-410095OR", "2-410093OR")
    private val offrampId = "1-410094FR"
    private val offrampData = allSensorData(offrampId)

    // Extract ramp flows: shape (2 x rows) - each row is a ramp, each column is a time window
    private val onRampTotalsPerRow: MatrixD = new MatrixD(2, anchorData.dim)
    cfor(0, 2) { ramp =>
        val rampData = allSensorData(onRampIds(ramp))
        onRampTotalsPerRow(ramp) = rampData(?, ramplaneIdx(0))  // vectorized column extraction
    }

    // Per-ramp mu series (carry-forward for zero counts): shape (2 x rows)
    // private val muRamps: MatrixD = new MatrixD(2, anchorData.dim)
    // cfor(0, 2) { ramp =>
    //     var lastValidMu = rowTime / 1.0
    //     cfor(0, anchorData.dim) { row =>
    //         val count = onRampTotalsPerRow(ramp, row)
    //         if count > 0.0 then
    //             lastValidMu = rowTime / count
    //         muRamps(ramp, row) = lastValidMu
    //     }
    // }
    private val muRamps: MatrixD = new MatrixD(2, anchorData.dim)
    cfor(0, 2) { ramp =>
        cfor(0, anchorData.dim) { row =>
            val count = onRampTotalsPerRow(ramp, row)
            muRamps(ramp, row) = 
                if count > 0.0 then rowTime / count
                else Double.MaxValue
        }
    }


// Per-ramp totals: sum each ramp row (vectorized)
    private val onRampTotals: Array[Int] =
        (0 until 2).map(ramp => onRampTotalsPerRow(ramp).sum.toInt).toArray

    // ----------------------------------------------------------------------
    // Totals (for backward compatibility)

    //lazy val sensor1Total: Int = mainlineLaneTotals.sum    // where did we use this?
    //lazy val onramp1Total: Int = onRampTotals(0)
    //lazy val onramp2Total: Int = onRampTotals(1)
    //lazy val offrampTotal: Int = offrampData(?, ramplaneIdx(0)).sum.toInt

    // ----------------------------------------------------------------------
    // Sources: lanes 0-4, then ramps 5-6
    //No more nStopArray
    //val nStopArray: Array[Int] = mainlineLaneTotals ++ onRampTotals    //??? each Vsouce should have it's own nStop value so why is mainlneaneTotals used here?

    // Combine all mu sources into single MatrixD (7 x rows): 5 mainline lanes + 2 ramps
    // Vectorized vertical concatenation eliminates loops and duplicate storage
    private val muAllSources: MatrixD = muMainlineLanes ++ muRamps  // (7 x rows)

    // Accessor extracts row on-demand and converts to Array[Double], to be used by the Vssouce counting process: very important
    def getMuForSource(i: Int): Array[Double] = muAllSources(i).toArray

    def getMainlineLaneTotals: Array[Int] = mainlineLaneTotals  // accessor for mainline lane totals to be extracted by the MultiVSource for each lane Vsouce: Important2
    def getOnRampTotals: Array[Int] = onRampTotals        // accessor for onramp totals to be extracted by the MultiVSource for each onramp Vsouce: Important3





    // ----------------------------------------------------------------------
    // Exit fraction (offramp / total mainline per row)

    /** Raw exit fraction per row (no smoothing) - more responsive to time-varying patterns
     * using raw exit fraction since it is more responsive to time-varying patterns
     * */
    val exitFractionRaw = Array.ofDim[Double](anchorData.dim)
    cfor (0, anchorData.dim) { row =>
        val mainlineTotal = mainlineLaneTotalsPerRow(row).sum  // vectorized row sum
        val offrampFlow = offrampData(row, ramplaneIdx(0))
        exitFractionRaw(row) = if mainlineTotal == 0.0 then 0.0 else offrampFlow / mainlineTotal
    }
    /**
     * Use RoadCood to load all GPS coordinates and convert them to screen coordinates
     * Returns:
     *  - mainline: sensor1..sensor6
     *  - ramps: onramp1,onramp2,offramp
     */
    def getRoadCoordinates(dims: (Double, Double)): Map[String, Array[(Double, Double)]] =
        val allLatLongs = Roadcood.latlong
        val coordsArray = allLatLongs.values.toArray
        val keys = allLatLongs.keys.toArray

        val coordinates = new scalation.Coordinates(dims._1, dims._2, coordsArray)
        val screenCoords = coordinates.aniCoords

        // Vectorized mapping: zip keys with screenCoords and convert to Map
        val coordMap = keys.zip(screenCoords).toMap

        val mainline = Array.ofDim[(Double, Double)](coordMap.size - 3) // 8 mainline points
        mainline(0) = coordMap("sensor1")     // take count from sensor1 point to compare with PEMS sensor1
        mainline(1) = coordMap("sensor2")     // take count from sensor2 point to compare with PEMS sensor2
        mainline(2) = coordMap("offR_merge")  // Vehicles that remains on the mainline after offramp  // compare with pems sensor3
        mainline(3) = coordMap("sensor3")  // Entry point of onramp1 vehicles. No comparison
        mainline(4) = coordMap("onR_merge1")     // take count from sensor3 point to compare with PEMS sensor4
        mainline(5) = coordMap("sensor4")  // Entry point of onramp2 vehicles. No comparison
        mainline(6) = coordMap("onR_merge2")     // take count from sensor4 point to compare with PEMS sensor5
        mainline(7) = coordMap("sensor5")     // take count from sensor5 point to compare with PEMS sensor5


        //sensor1---------sensor2-------sensor3-------sensor4-------sensor5      // Real pems road location
        //                          |         |         |
        //                    offR_merge  onR_merge1   onR_merge2
        //Our approach:
        //senso1-------sensor2---offR_merge---onR_merge1---sensor3---onR_merge2---sensor4---sensor5
        // sensor1 point: @ 0
        // sensor2 point: @ 1
        // offR_marge point: same @ 2
        // onR_merge1 point: now @ 3
        // sensor3 point: now @4
        // onR_merge2 point: now @ 5
        // sensor4 point: now @6
        // sensor5 point: now @7

        val ramps = Array.ofDim[(Double, Double)](3)
        ramps(0) = coordMap("onramp1")
        ramps(1) = coordMap("onramp2")
        ramps(2) = coordMap("offramp")

        Map("mainline" -> mainline, "ramps" -> ramps)
    end getRoadCoordinates

    // Legacy CSV-based junctions method (kept for compatibility)
    def getJunctions(path: String, w_h: (Double, Double)): Array[(Double, Double)] =
        Using.resource(Source.fromFile(path)) { src =>
            val data = src.getLines().toArray
            val gps = data.map { line =>
                val Array(lat, long) = line.split(",").map(_.toDouble)
                (lat, long)
            }
            val coords = new scalation.Coordinates(w_h._1, w_h._2, gps)
            coords.calcAniCoords()
            coords.aniCoords
        }
    end getJunctions

    // Provide coordinate accessors without relying on undefined cached values
    def getMainlineCoordinates(dims: (Double, Double)): Array[(Double, Double)] =
        getRoadCoordinates(dims)("mainline")

    def getRampCoordinates(dims: (Double, Double)): Array[(Double, Double)] =
        val (sx, sy) = (65.0, -70.0)
        val rawRamps = getRoadCoordinates(dims)("ramps")
        // Vectorized shift: map each coordinate pair
        rawRamps.map((x, y) => (x + sx, y + sy))
    end getRampCoordinates

    def getSensorCoordinates(dims: (Double, Double)): Array[(Double, Double)] =
        getMainlineCoordinates(dims)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def getVSourceCenterAndOffsets(dims: (Double, Double)): ((Int, Int), Array[(Int, Int)]) =
        val main = getMainlineCoordinates(dims)
        val ramps = getRampCoordinates(dims)

        val centerPos = ((main(0)._1 + 100.0).toInt, (main(0)._2 + 100.0).toInt)

        // NOTE: keep the (dx, dy) shift here per requirement
        val (dx, dy) = (800.0, -350.0)

        val offsets = Array(
            (0, 0),
            ((ramps(0)._1 + dx).toInt - centerPos._1, (ramps(0)._2 + dy).toInt - centerPos._2),
            ((ramps(1)._1 + dx).toInt - centerPos._1, (ramps(1)._2 + dy).toInt - centerPos._2),
            ((ramps(2)._1 + dx).toInt - centerPos._1, (ramps(2)._2 + dy).toInt - centerPos._2)
        )
        (centerPos, offsets)
    end getVSourceCenterAndOffsets




    //New code

    /** Get lane distribution as VectorD for a PEMS sensor at a given row.
     *
     * @param pemsIdx the PEMS sensor index (0..4)
     * @param row     the time row
     * @return VectorD of 5 lane fractions summing to ~1.0
     */
    def getLaneDistribution(pemsIdx: Int, row: Int): VectorD =
        val counts = getPemsCountMatrix(pemsIdx)(row)
        val total = counts.sum
        if total > 0 then counts / total else VectorD.fill(5)(0.2)
    end getLaneDistribution

end TrafficConfig



@main def TrafficConfigTest(): Unit =

    val simResult = "data/Mainline_VDS_Redwood_Creek_US101-N/result3.csv"
    val simResult_slice = MatrixD.load(simResult, fullPath = true)


    val result_slice = simResult_slice(1 until simResult_slice.dim)


    val pems1_count = "data/Mainline_VDS_Redwood_Creek_US101-N/1-404531ML.csv"
    val pems2_count = "data/Mainline_VDS_Redwood_Creek_US101-N/2-404532ML.csv"
    val pems3_count = "data/Mainline_VDS_Redwood_Creek_US101-N/3-401834ML.csv"
    val pems4_count = "data/Mainline_VDS_Redwood_Creek_US101-N/4-401833ML.csv"
    val pems5_count = "data/Mainline_VDS_Redwood_Creek_US101-N/5-401929ML.csv"
    val laneIdx = VectorI(3, 5, 7, 9, 11) // mainline lane FLOW columns for pems data

    val pems1 = MatrixD.load(pems1_count, fullPath = true)
    val pems2 = MatrixD.load(pems2_count, fullPath = true)
    val pems3 = MatrixD.load(pems3_count, fullPath = true)
    val pems4 = MatrixD.load(pems4_count, fullPath = true)
    val pems5 = MatrixD.load(pems5_count, fullPath = true)


//:SIM counts slicing
    val countSensor1 = VectorI(0,1,2,3,4) // sensor ID for mainline sensors
    val countSensor2 = VectorI(5,6,7,8,9) // sensor ID for ramp sensors
    val countSensor3 = VectorI(10, 11, 12, 13, 14) // sensor ID for ramp sensors
    val countSensor4 = VectorI(15, 16, 17, 18, 19) // sensor ID for ramp sensors
    val countSensor5 = VectorI(20, 21, 22, 23, 24) // sensor ID for ramp sensors

    val sens1_slice = result_slice(?, countSensor1)
    val sens2_slice = result_slice(?, countSensor2)
    val sens3_slice = result_slice(?, countSensor3)
    val sens4_slice = result_slice(?, countSensor4)
    val sens5_slice = result_slice(?, countSensor5)

//:Pems counts slicing

    val pems_slice1 = pems1(?, laneIdx)
    val pems_slice2 = pems2(?, laneIdx)
    val pems_slice3 = pems3(?, laneIdx)
    val pems_slice4 = pems4(?, laneIdx)
    val pems_slice5 = pems5(?, laneIdx)


    val sse1 = (pems_slice1 - sens1_slice).normFSq
    val sse2 = (pems_slice2 - sens2_slice).normFSq
    val sse3 = (pems_slice3 - sens3_slice).normFSq
    val sse4 = (pems_slice4 - sens4_slice).normFSq
    val sse5 = (pems_slice5 - sens5_slice).normFSq

    val sst1  = (pems_slice1 - pems_slice1.mean).normFSq
    val sst2  = (pems_slice2 - pems_slice2.mean).normFSq
    val sst3  = (pems_slice3 - pems_slice3.mean).normFSq
    val sst4  = (pems_slice4 - pems_slice4.mean).normFSq
    val sst5  = (pems_slice5 - pems_slice5.mean).normFSq


    val r2_1 = 1.0 - sse1 / sst1
    val r2_2 = 1.0 - sse2 / sst2
    val r2_3 = 1.0 - sse3 / sst3
    val r2_4 = 1.0 - sse4 / sst4
    val r2_5 = 1.0 - sse5 / sst5

    println(s"The R2 value for sensor 1 is $r2_1, sse1 is $sse1, sst1 is $sst1")
    println(s"The R2 value for sensor 2 is $r2_2, sse2 is $sse2, sst2 is $sst2")
    println(s"The R2 value for sensor 3 is $r2_3, sse3 is $sse3, sst3 is $sst3")
    println(s"The R2 value for sensor 4 is $r2_4, sse4 is $sse4, sst4 is $sst4")
    println(s"The R2 value for sensor 5 is $r2_5, sse5 is $sse5, sst5 is $sst5")


//
//    println(s"the pems1 sheet1 is $pems_slice1")
//    println(s"-----------------------------------")
//    println(s"the pems2 sheet1 is $pems_slice2")
//    println(s"-----------------------------------")
//    println(s"the pems3 sheet1 is $pems_slice3")
//    println(s"-----------------------------------")
//    println(s"the pems4 sheet1 is $pems_slice4")
//    println(s"-----------------------------------")
//    println(s"the pems5 sheet1 is $pems_slice5")
//    println(s"-----------------------------------")
//
//
//
//    println(s"\n-----------------------------------\n")
//
//
//    println(s"the result sheet1 is $sens1_slice")
//    println(s"-----------------------------------")
//    println(s"the result sheet2 is $sens2_slice")
//    println(s"-----------------------------------")
//    println(s"the result sheet3 is $sens3_slice")
//    println(s"-----------------------------------")
//    println(s"the result sheet4 is $sens4_slice")
//    println(s"-----------------------------------")
//    println(s"the result sheet5 is $sens5_slice")
//    println(s"-----------------------------------")

end TrafficConfigTest


//Sensor1      Sensor2      Offramp_merge        Sensor3      Onramp1_merge        Sensor4     onramp2_merge        Sensor5
// |---------------|---------------|----------------|---------------|----------------|---------------|----------------|