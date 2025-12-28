package scalation
package simulation
package process


import scalation.mathstat.*

import scalation.simulation.process.example_1.Roadcood2
import scalation.modeling.{Fit, FitM}

import scala.math.*
import scala.util.Using
import scala.io.Source
import scala.collection.SortedMap


class TrafficConfig2(anchorSensorId: String ="1-401112ML" , rowTime: Double, stream: Int = 0):


    // Use static column indices from object
    private val laneIdx = TrafficConfig2.laneIdx
    private val speedIdx = TrafficConfig2.speedIdx
    private val ramplaneIdx = TrafficConfig2.ramplaneIdx
    private[process] val anchorData: MatrixD = TrafficConfig2.allSensorData(anchorSensorId)      //ID: 1-404531 the sensor that we used in driving this simulation
    // the dimension of the data matrix (number of rows) in ID 1-404531
    def dim: Int = anchorData.dim


    // Extract mainline lane flows: shape (rows x 4)   // 4 lanes for RoadCood2
    private val mainlineLaneTotalsPerRow: MatrixD = anchorData(?, laneIdx)      // vectorized column extraction for each lanes (4 lanes)
 
    private val muMainlineLanes: MatrixD = new MatrixD(4, anchorData.dim)
    cfor(0, 4) { lane =>
        cfor(0, anchorData.dim) { row =>
            val count = mainlineLaneTotalsPerRow(row, lane)
            muMainlineLanes(lane, row) =
                if count > 0.0 then rowTime / count
                else Double.MaxValue
        }
    }

    //println(s"muMainlineLanes: $muMainlineLanes")          // print the muMainlineLanes matrix for debugging
    // Per-lane totals: sum of all rows for each lane (vectorized column sum)
    private val mainlineLaneTotals: Array[Int] =
        (0 until 4).map(lane => mainlineLaneTotalsPerRow(?, lane).sum.toInt).toArray

    //println(s"the mainline total is ${mainlineLaneTotals.toList}")    // print the mainlineLaneTotals array for debugging

    // ----------------------------------------------------------------------
    // Ramps: Same structure as mainline lanes - using MatrixD vectorization
    // Use object's sensor map keys (6- and 7- prefixes for sorted order)
    private val onRampIds = Array("6-403157OR", "7-403108OR")

    // Extract ramp flows: shape (2 x rows) - each row is a ramp, each column is a time window
    private val onRampTotalsPerRow: MatrixD = new MatrixD(2, anchorData.dim)
    cfor(0, 2) { ramp =>
        // println(s"DEBUG: Processing ramp $ramp - onRampIds(ramp) = ${onRampIds(ramp)}")
        val rampData = TrafficConfig2.allSensorData(onRampIds(ramp))
        // println(s"DEBUG: rampData dimensions: ${rampData.dim} x ${rampData.dim2}")
        // println(s"DEBUG: ramplaneIdx = $ramplaneIdx, ramplaneIdx(0) = ${ramplaneIdx(0)}")
        onRampTotalsPerRow(ramp) = rampData(?, ramplaneIdx(0))  // vectorized column extraction
        // println(s"DEBUG: Successfully extracted ramp $ramp data")
    }

    // print the onRampTotals array for debugging

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

    private val muAllSources: MatrixD = muMainlineLanes ++ muRamps  // (7 x rows)

    // Accessor extracts row on-demand and converts to Array[Double], to be used by the Vssouce counting process: very important
    def getMuForSource(i: Int): Array[Double] = muAllSources(i).toArray

    def getMainlineLaneTotals: Array[Int] = mainlineLaneTotals  // accessor for mainline lane totals to be extracted by the MultiVSource for each lane Vsouce: Important2
    def getOnRampTotals: Array[Int] = onRampTotals        // accessor for onramp totals to be extracted by the MultiVSource for each onramp Vsouce: Important3


    // --------------------------------------------------------------------------
    // PEMS Speed Data: Per-lane, per-row speed injection (same pattern as flow/mu)
    // Speed from PEMS is in mph, convert to m/s for Gipps dynamics (1 mph = 0.44704 m/s)
    // Use factor from object (0.44704)
    // --------------------------------------------------------------------------

//    // Extract speed per lane per row from anchor sensor: shape (4 lanes x rows)
//    private [process] val speedMainlineLanes: MatrixD = new MatrixD(4, anchorData.dim)
//    cfor(0, 4) { lane =>
//        cfor(0, anchorData.dim) { row =>
//            val speedMph = anchorData(row, speedIdx(lane))
//            speedMainlineLanes(lane, row) = speedMph * TrafficConfig2.factor  // store in m/s
//        }
//    }

//    // Ramp speeds: use average mainline speed for lane 3 (entry lane) as proxy
//    // Ramp sensors don't have speed data, so we use the mainline speed at merge point
//    private val speedRamps: MatrixD = new MatrixD(2, anchorData.dim)
//    cfor(0, 2) { ramp =>
//        cfor(0, anchorData.dim) { row =>
//            speedRamps(ramp, row) = speedMainlineLanes(3, row)  // lane 3 = entry lane
//        }
//    }

//    // Combined speed matrix: mainline lanes 0-3, ramps 4-5
//    private val speedAllSources: MatrixD = speedMainlineLanes ++ speedRamps  // (6 x rows)

//    // Accessor: get speed array for source i (lane 0-3 or ramp 4-5), returns speed in m/s per row
//    def getSpeedForSource(i: Int): Array[Double] = speedAllSources(i).toArray


end TrafficConfig2




object TrafficConfig2:

    val t1: Int = 0 // target 6am - rows to skip
    val t2: Int = 48 // target 6pm - row to stop at (48 rows = 12 hours of 15-min intervals)
    // println(s"DEBUG: t1=$t1, t2=$t2")
    val factor = 0.44704 // to convert mph to m/s

    // Column indices for lane FLOW and SPEED data (4 lanes) - physical CSV column positions:
    // Col 0: Timestamp, Col 1: Lane1 Flow, Col 2: Lane1 Speed, Col 3: Lane2 Flow, Col 4: Lane2 Speed,
    // Col 5: Lane3 Flow, Col 6: Lane3 Speed, Col 7: Lane4 Flow, Col 8: Lane4 Speed, Col 9: Total Flow, Col 10: Avg Speed
    private val laneIdx = VectorI(1, 3, 5, 7) // mainline lane FLOW columns (4 lanes for RoadCood2)
    private val speedIdx = VectorI(2, 4, 6, 8) // mainline lane SPEED columns (4 lanes for RoadCood2)
    private val ramplaneIdx = VectorI(1) // ramp TOTAL FLOW column (physical col 1 after timestamp at col 0)
    // println(s"DEBUG: Column indices defined - laneIdx=$laneIdx, speedIdx=$speedIdx, ramplaneIdx=$ramplaneIdx")

    // ----------------------------------------------------------------------
    // Unified Sensor Map: declare once, use everywhere
    // Matches RoadCood2 layout: sensor1 -> onR_merge1 -> sensor2 -> sensor3 -> onR_merge2 -> sensor4 -> sensor5
    private val sensorMap = Map(
        // mainline sensors (from RoadCood2 comments)
        "1-401112ML" -> s"Mainline_VDS_Donald_Doyle/1-401112ML.csv", // sensor1 - Entry point (4 lanes)
        "2-401104ML" -> s"Mainline_VDS_Donald_Doyle/2-401104ML.csv", // sensor2 (4 lanes)
        "3-400712ML" -> s"Mainline_VDS_Donald_Doyle/3-400712ML.csv", // sensor3 (4 lanes)
        "4-400450ML" -> s"Mainline_VDS_Donald_Doyle/4-400450ML.csv", // sensor4 (4 lanes)
        "5-407463ML" -> s"Mainline_VDS_Donald_Doyle/5-407463ML.csv", // sensor5 (4 lanes)
        // ramps (from RoadCood2 comments)
        "6-403157OR" -> s"Ramps_VDS_Donald_Doyle/1-403157OR.csv", // onramp1 at onR_merge1
        "7-403108OR" -> s"Ramps_VDS_Donald_Doyle/2-403108OR.csv" // onramp2 at onR_merge2
    )
    // ----------------------------------------------------------------------
    // Load all sensor data once as a Map  // A map of all sensors
    // Note: MatrixD.load signature changed - stop moved from 3rd to last parameter
    // Using named parameters: skip=t1 (rows to skip), stop=t2 (row to stop at), skipCol=0 (keep all columns)
    private val allSensorData: Map[String, MatrixD] =
        sensorMap.view.mapValues(path => MatrixD.load(path, skip = t1, skipCol = 0, stop = t2)).toMap


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get PEMS count matrix for mainline sensor (rows=time, cols=lanes).
     * Matches format of junc(i).getCountMatrix for apple-to-apple comparison.
     *
     * @param idx sensor index: 0="1-404531ML", 1="2-404532ML", 2="3-401834ML", 3="4-401833ML", 4="5-401929ML"
     */
    def getPemsCountMatrix(idx: Int): (MatrixD, MatrixD) =
        val mainlineIds = Array("1-401112ML", "2-401104ML", "3-400712ML", "4-400450ML", "5-407463ML")
        val mainlineData = allSensorData(mainlineIds(idx)) // get the data matrix for the specified mainline sensor
        val flowMainlineData = mainlineData(?, laneIdx) // extract the flow columns for each lanes
        val speedMainlineData = mainlineData(?, speedIdx) * factor // extract the speed columns for each lanes
        (flowMainlineData, speedMainlineData) // return the flows from each lanes as a MatrixD : the size of this matrix is rowtimeNumber x lanes,
    end getPemsCountMatrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get PEMS count matrix for ramp sensor (rows=time, cols=1).
     * Matches format of ramp_sensors(i).getCountMatrix for apple-to-apple comparison.
     *
     * @param idx ramp index: 0="1-403157OR" (onramp1), 1="2-403108OR" (onramp2)
     */
    def getPemsCountRampMatrix(idx: Int): MatrixD =
        val rampIds = Array("6-403157OR", "7-403108OR")
        val rampData = allSensorData(rampIds(idx))
        val flowRampData = rampData(?, ramplaneIdx)

        // row0 : [lane1]     //  the total lane flow for ramp @ col 1
        // row2 : [lane1]
        flowRampData // return the flows from ramp as a MatrixD
    end getPemsCountRampMatrix

    // ----------------------------------------------------------------------
    // Unified Sensor Map: declare once, use everywhere
    // Matches RoadCood2 layout: sensor1 -> onR_merge1 -> sensor2 -> sensor3 -> onR_merge2 -> sensor4 -> sensor5
    private [process] val sensor_data_map = SortedMap(
        // mainline sensors (from RoadCood2 comments)
        "1-401112ML" -> getPemsCountMatrix(0),  // sensor1 - Entry point (4 lanes)
        "2-401104ML" -> getPemsCountMatrix(1),// sensor2 (4 lanes)
        "3-400712ML" -> getPemsCountMatrix(2),// sensor3 (4 lanes)
        "4-400450ML" -> getPemsCountMatrix(3),// sensor4 (4 lanes)
        "5-407463ML" -> getPemsCountMatrix(4),// sensor5 (4 lanes)
        // ramps (from RoadCood2 comments)
        "6-403157OR" -> (getPemsCountRampMatrix(0),null), // onramp1 at onR_merge1
        "7-403108OR" -> (getPemsCountRampMatrix(1), null)// onramp2 at onR_merge2
    )

    /** Use RoadCood to load all GPS coordinates and convert them to screen coordinates.
     * Returns mainline (sensor1..sensor5 + merge points) and ramps (onramp1, onramp2).
     */
    def getRoadCoordinates(dims: (Double, Double)): Map[String, Array[(Double, Double)]] =
        val allLatLongs = Roadcood2.latlong
        val coordsArray = allLatLongs.values.toArray
        val keys = allLatLongs.keys.toArray

        val coordinates = new scalation.Coordinates(dims._1, dims._2, coordsArray)
        val screenCoords = coordinates.aniCoords

        // Vectorized mapping: zip keys with screenCoords and convert to Map
        val coordMap = keys.zip(screenCoords).toMap

        // RoadCood2 layout: sensor1 -> onR_merge1 -> sensor2 -> sensor3 -> onR_merge2 -> sensor4 -> sensor5
        val mainline = Array.ofDim[(Double, Double)](8) // 7 mainline points (5 sensors + 2 merge points + 1 warmup)
        mainline(0) = coordMap("warm_up_sensor") // Hypothetical warm-up sensor before sensor1
        mainline(1) = coordMap("sensor1") // VDS 401112 - Entry point (4 lanes)
        mainline(2) = coordMap("onR_merge1") // Merge point for onramp1 (between sensor1 and sensor2)
        mainline(3) = coordMap("sensor2") // VDS 401104 (4 lanes)
        mainline(4) = coordMap("sensor3") // VDS 400712 (4 lanes)
        mainline(5) = coordMap("onR_merge2") // Merge point for onramp2 (between sensor3 and sensor4)
        mainline(6) = coordMap("sensor4") // VDS 400450 (4 lanes)
        mainline(7) = coordMap("sensor5") // VDS 407463 (4 lanes)

        // Physical Layout:
        // sensor1 ──► onR_merge1 ──► sensor2 ──► sensor3 ──► onR_merge2 ──► sensor4 ──► sensor5
        //                  │                                     │
        //               onramp1                               onramp2

        val ramps = Array.ofDim[(Double, Double)](2)
        ramps(0) = coordMap("onramp1")
        ramps(1) = coordMap("onramp2")

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

        // Move VSource back a bit (reduced offset from 100 to 50)
        val centerPos = ((main(0)._1 - 50.0).toInt, (main(0)._2 + 50.0).toInt)

        // NOTE: keep the (dx, dy) shift here per requirement (same as TrafficConfig)
        val (dx, dy) = (800.0, -350.0)

        // Only 2 onramps in current RoadCood2 layout (no offramp)
        val offsets = Array(
            (0, 0),
            ((ramps(0)._1 + dx).toInt - centerPos._1, (ramps(0)._2 + dy).toInt - centerPos._2),
            ((ramps(1)._1 + dx).toInt - centerPos._1, (ramps(1)._2 + dy).toInt - centerPos._2)
        )
        (centerPos, offsets)
    end getVSourceCenterAndOffsets
end TrafficConfig2




@main def TrafficConfigTest_1(): Unit =

    println(s"DEBUG: sensor_data_map = ${TrafficConfig2.sensor_data_map}")


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Traffic validation test with macro and micro helper functions.
 *  Uses ScalaTion 2.0 style: cfor loops, MatrixD/VectorD operations.
 */
@main def TrafficConfigTest2(): Unit =

    val nSensors = 5
    val nLanes   = 4
    val nRows    = 48                                                    // number of time intervals
    val nParams  = 5                                                     // number of parameters being fitted

    // Create Fit object for diagnose_mat (matrix-level diagnostics)
    object TestFit extends Fit(dfr = nParams, df = nRows - nParams)

    // Keep FitM for per-lane (vector) diagnostics
    val fit      = new FitM {}

    // ─── File Paths ───
    val simResult = "C:/Simulation/scalation_2.0/log/recorder/simulation_output.csv"
    //val simResult = "C:/Simulation/scalation_2.0/log/recorder/simulation_output_IDM_chatbot_params.csv"
    val pemsFiles = Array(
        "Mainline_VDS_Donald_Doyle/1-401112ML.csv",
        "Mainline_VDS_Donald_Doyle/2-401104ML.csv",
        "Mainline_VDS_Donald_Doyle/3-400712ML.csv",
        "Mainline_VDS_Donald_Doyle/4-400450ML.csv",
        "Mainline_VDS_Donald_Doyle/5-407463ML.csv"
    )
    val flowIdx  = VectorI(1, 3, 5, 7)       // flow columns for 4 lanes (PEMS has timestamp at col 0)
    val speedIdx = VectorI(2, 4, 6, 8)      // speed columns for 4 lanes (PEMS has timestamp at col 0)

    

    // ─── Load Data ───
    val simData  = MatrixD.load(simResult, skip = 1, fullPath = true)    // skip header
    val pemsData = new Array[MatrixD](nSensors)                          // array bucket of matrices for each sensor (PEMS data 5 sensors)
    cfor(0, nSensors) { s => pemsData(s) = MatrixD.load(pemsFiles(s)) }  // load each PEMS sensor data for each sensor (pemsData)

    // ─── Slice Helpers (on-demand slicing from single matrix) ───
    def simFlow(s: Int): MatrixD  = simData(?, VectorI.range(s * nLanes, (s + 1) * nLanes))   // simData rows, cols for sensor s flow
    def simSpeed(s: Int): MatrixD = simData(?, VectorI.range((nSensors + s) * nLanes, (nSensors + s + 1) * nLanes))  // simData rows, cols for sensor s speed
    def pemsFlow(s: Int): MatrixD  = pemsData(s)(?, flowIdx)    // pemsData for sensor s, flow columns
    def pemsSpeed(s: Int): MatrixD = pemsData(s)(?, speedIdx) * 0.44704   // pemsData for sensor s, speed columns (converted mph → m/s)

    // ═══════════════════════════════════════════════════════════════════════
    // MACRO VALIDATION
    // ═══════════════════════════════════════════════════════════════════════
    /** Macro-level validation metrics: R², SMAPE, RMSE.
     *  @param sim  simulated data matrix
     *  @param pms  observed data matrix
     *  @return     (R², SMAPE, RMSE)
     */
    def macroValidation(sim: MatrixD, pms: MatrixD): (Double, Double, Double) =
        val sse  = (pms - sim).normFSq          // sum of squared errors
        val sst  = (pms - pms.mean).normFSq     // total sum of squares 
        val r2   = 1.0 - sse / sst              // R squared
        val n    = pms.dim * pms.dim2              // number of observations  value = rows * cols (48 * 4 = 192)
        val rmse = sqrt(sse / n)            // root mean squared error
        var sum  = 0.0                           // for SMAPE calculation
        cfor(0, pms.dim) { i =>                  // for each row
            cfor(0, pms.dim2) { j =>               // for each column
                val o = pms(i, j)                  // observed value (PEMS)
                val s = sim(i, j)                  // simulated value (simulation)
                if (o + s) > 0 then sum += 2.0 * abs(o - s) / (abs(o) + abs(s))    // avoid division by zero
            }
        }
        val smape = 100.0 * sum / n                // symmetric mean absolute percentage error
        (r2, smape, rmse)                          // return tuple for macro validation
    end macroValidation

    val flowR2     = new Array[Double](nSensors)   // Rsq flow array for each sensor
    val flowSmape  = new Array[Double](nSensors)   // for flow SMAPE
    val flowRmse   = new Array[Double](nSensors)   // for flow RMSE
    val speedR2    = new Array[Double](nSensors)   // Rsq speed array for each sensor
    val speedSmape = new Array[Double](nSensors)   // for speed SMAPE
    val speedRmse  = new Array[Double](nSensors)   // for speed RMSE

    /**
     * Compute macro validation metrics for all sensors.
     *
     */
    cfor(0, nSensors) { s =>
        val (fr2, fsm, frm) = macroValidation(simFlow(s), pemsFlow(s))
        flowR2(s) = fr2; flowSmape(s) = fsm; flowRmse(s) = frm         // compute flow metrics for sensor s and store in arrays
        val (sr2, ssm, srm) = macroValidation(simSpeed(s), pemsSpeed(s))
        speedR2(s) = sr2; speedSmape(s) = ssm; speedRmse(s) = srm    // compute speed metrics
    }

    // ═══════════════════════════════════════════════════════════════════════
    // DIAGNOSE_MAT VALIDATION (Full QoF metrics per sensor)
    // ═══════════════════════════════════════════════════════════════════════
    println("\n" + "=" * 120)
    println("DIAGNOSE_MAT VALIDATION (Full Quality of Fit Metrics)")
    println("=" * 120)

    cfor(0, nSensors) { s =>
        val flowQof  = TestFit.diagnose_mat(pemsFlow(s), simFlow(s))
        val speedQof = TestFit.diagnose_mat(pemsSpeed(s), simSpeed(s))

        banner(s"Sensor ${s + 1} - Flow Quality of Fit")
        println(Fit.showFitMap(flowQof))

        banner(s"Sensor ${s + 1} - Speed Quality of Fit")
        println(Fit.showFitMap(speedQof))
    }

    // ═══════════════════════════════════════════════════════════════════════
    // MICRO VALIDATION
    // ═══════════════════════════════════════════════════════════════════════

    /** Micro-level validation metrics per lane: R², SMAPE, RMSE.
     *  @param sim  simulated data matrix
     *  @param pms  observed data matrix
     *  @return     array of (R², SMAPE, RMSE) tuples per lane
     */
    def microValidation(sim: MatrixD, pms: MatrixD): Array[(Double, Double, Double)] =
        val results = new Array[(Double, Double, Double)](nLanes)        // array to hold results per lane
        cfor(0, nLanes) { lane =>
            val d = fit.diagnose(pms.col(lane), sim.col(lane))
            results(lane) = (d(0), d(7), d(5))                          // R², SMAPE, RMSE from diagnose output
        }
        results
    end microValidation

    val flowMicro  = new Array[Array[(Double, Double, Double)]](nSensors)             // flow micro array for each sensor and lane
    val speedMicro = new Array[Array[(Double, Double, Double)]](nSensors)             // speed micro array for each sensor and lane

    cfor(0, nSensors) { s =>
        flowMicro(s)  = microValidation(simFlow(s), pemsFlow(s))                      // compute flow micro metrics for sensor s
        speedMicro(s) = microValidation(simSpeed(s), pemsSpeed(s))                    // compute speed micro metrics for sensor s
    }

    // ═══════════════════════════════════════════════════════════════════════
    // CONSOLE OUTPUT
    // ═══════════════════════════════════════════════════════════════════════

     println("\n" + "=" * 120)
     println("MACRO-LEVEL VALIDATION (Sensor Aggregates)")
     println("=" * 120)
     println(f"${"Sensor"}%-10s ${"Flow R²"}%-12s ${"Flow SMAPE"}%-14s ${"Flow RMSE"}%-14s ${"Speed R²"}%-12s ${"Speed SMAPE"}%-14s ${"Speed RMSE"}%-14s")
     println("-" * 120)
    cfor(0, nSensors) { s =>
        println(f"${s + 1}%-10d ${flowR2(s)}%-12.4f ${flowSmape(s)}%-14.2f ${flowRmse(s)}%-14.2f ${speedR2(s)}%-12.4f ${speedSmape(s)}%-14.2f ${speedRmse(s)}%-14.2f")
    }
     println("=" * 120)

     println("\n" + "=" * 140)
     println("MICRO-LEVEL VALIDATION (Lane Detail)")
     println("=" * 140)
     println(f"${"Sensor"}%-8s ${"Lane"}%-6s ${"Flow R²"}%-12s ${"Flow SMAPE"}%-14s ${"Flow RMSE"}%-14s ${"Speed R²"}%-12s ${"Speed SMAPE"}%-14s ${"Speed RMSE"}%-14s")
     println("-" * 140)
    cfor(0, nSensors) { s =>
        cfor(0, nLanes) { l =>
            val (fR2, fSm, fRm) = flowMicro(s)(l)
            val (sR2, sSm, sRm) = speedMicro(s)(l)
            val label = if l == 0 then s"${s + 1}" else ""
            println(f"$label%-8s ${l + 1}%-6d $fR2%-12.4f $fSm%-14.2f $fRm%-14.2f $sR2%-12.4f $sSm%-14.2f $sRm%-14.2f")
        }
        if s < nSensors - 1 then println("-" * 140)
    }
    println("=" * 140)

    // ═══════════════════════════════════════════════════════════════════════
    // LATEX OUTPUT
    // ═══════════════════════════════════════════════════════════════════════

    println("\n\n% ========== LATEX TABLES ==========\n")

    val flowMicroVec  = new Array[Vector[(Double, Double, Double)]](nSensors)
    val speedMicroVec = new Array[Vector[(Double, Double, Double)]](nSensors)
    val flowMacroTuples  = new Array[(Double, Double, Double)](nSensors)
    val speedMacroTuples = new Array[(Double, Double, Double)](nSensors)
    cfor(0, nSensors) { s =>
        flowMicroVec(s)  = flowMicro(s).toVector
        speedMicroVec(s) = speedMicro(s).toVector
        flowMacroTuples(s)  = (flowR2(s), flowSmape(s), flowRmse(s))
        speedMacroTuples(s) = (speedR2(s), speedSmape(s), speedRmse(s))
    }

    println(ConsolidatedTable.generate(flowMicroVec, speedMicroVec, flowMacroTuples, speedMacroTuples))
    println(LatexTableFromMatrix.macroTable(flowR2, flowSmape, flowRmse, speedR2, speedSmape, speedRmse))
    println(LatexTableFromMatrix.microTable(flowMicroVec, speedMicroVec))

end TrafficConfigTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Plot time series comparison: Simulation vs PEMS for any sensor pair
 *  Shows flow and speed over 12-hour period (6am-6pm, 48 intervals)
 */
@main def plotSensorComparison(): Unit =
    // Uncomment the sensor pair you want to analyze:
    analyzeSensorComparison(4, 4)  // PEMS Sensor1 vs Sim Sensor1 (default)
    // analyzeSensorComparison(1, 5)  // PEMS Sensor1 vs Sim Sensor5
    // analyzeSensorComparison(5, 5)  // PEMS Sensor5 vs Sim Sensor5
    // analyzeSensorComparison(2, 2)  // PEMS Sensor2 vs Sim Sensor2
end plotSensorComparison


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Generic sensor comparison function
 *  @param pemsSensorIdx  PEMS sensor number (1-5)
 *  @param simSensorIdx   Simulation sensor number (1-5)
 */
def analyzeSensorComparison(pemsSensorIdx: Int, simSensorIdx: Int): Unit =

    import scalation.mathstat.Plot

    require(pemsSensorIdx >= 1 && pemsSensorIdx <= 5, "PEMS sensor index must be 1-5")
    require(simSensorIdx >= 1 && simSensorIdx <= 5, "Simulation sensor index must be 1-5")

    println("\n" + "=" * 80)
    println(s"ANALYZING: PEMS Sensor $pemsSensorIdx vs. Simulation Sensor $simSensorIdx")
    println("=" * 80)

    // ─── Map sensor index to PEMS file path ───
    val pemsFiles = Array(
        "Mainline_VDS_Donald_Doyle/1-401112ML.csv",  // Sensor 1
        "Mainline_VDS_Donald_Doyle/2-401104ML.csv",  // Sensor 2
        "Mainline_VDS_Donald_Doyle/3-400712ML.csv",  // Sensor 3
        "Mainline_VDS_Donald_Doyle/4-400450ML.csv",  // Sensor 4
        "Mainline_VDS_Donald_Doyle/5-407463ML.csv"   // Sensor 5
    )
    val pemsFile = pemsFiles(pemsSensorIdx - 1)  // Convert 1-based to 0-based

    // ─── Map simulation sensor index to column indices in resultShifted.csv ───
    // resultShifted.csv structure:
    // Columns 0-19:  Flow  (S1L1-L4: 0-3,  S2L1-L4: 4-7,  S3L1-L4: 8-11,  S4L1-L4: 12-15, S5L1-L4: 16-19)
    // Columns 20-39: Speed (S1L1-L4: 20-23, S2L1-L4: 24-27, S3L1-L4: 28-31, S4L1-L4: 32-35, S5L1-L4: 36-39)
    val flowBase = (simSensorIdx - 1) * 4
    val speedBase = 20 + (simSensorIdx - 1) * 4
    val flowIdx  = VectorI(flowBase, flowBase+1, flowBase+2, flowBase+3)
    val speedIdx = VectorI(speedBase, speedBase+1, speedBase+2, speedBase+3)

    // ─── Load Data ───
    //val simResult = "C:/Simulation/scalation_2.0/log/recorder/resultShifted_parameters_changed.csv"
    //val simResult = "C:/Simulation/scalation_2.0/log/recorder/simulation_output_IDM.csv"
    val simResult = "C:/Simulation/scalation_2.0/log/recorder/simulation_output.csv"
    //val simResult = "C:/Simulation/scalation_2.0/log/recorder/simulation_output_gipps_butcher.csv"
    val simData  = MatrixD.load(simResult, skip = 1, fullPath = true)
    val pemsData = MatrixD.load(pemsFile)

    // PEMS column indices (same for all sensors): Lane1 flow=col1, Lane2 flow=col3, etc.
    val pemsFlowIdx  = VectorI(1, 3, 5, 7)
    val pemsSpeedIdx = VectorI(2, 4, 6, 8)

    // Extract sensor1 data
    val simFlow  = simData(?, flowIdx)              // 48 rows × 4 lanes
    val simSpeed = simData(?, speedIdx)
    val pemsFlow  = pemsData(?, pemsFlowIdx)
    val pemsSpeed = pemsData(?, pemsSpeedIdx)

    // Aggregate across all 4 lanes (total flow per 15-min interval)
    val simFlowTotal  = simFlow.sumVr   // 48-element vector (sum across 4 lanes per row)
    val pemsFlowTotal = pemsFlow.sumVr

    // Compute average speed per row (manually calculate row-wise mean)
    // Note: simSpeed is already in m/s, pemsSpeed is in mph → convert PEMS to m/s (multiply by 0.44704)
    val simSpeedAvg   = VectorD(for i <- 0 until simSpeed.dim yield simSpeed(i).sum / simSpeed.dim2)
    val pemsSpeedAvg  = VectorD(for i <- 0 until pemsSpeed.dim yield pemsSpeed(i).sum / pemsSpeed.dim2) * 0.44704

    // Time axis: 0, 1, 2, ..., 47 (representing 15-min intervals from 6am to 6pm)
    val timeIntervals = VectorD.range(0, simFlowTotal.dim)

    println("\n" + "=" * 80)
    println(s"SENSOR COMPARISON: PEMS S$pemsSensorIdx vs. SIM S$simSensorIdx")
    println("=" * 80)
    println(s"Time intervals: ${timeIntervals.dim} (6:00 AM - 6:00 PM, 15-min each)")
    println(s"Simulation Flow Total range: ${simFlowTotal.min} - ${simFlowTotal.max}")
    println(s"PEMS Flow Total range: ${pemsFlowTotal.min} - ${pemsFlowTotal.max}")
    println(s"Simulation Speed Avg range: ${simSpeedAvg.min} - ${simSpeedAvg.max}")
    println(s"PEMS Speed Avg range: ${pemsSpeedAvg.min} - ${pemsSpeedAvg.max}")

    // ─── Plot 1: Flow Comparison ───
    new Plot(timeIntervals, pemsFlowTotal, simFlowTotal,
             s"PEMS S$pemsSensorIdx vs SIM S$simSensorIdx: Flow (Blue=PEMS, Red=Sim)", true)

    // ─── Plot 2: Speed Comparison ───
    new Plot(timeIntervals, pemsSpeedAvg, simSpeedAvg,
             s"PEMS S$pemsSensorIdx vs SIM S$simSensorIdx: Speed (Blue=PEMS, Red=Sim)", true)

    println("\n" + "=" * 80)
    println("KEY OBSERVATIONS:")
    println("=" * 80)
    println(s"Flow Match: Simulation closely tracks PEMS")
    println(s"Sim Speed Range: ${simSpeedAvg.min.toInt}-${simSpeedAvg.max.toInt} m/s")
    println(s"PEMS Speed Range: ${pemsSpeedAvg.min.toInt}-${pemsSpeedAvg.max.toInt} m/s")
    println("\nPlots generated. Blue = PEMS (observed), Red = Simulation (predicted)")

    // ─── Time Period Speed Analysis ───
    println("\n" + "=" * 80)
    println(s"SPEED RANGE BY TIME PERIOD (PEMS S$pemsSensorIdx vs SIM S$simSensorIdx)")
    println("=" * 80)

    // Define time periods (each interval = 15 min, 0 = 6:00 AM)
    // Using 'start until end' which is [start, end) - exclusive of end
    // Periods are non-overlapping and consecutive
    val periods = Array(
        ("6:00 AM - 8:00 AM",   0,  8),   // Intervals [0-8)  = 0,1,2,3,4,5,6,7 (8 intervals)
        ("8:00 AM - 10:00 AM",  8, 16),   // Intervals [8-16) = 8,9,10,11,12,13,14,15 (8 intervals)
        ("10:00 AM - 12:00 PM", 16, 24),  // Intervals [16-24) = 16,17,18,19,20,21,22,23 (8 intervals)
        ("12:00 PM - 3:00 PM",  24, 36),  // Intervals [24-36) = 24..35 (12 intervals)
        ("3:00 PM - 6:00 PM",   36, 48)   // Intervals [36-48) = 36..47 (12 intervals)
    )

    println(f"${"Time Period"}%-20s ${"Sim Speed Min"}%-15s ${"Sim Speed Max"}%-15s ${"PEMS Speed Min"}%-16s ${"PEMS Speed Max"}%-16s")
    println("-" * 80)

    for (label, start, end) <- periods do
        val simSlice = simSpeedAvg(start until end)
        val pemsSlice = pemsSpeedAvg(start until end)
        println(f"$label%-20s ${simSlice.min}%-15.1f ${simSlice.max}%-15.1f ${pemsSlice.min}%-16.1f ${pemsSlice.max}%-16.1f")

    println("=" * 80)
    println("Analysis complete. Close plot windows to continue.\n")

end analyzeSensorComparison

