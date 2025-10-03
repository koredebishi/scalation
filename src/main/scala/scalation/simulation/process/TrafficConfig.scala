package scalation
package simulation
package process


import scalation.mathstat.*
import scalation.random.*
import scalation.simulation.process.example_1.Roadcood
import scala.util.Using
import scala.io.Source


class TrafficConfig(fileName: String, rowTime: Double, stream: Int = 0):

    private[process] val ew = new EasyWriter("recorder", "TrafficConfigText.txt")

    private val loadStart: Long = System.nanoTime()

//
//    val t1: Int = 2 * 96 + 16
//    val t2: Int = 3 * 96

    val t1: Int = 16
    val t2: Int = 40


    private val rowOffset = t1

    // ----------------------------------------------------------------------
    // Index definitions (COLUMN POSITIONS in the CSV-derived MatrixD)
    // laneIdx holds the per-lane FLOW columns for the MAINLINE. These columns are spaced
    // by 3 (flow, other-metric, speed) in the raw file. We only take the FLOW positions.
    //   laneIdx(0) = first mainline lane FLOW column (used as denominator in exitFraction)
    // ramplaneIdx holds the (single) ramp/offramp TOTAL FLOW column. Because ramps have
    // only one lane, the lane flow column (e.g., 4) and the total flow column (1) carry
    // the same numeric value; we consciously use column 1 to emphasize it is a total.
    // exitFraction definition implemented below:
    //   offramp total flow (ramplaneIdx(0)) / mainline lane 0 flow (laneIdx(0))
//    private val laneIdx     = VectorI(4, 7, 10, 13, 16) // mainline lane FLOW columns
//    private val ramplaneIdx = VectorI(1)                // ramp/offramp TOTAL FLOW column
//
//    // ----------------------------------------------------------------------
//    // Sensor IDs
//    private val sensorIds = Array(
//        "1-404532ML", "2-401834ML", "3-401833ML", "4-401929ML", "5-401652ML", // mainline
//        "1-410095OR","2-410093OR", "1-410094FR"              // ramps (2 on, 1 off)
//    )
//
//    private val baseDirMain = "Mainline_VDS_Redwood_Creek_US101-N"
//    private val baseDirRamp = "Ramp_VDS_"
//
//    // ----------------------------------------------------------------------
//    // Load all sensor data once
//    private val allSensorData: Map[String, MatrixD] =
//        sensorIds.map { id =>
//            val path = if id.endsWith("ML") then s"$baseDirMain/$id.csv"
//            else s"$baseDirRamp/$id.csv"
//            id -> MatrixD.load(path, t1, t2)
//        }.toMap
//
//    // ----------------------------------------------------------------------
//    // Anchor dataset
//    private val anchorId = fileName.split("/").last.split("\\.")(0)
//    val data: MatrixD = allSensorData(anchorId)
//
//    // ----------------------------------------------------------------------
//    // Mainline: use raw arrays for hot path
//    private val mainlineCols: Array[Array[Double]] =
//        laneIdx.map(c => data(?, c).toArray).toArray   // extract columns as arrays
//
//    val arrivalCount: Array[Array[Double]] = mainlineCols   // name preserved
//    val totalArrivalsPerRow: Array[Double] =
//        Array.tabulate(data.dim)(r => mainlineCols.map(_(r)).sum)
//
//    val muMain: Array[Double] = totalArrivalsPerRow.map(rowTime / _)
//    val laneProbPerRow: Array[Array[Double]] =
//        Array.tabulate(data.dim)(r => {
//            val row = laneIdx.map(c => data(r, c))
//            val sum = row.sum
//            row.map(_ / sum).toArray
//        })
//    val laneRVPerRow: Array[Discrete] =
//        laneProbPerRow.map(p => Discrete(VectorD(p)))
//
//    // ----------------------------------------------------------------------
//    // Ramps
//    private val onRampIds = Array("1-410095OR","2-410093OR" )   // why this again when we already have sensorIds?
//    private val offrampId = "1-410094FR"        // why this too??
//
//    private val offrampData = allSensorData(offrampId) // cached ref
//
//
//    val onRampTotalsPerRow: Array[Array[Double]] =
//        onRampIds.map(id => allSensorData(id)(?, ramplaneIdx(0)).toArray)
//
//    private val muRamps: Array[Array[Double]] =
//        onRampTotalsPerRow.map(_.map(rowTime / _))
//
//    // ----------------------------------------------------------------------
//    // Evaluation sensors (mainline + offramp)
//    private val evalMainIds = Array("2-401834ML", "3-401833ML", "5-401652ML", offrampId)
//    val evalArrivalsPerRow: Array[Array[Double]] =
//        evalMainIds.map(id => allSensorData(id)(?, ramplaneIdx(0)).toArray)
//
//    // ----------------------------------------------------------------------
//    // Totals
//    lazy val sensor1Total: Int = totalArrivalsPerRow.sum.toInt
//    lazy val onramp1Total: Int = onRampTotalsPerRow(0).sum.toInt
//    lazy val onramp2Total: Int = onRampTotalsPerRow(1).sum.toInt
//    lazy val offrampTotal: Int = offrampData(?, ramplaneIdx(0)).sum.toInt
//
//    // ----------------------------------------------------------------------
//    // Sources
//    val nStopArray: Array[Int] = Array(sensor1Total, onramp1Total, onramp2Total)
//
//    val muPerSource: Array[Array[Double]] = Array(muMain) ++ muRamps
//
//    def getMuForSource(i: Int): Array[Double] = muPerSource(i)
//    @inline def getLaneRV(row: Int): Discrete = laneRVPerRow(row)
//
//    // ----------------------------------------------------------------------
//    // Exit fraction (offramp row0 / mainline lane0 row0)
//    // Helper to compute the fraction for any given (cropped) row index, if needed by callers.
//    def computeExitFraction(row: Int): Double =
//        val mainlineLane0 = mainlineCols(0)(row)                 // denominator: laneIdx(0)
//        val rampTotal     = offrampData(row, ramplaneIdx(0))     // numerator: ramplaneIdx(0)
//        if mainlineLane0 == 0.0 then 0.0 else rampTotal / mainlineLane0
//    end computeExitFraction
//
//    /**
//     * Moving Average (simple) of exit fractions ending at a given row index.
//     * Formula (clipped at row 0):MA_t = (1/m) * Σ_{j=0}^{m-1} f_{t-j}, where f_k = computeExitFraction(k)
//     * If t < m-1, the summation lower bound becomes 0 and the divisor is (t+1)
//     * @param row     current (cropped) row index t
//     * @param window  window size m (>= 1)
//     */
//    def computeExitFractionMA(row: Int, window: Int): Double =
//        if window <= 1 then return computeExitFraction(row)
//        val start = math.max(0, row - window + 1)
//        val count = row - start + 1
//        var sum = 0.0
//        var i = start
//        while i <= row do
//            sum += computeExitFraction(i)
//            i += 1
//        end while
//        sum / count
//    end computeExitFractionMA
//
//    def exitFraction: Double =
//        val average = computeExitFraction(0) // row 0 of CROPPED matrix (starts at original t1)
//        ew.println(s"offramp exit fraction = $average")
//        println(s"offramp exit fraction =${mainlineCols(0)(0)}, ${offrampData(0, ramplaneIdx(0))},  $average")
//        average
//    end exitFraction


    private val laneIdx = VectorI(4, 7, 10, 13, 16) // mainline lane FLOW columns
    private val ramplaneIdx = VectorI(1) // ramp/offramp TOTAL FLOW column

    // ----------------------------------------------------------------------
    // Unified Sensor Map: declare once, use everywhere
    private val sensorMap: Map[String, String] = Map(
        // mainline sensors
        "1-404532ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/1-404532ML.csv", // driver source
        "2-401834ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/2-401834ML.csv", // eval after offramp
        "3-401833ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/3-401833ML.csv", // eval after onramp1
        "4-401929ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/4-401929ML.csv",
        "5-401652ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/5-401652ML.csv", // final eval

        // ramps
        "1-410095OR" -> s"Ramp_VDS_/1-410095OR.csv", // onramp1
        "2-410093OR" -> s"Ramp_VDS_/2-410093OR.csv", // onramp2
        "1-410094FR" -> s"Ramp_VDS_/1-410094FR.csv" // offramp
    )

    // ----------------------------------------------------------------------
    // Load all sensor data once
    private val allSensorData: Map[String, MatrixD] =
        sensorMap.view.mapValues(path => MatrixD.load(path, t1, t2)).toMap

    // ----------------------------------------------------------------------
    // Anchor dataset
    private val anchorId = fileName.split("/").last.split("\\.")(0)
    val data: MatrixD = allSensorData(anchorId)

    // ----------------------------------------------------------------------
    // Mainline: use raw arrays for hot path
    private val mainlineCols: Array[Array[Double]] =
        laneIdx.map(c => data(?, c).toArray).toArray // extract columns as arrays

    val arrivalCount: Array[Array[Double]] = mainlineCols // name preserved
    val totalArrivalsPerRow: Array[Double] =
        Array.tabulate(data.dim)(r => mainlineCols.map(_(r)).sum)

    val muMain: Array[Double] = totalArrivalsPerRow.map(rowTime / _)
    val laneProbPerRow: Array[Array[Double]] =
        Array.tabulate(data.dim)(r => {
            val row = laneIdx.map(c => data(r, c))
            val sum = row.sum
            row.map(_ / sum).toArray
        })
    val laneRVPerRow: Array[Discrete] =
        laneProbPerRow.map(p => Discrete(VectorD(p)))

    // ----------------------------------------------------------------------
    // Ramps
    private val onRampIds = Array("1-410095OR", "2-410093OR") // preserved for compatibility
    private val offrampId = "1-410094FR"

    private val offrampData = allSensorData(offrampId) // cached ref

    val onRampTotalsPerRow: Array[Array[Double]] =
        onRampIds.map(id => allSensorData(id)(?, ramplaneIdx(0)).toArray)

    private val muRamps: Array[Array[Double]] =
        onRampTotalsPerRow.map(_.map(rowTime / _))

    // ----------------------------------------------------------------------
    // Evaluation sensors (mainline + offramp)
    private val evalMainIds = Array("2-401834ML", "3-401833ML", "5-401652ML", offrampId)
    val evalArrivalsPerRow: Array[Array[Double]] =
        evalMainIds.map(id => allSensorData(id)(?, ramplaneIdx(0)).toArray)

    // ----------------------------------------------------------------------
    // Totals
    lazy val sensor1Total: Int = totalArrivalsPerRow.sum.toInt
    lazy val onramp1Total: Int = onRampTotalsPerRow(0).sum.toInt
    lazy val onramp2Total: Int = onRampTotalsPerRow(1).sum.toInt
    lazy val offrampTotal: Int = offrampData(?, ramplaneIdx(0)).sum.toInt

    // ----------------------------------------------------------------------
    // Sources
    val nStopArray: Array[Int] = Array(sensor1Total, onramp1Total, onramp2Total)

    val muPerSource: Array[Array[Double]] = Array(muMain) ++ muRamps

    def getMuForSource(i: Int): Array[Double] = muPerSource(i)

    @inline def getLaneRV(row: Int): Discrete = laneRVPerRow(row)

    // ----------------------------------------------------------------------
    // Exit fraction (offramp row0 / mainline lane0 row0)
    def computeExitFraction(row: Int): Double =
        val mainlineLane0 = mainlineCols(0)(row) // denominator
        val rampTotal = offrampData(row, ramplaneIdx(0)) // numerator
        if mainlineLane0 == 0.0 then 0.0 else rampTotal / mainlineLane0
    end computeExitFraction

    def computeExitFractionMA(row: Int, window: Int): Double =
        if window <= 1 then return computeExitFraction(row)
        val start = math.max(0, row - window + 1)
        val count = row - start + 1
        var sum = 0.0
        var i = start
        while i <= row do
            sum += computeExitFraction(i)
            i += 1
        end while
        sum / count
    end computeExitFractionMA

    def exitFraction: Double =
        val average = computeExitFraction(0)
        ew.println(s"offramp exit fraction = $average")
        println(s"offramp exit fraction =${mainlineCols(0)(0)}, ${offrampData(0, ramplaneIdx(0))},  $average")
        average
    end exitFraction


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

        val coordMap = keys.zip(screenCoords).toMap

        val mainline = Array(
            coordMap("sensor1"),
            coordMap("sensor2"),            // offramp merge before sensor2
            coordMap("sensor3"),
            coordMap("sensor4"),
            coordMap("sensor5"),
            coordMap("sensor6")
        )

        val ramps = Array(
            coordMap("onramp1"),
            coordMap("onramp2"),
            coordMap("offramp")
        )
        Map(
            "mainline" -> mainline,
            "ramps" -> ramps
        )
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

    // Old: applied an extra (sx, sy) shift to ramps here
    def getRampCoordinates(dims: (Double, Double)): Array[(Double, Double)] =
        val (sx, sy) = (65.0, -70.0)
        getRoadCoordinates(dims)("ramps").map { case (x, y) => (x + sx, y + sy) }
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

end TrafficConfig


// Scala
@main def TrafficConfigTest(): Unit =
    val rowTime = 15 * MINUTE
    // test instantiation (anchor file must match one of the sensor IDs)
    val file    = "/Mainline_VDS_Redwood_Creek_US101-N/404532ML.csv"
    val cfg     = new TrafficConfig(file, rowTime, 0)
    println(s"exitFraction=${cfg.exitFraction}")
