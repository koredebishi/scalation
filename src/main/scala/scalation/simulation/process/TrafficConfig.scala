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

//full day 6AM to 6PM Tuesday  ; make sure it's not an holiday
//    val t1: Int = 2 * 96 + 16
//    val t2: Int = 3 * 96

    val t1: Int = 25    // target 6am
    val t2: Int = 72    // target 6pm


    private val rowOffset = t1

    println(s"TrafficConfig: loading data from row $t1 to $t2 (offset $rowOffset)")

    //private val laneIdx = VectorI(4, 7, 10, 13, 16) // mainline lane FLOW columns
    private val laneIdx = VectorI(5, 8, 11, 14, 17) // mainline lane FLOW columns
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

    /**
     * Compute mean inter-arrival time (mu) for the mainline source across all time rows.
     *
     * Literature-based approach (Highway Capacity Manual, PeMS standards):
     * - Uses carry-forward (LOCF - Last Observation Carried Forward) for zero-count intervals
     * - When a time window reports zero vehicles, the previous valid mu is maintained
     * - This prevents division by zero and reflects traffic flow reality: zero counts typically
     *   represent brief gaps between vehicle platoons, not a complete cessation of traffic
     *
     * Formula (when count > 0): mu = rowTime / vehicleCount
     * Formula (when count = 0): mu = previous valid mu (carry forward)
     *
     * Default fallback: If the first row has zero count, assumes 1 vehicle/period as baseline
     *
     * @return Array of mu values (mean inter-arrival time in seconds) for each time row
     */
    val muMain: Array[Double] =
        var lastValidMu = rowTime / 1.0  // default fallback: assume 1 vehicle per period
        totalArrivalsPerRow.map: count =>
            if count > 0.0 then
                lastValidMu = rowTime / count  // update and use new mu value
                lastValidMu
            else
                lastValidMu  // carry forward last valid value when count = 0
    end muMain

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

    /**
     * Compute mean inter-arrival time (mu) for each on-ramp source across all time rows.
     *
     * Literature-based approach (Traffic Flow Theory, Poisson Process estimation):
     * - Applies carry-forward (LOCF) independently for each ramp to handle zero-count intervals
     * - Each ramp maintains its own last valid mu value for temporal continuity
     * - Prevents Infinity/NaN when CSV data contains rows like: 5.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
     * - Ensures VSource always receives valid, finite mu values for inter-arrival generation
     *
     * Implementation:
     * - For each ramp (onramp1, onramp2), independently track lastValidMu
     * - When count > 0: calculate and update mu = rowTime / count
     * - When count = 0: reuse previous valid mu (no traffic generation freeze)
     *
     * Default fallback per ramp: If first row is zero, assumes 1 vehicle/period as baseline
     *
     * @return 2D Array where muRamps(i)(j) = mu for ramp i at time row j
     */
    private val muRamps: Array[Array[Double]] =
        onRampTotalsPerRow.map: rampData =>
            var lastValidMu = rowTime / 1.0  // default fallback per ramp
            rampData.map: count =>
                if count > 0.0 then
                    lastValidMu = rowTime / count
                    lastValidMu
                else
                    lastValidMu  // carry forward last valid value
    end muRamps

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

    val muPerSource: Array[Array[Double]] =
        Array(muMain) ++ muRamps


    def getMuForSource(i: Int): Array[Double] =
        println(s"the saferow: $i")
        muPerSource(i)


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
    val file    = "data/Tuesday-June-2025/d04_text_station_5min_2025_06_03.csv"

