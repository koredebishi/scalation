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
    val t2: Int = 1 // target 6pm


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

    // Per-lane mu series (carry-forward for zero counts): shape (5 x rows)
    //this mu value feeds into VSource to determine inter-arrival times per lane for th
    private val muMainlineLanes: MatrixD = new MatrixD(5, anchorData.dim)
    cfor(0, 5) { lane =>             // for each lane
        var lastValidMu = rowTime / 1.0                // initial mu value
        cfor(0, anchorData.dim) { row =>              // for each row
            val count = mainlineLaneTotalsPerRow(row, lane)        // get the count for that lane at that row
            if count > 0.0 then                                // if count is valid
                lastValidMu = rowTime / count                   // update last valid mu
            muMainlineLanes(lane, row) = lastValidMu            // set mu value for that lane at that row
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
    private val muRamps: MatrixD = new MatrixD(2, anchorData.dim)
    cfor(0, 2) { ramp =>
        var lastValidMu = rowTime / 1.0
        cfor(0, anchorData.dim) { row =>
            val count = onRampTotalsPerRow(ramp, row)
            if count > 0.0 then
                lastValidMu = rowTime / count
            muRamps(ramp, row) = lastValidMu
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

        val mainline = Array.ofDim[(Double, Double)](6)
        mainline(0) = coordMap("sensor1")
        mainline(1) = coordMap("sensor2")
        mainline(2) = coordMap("sensor3")
        mainline(3) = coordMap("sensor4")
        mainline(4) = coordMap("sensor5")
        mainline(5) = coordMap("sensor6")

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



end TrafficConfig
// One source per lane. Used in VSource to select lane based on discrete distribution.
