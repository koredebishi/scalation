//package scalation
//package simulation
//package process
//
//
//import scalation.mathstat.*
//import scalation.random.*
//import scalation.simulation.process.example_1.Roadcood
//import scala.util.Using
//import scala.io.Source
//
//
//class TrafficConfig(anchorSensorId: String ="1-404531ML" , rowTime: Double, stream: Int = 0):
//
//    private[process] val ew = new EasyWriter("recorder", "TrafficConfigText.txt")
//
//    private val loadStart: Long = System.nanoTime()
//
//    //full day 6AM to 6PM Tuesday  ; make sure it's not an holiday
//    //    val t1: Int = 2 * 96 + 16
//    //    val t2: Int = 3 * 96
//
//    //    val t1: Int = 25    // target 6am
//    //    val t2: Int = 72    // target 6pm
//
//    val t1: Int = 0 // target 6am
//    val t2: Int = 4 // target 6pm
//
//
//    private val rowOffset = t1
//    private val laneIdx = VectorI(3, 5, 7, 9, 11) // mainline lane FLOW columns
//    private val ramplaneIdx = VectorI(1) // ramp/offramp TOTAL FLOW column
//
//
//    // ----------------------------------------------------------------------
//    // Unified Sensor Map: declare once, use everywhere
//    private val sensorMap: Map[String, String] = Map(
//        // mainline sensors
//        "1-404531ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/1-404531ML.csv", // driver source   // needs to be updated to 404532
//        "2-404532ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/2-404532ML.csv", //
//        "3-401834ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/3-401834ML.csv", // eval after offramp
//        "4-401833ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/4-401833ML.csv", // eval after onramp1
//        "5-401929ML" -> s"Mainline_VDS_Redwood_Creek_US101-N/5-401929ML.csv",
//        // ramps
//        "1-410095OR" -> s"Ramps_VDS_Redwood_Creek_US101-N/1-410095OR.csv", // onramp1
//        "2-410093OR" -> s"Ramps_VDS_Redwood_Creek_US101-N/2-410093OR.csv", // onramp2
//        "1-410094FR" -> s"Ramps_VDS_Redwood_Creek_US101-N/1-410094FR.csv" // offramp
//    )
//
//    // ----------------------------------------------------------------------
//    // Load all sensor data once as a Map
//    val allSensorData: Map[String, MatrixD] =
//        sensorMap.view.mapValues(path => MatrixD.load(path, t1, t2)).toMap
//
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//    /** Get PEMS count matrix for mainline sensor (rows=time, cols=lanes).
//     * Matches format of junc(i).getCountMatrix for apple-to-apple comparison.
//     *
//     * @param idx sensor index: 0="1-404531ML", 1="2-404532ML", 2="3-401834ML", 3="4-401833ML", 4="5-401929ML"
//     */
//    def getPemsCountMatrix(idx: Int): MatrixD =
//        val mainlineIds = Array("1-404531ML", "2-404532ML", "3-401834ML", "4-401833ML", "5-401929ML")
//        val mainlineData = allSensorData(mainlineIds(idx))
//        val flowMainlineData = mainlineData(?, laneIdx)
//        // row0 : [lane1, lane2, lane3, lane4,lane5]
//        // row1 : [lane1, lane2, lane3,lane4,lane5]
//        // for our full data Matrix Dimention is 48 rows x 5 lanes_flow columns.
//        flowMainlineData   // return the flows from each lanes as a MatrixD : the size of this matrix is rowtimeNumber x lanes,
//    end getPemsCountMatrix
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//    /** Get PEMS count matrix for ramp sensor (rows=time, cols=1).
//     * Matches format of ramp_sensors(i).getCountMatrix for apple-to-apple comparison.
//     * @param idx ramp index: 0="1-410095OR" (onramp1), 1="2-410093OR" (onramp2), 2="1-410094FR" (offramp)
//     */
//    def getPemsCountRampMatrix(idx: Int): MatrixD =
//        val rampIds = Array("1-410095OR", "2-410093OR", "1-410094FR")
//        val rampData = allSensorData(rampIds(idx))
//        val flowRampData = rampData(?, ramplaneIdx)
//        // row0 : [lane1]     //  the total lane flow for ramp @ col 1
//        // row2 : [lane1]
//        flowRampData   // return the flows from ramp as a MatrixD
//    end getPemsCountRampMatrix
//
//
//    // Direct sensor ID usage (no path parsing)
//    private[process] val anchorData: MatrixD = allSensorData(anchorSensorId)
//
//    def dim: Int = anchorData.dim
//
//
//    // Public accessor for backward compatibility (mimics old data.dim access)
//    /** Get bootstrapped (or raw) mainline sensor matrix for evaluation.
//     * @param idx sensor index: 0="1-404531ML", 1="2-404532ML", 2="3-401834ML", 3="4-401833ML", 4="5-401929ML"
//     * @return MatrixD of bootstrapped/raw lane flows for the specified sensor
//     */
//    def getBootstrappedMainlineMatrix(idx: Int): MatrixD =
//        val mainlineIds = Array("1-404531ML", "2-404532ML", "3-401834ML", "4-401833ML", "5-401929ML")
//        mainlineMats(mainlineIds(idx))
//    end getBootstrappedMainlineMatrix
//
//    // ----------------------------------------------------------------------
//    // Mainline: use raw arrays for hot path
//    val mainlineCols = laneIdx.map(c => anchorData(?, c)) // extract columns as arrays
//
//    val totalArrivalsPerRow: Array[Double] =
//        Array.tabulate(anchorData.dim)(r => mainlineCols.map(_(r)).sum)
//
//    /**
//     * Compute mean inter-arrival time (mu) for the mainline source across all time rows.
//     *
//     * Literature-based approach (Highway Capacity Manual, PeMS standards):
//     * - Uses carry-forward (LOCF - Last Observation Carried Forward) for zero-count intervals
//     * - When a time window reports zero vehicles, the previous valid mu is maintained
//     * - This prevents division by zero and reflects traffic flow reality: zero counts typically
//     *   represent brief gaps between vehicle platoons, not a complete cessation of traffic
//     *
//     * Formula (when count > 0): mu = rowTime / vehicleCount
//     * Formula (when count = 0): mu = previous valid mu (carry forward)
//     *
//     * Default fallback: If the first row has zero count, assumes 1 vehicle/period as baseline
//     *
//     * @return Array of mu values (mean inter-arrival time in seconds) for each time row
//     */
//    val muMain: Array[Double] =
//        var lastValidMu = rowTime / 1.0  // default fallback: assume 1 vehicle per period
//        totalArrivalsPerRow.map: count =>
//            if count > 0.0 then
//                lastValidMu = rowTime / count  // update and use new mu value
//                lastValidMu
//            else
//                lastValidMu  // carry forward last valid value when count = 0
//    end muMain
//
//    val laneProbPerRow: Array[Array[Double]] =
//        Array.tabulate(anchorData.dim)(r => {
//            val row = laneIdx.map(c => anchorData(r, c))
//            val sum = row.sum
//            row.map(_ / sum).toArray
//        })
//    val laneRVPerRow: Array[Discrete] =
//        laneProbPerRow.map(p => Discrete(VectorD(p)))
//
//    // ----------------------------------------------------------------------
//    // Ramps
//    private val onRampIds = Array("1-410095OR", "2-410093OR") // preserved for compatibility
//    private val offrampId = "1-410094FR"
//
//    private val offrampData = allSensorData(offrampId) // cached ref
//
//    val onRampTotalsPerRow: Array[Array[Double]] =
//        onRampIds.map(id => allSensorData(id)(?, ramplaneIdx(0)).toArray)
//
//    /**
//     * Compute mean inter-arrival time (mu) for each on-ramp source across all time rows.
//     *
//     * Literature-based approach (Traffic Flow Theory, Poisson Process estimation):
//     * - Applies carry-forward (LOCF) independently for each ramp to handle zero-count intervals
//     * - Each ramp maintains its own last valid mu value for temporal continuity
//     * - Prevents Infinity/NaN when CSV data contains rows like: 5.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
//     * - Ensures VSource always receives valid, finite mu values for inter-arrival generation
//     *
//     * Implementation:
//     * - For each ramp (onramp1, onramp2), independently track lastValidMu
//     * - When count > 0: calculate and update mu = rowTime / count
//     * - When count = 0: reuse previous valid mu (no traffic generation freeze)
//     *
//     * Default fallback per ramp: If first row is zero, assumes 1 vehicle/period as baseline
//     *
//     * @return 2D Array where muRamps(i)(j) = mu for ramp i at time row j
//     */
//    private val muRamps: Array[Array[Double]] =
//        onRampTotalsPerRow.map: rampData =>
//            var lastValidMu = rowTime / 1.0  // default fallback per ramp
//            rampData.map: count =>
//                if count > 0.0 then
//                    lastValidMu = rowTime / count
//                    lastValidMu
//                else
//                    lastValidMu  // carry forward last valid value
//    end muRamps
////    // ----------------------------------------------------------------------
////    // Totals
////    lazy val sensor1Total: Int = totalArrivalsPerRow.sum.toInt
////    lazy val onramp1Total: Int = onRampTotalsPerRow(0).sum.toInt
////    lazy val onramp2Total: Int = onRampTotalsPerRow(1).sum.toInt
////    lazy val offrampTotal: Int = offrampData(?, ramplaneIdx(0)).sum.toInt
////
////    // ----------------------------------------------------------------------
////    // Sources
////
////    val nStopArray: Array[Int] = Array(sensor1Total, onramp1Total, onramp2Total)
//
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//        /** Generate a block-bootstrapped synthetic day from the anchor sensor data.
//         * Each block = contiguous time segment (e.g. 4 rows = 1 hour @ 15 min intervals).
//         * The method follows Politis (2003) §2.2 formula X* = Concat(B_i1 ,… ,B_ik).
//         *
//         * @param blockSize number of rows per block  (e.g., 4 → 1 hour)
//         * @return MatrixD   new synthetic traffic matrix for the driver sensor (5 lane flows)
//         */
//        def bootstrapSyntheticDay(blockSize: Int = 4): MatrixD =
//            val anchorMat = anchorData(?, laneIdx)                          // Extract lane flow columns from anchor sensor - same pattern as totalArrivalsPerRow
//            val nRows = anchorMat.dim                                   // Total number of time rows (e.g., 48 for 12 hours of 15-min intervals)
//            val nCols = anchorMat.dim2                                  // Number of lane columns (5 lanes)
//            val nBlocks = nRows - blockSize + 1                         // Number of overlapping blocks (e.g., 45 blocks of size 4 from 48 rows)
//
//            // Create sliding window blocks - simplified to avoid matrix slicing issues
//            val blocks = Array.tabulate(nBlocks) { i =>
//                val blockData = Array.ofDim[Double](blockSize * nCols)
//                for r <- 0 until blockSize; c <- 0 until nCols do
//                    blockData(r * nCols + c) = anchorMat(i + r, c)
//                MatrixD((blockSize, nCols), blockData*)
//            }
//
//            val k = (nRows.toDouble / blockSize).ceil.toInt             // Number of blocks needed to rebuild full day (e.g., 12 blocks of 4 rows)
//            val rand = scala.util.Random                                // Random number generator for bootstrap sampling
//
//            val resampled =
//                (0 until k).map(_ => blocks(rand.nextInt(nBlocks))).reduce(_ ++ _) // Randomly sample k blocks and concatenate them
//
//            // Trim resampled data to match original row count
//            if resampled.dim > nRows then                               // If concatenation produced more rows than needed
//                // Extract only the first nRows rows
//                val trimmedData = Array.ofDim[Double](nRows * nCols)
//                for r <- 0 until nRows; c <- 0 until nCols do
//                    trimmedData(r * nCols + c) = resampled(r, c)
//                MatrixD((nRows, nCols), trimmedData*)
//            else
//                resampled  // Already the right size
//        end bootstrapSyntheticDay
//
//        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//        /** Generate block-bootstrapped matrices for on-ramps and off-ramp.
//         * Keeps the same number of rows (48 × 1) for each ramp file.
//         */
//        def bootstrapRamps(blockSize: Int = 4): Map[String, MatrixD] =
//            val rampIds = Array("1-410095OR", "2-410093OR", "1-410094FR") // Onramp1, Onramp2, Offramp sensor IDs
//            val rand = scala.util.Random // Random number generator for bootstrap sampling
//
//            Array(0, 1, 2).map { idx =>
//                val id = rampIds(idx)                                                   // Get ramp sensor ID
//                val mat = allSensorData(id)(?, ramplaneIdx)                             // Extract ramp flow column - same pattern as offrampData(?, ramplaneIdx)
//                val n = mat.dim                                                     // Total number of time rows for this ramp
//                val nCols = mat.dim2                                                // Number of columns (always 1 for ramps)
//                val nb = n - blockSize + 1                                          // Number of overlapping blocks available for sampling
//
//                // Create sliding window blocks - simplified to avoid matrix slicing issues
//                val blocks = Array.tabulate(nb) { i =>
//                    val blockData = Array.ofDim[Double](blockSize * nCols)
//                    for r <- 0 until blockSize; c <- 0 until nCols do
//                        blockData(r * nCols + c) = mat(i + r, c)
//                    MatrixD((blockSize, nCols), blockData*)
//                }
//
//                val k = (n.toDouble / blockSize).ceil.toInt                         // Number of blocks needed to reconstruct full day
//                val res = (0 until k).map(_ => blocks(rand.nextInt(nb))).reduce(_ ++ _) // Randomly sample k blocks and concatenate
//
//                val trimmed = if res.dim > n then                                       // If concatenation produced more rows than needed
//                    // Extract only the first n rows
//                    val trimmedData = Array.ofDim[Double](n * nCols)
//                    for r <- 0 until n; c <- 0 until nCols do
//                        trimmedData(r * nCols + c) = res(r, c)
//                    MatrixD((n, nCols), trimmedData*)
//                else
//                    res  // Already the right size
//
//                rampIds(idx) -> trimmed                                                           // Return tuple of (ramp ID, bootstrapped matrix)
//            }.toMap                                                                     // Convert array of tuples to Map
//        end bootstrapRamps
//
//        // ----------------------------------------------------------------------
//        // Totals (bootstrapped version)
//        // ----------------------------------------------------------------------
//
//        /** Compute total flows using either raw or bootstrapped matrices.
//         * If enableBootstrap = true, generates synthetic day (Politis 2003 §2.2)
//         * otherwise uses the raw sensor data.
//         *
//         * The result directly feeds nStopArray driving the VSource creation.
//         */
//        val enableBootstrap = true // <-- flip this to false to restore original logic
//        val blockSize = 4 // 1-hour blocks for 15-min rows
//
//        // Mainline sensors (all 5 sensors) - for evaluation
//        val mainlineMats: Map[String, MatrixD] =
//            if enableBootstrap then bootstrapMainlineSensors(blockSize)
//            else Map(
//                "1-404531ML" -> allSensorData("1-404531ML")(?, laneIdx),
//                "2-404532ML" -> allSensorData("2-404532ML")(?, laneIdx),
//                "3-401834ML" -> allSensorData("3-401834ML")(?, laneIdx),
//                "4-401833ML" -> allSensorData("4-401833ML")(?, laneIdx),
//                "5-401929ML" -> allSensorData("5-401929ML")(?, laneIdx)
//            )
//
//        // Anchor sensor data (for driving simulation)
//        private val mainlineMat = mainlineMats(anchorSensorId)
//
//        // Ramps (onramp1, onramp2, offramp)
//        private val rampMats =
//            if enableBootstrap then bootstrapRamps(blockSize)
//            else Map(
//                "1-410095OR" -> allSensorData("1-410095OR")(?, ramplaneIdx),
//                "2-410093OR" -> allSensorData("2-410093OR")(?, ramplaneIdx),
//                "1-410094FR" -> allSensorData("1-410094FR")(?, ramplaneIdx)
//            )
//
//        // ----------------------------------------------------------------------
//        // Compute totals
//        lazy val sensor1Total: Int = mainlineMat.sum.toInt
//        lazy val onramp1Total: Int = rampMats("1-410095OR").sum.toInt
//        lazy val onramp2Total: Int = rampMats("2-410093OR").sum.toInt
//        lazy val offrampTotal: Int = rampMats("1-410094FR").sum.toInt
//
//        // ----------------------------------------------------------------------
//        // Sources: feeds the simulation
//        val nStopArray: Array[Int] = Array(sensor1Total, onramp1Total, onramp2Total)
//
//
//        println(s"nStop array: ${nStopArray.mkString(", ")}")
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//
//    val muPerSource: Array[Array[Double]] =
//        Array(muMain) ++ muRamps
//
//
//    def getMuForSource(i: Int): Array[Double] =
//        //println(s"the saferow: $i")
//        muPerSource(i)
//
//
//    @inline def getLaneRV(row: Int): Discrete = laneRVPerRow(row)
//
//    // ----------------------------------------------------------------------
//    // Exit fraction (offramp row0 / mainline lane0 row0)
//
//    /**
//     * Compute raw exit fraction for a single row (offramp flow / mainline lane0 flow).
//     *
//     * @param row Time row index
//     * @return Exit fraction (0.0 if mainline has zero flow)
//     */
//    private def computeExitFraction(row: Int): Double =
//        val mainlineLane = totalArrivalsPerRow(row)   //mainlineCols(0)(row) // denominator      /// bug fix //    // offramp
//        //println(s"mainline?? $mainlineLane")
//        val rampTotal = offrampData(row, ramplaneIdx(0)) // numerator
//        //println(s"offramp?? $rampTotal")
//        if mainlineLane == 0.0 then 0.0 else rampTotal / mainlineLane             //mainlineLane0
//    end computeExitFraction
//
//    /**
//     * Pre-computed raw exit fractions for all time rows.
//     * Computed once during initialization to avoid redundant calculations in MA computation.
//     *
//     * Performance optimization:
//     * - Old approach: For 96 rows with window=5, computeExitFraction() called ~4,656 times
//     * - New approach: computeExitFraction() called exactly 96 times (once per row)
//     * - MA computation becomes simple array slicing and averaging
//     */
//    private lazy val exitFractionRaw: Array[Double] =
//        Array.tabulate(anchorData.dim)(row => computeExitFraction(row))
//
//    /**
//     * Pre-computed moving average exit fractions for all time rows (window=5).
//     * Used by Car.act() for efficient lookup: config.exitFractionMA(curRow)
//     *
//     * Performance: Single array access instead of computing MA on-the-fly per vehicle
//     */
//    lazy val exitFractionMA: Array[Double] =
//        Array.tabulate(anchorData.dim)(row => computeExitFractionMA(row, 5))
//    /**
//     * Compute moving average of exit fraction using pre-computed raw values.
//     *
//     * @param row Current time row index
//     * @param window MA window size (default 5)
//     * @return Moving average of exit fraction over the window
//     */
//    def computeExitFractionMA(row: Int, window: Int): Double =
//        if window <= 1 then return exitFractionRaw(row)
//        val start = math.max(0, row - window + 1)
//        val count = row - start + 1
//        var sum = 0.0
//        var i = start
//        while i <= row do
//            sum += exitFractionRaw(i)  // Direct array access - no redundant computation
//            i += 1
//        end while
//        sum / count
//    end computeExitFractionMA
//
//    def exitFraction: Double =
//        val average = computeExitFraction(0)
//        //ew.println(s"offramp exit fraction = $average")
//        //println(s"offramp exit fraction =${mainlineCols(0)(0)}, ${offrampData(0, ramplaneIdx(0))},  $average")
//        average
//    end exitFraction
//
//
//
//    /**
//     * Use RoadCood to load all GPS coordinates and convert them to screen coordinates
//     * Returns:
//     *  - mainline: sensor1..sensor6
//     *  - ramps: onramp1,onramp2,offramp
//     */
//    def getRoadCoordinates(dims: (Double, Double)): Map[String, Array[(Double, Double)]] =
//        val allLatLongs = Roadcood.latlong
//        val coordsArray = allLatLongs.values.toArray
//        val keys = allLatLongs.keys.toArray
//
//        val coordinates = new scalation.Coordinates(dims._1, dims._2, coordsArray)
//        val screenCoords = coordinates.aniCoords
//
//        val coordMap = keys.zip(screenCoords).toMap
//
//        val mainline = Array(
//            coordMap("sensor1"),
//            coordMap("sensor2"),            // offramp merge before sensor2// old offramp loc
//            coordMap("sensor3"),            // New offramp loc
//            coordMap("sensor4"),
//            coordMap("sensor5"),
//            coordMap("sensor6")
//        )
//
//        val ramps = Array(
//            coordMap("onramp1"),
//            coordMap("onramp2"),
//            coordMap("offramp")
//        )
//        Map(
//            "mainline" -> mainline,
//            "ramps" -> ramps
//        )
//    end getRoadCoordinates
//
//    // Legacy CSV-based junctions method (kept for compatibility)
//    def getJunctions(path: String, w_h: (Double, Double)): Array[(Double, Double)] =
//        Using.resource(Source.fromFile(path)) { src =>
//            val data = src.getLines().toArray
//            val gps = data.map { line =>
//                val Array(lat, long) = line.split(",").map(_.toDouble)
//                (lat, long)
//            }
//            val coords = new scalation.Coordinates(w_h._1, w_h._2, gps)
//            coords.calcAniCoords()
//            coords.aniCoords
//        }
//    end getJunctions
//
//    // Provide coordinate accessors without relying on undefined cached values
//    def getMainlineCoordinates(dims: (Double, Double)): Array[(Double, Double)] =
//        getRoadCoordinates(dims)("mainline")
//
//    // Old: applied an extra (sx, sy) shift to ramps here
//    def getRampCoordinates(dims: (Double, Double)): Array[(Double, Double)] =
//        val (sx, sy) = (65.0, -70.0)
//        getRoadCoordinates(dims)("ramps").map { case (x, y) => (x + sx, y + sy) }
//    end getRampCoordinates
//
//    def getSensorCoordinates(dims: (Double, Double)): Array[(Double, Double)] =
//        getMainlineCoordinates(dims)
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//    def getVSourceCenterAndOffsets(dims: (Double, Double)): ((Int, Int), Array[(Int, Int)]) =
//        val main = getMainlineCoordinates(dims)
//        val ramps = getRampCoordinates(dims)
//
//        val centerPos = ((main(0)._1 + 100.0).toInt, (main(0)._2 + 100.0).toInt)
//
//        // NOTE: keep the (dx, dy) shift here per requirement
//        val (dx, dy) = (800.0, -350.0)
//
//        val offsets = Array(
//            (0, 0),
//            ((ramps(0)._1 + dx).toInt - centerPos._1, (ramps(0)._2 + dy).toInt - centerPos._2),
//            ((ramps(1)._1 + dx).toInt - centerPos._1, (ramps(1)._2 + dy).toInt - centerPos._2),
//            ((ramps(2)._1 + dx).toInt - centerPos._1, (ramps(2)._2 + dy).toInt - centerPos._2)
//        )
//        (centerPos, offsets)
//    end getVSourceCenterAndOffsets
//
//    /** Generate block-bootstrapped matrices for all 5 mainline sensors.
//         * Each sensor gets its own bootstrapped version for evaluation purposes.
//         * The method follows Politis (2003) §2.2 formula X* = Concat(B_i1 ,… ,B_ik).
//         *
//         * @param blockSize number of rows per block  (e.g., 4 → 1 hour)
//         * @return Map[String, MatrixD] mapping sensor ID to bootstrapped traffic matrix (5 lane flows)
//         */
//        def bootstrapMainlineSensors(blockSize: Int = 4): Map[String, MatrixD] =
//            val mainlineIds = Array("1-404531ML", "2-404532ML", "3-401834ML", "4-401833ML", "5-401929ML")
//            val rand = scala.util.Random // Random number generator for bootstrap sampling
//
//            mainlineIds.map { id =>
//                val sensorData = allSensorData(id)(?, laneIdx)              // Extract lane flow columns for this sensor
//                val nRows = sensorData.dim                                  // Total number of time rows
//                val nCols = sensorData.dim2                                 // Number of lane columns (5 lanes)
//                val nBlocks = nRows - blockSize + 1                         // Number of overlapping blocks
//
//                // Create sliding window blocks - simplified to avoid matrix slicing issues
//                val blocks = Array.tabulate(nBlocks) { i =>
//                    val blockData = Array.ofDim[Double](blockSize * nCols)
//                    for r <- 0 until blockSize; c <- 0 until nCols do
//                        blockData(r * nCols + c) = sensorData(i + r, c)
//                    MatrixD((blockSize, nCols), blockData*)
//                }
//
//                val k = (nRows.toDouble / blockSize).ceil.toInt             // Number of blocks needed to rebuild full day
//                val resampled =
//                    (0 until k).map(_ => blocks(rand.nextInt(nBlocks))).reduce(_ ++ _) // Randomly sample k blocks and concatenate
//
//                // Trim resampled data to match original row count
//                val trimmed = if resampled.dim > nRows then
//                    val trimmedData = Array.ofDim[Double](nRows * nCols)
//                    for r <- 0 until nRows; c <- 0 until nCols do
//                        trimmedData(r * nCols + c) = resampled(r, c)
//                    MatrixD((nRows, nCols), trimmedData*)
//                else
//                    resampled  // Already the right size
//
//                id -> trimmed                                               // Return tuple of (sensor ID, bootstrapped matrix)
//            }.toMap
//        end bootstrapMainlineSensors



package scalation
package simulation
package process


import scalation.mathstat.*
import scalation.random.*
import scalation.simulation.process.example_1.Roadcood
import scala.util.Using
import scala.io.Source


class TrafficConfig(anchorSensorId: String ="1-404531ML" , rowTime: Double, stream: Int = 0):

    private[process] val ew = new EasyWriter("recorder", "TrafficConfigText.txt")

    private val loadStart: Long = System.nanoTime()


    val t1: Int = 0 // target 6am
    val t2: Int = 49 // target 6pm


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
    // Load all sensor data once as a Map
    val allSensorData: Map[String, MatrixD] =
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
    private[process] val anchorData: MatrixD = allSensorData(anchorSensorId)

    // Public accessor for backward compatibility (mimics old data.dim access)
    def dim: Int = anchorData.dim

    // ----------------------------------------------------------------------
    // Mainline: use raw arrays for hot path
    val mainlineCols = laneIdx.map(c => anchorData(?, c)) // extract columns as arrays

    val totalArrivalsPerRow: Array[Double] =
        Array.tabulate(anchorData.dim)(r => mainlineCols.map(_(r)).sum)

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
        Array.tabulate(anchorData.dim)(r => {
            val row = laneIdx.map(c => anchorData(r, c))
            val sum = row.sum
            val value1 = row.map(_ / sum).toArray
            row.map(_ / sum).toArray
        })

//    val laneRVPerRow: Array[Discrete] =
//        laneProbPerRow.map(p => Discrete(VectorD(p)))

    val laneRVPerRow: Array[Discrete] = {
        val result = laneProbPerRow.map(p => Discrete(VectorD(p)))
        println(s"laneRVPerRow: ${result.mkString(", ")}")
        result
    }


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
        //println(s"the saferow: $i")
        muPerSource(i)


    @inline def getLaneRV(row: Int): Discrete = laneRVPerRow(row)

    // ----------------------------------------------------------------------
    // Exit fraction (offramp row0 / mainline lane0 row0)

    /**
     * Compute raw exit fraction for a single row (offramp flow / mainline lane0 flow).
     *
     * @param row Time row index
     * @return Exit fraction (0.0 if mainline has zero flow)
     */
    private def computeExitFraction(row: Int): Double =
        val mainlineLane = totalArrivalsPerRow(row)   //mainlineCols(0)(row) // denominator      /// bug fix //    // offramp
        //println(s"mainline?? $mainlineLane")
        val rampTotal = offrampData(row, ramplaneIdx(0)) // numerator
        //println(s"offramp?? $rampTotal")
        if mainlineLane == 0.0 then 0.0 else rampTotal / mainlineLane             //mainlineLane0
    end computeExitFraction

    /**
     * Pre-computed raw exit fractions for all time rows.
     * Computed once during initialization to avoid redundant calculations in MA computation.
     *
     * Performance optimization:
     * - Old approach: For 96 rows with window=5, computeExitFraction() called ~4,656 times
     * - New approach: computeExitFraction() called exactly 96 times (once per row)
     * - MA computation becomes simple array slicing and averaging
     */
    private lazy val exitFractionRaw: Array[Double] =
        Array.tabulate(anchorData.dim)(row => computeExitFraction(row))

    /**
     * Pre-computed moving average exit fractions for all time rows (window=5).
     * Used by Car.act() for efficient lookup: config.exitFractionMA(curRow)
     *
     * Performance: Single array access instead of computing MA on-the-fly per vehicle
     */
    lazy val exitFractionMA: Array[Double] =
        Array.tabulate(anchorData.dim)(row => computeExitFractionMA(row, 5))
    /**
     * Compute moving average of exit fraction using pre-computed raw values.
     *
     * @param row Current time row index
     * @param window MA window size (default 5)
     * @return Moving average of exit fraction over the window
     */
    def computeExitFractionMA(row: Int, window: Int): Double =
        if window <= 1 then return exitFractionRaw(row)
        val start = math.max(0, row - window + 1)
        val count = row - start + 1
        var sum = 0.0
        var i = start
        while i <= row do
            sum += exitFractionRaw(i)  // Direct array access - no redundant computation
            i += 1
        end while
        sum / count
    end computeExitFractionMA

    def exitFraction: Double =
        val average = computeExitFraction(0)
        //ew.println(s"offramp exit fraction = $average")
        //println(s"offramp exit fraction =${mainlineCols(0)(0)}, ${offrampData(0, ramplaneIdx(0))},  $average")
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
            coordMap("sensor2"),            // offramp merge before sensor2// old offramp loc
            coordMap("sensor3"),            // New offramp loc
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