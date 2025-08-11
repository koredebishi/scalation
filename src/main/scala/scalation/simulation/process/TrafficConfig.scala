package scalation
package simulation
package process


import scalation.mathstat.*
import scalation.random.*
import scalation.simulation.process.modeling.clustering.Coordinates


///**
// * @param fileName : the data file Pemps for this simulation
// * @param rowTime : the time interval for this simulation: 15min window
// * @param stream  : the stream value
// */
//class TrafficConfig(fileName: String, rowTime: Double, stream: Int = 0):
//
//    //--------------------------------------------------
//    // Load Data
//    private[process] val ew = new EasyWriter("recorder", "TrafficConfigText.txt")
//
//    val t1 = 1
//    val t2 = 2
////
////    val t1 = 2 * 96 + 16
////    val t2 = 3 * 96 // Just a few rows (for debugging)
//
//
//    val rowOffset = t1
//
//
//    val data = MatrixD.load(fileName, t1, t2)
//
//
//
//    //val data = MatrixD.load(fileName)
//    //println(s"\n the loaded data is: $data \n")
//
//    val laneIdx = VectorI(5, 8, 11, 14)                 // Indices for the four lanes
//
//    val arrivalCounts = data(?, laneIdx)                // data extract based on the flow/lanes
//    val totalArrivalsPerRow = arrivalCounts.sumVr // Sum of flows across the 4 lanes
//
//    val mu = totalArrivalsPerRow.map(rowTime / _) // the mu Vectors for each row. the intensity Vector.
//
//    val laneProbPerRow = arrivalCounts.mmap(_.toProbability)
//    //--------------------------------------------------
//    val laneRVPerRow = Vector.tabulate(data.dim)(row => Discrete(laneProbPerRow(row)))    // laneRV per roll stored here
//    //--------------------------------------------------
//    // Function to Get LaneRV for a Specific Row
//    @inline def getLaneRV(row: Int): Discrete = laneRVPerRow(row)                 // method to get the current laneRV for the current dataIndex
//
//    /**
//     * This method uses the Coodinate class to calculate the scales real data (Pims) to
//     * Animation coodinate for the simulation
//     *
//     * @param w_h   the width of the animation window
//     * @param path an array of lat-long coordinates from the file
//     */
//    def getJunctions(path: String, w_h: (Double, Double)): Array[(Double, Double)] =
//
//        val data = scala.io.Source.fromFile(path).getLines.toArray
//        val gps = Array.ofDim[(Double, Double)](data.length)
//
//        for i <- data.indices do
//            val ll = data(i).split(",")
//            val lat = ll(0).toDouble
//            val long = ll(1).toDouble
//            gps(i) = (lat, long)
//        end for
//        val coords = new Coordinates(w_h._1, w_h._2, gps)
//        coords.calcAniCoords()
//
//        for (lat, long) <- gps do println(s"the gps $lat and $long")
//        for (x, y) <- coords.aniCoords do println(s"the scaled coordinate is x: $x : y $y")
//
//        coords.aniCoords
//    end getJunctions
//
//
//end TrafficConfig
//
//
//
//
//@main def TrafficConfigTest(): Unit =
//    val rowTime = 15 * MINUTE
//    val stream = 0
//    val trafficData = new TrafficConfig("/seven_sensors/402376.csv", rowTime, stream)
//
//
//    println(s"the total arrival ${trafficData.mu}")


class TrafficConfig(fileName: String, rowTime: Double, stream: Int = 0):

    private[process] val ew = new EasyWriter("recorder", "TrafficConfigText.txt")
    private val t1 = 1
    private val t2 = 2
    private val rowOffset = t1

    // DEBUG: Segment length adjustment variables (set to 1.0 for real GPS accuracy)
    private val firstSegmentScale = 2.0   // Scale factor for first segment (sensor 0 to 1)
    private val lastSegmentScale = 2.0    // Scale factor for last segment (sensor 5 to 6)
    private val debugScaling = true       // Enable/disable debug scaling

    private val mainlineCoords = getJunctions(s"$DATA_DIR/15min_US101_N_Willow_to_Marsh_2miles_ML/gps_mainline.txt", (1000, 800))
    private val rampCoords = getJunctions(s"$DATA_DIR/15Min_US101_N_Willow_to_Marsh_Ramps/gps_ramp.txt", (1000, 800))


    private[process] val data = MatrixD.load(fileName, t1, t2)
    private[process] val laneIdx = VectorI(5, 8, 11, 14)
    private[process] val arrivalCounts = data(?, laneIdx)
    private[process] val totalArrivalsPerRow = arrivalCounts.sumVr        // Sum of flows across the 4 lanes for mainline only
    private[process] val muMain = totalArrivalsPerRow.map(rowTime / _)
    private[process] val laneProbPerRow = arrivalCounts.mmap(_.toProbability)
    private[process] val laneRVPerRow = Vector.tabulate(data.dim)(r => Discrete(laneProbPerRow(r)))     // need to use this ??

    //private val onRampData = Array.fill(3)(MatrixD.load("15Min_US101_N_Willow_to_Marsh_2miles_ML/400981.csv", t1, t2))

    val onRampData = Array(
                                MatrixD.load(s"15Min_US101_N_Willow_to_Marsh_Ramps/408267.csv", t1, t2),
                                MatrixD.load(s"15Min_US101_N_Willow_to_Marsh_Ramps/408264.csv", t1, t2),
                                MatrixD.load(s"15Min_US101_N_Willow_to_Marsh_Ramps/412783.csv", t1, t2)
    )

    private val muRamps = onRampData.map { mat =>
        val counts = mat(?, laneIdx).sumVr
        counts.map(rowTime / _)
    }


    //helper method to get sensor data for smape calculation
    def getSensorData(sensorId: String): VectorD =
        val sensorData = MatrixD.load(s"/15Min_US101_N_Willow_to_Marsh_2miles_ML/$sensorId.csv", t1, t2)
        val sensorArrivals = sensorData(?, laneIdx)
        sensorArrivals.sumVr
    end getSensorData



    def calcTotalArrivals(mat: MatrixD): VectorD = mat(?, laneIdx).sumVr

    //lazy val nStopArray = Array(2, 2, 2, 2)      // just a temp to help test the code

    lazy val nStopArray: Array[Int] =
        val mainStop = calcTotalArrivals(data).sum.toInt
        val rampStops = onRampData.map(m => calcTotalArrivals(m).sum.toInt)
        Array(mainStop) ++ rampStops
    end nStopArray

    val muPerSource: Array[VectorD] = Array(muMain) ++ muRamps

    def getMuForSource(idx: Int): VectorD = muPerSource(idx)

    @inline def getLaneRV(row: Int): Discrete = laneRVPerRow(row)

    def getJunctions(path: String, w_h: (Double, Double)): Array[(Double, Double)] =
        val data = scala.io.Source.fromFile(path).getLines.toArray
        val gps = data.map { line =>
            val Array(lat, long) = line.split(",").map(_.toDouble)
            (lat, long)
        }
        val coords = new Coordinates(w_h._1, w_h._2, gps)
        coords.calcAniCoords()
        coords.aniCoords
    end getJunctions

    def getMainlineCoordinates(dims: (Double, Double)): Array[(Double, Double)] =
        if debugScaling then
            val coords = mainlineCoords.toArray

            // Scale first segment: move sensor 1 further from sensor 0
            if coords.length > 1 then
                val (x0, y0) = coords(0)
                val (x1, y1) = coords(1)
                val dx = (x1 - x0) * (firstSegmentScale - 1.0)
                val dy = (y1 - y0) * (firstSegmentScale - 1.0)
                coords(1) = (x1 + dx, y1 + dy)

            // Scale last segment: move sensor 6 further from sensor 5
            if coords.length > 2 then
                val lastIdx = coords.length - 1
                val prevIdx = coords.length - 2
                val (xPrev, yPrev) = coords(prevIdx)
                val (xLast, yLast) = coords(lastIdx)
                val dx = (xLast - xPrev) * (lastSegmentScale - 1.0)
                val dy = (yLast - yPrev) * (lastSegmentScale - 1.0)
                coords(lastIdx) = (xLast + dx, yLast + dy)

            coords
        else
            mainlineCoords
            
    end getMainlineCoordinates
    //def getRampCoordinates(dims: (Double, Double)): Array[(Double, Double)] = rampCoords

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Ramp animation coords (apply your small nudge here, NOT at call site). */
    def getRampCoordinates(dims: (Double, Double)): Array[(Double, Double)] =
        // start from precomputed rampCoords; just bake in the +45/-50 shift
        val (sx, sy) = (45.0, -50.0)
        rampCoords.map { case (x, y) => (x + sx, y + sy) }
    end getRampCoordinates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** VSource layout: center position and per-source offsets (clean & centralized). */
    def getVSourceCenterAndOffsets(dims: (Double, Double))
    : ((Int, Int), Array[(Int, Int)]) =
        val main = getMainlineCoordinates(dims)
        val ramps = getRampCoordinates(dims)

        // center: a light nudge from the first main sensor (your convention)
        val centerPos = ((main(0)._1 + 100.0).toInt, (main(0)._2 + 100.0).toInt)

        // unified ramp-source shift (what you had inline before)
        val (dx, dy) = (130.0, -300.0)

        val offsets = Array(
            (0, 0),
            ((ramps(0)._1 + dx).toInt - centerPos._1, (ramps(0)._2 + dy).toInt - centerPos._2),
            ((ramps(1)._1 + dx).toInt - centerPos._1, (ramps(1)._2 + dy).toInt - centerPos._2),
            ((ramps(2)._1 + dx).toInt - centerPos._1, (ramps(2)._2 + dy).toInt - centerPos._2)
        )
        (centerPos, offsets)
    end getVSourceCenterAndOffsets




//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//    /** Full time-of-day × day-of-week imputation (vectorized where it matters).
//     * Implements historical profile imputation and preserves lane shares.
//     */
//    private def fullTimeOfDayImputation(): Array[MatrixD] =
//        val binsPerDay = (1440.0 / rowTime).toInt
//
//        inline def dayOfWeek(r: Int) = (r / binsPerDay) % 7
//
//        inline def binOfDay(r: Int) = r % binsPerDay
//
//        // Precompute row index buckets for each (dow, bin)
//        val R0 = onRampData.head.dim1
//        val buck = Array.ofDim[IVector](7, binsPerDay)
//        for dow <- 0 until 7 do
//            for b <- 0 until binsPerDay do
//                val rows = for r <- 0 until R0 if dayOfWeek(r) == dow && binOfDay(r) == b yield r
//                buck(dow)(b) = VectorI(rows: _*)
//            end for
//        end for
//
//        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//        /** Build historical profiles (means) for one ramp */
//        def histProfiles(m: MatrixD): (MatrixD, MatrixD) =
//            val C = laneIdx.dim
//            val tot = new MatrixD(7, binsPerDay)
//            val shr = new MatrixD(7, binsPerDay * C)
//
//            val lanesMat = m(?, laneIdx) // rows × C (lane slice)
//
//            for dow <- 0 until 7 do
//                for b <- 0 until binsPerDay do
//                    val idx = buck(dow)(b)
//                    if idx.dim > 0 then
//                        val sub = lanesMat(idx, ?) // |idx| × C
//                        val rowSum = sub.sumVr // |idx|-vector: per-row totals
//                        val keep = for i <- 0 until rowSum.dim if rowSum(i) > 0.0 yield i
//                        if keep.nonEmpty then
//                            val subK = sub(VectorI(keep: _*), ?) // keep only informative rows
//                            val meanTot = subK.sum().sum / subK.dim1 // (sum over all lanes & rows) / rows
//                            tot(dow, b) = meanTot
//
//                            if meanTot > 0.0 then
//                                val meanLane = subK.mean // 1 × C mean per lane
//                                val base = b * C
//                                for j <- 0 until C do
//                                    shr(dow, base + j) = meanLane(j) / meanTot
//                                end for
//                            else
//                                val base = b * C
//                                for j <- 0 until C do shr(dow, base + j) = 1.0 / C
//                            end if
//                        else
//                            tot(dow, b) = 0.0
//                            val base = b * C
//                            for j <- 0 until C do shr(dow, base + j) = 1.0 / C
//                        end if
//                    else
//                        tot(dow, b) = 0.0
//                        val base = b * C
//                        for j <- 0 until C do shr(dow, base + j) = 1.0 / C
//                    end if
//                end for
//            end for
//            (tot, shr)
//        end histProfiles
//
//        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//        /** Impute one ramp using profiles */
//        def imputeOne(m: MatrixD): MatrixD =
//            val (tot, shr) = histProfiles(m)
//            val out = m.copy
//            val C = laneIdx.dim
//            val R = m.dim1
//
//            val lanesMat = out(?, laneIdx) // view on lanes
//            val rowTot = lanesMat.sumVr // current totals per row
//
//            for r <- 0 until R do
//                if rowTot(r) == 0.0 then
//                    val dow = (r / binsPerDay) % 7
//                    val bin = r % binsPerDay
//                    val mean = tot(dow, bin)
//                    if mean > 1e-9 then
//                        val base = bin * C
//                        // write lane vector = mean * share(dow,bin,:)
//                        for j <- 0 until C do
//                            lanesMat(r, j) = mean * shr(dow, base + j)
//                        end for
//                    end if
//                end if
//            end for
//            out
//        end imputeOne
//
//        onRampData.map(imputeOne)
//    end fullTimeOfDayImputation
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//    /** Simple mean imputation for test datasets (lane-wise means). */
//    private def simpleZeroImputation(): Array[MatrixD] =
//        onRampData.map: mat =>
//            val out = mat.copy
//            val lanes = out(?, laneIdx) // rows × C
//            val C = laneIdx.dim
//
//            // compute lane means over non-zero rows (vectorized per lane)
//            val nonZero = lanes.sumVr // per-row totals
//            val keep = for i <- 0 until nonZero.dim if nonZero(i) > 0.0 yield i
//            if keep.nonEmpty then
//                val Lk = lanes(VectorI(keep: _*), ?) // kept rows
//                val meanLane = Lk.mean // 1 × C
//
//                // replace zeros row-wise where total==0 with meanLane
//                val zero = for i <- 0 until nonZero.dim if nonZero(i) == 0.0 yield i
//                if zero.nonEmpty then
//                    val Zi = VectorI(zero: _*)
//                    for j <- 0 until C do
//                        for i <- 0 until Zi.dim do lanes(Zi(i), j) = meanLane(j)
//                    end for
//                end if
//            end if
//            out
//    end simpleZeroImputation
//
//
//    def imputeOnRampData(): Array[MatrixD] =
//        val rows = onRampData.headOption.map(_.dim1).getOrElse(0)
//        val bins = (1440.0 / rowTime).toInt
//        if rows >= bins * 7 then fullTimeOfDayImputation() else simpleZeroImputation()
//    end imputeOnRampData


end TrafficConfig


@main def TrafficConfigTest(): Unit =
    val rowTime = 15 * MINUTE
    val stream = 0
    val trafficData = new TrafficConfig("/seven_sensors/402376.csv", rowTime, stream)

