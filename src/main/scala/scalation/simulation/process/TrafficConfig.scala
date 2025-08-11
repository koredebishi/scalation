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
//
//    val srcNames = Array("Vsrc", "srcRamp1", "srcRamp2", "srcRamp3")
//    val srcToJunc = Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 4)
//
//    val sinkNames = Array("Sink", "OffRampSink")
//    val sinkToJunction = Map(0 -> 6, 1 -> 3)

    private[process] val data = MatrixD.load(fileName, t1, t2)
    private[process] val laneIdx = VectorI(5, 8, 11, 14)
    private[process] val arrivalCounts = data(?, laneIdx)
    private[process] val totalArrivalsPerRow = arrivalCounts.sumVr
    private[process] val muMain = totalArrivalsPerRow.map(rowTime / _)
    private[process] val laneProbPerRow = arrivalCounts.mmap(_.toProbability)
    private[process] val laneRVPerRow = Vector.tabulate(data.dim)(r => Discrete(laneProbPerRow(r)))

    private val onRampData = Array.fill(3)(MatrixD.load("15Min_US101_N_Willow_to_Marsh_2miles_ML/400981.csv", t1, t2))

    private val muRamps = onRampData.map { mat =>
        val counts = mat(?, laneIdx).sumVr
        counts.map(rowTime / _)
    }

    private def calcTotalArrivals(mat: MatrixD): VectorD = mat(?, laneIdx).sumVr

    lazy val nStopArray = Array(2, 2, 2, 2)

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


end TrafficConfig


@main def TrafficConfigTest(): Unit =
    val rowTime = 15 * MINUTE
    val stream = 0
    val trafficData = new TrafficConfig("/seven_sensors/402376.csv", rowTime, stream)

//
//    //println(s"the total arrival ${trafficData.mu}")
//    def createSources(model: Model, car: () => Vehicle): Array[VSource] =
//        val mainPos = mainlineCoords(0)
//        val centerPos = ((mainPos._1 + 100.0).toInt, (mainPos._2 + 100.0).toInt)
//        val offsets = Array(
//            (0, 0),
//            ((rampCoords(0)._1 + 230.0).toInt - centerPos._1, (rampCoords(0)._2 - 300.0).toInt - centerPos._2),
//            ((rampCoords(1)._1 + 230.0).toInt - centerPos._1, (rampCoords(1)._2 - 300.0).toInt - centerPos._2),
//            ((rampCoords(2)._1 + 230.0).toInt - centerPos._1, (rampCoords(2)._2 - 300.0).toInt - centerPos._2)
//        )
//
//        VSource.group(model, car, centerPos,
//            (srcNames(0), 0, Erlang(), nStopArray(0), offsets(0)),
//            (srcNames(1), 1, Erlang(), nStopArray(1), offsets(1)),
//            (srcNames(2), 2, Erlang(), nStopArray(2), offsets(2)),
//            (srcNames(3), 3, Erlang(), nStopArray(3), offsets(3))
//        ).toArray
//    end createSources
//
//def createSinks(): Array[Sink] =
//    sinkNames.indices.map { i =>
//        val pos = if i == 0 then
//            val (x, y) = mainlineCoords.last
//            ((x - 100.0).toInt, (y - 100.0).toInt)
//        else
//            val (x, y) = rampCoords(3)
//            ((x + 230.0).toInt, (y - 300.0).toInt)
//        Sink(sinkNames(i), pos)
//    }.toArray
//end createSinks
//
//def createRamps(sources: Array[VSource], rampJunctions: Array[Junction],
//                sinks: Array[Sink], motion: Dynamics): Array[Ramp] =
//    println(s"Creating ramps using Ramp.group")
//    Ramp.group(motion,
//        ("onRamp1", sources(1), rampJunctions(0), RampMode.On, 0.15, 30.0),
//        ("onRamp2", sources(2), rampJunctions(1), RampMode.On, 0.15, 30.0),
//        ("onRamp3", sources(3), rampJunctions(2), RampMode.On, 0.15, 30.0),
//        ("offRamp", rampJunctions(3), sinks(1), RampMode.Off, 0.15, 30.0))
//end createRamps