package scalation
package simulation
package process


import scalation.mathstat.*
import scalation.random.*
import scalation.simulation.process.example_1.Roadcood


class TrafficConfig(fileName: String, rowTime: Double, stream: Int = 0):

    private[process] val ew = new EasyWriter("recorder", "TrafficConfigText.txt")

    val t1: Int = 2 * 96 + 16
    val t2: Int = 3 * 96



//    val t1 = 0
//    val t2 = 1

    private val rowOffset = t1

    private val allGpsCoords   = getRoadCoordinates((1500, 1500))
    private val mainlineCoords = allGpsCoords("mainline")
    private val rampCoords     = allGpsCoords("ramps")

    private[process] val laneIdx = VectorI(4, 7, 10, 13, 16) // the flow index in the csv data
    private [process] val ramplaneIdx  = VectorI(1)

    // mainline
    private[process] val data                       = MatrixD.load(fileName, t1, t2)  // load the data from csv
    private[process] val arrivalCount               = data(?, laneIdx) // select only the relevant columns
    private [process] val totalArrivalsPerRow       = arrivalCount.sumVr    // sum the per lane count to get total count per row
    private[process] val muMain                     = totalArrivalsPerRow.map(rowTime / _)   // get the mu for each row
    private[process] val laneProbPerRow             = arrivalCount.mmap(_.toProbability)
    private[process] val laneRVPerRow               = Vector.tabulate(totalArrivalsPerRow.dim)(r => Discrete(laneProbPerRow(r)))


    // ramps
    private val onRampIds            = Array("410093OR", "410095OR")
    private val onRampData           = onRampIds.map(id => MatrixD.load(s"Ramp_VDS_/$id.csv", t1, t2))   // load the data from csv for each ramp
    private val onRampTotalsPerRow   = onRampData.map(id => id(?, ramplaneIdx).sumVr) // get the total count per row for each ramp
    private val muRamps              = onRampTotalsPerRow.map(_.map(rowTime / _))  // get the mu for each ramp, per row


    // :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Evaluation ground truth (fixed 3 sensors)
    val evalMainIds = Array("401834ML", "401833ML", "401929ML")
    val evalArrivalsPerRow: Array[VectorD] = evalMainIds.map { id =>
        MatrixD.load(s"Mainline_VDS_Redwood_Creek_US101-N/$id.csv", t1, t2)(?, ramplaneIdx).sumVr
    }

    lazy val sensor1Total: Int = totalArrivalsPerRow.sum.toInt
    lazy val onramp1Total: Int = onRampTotalsPerRow(0).sum.toInt
    lazy val onramp2Total: Int = onRampTotalsPerRow(1).sum.toInt

    //val nStopArray: Array[Int] = Array(sensor1Total, onramp1Total, onramp2Total)

    val nStopArray: Array[Int] = Array(20,20,20)

    val muPerSource: Array[VectorD] = Array(muMain) ++ muRamps

    def getMuForSource(i: Int): VectorD = muPerSource(i)
    @inline def getLaneRV(row: Int): Discrete = laneRVPerRow(row)


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
        val data = scala.io.Source.fromFile(path).getLines.toArray
        val gps = data.map { line =>
            val Array(lat, long) = line.split(",").map(_.toDouble)
            (lat, long)
        }
        val coords = new scalation.Coordinates(w_h._1, w_h._2, gps)
        coords.calcAniCoords()
        coords.aniCoords
    end getJunctions

    def getMainlineCoordinates(dims: (Double, Double)): Array[(Double, Double)] = mainlineCoords

    // Old: applied an extra (sx, sy) shift to ramps here
     def getRampCoordinates(dims: (Double, Double)): Array[(Double, Double)] =
         val (sx, sy) = (65.0, -70.0)
         rampCoords.map { case (x, y) => (x + sx, y + sy) }
     end getRampCoordinates


    def getSensorCoordinates(dims: (Double, Double)): Array[(Double, Double)] = mainlineCoords

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
    val stream  = 0
    val file    = "/Mainline_VDS_Redwood_Creek_US101-N/404532ML.csv"
    val cfg     = new TrafficConfig(file, rowTime, stream)

    println(s"@@@@@@file: $file  rows: [${cfg.t1}, ${cfg.t2})  laneIdx: ${cfg.laneIdx}")

    // Mainline
    println("arrivalCount:")
    println(cfg.arrivalCount)
    println(s"totalArrivalsPerRow: ${cfg.totalArrivalsPerRow}")
    println(s"muMain:               ${cfg.muMain}")
    println("laneProbPerRow:")
    (0 until cfg.totalArrivalsPerRow.dim).foreach { r =>
        println(s"  row $r -> ${cfg.laneProbPerRow(r)}")
    }

    // Ramp aggregates
    println(s"sensor1Total: ${cfg.sensor1Total}")
    println(s"onramp1Total: ${cfg.onramp1Total}")
    println(s"onramp2Total: ${cfg.onramp2Total}")
    println(s"nStopArray:   ${cfg.nStopArray.mkString("[", ", ", "]")}")

    // Mu per source: index 0 = mainline, subsequent = ramps
    println("muPerSource:")

    cfg.muPerSource.zipWithIndex.foreach { case (v, i) =>
        println(s"  mu[$i]: $v")
    }

    // Optional spot-check via public API
    println(s"getMuForSource(0): ${cfg.getMuForSource(0)}")
    if cfg.muPerSource.length > 1 then println(s"getMuForSource(1): ${cfg.getMuForSource(1)}")
