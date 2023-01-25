
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John A. Miller, Casey Bowman, Yulong W.
 *  @version 2.0
 *  @date    Wed Feb 16 11:34:46 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 */
package scalation
package simulation.agent

import java.awt.geom.Point2D
import scala.math.{sqrt,abs}
import scala.collection.mutable.ArrayBuffer
import scalation.mathstat
import scalation.mathstat.VectorD
import scalation.simulation.agent.TrafficJunction1011.getMinMax


val timeConv = 900 //number of seconds of 15 min
val numInterval = 50 //5:45 am  to 6:15 pm: 50. 15 minutes time units
val topLeft  = new Point2D.Double(100.0, 100.0) //the top-left node for the sink of a traffic map
val botRight = new Point2D.Double(1500.0, 1500.0) //
val distScreen = sqrt((topLeft.x-botRight.x)~^2 + (topLeft.y-botRight.y)~^2)
val minmaxGPS = getMinMax()
val maxLat = minmaxGPS(1)
val maxLong = minmaxGPS(3)
val distGPS = sqrt((minmaxGPS(1)-minmaxGPS(0))~^2 + (minmaxGPS(3) -minmaxGPS(2))~^2)
val scaleRatio = distScreen/distGPS
val latratio = (1481.54395013872 - 10) / abs(37.097388 - 37.112261) //Casey's setup ratio
val longratio = (1696.6797419758 - 10) / abs(-121.621044 + 121.60395) //Casey's setup ratio
val avgratio = (longratio+latratio)/2
val CaseyRatio = sqrt(latratio*longratio) //Casey ratio 2D
val rRatio =CaseyRatio/scaleRatio


enum Optimizers:
  case SPSAAlg, NelderMeade, GeneticAlg, NoOpt

enum Carfollowing:
  case IDM, Gipps

enum Arrivalmodel:
  case OFFLINE, RATIO, DIFF, Sarima


object TrafficSetupInfo:
    //this obj contains the Casey's original setup info. just for reference
    val sourceTrainfile = "409880.csv"
    val sourceGPSx = 37.097388 //Lat, it was sources.line(0).words(2) in Casey code
    val sourceGPSy = -121.60395  //Long, it was sources.line(0).words(3) in Casey code
    val sourceAnimX = 1696.6797419758 // coordinates  it was sources.line(0).words(4) in Casey code
    val sourceAnimY = 1481.54395013872 //north bound, south bottom. origin top left. .. sink ->(10,10) work backwards.
    val sourceOnRampX = null // x coordinates along the road for OnRamp
    // conversion is determined by the scalation 1.7 longlat.scala with scale factor. 800 unit animation in pixels
    // the distance is 800 in the system correspond to 800 meters. car drives in m/s
    val sinkTrainfile = "402328.csv"
    val sinkGPS = (37.112261, -121.621044)
    val sinkCoordts = (10.0, 10.0)
    val sinkextra = null
    //some nodes  not working for validating but all  for constructing denoted by Usable4Valid.
    /*
    val JuncInfo = Array(
                      Map("FileName" -> "409880.csv","GPS" ->(37.097388, -121.60395), "No." -> 0,
                           "Coordts" -> (1696.6797419758, 1481.54395013872), "Usable4Valid" -> true),
                      Map("FileName" ->"402327.csv","GPS" -> (37.103108, -121.60944), "No." -> 1,
                          "Coordts" -> (1139.25502986443, 900.766290162406), "Usable4Valid" -> false ),
                      Map("FileName" ->"409877.csv", "GPS" -> (37.107061, -121.614493), "No." -> 2,
                          "Coordts" ->  (650.379227928203, 518.315064146322), "Usable4Valid" -> true ),
                      Map("FileName" ->"402328.csv", "GPS" -> (37.112261, -121.621044), "No." -> 3,
                          "Coordts" -> (10.0, 10.0), "Usable4Valid" -> false  )
                       )*/
end TrafficSetupInfo

def distance(x1:Double, y1:Double, x2:Double, y2:Double):Double = sqrt((x2-x1)~^2+(y2-y1)~^2)

object TrafficJunction1011:

    //the data is under road_structure 101 section the 1st folder so name with 1011: data/models/traffic/road_structure/101/1/
    // 1. one csv is one sensor throu one year for source.
    // 2. in total 4 sensors are used. 402327 have the bad data which is why is set false; only two sensors
    //    to check compare sim with real data.

    val JuncInfo = Array( //FileName, GPS, eligible for Validation
        ( "409880.csv", (37.097388, -121.60395),  true), //source
        ("402327.csv",  (37.103108, -121.60944),  false), //jun1
        ("409877.csv",  (37.107061, -121.614493), true), //jun2
        ("402328.csv",  (37.112261, -121.621044), false)  //sink
    )//north west sphere: +, _

    val juncGPSLat  = for junc <- JuncInfo yield junc._2._1.asInstanceOf[ValueType]
    val juncGPSLong = for junc <- JuncInfo yield junc._2._2.asInstanceOf[ValueType]

    val junGPSLongIndirectOrd = (new MergeSortIndirect (juncGPSLong)()).isort ()

    val latLongS = for i <-JuncInfo.indices yield new LatLong(JuncInfo(i)._2)

    val dist0_1 = latLongS(0).distance(latLongS(1))
    val dist1_2 = latLongS(1).distance(latLongS(2))
    val dist2_3 = latLongS(2).distance(latLongS(3))

    //2238.376677 pixel dist (1696.6797419758, 1481.54395013872)- (10.0, 10.0)
    //0.022659    gps dist (37.097388, -121.60395) (37.112261, -121.621044)
    //ratio is   98785.324904


    def getMinMax():Array[Double] =
        val arrLat  = for junc <- juncGPSLat  yield junc.toDouble
        val arrLong = for junc <- juncGPSLong yield junc.toDouble
        Array(arrLat.min,arrLat.max,arrLong.min,arrLong.max)
    end getMinMax

    def convertCoordinate() :(Array[Double],Array[Double])   =

        val junX = Array.ofDim[Double](junGPSLongIndirectOrd.length)
        val junY = Array.ofDim[Double](junGPSLongIndirectOrd.length)
        for i <- junGPSLongIndirectOrd do
            junX(i) = abs(juncGPSLong(i).toDouble - maxLong) * scaleRatio + topLeft.x
            junY(i) = abs(juncGPSLat(i).toDouble - maxLat) * scaleRatio + topLeft.y
        (junX.reverse,junY)
    end convertCoordinate


end TrafficJunction1011

/*
@main def testJunc():Unit = for dict <- TrafficJunction1011.JuncInfo do for (k, v) <- dict  do println(s"key: $k, value: $v")

@main def testJuncLen():Unit = println(TrafficJunction1011.JuncInfo.length)

@main def testJuncYield():Unit =
    val res1 = for dict <- TrafficJunction1011.JuncInfo yield dict("FileName").toString
    val res2 = for dict <- TrafficJunction1011.JuncInfo yield dict("Coordts").asInstanceOf[(Double,Double)]
    println(res1.isInstanceOf[Array[String]]) //without casting, it would be type matchable
    println(res2.isInstanceOf[Array[(Double,Double)]])
end testJuncYield

@main def testJunc4valid():Unit =
    val res = for dict <- TrafficJunction1011.JuncInfo if dict("Usable4Valid").asInstanceOf[Boolean] yield dict("FileName").toString
    println(res.isInstanceOf[Array[String]])
    for thing <- res do println(thing)
end testJunc4valid

*/
