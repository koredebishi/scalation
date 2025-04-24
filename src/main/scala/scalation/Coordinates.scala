
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Fri 31 May 2024 12:54:22 PM EDT
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Coordinates maps Lat-Long pairs to xy pairs in an animation environment
 */

package scalation
package simulation
package process
package modeling
package clustering


import scalation.mathstat.*
import scalation.LatLong2UTM.*

//import scalation.modeling.clustering.KMeansClusterer
//import scalation.modeling.clustering.Clusterer

import scala.collection.mutable.ListBuffer


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

/** The `Coordinates` class calculates the animation coordinates for a set of
 * provided lat-long coordinates.  Uses CTM (Custom Transverse Mercator)
 * coordinates as an intermediate step.
 *
 * @param aniWidth  the width of the animation window
 * @param aniHeight the height of the animation window
 * @param coords    an array of lat-long coordinates
 */
class Coordinates(aniWidth: Double, aniHeight: Double, coords: Array[(Double, Double)]):


    // For general case, calculate new central meridian based on coords and create
    //   a custom transverse mercator projection so that all the points are in the same
    //   zone.  The northing values should be unaffected.  New easting values should
    //   all make sense together.
    //
    //   In LatLong, in setVariables, "var2" is the value of the central meridian.
    //                                I believe that setting this value to a custom
    //                                central meridian will yield the custom UTM-type
    //                                zone that is needed.  Everything else would stay
    //                                the same.


    val cent = getCentral(coords)

    //    println (s"midlong = $midLong")

    val xt = 20.0 // x and y margins
    val yt = 20.0

    val aw = aniWidth - 2.0 * xt // animation width  (adjusted for margins)
    val ah = aniHeight - 2.0 * yt // animation height (adjusted for margins)

    val CTMCoords = getCtmCoords() // ListBuffer of UTM coordinates based on the lat-long coordinates

    //println (UTMCoords)

    val nsew = findNSEW() // nsew is the bounding box for the UTM coordinates

    val o = calcAniCoords()

    val aniCoords = o._1
    val scale = o._2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Calculate the central meridian for the CTM by taking the average longitude
     * of all latlongs in the set.
     */
    def getCentral(c: Array[(Double, Double)]): Double =

        var sum = 0.0
        for l <- c do
            sum += l._2
        end for

        sum / c.length

    end getCentral

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Calculate the animation coordinates that will represent the lat-long
     * coordinates provided to the class.
     */
    def calcAniCoords(): (Array[(Double, Double)], Double) =

        val aniCoords = ListBuffer[(Double, Double)]()

        val s = nsew(1);
        val w = nsew(3)
        val ctmT = for c <- CTMCoords yield (c._1 - w, c._2 - s)

        val ch = nsew(5) // CTM bounding box height
        val cw = nsew(4) // CTM bounding box width

        val aniAspect = ah / aw
        val ctmAspect = ch / cw
        val ctmAspRec = cw / ch

        var scale = 0.0

        if aniAspect <= ctmAspect then scale = ah / ch
        else scale = aw / cw

        for c <- ctmT do
            val nx = c._1 * scale + xt
            val ny = c._2 * -scale + ah + yt

            val np = (nx, ny)
            aniCoords += np
        end for

        (aniCoords.toArray, scale)
    end calcAniCoords

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Convert Latitude-Longitude coordinates to CTM coordinates using the
     * coords parameter of the class and output as a ListBuffer.
     */
    def getCtmCoords(): ListBuffer[(Double, Double)] =
        val c = ListBuffer[(Double, Double)]()
        for (cc <- coords) do
            val ll = new LatLong(cc)
            //c += latLong2CTMxy (ll, cent)
            c += LatLong2CTM.latLong2CTMxy(ll, cent)
        end for
        c
    end getCtmCoords

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Find the northern-most, southern-most, eastern-most, and western-most
     * CTM x-values and y-values in the set of provided coordinates.
     * NOTE:  This does not necessarily correspond to actual complete coordinates
     * in the set.  These values provide a bounding box around the set.
     */
    def findNSEW(): Array[Double] =
        val xs = for (c <- CTMCoords) yield c._1
        val ys = for (c <- CTMCoords) yield c._2
        val ysmax = ys.max;
        val ysmin = ys.min;
        val xsmax = xs.max;
        val xsmin = xs.min;
        Array(ysmax, ysmin, xsmax, xsmin, xsmax - xsmin, ysmax - ysmin)
    end findNSEW

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Utility function for calculating the distance between two (Double, Double)
     * values
     *
     * @param p the first (Double, Double)
     * @param q the second (Double, Double)
     */
    def dist(p: (Double, Double), q: (Double, Double)): Double =
        Math.sqrt((p._1 - q._1) * (p._1 - q._1) + (p._2 - q._2) * (p._2 - q._2))
    end dist

end Coordinates

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test main for Coordinates2 class.  The lat-longs are collected coordinates
 * around the Loop 10 highway in Athens, GA, USA.
 */
@main def CoordinatesTest(): Unit =

    val coords = Array((33.91634138069811, -83.4051542337287),
        (33.933357972261106, -83.36724178356303),
        (33.94675707615584, -83.35608200084363),
        (33.97805332627655, -83.35240177319633),
        (33.977939520362476, -83.37819359017722),
        (33.971885410013265, -83.4120232202679),
        (33.96427038393156, -83.42809200638573),
        (33.93966752188796, -83.46202941910865),
        (33.91661409923615, -83.45959899962827),
        (33.91279189753823, -83.45093951967232),
        (33.92110134424739, -83.37806326666644))

    val c = new Coordinates(840.0, 540.0, coords)

    val scale = c.scale

    val ac = c.aniCoords

    val lls = for cc <- coords yield new LatLong(cc)

    println(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    println("Distance computed using Lat-Long coordinates")

    for i <- 0 until lls.length - 1 do println(lls(i).distance(lls(i + 1)))

    println(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    println("Distance computed using Universal Transverse Mercator Projection")

    val utms = for ll <- lls yield latLong2UTMxy(ll)

    for i <- 0 until utms.length - 1 do println(c.dist(utms(i), utms(i + 1)))


    println(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    println("Distance computed using Custom Transverse Mercator Projection")

    for i <- 0 until ac.length - 1 do println(c.dist(ac(i), ac(i + 1)) / scale)

    println(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    println("Distance computed using UTM Projection at https://www.zonums.com/online/coords/cotrans.php?module=13")

    val utms2 = Array((277646.872, 3755485.132),
        (281196.135, 3757290.972),
        (282261.993, 3758753.348),
        (282681.836, 3762216.691),
        (280298.344, 3762259.084),
        (277156.604, 3761660.666),
        (275651.685, 3760851.112),
        (272449.715, 3758197.084),
        (272613.092, 3755634.710),
        (273403.711, 3755191.612),
        (280164.179, 3755954.693))

    for i <- 0 until utms2.length - 1 do println(c.dist(utms2(i), utms2(i + 1)))

end CoordinatesTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test main for Coordinates2 class.  The lat-longs are collected coordinates
 *  around the Loop 10 highway in Athens, GA, USA.
 */
@main def CoordinatesTest2 (): Unit =

    val c = new Coordinates (840.0, 540.0, Array ((0.0,  -90.0),
                                                  (10.0, -90.0),
                                                  (20.0, -90.0),
                                                  (30.0, -90.0),
                                                  (40.0, -90.0),
                                                  (0.0,  -84.0001),
                                                  (10.0, -84.0001),
                                                  (20.0, -84.0001),
                                                  (30.0, -84.0001),
                                                  (40.0, -84.0001)))

    for co <- c.CTMCoords do println (co)

//    println (c.aniCoords)    
//    println (s"scale = ${c.scale}")


end CoordinatesTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test main for Coordinates2 class.  The lat-longs are collected coordinates
 *  around the Loop 10 highway in Athens, GA, USA.
 */
@main def CoordinatesTest3 (): Unit =

    val c = new Coordinates (840.0, 540.0, Array ((33.91634138069811,  -83.4051542337287),
      (33.933357972261106, -83.36724178356303)))

    for co <- c.CTMCoords do println (co)

  //    println (c.aniCoords)
  //    println (s"scale = ${c.scale}")


end CoordinatesTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test main for Coordinates2 class.  The lat-longs are collected coordinates
 *  around the 101N highway in San Fransisco , CA, USA
 401474, 404533, 425697(After the No data sensor:419452No data) ,402380, 402376, 400388, 402379, 400981, 401653
 419452No data, 405673, 400859, 401927, 401872, 404534, 402398, 404529
 422116, 401874, 402377
 */

//import Clusterer.test

@main def CoordinatesTest4 (): Unit =

    val c = new Coordinates(840.0, 540.0, Array((37.43594500, -122.109246),    //402376   .1
      (37.44039300, -122.113925),    //402377   .2
      (37.44596100, -122.11976),     //402379   .3
      (37.45029500, -122.124287),    //425697   .7
      (37.45144200, -122.125642),    //402380   .4
      (37.45328000, -122.12842),     //404529   .5
      (37.45646700, -122.133857)     //422116   .6
    ))
    for co <- c.CTMCoords do banner (co.toString())

    val coSize = c.aniCoords.size

    val listSize = c.aniCoords.size      //get the size of my array list

    //Initialize empty bucket x, y of Vector with the same size as aniCooords: listSize
    val x = new VectorD(listSize)
    val y = new VectorD(listSize)

    //Use a for loop of the bucket size (listSize) to move items into the empty vector x, y created above
    for i <- 0 until listSize do
      x(i) = c.aniCoords(i)._1
      y(i) = 550 - c.aniCoords(i)._2
    println(s"x:$x: \n y:$y")

    new Plot(x, y, null, "latlong" , lines = true)  // Plot the graph of the points

    val matrixSensor = new MatrixD(listSize, 2)
    for j <- matrixSensor.indices do matrixSensor(j) = VectorD(x(j), y(j))

    println(s"lat and long $matrixSensor")
    println (s"scale = ${c.scale}")


end CoordinatesTest4

//pass 7 sensors coordinates
//store as VectorD (x,y)
//

//To do list
//1. Locations of the sensors inside the animation
//2. Sensors will be at the junction // plotting will have the coordinate of the animation
//3. Getting the lat/log should work with this code and the aniCord is just the list of the order in which you input them
//Give the junction as the location inside the loctions
//Should show up inside the animation asap
//Scale factors is important
//Car following model needs the real distances which // get the diff from animation coordinate and scale it up.

//<<--- Starting point --->
//Plot the points on graphs and see if it's diff from the PEMS coordinate locations.
//make 2 vectors X and Y vectors and go through the buffer


//import scalation.random.{Bernoulli, Normal}
//
//    val c = new Coordinates (840.0, 540.0, Array (( 37.47483,   -122.165147),   // 1
//                                                    (37.4652, -122.148841),
//                                                    (37.450295, -122.124287),
//                                                    (37.451442, -122.125642),
//                                                    (37.435945, -122.109246),
//                                                    (37.480426, -122.174688),
//                                                    (37.445961, -122.11976),
//                                                    (37.468732, -122.154696),
//                                                    (37.483811, -122.181671),
//                                                    (37.486916, -122.200187),
//                                                    (37.463019, -122.145022),
//                                                    (37.483144, -122.17985),
//                                                    (37.484232, -122.183031),
//                                                    (37.472754, -122.161627),
//                                                    (37.46959,  -122.156168),
//                                                    (37.45328,  -122.12842),
//                                                    (37.456467, -122.133857),
//                                                    (37.487321, -122.203433),
//                                                    (37.440393, -122.113925)
//                                                            ))
