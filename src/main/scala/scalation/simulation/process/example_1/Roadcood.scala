package scalation
package simulation
package process
package example_1


import scalation.mathstat.{MatrixD, VectorD, Plot}

object Roadcood:

    // we are assuming that the mergepoint for onramp1 is same as that of sensor2 and onramp2 is same as sensor3
    // sensor1 to sensor5 are mainline sensors
    // offramp merge point is different and is given as offrampmerge, since it's its coodinate is somewhat between sensor1 and sensor2
//    val latlong = Map (
//    "sensor1" -> (37.491812, -122.219838),   NEW
//    "sensor1" -> (37.493958158096504, -122.22470162714667),            //Mainline VDS 404532 N of Redwood Creek 5 lanes @ US101-N CA PM=6.2 (408.39)
//     "sensor2" -> (37.49547, -122.22961),                       // Off-ramp merge point between sensor1 and sensor2   // created to make offramp merge point
//    "sensor3" -> (37.49611371865748, -122.23194509331292),             //Mainline VDS 401834 oppo Holly St rm-s-coll2 lanes @ US101-N CA PM=7.02 (409.21)
//    "sensor4" -> (37.49660297971446, -122.23361567714998),         //Mainline VDS 401833 oppo Holly St rm-s-coll3 lanes @ US101-N CA PM=7.34 (409.53)
//    "sensor5" -> (37.50377824134259, -122.24447811368339),         //Mainline VDS 401929 oppo Holly St rm-s-coll1 lanes @ US101-N CA PM=7.68 (409.87)
//     "sensor6" -> (37.50798673265363, -122.24925380485341),          //Mainline VDS 401652 oppo Holly St rm-s-coll4 lanes @ US101-N CA PM=7.84 (410.03)
//    "offramp"   -> (37.49547, -122.22961),                                                  //(37.496371200804205, -122.2315368954958),          //Off ramp 410094
//    "onramp1"  -> (37.496442108229296, -122.23190311039164),            //On ramp 410095
//    "onramp2"  -> (37.496820213311665, -122.233427547066)              //On ramp 410093
//    )


    // need to rename these sensors points since we are adding some merge points as sensors too.
    // Sensor1 , Sensor2, Sensor3, Sensor4, Sensor5 // Pems mainline sensors
    // mergepoints: merge1 (offramp), merge2 (onramp1), merge3 (onramp2)
//    val latlong = Map(
//        "sensor1" -> (37.491812, -122.219838),                  // Mainline VDS 404531 - S of Maple st OC
//        "sensor2" -> (37.493958158096504, -122.22470162714667), //Mainline VDS 404532 N of Redwood Creek 5 lanes @ US101-N CA PM=6.2 (408.39)
//        "sensor3" -> (37.49547, -122.22961), // Off-ramp merge point between sensor1 and sensor2   // created to make offramp merge point
//        "sensor4" -> (37.49611371865748, -122.23194509331292), //Mainline VDS 401834 oppo Holly St rm-s-coll2 lanes @ US101-N CA PM=7.02 (409.21)
//        "sensor5" -> (37.49660297971446, -122.23361567714998), //Mainline VDS 401833 oppo Holly St rm-s-coll3 lanes @ US101-N CA PM=7.34 (409.53)
//        "sensor6" -> (37.50377824134259, -122.24447811368339), //Mainline VDS 401929 oppo Holly St rm-s-coll1 lanes @ US101-N CA PM=7.68 (409.87)
//        "offramp" -> (37.49547, -122.22961), //(37.496371200804205, -122.2315368954958),          //Off ramp 410094
//        "onramp1" -> (37.496442108229296, -122.23190311039164), //On ramp 410095
//        "onramp2" -> (37.496820213311665, -122.233427547066) //On ramp 410093
//    )
    // TCount         Tcount             X                      OR_M                       TCount             OR_M               TCount          TCount
    // sensor1--------sensor2---------merge1(offramp)-------------------------------------sensor3--------------------------------sensor4---------sensor5
    // sensor1--------sensor2---------merge1(offramp)-----------merge2(onramp1)-----LC_M--sensor3----------merge3(onramp2)-------sensor4---------sensor5
    //
        val latlong = Map(
            "sensor1" -> (37.491812, -122.219838),                  // Mainline VDS 404531 - S of Maple st OC
            "sensor2" -> (37.493958158096504, -122.22470162714667), //Mainline VDS 404532 N of Redwood Creek 5 lanes @ US101-N CA PM=6.2 (408.39)
            "offR_marge" -> (37.49547, -122.22961), // Off-ramp merge point between sensor1 and sensor2   // created to make offramp merge point
            "onR_marge" -> (37.49611371865748, -122.23194509331292),// on ramp merge point between sensor2 and sensor3
            "sensor3" -> (37.49660297971446, -122.23361567714998), //Mainline VDS 401834 oppo Holly St rm-s-coll2 lanes @ US101-N CA PM=7.02 (409.21)
            "onR_marge" -> (37.50377824134259, -122.24447811368339), // on ramp merge point between sensor3 and sensor4
            "sensor4" -> (37.50798673265363, -122.24925380485341),          //Mainline VDS 401833 oppo Holly St rm-s-coll3 lanes @ US101-N CA PM=7.34 (409.53)
            "sensor5" -> (37.510000, -122.252000),   // Mainline VDS 401929 oppo Holly St rm-s-coll1 lanes @ US101-N CA PM=7.68 (409.87)
            "offramp" -> (37.49547, -122.22961), //(37.496371200804205, -122.2315368954958),          //Off ramp 410094
            "onramp1" -> (37.496442108229296, -122.23190311039164), //On ramp 410095
            "onramp2" -> (37.496820213311665, -122.233427547066) //On ramp 410093
        )
    // sensor1 point: @ 0
    // sensor2 point: @ 1
    // offramp point: same @ 2
    // onramp1 point: now @ 3
    // onramp2 point: now @ 5
    // sensor3 point: now @4
    // sensor4 point: now @6
    // sensor5 point: now @7
end Roadcood


@main def RoadCoodTest(): Unit =

    for (k, v) <- Roadcood.latlong do println(s"the key is $k and value is $v")

    val size = Roadcood.latlong.size

    // Convert Map to Array of tuples for the Coordinates class
    val coordsArray = Roadcood.latlong.values.toArray
    val keys = Roadcood.latlong.keys.toArray

    // Set animation window dimensions
    val aniWidth = 1200.0
    val aniHeight = 800.0

    // Use the Coordinates class to convert lat/long to screen coordinates
    val coordinates = new scalation.Coordinates(aniWidth, aniHeight, coordsArray)
    val screenCoords = coordinates.aniCoords
    val scale = coordinates.scale

    println(s"Scale factor used by Coordinates class: $scale")

    // Extract x and y coordinates for plotting
    val x = new VectorD(size)
    val y = new VectorD(size)

    for i <- screenCoords.indices do
        x(i) = screenCoords(i)._1  // x-coordinate (screen)
        y(i) = 800 - screenCoords(i)._2  // y-coordinate (screen)
    end for

    println(s"Screen x coordinates: $x")
    println(s"Screen y coordinates: $y")

    // Print the mapping between keys and screen coordinates
    println("\nMapping from GPS locations to screen coordinates:")
    for i <- keys.indices do
        val (lat, lon) = coordsArray(i)
        val (screenX, screenY) = screenCoords(i)
        println(s"${keys(i)}: GPS($lat, $lon) -> Screen($screenX, $screenY)")
    end for

    // Create matrix for further processing if needed
    val matrixSensor = new MatrixD(size, 2)
    for j <- matrixSensor.indices do matrixSensor(j) = VectorD(x(j), y(j))

    // Plot the screen coordinates
    new Plot(x, y, null, "GPS Coordinates Converted to Screen Coordinates", lines = false)


end RoadCoodTest