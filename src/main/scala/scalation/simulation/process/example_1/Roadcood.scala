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


    /**
     * GPS Coordinates for PEMS Sensors and Operational Merge Points
     *
     * PEMS Mainline Sensors (Data Collection/Comparison Points):
     *   - sensor1: VDS 404531 (Entry point, 5 lanes)
     *   - sensor2: VDS 404532 (Before offramp, 5 lanes)
     *   - sensor3: VDS 401834 (After onramp1, 5 lanes)
     *   - sensor4: VDS 401833 (After onramp2, 5 lanes)
     *   - sensor5: VDS 401929 (Exit point, 5 lanes)
     *
     * Operational Merge Points (No PEMS Comparison):
     *   - offR_merge: Offramp diverge zone (vehicles exit to offramp)
     *   - onR_merge1: Onramp1 convergence zone (vehicles enter from onramp1, spread across lanes)
     *   - onR_merge2: Onramp2 convergence zone (vehicles enter from onramp2, spread across lanes)
     *
     * Physical Layout (CORRECT ORDER):
     * sensor1 ──► sensor2 ──► offR_merge ──► sensor3 ──► onR_merge1 ──► sensor4 ──► onR_merge2 ──► sensor5
     *                            │                          │                           │
     *                         offramp                    onramp1                     onramp2
     *
     * Junction Indices (for junc array):
     *   junc(0) = sensor1       // PEMS comparison
     *   junc(1) = sensor2       // PEMS comparison
     *   junc(2) = offR_merge    // Operational only (offramp diverge)
     *   junc(3) = sensor3       // PEMS comparison
     *   junc(4) = onR_merge1    // Operational only (onramp1 convergence + lane spread)
     *   junc(5) = sensor4       // PEMS comparison
     *   junc(6) = onR_merge2    // Operational only (onramp2 convergence + lane spread)
     *   junc(7) = sensor5       // PEMS comparison
     */
    val latlong = Map(
        // ─── PEMS Mainline Sensors (5 total) ───
        "sensor1" -> (37.491812, -122.219838),                    // VDS 404531 - Entry (S of Maple St OC)
        "sensor2" -> (37.493958158096504, -122.22470162714667),   // VDS 404532 - Before offramp (N of Redwood Creek, 5 lanes)
        "sensor3" -> (37.49611371865748, -122.23194509331292),   // VDS 401834 - After onramp1 (oppo Holly St, 5 lanes)
        //"sensor3" -> (37.49660297971446, -122.23361567714998),    // VDS 401834 - After onramp1 (oppo Holly St, 5 lanes)
        "sensor4" -> (37.50377824134259, -122.24447811368339),   // VDS 401833 - After onramp2 (oppo Holly St, 5 lanes)
        //"sensor4" -> (37.50798673265363, -122.24925380485341),    // VDS 401833 - After onramp2 (PM 7.34)
        "sensor5" -> (37.510000, -122.252000),                    // VDS 401929 - Exit (PM 7.68)

        // ─── Operational Merge Points (3 total - ADJUSTED TO MATCH PHYSICAL REALITY) ───
        "offR_merge" -> (37.49547, -122.22961),                   // Offramp diverge zone (between sensor2 and sensor3)
        "onR_merge1" -> (37.499945980000035, -122.238211603498155), // Onramp1 convergence - MIDPOINT between sensor3 and sensor4
        "onR_merge2" -> (37.50533368033694, -122.24635858526254), // Onramp2 convergence - 25% from sensor4 toward sensor5 (close to sensor4)

        // ─── Ramp Entry/Exit Points (ALIGNED WITH MERGE POINTS) ───
        "offramp"  -> (37.49547, -122.22961),                     // Offramp VDS 410094 - matches offR_merge
        "onramp1"  -> (37.499945980000035, -122.238211603498155), // Onramp1 VDS 410095 - matches onR_merge1 (midpoint sensor3-sensor4)
        "onramp2"  -> (37.50533368033694, -122.24635858526254)    // Onramp2 VDS 410093 - matches onR_merge2 (25% from sensor4 to sensor5)
    )
    /**
     * Junction Array Indices Mapping (CORRECT ORDER):
     *
     * junc(0) = sensor1      ← PEMS sensor (compare with VDS 404531)
     * junc(1) = sensor2      ← PEMS sensor (compare with VDS 404532)
     * junc(2) = offR_merge   ← Operational merge point (offramp diverge, no PEMS comparison)
     * junc(3) = sensor3      ← PEMS sensor (compare with VDS 401834)
     * junc(4) = onR_merge1   ← Operational merge point (onramp1 convergence + lane spread, no PEMS comparison)
     * junc(5) = sensor4      ← PEMS sensor (compare with VDS 401833)
     * junc(6) = onR_merge2   ← Operational merge point (onramp2 convergence + lane spread, no PEMS comparison)
     * junc(7) = sensor5      ← PEMS sensor (compare with VDS 401929)
     *
     * Total: 8 junctions (5 PEMS sensors + 3 operational merge points)
     *
     * PEMS Comparison Indices: 0, 1, 3, 5, 7 (sensors 1-5)
     * Operational Only: 2, 4, 6 (offramp diverge, onramp1 convergence, onramp2 convergence)
     */


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