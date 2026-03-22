package scalation
package simulation
package process
package example_1


object Roadcood2:
    /**
     * GPS Coordinates for PEMS Sensors and Operational Merge Points
     * Donald D. Doyle Hwy, Danville, California
     *
     * PEMS Mainline Sensors (Data Collection/Comparison Points):
     *   - sensor1: VDS 401112 (Entry point, 4 lanes)
     *   - sensor2: VDS 401104 (4 lanes)
     *   - sensor3: VDS 400712 (4 lanes)
     *   - sensor4: VDS 400450 (4 lanes)
     *   - sensor5: VDS 407463 (4 lanes)
     *
     * Operational Merge Points (No PEMS Comparison):
     *   - onR_merge1: Onramp1 convergence zone (between sensor1 and sensor2)
     *   - onR_merge2: Onramp2 convergence zone (between sensor3 and sensor4)
     *
     * Physical Layout (CORRECT ORDER):
     * sensor1 ──► onR_merge1 ──► sensor2 ──► sensor3 ──► onR_merge2 ──► sensor4 ──► sensor5
     *                 │                                      │
     *              onramp1                                onramp2
     *
     * Junction Indices (for junc array):
     *   junc(0) = sensor1       // PEMS comparison
     *   junc(1) = onR_merge1    // Operational only (onramp1 convergence)
     *   junc(2) = sensor2       // PEMS comparison
     *   junc(3) = sensor3       // PEMS comparison
     *   junc(4) = onR_merge2    // Operational only (onramp2 convergence)
     *   junc(5) = sensor4       // PEMS comparison
     *   junc(6) = sensor5       // PEMS comparison
     *
     * Raw GPS from PEMS:
     *   37.832229, -122.004645  VDS 401112 (sensor1)
     *   37.833874, -122.007206  VDS 401104 (sensor2)
     *   37.835529, -122.009979  VDS 400712 (sensor3)
     *   37.838067, -122.014224  VDS 400450 (sensor4)
     *   37.839933, -122.017269  VDS 407463 (sensor5)
     *   37.832229, -122.004645  VDS 403157 OR1 (onramp1 - at sensor1)
     *   37.835529, -122.009979  VDS 403108 OR2 (onramp2 - at sensor3)
     */

    // Helper function to calculate midpoint between two GPS coordinates
    private def midpoint(p1: (Double, Double), p2: (Double, Double)): (Double, Double) =
        ((p1._1 + p2._1) / 2.0, (p1._2 + p2._2) / 2.0)

    // Helper function to calculate a point at a fraction between two GPS coordinates
    private def interpolate(p1: (Double, Double), p2: (Double, Double), fraction: Double): (Double, Double) =
        (p1._1 + (p2._1 - p1._1) * fraction, p1._2 + (p2._2 - p1._2) * fraction)

    // Raw sensor GPS coordinates
    // Warm-up segment ~600m upstream for speed stabilization (40-50 seconds travel time @ 68mph)
    // Network span (sensor1→sensor5) ≈ 870m, so warm-up = ~65% of network length
    // Distance calculation: Δlat ≈ 0.0055° × 111km ≈ 610m
    private val warm_up_sensor_gps = (37.826729, -121.999645) // ~600m upstream of sensor1 no warm up for now
    private val sensor1_gps = (37.832229, -122.004645)   // VDS 401112
    private val sensor2_gps = (37.833874, -122.007206)   // VDS 401104
    private val sensor3_gps = (37.835529, -122.009979)   // VDS 400712
    private val sensor4_gps = (37.838067, -122.014224)   // VDS 400450
    private val sensor5_gps = (37.839933, -122.017269)   // VDS 407463

    // Calculate merge point coordinates (75% position for realistic merge dynamics)
    // 75% gives vehicles ~210m to establish flow before merge, ~70m post-merge recovery before sensor
    private val onR_merge1_gps = interpolate(sensor1_gps, sensor2_gps, 0.75)  // 75% from sensor1 to sensor2
    private val onR_merge2_gps = interpolate(sensor3_gps, sensor4_gps, 0.75)  // 75% from sensor3 to sensor4

    val latlong = Map(
        // ─── PEMS Mainline Sensors (5 total) ───
        "warm_up_sensor" -> warm_up_sensor_gps,           // Hypothetical warm-up sensor before sensor1
        "sensor1" -> sensor1_gps,                         // VDS 401112 - Entry point (4 lanes)
        "sensor2" -> sensor2_gps,                         // VDS 401104 (4 lanes)
        "sensor3" -> sensor3_gps,                         // VDS 400712 (4 lanes)
        "sensor4" -> sensor4_gps,                         // VDS 400450 (4 lanes)
        "sensor5" -> sensor5_gps,                         // VDS 407463 (4 lanes)

        // ─── Operational Merge Points (2 total - PROPERLY SPACED) ───
        "onR_merge1" -> onR_merge1_gps,                   // Onramp1 merge - midpoint between sensor1 and sensor2
        "onR_merge2" -> onR_merge2_gps,                   // Onramp2 merge - midpoint between sensor3 and sensor4

        // ─── Ramp Entry Points (ALIGNED WITH MERGE POINTS - same as RoadCood.scala) ───
        "onramp1"  -> onR_merge1_gps,                     // Onramp1 - matches onR_merge1 location
        "onramp2"  -> onR_merge2_gps                      // Onramp2 - matches onR_merge2 location
    )

end Roadcood2


@main def RoadCoodTest2(): Unit =
    println("=== RoadCood2 GPS Coordinates ===")
    for (k, v) <- Roadcood2.latlong do
        println(f"  $k%-12s -> (${v._1}%.6f, ${v._2}%.6f)")

    val size = Roadcood2.latlong.size
    println(s"\nTotal points: $size")

