//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Geographic declaration for a GeoJSON-backed simulation network.
 *
 *  An `OsmSpec` is model input, not simulation state. It identifies the area to
 *  prepare once and the sensors to project onto the resulting directed graph.
 *  `NetworkBuilder` remains the authority for the network created from the
 *  prepared GeoJSON artifact.
 */

package scalation
package simulation
package process
package config

import java.nio.file.{Files, Paths}
import scala.sys.process.Process
import scala.math.{cos, Pi}
import scalation.mathstat.{Plot, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A latitude/longitude bounding box in decimal degrees. */
case class BoundingBox (south: Double, west: Double, north: Double, east: Double):

    require (-90.0 <= south && south <= north && north <= 90.0,
             s"invalid latitude bounds [$south, $north]")
    require (-180.0 <= west && west <= east && east <= 180.0,
             s"invalid longitude bounds [$west, $east]")

    /** Expand this box by an approximate metric margin. */
    def expand (marginMeters: Double): BoundingBox =
        require (marginMeters >= 0.0, s"margin must be non-negative: $marginMeters")
        val midLat = (south + north) / 2.0
        val latMargin = marginMeters / 111320.0
        val lonScale = 111320.0 * cos (midLat * Pi / 180.0).max (1.0e-6)
        val lonMargin = marginMeters / lonScale
        BoundingBox (
            (south - latMargin).max (-90.0),
            (west - lonMargin).max (-180.0),
            (north + latMargin).min (90.0),
            (east + lonMargin).min (180.0)
        )
    end expand

    override def toString: String =
        f"south=$south%.6f, west=$west%.6f, north=$north%.6f, east=$east%.6f"
end BoundingBox

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A sensor declaration to be projected onto a directed network edge. */
case class VirtualSensor (id: String, lat: Double, lon: Double, direction: String = ""):
    require (id.nonEmpty, "virtual sensor id must not be empty")
    require (-90.0 <= lat && lat <= 90.0, s"invalid sensor latitude: $lat")
    require (-180.0 <= lon && lon <= 180.0, s"invalid sensor longitude: $lon")
end VirtualSensor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Model-level declaration for preparing and consuming an OSM network artifact.
 *  @param name          stable network/artifact name
 *  @param bounds        unexpanded geographic bounds
 *  @param marginMeters  room added around the declared bounds
 *  @param sensors       optional virtual or PeMS sensor coordinates
 *
 *  API COMPATIBILITY
 *    Keep the existing constructor names and meanings stable. Extend this API
 *    with optional trailing fields or methods so existing declarations such as
 *    `val osm = OsmSpec (...)` continue to compile and retain their semantics.
 */
case class OsmSpec (name: String, bounds: BoundingBox, marginMeters: Double = 500.0,
                    sensors: Vector [VirtualSensor] = Vector.empty):
    require (name.nonEmpty, "OSM specification name must not be empty")
    require (name.matches ("[A-Za-z0-9_-]+"), s"invalid network name: $name")
    require (marginMeters >= 0.0, s"margin must be non-negative: $marginMeters")

    /** Bounds used by the network preparation step. */
    val preparedBounds: BoundingBox = bounds.expand (marginMeters)

    require (sensors.map (_.id).distinct.size == sensors.size,
             "duplicate virtual sensor ids")
    for sensor <- sensors do
        require (preparedBounds.south <= sensor.lat && sensor.lat <= preparedBounds.north &&
                 preparedBounds.west <= sensor.lon && sensor.lon <= preparedBounds.east,
                 s"sensor ${sensor.id} lies outside prepared bounds ($preparedBounds)")

    /** Stable relative location for the prepared directed-edge artifact. */
    def edgeArtifact: String = s"data/network/$name/edges.geojson"

    /** Prepare the cached OSM artifact when it is not already available.
     *  The Python builder owns OSM download and cleaning; this method only
     *  invokes that builder and verifies its output.
     *  @param pythonCommand Python executable, normally the project venv
     *  @param scriptPath    standardized OSM builder script
     *  @return the existing or newly prepared edge artifact
     */
    def ensurePrepared (pythonCommand: String = ".venv/Scripts/python.exe",
                        scriptPath: String =
                            "src/main/scala/scalation/simulation/scripts/build_osm_network.py"): String =
        val artifact = Paths.get (edgeArtifact)
        if ! Files.isRegularFile (artifact) then
            println (s"OSM artifact not found: $edgeArtifact")
            println (s"Preparing network '$name' with $scriptPath ...")
            val command = Seq (
                pythonCommand, scriptPath,
                "--name", name,
                "--west", bounds.west.toString,
                "--south", bounds.south.toString,
                "--east", bounds.east.toString,
                "--north", bounds.north.toString,
                "--margin-meters", marginMeters.toString)
            val status = Process (command).!
            require (status == 0,
                     s"OSM preparation failed for '$name' with exit status $status")
        require (Files.isRegularFile (artifact),
                 s"OSM edge artifact was not created: $edgeArtifact")
        edgeArtifact
    end ensurePrepared

    override def toString: String =
        s"OsmSpec(name=$name, bounds=($bounds), marginMeters=$marginMeters, " +
        s"sensors=${sensors.map (_.id).mkString ("[", ", ", "]")}, " +
        s"artifact=$edgeArtifact)"
end OsmSpec

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Constructors for common OSM declarations. */
object OsmSpec:

    /** Derive the smallest valid box containing the supplied sensor coordinates. */
    def fromSensors (name: String, sensors: Seq [VirtualSensor],
                     marginMeters: Double = 500.0): OsmSpec =
        require (sensors.nonEmpty, "sensor-derived OSM bounds require at least one sensor")
        val box = BoundingBox (
            sensors.map (_.lat).min, sensors.map (_.lon).min,
            sensors.map (_.lat).max, sensors.map (_.lon).max)
        OsmSpec (name, box, marginMeters, sensors.toVector)
    end fromSensors
end OsmSpec

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Small executable check for the shared OSM declaration contract. */
@main def osmSpecTest (): Unit =
    val sensors = Vector (
        VirtualSensor ("sensor-1", 37.832229, -122.004645, "W"),
        VirtualSensor ("sensor-2", 37.839933, -122.017269, "W")
    )
    val osm = OsmSpec.fromSensors ("us101_donald_doyle", sensors, marginMeters = 500.0)
    println ("OSM specification test")
    println (s"bounds = ${osm.bounds}")
    println (s"prepared bounds = ${osm.preparedBounds}")
    println (s"artifact = ${osm.edgeArtifact}")
    println (s"sensors = ${osm.sensors.map (_.id).mkString (", ")}")
end osmSpecTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Print the OSM declaration derived from the CalRoute101 anchor data. */
@main def calRouteOsmSpecTest (): Unit =
    val anchors = NetworkConfig.mainlineGPSAnchors
    require (anchors.nonEmpty, "CalRoute101 must define at least one GPS anchor")
    val bounds = BoundingBox (
        anchors.map (_._1).min, anchors.map (_._2).min,
        anchors.map (_._1).max, anchors.map (_._2).max)
    val osm = OsmSpec ("us101_donald_doyle", bounds, marginMeters = 500.0)
    println ("CalRoute101 OSM specification test")
    println (s"anchors = ${anchors.length}")
    for (anchor, i) <- anchors.zipWithIndex do
        println (f"anchor[$i] = (${anchor._1}%.6f, ${anchor._2}%.6f)")
    println (s"bounds = ${osm.bounds}")
    println (s"prepared bounds = ${osm.preparedBounds}")
    println (s"artifact = ${osm.edgeArtifact}")

    //--------------------------------------------------
    // Plot CalRoute anchors and prepared bounds

    val anchorLon = VectorD (anchors.map (_._2).toArray)
    val anchorLat = VectorD (anchors.map (_._1).toArray)
    new Plot (anchorLon, anchorLat, null, "CalRoute101 GPS Anchors", lines = false)

    val pb = osm.preparedBounds
    val boundsLon = VectorD (Array (pb.west, pb.east, pb.east, pb.west, pb.west))
    val boundsLat = VectorD (Array (pb.south, pb.south, pb.north, pb.north, pb.south))
    new Plot (boundsLon, boundsLat, null, "CalRoute101 Prepared OSM Bounds", lines = true)
    println ("[PASS] CalRoute anchor and bounds plots created")
end calRouteOsmSpecTest


@main def osmSpecTestTwo (): Unit =

    val sensors = Vector (
        VirtualSensor ("sensor-1", 37.832229, -122.004645, "W"),
        VirtualSensor ("sensor-2", 37.839933, -122.017269, "W"),
        VirtualSensor ("sensor-3", 37.845000, -122.025000, "W")
    )

    val osm =
        OsmSpec.fromSensors (
            name = "us101_test",
            sensors = sensors,
            marginMeters = 500.0
        )

    println ("=== OsmSpec Test ===")

    //--------------------------------------------------
    // 1. Tight bounds should exactly contain sensor extremes
    //--------------------------------------------------

    assert (osm.bounds.south == sensors.map (_.lat).min)
    assert (osm.bounds.north == sensors.map (_.lat).max)
    assert (osm.bounds.west  == sensors.map (_.lon).min)
    assert (osm.bounds.east  == sensors.map (_.lon).max)

    println ("[PASS] sensor-derived bounds")

    //--------------------------------------------------
    // 2. Prepared bounds must be larger than original bounds
    //--------------------------------------------------

    assert (osm.preparedBounds.south < osm.bounds.south)
    assert (osm.preparedBounds.north > osm.bounds.north)
    assert (osm.preparedBounds.west  < osm.bounds.west)
    assert (osm.preparedBounds.east  > osm.bounds.east)

    println ("[PASS] margin expansion")

    //--------------------------------------------------
    // 3. Every sensor must lie inside prepared bounds
    //--------------------------------------------------

    for sensor <- osm.sensors do
        assert (
            osm.preparedBounds.south <= sensor.lat &&
              sensor.lat <= osm.preparedBounds.north &&
              osm.preparedBounds.west <= sensor.lon &&
              sensor.lon <= osm.preparedBounds.east
        )

    println ("[PASS] sensors inside prepared bounds")

    // ------------------------------------------------------------
    // 4. Stable artifact path
    // ------------------------------------------------------------

    assert (
        osm.edgeArtifact ==
          "data/network/us101_test/edges.geojson"
    )

    println ("[PASS] artifact path")

    // ------------------------------------------------------------
    // 5. Duplicate sensor IDs should fail
    // ------------------------------------------------------------

    val duplicateIdFailed =
        try
            OsmSpec (
                "duplicate_test",
                BoundingBox (
                    37.82, -122.03,
                    37.86, -122.00
                ),
                sensors = Vector (
                    VirtualSensor ("same-id", 37.83, -122.01),
                    VirtualSensor ("same-id", 37.84, -122.02)
                )
            )
            false
        catch
            case _: IllegalArgumentException => true

    assert (duplicateIdFailed)

    println ("[PASS] duplicate sensor IDs rejected")

    // ------------------------------------------------------------
    // 6. Sensor outside prepared bounds should fail
    // ------------------------------------------------------------

    val outsideSensorFailed =
        try
            OsmSpec (
                "outside_test",
                BoundingBox (
                    37.82, -122.03,
                    37.84, -122.01
                ),
                marginMeters = 0.0,
                sensors = Vector (
                    VirtualSensor (
                        "outside",
                        37.90,
                        -122.01
                    )
                )
            )
            false
        catch
            case _: IllegalArgumentException => true

    assert (outsideSensorFailed)

    println ("[PASS] outside sensor rejected")

    //--------------------------------------------------
    // Plot sensor locations and prepared bounds

    val sensorLon = VectorD (sensors.map (_.lon).toArray)
    val sensorLat = VectorD (sensors.map (_.lat).toArray)
    new Plot (sensorLon, sensorLat, null, "OSM Sensor Locations", lines = false)

    val pb = osm.preparedBounds
    val boundsLon = VectorD (Array (pb.west, pb.east, pb.east, pb.west, pb.west))
    val boundsLat = VectorD (Array (pb.south, pb.south, pb.north, pb.north, pb.south))
    new Plot (boundsLon, boundsLat, null, "Prepared OSM Bounds", lines = true)
    println ("[PASS] sensor and bounds plots created")

    // ------------------------------------------------------------
    //--------------------------------------------------
    // Summary

    println ()
    println (s"bounds          = ${osm.bounds}")
    println (s"prepared bounds = ${osm.preparedBounds}")
    println (s"artifact        = ${osm.edgeArtifact}")
    println (s"sensors         = ${osm.sensors.map (_.id).mkString (", ")}")
    println ("=== ALL TESTS PASSED ===")

end osmSpecTestTwo
