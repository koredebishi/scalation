//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Mon Apr 14 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    OsmRoadNetwork: Load OSM road + place data for background map rendering
 *
 *  Model-agnostic library class.  Loads a cached JSON file produced by
 *  `simulation/scripts/download_osm_geometry.py`, projects all GPS coordinates
 *  to screen space using the existing `Coordinates` class, and returns
 *  screen-space polylines + place labels for `DgAnimator` to render.
 *
 *  The JSON contains two data arrays:
 *    - "roads"  : road polylines with highway type, name, ref
 *    - "places" : geographic place nodes (city, suburb, neighbourhood, etc.)
 *
 *  Everything comes from OpenStreetMap data — no hardcoded labels.
 *  Works for any location worldwide.
 *
 *  The `gpsAnchors` parameter ensures OSM points share the SAME projection
 *  frame as the simulation junctions — without it, the background would be
 *  offset from the simulation roads.
 */

package scalation
package animation

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scalation.{Coordinates, LatLong, LatLong2CTM}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A geographic place from OpenStreetMap, projected to screen space.
 *  Downloaded by the Python script from OSM `place` nodes.
 *
 *  @param name       place name as it appears in OSM (e.g., "Pasadena", "Eagle Rock")
 *  @param placeType  OSM place tag: "city", "town", "suburb", "neighbourhood", "village", "hamlet"
 *  @param x          screen-space x coordinate
 *  @param y          screen-space y coordinate
 */
case class OsmPlace (name: String, placeType: String, x: Double, y: Double)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Immutable background road network + place labels for map rendering.
 *  All data comes from the OSM JSON — no model-specific content.
 *
 *  @param polylines   screen-space (x, y) arrays, one per road segment
 *  @param roadTypes   OSM highway tag per road ("motorway", "primary", etc.)
 *  @param roadNames   OSM name per road (may be empty string)
 *  @param roadRefs    OSM ref per road (e.g., "I 210", "CA 134"; may be empty)
 *  @param places      geographic place labels from OSM (cities, suburbs, neighbourhoods)
 */
case class OsmRoadNetwork (polylines:  Array[Array[(Double, Double)]],
                           roadTypes:  Array[String],
                           roadNames:  Array[String]      = Array.empty,
                           roadRefs:   Array[String]      = Array.empty,
                           places:     Array[OsmPlace]    = Array.empty)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Companion object for `OsmRoadNetwork`.
 *  Provides `load` factory that reads a cached JSON file and projects
 *  GPS → screen coordinates.
 */
object OsmRoadNetwork:

    private val debug = debugf ("OsmRoadNetwork", true)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load background road network from a cached JSON file.
     *
     *  @param jsonPath    path to `data/osm/<name>_roads.json`
     *  @param gpsAnchors  simulation junction GPS coords `(lat, lon)` — ensures shared projection
     *  @param dims        screen canvas dimensions `(width, height)`
     *  @return OsmRoadNetwork with screen-space polylines + places, or empty if file missing
     */
    def load (jsonPath: String,
              gpsAnchors: Array [(Double, Double)],
              dims: (Double, Double)): OsmRoadNetwork =

        // ── Read JSON file ──────────────────────────────────────────────
        val file = new java.io.File (jsonPath)
        if !file.exists () then
            debug ("load", s"JSON not found: $jsonPath — returning empty network")
            return OsmRoadNetwork (Array.empty, Array.empty)

        val src = Source.fromFile (file, "UTF-8")
        val raw = try src.mkString finally src.close ()
        debug ("load", s"Read ${raw.length} chars from $jsonPath")

        // ── Parse roads ─────────────────────────────────────────────────
        val roads    = ArrayBuffer [Array [(Double, Double)]] ()
        val types    = ArrayBuffer [String] ()
        val names    = ArrayBuffer [String] ()
        val refs     = ArrayBuffer [String] ()

        // Split into road objects by finding {"osm_id": patterns
        val roadBlocks = raw.split("\\{\"osm_id\"").drop(1)       // first element is preamble

        for block <- roadBlocks do
            // Extract highway type
            val hwMatch = "\"highway\"\\s*:\\s*\"([^\"]+)\"".r.findFirstMatchIn(block)
            val highway = hwMatch.map(_.group(1)).getOrElse("unknown")

            // Extract name (may be null in JSON)
            val nameMatch = "\"name\"\\s*:\\s*\"([^\"]+)\"".r.findFirstMatchIn(block)
            val name = nameMatch.map(_.group(1)).getOrElse("")

            // Extract ref (may be null in JSON)
            val refMatch = "\"ref\"\\s*:\\s*\"([^\"]+)\"".r.findFirstMatchIn(block)
            val ref = refMatch.map(_.group(1)).getOrElse("")

            // Extract points array: "points": [[lat, lon], [lat, lon], ...]
            val ptsIdx = block.indexOf("\"points\"")
            if ptsIdx >= 0 then
                val arrStart = block.indexOf("[[", ptsIdx)
                val arrEnd   = block.indexOf("]]", ptsIdx)
                if arrStart >= 0 && arrEnd >= 0 then
                    val ptsStr = block.substring(arrStart + 1, arrEnd + 1) // [lat,lon],[lat,lon],...
                    val pairs  = "\\[([\\d.\\-]+),\\s*([\\d.\\-]+)\\]".r
                                    .findAllMatchIn(ptsStr)
                                    .map(m => (m.group(1).toDouble, m.group(2).toDouble))
                                    .toArray
                    if pairs.length >= 2 then
                        roads += pairs
                        types += highway
                        names += name
                        refs  += ref
                    end if
                end if
            end if
        end for

        debug ("load", s"Parsed ${roads.length} roads from JSON")

        // ── Parse places ────────────────────────────────────────────────
        // Places are in a separate "places" JSON array after "roads".
        // Each: {"name": "...", "type": "city", "lat": ..., "lon": ...}
        val gpsPlaces = ArrayBuffer [(String, String, Double, Double)] ()  // (name, type, lat, lon)

        val placesIdx = raw.indexOf("\"places\"")
        if placesIdx >= 0 then
            val placesStart = raw.indexOf("[", placesIdx)
            val placesEnd   = raw.indexOf("]", placesStart)
            if placesStart >= 0 && placesEnd >= 0 then
                val placesStr = raw.substring(placesStart, placesEnd + 1)
                // Match each place object
                val placePattern = "\"name\"\\s*:\\s*\"([^\"]+)\"\\s*,\\s*\"type\"\\s*:\\s*\"([^\"]+)\"\\s*,\\s*\"lat\"\\s*:\\s*([\\d.\\-]+)\\s*,\\s*\"lon\"\\s*:\\s*([\\d.\\-]+)".r
                for m <- placePattern.findAllMatchIn(placesStr) do
                    gpsPlaces += ((m.group(1), m.group(2), m.group(3).toDouble, m.group(4).toDouble))
                end for
            end if
        end if

        debug ("load", s"Parsed ${gpsPlaces.length} places from JSON")

        if gpsAnchors.isEmpty then
            return OsmRoadNetwork (Array.empty, Array.empty)

        // ── Build projection frame from ONLY the anchor GPS points ──────
        // This ensures OSM roads use the EXACT same cent / nsew / scale
        // as EatonCorridorConfig.computeAllCoordinates, which also creates
        // Coordinates from station GPS points with the same dims.
        val anchorCoords = new Coordinates (dims._1, dims._2, gpsAnchors)
        val cent  = anchorCoords.cent                      // central meridian (avg longitude)
        val nsew  = anchorCoords.nsew                      // CTM bounding box [ysmax, ysmin, xsmax, xsmin, w, h]
        val scale = anchorCoords.scale                     // pixels per CTM unit
        val xt    = anchorCoords.xt                        // x margin (20.0)
        val yt    = anchorCoords.yt                        // y margin (20.0)
        val ah    = anchorCoords.ah                        // adjusted height (aniHeight - 2*yt)
        val w     = nsew(3)                                // xsmin (western CTM bound)
        val s     = nsew(1)                                // ysmin (southern CTM bound)

        debug ("load", s"Anchor projection: cent=$cent, scale=$scale, nsew=${nsew.mkString(",")}")

        /** Project a single GPS (lat, lon) to screen (x, y). */
        def project (lat: Double, lon: Double): (Double, Double) =
            val ctm = LatLong2CTM.latLong2CTMxy (new LatLong ((lat, lon)), cent)
            val tx  = ctm._1 - w
            val ty  = ctm._2 - s
            (tx * scale + xt, ty * -scale + ah + yt)

        // ── Project roads ───────────────────────────────────────────────
        val screenRoads = ArrayBuffer [Array [(Double, Double)]] ()
        for r <- roads do
            val screenPts = new Array [(Double, Double)] (r.length)
            var i = 0
            while i < r.length do
                screenPts(i) = project (r(i)._1, r(i)._2)
                i += 1
            end while
            screenRoads += screenPts
        end for

        // ── Project places ──────────────────────────────────────────────
        val screenPlaces = ArrayBuffer [OsmPlace] ()
        for (name, ptype, lat, lon) <- gpsPlaces do
            val (sx, sy) = project (lat, lon)
            screenPlaces += OsmPlace (name, ptype, sx, sy)
        end for

        debug ("load", s"Projected ${screenRoads.length} roads, ${screenPlaces.length} places to screen space")

        OsmRoadNetwork (screenRoads.toArray, types.toArray,
            names.toArray, refs.toArray, screenPlaces.toArray)
    end load

end OsmRoadNetwork
