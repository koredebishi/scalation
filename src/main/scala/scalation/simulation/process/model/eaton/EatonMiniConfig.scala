//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** EatonMini configuration ownership.
 *
 *  This file will contain only Eaton-specific constants and the `OsmSpec`
 *  declaration. It is configuration, not a `Model`, and it must not start a
 *  simulation or duplicate generic Scalation configuration.
 */

package scalation
package simulation
package process
package model
package eaton

import scalation.simulation.process.builder.{EdgePolicy, EdgeProfile, NetEdge}
import scalation.simulation.process.config.{BoundingBox, OsmSpec}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** EatonMini constants and OSM declaration. */
object EatonMiniConfig:

    val osm = OsmSpec (
        "eaton",
        BoundingBox (south = 34.131843, west = -118.304031,
                     north = 34.244987, east = -118.061321),
        marginMeters = 0.0)

    val sourceId  = 616843101L
    val i210Sink  = 356052035L
    val sr134Sink = 27568007L

    val rampStationIds = Array (716596, 716588, 716586, 770402)

    /** Eaton's OSM highway-to-resolution and speed policy. */
    val edgePolicy: EdgePolicy = new EdgePolicy:
        def profile (edge: NetEdge): EdgeProfile = edge.highway match
            case "motorway"      => EdgeProfile ("motorway", true, 65.0 * 0.44704)
            case "motorway_link" => EdgeProfile ("motorway_link", true, 65.0 * 0.44704)
            case "primary"       => EdgeProfile ("primary", false, 35.0 * 0.44704)
            case "secondary"     => EdgeProfile ("secondary", false, 35.0 * 0.44704)
            case "tertiary"      => EdgeProfile ("tertiary", false, 30.0 * 0.44704)
            case "residential"   => EdgeProfile ("residential", false, 25.0 * 0.44704)
            case _               => EdgeProfile ("other", false, 25.0 * 0.44704)
    end edgePolicy
end EatonMiniConfig
