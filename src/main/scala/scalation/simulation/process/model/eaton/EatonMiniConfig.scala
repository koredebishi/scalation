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

    val boxEdgeTolerance   = 0.002      // degrees (~200 m): a cut road ends this close to the box edge
    val maxStationDistance = 150.0      // metres: farthest a PeMS station may sit from its matched edge
    val rampTurn           = 90.0       // degrees: ramps curve, so they may turn this far from the freeway heading
    val rampInfluence      = 457.0      // metres (HCM 1,500 ft): micro freeway past a merge / before a diverge

    /** Eaton's OSM highway-to-resolution and speed policy. 
     * Korede 10/18/2026 Need Fixing.
     * Idea is sound for Eaton when we do not have data from Pems source 
     * but this is too deterministic and may need randomness
     * A random variate may be preferred for each road type policy where 
     * Variate sets (60 -75mph), all the way down to (20-35mph) for the roads 
     * in these cases to mirror slow and fast driver heterogenienty. 
     * */
    val edgePolicy: EdgePolicy = new EdgePolicy:
        def profile (edge: NetEdge): EdgeProfile =
            val typed = edge.highway match                         // road-type default limit
                case "motorway"      => EdgeProfile ("motorway", true, 65.0 * 0.44704)
                case "motorway_link" => EdgeProfile ("motorway_link", true, 65.0 * 0.44704)
                case "primary"       => EdgeProfile ("primary", false, 35.0 * 0.44704)
                case "secondary"     => EdgeProfile ("secondary", false, 35.0 * 0.44704)
                case "tertiary"      => EdgeProfile ("tertiary", false, 30.0 * 0.44704)
                case "residential"   => EdgeProfile ("residential", false, 25.0 * 0.44704)
                case _               => EdgeProfile ("other", false, 25.0 * 0.44704)
            if edge.maxspeed.isNaN then typed                      // untagged: road-type default
            else typed.copy (speed = edge.maxspeed)                // OSM posted limit
    end edgePolicy
end EatonMiniConfig
