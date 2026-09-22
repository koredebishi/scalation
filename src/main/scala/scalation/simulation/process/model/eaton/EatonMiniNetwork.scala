//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** EatonMini network-assembly ownership.
 *
 *  This file will contain Eaton-specific network inputs: the prepared
 *  OsmSpec, projection, source placement, destinations, and the call to the
 *  reusable NetworkBuilder API. It must not duplicate graph construction.
 */

package scalation
package simulation
package process
package model
package eaton

import scala.collection.immutable.Map
import scalation.animation.OsmRoadNetwork
import scalation.simulation.process.{Component, Dynamics}
import scalation.simulation.process.builder.{BuiltGraph, NetworkBuilder}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** EatonMini OSM network assembly and projection owner. */
class EatonMiniNetwork (stream: Int = 0):

    private val osm = EatonMiniConfig.osm
    val edges = NetworkBuilder.read (osm.ensurePrepared ())

    private val sourceEdge = edges.find (_.u == EatonMiniConfig.sourceId).get
    private val pxPerM = 2.9
    private val boxW = 1000.0
    private val boxH = 600.0
    private val westOfSrc = 114.0
    private val northOfSrc = 69.0
    private val mLat = 111320.0
    private val mLon = 111320.0 * math.cos (math.toRadians (sourceEdge.uLat))
    private val nwLat = sourceEdge.uLat + northOfSrc / mLat
    private val nwLon = sourceEdge.uLon - westOfSrc / mLon
    private val anchors = Array ((nwLat, nwLon),
                                (nwLat - boxH / mLat, nwLon + boxW / mLon))

    val project = OsmRoadNetwork.buildProjector (
        anchors, (boxW * pxPerM + 40.0, boxH * pxPerM + 40.0), "EatonMiniTest")
    val sourcePosition = project (sourceEdge.uLat, sourceEdge.uLon)

    def build (sourceComponents: Map [Long, Component], motion: Dynamics, nt: Int): BuiltGraph =
        NetworkBuilder.build (edges,
                              Array (EatonMiniConfig.i210Sink, EatonMiniConfig.sr134Sink),
                              project, motion, nt, EatonMiniConfig.edgePolicy,
                              stream, sourceComponents)
    end build
end EatonMiniNetwork
