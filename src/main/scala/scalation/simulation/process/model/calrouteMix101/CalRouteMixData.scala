//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** CalRouteMix data specification.
 *
 *  This file owns the Donald-Doyle sensor coordinates and PeMS file choices.
 *  OSM remains the authority for topology, lane counts, geometry, and routing.
 */

package scalation
package simulation
package process
package model
package calrouteMix101

import scalation.random.Erlang2S
import scalation.simulation.process.config.{DynamicsConfig, PeMSArrivals, PeMSDemand}

object CalRouteMixData:

    private def interpolate (p1: (Double, Double), p2: (Double, Double), f: Double): (Double, Double) =
        (p1._1 + (p2._1 - p1._1) * f, p1._2 + (p2._2 - p1._2) * f)

    val mainlineGPS =
        val sensor1 = (37.832229, -122.004645)
        val sensor2 = (37.833874, -122.007206)
        val sensor3 = (37.835529, -122.009979)
        val sensor4 = (37.838067, -122.014224)
        val sensor5 = (37.839933, -122.017269)
        val merge1  = interpolate (sensor1, sensor2, 0.75)
        val merge2  = interpolate (sensor3, sensor4, 0.75)
        Array ((37.826729, -121.999645), sensor1, merge1, sensor2,
               sensor3, merge2, sensor4, sensor5)

    val rampGPS = Array (mainlineGPS (2), mainlineGPS (5))

    val sensors = Array (
        ("sensor1", 1), ("sensor2", 3), ("sensor3", 4),
        ("sensor4", 6), ("sensor5", 7))

    val demand = PeMSDemand (
        mainline = PeMSArrivals ("1-401112ML.csv", Erlang2S (0.6), perLane = true),
        ramps = List (
            PeMSArrivals ("1-403157OR.csv", Erlang2S (4.0), perLane = false),
            PeMSArrivals ("2-403108OR.csv", Erlang2S (10.0), perLane = false)),
        dataDir = "Mainline_VDS_Donald_Doyle")

    val dynamics = DynamicsConfig.idmDefault

end CalRouteMixData
