//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Fri Jan 31 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Traffic Model Builder - Assembles Simulation from Configs
 */

package scalation
package simulation
package process
package builder

import scalation.random.Variate
import scalation.simulation.process.config.*
import scalation.simulation.process.arrival.ArrivalSource

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrafficModelBuilder` class constructs a traffic simulation model from
 *  configuration objects. Encapsulates the creation of junctions, routes, sources,
 *  ramps, and sinks.
 *  @param network    the network topology configuration
 *  @param demand     the demand/arrival configuration
 *  @param dynamics   the vehicle dynamics configuration
 *  @param name       the model name
 *  @param animating  whether to animate the model
 *  @param aniRatio   the animation speed ratio
 */
class TrafficModelBuilder (network: NetworkConfig, demand: PeMSDemand,
                           dynamics: DynamicsConfig, name: String = "TrafficModel",
                           animating: Boolean = false, aniRatio: Double = 500.0):

    private val debug = debugf ("TrafficModelBuilder", false)

    // Extract specs
    private val mainline = network.mainline
    private val nSegments = mainline.segments
    private val nLanes = mainline.lanesPerSegment
    private val nJunctions = network.numJunctions

    // Create arrival sources directly from demand config (no TrafficConfig2 needed)
    private val (mainlineSources, rampSources) = ArrivalSource.allSources (demand, nLanes)

    debug ("init", s"nSegments = $nSegments, nLanes = $nLanes, nJunctions = $nJunctions")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build junction names from network config.
     */
    private def buildJunctionNames (): Array [String] =
        val names = new Array [String] (nJunctions)
        var jIdx = 0

        // Map sensor and ramp positions to junction indices
        val sensorMap = network.sensors.map (s => s.segment -> s.id).toMap
        val rampMap = network.ramps.map (r => r.joinSegment -> r.id).toMap

        cfor (0, nJunctions) { j =>
            names(j) = if sensorMap.contains (j) then sensorMap(j)
                       else if rampMap.contains (j) then s"merge_$j"
                       else s"junc_$j"
        }
        names
    end buildJunctionNames

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the junction indices that correspond to PeMS sensors.
     */
    def pemsSensorIndices: Array [Int] = network.pemsSensorIndices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the junction indices where ramps join.
     */
    def rampJoinSegments: Array [Int] = network.ramps.map (_.joinSegment).toArray

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the number of lanes.
     */
    def numLanes: Int = nLanes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the number of segments.
     */
    def numSegments: Int = nSegments

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the dynamics configuration.
     */
    def getDynamics: DynamicsConfig = dynamics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the demand configuration.
     */
    def getDemand: PeMSDemand = demand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the network configuration.
     */
    def getNetwork: NetworkConfig = network

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get arrival sources for mainline (one per lane).
     */
    def mainlineArrivalSources: Array [ArrivalSource] = mainlineSources

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get arrival sources for ramps (one per ramp).
     */
    def rampArrivalSources: Array [ArrivalSource] = rampSources

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get car-following model type.
     */
    def carFollowingModel: CarFollowingModel = dynamics.carFollowing

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get maximum velocity.
     */
    def vmax: Double = dynamics.vmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get lane change probability.
     */
    def laneChangeProb: Double = dynamics.laneChangeProb

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the number of ramps.
     */
    def numRamps: Int = network.ramps.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build mainline VSources - one per lane, parameterized by numLanes.
     *  @param model      the simulation model
     *  @param makeCar    factory function to create vehicles
     *  @param route      the route for positioning
     *  @param baseName   base name for sources
     *  @param iArrivalRV the inter-arrival time distribution
     */
    def buildMainlineSources (model: Model, makeCar: () => Vehicle, route: Route,
                              baseName: String, iArrivalRV: Variate): List [VSource] =
        import scala.collection.mutable.ListBuffer

        debug ("buildMainlineSources", s"building $nLanes mainline sources")
        val sources = new ListBuffer [VSource] ()
        val arrivalSources = mainlineArrivalSources

        cfor (0, nLanes) { l =>
            val name = s"${baseName}_L$l"
            val subtype = l                                    // mainline lanes: 0 until numLanes
            val nStop = arrivalSources(l).getTotalVehicles (l) // vehicle count from arrival source
            val loc = shiftFromRoute (route, l)                // position near lane start
            debug ("buildMainlineSources", s"  lane $l: name=$name, nStop=$nStop, loc=(${loc(0)}, ${loc(1)})")
            val src = new VSource (name, model, makeCar, subtype, nStop, iArrivalRV, loc)
            sources += src
        }
        sources.toList
    end buildMainlineSources

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build ramp VSources - one per ramp, subtypes start at numLanes.
     *  @param model         the simulation model
     *  @param makeCar       factory function to create vehicles
     *  @param centerPos     center position for animation
     *  @param offsets       offset positions for each ramp
     *  @param iArrivalRV    the inter-arrival time distribution for ramps
     */
    def buildRampSources (model: Model, makeCar: () => Vehicle, centerPos: (Int, Int),
                          offsets: Array [(Int, Int)]): List [VSource] =
        import scala.collection.mutable.ListBuffer

        debug ("buildRampSources", s"building ${network.ramps.length} ramp sources")
        val sources = new ListBuffer [VSource] ()
        val arrivalSources = rampArrivalSources
        val rampConfigs = network.ramps

        cfor (0, rampConfigs.length) { r =>
            val ramp = rampConfigs(r)
            val name = s"srcRamp${r + 1}"
            val subtype = nLanes + r                           // ramp subtypes: numLanes, numLanes+1, ...
            val nStop = arrivalSources(r).getTotalVehicles (0) // vehicle count from arrival source
            val rampDistribution = arrivalSources(r).getDistribution  // Use per-ramp distribution
            val offset = offsets(r + 1)                        // offset for ramp r (offsets(0) is mainline)
            val loc = Array ((centerPos._1 + offset._1).toDouble,
                             (centerPos._2 + offset._2).toDouble, 20.0, 20.0)
            debug ("buildRampSources", s"  ramp $r: name=$name, nStop=$nStop, distribution=${rampDistribution}, loc=(${loc(0)}, ${loc(1)})")
            val src = new VSource (name, model, makeCar, subtype, nStop, rampDistribution, loc)
            sources += src
        }
        sources.toList
    end buildRampSources

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build Ramp objects with merge info encapsulated.
     *  @param motion       the dynamics model
     *  @param rampSources  the VSource list for ramps (from buildRampSources)
     *  @param rampSensors  the Junction array for ramp merge points
     */
    def buildRamps (motion: Dynamics, rampSources: List [VSource],
                    rampSensors: Array [Junction]): Array [Ramp] =
        val rampConfigs = network.ramps
        val ramps = new Array [Ramp] (rampConfigs.length)

        debug ("buildRamps", s"building ${rampConfigs.length} ramps")
        cfor (0, rampConfigs.length) { r =>
            val config = rampConfigs(r)
            val name = s"onRamp${r + 1}"
            val from = rampSources(r)                                   // VSource for this ramp
            val to = rampSensors(r)                                     // Junction where ramp merges
            val mode = scalation.simulation.process.RampMode.On         // use process.RampMode, not config.RampMode
            debug ("buildRamps", s"  ramp $r: name=$name, from=${from.name}, to=${to.name}")
            ramps(r) = new Ramp (name, from, to, motion, mode, false, 0.0, 0.0)
        }
        ramps
    end buildRamps

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute shifted location near start of lane for VSource positioning.
     *  @param route  the route
     *  @param l      the lane index
     *  @param back   how far back from lane start
     */
    private def shiftFromRoute (route: Route, l: Int, back: Double = 15.0): Array [Double] =
        import scala.math.hypot
        val seg0 = route.path(l).seg(0)
        val p1x = seg0.p1(0)
        val p1y = seg0.p1(1)
        val p2x = seg0.p2(0)
        val p2y = seg0.p2(1)
        val dx = p2x - p1x
        val dy = p2y - p1y
        val mag = hypot (dx, dy)
        val ux = if mag > 1e-9 then dx / mag else 0.0
        val uy = if mag > 1e-9 then dy / mag else 0.0
        val sx = p1x - ux * back
        val sy = p1y - uy * back
        Array (sx, sy, 20.0, 20.0)
    end shiftFromRoute

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print builder summary.
     */
    def summary (): Unit =
        println ("\n" + "=" * 70)
        println ("TRAFFIC MODEL BUILDER SUMMARY")
        println ("=" * 70)
        println (s"Network:      ${mainline.id}")
        println (s"Segments:     $nSegments")
        println (s"Lanes:        $nLanes")
        println (s"Junctions:    $nJunctions")
        println (s"Ramps:        ${network.ramps.length}")
        println (s"Sensors:      ${network.sensors.length}")
        println (s"Mode:         PeMS Data-Driven")
        println (s"Car-Following: ${dynamics.carFollowing}")
        println (s"ODE Solver:   ${dynamics.odeSolver}")
        println (s"Vmax:         ${dynamics.vmax} m/s")
        // println (s"Lane Change:  ${dynamics.laneChangeProb * 100}%")  // Not implemented yet
        println ("=" * 70)
    end summary

end TrafficModelBuilder


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrafficModelBuilder` companion object provides factory methods and presets.
 */
object TrafficModelBuilder:

    import scalation.simulation.process.config.{NetworkConfig, PeMSDemand, DynamicsConfig}

    // Mutable dynamics config for array job model switching
    private var _dynamicsOverride: DynamicsConfig = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the dynamics config override for array job model switching.
     *  Call this before creating a TrafficModelBuilder to use a specific model.
     *  @param config  the dynamics configuration (IDM, Gipps, or Krause)
     */
    def setDynamicsConfig(config: DynamicsConfig): Unit =
        _dynamicsOverride = config
    end setDynamicsConfig

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear the dynamics config override.
     */
    def clearDynamicsConfig(): Unit =
        _dynamicsOverride = null
    end clearDynamicsConfig

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a builder for US-101 Donald Doyle corridor with PeMS data.
     *  Uses dynamics override if set, otherwise defaults to IDM.
     *  @param tau  the Erlang2S shift parameter
     */
    def US101_DonaldDoyle_PeMS (tau: Double = 0.6): TrafficModelBuilder =
        val dynamicsToUse = if _dynamicsOverride != null then _dynamicsOverride
                            else DynamicsConfig.idmDefault
        new TrafficModelBuilder (
            network = NetworkConfig.US101_DonaldDoyle,
            demand = PeMSDemand.US101_DonaldDoyle (tau),
            dynamics = dynamicsToUse
        )
    end US101_DonaldDoyle_PeMS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a builder for US-101 Donald Doyle corridor with specific dynamics.
     *  @param dynamics  the dynamics configuration
     *  @param tau       the Erlang2S shift parameter
     */
    def US101_DonaldDoyle_PeMS_WithDynamics (dynamics: DynamicsConfig, tau: Double = 0.6): TrafficModelBuilder =
        new TrafficModelBuilder (
            network = NetworkConfig.US101_DonaldDoyle,
            demand = PeMSDemand.US101_DonaldDoyle (tau),
            dynamics = dynamics
        )
    end US101_DonaldDoyle_PeMS_WithDynamics

end TrafficModelBuilder


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test the TrafficModelBuilder with PeMS mode.
 *  > runMain scalation.simulation.process.builder.runTestBuilderPeMS
 */
@main def runTestBuilderPeMS (): Unit =
    banner ("TrafficModelBuilder Test - PeMS Mode")

    val builder = TrafficModelBuilder.US101_DonaldDoyle_PeMS ()
    builder.summary ()

    println (s"\nPeMS Sensor Indices: ${builder.pemsSensorIndices.mkString (", ")}")
    println (s"Ramp Join Segments:  ${builder.rampJoinSegments.mkString (", ")}")

    val mainSources = builder.mainlineArrivalSources
    println (s"\nMainline Sources: ${mainSources.length}")
    cfor (0, mainSources.length) { i =>
        val src = mainSources(i)
        println (f"  Source $i: total = ${src.getTotalVehicles (0)}%5d, mu(0) = ${src.getMu (0, 0)}%.2f")
    }

    val rampSources = builder.rampArrivalSources
    println (s"\nRamp Sources: ${rampSources.length}")
    cfor (0, rampSources.length) { i =>
        val src = rampSources(i)
        println (f"  Ramp $i: total = ${src.getTotalVehicles (0)}%5d, mu(0) = ${src.getMu (0, 0)}%.2f")
    }
end runTestBuilderPeMS


