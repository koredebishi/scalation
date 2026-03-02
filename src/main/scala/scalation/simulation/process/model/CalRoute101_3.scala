//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Fri Jan 31 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    CalRoute101_3: Traffic Model Using Refactored Config Architecture
 */

package scalation
package simulation
package process
package model

import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.Uniform
import scalation.simulation.process.{IntegratorType, IDMDynamics, GippsDynamics, KraussDynamics}
import scalation.simulation.process.config.{CarFollowingModel, NetworkConfig, SimulationReport}
import scalation.simulation.process.builder.TrafficModelBuilder

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run CalRoute101_3 simulation.
 *  > runMain scalation.simulation.process.model.runCalRoute101_3
 */
@main def runCalRoute101_3 (): Unit = new CalRoute101_3 ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CalRoute101_3` class is a traffic simulation model using the refactored
 *  config architecture. Uses NetworkConfig, DemandConfig, DynamicsConfig.
 *  @param name       the name of the model
 *  @param reps       the number of replications
 *  @param animating  whether to animate
 *  @param aniRatio   animation speed ratio
 */
class CalRoute101_3 (name: String = "CalRoute101_3", reps: Int = 1,
                     animating: Boolean = true, aniRatio: Double = 500.0)
      extends Model (name, reps, animating, aniRatio)
         with RowTimeLoader:

    private val debug = debugf ("CalRoute101_3", false)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Configuration - uses new architecture
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val builder = TrafficModelBuilder.US101_DonaldDoyle_PeMS()
    builder.summary ()

    // Extract from builder
    private val network  = builder.getNetwork                  // NetworkConfig: topology (segments, lanes, ramps, sensors)
    private val demand   = builder.getDemand                   // DemandConfig: PeMS arrivals (anchor file, distribution)
    private val dynamics = builder.getDynamics                 // DynamicsConfig: IDM/Gipps, ODE solver, cfParams

    private val numLanes    = builder.numLanes                 // 4 lanes from NetworkConfig
    private val numSegments = builder.numSegments              // 7 segments from NetworkConfig
    private val rampJoinSeg = builder.rampJoinSegments         // [2, 5] - where ramps merge
    private val pemsToJunc  = builder.pemsSensorIndices        // [1, 3, 4, 6, 7] - junctions with PeMS sensors

    // Time intervals - 48 for 15-min intervals over 12 hours (6am-6pm)
    private val nt = 48

    // Arrival sources from builder
    private val mainlineArrivalSources = builder.mainlineArrivalSources   // 4 sources, one per lane
    private val rampArrivalSources     = builder.rampArrivalSources       // 2 sources, one per ramp

    debug ("init", s"numLanes=$numLanes, numSegments=$numSegments, nt=$nt")
    debug ("init", s"rampJoinSeg=${rampJoinSeg.mkString (",")}, pemsToJunc=${pemsToJunc.mkString (",")}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Dynamics - from config
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val motion = dynamics.carFollowing match
        case CarFollowingModel.IDM    => IDMDynamics
        case CarFollowingModel.Gipps  => GippsDynamics
        case CarFollowingModel.Krause => KraussDynamics

    // Set Ballistic integrator for the selected model (ANNSIM 2026 concluded this is optimal)
    motion match
        case IDMDynamics    => IDMDynamics.integratorType = IntegratorType.Ballistic
        case GippsDynamics  => GippsDynamics.integratorType = IntegratorType.Ballistic
        case KraussDynamics => KraussDynamics.integratorType = IntegratorType.Ballistic
        case _              => // do nothing for unknown dynamics

    private val rand = Uniform (0.0, 1.0)
    private val iArrivalRV      = mainlineArrivalSources(0).getDistribution

    setTime (nt * rowTime)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Animation coordinates and junction setup
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val (w, h) = (1500, 1500)
    private val aniCoords_Main = NetworkConfig.getMainlineCoordinates ((w, h))
    private val aniCoords_Ramp = NetworkConfig.getRampCoordinates ((w, h))
    private val (centerPos, offsets) = NetworkConfig.getVSourceCenterAndOffsets ((w, h))

    // Junctions - use sensor names from NetworkConfig
    debug ("init", s"Creating ${aniCoords_Main.length} junctions")
    private val junc = Array.ofDim [Junction] (aniCoords_Main.length)
    private val juncNames = Array ("warm_up", "sensor1", "onR_merge1", "sensor2",
                                   "sensor3", "onR_merge2", "sensor4", "sensor5")
    for i <- junc.indices do
        junc(i) = new Junction (juncNames(i), xy = aniCoords_Main(i), nt = nt, nl = numLanes)
        debug ("init", s"  junc($i) = ${juncNames(i)} at ${aniCoords_Main(i)}")

    // Ramp sensors
    debug ("init", s"Creating ${aniCoords_Ramp.length} ramp sensors")
    private val ramp_sensors = Array.ofDim [Junction] (aniCoords_Ramp.length)
    for i <- ramp_sensors.indices do
        ramp_sensors(i) = new Junction (s"ramp${i + 1}", xy = aniCoords_Ramp(i), nt = nt, nl = numLanes)
        debug ("init", s"  ramp_sensor($i) = ramp${i + 1} at ${aniCoords_Ramp(i)}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Route and sources
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val intermediateJunc = junc.slice (1, junc.length - 1)
    debug ("init", s"Creating route with ${intermediateJunc.length} intermediate junctions")
    private val route = Route ("Rte", numLanes, intermediateJunc, junc(0), junc.last, motion)
    debug ("init", s"Route created with ${route.pathway.length} pathways")

    // Sinks
    private val (x0, y0) = (aniCoords_Main.last._1 - 100.0, aniCoords_Main.last._2 - 100.0)
    private val sinks = Sink.group ((x0.toInt, y0.toInt),
        ("sinkMain", (0, 0))
    )

    // Lane totals from arrival sources
    private val laneTotalsML = Array.tabulate (numLanes) { l =>
        mainlineArrivalSources(l).getTotalVehicles (l)
    }

    // Mainline sources - builder creates based on numLanes (no hardcoded lane count)
    debug ("init", "Building mainline sources...")
    private val mainlineSources: List [VSource] = builder.buildMainlineSources (
        this, () => Car (), route, "vsrcML", iArrivalRV
    )
    debug ("init", s"Created ${mainlineSources.length} mainline sources")

    // Ramp sources - builder creates based on numRamps, uses per-ramp distribution from config
    debug ("init", "Building ramp sources...")
    private val rampSources: List [VSource] = builder.buildRampSources (
        this, () => Car (), centerPos, offsets
    )
    debug ("init", s"Created ${rampSources.length} ramp sources")

    private val sources: List [VSource] = mainlineSources ++ rampSources

    // Ramps - builder encapsulates merge info (no hardcoded source indices)
    debug ("init", "Building ramps...")
    private val ramps: Array [Ramp] = builder.buildRamps (motion, rampSources, ramp_sensors)
    debug ("init", s"Created ${ramps.length} ramps")

    // Register components
    debug ("init", "Registering components with model...")
    addComponents (sources, junc.toList ++ ramp_sensors.toList, sinks, ramps.toList)
    route.pathway.foreach (addComponent (_))
    debug ("init", "Component registration complete")


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Car entity
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val highway_length = junc.length - 1

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            if subtype < numLanes then
                // Mainline entry - subtype 0 until numLanes
                laneID = subtype
                //debug ("Car.act", s"$me MAINLINE ENTRY: lane=$laneID, subtype=$subtype")
                val carAhead = route.pathway(laneID).getLast
                route.pathway(laneID).addToAlist (this, carAhead)
                junc(0).jump ()
                driveHighway ()
            else
                // Ramp entry - subtype numLanes, numLanes+1, ...
                laneID = numLanes - 1
                //debug ("Car.act", s"$me RAMP ENTRY: rampIdx=${subtype - numLanes}, merging to lane=$laneID")
                val onRamp = ramps(subtype - numLanes)
                driveRamp (onRamp)
                driveHighway ()
        end act

        private def driveHighway (): Unit =
            val joinSeg = if subtype < numLanes then 0 else rampJoinSeg(subtype - numLanes)
            //debug ("driveHighway", s"$me starting at seg=$joinSeg, lane=$laneID")

            if subtype >= numLanes then
                val carAhead = route.pathway(laneID).seg(joinSeg).getLast
                route.pathway(laneID).addToAlist (this, carAhead)
                junc(joinSeg).jump ()
            end if

            cfor (joinSeg, highway_length) { seg =>
                // Deterministic lane change: if car ahead is slow, change lanes
                val carAhead = getCarAhead(this)
                if carAhead != null && carAhead.velocity < 0.1 * vmax then
                    val target = if laneID > 0 then laneID - 1 else laneID + 1
                    route.changeLane(laneID, target, this, seg)
                end if
                
                route.pathway(laneID).seg(seg).move ()
                if junc(seg + 1).name.startsWith ("sensor") then
                    junc(seg + 1).jump ()
            }

            //debug ("driveHighway", s"$me EXITING at ${sinks.head.name}, lane=$laneID")
            route.pathway(laneID).removeFromAlist (this)
            sinks.head.leave ()
        end driveHighway

        private def driveRamp (comp: Component): Unit =
            val r = comp.asInstanceOf [Ramp]
            //debug ("driveRamp", s"$me on ramp ${r.name}, mode=${r.mode}")
            val carAhead = r.getLast
            r.addToAlist (this, carAhead)

            if r.mode == RampMode.On then
                r.lane.move ()
                r.to.asInstanceOf [Junction].jump ()
            end if

            r.removeFromAlist (this)
        end driveRamp

    end Car

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // RowTimeLoader data access methods - delegates to companion object helpers
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get number of time rows in loaded data. */
    def getDataDimension: Int = nt

    /** Get inter-arrival time (mu) - delegates to RowTimeLoader companion object. */
    def getMuForSource(sourceIdx: Int): VectorD =
        RowTimeLoader.getMuForSourceDefault(mainlineArrivalSources, rampArrivalSources, 
                                             numLanes, nt, sourceIdx)

    /** Get speed matrix - delegates to RowTimeLoader companion object. */
    def getSpeedMatrix(): MatrixD =
        RowTimeLoader.getSpeedMatrixDefault(demand)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Public accessors for calibration
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get junctions array for calibration/validation */
    def getJunctions: Array[Junction] = junc

    /** Get PeMS-to-junction mapping indices */
    def getPemsSensorIndices: Array[Int] = pemsToJunc

    /** Get demand configuration */
    def getDemandConfig = demand

    /** Get dynamics configuration (car-following model, solver, params) */
    def getDynamicsConfig = dynamics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Finish - use SimulationReport for validation
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    override def fini (rep: Int): Unit =
//        println ("\n" + "=" * 70)
//        println ("CalRoute101_3 SIMULATION COMPLETE")
//        println ("=" * 70)

        // Use new SimulationReport for validation
        val report = SimulationReport.fromJunctions (junc, pemsToJunc, demand)
//        report.report ()
//
//        println ("\nFitness (for optimization): " + report.fitness ())
        super.fini (rep)
    end fini

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Note: simulate() and waitFinished() are called from CalibrationFramework
    // For standalone run, use @main def runCalRoute101_3
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    // simulate ()
    // waitFinished ()
//    Model.shutdown ()

end CalRoute101_3

