//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Oct 25 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    RedwoodCreek101_fullMeso: Redwood Creek corridor, every mainline
 *           segment and all three ramps mesoscopic.  Copied from
 *           CalRoute101_3_fullMeso and modified (5 lanes, off-ramp).
 */

package scalation
package simulation
package process
package model
package redwood101

import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.{Sharp, Uniform}
import scalation.dynamics.IdmFlux
import scalation.simulation.process.*
import scalation.simulation.process.meso.{MesoVTransport, RampMeso, RampMesoMode}
import scalation.simulation.process.config.{CarFollowingModel, DynamicsConfig, PeMSDataHelper, PeMSDemand}
import scalation.simulation.process.model.calroute101.{CalRouteReadings, TrafficModelBuilder}
import scalation.{cfor, debugf}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run RedwoodCreek101_fullMeso simulation.
 *  > runMain scalation.simulation.process.model.redwood101.runRedwoodCreek101_fullMeso
 */
@main def runRedwoodCreek101_fullMeso (): Unit =
    val model = new RedwoodCreek101_fullMeso ()
    model.simulate ()
    model.waitFinished ()
    Model.shutdown ()
end runRedwoodCreek101_fullMeso


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RedwoodCreek101_fullMeso` class: same corridor, demand, exit fraction and
 *  junction/sensor jumps as `RedwoodCreek101`, with every mainline segment and
 *  all three ramps mesoscopic.
 *  @param name       the name of the model
 *  @param reps       the number of replications
 *  @param animating  whether to animate
 *  @param aniRatio   animation speed ratio
 */
class RedwoodCreek101_fullMeso (name: String = "RedwoodCreek101_fullMeso", reps: Int = 1,
                                animating: Boolean = true, aniRatio: Double = 500.0)
      extends Model (name, reps, animating, aniRatio)
         with RowTimeLoader
         with CalRouteReadings:

    private val debug = debugf ("RedwoodCreek101_fullMeso", false)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Configuration
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val builder = new TrafficModelBuilder (RedwoodCreekConfig.network,
                                                   RedwoodCreekConfig.demand (),
                                                   DynamicsConfig.idmDefault)
    builder.summary ()

    // Extract from builder
    private val network  = builder.getNetwork                  // NetworkConfig: topology (segments, lanes, ramps, sensors)
    private val demand   = builder.getDemand                   // DemandConfig: PeMS arrivals (anchor file, distribution)
    private val dynamics = builder.getDynamics                 // DynamicsConfig: IDM/Gipps, ODE solver, cfParams

    private val numLanes    = builder.numLanes                 // 5 lanes
    private val numSegments = builder.numSegments              // 8 segments
    private val rampJoinSeg = builder.rampJoinSegments         // [5, 7] - where on-ramps merge
    private val pemsToJunc  = builder.pemsSensorIndices        // [1, 2, 4, 6, 8] - junctions with PeMS sensors
    private val offJunc     = network.ramps.find (_.mode == config.RampMode.Off).get.joinSegment   // 3 - offR_merge

    // Time intervals - 48 for 15-min intervals over 12 hours (6am-6pm)
    private val nt = 48

    // Arrival sources from builder
    private val mainlineArrivalSources = builder.mainlineArrivalSources   // 5 sources, one per lane
    private val rampArrivalSources     = builder.rampArrivalSources       // 2 sources, one per on-ramp

    // Off-ramp exit fraction per 15-min row (derived from PeMS FR and S2 counts)
    private val exitFraction = RedwoodCreekConfig.exitFraction (demand)

    debug ("init", s"numLanes=$numLanes, numSegments=$numSegments, nt=$nt, offJunc=$offJunc")
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

    // Push dynamics model name to animation HUD
    if animating && dgAni != null then
        dgAni.setHudModelName (dynamics.carFollowing.toString)

    private val rand = Uniform (0.0, 1.0)
    private val iArrivalRV      = mainlineArrivalSources(0).getDistribution

    setTime (nt * rowTime)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Animation coordinates and junction setup
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val (w, h) = (3000, 2000)
    private val aniCoords_Main = RedwoodCreekConfig.getMainlineCoordinates ((w, h))
    private val aniCoords_Ramp = RedwoodCreekConfig.getRampCoordinates ((w, h))
    private val (centerPos, offsets) = RedwoodCreekConfig.getVSourceCenterAndOffsets ((w, h))

    // Junctions
    debug ("init", s"Creating ${aniCoords_Main.length} junctions")
    private val junc = Array.ofDim [Junction] (aniCoords_Main.length)
    private val juncNames = Array ("warm_up", "sensor1", "sensor2", "offR_merge", "sensor3",
                                   "onR_merge1", "sensor4", "onR_merge2", "sensor5")
    // Mainline junctions (and the diverge) hold 0 s; ramp merges keep the default 1 s until gap acceptance exists.
    for i <- junc.indices do
        if juncNames(i).startsWith ("onR_merge") then
            junc(i) = new Junction (juncNames(i), xy = aniCoords_Main(i), nt = nt, nl = numLanes)
        else
            junc(i) = new Junction (juncNames(i), Sharp (0.0), aniCoords_Main(i), nt, numLanes)
        end if
        debug ("init", s"  junc($i) = ${juncNames(i)} at ${aniCoords_Main(i)}")

    // On-ramp sensors
    debug ("init", s"Creating ${aniCoords_Ramp.length} ramp sensors")
    private val ramp_sensors = Array.ofDim [Junction] (aniCoords_Ramp.length)
    for i <- ramp_sensors.indices do
        ramp_sensors(i) = new Junction (s"ramp${i + 1}", xy = aniCoords_Ramp(i), nt = nt, nl = numLanes)
        debug ("init", s"  ramp_sensor($i) = ramp${i + 1} at ${aniCoords_Ramp(i)}")

    // Off-ramp sensor: on the outer lane at offR_merge, where the off-ramp starts
    private val offRamp_sensor = new Junction ("offR_sensor", Sharp (0.0), RedwoodCreekConfig.getOffRampStart ((w, h)), nt, numLanes)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Route and sources
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    // Every mainline section is a mesoscopic link; the Route builds and owns the meso lanes
    // (same ends, drawn offset and length as its micro lanes).  Only the meso links are
    // registered: nothing drives the micro pathways.
    private val flowDiagram = new IdmFlux (v0 = Vehicle.vmax, tau = Vehicle.T,
                                           s0 = Vehicle.s, len = Vehicle.len, delta = Vehicle.del)
    private val intermediateJunc = junc.slice (1, junc.length - 1)
    debug ("init", s"Creating route with ${intermediateJunc.length} intermediate junctions")
    private val route = Route ("Rte", numLanes, intermediateJunc, junc(0), junc.last, motion,
                               mesoSegs = (0 until numSegments).toSet, flowDiagram = Some (flowDiagram))
    debug ("init", s"Route created with ${route.pathway.length} pathways")

    // L5 does not exist from offR_merge to onR_merge1 (segments 3-4): do not draw it there
    private val outer = numLanes - 1
    for s <- offJunc until rampJoinSeg(0) do
        route.pathway(outer).seg(s).visible = false
        if route.mesoLink(outer)(s) != null then route.mesoLink(outer)(s).visible = false

    // Sinks: sinkMain past sensor5, sinkOff at the end of the off-ramp
    private val (x0, y0) = (aniCoords_Main.last._1 - 100.0, aniCoords_Main.last._2 - 100.0)
    private val offEnd   = RedwoodCreekConfig.getOffRampEnd ((w, h))
    private val sinks = Sink.group ((0, 0),
        ("sinkMain", (x0.toInt, y0.toInt)),
        ("sinkOff",  offEnd)
    )

    // Mainline sources - builder creates based on numLanes (no hardcoded lane count)
    debug ("init", "Building mainline sources...")
    private val mainlineSources: List [VSource] = builder.buildMainlineSources (
        this, () => Car (), route, "vsrcML", iArrivalRV
    )
    debug ("init", s"Created ${mainlineSources.length} mainline sources")

    // Ramp sources - builder creates one per on-ramp, uses per-ramp distribution from config
    debug ("init", "Building ramp sources...")
    private val rampSources: List [VSource] = builder.buildRampSources (
        this, () => Car (), centerPos, offsets
    )
    debug ("init", s"Created ${rampSources.length} ramp sources")

    private val sources: List [VSource] = mainlineSources ++ rampSources

    // Ramps - mesoscopic, same endpoints, bend (0) and offset (0) as the micro Ramps,
    // so each link has the same length.  The off-ramp runs from offR_sensor to sinkOff.
    debug ("init", "Building ramps...")
    private val ramps: Array [RampMeso] = Array.tabulate (rampSources.length) { r =>
        new RampMeso (s"onRamp${r + 1}", rampSources(r), ramp_sensors(r), 0.0,
                      Vehicle.vmax, flowDiagram.q_max * 3600.0, RampMesoMode.On,
                      bend = 0.0, offset = 0.0, flowDiagram = flowDiagram)
    }
    private val offRamp = new RampMeso ("offRamp1", offRamp_sensor, sinks(1), 0.0,
                                        Vehicle.vmax, flowDiagram.q_max * 3600.0, RampMesoMode.Off,
                                        bend = 0.0, offset = 0.0, flowDiagram = flowDiagram)
    debug ("init", s"Created ${ramps.length} on-ramps and 1 off-ramp")

    // Register components
    debug ("init", "Registering components with model...")
    addComponents (sources, junc.toList ++ ramp_sensors.toList :+ offRamp_sensor, sinks, ramps.toList :+ offRamp,
                   route.mesoLink.flatten.filter (_ != null).toList)
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
                debug ("Act::: ", s"For $this actor in: lane: ${this.laneID}: LaneID Assigned")
                driveHighway ()
            else
                // Ramp entry - subtype numLanes, numLanes+1, ...
                laneID = numLanes - 1
                debug ("Act::: ", s"For $this actor in: lane: ${this.laneID}: LaneID Assigned")
                val onRamp = ramps(subtype - numLanes)
                driveRamp (onRamp)
                driveHighway ()
        end act

        private def driveHighway (): Unit =
            val joinSeg = if subtype < numLanes then 0 else rampJoinSeg(subtype - numLanes)

            if subtype >= numLanes then junc(joinSeg).jump ()
            else junc(0).jump ()

            // On-ramp cars join downstream of the diverge, so they never meet it
            val divergeSeg = if joinSeg < offJunc then offJunc else highway_length
            driveSegments (joinSeg, divergeSeg)

            // L5 ends at offR_merge: an L5 car exits with share p(k), otherwise it merges into L4
            val inOuterLane = divergeSeg == offJunc && laneID == numLanes - 1
            if inOuterLane && rand.gen < exitFraction (getCurrentRow (director.clock) min (nt - 1)) then
                driveRamp (offRamp)                         // leave the mainline at offR_merge
                sinks(1).leave ()
            else
                if inOuterLane then laneID = numLanes - 2   // through car: enter L4's meso link next
                driveSegments (divergeSeg, highway_length)
                sinks.head.leave ()
            end if
        end driveHighway

        /** Drive meso segments [from, to), counting at each PeMS sensor junction. */
        private def driveSegments (from: Int, to: Int): Unit =
            cfor (from, to) { seg =>
                if seg > 0 then laneID = route.rampLaneMove (this, seg)   // SIMPLE ramp-lane rule (same as micro)
                val link = route.mesoLink(laneID)(seg)
                link.move ()

                if junc(seg + 1).name.startsWith ("sensor") then junc(seg + 1).jump (Some (link.density))
            }
        end driveSegments

        private def driveRamp (r: RampMeso): Unit =
            if r.mode == RampMesoMode.On then
                r.drive (this)                              // drives the ramp link; marks fromRamp
                r.to.asInstanceOf [Junction].jump ()
            else                                            // off-ramp: count the exit at offR_sensor, then drive off
                r.from.asInstanceOf [Junction].jump ()
                r.drive (this)
            end if
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

    /** Anchor-sensor speed matrix -- read once per run with the demand's 5-lane layout. */
    private val anchorSpeed = PeMSDataHelper.loadMainlineSensor (demand, 0, demand.window, demand.layout)._2

    /** Get speed matrix. */
    def getSpeedMatrix(): MatrixD = anchorSpeed

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Public accessors for calibration
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Get junctions array for calibration/validation */
    def getJunctions: Array[Junction] = junc

    /** Get PeMS-to-junction mapping indices */
    def getPemsSensorIndices: Array[Int] = pemsToJunc

    /** Get demand configuration */
    def getDemandConfig: PeMSDemand = demand

    /** Get dynamics configuration (car-following model, solver, params) */
    def getDynamicsConfig: DynamicsConfig = dynamics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Finish
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    override def fini (rep: Int): Unit =
        println (s"exits: sinkOff = ${sinks(1).durationStat.num}, sinkMain = ${sinks.head.durationStat.num}")
        super.fini (rep)
    end fini

end RedwoodCreek101_fullMeso
