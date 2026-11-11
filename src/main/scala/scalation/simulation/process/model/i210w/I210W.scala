
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Mon Nov 9 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    I210W: I-210 Westbound Pasadena corridor, microscopic (5 lanes,
 *           3 on-ramps, 3 off-ramps, 5-minute PeMS bins).  Copied from
 *           RedwoodCreek101 and generalised to several off-ramps.  Segment
 *           lengths are the real GPS distances from `I210WConfig`, not the
 *           drawn lengths.
 */

package scalation
package simulation
package process
package model
package i210w

import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.{Sharp, Uniform}
import scalation.{cfor, debugf}
import scalation.simulation.process.config.{CarFollowingModel, DynamicsConfig, PeMSDataHelper, PeMSDemand}
import scalation.simulation.process.*
import scalation.simulation.process.model.calroute101.{CalRouteReadings, SimulationReport, TrafficModelBuilder}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run the I210W micro model once on the default day.
 *  > runMain scalation.simulation.process.model.i210w.runI210W
 */
@main def runI210W (): Unit =
    val model = new I210W ()
    model.simulate ()
    model.waitFinished ()
    Model.shutdown ()
end runI210W

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `I210W` class simulates the Pasadena stretch of I-210 Westbound.  Same
 *  structure as `RedwoodCreek101` with three off-ramps: at each diverge junction
 *  an outer-lane car leaves with the PeMS exit fraction p_r(k) of that ramp.
 *  @param name       the name of the model
 *  @param reps       the number of replications
 *  @param animating  whether to animate
 *  @param aniRatio   animation speed ratio
 *  @param day        the PeMS data folder (one of `I210WConfig.DAYS`)
 */
class I210W (name: String = "I210W", reps: Int = 1,
             animating: Boolean = true, aniRatio: Double = 500.0,
             val day: String = I210WConfig.DEFAULT_DAY)
      extends Model (name, reps, animating, aniRatio)
         with RowTimeLoader
         with CalRouteReadings:

    private val debug = debugf ("I210W", false)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Configuration
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val builder = new TrafficModelBuilder (I210WConfig.network,
                                                   I210WConfig.demand (day),
                                                   DynamicsConfig.idmDefault)
    builder.summary ()

    private val network  = builder.getNetwork                  // topology (segments, lanes, ramps, sensors)
    private val demand   = builder.getDemand                   // PeMS arrivals for this day
    private val dynamics = builder.getDynamics                 // IDM, ODE solver, cfParams

    private val numLanes    = builder.numLanes                 // 5 lanes
    private val numSegments = builder.numSegments              // 10 segments
    private val rampJoinSeg = builder.rampJoinSegments         // [3, 5, 8]  on-ramp merge junctions
    private val pemsToJunc  = builder.pemsSensorIndices        // [1, 6, 7, 10] junctions with PeMS sensors
    private val offJuncs    = network.ramps.filter (_.mode == config.RampMode.Off).map (_.joinSegment).toArray
                                                               // [2, 4, 9]  off-ramp diverge junctions

    // Time: 205 five-minute rows, 06:00-23:00
    private val nt = I210WConfig.N_BINS
    override val rowTimeSlice: Double = demand.window.binSeconds   // the data's bin: 300 s here; VSource, Recorder and setTime follow it

    private val mainlineArrivalSources = builder.mainlineArrivalSources   // 5 sources, one per lane
    private val rampArrivalSources     = builder.rampArrivalSources       // 3 sources, one per on-ramp

    // Off-ramp exit fraction per row, one vector per off-ramp (network.ramps Off order)
    private val exitFractions = I210WConfig.exitFractions (demand)
    private val outer = numLanes - 1                               // L5, the exit lane

    debug ("init", s"numLanes=$numLanes, numSegments=$numSegments, nt=$nt, rowTimeSlice=$rowTimeSlice")
    debug ("init", s"rampJoinSeg=${rampJoinSeg.mkString (",")}, offJuncs=${offJuncs.mkString (",")}, pemsToJunc=${pemsToJunc.mkString (",")}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Dynamics - from config
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val motion = dynamics.carFollowing match
        case CarFollowingModel.IDM    => IDMDynamics
        case CarFollowingModel.Gipps  => GippsDynamics
        case CarFollowingModel.Krause => KraussDynamics

    motion match
        case IDMDynamics    => IDMDynamics.integratorType = IntegratorType.Ballistic
        case GippsDynamics  => GippsDynamics.integratorType = IntegratorType.Ballistic
        case KraussDynamics => KraussDynamics.integratorType = IntegratorType.Ballistic
        case _              =>

    if animating && dgAni != null then
        dgAni.setHudModelName (dynamics.carFollowing.toString)

    private val rand        = Uniform (0.0, 1.0)
    private val iArrivalRV  = mainlineArrivalSources(0).getDistribution

    setTime (nt * rowTime)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Animation coordinates and junctions
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val (w, h) = (3000, 2000)
    private val aniCoords_Main = I210WConfig.getMainlineCoordinates ((w, h))
    private val aniCoords_Ramp = I210WConfig.getRampCoordinates ((w, h))
    private val (centerPos, offsets) = I210WConfig.getVSourceCenterAndOffsets ((w, h))

    private val juncNames = Array ("warm_up", "sensor1", "offR_diverge1", "onR_merge1", "offR_diverge2",
                                   "onR_merge2", "sensor2", "sensor3", "onR_merge3", "offR_diverge3", "sensor4")
    require (juncNames.length == aniCoords_Main.length, "junction names and coordinates differ in length")

    // Mainline junctions and diverges hold 0 s; on-ramp merges keep the default 1 s until gap acceptance exists.
    private val junc = Array.tabulate (juncNames.length) { i =>
        if juncNames(i).startsWith ("onR_merge") then
            new Junction (juncNames(i), xy = aniCoords_Main(i), nt = nt, nl = numLanes)
        else
            new Junction (juncNames(i), Sharp (0.0), aniCoords_Main(i), nt, numLanes)
    }

    // On-ramp sensors (merge points) and off-ramp sensors (where each off-ramp starts, on the outer lane)
    private val ramp_sensors = Array.tabulate (aniCoords_Ramp.length) { i =>
        new Junction (s"ramp${i + 1}", xy = aniCoords_Ramp(i), nt = nt, nl = numLanes) }
    private val offRamp_sensors = Array.tabulate (offJuncs.length) { r =>
        new Junction (s"offR_sensor${r + 1}", Sharp (0.0), I210WConfig.getOffRampStart ((w, h), r), nt, numLanes) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Route (with real segment lengths), sinks, sources, ramps
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val intermediateJunc = junc.slice (1, junc.length - 1)
    private val route = Route ("Rte", numLanes, intermediateJunc, junc(0), junc.last, motion)

    // Physics runs on the real GPS lengths, not the drawn ones (as NetworkBuilder does for OSM edges).
    for l <- 0 until numLanes; s <- 0 until numSegments do
        route.pathway(l).seg(s).length = I210WConfig.segmentLengthsM(s)
        if route.mesoLink(l)(s) != null then route.mesoLink(l)(s).length = I210WConfig.segmentLengthsM(s)

    private val (x0, y0) = (aniCoords_Main.last._1 - 100.0, aniCoords_Main.last._2 - 100.0)

    private val sinks = Sink.group ((0, 0),
        ("sinkMain", (x0.toInt, y0.toInt)),
        ("sinkOff1", I210WConfig.getOffRampEnd ((w, h), 0)),
        ("sinkOff2", I210WConfig.getOffRampEnd ((w, h), 1)),
        ("sinkOff3", I210WConfig.getOffRampEnd ((w, h), 2)))

    private val mainlineSources: List [VSource] = builder.buildMainlineSources (this, () => Car (), route, "vsrcML", iArrivalRV)
    private val rampSources: List [VSource]     = builder.buildRampSources (this, () => Car (), centerPos, offsets)
    private val sources: List [VSource]         = mainlineSources ++ rampSources

    private val ramps: Array [Ramp] = builder.buildRamps (motion, rampSources, ramp_sensors)
    private val offRamps: Array [Ramp] = Array.tabulate (offJuncs.length) { r =>
        new Ramp (s"offRamp${r + 1}", offRamp_sensors(r), sinks(r + 1), motion, RampMode.Off, false, 0.0, 0.0) }

    addComponents (sources, junc.toList ++ ramp_sensors.toList ++ offRamp_sensors.toList, sinks,
                   ramps.toList ++ offRamps.toList)
    route.pathway.foreach (addComponent (_))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Car entity
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val highway_length = junc.length - 1               // number of segments

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            if subtype < numLanes then                           // mainline entry, subtype = lane
                laneID = subtype
                junc(0).jump ()
                val carAhead = route.pathway(laneID).getLast
                route.pathway(laneID).joinAt (this, carAhead, 0)
                driveHighway (0)
            else                                                 // on-ramp entry
                laneID = numLanes - 1
                val onRamp = ramps(subtype - numLanes)
                driveRamp (onRamp)
                driveHighway (rampJoinSeg(subtype - numLanes))
        end act

        /** Drive from `joinSeg` to the sink, leaving at an off-ramp with its exit fraction.
         *  A ramp car first merges at its junction.  Diverges upstream of `joinSeg` are
         *  never met.  All five lanes run the whole stretch, so a through car in L5 stays in L5.
         */
        private def driveHighway (joinSeg: Int): Unit =
            if joinSeg > 0 then
                junc(joinSeg).jump ()                            // wait at the merge junction first
                val carAhead = route.pathway(laneID).mergeLeaderAt (joinSeg)
                route.pathway(laneID).joinAt (this, carAhead, joinSeg)
            end if
            var seg    = joinSeg
            var exited = false
            var r      = offJuncs.indexWhere (_ > joinSeg)       // first diverge ahead of the join (-1 if none)
            while ! exited && r >= 0 && r < offJuncs.length do
                val diverge = offJuncs(r)
                driveSegments (seg, diverge)
                val row = getCurrentRow (director.clock) min (nt - 1)
                if laneID == outer && rand.gen < exitFractions(r)(row) then
                    route.pathway(laneID).removeFromAlist (this)   // leave the mainline at this diverge
                    driveRamp (offRamps(r))
                    sinks(r + 1).leave ()
                    exited = true
                else
                    seg = diverge
                    r  += 1
                end if
            end while
            if ! exited then
                driveSegments (seg, highway_length)
                route.pathway(laneID).removeFromAlist (this)     // leave the mainline at the end of the corridor
                sinks.head.leave ()
            end if
        end driveHighway

        /** Drive mainline segments [from, to), counting at each PeMS sensor junction. */
        private def driveSegments (from: Int, to: Int): Unit =
            cfor (from, to) { seg =>
                if seg == 0 then route.pathway(laneID).seg(seg).move ()   // warm-up: keep the PeMS lane up to sensor 1
                else route.driveSegmentSimple (this, seg)                 // SIMPLE: ramp-lane rule (Route.rampLaneMove)
                if junc(seg + 1).name.startsWith ("sensor") then
                    junc(seg + 1).jump ()
            }
        end driveSegments

        private def driveRamp (comp: Component): Unit =
            val r = comp.asInstanceOf [Ramp]
            val carAhead = r.getLast
            r.addToAlist (this, carAhead)
            if r.mode == RampMode.On then
                r.lane.move ()
                r.to.asInstanceOf [Junction].jump ()
            else                                                 // off-ramp: count the exit at the diverge, then drive off
                r.from.asInstanceOf [Junction].jump ()
                r.lane.move ()
            end if
            r.removeFromAlist (this)
        end driveRamp

    end Car

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // RowTimeLoader data access
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def getDataDimension: Int = nt

    def getMuForSource (sourceIdx: Int): VectorD =
        RowTimeLoader.getMuForSourceDefault (mainlineArrivalSources, rampArrivalSources, numLanes, nt, sourceIdx)

    private val anchorSpeed = PeMSDataHelper.loadMainlineSensor (demand, 0, demand.window, demand.layout)._2

    def getSpeedMatrix (): MatrixD = anchorSpeed

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // CalRouteReadings
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def getJunctions: Array [Junction]   = junc
    def getPemsSensorIndices: Array [Int] = pemsToJunc
    def getDemandConfig: PeMSDemand       = demand
    def getDynamicsConfig: DynamicsConfig = dynamics

    override def fini (rep: Int): Unit =
        val report = SimulationReport.fromJunctions (junc, pemsToJunc, demand)
        report.report ()
        println (s"exits: sinkMain = ${sinks.head.durationStat.num}, " +
                 (1 to offJuncs.length).map (r => s"sinkOff$r = ${sinks(r).durationStat.num}").mkString (", "))
        super.fini (rep)
    end fini

end I210W
