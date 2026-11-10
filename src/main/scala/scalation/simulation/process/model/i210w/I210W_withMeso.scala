
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Mon Nov 9 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    I210W_withMeso: I-210 W Pasadena, mixed resolution.  Copied from
 *           RedwoodCreek101_withMeso and the I210W micro model.  The segments
 *           just before each diverge and just after each merge stay micro;
 *           the rest run as meso links with the IDM-derived diagram.
 */

package scalation
package simulation
package process
package model
package i210w

import scalation.dynamics.IdmFlux
import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.{Sharp, Uniform}
import scalation.{cfor, debugf}
import scalation.simulation.process.config.{CarFollowingModel, DynamicsConfig, PeMSDataHelper, PeMSDemand}
import scalation.simulation.process.*
import scalation.simulation.process.model.calroute101.{CalRouteReadings, SimulationReport, TrafficModelBuilder}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run the I210W mixed model once on the default day.
 *  > runMain scalation.simulation.process.model.i210w.runI210W_withMeso
 */
@main def runI210W_withMeso (): Unit =
    val model = new I210W_withMeso ()
    model.simulate ()
    model.waitFinished ()
    Model.shutdown ()
end runI210W_withMeso

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `I210W_withMeso` class simulates the Pasadena stretch with micro segments
 *  at the ramps and meso links elsewhere.
 *
 *  Segments: 0 warm_up-S1, 1 S1-SMoff, 2 SMoff-SMon, 3 SMon-SGoff, 4 SGoff-SGon,
 *  5 SGon-S2, 6 S2-S3, 7 S3-ALTon, 8 ALTon-ALLoff, 9 ALLoff-S4.
 *  Micro: 1, 3, 5, 8 (before a diverge or after a merge, as RedwoodCreek101_withMeso).
 *  Meso:  0, 2, 4, 6, 7, 9.  First partition; revisit after the micro run.
 *  @param name       the name of the model
 *  @param reps       the number of replications
 *  @param animating  whether to animate
 *  @param aniRatio   animation speed ratio
 *  @param day        the PeMS data folder (one of `I210WConfig.DAYS`)
 */
class I210W_withMeso (name: String = "I210W_withMeso", reps: Int = 1,
                      animating: Boolean = true, aniRatio: Double = 500.0,
                      val day: String = I210WConfig.DEFAULT_DAY)
      extends Model (name, reps, animating, aniRatio)
         with RowTimeLoader
         with CalRouteReadings:

    private val debug = debugf ("I210W_withMeso", false)

    /** Segments that run as meso links. */
    val MESO_SEGS: Set [Int] = Set (0, 2, 4, 6, 7, 9)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Configuration (as I210W)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val builder = new TrafficModelBuilder (I210WConfig.network,
                                                   I210WConfig.demand (day),
                                                   DynamicsConfig.idmDefault)
    builder.summary ()

    private val network  = builder.getNetwork
    private val demand   = builder.getDemand
    private val dynamics = builder.getDynamics

    private val numLanes    = builder.numLanes
    private val numSegments = builder.numSegments
    private val rampJoinSeg = builder.rampJoinSegments         // [3, 5, 8]
    private val pemsToJunc  = builder.pemsSensorIndices        // [1, 6, 7, 10]
    private val offJuncs    = network.ramps.filter (_.mode == config.RampMode.Off).map (_.joinSegment).toArray   // [2, 4, 9]

    private val nt = I210WConfig.N_BINS
    override val rowTimeSlice: Double = demand.window.binSeconds   // the data's bin: 300 s here; VSource, Recorder and setTime follow it

    private val mainlineArrivalSources = builder.mainlineArrivalSources
    private val rampArrivalSources     = builder.rampArrivalSources

    private val exitFractions = I210WConfig.exitFractions (demand)
    private val outer = numLanes - 1

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
        dgAni.setHudModelName (dynamics.carFollowing.toString + " mixed")

    private val rand        = Uniform (0.0, 1.0)
    private val iArrivalRV  = mainlineArrivalSources(0).getDistribution

    setTime (nt * rowTime)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Junctions (as I210W)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val (w, h) = (3000, 2000)
    private val aniCoords_Main = I210WConfig.getMainlineCoordinates ((w, h))
    private val aniCoords_Ramp = I210WConfig.getRampCoordinates ((w, h))
    private val (centerPos, offsets) = I210WConfig.getVSourceCenterAndOffsets ((w, h))

    private val juncNames = Array ("warm_up", "sensor1", "offR_diverge1", "onR_merge1", "offR_diverge2",
                                   "onR_merge2", "sensor2", "sensor3", "onR_merge3", "offR_diverge3", "sensor4")
    require (juncNames.length == aniCoords_Main.length, "junction names and coordinates differ in length")

    private val junc = Array.tabulate (juncNames.length) { i =>
        if juncNames(i).startsWith ("onR_merge") then
            new Junction (juncNames(i), xy = aniCoords_Main(i), nt = nt, nl = numLanes)
        else
            new Junction (juncNames(i), Sharp (0.0), aniCoords_Main(i), nt, numLanes)
    }
    private val ramp_sensors = Array.tabulate (aniCoords_Ramp.length) { i =>
        new Junction (s"ramp${i + 1}", xy = aniCoords_Ramp(i), nt = nt, nl = numLanes) }
    private val offRamp_sensors = Array.tabulate (offJuncs.length) { r =>
        new Junction (s"offR_sensor${r + 1}", Sharp (0.0), I210WConfig.getOffRampStart ((w, h), r), nt, numLanes) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Route with meso segments, real lengths, sinks, sources, ramps
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    // The meso links use the IDM diagram of the micro vehicles' own constants (one calibration).
    private val flowDiagram = new IdmFlux (v0 = Vehicle.vmax, tau = Vehicle.T,
                                           s0 = Vehicle.s, len = Vehicle.len, delta = Vehicle.del)

    private val intermediateJunc = junc.slice (1, junc.length - 1)
    private val route = Route ("Rte", numLanes, intermediateJunc, junc(0), junc.last, motion,
                               mesoSegs = MESO_SEGS, flowDiagram = Some (flowDiagram))

    // Physics runs on the real GPS lengths, micro lanes and meso links alike.
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

    // Ramps stay micro: they feed or leave micro segments
    private val ramps: Array [Ramp] = builder.buildRamps (motion, rampSources, ramp_sensors)
    private val offRamps: Array [Ramp] = Array.tabulate (offJuncs.length) { r =>
        new Ramp (s"offRamp${r + 1}", offRamp_sensors(r), sinks(r + 1), motion, RampMode.Off, false, 0.0, 0.0) }

    addComponents (sources, junc.toList ++ ramp_sensors.toList ++ offRamp_sensors.toList, sinks,
                   ramps.toList ++ offRamps.toList, route.mesoLink.flatten.filter (_ != null).toList)
    route.pathway.foreach (addComponent (_))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Car entity
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val highway_length = junc.length - 1

    case class Car () extends Vehicle ("c", this):

        private var inMicro = false                          // true while the car is in a micro pathway

        override def act (): Unit =
            if subtype < numLanes then                           // mainline entry, subtype = lane
                laneID = subtype
                junc(0).jump ()
                driveHighway (0)
            else                                                 // on-ramp entry
                laneID = numLanes - 1
                val onRamp = ramps(subtype - numLanes)
                driveRamp (onRamp)
                driveHighway (rampJoinSeg(subtype - numLanes))
        end act

        /** Drive from `joinSeg` to the sink, leaving at an off-ramp with its exit fraction.
         *  `driveSegments` joins the micro pathway when a micro segment comes next.
         */
        private def driveHighway (joinSeg: Int): Unit =
            if joinSeg > 0 then junc(joinSeg).jump ()            // wait at the merge junction first
            var seg    = joinSeg
            var exited = false
            var r      = offJuncs.indexWhere (_ > joinSeg)
            while ! exited && r >= 0 && r < offJuncs.length do
                val diverge = offJuncs(r)
                driveSegments (seg, diverge)
                val row = getCurrentRow (director.clock) min (nt - 1)
                if laneID == outer && rand.gen < exitFractions(r)(row) then
                    leaveMicro ()                                // leave the mainline at this diverge
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
                leaveMicro ()
                sinks.head.leave ()
            end if
        end driveHighway

        /** Drive segments [from, to): meso links or micro pathway, counting at each PeMS sensor junction. */
        private def driveSegments (from: Int, to: Int): Unit =
            cfor (from, to) { seg =>
                if route.mesoSegs.contains (seg) then            // meso link for this segment
                    leaveMicro ()                                // leave the micro lane BEFORE choosing a lane
                    if seg > 0 then laneID = route.rampLaneMove (this, seg)   // SIMPLE ramp-lane rule (same as micro)
                    route.mesoLink(laneID)(seg).move ()
                else
                    if ! inMicro then
                        val carAhead = route.pathway(laneID).mergeLeaderAt (seg)
                        route.pathway(laneID).joinAt (this, carAhead, seg)
                        inMicro = true
                    end if
                    route.driveSegmentSimple (this, seg)         // SIMPLE: ramp-lane rule (Route.rampLaneMove)
                end if
                if junc(seg + 1).name.startsWith ("sensor") then
                    if route.mesoSegs.contains (seg) then
                        junc(seg + 1).jump (Some (route.mesoLink(laneID)(seg).density))
                    else junc(seg + 1).jump ()
            }
        end driveSegments

        /** Leave the micro pathway if the car is in one. */
        private def leaveMicro (): Unit =
            if inMicro then
                route.pathway(laneID).removeFromAlist (this)
                inMicro = false
            end if
        end leaveMicro

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
    // RowTimeLoader data access and CalRouteReadings (as I210W)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def getDataDimension: Int = nt

    def getMuForSource (sourceIdx: Int): VectorD =
        RowTimeLoader.getMuForSourceDefault (mainlineArrivalSources, rampArrivalSources, numLanes, nt, sourceIdx)

    private val anchorSpeed = PeMSDataHelper.loadMainlineSensor (demand, 0, demand.window, demand.layout)._2

    def getSpeedMatrix (): MatrixD = anchorSpeed

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

end I210W_withMeso
