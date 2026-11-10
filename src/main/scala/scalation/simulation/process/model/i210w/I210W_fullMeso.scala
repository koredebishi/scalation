
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Mon Nov 9 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    I210W_fullMeso: I-210 W Pasadena, every segment and ramp a meso
 *           link.  Copied from RedwoodCreek101_fullMeso and the I210W micro
 *           model, generalised to three off-ramps.
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
import scalation.simulation.process.meso.{RampMeso, RampMesoMode}
import scalation.simulation.process.model.calroute101.{CalRouteReadings, SimulationReport, TrafficModelBuilder}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run the I210W full-meso model once on the default day.
 *  > runMain scalation.simulation.process.model.i210w.runI210W_fullMeso
 */
@main def runI210W_fullMeso (): Unit =
    val model = new I210W_fullMeso ()
    model.simulate ()
    model.waitFinished ()
    Model.shutdown ()
end runI210W_fullMeso

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `I210W_fullMeso` class simulates the Pasadena stretch with meso links on
 *  every segment.  Only the meso links are registered; nothing drives the micro
 *  pathways.  At each diverge an outer-lane car leaves with the PeMS exit
 *  fraction of that ramp.
 *  @param name       the name of the model
 *  @param reps       the number of replications
 *  @param animating  whether to animate
 *  @param aniRatio   animation speed ratio
 *  @param day        the PeMS data folder (one of `I210WConfig.DAYS`)
 */
class I210W_fullMeso (name: String = "I210W_fullMeso", reps: Int = 1,
                      animating: Boolean = true, aniRatio: Double = 500.0,
                      val day: String = I210WConfig.DEFAULT_DAY)
      extends Model (name, reps, animating, aniRatio)
         with RowTimeLoader
         with CalRouteReadings:

    private val debug = debugf ("I210W_fullMeso", false)

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

    if animating && dgAni != null then
        dgAni.setHudModelName (dynamics.carFollowing.toString + " meso")

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
    // Route: every segment meso, real lengths; sinks, sources, meso ramps
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val flowDiagram = new IdmFlux (v0 = Vehicle.vmax, tau = Vehicle.T,
                                           s0 = Vehicle.s, len = Vehicle.len, delta = Vehicle.del)

    private val intermediateJunc = junc.slice (1, junc.length - 1)
    private val route = Route ("Rte", numLanes, intermediateJunc, junc(0), junc.last, motion,
                               mesoSegs = (0 until numSegments).toSet, flowDiagram = Some (flowDiagram))

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

    // Meso ramps: capacity and free speed from the same diagram as the mainline links
    private val ramps: Array [RampMeso] = Array.tabulate (rampSources.length) { r =>
        new RampMeso (s"onRamp${r + 1}", rampSources(r), ramp_sensors(r), 0.0,
                      Vehicle.vmax, flowDiagram.q_max * 3600.0, RampMesoMode.On,
                      bend = 0.0, offset = 0.0, flowDiagram = flowDiagram) }
    private val offRamps: Array [RampMeso] = Array.tabulate (offJuncs.length) { r =>
        new RampMeso (s"offRamp${r + 1}", offRamp_sensors(r), sinks(r + 1), 0.0,
                      Vehicle.vmax, flowDiagram.q_max * 3600.0, RampMesoMode.Off,
                      bend = 0.0, offset = 0.0, flowDiagram = flowDiagram) }

    addComponents (sources, junc.toList ++ ramp_sensors.toList ++ offRamp_sensors.toList, sinks,
                   ramps.toList ++ offRamps.toList, route.mesoLink.flatten.filter (_ != null).toList)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Car entity
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val highway_length = junc.length - 1

    case class Car () extends Vehicle ("c", this):

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

        /** Drive from `joinSeg` to the sink, leaving at an off-ramp with its exit fraction. */
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
                    driveRamp (offRamps(r))                      // leave the mainline at this diverge
                    sinks(r + 1).leave ()
                    exited = true
                else
                    seg = diverge
                    r  += 1
                end if
            end while
            if ! exited then
                driveSegments (seg, highway_length)
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
                r.drive (this)                                   // drives the ramp link; marks fromRamp
                r.to.asInstanceOf [Junction].jump ()
            else                                                 // off-ramp: count the exit at the diverge, then drive off
                r.from.asInstanceOf [Junction].jump ()
                r.drive (this)
            end if
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

end I210W_fullMeso
