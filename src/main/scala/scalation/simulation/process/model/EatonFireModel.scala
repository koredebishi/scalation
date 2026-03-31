//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sat Mar 22 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    EatonFireModel: I-210 WB + SR-134 WB Dual-Corridor Traffic Model
 *
 *  Uses CorridorBuilder to construct topology from MultiCorridorConfig.
 *  Both corridors share one coordinate frame so they appear in correct
 *  spatial relationship in the animation.
 *
 *  @see config-layer-standard.md Section 4c, 7
 */

package scalation
package simulation
package process
package model

import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.{Exponential, Uniform}
import scalation.simulation.process.{IntegratorType, IDMDynamics}
import scalation.simulation.process.config.{AggregatedDemand, CorridorLayout, MultiCorridorConfig, PeMSDemand}
import scalation.simulation.process.builder.{CorridorBuilder, BuiltNetwork}
import scalation.simulation.process.arrival.{ArrivalSource, AggregatedArrivalSource, AggregatedRampArrivalSource}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run the EatonFireModel simulation.
 *  > runMain scalation.simulation.process.model.runEatonFireModel
 */
@main def runEatonFireModel (): Unit = new EatonFireModel (synthetic = true)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EatonFireModel` class is a dual-corridor traffic simulation model
 *  for I-210 Westbound + SR-134 Westbound (Eaton fire evacuation).
 *
 *  Both corridors share one animation coordinate frame.
 *  When synthetic=true, uses fixed vehicle counts (100 mainline, 50 ramps).
 *  When synthetic=false, loads mainline from PeMSDemand (anchor CSV),
 *  ramps from AggregatedDemand (aggregated OR CSV).
 *
 *  Subtype encoding:
 *    0 .. numLanes210-1                           = I-210 mainline lanes
 *    numLanes210 .. numLanes210+nOnRamps210-1     = I-210 on-ramps
 *    SR134_BASE+numLanes134 .. ...                = SR-134 on-ramps
 *  Note: SR-134 has NO mainline VSources.  All SR-134 mainline traffic
 *        enters via the FF connector from I-210 at the Pasadena interchange.
 *
 *  @param synthetic  if true, use fixed counts (100/50); if false, load PeMS data
 */
class EatonFireModel (name: String = "EatonFireModel", reps: Int = 1,
                      animating: Boolean = true, aniRatio: Double = 500.0,
                      synthetic: Boolean = true)
      extends Model (name, reps, animating, aniRatio)
         with RowTimeLoader:

    private val debug = debugf ("EatonFireModel", true)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Constants
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val SR134_BASE = 100                               // subtype offset for SR-134
    private val nt         = 73                                // 73 × 5-min = 6h5m (17:00–23:00 inclusive)

    // Override RowTimeLoader defaults for 5-min Eaton bins (must precede setTime)
    rowTime = 5.0 * MINUTE                                     // 300 s (overrides 15-min default)
    override val rowTimeSlice: Double = 5.0 * MINUTE           // 300 s per bin

    override def nextRow (clock: Double): Unit =
        if clock >= rowTime then
            curRow += 1
            rowTime += 5.0 * MINUTE                            // 5-min increment (not 15)
        end if
    end nextRow

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 1: Dynamics (must be configured BEFORE builder call)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val motion = IDMDynamics
    IDMDynamics.integratorType = IntegratorType.Ballistic
    private val iArrivalRV = Exponential (MINUTE / 10.0)  // no need : We use getDistribution from ArrivalSource for synthetic vs. aggregated demand
    private val rand       = Uniform (0.0, 1.0)            // for FF split ratio decisions
    setTime (nt * rowTime)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 1b: PeMS demand configs
    //          I-210 mainline: cleaned anchor sensor CSV (same pattern as CalRoute101_3)
    //          I-210 ramps: aggregated OR CSV (multiple ramp stations in one file)
    //          SR-134 ramps: aggregated OR CSV
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val pems210     = PeMSDemand.I210_WB_Anchor ()       // anchor CSV → PeMSArrivalSource
    private val anchorSpeed = RowTimeLoader.getSpeedMatrixFromFile (
        pems210.dataDir + "/" + pems210.mainline.anchorFile, pems210.window, pems210.layout)
    debug ("init", s"Anchor speed: ${anchorSpeed.dim} rows × ${anchorSpeed.dim2} lanes (m/s)")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 2: Build topology from config using CorridorBuilder
    //         Replaces manual junction/route/sink/FF creation (old Steps 3-9a)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val multiConfig = MultiCorridorConfig.EatonFire_WB ()
    private val net         = CorridorBuilder.buildMulti (multiConfig, motion, nt)
    private val b210        = net.corridors ("I-210-W")
    private val b134        = net.corridors ("SR-134-W")

    // Corridor summaries
    CorridorBuilder.summary ("I-210-W", b210)
    CorridorBuilder.summary ("SR-134-W", b134)

    // Convenience aliases for Car.act()
    private val numLanes210    = b210.numLanes
    private val numSegments210 = b210.numSegments
    private val numLanes134    = b134.numLanes
    private val route210       = b210.route
    private val route134       = b134.route
    private val junc210        = b210.junctions
    private val junc134        = b134.junctions
    private val sinks210       = b210.sinks
    private val sinks134       = b134.sinks
    private val hwLen210       = b210.hwLen
    private val hwLen134       = b134.hwLen
    private val rampJoinSeg210 = b210.rampJoinSegs
    private val rampJoinSeg134 = b134.rampJoinSegs
    private val nOnRamps210    = b210.rampSensors.length
    private val nOnRamps134    = b134.rampSensors.length

    // FF connector (built by CorridorBuilder from FFConnectorSpec)
    // May have multiple lanes (parallel connectors)
    private val ffConnectors210to134: List[FFConnector] = net.ffConnectors
    private val ff210to134: FFConnector =
        if ffConnectors210to134.nonEmpty then ffConnectors210to134.head else null

    // FF diverge/merge segment indices (for Car.act() routing)
    private val ffDivJuncIdx210 = junc210.indexWhere (_.name.contains ("WINONA"))
    private val ffMrgJuncIdx134 = junc134.indexWhere (_.name.contains ("ORANGE"))
    private val ffDivSeg210 = if ffDivJuncIdx210 > 0 then ffDivJuncIdx210 - 1 else -1
    private val ffMrgSeg134 = ffMrgJuncIdx134

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Time-varying FF split ratios from PeMS data
    // splitRatio(i) = flow_717603 / flow_717634 for each 5-min interval
    // Reuses existing AggregatedDemand CSV loading — no new file I/O in model
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val splitIntervalSec = 5.0 * MINUTE                // 300 s per PeMS interval
    private val splitRatios: Array[Double] =
        AggregatedArrivalSource.computeSplitRatios (
            AggregatedDemand.I210_WB_Baseline,  717634,        // upstream: LAKE 1 on I-210
            AggregatedDemand.SR134_WB_Baseline,  717603         // FF merge: ORANGE GROVE on SR-134
        )

    /** Look up the current split ratio from the time-varying array.
     *  Uses the simulation clock to index into the 5-min interval array.
     */
    private def currentSplitRatio: Double =
        val idx = (clock / splitIntervalSec).toInt
        if idx >= 0 && idx < splitRatios.length then splitRatios(idx)
        else if splitRatios.nonEmpty then splitRatios.last      // clamp to last interval
        else 0.30                                                // ultimate fallback

    debug ("init", s"I-210: lanes=$numLanes210, segs=$numSegments210, " +
                   s"onRamps=$nOnRamps210, hwLen=$hwLen210")
    debug ("init", s"SR-134: lanes=$numLanes134, " +
                   s"onRamps=$nOnRamps134, hwLen=$hwLen134")
    debug ("init", s"FF: divSeg=$ffDivSeg210, mrgSeg=$ffMrgSeg134, " +
                   f"splitRatios=${splitRatios.length} intervals, avg=${splitRatios.sum / splitRatios.length.max(1)}%.3f")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 3: Arrival Sources (same pattern as CalRoute101_3 / TrafficModelBuilder)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    // I-210 mainline: PeMSArrivalSource from anchor CSV (same as CalRoute101_3)
    // I-210 ramps: AggregatedRampArrivalSource from aggregated OR CSV
    private val nLanesAnchor = anchorSpeed.dim2                    // 5 lanes from anchor sensor
    private val (mlSources210, rampSrcArr210) =
        if synthetic then
            ArrivalSource.syntheticSources (100, 50, nLanesAnchor, nOnRamps210, iArrivalRV)
        else
            val (ml, _) = ArrivalSource.allSources (pems210, nLanesAnchor)
            val ramps: Array[ArrivalSource] = Array.tabulate (nOnRamps210) { r =>
                new AggregatedRampArrivalSource (AggregatedDemand.I210_WB_Baseline, r, rowTimeSlice)
            }
            (ml, ramps)

    // SR-134 arrival sources (ramps only — no mainline sources)
    // SR-134 has no anchor CSV; ramps still use aggregated OR
    private val (_, rampSrcArr134) =
        if synthetic then ArrivalSource.syntheticSources (100, 50, numLanes134, nOnRamps134, iArrivalRV)
        else ArrivalSource.fromAggregated (AggregatedDemand.SR134_WB_Baseline, numLanes134, nOnRamps134)

    debug ("init", s"synthetic=$synthetic, mlSources210=${mlSources210.length} (anchor, $nLanesAnchor lanes), rampSrcArr210=${rampSrcArr210.length}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 4: Sources (model owns demand — uses ArrivalSource.getTotalVehicles)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val RAMP_LEN = 150.0                                 // visual ramp length (px) — matches CorridorBuilder

    // I-210 mainline sources at EASTERN end (high postmile = entry for WB)
    // Spaced perpendicular to road direction — matches Route lane GAP (50 px)
    private val mainlineSources210 = {
        import scala.collection.mutable.ListBuffer
        val buf = ListBuffer [VSource] ()
        val dx  = junc210(1).at(0) - junc210(0).at(0)
        val dy  = junc210(1).at(1) - junc210(0).at(1)
        val hyp = math.hypot (dx, dy).max (1e-9)
        val perpX =  dy / hyp                              // perpendicular unit vector (same as Route.calcShift2)
        val perpY = -dx / hyp
        val upX   = -dx / hyp                              // upstream unit vector (away from junc(1))
        val upY   = -dy / hyp
        val LANE_GAP  = 50.0                               // matches Route.GAP
        val UPSTREAM  = 80.0                                // distance upstream of junc(0)
        cfor (0, numLanes210) { l =>
            val physLane   = numLanes210 - 1 - l            // match Route's physical lane mapping
            val laneOffset = (physLane - (numLanes210 - 1) / 2.0) * LANE_GAP
            val loc = Array (junc210(0).at(0) + upX * UPSTREAM + perpX * laneOffset,
                             junc210(0).at(1) + upY * UPSTREAM + perpY * laneOffset,
                             20.0, 20.0)
            val nStop = mlSources210(l).getTotalVehicles (l)   // from ArrivalSource
            val iArrivalRV = mlSources210(l).getDistribution   // only use RV for synthetic demand
            buf += new VSource (s"I210_ML$l", this, () => Car (), l, nStop, iArrivalRV, loc)
        }
        buf.toList
    }

    // I-210 ramp sources — positioned at rampAttachPoint + outward * RAMP_LEN
    private val rampSources210 = {
        import scala.collection.mutable.ListBuffer
        val buf = ListBuffer [VSource] ()
        val (px, py) = route210.perpVec
        cfor (0, nOnRamps210) { r =>
            val (ax, ay) = route210.rampAttachPoint (rampJoinSeg210(r))
            val loc = Array (ax + px * RAMP_LEN, ay + py * RAMP_LEN, 20.0, 20.0)
            val nStop = rampSrcArr210(r).getTotalVehicles (0)
            val iArrivalRV = rampSrcArr210(r).getDistribution
            buf += new VSource (s"", this, () => Car (), numLanes210 + r,
                                nStop, iArrivalRV, loc)
        }
        buf.toList
    }

    // SR-134 has NO mainline VSource — all mainline traffic enters via FF from I-210.
    // Only on-ramp VSources feed local traffic into SR-134 mid-corridor.

    // SR-134 ramp sources — positioned at rampAttachPoint + outward * RAMP_LEN
    private val rampSources134 = {
        import scala.collection.mutable.ListBuffer
        val buf = ListBuffer [VSource] ()
        val (px, py) = route134.perpVec
        cfor (0, nOnRamps134) { r =>
            val (ax, ay) = route134.rampAttachPoint (rampJoinSeg134(r))
            val loc = Array (ax + px * RAMP_LEN, ay + py * RAMP_LEN, 20.0, 20.0)
            val nStop = rampSrcArr134(r).getTotalVehicles (0)
            val iArrivalRV = rampSrcArr134(r).getDistribution
            buf += new VSource (s"", this, () => Car (),
                                SR134_BASE + numLanes134 + r, nStop, iArrivalRV, loc)
        }
        buf.toList
    }

    private val sources: List [VSource] =
        mainlineSources210 ++ rampSources210 ++ rampSources134

    println (s"VSource nStop: ${sources.map (s => s"${s.name}=${s.nStop}").mkString (", ")}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 4: Ramps (model owns — need VSource as `from` component)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val ramps210 = new Array [Ramp] (nOnRamps210)
    cfor (0, nOnRamps210) { r =>
        ramps210(r) = new Ramp (s"I210_OR${r + 1}", rampSources210(r), b210.rampSensors(r),
                                motion, scalation.simulation.process.RampMode.On, false, 0.1, 0.0)
    }

    private val ramps134 = new Array [Ramp] (nOnRamps134)
    cfor (0, nOnRamps134) { r =>
        ramps134(r) = new Ramp (s"S134_OR${r + 1}", rampSources134(r), b134.rampSensors(r),
                                motion, scalation.simulation.process.RampMode.On, false, 0.1, 0.0)
    }

    // Off-ramps: from = off-ramp diverge junction (side of road), to = off-ramp sink
    // Mirrors on-ramp pattern: both endpoints on same side, not crossing all lanes.
    private val nOffRamps210   = b210.offRampJoinSegs.length
    private val offRamps210    = new Array [Ramp] (nOffRamps210)
    cfor (0, nOffRamps210) { r =>
        offRamps210(r) = new Ramp (s"I210_FR${r + 1}",
            b210.offRampSensors(r),                        // from: diverge junction (road edge)
            b210.offRampSinks(r),                           // to: off-ramp sink
            motion, scalation.simulation.process.RampMode.Off, false, -0.1, 0.0)
    }

    private val nOffRamps134   = b134.offRampJoinSegs.length
    private val offRamps134    = new Array [Ramp] (nOffRamps134)
    cfor (0, nOffRamps134) { r =>
        offRamps134(r) = new Ramp (s"S134_FR${r + 1}",
            b134.offRampSensors(r),                        // from: diverge junction (road edge)
            b134.offRampSinks(r),                           // to: off-ramp sink
            motion, scalation.simulation.process.RampMode.Off, false, -0.1, 0.0)
    }

    debug ("init", s"Off-ramps: I-210=$nOffRamps210, SR-134=$nOffRamps134")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 5: Register ALL components
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val allJunctions = junc210.toList ++ b210.rampSensors.toList ++ b210.offRampSensors.toList ++
                               junc134.toList ++ b134.rampSensors.toList ++ b134.offRampSensors.toList
    private val allSinks     = sinks210 ++ sinks134 ++
                               b210.offRampSinks.toList ++ b134.offRampSinks.toList
    private val allRamps     = ramps210.toList ++ ramps134.toList ++
                               offRamps210.toList ++ offRamps134.toList

    addComponents (sources, allJunctions, allSinks, allRamps)
    ffConnectors210to134.foreach (addComponent (_))              // register all FF lanes
    route210.pathway.foreach (addComponent (_))
    route134.pathway.foreach (addComponent (_))
    debug ("init", s"All components registered (I-210 + SR-134 + ${ffConnectors210to134.length} FF lanes)")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Car entity — parameterized to drive either corridor
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            if subtype < SR134_BASE then actOnCorridor (
                subtype, numLanes210, route210, junc210, sinks210, ramps210, rampJoinSeg210, hwLen210
            )
            else actOnCorridor (
                subtype - SR134_BASE, numLanes134, route134, junc134, sinks134, ramps134, rampJoinSeg134, hwLen134
            )
        end act

        /** Drive a car on any corridor — mainline or ramp entry. */
        private def actOnCorridor (localSub: Int, nLanes: Int,
                                   route: Route, junc: Array [Junction],
                                   sinks: List [Sink], ramps: Array [Ramp],
                                   joinSegs: Array [Int], hwLen: Int): Unit =
            if localSub < nLanes then
                // Mainline entry
                laneID = localSub
                // Guard: ensure assigned lane exists at entry segment (seg 0)
                if !route.laneExistsAt (laneID, 0) then
                    laneID = route.lanesAt (0) - 1           // outermost existing lane
                end if
                val carAhead = route.pathway(laneID).seg(0).getLast
                route.pathway(laneID).addToAlist (this, carAhead, 0)
                segId = 0                                  // set before jump so density records correctly
                junc(0).jump ()
                driveHighway (route, junc, sinks, hwLen, 0)
            else
                // Ramp entry
                laneID = nLanes - 1
                val rampIdx = localSub - nLanes
                val r = ramps(rampIdx)
                driveRamp (r)
                val joinSeg = joinSegs(rampIdx)
                // Guard: ensure target lane exists at join segment
                if !route.laneExistsAt (laneID, joinSeg) then
                    laneID = route.lanesAt (joinSeg) - 1     // outermost existing lane
                end if
                val carAhead = route.pathway(laneID).seg(joinSeg).getLast
                route.pathway(laneID).addToAlist (this, carAhead, joinSeg)
                segId = joinSeg                            // set before jump so density records correctly
                junc(joinSeg).jump ()
                driveHighway (route, junc, sinks, hwLen, joinSeg)
        end actOnCorridor

        /** Drive segment by segment from startSeg to the corridor sink.
         *  At the FF interchange segment, I-210 cars may divert to SR-134.
         */
        private def driveHighway (route: Route, junc: Array [Junction],
                                  sinks: List [Sink], hwLen: Int, startSeg: Int): Unit =
            var seg      = startSeg
            var diverted = false

            while seg < hwLen && !diverted do
                route.pathway(laneID).seg(seg).move ()
                junc(seg + 1).jump ()

                // ── FF diversion: I-210 WB → SR-134 WB at interchange ──────
                // Time-varying split ratio from PeMS (flow_717603 / flow_717634)
                if !diverted && ffConnectors210to134.nonEmpty
                   && subtype < SR134_BASE                    // car is on I-210
                   && seg == ffDivSeg210                      // at interchange segment
                   && ffMrgSeg134 >= 0                        // merge point valid
                   && rand.gen < currentSplitRatio then       // time-varying probabilistic split
                    diverted = true
                    // 1. Exit I-210 pathway — remove from current segment's DLL
                    route.pathway(laneID).seg(seg).removeFromAlist (this)
                    myPathway = null
                    // 2. Randomly select one of the FF connector lanes
                    val ffLaneIdx = (rand.gen * ffConnectors210to134.length).toInt.min(ffConnectors210to134.length - 1)
                    val ffLane = ffConnectors210to134(ffLaneIdx)
                    // 3. Drive the selected FF connector lane
                    val ahead = ffLane.getLast
                    ffLane.addToAlist (this, ahead)
                    ffLane.lane.move ()
                    ffLane.removeFromAlist (this)
                    // 4. Enter SR-134 at merge junction — spread across all lanes
                    laneID = (rand.gen * numLanes134).toInt.min (numLanes134 - 1)
                    // Guard: ensure target lane exists at merge segment on SR-134
                    if !route134.laneExistsAt (laneID, ffMrgSeg134) then
                        laneID = route134.lanesAt (ffMrgSeg134) - 1
                    end if
                    val carAhead = route134.pathway(laneID).seg(ffMrgSeg134).getLast
                    route134.pathway(laneID).addToAlist (this, carAhead, ffMrgSeg134)
                    junc134(ffMrgSeg134).jump ()
                    // 5. Continue driving on SR-134 to its sink
                    driveHighway (route134, junc134, sinks134, hwLen134, ffMrgSeg134)
                end if

                // ── DLL hop: exit this segment's DLL, enter next segment's DLL ──
                if !diverted then
                    route.pathway(laneID).seg(seg).removeFromAlist (this)
                    seg += 1
                    if seg < hwLen then
                        // ── Lane-end check: does my lane exist at the next segment? ──
                        if !route.laneExistsAt (laneID, seg) then
                            val avail = 0 until route.lanesAt (seg)
                            laneID = route.forceMerge (laneID, avail, this, seg)
                        end if
                        val nextVT = route.pathway(laneID).seg(seg)
                        val ahead  = nextVT.getLast
                        nextVT.addToAlist (this, ahead)
                        myPathway = route.pathway(laneID)
                    end if
                end if
            end while

            if !diverted then
                myPathway = null
                sinks.head.leave ()
        end driveHighway

        /** Traverse an on-ramp single segment. */
        private def driveRamp (r: Ramp): Unit =
            val carAhead = r.getLast
            r.addToAlist (this, carAhead)
            if r.mode == scalation.simulation.process.RampMode.On then
                r.lane.move ()
                r.to.asInstanceOf [Junction].jump ()
            end if
            r.removeFromAlist (this)
        end driveRamp

    end Car

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // RowTimeLoader — delegates to ArrivalSource (mu) + anchor CSV (speed)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def getDataDimension: Int = nt

    /** Mu dispatch: routes subtype to the correct corridor's ArrivalSource objects. */
    def getMuForSource (sourceIdx: Int): VectorD =
        if sourceIdx < numLanes210 + nOnRamps210 then
            // I-210 mainline or on-ramp
            RowTimeLoader.getMuForSourceDefault (
                mlSources210, rampSrcArr210, numLanes210, nt, sourceIdx)
        else if sourceIdx >= SR134_BASE then
            // SR-134 on-ramp (remap to local index; no mainline VSources)
            RowTimeLoader.getMuForSourceDefault (
                Array.empty, rampSrcArr134, numLanes134, nt, sourceIdx - SR134_BASE)
        else
            VectorD.fill (nt)(Double.MaxValue)                 // gap between corridors
    end getMuForSource

    /** Speed from anchor sensor (72 × 5, already m/s). */
    def getSpeedMatrix (): MatrixD = anchorSpeed

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Public accessors
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def getJunctions210: Array [Junction] = junc210
    def getJunctions134: Array [Junction] = junc134
    def getLayout210: CorridorLayout = multiConfig.corridor ("I-210-W").layout
    def getLayout134: CorridorLayout = multiConfig.corridor ("SR-134-W").layout
    def getBuiltNetwork: BuiltNetwork = net

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Finish
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    override def fini (rep: Int): Unit =
        println ("\n" + "=" * 70)
        println ("EatonFireModel SIMULATION COMPLETE (I-210 + SR-134)")
        println ("=" * 70)
        super.fini (rep)
    end fini

    simulate ()
    waitFinished ()
    Model.shutdown ()

end EatonFireModel

