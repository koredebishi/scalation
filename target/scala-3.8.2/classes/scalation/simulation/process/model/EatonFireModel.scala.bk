//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sat Mar 22 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    EatonFireModel: I-210 WB + SR-134 WB Dual-Corridor Traffic Model
 *
 *  Uses CorridorLayout auto-generated from PeMS station_map.csv.
 *  Both corridors share one coordinate frame so they appear in correct
 *  spatial relationship in the animation.
 */

package scalation
package simulation
package process
package model

import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.{Exponential, Uniform}
import scalation.simulation.process.{IntegratorType, IDMDynamics}
import scalation.simulation.process.config.{CorridorLayout, EatonCorridorConfig}
import scalation.simulation.process.config.{RampMode => ConfigRampMode}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run the EatonFireModel simulation.
 *  > runMain scalation.simulation.process.model.runEatonFireModel
 */
@main def runEatonFireModel (): Unit = new EatonFireModel ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EatonFireModel` class is a dual-corridor traffic simulation model
 *  for I-210 Westbound + SR-134 Westbound (Eaton fire evacuation).
 *
 *  Both corridors share one animation coordinate frame.
 *  Demand uses placeholder uniform arrivals until PeMS flow data is integrated.
 *
 *  Subtype encoding:
 *    0 .. numLanes210-1                           = I-210 mainline lanes
 *    numLanes210 .. numLanes210+nOnRamps210-1     = I-210 on-ramps
 *    SR134_BASE+numLanes134 .. ...                = SR-134 on-ramps
 *  Note: SR-134 has NO mainline VSources.  All SR-134 mainline traffic
 *        enters via the FF connector from I-210 at the Pasadena interchange.
 */
class EatonFireModel (name: String = "EatonFireModel", reps: Int = 1,
                      animating: Boolean = true, aniRatio: Double = 500.0)
      extends Model (name, reps, animating, aniRatio)
         with RowTimeLoader:

    private val debug = debugf ("EatonFireModel", true)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 1: Load BOTH corridor layouts in a shared coordinate frame
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val layout210 = EatonCorridorConfig.I210_WB
    private val layout134 = EatonCorridorConfig.SR134_WB
    layout210.summary ()
    layout134.summary ()

    // I-210 topology
    private val config210      = layout210.config
    private val numLanes210    = config210.mainline.lanesPerSegment
    private val numSegments210 = config210.mainline.segments
    private val nJunc210       = layout210.numJunctions
    private val nOnRamps210    = layout210.numOnRamps

    // SR-134 topology
    private val config134      = layout134.config
    private val numLanes134    = config134.mainline.lanesPerSegment
    private val numSegments134 = config134.mainline.segments
    private val nJunc134       = layout134.numJunctions
    private val nOnRamps134    = layout134.numOnRamps

    private val SR134_BASE = 100                               // subtype offset for SR-134
    private val nt         = 48                                // 48 × 15-min = 12 hours

    debug ("init", s"I-210: lanes=$numLanes210, segs=$numSegments210, juncs=$nJunc210, onRamps=$nOnRamps210")
    debug ("init", s"SR-134: lanes=$numLanes134, segs=$numSegments134, juncs=$nJunc134, onRamps=$nOnRamps134")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 2: Dynamics
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val motion = IDMDynamics
    IDMDynamics.integratorType = IntegratorType.Ballistic
    private val iArrivalRV = Exponential (MINUTE / 10.0)
    private val rand       = Uniform (0.0, 1.0)            // for FF split ratio decisions
    setTime (nt * rowTime)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 3: I-210 Junctions
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    // Junctions are reversed: junc(0) = EASTERN end (high postmile),
    // junc.last = WESTERN end (low postmile).  Traffic flows east → west = WB.
    private val junc210 = Array.ofDim [Junction] (nJunc210)
    cfor (0, nJunc210) { i =>
        val ri = nJunc210 - 1 - i                              // reverse index
        junc210(i) = new Junction (s"I210_${layout210.junctionNames(ri)}",
                                   xy = layout210.mainlineScreenXY(ri), nt = nt, nl = numLanes210)
    }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 4: SR-134 Junctions
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val junc134 = Array.ofDim [Junction] (nJunc134)
    cfor (0, nJunc134) { i =>
        val ri = nJunc134 - 1 - i                              // reverse index
        junc134(i) = new Junction (s"SR134_${layout134.junctionNames(ri)}",
                                   xy = layout134.mainlineScreenXY(ri), nt = nt, nl = numLanes134)
    }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 5: Ramp Junctions (both corridors)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val rampSensors210 = Array.ofDim [Junction] (nOnRamps210)
    cfor (0, nOnRamps210) { i =>
        rampSensors210(i) = new Junction (s"I210_onRamp${i + 1}",
                                          xy = layout210.onRampScreenXY(i), nt = nt, nl = numLanes210)
    }

    private val rampSensors134 = Array.ofDim [Junction] (nOnRamps134)
    cfor (0, nOnRamps134) { i =>
        rampSensors134(i) = new Junction (s"SR134_onRamp${i + 1}",
                                          xy = layout134.onRampScreenXY(i), nt = nt, nl = numLanes134)
    }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 6: Routes (each corridor has its own Route)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val interJunc210 = junc210.slice (1, junc210.length - 1)
    private val route210 = Route ("I210_Rte", numLanes210, interJunc210, junc210(0), junc210.last, motion)

    private val interJunc134 = junc134.slice (1, junc134.length - 1)
    private val route134 = Route ("SR134_Rte", numLanes134, interJunc134, junc134(0), junc134.last, motion)

    debug ("init", s"I-210 Route: ${route210.pathway.length} pathways")
    debug ("init", s"SR-134 Route: ${route134.pathway.length} pathways")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 7: Sinks (one per corridor, positioned near last junction)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    // Sinks at WESTERN end (low postmile) — evacuation exit direction
    private val (xEnd210, yEnd210) = layout210.mainlineScreenXY.head
    private val sinks210 = Sink.group ((xEnd210.toInt - 100, yEnd210.toInt - 100),
        ("sinkI210", (0, 0))
    )

    private val (xEnd134, yEnd134) = layout134.mainlineScreenXY.head
    private val sinks134 = Sink.group ((xEnd134.toInt - 100, yEnd134.toInt - 100),
        ("sinkSR134", (0, 0))
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 8: Sources (placeholder arrivals)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val (center210, offsets210) = layout210.getVSourceCenterAndOffsets
    private val (center134, offsets134) = layout134.getVSourceCenterAndOffsets

    // I-210 mainline sources at EASTERN end (high postmile = Rosemead)
    private val mainlineSources210 = {
        import scala.collection.mutable.ListBuffer
        val buf = ListBuffer [VSource] ()
        cfor (0, numLanes210) { l =>
            val loc = Array (layout210.mainlineScreenXY.last._1 - 50.0 + l * 10.0,
                             layout210.mainlineScreenXY.last._2 + 50.0, 20.0, 20.0)
            buf += new VSource (s"I210_vsrcML_L$l", this, () => Car (), l, 100, iArrivalRV, loc)
        }
        buf.toList
    }

    // I-210 ramp sources — subtypes numLanes210..numLanes210+nOnRamps210-1
    private val rampSources210 = {
        import scala.collection.mutable.ListBuffer
        val buf = ListBuffer [VSource] ()
        cfor (0, nOnRamps210) { r =>
            val offset = offsets210(r + 1)
            val loc = Array ((center210._1 + offset._1).toDouble,
                             (center210._2 + offset._2).toDouble, 20.0, 20.0)
            buf += new VSource (s"I210_srcRamp${r + 1}", this, () => Car (), numLanes210 + r,
                                50, iArrivalRV, loc)
        }
        buf.toList
    }

    // SR-134 has NO mainline VSource — all mainline traffic enters via FF from I-210.
    // The PeMS sensor at Orange Grove measures FF-derived flow, not independent arrivals.
    // Only on-ramp VSources feed local traffic into SR-134 mid-corridor.

    // SR-134 ramp sources — subtypes SR134_BASE+numLanes134..
    private val rampSources134 = {
        import scala.collection.mutable.ListBuffer
        val buf = ListBuffer [VSource] ()
        cfor (0, nOnRamps134) { r =>
            val offset = offsets134(r + 1)
            val loc = Array ((center134._1 + offset._1).toDouble,
                             (center134._2 + offset._2).toDouble, 20.0, 20.0)
            buf += new VSource (s"SR134_srcRamp${r + 1}", this, () => Car (),
                                SR134_BASE + numLanes134 + r, 50, iArrivalRV, loc)
        }
        buf.toList
    }

    private val sources: List [VSource] =
        mainlineSources210 ++ rampSources210 ++ rampSources134

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 9: Ramp objects (both corridors)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val ramps210 = new Array [Ramp] (nOnRamps210)
    cfor (0, nOnRamps210) { r =>
        ramps210(r) = new Ramp (s"I210_onRamp${r + 1}", rampSources210(r), rampSensors210(r),
                                motion, scalation.simulation.process.RampMode.On, false, 0.1, 0.0)
    }

    private val ramps134 = new Array [Ramp] (nOnRamps134)
    cfor (0, nOnRamps134) { r =>
        ramps134(r) = new Ramp (s"SR134_onRamp${r + 1}", rampSources134(r), rampSensors134(r),
                                motion, scalation.simulation.process.RampMode.On, false, 0.1, 0.0)
    }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 9a: FF Connector — I-210 WB → SR-134 WB at Pasadena interchange
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Find junction index whose name contains the given substring. */
    private def findJuncIdx (juncs: Array [Junction], nameContains: String): Int =
        juncs.indices.find (i => juncs(i).name.contains (nameContains)).getOrElse (-1)

    // FF diverge: I-210 at WINONA WAY (PM 24.442) — car exits I-210 here
    // FF merge:   SR-134 at ORANGE GROVE (PM 12.763) — car enters SR-134 here
    //             (ORANGE GROVE is the easternmost SR-134 junction = the 210/134 interchange)
    // PeMS station 775725 "WB 210 CON" at PM 24.49: 2-lane connector ramp
    private val ffDivJuncIdx210 = findJuncIdx (junc210, "WINONA")
    private val ffMrgJuncIdx134 = findJuncIdx (junc134, "ORANGE")

    debug ("init", s"FF interchange: I-210 divJuncIdx=$ffDivJuncIdx210 (${if ffDivJuncIdx210 >= 0 then junc210(ffDivJuncIdx210).name else "NOT FOUND"})")
    debug ("init", s"FF interchange: SR-134 mrgJuncIdx=$ffMrgJuncIdx134 (${if ffMrgJuncIdx134 >= 0 then junc134(ffMrgJuncIdx134).name else "NOT FOUND"})")

    // Create the FF connector (30% split ratio — placeholder, calibrate from PeMS)
    private val ff210to134: FFConnector =
        if ffDivJuncIdx210 >= 0 && ffMrgJuncIdx134 >= 0 then
            new FFConnector ("FF_I210_to_SR134",
                             junc210(ffDivJuncIdx210), junc134(ffMrgJuncIdx134),
                             motion, splitRatio = 0.30, bend = 0.25)
        else null

    // The diverge segment = the segment that ENDS at the diverge junction
    // After driving segment (ffDivJuncIdx210 - 1), car is at ffDivJuncIdx210 and decides
    private val ffDivSeg210 = if ffDivJuncIdx210 > 0 then ffDivJuncIdx210 - 1 else -1
    // The merge segment = where the car starts driving on SR-134 after entering
    private val ffMrgSeg134 = ffMrgJuncIdx134

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Step 10: Register ALL components
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val allJunctions = junc210.toList ++ rampSensors210.toList ++
                               junc134.toList ++ rampSensors134.toList
    private val allSinks     = sinks210 ++ sinks134
    private val allRamps     = ramps210.toList ++ ramps134.toList

    addComponents (sources, allJunctions, allSinks, allRamps)
    if ff210to134 != null then addComponent (ff210to134)
    route210.pathway.foreach (addComponent (_))
    route134.pathway.foreach (addComponent (_))
    debug ("init", "All components registered (I-210 + SR-134 + FF)")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Car entity — parameterized to drive either corridor
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    private val hwLen210       = junc210.length - 1
    private val hwLen134       = junc134.length - 1
    // Ramp join segments remapped for reversed junction order
    private val rampJoinSeg210 = config210.ramps.filter (_.mode == ConfigRampMode.On)
        .map (r => numSegments210 - 1 - r.joinSegment).toArray
    private val rampJoinSeg134 = config134.ramps.filter (_.mode == ConfigRampMode.On)
        .map (r => numSegments134 - 1 - r.joinSegment).toArray

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
                val carAhead = route.pathway(laneID).getLast
                route.pathway(laneID).addToAlist (this, carAhead)
                junc(0).jump ()
                driveHighway (route, junc, sinks, hwLen, 0)
            else
                // Ramp entry
                laneID = nLanes - 1
                val rampIdx = localSub - nLanes
                val r = ramps(rampIdx)
                driveRamp (r)
                val joinSeg = joinSegs(rampIdx)
                val carAhead = route.pathway(laneID).seg(joinSeg).getLast
                route.pathway(laneID).addToAlist (this, carAhead)
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
                if !diverted && ff210to134 != null
                   && subtype < SR134_BASE                    // car is on I-210
                   && seg == ffDivSeg210                      // at interchange segment
                   && ffMrgSeg134 >= 0                        // merge point valid
                   && rand.gen < ff210to134.splitRatio then   // probabilistic split
                    diverted = true
                    // 1. Exit I-210 pathway
                    route.pathway(laneID).removeFromAlist (this)
                    // 2. Drive the FF connector ramp
                    val ahead = ff210to134.getLast
                    ff210to134.addToAlist (this, ahead)
                    ff210to134.lane.move ()
                    ff210to134.removeFromAlist (this)
                    // 3. Enter SR-134 at merge junction — spread across all lanes
                    laneID = (rand.gen * numLanes134).toInt.min (numLanes134 - 1)
                    val carAhead = route134.pathway(laneID).seg(ffMrgSeg134).getLast
                    route134.pathway(laneID).addToAlist (this, carAhead)
                    junc134(ffMrgSeg134).jump ()
                    // 4. Continue driving on SR-134 to its sink
                    driveHighway (route134, junc134, sinks134, hwLen134, ffMrgSeg134)
                end if

                if !diverted then seg += 1
            end while

            if !diverted then
                route.pathway(laneID).removeFromAlist (this)
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
    // RowTimeLoader (placeholder — no PeMS data yet)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def getDataDimension: Int = nt

    def getMuForSource (sourceIdx: Int): VectorD =
        VectorD.fill (nt)(MINUTE / 10.0)

    def getSpeedMatrix (): MatrixD =
        MatrixD.fill (nt, math.max (numLanes210, numLanes134), 30.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Public accessors
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def getJunctions210: Array [Junction] = junc210
    def getJunctions134: Array [Junction] = junc134
    def getLayout210: CorridorLayout = layout210
    def getLayout134: CorridorLayout = layout134

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
