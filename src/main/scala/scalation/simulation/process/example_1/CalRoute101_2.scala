//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Model: One-Way-Street (with Vehicle) for Process-Interaction Simulation
 */

package scalation
package simulation
package process
package example_1                                       // One-Shot

import scalation.modeling.FitM
import scalation.random.*
import scalation.mathstat.*


@main def runCalRoute101_2(): Unit = new CalRoute101_2()

class CalRoute101_2(name: String = "CalRoute101_2", reps: Int = 1, animating: Boolean = false,
                    aniRatio: Double = 500.0, stream: Int = 0,
                    arrivalType: String = "Erlang2S")  // "Erlang2S" or "Poisson" for experiments
    extends Model(name, reps, animating, aniRatio)
        with RowTimeLoader
        with FitM:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Debugging and traffic data loading
    private val debug       = debugf("CalRoute101_2", false)
    val config      = new TrafficConfig2("1-401112ML", rowTime, stream)
    private val nt          = config.dim                                        // Number of time rows in anchor data

    val rand = Uniform(0.0, 1.0)                                                // probability uniform in [0,1)

// discrete event and discrete time.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Simulation dynamics and random variables */
    private val motion         = IDMDynamics                                     //GippsDynamics
    private val numLanes        = 4                                             // RoadCood2 specifies 4 lanes for all sensors

    // Arrival variate based on constructor parameter (for experiments)
    private val iArrivalRV: Variate = arrivalType match
        case "Poisson"  => Exponential()      // mu overridden per-lane
        case _          => Erlang2S(tau = 0.6)   // default: shifted Erlang-2

    private val iArrivalRV_ramp1: Variate = arrivalType match
        case "Poisson"  => Exponential()
        case _          => Erlang2S(tau = 4.0)

    private val iArrivalRV_ramp2: Variate = arrivalType match
        case "Poisson"  => Exponential()
        case _          => Erlang2S(tau = 10.0)

    private val laneChangeRV       = Bernoulli(0.6)


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Delegate per‑source μ lookup to TrafficConfig */
    def getMuForSource(idx: Int): VectorD =
        VectorD(config.getMuForSource(idx))

    setTime(nt * rowTime)     // I need my time stamp formatted for easy passing for time calculations and advancement.
    // 00:00:00  ----> 00.00, 00.15

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animation coordinates and junction setup */
    val (w, h) = (1500, 1500)
    val shift = 20.0

    private val aniCoords_Main = TrafficConfig2.getMainlineCoordinates((w, h))
    private val aniCoords_Ramp = TrafficConfig2.getRampCoordinates((w, h)) // already nudged
    private val (centerPos, offsets) = TrafficConfig2.getVSourceCenterAndOffsets((w, h))

    // Junction naming convention:
    //   - PEMS sensors: sensor1, sensor2, sensor3, sensor4, sensor5 (data comparison points)
    //   - Merge points: onR_merge1, onR_merge2 (operational only, no PEMS comparison)
    // Layout: sensor1 -> onR_merge1 -> sensor2 -> sensor3 -> onR_merge2 -> sensor4 -> sensor5
    private [process] val junc: Array[Junction] = Array.ofDim[Junction](aniCoords_Main.length)
    private val juncNames = Array("warm_up", "sensor1", "onR_merge1", "sensor2", "sensor3", "onR_merge2", "sensor4", "sensor5")
    //private val juncNames = Array("sensor1", "onR_merge1", "sensor2", "sensor3", "onR_merge2", "sensor4", "sensor5")  // no warm-up sensor

    for i <- junc.indices do
        junc(i) = new Junction(juncNames(i), xy = aniCoords_Main(i), nt = nt, nl = numLanes)

    private [process] val ramp_sensors = Array.ofDim[Junction](aniCoords_Ramp.length)                       // 2 onramps
    for i <- ramp_sensors.indices do
        ramp_sensors(i) = new Junction(s"ramp${i+1}", aniCoords_Ramp(i), nt, numLanes)                      // use numLanes for recording compatibility

    // Build Route FIRST so we can place VSources using pathway geometry
    private val intermediateJunc = junc.slice(1, junc.length - 1)
    private val route = Route("Rte", numLanes, intermediateJunc, junc(0), junc.last, motion)

    // Create sink at end of mainline (no offramp in current layout)
    private val (x0, y0) = (aniCoords_Main.last._1 - 100.0, aniCoords_Main.last._2 - 100.0)
    private val sinks = Sink.group((x0.toInt, y0.toInt),
        ("sinkMain", (0, 0))
    )

    // Per-lane totals from TrafficConfig (each lane column summed)
    private val laneTotalsML = config.getMainlineLaneTotals

    // Build 4 lane-specific mainline VSources (subtypes 0..3) for 4-lane road
    private val mainlineSources: List[VSource] = MultiVSource.mainline4(
        this, () => Car(), route, "vsrcML",
        iArrivalRV, laneTotalsML
    )

    // Build ramp sources (subtypes 4,5) using existing offsets
    private val rampSources: List[VSource] = VSource.group(
        this, () => Car(), centerPos,
        ("srcRamp1", 4, iArrivalRV_ramp1, config.getOnRampTotals(0), offsets(1)),
        ("srcRamp2", 5, iArrivalRV_ramp1, config.getOnRampTotals(1), offsets(2))
    )

    // Combine sources: lanes first, then ramps
    private val sources: List[VSource] = mainlineSources ++ rampSources

    // Create ramps: only 2 onramps in current RoadCood2 layout (no offramp)
    private val ramps: Array[Ramp] = Ramp.group(motion,
        ("onRamp1", sources(4), ramp_sensors(0), RampMode.On, 0.0, 0.0),
        ("onRamp2", sources(5), ramp_sensors(1), RampMode.On, 0.0, 0.0)
    )

    // Register components
    addComponents(sources, junc.toList ++ ramp_sensors.toList, sinks, ramps.toList)
    route.pathway.foreach(addComponent(_))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map each ramp index → segment index on the main route where it merges.
     * Layout: sensor1(0) -> onR_merge1(1) -> sensor2(2) -> sensor3(3) -> onR_merge2(4) -> sensor4(5) -> sensor5(6)
     */

    // Ramp join segments: onramp1 joins at onR_merge1 (index 1), onramp2 joins at onR_merge2 (index 4)
    //private val rampJoinSeg = Array(1, 4)    // no warm-up sensor
    private val rampJoinSeg = Array(2, 5) // Corrected: onramp1 joins before sensor2 (index 2), onramp2 joins before sensor4 (index 4)
    @inline private def pos(rampIdx: Int): Int = rampJoinSeg(rampIdx)


    // Junction index → PEMS sensor index mapping
    // sensor1=0, sensor2=2, sensor3=3, sensor4=5, sensor5=6
    //val pemsToJunc = Array(0, 2, 3, 5, 6)     // PEMS sensors only no warm-up again.
    val pemsToJunc = Array(1, 3, 4, 6, 7)           // with warm-up sensor included

    case class Car() extends Vehicle("c", this):

        private val highway_length = junc.length - 1

        override def act(): Unit = {


            // ------------------ handle main entry vehicles -------------------
            if subtype <= 3 then       // subtypes 0..3 = mainline lane-specific sources (4 lanes)

                //laneID = subtype
                val carAhead = route.path(laneID).getLast
                route.path(laneID).addToAlist(this, carAhead)

                junc(0).jump()  // Count at sensors (sensor1)


                // Drive the full highway (no offramp in current layout)
                driveHighway()

            // ------------------ handle on-ramp entry vehicles -------------------
            else
                val onRamp = ramps(subtype - 4) // subtype 4,5 = onRamp1, onRamp2
                laneID = numLanes - 1 // Physical entry lane (rightmost, lane index 3 for 4 lanes)

                driveRamp(onRamp)    // first drive the ramp
                driveHighway()
        } // then drive the highway
        end act

        private def driveHighway(): Unit =
            var lastLaneChange = 20.0 //seconds

            // For mainline vehicles (subtype 0-3), start from segment 0
            // For onramp vehicles (subtype 4,5), start from their join segment
            val joinSeg = if subtype <= 3 then 0 else pos(subtype - 4)

            if subtype > 3 then
                // ═══ ONRAMP MERGE POINT LOGIC ═══
                // Vehicle has just exited ramp and needs to merge into mainline
                val carAhead = route.path(laneID).seg(joinSeg).getLast
                route.path(laneID).addToAlist(this, carAhead)
                if junc(joinSeg).name.startsWith("onR") then
                    junc(joinSeg).jump()

            // Now onramp vehicle joined the lane already.
            end if
            // nexting of method inside the for loop.
            cfor(joinSeg, highway_length) { seg =>

                val carAhead = getCarAhead(this)
                if carAhead != null && carAhead.velocity < 0.1 * vmax then  //
                    val target = if laneID > 0 then laneID - 1 else laneID + 1
                    route.changeLane(laneID, target, this, seg)
                end if

                route.path(laneID).seg(seg).move()

                if junc(seg + 1).name.startsWith("sensor") then
                    junc(seg + 1).jump()
            }

            route.path(laneID).removeFromAlist(this)
            // println(s"[SINK] ${this.displayLabel}: Entering sink")
            sinks.head.leave()

        end driveHighway

        private def driveRamp(comp: Component): Unit =

            val r = comp.asInstanceOf[Ramp]
            val carAhead = r.getLast
            r.addToAlist(this, carAhead)

            if r.mode == RampMode.On then
                r.lane.move()
                r.to.asInstanceOf[Junction].jump()
            else if r.mode == RampMode.Off then
                r.from.asInstanceOf[Junction].jump()
                r.lane.move()
            end if

            r.removeFromAlist(this)

            r.to match
                case s: Sink => s.leave()
                case _       =>
        end driveRamp

    end Car

    override def fini(rep: Int): Unit =

        // Formatted output for TrafficConfigTest2 (5 PEMS sensors only)
        val pemsSensorJuncs = pemsToJunc.map(junc(_))
        //Recorder.writeFormattedCSV(pemsSensorJuncs.toArray, "resultShifted.csv")
        super.fini(rep)
    end fini
    ////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation */
    simulate()
    waitFinished()
    //Model.shutdown()       // to be removed when TrafficOptimization is used
end CalRoute101_2