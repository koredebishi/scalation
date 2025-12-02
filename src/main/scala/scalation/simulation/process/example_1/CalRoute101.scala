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


@main def runCalRoute101(): Unit = new CalRoute101()

class CalRoute101(name: String = "CalRoute101", reps: Int = 1, animating: Boolean = false,
                  aniRatio: Double = 500.0, stream: Int = 0)
    extends Model(name, reps, animating, aniRatio)
        with RowTimeLoader
        with FitM:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Debugging and traffic data loading
    private val debug       = debugf("CalRoute101", false)
    val config      = new TrafficConfig("1-404531ML", rowTime, stream)
    private val nt          = config.dim  // Number of time rows in anchor data

    val rand = Uniform(0.0, 1.0)              // probability uniform in [0,1)


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Simulation dynamics and random variables */
    private val motion         = GippsDynamics
    private val numLanes        = 5
    private val iArrivalRV     = Erlang(3)
    //private val nStop          = config.nStopArray
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

    private val aniCoords_Main = config.getMainlineCoordinates((w, h))
    private val aniCoords_Ramp = config.getRampCoordinates((w, h)) // already nudged
    private val (centerPos, offsets) = config.getVSourceCenterAndOffsets((w, h))

    private val junc: Array[Junction] = Array.ofDim[Junction](aniCoords_Main.length)
    for i <- junc.indices do
        junc(i) = new Junction(s"ssor$i", xy = aniCoords_Main(i), nt = nt)

    private val ramp_sensors = Array.ofDim[Junction](aniCoords_Ramp.length)
    for i <- ramp_sensors.indices do
        ramp_sensors(i) = new Junction(s"ramp${i+1}", xy = aniCoords_Ramp(i), nt = nt)




    // Build Route FIRST so we can place VSources using pathway geometry
    private val intermediateJunc = junc.slice(1, junc.length - 1)
    private val route = Route("Rte", numLanes, intermediateJunc, junc(0), junc.last, motion)

    // Create sinks (mainline + ramp) near the last mainline junction and ramp end
    private val (x0, y0) = (aniCoords_Main.last._1 - 100.0, aniCoords_Main.last._2 - 100.0)
    private val (s, y) = (aniCoords_Ramp(2)._1, aniCoords_Ramp(2)._2)
    private val sinks = Sink.group((x0.toInt, y0.toInt),
        ("sinkMain", (0, 0)),
        ("sinkRamp", ((s + 800.0).toInt - x0.toInt, (y - 350).toInt - y0.toInt))
    )

    // Per-lane totals from TrafficConfig (each lane column summed)
    private val laneTotalsML = config.getMainlineLaneTotals

    // Build 5 lane-specific mainline VSources (subtypes 0..4)
    private val mainlineSources: List[VSource] = MultiVSource.mainline5(
        this, () => Car(), route, "vsrcML",
        iArrivalRV, laneTotalsML
    )

    // Build ramp sources (subtypes 5,6) using existing offsets
    private val rampSources: List[VSource] = VSource.group(
        this, () => Car(), centerPos,
        ("srcRamp1", 5, iArrivalRV, config.getOnRampTotals(0), offsets(1)),
        ("srcRamp2", 6, iArrivalRV, config.getOnRampTotals(1), offsets(2))
    )

    // Combine sources: lanes first, then ramps
    private val sources: List[VSource] = mainlineSources ++ rampSources

    // Create ramps inline to preserve subtype mapping: sources(5)=onRamp1, sources(6)=onRamp2
    private val ramps: Array[Ramp] = Ramp.group(motion,
        ("onRamp1", sources(5), ramp_sensors(0), RampMode.On, 0.0, 0.0),
        ("onRamp2", sources(6), ramp_sensors(1), RampMode.On, 0.0, 0.0),
        ("offRamp", ramp_sensors(2), sinks(1), RampMode.Off, 0.0, 0.0)
    )

    // Register components
    addComponents(sources, junc.toList ++ ramp_sensors.toList, sinks, ramps.toList)
    route.pathway.foreach(addComponent(_))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map each ramp index → segment index on the main route where it merges.
     * Adjust indices to match your network geometry.
     * Example: ramp1 ↦ seg0, ramp2 ↦ seg1, ramp3 ↦ seg5
     */

    private val rampJoinSeg = Array(3, 4)    // hardcoded part needs generalization for the ramp joining segment.
    @inline private def pos(rampIdx: Int): Int = rampJoinSeg(rampIdx)

    // Per-lane speed initialization now handled in MultiVSource.mainline5()
    // Ramp vehicles use default speed set below
    Vehicle.setInitialSpeed(68.0 / 2.24694)  // Default for ramp vehicles (subtypes 5,6)

    case class Car() extends Vehicle("c", this):


        val offRampJunction = 2
        private val highway_length = junc.length - 1

        override def act(): Unit =


            // ------------------ handle main entry vehicles -------------------
            if subtype <= 4 then       // subtypes 0..4 = mainline lane-specific sources


                // ===== SIMPLE IMPROVEMENT 1: Use raw exit fraction (removes 5-row MA lag) =====
                val baseExitFraction = config.exitFractionRaw(curRow)

                // ===== SIMPLE IMPROVEMENT 2: Per-lane exit multipliers (behavioral realism) =====
                // Lane 0 (fast/left) = lower exit probability (requires 4 lane changes)
                // Lane 4 (slow/right) = higher exit probability (already positioned)
                // Multipliers are calibrated so weighted average ≈ 1.0
                val laneMultiplier = Array(0.5, 0.8, 1.0, 1.4, 1.8)  // lane 0..4
                val laneExitFraction = baseExitFraction * laneMultiplier(subtype)

                // ===== Simple stochastic decision (no synchronization needed) =====
                val u = rand.gen
                val useOffRamp = u <= laneExitFraction

                laneID = subtype

                val carAhead = route.path(laneID).getLast

                route.path(laneID).addToAlist(this, carAhead)

                // drive until off-ramp junction// Universal for all vehicles.
                cfor (0 , offRampJunction) { seg =>
                    junc(seg).jump()
                    route.path(laneID).seg(seg).move()
                }

                // at junction 2, decide whether to take off-ramp
                if useOffRamp then
                    route.path(laneID).removeFromAlist(this)   // take offramp, leave highway
                    driveRamp(ramps(2)) // offramp
                else


                    // A force merge of vehicle to lanes 0-3 if currently on lane 4.
                    // because lane 4 is no longer available after the offramp.
                    // continue on highway but you can't be on lane 4 anymore.
                    //if the lane_id = 4 then, those vehicles need
                    // to change lane to ID= 0-3 (Mandatory lane change) for only those vehicles @ lane4
                    // code change here!!!!!
                    //Animation/ need to not draw that particular segment.
                    val segIdx = offRampJunction
                    if laneID == 4 && segIdx == offRampJunction then
                        laneID = 3 // force merge to lane 3; simple implementation

                           // segment index where the offramp is located
                        //val availLane = 0 to 3         // available lanes after the offramp
                        //val newLane = route.forceMerge(laneID, availLane, this, segIdx)    // force merge to lanes 0-3
                        //laneID = newLane  // update laneID after forced merge
                    end if
                    junc(offRampJunction).jump() // take count as normal (this becomes 4 lane counts)
                    driveHighway() // continue on highway driving.
                end if
            // ------------------ handle on-ramp entry vehicles -------------------
            else
                val onRamp = ramps(subtype - 5) // subtype 5,6 = onRamp1, onRamp2

                 //===== OLD code
                 laneID = 4 // FORCED lane for ramp vehicles; all onramp vehicles enter lane 4

                driveRamp(onRamp)
                driveHighway()    // then drive the highway
        end act

        private def driveHighway(): Unit =
            var lastLaneChange = 20.0 //seconds

            val joinSeg = if subtype <= 4 then offRampJunction else pos(subtype - 5)       // offrampJunc for highway vehicles


            // for onramp vehicles, jump to joinSeg first before adding to alist
//            if subtype > 4 then
//                val carAhead = route.path(laneID).seg(joinSeg).getLast
//                route.path(laneID).addToAlist(this, carAhead)
//                junc(joinSeg).jump()
//            end if

            if subtype > 4 then
                val carAhead = route.path(laneID).seg(joinSeg).getLast
                route.path(laneID).addToAlist(this, carAhead)

                // ─── Reset t_disp to ML coordinate system ───
                t_disp = route.toCumulative(joinSeg, 0.0)
                disp = 0.0
                segId = joinSeg
                // ─────────────────────────────────────────────

                junc(joinSeg).jump()
            end if

            cfor (joinSeg , highway_length) { seg =>

////------------ lane change at segment boundaries ---
//            if clock - lastLaneChange >= 20.0 then
//                val carAhead = getCarAhead(this)
//                if carAhead != null && carAhead.velocity < 0.9 * vmax then
//                    val target =
//                        if laneID == 0 then 1
//                        else if laneID == numLanes - 1 then numLanes - 2
//                        else if laneChangeRV.igen == 1 then laneID + 1
//                        else laneID - 1
//
//                    val currentLane = laneID
//                    route.changeLane(currentLane, target, this, seg)
//
//                    lastLaneChange = clock
//                end if
//            end if
////---------------END lane change at segment boundaries ---
                route.path(laneID).seg(seg).move()
                junc(seg + 1).jump()
            }

            route.path(laneID).removeFromAlist(this)
            println(s"THis car just sinked ${this.displayLabel}")
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

    // Remove duplicate sinks/ramps block at file end
    override def fini(rep: Int): Unit =
        Recorder.writeAllSensorStats(junc.toList ++ ramp_sensors.toList)

        // Sensor names for clear output
        val sensorNames = Array(
            "PEMS 531 (Entry)",
            "PEMS 532 (Second)",
            "PEMS 834 (After offramp)",
            "PEMS 833 (After onramp1)",
            "PEMS 929 (After onramp2)"
        )

        println("\n" + "=" * 80)
        println("MAINLINE VALIDATION: Comparing Simulation vs PEMS Ground Truth")
        println("=" * 80)

        // Loop over each mainline sensor
        for i <- 0 until 5 do
            val simMatrix = junc(i).getCountMatrix // Simulation counts at junction i
            val pemsMatrix = config.getPemsCountMatrix(i) // PEMS ground truth for sensor i
            //val bootstrappedMatrix = config.getBootstrappedMainlineMatrix(i)


            println(s"\n--- Sensor $i: junc($i) vs ${sensorNames(i)} ---")

            // Loop over each time row
            for row <- 0 until simMatrix.dim do
                val simRow = simMatrix(row) // Simulation: [lane1, lane2, lane3, lane4, lane5]
                val pemsRow = pemsMatrix(row) // PEMS:       [lane1, lane2, lane3, lane4, lane5]
                //val bootstrappedRow = bootstrappedMatrix(row)

                val totSimRow = simMatrix(row).sum // Simulation: [lane1, lane2, lane3, lane4, lane5]
                val totPemsRow = pemsMatrix(row).sum // PEMS:       [lane1, lane2, lane3, lane4, lane5]
                //val totBootsRow = bootstrappedMatrix(row).sum // Bootstrapped:       [lane1, lane2, lane3, lane4, lane5]


                // What we are comparing
                println(s"  Row $row:")
                println(s"    SIM  counts: ${simRow.toString}")
                println(s"    PEMS counts: ${pemsRow.toString}")
                //println(s"    BOOT counts: ${bootstrappedMatrix.toString}")

                // Compute fit statistics
                val diag = diagnose(pemsRow, simRow)
                val diag1 = diagnose(VectorD(totPemsRow), VectorD(totSimRow))
                val fit = FitM.fitMap(diag)
                //println(s"fit $fit ")
                val fit1 = FitM.fitMap(diag1)
                val rSq = fit("rSq")
                val rmse = fit("rmse")
                val smape = fit("smape")
                val mae = fit("mae")
                val sse = fit("sse")
                val sst = fit("sst")


                // Total counts fit
                //val smape_total = fit1("smape")
                //val rsme_total = fit1("rmse")
                //println(s"  Fit Statistics:  $diag")
                println(s" R² = $rSq, RMSE = $rmse, SMAPE = $smape, MAE = $mae, SSE = $sse, SST = $sst")
            end for
        end for
        super.fini(rep)
    end fini

    ////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation */
    simulate()
    waitFinished()
    Model.shutdown()       // to be removed when TrafficOptimization is used
end CalRoute101


//when cars get into



//----------------------|----------------------9|-------------------------------------: dll
//--------------------04|----------------8------|--------------------------------------: dll
//-------------------03-|----------7------------|--------------------------------------: dll
//----------------02----|----3-----------4------|--------------------------------------: doublylinkedlist
//----1------2--------11|                      |--------------------------------------: doublylinkedlist

//                    juns//
//1. all the cars in that lane 1 should all go to the offramp.
//2. either need to change lane or all use the offramp.
//3. either change lane or use offramp.


//route---(pahtway_lanes:4 , Vtransport_segment:3)
//Vehicles have rulls ---> when they pass that offramp, they can't get into that last lane;
//You could have an array of lane_ids that are acceptable, and when you pass that offramp,
// that lanes is no longet acceptable. and off limit to you.
// We might need a DTS, that can allow each point and space the geography of the road.

// When the second onramp is over the road have 5 lanes again.

//






