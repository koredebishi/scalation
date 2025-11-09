//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
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
    // ::
    /** Debugging and traffic data loading */
    val debug       = debugf("CalRoute101", false)
    val config      = new TrafficConfig("1-404531ML", rowTime, stream)
    val nt          = config.dim  // Number of time rows in anchor data

    val rand = Uniform(0.0, 1.0)              // probability uniform in [0,1)


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Simulation dynamics and random variables */
    val motion         = GippsDynamics
    val numLanes        = 5
    val iArrivalRV     = Erlang()
    val nStop          = config.nStopArray
    val laneChangeRV       = Bernoulli(0.6)


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Delegate per‑source μ lookup to TrafficConfig */
    def getMuForSource(idx: Int): VectorD =
        VectorD(config.getMuForSource(idx))

    setTime((nt) * rowTime)     // I need my time stamp formatted for easy passing for time calculations and advancement.
    // 00:00:00  ----> 00.00, 00.15

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animation coordinates and junction setup */
    val (w, h) = (1500, 1500)
    val shift = 20.0

    val aniCoords_Main = config.getMainlineCoordinates((w, h))
    val aniCoords_Ramp = config.getRampCoordinates((w, h)) // already nudged
    val (centerPos, offsets) = config.getVSourceCenterAndOffsets((w, h))

    val junc: Array[Junction] = Array.ofDim[Junction](aniCoords_Main.length)
    for i <- junc.indices do
        junc(i) = new Junction(s"ssor$i", xy = aniCoords_Main(i), nt = nt)

    val ramp_sensors = Array.ofDim[Junction](aniCoords_Ramp.length)
    for i <- ramp_sensors.indices do
        ramp_sensors(i) = new Junction(s"ramp${i+1}", xy = aniCoords_Ramp(i), nt = nt)


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create VSources inline so mySource is bound properly */
    val sources: List[VSource] = VSource.group(this, () => Car(), centerPos,
                                ("Vsrc", 0, Erlang(), nStop(0), offsets(0)),
                                ("srcRamp1", 1, Erlang(), nStop(1), offsets(1)),
                                ("srcRamp2", 2, Erlang(), nStop(2), offsets(2))
    )


//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Create VSources inline so mySource is bound properly */
//    val sources: List[VSource] = VSource.group(this, () => Car(), centerPos,
//        ("Vsrc", 0, Erlang(), 3, offsets(0)),
//        ("srcRamp1", 1, Erlang(), 4, offsets(1)),
//        ("srcRamp2", 2, Erlang(), 4, offsets(2))
//    )


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create Sinks locally for correct mapping and final routing */
    val (x0, y0) = (aniCoords_Main.last._1 - 100.0, aniCoords_Main.last._2 - 100.0)
    val (s, y) = (aniCoords_Ramp(2)._1, aniCoords_Ramp(2)._2) // ramp end sensor
    private val sinks = Sink.group((x0.toInt, y0.toInt),
                    ("sinkMain", (0, 0)),
                    ("sinkRamp", ((s + 800.0).toInt - x0.toInt, (y - 350).toInt - y0.toInt))
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create ramps inline to preserve subtype mapping.
     * First 3 = onramps; last = offramp.
     */
    val ramps: Array[Ramp] = Ramp.group(motion,
                ("onRamp1", sources(1), ramp_sensors(0), RampMode.On, 0.0, 0.0),
                ("onRamp2", sources(2), ramp_sensors(1), RampMode.On, 0.0, 0.0),
                ("offRamp", ramp_sensors(2), sinks(1), RampMode.Off, 0.0, 0.0)
    )
    private val intermediateJunc = junc.slice(1, junc.length - 1)
    private val route = Route("Rte", numLanes, intermediateJunc, junc(0), junc.last, motion)

    addComponents(sources, junc.toList ++ ramp_sensors.toList, sinks, ramps.toList)  // effectively this becomes
    route.pathway.foreach(addComponent(_))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map each ramp index → segment index on the main route where it merges.
     * Adjust indices to match your network geometry.
     * Example: ramp1 ↦ seg0, ramp2 ↦ seg1, ramp3 ↦ seg5
     */

    val rampJoinSeg = Array(3, 4)    // hardcoded part needs generalization for the ramp joining segment.
    @inline def pos(rampIdx: Int): Int = rampJoinSeg(rampIdx)
    var carAhead: Vehicle = null

    Vehicle.setInitialSpeed(68.0 / 2.24694)

    case class Car() extends Vehicle("c", this):


        val offRampJunction = 2


        val highway_length = junc.length - 1
        val laneRV = config.getLaneRV((clock.toInt / rowTime.toInt) % nt)

        override def act(): Unit =

            // ------------------ handle main entry vehicles -------------------
            if subtype == 0 then       // subtype 0 = mainline vehicle
                val currentOfframpFractionMA = config.exitFractionMA(curRow)

                val u = rand.gen
                //println(s"NEW: RowIdx: $curRow | OfframpFraction: $currentOfframpFractionMA | RandomU: $u ")
                val useOffRamp = u <= currentOfframpFractionMA

//                 ============================================================================
//                 OLD LANE ASSIGNMENT (Commented out - caused Lane 5 vehicles at 4-lane sensor)
//                 ============================================================================
                 laneID = 4 - laneRV.igen  // Randomly assigned lane 0-4, ignoring exit decision

//                 ============================================================================
//                 NEW LANE ASSIGNMENT (Exit-aware lane selection)
//                 Reason: Sensor 3 (401834) has only 4 lanes after offramp divergence
//                 Lane 5 vehicles must either exit or merge before reaching sensor 3
//                 Strategy: Assign Lane 5 ONLY to vehicles that will exit
//                 ============================================================================
//                if useOffRamp then
//                    laneID = 4  // Lane 5 (0-indexed) - exit lane for offramp-bound vehicles
//                else
//                    laneID = laneRV.igen % 4  // Restrict to lanes 0-3 (through traffic only)
//                end if

                val carAhead = route.path(laneID).getLast

                route.path(laneID).addToAlist(this, carAhead)

                // drive until off-ramp junction// Universal for all vehicles.
                cfor (0 , offRampJunction) { seg =>
                    junc(seg).jump() // take recording at the first sensor (almost like the Vsource)
                    route.path(laneID).seg(seg).move() // move at seg0 sensor0-------seg0
                    //junc(seg + 1).jump() //record all vehicle (Some vehicle will use offramp after here)
                }

                // at junction 2, decide whether to take off-ramp
                if useOffRamp then
                    //junc(offRampJunction).jump() // take recording at the sensor before offramp
                    route.path(laneID).removeFromAlist(this)   // take offramp, leave highway
                    driveRamp(ramps(2)) //
                else
                    junc(offRampJunction).jump() // take recording at the sensor before offramp
                    driveHighway() // continue on highway driving.
                end if
            // ------------------ handle on-ramp entry vehicles -------------------
            else
                val onRamp = ramps(subtype - 1) // subtype 1,2 = onRamp1, onRamp2

                laneID = 0 // FORCED lane for ramp vehicles; consider delaying lane selection until merge.
                driveRamp(onRamp)
                driveHighway()    // then drive the highway
        end act

        private def driveHighway(): Unit =
            var lastLaneChange = 20.0 //seconds

            val joinSeg = if subtype == 0 then offRampJunction else pos(subtype - 1)       // the offrampJunc is those Highway vehicle that did not use the offramp

            if subtype > 0 then
                val carAhead = route.path(laneID).seg(joinSeg).getLast  //get the carAhead inside the joined segment based on the random laneID
                route.path(laneID).addToAlist(this, carAhead)
                junc(joinSeg).jump() // take recording at the sensor where it joins

                //easyW.println(s"Onramp vehicle added to highway list $this join at seg $joinSeg and CarAhead = $carAhead and laneID = $laneID")
            end if

            cfor (joinSeg , highway_length) { seg =>
                //easyW.println(s"Highway and moving $this join at seg $joinSeg and CarAhead = ${this.getCarAhead(this)} and laneID = $laneID")

                //------------ lane change at segment boundaries ---
//                if clock - lastLaneChange >= 20.0 then
//                    val carAhead = getCarAhead(this)
//                    if carAhead != null && carAhead.velocity < 0.9 * vmax then
//                        val target =
//                            if laneID == 0 then 1
//                            else if laneID == numLanes - 1 then numLanes - 2
//                            else if laneChangeRV.igen == 1 then laneID + 1
//                            else laneID - 1
//
//                        val currentLane = laneID
//                        route.changeLane(currentLane, target, this, seg)
//
//                        lastLaneChange = clock
//                    end if
//                end if
                // ---------------END lane change at segment boundaries ---

                // --- advance vehicle along highway ---
                route.path(laneID).seg(seg).move()
                junc(seg + 1).jump()
            }

            route.path(laneID).removeFromAlist(this)
            //easyW.println(s"Highway exit: $this leaving seg=$segId lane=$laneID at clock=$clock")
            //println(s"To  mainline sink: count ${this.displayLabel}")
            sinks(0).leave()

        end driveHighway


        private def driveRamp(comp: Component): Unit =

            val r = comp.asInstanceOf[Ramp]
            val carAhead = r.getLast                  // always null because ramp.vList empty
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

//    override def fini(rep: Int): Unit =
//        Recorder.writeAllSensorStats(junc.toList ++ ramp_sensors.toList)
//        //val qofMetrics = getQoFMetrics()
//        val mainlineSimCounts = Array(
//            junc(0).getCountMatrix,        // taking reading @ sensor 531|first junction.
//            junc(1).getCountMatrix,        // taking reading @ sensor 532|first junction.
//            junc(2).getCountMatrix,        // offramp exit
//            junc(3).getCountMatrix,        // Onramp1 entry
//            junc(4).getCountMatrix        // Onramp2 entry
////            MatrixD(ramp_sensors(0).getCountMatrix(?, 0)).𝐓,  // Extract column 0 only (active ramp lane)  // single row comparism
////            MatrixD(ramp_sensors(1).getCountMatrix(?, 0)).𝐓  // Extract column 0 only (active ramp lane)
//        )
//        val pemsCounts = Array(
//            config.getPemsCountMatrix(0),  // sensor1
//            config.getPemsCountMatrix(1),   // sensor2
//            config.getPemsCountMatrix(2),   // sensor3
//            config.getPemsCountMatrix(3),   // sensor4
//            config.getPemsCountMatrix(4)  // sensor5
////            config.getPemsCountRampMatrix(0),   // ramp sensor1
////            config.getPemsCountRampMatrix(1),   // ramap
//        )
//
////
//
////        for i <- simCounts.indices do
////            println(s" Sensor $i: SimCounts = ${simCounts(i).toString}, \n PEMSCounts $i = ${pemsCounts(i).toString} \n ")
//
//        for i <- mainlineSimCounts.indices do      // loop over Array of sensors
//            val simMatrix = mainlineSimCounts(i)
//            val pemsMatrix = pemsCounts(i)
//            for j <- 0 until simMatrix.dim do                 // loop over time intervals (rows)
//                val simRow = simMatrix(j)
//                val pemsRow = pemsMatrix(j)
//                //println(s" Sensor $i: row $j : SIMCount: ${simRow.toString}: PEMSCounts: ${pemsRow.toString} \n ")
//                val diag = diagnose(pemsRow, simRow)
//                println(s"Sensor $i, Row $j: ${FitM.fitMap(diag)}")
//        end for
//        super.fini(rep)
//    end fini

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

                // Total counts fit
                val smape_total = fit1("smape")
                val rsme_total = fit1("rmse")

                println(s" R² = $rSq, RMSE = $rmse, SMAPE = $smape, MAE = $mae , Smape15min = $smape_total, Rmse15min = $rsme_total")
            end for
        end for
//
//        println("\n" + "=" * 80)
//        println("MAINLINE COLUMN-WISE VALIDATION: Each Lane Across Time (6:00-6:45)")
//        println("=" * 80)
//
//        // Loop over each mainline sensor
//        for i <- 0 until 5 do
//            val simMatrix = junc(i).getCountMatrix // Simulation counts at junction i
//            val pemsMatrix = config.getPemsCountMatrix(i) // PEMS ground truth for sensor i
//
//            println(s"\n--- Sensor $i: junc($i) vs ${sensorNames(i)} ---")
//
//            // Loop over each lane (column)
//            for lane <- 0 until 5 do
//                val simCol = simMatrix(?, lane)    // Simulation: [row0, row1, row2, row3] for this lane
//                val pemsCol = pemsMatrix(?, lane)  // PEMS:       [row0, row1, row2, row3] for this lane
//
//                // What we are comparing
//                println(s"  Lane $lane across all time:")
//                println(s"    SIM  counts: ${simCol.toString}")
//                println(s"    PEMS counts: ${pemsCol.toString}")
//
//                // Compute fit statistics
//                val diag = diagnose(pemsCol, simCol)
//                val fit = FitM.fitMap(diag)
//                val rSq = fit("rSq")
//                val rmse = fit("rmse")
//                val smape = fit("smape")
//                val mae = fit("mae")
//
//                println(s"    R² = $rSq, RMSE = $rmse, SMAPE = $smape, MAE = $mae")
//            end for
//        end for
//
//        println("\n" + "=" * 80 + "\n")

        super.fini(rep)
    end fini

////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation */
    simulate()
    waitFinished()
    Model.shutdown()       // to be removed when TrafficOptimization is used
end CalRoute101





// 3hours, range of































// column wise
//        println("\n" + "=" * 80)
//        println("MAINLINE COLUMN-WISE VALIDATION: Each Lane Across Time (6:00-6:45)")
//        println("=" * 80)
//
//        // Loop over each mainline sensor
//        for i <- 0 until 5 do
//            val simMatrix = junc(i).getCountMatrix // Simulation counts at junction i
//            val pemsMatrix = config.getPemsCountMatrix(i) // PEMS ground truth for sensor i
//
//            println(s"\n--- Sensor $i: junc($i) vs ${sensorNames(i)} ---")
//
//            // Loop over each lane (column)
//            for lane <- 0 until 5 do
//                val simCol = simMatrix(?, lane)    // Simulation: [row0, row1, row2, row3] for this lane
//                val pemsCol = pemsMatrix(?, lane)  // PEMS:       [row0, row1, row2, row3] for this lane
//
//                // What we are comparing
//                println(s"  Lane $lane across all time:")
//                println(s"    SIM  counts: ${simCol.toString}")
//                println(s"    PEMS counts: ${pemsCol.toString}")
//
//                // Compute fit statistics
//                val diag = diagnose(pemsCol, simCol)
//                val fit = FitM.fitMap(diag)
//                val rSq = fit("rSq")
//                val rmse = fit("rmse")
//                val smape = fit("smape")
//                val mae = fit("mae")
//
//                println(s"    R² = $rSq, RMSE = $rmse, SMAPE = $smape, MAE = $mae")
//            end for
//        end for








//
//        // Ramp names for clear output
//        val rampNames = Array(
//            "PEMS Onramp1 (410095OR)",
//            "PEMS Onramp2 (410093OR)",
//            "PEMS Offramp (410094FR)"
//        )
//
//        println("\n" + "=" * 80)
//        println("RAMP VALIDATION: Comparing Simulation vs PEMS Ground Truth")
//        println("=" * 80)
//
//        // Loop over each ramp sensor
//        for i <- 0 until 3 do
//            // Extract single column for ramps (they only have 1 lane)
//            val simMatrix = MatrixD(ramp_sensors(i).getCountMatrix(?, 0)).𝐓 // Simulation counts at ramp i
//            val pemsMatrix = config.getPemsCountRampMatrix(i) // PEMS ground truth for ramp i
//
//            println(s"\n--- Ramp Sensor $i: ramp_sensors($i) vs ${rampNames(i)} ---")
//
//            // Loop over each time row
//            for row <- 0 until simMatrix.dim do
//                val simRow = simMatrix(row) // Simulation: [total_flow]
//                val pemsRow = pemsMatrix(row) // PEMS:       [total_flow]
//
//                // What we are comparing
//                println(s"  Row $row:")
//                println(s"    SIM  counts: ${simRow.toString}")
//                println(s"    PEMS counts: ${pemsRow.toString}")
//
//                // Compute fit statistics
//                val diag = diagnose(pemsRow, simRow)
//                val fit = FitM.fitMap(diag)
//                //                val rSq = fit("rSq")
//                val rmse = fit("rmse")
//                val smape = fit("smape")
//                val mae = fit("mae")
//
//                println(s" RMSE = $rmse, SMAPE = $smape, MAE = $mae ")
//            end for
//        end for
//


// ============================================================================
// RAMP VALIDATION
// ============================================================================

//        // ============================================================================
//        // RAMP COLUMN-WISE VALIDATION (Temporal: Ramp flow across all time)
//        // ============================================================================
//
//        println("\n" + "=" * 80)
//        println("RAMP COLUMN-WISE VALIDATION: Ramp Flow Across Time (6:00-6:45)")
//        println("=" * 80)
//
//        // Loop over each ramp sensor
//        for i <- 0 until 3 do
//            // Extract single column for ramps (they only have 1 lane)
//            val simMatrix = MatrixD(ramp_sensors(i).getCountMatrix(?, 0)).𝐓
//            val pemsMatrix = config.getPemsCountRampMatrix(i)
//
//            println(s"\n--- Ramp Sensor $i: ramp_sensors($i) vs ${rampNames(i)} ---")
//
//            // Extract all time rows for the single ramp lane
//            val simCol = simMatrix(?, 0)    // Simulation: [row0, row1, row2, row3] for ramp
//            val pemsCol = pemsMatrix(?, 0)  // PEMS:       [row0, row1, row2, row3] for ramp
//
//            // What we are comparing
//            println(s"  Ramp flow across all time:")
//            println(s"    SIM  counts: ${simCol.toString}")
//            println(s"    PEMS counts: ${pemsCol.toString}")
//
//            // Compute fit statistics
//            val diag = diagnose(pemsCol, simCol)
//            val fit = FitM.fitMap(diag)
//            val rSq = fit("rSq")
//            val rmse = fit("rmse")
//            val smape = fit("smape")
//            val mae = fit("mae")
//
//            println(s"    R² = $rSq, RMSE = $rmse, SMAPE = $smape, MAE = $mae")
//        end for









// 5                     5                  4                 5               5
////1---------------------2------------------3----------------4----------------5
////                             5th_turns offramp----------
//  //                              offramp              onramp1         onramp2


// We have to assume that , at the cars in the outter most lanes coming from sensor2 that they will be taking the offramp,
                                // and at that point in 2018, the highway goes from 5 lanes to 4 lanes after the offramp divergence.
                                // At sensor3, the highway has 4 lanes that is going on, the outter lanes goes into the onramps
                                // the cars coming from the onramps are going to create the 5th lane again.// Sensor 3 and 4 are really 4 lanes
                                //while 1, 2 and 5 are 5 lanes.
                                // correct the structure of the simulation a bit.
                                // Cold start issue: 15min before simulation chopped off.   5:45PM

//
//1.5 lanes -------> normal 5 segments
//2.5 lanes -------> normal 4 segment , 5th segment turns offramp
//3 4 lanes --------> normal 4 segment, does not have a 5th lane because the 5th lane turned offramp from sensor2, and there is an onramp coming up so no space (the diff between sensor3 - sensor2 = offramps cars)
//// if the diff between the count from 2-3 does not drop then we there might be an issue.
//4 5 lanes --------> normal 4 lanes, 5th lane is vehicles using the onramp1, ie coming from onramp1 ( sensor 4 takes the count of onramp1 vehicles)
//5 5 lanes -------- this takes the count of onramp2 vehicles.
//
//1. We have to assume that the last lane coming from sensor2 is protruding to an offramp. // if a route segment has a offramp ahead, the outer most lane is the offramp lane.
//2. this is the reason why At sensor3, the highway has 4 lanes that is going on since the 5th lane coming from sensor2 is taking the offramp.
//3. at sensor4 we have 5 lanes again because of onramp1 vehicles coming in. so the last lane is onramp1 vehicles traffic
//    4. at sensor5 we have 5 lanes again because of onramp2 vehicles coming in. so the last lane is onramp2 vehicles traffic, but because that sensor is far upfront, everything blends in.
//
//













