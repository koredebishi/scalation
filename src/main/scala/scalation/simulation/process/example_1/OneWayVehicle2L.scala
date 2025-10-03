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
//import scalation.simulation.process.Vehicle.getCarAhead

import scala.math.abs   //, max, min}


@main def runOneWayVehicle2L(): Unit = new OneWayVehicle2L()

class OneWayVehicle2L(name: String = "OneWayVehicle2L", reps: Int = 1, animating: Boolean = false,
                      aniRatio: Double = 500.0, stream: Int = 0)
    extends Model(name, reps, animating, aniRatio)
        with RowTimeLoader
        with FitM:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // ::
    /** Debugging and traffic data loading */
    val debug       = debugf("OneWayVehicle2L", false)
    val config      = new TrafficConfig("/Mainline_VDS_Redwood_Creek_US101-N/1-404532ML.csv", rowTime, stream)
    val nt          = config.data.dim

    val rand = Uniform(0.0, 1.0)              // probability uniform in [0,1)
    // val offrampFraction = config.exitFraction  // FIXED fraction (deprecated - replaced by dynamic per-row fraction below)
    // Dynamic off-ramp fraction will be computed inside Car.act using current simulation row index.
    val offRampMAWindow = 5  // Simple Moving Average window m; MA_t = (1/m) * Σ_{j=0}^{m-1} f_{t-j}




    private [process] val easyW = new EasyWriter("simulation", "OnewayVehicle2LModel.txt")



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
        // TrafficConfig now returns Array[Double]; wrap in VectorD
        VectorD(config.getMuForSource(idx).toIndexedSeq)

    setTime(nt * rowTime)     // I need my time stamp formatted for easy passing for time calculations and advancement.
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

    val rampJoinSeg = Array(2, 3)
    @inline def pos(rampIdx: Int): Int = rampJoinSeg(rampIdx)
    var carAhead: Vehicle = null


    case class Car() extends Vehicle("c", this):

        val offRampJunction = 1
        val highway_length = junc.length - 1
        val laneRV = config.getLaneRV((clock.toInt / rowTime.toInt) % nt)

        override def act(): Unit =
            Vehicle.setInitialSpeed(68.0 / 2.24694)


            //val offRampRV = Bernoulli() // 50% chance to take off-ramp



            // ------------------ handle main entry vehicles -------------------
            if subtype == 0 then
                // ------------------------------------------------------------------
                // FIXED OFF-RAMP DECISION (OLD IMPLEMENTATION - KEPT FOR ROLLBACK)
                // val u = rand.gen                           // single draw in [0,1)
                // val useOffRamp = u <= offrampFraction      // probability decision using fixed threshold
                // easyW.println(s"Mainline Vehicle ${this.displayLabel} (FIXED) useOffRamp=$useOffRamp draw=$u threshold=$offrampFraction")
                // ------------------------------------------------------------------
                // DYNAMIC OFF-RAMP DECISION (NEW): fraction varies with time row
                // Row index within cropped matrix based on current clock
                val rowIdx = ((clock / rowTime).toInt) % nt
                val currentOfframpFractionRaw = config.computeExitFraction(rowIdx)
                val currentOfframpFractionMA  = config.computeExitFractionMA(rowIdx, offRampMAWindow)
                val currentOfframpFraction = currentOfframpFractionMA
                val u = rand.gen
                val useOffRamp = u <=  currentOfframpFraction
//                easyW.println(s"Mainline Vehicle ${this.displayLabel} (DYNAMIC) rowIdx=$rowIdx frac=$currentOfframpFraction draw=$u useOffRamp=$useOffRamp")

                laneID = laneRV.igen % numLanes
                val carAhead = route.path(laneID).getLast

                route.path(laneID).addToAlist(this, carAhead)

                // drive until off-ramp junction// Universal for all vehicles.
                for seg <- 0 until offRampJunction do
                    junc(seg).jump()   // take recording at the first sensor (almost like the Vsource)
                    route.path(laneID).seg(seg).move()    // move at seg0 sensor0-------seg0
                    junc(seg + 1).jump()       // take recording at sensor1      seg0----sensor1
                end for

                // at junction 2, decide whether to take off-ramp
                if useOffRamp then
                    junc(offRampJunction).jump() // take recording at the sensor before offramp
                    route.path(laneID).removeFromAlist(this)   // take offramp, leave highway
                    driveRamp(ramps(2)) //
                else 
                    driveRamp(this)
                    driveHighway(this) // continue on highway driving.
                end if
            // ------------------ handle on-ramp entry vehicles -------------------
            else
                val onRamp = ramps(subtype - 1) // subtype 1,2 = onRamp1, onRamp2

                laneID = 0 // FORCED lane for ramp vehicles; consider delaying lane selection until merge.
                driveHighway(this)    // then drive the highway
        end act

        private def driveHighway(car: Car): Unit =
            var lastLaneChange = 20.0 // seconds
            val joinSeg = if subtype == 0 then offRampJunction else pos(subtype - 1)

            // for on-ramp vehicles, joinSeg = pos(subtype - 1)
            if subtype > 0 then
                val insertLane = 0 // laneRV.igen % numLanes
                laneID = insertLane

                val carAhead = route.path(laneID).seg(joinSeg).getLast
                route.path(laneID).addToAlist(this, carAhead)
                junc(joinSeg).jump()
            end if

            easyW.flush()
            val startSeg = joinSeg
            var seg = startSeg

            while seg < highway_length do
                // --- lane change at segment boundaries ---
                if clock - lastLaneChange >= 20.0 then
                    val carAhead = getCarAhead(this)
                    if carAhead != null && carAhead.velocity < 0.9 * vmax then
                        val target =
                            if laneID == 0 then 1
                            else if laneID == numLanes - 1 then numLanes - 2
                            else if laneChangeRV.igen == 1 then laneID + 1
                            else laneID - 1

                        val currentLane = laneID
                        route.changeLane(currentLane, target, this, seg)
                        lastLaneChange = clock
                    end if
                end if

                // --- advance vehicle along highway ---
                route.path(laneID).seg(seg).move()
                junc(seg + 1).jump()

                seg += 1
            end while

            route.path(laneID).removeFromAlist(this)
            println(s"To  mainline sink: count ${this.displayLabel}")
            easyW.println(s"Highway exit: $this leaving seg=$segId lane=$laneID at clock=$clock")
            sinks(0).leave()
        end driveHighway


        //        private def driveHighway(car: Car): Unit =
//            var lastLaneChange = 20.0 //seconds
//            //easyW.println(s"Inside the DH method to check me again ${this}:  laneID =$laneID")
//            //val highway = route.path(this.laneID) // mainline path for current lane, does does this worl for a vehicle that is just joining from onramp
//
//            val joinSeg = if subtype == 0 then offRampJunction else pos(subtype - 1)
//
//            //for on-ramp vehicles, joinSeg = pos(subtype - 1)
//            if subtype > 0 then
//                val insertLane = 0   //  laneRV.igen % numLanes    // using laneRV to help spread out vehicles joining from onramp
//                laneID = insertLane               // set the onramp car laneID to this random lane ID
//
//                val carAhead = route.path(laneID).seg(joinSeg).getLast  //get the carAhead inside the joined segment based on the random laneID
//
//                //easyW.println(s"Onramp Vehicle $this join at seg $joinSeg and CarAhead = $carAhead and laneID = $laneID")
//                route.path(laneID).addToAlist(this, carAhead)
//
//                junc(joinSeg).jump() // take recording at the sensor where it joins
//
//                easyW.println(s"Onramp vehicle added to highway list $this join at seg $joinSeg and CarAhead = $carAhead and laneID = $laneID")
//            end if
//
//            easyW.flush()
//            val startSeg = joinSeg
//
//            for seg <- startSeg until highway_length do
//                // --- lane change at segment boundaries ---
//                //easyW.println(s"Highway and moving $this join at seg $joinSeg and CarAhead = ${this.getCarAhead(this)} and laneID = $laneID")
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
//
//                // --- advance vehicle along highway ---
//                route.path(laneID).seg(seg).move()
//                junc(seg + 1).jump()
//
//            end for
//
//            //println(s"Before removal from mainline: count ${this.displayLabel}")
//            route.path(laneID).removeFromAlist(this)
//            easyW.println(s"Highway exit: $this leaving seg=$segId lane=$laneID at clock=$clock")
//            println(s"To  mainline sink: count ${this.displayLabel}")
//            sinks(0).leave()
//
//        end driveHighway


        private def driveRamp(comp: Component): Unit = comp match
            case r: Ramp =>
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
                    case _ =>
            case s: Sink => s.leave()
        end driveRamp


    end Car



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute SMAPE between simulation and PEMS data at 3 fixed sensors, using upstream sensor data
     * 2-401834ML", "3-401833ML", "5-401652ML */

    def simRunVsPemsRun(): Array[Double] =
        val ytrue = config.evalArrivalsPerRow
        val onRampTotalsPerRow = config.onRampTotalsPerRow
        Array(
            smapeF(VectorD(ytrue(0)), junc(1).getCountMatrix.sumVr), // 2-401834ML, after offramp
            smapeF(VectorD(ytrue(1)), junc(2).getCountMatrix.sumVr), // 3-401833ML, after onramp1
            smapeF(VectorD(ytrue(2)), junc(4).getCountMatrix.sumVr), // 5-401652ML, upstream sensor After all ramps
            smapeF(VectorD(onRampTotalsPerRow(0)), ramp_sensors(0).getCountMatrix.sumVr), // onramp1 inflow
            smapeF(VectorD(onRampTotalsPerRow(1)), ramp_sensors(1).getCountMatrix.sumVr) // onramp2 inflow
        )



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Write stats and finalize simulation */
    override def fini(rep: Int): Unit =
        Recorder.writeAllSensorStats(junc.toList ++ ramp_sensors.toList)
        val smapeResults = simRunVsPemsRun()
        easyW.println(f"SMAPE after offramp1: ${smapeResults(0)}")
        easyW.println(f"SMAPE after onramp1 : ${smapeResults(1)}")
        easyW.println(f"SMAPE after onramp2 : ${smapeResults(2)}")
        easyW.println(f"SMAPE onramp1 inflow: ${smapeResults(3)}")
        easyW.println(f"SMAPE onramp2 inflow: ${smapeResults(4)}")
        easyW.flush()
        super.fini(rep)
    end fini



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation */
    simulate()
    waitFinished()
    Model.shutdown()       // to be removed when TrafficOptimization is used
end OneWayVehicle2L



//To do list: 1. See the smape values generated and compare it with normal
// 2. Once 1 is achieved, go to the objective function part and see if it works from there.
//3. Once 1 & 2 is achieved, add the optimizer part and see if that works too.
//4. Setup the sapello two work order for this simulation. Might be cutting out the library you need for this project from scalation.
