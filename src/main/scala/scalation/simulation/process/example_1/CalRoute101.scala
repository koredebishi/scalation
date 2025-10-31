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
    val config      = new TrafficConfig("/Mainline_VDS_Redwood_Creek_US101-N/1-404532ML.csv", rowTime, stream)
    val nt          = config.data.dim

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

                laneID = laneRV.igen
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



//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Compute SMAPE between simulation and PEMS data at co-located evaluation sensors.
//     *
//     * Validation Strategy (Apple-to-Apple Comparison):
//     *  - Each PEMS sensor is matched with the simulation junction at the SAME physical location
//     *  - This ensures we compare traffic state at identical points in the network after merge/diverge events
//     *
//     * Mainline Sensors (co-located PEMS : Simulation):
//     *  - ytrue(0) = 2-401834ML (PEMS after offramp)  : junc(1) = ssor1 (Sim after offramp)
//     *  - ytrue(1) = 3-401833ML (PEMS after onramp1)  : junc(2) = ssor2 (Sim after onramp1)
//     *  - ytrue(2) = 5-401652ML (PEMS after onramp2)  : junc(4) = ssor4 (Sim after onramp2)
//     *
//     * Ramp Inflow Sensors (at ramp entrance, upstream of merge point):
//     *  - onRampTotalsPerRow(0) = 1-410095OR (PEMS) : ramp_sensors(0) (Sim onramp1 entrance)
//     *  - onRampTotalsPerRow(1) = 2-410093OR (PEMS) : ramp_sensors(1) (Sim onramp2 entrance)
//     *
//     * Note: Sensor 1-404532ML drives the simulation (mainline source) but is NOT used for validation
//     * since it's the input, not an output to be validated.
//     */
////    def simRunVsPemsRun(): Array[Double] =
////        val ytrue = config.evalArrivalsPerRow
////        val onRampTotalsPerRow = config.onRampTotalsPerRow
////        Array(
////            smapeF(VectorD(ytrue(0)), junc(2).getCountMatrix.sumVr), // 2-401834ML, after offramp
////            smapeF(VectorD(ytrue(1)), junc(3).getCountMatrix.sumVr), // 3-401833ML, after onramp1
////            smapeF(VectorD(ytrue(2)), junc(5).getCountMatrix.sumVr), // 5-401652ML, after onramp2 (final evaluation)
////            smapeF(VectorD(onRampTotalsPerRow(0)), ramp_sensors(0).getCountMatrix.sumVr), // onramp1 inflow
////            smapeF(VectorD(onRampTotalsPerRow(1)), ramp_sensors(1).getCountMatrix.sumVr) // onramp2 inflow
////        )
////    end simRunVsPemsRun
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Compute RMSE values for all evaluation sensors.
//     * Extracts RMSE (index 5) from the QoF metrics for use in optimization.
//     *
//     * @return Array of RMSE values: [afterOfframp1, afterOnramp1, afterOnramp2, onramp1Inflow, onramp2Inflow]
//     */
//    //    def simRunVsPemsRunRMSE(): Array[Double] =
//    //        val qofMetrics = getQoFMetrics()
//    //        qofMetrics.map(qof => qof(5))  // Extract RMSE (index 5) from each QoF vector
//    //    end simRunVsPemsRunRMSE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute all QoF metrics (including RMSE, MAE, SMAPE) using ScalaTion's built-in FitM.diagnose().
     *  Returns a map of sensor names to their QoF vectors.
     *  QoF vector format: VectorD(rSq, sst, sse, sde, mse0, rmse, mae, smape, m)
     *  Indices: rmse=5, smape=7
     */
    def getQoFMetrics(): Array[VectorD] =
        val ytrue = config.evalArrivalsPerRow
        val onRampTotalsPerRow = config.onRampTotalsPerRow
        
        Array(
            diagnose(VectorD(ytrue(0)), junc(2).getCountMatrix.sumVr), // 2-404532ML, after offramp
            diagnose(VectorD(ytrue(1)), junc(3).getCountMatrix.sumVr), // 3-401834ML, after onramp1
            diagnose(VectorD(ytrue(2)), junc(5).getCountMatrix.sumVr), // 5-401929ML, after onramp2
            diagnose(VectorD(onRampTotalsPerRow(0)), ramp_sensors(0).getCountMatrix.sumVr), // onramp1 inflow
            diagnose(VectorD(onRampTotalsPerRow(1)), ramp_sensors(1).getCountMatrix.sumVr) // onramp2 inflow
        )
    end getQoFMetrics



    override def fini(rep: Int): Unit =
        Recorder.writeAllSensorStats(junc.toList ++ ramp_sensors.toList)
        
        val qofMetrics = getQoFMetrics()
        val names = Array("afterOfframp1", "afterOnramp1", "afterOnramp2", "onramp1Inflow", "onramp2Inflow")
        val pemIds = Array("3-401834ML", "4-401833ML", "5-401929ML", "ramp1", "ramp2")
        val simCounts = Array(
            junc(2).getCountMatrix.sumVr.sum,        // taking reading @ sensor 532|second junction.
            junc(3).getCountMatrix.sumVr.sum,        // no sensor here, we only have a joining point to take reading for offramps
            //junc(4).getCountMatrix.sumVr.sum,        // Onramp1 with sensor 834
            junc(5).getCountMatrix.sumVr.sum,        // Onramp2 with sensor 929
            ramp_sensors(0).getCountMatrix.sumVr.sum,
            ramp_sensors(1).getCountMatrix.sumVr.sum
        )
        val pemsCounts = Array(
            config.evalArrivalsPerRow(0).sum,
            config.evalArrivalsPerRow(1).sum,
            config.evalArrivalsPerRow(2).sum,
            config.onRampTotalsPerRow(0).sum,
            config.onRampTotalsPerRow(1).sum
        )

        for i <- names.indices do
            val rSq   = qofMetrics(i)(0)    // rSq is at index 0 in QoF vector
            val rmse  = qofMetrics(i)(5)   // rmse is at index 5 in QoF vector
            val smape = qofMetrics(i)(7)  // smape is at index 7 in QoF vector
            
            //easyW.println(s"PEMSID:${pemIds(i)} | PEMSCount:${pemsCounts(i)} | SimCount:${simCounts(i)} | SMAPE ${names(i)}: ${smape} | RMSE: ${rmse} ")
            println(s"PEMSID:${pemIds(i)} | PEMSCount:${pemsCounts(i)} | SimCount:${simCounts(i)} |R^2 ${names(i)}: $rSq | SMAPE ${names(i)}: ${smape} | RMSE: ${rmse} ")

        super.fini(rep)
    end fini

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation */
    simulate()
    waitFinished()
    //Model.shutdown()       // to be removed when TrafficOptimization is used
end CalRoute101