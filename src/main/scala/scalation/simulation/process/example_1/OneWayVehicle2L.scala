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

import scala.math.{abs}   //, max, min}


@main def runOneWayVehicle2L(): Unit = new OneWayVehicle2L()

class OneWayVehicle2L(name: String = "OneWayVehicle2L", reps: Int = 1, animating: Boolean = true,
                      aniRatio: Double = 500.0, stream: Int = 0)
    extends Model(name, reps, animating, aniRatio)
        with RowTimeLoader
        with FitM:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // ::
    /** Debugging and traffic data loading */
    val debug       = debugf("OneWayVehicle2L", false)
    val config = new TrafficConfig("/Mainline_VDS_Redwood_Creek_US101-N/404532ML.csv", rowTime, stream)
    val nt          = config.data.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Simulation dynamics and random variables */
    val motion         = GippsDynamics
    val numLanes        = 5
    val iArrivalRV     = Erlang()
//    val nStop          = trafficData.totalArrivalsPerRow.sum.toInt
    val nStop          = config.nStopArray
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Delegate per‑source μ lookup to TrafficConfig */
    def getMuForSource(idx: Int): VectorD =
        config.getMuForSource(idx)

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
        ramp_sensors(i) = new Junction(s"ramp$i", xy = aniCoords_Ramp(i), nt = nt) // no +45/-50 here

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

    var carAhead: Vehicle = null

    case class Car() extends Vehicle("c", this):
        override def act(): Unit =
            Vehicle.setInitialSpeed(68.0 / 2.24694)
            val laneRV = config.getLaneRV((clock.toInt / rowTime.toInt) % nt)
            val laneChangeRV = Bernoulli() // 80% chance to attempt lane change
            val offRampRV = Bernoulli() // 50% chance to take off-ramp
            var laneIdx = laneRV.igen % numLanes // pick an initial lane
            this.laneID = laneIdx

            if subtype == 0 then
                // Main entry vehicles - decide route at the beginning
                val useOffRamp = offRampRV.igen > 0.5

                println(s"I passed this face $laneIdx")
                // ------------------ enter chosen lane ----------------------------
                val path = route.path(laneIdx)
                val carAhead = path.getLast
                println(s"carAhead = $carAhead")
                path.addToAlist(this, carAhead)

                println(s"I passed this face")
                // ------------------ drive through segments up to junction 2 ------
                val offRampJunction = 1
                val highway_length = junc.length - 1
                for seg <- 0 until highway_length do      // this is the problem
                    if laneChangeRV.igen == 1 then
                        val target = (laneIdx + 1) % numLanes // simple rule
                        if route.changeLane(laneIdx, target, this, seg) then
                            //laneIdx = target // update on success
                            println(s"$this changed to lane $laneIdx @ seg $seg")
                        end if
                    end if
                    route.path(laneIdx).seg(seg).move()
                    if seg < highway_length then junc(seg+1).jump()
                end for

                // At junction 2, decide whether to take off-ramp
                if useOffRamp then
                    val offRamp = ramps(2)
                    route.path(laneIdx).removeFromAlist(this)
                    drive(offRamp)
                else
                    // ------------------ drive through remaining segments ----------
                    for seg <- offRampJunction until route.segments do
                        if laneChangeRV.igen == 1 then
                            val target = (laneIdx + 1) % numLanes // simple rule
                            if route.changeLane(laneIdx, target, this, seg) then
                                //laneIdx = target // update on success
                                println(s"$this changed to lane $laneIdx @ seg $seg")
                            end if
                        end if
                        route.path(laneIdx).seg(seg).move()
                        if seg < junc.length then junc(seg).jump()
                    end for

                    route.path(laneIdx).removeFromAlist(this)
                    sinks(0).leave()
                end if


            // ------------------ handle on-ramp entry vehicles -------------------
            else
                val onRamp = ramps(subtype - 1) // subtype 1,2 = onRamp1, onRamp2

                drive(onRamp)

                // Now continue on the route starting from segment 1
                laneIdx = 0
                val carAhead = route.getLast(laneIdx)
                route.path(laneIdx).addToAlist(this, carAhead)

                for seg <- 1 until route.segments do
                    if laneChangeRV.igen == 1 then
                        val target = (laneIdx + 1) % numLanes
                        if route.changeLane(laneIdx, target, this, seg) then
                            println(s"$this changed to lane $laneIdx @ seg $seg")
                    //laneIdx = target
                    end if
                    route.path(laneIdx).seg(seg).move()
                    if seg < junc.length then junc(seg).jump()
                end for

                route.path(laneIdx).removeFromAlist(this)
                sinks(0).leave()
        end act

        private def drive(comp: Component): Unit = comp match
            case r: Ramp =>
                println(s"--> ${this.name} entering Ramp: ${r.name}")
                val carAhead = r.getLast
                r.addToAlist(this, carAhead)

                r.lane.move()

                r.removeFromAlist(this)
                r.to match
                    case s: Sink => s.leave()
                    case _ => // For off-ramps, this should be a Sink

            case s: Sink =>
                println(s"==> ${this.name} reached Sink: ${s.name}")
                s.leave()
        end drive

    end Car



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Compute SMAPE between simulation and PEMS data at 3 fixed sensors */
    def simRunVsPemsRun(): Array[Double] =
        val ytrue = config.evalArrivalsPerRow
        Array(
            smapeF(ytrue(0), junc(1).getCountMatrix.sumVr), // 401834ML
            smapeF(ytrue(1), junc(2).getCountMatrix.sumVr), // 401833ML
            smapeF(ytrue(2), junc(3).getCountMatrix.sumVr) // 401929ML
        )
    end simRunVsPemsRun


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Write stats and finalize simulation */
    override def fini(rep: Int): Unit =
        Recorder.writeAllSensorStats(junc.toList ++ ramp_sensors.toList)
        val smapeResults = simRunVsPemsRun()
        println(f"SMAPE after offramp1: ${smapeResults(0)}")
        println(f"SMAPE after onramp1 : ${smapeResults(1)}")
        println(f"SMAPE after onramp2 : ${smapeResults(2)}")
        super.fini(rep)
    end fini

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation */
    simulate()
    waitFinished()
    Model.shutdown()       // to be removed when TrafficOptimization is used
end OneWayVehicle2L


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//Very important notes to self:
//Plot the arrival rates of my date with time.
//Number of vehicle vs Time. <-----: y-axis vs x-axis
//Use the data from the csv file to plot the graph.
//Also measure the departure rate from the sink.
//Compare the arrival rate and departure rate.
//per lane arrival rate and departure rate.

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//y = probability density function;
//x = inter-arrival time
//mean = average inter-arrival time
//variance = variance of inter-arrival time
//standard deviation = standard deviation of inter-arrival time