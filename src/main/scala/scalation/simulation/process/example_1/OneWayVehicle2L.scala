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
import scalation.simulation.process.Vehicle.getCarAhead

import scala.math.abs   //, max, min}


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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map each ramp index → segment index on the main route where it merges.
     * Adjust indices to match your network geometry.
     * Example: ramp1 ↦ seg0, ramp2 ↦ seg1, ramp3 ↦ seg5
     */
    val rampJoinSeg = Array(2, 3)
    @inline def pos(rampIdx: Int): Int = rampJoinSeg(rampIdx)
    var carAhead: Vehicle = null


    case class Car() extends Vehicle("c", this):
        override def act(): Unit =
            Vehicle.setInitialSpeed(68.0 / 2.24694)
            // speed profiles per lanes// ???
            val laneRV = config.getLaneRV((clock.toInt / rowTime.toInt) % nt)
            val laneChangeRV = Bernoulli(0.0) // 80% chance to attempt lane change
            val offRampRV = Bernoulli() // 50% chance to take off-ramp
            val highway_length = junc.length -1

            // ------------------ handle main entry vehicles -------------------
            if subtype == 0 then

                val useOffRamp = false          //offRampRV.igen < 0.5 // 50% chance to take off-ram
                // set lane first, then get handle
                this.laneID = laneRV.igen % numLanes
                var laneIdx = this.laneID
                val highway = route.path(laneIdx)   // get the correct lane Pathway

                // join the lane
                val carAhead = highway.getLast
                highway.addToAlist(this, carAhead)

                val offRampJunction = 1


                // drive until off-ramp junction
                for seg <- 0 until offRampJunction do
                    highway.seg(seg).move()
                    junc(seg + 1).jump()
                end for

                // at junction 2, decide whether to take off-ramp
                if useOffRamp then
                    highway.removeFromAlist(this)
                    println(s"I printed inside use offramp")
                    driveRamp(ramps(2))
                else
                    for seg <- offRampJunction until highway_length do
                        println(s" the loop for the second junc")

                        var lastlanechange = 20.0
                        val currentTime = clock - lastlanechange



                        if currentTime >= 20.0 then

                            val carAhead  = getCarAhead(this)

                            println(s" @@@@the loop for the second junc $carAhead")

                            if carAhead != null && carAhead.velocity < 0.9 * vmax then
                                val target = if laneID == 0 then 1
                                else if laneID == numLanes - 1 then numLanes - 2
                                else if Bernoulli(0.6).igen == 1 then laneID + 1
                                else laneID - 1

                                val currentlane = laneID
                                route.changeLane(laneID, target, this, seg)

                                println(s"$this changed@main from: $currentlane  to target: $target @ seg $seg")

                                lastlanechange = clock
                            end if

                        end if

                        highway.seg(seg).move()
                        junc(seg + 1).jump()
                    end for

                    highway.removeFromAlist(this)
                    sinks(0).leave()
                end if
            // ------------------ handle on-ramp entry vehicles -------------------
            else
             
                val onRamp = ramps(subtype - 1) // subtype 1,2 = onRamp1, onRamp2

                val pathInx = 0 // the path Index to enter after on-ramp merge
                this.laneID = pathInx // update laneID first

                driveRamp(onRamp)

                val highway  = route.path(this.laneID) // merging into rightmost lane lane 0

                val joinSeg = pos(subtype - 1)        // joining at junction (2,3) for ramp (1,2)
                println(s"$this joining at seg $joinSeg")

                val carAhead = highway.seg(joinSeg).getLast // get last vehicle in the joining segment
                highway.addToAlist(this, carAhead)

                for seg <- joinSeg until highway_length do

                    var lastlanechange = 20.0
                    val currentTime = clock - lastlanechange


                    if currentTime >= 20.0 then

                        val carAhead = getCarAhead(this)

                        println(s" @@@@the loop for the second junc $carAhead")

                        if carAhead != null && carAhead.velocity < 0.9 * vmax then
                            val target = if laneID == 0 then 1
                            else if laneID == numLanes - 1 then numLanes - 2
                            else if Bernoulli(0.6).igen == 1 then laneID + 1
                            else laneID - 1
                            route.changeLane(laneID, target, this, seg)
                            lastlanechange = clock
                        end if

                    end if

                    highway.seg(seg).move()
                    junc(seg + 1).jump()
                end for

                highway.removeFromAlist(this)
                sinks(0).leave()

        end act
//
//        private driveMainLine(comp: Component): Unit = comp match
//            case h: Pathway =>
//
//
//        end drive

        private def driveRamp(comp: Component): Unit = comp match
            case r: Ramp =>
                println(s"--> ${this.name} entering Ramp: ${r.name} this: ${this.laneID}")
                val carAhead = r.getLast
                r.addToAlist(this, carAhead)

                r.lane.move()

                if r.mode == RampMode.On then
                    r.to.asInstanceOf[Junction].jump()
                end if


                r.removeFromAlist(this)
                r.to match
                    case s: Sink => s.leave()
                    case _ => // For off-ramps, this should be a Sink

            case s: Sink =>
                println(s"==> ${this.name} reached Sink: ${s.name}")
                s.leave()
        end driveRamp

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


//
//                            var lastlaneChange = 20.0 //seconds
//                            if clock - lastlaneChange >= 20.0 then
//                                val goLeft = Bernoulli(0.6).igen == 1 // 60% left, 40% right
//                                val target = (laneIdx + 1) % numLanes
//                                if this.laneID == 0 then  1
//                                else if this.laneID == numLanes - 1 then  numLanes - 2
//                                else if goLeft then this.laneID + 1
//                                else this.laneID - 1
//
//                                if route.changeLane(this.laneID, target, this, seg) then
//                                    println(s"$this changed@main to lane ${this.laneID} @ seg $seg")
//                                    lastlaneChange = clock
//                                end if


//                            if route.changeLane(laneIdx, target, this, seg) then
//                                println(s"$this changed to lane $laneIdx @ seg $seg")
//
//                            end if
//end if



//
//    MatrixD Row 1: VectorD(3.00000,	5.00000,	4.00000,	3.00000,	5.00000): totalcount:20.0
//    MatrixD Row 1: VectorD(5.00000,	4.00000,	4.00000,	4.00000,	3.00000): totalcount:20.0
//    MatrixD Row 1: VectorD(25.0000,	4.00000,	4.00000,	5.00000,	2.00000): totalcount:40.0
//    MatrixD Row 1: VectorD(45.0000,	4.00000,	4.00000,	5.00000,	2.00000): totalcount:60.0
//    MatrixD Row 1: VectorD(45.0000,	4.00000,	5.00000,	4.00000,	2.00000): totalcount:60.0
//
//


// Need to check if the lane change print out curresponds to the counts in the lane matrix
// Add counts for exit ramps (offramps)
// make methods so that onramps vehicles:
// starts at different subtypes then once finished the ramp substype , join mainline.
// driveRamp(substype - 1)
// driveMainLine  (onRamp)
























