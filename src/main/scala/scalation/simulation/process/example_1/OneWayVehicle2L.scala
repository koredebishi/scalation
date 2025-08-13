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
import scalation.random._
import scalation.mathstat._
import scala.math.abs



@main def runOneWayVehicle2L(): Unit = new OneWayVehicle2L()

class OneWayVehicle2L(name: String = "OneWayVehicle2L", reps: Int = 1, animating: Boolean = true,
                      aniRatio: Double = 500.0, stream: Int = 0)
    extends Model(name, reps, animating, aniRatio)
        with RowTimeLoader
        with FitM:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Debugging and traffic data loading */
    private val debug       = debugf("OneWayVehicle2L", false)
    private val trafficData = new TrafficConfig("/15min_US101_N_Willow_to_Marsh_2miles_ML/400981.csv", rowTime, stream)
    private val nt          = trafficData.data.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Simulation dynamics and random variables */
    val motion         = GippsDynamics
    val numLanes        = 4
    val iArrivalRV     = Erlang()
//    val nStop          = trafficData.totalArrivalsPerRow.sum.toInt
    val nStop          = trafficData.nStopArray

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Delegate per‑source μ lookup to TrafficConfig */
    def getMuForSource(idx: Int): VectorD =
        trafficData.getMuForSource(idx)


    setTime(nt * rowTime)     // I need my time stamp formatted for easy passing for time calculations and advancement.
    // 00:00:00  ----> 00.00, 00.15



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animation coordinates and junction setup */
    val (w, h)    = (1000, 800)
    val shift     = 20.0

//    // Use TrafficConfig methods to get coordinates (eliminates redundant GPS loading)
//    val aniCoords_Main = trafficData.getMainlineCoordinates((w, h))
//    val aniCoords_Ramp = trafficData.getRampCoordinates((w, h))
//
//    // Create the classic highway structure: sensor1 to sensor7 (7 junctions, 6 segments)
//
//    val main_sensors = Array.ofDim[Junction](aniCoords_Main.length)
//    for i <- main_sensors.indices do
//        main_sensors(i) = new Junction(s"ssor${i}", xy = (aniCoords_Main(i)._1, aniCoords_Main(i)._2), nt = nt)
//
//    val ramp_sensors = Array.ofDim[Junction](aniCoords_Ramp.length)
//    for i <- ramp_sensors.indices do
//        ramp_sensors(i) = new Junction(s"ramp${i}", xy = (aniCoords_Ramp(i)._1 + 45, aniCoords_Ramp(i)._2 - 50), nt = nt)
//
//
//    /** Define animation center and offsets for the Vsouces offsets and positioning */
//    val mainPos = aniCoords_Main(0)    //the real GPS position of the first sensor
//    val centerPos = ((mainPos._1 + 100.0).toInt, (mainPos._2 + 100.0).toInt) //the nudge for Vsrc. take the Vsrc pos farther from the first sensor
//    val offsets = Array(
//        (0, 0),
//        ((aniCoords_Ramp(0)._1 + 130.0).toInt - centerPos._1, (aniCoords_Ramp(0)._2 - 300.0).toInt - centerPos._2),
//        ((aniCoords_Ramp(1)._1 + 130.0).toInt - centerPos._1, (aniCoords_Ramp(1)._2 - 300.0).toInt - centerPos._2),
//        ((aniCoords_Ramp(2)._1 + 130.0).toInt - centerPos._1, (aniCoords_Ramp(2)._2 - 300.0).toInt - centerPos._2)
//    )

    private val aniCoords_Main = trafficData.getMainlineCoordinates((w, h))
    private val aniCoords_Ramp = trafficData.getRampCoordinates((w, h)) // already nudged
    val (centerPos, offsets) = trafficData.getVSourceCenterAndOffsets((w, h))

    private val main_sensors: Array[Junction] = Array.ofDim[Junction](aniCoords_Main.length)
    for i <- main_sensors.indices do
        main_sensors(i) = new Junction(s"ssor$i", xy = aniCoords_Main(i), nt = nt)

    private val ramp_sensors = Array.ofDim[Junction](aniCoords_Ramp.length)
    for i <- ramp_sensors.indices do
        ramp_sensors(i) = new Junction(s"ramp$i", xy = aniCoords_Ramp(i), nt = nt) // no +45/-50 here

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create VSources inline so mySource is bound properly */
    val sources: List[VSource] = VSource.group(this, () => Car(), centerPos,
                                ("Vsrc", 0, Erlang(), nStop(0), offsets(0)),
                                ("srcRamp1", 1, Erlang(), nStop(1), offsets(1)),
                                ("srcRamp2", 2, Erlang(), nStop(2), offsets(2)),
                                ("srcRamp3", 3, Erlang(), nStop(3), offsets(3))
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create Sinks locally for correct mapping and final routing */
    val (x0, y0) = aniCoords_Main.last
    val (x1, y1) = aniCoords_Ramp(3)

    private val sinks = Sink.group((x0.toInt - 100, y0.toInt - 100),
        ("sinkMain", (0, 0)),
        ("sinkRamp", ((x1 + 230.0).toInt - (x0.toInt - 100), (y1 - 300.0).toInt - (y0.toInt - 100)))
    )


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create ramps inline to preserve subtype mapping */
    val ramps: Array[Ramp] = Ramp.group(motion,
                            ("onRamp1", sources(1), ramp_sensors(0), RampMode.On, 0.0, 0.0, RampControl.Metered, 540, 30),
                            ("onRamp2", sources(2), ramp_sensors(1), RampMode.On, 0.0, 0.0, RampControl.Metered, 720, 40),
                            ("onRamp3", sources(3), ramp_sensors(2), RampMode.On, 0.0, 0.0, RampControl.Freemerged),
                            ("offRamp", ramp_sensors(3), sinks(1), RampMode.Off, 0.0, 0.0)
    )
    
    

    private val intermediateJunctions = main_sensors.slice(1, main_sensors.length - 1)
    private val mainRoute = Route("Rte", numLanes, intermediateJunctions, main_sensors(0), main_sensors.last, motion)


    addComponents(sources.toList, main_sensors.toList ++ ramp_sensors.toList, sinks.toList, ramps.toList)
    mainRoute.pathway.foreach(addComponent(_))
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map each ramp index → segment index on the main route where it merges.
     * onRamp1 ↦ seg 0, onRamp2 ↦ seg 1, onRamp3 ↦ seg 5
     */
    private val rampJoinSeg = Array(0, 1, 5)
    @inline def pos(rampIdx: Int): Int = rampJoinSeg(rampIdx)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Define the Vehicle subclass with its own act method (Scalation style). */
    case class Car() extends Vehicle("c", this):

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

        /** Vehicle behavior: set initial speed, sample decisions, move, sense. */
        override def act(): Unit =
            Vehicle.setInitialSpeed(68.0 / 2.24694) // mph → m/s

            //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            // Random variables (Bernoulli) for tactical decisions
            val laneRV = trafficData.getLaneRV((clock.toInt / rowTime.toInt) % nt)
            val laneChangeRV = Bernoulli(0.8) // 80% attempt LC
            val offRampRV = Bernoulli(0.8) // 80% attempt exit

            //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            // Actor identity & initial lane (keep user’s naming)
            val i = subtype
            val j = if i == 0 then laneRV.igen else 0
            this.laneID = j

            //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            // Route references & indices (main sink, off-ramp sink/junction)
            val highWay = mainRoute
            val highWaySrc = 0
            val highWayExit = sinks(0)
            val offRampExit = sinks(1)
            val offRampIdx = 3 // off-ramp index
            val offRampSegment = main_sensors.length - 1 // last main segment

            //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            // Mainline vehicles
            if i == highWaySrc then
                val lane = highWay.pathway(j) // current lane handle
                val carAhead = lane.getLast // enqueue behind tail
                lane.addToAlist(this, carAhead)

                var exited = false // off-ramp taken?
                // Iterate segments up to (but not including) the last segment
                for seg <- 0 until offRampSegment if !exited do

                    //::::::::::::::::::::::::::::::::::::::::::::::::::::::
                    // Lane-change decision (probabilistic, 80%)
                    if laneChangeRV.igen == 1 then
                        val current = this.laneID
                        val target = (current + 1) % numLanes // adjacent lane (wrap)
                        if highWay.changeLane(current, target, this, seg) then
                            println(s"$this changed from lane $current @ seg:$seg to lane${this.laneID}--> target:$target")
                        end if
                    end if

                    //::::::::::::::::::::::::::::::::::::::::::::::::::::::
                    // Off-ramp decision BEFORE entering the last segment
                    if seg + 1 == offRampSegment && offRampRV.igen == 1 then
                        val currentLane = highWay.pathway(this.laneID)
                        currentLane.removeFromAlist(this) // detach from main

                        val offRamp = ramps(offRampIdx)
                        val carAheadO = offRamp.getLast
                        offRamp.addToAlist(this, carAheadO) // enter ramp queue (enter off-ramp list)

                        ramp_sensors(offRampIdx).jump() // sense at junction
                        offRamp.lane.move() // traverse ramp
                        offRamp.removeFromAlist(this)
                        offRampExit.leave() // exit network
                        exited = true
                    else
                        // Stay on mainline: move within seg, then record at next sensor
                        val currentLane = highWay.pathway(this.laneID)
                        currentLane.seg(seg).move()
                        main_sensors(seg + 1).jump()
                    end if
                end for

                //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                // If not exited via off-ramp, leave through main sink
                if !exited then
                    highWay.pathway(this.laneID).removeFromAlist(this)
                    highWayExit.leave()
                end if

            //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            // On-Ramp vehicles
            else
                val rampId = i - 1
                val ramp = ramps(rampId)
                val carAhead = ramp.getLast

                if (ramp.isOn) ramp.waitAtLimit()

                ramp.addToAlist(this, carAhead) // enter ramp
                ramp_sensors(rampId).jump()
                ramp.lane.move()
                ramp.removeFromAlist(this)

                // Merge onto main route (rightmost lane)
                this.laneID = 0
                val joinSeg = pos(rampId) // merge segment idx
                val mergeLane = highWay.pathway(0)
                val carAheadMain = mergeLane.getLast
                mergeLane.addToAlist(this, carAheadMain)

                var exited = false
                // Traverse from merge segment up to (but not including) last segment
                for seg <- joinSeg until offRampSegment if !exited do

                    // Off-ramp decision BEFORE entering the last segment
                    if seg + 1 == offRampSegment && offRampRV.igen == 1 then
                        val currentLane = highWay.pathway(this.laneID)
                        currentLane.removeFromAlist(this)

                        val offRamp = ramps(offRampIdx)
                        val carAheadO = offRamp.getLast
                        offRamp.addToAlist(this, carAheadO)

                        ramp_sensors(offRampIdx).jump()
                        offRamp.lane.move()
                        offRamp.removeFromAlist(this)
                        offRampExit.leave()
                        exited = true
                    else
                        val currentLane = highWay.pathway(this.laneID)
                        currentLane.seg(seg).move()
                        main_sensors(seg + 1).jump()
                    end if
                end for

                // If no off-ramp taken, leave through main sink after traversal
                if !exited then
                    highWay.pathway(this.laneID).removeFromAlist(this)
                    highWayExit.leave()
                end if
            end if
    end Car



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute SMAPE between simulation and PEMS data
     *Need to add the ramp sensors to this
     */
//    def simRunVsPemsRun(): Double =
//        val sensor3Arrival = trafficData.totalArrivalsPerRow     // no need for this for now
//
//        val scores = for sensor <- main_sensors ++ ramp_sensors yield smapeF(ytrue, sensor.getCountMatrix.sumVr)
//        scores.sum / scores.length
//    end simRunVsPemsRun

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Compute SMAPE between simulation and PEMS data at key measurement points */
    def simRunVsPemsRun(): Array[Double] =
        // SMAPE 1: After 2 onramps (sensor 3) - compare against sensor 402398 data
        val sensor3Expected = trafficData.getSensorData("402398")
        val smape1 = smapeF(sensor3Expected, main_sensors(3).getCountMatrix.sumVr)
        // SMAPE 2: After offramp (sensor 5) - compare against sensor 401927 data
        val sensor5Expected = trafficData.getSensorData("401927")
        val smape2 = smapeF(sensor5Expected, main_sensors(5).getCountMatrix.sumVr)
        // SMAPE 3: Final measurement (sensor 6) - compare against sensor 401653 data
        val sensor6Expected = trafficData.getSensorData("401653")
        val smape3 = smapeF(sensor6Expected, main_sensors(6).getCountMatrix.sumVr)
        Array(smape1, smape2, smape3)
    end simRunVsPemsRun

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Write stats and finalize simulation */
    override def fini(rep: Int): Unit =
        Recorder.writeAllSensorStats(main_sensors.toList ++ ramp_sensors.toList)
        val smapeResults = simRunVsPemsRun()
        println(f"SMAPE after 2 onramps: ${smapeResults(0)}")
        println(f"SMAPE after offramp: ${smapeResults(1)}")
        println(f"SMAPE after last onramp: ${smapeResults(2)}")
        super.fini(rep)
    end fini

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation */
    simulate()
    waitFinished()
    Model.shutdown()       // to be removed when TrafficOptimization is used
end OneWayVehicle2L


































/**
 * Create Array structure of components: (An array of Vsources).
 * Use the same order of the Array to put the Onramps on the road
 * then access the Sources via Index of the Array such that you can get the correct source at the correct time
 * No need of the name of the source. We just need to Index.
 * the substyoe of the Vehicle can help determing the source the vehicle came from
 * Array(Vsource1, Vsource2):
 * The Vehcles can enter the pathway via the based on the source and subtype:
 * Need a Map: -----> which source goes to which pathway via a Junction and the Vsource subtype
 * Source Index: Map to Pathway Index (junction) ---> Allows Vehicles to know which pathway to take based on the source they came from
 * junction to Sink for offRamp: Should model same for onRamp
 * Maybe a configuration file: And this needs to be exposed to Vehicles
 *
 * A method that creates this Vsoucres and add them to array then u can use them via the index of the array: Components in a loop
 *
 * A config
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Drive through main route with optional off-ramp exit
 * Should be based on the cars that trully exit the ramp since we have the on-ramp sensor data....
 * We may need a way to model this scene??????
 * //if we take 6AM-11AM. How many Vehicles exited at Marsh road?
 * //Once we have the data, we can use it to model the off-ramp exit probability
 * //We have to model this true scenerions
 * Currently we are using an offRampRV Bernoulli distribution to decide if the vehicle exits
 */

/** Main act method: choose lane and route based on subtype
 * We have to check the speed of the vehicles
 * then model the speed based on the data we are feeding the sensors
 * if we are doing 6AM to 12PM, then we have to model the speed of the vehicles
 * The data is already available in the sensor file
 * All we have to do is to also feed this speed same way we are feeding the arrival rate
 * Maybe the arival rate flow can help wit this speed modeling
 * Vehicle need to enter onramp or main route
 * Be added to the path's alist at the start
 * before you can drive or even change lanes of whats so ever
 * that logic must be sound and correct
 */







// The GPS coordinates for the junctions in the OneWayVehicle2L model
//37.468732, -122.154696   // 400981 → junc(0) → Mainline entry (Willow))
//37.468732, -122.154696   // 408267 → OnRamp from Willow → junc(1)
//37.469590, -122.156168   // 402398 → junc(1) → Mainline
//37.469675, -122.156317   // 408264 → OnRamp from Willow → junc(2)
//37.472754, -122.161627   // 404534 → junc(2) → Mainline
//37.474830, -122.165147   // 401474 → junc(3) → Mainline
//37.480426, -122.174688   // 400388 → junc(4) → Mainline
//37.482799, -122.179049   // 412784 → OffRamp to Marsh → junc(2)
//37.483144, -122.179850   // 401927 → junc(5) → Mainline
//37.483144, -122.179850   // 412783 → OnRamp from Marsh → junc(4)
//37.483811, -122.181671   // 401653 → junc(6) → Mainline exit  (Marsh)

