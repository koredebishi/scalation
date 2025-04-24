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
//import scalation.simulation.process.modeling.clustering.Coordinates
import scalation.mathstat._
import scala.math.abs
//import scala.collection.mutable.ArrayBuffer


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runOneWayVehicle` function is used to launch the `OneWayVehicleModel` class.
 *  > runMain scalation.simulation.process.example_1.runOneWayVehicle
 */
@main def runOneWayVehicle2L (): Unit = new OneWayVehicle2L ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OneWayVehicletModel` class simulates a one-lane roead.
 *  Caveat: must add 'from' and 'to' components before transport!!
 *  @param name       the name of the simulation modelRR
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 
 */

val wk = 2    // second week of Jan, 2024
val dy = 3    // 3rd day of the week (3rd day of week 2)

class OneWayVehicle2L (name: String = "OneWayVehicle2L", reps: Int = 1, animating: Boolean = false,
                          aniRatio: Double = 50.0, stream: Int = 0)
    extends Model (name, reps, animating , aniRatio)
        with RowTimeLoader
        with FitM:

    //runtime for the simulation: Time slice for rows
    //The logic I am targeting is to; say
    //Given the sensor data for a month (January 2024)
    //I need the exact rows that will be processed and run for the simulation.
    //We will dynamically extract those rows data
    //put them inside a new MatrixD and use them from the TrafficConfig
//    val (mRStart, mREnd) = simRunTime(wk, dy, 6, 10)  //6am - 10am data for the day; Morning rushtime data extraction for the Morning rushHourTime
//    val (eRStart, eREnd) = simRunTime(wk, dy, 4, 9)  //4pm - 9pm data for the day; evening rushtime  data extraction for the evening rushHourTime
//
//    val rushHour = (mRStart to mREnd) ++ (eRStart to eREnd)  // concat the returned list
//    val rushHourVec = VectorI(rushHour)              // make this rushHour a Vector
//
    //this rushHourRow will be used to filter the main simulation data
    //val nt = rushHourVec.dim // row time for this simulation (time intervals for setting time of simulation; the size of this rushHour
    val trafficData = new TrafficConfig("/seven_sensors/402376.csv", rowTime, stream)

    val nt = trafficData.data.dim
    private val debug = debugf("OneWayVehicle2L", false) // debug function
    val arrivalCount = new VectorI(nt)

    //--------------------------------------------------
    // Motion and traffic data creations
    val motion = GippsDynamics                                              // Gipps function for velocity / motion

    //--------------------------------------------------
    // Create Random Variables (RVs)
    val laneRV      = () => trafficData.getLaneRV(curRow )                   //Random Variate for lane scheduling and selection
    val coin        = Bernoulli(0.5)                                        // should be tunable by calibration: Or analyse(estimate from data) OR Calibrate from the model.
    //val iArrivalRV  = () => trafficData.getIArrivalRV(curRow + trafficData.rowOffset)               // get the current iArrival for the current row index

    val muVal = trafficData.mu          // accessor for all the mu (Vector of mu to be accessed individually)
    //val iArrivalRV = Exponential()
    val iArrivalRV = Erlang()
    val nStop       = trafficData.totalArrivalsPerRow.sum.toInt



    //::::::::::Notion of Time to Stop Simulation
    //val nt = 20                                                              // row time for this simulation (time intervals for setting time of simulation

    setTime(nt * rowTime)   //rowTime: 15 * MINUTE                                                 // e.g., 4 intervals of 15 minutes = 1 hour sim
    val numJunc = 5                                                         //Numbers of intermediate junction between the entry and exit
    val (w, h) = (1000, 800)
    private val aniCoords  = trafficData.getJunctions(DATA_DIR + "gps.txt", (w,h))


    //-----------------------Sensors initializations 7 sensors
    //Entry and Exit sensors (for statistical purposes)
    val srcSensor = new Junction(s"sensor1", xy = (aniCoords(0)._1 - 20.0, aniCoords(0)._2 - 20.0), nt=nt)              // this is the coodinate of VSouce, we will need to add a small value to this to create a small shift
    val exitSensor = new Junction(s"sensor7", xy = (aniCoords.last._1 + 20.0, aniCoords.last._2 + 20.0), nt=nt )        // this is the coodinate of Sink, we will need to add a small value to this to create a small shift


    //-----------------------Sensor2 t0 6: fors the main 5 junctions-------------
    val sensors = Array.ofDim[Junction](numJunc)                                       //Numbers of Junctions (Intermediate Sensors (5))
    for i <- sensors.indices do
        sensors(i) = new Junction(s"sensor${i + 2}", xy = (aniCoords(i + 1)._1, aniCoords(i + 1)._2), nt= nt)

    val entry = VSource("entry", this, () => Car(), 0, nStop, iArrivalRV, (aniCoords(0)._1.toInt, aniCoords(0)._2.toInt))
    val exit  = Sink("exit", (aniCoords.last._1.toInt,aniCoords.last._2.toInt)) // Positioned after last sensor

    //Pathway: lane: Each lane is a doublyLinkedList
    val lane = new Pathway("", 4, numJunc, entry, srcSensor, sensors, exitSensor, exit, motion, false)                  //A doublylinkedlist of lane
    val nlane = lane.pathways.length


    addComponents (List(entry),List(srcSensor),sensors.toList, List(exitSensor), List(exit), List(lane))                // Caveat: must add from and to before transport!! //Sensor 7
    


    var carAhead: Vehicle = null                                                                                        //Null reference to CarAhead

    //var l1:Int = -1, var seg:Int = -1
    case class Car() extends Vehicle("c", this):
        override def act(): Unit =
            debug("Act::: ", s"For $this actor in: lane: ${this.laneID}: No laneID Yet")

            Vehicle.setInitialSpeed(68.0/2.24694)                                             // Speed initialization


            //::::::::::::::::::::::::::::::::::::::
            //lane and path information assignment
            var l1 = laneRV().igen % nlane                                             // Randomly select a pathway (0 to (3-1))
            this.laneID = l1                                                           //Assign this laneID to the Vehicle
            this.pathInfo = lane.pathways(l1)(0).name                                  // the pathway of this vehicle. helps to tract the current segment the vehicle is initially.
            debug("Act::: ", s"For $this actor in: lane: ${this.laneID}: LaneID Assigned")

            //:::::::::::::::::::::::::::::::::::::::::::
            //get the carAhead in your segment
            carAhead = lane.getLast(l1)                                                // The car to follow in this pathway (double linked list)
            //carAhead = lane.pathways(l1)(0).getLast                                  // get the last vehicle in this segment(0). that is the car ahead of u.
            println(s"act: Car: $this:  carAhead in pathway_$l1 = $carAhead")                    // Print the car ahead in the selected pathway

            lane.addToAlist(this, carAhead,l1)                                          // Add to the pathway's doubly linked list

            //Step1::::::::::::::No lane change for this part of the movement::::::::::::::
            //lane change logic will be done inside in sensor2----sensor6
            srcSensor.jump()                                                            // jump via the entry sensor to capture stats
            lane.pathways(l1)(0).move()                                                 // Move through the first segment
            //::::::::::::::End of First Segment Movement::::::::::::::::::::::::::::::

            //::::::::Dummy code without lane change:::::::::::::::::::::::::::::::::::
            //Uncomment if needed to see behavior of vehicles without lane change
            //            move via the sensor2---sensor6
            //            for i <- 0 until numJunc  do
            //                sensors(i).jump()
            //                lane.pathways(l1)(i + 1).move()
            //::::::::End Dummy code without lane change:::::::::::::::::::::::::::::::::::

            //Step2:::::::::::Process intermediate sensors (sensor2 to sensor6)
            //Lane change logic starts here::::::::::::::::::::::::::::::::::::::::::::::::
            for seg <- 0 until numJunc  do                                             // Correct: Iterate over number of Junc
                if coin.igen == 1 then                                                 //tose a coin for lane change (uses Bernoulli)
                    var l2 = (l1 + 1) % nlane                                          //Make a second lane number: % to ensure within range
                    if lane.changeLane(l1, l2, this, seg) then                         //Lane change Logic return True or False (DataStructure Manipulation)
                        println(s"Car: ${this.id} changes lane from lane:$l1 to lane:$l2 at ${sensors(seg).name}")
                        l1 = l2                                                         // Update lane reference after successful change
                        this.laneID = l2                                                //Uodate Vehicle new lane
                    else
                        println(s"Car: ${this.id} failed to change lane from lane:$l1 to lane:$l2 at ${sensors(seg).name}, continuing in lane $l1")
                end if
                //:::Regardless of lane change success/failure, move vehicle
                sensors(seg).jump()                                                     // Sensor records passage(jump and record start at this junction
                lane.pathways(l1)(seg + 1).move()                                       // Move through the corresponding transport segment
            end for
            //::::::::::::::::::::End of the movement loop::::::::::::::::::::::::::::::::::
            exitSensor.jump()                                                           // jump via the exit sensor to capture stats

            debug("End of Act::: ", s"For $this actor in: lane: ${this.laneID}")
            lane.removeFromAlist(this, this.laneID)                                                    // Remove from DLL after moving
            exit.leave()                                                                        // Exit at the shared sink
        end act
    end Car



    //sMAPE Value calculation
    //get the Matrix of the simulation
    //get the Matrix of the Pemps Data
    //Get the value difference from this two Matrix
    //then run the sMAPE of the both
    //mode



    def simRunVsPemsRun():Unit =
        val ytrue = trafficData.totalArrivalsPerRow//(0 until nt, ?)   // MatrixD //maybe useful to take the 0 to nt if need be
        val ysim1 = srcSensor.getCountMatrix//(0 until nt, ?)    //MatrixD
        val ysim2 = sensors(0).getCountMatrix //(0 until nt, ?)    //MatrixD
        val ysim3 = sensors(1).getCountMatrix //(0 until nt, ?)    //MatrixD
        val ysim4 = sensors(2).getCountMatrix //(0 until nt, ?)    //MatrixD
        val ysim5 = sensors(3).getCountMatrix //(0 until nt, ?)    //MatrixD
        val ysim6 = sensors(4).getCountMatrix //(0 until nt, ?)    //MatrixD
        val ysim7 = exitSensor.getCountMatrix //(0 until nt, ?)    //MatrixD

        val smapeValue = new VectorD(nt)

//
//        for i <- 0 until nt do
//            val resid = (ytrue(i) - ysim(i)).map(abs)      // absolute diff between ytrue and ysim
//            smapeValeu(i) = smapeF(ytrue(i), ysim(i))          // data fitted into sMape from FitM. (Model mixed with FitM
//            ew.write(s"\n Row $i  === Truth: ${ytrue(i)} | Simulated: ${ysim(i)} | Diff: $resid \n")
//        end for
//
//        ew.write(s"\n sMape Across all intervals: ${smapeValue.mean} with nt@@@@@@@@: $nt\n")   // sMape Value inside the same file
//        ew.write(s"Max sMAPE: ${smapeValue.max()} | Min sMAPE: ${smapeValue.min()}\n")
//        ew.write(s"PemsCount: ${ytrue.sum} | SimulationCount: ${ysim.sum}\n")

        println(s"The Smape EntrySensor: ${smapeF(ytrue, ysim1.sumVr)}\n")
        println(s"The Smape Sensor2:     ${smapeF(ytrue, ysim2.sumVr)}\n")
        println(s"The Smape Sensor3:     ${smapeF(ytrue, ysim3.sumVr)}\n")
        println(s"The Smape Sensor4:     ${smapeF(ytrue, ysim4.sumVr)}\n")
        println(s"The Smape Senson5:     ${smapeF(ytrue, ysim5.sumVr)}\n")
        println(s"The Smape Sensor6:     ${smapeF(ytrue, ysim6.sumVr)}\n")
        println(s"The Smape ExitSensor:  ${smapeF(ytrue, ysim7.sumVr)}\n")


        //new Plot(null, ytrue, ysim.sumVr, "Simulation vs Actual Plot")

        ew.flush()
    end simRunVsPemsRun


    //simulation; running //


/**
     * @param rep  the replication number (1, ... reps)
     * We are overriding the fini method of the Main Model so that we can capture Sensor Statistics
     * before the Model/Director Initiate cleanup of the Simulation Data
     * The Recorder will capture all it's statistics and write it into the fill defines
     * in the recorder trait
     */
    override def fini(rep: Int):Unit =
        Recorder.writeAllSensorStats(List(srcSensor) ++ sensors.toList ++ List(exitSensor))   // Use the write method in Recorder to capture all stats before fini is called
        simRunVsPemsRun()                                                                     // get the count difference between the lanes and dump it
        super.fini(rep)                                                                       //call fini and clean up the simulation at this point.
    end fini


    simulate ()
    waitFinished ()
    Model.shutdown ()
end OneWayVehicle2L


























//def statMatrices(): (Array[MatrixD], Array[MatrixD]) =
//
//    val counts = Array.ofDim[MatrixD](7)
//    val speeds = Array.ofDim[MatrixD](7)
//
//    counts(0) = srcSensor.getCountMatrix
//    speeds(0) = srcSensor.getSpeedMatrix
//
//    //jun2--6
//    for i <- sensors.indices do
//        counts(i + 1) = sensors(i).getCountMatrix
//        speeds(i + 1) = sensors(i).getSpeedMatrix
//
//    counts(6) = exitSensor.getCountMatrix
//    speeds(6) = exitSensor.getSpeedMatrix
//
//    (counts, speeds)
//end statMatrices
//
//val (countMats, speedMats) = statMatrices()
//











































//import scalation.random.{Bernoulli, Sharp, Variate, Exponential2}
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** The `runOneWayVehicle` function is used to launch the `OneWayVehicleModel` class.
// *  > runMain scalation.simulation.process.example_1.runOneWayVehicle
// */
//@main def runOneWayVehicle2L (): Unit = new OneWayVehicle2L ()
//
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** The `OneWayVehicletModel` class simulates a one-lane roead.
// *  Caveat: must add 'from' and 'to' components before transport!!
// *  @param name       the name of the simulation modelRR
// *  @param reps       the number of independent replications to run
// *  @param animating  whether to animate the model
// *  @param aniRatio   the ratio of simulation speed vs. animation speed
// *  @param nStop      the number arrivals before stopping
// *  @param stream     the base random number stream (0 to 999)
// */
//class OneWayVehicle2L (name: String = "OneWayVehicle2L", reps: Int = 1, animating: Boolean = false,
//                          aniRatio: Double = 500.0, stream: Int = 0)
//    extends Model (name, reps, animating , aniRatio):
//
//
//    //--------------------------------------------------
//    // Create Random Variables (RVs)
//    val motion      = GippsDynamics                                                             // Gipps function for velocity / motion
//    val rowTime     = 15 * MINUTE
//    val trafficData = new TrafficConfig("/seven_sensors/402376.csv", rowTime, stream)
//
//    private val aniCoords  = TrafficConfig.getJunctions (DATA_DIR + "gps.txt", 840.0, 540.0) //500,500 what does that means?
//
//    var curRow      = 0
//    val laneRV      = () => trafficData.getLaneRV(curRow)
//    val coin        = Bernoulli(0.5)    // should be tunable by calibration: Or analyse(estimate from data) OR Calibrate from the model.
//    val iArrivalRV  = () => trafficData.getIArrivalRV(curRow) // get the current iArrival for the current row index
//    val nStop       = trafficData.totalArrivalsPerRow.sum
//    val jTime : Variate = Sharp (1.0)
//    //maybe the exponential variate should
//    //--------------------------------------------------
//    //Create Model Components
//    //call the coordinate class to get the screen coordinates
//    //adjust the junctions to multiple junctions:
//    //real coordinate to screen coodinates.
//    //
//    val entry = VSource ("entry", this, () => Car (), 0, nStop, iArrivalRV(), (100, 290))          // the entry point of the road // Need to change the coodinates based on my datasets// Sensor1
//
//
//    val sensors = Array.ofDim[Junction](5)
//    for i <- sensors.indices do
//        sensors(i) = new Junction(s"sensor${i + 2}", xy = (aniCoords(i + 1)._1.toInt, aniCoords(i + 1)._2.toInt))
//    // Ensure entry and exit match sensor alignment
//    val entry = VSource("entry", this, () => Car(), 0, nStop, iArrivalRV, (aniCoords(0)._1.toInt, aniCoords(0)._2.toInt))
//    val exit = Sink("exit", (aniCoords.last._1.toInt, aniCoords.last._2.toInt)) // Positioned after last sensor
//
//    // Ensure the pathway aligns with the straight-line sensors
//    val lane = new Pathway("", 4, numSeg, entry, sensors, exit, motion, false)
//
//    addComponents(List(entry), sensors.toList, List(exit), List(lane)) // Caveat: must add from and to before transport!! //Sensor 7
////
////    // Connector of two paths      // May have Sensor (2-6)
////    //Sensor7
////    val exit  = Sink ("exit", (800, 290))                                                        // The sink/exits for all cars
////    val lane  = new Pathway("pway",4, entry, junc, exit, motion, false)                       // The pathway for the cars
//    addComponents (List(entry),junc, List(exit), List(lane))                                                     // Caveat: must add from and to before transport!! //Sensor 7
//
//    val plane = lane.pathways.length
//
//    //val laneRV = Uniform(0, plane, stream + plane)
//
//    var carAhead: Vehicle = null
//
//    case class Car() extends Vehicle("c", this):
//        override def act(): Unit =
//            println(s"act: Car $this BEGINS")
//            println(s" the plane is $plane ")
//            val i = laneRV().igen //% plane      // Randomly select a pathway (0 to (3-1))
//            this.laneID = i
//
//            val attemptLC = coin.igen
//
//            println(s"act: Car: $this:  laneID = $i")                                             // Print the lane selected
//            carAhead = lane.getLast(i)                                                      // The car to follow in this pathway (double linked list)
//            println(s"act: Car: $this:  carAhead in pathway_$i = $carAhead")                    // Print the car ahead in the selected pathway
//
//            lane.addToAlist(this, carAhead,i)                                                // Add to the pathway's doubly linked list
//
//            lane.pathways(i)(0).move() // Always move first segment (source ---> junction)
//            if attemptLC == 1 then
//                val j = (i + 1) % plane
//                if lane.changeLane(i, j, this) then // if this is true, then manipulate the data structure
//                    junc.jump() // jump the token via animation
//                    println(s"Car: ${this.id} will change lane from $i to $j")
//                    lane.pathways(j)(1).move()          //move the token in the new direction
//                    this.laneID = j                     //update current lane index
//                else
//                    lane.pathways(i)(1).move()          // Failed attempted lane changes (gap too small or multiple lane change attempts)
//            else
//                lane.pathways(i)(1).move()              // Move second segment (junction to sink)
//
//            println(s"$this has an id ${this.laneID}")
//            lane.removeFromAlist(this, this.laneID)                                                    // Remove from DLL after moving
//            exit.leave()                                                                        // Exit at the shared sink
//            println(s"act: car= $this end of act in pathway_$i")
////
////            if clock % rowTime == 0 && curRow < trafficData.data.dim-1
////                then curRow +=1
////                println(s"updated currRow: $curRow at $clock")
//        end act
//    end Car
//
//    simulate ()
//    waitFinished ()
//    Model.shutdown ()
//
//end OneWayVehicle2L









//The latest Model without live data feed:
//import scalation.random.{Bernoulli, Exponential,Uniform}//, Exponential2}
//import scalation.simulation.process.modeling.clustering.Coordinates
////import scalation.mathstat._
////import scala.collection.mutable.ArrayBuffer
//
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** The `runOneWayVehicle` function is used to launch the `OneWayVehicleModel` class.
// *  > runMain scalation.simulation.process.example_1.runOneWayVehicle
// */
//@main def runOneWayVehicle2L (): Unit = new OneWayVehicle2L ()
//
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** The `OneWayVehicletModel` class simulates a one-lane roead.
// *  Caveat: must add 'from' and 'to' components before transport!!
// *  @param name       the name of the simulation modelRR
// *  @param reps       the number of independent replications to run
// *  @param animating  whether to animate the model
// *  @param aniRatio   the ratio of simulation speed vs. animation speed
// *  @param nStop      the number arrivals before stopping
// *  @param stream     the base random number stream (0 to 999)
// */
//class OneWayVehicle2L (name: String = "OneWayVehicle2L", reps: Int = 1, animating: Boolean = true,
//                       aniRatio: Double = 50.0, nStop: Int = 1000, stream: Int = 0)
//    extends Model (name, reps, animating , aniRatio):
//
//    //Why did the cars not stop at the 3600Secs?  because the notion of source is in force
//    //          even though we asked that time should terminate, we did not ask the Model to stop simulation using time as the notion
//    //Dominic is going to increase the size of the clock and numbers of actors
//    //          this is done already.
//    //Debug the easywriter:
//    //Eliminate the short distance problem caused during the lane change problem
//    //Check the 4 rows of the matrix and ensure that the counts matches that of the Pems data
//    //We have to get the statistics and count and validate that it meets whats in the pemps data
//    //then we will
//
//
//
//    val nt = 4
//    setTime(nt * 15 * MINUTE) // e.g., 4 intervals of 15 minutes = 1 hour sim
//
//    //--------------------------------------------------
//    // Create Random Variables (RVs)
//    val motion      = GippsDynamics                                                             // Gipps function for velocity / motion
//
//    val (w,h) = (1000, 800)
//    val numJunc = 5
//    val lambda = 10.0
//    val coin        = Bernoulli(0.5)
//    val iArrivalRV = Exponential(MINUTE/lambda , stream) // rate of arrivals of cars into the simulation // exponential function
//
//    /**
//     * This method uses the Coodinate class to calculate the scales real data (Pims) to
//     * Animation coodinate for the simulation
//     * @param aw   the width of the animation window
//     * @param ah  the height of the animation window
//     * @param path     an array of lat-long coordinates from the file
//     */
//    def getJunctions(path: String, w_h:(Double,Double)): Array[(Double, Double)] =
//
//        val data = scala.io.Source.fromFile(path).getLines.toArray
//        val gps = Array.ofDim[(Double, Double)](data.length)
//
//        for i <- data.indices do
//            val ll      = data(i).split(",")
//            val lat     = ll(0).toDouble
//            val long    = ll(1).toDouble
//            gps(i) = (lat, long)
//        end for
//        val coords = new Coordinates(w_h._1,w_h._2, gps)
//        coords.calcAniCoords()
//
//        for (lat, long) <- gps do println(s"the gps $lat and $long")
//        for (x, y) <- coords.aniCoords do println(s"the scaled coordinate is x: $x : y $y")
//
//        coords.aniCoords
//    end getJunctions
//
//
//    private val aniCoords  = getJunctions (DATA_DIR + "gps.txt", (w,h)) //500,500 what does that means?
//
//
//    val sensors = Array.ofDim[Junction](5)
//    for i <- sensors.indices do
//        sensors(i) = new Junction(s"sensor${i + 2}", xy = (aniCoords(i + 1)._1, aniCoords(i + 1)._2), nt= nt)
//
//
//
//    val srcSensor = new Junction(s"sensor1", xy = (aniCoords(0)._1 - 20.0, aniCoords(0)._2 - 20.0), nt=nt)  // this is the coodinate of VSouce, we will need to add a small value to this to create a small shift
//    val exitSensor = new Junction(s"sensor7", xy = (aniCoords.last._1 + 20.0, aniCoords.last._2 + 20.0), nt=nt )  // this is the coodinate of Sink, we will need to add a small value to this to create a small shift
//
//    val entry = VSource("entry", this, () => Car(), 0, nStop, iArrivalRV, (aniCoords(0)._1.toInt, aniCoords(0)._2.toInt))
//    val exit  = Sink("exit", (aniCoords.last._1.toInt,aniCoords.last._2.toInt)) // Positioned after last sensor
//
//    // Ensure the pathway aligns with the straight-line sensors
//    //need to add the entry and exit sensor to the lane so we will modify it as such
//    val lane = new Pathway("", 4, numJunc, entry, srcSensor, sensors, exitSensor, exit, motion, false)
//    //val lane = new Pathway("", 4, numJunc, entry, sensors, exit, motion, false)
//
//    addComponents (List(entry),List(srcSensor),sensors.toList, List(exitSensor), List(exit), List(lane))                                                     // Caveat: must add from and to before transport!! //Sensor 7
//
//
//    val nlane = lane.pathways.length
//    val laneRV = Uniform(0, nlane, stream + nlane)
//
//
//
//
//    var carAhead: Vehicle = null
//
//    //var l1:Int = -1, var seg:Int = -1
//    case class Car() extends Vehicle("c", this):
//        override def act(): Unit =
//            println(s"act: Car $this BEGINS")
//            println(s" the no_lane is $nlane ")
//
//            //::::::::::::::::::::::::::::::::::::::
//            //lane and path information assignment
//            var l1 = laneRV.igen % nlane      // Randomly select a pathway (0 to (3-1))
//            this.laneID = l1
//            this.pathInfo = lane.pathways(l1)(0).name     // the pathway of this vehicle. helps to tract the current segment the vehicle is initially.
//            println(s"act: Car: $this:  laneID = $l1")                                             // Print the lane selected
//
//
//            //:::::::::::::::::::::::::::::::::::::::::::
//            //get the carAhead in your segmeb
//            carAhead = lane.getLast(l1)                                                     // The car to follow in this pathway (double linked list)
//            //carAhead = lane.pathways(l1)(0).getLast          // get the last vehicle in this segment(0). that is the car ahead of u.
//            println(s"act: Car: $this:  carAhead in pathway_$l1 = $carAhead")                    // Print the car ahead in the selected pathway
//
//            lane.addToAlist(this, carAhead,l1)                                                // Add to the pathway's doubly linked list
//
//
//            //No lane change for this part of the movement
//            //lane change logic will be done inside in sensor2----sensor6
//            srcSensor.jump()                            // jump via the entry sensor to capture stats
//            lane.pathways(l1)(0).move()                 // Move through the first segment
////
////            move via the sensor2---sensor6
////            for i <- 0 until numJunc  do
////                sensors(i).jump()
////                lane.pathways(l1)(i + 1).move()
//
//            // Step 2: Process intermediate sensors (sensor2 to sensor6)
//            for seg <- 0 until numJunc  do               // Correct: Iterate over transport segments, not just sensors
//                if coin.igen == 1 then
//                    var l2 = (l1 + 1) % nlane
//                    if lane.changeLane(l1, l2, this, seg) then
//                        println(s"Car: ${this.id} changes lane from lane:$l1 to lane:$l2 at ${sensors(seg).name}")
//                        l1 = l2 // Update lane reference after successful change
//                        this.laneID = l2
//                    else
//                        println(s"Car: ${this.id} failed to change lane from lane:$l1 to lane:$l2 at ${sensors(seg).name}, continuing in lane $l1")
//                end if
//                // Regardless of lane change success/failure, move vehicle
//                sensors(seg).jump() // Sensor records passage
//                lane.pathways(l1)(seg + 1).move() // Move through the corresponding transport segment
//            end for
//            exitSensor.jump()                          // jump via the exit sensor to capture stats
//
//
//            println(s"$this has an id ${this.laneID}")
//            lane.removeFromAlist(this, this.laneID)                                                    // Remove from DLL after moving
//            exit.leave()                                                                        // Exit at the shared sink
//            //println(s"act: car= $this end of act in pathway_$i")
//        end act
//    end Car
//
//
//    override def fini(rep: Int):Unit =
//        Recorder.writeAllSensorStats(List(srcSensor) ++ sensors.toList ++ List(exitSensor))
//        super.fini(rep)
//    end fini
//
//
//    simulate ()
//    waitFinished ()
//    Model.shutdown ()
//end OneWayVehicle2L





































//
//    //    for i <- countMats.indices do
//    //        println(s"Sensor $i counts:")
//    //        countMats(i)
//    //        println(s"Sensor $i speeds:")
//    //        speedMats(i)
//    //    end for
//    for i <- countMats.indices do
//        println(s"Sensor $i lane counts:")
//        val laneTotals = (0 until countMats(i).dim2).map { lane =>
//            s"lane $lane: ${countMats(i).col(lane).sum}"
//        }
//        banner(laneTotals.mkString(", "))
//    end for

/**
 * Source ---------junc1------------junc2------C7junc3--C5------junc4-----C3junc5------C1----Sink: Pway1:
 * Source ---------junc1------------junc2-------junc3--C6------junc4---C4--junc5---C2-------Sink: Pway2
 * Source ---------junc1------------junc2---C8--junc3----------junc4-------junc5------------Sink: Pway3
 * Source ---------junc1------------junc2--C9---junc3----------junc4-------junc5------------Sink: Pway4
 *
 //            //move via the sensor2---sensor6
 //            for i <- 0 until numJunc  do
 //                sensors(i).jump()
 //                lane.pathways(l1)(i + 1).move()
 *
 * C3 ---change pway(i)(junc(j)   to: pway(i+1)(transportj+1)------dll: pway(i)---pway(i+1)
 * C7
 * //                        lane.pathways(l2)(seg).move()          //move the token in the new direction
 * //                        sensors(seg).jump()                     // jump the vehicle in this new lane
 * //                        seg += 1
 * //                    else
 * //
 * //                        lane.pathways(l1)(seg).move()          // Failed attempted lane changes (gap too small or multiple lane change attempts)
 * //                        sensors(seg).jump()                    // move in the same direction no lane change
 * //                        seg +=1                                // increment counter, no lane change failes situation
 * //else
 * var seg = 1
 * while seg < numJunc do
 * if coin.igen == 1 then
 * var l2 = (l1 + 1) % nlane
 * if lane.changeLane(l1, l2, this, seg) then // if this is true, then manipulate the data structure
 * println(s"Car: ${this.id} will change lane from $l1 to $l2")
 * l1 = l2
 * this.laneID = l2      // lane change true; assign the car the new lane id
 * end if
 * end if
 * sensors(seg).jump()
 * lane.pathways(l1)(seg).move()              // No lane change at all coin.igen != 1 in this condition
 * //
 * seg +=1                                      // increment segment for the next segment movement
 * end while
 *
 * Tasks:
 * the sensors needs to be aware of the cars that passed through them.
 * the sensors needs to hold a count of the vehicles that passed through them and the lane those cars belongs to
 * Optionally we also need to get the average speed of these vehicles: this is secondary for now
 * Since source and Sink are also sensors: We need to increase the Array of sensors to 7 from 5
 * Now, the 1st and last sensor will be position very close to the source and sink respectively,
 *      we will ensure that we do not have a transport from Source to sensor 1 since they are logically the
 *      same, same goes for Sink and last sensor. the idea is that we need to ensure that all vehicles are counted
 *      passing through our sensors. Our Source and Sink are natural course for us since that's how we produce and
 *      absorb vehicles even though they are sensors logically themselves.
 *
 *      to make the implementation simple and not overtly complex, we might use the jump method inside the Junction class
 *      as though: sensor(i).jump. cars enters a sensor; jump to the next Vtransport
 *      Inside the jump method; we take account of our counts of cars
 *      We keep a Matrix inside the jump method that records these occurence and give us a tally at the end
 *      of simulation. Matrix(i)[car_id, count]: So we can get sensor(i) counts of total cars that flows through it.
 *      ---> We do have partial implementation here and there for this situation but we need to develop it using matrix.
 *
 *      May be useful >>>
 * //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * /** Method to save the counts data from the simulation
 * */
 * def saveCounts(): (VectorD, MatrixD, VectorD) =
 * val jund = new MatrixD(nt, juncs.length)
 * for j <- jund.indices2 do jund(?, j) = countsToVector(juncs(j).getCounts)
 * (countsToVector(entry.getCounts), jund, countsToVector(exit.getCounts)) //was 3 matrices, the other two like jund
 * end saveCounts
 *
 * //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 *
 * /** Utility method to convert data arrays of Ints to VectorDs
 * *
 * * @param d the array to convert to a VectorD
 * */
 * def countsToVector(d: VectorD): VectorD =
 *
 * val v = new VectorD(nt)
 * for i <- 0 until (t2 - t1) do v(i + t1) = d(i)
 * v
 * end countsToVector
 */


//each sensor will keep an array of counts
//and the array of counts will be indexed
//A sensor will have an array of count: A count for each lane
//then a Car should be able to tell which the Sensor which lane it's on and use it
//if a car is changing lane, then we have to either count it for where it's coming from
//or where it's going to.

//----------------Sensor1--------------Sensor4
//pway1----------cLane pway2 ----------pway3------Sink

//5 junction sensors representing sensor2 to sensor 5
//Sensor 1 and sensor 7 are logical Vsource and Sink respectively
//the entry and exit sensors that will be very close to the entry and exit. needed for the purpose of counting and speed validaton and stats
//also no lane change will be effective at the sensors/ junctions which is also the reason we need them
//1.  make a sensor near the Source and Sink:
// sensor can also know it's 4 lanes that i am connected to.
//this is to ensure that we can have a valid count of vehicles that flows through this sensor
//Also the speed of these vehicles.
//
// Ensure entry and exit match sensor alignment
//Source: Sensor  and Sink Sensor
//----Sourse--No transport---sensor1------transport------Sensor2-----------Sensor6------Sensor7No transport: Sink


/**
 * I need to change the laneRV to Variate
 * Upload my traffic data to this model
 * use exponential2 to get flowRT
 * feed the Variete class into the Variate(dataSet)
 * Use Variate.igen to get the laneRV for Source
 * that should be a combined flow rate for lamda generation
 * laneRV will then gen with a probability close to vehicle flow rate
 * in the original dataset.
 *
 * Harvest the new flows: and times stamps
 * get the MSE of them.
 */
//
//    def writeCSV(m: MatrixD, f: String, time: Boolean = true): Unit =
//        val w = new java.io.PrintWriter(f)
//        val nl = m.dim2
//
//        // Header
//        val h = if time then "t," + (0 until nl).map("l" + _).mkString(",")
//        else (0 until nl).map("l" + _).mkString(",")
//        w.println(h)
//
//        // Loop over rows using .indices1 (clean scalation way)
//        for i <- m.indices do
//            val row = m(i)
//            if row.exists(_ != 0.0) then
//                val line = if time then s"$i,${row.mkString(",")}" else row.mkString(",")
//                w.println(line)
//
//        w.close()
//    end writeCSV


//    def stat(): Unit =
//        println("==== Saving Stats ====")
//
//        writeCSV(srcSensor.getCountMatrix, "entry_counts.csv")
//        writeCSV(srcSensor.getSpeedMatrix, "entry_speeds.csv")
//
//        for i <- sensors.indices do
//            writeCSV(sensors(i).getCountMatrix, s"s${i + 2}_counts.csv")
//            writeCSV(sensors(i).getSpeedMatrix, s"s${i + 2}_speeds.csv")
//
//        writeCSV(exitSensor.getCountMatrix, "exit_counts.csv")
//        writeCSV(exitSensor.getSpeedMatrix, "exit_speeds.csv")
//
//        println("==== Done ====")
//    end stat