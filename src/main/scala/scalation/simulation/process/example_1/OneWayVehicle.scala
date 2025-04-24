//
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** @author  John Miller
// *  @version 2.0
// *  @date    Sun Sep 26 15:00:24 EDT 2021
// *  @see     LICENSE (MIT style license file).
// *
// *  @note    Example Model: One-Way-Street (with Vehicle) for Process-Interaction Simulation
// */
//
//package scalation
//package simulation
//package process
//package example_1                                       // One-Shot
//
//import scalation.random.Exponential
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** The `runOneWayVehicle` function is used to launch the `OneWayVehicleModel` class.
// *  > runMain scalation.simulation.process.example_1.runOneWayVehicle
// */
//@main def runOneWayVehicle (): Unit = new OneWayVehicleModel ()
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
//class OneWayVehicleModel (name: String = "OneWayVehicle", reps: Int = 1, animating: Boolean = true,
//                          aniRatio: Double = 800.0, nStop: Int = 4, stream: Int = 0)
//    extends Model (name, reps, animating , aniRatio):
//
//    //--------------------------------------------------
//    // Initialize Model Constants
//
//    val lambda = 5.0                                     // car arrival rate (per hour)
//
//
//    //--------------------------------------------------
//    // Create Random Variables (RVs)
//
//    val iArrivalRV = Exponential (HOUR / lambda, stream)
//    val motion     = GippsDynamics
//
//    //--------------------------------------------------
//    // Create Model Components
//
//    val entry = VSource ("entry", this, () => Car (), 0, nStop, iArrivalRV, (100, 290))
//    val exit  = Sink ("exit", (800, 290))
//    val lane  = VTransport ("lane", entry, exit, motion, false)
//
//
//    addComponent (entry, exit, lane)                      // Caveat: must add from and to before transport!!
//
//    var car_ahead : Vehicle = null
//    case class Car() extends Vehicle("c", this):
//        override def act(): Unit =
//            println(s"act: $this BEGINS")
//            println(s"for car = $this the @@car_ahead = $car_ahead")
//            SimActor.addToAlist(this ,car_ahead)
//            car_ahead = this
//
//            println(s"Vehicle count = $this")
//            lane.move() // move down the street/lane to the exit
//
//            SimActor.removeFromAlist(this)
//
//            exit.leave() // exit the street/lane
//            println(s"Vehicle: me = $this terminate Car ")
//        end act
//    end Car
//    simulate ()
//    waitFinished ()
//    Model.shutdown ()
//
//end OneWayVehicleModel
//
//

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

import scalation.random.{Exponential}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runOneWayVehicle` function is used to launch the `OneWayVehicleModel` class.
 *  > runMain scalation.simulation.process.example_1.runOneWayVehicle
 */
@main def runOneWayVehicle (): Unit = new OneWayVehicleModel ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OneWayVehicletModel` class simulates a one-lane roead.
 *  Caveat: must add 'from' and 'to' components before transport!!
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 */
class OneWayVehicleModel (name: String = "OneWayVehicle", reps: Int = 1, animating: Boolean = true,
                          aniRatio: Double = 500.0, nStop: Int = 10, stream: Int = 0)
    extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 10.0                                     // car arrival rate (per hour)
    val mvTime = (2900.0, 3100.0)                         // (lower, upper) on move time

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (MINUTE / lambda, stream)
    val motion     = GippsDynamics

    //--------------------------------------------------
    // Create Model Components

    val entry = VSource ("entry", this, () => Car (), 0, nStop, iArrivalRV, (100, 290))
    val exit  = Sink ("exit", (600, 290))
    val lane  = VTransport ("lane", entry, exit, motion, false, 0.25)

    addComponent (entry, exit, lane)                      // Caveat: must add from and to before transport!!

    //--------------------------------------------------
    // Specify Scripts for each Type of Simulation Actor

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            println (s"act: Car $this BEGINS")
            //          val carAhead = lane.getFirst                  // find the car-ahead in lane i (the one to follow)
            println (s"act: call getLsst on lane = $lane")
            val carAhead = lane.getLast                   // find the car-ahead in lane i (the one to follow)
            println (s"act: carAhead = $carAhead")
            SimActor.addToAlist (this, carAhead)          // add this car after the car-ahead in alist
            lane.move ()                                  // move down the street/lane to the exit
            println (s"act: $this ENDS")
            SimActor.removeFromAlist (this)               // remove this car from alist
            exit.leave ()                                 // exit the street/lane
        end act
    end Car

    simulate ()
    waitFinished ()
    Model.shutdown ()

end OneWayVehicleModel

//
//val motion = GippsDynamics
//val iArrivalRV = Exponential(MINUTE / 10, 0) // rate of arrivals of cars into the simulation // exponential function
//
//
//// Define Road Components
//val entry = VSource("entry", this, () => Car(), 0, nStop, iArrivalRV, xy = (100, 200))
//val exit = Sink("exit", (700, 200))
//
//
//val entryJunc = new Junction("entryJunc", Sharp(0.0), xy = (150.0, 290.0))
//val junction = new Junction("junction", Sharp(0.0), xy = (250.0, 290.0))
//val exitJunc = new Junction("exitJunc", Sharp(0.0), xy = (350.0, 290.0))
//
//val road1 = new VTransport("road1", entry, junction, motion, false)
//val road2 = new VTransport("road2", junction, exit, motion, false)
//
//
//// Add components to simulation
//addComponent(entry, exit, entryJunc, exitJunc, junction, road1, road2)
//
//// Define Vehicle Behavior
//case class Car() extends Vehicle("c", this):
//
//    override def act(): Unit =
//        println(s"Car $this activated! Moving through simulation.")
//
//        val vehicleAhead = road1.getLast
//
//        SimActor.addToAlist(this, vehicleAhead)
//
//        entryJunc.jump(true) // this should only capture entry stat //maybe same or just jump
//        road1.move() // Move to junction
//        junction.jump() // Track statistics at junction at first transport entryJunc-----junction
//        road2.move() // Move to exit move junction -----exitJunc(stat will be captured at exitJunc.jump()
//        exitJunc.jump(true) // this should only capture exit stat. May not use a jump. maybe entryJunc.jump(stat)//overloaded jump to only capture stat might be fine
//
//        SimActor.removeFromAlist(this)
//        exit.leave() // Vehicle exits
//    end act
//
//end Car
//
//// Run the simulation
//simulate()
//waitFinished()
//Model.shutdown()

