
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

import scalation.random.Exponential

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runOneWayVehicle` function is used to launch the `OneWayVehicleModel` class.
 *  > runMain scalation.simulation.process.example_1.runOneWayVehicle
 */
@main def runOneWayVehicle (): Unit = new OneWayVehicleModel ()


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
class OneWayVehicleModel (name: String = "OneWayVehicle", reps: Int = 1, animating: Boolean = true,
                          aniRatio: Double = 800.0, nStop: Int = 20, stream: Int = 0)
    extends Model (name, reps, animating , aniRatio):

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 5.0                                     // car arrival rate (per hour)


    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (HOUR / lambda, stream)
    val motion     = GippsDynamics

    //--------------------------------------------------
    // Create Model Components

    val entry = VSource ("entry", this, () => Car (), 0, nStop, iArrivalRV, (100, 290))
    val exit  = Sink ("exit", (800, 290))
    val lane  = VTransport ("lane", entry, exit, motion, false)


    addComponent (entry, exit, lane)                      // Caveat: must add from and to before transport!!

    var car_ahead : Vehicle = null
    case class Car() extends Vehicle("c", this):
        override def act(): Unit =
            println(s"act: $this BEGINS")
            println(s"for car = $this the @@car_ahead = $car_ahead")
            SimActor.addToAlist(this ,car_ahead)
            car_ahead = this

            println(s"Vehicle count = $this")
            lane.move() // move down the street/lane to the exit

            SimActor.removeFromAlist(this)

            exit.leave() // exit the street/lane

        end act
    end Car
    simulate ()
    waitFinished ()
    Model.shutdown ()

end OneWayVehicleModel











//    case class Car() extends Vehicle("c", this):
//        override def act(): Unit =
//            println(s"act: $this BEGINS")
//            Vehicle.addToVlist(this)
//            //val car_ahead = Vehicle.vlist.getPred(this).asInstanceOf[Vehicle]
//            lane.move() // move down the street/lane to the exit
//            //Vehicle.removeFromAlist (this)               // remove this car from alist
//            exit.leave() // exit the street/lane
//        end act




//val carList = DoublyLinkedList[Car]()
//
//def sharedList(car: Car): Unit = carList.add(car)
//
//case class Car() extends Vehicle("c", this):
//
//
//    override def act(): Unit =
//        println(s"act: $this BEGINS")
//        val curCar = sharedList(this) // the Node that holds all cars
//        println(s"carList: $carList")
//        //println(s"@@@@ carCurr = $carCurr")
//        lane.move() // move down the street/lane to the exit
//        //myNode = carCurr
//        //val carAhead = carCurr.prev.elem
//        //            println(s"act: $this ENDS")
//        //            //SimActor.removeFromAlist (this)               // remove this car from alist
//        exit.leave() // exit the street/lane
//    end act