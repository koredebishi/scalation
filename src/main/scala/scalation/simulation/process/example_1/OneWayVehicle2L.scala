
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

import scalation.random.{Exponential,Bernoulli}

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
class OneWayVehicle2L (name: String = "OneWayVehicle2L", reps: Int = 1, animating: Boolean = true,
                          aniRatio: Double = 500.0, nStop: Int = 50, stream: Int = 0)
    extends Model (name, reps, animating , aniRatio):

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 5.0                                     // car arrival rate (per hour)

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (HOUR / lambda, stream)
    val motion     = GippsDynamics

    val laneRV = Bernoulli(stream = stream + 4)
    //val jTime = Uniform (5, 10)
    //--------------------------------------------------
    // Create Model Components

    val entry = VSource ("entry", this, () => Car (), 0, nStop, iArrivalRV, (100, 290))
    val checkpt = new  Checkpoint("checkpt", xy = (450, 290) )

    val exit  = Sink ("exit", (800, 290))

    val route1  = Route ("lane",2, entry, checkpt, motion, false)
    val route2  = Route ("lane",2, checkpt, exit, motion, false)


    addComponent (entry,checkpt, exit, route1, route2)                      // Caveat: must add from and to before transport!!

    var car_ahead: Vehicle = null
    case class Car() extends Vehicle("c", this):
        override def act(): Unit =
            println(s"for car = $this the car_ahead = $car_ahead")
            SimActor.addToAlist(this, car_ahead) // global doublylinkedlist
            car_ahead = this

            val i = laneRV.igen                                                             // Randomly select lane i

                                                                   // Move halfway down lane i
            //println(s"Vehicle: me = $this, car_ahead = $car_ahead, lane = $i")


            if this.id == 12 then
                println("@@@@@@ the if part @@@@")

                route1.lane(i).move()  // moving twice inside the move method
                val j = (i + 1) % 2
                if checkpt.jump(i, j, route1.lane) then
                route2.lane(j).move()  // lane change done, move in lane j
                else route1.lane(i).move() // lane change failed move in same lane i
            else
                println("@@@@@@ the else part @@@@")
                route1.lane(i).move()
                checkpt.jump(i, i, route1.lane)
                route2.lane(i).move()
             // Other vehicles just continue in the same lane

            SimActor.removeFromAlist(this)
            exit.leave()                                                                    // Exit the street/lane

            println(s"Vehicle: me = $this terminate Car ")
        end act

    end Car
    simulate ()
    waitFinished ()
    Model.shutdown ()

end OneWayVehicle2L







