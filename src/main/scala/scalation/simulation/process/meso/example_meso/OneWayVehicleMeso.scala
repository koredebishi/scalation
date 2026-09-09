
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Model: One Mesoscopic Link with Vehicles
 */

package scalation
package simulation
package process
package meso
package example_meso                                    // One-Shot

import scalation.random.Exponential

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runOneWayVehicleMeso` function is used to launch the `OneWayVehicleMeso` class.
 *  > runMain scalation.simulation.process.meso.example_meso.runOneWayVehicleMeso
 */
@main def runOneWayVehicleMeso (): Unit = new OneWayVehicleMeso ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OneWayVehicleMeso` class simulates one mesoscopic road link.
 *  Caveat: must add 'from' and 'to' components before transport.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number of arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 */
class OneWayVehicleMeso (name: String = "OneWayVehicleMeso", reps: Int = 1, animating: Boolean = false,
                        aniRatio: Double = 8.0, nStop: Int = 10, stream: Int = 0)
    extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda        = 20.0                            // car arrival rate (per hour)
    val freeFlowSpeed = 10.0                            // free-flow speed (metres/second)
    val capacity      = 1800.0                          // discharge capacity (vehicles/hour)

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (HOUR / lambda, stream)

    //--------------------------------------------------
    // Create Model Components

    val entry = VSource ("entry", this, () => Car (), 0, nStop, iArrivalRV, (100, 290))
    val exit  = Sink ("exit", (600, 290))
    val lane  = MesoVTransport ("lane", entry, exit, freeFlowSpeed, capacity, 0.0)

    addComponent (entry, exit, lane)                     // add endpoints before transport

    //--------------------------------------------------
    // Specify Scripts for each Type of Simulation Actor

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            lane.move ()                               // move along the link
            exit.leave ()                              // leave the model
        end act

    end Car

    simulate ()
    waitFinished ()
    Model.shutdown ()

end OneWayVehicleMeso
