
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Deterministic Finite-Storage Admission and Wakeup Correctness Test
 */

package scalation
package simulation
package process
package meso
package example_meso                                    // One-Shot

import scalation.random.Sharp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runOneWayVehicleMeso` function launches the deterministic finite-storage test.
 *  > runMain scalation.simulation.process.meso.example_meso.runOneWayVehicleMeso
 */
@main def runOneWayVehicleMeso (): Unit = new OneWayVehicleMeso ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OneWayVehicleMeso` class checks finite admission, FIFO blocking, and
 *  event-driven wakeup on one mesoscopic road link.
 *  Caveat: must add 'from' and 'to' components before transport.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number of arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 */
class OneWayVehicleMeso (name: String = "OneWayVehicleMeso", reps: Int = 1, animating: Boolean = true,
                        aniRatio: Double = 500.0, nStop: Int = 4, stream: Int = 0)
    extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Initialize Model Constants

    val arrivalInterval = 0.5                           // deterministic seconds between arrivals
    val linkLength      = 500.0                         // metres
    val freeFlowSpeed   = 10.0                          // metres/second: 50-second traversal
    val capacity        = 1800.0                        // vehicles/hour: two-second release headway
    val storageCapacity = 2                             // admitted vehicles

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Sharp (arrivalInterval, stream)

    //--------------------------------------------------
    // Create Model Components

    val entry = VSource ("entry", this, () => Car (), 0, nStop, iArrivalRV, (100, 290))
    val exit  = Sink ("exit", (600, 290))
    val lane  = MesoVTransport ("lane", entry, exit, freeFlowSpeed, capacity,
                                bend = 0.0, storageCapacity = storageCapacity)
    lane.length = linkLength

    addComponent (entry, exit, lane)                     // add endpoints before transport

    //--------------------------------------------------
    // Specify Scripts for each Type of Simulation Actor

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            lane.move ()                               // move along the link
            exit.leave ()                              // leave the model
        end act

    end Car

    banner ("Expected finite-storage trace")
    println ("V1 admit 0.0; V2 admit 0.5; V3 block 1.0; V4 block 1.5")
    println ("V1 release / V3 wake 50.0; V2 release / V4 wake 52.0")
    println ("V3 release 100.0; V4 release 102.0")

    simulate ()
    waitFinished ()

    val completed = exit.durationStat.num
    val maxOccupancy = lane.persistentStat.max.toInt
    require (lane.enteredCount == nStop, s"entered=${lane.enteredCount}, expected=$nStop")
    require (lane.releasedCount == nStop, s"released=${lane.releasedCount}, expected=$nStop")
    require (completed == nStop, s"completed=$completed, expected=$nStop")
    require (maxOccupancy <= storageCapacity,
             s"maximum occupancy=$maxOccupancy exceeds storage=$storageCapacity")
    require (lane.occupancy == 0, s"final occupancy=${lane.occupancy}, expected=0")
    require (lane.blockedCount == 0, s"blocked vehicles remain=${lane.blockedCount}")
    require (agendaActivations == 15L,
             s"agenda activations=$agendaActivations, expected=15 (polling or missed wakeup)")

    banner ("Finite-storage admission/wakeup: PASS")
    println (s"entered=${lane.enteredCount}, released=${lane.releasedCount}, completed=$completed")
    println (s"maximum occupancy=$maxOccupancy, storage=$storageCapacity, final occupancy=${lane.occupancy}")
    println (s"agenda activations=$agendaActivations (expected 15; blocked vehicles do not poll)")
    Model.shutdown ()

end OneWayVehicleMeso
