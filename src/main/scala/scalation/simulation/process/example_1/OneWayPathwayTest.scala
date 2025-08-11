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
package example_1

// One-Shot

import scalation.random._
import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runOneWayPathwayTest` function is used to launc h the `OneWayVehicleModel` class.
 *  > runMain scalation.simulation.process.example_1.runOneWayPathwayTest
 */
@main def runOneWayPathwayTest (): Unit = new OneWayPathwayTest ()


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
class OneWayPathwayTest(name: String = "OneWayVehicle", reps: Int = 1, animating: Boolean = true,
                        aniRatio: Double = 500.0, nStop: Int = 10, stream: Int = 0)
    extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 10.0                                     // car arrival rate (per hour)
    private val debug = debugf("OneWayPathwayTest", true) // debug function

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (MINUTE / lambda, stream)
    val motion     = GippsDynamics
    val numJunc = 0
    //--------------------------------------------------
    // Create Model Components
    val entry = VSource("entry", this, () => Car(), 0, nStop, iArrivalRV, (100, 290))
    val exit = Sink("exit", (1500, 290))
    val junc = Array.ofDim[Junction](numJunc)    // an array of numJunc

    val spacing = (exit.at(0) - entry.at(0)) / (numJunc + 1)    // need this spacing for the junc components
    for i <- 0 until numJunc do
        junc(i) = Junction(s"jc-$i", xy = (entry.at(0) + spacing * (i + 1), 290), nt = 2)
    end for

    val lane  = Pathway("lane",junc,entry,exit,motion, laneShift = VectorD(0.0, 0.0)) // simple and clean
    addComponents (List(entry),junc.toList, List(exit), List(lane))                      // Caveat: must add from and to before transport!!

    //--------------------------------------------------
    // Specify Scripts for each Type of Simulation Actor

    case class Car() extends Vehicle("c", this):

        override def act(): Unit =
            println(s"act: Car $this BEGINS")


            Vehicle.setInitialSpeed(68.0 / 2.24694) // Speed initialization
            //::::::::::::::::::::::::::::::::::::::
            this.laneID = subtype                                                           //Assign this laneID to the Vehicle
            debug("Act::: ", s"For $this actor in: lane: ${this.laneID}: LaneID Assigned")

            val carAhead = lane.getLast // Follow the last vehicle in the lane
            println(s"act: carAhead = $carAhead")
            lane.addToAlist(this, carAhead) // Join the doubly linked list


            //:::::::Move through junctions
            for i <- lane.junc.indices do
                lane.seg(i).move() // Move along the current segment
                lane.junc(i).jump() // Take stats at the corresponding junction
            end for

            lane.seg.last.move()    // move at the last junction to exit.

            println(s"act: $this ENDS")
            lane.removeFromAlist(this) // Remove from the lane at the end
            exit.leave() // Exit the system
        end act
    end Car


    simulate ()
    waitFinished ()
    Model.shutdown ()

end OneWayPathwayTest


