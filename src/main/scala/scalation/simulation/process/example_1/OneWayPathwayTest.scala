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
class OneWayPathwayTest(name: String = "OneWayVehicle", reps: Int = 1, animating: Boolean = false,
                        aniRatio: Double = 500.0, nStop: Int = 300, stream: Int = 0)
    extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda        = 5400.0                              // Experiment B held-out demand
    val freeFlowSpeed = 68.0 / 2.24694                    // initial speed (metres/second)

    Vehicle.setInitialSpeed (freeFlowSpeed)

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (HOUR / lambda, stream)
    val motion     = IDMDynamics
    val numJunc = 0
    //--------------------------------------------------
    // Create Model Components
    val entry = VSource("entry", this, () => Car(), 0, nStop, iArrivalRV, (100, 290))
    val exit = Sink("exit", (1500, 290))
    val junc = Array.ofDim[Junction](numJunc)    // an array of numJunc

    val spacing = (exit.at(0) - entry.at(0)) / (numJunc + 1)    // need this spacing for the junc components
    for i <- 0 until numJunc do
        junc(i) = Junction(s"jc-$i", xy = (entry.at(0) + spacing * (i + 1), 290), nt = 2, nl = 1)
    end for

    val lane  = Pathway("lane",junc,entry,exit,motion, laneShift = VectorD(0.0, 0.0)) // simple and clean
    addComponents (List(entry),junc.toList, List(exit), List(lane))                      // Caveat: must add from and to before transport!!

    //--------------------------------------------------
    // Specify Scripts for each Type of Simulation Actor

    val linkCounts = Array.ofDim [Int] (lane.seg.length)
    val linkTimes  = Array.ofDim [Double] (lane.seg.length)

    case class Car() extends Vehicle("c", this):

        override def act(): Unit =


            this.laneID = subtype                                                           //Assign this laneID to the Vehicle

            val carAhead = lane.getLast // Follow the last vehicle in the lane
            lane.joinAt(this, carAhead, 0) // Join the doubly linked list at seg 0


            for i <- lane.seg.indices do
                val entryTime = director.clock
                lane.seg(i).move ()
                linkCounts(i) += 1
                linkTimes(i) += director.clock - entryTime
                if i < lane.junc.length then lane.junc(i).jump ()
            end for

            lane.removeFromAlist(this) // Remove from the lane at the end
            exit.leave() // Exit the system
        end act
    end Car


    simulate ()
    waitFinished ()

    val completed  = exit.durationStat.num                 // Sink owns the completed-vehicle count
    val meanTravel = exit.durationStat.mean                // and the mean travel time
    val runSecs    = simDuration                           // the one run-level fact the Model supplies
    banner ("Micro pathway statistics")
    println (s"Generated vehicles: $getNumActors")
    println (s"Junctions: ${lane.junc.length}")
    println (s"Transport segments: ${lane.seg.length}")
    println ("Lanes: 1")
    println (s"Completed vehicles: $completed")
    println (s"Mean travel time (s): $meanTravel")
    println (s"Simulation duration (s): $runSecs")
    println (s"Whole-run throughput (veh/h): ${if runSecs > 0.0 then completed * HOUR / runSecs else 0.0}")
    println (s"Agenda activations: $agendaActivations")
    println (s"Agenda activations per completed vehicle: ${if completed > 0 then agendaActivations.toDouble / completed else 0.0}")
    println (s"Mean arrival interval (s): ${HOUR / lambda}")
    // Whole-run trial averages for a fully drained road; junction time is excluded.
    println ("Link | From -> To | Length (m) | Completed | Whole-run flow (veh/h) | " +
             "Mean travel time (s) | Mean traversal speed (km/h) | Whole-run mean density (veh/km/lane) | " +
             "Maximum occupancy | Maximum instantaneous density (veh/km/lane)")
    for i <- lane.seg.indices do
        val segment = lane.seg(i)
        val flow = if runSecs > 0.0 then linkCounts(i) * HOUR / runSecs else 0.0
        val travelTime = if linkCounts(i) > 0 then linkTimes(i) / linkCounts(i) else 0.0
        val speed = if travelTime > 0.0 then 3.6 * segment.length / travelTime else 0.0
        val density =
            if runSecs > 0.0 && segment.length > 0.0 then
                linkTimes(i) * 1000.0 / (runSecs * segment.length)
            else 0.0
        val maxOccupancy = segment.persistentStat.max.toInt
        val maxDensity = if segment.length > 0.0 then maxOccupancy * 1000.0 / segment.length else 0.0
        println (s"${segment.name} | ${segment.from.name} -> ${segment.to.name} | " +
                 f"${segment.length}%.2f | ${linkCounts(i)}%d | $flow%.3f | $travelTime%.3f | $speed%.3f | " +
                 f"$density%.3f | $maxOccupancy%d | $maxDensity%.3f")
    end for

    Model.shutdown ()

end OneWayPathwayTest
