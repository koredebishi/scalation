
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Model: Road for Process Interaction Simulation
 */

package scalation
package simulation
package process
package example_1                                       // One-Shot

import scalation.random.{Bernoulli, Uniform}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runRoad` function is used to run the `RoadModel` class.
 *  > runMain scalation.simulation.process.example_1.runRoad
 */
@main def runRoad (): Unit = new RoadModel ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RoadModel` class simulates a two-lane road in two directions, i.e., it
 *  has 2 West-bound lanes and 2 East-bound lanes.  It used a composite class called
 *  `Path`, which will have a `Transport` for each lane.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 */
class RoadModel (name: String = "Road", reps: Int = 1, animating: Boolean = true,
                 aniRatio: Double = 4.0, nStop: Int = 100, stream: Int = 0)
      extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Initialize Model Constants

    val iaTime = (4000.0, 6000.0)                       // (lower, upper) on inter-arrival time
    val mvTime = (2900.0, 3100.0)                       // (lower, upper) on move time

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Uniform (iaTime, stream)           // use different random number streams for independence
    val moveRV     = Uniform (mvTime, stream + 1)
    val laneRV     = Bernoulli (stream = stream + 2)

    //--------------------------------------------------
    // Create Model Components

    val src1  = Source ("src1", this, () => Car1 (), 0, nStop, iArrivalRV, (400, 220))
    val src2  = Source ("src2", this, () => Car2 (), 0, nStop, iArrivalRV, (100, 260))
    val snk1  = Sink ("snk1", (100, 220))
    val snk2  = Sink ("snk2", (400, 260))
    val road1 = Path ("road1", 2, src1, snk1, moveRV)
    val road2 = Path ("road2", 2, src2, snk2, moveRV)

    addComponent (src1, src2, snk1, snk2, road1, road2)

    //--------------------------------------------------
    // Specify Scripts for each Type of Simulation Actor

    case class Car1 () extends SimActor ("c1", this):   // for West-bound cars

        override def act (): Unit =
            val l = laneRV.igen                         // pick a lane
            road1.lane(l).move ()                       // move down road 1
            snk1.leave ()                               // exit road 1 via sink 1
        end act

    end Car1

    case class Car2 () extends SimActor ("c2", this):   // for East-bound cars

        override def act (): Unit =
            val l = laneRV.igen                         // pick a lane
            road2.lane(l).move ()                       // move down road 2
            snk2.leave ()                               // exit road 2 via sink 2
        end act

    end Car2

    simulate ()
    waitFinished ()
    Model.shutdown ()

end RoadModel

