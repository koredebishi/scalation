
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Model: Drive for Agent-Based Simulation
 */

package scalation
package simulation.agent
package example_1                                     // One-Shot

import scalation.mathstat.VectorD
import scalation.random.{Exponential, Uniform}
import scalation.random.RandomSeeds.N_STREAMS

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runDrive` function is used to launch the `DriveModel` class.
 *  > runMain scalation.simulation.agent.example_1.runDrive
 */
@main def runDrive (): Unit = new DriveModel ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DriveModel` class defines a simple agent-based simulation model of a Model
 *  where service is provided by one or more tellers.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param startSim   the start time of the simulation
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 */
class DriveModel (name: String = "Drive", reps: Int = 1, startSim: Double = 0.0,
                 animating: Boolean = true, aniRatio: Double = 10.0,
                 nStop: Int = 100, stream: Int = 0)
  extends Model (name, reps, startSim, animating, aniRatio):

  //--------------------------------------------------
  // Initialize Model Constants

  val lambda   = 1.0                                // customer arrival rate (per hour
  val mvTime  = (450.0, 550.0)


  //--------------------------------------------------
  // Create Random Variates (RVs)

  val iArrivalRV = Exponential (HOUR / lambda, stream)
  val moveRV     = Uniform (mvTime,(stream + 2) % N_STREAMS)
  //val moveRV     = Uniform (4 * MINUTE, 6 * MINUTE, (stream + 2) % N_STREAMS)

  //--------------------------------------------------
  // Create the Graph Model: Vertices and Edges

  val entry_pos = Source.at (100, 290)
  val cust_pos  = VectorD (110, 290, 10, 10)         // FIX - need general solution for multiple sources

  val entry     = Source ("entry", this, 0.0, iArrivalRV, () => Customer (), nStop, pos = entry_pos)
  val exit      = Sink ("door", this, pos = Sink.at (800, 290))
  val toExit    = Transport ("toExit", this, entry.vert, exit, moveRV)

  //--------------------------------------------------
  // Specify Scripts for each Type of Simulation Agent

  case class Customer () extends SimAgent ("c", director.clock, this, cust_pos.copy):

    def act (): Unit =
      println(s"Car $this pos=$pos")
      toExit.move (this)
      exit.leave (this)
    end act

  end Customer

  simulate ()
  waitFinished ()
  Model.shutdown ()

end DriveModel


