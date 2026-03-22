
package scalation
package simulation
package process

import scalation.random._


object RampNetworkConfig:

    // Define the number of lanes and junctions in the route
    val numLanes     = 4
    val numJunctions = 5

    // Define source types and map them to their respective junction index (entry point)
    val srcNames = Array("mainEntry", "onRamp1")
    val srcToJunc = Map(
        0 -> 0,
        1 -> 1)

    val sinkNames = Array("mainExit", "offE1", "offE2")
    val sinkToJunc = Map(
        0 -> 4,
        1 -> 3,
        2 -> 5)

    val ramps = Map(
        "onRamp1"   -> (1, RampMode.On),
        "offRamp1"  -> (4, RampMode.Off),
        "offRamp2"  -> (5, RampMode.Off)
    )

    // Define arrival random variables for each source (optional setup)
    val arrivalRVMap = Map(
        0 -> Erlang(),
        1 -> Exponential()
    )

    // Define lane change behavior
    val laneChangeRV = Bernoulli()
    val coin         = Bernoulli()

    // Define vehicle parameters by source
    val srcSpeedParams = Map(
        0 -> 68.0 / 2.24694, // mainEntry speed (mph converted to m/s)
        1 -> 40.0 / 2.24694 // onRamp1 speed (slower for merging)
    )

end RampNetworkConfig
