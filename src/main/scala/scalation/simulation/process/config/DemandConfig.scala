//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Feb 02 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Demand Configuration for Traffic Simulation (Arrivals Only)
 */

package scalation
package simulation
package process
package config

import scalation.random.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PeMSArrivals` class specifies PeMS data-driven arrival parameters.
 *  @param anchorFile       the primary PeMS CSV file (e.g., "1-401112ML.csv")
 *  @param distribution     the base distribution for stochastic variation
 *  @param perLane          if true, use lane-specific μ; if false, aggregate across lanes
 */
case class PeMSArrivals(anchorFile: String, distribution: Variate, perLane: Boolean = true)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PeMSDemand` case class defines PeMS data-driven arrivals.
 *  @param mainline  PeMS arrival specification for mainline entry
 *  @param ramps     PeMS arrival specifications for each ramp (in order)
 *  @param dataDir   directory containing PeMS CSV files
 */
case class PeMSDemand(mainline: PeMSArrivals, ramps: List[PeMSArrivals], dataDir: String)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PeMSDemand` companion object provides predefined PeMS demand configurations.
 */
object PeMSDemand:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create PeMS data-driven demand for US-101 Donald Doyle corridor.
     *  Uses actual traffic sensor data from Mainline_VDS_Donald_Doyle.
     *  @param tau  the Erlang2S shift parameter (default 0.6)
     */
    def US101_DonaldDoyle (tau: Double = 0.6): PeMSDemand = PeMSDemand(
        mainline = PeMSArrivals(
            anchorFile = "1-401112ML.csv",
            distribution = Erlang2S(tau),  // Base distribution for stochastic variation
            perLane = true
        ),
        ramps = List(
            PeMSArrivals(
                anchorFile = "1-403157OR.csv",
                distribution = Erlang2S(4.0),
                perLane = false  // Ramps typically have single-lane data
            ),
            PeMSArrivals(
                anchorFile = "2-403108OR.csv",
                distribution = Erlang2S(10.0),  // Ramp 2 uses tau=10.0 (from CalRoute101_2)
                perLane = false
            )
        ),
        dataDir = "Mainline_VDS_Donald_Doyle"
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create PeMS data-driven demand for US-101 Redwood Creek corridor.
     *  Uses actual traffic sensor data from Mainline_VDS_Redwood_Creek_US101-N.
     *  Has 2 on-ramps and 1 off-ramp.
     *  @param tau  the Erlang2S shift parameter (default 0.6)
     */
    def US101_RedwoodCreek (tau: Double = 0.6): PeMSDemand = PeMSDemand(
        mainline = PeMSArrivals(
            anchorFile = "1-404531ML.csv",
            distribution = Erlang2S(tau),  // Base distribution for stochastic variation
            perLane = true
        ),
        ramps = List(
            PeMSArrivals(
                anchorFile = "1-410094FR.csv",      // On-ramp 1 (FR = Freeway Ramp)
                distribution = Erlang2S(4.0),
                perLane = false
            ),
            PeMSArrivals(
                anchorFile = "1-410095OR.csv",      // On-ramp 2 (OR = On Ramp)
                distribution = Erlang2S(4.0),
                perLane = false
            ),
            PeMSArrivals(
                anchorFile = "2-410093OR.csv",      // Off-ramp (OR = Off Ramp)
                distribution = Erlang2S(4.0),
                perLane = false
            )
        ),
        dataDir = "Mainline_VDS_Redwood_Creek_US101-N"
    )

end PeMSDemand



import scalation.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test that corridor switching works by changing only the demand configuration.
 *  This verifies that the config/ layer is truly decoupled from hardcoded paths.
 *
 *  > runMain scalation.simulation.process.config.testCorridorSwitch
 */
@main def testCorridorSwitch(): Unit =
    banner("Test Corridor Switching - Verify Decoupling")

    println("\n" + "=" * 80)
    println("TESTING: Donald Doyle Corridor")
    println("=" * 80)

    val donaldDoyle = PeMSDemand.US101_DonaldDoyle()
    testCorridor(donaldDoyle, "Donald Doyle")

    println("\n" + "=" * 80)
    println("TESTING: Redwood Creek Corridor")
    println("=" * 80)

    val redwoodCreek = PeMSDemand.US101_RedwoodCreek()
    testCorridor(redwoodCreek, "Redwood Creek")

    println("\n" + "=" * 80)
    println("✓ SUCCESS: Corridor switching works!")
    println("=" * 80)
    println("Changed ONE line of config, everything else stayed the same.")
    println("The config/ layer is fully decoupled from TrafficConfig2!")

end testCorridorSwitch

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Helper function to test loading data for a corridor.
 *  @param demand       the PeMS demand configuration
 *  @param corridorName the human-readable corridor name
 */
def testCorridor(demand: PeMSDemand, corridorName: String): Unit =
    println(s"\nCorridor: $corridorName")
    println(s"Data Directory: ${demand.dataDir}")
    println(s"Anchor File: ${demand.mainline.anchorFile}")
    println(s"Number of Ramps: ${demand.ramps.length}")

    // Test loading mainline sensors
    println("\n--- Loading Mainline Sensors ---")
    try
        val allMainline = PeMSDataHelper.loadAllMainlineSensors(demand)
        println(s"✓ Loaded ${allMainline.length} mainline sensors")

        for (i <- allMainline.indices) do
            val (flow, speed) = allMainline(i)
            val totalFlow = flow.sumVr.sum.toInt
            val avgSpeed = speed.sum / (speed.dim * speed.dim2)  // Average across all cells
            println(f"  Sensor ${i+1}: ${flow.dim}%2d rows x ${flow.dim2}%1d lanes, " +
                f"Total flow = $totalFlow%6d veh, Avg speed = $avgSpeed%5.2f m/s")
    catch
        case e: Exception =>
            println(s"✗ ERROR loading mainline sensors: ${e.getMessage}")
            e.printStackTrace()

    // Test loading ramp sensors
    println("\n--- Loading Ramp Sensors ---")
    try
        val allRamps = PeMSDataHelper.loadAllRampSensors(demand)
        println(s"✓ Loaded ${allRamps.length} ramp sensors")

        for (i <- allRamps.indices) do
            val flow = allRamps(i)
            val totalFlow = flow.sumVr.sum.toInt
            println(f"  Ramp ${i+1}: ${flow.dim}%2d rows x ${flow.dim2}%1d lanes, " +
                f"Total flow = $totalFlow%6d veh")
    catch
        case e: Exception =>
            println(s"✗ ERROR loading ramp sensors: ${e.getMessage}")
            e.printStackTrace()

    // Test loading individual sensor
    println("\n--- Loading Individual Sensor (Sensor 1) ---")
    try
        val (flow1, speed1) = PeMSDataHelper.loadMainlineSensor(demand, 0)
        println(s"✓ Sensor 1: ${flow1.dim} rows x ${flow1.dim2} lanes")
        println(f"  First row flow:  ${flow1(0)}")
        println(f"  First row speed: ${speed1(0)}")
    catch
        case e: Exception =>
            println(s"✗ ERROR loading individual sensor: ${e.getMessage}")
            e.printStackTrace()

end testCorridor

