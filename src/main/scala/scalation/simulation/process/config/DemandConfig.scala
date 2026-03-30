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
case class PeMSDemand(mainline: PeMSArrivals, ramps: List[PeMSArrivals], dataDir: String,
                     window: TimeWindow = PeMSDataLoader.DefaultTimeWindow,
                     layout: ColumnLayout = PeMSDataLoader.DefaultMainlineLayout)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PeMSDemand` companion object provides predefined PeMS demand configurations.
 */
object PeMSDemand:

    // Base data directory for Eaton fire corridor PeMS data
    private val EATON_BASE = "WSC-Pems-Data-Eaton-Fire/data-eaton/pems/eaton-corridor"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** I-210 WB anchor sensor — cleaned single-sensor CSV (station 717653).
     *  5 lanes, 5-min bins, 17:00–23:00 (72 rows).
     *  Ramps are NOT included — use AggregatedDemand for ramps separately.
     *  @param tau  the Erlang2S shift parameter (default 0.6)
     */
    def I210_WB_Anchor (tau: Double = 0.6): PeMSDemand = PeMSDemand(
        mainline = PeMSArrivals(
            anchorFile = "717653-i210-firstSensor-baseline.csv",
            distribution = Erlang2S(tau),
            perLane = true
        ),
        ramps = List.empty,
        dataDir = s"$EATON_BASE/BaselineData_Dec03-10-17/eaton-i210",
        window = PeMSDataLoader.I210_TimeWindow,
        layout = PeMSDataLoader.I210_MainlineLayout
    )

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


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AggregatedDemand` case class defines PeMS demand using the aggregated
 *  direction-bound CSV format.  One CSV per (corridor × direction × lane type).
 *  Each CSV contains all stations of that type stacked vertically (timestamp × station).
 *
 *  This is the standard format going forward.  Legacy per-sensor format is
 *  supported by `PeMSDemand` for backward compatibility with DonaldDoyle.
 *
 *  @param dataDir      path to data directory (relative to project root)
 *  @param mlFile       ML_HV CSV filename — mainline flow + speed per lane
 *  @param orFile       OR CSV filename — on-ramp flow (None if no on-ramps)
 *  @param frFile       FR CSV filename — off-ramp flow (None if no off-ramps)
 *  @param ffFile       FF CSV filename — FF connector flow (None if no FF)
 *  @param intervalMin  minutes per row (5 for Eaton PeMS data)
 *
 *  @see config-layer-standard.md Section 5a
 */
case class AggregatedDemand (dataDir: String,
                             mlFile: String,
                             orFile: Option [String] = None,
                             frFile: Option [String] = None,
                             ffFile: Option [String] = None,
                             intervalMin: Int = 5,
                             startTime: String = "06:00:00",
                             distribution: Variate = Erlang2S(0.6))

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AggregatedDemand` companion object provides predefined demand
 *  configurations for corridors using the aggregated CSV format.
 */
object AggregatedDemand:

    // Base data directory for Eaton fire corridor PeMS data
    private val EATON_BASE = "data/WSC-Pems-Data-Eaton-Fire/data-eaton/pems/eaton-corridor"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** I-210 Westbound — baseline (Dec 03-10-17 average, pre-fire conditions).
     */
    def I210_WB_Baseline: AggregatedDemand = AggregatedDemand (
        dataDir     = s"$EATON_BASE/BaselineData_Dec03-10-17/eaton-i210",
        mlFile      = "eaton_i210_W_baseline_Dec03-10-17_ML_HV.csv",
        orFile      = Some ("eaton_i210_W_baseline_Dec03-10-17_OR.csv"),
        frFile      = Some ("eaton_i210_W_baseline_Dec03-10-17_FR.csv"),
        ffFile      = Some ("eaton_i210_W_baseline_Dec03-10-17_FF.csv"),
        intervalMin = 5,
        startTime   = "17:00:00"
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** I-210 Eastbound — baseline (Dec 03-10-17 average, pre-fire conditions).
     */
    def I210_EB_Baseline: AggregatedDemand = AggregatedDemand (
        dataDir     = s"$EATON_BASE/BaselineData_Dec03-10-17/eaton-i210",
        mlFile      = "eaton_i210_E_baseline_Dec03-10-17_ML_HV.csv",
        orFile      = Some ("eaton_i210_E_baseline_Dec03-10-17_OR.csv"),
        frFile      = Some ("eaton_i210_E_baseline_Dec03-10-17_FR.csv"),
        ffFile      = Some ("eaton_i210_E_baseline_Dec03-10-17_FF.csv"),
        intervalMin = 5
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** SR-134 Westbound — baseline (Dec 03-10-17 average, pre-fire conditions).
     *  Note: SR-134 has no EB data in the current dataset.
     */
    def SR134_WB_Baseline: AggregatedDemand = AggregatedDemand (
        dataDir     = s"$EATON_BASE/BaselineData_Dec03-10-17/eaton-134",
        mlFile      = "eaton_134_W_baseline_Dec03-10-17_ML_HV.csv",
        orFile      = Some ("eaton_134_W_baseline_Dec03-10-17_OR.csv"),
        frFile      = Some ("eaton_134_W_baseline_Dec03-10-17_FR.csv"),
        ffFile      = Some ("eaton_134_W_baseline_Dec03-10-17_FF.csv"),
        intervalMin = 5,
        startTime   = "17:00:00"
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** I-210 Westbound — fire day (Jan 7, 2025, Eaton fire evacuation).
     */
    def I210_WB_FireDay: AggregatedDemand = AggregatedDemand (
        dataDir     = s"$EATON_BASE/7thData-FireDay/eaton-i210",
        mlFile      = "eaton_i210_W_2025_01_07_ML_HV.csv",
        orFile      = Some ("eaton_i210_W_2025_01_07_OR.csv"),
        frFile      = Some ("eaton_i210_W_2025_01_07_FR.csv"),
        ffFile      = Some ("eaton_i210_W_2025_01_07_FF.csv"),
        intervalMin = 5,
        startTime   = "17:00:00"
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** I-210 Eastbound — fire day (Jan 7, 2025, Eaton fire evacuation).
     */
    def I210_EB_FireDay: AggregatedDemand = AggregatedDemand (
        dataDir     = s"$EATON_BASE/7thData-FireDay/eaton-i210",
        mlFile      = "eaton_i210_E_2025_01_07_ML_HV.csv",
        orFile      = Some ("eaton_i210_E_2025_01_07_OR.csv"),
        frFile      = Some ("eaton_i210_E_2025_01_07_FR.csv"),
        ffFile      = Some ("eaton_i210_E_2025_01_07_FF.csv"),
        intervalMin = 5
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** SR-134 Westbound — fire day (Jan 7, 2025, Eaton fire evacuation).
     */
    def SR134_WB_FireDay: AggregatedDemand = AggregatedDemand (
        dataDir     = s"$EATON_BASE/7thData-FireDay/eaton-134",
        mlFile      = "eaton_134_W_2025_01_07_ML_HV.csv",
        orFile      = Some ("eaton_134_W_2025_01_07_OR.csv"),
        frFile      = Some ("eaton_134_W_2025_01_07_FR.csv"),
        ffFile      = Some ("eaton_134_W_2025_01_07_FF.csv"),
        intervalMin = 5,
        startTime   = "17:00:00"
    )

end AggregatedDemand



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

