//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Model: One-Way Mesoscopic Pathway with Vehicles
 */

package scalation
package simulation
package process
package meso
package example_meso                                    // One-Shot

import scalation.dynamics.IdmFlux
import scalation.mathstat.VectorD
import scalation.mathstat.Statistic
import scalation.random.Exponential
import java.nio.file.Path

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runOneWayPathwayMeso` function is used to launch the `OneWayPathwayMeso` class.
 *  > runMain scalation.simulation.process.meso.example_meso.runOneWayPathwayMeso
 */
@main def runOneWayPathwayMeso (): Unit =
    new OneWayPathwayMeso (name = "Fixed meso")
end runOneWayPathwayMeso


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runOneWayPathwayMesoIdm` function launches the same pathway with the
 *  density-sensitive IDM equilibrium speed built from the microscopic Vehicle
 *  parameters.  This example is separate from the frozen C = 2390 veh/h,
 *  K = 133 calibration in the validation log.
 *  > runMain scalation.simulation.process.meso.example_meso.runOneWayPathwayMesoIdm
 */
@main def runOneWayPathwayMesoIdm (): Unit =
    val fd = new IdmFlux (v0 = Vehicle.vmax, tau = Vehicle.T, s0 = Vehicle.s,
                          len = Vehicle.len, delta = Vehicle.del)
    new OneWayPathwayMeso (name = "IDM meso", flowDiagram = Some (fd), traceSpeeds = true)
end runOneWayPathwayMesoIdm


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OneWayPathwayMeso` class simulates a one-lane mesoscopic road.
 *  Caveat: must add 'from' and 'to' components before transport.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number of arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 *  @param flowDiagram optional density-sensitive equilibrium speed relation; when absent,
 *                     every segment keeps the fixed `freeFlowSpeed`, unchanged from prior behavior
 *  @param traceSpeeds  print each IDM density/speed lookup
 *  @param warmups    initial runs omitted from the summary; for 10 measured runs,
 *                    set `reps = 12, warmups = 2`. Defaults preserve one ordinary run.
 */
class OneWayPathwayMeso (name: String = "OneWayPathwayMeso", reps: Int = 12, animating: Boolean = false,
                        aniRatio: Double = 500.0, nStop: Int = 30, stream: Int = 0,
                        flowDiagram: Option [IdmFlux] = None, warmups: Int = 2,
                        traceSpeeds: Boolean = false)
    extends Model (name, reps, animating, aniRatio, warmupReps = warmups):

    //--------------------------------------------------
       // Initialize Model Constants

    val lambda         = 3200.0                           // Experiment B held-out demand
    val numJunc        = 1                              // number of intermediate junctions
    val freeFlowSpeed  = 68.0 / 2.23694                 // free-flow speed (metres/second)
    val capacity       = 2390.0                         // discharge capacity (vehicles/hour)
    val storageCapacity = 133                            // frozen from micro 3600 veh/h maximum occupancy
    val segmentLengths = new VectorD (numJunc + 1)      // zero uses the display length

    Vehicle.setInitialSpeed (freeFlowSpeed)

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential(HOUR / lambda, stream)

    //--------------------------------------------------
    // Create Model Components

    val entry = VSource("entry", this, () => Car(), 0, nStop, iArrivalRV, (100, 290))
    val exit = Sink("exit", (1500, 290))
    val junc = Array.ofDim[Junction](numJunc)

    val spacing = (exit.at(0) - entry.at(0)) / (numJunc + 1)
    for i <- junc.indices do
        junc(i) = Junction(s"jc-$i", xy = (entry.at(0) + spacing * (i + 1), 290), nt = 2, nl = 1)
    end for

    val lane = PathwayMeso ("lane", junc, entry, exit, segmentLengths, freeFlowSpeed, capacity,
                            storageCapacity = storageCapacity, flowDiagram = flowDiagram,
                            traceSpeed = traceSpeeds)

    addComponents (List (entry), junc.toList, List (exit), List (lane))  // add endpoints before transport

    //--------------------------------------------------
    // Specify Scripts for each Type of Simulation Actor

    val linkCounts = Array.ofDim [Int] (lane.seg.length)                 // number of completed vehicles per link
    val linkTimes  = Array.ofDim [Double] (lane.seg.length)              // total travel time per link

    private val completedPerRepStat = new Statistic ("completed vehicles per replication")
    private val link0SpeedStat = new Statistic ("link 0 speed (m/s)")
    private val meanTravelTimeStat = new Statistic ("mean travel time (s)")
    private val simulationTimeStat = new Statistic ("simulation time (s)")
    private val computationTimeStat = new Statistic ("computation time (ms)")
    private val agendaActivationsStat = new Statistic ("agenda activations per replication")
    private val activationsPerVehicleStat = new Statistic ("activations per vehicle")
    private var lastReplicationComputationTimeMs = 0.0

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            for i <- lane.seg.indices do
                val entryTime = director.clock
                lane.seg(i).move ()
                linkCounts(i) += 1
                linkTimes(i) += director.clock - entryTime
                if i < lane.junc.length then lane.junc(i).jump ()
            end for
            exit.leave ()                              // leave the model
        end act

    end Car

    override protected def replicationCompleted (rep: Int, computationTimeMs: Double): Unit =
        lastReplicationComputationTimeMs = computationTimeMs
        if rep > warmups then
            val completed = exit.durationStat.num
            val travelTime = exit.durationStat.mean
            val link0Time = if linkCounts(0) > 0 then linkTimes(0) / linkCounts(0) else 0.0
            completedPerRepStat.tally (completed.toDouble)
            link0SpeedStat.tally (if link0Time > 0.0 then lane.seg(0).length / link0Time else 0.0)
            meanTravelTimeStat.tally (travelTime)
            simulationTimeStat.tally (simDuration)
            computationTimeStat.tally (computationTimeMs)
            agendaActivationsStat.tally (agendaActivations.toDouble)
            activationsPerVehicleStat.tally (if completed > 0 then agendaActivations.toDouble / completed else 0.0)
        end if
        if rep < reps then
            java.util.Arrays.fill (linkCounts, 0)
            java.util.Arrays.fill (linkTimes, 0.0)
        end if
    end replicationCompleted

    simulate ()
    waitFinished ()
    val computationTimeMs = lastReplicationComputationTimeMs

    //--------------------------------------------------
    // Collect and report statistics

    val completed  = exit.durationStat.num                 // Numbers of vehicles that completed the pathway
    val meanTravel = exit.durationStat.mean                // and the mean travel time
    val runSecs    = simDuration                           // the one run-level fact the Model supplies
    val throughput = if runSecs > 0.0 then completed * HOUR / runSecs else 0.0
    val activationsPerVehicle = if completed > 0 then agendaActivations.toDouble / completed else 0.0
    val recorderReports = Array.ofDim [SimulationRecorderReport] (lane.junc.length)
    cfor (0, lane.junc.length) { i =>
        val (counts, speeds) = lane.junc(i).getRecorderMat
        recorderReports(i) = SimulationRecorderReport (lane.junc(i).name, lane.junc(i).rowTime,
            counts, speeds, lane.junc(i).getDensityMat)
    }
    val laneCount = if recorderReports.nonEmpty then recorderReports(0).counts.dim2 else 0
    val linkReports = Array.ofDim [SimulationLinkReport] (lane.seg.length)
    for i <- lane.seg.indices do
        val segment = lane.seg(i)
        val flow = if runSecs > 0.0 then linkCounts(i) * HOUR / runSecs else 0.0
        val travelTime = if linkCounts(i) > 0 then linkTimes(i) / linkCounts(i) else 0.0
        val speed = if travelTime > 0.0 then segment.length / travelTime else 0.0
        val density =
            if runSecs > 0.0 && segment.length > 0.0 then
                linkTimes(i) * 1000.0 / (runSecs * segment.length)
            else 0.0
        val maxOccupancy = segment.persistentStat.max.toInt
        val maxDensity = if segment.length > 0.0 then maxOccupancy * 1000.0 / segment.length else 0.0
        linkReports(i) = SimulationLinkReport (segment.name, segment.from.name, segment.to.name,
            segment.length, linkCounts(i), flow, travelTime, speed, density, maxOccupancy, maxDensity)
    end for

    val runReport = SimulationRunReport (name, nStop, lane.junc.length, lane.seg.length, laneCount,
        completed, meanTravel, runSecs, computationTimeMs, throughput, agendaActivations, activationsPerVehicle,
        HOUR / lambda, recorderReports, linkReports)
    banner ("Meso pathway JSON report")
    println (s"simulation_run_json =\n${runReport.prettyJson}")

    val experimentSummary = SimulationExperimentSummary (name, warmups, reps - warmups, nStop,
        SimulationMetricSummary.from (completedPerRepStat),
        SimulationMetricSummary.from (link0SpeedStat),
        SimulationMetricSummary.from (meanTravelTimeStat),
        SimulationMetricSummary.from (simulationTimeStat),
        SimulationMetricSummary.from (computationTimeStat),
        SimulationMetricSummary.from (agendaActivationsStat),
        SimulationMetricSummary.from (activationsPerVehicleStat))
    val summaryFile = if flowDiagram.isDefined then "idm_meso.json" else "fixed_meso.json"
    val summaryPath = Path.of ("log", "simulation", "Simulation-plot", summaryFile)
    experimentSummary.writeJson (summaryPath)
    println (s"simulation_experiment_json = ${summaryPath}")
    println (experimentSummary.prettyJson)

    Model.shutdown ()

end OneWayPathwayMeso
