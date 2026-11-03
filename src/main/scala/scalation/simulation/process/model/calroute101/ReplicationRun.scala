//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Tue Nov 3 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Independent replications of the micro, mixed and meso corridor models.
 *           Each model runs `REPS` replications in one `Model` run (`Model.reps`):
 *           random number streams continue from one replication to the next, so
 *           replications are independent, and replication k of every model draws
 *           the same arrival numbers (common random numbers).
 */

package scalation
package simulation
package process
package model
package calroute101

import scala.collection.mutable.ArrayBuffer

import scalation.{banner, gauge}
import scalation.mathstat.{MatrixD, Statistic, VectorD}
import scalation.simulation.process.config.{DynamicsConfig, PeMSDemand}
import scalation.simulation.process.model.redwood101.{RedwoodCreek101, RedwoodCreek101_fullMeso,
                                                     RedwoodCreek101_withMeso}

/** Number of measured replications per model. */
private val REPS = 10

/** Per-replication measures, in column order of `<model>_per_rep.csv`. */
private val MEASURES = Array ("flow NRMSE*100", "speed NRMSE*100", "fitness", "mean travel time (s)",
                              "computation time (ms)", "agenda activations")

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Replications for US-101 Donald Doyle, written to log/simulation/pads_reps/donalddoyle.
 *  > runMain scalation.simulation.process.model.calroute101.repsDonaldDoyle
 */
@main def repsDonaldDoyle (): Unit =
    val dynamics = TrafficModelBuilder.US101_DonaldDoyle_PeMS ().getDynamics      // as `evalRun`
    runReplications ("donalddoyle", dynamics, Seq (
        "micro" -> (() => new CalRoute101_3 (reps = REPS, animating = false)),
        "mixed" -> (() => new CalRoute101_3_withMeso (reps = REPS, animating = false)),
        "meso"  -> (() => new CalRoute101_3_fullMeso (reps = REPS, animating = false))))
end repsDonaldDoyle

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Replications for US-101 Redwood Creek, written to log/simulation/pads_reps/redwood.
 *  > runMain scalation.simulation.process.model.calroute101.repsRedwood
 */
@main def repsRedwood (): Unit =
    runReplications ("redwood", DynamicsConfig.idmDefault, Seq (                   // as `evalRedwoodRun`
        "micro" -> (() => new RedwoodCreek101 (reps = REPS, animating = false)),
        "mixed" -> (() => new RedwoodCreek101_withMeso (reps = REPS, animating = false)),
        "meso"  -> (() => new RedwoodCreek101_fullMeso (reps = REPS, animating = false))))
end repsRedwood

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run each model for `REPS` replications, write its results, then compare
 *  every model with the first (micro) by paired differences.
 *  @param corridor  the sub-folder of log/simulation/pads_reps/ (must exist)
 *  @param dynamics  the car-following configuration (parameters applied once)
 *  @param models    (label, builder) for each model, micro first
 */
private def runReplications (corridor: String, dynamics: DynamicsConfig,
                             models: Seq [(String, () => Model & CalRouteReadings)]): Unit =
    val params = dynamics.cfParams.toVector
    Vehicle.setProps (Vehicle.setParams (params))
    val folder = s"pads_reps/$corridor"

    val results = for (label, make) <- models yield
        banner (s"REPLICATIONS - $corridor $label ($REPS reps)")
        val result = new ReplicationResults (label)
        var model: Model & CalRouteReadings = null
        val setupMs = gauge { model = make () }                                 // model construction
        model.onReplication = (rep, ms) => result.add (model, ms)
        val runMs = gauge { model.simulate (); model.waitFinished () }          // all replications
        result.write (folder, model.getDemandConfig, setupMs, runMs)
        result

    writeComparison (folder, results)
    Model.shutdown ()
end runReplications

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Paired differences (model - first model) per replication, with 95% t intervals.
 *  Valid because replication k of every model uses the same arrival numbers.
 *  @param folder   the output sub-folder of log/simulation/
 *  @param results  the results of each model, the reference model first
 */
private def writeComparison (folder: String, results: Seq [ReplicationResults]): Unit =
    val base = results.head
    val ew   = new EasyWriter ("simulation", s"$folder/comparison.txt")
    def emit (line: String): Unit = { println (line); ew.println (line) }

    emit (s"Paired differences over $REPS replications (model - ${base.label}), mean +/- 95% t half-width")
    for other <- results.tail; j <- 0 until 3 do                               // flow, speed, fitness
        val diff = new Statistic (s"${other.label} - ${base.label}: ${MEASURES(j)}")
        for k <- base.rows.indices do diff.tally (other.rows(k)(j) - base.rows(k)(j))
        val (m, h) = (diff.mean, diff.interval ())
        val verdict = if m - h > 0.0 || m + h < 0.0 then "differs" else "no difference"
        emit (f"${diff.name}%-45s ${m}%9.4f +/- ${h}%.4f   ($verdict)")
    ew.close ()
end writeComparison

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ReplicationResults` class keeps one model's per-replication measures and
 *  the running sums of its recorder matrices, read in `Model.replicationCompleted`
 *  before the recorders and statistics are reset.
 *  @param label  the model label (micro, mixed, meso)
 */
private class ReplicationResults (val label: String):

    val rows = ArrayBuffer [VectorD] ()                                         // one row per replication, MEASURES order
    private var sumFlow: Array [MatrixD]    = null                             // per sensor: sum over replications
    private var sumSpeed: Array [MatrixD]   = null
    private var sumDensity: Array [MatrixD] = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Record one completed replication.
     *  @param model              the model, between its replication end and reset
     *  @param computationTimeMs  scheduler wall-clock time for this replication
     */
    def add (model: Model & CalRouteReadings, computationTimeMs: Double): Unit =
        val junctions  = model.getJunctions
        val sensors    = model.getPemsSensorIndices
        val validation = SimulationReport.fromJunctions (junctions, sensors, model.getDemandConfig).validate ()

        val sinkStats  = ArrayBuffer.from (model.getComponents.collect { case s: Sink => s.durationStat })
        require (sinkStats.nonEmpty, s"$label: no sinks found among the model's components")
        val travelTime = Statistic.aggregate (sinkStats, "sinks").mean     // pooled over all sinks

        rows += VectorD (validation.avgFlowNRMSE, validation.avgSpeedNRMSE, validation.fitness,
                         travelTime, computationTimeMs, model.agendaActivations.toDouble)

        val n = sensors.length
        if sumFlow == null then
            sumFlow = new Array [MatrixD] (n); sumSpeed = new Array [MatrixD] (n); sumDensity = new Array [MatrixD] (n)
        for s <- 0 until n do
            val junction      = junctions(sensors(s))
            val (flow, speed) = junction.getRecorderMat
            val density       = junction.getDensityMat
            sumFlow(s)    = if sumFlow(s) == null then flow else sumFlow(s) + flow
            sumSpeed(s)   = if sumSpeed(s) == null then speed else sumSpeed(s) + speed
            sumDensity(s) = if sumDensity(s) == null then density else sumDensity(s) + density
        println (f"[rep ${rows.size}] $label: flow ${validation.avgFlowNRMSE}%.4f  speed ${validation.avgSpeedNRMSE}%.4f" +
                 f"  fitness ${validation.fitness}%.4f")
    end add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write the per-replication table, the summary (mean, sd, 95% t half-width)
     *  and the mean-series validation CSV (for figures, not for NRMSE).
     *  @param folder   the output sub-folder of log/simulation/
     *  @param demand   the PeMS demand the model used
     *  @param setupMs  model construction wall time
     *  @param runMs    wall time of all replications
     */
    def write (folder: String, demand: PeMSDemand, setupMs: Double, runMs: Double): Unit =
        val table = new EasyWriter ("simulation", s"$folder/${label}_per_rep.csv")
        table.println ("rep,flow_nrmse,speed_nrmse,fitness,mean_travel_time_s,computation_ms,agenda_activations")
        for (row, k) <- rows.zipWithIndex do
            table.println (s"${k + 1}," + (0 until row.dim).map (j => row(j).toString).mkString (","))
        table.close ()

        val summary = new EasyWriter ("simulation", s"$folder/${label}_summary.txt")
        def emit (line: String): Unit = { println (line); summary.println (line) }
        emit (f"$label: ${rows.size} replications   setup ${setupMs / 1000}%.1f s   run ${runMs / 1000}%.1f s")
        for j <- MEASURES.indices do
            val stat = new Statistic (MEASURES(j))
            for row <- rows do stat.tally (row(j))
            val m = SimulationMetricSummary.from (stat)
            emit (f"${MEASURES(j)}%-24s mean ${m.mean}%12.4f   sd ${m.standardDeviation.getOrElse (0.0)}%10.4f" +
                  f"   95%% CI +/- ${m.ci95.getOrElse (0.0)}%.4f")
        summary.close ()

        val n = rows.size.toDouble
        val nSensors = sumFlow.length
        val mean = new SimulationReport (sumFlow.map (_ / n), sumSpeed.map (_ / n), sumDensity.map (_ / n),
                                         demand, nSensors, sumFlow(0).dim2, sumFlow(0).dim)
        mean.exportCSV (s"$folder/${label}_mean_validation_data.csv")
    end write

end ReplicationResults
