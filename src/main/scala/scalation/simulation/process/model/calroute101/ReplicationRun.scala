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

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//  Text output format.  The CSV files are the source; the text files (summary,
//  comparison, validation report) are views of them.  Every view uses the same
//  header, the same labels and the same cell, so a name means the same thing in
//  every file, and each block names the function and formula that produced it.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

/** Per-replication measures, in column order of `<model>_per_rep.csv`:
 *  (label, decimals, formula).
 */
private val MEASURES = Array (
    ("flow NRMSE (%)",        4, "mean over the stations of station flow NRMSE (%)              [SimulationReport.validate: avgFlowNRMSE]"),
    ("speed NRMSE (%)",       4, "mean over the stations of station speed NRMSE (%)             [SimulationReport.validate: avgSpeedNRMSE]"),
    ("fitness",               4, "0.2 * flow NRMSE + 0.8 * speed NRMSE                          [SimulationReport.validate: fitness]"),
    ("mean travel time (s)",  2, "Statistic.aggregate (durationStat of every Sink).mean         [ReplicationResults.add]"),
    ("computation time (ms)", 1, "scheduler wall-clock time of the replication                  [Model.simulate -> onReplication]"),
    ("agenda activations",    0, "Model.agendaActivations: times the scheduler ran a vehicle"))

/** Validation metrics of `<model>_metrics.csv`: (CSV key, column label, decimals,
 *  station-level formula).  `cells` in `writeMetrics` follows this order.
 */
private val METRICS = Array (
    ("r2",          "R2",          4, "1 - SSE / SST over all cells (time x lane), SST about the per-cell mean"),
    ("mae",         "MAE",         2, "sum |obs - sim| / n over all cells"),
    ("rmse",        "RMSE",        2, "sqrt (SSE / n) over all cells"),
    ("series_rmse", "Series RMSE", 2, "sqrt (sum_t (obs_t - sim_t)^2 / T) of the station series: lane sum (flow) or flow-weighted lane mean (speed)"),
    ("nrmse",       "NRMSE (%)",   2, "100 * Series RMSE / mean_t (observed station series)"),
    ("smape",       "sMAPE (%)",   2, "100 * mean over cells of 2 |obs - sim| / (|obs| + |sim|)"),
    ("pems_mean",   "PeMS mean",   2, "mean_t (observed station series); the same in every replication, so printed alone"),
    ("sim_mean",    "Sim mean",    2, "mean_t (simulated station series)"))

/** Lane-level formulas where they differ from the station level: a lane is its own series. */
private val LANE_FORMULA = Map (
    "series_rmse" -> "RMSE (a lane is its own series)",
    "nrmse"       -> "100 * RMSE / mean_t (observed lane)",
    "pems_mean"   -> "mean_t (observed lane); the same in every replication, so printed alone",
    "sim_mean"    -> "mean_t (simulated lane)")

private val RULE_W  = 150
private val RULE    = "=" * RULE_W
private val DASH    = "-" * RULE_W
private val CELL_W  = 20
private val CI_RULE = "mean, sd = Statistic.mean, Statistic.stdev over the replications; " +
                      "half-width = t(0.975, n-1) * sd / sqrt(n)   [Statistic.interval]"

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The header every block starts with: title, context, source, ci rule, one
 *  formula per column, then a rule.
 *  @param title     what the block holds
 *  @param context   corridor, model and replication count (see `context`)
 *  @param source    the file and function that computed the numbers
 *  @param formulas  one line per column
 */
private def blockHeader (title: String, context: String, source: String, formulas: Seq [String]): Seq [String] =
    (Seq (RULE, title, context, s"source:  $source", s"ci:      $CI_RULE") ++ formulas.map (f => s"formula: $f")) :+ RULE

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** "mean +/- half-width" in a fixed-width cell.  A value with no spread over the
 *  replications, such as the observed mean, prints alone.
 *  @param m    the summary over replications
 *  @param dec  decimals
 */
private def cell (m: SimulationMetricSummary, dec: Int): String =
    val h = m.ci95.getOrElse (0.0)
    val s = if h.isNaN || scala.math.abs (h) < 1e-6 then s"%.${dec}f".format (m.mean)
            else s"%.${dec}f +/- %.${dec}f".format (m.mean, h)
    s.padTo (CELL_W, ' ')
end cell

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The context line: "corridor / model: mean +/- 95% Student-t half-width over n replications".
 *  @param corridor  the output sub-folder name
 *  @param label     the model label, or "all models"
 *  @param n         the number of measured replications
 */
private def context (corridor: String, label: String, n: Int): String =
    s"$corridor / $label: mean +/- 95% Student-t half-width over $n replications"

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
    val base     = results.head
    val corridor = folder.split ('/').last
    val ew       = new EasyWriter ("simulation", s"$folder/comparison.txt")
    def emit (line: String): Unit = { println (line); ew.println (line) }

    blockHeader (s"PAIRED DIFFERENCES TO ${base.label} (model - ${base.label}), per replication",
                 context (corridor, "all models", base.rows.size),
                 "ReplicationRun.scala: writeComparison, from <model>_per_rep.csv of every model",
                 Seq (s"d_k = measure_k (model) - measure_k (${base.label}) for replication k, " +
                      "valid because replication k of every model uses the same random streams",
                      "verdict = 'differs' when the 95% interval of d excludes 0, else 'no difference'")).foreach (emit)
    emit ("%-8s%-26s%12s%12s%18s  verdict".format ("model", "measure", "mean", "sd", "95% half-width"))
    emit (DASH)
    for other <- results.tail; j <- 0 until 3 do                               // flow, speed, fitness
        val (name, dec, _) = MEASURES(j)
        val diff = new Statistic (s"${other.label} - ${base.label}: $name")
        for k <- base.rows.indices do diff.tally (other.rows(k)(j) - base.rows(k)(j))
        val (m, sd, h) = (diff.mean, diff.stdev, diff.interval ())
        val verdict = if m - h > 0.0 || m + h < 0.0 then "differs" else "no difference"
        emit ("%-8s%-26s".format (other.label, name) +
              s"%12.${dec}f%12.${dec}f%18.${dec}f".format (m, sd, h) + s"  $verdict")
    emit (RULE)
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
    private val validations = ArrayBuffer [ValidationResult] ()                // one full validation per replication
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
        validations += validation

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

        val corridor = folder.split ('/').last
        val summary  = new EasyWriter ("simulation", s"$folder/${label}_summary.txt")
        def emit (line: String): Unit = { println (line); summary.println (line) }
        blockHeader ("REPLICATION SUMMARY",
                     context (corridor, label, rows.size) + f"   (setup ${setupMs / 1000}%.1f s, run ${runMs / 1000}%.1f s)",
                     s"ReplicationRun.scala: ReplicationResults.write, from the rows of ${label}_per_rep.csv collected in ReplicationResults.add",
                     MEASURES.toSeq.map { case (name, _, formula) => "%-22s = %s".format (name, formula) }).foreach (emit)
        emit ("%-26s%16s%14s%18s".format ("measure", "mean", "sd", "95% half-width"))
        emit (DASH)
        for j <- MEASURES.indices do
            val (name, dec, _) = MEASURES(j)
            val stat = new Statistic (name)
            for row <- rows do stat.tally (row(j))
            emit ("%-26s".format (name) + s"%16.${dec}f%14.${dec}f%18.${dec}f".format (stat.mean, stat.stdev, stat.interval ()))
        emit (RULE)
        summary.close ()
        writeMetrics (folder)

        val n = rows.size.toDouble
        val nSensors = sumFlow.length
        val mean = new SimulationReport (sumFlow.map (_ / n), sumSpeed.map (_ / n), sumDensity.map (_ / n),
                                         demand, nSensors, sumFlow(0).dim2, sumFlow(0).dim)
        mean.exportCSV (s"$folder/${label}_mean_validation_data.csv")
    end write

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write every metric of the per-replication validation (station and lane
     *  level, flow and speed) as mean, sd and 95% t half-width over the
     *  replications: `<label>_metrics.csv` in long format (lane 0 = station) and
     *  `<label>_validation_report.txt` in four blocks (station flow, station
     *  speed, lane flow, lane speed), each headed by its source and formulas.
     *  @param folder  the output sub-folder of log/simulation/
     */
    private def writeMetrics (folder: String): Unit =
        val corridor = folder.split ('/').last
        def cells (m: MacroMetrics): Array [Double] =                           // METRICS order
            Array (m.r2, m.mae, m.rmse, m.seriesRmse, m.nrmse, m.smape, m.mean, m.simMean)

        // mean, sd and half-width of each metric over the replications
        def summarize (pick: ValidationResult => MacroMetrics): Array [SimulationMetricSummary] =
            val perRep = validations.map (v => cells (pick (v)))
            METRICS.indices.map { j =>
                val stat = new Statistic (METRICS(j)._1)
                for row <- perRep do stat.tally (row(j))
                SimulationMetricSummary.from (stat)
            }.toArray

        val head    = validations.head
        val nSta    = head.sensors.length
        val station = (for s <- 0 until nSta yield
                           (s, summarize (_.sensors(s).flowMetrics), summarize (_.sensors(s).speedMetrics))).toArray
        val lane    = (for s <- 0 until nSta; l <- head.sensors(s).flowMicro.indices yield
                           (s, l, summarize (_.sensors(s).flowMicro(l)), summarize (_.sensors(s).speedMicro(l)))).toArray

        // ---- the CSV: one row per station/lane, quantity and metric
        val csv = new EasyWriter ("simulation", s"$folder/${label}_metrics.csv")
        csv.println ("station,lane,quantity,metric,mean,sd,ci95")
        def csvRows (s: Int, l: Int, quantity: String, sums: Array [SimulationMetricSummary]): Unit =
            for j <- METRICS.indices do
                val m = sums(j)
                csv.println (s"$s,$l,$quantity,${METRICS(j)._1},${m.mean}," +
                             s"${m.standardDeviation.getOrElse (0.0)},${m.ci95.getOrElse (0.0)}")
        for (s, f, v) <- station do { csvRows (s + 1, 0, "flow", f); csvRows (s + 1, 0, "speed", v) }
        for (s, l, f, v) <- lane do { csvRows (s + 1, l + 1, "flow", f); csvRows (s + 1, l + 1, "speed", v) }
        csv.close ()

        // ---- the report: four blocks, one header each
        val report = new EasyWriter ("simulation", s"$folder/${label}_validation_report.txt")
        val units  = Map ("flow" -> "veh / 15 min", "speed" -> "m/s")
        def block (title: String, source: String, laneLevel: Boolean, quantity: String,
                   rows: Seq [(String, Array [SimulationMetricSummary])]): Unit =
            val formulas = METRICS.toSeq.map { case (key, name, _, formula) =>
                "%-12s = %s".format (name, if laneLevel then LANE_FORMULA.getOrElse (key, formula) else formula) }
            blockHeader (s"$title: $quantity (${units(quantity)})", context (corridor, label, validations.size),
                         source, formulas).foreach (report.println)
            report.println ((if laneLevel then "Station  Lane   " else "Station         ") +
                            METRICS.map (_._2.padTo (CELL_W, ' ')).mkString)
            report.println (DASH)
            for (first, sums) <- rows do
                report.println (first + sums.zipWithIndex.map { case (m, j) => cell (m, METRICS(j)._3) }.mkString)
            report.println (RULE); report.println ()
        end block

        val stationSrc = "SimulationReport.scala: SimulationReport.macroValidation, one call per station; " +
                         "summarised in ReplicationRun.scala: ReplicationResults.writeMetrics"
        val laneSrc    = "SimulationReport.scala: SimulationReport.microValidation (FitM.diagnose per lane column); " +
                         "summarised in ReplicationRun.scala: ReplicationResults.writeMetrics"
        val stationTitle = "STATION LEVEL (lane 0 = station total)"
        block (stationTitle, stationSrc, false, "flow",  station.toSeq.map { case (s, f, _) => ("%-16d".format (s + 1), f) })
        block (stationTitle, stationSrc, false, "speed", station.toSeq.map { case (s, _, v) => ("%-16d".format (s + 1), v) })
        block ("LANE LEVEL",  laneSrc,    true,  "flow",  lane.toSeq.map { case (s, l, f, _) => ("%-9d%-7d".format (s + 1, l + 1), f) })
        block ("LANE LEVEL",  laneSrc,    true,  "speed", lane.toSeq.map { case (s, l, _, v) => ("%-9d%-7d".format (s + 1, l + 1), v) })
        report.close ()
    end writeMetrics

end ReplicationResults
