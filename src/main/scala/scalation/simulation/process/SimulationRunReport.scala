//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishi
 *  @version 2.0
 *  @date    Sun Sep 13 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Generic JSON report for one completed simulation run.
 */

package scalation
package simulation
package process

import com.google.gson.{JsonArray, JsonObject}
import scalation.mathstat.MatrixD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SimulationLinkReport class stores measured results for one transport link.
 */
case class SimulationLinkReport (id: String, from: String, to: String, length: Double,
                                 completed: Int, flow: Double, travelTime: Double,
                                 speed: Double, density: Double, maximumOccupancy: Int,
                                 maximumDensity: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SimulationRecorderReport class stores one Junction Recorder snapshot.
 *  Counts and speeds are interval observations; density is the latest snapshot
 *  recorded in each interval.
 */
case class SimulationRecorderReport (component: String, rowTime: Double,
                                     counts: MatrixD, speeds: MatrixD, density: MatrixD)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SimulationRunReport class stores the measured results of one run.
 *  It is independent of the model type and can be emitted by any process model.
 */
case class SimulationRunReport (model: String, generatedVehicles: Int, junctions: Int,
                                transportSegments: Int, lanes: Int, completedVehicles: Int,
                                meanTravelTime: Double, simulationDuration: Double,
                                throughput: Double, agendaActivations: Long,
                                activationsPerCompletedVehicle: Double,
                                meanArrivalInterval: Double,
                                recorders: Array [SimulationRecorderReport],
                                links: Array [SimulationLinkReport]):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return this report as readable indented JSON for command-line output.
     */
    def prettyJson: String = SimulationRunReportJson.pretty (this)

end SimulationRunReport

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SimulationRunReportJson object encodes runtime reports as JSON.
 */
object SimulationRunReportJson:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode a runtime report as readable indented JSON.
     */
    def pretty (report: SimulationRunReport): String =
        val root = new JsonObject ()
        root.addProperty ("model", report.model)
        root.addProperty ("generatedVehicles", report.generatedVehicles)
        root.addProperty ("junctions", report.junctions)
        root.addProperty ("transportSegments", report.transportSegments)
        root.addProperty ("lanes", report.lanes)
        root.addProperty ("completedVehicles", report.completedVehicles)
        root.addProperty ("meanTravelTime", report.meanTravelTime)
        root.addProperty ("simulationDuration", report.simulationDuration)
        root.addProperty ("throughput", report.throughput)
        root.addProperty ("agendaActivations", report.agendaActivations)
        root.addProperty ("activationsPerCompletedVehicle", report.activationsPerCompletedVehicle)
        root.addProperty ("meanArrivalInterval", report.meanArrivalInterval)

        val semantics = new JsonObject ()
        semantics.addProperty ("linkAggregates", "whole-run traversal totals")
        semantics.addProperty ("detectors", "Junction Recorder interval observations")
        semantics.addProperty ("detectorDensity", "latest density snapshot in each interval")
        semantics.addProperty ("speed", "metres per second")
        semantics.addProperty ("agendaActivations", "actors dequeued by the shared Model scheduler")
        root.add ("measurementSemantics", semantics)

        val recorders = new JsonArray ()
        for recorder <- report.recorders do
            val value = new JsonObject ()
            value.addProperty ("component", recorder.component)
            value.addProperty ("rowTime", recorder.rowTime)
            value.add ("counts", encodeMatrix (recorder.counts))
            value.add ("speeds", encodeMatrix (recorder.speeds))
            value.add ("density", encodeMatrix (recorder.density))
            recorders.add (value)
        end for
        root.add ("recorders", recorders)

        val links = new JsonArray ()
        for link <- report.links do
            val value = new JsonObject ()
            value.addProperty ("id", link.id)
            value.addProperty ("from", link.from)
            value.addProperty ("to", link.to)
            value.addProperty ("length", link.length)
            value.addProperty ("completed", link.completed)
            value.addProperty ("flow", link.flow)
            value.addProperty ("travelTime", link.travelTime)
            value.addProperty ("speed", link.speed)
            value.addProperty ("density", link.density)
            value.addProperty ("maximumOccupancy", link.maximumOccupancy)
            value.addProperty ("maximumDensity", link.maximumDensity)
            links.add (value)
        end for
        root.add ("links", links)

        val gson = new com.google.gson.GsonBuilder ().setPrettyPrinting ().create ()
        gson.toJson (root)
    end pretty

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode a MatrixD without changing its row and column order.
     */
    private def encodeMatrix (matrix: MatrixD): JsonArray =
        val rows = new JsonArray ()
        cfor (0, matrix.dim) { i =>
            val row = new JsonArray ()
            cfor (0, matrix.dim2) { j => row.add (matrix(i, j)) }
            rows.add (row)
        }
        rows
    end encodeMatrix

end SimulationRunReportJson
