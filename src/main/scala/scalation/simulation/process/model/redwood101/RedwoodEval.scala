//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Oct 25 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Evaluation runs for the Redwood Creek models (micro, mixed, full meso).
 *           Same steps and exports as `evalRun` in CalibrationFramework.
 */

package scalation
package simulation
package process
package model
package redwood101

import scalation.{banner, gauge}
import scalation.simulation.process.config.DynamicsConfig
import scalation.simulation.process.model.calroute101.{CalRouteReadings, SimulationReport}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Evaluation runs, written to log/simulation/redwood/{micro, mixed, meso}.
 *  > runMain scalation.simulation.process.model.redwood101.evalRedwood
 *  > runMain scalation.simulation.process.model.redwood101.evalRedwoodWithMeso
 *  > runMain scalation.simulation.process.model.redwood101.evalRedwoodFullMeso
 */
@main def evalRedwood (): Unit =
    evalRedwoodRun ("RedwoodCreek101", "redwood/micro", () => new RedwoodCreek101 (animating = false))

@main def evalRedwoodWithMeso (): Unit =
    evalRedwoodRun ("RedwoodCreek101_withMeso", "redwood/mixed", () => new RedwoodCreek101_withMeso (animating = false))

@main def evalRedwoodFullMeso (): Unit =
    evalRedwoodRun ("RedwoodCreek101_fullMeso", "redwood/meso", () => new RedwoodCreek101_fullMeso (animating = false))

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Build, run, report and export one Redwood model.
 *  @param label     the model name for the banner
 *  @param folder    the existing sub-folder of log/simulation/ to write into
 *  @param makeModel builds the model (construction is timed as setup)
 */
private def evalRedwoodRun (label: String, folder: String, makeModel: () => Model & CalRouteReadings): Unit =
    banner (s"EVALUATION RUN - $label (Unoptimized)")

    val dynamicsConfig = DynamicsConfig.idmDefault
    val defaultParams  = dynamicsConfig.cfParams.toVector
    println (s"Applying parameters: $defaultParams")
    Vehicle.setProps (Vehicle.setParams (defaultParams))

    var model: Model & CalRouteReadings = null
    val setupMs = gauge { model = makeModel () }                            // model construction

    println ("Running simulation...")
    val runMs = gauge { model.simulate (); model.waitFinished () }          // simulate + waitFinished

    val wallTime = f"setup ${setupMs / 1000}%.1f s   run ${runMs / 1000}%.1f s   total ${(setupMs + runMs) / 1000}%.1f s"
    println (s"[wall] $wallTime")

    val report = SimulationReport.fromJunctions (model.getJunctions, model.getPemsSensorIndices, model.getDemandConfig)
    println (s"Fitness value: ${report.fitness ()}")
    report.report ()
    val runPrefix = SimulationReport.exportRun (model, dynamicsConfig.toString, defaultParams, wallTime,
                                                Some (report.validate ()), folder)
    report.exportCSV (s"${runPrefix}_validation_data.csv")
    report.exportReport (s"${runPrefix}_validation_report.txt")

    Model.shutdown ()
end evalRedwoodRun
