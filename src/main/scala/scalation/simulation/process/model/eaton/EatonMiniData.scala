//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** EatonMini data-intake ownership.
 *
 *  This file will contain only Eaton data preparation: PeMS station records,
 *  reviewed station-to-edge bindings, arrival data, and measured observations.
 *  It must reuse the existing loaders and must not recreate network topology.
 */

package scalation
package simulation
package process
package model
package eaton

import scalation.mathstat.{MatrixD, VectorD}
import scalation.simulation.process.arrival.{AggregatedArrivalSource, AggregatedRampArrivalSource, ArrivalSource}
import scalation.simulation.process.builder.NetEdge
import scalation.simulation.process.config.{AggregatedDemand, EatonCorridorConfig, PeMSDemand}
import scalation.simulation.process.RowTimeLoader

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** EatonMini PeMS data and station-binding owner. */
class EatonMiniData (edges: Array [NetEdge]):

    val stations = EatonCorridorConfig.loadStationMap ()
    val stationEdges = EatonCorridorConfig.loadStationEdges (stations, edges)

    val demand = AggregatedDemand.I210_WB_Baseline
    val pems210 = PeMSDemand.I210_WB_Anchor ()
    val anchorSpeed = RowTimeLoader.getSpeedMatrixFromFile (
        pems210.dataDir + "/" + pems210.mainline.anchorFile, pems210.window, pems210.layout)
    val nLanes = anchorSpeed.dim2
    val (mainlineSources, _) = ArrivalSource.fromAggregated (demand, nLanes, 0)
    val generatedVehicles = mainlineSources.map (_.getTotalVehicles (0)).sum
    val arrivalDistribution = demand.distribution
    val splitRatios = AggregatedArrivalSource.computeSplitRatios (
        AggregatedDemand.I210_WB_Baseline, 717634,
        AggregatedDemand.SR134_WB_Baseline, 717603)

    val rampBindings = EatonMiniConfig.rampStationIds.map { stationId =>
        stationEdges.find (_._1.stationId == stationId).flatMap (_._2) match
            case Some (edge) => edge
            case None => throw new IllegalArgumentException (s"missing mapped ramp station $stationId")
    }
    val rampDemand = EatonMiniConfig.rampStationIds.map { stationId =>
        AggregatedRampArrivalSource.loadRampByStation (
            demand, stationId, demand.intervalMin * 60.0)
    }
    val rampTotals = rampDemand.map (_._1)
    val rampMu = rampDemand.map (_._2)
    val totalGeneratedVehicles = generatedVehicles + rampTotals.sum
    val nRows = math.min ((rampMu.map (_.length) :+ 72).max, anchorSpeed.dim)
    val rowTimeSlice = demand.intervalMin * 60.0

    def getMuForSource (sourceIdx: Int): VectorD =
        if sourceIdx < nLanes then
            RowTimeLoader.getMuForSourceDefault (mainlineSources, Array.empty [ArrivalSource], nLanes, nRows, sourceIdx)
        else if sourceIdx - nLanes < rampMu.length then VectorD (rampMu(sourceIdx - nLanes))
        else VectorD.fill (nRows) (Double.MaxValue)
    end getMuForSource

    def getSpeedMatrix: MatrixD = anchorSpeed
end EatonMiniData
