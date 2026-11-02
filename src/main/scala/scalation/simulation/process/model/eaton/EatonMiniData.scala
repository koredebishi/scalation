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
import scalation.simulation.process.builder.{NetEdge, NetworkBuilder}
import scalation.simulation.process.config.{AggregatedDemand, EatonCorridorConfig, PeMSDemand, StationRecord}
import scalation.simulation.process.RowTimeLoader

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** EatonMini PeMS data and station-binding owner.
 *  Sources: I-210 WB and EB mainline entries and every on-ramp; off-ramps give the
 *  share of cars leaving the freeway.  A ramp with no PeMS flow is FILLED with the
 *  mean of the ramps of its freeway and direction that have data (printed).
 */
class EatonMiniData (network: EatonMiniNetwork):

    val stations = EatonCorridorConfig.loadStationMap ()
    val stationEdges = EatonCorridorConfig.loadStationEdges (stations, network.edges)
    private val reviewed: Map [Int, NetEdge] = stationEdges.collect { case (s, Some (e), _) => s.stationId -> e }.toMap

    val demand    = AggregatedDemand.I210_WB_Baseline
    val demandEB  = AggregatedDemand.I210_EB_Baseline
    val demand134 = AggregatedDemand.SR134_WB_Baseline
    private def demandOf (s: StationRecord): AggregatedDemand =
        if s.freeway == 134 then demand134 else if s.direction == "E" then demandEB else demand
    val rowTimeSlice = demand.intervalMin * 60.0

    //--------------------------------------------------
    // Freeway geometry from the mainline stations: travel order and local heading
    private def mainlineOf (s: StationRecord): Array [StationRecord] =     // upstream first
        val ml = stations.filter (m => m.freeway == s.freeway && m.direction == s.direction && m.laneType == "ML")
                         .distinctBy (_.absPM)
        if s.direction == "E" then ml.sortBy (_.absPM) else ml.sortBy (-_.absPM)
    end mainlineOf

    private def downstream (s: StationRecord, m: StationRecord): Boolean =
        if s.direction == "E" then m.absPM > s.absPM else m.absPM < s.absPM

    def headingAt (s: StationRecord): Double =
        val ml = mainlineOf (s)
        val i  = ml.indexWhere (downstream (s, _))                         // first mainline station past s
        val (a, b) = if i < 0 then (ml(ml.length - 2), ml.last)
                     else if i == 0 then (ml(0), ml(1))
                     else (ml(i - 1), ml(i))
        NetworkBuilder.heading ((a.latitude, a.longitude), (b.latitude, b.longitude))
    end headingAt

    private def place (s: StationRecord): Option [(StationRecord, NetEdge)] =
        network.placeStation (s, headingAt (s), reviewed.get (s.stationId)).map (e => (s, e))

    //--------------------------------------------------
    // Mainline entries: I-210 WB (anchor 717653, its speeds feed every source) and I-210 EB
    val pems210 = PeMSDemand.I210_WB_Anchor ()
    val anchorSpeed = RowTimeLoader.getSpeedMatrixFromFile (
        pems210.dataDir + "/" + pems210.mainline.anchorFile, pems210.window, pems210.layout)
    val nLanes = anchorSpeed.dim2
    val (mainlineSources, _) = ArrivalSource.fromAggregated (demand, nLanes, 0)

    private val entryEB = stations.filter (s => s.freeway == 210 && s.direction == "E" && s.laneType == "ML")
                                  .minBy (_.absPM)
    println ("EatonMiniData: placing stations")
    val sourceEB: Long = place (entryEB).getOrElse (
        throw new IllegalArgumentException (s"I-210 EB entry station ${entryEB.stationId} has no edge"))._2.u
    val nLanesEB = entryEB.lanes
    val (mainlineSourcesEB, _) = ArrivalSource.fromAggregated (demandEB, nLanesEB, 0)

    val arrivalDistribution = demand.distribution
    val splitRatios = AggregatedArrivalSource.computeSplitRatios (
        AggregatedDemand.I210_WB_Baseline, 717634,
        AggregatedDemand.SR134_WB_Baseline, 717603)

    //--------------------------------------------------
    // Fill a station with no data: the mean series of the data-backed stations of the
    // same freeway and direction (else the same direction on any freeway)
    private def meanSeries (series: Seq [Array [Double]]): Array [Double] =
        if series.isEmpty then Array.empty
        else Array.tabulate (series.map (_.length).min) (t => series.map (_(t)).sum / series.length)

    private def filled (placed: Array [(StationRecord, NetEdge)], series: Array [Array [Double]]): Array [Array [Double]] =
        val real = placed.indices.filter (j => series(j).nonEmpty && series(j).sum > 0.0)
        Array.tabulate (placed.length) { i =>
            if real.contains (i) then series(i)
            else
                val s    = placed(i)._1
                val same = real.filter (j => placed(j)._1.freeway == s.freeway && placed(j)._1.direction == s.direction)
                val pool = if same.nonEmpty then same else real.filter (j => placed(j)._1.direction == s.direction)
                meanSeries (pool.map (series(_)))
        }
    end filled

    //--------------------------------------------------
    // On-ramps: every OR station placed on a ramp edge; each is one source
    private val onRamps = stations.filter (_.laneType == "OR").flatMap (place)
    private val onRampFlow = onRamps.map { (s, _) =>
        val (_, mu) = AggregatedRampArrivalSource.loadRampByStation (demandOf (s), s.stationId, rowTimeSlice)
        mu.map (m => if m == Double.MaxValue then 0.0 else rowTimeSlice / m)   // veh per interval
    }
    val rampFilled   = onRampFlow.map (f => f.isEmpty || f.sum <= 0.0)
    private val rampFlows = filled (onRamps, onRampFlow)
    val rampStations = onRamps.map (_._1)
    val rampBindings = onRamps.map (_._2)
    val rampTotals   = rampFlows.map (_.sum.toInt)
    val rampMu       = rampFlows.map (_.map (f => if f > 0.0 then rowTimeSlice / f else Double.MaxValue))

    //--------------------------------------------------
    // Off-ramps: share leaving = FR flow / flow at the nearest mainline station upstream
    private val offRamps = stations.filter (_.laneType == "FR").flatMap (place)
    private val offRampShareData = offRamps.map { (s, _) =>
        val d  = demandOf (s)
        val fr = AggregatedArrivalSource.extractStationFlow (d, s.stationId, d.frFile.get, "FR")
        val ml = mainlineOf (s)
        val up = ml.filterNot (downstream (s, _)).lastOption.getOrElse (ml(0))     // nearest upstream mainline station
        val mf = AggregatedArrivalSource.extractStationFlow (d, up.stationId)
        if fr.sum <= 0.0 || mf.isEmpty then Array.empty [Double]
        else Array.tabulate (math.min (fr.length, mf.length)) (t => if mf(t) > 0.0 then math.min (1.0, fr(t) / mf(t)) else 0.0)
    }
    val offRampFilled   = offRampShareData.map (_.isEmpty)
    val offRampShares   = filled (offRamps, offRampShareData)
    val offRampStations = offRamps.map (_._1)
    val offRampBindings = offRamps.map (_._2)

    println (s"EatonMiniData: ${rampStations.length} on-ramps (${rampFilled.count (identity)} filled), " +
             s"${offRampStations.length} off-ramps (${offRampFilled.count (identity)} filled)")
    for i <- rampStations.indices if rampFilled(i) do
        println (s"  FILLED on-ramp  ${rampStations(i).stationId} ${rampStations(i).location}")
    for i <- offRampStations.indices if offRampFilled(i) do
        println (s"  FILLED off-ramp ${offRampStations(i).stationId} ${offRampStations(i).location}")

    //--------------------------------------------------
    // Source order (subtypes): WB lanes, then EB lanes, then on-ramps
    val generatedVehicles = mainlineSources.map (_.getTotalVehicles (0)).sum +
                            mainlineSourcesEB.map (_.getTotalVehicles (0)).sum
    val totalGeneratedVehicles = generatedVehicles + rampTotals.sum
    val nRows = math.min ((rampMu.map (_.length) :+ 72).max, anchorSpeed.dim)

    def getMuForSource (sourceIdx: Int): VectorD =
        val eb   = sourceIdx - nLanes
        val ramp = eb - nLanesEB
        if sourceIdx < nLanes then
            RowTimeLoader.getMuForSourceDefault (mainlineSources, Array.empty [ArrivalSource], nLanes, nRows, sourceIdx)
        else if eb < nLanesEB then
            RowTimeLoader.getMuForSourceDefault (mainlineSourcesEB, Array.empty [ArrivalSource], nLanesEB, nRows, eb)
        else if ramp < rampMu.length then VectorD (rampMu(ramp))
        else VectorD.fill (nRows) (Double.MaxValue)
    end getMuForSource

    def getSpeedMatrix: MatrixD = anchorSpeed
end EatonMiniData
