//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Arrival sources for PeMS-driven simulations. */
package scalation
package simulation
package process
package arrival

import scalation.random.Variate
import scalation.simulation.process.config.{PeMSArrivals, PeMSDemand, PeMSDataHelper}

private val flaw = flawf ("ArrivalSource")

/** Interface for any arrival source (mainline lane or ramp). */
trait ArrivalSource:
    def getTotalVehicles(laneIdx: Int): Int
    def getMu(laneIdx: Int, timeIdx: Int): Double
    def getDistribution: Variate
    def isPerLane: Boolean
    def numLanes: Int
end ArrivalSource

/** Mainline arrival source backed by PeMS data via PeMSDataHelper.
 *  @param arrivals   PeMS arrival specification
 *  @param demand     PeMS demand configuration
 *  @param laneIdx    which lane this source serves (0-based)
 *  @param nLanes     total number of lanes
 *  @param rowTime    seconds per time bin (e.g., 900.0 for 15-min)
 */
class PeMSArrivalSource(arrivals: PeMSArrivals, demand: PeMSDemand,
                        laneIdx: Int, nLanes: Int, rowTime: Double = 900.0)
      extends ArrivalSource:

    private val debug = debugf ("PeMSArrivalSource", false)

    // Load flow data for mainline anchor sensor
    private val (flowMatrix, _) = PeMSDataHelper.loadMainlineSensor(demand, 0)  // sensor 0 = anchor
    
    // Extract lane totals: sum of flow for this lane across all time rows
    private val laneTotals: Array[Int] = Array.tabulate(nLanes) { lane =>
        flowMatrix(?, lane).sum.toInt
    }
    
    // Compute mu (inter-arrival time) for this lane per time row
    // mu = rowTime / count (if count > 0, else MaxValue)
    private val muArray: Array[Double] = Array.tabulate(flowMatrix.dim) { row =>
        val count = flowMatrix(row, laneIdx)
        if count > 0.0 then rowTime / count else Double.MaxValue
    }

    debug ("init", s"lane=$laneIdx, total=${laneTotals(laneIdx)}, mu(0)=${muArray(0)}%.2f")

    def getTotalVehicles(idx: Int): Int =
        if arrivals.perLane && laneIdx < laneTotals.length then laneTotals(laneIdx)
        else laneTotals.sum / nLanes

    def getMu(idx: Int, timeIdx: Int): Double =
        if timeIdx < muArray.length then muArray(timeIdx) else muArray.last

    def getDistribution: Variate = arrivals.distribution
    def isPerLane: Boolean = arrivals.perLane
    def numLanes: Int = 1
end PeMSArrivalSource

/** Ramp arrival source backed by PeMS data via PeMSDataHelper.
 *  @param arrivals   PeMS arrival specification
 *  @param demand     PeMS demand configuration
 *  @param rampIdx    which ramp this source serves (0-based)
 *  @param rowTime    seconds per time bin (e.g., 900.0 for 15-min)
 */
class PeMSRampArrivalSource(arrivals: PeMSArrivals, demand: PeMSDemand,
                            rampIdx: Int, rowTime: Double = 900.0)
      extends ArrivalSource:

    private val debug = debugf ("PeMSRampArrivalSource", false)

    // Load flow data for this ramp
    private val flowMatrix = PeMSDataHelper.loadRampSensor(demand, rampIdx)
    
    // Total vehicles: sum of flow across all time rows
    private val rampTotal: Int = flowMatrix.sumVr.sum.toInt
    
    // Compute mu (inter-arrival time) per time row
    // For ramps, use column 0 (single lane typically)
    private val muArray: Array[Double] = Array.tabulate(flowMatrix.dim) { row =>
        val count = flowMatrix(row, 0)
        if count > 0.0 then rowTime / count else Double.MaxValue
    }

    debug ("init", s"ramp=$rampIdx, total=$rampTotal, mu(0)=${muArray(0)}%.2f")

    def getTotalVehicles(idx: Int): Int = rampTotal
    
    def getMu(idx: Int, timeIdx: Int): Double =
        if timeIdx < muArray.length then muArray(timeIdx) else muArray.last

    def getDistribution: Variate = arrivals.distribution
    def isPerLane: Boolean = false
    def numLanes: Int = 1
end PeMSRampArrivalSource

/** Factory for constructing arrival sources given demand + network meta. */
object ArrivalSource:

    private val RowTime = 900.0 // 15-minute bins

    /** Create mainline arrival sources (one per lane). */
    private def mainlineSources(demand: PeMSDemand, nLanes: Int): Array[ArrivalSource] =
        Array.tabulate(nLanes) { lane =>
            new PeMSArrivalSource(demand.mainline, demand, lane, nLanes, RowTime)
        }

    /** Create ramp arrival sources (one per ramp). */
    private def rampSources(demand: PeMSDemand): Array[ArrivalSource] =
        demand.ramps.zipWithIndex.map { case (arr, idx) =>
            new PeMSRampArrivalSource(arr, demand, idx, RowTime)
        }.toArray

    /** Create both mainline and ramp arrival sources.
     *  @param demand  PeMS demand configuration
     *  @param nLanes  number of mainline lanes (default 4)
     *  @return tuple of (mainline sources, ramp sources)
     */
    def allSources(demand: PeMSDemand, nLanes: Int = 4): (Array[ArrivalSource], Array[ArrivalSource]) =
        (mainlineSources(demand, nLanes), rampSources(demand))
        
end ArrivalSource
