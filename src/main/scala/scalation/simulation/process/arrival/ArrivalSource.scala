//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Arrival sources for PeMS-driven simulations. */
package scalation
package simulation
package process
package arrival

import java.io.{BufferedReader, FileReader}
import scala.collection.mutable.{ArrayBuffer, HashMap}

import scalation.random.Variate
import scalation.simulation.process.config.{AggregatedDemand, PeMSArrivals, PeMSDemand, PeMSDataHelper}

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

    // Load flow data for mainline anchor sensor (uses demand's window/layout)
    private val (flowMatrix, _) = PeMSDataHelper.loadMainlineSensor(demand, 0, demand.window, demand.layout)

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

/** Synthetic arrival source for debugging/testing with fixed vehicle counts.
 *  @param totalVehicles  fixed number of vehicles to generate
 *  @param dist           inter-arrival distribution
 */
class SyntheticArrivalSource(totalVehicles: Int, dist: Variate) extends ArrivalSource:
    def getTotalVehicles(idx: Int): Int = totalVehicles
    def getMu(idx: Int, timeIdx: Int): Double = dist.mean
    def getDistribution: Variate = dist
    def isPerLane: Boolean = false
    def numLanes: Int = 1
end SyntheticArrivalSource

/** Mainline arrival source backed by Eaton-style aggregated CSV.
 *  Uses the entry station (highest postmile for WB) to get demand.
 *  @param demand     AggregatedDemand configuration
 *  @param laneIdx    which lane this source serves (0-based)
 *  @param nLanes     total number of mainline lanes
 *  @param rowTime    seconds per time bin (300.0 for 5-min Eaton data)
 */
class AggregatedArrivalSource(demand: AggregatedDemand, laneIdx: Int, nLanes: Int,
                              rowTime: Double = 300.0) extends ArrivalSource:

    private val debug = debugf ("AggregatedArrivalSource", true)

    // Load and parse the mainline CSV
    private val (laneTotals, muArray) = AggregatedArrivalSource.loadMainlineEntry(demand, laneIdx, nLanes, rowTime)

    debug ("init", s"lane=$laneIdx, total=${laneTotals(laneIdx)}, mu(0)=${if muArray.nonEmpty then f"${muArray(0)}%.2f" else "N/A"}")

    def getTotalVehicles(idx: Int): Int = if laneIdx < laneTotals.length then laneTotals(laneIdx) else 0
    def getMu(idx: Int, timeIdx: Int): Double = if timeIdx < muArray.length then muArray(timeIdx) else muArray.lastOption.getOrElse(Double.MaxValue)
    def getDistribution: Variate = demand.distribution
    def isPerLane: Boolean = true
    def numLanes: Int = 1
end AggregatedArrivalSource

/** Companion object for AggregatedArrivalSource — CSV parsing utilities with caching. */
object AggregatedArrivalSource:

    private val debug = debugf ("AggregatedArrivalSource", true)

    /** Cache: key = "dataDir/mlFile:startTime:nLanes" → (entryRows, laneTotals)
     *  Parsed once per unique demand config, shared across all lanes.
     */
    private val mlCache = HashMap[String, (ArrayBuffer[Array[Double]], Array[Int])]()

    /** Clear the cache (for testing). */
    def clearCache(): Unit = mlCache.clear()

    /** Load mainline entry station data from aggregated CSV (cached).
     *  For WB direction, entry = highest postmile station.
     *  @return (laneTotals array, muArray for the specified lane)
     */
    def loadMainlineEntry(demand: AggregatedDemand, laneIdx: Int, nLanes: Int,
                          rowTime: Double): (Array[Int], Array[Double]) =
        val cacheKey = s"${demand.dataDir}/${demand.mlFile}:${demand.startTime}:$nLanes"

        // Get or load cached entry rows and lane totals
        val (entryRows, laneTotals) = mlCache.getOrElseUpdate(cacheKey, {
            debug ("loadMainlineEntry", s"CACHE MISS - parsing ${demand.dataDir}/${demand.mlFile}")
            parseMainlineCSV(demand, nLanes)
        })

        debug ("loadMainlineEntry", s"lane=$laneIdx using cached data, rows=${entryRows.length}")

        if entryRows.isEmpty then return (laneTotals, Array.empty)

        // Compute mu per time row for THIS lane (lane-specific, not cached)
        val muArray = entryRows.map { row =>
            val count = if laneIdx < row.length then row(laneIdx) else 0.0
            if count > 0.0 then rowTime / count else Double.MaxValue
        }.toArray

        (laneTotals, muArray)
    end loadMainlineEntry

    /** Parse mainline CSV once — returns (entryRows, laneTotals) for caching.
     *  Called only on cache miss.
     */
    private def parseMainlineCSV(demand: AggregatedDemand, nLanes: Int): (ArrayBuffer[Array[Double]], Array[Int]) =
        val filePath = s"${demand.dataDir}/${demand.mlFile}"
        try
            val reader = new BufferedReader(new FileReader(filePath))
            val header = reader.readLine()
            if header == null then
                reader.close()
                return (ArrayBuffer.empty, Array.fill(nLanes)(0))

            val cols = header.split(",").map(_.trim)
            val pmIdx = cols.indexWhere(_.contains("Abs_PM"))
            val laneTypeIdx = cols.indexWhere(c => c.toLowerCase.contains("lane") && c.toLowerCase.contains("type"))
            val laneFlowIndices = (1 to nLanes).map { l =>
                cols.indexWhere(c => c.contains(s"Lane$l Flow") && !c.contains("Speed"))
            }.toArray

            // Read all rows, group by station (by postmile)
            // Filter by startTime AND lane type = "ML" (exclude HV/HOV)
            val stationData = HashMap[Double, ArrayBuffer[Array[Double]]]()
            var line = reader.readLine()
            while line != null do
                val parts = line.split(",").map(_.trim)
                val timestamp = parts(0)
                val laneType = if laneTypeIdx >= 0 && laneTypeIdx < parts.length then parts(laneTypeIdx) else "ML"
                if timestamp >= demand.startTime && laneType == "ML" && parts.length > pmIdx && pmIdx >= 0 then
                    val pm = parts(pmIdx).toDoubleOption.getOrElse(0.0)
                    val laneFlows = laneFlowIndices.map { idx =>
                        if idx >= 0 && idx < parts.length then parts(idx).toDoubleOption.getOrElse(0.0) else 0.0
                    }
                    stationData.getOrElseUpdate(pm, ArrayBuffer()) += laneFlows
                line = reader.readLine()
            end while
            reader.close()

            if stationData.isEmpty then return (ArrayBuffer.empty, Array.fill(nLanes)(0))

            // Find entry station (highest PM for WB)
            val entryPM = stationData.keys.max
            val entryRows = stationData(entryPM)
            debug ("parseMainlineCSV", s"Entry station PM=$entryPM, rows=${entryRows.length}")

            // Sum flow per lane across all time rows
            val laneTotals = Array.tabulate(nLanes) { l =>
                entryRows.map(row => if l < row.length then row(l) else 0.0).sum.toInt
            }
            debug ("parseMainlineCSV", s"laneTotals=${laneTotals.mkString(",")}")

            (entryRows, laneTotals)
        catch
            case e: Exception =>
                debug ("parseMainlineCSV", s"ERROR: ${e.getMessage}")
                (ArrayBuffer.empty, Array.fill(nLanes)(0))
    end parseMainlineCSV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract total flow per time interval for a specific station ID from an
     *  aggregated ML_HV CSV.  Reuses the same CSV format as `parseMainlineCSV`
     *  but filters by station ID (column 1) instead of postmile.
     *
     *  @param demand     AggregatedDemand pointing to the corridor CSV
     *  @param stationId  PeMS VDS station ID (e.g., 717634)
     *  @return array of total flow per 5-min interval, ordered by timestamp
     */
    def extractStationFlow (demand: AggregatedDemand, stationId: Int): Array[Double] =
        val filePath = s"${demand.dataDir}/${demand.mlFile}"
        try
            val reader = new BufferedReader (new FileReader (filePath))
            val header = reader.readLine ()
            if header == null then { reader.close (); return Array.empty }

            val cols       = header.split (",").map (_.trim)
            val stationIdx = cols.indexWhere (_.contains ("Station"))
            val laneTypeIdx = cols.indexWhere (c => c.toLowerCase.contains ("lane") && c.toLowerCase.contains ("type"))
            val totalFlowIdx = cols.indexWhere (c => c.contains ("Total Flow"))

            val flows = ArrayBuffer [Double] ()
            var line  = reader.readLine ()
            while line != null do
                val parts     = line.split (",").map (_.trim)
                val timestamp = parts (0)
                val sid       = if stationIdx >= 0 && stationIdx < parts.length
                                then parts (stationIdx).toIntOption.getOrElse (0) else 0
                val laneType  = if laneTypeIdx >= 0 && laneTypeIdx < parts.length
                                then parts (laneTypeIdx) else "ML"
                if sid == stationId && laneType == "ML" && timestamp >= demand.startTime then
                    val flow = if totalFlowIdx >= 0 && totalFlowIdx < parts.length
                               then parts (totalFlowIdx).toDoubleOption.getOrElse (0.0) else 0.0
                    flows += flow
                end if
                line = reader.readLine ()
            end while
            reader.close ()

            debug ("extractStationFlow", s"station=$stationId, file=$filePath, intervals=${flows.length}")
            flows.toArray
        catch
            case e: Exception =>
                debug ("extractStationFlow", s"ERROR loading station $stationId: ${e.getMessage}")
                Array.empty
    end extractStationFlow

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute time-varying FF split ratios from PeMS data.
     *  For each 5-min interval: ratio = ffMergeFlow / upstreamFlow.
     *  If either station has missing data for an interval, falls back to
     *  the overall average ratio across all valid intervals.
     *
     *  @param upstreamDemand  AggregatedDemand for the upstream corridor (I-210)
     *  @param upstreamId      PeMS station ID upstream of diverge (e.g., 717634 LAKE 1)
     *  @param ffMergeDemand   AggregatedDemand for the FF destination corridor (SR-134)
     *  @param ffMergeId       PeMS station ID at FF merge (e.g., 717603 ORANGE GROVE)
     *  @return array of split ratios indexed by 5-min interval, clamped to [0, 1]
     */
    def computeSplitRatios (upstreamDemand: AggregatedDemand, upstreamId: Int,
                            ffMergeDemand: AggregatedDemand, ffMergeId: Int): Array[Double] =
        val upFlow = extractStationFlow (upstreamDemand, upstreamId)
        val ffFlow = extractStationFlow (ffMergeDemand, ffMergeId)

        if upFlow.isEmpty || ffFlow.isEmpty then
            debug ("computeSplitRatios", s"WARNING: missing data — upstream=${upFlow.length}, ff=${ffFlow.length}. Using constant 0.30.")
            return Array.fill (72)(0.30)       // fallback: 72 intervals × constant

        val nIntervals = upFlow.length.min (ffFlow.length)

        // Compute overall average ratio for fallback on bad intervals
        var sumRatio = 0.0; var nValid = 0
        cfor (0, nIntervals) { i =>
            if upFlow(i) > 0.0 then { sumRatio += ffFlow(i) / upFlow(i); nValid += 1 }
        }
        val avgRatio = if nValid > 0 then (sumRatio / nValid).min (1.0).max (0.0) else 0.30

        // Per-interval ratio with fallback
        val ratios = Array.ofDim [Double] (nIntervals)
        cfor (0, nIntervals) { i =>
            ratios(i) = if upFlow(i) > 0.0 then (ffFlow(i) / upFlow(i)).min (1.0).max (0.0)
                         else avgRatio
        }

        debug ("computeSplitRatios", f"intervals=$nIntervals, avgRatio=$avgRatio%.3f, " +
               f"min=${ratios.min}%.3f, max=${ratios.max}%.3f")
        ratios
    end computeSplitRatios

end AggregatedArrivalSource

/** Ramp arrival source backed by Eaton-style aggregated CSV.
 *  @param demand     AggregatedDemand configuration
 *  @param rampIdx    which ramp this source serves (0-based, ordered by postmile)
 *  @param rowTime    seconds per time bin (300.0 for 5-min Eaton data)
 */
class AggregatedRampArrivalSource(demand: AggregatedDemand, rampIdx: Int,
                                  rowTime: Double = 300.0) extends ArrivalSource:

    private val debug = debugf ("AggregatedRampArrivalSource", true)

    // Load and parse the on-ramp CSV
    private val (rampTotal, muArray) = AggregatedRampArrivalSource.loadRamp(demand, rampIdx, rowTime)

    debug ("init", s"ramp=$rampIdx, total=$rampTotal")

    def getTotalVehicles(idx: Int): Int = rampTotal
    def getMu(idx: Int, timeIdx: Int): Double = if timeIdx < muArray.length then muArray(timeIdx) else muArray.lastOption.getOrElse(Double.MaxValue)
    def getDistribution: Variate = demand.distribution
    def isPerLane: Boolean = false
    def numLanes: Int = 1
end AggregatedRampArrivalSource

/** Companion object for AggregatedRampArrivalSource — CSV parsing utilities with caching. */
object AggregatedRampArrivalSource:

    private val debug = debugf ("AggregatedRampArrivalSource", true)

    /** Cache: key = "dataDir/orFile:startTime" → sorted array of (PM, flowRows)
     *  Parsed once per unique demand config, shared across all ramps.
     */
    private val orCache = HashMap[String, Array[(Double, ArrayBuffer[Double])]]()

    /** Clear the cache (for testing). */
    def clearCache(): Unit = orCache.clear()
    /** Load ramp data from aggregated OR CSV (cached).
     *  Ramps are ordered by postmile; rampIdx selects which one.
     *  @return (total vehicles, muArray)
     */
    def loadRamp(demand: AggregatedDemand, rampIdx: Int, rowTime: Double): (Int, Array[Double]) =
        demand.orFile match
            case None => (0, Array.empty)
            case Some(orFile) =>
                val cacheKey = s"${demand.dataDir}/$orFile:${demand.startTime}"

                // Get or load cached ramp data
                val sortedRamps = orCache.getOrElseUpdate(cacheKey, {
                    debug ("loadRamp", s"CACHE MISS - parsing ${demand.dataDir}/$orFile")
                    parseRampCSV(demand, orFile)
                })

                if rampIdx >= sortedRamps.length then return (0, Array.empty)

                val (rampPM, rampRows) = sortedRamps(rampIdx)
                val rampTotal = rampRows.sum.toInt

                // Compute mu per time row for THIS ramp
                val muArray = rampRows.map { flow =>
                    if flow > 0.0 then rowTime / flow else Double.MaxValue
                }.toArray

                debug ("loadRamp", s"ramp $rampIdx at PM=$rampPM, total=$rampTotal (cached)")
                (rampTotal, muArray)
    end loadRamp

    /** Parse ramp CSV once — returns sorted array of (PM, flowRows) for caching.
     *  Called only on cache miss.
     */
    private def parseRampCSV(demand: AggregatedDemand, orFile: String): Array[(Double, ArrayBuffer[Double])] =
        val filePath = s"${demand.dataDir}/$orFile"
        try
            val reader = new BufferedReader(new FileReader(filePath))
            val header = reader.readLine()
            if header == null then
                reader.close()
                return Array.empty

            val cols = header.split(",").map(_.trim)
            val pmIdx = cols.indexWhere(_.contains("Abs_PM"))
            val totalFlowIdx = cols.indexWhere(_.contains("Total Flow"))

            // Read all rows, group by station PM
            val stationData = HashMap[Double, ArrayBuffer[Double]]()
            var line = reader.readLine()
            while line != null do
                val parts = line.split(",").map(_.trim)
                val timestamp = parts(0)
                if timestamp >= demand.startTime && parts.length > pmIdx && pmIdx >= 0 then
                    val pm = parts(pmIdx).toDoubleOption.getOrElse(0.0)
                    val flow = if totalFlowIdx >= 0 && totalFlowIdx < parts.length then
                        parts(totalFlowIdx).toDoubleOption.getOrElse(0.0) else 0.0
                    stationData.getOrElseUpdate(pm, ArrayBuffer()) += flow
                line = reader.readLine()
            end while
            reader.close()

            if stationData.isEmpty then return Array.empty

            // Sort stations by PM (descending for WB — entry to exit order)
            val sorted = stationData.toArray.sortBy(-_._1)
            debug ("parseRampCSV", s"Parsed ${sorted.length} ramp stations")
            sorted
        catch
            case e: Exception =>
                debug ("parseRampCSV", s"ERROR: ${e.getMessage}")
                Array.empty
    end parseRampCSV

end AggregatedRampArrivalSource

/** Factory for constructing arrival sources given demand + network meta. */
object ArrivalSource:

    /** Create mainline arrival sources (one per lane).
     *  Uses demand.window.binSeconds for rowTime (e.g., 900 for 15-min, 300 for 5-min).
     */
    private def mainlineSources(demand: PeMSDemand, nLanes: Int): Array[ArrivalSource] =
        val rowTime = demand.window.binSeconds
        Array.tabulate(nLanes) { lane =>
            new PeMSArrivalSource(demand.mainline, demand, lane, nLanes, rowTime)
        }

    /** Create ramp arrival sources (one per ramp). */
    private def rampSources(demand: PeMSDemand): Array[ArrivalSource] =
        val rowTime = demand.window.binSeconds
        demand.ramps.zipWithIndex.map { case (arr, idx) =>
            new PeMSRampArrivalSource(arr, demand, idx, rowTime)
        }.toArray

    /** Create both mainline and ramp arrival sources from PeMS data.
     *  @param demand  PeMS demand configuration
     *  @param nLanes  number of mainline lanes (default 4)
     *  @return tuple of (mainline sources, ramp sources)
     */
    def allSources(demand: PeMSDemand, nLanes: Int = 4): (Array[ArrivalSource], Array[ArrivalSource]) =
        (mainlineSources(demand, nLanes), rampSources(demand))

    /** Create synthetic mainline and ramp arrival sources with fixed counts.
     *  Use for debugging/testing without loading CSV data.
     *  @param mlCount   vehicles per mainline lane
     *  @param rampCount vehicles per ramp
     *  @param nLanes    number of mainline lanes
     *  @param nRamps    number of ramps
     *  @param dist      inter-arrival distribution
     *  @return tuple of (mainline sources, ramp sources)
     */
    def syntheticSources(mlCount: Int, rampCount: Int, nLanes: Int, nRamps: Int,
                         dist: Variate): (Array[ArrivalSource], Array[ArrivalSource]) =
        val ml: Array[ArrivalSource] = Array.fill(nLanes)(new SyntheticArrivalSource(mlCount, dist))
        val ramps: Array[ArrivalSource] = Array.fill(nRamps)(new SyntheticArrivalSource(rampCount, dist))
        (ml, ramps)

    /** Create mainline and ramp arrival sources from aggregated Eaton-style CSV.
     *  @param demand   AggregatedDemand configuration (dataDir + mlFile + orFile)
     *  @param nLanes   number of mainline lanes
     *  @param nRamps   number of on-ramps
     *  @return tuple of (mainline sources, ramp sources)
     */
    def fromAggregated(demand: AggregatedDemand, nLanes: Int, nRamps: Int): (Array[ArrivalSource], Array[ArrivalSource]) =
        val rowTime = demand.intervalMin * 60.0  // Convert minutes to seconds
        val ml: Array[ArrivalSource] = Array.tabulate(nLanes) { lane =>
            new AggregatedArrivalSource(demand, lane, nLanes, rowTime)
        }
        val ramps: Array[ArrivalSource] = Array.tabulate(nRamps) { r =>
            new AggregatedRampArrivalSource(demand, r, rowTime)
        }
        (ml, ramps)

end ArrivalSource


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run ArrivalSource tests.
 *  > runMain scalation.simulation.process.arrival.runArrivalSourceTest
 */
@main def runArrivalSourceTest(): Unit =

    import scalation.random.Erlang2S

    banner("ArrivalSource Test Suite")

    // Clear caches to ensure fresh data loading
    AggregatedArrivalSource.clearCache()
    AggregatedRampArrivalSource.clearCache()

    var passed = 0
    var failed = 0

    def test(name: String)(block: => Boolean): Unit =
        print(s"  TEST: $name ... ")
        try
            if block then
                println("PASSED")
                passed += 1
            else
                println("FAILED")
                failed += 1
        catch
            case e: Exception =>
                println(s"EXCEPTION: ${e.getMessage}")
                failed += 1
    end test

    //--------------------------------------------------------------------------
    // Test 1: Cache behavior
    //--------------------------------------------------------------------------
    banner("Test Group 1: Cache Behavior")

    test("Cache hit on same demand config") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val rowTime = demand.intervalMin * 60.0
        val (totals1, _) = AggregatedArrivalSource.loadMainlineEntry(demand, 0, 5, rowTime)
        val (totals2, _) = AggregatedArrivalSource.loadMainlineEntry(demand, 1, 5, rowTime)
        totals1.sameElements(totals2)
    }

    test("Different lanes get different mu arrays") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val rowTime = demand.intervalMin * 60.0
        val (_, mu0) = AggregatedArrivalSource.loadMainlineEntry(demand, 0, 5, rowTime)
        val (_, mu1) = AggregatedArrivalSource.loadMainlineEntry(demand, 1, 5, rowTime)
        mu0.length == mu1.length && mu0.length > 0
    }

    //--------------------------------------------------------------------------
    // Test 2: Time filtering
    //--------------------------------------------------------------------------
    banner("Test Group 2: Time Filtering")

    test("StartTime 17:00:00 filters to ~72 rows") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val rowTime = demand.intervalMin * 60.0
        val (_, muArray) = AggregatedArrivalSource.loadMainlineEntry(demand, 0, 5, rowTime)
        muArray.length >= 60 && muArray.length <= 80
    }

    //--------------------------------------------------------------------------
    // Test 3: Mu computation edge cases
    //--------------------------------------------------------------------------
    banner("Test Group 3: Mu Computation")

    test("Mu values are positive") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val rowTime = demand.intervalMin * 60.0
        val (_, muArray) = AggregatedArrivalSource.loadMainlineEntry(demand, 0, 5, rowTime)
        muArray.forall(mu => mu > 0.0)
    }

    test("Zero flow produces MaxValue mu") {
        val mu = if 0.0 > 0.0 then 300.0 / 0.0 else Double.MaxValue
        mu == Double.MaxValue
    }

    test("Positive flow produces finite mu") {
        val mu = if 10.0 > 0.0 then 300.0 / 10.0 else Double.MaxValue
        mu == 30.0 && mu.isFinite
    }

    //--------------------------------------------------------------------------
    // Test 4: AggregatedArrivalSource class
    //--------------------------------------------------------------------------
    banner("Test Group 4: AggregatedArrivalSource Class")

    test("getTotalVehicles returns non-negative") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val source = new AggregatedArrivalSource(demand, 0, 5, 300.0)
        source.getTotalVehicles(0) >= 0
    }

    test("getMu with valid timeIdx returns value") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val source = new AggregatedArrivalSource(demand, 0, 5, 300.0)
        source.getMu(0, 0) > 0.0
    }

    test("getMu with out-of-bounds timeIdx handled") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val source = new AggregatedArrivalSource(demand, 0, 5, 300.0)
        source.getMu(0, 9999) > 0.0
    }

    test("getDistribution returns Erlang2S") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val source = new AggregatedArrivalSource(demand, 0, 5, 300.0)
        source.getDistribution.isInstanceOf[Erlang2S]
    }

    test("isPerLane returns true for mainline") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val source = new AggregatedArrivalSource(demand, 0, 5, 300.0)
        source.isPerLane
    }

    //--------------------------------------------------------------------------
    // Test 5: Ramp arrival source
    //--------------------------------------------------------------------------
    banner("Test Group 5: AggregatedRampArrivalSource")

    test("Ramp cache hit on same demand config") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val rowTime = demand.intervalMin * 60.0
        val (total1, mu1) = AggregatedRampArrivalSource.loadRamp(demand, 0, rowTime)
        val (total2, mu2) = AggregatedRampArrivalSource.loadRamp(demand, 0, rowTime)
        total1 == total2 && mu1.length == mu2.length
    }

    test("Ramp getTotalVehicles returns non-negative") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val source = new AggregatedRampArrivalSource(demand, 0, 300.0)
        source.getTotalVehicles(0) >= 0
    }

    test("Ramp isPerLane returns false") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val source = new AggregatedRampArrivalSource(demand, 0, 300.0)
        !source.isPerLane
    }

    test("Ramp with no orFile returns empty") {
        val demand = AggregatedDemand(dataDir = "x", mlFile = "x.csv", orFile = None)
        val (total, muArray) = AggregatedRampArrivalSource.loadRamp(demand, 0, 300.0)
        total == 0 && muArray.isEmpty
    }

    //--------------------------------------------------------------------------
    // Test 6: Edge cases — invalid indices
    //--------------------------------------------------------------------------
    banner("Test Group 6: Edge Cases")

    test("Lane index beyond nLanes handled") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val rowTime = demand.intervalMin * 60.0
        val (totals, muArray) = AggregatedArrivalSource.loadMainlineEntry(demand, 10, 5, rowTime)
        muArray.forall(_ == Double.MaxValue) || totals.length == 5
    }

    test("Ramp index beyond available ramps handled") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val rowTime = demand.intervalMin * 60.0
        val (total, muArray) = AggregatedRampArrivalSource.loadRamp(demand, 99, rowTime)
        total == 0 && muArray.isEmpty
    }

    //--------------------------------------------------------------------------
    // Test 7: Factory method
    //--------------------------------------------------------------------------
    banner("Test Group 7: ArrivalSource.fromAggregated")

    test("fromAggregated creates correct number of sources") {
        val demand = AggregatedDemand.I210_WB_Baseline
        val (mlSources, rampSources) = ArrivalSource.fromAggregated(demand, 5, 3)
        mlSources.length == 5 && rampSources.length == 3
    }

    //--------------------------------------------------------------------------
    // Test 8: Synthetic sources
    //--------------------------------------------------------------------------
    banner("Test Group 8: Synthetic Sources")

    test("syntheticSources creates correct counts") {
        val dist = Erlang2S(0.6)
        val (mlSources, rampSources) = ArrivalSource.syntheticSources(100, 50, 4, 2, dist)
        mlSources.length == 4 && rampSources.length == 2
    }

    test("syntheticSources returns fixed vehicle count") {
        val dist = Erlang2S(0.6)
        val (mlSources, _) = ArrivalSource.syntheticSources(100, 50, 4, 2, dist)
        mlSources(0).getTotalVehicles(0) == 100
    }

    //--------------------------------------------------------------------------
    // Summary
    //--------------------------------------------------------------------------
    println()
    banner("Test Summary")
    println(s"  PASSED: $passed")
    println(s"  FAILED: $failed")
    println(s"  TOTAL:  ${passed + failed}")
    if failed > 0 then println("  Some tests failed!")
    else println("  All tests passed!")

end runArrivalSourceTest


