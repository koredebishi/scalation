//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** EatonMiniTest: Eaton's full-network mixed-resolution stress model.
 *
 *  Eaton-specific configuration, data, network assembly, and vehicle driving
 *  live in the companion classes in this package. This class owns only the
 *  Scalation model lifecycle and their composition.
 */

package scalation
package simulation
package process
package model
package eaton

import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer

import scalation.mathstat.{MatrixD, Statistic, VectorD}
import scalation.random.Uniform
import scalation.simulation.process.{IDMDynamics, IntegratorType, Model, RowTimeLoader, Vehicle}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run the EatonMiniTest simulation. */
@main def runEatonMiniTest (): Unit = new EatonMiniTest ()

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Benchmark EatonMiniTest: build time, then per-replication run time, events,
 *  memory and completions (mean ± CI), written as one JSON summary.
 */
@main def benchEatonMiniTest (): Unit =
    var model: EatonMiniTest = null
    val setupMs = gauge { model = new EatonMiniTest (animating = false, reps = 1, autoRun = false) }
    model.setTime (3600.0)                       // generate demand for one hour (17:00-18:00), then let the network drain
    model.simulate ()
    model.waitFinished ()
    val path = Path.of ("log", "simulation", "eaton", s"eaton_bench_${System.currentTimeMillis}.json")
    val summary = model.experimentSummary (setupMs)
    summary.writeJson (path)
    println (s"simulation_experiment_json = $path")
    println (summary.prettyJson)
    Model.shutdown ()
end benchEatonMiniTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Compose the EatonMini components and run the model. */
class EatonMiniTest (name: String = "EatonMiniTest", reps: Int = 1,
                     animating: Boolean = true, aniRatio: Double = 200.0,
                     stream: Int = 0, warmups: Int = 0, autoRun: Boolean = true)
      extends Model (name, reps, animating, aniRatio, warmupReps = warmups) with RowTimeLoader:

    //--------------------------------------------------
    // Configure the car-following dynamics
    IDMDynamics.integratorType = IntegratorType.Ballistic

    //--------------------------------------------------
    // Prepare the OSM network and Eaton data
    private val network = new EatonMiniNetwork (stream)
    private val data = new EatonMiniData (network)

    //--------------------------------------------------
    // Supply the RowTimeLoader contract from Eaton data
    override val rowTimeSlice: Double = data.rowTimeSlice
    override def getDataDimension: Int = data.nRows
    override def getMuForSource (sourceIdx: Int): VectorD = data.getMuForSource (sourceIdx)
    override def getSpeedMatrix (): MatrixD = data.getSpeedMatrix

    //--------------------------------------------------
    // Build the shared graph and route to every sink (area exits)
    private lazy val graph = network.build(IDMDynamics, nt = 2)
    require (network.sinks.contains (EatonMiniConfig.i210Sink) && network.sinks.contains (EatonMiniConfig.sr134Sink),
             "the I-210 and SR-134 freeway ends must be sinks")
    private val sinkNodes = network.sinks.map (graph.nodeIndex)
    private lazy val i210DestNode = graph.nodeIndex(EatonMiniConfig.i210Sink)
    private lazy val sr134DestNode = graph.nodeIndex(EatonMiniConfig.sr134Sink)
    private val ebDestNode = graph.nodeIndex (network.freewayEnd (data.sourceEB))

    private val i210Exit = graph.node(i210DestNode).asInstanceOf[Sink]
    private val sr134Exit = graph.node(sr134DestNode).asInstanceOf[Sink]
    private val completed = i210Exit.durationStat.num + sr134Exit.durationStat.num

    graph.routeTo(sinkNodes)

    //--------------------------------------------------
    // Origins: the two mainline entries and every on-ramp (node indices)
    private val wbOrigin = graph.nodeIndex (EatonMiniConfig.sourceId)
    private val ebOrigin = graph.nodeIndex (data.sourceEB)
    private val rampOrigins = data.rampBindings.map (e => graph.nodeIndex (e.u))

    //--------------------------------------------------
    // Off-ramps: off-ramp k is the edge from(k) -> to(k) of its PeMS station
    private val offRampOf: Map [(Int, Int), Int] = data.offRampBindings.indices.map { k =>
        val e = data.offRampBindings(k)
        (graph.nodeIndex (e.u), graph.nodeIndex (e.v)) -> k
    }.toMap

    //--------------------------------------------------
    // Freeway ends each origin can reach on the freeway alone (else the nearest by path)
    private val highwayOf: Map [(Int, Int), String] =
        graph.edge.indices.map (e => (graph.from(e), graph.to(e)) -> graph.edge(e).highway).toMap

    private def onFreeway (p: Array [Int]): Boolean =
        p.nonEmpty && p.indices.dropRight (1).forall (i => highwayOf ((p(i), p(i + 1))).startsWith ("motorway"))

    private def freewayEnds (origin: Int): Array [Int] =
        val ends  = Array (i210DestNode, sr134DestNode, ebDestNode)
        val paths = ends.map (network.path (graph, origin, _))
        val free  = ends.indices.filter (i => onFreeway (paths(i))).map (ends(_)).toArray
        val reached = ends.indices.filter (paths(_).nonEmpty)
        require (reached.nonEmpty, s"origin ${graph.nodeId (origin)} reaches no freeway end")
        if free.nonEmpty then free
        else Array (ends (reached.minBy (paths(_).length)))
    end freewayEnds

    //--------------------------------------------------
    // Each origin's exit plan, derived from the routing table: the sinks reached through
    // each off-ramp, and, per freeway end, the off-ramps met in travel order.  Off-ramp k
    // is "ahead" on a freeway path when the path to k's sinks leaves it on ramp edges only.
    private val origins   = (Array (wbOrigin, ebOrigin) ++ rampOrigins).distinct
    private val groups    = origins.map (o => o -> network.exitGroups (graph, o, sinkNodes, offRampOf)).toMap
    private val endsOf    = origins.map (o => o -> freewayEnds (o)).toMap

    private def leavesAt (freeway: Array [Int], toSink: Array [Int], k: Int): Int =   // -1: not by ramp k
        var d = 0                                                                   // first node off the freeway path
        while d < freeway.length && d < toSink.length && freeway(d) == toSink(d) do d += 1
        val f = toSink.indices.dropRight (1).indexWhere (i => offRampOf.get ((toSink(i), toSink(i + 1))).contains (k))
        val byRamp = d >= 1 && f >= d - 1 &&
                     (d - 1 until f).forall (i => highwayOf ((toSink(i), toSink(i + 1))) == "motorway_link")
        if byRamp && d < freeway.length then d else -1
    end leavesAt

    private val rampsAhead: Map [(Int, Int), Array [Int]] =
        (for o <- origins; end <- endsOf (o) yield
            val p  = network.path (graph, o, end)
            val ks = groups (o).keys.filter (_ >= 0).toArray
            val at = ks.map (k => leavesAt (p, network.path (graph, o, groups (o)(k)(0)), k))
            (o, end) -> ks.indices.filter (at(_) >= 0).sortBy (at(_)).map (ks(_)).toArray
        ).toMap

    private val unusedOffRamps = data.offRampStations.indices.filterNot (k => groups.values.exists (_.contains (k)))
    println (s"EatonMiniTest: ${origins.length} origins, ${sinkNodes.length} sinks; " +
             s"${unusedOffRamps.length} off-ramps lead to no sink: " +
             unusedOffRamps.map (data.offRampStations(_).stationId).mkString (", "))

    //--------------------------------------------------
    // Select each car's sink (the OD rule): pick its freeway end (I-210 vs SR-134 by
    // the measured split), then step through the off-ramps ahead; leave at off-ramp k
    // with its PeMS share for this interval and head for a sink in k's group
    private val chooseRV = Uniform (0.0, 1.0, stream)

    private def interval: Int = (clock / rowTimeSlice).toInt

    private def currentSplitRatio: Double =
        val i = (clock / (data.demand.intervalMin * 60.0)).toInt
        if i >= 0 && i < data.splitRatios.length then data.splitRatios(i)
        else if data.splitRatios.nonEmpty then data.splitRatios.last
        else 0.30

    private def freewayEnd (origin: Int): Int =
        val ends = endsOf (origin)
        if ends.contains (i210DestNode) && ends.contains (sr134DestNode) then
            if chooseRV.gen < currentSplitRatio then sr134DestNode else i210DestNode
        else ends(0)
    end freewayEnd

    private def share (k: Int): Double =
        val s = data.offRampShares(k)
        if s.isEmpty then 0.0 else s(math.min (math.max (interval, 0), s.length - 1))

    private def destination (origin: Int): Long =
        val end   = freewayEnd (origin)
        val ahead = rampsAhead ((origin, end))
        var sink  = end
        var i     = 0
        while sink == end && i < ahead.length do
            val k = ahead(i)
            if chooseRV.gen < share (k) then
                val group = groups (origin)(k)
                sink = group (math.min ((chooseRV.gen * group.length).toInt, group.length - 1))
            i += 1
        end while
        graph.nodeId (sink)
    end destination

    //--------------------------------------------------
    // Create the vehicle sources: WB lanes, EB lanes, then on-ramps (subtype order)
    private val (sx, sy) = network.sourcePosition
    private val entries = Array.tabulate(data.nLanes) { l =>
        new VSource(s"SO$l", this, () => Car(EatonMiniConfig.sourceId, destination (wbOrigin), l), l,
            data.mainlineSources(l).getTotalVehicles(l), data.mainlineSources(l).getDistribution,
            Array(sx, sy, 4.0, 4.0), s"M$l")
    }

    private val ebEdge = network.edges.find (_.u == data.sourceEB).get
    private val (ex, ey) = network.project (ebEdge.uLat, ebEdge.uLon)
    private val entriesEB = Array.tabulate (data.nLanesEB) { l =>
        new VSource (s"SOE$l", this, () => Car (data.sourceEB, destination (ebOrigin), l), data.nLanes + l,
            data.mainlineSourcesEB(l).getTotalVehicles (l), data.mainlineSourcesEB(l).getDistribution,
            Array (ex, ey, 4.0, 4.0), s"E$l")
    }

    private val rampSources = data.rampBindings.indices.map { i =>
        val edge = data.rampBindings(i)
        val id   = data.rampStations(i).stationId
        val (x, y) = network.project(edge.uLat, edge.uLon)
        new VSource(s"RAMP-$id", this,
            () => Car(edge.u, destination (rampOrigins(i))), data.nLanes + data.nLanesEB + i,
            data.rampTotals(i), data.arrivalDistribution,
            Array(x, y, 4.0, 4.0), s"R$id")
    }.toArray

    //--------------------------------------------------
    // Register graph and source components; every node stays the builder's Junction
    addComponent(graph.components *)
    addComponent(entries *)
    addComponent(entriesEB *)
    addComponent(rampSources *)

    //--------------------------------------------------
    // Define the Eaton vehicle actor and its destination handoff
    case class Car (originId: Long, destinationId: Long, lane: Int = -1) extends Vehicle ("c", this):
        override def act (): Unit =
            if lane >= 0 then laneID = lane                     // mainline car: its source lane
            val destinationNode = graph.nodeIndex (destinationId)
            graph.drive (this, graph.nodeIndex (originId), destinationNode)
        end act
    end Car

    //--------------------------------------------------
    // Per-replication measures, tallied after each measured replication; completions
    // and travel time come from the sinks' own duration statistics
    private val exits                  = sinkNodes.map (graph.node(_).asInstanceOf [Sink])
    private val completedStat          = new Statistic ("completed vehicles per replication")
    private val meanTravelTimeStat     = new Statistic ("mean travel time (s)")
    private val simulationTimeStat     = new Statistic ("simulation time (s)")
    private val computationTimeStat    = new Statistic ("computation time (ms)")
    private val agendaActivationsStat  = new Statistic ("agenda activations per replication")
    private val activationsPerVehStat  = new Statistic ("activations per vehicle")
    private val heapUsedStat           = new Statistic ("heap used (MB)")

    override protected def replicationCompleted (rep: Int, computationTimeMs: Double): Unit =
        if rep > warmupReps then
            val pooled    = Statistic.aggregate (ArrayBuffer.from (exits.map (_.durationStat)), "sinks")
            val completed = pooled.num
            completedStat.tally (completed.toDouble)
            meanTravelTimeStat.tally (pooled.mean)
            simulationTimeStat.tally (simDuration)
            computationTimeStat.tally (computationTimeMs)
            agendaActivationsStat.tally (agendaActivations.toDouble)
            activationsPerVehStat.tally (if completed > 0 then agendaActivations.toDouble / completed else 0.0)
            heapUsedStat.tally (heapUsed.toDouble)
        end if
    end replicationCompleted

    //--------------------------------------------------
    // The replication summary (no single "link 0" on this network)
    def experimentSummary (setupTimeMs: Double): SimulationExperimentSummary =
        SimulationExperimentSummary (name, warmupReps, reps - warmupReps, data.totalGeneratedVehicles,
            SimulationMetricSummary.from (completedStat),
            None,
            SimulationMetricSummary.from (meanTravelTimeStat),
            SimulationMetricSummary.from (simulationTimeStat),
            SimulationMetricSummary.from (computationTimeStat),
            SimulationMetricSummary.from (agendaActivationsStat),
            SimulationMetricSummary.from (activationsPerVehStat),
            heapUsedMB  = Some (SimulationMetricSummary.from (heapUsedStat)),
            setupTimeMs = Some (setupTimeMs),
            scale       = SimulationExperimentSummary.scaleOf (this))
    end experimentSummary

    //--------------------------------------------------
    // Run the model and verify completion
    if autoRun then
        simulate ()
        waitFinished ()
        Model.shutdown ()
    end if

end EatonMiniTest
