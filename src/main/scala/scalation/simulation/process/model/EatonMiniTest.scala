//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    EatonMiniTest: the full cleaned Eaton network, built by `NetworkBuilder`
 *
 *  Every node and directed edge of `data/network/eaton/edges.geojson` becomes a
 *  simulation component: motorway and motorway_link edges microscopic, every other
 *  class mesoscopic.  There is no background layer: what is drawn is the network
 *  that is simulated.  Vehicles are routed over the whole graph from one source
 *  node to one of two real dead ends selected from the PeMS split estimate, by
 *  the same `Vehicle` across both resolutions under one agenda.
 */

package scalation
package simulation
package process
package model

import scala.collection.immutable.Map
import scalation.animation.OsmRoadNetwork
import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.Uniform
import scalation.simulation.process.{IntegratorType, IDMDynamics}
import scalation.simulation.process.builder.NetworkBuilder
import scalation.simulation.process.arrival.{AggregatedArrivalSource, AggregatedRampArrivalSource, ArrivalSource}
import scalation.simulation.process.config.{AggregatedDemand, EatonCorridorConfig, PeMSDemand}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run the EatonMiniTest simulation.
 *  > runMain scalation.simulation.process.model.runEatonMiniTest
 */
@main def runEatonMiniTest (): Unit = new EatonMiniTest ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EatonMiniTest` class sends vehicles across the full Eaton network.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed versus animation speed
 *  @param stream     the base random-number stream
 */
class EatonMiniTest (name: String = "EatonMiniTest", reps: Int = 1, animating: Boolean = true,
                     aniRatio: Double = 200.0, stream: Int = 0)
      extends Model (name, reps, animating, aniRatio) with RowTimeLoader:

    IDMDynamics.integratorType = IntegratorType.Ballistic

    // The source is upstream of the I-210/SR-134 westbound split.
    private val sourceId = 616843101L        // Rosemead I-210 WB entry edge tail
    private val i210Sink = 356052035L        // terminal I-210 westbound node
    private val sr134Sink = 27568007L        // terminal SR-134/CA-2 westbound node

    // the whole network, straight from the GeoJSON
    private val edges = NetworkBuilder.read ("data/network/eaton/edges.geojson")

    // Bind PeMS observations to existing roads; internal observations are not sources.
    private val stations = EatonCorridorConfig.loadStationMap ()
    private val stationEdges = EatonCorridorConfig.loadStationEdges (stations, edges)
    banner ("Eaton station-to-road bindings")
    for (station, edge, note) <- stationEdges do
        val road = edge match
            case Some (e) => s"${e.u} -> ${e.v} key=${e.key} ref=${e.ref}"
            case None     => "EXCLUDED"
        println (s"${station.stationId} ${station.location} (${station.laneType}): $road | $note")
    end for
    private val reviewedIds = stationEdges.map (_._1.stationId).toSet
    private val unmapped = stations.filter (s => s.direction == "W" && ! reviewedIds.contains (s.stationId))
    println (s"mapped=${stationEdges.count (_._2.nonEmpty)}, excluded=${stationEdges.count (_._2.isEmpty)}, " +
             s"unreviewed WB=${unmapped.length}")
    println (s"Unreviewed WB stations: ${unmapped.map (_.stationId).mkString (", ")}")

    // PeMS supplies aggregate mainline arrivals; the graph supplies all paths.
    private val demand = AggregatedDemand.I210_WB_Baseline
    private val pems210     = PeMSDemand.I210_WB_Anchor ()               // anchor sensor 717653 (Rosemead): lane speeds, as in EatonFireModel
    private val anchorSpeed = RowTimeLoader.getSpeedMatrixFromFile (
        pems210.dataDir + "/" + pems210.mainline.anchorFile, pems210.window, pems210.layout)
    private val nLanes      = anchorSpeed.dim2                           // one mainline source per lane
    private val (mainlineSources, _) = ArrivalSource.fromAggregated (demand, nLanes, 0)
    private val generatedVehicles = mainlineSources.map (_.getTotalVehicles (0)).sum
    private val arrivalDistribution = demand.distribution
    private val destinationRV = Uniform (0.0, 1.0)
    private val splitRatios = AggregatedArrivalSource.computeSplitRatios (
        AggregatedDemand.I210_WB_Baseline, 717634,
        AggregatedDemand.SR134_WB_Baseline, 717603)

    private def currentSplitRatio: Double =
        val interval = (clock / (demand.intervalMin * 60.0)).toInt
        if interval >= 0 && interval < splitRatios.length then splitRatios(interval)
        else if splitRatios.nonEmpty then splitRatios.last
        else 0.30

    private def destination: Long =
        if destinationRV.gen < currentSplitRatio then sr134Sink else i210Sink

    // These reviewed on-ramps have station-ID demand in the I-210 OR file.
    // The SR-134 FF connector remains excluded until its FF-file lookup is added.
    private val rampStationIds = Array (716596, 716588, 716586, 770402)
    private val rampBindings = rampStationIds.map { stationId =>
        stationEdges.find (_._1.stationId == stationId).flatMap (_._2) match
            case Some (edge) => edge
            case None => throw new IllegalArgumentException (s"missing mapped ramp station $stationId")
    }
    private val rampDemand = rampStationIds.map { stationId =>
        AggregatedRampArrivalSource.loadRampByStation (
            demand, stationId, demand.intervalMin * 60.0)
    }
    private val rampTotals = rampDemand.map (_._1)
    private val rampMu = rampDemand.map (_._2)
    private val totalGeneratedVehicles = generatedVehicles + rampTotals.sum
    private val nRows = math.min ((rampMu.map (_.length) :+ 72).max, anchorSpeed.dim)   // rows of demand and lane speed

    override val rowTimeSlice: Double = demand.intervalMin * 60.0
    override def getDataDimension: Int = nRows

    // subtypes 0 until nLanes are the mainline lanes, then the ramps (as in CalRoute101_3)
    override def getMuForSource (sourceIdx: Int): VectorD =
        if sourceIdx < nLanes then
            RowTimeLoader.getMuForSourceDefault (mainlineSources, Array.empty [ArrivalSource], nLanes, nRows, sourceIdx)
        else if sourceIdx - nLanes < rampMu.length then VectorD (rampMu(sourceIdx - nLanes))
        else VectorD.fill (nRows) (Double.MaxValue)

    override def getSpeedMatrix (): MatrixD = anchorSpeed

    // The camera opens on the source (no background layer, no fit-to-window): the frame is a
    // 1000 m x 600 m box around the source at 2.9 px per metre, so the default top-left view
    // shows the source close up.  The rest of the network projects outside that box (large or
    // negative coordinates); the mouse wheel zooms out to it.
    private val sourceEdge = edges.find (_.u == sourceId).get
    private val PX_PER_M   = 2.9                                            // drawing scale, as before
    private val boxW       = 1000.0                                         // metres
    private val boxH       = 600.0
    private val westOfSrc  = 114.0                                          // the source sits 114 m from the box's west side ...
    private val northOfSrc = 69.0                                           // ... and 69 m from its north side: about (350, 220) px
    private val mLat       = 111320.0                                       // metres per degree of latitude
    private val mLon       = 111320.0 * math.cos (math.toRadians (sourceEdge.uLat))
    private val nwLat      = sourceEdge.uLat + northOfSrc / mLat
    private val nwLon      = sourceEdge.uLon - westOfSrc / mLon
    private val anchors    = Array ((nwLat, nwLon), (nwLat - boxH / mLat, nwLon + boxW / mLon))
    private val project    = OsmRoadNetwork.buildProjector (anchors, (boxW * PX_PER_M + 40.0, boxH * PX_PER_M + 40.0), "EatonMiniTest")

    private val (sx, sy)   = project (sourceEdge.uLat, sourceEdge.uLon)
    // one mainline VSource per PeMS lane: its subtype is the lane, its count and rate are that lane's
    private val entries = Array.tabulate (nLanes) { l =>
        new VSource (s"SO$l", this, () => Car (sourceId, destination), l,
                     mainlineSources(l).getTotalVehicles (l), mainlineSources(l).getDistribution,
                     Array (sx, sy, 4.0, 4.0), s"M$l")
    }

    private val rampSources = rampBindings.indices.map { i =>
        val edge = rampBindings(i)
        val (x, y) = project (edge.uLat, edge.uLon)
        new VSource (s"RAMP-${rampStationIds(i)}", this,
                     () => Car (edge.u, destination), nLanes + i, rampTotals(i),
                     arrivalDistribution, Array (x, y, 4.0, 4.0), s"R${rampStationIds(i)}")
    }.toArray

    /** Lane 0's source is installed as the actual graph component at its OSM node.
     *  This keeps the first transport attached to a VSource rather than to a
     *  duplicate Junction; the other lane sources are registered below.
     */
    private lazy val graph = NetworkBuilder.build (edges, Array (i210Sink, sr134Sink), project, IDMDynamics, nt = 2,
                                                    stream = stream,
                                                    sourceComponents = Map (sourceId -> entries(0)) ++
                                                        rampSources.zip (rampBindings).map ((s, e) => e.u -> s).toMap)
    private lazy val startNode = graph.nodeIndex (sourceId)
    private lazy val i210DestNode = graph.nodeIndex (i210Sink)
    private lazy val sr134DestNode = graph.nodeIndex (sr134Sink)
    graph.routeTo (Array (i210DestNode, sr134DestNode))

    addComponent (graph.components*)
    addComponent (entries.tail*)                                        // lane 0 is a graph node; lanes 1 and up are not

    case class Car (originId: Long, destinationId: Long) extends Vehicle ("c", this):
        override def act (): Unit =
            val destinationNode = graph.nodeIndex (destinationId)
            graph.drive (this, graph.nodeIndex (originId), destinationNode)
        end act
    end Car

    banner ("EatonMiniTest: the full Eaton network, micro and meso")
    println (s"nodes=${graph.nodeId.length}, edges=${edges.length} " +
             s"(micro=${edges.count (_.group.micro)}, meso=${edges.count (! _.group.micro)})")
    println (s"components registered=${graph.components.length}")
    println (s"source node=$sourceId, destinations=I210:$i210Sink/SR134:$sr134Sink, " +
             s"vehicles=$totalGeneratedVehicles (mainline=$generatedVehicles, ramps=${rampTotals.sum}), " +
             s"split intervals=${splitRatios.length}")

    simulate ()
    waitFinished ()

    private val i210Exit = graph.node (i210DestNode).asInstanceOf [Sink]
    private val sr134Exit = graph.node (sr134DestNode).asInstanceOf [Sink]
    val completed = i210Exit.durationStat.num + sr134Exit.durationStat.num
    require (completed == totalGeneratedVehicles, s"completed=$completed, expected=$totalGeneratedVehicles")
    banner ("EatonMiniTest: PASS")
    println (s"completed=$completed, I210=${i210Exit.durationStat.num}, SR134=${sr134Exit.durationStat.num}")
    println (s"agenda activations=$agendaActivations")
    Model.shutdown ()

end EatonMiniTest
