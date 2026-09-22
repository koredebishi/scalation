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

import scala.collection.immutable.Map
import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.Uniform
import scalation.simulation.process.{IDMDynamics, IntegratorType, Model, RowTimeLoader, Vehicle}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run the EatonMiniTest simulation. */
@main def runEatonMiniTest (): Unit = new EatonMiniTest ()

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Compose the EatonMini components and run the model. */
class EatonMiniTest (name: String = "EatonMiniTest", reps: Int = 1,
                     animating: Boolean = true, aniRatio: Double = 200.0,
                     stream: Int = 0)
      extends Model (name, reps, animating, aniRatio) with RowTimeLoader:

    //--------------------------------------------------
    // Configure the car-following dynamics
    IDMDynamics.integratorType = IntegratorType.Ballistic

    //--------------------------------------------------
    // Prepare the OSM network and Eaton data
    private val network = new EatonMiniNetwork (stream)
    private val data = new EatonMiniData (network.edges)

    //--------------------------------------------------
    // Supply the RowTimeLoader contract from Eaton data
    override val rowTimeSlice: Double = data.rowTimeSlice
    override def getDataDimension: Int = data.nRows
    override def getMuForSource (sourceIdx: Int): VectorD = data.getMuForSource (sourceIdx)
    override def getSpeedMatrix (): MatrixD = data.getSpeedMatrix

    //--------------------------------------------------
    // Select a destination from the measured split ratio
    private val destinationRV = Uniform (0.0, 1.0)

    private def currentSplitRatio: Double =
        val interval = (clock / (data.demand.intervalMin * 60.0)).toInt
        if interval >= 0 && interval < data.splitRatios.length then data.splitRatios(interval)
        else if data.splitRatios.nonEmpty then data.splitRatios.last
        else 0.30

    private def destination: Long =
        if destinationRV.gen < currentSplitRatio then EatonMiniConfig.sr134Sink
        else EatonMiniConfig.i210Sink


    //--------------------------------------------------
    // Create the vehicle sources and source positions
    private val (sx, sy) = network.sourcePosition
    private val entries = Array.tabulate(data.nLanes) { l =>
        new VSource(s"SO$l", this, () => Car(EatonMiniConfig.sourceId, destination), l,
            data.mainlineSources(l).getTotalVehicles(l), data.mainlineSources(l).getDistribution,
            Array(sx, sy, 4.0, 4.0), s"M$l")
    }

    private val rampSources = data.rampBindings.indices.map { i =>
        val edge = data.rampBindings(i)
        val (x, y) = network.project(edge.uLat, edge.uLon)
        new VSource(s"RAMP-${EatonMiniConfig.rampStationIds(i)}", this,
            () => Car(edge.u, destination), data.nLanes + i,
            data.rampTotals(i), data.arrivalDistribution,
            Array(x, y, 4.0, 4.0), s"R${EatonMiniConfig.rampStationIds(i)}")
    }.toArray

    //--------------------------------------------------
    // Build the shared graph and register its source components
    private lazy val graph = network.build(
        Map(EatonMiniConfig.sourceId -> entries(0)) ++
          rampSources.zip(data.rampBindings).map((s, e) => e.u -> s),
        IDMDynamics, nt = 2)
    private lazy val i210DestNode = graph.nodeIndex(EatonMiniConfig.i210Sink)
    private lazy val sr134DestNode = graph.nodeIndex(EatonMiniConfig.sr134Sink)

    private val i210Exit = graph.node(i210DestNode).asInstanceOf[Sink]
    private val sr134Exit = graph.node(sr134DestNode).asInstanceOf[Sink]
    private val completed = i210Exit.durationStat.num + sr134Exit.durationStat.num

    //--------------------------------------------------
    // Route destinations and register graph and source components
    graph.routeTo(Array(i210DestNode, sr134DestNode))
    addComponent(graph.components *)
    addComponent(entries.tail *)


    //--------------------------------------------------
    // Define the Eaton vehicle actor and its destination handoff
    case class Car (originId: Long, destinationId: Long) extends Vehicle ("c", this):
        override def act (): Unit =
            val destinationNode = graph.nodeIndex (destinationId)
            graph.drive (this, graph.nodeIndex (originId), destinationNode)
        end act
    end Car

    //--------------------------------------------------
    // Run the model and verify completion
    simulate ()
    waitFinished ()
    Model.shutdown ()

end EatonMiniTest
