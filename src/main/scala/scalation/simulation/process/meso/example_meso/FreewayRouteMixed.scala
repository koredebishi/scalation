//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Three-lane mixed-resolution freeway using Route for microscopic sections. */
package scalation
package simulation
package process
package meso
package example_meso

import scalation.dynamics.IdmFlux
import scalation.mathstat.VectorD
import scalation.random.Exponential

@main def runFreewayRouteMixed (): Unit = new FreewayRouteMixed ()

@main def runFreewayRouteMesoIdm (): Unit =
    val fd = new IdmFlux (v0 = Vehicle.vmax, tau = Vehicle.T, s0 = Vehicle.s,
        len = Vehicle.len, delta = Vehicle.del)
    new FreewayRouteMixed (name = "FreewayRouteMixIdmFlux", flowDiagram = Some (fd), traceSpeeds = true)
end runFreewayRouteMesoIdm

/** Parallel three-lane version of the repeated mixed-resolution freeway.
 *  Meso sections use one `MesoVTransport` per lane; micro sections use one
 *  `Route` containing one continuous DLL per lane.
 */
class FreewayRouteMixed (name: String = "FreewayRouteMixed", reps: Int = 1,
                         animating: Boolean = true, aniRatio: Double = 500.0,
                         nStop: Int = 30, stream: Int = 0,
                         flowDiagram: Option [IdmFlux] = None,
                         traceSpeeds: Boolean = false)
    extends Model (name, reps, animating, aniRatio):

    private val debug = debugf ("FreewayRouteMixed", true)
    private val numLanes = 3
    private val lambda = 3200.0
    private val linkLength = 500.0
    private val laneGap = 20.0
    private val freeFlowSpeed = 68.0 / 2.23694
    private val mesoCapacity = 1800.0
    private val storageCapacity = 10

    Vehicle.setInitialSpeed (freeFlowSpeed)

    private val iArrivalRV = Exponential (HOUR / lambda, stream)

    val entry = VSource ("entry", this, () => Car (), 0, nStop,
                         iArrivalRV, (100, 290))

    val j1 = Junction ("j1", xy = (600, 290), nt = 2, nl = numLanes)
    val j2 = Junction ("j2", xy = (1100, 290), nt = 2, nl = numLanes)
    val j3 = Junction ("j3", xy = (1600, 290), nt = 2, nl = numLanes)
    val j4 = Junction ("j4", xy = (2100, 290), nt = 2, nl = numLanes)
    val j5 = Junction ("j5", xy = (2600, 290), nt = 2, nl = numLanes)
    val j6 = Junction ("j6", xy = (3100, 290), nt = 2, nl = numLanes)
    val j7 = Junction ("j7", xy = (3600, 290), nt = 2, nl = numLanes)
    val j8 = Junction ("j8", xy = (4100, 290), nt = 2, nl = numLanes)
    val exit = Sink ("exit", (4600, 290))

    private def laneShift (lane: Int): VectorD =
        val physicalLane = numLanes - 1 - lane
        val offset = (physicalLane - (numLanes - 1) / 2.0) * laneGap
        VectorD (0.0, -offset)
    end laneShift

    private def makeMeso (prefix: String, from: Component, to: Component): Array [MesoVTransport] =
        Array.tabulate (numLanes) { lane =>
            val shift = laneShift (lane)
            val link = MesoVTransport (s"$prefix-$lane", from, to, freeFlowSpeed, mesoCapacity,
                                       shift1 = shift, shift2 = shift,
                                       storageCapacity = storageCapacity,
                                       flowDiagram = flowDiagram,
                                       traceSpeed = traceSpeeds)
            link.length = linkLength
            link
        }
    end makeMeso

    private val meso1 = makeMeso ("meso1", entry, j1)
    private val meso2 = makeMeso ("meso2", j1, j2)
    private val meso3 = makeMeso ("meso3", j5, j6)
    private val meso4 = makeMeso ("meso4", j7, j8)
    private val meso5 = makeMeso ("meso5", j8, exit)

    private val microMain = Route ("micro-main", numLanes, Array (j3, j4), j2, j5,
                                   IDMDynamics, laneGap = laneGap)
    private val microIsolated = Route ("micro-isolated", numLanes, Array.empty [Junction],
                                      j6, j7, IDMDynamics, laneGap = laneGap)

    for pathway <- microMain.pathway do
        for segment <- pathway.seg do segment.length = linkLength
    for pathway <- microIsolated.pathway do pathway.seg(0).length = linkLength

    private val mesoLinks = Array (meso1, meso2, meso3, meso4, meso5)
    private val junctions = Array (j1, j2, j3, j4, j5, j6, j7, j8)
    private val components: Array [Component] =
        Array [Component] (entry) ++ junctions ++ Array [Component] (exit, microMain, microIsolated) ++ mesoLinks.flatten
    addComponent (components*)

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            laneID = Math.floorMod (id, numLanes)
            val lane = laneID

            meso1(lane).move (); j1.jump ()
            meso2(lane).move (); j2.jump ()

            val main = microMain.pathway (lane)
            main.joinAt (this, main.mergeLeaderAt (0), 0)
            for i <- main.seg.indices do
                main.seg(i).move ()
                if i < main.junc.length then main.junc(i).jump ()
            end for
            j5.jump ()
            main.removeFromAlist (this)

            meso3(lane).move (); j6.jump ()

            val isolated = microIsolated.pathway (lane)
            isolated.joinAt (this, isolated.mergeLeaderAt (0), 0)
            isolated.seg(0).move ()
            j7.jump ()
            isolated.removeFromAlist (this)

            meso4(lane).move (); j8.jump ()
            meso5(lane).move ()
            exit.leave ()
        end act
    end Car

    banner ("Three-lane mixed-resolution Route corridor")
    println ("Meso[3] -> Route[3] micro -> Meso[3] -> Route[3] micro -> Meso[3]")
    println (s"vehicles=$nStop, lanes=$numLanes, idmFlux=${flowDiagram.isDefined}")

    simulate ()
    waitFinished ()

    require (exit.durationStat.num == nStop,
             s"completed=${exit.durationStat.num}, expected=$nStop")
    for links <- mesoLinks; link <- links do
        require (link.enteredCount == nStop / numLanes || link.enteredCount == nStop / numLanes + 1,
                 s"${link.name} lane distribution=${link.enteredCount}")
        require (link.occupancy == 0 && link.blockedCount == 0,
                 s"${link.name} did not drain")
    for pathway <- microMain.pathway ++ microIsolated.pathway do
        require (pathway.vList.isEmpty, s"${pathway.name} did not drain")

    banner ("Three-lane mixed-resolution Route corridor: PASS")
    println (s"completed=${exit.durationStat.num}, agenda activations=$agendaActivations")
    Model.shutdown ()

end FreewayRouteMixed
