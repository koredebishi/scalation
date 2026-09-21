//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishi
 *  @version 2.0
 *  @date    Wed Sep 9 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Repeated Same-Actor Resolution-Transfer Experiment
 */

package scalation
package simulation
package process
package meso
package example_meso                                    // One-Shot

import scalation.dynamics.IdmFlux
import scalation.mathstat.VectorD
import scalation.random.Exponential

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Launch the repeated mixed-resolution freeway-corridor experiment.
 *  > runMain scalation.simulation.process.meso.example_meso.runFreewayCorridorMixed
 */
@main def runFreewayCorridorMixed (): Unit = new FreewayCorridorMixed ()




@main def runFreewayMesoIdm (): Unit =
    val fd = new IdmFlux (v0 = Vehicle.vmax, tau = Vehicle.T, s0 = Vehicle.s,
        len = Vehicle.len, delta = Vehicle.del)
    new FreewayCorridorMixed (name = "FreewayMixIdmFlux", flowDiagram = Some (fd), traceSpeeds = true)
end runFreewayMesoIdm
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FreewayCorridorMixed` class sends each `Vehicle` actor through this
 *  sequence under one process-interaction director and agenda:
 *
 *  Source -> Meso -> Meso -> Micro -> Micro -> Micro -> Meso -> Micro
 *         -> Meso -> Meso -> Sink
 *
 *  The three consecutive microscopic segments share one `Pathway` and DLL.
 *  The isolated microscopic segment has its own `Pathway` and DLL.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed versus animation speed
 *  @param nStop      the number of arrivals before stopping
 *  @param stream     the base random-number stream
 *  @param flowDiagram optional IDM equilibrium speed relation shared by all mesoscopic links
 *  @param traceSpeeds print each IDM density/speed lookup
 */
class FreewayCorridorMixed (name: String = "FreewayCorridorMixed", reps: Int = 1,
                            animating: Boolean = true, aniRatio: Double = 500.0,
                            nStop: Int = 130, stream: Int = 0,
                            flowDiagram: Option [IdmFlux] = None, traceSpeeds: Boolean = false)
    extends Model (name, reps, animating, aniRatio):

    private val debug = debugf ("FreewayCorridorMixed", true)


    val lambda = 1800.0 // Experiment B held-out demand
    val linkLength      = 500.0                         // metres per transport segment
    val freeFlowSpeed   = 68.0 / 2.23694                // 68 mph in metres/second
    val mesoCapacity    = 1800.0                        // vehicles/hour
    val storageCapacity = 10                            // vehicles per mesoscopic link

    Vehicle.setInitialSpeed (freeFlowSpeed)

    
    //--------------------------------------------------
    // Create Random Variables (RVs)
    
    val iArrivalRV = Exponential(HOUR / lambda, stream)


//--------------------------------------------------
    // Nine transport segments separated by eight junctio

    val entry = VSource ("entry", this, () => Car (), 0, nStop,
                         iArrivalRV, (100, 290))

    val j1 = Junction ("j1", xy = (600, 290), nt = 2, nl = 1)
    val j2 = Junction ("j2", xy = (1100, 290), nt = 2, nl = 1)
    val j3 = Junction ("j3", xy = (1600, 290), nt = 2, nl = 1)
    val j4 = Junction ("j4", xy = (2100, 290), nt = 2, nl = 1)
    val j5 = Junction ("j5", xy = (2600, 290), nt = 2, nl = 1)
    val j6 = Junction ("j6", xy = (3100, 290), nt = 2, nl = 1)
    val j7 = Junction ("j7", xy = (3600, 290), nt = 2, nl = 1)
    val j8 = Junction ("j8", xy = (4100, 290), nt = 2, nl = 1)

    val exit = Sink ("exit", (4600, 290))

    val meso1 = MesoVTransport ("meso1", entry, j1, freeFlowSpeed, mesoCapacity,
                                storageCapacity = storageCapacity, flowDiagram = flowDiagram,
                                traceSpeed = traceSpeeds)
    val meso2 = MesoVTransport ("meso2", j1, j2, freeFlowSpeed, mesoCapacity,
                                storageCapacity = storageCapacity, flowDiagram = flowDiagram,
                                traceSpeed = traceSpeeds)

    val microMain = Pathway ("micro-main", Array (j3, j4), j2, j5,
                             IDMDynamics, laneShift = VectorD (0.0, 0.0))

    val meso3 = MesoVTransport ("meso3", j5, j6, freeFlowSpeed, mesoCapacity,
                                storageCapacity = storageCapacity, flowDiagram = flowDiagram,
                                traceSpeed = traceSpeeds)

    val microIsolated = Pathway ("micro-isolated", Array.empty [Junction], j6, j7,
                                 IDMDynamics, laneShift = VectorD (0.0, 0.0))

    val meso4 = MesoVTransport ("meso4", j7, j8, freeFlowSpeed, mesoCapacity,
                                storageCapacity = storageCapacity, flowDiagram = flowDiagram,
                                traceSpeed = traceSpeeds)
    val meso5 = MesoVTransport ("meso5", j8, exit, freeFlowSpeed, mesoCapacity,
                                storageCapacity = storageCapacity, flowDiagram = flowDiagram,
                                traceSpeed = traceSpeeds)

    val junctions = Array (j1, j2, j3, j4, j5, j6, j7, j8)
    val mesoLinks = Array (meso1, meso2, meso3, meso4, meso5)

    for link <- mesoLinks do link.length = linkLength
    for segment <- microMain.seg do segment.length = linkLength
    microIsolated.seg(0).length = linkLength

    addComponent (entry, j1, j2, j3, j4, j5, j6, j7, j8, exit,
                  meso1, meso2, microMain, meso3, microIsolated, meso4, meso5)

    //--------------------------------------------------
    // The same actor crosses every resolution boundary

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            val originalId = id
            laneID = subtype

            meso1.move ()
            j1.jump ()
            meso2.move ()
            j2.jump ()

            microMain.joinAt (this, microMain.mergeLeaderAt (0), 0)        // join the continuous micro DLL
            require (myPathNode != null && (myPathway eq microMain),
                     "vehicle must join the continuous micro DLL")
            for i <- microMain.seg.indices do
                microMain.seg(i).move ()
                if i < microMain.junc.length then microMain.junc(i).jump ()
            end for
            j5.jump ()
            microMain.removeFromAlist (this)
            require (myPathNode == null && myPathway == null,
                     "vehicle must leave the continuous micro DLL before meso3")

            meso3.move ()
            j6.jump ()

            microIsolated.joinAt (this, microIsolated.mergeLeaderAt (0), 0)
            require (myPathNode != null && (myPathway eq microIsolated),
                     "vehicle must join the isolated micro DLL")
            microIsolated.seg(0).move ()
            j7.jump ()
            microIsolated.removeFromAlist (this)
            require (myPathNode == null && myPathway == null,
                     "vehicle must leave the isolated micro DLL before meso4")

            meso4.move ()
            j8.jump ()
            meso5.move ()

            require (id == originalId, "vehicle identity changed in the mixed corridor")
            debug ("SINK", s"t=${director.clock}, actor=$displayLabel, id=$id, completed corridor")
            exit.leave ()
        end act

    end Car

    banner ("G2.2 repeated mixed-resolution freeway corridor")
    println ("Source -> Meso -> Meso -> Micro -> Micro -> Micro -> Meso -> Micro -> Meso -> Meso -> Sink")
    println (s"vehicles=$nStop, corridor junctions=${junctions.length}, " +
             s"continuous micro segments=${microMain.seg.length}")

    simulate ()
    waitFinished ()

    val completed = exit.durationStat.num
    require (completed == nStop, s"completed=$completed, expected=$nStop")
    for junction <- junctions do
        require (junction.durationStat.num == nStop,
                 s"${junction.name} crossings=${junction.durationStat.num}, expected=$nStop")
    end for
    for link <- mesoLinks do
        require (link.enteredCount == nStop && link.releasedCount == nStop,
                 s"${link.name} entered/released=${link.enteredCount}/${link.releasedCount}, expected=$nStop/$nStop")
        require (link.occupancy == 0 && link.blockedCount == 0,
                 s"${link.name} final occupancy/blocked=${link.occupancy}/${link.blockedCount}, expected=0/0")
    end for
    require (microMain.vList.size == 0,
             s"continuous micro DLL size=${microMain.vList.size}, expected=0")
    require (microIsolated.vList.size == 0,
             s"isolated micro DLL size=${microIsolated.vList.size}, expected=0")

    banner ("Repeated mixed-resolution freeway corridor: PASS")
    println (s"completed=$completed, junction crossings=${junctions.map (_.durationStat.num).mkString ("[", ",", "]")}")
    println (s"meso entered=${mesoLinks.map (_.enteredCount).mkString ("[", ",", "]")}")
    println (s"meso released=${mesoLinks.map (_.releasedCount).mkString ("[", ",", "]")}")
    println (s"continuous micro DLL=${microMain.vList.size}, isolated micro DLL=${microIsolated.vList.size}")
    println (s"agenda activations=$agendaActivations")
    Model.shutdown ()

end FreewayCorridorMixed
