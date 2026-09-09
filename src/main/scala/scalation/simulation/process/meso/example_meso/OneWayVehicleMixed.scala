//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishi
 *  @version 2.0
 *  @date    Wed Sep 9 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Deterministic Same-Actor Micro-Meso-Micro Viability Test
 */

package scalation
package simulation
package process
package meso
package example_meso                                    // One-Shot

import scalation.mathstat.VectorD
import scalation.random.Sharp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Launch the deterministic mixed-resolution viability test.
 *  > runMain scalation.simulation.process.meso.example_meso.runOneWayVehicleMixed
 */
@main def runOneWayVehicleMixed (): Unit = new OneWayVehicleMixed ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OneWayVehicleMixed` class sends the same `Vehicle` actor through a
 *  microscopic link, a mesoscopic link, and a second microscopic link under
 *  one process-interaction director and agenda.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed versus animation speed
 *  @param nStop      the number of arrivals before stopping
 *  @param stream     the base random-number stream
 */
class OneWayVehicleMixed (name: String = "OneWayVehicleMixed", reps: Int = 1,
                          animating: Boolean = false, aniRatio: Double = 500.0,
                          nStop: Int = 4, stream: Int = 0)
    extends Model (name, reps, animating, aniRatio):

    private val debug = debugf ("OneWayVehicleMixed", true)


    //--------------------------------------------------
    // Deterministic corridor configuration

    val linkLength      = 500.0                         // metres per link
    val freeFlowSpeed   = 10.0                          // metres/second
    val mesoCapacity    = 1800.0                        // vehicles/hour
    val storageCapacity = 2                             // vehicles

    Vehicle.setInitialSpeed (freeFlowSpeed)

    //--------------------------------------------------
    // Existing Scalation components

    val entry     = VSource ("entry", this, () => Car (), 0, nStop,
                             Sharp (1.0, stream), (100, 290))
    val boundary1 = Junction ("micro-to-meso", xy = (600, 290), nt = 2, nl = 1)
    val boundary2 = Junction ("meso-to-micro", xy = (1100, 290), nt = 2, nl = 1)
    val exit      = Sink ("exit", (1600, 290))

    val microIn = Pathway ("micro-in", Array.empty [Junction], entry, boundary1,
                           IDMDynamics, laneShift = VectorD (0.0, 0.0))
    val meso = MesoVTransport ("meso", boundary1, boundary2,
                               freeFlowSpeed, mesoCapacity,
                               bend = 0.0, storageCapacity = storageCapacity)
    val microOut = Pathway ("micro-out", Array.empty [Junction], boundary2, exit,
                            IDMDynamics, laneShift = VectorD (0.0, 0.0))

    microIn.seg(0).length  = linkLength
    meso.length            = linkLength
    microOut.seg(0).length = linkLength

    addComponent (entry, boundary1, boundary2, exit, microIn, meso, microOut)

    //--------------------------------------------------
    // One actor crosses both resolution boundaries

    case class Car () extends Vehicle ("c", this):

        override def act (): Unit =
            val originalId = id
            laneID = subtype

            microIn.joinAt (this, microIn.getLast, 0)
            debug ("MICRO_IN", s"t=${director.clock}, actor=$displayLabel, id=$id, joined micro-in DLL")
            microIn.seg(0).move ()
            boundary1.jump ()
            microIn.removeFromAlist (this)
            require (myPathNode == null && myPathway == null,
                     "vehicle must leave the incoming micro DLL before entering meso")

            debug ("MESO_IN", s"t=${director.clock}, actor=$displayLabel, id=$id, no micro DLL membership")
            meso.move ()
            require (id == originalId, "vehicle identity changed across the mesoscopic link")
            require (myPathNode == null && myPathway == null,
                     "mesoscopic traversal must not create micro DLL membership")

            boundary2.jump ()
            val leader = microOut.mergeLeaderAt (0)
            microOut.joinAt (this, leader, 0)
            require (id == originalId, "vehicle identity changed at the meso-to-micro boundary")
            require (myPathNode != null && (myPathway eq microOut),
                     "vehicle must join the outgoing micro DLL before microscopic movement")
            debug ("MICRO_OUT", s"t=${director.clock}, actor=$displayLabel, id=$id, joined micro-out DLL")
            microOut.seg(0).move ()
            microOut.removeFromAlist (this)

            require (id == originalId, "vehicle identity changed before sink completion")
            debug ("SINK", s"t=${director.clock}, actor=$displayLabel, id=$id, completed mixed corridor")
            exit.leave ()
        end act

    end Car

    banner ("Expected mixed-resolution trace")
    println ("same Vehicle: micro-in join/move/jump/remove -> meso move/jump -> micro-out join/move/remove -> sink")
    println ("DLL membership exists only on the two microscopic pathways")

    simulate ()
    waitFinished ()

    val completed = exit.durationStat.num
    require (completed == nStop, s"completed=$completed, expected=$nStop")
    require (meso.enteredCount == nStop, s"meso entered=${meso.enteredCount}, expected=$nStop")
    require (meso.releasedCount == nStop, s"meso released=${meso.releasedCount}, expected=$nStop")
    require (meso.occupancy == 0, s"final meso occupancy=${meso.occupancy}, expected=0")
    require (meso.blockedCount == 0, s"blocked meso vehicles remain=${meso.blockedCount}")
    require (microIn.vList.size == 0, s"incoming micro DLL size=${microIn.vList.size}, expected=0")
    require (microOut.vList.size == 0, s"outgoing micro DLL size=${microOut.vList.size}, expected=0")

    banner ("Same-actor micro-meso-micro viability: PASS")
    println (s"completed=$completed, meso entered=${meso.enteredCount}, meso released=${meso.releasedCount}")
    println (s"micro-in DLL=${microIn.vList.size}, meso occupancy=${meso.occupancy}, " +
             s"micro-out DLL=${microOut.vList.size}")
    println (s"agenda activations=$agendaActivations")
    Model.shutdown ()

end OneWayVehicleMixed
