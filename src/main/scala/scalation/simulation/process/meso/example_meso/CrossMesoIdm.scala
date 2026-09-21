//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author Korede Bishi
 *  @version 2.0
 *  @date    Sun Sep 20 2026
 *  @note    Equal-link cross junction using the IDM equilibrium lookup table.
 */

package scalation
package simulation
package process
package meso
package example_meso

import scalation.dynamics.IdmFlux
import scalation.random.Exponential

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run two crossing mesoscopic routes with one shared IDM equilibrium diagram.
 *
 *  west -> center -> east
 *  south -> center -> north
 */
@main def runCrossMesoIdm (): Unit = new CrossMesoIdm ()

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A four-link cross with equal link lengths, lane counts, and IDM parameters. */
class CrossMesoIdm (name: String = "CrossMesoIdm", reps: Int = 1,
                    animating: Boolean = true, aniRatio: Double = 500.0,
                    nStop: Int = 30, stream: Int = 0)
    extends Model (name, reps, animating, aniRatio):

    private val linkLength = 500.0
    private val freeFlowSpeed = 68.0 / 2.23694
    private val capacity = 1800.0
    private val storageCapacity = 10
    private val lambda = 1800.0

    Vehicle.setInitialSpeed (freeFlowSpeed)

    // Build the IDM table after setting the model's actual desired speed.
    private val flowDiagram = new IdmFlux (v0 = freeFlowSpeed, tau = Vehicle.T,
                                           s0 = Vehicle.s, len = Vehicle.len,
                                           delta = Vehicle.del)
    private val westArrival = Exponential (HOUR / lambda, stream)
    private val southArrival = Exponential (HOUR / lambda, stream + 1)

    private val west = VSource ("west", this, () => WestCar (), 0, nStop,
                                westArrival, (100, 300))
    private val south = VSource ("south", this, () => SouthCar (), 1, nStop,
                                 southArrival, (600, 0))

    private val center = Junction ("center", xy = (600, 300), nt = 4, nl = 1)
    private val east = Sink ("east", (1100, 300))
    private val north = Sink ("north", (600, 800))

    private val westIn = MesoVTransport ("west-in", west, center, freeFlowSpeed,
                                         capacity, storageCapacity = storageCapacity,
                                         flowDiagram = Some (flowDiagram), traceSpeed = true)
    private val eastOut = MesoVTransport ("east-out", center, east, freeFlowSpeed,
                                          capacity, storageCapacity = storageCapacity,
                                          flowDiagram = Some (flowDiagram), traceSpeed = true)
    private val southIn = MesoVTransport ("south-in", south, center, freeFlowSpeed,
                                          capacity, storageCapacity = storageCapacity,
                                          flowDiagram = Some (flowDiagram), traceSpeed = true)
    private val northOut = MesoVTransport ("north-out", center, north, freeFlowSpeed,
                                           capacity, storageCapacity = storageCapacity,
                                           flowDiagram = Some (flowDiagram), traceSpeed = true)

    private val links = Array (westIn, eastOut, southIn, northOut)
    for link <- links do link.length = linkLength

    private val components: Array [Component] =
        Array [Component] (west, south, center, east, north) ++ links
    addComponent (components*)

    case class WestCar () extends Vehicle ("west-car", this):
        override def act (): Unit =
            westIn.move ()
            center.jump ()
            eastOut.move ()
            east.leave ()
        end act
    end WestCar

    case class SouthCar () extends Vehicle ("south-car", this):
        override def act (): Unit =
            southIn.move ()
            center.jump ()
            northOut.move ()
            north.leave ()
        end act
    end SouthCar

    banner ("Cross Meso IDM test")
    println ("west -> center -> east")
    println ("south -> center -> north")
    println (s"equal links: length=$linkLength m, lanes=1, shared IDM table=true")
    println (s"vehicles per route=$nStop")

    simulate ()
    waitFinished ()

    require (east.durationStat.num == nStop,
             s"east completed=${east.durationStat.num}, expected=$nStop")
    require (north.durationStat.num == nStop,
             s"north completed=${north.durationStat.num}, expected=$nStop")
    for link <- links do
        require (link.enteredCount == nStop && link.releasedCount == nStop,
                 s"${link.name} entered/released=${link.enteredCount}/${link.releasedCount}")
        require (link.occupancy == 0 && link.blockedCount == 0,
                 s"${link.name} did not drain")
    end for

    banner ("Cross Meso IDM test: PASS")
    println (s"east completed=${east.durationStat.num}, north completed=${north.durationStat.num}")
    println (s"agenda activations=$agendaActivations")
    Model.shutdown ()

end CrossMesoIdm
