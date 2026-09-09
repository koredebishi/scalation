//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman, Korede Bishi
 *  @version 2.0
 *  @date    Sat Sep 5 2026
 *  @see     LICENSE (MIT style license file).
 *  @note    Vehicle-Based Mesoscopic Transport with FIFO and Discharge Capacity
 */

package scalation
package simulation
package process
package meso

import scala.collection.mutable.ArrayDeque

import scalation.animation.CommandType.MoveToken
import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoVTransport` class models one lane using sparse vehicle activations.
 *  Free-flow traversal makes a vehicle ready; FIFO and capacity govern departure.
 *  Finite storage blocks excess vehicles in a native FIFO `WaitQueue` and wakes
 *  one blocked vehicle whenever a departure frees one reserved slot.
 *  @param name           the name of the transport
 *  @param from_          the starting component
 *  @param to_            the ending component
 *  @param freeFlowSpeed  free-flow speed (metres/second)
 *  @param capacity       discharge capacity (vehicles/hour for this lane)
 *  @param bend           curvature of the displayed road
 *  @param shift1         display shift at the starting component
 *  @param shift2         display shift at the ending component
 *  @param storageCapacity maximum admitted vehicles, including running and ready vehicles
 */
class MesoVTransport (name: String, from_ : Component, to_ : Component,
                      val freeFlowSpeed: Double, val capacity: Double,
                      bend: Double = 0.0, shift1: VectorD = VectorD (0, 0),
                      shift2: VectorD = VectorD (0, 0),
                      val storageCapacity: Int = Int.MaxValue)
      extends Transport (name, from_, to_, null, false, bend, shift1, shift2):

    private val debug = debugf ("MesoVTransport", false)

    require (storageCapacity > 0, "mesoscopic link storage capacity must be positive")

    var length = 0.0                                            // physical road length (metres); builder/config may set it
    if length <= 0.0 then length = curve.length                 // else fall back to the drawing length, same as VTransport

    def freeFlowTime = length / freeFlowSpeed                   // seconds until readiness (live: length may be set after construction)
    val releaseHeadway = 3600.0 / capacity                     // seconds between departures

    /** Distinguish a sleeping ready vehicle from one with readiness on the agenda. */
    private class Entry (val actor: Vehicle):
        var ready = false
    end Entry

    private val vdeque = ArrayDeque [Entry] ()
    private val blocked = new WaitQueue (s"$name-blocked", from_.at.clone ())
    private var nextReleaseTime = Double.NegativeInfinity
    private var reservedSlots = 0
    private var entered = 0L
    private var released = 0L

    def occupancy: Int = vdeque.size
    def enteredCount: Long = entered
    def releasedCount: Long = released
    def blockedCount: Int = blocked.size

    /** Instantaneous density (vehicles/km for this lane), including waiting vehicles. */
    def density: Double = occupancy * 1000.0 / length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Wait outside the link until storage is available.  A released slot is
     *  reserved before its FIFO waiter is scheduled, so a same-time new arrival
     *  cannot steal it.
     */
    private def awaitAdmission (actor: Vehicle): Unit =
        if occupancy + reservedSlots >= storageCapacity || blocked.nonEmpty then
            blocked.director = director
            debug ("BLOCK", s"t=${director.clock}, actor=${actor.displayLabel}, occupancy=$occupancy, " +
                   s"reserved=$reservedSlots, blocked=${blocked.size}")
            val joined = blocked.waitIn ()
            assert (joined, "the mesoscopic entry wait queue must not bar vehicles")
            assert (reservedSlots > 0, "a woken vehicle must own a reserved storage slot")
            reservedSlots -= 1
            debug ("WAKE", s"t=${director.clock}, actor=${actor.displayLabel}, occupancy=$occupancy, " +
                   s"reserved=$reservedSlots, blocked=${blocked.size}")
        end if
        assert (occupancy + reservedSlots < storageCapacity,
                "admission must not exceed mesoscopic link storage")
    end awaitAdmission

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Traverse the link on the existing process agenda with the same vehicle.
     *  FIFO follows entry execution order, including simultaneous arrivals.
     *  A ready non-head vehicle sleeps until its predecessor releases it.
     */
    override def move (): Unit =
        val actor = director.theActor.asInstanceOf [Vehicle]
        awaitAdmission (actor)
        val entryTime = director.clock
        val entry = new Entry (actor)
        accum (occupancy)
        vdeque += entry
        entered += 1
        debug ("ADMIT", s"t=${director.clock}, actor=${actor.displayLabel}, occupancy=$occupancy, " +
               s"reserved=$reservedSlots, blocked=${blocked.size}")
        assert (occupancy + reservedSlots <= storageCapacity,
                "mesoscopic link occupancy plus reservations exceeds storage")

        actor.schedule (freeFlowTime)
        actor.yieldToDirector ()
        entry.ready = true

        if vdeque.head ne entry then
            actor.yieldToDirector ()                            // sleep until head releases
        end if
        assert (vdeque.head eq entry, "only the FIFO head may release")

        val delay = nextReleaseTime - director.clock
        if delay > 0.0 then
            actor.schedule (delay)
            actor.yieldToDirector ()
        end if

        nextReleaseTime = director.clock + releaseHeadway
        accum (occupancy)
        vdeque.removeHead ()
        released += 1
        tally (director.clock - entryTime)                     // complete link travel time
        debug ("RELEASE", s"t=${director.clock}, actor=${actor.displayLabel}, occupancy=$occupancy, " +
               s"reserved=$reservedSlots, blocked=${blocked.size}")

        // Reserve the newly freed slot before scheduling exactly one FIFO waiter.
        if blocked.nonEmpty then
            val waitingActor = blocked.dequeue ()
            reservedSlots += 1
            debug ("SPACE_RELEASE", s"t=${director.clock}, actor=${waitingActor.asInstanceOf [Vehicle].displayLabel}, " +
                   s"occupancy=$occupancy, reserved=$reservedSlots, blocked=${blocked.size}")
            waitingActor.schedule (0.0)
        end if

        // Draw only the departure.
        if director.isAnimating then
            curve.traj = 1.0
            val xy = curve.eval ()
            director.animate (actor, MoveToken, null, null, Array (xy.x, xy.y))
        end if

        // Never duplicate a readiness activation that is still on the agenda.
        if vdeque.nonEmpty && vdeque.head.ready then
            vdeque.head.actor.schedule (0.0)
        end if
    end move

end MesoVTransport
