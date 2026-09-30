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
import scalation.dynamics.IdmFlux
import scalation.mathstat.VectorD
import scala.math.min
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
 *  @param storageCapacity configured upper bound on admitted vehicles; with a flow diagram,
 *                         physical storage is also bounded by link length and jam density
 *  @param flowDiagram    optional density-sensitive equilibrium speed relation (`IdmFlux`,
 *                        derived from the corridor's own IDM parameters).  When absent, traversal
 *                        speed remains the fixed `freeFlowSpeed`, unchanged from prior behavior.
 *  @param traceSpeed     print the density and equilibrium speed for each IDM traversal
 */
class MesoVTransport (name: String, from_ : Component, to_ : Component,
                      val freeFlowSpeed: Double, val capacity: Double,
                      bend: Double = 0.0, shift1: VectorD = VectorD (0, 0),
                      shift2: VectorD = VectorD (0, 0),
                      val storageCapacity: Int = Int.MaxValue,
                      val flowDiagram: Option [IdmFlux] = None,
                      traceSpeed: Boolean = false,
                      geometry: Array [(Double, Double)] = Array.empty)
      extends Transport (name, from_, to_, null, false, bend, shift1, shift2, geometry):

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

    private val vdeque = ArrayDeque [Entry] ()   // Array deque of vehicle 
    private val blocked = new WaitQueue (s"$name-blocked", from_.at.clone ())     // FIFO queue for blocked vehicles
    private var nextReleaseTime = Double.NegativeInfinity
    private var reservedSlots = 0
    private var entered = 0L
    private var released = 0L

    def occupancy: Int = vdeque.size
    def enteredCount: Long = entered
    def releasedCount: Long = released
    def blockedCount: Int = blocked.size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset link-owned runtime state between independent model replications.
     *  A completed replication must have drained the link and its admission queue.
     */
    override def resetForReplication (): Unit =
        assert (vdeque.isEmpty, s"$name retains ${vdeque.size} vehicles at replication boundary")
        assert (blocked.isEmpty && reservedSlots == 0,
                s"$name retains blocked vehicles or slot reservations at replication boundary")
        nextReleaseTime = Double.NegativeInfinity
        entered = 0L
        released = 0L
    end resetForReplication

    /** Number of vehicles still traversing this link. */
    def runningCount: Int = vdeque.count (e => ! e.ready)

    /** Number of vehicles that have reached the downstream queue. */
    def queuedCount: Int = vdeque.count (_.ready)

    /** Physical spacing at the jam density specified by the IDM diagram. */
    private def jamSpacing: Double = flowDiagram.map (fd => 1.0 / fd.rho_m).getOrElse (0.0)

    /** Length occupied by vehicles waiting for link discharge. */
    private def queueLength: Double = queuedCount * jamSpacing

    /** Physical storage limit for this link, bounded by its own length.
     * Compute the max physical vehicle that can fit on this Vtransport.
     * by choosing between the: Storage Cap, and the length of the Vtransport * jam density.
     * While ensuring the vehcle can hold at min 1 vehicle.
     * If no flow diagram is provided, we use the configured storage capacity.
     *
     * */
    private def admissionCapacity: Int =
        flowDiagram match
            case Some (fd) => min (storageCapacity, (length * fd.rho_m).toInt.max (1))
            case None      => storageCapacity  // the free-flow speed counterpath.
    end admissionCapacity

    /** Density on the moving part of this link (vehicles/metre).
     *
     *
     * */
    private def runningDensity: Double =
        if runningCount == 0 then 0.0
        else runningCount / (length - queueLength)
    end runningDensity

    /** Instantaneous density over the whole link (vehicles/km for this lane).
     *   k = n / L
     *  where n is the number of vehicles occupying the link right now and L
     *  is its length. For example, 10 vehicles on a 500 m link:
     * 10 / 0.5 km = 20 vehicles/km
     * */
    def density: Double = occupancy * 1000.0 / length          // diving by length in metres gives density in veh/km for this lane

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Wait outside the link until storage is available.  A released slot is
     *  reserved before its FIFO waiter is scheduled, so a same-time new arrival
     *  cannot steal it.
     */
    private def awaitAdmission (actor: Vehicle): Unit =
        val linkCapacity = admissionCapacity
        if occupancy + reservedSlots >= linkCapacity || blocked.nonEmpty then
            blocked.director = director      // ensure the blocked queue has a director
            debug ("BLOCK", s"t=${director.clock}, actor=${actor.displayLabel}, occupancy=$occupancy, " +
                   s"reserved=$reservedSlots, capacity=$linkCapacity, blocked=${blocked.size}")
            val joined = blocked.waitIn ()       // wait until a reserved slot is available
            assert (joined, "the mesoscopic entry wait queue must not bar vehicles")
            assert (reservedSlots > 0, "a woken vehicle must own a reserved storage slot")
            reservedSlots -= 1
            debug ("WAKE", s"t=${director.clock}, actor=${actor.displayLabel}, occupancy=$occupancy, " +
                   s"reserved=$reservedSlots, blocked=${blocked.size}")
        end if
        assert (occupancy + reservedSlots < linkCapacity,
                "admission must not exceed mesoscopic link storage")
    end awaitAdmission

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Traverse the link on the existing process agenda with the same vehicle.
     *  FIFO follows entry execution order, including simultaneous arrivals.
     *  A ready non-head vehicle sleeps until its predecessor releases it.
     *  how the flow diaram helps speed calculation:
     *
     *
     */
    override def move (): Unit =
        val actor = director.theActor.asInstanceOf [Vehicle]
        awaitAdmission (actor)          // wait until storage is available 
        val entryTime = director.clock
        val rho = flowDiagram.map (_ => runningDensity).getOrElse (0.0)
        val traversalTime = flowDiagram match
            case Some (fd) =>
                val speed = fd.speed (rho)         // use the flow diagram to determine the speed at the current density
                if traceSpeed then
                    println (s"IDM_FLUX_SPEED vehicle=${actor.displayLabel}, link=$name, " +
                             s"density=$rho veh/m, speed=$speed m/s")
                length / speed                     // convert speed to time (seconds) for the current vehicle
            case None => freeFlowTime             // use the fixed free-flow speed when no flow diagram is provided
        val entry = new Entry (actor)     
        accum (occupancy)                        // existing time-persistent occupancy statistic
        vdeque += entry                          // add the vehicle to the end of the FIFO
        entered += 1                             // increment vehicle count on this Vtranport. need to keep track of this for the statistics
        debug ("ADMIT", s"t=${director.clock}, actor=${actor.displayLabel}, occupancy=$occupancy, " +
               s"reserved=$reservedSlots, blocked=${blocked.size}")
        assert (occupancy + reservedSlots <= admissionCapacity,
                "mesoscopic link occupancy plus reservations exceeds storage")
        director.animateTraversal (actor, this, traversalTime)        // animate the vehicle's traversal of this Vtransport
        actor.schedule (traversalTime)                               // schedule the vehicle's next activation after the traversal time
        actor.yieldToDirector ()                                     // yield to the director until the traversal time has passed
        entry.ready = true                                           // mark the vehicle as ready to be released

        //A vehicle can't release until it is the head of the FIFO and the release headway has passed.
        //this way a vehicle can't overtake another vehicle that is ahead of it in the FIFO, 
        // even if it is ready to release.
        if vdeque.head ne entry then
            actor.yieldToDirector ()                            // sleep until head releases
        end if
        assert (vdeque.head eq entry, "only the FIFO head may release") // sanity check

        val delay = nextReleaseTime - director.clock           // seconds until next release
        if delay > 0.0 then
            actor.schedule (delay)
            actor.yieldToDirector ()
        end if

        nextReleaseTime = director.clock + releaseHeadway         // schedule next release time
        accum (occupancy)                                         // 
        vdeque.removeHead ()
        released += 1
        val linkTime = director.clock - entryTime               // time on this link: traversal + FIFO wait + headway
        tally (linkTime)                                        // complete link travel time
        actor.velocity = length / linkTime                      // leave the link at the speed averaged across it
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


//So the complete runtime path is:
//
//    vehicles currently on link
//
//    running density  // measure the running desity
//      ↓
//    IdmFlux.speed(rho)   // get the equilibrium speed from the flow diagram
//    ↓
//    equilibrium speed     // the speed that the vehicle will travel at
//      ↓
//    link length / speed   // get the time it will take to traverse the link at that speed
//      ↓
//    vehicle traversal time  // vehicle travel time on the Vtransport
//
//    If flowDiagram is None, the link uses its
//  fixed free-flow traversal time instead.


