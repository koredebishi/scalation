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
import scala.math.{min,max}
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoVTransport` class models one lane using sparse vehicle activations.
 *  Traversal of the running part makes a vehicle ready; FIFO and capacity govern departure.
 *  Finite storage blocks excess vehicles in a native FIFO `WaitQueue` and wakes
 *  one blocked vehicle whenever a departure frees one reserved slot.
 *
 *  How the `IdmFlux` table is used (derivation in `IdmFlux`, IdmFlux.scala):
 *      L = length, n_r = runningCount, n_q = queuedCount, N = occupancy, h = releaseHeadway
 *      table      -> V_e (rho)           speed read at entry             `IdmFlux.speed`
 *      table      -> h = 1 / q_max       discharge headway               `capacity` -> `releaseHeadway`
 *      table      -> rho_m               queue spacing, storage limit    `jamSpacing`, `admissionCapacity`
 *  Running part and queue, and speed from running density: Burghout (2004), pp. 52, 55-56.
 *  The steady state is a Treiber & Kesting result (2025, p. 218); using it in this link is our step.
 *  The equations are written out at `runningDensity`, `admissionCapacity` and `move`.
 *
 *  @param name           the name of the transport
 *  @param from_          the starting component
 *  @param to_            the ending component
 *  @param freeFlowSpeed  free-flow speed (metres/second)
 *  @param capacity       discharge capacity (vehicles/hour for this lane); q_max * 3600 from `Route`
 *  @param bend           curvature of the displayed road
 *  @param shift1         display shift at the starting component
 *  @param shift2         display shift at the ending component
 *  @param storageCapacity configured upper bound on admitted vehicles; physical storage
 *                         is also bounded by link length and jam density
 *  @param flowDiagram    density-sensitive equilibrium speed relation (`IdmFlux`,
 *                        derived from the corridor's own IDM parameters)
 *  @param traceSpeed     print the density and equilibrium speed for each IDM traversal
 */
class MesoVTransport (name: String, from_ : Component, to_ : Component,
                      val freeFlowSpeed: Double, val capacity: Double,
                      bend: Double = 0.0, shift1: VectorD = VectorD (0, 0),
                      shift2: VectorD = VectorD (0, 0),
                      val storageCapacity: Int = Int.MaxValue,
                      val flowDiagram: IdmFlux,
                      traceSpeed: Boolean = false,
                      geometry: Array [(Double, Double)] = Array.empty)
      extends Transport (name, from_, to_, null, false, bend, shift1, shift2, geometry), TrafficLink:

    private val debug = debugf ("MesoVTransport", false)

    require (storageCapacity > 0, "mesoscopic link storage capacity must be positive")

    def freeFlowTime = length / freeFlowSpeed                   // seconds until readiness (live: length may be set after construction)
    val releaseHeadway = 3600.0 / capacity                     // h = 3600 / capacity = 1 / q_max: seconds between departures (1.377 s at 2615 veh/h)

    /** Distinguish a sleeping ready vehicle from one with readiness on the agenda. */
    private class Entry (val actor: Vehicle):
        var ready = false
        var speed = 0.0                                         // speed assigned for crossing the running part (m/s)
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

    /** Whether a vehicle arriving now would be admitted without waiting. */
    def hasRoom: Boolean = occupancy + reservedSlots < admissionCapacity && blocked.isEmpty

    /** The speed a vehicle with desired speed `vmax` would be given if it entered now
     *  (same rule as `move`: IdmFlux at the running density, v0 = min (road, car)). */
    def entrySpeed (vmax: Double): Double =
        flowDiagram.withV0 (min (flowDiagram.v0, vmax)).speed (runningDensity)
    end entrySpeed

    /** Number of vehicles on this link right now (running + queued). */
    def vehicleCount: Int = occupancy

    /** Space-mean speed on this link: each running vehicle at its assigned crossing
     *  speed, queued vehicles at 0 (stopped vehicles belong in the average, as in `VTransport`).
     *  @return space-mean speed in m/s (0.0 when the link is empty)
     */
    def spaceMeanSpeed: Double =
        if vdeque.isEmpty then 0.0
        else
            var sum = 0.0
            for e <- vdeque if ! e.ready do sum += e.speed
            sum / vdeque.size
    end spaceMeanSpeed

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

    /** Physical spacing at the jam density specified by the IDM diagram: 1 / rho_m = s0 + len. */
    private def jamSpacing: Double = 1.0 / flowDiagram.rho_m

    /** Length occupied by vehicles waiting for link discharge: n_q / rho_m. */
    private def queueLength: Double = queuedCount * jamSpacing

    /** Vehicles this link can hold: its length L over the IDM standstill spacing s0 + len
     *  (rho_m = 1 / (s0 + len), the jam density), never below one, and never above the
     *  configured `storageCapacity` (unset in the corridor models, so L rho_m decides).
     *      N_max = min (N_store, max (floor (L rho_m), 1)),  e.g. 156 m / (2 + 4) m = 26 vehicles.
     */
    private def admissionCapacity: Int =
        min (storageCapacity, (length * flowDiagram.rho_m).toInt.max (1))
    end admissionCapacity

    /** Density on the moving part of this link (vehicles/metre).
     *     L_r   = L - n_q / rho_m            the queue takes s0 + len metres per vehicle
     *     rho_r = 0                          if n_r = 0
     *             rho_m                      if L_r <= 1 / rho_m (no room for one vehicle)
     *             n_r / L_r                  otherwise
     * n_r / L_r can exceed rho_m; `IdmFlux.speed` then reads it as rho_m.
     * Read at entry, before the entering vehicle is added, so that vehicle is not counted.
     * */
    private def runningDensity: Double =
        val running = length - queueLength                          // the running (moving) part of the link
        if runningCount == 0 then 0.0
        else if running <= jamSpacing then flowDiagram.rho_m        // queue fills the link: jam density
        else runningCount / running
    end runningDensity

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
     *  how the flow diagram helps speed calculation:
     *
     *  1. Storage: enter only if N + reserved < N_max (`awaitAdmission`).
     *  2. Density at entry: rho = rho_r (`runningDensity`), L_r = max (L - n_q / rho_m, 0) (`runningLength`).
     *  3. Speed from the table, with v0 = min (road v0, car's vmax):
     *         u_i = V_e (rho_r)              `flowDiagram.withV0 (v0).speed (rho)`
     *  4. Crossing.  u_i is constant, so dv_i/dt = 0 and dx_i/dt = u_i integrate exactly:
     *         x_i (t) = x_i (t_in) + u_i (t - t_in),  which takes L_r / u_i = `fluxTime`
     *     This one event replaces the ballistic steps of the micro model.
     *  5. Limit on the crossing, which is our addition and not from the IDM.  A jam empties from
     *     its front one vehicle every h (Newell, 1993), so the N vehicles ahead take N h:
     *         `queueTime` = L_r / vmax + N h
     *         t_cross     = min (L_r / u_i, L_r / vmax + N h) = `traversalTime`
     *     This uses the car's own vmax, even if vmax is above the road's v0.
     *  6. Leaving: the vehicle waits to be the FIFO head and for h after the previous release:
     *         t_out = max (t_in + t_cross, t_out_prev + h)
     *  7. Speed on exit, averaged over the whole link: v_bar = L / (t_out - t_in) = `actor.velocity`.
     */
    override def move (): Unit =

        val actor = director.theActor.asInstanceOf [Vehicle]
        awaitAdmission (actor)          // wait until storage is available 
        val entryTime = director.clock


        //::::::::::::::::::::::::::::::::::::::::::::::::::::
        //Once a vehicle enters a link, we need a snapshot of moving
        //vehicle on the link / the meters they have to travel
        // return 0 if nothing is moving and rho_max if at full running.
        val rho = runningDensity               
        val runningLength = max (length - queueLength, 0.0)    // The meter this vehicle has to drive before getting to the back of queue.

        // our v0 is a choice between the vmax of the vehicle or the v0 we used in the table.
        val v0    = min (flowDiagram.v0, actor.vmax)                // the car's desired speed, never above the road's
        val speed = flowDiagram.withV0 (v0).speed (rho)             // IDM steady state with that v0 (same v0 as micro's IDM)

        val fluxTime  = if runningLength <= 0.0 then 0.0            // queue fills the link: no running part, join the queue
                        else if speed <= 0.0 then Double.PositiveInfinity   // cannot cross at zero speed: the min picks queueTime
                        else runningLength / speed                  // time (s) to cross the running part
        val queueTime = runningLength / actor.vmax + occupancy * releaseHeadway   // cars ahead leave at capacity: a jam empties from its front
        val traversalTime = min (fluxTime, queueTime)

        val entry = new Entry (actor)     
        entry.speed = if traversalTime > 0.0 then runningLength / traversalTime else 0.0   // derived: the speed this car crosses at

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

        actor.lastLink = this                                   // the junction ahead reads this road's density
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
//    running density  // measure the running desity: rho_r = n_r / L_r
//      ↓
//    IdmFlux.speed(rho)   // get the equilibrium speed from the flow diagram: V_e (rho_r)
//    ↓
//    equilibrium speed     // the speed that the vehicle will travel at: u_i
//      ↓
//    running length / speed   // get the time it will take to cross the running part: L_r / u_i
//      ↓
//    min with L_r / vmax + N h  // limit from the queue discharging at capacity: t_cross
//      ↓
//    wait for FIFO head and h  // t_out = max (t_in + t_cross, t_out_prev + h)
//      ↓
//    vehicle link time  // t_out - t_in, recorded with `tally`; exit speed L / (t_out - t_in)


