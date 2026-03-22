//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Fri Jan 31 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Highway Abstraction for Traffic Simulation (Entry/Drive/Exit)
 */

package scalation
package simulation
package process
package builder


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Highway` class encapsulates highway driving operations.
 *  Manages entry, segment-by-segment driving, and exit.
 *  Physics-critical operations (carAhead lookup, lane change decisions) remain explicit.
 *  @param route          the Route containing pathway segments
 *  @param junctions      the array of junctions (sensors and merge points)
 *  @param numLanes       the number of lanes
 *  @param numSegments    the number of segments (junctions - 1)
 *  @param rampJoinSegs   the segment indices where ramps merge
 *  @param pemsSensorIdx  the junction indices for PeMS sensors
 */
class Highway (route: Route, junctions: Array [Junction], numLanes: Int,
               numSegments: Int, rampJoinSegs: Array [Int],
               pemsSensorIdx: Array [Int]):

    private val debug = debugf ("Highway", false)

    private val highwayLength = numSegments

    debug ("init", s"numLanes = $numLanes, numSegments = $numSegments, " +
                   s"rampJoinSegs = ${rampJoinSegs.mkString (",")}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Enter the highway — add vehicle to lane's agent list.
     *  Called at simulation start for mainline vehicles.
     *  @param car  the vehicle entering
     */
    def enter (car: Vehicle): Unit =
        val laneID = car.laneID
        val carAhead = route.pathway(laneID).getLast
        route.pathway(laneID).addToAlist (car, carAhead)
        debug ("enter", s"${car.me} entered lane $laneID")
    end enter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Enter at a specific segment (for ramp merges).
     *  @param car      the vehicle entering
     *  @param segment  the segment index to enter at
     */
    def enterAtSegment (car: Vehicle, segment: Int): Unit =
        val laneID = car.laneID
        val carAhead = route.pathway(laneID).seg(segment).getLast
        route.pathway(laneID).addToAlist (car, carAhead)
        debug ("enterAtSegment", s"${car.me} entered lane $laneID at segment $segment")
    end enterAtSegment

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Exit the highway — remove vehicle from lane's agent list.
     *  @param car  the vehicle exiting
     */
    def exit (car: Vehicle): Unit =
        route.pathway(car.laneID).removeFromAlist (car)
        debug ("exit", s"${car.me} exited from lane ${car.laneID}")
    end exit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the starting segment for a vehicle based on subtype.
     *  Mainline (subtype 0-3) starts at segment 0.
     *  Ramps (subtype 4+) start at their merge segment.
     *  @param subtype  the vehicle subtype
     */
    def getJoinSegment (subtype: Int): Int =
        if subtype <= 3 then 0                                       // mainline
        else if subtype - 4 < rampJoinSegs.length then rampJoinSegs(subtype - 4)
        else 0
    end getJoinSegment

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if a junction is a PeMS sensor (should record data).
     *  @param juncIdx  the junction index
     */
    def isSensor (juncIdx: Int): Boolean = pemsSensorIdx.contains (juncIdx)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if a junction is a merge point.
     *  @param juncIdx  the junction index
     */
    def isMergePoint (juncIdx: Int): Boolean = rampJoinSegs.contains (juncIdx)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the junction at the end of a segment.
     *  @param segment  the segment index
     */
    def junctionAfter (segment: Int): Junction =
        if segment + 1 < junctions.length then junctions(segment + 1)
        else junctions.last
    end junctionAfter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the pathway segment for a lane.
     *  @param laneID   the lane index
     *  @param segIdx   the segment index
     */
    def segment (laneID: Int, segIdx: Int): VTransport =
        route.pathway(laneID).seg(segIdx)
    end segment

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Attempt a lane change.
     *  @param car        the vehicle changing lanes
     *  @param fromLane   the current lane
     *  @param toLane     the target lane
     *  @param segment    the current segment
     */
    def changeLane (car: Vehicle, fromLane: Int, toLane: Int, segment: Int): Unit =
        route.changeLane (fromLane, toLane, car, segment)
        debug ("changeLane", s"${car.me} changed from lane $fromLane to $toLane at segment $segment")
    end changeLane

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the highway length (number of segments).
     */
    def length: Int = highwayLength

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the route.
     */
    def getRoute: Route = route

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get junctions array.
     */
    def getJunctions: Array [Junction] = junctions

end Highway


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Highway` companion object provides documentation and usage examples.
 */
object Highway:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Example of how Car.act() uses Highway (documentation only).
     *  This shows the pattern — actual implementation is in the model's Car class.
     *
     *  {{{
     *  case class Car () extends Vehicle ("c", model):
     *      override def act (): Unit =
     *          // Mainline entry
     *          if subtype <= 3 then
     *              highway.enter (this)
     *              junc(0).jump ()
     *              driveHighway ()
     *          // Ramp entry
     *          else
     *              driveRamp (ramps(subtype - 4))
     *              driveHighway ()
     *      end act
     *
     *      private def driveHighway (): Unit =
     *          val joinSeg = highway.getJoinSegment (subtype)
     *
     *          if subtype > 3 then
     *              highway.enterAtSegment (this, joinSeg)
     *              if highway.isMergePoint (joinSeg) then
     *                  junc(joinSeg).jump ()
     *          end if
     *
     *          // Segment-by-segment loop (physics-critical, cannot be encapsulated)
     *          cfor (joinSeg, highway.length) { seg =>
     *              // Car-following decision (explicit — needs current state)
     *              val carAhead = getCarAhead (this)
     *              if carAhead != null && carAhead.velocity < 0.1 * vmax then
     *                  val target = if laneID > 0 then laneID - 1 else laneID + 1
     *                  highway.changeLane (this, laneID, target, seg)
     *              end if
     *
     *              // Move on segment (ODE integration happens here)
     *              route.path(laneID).seg(seg).move ()
     *
     *              // Record at sensors
     *              val juncAfter = highway.junctionAfter (seg)
     *              if highway.isSensor (seg + 1) then
     *                  juncAfter.jump ()
     *          }
     *
     *          highway.exit (this)
     *          sink.leave ()
     *      end driveHighway
     *  end Car
     *  }}}
     */
    def usagePattern: String =
        """
        |Highway encapsulates:
        |  - enter(car)              : Add to lane's agent list
        |  - enterAtSegment(car, s)  : Add at specific segment (ramps)
        |  - exit(car)               : Remove from agent list
        |  - getJoinSegment(subtype) : Where ramps merge
        |  - isSensor(juncIdx)       : Check if PeMS sensor
        |  - isMergePoint(juncIdx)   : Check if ramp merge
        |  - changeLane(...)         : Delegate to Route
        |
        |Car.act() retains explicit control over:
        |  - cfor loop (segment iteration)
        |  - getCarAhead() lookup
        |  - Lane change decisions
        |  - segment.move() calls
        |  - junction.jump() calls
        |
        |This is intentional — physics requires explicit timing.
        """.stripMargin
    end usagePattern

end Highway
