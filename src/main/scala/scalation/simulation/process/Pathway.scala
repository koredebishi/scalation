//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Korede Bishi
 *  @version 2.0
 *  @date    Sat Jan 25 19:44:16 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Pathway for Modeling a Lane Consisting of Multiple Segments
 */

package scalation
package simulation
package process

import scalation.animation.CommandType._
import scalation.mathstat.VectorD
import scalation.random.Variate
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Pathway` class defines a single lane with multiple segments, connected by
 *  shared junctions.
 *  pathway:  VSource --- Segment0 --- Junction0 --- Segment1 --- Junction1 --- Segment2 --- Sink
 *  @param name     the name of the pathway
 *  @param junc     the array of junctions connecting the segments
 *  @param from     the starting component (e.g., `VSource`)
 *  @param to       the ending component (e.g., `Sink`)
 *  @param motion   the variate or dynamics model
 *  @param isSpeed  whether speed or trip-time is used
 *  @param bend     curvature of the lane
 *  @param geometry projected centerline for a single-segment pathway
 *  @param segmentGeometry projected centerline for each segment
 */
class Pathway (name: String, val junc: Array [Junction], val from: Component, val to: Component,
               motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0, 
               laneShift: VectorD = VectorD(0.0, 0.0),
               geometry: Array [(Double, Double)] = Array.empty,
               segmentGeometry: Array [Array [(Double, Double)]] = Array.empty)
    extends Component with Joinable:

    private val debug = debugf ("Pathway", true)             // debug function
    val vList = DoublyLinkedList [Vehicle]                   // one lane = one doubly linked list

    // Enhanced DLL identification for debugging
    val dllId = s"DLL_${name}_Lane"
    private def logDLLOperation(operation: String, vehicle: Vehicle, details: String = ""): Unit =
        debug(s"$operation", s"[$dllId] Vehicle ${vehicle.id} $details | DLL size: ${vList.size}")

    val points = from +: junc.toList :+ to
    val seg = Array.ofDim[VTransport](points.length - 1)
    require (segmentGeometry.isEmpty || segmentGeometry.length == seg.length,
             s"segment geometry count ${segmentGeometry.length} does not match segment count ${seg.length}")

    for i <- 0 until points.length - 1 do
        val p1 = points(i)
        val p2 = points(i + 1)
        val shift = laneShift
        
        val pointsForSegment =
            if segmentGeometry.nonEmpty then segmentGeometry(i)
            else if seg.length == 1 then geometry
            else Array.empty [(Double, Double)]
        seg(i) = new VTransport (s"${name}_seg${i}", p1, p2, motion, isSpeed, bend, shift, shift, i,
                                 pointsForSegment)
        subpart  += seg(i)                                   // add to the subpart
    end for

    // -----------jun-------------jun-----------jun------------jun
    // An array of the highway segment length
    //get the length of each road segment, add and make it an array
    // if a car is on this segment and I need the length of (behind segment and ahead segment)
    // Index of the
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
//    /** Return the number of segments (same for all Pathways).
//     */
//    def segments: Int = pathway(0).seg.length


    initComponent(name, Array())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join this lane at the START of segment `segIdx` -- what a vehicle entering
     *  from a source, a ramp or a connector does.
     *
     *  `vList` is a single file ordered by position, so a member must HAVE a
     *  position: `corridorPos` is `cumLen(segId) + disp`, and a vehicle that has
     *  not run `move()` yet still carries `segId = -1` (index -1 into `cumLen`)
     *  or, off a ramp, a `disp` measured in the ramp's frame.  Either way the
     *  vehicle is unplaceable and unreadable as a leader.
     *
     *  `VTransport.move` normally owns `segId`/`disp` and sets exactly this, but
     *  it runs AFTER the join, and every leader search in between falls in the
     *  gap.  So the position is taken here, in the one place that admits a
     *  vehicle to a lane, rather than at each call site.
     *
     *  A lane change does NOT use this: that vehicle is mid-segment and already
     *  positioned, so it calls `addToAlist` directly and keeps its `disp`.
     *
     *  @param actor   the vehicle to add
     *  @param other   the vehicle ahead of it (null if none)
     *  @param segIdx  the segment it joins at
     */
    def joinAt (actor: Vehicle, other: Vehicle, segIdx: Int): Unit =
        actor.segId = segIdx
        actor.disp  = 0.0
        addToAlist (actor, other)
    end joinAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a vehicle to this pathway's doubly linked list, at the position it
     *  already holds.  Use `joinAt` when the vehicle is entering the lane and has
     *  no position in it yet.
     *  @param actor  the vehicle to add
     *  @param other  the other vehicle (the one ahead, null if none)
     */
    def addToAlist (actor: Vehicle, other: Vehicle): Unit =
        val otherNode = if other != null then other.myPathNode.asInstanceOf [vList.Node]
        else null
//        logDLLOperation("ADD_TO_DLL", actor, s"following ${if other != null then other.name else "NONE"}")
        actor.myPathway = this
        actor.myFFConnector = null                   // not on an FFConnector
        actor.myRamp = null                          // not on a ramp
        actor.myPathNode = vList.add (actor, otherNode)
        actor.pathInfo = s"${dllId}" // Update path info with clear DLL identifier
    end addToAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove a vehicle from the correct pathway's doubly linked list.
     *  @param actor  the vehicle to remove
     */
    def removeFromAlist (actor: Vehicle): Unit =
//        logDLLOperation("REMOVE_FROM_DLL", actor)
        vList.remove (actor.myPathNode.asInstanceOf [vList.Node])
        actor.myPathNode = null
        actor.myPathway  = null
    end removeFromAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cumulative distance to the start of each segment: cumLen(i) is the sum of
     *  the lengths of segments 0 until i.  One extra entry holds the lane total.
     *  Lazy because segment lengths are assigned by the builder AFTER the Route
     *  (and so this Pathway) is constructed.
     */
    lazy val cumLen: Array [Double] =
        val c = new Array [Double] (seg.length + 1)
        for i <- seg.indices do c(i + 1) = c(i) + seg(i).length
        c
    end cumLen

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Position along this lane measured from its entry -- one frame shared by
     *  every vehicle in the lane, whatever segment it entered at.  This is what
     *  makes a gap between a mainline vehicle and a merged ramp vehicle mean
     *  anything: `t_disp` counts from each vehicle's own origin, this does not.
     *  @param segIdx  the vehicle's current segment index
     *  @param disp    its displacement within that segment
     */
    def corridorPos (segIdx: Int, disp: Double): Double = cumLen(segIdx) + disp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The nearest vehicle AHEAD of a car entering this lane at the start of
     *  segment `segIdx` -- i.e. the car it must be inserted behind in `vList`.
     *
     *  Only a merging vehicle needs this.  Car-following never does: a car
     *  already in the list reads its leader with one hop, `myPathNode.ahead`.
     *  The lookup is non-local only because `vList` spans the whole lane, so a
     *  merging car's neighbours can sit in other segments.
     *
     *  Walks `vList` from the tail (furthest upstream) toward the head and
     *  returns the first vehicle whose position is greater than the entry point.
     *
     *  It searches the DLL rather than the segment deques because a `vdeque`
     *  holds only the vehicles currently inside `VTransport.move` -- a vehicle
     *  delayed at a junction is on the road but in no deque, and a merging
     *  vehicle that could not see it would insert itself ahead of it.  The DLL
     *  never loses a vehicle, and `corridorPos` stays right across a junction
     *  (`disp` is only reset at the top of `move`, so a waiting vehicle sits
     *  exactly on the boundary).
     *
     *  Cost is the number of vehicles BEHIND the entry point, which is bounded
     *  by lane capacity, not by fleet size.
     *
     *  @param segIdx  the segment where the vehicle joins the lane
     *  @return the vehicle to merge in behind, or null when the lane is clear ahead
     */
    def mergeLeaderAt (segIdx: Int): Vehicle = leaderAt (cumLen(segIdx))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The nearest vehicle AHEAD of position `pos` in this lane -- the car to be
     *  inserted behind in `vList`.
     *
     *  `mergeLeaderAt` covers a ramp or connector, which always joins at `disp`
     *  0 of a segment.  A lane change happens MID-segment, so it must pass the
     *  changing vehicle's own `corridorPos`: searching from the segment start
     *  would return a car the vehicle has already driven past.
     *
     *  Every member of `vList` has a valid position -- see `addToAlist` -- so this
     *  reads `corridorPos` unguarded.  If that ever throws on `cumLen(-1)`, a
     *  caller joined the list without taking up a position first; fix the caller,
     *  do not add a skip here.  A silent skip would let a positionless vehicle sit
     *  in the file invisible to every leader search around it.
     *
     *  @param pos  the entry position, in the lane's `corridorPos` frame
     *  @return the vehicle to merge in behind, or null when the lane is clear ahead
     */
    def leaderAt (pos: Double): Vehicle =
        var n = vList.lastNode                               // tail = furthest upstream
        while n != null do
            val v = n.elem
            if v.corridorPos > pos then return v             // first vehicle ahead of me
            n = n.ahead
        end while
        null                                                 // nothing ahead in this lane
    end leaderAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the first vehicle in this pathway.
     */
    def getFirst: Vehicle =
        if vList.isEmpty then null else vList.head           // return first vehicle in this doubly linked list
    end getFirst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the last vehicle in this pathway.
     */
    def getLast: Vehicle =
//        println(s"I only executed up to this point and the reason is,vlistSize= ${vList.size}, ${vList.toList}")
        val car =  if vList.isEmpty then null else vList.last // return last vehicle in this doubly linked list
        car
    end getLast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the location of the first curve to be the pathway starting point.
     */
    override def at: Array[Double] =
        val xy = seg(0).at // (x,y) for the first curve end-point
        Array(xy(0), xy(1), 0.0, 0.0) // add dummy width & height

//
//    def segLength(laneId: Int , car: Vehicle): Array[Double] =
//        val pathInfo = car.pathInfo     // return the pathInfo
//        // the length of this current segment
//        // the length of the previous segment
//        //the length of the ahead segment
//        // prevSeg(lenght) current seglenght  (nextSeglenght)
//
//        Array(0.0)
//    end segLength

    def getSeglength: Int = seg.length



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the direction/turn random variate to determine next the direction.
     *  This allows an application model to select the next component.
     *  FIX - this won't work in general - seg(0) will only allow turns from first segment
     */
    def selector: Variate = seg(0).selector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the direction/turn random variate for this pathway.
     *  FIX - this won't work in general
     *  @param selectorRV  the random variate used to select the direction
     */
    def selector_= (selectorRV: Variate): Unit = seg(0).selector = selectorRV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display this pathway.
     */
    override def display (): Unit =
        for s <- seg.indices do
            val segment = seg(s)
            director.animate (segment, CreateEdge, blue, segment.curve, segment.from, segment.to,
                Array (segment.p1(0), segment.p1(1),
                    segment.pc(0), segment.pc(1),
                    segment.p2(0), segment.p2(1)))
        end for
    end display

end Pathway
