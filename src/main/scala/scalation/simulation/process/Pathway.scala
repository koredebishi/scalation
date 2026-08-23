//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Korede Bishi
 *  @version 2.0
 *  @date    Sat Jan 25 19:44:16 EST 2014
 *  @see     LICENSE (MIT style license file).
 *  @note    Pathway for Modeling a Lane Consisting of Multiple Segments
 *
 *
 *Pathway
 *           │
 *           ├── lane identity
 *           ├── ordered VTransport segments
 *           ├── lane existence by segment
 *           ├── longitudinal wiring
 *           │     VT0.next = VT1
 *           │     VT1.prev = VT0
 *           │     ...
 *           └── visualization / lane geometry
 *
 *
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
 */
class Pathway (name: String, val junc: Array [Junction], val from: Component, val to: Component,
               motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0, 
               laneShift: VectorD = VectorD(0.0, 0.0),
               laneIndex: Int = 0, lanesPerSeg: Array[Int] = null)
    extends Component with Joinable:

    private val debug = debugf ("Pathway", true)             // debug function

    /** Back-reference to the parent Route (set by Route constructor). */
    var parentRoute: Route = null


    val points = from +: junc.toList :+ to
    val seg = Array.ofDim[VTransport](points.length - 1)

    for i <- 0 until points.length - 1 do
        // Only create VTransport where this lane exists
        val exists = lanesPerSeg == null || laneIndex < lanesPerSeg(i)
        if exists then
            val p1 = points(i)
            val p2 = points(i + 1)
            val shift = laneShift
            seg(i) = new VTransport (s"${name}s${i}", p1, p2, motion, isSpeed, bend, shift, shift, i)
            subpart  += seg(i)
        else
            seg(i) = null                                        // lane doesn't exist at this segment
    end for


    initComponent(name, Array())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a vehicle to this pathway at a specific segment's DLL.
     *  Sets pathway/connector/ramp references, then delegates to VTransport.
     *  @param actor  the vehicle to add
     *  @param other  the vehicle ahead (null if none)
     *  @param segId  the segment index within this pathway
     */
    def addToAlist (actor: Vehicle, other: Vehicle, segId: Int): Unit =
        actor.myPathway = this
        actor.myFFConnector = null                   // not on an FFConnector
        actor.myRamp = null                          // not on a ramp
        if parentRoute != null then actor.myRoute = parentRoute
        seg(segId).addToAlist (actor, other)
    end addToAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a vehicle — backward-compatible overload (defaults to seg 0).
     *  Used by existing call sites that don't pass segId yet.
     *  @param actor  the vehicle to add
     *  @param other  the vehicle ahead (null if none)
     */
    def addToAlist (actor: Vehicle, other: Vehicle): Unit =
        addToAlist (actor, other, 0)
    end addToAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove a vehicle from the segment's DLL it currently occupies.
     *  Uses actor.segId to find the right VTransport.
     *  @param actor  the vehicle to remove
     */
    def removeFromAlist (actor: Vehicle): Unit =
        if actor.segId >= 0 && actor.segId < seg.length && seg(actor.segId) != null then
            seg(actor.segId).removeFromAlist (actor)
        end if
        actor.myPathway = null
    end removeFromAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove a vehicle from a specific segment's DLL.
     *  @param actor  the vehicle to remove
     *  @param segId  the segment index
     */
    def removeFromAlist (actor: Vehicle, segId: Int): Unit =
        seg(segId).removeFromAlist (actor)
        actor.myPathway = null
    end removeFromAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the first vehicle in this pathway (lead car across all segments).
     *  Scans from last segment backward to find the first non-empty segment's head.
     */
    def getFirst: Vehicle =
        var i = seg.length - 1
        while i >= 0 do
            if seg(i) != null then
                val f = seg(i).getFirst
                if f != null then return f
            end if
            i -= 1
        end while
        null
    end getFirst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the last vehicle in this pathway (most recently entered car).
     *  Scans from first segment forward to find the first non-empty segment's tail.
     */
    def getLast: Vehicle =
        var i = 0
        while i < seg.length do
            if seg(i) != null then
                val l = seg(i).getLast
                if l != null then return l
            end if
            i += 1
        end while
        null
    end getLast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Check whether this lane has a VTransport at the given segment index.
     *
     * @param segId the segment index to check
     */
    def existsAt(segId: Int): Boolean =
        segId >= 0 && segId < seg.length && seg(segId) != null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Return the location of the first curve to be the pathway starting point.
     * Finds the first non-null segment for sparse pathways.
     */
    override def at: Array[Double] =
        val first = seg.find(_ != null)
        if first.isDefined then
            val xy = first.get.at
            Array(xy(0), xy(1), 0.0, 0.0)
        else Array(0.0, 0.0, 0.0, 0.0)

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
     * This allows an application model to select the next component.
     * FIX - this won't work in general - seg(0) will only allow turns from first segment
     */
    def selector: Variate =
        val first = seg.find(_ != null)
        if first.isDefined then first.get.selector else null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Set the direction/turn random variate for this pathway.
     * FIX - this won't work in general
     *
     * @param selectorRV the random variate used to select the direction
     */
    def selector_=(selectorRV: Variate): Unit =
        val first = seg.find(_ != null)
        if first.isDefined then first.get.selector = selectorRV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Display this pathway — skip null segments (lane doesn't exist there).
     */
    override def display(): Unit =
        for s <- seg.indices if seg(s) != null do
            val segment = seg(s)
            director.animate(segment, CreateEdge, blue, segment.curve, segment.from, segment.to,
                Array(segment.p1(0), segment.p1(1),
                    segment.pc(0), segment.pc(1),
                    segment.p2(0), segment.p2(1)))
        end for
    end display

end Pathway
