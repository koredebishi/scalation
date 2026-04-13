//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jan 25 19:44:16 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Route for Modeling Multi-Stage, Multi-Lane Pathway
 */

package scalation
package simulation
package process


import scalation.mathstat.VectorD
import scala.math.{min, abs}
//import scalation.random.Variate

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Route` class defines a multi-lane route composed of multiple `Pathway`s
 *  running in parallel. Each `Pathway` is a single-lane road that shares the same
 *  `from` (entry), `to` (exit), and intermediate `junc` (junction) components.
 *
 *  @param name     the name of the route
 *  @param numLanes the number of parallel lanes (Pathways)
 *  @param junc     the shared junction components between segments
 *  @param from     the shared starting component (e.g., VSource)
 *  @param to       the shared ending component (e.g., Sink)
 *  @param motion   the dynamics model used for movement
 *  @param isSpeed  whether speed or trip-time is used
 *  @param bend     curvature of the lanes
 */
class Route (name: String, numLanes: Int, junc: Array[Junction], from: Component, to: Component,
             motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0,
             val lanesPerSeg: Array[Int] = null)
    extends Component ():


    private val debug = debugf("Route", true)     // debug function

    /** Number of segments in this route. */
    private val nSegments: Int = junc.length + 1   // from + junc.length intermediates + to = junc.length+1 points → junc.length+1-1 segs ... actually segments = intermediateJunc.length + 1

    /** Per-segment lane counts. If lanesPerSeg is null (uniform), fill with numLanes. */
    private val _lanesPerSeg: Array[Int] =
        if lanesPerSeg != null then lanesPerSeg
        else Array.fill(junc.length + 1)(numLanes)   // junc = intermediate junctions → nSegments = junc.length + 1

    /** Maximum lane count across all segments. */
    val maxLanes: Int = if _lanesPerSeg.nonEmpty then _lanesPerSeg.max else numLanes

    /** Query: how many lanes exist at segment `seg`. */
    def lanesAt (seg: Int): Int = _lanesPerSeg(seg)

    /** Query: does lane `lane` exist at segment `seg`? */
    def laneExistsAt (lane: Int, seg: Int): Boolean = lane < _lanesPerSeg(seg)

    val pathway: Array[Pathway] = Array.ofDim[Pathway](numLanes)      // create array of parallel Pathways
    private val GAP = 50.0     // pixel between lanes

    for i <- pathway.indices do
        val physicalLane = numLanes - 1 - i  // Reverse: i=0 → lane 4 (rightmost), i=4 → lane 0 (leftmost)
        val shift = calcShift2 * ((physicalLane - (numLanes - 1) / 2.0) * GAP)
        pathway(i) = new Pathway(s"${name}L$i", junc, from, to, motion, isSpeed, bend,
                                 laneShift = shift, laneIndex = i, lanesPerSeg = _lanesPerSeg)
        subpart += pathway(i)
    end for


    initComponent(name, from.at)   // using from.at for the initComponent


    // Cumulative length up to (but not including) each segment
    // segmentOffsets(0) = 0
    // segmentOffsets(1) = seg(0).length
    // segmentOffsets(2) = seg(0).length + seg(1).length
    // etc.
    val segmentOffsets: Array[Double] =
        val n = pathway(0).seg.length  // number of segments (same for all Pathways)
        val offsets = new Array[Double](n + 1)  // array to hold offsets
        offsets(0) = 0.0  // starting offset is 0.0
        for i <- 0 until n do
            // Find a non-null VTransport at this segment index (any lane will do — same geometry)
            val vt = pathway.map(_.seg(i)).find(_ != null).orNull
            offsets(i + 1) = offsets(i) + (if vt != null then vt.length else 0.0)
        end for
        offsets
    end segmentOffsets

    // O(1) helper: convert (segId, disp) → cumulative position
    @inline def toCumulative(segId: Int, disp: Double): Double =
        segmentOffsets(segId) + disp
    end toCumulative

    // ----------------------------------------------------------------------------

    /** Attempt to change lanes for a vehicle that is on segment `seg` of lane `l1`
     * and wants to move to adjacent lane `l2`.
     *
     * @return true if the lane-change succeeds; false otherwise
     */
    def changeLane(l1: Int, l2: Int, actor: Vehicle, seg: Int): Boolean =

        var success = abs(l1 - l2) == 1
        if !success then return success

        // Check if target lane exists at this segment (variable lane support)
        if !laneExistsAt(l2, seg) then return false
        // Check if source lane exists at this segment (car can't be here if lane is null)
        if !laneExistsAt(l1, seg) then return false

        val fromPath = pathway(l1) // current Pathway  (lane l1)
        val toPath = pathway(l2) // target Pathway   (lane l2)

        // Check if either lane doesn't exist
        if fromPath == null || toPath == null then return false

        val safeDisp = fromPath.seg(seg).safetydist

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // vehicles to check in target lane
        val vBehind = toPath.seg(seg).getFirst // car behind in same seg
        val vAhead = if seg + 1 < toPath.seg.length && toPath.seg(seg + 1) != null
            then toPath.seg(seg + 1).getLast else null

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // compute bumper-to-bumper gaps
        val gapBehind = if vBehind != null then actor.disp - vBehind.disp else safeDisp

        val nextSeg = min(seg + 1, toPath.seg.length - 1)

        val gapAhead =
            if vAhead != null then toPath.seg(nextSeg).length - vAhead.disp - Vehicle.len
            else safeDisp

        val gap = min(gapBehind, gapAhead)
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // feasibility check
        success &&= gapBehind >= safeDisp && gapAhead >= safeDisp

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // --- perform the lane change --------------------------------------------
        if success then
            fromPath.removeFromAlist(actor, seg) // detach from old lane at this segment
            assert(actor.myPathNode == null, s"Vehicle $this not cleared before lane-change insertion!")

            actor.laneID = l2 // update laneID
            actor.pathInfo = toPath.seg(seg).name // update pathInfo
            toPath.addToAlist(actor, vAhead, seg) // insert before vAhead in target lane at this segment
        end if

        success
    end changeLane

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Merge a ramp vehicle into the mainline at the given lane and segment.
     *  Remove from ramp, look who's ahead in the merge segment, insert behind them.
     *  IDM in Dynamics handles car-following after insertion.
     *
     *  @param lane     the target mainline lane
     *  @param seg      the merge segment index
     *  @param car      the merging vehicle (just finished driving the ramp)
     *  @param fromRamp the ramp the vehicle came from (null if direct)
     */
    def mergeFromRamp (lane: Int, seg: Int, car: Vehicle,
                       fromRamp: Ramp = null): Unit =
        val effectiveLane = if laneExistsAt (lane, seg) then lane else lanesAt (seg) - 1
        if fromRamp != null then fromRamp.removeFromAlist (car)
        car.disp = 0.0
        val vAhead = pathway(effectiveLane).seg(seg).getLast
        pathway(effectiveLane).addToAlist (car, vAhead, seg)
    end mergeFromRamp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /**
     * Lane-drop forced merge.  A lane dies (e.g. 5 lanes → 4 lanes).
     * Every vehicle in the dying lane moves into the adjacent surviving lane.
     * If there's a car ahead in that lane, slot behind it.  That's it.
     *
     * This method does the FULL job: remove from dead lane, insert into
     * surviving lane.  Caller just uses the returned laneID and moves on.
     *
     * @param l1          the vehicle's current lane (dead at this segment)
     * @param availLanes  the lanes that exist at this segment (e.g. 0 until 4)
     * @param car         the vehicle being merged
     * @param seg         the segment index where the lane drop occurs
     * @return            the new lane index
     */
    def forceMerge(l1: Int, availLanes: Range, car: Vehicle, seg: Int): Int =
        val target = availLanes.last                             // adjacent surviving lane

        // 1. Remove from dead lane (if still attached)
        if car.myPathNode != null && pathway(l1) != null && pathway(l1).seg(seg) != null then
            pathway(l1).removeFromAlist(car, seg)
        end if

        // 2. Insert into surviving lane — slot behind whoever is ahead
        val ahead = pathway(target).seg(seg).getLast
        car.laneID = target
        pathway(target).addToAlist(car, ahead, seg)

        println(f"[forceMerge] ${car.displayLabel}%-12s lane $l1 → $target at seg $seg  (lane drop)")
        target
    end forceMerge


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the first vehicle in the specified lane.
     *  @param i  the index of the pathway/lane
     */
    def getFirst(i: Int): Vehicle = if pathway(i) != null then pathway(i).getFirst else null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the last vehicle in the specified lane.
     *  @param i  the index of the lane
     */
    def getLast(i: Int): Vehicle = if pathway(i) != null then pathway(i).getLast else null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the Pathway at index i.
     *  @param i  the lane index
     */
    def path(i: Int): Pathway = pathway(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Return the shared junctions.
     */
    def junctions: Array[Junction] = junc


    /** Return the number of segments (same for all Pathways).
     */
    def segments: Int = pathway(0).seg.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** All junction points in order: from +: junc :+ to.
     *  points(seg) is the upstream junction of segment `seg`.
     */
    private val _points: Array [Component] = (from +: junc.toIndexedSeq :+ to).toArray

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perpendicular unit vector pointing outward from road centerline
     *  toward the ramp side (opposite from lane 0 / rightmost lane).
     */
    val perpVec: (Double, Double) =
        val v = calcShift2
        (-v(0), -v(1))                                          // negate: away from lane 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the screen (x, y) at the outermost lane edge of segment `seg`.
     *  Adapts to variable lane counts: more lanes → further from centerline.
     *  Points toward the ramp side (away from lane 0).
     *  @param seg  segment index (0 .. nSegments-1)
     */
    def rampAttachPoint (seg: Int): (Double, Double) =
        val pt      = _points(seg)                              // junction at segment boundary
        val nHere   = lanesAt(seg)
        val offset  = (nHere - numLanes / 2.0 - 0.4) * GAP       // slightly inside lane edge for visual snugness
        (pt.at(0) + perpVec._1 * offset,
         pt.at(1) + perpVec._2 * offset)
    end rampAttachPoint

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Calculate the amount of shift in the x and y directions.
     */
    private def calcShift2: VectorD =
        val dx = to.at(0) - from.at(0)
        val dy = to.at(1) - from.at(1)
        val hyp = math.hypot(dx, dy)
        VectorD(dy / hyp, -dx / hyp)
    end calcShift2

    /** Calculate the amount of shift in the x and y directions.
     */
    private def calcShift: VectorD =
        val xdist = from.at(0) - to.at(0)
        val ydist = from.at(1) - to.at(1)
        val hyp = math.hypot(xdist, ydist)
        VectorD((ydist / hyp) * GAP, -(xdist / hyp) * GAP)
    end calcShift

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Display the Route by displaying each Pathway.
     */
    override def display(): Unit =
        for l <- pathway if l != null do l.display()
    end display

end Route