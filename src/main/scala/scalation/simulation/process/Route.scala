

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
import scala.math.abs
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
             motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0)
    extends Component ():


    private val debug = debugf("Route", true)     // debug function

    val pathway = Array.ofDim[Pathway](numLanes)      // create array of parallel Pathways
    private val GAP = 50.0     // pixel between lanes

    for i <- pathway.indices do
        // this for loop draws n amount of pathways based on numLanes. each of these pathways
        //consist of segment called Vtransport.
        // we want a situation where we can make this route draw. (n-1) standard pathway
        // and allow us to configure the last pathway such that that last pathway can
        //mimic the last lane of a highway. it can end and continue  at the same time.
        // something like:
        // -------------|                  |------------------|-------------------|
        val physicalLane = numLanes - 1 - i  // Reverse: i=0 → lane 4 (rightmost), i=4 → lane 0 (leftmost)
        val shift = calcShift2 * ((physicalLane - (numLanes - 1) / 2.0) * GAP)
        pathway(i) = new Pathway(s"${name}_$i", junc, from, to, motion, isSpeed, bend, laneShift = shift)
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
            offsets(i + 1) = offsets(i) + pathway(0).seg(i).length  // cumulative sum of segment lengths
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

        val fromPath = pathway(l1) // current Pathway  (lane l1)
        val toPath = pathway(l2) // target Pathway   (lane l2)

        // Check if either lane doesn't exist
        if fromPath == null || toPath == null then return false

        val safeDisp = fromPath.seg(seg).safetydist

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // Where the vehicle lands in the target lane.  Every lane of a Route is
        // built from the same junction list, so the lanes share a segment layout
        // and `corridorPos` carries across unchanged.
        val pos = actor.corridorPos

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // Neighbours in the TARGET lane, read from that lane's list.  NOT from a
        // segment `vdeque`: a deque holds only the cars currently inside
        // `seg.move()`, so it misses a neighbour one segment away and returns
        // null -- which `addToAlist` reads as "front of the whole lane".  A lane
        // change is mid-segment, so the search starts at `pos`, not at a segment
        // boundary.  With the leader in hand the follower is one hop back.
        val vAhead  = toPath.leaderAt (pos)
        val vBehind = if vAhead != null then vAhead.follower else toPath.getLast

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // Bumper-to-bumper gaps, both in the one `corridorPos` frame.  Every
        // vehicle in a lane's list has a position (see `Pathway.addToAlist`), so
        // both neighbours are read unguarded.
        val gapAhead =
            if vAhead != null then vAhead.corridorPos - pos - Vehicle.len
            else Double.PositiveInfinity

        val gapBehind =
            if vBehind != null then pos - vBehind.corridorPos - Vehicle.len
            else Double.PositiveInfinity

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // feasibility check
        success &&= gapBehind >= safeDisp && gapAhead >= safeDisp

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // --- perform the lane change --------------------------------------------
        if success then
            fromPath.removeFromAlist(actor) // detach from old lane
            assert(actor.myPathNode == null, s"Vehicle $this not cleared before lane-change insertion!")

            actor.laneID = l2 // update laneID
            actor.pathInfo = toPath.seg(seg).name // update pathInfo
            toPath.addToAlist(actor, vAhead) // insert before vAhead in target lane
        end if

        success
    end changeLane

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /**
     * A forcemerge that allows a vehicle to make mandatory lane changes,
     * as in the case of highway offramps where the vehicles that did not exit
     * must merge back into the mainline traffic.
     */
    def forceMerge(l1:Int, availLanes: Range, car: Vehicle, seg:Int): Int =

        val fromPath = pathway(l1) // current Pathway  (lane l1)
        var bestLane = -1     // initialize best lane to -1 (no best lane found for now)
        var minGap = -1.0      //fromPath.seg(seg).safetydist  // initialize minGap to a large value

        val pos = car.corridorPos    // where the car lands in whichever lane it picks

        // look for the best lane to merge into using max space availability as the criteria
        cfor(availLanes){ i =>
            if pathway(i) != null then
                // Leader from lane i's list, searched from the car's own position.
                // A segment vdeque cannot answer this: it holds only the cars
                // currently inside seg.move(), so it misses a leader one segment
                // downstream and reports the lane as wide open.
                val vAhead = pathway(i).leaderAt (pos)

                // compute the gap ahead:
                // if there is no vehicle ahead, then the gap is infinite
                val gapAhead = if vAhead == null then Double.PositiveInfinity
                else vAhead.corridorPos - pos - Vehicle.len

                if gapAhead > minGap then
                    minGap = gapAhead
                    bestLane = i
                end if
            end if
        }

        if bestLane == -1 then bestLane = availLanes.start  // no available lane found, stay in the same lane

        // we can try standard adjacent lane change first
        val adjacent = abs(bestLane - l1) == 1    // adjacent lane
        val changed = adjacent && changeLane(l1, bestLane, car, seg)   // try standard lane change
        if changed then return bestLane    // successful standard lane change

        // force insert even if safety fails (lane ends).  The safety check is
        // what is being skipped here -- the ordering is not: the car still has
        // to land in the right place in the target lane's list.
        val toPath = pathway(bestLane)
        val vAhead = toPath.leaderAt (pos)

        fromPath.removeFromAlist(car)
        car.laneID = bestLane
        car.pathInfo = toPath.seg(seg).name
        toPath.addToAlist(car, vAhead)

        bestLane
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


     // reurn the pathway of this lane


    // 90-degree unit vector to the road centre-line
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