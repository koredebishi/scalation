

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
 *  @param geometry projected centerline shared by the lane pathways
 *  @param segmentGeometry projected centerline for each pathway segment
 *  @param laneGap  pixels between adjacent drawn lanes (default 50, the original spacing)
 */
class Route (name: String, numLanes: Int, junc: Array[Junction], from: Component, to: Component,
             motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0,
             geometry: Array [(Double, Double)] = Array.empty, laneGap: Double = 50.0,
             segmentGeometry: Array [Array [(Double, Double)]] = Array.empty)
    extends Component ():


    private val debug = debugf("Route", true)     // debug function

    val pathway = Array.ofDim[Pathway](numLanes)      // create array of parallel Pathways
    private val GAP = laneGap  // pixel between lanes

    private def shiftGeometry (points: Array [(Double, Double)], shift: VectorD): Array [(Double, Double)] =
        points.map (point => (point._1 + shift (0), point._2 + shift (1)))
    end shiftGeometry

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
        val laneGeometry = geometry.map (p => (p._1 + shift (0), p._2 + shift (1)))
        val laneSegmentGeometry: Array [Array [(Double, Double)]] =
            segmentGeometry.map (points => shiftGeometry (points, shift))
        pathway(i) = new Pathway(s"${name}_$i", junc, from, to, motion, isSpeed, bend,
                                 laneShift = shift, geometry = laneGeometry,
                                 segmentGeometry = laneSegmentGeometry)
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

    /** Move a vehicle on segment `seg` from lane `l1` to the adjacent lane `l2`, at its
     *  current position.  Makes NO decision: the caller (e.g., MOBIL via `driveSegment`)
     *  has already judged the change safe and worthwhile.
     *  @return true if moved; false if the lanes are not adjacent or do not exist
     */
    def changeLane(l1: Int, l2: Int, actor: Vehicle, seg: Int): Boolean =
        if abs(l1 - l2) != 1 then return false

        val fromPath = pathway(l1) // current Pathway  (lane l1)
        val toPath = pathway(l2) // target Pathway   (lane l2)
        if fromPath == null || toPath == null then return false      // a lane does not exist

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // Where the vehicle lands in the target lane.  Every lane of a Route is
        // built from the same junction list, so the lanes share a segment layout
        // and `corridorPos` carries across unchanged.  The insertion point is the
        // first car ahead of that position in the TARGET lane's list (not a segment
        // `vdeque`, which only holds cars inside `seg.move()`).
        val vAhead = toPath.leaderAt (actor.corridorPos)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // --- perform the lane change --------------------------------------------
        fromPath.removeFromAlist(actor) // detach from old lane
        assert(actor.myPathNode == null, s"Vehicle $this not cleared before lane-change insertion!")

        actor.laneID = l2 // update laneID
        actor.pathInfo = toPath.seg(seg).name // update pathInfo
        toPath.addToAlist(actor, vAhead) // insert before vAhead in target lane
        toPath.laneChangeStat.tally (l2 - l1) // count the change where it happens (+1 / -1)
        true
    end changeLane

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Drive one vehicle along segment `seg`: this route's car-following model decides
     *  a lane change (MOBIL, `Dynamics.laneChange`), `changeLane` makes it, then the
     *  vehicle car-follows to the end of the segment.  The one verb drive loops call.
     *  @param car  the vehicle (in one of this route's lanes)
     *  @param seg  the segment index it is about to drive
     */
    def driveSegment (car: Vehicle, seg: Int): Unit =
        val dir = motion.laneChange (car, this)                       // -1, 0, +1 (decides only)
        if dir != 0 then changeLane (car.laneID, car.laneID + dir, car, seg)
        pathway(car.laneID).seg(seg).move ()                          // car-follow on the (new) lane
    end driveSegment

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Drive one vehicle along segment `seg` with the SIMPLE (pre-MOBIL) lane rule:
     *  if its leader is almost stopped (< 0.1 vmax), try one lane over (left, or
     *  right from lane 0), only when the target lane has a safety-distance gap ahead
     *  and behind; then car-follow.  Drop-in alternative to `driveSegment` (MOBIL).
     *  @param car  the vehicle (in one of this route's lanes)
     *  @param seg  the segment index it is about to drive
     */
    def driveSegmentSimple (car: Vehicle, seg: Int): Unit =

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        //  Decide: leader almost stopped -> try one lane over (the old rule)
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        val lane     = car.laneID
        val carAhead = car.leader                                     // ahead of the car, current lane
        if carAhead != null && carAhead.velocity < 0.1 * car.vmax then
            val target = if lane > 0 then lane - 1 else lane + 1      // left, or right from lane 0
            if target < pathway.length && gapsClear (car, target, seg) then
                changeLane (lane, target, car, seg)                   // move only (no decision inside)
            end if
        end if

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        //  Car-follow on the (possibly new) lane
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        pathway(car.laneID).seg(seg).move ()
    end driveSegmentSimple

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The pre-MOBIL safety test: true when lane `target` has at least the segment's
     *  `safetydist` of bumper-to-bumper gap ahead of and behind the car's position.
     *  @param car     the vehicle wanting to change
     *  @param target  the lane it wants to move into
     *  @param seg     the segment it is on
     */
    private def gapsClear (car: Vehicle, target: Int, seg: Int): Boolean =
        val toPath    = pathway(target)
        val pos       = car.corridorPos                               // same frame in every lane
        val safe      = toPath.seg(seg).safetydist                    // 20 m
        val vAhead    = toPath.leaderAt (pos)                         // first car ahead of pos in the target lane
        val vBehind   = if vAhead != null then vAhead.follower        // the car just behind pos, or
                        else toPath.getFirst                          //   nobody ahead -> the front car is behind pos
        val gapAhead  = if vAhead != null then vAhead.corridorPos - pos - Vehicle.len
                        else Double.PositiveInfinity
        val gapBehind = if vBehind != null then pos - vBehind.corridorPos - Vehicle.len
                        else Double.PositiveInfinity
        gapAhead >= safe && gapBehind >= safe
    end gapsClear

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
