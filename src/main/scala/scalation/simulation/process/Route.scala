

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
import scala.math.{min,abs}
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


    for i <- pathway.indices do
        val shift = calcShift2 * ((i - (numLanes - 1) / 2.0) * GAP)
        pathway(i) = new Pathway(s"${name}_$i", junc, from, to, motion, isSpeed, bend, laneShift = shift)
        subpart += pathway(i)
    end for


    initComponent(name, from.at)   // using from.at for the initComponent


    // ----------------------------------------------------------------------------

    /** Attempt to change lanes for a vehicle that is on segment `seg` of lane `l1`
     * and wants to move to adjacent lane `l2`.
     *
     * @return true if the lane-change succeeds; false otherwise
     */
    def changeLane(l1: Int, l2: Int, actor: Vehicle, seg: Int): Boolean =

        var success = abs(l1 - l2) == 1
        if !success then return success
        //if abs(l1 - l2) != 1 then return false

        val fromPath = pathway(l1)                                     // current Pathway  (lane l1)
        val toPath = pathway(l2)                                       // target Pathway   (lane l2)

        // disallow LC on first and last segment

        val nSeg = toPath.seg.length
        if seg == 0 || seg == nSeg - 1 then return false

        val safeDisp = fromPath.seg(seg).safetydist

        // vehicle behind in target lane, on same segment
        val vBehind = toPath.seg(seg).getFirst
        // vehicle ahead is the node behind’s successor (if any)
        val vAhead = if vBehind != null && vBehind.myPathNode.ahead != null then vBehind.myPathNode.ahead.elem else null

        // distance from vBehind’s bumper to the actor’s current bumper
        // bumper to bumper calculation.
        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        val gapBehind =
            if vBehind != null && actor.disp > vBehind.disp
            then actor.disp - vBehind.disp                          // bumper-to-bumper
            else safeDisp                                           // no car behind or overlap guard

        val nextSeg = min(seg + 1, toPath.seg.length - 1)
        val gapAhead =
            if vAhead != null
            then abs(vAhead.t_disp - toPath.seg(nextSeg).length)
            else safeDisp

        val gap = min(gapBehind, gapAhead)
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // update overall feasibility
        success &&= gapBehind >= safeDisp && gapAhead >= safeDisp

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // --- perform the lane change ------------------------------------------
        if success then
            fromPath.removeFromAlist(actor)                             // detach from old lane
            actor.laneID = l2                                           //update the laneID of the actor
            actor.pathInfo = toPath.seg(seg).name                       //update the pathinfo of the actor
            toPath.addToAlist(actor, vAhead)                            // insert into target lane
        end if
        success
    end changeLane
    
    //def merge


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the first vehicle in the specified lane.
     *  @param i  the index of the pathway/lane
     */
    def getFirst(i: Int): Vehicle = pathway(i).getFirst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the last vehicle in the specified lane.
     *  @param i  the index of the lane
     */
    def getLast(i: Int): Vehicle = pathway(i).getLast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the Pathway at index i.
     *  @param i  the lane index
     */
    def path(i: Int): Pathway = pathway(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of segments (same for all Pathways).
     */
    def segments: Int = pathway(0).seg.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the shared junctions.
     */
    def junctions: Array[Junction] = junc

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
        for l <- pathway do l.display()
    end display

end Route