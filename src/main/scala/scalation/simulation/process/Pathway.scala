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
 */
class Pathway (name: String, val junc: Array [Junction], val from: Component, val to: Component,
               motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0, 
               laneShift: VectorD = VectorD(0.0, 0.0))
    extends Component:

    private val debug = debugf ("Pathway", true)             // debug function
    val vList = DoublyLinkedList [Vehicle]                   // one lane = one doubly linked list

    val points = from +: junc.toList :+ to
    val seg = Array.ofDim[VTransport](points.length - 1)

    for i <- 0 until points.length - 1 do
        val p1 = points(i)
        val p2 = points(i + 1)
        val shift = laneShift
        
        seg(i) = new VTransport (s"${name}_seg${i}", p1, p2, motion, isSpeed, bend, shift, shift)
        
        subpart  += seg(i)                                   // add to the subpart
    end for

    initComponent(name, Array())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a vehicle to the correct pathway's doubly linked list.
     *  @param actor  the vehicle to add
     *  @param other  the other vehicle (the one ahead, null if none)
     */
    def addToAlist (actor: Vehicle, other: Vehicle): Unit =
        val otherNode = if other != null then other.myPathNode.asInstanceOf [vList.Node]
        else null
        debug ("addToList", s"actor = $actor follows otherNode = $otherNode")
        actor.myPathway = this
        actor.myPathNode = vList.add (actor, otherNode)
    end addToAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove a vehicle from the correct pathway's doubly linked list.
     *  @param actor  the vehicle to remove
     */
    def removeFromAlist (actor: Vehicle): Unit =
        vList.remove (actor.myPathNode.asInstanceOf [vList.Node])
        actor.myPathNode = null
        actor.myPathway  = null
    end removeFromAlist

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
        if vList.isEmpty then null else vList.last           // return last vehicle in this doubly linked list
    end getLast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the location of the first curve to be the pathway starting point.
     */
    override def at: Array[Double] =
        val xy = seg(0).at // (x,y) for the first curve end-point
        Array(xy(0), xy(1), 0.0, 0.0) // add dummy width & height

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
