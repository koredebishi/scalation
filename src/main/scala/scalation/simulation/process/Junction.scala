//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Nov 21 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Junction/Connector between Two Pathways (Transport, VTransport, Path, or Route)
 */

package scalation
package simulation
package process

import scala.collection.mutable.ArrayBuffer as VEC
import scala.collection.immutable.Map
import scala.runtime.ScalaRunTime.stringOf
import scala.annotation.targetName
import scalation.animation.CommandType.*
import scalation.random.{Variate, Sharp}
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Junction` class provides a connector between two pathways.
 *  Since `Lines` and `QCurves` have limitations (e.g., hard to make a loop back),
 *  a junction may be needed.  Also, may be used to model road segments connected
 *  by junctions or placements of sensors.
 *  @param name   the name of the junction
 *  @param jTime  the jump-time through the junction
 *  @param at     the location of the junction (x, y, w, h)
 *  @param nt     number of time intervals
 *  @param nl     number of lanes
 *  @param conflictQueue  optional FIFO queue that makes this junction an
 *                        exclusive conflict zone (`null` allows concurrent crossings)
 */
class Junction (name: String, jTime: Variate, at: Array [Double], nt: Int, nl: Int,
                val conflictQueue: WaitQueue = null)
    extends Component
        with Recorder(nt, nl):

    initComponent(name, at)

    private val debug = debugf("Junction", false)
    debug("init", s"name = $name, located at ${stringOf(at)}")

    private var onJunction = 0
    private var reservedCrossings = 0
    private [process] var routingTable: RoutingTable = null
    private [process] var nodeId = -1
    private [process] var control: SignalControl = null
    private [process] var outgoing: Map [Int, Component] = Map.empty

    // Overloaded constructor with (Double, Double) tuple
    def this(name: String, jTime: Variate, xy: (Double, Double), nt: Int, nl: Int) =
        this(name, jTime, Array(xy._1, xy._2, 20.0, 20.0), nt, nl, null)
    end this

    // Convenience constructor without jTime
    def this(name: String, xy: (Double, Double), nt: Int, nl: Int) =
        this(name, Sharp(1.0), Array(xy._1, xy._2, 20.0, 20.0), nt, nl, null)
    end this

    override def display(): Unit =
        director.animate(this, CreateNode, purple, Ellipse(), at)
    end display

    override def toString: String = name

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Attach the precomputed network routing policy to this junction.
     *  The node id identifies this junction in the routing table.
     *  @param table the destination-indexed routing table
     *  @param id    this junction's node identifier
     */
    private [process] def setRouting (table: RoutingTable, id: Int): Unit =
        routingTable = table
        nodeId = id
    end setRouting

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next node selected for a destination.
     *  A value of `-1` means that routing has not been configured or the
     *  destination is unreachable from this junction.
     *  @param destination the vehicle's destination node
     *  @return the selected next node, or `-1`
     */
    private [process] def nextNode (destination: Int): Int =
        if routingTable == null then -1 else routingTable.next (nodeId, destination)
    end nextNode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Attach an optional control device governing entry to this junction.
     *  @param device the control device governing this junction
     */
    private [process] def setControl (device: SignalControl): Unit = control = device

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Wait in the configured control queue when the movement is closed.
     */
    private def awaitControl (): Unit =
        if control != null && control.shut then
            control.line.director = director
            control.line.waitIn ()
        end if
    end awaitControl

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Configure the destination-indexed outgoing movements for this junction.
     *  @param movement pairs of next-node identifiers and outgoing components
     */
    private [process] def setOutgoing (movement: (Int, Component)*): Unit =
        outgoing = Map (movement*)
    end setOutgoing

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Admit, cross, and select the outgoing movement for a destination.
     *  @param destination the vehicle's destination node
     *  @return the selected outgoing component
     */
    private [process] def jumpTo (destination: Int): Component =
        awaitControl ()
        val next = nextNode (destination)
        val movement = outgoing.getOrElse (next,
            throw new IllegalStateException (s"no outgoing movement from $name to node $next"))
        jump ()
        movement
    end jumpTo

    /** Wait for exclusive access when this junction has a conflict queue.
     *  A released crossing is reserved before its FIFO waiter is scheduled, so
     *  a same-time new arrival cannot take the crossing first.
     */
    private def awaitConflictZone (actor: Vehicle): Unit =
        if conflictQueue != null then
            conflictQueue.director = director
            if onJunction + reservedCrossings > 0 || conflictQueue.nonEmpty then
                val joined = conflictQueue.waitIn ()
                require (joined, s"$name conflict queue must admit every waiting vehicle")
                require (reservedCrossings > 0,
                         s"a vehicle woken at $name must own a reserved crossing")
                reservedCrossings -= 1
            else
                conflictQueue.noWait ()
            end if
        end if
    end awaitConflictZone

    def jump(): Unit =
        val actor = director.theActor.asInstanceOf[Vehicle]
        awaitConflictZone (actor)
        val duration = jTime.gen
        val ctime = director.clock
        tally(duration)
        accum(onJunction)
        record(actor, ctime)

        // Snapshot density from the segment the vehicle just finished.
        // actor.segId is the segment index; actor.myPathway.seg(segId) is that VTransport.
        // Recorder columns identify lanes at this junction, not segment indices.
        if actor.myPathway != null && actor.segId >= 0 && actor.segId < actor.myPathway.seg.length then
            val k = actor.myPathway.seg(actor.segId).density        // veh/km per lane
            recordDensity(ctime, k, actor.laneID)

        onJunction += 1
        director.log.trace(this, s"jump for $duration", actor, director.clock)

        director.animate(actor, MoveToken, null, null, Array(at(0) + RAD, at(1) + RAD))
        actor.schedule(duration)
        actor.yieldToDirector()
        accum(onJunction)
        onJunction -= 1

        if conflictQueue != null && conflictQueue.nonEmpty then
            val waitingActor = conflictQueue.dequeue ()
            reservedCrossings += 1
            waitingActor.schedule (0.0)
        end if

    end jump

end Junction




object Junction:
    def apply(name: String, jTime: Variate, xy: (Double, Double), nt: Int, nl: Int): Junction =
        new Junction(name, jTime, xy, nt, nl)
    end apply

    /** Create a junction using integer-valued animation coordinates. */
    @targetName ("applyIntCoordinates")
    def apply(name: String, jTime: Variate, xy: (Int, Int), nt: Int, nl: Int): Junction =
        new Junction(name, jTime,
                     Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0), nt, nl)
    end apply

    def apply(name: String, xy: (Double, Double), nt: Int, nl: Int): Junction =
        new Junction(name, Sharp(1.0), xy, nt, nl)
    end apply

    def apply(name: String, xy: (Double, Double), nt: Int, nl: Int,
              conflictQueue: WaitQueue): Junction =
        new Junction(name, Sharp(1.0), Array(xy._1, xy._2, 20.0, 20.0), nt, nl,
                     conflictQueue)
    end apply

    def group(jTime: Variate, xy: (Int, Int), nt: Int, nl: Int,
              jnt: (String, (Int, Int))*): List[Junction] =
        val junctionGroup = new VEC[Junction]()
        for j <- jnt do
            val jxy = ( (xy._1 + j._2._1).toDouble, (xy._2 + j._2._2).toDouble )
            junctionGroup += Junction (j._1, jTime, jxy, nt, nl)
        junctionGroup.toList
    end group
end Junction
