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
import scala.runtime.ScalaRunTime.stringOf
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
 */
class Junction (name: String, jTime: Variate, at: Array [Double], nt: Int, nl: Int)
    extends Component
        with Recorder(nt, nl):

    initComponent(name, at)

    private val debug = debugf("Junction", false)
    debug("init", s"name = $name, located at ${stringOf(at)}")

    private var onJunction = 0

    // Overloaded constructor with (Double, Double) tuple
    def this(name: String, jTime: Variate, xy: (Double, Double), nt: Int, nl: Int) =
        this(name, jTime, Array(xy._1, xy._2, 20.0, 20.0), nt, nl)
    end this

    // Convenience constructor without jTime
    def this(name: String, xy: (Double, Double), nt: Int, nl: Int) =
        this(name, Sharp(1.0), Array(xy._1, xy._2, 20.0, 20.0), nt, nl)
    end this

    override def display(): Unit =
        director.animate(this, CreateNode, purple, Ellipse(), at)
    end display

    override def toString: String = name

    def jump(): Unit =
        val actor = director.theActor.asInstanceOf[Vehicle]
        val duration = jTime.gen
        val ctime = director.clock
        tally(duration)
        accum(onJunction)
        record(actor, ctime)

        // Snapshot density from the segment the vehicle just finished.
        // actor.segId is the segment index; actor.myPathway.seg(segId) is that VTransport.
        // segId maps directly to the column in r_density.
        if actor.myPathway != null && actor.segId >= 0 && actor.segId < actor.myPathway.seg.length then
            val k = actor.myPathway.seg(actor.segId).snapshotDensity()
            recordDensity(ctime, k, actor.segId)

        onJunction += 1
        director.log.trace(this, s"jump for $duration", actor, director.clock)

        director.animate(actor, MoveToken, null, null, Array(at(0) + RAD, at(1) + RAD))
        actor.schedule(duration)
        actor.yieldToDirector()
        accum(onJunction)
        onJunction -= 1

    end jump

end Junction




object Junction:
    def apply(name: String, jTime: Variate, xy: (Double, Double), nt: Int, nl: Int): Junction =
        new Junction(name, jTime, xy, nt, nl)
    end apply

    def apply(name: String, xy: (Double, Double), nt: Int, nl: Int): Junction =
        new Junction(name, Sharp(1.0), xy, nt, nl)
    end apply

    def group(jTime: Variate, xy: (Int, Int), nt: Int, nl: Int,
              jnt: (String, (Int, Int))*): List[Junction] =
        val junctionGroup = new VEC[Junction]()
        for j <- jnt do junctionGroup += Junction(j._1, jTime, (xy._1 + j._2._1, xy._2 + j._2._2), nt, nl)
        junctionGroup.toList
    end group
end Junction
