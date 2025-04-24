
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
 */
class Junction (name: String, jTime: Variate, at: Array [Double], nt: Int)
    extends Component
        with Recorder(nt):

    initComponent(name, at)

    private val debug = debugf("Junction", true)
    debug("init", s"name = $name, located at ${stringOf(at)}")

    private var onJunction = 0

    // Overloaded constructor (NO DEFAULT VALUES)
    def this(name: String, jTime: Variate = Sharp(1.0), xy: (Double, Double), nt: Int) =
        this(name, jTime, Array(xy._1, xy._2, 20.0, 20.0), nt)
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
        println(s"Taking record at the Junction:$name")
        record(actor, ctime)
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
    def apply(name: String, jTime: Variate = Sharp(1.0), xy: (Double, Double), nt: Int): Junction =
        new Junction(name, jTime, xy, nt)
    end apply

    def group(jTime: Variate, xy: (Int, Int),nt:Int,
              jnt: (String, (Int, Int))*): List[Junction] =
        val junctionGroup = new VEC[Junction]()
        for j <- jnt do junctionGroup += Junction(j._1, jTime, (xy._1 + j._2._1, xy._2 + j._2._2), nt)
        junctionGroup.toList
    end group
end Junction
