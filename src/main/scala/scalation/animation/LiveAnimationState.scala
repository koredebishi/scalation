package scalation.animation

import scala.collection.mutable
import CommandType.*

/** A mailbox for a live display, not an event history. All access is synchronized.
 *  Tokens born and destroyed between display frames need never be drawn.
 */
private [animation] class LiveAnimationState:
    private class Token (val create: AnimateCommand):
        var move: AnimateCommand = null
        var traversalPts: Array [Double] = null
        var traversalStart = 0.0
        var traversalStop  = 0.0
    private val tokens = mutable.HashMap.empty [Int, Token]
    private val structure = mutable.ArrayBuffer.empty [AnimateCommand]
    private var time = 0.0

    def submit (c: AnimateCommand): Unit = synchronized {
        time = c.time
        c.action match
            case CreateToken  => tokens(c.eid) = new Token (c)
            case DestroyToken => tokens.remove (c.eid)
            case MoveToken =>
                tokens.get (c.eid).foreach { token =>
                    token.traversalPts = null
                    val prev = token.move
                    if prev == null || prev.color != c.color || !java.util.Arrays.equals (prev.pts, c.pts) then
                        token.move = c
                }
            case _ => structure += c
    }

    /** Plan display-only motion along a quadratic edge.  This changes no
     *  simulation state and schedules no simulation event.
     */
    def planTraversal (eid: Int, pts: Array [Double], start: Double, stop: Double): Unit = synchronized {
        require (pts != null && pts.length >= 6, "a traversal requires start, control and end coordinates")
        require (stop >= start, "a traversal must not stop before it starts")
        tokens.get (eid).foreach { token =>
            token.traversalPts   = pts.clone ()
            token.traversalStart = start
            token.traversalStop  = stop
        }
    }

    /** Return the current live scene, interpolating planned traversals at
     *  `atTime`.  The no-argument default preserves recorded-state callers.
     */
    def snapshot (atTime: Double = time):
            (Double, Vector [AnimateCommand], Vector [(AnimateCommand, AnimateCommand)]) = synchronized {
        val commands = structure.toVector
        structure.clear ()
        val tokenStates = tokens.valuesIterator.map { token =>
            val move = if token.traversalPts == null then token.move
                       else
                           val span = token.traversalStop - token.traversalStart
                           val u = if span <= 0.0 then 1.0
                                   else math.max (0.0, math.min (1.0, (atTime - token.traversalStart) / span))
                           val q = 1.0 - u
                           val p = token.traversalPts
                           val x = q * q * p(0) + 2.0 * q * u * p(2) + u * u * p(4)
                           val y = q * q * p(1) + 2.0 * q * u * p(3) + u * u * p(5)
                           AnimateCommand (MoveToken, token.create.eid, null, null, true,
                                           null, Array (x, y), atTime)
            (token.create, move)
        }.toVector
        (atTime, commands, tokenStates)
    }

    def size: Int = synchronized { tokens.size + structure.size }
end LiveAnimationState
