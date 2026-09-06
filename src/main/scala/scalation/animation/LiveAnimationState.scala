package scalation.animation

import scala.collection.mutable
import CommandType.*

/** A mailbox for a live display, not an event history. All access is synchronized.
 *  Tokens born and destroyed between display frames need never be drawn.
 */
private [animation] class LiveAnimationState:
    private class Token (val create: AnimateCommand):
        var move: AnimateCommand = null
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
                    val prev = token.move
                    if prev == null || prev.color != c.color || !java.util.Arrays.equals (prev.pts, c.pts) then
                        token.move = c
                }
            case _ => structure += c
    }

    def snapshot (): (Double, Vector [AnimateCommand], Vector [(AnimateCommand, AnimateCommand)]) = synchronized {
        val commands = structure.toVector
        structure.clear ()
        (time, commands, tokens.valuesIterator.map (t => (t.create, t.move)).toVector)
    }

    def size: Int = synchronized { tokens.size + structure.size }
end LiveAnimationState
