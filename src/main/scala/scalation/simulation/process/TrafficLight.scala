//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishi
 *  @version 2.0
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Three-Phase Fixed-Time Traffic Signal
 */

package scalation
package simulation
package process

import scala.runtime.ScalaRunTime.stringOf
import scala.util.control.Breaks.{breakable, break}

import scalation.animation.CommandType._
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrafficLight` class models a fixed-time signal cycling through green,
 *  amber and red.  It is deliberately separate from `Gate`, which other models
 *  use as a two-state holding gate.
 *
 *  Vehicles are discharged only on the transition into green, spaced by the
 *  saturation headway.  Amber does not discharge: to model discharge that
 *  continues into amber, use effective green instead, i.e. set
 *  `greenTime = actualGreen + amberTime` and `amberTime = 0.0`.
 *
 *  @param name         the name of the signal
 *  @param director     the model/container for this signal
 *  @param line         the queue holding vehicles waiting on red
 *  @param greenTime    duration of the green phase (seconds)
 *  @param amberTime    duration of the amber phase (seconds)
 *  @param redTime      duration of the red phase (seconds)
 *  @param loc          the location of the signal (x, y, w, h)
 *  @param cycles       number of full green-amber-red cycles to run
 *  @param satHeadway   saturation discharge headway (seconds per vehicle)
 *  @param startupLoss  start-up lost time at the beginning of green (seconds)
 *  @param offset       delay before the first green, for coordinating signals
 */
class TrafficLight (name: String, director: Model, val line: WaitQueue,
                    greenTime: Double, amberTime: Double, redTime: Double,
                    loc: Array [Double], cycles: Int = 1000,
                    satHeadway: Double = 2.0, startupLoss: Double = 2.0,
                    offset: Double = 0.0)
  extends SimActor (name, director)
    with Component
    with SignalControl:

  import TrafficLight.Phase

  initStats (name)
  at = loc

  private val debug = debugf ("TrafficLight", true)
  private val flaw  = flawf ("TrafficLight")

  private var _phase = Phase.Red                       // start on red

  /** Maximum vehicles dischargeable in one green, from effective green time.
   */
  val cap: Int = (((greenTime - startupLoss) / satHeadway).toInt) max 0

  /** Full cycle length, useful for coordinating adjacent signals.
   */
  val cycleLength: Double = greenTime + amberTime + redTime

  if line == null then flaw ("init", "must have a line for vehicles waiting on red")
  if greenTime <= startupLoss then flaw ("init", "green is shorter than start-up lost time")

  debug ("init", s"name = $name, cycle = $cycleLength s, cap = $cap veh/green, " +
    s"located at ${stringOf (at)}")

  schedule (offset)

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the current phase.
   */
  def phase: Phase = _phase

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return whether vehicles are barred from proceeding.  True on amber and
   *  red.  Vehicles should test this on arrival: wait in `line` when true,
   *  pass without queueing when false.
   */
  def shut: Boolean = _phase != Phase.Green

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the duration of the current phase.
   */
  def duration: Double = _phase match
    case Phase.Green => greenTime
    case Phase.Amber => amberTime
    case Phase.Red   => redTime

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the display color for the current phase.
   */
  def phaseColor: Color = _phase match
    case Phase.Green => green
    case Phase.Amber => yellow
    case Phase.Red   => red

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Tell the animation engine to display this signal and its queue.
   */
  def display (): Unit =
    director.animate (this, CreateNode, phaseColor, Ellipse (), at)
  end display

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Discharge queued vehicles at the saturation headway, up to `cap`.
   *  The i-th vehicle is scheduled `startupLoss + i * satHeadway` after the
   *  start of green.
   */
  def release (): Unit =
    breakable {
      for i <- 0 until cap do
        if line.isEmpty then break ()
        val waiting = line.dequeue ()
        director.log.trace (this, "releases", waiting, director.clock)
        waiting.schedule (startupLoss + i * satHeadway)
      end for
    } // breakable
  end release

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Cycle green -> amber -> red for the configured number of cycles.
   */
  override def act (): Unit =
    for c <- 1 to cycles do
      for p <- Array (Phase.Green, Phase.Amber, Phase.Red) do
        _phase = p
        if p == Phase.Green then release ()
        director.animate (this, SetPaintNode, phaseColor, Ellipse (), at)
        val dur = duration
        tally (dur)
        schedule (dur)
        yieldToDirector ()
      end for
    end for
    yieldToDirector (true)
  end act

end TrafficLight


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrafficLight` companion object provides the phase type and builder
 *  methods.
 */
object TrafficLight:

  /** The three signal phases, in cycle order.
   */
  enum Phase:
    case Green, Amber, Red
  end Phase

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Create a signal using defaults for width and height.
   *  @param name         the name of the signal
   *  @param director     the model/container for this signal
   *  @param line         the queue holding vehicles waiting on red
   *  @param greenTime    duration of the green phase (seconds)
   *  @param amberTime    duration of the amber phase (seconds)
   *  @param redTime      duration of the red phase (seconds)
   *  @param xy           the (x, y) coordinates for the top-left corner
   *  @param cycles       number of full cycles to run
   *  @param satHeadway   saturation discharge headway (seconds per vehicle)
   *  @param startupLoss  start-up lost time (seconds)
   *  @param offset       delay before the first green
   */
  def apply (name: String, director: Model, line: WaitQueue,
             greenTime: Double, amberTime: Double, redTime: Double,
             xy: (Int, Int), cycles: Int = 1000,
             satHeadway: Double = 2.0, startupLoss: Double = 2.0,
             offset: Double = 0.0): TrafficLight =
    new TrafficLight (name, director, line, greenTime, amberTime, redTime,
      Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0),
      cycles, satHeadway, startupLoss, offset)
  end apply

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Create a pair of conflicting signals on a shared cycle, the second
   *  offset so that its green begins when the first turns red.
   *  @param director     the model/container for these signals
   *  @param greenTime    duration of each green phase (seconds)
   *  @param amberTime    duration of each amber phase (seconds)
   *  @param sig          per-signal info: name, line, (x, y)
   */
  def pair (director: Model, greenTime: Double, amberTime: Double,
            sig: (String, WaitQueue, (Int, Int))*): List [TrafficLight] =
    val redTime = (sig.size - 1) * (greenTime + amberTime)
    (for (s, i) <- sig.zipWithIndex yield
      apply (s._1, director, s._2, greenTime, amberTime, redTime, s._3,
        offset = i * (greenTime + amberTime))
      ).toList
  end pair

end TrafficLight
