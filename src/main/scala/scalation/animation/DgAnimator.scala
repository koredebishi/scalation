//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep 14 14:15:51 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Animation Engine for Animating Graphs
 */

package scalation
package animation

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.{JButton, JToolBar, JComponent, JLabel}
import javax.swing.KeyStroke
import java.awt.BorderLayout
import scala.math.round
import scala.util.control.Breaks.{break, breakable}
import scalation.scala2d.*
import scalation.scala2d.Colors.*
import CommandType.*
import scala.collection.mutable.ArrayBuffer


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DgAnimator` class is an animation engine for animating graphs.
 *  For example, it can animate bipartite graphs to animate Petri Nets.
 *  @param _title    the title for the display frame
 *  @param fgColor   the foreground color
 *  @param bgColor   the background color
 *  @param aniRatio  the ratio of simulation speed vs. animation speed
 *  @param width     the width of the animation panel
 *  @param height    the height of the animation panel
 */
class DgAnimator (_title: String, fgColor: Color = black, bgColor: Color = white,
                  aniRatio: Double = 1.0,  width: Int = 800, height: Int = 800)
      extends VizFrame (_title, null, width, height)
         with Runnable:

    /** The debug function
     */
    private val debug = debugf ("DgAnimator", false)

    /** Clock for animation engine
     */
    private var clock = 0.0

    private var actorCount = 0        // count of vehicles to be used by the run method to update actors counts real time

    private var totalActorCount = 0    // totalActor counts to be used by the Model to track the total produced actors for animation purpose

    /** Width and height for the clock
     */
    private val clockWH = (20, 30)

    /** Stop time for animation engine
     */
    private var stopTime = 0.0

    /** Graph to animate
     */
    private val graph = new Dgraph ("Animated_Graph")

    /** Shared queue holding animation commands
     */    
    private val cmdQ = new ConcurrentLinkedQueue [AnimateCommand] ()

    /** Animation command processor
     */
    private val ani = new Animator (graph)

    /** Flag to indicate that the animation is complete
     */
    private var aniDone = false
 
     //================================================================================
     // Recording and Replay
     //================================================================================
     /** Lightweight frame wrapper for executed animation commands. */
     private case class Frame(cmd: AnimateCommand)

    /** Buffer of executed animation commands for replay. */
    private val frames = new ArrayBuffer[Frame](1 << 18)   // ~262k capacity

    /** Whether a replay is currently running (prevents concurrent replays). */
    private val replaying = new AtomicBoolean(false)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear all recorded frames from the last run.
      * Use before starting a new simulation to avoid mixing runs.
      */
    def clearRecording(): Unit = frames.clear()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the scene to an empty canvas for a clean replay.
      * Clears graph containers and resets counters/clock (UI only).
      */
    private def resetSceneForReplay(): Unit =
        graph.nodes.clear()
        graph.edges.clear()
        graph.freeTokens.clear()
        actorCount = 0
        clock = 0.0
        repaint()
    end resetSceneForReplay

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Replay the last recorded animation without re-running the simulation.
      * Respects current aniRatio and Animator.timeDilationFactor to pace frames.
      * - No interaction with the live command queue; draws directly via invokeCommand.
      * - Safe to invoke multiple times; ignored if already replaying or nothing recorded.
      */
    def replay(): Unit =
        if frames.isEmpty || replaying.get() then return
        new Thread(() =>
            replaying.set(true)
            resetSceneForReplay()
            val td    = ani.timeDilationFactor
            var lastT = frames.head.cmd.time
            var i     = 0
            while i < frames.length && replaying.get() do
                val f  = frames(i).cmd
                val dt = math.max(0.0, (f.time - lastT) * aniRatio * td)
                if dt > 0 then Thread.sleep(math.round(dt))
                invokeCommand(f)
                if f.action == CreateToken then actorCount += 1
                repaint()
                lastT = f.time
                i += 1
            end while
            replaying.set(false)
        ).start()
    end replay

     //================================================================================
     // Playback Rate Control
     //================================================================================
     private var speedFactor = 1.0                     // user-facing speed multiplier (1.0x default)
     private val speedLbl    = new JLabel("Speed 1.00x")
     private val slowerBtn   = new JButton("–")       // halve speed (slower animation)
     private val fasterBtn   = new JButton("+")       // double speed (faster animation)
     private val resetBtn    = new JButton("1x")      // reset to 1.0x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the playback speed multiplier and update Animator time dilation.
      * Mapping: dilation (>1 slows, <1 speeds) = 1.0 / speedFactor.
      * @param speed  desired speed multiplier (clamped to [0.25, 8.0])
      * @since 2025-09-18  Added playback rate control.
      */
    private def setSpeedFactor(speed: Double): Unit =
        val s  = math.max(0.25, math.min(8.0, speed))      // clamp for stability
        speedFactor = s
        // Convert user speed to animator dilation factor
        val dilation = 1.0 / s
        ani.timeDilation(Array(dilation))                   // update underlying animator
        speedLbl.setText(f"Speed ${s}%.2fx")               // update UI label
     end setSpeedFactor
 
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Set the animation complete flag to true.
      */
     def setAniDone () = aniDone = true

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save the graphics into an image file.
     *  @param fname  the file name
     */
    def saveImage (fname: String): Unit = writeImage (fname, this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The canvas Panel is used to place shapes in the drawing region.
     */
    class Canvas
          extends ZoomablePanel:

        private val fsize = 18    // was originally @ 12; increased to 18.
        private val f     = new Font ("Serif", Font_BOLD, fsize)

        /** Overlay a small PAUSED stamp (optional visual cue).
          * @since 2025-09-16  Added along with Play/Pause controls.
          */
        private def drawPausedOverlay(g2d: Graphics2D): Unit =
            if paused.get() then
                g2d.setPaint (red)
                g2d.drawString ("PAUSED", getW - 100, 30)
            end if
        end drawPausedOverlay

        /** Draw a token label centered on the token shape.
          * @param g2d   hi-res graphics context used by the animator canvas
          * @param token the token whose label should be rendered (uses token.label)
          * Notes:
          * - Font size adapts to token height for readability (clamped 10..24).
          * - Centers text over the token "head"; does not rotate with heading.
          * - Used by: Canvas.paintComponent after each token is filled (node-bound,
          *   edge-bound, and free tokens).
          * @since 2025-09-16  Documented; rendering logic unchanged.
          */
        private def drawTokenLabel (g2d: Graphics2D, token: graph.Token): Unit =
            val lbl = token.label
            if lbl != null && lbl.nonEmpty then
                val b      = token.shape.getBounds2D
                val size   = math.max(10, math.min(24, (b.getHeight * 0.8).toInt))
                val prevF  = g2d.getFont
                val dynF   = new Font ("SansSerif", Font_BOLD, size)
                g2d.setFont (dynF)
                val fm     = g2d.getFontMetrics
                val x      = (b.getCenterX - fm.stringWidth(lbl) / 2.0).toFloat
                val y      = (b.getCenterY + fm.getAscent / 3.5).toFloat
                g2d.setPaint (black)
                g2d.drawString (lbl, x, y)
                g2d.setFont (prevF)
            end if
        end drawTokenLabel

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Paint the display panel component.
         *  @param gr  low-resolution graphics environment
         */
        override def paintComponent (gr: Graphics): Unit =
            super.paintComponent (gr)
            val g2d = gr.asInstanceOf [Graphics2D]            // use hi-resolution

            g2d.setTransform (at)                             // used for zooming @author Casey Bowman

            //:: Display the animation clock

            g2d.setFont (f)
            g2d.setPaint (fgColor)
            //g2d.drawString ("CLOCK = " + "%10.3f".format(clock), clockWH._1, getH - clockWH._2)

            g2d.drawString(f"CLOCK = $clock%10.3f", clockWH._1, getH - clockWH._2)
            //g2d.drawString(s"ACTORS = $actorCount" , baseX, baseY + 20)
            g2d.drawString(s"ACTORS = $actorCount / $totalActorCount", clockWH._1, getH - clockWH._2 - 20)

            // Optional PAUSED overlay
            drawPausedOverlay(g2d)

            //:: Display all nodes in graph and tokens bound to these nodes.
            debug ("paintComponent", s"paint ${graph.nodes.length} nodes")
            val nodes = graph.nodes.toList                                         // avoid ConcurrentModificationException
            for node <- nodes do
                g2d.setPaint (node.color)
                g2d.fill (node.shape)
                g2d.setPaint (black)
                g2d.draw (node.shape)
                val x = node.shape.getCenterX ().asInstanceOf [Float]              // - 20.0f
                val y = node.shape.getBounds2D.getMaxY ().asInstanceOf [Float]     // + 12.0f
                g2d.drawString (node.label, x, y)
                val node_tokens = node.tokens.toList             // copy to avoid ConcurrentModificationException
                for token <- node_tokens do
                    g2d.setPaint (token.color)
                    g2d.fill (token.shape)
                    drawTokenLabel (g2d, token)                  // render vehicle number/label on token head
                end for
            end for

            //:: Display all edges in graph and tokens bound to these edges.
            debug ("paintComponent", s"paint ${graph.edges.length} edges")
            val edges = graph.edges.toList
            for edge <- edges do
                g2d.setPaint (edge.color)
                g2d.draw (edge.shape)
                val x = edge.shape.getCenterX.asInstanceOf [Float]                 // - 30.0f
                val y = edge.shape.getCenterY.asInstanceOf [Float]
                g2d.drawString (edge.label, x, y)
                val edge_tokens = edge.tokens.toList             // copy to avoid Exception
                for token <- edge_tokens if token.shape.getWidth () > 0.0 do
                    g2d.setPaint (token.color)
                    g2d.fill (token.shape)
                    drawTokenLabel (g2d, token)                  //NEW render vehicle number/label on token head
                end for
            end for

            //:: Display all free tokens in the graph.
            debug ("paintComponent" , s"paint ${graph.freeTokens.length} free tokens")
            val free_tokens = graph.freeTokens.toList            // copy to avoid Exception
            for token <- free_tokens if token.shape.getWidth () > 0.0 do
                g2d.setPaint (token.color)
                g2d.fill (token.shape)
                drawTokenLabel (g2d, token)                      //NEW render vehicle number/label on token head
            end for
        end paintComponent

    end Canvas

    //================================================================================
    // Playback controls (Play/Pause/Step)
    //================================================================================
    /** Thread-safe flags for playback control.
      * @since 2025-09-16  Added Play/Pause/Step controls to DgAnimator.
      */
    private val paused    = new AtomicBoolean(false)  // true => run loop waits
    private val stepOnce  = new AtomicBoolean(false)  // true => process one command, then re-pause
    private val pauseLock = new Object()              // monitor for wait/notify

    /** UI: small toolbar buttons for playback control.
      * @since 2025-09-16  Added toolbar with Play/Pause and Step.
      */
    private val playPauseBtn = new JButton("Pause")   // toggles to "Play" when paused
    private val stepBtn      = new JButton("Step")
    private val replayBtn    = new JButton("Replay")

    /** Build a minimal toolbar and keyboard shortcuts.
     *  - Space toggles Play/Pause, N steps once when paused.
     *  @since 2025-09-16  New.
     */
    private def buildControls(): Unit =
        val bar = new JToolBar()
        bar.setFloatable(false)
        playPauseBtn.addActionListener(_ => togglePause())
        stepBtn.addActionListener(_ => step())
        replayBtn.setToolTipText("Replay the last run without re-simulating")
        replayBtn.addActionListener(_ => replay())
        bar.add(playPauseBtn)
        bar.add(stepBtn)
        bar.add(replayBtn)
        // Speed controls: – 1x +  and live label
        slowerBtn.setToolTipText("Slow down (halve speed)")
        fasterBtn.setToolTipText("Speed up (double speed)")
        resetBtn.setToolTipText("Reset speed to 1.0x")
        slowerBtn.addActionListener(_ => setSpeedFactor(speedFactor / 2.0))
        resetBtn.addActionListener(_ => setSpeedFactor(1.0))
        fasterBtn.addActionListener(_ => setSpeedFactor(speedFactor * 2.0))
        bar.add(slowerBtn)
        bar.add(resetBtn)
        bar.add(fasterBtn)
        bar.add(speedLbl)
        getContentPane().add(bar, BorderLayout.NORTH)

        // Keyboard shortcuts
        val rp = getRootPane()
        rp.registerKeyboardAction(_ => togglePause(), KeyStroke.getKeyStroke("SPACE"), JComponent.WHEN_IN_FOCUSED_WINDOW)
        rp.registerKeyboardAction(_ => step(),        KeyStroke.getKeyStroke("N"),     JComponent.WHEN_IN_FOCUSED_WINDOW)
        // Optional: map R to replay
        rp.registerKeyboardAction(_ => replay(),      KeyStroke.getKeyStroke("R"),     JComponent.WHEN_IN_FOCUSED_WINDOW)
        // Initialize speed to 1.0x
        setSpeedFactor(1.0)
    end buildControls

    /** Pause the animation thread.
      * @since 2025-09-16  New.
      */
    private def pause(): Unit =
        if paused.compareAndSet(false, true) then playPauseBtn.setText("Play")
    end pause

    /** Resume the animation thread.
      * @since 2025-09-16  New.
      */
    private def resume(): Unit =
        if paused.compareAndSet(true, false) then
            playPauseBtn.setText("Pause")
            pauseLock.synchronized { pauseLock.notifyAll() }
        end if
    end resume

    /** Toggle between Play and Pause.
      * @since 2025-09-16  New.
      */
    private def togglePause(): Unit = if paused.get() then resume() else pause()

    /** Step a single command when paused, then re-pause.
      * @since 2025-09-16  New.
      */
    private def step(): Unit =
        if paused.get() then
            stepOnce.set(true)
            pauseLock.synchronized { pauseLock.notifyAll() }
        end if
    end step

    {
        getContentPane ().add (new Canvas)
        buildControls()                       // mount playback controls toolbar and shortcuts
        setVisible (true)
        setBackground (bgColor)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invoke the animation command.
     *  @param c  the animation command to invoke
     */
    private def invokeCommand (c: AnimateCommand): Unit =
        if c.action != MoveToken then                                          // remove if to see all move steps
            println (s"DgAnimator.invokeCommand: $c")
        end if

        c.action match
        case CreateNode =>
            ani.createNode (c.eid, c.shape.asInstanceOf [RectPolyShape], c.label, c.primary, c.color, c.pts)
//          ani.createNode (c.eid, c.shape.asInstanceOf [RectangularShape], c.label, c.primary, c.color, c.pts)
        case CreateEdge =>
            ani.createEdge (c.eid, c.shape.asInstanceOf [CurvilinearShape], c.label, c.primary, c.color, c.from_eid, c.to_eid,
                            c.pts, c.shift)
//          ani.createEdge (c.eid, c.shape.asInstanceOf [QCurve], c.label, c.primary, c.color, c.from_eid, c.to_eid, c.pts)
        case CreateToken =>
            ani.createToken (c.eid, c.shape.asInstanceOf [RectangularShape], c.label, c.primary, c.color, c.from_eid, c.pts)
        case DestroyNode =>
            ani.destroyNode (c.eid)
        case DestroyEdge =>
            ani.destroyEdge (c.eid)
        case DestroyToken =>
            ani.destroyToken (c.eid)
        case MoveNode =>
            ani.moveNode (c.eid, c.pts)
        case MoveToken =>
            ani.moveToken (c.eid, c.pts)
        case MoveToken2Node =>
            ani.moveToken2Node (c.eid, c.from_eid)
        case MoveTokens2Node =>
            ani.moveTokens2Node (c.color, c.from_eid, c.to_eid, c.pts)
        case MoveToken2Edge =>
            ani.moveToken2Edge (c.eid, c.from_eid, 10.0)              // FIX: 10.0?
        case ScaleNode =>
            ani.scaleNode (c.eid, c.pts)
        case ScaleToken =>
            ani.scaleToken (c.eid, c.pts)
        case ScaleTokensAt =>
            ani.scaleTokensAt (c.color, c.from_eid, c.to_eid, c.pts)
        case SetPaintNode =>
            ani.setPaintNode (c.eid, c.color)
        case SetPaintEdge =>
            ani.setPaintEdge (c.eid, c.color)
        case SetPaintToken =>
            ani.setPaintToken (c.eid, c.color)
        case TimeDilation =>
            ani.timeDilation (c.pts)
        end match
    end invokeCommand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Repeatedly execute animation commands, sleep and repaint.
     */
    def run (): Unit =
        var cmd: AnimateCommand = null
        var when  = 0.0
        var delay = 0L
        var nCmds = 0

        println (s"DgAnimator.run: start animation at time $clock")
        printCommandQueue (clock)

        breakable {
            while clock < stopTime do

                // PAUSE GATE: block the animation thread while paused (unless stepping once)
                pauseLock.synchronized {
                    while paused.get() && ! stepOnce.get() do pauseLock.wait()
                }

                //:: Get the next animation command from the shared queue.

                if cmdQ.isEmpty && aniDone then
                    println ("DgAnimator.run: command queue is empty")
                    break ()
                else if ! cmdQ.isEmpty then
                    cmd   = cmdQ.poll ()
                    when  = cmd.time
                    delay = round ((when - clock) * aniRatio * ani.timeDilationFactor)

                    //:: Sleep for the given number (delay) of milliseconds.

                    Thread.sleep (delay)

                    //:: set the animation clock and invoke the animation command

                    // Record executed command for replay
                    frames += Frame(cmd)
                    clock  = when
                    nCmds += 1
                    invokeCommand (cmd)

                    //::If the command is to create a new token, then increment the actor count
                    if cmd.action == CreateToken then actorCount +=1

                    //:: Repaint the canvas.

                    repaint ()

                    // If stepping, consume one command and re-enter pause
                    if stepOnce.compareAndSet(true, false) then
                        paused.set(true)
                        playPauseBtn.setText("Play")
                    end if
                end if
            end while
        } // breakable

        println (s"DgAnimator.run: end animation at time $clock with $nCmds commands invoked")
    end run

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start the animation by staring the animation thread.
     *  @param tStart  the animation start time
     *  @param tStop   the animation stop time
     */
    def animate (tStart: Double, tStop: Double): Unit =
        clock    = tStart
        stopTime = tStop
        new Thread (this).start ()
    end animate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invoke animation command cmd immediately (useful for testing).
     *  @param cmd  the animation command to invoke
     */
    def invokeNow (cmd: AnimateCommand): Unit =
        invokeCommand (cmd)
        repaint ()
    end invokeNow

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the animation command queue.
     */
    def getCommandQueue: ConcurrentLinkedQueue [AnimateCommand] = cmdQ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the contents of the command queue one animation command per line.
     *  @param t  the given time
     */
    def printCommandQueue (t: Double): Unit =
        println (s"At time t = $t: command queue = ")
        println (cmdQ.toString.replace ("), A", ")\nA"))
        println ("-" * 80)
    end printCommandQueue

    /**
     * @param count: The count of the actors produced/generated that the model is working with
     * We use this to update the totalActor count once so that we can use it with the animator drawing
     * canvas.
     */
    def updateActorCount(count: Int): Unit =
        totalActorCount = count             // update the total count of actors the models is working to be used by the animator
    end updateActorCount

end DgAnimator


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `dgAnimatorTest` main function is used to test the `DgAnimator` class.
 *  It tests the creation of nodes.
 *  > runMain scalation.animation.dgAnimatorTest
 */
@main def dgAnimatorTest (): Unit =

    banner ("Run dgAnimatorTest")
    val dga  = new DgAnimator ("DgAnimator", bgColor = lightgrey)
    val aniQ = dga.getCommandQueue

    //:: Place the nodes into graph.

    aniQ.add (AnimateCommand (CreateNode, 1, Ellipse (),   "node1", false, yellow, Array (100.0, 110.0, 30.0, 30.0), 0))
    aniQ.add (AnimateCommand (CreateNode, 2, Ellipse (),   "node2", false, yellow, Array (100.0, 290.0, 30.0, 30.0), 0))
    aniQ.add (AnimateCommand (CreateNode, 3, Rectangle (), "node3", true,  gold,   Array (300.0, 185.0, 30.0, 60.0), 1000))
    aniQ.add (AnimateCommand (CreateNode, 4, Ellipse (),   "node4", false, silver, Array (500.0, 110.0, 30.0, 30.0), 2000))
    aniQ.add (AnimateCommand (CreateNode, 5, Ellipse (),   "node5", false, silver, Array (500.0, 290.0, 30.0, 30.0), 2000))
    aniQ.add (AnimateCommand (CreateNode, 6, Rectangle (), "node6", true,  gold,   Array (300.0,  35.0, 30.0, 60.0), 3000))
    aniQ.add (AnimateCommand (CreateNode, 7, Rectangle (), "node7", true,  gold,   Array (300.0, 335.0, 30.0, 60.0), 3000))

    dga.animate (0, 100000)

end dgAnimatorTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `dgAnimatorTest2` main function is used to test the `DgAnimator` class.
 *  It tests the creation of nodes, edges and tokens.
 *  > runMain scalation.animation.dgAnimatorTest2
 */
@main def dgAnimatorTest2 (): Unit =

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sample method for loading the shared command queue.
     *  Ordinarily these commands would come from some simulation engine.
     *  @param cq  the animation command queue
     */
    def loadCommandQueue (cq: ConcurrentLinkedQueue [AnimateCommand]): Unit =

        //:: Place the nodes into graph.

        cq.add (AnimateCommand (CreateNode, 1, Ellipse (),   "node1", false, yellow, Array (100.0, 110.0, 30.0, 30.0), 0))
        cq.add (AnimateCommand (CreateNode, 2, Ellipse (),   "node2", false, yellow, Array (100.0, 290.0, 30.0, 30.0), 0))
        cq.add (AnimateCommand (CreateNode, 3, Rectangle (), "node3", true,  gold,   Array (300.0, 185.0, 30.0, 60.0), 1000))
        cq.add (AnimateCommand (CreateNode, 4, Ellipse (),   "node4", false, silver, Array (500.0, 110.0, 30.0, 30.0), 2000))
        cq.add (AnimateCommand (CreateNode, 5, Ellipse (),   "node5", false, silver, Array (500.0, 290.0, 30.0, 30.0), 2000))
        cq.add (AnimateCommand (CreateNode, 6, Rectangle (), "node6", true,  gold,   Array (300.0,  35.0, 30.0, 60.0), 3000))
        cq.add (AnimateCommand (CreateNode, 7, Rectangle (), "node7", true,  gold,   Array (300.0, 335.0, 30.0, 60.0), 3000))
 
        //:: Place the edges into graph.

        cq.add (AnimateCommand (CreateEdge, 8,  QCurve (), "edge1", true, red, null, 4000, 1, 3))
        cq.add (AnimateCommand (CreateEdge, 9,  QCurve (), "edge2", true, red, null, 4000, 2, 3))
        cq.add (AnimateCommand (CreateEdge, 10, QCurve (), "edge3", true, red, null, 5000, 3, 4))
        cq.add (AnimateCommand (CreateEdge, 11, QCurve (), "edge4", true, red, null, 5000, 3, 5))
        cq.add (AnimateCommand (CreateEdge, 12, QCurve (), "edge5", true, red, null, 6000, 4, 6))
        cq.add (AnimateCommand (CreateEdge, 13, QCurve (), "edge6", true, red, null, 6000, 5, 7))
        cq.add (AnimateCommand (CreateEdge, 14, QCurve (), "edge7", true, red, null, 7000, 6, 1))
        cq.add (AnimateCommand (CreateEdge, 15, QCurve (), "edge8", true, red, null, 7000, 7, 2))

        //:: Place the tokens into graph.

        cq.add (AnimateCommand (CreateToken, 16, Ellipse (), "token1", false, blue, null, 8000, 1))
        cq.add (AnimateCommand (CreateToken, 17, Ellipse (), "token2", false, cyan, null, 8000, 2))

        //:: Move the tokens around graph.

        for i <- 0 to 10 do
            cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 12000 + 10000 * i, 3))
            cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 12000 + 10000 * i, 3))
            cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 13000 + 10000 * i, 4))
            cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 13000 + 10000 * i, 5))
            cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 17000 + 10000 * i, 6))
            cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 17000 + 10000 * i, 7))
            cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 18000 + 10000 * i, 1))
            cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 18000 + 10000 * i, 2))
        end for
    end loadCommandQueue

    banner ("Run DgAnimatorTest2")
    val dga = new DgAnimator ("DgAnimator", bgColor = lightgrey)
    loadCommandQueue (dga.getCommandQueue)
    dga.animate (0, 100000)

end dgAnimatorTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `dgAnimatorTest3` main function is used to test the `DgAnimator` class.
 *  It tests zoom in and zoom out of a triagle with three nodes and three edges.
 *  > runMain scalation.animation.dgAnimatorTest3
 */
@main def dgAnimatorTest3 (): Unit =

    banner ("Run dgAnimatorTest3")
    val dga  = new DgAnimator ("DgAnimator")
    val aniQ = dga.getCommandQueue

    println ("Make a triangle and zoom in and zoom out")
    println ("print zooming instructions")

    //:: Place the nodes into graph.

    aniQ.add (AnimateCommand (CreateNode, 1, Ellipse(),   "node1", false, yellow, Array(100.0, 110.0, 30.0, 30.0), 0))
    aniQ.add (AnimateCommand (CreateNode, 2, Ellipse(),   "node2", false, yellow, Array(100.0, 290.0, 30.0, 30.0), 0))
    aniQ.add (AnimateCommand (CreateNode, 3, Rectangle(), "node3", true,  gold,   Array(300.0, 185.0, 30.0, 60.0), 0))

    //:: Place the edges into graph.

    aniQ.add (AnimateCommand (CreateEdge, 4, QCurve(), "edge1", true, blue, null, 100, 1, 2))
    aniQ.add (AnimateCommand (CreateEdge, 5, QCurve(), "edge2", true, blue, null, 200, 2, 3))
    aniQ.add (AnimateCommand (CreateEdge, 6, QCurve(), "edge3", true, blue, null, 300, 3, 1))

    dga.animate (0, 100000)

end dgAnimatorTest3

