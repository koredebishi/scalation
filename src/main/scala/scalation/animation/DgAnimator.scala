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
import scala.collection.mutable.{ArrayBuffer, HashMap}
import javax.swing.{JPanel, SwingUtilities}
import java.awt.{GridBagLayout, GridBagConstraints}
import java.awt.event.MouseEvent
import java.awt.geom.Point2D


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

    //=========================== Vehicle Inspector (Steps 1+2) ==========================
    /** Lightweight immutable snapshot of vehicle state for the inspector. */
    case class VehicleState(
        actorId: Int,
        label: String,
        laneId: String,
        pathInfo: String,
        segDisp: Double,
        pathDisp: Double,
        odo: Double,
        velocity: Double,
        carAheadLabel: String,
        segmentIndex: Int = -1,
        laneGroupId: String = ""
    )

    /** Concurrent-like registry keyed by actorId with latest snapshots (guarded by vsLock). */
    private val vehicleStateRegistry = new HashMap[Int, VehicleState]()

    /** Map token eid -> actorId (defaults to identity when unknown). */
    private val tokenToActorId = new HashMap[Int, Int]()

    /** Simple lock for synchronizing state updates/reads. */
    private object vsLock

    /** Currently selected actorId (-1 means none). */
    @volatile private var selectedActorId: Int = -1

    /** Allow the simulation to set/override the token eid -> actorId mapping. */
    def setTokenActorId(tokenEid: Int, actorId: Int): Unit = vsLock.synchronized { tokenToActorId.update(tokenEid, actorId) }

    /** Public hook for the simulation to push state updates. */
    def updateVehicleState(actorId: Int, state: VehicleState): Unit =
        vsLock.synchronized { vehicleStateRegistry.update(actorId, state) }
        if inspectorVisible && actorId == selectedActorId then SwingUtilities.invokeLater(() => inspectorPanel.refresh())
    end updateVehicleState

    /** Compute a simple same-lane/segment gap; else None. */
    private def computeGap(me: VehicleState, ahead: VehicleState, vehicleLen: Double = 4.5): Option[Double] =
        val sameLane   = (me.laneId == ahead.laneId) && (me.laneGroupId == ahead.laneGroupId)
        val sameSeg    = me.segmentIndex >= 0 && me.segmentIndex == ahead.segmentIndex
        if sameLane && sameSeg then Some(math.max(0.0, (ahead.segDisp - me.segDisp) - vehicleLen)) else None
    end computeGap

    /** Inspector visibility and UI. */
    @volatile private var inspectorVisible = false
    private val inspectorPanel = new JPanel(new GridBagLayout())
    private val statusLbl   = new JLabel("No selection")
    private val idLbl       = new JLabel("")
    private val laneLbl     = new JLabel("")
    private val pathLbl     = new JLabel("")
    private val segDispLbl  = new JLabel("")
    private val pathDispLbl = new JLabel("")
    private val odoLbl      = new JLabel("")
    private val velLbl      = new JLabel("")
    private val aheadLbl    = new JLabel("")
    private val gapLbl      = new JLabel("")

    /** Build the inspector panel (right dock), initially hidden. */
    private def buildInspector(): Unit =
        val gbc = new GridBagConstraints()
        gbc.gridx = 0; gbc.gridy = 0; gbc.anchor = GridBagConstraints.WEST
        inspectorPanel.add(new JLabel("Vehicle Inspector"), gbc)
        gbc.gridy += 1; inspectorPanel.add(statusLbl, gbc)
        def row(name: String, value: JLabel): Unit =
            gbc.gridy += 1; inspectorPanel.add(new JLabel(name+":"), gbc)
            gbc.gridx = 1; inspectorPanel.add(value, gbc); gbc.gridx = 0
        end row
        row("Actor", idLbl)
        row("Lane", laneLbl)
        row("Path", pathLbl)
        row("segDisp", segDispLbl)
        row("pathDisp", pathDispLbl)
        row("Odo", odoLbl)
        row("Velocity", velLbl)
        row("CarAhead", aheadLbl)
        row("Gap", gapLbl)
        inspectorPanel.setVisible(false)
        getContentPane().add(inspectorPanel, BorderLayout.EAST)
    end buildInspector

    /** Refresh inspector fields based on selectedActorId. */
    extension (p: JPanel)
        def refresh(): Unit =
            if selectedActorId < 0 then
                statusLbl.setText("No selection")
                idLbl.setText(""); laneLbl.setText(""); pathLbl.setText("")
                segDispLbl.setText(""); pathDispLbl.setText(""); odoLbl.setText("")
                velLbl.setText(""); aheadLbl.setText(""); gapLbl.setText("")
                return
            end if
            val st = vsLock.synchronized { vehicleStateRegistry.get(selectedActorId).orNull }
            if st == null then
                statusLbl.setText("No data yet")
                idLbl.setText(selectedActorId.toString)
                laneLbl.setText(""); pathLbl.setText("")
                segDispLbl.setText(""); pathDispLbl.setText(""); odoLbl.setText("")
                velLbl.setText(""); aheadLbl.setText(""); gapLbl.setText("")
                return
            end if
            statusLbl.setText("")
            idLbl.setText(s"${st.label} (#${st.actorId})")
            laneLbl.setText(st.laneId)
            pathLbl.setText(st.pathInfo)
            segDispLbl.setText(f"${st.segDisp}%.2f")
            pathDispLbl.setText(f"${st.pathDisp}%.2f")
            odoLbl.setText(f"${st.odo}%.2f")
            velLbl.setText(f"${st.velocity}%.2f")
            aheadLbl.setText(st.carAheadLabel)
            var gapTxt = "N/A"
            if st.carAheadLabel != null && st.carAheadLabel.nonEmpty then
                val idOpt = try { Some(st.carAheadLabel.filter(_.isDigit).toInt) } catch { case _: Throwable => None }
                idOpt.foreach { aid =>
                    val ahead = vsLock.synchronized { vehicleStateRegistry.get(aid).orNull }
                    if ahead != null then computeGap(st, ahead).foreach(g => gapTxt = f"$g%.2f")
                }
            end if
            gapLbl.setText(gapTxt)
        end refresh

    /** Toggle inspector visibility. */
    private def toggleInspector(): Unit =
        inspectorVisible = !inspectorVisible
        inspectorPanel.setVisible(inspectorVisible)
        if inspectorVisible then inspectorPanel.refresh()
        revalidate(); repaint()
    end toggleInspector

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
    /** The canvas Panel is used to place shapes in the drawing region. */
    class Canvas
          extends ZoomablePanel:

        private val fsize = 18
        private val f     = new Font ("Serif", Font_BOLD, fsize)

        // Track press/drag to treat small drags as clicks
        private var pressX = -1
        private var pressY = -1
        private var dragged = false
        private val clickSlop = 3   // pixels tolerance

        /** Overlay a small PAUSED stamp (optional visual cue). */
        private def drawPausedOverlay(g2d: Graphics2D): Unit =
            if paused.get() then
                g2d.setPaint (red)
                g2d.drawString ("PAUSED", getW - 100, 30)
            end if
        end drawPausedOverlay

        /** Draw a token label centered on the token shape. */
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

        /** Convert screen coords to world coords using inverse transform. */
        private def toWorldPoint(x: Int, y: Int): Point2D.Double =
            val src = new Point2D.Double(x.toDouble, y.toDouble)
            val dst = new Point2D.Double()
            try at.inverseTransform(src, dst) catch case _: Throwable => dst.setLocation(src)
            dst
        end toWorldPoint

        /** Pick the top-most token under the given world point. */
        private def pickToken(wx: Double, wy: Double): graph.Token =
            val pt = new Point2D.Double(wx, wy)
            val free = graph.freeTokens.toList
            var i = free.length - 1
            while i >= 0 do
                val t = free(i)
                if t.shape.contains(pt) then return t
                i -= 1
            end while
            val edges = graph.edges.toList
            var e = edges.length - 1
            while e >= 0 do
                val ts = edges(e).tokens.toList
                var j = ts.length - 1
                while j >= 0 do
                    val t = ts(j)
                    if t.shape.contains(pt) then return t
                    j -= 1
                end while
                e -= 1
            end while
            val nodes = graph.nodes.toList
            var n = nodes.length - 1
            while n >= 0 do
                val ts = nodes(n).tokens.toList
                var k = ts.length - 1
                while k >= 0 do
                    val t = ts(k)
                    if t.shape.contains(pt) then return t
                    k -= 1
                end while
                n -= 1
            end while
            null
        end pickToken

        /** Robust click: short press-release without movement selects a token. */
        override def mousePressed(e: MouseEvent): Unit =
            pressX = e.getX
            pressY = e.getY
            dragged = false
            super.mousePressed(e)
        end mousePressed

        override def mouseDragged(e: MouseEvent): Unit =
            if math.abs(e.getX - pressX) > clickSlop || math.abs(e.getY - pressY) > clickSlop then dragged = true
            super.mouseDragged(e)
        end mouseDragged

        override def mouseReleased(e: MouseEvent): Unit =
            super.mouseReleased(e)
            if pressX >= 0 && !dragged &&
               math.abs(e.getX - pressX) <= clickSlop && math.abs(e.getY - pressY) <= clickSlop then
                val wp  = toWorldPoint(e.getX, e.getY)
                val tok = pickToken(wp.getX, wp.getY)
                if tok != null then
                    ani.getTokenIdByRef(tok) match
                        case Some(eid) =>
                            val actorId = vsLock.synchronized { tokenToActorId.getOrElse(eid, eid) }
                            selectedActorId = actorId
                            if inspectorVisible then inspectorPanel.refresh()
                        case None =>
                            selectedActorId = -1
                            if inspectorVisible then inspectorPanel.refresh()
                    end match
                else
                    selectedActorId = -1
                    if inspectorVisible then inspectorPanel.refresh()
                end if
            end if
            pressX = -1; pressY = -1; dragged = false
        end mouseReleased

        /** Fallback: also handle standard Swing mouseClicked events. */
        override def mouseClicked(e: MouseEvent): Unit =
            val wp  = toWorldPoint(e.getX, e.getY)
            val tok = pickToken(wp.getX, wp.getY)
            if tok != null then
                ani.getTokenIdByRef(tok) match
                    case Some(eid) =>
                        val actorId = vsLock.synchronized { tokenToActorId.getOrElse(eid, eid) }
                        selectedActorId = actorId
                        if inspectorVisible then inspectorPanel.refresh()
                    case None =>
                        selectedActorId = -1
                        if inspectorVisible then inspectorPanel.refresh()
                end match
            else
                selectedActorId = -1
                if inspectorVisible then inspectorPanel.refresh()
            end if
        end mouseClicked

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Paint the display panel component. */
        override def paintComponent (gr: Graphics): Unit =
            super.paintComponent (gr)
            val g2d = gr.asInstanceOf [Graphics2D]            // use hi-resolution

            g2d.setTransform (at)                             // used for zooming

            // Display the animation clock and actor counts
            g2d.setFont (f)
            g2d.setPaint (fgColor)
            g2d.drawString(f"CLOCK = $clock%10.3f", clockWH._1, getH - clockWH._2)
            g2d.drawString(s"ACTORS = $actorCount / $totalActorCount", clockWH._1, getH - clockWH._2 - 20)

            // Optional PAUSED overlay
            drawPausedOverlay(g2d)

            // Display all nodes and their bound tokens
            val nodes = graph.nodes.toList
            for node <- nodes do
                g2d.setPaint (node.color)
                g2d.fill (node.shape)
                g2d.setPaint (black)
                g2d.draw (node.shape)
                val x = node.shape.getCenterX ().asInstanceOf [Float]
                val y = node.shape.getBounds2D.getMaxY ().asInstanceOf [Float]
                g2d.drawString (node.label, x, y)
                val node_tokens = node.tokens.toList
                for token <- node_tokens do
                    g2d.setPaint (token.color)
                    g2d.fill (token.shape)
                    drawTokenLabel (g2d, token)
                end for
            end for

            // Display all edges and their bound tokens
            val edges = graph.edges.toList
            for edge <- edges do
                g2d.setPaint (edge.color)
                g2d.draw (edge.shape)
                val x = edge.shape.getCenterX.asInstanceOf [Float]
                val y = edge.shape.getCenterY.asInstanceOf [Float]
                g2d.drawString (edge.label, x, y)
                val edge_tokens = edge.tokens.toList
                for token <- edge_tokens if token.shape.getWidth () > 0.0 do
                    g2d.setPaint (token.color)
                    g2d.fill (token.shape)
                    drawTokenLabel (g2d, token)
                end for
            end for

            // Display all free tokens
            val free_tokens = graph.freeTokens.toList
            for token <- free_tokens if token.shape.getWidth () > 0.0 do
                g2d.setPaint (token.color)
                g2d.fill (token.shape)
                drawTokenLabel (g2d, token)
            end for
        end paintComponent

    end Canvas

    //================================================================================
    // Playback controls (Play/Pause/Step)
    //================================================================================
    /** Thread-safe flags for playback control. */
    private val paused    = new AtomicBoolean(false)  // true => run loop waits
    private val stepOnce  = new AtomicBoolean(false)  // true => process one command, then re-pause
    private val pauseLock = new Object()              // monitor for wait/notify

    /** UI: toolbar buttons for playback control and inspector toggle. */
    private val playPauseBtn = new JButton("Pause")
    private val stepBtn      = new JButton("Step")
    private val replayBtn    = new JButton("Replay")
    private val inspectBtn   = new JButton("Inspect")

    /** Build toolbar and keyboard shortcuts. */
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
        // Speed controls
        slowerBtn.setToolTipText("Slow down (halve speed)")
        fasterBtn.setToolTipText("Speed up (double speed)")
        resetBtn.setToolTipText("Reset speed to 1.0x")
        slowerBtn.addActionListener(_ => setSpeedFactor(speedFactor / 2.0))
        resetBtn.addActionListener(_ => setSpeedFactor(1.0))
        fasterBtn.addActionListener(_ => setSpeedFactor(speedFactor * 2.0))
        bar.add(slowerBtn); bar.add(resetBtn); bar.add(fasterBtn); bar.add(speedLbl)
        // Inspector toggle
        inspectBtn.setToolTipText("Toggle Vehicle Inspector (I)")
        inspectBtn.addActionListener(_ => toggleInspector())
        bar.add(inspectBtn)
        getContentPane().add(bar, BorderLayout.NORTH)
        // Keyboard shortcuts
        val rp = getRootPane()
        rp.registerKeyboardAction(_ => togglePause(),    KeyStroke.getKeyStroke("SPACE"), JComponent.WHEN_IN_FOCUSED_WINDOW)
        rp.registerKeyboardAction(_ => step(),           KeyStroke.getKeyStroke("N"),     JComponent.WHEN_IN_FOCUSED_WINDOW)
        rp.registerKeyboardAction(_ => replay(),         KeyStroke.getKeyStroke("R"),     JComponent.WHEN_IN_FOCUSED_WINDOW)
        rp.registerKeyboardAction(_ => toggleInspector(),KeyStroke.getKeyStroke("I"),     JComponent.WHEN_IN_FOCUSED_WINDOW)
        setSpeedFactor(1.0)
    end buildControls

    /** Pause the animation thread. */
    private def pause(): Unit =
        if paused.compareAndSet(false, true) then playPauseBtn.setText("Play")
    end pause

    /** Resume the animation thread. */
    private def resume(): Unit =
        if paused.compareAndSet(true, false) then
            playPauseBtn.setText("Pause")
            pauseLock.synchronized { pauseLock.notifyAll() }
        end if
    end resume

    /** Toggle between Play and Pause. */
    private def togglePause(): Unit = if paused.get() then resume() else pause()

    /** Step a single command when paused, then re-pause. */
    private def step(): Unit =
        if paused.get() then
            stepOnce.set(true)
            pauseLock.synchronized { pauseLock.notifyAll() }
        end if
    end step

    // Primary constructor block
    {
        getContentPane ().add (new Canvas)
        buildControls()
        buildInspector()
        setVisible (true)
        setBackground (bgColor)
    }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invoke the animation command. */
    private def invokeCommand (c: AnimateCommand): Unit =
        if c.action != MoveToken then println (s"DgAnimator.invokeCommand: $c")
        c.action match
        case CreateNode =>
            ani.createNode (c.eid, c.shape.asInstanceOf [RectPolyShape], c.label, c.primary, c.color, c.pts)
        case CreateEdge =>
            ani.createEdge (c.eid, c.shape.asInstanceOf [CurvilinearShape], c.label, c.primary, c.color, c.from_eid, c.to_eid,
                            c.pts, c.shift)
        case CreateToken =>
            ani.createToken (c.eid, c.shape.asInstanceOf [RectangularShape], c.label, c.primary, c.color, c.from_eid, c.pts)
            vsLock.synchronized { tokenToActorId.getOrElseUpdate(c.eid, c.eid) }
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
            ani.moveToken2Edge (c.eid, c.from_eid, 10.0)
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
    /** Repeatedly execute animation commands, sleep and repaint. */
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

                // Get the next animation command from the shared queue.
                if cmdQ.isEmpty && aniDone then
                    println ("DgAnimator.run: command queue is empty")
                    break ()
                else if ! cmdQ.isEmpty then
                    cmd   = cmdQ.poll ()
                    when  = cmd.time
                    delay = round ((when - clock) * aniRatio * ani.timeDilationFactor)

                    // Sleep for the given number (delay) of milliseconds.
                    Thread.sleep (delay)

                    // Set the animation clock and invoke the animation command
                    clock  = when
                    nCmds += 1
                    invokeCommand (cmd)

                    // If the command is to create a new token, then increment the actor count
                    if cmd.action == CreateToken then actorCount += 1

                    // Repaint the canvas.
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
    /** Start the animation by staring the animation thread. */
    def animate (tStart: Double, tStop: Double): Unit =
        clock    = tStart
        stopTime = tStop
        new Thread (this).start ()
    end animate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invoke animation command cmd immediately (useful for testing). */
    def invokeNow (cmd: AnimateCommand): Unit =
        invokeCommand (cmd)
        repaint ()
    end invokeNow

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the animation command queue. */
    def getCommandQueue: ConcurrentLinkedQueue [AnimateCommand] = cmdQ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the contents of the command queue one animation command per line. */
    def printCommandQueue (t: Double): Unit =
        println (s"At time t = $t: command queue = ")
        println (cmdQ.toString.replace ("), A", ")\nA"))
        println ("-" * 80)
    end printCommandQueue

    /** Update the total actor count (for UI display). */
    def updateActorCount(count: Int): Unit =
        totalActorCount = count
    end updateActorCount

    /** Set the animation complete flag to true (used by external drivers). */
    def setAniDone(): Unit =
        aniDone = true
    end setAniDone


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save the graphics into an image file.
     *  @param fname  the file name
     */
    def saveImage (fname: String): Unit = writeImage (fname, this)

end DgAnimator

// ...tests (dgAnimatorTest, dgAnimatorTest2, dgAnimatorTest3) can follow below if present...
