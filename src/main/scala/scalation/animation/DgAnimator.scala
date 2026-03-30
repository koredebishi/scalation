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
import java.awt.{BasicStroke, GradientPaint, RenderingHints}
import java.awt.geom.{AffineTransform, Point2D}
import scala.math.round
import scalation.scala2d.*
import scalation.scala2d.Colors.*
import CommandType.*
import scala.collection.mutable.{ArrayBuffer, HashMap}
import javax.swing.{JPanel, SwingUtilities}
import java.awt.{GridBagLayout, GridBagConstraints}
import java.awt.event.MouseEvent


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

    //================================ Unified Playback Mode ==============================
    /** Playback modes: Idle (no loop active), Live (consuming cmdQ), Replay (consuming frames). */
    private enum PlaybackMode:
        case Idle, Live, Replay
    private var playbackMode: PlaybackMode = PlaybackMode.Idle      // current playback mode
    private val running = new AtomicBoolean(false)                  // true while run loop thread active
    // Replay iteration state
    private var replayIndex  = 0
    private var replayLength = 0
    private var lastTime     = 0.0                                  // last processed command time (for pacing)
    // Track when the live queue first became empty (for auto-finalization if aniDone not set)
    private var emptySince: Long = -1L

    //=========================== Vehicle Inspector (Steps 1+2) ==========================
    /** Lightweight immutable snapshot of vehicle state for the inspector.
      *  Defined in the DgAnimator companion object (see bottom of file).
      */
    import DgAnimator.VehicleState

    private val vehicleStateRegistry = new HashMap[Int, VehicleState]()

    /** Map token eid -> actorId (defaults to identity when unknown). */
    private val tokenToActorId = new HashMap[Int, Int]()

    /** Simple lock for synchronizing state updates/reads. */
    private object vsLock

    /** Currently selected actorId (-1 means none). */
    @volatile private var selectedActorId: Int = -1

    // ── HUD data (pushed by simulation, read by renderer) ───────────────
    @volatile private var hudModelName: String   = "IDM"
    @volatile private var hudAvgSpeed: Double    = 0.0
    @volatile private var hudThroughput: Double  = 0.0
    @volatile private var hudSegDensities: Array [Double] = Array.empty  // veh/km per segment
    @volatile private var hudSegLabels: Array [String]    = Array.empty  // e.g. "S0","S1",...
    @volatile private var hudSpeedLimitMph: Int = 65                    // corridor speed limit for signs

    /** Set the dynamics model name shown on the HUD (e.g., "IDM", "Gipps"). */
    def setHudModelName (name: String): Unit = hudModelName = name

    /** Set the corridor speed limit (mph) displayed on road signs.  Default 65. */
    def setHudSpeedLimit (mph: Int): Unit = hudSpeedLimitMph = mph

    /** Push live throughput (veh/hr) and average speed (m/s) for HUD display. */
    def updateHudStats (throughput: Double, avgSpeed: Double): Unit =
        hudThroughput = throughput
        hudAvgSpeed   = avgSpeed
    end updateHudStats

    /** Push per-segment density values (veh/km) for the mini bar chart.
     *  @param densities  array of densities, one per segment
     *  @param labels     optional short labels for each segment
     */
    def updateSegmentDensities (densities: Array [Double], labels: Array [String] = null): Unit =
        hudSegDensities = densities.clone ()
        if labels != null then hudSegLabels = labels.clone ()
        else if hudSegLabels.length != densities.length then
            hudSegLabels = Array.tabulate (densities.length)(i => s"S$i")
    end updateSegmentDensities

    /** Allow the simulation to set/override the token eid -> actorId mapping. */
    def setTokenActorId(tokenEid: Int, actorId: Int): Unit = vsLock.synchronized { tokenToActorId.update(tokenEid, actorId) }

    /** Public hook for the simulation to push state updates. */
    def updateVehicleState(actorId: Int, state: VehicleState): Unit =
        vsLock.synchronized { vehicleStateRegistry.update(actorId, state) }
        if inspectorVisible && actorId == selectedActorId then SwingUtilities.invokeLater(() => inspectorPanel.refresh())
    end updateVehicleState


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
            segDispLbl.setText(s"${st.segDisp}")
            pathDispLbl.setText(s"${st.pathDisp}")
            velLbl.setText(s"${st.velocity}")
            var gapTxt = "N/A"
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
        // Unified replay entry point: switches run loop to Replay mode (no separate thread logic)
        if frames.isEmpty then
            println("Replay: no recorded frames (frames buffer empty). Run a simulation first.")
            return
        end if
        // Disallow starting replay while live playback still consuming queue
        if playbackMode == PlaybackMode.Live && running.get() then
            if !cmdQ.isEmpty then
                println("Replay: live animation still in progress (commands remaining).")
                return
            else if !aniDone then
                // Queue empty but aniDone not set: auto-finalize live mode
                println("Replay: live mode idle with empty queue; auto-finalizing to allow replay.")
                aniDone = true
                playbackMode = PlaybackMode.Idle
                running.set(false)
                // Small wait to allow loop to exit cleanly
//                Thread.sleep(20)
                while running.get() do Thread.`yield`()
            end if
        end if
        // If a previous run loop thread finished, we will start a new one; if still running in Idle, reuse
        resetSceneForReplay()                                    // clear canvas for fresh reconstruction
        playbackMode  = PlaybackMode.Replay
        replayIndex   = 0
        replayLength  = frames.length
        clock         = 0.0                                      // normalize display clock for replay start
        lastTime      = 0.0
        println(s"Replay: entering Replay mode with $replayLength frames")
        if !running.get() then
            running.set(true)
            new Thread(this).start()                             // start unified run loop in Replay mode
        else
            // If thread somehow still running (Idle spin not expected), it will pick up mode switch next cycle
            ()
        end if
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

        setBackground (bgColor)                                     // dark/light theme background

        private val fsize = 18
        private val f     = new Font ("SansSerif", Font_BOLD, fsize)
        private val roadStroke    = new BasicStroke (2.5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
        private val pavementStroke = new BasicStroke (10.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
        private val dashStroke     = new BasicStroke (1.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER,
                                         10.0f, Array (12.0f, 8.0f), 0.0f)
        private val pavementColor  = new Color (50, 50, 58)
        private val dashColor      = new Color (200, 200, 210, 160)
        private val hudFont        = new Font ("SansSerif", Font_BOLD, 22)
        private val hudFontSmall   = new Font ("SansSerif", java.awt.Font.PLAIN, 18)
        private val hudBg          = new Color (20, 20, 28, 200)

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

            // Anti-aliasing for smooth curves and text
            g2d.setRenderingHint (RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
            g2d.setRenderingHint (RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
            g2d.setRenderingHint (RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)

            // Gradient background (screen-space, before zoom transform)
            val gradTop = new Color (
                math.min (255, bgColor.getRed + 18),
                math.min (255, bgColor.getGreen + 18),
                math.min (255, bgColor.getBlue + 22))
            g2d.setPaint (new GradientPaint (0f, 0f, gradTop, 0f, getHeight.toFloat, bgColor))
            g2d.fillRect (0, 0, getWidth, getHeight)

            g2d.setTransform (at)                             // used for zooming


            // Display all nodes and their bound tokens
            val nodes = graph.nodes.toList
            val labelFont  = new Font ("SansSerif", java.awt.Font.BOLD, 11)
            val signFont   = new Font ("SansSerif", java.awt.Font.BOLD, 10)
            var nodeIdx = 0
            for node <- nodes do
                g2d.setPaint (node.color)
                g2d.fill (node.shape)
                g2d.setPaint (fgColor)
                g2d.draw (node.shape)

                // ── #10  Junction label with background badge ──────────────
                val nx = node.shape.getCenterX ()
                val ny = node.shape.getBounds2D.getMaxY ()
                val label = node.label
                if label != null && label.nonEmpty then
                    g2d.setFont (labelFont)
                    val fm = g2d.getFontMetrics
                    val lw = fm.stringWidth (label)
                    val lh = fm.getHeight
                    val lx = (nx - lw / 2.0).toInt
                    val ly = (ny + 14).toInt
                    val prevComp = g2d.getComposite
                    g2d.setComposite (java.awt.AlphaComposite.getInstance (
                        java.awt.AlphaComposite.SRC_OVER, 0.80f))
                    g2d.setPaint (new Color (20, 20, 35))
                    g2d.fillRoundRect (lx - 4, ly - lh + 3, lw + 8, lh + 2, 8, 8)
                    g2d.setComposite (prevComp)
                    g2d.setPaint (fgColor)
                    g2d.drawString (label, lx.toFloat, ly.toFloat)
                end if

                // ── #11  Speed-limit sign at every 3rd node ────────────────
                if nodeIdx % 3 == 0 then
                    val sx = (node.shape.getBounds2D.getMaxX + 6).toInt
                    val sy = (node.shape.getBounds2D.getMinY - 2).toInt
                    val signTxt = s"${hudSpeedLimitMph}"
                    g2d.setFont (signFont)
                    val sfm = g2d.getFontMetrics
                    val sw  = sfm.stringWidth (signTxt)
                    val sh  = sfm.getHeight
                    val pw  = sw + 8; val ph = sh + 6
                    g2d.setPaint (white)
                    g2d.fillRoundRect (sx, sy, pw, ph, 5, 5)
                    g2d.setPaint (black)
                    g2d.setStroke (new BasicStroke (1.5f))
                    g2d.drawRoundRect (sx, sy, pw, ph, 5, 5)
                    g2d.drawString (signTxt, sx + 4, sy + sh + 1)
                end if
                nodeIdx += 1

                val node_tokens = node.tokens.toList
                for token <- node_tokens do
                    drawTokenGlow (g2d, token)
                    g2d.setPaint (token.color)
                    g2d.fill (token.shape)
                    drawTokenLabel (g2d, token)
                end for
            end for

            // Display all edges: pavement → dashed lane markings → labels/tokens
            val defaultStroke = g2d.getStroke                 // save default stroke
            val edges = graph.edges.toList

            // Layer 1: wide dark pavement behind each lane
            g2d.setStroke (pavementStroke)
            for edge <- edges do
                g2d.setPaint (pavementColor)
                g2d.draw (edge.shape)
            end for

            // Layer 2: dashed lane markings along each lane center
            g2d.setStroke (dashStroke)
            for edge <- edges do
                g2d.setPaint (dashColor)
                g2d.draw (edge.shape)
            end for

            // Layer 3: thin edge outline in lane color
            g2d.setStroke (roadStroke)
            for edge <- edges do
                g2d.setPaint (edge.color)
                g2d.draw (edge.shape)
            end for

            g2d.setStroke (defaultStroke)                     // restore for labels/tokens
            val badgeFont = new Font ("SansSerif", java.awt.Font.BOLD, 10)
            for edge <- edges do
                val x = edge.shape.getCenterX.asInstanceOf [Float]
                val y = edge.shape.getCenterY.asInstanceOf [Float]
                g2d.setPaint (fgColor)
                g2d.drawString (edge.label, x, y)
                val edge_tokens = edge.tokens.toList
                for token <- edge_tokens if token.shape.getWidth () > 0.0 do
                    drawTokenGlow (g2d, token)
                    g2d.setPaint (token.color)
                    g2d.fill (token.shape)
                    drawTokenLabel (g2d, token)
                end for

                // ── #12  Vehicle count badge ───────────────────────────────
                val nTok = edge_tokens.size
                if nTok > 0 then
                    val countStr = nTok.toString
                    g2d.setFont (badgeFont)
                    val bfm  = g2d.getFontMetrics
                    val bsw  = bfm.stringWidth (countStr)
                    val brad = (math.max (bsw, bfm.getHeight) / 2 + 4).toInt
                    val bx   = x.toInt
                    val by   = (y - 14).toInt                // slightly above edge center
                    val prevComp = g2d.getComposite
                    g2d.setComposite (java.awt.AlphaComposite.getInstance (
                        java.awt.AlphaComposite.SRC_OVER, 0.85f))
                    g2d.setPaint (new Color (30, 30, 50))
                    g2d.fillOval (bx - brad, by - brad, brad * 2, brad * 2)
                    g2d.setComposite (prevComp)
                    g2d.setPaint (white)
                    g2d.drawString (countStr, (bx - bsw / 2.0f), (by + bfm.getAscent / 2.0f - 1))
                end if
            end for

            // Display all free tokens
            val free_tokens = graph.freeTokens.toList
            for token <- free_tokens if token.shape.getWidth () > 0.0 do
                drawTokenGlow (g2d, token)
                g2d.setPaint (token.color)
                g2d.fill (token.shape)
                drawTokenLabel (g2d, token)
            end for

            // ── HUD overlay (screen-space, fixed position) ──────────────────
            val savedTransform = g2d.getTransform
            g2d.setTransform (new AffineTransform ())         // reset to identity (screen coords)
            drawHUD (g2d)
            g2d.setTransform (savedTransform)                 // restore world transform
        end paintComponent

        /** Draw a translucent HUD panel in the top-left corner with sim stats. */
        private def drawHUD (g2d: Graphics2D): Unit =
            import java.awt.AlphaComposite

            val pad    = 16
            val lineH  = 28
            val barH   = 10                                    // height of each density bar
            val barMaxW = 120                                  // max width of density bar (pixels)

            // ── Collect on-road token count ─────────────────────────────────
            var onRoad = 0
            for e <- graph.edges.toList do onRoad += e.tokens.size
            for t <- graph.freeTokens.toList do if t.shape.getWidth () > 0.0 then onRoad += 1

            // ── Build text lines ────────────────────────────────────────────
            val lines  = ArrayBuffer.empty [(String, Color, Boolean)]  // (text, color, isBold)

            lines.addOne ((s"Model:  $hudModelName", fgColor, true))
            val wallSec  = clock.toLong
            val wallHr24 = 6 + (wallSec / 3600).toInt          // base 6 AM
            val wallMin  = ((wallSec % 3600) / 60).toInt
            val ampm     = if wallHr24 >= 12 then "PM" else "AM"
            val wallHr12 = { val h = wallHr24 % 12; if h == 0 then 12 else h }
            lines.addOne ((f"Clock:  $wallHr12%d:$wallMin%02d $ampm  ($clock%,.0f s)", fgColor, false))
            lines.addOne ((s"Vehicles:  $onRoad on-road  /  $totalActorCount total", fgColor, false))
            lines.addOne ((f"Throughput: $hudThroughput%,.0f veh/hr", fgColor, false))
            lines.addOne ((f"Avg Speed:  $hudAvgSpeed%.1f m/s  (${hudAvgSpeed * 2.237}%.0f mph)", fgColor, false))
            lines.addOne ((f"Playback:   ${speedFactor}%.2fx", fgColor, false))

            if paused.get () then lines.addOne (("▐▐  PAUSED", red, true))

            // spacer + legend
            lines.addOne (("", fgColor, false))
            lines.addOne (("Speed Legend", fgColor, true))
            lines.addOne (("  ● free-flow",  new Color (0, 200, 0), false))
            lines.addOne (("  ● moderate",   new Color (220, 220, 0), false))
            lines.addOne (("  ● congested",  new Color (220, 40, 40), false))

            // segment density header (only if data available)
            val densities = hudSegDensities
            val segLabels = hudSegLabels
            val hasDensity = densities != null && densities.nonEmpty
            if hasDensity then
                lines.addOne (("", fgColor, false))
                lines.addOne (("Segment Density (veh/km)", fgColor, true))

            // ── Measure box size ────────────────────────────────────────────
            g2d.setFont (hudFont)
            val fmBold  = g2d.getFontMetrics
            g2d.setFont (hudFontSmall)
            val fmPlain = g2d.getFontMetrics
            val maxW    = lines.map { case (txt, _, bold) =>
                if bold then fmBold.stringWidth (txt) else fmPlain.stringWidth (txt)
            }.max
            // ensure box is wide enough for density bars
            val densityRowW = if hasDensity then 50 + barMaxW + 40 else 0    // label + bar + value
            val boxW = math.max (maxW, densityRowW) + pad * 2 + 12
            val densityH = if hasDensity then densities.length * (barH + 6) + 4 else 0
            val boxH = lines.length * lineH + pad * 2 + densityH

            // ── Semi-transparent background with AlphaComposite ─────────────
            val prevComposite = g2d.getComposite
            g2d.setComposite (AlphaComposite.getInstance (AlphaComposite.SRC_OVER, 0.88f))
            g2d.setPaint (hudBg)
            g2d.fillRoundRect (pad, pad, boxW, boxH, 14, 14)
            g2d.setComposite (prevComposite)

            // border
            g2d.setPaint (new Color (80, 80, 100))
            g2d.drawRoundRect (pad, pad, boxW, boxH, 14, 14)

            // ── Draw text lines ─────────────────────────────────────────────
            for i <- lines.indices do
                val (txt, color, bold) = lines (i)
                if txt.nonEmpty then
                    g2d.setFont (if bold then hudFont else hudFontSmall)
                    g2d.setPaint (color)
                    g2d.drawString (txt, pad + pad / 2, pad + lineH * (i + 1))
            end for

            // ── Draw segment density mini bar chart ─────────────────────────
            if hasDensity then
                val maxDensity = math.max (1.0, densities.max)  // avoid div-by-zero
                val startY = pad + lines.length * lineH + 4
                val labelX = pad + pad / 2
                g2d.setFont (new Font ("SansSerif", java.awt.Font.PLAIN, 12))
                val fm12 = g2d.getFontMetrics

                for i <- densities.indices do
                    val y = startY + i * (barH + 6)
                    // label
                    val lbl = if i < segLabels.length then segLabels (i) else s"S$i"
                    g2d.setPaint (fgColor)
                    g2d.drawString (lbl, labelX, y + barH - 1)

                    // bar
                    val barX = labelX + 45
                    val ratio = densities (i) / maxDensity
                    val bw = (ratio * barMaxW).toInt
                    // color: green (low) → yellow → red (high jam density ~150 veh/km)
                    val hue = ((1.0 - math.min (1.0, densities (i) / 150.0)) * 120.0 / 360.0).toFloat
                    g2d.setPaint (java.awt.Color.getHSBColor (hue, 0.8f, 0.9f))
                    g2d.fillRoundRect (barX, y, math.max (2, bw), barH, 4, 4)

                    // value text
                    g2d.setPaint (fgColor)
                    g2d.drawString (f"${densities (i)}%.0f", barX + bw + 4, y + barH - 1)
                end for
            end if
        end drawHUD

        /** Draw a soft glow/shadow behind a token to visually separate it from the road. */
        private def drawTokenGlow (g2d: Graphics2D, token: graph.Token): Unit =
            val b = token.shape.getBounds2D
            if b.getWidth <= 0.0 then return
            val c     = token.color
            val glow  = new Color (c.getRed, c.getGreen, c.getBlue, 55)
            val grow  = 4.0                                    // pixels larger on each side
            val prevComposite = g2d.getComposite
            g2d.setComposite (java.awt.AlphaComposite.getInstance (java.awt.AlphaComposite.SRC_OVER, 0.5f))
            g2d.setPaint (glow)
            g2d.fill (new java.awt.geom.RoundRectangle2D.Double (
                b.getX - grow, b.getY - grow,
                b.getWidth + grow * 2, b.getHeight + grow * 2, 6, 6))
            g2d.setComposite (prevComposite)
        end drawTokenGlow

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
            if c.color != null then ani.setPaintToken (c.eid, c.color)
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
        println(s"DgAnimator.run: unified loop start (mode=$playbackMode)")
        var nCmds = 0
        lastTime = clock
        while running.get() do
            // PAUSE GATE (shared for Live & Replay)
            pauseLock.synchronized {
                while paused.get() && !stepOnce.get() do pauseLock.wait()
            }

            playbackMode match
            case PlaybackMode.Live =>
                // Termination conditions for Live mode
                if cmdQ.isEmpty then
                    if aniDone then
                        println("DgAnimator.run: Live mode complete (queue empty & aniDone)")
                        running.set(false)
                        emptySince = -1L
                    else
                        // Start or continue idle timing
                        if emptySince < 0 then emptySince = System.nanoTime()
                        else
                            val idleNanos = System.nanoTime() - emptySince
                            if idleNanos > 250_000_000L then   // > 250 ms idle with empty queue => auto-finalize
                                println("DgAnimator.run: auto-finalizing Live mode after idle empty queue.")
                                aniDone = true
                                running.set(false)
                                playbackMode = PlaybackMode.Idle
                                emptySince = -1L
                            end if
                        end if
                        if running.get() then Thread.sleep(5)   // brief idle wait only if still running
                    end if
                else
                    // Queue has commands: reset idle timer
                    emptySince = -1L
                    val cmd = cmdQ.poll()
                    val when = cmd.time
                    val dt   = math.max(0.0, (when - lastTime) * aniRatio * ani.timeDilationFactor)
                    if dt > 0 then Thread.sleep(round(dt))
                    clock  = when
                    nCmds += 1
                    invokeCommand(cmd)
                    // Record only during Live mode
                    frames += Frame(cmd)
                    if cmd.action == CreateToken then actorCount += 1
                    repaint()
                    lastTime = clock
                    if stepOnce.compareAndSet(true, false) then
                        paused.set(true); playPauseBtn.setText("Play")
                    end if
                end if

            case PlaybackMode.Replay =>
                if replayIndex >= replayLength then
                    println("DgAnimator.run: Replay mode complete")
                    playbackMode = PlaybackMode.Idle
                    running.set(false)
                else
                    val f    = frames(replayIndex).cmd
                    val dt   = math.max(0.0, (f.time - lastTime) * aniRatio * ani.timeDilationFactor)
                    if dt > 0 then Thread.sleep(round(dt))
                    clock = f.time
                    invokeCommand(f)
                    if f.action == CreateToken then actorCount += 1
                    repaint()
                    lastTime = clock
                    replayIndex += 1
                    if stepOnce.compareAndSet(true, false) then
                        paused.set(true); playPauseBtn.setText("Play")
                    end if
                end if

            case PlaybackMode.Idle =>
                // Idle should not spin indefinitely; shut down if reached
                running.set(false)
            end match
        end while
        println(s"DgAnimator.run: unified loop end (processed=$nCmds, finalMode=$playbackMode, clock=$clock)")
    end run
//
//    def run(): Unit =
//        println(s"DgAnimator.run: modern loop start (mode=$playbackMode)")
//
//        val targetFPS = 120.0
//        val frameTimeNs = (1e9 / targetFPS).toLong
//
//        var lastFrameTime = System.nanoTime()
//        lastTime = clock
//
//        while running.get() do
//
//            // ---- PAUSE HANDLING ----
//            pauseLock.synchronized {
//                while paused.get() && !stepOnce.get() do pauseLock.wait()
//            }
//
//            val now = System.nanoTime()
//            val elapsedNs = now - lastFrameTime
//
//            if elapsedNs >= frameTimeNs then
//                lastFrameTime = now
//
//                playbackMode match
//
//                    // ================= LIVE =================
//                    case PlaybackMode.Live =>
//                        if !cmdQ.isEmpty then
//                            val cmd = cmdQ.poll()
//                            val when = cmd.time
//
//                            // Smooth time progression (no sleep)
//                            val dtSim = (when - lastTime) * aniRatio * ani.timeDilationFactor
//                            clock += math.max(0.0, dtSim)
//
//                            invokeCommand(cmd)
//
//                            // Record
//                            frames += Frame(cmd)
//
//                            if cmd.action == CreateToken then actorCount += 1
//
//                            repaint()
//                            lastTime = when
//
//                            if stepOnce.compareAndSet(true, false) then
//                                paused.set(true)
//                                playPauseBtn.setText("Play")
//
//                        else
//                            // auto finalize
//                            if aniDone then
//                                println("DgAnimator: Live complete")
//                                running.set(false)
//                            else
//                                Thread.`yield`() // non-blocking idle
//
//                    // ================= REPLAY =================
//                    case PlaybackMode.Replay =>
//                        if replayIndex >= replayLength then
//                            println("DgAnimator: Replay complete")
//                            playbackMode = PlaybackMode.Idle
//                            running.set(false)
//                        else
//                            val f = frames(replayIndex).cmd
//
//                            val dtSim = (f.time - lastTime) * aniRatio * ani.timeDilationFactor
//                            clock += math.max(0.0, dtSim)
//
//                            invokeCommand(f)
//
//                            if f.action == CreateToken then actorCount += 1
//
//                            repaint()
//                            lastTime = f.time
//                            replayIndex += 1
//
//                            if stepOnce.compareAndSet(true, false) then
//                                paused.set(true)
//                                playPauseBtn.setText("Play")
//
//                    // ================= IDLE =================
//                    case PlaybackMode.Idle =>
//                        running.set(false)
//
//            else
//                // ultra-light wait (no busy spin, no sleep jitter)
//                Thread.onSpinWait()
//
//        end while
//
//        println(s"DgAnimator.run: modern loop end (clock=$clock)")
//    end run

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start the animation by staring the animation thread. */
    def animate (tStart: Double, tStop: Double): Unit =
        // Prepare Live mode run
        clearRecording()                  // discard any prior run's frames
        playbackMode = PlaybackMode.Live  // enter Live mode
        clock    = tStart
        stopTime = tStop                  // retained for external reference (not hard loop condition now)
        lastTime = clock
        if !running.get() then
            running.set(true)
            new Thread(this).start()
        else
            println("animate: run loop already active; ignoring duplicate animate() call")
        end if
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

// Companion object added only to define VehicleState for inspector usage.
object DgAnimator:
    case class VehicleState(
        actorId: Int,
        label: String,
        laneId: String,
        pathInfo: String,
        segDisp: Double,
        pathDisp: Double,
        velocity: Double,
        segmentIndex: Int = -1,
    )
end DgAnimator

// ...tests (dgAnimatorTest, dgAnimatorTest2, dgAnimatorTest3) can follow below if present...


