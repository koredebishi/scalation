//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Base Model Class for Process Simulation - Method of Independent Replications
 */

package scalation
package simulation
package process

import scala.collection.mutable.{ArrayBuffer => VEC}
import scala.collection.mutable.{LinkedHashMap, PriorityQueue}
import scala.runtime.ScalaRunTime.stringOf

import scalation.animation.{AnimateCommand, CommandType, DgAnimator, OsmRoadNetwork}
import scalation.mathstat._
import scalation.scala2d.Colors._
import scalation.scala2d.Shape

import scala.collection.mutable.ArrayBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model` class maintains a list of components making up the model and
 *  controls the flow of entities (`SimActor`s) through the model, following the
 *  process-interaction world-view.  It maintains a time-ordered priority queue
 *  to activate/re-activate each of the entities.  Each entity (`SimActor`) is
 *  implemented as a `Coroutine` and may be thought of as running in its own thread.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param full       generate a full report with both sample and time-persistent statistics
 *  @param width      the width of the animation panel
 *  @param height     the height of the animation panel
 */
class Model (name: String, val reps: Int = 1, animating: Boolean = true, aniRatio: Double = 1.0,
             val full: Boolean = true, weight: Int = 1200, height: Int = 800)
      extends Coroutine (name)
         with Completion
         with Modelable
         with Component:

    initComponent (name, Array ())

    private val debug = debugf ("Model", false)                      // debug function
    private val flaw  = flawf ("Model")                             // flaw function



    private [process] val log       = Monitor ("simulation")        // log for model execution
    private [process] var numActors = 0                             // number of actors created so far

    protected var startTime = 0.0                                   // time at which the simulation is to begin
    protected val agenda    = PriorityQueue.empty [SimActor]        // agenda of things to be done (time-ordered activation list)
    protected var _theActor: SimActor = null                        // currently acting actor (act one at a time)

    def isAnimating:Boolean = animating             // getter for animating;

    // ── HUD stats push throttle (base infrastructure — works for any model) ──
    private var hudPushCounter = 0                                 // counts actor activations between HUD updates
    private val HUD_PUSH_INTERVAL = 50                             // push HUD data every N actor activations

    director = this
    debug ("init", s"make ${director.name} with cor_id $id the director")


    private val statV    = LinkedHashMap [String, VectorD] ()       // map of stat-vectors recording means of each rep
    //private val stopTime = MAX_VALUE                                // max stop time for the model
    private var stopTime = MAX_VALUE                                // max stop time for the model

    def setTime(sTime: Double):Unit =
        stopTime =  sTime

    def getStopTime: Double = stopTime

    private val parts    = VEC [Component] ()                       // List (VEC) of Components making up the model

    /** The animation engine
     */
    private [simulation] val dgAni = if animating then new DgAnimator ("Process Animator",
                                                          new Color (0xD4D4D4), new Color (0x1E1E2E),
                                                          aniRatio, weight, height)
                        else null

    /** The animation engine's command queue
     */
    private val aniQ = if animating then dgAni.getCommandQueue
                       else null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add component parts to the model.
     *  @param _parts  the component parts
     */
    def addComponent (_parts: Component*): Unit = for p <- _parts do parts += p

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add lists of component parts to the model.
     *  @param _parts  the lists of component parts (need List rather than VEC)
     */
    def addComponents (_parts: List [Component]*): Unit = for p <- _parts; q <- p do parts += q

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load an OpenStreetMap road network as background map context.
     *  If the JSON file does not exist, it is auto-downloaded from the Overpass
     *  API using the Python script.  Bounding box is computed from `gpsAnchors`
     *  with a margin.  After first download, the cached file is reused.
     *
     *  Any model subclass can call this to add geographic context.
     *  @param jsonPath    path to cached OSM JSON (e.g., "data/osm/eaton_roads.json")
     *  @param gpsAnchors  junction GPS coordinates (lat, lon) — for projection alignment
     *  @param dims        screen canvas dimensions (width, height)
     */
    protected def loadOsmBackground (jsonPath: String,
                                      gpsAnchors: Array [(Double, Double)],
                                      dims: (Double, Double)): Unit =
        if dgAni == null || gpsAnchors.isEmpty then return

        // ── Auto-download if JSON doesn't exist ─────────────────────────
        val file = new java.io.File (jsonPath)
        if !file.exists () then
            val lats = gpsAnchors.map (_._1)
            val lons = gpsAnchors.map (_._2)
            val margin = 0.01                                  // ~1 km padding
            val south = lats.min - margin
            val north = lats.max + margin
            val west  = lons.min - margin
            val east  = lons.max + margin
            // Derive area name from filename: "data/osm/eaton_roads.json" → "eaton"
            val areaName = file.getName.replace ("_roads.json", "")
            val outDir   = file.getParent
            val script   = "src/main/scala/scalation/simulation/scripts/download_osm_geometry.py"
            val cmd = Array ("python", script,
                "--south", f"$south%.6f", "--west",  f"$west%.6f",
                "--north", f"$north%.6f", "--east",  f"$east%.6f",
                "--name",  areaName,      "--outdir", outDir)
            println (s"OSM JSON not found: $jsonPath — downloading from Overpass API...")
            try
                val proc = Runtime.getRuntime.exec (cmd)
                val exit = proc.waitFor ()
                if exit == 0 then println (s"OSM download complete: $jsonPath")
                else
                    val err = scala.io.Source.fromInputStream (proc.getErrorStream).mkString
                    println (s"OSM download failed (exit=$exit): $err")
            catch case e: Exception =>
                println (s"OSM download error: ${e.getMessage}")
        end if

        // ── Load and push to animator ───────────────────────────────────
        val net = OsmRoadNetwork.load (jsonPath, gpsAnchors, dims)
        dgAni.setBackgroundRoads (net.polylines, net.roadTypes)
        if net.places.nonEmpty then dgAni.setBackgroundPlaces (net.places)
    end loadOsmBackground

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current acting actor.
     */
    def theActor: SimActor = _theActor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indicate whether the model has been stopped.
     */
    def stopped: Boolean = ! simulating

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the agenda and stateful components for next replication.
     */
    def reset (): Unit =
        banner ("Model.reset in progress")

        // reset the agenda - activation priority queue
        while ! agenda.isEmpty do agenda.dequeue ()                 // clean out actors from agenda

        // reset stateful components
        for p <- parts do
            if p.isInstanceOf [Source] then                         // reset sources
                val s = p.asInstanceOf [Source]
                reschedule (s)
            if p.isInstanceOf [WaitQueue] then                      // reset wait queues
                val w = p.asInstanceOf [WaitQueue]
                while ! w.isEmpty do w.dequeue ()
        end for
    end reset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the simulation (includes scheduling all Sources) returning summary
     *  statistics.
     */
    def simulate (_startTime: Double = 0.0): Unit =
        startTime = _startTime
        _clock = startTime
        log.trace (this, "starts", this, _clock)

        for p <- parts do
            log.trace (this, s"establish x = ${p.at(0)}, y = ${p.at(1)}", p, _clock)
            p.director = this
            for q <- p.subpart do q.director = this
            if p.isInstanceOf [Source] then reschedule (p.asInstanceOf [Source]) 
        end for

        start ()                                                    // start the director thread/actor -> act ()
    end simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cleanup the agenda and any stateful components.  Any actors left in the
     *  agenda or a wait queue must be terminated.  The model (i.e., the director)
     *  must be terminated as well.
     */
    def cleanup (): Unit =
        banner ("Model.cleanup in progress")

        println ("cleanup: agenda")
        while ! agenda.isEmpty do                                   // cleanup actors left on agenda
            val a = agenda.dequeue ()
            if a != this then
                println (s"cleanup: terminate actor $a in agenda")
                a.interrupt ()                                      // terminate all actors, except director
        end while

        println ("cleanup: wait queues")
        for p <- parts do
            if p.isInstanceOf [WaitQueue] then                      // cleanup wait queues
                val w = p.asInstanceOf [WaitQueue]
                while ! w.isEmpty do
                    val a = w.dequeue ()
                    println (s"cleanup: terminate actor $a in $w")
                    a.interrupt ()                                  // terminate all actors in queue
        end for
    end cleanup

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Schedule (first time) or reschedule (subsequent times) an actor to act.
     *  @param actor  the actor to be scheduled
     */
    def reschedule (actor: SimActor): Unit = agenda += actor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Push live statistics to the animation HUD.
     *  Scans model `parts` for VTransport segments, aggregates vehicle counts,
     *  average speed, and per-segment density.  Called periodically from the
     *  scheduling loop so that **any** model inheriting from `Model` gets a
     *  live HUD without model-specific wiring.
     */
    private def pushHudStats (): Unit =
        if !animating || dgAni == null then return

        val vtSegs     = ArrayBuffer.empty [VTransport]
        val segLabels  = ArrayBuffer.empty [String]

        // VTransport segments live inside Pathway.subpart (or Pathway.seg),
        // which in turn lives inside parts.  We must recurse one level.
        for p <- parts do
            p match
                case pw: Pathway =>
                    for s <- pw.seg if s != null do
                        vtSegs    += s
                        segLabels += s.name
                case vt: VTransport =>          // in case a VTransport is added directly
                    vtSegs    += vt
                    segLabels += vt.name
                case _ =>   // ignore non-transport parts

        // -- per-segment density (veh/km) and aggregate speed ----------------
        var totalVehicles = 0
        var sumVelocity   = 0.0
        val densities     = new Array [Double] (vtSegs.length)
        val velocities    = scala.collection.mutable.ArrayBuffer.empty [Double]   // for median

        for i <- vtSegs.indices do
            val vt    = vtSegs(i)
            val nVeh  = vt.vCount                              // O(1) — was: vt.vdeque.size
            val lenKm = vt.length / 1000.0                     // convert metres → km
            densities(i) = if lenKm > 0.0 then nVeh.toDouble / lenKm else 0.0
            totalVehicles += nVeh
            for v <- vt.vList do
                sumVelocity += v.velocity
                velocities  += v.velocity                       // collect for median
            end for
        end for

        val meanSpeed = if totalVehicles > 0 then sumVelocity / totalVehicles else 0.0
        // Median speed — robust to stopped-car tail at jammed merges
        val medSpeed = if velocities.isEmpty then 0.0 else
            val sorted = velocities.toArray.sorted
            val n = sorted.length
            if n % 2 == 1 then sorted (n / 2)
            else (sorted (n / 2 - 1) + sorted (n / 2)) / 2.0
        val elapsed    = _clock - startTime
        val throughput = if elapsed > 0.0 then numActors / (elapsed / 3600.0) else 0.0   // veh/hr

        dgAni.updateHudStats (throughput, meanSpeed, medSpeed)
        if vtSegs.nonEmpty then
            dgAni.updateSegmentDensities (densities, segLabels.toArray)
    end pushHudStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The model itself is an Actor (not an ordinary `SimActor`) and may be
     *  thought of as the director.  The director iteratively manages the clock
     *  and the agenda of actors until the simulation flag becomes false
     *  or the agenda (priority queue) becomes empty.
     */
//    override def act (): Unit =
//        log.trace (this, s"starts model for $reps replications", null, _clock)
//
//        for rep <- 1 to reps do                                     // LOOP THROUGH REPLICATIONS
//            _clock = startTime
//            if rep == 1 && animating then display ()                // turn animation on (true) off (false)
//
//            log.trace (this, s"starts rep $rep", null, _clock)
////            simulating = _clock <= stopTime                         // simulate unless past stop time

//            while simulating && ! agenda.isEmpty do                 // INNER SCHEDULING LOOP
//                _theActor = agenda.dequeue ()                       // next from priority queue
//                if _theActor.actTime < clock then                   // out of order execution => QUIT
//                    flaw ("act", s"actor $_theActor activation time < $_clock")
//                    println ("QUIT")
//                    simulating = false
//                else
//                    _clock    = _theActor.actTime                   // advance the time
//                    if isAnimating then dgAni.updateActorCount(numActors) // send the total number of actors to the dgAnimator for display purpose.
//                    debug ("act", s"${this.me} resumes ${_theActor} at clock= $clock")
//                    log.trace (this, "resumes", _theActor, _clock)
//                    debug ("act", s"before yyield at clock $clock")
//                    yyield (_theActor)                              // director yields to actor
//                    debug ("act", s"after yyield at clock $clock")
//            end while
//
//            simulating = false
//            log.trace (this, s"ends rep $rep", null, _clock)
//
//            fini (rep)                                              // post-run results
//            if rep < reps then reset ()                             // reset for next replication
//            resetStats (rep)                                        // reset and aggregate statistics
//        end for
//
//        cleanup ()
//        if reps > 1 then reportV ()
//        println (s"coroutine counts = $counts")
//        log.trace (this, "terminates model", null, _clock)
//        hasFinished ()                                              // signal via semaphore that simulation is finished
//        yyield (null, true)                                         // yield and terminate the director
//    end act

    override def act(): Unit =
        log.trace(this, s"starts model for $reps replications", null, _clock)

        //for rep <- 1 to reps do                                         // LOOP THROUGH REPLICATIONS
        val rep = 1
        _clock = startTime                                          //Initialize the clock at StartTime
        if rep == 1 && animating then display()                     // turn animation on (true) off (false)

        log.trace(this, s"starts rep $rep", null, _clock)           // log this simulation

        simulating = true                                           // Start the simulation as true,

        while simulating && !agenda.isEmpty do                      // INNER SCHEDULING LOOP
            _theActor = agenda.dequeue()                            // get next actor from priority queue
            if _theActor.actTime < clock then
                flaw("act", s"actor $_theActor activation time < $_clock")
                println("QUIT")
                simulating = false                                  //stop the simulation and quit
            else
                _clock = _theActor.actTime                          // advance time

                if _clock > stopTime && _theActor.isInstanceOf[Source] then
                    println(s"Skipping Source actor due to time limit at clock = $clock")
                else
                    if isAnimating then dgAni.updateActorCount(numActors)
                    hudPushCounter += 1
                    if isAnimating && hudPushCounter >= HUD_PUSH_INTERVAL then
                        hudPushCounter = 0
                        pushHudStats ()
                    end if
                    //debug("act", s"${this.me} resumes ${_theActor} at clock= $clock")
                    //log.trace(this, "resumes", _theActor, _clock)
                    yyield(_theActor) // yield to actor
                    //debug ("act", s"after yyield at clock $clock")
            end if
        end while

        simulating = false
        log.trace(this, s"ends rep $rep", null, _clock)

        fini(rep)
        if rep < reps then reset()
        resetStats(rep)
        //end for

        cleanup()
        if reps > 1 then reportV()
        println(s"coroutine counts = $counts")
        log.trace(this, "terminates model", null, _clock)
        hasFinished()
        yyield(null, true)
    end act

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the components on the animation engine's queue.
     */
    def display (): Unit = for p <- parts do p.display ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put a node/token command on the animation queue.
     *  @param who    who is being animated
     *  @param what   what animation command
     *  @param color  the color the node/token
     *  @param shape  the shape of the node/token
     *  @param at     the location of the node/token
     */
    def animate (who: Identifiable, what: CommandType, color: Color, shape: Shape,
                 at: Array [Double]): Unit =
        if animating then
            val eid   = who.id
            //val = label = who.name
            // Use numeric id as label for tokens so vehicle/car tokens show their number
            val label = if what == CommandType.CreateToken then eid.toString else who.name
            debug ("animate", s"$label.$eid, $what, $color, $shape, ${stringOf (at)}")
            aniQ.add (AnimateCommand (what, eid, shape, label, true, color, at, _clock))
        end if
    end animate

    // Convenience: explicit-label variant to avoid overload ambiguity
    /** Enqueue an animation command with an explicit display label. NEW
      * @param who    Identifiable being animated (provides unique id used as eid)
      * @param what   Animation command (e.g., CreateToken)
      * @param color  Paint color for the node/token
      * @param shape  Shape to render (Ellipse, Rectangle, etc.)
      * @param at     Location/size array for the shape (x, y, w, h) or variant
      * @param label  The display label to show on the token/node (overrides name)
      * Behavior:
      * - Uses the supplied label for AnimateCommand.label; falls back to who.name
      *   if the supplied label is null/empty.
      * - Leaves internal ids and simulation logic unchanged; this is display-only.
      * Used by:
      * - VSource when creating vehicle tokens so the animator shows compact
      *   per-source labels (e.g., M-1, R1-7) on token heads for visual validation.
      */
    def animateWithLabel (who: Identifiable, what: CommandType, color: Color, shape: Shape,
                          at: Array [Double], label: String): Unit =
        if animating then
            val eid = who.id
            val lbl = if label != null && label.nonEmpty then label else who.name
            debug ("animateWithLabel", s"$lbl.$eid, $what, $color, $shape, ${stringOf (at)}")
            aniQ.add (AnimateCommand (what, eid, shape, lbl, true, color, at, _clock))
        end if
    end animateWithLabel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put a edge command on the animation queue.
     *  @param who    who is being animated
     *  @param what   what animation command
     *  @param color  the color the edge
     *  @param shape  the shape of the edge
     *  @param from   the location of the origination node
     *  @param to     the location of the destination node
     *  @param at     the location of the edge (empty array => implicitly determined)
     */
    def animate (who: Identifiable, what: CommandType, color: Color, shape: Shape,
                 from: Component, to: Component, at: Array [Double] = Array ()): Unit =
        if animating then
            val eid   = who.id
            val label = who.name
            debug ("animate", s"$label.$eid, $what, $color,+ $shape, ${from.me} ${to.me}, ${stringOf (at)}")
            aniQ.add (AnimateCommand (what, eid, shape, label, true, color, at, _clock, from.id, to.id))
        end if
    end animate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the order of actors based on their 'actTime's.
     *  @param actor1  the first actor in comparison
     */
    def orderedActor (actor1: SimActor): Ordered [SimActor] =
        new Ordered [SimActor]
            { def compare (actor2: SimActor) = actor1.actTime compare actor2.actTime }
    end orderedActor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Methods for Reporting Results
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Finish by producing statistical reports and optionally animation.
     *  Typically animation and reports in pop up window turned off for high
     *  replications and/or simulation optimization.
     *  @param rep  the replication number (1, ... reps)
     */
    protected def fini (rep: Int): Unit =
        //report ()                                                   // report in terminal
        if animating then
            reportF ()                                              // report in new window/frame
            if rep == 1 then dgAni.animate (0, 100000)              // only animate first rep
            dgAni.saveImage (DATA_DIR + name + ".png")
        end if
    end fini

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset and aggregate all statistics.
     *  @param rep   the current replication (1, ... reps)
     *  @param rmax  the maximum number of replications/batches
     */
    def resetStats (rep: Int, rmax: Int = reps): Unit =
        if rep == 1 then
            for stat <- getStatistics do statV += stat.name -> new VectorD (rmax)
        for stat <- getStatistics do
            statV (stat.name)(rep - 1) = stat.mean
            stat.reset ()
    end resetStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the statistical results of the simulation (statistics for each part).
     *  This includes the sample/duration statistics and if 'full', time persistent
     *  statistics as well.
     */
    def getStatistics: VEC [Statistic] =
        val stats = new VEC [Statistic] ()
        for p <- parts do
            if p.composite then p.aggregate ()
            stats += p.durationStat
        if full then
            for p <- parts if p.persistentStat != null do
                stats += p.persistentStat
//      for st <- stats do println (s"getStatistics: ${st.show}")
        stats
    end getStatistics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of a simulation run.
     */
    protected def report (): Unit =
        println (Statistic.line)
        println (Statistic.labels)
        println (Statistic.line)
        for stat <- getStatistics do println (stat)
        println (Statistic.line)
    end report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of the overall simulation as recorded
     *  in statV (may include multiple replications/batches).
     *  @param showMeans  whether to show the individual run/batch means
     */
    protected def reportV (showMeans: Boolean = false): Unit =
        banner (s"Summary over replications/batches")
        println (Statistic.line)
        println (Statistic.labels)
        println (Statistic.line)
        if showMeans then println (s"reportV: MEANS statV = $statV")
        for (k, v) <- statV do
            val aStat = new Statistic (k)
            aStat.tallyVec (v)
            println (aStat)
        println (Statistic.line)
    end reportV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of the simulation in a new GUI window/frame.
     */
    protected def reportF (): Unit = new StatTable (s"$name statistics", getStatistics)

end Model


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model` companion object provides a shutdown method.
 */
object Model:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shutdown the Model execution infrastructure (WARNING: this method should
     *  only be called right before program termination).  Make sure all threads
     *  have finished (e.g., call `waitFinished`), not just the main thread.
     *  If `shutdown` is not called, the application may hang.
     */

    def shutdown (): Unit =
        Recorder.shutdownRecorder()
        Coroutine.shutdown ()

end Model





















