package scalation
package simulation
package process

import scala.collection.mutable.ArrayBuffer as VEC
import scala.runtime.ScalaRunTime.stringOf
import scala.util.control.Breaks.{break, breakable}
import scalation.animation.CommandType.*
import scalation.random.*
import scalation.scala2d.Colors.*

import java.awt.geom.RoundRectangle2D
//import scalation.simulation.process.SimActor.removeFromAlist

//::::::::::::::::::for my model::::::::::::::::::::::::::::
// VSource now uses RowTimeLoader trait for generic model support


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//Vsource for my use case in OneWayVehicle2L
/** The `VSource` class is used to periodically inject entities (`Vehicle`s) into a
 *  running simulation model.  May act as an arrival generator.  VSource is both
 *  a simulation `Component` and special `SimActor` and therefore runs in own thread.
 *  @param name          the name of the source
 *  @param director      the director controlling the model
 *  @param makeEntity    the function to make entities of a specified type
 *  @param esubtype      indicator of the subtype of the entities to be made
 *  @param units         the number of entities to make
 *  @param iArrivalTime  the inter-arrival time distribution
 *  @param loc           the location of the source (x, y, w, h)
 */
class VSource (name: String, director: Model, makeEntity: () => Vehicle,
               esubtype: Int, units: Int,
               iArrivalTime: Variate, loc: Array [Double], prefix: String = null)
    extends Source (name, director, null, esubtype, units, iArrivalTime, loc):

    private val debug = debugf ("VSource", false)                             // debug function

    private[process] val ew = new EasyWriter("recorder", "VsourceWriter.txt")
    ew.off()

    // Speed data removed from constructor-time initialization
    // Now retrieved dynamically in act() based on director type
    // This allows corridor-agnostic operation

    // Per-source sequence for human-readable labels (e.g., M-1, R1-3): NEW
    /** Monotonic per-source counter used solely for display labels on tokens.
     * Example: for mainline (M), the sequence produces M-1, M-2, ...; for ramp 1,
     * R1-1, R1-2, ...
     * Notes:
     * - This does NOT affect simulation ids or logic (purely presentation).
     * - Reset occurs on JVM restart; not reset between replications by default.
     */
    private var seq = 0

    /** Derive a short source prefix from the entity subtype. NEW
     * @return "M" for mainline (subtype == 0), else "R<subtype>" (e.g., R1, R2)
     * Used by: label construction below to preserve origin after merging.
     */
    private def srcPrefix: String =
        if prefix != null then prefix               // caller named it: use that
        else esubtype match                         // otherwise guess from subtype
            case 0 | 1 | 2 | 3 | 4 => "M"           // mainline
            case n             => s"R$n"     // ramps


    //        case 0 => "M"
    //        case n => s"R$n"


    debug ("Init", s"name = $name, located at ${stringOf (at)}")

    val nStop: Int = units     // need this nStop/Unit for ramp metering gate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name          the name of the source
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param esubtype      indicator of the subtype of the entities to me made
     *  @param units         the number of entities to make
     *  @param iArrivalTime  the inter-arrival time distribution
     *  @param xy            the (x, y) coordinates for the top-left corner of the source.
                             def this (name: String, director: Model, makeEntity: () => Vehicle, esubtype: Int,
                             units: Int, iArrivalTime: Variate, xy: (Double, Double)) =
        this (name, director, makeEntity, esubtype, units, iArrivalTime,
                             Array (xy._1, xy._2, 20.0, 20.0))
                             end this
     */
    override def act(): Unit =

        val sourceType = s"SRC$esubtype"
        this.subtype = esubtype   // ensure source subtype is set
        //ew.write(f"\n[$sourceType] STARTED | Budget=$units vehicles\n")

        for rep <- 1 to director.reps do {                         // MAJOR LOOP - replications
        actTime = director.clock // set to model start time

        breakable {
            debug("act", s"start making $units Vehicles")
            cfor (1 , units + 1) { i => // MINOR LOOP - make actors
                if director.stopped then
                    //println(s"VSource.act: simulation unexpectedly ended at ${director.clock}")
                    break() // terminate source, simulation ended
                //debug("act", s"make entity/vehicle $i")

                val actor = makeEntity() // make new actor/vehicle
                actor.laneID = subtype   // temporary — Car.act() sets the real laneID
                //debug("act", s"after make entity/vehicle $i: actor = $actor")
                actor.mySource = this // actor's source
                actor.subtype = esubtype // set the entity subtype
                seq += 1   // increment per-source sequence labeller
                actor.setDisplayLabel(s"$srcPrefix-$seq") // NEW: set human-friendly display label


                director.recordGenerated (this)
                if director.isAnimating then director.dgAni.updateActorCount(director.numActors) // korede
                //director.log.trace(this, "generates", actor, director.clock)
                val carShape = new RoundRectangle2D.Double (0, 0, 14, 7, 4, 4)   // car-shaped token
                director.animate(actor, CreateToken, randomColor(actor.id), carShape,
                    Array(at(0) + at(2) + RAD / 2.0, at(1) + at(3) / 2.0 - RAD, 14.0, 7.0))
                //debug("act", s"schedule actor $i")

                //debug("act", s"after schedule actor $i")

                // PeMS lookup, only for models that carry per-row data.  Everything
                // BELOW this block -- releasing the actor and waiting out the
                // inter-arrival gap -- is what a source does regardless, so it must
                // not sit inside this `if`.  It used to, which meant a model that is
                // not a `RowTimeLoader` created its actors and never scheduled a
                // single one: no events, empty agenda, run over at clock 0.
                // `mu` is left NaN when there is no data, and gen1 is skipped for it.
                var mu = Double.NaN
                if director.isInstanceOf[RowTimeLoader] then
                    val timeModel = director.asInstanceOf[RowTimeLoader]
                    timeModel.nextRow(director.clock) // update current row if needed

                    val currentRow = timeModel.getCurrentRow(director.clock)   // get the current row based on clock time
                    val safeRow = math.min(currentRow, timeModel.getDataDimension - 1)  // ensure row index is within bounds

                    // Get mu (inter-arrival time) from model
                    mu = timeModel.getMuForSource(this.subtype)(safeRow)

                    // Get speed data from model (delegates to model's data source)
                    val speedMatrix = timeModel.getSpeedMatrix()
                    val safeLane  = math.min(actor.laneID, speedMatrix.dim2 - 1)
                    val laneSpeed = speedMatrix(safeRow, safeLane)

                    // Both entry speed and desired speed come from the PeMS lane
                    // speed for this bin, so the corridor tracks the observed data
                    // rather than a sampled free-flow speed.
                    //
                    // Known consequence, not a bug: `vmax` also caps the head of a
                    // queue.  `iDMFree` gives a = amax*(1 - (v/vmax)^4), so a car
                    // with no leader tops out at exactly vmax, and a queue cannot
                    // discharge faster than its head car.  In a bin where the
                    // detector saw 25 mph, the lane discharges at 25 mph.
                    actor.velocity = laneSpeed // set initial velocity of the vehicle
                    actor.vmax     = laneSpeed // set vmax of the vehicle to lane speed
                end if

                actor.schedule(0.0)                    // release the vehicle into the model

                if i < units then                      // last one has nobody to wait for
                    // Inter-arrival time.  With PeMS data the distribution is
                    // re-parameterised to the row's mean `mu`; without it, the
                    // distribution is used as configured.
                    val duration =
                        if mu.isNaN then iArrivalTime.gen
                        else iArrivalTime match
                            case erlang: Erlang =>
                                val muPerStage = mu / erlang.k // 2
                                iArrivalTime.gen1(muPerStage)
                            case erlang2S: Erlang2S =>
                                val muPerStage = (mu - erlang2S.tau) / 2.0
                                iArrivalTime.gen1(muPerStage)
                            case _ =>
                                iArrivalTime.gen1(mu)

                    tally(duration) // tally duration
                    schedule(duration)
                    yieldToDirector() // yield and wait duration time units
                end if

            }
        } // breakable

        ew.flush()
        if rep < director.reps then
            //director.log.trace(this, "wait for next rep", director, director.clock)
            yieldToDirector() // yield and wait for next replication
        }

        //director.log.trace(this, "terminates", null, director.clock)
        yieldToDirector(true) // yield and terminate
    end act

end VSource

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VSource` companion object provides a builder method for sources.
 */
object VSource:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a source using defaults for width 'w' and height 'h'.
     *  @param name          the name of the source
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param esubtype      indicator of the subtype of the entities to me made
     *  @param units         the number of entities to make
     *  @param iArrivalTime  the inter-arrival time distribution
     *  @param xy            the (x, y) coordinates for the top-left corner of the source.
     */
    def apply (name: String, director: Model, makeEntity: () => Vehicle, esubtype: Int, units: Int,
               iArrivalTime: Variate, xy: (Int, Int), prefix: String = null): VSource =
        new VSource (name, director, makeEntity, esubtype, units, iArrivalTime,
            Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0), prefix)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related sources using defaults for width 'w' and height 'h'.
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param units         the number of entities to make
     *  @param xy            the (x, y) coordinates for the top-left corner of the reference source.
     *  @param src           repeated source specific info: name, subtype, distribution, offset
     */
    //    def group (director: Model, makeEntity: () => Vehicle, units: Int, xy: (Int, Int),
    ///               src: (String, Int, Variate, (Int, Int))*): List [VSource] =
    //        val sourceGroup = new VEC [VSource] ()
    //        for s <- src do sourceGroup += VSource (s._1, director, makeEntity, s._2, units, s._3,
    //                                              (xy._1 + s._4._1, xy._2 + s._4._2))
    ///        sourceGroup.toList
    //    end group

    def group(director: Model, makeEntity: () => Vehicle, xy: (Int, Int),
              src: (String, Int, Variate, Int, (Int, Int))*): List[VSource] =
        val sourceGroup = new VEC[VSource]()
        for s <- src do
            val name = s._1
            val subtype = s._2
            val dist = s._3
            val units = s._4
            val offset = s._5
            val position = (xy._1 + offset._1, xy._2 + offset._2)
            sourceGroup += VSource(name, director, makeEntity, subtype, units, dist, position)
        sourceGroup.toList
    end group


end VSource


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `vSourceTest` main function tests the `VSource` class by generating several Car objects.
 *  > runMain scalation.simulation.process.vSourceTest
 */
@main def vSourceTest (): Unit =

    import scalation.random.Uniform

    object CarModel extends Model ("CarModel"):

        val maker = VSource ("maker", CarModel, () => Car (), 0, 5, Uniform (2000, 5000), (100, 200))

        addComponent (maker)

        case class Car () extends Vehicle ("c", CarModel):
            override def act (): Unit = println ("act")
        end Car

    end CarModel

    CarModel.simulate ()
    CarModel.waitFinished ()
    Model.shutdown ()

end vSourceTest
