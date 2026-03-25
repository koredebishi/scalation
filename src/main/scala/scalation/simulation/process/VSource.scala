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
               iArrivalTime: Variate, loc: Array [Double])
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
    private def srcPrefix: String = esubtype match
        case 0 | 1 | 2 | 3 | 4 => "M"               // mainline
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

        val rep = 1
        //for rep <- 1 to director.reps do // MAJOR LOOP - replications
        actTime = director.clock // set to model start time

        breakable {
            debug("act", s"start making $units Vehicles")
            cfor (1 , units + 1) { i => // MINOR LOOP - make actors
                if director.stopped then
                    //println(s"VSource.act: simulation unexpectedly ended at ${director.clock}")
                    break() // terminate source, simulation ended
                //debug("act", s"make entity/vehicle $i")

                val actor = makeEntity() // make new actor/vehicle
                actor.laneID = math.min(3, subtype)   // set laneID to subtype for later use in Vehicle
                //debug("act", s"after make entity/vehicle $i: actor = $actor")
                actor.mySource = this // actor's source
                actor.subtype = esubtype // set the entity subtype
                seq += 1   // increment per-source sequence labeller
                actor.setDisplayLabel(s"$srcPrefix-$seq") // NEW: set human-friendly display label


                director.numActors += 1 // number of actors created by all sources, so far
                if director.isAnimating then director.dgAni.updateActorCount(director.numActors) // korede
                //director.log.trace(this, "generates", actor, director.clock)
                val carShape = new RoundRectangle2D.Double (0, 0, 14, 7, 4, 4)   // car-shaped token
                director.animate(actor, CreateToken, randomColor(actor.id), carShape,
                    Array(at(0) + at(2) + RAD / 2.0, at(1) + at(3) / 2.0 - RAD, 14.0, 7.0))
                //debug("act", s"schedule actor $i")

                //debug("act", s"after schedule actor $i")

                // Generic check: works with any model that implements RowTimeLoader (CalRoute101_3, etc.)
                if director.isInstanceOf[RowTimeLoader] then
                    val timeModel = director.asInstanceOf[RowTimeLoader]
                    timeModel.nextRow(director.clock) // update current row if needed
                    if i < units then

                        val currentRow = timeModel.getCurrentRow(director.clock)   // get the current row based on clock time
                        val safeRow = math.min(currentRow, timeModel.getDataDimension - 1)  // ensure row index is within bounds

                        // Get mu (inter-arrival time) from model
                        val mu = timeModel.getMuForSource(this.subtype)(safeRow)

                        // Get speed data from model (delegates to model's data source)
                        val speedMatrix = timeModel.getSpeedMatrix()
                        val laneSpeed = speedMatrix(safeRow, actor.laneID)

                        actor.velocity = laneSpeed // set initial velocity of the vehicle
                        actor.vmax     = laneSpeed // set vmax of the vehicle to lane speed
                        actor.schedule(0.0)

                        // Generate inter-arrival time based on distribution type
                        val duration = iArrivalTime match
                            case erlang: Erlang =>
                                val muPerStage = mu / erlang.k // 2
                                iArrivalTime.gen1(muPerStage)
                            case erlang2S: Erlang2S =>
                                //println(s"Arrival model is ${iArrivalTime.toString}")
                                val muPerStage = (mu - erlang2S.tau) / 2.0
                                iArrivalTime.gen1(muPerStage)
                            case _ =>
                                //println(s"Arrival model is ${iArrivalTime.toString}")
                                iArrivalTime.gen1(mu)
                        val ctime = director.clock // clock time
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
        //end for

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
               iArrivalTime: Variate, xy: (Int, Int)): VSource =
        new VSource (name, director, makeEntity, esubtype, units, iArrivalTime,
            Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0))
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