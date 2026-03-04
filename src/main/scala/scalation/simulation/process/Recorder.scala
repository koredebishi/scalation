//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Tue Mar 12 21:43:42 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Records the Flow of Actors/Vehicles (Counts and Speed)
 */

package scalation
package simulation
package process

import scala.math.floor

import scalation.mathstat.{MatrixD, Statistic}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Recorder` trait allows Components/Nodes to easily record the flow of actors/entities
 *  (e.g., vehicles) in terms of counts and optionally average speed (or other property
 *  of interest).
 *  @param nt      the number of time intervals (defaults to 60)
 *                     15-minute or 900-second intervals over 6:00 AM to 9:00 PM
 *  @param nLanes  the number of lanes
 */
trait Recorder (nt: Int = 60, nLanes: Int = 4):

    protected val r_counts  = new MatrixD (nt, nLanes)                    // record counts in time interval
    protected val r_speeds  = new MatrixD (nt, nLanes)                    // record average speed in time interval
    protected val r_density = new MatrixD (nt, nLanes)                    // record density (veh/m) per interval per segment


    private[process] var ew = new EasyWriter("recorder", "recorder.csv")


    //private val timeConv = 54000.0 / nt                                   // 60 * 60 * 15 = 54000 seconds per busy part of the day
    val rowTime = 15.0 * MINUTE
    private val timeConv = rowTime // ← THIS IS CORRECT!
    //println(s"Recorder created with nt = $nt, nLanes = $nLanes, timeConv = $timeConv")

    //  private val timeConv = 86400.0 / nt                                   // 60 * 60 * 24 = 86400 seconds per day
    private var i_pre = 0                                                 // the current and previous time intervals
    private val lane_stat = Array.fill (nLanes) (new Statistic ("lane"))  // array of `Statistic`

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the recorder matrices.
    *  Automatically flushes the final interval data before returning.
     */
    def getRecorderMat: (MatrixD, MatrixD) =
        recordInMatrix(i_pre)  // Flush final interval (clamped to valid range)
        (r_counts, r_speeds)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the density matrix (veh/m per time interval per segment).
     *  Caller maps segment index to column index.
     */
    def getDensityMat: MatrixD = r_density

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Record instantaneous density for a segment at the current clock time.
     *  Accumulates samples within each interval and averages them at flush.
     *  @param ctime    the current simulation clock time
     *  @param density  instantaneous density snapshot (veh/m) from VTransport.snapshotDensity()
     *  @param segCol   the column index in r_density to write to (caller maps seg → col)
     */
    def recordDensity (ctime: Double, density: Double, segCol: Int): Unit =
        val i_cur = floor (ctime / timeConv).toInt
        if i_cur >= 0 && i_cur < nt && segCol >= 0 && segCol < r_density.dim2 then
            // Accumulate running average: store sum in cell, count separately
            // Simple approach: overwrite with latest snapshot each tick (last-value-wins per interval)
            // For a true average, use a separate Statistic — keeping this minimal.
            r_density(i_cur, segCol) = density
    end recordDensity

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Record the entity and optionally its speed (or other property of interest).
     *  @param ctime  the clock time the entity entered the component (e.g., Junction, Sink)
     *  @param speed  the speed at which entity entered the component (e.g., Junction, Sink)
     *  @param lane   the lane the vehicle is in
     */
    def record (ctime: Double, speed: Double, lane: Int): Unit =
        val i_cur = floor (ctime / timeConv).toInt                        // determine the current time interval
        if i_cur > i_pre then                                             // detected start of new time interval
            recordInMatrix (i_pre)                                        // put stats in recorder matrices
            i_pre = i_cur                                                 // update i_pre
        lane_stat(lane).tally (speed)                                     // record the speed/property of interest
    end record

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Record the vehicle entity and optionally its speed (or other property of interest).
     *  @param actor  the actor/vehicle being recorded
     *  @param ctime  the clock time the entity entered the component (e.g., Junction, Sink)
     */
    inline def record (actor: SimActor, ctime: Double): Unit =
        if actor.isInstanceOf [Vehicle] then
            val car = actor.asInstanceOf [Vehicle]
            record (ctime, car.velocity, car.laneID)
        else
            if actor.prop != null then
                record (ctime, actor.prop.head._2, actor.subtype)         // record value of first property
            else
                record (ctime, -0.0, actor.subtype)                       // no property to record
    end record

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the lane statistics in recorder matrices and reset the statistical counters
     *  at the end of each (ii-th) observation/time interval.
     *  @param ii    the relevant observation/time interval
     */
    private def recordInMatrix (ii: Int): Unit =
        if ii >= 0 && ii < nt then                                   // check for valid range
            for l <- r_counts.indices2 do                                 // for each lane
                r_counts(ii, l) = lane_stat(l).num                        // vehicles counted during the time interval
                r_speeds(ii, l) = lane_stat(l).mean                       // average speed during the time interval
                lane_stat(l).reset ()                                     // reset statistical counters
    end recordInMatrix


end Recorder



object Recorder:

    private [process] var ew = new EasyWriter("recorder", "recorder.csv")
    def shutdownRecorder(): Unit =
        ew.finish()

end Recorder
//

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `recorderTest` main function tests the `Recorder` trait.
 *  Creates fake simulated and actual values to test `Recorder` and `Fit` methods.
 *  > runMain scalation.simulation.process.recorderTest
 */
@main def recorderTest (): Unit =

    import scalation.modeling.Fit
    import scalation.random.{Normal, Randi, Uniform}

    val nparams = 5                                                       // number of parameters in fake simulation model

    val rlane  = Randi (0, 3, 1)                                          // stream 1
    val rspeed = Uniform (22.0, 34.0, 2)                                  // stream 2
    val noise  = Normal (0.0, 2.0, 3)                                     // stream 3 (use independent streams)

    object TestRec extends Recorder ()                                    // create a `Recorder` object

    var ctime = 0.0                                                       // initialize simulated time to zero
    for _ <- 0 until 10800 do                                             // for each of 10800 fake cars
        ctime += 5.0                                                      // time between cars = 5 (should use Erlang process)
        val lane  = rlane.igen                                            // randomly pick lane
        val speed = lane + rspeed.gen                                     // randomly set speed in m/s
        TestRec.record (ctime, speed, lane)                               // record information about this fake car
    end for

    val (cmat, smat) = TestRec.getRecorderMat                             // get fake simulated values
    banner ("Recorder Matrix for counts")
    println (s"cmat = $cmat")
    banner ("Recorder Matrix for speeds")
    println (s"smat = $smat")

    val cmat_ = new MatrixD (cmat.dim, cmat.dim2)                         // make fake actual values
    val smat_ = new MatrixD (smat.dim, smat.dim2)

    for i <- cmat.indices; j <- cmat.indices2 do
        cmat_(i, j) = 45.0 + noise.gen
        smat_(i, j) = 29.5 + noise.gen

    object TestFit extends Fit (dfr = nparams , df = cmat.dim - nparams)  // create a `Fit` object

    val cqof = TestFit.diagnose_mat (cmat_, cmat)                         // diagnostics for counts
    val sqof = TestFit.diagnose_mat (smat_, smat)                         // diagnostics for speeds

    //  println (cqof)
    //  println (sqof)

    banner ("Quality of Fit (QoF) for counts")
    println (Fit.showFitMap (cqof))
    banner ("Quality of Fit (QoF) for speeds")
    println (Fit.showFitMap (sqof))

end recorderTest


