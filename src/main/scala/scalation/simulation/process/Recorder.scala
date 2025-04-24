
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
import scalation.mathstat.MatrixD
import scala.collection.mutable.ArrayBuffer
//import java.io.PrintWriter

////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** The `Recorder` trait allows Nodes to easily record the flow of actors/entities
// *  (e.g., vehicles) in terms of counts and optionally average speed.
// *  @param nt  the number of time intervals
// */
//trait Recorder (nt: Int = 200):
//
//    private val timeConv = 86400.0 / nt                                 // 50 * 60 * 24 = 86400 seconds per day
//
//    protected val r_counts = Array.ofDim [Int] (nt)                     // record counts in time interval
//    protected val r_speeds = Array.ofDim [Double] (nt)                  // record average speed in time interval Fix<--turn this to matrix
//    //r_speed will be MatrixI[count_in_time_interval, lane]
//    //the counts in the first row will get incremented before the next time interval
//    //at the begining of the simulation they are all 0; then increment as time moves
//    //each sensor will need this counts;
//    //each sensor will have r_speeds of it's own
//    //passed the number of lanes
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Record the entity and optionally its speed.
//     *  @param ctime  the clock time the entity entered the component (e.g., Sink)
//     *  @param speed  the speed at which entity entered the component (e.g., Sink)
//     */
//    def record (actor: SimActor, ctime: Double): Unit =
//        val i = floor (ctime / timeConv).toInt
//        val cnt = r_counts(i) + 1
//        r_counts(i) = cnt
//        if actor.isInstanceOf [Vehicle] then
//            val speed = actor.asInstanceOf [Vehicle].velocity
//            r_speeds(i)  = (r_speeds(i) * (cnt - 1) + speed) / cnt
//    end record
//
//end Recorder
//


trait Recorder(nt: Int):

    private [process] val log       = Monitor ("recorder")        // log for model execution

    //private val timeConv = 86400.0 / nt  // Convert seconds in a day into time intervals
    val rowTime = 15.0 * MINUTE
    private val timeConv = rowTime
    val nl = 4                           // Number of lanes for the model. May need to be dynamic

    protected val r_counts = new MatrixD(nt, nl)   // Count matrix [time_intervals × lanes] [nt, number of lanes]  
    protected val r_speeds = new MatrixD(nt, nl)   // Speed matrix [time_intervals × lanes]

    private val recordedVehicles = new ArrayBuffer[String]()



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Record the entity and optionally its speed.
     *  @param ctime  the clock time the entity entered the component (e.g., Sink)
     *  @param speed  the speed at which entity entered the component (e.g., Sink)
     *  @param actor  the actor to be recorded
     */
//    def record(actor: SimActor, ctime: Double): Unit =
//
//        println(s"the clock time is $ctime and the timeConv is $timeConv")
//        val i = floor(ctime / timeConv).toInt  // Time bucket index //Fix this time conversion thing.
//
//        println(s"[Recorder] recording for: $this")
//
//        if actor.isInstanceOf[Vehicle] then
//            val vehicle = actor.asInstanceOf[Vehicle]
//            val laneID = vehicle.laneID
//            r_counts(i, laneID) += 1
//            val speed = if vehicle.velocity.isNaN then 0.0 else vehicle.velocity
//            val cnt = r_counts(i, laneID).toInt
//
//            if cnt > 0 then
//                r_speeds(i, laneID) = (r_speeds(i, laneID) * (cnt - 1) + speed) / cnt  // Compute running avg speed
//            recordedVehicles += vehicle.name
//        else
//            r_counts(i, 0) += 1           //None vehicle actors records
//    end record

    def record(actor: SimActor, ctime: Double): Unit =

        println(s"the clock time is $ctime and the timeConv is $timeConv")

        val i = floor(ctime / timeConv).toInt // Time bucket index //Fix this time conversion thing.
        val j = if i >= nt then nt - 1 else i // cap the last time bucker for overflow

        Recorder.ew.write(s"\n the clock time is $ctime and the timeConv is $timeConv i: $i \n")
        println(s"[Recorder] recording for: $this")

        if actor.isInstanceOf[Vehicle] then
            val vehicle = actor.asInstanceOf[Vehicle]
            val laneID = vehicle.laneID
            val cnt = r_counts(j, laneID).toInt + 1
            r_counts(j, laneID) = cnt
            val speed = if vehicle.velocity.isNaN then 0.0 else vehicle.velocity
            r_speeds(j, laneID) = (r_speeds(j, laneID) * (cnt - 1) + (speed * 2.24694) ) / cnt // Compute running avg speed

            recordedVehicles += vehicle.name
        else
            r_counts(j, 0) += 1 //None vehicle actors records
    end record



    /** Get recorded vehicle counts per lane & time */
    def getCountMatrix: MatrixD = r_counts

    /** Get recorded speeds per lane & time */
    def getSpeedMatrix: MatrixD = r_speeds

    def writeIntervalLaneStats(): Unit =
        Recorder.ew.write(s"\n================== ROW-WISE LANE STATS FOR SENSOR $this : ${r_counts.sum} ==================\n")
        for i <- 0 until r_counts.dim do
            Recorder.ew.write(s"MatrixD Row ${i + 1}: ${r_counts(i)}: totalcount:${r_counts(i).sum} \n")
            //Recorder.ew.write(s"MatrixD AVERAGE SPEED ${i + 1}: ${r_speeds(i)}: \n")
        end for
        Recorder.ew.write(s"\n================== ROW-WISE LANE STATS FOR SENSOR==================\n")
        //Recorder.ew.flush()
    end writeIntervalLaneStats




end Recorder




object Recorder:


    private [process] val ew = new EasyWriter("recorder", "recorder.txt")

    def writeAllSensorStats(sensors: List[Recorder]): Unit =
        ew.write("\n================== FINAL SENSOR STATS ==================\n")
        for s <- sensors do s.writeIntervalLaneStats()
        ew.finish()                                     // finalize the writer after logging everything
    end writeAllSensorStats
    
    
    def shutdownRecorder(): Unit =
        ew.finish()

end Recorder


























