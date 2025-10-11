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
    val nl = 5                           // Number of lanes for the model. May need to be dynamic

    protected val r_counts = new MatrixD(nt, nl)   // Count matrix [time_intervals × lanes] [nt, number of lanes]  
    protected val r_speeds = new MatrixD(nt, nl)   // Speed matrix [time_intervals × lanes]

    private val recordedVehicles = new ArrayBuffer[String]()
    
    // Track the last row index to detect transitions
    private var lastRecordedRow: Int = -1



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Record the entity and optionally its speed.
     *  @param ctime  the clock time the entity entered the component (e.g., Sink)
     *  @param speed  the speed at which entity entered the component (e.g., Sink)
     *  @param actor  the actor to be recorded
     */
    def record(actor: SimActor, ctime: Double): Unit =

        val i = floor(ctime / timeConv).toInt // Time bucket index
        val j = if i >= nt then nt - 1 else i // cap the last time bucket for overflow
        
        // ===== DETECT ROW TRANSITION =====
        if j != lastRecordedRow then
            val rowStartTime = j * timeConv
            val rowEndTime = (j + 1) * timeConv
            Recorder.ew.write(f"\n╔═══════════════════════════════════════════════════════════════════════════════╗\n")
            Recorder.ew.write(f"║ ROW TRANSITION DETECTED: Row $lastRecordedRow%2d → Row $j%2d at clock=$ctime%8.2f sec\n")
            Recorder.ew.write(f"║ Row $j%2d time window: [$rowStartTime%8.2f - $rowEndTime%8.2f) seconds\n")
            Recorder.ew.write(f"║ Sensor: $this\n")
            Recorder.ew.write(f"╚═══════════════════════════════════════════════════════════════════════════════╝\n")
            lastRecordedRow = j
        end if

        // ===== LOG EVERY RECORDING EVENT =====
        Recorder.ew.write(f"\n[RECORD] Sensor=$this | Clock=$ctime%8.2f | Row=$j%2d | timeConv=$timeConv%6.1f | CalcRow_i=$i%2d")

        if actor.isInstanceOf[Vehicle] then

            val vehicle = actor.asInstanceOf[Vehicle]
            val laneID = vehicle.laneID
            val cnt = r_counts(j, laneID).toInt + 1
            r_counts(j, laneID) = cnt
            val speed = if vehicle.velocity.isNaN then 0.0 else vehicle.velocity
            r_speeds(j, laneID) = (r_speeds(j, laneID) * (cnt - 1) + (speed * 2.24694) ) / cnt // Compute running avg speed

            recordedVehicles += vehicle.name

            // Enhanced vehicle recording log
            Recorder.ew.write(f" | Vehicle=${vehicle.displayLabel}%-8s | Lane=$laneID%d | Speed=$speed%5.2f | Count_in_row=$cnt%3d\n")
        else
            r_counts(j, 0) += 1 //None vehicle actors records
            Recorder.ew.write(f" | Actor=${actor.name}%-8s | Type=Non-Vehicle\n")
        end if
    end record



    /** Get recorded vehicle counts per lane & time */
    def getCountMatrix: MatrixD = r_counts

    /** Get recorded speeds per lane & time */
    def getSpeedMatrix: MatrixD = r_speeds

    def writeLaneIntervalStats(): Unit =
        Recorder.ew.write(s"\n================== ROW-WISE LANE STATS FOR SENSOR $this : ${r_counts.sum} ==================\n")
        for i <- 0 until r_counts.dim do
            val counts = r_counts(i)
            val speeds = r_speeds(i)

            // Weighted average speed
            var weighted, total = 0.0
            var j = 0
            while j < counts.dim do
                val c = counts(j)
                if c > 0 then
                    weighted += speeds(j) * c
                    total += c
                end if
                j += 1
            end while
            val avgSpeed = if total > 0 then weighted / total else 0.0

            // Format
            val countsStr = counts.map(_.toInt).mkString(", ")
            val speedsStr = speeds.map(s => f"$s%2.1f").mkString(", ")

            Recorder.ew.write(
                s" lane_flow: [$countsStr] : lane_speed: [$speedsStr] : flow_total: [${total.toInt}] : ave_speed = [${f"$avgSpeed%2.1f"}]\n"
            )
        end for
        Recorder.ew.write(s"\n================== ROW-WISE LANE STATS FOR SENSOR==================\n")
        //Recorder.ew.flush()
    end writeLaneIntervalStats






end Recorder




object Recorder:


    private [process] val ew = new EasyWriter("recorder", "recorder.txt")

    def writeAllSensorStats(sensors: List[Recorder]): Unit =
        ew.write("\n================== FINAL SENSOR STATS ==================\n")
        for s <- sensors do s.writeLaneIntervalStats()
        ew.finish()                                     // finalize the writer after logging everything
    end writeAllSensorStats
    
    
    def shutdownRecorder(): Unit =
        ew.finish()

end Recorder
