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

trait Recorder(nt: Int, nl: Int):

    private [process] val log       = Monitor ("recorder")        // log for model execution

    //private val timeConv = 86400.0 / nt  // Convert seconds in a day into time intervals
    val rowTime = 15.0 * MINUTE
    private val timeConv = rowTime
    // nl is now a parameter - number of lanes (e.g., 4 for RoadCood2, 5 for original CalRoute101)

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

        if actor.isInstanceOf[Vehicle] then

            val vehicle = actor.asInstanceOf[Vehicle]

            // Use vehicle's laneID directly for recording to match PEMS lane order
            // Assumption: laneID 0 corresponds to PEMS L1 (leftmost/fast), laneID 4 to PEMS L5 (rightmost/slow)
            val lane = vehicle.laneID

            val cnt = r_counts(j, lane).toInt + 1
            r_counts(j, lane) = cnt
            val speed = if vehicle.velocity.isNaN then 0.0 else vehicle.velocity
            r_speeds(j, lane) = (r_speeds(j, lane) * (cnt - 1) + (speed * 2.24694) ) / cnt // Compute running avg speed

            recordedVehicles += vehicle.displayLabel

            // Since vehicle name/displayLabel is been recorded. I might use this to find vehicles that are not recorded at the next sensor.
            // So, at sensor 0 and sensor 1, I can compare the recordedVehicles list to see which vehicles are missing.
            //Sensor0: [lane1, lane2, lane3, lane4, lane5].row(i)
            //Sensor1: [lane1, lane2, lane3, lane4, lane5].row(i)
            // missingVehicles = recordedVehicles_sensor0.diff(recordedVehicles_sensor1)
            //Should give me the vehicles that are missing between sensor0 and sensor1 @ time interval i. How can I get the exact value/names of those vehicles?
            //Suppose this is the condense counts:
            // ssro(row_0)  386,247,314,222,136  flow_total:1305
            // ssr1 (row_0) 371,244,307,219,133  flow_total:1274
            // missingVehicles = 15,3,7,3,3  flow_total:31 where did these vehicles go? there is no offramp or onramp between these two sensors.

            //similarlt:
            //ssr1(row_1) 462,301,372,254,145  flow_total:1534
            //ssr2(row_1) 458,294,368,250,143  flow_total:1513
            // missingVehicles = 4,7,4,4,2  flow_total:21 where did these vehicles go? there is no offramp or onramp between these two sensors.
            // I can use this to estimate the number of vehicles that are missing between two sensors at

            //Recorder.ew.write(f" | Vehicle=${vehicle.displayLabel}%-8s | Lane=$laneID%d | Speed=$speed%5.2f | Count_in_row=$cnt%3d\n")
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
        Recorder.ew.write(s"\n $this : ${r_counts.sum}\n")

//        if this.toString.startsWith("ssor") then

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
            val countsStr = counts.map(_.toInt).mkString(",")
            val speedsStr = speeds.map(s => f"$s%2.1f").mkString(", ")
//
            Recorder.ew.write(
                s" lane_flow: [$countsStr] : lane_speed: [$speedsStr] : flow_total: [${total.toInt}] : ave_speed = [${f"$avgSpeed%2.1f"}]\n"
            )

//            Recorder.ew.write(
//                s"$countsStr \n"
//            )
        end for
        //Recorder.ew.write(s"\n================== ROW-WISE LANE STATS FOR SENSOR==================\n")
        //Recorder.ew.flush()
        //end  if
    end writeLaneIntervalStats


end Recorder




object Recorder:

    // Default writer for backward compatibility
    private [process] var ew = new EasyWriter("recorder", "recorder.csv")

    /** Write all sensor stats to a specific output file.
     *  @param sensors   list of Recorder instances (e.g., Junctions)
     *  @param filename  output filename (default: "recorder.csv")
     */
    def writeAllSensorStats(sensors: List[Recorder], filename: String = "recorder.csv"): Unit =
        ew = new EasyWriter("recorder", filename)  // Create new writer with specified filename
        for s <- sensors do
            s.writeLaneIntervalStats()
        ew.finish()
    end writeAllSensorStats
    
    
    def shutdownRecorder(): Unit =
        ew.finish()

end Recorder
