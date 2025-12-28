package scalation
package simulation
package process

trait RowTimeLoader:

    var curRow = 0
    var rowTime = 15 * MINUTE // row time window

    private[process] val ew = new EasyWriter("recorder", "rowTimeRecord.txt")
    
    // NEW: Model-specific time slice duration (override in implementing models)
    // Purpose: Allow different simulations to use different observation windows
    // Example: Traffic model = 15min, Manufacturing = 1hr, Network = 5sec
    // Default: 15 minutes for backward compatibility with existing traffic models
    val rowTimeSlice: Double = 15 * MINUTE
    
    // NEW: Calculate current row index from simulation clock
    // Purpose: Convert clock time to data row index using model's time granularity
    // Formula: currentRow = floor(clock / rowTimeSlice)
    // Example: clock=1350s, rowTimeSlice=900s → row = floor(1350/900) = 1
    // Use: VSource calls this to determine which row's mu to fetch
    def getCurrentRow(clock: Double): Int = 
        (clock / rowTimeSlice).toInt
    
    def nextRow(clock: Double): Unit =
        //println(s"I was called by @@@@@@ director clock: $clock and rowTime: $rowTime")
        //ew.write(s"\n I was called by this director clock: $clock and rowtime $rowTime \n ")
        //900sec;
        if clock >= rowTime then
            curRow += 1
            rowTime += 15 * MINUTE
        end if
    end nextRow
    

end RowTimeLoader
