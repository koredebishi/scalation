package scalation
package simulation
package process

import scalation.mathstat.{MatrixD, VectorD}

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
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Data access methods for VSource (must be implemented by traffic models)
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    /** Get number of time rows in loaded data.
     *  Must be implemented by models that use time-indexed data.
     */
    def getDataDimension: Int
    
    /** Get inter-arrival time (mu) for a specific source at each time row.
     *  @param sourceIdx  source index (0-3 for mainline lanes, 4+ for ramps)
     *  @return vector of mu values indexed by time row
     */
    def getMuForSource(sourceIdx: Int): VectorD
    
    /** Get speed matrix for the corridor (time-indexed, lane-specific).
     *  @return matrix where rows = time bins, columns = lanes
     */
    def getSpeedMatrix(): MatrixD

end RowTimeLoader


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RowTimeLoader` companion object provides default implementations
 *  for traffic models using the config architecture.
 */
object RowTimeLoader:

    import scalation.simulation.process.arrival.ArrivalSource
    import scalation.simulation.process.config.{PeMSDemand, PeMSDataHelper, PeMSDataLoader, ColumnLayout, TimeWindow}

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Default implementation: Get inter-arrival time (mu) for a source.
     *  Traffic models can delegate to this method.
     *  @param mainlineArrivalSources  mainline arrival sources array
     *  @param rampArrivalSources      ramp arrival sources array
     *  @param numLanes               number of mainline lanes
     *  @param nt                     number of time rows
     *  @param sourceIdx              source index (0 until numLanes for mainline, numLanes+ for ramps)
     */
    def getMuForSourceDefault(mainlineArrivalSources: Array[ArrivalSource],
                               rampArrivalSources: Array[ArrivalSource],
                               numLanes: Int, nt: Int, sourceIdx: Int): VectorD =
        if sourceIdx < numLanes then
            // Mainline lane
            val arrivalSource = mainlineArrivalSources(sourceIdx)
            VectorD(Array.tabulate(nt)(row => arrivalSource.getMu(sourceIdx, row)))
        else
            // Ramp
            val rampIdx = sourceIdx - numLanes
            if rampIdx < rampArrivalSources.length then
                val arrivalSource = rampArrivalSources(rampIdx)
                VectorD(Array.tabulate(nt)(row => arrivalSource.getMu(rampIdx, row)))
            else
                VectorD.fill(nt)(Double.MaxValue)  // fallback
        end if
    end getMuForSourceDefault

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Default implementation: Get speed matrix from PeMSDemand.
     *  Traffic models can delegate to this method.
     *  @param demand  PeMS demand configuration
     */
    def getSpeedMatrixDefault(demand: PeMSDemand): MatrixD =
        // Load speed data for anchor sensor (sensor 0)
        val (_, speedMatrix) = PeMSDataHelper.loadMainlineSensor(demand, 0)
        speedMatrix
    end getSpeedMatrixDefault


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load speed matrix from a single PeMS CSV file with custom layout/window.
     *  Use this when the CSV format differs from the default (e.g., I-210 Eaton
     *  5-lane, 5-min bins with TotalFlow/AvgSpeed before lane columns).
     *
     *  @param filePath  path to the CSV file (relative to project root)
     *  @param window    time window (start/end row, bin seconds)
     *  @param layout    column layout (flow/speed column indices, speed factor)
     */
    def getSpeedMatrixFromFile (filePath: String,
                                window: TimeWindow = PeMSDataLoader.DefaultTimeWindow,
                                layout: ColumnLayout = PeMSDataLoader.DefaultMainlineLayout): MatrixD =
        val (_, speedMatrix) = PeMSDataLoader.loadMainlineSensor (filePath, window, layout)
        speedMatrix
    end getSpeedMatrixFromFile

end RowTimeLoader


