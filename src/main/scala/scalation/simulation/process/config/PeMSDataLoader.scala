//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Feb 02 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Generic PeMS Data Loader (Corridor-Agnostic)
 *
 *  Extracted from TrafficConfig2 to eliminate hardcoded paths and sensor IDs.
 *  All corridor-specific information comes from parameters, not hardcoded values.
 */

package scalation
package simulation
package process
package config

import scalation.mathstat.{MatrixD, VectorI}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Time window specification for PeMS data loading.
 *  @param startRow    first row to load (0-based, e.g., 0 for 6am)
 *  @param endRow      last row to load (exclusive, e.g., 48 for 6pm = 12 hours)
 *  @param binSeconds  seconds per time bin (e.g., 900.0 for 15-minute intervals)
 */
case class TimeWindow(startRow: Int, endRow: Int, binSeconds: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Column layout specification for PeMS CSV files.
 *  @param flowCols     column indices for lane flow data (e.g., VectorI(1,3,5,7) for 4 lanes)
 *  @param speedCols    column indices for lane speed data (e.g., VectorI(2,4,6,8) for 4 lanes)
 *  @param speedFactor  conversion factor for speed (e.g., 0.44704 to convert mph → m/s)
 */
case class ColumnLayout(flowCols: VectorI, speedCols: VectorI, speedFactor: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PeMSDataLoader` object provides corridor-agnostic CSV loading for PeMS data.
 *
 *  Design principles:
 *  - No hardcoded file paths (caller provides full path)
 *  - No hardcoded sensor IDs (caller identifies sensors)
 *  - No hardcoded column indices (caller provides layout)
 *  - Works for any corridor by parameterization
 */
object PeMSDataLoader:

    // Default configurations (can be overridden by caller)
    val DefaultTimeWindow = TimeWindow(
        startRow = 0,      // 6am (first row)
        endRow = 48,       // 6pm (48 rows = 12 hours @ 15-min intervals)
        binSeconds = 900.0 // 15 minutes
    )

    val DefaultMainlineLayout = ColumnLayout(
        flowCols = VectorI(1, 3, 5, 7),   // Flow columns for 4-lane mainline
        speedCols = VectorI(2, 4, 6, 8),  // Speed columns for 4-lane mainline
        speedFactor = 0.44704             // mph → m/s conversion
    )

    val DefaultRampLayout = ColumnLayout(
        flowCols = VectorI(1),            // Single flow column for ramps
        speedCols = VectorI.nullv,        // Ramps typically don't have speed data
        speedFactor = 0.44704
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load a mainline sensor from PeMS CSV file.
     *  Returns (flow matrix, speed matrix) where rows = time bins, cols = lanes.
     *
     *  @param filePath  full path to CSV file (e.g., "data/Mainline_VDS_Donald_Doyle/1-401112ML.csv")
     *  @param window    time window specification (default: 6am-6pm)
     *  @param layout    column layout specification (default: 4-lane mainline)
     */
    def loadMainlineSensor(
        filePath: String,
        window: TimeWindow = DefaultTimeWindow,
        layout: ColumnLayout = DefaultMainlineLayout
    ): (MatrixD, MatrixD) =
        // Load raw data from CSV
        val rawData = MatrixD.load(
            filePath,
            skip = window.startRow,
            skipCol = 0,
            stop = window.endRow
        )

        // Extract flow columns (rows x lanes)
        val flowMatrix = rawData(?, layout.flowCols)

        // Extract speed columns and apply conversion factor (rows x lanes)
        val speedMatrix = rawData(?, layout.speedCols) * layout.speedFactor

        (flowMatrix, speedMatrix)
    end loadMainlineSensor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load a ramp sensor from PeMS CSV file.
     *  Returns flow matrix where rows = time bins, cols = lanes (typically 1).
     *
     *  @param filePath  full path to CSV file (e.g., "data/Ramps_VDS_Donald_Doyle/1-403157OR.csv")
     *  @param window    time window specification (default: 6am-6pm)
     *  @param layout    column layout specification (default: single-lane ramp)
     */
    def loadRampSensor(
        filePath: String,
        window: TimeWindow = DefaultTimeWindow,
        layout: ColumnLayout = DefaultRampLayout
    ): MatrixD =
        // Load raw data from CSV
        val rawData = MatrixD.load(
            filePath,
            skip = window.startRow,
            skipCol = 0,
            stop = window.endRow
        )

        // Extract flow columns (rows x 1 for typical ramps)
        val flowMatrix = rawData(?, layout.flowCols)

        flowMatrix
    end loadRampSensor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load all mainline sensors for a corridor.
     *
     *  @param dataDir       base directory for data files (e.g., "data/Mainline_VDS_Donald_Doyle")
     *  @param sensorFiles   ordered list of sensor filenames (e.g., List("1-401112ML.csv", ...))
     *  @param window        time window specification
     *  @param layout        column layout specification
     */
    def loadMainlineSensors(dataDir: String, sensorFiles: List[String], window: TimeWindow = DefaultTimeWindow, layout: ColumnLayout = DefaultMainlineLayout): Array[(MatrixD, MatrixD)] =
        sensorFiles.map { filename =>
            val fullPath = s"$dataDir/$filename"
            loadMainlineSensor(fullPath, window, layout)
        }.toArray
    end loadMainlineSensors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load all ramp sensors for a corridor.
     *
     *  @param dataDir       base directory for data files (e.g., "data/Ramps_VDS_Donald_Doyle")
     *  @param rampFiles     ordered list of ramp filenames (e.g., List("1-403157OR.csv", ...))
     *  @param window        time window specification
     *  @param layout        column layout specification
     */
    def loadRampSensors(dataDir: String, rampFiles: List[String], window: TimeWindow = DefaultTimeWindow, layout: ColumnLayout = DefaultRampLayout): Array[MatrixD] =
        rampFiles.map { filename =>
            val fullPath = s"$dataDir/$filename"
            loadRampSensor(fullPath, window, layout)
        }.toArray
    end loadRampSensors

end PeMSDataLoader
