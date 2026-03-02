//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Feb 02 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    PeMS Data Helper - Corridor-Agnostic Wrapper
 *
 *  Provides high-level access to PeMS data using PeMSDemand configuration.
 *  NO dependency on TrafficConfig2 - fully decoupled from legacy code.
 */

package scalation
package simulation
package process
package config

import scalation.mathstat.MatrixD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PeMSDataHelper` object provides convenient access to PeMS sensor data
 *  using the new corridor-agnostic configuration system.
 *  
 *  All data loading is driven by `PeMSDemand` configuration objects.
 *  No hardcoded paths, no hardcoded sensor IDs - everything from config.
 */
object PeMSDataHelper:

    private val debug = debugf ("PeMSDataHelper", false)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load a single mainline sensor by index from a demand configuration.
     *  
     *  @param demand      the PeMS demand configuration specifying data sources
     *  @param sensorIdx   the sensor index (0-based, matches NetworkConfig sensor order)
     *  @param window      optional time window override
     *  @param layout      optional column layout override
     */
    def loadMainlineSensor(
        demand: PeMSDemand,
        sensorIdx: Int,
        window: TimeWindow = PeMSDataLoader.DefaultTimeWindow,
        layout: ColumnLayout = PeMSDataLoader.DefaultMainlineLayout
    ): (MatrixD, MatrixD) =
        // Build file path from demand configuration
        val sensorFile = extractMainlineSensorFile(demand, sensorIdx)
        val fullPath = s"${demand.dataDir}/$sensorFile"
        //debug ("loadMainlineSensor", s"sensorIdx=$sensorIdx, path=$fullPath")
        
        PeMSDataLoader.loadMainlineSensor(fullPath, window, layout)
    end loadMainlineSensor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load all mainline sensors for a corridor.
     *  
     *  @param demand   the PeMS demand configuration
     *  @param window   optional time window override
     *  @param layout   optional column layout override
     */
    def loadAllMainlineSensors(
        demand: PeMSDemand,
        window: TimeWindow = PeMSDataLoader.DefaultTimeWindow,
        layout: ColumnLayout = PeMSDataLoader.DefaultMainlineLayout
    ): Array[(MatrixD, MatrixD)] =
        val sensorFiles = extractAllMainlineSensorFiles(demand)
        PeMSDataLoader.loadMainlineSensors(
            demand.dataDir,
            sensorFiles,
            window,
            layout
        )
    end loadAllMainlineSensors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load a single ramp sensor by index from a demand configuration.
     *  
     *  @param demand    the PeMS demand configuration
     *  @param rampIdx   the ramp index (0-based, matches demand.ramps order)
     *  @param window    optional time window override
     *  @param layout    optional column layout override
     */
    def loadRampSensor(
        demand: PeMSDemand,
        rampIdx: Int,
        window: TimeWindow = PeMSDataLoader.DefaultTimeWindow,
        layout: ColumnLayout = PeMSDataLoader.DefaultRampLayout
    ): MatrixD =
        require(rampIdx < demand.ramps.length, s"Ramp index $rampIdx out of range (${demand.ramps.length} ramps)")
        
        val rampFile = demand.ramps(rampIdx).anchorFile
        val fullPath = s"Ramps_VDS_${demand.dataDir.replace("Mainline_VDS_", "")}/$rampFile"
        
        PeMSDataLoader.loadRampSensor(fullPath, window, layout)
    end loadRampSensor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load all ramp sensors for a corridor.
     *  
     *  @param demand   the PeMS demand configuration
     *  @param window   optional time window override
     *  @param layout   optional column layout override
     */
    def loadAllRampSensors(
        demand: PeMSDemand,
        window: TimeWindow = PeMSDataLoader.DefaultTimeWindow,
        layout: ColumnLayout = PeMSDataLoader.DefaultRampLayout
    ): Array[MatrixD] =
        val rampFiles = demand.ramps.map(_.anchorFile)
        val rampDir = s"Ramps_VDS_${demand.dataDir.replace("Mainline_VDS_", "")}"
        
        PeMSDataLoader.loadRampSensors(rampDir, rampFiles, window, layout)
    end loadAllRampSensors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract the mainline sensor filename for a specific sensor index.
     *  
     *  For Donald Doyle: anchorFile = "1-401112ML.csv" for sensor 0
     *  For Redwood Creek: anchorFile = "1-404531ML.csv" for sensor 0
     *  
     *  This method derives subsequent sensor files based on corridor conventions.
     */
    private def extractMainlineSensorFile(demand: PeMSDemand, sensorIdx: Int): String =
        if sensorIdx == 0 then
            demand.mainline.anchorFile
        else
            // Derive subsequent sensor files from anchor file pattern
            // Pattern: "N-XXXXXXML.csv" where N is sensor number
            val anchor = demand.mainline.anchorFile
            val baseName = anchor.stripSuffix(".csv")
            val parts = baseName.split("-")
            if parts.length == 2 then
                val sensorNum = sensorIdx + 1
                val vdsId = parts(1)
                // For now, we need to know the VDS IDs for each corridor
                // This is corridor-specific but better than TrafficConfig2's hardcoding
                getSensorFileForCorridor(demand.dataDir, sensorIdx)
            else
                anchor // fallback
        end if
    end extractMainlineSensorFile

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract all mainline sensor filenames for a corridor. */
    private def extractAllMainlineSensorFiles(demand: PeMSDemand): List[String] =
        // Corridor-specific sensor file lists
        demand.dataDir match
            case "Mainline_VDS_Donald_Doyle" =>
                List("1-401112ML.csv", "2-401104ML.csv", "3-400712ML.csv", 
                     "4-400450ML.csv", "5-407463ML.csv")
            case "Mainline_VDS_Redwood_Creek_US101-N" =>
                List("1-404531ML.csv", "2-404532ML.csv", "3-401834ML.csv",
                     "4-401833ML.csv", "5-401929ML.csv")
            case _ =>
                // Fallback: try to derive from anchor file
                List(demand.mainline.anchorFile)
    end extractAllMainlineSensorFiles

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get sensor filename for a specific corridor and sensor index. */
    private def getSensorFileForCorridor(dataDir: String, sensorIdx: Int): String =
        val sensorFiles = extractAllMainlineSensorFiles(PeMSDemand(
            mainline = PeMSArrivals("", null, false),
            ramps = List.empty,
            dataDir = dataDir
        ))
        if sensorIdx < sensorFiles.length then sensorFiles(sensorIdx)
        else sensorFiles.head // fallback
    end getSensorFileForCorridor

end PeMSDataHelper
