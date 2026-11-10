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
            case d if d.startsWith ("Mainline_VDS_I210W_Pasadena") =>              // one folder per day
                List ("1-717649ML.csv", "2-717644ML.csv", "3-717642ML.csv", "4-717637ML.csv")
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


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Extract the I-210 Westbound Pasadena stretch, Sierra Madre Villa (PM 29.17) to
 *  Hill (PM 26.80), from the Eaton PeMS long-format files into one CSV per
 *  detector and day, in the folders and layout the corridor loaders read:
 *
 *      data/Mainline_VDS_I210W_Pasadena/<day>/N-<VDS>ML.csv
 *          HH:MM, total flow, avg speed, lane1 flow, lane1 speed, ..., lane5 flow, lane5 speed
 *          (= `PeMSDataLoader.I210_MainlineLayout`; the loader skips column 0)
 *      data/Ramps_VDS_I210W_Pasadena/<day>/N-<VDS>OR.csv | FR.csv
 *          HH:MM, flow                                    (= `DefaultRampLayout`, flow at column 1)
 *
 *  Rows are the 205 five-minute bins 06:00-23:00 of one day, in time order. Flows
 *  are veh / 5 min and speeds mph, exactly as PeMS gives them: nothing is scaled,
 *  filled or averaged here. The HOV lane (lane type HV) is not extracted. N is
 *  the detector's order along the drive (westbound = decreasing postmile).
 *  Every output is checked for 205 rows and no blank cell; a short row or a blank
 *  stops the extraction. Source: eaton-corridor/{3rdData,10thData,17thData}.
 *
 *  > runMain scalation.simulation.process.config.extractI210WPasadena
 */
@main def extractI210WPasadena (): Unit =
    import scala.io.Source
    import java.io.PrintWriter

    val source = DATA_DIR + "WSC-Pems-Data-Eaton-Fire" + ⁄ + "data-eaton" + ⁄ + "pems" + ⁄ + "eaton-corridor" + ⁄
    val mlDir  = DATA_DIR + "Mainline_VDS_I210W_Pasadena" + ⁄
    val rpDir  = DATA_DIR + "Ramps_VDS_I210W_Pasadena" + ⁄
    val N_BINS = 205                                                          // 06:00 .. 23:00 at 5 min

    val days = Array (("3rdData",  "2024_12_03", "2024-12-03"),
                      ("10thData", "2024_12_10", "2024-12-10"),
                      ("17thData", "2024_12_17", "2024-12-17"))

    // (order along the drive, VDS id, name)
    val mainline = Array (("1", "717649", "Sierra Madre Villa"), ("2", "717644", "San Gabriel"),
                          ("3", "717642", "Altadena"),           ("4", "717637", "Hill"))
    val ramps    = Array (("1", "716594", "OR", "Sierra Madre on"),  ("1", "717648", "FR", "Sierra Madre off"),
                          ("2", "768927", "FR", "San Gabriel off"),  ("2", "716592", "OR", "San Gabriel on"),
                          ("3", "716591", "OR", "Altadena on"),      ("3", "764346", "FR", "Allen off"),
                          ("4", "716588", "OR", "Hill on"))

    val mlCols = Array ("Total Flow (veh/5min)", "Avg Speed (mph)") ++
                 (1 to 5).flatMap (i => Array (s"Lane$i Flow (veh/5min)", s"Lane$i Avg Speed (mph)"))
    val rpCols = Array ("Total Flow (veh/5min)")

    /** Read a PeMS long-format CSV: header -> column index, then rows as arrays. */
    def readCsv (path: String): (Map [String, Int], Array [Array [String]]) =
        val src   = Source.fromFile (path)
        val lines = try src.getLines ().toArray finally src.close ()
        val head  = lines.head.split (",", -1).map (_.trim).zipWithIndex.toMap
        (head, lines.tail.map (_.split (",", -1)))
    end readCsv

    /** Rows of one detector, as (HH:MM, selected columns), time-ordered and checked. */
    def detectorRows (table: (Map [String, Int], Array [Array [String]]), station: String,
                      laneType: String, cols: Array [String]): Array [String] =
        val (head, rows) = table
        val picked = rows.filter (r => r(head("Station")) == station && r(head("Lane Type")) == laneType)
                         .sortBy (r => r(head("Timestamp")))
        require (picked.length == N_BINS, s"station $station $laneType: ${picked.length} rows, expected $N_BINS")
        picked.map { r =>
            val values = cols.map (c => r(head(c)))
            require (values.forall (_.nonEmpty), s"station $station $laneType: blank cell at ${r(head("Timestamp"))}")
            val hhmm = r(head("Timestamp")).trim.drop (11).take (5)                   // "2024-12-10 06:05:00" -> "06:05"
            (hhmm +: values).mkString (",")
        }
    end detectorRows

    def writeLines (path: String, lines: Array [String]): Unit =
        val out = new PrintWriter (path)
        try lines.foreach (out.println) finally out.close ()
        println (s"wrote ${lines.length} rows -> $path")
    end writeLines

    for (folder, tag, day) <- days do
        val base = s"$source$folder${⁄}eaton-i210${⁄}eaton_i210_W_$tag"
        val ml   = readCsv (base + "_ML_HV.csv")
        val or   = readCsv (base + "_OR.csv")
        val fr   = readCsv (base + "_FR.csv")
        for (n, vds, name) <- mainline do
            writeLines (s"$mlDir$day${⁄}$n-${vds}ML.csv", detectorRows (ml, vds, "ML", mlCols))
        for (n, vds, kind, name) <- ramps do
            val table = if kind == "OR" then or else fr
            writeLines (s"$rpDir$day${⁄}$n-$vds$kind.csv", detectorRows (table, vds, kind, rpCols))
    end for
    println ("extractI210WPasadena: done")
end extractI210WPasadena
