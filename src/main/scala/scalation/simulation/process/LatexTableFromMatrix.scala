package scalation
package simulation
package process

import scalation.mathstat.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LatexTableFromMatrix` object generates publication-ready LaTeX tables
 *  for traffic simulation validation results.
 */
object LatexTableFromMatrix:

    /** Generate a basic LaTeX table from data matrix.
     *  @param data    matrix of values (rows x cols)
     *  @param headers column header names
     *  @param caption table caption
     *  @param label   table label for referencing
     */
    def makeLatexTable(data: MatrixD, headers: Seq[String], caption: String, label: String): String =
        val sb = new StringBuilder
        val colfmt = "l" + "r" * (headers.length - 1)

        sb ++= "\\begin{table}[h!]\n"
        sb ++= "  \\centering\n"
        sb ++= s"  \\caption{$caption}\n"
        sb ++= s"  \\label{$label}\n"
        sb ++= s"  \\begin{tabular}{$colfmt}\n"
        sb ++= "    \\hline\n"
        sb ++= "    " + headers.mkString(" & ") + " \\\\\n"
        sb ++= "    \\hline\n"

        for i <- data.indices do
            val row = (for j <- data.indices2 yield f"${data(i, j)}%.4f").mkString(" & ")
            sb ++= s"    $row \\\\\n"

        sb ++= "    \\hline\n"
        sb ++= "  \\end{tabular}\n"
        sb ++= "\\end{table}\n"
        sb.toString
    end makeLatexTable

    /** Generate Macro-Level Validation Table (Sensor Aggregates).
     *  Columns: Sensor | Flow R² | Flow SMAPE | Flow RMSE | Speed R² | Speed SMAPE | Speed RMSE
     *  @param flowR2     array of flow R² values per sensor
     *  @param flowSmape  array of flow SMAPE values per sensor
     *  @param flowRmse   array of flow RMSE values per sensor
     *  @param speedR2    array of speed R² values per sensor
     *  @param speedSmape array of speed SMAPE values per sensor
     *  @param speedRmse  array of speed RMSE values per sensor
     */
    def macroTable(flowR2: Array[Double], flowSmape: Array[Double], flowRmse: Array[Double],
                   speedR2: Array[Double], speedSmape: Array[Double], speedRmse: Array[Double]): String =
        val sb = new StringBuilder
        val n = flowR2.length

        sb ++= "\\begin{table}[h!]\n"
        sb ++= "  \\centering\n"
        sb ++= "  \\caption{Macro-Level Validation: Sensor Aggregates}\n"
        sb ++= "  \\label{tab:macro_validation}\n"
        sb ++= "  \\begin{tabular}{lrrrrrr}\n"
        sb ++= "    \\hline\n"
        sb ++= "    Sensor & Flow $R^2$ & Flow SMAPE (\\%) & Flow RMSE & Speed $R^2$ & Speed SMAPE (\\%) & Speed RMSE \\\\\n"
        sb ++= "    \\hline\n"

        for i <- 0 until n do
            sb ++= f"    ${i + 1} & ${flowR2(i)}%.4f & ${flowSmape(i)}%.2f & ${flowRmse(i)}%.2f & ${speedR2(i)}%.4f & ${speedSmape(i)}%.2f & ${speedRmse(i)}%.2f \\\\\n"

        sb ++= "    \\hline\n"
        sb ++= "  \\end{tabular}\n"
        sb ++= "\\end{table}\n"
        sb.toString
    end macroTable

    /** Generate Micro-Level Validation Table (Lane Detail).
     *  Columns: Sensor | Lane | Flow R² | Flow SMAPE | Flow RMSE | Speed R² | Speed SMAPE | Speed RMSE
     *  @param flowMetrics  array of (R2, SMAPE, RMSE) tuples per sensor per lane
     *  @param speedMetrics array of (R2, SMAPE, RMSE) tuples per sensor per lane
     */
    def microTable(flowMetrics: Array[Vector[(Double, Double, Double)]],
                   speedMetrics: Array[Vector[(Double, Double, Double)]]): String =
        val sb = new StringBuilder
        val nSensors = flowMetrics.length
        val nLanes = if nSensors > 0 then flowMetrics(0).length else 0

        sb ++= "\\begin{table}[h!]\n"
        sb ++= "  \\centering\n"
        sb ++= "  \\caption{Micro-Level Validation: Lane Detail}\n"
        sb ++= "  \\label{tab:micro_validation}\n"
        sb ++= "  \\begin{tabular}{llrrrrrr}\n"
        sb ++= "    \\hline\n"
        sb ++= "    Sensor & Lane & Flow $R^2$ & Flow SMAPE (\\%) & Flow RMSE & Speed $R^2$ & Speed SMAPE (\\%) & Speed RMSE \\\\\n"
        sb ++= "    \\hline\n"

        for s <- 0 until nSensors do
            for l <- 0 until nLanes do
                val (fR2, fSmape, fRmse) = flowMetrics(s)(l)
                val (sR2, sSmape, sRmse) = speedMetrics(s)(l)
                val sensorLabel = if l == 0 then s"${s + 1}" else ""
                sb ++= f"    $sensorLabel & ${l + 1} & $fR2%.4f & $fSmape%.2f & $fRmse%.2f & $sR2%.4f & $sSmape%.2f & $sRmse%.2f \\\\\n"
            if s < nSensors - 1 then sb ++= "    \\hline\n"

        sb ++= "    \\hline\n"
        sb ++= "  \\end{tabular}\n"
        sb ++= "\\end{table}\n"
        sb.toString
    end microTable

end LatexTableFromMatrix


/** Generate consolidated validation matrix - "one table tells all"
 *  Rows: Lanes 1-4 + Macro
 *  Columns: Flow (R², SMAPE, RMSE) for S1-S5, Speed (R², SMAPE, RMSE) for S1-S5
 */
object ConsolidatedTable:

    /** Generate a consolidated matrix table for publication.
     *  @param flowMicro   Array of lane metrics per sensor: Array[Vector[(R2, SMAPE, RMSE)]]
     *  @param speedMicro  Array of lane metrics per sensor
     *  @param flowMacro   (R2, SMAPE, RMSE) per sensor
     *  @param speedMacro  (R2, SMAPE, RMSE) per sensor
     */
    def generate(
        flowMicro: Array[Vector[(Double, Double, Double)]],
        speedMicro: Array[Vector[(Double, Double, Double)]],
        flowMacro: Array[(Double, Double, Double)],
        speedMacro: Array[(Double, Double, Double)]
    ): String =
        val sb = new StringBuilder
        val nSensors = flowMicro.length
        val nLanes = if nSensors > 0 then flowMicro(0).length else 0

        // ═══════════════════════════════════════════════════════════════════
        // Table 1: R² Matrix (Flow + Speed side-by-side)
        // ═══════════════════════════════════════════════════════════════════
        sb ++= "% ═══ CONSOLIDATED R² MATRIX ═══\n"
        sb ++= "\\begin{table}[h!]\n"
        sb ++= "  \\centering\n"
        sb ++= "  \\caption{Consolidated $R^2$ Validation Matrix}\n"
        sb ++= "  \\label{tab:consolidated_r2}\n"
        sb ++= "  \\begin{tabular}{l|ccccc|ccccc}\n"
        sb ++= "    \\hline\n"
        sb ++= "    & \\multicolumn{5}{c|}{Flow $R^2$} & \\multicolumn{5}{c}{Speed $R^2$} \\\\\n"
        sb ++= "    & S1 & S2 & S3 & S4 & S5 & S1 & S2 & S3 & S4 & S5 \\\\\n"
        sb ++= "    \\hline\n"

        // Lane rows
        for l <- 0 until nLanes do
            val flowVals = (0 until nSensors).map(s => f"${flowMicro(s)(l)._1}%.2f").mkString(" & ")
            val speedVals = (0 until nSensors).map(s => f"${speedMicro(s)(l)._1}%.2f").mkString(" & ")
            sb ++= f"    Lane ${l + 1} & $flowVals & $speedVals \\\\\n"

        sb ++= "    \\hline\n"

        // Macro row
        val flowMacroR2 = (0 until nSensors).map(s => f"${flowMacro(s)._1}%.2f").mkString(" & ")
        val speedMacroR2 = (0 until nSensors).map(s => f"${speedMacro(s)._1}%.2f").mkString(" & ")
        sb ++= f"    \\textbf{Macro} & $flowMacroR2 & $speedMacroR2 \\\\\n"

        sb ++= "    \\hline\n"
        sb ++= "  \\end{tabular}\n"
        sb ++= "\\end{table}\n\n"

        // ═══════════════════════════════════════════════════════════════════
        // Table 2: SMAPE Matrix (Flow + Speed side-by-side)
        // ═══════════════════════════════════════════════════════════════════
        sb ++= "% ═══ CONSOLIDATED SMAPE MATRIX ═══\n"
        sb ++= "\\begin{table}[h!]\n"
        sb ++= "  \\centering\n"
        sb ++= "  \\caption{Consolidated SMAPE (\\%) Validation Matrix}\n"
        sb ++= "  \\label{tab:consolidated_smape}\n"
        sb ++= "  \\begin{tabular}{l|ccccc|ccccc}\n"
        sb ++= "    \\hline\n"
        sb ++= "    & \\multicolumn{5}{c|}{Flow SMAPE (\\%)} & \\multicolumn{5}{c}{Speed SMAPE (\\%)} \\\\\n"
        sb ++= "    & S1 & S2 & S3 & S4 & S5 & S1 & S2 & S3 & S4 & S5 \\\\\n"
        sb ++= "    \\hline\n"

        for l <- 0 until nLanes do
            val flowVals = (0 until nSensors).map(s => f"${flowMicro(s)(l)._2}%.1f").mkString(" & ")
            val speedVals = (0 until nSensors).map(s => f"${speedMicro(s)(l)._2}%.1f").mkString(" & ")
            sb ++= f"    Lane ${l + 1} & $flowVals & $speedVals \\\\\n"

        sb ++= "    \\hline\n"
        val flowMacroSmape = (0 until nSensors).map(s => f"${flowMacro(s)._2}%.1f").mkString(" & ")
        val speedMacroSmape = (0 until nSensors).map(s => f"${speedMacro(s)._2}%.1f").mkString(" & ")
        sb ++= f"    \\textbf{Macro} & $flowMacroSmape & $speedMacroSmape \\\\\n"

        sb ++= "    \\hline\n"
        sb ++= "  \\end{tabular}\n"
        sb ++= "\\end{table}\n\n"

        // ═══════════════════════════════════════════════════════════════════
        // Table 3: RMSE Matrix (Flow + Speed side-by-side)
        // ═══════════════════════════════════════════════════════════════════
        sb ++= "% ═══ CONSOLIDATED RMSE MATRIX ═══\n"
        sb ++= "\\begin{table}[h!]\n"
        sb ++= "  \\centering\n"
        sb ++= "  \\caption{Consolidated RMSE Validation Matrix}\n"
        sb ++= "  \\label{tab:consolidated_rmse}\n"
        sb ++= "  \\begin{tabular}{l|ccccc|ccccc}\n"
        sb ++= "    \\hline\n"
        sb ++= "    & \\multicolumn{5}{c|}{Flow RMSE} & \\multicolumn{5}{c}{Speed RMSE} \\\\\n"
        sb ++= "    & S1 & S2 & S3 & S4 & S5 & S1 & S2 & S3 & S4 & S5 \\\\\n"
        sb ++= "    \\hline\n"

        for l <- 0 until nLanes do
            val flowVals = (0 until nSensors).map(s => f"${flowMicro(s)(l)._3}%.1f").mkString(" & ")
            val speedVals = (0 until nSensors).map(s => f"${speedMicro(s)(l)._3}%.1f").mkString(" & ")
            sb ++= f"    Lane ${l + 1} & $flowVals & $speedVals \\\\\n"

        sb ++= "    \\hline\n"
        val flowMacroRmse = (0 until nSensors).map(s => f"${flowMacro(s)._3}%.1f").mkString(" & ")
        val speedMacroRmse = (0 until nSensors).map(s => f"${speedMacro(s)._3}%.1f").mkString(" & ")
        sb ++= f"    \\textbf{Macro} & $flowMacroRmse & $speedMacroRmse \\\\\n"

        sb ++= "    \\hline\n"
        sb ++= "  \\end{tabular}\n"
        sb ++= "\\end{table}\n"

        sb.toString
    end generate

end ConsolidatedTable


