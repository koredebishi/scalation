//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Wed Sep 16 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Export the Scalation IDM equilibrium table for Maude consumption
 */

package scalation
package simulation
package process
package ir

import scalation.dynamics.IdmFlux

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoSpecMaude` object exports the already-built `IdmFlux` speed table.
 *  Maude imports these samples; it does not derive a second IDM table.
 */
object MesoSpecMaude:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a Maude functional module containing the source IDM table.
     *  The source MicroIR identity gives the generated module a stable name.
     *  @param spec   the accepted mesoscopic specification
     *  @param micro  the source microscopic model
     */
    def idmSpeedTableModule (spec: MesoSpec, micro: MicroIR): Option [String] =
        spec.validateAgainst (micro)                            // verify the declared source model
        MesoSpecScalation.flowDiagram (spec, micro).map { flux =>
            idmSpeedTableModule (flux, micro.sha256)
        }
    end idmSpeedTableModule

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode the existing density/speed samples as Maude equations.
     *  @param flux       the authoritative ScalaTion IDM diagram
     *  @param sourceHash the source MicroIR SHA-256 identity
     */
    private [ir] def idmSpeedTableModule (flux: IdmFlux, sourceHash: String): String =
        require (sourceHash.matches ("[0-9a-f]{64}"),
                 "source MicroIR identity must be a lowercase SHA-256 hash")

        val density = flux.rhoGrid                            // read-only copy of authoritative samples
        val speed   = flux.vGrid
        require (density.dim == speed.dim && density.dim >= 2,
                 "IdmFlux density and speed grids must have matching samples")

        val moduleName = s"IDM_SPEED_TABLE_${sourceHash.take (16).toUpperCase}"
        val out = new StringBuilder
        out.append (s"*** Imported from Scalation IdmFlux; source MicroIR SHA-256: $sourceHash\n")
        out.append ("*** Density is in vehicles/metre; speed is in metres/second.\n")
        out.append (s"fmod $moduleName is\n")
        out.append ("  protecting FLOAT .\n")
        out.append ("  protecting NAT .\n")
        out.append ("  op tableSize : -> Nat .\n")
        out.append ("  ops rhoAt speedAt : Nat -> Float .\n")
        out.append (s"  eq tableSize = ${density.dim} .\n")

        for i <- 0 until density.dim do
            out.append (s"  eq rhoAt($i) = ${maudeFloat (density(i))} .\n")
            out.append (s"  eq speedAt($i) = ${maudeFloat (speed(i))} .\n")
        end for

        out.append ("endfm\n")
        out.toString
    end idmSpeedTableModule

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Format a finite JVM double as a decimal Maude Float literal.
     */
    private [ir] def maudeFloat (value: Double): String =
        require (value.isFinite, s"IDM table contains non-finite value $value")
        val decimal = java.math.BigDecimal.valueOf (value).stripTrailingZeros ().toPlainString
        if decimal.contains (".") then decimal else decimal + ".0"
    end maudeFloat

end MesoSpecMaude


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runMesoSpecMaudeIdmTable` main function checks that the exported
 *  Maude module contains the same endpoint samples as the native `IdmFlux`.
 *  > runMain scalation.simulation.process.ir.runMesoSpecMaudeIdmTable
 */
@main def runMesoSpecMaudeIdmTable (): Unit =

    banner ("MesoSpec IDM table export test")

    val flux = new IdmFlux ()
    val sourceHash = "0123456789abcdef" * 4
    val module = MesoSpecMaude.idmSpeedTableModule (flux, sourceHash)
    val density = flux.rhoGrid
    val speed = flux.vGrid
    val moduleName = s"IDM_SPEED_TABLE_${sourceHash.take (16).toUpperCase}"

    assert (module.startsWith (s"*** Imported from Scalation IdmFlux; source MicroIR SHA-256: $sourceHash\n"))
    assert (module.contains (s"fmod $moduleName is"))
    assert (module.contains (s"eq tableSize = ${density.dim} ."))
    assert (module.contains (s"eq rhoAt(0) = ${MesoSpecMaude.maudeFloat (density(0))} ."))
    assert (module.contains (s"eq speedAt(0) = ${MesoSpecMaude.maudeFloat (speed(0))} ."))
    assert (module.contains (s"eq rhoAt(${density.dim - 1}) = ${MesoSpecMaude.maudeFloat (density(density.dim - 1))} ."))
    assert (module.contains (s"eq speedAt(${speed.dim - 1}) = ${MesoSpecMaude.maudeFloat (speed(speed.dim - 1))} ."))

    banner ("MesoSpec IDM table export test: PASS")
    println (s"source_microir_sha256 = $sourceHash")
    println (s"exported_table_rows = ${density.dim}")

end runMesoSpecMaudeIdmTable
