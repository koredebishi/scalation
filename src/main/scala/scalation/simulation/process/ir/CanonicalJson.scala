//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Sep 13 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    RFC 8785 Canonical JSON Support for Traffic IR Artifacts
 */

package scalation
package simulation
package process
package ir

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import com.google.gson.{GsonBuilder, JsonParser}
import org.erdtman.jcs.JsonCanonicalizer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CanonicalJson` object supplies the shared JSON representation mechanics
 *  used by the typed traffic intermediate representations.
 */
object CanonicalJson:

    private val prettyPrinter = new GsonBuilder ().setPrettyPrinting ().create ()  // display only

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Canonicalize the given JSON document using RFC 8785.
     *  @param json  the JSON document to canonicalize
     */
    def apply (json: String): String =
        new JsonCanonicalizer (json).getEncodedString
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Format the given JSON document for human-readable display.
     *  @param json  the JSON document to format
     */
    def pretty (json: String): String =
        prettyPrinter.toJson (JsonParser.parseString (apply (json)))
    end pretty

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the SHA-256 identity of the given canonical JSON document.
     *  @param json  the JSON document to identify
     */
    def sha256 (json: String): String =
        val bytes  = apply (json).getBytes (StandardCharsets.UTF_8)  // canonical UTF-8 bytes
        val digest = MessageDigest.getInstance ("SHA-256").digest (bytes)
        val hex    = new StringBuilder (digest.length * 2)           // lowercase hexadecimal identity
        for b <- digest do hex.append (f"${b & 0xff}%02x")
        hex.toString
    end sha256

end CanonicalJson


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runCanonicalJson` main function tests shared canonical JSON support.
 *  > runMain scalation.simulation.process.ir.runCanonicalJson
 */
@main def runCanonicalJson (): Unit =

    banner ("CanonicalJson test")

    val json1 = """{"z":2.0,"a":1,"items":["first","second"]}"""
    val json2 = """{"items":["first","second"],"a":1.0,"z":2}"""
    val json3 = """{"items":["second","first"],"a":1,"z":2}"""

    assert (CanonicalJson (json1) == CanonicalJson (json2))     // keys and numbers canonicalize
    assert (CanonicalJson.sha256 (json1) == CanonicalJson.sha256 (json2))
    assert (CanonicalJson.sha256 (json1) != CanonicalJson.sha256 (json3)) // array order remains semantic
    assert (CanonicalJson (CanonicalJson.pretty (json1)) == CanonicalJson (json1))

    banner ("CanonicalJson test: PASS")

end runCanonicalJson
