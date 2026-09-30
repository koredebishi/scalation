//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Shared data types for OSM network construction. */

package scalation
package simulation
package process
package builder

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** One directed road segment read from `edges.geojson`. */
case class NetEdge (u: Long, v: Long, highway: String, lanes: Int, length: Double,
                    uLat: Double, uLon: Double, vLat: Double, vLon: Double, name: String,
                    geometry: Array [(Double, Double)] = Array.empty,
                    ref: String = "", key: Int = 0, osmid: String = "")

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Generic edge behavior selected by the consuming model. */
case class EdgeProfile (group: String, micro: Boolean, speed: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Policy supplied by a model when converting raw OSM edges to simulation links. */
trait EdgePolicy:
    def profile (edge: NetEdge): EdgeProfile
end EdgePolicy
