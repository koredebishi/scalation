//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Sep 13 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Typed Specification for Vehicle-Based Mesoscopic Traffic Models
 */

package scalation
package simulation
package process
package ir

import scala.collection.mutable.ArrayBuffer as VEC
import scala.jdk.CollectionConverters.*

import com.google.gson.{JsonArray, JsonElement, JsonObject, JsonParser}

import scalation.simulation.process.config.{CarFollowingModel, ODESolverType}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Enumeration for simultaneous mesoscopic event phases in execution order.
 */
enum MesoEventPhase:
    case SpaceRelease                                // downstream storage becomes available
    case Release                                     // a ready vehicle transfers or departs
    case Ready                                       // a vehicle completes free-flow traversal
end MesoEventPhase

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoNode` class stores one mesoscopic network node.
 *  @param id    the stable node identifier
 *  @param kind  the semantic role of the node
 */
case class MesoNode (id: String, kind: MicroNodeKind)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoLink` class specifies one directed, single-lane mesoscopic link.
 *  Its FIFO transit/ready queue and blocked-entry queue are fixed v0.1
 *  semantics implemented by `MesoVTransport`.
 *  @param id               the stable link identifier
 *  @param microSegments    the microscopic segments represented by this link
 *  @param from             the upstream node identifier
 *  @param to               the downstream node identifier
 *  @param length           the physical link length in metres
 *  @param freeFlowTime     the free-flow traversal time in seconds
 *  @param capacity         the discharge capacity in vehicles/hour
 *  @param storageCapacity  the maximum number of admitted vehicles
 */
case class MesoLink (id: String, microSegments: Array [String], from: String, to: String,
                     length: Double, freeFlowTime: Double, capacity: Double,
                     storageCapacity: Int)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoMovement` class specifies one route-conditioned junction movement.
 *  @param id        the stable movement identifier
 *  @param node      the junction where the movement occurs
 *  @param fromLink  the incoming link identifier
 *  @param toLink    the outgoing link identifier
 *  @param routes    the routes allowed to use this movement
 */
case class MesoMovement (id: String, node: String, fromLink: String, toLink: String,
                         routes: Array [String])

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoSource` class stores one mesoscopic demand source.
 *  The arrival process is preserved from MicroIR rather than redefined.
 *  @param id       the stable source identifier
 *  @param node     the source node identifier
 *  @param arrival  the source inter-arrival specification
 */
case class MesoSource (id: String, node: String, arrival: MicroArrival)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoRoute` class stores one ordered mesoscopic route.
 *  @param id           the stable route identifier
 *  @param source       the source node identifier
 *  @param destination  the destination node identifier
 *  @param links        the links in traversal order
 *  @param probability  the route-choice probability
 */
case class MesoRoute (id: String, source: String, destination: String,
                      links: Array [String], probability: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoDemand` class stores mesoscopic sources and routes.
 *  @param sources  the demand sources
 *  @param routes   the available routes
 */
case class MesoDemand (sources: Array [MesoSource], routes: Array [MesoRoute])

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `UnsupportedMesoSpecVersion` exception indicates an unsupported schema.
 *  @param version  the unsupported schema version
 */
class UnsupportedMesoSpecVersion (version: String)
      extends IllegalArgumentException (s"unsupported MesoSpec version '$version'")

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoObservable` class identifies one retained mesoscopic measure.
 *  @param measure   the retained measure
 *  @param location  the link or route where it is observed
 */
case class MesoObservable (measure: MicroMeasure, location: String)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoSpec` class is the typed v0.1 specification consumed by both the
 *  ScalaTion and Maude translators.
 *  @param version         the MesoSpec schema version
 *  @param microIRVersion  the source MicroIR schema version
 *  @param microIRSha256   the identity of the source canonical MicroIR
 *  @param nodes           the mesoscopic network nodes
 *  @param links           the directed mesoscopic links
 *  @param movements       the allowed junction movements
 *  @param demand          the mesoscopic demand
 *  @param observables     the measures retained for comparison
 *  @param eventOrder      the simultaneous-event phase order
 */
case class MesoSpec (version: String, microIRVersion: String, microIRSha256: String,
                     nodes: Array [MesoNode], links: Array [MesoLink],
                     movements: Array [MesoMovement], demand: MesoDemand,
                     observables: Array [MesoObservable],
                     eventOrder: Array [MesoEventPhase]):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return validation errors in deterministic order.
     */
    def validationErrors: Array [String] = MesoSpec.validationErrors (this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate this MesoSpec, throwing one error containing all flaws.
     */
    def validate (): Unit =
        val errors = validationErrors                            // deterministic flaws
        if errors.nonEmpty then
            throw new IllegalArgumentException (errors.mkString ("invalid MesoSpec: ", "; ", ""))
    end validate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate this MesoSpec as an abstraction of the given MicroIR.
     *  @param micro  the microscopic model claimed by this specification
     */
    def validateAgainst (micro: MicroIR): Unit =
        validate ()                                             // first ensure a legal MesoSpec
        micro.validate ()                                       // and a legal source MicroIR
        val errors = MesoSpec.validationErrorsAgainst (this, micro)
        if errors.nonEmpty then
            throw new IllegalArgumentException (
                errors.mkString ("invalid MesoSpec abstraction: ", "; ", ""))
    end validateAgainst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return this MesoSpec as canonical RFC 8785 JSON.
     */
    def json: String = MesoSpecJson (this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return this MesoSpec as indented JSON for display.
     */
    def prettyJson: String = MesoSpecJson.pretty (this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the SHA-256 identity of this canonical MesoSpec.
     */
    def sha256: String = MesoSpecJson.sha256 (this)

end MesoSpec


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoSpec` companion object contains the v0.1 semantic contract.
 */
object MesoSpec:

    val version = "0.1"                                        // current schema version

    def eventOrder: Array [MesoEventPhase] =
        Array (MesoEventPhase.SpaceRelease,                     // storage is freed first
               MesoEventPhase.Release,                          // then eligible transfers occur
               MesoEventPhase.Ready)                            // then new readiness is processed
    end eventOrder

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return validation errors for the given MesoSpec.
     *  @param spec  the mesoscopic specification
     */
    private [ir] def validationErrors (spec: MesoSpec): Array [String] =
        val errors      = VEC [String] ()                        // accumulated flaws
        val nodeIds     = spec.nodes.map (_.id)                  // declared node identifiers
        val linkIds     = spec.links.map (_.id)                  // declared link identifiers
        val movementIds = spec.movements.map (_.id)              // declared movement identifiers
        val sourceIds   = spec.demand.sources.map (_.id)         // declared source identifiers
        val routeIds    = spec.demand.routes.map (_.id)          // declared route identifiers
        val nodeMap     = spec.nodes.map (n => n.id -> n).toMap  // node lookup
        val linkMap     = spec.links.map (e => e.id -> e).toMap  // link lookup
        val routeMap    = spec.demand.routes.map (r => r.id -> r).toMap
        val linkSet     = linkIds.toSet                          // observable lookup
        val routeSet    = routeIds.toSet                         // observable lookup

        if spec.version != version then errors += s"unsupported version '${spec.version}'"
        if spec.microIRVersion != MicroIR.version then
            errors += s"unsupported source MicroIR version '${spec.microIRVersion}'"
        if !spec.microIRSha256.matches ("[0-9a-f]{64}") then
            errors += "microIRSha256 must contain 64 lowercase hexadecimal characters"

        if spec.nodes.isEmpty then errors += "nodes must not be empty"
        if spec.links.isEmpty then errors += "links must not be empty"
        if spec.demand.sources.isEmpty then errors += "demand sources must not be empty"
        if spec.demand.routes.isEmpty then errors += "demand routes must not be empty"

        addEmptyIds (errors, "node", nodeIds)
        addEmptyIds (errors, "link", linkIds)
        addEmptyIds (errors, "movement", movementIds)
        addEmptyIds (errors, "source", sourceIds)
        addEmptyIds (errors, "route", routeIds)
        addDuplicates (errors, "node", nodeIds)
        addDuplicates (errors, "link", linkIds)
        addDuplicates (errors, "movement", movementIds)
        addDuplicates (errors, "source", sourceIds)
        addDuplicates (errors, "route", routeIds)

        validateLinks (errors, spec.links, nodeMap)
        validateMovements (errors, spec.movements, nodeMap, linkMap, routeMap)
        validateDemand (errors, spec.demand, nodeMap, linkMap, spec.movements)
        validateObservables (errors, spec.observables, linkSet, routeSet)

        if !spec.eventOrder.sameElements (eventOrder) then
            errors += "eventOrder must be [SpaceRelease,Release,Ready]"

        errors.sorted.toArray
    end validationErrors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate mesoscopic links and microscopic-segment ownership.
     */
    private def validateLinks (errors: VEC [String], links: Array [MesoLink],
                               nodeMap: Map [String, MesoNode]): Unit =
        val microSegments = links.flatMap (_.microSegments)      // all represented segments
        addEmptyIds (errors, "microscopic segment", microSegments)
        addDuplicates (errors, "microscopic segment mapping", microSegments)

        for link <- links.sortBy (_.id) do
            if link.microSegments.isEmpty then
                errors += s"link '${link.id}' represents no microscopic segments"
            if !nodeMap.contains (link.from) then
                errors += s"link '${link.id}' references missing from-node '${link.from}'"
            if !nodeMap.contains (link.to) then
                errors += s"link '${link.id}' references missing to-node '${link.to}'"
            if !link.length.isFinite || link.length <= 0.0 then
                errors += s"link '${link.id}' has nonpositive length ${link.length}"
            if !link.freeFlowTime.isFinite || link.freeFlowTime <= 0.0 then
                errors += s"link '${link.id}' has nonpositive freeFlowTime ${link.freeFlowTime}"
            if !link.capacity.isFinite || link.capacity <= 0.0 then
                errors += s"link '${link.id}' has nonpositive capacity ${link.capacity}"
            if link.storageCapacity <= 0 then
                errors += s"link '${link.id}' has nonpositive storageCapacity ${link.storageCapacity}"
        end for
    end validateLinks

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate movement topology, route conditions and priorities.
     */
    private def validateMovements (errors: VEC [String], movements: Array [MesoMovement],
                                   nodeMap: Map [String, MesoNode],
                                   linkMap: Map [String, MesoLink],
                                   routeMap: Map [String, MesoRoute]): Unit =
        val selections = VEC [(String, String, String)] ()       // node, incoming link, route

        for movement <- movements.sortBy (_.id) do
            nodeMap.get (movement.node) match
                case None => errors += s"movement '${movement.id}' references missing node '${movement.node}'"
                case Some (node) if node.kind != MicroNodeKind.Junction =>
                    errors += s"movement '${movement.id}' node '${movement.node}' is not a junction"
                case _ => ()

            linkMap.get (movement.fromLink) match
                case None => errors += s"movement '${movement.id}' references missing from-link '${movement.fromLink}'"
                case Some (link) if link.to != movement.node =>
                    errors += s"movement '${movement.id}' from-link '${movement.fromLink}' does not enter '${movement.node}'"
                case _ => ()

            linkMap.get (movement.toLink) match
                case None => errors += s"movement '${movement.id}' references missing to-link '${movement.toLink}'"
                case Some (link) if link.from != movement.node =>
                    errors += s"movement '${movement.id}' to-link '${movement.toLink}' does not leave '${movement.node}'"
                case _ => ()

            if movement.routes.isEmpty then
                errors += s"movement '${movement.id}' has no route condition"
            addEmptyIds (errors, s"movement '${movement.id}' route", movement.routes)
            addDuplicates (errors, s"movement '${movement.id}' route", movement.routes)
            for routeId <- movement.routes.sorted do
                routeMap.get (routeId) match
                    case None => errors += s"movement '${movement.id}' references missing route '$routeId'"
                    case Some (route) =>
                        selections += ((movement.node, movement.fromLink, routeId))
                        if !usesMovement (route, movement.fromLink, movement.toLink) then
                            errors += s"movement '${movement.id}' route '$routeId' does not traverse " +
                                      s"'${movement.fromLink}' to '${movement.toLink}'"
            end for
        end for

        val ambiguous = selections.groupBy (identity).collect {
            case (selection, values) if values.length > 1 => selection
        }.toArray.sortBy (x => (x._1, x._2, x._3))
        for (node, incoming, routeId) <- ambiguous do
            errors += s"node '$node' has multiple movements from '$incoming' for route '$routeId'"
    end validateMovements

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate demand sources, arrival processes and ordered routes.
     */
    private def validateDemand (errors: VEC [String], demand: MesoDemand,
                                nodeMap: Map [String, MesoNode],
                                linkMap: Map [String, MesoLink],
                                movements: Array [MesoMovement]): Unit =
        val sourceNodes = demand.sources.map (_.node).toSet       // nodes with declared demand

        for source <- demand.sources.sortBy (_.id) do
            nodeMap.get (source.node) match
                case None => errors += s"source '${source.id}' references missing node '${source.node}'"
                case Some (node) if node.kind != MicroNodeKind.Source =>
                    errors += s"source '${source.id}' node '${source.node}' is not a source"
                case _ => ()
            validateArrival (errors, source)
        end for

        for route <- demand.routes.sortBy (_.id) do
            if !sourceNodes.contains (route.source) then
                errors += s"route '${route.id}' has no demand source at '${route.source}'"
            nodeMap.get (route.destination) match
                case None => errors += s"route '${route.id}' references missing destination '${route.destination}'"
                case Some (node) if node.kind != MicroNodeKind.Sink =>
                    errors += s"route '${route.id}' destination '${route.destination}' is not a sink"
                case _ => ()
            if !route.probability.isFinite || route.probability < 0.0 || route.probability > 1.0 then
                errors += s"route '${route.id}' has probability ${route.probability} outside [0,1]"
            validateRoute (errors, route, linkMap, movements)
        end for

        for source <- sourceNodes.toArray.sorted do
            val probability = demand.routes.filter (_.source == source).map (_.probability).sum
            if math.abs (probability - 1.0) > 1E-9 then
                errors += s"route probabilities from '$source' sum to $probability instead of 1.0"
        end for
    end validateDemand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate one source's preserved MicroIR arrival specification.
     */
    private def validateArrival (errors: VEC [String], source: MesoSource): Unit =
        val owner      = s"source '${source.id}' arrival"       // validation owner
        val parameters = source.arrival.parameters              // supplied parameters
        val names      = parameters.map (_.name).sorted          // supplied parameter names
        val expected = source.arrival.model match               // required native parameters
            case MicroArrivalModel.Deterministic => Array ("x")
            case MicroArrivalModel.Exponential   => Array ("mu")
            case MicroArrivalModel.Erlang        => Array ("k", "mu")
            case MicroArrivalModel.Erlang2S      => Array ("mu", "tau")

        addEmptyIds (errors, s"$owner parameter", names)
        addDuplicates (errors, s"$owner parameter", names)
        for parameter <- parameters.sortBy (_.name) do
            if !parameter.value.isFinite then
                errors += s"$owner parameter '${parameter.name}' is not finite"
        end for

        if !names.sameElements (expected) then
            errors += s"$owner ${source.arrival.model} requires parameters [${expected.mkString (",")}]"
        else
            def value (name: String): Double = parameters.find (_.name == name).get.value
            def positive (name: String): Unit =
                if value (name) <= 0.0 then errors += s"$owner parameter '$name' must be positive"
            end positive

            source.arrival.model match
                case MicroArrivalModel.Deterministic => positive ("x")
                case MicroArrivalModel.Exponential   => positive ("mu")
                case MicroArrivalModel.Erlang =>
                    positive ("mu")
                    positive ("k")
                    if value ("k") % 1.0 != 0.0 then
                        errors += s"$owner parameter 'k' must be an integer"
                case MicroArrivalModel.Erlang2S =>
                    positive ("mu")
                    positive ("tau")
                    if value ("tau") >= value ("mu") then
                        errors += s"$owner parameter 'tau' must be less than 'mu'"
        end if
    end validateArrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate the connectivity and movements of one ordered route.
     */
    private def validateRoute (errors: VEC [String], route: MesoRoute,
                               linkMap: Map [String, MesoLink],
                               movements: Array [MesoMovement]): Unit =
        if route.links.isEmpty then
            errors += s"route '${route.id}' has no links"
            return
        end if

        val path = Array.ofDim [MesoLink] (route.links.length)   // resolved ordered route
        var complete = true                                     // whether every link resolved
        cfor (0, route.links.length) { i =>
            linkMap.get (route.links(i)) match
                case Some (link) => path(i) = link
                case None =>
                    complete = false
                    errors += s"route '${route.id}' references missing link '${route.links(i)}'"
        }
        if !complete then return

        if path(0).from != route.source then
            errors += s"route '${route.id}' does not begin at '${route.source}'"
        cfor (1, path.length) { i =>
            val incoming = path(i - 1)                          // link entering the junction
            val outgoing = path(i)                              // link leaving the junction
            if incoming.to != outgoing.from then
                errors += s"route '${route.id}' is disconnected between '${incoming.id}' and '${outgoing.id}'"
            else if !movements.exists (m => m.node == incoming.to &&
                                            m.fromLink == incoming.id &&
                                            m.toLink == outgoing.id &&
                                            m.routes.contains (route.id)) then
                errors += s"route '${route.id}' has no movement from '${incoming.id}' to '${outgoing.id}'"
        }
        if path.last.to != route.destination then
            errors += s"route '${route.id}' does not end at '${route.destination}'"
    end validateRoute

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the route traverses the given ordered link pair.
     */
    private def usesMovement (route: MesoRoute, fromLink: String, toLink: String): Boolean =
        var found = false                                       // matching adjacent pair found
        cfor (1, route.links.length) { i =>
            if route.links(i - 1) == fromLink && route.links(i) == toLink then found = true
        }
        found
    end usesMovement

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return deterministic errors for the claimed MicroIR abstraction.
     */
    private def validationErrorsAgainst (spec: MesoSpec, micro: MicroIR): Array [String] =
        val errors          = VEC [String] ()                    // accumulated cross-model flaws
        val microSegmentMap = micro.network.segments.map (s => s.id -> s).toMap
        val mappedSegments  = spec.links.flatMap (_.microSegments)
        val mappedSet       = mappedSegments.toSet               // microscopic ownership lookup
        val microSegmentSet = microSegmentMap.keySet             // source segment lookup

        if spec.microIRVersion != micro.version then
            errors += s"microIRVersion '${spec.microIRVersion}' does not match '${micro.version}'"
        if spec.microIRSha256 != micro.sha256 then
            errors += s"microIRSha256 '${spec.microIRSha256}' does not match '${micro.sha256}'"

        for segment <- (mappedSet -- microSegmentSet).toArray.sorted do
            errors += s"mapped microscopic segment '$segment' does not exist"

        for link <- spec.links.sortBy (_.id) do
            val path = Array.ofDim [MicroSegment] (link.microSegments.length)
            var complete = true                                 // every mapped segment resolved
            cfor (0, link.microSegments.length) { i =>
                microSegmentMap.get (link.microSegments(i)) match
                    case Some (segment) => path(i) = segment
                    case None => complete = false
            }
            if complete && path.nonEmpty then
                cfor (1, path.length) { i =>
                    if path(i - 1).to != path(i).from then
                        errors += s"link '${link.id}' mapping is disconnected between " +
                                  s"'${path(i - 1).id}' and '${path(i).id}'"
                }
                if link.from != path(0).from then
                    errors += s"link '${link.id}' from-node '${link.from}' does not match " +
                              s"microscopic boundary '${path(0).from}'"
                if link.to != path.last.to then
                    errors += s"link '${link.id}' to-node '${link.to}' does not match " +
                              s"microscopic boundary '${path.last.to}'"
                val length = path.map (_.length).sum            // preserved physical length
                if !close (link.length, length) then
                    errors += s"link '${link.id}' length ${link.length} does not match " +
                              s"microscopic length $length"
            end if
        end for

        errors.sorted.toArray
    end validationErrorsAgainst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether two semantic numeric values agree within roundoff.
     */
    private def close (x: Double, y: Double): Boolean =
        math.abs (x - y) <= 1E-9 * math.max (1.0, math.max (math.abs (x), math.abs (y)))
    end close

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate retained observables and their locations.
     */
    private def validateObservables (errors: VEC [String], observables: Array [MesoObservable],
                                     linkSet: Set [String], routeSet: Set [String]): Unit =
        val keys = observables.map (x => s"${x.measure}:${x.location}") // duplicate lookup
        addDuplicates (errors, "observable", keys)

        for observable <- observables.sortBy (x => (x.measure.toString, x.location)) do
            observable.measure match
                case MicroMeasure.TravelTime =>
                    if !routeSet.contains (observable.location) then
                        errors += s"travel-time observable references missing route '${observable.location}'"
                case _ =>
                    if !linkSet.contains (observable.location) then
                        errors += s"${observable.measure} observable references missing link '${observable.location}'"
        end for
    end validateObservables

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add empty identifier errors for one entity kind.
     */
    private def addEmptyIds (errors: VEC [String], kind: String, ids: Array [String]): Unit =
        if ids.exists (_.isBlank) then errors += s"$kind identifier is empty"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add duplicate identifier errors for one entity kind.
     */
    private def addDuplicates (errors: VEC [String], kind: String, ids: Array [String]): Unit =
        val duplicates = ids.groupBy (identity).collect {
            case (id, values) if values.length > 1 => id
        }.toArray.sorted
        for id <- duplicates do errors += s"duplicate $kind identifier '$id'"
    end addDuplicates

end MesoSpec


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MesoSpecJson` object explicitly encodes MesoSpec as canonical JSON.
 */
object MesoSpecJson:

    private val rootFields = Array ("version", "microIRVersion", "microIRSha256",
                                    "nodes", "links", "movements", "demand",
                                    "observables", "eventOrder")       // MesoSpec fields
    private val nodeFields = Array ("id", "kind")                     // MesoNode fields
    private val linkFields = Array ("id", "microSegments", "from", "to", "length",
                                    "freeFlowTime", "capacity", "storageCapacity")
    private val movementFields = Array ("id", "node", "fromLink", "toLink",
                                        "routes")                      // MesoMovement fields
    private val demandFields = Array ("sources", "routes")             // MesoDemand fields
    private val sourceFields = Array ("id", "node", "arrival")        // MesoSource fields
    private val arrivalFields = Array ("model", "parameters")          // MicroArrival fields
    private val parameterFields = Array ("name", "value")              // MicroParameter fields
    private val routeFields = Array ("id", "source", "destination",
                                     "links", "probability")           // MesoRoute fields
    private val observableFields = Array ("measure", "location")       // MesoObservable fields

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode and canonicalize the given MesoSpec.
     *  @param spec  the mesoscopic specification
     */
    def apply (spec: MesoSpec): String =
        spec.validate ()
        CanonicalJson (raw (spec))
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode the given MesoSpec as indented JSON for display.
     *  @param spec  the mesoscopic specification
     */
    def pretty (spec: MesoSpec): String = CanonicalJson.pretty (apply (spec))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the SHA-256 identity of the given MesoSpec.
     *  @param spec  the mesoscopic specification
     */
    private [ir] def sha256 (spec: MesoSpec): String = CanonicalJson.sha256 (apply (spec))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the canonical JSON Schema supplied to the LLM for MesoSpec v0.1.
     */
    def schema: String = CanonicalJson (rawSchema)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the MesoSpec JSON Schema as indented JSON for display or prompts.
     */
    def prettySchema: String = CanonicalJson.pretty (schema)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse and validate one strict MesoSpec JSON document.
     *  @param json  the MesoSpec JSON document
     */
    def parse (json: String): MesoSpec =
        val root = asObject (JsonParser.parseString (json), "MesoSpec")
        checkFields (root, "MesoSpec", rootFields*)

        val version = stringField (root, "version", "MesoSpec")
        if version != MesoSpec.version then throw new UnsupportedMesoSpecVersion (version)

        val spec = MesoSpec (
            version,
            stringField (root, "microIRVersion", "MesoSpec"),
            stringField (root, "microIRSha256", "MesoSpec"),
            parseNodes (arrayField (root, "nodes", "MesoSpec")),
            parseLinks (arrayField (root, "links", "MesoSpec")),
            parseMovements (arrayField (root, "movements", "MesoSpec")),
            parseDemand (objectField (root, "demand", "MesoSpec")),
            parseObservables (arrayField (root, "observables", "MesoSpec")),
            parseEventOrder (arrayField (root, "eventOrder", "MesoSpec")))
        spec.validate ()
        spec
    end parse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode the given MesoSpec as a JSON document.
     *  Entity collections use stable identifiers while path collections retain
     *  their semantic traversal order.
     */
    private def raw (spec: MesoSpec): String =
        val root = new JsonObject ()                             // MesoSpec document root
        root.addProperty ("version", spec.version)
        root.addProperty ("microIRVersion", spec.microIRVersion)
        root.addProperty ("microIRSha256", spec.microIRSha256)
        root.add ("nodes", encodeNodes (spec.nodes))
        root.add ("links", encodeLinks (spec.links))
        root.add ("movements", encodeMovements (spec.movements))
        root.add ("demand", encodeDemand (spec.demand))
        root.add ("observables", encodeObservables (spec.observables))
        root.add ("eventOrder", encodeEventOrder (spec.eventOrder))
        root.toString
    end raw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the MesoSpec v0.1 JSON Schema from the parser's field contracts.
     */
    private def rawSchema: String =
        val definitions = new JsonObject ()                      // reusable record schemas

        definitions.add ("node", objectSchema (nodeFields, propertySchemas (
            "id" -> nonEmptyStringSchema,
            "kind" -> enumSchema (MicroNodeKind.values.map (nodeKind)))))

        definitions.add ("link", objectSchema (linkFields, propertySchemas (
            "id" -> nonEmptyStringSchema,
            "microSegments" -> arraySchema (nonEmptyStringSchema, true),
            "from" -> nonEmptyStringSchema,
            "to" -> nonEmptyStringSchema,
            "length" -> typeSchema ("number"),
            "freeFlowTime" -> typeSchema ("number"),
            "capacity" -> typeSchema ("number"),
            "storageCapacity" -> typeSchema ("integer"))))

        definitions.add ("movement", objectSchema (movementFields, propertySchemas (
            "id" -> nonEmptyStringSchema,
            "node" -> nonEmptyStringSchema,
            "fromLink" -> nonEmptyStringSchema,
            "toLink" -> nonEmptyStringSchema,
            "routes" -> arraySchema (nonEmptyStringSchema, true))))

        definitions.add ("parameter", objectSchema (parameterFields, propertySchemas (
            "name" -> nonEmptyStringSchema,
            "value" -> typeSchema ("number"))))

        definitions.add ("arrival", objectSchema (arrivalFields, propertySchemas (
            "model" -> enumSchema (MicroArrivalModel.values.map (arrivalModel)),
            "parameters" -> arraySchema (referenceSchema ("parameter"), true))))

        definitions.add ("source", objectSchema (sourceFields, propertySchemas (
            "id" -> nonEmptyStringSchema,
            "node" -> nonEmptyStringSchema,
            "arrival" -> referenceSchema ("arrival"))))

        definitions.add ("route", objectSchema (routeFields, propertySchemas (
            "id" -> nonEmptyStringSchema,
            "source" -> nonEmptyStringSchema,
            "destination" -> nonEmptyStringSchema,
            "links" -> arraySchema (nonEmptyStringSchema, true),
            "probability" -> typeSchema ("number"))))

        definitions.add ("demand", objectSchema (demandFields, propertySchemas (
            "sources" -> arraySchema (referenceSchema ("source"), true),
            "routes" -> arraySchema (referenceSchema ("route"), true))))

        definitions.add ("observable", objectSchema (observableFields, propertySchemas (
            "measure" -> enumSchema (MicroMeasure.values.map (measure)),
            "location" -> nonEmptyStringSchema)))

        val root = objectSchema (rootFields, propertySchemas ( // complete MesoSpec document
            "version" -> enumSchema (Array (MesoSpec.version)),
            "microIRVersion" -> enumSchema (Array (MicroIR.version)),
            "microIRSha256" -> patternSchema ("^[0-9a-f]{64}$"),
            "nodes" -> arraySchema (referenceSchema ("node"), true),
            "links" -> arraySchema (referenceSchema ("link"), true),
            "movements" -> arraySchema (referenceSchema ("movement"), false),
            "demand" -> referenceSchema ("demand"),
            "observables" -> arraySchema (referenceSchema ("observable"), false),
            "eventOrder" -> arraySchema (
                enumSchema (MesoEventPhase.values.map (eventPhase)), true)))
        root.addProperty ("$schema", "https://json-schema.org/draft/2020-12/schema")
        root.addProperty ("title", "ScalaTion MesoSpec v0.1")
        root.add ("$defs", definitions)
        root.toString
    end rawSchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build one closed JSON object schema.
     */
    private def objectSchema (fields: Array [String], properties: JsonObject): JsonObject =
        val schema = typeSchema ("object")                       // closed record schema
        schema.addProperty ("additionalProperties", false)
        schema.add ("required", encodeStrings (fields, false))
        schema.add ("properties", properties)
        schema
    end objectSchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the property table for one JSON object schema.
     */
    private def propertySchemas (entries: (String, JsonElement)*): JsonObject =
        val properties = new JsonObject ()                       // property-name lookup
        for (name, schema) <- entries do properties.add (name, schema)
        properties
    end propertySchemas

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a JSON schema for one primitive type.
     */
    private def typeSchema (name: String): JsonObject =
        val schema = new JsonObject ()                           // primitive schema
        schema.addProperty ("type", name)
        schema
    end typeSchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a JSON schema for a non-empty string.
     */
    private def nonEmptyStringSchema: JsonObject =
        val schema = typeSchema ("string")                       // identifier schema
        schema.addProperty ("minLength", 1)
        schema
    end nonEmptyStringSchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a JSON schema for a string matching the given pattern.
     */
    private def patternSchema (pattern: String): JsonObject =
        val schema = typeSchema ("string")                       // constrained string schema
        schema.addProperty ("pattern", pattern)
        schema
    end patternSchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a JSON schema for one closed enumeration.
     */
    private def enumSchema (values: Array [String]): JsonObject =
        val schema = typeSchema ("string")                       // enumeration schema
        schema.add ("enum", encodeStrings (values, false))
        schema
    end enumSchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a JSON schema for an array containing the given item schema.
     */
    private def arraySchema (items: JsonElement, nonEmpty: Boolean): JsonObject =
        val schema = typeSchema ("array")                        // collection schema
        schema.add ("items", items)
        if nonEmpty then schema.addProperty ("minItems", 1)
        schema
    end arraySchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a reference to a definition in the MesoSpec schema.
     */
    private def referenceSchema (name: String): JsonObject =
        val schema = new JsonObject ()                           // local definition reference
        schema.addProperty ("$ref", s"#/$$defs/$name")
        schema
    end referenceSchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode nodes in stable identifier order.
     */
    private def encodeNodes (nodes: Array [MesoNode]): JsonArray =
        val json = new JsonArray ()                              // encoded nodes
        for node <- nodes.sortBy (_.id) do
            val value = new JsonObject ()
            value.addProperty ("id", node.id)
            value.addProperty ("kind", nodeKind (node.kind))
            json.add (value)
        end for
        json
    end encodeNodes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode links in stable identifier order.
     */
    private def encodeLinks (links: Array [MesoLink]): JsonArray =
        val json = new JsonArray ()                              // encoded links
        for link <- links.sortBy (_.id) do
            val value = new JsonObject ()
            value.addProperty ("id", link.id)
            value.add ("microSegments", encodeStrings (link.microSegments, false))
            value.addProperty ("from", link.from)
            value.addProperty ("to", link.to)
            value.addProperty ("length", link.length)
            value.addProperty ("freeFlowTime", link.freeFlowTime)
            value.addProperty ("capacity", link.capacity)
            value.addProperty ("storageCapacity", link.storageCapacity)
            json.add (value)
        end for
        json
    end encodeLinks

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode movements in stable identifier order.
     */
    private def encodeMovements (movements: Array [MesoMovement]): JsonArray =
        val json = new JsonArray ()                              // encoded movements
        for movement <- movements.sortBy (_.id) do
            val value = new JsonObject ()
            value.addProperty ("id", movement.id)
            value.addProperty ("node", movement.node)
            value.addProperty ("fromLink", movement.fromLink)
            value.addProperty ("toLink", movement.toLink)
            value.add ("routes", encodeStrings (movement.routes, true))
            json.add (value)
        end for
        json
    end encodeMovements

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode mesoscopic demand in stable identifier order.
     */
    private def encodeDemand (demand: MesoDemand): JsonObject =
        val json    = new JsonObject ()                          // encoded demand
        val sources = new JsonArray ()                           // encoded sources
        val routes  = new JsonArray ()                           // encoded routes

        for source <- demand.sources.sortBy (_.id) do
            val value = new JsonObject ()
            value.addProperty ("id", source.id)
            value.addProperty ("node", source.node)
            value.add ("arrival", encodeArrival (source.arrival))
            sources.add (value)
        end for

        for route <- demand.routes.sortBy (_.id) do
            val value = new JsonObject ()
            value.addProperty ("id", route.id)
            value.addProperty ("source", route.source)
            value.addProperty ("destination", route.destination)
            value.add ("links", encodeStrings (route.links, false))
            value.addProperty ("probability", route.probability)
            routes.add (value)
        end for

        json.add ("sources", sources)
        json.add ("routes", routes)
        json
    end encodeDemand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode one arrival-process specification.
     */
    private def encodeArrival (arrival: MicroArrival): JsonObject =
        val json       = new JsonObject ()                       // encoded arrival process
        val parameters = new JsonArray ()                        // encoded parameters
        for parameter <- arrival.parameters.sortBy (_.name) do
            val value = new JsonObject ()
            value.addProperty ("name", parameter.name)
            value.addProperty ("value", parameter.value)
            parameters.add (value)
        end for
        json.addProperty ("model", arrivalModel (arrival.model))
        json.add ("parameters", parameters)
        json
    end encodeArrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode observables in stable measure/location order.
     */
    private def encodeObservables (observables: Array [MesoObservable]): JsonArray =
        val json = new JsonArray ()                              // encoded observables
        for observable <- observables.sortBy (x => (measure (x.measure), x.location)) do
            val value = new JsonObject ()
            value.addProperty ("measure", measure (observable.measure))
            value.addProperty ("location", observable.location)
            json.add (value)
        end for
        json
    end encodeObservables

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode the fixed simultaneous-event order without reordering it.
     */
    private def encodeEventOrder (phases: Array [MesoEventPhase]): JsonArray =
        val json = new JsonArray ()                              // encoded phase order
        for phase <- phases do json.add (eventPhase (phase))
        json
    end encodeEventOrder

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode strings, sorting only when their order is not semantic.
     */
    private def encodeStrings (strings: Array [String], sorted: Boolean): JsonArray =
        val json   = new JsonArray ()                            // encoded strings
        val values = if sorted then strings.sorted else strings
        for value <- values do json.add (value)
        json
    end encodeStrings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse mesoscopic nodes.
     */
    private def parseNodes (json: JsonArray): Array [MesoNode] =
        val nodes = Array.ofDim [MesoNode] (json.size)           // parsed nodes
        cfor (0, json.size) { i =>
            val path  = s"nodes[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, nodeFields*)
            nodes(i) = MesoNode (stringField (value, "id", path),
                                 parseNodeKind (stringField (value, "kind", path)))
        }
        nodes
    end parseNodes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse directed mesoscopic links.
     */
    private def parseLinks (json: JsonArray): Array [MesoLink] =
        val links = Array.ofDim [MesoLink] (json.size)           // parsed links
        cfor (0, json.size) { i =>
            val path  = s"links[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, linkFields*)
            links(i) = MesoLink (
                stringField (value, "id", path),
                parseStrings (arrayField (value, "microSegments", path), s"$path.microSegments"),
                stringField (value, "from", path),
                stringField (value, "to", path),
                doubleField (value, "length", path),
                doubleField (value, "freeFlowTime", path),
                doubleField (value, "capacity", path),
                intField (value, "storageCapacity", path))
        }
        links
    end parseLinks

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse route-conditioned junction movements.
     */
    private def parseMovements (json: JsonArray): Array [MesoMovement] =
        val movements = Array.ofDim [MesoMovement] (json.size)  // parsed movements
        cfor (0, json.size) { i =>
            val path  = s"movements[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, movementFields*)
            movements(i) = MesoMovement (
                stringField (value, "id", path),
                stringField (value, "node", path),
                stringField (value, "fromLink", path),
                stringField (value, "toLink", path),
                parseStrings (arrayField (value, "routes", path), s"$path.routes"))
        }
        movements
    end parseMovements

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse mesoscopic demand.
     */
    private def parseDemand (json: JsonObject): MesoDemand =
        checkFields (json, "demand", demandFields*)
        MesoDemand (parseSources (arrayField (json, "sources", "demand")),
                    parseRoutes (arrayField (json, "routes", "demand")))
    end parseDemand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse mesoscopic demand sources.
     */
    private def parseSources (json: JsonArray): Array [MesoSource] =
        val sources = Array.ofDim [MesoSource] (json.size)       // parsed sources
        cfor (0, json.size) { i =>
            val path  = s"demand.sources[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, sourceFields*)
            sources(i) = MesoSource (
                stringField (value, "id", path),
                stringField (value, "node", path),
                parseArrival (objectField (value, "arrival", path), s"$path.arrival"))
        }
        sources
    end parseSources

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse one arrival-process specification.
     */
    private def parseArrival (json: JsonObject, path: String): MicroArrival =
        checkFields (json, path, arrivalFields*)
        MicroArrival (parseArrivalModel (stringField (json, "model", path)),
                      parseParameters (arrayField (json, "parameters", path), s"$path.parameters"))
    end parseArrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse named arrival parameters.
     */
    private def parseParameters (json: JsonArray, path: String): Array [MicroParameter] =
        val parameters = Array.ofDim [MicroParameter] (json.size) // parsed parameters
        cfor (0, json.size) { i =>
            val entry = s"$path[$i]"
            val value = asObject (json.get (i), entry)
            checkFields (value, entry, parameterFields*)
            parameters(i) = MicroParameter (stringField (value, "name", entry),
                                            doubleField (value, "value", entry))
        }
        parameters
    end parseParameters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse ordered mesoscopic routes.
     */
    private def parseRoutes (json: JsonArray): Array [MesoRoute] =
        val routes = Array.ofDim [MesoRoute] (json.size)         // parsed routes
        cfor (0, json.size) { i =>
            val path  = s"demand.routes[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, routeFields*)
            routes(i) = MesoRoute (
                stringField (value, "id", path),
                stringField (value, "source", path),
                stringField (value, "destination", path),
                parseStrings (arrayField (value, "links", path), s"$path.links"),
                doubleField (value, "probability", path))
        }
        routes
    end parseRoutes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse retained observables.
     */
    private def parseObservables (json: JsonArray): Array [MesoObservable] =
        val observables = Array.ofDim [MesoObservable] (json.size) // parsed observables
        cfor (0, json.size) { i =>
            val path  = s"observables[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, observableFields*)
            observables(i) = MesoObservable (
                parseMeasure (stringField (value, "measure", path)),
                stringField (value, "location", path))
        }
        observables
    end parseObservables

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse the simultaneous-event order without reordering it.
     */
    private def parseEventOrder (json: JsonArray): Array [MesoEventPhase] =
        val phases = Array.ofDim [MesoEventPhase] (json.size)    // parsed phase order
        cfor (0, json.size) { i =>
            phases(i) = parseEventPhase (asString (json.get (i), s"eventOrder[$i]"))
        }
        phases
    end parseEventOrder

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse an array of strings without reordering it.
     */
    private def parseStrings (json: JsonArray, path: String): Array [String] =
        val strings = Array.ofDim [String] (json.size)           // parsed strings
        cfor (0, json.size) { i => strings(i) = asString (json.get (i), s"$path[$i]") }
        strings
    end parseStrings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Verify that an object has exactly the expected fields.
     */
    private def checkFields (json: JsonObject, path: String, fields: String*): Unit =
        val expected   = fields.toSet                            // allowed fields
        val missing    = fields.filterNot (json.has).sorted      // missing fields
        val unexpected = json.keySet.asScala.filterNot (expected.contains).toArray.sorted
        if missing.nonEmpty || unexpected.nonEmpty then
            val parts = VEC [String] ()
            if missing.nonEmpty then parts += s"missing [${missing.mkString (", ")}]"
            if unexpected.nonEmpty then parts += s"unknown [${unexpected.mkString (", ")}]"
            throw new IllegalArgumentException (s"$path has ${parts.mkString (" and ")} fields")
    end checkFields

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a required object field.
     */
    private def objectField (json: JsonObject, name: String, path: String): JsonObject =
        asObject (field (json, name, path), s"$path.$name")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a required array field.
     */
    private def arrayField (json: JsonObject, name: String, path: String): JsonArray =
        val value = field (json, name, path)
        if !value.isJsonArray then throw new IllegalArgumentException (s"$path.$name must be an array")
        value.getAsJsonArray
    end arrayField

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a required string field.
     */
    private def stringField (json: JsonObject, name: String, path: String): String =
        asString (field (json, name, path), s"$path.$name")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a required numeric field.
     */
    private def doubleField (json: JsonObject, name: String, path: String): Double =
        val value = field (json, name, path)
        if !value.isJsonPrimitive || !value.getAsJsonPrimitive.isNumber then
            throw new IllegalArgumentException (s"$path.$name must be a number")
        value.getAsDouble
    end doubleField

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a required exact integer field.
     */
    private def intField (json: JsonObject, name: String, path: String): Int =
        val value = field (json, name, path)
        if !value.isJsonPrimitive || !value.getAsJsonPrimitive.isNumber then
            throw new IllegalArgumentException (s"$path.$name must be an integer")
        try value.getAsBigDecimal.intValueExact
        catch case _: ArithmeticException =>
            throw new IllegalArgumentException (s"$path.$name must be an integer")
    end intField

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a required non-null field.
     */
    private def field (json: JsonObject, name: String, path: String): JsonElement =
        val value = json.get (name)
        if value == null || value.isJsonNull then
            throw new IllegalArgumentException (s"$path.$name is required")
        value
    end field

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a JSON value as an object.
     */
    private def asObject (json: JsonElement, path: String): JsonObject =
        if json == null || !json.isJsonObject then
            throw new IllegalArgumentException (s"$path must be an object")
        json.getAsJsonObject
    end asObject

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a JSON value as a string.
     */
    private def asString (json: JsonElement, path: String): String =
        if !json.isJsonPrimitive || !json.getAsJsonPrimitive.isString then
            throw new IllegalArgumentException (s"$path must be a string")
        json.getAsString
    end asString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the canonical label for a node kind.
     */
    private def nodeKind (kind: MicroNodeKind): String = kind match
        case MicroNodeKind.Source   => "source"
        case MicroNodeKind.Junction => "junction"
        case MicroNodeKind.Sink     => "sink"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the canonical label for an arrival model.
     */
    private def arrivalModel (model: MicroArrivalModel): String = model match
        case MicroArrivalModel.Deterministic => "deterministic"
        case MicroArrivalModel.Exponential   => "exponential"
        case MicroArrivalModel.Erlang        => "erlang"
        case MicroArrivalModel.Erlang2S      => "erlang2S"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the canonical label for an observable measure.
     */
    private def measure (observable: MicroMeasure): String = observable match
        case MicroMeasure.Flow       => "flow"
        case MicroMeasure.Speed      => "speed"
        case MicroMeasure.Density    => "density"
        case MicroMeasure.TravelTime => "travelTime"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the canonical label for a simultaneous-event phase.
     */
    private def eventPhase (phase: MesoEventPhase): String = phase match
        case MesoEventPhase.SpaceRelease => "spaceRelease"
        case MesoEventPhase.Release      => "release"
        case MesoEventPhase.Ready        => "ready"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse the canonical label for a node kind.
     */
    private def parseNodeKind (kind: String): MicroNodeKind = kind match
        case "source"   => MicroNodeKind.Source
        case "junction" => MicroNodeKind.Junction
        case "sink"     => MicroNodeKind.Sink
        case _ => throw new IllegalArgumentException (s"unknown node kind '$kind'")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse the canonical label for an arrival model.
     */
    private def parseArrivalModel (model: String): MicroArrivalModel = model match
        case "deterministic" => MicroArrivalModel.Deterministic
        case "exponential"   => MicroArrivalModel.Exponential
        case "erlang"        => MicroArrivalModel.Erlang
        case "erlang2S"      => MicroArrivalModel.Erlang2S
        case _ => throw new IllegalArgumentException (s"unknown arrival model '$model'")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse the canonical label for an observable measure.
     */
    private def parseMeasure (observable: String): MicroMeasure = observable match
        case "flow"       => MicroMeasure.Flow
        case "speed"      => MicroMeasure.Speed
        case "density"    => MicroMeasure.Density
        case "travelTime" => MicroMeasure.TravelTime
        case _ => throw new IllegalArgumentException (s"unknown observable measure '$observable'")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse the canonical label for a simultaneous-event phase.
     */
    private def parseEventPhase (phase: String): MesoEventPhase = phase match
        case "spaceRelease" => MesoEventPhase.SpaceRelease
        case "release"      => MesoEventPhase.Release
        case "ready"        => MesoEventPhase.Ready
        case _ => throw new IllegalArgumentException (s"unknown mesoscopic event phase '$phase'")

end MesoSpecJson


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runMesoSpec` main function tests the typed MesoSpec v0.1 contract.
 *  > runMain scalation.simulation.process.ir.runMesoSpec
 */
@main def runMesoSpec (): Unit =

    banner ("MesoSpec test")

    def rejectsAgainst (candidate: MesoSpec, micro: MicroIR): Boolean =
        try
            candidate.validateAgainst (micro)
            false
        catch case _: IllegalArgumentException => true
    end rejectsAgainst

    def rejectsJson (test: => Unit): Boolean =
        try
            test
            false
        catch case _: IllegalArgumentException => true
    end rejectsJson

    val westArrival = MicroArrival (MicroArrivalModel.Deterministic,
                                    Array (MicroParameter ("x", 2.0)))
    val rampArrival = MicroArrival (MicroArrivalModel.Exponential,
                                    Array (MicroParameter ("mu", 4.0)))

    val microNodes = Array (
        MicroNode ("west", MicroNodeKind.Source),              // mainline source
        MicroNode ("ramp", MicroNodeKind.Source),              // ramp source
        MicroNode ("merge", MicroNodeKind.Junction),           // shared merge
        MicroNode ("middle", MicroNodeKind.Junction),          // collapsed internal node
        MicroNode ("exit", MicroNodeKind.Sink))                // common destination
    val microSegments = Array (
        MicroSegment ("micro-west", "west", "merge", 500.0, 1, 25.0),
        MicroSegment ("micro-ramp", "ramp", "merge", 250.0, 1, 20.0),
        MicroSegment ("micro-exit-0", "merge", "middle", 350.0, 1, 30.0),
        MicroSegment ("micro-exit-1", "middle", "exit", 400.0, 1, 30.0))
    val microSources = Array (
        MicroSource ("west-source", "west", westArrival),
        MicroSource ("ramp-source", "ramp", rampArrival))
    val microRoutes = Array (
        MicroRoute ("west-route", "west", "exit",
                    Array ("micro-west", "micro-exit-0", "micro-exit-1"), 1.0),
        MicroRoute ("ramp-route", "ramp", "exit",
                    Array ("micro-ramp", "micro-exit-0", "micro-exit-1"), 1.0))
    val microMotion = MicroVehicleDynamics (
        CarFollowingModel.IDM,
        ODESolverType.Ballistic,
        Array (MicroParameter ("minimumGap", 2.0),
               MicroParameter ("timeHeadway", 1.5)), 0.5)
    val microObservables = Array (
        MicroObservable (MicroMeasure.Flow, "micro-exit-1"),
        MicroObservable (MicroMeasure.Density, "micro-exit-1"),
        MicroObservable (MicroMeasure.TravelTime, "west-route"))
    val micro = MicroIR (
        MicroIR.version,
        MicroNetwork (microNodes, microSegments),
        MicroDemand (microSources, microRoutes),
        microMotion,
        microObservables)
    micro.validate ()

    val west  = MesoNode ("west", MicroNodeKind.Source)        // mainline source
    val ramp  = MesoNode ("ramp", MicroNodeKind.Source)        // ramp source
    val merge = MesoNode ("merge", MicroNodeKind.Junction)     // shared merge node
    val exit  = MesoNode ("exit", MicroNodeKind.Sink)          // common destination

    val westLink = MesoLink ("west-link", Array ("micro-west"), "west", "merge",
                             500.0, 20.0, 1800.0, 25)
    val rampLink = MesoLink ("ramp-link", Array ("micro-ramp"), "ramp", "merge",
                             250.0, 12.0, 900.0, 10)
    val exitLink = MesoLink ("exit-link", Array ("micro-exit-0", "micro-exit-1"),
                             "merge", "exit", 750.0, 25.0, 1800.0, 35)

    val westMove = MesoMovement ("west-move", "merge", "west-link", "exit-link",
                                 Array ("west-route"))
    val rampMove = MesoMovement ("ramp-move", "merge", "ramp-link", "exit-link",
                                 Array ("ramp-route"))

    val westSource  = MesoSource ("west-source", "west", westArrival)
    val rampSource  = MesoSource ("ramp-source", "ramp", rampArrival)
    val westRoute   = MesoRoute ("west-route", "west", "exit",
                                 Array ("west-link", "exit-link"), 1.0)
    val rampRoute   = MesoRoute ("ramp-route", "ramp", "exit",
                                 Array ("ramp-link", "exit-link"), 1.0)
    val demand      = MesoDemand (Array (westSource, rampSource), Array (westRoute, rampRoute))
    val observables = Array (MesoObservable (MicroMeasure.Flow, "exit-link"),
                             MesoObservable (MicroMeasure.Density, "exit-link"),
                             MesoObservable (MicroMeasure.TravelTime, "west-route"))

    val spec = MesoSpec (
        MesoSpec.version,
        micro.version,
        micro.sha256,
        Array (west, ramp, merge, exit),
        Array (westLink, rampLink, exitLink),
        Array (westMove, rampMove),
        demand,
        observables,
        MesoSpec.eventOrder)

    assert (spec.validationErrors.isEmpty)                       // valid merge composition
    spec.validate ()
    spec.validateAgainst (micro)                                 // valid microscopic abstraction

    val json       = spec.json                                   // canonical interchange artifact
    val schema     = MesoSpecJson.schema                         // canonical prompt contract
    val schemaRoot = JsonParser.parseString (schema).getAsJsonObject
    val jsonFields = JsonParser.parseString (json).getAsJsonObject.keySet.asScala.toSet
    val schemaFields = schemaRoot.get ("properties").getAsJsonObject.keySet.asScala.toSet

    assert (CanonicalJson (schema) == schema)                    // schema itself is canonical
    assert (CanonicalJson (MesoSpecJson.prettySchema) == schema) // display form is equivalent
    assert (jsonFields == schemaFields)                          // codec and schema root agree
    assert (!schemaRoot.get ("additionalProperties").getAsBoolean) // root schema is closed

    val parsed = MesoSpecJson.parse (json)
    parsed.validateAgainst (micro)
    assert (parsed.json == json)                                 // strict round-trip stability
    assert (parsed.sha256 == spec.sha256)                        // artifact identity survives parsing

    val reordered = spec.copy (
        nodes = spec.nodes.reverse,
        links = spec.links.reverse,
        movements = spec.movements.reverse,
        demand = MesoDemand (spec.demand.sources.reverse, spec.demand.routes.reverse),
        observables = spec.observables.reverse)
    assert (reordered.json == json)                              // entity order is non-semantic
    assert (reordered.sha256 == spec.sha256)                     // identity is canonical

    val semanticChange = spec.copy (links = Array (
        westLink, rampLink, exitLink.copy (capacity = exitLink.capacity + 1.0)))
    assert (semanticChange.sha256 != spec.sha256)                // semantics change identity

    val unknownField = JsonParser.parseString (json).getAsJsonObject
    unknownField.addProperty ("extra", true)
    assert (rejectsJson { MesoSpecJson.parse (unknownField.toString) }) // strict field set

    val wrongVersionJson = JsonParser.parseString (json).getAsJsonObject
    wrongVersionJson.addProperty ("version", "0.2")
    assert (rejectsJson { MesoSpecJson.parse (wrongVersionJson.toString) }) // explicit version dispatch

    assert (MesoSpecJson.parse (spec.prettyJson).json == json)   // display form remains parseable

    val badStorage = spec.copy (links = Array (westLink.copy (storageCapacity = 0),
                                               rampLink, exitLink))
    assert (badStorage.validationErrors.nonEmpty)                // storage safety

    val badDirection = spec.copy (movements = Array (
        westMove.copy (toLink = "ramp-link"), rampMove))
    assert (badDirection.validationErrors.nonEmpty)              // movement adjacency

    val badRoute = spec.copy (demand = demand.copy (routes = Array (
        westRoute.copy (links = Array ("exit-link", "west-link")), rampRoute)))
    assert (badRoute.validationErrors.nonEmpty)                  // ordered connectivity

    val duplicateMapping = spec.copy (links = Array (
        westLink, rampLink.copy (microSegments = Array ("micro-west")), exitLink))
    assert (duplicateMapping.validationErrors.nonEmpty)          // unique abstraction ownership

    val ambiguousMove = westMove.copy (id = "ambiguous")
    val ambiguous = spec.copy (movements = Array (westMove, rampMove, ambiguousMove))
    assert (ambiguous.validationErrors.nonEmpty)                 // deterministic route choice

    val badOrder = spec.copy (eventOrder = MesoSpec.eventOrder.reverse)
    assert (badOrder.validationErrors.nonEmpty)                  // frozen event precedence

    val badProvenance = spec.copy (microIRSha256 = "not-a-hash")
    assert (badProvenance.validationErrors.nonEmpty)             // source identity

    val wrongHash = spec.copy (microIRSha256 = "0" * 64)
    assert (rejectsAgainst (wrongHash, micro))                    // exact source identity

    val inventedMapping = spec.copy (links = Array (
        westLink, rampLink,
        exitLink.copy (microSegments = Array ("micro-exit-0", "micro-ghost"))))
    assert (rejectsAgainst (inventedMapping, micro))              // no invented segment

    val reversedMapping = spec.copy (links = Array (
        westLink, rampLink, exitLink.copy (microSegments = exitLink.microSegments.reverse)))
    assert (rejectsAgainst (reversedMapping, micro))              // ordered segment chain

    val wrongLength = spec.copy (links = Array (
        westLink, rampLink, exitLink.copy (length = 751.0)))
    assert (rejectsAgainst (wrongLength, micro))                  // physical length preserved

    val wrongRouteMove = spec.copy (movements = Array (
        westMove.copy (routes = Array ("ramp-route")), rampMove))
    assert (wrongRouteMove.validationErrors.nonEmpty)             // route-conditioned movement

    println (s"mesospec_sha256 = ${spec.sha256}")

    banner ("MesoSpec test: PASS")

end runMesoSpec
