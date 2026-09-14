//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Fri Sep 12 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Canonical Intermediate Representation for Microscopic Traffic Models
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
/** Enumeration for the semantic roles of microscopic network nodes retained
 *  by MicroIR v0.1.
 */
enum MicroNodeKind:
    case Source                                      // vehicles enter the network
    case Junction                                    // connects road segments
    case Sink                                        // vehicles leave the network
end MicroNodeKind

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Enumeration for arrival-process models supported by MicroIR v0.1.
 */
enum MicroArrivalModel:
    case Deterministic                               // maps to Sharp
    case Exponential                                 // maps to Exponential
    case Erlang                                      // maps to Erlang
    case Erlang2S                                    // maps to shifted Erlang-2
end MicroArrivalModel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Enumeration for microscopic measures retained for abstraction.
 */
enum MicroMeasure:
    case Flow                                        // vehicle flow rate
    case Speed                                       // vehicle speed
    case Density                                     // vehicles per unit distance
    case TravelTime                                  // route or segment travel time
end MicroMeasure

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroParameter` class stores one named numeric model parameter.
 *  @param name   the stable parameter name
 *  @param value  the parameter value in SI units
 */
case class MicroParameter (name: String, value: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroNode` class stores one microscopic network node.
 *  @param id    the stable node identifier
 *  @param kind  the semantic role of the node
 */
case class MicroNode (id: String, kind: MicroNodeKind)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroSegment` class stores one directed microscopic road segment.
 *  @param id             the stable segment identifier
 *  @param from           the upstream node identifier
 *  @param to             the downstream node identifier
 *  @param length         the segment length in meters
 *  @param lanes          the number of lanes
 *  @param freeFlowSpeed  the free-flow speed in meters per second
 */
case class MicroSegment (id: String, from: String, to: String, length: Double,
                         lanes: Int, freeFlowSpeed: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroNetwork` class stores the microscopic network topology.
 *  @param nodes     the network nodes
 *  @param segments  the directed road segments
 */
case class MicroNetwork (nodes: Array [MicroNode], segments: Array [MicroSegment])

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroArrival` class stores an arrival-process specification.
 *  @param model       the arrival-process model
 *  @param parameters  the named model parameters
 */
case class MicroArrival (model: MicroArrivalModel, parameters: Array [MicroParameter])

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroSource` class stores one demand source.
 *  @param id       the stable source identifier
 *  @param node     the source node identifier
 *  @param arrival  the arrival-process specification
 */
case class MicroSource (id: String, node: String, arrival: MicroArrival)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `UnsupportedMicroIRVersion` exception indicates an unsupported schema.
 *  @param version  the unsupported schema version
 */
class UnsupportedMicroIRVersion (version: String)
      extends IllegalArgumentException (s"unsupported MicroIR version '$version'")

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroRoute` class stores one ordered microscopic route.
 *  @param id           the stable route identifier
 *  @param source       the source node identifier
 *  @param destination  the destination node identifier
 *  @param segments     the ordered segment identifiers
 *  @param probability  the route-choice probability
 */
case class MicroRoute (id: String, source: String, destination: String,
                       segments: Array [String], probability: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroDemand` class stores microscopic sources and routes.
 *  @param sources  the demand sources
 *  @param routes   the available routes
 */
case class MicroDemand (sources: Array [MicroSource], routes: Array [MicroRoute])

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroVehicleDynamics` class stores the microscopic motion model.
 *  @param model           the vehicle-dynamics model
 *  @param odeSolver       the numerical integrator
 *  @param parameters      the named dynamics parameters
 *  @param updateInterval  the microscopic update interval in seconds
 */
case class MicroVehicleDynamics (model: CarFollowingModel,
                                 odeSolver: ODESolverType,
                                 parameters: Array [MicroParameter],
                                 updateInterval: Double)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroObservable` class identifies one retained microscopic measure.
 *  @param measure   the measure to retain
 *  @param location  the node or segment identifier where it is observed
 */
case class MicroObservable (measure: MicroMeasure, location: String)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroIR` class is the typed v0.1 intermediate representation of a
 *  microscopic traffic model.
 *  Runtime `Graph`, `Component`, `Route`, `Variate` and `Statistic` classes
 *  retain execution ownership.  The records above are detached semantic
 *  snapshots used only for deterministic interchange and abstraction.
 *  @param version          the MicroIR schema version
 *  @param network          the microscopic network
 *  @param demand           the microscopic demand
 *  @param vehicleDynamics  the microscopic vehicle-dynamics model
 *  @param observables      the measures retained for abstraction
 */
case class MicroIR (version: String, network: MicroNetwork, demand: MicroDemand,
                    vehicleDynamics: MicroVehicleDynamics,
                    observables: Array [MicroObservable]):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return validation errors in deterministic order.
     */
    def validationErrors: Array [String] = MicroIR.validationErrors (this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate this MicroIR model, throwing one error containing all flaws.
     */
    def validate (): Unit =
        val errors = validationErrors
        if errors.nonEmpty then
            throw new IllegalArgumentException (errors.mkString ("invalid MicroIR: ", "; ", ""))
    end validate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return this MicroIR model as canonical RFC 8785 JSON.
     */
    def json: String = MicroIRJson (this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return this MicroIR model as indented JSON for display.
     */
    def prettyJson: String = MicroIRJson.pretty (this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the SHA-256 identity of this canonical MicroIR model.
     */
    def sha256: String = MicroIRJson.sha256 (this)

end MicroIR


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroIR` companion object contains the current schema version.
 */
object MicroIR:
    val version = "0.1"                                         // current schema version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return validation errors for the given MicroIR model.
     *  @param ir  the microscopic intermediate representation
     */
    private [ir] def validationErrors (ir: MicroIR): Array [String] =
        val errors     = VEC [String] ()                         // accumulated flaws
        val nodeIds    = ir.network.nodes.map (_.id)             // declared node ids
        val segmentIds = ir.network.segments.map (_.id)          // declared segment ids
        val sourceIds  = ir.demand.sources.map (_.id)            // declared source ids
        val routeIds   = ir.demand.routes.map (_.id)             // declared route ids
        val nodeSet    = nodeIds.toSet                           // node lookup
        val segmentSet = segmentIds.toSet                        // segment lookup
        val routeSet   = routeIds.toSet                          // route lookup
        val segmentMap = ir.network.segments.map (s => s.id -> s).toMap

        if ir.version != version then errors += s"unsupported version '${ir.version}'"
        addEmptyIds (errors, "node", nodeIds)
        addEmptyIds (errors, "segment", segmentIds)
        addEmptyIds (errors, "source", sourceIds)
        addEmptyIds (errors, "route", routeIds)
        addDuplicates (errors, "node", nodeIds)
        addDuplicates (errors, "segment", segmentIds)
        addDuplicates (errors, "source", sourceIds)
        addDuplicates (errors, "route", routeIds)

        for segment <- ir.network.segments.sortBy (_.id) do
            if !nodeSet.contains (segment.from) then
                errors += s"segment '${segment.id}' references missing from-node '${segment.from}'"
            if !nodeSet.contains (segment.to) then
                errors += s"segment '${segment.id}' references missing to-node '${segment.to}'"
            if !segment.length.isFinite || segment.length <= 0.0 then
                errors += s"segment '${segment.id}' has nonpositive length ${segment.length}"
            if segment.lanes <= 0 then
                errors += s"segment '${segment.id}' has nonpositive lanes ${segment.lanes}"
            if !segment.freeFlowSpeed.isFinite || segment.freeFlowSpeed <= 0.0 then
                errors += s"segment '${segment.id}' has nonpositive freeFlowSpeed ${segment.freeFlowSpeed}"
        end for

        for source <- ir.demand.sources.sortBy (_.id) do
            if !nodeSet.contains (source.node) then
                errors += s"source '${source.id}' references missing node '${source.node}'"
            validateArrival (errors, source)
        end for

        for route <- ir.demand.routes.sortBy (_.id) do
            if !nodeSet.contains (route.source) then
                errors += s"route '${route.id}' references missing source node '${route.source}'"
            if !nodeSet.contains (route.destination) then
                errors += s"route '${route.id}' references missing destination node '${route.destination}'"
            if !route.probability.isFinite || route.probability < 0.0 || route.probability > 1.0 then
                errors += s"route '${route.id}' has probability ${route.probability} outside [0,1]"
            validateRoute (errors, route, segmentMap)
        end for

        val dynamics = ir.vehicleDynamics
        if !dynamics.updateInterval.isFinite || dynamics.updateInterval <= 0.0 then
            errors += s"vehicleDynamics has nonpositive updateInterval ${dynamics.updateInterval}"
        addParameterErrors (errors, "vehicleDynamics", dynamics.parameters)

        for observable <- ir.observables.sortBy (x => (x.measure.toString, x.location)) do
            if !nodeSet.contains (observable.location) &&
               !segmentSet.contains (observable.location) &&
               !routeSet.contains (observable.location)
            then errors += s"observable '${observable.measure}' references missing location '${observable.location}'"
        end for

        errors.sorted.toArray
    end validationErrors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add empty identifier errors for one entity kind.
     */
    private def addEmptyIds (errors: VEC [String], kind: String, ids: Array [String]): Unit =
        if ids.exists (_.isBlank) then errors += s"$kind identifier is empty"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add duplicate identifier errors for one entity kind.
     */
    private def addDuplicates (errors: VEC [String], kind: String, ids: Array [String]): Unit =
        val duplicates = ids.groupBy (identity).collect { case (id, values) if values.length > 1 => id }.toArray.sorted
        for id <- duplicates do errors += s"duplicate $kind identifier '$id'"
    end addDuplicates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add parameter-name and finite-value errors for one parameter owner.
     */
    private def addParameterErrors (errors: VEC [String], owner: String,
                                    parameters: Array [MicroParameter]): Unit =
        val names = parameters.map (_.name)
        addEmptyIds (errors, s"$owner parameter", names)
        addDuplicates (errors, s"$owner parameter", names)
        for parameter <- parameters.sortBy (_.name) do
            if !parameter.value.isFinite then
                errors += s"$owner parameter '${parameter.name}' is not finite"
        end for
    end addParameterErrors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate the model-specific parameters for one arrival source.
     */
    private def validateArrival (errors: VEC [String], source: MicroSource): Unit =
        val owner      = s"source '${source.id}' arrival"        // error-message owner
        val arrival    = source.arrival                          // arrival specification
        val parameters = arrival.parameters                     // model parameters
        val names      = parameters.map (_.name).sorted          // supplied parameter names
        val expected = arrival.model match                      // required native parameters
            case MicroArrivalModel.Deterministic => Array ("x")
            case MicroArrivalModel.Exponential   => Array ("mu")
            case MicroArrivalModel.Erlang        => Array ("k", "mu")
            case MicroArrivalModel.Erlang2S      => Array ("mu", "tau")

        addParameterErrors (errors, owner, parameters)
        if !names.sameElements (expected) then
            errors += s"$owner ${arrival.model} requires parameters [${expected.mkString (",")}]"
            return
        end if

        def value (name: String): Double = parameters.find (_.name == name).get.value
        def positive (name: String): Unit =
            if value (name) <= 0.0 then errors += s"$owner parameter '$name' must be positive"
        end positive

        arrival.model match
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
    end validateArrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Validate the ordered connectivity of one route.
     */
    private def validateRoute (errors: VEC [String], route: MicroRoute,
                               segmentMap: Map [String, MicroSegment]): Unit =
        if route.segments.isEmpty then
            errors += s"route '${route.id}' has no segments"
            return
        end if

        val path = Array.ofDim [MicroSegment] (route.segments.length)
        var complete = true
        cfor (0, route.segments.length) { i =>
            segmentMap.get (route.segments(i)) match
                case Some (segment) => path(i) = segment
                case None =>
                    complete = false
                    errors += s"route '${route.id}' references missing segment '${route.segments(i)}'"
        }
        if !complete then return

        if path(0).from != route.source then
            errors += s"route '${route.id}' does not begin at '${route.source}'"
        cfor (1, path.length) { i =>
            if path(i - 1).to != path(i).from then
                errors += s"route '${route.id}' is disconnected between '${path(i - 1).id}' and '${path(i).id}'"
        }
        if path.last.to != route.destination then
            errors += s"route '${route.id}' does not end at '${route.destination}'"
    end validateRoute

end MicroIR


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroIRJson` object explicitly encodes MicroIR as canonical JSON.
 */
object MicroIRJson:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode and canonicalize the given MicroIR model.
     *  @param ir  the microscopic intermediate representation
     */
    def apply (ir: MicroIR): String =
        ir.validate ()
        CanonicalJson (raw (ir))
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode the given MicroIR model as indented JSON for display.
     *  Canonical JSON remains the source for hashing and interchange.
     *  @param ir  the microscopic intermediate representation
     */
    def pretty (ir: MicroIR): String =
        CanonicalJson.pretty (apply (ir))
    end pretty

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse and validate one strict MicroIR JSON document.
     *  @param json  the MicroIR JSON document
     */
    def parse (json: String): MicroIR =
        val root = asObject (JsonParser.parseString (json), "MicroIR")
        checkFields (root, "MicroIR", "version", "network", "demand",
                     "vehicleDynamics", "observables")

        val version = stringField (root, "version", "MicroIR")
        if version != MicroIR.version then throw new UnsupportedMicroIRVersion (version)

        val ir = MicroIR (
            version,
            parseNetwork (objectField (root, "network", "MicroIR")),
            parseDemand (objectField (root, "demand", "MicroIR")),
            parseDynamics (objectField (root, "vehicleDynamics", "MicroIR")),
            parseObservables (arrayField (root, "observables", "MicroIR")))
        ir.validate ()
        ir
    end parse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the SHA-256 identity of the given MicroIR model.
     *  @param ir  the microscopic intermediate representation
     */
    private [ir] def sha256 (ir: MicroIR): String =
        CanonicalJson.sha256 (apply (ir))
    end sha256

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode the given MicroIR model as a JSON document.
     *  Stable identifiers order entity collections; route segments retain
     *  their semantic traversal order.
     *  @param ir  the microscopic intermediate representation
     */
    private def raw (ir: MicroIR): String =
        val root = new JsonObject ()                             // MicroIR document root
        root.addProperty ("version", ir.version)
        root.add ("network", encodeNetwork (ir.network))
        root.add ("demand", encodeDemand (ir.demand))
        root.add ("vehicleDynamics", encodeDynamics (ir.vehicleDynamics))
        root.add ("observables", encodeObservables (ir.observables))
        root.toString
    end raw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode the microscopic network in stable identifier order.
     */
    private def encodeNetwork (network: MicroNetwork): JsonObject =
        val json     = new JsonObject ()                         // encoded network
        val nodes    = new JsonArray ()                          // encoded nodes
        val segments = new JsonArray ()                          // encoded segments

        for node <- network.nodes.sortBy (_.id) do
            val value = new JsonObject ()
            value.addProperty ("id", node.id)
            value.addProperty ("kind", nodeKind (node.kind))
            nodes.add (value)
        end for

        for segment <- network.segments.sortBy (_.id) do
            val value = new JsonObject ()
            value.addProperty ("id", segment.id)
            value.addProperty ("from", segment.from)
            value.addProperty ("to", segment.to)
            value.addProperty ("length", segment.length)
            value.addProperty ("lanes", segment.lanes)
            value.addProperty ("freeFlowSpeed", segment.freeFlowSpeed)
            segments.add (value)
        end for

        json.add ("nodes", nodes)
        json.add ("segments", segments)
        json
    end encodeNetwork

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode microscopic demand in stable identifier order.
     */
    private def encodeDemand (demand: MicroDemand): JsonObject =
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
            val value    = new JsonObject ()
            val segments = new JsonArray ()
            for segment <- route.segments do segments.add (segment)
            value.addProperty ("id", route.id)
            value.addProperty ("source", route.source)
            value.addProperty ("destination", route.destination)
            value.add ("segments", segments)
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
        val json = new JsonObject ()                             // encoded arrival model
        json.addProperty ("model", arrivalModel (arrival.model))
        json.add ("parameters", encodeParameters (arrival.parameters))
        json
    end encodeArrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode the microscopic vehicle-dynamics specification.
     */
    private def encodeDynamics (dynamics: MicroVehicleDynamics): JsonObject =
        val json = new JsonObject ()                             // encoded dynamics model
        json.addProperty ("model", carFollowingModel (dynamics.model))
        json.addProperty ("odeSolver", odeSolver (dynamics.odeSolver))
        json.add ("parameters", encodeParameters (dynamics.parameters))
        json.addProperty ("updateInterval", dynamics.updateInterval)
        json
    end encodeDynamics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode named parameters in stable name order.
     */
    private def encodeParameters (parameters: Array [MicroParameter]): JsonArray =
        val json = new JsonArray ()                              // encoded parameters
        for parameter <- parameters.sortBy (_.name) do
            val value = new JsonObject ()
            value.addProperty ("name", parameter.name)
            value.addProperty ("value", parameter.value)
            json.add (value)
        end for
        json
    end encodeParameters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Encode observables in stable measure/location order.
     */
    private def encodeObservables (observables: Array [MicroObservable]): JsonArray =
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
    /** Parse a microscopic network.
     */
    private def parseNetwork (json: JsonObject): MicroNetwork =
        checkFields (json, "network", "nodes", "segments")
        MicroNetwork (parseNodes (arrayField (json, "nodes", "network")),
                      parseSegments (arrayField (json, "segments", "network")))
    end parseNetwork

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse microscopic network nodes.
     */
    private def parseNodes (json: JsonArray): Array [MicroNode] =
        val nodes = Array.ofDim [MicroNode] (json.size)          // parsed nodes
        cfor (0, json.size) { i =>
            val path  = s"network.nodes[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, "id", "kind")
            nodes(i) = MicroNode (stringField (value, "id", path),
                                  parseNodeKind (stringField (value, "kind", path)))
        }
        nodes
    end parseNodes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse microscopic road segments.
     */
    private def parseSegments (json: JsonArray): Array [MicroSegment] =
        val segments = Array.ofDim [MicroSegment] (json.size)    // parsed segments
        cfor (0, json.size) { i =>
            val path  = s"network.segments[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, "id", "from", "to", "length", "lanes",
                         "freeFlowSpeed")
            segments(i) = MicroSegment (
                stringField (value, "id", path),
                stringField (value, "from", path),
                stringField (value, "to", path),
                doubleField (value, "length", path),
                intField (value, "lanes", path),
                doubleField (value, "freeFlowSpeed", path))
        }
        segments
    end parseSegments

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse microscopic demand.
     */
    private def parseDemand (json: JsonObject): MicroDemand =
        checkFields (json, "demand", "sources", "routes")
        MicroDemand (parseSources (arrayField (json, "sources", "demand")),
                     parseRoutes (arrayField (json, "routes", "demand")))
    end parseDemand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse microscopic demand sources.
     */
    private def parseSources (json: JsonArray): Array [MicroSource] =
        val sources = Array.ofDim [MicroSource] (json.size)      // parsed sources
        cfor (0, json.size) { i =>
            val path  = s"demand.sources[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, "id", "node", "arrival")
            sources(i) = MicroSource (
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
        checkFields (json, path, "model", "parameters")
        MicroArrival (parseArrivalModel (stringField (json, "model", path)),
                      parseParameters (arrayField (json, "parameters", path),
                                       s"$path.parameters"))
    end parseArrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse microscopic routes while preserving traversal order.
     */
    private def parseRoutes (json: JsonArray): Array [MicroRoute] =
        val routes = Array.ofDim [MicroRoute] (json.size)        // parsed routes
        cfor (0, json.size) { i =>
            val path  = s"demand.routes[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, "id", "source", "destination", "segments",
                         "probability")
            routes(i) = MicroRoute (
                stringField (value, "id", path),
                stringField (value, "source", path),
                stringField (value, "destination", path),
                parseStrings (arrayField (value, "segments", path), s"$path.segments"),
                doubleField (value, "probability", path))
        }
        routes
    end parseRoutes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse the microscopic vehicle-dynamics specification.
     */
    private def parseDynamics (json: JsonObject): MicroVehicleDynamics =
        val path = "vehicleDynamics"
        checkFields (json, path, "model", "odeSolver", "parameters", "updateInterval")
        MicroVehicleDynamics (
            parseCarFollowingModel (stringField (json, "model", path)),
            parseOdeSolver (stringField (json, "odeSolver", path)),
            parseParameters (arrayField (json, "parameters", path), s"$path.parameters"),
            doubleField (json, "updateInterval", path))
    end parseDynamics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse named numeric parameters.
     */
    private def parseParameters (json: JsonArray, path: String): Array [MicroParameter] =
        val parameters = Array.ofDim [MicroParameter] (json.size) // parsed parameters
        cfor (0, json.size) { i =>
            val entry = s"$path[$i]"
            val value = asObject (json.get (i), entry)
            checkFields (value, entry, "name", "value")
            parameters(i) = MicroParameter (stringField (value, "name", entry),
                                            doubleField (value, "value", entry))
        }
        parameters
    end parseParameters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse observable specifications.
     */
    private def parseObservables (json: JsonArray): Array [MicroObservable] =
        val observables = Array.ofDim [MicroObservable] (json.size) // parsed observables
        cfor (0, json.size) { i =>
            val path  = s"observables[$i]"
            val value = asObject (json.get (i), path)
            checkFields (value, path, "measure", "location")
            observables(i) = MicroObservable (
                parseMeasure (stringField (value, "measure", path)),
                stringField (value, "location", path))
        }
        observables
    end parseObservables

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
        if !value.isJsonArray then
            throw new IllegalArgumentException (s"$path.$name must be an array")
        value.getAsJsonArray
    end arrayField

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a required string field.
     */
    private def stringField (json: JsonObject, name: String, path: String): String =
        asString (field (json, name, path), s"$path.$name")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a required finite numeric field.
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
    /** Return the canonical label for the given node kind.
     */
    private def nodeKind (kind: MicroNodeKind): String = kind match
        case MicroNodeKind.Source   => "source"
        case MicroNodeKind.Junction => "junction"
        case MicroNodeKind.Sink     => "sink"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the canonical label for the given arrival model.
     */
    private def arrivalModel (model: MicroArrivalModel): String = model match
        case MicroArrivalModel.Deterministic => "deterministic"
        case MicroArrivalModel.Exponential   => "exponential"
        case MicroArrivalModel.Erlang        => "erlang"
        case MicroArrivalModel.Erlang2S      => "erlang2S"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the canonical label for the given car-following model.
     */
    private def carFollowingModel (model: CarFollowingModel): String = model match
        case CarFollowingModel.IDM    => "idm"
        case CarFollowingModel.Gipps  => "gipps"
        case CarFollowingModel.Krause => "krause"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the canonical label for the given ODE solver.
     */
    private def odeSolver (solver: ODESolverType): String = solver match
        case ODESolverType.Ballistic => "ballistic"
        case ODESolverType.Euler     => "euler"
        case ODESolverType.Heun      => "heun"
        case ODESolverType.RK2       => "rk2"
        case ODESolverType.RK3       => "rk3"
        case ODESolverType.RK4       => "rk4"
        case ODESolverType.DOPRI5    => "dopri5"
        case ODESolverType.Butcher   => "butcher"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the canonical label for the given observable measure.
     */
    private def measure (observable: MicroMeasure): String = observable match
        case MicroMeasure.Flow       => "flow"
        case MicroMeasure.Speed      => "speed"
        case MicroMeasure.Density    => "density"
        case MicroMeasure.TravelTime => "travelTime"

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
    /** Parse the canonical label for a car-following model.
     */
    private def parseCarFollowingModel (model: String): CarFollowingModel = model match
        case "idm"    => CarFollowingModel.IDM
        case "gipps"  => CarFollowingModel.Gipps
        case "krause" => CarFollowingModel.Krause
        case _ => throw new IllegalArgumentException (s"unknown car-following model '$model'")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse the canonical label for an ODE solver.
     */
    private def parseOdeSolver (solver: String): ODESolverType = solver match
        case "ballistic" => ODESolverType.Ballistic
        case "euler"     => ODESolverType.Euler
        case "heun"      => ODESolverType.Heun
        case "rk2"       => ODESolverType.RK2
        case "rk3"       => ODESolverType.RK3
        case "rk4"       => ODESolverType.RK4
        case "dopri5"    => ODESolverType.DOPRI5
        case "butcher"   => ODESolverType.Butcher
        case _ => throw new IllegalArgumentException (s"unknown ODE solver '$solver'")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse the canonical label for an observable measure.
     */
    private def parseMeasure (observable: String): MicroMeasure = observable match
        case "flow"       => MicroMeasure.Flow
        case "speed"      => MicroMeasure.Speed
        case "density"    => MicroMeasure.Density
        case "travelTime" => MicroMeasure.TravelTime
        case _ => throw new IllegalArgumentException (s"unknown observable measure '$observable'")

end MicroIRJson


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runMicroIR` main function tests canonical MicroIR serialization.
 *  > runMain scalation.simulation.process.ir.runMicroIR
 */
@main def runMicroIR (): Unit =

    banner ("MicroIR test")

    def rejects (test: => Unit): Boolean =
        try
            test
            false
        catch case _: IllegalArgumentException => true
    end rejects

    val n0 = MicroNode ("n0", MicroNodeKind.Source)             // source node
    val n1 = MicroNode ("n1", MicroNodeKind.Junction)           // middle node
    val n2 = MicroNode ("n2", MicroNodeKind.Sink)               // sink node
    val s0 = MicroSegment ("s0", "n0", "n1", 250.0, 1, 30.0) // first segment
    val s1 = MicroSegment ("s1", "n1", "n2", 300.0, 1, 25.0) // second segment

    val arrival = MicroArrival (MicroArrivalModel.Deterministic,
                                Array (MicroParameter ("x", 2.0)))
    val source  = MicroSource ("source-0", "n0", arrival)
    val route   = MicroRoute ("route-0", "n0", "n2", Array ("s0", "s1"), 1.0)
    val motion  = MicroVehicleDynamics (
        CarFollowingModel.IDM,
        ODESolverType.Ballistic,
        Array (MicroParameter ("timeHeadway", 1.5),
               MicroParameter ("minimumGap", 2.0)),
        0.5)
    val measures = Array (MicroObservable (MicroMeasure.TravelTime, "route-0"),
                          MicroObservable (MicroMeasure.Flow, "s1"))

    val ir1 = MicroIR (MicroIR.version,
                       MicroNetwork (Array (n0, n1, n2), Array (s0, s1)),
                       MicroDemand (Array (source), Array (route)), motion, measures)

    val reversedMotion = motion.copy (parameters = motion.parameters.reverse)
    val ir2 = MicroIR (MicroIR.version,
                       MicroNetwork (Array (n2, n1, n0), Array (s1, s0)),
                       MicroDemand (Array (source), Array (route)),
                       reversedMotion, measures.reverse)

    require (ir1.json == ir2.json)                              // entity order is canonical
    require (ir1.sha256 == ir2.sha256)                          // identity is canonical

    val parsed = MicroIRJson.parse (ir1.json)                   // strict round trip
    require (parsed.json == ir1.json)                           // arrays use canonical equality
    require (parsed.sha256 == ir1.sha256)

    val arrivals = Array (                                     // supported arrival families
        arrival,
        MicroArrival (MicroArrivalModel.Exponential,
                      Array (MicroParameter ("mu", 2.0))),
        MicroArrival (MicroArrivalModel.Erlang,
                      Array (MicroParameter ("mu", 1.0), MicroParameter ("k", 2.0))),
        MicroArrival (MicroArrivalModel.Erlang2S,
                      Array (MicroParameter ("mu", 1.0), MicroParameter ("tau", 0.2))))
    cfor (0, arrivals.length) { i =>
        val sourceI = source.copy (arrival = arrivals(i))       // source using family i
        val irI = ir1.copy (demand = MicroDemand (Array (sourceI), Array (route)))
        irI.validate ()
        require (MicroIRJson.parse (irI.json).json == irI.json) // family round trip
    }

    val reversedRoute = route.copy (segments = route.segments.reverse)
    val ir3 = ir1.copy (demand = MicroDemand (Array (source), Array (reversedRoute)))
    require (rejects { ir3.validate () })                       // reversed route is invalid

    val changedSegment = s1.copy (length = 301.0)
    val ir4 = ir1.copy (network = MicroNetwork (Array (n0, n1, n2),
                                                    Array (s0, changedSegment)))
    require (ir1.sha256 != ir4.sha256)                          // semantics change identity

    val ir5 = ir1.copy (vehicleDynamics = motion.copy (odeSolver = ODESolverType.Euler))
    require (ir1.sha256 != ir5.sha256)                          // solver is model semantics

    val badVersion = ir1.json.replace ("\"0.1\"", "\"0.2\"")
    require (rejects { MicroIRJson.parse (badVersion) })         // version is enforced

    val unknownField = ir1.json.dropRight (1) + ",\"unknown\":true}"
    require (rejects { MicroIRJson.parse (unknownField) })       // schema is closed

    val invalidSegment = s1.copy (lanes = 0)
    val invalid = ir1.copy (network = MicroNetwork (Array (n0, n1, n2),
                                                     Array (s0, invalidSegment)))
    require (invalid.validationErrors.nonEmpty)                 // validation finds the flaw
    require (rejects { invalid.validate () })                   // invalid ADT is rejected

    println (s"microir_sha256 = ${ir1.sha256}")
    banner ("MicroIR test: PASS")

end runMicroIR
