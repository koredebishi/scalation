//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Sun Sep 13 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Extract MicroIR from Native Microscopic Traffic Components
 */

package scalation
package simulation
package process
package ir

import scalation.random.{Erlang, Erlang2S, Exponential, Sharp, Variate}
import scalation.simulation.process.config.{CarFollowingModel, ODESolverType}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MicroIRExtractor` object extracts the semantic state of one native
 *  microscopic `Pathway`.  Execution state remains owned by Scalation.
 */
object MicroIRExtractor:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract MicroIR from the given one-lane pathway.
     *  @param pathway  the native microscopic pathway
     *  @param arrival  the source inter-arrival distribution
     *  @param motion   the pathway's car-following dynamics
     */
    def apply (pathway: Pathway, arrival: Variate, motion: Dynamics): MicroIR =
        val routeId     = s"${pathway.name}-route"              // stable route identifier
        val nodes       = extractNodes (pathway)                 // source, junctions and sink
        val segments    = extractSegments (pathway)              // directed pathway segments
        val source      = MicroSource (pathway.from.name, pathway.from.name,
                                       extractArrival (arrival))
        val route       = MicroRoute (routeId, pathway.from.name, pathway.to.name,
                                      segments.map (_.id), 1.0)
        val dynamics    = extractDynamics (motion)               // microscopic motion semantics
        val observables = extractObservables (segments, routeId) // measures already reported

        val ir = MicroIR (MicroIR.version,
                          MicroNetwork (nodes, segments),
                          MicroDemand (Array (source), Array (route)),
                          dynamics, observables)
        ir.validate ()
        ir
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract the ordered pathway points as semantic network nodes.
     */
    private def extractNodes (pathway: Pathway): Array [MicroNode] =
        val nodes = Array.ofDim [MicroNode] (pathway.points.length) // extracted nodes
        cfor (0, pathway.points.length) { i =>
            val point = pathway.points(i)                         // native pathway point
            val kind = point match                               // native semantic role
                case _: Source   => MicroNodeKind.Source
                case _: Junction => MicroNodeKind.Junction
                case _: Sink     => MicroNodeKind.Sink
                case _ => throw new IllegalArgumentException (
                    s"unsupported pathway point '${point.name}' of type ${point.getClass.getName}")
            nodes(i) = MicroNode (point.name, kind)
        }
        nodes
    end extractNodes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract the native `VTransport` segments of the pathway.
     */
    private def extractSegments (pathway: Pathway): Array [MicroSegment] =
        val segments = Array.ofDim [MicroSegment] (pathway.seg.length) // extracted segments
        cfor (0, pathway.seg.length) { i =>
            val segment = pathway.seg(i)                            // native lane segment
            segments(i) = MicroSegment (segment.name, segment.from.name, segment.to.name,
                                        segment.length, 1, Vehicle.vmax)
        }
        segments
    end extractSegments

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract one supported native inter-arrival distribution.
     */
    private def extractArrival (arrival: Variate): MicroArrival = arrival match
        case rv: Sharp =>
            MicroArrival (MicroArrivalModel.Deterministic,
                          Array (MicroParameter ("x", rv.x)))
        case rv: Exponential =>
            MicroArrival (MicroArrivalModel.Exponential,
                          Array (MicroParameter ("mu", rv.mu)))
        case rv: Erlang =>
            MicroArrival (MicroArrivalModel.Erlang,
                          Array (MicroParameter ("mu", rv.mu),
                                 MicroParameter ("k", rv.k.toDouble)))
        case rv: Erlang2S =>
            MicroArrival (MicroArrivalModel.Erlang2S,
                          Array (MicroParameter ("mu", rv.mu),
                                 MicroParameter ("tau", rv.tau)))
        case _ => throw new IllegalArgumentException (
            s"unsupported arrival distribution ${arrival.getClass.getName}")
    end extractArrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract the current native IDM configuration.
     */
    private def extractDynamics (motion: Dynamics): MicroVehicleDynamics = motion match
        case IDMDynamics =>
            val parameters = Array (
                MicroParameter ("initialSpeed", Vehicle.v0),
                MicroParameter ("maximumSpeed", Vehicle.vmax),
                MicroParameter ("maximumAcceleration", Vehicle.amax),
                MicroParameter ("comfortableDeceleration", math.abs (Vehicle.bmax)),
                MicroParameter ("timeHeadway", Vehicle.T),
                MicroParameter ("minimumGap", Vehicle.s),
                MicroParameter ("vehicleLength", Vehicle.len),
                MicroParameter ("delta", Vehicle.del))
            MicroVehicleDynamics (CarFollowingModel.IDM,
                                  odeSolver (IDMDynamics.integratorType),
                                  parameters, Vehicle.rt)
        case _ => throw new IllegalArgumentException (
            s"unsupported dynamics ${motion.getClass.getName}")
    end extractDynamics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the native dynamics integrator to its configuration enumeration.
     */
    private def odeSolver (integrator: IntegratorType): ODESolverType = integrator match
        case IntegratorType.Ballistic => ODESolverType.Ballistic
        case IntegratorType.Euler     => ODESolverType.Euler
        case IntegratorType.Heun      => ODESolverType.Heun
        case IntegratorType.RK2       => ODESolverType.RK2
        case IntegratorType.RK3       => ODESolverType.RK3
        case IntegratorType.RK4       => ODESolverType.RK4
        case IntegratorType.DOPRI5    => ODESolverType.DOPRI5
        case IntegratorType.butcher   => ODESolverType.Butcher

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract the measures reported by the one-way microscopic pathway.
     */
    private def extractObservables (segments: Array [MicroSegment],
                                    routeId: String): Array [MicroObservable] =
        val observables = Array.ofDim [MicroObservable] (3 * segments.length + 1)
        cfor (0, segments.length) { i =>
            val j = 3 * i                                      // first measure for segment i
            observables(j)     = MicroObservable (MicroMeasure.Flow, segments(i).id)
            observables(j + 1) = MicroObservable (MicroMeasure.Speed, segments(i).id)
            observables(j + 2) = MicroObservable (MicroMeasure.Density, segments(i).id)
        }
        observables(3 * segments.length) = MicroObservable (MicroMeasure.TravelTime, routeId)
        observables
    end extractObservables

end MicroIRExtractor
