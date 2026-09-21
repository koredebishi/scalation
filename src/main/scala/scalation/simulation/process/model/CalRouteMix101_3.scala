//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Mixed-resolution replica of the CalRoute101_3 US-101 model. */

package scalation
package simulation
package process
package meso
package model

import scalation.dynamics.IdmFlux
import scalation.mathstat.{MatrixD, VectorD}
import scalation.simulation.process.builder.TrafficModelBuilder
import scalation.simulation.process.config.{CarFollowingModel, NetworkConfig, SimulationReport}
import scalation.simulation.process.{GippsDynamics, IDMDynamics, IntegratorType, KraussDynamics}

@main def runCalRouteMix101_3 (): Unit = new CalRouteMix101_3 ()

/** CalRoute101_3 with microscopic chains at the two on-ramp merges.
 *
 *  Meso: 0, 3, 4.  Micro: 1--2 and 5--6.
 *  All demand and observations remain supplied by the CalRoute PeMS builder.
 */
class CalRouteMix101_3 (name: String = "CalRouteMix101_3", reps: Int = 1,
                        animating: Boolean = true, aniRatio: Double = 500.0,
                        autoRun: Boolean = true, traceSpeeds: Boolean = false)
      extends Model (name, reps, animating, aniRatio)
         with RowTimeLoader:

    private val debug = debugf ("CalRouteMix101_3", false)
    private val builder = TrafficModelBuilder.US101_DonaldDoyle_PeMS ()
    builder.summary ()

    private val network  = builder.getNetwork
    private val demand   = builder.getDemand
    private val dynamics = builder.getDynamics
    private val numLanes = builder.numLanes
    private val numSegments = builder.numSegments
    private val rampJoinSeg = builder.rampJoinSegments
    private val pemsToJunc = builder.pemsSensorIndices
    private val nt = 48
    private val mainlineArrivalSources = builder.mainlineArrivalSources
    private val rampArrivalSources = builder.rampArrivalSources

    private val freeFlowSpeed = dynamics.vmax
    private val linkLength = 500.0
    private val mesoCapacity = 1800.0
    private val storageCapacity = 10

    require (numSegments == 7, s"CalRoute mixed model expects 7 segments, got $numSegments")
    require (rampJoinSeg.sameElements (Array (2, 5)),
             s"CalRoute mixed model expects ramp joins [2,5], got ${rampJoinSeg.mkString ("[", ",", "]")}")

    private val motion = dynamics.carFollowing match
        case CarFollowingModel.IDM    => IDMDynamics
        case CarFollowingModel.Gipps  => GippsDynamics
        case CarFollowingModel.Krause => KraussDynamics

    motion match
        case IDMDynamics    => IDMDynamics.integratorType = IntegratorType.Ballistic
        case GippsDynamics  => GippsDynamics.integratorType = IntegratorType.Ballistic
        case KraussDynamics => KraussDynamics.integratorType = IntegratorType.Ballistic
        case _              =>

    Vehicle.setInitialSpeed (freeFlowSpeed)
    private val flowDiagram = new IdmFlux (v0 = freeFlowSpeed,
        tau = Vehicle.T, s0 = Vehicle.s, len = Vehicle.len, delta = Vehicle.del)

    private val (w, h) = (3000, 2000)
    private val mainCoords = NetworkConfig.getMainlineCoordinates ((w, h))

    // Mainline boundaries are shared by mesoscopic links and microscopic pathways.
    private val junc = Array.tabulate (numSegments + 1) { i =>
        val jName = i match
            case 0 => "warm_up"
            case 1 => "sensor1"
            case 2 => "onR_merge1"
            case 3 => "sensor2"
            case 4 => "sensor3"
            case 5 => "onR_merge2"
            case 6 => "sensor4"
            case 7 => "sensor5"
            case _ => s"junc_$i"
        new Junction (jName, xy = mainCoords(i), nt = nt, nl = numLanes)
    }
    private val sink = Sink ("sinkMain", (mainCoords.last._1.toInt, mainCoords.last._2.toInt))

    private def laneShift (lane: Int): VectorD =
        val physicalLane = numLanes - 1 - lane
        VectorD (0.0, (physicalLane - (numLanes - 1) / 2.0) * 60.0)
    end laneShift

    private def makeMeso (prefix: String, from: Component, to: Component): Array [MesoVTransport] =
        Array.tabulate (numLanes) { lane =>
            val shift = laneShift (lane)
            val link = MesoVTransport (s"$prefix-$lane", from, to, freeFlowSpeed, mesoCapacity,
                                       shift1 = shift, shift2 = shift,
                                       storageCapacity = storageCapacity,
                                       flowDiagram = Some (flowDiagram), traceSpeed = traceSpeeds)
            link.length = linkLength
            link
        }
    end makeMeso

    private val meso0 = makeMeso ("meso0", junc(0), junc(1))
    private val meso3 = makeMeso ("meso3", junc(3), junc(4))
    private val meso4 = makeMeso ("meso4", junc(4), junc(5))

    // One DLL per lane spans each two-segment microscopic chain.
    private val micro12 = Route ("micro12", numLanes, Array (junc(2)), junc(1), junc(3),
                                 motion, laneGap = 50.0)
    private val micro56 = Route ("micro56", numLanes, Array (junc(6)), junc(5), junc(7),
                                 motion, laneGap = 50.0)
    for route <- Array (micro12, micro56); pathway <- route.pathway; segment <- pathway.seg do
        segment.length = linkLength

    // Use the shared CalRoute source geometry instead of an ad-hoc ramp offset.
    private val (sourceCenter, sourceOffsets) =
        NetworkConfig.getVSourceCenterAndOffsets ((w.toDouble + 500.0, h.toDouble - 500.0))
    private val rampSources = builder.buildRampSources (this, () => Car (),
                                                        sourceCenter, sourceOffsets)
    private val rampJunctions = Array.tabulate (rampJoinSeg.length) (r => junc(rampJoinSeg(r)))
    private val ramps = builder.buildRamps (motion, rampSources, rampJunctions)

    for (source, r) <- rampArrivalSources.zipWithIndex do
        println (s"CalRoute ramp ${r + 1}: vehicles=${source.getTotalVehicles (0)}, " +
                 s"joinSegment=${rampJoinSeg(r)}")

    private val mainlineSources = Array.tabulate (numLanes) { lane =>
        val source = mainlineArrivalSources(lane)
        new VSource (s"vsrcML_L$lane", this, () => Car (), lane,
                     source.getTotalVehicles (lane), source.getDistribution,
                     Array (mainCoords(0)._1 - 30.0, mainCoords(0)._2 + lane * 40.0,
                         40.0, 40.0), s"M$lane")
    }

    private val components: Array [Component] =
        mainlineSources ++ rampSources ++ junc ++ Array (sink) ++ ramps ++
        meso0 ++ meso3 ++ meso4 ++ micro12.pathway ++ micro56.pathway
    addComponent (components*)

    private def traverse (pathway: Pathway, car: Vehicle, start: Int, end: Int,
                          endpoint: Junction): Unit =
        pathway.joinAt (car, pathway.mergeLeaderAt (start), start)
        for seg <- start until end do
            pathway.seg(seg).move ()
            if seg < pathway.junc.length then pathway.junc(seg).jump ()
        end for
        endpoint.jump ()
        pathway.removeFromAlist (car)
    end traverse

    case class Car () extends Vehicle ("c", this):
        override def act (): Unit =
            laneID = if subtype < numLanes then subtype else numLanes - 1
            if subtype < numLanes then
                junc(0).jump ()
                meso0(laneID).move (); junc(1).jump ()
                traverse (micro12.pathway(laneID), this, 0, 2, junc(3))
                meso3(laneID).move (); junc(4).jump ()
                meso4(laneID).move (); junc(5).jump ()
                traverse (micro56.pathway(laneID), this, 0, 2, junc(7))
                sink.leave ()
            else
                val r = subtype - numLanes
                val ramp = ramps(r)
                ramp.addToAlist (this, ramp.getLast)
                ramp.lane.move ()
                junc(rampJoinSeg(r)).jump ()
                ramp.removeFromAlist (this)
                if r == 0 then
                    traverse (micro12.pathway(laneID), this, 1, 2, junc(3))
                    meso3(laneID).move (); junc(4).jump ()
                    meso4(laneID).move (); junc(5).jump ()
                    traverse (micro56.pathway(laneID), this, 0, 2, junc(7))
                else
                    traverse (micro56.pathway(laneID), this, 0, 2, junc(7))
                end if
                sink.leave ()
            end if
        end act
    end Car

    def getDataDimension: Int = nt
    def getMuForSource (sourceIdx: Int): VectorD =
        RowTimeLoader.getMuForSourceDefault (mainlineArrivalSources, rampArrivalSources,
                                             numLanes, nt, sourceIdx)
    private val anchorSpeed = RowTimeLoader.getSpeedMatrixDefault (demand)
    def getSpeedMatrix (): MatrixD = anchorSpeed
    def getJunctions: Array [Junction] = junc
    def getPemsSensorIndices: Array [Int] = pemsToJunc
    def getDemandConfig = demand
    def getDynamicsConfig = dynamics

    override def fini (rep: Int): Unit =
        val report = SimulationReport.fromJunctions (junc, pemsToJunc, demand)
        super.fini (rep)
    end fini

    if autoRun then
        simulate ()
        waitFinished ()
        Model.shutdown ()
end CalRouteMix101_3
