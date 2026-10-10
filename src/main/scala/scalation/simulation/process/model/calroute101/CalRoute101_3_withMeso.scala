//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Bishi
 *  @version 2.0
 *  @date    Fri Jan 31 2026
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    CalRoute101_3_withMeso: Traffic Model Using Refactored Config Architecture
 */

package scalation
package simulation
package process
package model
package calroute101

import scalation.mathstat.{MatrixD, VectorD}
import scalation.random.{Sharp, Uniform}
import scalation.dynamics.IdmFlux
import scalation.simulation.process.*
import scalation.simulation.process.meso.MesoVTransport
import scalation.simulation.process.config.{CarFollowingModel, DynamicsConfig, NetworkConfig, PeMSDemand}
import scalation.{cfor, debugf}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Run CalRoute101_3_withMeso simulation.
 *  > runMain scalation.simulation.process.model.calroute101.runCalRoute101_3_withMeso
 */
@main def runCalRoute101_3_withMeso (): Unit = new CalRoute101_3_withMeso ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

/** The `CalRoute101_3_withMeso` class is a traffic simulation model using the refactored
 *  config architecture. Uses NetworkConfig, DemandConfig, DynamicsConfig.
 *
 *  @param name       the name of the model
 *  @param reps       the number of replications
 *  @param animating  whether to animate
 *  @param aniRatio   animation speed ratio
 *  @param autoRun    run immediately for standalone use; false lets an evaluation adapter own the run
 */
class CalRoute101_3_withMeso(name: String = "CalRoute101_3_withMeso", reps: Int = 1,
                             animating: Boolean = true, aniRatio: Double = 500.0)
  extends Model (name, reps, animating, aniRatio)
           with RowTimeLoader
           with CalRouteReadings:

      private val debug = debugf ("CalRoute101_3_withMeso", false)

//      /** Wall-clock mark taken before anything is built (see the [wall] line at the end). */
//      private val wallStart = System.nanoTime ()

      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      // Configuration - uses new architecture
      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      private val builder = TrafficModelBuilder.US101_DonaldDoyle_PeMS()
      builder.summary ()

      // Extract from builder
      private val network  = builder.getNetwork                  // NetworkConfig: topology (segments, lanes, ramps, sensors)
      private val demand   = builder.getDemand                   // DemandConfig: PeMS arrivals (anchor file, distribution)
      private val dynamics = builder.getDynamics                 // DynamicsConfig: IDM/Gipps, ODE solver, cfParams

      private val numLanes    = builder.numLanes                 // 4 lanes from NetworkConfig
      private val numSegments = builder.numSegments              // 7 segments from NetworkConfig
      private val rampJoinSeg = builder.rampJoinSegments         // [2, 5] - where ramps merge
      private val pemsToJunc  = builder.pemsSensorIndices        // [1, 3, 4, 6, 7] - junctions with PeMS sensors

      // Time intervals - 48 for 15-min intervals over 12 hours (6am-6pm)
      private val nt = 48

      // Arrival sources from builder
      private val mainlineArrivalSources = builder.mainlineArrivalSources   // 4 sources, one per lane
      private val rampArrivalSources     = builder.rampArrivalSources       // 2 sources, one per ramp

      debug ("init", s"numLanes=$numLanes, numSegments=$numSegments, nt=$nt")
      debug ("init", s"rampJoinSeg=${rampJoinSeg.mkString (",")}, pemsToJunc=${pemsToJunc.mkString (",")}")

      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      // Dynamics - from config
      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      private val motion = dynamics.carFollowing match
          case CarFollowingModel.IDM    => IDMDynamics
          case CarFollowingModel.Gipps  => GippsDynamics
          case CarFollowingModel.Krause => KraussDynamics

      // Set Ballistic integrator for the selected model (ANNSIM 2026 concluded this is optimal)
      motion match
          case IDMDynamics    => IDMDynamics.integratorType = IntegratorType.Ballistic
          case GippsDynamics  => GippsDynamics.integratorType = IntegratorType.Ballistic
          case KraussDynamics => KraussDynamics.integratorType = IntegratorType.Ballistic
          case _              => // do nothing for unknown dynamics

      // Push dynamics model name to animation HUD
      if animating && dgAni != null then
          dgAni.setHudModelName (dynamics.carFollowing.toString)

      private val rand = Uniform (0.0, 1.0)
      private val iArrivalRV      = mainlineArrivalSources(0).getDistribution

      setTime (nt * rowTime)

      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      // Animation coordinates and junction setup
      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      private val (w, h) = (3000, 2000)
      private val aniCoords_Main = NetworkConfig.getMainlineCoordinates ((w, h))
      private val aniCoords_Ramp = NetworkConfig.getRampCoordinates ((w, h))
      private val (centerPos, offsets) = NetworkConfig.getVSourceCenterAndOffsets ((w, h))

      // Junctions - use sensor names from NetworkConfig
      debug ("init", s"Creating ${aniCoords_Main.length} junctions")
      private val junc = Array.ofDim [Junction] (aniCoords_Main.length)
      private val juncNames = Array ("warm_up", "sensor1", "onR_merge1", "sensor2",
                                     "sensor3", "onR_merge2", "sensor4", "sensor5")
      // Mainline junctions hold 0 s; ramp merges keep the default 1 s until gap acceptance exists.
      for i <- junc.indices do
          if juncNames(i).startsWith ("onR_merge") then
              junc(i) = new Junction (juncNames(i), xy = aniCoords_Main(i), nt = nt, nl = numLanes)
          else
              junc(i) = new Junction (juncNames(i), Sharp (0.0), aniCoords_Main(i), nt, numLanes)
          end if
          debug ("init", s"  junc($i) = ${juncNames(i)} at ${aniCoords_Main(i)}")

      // Ramp sensors
      debug ("init", s"Creating ${aniCoords_Ramp.length} ramp sensors")
      private val ramp_sensors = Array.ofDim [Junction] (aniCoords_Ramp.length)
      for i <- ramp_sensors.indices do
          ramp_sensors(i) = new Junction (s"ramp${i + 1}", xy = aniCoords_Ramp(i), nt = nt, nl = numLanes)
          debug ("init", s"  ramp_sensor($i) = ramp${i + 1} at ${aniCoords_Ramp(i)}")

      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      // Route and sources
      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      private val intermediateJunc = junc.slice (1, junc.length - 1)
      debug ("init", s"Creating route with ${intermediateJunc.length} intermediate junctions")
      private val route = Route ("Rte", numLanes, intermediateJunc, junc(0), junc.last, motion)
      debug ("init", s"Route created with ${route.pathway.length} pathways")

      // Keep the legacy route and replace only the selected sections with mesoscopic links.
      private val mesoSegments = Set (0, 3, 4)
      private val mesoLinks = Array.fill (numSegments) (Array.empty [MesoVTransport])  // mesoLinks(seg)(lane) = MesoVTransport for segment seg and lane
      private val flowDiagram = new IdmFlux (v0 = Vehicle.vmax, tau = Vehicle.T,
                                             s0 = Vehicle.s, len = Vehicle.len, delta = Vehicle.del)  //

      // Same perpendicular-offset formula as Route.calcShift2, against the same
      // from/to Route itself was built with (junc(0), junc.last) -- calcShift2 is
      // private, so this mirrors it rather than reimplementing a different one.
      // Same GAP as Route's own default (no laneGap was passed to Route below).
      private val corridorDx  = junc.last.at(0) - junc(0).at(0)
      private val corridorDy  = junc.last.at(1) - junc(0).at(1)
      private val corridorHyp = math.hypot (corridorDx, corridorDy)
      private val laneUnit    = VectorD (corridorDy / corridorHyp, -corridorDx / corridorHyp)
      private val LANE_GAP    = 50.0
      private def laneShift (lane: Int): VectorD =
          val physicalLane = numLanes - 1 - lane
          laneUnit * ((physicalLane - (numLanes - 1) / 2.0) * LANE_GAP)
      end laneShift

      for seg <- mesoSegments do
          mesoLinks(seg) = Array.tabulate (numLanes) { lane =>
              val shift = laneShift (lane)
              val link = new MesoVTransport (s"meso-$seg-$lane", junc(seg), junc(seg + 1),
                                             Vehicle.vmax, flowDiagram.q_max * 3600.0,
                                             shift1 = shift, shift2 = shift,
                                             flowDiagram = Some (flowDiagram))
              link.length = route.pathway(lane).seg(seg).length
              link
          }
      end for

      // Sinks
      private val (x0, y0) = (aniCoords_Main.last._1 - 100.0, aniCoords_Main.last._2 - 100.0)
      private val sinks = Sink.group ((x0.toInt, y0.toInt),
          ("sinkMain", (0, 0))
      )

      // Lane totals from arrival sources
      private val laneTotalsML = Array.tabulate (numLanes) { l =>
          mainlineArrivalSources(l).getTotalVehicles (l)
      }

      // Mainline sources - builder creates based on numLanes (no hardcoded lane count)
      debug ("init", "Building mainline sources...")
      private val mainlineSources: List [VSource] = builder.buildMainlineSources (
          this, () => Car (), route, "vsrcML", iArrivalRV
      )
      debug ("init", s"Created ${mainlineSources.length} mainline sources")

      // Ramp sources - builder creates based on numRamps, uses per-ramp distribution from config
      debug ("init", "Building ramp sources...")
      private val rampSources: List [VSource] = builder.buildRampSources (
          this, () => Car (), centerPos, offsets
      )
      debug ("init", s"Created ${rampSources.length} ramp sources")

      private val sources: List [VSource] = mainlineSources ++ rampSources

      // Ramps - builder encapsulates merge info (no hardcoded source indices)
      debug ("init", "Building ramps...")
      private val ramps: Array [Ramp] = builder.buildRamps (motion, rampSources, ramp_sensors)
      debug ("init", s"Created ${ramps.length} ramps")

      // Register components
      debug ("init", "Registering components with model...")
      addComponents (sources, junc.toList ++ ramp_sensors.toList, sinks, ramps.toList,
                     mesoLinks.flatten.toList)
      route.pathway.foreach (addComponent (_))
      debug ("init", "Component registration complete")


      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      // Car entity
      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      private val highway_length = junc.length - 1

      case class Car () extends Vehicle ("c", this):

          override def act (): Unit =
              //println (s"act: Car $this BEGINS")            // DIAG: ~56k vehicles, one line each
              if subtype < numLanes then
                  // Mainline entry - subtype 0 until numLanes
                  laneID = subtype
                  debug ("Act::: ", s"For $this actor in: lane: ${this.laneID}: LaneID Assigned")
                  driveHighway ()
              else
                  // Ramp entry - subtype numLanes, numLanes+1, ...
                  laneID = numLanes - 1
                  debug ("Act::: ", s"For $this actor in: lane: ${this.laneID}: LaneID Assigned")
                  val onRamp = ramps(subtype - numLanes)
                  driveRamp (onRamp)
                  driveHighway ()
          end act

          private def driveHighway (): Unit =
              val joinSeg = if subtype < numLanes then 0 else rampJoinSeg(subtype - numLanes)
              //debug ("driveHighway", s"$me starting at seg=$joinSeg, lane=$laneID")

              if subtype >= numLanes then junc(joinSeg).jump ()
              else junc(0).jump ()

              var inMicro = false
              cfor (joinSeg, highway_length) { seg =>
                  if mesoLinks(seg).nonEmpty then          // Meso link for this segment
                      if inMicro then
                          route.pathway(laneID).removeFromAlist (this)  // Remove car from micro simulation
                          inMicro = false
                      end if
                      mesoLinks(seg)(laneID).move ()
                  else
                      if ! inMicro then
                          val carAhead = route.pathway(laneID).mergeLeaderAt (seg)
                          route.pathway(laneID).joinAt (this, carAhead, seg)
                          inMicro = true
                      end if

//                    route.driveSegment (this, seg)            // MOBIL ON: lane choice + car-follow this segment
//                    route.pathway(laneID).seg(seg).move ()    // MOBIL OFF: no lane changes (swap with the line above)
                      route.driveSegmentSimple (this, seg)      // SIMPLE: old rule (leader < 0.1 vmax + 20 m gaps)
                  end if

                  if junc(seg + 1).name.startsWith ("sensor") then
                      if mesoLinks(seg).nonEmpty then
                          val link = mesoLinks(seg)(laneID)
                          junc(seg + 1).jump (Some (link.density))
                      else junc(seg + 1).jump ()
              }

              if inMicro then route.pathway(laneID).removeFromAlist (this)

              //println (s"act: $this ENDS")
              sinks.head.leave ()
          end driveHighway

          private def driveRamp (comp: Component): Unit =
              val r = comp.asInstanceOf [Ramp]
              //debug ("driveRamp", s"$me on ramp ${r.name}, mode=${r.mode}")
              val carAhead = r.getLast
              //println (s"act: carAhead = $carAhead")
              r.addToAlist (this, carAhead)

              if r.mode == RampMode.On then
                  r.lane.move ()
                  r.to.asInstanceOf [Junction].jump ()
              end if

              r.removeFromAlist (this)
          end driveRamp

      end Car

      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      // RowTimeLoader data access methods - delegates to companion object helpers
      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      /** Get number of time rows in loaded data. */
      def getDataDimension: Int = nt

      /** Get inter-arrival time (mu) - delegates to RowTimeLoader companion object. */
      def getMuForSource(sourceIdx: Int): VectorD =
          RowTimeLoader.getMuForSourceDefault(mainlineArrivalSources, rampArrivalSources,
                                               numLanes, nt, sourceIdx)

      /** Anchor-sensor speed matrix -- read from the PeMS CSV once per run.
       *  `VSource` asks for this on every vehicle it creates, so it must not
       *  re-read the file each time.
       */
      private val anchorSpeed = RowTimeLoader.getSpeedMatrixDefault(demand)

      /** Get speed matrix - delegates to RowTimeLoader companion object. */
      def getSpeedMatrix(): MatrixD = anchorSpeed

      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      // Public accessors for calibration
      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      /** Get junctions array for calibration/validation */
      def getJunctions: Array[Junction] = junc

      /** Get PeMS-to-junction mapping indices */
      def getPemsSensorIndices: Array[Int] = pemsToJunc

      /** Get demand configuration */
      def getDemandConfig: PeMSDemand = demand

      /** Get dynamics configuration (car-following model, solver, params) */
      def getDynamicsConfig: DynamicsConfig = dynamics

      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      // Finish - use SimulationReport for validation
      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      override def fini (rep: Int): Unit =
  //        println ("\n" + "=" * 70)
  //        println ("CalRoute101_3 SIMULATION COMPLETE")
  //        println ("=" * 70)

          // Use new SimulationReport for validation
          val report = SimulationReport.fromJunctions (junc, pemsToJunc, demand)
  //        report.report ()
  //
  //        println ("\nFitness (for optimization): " + report.fitness ())
          super.fini (rep)
      end fini

      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      // Note: simulate() and waitFinished() are called from CalibrationFramework
      // For standalone run, use @main def runCalRoute101_3_withMeso
      //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



//      simulate ()
//      waitFinished ()
//      //println (f"[wall] setup ${(wallBuilt - wallStart) / 1e9}%.1f s   run ${(wallEnd - wallBuilt) / 1e9}%.1f s   total ${(wallEnd - wallStart) / 1e9}%.1f s")
//      Model.shutdown ()

  end CalRoute101_3_withMeso
