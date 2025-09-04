//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Model: One-Way-Street (with Vehicle) for Process-Interaction Simulation
 */


package scalation
package simulation
package process
package example_1                                       // One-Shot

import scalation.random.{Exponential, Uniform, Bernoulli}


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runOneWayPathwayTest` function is used to launch the `OneWayVehicleModel` class.
 *  > runMain scalation.simulation.process.example_1.runOneWayPathwayTest
 */
//@main def runOneWayRouteTest (): Unit = new OneWayRouteTest ()
//
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//
///** The `OneWayVehicletModel` class simulates a one-lane roead.
// *  Caveat: must add 'from' and 'to' components before transport!!
// *  @param name       the name of the simulation model
// *  @param reps       the number of independent replications to run
// *  @param animating  whether to animate the model
// *  @param aniRatio   the ratio of simulation speed vs. animation speed
// *  @param nStop      the number arrivals before stopping
// *  @param stream     the base random number stream (0 to 999)
// */
//class OneWayRouteTest(name: String = "OneWayRouteTest", reps: Int = 1, animating: Boolean = true,
//                      aniRatio: Double = 500.0, nStop: Int = 10, stream: Int = 0)
//    extends Model(name, reps, animating, aniRatio):
//
//    val lambda = 10.0
//    val iArrival = Exponential(MINUTE / lambda, stream)
//    val motion = GippsDynamics
//    val laneChangeRV = Bernoulli(0.3, stream + 999) // 30 % chance per segment
//
//
//
//    private val debug = debugf("OneWayVehicle2L", true) // debug function
//
//
//    val numJunc = 4
//    val numLane = 4
//    val laneRV = Uniform(0, numLane, stream + numLane)
//
//
//    val entry = VSource("entry", this, () => Car(), 0, nStop, iArrival, (100, 290))
//    val exit = Sink("exit", (1500, 290))
//    val junc = Array.ofDim[Junction](numJunc)
//
//    val spacing = (exit.at(0) - entry.at(0)) / (numJunc + 1)
//    for i <- 0 until numJunc do
//        junc(i) = Junction(s"junc-$i", xy = (entry.at(0) + spacing * (i + 1), 290), nt = numJunc)
//    end for
//
//    val route = Route("route", numLane, junc, entry, exit, motion)
//
//
//    addComponents(List(entry), junc.toList, List(exit))
//    route.lane.foreach(addComponent(_))
//
//
//    println(s"This is the director for this simulation $this")
//
//    var carAhead: Vehicle = null
//
//    case class Car() extends Vehicle("c", this):
//        override def act(): Unit =
//
//            Vehicle.setInitialSpeed(68.0 / 2.24694)
//            var laneIdx = laneRV.igen % numLane // pick an initial lane
//            this.laneID = laneIdx
//
//            // ------------------ enter chosen lane ----------------------------
//            val path = route.path(laneIdx)
//            val carAhead = route.getLast(laneIdx)
//            path.addToAlist(this, carAhead)
//
//            // ------------------ drive through every segment ------------------
//            for seg <- 0 until route.segments do
//
//                if laneChangeRV.igen == 1 then
//                    val target = (laneIdx + 1) % numLane // simple rule
//                    if route.changeLane(laneIdx, target, this, seg) then
//                        laneIdx = target // update on success
//                        println(s"$this changed to lane $laneIdx @ seg $seg")
//                    end if
//                end if
//                route.path(laneIdx).seg(seg).move()
//
//                if seg < junc.length then junc(seg).jump()
//            end for
//
//            //:::::::::::::::::::::::::::::::::::::::
//            route.path(laneIdx).removeFromAlist(this)
//            exit.leave()
//        end act
//    end Car
//
//
//    simulate()
//    waitFinished()
//    Model.shutdown()
//end OneWayRouteTest
//


@main def runOneWayRouteTest(): Unit = new OneWayRouteTest()

class OneWayRouteTest(name: String = "OneWayRouteTest", reps: Int = 1, animating: Boolean = true,
                      aniRatio: Double = 500.0, nStop: Int = 2, stream: Int = 0)
    extends Model(name, reps, animating, aniRatio):

    // Configuration parameters
    val lambda = 10.0
    val iArrival = Exponential(MINUTE / lambda, stream)
    val motion = GippsDynamics
    val laneChangeRV = Bernoulli(0.3, stream + 999) // 30% chance per segment
    val exDec = Bernoulli()
    val rowTime = 100.0




    /**
     * Use RoadCood to load all GPS coordinates and convert them to screen coordinates
     * Returns:
     *  - mainline: sensor1..sensor6
     *  - ramps: onramp1,onramp2,offramp
     */
    def getRoadCoordinates(dims: (Double, Double)): Map[String, Array[(Double, Double)]] =
        val allLatLongs = Roadcood.latlong
        val coordsArray = allLatLongs.values.toArray
        val keys = allLatLongs.keys.toArray

        val coordinates = new scalation.Coordinates(dims._1, dims._2, coordsArray)
        val screenCoords = coordinates.aniCoords

        val coordMap = keys.zip(screenCoords).toMap

        val mainline = Array(
            coordMap("sensor1"),
            coordMap("sensor2"), // offramp merge before sensor2
            coordMap("sensor3"),
            coordMap("sensor4"),
            coordMap("sensor5"),
            coordMap("sensor6")
        )

        val ramps = Array(
            coordMap("onramp1"),
            coordMap("onramp2"),
            coordMap("offramp")
        )
        Map(
            "mainline" -> mainline,
            "ramps" -> ramps
        )
    end getRoadCoordinates


    val (w, h) = (1500, 1500.0) // width and height of animation window

    // Important coordinate system
    //val trafficData = new TrafficConfig("/seven_sensors_old_data/402376.csv", rowTime, stream)



//    val aniCoords = getRoadCoordinates((w, h))
//    val aniCoords_Ramp = getRampCoordinates((w, h)) // already nudged

    private val allGpsCoords = getRoadCoordinates((1500, 1500))
    private val aniCoords = allGpsCoords("mainline")
    private val aniCoords_Ramp = allGpsCoords("ramps")

    private val debug = debugf("OneWayVehicle2L", true) // debug function

    val numJunc = 5
    val numLane = 5
    val laneRV = Uniform(0, numLane, stream + numLane)



    // Use the coordinate system for component placement
    val entry = VSource("entry", this, () => Car(), 0, nStop, iArrival, (aniCoords(0)._1.toInt, aniCoords(0)._2.toInt))
    val exit = Sink("exit", (aniCoords.last._1.toInt, aniCoords.last._2.toInt))
    val junc = Array.ofDim[Junction](numJunc)

    // Use coordinates from aniCoords for junctions
    for i <- 0 until numJunc do
        junc(i) = Junction(s"junc-$i", xy = aniCoords(i + 1), nt = numJunc)
    end for

    val route = Route("route", numLane, junc, entry, exit, motion)

    // Add on-ramp and off-ramp using appropriate coordinates
    val onRampEntry = VSource("orEntry", this, () => Car(), 0, 2, iArrival, (aniCoords(1)._1.toInt + 800, aniCoords(1)._2.toInt - 200))
    val onRamp = Ramp("onRamp", onRampEntry, junc(0), motion, RampMode.On)

    val offRampExit = Sink("offrExit", (aniCoords(4)._1.toInt , aniCoords(4)._2.toInt - 800))
    val offRamp = Ramp("offRamp", junc(3), offRampExit, motion, RampMode.Off)

    addComponents(List(entry, onRampEntry), junc.toList, List(exit, offRampExit))
    route.pathway.foreach(addComponent(_))
    addComponents(List(), List(), List(), List(onRamp, offRamp))

    println(s"This is the director for this simulation $this")

    var carAhead: Vehicle = null

    case class Car() extends Vehicle("c", this):
        override def act(): Unit =


            Vehicle.setInitialSpeed(68.0 / 2.24694)
            var laneIdx = laneRV.igen % numLane // pick an initial lane
            this.laneID = laneIdx

            if mySource == entry then
                // Main entry vehicles - decide route at the beginning
                val useOffRamp = exDec.igen < 0.5

                // ------------------ enter chosen lane ----------------------------
                val path = route.path(laneIdx)
                val carAhead = route.getLast(laneIdx)
                path.addToAlist(this, carAhead)

                // ------------------ drive through segments up to junction 2 ------
                val offRampJunction = 2
                for seg <- 0 until offRampJunction do
                    if laneChangeRV.igen == 1 then
                        val target = (laneIdx + 1) % numLane // simple rule
                        if route.changeLane(laneIdx, target, this, seg) then
                            //laneIdx = target // update on success
                            println(s"$this changed to lane $laneIdx @ seg $seg")
                        end if
                    end if
                    route.path(laneIdx).seg(seg).move()
                    if seg < junc.length then junc(seg).jump()
                end for

                // At junction 2, decide whether to take off-ramp
                if useOffRamp then
                    route.path(laneIdx).removeFromAlist(this)
                    drive(offRamp)
                else
                    // ------------------ drive through remaining segments ----------
                    for seg <- offRampJunction until route.segments do
                        if laneChangeRV.igen == 1 then
                            val target = (laneIdx + 1) % numLane // simple rule
                            if route.changeLane(laneIdx, target, this, seg) then
                                //laneIdx = target // update on success
                                println(s"$this changed to lane $laneIdx @ seg $seg")
                            end if
                        end if
                        route.path(laneIdx).seg(seg).move()
                        if seg < junc.length then junc(seg).jump()
                    end for

                    route.path(laneIdx).removeFromAlist(this)
                    exit.leave()
                end if


            // ------------------ handle on-ramp entry vehicles -------------------
            else if mySource == onRampEntry then

                drive(onRamp)

                // Now continue on the route starting from segment 1
                laneIdx = 0
                val carAhead = route.getLast(laneIdx)
                route.path(laneIdx).addToAlist(this, carAhead)

                for seg <- 1 until route.segments do
                    if laneChangeRV.igen == 1 then
                        val target = (laneIdx + 1) % numLane
                        if route.changeLane(laneIdx, target, this, seg) then
                            println(s"$this changed to lane $laneIdx @ seg $seg")
                            //laneIdx = target
                    end if
                    route.path(laneIdx).seg(seg).move()
                    if seg < junc.length then junc(seg).jump()
                end for

                route.path(laneIdx).removeFromAlist(this)
                exit.leave()
        end act

        private def drive(comp: Component): Unit = comp match
            case r: Ramp =>
                println(s"--> ${this.name} entering Ramp: ${r.name}")
                val carAhead = r.getLast
                r.addToAlist(this, carAhead)

                r.lane.move()

                r.removeFromAlist(this)
                r.to match
                    case s: Sink => s.leave()
                    case _ => // For off-ramps, this should be a Sink

            case s: Sink =>
                println(s"==> ${this.name} reached Sink: ${s.name}")
                s.leave()
        end drive

    end Car

    simulate()
    waitFinished()
    Model.shutdown()
end OneWayRouteTest



//Want to verify that the car ahead is correct
//this code that works using arrival rate then swap for data driven process
// adjust the positioning of the offsets.
