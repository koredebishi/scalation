package scalation
package simulation
package process
package example_1

import scalation.random.*


@main def runPathwayRampSmokeTest(): Unit = new PathwayRampSmokeTest()

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class PathwayRampSmokeTest(name: String = "PathwayRampSmokeTest", reps: Int = 1, animating: Boolean = true, aniRatio: Double = 500.0, nStop: Int = 0, stream: Int = 0)
    extends Model(name, reps, animating, aniRatio):

    // ------------------------------------------------------------
    // Configuration and Data
    // ------------------------------------------------------------
    val iArr = Exponential(MINUTE / 12.0, stream)
    val motion = GippsDynamics
    val coin = Bernoulli()
    val rowTime = 100.0
    val trafficData = new TrafficConfig("/seven_sensors/402376.csv", rowTime, stream)
    val aniCoords = trafficData.getJunctions(DATA_DIR + "gps.txt", (1000, 800))

    // ------------------------------------------------------------
    // Component Setup
    // ------------------------------------------------------------

    val entry = VSource("entry", this, () => Car(), 0, nStop, iArr, (aniCoords(0)._1.toInt, aniCoords(0)._2.toInt))
    val exit = Sink("exit", (aniCoords.last._1.toInt, aniCoords.last._2.toInt))
    val junc = Array.tabulate(5)(i => Junction(s"jc-$i", xy = aniCoords(i + 1), nt = 5))
    val lane = Pathway("trunk", junc, entry, exit, motion)


    //Multiple Vsource (An array of Vsources)
    val onRampEntry = VSource("orEntry", this, () => Car(), 0, 2, iArr, (aniCoords(1)._1.toInt + 200, aniCoords(1)._2.toInt - 20))

    val onRamp = Ramp("onRamp", onRampEntry, junc(0), motion, RampMode.On)  // Index based on the Vsource Aray.....

    val offRampExit = Sink("offrExit", (aniCoords(5)._1.toInt, aniCoords(5)._2.toInt -  200))
    val offRamp = Ramp("offRamp", junc(3), offRampExit, motion, RampMode.Off)


    // ------------------------------------------------------------
    // Register Components
    // ------------------------------------------------------------
    addComponents(List(entry, onRampEntry), junc.toList, List(exit, offRampExit), List(lane, onRamp, offRamp))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    case class Car() extends Vehicle("c", this):

        override def act(): Unit =
            // Adjust speed based on source
            if mySource == onRampEntry then
                Vehicle.setInitialSpeed((60.0 / 2.236) / 2.0) // Slower for onRamp
            else
                Vehicle.setInitialSpeed(60.0 / 2.236)

            this.laneID = subtype
            println(s"==> Car ${this.name} spawned from: ${mySource.name}")

            if mySource == entry then
                val carAhead = lane.getLast
                lane.addToAlist(this, carAhead)
                for i <- lane.seg.indices do
                    lane.seg(i).move()
                    lane.junc(i).jump()
                    lane.removeFromAlist(this)
                    if coin.igen > 0.5 then
                        exit.leave()
                    else
                        val carAhead = offRamp.getLast
                        offRamp.lane.move()
                        offRampExit.leave()
                end for

            else if mySource == onRampEntry then
                val carAhead = onRamp.getLast
                onRamp.addToAlist(this, carAhead)
                onRamp.lane.move()
                for i <- junc.indices do
                    junc(i).jump()
                    lane.seg(i).move()
                    if coin.igen > 0.5 then
                        exit.leave()
                    else
                        val carAhead = offRamp.getLast
                        offRamp.lane.move()
                        offRampExit.leave()
                end for

    end Car




    //1. Cars spawns from the entry VSource
            //2. Cars spawns from the onRampEntry VSource we slow them down abit using           Vehicle.setInitialSpeed((60.0 / 2.236)/2.0) or whichever is best
            //the onramp cars move from Onramp -- junction0
            //at that junction0, they continue moving using the junction0 connecting frontal segment
            //they continue moving at the trunk pathway and then exited.
            // Now, the exiting stratergy may be (exit at truck exit) or (exit at offRamp exit): We can use a Bernoulli RV to decide
            //3.for the truck cars, the movement is just straight forward, they move from the trunk pathway to the exit sink or, we use a Bernoulli RV to decide if they exit at the offRamp exit or the trunk exit
            //At the for loop for the truck, we can add our lane change logic


        // this is the simple code without any bullshit verbose code added.

   // end Car

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    simulate()
    waitFinished()
    Model.shutdown()
end PathwayRampSmokeTest


/**
 * Create Array structure of components: (An array of Vsources).
 * Use the same order of the Array to put the Onramps on the road
 * then access the Sources via Index of the Array such that you can get the correct source at the correct time
 * No need of the name of the source. We just need to Index.
 * the substyoe of the Vehicle can help determing the source the vehicle came from
 * Array(Vsource1, Vsource2):
 * The Vehcles can enter the pathway via the based on the source and subtype:
 * Need a Map: -----> which source goes to which pathway via a Junction and the Vsource subtype
 * Source Index: Map to Pathway Index (junction)  ---> Allows Vehicles to know which pathway to take based on the source they came from
 * junction to Sink for offRamp: Should model same for onRamp
 * Maybe a configuration file: And this needs to be exposed to Vehicles
 *
 * A method that creates this Vsoucres and add them to array then u can use them via the index of the array: Components in a loop
 *
 * A config
 * //
 * 1. chnage the hard coded C: reference in my root class before anything
 * 2. Brainstorm the notion of An array of Vsources and it's mapped junctions.
 */