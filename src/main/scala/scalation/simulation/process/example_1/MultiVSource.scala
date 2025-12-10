package scalation
package simulation
package process
package example_1

import scalation.random.*
import scala.collection.mutable.ListBuffer
import scala.math.hypot


object MultiVSource:

    /** Per-lane speed limits in mph for mainline lanes 0..4 */
    private val laneSpeedsMph = Array(72.0, 70.0, 68.0, 65.0, 62.0)

    /** Build 4 per-lane mainline VSources positioned from Route geometry using shift().
     * NOTE: iArrivalRV parameter is ignored - each lane gets its own Erlang instance for independent mu values
     */
    def mainline4(model: Model, makeCar: () => Vehicle, route: Route, baseName: String,
                  iArrivalRV: Variate, laneTotals: Array[Int]): List[VSource] =


        val sources = new ListBuffer[VSource]()

        require(laneTotals.length == 4, "MultiVSource expected 5 lane counts")

        var l = 0
        while l < 4 do
            val name = s"${baseName}_L$l"
            val subtype = l // 0..4 = mainline lanes
            val nStop = laneTotals(l) // number of vehicles to generate for lane l (0..4): sum of the flow counts by lanes column
            val loc = shift(route, l) // positioning of the VSource near the start of lane l

            // Create per-lane vehicle factory that captures the correct speed
            val laneSpeed = laneSpeedsMph(l) / 2.24694 // Convert to m/s and capture for this lane
            val makeCarForLane = () => {
                Vehicle.setInitialSpeed(laneSpeed) // Set speed for this specific lane
                makeCar() // Create vehicle
            }

//            // Create per-lane Erlang instance so each VSource has independent mu calculation
//            val laneArrivalRV = Erlang2S() // Each lane gets its own Erlang-3 instance

            val src = new VSource(name, model, makeCarForLane, subtype, nStop, iArrivalRV, loc) // VSource for lane l (0..4)lanes
            sources += src // add each lane's VSource to the list
            l += 1 // next lane
        end while

        sources.toList // return the list of 5 lane VSources
    end mainline4

    /** Build 5 per-lane mainline VSources positioned from Route geometry using shift().
     *  NOTE: iArrivalRV parameter is ignored - each lane gets its own Erlang instance for independent mu values
     */
    def mainline5(model: Model, makeCar: () => Vehicle, route: Route, baseName: String,
                  iArrivalRV: Variate, laneTotals: Array[Int]): List[VSource] =


        val sources = new ListBuffer[VSource]()

        require(laneTotals.length == 5, "MultiVSource expected 5 lane counts")

        var l = 0
        while l < 5 do
            val name    = s"${baseName}_L$l"
            val subtype = l                 // 0..4 = mainline lanes
            val nStop   = laneTotals(l)     // number of vehicles to generate for lane l (0..4): sum of the flow counts by lanes column
            val loc     = shift(route, l)   // positioning of the VSource near the start of lane l

            // Create per-lane vehicle factory that captures the correct speed
            val laneSpeed = laneSpeedsMph(l) / 2.24694  // Convert to m/s and capture for this lane
            val makeCarForLane = () => {
                Vehicle.setInitialSpeed(laneSpeed)  // Set speed for this specific lane
                makeCar()  // Create vehicle
            }

            // Create per-lane Erlang instance so each VSource has independent mu calculation
            val laneArrivalRV = Erlang(3)  // Each lane gets its own Erlang-3 instance

            val src  = new VSource(name, model, makeCarForLane, subtype, nStop, laneArrivalRV, loc)  // VSource for lane l (0..4)lanes
            sources += src  // add each lane's VSource to the list
            l += 1          // next lane
        end while

        sources.toList    // return the list of 5 lane VSources
    end mainline5

    /** Compute a shifted location near the start of lane `l` for placing a VSource.
     * using the first segment of the route's pathway for lane `l`.
     *  @param route  the multi-lane route
     *  @param l      the lane index (0..4)
     *  @param back   how far back from the start of the lane to position the VSource
     *  @return       the shifted location as an array [x, y, width, height]
     * */
    def shift(route: Route, l: Int, back: Double = 15.0): Array[Double] =
        val seg0 = route.path(l).seg(0)
        val p1x = seg0.p1(0)             // start point of the first segment of lane l
        val p1y = seg0.p1(1)             // end point of the first segment of lane l
        val p2x = seg0.p2(0)            // start point of the first segment of lane l
        val p2y = seg0.p2(1)            // end point of the first segment of lane l
        val dx = p2x - p1x              // segment vector components
        val dy = p2y - p1y              // segment vector components
        val mag = hypot(dx, dy)       // magnitude of the segment vector
        val ux = if mag > 1e-9 then dx / mag else 0.0     // unit vector components
        val uy = if mag > 1e-9 then dy / mag else 0.0     // shifted position back from p1 along the segment direction
        val sx = p1x - ux * back
        val sy = p1y - uy * back
        Array(sx, sy, 20.0, 20.0)      // return shifted location with width and height
    end shift

end MultiVSource
