package scalation
package simulation
package process
package example_1

import scalation.random.*
import scala.collection.mutable.ListBuffer
import scala.math.hypot

object MultiVSource:

    /** Build 4 per-lane mainline VSources positioned from Route geometry.
     *  Speed is injected dynamically from PEMS data in VSource.act() via actor.setVmax().
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
            val src = new VSource(name, model, makeCar, subtype, nStop, iArrivalRV, loc) // VSource for lane l (0..4)lanes
            sources += src // add each lane's VSource to the list
            l += 1 // next lane
        end while

        sources.toList // return the list of 5 lane VSources
    end mainline4

    /** Build 5 per-lane mainline VSources positioned from Route geometry.
     *  Speed is injected dynamically from PEMS data in VSource.act() via actor.setVmax().
     */
    def mainline5(model: Model, makeCar: () => Vehicle, route: Route, baseName: String,
                  iArrivalRV: Variate, laneTotals: Array[Int]): List[VSource] =

        val sources = new ListBuffer[VSource]()
        require(laneTotals.length == 5, "MultiVSource expected 5 lane counts")

        var l = 0
        while l < 5 do
            val name = s"${baseName}_L$l"
            val subtype = l
            val nStop = laneTotals(l)
            val loc = shift(route, l)

            val src = new VSource(name, model, makeCar, subtype, nStop, iArrivalRV, loc)
            sources += src
            l += 1
        end while

        sources.toList
    end mainline5

    /** Compute a shifted location near the start of lane `l` for placing a VSource.
     *  @param route  the multi-lane route
     *  @param l      the lane index
     *  @param back   how far back from the start of the lane to position the VSource
     */
    def shift(route: Route, l: Int, back: Double = 15.0): Array[Double] =
        val seg0 = route.path(l).seg(0)
        val p1x = seg0.p1(0)
        val p1y = seg0.p1(1)
        val p2x = seg0.p2(0)
        val p2y = seg0.p2(1)
        val dx = p2x - p1x
        val dy = p2y - p1y
        val mag = hypot(dx, dy)
        val ux = if mag > 1e-9 then dx / mag else 0.0
        val uy = if mag > 1e-9 then dy / mag else 0.0
        val sx = p1x - ux * back
        val sy = p1y - uy * back
        Array(sx, sy, 20.0, 20.0)
    end shift

end MultiVSource
