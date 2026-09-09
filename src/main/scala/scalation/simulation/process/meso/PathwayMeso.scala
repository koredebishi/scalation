//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Korede Bishi
 *  @version 2.0
 *  @date    Sat Jan 25 19:44:16 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    PathwayMeso for Modeling a Lane Consisting of Multiple Meso Segments
 */

package scalation
package simulation
package process
package meso

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PathwayMeso` class defines a single lane of mesoscopic segments connected
 *  by shared junctions.  Each segment has its own FIFO and discharge capacity,
 *  with unlimited storage and an open downstream boundary.
 *  @param name            the name of the pathway
 *  @param junc            the array of junctions connecting the segments
 *  @param from            the starting component
 *  @param to              the ending component
 *  @param segmentLengths  physical lengths in metres, or zero to use the drawing lengths
 *  @param freeFlowSpeed   free-flow speed in metres/second for all segments
 *  @param capacity        discharge capacity in vehicles/hour per lane for all segments
 *  @param bend            curvature of the displayed lane
 *  @param laneShift       display shift applied to each segment's endpoints
 */
class PathwayMeso (name: String, val junc: Array [Junction], val from: Component, val to: Component,
                   segmentLengths: VectorD, freeFlowSpeed: Double, capacity: Double,
                   bend: Double = 0.0, laneShift: VectorD = VectorD (0.0, 0.0))
    extends Component:

    val points = from +: junc.toList :+ to

    val seg = Array.ofDim [MesoVTransport] (points.length - 1)

    for i <- seg.indices do
        val p1 = points(i)
        val p2 = points(i + 1)

        seg(i) = new MesoVTransport (s"${name}_seg${i}", p1, p2,
                                    freeFlowSpeed, capacity,
                                    bend, laneShift, laneShift)
        if segmentLengths(i) > 0.0 then seg(i).length = segmentLengths(i)   // else keep the drawing length
        subpart += seg(i)                               // add to the subparts
    end for

    initComponent (name, at)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the first segment's starting point as the pathway location.
     */
    override def at: Array [Double] =
        val xy = seg(0).at
        Array (xy(0), xy(1), 0.0, 0.0)                   // add width and height
    end at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of segments in this lane.
     */
    def getSeglength: Int = seg.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display this pathway's segments.
     */
    override def display (): Unit =
        for segment <- seg do segment.display ()
    end display

end PathwayMeso
