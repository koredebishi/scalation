
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jan 25 19:44:16 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Path for Modeling Multi-Lane Pathway
 */

package scalation
package simulation
package process

import scala.math.hypot

import scalation.animation.CommandType._
import scalation.mathstat.VectorD
import scalation.random.Variate
import scalation.scala2d.Colors._
import scala.math.{abs,min}
import scalation.random.Bernoulli
//import scalation.simulation.process.modeling.clustering.Coordinates
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///**
// A pathway is a single lane that consists of VTransports and Junctions from Source to Sink
// this single lane pathway is a single doublylinked list.
// NB: the data structure that allows us query for car in a pathway is the doublylinked list
// ------------A-doublyLinkedList-of-cars par pathway---------------------------------------------
// pathway1//VSource-----------------path-----------------Junction-----------------path-----------------Sink
// pathway2//VSource-----------------path-----------------Junction-----------------path-----------------Sink
// pathway3//VSource-----------------path-----------------Junction-----------------path-----------------Sink
// pathway4//VSource-----------------path-----------------Junction-----------------path-----------------Sink
// *  @param name      the name of the pathway
// *  @param from      the starting component
// *  @param to        the ending component
// *  @param junc  the junction in the middle of the pathway
// *  @param motion    the variate or dynamics model for the speed/trip-time for motion down the `Path`
// *  @param isSpeed   whether speed or trip-time is used for motion
// *  @param bend      the bend or curvature of the `Path` (0 => line)
// */
//
//class Pathway(name: String, k: Int, val from: Component, val junc: Junction, val to: Component,
//              motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0)
//    extends Component:
//
//    private val GAP = 20.0
//    private val delta = calcShift
//    val coin = Bernoulli(0.9)
//
//
//    private val debug = debugf("Pathway", true)
//    private val flaw = flawf("Pathway")
//
//    val pathways = Array.ofDim[VTransport](k, 2)                                                                        //k pathways, each with 2 VTransports
//    val vtrees = Array.fill(k)(DoublyLinkedList[Vehicle]())                                                             //Each pathway maintains its own doubly linked list of vehicles
//
//    debug("init", s"name = $name, k = $k, from = ${from.name}, junc = ${junc.name}, to = ${to.name}")
//
//    // Create k pathways
//    //Source-----------junc1------------junc2------------junc3------junc4------junc5------Sink
//    //val junc = Array(junc1, junc2, junc3, junc4, junc5)
//
//
//    for i <- 0 until k do
//        val shift = VectorD((i - (k - 1) / 2.0) * delta(0), (i - (k - 1) / 2.0) * delta(1))                             //shift for each pathway
//        pathways(i)(0) = new VTransport(s"${name}_lane${i}_seg0", from, junc, motion, isSpeed, 0.0, shift, shift)       // from source to junction
//        pathways(i)(1) = new VTransport(s"${name}_lane${i}_seg1", junc, to, motion, isSpeed, 0.0, shift, shift)         // from junction to sink
//
//        subpart += pathways(i)(0)                                                                                       // add the first segment to the subpart
//        subpart += pathways(i)(1)                                                                                       // add the second segment to the subpart
//    end for
//
//    initComponent(name, Array())
//
//
////    def move(i: Int, actor:Vehicle): Unit =
////        pathways(i)(0).move() // Always move first segment (source → junction)
////
//////
////        val attemptLaneChange = coin.igen
////
////        if attemptLaneChange == 1 then
////            val j = (i + 1) % pathways.length
////            if changeLane(i, j, actor) then   // if this is true, then manipulate the data structure
////                junc.jump()                   // jump the token via animation
////                println(s"Car: ${actor.id} will change lane from $i to $j")
////                pathways(j)(1).move()         //move the token in the new direction
////        else
////            pathways(i)(1).move() // Move second segment (junction → sink)
////    end move
//
//    /**
//     * Attempt to change pathways for a vehicle.
//     *
//     * @param l1    The current pathway index
//     * @param l2    The target pathway index
//     * @param actor The vehicle attempting to change pathways
//     * @return True if lane change was successful, False otherwise
//     */
//    def changeLane(i: Int, j: Int, actor: Vehicle): Boolean =
//        if abs(i - j) > 1 then
//            flaw("changePathway", s"Car: $actor UNSAFE to change multiple pathways at once: lane:$i to lane:$j")
//            return false
//
//        println(s"Vehicle $actor needs to change lane from $i to $j")
//        val safeDisp = pathways(i)(0).safetydist
//        val vBehind = pathways(j)(0).getFirst
//        val vAhead =  if vBehind != null && vBehind.myPathNode.ahead != null then vBehind.myPathNode.ahead.elem else null
//
//        println(s"changeLane $actor in lane $i: vAhead: $vAhead and vBehind: $vBehind @ lane$j")
//
//        //suppose car4 in pathway(i) needs to change lane to pathway(j):
//        val gapBehind = if vBehind != null then pathways(j)(0).length - vBehind.disp else safeDisp    // use gap as displacement between length and car or make it 20 if otherwise
//        val gapVahead = if vAhead  != null then abs(vAhead.t_disp - pathways(j)(1).length) else safeDisp  // do same as above here too
//
//        val gap = min(gapBehind, gapVahead)     // return the minimum gap between ahead and behind, this allows for a safe lane change.
//        if gap >= safeDisp
//            then
//                removeFromAlist(actor, i)       // remove the car from it's doublylinkedlist
//                addToAlist(actor, vAhead, j)    // add the car to the new doublylinkedlist
//                true
//        else
//            println(s"Unsafe to change lane from $i to $j because the gap is small")
//            false                              // gap too small, no lane change allowed.
//    end changeLane
//
//
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Add a vehicle to the correct pathway's doubly linked list.
//     * @param actor the vehicle to add
//     * @param other the other vehicle (the one ahead, null if none)
//     * @param j the pathway index (0 to k-1), where k is the number of pathways
//     * */
//    def addToAlist(actor: Vehicle, other: Vehicle, j: Int): Unit =
//        actor.myPathway = this                                                                                          //set the actor's pathway to this pathway
//        val thisVtree = vtrees(j)                                                                                       //extract the correct vtree first
//        val otherNode = if other != null then other.myPathNode.asInstanceOf[thisVtree.Node] else thisVtree.headNode //maybe null                  //get the other actor's node
//        if otherNode != null then
//            println(s"the if part(addBefore method) is used otherNode: $otherNode")
//            actor.myPathNode = thisVtree.addBefore(actor, otherNode)
//        else
//            println(s"the else part (add method) was used otherNode: $otherNode")
//            actor.myPathNode = thisVtree.add(actor, otherNode)                                                             //add the actor to the vtree
//    end addToAlist
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Remove a vehicle from the correct pathway's doubly linked list.
//     * @param actor the vehicle to remove
//     * @param i the pathway index (0 to k-1), where k is the number of pathways
//     * */
//    def removeFromAlist(actor: Vehicle, i: Int): Unit =
//        val thisVtree = vtrees(i)
//        thisVtree.remove(actor.myPathNode.asInstanceOf[thisVtree.Node])
//    end removeFromAlist
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Get the first vehicle in a specific pathway.
//     * @param i the pathway index
//     * */
//    def getFirst(i: Int): Vehicle =
//        if vtrees(i).isEmpty then null else vtrees(i).head
//    end getFirst
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Get the last vehicle in a specific pathway.
//     * @param i the pathway index
//     * */
//    def getLast(i: Int): Vehicle =
//        println(s"I was called with lane id $i")
//        val thisVtree = vtrees(i)
//        println(vtrees(i))
//        if thisVtree.isEmpty then
//            {println(s"the vtree is empty"); null}
//        else
//            println(s"the vtree is not empty ${vtrees(i)}")
//            thisVtree.last
//    end getLast
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Calculate spacing adjustment.
//     *
//     * */
//    private def calcShift: VectorD =
//        val xdist = from.at(0) - to.at(0)
//        val ydist = from.at(1) - to.at(1)
//        val hyp = hypot(xdist, ydist)
//        VectorD((ydist / hyp) * GAP, -(xdist / hyp) * GAP)
//    end calcShift
//
//    /** Give the location of the curve to be its starting point. */
//    override def at: Array[Double] = pathways(0)(0).at
//
//    /** Get the selector random variate (delegated to the first transport segment). */
//    def selector: Variate = pathways(0)(0).selector
//
//    /** Set the selector random variate in the first transport segment. */
//    def selector_=(selectorRV: Variate): Unit = pathways(0)(0).selector = selectorRV
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Display all pathways. */
//    override def display(): Unit =
//        for i <- 0 until k do
//            director.animate(pathways(i)(0), CreateEdge, blue, pathways(i)(0).curve, pathways(i)(0).from, pathways(i)(0).to,
//                Array(pathways(i)(0).p1(0), pathways(i)(0).p1(1), pathways(i)(0).pc(0), pathways(i)(0).pc(1), pathways(i)(0).p2(0), pathways(i)(0).p2(1)))
//
//            director.animate(pathways(i)(1), CreateEdge, blue, pathways(i)(1).curve, pathways(i)(1).from, pathways(i)(1).to,
//                Array(pathways(i)(1).p1(0), pathways(i)(1).p1(1), pathways(i)(1).pc(0), pathways(i)(1).pc(1), pathways(i)(1).p2(0), pathways(i)(1).p2(1)))
//    end display
//
//end Pathway


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**
 A pathway is a single lane that consists of VTransports and Junctions from Source to Sink
 this single lane pathway is a single doublylinked list.
 NB: the data structure that allows us query for car in a pathway is the doublylinked list
 ------------A-doublyLinkedList-of-cars par pathway---------------------------------------------
 pathway1//VSource-----------------path-----------------Junction-----------------path-----------------Sink
 pathway2//VSource-----------------path-----------------Junction-----------------path-----------------Sink
 pathway3//VSource-----------------path-----------------Junction-----------------path-----------------Sink
 pathway4//VSource-----------------path-----------------Junction-----------------path-----------------Sink
 *  @param name      the name of the pathway
 *  @param from      the starting component
 *  @param to        the ending component
 *  @param junc  the junction in the middle of the pathway
 *  @param motion    the variate or dynamics model for the speed/trip-time for motion down the `Path`
 *  @param isSpeed   whether speed or trip-time is used for motion
 *  @param bend      the bend or curvature of the `Path` (0 => line)
 */

class Pathway(name: String, k: Int, j: Int, from: Component,val srcSensor: Junction, val junc: Array[Junction], val exitSensor: Junction, val to: Component,
              motion: Dynamics, isSpeed: Boolean = false, bend: Double = 0.0)
    extends Component:

    private val GAP = 30.0
    private val delta = calcShift
    val coin = Bernoulli(0.9)

    private val debug = debugf("Pathway", true)
    private val flaw = flawf("Pathway")

    // Create pathway structure: k pathways, each with j+1 segments (including exit)
    val pathways = Array.ofDim[VTransport](k, j+1) // Number of junctions is j, so segments must be j+1
    val vtrees = Array.fill(k)(DoublyLinkedList[Vehicle]())

    debug("init", s"name = $name, k = $k, from = ${from.name}, junc =, to = ${to.name}")

//    for i <- 0 until k do
//        val shift = VectorD((i - (k - 1) / 2.0) * delta(0), (i - (k - 1) / 2.0) * delta(1))
//        pathways(i)(0) = new VTransport(s"${name}l${i}_seg0", from, junc(0), motion, isSpeed, 0.0, shift, shift)
//        for s <- 0 until j - 1 do
//            pathways(i)(s + 1) = new VTransport(s"${name}l${i}_seg${s + 1}", junc(s), junc(s + 1), motion, isSpeed, 0.0, shift, shift)
//        pathways(i)(j) = new VTransport(s"${name}l${i}_seg${j}", junc(j - 1), to, motion, isSpeed, 0.0, shift, shift)
//        subpart ++= pathways(i)
//    end for


    for i <- 0 until k do
        val shift = VectorD((i - (k - 1) / 2.0) * delta(0), (i - (k - 1) / 2.0) * delta(1))

        // Hidden segment: from → srcSensor (Not displayed)
        val entrySegment = new VTransport(s"${name}l${i}_entry", from, srcSensor, motion, isSpeed, 0.0, shift, shift)

        // First visible segment: srcSensor → junc(0)
        pathways(i)(0) = new VTransport(s"${name}l${i}_seg0", srcSensor, junc(0), motion, isSpeed, 0.0, shift, shift)

        // Mid-segments: Junction connections
        for s <- 0 until j - 1 do
            pathways(i)(s + 1) = new VTransport(s"${name}l${i}_seg${s + 1}", junc(s), junc(s + 1), motion, isSpeed, 0.0, shift, shift)

        // Last visible segment: junc(j - 1) → exitSensor
        pathways(i)(j) = new VTransport(s"${name}l${i}_seg${j}", junc(j - 1), exitSensor, motion, isSpeed, 0.0, shift, shift)

        // Hidden segment: exitSensor → to (Not displayed)
        val exitSegment = new VTransport(s"${name}l${i}_exit", exitSensor, to, motion, isSpeed, 0.0, shift, shift)

        // Add segments to the pathway
        subpart ++= (entrySegment :: pathways(i).toList) :+ exitSegment
    end for


    initComponent(name, Array())

    /**
     * Attempt to change pathways for a vehicle.
     *
     * @param i    The current pathway index
     * @param j    The target pathway index
     * @param k    The segment index
     * @param actor The vehicle attempting to change pathways
     * @return True if lane change was successful, False otherwise
     */
    def changeLane(i: Int, j: Int, actor: Vehicle, k:Int): Boolean =
        if abs(i - j) > 1 then
            flaw("changePathway", s"Car: $actor UNSAFE to change multiple pathways at once: lane:$i to lane:$j")
            return false
        //Fix < --- if a vehicle changes lane from i to j
        //need to find the proper behind vehicle from the second pathway(dll). that's going to be
        println(s"Vehicle $actor needs to change lane from lane$i to lane$j with seg:$k")
        val safeDisp = pathways(i)(k).safetydist
        val vBehind = pathways(j)(k).getFirst
        val vAhead = if vBehind != null && vBehind.myPathNode.ahead != null then vBehind.myPathNode.ahead.elem else null

        println(s"changeLane $actor in lane $i: vAhead: $vAhead and vBehind: $vBehind @ lane$j")

        //suppose car4 in pathway(i) needs to change lane to pathway(j):
        val gapBehind = if vBehind != null then pathways(j)(k).length - vBehind.disp else safeDisp // use gap as displacement between length and car or make it 20 if otherwise
        val gapVahead = if vAhead != null then abs(vAhead.t_disp - pathways(j)(k+1).length) else safeDisp // do same as above here too

        val gap = min(gapBehind, gapVahead) // return the minimum gap between ahead and behind, this allows for a safe lane change.
        if gap >= safeDisp
        then
            removeFromAlist(actor, i) // remove the car from it's doublylinkedlist
            actor.laneID = j          // update the car lane id to reflect the new lane info
            actor.pathInfo =  pathways(j)(k).name  // update the pathway info before adding to the dll for consistency of information
            addToAlist(actor, vAhead, j) // add the car to the new doublylinkedlist
            true
        else
            println(s"Unsafe to change lane from $i to $j because the gap is small")
            false // gap too small, no lane change allowed.
    end changeLane


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a vehicle to the correct pathway's doubly linked list.
     * @param actor the vehicle to add
     * @param other the other vehicle (the one ahead, null if none)
     * @param j the pathway index (0 to k-1), where k is the number of pathways
     * */
    def addToAlist(actor: Vehicle, other: Vehicle, j: Int): Unit =
        actor.myPathway = this                                                                                          //set the actor's pathway to this pathway
        val thisVtree = vtrees(j)                                                                                       //extract the correct vtree first
        val otherNode = if other != null then other.myPathNode.asInstanceOf[thisVtree.Node] else null//thisVtree.headNode //maybe null                  //get the other actor's node
        if otherNode != null then
            println(s"the if part(addBefore method) is used otherNode: $otherNode")
            actor.myPathNode = thisVtree.addBefore(actor, otherNode)
        else
            println(s"the else part (add method) was used otherNode: $otherNode")
            actor.myPathNode = thisVtree.add(actor, otherNode)                                                             //add the actor to the vtree
    end addToAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove a vehicle from the correct pathway's doubly linked list.
     * @param actor the vehicle to remove
     * @param i the pathway index (0 to k-1), where k is the number of pathways
     * */
    def removeFromAlist(actor: Vehicle, i: Int): Unit =
        val thisVtree = vtrees(i)
        thisVtree.remove(actor.myPathNode.asInstanceOf[thisVtree.Node])
    end removeFromAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the first vehicle in a specific pathway.
     * @param i the pathway index
     * */
    def getFirst(i: Int): Vehicle =
        if vtrees(i).isEmpty then null else vtrees(i).head
    end getFirst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the last vehicle in a specific pathway.
     * @param i the pathway index
     * */
    def getLast(i: Int): Vehicle =
        val thisVtree = vtrees(i)
        println(vtrees(i))
        if thisVtree.isEmpty then
        {println(s"the vtree is empty"); null}
        else
            println(s"the vtree is not empty ${vtrees(i)}")
            thisVtree.last
    end getLast

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Calculate spacing adjustment.
//     *
//     * */
//    private def calcShift: VectorD =
//        val xdist = from.at(0) - to.at(0)
//        val ydist = from.at(1) - to.at(1)
//        val hyp = hypot(xdist, ydist)
//        VectorD((ydist / hyp) * GAP, -(xdist / hyp) * GAP)
//    end calcShift
//
//    /** Give the location of the curve to be its starting point. */
//    override def at: Array[Double] = pathways(0)(0).at
//
//    /** Get the selector random variate (delegated to the first transport segment). */
//    def selector: Variate = pathways(0)(0).selector
//
//    /** Set the selector random variate in the first transport segment. */
//    def selector_=(selectorRV: Variate): Unit = pathways(0)(0).selector = selectorRV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //    /** Display all pathways. */
    //    override def display(): Unit =
    //        for i <- 0 until k do
    //            director.animate(pathways(i)(0), CreateEdge, blue, pathways(i)(0).curve, pathways(i)(0).from, pathways(i)(0).to,
    //                Array(pathways(i)(0).p1(0), pathways(i)(0).p1(1), pathways(i)(0).pc(0), pathways(i)(0).pc(1), pathways(i)(0).p2(0), pathways(i)(0).p2(1)))
    //
    //            director.animate(pathways(i)(1), CreateEdge, blue, pathways(i)(1).curve, pathways(i)(1).from, pathways(i)(1).to,
    //                Array(pathways(i)(1).p1(0), pathways(i)(1).p1(1), pathways(i)(1).pc(0), pathways(i)(1).pc(1), pathways(i)(1).p2(0), pathways(i)(1).p2(1)))
    //    end display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


    /** Calculate spacing adjustment.
     *
     * */
    private def calcShift: VectorD =
        val xdist = from.at(0) - to.at(0)
        val ydist = from.at(1) - to.at(1)
        val hyp = hypot(xdist, ydist)
        VectorD((ydist / hyp) * GAP, -(xdist / hyp) * GAP)
    end calcShift

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /**
     * Give the location of the curve to be its starting point.
     */
    override def at: Array[Double] = pathways(0)(0).at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /**
     * Get the selector random variate (delegated to the first transport segment).
     */
    def selector: Variate = pathways(0)(0).selector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /**
     * Set the selector random variate in the first transport segment.
     */
    def selector_=(selectorRV: Variate): Unit = pathways(0)(0).selector = selectorRV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /**
     * Display all pathways.
     * Ensures correct animation of all segments from Source → Junctions → Sink.
     */
    override def display(): Unit =
        for i <- 0 until k do
            for j <- pathways(i).indices do // Iterate through all pathway segments
                val segment = pathways(i)(j)
                director.animate(segment, CreateEdge, blue, segment.curve,
                    segment.from, segment.to,
                    Array(segment.p1(0), segment.p1(1),
                        segment.pc(0), segment.pc(1),
                        segment.p2(0), segment.p2(1)))
            end for
        end for
    end display

end Pathway






