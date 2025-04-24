

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Tue Feb  4 14:56:34 EST 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Vehicle Is Enhanced SimActor Supporting Changing Velocities
 */

package scalation
package simulation
package process


import scala.collection.mutable.Map
import scalation.random.Uniform



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vehicle` class extends the `SimActor` and represents a vehicle in motion.
 *  @param name_     the name of the vehicle
 *  @param director  the model to which this vehicle belongs
 */
abstract class Vehicle (name_ : String, director: Model)
    extends SimActor (name_, director)
        with Dynamics:

    val vmax = Vehicle.speedGen.gen
    var key  = -0.0
    var laneID: Int = -1
    var pathInfo : String = ""

    //var myPathNode: Pathway = null   // my (the actor's) node in the ACTOR LIST pred <-> me <-> succ

    var myPathway: Pathway = null // my (the actor's) node in the ACTOR LIST pred <-> me <-> succ
    private [process] var myPathNode: DoublyLinkedList[Vehicle]#Node = null // my (the actor's) node in the ACTOR LIST pred <-> me <-> succ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The abstract method, 'act', is defined in each subclass to provide specific
     *  behavior.
     */
    override def act (): Unit = println (s"Vehicle.act method should be overridden")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the `SimActor`s full name and activation time.
     */
    override def toString: String = s"Vehicle ($me at $actTime:sec, actor_id= $id, disp:$disp:m, lane:$laneID, path:$pathInfo)"


end Vehicle



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vehicle` object contains driver/vehicle characteristics/properties.
 */
object Vehicle:

    val speedGen = Uniform(20.0 , 40.0)

    /** defaults values for driver/vehicle characteristics/properties (PUBLIC access required)
     *  @see https://en.wikipedia.org/wiki/Intelligent_driver_model
     */

    val def_prop = Map ("rt"   -> 1.0,                       // driver reaction time
        "amax" -> 4.0,                       // max acceleration
        "bmax" -> -1.5,                      // max deceleration
        "v0"   -> 4.0,                       // starting velocity // v0 should be adjustable to 0
        "vmax" -> 33.528,                    // max velocity
        "T"    -> 3.0,                       // safe min time headway
        "s"    -> 5.0,                       // safe min distance headway
        "len"  -> 4.0,                       // length of the vehicles
        "del"  -> 4.0)                       // acceleration exponent (delta)

    /** current values for driver/vehicle characteristics/properties
     */
    private [process] var prop = def_prop


    //    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current value for 'prop("rt") the driver reaction time
     */
    inline def rt: Double   = prop("rt")                     // driver reaction time
    inline def amax: Double = prop("amax")                   // max acceleration
    inline def bmax: Double = prop("bmax")                   // max deceleration
    inline def v0: Double   = prop("v0")                     // starting velocity
    inline def vmax: Double = prop("vmax")                   // max velocity
    inline def T: Double    = prop("T")                      // min time headway
    inline def s: Double    = prop("s")                      // min distance headway
    inline def len: Double  = prop("len")                    // length of the vehicles
    inline def del: Double  = prop("del")                    // acceleration exponent (delta)


    def setInitialSpeed(v0: Double):Unit =
        prop("v0")  = v0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the driver/vehicle characteristics/properties to the new property values.
     *  @param new_prop  the new property values
     */
    private def setProps (new_prop: Map [String, Double]): Unit = prop = new_prop

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Butcher's Method (fifth order) for numerically solving an ordinary differential equation.
     *  @param Ft     the "original" function value at time t
     *  @param ft     the "derivative" function value at time t
     *  @param ft_rt  the "derivative" function value at time t - rt
     *  @param rt     the time difference (reaction time)
     *
     *  FIX - integrate into Dynamics package
//     */

    def butcher(Ft: Double, ft: Double, ft_rt: Double, rt: Double): Double =
        val _1_by_90 = 1.0 / 90.0

        val k1 = ft_rt
        //val k2 = ft_rt + 0.25 * (ft - ft_rt)
        val k3 = ft_rt + 0.25 * (ft - ft_rt)
        val k4 = ft_rt + 0.50 * (ft - ft_rt)
        val k5 = ft_rt + 0.75 * (ft - ft_rt)
        val k6 = ft

        Ft + _1_by_90 * (7 * k1 + 32 * k3 + 12 * k4 + 32 * k5 + 7 * k6) * rt

    end butcher


end Vehicle




//    //Return the position of the vehicle on the lane
//    def getPosOnLane(): Double = t_disp
//
//    //Return the length of the lane
//    def getLaneLength(lane: Int): Double = lane
//
//    //Return the speed of the vehicle
//    def getVehicleAhead(vehicle: Vehicle): Vehicle = vehicle
//
//    //Return the vehicle behind using the B+Tree
//    //Get your position in the B+tree lane, then check behind you for
//    //the vehicle behind you on same lane
//    def getVehicleBehind(vehicle: Vehicle): Vehicle = vehicle
//
//    //Return true if a vehicle needs to change lane
//    def needLaneChange(vehicle: Vehicle): Boolean = false


