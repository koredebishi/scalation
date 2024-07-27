//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** @author  John Miller, Casey Bowman
// *  @version 2.0
// *  @date    Tue Feb  4 14:56:34 EST 2020
// *  @see     LICENSE (MIT style license file).
// *
// *  @note    Vehicle Is Enhanced SimActor Supporting Changing Velocities
// */
//
//package scalation
//package simulation
//package process
//
//
//import scala.collection.immutable.Map
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** The `Vehicle` class extends the `SimActor` and represents a vehicle in motion.
// *  @param name_     the name of the vehicle
// *  @param director  the model to which this vehicle belongs
// */
//abstract class Vehicle (name_ : String, director: Model)
//    extends SimActor (name_, director)
//        with Dynamics:
//
//    //var myNode: DoublyLinkedList[Vehicle]#Node = _
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** The abstract method, 'act', is defined in each subclass to provide specific
//     *  behavior.
//     */
//    override def act (): Unit = println ("Vehicle.act method should be overridden")
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Show the `SimActor`s full name and activation time.
//     */
//    override def toString: String = s"Vehicle ($me at $actTime :sec with cor_id= $id and location (disp) $disp :m )"
//
//
//end Vehicle
//
//
//
////::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
///** The `Vehicle` object contains driver/vehicle characteristics/properties.
// */
//object Vehicle:
//
//    /** defaults values for driver/vehicle characteristics/properties (PUBLIC access required)
//     *  @see https://en.wikipedia.org/wiki/Intelligent_driver_model
//     */
//    val def_prop = Map ("rt"   -> 1.0,                       // driver reaction time
//        "amax" -> 2.0,                       // max acceleration
//        "bmax" -> -1.5,                      // max deceleration
//        "v0"   -> 0.0,                       // starting velocity
//        "vmax" -> 33.528,                    // max velocity
//        "T"    -> 3.0,                       // safe min time headway
//        "s"    -> 5.0,                       // safe min distance headway
//        "len"  -> 4.0,                       // length of the vehicles
//        "del"  -> 4.0)                       // acceleration exponent (delta)
//
//    /** current values for driver/vehicle characteristics/properties
//     */
//    private [process] var prop = def_prop
//
//    //val vlist = DoublyLinkedList[Vehicle]()
//
//
////    /** Add the given actor AFTER the other actor in the alist, e.g., when beginning
////     *  a Route/VTransport.
////     *  @param actor  the given actor/vehicle to add
////     *  @param other  the other actor/vehicle (the one ahead, null if none)
////     */
////    def addToAlist (actor: Vehicle): Unit =
////        //val other_node = if other != null then other.myNode else null
////        actor.myNode   = vlist.add(actor)
////    end addToAlist
//
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Remove the given actor from the alist, e.g., because of termination, lane change,
//     *  or turn.
//     *  @param actor  the given actor/vehicle to add
//     */
////    def removeFromAlist (actor: Vehicle): Unit =
////        actor.myNode = vlist.remove(actor)
////    end removeFromAlist
////
////    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Return the current value for 'prop("rt") the driver reaction time
//     */
//    inline def rt: Double   = prop("rt")                     // driver reaction time
//    inline def amax: Double = prop("amax")                   // max acceleration
//    inline def bmax: Double = prop("bmax")                   // max deceleration
//    inline def v0: Double   = prop("v0")                     // starting velocity
//    inline def vmax: Double = prop("vmax")                   // max velocity
//    inline def T: Double    = prop("T")                      // min time headway
//    inline def s: Double    = prop("s")                      // min distance headway
//    inline def len: Double  = prop("len")                    // length of the vehicles
//    inline def del: Double  = prop("del")                    // acceleration exponent (delta)
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Set the driver/vehicle characteristics/properties to the new property values.
//     *  @param new_prop  the new property values
//     */
//    def setProps (new_prop: Map [String, Double]): Unit = prop = new_prop
//
//    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
//    /** Butcher's Method (fifth order) for numerically solving an ordinary differential equation.
//     *  @param Ft     the "original" function value at time t
//     *  @param ft     the "derivative" function value at time t
//     *  @param ft_rt  the "derivative" function value at time t - rt
//     *  @param rt     the time difference (reaction time)
//     *
//     *  FIX - integrate into Dynamics package
//     */
//    def butcher (Ft: Double, ft: Double, ft_rt: Double, rt: Double): Double =
//        val _1_by_9 = 1.0 / 9.0
//        val k1 = ft_rt
//        val k3 = ft_rt + 0.25 * (ft - ft_rt)
//        val k4 = ft_rt + 0.50 * (ft - ft_rt)
//        val k5 = ft_rt + 0.75 * (ft - ft_rt)
//        val k6 = ft
//        Ft + _1_by_9 * (7 * k1 + 32 * k3 + 12 * k4 + 32 * k5 + 7 * k6) * rt
//    end butcher
//
//    //    def butcher(Ft: Double, ft: Double, ft_rt: Double, rt: Double): Double =
//    //        val _1_by_90 = 1.0 / 90.0
//    //        val k1 = ft_rt
//    //        val k3 = ft_rt + (1.0 / 14.0) * (ft - ft_rt)
//    //        val k4 = ft_rt + (1.0 / 12.0) * (ft - ft_rt)
//    //        val k5 = ft_rt + (3.0 / 4.0) * (ft - ft_rt)
//    //        val k6 = ft
//    //        Ft + _1_by_90 * (7 * k1 + 32 * k3 + 12 * k4 + 32 * k5 + 7 * k6) * rt
//    //    end butcher
//
//end Vehicle


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


import scala.collection.immutable.Map
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

   private[process] var myCarNode: Vehicle.vlist.Node = null


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The abstract method, 'act', is defined in each subclass to provide specific
     *  behavior.
     */
    override def act (): Unit = println ("Vehicle.act method should be overridden")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the `SimActor`s full name and activation time.
     */
    override def toString: String = s"Vehicle ($me at $actTime :sec with cor_id= $id and location (disp) $disp :m )"


end Vehicle



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vehicle` object contains driver/vehicle characteristics/properties.
 */
object Vehicle:

    val vlist = new DoublyLinkedList[Vehicle]() // DoublyLinkList for keeping track of cars ahead

    val speedGen = Uniform(20.0 , 40.0)

    /** defaults values for driver/vehicle characteristics/properties (PUBLIC access required)
     *  @see https://en.wikipedia.org/wiki/Intelligent_driver_model
     */


    val def_prop = Map ("rt"   -> 1.0,                       // driver reaction time
        "amax" -> 2.0,                       // max acceleration
        "bmax" -> -1.5,                      // max deceleration
        "v0"   -> 0.0,                       // starting velocity
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
    //inline def vmax: Double = prop("vmax")                   // max velocity
    inline def T: Double    = prop("T")                      // min time headway
    inline def s: Double    = prop("s")                      // min distance headway
    inline def len: Double  = prop("len")                    // length of the vehicles
    inline def del: Double  = prop("del")                    // acceleration exponent (delta)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the driver/vehicle characteristics/properties to the new property values.
     *  @param new_prop  the new property values
     */
    def setProps (new_prop: Map [String, Double]): Unit = prop = new_prop

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Butcher's Method (fifth order) for numerically solving an ordinary differential equation.
     *  @param Ft     the "original" function value at time t
     *  @param ft     the "derivative" function value at time t
     *  @param ft_rt  the "derivative" function value at time t - rt
     *  @param rt     the time difference (reaction time)
     *
     *  FIX - integrate into Dynamics package
//     */
    def butcher (Ft: Double, ft: Double, ft_rt: Double, rt: Double): Double =
        val _1_by_9 = 1.0 / 90.0
        val k1 = ft_rt
        val k3 = ft_rt + 0.25 * (ft - ft_rt)
        val k4 = ft_rt + 0.50 * (ft - ft_rt)
        val k5 = ft_rt + 0.75 * (ft - ft_rt)
        val k6 = ft
        Ft + _1_by_9 * (7 * k1 + 32 * k3 + 12 * k4 + 32 * k5 + 7 * k6) * rt
    end butcher


end Vehicle