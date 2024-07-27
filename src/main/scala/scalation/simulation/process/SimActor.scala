

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Active Entity/Simulation Actor
 */

package scalation
package simulation
package process

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimActor` abstract class represents entities that are active in the model.
 *  The act abstract method, which specifies entity behavior, must be defined
 *  for each subclass.  Each `SimActor` extends ScalaTion's `Coroutine` class and
 *  may be  roughly thought of as running in its own thread.
 *  @param label     the label/name of the entity (`SimActor`)
 *  @param director  the director controlling the model
 *  @param prop      the properties (Map) for this actor, e.g., speed, color, weight
 */
abstract class SimActor (label: String, director: Model,
                         val prop: Map [String, Double] = null)
    extends Coroutine (label)
        with Temporal
        with Ordered [SimActor]
        with Locatable:

    name = label                                                  // set the name for this entity `SimActor`

    private val flaw = flawf ("SimActor")                         // flaw function

    var nextTransport: Transport = null                           // next `Transport` to move along for this entity `SimActor`
    // must be specified, e.g.,, before entering a bus (PUBLIC access required)
    var subtype = 0                                               // indicator of entity subtype `SimActor`, e.g., for turning choices (PUBLIC)

    private [process] var arrivalT = director.clock               // time at which this entity `SimActor` arrived
    private [process] var mySource: Source = null                 // `Source` that created this entity `SimActor`
    private [process] var myNode: SimActor.alist.Node = null      // my (the actor's) node in the ACTOR LIST pred <-> me <-> succ

    private var _trajectory = 0.0                                 // value of the trajectory along `QCurve` for this entity `SimActor`

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the current trajectory (along the `QCurve`) of this `SimActor`.
     */
    def trajectory: Double = _trajectory

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the trajectory along the `QCurve` for this `SimActor`.
     *  @param t  the new trajectory for the `SimActor`
     */
    def trajectory_= (trajectory: Double): Unit =
        if trajectory >= 0.0 then _trajectory = trajectory
        else flaw ("trajectory_=", "the trajectory can not be negative")
    end trajectory_=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the activation times of the two actors, this and actor2.
     *  Their activation times are used to order them in the director's agenda
     *  (a time-based priority queue).
     *  @param actor2  the other actor to compare with this
     */
    def compare (actor2: SimActor): Int = actor2.actTime compare actTime

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The abstract method, 'act', is defined in each subclass to provide specific
     *  behavior.
     */
    def act (): Unit = println ("SimActor.act method should be overridden")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Schedule a reactivation of this `SimActor` delay time units in the future.
     *  @param delay  the time delay before reactivation
     */
    def schedule (delay: Double): Unit =
        actTime = director.clock + delay
        director.reschedule (this)
    end schedule

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Yield control to the director so the director can take the next action.
     *  @param quit  the flag indicating whether this actor is done
     */
    def yieldToDirector (quit: Boolean = false): Unit =
        director.log.trace (this, "resumes", director, director.clock)
        yyield (director, quit)
    end yieldToDirector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the `SimActor`s full name and activation time.
     */
    override def toString: String = s"SimActor ($me with cor_id $id at $actTime)"

end SimActor


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimActor` companion object holds the ACTOR LIST and provides methods for adding
 *  and removing actors from the list.  These methods should be called in the application
 *  models to give users full control.  For example, in a traffic model, if a car stays
 *  in the same lane over a complete `Route` (multiple road segments), the car should
 *  be added to the alist at the beginning of the route and removed only at the end.
 *  Any lane changes or turns will require changes in the actor list.
 *  @see `myNode` in `SimActor` class.
 *  @see `routeTest` main function for example of use of alist.
 */
object SimActor:

    val alist = DoublyLinkedList [SimActor] ()              // actor list for ordering of actors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the given actor AFTER the other actor in the alist, e.g., when beginning
     *  a Route/VTransport.
     *  @param actor  the given actor/vehicle to add
     *  @param other  the other actor/vehicle (the one ahead, null if none)
     */
    def addToAlist (actor: SimActor, other: SimActor): Unit =
        val new_node = alist.add(actor)
        actor.myNode = new_node
        val other_node = if actor.myNode != null then alist.getSucc(actor.myNode) else null
        println(s"${Console.RED} adding car: ${actor.myNode}, other_node = $other_node, vlist=$alist ${Console.RESET}")
    end addToAlist

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the given actor from the alist, e.g., because of termination, lane change,
     *  or turn.
     *  @param actor  the given actor/vehicle to remove
     */
    def removeFromAlist (actor: SimActor): Unit =
        alist.remove (actor.myNode)
        //println(s"${Console.RED} vlist $alist ${Console.RESET}")
    end removeFromAlist


    /**
     * @param disp: The current displacement of the actor, also the key
     * insertion reference
     * get other Cars displacement that is just less than the disp
     * get the Cars displacement that is just higher than the disp
     * insert disp (car ref) between this nodes
     * @param actor The actor that needs to make the lane change
     */
    def addBeforeAlist(actor: SimActor, l2:Int, lane:Array[VTransport]): Unit =
        println(s"The method to add actor $actor from exact displacement location")


//        // Find the car with t_disp just larger than actor.t_disp
//        val succCar = rangeCar.collect { case (disp, car) if disp > actor.asInstanceOf[Vehicle].t_disp => car}.minByOption(_.t_disp)
//
//        // Add the actor to the doubly linked list before the successor car
//        if succCar.isDefined then
//            actor.myNode = lane(l2).vList.addBefore(actor.asInstanceOf[Vehicle], succCar.get.myNode.succ)
//        // If there is no successor car, add the actor to the end of the list
//        else actor.myNode = lane(l2).vList.add(actor)
    // Add the actor to the global DLL in the correct position

        // to find out where you are use the B+tree
        // to keep track of the car ahead of you use the DoublyLinkList
        // insert based on the pn<--->nn<--->sn

        // for each lanes make


        //What car is ahead of me
        //val carMin = {rangeCar.minBy { case (k, _) => (k.asInstanceOf[Double] - actor.asInstanceOf[Vehicle].t_disp).abs }._2}
    end addBeforeAlist


end SimActor
