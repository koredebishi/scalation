
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Korede Bishi
 *  @version 2.0
 *  @date    Sun Feb 25 20:55:28 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Data Structure: Doubly Linked List
 */

package scalation


//import scalation.simulation.process.{SimActor, Vehicle}

import scala.collection.mutable.AbstractIterable
import scala.reflect.ClassTag


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DoublyLinkedList` class provides a data structure implementing mutable doubly-linked lists.
 *      behind                    -->    -->
 *      tail (last car) --> [e1]   [e2]   [e3] <-- head (lead car)
 *      ahead                    <--    <--
 *  @param A  the type of the elements/values in the list
 */
class DoublyLinkedList [A: ClassTag]
  extends AbstractIterable [A]
    with Serializable:

    private val debug = debugf ("DoublyLinkedList", true)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Node` inner case class wraps elements in nodes for double linkage.
     *  @param elem  the element
     *  @param ahead  the predecessor node (e.g., the car ahead)
     *  @param behind  the successor node   (e.g., the car behind)
     */
    case class Node (elem: A, var ahead: Node, var behind: Node):
        override def toString: String =
            val behind_elem = if behind == null then null else behind.elem
            val ahead_elem = if ahead == null then null else ahead.elem
            //s"[[ Node_behind ( $behind_elem ) ----> Node_current ($elem) -----> Node_ahead ( $ahead_elem )]]"
            s"[[ Node_current ($elem)]]"
    end Node

    private var head_ : Node = null                                 // head node (lead car)
    private var tail_ : Node = null                                 // tail node (last car)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::fixed
    /** The `NodeIterator` inner class supports iterating over all the nodes in this list.
     *  @param ns  the starting node (defaults to tail)
     */
//    class NodeIterator (ns: Node = tail_) extends Iterator [Node]:
//        var n = ns
//        def hasNext: Boolean = n != null           //hasBehind
//        def next (): Node = { val n_ = n; n = n.behind; n_ }
//    end NodeIterator

    class NodeIterator(ns: Node = head_) extends Iterator[Node]:
        var n = ns
        def hasNext: Boolean = n != null
        def next(): Node =
            val cur = n
            n = n.behind
            cur
    end NodeIterator



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::fixed
    /** Return an iterator for retrieving all the nodes in this list.
     *  @see scala.collection.IterableOnce
     */
    def nodeIterator: Iterator [Node] = new NodeIterator ()

    def getAhead(n: Node) : Node = n.ahead

    def getBehind(n: Node ) : Node = n.behind


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::fixed
    /** The `ListIterator` inner class supports iterating over all the elements in this list.
     *  @param ns  the starting leaf node (defaults to tail)
     */
    class ListIterator (ns: Node = tail_) extends Iterator [A]:
        var n = ns
        def hasNext: Boolean = n != null
        def next (): A = { val n_ = n; n = n.behind; n_.elem }
    end ListIterator


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an iterator for retrieving all the elements in this list.
     *  @see scala.collection.IterableOnce
     */
    def iterator: Iterator [A] = new ListIterator ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::fixed
    /** Retreive the element in node n (e.g., the current car).
     *  @param n  the node containing the sought element
     */
    def elemAt (n: Node): A = n.elem

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the lead/first node in the list (e.g, node holding the lead car).
     */
    override def head: A = head_.elem

    def headNode: Node = head_


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the trail/last node in the list (e.g, node holding the trail car).
     */
    override def last: A = tail_.elem

    def lastNode: Node = tail_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether the list is empty (head and tail are null).
     */
    override def isEmpty: Boolean = head_ == null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::fixed
    /** Add the first element to an empty list and return the new node n.
     *  @param elm  the element to be added
     *  @return the new node added n
     */
    def addFirst(elm: A): Node =
        val n = Node(elm, null, head_)                              // new node has no predecessor, and its behind is the current head
        if head_ != null then                                       // if list is not empty
            head_.ahead = n                                          // update the aheadious head's ahead to point to the new node
        head_ = n                                                   // update head to point to the new node
        if tail_ == null then                                       // if the list was empty (tail is null)
            tail_ = n                                               // set tail to the new node
        debug("addFirst", s"Added node $n as the first element")
        n
    end addFirst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the new element into the list BEFORE the given successor node `nn` and
     * return the new node `n`.
     * Relink:  pn <-> _nn_  TO  nn <-> n <-> _pn_
     *
     * @param elm the element to be added
     * @param nn  the successor(next or car_behind) node (defaults to tail if not given)????
     * @param pn  the predecessor (the car_ahead)
     * @return the new node `n`
     *          //this implementation is rightly identifying the head node as the first entered node which is correct
     *          //however, the node behind and ahead pointer reference is interchanges.
     *          // the forward pointer is the backward pointer and the backward pointer is the forward pointer
     *          // a call to getAhead points backward: this should point forward
     *          // a call to getBehind points forward: this should point backward
     *          //Similarly ahead is behind and behind is ahead for this implementation
     *          // all linkages are perfect and do not need to change. only need to
     *          // make the swap of ahead and behind so that a call to them returns the right thing.
     *          eg: tail:->:9-----8-----7-----6-----5-----4----3----2----1----0<--head:
     *          currently a call to dll.headNode returns node:0 this is correct
     *          also a call to dll.getAhead(n) returns 1. this is wrong; should be null
     *          Also a call to dll.getBehind(n) returns null: this should be Node 1.
     *          //I do not think this add method is the problem or maybe at the minimum might need a small tweak
     *          //I do think that the issue lies somewhere on what is recognize as the forward and backward pointer
     *          // I do think we might change the name and adjust the ahead and behind naming with it's accessors that
     *          // infers this twisted naming so that a call to them can be the right thing.
//     */

//    def add(elm: A, nn: Node = tail_): Node =     // entering from tail which is what we want
//        println(s"Adding: $elm to the list")
//        if nn == null || isEmpty then
//            addFirst(elm)
//        else
//            val pn = nn.ahead // predecessor node pn
//            val n = Node(elm, pn, nn) // make a new node n
//            nn.ahead = n // link backward
//            if pn != null then pn.behind = n // link forward
//            if nn == tail_ then tail_ = n // if nn was tail, reset to n
//            debug("add", s"pn = $pn, n = $n, nn = $nn")
//            n
//    end add
    def add(elm: A, nn: Node = tail_): Node =
        println(s"Adding: $elm to the list with reference node = $nn")

        if isEmpty || nn == null then
            addFirst(elm) // Case 1: List is empty or no reference node
        else
            val pn = nn.behind // Correct reference to predecessor (should be the node behind nn)
            val n = Node(elm, nn, pn) // New node is inserted with nn ahead and pn behind

            if pn != null then pn.ahead = n // Fix ahead linkage of previous node
            nn.behind = n // Fix behind linkage of the reference node

            if nn == tail_ then tail_ = n // Update tail if inserting at the end

            debug("add", s"pn = $pn n=$n ,  $nn")
            n
    end add



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Add the new element BEFORE the given successor node `nn` and return the new node `n`.
     * Relink:  pn <-> nn  TO  pn <-> n <-> nn
     * The predecessor (`pn`) of the successor node `nn` is relinked to point to the new node `n`.
     * Similarly, the new node `n` links back to `pn` and forward to `nn`. If `nn` is `null`,
     * this method adds the element as the first element in the list.
     *
     * @param elm the element to be added
     * @param nn  the successor node (defaults to `null` if not provided)
     * @return the newly created node `n` inserted before node `nn`
     */
//    def addBefore(elm: A, nn: Node): Node =
//        if nn == null || nn == head_ then
//            addFirst(elm) // If nn is null or nn is head, insert at the front
//        else
//            val pn = nn.behind // Correct predecessor (the node behind nn)
//            val n = Node(elm, pn, nn) // New node with correct pointers
//
//            if pn != null then pn.ahead = n // Fix ahead linkage of previous node
//            nn.behind = n // Update nn’s behind pointer to point to the new node
//            debug("addBefore", s"Inserted node $n between $pn and $nn")
//            n
//    end addBefore

    def addBefore(elm: A, pn: Node): Node =
        val nn = pn.behind // Get the behind node (car behind `pn`)

        if nn == null then
            // Case 1: `pn` is the head, so insert BEHIND it and assume tail.
            val n = Node(elm, pn, null) // New node's ahead = pn, behind = null
            pn.behind = n // Fix behind linkage
            tail_ = n // Update the tail pointer
            debug("addBefore", s"Inserted node $n behind head $pn (new tail)")
            n
        else
            // Case 2: `pn` has a behind node (normal case, inserting between two nodes)
            val n = Node(elm, pn, nn) // Insert between `pn` (ahead) and `nn` (behind)
            pn.behind = n // Fix pn's behind pointer
            nn.ahead = n // Fix nn's ahead pointer
            debug("addBefore", s" pn= $pn n=$n and $nn")
            n
    end addBefore





    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Remove the node `n` from the linked list.
     * Relink:  pn <-> n <-> nn  TO  pn <-> nn
     * ReLink should be nn <-> n <-> pn TO nn <-> pn // desired effect
     * @param n the node to remove (unlink)
     */
    def remove(n: Node = head_): Unit =
        val pn = n.ahead                                 // predecessor node pn // car ahead
        val nn = n.behind                                 // successor node nn  // car behind

        if pn != null then pn.behind = nn                 // forward bypass of n
        if nn != null then nn.ahead = pn                 // backward bypass of n

        if n == head_ then head_ = nn                   // if n was head, reset to nn
        if n == tail_ then tail_ = pn                   // if n was tail, reset to pn

        n.ahead = null                                   // n no longer links
        n.behind = null
        debug("remove", s"pn = $pn, nn = $nn")
    end remove



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear the list of all nodes (and their elements).
     */
    def clear (): Unit = { tail_ = null; head_ = null }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this doubly linked list to a string.
     */
//    override def toString (): String =
//        val sb = StringBuilder ("DoublyLinkedList (tail -")
//        for n <- nodeIterator do sb.append (s"> [ $n ] <-")
//        sb.append (" head)").mkString
//    end toString

    override def toString(): String =
        val sb = StringBuilder("DoublyLinkedList (head -> ")
        for n <- nodeIterator do sb.append(s"[ $n ] -> ")
        sb.append("tail)")
        sb.mkString
    end toString




    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Convert the elements of this doubly linked list to a Scala List.
     * This method is efficient in terms of maintaining the correct order without
     * needing a separate reverse at the end.
     */
    override def toList: List[A] =
        val buf = new scala.collection.mutable.ListBuffer[A]()      // use ListBuffer for efficient appends
        for n <- nodeIterator do                                    // traverse using the aheadefined nodeIterator
            buf += n.elem
        end for
        buf.toList                                                  // convert ListBuffer to List
    end toList


end DoublyLinkedList


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `doublyLinkedListTest` main function tests the `DoublyLinkedList` class.
 *  > runMain scalation.doublyLinkedListTest
 */
@main def doublyLinkedListTest (): Unit =

    banner ("Test the add method")
    val dll = DoublyLinkedList [Int] ()
    for i <- 0 until 10 do
        if dll.isEmpty then dll.addFirst(i)
        else dll.add (i)

    banner ("Test the remove method")
    while ! dll.isEmpty do
        dll.remove ()
        println (dll)

end doublyLinkedListTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `doublyLinkedListTest` main function tests the `DoublyLinkedList` class.
 *  > runMain scalation.doublyLinkedListTest
 */
@main def doublyLinkedListTest2 (): Unit =

    banner ("Test the add method")
    val dll = DoublyLinkedList [Int] ()
    for i <- 0 until 10 do dll.add(i)
    val n = dll.headNode
    println (s"n is the head node: = $n")
    println (s"the node behind n  is: ${dll.getBehind (n)}")
    println(s"the node ahead of n is: ${dll.getAhead(n)}")
    println(dll)
//
//    val b = n.behind
//    println(s"b is the head node: = $b")
//    println(s"the node behind b  is: ${dll.getBehind(b)}")
//    println(s"the node ahead of b is: ${dll.getAhead(b)}")
//    println(dll)



//    banner ("Test the addAfter method")
//    dll.clear ()
//    for i <- 0 until 10 do dll.addAfter (i)
//    println(dll)
//
//
//    banner ("Test the addBefore method")
//    dll.clear ()
//    val initialNode = dll.addFirst (10)            // start by adding an initial node to reference
//    for i <- 1 until 10 do
//        dll.addBefore (i, initialNode)             // add before the initial node
//    println(dll)
//
    banner ("Test the remove method")
    while ! dll.isEmpty do
        dll.remove ()
        println (dll)

end doublyLinkedListTest2

@main def doublyLinkedListTest3(): Unit =

    banner("Test the add and addBefore methods")

    // Create DoublyLinkedList
    val dll = DoublyLinkedList[Int]()

//    // Insert 10 elements into the list using `add`
//    for i <- 0 until 10 do dll.add(i)
//
//    // Validate head node
//    val n = dll.headNode
//    println(s"n is the head node: = $n")
//    println(s"the node behind n is: ${dll.getBehind(n)}")
//    println(s"the node ahead of n is: ${dll.getAhead(n)}")
//    println(dll)
//
//    // Validate second node
//    val b = n.behind
//    println(s"b is the second node: = $b")
//    println(s"the node behind b is: ${dll.getBehind(b)}")
//    println(s"the node ahead of b is: ${dll.getAhead(b)}")
//    println(dll)
//
//    // --- Test Case: Insert Before Head ---
//    println(s"\nTesting addBefore: Insert before head ${dll.headNode}")
//    dll.addBefore(-1, dll.headNode)  // Should now be the new head
//    println(dll)
//
//    // --- Test Case: Insert in Between Two Nodes ---
//    println("\nTesting addBefore: Insert between nodes")
//    val node4 = dll.headNode.behind.behind.behind.behind // Get Node(4)
//    println(s"\nTesting addBefore: Insert between nodes $node4")
//    dll.addBefore(99, node4)  // Insert 99 before Node(4)
//    println(dll)
//
//    // --- Test Case: Insert Before Tail ---
//    println(s"\nTesting addBefore: Insert 88 before tail ${dll.lastNode}")
//    dll.addBefore(88, dll.lastNode)  // Insert 88 before the tail
//    println(dll)
//
//    // Final state of the list
//    println("\nFinal DoublyLinkedList:")
//    println(dll.lastNode.behind)
//    println(dll.lastNode.ahead)
//    println(dll)

    //val dll = DoublyLinkedList[Int]()
    dll.add(98) // DLL now contains [ 98 ] (single node)
    val head = dll.headNode
    dll.addBefore(99, head) // Insert 99 before Node(98)
    dll.addBefore(95, head) // Insert 99 before Node(98)
    println(dll)

@main def doublyLinkedListTest4(): Unit =
    banner("Test the add and addBefore methods")

    // Case 1: Insert normally at the tail (Default behavior)
    val dll = DoublyLinkedList[Int]()
    for i <- 0 until 5 do dll.add(i)
    println("After normal insertion:")
    println(dll)

    // Case 2: Insert before a given node (Middle of list)
    val refNode = dll.headNode.behind // Second node in the list
    println(s"Inserting before $refNode")
    dll.addBefore(99, refNode)
    println("After inserting 99 before the second node:")
    println(dll)

    // Case 3: Insert before head (Becomes new head)
    dll.addBefore(77, dll.headNode)
    println("After inserting 77 before head:")
    println(dll)

    // Case 4: Insert when list is empty (Should work with addFirst)
    val emptyDll = DoublyLinkedList[Int]()
    emptyDll.addBefore(55, null)
    println("After inserting 55 into empty list:")
    println(emptyDll)
































