
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
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
/** The `DoubleLinked` class provides a data structure implementing mutable doubly-linked
 *  lists.
 *      succ                    -->    -->
 *      tail (last car) --> [e1]   [e2]   [e3] <-- head (lead car)
 *      pred                    <--    <--
 *  @tparam A  the type of the elements/values in the list
 */
class DoublyLinkedList [A: ClassTag]
    extends AbstractIterable [A]
        with Serializable:

    private val debug = debugf ("DoublyLinkedList", true)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Node` inner case class wraps elements in nodes for double linkage.
     *  @param elem  the element
     *  @param pred  the predecessor node
     *  @param succ  the successor node
     */
    case class Node (elem: A, var pred: Node, var succ: Node):
        def next: Node = succ
        def prev: Node = pred
        override def toString: String = s"Node ($elem)"
    end Node

    private var head_ : Node = null                                 // head node (lead car)
    private var tail_ : Node = null                                 // tail node (last car)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `NodeIterator` inner class supports iterating over all the nodes in this list.
     *  @param ns  the starting node (defaults to tail)
     */
    class NodeIterator (ns: Node = tail_) extends Iterator [Node]:
        var n = ns
        def hasNext: Boolean = n != null
        def next (): Node = { val n_ = n; n = n.succ; n_ }
    end NodeIterator

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an iterator for retrieving all the nodes in this list.
     *  @see scala.collection.IterableOnce
     */
    def nodeIterator: Iterator [Node] = new NodeIterator ()

    def getPred(nn: Node) : Node = nn.prev

    def getSucc(nn: Node ) : Node = nn.succ


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `ListIterator` inner class supports iterating over all the elements in this list.
     *  @param ns  the starting leaf node (defaults to tail)
     */
    class ListIterator (ns: Node = tail_) extends Iterator [A]:
        var n = ns
        def hasNext: Boolean = n != null
        def next (): A = { val n_ = n; n = n.succ; n_.elem }
    end ListIterator

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an iterator for retrieving all the elements in this list.
     *  @see scala.collection.IterableOnce
     */
    def iterator: Iterator [A] = new ListIterator ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retreive the element in node nn (e.g., the current car).
     *  @param nn  the node containing the sought element
     */
    def elemAt (nn: Node): A = nn.elem

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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the first element to an empty list and return the new node nn.
     *  @param elm  the element to be added
     */
    def addFirst (elm: A): Node =
        val nn = Node (elm, null, null)                             // make a new node nn
        head_  = nn
        tail_  = nn
        nn
    end addFirst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the new element into the list AFTER the given predecessor node pn and
     *  return the new node nn.
     *  Relink:  _pn_ <-> sn  TO  _pn_ <-> nn <-> sn
     *  @param elm  the element to be added
     *  @param pn   the predecessor node (use head if not given)
     */
    def addAfter (elm: A, pn: Node = head_): Node =
        if pn == null || isEmpty then
            addFirst (elm)
        else
            val sn = pn.succ                                         // successor node sn
            val nn = Node (elm, pn, sn)                              // make a new node nn
            pn.succ = nn                                             // link forward
            if sn != null then sn.pred = nn                          // link backward

            if pn == head_ then head_ = nn                           // if pn was head, reset to nn
            debug ("addAfter", s"pn = $pn, nn = $nn, sn = $sn")
            nn
    end addAfter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the new element into the list BEFORE the given successor node sn and
     *  return the new node nn.
     *  Relink:  pn <-> _sn_  TO  pn <-> nn <-> _sn_
     *  @param elm  the element to be added
     *  @param sn   the successor node (use tail if not given)
     */
    def add (elm: A, sn: Node = tail_): Node =
        if sn == null || isEmpty then
            addFirst (elm)
        else
            val pn  = sn.pred                                        // predecessor node sn
            val nn  = Node (elm, pn, sn)                             // make a new node nn
            sn.pred = nn                                             // link backward
            if pn != null then pn.succ = nn                          // link forward

            if sn == tail_ then tail_ = nn                           // if sn was tail, reset to nn
            debug ("@@@@add", s"pn = $pn, nn = $nn, sn = $sn")
            nn
    end add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the node nn from the linked list.
     *  Relink:  pn <-> nn <-> sn  TO  pn <-> sn
     *  @param nn  the node to remove (unlink)
     */
    def remove (nn: Node = head_): Unit =
        val pn = nn.pred                                             // nn's predecessor
        val sn = nn.succ                                             // nn's successor
        if pn != null then pn.succ = sn                              // forward bypass of nn
        if sn != null then sn.pred = pn                              // backward bypass of nn

        if nn == head_ then head_ = pn                               // if nn was head, reset to pn
        if nn == tail_ then tail_ = sn                               // if nn was tail, reset to sn

        nn.pred = null                                               // nn no longer links
        nn.succ = null
        debug ("remove", s"pn = $pn, sn = $sn")
    end remove



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    // pn <--> nn <--> sn

    /**
     *    nn.pred = pn    pn.succ= nn
     *    nn.succ = sn    sn.prev = nn
     * @return
     */
    // need to test this logic if it works
    def addBefore(elm: A, sn: Node): Node =
        if sn == null then addFirst(elm)

        val nn = Node(elm, sn.pred, sn)
        if sn.pred != null then
            sn.pred.succ = nn
        sn.pred = nn
        debug("addBefore", s"elm = $elm, inserted before = ${sn.elem}")
        nn
    end addBefore

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear the list of all nodes (and their elements).
     */
    def clear (): Unit = { tail_ = null; head_ = null }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this doubly linked list to a string.
     */
    override def toString (): String =
        val sb = StringBuilder ("DoublyLinkedList (tail -")
        for n <- nodeIterator do sb.append (s"> [ $n ] <-")
        sb.append (" head)").mkString
    end toString



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    /** Convert the elements of this doubly linked list to a Scala List.
     * This method is efficient in terms of maintaining the correct order without
     * needing a separate reverse at the end.
     */
    override def toList: List[A] =
        val buf = new scala.collection.mutable.ListBuffer[A]() // use ListBuffer for efficient appends
        for n <- nodeIterator do // traverse using the predefined nodeIterator
            buf += n.elem
        end for
        buf.toList // convert ListBuffer to List
    end toList


end DoublyLinkedList


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `doublyLinkedListTest` main function tests the `DoublyLinkedList` class.
 *  > runMain scalation.doublyLinkedListTest
 */
@main def doublyLinkedListTest (): Unit =

    banner ("Test the add method")
    val dll = DoublyLinkedList [Int] ()
    for i <- 0 until 10 do dll.add (i)
    val nn = dll.headNode
    println (s"@@@nn=  $nn")
    println(dll.getSucc(nn))

    banner ("Test the addAfter method")
    dll.clear ()
    for i <- 0 until 10 do dll.addAfter (i)

//
//    banner("Test the addBefore method")
//    dll.clear()
//    val initialNode = dll.addFirst(10) // Start by adding an initial node to reference
//    for i <- 1 until 10 do
//        dll.addBefore(i, initialNode) // Add before the initial node

    //    banner (dll.getPred(i))
//    banner(dll.getSucc(i))

    banner ("Test the remove method")
    while ! dll.isEmpty do
        dll.remove ()
        println (dll)

end doublyLinkedListTest




