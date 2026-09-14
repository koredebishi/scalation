//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SignalControl` trait defines optional junction admission control.
 *  The control device owns timing, release policy and animation; the junction
 *  applies the waiting rule when the device is shut.
 */
package scalation
package simulation
package process

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A local control device for a junction approach.
 */
trait SignalControl:

    /** Return whether the controlled movement is currently closed. */
    def shut: Boolean

    /** Return the queue in which a closed movement waits. */
    def line: WaitQueue

end SignalControl
