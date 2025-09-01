package scalation
package simulation
package process

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Trait for any component that maintains an ordered list of vehicles.
 *  Provides common API for inserting/removing vehicles and querying ends.
 *  Example implementers: Pathway, Ramp.
 */
trait Joinable extends Component:

    def addToAlist(actor: Vehicle, other: Vehicle): Unit

    def removeFromAlist(actor: Vehicle): Unit

    def getFirst: Vehicle

    def getLast: Vehicle
    
end Joinable
