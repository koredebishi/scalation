package scalation
package simulation
package process

trait RowTimeLoader:

    var curRow = 0
    var rowTime = 15 * MINUTE // row time window

   

    private[process] val ew = new EasyWriter("recorder", "rowTimeRecord.txt")
    
    def nextRow(clock: Double): Unit =
        //println(s"I was called by @@@@@@ director clock: $clock and rowTime: $rowTime")
        //ew.write(s"\n I was called by this director clock: $clock and rowtime $rowTime \n ")
        //900sec;
        if clock >= rowTime then
            curRow += 1
            rowTime += 15 * MINUTE
            ew.write(s"\n Director clock advanced: $clock and new rowtime $rowTime and curRow $curRow \n ")
            //println(s"[RowManager] Advanced to row $curRow at clock = $clock")

        end if
    end nextRow
    
//    def simRunTime(week:Int, dayOfW: Int, ts:Int, te:Int):(Int, Int)=
//        //the timeslice of Pems DataSet
//        //to be implemented 
//        //Fix <---
//        banner("The Simulation time sliced value ")
//
//        (10, 20)
//    end simRunTime    


end RowTimeLoader

