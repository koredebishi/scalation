package scalation
package simulation
package process


import scalation.mathstat._

import scala.collection.mutable.ArrayBuffer
import scalation.random.{Erlang, Exponential, Variate}

import scala.math.abs
//import scala.math.abs
import scalation.modeling.FitM





class ArrivalProcess(fileName: String,t1:Int, t2:Int,  stream:Int = 0)
    extends FitM:

    val data = MatrixD.load(fileName, t1, t2)

    /** Return the number of arrivals by time tt, i.e., N(tt)
     *
     * @param tt the inquiry time (how many arrivals by time tt)
     *           Helper method to count the number for the inquiry 15min time
     */
    def num(tt: Double, t_a: ArrayBuffer[Double]): Int =
        val i = t_a.indexWhere(_ > tt)
        if i < 0 then t_a.size else i
    end num


    /**
     * @param variate : The Variate function for this method
     * @param mu      : A Vector of mu precomputed by 15min Intensity (900sec/flow).per row data
     * @param t_span  : The time inquery for this for the Arrival time gen (15min)
     * @param stream  : The number stream to be used
     * @param row     : The current row of the data; needed to simulate each 15min data
     * @return        : A turple of (InterArrivalTime and a count of Vehicles generated): (a_t,count)
     */
    def flow(variate: Variate, mu:VectorD, t_span: Double, stream: Int, row: Int): (VectorD, Int) =
        val atime = ArrayBuffer[Double]()                                                    // arrival time bucket
        //val flw = ArrayBuffer[Int]() // flow time bucket
        var now = 0.0                                                                        // counter for now// simulating clock time to track from 0.0 to 900sec(t_span)
        var n1 = 0                                                                           // the counting for the interArrival generated (now)
        while now <= t_span do
            //println(s"the time is $now and thee mu used is ${mu(row)}, with row:$row")
            val gen = if variate.isInstanceOf[Erlang] then variate.gen1(mu(row) / 2) else variate.gen1(mu(row)) // use mu/k where k=2 here
            now += gen // accumulate the gen arrivals times
            if now <= t_span then
                atime += now
//                val n2 = num(now, atime) // helper to tally the counting for this current simulation suppose it's a full blown type
//                flw += n2 - n1
//                n1 = n2
            end if
        end while
        val t_a = VectorD(atime)             // Arrival time for this t_span (15min)
        val i_a = VectorD(t_a.indices.map{i => if i == 0 then t_a(0) else t_a(i) - t_a(i-1)})    //interArrivalTime for this t_span (15min)
        val n_v = atime.size                // count of this Arrival time // A count of the cars generated at this 15min time (t_span)
        (i_a, i_a.size)
    end flow


end ArrivalProcess



@main def plotdata(): Unit =

    //Tuesday // 4AM - Midnight

    val t1 = 2*96 + 16
    val t2 = 3*96                        // Just a few rows (for debugging)



    val run = new ArrivalProcess("/seven_sensors/402376.csv", t1, t2)


    new Plot(null, run.data(?, 2), null, "Jan1-31th")



end plotdata














@main def ArrivalProcessTest(): Unit =
//
//    val t1 = 40
//    val t2 = 65                        // Just a few rows (for debugging)


//    val t1 = 0
//    val t2 = 4                        // Just a few rows (for debugging)
//


    val t1 = 2*96 + 16
    val t2 = 3*96                        // Just a few rows (for debugging)


    val offset = t1
    var t_span = 15 * MINUTE // 900 seconds
    val stream = 0
    var curRow = t1 - offset // Used for iterating through day

    val run = new ArrivalProcess("/seven_sensors/402376.csv", t1, t2)
    val data = run.data              // MatrixD with flow data
    val nt = data.dim               // Number of 15-minute intervals

    val intervalCount = new VectorD(nt)        //15min count
    val yTrue = data(?, 2)           // True flow values (total flow column)
    val simCounts = new MatrixD(nt, 2)  // To hold [actual_flow, simulated_count]
    val simTime = nt.toDouble * t_span
    val mu = yTrue.map(t_span / _)  // vector of means per interval

    val atime = ArrayBuffer[Double]()     // All arrival times

    //val variate = Exponential()       // or new Erlang() as needed
    val variate = Erlang()

    var now = 0.0                                                                        // counter for now// simulating clock time to track from 0.0 to 900sec(t_span)
    var n1 = 0                                                                           // the counting for the interArrival generated (now)
    while now <= simTime && curRow < nt do
        //println(s"the time is $now and thee mu used is ${mu(curRow)}, with row:$curRow")
        val gen = variate.gen1(mu(curRow)/2)

        //val gen = variate.gen1(mu(curRow)) // use mu/k where k=2 here
        now += gen // new arrival time
        atime += now    // collect/store the new arrival time
        //println(s"atime: ${atime.size}")
        //if should be counting every 15min flows and returning the counts for that 15min.
        val currTime = (curRow + 1) * t_span     // intermediate timeStamp to help with tallying of counts
        if now > currTime then
            val n2 = run.num(currTime, atime)    //tally these times and return it
            intervalCount(curRow) = n2 - n1      // collect this tally here
            n1 = n2                             // reset n1
            curRow += 1                         // increment the row
        end if

    end while

    val t_a = VectorD(atime)             //total arrival for the simulation time (900* nt)
    val i_a = VectorD(t_a.indices.map{i => if i == 0 then t_a(0) else t_a(i) - t_a(i-1)})    //interArrivalTime for this simTime (900*nt)
    val n_v = t_a.size                // count of these total arrivals.
//
    //println(s"\nthe t_a: ${t_a.size}n")
//    println(s"\nthe i_a: $i_a\n")
//    println(s"\nthe n_v: $n_v\n")
    //println(s"\nthe flow: ${intervalCount.sum}\n")
//    println(s"\nthe yTrue: $yTrue\n")

    simCounts(?, 0) = yTrue
    simCounts(?, 1) = intervalCount





    val res = new VectorD(nt)
    //val res2 = new VectorD(nt)
    for i <- 0 until nt do
        //res(i) = run.smapeF(yTrue, intervalCount)

        val tes =  (simCounts(i, 0), simCounts(i, 1))
        println(s"the: yy: ${tes._1} | ys ${tes._2} | diff: ${tes._1 - tes._2}")
        res(i) = run.smapeF(VectorD(tes._1), VectorD(tes._2))
    end for



    new Plot(null, run.data(?, 2), simCounts(?, 1), "Actual vs Predicted Car Count")
    println(s"The Smape value: ${run.smapeF(run.data(?, 2), simCounts(?, 1))}")


    println(s"SMAPE Expo=========>resD:${res.mean}:")


end ArrivalProcessTest



@main def ArrivalProcessTest2(): Unit =

//    val t1 = 0           // Start of the day in seconds (e.g., 0 = 12:00am)
//    val t2 = 96       // End of the day in seconds (24 * 60 * 60)
//
//    val t1 = 40
//    val t2 = 65 // Just a few rows (for debugging)


    val t1 = 2*96 + 16
    val t2 = 3*96                        // Just a few rows (for debugging)


//    val t1 = 0
//    val t2 = 4                        // Just a few rows (for debugging)

    val offset = t1

    val timeInterval = 15 * MINUTE   // 900 seconds = 15 minutes
    var curRow = t1 - offset                  // Used for iterating through day
    val stream = 0


    val run = new ArrivalProcess("/seven_sensors/402376.csv", t1, t2)
    val data = run.data              // MatrixD with flow data

    val nt = data.dim               // Number of 15-minute intervals
    val yTrue = data(?, 2)                    // True flow values (total flow column)
    val simCounts = new MatrixD(nt, 3)        // Simulated counts per interval
    val mu = yTrue.map(timeInterval / _)       // the mu Vectors for each row. the intensity Vector.


    val resExp = new VectorD(nt)
    val resErl = new VectorD(nt)

    println(s"\nyTrue  |Expo  |expoDiff  |Erland  |erDiff")
    for i <- curRow until nt do // a for loop that runs for the interval of the number eg for a day that 96, for just 1hr that's 4

        val expoFlow = run.flow(Exponential(),mu, timeInterval, stream, i)  // 15min
        val erlangFLow = run.flow(Erlang(),mu, timeInterval, stream, i)


        simCounts(i, 0) = yTrue(i)        // Pemps
        simCounts(i, 1) = expoFlow._2          // store the count for each  15min here
        simCounts(i, 2) = erlangFLow._2        // store the couunt for each 15min here

        val yt  = simCounts(i, 0)
        val ye  = simCounts(i, 1)
        val yer = simCounts(i, 2)

        //val resid = (simCounts(i, 0) - simCounts(i, 1).map(abs) // absolute diff between ytrue and ysim
        resExp(i) = run.smapeF(VectorD(yt), VectorD(ye)) // data fitted into sMape from FitM. (Model mixed with FitM
        resErl(i) = run.smapeF(VectorD(yt), VectorD(yer))
        val dif1 = (yt - ye,yt-yer)

        println(s"\n$yt \t$ye  \t${dif1._1}     \t$yer \t${dif1._2}")
    end for





    println(s"SMAPE Expo=========>${resExp.mean}")
    println(s"SMAPE Erlang=======>${resErl.mean}")


end ArrivalProcessTest2



@main def ArrivalProcessTest3():Unit =

//    val t1 = 0
//    val t2 = 4                        // Just a few rows (for debugging)

    val t1 = 40
    val t2 = 65 // Just a few rows (for debugging)

    val offset = t1
    var t_span = 15 * MINUTE // 900 seconds
    val stream = 0
    var curRow = t1 - offset // Used for iterating through day

    val run = new ArrivalProcess("/seven_sensors/402376.csv", t1, t2)
    val data = run.data              // MatrixD with flow data
    val nt = data.dim               // Number of 15-minute intervals

    val intervalCount = new VectorD(nt)        //15min count
    val yTrue = data(?, 2)           // True flow values (total flow column)
    val simCounts = new MatrixD(nt, 2)  // To hold [actual_flow, simulated_count]
    val simTime = nt.toDouble * t_span
    val mu = yTrue.map(t_span / _)  // vector of means per interval
    val lamd = mu.map(1.0 / _)      // precomputed lamda

    val atime = ArrayBuffer[Double]()     // All arrival times


    def lamdfuc(t:Double): Double =
        val lamdd = lamd(curRow)
        lamdd
    end lamdfuc

    val pp = new NH_PoissonProcess(t_span, lamdfuc)

    var now = 0.0                                                                        // counter for now// simulating clock time to track from 0.0 to 900sec(t_span)
    var n1 = 0                                                                           // the counting for the interArrival generated (now)
    while now <= simTime && curRow < nt do
        //println(s"the time is $now and thee mu used is ${mu(curRow)}, with row:$curRow")
        val gen = pp.gen
        now += gen.last // the last arrivaltime in this bucket
        println(s"the last time now: ${now} gensize:| ${gen.size} |curRow: $curRow")
        atime ++= gen.toArray

        val currTime = (curRow + 1) * t_span     // intermediate timeStamp to help with tallying of counts
        if now > currTime then
            val n2 = run.num(currTime, atime )    //tally these times and return it
            intervalCount(curRow) = n2 - n1      // collect this tally here
            n1 = n2                             // reset n1
            curRow += 1                         // increment the row
        end if

    end while

    val t_a = VectorD(atime)             //total arrival for the simulation time (900* nt)
    val i_a = VectorD(t_a.indices.map{i => if i == 0 then t_a(0) else t_a(i) - t_a(i-1)})    //interArrivalTime for this simTime (900*nt)
    val n_v = t_a.size                // count of these total arrivals.
    //
    simCounts(?, 0) = yTrue
    simCounts(?, 1) = intervalCount


    val res = new VectorD(nt)
    //val res2 = new VectorD(nt)
    for i <- 0 until nt do
        //res(i) = run.smapeF(yTrue, intervalCount)

        val tes =  (simCounts(i, 0), simCounts(i, 1))
        println(s"the: yy: ${tes._1} | ys ${tes._2} | diff: ${tes._1 - tes._2}")
        res(i) = run.smapeF(VectorD(tes._1), VectorD(tes._2))
    end for


    println(s"SMAPE NHPP=========>resD:${res.mean}:")



end ArrivalProcessTest3





//Fix
//Make the random Variates
//I need to loop through time generating entities based upon the iArrivalTime
//In each Arrival, I will increment the counter
//Once time gets pass the end of the Interval
//Record the count and restart the counter back at 0
//ReEstimate mu as I go since mu changes.
//Continue until the end of time window


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




/**                             15min:Interval
 *   0                           4cars
 *  |--------------------------|-----------------------------------|
 *        3
 *              2    2        4    3   1    2        3         4
 *
 *
 * Interval_1 = 15min
 * total cars = 4
 * totalArrival time = 11:
 * 4 cars arrived in 11min
 *
 * Interval_2  = 15min
 * total cars = 5
 * totalArrival time = 12
 *
 *
 * Create a Vector that stores this car counts across this intervals
 * val vehicleCount = new Vector(nt): Stores the counts from nt
 * val arrivalTimePerInterval = new VectorD(nt): Store the mean or total time per intervals
 *
 *----What is the interArrival times between vehicles??
 *
 * take in our data
 *
 *
 * Counting Process that we want to use.
 *
 *1. Poisson Process
 * --- I need the gen
 * --- I need the flow
 * ----I need the count
 *2. Erland Process
 * 3. NH Poison Process
 *
 *
 * I need the mean of my data
 *
 *
 *
 *
 *
 *
 */


//Use the gen to generate arrivals.
//it gen a small value and adding them up takes the count.
// thing them all up also takes the (15min time) for the next arrival count harvesting.



// Converts the Arrival times of these Variates into Counts
//Every 15min, start the counts

// counter to stop vehicles for each 15min
// every 15min record the old count, restart the counts
// Every 900 sec; Has many interArrival time you got in the 900sec.
// Take the count for that 900sec. that's the Vehicle counts.

//It's very unlikely that the last will fall in say 890sec,
//Use the boundry situation to do an if statement
// if last counts falls >900 then let it go into the next count else
//let it be in the old count

//Put all in a MatrixD: So you can maintain proper indexing with the Pimps data





