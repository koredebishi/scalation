


package scalation
package simulation
package process


import scalation.mathstat.*
import scalation.random.*
import scalation.simulation.process.modeling.clustering.Coordinates


class TrafficConfig(fileName: String, rowTime: Double, stream: Int = 0):


    //--------------------------------------------------
    // Load Data
    private[process] val ew = new EasyWriter("recorder", "TrafficConfigText.txt")

//    val t1 = 0
//    val t2 = 6

//    val t1 = 40
//    val t2 = 65


    val t1 = 2 * 96 + 16
    val t2 = 3 * 96 // Just a few rows (for debugging)


    val rowOffset = t1


    val data = MatrixD.load(fileName, t1, t2)



    //val data = MatrixD.load(fileName)
    //println(s"\n the loaded data is: $data \n")

    val laneIdx = VectorI(5, 8, 11, 14)                 // Indices for the four lanes

    val arrivalCounts = data(?, laneIdx)                // data extract based on the flow/lanes
    val totalArrivalsPerRow = arrivalCounts.sumVr // Sum of flows across the 4 lanes

    val mu = totalArrivalsPerRow.map(rowTime / _) // the mu Vectors for each row. the intensity Vector.

    val laneProbPerRow = arrivalCounts.mmap(_.toProbability)
    //--------------------------------------------------
    val laneRVPerRow = Vector.tabulate(data.dim)(row => Discrete(laneProbPerRow(row)))    // laneRV per roll stored here
    //--------------------------------------------------
    // Function to Get LaneRV for a Specific Row
    @inline def getLaneRV(row: Int): Discrete = laneRVPerRow(row)                 // method to get the current laneRV for the current dataIndex

    /**
     * This method uses the Coodinate class to calculate the scales real data (Pims) to
     * Animation coodinate for the simulation
     *
     * @param aw   the width of the animation window
     * @param ah   the height of the animation window
     * @param path an array of lat-long coordinates from the file
     */
    def getJunctions(path: String, w_h: (Double, Double)): Array[(Double, Double)] =

        val data = scala.io.Source.fromFile(path).getLines.toArray
        val gps = Array.ofDim[(Double, Double)](data.length)

        for i <- data.indices do
            val ll = data(i).split(",")
            val lat = ll(0).toDouble
            val long = ll(1).toDouble
            gps(i) = (lat, long)
        end for
        val coords = new Coordinates(w_h._1, w_h._2, gps)
        coords.calcAniCoords()

        for (lat, long) <- gps do println(s"the gps $lat and $long")
        for (x, y) <- coords.aniCoords do println(s"the scaled coordinate is x: $x : y $y")

        coords.aniCoords
    end getJunctions


end TrafficConfig




@main def TrafficConfigTest(): Unit =
    val rowTime = 15 * MINUTE
    val stream = 0
    val trafficData = new TrafficConfig("/seven_sensors/402376.csv", rowTime, stream)


    println(s"the total arrival ${trafficData.mu}")

//    // Verify lane probabilities sum to 1
//    println("\nValidating lane probability normalization...")
//    for row <- 0 until 20 do
//        val probSum = trafficData.laneProbPerRow(row).sum
//        println(f"Row $row Probability Sum: $probSum%.6f (should be ~1.0)")
//
//    // Validate LaneRV correctness
//    println("\nValidating laneRV generation...")
//    for row <- 0 until 20 do
//        val laneRV = trafficData.getLaneRV(row)
//        println(s"Row $row LaneRV: $laneRV")
//
//    // Validate lane selection using `igen`
//    println("\nValidating lane selection using Discrete.igen...")
//    for row <- 0 until 20 do
//        val laneRV = trafficData.getLaneRV(row)
//        val sampledLane = laneRV.igen // Generate lane selection
//        println(s"Row $row Sampled Lane: $sampledLane")
//
//    // Validate iArrivalRV initialization and values
//    println("\nValidating inter-arrival time distributions...")
//    for row <- 0 until 20 do
//        val iArrivalRV = trafficData.getIArrivalRV(row)
//        val sampledIArrival = iArrivalRV.igen
//        println(f"Row $row iArrivalRV: Mean=${iArrivalRV.mean}%.6f, Sampled Inter-Arrival=${sampledIArrival}%.6f")
//












//Plan is to beat Dr Casey's result
//lane change is a more realistics
//May/ exit and entry ramp
//Empirical creation of arrivals (I did it using the main datasets: )
//Calibration: I need to tune the Gipps Models Hyperparameter
//Optimization: SPSA, GA, NM (Already in scalation)
//lane change probability: A more robust one


//Model is trying to predict the traffic flow; then I have the traffic flow: then I have to measure
//the MSE of the simulated data vs the main dataset
//each of the location where there is a sensor: the model will say the flow in this lane is X
//PEMS Says Y; How big is the difference
// Use SMAPE(hoping for 6%) OR RSquared( hope for 90%)
// Will also need to tune the Gipps Model before getting the error rate
//change lane based on what I see in the data


//    val iArrivalRVPerRow = Vector.tabulate(data.dim) ( row =>
//        val mu = if totalArrivalsPerRow(row) > 0 then rowTime / totalArrivalsPerRow(row) else 1.0  // 15 /
//        ew.write(s"\n row $row => : $mu and rowtime is $rowTime \n ")
//        Exponential2(mu, stream + row)
//        //Erlang(mu/2)
//    )
//    ew.flush()


// We will use this to access each interArrival (precomputer per row)
//@inline def getIArrivalRV(row: Int): Erlang = iArrivalRVPerRow(row - rowOffset) // method to get the current iArrivalRV for the current dataIndex

//    //
////    // We will use this to access each interArrival (precomputer per row)
//    @inline def getIArrivalRV(row: Int): Exponential2 = iArrivalRVPerRow(row - rowOffset) // method to get the current iArrivalRV for the current dataIndex

//    // We will use this to access each interArrival (precomputer per row)
//def ArrivalRV(row:Int,mu:Double, variate: Variate): Unit =

//        val gen = if variate.isInstanceOf[Erlang] then variate.gen1(mu/2) else variate.gen1(mu)

//    @inline def computeMu(row: Int): Double =
//        if totalArrivalsPerRow(row) > 0 then rowTime / totalArrivalsPerRow(row) else 1.0
//
