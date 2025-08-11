package scalation
package simulation
package process

import scalation.optimization.NelderMeadSimplex2
import scalation.simulation.process.example_1.OneWayVehicle2L
import scalation.mathstat._


/**
 * This class is used for the optimization of the OneWayVehicle simulation model. 
 */
class TrafficOptimization:
    
    /**
     * //y=f(θ) + e :
     * @param params : take a parameters of θ; and return a single double value of smape
     * @return: the returned value is an average of the 7 sensors smape values.
     * A simple average of (s1+s2+s3+s4+s5+s6+s7)/7.0
     */
    def objFunc(params: VectorD): Double =

        //Vehicle.setParams(params)

        Vehicle.setProps(Vehicle.setParams(params))
        println(s"The prop of the vehicle ${Vehicle.prop}")
        println(s"the param values $params")

        val model = new OneWayVehicle2L()

        val smapeAvg = 0.0// model.simRunVsPemsRun()

        println(s"The smape average value for the 7 sensors $smapeAvg")

        smapeAvg   // return the average smape value for the 7 sensors.
    end objFunc


    val func: FunctionV2S = (params:VectorD) => objFunc(params)

end TrafficOptimization



@main def trafficOptimizationTest():Unit =

    val simOpt = new TrafficOptimization

    //the parameters to be optimized by the objective function
    //s:Double, amax:Double, bmax:Double, T:Double ,τ:Double
    val params = VectorD(5.0, 4.0, -1.5, 3.0, 1.0)

    val smapeValue = simOpt.func(params)


    println(s"The smape value for the 7 sensor is : $smapeValue")
    val optimizer = new NelderMeadSimplex2 (simOpt.func, params.dim)


    val opt = optimizer.solve (params)                                  // optimal solution
    println (s"optimal solution = (f(x), x) = $opt")

    optimizer.plotLoss ()

    Model.shutdown()
end trafficOptimizationTest
