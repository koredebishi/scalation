
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Korede Bishe, John Miller
 *  @version 2.0
 *  @date    Mon Jun  2 15:00:25 EDT 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Differential Evolution (DE) Derivative-Free Optimization Algorithm
 *           Initial draft of code written by ChatGPT
 *
 *  @see     en.wikipedia.org/wiki/Differential_evolution
 *  @see     `GeneticAlgorithm` for a more general metaheuristic optimizer
 */

package scalation
package optimization

import scala.runtime.ScalaRunTime.stringOf
import scala.util.Random.{nextInt, nextDouble, shuffle}
import scala.util.boundary, boundary.break

import scalation.mathstat.{FunctionV2S, VectorD}
import scalation.random.RandomVecD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DifferentialEvolution` object solves optimization problems using
 *  the Differential Evolution algorithm.  This population-based metaheuristic
 *  optimizes a real-valued function by iteratively improving candidate solutions.
 *  minimize    f(x)
 */
object DifferentialEvolution
    extends MonitorEpochs:

    private val debug = debugf ("DifferentialEvolution", false)                   // debug function
    private val eps   = 1E-12                                                    // number close to zero

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform Differential Evolution optimization on objective function f.
     *  @param f        the real-valued objective function to be minimized
     *  @param dim      the dimensionality of the solution space
     *  @param bounds   the search boundaries as a tuple (min, max)
     *  @param maxGen   the maximum number of generations
     *  @param F        the differential weight (scaling factor)
     *  @param CR       the crossover probability
     *  @param popSize  the population size (approx. 10 * dim)
     *  @return a tuple containing the best solution vector and its objective function value
     */
    def optimize (f: FunctionV2S, dim: Int, bounds: (Double, Double), maxGen: Int = 400,
                  F: Double = 0.8, CR: Double = 0.9)(popSize: Int = 10 * dim):
    (VectorD, Double) = boundary:                                        // boundary block to allow breaking early

        initializeMonitoring(maxGen)                                             // reset monitoring state
        val startTime = System.nanoTime()                                        // start timer

        val rrv     = RandomVecD (dim, bounds._2, bounds._1)                     // random vector generator within bounds
        val pop     = Array.fill (popSize)(rrv.gen)                              // initialize population
        var best    = pop.minBy (f)                                              // find initial best individual
        var bestVal = f(best)                                                    // evaluate best value

        var noImprove = 0                                                        // count of generations with no improvement
        val patience  = 25                                                       // maximum allowed stagnation (complex problems may require more)

        debug ("optimize", s"initial population: ${stringOf(pop)}")

        for gen <- 1 to maxGen do
            var improved = false                                                 // flag to check improvement in current generation

            for i <- 0 until popSize do

                // Mutation: select 3 unique individuals (not including i)
                val idxs = shuffle ((0 until popSize).filter (_ != i)).take(3)
                val (a, b, c) = (pop(idxs(0)), pop(idxs(1)), pop(idxs(2)))
                val mutant = a + (b - c) * F                                     // differential mutation formula

                // Crossover: combine target and mutant to form trial vector
                val trial = new VectorD (dim)
                val jj = nextInt (dim)                                           // force at least one mutant gene
                cfor (0, dim) { j =>
                    trial(j) = if j == jj || nextDouble() < CR then mutant(j) else pop(i)(j) }

                // Selection: replace individual if trial is better
                if f(trial) + eps < f(pop(i)) then
                    pop(i) = trial
                    val trialVal = f(trial)
                    if trialVal < bestVal then
                        best     = trial                                         // update best vector
                        bestVal  = trialVal                                      // update best value
                        improved = true                                          // mark that improvement occurred
            end for

            updateMonitoring(gen, bestVal)                                       // update epoch monitoring
            epochLoss += bestVal                                                 // track bestVal for plotting convergence
            debug ("optimize", s"Generation $gen: bestVal = $bestVal")

            if improved then noImprove = 0 else noImprove += 1                   // update stagnation count
            if noImprove >= patience then
                println (s"Early stopping at generation $gen (no improvement in $patience generations).")

                val endTime       = System.nanoTime()
                val elapsedTimeMs = (endTime - startTime) / 1E6
                val elapsedTimeSec= elapsedTimeMs / 1E3


                println()
                println(sline(70).trim)
                println("DIFFERENTIAL EVOLUTION: OPTIMIZATION SUMMARY")
                println(sline(70).trim)
                println(f"${"Metric"}%-25s | ${"Value"}%s")
                println(sline(70).trim)
                println(f"${"Best position found"}%-25s | $best")
                println(f"${"Best loss achieved"}%-25s | $bestVal%.8f")
                println(f"${"Total generations"}%-25s | ${epochLoss.size}")
                println(f"${"Population size"}%-25s | $popSize")
                println(f"${"Elapsed time"}%-25s | ${elapsedTimeSec}%.4f seconds (${elapsedTimeMs}%.2f ms)")
                println(f"${"Time per generation"}%-25s | ${elapsedTimeMs / epochLoss.size}%.2f ms")
                println(sline(70).trim)

                break ((best, bestVal))                                          // exit early with best solution
        end for

        val endTime       = System.nanoTime()
        val elapsedTimeMs = (endTime - startTime) / 1E6
        val elapsedTimeSec= elapsedTimeMs / 1E3


        println()
        println(sline(70).trim)
        println("DIFFERENTIAL EVOLUTION: OPTIMIZATION SUMMARY")
        println(sline(70).trim)
        println(f"${"Metric"}%-25s | ${"Value"}%s")
        println(sline(70).trim)
        println(f"${"Best position found"}%-25s | $best")
        println(f"${"Best loss achieved"}%-25s | $bestVal%.8f")
        println(f"${"Total generations"}%-25s | ${epochLoss.size}")
        println(f"${"Population size"}%-25s | $popSize")
        println(f"${"Elapsed time"}%-25s | ${elapsedTimeSec}%.4f seconds (${elapsedTimeMs}%.2f ms)")
        println(f"${"Time per generation"}%-25s | ${elapsedTimeMs / epochLoss.size}%.2f ms")
        println(sline(70).trim)

        (best, bestVal)                                                          // return best result after all generations
    end optimize

end DifferentialEvolution


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `differentialEvolutionTest` main function is used to test the `DifferentialEvolution`
 *  object. Tests the unified metrics implementation (elapsed time, epoch monitoring, best tracking).
 *  > runMain scalation.optimization.differentialEvolutionTest
 */
@main def differentialEvolutionTest (): Unit =

    import DifferentialEvolution._

    println("="*70)
    println("DIFFERENTIAL EVOLUTION - UNIFIED METRICS TEST")
    println("="*70)

    println ("\nProblem: (x_0 - 3)^2 + (x_1 + 1)^2 + (x_2)^2 + 2")
    println (s"Expected optimum: VectorD(3.0, -1.0, 0.0) with f(x) = 2.0")
    println ()

    val f: FunctionV2S = (x: VectorD) => (x(0) - 3)~^2 + (x(1) + 1)~^2 + 1 + (x(2)~^2 + 1)      // test function

    val (bestSol, bestVal) = optimize (f, 3, (-5.0, 5.0))()

    println ()
    println (s"Final result: best solution = $bestSol")
    println (s"              objective value = $bestVal")
    println ()
    println ("Plotting loss convergence...")
    plotLoss ()


end differentialEvolutionTest

