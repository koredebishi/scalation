
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Casey Bowman, John Miller
 *  @version 2.0
 *  @date    Wed Feb 16 11:34:46 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 */

package scalation
package optimization

import scalation.mathstat.{FunctionV2S, VectorD}
import scalation.random.{Discrete, Randi, Uniform, Variate}

import javax.naming.OperationNotSupportedException
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import scala.util.control.Breaks.{break, breakable}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GA` class solves unconstrained Non-Linear Programming (NLP) problems
 *  using a genetic algorithm approach.  Given a function 'f' and a set of 
 *  randomm variables based on the dimensionality of the search space, and
 *  the domain of the search space, the GA will evolve a pool of candidate
 *  solutions using evolutionary concepts such as crossover and mutation.
 *  The randomm variables are used to create randomm candidate solutions for
 *  the solution pool. The algorithm iterates until it converges or has 
 *  reached a maximum number of generations.
 *
 *  minimize    f(x)
 *
 *  @param f      the vector-to-scalar objective function
 *  @param rands  randomm variables used to create the initial 'gene' pool.
 *                There is one r.v. per dimension, and should reflect the
 *                domain of the search space.
 */
class GeneticAlgorithm (f: FunctionV2S, rands: Array [Variate])
    extends Minimizer (50)
        with MonitorEpochs:

    private val N = 15        // number of candidates to keep in the pool

    private val pool = Array.ofDim [FuncVec] (N)    // the pool of candidate solutions. The values are a tuple
                                                             // of the candidate with their objective function value.

    private val randInd = Randi (0, rands.length)         // an r.v. to generate a randomm index for crossover and mutation
    private val randMut = Uniform (-0.2, 0.2)             // an r.v. to generate a size for mututations.
    private val epochs    = new ArrayBuffer [Double] ()

//    println (rands.deep)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create the initial pool of candidate solutions.
     *  @param seeds  a (possibly null) array of initial candidates provided
     *                by the user.
     */    
    def initPool (seeds: Array[VectorD] = Array.ofDim(0)): Unit =

        var i0 = 0
        if seeds != null then
            i0 = seeds.length
            for i <- 0 until i0 do pool(i) = (f(seeds(i)), seeds(i))
        end if

        for i <- i0 until N do
            val x = new VectorD (rands.length)
            for j <- x.indices do x(j) = rands(j).gen
            pool(i) = (f(x), x)
        end for
    end initPool

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort the pool by the objective function value of the candidate solutions.
     */    
    def sortPool (): Unit =

        for i <- 0 until N - 1 do
            val j = findMin(i)
            if i != j then {val t = pool(i); pool(i)= pool(j); pool(j)= t}
        end for

/*
        for i <- 0 until N - 1 do
            for j <- 0 until N - i - 1 do
                val y1 = pool(j)._1
                val y2 = pool(j + 1)._1
                if y2 < y1 then
                    val t       = pool(j)
                    pool(j)     = pool(j + 1)
                    pool(j + 1) = t
                end if
            end for
        end for
*/
    end sortPool

    def findMin( i:Int ) :Int=
        var jm = i
        for j <- i+1 until N do
            if pool(j)._1 <= pool(jm)._1 then
                jm = j
        jm
    end findMin



    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the next generation of solutions. The top four solutions are
     *  kept and the rest of the pool size is filled out by evolving new solutions
     *  with crossover and mutation.
     */
    def nextGen (): Unit =
        var count = 4
        for i <- 0 until 3 do
            for j <- i + 1 until 4 do
                val x1 = pool(i)._2
                val x2 = pool(j)._2
                var x3 = cross (x1, x2)
                mutate (x3)
                val y = f(x3)
                pool(count) = (y, x3)
                count += 1
            end for
        end for

        for i <- count until pool.length do
            val x = new VectorD (rands.length)
            for j <- x.indices do x(j) = rands(j).gen
            val y = f(x)
            pool(i) = (y, x)
        end for
    end nextGen

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the crossover of two solutions.
     *  @param x1  the first solution for the crossover
     *  @param x2  the second solution for the crossover
     */
    def cross (x1: VectorD, x2: VectorD): VectorD = 

        val k = randInd.igen           // generate a randomm index
        val j = randInd.igen
        if j % 2 == 0 then x1(0 until k) ++ x2(k until x2.dim)
        else               x2(0 until k) ++ x1(k until x1.dim)
    end cross

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform a mutation on a solution.
     *  @param x  the solution on which to perform the mutation
     */
    def mutate (x: VectorD): Unit =
        for i <- x.indices do x(i) *= (1.0 + randMut.gen)  // apply a multiplicative factor to the current index-value of the solution.
    end mutate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Utility method to print the current solution pool.
     *  @param n  the number of solutions from the pool to include in the print.
     *            the default is set to 5
     */
    def printPool (n: Int = 5): Unit =
        print ("Pool = [")
        for i <- 0 until n - 1 do  print (s"${pool(i)}, ")
        println (s"${pool(n - 1)}]")
    end printPool

    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
        throw new OperationNotSupportedException("lineSearch is not needed for GA")
    end lineSearch


    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPSILON): FuncVec =
        throw new OperationNotSupportedException("Use solve2 instead of solve")
    end solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the optimization problem. 
     *  @param seeds  an array of initial candidates provided
     *                by the user.
     */
    def solve2 (seeds: Array[VectorD] = null): FuncVec =

        initPool (seeds)
        sortPool ()
        println ("-------------------------------------------------------")
        println ("Generation 0:")
        printPool ()
        breakable {
            for i <- 0 until MAX_ITER do
                println ("-------------------------------------------------------")
                println ("Generation " + (i + 1) + ":")
                nextGen ()
                sortPool ()
                epochs += pool(0)._1
                printPool ()
            end for
        }//breakable
        pool(0)
    end solve2
end GeneticAlgorithm


@main def GATest(): Unit =

    def f (x: VectorD): Double = (x(0) - 3.0) * (x(0) - 3.0) + (x(1) + 1.0) * (x(1) + 1.0) + 1.0

    val r0 = Uniform (-10.0, 10.0, 1)
    val r1 = Uniform (-10.0, 10.0, 2)

    val seeds = Array (VectorD (2.0, 0.0), VectorD (4.0, -2.0))

    val solver = new GeneticAlgorithm (f, Array (r0, r1))
    val x      = solver.solve2 (seeds)

    println ("optimal x = " + x)
end GATest

@main def selectionSortTest(): Unit =

    def f(x: VectorD): Double = (x(0) - 3.0) * (x(0) - 3.0) + (x(1) + 1.0) * (x(1) + 1.0) + 1.0

    val r0 = Uniform(-10.0, 10.0, 1)
    val r1 = Uniform(-10.0, 10.0, 2)

    val seeds = Array(VectorD(2.0, 0.0), VectorD(4.0, -2.0))

    val solver = new GeneticAlgorithm(f, Array(r0, r1))
    solver.initPool(seeds)
    solver.printPool(15)
    solver.sortPool()
    solver.printPool(15)

end selectionSortTest


























