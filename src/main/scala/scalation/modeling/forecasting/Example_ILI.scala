
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Apr 29 16:31:34 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Time Series Data: Influenza-Like Illness (ILI) Weekly Data
 *
 *  @see www.medrxiv.org/content/10.1101/2022.10.27.22281617v1.full
 *       arxiv.org/pdf/2001.08317v1
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Example_ILI` object provides a convenient way to load ILI weekly data.
 */
object Example_ILI:

    import scala.collection.mutable.HashMap

    val fileName = "national_illness.csv"

    val header = Array ("%WEIGHTED ILI",                // percent per state weighted by population
                        "%UNWEIGHTED ILI",              // aggregated without weighting
                        "AGE 0-4",                      // count in age group 0 to 4
                        "AGE 5-24",                     // count in age group 5 to 24
                        "ILITOTAL",                     // total ILI count
                        "NUM. OF PROVIDERS",            // number of clinics that report
                        "OT")                           // number of patient visits

    val response = "ILITOTAL"                           // main response/output variable

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load the ILI weekly data into a matrix for the exogenous variables x
     *  and a vector for the response/endogenous variable y.
     *  @param x_strs  the column names for the exogenous variables x
     *  @param y_str   the column name for the endogenous variable y
     *  @param trim    the number of initial rows to trim away (e.g., they are all 0)
     */
    def loadData (x_strs: Array [String], y_str: String, trim: Int = 0): (MatrixD, VectorD) =
        val col = HashMap [String, Int] ()
        for i <- header.indices do col += header(i) -> i

        val data = MatrixD.load (fileName, 1+trim, 1)      // skip first row (header) + trim first column
        val x_cols = for s <- x_strs yield col(s)
        (data(?, x_cols), data(?, col(y_str)))
    end loadData

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load the ILI weekly data into a vector for the response/endogenous variable y.
     *  @param y_str  the column name for the endogenous variable y
     *  @param trim   the number of initial rows to trim away (e.g., they are all 0)
     */
    def loadData_y (y_str: String, trim: Int = 0): VectorD =
        val col = HashMap [String, Int] ()
        for i <- header.indices do col += header(i) -> i

        val data = MatrixD.load (fileName, 1+trim, 1)      // skip first row (header) + trim first column
        data(?, col(y_str))
    end loadData_y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load the ILI weekly data into a matrix for the variables y.
     *  @param y_str  the column names for the variables y (e.g., used in a VAR model)
     *  @param trim   the number of initial rows to trim away (e.g., they are all 0)
     */
    def loadData_yy (y_strs: Array [String], trim: Int = 0): MatrixD =
        val col = HashMap [String, Int] ()
        for i <- header.indices do col += header(i) -> i

        val data = MatrixD.load (fileName, 1+trim, 1)      // skip first row (header) + trim first column
        val y_cols = for s <- y_strs yield col(s)
        data(?, y_cols)
    end loadData_yy

end Example_ILI

import Example_ILI._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_ILITest` main function test the `Example_ILI` object.
 *  Plots the response column.
 *  > runMain scalation.modeling.forecasting.example_ILITest
 */
@main def example_ILITest (): Unit =

    val y = Example_ILI.loadData_y (response)

    banner (s"Plot the response = $response column for the ILI dataset (${y.dim} points")
    new Plot (null, y, null, s"y ($response)", lines = true)

end example_ILITest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_ILITest2` main function test the `Example_ILI` object.
 *  This performs Exploratory Data Analysis (EDA) to find relationships
 *  between contemporaneous variables.
 *  > runMain scalation.modeling.forecasting.example_ILITest2
 */
@main def example_ILITest2 (): Unit =

    import scala.collection.mutable.Set

    val (x, y) = loadData (header, response)

    new Plot (null, y, null, "y ($response)", lines = true)

    for j <- x.indices2 do
        banner (s"EDA for response = $response vs. ${header(j)}")
        var xj  = x(?, j)                                               // get column j
        xj = scaleV (extreme (xj), (0.0, 2.0))(xj)                      // rescale vector xj to [0, 2]
        val xxj = MatrixD.fromVector (xj)
//      val mod = SymbolicRegression.quadratic (xxj, y)
//      val mod = SymbolicRegression.rescale (xxj, y, null, Set (1.0, 2.0, 3.0), cross = false)
        val mod = SymbolicRegression (xxj, y, null, Set (0.5, 1.0, 2.0, 3.0), cross = false)
        mod.trainNtest ()()
        val yp = mod.predict (mod.getX)
        println (mod.summary ())
        new Plot (xj, y, yp, s"y, yp ($response) vs. x_$j (${header(j)})")
    end for

end example_ILITest2



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_ILITest3` main function test the `Example_ILI` object.
 *  This test compares the ARMA model for several values of p and q.
 *  > runMain scalation.modeling.forecasting.example_ILITest3
 */
@main def example_ILITest3 (): Unit =

    import AR.hp

    val y = Example_ILI.loadData_y (response)

    val hh = 2                                                          // maximum forecasting horizon

    for p <- 1 to 8; q <- 0 to 1 do
        hp("p") = p; hp("q") = q                                        // set p (AR) and q (MA) hyper-parameters
        val mod = new ARMA (y, hh)                                      // create model for time series data
        banner (s"Test: ${mod.modelName} on ILI Dataset")
        mod.trainNtest ()()                                             // train and test the model on full dataset

        mod.forecastAll (y)                                             // forecast h-steps ahead for all y
        Forecaster.evalForecasts (mod, y, hh)
    end for

end example_ILITest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_ILITest4` main function test the `Example_ILI` object.
 *  This test compares the ARIMA model for several values of p and q.
 *  > runMain scalation.modeling.forecasting.example_ILITest4
 */
@main def example_ILITest4 (): Unit =

    import AR.hp

    val y = Example_ILI.loadData_y (response)

    val hh = 2                                                          // maximum forecasting horizon

    for p <- 1 to 8; q <- 0 to 1 do
        hp("p") = p; hp("q") = q                                        // set p (AR) and q (MA) hyper-parameters
        val mod = new ARIMA (y, hh)                                     // create model for time series data
        banner (s"Test: ${mod.modelName} on ILI Dataset")
        mod.trainNtest ()()                                             // train and test the model on full dataset

        mod.forecastAll (y)                                             // forecast h-steps ahead for all y
        Forecaster.evalForecasts (mod, y, hh)
    end for

end example_ILITest4

