
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Aug 28 18:17:12 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Functions to Write an Image into a Files in ScalaTion's "images" Directory
 */

package scalation
package scala2d

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Write the graphs into a buffered image file.
 *  @see stackoverflow.com/questions/5655908/export-jpanel-graphics-to-png-or-gif-or-jpg/39490801#39490801
 *  @param fname   the name of the file
 *  @param gframe  the gframe containing the graphics
 */
//def writeImage (fname: String, frame: VizFrame): Unit =
//    val bimg = new BufferedImage (frame.getSize ().width, frame.getSize ().height,
//                                  BufferedImage.TYPE_INT_ARGB)
//    val gr   = bimg.createGraphics ()
//    frame.paint (gr)
//    gr.dispose ()
//    ImageIO.write (bimg, "png", new File ("images" + fname))
//end writeImage

def writeImage(fname: String, frame: VizFrame): Unit =
    val bimg = new BufferedImage(frame.getSize().width, frame.getSize().height, BufferedImage.TYPE_INT_ARGB)
    val gr = bimg.createGraphics()
    frame.paint(gr)
    gr.dispose()

    // Ensure the "images" directory exists
    val imagesDir = new File("images")
    if !imagesDir.exists() then imagesDir.mkdirs()

    // Construct the file path
    val outputFile = new File(imagesDir, fname)

    // Write the image
    ImageIO.write(bimg, "png", outputFile)
end writeImage



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `writeImageTest` main function is used to test the writing an image file.
 *  @see scalation.mathstat.PlotM
 *  > runMain scalation.scala2d.writeImageTest
 */
@main def writeImageTest (): Unit =

    println (s"writeImageTest: see `Plot`, PlotC, and `PlotM` classes for examples")

end writeImageTest

