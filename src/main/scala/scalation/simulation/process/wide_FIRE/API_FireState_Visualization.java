package scalation.simulation.process.wide_FIRE;

/**
 * This class visualizes the fire state returned from the DEVS-FIRE API call.
 * This is a preliminary implementation of the visualization. For example, the cell
 * space dimension is pre-defined as 200 x 200. More advanced implementation will be
 * provided later
 *
 * Copyright: Systems Integrated Modeling and Simulation (SIMS) Lab, Georgia State University, All Rights Reserved
 * Contact: Prof. Xiaolin Hu (xhu@gsu.edu)
 * Date: April 13 2024
 *
 */


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;


public class API_FireState_Visualization extends JFrame{
    public static int x_cellDisplaySize = 3;//6;//3;//10;//
    public static int y_cellDisplaySize = 3;//6;//3;//10;//
    public static int xCellspaceDim = 200;
    public static int yCellspaceDim = 200;
    public static int x_spaceDisplaySize = xCellspaceDim*x_cellDisplaySize;
    public static int y_spaceDisplaySize = yCellspaceDim*y_cellDisplaySize;


    /**
     * The color of each cell in the grid.
     */
    protected Color[][] grid;

    /**
     * The actual panel on which the grid is drawn.
     */
    protected GridPanel gridPanel;

    /**
     * The size of the cell space being depicted (in cells).
     */
    protected static double xRange=400, yRange=400;

    protected Dimension spaceSize = new Dimension (40,40);

    /**
     * The size of each cell (in pixels).
     */
    protected int cellSize = 10;


    /**
     * The scale factors to use when depicting a range that is larger than
     * the space-size.
     */
    protected double xScaleFactor = 1, yScaleFactor = 1;

    /**
     * The font used to draw labels on the grid, as well as its associated
     * metrics object and aspects of its size.
     */
    protected Font labelFont = new Font("SansSerif", Font.PLAIN, 12);
    protected FontMetrics labelFontMetrics = getFontMetrics(labelFont);
    protected int labelFontAscent = labelFontMetrics.getAscent();

    /**
     * The axes labels.
     */
    protected String xLabel = "X", yLabel = "Y";

    /**
     * The width of the border around the grid.
     */
    protected final int borderWidth = 5;
    //for agent display purpose --- Xiaolin Hu
    protected int AgentX = -1;
    protected int AgentY = -1;
    protected Color preAgentCellColor = null;

    public boolean displayGrid = false;


    public API_FireState_Visualization (){
        super("FireStateVisualization");
        createGridView();
    }

    /**
     * Draw the fire shape from input file using desiredColor
     */
    public void visualize(String data){

        double timeGranularity = 100; // seconds
        // remove the beginning "[{" and the ending "]"
        data = data.substring(2,data.length()-1);
        //System.out.println(data);

        String[] dataPoint = data.split("\\{");
        int dataSize= dataPoint.length;
        int index=0;
        int[] cellX= new int[dataSize];
        int[] cellY= new int[dataSize];
        double[] time= new double[dataSize];
        int[] state = new int[dataSize];

        for(int i=0; i<dataSize;i++) {
            //System.out.println(dataPoint[i]);
            int yLoc = dataPoint[i].indexOf("y");
            int OpeLoc = dataPoint[i].indexOf("Operation");
            int timeLoc = dataPoint[i].indexOf("time");
            if(dataPoint[i].substring(OpeLoc+12, timeLoc-3).compareTo("BurnCell")==0) {
                int stateLoc = dataPoint[i].indexOf("state");
                cellX[index] = Integer.valueOf(dataPoint[i].substring(5, yLoc-3));
                cellY[index] = Integer.valueOf(dataPoint[i].substring(yLoc+4, OpeLoc-3));
                time[index] = Double.valueOf(dataPoint[i].substring(timeLoc+7, stateLoc-3));
                state[index] = Integer.valueOf(dataPoint[i].substring(stateLoc+8, stateLoc+9));
                index++;
            }
            else { // the data is for the ignition team. Skip it for now
                // do nothing
            }
        }

        double currentTime = time[0];
        setTitle("FireStateVisualization - SimulationTime="+currentTime);
        int currentIdx=0;

        while(currentIdx<index) {
            if(time[currentIdx]<(currentTime+timeGranularity)) {
                System.out.println("draw cell:"+cellX[currentIdx]+"_"+cellY[currentIdx]+" State="+state[currentIdx]);
                double x_pos = cellX[currentIdx]-xCellspaceDim/2;
                double y_pos = cellY[currentIdx]-yCellspaceDim/2;
                if(state[currentIdx]==1){
                    drawCellToScale(x_pos, y_pos,Color.red);
                }
                else if(state[currentIdx]==2){
                    drawCellToScale(x_pos, y_pos,Color.black);
                }
                currentIdx++;
            }
            else {
                currentTime+=timeGranularity;
                setTitle("FireStateVisualization - SimulationTime="+currentTime);
                try {
                    Thread.sleep(100);
                } catch (Exception e){}
            }
        }
        setTitle("FireStateVisualization - SimulationTime="+(int)time[currentIdx-1]);

    }

    public void createGridView(){
        xRange = x_spaceDisplaySize;
        yRange = y_spaceDisplaySize;
        xLabel = "" + .5 * xRange;
        yLabel = "" + .5 * yRange;

        cellSize = x_cellDisplaySize;
        spaceSize = new Dimension((int)xRange/cellSize, (int)yRange/cellSize);

        // create the grid data structure
        grid = new Color[spaceSize.width][spaceSize.height];

        // set the properties of this view's content pane
        Container pane = getContentPane();
        //Container pane = this;
        pane.setBackground(Color.white);
        pane.setLayout(new BorderLayout());

        // add the main panel
        JPanel main = new JPanel();

        main.setLayout(new BorderLayout());
        main.setBorder(BorderFactory.createEmptyBorder(
                borderWidth, borderWidth, borderWidth, borderWidth));
        pane.add(main, BorderLayout.CENTER);

        // add the grid panel
        gridPanel = new GridPanel();
        //GridPanel gridPanel=(GridPanel)GUI.ttest.jPanel2;
        main.add(gridPanel, BorderLayout.CENTER);

        setCellSize(x_cellDisplaySize);
        setXScale(xRange);
        setYScale(yRange);
        setLocation(300, 100);
        setVisible(true);

        // draw map color based on fuel, currently it is assumes a single color of green
        double x_pos;
        double y_pos;
        for (int i = 0; i < yCellspaceDim; i++){
            for (int j = 0; j < xCellspaceDim; j++) {
                x_pos = j-xCellspaceDim/2;
                y_pos = i-yCellspaceDim/2;
                drawCellToScale(x_pos, y_pos, Color.getHSBColor(200, 240, 250));
            }
        }
    }

    public void setCellSize(int size)
    {
        cellSize = size;
        if(cellSize>0){
            spaceSize = new Dimension( (int) xRange / cellSize,
                    (int) yRange / cellSize);
            grid = new Color[spaceSize.width][spaceSize.height];
        }
        adjustSizeToHoldGridPanel();
    }

    /**
     * Adjusts the size of this view's frame to just contain the grid
     * panel and its border.
     */
    protected void adjustSizeToHoldGridPanel()
    {
        // adjust the size of this view to just contain the grid and
        // its border
        Insets insets = getInsets();
        setSize(gridPanel.getPreferredSize().width
                        + insets.left + insets.right + borderWidth * 2,
                gridPanel.getPreferredSize().height
                        + insets.top + insets.bottom + borderWidth * 2);
    }

    /**
     * Sets the x scale factor to use when depicting a range that is larger
     * than the space-size.
     *
     * @param   range       The x-range the cell space is supposed to depict.
     */
    public void setXScale(double range)
    {
        if(cellSize > 0) xScaleFactor = 1/((double)cellSize);  //Xiaolin Hu
        else xScaleFactor = spaceSize.width / range;

    }

    /**
     * Sets the y scale factor to use when depicting a range that is larger
     * than the space-size.
     *
     * @param   range       The y-range the cell space is supposed to depict.
     */
    public void setYScale(double range)
    {
        if(cellSize > 0) yScaleFactor = 1/((double)cellSize); // Xiaolin Hu
        else yScaleFactor = spaceSize.height / range;
    }

    /**
     * Returns the given x scaled according to the current x-scale-factor.
     *
     * @param   x       The x to scale.
     * @return          The x-value, scaled.
     */
    protected int scaleX(double x)
    {
        int centerX = (int)Math.rint(spaceSize.width / 2.0);
        int scaled = centerX + (int)x;//(int)Math.rint(x * xScaleFactor);
        return forceXInBounds(scaled);
    }

    /**
     * Returns the given y scaled according to the current y-scale-factor.
     *
     * @param   y       The y to scale.
     * @return          The y-value, scaled.
     */
    protected int scaleY(double y)
    {
        int centerY = (int)Math.rint(spaceSize.height / 2.0);
        int scaled = centerY + (int)y ;//(int)Math.rint(y * yScaleFactor);
        return forceYInBounds(scaled);
    }

    /**
     * Returns the given x limited to the cell-space's width (and zero).
     *
     * @param   x       The cell x to limit.
     */
    protected int forceXInBounds(int x)
    {
        x = (x < 0) ? 0 : x;
        x = (x >= spaceSize.width) ? spaceSize.width - 1 : x;
        return x;
    }

    /**
     * Returns the given y limited to the cell-space's height (and zero).
     *
     * @param   y       The cell y to limit.
     */
    protected int forceYInBounds(int y)
    {
        y = (y < 0) ? 0 : y;
        y = (y >= spaceSize.height) ? spaceSize.height - 1 : y;
        return y;
    }

    /**
     * Draws a cell at the location determined by scaling the given
     * cell location by the current scale factors.
     *
     * @param   cellX, cellY        The cell location to scale.
     * @param   color               The color to fill the cell with.
     */
    public void drawCellToScale(double cellX, double cellY, Color color)
    {
        // detm the scaled pixel location of the cell
        int x = (scaleX(cellX) * cellSize) + 1;
        int y = (scaleY(-cellY) * cellSize) + 1;

        if(x==AgentX && y==AgentY){  //Xiaolin Hu
            preAgentCellColor = color;
            return;
        }

        fillCellOnSwingThread(x, y, color);
    }

    /**
     * Fills the cell at the given pixel location with the given color.
     * Note that when filling a cell, neither its starting row or column
     * are filled; this keeps the cell from obliterating the axis lines and
     * provides a border between adjacent cells.
     *
     * @param   pixelX, pixelY      The pixel location of the upper-left-hand
     *                              corner of the cell to fill.
     * @param   color               The fill color.
     */
    protected void fillCell(int pixelX, int pixelY, Color color)
    {
        // fill in the cell's rectangle
        Graphics g = gridPanel.getGraphics();
        g.setColor(color);
        if(displayGrid) // Xiaolin Hu, Oct. 16, 2007
            g.fillRect(pixelX, pixelY, cellSize - 1, cellSize - 1);
        else
            g.fillRect(pixelX, pixelY, cellSize, cellSize);

        // remember the cell's new color
        grid[pixelX / cellSize][pixelY / cellSize] = color;
    }

    /**
     * Makes a call to the fillCell() method on the swing thread.
     *
     * See fillCell() for parameter descriptions.
     */
    protected void fillCellOnSwingThread(final int pixelX, final int pixelY,
                                         final Color color)
    {
        // run this code on the swing thread
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                fillCell(pixelX, pixelY, color);
            }
        });
    }

    /**
     * Draws a string at the given cell location using the given color.
     *
     * @param   cellX, cellY        The cell location at which to draw
     *                              the string.
     * @param   string              The text string to draw.
     * @param   color               The color with which to draw the string.
     */
    public void drawString(int cellX, int cellY, final String string,
                           final Color color)
    {
        // detm the pixel location of the cell
        final int x = forceXInBounds(cellX) * cellSize;
        final int y = forceYInBounds(cellY) * cellSize;

        // run this code on the swing thread
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                // draw the given string with its starting point centered
                // within the cell
                Graphics g = gridPanel.getGraphics();
                g.setColor(color);
                g.drawString(string, x + cellSize / 2, y + cellSize / 2);
            }
        });
    }
    /**
     * The panel on which the actual grid is drawn.
     */
    protected class GridPanel extends JPanel
    {
        /**
         * Constructor.
         */
        public GridPanel()
        {
            setBackground(Color.white);
        }

        /**
         * See parent method.
         */
        public Dimension getPreferredSize()
        {
            return new Dimension(spaceSize.width * cellSize,
                    spaceSize.height * cellSize);
        }

        /**
         * Paints this view's static graphical elements, such as its border
         * and axes labels.
         */
        public void paint(Graphics g)
        {
            //System.out.println("paint");
            super.paint(g);

            // draw and label the 0,0 cell
            int width = spaceSize.width * cellSize, halfX = width / 2;
            int height = spaceSize.height * cellSize, halfY = height / 2;
            g.drawRect(halfX, halfY, cellSize, cellSize);
            g.drawString("0,0", halfX + 4, halfY - 4);

            // label the axes
            g.drawString(xLabel,
                    width - labelFontMetrics.stringWidth(xLabel) - 4,
                    halfY - 4);
            g.drawString(yLabel, halfX + 4, labelFontAscent + 4);

            // draw the two lines that form the four quadrants
            g.drawLine(halfX, 0, halfX, height);
            g.drawLine(0, halfY, width, halfY);

            // for each cell in the grid
            for (int i = 0; i < spaceSize.width; i++) {
                for (int j = 0; j < spaceSize.height; j++) {
                    // if this cell has had its color set
                    if (grid[i][j] != null) {
                        // fill in this cell with its color
                        g.setColor(grid[i][j]);
                        if(displayGrid) // Xiaolin Hu, Oct. 16, 2007
                            g.fillRect(i * cellSize + 1, j * cellSize + 1,
                                    cellSize - 1, cellSize - 1);
                        else
                            g.fillRect(i * cellSize + 1, j * cellSize + 1,
                                    cellSize, cellSize);
                    }
                }
            }
        }
    }

}
