/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.map.boardPicker.board.*;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

import java.awt.*;
import java.awt.geom.Area;

public class Zone extends AbstractConfigurable implements GridContainer {
  public static final String NAME = "name";
  public static final String PATH = "path";
  public static final String LOCATION_FORMAT = "locationFormat";
  public static final String GRID_LOCATION = "Grid Location";

  protected String locationFormat = "$" + NAME + "$";
  protected FormattedString format = new FormattedString();
  protected Polygon myPolygon;
  private MapGrid grid = null;

  public Zone() {
    this(new Polygon());
  }

  public Zone(Polygon p) {
    myPolygon = p;
  }

  public String getName() {
    return name;
  }

/*
  public static class PathConfig implements ConfigurerFactory {
    public Configurer getConfigurer(
      AutoConfigurable c,
      String key,
      String name) {
      return new PathConfigurer(key, name);
    }

    public class PathConfigurer
      extends StringConfigurer
      implements ActionListener {

      JButton modButton;

      public PathConfigurer(String key, String name) {
        super(key, name);
      }

      public java.awt.Component getControls() {
        if (p == null) {
          super.getControls();
          nameField.setEnabled(false);
          modButton = new JButton("Modify Shape");
          modButton.addActionListener(this);
          p.add(modButton);
        }

        return p;
      }

       //Modify button has been clicked, pop up the path editor
      public void actionPerformed(ActionEvent arg0) {
        PolygonConfigurer config = new PolygonConfigurer(editZone, this);
        config.setVisible(true);

        //
        if (!config.isCancelled()) {
          editZone.setPath(config.getPoints());
          nameField.setText(calcPath(editZone.getPolygon()));
        }
      }
    }

    public class PolygonConfigurer extends JDialog implements ActionListener {

      ArrayList points = new ArrayList();
      private JPanel topPanel;
      private JPanel headerPanel;
      private JPanel bodyPanel;
      private JPanel optionPanel;
      private JPanel scrollPanel;
      private JPanel visPanel;
      private PolygonVisualizer visualizer;
      private JPanel buttonPanel;
      private JScrollPane scroll;
      private ArrayList rowPanels;
      private int selectedRow = -1;
      private boolean cancelled = false;
      private Zone myZone;
      private PathConfigurer myPathConfigurer;

      private JButton okButton, canButton;

      protected PolygonConfigurer(Zone zone, PathConfigurer pathConfig) {

        myZone = zone;
        myPathConfigurer = pathConfig;

        Polygon poly = zone.getPolygon();
        setModal(true);

        setTitle(zone.getName() + " - Edit Shape");
        setBackground(Color.gray);

        if (zone.pathPoints == null) {
          points = new ArrayList();
        }
        else {
          points = new ArrayList(zone.pathPoints);
        }

        headerPanel = new JPanel();

        scrollPanel = new JPanel();
        scrollPanel.setLayout(new BoxLayout(scrollPanel, BoxLayout.Y_AXIS));
        scrollPanel.setBorder(BorderFactory.createLineBorder(Color.black));
        rowPanels = new ArrayList();

        for (int i = 0; i < points.size(); i++) {
          Row line = new Row(i, points, this);
          scrollPanel.add(line);
          rowPanels.add(i, line);
        }

        scroll =
          new JScrollPane(
            scrollPanel,
            JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        buttonPanel = new JPanel();
        okButton = new JButton(OK_ACTION);
        okButton.addActionListener(this);
        canButton = new JButton(CANCEL_ACTION);
        canButton.addActionListener(this);
        buttonPanel.add(okButton);
        buttonPanel.add(canButton);

        optionPanel = new JPanel();
        optionPanel.setLayout(new BoxLayout(optionPanel, BoxLayout.Y_AXIS));
        addAButton(UP_ACTION, optionPanel);
        addAButton(DOWN_ACTION, optionPanel);
        addAButton(ADD_ACTION, optionPanel);
        addAButton(REMOVE_ACTION, optionPanel);

        visPanel = new JPanel();
        visualizer = new PolygonVisualizer(points);
        visPanel.add(visualizer);

        bodyPanel = new JPanel();
        bodyPanel.setLayout(new BoxLayout(bodyPanel, BoxLayout.X_AXIS));
        bodyPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        bodyPanel.add(optionPanel);
        bodyPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        bodyPanel.add(scrollPanel);
        bodyPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        bodyPanel.add(visPanel);
        bodyPanel.add(Box.createRigidArea(new Dimension(5, 0)));

        getContentPane().add(headerPanel, BorderLayout.PAGE_START);
        getContentPane().add(bodyPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.PAGE_END);

        pack();
      }

      private void setZone(Zone z) {
        myZone = z;
      }

      private Zone getZone() {
        return myZone;
      }

      private void addAButton(String text, Container container) {
        JButton button = new JButton(text);
        button.addActionListener(this);
        button.setAlignmentX(Component.CENTER_ALIGNMENT);
        container.add(button);
      }

      public void reVisualize() {
        visualizer.setPoly(points);
        visualizer.repaint();
      }

      public class PolygonVisualizer extends JLabel {

        private static final int SIZE = 100;
        private static final int INSET = 5;
        private Polygon myPoly;
        private Polygon drawPoly;
        private int selectedVertex = -1;

        public PolygonVisualizer() {
          setPreferredSize(new Dimension(SIZE, SIZE));
        }

        public PolygonVisualizer(ArrayList v) {
          this();
          setPoly(v);
          repaint();
        }

        public void setPoly(ArrayList v) {
          myPoly = new Polygon();
          Iterator it = v.iterator();
          while (it.hasNext()) {
            Point p = (Point) it.next();
            myPoly.addPoint(p.x, p.y);
          }

          // Shrink
          Rectangle r = myPoly.getBounds();
          int max = r.height > r.width ? r.height : r.width;
          float factor = (float) max / (float) (SIZE - (INSET * 2));
          drawPoly = new Polygon();
          for (int i = 0; i < myPoly.npoints; i++) {
            drawPoly.addPoint(
              (int) (myPoly.xpoints[i] / factor),
              (int) (myPoly.ypoints[i] / factor));
          }

          // And move
          r = drawPoly.getBounds();
          int dx = -r.x;
          int dy = -r.y;
          drawPoly.translate(dx+INSET, dy+INSET);
        }

        public void selectVertex(int vertex) {
          selectedVertex = vertex;
          repaint();
        }

        protected void paintComponent(Graphics g) {

          g.drawPolygon(drawPoly);
          if (selectedVertex >= 0 && selectedVertex < drawPoly.npoints) {
            int x = drawPoly.xpoints[selectedVertex];
            int y = drawPoly.ypoints[selectedVertex];
            g.fillRect(x-2, y-2, 5, 5);
          }

        }
      }

      private void updateScrollPane() {

        // Add a new Row if necessary
        if (points.size() > rowPanels.size()) {
          Row newRow = new Row(points.size() - 1, points, this);
          scrollPanel.add(newRow);
          rowPanels.add(newRow);
        }

        // Fill in the data
        for (int i = 0; i < points.size(); i++) {
          Point p = (Point) points.get(i);
          Row row = (Row) rowPanels.get(i);
          row.setVisible(true);
          row.setRow(i);
          row.setPos(p);
          row.setHighlighted(selectedRow == i);
        }

        // Hide any extra rows
        for (int i = points.size(); i < rowPanels.size(); i++) {
          ((Row) rowPanels.get(i)).setVisible(false);
        }

        pack();
        reVisualize();

      }

      public boolean isCancelled() {
        return cancelled;
      }

      public ArrayList getPoints() {
        return points;
      }

      public void selectRow(int row) {
        if (row >= 0 && row <= points.size() - 1) {
          if (rowPanels.size() > 0) {
            ((Row) rowPanels.get(selectedRow)).setHighlighted(false);
          }
          ((Row) rowPanels.get(row)).setHighlighted(true);
          selectedRow = row;
          visualizer.selectVertex(row);
        }
      }

      public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals(OK_ACTION)) {
          cancelled = false;
          setVisible(false);
        }
        else if (command.equals(CANCEL_ACTION)) {
          cancelled = true;
          setVisible(false);
        }
        else if (command.equals(UP_ACTION)) {
          if (selectedRow > 0) {
            swapRows(selectedRow, selectedRow - 1);
            selectRow(selectedRow - 1);
          }
        }
        else if (command.equals(DOWN_ACTION)) {
          if (selectedRow < points.size() - 1) {
            swapRows(selectedRow, selectedRow + 1);
            selectRow(selectedRow + 1);
          }
        }
        else if (command.equals(ADD_ACTION)) {
          if (selectedRow < 0) {
            points.add(0, new Point(0, 0));
            updateScrollPane();
            selectRow(0);
          }
          else {
            points.add(selectedRow, new Point((Point) points.get(selectedRow)));
            updateScrollPane();
          }

        }
        else if (command.equals(REMOVE_ACTION)) {
          points.remove(selectedRow);
          updateScrollPane();
          if (selectedRow > points.size() - 1) {
            selectedRow = points.size() - 1;
          }
          selectRow(selectedRow);
        }
      }

      private void swapRows(int r1, int r2) {
        Point p = (Point) points.get(r1);
        points.set(r1, points.get(r2));
        points.set(r2, p);
        updateScrollPane();
      }
    }

    protected class Row extends JPanel implements FocusListener {

      protected int myRow;
      protected ArrayList myPoints;
      protected JTextField xPos, yPos;
      protected Zone.PathConfig.PolygonConfigurer myConfig;

      public void setHighlighted(boolean b) {
        if (b) {
          setBorder(BorderFactory.createBevelBorder(1));
        }
        else {
          setBorder(null);
        }
      }

      public Row(int i, ArrayList v, Zone.PathConfig.PolygonConfigurer c) {
        myRow = i;
        myPoints = v;
        myConfig = c;

        Point p = (Point) myPoints.get(myRow);

        JLabel label = new JLabel(i + 1 + "");
        add(label);

        xPos = new JTextField(p.x + "", 5);
        xPos.addFocusListener(this);
        xPos.addKeyListener(new KeyAdapter() {
          public void keyPressed(KeyEvent e) {
            myConfig.selectRow(myRow);
          }
          public void keyReleased(KeyEvent e) {
            try {
              int val = Integer.valueOf(xPos.getText()).intValue();
              Point myP = (Point) myPoints.get(myRow);
              myP.x = val;
              myConfig.reVisualize();
            }
            catch (Exception ev) {
              //rolls[myRow].setPlus(0);
            }
          }
        });
        add(xPos);
        yPos = new JTextField(p.y + "", 5);
        yPos.addFocusListener(this);
        yPos.addKeyListener(new KeyAdapter() {
          public void keyPressed(KeyEvent e) {
            myConfig.selectRow(myRow);
          }
          public void keyReleased(KeyEvent e) {
            try {
              int val = Integer.valueOf(yPos.getText()).intValue();
              Point myP = (Point) myPoints.get(myRow);
              myP.y = val;
              myConfig.reVisualize();
            }
            catch (Exception ev) {
              //rolls[myRow].setPlus(0);
            }
          }
        });
        add(yPos);

      }

      public void setPos(Point p) {
        xPos.setText(p.x + "");
        yPos.setText(p.y + "");
      }

      public void setRow(int row) {
        myRow = row;
      }

      public void focusGained(FocusEvent e) {
        myConfig.selectRow(myRow);
      }
      public void focusLost(FocusEvent e) {
        //setHighlighted(false);
      }
    }
*/

  public String[] getAttributeNames() {
    String s[] = {NAME, LOCATION_FORMAT, PATH};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      "Name",
      "Location Format",
      "Shape"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{
      String.class,
      LocationFormatConfig.class,
      String.class};
  }

  public static class LocationFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{NAME, GRID_LOCATION});
    }
  }

  public void addTo(Buildable b) {
    ((ZonedGrid) b).addZone(this);
  }

  public void removeFrom(Buildable b) {
    ((ZonedGrid) b).removeZone(this);
  }

  public static String getConfigureTypeName() {
    return "Zone";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (PATH.equals(key)) {
      return polygonToString(myPolygon);
    }
    else if (LOCATION_FORMAT.equals(key)) {
      return locationFormat;
    }
    return null;
  }

  public void setAttribute(String key, Object val) {
    if (val == null)
      return;

    if (NAME.equals(key)) {
      setConfigureName((String) val);
    }
    else if (PATH.equals(key)) {
      setPolygon((String) val);
    }
    else if (LOCATION_FORMAT.equals(key)) {
      locationFormat = (String) val;
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{HexGrid.class, SquareGrid.class, RegionGrid.class};
  }

  public String locationName(Point p) {
    format.setFormat(locationFormat);
    format.setProperty(NAME, getConfigureName());
    String gridLocation = null;
    if (grid != null) {
      gridLocation = grid.locationName(p);
    }
    format.setProperty(GRID_LOCATION, gridLocation);
    return format.getText();
  }

  public boolean contains(Point p) {
    return myPolygon.contains(p);
  }

  public static String polygonToString(Polygon p) {
    StringBuffer s = new StringBuffer();
    for (int i = 0; i < p.npoints; i++) {
      s.append(Math.round(p.xpoints[i])).append(',').append(Math.round(p.ypoints[i]));
      if (i < (p.npoints - 1)) {
        s.append(';');
      }
    }
    return s.toString();
  }

  public void setPolygon(String path) {
    myPolygon.reset();

    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(path, ';');
    while (sd.hasMoreTokens()) {
      String s = sd.nextToken();
      SequenceEncoder.Decoder pd = new SequenceEncoder.Decoder(s, ',');
      if (pd.hasMoreTokens()) {
        try {
          int x = Integer.parseInt(pd.nextToken());
          if (pd.hasMoreTokens()) {
            int y = Integer.parseInt(pd.nextToken());
            myPolygon.addPoint(x, y);
          }
        }
        catch (NumberFormatException e) {
          e.printStackTrace();
        }
      }
    }
  }
//  private void checkRectangular() {
//    if (pathPoints.size() == 4) {
//      Point p1 = (Point) pathPoints.get(0);
//      Point p2 = (Point) pathPoints.get(1);
//      Point p3 = (Point) pathPoints.get(2);
//      Point p4 = (Point) pathPoints.get(3);
//      if (p1.x == p2.x) {
//        if (p2.y == p3.y && p3.x == p4.x && p4.y == p1.y) {
//          box = new Rectangle(p1.x, p1.y, (p4.x - p1.x), (p2.y - p1.y));
//        }
//      }
//      else if (p1.y == p2.y) {
//        if (p2.x == p3.x && p3.y == p4.y && p4.x == p1.x) {
//          box = new Rectangle(p1.x, p1.y, (p2.x - p1.x), (p4.y - p1.y));
//        }
//      }
//    }
//  }

  /**
   * Snap to the grid in this zone,
   */
  public Point snapTo(Point p) {
    Point snap = p;
    if (grid != null) {
      snap = grid.snapTo(p);
    }
    return snap;
  }

  public Dimension getSize() {
    return myPolygon.getBounds().getSize();
  }

  public void removeGrid(MapGrid grid) {
    if (this.grid == grid) {
      grid = null;
    }
  }

  public void setGrid(MapGrid m) {
    grid = m;
  }

  public MapGrid getGrid() {
    return grid;
  }

  public Shape getShape() {
    return myPolygon;
  }

/*
  public Shape getShape(double zoom) {
    Polygon p = new Polygon();
    for (int i = 0; i < myPolygon.npoints; i++) {
      int x = (int) (myPolygon.xpoints[i] * zoom);
      int y = (int) (myPolygon.ypoints[i] * zoom);
      p.addPoint(x, y);
    }
    return p;
  }

  public Polygon getPolygon() {
    return myPolygon;
  }
*/

  public Rectangle getBounds() {
    Rectangle r = myPolygon.getBounds();
    return r;
  }

  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    Graphics2D g2d = (Graphics2D)g;
    Shape oldClip = g2d.getClip();
    Area newClip = new Area(visibleRect);
    newClip.intersect(new Area(myPolygon));
    g2d.setClip(newClip);
    if (grid != null) {
      grid.draw(g, bounds, visibleRect, scale, reversed);
    }
    g2d.setClip(oldClip);
  }

/*
  public boolean isRectangle() {
    return (box != null);
  }

  public void setShape(Rectangle r) {
    ;
  }

*/
/*
  public void move(int dx, int dy, JComponent c) {
    Rectangle oldRect = new Rectangle(getBounds());
    myPolygon.translate(dx, dy);
    c.repaint(oldRect.union(getBounds()));
  }
*/

/*
  public void drawGrid(
    Graphics g,
    Rectangle bounds,
    Rectangle visibleRect,
    double scale,
    boolean reversed,
    Component obs) {
      if (grid != null) {
        if (grid.isVisible()) {
          grid.draw(g, bounds, visibleRect, scale, reversed, getShape(scale));
        }
        if (grid.getGridNumbering() != null
          && grid.getGridNumbering().isVisible()) {
          grid.getGridNumbering().draw(
            g,
            bounds,
            visibleRect,
            scale,
            reversed,
            getShape(scale));
        }
      }
    }
*/

/*
  public void draw(
    Graphics g,
    Rectangle bounds,
    Rectangle visibleRect,
    double scale,
    boolean reversed,
    Component obs) {

    if (!bounds.intersects(visibleRect)) {
      return;
    }

    drawGrid(g, bounds, visibleRect, scale, reversed, obs);

    if (isHighlighted()) {
      drawHighlight(g, bounds, visibleRect, scale, obs);
    }
  }

*/
/*
  public void drawSelection(
    Graphics g,
    Rectangle bounds,
    Rectangle visibleRect,
    double scale,
    Component obs) {

    Point location = new Point(bounds.x, bounds.y);

    getBoard().drawShape(g, location, visibleRect, scale, obs, getShape());
    drawGrid(g, bounds, visibleRect, scale, board.isReversed(), obs);

  }
*/

/*
  public void drawHighlight(
    Graphics g,
    Rectangle bounds,
    Rectangle visibleRect,
    double scale,
    Component obs) {

    Graphics2D g2 = (Graphics2D) g;

    Color oldColor = g2.getColor();
    Stroke oldStroke = g2.getStroke();

    g2.setColor(Color.RED);
    g2.setStroke(new BasicStroke(2F));

    g2.drawPolygon(getPolygon());

    g2.setColor(oldColor);
    g2.setStroke(oldStroke);
  }

  public void setSelected(boolean b) {
    selected = b;
  }

  public boolean isSelected() {
    return selected;
  }

  public void setHighlighted(boolean b) {
    highlighted = b;
  }

  private boolean isHighlighted() {
    return highlighted;
  }
*/

}
