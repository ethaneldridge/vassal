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
package VASSAL.build.module.map.boardPicker.board;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;
import VASSAL.build.module.map.boardPicker.board.mapgrid.HexGridNumbering;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.*;

import java.awt.*;
import java.io.File;
import java.net.MalformedURLException;

/**
 * A Hexgrid is a map grid composed of hexes.
 */
public class HexGrid extends AbstractConfigurable implements MapGrid {
  protected Point origin = new Point(0, 32);

  protected double dx;
  protected double dy;

  protected Board board;

  protected GridNumbering numbering;

  protected boolean visible = false;
  protected boolean dotsVisible = false;
  protected boolean edgesLegal = false;
  protected boolean cornersLegal = false;
  protected Color color = Color.black;
  protected boolean sideways = false;

  public static final String X0 = "x0";
  public static final String Y0 = "y0";
  public static final String DY = "dy";
  public static final String DX = "dx";
  public static final String VISIBLE = "visible";
  public static final String DOTS_VISIBLE = "dotsVisible";
  public static final String CORNERS = "cornersLegal";
  public static final String EDGES = "edgesLegal";
  public static final String SIDEWAYS = "sideways";
  public static final String COLOR = "color";

  public String[] getAttributeNames() {
    String s[] = {SIDEWAYS, X0, Y0, DY, DX, EDGES, CORNERS, VISIBLE, DOTS_VISIBLE, COLOR};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Sideways (hexrows go horizontal)",
                        "X offset",
                        "Y offset",
                        "Hex Height",
                        "Hex Width",
                        "Edges are legal locations",
                        "Vertices are legal locations",
                        "Show grid",
                        "Draw center dots",
                        "Color"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Boolean.class,
                       Integer.class,
                       Integer.class,
                       Double.class,
                       Double.class,
                       Boolean.class,
                       Boolean.class,
                       Boolean.class,
                       Boolean.class,
                       Color.class};
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (COLOR.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return visible;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public Configurer getConfigurer() {
    AutoConfigurer c = (AutoConfigurer) super.getConfigurer();
    final Configurer dxConfig = c.getConfigurer(DX);
    c.getConfigurer(DY).addPropertyChangeListener
        (new java.beans.PropertyChangeListener() {
          public void propertyChange(java.beans.PropertyChangeEvent evt) {
            if (evt.getNewValue() != null) {
              double hgt = ((Double) evt.getNewValue()).doubleValue();
              dxConfig.setValue(new Double(Math.sqrt(3) * hgt / 2.).toString());
            }
          }
        }
        );

    return config;
  }


  protected boolean alternate = false;// true if hex B1 is above A1

  public HexGrid(double height, double width, boolean alt) {
    dy = height;
    dx = width;
    alternate = alt;
  }

  public HexGrid(double size, boolean alt) {
    this(size, Math.sqrt(3) * size / 2., alt);
  }

  public HexGrid() {
    this(64.0, false);
  }

  public boolean isVisible() {
    return visible;
  }

  public boolean isEdgesLegal() {
    return edgesLegal;
  }

  public boolean isCornersLegal() {
    return cornersLegal;
  }

  public void setVisible(boolean legal) {
    visible = legal;
  }

  public void setEdgesLegal(boolean legal) {
    edgesLegal = legal;
  }

  public boolean isSideways() {
    return sideways;
  }

  public void setCornersLegal(boolean legal) {
    cornersLegal = legal;
  }

  public void setHexSize(double size) {
    dy = size;
    dx = Math.sqrt(3) * size / 2.;
  }

  public double getHexSize() {
    return dy;
  }

  public double getHexWidth() {
    return dx;
  }

  public void setHexWidth(double w) {
    dx = w;
  }

  public Board getBoard() {
    return board;
  }

  public void addTo(Buildable b) {
    board = (Board) b;
    ((Board) b).setGrid(this);
  }

  public void removeFrom(Buildable b) {
    if (((Board) b).getGrid() == this)
      ((Board) b).setGrid(null);
  }

  public static String getConfigureTypeName() {
    return "Hex Grid";
  }

  public String getConfigureName() {
    return null;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "HexGrid.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public String getAttributeValueString(String key) {
    if (X0.equals(key)) {
      return "" + origin.x;
    }
    else if (Y0.equals(key)) {
      return "" + origin.y;
    }
    else if (DY.equals(key)) {
      return "" + dy;
    }
    else if (DX.equals(key)) {
      return "" + dx;
    }
    else if (CORNERS.equals(key)) {
      return "" + cornersLegal;
    }
    else if (EDGES.equals(key)) {
      return "" + edgesLegal;
    }
    else if (SIDEWAYS.equals(key)) {
      return "" + sideways;
    }
    else if (VISIBLE.equals(key)) {
      return "" + visible;
    }
    else if (DOTS_VISIBLE.equals(key)) {
      return "" + dotsVisible;
    }
    else if (COLOR.equals(key)) {
      return visible ? ColorConfigurer.colorToString(color) : null;
    }
    return null;
  }

  public void setAttribute(String key, Object val) {
    if (val == null)
      return;
    if (X0.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }
      origin.x = ((Integer) val).intValue();
    }
    else if (Y0.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }
      origin.y = ((Integer) val).intValue();
    }
    else if (DY.equals(key)) {
      if (val instanceof String) {
        val = new Double((String) val);
      }
      dy = ((Double) val).doubleValue();
      if (dx == Math.sqrt(3) * 64.0 / 2.) {
        dx = Math.sqrt(3) * dy / 2.;
      }
    }
    else if (DX.equals(key)) {
      if (val instanceof String) {
        val = new Double((String) val);
      }
      dx = ((Double) val).doubleValue();
    }
    else if (CORNERS.equals(key)) {
      if (val instanceof String) {
        val = new Boolean((String) val);
      }
      cornersLegal = ((Boolean) val).booleanValue();
    }
    else if (EDGES.equals(key)) {
      if (val instanceof String) {
        val = new Boolean((String) val);
      }
      edgesLegal = ((Boolean) val).booleanValue();
    }
    else if (SIDEWAYS.equals(key)) {
      if (val instanceof String) {
        val = new Boolean((String) val);
      }
      sideways = ((Boolean) val).booleanValue();
    }
    else if (VISIBLE.equals(key)) {
      if (val instanceof String) {
        val = new Boolean((String) val);
      }
      visible = ((Boolean) val).booleanValue();
    }
    else if (DOTS_VISIBLE.equals(key)) {
      if (val instanceof String) {
        val = new Boolean((String) val);
      }
      dotsVisible = ((Boolean) val).booleanValue();
    }
    else if (COLOR.equals(key)) {
      if (val instanceof String) {
        val = ColorConfigurer.stringToColor((String) val);
      }
      color = (Color) val;
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{HexGridNumbering.class};
  }

  public String locationName(Point p) {
    return numbering == null ? null : numbering.locationName(p);
  }

  public Point getLocation(String hex) throws MapGrid.BadCoords {
    throw new MapGrid.BadCoords("No naming scheme specified");
  }

  public Point snapTo(Point p) {
    if (edgesLegal && cornersLegal) {
      Point edge = snapToHexSide(p);
      Point vertex = snapToHexVertex(p);
      if ((p.x - edge.x) * (p.x - edge.x)
          + (p.y - edge.y) * (p.y - edge.y)
          < (p.x - vertex.x) * (p.x - vertex.x)
          + (p.y - vertex.y) * (p.y - vertex.y)) {
        return edge;
      }
      else {
        return vertex;
      }
    }
    else if (edgesLegal) {
      return snapToHexSide(p);
    }
    else if (cornersLegal) {
      return snapToHexVertex(p);
    }
    else {
      return snapToHex(p);
    }
  }

  /**
   * @return the nearest hex center
   */
  public Point snapToHex(Point p) {
    p = new Point(p);
    rotateIfSideways(p);
    p.setLocation(hexX(p.x, p.y), hexY(p.x, p.y));
    rotateIfSideways(p);
    return p;
  }

  /**
   * @return the nearest hex center or hexside
   */
  public Point snapToHexSide(Point p) {
    p = new Point(p);
    rotateIfSideways(p);
    p.setLocation(sideX(p.x, p.y), sideY(p.x, p.y));
    rotateIfSideways(p);
    return p;
  }

  /**
   * @return the nearest hex center or vertex
   */
  public Point snapToHexVertex(Point p) {
    p = new Point(p);
    rotateIfSideways(p);
    p.setLocation(vertexX(p.x, p.y), vertexY(p.x, p.y));
    rotateIfSideways(p);
    return p;
  }

  public void rotate(Point p) {
    int swap = p.x;
    p.x = p.y;
    p.y = swap;
  }

  public void rotateIfSideways(Point p) {
    if (sideways) {
      rotate(p);
    }
  }


  public int range(Point p1, Point p2) {
    p1 = new Point(p1);
    rotateIfSideways(p1);
    p2 = new Point(p2);
    rotateIfSideways(p2);
    int x = p2.x - p1.x;
    int y = p2.y - p1.y;
    double theta = Math.atan2((double) (-x), (double) (-y)) + Math.PI;
    while (theta > Math.PI / 3.)
      theta -= Math.PI / 3.;
    theta = Math.PI / 6. - theta;
    double r = Math.sqrt((double) (x * x + y * y));
    r *= Math.cos(theta);
    return (int) (r / (dy * Math.sqrt(3) / 2.) + 0.5);
  }

  protected int hexX(int x, int y) {
    return ((int) (dx * (int) Math.floor((x - origin.x + dx / 2) / dx) + origin.x));
  }

  protected int hexY(int x, int y) {
    int nx = (int) Math.floor((x - origin.x + dx / 2) / dx);
    if (nx % 2 == 0)
      return ((int)
          (dy * (int) Math.floor((y - origin.y + dy / 2) / dy) + origin.y));
    else
      return ((int)
          (dy * (int) Math.floor((y - origin.y) / dy) + (int) (dy / 2) + origin.y));
  }

  protected int sideX(int x, int y) {
    return ((int) (dx / 2 * (int) Math.floor((x - origin.x + dx / 4) * 2 / dx) + origin.x));
  }

  protected int sideY(int x, int y) {
    int nx = (int) Math.floor((x - origin.x + dx / 4) * 2 / dx);
    if (nx % 2 == 0) {
      return ((int) (dy / 2 * (int) Math.floor((y - origin.y + dy / 4) * 2 / dy) + origin.y));
    }
    else {
      return ((int) ((dy / 2) * (int) Math.floor((y - origin.y) * 2 / dy) + (int) (dy / 4) + origin.y));
    }
  }

  protected int vertexX(int x, int y) {
    int ny = (int) Math.floor((y - origin.y + dy / 4) * 2 / dy);
    if (ny % 2 == 0) {
      return ((int) (2 * dx / 3 * (int) (Math.floor(x - origin.x + dx / 3) * 3 / (2 * dx)) + origin.x));
    }
    else {
      return ((int) (2 * dx / 3 * (int) (Math.floor(x - origin.x + dx / 3 + dx / 3) * 3 / (2 * dx))
          - (int) (dx / 3) + origin.x));
    }
  }

  protected int vertexY(int x, int y) {
    return ((int) (dy / 2 * (int) Math.floor((y - origin.y + dy / 4) * 2 / dy) + origin.y));
  }

  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double zoom, boolean reversed) {
    if (!bounds.intersects(visibleRect)) {
      return;
    }
    if (g instanceof Graphics2D) {
      ((Graphics2D) g).addRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                                            RenderingHints.VALUE_ANTIALIAS_ON));
    }

    g.setColor(color == null ? Color.black : color);

    float x1,y1, x2,y2, x3,y3, x4, y4;

    float deltaX = (float) (this.dx * zoom);
    float deltaY = (float) (this.dy * zoom);

    float r = 2.F * deltaX / 3.F;

    Rectangle region = bounds.intersection(visibleRect);

    Shape oldClip = g.getClip();
    g.setClip(region.x, region.y, region.width, region.height);

    if (sideways) {
      bounds = new Rectangle(bounds.y, bounds.x, bounds.height, bounds.width);
      region = new Rectangle(region.y, region.x, region.height, region.width);
    }

    float xmin = reversed ? bounds.x + (float) zoom * origin.x + bounds.width - 2 * deltaX * (float) Math.ceil((bounds.x + zoom * origin.x + bounds.width - region.x) / (2 * deltaX))
        : bounds.x + (float) zoom * origin.x + 2 * deltaX * (float) Math.floor((region.x - bounds.x - zoom * origin.x) / (2 * deltaX));
    float xmax = region.x + region.width + 2 * deltaX;
    float ymin = reversed ? bounds.y + (float) zoom * origin.y + bounds.height - deltaY * (float) Math.ceil((bounds.y + zoom * origin.y + bounds.height - region.y) / deltaY)
        : bounds.y + (float) zoom * origin.y + deltaY * (float) Math.floor((region.y - bounds.y - zoom * origin.y) / deltaY);
    float ymax = region.y + region.height + deltaY;

    Point center = new Point();
    Point p1 = new Point();
    Point p2 = new Point();
    Point p3 = new Point();
    Point p4 = new Point();

    // x,y is the center of a hex
    for (float x = xmin; x < xmax; x += zoom * 2 * dx) {
      for (float y = ymin; y < ymax; y += zoom * dy) {
        x1 = x - r;
        y1 = y;
        p1.setLocation(Math.round(x1), Math.round(y1));
        x2 = x - .5F * r;
        y2 = reversed ? y + .5F * deltaY : y - .5F * deltaY;
        p2.setLocation(Math.round(x2), Math.round(y2));
        x3 = x + .5F * r;
        y3 = y2;
        p3.setLocation(Math.round(x3), Math.round(y3));
        x4 = x + r;
        y4 = y;
        p4.setLocation(Math.round(x4), Math.round(y4));
        if (sideways) {
          rotate(p1);
          rotate(p2);
          rotate(p3);
          rotate(p4);
        }
        g.drawLine(p1.x, p1.y, p2.x, p2.y);
        g.drawLine(p2.x, p2.y, p3.x, p3.y);
        g.drawLine(p3.x, p3.y, p4.x, p4.y);
        if (dotsVisible) {
          center.setLocation(Math.round(x), Math.round(y));
          rotateIfSideways(center);
          g.fillRect(center.x, center.y, 2, 2);
          center.setLocation(Math.round(x + deltaX), Math.round(y + deltaY / 2));
          rotateIfSideways(center);
          g.fillRect(center.x, center.y, 2, 2);
        }
        x1 += deltaX;
        x2 += deltaX;
        x3 += deltaX;
        x4 += deltaX;
        y1 += .5F * deltaY;
        y2 += .5F * deltaY;
        y3 += .5F * deltaY;
        y4 += .5F * deltaY;
        p1.setLocation(Math.round(x1), Math.round(y1));
        p2.setLocation(Math.round(x2), Math.round(y2));
        p3.setLocation(Math.round(x3), Math.round(y3));
        p4.setLocation(Math.round(x4), Math.round(y4));
        if (sideways) {
          rotate(p1);
          rotate(p2);
          rotate(p3);
          rotate(p4);
        }
        g.drawLine(p1.x, p1.y, p2.x, p2.y);
        g.drawLine(p2.x, p2.y, p3.x, p3.y);
        g.drawLine(p3.x, p3.y, p4.x, p4.y);
        if (x == xmin) {
          p1.setLocation(Math.round(x - r), Math.round(y));
          p2.setLocation(Math.round(x - r / 2), Math.round(y + deltaY / 2));
          if (sideways) {
            rotate(p1);
            rotate(p2);
          }
          g.drawLine(p1.x, p1.y, p2.x, p2.y);
        }
      }
    }
    g.setClip(oldClip);
  }

  public javax.swing.JPanel configurePanel() {
    return new ConfigureHexGrid();
  }


  public void setGridNumbering(GridNumbering numbering) {

    this.numbering = numbering;

  }


  public GridNumbering getGridNumbering() {

    return numbering;

  }


  public Point getOrigin() {

    return new Point(origin);

  }


  private class ConfigureHexGrid extends javax.swing.JPanel {

    /** Initializes the Form */
    public ConfigureHexGrid() {
      initComponents();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the FormEditor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
      setLayout(new javax.swing.BoxLayout(this, javax.swing.BoxLayout.X_AXIS));

      javax.swing.Box numbers = javax.swing.Box.createVerticalBox();

      jPanel1 = new javax.swing.JPanel();
      jPanel1.setLayout(new java.awt.FlowLayout());

      jLabel1 = new javax.swing.JLabel();
      jLabel1.setText("Hex height");
      jPanel1.add(jLabel1);

      dyInput = new javax.swing.JTextField();
      dyInput.setText("" + dy);
      jPanel1.add(dyInput);
      numbers.add(jPanel1);

      jPanel2 = new javax.swing.JPanel();
      jPanel2.setLayout(new java.awt.FlowLayout());

      jLabel2 = new javax.swing.JLabel();
      jLabel2.setText("Offset");
      jPanel2.add(jLabel2);
      x0Input = new javax.swing.JTextField(3);
      x0Input.setText("" + origin.x);
      jPanel2.add(x0Input);
      jPanel2.add(new javax.swing.JLabel(","));
      y0Input = new javax.swing.JTextField(3);
      y0Input.setText("" + origin.y);
      jPanel2.add(y0Input);
      numbers.add(jPanel2);

      javax.swing.Box boxes = javax.swing.Box.createVerticalBox();

      visibleBox = new javax.swing.JCheckBox("Draw Grid");
      visibleBox.setSelected(HexGrid.this.visible);
      boxes.add(visibleBox);
      edgeBox = new javax.swing.JCheckBox("Edges Legal");
      edgeBox.setSelected(edgesLegal);
      boxes.add(edgeBox);
      cornerBox = new javax.swing.JCheckBox("Corners Legal");
      cornerBox.setSelected(cornersLegal);
      boxes.add(cornerBox);

      add(numbers);
      add(boxes);

      java.awt.event.FocusListener fl =
          new java.awt.event.FocusAdapter() {
            public void focusLost(java.awt.event.FocusEvent e) {
              setValues();
            }
          };
      dyInput.addFocusListener(fl);
      x0Input.addFocusListener(fl);
      y0Input.addFocusListener(fl);

      java.awt.event.ActionListener al =
          new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent e) {
              setValues();
            }
          };
      dyInput.addActionListener(al);
      x0Input.addActionListener(al);
      y0Input.addActionListener(al);
      visibleBox.addActionListener(al);
      edgeBox.addActionListener(al);
      cornerBox.addActionListener(al);
    }//GEN-END:initComponents

    private void setValues() {
      HexGrid.this.visible = visibleBox.isSelected();
      edgesLegal = edgeBox.isSelected();
      cornersLegal = cornerBox.isSelected();
      try {
        setHexSize(Double.valueOf(dyInput.getText()).doubleValue());
      }
      catch (NumberFormatException NANdy) {
        setHexSize(64.);
        dyInput.setText("64");
      }
      try {
        origin.x = Integer.parseInt(x0Input.getText());
      }
      catch (NumberFormatException NANx0) {
        origin.x = 0;
        x0Input.setText("0");
      }
      try {
        origin.y = Integer.parseInt(y0Input.getText());
      }
      catch (NumberFormatException NANy0) {
        origin.y = 0;
        y0Input.setText("0");
      }
    }

    private javax.swing.JCheckBox visibleBox;
    private javax.swing.JCheckBox edgeBox;
    private javax.swing.JCheckBox cornerBox;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JTextField dyInput;
    private javax.swing.JTextField x0Input;
    private javax.swing.JTextField y0Input;
  }
}

