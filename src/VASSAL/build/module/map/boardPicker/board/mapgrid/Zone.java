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
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.*;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.tools.FormattedString;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
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
  private ZonedGrid parentGrid;

  public Zone() {
    myPolygon = new Polygon();
  }

  public String getName() {
    return name;
  }

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
      ShapeEditor.class};
  }

  public static class LocationFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{NAME, GRID_LOCATION});
    }
  }

  public static class ShapeEditor implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new Editor((Zone) c);
    }
  }

  public void addTo(Buildable b) {
    parentGrid = (ZonedGrid) b;
    parentGrid.addZone(this);
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
      return PolygonEditor.polygonToString(myPolygon);
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
      PolygonEditor.reset(myPolygon,(String) val);
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

  public Board getBoard() {
    return parentGrid.getBoard();
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

  public Rectangle getBounds() {
    Rectangle r = myPolygon.getBounds();
    return r;
  }

  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    Graphics2D g2d = (Graphics2D) g;
    Shape oldClip = g2d.getClip();
    Area newClip = new Area(visibleRect);
    AffineTransform transform = AffineTransform.getScaleInstance(scale, scale);
    transform.translate(bounds.x, bounds.y);
    Shape s = transform.createTransformedShape(myPolygon);
    newClip.intersect(new Area(s));
    g2d.setClip(newClip);
    if (grid != null) {
      grid.draw(g, bounds, visibleRect, scale, reversed);
    }
    g2d.setClip(oldClip);
  }

  public static class Editor extends Configurer {
    private JButton button;
    private PolygonEditor editor;
    private Board board;
    private JFrame frame;

    public Editor(final Zone zone) {
      super(PATH, null);
      button = new JButton("Define Shape");
      button.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          init(zone);
        }
      });
      editor = new PolygonEditor(new Polygon(zone.myPolygon.xpoints,zone.myPolygon.ypoints,zone.myPolygon.npoints)) {
        protected void paintBackground(Graphics g) {
          if (board != null) {
            board.draw(g, 0, 0, 1.0, editor);
          }
          else {
            super.paintBackground(g);
          }
        }
      };
      frame = new JFrame(zone.getConfigureName());
      frame.getContentPane().setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
      JPanel labels = new JPanel();
      labels.setLayout(new GridLayout(2,2));
      labels.add(new JLabel("Drag to create initial shape"));
      labels.add(new JLabel("Right-click to add point"));
      labels.add(new JLabel("Left-click to move points"));
      labels.add(new JLabel("DEL to remove point"));
      frame.getContentPane().add(labels);
      frame.getContentPane().add(new JScrollPane(editor));
      JPanel buttonPanel = new JPanel();
      JButton closeButton = new JButton("Ok");
      closeButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setValue((Object) getValueString());
          frame.setVisible(false);
        }
      });
      buttonPanel.add(closeButton);
      frame.getContentPane().add(buttonPanel);
    }

    private void init(Zone zone) {
      board = zone.getBoard();
      if (board != null) {
        board.fixImage(editor);
      }
      editor.setPreferredSize(board != null ? board.getSize() : new Dimension(600,600));
      frame.pack();
      frame.setVisible(true);
    }

    public Component getControls() {
      return button;
    }

    public String getValueString() {
      return PolygonEditor.polygonToString(editor.getPolygon());
    }

    public void setValue(String s) {
      PolygonEditor.reset(editor.getPolygon(),s);
    }
  }

}
