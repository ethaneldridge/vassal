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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jul 25, 2002
 * Time: 11:46:35 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import VASSAL.build.Buildable;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.counters.Labeler;

import javax.swing.*;
import java.awt.*;

public class HexGridNumbering extends RegularGridNumbering {
  private HexGrid grid;
  private boolean stagger = true;

  public void addTo(Buildable parent) {
    grid = (HexGrid) parent;
    grid.setGridNumbering(this);
  }

  public static final String STAGGER = "stagger";

  public String[] getAttributeDescriptions() {
    String[] s = super.getAttributeDescriptions();
    String[] val = new String[s.length + 1];
    System.arraycopy(s, 0, val, 0, s.length);
    val[s.length] = "Odd-numbered rows numbered higher";
    return val;
  }

  public String[] getAttributeNames() {
    String[] s = super.getAttributeNames();
    String[] val = new String[s.length + 1];
    System.arraycopy(s, 0, val, 0, s.length);
    val[s.length] = STAGGER;
    return val;
  }

  public Class[] getAttributeTypes() {
    Class[] s = super.getAttributeTypes();
    Class[] val = new Class[s.length + 1];
    System.arraycopy(s, 0, val, 0, s.length);
    val[s.length] = Boolean.class;
    return val;
  }

  public void setAttribute(String key, Object value) {
    if (STAGGER.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      stagger = ((Boolean) value).booleanValue();
    }
    else {
      super.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (STAGGER.equals(key)) {
      return "" + stagger;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    int size = (int) (scale * fontSize + 0.5);
    if (size < 5 || !bounds.intersects(visibleRect)) {
      return;
    }

    Rectangle region = bounds.intersection(visibleRect);

    Shape oldClip = g.getClip();
    g.setClip(region.x,region.y,region.width,region.height);

    double deltaX = scale * grid.getHexWidth();
    double deltaY = scale * grid.getHexSize();

    if (grid.isSideways()) {
      bounds = new Rectangle(bounds.y,bounds.x,bounds.height,bounds.width);
      region = new Rectangle(region.y,region.x,region.height,region.width);
    }

    int minCol = reversed ? 2*(int)Math.ceil((bounds.x-scale*grid.getOrigin().x+bounds.width-region.x)/(2*deltaX))
      : 2*(int)Math.floor((region.x - bounds.x - scale * grid.getOrigin().x) / (2*deltaX));
    double xmin = reversed ? bounds.x-scale*grid.getOrigin().x+bounds.width - deltaX*minCol
      : bounds.x + scale * grid.getOrigin().x + deltaX * minCol;
    double xmax = region.x+region.width+deltaX;
    int minRow = reversed ? (int)Math.ceil((bounds.y-scale*grid.getOrigin().y+bounds.height-region.y)/deltaY)
    : (int)Math.floor((region.y - bounds.y - scale * grid.getOrigin().y) / deltaY);
    double ymin = reversed ? bounds.y-scale*grid.getOrigin().y+bounds.height - deltaY*minRow
      : bounds.y + scale * grid.getOrigin().y + deltaY * minRow;
    double ymax = region.y+region.height + deltaY;

    Font f = new Font("Dialog", Font.PLAIN, size);
    Point p = new Point();
    int alignment = Labeler.TOP;
    int offset = -(int)deltaY/2;
    if (grid.isSideways()) {
      alignment = Labeler.CENTER;
      offset = 0;
    }
    int column = minCol;
    int nextColumn = reversed ? -1 : 1;
    int nextRow = reversed ? -1 : 0;
    for (double x = xmin; x < xmax; x += 2*deltaX, column += 2*nextColumn) {
      int row = minRow;
      for (double y = ymin; y < ymax; y += deltaY, row += nextColumn) {
        p.setLocation((int)x,(int)y+offset);
        grid.rotateIfSideways(p);
        Labeler.drawLabel(g, getName(row, column),
                          p.x,
                          p.y,
                          f,
                          Labeler.CENTER,
                          alignment, color, null, null);
        p.setLocation((int)(x+deltaX),(int)(y+deltaY/2)+offset);
        grid.rotateIfSideways(p);
        Labeler.drawLabel(g, getName(row + nextRow, column + nextColumn),
                          p.x,
                          p.y,
                          f,
                          Labeler.CENTER,
                          alignment, color, null, null);
      }
    }
    g.setClip(oldClip);
  }

  public int getColumn(Point p) {
    p = new Point(p);
    grid.rotateIfSideways(p);
    int x = p.x - grid.getOrigin().x;

    x = (int) Math.floor(x / grid.getHexWidth() + 0.5);
    return x;
  }

  protected JComponent getGridVisualizer() {
    if (visualizer == null) {
      visualizer = new JPanel() {
        public void paint(Graphics g) {
          g.clearRect(0, 0, getWidth(), getHeight());
          Rectangle bounds = new Rectangle(0, 0, getWidth(), getHeight());
          grid.draw(g, bounds, bounds, 1.0, false);
          draw(g, bounds, bounds, 1.0, false);
        }

        public Dimension getPreferredSize() {
          return new Dimension(3 * (int) grid.getHexSize(), 3 * (int) grid.getHexWidth());
        }
      };
    }
    return visualizer;
  }

  public int getRow(Point p) {
    p = new Point(p);
    grid.rotateIfSideways(p);
    Point origin = grid.getOrigin();
    double dx = grid.getHexWidth();
    double dy = grid.getHexSize();
    int nx = (int) Math.floor((p.x - origin.x + dx / 2) / dx);
    int ny;
    if (nx % 2 == 0) {
      ny = (int) Math.floor((p.y - origin.y + dy / 2) / dy);
    }
    else {
      ny = (int) Math.floor((p.y - origin.y) / dy);
    }
    return ny;
  }

  public String getName(int row, int column) {
    if (column % 2 != 0
      && stagger) {
      row++;
    }
    return super.getName(row, column);
  }

  public void removeFrom(Buildable parent) {
    grid.setGridNumbering(null);
  }
}
