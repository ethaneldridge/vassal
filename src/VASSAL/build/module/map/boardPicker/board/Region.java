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
import VASSAL.counters.Labeler;

import java.awt.*;
import java.awt.geom.Area;

import javax.swing.JComponent;


public class Region extends AbstractConfigurable {

  private String name = "";
  private Point origin = new Point(0, 0);

  private RegionGrid myGrid;

  // Variables for the GUI configurer
  private Rectangle selectionRect = new Rectangle();
  private boolean selected = false;

  public Region() {
  }

  public Region(Point p) {
    origin = p;
  }

  public String getName() {
    return name;
  }

  public Rectangle getSelectionRect() {
    return selectionRect;
  }

  public Point getOrigin() {
    return origin;
  }

  public void setOrigin(Point p) {
    origin = p;
  }

  public void moveOrigin(int dx, int dy) {
    origin.translate(dx, dy);
  }

  public static final String NAME = "name";
  public static final String X = "originx";
  public static final String Y = "originy";

  public String[] getAttributeNames() {
    String s[] = {NAME, X, Y};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name",
                        "X Co-ord",
                        "Y Co-ord"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,
                       Integer.class,
                       Integer.class};
  }


  public void addTo(Buildable b) {
    myGrid = (RegionGrid) b;
    myGrid.addRegion(this);
  }

  public void removeFrom(Buildable b) {
    ((RegionGrid) b).removeRegion(this);
  }

  public static String getConfigureTypeName() {
    return "Region";
  }

  public String getConfigureName() {
    return name;
  }

  public boolean contains(Point p) {
    return selectionRect.contains(p);
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;
  }


  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return "" + name;
    }
    else if (X.equals(key)) {
      return "" + origin.x;
    }
    else if (Y.equals(key)) {
      return "" + origin.y;
    }
    return null;
  }

  public void setAttribute(String key, Object val) {
    if (val == null)
      return;

    if (NAME.equals(key)) {
      name = (String) val;
      setConfigureName(name);
      if (myGrid != null) myGrid.addRegion(this);
    }
    else if (X.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }
      origin.x = ((Integer) val).intValue();
    }
    else if (Y.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }
      origin.y = ((Integer) val).intValue();
    }

  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String locationName() {
    return name;
  }

  public boolean isSelected() {
    return selected;
  }

  public void setSelected(boolean s) {
    selected = s;
  }

  /*
   * Move the region. Ensure the selectable region remains within
   * the panel
   */
  public void move(int dx, int dy, JComponent c) {
    Rectangle newRect = selectionRect;
    newRect.translate(dx, dy);

    if (c.getVisibleRect().contains(newRect)) {
      moveOrigin(dx, dy);
    }
    return;
  }

  /*
   * If the grid is visible, draw a dot and a label. Mainly of use for testing
   * a newly created grid.
   */

  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    if (!bounds.intersects(visibleRect)) {
      return;
    }

    final int labelOffset = 7;

    int size = (int) (scale * myGrid.getFontSize() + 0.5);
    Font f = new Font("Dialog", Font.PLAIN, size);

    Color fg = selected ? Color.white : Color.black;
    Color bg = selected ? Color.black : Color.white;

    Rectangle region = bounds.intersection(visibleRect);

    Shape oldClip = g.getClip();
    Area clipArea = new Area(oldClip);
    clipArea.intersect(new Area(region));
    g.setClip(clipArea);

    int posX = (int) (scale * origin.x + 0.5) + bounds.x - 1;
    int posY = (int) (scale * origin.y + 0.5) + bounds.y - 1;

    Color saveColor = g.getColor();

    g.setColor(bg);
    g.fillRect(posX, posY, 3, 3);
    g.setColor(fg);
    g.drawRect(posX, posY, 3, 3);

    g.setColor(saveColor);

    Labeler.drawLabel(g, name, posX, posY + labelOffset, f, Labeler.CENTER,
                      Labeler.TOP, fg, bg, fg);
    g.setClip(oldClip);

    // Calculate and store the selection rectangle
    int width = g.getFontMetrics().stringWidth(name + "  ");
    int height = g.getFontMetrics().getHeight();

    selectionRect.setLocation(posX - (width / 2), posY - 1);
    selectionRect.setSize(width, height + labelOffset + 1);

  }
}

