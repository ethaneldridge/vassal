/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;

import java.awt.*;
import java.awt.geom.Area;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Map Grid that contains any number of {@link VASSAL.build.module.map.boardPicker.board.mapgrid.Zone}s against a background {@link MapGrid}
 */
public class ZonedGrid extends AbstractConfigurable implements MapGrid, GridContainer {
  private ArrayList zones = new ArrayList();
  private MapGrid background;
  private GridContainer container;

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setAttribute(String key, Object value) {
  }

  public void addTo(Buildable parent) {
    container = (GridContainer) parent;
    container.setGrid(this);
  }

  public Dimension getSize() {
    return container.getSize();
  }

  public void removeGrid(MapGrid grid) {
    if (background == grid) {
      background = null;
    }
  }

  public void setGrid(MapGrid grid) {
    background = grid;
  }

  public Class[] getAllowableConfigureComponents() {
    return background == null ? new Class[]{Zone.class, HexGrid.class, SquareGrid.class, RegionGrid.class}
          : new Class[]{Zone.class};
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
    ((GridContainer)parent).removeGrid(this);
  }

  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    Area allZones = null;
    for (Iterator it = zones.iterator(); it.hasNext();) {
      Zone zone = (Zone) it.next();
      if (allZones == null) {
        allZones = new Area(zone.getShape());
      }
      else {
        allZones.add(new Area(zone.getShape()));
      }
    }
    if (background != null) {
      Graphics2D g2d = (Graphics2D) g;
      Shape oldClip = g2d.getClip();
      if (allZones != null) {
        Area clipArea = new Area(g2d.getClip());
        clipArea.subtract(allZones);
        g2d.setClip(clipArea);
      }
      background.draw(g,bounds,visibleRect,scale,reversed);
      g2d.setClip(oldClip);
    }
    for (Iterator it = zones.iterator(); it.hasNext();) {
      Zone zone = (Zone) it.next();
      zone.draw(g,bounds,visibleRect,scale,reversed);
    }
  }

  public GridNumbering getGridNumbering() {
    return background != null ? background.getGridNumbering() : null;
  }

  public Point getLocation(String location) throws MapGrid.BadCoords {
    if (background != null) {
      return background.getLocation(location);
    }
    else {
      throw new MapGrid.BadCoords("No naming scheme defined");
    }
  }

  public boolean isVisible() {
    return true;
  }

  public String locationName(Point p) {
    String name = null;
    for (Iterator it = zones.iterator(); it.hasNext();) {
      Zone zone = (Zone) it.next();
        if (zone.contains(p)) {
          name = zone.locationName(p);
          break;
        }
    }
    if (name == null
      && background != null) {
      name = background.locationName(p);
    }
    return name;
  }

  public int range(Point p1, Point p2) {
    return background != null ? background.range(p1,p2) : 0;
  }

  public Point snapTo(Point p) {
    Point snap = null;
    for (Iterator it = zones.iterator(); it.hasNext();) {
      Zone zone = (Zone) it.next();
        if (zone.contains(p)) {
          snap = zone.snapTo(p);
          break;
        }
    }
    if (snap == null
      && background != null) {
      snap = background.snapTo(p);
    }
    return snap;
  }

  public void addZone(Zone z) {
    zones.add(z);
  }

  public void removeZone(Zone z) {
    zones.remove(z);
  }

  public Iterator getZones() {
    return zones.iterator();
  }

  public MapGrid getBackgroundGrid() {
    return background;
  }

  public void setBackgroundGrid(MapGrid background) {
    this.background = background;
  }
}
