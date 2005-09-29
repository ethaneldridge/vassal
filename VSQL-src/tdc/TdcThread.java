/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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
 
package tdc;

import java.awt.Color;
import java.awt.Point;
import java.awt.event.MouseEvent;

import javax.swing.KeyStroke;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.LOS_Thread;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.HexGridNumbering;
import VASSAL.configure.VisibilityCondition;

/**
 * @author Brent Easton
 *
 * TDC LOS Thread
 * Force End point of LOS Thread to Snap to grid.
 */
public class TdcThread extends LOS_Thread {
  
  public static final String HIDE_OPACITY = "hideOpacity";
  protected int hideOpacity = 0;
  
  public String[] getAttributeNames() {
    return new String[]{HOTKEY, LABEL, DRAW_RANGE, RANGE_SCALE, RANGE_ROUNDING, HIDE_COUNTERS, HIDE_OPACITY, LOS_COLOR, RANGE_FOREGROUND, RANGE_BACKGROUND};
  }
  
  public String[] getAttributeDescriptions() {
    return new String[]{"Hotkey",
                        "Button text",
                        "Draw Range",
                        "Pixels per range unit",
                        "Round fractions",
                        "Hide Pieces while drawing",
                        "Opacity of hidden pieces (0-100%)",
                        "Thread color"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{KeyStroke.class,
                       String.class,
                       Boolean.class,
                       Integer.class,
                       RoundingOptions.class,
                       Boolean.class,
                       Integer.class,
                       Color.class};
  }

  public void setAttribute(String key, Object value) {
    if (HIDE_OPACITY.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      setTransparency (((Integer) value).intValue());
    }
    else {
      super.setAttribute(key, value);
    }
  }


  protected void setTransparency(int h) {
    if (h < 0) {
      hideOpacity = 0;
    }
    else if (h > 100) {
      hideOpacity = 100;
    }
    else {
      hideOpacity = h;
    }
  }

  public String getAttributeValueString(String key) {
    if (HIDE_OPACITY.equals(key)) {
      return String.valueOf(hideOpacity);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }
  
  public VisibilityCondition getAttributeVisibility(String name) {
    VisibilityCondition cond = null;
    if (HIDE_OPACITY.equals(name)) {
      cond = new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return hideCounters;
        }
      };
      return cond;
    }
    else {
      return super.getAttributeVisibility(name);
    }      
  }
  
  protected void launch() {
    if (!visible) {
      map.pushMouseListener(this);
      if (hideCounters) {
        ((TdcMap) map).setPieceOpacity(hideOpacity / 100.0f);
        map.repaint();
      }
      visible = true;
      anchor.move(0, 0);
      arrow.move(0, 0);
      retainAfterRelease = false;
    }
  }
  
  public void mouseReleased(MouseEvent e) {
    if (retainAfterRelease) {
      retainAfterRelease = false;
    }
    else if (e.getWhen() != lastRelease) {
      visible = false;
      ((TdcMap) map).setPieceOpacity(1.0f);
      map.popMouseListener();
      map.repaint();
    }
    lastRelease = e.getWhen();
  }
  
  public void draw(java.awt.Graphics g, Map m) {
    if (!visible) {
      return;
    }
    g.setColor(threadColor);
    Point mapAnchor = map.componentCoordinates(anchor);
    Point mapArrow = map.componentCoordinates(arrow);
    g.drawLine(mapAnchor.x, mapAnchor.y, mapArrow.x, mapArrow.y);
    Board b;
    if (drawRange) {
      if (rangeScale > 0) {
        int dist = (int)(rangeRounding + anchor.getLocation().distance(arrow.getLocation())/rangeScale);
        drawRange(g, dist);
      }
      else if ((b = map.findBoard(anchor)) != null
        && b.getGrid() != null) {

        Point new_anchor = trueSnap(m, b, anchor);
        Point new_arrow = trueSnap(m, b, arrow);
        
        int range = b.getGrid().range(new_anchor, new_arrow);
        drawRange(g, range);
      }
    }
  }

  protected Point trueSnap(Map m, Board b, Point anchor) {
    HexGrid grid = (HexGrid) b.getGrid();
    HexGridNumbering numbering = (HexGridNumbering) grid.getGridNumbering();
    double dx = grid.getHexWidth() / 4;
    double dy = grid.getHexSize() / 4;
    
    Point[] points = new Point[5];
    Point[] spoints = new Point[5];
    double[] dists = new double[5];
    
    points[0] = anchor;
    points[1] = new Point((int) (anchor.x + dx + 0.5), (int) (anchor.y+dy+0.5));
    points[2] = new Point((int) (anchor.x - dx + 0.5), (int) (anchor.y+dy+0.5));
    points[3] = new Point((int) (anchor.x + dx + 0.5), (int) (anchor.y-dy+0.5));
    points[4] = new Point((int) (anchor.x - dx + 0.5), (int) (anchor.y-dy+0.5));
    
    double min_dist = 99999.0;
    Point min_point = null;
    
    for (int i = 0; i < 5; i++) {
      Point p = m.snapTo(points[i]);
      spoints[i] = p;
      double dist = Math.sqrt(Math.pow(anchor.x-spoints[i].x, 2) + 
          			   Math.pow(anchor.y-spoints[i].y,2));
      dists[i] = dist;
      if (dist < min_dist) {
        min_dist = dist;
        min_point = p;
      }
    }
    
    return min_point;
  }


  public void mouseDragged(MouseEvent e) {
    if (visible) {
      retainAfterRelease = true;

      Point p = e.getPoint();
      //if (Boolean.TRUE.equals
      //    (GameModule.getGameModule().getPrefs().getValue(SNAP_LOS))) {
        p = map.componentCoordinates(map.snapTo(map.mapCoordinates(p)));
      //}
      arrow = map.mapCoordinates(p);

      map.repaint();
    }
  }

}
