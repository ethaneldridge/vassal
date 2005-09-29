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

import java.awt.Point;
import java.awt.event.MouseEvent;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.LOS_Thread;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.HexGridNumbering;

/**
 * @author Brent Easton
 *
 * TDC LOS Thread
 * Force End point of LOS Thread to Snap to grid.
 */
public class TdcThread extends LOS_Thread {
  
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

        Point new_anchor = smartSnap(m, b, anchor);
        Point new_arrow = smartSnap(m, b, arrow);
//        int anchor_col = numbering.getColumn(b.snapTo(anchor));
//        int anchor_row = numbering.getRow(b.snapTo(anchor));
//        int arrow_col = numbering.getColumn(arrow);
//        int arrow_row = numbering.getRow(arrow);
        
        int range = b.getGrid().range(new_anchor, new_arrow);
        //drawRange(g, b.getGrid().range(b.snapTo(anchor), arrow));
        drawRange(g, range);
      }
    }
  }

  protected Point smartSnap(Map m, Board b, Point anchor) {
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
