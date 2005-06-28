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
 
package VSQL;

import java.awt.Point;
import java.awt.Rectangle;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;

public class VSQLMap extends Map {

  public Point snapTo(Point p) {
    Point snap = new Point(p);

    Board b = findBoard(p);
    if (b == null)
      return snap;

    Rectangle r = b.bounds();
    snap.translate(-r.x, -r.y);
    snap = b.snapTo(snap);
    
    /*
     * RFE 882378 
     * If we have snapped to a point 1 pixel off the edge of the map, move back onto the map.
     */
    if (snap.x == r.width+1) {
      snap.x = r.width-1;
    }
    else if (snap.x == -1) {
      snap.x = 0;
    }
    if (snap.y == r.height+1) {
      snap.y = r.height;
    }
    else if (snap.y == -1) {
      snap.y = 0;
    }
    // End RFE 882378
    
    snap.translate(r.x, r.y);
    return snap;
  }
  
}
