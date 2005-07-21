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
 
package shader;

import java.awt.Point;
import java.awt.Polygon;
import java.awt.Shape;

import VASSAL.build.module.map.boardPicker.board.HexGrid;

public class ShadeableHexGrid extends HexGrid {

  public Shape getHexShape(int centerX, int centerY, double zoom, boolean reversed) {
    Polygon poly = new Polygon(); 
    
    float x = (float) centerX;
    float y = (float) centerY;
    
    float x1,y1, x2,y2, x3,y3, x4, y4;

    float deltaX = (float) (this.dx * zoom);
    float deltaY = (float) (this.dy * zoom);

    float r = 2.F * deltaX / 3.F;
    
    Point p1 = new Point();
    Point p2 = new Point();
    Point p3 = new Point();
    Point p4 = new Point();
    
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
    poly.addPoint(p1.x, p1.y);
    poly.addPoint(p2.x, p2.y);
    poly.addPoint(p3.x, p3.y);
    poly.addPoint(p4.x, p4.y);
    poly.addPoint(p1.x, p1.y);
    
    return poly;
  }
}
