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
 
package Shader;

import java.awt.Point;
import java.awt.Polygon;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;

import VASSAL.build.module.map.boardPicker.board.HexGrid;

public class ShadeableHexGrid extends HexGrid {

  public ShadeableHexGrid() {
    super();
  }
  
  public Area getShape(int centerX, int centerY, double zoom, boolean reversed) {
    Polygon poly = new Polygon(); 
    
    float x = (float) (sideways ? centerY : centerX) * (float) zoom;
    float y = (float) (sideways ? centerX : centerY) * (float) zoom;
    
    float x1,y1, x2,y2, x3,y3, x4, y4, x5, y5, x6, y6;

    float deltaX = (float) (this.dx * zoom);
    float deltaY = (float) (this.dy * zoom);

    float r = 2.F * deltaX / 3.F;
    
    Point p1 = new Point();
    Point p2 = new Point();
    Point p3 = new Point();
    Point p4 = new Point();
    Point p5 = new Point();
    Point p6 = new Point();
    
    x1 = x - r;
    y1 = y;
    p1.setLocation(Math.round(x1), Math.round(y1));
    
    x2 = x - .5F * r;
    y2 = reversed ? y + .5F * deltaY : y - .5F * deltaY;
    p2.setLocation(Math.round(x2), Math.round(y2));
    
    x3 = x + .5F * r;
    y3 = y2;
    p3.setLocation(Math.round(x3)+1, Math.round(y3));
    
    x4 = x + r;
    y4 = y;
    p4.setLocation(Math.round(x4)+1, Math.round(y4));
    
    x5 = x3;
    y5 = reversed ? y - .5F * deltaY : y + .5F * deltaY;
    p5.setLocation(Math.round(x5)+1, Math.round(y5)+1);
    
    x6 = x2;
    y6 = y5;
    p6.setLocation(Math.round(x6), Math.round(y6)+1);
    
    if (sideways) {
      rotate(p1);
      rotate(p2);
      rotate(p3);
      rotate(p4);
      rotate(p5);
      rotate(p6);
    }
    
    poly.addPoint(p1.x, p1.y);
    poly.addPoint(p2.x, p2.y);
    poly.addPoint(p3.x, p3.y);
    poly.addPoint(p4.x, p4.y);
    poly.addPoint(p5.x, p5.y);
    poly.addPoint(p6.x, p6.y);
    poly.addPoint(p1.x, p1.y);
    
    return new Area(poly);
  }
  
  public static String getConfigureTypeName() {
    return "Shadeable Hex Grid";
  }

  /**
   * Return an Area the shape of the hex grid to a given range, centred
   * on 0,0.
   */
  public Area getRangeShape(int range, double zoom) {
    //Choose a starting point 
    Point origin = snapToHex(new Point(0, 0));
    Area shape = getShape(origin.x, origin.y, zoom, false);
    
    for (int i = -range; i <= range; i++) {
      int x = origin.x + (int) (i * dx);
      
      int length = range*2 + 1 - Math.abs(i);
      
      int startY = 0;
      if (length % 2 == 1) {
        startY = origin.y - (int) (dy * (length-1)/2);
      }
      else {
        startY = origin.y  - (int) (dy * (0.5 + (length-2)/2));
      }
      
      int y = startY;
      for (int j = 0; j < length; j++) {
        Point p = new Point(x, y);
        rotateIfSideways(p);
        shape.add(getShape(p.x, p.y, zoom, false));
        y += dy;
      }
    }
    
    rotateIfSideways(origin);
    shape.transform(AffineTransform.getTranslateInstance(0-origin.x, 0-origin.y));

    return shape;
  }
}
