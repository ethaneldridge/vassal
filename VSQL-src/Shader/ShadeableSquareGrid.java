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

import java.awt.Rectangle;
import java.awt.geom.Area;

import VASSAL.build.module.map.boardPicker.board.SquareGrid;

public class ShadeableSquareGrid extends SquareGrid {

  public ShadeableSquareGrid() {
    super();
  }
  
  public Area getSquareShape(int centerX, int centerY, double zoom) {
    
	Rectangle rect = new Rectangle((int) (centerX - getDx()/2), (int) (centerY - getDy()/2), (int) getDx(), (int) getDy()); 
    return new Area(rect);
  }
  
  public static String getConfigureTypeName() {
    return "Shadeable Square Grid";
  }

  /**
   * @param range
   * @return
   */
  public Area getRangeShape(int range, double zoom) {
    // TODO Auto-generated method stub
    return null;
  }
}
