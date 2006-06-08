/*
 * $Id$
 * 
 * Copyright (c) 2000-2005 by Rodney Kinney, Brent Easton
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

package ThirdReich;

import java.awt.Point;
import VASSAL.build.module.map.boardPicker.board.mapgrid.HexGridNumbering;

public class ObliqueHexGridNumbering extends HexGridNumbering {

  public int getRow(Point p) {
 
    int row = super.getRow(p);
    
    if (getGrid().isSideways()) {
      int col = super.getColumn(p);
      row -= (int) (col-1) / 2;
    }
    return row;
  }

  public int getColumn(Point p) {

    int col = super.getColumn(p);
    return col;

  }

  public static String getConfigureTypeName() {
    return "3R4 Oblique Hex Grid Numbering";
  }

}