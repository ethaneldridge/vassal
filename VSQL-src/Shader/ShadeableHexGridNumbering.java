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

import VASSAL.build.module.map.boardPicker.board.mapgrid.HexGridNumbering;

public class ShadeableHexGridNumbering extends HexGridNumbering {

  public ShadeableHexGridNumbering() {
    super();
  }
  
  public int getMaxRows() {
    return super.getMaxRows();
  }

  public int getMaxColumns() {
    return super.getMaxColumns();
  }
  
  public static String getConfigureTypeName() {
    return "Shadeable Hex Grid Numbering";
  }
  
}
