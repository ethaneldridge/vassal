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

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Area;
import java.util.ArrayList;
import java.util.Iterator;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;

public class ShadeableMap extends Map {

  protected ArrayList shaders;
  
  public ShadeableMap() {
    super();
    shaders = new ArrayList();
  }

  public void paintRegion(Graphics g, Rectangle visibleRect) {
    clearMapBorder(g); // To avoid ghost pieces around the edge

    drawBoardsInRegion(g, visibleRect);
    drawShaders(g, visibleRect);
    drawPiecesInRegion(g, visibleRect);
    drawDrawable(g);
  }

  protected void drawShaders(Graphics g, Rectangle visibleRect) {
    Iterator i = shaders.iterator();
    while (i.hasNext()) {
      ((MapShader) i.next()).draw(g, visibleRect, this);
    }    
  }
  
//  /*
//   * A stack has been moved, rebuild the shaders
//   */
//  public void addPiece(GamePiece p) {
//    super.addPiece(p);
//    Iterator i = shaders.iterator();
//    while (i.hasNext()) {
//      ((MapShader) i.next()).update();
//    } 
//  }

  
  protected void addShader(MapShader shader) {
    shaders.add(shader);
  }
  
  protected void removeShader(MapShader shader) {
    shaders.remove(shader);
  }
  
  public void dirtyShade(String shadeName) {
    Iterator i = shaders.iterator();
    while (i.hasNext()) {
      ((MapShader) i.next()).dirtyShade(shadeName);
    } 
  }
  
  public static String getConfigureTypeName() {
    return "Shadeable Map";
  }

  public Area getGridRangeShape(Point p, int range) {

    Board b = findBoard(p);
    if (b != null) {
      MapGrid grid = b.getGrid();
      if (grid != null) {
        return grid.getRangeShape(range, getZoom());
      }
    }
    return null;
    
  }
}
