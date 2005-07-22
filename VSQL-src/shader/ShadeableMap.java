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

import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Iterator;

import VASSAL.build.module.Map;

public class ShadeableMap extends Map {

  protected ArrayList shaders;
  
  public ShadeableMap() {
    super();
    shaders = new ArrayList();
  }
  
  public void paint(Graphics g, int xOffset, int yOffset) {
    clearMapBorder(g); // To avoid ghost pieces around the edge

    drawBoards(g, xOffset, yOffset, getZoom(), theMap);
    drawShaders(g, xOffset, yOffset);
    drawPieces(g, xOffset, yOffset);
    drawDrawable(g);
  }
  
  public void paintRegion(Graphics g, Rectangle visibleRect) {
    clearMapBorder(g); // To avoid ghost pieces around the edge

    drawBoardsInRegion(g, visibleRect);
    drawShaders(g, visibleRect);
    drawPiecesInRegion(g, visibleRect);
    drawDrawable(g);
  }

  protected void drawShaders(Graphics g, int offset, int offset2) {
    Iterator i = shaders.iterator();
    while (i.hasNext()) {
      ((MapShader) i.next()).draw(g, this);
    }    
  }
  
  protected void drawShaders(Graphics g, Rectangle visibleRect) {
    Iterator i = shaders.iterator();
    while (i.hasNext()) {
      ((MapShader) i.next()).draw(g, this);
    }    
  }
  
  protected void addShader(MapShader shader) {
    shaders.add(shader);
  }
  
  protected void removeShader(MapShader shader) {
    shaders.remove(shader);
  }
}
