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
import VASSAL.command.Command;
import VASSAL.counters.GamePiece;

public class ShadeableMap extends Map {

  protected ArrayList shaders;
  
  public ShadeableMap() {
    super();
    shaders = new ArrayList();
  }

  /**
   * Over-ride Map.paintRegion().
   * Draw Map shaders aft the boards and before the pieces.
   */
  public void paintRegion(Graphics g, Rectangle visibleRect) {
    clearMapBorder(g); // To avoid ghost pieces around the edge

    drawBoardsInRegion(g, visibleRect);
    drawShaders(g, visibleRect);
    drawPiecesInRegion(g, visibleRect);
    drawDrawable(g);
  }

  /**
   * Draw all MapShaders registered on this map.
   * @param g Graphics
   * @param visibleRect Visible part of map on screen
   */
  protected void drawShaders(Graphics g, Rectangle visibleRect) {
    Iterator i = shaders.iterator();
    while (i.hasNext()) {
      ((MapShader) i.next()).draw(g, visibleRect, this);
    }    
  }
  
  /**
   * A Gamepiece is being moved to or from this map, Rebuild any shade in either map
   * that this piece has active. 
   */
  public void addPiece(GamePiece p) {
    Map fromMap = p.getMap();
    if (fromMap != null && fromMap instanceof ShadeableMap) {
      ((ShadeableMap) fromMap).dirtyShade(p);
    }
    
    super.addPiece(p);
    
    Map toMap = p.getMap();
    if (toMap != null && toMap instanceof ShadeableMap) {
      ((ShadeableMap) toMap).dirtyShade(p);
    }
  }
  
  public void removePiece(GamePiece p) {
    Map fromMap = p.getMap();
    if (fromMap != null && fromMap instanceof ShadeableMap) {
      ((ShadeableMap) fromMap).dirtyShade(p);
    }
    
    super.removePiece(p);
  }

  public Command placeOrMerge(final GamePiece p, final Point pt) {
    Command c = super.placeOrMerge(p, pt);
    Map map = p.getMap();
    if (map != null && map instanceof ShadeableMap) {
      ((ShadeableMap) map).dirtyShade(p);
    }
    return c;
  }
  
  /**
   * Add a Shader to the list of Shaders on this map.
   * @param shader The shader to add.
   */
  protected void addShader(MapShader shader) {
    shaders.add(shader);
  }
  
  /**
   * Removed a Shader from the list of Shaders on this map.
   * @param shader The shader to remove
   */
  protected void removeShader(MapShader shader) {
    shaders.remove(shader);
  }
  
  /**
   * Mark the specified Shade as dirty. Must be rebuild when next redisplayed.
   * @param shadeName Name of the Shade to mark dirty
   */
  public void dirtyShade(String shadeName) {
    Shade s = getShade(shadeName);
    if (s != null) {
      s.setDirty(true);
    }
  }

  /**
   * Return the Shade of the given name
   * @param shadeName 
   * @return
   */
  public Shade getShade(String shadeName) {
    Shade s = null;
    Iterator i = shaders.iterator();
    while (i.hasNext()) {
      s = ((MapShader) i.next()).getShade(shadeName);
    } 
    return s;
  }
  /**
   * Mark any Shade that this piece has Active as dirty.
   * @param piece The piece to check
   */
  public void dirtyShade(GamePiece piece) {
    Iterator i = shaders.iterator();
    while (i.hasNext()) {
      ((MapShader) i.next()).dirtyShade(piece);
    } 
  }
  
  public static String getConfigureTypeName() {
    return "Shadeable Map";
  }

  /**
   * Return an Area centered on a given point out to a set number
   * of grid elements (hexes or squares).
   * @param p Center point
   * @param range Range in grid elements
   * @return Area representing the shape of the range.
   */
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
