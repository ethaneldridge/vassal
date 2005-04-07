/*
 * $Id$
 *
 * Copyright (c) 2003-2005 by David Sullivan, Rodney Kinney, Brent Easton
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

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.Enumeration;

import javax.swing.JComponent;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

public class VSQLCounterDetailViewer extends CounterDetailViewer {

  public VSQLCounterDetailViewer() {
  }

  public void draw(Graphics g, Point pt, JComponent comp) {

    if (!graphicsVisible && !textVisible) {
      return;
    }

    PieceIterator pi;

    if (graphicsVisible) {

      pi = new PieceIterator(new MapPieceServer(map), new Selector(pt));
      drawGraphics(g, pt, comp, pi);
    }

    if (textVisible) {
      pi = new PieceIterator(new MapPieceServer(map), new Selector(pt));
      drawText(g, pt, comp, pi);
    }
  }

  protected void showDetails() {
    currentPiece = findPieceAtMousePosition();
    /*
     * Visibility Rules:
     *   Stack         - Depends on setting of showGraphics/showText
     *   Single Unit   - Depends on setting of showGraphics/showText and showGraphicsSingle/showTextSingle
     *                   and stack must not be expanded.
     *   Empty space   - Depends on setting of showText
     */
    
    // Count number of pieces on all layers
    int pieceCount = 0;
    PieceIterator pi = new PieceIterator(new MapPieceServer(map), new Selector(currentMousePosition.getPoint()));
    while (pi.hasMoreElements()) {
      pieceCount++;
      pi.nextPiece();
    }
    
    // No pieces
    if (currentPiece == null) {
      textVisible = (showRef && map.getZoom() < zoomLevel);
      graphicsVisible = false;
    }
    else {
      // Below zoom specified zoom level
      if (map.getZoom() < zoomLevel) {
        boolean val = !Boolean.TRUE.equals(currentPiece.getProperty(Properties.TERRAIN));
        graphicsVisible = (showGraph && val);
        textVisible = (showText && val);
      }
      // One piece only
      else if (pieceCount == 1) {
        graphicsVisible = (showGraph && showGraphSingle);
        textVisible = (showText && showTextSingle);
      }
      else {
        // Multiple pieces, Stack on top.
        if (currentPiece instanceof Stack) {
           Stack s = (Stack) currentPiece;
           graphicsVisible = (showGraph && !s.isExpanded());
           textVisible = (showText && !s.isExpanded());
        }
        //Multiple single pieces
        else {
          graphicsVisible = showGraph;
          textVisible = showText;
        }
      }
    }

    map.repaint();
  }
  
  /**
   * Serve up all pieces on a Map indiviually as an Enumeration
   * 
   * */
  protected class MapPieceServer implements Enumeration {

    protected GamePiece[] pieces;
    protected int index;
    protected int stackIndex;
    protected Stack currentStack;
    protected GamePiece next;
    
    public MapPieceServer(Map m) {
      pieces = m.getPieces();
      index = -1;
      next = nextPiece();
    }
    
    protected GamePiece nextPiece() {
      
      GamePiece p = null;
      
      p = nextFromStack();
      if (p == null) {
         index++;
         if (index < pieces.length) {
            p = pieces[index];
            if (p instanceof Stack) {
              currentStack = (Stack) p;
              stackIndex = -1;
              p = nextFromStack();
              if (p == null) {
                p = nextPiece();
              }
           }
         }         
      }      
      return p;      
    }
    
    protected GamePiece nextFromStack() {
      stackIndex++;
      if (currentStack != null && stackIndex < currentStack.getPieceCount()) {
        return currentStack.getPieceAt(stackIndex);
      }
      else
        currentStack = null;
        stackIndex = -1;
        return null;
    }
    
    public boolean hasMoreElements() {
      return next != null;
    }


    public Object nextElement() {
      GamePiece p = next;
      next = nextPiece();
      return p;
    }    
  }
  
  /**
   * Filter to select pieces which contain the given point within their perimeter
   * @author Brent Easton
   */
  protected class Selector implements PieceFilter {

    protected Point point;
    
    public Selector(Point p) {
      point = p;
    }
    
    public boolean accept(GamePiece piece) {
       double zoom = map.getZoom();
       Rectangle r = piece.getShape().getBounds();
       Point pos = piece.getPosition();
       Rectangle check = new Rectangle();
       check.x = (int) ((pos.x + r.x)*zoom);
       check.y = (int) ((pos.y + r.y)*zoom);
       check.height = (int) (r.height * zoom);
       check.width = (int) (r.width * zoom);
       return check.contains(point);
    }
    
  }
  
}
