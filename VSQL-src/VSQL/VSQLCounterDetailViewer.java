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
  
  protected class Selector implements PieceFilter {

    protected Point point;
    
    public Selector(Point p) {
      point = p;
    }
    
    public boolean accept(GamePiece piece) {
       Rectangle r = piece.boundingBox();
       Point pos = piece.getPosition();
       r.translate(pos.x, pos.y);
       return r.contains(point);
    }
    
  }
  
}
