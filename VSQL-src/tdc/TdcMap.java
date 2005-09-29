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
 
package tdc;

import java.awt.AlphaComposite;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;

import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

/**
 *
 */
public class TdcMap extends Map {

  private float pieceOpacity = 1.0f;
  
  public void addTo(Buildable b) {
    super.addTo(b);
    TdcHighlighter highlighter = new TdcHighlighter();
    setHighlighter(highlighter);
    BasicPiece.setHighlighter(highlighter);
  }
  
  public Point componentCoordinates(Point p1) {
    return new Point((int) Math.round(p1.x * getZoom()), (int) Math.round(p1.y * getZoom()));
  }
  
  public void setPieceOpacity(float o) {
    pieceOpacity = o;
  }
  
  public float getPieceOpacity() {
    return pieceOpacity;
  }
  
  public void drawPiecesInRegion(Graphics g, Rectangle visibleRect) {
    if (isPiecesVisible()) {
      Graphics2D g2d = (Graphics2D) g;
      Composite oldComposite = g2d.getComposite();
      g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, pieceOpacity));

      GamePiece[] stack = pieces.getPieces();
      for (int i = 0; i < stack.length; ++i) {
        Point pt = componentCoordinates(stack[i].getPosition());
        if (stack[i].getClass() == Stack.class) {
          getStackMetrics().draw((Stack) stack[i], pt, g, this, getZoom(), visibleRect);
        }
        else {
          stack[i].draw(g, pt.x, pt.y, theMap, getZoom());
          if (Boolean.TRUE.equals(stack[i].getProperty(Properties.SELECTED))) {
            highlighter.draw
                (stack[i], g, pt.x, pt.y, theMap, getZoom());
          }
        }
      }
      g2d.setComposite(oldComposite);
    }
  }

  public void drawPieces(Graphics g, int xOffset, int yOffset) {
    if (isPiecesVisible()) {
      Graphics2D g2d = (Graphics2D) g;
      Composite oldComposite = g2d.getComposite();
      g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, pieceOpacity));
      
      GamePiece[] stack = pieces.getPieces();
      for (int i = 0; i < stack.length; ++i) {
        Point pt = componentCoordinates(stack[i].getPosition());
        stack[i].draw(g, pt.x + xOffset, pt.y + yOffset, theMap, getZoom());
        if (Boolean.TRUE.equals(stack[i].getProperty(Properties.SELECTED))) {
          highlighter.draw
              (stack[i], g, pt.x - xOffset, pt.y - yOffset, theMap, getZoom());
        }
      }
      g2d.setComposite(oldComposite);
    }
  }

}
