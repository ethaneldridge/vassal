/*
 * $Id$
 *
 * Copyright (c) 2003 by David Sullivan and Rodney Kinney
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
package vip;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Vector;

import javax.swing.JComponent;

import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.build.module.map.Drawable;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceIterator;

/**
 * This is a {@link Drawable} class that draws the counters horizontally
 * when the mouse is held over a stack with the control key down.
 *
 * @author       David Sullivan
 * @version      1.0
 */
public class VIPCounterDetailViewer extends CounterDetailViewer {


  public VIPCounterDetailViewer() {
    super();
  }

  protected void drawGraphics(Graphics g, Point pt, JComponent comp, PieceIterator pi) {

    Rectangle bounds = new Rectangle(pt.x, pt.y, 0, 0);
    Vector v = new Vector();
    while (pi.hasMoreElements()) {
      GamePiece piece = pi.nextPiece();
      v.addElement(piece);
      Rectangle pieceBounds = piece.getShape().getBounds();
      bounds.width += pieceBounds.width;
      bounds.height = Math.max(bounds.height, pieceBounds.height);
    }

    bounds.y -= bounds.height;
    
    if (bounds.width > 0) {

      Color outline = map.getHighlighter() instanceof ColoredBorder ? ((ColoredBorder) map.getHighlighter()).getColor() : Color.black;
      Color background = new Color(255 - outline.getRed(), 255 - outline.getGreen(), 255 - outline.getBlue());

      Rectangle visibleRect = comp.getVisibleRect();
      bounds.x = Math.min(bounds.x, visibleRect.x + visibleRect.width - bounds.width);
      if (bounds.x < visibleRect.x) bounds.x = visibleRect.x;
      bounds.y = Math.min(bounds.y, visibleRect.y + visibleRect.height - bounds.height);
      if (bounds.y < visibleRect.y) bounds.y = visibleRect.y;
      g.setColor(background);
      g.fillRect(bounds.x - 1, bounds.y - 1, bounds.width + 2, bounds.height + 2);
      g.setColor(outline);
      g.drawRect(bounds.x - 2, bounds.y - 2, bounds.width + 3, bounds.height + 3);
      g.drawRect(bounds.x - 3, bounds.y - 3, bounds.width + 5, bounds.height + 5);
      Shape oldClip = g.getClip();
      g.setClip(bounds.x - 3, bounds.y - 3, bounds.width + 5, bounds.height + 5);
      pi = new PieceIterator(v.elements());
      while (pi.hasMoreElements()) {
        // Draw the next piece
        // pt is the location of the left edge of the piece
        GamePiece piece = pi.nextPiece();
        Rectangle pieceBounds = piece.getShape().getBounds();
        piece.draw(g, bounds.x - pieceBounds.x, bounds.y - pieceBounds.y, comp, 1.0);

        bounds.translate(pieceBounds.width, 0);
      }
      g.setClip(oldClip);
    }
  }

}
