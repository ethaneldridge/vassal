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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;

import VASSAL.counters.ColoredBorder;
import VASSAL.counters.GamePiece;

/**
 * Change border color depending on Command Status
 * @author Brent Easton
 * 
 */
public class TdcHighlighter extends ColoredBorder {

  public TdcHighlighter() {
    super();
  }

  /**
   * Change outline Color to 3 pixel wide Red if unit is not in command.
   */
  public void draw(GamePiece p, Graphics g, int x, int y, Component obs, double zoom) {

    if (p.getMap() == null) {
      super.draw(p, g, x, y, obs, zoom);
    }
    else {
      CommandRangeChecker check = new CommandRangeChecker(p);
      if (check.inCommand()) {
        super.draw(p, g, x, y, obs, zoom);
      }
      else {
        Color oldColor = getColor();
        int oldThickness = getThickness();
        setColor(Color.red);
        setThickness(3);
        super.draw(p, g, x, y, obs, zoom);
        Rectangle r = p.getShape().getBounds();
        int x1 = x + (int) (zoom * r.x) - 1;
        int y2 = y + (int) (zoom * (r.y + r.height));
        int x2 = x1 + (int) (zoom * r.width);
        int y1 = y2 - (int) (zoom * 75);
        g.drawLine(x1, y2, x2, y1);
        setColor(oldColor);
        setThickness(oldThickness);
      }
    }
  }
}
