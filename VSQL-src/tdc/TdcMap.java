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

import java.awt.Point;

import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.BasicPiece;

/**
 *
 */
public class TdcMap extends Map {
  
  public void addTo(Buildable b) {
    super.addTo(b);
    TdcHighlighter highlighter = new TdcHighlighter();
    setHighlighter(highlighter);
    BasicPiece.setHighlighter(highlighter);
  }
  
  public Point componentCoordinates(Point p1) {
    return new Point((int) Math.round(p1.x * getZoom()), (int) Math.round(p1.y * getZoom()));
  }
}
