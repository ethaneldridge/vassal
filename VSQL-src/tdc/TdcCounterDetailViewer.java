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

import java.awt.Graphics;
import java.awt.Point;

import javax.swing.JComponent;

import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceIterator;

/**
 * Report the Step mass of a stack in the Text description.
 */
public class TdcCounterDetailViewer extends CounterDetailViewer {

  protected void drawText(Graphics g, Point pt, JComponent comp, PieceIterator pi) {
    /*
     * Label with the location
     * If the counter viewer is being displayed, then place the location name just above the
     * left hand end of the counters.
     * If no counter viewer (i.e. single piece or expanded stack), then place the location
     * name above the centre of the first piece in the stack.
     */
    String locationName = null;
    int steps = 0;

    while (pi.hasMoreElements()) {
      GamePiece piece = pi.nextPiece();
      if (locationName == null) {
        locationName = map.locationName(piece.getPosition());
      }
      String s = (String) piece.getProperty("Step");
      if (s != null) {
        int step = 0;
        try {
           step += Integer.parseInt(s);
        }
        catch (Exception e) {     
        }
        if (step == 2) {
          String a = (String) piece.getProperty("Step" + Embellishment.ACTIVE);
          if (a != null && a.equals("true")) {
            step = 1;
          }
        }
        steps += step;
      }
    }

    if (locationName == null) {
      Point mapPt = map.mapCoordinates(currentMousePosition.getPoint());
      Point snapPt = map.snapTo(mapPt);
      locationName = map.locationName(snapPt);
    }
    else {
      locationName += " - Mass: " + steps;
    }
    drawLabel(g, pt, locationName);
  }
  
}
