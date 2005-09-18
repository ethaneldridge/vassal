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
import java.awt.event.MouseEvent;

import VASSAL.build.module.map.LOS_Thread;

/**
 * @author Brent Easton
 *
 * TDC LOS Thread
 * Force End point of LOS Thread to Snap to grid.
 */
public class TdcThread extends LOS_Thread {
  
  public void mouseDragged(MouseEvent e) {
    if (visible) {
      retainAfterRelease = true;

      Point p = e.getPoint();
      //if (Boolean.TRUE.equals
      //    (GameModule.getGameModule().getPrefs().getValue(SNAP_LOS))) {
        p = map.componentCoordinates(map.snapTo(map.mapCoordinates(p)));
      //}
      arrow = map.mapCoordinates(p);

      map.repaint();
    }
  }

}
