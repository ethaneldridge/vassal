/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASL.build.module.map;

import VASL.counters.ASLProperties;
import VASSAL.build.module.map.StackMetrics;
import VASSAL.counters.GamePiece;

import java.awt.*;

public class ASLStackMetrics extends StackMetrics {
  protected void drawUnexpanded(GamePiece p, Graphics g,
                                int x, int y, Component obs, double zoom) {
    if (p.getProperty(ASLProperties.LOCATION) != null) {
      p.draw(g, x - (int) (zoom * 15), y, obs, zoom);
    }
    else {
      super.drawUnexpanded(p, g, x, y, obs, zoom);
    }
  }
}
