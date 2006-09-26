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
package VASSAL.counters;

import VASSAL.build.module.Map;

import java.awt.*;
import java.util.Enumeration;
import java.util.Hashtable;

/**
 * Records the bounding boxes of GamePieces.  Use addPiece() to
 * record the bounding box of a GamePiece at a certain time.  Use
 * repaint() to repaint the appropriate areas of the maps to which the
 * added pieces belonged.
 */
public class BoundsTracker {
  private Hashtable rects;

  public BoundsTracker() {
    rects = new Hashtable();
  }

  public void clear() {
    rects.clear();
  }

  public void addPiece(GamePiece p) {
    if (p.getMap() != null) {
      Rectangle r = p.getMap().boundingBoxOf(p);
      if (rects.get(p.getMap()) == null) {
        rects.put(p.getMap(), r);
      }
      else {
        rects.put(p.getMap(), r.union((Rectangle) rects.get(p.getMap())));
      }
    }
  }

  public void repaint() {
    for (Enumeration e = rects.keys();
         e.hasMoreElements();) {
      Map m = (Map) e.nextElement();
      Rectangle r = (Rectangle) rects.get(m);
      m.repaint(r);
    }
  }
}
