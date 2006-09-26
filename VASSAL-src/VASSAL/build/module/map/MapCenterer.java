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
package VASSAL.build.module.map;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.PieceFinder;

import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;

/**
 * Centers the map when user right-clicks on an empty hex
 */
public class MapCenterer extends AbstractBuildable implements MouseListener {
  private Map map;
  public void addTo(Buildable b) {
    map = (Map) b;
    map.addLocalMouseListener(this);
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String getAttributeValueString(String attName) {
    return null;
  }

  public void setAttribute(String attName, Object value) {
  }

  public void mouseReleased(MouseEvent e) {
    if (e.isMetaDown()
      && map.findPiece(e.getPoint(), PieceFinder.PIECE_IN_STACK) == null
      /*&& !e.isConsumed()*/) {
      Map.View m = (Map.View) e.getSource();
      m.getMap().centerAt(e.getPoint());
    }
  }

  public void mousePressed(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mouseClicked(MouseEvent e) {
  }
}
