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

import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * This component listens for mouse clicks on a map.
 * If the user clicks on a {@link GamePiece}, that piece is added to the {@link KeyBuffer}
 *
 * @see Map#addLocalMouseListener
 */
public class KeyBufferer extends MouseAdapter implements Buildable {
  protected Map map;

  public void addTo(Buildable b) {
    map = (Map) b;
    map.addLocalMouseListener(this);
  }

  public void add(Buildable b) {
  }

  public Element getBuildElement(Document doc) {
    return doc.createElement(getClass().getName());
  }

  public void build(Element e) {
  }

  public void mousePressed(MouseEvent e) {
    if (e.isConsumed()) {
      return;
    }
    GamePiece p = map.findPiece(e.getPoint(), PieceFinder.PIECE_IN_STACK);
    // Don't clear the buffer until we find the clicked-on piece
    // Because selecting a piece affects its visibility
    EventFilter filter = null;
    if (p != null) {
      filter = (EventFilter) p.getProperty(Properties.EVENT_FILTER);
    }
    boolean ignoreEvent = filter != null && filter.rejectEvent(e);
    if (!e.isShiftDown()
        || ignoreEvent) {
      KeyBuffer.getBuffer().clear();
    }
    if (p != null && !ignoreEvent) {
        KeyBuffer.getBuffer().add(p);
        if (p.getParent() != null) {
          map.getPieceCollection().moveToFront(p.getParent());
        }
        else {
          map.getPieceCollection().moveToFront(p);
        }
    }
  }

  public void mouseReleased(MouseEvent evt) {
  }
}
