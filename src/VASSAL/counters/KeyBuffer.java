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

import VASSAL.command.Command;
import VASSAL.command.NullCommand;

import java.util.Enumeration;
import java.util.Vector;


public class KeyBuffer {
  private static KeyBuffer theBuffer;
  private Vector pieces;
  private BoundsTracker bounds;

  private KeyBuffer() {
    pieces = new Vector();
    bounds = new BoundsTracker();
  }

  public static void init(KeyBuffer kb) {
    if (theBuffer == null)
      theBuffer = kb;
  }

  public static KeyBuffer getBuffer() {
    if (theBuffer == null) {
      theBuffer = new KeyBuffer();
    }
    return theBuffer;
  }

  public void add(GamePiece p) {
    if (p != null
        && !pieces.contains(p)) {
      pieces.addElement(p);
      p.setProperty(Properties.SELECTED, Boolean.TRUE);
    }
  }

  public void clear() {
    while (pieces.size() > 0) {
      remove((GamePiece) pieces.lastElement());
    }
  }

  public void remove(GamePiece p) {
    if (p != null) {
      pieces.removeElement(p);
      p.setProperty(Properties.SELECTED, null);
    }
  }

  public boolean contains(GamePiece p) {
    if (p instanceof Stack) {
      for (Enumeration e = ((Stack) p).getPieces(); e.hasMoreElements();) {
        if (!pieces.contains(e.nextElement())) {
          return false;
        }
      }
      return true;
    }
    else {
      return pieces.contains(p);
    }
  }

  public boolean isEmpty() {
    return pieces.size() == 0;
  }

  public Command keyCommand(javax.swing.KeyStroke stroke) {
    Command comm = new NullCommand();

    bounds.clear();

    for (Enumeration e = pieces.elements();
         e.hasMoreElements();) {
      bounds.addPiece((GamePiece) e.nextElement());
    }
    Vector targets = new Vector();
    for (Enumeration e = pieces.elements();
         e.hasMoreElements();) {
      targets.addElement(e.nextElement());
    }
    for (Enumeration e = targets.elements();
         e.hasMoreElements();) {
      GamePiece g = (GamePiece) e.nextElement();
      g.setProperty(Properties.SNAPSHOT,PieceCloner.getInstance().clonePiece(g)); // save state prior to command
      Command c2 = g.keyEvent(stroke);
      comm = comm.append(c2);
    }

    for (Enumeration e = targets.elements();
         e.hasMoreElements();) {
      bounds.addPiece((GamePiece) e.nextElement());
    }
    bounds.repaint();
    return comm;
  }

  public Enumeration getPieces() {
    return pieces.elements();
  }
}
