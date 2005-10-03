/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney
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
package VASL.counters;

import VASSAL.command.Command;
import VASSAL.command.MoveTracker;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.counters.Translate;

import javax.swing.*;
import java.awt.*;
import java.util.Enumeration;

/**
 * Modifies the {@link Translate} base class by not moving counters with the {@link ASLProperties#LOCATION} trait
 */
public class ASLTranslate extends Translate {
  public ASLTranslate() {
  }

  public ASLTranslate(String type, GamePiece inner) {
    super(type, inner);
  }

  public Command myKeyEvent(KeyStroke stroke) {
    Point p = getPosition();
    Command c = super.myKeyEvent(stroke);
    if (c != null
        && getParent() != null) {
      // Return LOCATION pieces to original location
      Stack parent = getParent();
      for (Enumeration e = parent.getPiecesInReverseOrder(); e.hasMoreElements();) {
        GamePiece piece = (GamePiece) e.nextElement();
        if (piece.getProperty(ASLProperties.LOCATION) != null) {
          MoveTracker t = new MoveTracker(piece);
          getMap().placeOrMerge(piece, p);
          c = c.append(t.getMoveCommand());
        }
      }
    }
    return c;
  }
}
