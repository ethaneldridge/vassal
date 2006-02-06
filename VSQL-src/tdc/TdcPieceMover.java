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
import java.awt.event.InputEvent;
import java.util.Enumeration;

import javax.swing.KeyStroke;

import VASSAL.build.module.map.PieceMover;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

/**
 * 
 * @author Brent Easton
 *
 * Mark units done when they move, If they are Active and not yet marked.
 */
public class TdcPieceMover extends PieceMover {

  protected Command movedPiece(GamePiece p, Point loc) {
    Command c = super.movedPiece(p, loc);
    if (c == null) {
      c = new NullCommand();
    }
    if (p.getMap() != null) {
      if (p instanceof Stack) {
        Enumeration e = ((Stack) p).getPieces();
        while (e.hasMoreElements()) {
          c.append(activatePiece((GamePiece) e.nextElement()));
        }
      }
      else {
        c.append(activatePiece(p));
      }
    }
    return c;
  }
  
  protected Command activatePiece(GamePiece p) {
    String markLevel = (String) p.getProperty("Mark_Level") + "";
    String activeLevel = (String) p.getProperty("Active_Level") + "";
    if (markLevel != null && markLevel.equals("1") && activeLevel.equals("2")) {
      p.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(p));
      return p.keyEvent(KeyStroke.getKeyStroke('K', InputEvent.CTRL_MASK));
    }
    else {
      return new NullCommand();
    }
  }
  
}
