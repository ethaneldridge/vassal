/*
 * $Id$
 * 
 * Copyright (c) 2005 by Brent Easton
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package VSQL;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

import VASL.build.module.map.ASLPieceMover;
import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.counters.DragBuffer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Stack;

public class VSQLPieceMover extends ASLPieceMover {

  public VSQLPieceMover() {
    super();
  }

  public Command movePieces(Map map, Point p) {
    Command c = super.movePieces(map, p);

    PieceIterator it = DragBuffer.getBuffer().getIterator();

    while (it.hasMoreElements()) {
      GamePiece piece = it.nextPiece();
      if (piece instanceof Stack) {
        processStack((Stack) piece);
      }
      else {
        String subType = (String) piece.getProperty(VSQLProperties.UNIT_SUB_TYPE);
        if (subType != null && subType.equals(VSQLProperties.LEADER)) {
          piece.setProperty(VSQLProperties.STACKED_COUNT, "0");
        }
      }

    }

    
    
    return c;
  }

  /**
   * Notify any leaders of the number of squads stacked with them
   */
  protected void processStack(Stack stack) {

    ArrayList leaders = new ArrayList(3);
    int squadCount = 0;

    Enumeration e = stack.getPieces();
    while (e.hasMoreElements()) {
      GamePiece piece = (GamePiece) e.nextElement();

      String type = (String) piece.getProperty(VSQLProperties.UNIT_TYPE);
      if (type != null && type.equals(VSQLProperties.INFANTRY)) {
        String subType = (String) piece.getProperty(VSQLProperties.UNIT_SUB_TYPE);
        if (subType != null) {
          if (subType.equals(VSQLProperties.LEADER)) {
            leaders.add(piece);
          }
          else if (subType.equals(VSQLProperties.SQUAD) || subType.equals(VSQLProperties.CREW)) {
            squadCount++;
          }
        }
      }
    }

    Iterator i = leaders.iterator();
    while (i.hasNext()) {
      GamePiece leader = (GamePiece) i.next();
      leader.setProperty(VSQLProperties.STACKED_COUNT, squadCount + "");
    }

  }
}