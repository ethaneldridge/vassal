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
package PB;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.PieceMover;
import VASSAL.command.Command;
import VASSAL.counters.Decorator;
import VASSAL.counters.DragBuffer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Stack;

public class PBPieceMover extends PieceMover {

  public PBPieceMover() {
    super();
  }

  /**
   * In addition to moving pieces normally, we mark units that have moved
   * and adjust the concealment status of units
   */
  public Command movePieces(Map m, java.awt.Point p) {

    GamePiece movingConcealment = null;
    Stack formerParent = null;
    PieceIterator it = DragBuffer.getBuffer().getIterator();
    if (it.hasMoreElements()) {
      GamePiece moving = it.nextPiece();
      if (moving instanceof Stack) {
        Stack s = (Stack) moving;
        moving = s.topPiece();
        if (moving != s.bottomPiece()) {
          moving = null;
        }
      }
      if (Decorator.getDecorator(moving, PBConcealment.class) != null
          && !it.hasMoreElements()) {
        movingConcealment = moving;
        formerParent = movingConcealment.getParent();
      }
    }
    Command c = super.movePieces(m, p);
    if (c == null || c.isNull()) {
      return c;
    }
    if (movingConcealment != null) {
      if (movingConcealment.getParent() != null) {
        c.append(PBConcealable.adjustConcealment(movingConcealment.getParent()));
      }
      if (formerParent != null) {
        c.append(PBConcealable.adjustConcealment(formerParent));
      }
    }
    return c;
  }

 
}
