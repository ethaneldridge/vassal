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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: May 22, 2002
 * Time: 10:25:53 PM
 * To change template for new interface use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.counters;

import VASSAL.build.module.Map;
import VASSAL.Info;

import java.awt.*;
import java.util.Enumeration;

/**
 * This interface defines selection criteria for finding a GamePiece in a Map
 */
public interface PieceFinder {
  /** Return the argument GamePiece (or one of its children if a Stack) found at the given point on the given Map */
  public GamePiece select(Map map, GamePiece piece, Point pt);

  /** Return a Stack overlapping the given point */
  public static final PieceFinder STACK_ONLY = new StackOnly();

  /**
   * If a Stack overlaps the given point, return the piece containing that point if expanded,
   * or the top piece if not expanded.
   * */
  public static final PieceFinder PIECE_IN_STACK = new PieceInStack();

  /** Returns a Stack if unexpanded and overlapping the given point,
   * or a piece within that stack if expanded and overlapping the given point
   */
  public static final PieceFinder MOVABLE = new Movable();

}

class StackOnly extends Movable {
  public GamePiece select(Map map, GamePiece piece, Point pt) {
    GamePiece selected = null;
    if (piece instanceof Stack) {
      selected = super.select(map, piece, pt);
      if (selected != null
          && !(selected instanceof Stack)) {
        selected = selected.getParent();
      }
    }
    return selected;
  }
}

class PieceInStack extends Movable {
  public GamePiece select(Map map, GamePiece piece, Point pt) {
    GamePiece selected = null;
    selected = super.select(map, piece, pt);
    if (selected != null
        && piece instanceof Stack
        && !((Stack) piece).isExpanded()) {
      selected = ((Stack) piece).topPiece();
    }
    return selected;
  }
}

class Movable implements PieceFinder {
  protected Shape[] shapes = new Shape[0];

  public GamePiece select(Map map, GamePiece piece, Point pt) {
    GamePiece selected = null;
    if (piece instanceof Stack) {
      Stack s = (Stack) piece;
        if (shapes.length < s.getPieceCount()) {
          shapes = new Shape[s.getPieceCount()];
        }
        map.getStackMetrics().getContents(s, null, shapes, null, s.getPosition().x, s.getPosition().y);
        for (Enumeration e = s.getPiecesInVisibleOrder();
             e.hasMoreElements();) {
          GamePiece child = (GamePiece) e.nextElement();
          if (Info.is2dEnabled() ? shapes[s.indexOf(child)].contains(pt) : shapes[s.indexOf(child)].getBounds().contains(pt)) {
            selected = s.isExpanded() ? child : s;
            break;
          }
        }
    }
    else if (piece.getShape().contains(pt)) {
      selected = piece;
    }
    return selected;
  }

}
