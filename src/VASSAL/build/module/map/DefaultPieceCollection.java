/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;

/**
 * Default implementation of {@link PieceCollection} separates pieces into
 * two layers:  stacking pieces always above non-stacking pieces
 */
public class DefaultPieceCollection implements PieceCollection {
  private SimplePieceCollection stacking = new SimplePieceCollection();
  private SimplePieceCollection nonstacking = new SimplePieceCollection();

  protected PieceCollection getCollectionForPiece(GamePiece p) {
    if (Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))) {
      return nonstacking;
    }
    else {
      return stacking;
    }
  }

  public void add(GamePiece p) {
    getCollectionForPiece(p).add(p);
  }

  public void clear() {
    stacking.clear();
    nonstacking.clear();
  }

  public GamePiece[] getPieces() {
    GamePiece[] top = stacking.getPieces();
    GamePiece[] bottom = nonstacking.getPieces();
    GamePiece[] all = new GamePiece[bottom.length+top.length];
    System.arraycopy(bottom,0,all,0,bottom.length);
    System.arraycopy(top,0,all,bottom.length,top.length);
    return all;
  }

  public int indexOf(GamePiece p) {
    return getCollectionForPiece(p).indexOf(p);
  }

  public void remove(GamePiece p) {
    getCollectionForPiece(p).remove(p);
  }

  public void moveToBack(GamePiece p) {
    getCollectionForPiece(p).moveToBack(p);
  }

  public void moveToFront(GamePiece p) {
    getCollectionForPiece(p).moveToFront(p);
  }
}
