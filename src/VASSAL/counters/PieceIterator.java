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

import java.util.Enumeration;
import java.util.Iterator;

/**
 * An iterator for GamePieces.  Takes an optional PieceFilter to extract GamePiece instances from an Enumeration
 */
public class PieceIterator {
  private Enumeration enum;
  private PieceFilter filter;
  private GamePiece next;

  public PieceIterator(Enumeration e) {
    this(e, null);
  }

  public PieceIterator(Enumeration e, PieceFilter f) {
    this(f);
    reset(e);
  }

  public PieceIterator(PieceFilter f) {
    filter = f;
  }

  /** This supports having the same instance of an Iterator be usable many times,
   * cutting down on 'new' overhead
   * @param e
   */
  public void reset(Enumeration e) {
    enum = e;
    next = next();
  }

  public PieceIterator(final Iterator it) {
    this(new Enumeration() {
      public boolean hasMoreElements() {
        return it.hasNext();
      }

      public Object nextElement() {
        return it.next();
      }
    });
  }

  private GamePiece next() {
    while (enum.hasMoreElements()) {
      Object o = enum.nextElement();
      if (o instanceof GamePiece
        && (filter == null
        || filter.accept((GamePiece) o))) {
        return (GamePiece) o;
      }
    }
    return null;
  }

  public GamePiece nextPiece() {
    GamePiece p = next;
    next = next();
    return p;
  }

  public boolean hasMoreElements() {
    return next != null;
  }

  /**
   * @return a PieceIterator containing only those GamePieces visible to the user in the given Enumeration
   */
  public static PieceIterator visible(Enumeration e) {
    return new PieceIterator(e, new PieceFilter() {
      public boolean accept(GamePiece piece) {
        return !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME));
      }
    });
  }
}
