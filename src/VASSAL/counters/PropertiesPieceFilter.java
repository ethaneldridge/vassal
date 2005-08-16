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
package VASSAL.counters;

import java.util.StringTokenizer;

/**
 * Accepts pieces based on whether the piece has properties that
 * match a given set of conditions
 */
public class PropertiesPieceFilter implements PieceFilter {
  private String name;
  private String value;

  public PropertiesPieceFilter(String name, String value) {
    this.name = name;
    this.value = value;
  }

  public boolean accept(GamePiece piece) {
    return value.equals(piece.getProperty(name));
  }

  /**
   * Return a PieceFilter parsed from a boolean expression such as
   * prop1 = value1 & prop2 = value2 | prop3 = value3
   * @param expression
   * @return
   */
  public static PieceFilter parse(String expression) {
    StringTokenizer st = new StringTokenizer(expression, "|");
    PieceFilter f;
    if (st.countTokens() > 1) {
      f = parse(st.nextToken());
      while (st.hasMoreTokens()) {
        f = new BooleanOrPieceFilter(f, parse(st.nextToken()));
      }
    }
    else {
      st = new StringTokenizer(expression,"&");
      if (st.countTokens() > 1) {
        f = parse(st.nextToken());
        while (st.hasMoreTokens()) {
          f = new BooleanAndPieceFilter(f,parse(st.nextToken()));
        }
      }
      else {
        st = new StringTokenizer(expression,"=");
        if (st.countTokens() == 2) {
          f = new PropertiesPieceFilter(st.nextToken().trim(),st.nextToken().trim());
        }
        else {
          f = new PieceFilter() {
            public boolean accept(GamePiece piece) {
              return false;
            }
          };
        }
      }
    }
    return f;
  }
}
