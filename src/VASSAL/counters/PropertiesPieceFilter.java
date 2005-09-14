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
  
  public static final String EQ = "=";
  public static final String NE = "!=";
  public static final String LT = ".lt.";
  public static final String LE = ".le.";
  public static final String GT = ".gt.";
  public static final String GE = ".ge.";
  public static final String[] CONDITIONS = new String[] {EQ, NE, LT, LE, GT, GE};
  
  private String name;
  private String value;
  private String condition;

  public PropertiesPieceFilter(String name, String value) {
    this.name = name;
    this.value = value;
    this.condition = EQ;
  }

  public PropertiesPieceFilter(String name, String condition, String value) {
    this(name, value);
    this.condition = condition;
  }
  
  public boolean accept(GamePiece piece) {

    String v1 = (String) piece.getProperty(name) + "";
    String v2 = value;

    if (EQ.equals(condition)) {
      return v1.equals(v2);
    }
    else if (NE.equals(condition)) {
      return !v1.equals(v2);
    }
    else if (LT.equals(condition)) {
      try { return Integer.parseInt(v1) < Integer.parseInt(v2); } 
      catch (Exception e) { return v1.compareTo(v2) < 0; }
    }
    else if (LE.equals(condition)) {
      try { return Integer.parseInt(v1) <= Integer.parseInt(v2); } 
      catch (Exception e) { return v1.compareTo(v2) <= 0; }
    }
    else if (GT.equals(condition)) {
      try { return Integer.parseInt(v1) > Integer.parseInt(v2); } 
      catch (Exception e) { return v1.compareTo(v2) > 0; }
    }
    else if (GE.equals(condition)) {
      try { return Integer.parseInt(v1) >= Integer.parseInt(v2); } 
      catch (Exception e) { return v1.compareTo(v2) >= 0; }
    }

    return false;
    
  }

  /**
   * Return a PieceFilter parsed from a boolean expression such as
   * prop1 = value1 & prop2 = value2 | prop3 = value3
   * @param expression
   * @return
   */
  public static PieceFilter parse(String expression) {
    StringTokenizer st = new StringTokenizer(expression, "|");
    PieceFilter f = null;
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
        for (int i = 0; i < CONDITIONS.length && f == null; i++) {
          String[] s = expression.split(CONDITIONS[i]);
          if (s.length == 2) {
            f = new PropertiesPieceFilter(s[0].trim(), CONDITIONS[i], s[1].trim());
          }
        }
        if (f == null) {
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
