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
package VASSAL.tools;

import java.util.NoSuchElementException;

/**
 * Encodes a sequence of Strings into a single String with a given delimiter.
 * Escapes the delimiter character if it occurs in the element strings.
 *
 * This is a very handy class for storing structured data into flat text and quite a bit faster than parsing
 * an XML document.
 *
 * For example, a structure such as {A,{B,C}} can be encoded with
 *
 * <pre>
 * new SequenceEncoder("A",',').append(new SequenceEncoder("B",',').append("C").getValue()).getValue()
 * </pre>
 *
 * which returns <code>A,B\,C</code>
 *
 * and then extracted with
 *
 * <pre>
 * SequenceEncoder.Decoder st = new SequenceEncoder.Decoder("A,B\,C",',');
 * String A = st.nextToken();
 * SequenceEncoder.Decoder BC = new SequenceEncoder.Decoder(st.nextToken(),',');
 * String B = BC.nextToken();
 * String C = BC.nextToken();
 * </pre>
 */
public class SequenceEncoder {
  private String value;
  private char delimit;

  public SequenceEncoder(char delimiter) {
    this(null, delimiter);
  }

  public SequenceEncoder(String val, char delimiter) {
    delimit = delimiter;
    value = val == null ? null : insertBackslash(val);
  }

  public SequenceEncoder append(String s) {
    if (value == null) {
      value = insertBackslash(s);
    }
    else {
      value += delimit + insertBackslash(s);
    }
    return this;
  }

  public String getValue() {
    return value;
  }

  private String insertBackslash(String s) {
    int begin = 0;
    int end = s.indexOf(delimit);

    String val = "";
    while (begin <= end) {
      val = val.concat(s.substring(begin, end)) + '\\';
      begin = end;
      end = s.indexOf(delimit, end + 1);
    }
    val = val.concat(s.substring(begin));
    if (val.endsWith("\\")
      || (val.startsWith("'")
      && val.endsWith("'"))) {
      val = "'".concat(val).concat("'");
    }
    return val;
  }

  public static class Decoder {
    private String val;
    private char delimit;

    public Decoder(String value, char delimiter) {
      val = value;
      delimit = delimiter;
    }

    public boolean hasMoreTokens() {
      return val != null;
    }

    public String nextToken() {
      if (!hasMoreTokens()) {
        throw new NoSuchElementException();
      }
      String value = val;
      int i = val.indexOf(delimit);
      if (i < 0) {
        value = val;
        val = null;
      }
      else {
        value = "";
        int begin = 0;
        int end = i;
        while (begin < end) {
          if (val.charAt(end - 1) == '\\') {
            value += val.substring(begin, end - 1);
            begin = end;
            end = val.indexOf(delimit, end + 1);
          }
          else {
            break;
          }
        }
        if (end < 0) {
          value += val.substring(begin);
          val = null;
        }
        else {
          value += val.substring(begin, end);
          val = end >= val.length() - 1 ? "" : val.substring(end + 1);
        }
      }
      if (value.startsWith("'")
        && value.endsWith("'")) {
        value = value.substring(1, value.length() - 1);
      }
      return value;
    }

    /**
     * Parse the next token into an integer
     * @param defaultValue Return this value if no more tokens, or next token doesn't parse to an integer
     * @return
     */
    public int nextInt(int defaultValue) {
      try {
        return Integer.parseInt(nextToken());
      }
      catch (NumberFormatException e) {
        return defaultValue;
      }
      catch (NoSuchElementException e) {
        return defaultValue;
      }
    }

    /**
     * Return the first character of the next token
     * @param defaultValue Return this value if no more tokens, or if next token has zero length
     * @return
     */
    public char nextChar(char defaultValue) {
      try {
        String s = nextToken();
        return s.length() > 0 ? s.charAt(0) : defaultValue;
      }
      catch (NoSuchElementException e) {
        return defaultValue;
      }
    }
  }

  public static void main(String args[]) {
    SequenceEncoder se = new SequenceEncoder(',');
    for (int i = 0; i < args.length; ++i) {
      se.append(args[i]);
    }
    System.out.println(se.getValue());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(se.getValue(), ',');
    while (st.hasMoreTokens()) {
      System.out.println(st.nextToken());
    }
  }
}
