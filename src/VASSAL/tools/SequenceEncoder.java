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
  private StringBuffer buffer;
  private char delimit;

  public SequenceEncoder(char delimiter) {
    this(null, delimiter);
  }

  public SequenceEncoder(String val, char delimiter) {
    delimit = delimiter;
    if (val != null) {
      append(val);
    }
  }

  public SequenceEncoder append(String s) {
    if (buffer == null) {
      buffer = new StringBuffer();
      appendEscapedString(s);
    }
    else {
      buffer.append(delimit);
      appendEscapedString(s);
    }
    return this;
  }

  public String getValue() {
    return buffer != null ? buffer.toString() : null;
  }

  private void appendEscapedString(String s) {
    int begin = 0;
    int end = s.indexOf(delimit);
    int length = buffer.length();

    while (begin <= end) {
      buffer.append(s.substring(begin, end)).append('\\');
      begin = end;
      end = s.indexOf(delimit, end + 1);
    }
    buffer.append(s.substring(begin));
    if (s.endsWith("\\")
      || (s.startsWith("\'")
      && s.endsWith("\'"))) {
      buffer.insert(length,"'").append("'");
    }
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
      if (val != null) {
        try {
          defaultValue = Integer.parseInt(nextToken());
        }
        catch (NumberFormatException e) {
        }
      }
      return defaultValue;
    }

    public boolean nextBoolean(boolean defaultValue) {
      if (val != null) {
        defaultValue = "true".equals(nextToken());
      }
      return defaultValue;
    }

    /**
     * Return the first character of the next token
     * @param defaultValue Return this value if no more tokens, or if next token has zero length
     * @return
     */
    public char nextChar(char defaultValue) {
      if (val != null) {
        String s = nextToken();
        defaultValue =  s.length() > 0 ? s.charAt(0) : defaultValue;
      }
      return defaultValue;
    }

    /**
     * Return the next token, or the default value if there are no more tokens
     * @param defaultValue
     * @return
     */
    public String nextToken(String defaultValue) {
      if (val != null) {
        defaultValue = nextToken();
      }
      return defaultValue;
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
