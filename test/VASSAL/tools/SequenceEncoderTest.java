/*
 * $Id$
 *
 * Copyright (c) 2009 by Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available 
 * at http://www.opensource.org.
 */
package VASSAL.tools;

import org.junit.Test;

import static org.junit.Assert.*;

public class SequenceEncoderTest {
  @Test
  public void testSingleQuoteBug2481() {
    // NB: This input can only be produced by hand-editing,
    // not by SequenceEncoder.
    final String bad = "stuff,'";

    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(bad, ',');

    assertEquals("stuff", sd.nextToken());
    assertEquals("'", sd.nextToken());
  }
  
  @Test
  public void testInitialNullBug3465() {
    // SequenceEncoder was failing to include initial null in sequence
    // Nulls are decoded as empty strings
    final String value2 = "value";
    final char delim = ';';
    
    final SequenceEncoder se = new SequenceEncoder(null,delim);
    se.append(value2);
    
    final SequenceEncoder.Decoder sd 
        = new SequenceEncoder.Decoder(se.getValue(),delim);
    
    assertEquals("", sd.nextToken());
    assertEquals(value2, sd.nextToken());
  }
}
