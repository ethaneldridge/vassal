/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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
 
package Generic;

import VASSAL.tools.SequenceEncoder;

public class TextInstance extends Instance {

  protected String value;
  
  public TextInstance(String nam, String typ, String loc, String val) {
    super(nam, typ, loc);
    setValue(val);
   }
  
  public TextInstance(String code) {
    super();
    decode(code);
  }
  
  public void setValue(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public String encode() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(getType());
    se.append(getName());
    se.append(getLocation());
    se.append(getValue());
    return se.getValue();
  }
  
  public void decode(String code) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ';');
    setType(sd.nextToken(""));
    setName(sd.nextToken(""));
    setLocation(sd.nextToken(""));
    setValue(sd.nextToken(""));
  }

}
