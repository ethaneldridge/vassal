/*
 * $Id$
 *
 * Copyright (c) 2005 by Brent Easton
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
package VSQL;

import javax.swing.KeyStroke;

import VASSAL.command.Command;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;

public class VSQLEmbellishment extends Embellishment {
  
  protected static final String CA_PREFIX = "+ CA = ";
  
  public VSQLEmbellishment() {
    super();
  }
  
  public VSQLEmbellishment(String type, GamePiece d) {
    super(type, d);
  }
  
  public Command myKeyEvent(KeyStroke stroke) {
    
    int oldValue = value;
    
    Command c = super.myKeyEvent(stroke);
    
    String name = commonName[Math.abs(value)-1];
    if (value != oldValue && name.startsWith(CA_PREFIX)) {
      String ca = name.substring(CA_PREFIX.length());
      setProperty(VSQLFootprint.VEHICLE_CA_PROPERTY, ca);
    }
    return c;
  }
}