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
 
package TDC;

import VASSAL.build.module.Map;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Obscurable;

public class TdcObscurable extends Obscurable {

  public static final String LOCATION_NAME = "locationName";
  
  public TdcObscurable() {
    super();
  }

  public TdcObscurable(String type, GamePiece d) {
    super(type, d);
  }
  
  public Object getProperty(Object key) {
    
    if (LOCATION_NAME.equals(key)) {
      String loc = "";
      Map map = getMap();
      if (map != null) {
        loc = map.locationName(this.getPosition());
      }
      return loc;
    }
    else {
      return super.getProperty(key);      
    }
  }
  
}
