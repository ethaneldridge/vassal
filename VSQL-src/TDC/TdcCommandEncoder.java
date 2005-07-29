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

import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Obscurable;

public class TdcCommandEncoder extends BasicCommandEncoder {

  public TdcCommandEncoder() {
  }
  
  protected Decorator createDecorator(String type, GamePiece inner) {
   if (type.startsWith(Obscurable.ID)) {
      return new TdcObscurable(type, inner);
    }
   else {
     return super.createDecorator(type,inner);
   }
  }

}
