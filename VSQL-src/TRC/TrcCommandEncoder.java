/*
 * $Id$
 * 
 * Copyright (c) 2000-2005 by Rodney Kinney, Brent Easton
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package TRC;

import wga.WgaImmobilized;
import Inventory.InvEmbellishment;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Immobilized;

public class TrcCommandEncoder extends AutoImage.CommandEncoder {

  protected Decorator createDecorator(String type, GamePiece inner) {
    if (type.startsWith(InvEmbellishment.ID)) {
      return new InvEmbellishment(type, inner);
    }
    else if (type.startsWith(Immobilized.ID)) {
      return new WgaImmobilized(type, inner);
    }
    return super.createDecorator(type, inner);
  }
}
