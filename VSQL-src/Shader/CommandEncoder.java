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
 
package Shader;

import wga.WgaImmobilized;
import AutoImage.AIEmbellishment;
import AutoImage.AutoImage;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.counters.Decorator;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Immobilized;

public class CommandEncoder extends BasicCommandEncoder {

  protected Decorator createDecorator(String type, GamePiece inner) {
    if (type.startsWith(AutoImage.ID)) {
      return new AutoImage(type, inner);
    }
    else if (type.startsWith(Embellishment.ID)) {
      return new AIEmbellishment(type, inner);
    }
    else if (type.startsWith(Immobilized.ID)) {
      return new WgaImmobilized(type, inner);
    }
    else if (type.startsWith(Shading.ID)) {
      return new Shading(type, inner);
    }
     else {
      return super.createDecorator(type, inner);
    }
  }
}