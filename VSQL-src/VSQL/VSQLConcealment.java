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

package VSQL;

import VASL.counters.ASLProperties;
import VASL.counters.ColoredBox;
import VASL.counters.Concealable;
import VASL.counters.Concealment;
import VASSAL.build.GameModule;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

public class VSQLConcealment extends Concealment {
  
  public VSQLConcealment() {
    super();
  }

  public VSQLConcealment(String type, GamePiece p) {
    super(type, p);
  }
  
  public boolean canConceal(GamePiece p) {
    Concealable c = (Concealable) Decorator.getDecorator(p, Concealable.class);
    if (c == null
        || !c.isMaskableBy(GameModule.getUserId())) {
      return false;
    }
    else {
      return getNationality().equals(c.getProperty(ASLProperties.NATIONALITY));
    }
  }
  
  private String getNationality() {
    String value = nation;
    if (value == null) {
      ColoredBox b = (ColoredBox) Decorator.getDecorator(this,ColoredBox.class);
      if (b != null) {
        value = b.getColorId();
      }
    }
    return value;
  }
}