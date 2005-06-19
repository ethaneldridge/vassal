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
 
package AutoImage;

import java.awt.Image;
import java.io.IOException;

import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;

public class AIEmbellishment extends Embellishment {

  public AIEmbellishment() {
    super();
  }
  
  public AIEmbellishment(String type, GamePiece d) {
    super(type, d);
  }
  
  protected Image getCurrentImage() throws IOException {
    return new AutoImage("ge-inf-574").getImage();
    //return super.getCurrentImage();
    
  }

  
}
