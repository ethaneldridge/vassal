/*
 * $Id$
 * 
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package AutoImage;

import java.awt.Image;
import java.awt.Rectangle;
import java.io.IOException;

import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;

public class AIEmbellishment extends Embellishment {

  protected String[] aiNames;
  protected AutoImage[] autoImages;

  public AIEmbellishment() {
    super();
  }

  public AIEmbellishment(String type, GamePiece d) {
    super(type, d);
  }

  protected Image getCurrentImage() throws IOException {
    if (value <= 0) {
      return null;
    }

    if (autoImages == null || autoImages.length < commonName.length) {
      buildAutoImages();
    }

    if (autoImages[value - 1] == null) {
      return super.getCurrentImage();
    }
    else {
      return autoImages[value - 1].getImage();
    }
  }

  public Rectangle getCurrentImageBounds() {
    if (autoImages == null) {
      buildAutoImages();
    }
    
    if (value > 0) {
      if (size[value - 1] == null) {
        if (autoImages[value - 1] == null) {
          return super.getCurrentImageBounds();
        }
        else {
          size[value - 1] = autoImages[value - 1].getImageBounds();
        }
      }
      return size[value - 1];
    }
    else {
      return new Rectangle();
    }
  }

  public String getName() {
    if (value > 0 && aiNames != null && aiNames[value-1] != null) {
      return aiNames[value-1];
    }
    else {
      return super.getName();
    }
  }

  protected void buildAutoImages() {
    autoImages = new AutoImage[commonName.length];
    aiNames = new String[commonName.length];

    for (int i = 0; i < commonName.length; i++) {
      String s = commonName[i] + "";
      if (s.startsWith("+")) {
        s = s.substring(1, s.length());
      }
      if (s.endsWith("+")) {
        s = s.substring(0, s.length() - 1);
      }
      if (s.startsWith("%") && s.endsWith("%")) {
        s = s.substring(1, s.length() - 1);
        autoImages[i] = new AutoImage(s);
        aiNames[i] = s;
      }
    }
  }
  
  public Object getProperty(Object key) {
    if (key instanceof String && ((String) key).startsWith(AutoImage.PROPERTY_PREFIX)) {
      return autoImages[value-1].getProperty(key);
    }
    return super.getProperty(key);
  }
  
}
