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

/** 
 * Class tht implements a names Color Swatch
 */

package Dev;

import java.awt.Color;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ColorConfigurer;

public class GenericColor extends AbstractConfigurable {

  protected static final String NAME = "name";
  protected static final String COLOR = "color";
  protected Color color;

  public GenericColor() {
    super();
    name = "";
  }
  
  public Color getColor() {
    return color;
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Color Name", "Color"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, Color.class };
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, COLOR };
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (COLOR.equals(key)) {
      if (o instanceof String) {
        o = ColorConfigurer.stringToColor((String) o);
      }
      color = (Color) o;
    }
  }


  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else
      return null;
  }

  public void removeFrom(Buildable parent) {
  
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {

  }
  
}