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

package Dev;

import java.awt.Color;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;

public class GenericDefinition extends AbstractConfigurable {

  protected static final String NAME = "name";
  protected static final String WIDTH = "width";
  protected static final String HEIGHT = "height";
  protected static final String BGCOLOR = "bgColor";

  protected int width = 54;
  private int height = 54;
  protected ColorSwatch bgColor = new ColorSwatch();
  
  public GenericDefinition() {
    super();
    name = "";
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] {
        "Name",
        "Counter Width",
        "Counter Height",
        "Background Color"
    };
  }

  public Class[] getAttributeTypes() {
    return new Class[] {
        String.class,
        Integer.class,
        Integer.class,
        BgColorSwatchConfig.class
    };
  }

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {     
      return new ColorSwatchConfigurer(key, name, ((GenericDefinition) c).bgColor);
    }
  }
  
  public String[] getAttributeNames() {
    return new String[] {
        NAME,
        WIDTH,
        HEIGHT,
        BGCOLOR
    };
  }

  public void setAttribute(String key, Object value) {
   if (NAME.equals(key)) {
     setConfigureName((String) value);
   }
   else if (WIDTH.equals(key)) {
     if (value instanceof String) {
       value = new Integer((String) value);
     }
     setWidth(((Integer) value).intValue());
   }
   else if (HEIGHT.equals(key)) {
     if (value instanceof String) {
       value = new Integer((String) value);
     }
     setHeight(((Integer) value).intValue());
   }
   else if (BGCOLOR.equals(key)) {
     if (value instanceof String) {
       value = GenericsContainer.getColorSwatch((String) value);
     }
     bgColor = (ColorSwatch) value;
   }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (WIDTH.equals(key)) {
      return getWidth() + "";
    }
    else if (HEIGHT.equals(key)) {
      return getHeight() + "";
    }
    else if (BGCOLOR.equals(key)) {
      return bgColor.getConfigureName();
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

  protected void setWidth(int width) {
    this.width = width;
  }

  protected int getWidth() {
    return width;
  }

  protected void setHeight(int height) {
    this.height = height;
  }

  protected int getHeight() {
    return height;
  }

  protected Color getBgColor() {
    return bgColor.getColor();
  }
  
}