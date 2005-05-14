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
 * Class tht implements a names Font Swatch
 */

package Dev;

import java.awt.Font;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;

public class FontStyle extends AbstractConfigurable {

  protected static final String NAME = "name";
  protected static final String STYLE = "style";
  
  protected String fontName;
  protected Font font;

  public FontStyle() {
    super();
    name = "";
    font = new Font("Dialog", Font.PLAIN, 10);
  }
  
  public Font getFont() {
    return font;
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { 
        "Style Name:  ", 
        "Font Style"};
    
  }

  public Class[] getAttributeTypes() {
    return new Class[] { 
        String.class, 
        FontStyleConfig.class};
  }

  public static class FontStyleConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {     
      return new FontStyleConfigurer(key, name, ((FontStyle) c).font);
    }
  }
  
  public String[] getAttributeNames() {
    return new String[] { NAME, STYLE };
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (STYLE.equals(key)) {
      if (o instanceof String) {
        o = FontStyleConfigurer.decode((String) o);
      }
      font = (Font) o;
    }
   
  }
  
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (STYLE.equals(key)) {
      return FontStyleConfigurer.encode(font);
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