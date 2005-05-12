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

import java.awt.Color;
import java.awt.Font;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;

public class FontStyle extends AbstractConfigurable {

  protected static final String NAME = "name";
  protected static final String FONT = "fontName";
  protected static final String SIZE = "size";
  protected static final String BOLD = "bold";
  protected static final String ITALIC = "italic";
  protected static final String FG_COLOR = "fgColor";
  protected static final String BG_TRANSPARENT = "bgTransparent";
  protected static final String BG_COLOR = "bgColor";
  
  protected String fontName;
  protected StyledFont font;

  public FontStyle() {
    super();
    name = "";
    font = new StyledFont("Dialog", Font.PLAIN, 10, Color.BLACK, null);
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { 
        "Style Name", 
        "Font Style"};
    
  }

  public Class[] getAttributeTypes() {
    return new Class[] { 
        String.class, 
        FontStyleConfig.class};
  }

  public static class FontStyleConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {     
      return new StyledFontConfigurer(key, name, ((FontStyle) c).font);
    }
  }
  
  public String[] getAttributeNames() {
    return new String[] { NAME, FONT };
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (FONT.equals(key)) {
      if (o instanceof String) {
        o = StyledFontConfigurer.decode((String) o);
      }
      font = (StyledFont) o;
    }
   
  }
  
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (FONT.equals(key)) {
      return StyledFontConfigurer.encode(font);
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