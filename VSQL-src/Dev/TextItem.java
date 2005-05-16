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

package Dev;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Point;
import java.util.Properties;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.StringEnum;
import VASSAL.counters.Labeler;

public class TextItem extends Item {

  protected static final String FONT = "font";
  protected static final String ROTATION = "rotation";
  protected static final String ALIGN = "align";

  protected static final String LEFT = "left";
  protected static final String CENTER = "center";
  protected static final String RIGHT = "right";

  protected FontStyle fontStyle = new FontStyle();
  protected int rotation;
  protected String alignment = CENTER;

  public String[] getAttributeDescriptions() {
    String a[] = new String[] { "Font style:  ", "Rotation (Degrees):  ", "Alignment:  " };
    String b[] = super.getAttributeDescriptions();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 3);
    System.arraycopy(a, 0, c, 3, a.length);
    System.arraycopy(b, 3, c, a.length+3, b.length-3);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = new Class[] { FontStyleConfig.class, Integer.class, AlignConfig.class };
    Class b[] = super.getAttributeTypes();
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 3);
    System.arraycopy(a, 0, c, 3, a.length);
    System.arraycopy(b, 3, c, a.length+3, b.length-3);
    return c;
  }

  public String[] getAttributeNames() {
    String a[] = new String[] { FONT, ROTATION, ALIGN };
    String b[] = super.getAttributeNames();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 3);
    System.arraycopy(a, 0, c, 3, a.length);
    System.arraycopy(b, 3, c, a.length+3, b.length-3);
    return c;
  }
  public static class FontStyleConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FontStyleConfigurer(key, name, ((TextItem) c).fontStyle);
    }
  }

  public static class AlignConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { LEFT, CENTER, RIGHT };
    }
  }
  
  public void setAttribute(String key, Object o) {
    if (FONT.equals(key)) {
      if (o instanceof String) {
        o = GenericsContainer.getFontStyleByName((String) o);
      }
      fontStyle = (FontStyle) o;
    }
    else if (ROTATION.equals(key)) {
      if (o instanceof String) {
        o = new Integer((String) o);
      }
      rotation = ((Integer) o).intValue();
    }
    else if (ALIGN.equals(key)) {
      alignment = (String) o;
    }
    else {
      super.setAttribute(key, o);
    }
  }
  
  public String getAttributeValueString(String key) {
    
    if (FONT.equals(key)) {
      return fontStyle.getConfigureName();
    }
    else if (ROTATION.equals(key)) {
      return rotation + "";
    }
    else if (ALIGN.equals(key)) {
      return alignment;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public void draw(Graphics g, Properties p) {
    Font f = fontStyle.getFont();
    Color fg = fgColor.getColor();
    Color bg = bgColor.getColor();
    int align = Labeler.CENTER;
    if (alignment == LEFT) {
      align = Labeler.LEFT;
    }
    else if (alignment == RIGHT) {
      align = Labeler.RIGHT;
    }
    Point pos = CounterLayout.getPosition(location, layout);
    int x = pos.x + xoffset;
    int y = pos.y + yoffset;
    String s = p.getProperty(this.getConfigureName());
    Labeler.drawLabel(g, s, x, y, f, align, Labeler.CENTER, fg, bg, null, rotation);
  }
  
}
