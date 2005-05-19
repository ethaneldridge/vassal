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

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.Properties;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.configure.StringEnum;

public class SymbolItem extends Item {  

  protected static final String WIDTH = "width";
  protected static final String HEIGHT = "height";
  protected static final String SYMBOL_SET = "symbolset";
  protected static final String LINE_WIDTH = "linewidth";

  protected int height = 30;
  protected int width = 40;
  protected String symbolSet = "";
  protected double lineWidth = 2.0f;

  public String[] getAttributeDescriptions() {
    String a[] = new String[] { "Symbol Set:  ", "Width:  ", "Height:  ", "Line Width:  " };
    String b[] = super.getAttributeDescriptions();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 3);
    System.arraycopy(a, 0, c, 3, a.length);
    System.arraycopy(b, 3, c, a.length+3, b.length-3);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = new Class[] { SymbolSetPrompt.class, Integer.class, Integer.class, Double.class };
    Class b[] = super.getAttributeTypes();
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 3);
    System.arraycopy(a, 0, c, 3, a.length);
    System.arraycopy(b, 3, c, a.length+3, b.length-3);
    return c;
  }

  public String[] getAttributeNames() {
    String a[] = new String[] { SYMBOL_SET, WIDTH, HEIGHT, LINE_WIDTH };
    String b[] = super.getAttributeNames();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 3);
    System.arraycopy(a, 0, c, 3, a.length);
    System.arraycopy(b, 3, c, a.length+3, b.length-3);
    return c;
  }
  
  public static class SymbolSetPrompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return Symbol.SYMBOL_SETS;
    }
  }
  public void setAttribute(String key, Object o) {
    
    if (WIDTH.equals(key)) {
      if (o instanceof String) {
        o = new Integer((String) o);
      }
      width = ((Integer) o).intValue();
    }
    else if (HEIGHT.equals(key)) {
      if (o instanceof String) {
        o = new Integer((String) o);
      }
      height = ((Integer) o).intValue();
    }    
    else if (SYMBOL_SET.equals(key)) {
      symbolSet = (String) o;
    }
    else if (LINE_WIDTH.equals(key)) {
      if (o instanceof String) {
        o = new Double(Double.parseDouble((String) o));
      }
      lineWidth = ((Double) o).doubleValue();
    }
    else
      super.setAttribute(key, o);

  }
  
  public String getAttributeValueString(String key) {
    
    if (WIDTH.equals(key)) {
      return width + "";
    }
    else if (HEIGHT.equals(key)) {
      return height + "";
    }
    else if (SYMBOL_SET.equals(key)) {
      return symbolSet;
    }
    else if (LINE_WIDTH.equals(key)) {
      return lineWidth + "";
    }
    else
      return super.getAttributeValueString(key);

  }
  
  public void addTo(Buildable parent) {
    super.addTo(parent);
    width = layout.getLayoutWidth() / 2;
    height = (int) (width * 0.75);
  }
  
  public int getWidth() {
    return width;
  }

  public int getHeight() {
    return height;
  }
  
  public void draw(Graphics g, Properties p) {

    Symbol symbol = new Symbol(symbolSet, "Artillery");

    Point origin = getOrigin();
    origin.translate(-getWidth() / 2, -getHeight() / 2);    
    Rectangle r = new Rectangle(origin.x, origin.y, getWidth(), getHeight());
    
    symbol.draw(g, r, getFgColor(), getBgColor(), (float) lineWidth);
    
  }
  
}
