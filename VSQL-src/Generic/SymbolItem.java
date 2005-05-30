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

package Generic;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.Properties;

import VASSAL.tools.SequenceEncoder;

public class SymbolItem extends Item {  

  public static final String TYPE = "Symbol";
  
  protected static final String WIDTH = "width";
  protected static final String HEIGHT = "height";
  protected static final String LINE_WIDTH = "linewidth";

  protected int height = 30;
  protected int width = 40;
  protected double lineWidth = 2.0f;
  
  public SymbolItem() {
    super();
  }
  
  public SymbolItem(CounterLayout l) {
    super(l);
    width = getLayout().getLayoutWidth() / 2;
    height = (int) (width * 0.75);
  }
  
  public String[] getAttributeDescriptions() {
    String a[] = new String[] { "Width:  ", "Height:  ", "Line Width:  " };
    String b[] = super.getAttributeDescriptions();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = new Class[] { Integer.class, Integer.class, Double.class };
    Class b[] = super.getAttributeTypes();
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public String[] getAttributeNames() {
    String a[] = new String[] { WIDTH, HEIGHT, LINE_WIDTH };
    String b[] = super.getAttributeNames();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
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
    else if (LINE_WIDTH.equals(key)) {
      if (o instanceof String) {
        o = new Double(Double.parseDouble((String) o));
      }
      lineWidth = ((Double) o).doubleValue();
    }
    else
      super.setAttribute(key, o);
    
    if (layout != null) {
      layout.refresh();
    }

  }
  
  public String getAttributeValueString(String key) {
    
    if (WIDTH.equals(key)) {
      return width + "";
    }
    else if (HEIGHT.equals(key)) {
      return height + "";
    }
    else if (LINE_WIDTH.equals(key)) {
      return lineWidth + "";
    }
    else
      return super.getAttributeValueString(key);

  }
  
//  public void addTo(Buildable parent) {
//    super.addTo(parent);
//
//  }
  
  public int getWidth() {
    return width;
  }

  public int getHeight() {
    return height;
  }
  
  public void draw(Graphics g, Properties p) {

    Symbol symbol = new Symbol(Symbol.NATO_UNIT_SET, "Infantry");

    Point origin = getOrigin();
    origin.translate(-getWidth() / 2, -getHeight() / 2);    
    Rectangle r = new Rectangle(origin.x, origin.y, getWidth(), getHeight());
    
    symbol.draw(g, r, Color.BLACK, Color.WHITE, (float) lineWidth);
    
  }
  
  public String getType() {
    return TYPE;
  }
  
  public static Item decode(CounterLayout l, String s) {
    
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    
    SymbolItem item = new SymbolItem(l);
    
    sd.nextToken();
    item.width = sd.nextInt(54);
    item.height = sd.nextInt(54);
    item.lineWidth = sd.nextDouble(1.0f);
    
    return item;
  }
  
  public String encode() {
   
    SequenceEncoder se1 = new SequenceEncoder(TYPE, ';');
    
    se1.append(width);
    se1.append(height);
    se1.append(lineWidth);
   
    SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), '|');
    se2.append(super.encode());
    
    return se2.getValue();
  }
  
}
