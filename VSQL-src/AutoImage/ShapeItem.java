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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.SequenceEncoder;

public class ShapeItem extends Item {

  public static final String TYPE = "Box";
  
  protected static final String WIDTH = "width";
  protected static final String HEIGHT = "height";
  protected static final String SHAPE = "shape";
  protected static final String BEVEL = "bevel";

  protected static final String RECT = "Rectangle";
  protected static final String RRECT = "Rounded Rectangle";
  protected static final String OVAL = "Oval";
  
  protected int height = 30;
  protected int width = 40;
  protected int bevel = 5;
  protected String shape = RECT;
  

  public ShapeItem() {
    super();
  }

  public ShapeItem(Layout l) {
    super(l);
  }

  public ShapeItem(Layout l, String n) {
    this(l);
    setConfigureName(n);
  }
  
  public String[] getAttributeDescriptions() {
    String a[] = new String[] { "Width:  ", "Height:  ", "Shape:  ", "Bevel:  " };
    String b[] = super.getAttributeDescriptions();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = new Class[] { Integer.class, Integer.class, ShapeConfig.class, Integer.class };
    Class b[] = super.getAttributeTypes();
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public String[] getAttributeNames() {
    String a[] = new String[] { WIDTH, HEIGHT, SHAPE, BEVEL };
    String b[] = super.getAttributeNames();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }
  
  public static class ShapeConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { RECT, RRECT, OVAL };
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
    else if (SHAPE.equals(key)) {
      shape = (String) o;
    }
    else if (BEVEL.equals(key)) {
      if (o instanceof String) {
        o = new Integer((String) o);
      }
      bevel = ((Integer) o).intValue();
    }  
    else {
      super.setAttribute(key, o);
    }
    
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
    else if (SHAPE.equals(key)) {
      return shape;
    }
    else if (BEVEL.equals(key)) {
      return bevel + "";
    }
    else {
      return super.getAttributeValueString(key);
    }
  }
  
  public VisibilityCondition getAttributeVisibility(String name) {
    if (ROTATION.equals(name)) {
       return falseCond;
    }
    else if (BEVEL.equals(name)) {
      return bevelCond;
    }
    else {
      return super.getAttributeVisibility(name);
    }
   }

  private VisibilityCondition falseCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return false;
    }
  };
  
  private VisibilityCondition bevelCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return shape.equals(RRECT);
    }
  };
  
  public int getWidth() {
    return width;
  }

  public int getHeight() {
    return height;
  }
  
  public void draw(Graphics g, ImageDefn defn) {

    ShapeItemInstance si = null;
    if (defn != null) {
      si = defn.getShapeInstance(getConfigureName());
    }
    if (si == null) {
      si = new ShapeItemInstance();
    }
    
    Color fg = si.getFgColor().getColor();
    Color bg = si.getBorderColor().getColor();
    
    Point origin = getOrigin();
    origin.translate(-getWidth() / 2, -getHeight() / 2);    
    Rectangle r = new Rectangle(origin.x, origin.y, getWidth(), getHeight());

    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
    
    g.setColor(fg);
    if (shape.equals(RECT)) {
      g.fillRect(r.x, r.y, r.width, r.height);
    }
    else if (shape.equals(RRECT)) {
      g.fillRoundRect(r.x, r.y, r.width, r.height, bevel*2, bevel*2);
    }
    else if (shape.equals(OVAL)) {
      g.fillOval(r.x, r.y, r.width, r.height);
    }
    
    if (bg != null) {
      g.setColor(bg);
      if (shape.equals(RECT)) {
        g.drawRect(r.x, r.y, r.width, r.height);
      }
      else if (shape.equals(RRECT)) {
        g.drawRoundRect(r.x, r.y, r.width, r.height, bevel*2, bevel*2);
      }
      else if (shape.equals(OVAL)) {
        g.drawOval(r.x, r.y, r.width, r.height);
      }
    }
  }
  
  public String getType() {
    return TYPE;
  }
 
  
  public static Item decode(Layout l, String s) {
    
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    
    ShapeItem item = new ShapeItem(l);
    
    sd.nextToken();
    item.width = sd.nextInt(30);
    item.height = sd.nextInt(40);
    item.shape = sd.nextToken(RECT);
    item.bevel = sd.nextInt(5);
    
    return item;
  }
  
  public String encode() {
   
    SequenceEncoder se1 = new SequenceEncoder(TYPE, ';');
    
    se1.append(width);
    se1.append(height);
    se1.append(shape);
    se1.append(bevel);
   
    SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), '|');
    se2.append(super.encode());
    
    return se2.getValue();
  }
  
  
}
