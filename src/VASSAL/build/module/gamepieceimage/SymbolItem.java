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

package VASSAL.build.module.gamepieceimage;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.StringEnum;
import VASSAL.tools.SequenceEncoder;

import java.awt.*;
import java.awt.geom.AffineTransform;

public class SymbolItem extends Item {  

  public static final String TYPE = "Symbol";
  
  protected static final String SET = "set";
  protected static final String WIDTH = "width";
  protected static final String HEIGHT = "height";
  protected static final String LINE_WIDTH = "linewidth";

  protected String symbolSet = "";
  protected int height = 30;
  protected int width = 40;
  protected double lineWidth = 1.0f;
  
  public SymbolItem() {
    super();
  }

  public SymbolItem(GamePieceLayout l) {
    super(l);
    width = getLayout().getLayoutWidth() / 2;
    height = (int) (width * 0.75);
  }
  
  public SymbolItem(GamePieceLayout l, String nam) {
    this(l);
    setConfigureName(nam);
  }
  
  public String[] getAttributeDescriptions() {
    String a[] = new String[] { "Symbol Set:  ", "Width:  ", "Height:  ", "Line Width:  " };
    String b[] = super.getAttributeDescriptions();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = new Class[] { SetConfig.class, Integer.class, Integer.class, Double.class };
    Class b[] = super.getAttributeTypes();
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public String[] getAttributeNames() {
    String a[] = new String[] { SET, WIDTH, HEIGHT, LINE_WIDTH };
    String b[] = super.getAttributeNames();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }
  
  public static class SetConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return Symbol.SYMBOL_SETS;
    }
  }
  public void setAttribute(String key, Object o) {
    
    if (SET.equals(key)) {
      
    }
    else if (WIDTH.equals(key)) {
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
    
    if (SET.equals(key)) {
      return symbolSet;
    }
    else if (WIDTH.equals(key)) {
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
  
  public void draw(Graphics g, GamePieceImage defn) {

    SymbolItemInstance si = null;
    if (defn != null) {
      si = defn.getSymbolInstance(getConfigureName());
    }
    Symbol symbol = null;
    if (si == null) {
      symbol = new Symbol(Symbol.NATO, Symbol.NatoUnitSymbolSet.INFANTRY, Symbol.NatoUnitSymbolSet.NONE, Symbol.NatoUnitSymbolSet.SZ_DIVISION);
      si = new SymbolItemInstance();
    }
    else {
      symbol = new Symbol(Symbol.NATO, si.getSymbol1(), si.getSymbol2(), si.getSize());
    }

    Point origin = layout.getPosition(this);
    Rectangle r = new Rectangle(origin.x, origin.y, getWidth(), getHeight());
    
    if (getRotation() != 0) {
      Graphics2D g2d = (Graphics2D) g;
      AffineTransform newXForm =
          AffineTransform.getRotateInstance(Math.toRadians(getRotation()), layout.getPosition(this).x, layout.getPosition(this).y);
        g2d.transform(newXForm);
    }
    
    if (isAntialias()) {    
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
    } 
    else {
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_OFF);
    }
    
    symbol.draw(g, r, si.getFgColor().getColor(), si.getBgColor().getColor(), si.getSizeColor().getColor(), (float) lineWidth);
 
  }
  
  public String getType() {
    return TYPE;
  }

  public Dimension getSize() {
    return new Dimension(getWidth(),getHeight());
  }

  public static Item decode(GamePieceLayout l, String s) {
    
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
