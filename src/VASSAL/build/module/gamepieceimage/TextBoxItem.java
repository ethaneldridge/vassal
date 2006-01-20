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
import java.awt.image.BufferedImage;

import javax.swing.JLabel;

public class TextBoxItem extends TextItem {

  public static final String TYPE = "TextBox";
  
  protected static final String WIDTH = "width";
  protected static final String HEIGHT = "height";
  protected static final String ALIGN = "align";
  protected static final String[] ALIGNMENTS = new String[] { LEFT, CENTER, RIGHT };
  
  protected int height = 30;
  protected int width = 40;
  protected String alignment = CENTER;
  protected boolean isHTML = false;

  public TextBoxItem() {
    super();
  }

  public TextBoxItem(GamePieceLayout l) {
    super(l);
  }

  public TextBoxItem(GamePieceLayout l, String n) {
    this(l);
    setConfigureName(n);
  }
  
  public String[] getAttributeDescriptions() {
    String a[] = new String[] { "Width:  ", "Height:  ", "Text Alignment:  " };
    String b[] = super.getAttributeDescriptions();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = new Class[] { Integer.class, Integer.class, AlignConfig.class };
    Class b[] = super.getAttributeTypes();
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public static class AlignConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return ALIGNMENTS;
    }
  }
  
  public String[] getAttributeNames() {
    String a[] = new String[] { WIDTH, HEIGHT, ALIGN };
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
    else if (ALIGN.equals(key)) {
      alignment = (String) o;
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
    else if (ALIGN.equals(key)) {
      return alignment;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }
  
  public int getWidth() {
    return width;
  }

  public int getHeight() {
    return height;
  }
  
  public void draw(Graphics g, GamePieceImage defn) {

    TextBoxItemInstance tbi = null;
    if (defn != null) {
      tbi = defn.getTextBoxInstance(getConfigureName());
    }
    if (tbi == null) {
      tbi = new TextBoxItemInstance();
    }
    
    Color fg = tbi.getFgColor().getColor();
    Color bg = tbi.getBorderColor().getColor();

    Point origin = layout.getPosition(this);
    Rectangle r = new Rectangle(origin.x, origin.y, getWidth(), getHeight());
    String s = null;
    if (textSource.equals(SRC_FIXED)) {
      s = text;
    }
    else {
      if (defn != null) {
        if (tbi != null) {
          s = tbi.getValue();
        }
      }
    }

//    if (s.length() < 7 || ! s.substring(1,6).toLowerCase().equals("<html>")) {
//      s = "<html>" + s;
//    }
    
    if (isAntialias()) {    
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
    } 
    else {
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_OFF);
    }
    
    if (fg != null) {
      g.setColor(fg);
      g.fillRect(r.x, r.y, r.width, r.height);
    }
 
    if (bg != null) {
      g.setColor(bg);
      g.drawRect(r.x, r.y, r.width, r.height);
    }
    
    JLabel l = new JLabel(s);
    l.setSize(width-2, height-2);
    if (fg != null) l.setBackground(fg);
    if (bg != null) l.setForeground(bg);
    FontStyle fs = FontManager.getFontManager().getFontStyle(fontStyleName);
    Font f = fs.getFont();
    l.setFont(f);
    l.setVerticalAlignment(JLabel.TOP);
    if (alignment.equals(LEFT)) {
      l.setHorizontalAlignment(JLabel.LEFT);
    }
    else if (alignment.equals(CENTER)) {
      l.setHorizontalAlignment(JLabel.CENTER);
    }
    else if (alignment.equals(RIGHT)) {
      l.setHorizontalAlignment(JLabel.RIGHT);
    }
    
    BufferedImage bi = new BufferedImage(Math.max(l.getWidth(), 1), Math.max(l.getHeight(), 1), BufferedImage.TYPE_4BYTE_ABGR);
    Graphics big = bi.getGraphics();
    l.paint(big);
    g.drawImage(bi, origin.x+1, origin.y+1, null);
  }
  
  public String getType() {
    return TextBoxItem.TYPE;
  }

  public Dimension getSize() {
    return new Dimension(getWidth(),getHeight());
  }


  public static Item decode(GamePieceLayout l, String s) {
    
    TextBoxItem item = new TextBoxItem(l);
    
    SequenceEncoder.Decoder sd1 = new SequenceEncoder.Decoder(s, ',');
    String s1 = sd1.nextToken("");
    
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s1, ';');
    sd.nextToken();
    item.width = sd.nextInt(30);
    item.height = sd.nextInt(40);
    
    TextItem.decode(item, sd1.nextToken(""));
        
    return item;
  }
  
  public String encode() {
   
    SequenceEncoder se1 = new SequenceEncoder(TextBoxItem.TYPE, ';');
    
    se1.append(width);
    se1.append(height);
   
    SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), ',');
    se2.append(super.encode());
    
    return se2.getValue();
  }
  
  
}
