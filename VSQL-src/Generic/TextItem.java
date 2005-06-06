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
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.Labeler;
import VASSAL.tools.SequenceEncoder;

public class TextItem extends Item {

  public static final String TYPE = "Text";

  protected static final String FONT = "font";
  protected static final String ALIGN = "align";
  protected static final String FIXED = "fixed";
  protected static final String TEXT = "text";

  protected static final String LEFT = "left";
  protected static final String CENTER = "center";
  protected static final String RIGHT = "right";

  protected FontStyle fontStyle = new FontStyle();
  protected String alignment = CENTER;
  protected boolean fixed;
  protected String text = "";

  public TextItem() {
    super();
  }

  public TextItem(CounterLayout l) {
    super(l);
  }

  public String[] getAttributeDescriptions() {
    String a[] = new String[] { "Font style:  ", "Alignment:  ", "Fixed Text?  ", "Text:  " };
    String b[] = super.getAttributeDescriptions();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length + 2, b.length - 2);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = new Class[] { FontStyleConfig.class, AlignConfig.class, Boolean.class, String.class };
    Class b[] = super.getAttributeTypes();
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length + 2, b.length - 2);
    return c;
  }

  public String[] getAttributeNames() {
    String a[] = new String[] { FONT, ALIGN, FIXED, TEXT };
    String b[] = super.getAttributeNames();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length + 2, b.length - 2);
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
        o = FontManager.getFontManager().getFontStyle((String) o);
      }
      fontStyle = (FontStyle) o;
    }
    else if (ALIGN.equals(key)) {
      alignment = (String) o;
    }
    else if (FIXED.equals(key)) {
      if (o instanceof String) {
        o = new Boolean((String) o);
      }
      fixed = ((Boolean) o).booleanValue();
    }
    else if (TEXT.equals(key)) {
      text = (String) o;
    }
    else {
      super.setAttribute(key, o);
    }

    if (layout != null) {
      layout.refresh();
    }

  }

  public String getAttributeValueString(String key) {

    if (FONT.equals(key)) {
      return fontStyle.getConfigureName();
    }
    else if (ALIGN.equals(key)) {
      return alignment;
    }
    else if (FIXED.equals(key)) {
      return fixed + "";
    }
    else if (TEXT.equals(key)) {
      return text;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (TEXT.equals(name)) {
      return fixedCond;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  private VisibilityCondition fixedCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return fixed;
    }
  };

  public void draw(Graphics g, SchemeElement se, ImageDefn defn) {

    Font f = fontStyle.getFont();
    Color fg = se.getFgColor().getColor();
    Color bg = se.getBgColor().getColor();

    int align = Labeler.CENTER;
    if (alignment == LEFT) {
      align = Labeler.LEFT;
    }
    else if (alignment == RIGHT) {
      align = Labeler.RIGHT;
    }

    Point origin = getOrigin();
    String s = null;
    if (fixed) {
      s = text;
    }
    else {
      if (defn != null) {
        TextInstance ti = defn.getTextInstance(getConfigureName());
        if (ti != null) {
          s = ti.getValue();
        }
      }
    }
    if (s == null) {
      s = "Xx";
    }

    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
    Labeler.drawLabel(g, s, origin.x, origin.y, f, align, Labeler.CENTER, fg, bg, null, getRotation());
  }

  public String getType() {
    return TYPE;
  }

  public static Item decode(CounterLayout l, String s) {

    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');

    TextItem item = new TextItem(l);

    sd.nextToken();
    item.fontStyle = FontManager.getFontManager().getFontStyle(sd.nextToken());
    item.alignment = sd.nextToken();

    return item;
  }

  public String encode() {

    SequenceEncoder se1 = new SequenceEncoder(TYPE, ';');

    se1.append(fontStyle.getConfigureName());
    se1.append(alignment);

    SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), '|');
    se2.append(super.encode());

    return se2.getValue();
  }

}
