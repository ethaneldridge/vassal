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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;

import javax.swing.KeyStroke;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

public class TextItem extends Item {

  public static final String TYPE = "Text";

  protected static final String FONT = "font";
  protected static final String ALIGN = "align";
  protected static final String SOURCE = "source";
  protected static final String TEXT = "text";
  protected static final String CHANGE_CMD = "changeCmd";
  protected static final String CHANGE_KEY = "changeKey";
  protected static final String LOCKABLE = "lockable";
  protected static final String LOCK_CMD = "lockCmd";
  protected static final String LOCK_KEY = "lockKey";
  protected static final String NAME_FORMAT = "nameFormat";

  protected static final String LEFT = "left";
  protected static final String CENTER = "center";
  protected static final String RIGHT = "right";

  public static final String SRC_VARIABLE = "Variable";
  public static final String SRC_FIXED = "Fixed";
  public static final String SRC_COMMAND = "Command";

  protected static final String PIECE_NAME = "pieceName";
  protected static final String LABEL = "label";
  protected static final String DEFAULT_FORMAT = "$"+PIECE_NAME+"$";
  
  public static final int AL_CENTER = 0;
  public static final int AL_RIGHT = 1;
  public static final int AL_LEFT = 2;
  public static final int AL_TOP = 3;
  public static final int AL_BOTTOM = 4;
  
  protected FontStyle fontStyle = new FontStyle();
  protected String alignment = CENTER;
  protected String textSource = SRC_VARIABLE;
  protected String text = "";

  protected String changeCmd = "";
  protected KeyStroke changeKey;
  protected boolean lockable = false;
  protected String lockCmd = "";
  protected KeyStroke lockKey;
  
  protected FormattedString nameFormat = new FormattedString(DEFAULT_FORMAT);

  public TextItem() {
    super();
  }

  public TextItem(Layout l) {
    super(l);
  }
  
  public TextItem(Layout l, String nam) {
    this(l);
    setConfigureName(nam);
  }

  public String[] getAttributeDescriptions() {
    String a[] = new String[] { "Font style:  ", "Alignment:  ", "Label Source:  ", "Label:  ",
        "Change Label Command Name:  ", "Change Label Keyboard Command:  ", "Lockable?", "Lock Label Command Name:  ",
        "Lock Label Keyboard Command:  ", "Counter Name Format:  " };
    String b[] = super.getAttributeDescriptions();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length + 2, b.length - 2);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = new Class[] { FontStyleConfig.class, AlignConfig.class, TextSource.class, String.class, 
        String.class, KeyStroke.class, Boolean.class, String.class, KeyStroke.class, NameFormatConfig.class };
    Class b[] = super.getAttributeTypes();
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length + 2, b.length - 2);
    return c;
  }

  public String[] getAttributeNames() {
    String a[] = new String[] { FONT, ALIGN, SOURCE, TEXT, CHANGE_CMD, CHANGE_KEY, LOCKABLE, LOCK_CMD, LOCK_KEY, NAME_FORMAT };
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
    else if (SOURCE.equals(key)) {
      textSource = (String) o;
    }
    else if (TEXT.equals(key)) {
      text = (String) o;
    }
    else if (CHANGE_CMD.equals(key)) {
      changeCmd = (String) o;
    }
    else if (CHANGE_KEY.equals(key)) {
      if (o instanceof String) {
        o = HotKeyConfigurer.decode((String) o);
      }
      changeKey = (KeyStroke) o;
    }
    else if (LOCKABLE.equals(key)) {
      if (o instanceof String) {
        o = new Boolean((String) o);
      }
      lockable = ((Boolean) o).booleanValue();
    }
    else if (LOCK_CMD.equals(key)) {
      lockCmd = (String) o;
    }
    else if (LOCK_KEY.equals(key)) {
      if (o instanceof String) {
        o = HotKeyConfigurer.decode((String) o);
      }
      lockKey = (KeyStroke) o;
    }
    else if (NAME_FORMAT.equals(key)) {
      nameFormat.setFormat((String) o);
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
    else if (SOURCE.equals(key)) {
      return textSource + "";
    }
    else if (TEXT.equals(key)) {
      return text;
    }
    else if (CHANGE_CMD.equals(key)) {
      return changeCmd + "";
    }
    else if (CHANGE_KEY.equals(key)) {
      return HotKeyConfigurer.encode(changeKey);
    }
    else if (LOCKABLE.equals(key)) {
      return lockable + "";
    }
    else if (LOCK_CMD.equals(key)) {
      return lockCmd;
    }
    else if (LOCK_KEY.equals(key)) {
      return HotKeyConfigurer.encode(lockKey);
    }
    else if (NAME_FORMAT.equals(key)) {
      return nameFormat.getFormat();
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (TEXT.equals(name)) {
      return fixedCond;
    }
    else if (CHANGE_CMD.equals(name) || CHANGE_KEY.equals(name) || LOCKABLE.equals(name)) {
      return commandCond;
    }
    else if (LOCK_CMD.equals(name) || LOCK_KEY.equals(name)) {
      return lockCond;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  private VisibilityCondition fixedCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return textSource.equals(SRC_FIXED);
    }
  };

  private VisibilityCondition commandCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return textSource.equals(SRC_COMMAND);
    }
  };

  private VisibilityCondition lockCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return textSource.equals(SRC_COMMAND) && lockable;
    }
  };
  
  public void draw(Graphics g, ImageDefn defn) {

    TextItemInstance ti = null;

    if (fontStyle == null) {
      return;
    }
    
    Font f = fontStyle.getFont();

    if (defn != null) {
      ti = defn.getTextInstance(name);
    }

    if (ti == null) {
      ti = new TextItemInstance();
    }

    Color fg = ti.getFgColor().getColor();
    Color bg = ti.getBgColor().getColor();
    boolean outline = ti.isOutline();
    Color ol = ti.getOutlineColor().getColor();

    int align = AL_CENTER;
    if (alignment.equals(LEFT)) {
      align = AL_LEFT;
    }
    else if (alignment.equals(RIGHT)) {
      align = AL_RIGHT;
    }

    Point origin = getOrigin();
    String s = null;
    if (textSource.equals(SRC_FIXED)) {
      s = text;
    }
    else {
      if (defn != null) {
        if (ti != null) {
          s = ti.getValue();
        }
      }
    }

    if (isAntialias()) {    
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
    } 
    else {
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
    }

    drawLabel(g, s, origin.x, origin.y, f, align, AL_CENTER, fg, bg, null, getRotation(), outline, ol);
  }

  public String getType() {
    return TYPE;
  }

  public static Item decode(Layout l, String s) {

    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');

    TextItem item = new TextItem(l);

    sd.nextToken();
    item.fontStyle = FontManager.getFontManager().getFontStyle(sd.nextToken(FontManager.DEFAULT));
    item.alignment = sd.nextToken(CENTER);
    item.textSource = sd.nextToken(SRC_VARIABLE);
    item.text = sd.nextToken("");
    item.changeCmd = sd.nextToken("");
    item.changeKey = sd.nextKeyStroke(null);
    item.lockCmd = sd.nextToken("");
    item.lockKey = sd.nextKeyStroke(null);
    item.nameFormat.setFormat(sd.nextToken(DEFAULT_FORMAT));
    item.lockable = sd.nextBoolean(false);

    return item;
  }

  public String encode() {

    SequenceEncoder se1 = new SequenceEncoder(TYPE, ';');

    se1.append(fontStyle.getConfigureName());
    se1.append(alignment);
    se1.append(textSource);
    se1.append(text);
    se1.append(changeCmd);
    se1.append(changeKey);
    se1.append(lockCmd);
    se1.append(lockKey);
    se1.append(nameFormat.getFormat());
    se1.append(lockable);

    SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), '|');
    se2.append(super.encode());

    return se2.getValue();
  }

  public boolean isOutline() {
    return fontStyle.isOutline();
  }
  
  public boolean isFixed() {
    return textSource.equals(SRC_FIXED);
  }

  public boolean isLockable() {
    return textSource.equals(SRC_COMMAND) && lockable &&  lockKey != null;
  }
  
  public boolean isChangeable() {
    return textSource.equals(SRC_COMMAND) && changeKey != null;
  }
  
  public FormattedString getNameFormat() {
    return nameFormat;
  }
  
  public static class TextSource extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { SRC_VARIABLE, SRC_FIXED, SRC_COMMAND };
    }
  }
  
  public static class NameFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{PIECE_NAME, LABEL});
    }
  }

  public static void drawLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor, int rotateDegrees, boolean outline, Color outlineColor) {
    g.setFont(f);
    int width = g.getFontMetrics().stringWidth(text + "  ");
    int height = g.getFontMetrics().getHeight();
    int x0 = x;
    int y0 = y;
    switch (hAlign) {
      case AL_CENTER:
        x0 = x - width / 2;
        break;
      case AL_RIGHT:
        x0 = x - width;
        break;
    }
    switch (vAlign) {
      case AL_CENTER:
        y0 = y - height / 2;
        break;
      case AL_BOTTOM:
        y0 = y - height;
        break;
    }
    
    AffineTransform saveXForm = null;
    Graphics2D g2d = (Graphics2D) g;
    
    if (rotateDegrees != 0) {
      saveXForm = g2d.getTransform(); 
      AffineTransform newXForm =
        AffineTransform.getRotateInstance(Math.toRadians(rotateDegrees), x, y);
      g2d.transform(newXForm);
    }
    
    if (bgColor != null) {
      g.setColor(bgColor);
      g.fillRect(x0, y0, width, height);
    }
    if (borderColor != null) {
      g.setColor(borderColor);
      g.drawRect(x0, y0, width, height);
    }

    int y1 = y0 + g.getFontMetrics().getHeight() - g.getFontMetrics().getDescent();
    String theText = " " + text + " ";
    
    if (outline && outlineColor != null) {
      g.setColor(outlineColor);
      g.drawString(theText, x0-1, y1-1);
      g.drawString(theText, x0-1, y1+1);
      g.drawString(theText, x0+1, y1-1);
      g.drawString(theText, x0+1, y1+1);
    }
    
    g.setColor(fgColor);
    g.drawString(theText, x0, y1);
    
    if (rotateDegrees != 0) {
      g2d.setTransform(saveXForm); 
    }
  }
}
