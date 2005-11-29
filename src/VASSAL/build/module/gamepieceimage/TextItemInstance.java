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

import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import VASSAL.build.AutoConfigurable;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

public class TextItemInstance extends ItemInstance {

  protected static final String VALUE = "value";
  protected static final String OUTLINE_COLOR = "outlineColor";

  protected String val = "";
  protected ColorSwatch outlineColor = ColorSwatch.getRed();
  private boolean locked = false;

  public TextItemInstance() {
    super();
    val = "Xx";
    outlineColor = ColorSwatch.getRed();
  }

  public TextItemInstance(String nam, String typ, String loc, String val) {
    super(nam, typ, loc);
    if (val == null) {
      switch (nam.length()) {
        case 0:
          setValue("Xx");
          break;
        case 1:
          setValue(nam);
          break;
        default:
          setValue(nam.substring(0, 2));
          break;
      }
    }
    else {
      setValue(val);
    }
  }

  public TextItemInstance(String code, GamePieceImage defn) {
    super(defn);
    decode(code);
  }

  public void setValue(String value) {
    this.val = value;
  }

  public String getValue() {
    return val;
  }

  public void setLocked(boolean locked) {
    this.locked = locked;
  }

  public boolean isLocked() {
    return locked;
  }

  public boolean isOutline() {
    TextItem item = (TextItem) getItem();
    return (item == null) ? false : item.isOutline();
  }

  public ColorSwatch getOutlineColor() {
    return outlineColor;
  }

  public void setOutlineColor(ColorSwatch c) {
    outlineColor = c;
  }

  public void setState(String newState) {
    getItem();
    if (item != null && ((TextItem) item).isChangeable()) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(newState, ';');
      sd.nextToken();
      locked = sd.nextBoolean(false);
      val = sd.nextToken("");
    }
  }

  public String getState() {

    getItem();
    if (item != null && ((TextItem) item).isChangeable()) {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(getName());
      se.append(locked);
      se.append(val);
      return (se.getValue());
    }
    else {
      return "";
    }

  }

  public String encode() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(getType());
    se.append(getName());
    se.append(getLocation());
    se.append(getFgColor().encode());
    se.append(getBgColor().encode());
    se.append(getValue());
    se.append(getOutlineColor().encode());
    return se.getValue();
  }

  public void decode(String code) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ';');
    setType(sd.nextToken(""));
    setName(sd.nextToken(""));
    setLocation(sd.nextToken(""));
    setFgColor(new ColorSwatch(sd.nextToken("")));
    setBgColor(new ColorSwatch(sd.nextToken("")));
    setValue(sd.nextToken(""));
    setOutlineColor(new ColorSwatch(sd.nextToken("")));
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Value:  ", "Foreground Color:  ", "Background Color:  ", "Outline Color:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, FgColorSwatchConfig.class, BgColorSwatchConfig.class, OutlineColorSwatchConfig.class};
  }

  public String[] getAttributeNames() {
    return new String[] { VALUE, FG_COLOR, BG_COLOR, OUTLINE_COLOR };
  }

  public void setAttribute(String key, Object o) {

    if (VALUE.equals(key)) {
      this.val = (String) o;
    }
    else if (FG_COLOR.equals(key)) {
      if (o instanceof String) {
        o = new ColorSwatch((String) o);
      }
      fgColor = (ColorSwatch) o;
    }
    else if (BG_COLOR.equals(key)) {
      if (o instanceof String) {
        o = new ColorSwatch((String) o);
      }
      bgColor = (ColorSwatch) o;
    }
    else if (OUTLINE_COLOR.equals(key)) {
      if (o instanceof String) {
        o = new ColorSwatch((String) o);
      }
      outlineColor = (ColorSwatch) o;
    }
    if (myConfig != null) {
      myConfig.rebuildViz();
    }

  }

  public String getAttributeValueString(String key) {
    if (VALUE.equals(key)) {
      return val;
    }
    else if (FG_COLOR.equals(key)) {
      return fgColor.encode();
    }
    else if (BG_COLOR.equals(key)) {
      return bgColor.encode();
    }
    else if (OUTLINE_COLOR.equals(key)) {
      return outlineColor.encode();
    }
    else
      return null;
  }

  public String formatName(String name) {
    FormattedString s = ((TextItem) getItem()).getNameFormat();
    s.setProperty(TextItem.PIECE_NAME, name);
    s.setProperty(TextItem.LABEL, val);
    return s.getText();
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (VALUE.equals(name)) {
      return valueCond;
    }
    else if (OUTLINE_COLOR.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return isOutline();
        }};
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public int getKeyCommandCount() {
    TextItem item = (TextItem) getItem();
    int count = 0;

    if (item.isChangeable() && !isLocked()) {
      count++;
    }
    if (item.isLockable() && !isLocked()) {
      count++;
    }
    return count;
  }

  public KeyCommand[] getKeyCommands(GamePiece target) {

    TextItem item = (TextItem) getItem();

    KeyCommand[] commands = new KeyCommand[getKeyCommandCount()];
    int count = 0;
    if (item.isChangeable() && !isLocked()) {
      commands[count++] = new KeyCommand(item.changeCmd, item.changeKey, target);
    }
    if (item.isLockable() && !isLocked()) {
      commands[count++] = new KeyCommand(item.lockCmd, item.lockKey, target);
    }

    return commands;
  }

  public void keyEvent(KeyStroke stroke) {
    if (isLocked()) {
      return;
    }

    TextItem item = (TextItem) getItem();
    Command command = new NullCommand();

    if (item.lockKey != null && stroke.equals(item.lockKey)) {
      locked = !locked;
    }

    if (item.changeKey != null && stroke.equals(item.changeKey)) {
      String s = (String) JOptionPane.showInputDialog(null, item.changeCmd, null, JOptionPane.QUESTION_MESSAGE, null,
          null, val);
      if (s != null) {
        val = s;
      }
    }
    return;
  }

  private VisibilityCondition valueCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return !((TextItem) getItem()).isFixed();
    }
  };

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ItemInstance) c).getBgColor());
    }
  }

  public static class FgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ItemInstance) c).getFgColor());
    }
  }

  public static class OutlineColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((TextItemInstance) c).getOutlineColor());
    }
  }

  public Object getProperty(Object key) {
     String k = (String) key;
     String propertyName = k.substring(AutoImage.PROPERTY_PREFIX.length());
     if (propertyName.equals(name)) {
       return getValue();
     }
     return null;
  }


}
