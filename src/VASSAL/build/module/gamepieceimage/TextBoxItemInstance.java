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
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.SequenceEncoder;

public class TextBoxItemInstance extends ItemInstance {

  protected static final String VALUE = "value";
  protected static final String BORDER_COLOR = "borderColor";
  
  protected String val = "";
  private ColorSwatch borderColor = ColorSwatch.getBlack();
  
  public TextBoxItemInstance() {
    super();
    setFgColor(ColorSwatch.getClear());
  }

  public TextBoxItemInstance(String code, GamePieceImage defn) {
    super(defn);
    decode(code);
  }

  public TextBoxItemInstance(String name, String type, String location) {
    super(name, type, location);
    setFgColor(ColorSwatch.getClear());
  }

  public void setValue(String value) {
    this.val = value;
  }

  public String getValue() {
    return val;
  }
  
  public String encode() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(getType());
    se.append(getName());
    se.append(getLocation());
    se.append(getFgColor().encode());
    se.append(getBorderColor().encode());
    se.append(getValue());
    return se.getValue();
  }

  public void decode(String code) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ';');
    setType(sd.nextToken(""));
    setName(sd.nextToken(""));
    setLocation(sd.nextToken(""));
    setFgColor(new ColorSwatch(sd.nextToken("")));
    setBorderColor(new ColorSwatch(sd.nextToken("")));
    setValue(sd.nextToken(""));
  }

  protected void setBorderColor(ColorSwatch borderColor) {
    this.borderColor = borderColor;
  }

  protected ColorSwatch getBorderColor() {
    return borderColor;
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Value:  ", "Foreground Color:  ", "Border Color:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, FgColorSwatchConfig.class, BorderColorSwatchConfig.class, };
  }

  public String[] getAttributeNames() {
    return new String[] { VALUE, FG_COLOR, BORDER_COLOR };
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
    else if (BORDER_COLOR.equals(key)) {
      if (o instanceof String) {
        o = new ColorSwatch((String) o);
      }
      setBorderColor((ColorSwatch) o);
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
    else if (BORDER_COLOR.equals(key)) {
      return getBorderColor().encode();
    }
    else
      return null;
  }
  
  public VisibilityCondition getAttributeVisibility(String name) {
    if (VALUE.equals(name)) {
      return valueCond;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }
  
  private VisibilityCondition valueCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return !((TextItem) getItem()).isFixed();
    }
  };
  
  public static class BorderColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ItemInstance) c).getBgColor());
    }
  }

  public static class FgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ItemInstance) c).getFgColor());
    }
  }

}
