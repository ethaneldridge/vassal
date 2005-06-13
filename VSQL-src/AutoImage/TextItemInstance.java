/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
 
package AutoImage;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.SequenceEncoder;

public class TextItemInstance extends ItemInstance {

  protected static final String VALUE = "value";
  
  protected String val = "";
  private boolean locked = false;
  
  public TextItemInstance() {
    super();
  }
  
  public TextItemInstance(String nam, String typ, String loc, String val) {
    super(nam, typ, loc);
    setValue(val);
   }
  
  public TextItemInstance(String code, ImageDefn defn) {
    super(defn);
    decode(code);
  }
  
  public void setValue(String value) {
    this.val = value;
    updateState();
  }

  public String getValue() {
    return val;
  }

  public void setLocked(boolean locked) {
    this.locked = locked;
    updateState();
  }

  public boolean isLocked() {
    return locked;
  }

  public void updateState() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(locked);
    se.append(val);
    setState(se.getValue());
  }
  
  public String encode() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(getType());
    se.append(getName());
    se.append(getLocation());
    se.append(getFgColor().encode());
    se.append(getBgColor().encode());
    se.append(getValue());
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
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Value:  ", "Foreground Color:  ", "Background Color:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, FgColorSwatchConfig.class, BgColorSwatchConfig.class, };
  }
  
  public String[] getAttributeNames() {
    return new String[] { VALUE, FG_COLOR, BG_COLOR };
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
      return ! ((TextItem) getMyItem()).isFixed();
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

}
