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

import java.awt.Graphics;
import java.awt.Point;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.tools.SequenceEncoder;

/**
 * 
 * The base portion of a Counter Layout component.
 */

public abstract class Item extends AbstractConfigurable {

  public static final String TYPE = "";
  
  protected static final String NAME = "name";
  protected static final String LOCATION = "location";
  protected static final String ADVANCED = "advanced";
  protected static final String ROTATION = "rotation";
  protected static final String X_OFFSET = "xoffset";
  protected static final String Y_OFFSET = "yoffset";

  String location = CounterLayout.CENTER;
  protected int xoffset, yoffset;
  protected boolean advanced = false;
  protected int rotation = 0;

  protected CounterLayout layout;

  public Item() {
    super();
    setConfigureName("");
  }
  
  public Item(CounterLayout l) {
    this();
    layout = l;
  }

  public Item(String name) {
    this();
    setConfigureName(name);
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Location:  ", "Advanced Options", 
        "X Offset:  ", "Y Offset:  ", "Rotation (Degrees):  "};

  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, LocationConfig.class,  Boolean.class,
        Integer.class, Integer.class, Integer.class };
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "");
    }
  }

  public static class LocationConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return CounterLayout.LOCATIONS;
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, LOCATION, ADVANCED, X_OFFSET, Y_OFFSET, ROTATION };
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (LOCATION.equals(key)) {
      location = (String) o;
    }
    else if (X_OFFSET.equals(key)) {
      if (o instanceof String) {
        o = new Integer((String) o);
      }
      xoffset = ((Integer) o).intValue();
    }
    else if (Y_OFFSET.equals(key)) {
      if (o instanceof String) {
        o = new Integer((String) o);
      }
      yoffset = ((Integer) o).intValue();
    }
    else if (ADVANCED.equals(key)) {
      if (o instanceof String) {
        o = new Boolean(Boolean.getBoolean((String) o));
      }
      advanced = ((Boolean) o).booleanValue();
    }
    else if (ROTATION.equals(key)) {
      if (o instanceof String) {
        o = new Integer((String) o);
      }
      rotation = ((Integer) o).intValue();
    }

  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (LOCATION.equals(key)) {
      return location + "";
    }
    else if (X_OFFSET.equals(key)) {
      return xoffset + "";
    }
    else if (Y_OFFSET.equals(key)) {
      return yoffset + "";
    }
    else if (ADVANCED.equals(key)) {
      return advanced + "";
    }
    else if (ROTATION.equals(key)) {
      return rotation + "";
    }
    else
      return null;
  }

  public VisibilityCondition getAttributeVisibility(String name) {
   if (ROTATION.equals(name) || X_OFFSET.equals(name) || Y_OFFSET.equals(name)) {
      return advancedCond;
    }
    else {
      return null;
    }
  }
  
  public void removeFrom(Buildable parent) {

  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {

  }

  public int getKeyCommandCount() {
    return 0;
    
  }
  public KeyCommand[] getKeyCommands(GamePiece target) {
    return new KeyCommand[getKeyCommandCount()];
  }
  
  private VisibilityCondition advancedCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return advanced;
    }
  };


  /**
   * Implemented by subclass to draw itself.
   */
  public abstract void draw(Graphics g, ImageDefn defn);
  public abstract String getType();

  public String getLocation() {
    return location;
  }

  public int getXoffset() {
    return xoffset;
  }

  public int getYoffset() {
    return yoffset;
  }

  public int getRotation() {
    return rotation;
  }
  
  protected Point getOrigin() {
    Point p = CounterLayout.getPosition(getLocation(), layout);
    p.translate(getXoffset(), getYoffset());
    return p;
  }
  
  protected CounterLayout getLayout() {
    return layout;
  }
  
  public static Item decode(CounterLayout layout, String s) {
    SequenceEncoder.Decoder sd1= new SequenceEncoder.Decoder(s, '|');
    String t1 = sd1.nextToken();
    String t2 = sd1.nextToken();
    
    Item item;
    
    if (t1.startsWith(SymbolItem.TYPE)) {
      item = SymbolItem.decode(layout, t1);
    }
    else if (t1.startsWith(TextItem.TYPE)) {
      item = TextItem.decode(layout, t1);
    }
    else if (t1.startsWith(ImageItem.TYPE)) {
      item = ImageItem.decode(layout, t1);
    }
    else
      return null;
    
    SequenceEncoder.Decoder sd2 = new SequenceEncoder.Decoder(t2, ';');
    item.setConfigureName(sd2.nextToken());
    item.location = sd2.nextToken();
    item.xoffset = sd2.nextInt(0);
    item.yoffset = sd2.nextInt(0);
    item.rotation = sd2.nextInt(0);
    
    return item;
  }
  
  public String encode() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(getConfigureName());
    se.append(location);
    se.append(xoffset);
    se.append(yoffset);
    se.append(rotation);
    return se.getValue();
  }
  
}
