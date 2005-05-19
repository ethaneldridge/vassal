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
package Dev;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.Properties;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;

/**
 * 
 * The base portion of a Counter Layout component.
 */

public abstract class Item extends AbstractConfigurable {

  protected static final String NAME = "name";
  protected static final String LOCATION = "location";
  protected static final String BG_COLOR = "bgColor";
  protected static final String FG_COLOR = "fgColor";
  protected static final String ADVANCED = "advanced";
  protected static final String ROTATION = "rotation";
  protected static final String X_OFFSET = "xoffset";
  protected static final String Y_OFFSET = "yoffset";
  protected static final String BORDER_COLOR = "borderColor";
  protected static final String BORDER_WIDTH = "borderWidth";

  protected String type;
  String location = CounterLayout.CENTER;
  protected int xoffset, yoffset;
  double borderWidth = 0.0f;
  protected ColorSwatch bgColor = ColorSwatch.getClear();
  protected ColorSwatch fgColor = ColorSwatch.getBlack();
  protected ColorSwatch borderColor = ColorSwatch.getBlack();
  protected boolean advanced = false;
  protected int rotation = 0;

  protected CounterLayout layout;

  public Item() {
    super();
    setConfigureName("");
  }

  public Item(String name) {
    this();
    setConfigureName(name);
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Location:  ", "Color:  ", "Advanced Options", "Background Color:  ",
        "X Offset:  ", "Y Offset:  ", "Rotation (Degrees):  ", "Border Width:  ", "Border Color:  " };

  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, LocationConfig.class, FgColorConfig.class, Boolean.class, BgColorConfig.class,
        Integer.class, Integer.class, Integer.class, Double.class, BorderColorConfig.class };
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

  public static class BgColorConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((Item) c).bgColor);
    }
  }

  public static class FgColorConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((Item) c).fgColor);
    }
  }

  public static class BorderColorConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((Item) c).borderColor);
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, LOCATION, FG_COLOR, ADVANCED, BG_COLOR, X_OFFSET, Y_OFFSET, ROTATION, BORDER_WIDTH,
        BORDER_COLOR };
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
    else if (BG_COLOR.equals(key)) {
      if (o instanceof String) {
        o = GenericsContainer.getColorSwatch((String) o);
      }
      bgColor = (ColorSwatch) o;
    }
    else if (FG_COLOR.equals(key)) {
      if (o instanceof String) {
        o = GenericsContainer.getColorSwatch((String) o);
      }
      fgColor = (ColorSwatch) o;
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
    else if (BORDER_WIDTH.equals(key)) {
      if (o instanceof String) {
        o = new Double(Double.parseDouble((String) o));
      }
      borderWidth = ((Double) o).doubleValue();
    }
    else if (BORDER_COLOR.equals(key)) {
      if (o instanceof String) {
        o = GenericsContainer.getColorSwatch((String) o);
      }
      borderColor = (ColorSwatch) o;
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
    else if (BG_COLOR.equals(key)) {
      return bgColor.getConfigureName();
    }
    else if (FG_COLOR.equals(key)) {
      return fgColor.getConfigureName();
    }
    else if (ADVANCED.equals(key)) {
      return advanced + "";
    }
    else if (ROTATION.equals(key)) {
      return rotation + "";
    }
    else if (BORDER_WIDTH.equals(key)) {
      return borderWidth + "";
    }
    else if (BORDER_COLOR.equals(key)) {
      return borderColor.getConfigureName();
    }
    else
      return null;
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
    layout = (CounterLayout) parent;
  }

  private VisibilityCondition borderCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return advanced && (borderWidth > 0.0f);
    }
  };

  private VisibilityCondition advancedCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return advanced;
    }
  };

  public VisibilityCondition getAttributeVisibility(String name) {
    if (BORDER_COLOR.equals(name)) {
      return borderCond;
    }
    else if (BORDER_WIDTH.equals(name) || BG_COLOR.equals(name) || ROTATION.equals(name) || X_OFFSET.equals(name)
        || Y_OFFSET.equals(name)) {
      return advancedCond;
    }
    else {
      return null;
    }
  }

  /**
   * Implement by subclass to draw itself.
   */
  public abstract void draw(Graphics g, Properties p);

  public String getLocation() {
    return location;
  }

  public int getXoffset() {
    return xoffset;
  }

  public int getYoffset() {
    return yoffset;
  }

  public String getBgColorName() {
    return bgColor.getConfigureName();
  }

  public Color getBgColor() {
    return bgColor.getColor();
  }
  
  public String getFgColorName() {
    return fgColor.getConfigureName();
  }
  
  public Color getFgColor() {
    return fgColor.getColor();
  }

  public int getRotation() {
    return rotation;
  }

  public double getborderWidth() {
    return borderWidth;
  }

  public String getBorderColorName() {
    return borderColor.getConfigureName();
  }
  
  protected Point getOrigin() {
    Point p = CounterLayout.getPosition(getLocation(), layout);
    p.translate(getXoffset(), getYoffset());
    return p;
  }
  
  protected CounterLayout getLayout() {
    return layout;
  }
}
