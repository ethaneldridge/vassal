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
 
package Shader;

import java.awt.Color;
import java.awt.Image;
import java.io.File;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;


public class Shade extends AbstractConfigurable {

  public static final String NAME = "name";

  public static final String TYPE = "type";
  public static final String COLOR = "color";
  public static final String IMAGE = "image";
  public static final String OPACITY = "opacity";
  
  public static final String SETTING = "setting";
  public static final String RANGE_TYPE = "rangeType";
  public static final String RANGE = "range";
  
  public static final String RANGE_MARKER = "Use Marker Value";
  public static final String RANGE_FIXED = "Fixed";
  public static final String RANGE_USER = "Set by User";
  public static final String SETTING_MARKER = "Use Marker Value";  
  
  public static final String TYPE_STD = "Standard";
  public static final String TYPE_IMAGE = "Custom Image";
  public static final String LIGHT = "Light";
  public static final String SHADE = "Shade";
  
  protected String marker = "";
  protected String markerValue = "";
  protected String rangeType = RANGE_FIXED;
  protected int range = 3;
  protected String setting = LIGHT;
  protected String imageName = "";
  protected Color color = Color.BLACK;
  protected String type = TYPE_STD;
  protected int opacity = 100;
  
  public Shade() {
    super();
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Shade Name:  ", "Type:  ", "Shade Image:  ", "Color:  ",
        "Image:  ", "Opacity(%):  ", "Range Source:  ", "Range:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, SettingConfig.class, TypeConfig.class, Color.class,
        Image.class, Integer.class, RangeConfig.class, Integer.class };
  }

  public static class TypeConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { TYPE_STD, TYPE_IMAGE };
    }
  }
  
  public static class RangeConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ RANGE_FIXED, RANGE_USER, RANGE_MARKER };
    }
  }
  
  public static class SettingConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ LIGHT, SHADE };
    }
  }
  
  public String[] getAttributeNames() {
    return new String[] { NAME, SETTING, TYPE, COLOR, IMAGE, OPACITY, RANGE_TYPE, RANGE };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (SETTING.equals(key)) {
      setting = (String) value;
    }
    else if (TYPE.equals(key)) {
      type = (String) value;
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      color = (Color) value;
    }
    else if (IMAGE.equals(key)) {
      if (value instanceof File) {
        value = ((File) value).getName();
      }
      imageName = (String) value;
    }
    else if (OPACITY.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      opacity = ((Integer) value).intValue();
      if (opacity < 0 || opacity > 100) {
        opacity = 100;
      }
    }
    else if (RANGE_TYPE.equals(key)) {
      rangeType = (String) value;
    }
    else if (RANGE.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      range = ((Integer) value).intValue();
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (SETTING.equals(key)) {
      return setting;
    }
    else if (TYPE.equals(key)) {
      return type + "";
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else if (IMAGE.equals(key)) {
      return imageName + "";
    }
    else if (OPACITY.equals(key)) {
      return opacity + "";
    }
    else if (RANGE_TYPE.equals(key)) {
      return rangeType;
    }
    else if (RANGE.equals(key)) {
      return range + "";
    }
    else {
      return null;
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (RANGE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !rangeType.equals(RANGE_MARKER);
        }
      };
    }  
    else if (TYPE.equals(name) || OPACITY.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return setting.equals(SHADE);
        }
      };
    }
    else if (IMAGE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return setting.equals(SHADE) && type.equals(TYPE_IMAGE);
        }
      };
    }
    else if (COLOR.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return setting.equals(SHADE) && type.equals(TYPE_STD);
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }
  
  public static String getConfigureTypeName() {
    return "Shade";
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
  
}