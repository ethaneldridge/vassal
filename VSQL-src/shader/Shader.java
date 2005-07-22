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
 
package shader;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;


public class Shader extends AbstractConfigurable {

  public static final String NAME = "name";

  public static final String MARKER_NAME = "markerName";
  public static final String MARKER_VALUE = "markerValue";
  public static final String SETTING = "setting";
  public static final String RANGE_TYPE = "rangeType";
  public static final String RANGE = "range";
  
  public static final String RANGE_MARKER = "Use Marker Value";
  public static final String RANGE_FIXED = "Fixed";
  public static final String RANGE_USER = "Set by User";
  public static final String SETTING_MARKER = "Use Marker Value";  
  
  public static final String ON = "On";
  public static final String OFF = "Off";
  
  protected String marker = "";
  protected String markerValue = "";
  protected String rangeType = RANGE_FIXED;
  protected int range = 3;
  protected String setting = ON;
  
  public Shader() {
    super();
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Shader Name:  ", "Select Counters with Marker Name:  ", 
                                            "and Value:  ", "Shading:  ", "Range Source:  ", "Range:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, String.class, SettingConfig.class, RangeConfig.class, Integer.class };
  }

  public static class RangeConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ RANGE_FIXED, RANGE_USER, RANGE_MARKER };
    }
  }
  
  public static class SettingConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ ON, OFF, SETTING_MARKER };
    }
  }
  
  public String[] getAttributeNames() {
    return new String[] { NAME, MARKER_NAME, MARKER_VALUE, SETTING, RANGE_TYPE, RANGE };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (MARKER_NAME.equals(key)) {
      marker = (String) value;
    }
    else if (MARKER_VALUE.equals(key)) {
      markerValue = (String) value;
    }
    else if (SETTING.equals(key)) {
      setting = (String) value;
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
    else if (MARKER_NAME.equals(key)) {
      return marker;
    }
    else if (MARKER_VALUE.equals(key)) {
      return markerValue;
    }
    else if (SETTING.equals(key)) {
      return setting;
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
    else {
      return super.getAttributeVisibility(name);
    }
  }
  
  public static String getConfigureTypeName() {
    return "Shader";
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