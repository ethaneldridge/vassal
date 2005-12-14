/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney
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
package VASSAL.build.module.map;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.Map;
import VASSAL.counters.ColoredBorder;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.SingleChildInstance;

import java.awt.*;

public class HighlightLastMoved extends AbstractConfigurable {
  public static final String COLOR = "color";
  public static final String THICKNESS = "thickness";
  private ColoredBorder highlighter = new ColoredBorder();

  public String[] getAttributeDescriptions() {
    return new String[]{"Color", "Thickness"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Color.class, Integer.class};
  }

  public String[] getAttributeNames() {
    return new String[]{COLOR, THICKNESS};
  }

  public void setAttribute(String key, Object value) {
    if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      highlighter.setColor((Color) value);
    }
    else if (THICKNESS.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      highlighter.setThickness(((Integer) value).intValue());
    }
  }

  public String getAttributeValueString(String key) {
    if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(highlighter.getColor());
    }
    else if (THICKNESS.equals(key)) {
      return String.valueOf(highlighter.getThickness());
    }
    else {
      return null;
    }
  }

  public void addTo(Buildable parent) {
    ((Map) parent).setLastMovedHighlighter(highlighter);
    validator = new SingleChildInstance((Map) parent, getClass());
  }

  public void removeFrom(Buildable parent) {
    ((Map) parent).setLastMovedHighlighter(null);
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public static String getConfigureTypeName() {
    return "Last Move Highlighter";
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }
}
