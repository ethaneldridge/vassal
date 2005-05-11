package Dev;

import java.awt.Color;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;
import java.util.HashMap;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.SingleChildInstance;

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

/**
 * Container for definitions of Generic Counter Definitions.
 * Actual definition is in inner class {@link VASSAL.build.module.GenericDefinition}
 */
public class GenericsContainer extends AbstractConfigurable {
  
  protected static GenericsContainer instance;
  protected HashMap definitions = new HashMap();
  protected HashMap colors = new HashMap();

  protected static final Color DEFAULT_COLOR  = Color.WHITE;
  
  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setAttribute(String key, Object value) {
  }

  public Configurer getConfigurer() {
    return null;
  }

  public void addTo(Buildable parent) {
    validator = new SingleChildInstance(GameModule.getGameModule(),getClass());
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{GenericDefinition.class, GenericColor.class};
  }

  public static String getConfigureTypeName() {
    return "Generic Counter Definitions";
  }

  public void add(Buildable b) {
    super.add(b);
    if (b instanceof GenericDefinition) {
      GenericDefinition def = (GenericDefinition) b;
      definitions.put(def.getConfigureName(), def);
      def.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
            definitions.remove(evt.getOldValue());
            definitions.put(evt.getNewValue(), evt.getSource());
          }
        }
      });
    }
    else if (b instanceof GenericColor) {
      GenericColor def = (GenericColor) b;
        colors.put(def.getConfigureName(), def);
        def.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
              colors.remove(evt.getOldValue());
              colors.put(evt.getNewValue(), evt.getSource());
            }
          }
        });
    }
  }

  public void remove(Buildable b) {
    super.remove(b);
    if (b instanceof GenericDefinition) {
      definitions.remove(((GenericDefinition) b).getConfigureName()); 
    }
    else if (b instanceof GenericColor) {
      colors.remove(((GenericColor) b).getConfigureName());
    }
  }
  
  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
  }

  public static GenericDefinition getDefinition(String name) {
    
    GenericDefinition def = null;
    
    setInstance();
    if (instance != null) {
        def = (GenericDefinition) instance.definitions.get(name);
    }
    
    if (def == null) {
      def = new GenericDefinition();
    }
    
    return def;
  }
  
  public static void setInstance() {
    if (instance == null) {
      Enumeration e = GameModule.getGameModule().getComponents(GenericsContainer.class);
      if (e.hasMoreElements()) {
        instance = (GenericsContainer) e.nextElement();
      }
    }
  }
  
  
  /**
   * Find a named color swatch
   */
  
  public static Color getColorByName (String colorName) {
    setInstance();
    if (instance != null) {
      GenericColor gcolor = (GenericColor) instance.colors.get(colorName);
      if (gcolor != null) {
        Color color = gcolor.getColor();
        if (color != null) {
          return color;
        }
      }
    }
    return DEFAULT_COLOR;
  }
}
