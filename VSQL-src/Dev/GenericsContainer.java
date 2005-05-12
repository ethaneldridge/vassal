package Dev;

import java.awt.Color;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
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
  protected GenericDefinitionsContainer definitions;
  protected ColorSwatchsContainer colors;
  protected FontStylesContainer fonts;
  
  protected static final Color DEFAULT_COLOR  = Color.WHITE;
  
  public GenericsContainer() {
    instance = this;
    add(new ColorSwatchsContainer());
    colors.addTo(this);
    add(new FontStylesContainer());
    fonts.addTo(this);
    add(new GenericDefinitionsContainer());
    definitions.addTo(this);
  }
  
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
    return new Class[] {
        GenericDefinitionsContainer.class, 
        ColorSwatchsContainer.class,
        FontStylesContainer.class};
  }

  public static String getConfigureTypeName() {
    return "Generic Counter Definitions";
  }

  public void add(Buildable b) {
    super.add(b);
    if (b instanceof GenericDefinitionsContainer) {
      definitions = (GenericDefinitionsContainer) b;
    }
    else if (b instanceof ColorSwatchsContainer) {
      colors = (ColorSwatchsContainer) b;
    }
    else if (b instanceof FontStylesContainer) {
      fonts = (FontStylesContainer) b;
    }    
  }

  public void remove(Buildable b) {
    super.remove(b);
    if (b instanceof GenericDefinitionsContainer) {
      definitions = null;
    }
    else if (b instanceof ColorSwatchsContainer) {
      colors = null;
    }
    else if (b instanceof FontStylesContainer) {
      fonts = null;
    }  
  }
  
  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
  }

  public static GenericDefinition getDefinitionByName(String name) {
    
    GenericDefinition def = null;
    
    if (instance.definitions != null) {
        def = (GenericDefinition) instance.definitions.getDefinition(name);
    }
    
    if (def == null) {
      def = new GenericDefinition();
    }
    
    return def;
  }


  public static FontStyle getFontStyleByName(String name) {
    
    FontStyle def = null;
    
    if (instance.fonts != null) {
        def = (FontStyle) instance.fonts.getFontStyle(name);
    }
    
    if (def == null) {
      def = new FontStyle();
    }
    
    return def;
  }
  
  public static Color getColorByName (String colorName) {
    if (instance.colors != null) {
      Color color = (Color) instance.colors.getColorByName(colorName);
      if (color != null) {
          return color;
      }
    }
    return DEFAULT_COLOR;
  }
}
