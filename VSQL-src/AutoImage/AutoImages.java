package AutoImage;

import java.awt.Color;

import org.w3c.dom.Element;

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
 * Actual definition is in inner class {@link VASSAL.build.module.CounterLayout}
 */
public class AutoImages extends AbstractConfigurable {
  
  protected static AutoImages instance;
  protected LayoutsContainer definitions;
  protected ColorManager colors;
  protected FontManager fonts;
  
  protected static final Color DEFAULT_COLOR  = Color.WHITE;
  
  public AutoImages() {
    instance = this;
  }
  
  public static AutoImages getInstance() {
    return instance;
  }
  
  public void build(Element e) {
    super.build(e);

    if (colors == null) { 
      addChild(new ColorManager());
      colors.build(null);
    }
    if (fonts == null) { 
      addChild(new FontManager());
      fonts.build(null);
    }
    if (definitions == null) addChild(new LayoutsContainer());

  }
  
  private void addChild(Buildable b) {
    add(b);
    b.addTo(this);
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
        LayoutsContainer.class, 
        ColorManager.class,
        FontManager.class};
  }

  public static String getConfigureTypeName() {
    return "Generic Images";
  }

  public void add(Buildable b) {
    super.add(b);
    if (b instanceof LayoutsContainer) {
      definitions = (LayoutsContainer) b;
    }
    else if (b instanceof ColorManager) {
      colors = (ColorManager) b;
    }
    else if (b instanceof FontManager) {
      fonts = (FontManager) b;
    }    
  }

  public void remove(Buildable b) {
    super.remove(b);
    if (b instanceof LayoutsContainer) {
      definitions = null;
    }
    else if (b instanceof ColorManager) {
      colors = null;
    }
    else if (b instanceof FontManager) {
      fonts = null;
    }  
  }
  
  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
  }

  public ImageDefn getGenericDefn(String defnName) {
    
    return definitions.getGenericDefn(defnName);
    
  }
  
//  public static CounterLayout getDefinitionByName(String name) {
//    
//    CounterLayout def = null;
//    
//    if (instance.definitions != null) {
//        def = (CounterLayout) instance.definitions.getDefinition(name);
//    }
//    
//    if (def == null) {
//      def = new CounterLayout();
//    }
//    
//    return def;
//  }


//  public static FontStyle getFontStyleByName(String name) {
//    
//    FontStyle def = null;
//    
//    if (instance.fonts != null) {
//        def = (FontStyle) instance.fonts.getFontStyle(name);
//    }
//    
//    if (def == null) {
//      def = new FontStyle();
//    }
//    
//    return def;
//  }
//  
  
//  public static FontStyle getStyledFont(String fontName) {
//    if (instance.fonts != null) {
//      return instance.fonts.getFontStyle(fontName);
//    }
//    else
//      return null;
//  }
//  
//  public static String[] getFontNames() {
//    return instance.fonts.getFontNames();
//  }
}
