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

package Generic;

import java.awt.Image;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;

/**
 *  
 */
public class ImageDefn extends AbstractConfigurable implements Visualizable {

  protected static final String NAME = "name";
  protected static final String DEFN = "defn";

  protected CounterLayout layout;
  protected ColorScheme scheme;

  public ImageDefn() {
    super();
    setConfigureName("");
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Name",  "" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, DefnConfig.class };
  }

  public static class DefnConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new DefnConfigurer(key, name, ((ImageDefn) c));
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, DEFN };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (DEFN.equals(key)) {
      decodeDefn((String) value);
    }
  }

  private void decodeDefn(String string) {

  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (DEFN.equals(key)) {
      return encodeDefn();
    }
    else
      return null;
  }

  private String encodeDefn() {
   return "";
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
    scheme = (ColorScheme) parent;
    layout = scheme.getLayout();
  }
  
  public ColorScheme getColorScheme() {
    return scheme;
  }

  public int getVisualizerHeight() {
    return layout.getVisualizerHeight();
  }

  public int getVisualizerWidth() {
    return layout.getVisualizerWidth();
  }

  public Image getVisualizerImage() {
    return layout.getVisualizerImage(scheme);
  }

  public Image getVisualizerImage(ImageDefn c) {
    return layout.getVisualizerImage(scheme);
  }
  
  public void rebuildVisualizerImage(ImageDefn c) {
    layout.rebuildVisualizerImage(scheme);
  }
  
  public void rebuildVisualizerImage() {
    layout.rebuildVisualizerImage(scheme);
  }

  public Image getVisualizerImage(ColorScheme scheme) {
    return scheme.getVisualizerImage();
  }

  public void rebuildVisualizerImage(ColorScheme scheme) {
    scheme.rebuildVisualizerImage();
    
  }

 
}
