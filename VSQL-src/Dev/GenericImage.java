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

import org.w3c.dom.Element;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;

/**
 * A class that creates an image from a Counter Layout and supplied property data
 */
public class GenericImage extends AbstractConfigurable {
  
  protected static final String NAME = "name";
  protected static final String LAYOUT = "layout";
  
  protected String layoutName;
  
  public GenericImage() {
    super();
    name = "";
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Layout" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class };
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, LAYOUT };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (LAYOUT.equals(key)) {
      layoutName = (String) value;
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (LAYOUT.equals(key)) {
      return layoutName;
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
  }

  public void add(Buildable b) {
  }

  public void remove(Buildable b) {
  }

  public void build(Element e) {
    super.build(e);
  }

}