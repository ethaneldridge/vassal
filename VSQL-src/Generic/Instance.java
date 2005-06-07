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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;

public abstract class Instance extends AbstractConfigurable {
  
  protected String name;
  protected String type;
  protected String location;
  protected ImageDefn defn;
  
  public Instance(String nam, String typ, String loc) {
    setName(nam);
    setType(typ);
    setLocation(loc);
  }

  public Instance() {
    this("", "", ""); 
  }
 
  public Instance(ImageDefn defn) {
    this();
    this.defn = defn;
  }
  
  public abstract String encode();

  public static Instance newDefaultInstance(String name, String type, String location) {
     
    if (type.equals(SymbolItem.TYPE)) {
      return new SymbolInstance(name, type, location, "", "Infantry", "");
    }
    else if (type.equals(TextItem.TYPE)) {
      return new TextInstance(name, type, location, "Xx");
    }
    return null;
  }
  
  public void setName(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public void setType(String type) {
    this.type = type;
  }

  public String getType() {
    return type;
  }

  public void setLocation(String location) {
    this.location = location;
  }

  public String getLocation() {
    return location;
  }
  
  public String[] getAttributeDescriptions() {
    return null;
  }

  public Class[] getAttributeTypes() {
    return null;
  }

  public String[] getAttributeNames() {
    return null;
  }

  public void setAttribute(String key, Object value) {

  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void removeFrom(Buildable parent) {
  
  }
  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return null;
  }

  public void addTo(Buildable parent) {
    if (parent instanceof ImageDefn) {
      defn = (ImageDefn) parent;
    }
  }
}
