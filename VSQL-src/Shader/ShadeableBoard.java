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

import VASSAL.build.module.map.boardPicker.Board;

public class ShadeableBoard extends Board {

  public static final String SHADEABLE = "shadeable";
  protected boolean shadeable = true;
  
  public ShadeableBoard() {
    super();
  }
  
  public boolean isShadeable() {
    return shadeable;
  }
  
  public String[] getAttributeDescriptions() {
    String a[] = super.getAttributeDescriptions();
    String b[] = new String[] { "Shadeable" };
    String c[] = new String[a.length + b.length];
    System.arraycopy(a, 0, c, 0, a.length);
    System.arraycopy(b, 0, c, a.length, b.length);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = super.getAttributeTypes();
    Class b[] = new Class[] { Boolean.class };
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(a, 0, c, 0, a.length);
    System.arraycopy(b, 0, c, a.length, b.length);
    return c;
  }

  public String[] getAttributeNames() {
    String a[] = super.getAttributeNames();
    String b[] = new String[] { SHADEABLE };
    String c[] = new String[a.length + b.length];
    System.arraycopy(a, 0, c, 0, a.length);
    System.arraycopy(b, 0, c, a.length, b.length);
    return c;
  }

  public void setAttribute(String key, Object value) {

    if (SHADEABLE.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      shadeable = ((Boolean) value).booleanValue();
    }
    else {
      super.setAttribute(key, value);
    }

  }

  public String getAttributeValueString(String key) {
    if (SHADEABLE.equals(key)) {
      return shadeable + "";
    }
    else
      return super.getAttributeValueString(key);
  }
  
  public static String getConfigureTypeName() {
    return "Shadeable Board";
  }
}
