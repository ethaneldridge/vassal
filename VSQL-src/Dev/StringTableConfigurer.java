/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package Dev;

import java.awt.Component;

import VASSAL.configure.Configurer;

/**
 * A Configurer that returns multiple arrays of Strings
 */
public class StringTableConfigurer extends Configurer {

  protected Component controls = null;
  /**
   * @param key
   * @param name
   * @param val
   */
  public StringTableConfigurer(String key, String name, Object val) {
    super(key, name, val);
    // TODO Auto-generated constructor stub
  }

  /* (non-Javadoc)
   * @see VASSAL.configure.Configurer#getValueString()
   */
  public String getValueString() {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see VASSAL.configure.Configurer#setValue(java.lang.String)
   */
  public void setValue(String s) {
    // TODO Auto-generated method stub
    
  }

  /* (non-Javadoc)
   * @see VASSAL.configure.Configurer#getControls()
   */
  public Component getControls() {
    // TODO Auto-generated method stub
    return controls;
  }
  


}
