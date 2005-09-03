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
 
package wga;

import VASSAL.build.Buildable;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.configure.VisibilityCondition;

/**
 * @author Brent Easton
 *
 */
public class BasicMassKeyCommand extends MassKeyCommand {

  public BasicMassKeyCommand() {
  }
  
  public void addTo(Buildable parent) {
  }

  public void removeFrom(Buildable parent) {
  }
  
  public VisibilityCondition getAttributeVisibility(String name) {
    if (ICON.equals(name) || BUTTON_TEXT.equals(name)|| HOTKEY.equals(name)) {
       return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return false;
        }};
     }
     else {
       return null;
     }
   }
   
}
