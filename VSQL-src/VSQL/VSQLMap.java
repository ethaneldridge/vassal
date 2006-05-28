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
 
package VSQL;

import java.util.Enumeration;

import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.LOS_Thread;

public class VSQLMap extends Map {

  protected LOS_Thread los = null;
  protected VSQLHidePiecesButton hide = null;
  
  public VSQLMap() {
    super();
  }
  
  public boolean isLOSactivated() {
    findLOSThread();
    if (los != null) {
      return los.isVisible();
    }
    return false;
  }

  protected void findLOSThread() {
    if (los == null) {
      Enumeration e = getBuildComponents();
      while (e.hasMoreElements()) {
        Buildable c = (Buildable) e.nextElement();
        if (c instanceof LOS_Thread) {
          los = (LOS_Thread) c;
        }
      }
    }
  }
  
  public void hidePieces() {
    findHidePiecesButton();
    if (hide != null) {
      hide.setPiecesVisible(false);
    }
  }
  
  public void showPieces() {
    findHidePiecesButton();
    if (hide != null) {
      hide.setPiecesVisible(true);
    }
  }
  
  protected void findHidePiecesButton() {
    if (hide == null) {
      Enumeration e = getBuildComponents();
      while (e.hasMoreElements()) {
        Buildable c = (Buildable) e.nextElement();
        if (c instanceof VSQLHidePiecesButton) {
          hide = (VSQLHidePiecesButton) c;
        }
      }
    }
  }
}
