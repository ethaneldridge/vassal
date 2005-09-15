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
 
package tdc;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;

import VASSAL.counters.ColoredBorder;
import VASSAL.counters.GamePiece;

/**
 * @author Brent Easton
 *
 * Change border color depending on Command Status
 */
public class TdcHighlighter extends ColoredBorder {

  public TdcHighlighter() {
    super();
  }
  
  /**
   * Change Color to Red if unit is not in command.
   */
  public void draw(GamePiece p, Graphics g, int x, int y, Component obs, double zoom) {
    
    if (inCommand(p)) {
      super.draw(p, g, x, y, obs, zoom);
    }
    else {
      Color oldColor = getColor();
      int oldThickness = getThickness();
      setColor(Color.red);
      setThickness(3);
      super.draw(p, g, x, y, obs, zoom);
      setColor(oldColor);
      setThickness(oldThickness);
    }
  }
  
  public boolean inCommand(GamePiece p) {
    
    String unitClass = (String) p.getProperty("Class");
    if (unitClass != null) {
      if (unitClass.equals("Infantry") || unitClass.equals("Vehicle") || unitClass.equals("Gun")) {
        String formation = (String) p.getProperty("Formation");
        if (formation != null) {
          
        }
      }
    }
    return false;
  }
}

