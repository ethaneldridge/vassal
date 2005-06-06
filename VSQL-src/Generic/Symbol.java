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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;


/**
 *
 */

public class Symbol {

  protected static final String NATO = "NATO Unit Symbols";
  protected static final String[] SYMBOL_SETS = new String[] { NATO };
  
//  public static void draw(Graphics g, Rectangle r, Color fg, Color bg, String symbolSet, String symbolName) {
//    
//    if (symbolSet.equals(NATO_SIZE_SET)) {
//      
//    }
//    else if (symbolSet.equals(NATO_UNIT_SET)) {
//      NatoUnitSymbolSet.draw(g, r, fg, bg, symbolName);
//    }
//    
//  }
  
  protected String symbolSetName;
  protected String symbolName;
  protected String symbolSize;
  
  public Symbol(String setName, String name, String size) {
    symbolSetName = setName;
    symbolName = name;
    symbolSize = size;
  }
  
  public void draw(Graphics g, Rectangle bounds, Color fg, Color bg, float lineWidth) {
    
    if (bg != null) {
      g.setColor(bg);
      g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
    }
    
    if (symbolSetName.equals(NATO)) {
      NatoUnitSymbolSet.draw(symbolName, g, bounds, fg, lineWidth, symbolSize);
    }
  }
  

  public static class NatoUnitSymbolSet {

    protected static final String SZ_NONE = "None";
    protected static final String SZ_PLATOON = "Platoon";
    protected static final String SZ_COMPANY = "Company";
    
    protected static final String NONE = "None";
    protected static final String INFANTRY = "Infantry";
    protected static final String CAVALRY = "Cavalry/Recon";
    protected static final String ARTILLERY = "Artillery";
    protected static final String ENGINEERS = "Engineers";
    
    protected static String[] getSymbolNames() {
      return new String[] {
          NONE,
          INFANTRY,
          CAVALRY,
          ARTILLERY,
          ENGINEERS
      };
    }
    
    protected static String[] getSymbolSizes() {
      return new String[] {
          SZ_NONE,
          SZ_PLATOON,
          SZ_COMPANY
      };
    }
    
    protected static void draw(String name, Graphics g, Rectangle bounds, Color fg, float lineWidth, String size) {
      
      g.setColor(fg);
      BasicStroke stroke = new BasicStroke(lineWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
      Graphics2D g2 = ((Graphics2D) g);
      g2.setStroke(stroke);
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);

      if (name.equals(NONE)) {
        
      }
      else if (name.equals(INFANTRY)) {
        g.drawRect(bounds.x, bounds.y, bounds.width, bounds.height);
        g.drawLine(bounds.x, bounds.y, bounds.x+bounds.width, bounds.y+bounds.height);
        g.drawLine(bounds.x, bounds.y+bounds.height, bounds.x+bounds.width, bounds.y);
      }
      else if (name.equals(CAVALRY)) {
        g.drawRect(bounds.x, bounds.y, bounds.width, bounds.height);
        g.drawLine(bounds.x, bounds.y+bounds.height, bounds.x+bounds.width, bounds.y);
      }
      else if (name.equals(ARTILLERY)) {
        int radius = bounds.height  / 5;
        int x1 = bounds.width / 2 - radius + 1;
        int y1 = bounds.height / 2 - radius;
        g.drawRect(bounds.x, bounds.y, bounds.width, bounds.height);
        g.fillOval(bounds.x+x1, bounds.y+y1, radius*2, radius*2);
      }
      else if (name.equals(ENGINEERS)) {
        
      }
    }
    
  }
}
