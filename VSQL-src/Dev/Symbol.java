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
 
package Dev;

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

  protected static final String NATO_SIZE_SET = "NATO Size Symbols";
  protected static final String NATO_UNIT_SET = "NATO Unit Symbols";
  protected static final String[] SYMBOL_SETS = new String[] { NATO_SIZE_SET, NATO_UNIT_SET };
  
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
  
  public Symbol(String setName, String name) {
    symbolSetName = setName;
    symbolName = name;
  }
  
  public void draw(Graphics g, Rectangle bounds, Color fg, Color bg, float lineWidth) {
    
    if (bg != null) {
      g.setColor(bg);
      g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
    }
    
    if (symbolSetName.equals(NATO_UNIT_SET)) {
      NatoUnitSymbolSet.draw(symbolName, g, bounds, fg, lineWidth);
    }
  }
  

  public static class NatoUnitSymbolSet {
    protected static final String INFANTRY = "Infantry";
    protected static final String CAVALRY = "Cavalry/Recon";
    protected static final String ARTILLERY = "Artillery";
    protected static final String ENGINEERS = "Engineers";
    
    protected static String[] getSymbolNames() {
      return new String[] {
          INFANTRY,
          CAVALRY,
          ARTILLERY,
          ENGINEERS
      };
    }
    
    protected static void draw(String name, Graphics g, Rectangle bounds, Color fg, float lineWidth) {
      
      g.setColor(fg);
      BasicStroke stroke = new BasicStroke(lineWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
      Graphics2D g2 = ((Graphics2D) g);
      g2.setStroke(stroke);
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);

      
      if (name.equals(INFANTRY)) {
        g.drawRect(bounds.x, bounds.y, bounds.width, bounds.height);
        g.drawLine(bounds.x, bounds.y, bounds.x+bounds.width, bounds.y+bounds.height);
        g.drawLine(bounds.x, bounds.y+bounds.height, bounds.x+bounds.width, bounds.y);
      }
      else if (name.equals(CAVALRY)) {
        g.drawRect(bounds.x, bounds.y, bounds.width, bounds.height);
        g.drawLine(bounds.x, bounds.y+bounds.height, bounds.x+bounds.width, bounds.y);
      }
      else if (name.equals(ARTILLERY)) {
        int p40 = bounds.height * 2 / 5;
        int p20 = p40 / 2;
        g.drawRect(bounds.x, bounds.y, bounds.width, bounds.height);
        g.fillOval(bounds.x+p40, bounds.y+p40, p40, p40);
      }
      else if (name.equals(ENGINEERS)) {
        
      }
    }
    
  }
}
