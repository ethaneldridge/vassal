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

public class Symbol {

  protected static final String NATO = "NATO Unit Symbols";
  protected static final String[] SYMBOL_SETS = new String[] { NATO };

  //  public static void draw(Graphics g, Rectangle r, Color fg, Color bg, String
  // symbolSet, String symbolName) {
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
  protected String symbolName1;
  protected String symbolName2;
  protected String symbolSize;

  public Symbol(String setName, String name1, String name2, String size) {
    symbolSetName = setName;
    symbolName1 = name1;
    symbolName2 = name2;
    symbolSize = size;
  }

  public void draw(Graphics g, Rectangle bounds, Color fg, Color bg, float lineWidth) {

    if (symbolSetName.equals(NATO)) {
      NatoUnitSymbolSet.draw(symbolName1, symbolName2, g, bounds, fg, bg, lineWidth, symbolSize);
    }
  }

  public static class NatoUnitSymbolSet {

    protected static final String SZ_NONE = "None";
    protected static final String SZ_TEAM = "Team";
    protected static final String SZ_SQUAD = "Squad";
    protected static final String SZ_SECTION = "Section";
    protected static final String SZ_PLATOON = "Platoon";
    protected static final String SZ_ECHELON = "Echelon";
    protected static final String SZ_COMPANY = "Company";
    protected static final String SZ_BATTALION = "Battalion";
    protected static final String SZ_REGIMENT = "Regiment";
    protected static final String SZ_BRIGADE = "Brigade";
    protected static final String SZ_DIVISION = "Division";
    protected static final String SZ_CORPS = "Corps";
    protected static final String SZ_ARMY = "Army";
    protected static final String SZ_ARMY_GROUP = "Army Group";
    protected static final String SZ_REGION = "Region";

    protected static final String NONE = "None";
    protected static final String AIRBORNE = "Airborne";
    protected static final String AIR_DEFENCE = "Air Defence";
    //    protected static final String AIR_FORCE = "Air Force";
    //    protected static final String AIR_MOBILE = "Air Mobile";
    //    protected static final String AMPHIBIOUS = "Amphibious";
    protected static final String ANTI_TANK = "Anti Tank";
    protected static final String ARMORED = "Armored";
    //    protected static final String ARMY_AVIATION = "Army Aviation";
    protected static final String ARTILLERY = "Artillery";
    //    protected static final String BRIDGING = "Bridging";
    //    protected static final String COMBAT_SERVICE_SUPPORT = "";
    //    protected static final String ELECTRONIC_RANGING = "";
    //    protected static final String ELECTRONIC_WARFARE = "";
    protected static final String ENGINEERS = "Engineers";
    //    protected static final String HEADQUARTERS_SUPPORT = "";
    protected static final String INFANTRY = "Infantry";
    //    protected static final String LABOR_RESOURCES = "";
    //    protected static final String MAINTENANCE = "";
    protected static final String MARINES = "Marines";
    //    protected static final String METEOROLOGICAL = "";
    //    protected static final String MILITARY_CIVIL = "";
    //    protected static final String MP = "";
    //    protected static final String MISSILE = "";
    protected static final String MOUNTAIN = "Mountain";
    //    protected static final String NAVY = "";
    //    protected static final String NBC = "";
    //    protected static final String ORDNANCE = "";
    //    protected static final String PARACHUTE = "";
    //    protected static final String PAY_FINANCE = "";
    //    protected static final String PERSONNEL = "";
    //    protected static final String PIPELINE = "";
    //    protected static final String POSTAL = "";
    //    protected static final String PSYCH = "";
    //    protected static final String QUARTERMASTER = "";
    protected static final String RECON = "Cavalry/Recon";

    //    protected static final String REPLACEMENT = "";
    //    protected static final String SERVICE = "";
    //    protected static final String SIGNAL = "";
    //    protected static final String SOUND_RANGING = "";
    //    protected static final String SUPPLY = "";
    //    protected static final String TRANSPORT = "";
    //    protected static final String TOPO = "";
    //    protected static final String UNMANNED_AIR = "";
    //    protected static final String VET = "";

    protected static String[] getSymbolNames() {
      return new String[] { 
          NONE, 
          INFANTRY, 
          RECON,
          ARMORED, 
          ARTILLERY, 
          ENGINEERS, 
          AIRBORNE, 
          AIR_DEFENCE, 
          ANTI_TANK, 
          MARINES,
          MOUNTAIN 
          };
    }

    protected static String[] getSymbolSizes() {
      return new String[] { 
          SZ_NONE, 
          SZ_TEAM,
          SZ_SQUAD, 
          SZ_SECTION, 
          SZ_PLATOON, 
          SZ_ECHELON,
          SZ_COMPANY, 
          SZ_BATTALION, 
          SZ_REGIMENT,
          SZ_BRIGADE, 
          SZ_DIVISION, 
          SZ_CORPS, 
          SZ_ARMY, 
          SZ_ARMY_GROUP, 
          SZ_REGION };
    }

    protected static void draw(String name1, String name2, Graphics g, Rectangle bounds, Color fg, Color bg,
        float lineWidth, String size) {

      if (bg != null) {
        g.setColor(bg);
        g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
      }

      g.setColor(fg);
      BasicStroke stroke = new BasicStroke(lineWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
      Graphics2D g2 = ((Graphics2D) g);
      g2.setStroke(stroke);
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

      g.drawRect(bounds.x, bounds.y, bounds.width, bounds.height);

      drawSize(g, size, bounds);
      draw(g, name1, bounds);
      draw(g, name2, bounds);
    }

    protected static void draw(Graphics g, String name, Rectangle bounds) {

      Graphics2D g2 = (Graphics2D) g;

      int x_left = bounds.x;
      int x_center = bounds.x + bounds.width / 2 + 1;
      int x_right = bounds.x + bounds.width;

      int y_top = bounds.y;
      int y_center = bounds.y + bounds.height / 2;
      int y_bottom = bounds.y + bounds.height;

      if (name.equals(NONE)) {

      }

      else if (name.equals(AIRBORNE)) {
        //        g2.draw(new Arc2D.Double(x_left, y_top,
        //            //bounds.width,
        //            //bounds.height * 1.8,
        //            10,
        //            10,
        //            0, 90,
        //            Arc2D.OPEN));
      }

      else if (name.equals(AIR_DEFENCE)) {

      }

      else if (name.equals(ANTI_TANK)) {
        g.drawLine(x_left, y_bottom, x_center, y_top);
        g.drawLine(x_center, y_top, x_right, y_bottom);
      }

      else if (name.equals(ARMORED)) {

      }

      else if (name.equals(ARTILLERY)) {
        int radius = bounds.height / 5;
        g.fillOval(x_center - radius, y_center - radius, radius * 2, radius * 2);
      }

      else if (name.equals(ENGINEERS)) {

      }

      else if (name.equals(INFANTRY)) {

        g.drawLine(x_left, y_top, x_right, y_bottom);
        g.drawLine(x_left, y_bottom, x_right, y_top);
      }

      else if (name.equals(MARINES)) {

      }

      else if (name.equals(MOUNTAIN)) {

      }

      else if (name.equals(RECON)) {
        g.drawLine(bounds.x, bounds.y + bounds.height, bounds.x + bounds.width, bounds.y);
      }

    }
    
    /**
     * 
     * @param g       Grahics 
     * @param size    Name of size symbol
     * @param bounds  Size of the unit symbol
     */
    protected static void drawSize(Graphics g, String size, Rectangle bounds) {
      
    }

  }

  
}
