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
 
package oyw;

import Inventory.Inventory;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Stack;

/**
 * Specialised Counter Inventory for 100 Years War
 */
public class OywInventory extends Inventory {

  protected static final String MAIN_MAP_NAME = "Main Map";
  protected static final String GRAVEYARD_MAP_NAME = "Graveyard";
  protected static final String VP_MARKER = "VP";
  protected static final String VP_MARKER_2 = "VP_Level";
    
  public OywInventory() {
    super();
  }
  
  protected void generateInventory() {

    int English_terrain = getCount(MAIN_MAP_NAME, new String[] { "Control", "Control_Level" }, new String[] { "true", "1" }, VP_MARKER_2);
    int French_terrain = getCount(MAIN_MAP_NAME, new String[] { "Control", "Control_Level" }, new String[] { "true", "2" }, VP_MARKER_2);
    int English_graveyard = getCount(GRAVEYARD_MAP_NAME, new String[] { "CurrentZone" }, new String[] { "French Eliminated" }, VP_MARKER);
    int French_graveyard = getCount(GRAVEYARD_MAP_NAME, new String[] { "CurrentZone" }, new String[] { "English Eliminated" }, VP_MARKER);
    
    int English_total = English_terrain + English_graveyard;
    int French_total = French_terrain + French_graveyard; 
      
    String[] result = new String[3];
    result[0] = getConfigureName() + ":";
    result[1] = "English: " + English_total + 
    	" (Countries: " + English_terrain + 
    	", Eliminated: " + English_graveyard + ")";
    result[2] = "French: " + French_total +
    	" (Countries: " + French_terrain + 
    	", Eliminated: " + French_graveyard +
    	")";
    	
      
    Command c = new DisplayResults(result, destination);
    c.execute();
    GameModule.getGameModule().sendAndLog(c);
    
  }
  
  /*
   * Count the VPs stored in property countPropertyName on map mapName for counters
   * with properties match matchNames/matchValues.
   */
  protected int getCount(String mapName, String[] matchNames, String[] matchValues, String countPropertyName) {
    int count = 0;
    
    PieceIterator pi = new PieceIterator(GameModule.getGameModule().getGameState().getPieces(), 
        new OywSelector(mapName, matchNames, matchValues));

    while (pi.hasMoreElements()) {

      GamePiece p = pi.nextPiece();
      
      String vp = countPropertyName == null ? null : (String) p.getProperty(countPropertyName);
      
      if (vp == null) {
        count +=1 ;
      }
      else {
        try {
          count += Integer.parseInt(vp);
        } 
        catch (Exception e) {
          count += 1;
        }
      }
    }
    
    return count;
  }
  
  protected class OywSelector implements PieceFilter {

    protected String mapName;
    protected String[] matchNames;
    protected String[] matchValues;

    public OywSelector(String mapName, String[] matchNames, String[] matchValues) {
      this.mapName = mapName;
      this.matchNames = matchNames;
      this.matchValues = matchValues;
    }

    public boolean accept(GamePiece piece) {

      // Ignore Stacks, pieces are reported individually from GameState
      if (piece instanceof Stack) return false;

      // Don't report pieces with no map
      if (piece.getMap() == null) return false;

      if (mapName != null && mapName.length() > 0) {
        String name = piece.getMap().getMapName();
        if (!mapName.equals(piece.getMap().getMapName())) {
          return false;
        }
      }

      // Check for markers
      if (matchNames != null) {
        boolean found = true;
        for (int i = 0; i < matchNames.length; i++) {
          String val = ((String) piece.getProperty(matchNames[i]));
          if (val == null) {
            found = false;
          }
          else {
            val = val.trim();
            String matchValue = matchValues[i];
            if (matchValue != null && !val.equals(matchValue)) {
              found = false;
            }
          }
        }
        return found;
      }

      // Default Accept piece
      return true;
    }

  }
}
