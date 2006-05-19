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
 
package avl;

import Inventory.Inventory;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Stack;

/**
 * Specialised Counter Inventory for AVL
 */
public class AvlInventory extends Inventory {

  protected static final String MAIN_MAP_NAME = "Main";
  protected static final String GRAVEYARD_MAP_NAME = "Graveyard";
  protected static final String VP_MARKER = "VP";
    
  public AvlInventory() {
    super();
  }
  
  protected void generateInventory() {

    int german_terrain = getCount(MAIN_MAP_NAME, new String[] { "Type", "Control_Name" }, new String[] { "Control", "German" }, VP_MARKER);
    int soviet_terrain = getCount(MAIN_MAP_NAME, new String[] { "Type", "Control_Name" }, new String[] { "Control", "Soviet" }, VP_MARKER);
    int german_graveyard = getCount(GRAVEYARD_MAP_NAME, new String[] {"Nation" }, new String[] { "Soviet" }, VP_MARKER);
    int soviet_graveyard = getCount(GRAVEYARD_MAP_NAME, new String[] {"Nation" }, new String[] { "German" }, VP_MARKER);
    
    int german_total = german_terrain + german_graveyard;
    int soviet_total = soviet_terrain + soviet_graveyard; 
      
    String[] result = new String[3];
    result[0] = getConfigureName() + ":";
    result[1] = "German: " + german_total + 
    	" (Cities: " + german_terrain + 
    	", Eliminated: " + german_graveyard + ")";
    result[2] = "Soviet: " + soviet_total +
    	" (Cities: " + soviet_terrain + 
    	", Eliminated: " + soviet_graveyard + ")";
    	
      
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
        new AvlSelector(mapName, matchNames, matchValues));

    while (pi.hasMoreElements()) {

      GamePiece p = pi.nextPiece();
      
      String vp = countPropertyName == null ? null : (String) p.getProperty(countPropertyName);
      
      if (vp != null) {
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
  
  protected class AvlSelector implements PieceFilter {

    protected String mapName;
    protected String[] matchNames;
    protected String[] matchValues;

    public AvlSelector(String mapName, String[] matchNames, String[] matchValues) {
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
