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
 
package G88;

import Inventory.Inventory;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Stack;

/**
 * Specialised Counter Inventory for Gettysburg 1988
 * BME 4-Aug-06 Modify to allow 0.5 VP's
 */
public class G88Inventory extends Inventory {

  protected static final String MAIN_MAP_NAME = "G88 Map";
  protected static final String GRAVEYARD_MAP_NAME = "Graveyard";
  protected static final String VP_MARKER = "VP";
    
  public G88Inventory() {
    super();
  }
  
  protected void generateInventory() {

    double USA_terrain = getCount(MAIN_MAP_NAME, new String[] { "Control", "LAYER_NAME" }, new String[] { "true", "USA" }, VP_MARKER);
    double CSA_terrain = getCount(MAIN_MAP_NAME, new String[] { "Control", "LAYER_NAME" }, new String[] { "true", "CSA" }, VP_MARKER);
    double USA_step = getCount(MAIN_MAP_NAME, new String[] { "LAYER_NAME", "OtherSide" }, new String[] { "[Step]", "USA" }, null);
    double CSA_step = getCount(MAIN_MAP_NAME, new String[] { "LAYER_NAME", "OtherSide" }, new String[] { "[Step]", "CSA" }, null);
    double USA_graveyard = getCount(GRAVEYARD_MAP_NAME, new String[] { "VP", "OtherSide" }, new String[] { null, "USA" }, VP_MARKER);
    double CSA_graveyard = getCount(GRAVEYARD_MAP_NAME, new String[] { "VP", "OtherSide" }, new String[] { null, "CSA" }, VP_MARKER);
    
    double USA_total = USA_terrain + USA_step + USA_graveyard;
    double CSA_total = CSA_terrain + CSA_step + CSA_graveyard; 
      
    String[] result = new String[3];
    result[0] = getConfigureName() + ":";
    result[1] = "USA " + format(USA_total) + 
    	" (Terrain: " + format(USA_terrain) + 
    	", Eliminated: " + format(USA_graveyard) +
    	", Step: " + format(USA_step) +	")";
    result[2] = "CSA: " + format(CSA_total) +
    	" (Terrain: " + format(CSA_terrain) + 
    	", Eliminated: " + format(CSA_graveyard) +
    	", Step: " + format(CSA_step) +
    	")";
    	
      
    Command c = new DisplayResults(result, destination);
    c.execute();
    GameModule.getGameModule().sendAndLog(c);
    
  }
  
  protected String format(double number) {
    String result = Double.toString(number);
    if (result.endsWith(".0")) {
      return result.substring(0, result.length()-2);
    }
    return result;
  }
  /*
   * Count the VPs stored in property countPropertyName on map mapName for counters
   * with properties match matchNames/matchValues.
   */
  protected double getCount(String mapName, String[] matchNames, String[] matchValues, String countPropertyName) {
    double count = 0.0;
    
    PieceIterator pi = new PieceIterator(GameModule.getGameModule().getGameState().getPieces(), 
        new G88Selector(mapName, matchNames, matchValues));

    while (pi.hasMoreElements()) {

      GamePiece p = pi.nextPiece();
      
      String vp = countPropertyName == null ? null : (String) p.getProperty(countPropertyName);
      
      if (vp == null) {
        count +=1 ;
      }
      else {
        try {
          count += Double.parseDouble(vp);
        } 
        catch (Exception e) {
          count += 1;
        }
      }
    }
    
    return count;
  }
  
  protected class G88Selector implements PieceFilter {

    protected String mapName;
    protected String[] matchNames;
    protected String[] matchValues;

    public G88Selector(String mapName, String[] matchNames, String[] matchValues) {
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
