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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Sep 3, 2002
 * Time: 9:44:57 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module;

import VASSAL.build.*;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Hideable;
import VASSAL.counters.Obscurable;
import VASSAL.tools.FormattedString;
import VASSAL.Info;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

public class GlobalOptions extends AbstractConfigurable {
  public static final String NON_OWNER_UNMASKABLE = "nonOwnerUnmaskable";
  public static final String PROMPT_STRING = "promptString";
  public static final String CENTER_ON_MOVE = "centerOnMove";
  public static final String MARK_MOVED = "markMoved";
  public static final String AUTO_REPORT = "autoReport";
  public static final String ALWAYS = "Always";
  public static final String NEVER = "Never";
  public static final String PROMPT = "Use Preferences Setting";
  public static final String SINGLE_WINDOW = "singleWindow";
  public static final String SCALER_ALGORITHM = "scalerAlgorithm";
  
  public static final String PLAYER_NAME = "playerName";
  public static final String PLAYER_SIDE = "playerSide";
  public static final String PLAYER_ID = "playerId";
  public static final String GRID_REF = "gridRef";
  public static final String MAP_NAME = "mapName";
  public static final String MAP_REF = "mapRef";
  public static final String FROM_MAP_REF = "fromMapRef";
  public static final String TO_MAP_REF = "toMapRef";
  public static final String UNIT_NAME = "unitName";
  public static final String OLD_UNIT_NAME = "oldUnitName";
  public static final String NEW_UNIT_NAME = "newUnitName";
  public static final String INITIAL_NAME = "initialName";
  public static final String INITIAL_LOCATION = "initialLoc";
  public static final String INITIAL_INVISIBILITY = "initialVis";
  public static final String COMMAND_NAME = "commandName";
  public static final String DECK_NAME = "deckName";
  
  public static final String PLAYER_ID_FMT_1 = "pidfmt1";
  public static final String PLAYER_ID_FMT_2 = "pidfmt2";
  public static final String MAP_REF_FMT_1   = "mapfmt1";
  public static final String MAP_REF_FMT_2   = "mapfmt2";
  public static final String MOVE_FMT        = "movefmt";
  public static final String CREATE_FMT      = "createfmt";

  private String promptString = "Opponents can unmask my pieces";
  private String nonOwnerUnmaskable = NEVER;
  private String centerOnMoves = ALWAYS;
  private String autoReport = NEVER;
  private String markMoved = NEVER;
  
  private static String playerIdFmt1 = "<$playerName$($playerSide$)>";
  private static String playerIdFmt2 = "<$playerName$>";
  private static String mapRefFmt1   = "$gridRef$[$mapName$]";
  private static String mapRefFmt2   = "$gridRef$";
  private static String moveFmt      = "$unitName$ moves $fromMapRef$ -> $toMapRef$ *";
  private static String createFmt    = "$unitName$ created in $toMapRef$";
    
  private static GlobalOptions instance;
  private boolean useSingleWindow;
	private boolean scalerAlgorithm;

  public void addTo(Buildable parent) {
    if (GameModule.getGameModule().getComponents(GlobalOptions.class).hasMoreElements()) {
      throw new IllegalBuildException("Only one Global Options allowed");
    }
    instance = this;

    BooleanConfigurer config = new BooleanConfigurer(SINGLE_WINDOW, "Use combined application window (requires restart)",Boolean.TRUE);
    GameModule.getGameModule().getPrefs().addOption(config);
    useSingleWindow = !Boolean.FALSE.equals(config.getValue());

		config = new BooleanConfigurer(SCALER_ALGORITHM, "Smooth image scaling",Boolean.TRUE);
		GameModule.getGameModule().getPrefs().addOption(config);
		scalerAlgorithm = !Boolean.FALSE.equals(config.getValue());
  }

  public static GlobalOptions getInstance() {
    if (instance == null) {
      instance = new GlobalOptions();
    }
    return instance;
  }

  public boolean isUseSingleWindow() {
    return useSingleWindow && Info.is2dEnabled();
  }

  public boolean isAveragedScaling() {
		return scalerAlgorithm;
  }

  public static String getConfigureTypeName() {
    return "Global Options";
  }

  public static class Prompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, NEVER, PROMPT};
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Allow non-owners to un-mask pieces",
                        null,
                        "Center on opponent's moves",
                        "Auto-report moves",
                        "Player Id format with sides selected",
						"Player Id format without sides selected",
						"Map Reference format for named maps",
						"Map Reference format for unnamed maps",
						"Move report format",
						"Create Piece report format"};
  }

  public String[] getAttributeNames() {
    return new String[]{NON_OWNER_UNMASKABLE, PROMPT_STRING, CENTER_ON_MOVE, AUTO_REPORT, 
    	PLAYER_ID_FMT_1, PLAYER_ID_FMT_2, MAP_REF_FMT_1, MAP_REF_FMT_2, MOVE_FMT, CREATE_FMT };
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Prompt.class, null, Prompt.class, Prompt.class, 
    	FormattedString1.class, FormattedString1.class, 
    	FormattedString2.class, FormattedString2.class, 
		FormattedString3.class, FormattedString3.class};
  }

  public String getAttributeValueString(String key) {
    if (NON_OWNER_UNMASKABLE.equals(key)) {
      return nonOwnerUnmaskable;
    }
    else if (PROMPT_STRING.equals(key)) {
      return promptString;
    }
    else if (CENTER_ON_MOVE.equals(key)) {
      return centerOnMoves;
    }
    else if (AUTO_REPORT.equals(key)) {
      return autoReport;
    }
    else if (MARK_MOVED.equals(key)) {
      return markMoved;
    }
	else if (PLAYER_ID_FMT_1.equals(key)) {
	  return playerIdFmt1;
	}
	else if (PLAYER_ID_FMT_2.equals(key)) {
	  return playerIdFmt2;
	}
	else if (MAP_REF_FMT_1.equals(key)) {
	  return mapRefFmt1;
	}
	else if (MAP_REF_FMT_2.equals(key)) {
	  return mapRefFmt2;
	}
	else if (MOVE_FMT.equals(key)) {
	  return moveFmt;
	}
	else if (CREATE_FMT.equals(key)) {
	  return createFmt;
	}
    else {
      return null;
    }
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GameModule.htm"), "#GlobalOptions");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void removeFrom(Buildable parent) {
  }

  public void setAttribute(String key, Object value) {
    if (NON_OWNER_UNMASKABLE.equals(key)) {
      nonOwnerUnmaskable = (String) value;
      if (ALWAYS.equals(nonOwnerUnmaskable)) {
        ObscurableOptions.getInstance().allowAll();
      }
      else if (NEVER.equals(nonOwnerUnmaskable)) {
        ObscurableOptions.getInstance().allowNone();
      }
      else if (PROMPT.equals(nonOwnerUnmaskable)) {
        ObscurableOptions.getInstance().allowSome(promptString);
        GameModule.getGameModule().getGameState().addGameComponent(ObscurableOptions.getInstance());
        GameModule.getGameModule().addCommandEncoder(ObscurableOptions.getInstance());
      }
    }
    else if (PROMPT_STRING.equals(key)) {
      promptString = (String) value;
      ObscurableOptions.getInstance().setPrompt(promptString);
    }
    else if (CENTER_ON_MOVE.equals(key)) {
      centerOnMoves = (String) value;
      if (PROMPT.equals(centerOnMoves)) {
        BooleanConfigurer config = new BooleanConfigurer(CENTER_ON_MOVE, "Center on opponent's moves");
        GameModule.getGameModule().getPrefs().addOption(config);
      }
    }
    else if (AUTO_REPORT.equals(key)) {
      autoReport = (String) value;
      if (PROMPT.equals(autoReport)) {
        BooleanConfigurer config = new BooleanConfigurer(AUTO_REPORT, "Auto-report moves");
        GameModule.getGameModule().getPrefs().addOption(config);
      }
    }
    else if (MARK_MOVED.equals(key)) {
      markMoved = (String) value;
      if (PROMPT.equals(markMoved)) {
        BooleanConfigurer config = new BooleanConfigurer(MARK_MOVED, "Mark moved pieces");
        GameModule.getGameModule().getPrefs().addOption(config);
      }
    }
	else if (PLAYER_ID_FMT_1.equals(key)) {
		playerIdFmt1 = (String) value;
	}
	else if (PLAYER_ID_FMT_2.equals(key)) {
		playerIdFmt2 = (String) value;
	}
	else if (MAP_REF_FMT_1.equals(key)) {
		mapRefFmt1 = (String) value;
	}
	else if (MAP_REF_FMT_2.equals(key)) {
		mapRefFmt2  = (String) value;
	}
	else if (MOVE_FMT.equals(key)) {
		moveFmt = (String) value;
	}
	else if (CREATE_FMT.equals(key)) {
		createFmt = (String) value;
	}	
  }

  public boolean autoReportEnabled() {
    return isEnabled(autoReport, AUTO_REPORT);
  }

  public boolean centerOnOpponentsMove() {
    return isEnabled(centerOnMoves, CENTER_ON_MOVE);
  }

  public boolean isMarkMoveEnabled() {
    return isEnabled(markMoved, MARK_MOVED);
  }

  private boolean isEnabled(String attValue, String prefsPrompt) {
    if (ALWAYS.equals(attValue)) {
      return true;
    }
    else if (NEVER.equals(attValue)) {
      return false;
    }
    else {
      return Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(prefsPrompt));
    }
  }
  
  public static String getPlayerId() {
  	
  	String id = "", playerName = "", playerSide = "", formatString;
  	
	playerName = (String) GameModule.getGameModule().getPrefs().getOption(GameModule.REAL_NAME).getValue();
	 	
	if (PlayerRoster.isActive() && PlayerRoster.getMySide() != null) {
	    playerSide = PlayerRoster.getMySide();
	}
	
	if (playerSide.length() > 0) {
	    formatString = playerIdFmt1;			
	}
	else {
		formatString = playerIdFmt2;
	}
	
	FormattedString fmt = new FormattedString(formatString);
	fmt.setProperty(PLAYER_NAME, playerName);
	fmt.setProperty(PLAYER_SIDE, playerSide);
	id = fmt.getText();
	
	return id;  	
  }
  
  public static String formatLocationId(String gridRef, String mapId) {
	String id = "", formatString;
 	
	if (mapId.length() > 0) {
		formatString = mapRefFmt1;			
	}
	else {
		formatString = mapRefFmt2;
	}
	
	FormattedString fmt = new FormattedString(formatString);
	fmt.setProperty(GRID_REF, gridRef);
	fmt.setProperty(MAP_NAME, mapId);
	id = fmt.getText();
	
	return id; 
  }
  
  public static String formatChat(String chatText) {
	String id = "";
	FormattedString fmt = new FormattedString("$playerId$ - " + chatText);
	fmt.setProperty(PLAYER_ID, getPlayerId());
	id = fmt.getText();
	return id;	
  }

  public static String formatMove(String unitName, String from, String to) {
	String id = "";
	FormattedString fmt = new FormattedString(moveFmt);
	fmt.setProperty(PLAYER_ID, getPlayerId());
	fmt.setProperty(UNIT_NAME, unitName);
	fmt.setProperty(FROM_MAP_REF, from);
	fmt.setProperty(TO_MAP_REF, to);
	fmt.setProperty(MAP_REF, to);
	id = fmt.getText();
	return id;	
  }  
  
  public static String formatCreate(String unitName, String loc) {
	String id = "";
	FormattedString fmt = new FormattedString(createFmt);
	fmt.setProperty(PLAYER_ID, getPlayerId());
	fmt.setProperty(UNIT_NAME, unitName);
	fmt.setProperty(FROM_MAP_REF, "Off Map");
	fmt.setProperty(TO_MAP_REF, loc);
	fmt.setProperty(MAP_REF, loc);
	id = fmt.getText();
	return id;	
  }
  
  public static void setInitialState (GamePiece outer) {
  	Map map = outer.getMap();
  	if (map != null) {
  		outer.setProperty(INITIAL_LOCATION, map.locationName(outer.getPosition()));
		outer.setProperty(INITIAL_INVISIBILITY, outer.getProperty(VASSAL.counters.Properties.INVISIBLE_TO_OTHERS));
		Hideable.setAllHidden(true);
		Obscurable.setAllHidden(true);
		outer.setProperty(INITIAL_NAME, outer.getName());
		Hideable.setAllHidden(false);
		Obscurable.setAllHidden(false);
  	}
  }
  
  public static final String[] getIdOptions() {
  	return new String[] {GlobalOptions.PLAYER_NAME,
						 GlobalOptions.PLAYER_SIDE};
  }
  
  public static final String[] getMapOptions() {
  	return new String[] {GlobalOptions.GRID_REF, 
						 GlobalOptions.MAP_NAME};
  }						 
		
  public static final String[] getMoveOptions() {
	  return new String[] {GlobalOptions.PLAYER_ID, 
						   GlobalOptions.UNIT_NAME,
						   GlobalOptions.MAP_REF,
					  	   GlobalOptions.FROM_MAP_REF,
						   GlobalOptions.TO_MAP_REF};
  }

  public static final String[] getDeckOptions() {
	  return new String[] {GlobalOptions.PLAYER_ID,
						   GlobalOptions.DECK_NAME, 
						   GlobalOptions.COMMAND_NAME};
  }
  
  public static final String[] getMassKeyOptions() {
	  return new String[] {GlobalOptions.PLAYER_ID, 
						   GlobalOptions.COMMAND_NAME};
  }
  
  public static final String[] getTraitOptions() {
	  return new String[] {GlobalOptions.PLAYER_ID,
						   GlobalOptions.UNIT_NAME,
						   GlobalOptions.OLD_UNIT_NAME, 
						   GlobalOptions.NEW_UNIT_NAME,  
						   GlobalOptions.MAP_REF,
						   GlobalOptions.FROM_MAP_REF, 
						   GlobalOptions.TO_MAP_REF, 						   
						   GlobalOptions.COMMAND_NAME};
  }						   
    
  public static class FormattedString1 implements ConfigurerFactory {
	  public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
		  return new FormattedStringConfigurer(key, name, getIdOptions());
	  }
  }
  
  public static class FormattedString2 implements ConfigurerFactory {
	  public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
		return new FormattedStringConfigurer(key,name, getMapOptions());
	  }
  }
  
  public static class FormattedString3 implements ConfigurerFactory {
	  public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
		return new FormattedStringConfigurer(key,name, getMoveOptions());
	  }
  }
}
