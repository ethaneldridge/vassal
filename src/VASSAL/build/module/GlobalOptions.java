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

import VASSAL.Info;
import VASSAL.tools.FormattedString;
import VASSAL.build.*;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.*;

import java.io.File;
import java.net.MalformedURLException;

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
  public static final String PLAYER_ID_FORMAT="playerIdFormat";

  private String promptString = "Opponents can unmask my pieces";
  private String nonOwnerUnmaskable = NEVER;
  private String centerOnMoves = ALWAYS;
  private String autoReport = NEVER;
  private String markMoved = NEVER;

  private FormattedString playerIdFormat = new FormattedString("$"+PLAYER_NAME+"$");

  private static GlobalOptions instance;
  private boolean useSingleWindow;
  private boolean scalerAlgorithm;

  public void addTo(Buildable parent) {
    instance = this;

    BooleanConfigurer config = new BooleanConfigurer(SINGLE_WINDOW, "Use combined application window (requires restart)", Boolean.TRUE);
    GameModule.getGameModule().getPrefs().addOption(config);
    useSingleWindow = !Boolean.FALSE.equals(config.getValue());

    config = new BooleanConfigurer(SCALER_ALGORITHM, "Smooth image scaling", Boolean.TRUE);
    GameModule.getGameModule().getPrefs().addOption(config);
    scalerAlgorithm = !Boolean.FALSE.equals(config.getValue());

    validator = new SingleChildInstance(GameModule.getGameModule(), getClass());
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

  public static class PlayerIdFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key,name,new String[]{PLAYER_NAME,PLAYER_SIDE});
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
      "Player Id format"};
  }

  public String[] getAttributeNames() {
    return new String[]{NON_OWNER_UNMASKABLE, PROMPT_STRING, CENTER_ON_MOVE, AUTO_REPORT, PLAYER_ID_FORMAT};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Prompt.class, null, Prompt.class, Prompt.class, PlayerIdFormatConfig.class};
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
    else if (PLAYER_ID_FORMAT.equals(key)){
      return playerIdFormat.getFormat();
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
    else if (PLAYER_ID_FORMAT.equals(key)) {
      playerIdFormat.setFormat((String) value);
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

  public String getPlayerId() {
    playerIdFormat.setProperty(PLAYER_NAME, (String) GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME));
    playerIdFormat.setProperty(PLAYER_SIDE,PlayerRoster.getMySide());
    return playerIdFormat.getText();
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
}
