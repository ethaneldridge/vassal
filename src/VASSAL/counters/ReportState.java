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
 * Date: Oct 2, 2002
 * Time: 6:30:35 AM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.command.Command;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.io.File;
import java.net.MalformedURLException;

/**
 * A GamePiece with this trait will echo the piece's current name when any of a given key commands are pressed
 * (and after they take effect)
 */
public class ReportState extends Decorator implements EditablePiece {
  public static final String ID = "report;";
  private String keys = "";
  private FormattedString format = new FormattedString("$" + LOCATION_NAME + "$: $" + NEW_UNIT_NAME + "$ *");

  public ReportState() {
    this(ID, null);
  }

  public ReportState(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(format.getFormat());
    return ID + keys + se.getValue();
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public Command keyEvent(KeyStroke stroke) {
    GamePiece outer = getOutermost(this);

    // Retrieve the name, location and visibilty of the unit prior to the
    // trait being executed if it is outside this one.


    // The following line will execute the trait if it is inside this one
    Command c = super.keyEvent(stroke);

    GamePiece oldPiece = (GamePiece) getProperty(Properties.SNAPSHOT);

    boolean wasVisible = !Boolean.TRUE.equals(oldPiece.getProperty(Properties.INVISIBLE_TO_OTHERS));
    boolean isVisible = !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS));

    Hideable.setAllHidden(true);
    Obscurable.setAllHidden(true);
    String oldUnitName = oldPiece.getName();
    String newUnitName = outer.getName();
    Hideable.setAllHidden(false);
    Obscurable.setAllHidden(false);

    // Only make a report if:
    //  1. It's not part of a global command with Single Reporting on
    //  2. The piece is visible to all players either before or after the trait
    //     command was executed.

    if (!MassKeyCommand.suppressTraitReporting() && (isVisible || wasVisible)) {

      for (int i = 0; i < keys.length(); ++i) {
        if (stroke.equals(KeyStroke.getKeyStroke(keys.charAt(i), InputEvent.CTRL_MASK))) {

          //
          // Find the Command Name
          //
          String commandName = "";
          KeyCommand[] k = ((Decorator) outer).getKeyCommands();
          for (int j = 0; j < k.length; j++) {
            KeyStroke commandKey = k[j].getKeyStroke();
            if (stroke.equals(commandKey)) {
              commandName = k[j].getName();
            }
          }

          format.setProperty(PLAYER_NAME, (String) GameModule.getGameModule().getPrefs().getOption(GameModule.REAL_NAME).getValue());
          format.setProperty(PLAYER_SIDE, PlayerRoster.getMySide());
          format.setProperty(OLD_UNIT_NAME, oldUnitName);
          format.setProperty(NEW_UNIT_NAME, newUnitName);
          format.setProperty(MAP_NAME, getMap().getConfigureName());
          format.setProperty(LOCATION_NAME, getMap().locationName(getPosition()));
          format.setProperty(COMMAND_NAME, commandName);

          String reportText = format.getText();

          if (reportText.length() > 0) {
            Command display = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + reportText);
            display.execute();
            c = c == null ? display : c.append(display);
          }
          break;
        }
      }
    }

    return c;
  }

  protected String getPieceName() {

    String name = "";

    Hideable.setAllHidden(true);
    Obscurable.setAllHidden(true);

    name = getOutermost(this).getName();

    Hideable.setAllHidden(false);
    Obscurable.setAllHidden(false);

    return name;
  }

  public void mySetState(String newState) {
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getDescription() {
    return "Report Action";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "ReportChanges.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void mySetType(String type) {
    // keys = type.length() <= ID.length() ? "" : type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    if (st.hasMoreTokens()) {
      keys = st.nextToken();
    }
    if (st.hasMoreTokens()) {
      format.setFormat(st.nextToken());
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static final String PLAYER_NAME = "playerName";
  private static final String PLAYER_SIDE = "playerSide";
  private static final String OLD_UNIT_NAME = "oldPieceName";
  private static final String NEW_UNIT_NAME = "newPieceName";
  private static final String MAP_NAME = "mapName";
  private static final String LOCATION_NAME = "location";
  private static final String COMMAND_NAME = "menuCommand";

  // Options for Trait Command Report
  private static final String[] getFormatParameters() {
    return new String[]{PLAYER_NAME,
                        PLAYER_SIDE,
                        COMMAND_NAME,
                        OLD_UNIT_NAME,
                        NEW_UNIT_NAME,
                        MAP_NAME,
                        LOCATION_NAME};
  }

  public static class Ed implements PieceEditor {

    StringConfigurer tf;
    StringConfigurer fmt;
    private JPanel box;

    public Ed(ReportState piece) {

      box = new JPanel();
      box.setLayout(new BoxLayout(box, BoxLayout.Y_AXIS));
      tf = new StringConfigurer(null, "Report when player presses CTRL-", piece.keys);
      fmt = new FormattedStringConfigurer(null, "Report format", getFormatParameters());
      fmt.setValue(piece.format.getFormat());
      box.add(tf.getControls());
      box.add(fmt.getControls());
    }

    public Component getControls() {
      return box;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(tf.getValueString()).append(fmt.getValueString());
      return ID + se.getValue();
    }
  }
}
