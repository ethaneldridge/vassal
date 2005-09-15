package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.util.Enumeration;

/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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

/**
 * Adds a menu item that applies a {@link GlobalCommand} to other pieces
 */
public class CounterGlobalKeyCommand extends Decorator implements EditablePiece {
  public static final String ID = "globalkey;";
  protected KeyCommand[] command;
  protected String commandName;
  protected KeyStroke key;
  protected KeyStroke globalKey;
  protected GlobalCommand globalCommand = new GlobalCommand();
  protected String propertiesFilter;
  protected boolean allMaps;

  public CounterGlobalKeyCommand() {
    this(ID + "Global;G", null);
  }

  public CounterGlobalKeyCommand(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken();
    key = st.nextKeyStroke('G');
    globalKey = st.nextKeyStroke('K');
    propertiesFilter = st.nextToken("");
    allMaps = st.nextBoolean(false);
    globalCommand.setReportSingle(st.nextBoolean(true));
    globalCommand.setReportFormat(st.nextToken(""));
    command = null;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName)
        .append(key)
        .append(globalKey)
        .append(propertiesFilter)
        .append(allMaps)
        .append(globalCommand.isReportSingle())
        .append(globalCommand.getReportFormat());
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      if (commandName.length() > 0 && key != null) {
        command = new KeyCommand[]{new KeyCommand(commandName, key, Decorator.getOutermost(this))};
      }
      else {
        command = new KeyCommand[0];
      }
    }
    if (command.length > 0) {
      command[0].setEnabled(allMaps || getMap() != null);
    }
    return command;
  }

  public String myGetState() {
    return "";
  }

  public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    myGetKeyCommands();
    if (command[0].matches(stroke)) {
      apply();
    }
    return c;
  }

  public void mySetState(String newState) {
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

  public Shape getShape() {
    return piece.getShape();
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public String getDescription() {
    return "Global Key Command";
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void apply() {
    PieceFilter filter = PropertiesPieceFilter.parse(new FormattedString(propertiesFilter).getText(this));
    Command c = null;
    if (allMaps) {
      for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
        Map m = (Map) e.nextElement();
        c = globalCommand.apply(m, filter);
      }
    }
    else if (getMap() != null) {
      c = globalCommand.apply(getMap(), filter);
    }
    GameModule.getGameModule().sendAndLog(c);
  }

  public static class Ed implements PieceEditor {
    protected StringConfigurer nameInput;
    protected HotKeyConfigurer keyInput;
    protected HotKeyConfigurer globalKey;
    protected StringConfigurer propertyMatch;
    protected BooleanConfigurer suppress;
    protected BooleanConfigurer allMaps;
    protected FormattedStringConfigurer reportFormat;
    protected JPanel controls;

    public Ed(CounterGlobalKeyCommand p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      nameInput = new StringConfigurer(null, "Command name:  ", p.commandName);
      controls.add(nameInput.getControls());

      keyInput = new HotKeyConfigurer(null, "Keyboard Command:  ", p.key);
      controls.add(keyInput.getControls());

      globalKey = new HotKeyConfigurer(null, "Global Key Command:  ", p.globalKey);
      controls.add(globalKey.getControls());

      propertyMatch = new StringConfigurer(null, "Matching Properties:  ", p.propertiesFilter);
      controls.add(propertyMatch.getControls());

      allMaps = new BooleanConfigurer(null, "Apply to all maps:  ", p.allMaps);
      controls.add(allMaps.getControls());

      suppress = new BooleanConfigurer(null, "Suppress individual reports?", p.globalCommand.isReportSingle());
      controls.add(suppress.getControls());

      reportFormat = new FormattedStringConfigurer(null, "Report Format: ");
      controls.add(reportFormat.getControls());
    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameInput.getValueString()).append((KeyStroke) keyInput.getValue()).append(
          (KeyStroke) globalKey.getValue()).append(propertyMatch.getValueString()).append(allMaps.getValueString())
          .append(suppress.booleanValue().booleanValue()).append(reportFormat.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }

}
