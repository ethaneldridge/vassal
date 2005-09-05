package wga;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Enumeration;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.PieceEditor;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceVisitor;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Properties;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.counters.Stack;
import VASSAL.tools.FormattedString;
import VASSAL.tools.PlayerIdFormattedString;
import VASSAL.tools.SequenceEncoder;

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
 * This trait adds a command that creates a duplicate of the selected Gamepiece
 */
public class CounterGlobalKeyCommand extends Decorator implements EditablePiece, PieceVisitor {
  public static final String ID = "globalkey;";
  protected KeyCommand[] command;
  protected String commandName;
  protected KeyStroke key;
  protected KeyStroke globalKey;
  protected String propertiesFilter;
  protected boolean reportSingle;
  protected FormattedString reportFormat = new PlayerIdFormattedString("");

  protected PieceFilter filter;
  protected PieceVisitorDispatcher dispatcher = new PieceVisitorDispatcher(this);
  protected BoundsTracker tracker;
  protected Command keyCommand;

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
    reportSingle = st.nextBoolean(true);
    reportFormat = new FormattedString(st.nextToken(""));
    buildFilter();
    command = null;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName).append(key).append(globalKey).append(propertiesFilter).append(reportSingle).append(reportFormat.getFormat());
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      if (commandName.length() > 0 && key != null) {
        command = new KeyCommand[] { new KeyCommand(commandName, key, Decorator.getOutermost(this)) };
      }
      else {
        command = new KeyCommand[0];
      }
    }
    if (command.length > 0) {
      command[0].setEnabled(getMap() != null);
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
    for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
      Map m = (Map) e.nextElement();
      apply(m);
    }
  }
  
  public void apply(Map m) {
    String mapFormat = m.getChangeFormat();
    if (reportSingle) {
      m.setAttribute(Map.CHANGE_FORMAT, "");
    }
    String reportText = reportFormat.getText();
    if (reportText.length() > 0) {
      keyCommand = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "*" + reportText);
      keyCommand.execute();
    }
    else {
      keyCommand = new NullCommand();
    }
    tracker = new BoundsTracker();
    GamePiece[] p = m.getPieces();
    for (int i = 0; i < p.length; ++i) {
      dispatcher.accept(p[i]);
    }
    tracker.repaint();
    GameModule.getGameModule().sendAndLog(keyCommand);
    if (reportSingle) {
      m.setAttribute(Map.CHANGE_FORMAT, mapFormat);
    }
  }

  public Object visitStack(Stack s) {
    for (Enumeration e = s.getPieces(); e.hasMoreElements();) {
      apply((GamePiece) e.nextElement(), keyCommand, tracker);
    }
    return null;
  }

  public Object visitDefault(GamePiece p) {
    apply(p, keyCommand, tracker);
    return null;
  }

  private void apply(GamePiece p, Command c, BoundsTracker tracker) {
    if (isValidTarget(p)) {
      tracker.addPiece(p);
      p.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(p));
      c.append(p.keyEvent(globalKey));
      tracker.addPiece(p);
    }
  }

  /**
   * 
   * @param target
   * @return true if the KeyCommand should be applied to the <code>target</code>
   *         GamePiece
   */
  protected boolean isValidTarget(GamePiece target) {
    return isAffected(target);
  }

  /**
   * Return true if the name of the argument GamePiece is in the list of target
   * piece names
   */
  protected boolean isAffected(GamePiece target) {
    return filter != null && filter.accept(target);
  }

  private void buildFilter() {
    if (propertiesFilter != null) {
      filter = PropertiesPieceFilter.parse(propertiesFilter);
    }
  }

  public static class Ed implements PieceEditor {
    protected StringConfigurer nameInput;
    protected HotKeyConfigurer keyInput;
    protected HotKeyConfigurer globalKey;
    protected StringConfigurer propertyMatch;
    protected BooleanConfigurer suppress;
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
      
      suppress = new BooleanConfigurer(null, "Suppress individual reports?", new Boolean(p.reportSingle));
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
          (KeyStroke) globalKey.getValue()).append(propertyMatch.getValueString())
          .append(suppress.booleanValue().booleanValue()).append(reportFormat.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }

}
