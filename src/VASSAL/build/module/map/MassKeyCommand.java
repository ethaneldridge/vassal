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
 * Date: Sep 25, 2002
 * Time: 10:43:11 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module.map;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.Chatter;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.*;
import VASSAL.counters.*;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;

/** Adds a button to a map window toolbar.  Hitting the button applies a particular key command to all pieces
 * on that map with a given name.
 */
public class MassKeyCommand extends AbstractConfigurable implements PieceVisitor {
  public static final String DEPRECATED_NAME = "text";
  public static final String NAME = "name";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String HOTKEY = "buttonHotkey";
  public static final String KEY_COMMAND = "hotkey";
  public static final String AFFECTED_PIECE_NAMES = "names";
  public static final String REPORT_SINGLE = "reportSingle";
  public static final String REPORT_FORMAT = "reportFormat";
  public static final String CONDITION = "condition";
  private static final String IF_ACTIVE = "If layer is active";
  private static final String IF_INACTIVE = "If layer is inactive";
  private static final String ALWAYS = "Always";
  public static final String CHECK_PROPERTY = "property";
  public static final String CHECK_VALUE = "propValue";

  private LaunchButton launch;
  private KeyStroke stroke = KeyStroke.getKeyStroke(0, 0);
  private String[] names = new String[0];
  private String condition = ALWAYS;
  protected String checkProperty;
  protected String checkValue;
  protected PieceVisitorDispatcher dispatcher = new PieceVisitorDispatcher(this);
  private Map map;
  private Command keyCommand;
  private BoundsTracker tracker;
  protected boolean reportSingle;
  protected static boolean suppressTraitReports = false;
  protected FormattedString reportFormat = new FormattedString("");

  public MassKeyCommand() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        apply();
      }
    };
    launch = new LaunchButton("CTRL", BUTTON_TEXT, HOTKEY, ICON, al);
  }

  public void addTo(Buildable parent) {
    map = (Map) parent;
    map.getToolBar().add(launch);
  }

  public void apply() {

    suppressTraitReports = reportSingle;

    reportFormat.setProperty(GlobalOptions.COMMAND_NAME, getConfigureName());
    reportFormat.setProperty(GlobalOptions.PLAYER_ID, GlobalOptions.getPlayerId());
    String reportText = reportFormat.getText();
    if (reportText.length() > 0) {
      keyCommand = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "*" + reportText);
      keyCommand.execute();
    }
    else {
      keyCommand = new NullCommand();
    }
    tracker = new BoundsTracker();
    GamePiece[] p = map.getPieces();
    for (int i = 0; i < p.length; ++i) {
      dispatcher.accept(p[i]);
    }
    tracker.repaint();
    GameModule.getGameModule().sendAndLog(keyCommand);

    suppressTraitReports = false;
  }

  /* We don't treat {@link Deck}s any differently than {@link Stack}s, so
  no need to implement {@link DeckVisitor */
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
      BasicPiece.setInitialState(p);
      c.append(p.keyEvent(stroke));
      tracker.addPiece(p);
    }
  }

  /**
   *
   * @param target
   * @return true if the KeyCommand should be applied to the <code>target</code> GamePiece
   */
  protected boolean isValidTarget(GamePiece target) {
    boolean valid = false;
    if (isAffected(target)) {
      if (ALWAYS.equals(condition)) {
        valid = true;
      }
      else if (IF_ACTIVE.equals(condition)) {
        valid = Embellishment.getLayerWithMatchingActivateCommand(target, stroke, true) != null;
      }
      else if (IF_INACTIVE.equals(condition)) {
        valid = Embellishment.getLayerWithMatchingActivateCommand(target, stroke, false) != null;
      }
    }
    return valid;
  }

  /**
   * Return true if the name of the argument GamePiece is in the list of target piece names
   */
  protected boolean isAffected(GamePiece target) {
    boolean affected = false;
    if (names != null) {
      for (int j = 0; j < names.length; ++j) {
        if (Decorator.getInnermost(target).getName().equals(names[j])) {
          affected = true;
          break;
        }
      }
    }
    else if (checkValue != null) {
      affected = checkValue.equals(target.getProperty(checkProperty));
    }
    return affected;
  }

  /**
   * Return true if a Global Command is currently in action
   */
  public static boolean suppressTraitReporting() {
    return suppressTraitReports;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Description", "Key Command", "Apply to pieces whose property", "is equal to this value", "Apply command", "Button text", "Button Icon", "Hotkey",
                        "Report as single Command", "Report Format"};
  }

  public String[] getAttributeNames() {
    return new String[]{NAME, KEY_COMMAND, CHECK_PROPERTY, CHECK_VALUE, CONDITION, BUTTON_TEXT, ICON, HOTKEY,
                        REPORT_SINGLE, REPORT_FORMAT};
  }

  public static class Prompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, IF_ACTIVE, IF_INACTIVE};
    }
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, KeyStroke.class, String.class, String.class, Prompt.class, String.class, IconConfig.class, KeyStroke.class,
                       Boolean.class, ReportFormatConfig.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/keyCommand.gif");
    }
  }

  public static class ReportFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, GlobalOptions.getMassKeyOptions());
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (KEY_COMMAND.equals(key)) {
      return HotKeyConfigurer.encode(stroke);
    }
    else if (AFFECTED_PIECE_NAMES.equals(key)) {
      return StringArrayConfigurer.arrayToString(names);
    }
    else if (CHECK_PROPERTY.equals(key)) {
      return checkProperty;
    }
    else if (CHECK_VALUE.equals(key)) {
      return checkValue;
    }
    else if (CONDITION.equals(key)) {
      return condition;
    }
    else if (REPORT_SINGLE.equals(key)) {
      return reportSingle + "";
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat.getFormat();
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public static String getConfigureTypeName() {
    return "Global Key Command";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#GlobalKeyCommand");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void removeFrom(Buildable parent) {
    map.getToolBar().remove(launch);
  }

  public void setAttribute(String key, Object value) {
    if (DEPRECATED_NAME.equals(key)) {
      setAttribute(NAME, value);
      setAttribute(BUTTON_TEXT, value);
    }
    else if (NAME.equals(key)) {
      setConfigureName((String) value);
      launch.setToolTipText((String) value);
    }
    else if (KEY_COMMAND.equals(key)) {
      if (value instanceof String) {
        value = HotKeyConfigurer.decode((String) value);
      }
      stroke = (KeyStroke) value;
    }
    else if (AFFECTED_PIECE_NAMES.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      names = (String[]) value;
      if (names.length == 0) {
        names = null;
      }
      else {
        checkProperty = null;
        checkValue = null;
      }
    }
    else if (CHECK_PROPERTY.equals(key)) {
      checkProperty = (String) value;
      names = null;
    }
    else if (CHECK_VALUE.equals(key)) {
      checkValue = (String) value;
      names = null;
    }
    else if (CONDITION.equals(key)) {
      condition = (String) value;
    }
    else if (REPORT_SINGLE.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      reportSingle = ((Boolean) value).booleanValue();
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat.setFormat((String) value);
    }
    else {
      launch.setAttribute(key, value);
    }
  }
}
