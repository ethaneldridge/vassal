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
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.*;
import VASSAL.counters.*;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.PlayerIdFormattedString;

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
  public static final String PROPERTIES_FILTER = "filter";
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
  private String condition;
  protected String checkProperty;
  protected String checkValue;
  protected String propertiesFilter;
  protected PieceFilter filter;
  protected PieceVisitorDispatcher dispatcher = new PieceVisitorDispatcher(this);
  private Map map;
  private Command keyCommand;
  private BoundsTracker tracker;
  protected boolean reportSingle;
  protected FormattedString reportFormat = new PlayerIdFormattedString("");

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
    apply(map);
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
      p.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(p));
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
    return isAffected(target);
  }

  /**
   * Return true if the name of the argument GamePiece is in the list of target piece names
   */
  protected boolean isAffected(GamePiece target) {
    return filter != null && filter.accept(target);
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String[] getAttributeDescriptions() {
    if (condition == null) {
      return new String[]{"Description", "Key Command", "Matching properties", "Button text", "Button Icon", "Hotkey",
                          "Suppress individual reports", "Report Format"};
    }
    else {
      return new String[]{"Description", "Key Command", "Matching properties", "Button text", "Button Icon", "Hotkey",
                          "Suppress individual reports", "Report Format", "Apply Command"};

    }
  }

  public String[] getAttributeNames() {
    return new String[]{NAME, KEY_COMMAND, PROPERTIES_FILTER, BUTTON_TEXT, ICON, HOTKEY,
                        REPORT_SINGLE, REPORT_FORMAT, CONDITION, CHECK_VALUE, CHECK_PROPERTY, AFFECTED_PIECE_NAMES};
  }

  public static class Prompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, IF_ACTIVE, IF_INACTIVE};
    }
  }

  public Class[] getAttributeTypes() {
    if (condition == null) {
      return new Class[]{String.class, KeyStroke.class, String.class, String.class, IconConfig.class, KeyStroke.class,
                         Boolean.class, ReportFormatConfig.class};
    }
    else {
      return new Class[]{String.class, KeyStroke.class, String.class, String.class, IconConfig.class, KeyStroke.class,
                         Boolean.class, ReportFormatConfig.class, Prompt.class};
    }
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/keyCommand.gif");
    }
  }

  public static class ReportFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[0]);
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
      return names == null || names.length == 0 ? null : StringArrayConfigurer.arrayToString(names);
    }
    else if (CHECK_PROPERTY.equals(key)) {
      return propertiesFilter != null ? null : checkProperty;
    }
    else if (CHECK_VALUE.equals(key)) {
      return propertiesFilter != null ? null : checkValue;
    }
    else if (PROPERTIES_FILTER.equals(key)) {
      return propertiesFilter;
    }
    else if (CONDITION.equals(key)) {
      return ALWAYS.equals(condition) ? null : condition;
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

  protected LaunchButton getLaunchButton() {
    return launch;
  }

  protected void setLaunchButton(LaunchButton launch) {
    this.launch = launch;
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

  private void buildFilter() {
    if (checkValue != null) {
      propertiesFilter = checkProperty + "=" + checkValue;
    }
    if (propertiesFilter != null) {
      filter = PropertiesPieceFilter.parse(propertiesFilter);
    }
    if (filter != null
        && condition != null) {
      filter = new BooleanAndPieceFilter(filter, new PieceFilter() {
        public boolean accept(GamePiece piece) {
          boolean valid = false;
          if (ALWAYS.equals(condition)) {
            valid = true;
          }
          else if (IF_ACTIVE.equals(condition)) {
            valid = Embellishment.getLayerWithMatchingActivateCommand(piece, stroke, true) != null;
          }
          else if (IF_INACTIVE.equals(condition)) {
            valid = Embellishment.getLayerWithMatchingActivateCommand(piece, stroke, false) != null;
          }
          return valid;
        }
      });
    }
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
        filter = new PieceFilter() {
          public boolean accept(GamePiece piece) {
            for (int j = 0; j < names.length; ++j) {
              if (Decorator.getInnermost(piece).getName().equals(names[j])) {
                return true;
              }
            }
            return false;
          }
        };
      }
    }
    else if (CHECK_PROPERTY.equals(key)) {
      checkProperty = (String) value;
      buildFilter();
    }
    else if (CHECK_VALUE.equals(key)) {
      checkValue = (String) value;
      buildFilter();
    }
    else if (PROPERTIES_FILTER.equals(key)) {
      propertiesFilter = (String) value;
      buildFilter();
    }
    else if (CONDITION.equals(key)) {
      condition = (String) value;
      buildFilter();
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
