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
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.Map;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.counters.*;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.Enumeration;
import java.net.URL;
import java.net.MalformedURLException;
import java.io.File;

/** Adds a button to a map window toolbar.  Hitting the button applies a particular key command to all pieces
 * on that map with a given name.
 */
public class MassKeyCommand extends AbstractConfigurable {
  public static final String TEXT = "text";
  public static final String HOTKEY = "hotkey";
  public static final String NAMES = "names";
  public static final String CONDITION = "condition";
  private static final String IF_ACTIVE = "If layer is active";
  private static final String IF_INACTIVE = "If layer is inactive";
  private static final String ALWAYS = "Always";

  private JButton launch = new JButton();
  private KeyStroke stroke = KeyStroke.getKeyStroke(0, 0);
  private String[] names = new String[0];
  private String condition = ALWAYS;
  private Map map;

  public void addTo(Buildable parent) {
    map = (Map) parent;
    launch.setAlignmentY(0.0F);
    launch.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        apply();
      }
    });
    map.getToolBar().add(launch);
  }

  public void apply() {
    Command c = new NullCommand();
    BoundsTracker tracker = new BoundsTracker();
    GamePiece[] p = map.getPieces();
    for (int i = 0; i < p.length; ++i) {
      if ((p[i] instanceof Stack)) {
        Stack s = (Stack) p[i];
        for (Enumeration e = s.getPieces(); e.hasMoreElements();) {
          apply((GamePiece) e.nextElement(), c, tracker);
        }
      }
      else {
        apply(p[i], c, tracker);
      }
    }
    tracker.repaint();
    GameModule.getGameModule().sendAndLog(c);
  }

  private void apply(GamePiece p, Command c, BoundsTracker tracker) {
    if (isValidTarget(p)) {
      tracker.addPiece(p);
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
    if (containsName(target)) {
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
  protected boolean containsName(GamePiece target) {
    for (int j = 0; j < names.length; ++j) {
      if (Decorator.getInnermost(target).getName().equals(names[j])) {
        return true;
      }
    }
    return false;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Button text", "Key Command", "Pieces affected", "Apply command"};
  }

  public String[] getAttributeNames() {
    return new String[]{TEXT, HOTKEY, NAMES, CONDITION};
  }

  public static class Prompt extends StringEnum {
    public String[] getValidValues() {
      return new String[]{ALWAYS, IF_ACTIVE, IF_INACTIVE};
    }
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, KeyStroke.class, String[].class, Prompt.class};
  }

  public String getAttributeValueString(String key) {
    if (TEXT.equals(key)) {
      return launch.getText();
    }
    else if (HOTKEY.equals(key)) {
      return HotKeyConfigurer.encode(stroke);
    }
    else if (NAMES.equals(key)) {
      return StringArrayConfigurer.arrayToString(names);
    }
    else if (CONDITION.equals(key)) {
      return condition;
    }
    else {
      return null;
    }
  }

  public static String getConfigureTypeName() {
    return "Global Key Command";
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
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
    if (TEXT.equals(key)) {
      launch.setText((String) value);
    }
    else if (HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = HotKeyConfigurer.decode((String) value);
      }
      stroke = (KeyStroke) value;
    }
    else if (NAMES.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      names = (String[]) value;
    }
    else if (CONDITION.equals(key)) {
      condition = (String) value;
    }
  }
}
