/*
 * $Id$
 *
 * Copyright (c) 2000-2005 by Rodney Kinney, Brent Easton
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

package wga;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.DiceButton.IconConfig;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.tools.LaunchButton;

public class MultiMassKeyCommand extends AbstractConfigurable implements GameComponent, ActionListener {

  protected LaunchButton launch;
  protected ArrayList children;
  protected JPopupMenu popup = null;
  protected Map map;

  public static final String VERSION = "1.0";
  public static final String HOTKEY = "hotkey";
  public static final String BUTTON_TEXT = "text";
  public static final String NAME = "name";
  public static final String ICON = "icon";
  public static final String DEST = "destination";

  public MultiMassKeyCommand() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        ;
        doPopup();
      }
    };
    launch = new LaunchButton(null, BUTTON_TEXT, HOTKEY, ICON, al);
    setAttribute(NAME, "Mass Key");
    setAttribute(BUTTON_TEXT, "Mass Key");
    launch.setToolTipText(getConfigureName());
    launch.setEnabled(false);
  }

  public void doPopup() {
    if (popup == null) {
      buildPopup();
    }
    if (popup.getComponentCount() == 1) {
      ((BasicMassKeyCommand) this.getComponents(BasicMassKeyCommand.class).nextElement()).apply(map);
    }
    else 
      popup.show(launch, 0, 0);
  }
  
  private void buildPopup() {
    popup = new JPopupMenu();
    
    Enumeration e = this.getComponents(BasicMassKeyCommand.class);
    while (e.hasMoreElements()) {
      BasicMassKeyCommand basic = (BasicMassKeyCommand) e.nextElement();
      JMenuItem item = new JMenuItem(basic.getConfigureName());
      item.addActionListener(this);
      popup.add(item);
    }
  }

  public void actionPerformed(ActionEvent ev) {
    String actionName = ev.getActionCommand();
    Enumeration e = this.getComponents(BasicMassKeyCommand.class);
    while (e.hasMoreElements()) {
      BasicMassKeyCommand basic = (BasicMassKeyCommand) e.nextElement();
      if (actionName.equals(basic.getConfigureName())) {
        basic.apply(map);
      }
    }
  }
  
  public static String getConfigureTypeName() {
    return "Multi Mass Key Command v"+VERSION;
  }
  
  public void addTo(Buildable b) {
    map = (Map) b;
    launch.setAlignmentY(0.0F);
    map.getToolBar().add(getComponent());
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  protected Component getComponent() {
    return launch;
  }

  public void removeFrom(Buildable b) {
    map.getToolBar().remove(getComponent());
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public void add(Buildable b) {
    super.add(b);
    popup = null;
  }

  public void remove(Buildable b) {
    super.remove(b);
    popup = null;
  }

 
  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] { BasicMassKeyCommand.class };
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Button text", "Button icon", "Hotkey" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, KeyStroke.class};
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, HOTKEY  };
  }

  public void setAttribute(String key, Object o) {

    if (NAME.equals(key)) {
      setConfigureName((String) o);
      launch.setToolTipText((String) o);
    }
 
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
     else {
      return launch.getAttributeValueString(key);
    }
  }
  
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);   
  }

  public Command getRestoreCommand() {
    return null;
  }

}