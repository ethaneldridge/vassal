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

package UpFront;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.DiceButton.IconConfig;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.PlayerIdFormattedString;

public class Messenger extends AbstractConfigurable implements ActionListener {
  
  protected LaunchButton launch;
  protected JPopupMenu popup;
  protected Component owner;
  protected String[] messages = new String[0];
  protected FormattedString format = new PlayerIdFormattedString();
  Map map = null;
  protected String reportFormat = "$message$";

  public static final String HOTKEY = "hotkey";
  public static final String BUTTON_TEXT = "text";
  public static final String NAME = "name";
  public static final String ICON = "icon";
  public static final String FORMAT = "format";
  public static final String MESSAGES = "messages";
  public static final String MESSAGE = "message";

  public Messenger() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        selectAndSendMessage();
      }
    };
    launch = new LaunchButton(null, BUTTON_TEXT, HOTKEY, ICON, al);
    setAttribute(NAME, "Messenger");
    setAttribute(BUTTON_TEXT, "Messages");
    launch.setToolTipText("Generate Counter Inventory");
    setConfigureName("Chat Messenger");
  }

  public void addTo(Buildable b) {
    launch.setAlignmentY(0.0F);
    if (b instanceof Map) {     
      map = (Map) b;
      map.getToolBar().add(launch);
      if (map.shouldDockIntoMainWindow()) {        
         owner = GameModule.getGameModule().getFrame();
      }
      else {
         owner = map.getView();
      }
    }
    else {
       GameModule.getGameModule().getToolBar().add(launch);     
       owner = GameModule.getGameModule().getFrame();
    }
  }

  public void removeFrom(Buildable b) {
    if (b instanceof Map) {
      ((Map) b).getToolBar().remove(launch);
    }
    else {
      GameModule.getGameModule().getToolBar().remove(launch);
    } 
  }

  public HelpFile getHelpFile() {
    return null;
  }

//  /**
//   * The name of this Configurable Object
//   */
//  public String getConfigureName() {
//    return "Chat Messenger";
//  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Button text", "Button icon", "Messages", "Message Format" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, String[].class, formatConfig.class };
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, MESSAGES, FORMAT };
  }


  public static class formatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{MESSAGE});
    }
  }
      
  public void setAttribute(String key, Object o) {

    if (NAME.equals(key)) {
      setConfigureName((String) o);
      launch.setToolTipText((String) o);
    }
    else if(MESSAGES.equals(key)) {
      if (o instanceof String) {
        o = StringArrayConfigurer.stringToArray((String) o);
      }
      messages = (String[]) o;
    }
    else if (FORMAT.equals(key)) {
      reportFormat = (String) o;
    }
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if(MESSAGES.equals(key)) {
      return StringArrayConfigurer.arrayToString(messages);
    }
    else if (FORMAT.equals(key)) {
      return reportFormat;
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }
  
  public void selectAndSendMessage() {
    if (popup == null) {
      buildPopup();
    }

    int x = launch.getX() + ((map == null) ? 0 : launch.getParent().getBounds().x);

    popup.show(owner, x, launch.getY());
  }
  
  protected void buildPopup() {
    popup = new JPopupMenu();

    popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
      public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
        owner.repaint();
      }

      public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
        owner.repaint();
      }

      public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
      }
    });

    for (int i = 0; i < messages.length; i++) {
      addItem(messages[i]);
    }
  }

  protected void addItem(String item) {
    JMenuItem messageItem = new JMenuItem(item);
    messageItem.addActionListener(this);
    popup.add(messageItem);
  }

  public void actionPerformed(ActionEvent e) {

    String message = e.getActionCommand();
    format.setFormat(reportFormat);
    format.setProperty(MESSAGE, message);
    String formattedMessage = format.getText();
    Command c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), formattedMessage);
    c.execute();
    GameModule.getGameModule().sendAndLog(c);
  }
}