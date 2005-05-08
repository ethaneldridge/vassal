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

import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

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
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.PlayerIdFormattedString;

import UpFront.StringArrayConfigurer;

public class Messenger extends AbstractConfigurable implements ActionListener {
  
  protected NewLaunchButton launch;
  protected JPopupMenu popup;
  protected Component owner;
  protected String[] messages = new String[0];
  protected FormattedString format = new PlayerIdFormattedString();
  Map map = null;
  protected String reportFormat = "$message$";
  protected Messenger me;

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
    launch = new NewLaunchButton(null, BUTTON_TEXT, HOTKEY, ICON, al);
    setAttribute(NAME, "Messenger");
    setAttribute(BUTTON_TEXT, "Messages");
    launch.setToolTipText("Generate Counter Inventory");
    setConfigureName("Chat Messenger");
    me = this;
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

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Button text", "Button icon", "Messages", "Message Format" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, listConfig.class, formatConfig.class };
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, MESSAGES, FORMAT };
  }


  public static class formatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{MESSAGE});
    }
  }

  public static class listConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new StringArrayConfigurer(key, name, new String[]{MESSAGE});
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
    int y = launch.getY();

    popup.show(owner, x, y);
  }
  
  protected void buildPopup() {
    
    JMenu subMenu = null;
    JMenuItem item;
    
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
      String message = messages[i];
      if (message.charAt(0) == '>') {
        if (subMenu != null) {
          popup.add(subMenu);
        }
        subMenu = new JMenu(message.substring(1));
      }
      else {
        item = new JMenuItem(message);
        item.addActionListener(this);
        if (subMenu == null) {
          popup.add(item);
        }
        else {
          subMenu.add(item);  
        }
      }
    }
    if (subMenu != null) {
      popup.add(subMenu);
    }
  }

  protected void addItem(JMenu menu, String item) {
    JMenuItem messageItem = new JMenuItem(item);
    messageItem.addActionListener(this);
    menu.add(messageItem);
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
 
  public class NewLaunchButton extends JButton {
    private String nameAtt;
    private String keyAtt;
    private String iconAtt;
    private IconConfigurer iconConfig;
    private String toolTipText;
    private KeyStrokeListener keyListener;
    private Configurer nameConfig, keyConfig;

    public NewLaunchButton(String name, String nameAttribute,
                        String hotkeyAttribute, ActionListener al) {
                        this(name,nameAttribute,hotkeyAttribute,null,al);
    }

    public NewLaunchButton(String name, String nameAttribute,
                        String hotkeyAttribute, String iconAttribute, final ActionListener al) {
      super(name);
      nameAtt = nameAttribute;
      keyAtt = hotkeyAttribute;
      iconAtt = iconAttribute;
      iconConfig = new IconConfigurer(iconAtt,null,null);
      setAlignmentY(0.0F);
      keyListener = new KeyStrokeListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          if (isEnabled() && isShowing()) {
            al.actionPerformed(e);
          }
        }
      });
      GameModule.getGameModule().addKeyStrokeListener(keyListener);
      addActionListener(al);
    }

    protected void fireActionPerformed(ActionEvent event) {
      super.fireActionPerformed(event);
      //GameModule.getGameModule().getChatter().getInputField().requestFocus();
    }

    public String getNameAttribute() {
      return nameAtt;
    }

    public String getHotkeyAttribute() {
      return keyAtt;
    }

    public String getIconAttribute() {
      return iconAtt;
    }

    public String getAttributeValueString(String key) {
      if (key.equals(nameAtt)) {
        return getText();
      }
      else if (key.equals(keyAtt)) {
        return HotKeyConfigurer.encode(keyListener.getKeyStroke());
      }
      else if (key.equals(iconAtt)) {
        return iconConfig.getValueString();
      }
      else {
        return null;
      }
    }

    public void setAttribute(String key, Object value) {
      if (key.equals(nameAtt)) {
        setText((String) value);
      }
      else if (key.equals(keyAtt)) {
        if (value instanceof String) {
          value = HotKeyConfigurer.decode((String) value);
        }
        keyListener.setKeyStroke((KeyStroke) value);
        setToolTipText(toolTipText);
      }
      else if (key.equals(iconAtt)) {
        if (value instanceof String) {
          iconConfig.setValue((String)value);
          setIcon(iconConfig.getIconValue());
        }
      }
    }

    public void setToolTipText(String text) {
      toolTipText = text;
      if (keyListener.getKeyStroke() != null) {
        text =
          (text == null ? "" : text + " ")
          + "[" + HotKeyConfigurer.getString(keyListener.getKeyStroke()) + "]";
      }
      super.setToolTipText(text);
    }

    public Configurer getNameConfigurer() {
      if (nameConfig == null && nameAtt != null) {
        nameConfig = new StringConfigurer(nameAtt, "Button text", getText());
      }
      return nameConfig;
    }

    public Configurer getHotkeyConfigurer() {
      if (keyConfig == null && keyAtt != null) {
        keyConfig = new HotKeyConfigurer(keyAtt, "Hotkey", keyListener.getKeyStroke());
      }
      return keyConfig;
    }
  }
}