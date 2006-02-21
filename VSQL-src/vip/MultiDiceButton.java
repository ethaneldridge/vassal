/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney, Brent Easton
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

package vip;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Enumeration;

import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.DiceButton;
import VASSAL.build.module.Map;
import VASSAL.build.module.DiceButton.IconConfig;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.KeyStrokeListener;

public class MultiDiceButton extends AbstractConfigurable implements ActionListener {
  
  protected NewLaunchButton launch;
  protected JPopupMenu popup;
  protected Component owner;
  Map map = null;
  protected MultiDiceButton instance;

  public static final String HOTKEY = "hotkey";
  public static final String BUTTON_TEXT = "text";
  public static final String NAME = "name";
  public static final String ICON = "icon";

  public MultiDiceButton() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        selectAndSendMessage();
      }
    };
    launch = new NewLaunchButton(null, BUTTON_TEXT, HOTKEY, ICON, al);
    setAttribute(NAME, "Dice");
    setAttribute(BUTTON_TEXT, "Dice");
    launch.setToolTipText("Dice Button Menu");
    setConfigureName("Dice Menu");
    instance = this;
  }

  public static String getConfigureTypeName() {
    return "Multi Dice Button";
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
    return new Class[] {MiniDiceButton.class};
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Button text", "Button icon"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class};
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON };
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
  
  public void selectAndSendMessage() {
    if (popup == null) {
      buildPopup();
    }

    int x = launch.getX() + ((map == null) ? 0 : launch.getParent().getBounds().x);
    int y = launch.getY()+40;

    popup.show(owner, x, y);
  }
  
  protected void buildPopup() {
    
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

    Enumeration e = this.getComponents(DiceButton.class);
    while (e.hasMoreElements()) {
      DiceButton button = (DiceButton) e.nextElement();  
      String s = button.getConfigureName();
      item = new JMenuItem(button.getConfigureName());
      item.addActionListener(this);
      popup.add(item);
    }
  }

  protected void addItem(JMenu menu, String item) {
    JMenuItem messageItem = new JMenuItem(item);
    messageItem.addActionListener(this);
    menu.add(messageItem);
  }

  public void actionPerformed(ActionEvent event) {

    String message = event.getActionCommand();
    
    Enumeration e = this.getComponents(DiceButton.class);

    while (e.hasMoreElements()) {
      MiniDiceButton button = (MiniDiceButton) e.nextElement();  
      if (message.equals(button.getConfigureName())) {
        button.doRoll();
      }
    }
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