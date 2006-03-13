/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package vp;

import java.awt.Color;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.UniqueIdManager;

/**
 * Generic VP Counter
 */
public class VPTracker extends AbstractConfigurable implements GameComponent, UniqueIdManager.Identifyable {

  protected static UniqueIdManager idMgr = new UniqueIdManager("VPTracker");
  
  protected static final String COMMAND_PREFIX = "VP";
  public static final String VERSION = "1.1";

  public static final String NAME = "name";
  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String COLOR = "color";

  protected Color color = Color.white;

  protected VPWindow vpWindow;
  protected LaunchButton launch;
  protected JTextArea VPLabel = new JTextArea();
  protected String id;

  public VPTracker() {
    
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        vpWindow.setVisible(!vpWindow.isShowing());
      }
    };
    launch = new LaunchButton("VP", BUTTON_TEXT, HOT_KEY, ICON, al);
    launch.setToolTipText("VP");
    launch.setEnabled(false);
  }
  
  /*
   * Module level Configuration stuff
   */
  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, HOT_KEY, COLOR };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      launch.setToolTipText((String) value);
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      color = (Color) value;
    }
    else {
      launch.setAttribute(key, value);
    }
  }
  
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName() + "";
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Button text:  ", "Button Icon:  ", "Hotkey:  ", "Background Color:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, KeyStroke.class, Color.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((VPTracker) c).launch.getAttributeValueString(ICON));
    }
  }

  
  public Class[] getAllowableConfigureComponents() {
    return new Class[] {  };
  }

  public static String getConfigureTypeName() {
    return "VP Tracker v"+VERSION;
  }

  public void addTo(Buildable b) {
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().getToolBar().add(launch);
    launch.setAlignmentY(0.0F);
    idMgr.add(this);
 
    vpWindow = new VPWindow();    
    vpWindow.setColor(color);
    vpWindow.pack(); 

  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().getToolBar().remove(launch);
  }

  public HelpFile getHelpFile() {
    return null;
  }
  
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);
  }
  
  public Command getRestoreCommand() {
    return null;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  protected void updateVPDisplay() {
    vpWindow.setColor(color);
    vpWindow.repaint();
  }

  protected class VPWindow extends JDialog  {

    protected JPanel mainPanel;
    protected JPanel controlPanel;
    protected JPanel VPPanel;


    protected VPWindow() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
    }


    public void setColor(Color color) {
//      VPPanel.setBackground(color);
   
    }

    protected void initComponents() {

      setTitle(getConfigureName());

      mainPanel = new JPanel();
      mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
      getContentPane().add(mainPanel);

      pack();
      setLocation(100, 100);
    }    
 
  }

}
