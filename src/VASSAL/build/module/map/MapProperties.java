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

package VASSAL.build.module.map;

import java.awt.event.ActionListener;
import java.util.Enumeration;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.PropertyProducer;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.counters.KeyCommand;
import VASSAL.tools.LaunchButton;

/**
 * A container for Global properties attached to a Map
 */
public class MapProperties extends AbstractConfigurable implements PropertyProducer {

  public static final String NAME = "name";
  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";

  protected LaunchButton launch;
  protected JPopupMenu popup = null;
  protected Map map = null;

  public MapProperties() {

    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        apply();
      }
    };
    launch = new LaunchButton("Properties", BUTTON_TEXT, HOT_KEY, ICON, al);
    setConfigureName("Global Properties");
  }

  /**
   * When the launch button is pressed, popup a menu of available Global
   * properties. Each menu item has a sub-menu of key commands applicable to
   * that property.
   */
  protected void apply() {
    buildPopup();
    popup.show(launch, 0, 0);
  }

  protected void buildPopup() {
    popup = new JPopupMenu();
    Enumeration e = getBuildComponents();
    while (e.hasMoreElements()) {
      DynamicProperty property = (DynamicProperty) e.nextElement();

      KeyCommand[] keys = property.getKeyCommands(null);

      if (keys.length > 0) {
        JMenu subMenu = new JMenu(property.getDescription() + " = " + property.getValue());
        JMenuItem item;

        for (int i = 0; i < keys.length; i++) {
          item = new JMenuItem(keys[i].getName());
          item.addActionListener(property);
          subMenu.add(item);
        }

        popup.add(subMenu);
      }
      else {
        JMenuItem item = new JMenuItem(property.getDescription() + " = " + property.getValue());
        popup.add(item);
      }
    }
  }

  public Object getProperty(Object key) {
    if (key instanceof String) {
      DynamicProperty marker = getMarker((String) key);
      if (marker != null) {
        return marker.getProperty(key);
      }
    }
    return null;
  }

  public void setProperty(Object key, Object value) {
    if (key instanceof String) {
      DynamicProperty marker = getMarker((String) key);
      if (marker != null) {
        marker.setProperty(key, value);
      }
    }
  }

  /**
   * Find a child Dynamic Property by matching Name
   * 
   * @param name
   * @return
   */
  protected DynamicProperty getMarker(String name) {

    Enumeration e = getComponents(DynamicProperty.class);
    while (e.hasMoreElements()) {
      DynamicProperty marker = (DynamicProperty) e.nextElement();
      if (name.equals(marker.getConfigureName())) {
        return marker;
      }
    }

    return null;
  }

  protected void childWasUpdated() {
    map.repaint();
  }

  public Map getMap() {
    return map;
  }

  /*
   * -----------------------------------------------------------------------
   * AbstractConfigurable Implementation
   * -----------------------------------------------------------------------
   */

  public String[] getAttributeDescriptions() {
    return new String[] {"Name:  ", "Button text:  ", "Button Icon:  ", "Hotkey:  "};

  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, String.class, IconConfig.class, KeyStroke.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((MapProperties) c).launch.getAttributeValueString(ICON));
    }
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, BUTTON_TEXT, ICON, HOT_KEY};
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      launch.setToolTipText((String) value);
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName() + "";
    }
    else {
      return launch.getAttributeValueString(key);
    }

  }

  public void removeFrom(Buildable parent) {
    map.getToolBar().remove(getLaunchButton());
    map = null;
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] {DynamicProperty.class};
  }

  public void addTo(Buildable parent) {
    map = (Map) parent;
    map.getToolBar().add(getLaunchButton());
  }

  public LaunchButton getLaunchButton() {
    return launch;
  }

  public static String getConfigureTypeName() {
    return "Global Properties";
  }

}
