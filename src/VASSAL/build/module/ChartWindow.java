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
package VASSAL.build.module;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.PanelWidget;
import VASSAL.build.widget.TabWidget;
import VASSAL.preferences.PositionOption;
import VASSAL.tools.LaunchButton;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.MalformedURLException;

/**
 * A top-level Widget for displaying Charts
 */
public class ChartWindow extends Widget {

  public static final String NAME = "label";
  public static final String HOTKEY = "hotkey";

  private LaunchButton launch;
  private JDialog frame;
  private JComponent root;

  private String id;

  public ChartWindow() {
    root = new JPanel();
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        frame.setVisible(!frame.isVisible());
      }
    };
    launch = new LaunchButton(null, NAME, HOTKEY, al);

    setAttribute(NAME, "Charts");
  }

  /**
   * Expects to be added to a GameModule.  Adds a JButton to the
   * control window's toolbar.  Pushing the button displays the window
   */
  public void addTo(Buildable b) {
    rebuild();
    int count = 0;
    for (java.util.Enumeration e =
      GameModule.getGameModule().getComponents(PieceWindow.class);
         e.hasMoreElements();) {
      count++;
      e.nextElement();
    }

    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().getToolBar().add(launch);

    frame = new JDialog(GameModule.getGameModule().getFrame());
    frame.getContentPane().add(root);
    frame.setTitle(launch.getAttributeValueString(NAME));
    id = "ChartWindow" + count;
    String key = PositionOption.key + id;
    GameModule.getGameModule().getPrefs().addOption
      (new PositionOption(key, frame));
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(launch);
  }

  public void setAttribute(String key, Object val) {
    if (NAME.equals(key)) {
      setConfigureName(launch.getText());
      if (frame != null) {
        frame.setTitle((String) val);
      }
      launch.setAttribute(key, val);
    }
    else {
      launch.setAttribute(key, val);
    }
  }

  /**
   * The attributes of a ChartWindow are:
   * <code>NAME</code> Appears as the name of the button in the toolbar and the window itself
   * <code>HOTKEY</code> for the hotkey equivalent for the button
   */
  public String[] getAttributeNames() {
    String[] s = {NAME, HOTKEY};
    return s;
  }

  public String getAttributeValueString(String name) {
    return launch.getAttributeValueString(name);
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{VASSAL.build.widget.Chart.class, TabWidget.class, PanelWidget.class};
  }

  public void add(Buildable b) {
    if (b instanceof Widget) {
      root.add(((Widget) b).getComponent());
    }
    super.add(b);
  }

  public void remove(Buildable b) {
    if (b instanceof Widget) {
      root.remove(((Widget) b).getComponent());
    }
    super.remove(b);
  }

  public java.awt.Component getComponent() {
    return root;
  }

  public static String getConfigureTypeName() {
    return "Charts";
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Button text", "Hotkey"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, KeyStroke.class};
  }

  public HelpFile getHelpFile() {
File dir = new File("docs");
dir = new File(dir,"ReferenceManual");
try {
  return new HelpFile(null,new File(dir,"ChartWindow.htm"));
}
catch (MalformedURLException ex) {
  return null;
}  }
}

